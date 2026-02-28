use std::collections::HashMap;

use dynasmrt::{AssemblyOffset, DynamicLabel};
use facet::{Def, EnumRepr, ListDef, OptionDef, ScalarType, Shape, Type, UserType};

use crate::arch::EmitCtx;
use crate::format::{FieldEmitInfo, Format, VariantEmitInfo, VariantKind};

/// A compiled deserializer. Owns the executable buffer containing JIT'd machine code.
pub struct CompiledDeser {
    buf: dynasmrt::ExecutableBuffer,
    entry: AssemblyOffset,
    func: unsafe extern "C" fn(*mut u8, *mut crate::context::DeserContext),
}

impl CompiledDeser {
    pub(crate) fn func(&self) -> unsafe extern "C" fn(*mut u8, *mut crate::context::DeserContext) {
        self.func
    }

    /// The raw executable code buffer.
    pub fn code(&self) -> &[u8] {
        &self.buf
    }

    /// Byte offset of the entry point within the code buffer.
    pub fn entry_offset(&self) -> usize {
        self.entry.0
    }
}

// r[impl compiler.walk]
// r[impl compiler.recursive]
// r[impl compiler.recursive.one-func-per-shape]

/// Per-shape compilation result: the DynamicLabel for inter-function calls,
/// and the AssemblyOffset for resolving the final function pointer.
struct ShapeEntry {
    /// Label bound at the function entry — used by `bl =>label` / `call =>label`.
    label: DynamicLabel,
    /// Assembly offset of the function entry — used to get the pointer from the buffer.
    offset: Option<AssemblyOffset>,
}

/// Compiler state for emitting one function per shape into a shared buffer.
struct Compiler<'fmt> {
    ectx: EmitCtx,
    format: &'fmt dyn Format,
    /// All shapes we've started or finished compiling.
    /// If offset is Some, the function is fully emitted.
    /// If offset is None, the function is in progress (label allocated but not yet bound).
    shapes: HashMap<*const Shape, ShapeEntry>,
    /// Stack offset where Option inner values can be temporarily deserialized.
    /// Zero if no Options are present.
    option_scratch_offset: u32,
}

impl<'fmt> Compiler<'fmt> {
    fn new(extra_stack: u32, option_scratch_offset: u32, format: &'fmt dyn Format) -> Self {
        Compiler {
            ectx: EmitCtx::new(extra_stack),
            format,
            shapes: HashMap::new(),
            option_scratch_offset,
        }
    }

    /// Compile a deserializer for `shape`. Returns the entry label and offset.
    ///
    /// - If already compiled, returns the cached entry immediately.
    /// - If currently in progress (recursive back-edge), returns the pre-allocated
    ///   label (not yet bound) — dynasmrt patches it as a forward reference.
    /// - Otherwise, emits the function depth-first.
    fn compile_shape(&mut self, shape: &'static Shape) -> DynamicLabel {
        let key = shape as *const Shape;

        if let Some(entry) = self.shapes.get(&key) {
            return entry.label;
        }

        // Pre-allocate the entry label so recursive calls can target it.
        let entry_label = self.ectx.new_label();
        self.shapes.insert(
            key,
            ShapeEntry {
                label: entry_label,
                offset: None, // not yet emitted
            },
        );

        // Check for List (Vec<T>) first — it's detected by Def, not Type.
        // Vec<T> has Type::User(UserType::Opaque) + Def::List.
        if let Def::List(list_def) = &shape.def {
            self.compile_vec(shape, list_def, entry_label);
            return entry_label;
        }

        match &shape.ty {
            Type::User(UserType::Struct(_)) => {
                self.compile_struct(shape, entry_label);
            }
            Type::User(UserType::Enum(enum_type)) => {
                self.compile_enum(shape, enum_type, entry_label);
            }
            _ => panic!("unsupported shape: {}", shape.type_identifier),
        }

        entry_label
    }

    /// Compile a struct shape into a function.
    fn compile_struct(
        &mut self,
        shape: &'static Shape,
        entry_label: DynamicLabel,
    ) {
        let key = shape as *const Shape;
        let fields = collect_fields(shape);
        let inline = self.format.supports_inline_nested();

        // For non-inlining formats, depth-first compile all nested composite fields
        // (structs and enums) so they're available as call targets.
        // Enum fields always need pre-compilation (even in inline formats) since
        // they can't be inlined — they need their own discriminant dispatch.
        // Option fields: if the inner T is composite, pre-compile it too.
        let nested: Vec<Option<DynamicLabel>> = if inline {
            fields
                .iter()
                .map(|f| {
                    let target = option_inner_or_self(f.shape);
                    // Lists (Vec<T>) always need pre-compilation.
                    // Enums always need pre-compilation (can't be inlined).
                    if matches!(&target.ty, Type::User(UserType::Enum(_)))
                        || matches!(&target.def, Def::List(_))
                    {
                        Some(self.compile_shape(target))
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            fields
                .iter()
                .map(|f| {
                    let target = option_inner_or_self(f.shape);
                    if is_composite(target) {
                        Some(self.compile_shape(target))
                    } else {
                        None
                    }
                })
                .collect()
        };

        // Bind the entry label at the function start, then emit prologue.
        self.ectx.bind_label(entry_label);
        let (entry_offset, error_exit) = self.ectx.begin_func();

        let format = self.format;
        let option_scratch_offset = self.option_scratch_offset;
        let ectx = &mut self.ectx;

        format.emit_struct_fields(ectx, &fields, &mut |ectx, field| {
            emit_field(ectx, format, field, &fields, &nested, option_scratch_offset);
        });

        self.ectx.end_func(error_exit);

        // Mark as finished with the resolved offset.
        self.shapes.get_mut(&key).unwrap().offset = Some(entry_offset);
    }

    // r[impl seq.malum]

    /// Compile a Vec<T> shape into a function.
    fn compile_vec(
        &mut self,
        shape: &'static Shape,
        list_def: &'static ListDef,
        entry_label: DynamicLabel,
    ) {
        let key = shape as *const Shape;

        // Discover Vec<T> layout at JIT-compile time using vtable probing.
        let vec_offsets = crate::malum::discover_vec_offsets(list_def, shape);

        let elem_shape = list_def.t;

        // Pre-compile element shape if it's composite (struct/enum/vec).
        let elem_label = if needs_precompilation(elem_shape) {
            Some(self.compile_shape(elem_shape))
        } else {
            None
        };

        self.ectx.bind_label(entry_label);
        let (entry_offset, error_exit) = self.ectx.begin_func();

        let format = self.format;
        let option_scratch_offset = self.option_scratch_offset;

        format.emit_vec(
            &mut self.ectx,
            0, // offset=0: function receives out pointing to the Vec slot
            elem_shape,
            elem_label,
            &vec_offsets,
            option_scratch_offset,
            &mut |ectx| {
                // Emit element deserialization. Out is already redirected to the slot.
                emit_elem(ectx, format, elem_shape, elem_label, option_scratch_offset);
            },
        );

        self.ectx.end_func(error_exit);
        self.shapes.get_mut(&key).unwrap().offset = Some(entry_offset);
    }

    // r[impl deser.postcard.enum]
    // r[impl deser.json.enum.external]

    /// Compile an enum shape into a function.
    fn compile_enum(
        &mut self,
        shape: &'static Shape,
        enum_type: &'static facet::EnumType,
        entry_label: DynamicLabel,
    ) {
        let key = shape as *const Shape;
        let variants = collect_variants(enum_type);
        let disc_size = discriminant_size(enum_type.enum_repr);
        let inline = self.format.supports_inline_nested();

        // Depth-first compile all nested composite types (structs and enums)
        // found in variant fields so they're available as call targets.
        // Enum fields always need pre-compilation even in inline formats.
        // Option fields: if the inner T is composite, pre-compile it too.
        let nested_labels: Vec<Vec<Option<DynamicLabel>>> = if inline {
            variants
                .iter()
                .map(|v| {
                    v.fields
                        .iter()
                        .map(|f| {
                            let target = option_inner_or_self(f.shape);
                            if matches!(&target.ty, Type::User(UserType::Enum(_)))
                                || matches!(&target.def, Def::List(_))
                            {
                                Some(self.compile_shape(target))
                            } else {
                                None
                            }
                        })
                        .collect()
                })
                .collect()
        } else {
            variants
                .iter()
                .map(|v| {
                    v.fields
                        .iter()
                        .map(|f| {
                            let target = option_inner_or_self(f.shape);
                            if is_composite(target) {
                                Some(self.compile_shape(target))
                            } else {
                                None
                            }
                        })
                        .collect()
                })
                .collect()
        };

        // Bind the entry label, emit prologue.
        self.ectx.bind_label(entry_label);
        let (entry_offset, error_exit) = self.ectx.begin_func();

        let format = self.format;
        let option_scratch_offset = self.option_scratch_offset;
        let ectx = &mut self.ectx;

        // Detect tagging mode from shape metadata.
        let tag_key = shape.get_tag_attr();
        let content_key = shape.get_content_attr();

        // Closure for the standard variant body: writes discriminant + deserializes fields.
        let mut emit_standard_variant_body = |ectx: &mut EmitCtx, variant: &VariantEmitInfo| {
            ectx.emit_write_discriminant(variant.rust_discriminant, disc_size);

            if variant.kind == VariantKind::Unit {
                return;
            }

            let nested = &nested_labels[variant.index];

            if variant.kind == VariantKind::Struct && !inline {
                format.emit_struct_fields(ectx, &variant.fields, &mut |ectx, field| {
                    emit_field(ectx, format, field, &variant.fields, nested, option_scratch_offset);
                });
                return;
            }

            for field in &variant.fields {
                emit_field(ectx, format, field, &variant.fields, nested, option_scratch_offset);
            }
        };

        match (tag_key, content_key, shape.is_untagged()) {
            // Externally tagged (default)
            (None, None, false) => {
                format.emit_enum(ectx, &variants, &mut emit_standard_variant_body);
            }

            // r[impl deser.json.enum.adjacent]
            // Adjacently tagged: { "tag_key": "Variant", "content_key": value }
            (Some(tk), Some(ck), false) => {
                format.emit_enum_adjacent(
                    ectx,
                    &variants,
                    tk,
                    ck,
                    &mut emit_standard_variant_body,
                );
            }

            // r[impl deser.json.enum.internal]
            // Internally tagged: { "tag_key": "Variant", ...variant_fields... }
            (Some(tk), None, false) => {
                format.emit_enum_internal(
                    ectx,
                    &variants,
                    tk,
                    &mut |ectx, variant| {
                        ectx.emit_write_discriminant(variant.rust_discriminant, disc_size);
                    },
                    &mut |ectx, variant, field| {
                        let nested = &nested_labels[variant.index];
                        emit_field(ectx, format, field, &variant.fields, nested, option_scratch_offset);
                    },
                );
            }

            // r[impl deser.json.enum.untagged]
            // Untagged: value-type bucketing + peek dispatch + solver
            (_, _, true) => {
                format.emit_enum_untagged(ectx, &variants, &mut emit_standard_variant_body);
            }

            // Invalid: content without tag
            (None, Some(_), _) => {
                panic!("content attribute without tag attribute is invalid");
            }
        }

        self.ectx.end_func(error_exit);
        self.shapes.get_mut(&key).unwrap().offset = Some(entry_offset);
    }
}

// r[impl deser.flatten]
// r[impl deser.flatten.offset-accumulation]
// r[impl deser.flatten.inline]

fn collect_fields(shape: &'static Shape) -> Vec<FieldEmitInfo> {
    let mut out = Vec::new();
    collect_fields_recursive(shape, 0, &mut out);
    check_field_name_collisions(&out);
    out
}

fn collect_fields_recursive(
    shape: &'static Shape,
    base_offset: usize,
    out: &mut Vec<FieldEmitInfo>,
) {
    let st = match &shape.ty {
        Type::User(UserType::Struct(st)) => st,
        _ => panic!("unsupported shape: {}", shape.type_identifier),
    };
    for f in st.fields {
        if f.is_flattened() {
            collect_fields_recursive(f.shape(), base_offset + f.offset, out);
        } else {
            out.push(FieldEmitInfo {
                offset: base_offset + f.offset,
                shape: f.shape(),
                name: f.effective_name(),
                required_index: out.len(),
            });
        }
    }
}

// r[impl deser.flatten.conflict]
fn check_field_name_collisions(fields: &[FieldEmitInfo]) {
    let mut seen = std::collections::HashSet::new();
    for f in fields {
        if !seen.insert(f.name) {
            panic!(
                "field name collision: \"{}\" (possibly from #[facet(flatten)])",
                f.name
            );
        }
    }
}

// r[impl deser.enum.variant-kinds]

fn collect_variants(enum_type: &'static facet::EnumType) -> Vec<VariantEmitInfo> {
    enum_type
        .variants
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let kind = VariantKind::from_struct_type(&v.data);
            let mut fields = Vec::new();
            for f in v.data.fields {
                if f.is_flattened() {
                    collect_fields_recursive(f.shape(), f.offset, &mut fields);
                } else {
                    fields.push(FieldEmitInfo {
                        offset: f.offset,
                        shape: f.shape(),
                        name: f.effective_name(),
                        required_index: fields.len(),
                    });
                }
            }
            VariantEmitInfo {
                index: i,
                name: v.effective_name(),
                rust_discriminant: v.discriminant.expect(
                    "enum variant must have a known discriminant (use #[repr(u8)] or similar)",
                ),
                fields,
                kind,
            }
        })
        .collect()
}

/// Get the discriminant storage size in bytes from an EnumRepr.
fn discriminant_size(repr: EnumRepr) -> u32 {
    match repr {
        EnumRepr::U8 | EnumRepr::I8 => 1,
        EnumRepr::U16 | EnumRepr::I16 => 2,
        EnumRepr::U32 | EnumRepr::I32 => 4,
        EnumRepr::U64 | EnumRepr::I64 | EnumRepr::USize | EnumRepr::ISize => 8,
        EnumRepr::Rust | EnumRepr::RustNPO => {
            panic!("cannot JIT-compile enums with #[repr(Rust)] — use #[repr(u8)] or similar")
        }
    }
}

/// Returns the OptionDef if this shape is an Option type.
fn get_option_def(shape: &'static Shape) -> Option<&'static OptionDef> {
    match &shape.def {
        Def::Option(opt_def) => Some(opt_def),
        _ => None,
    }
}

/// If the shape is an Option, return the inner T's shape; otherwise return the shape itself.
/// Used for nested label pre-compilation — we need to compile the inner T, not Option<T>.
fn option_inner_or_self(shape: &'static Shape) -> &'static Shape {
    match get_option_def(shape) {
        Some(opt_def) => opt_def.t,
        None => shape,
    }
}

/// Returns true if the shape is a struct type.
fn is_struct(shape: &'static Shape) -> bool {
    matches!(&shape.ty, Type::User(UserType::Struct(_)))
}

/// Returns true if the shape needs its own compiled function (struct, enum, or vec).
fn is_composite(shape: &'static Shape) -> bool {
    matches!(
        &shape.ty,
        Type::User(UserType::Struct(_) | UserType::Enum(_))
    ) || matches!(&shape.def, Def::List(_))
}

/// Returns true if the shape needs pre-compilation as a separate function.
/// This includes structs, enums, and lists (Vec<T>).
fn needs_precompilation(shape: &'static Shape) -> bool {
    is_composite(shape)
}

/// Emit code for a single field, dispatching to nested struct calls, inline
/// expansion, scalar intrinsics, or Option handling.
fn emit_field(
    ectx: &mut EmitCtx,
    format: &dyn Format,
    field: &FieldEmitInfo,
    all_fields: &[FieldEmitInfo],
    nested: &[Option<DynamicLabel>],
    option_scratch_offset: u32,
) {
    // Find this field's index by matching offset.
    let idx = all_fields
        .iter()
        .position(|f| f.offset == field.offset)
        .expect("field not found in all_fields");

    // r[impl deser.option]
    // Check if this field is an Option<T>.
    if let Some(opt_def) = get_option_def(field.shape) {
        let init_none_fn = opt_def.vtable.init_none as *const u8;
        let init_some_fn = opt_def.vtable.init_some as *const u8;
        let inner_shape = opt_def.t;

        format.emit_option(ectx, field.offset, init_none_fn, init_some_fn, option_scratch_offset, &mut |ectx| {
            // Emit inner T deserialization into the scratch area (out is already redirected).
            // The inner T's offset is 0 because out now points to the scratch area.
            if let Some(label) = nested[idx] {
                ectx.emit_call_emitted_func(label, 0);
            } else if is_struct(inner_shape) && format.supports_inline_nested() {
                emit_inline_struct(ectx, format, inner_shape, 0, option_scratch_offset);
            } else {
                match inner_shape.scalar_type() {
                    Some(ScalarType::String) => {
                        format.emit_read_string(ectx, 0);
                    }
                    Some(st) => {
                        format.emit_read_scalar(ectx, 0, st);
                    }
                    None => panic!(
                        "unsupported Option inner type: {}",
                        inner_shape.type_identifier,
                    ),
                }
            }
        });
        return;
    }

    if let Some(label) = nested[idx] {
        // r[impl deser.nested-struct]
        // r[impl deser.nested-struct.offset]
        ectx.emit_call_emitted_func(label, field.offset as u32);
        return;
    }

    // r[impl deser.nested-struct]
    // r[impl deser.nested-struct.offset]
    if is_struct(field.shape) && format.supports_inline_nested() {
        emit_inline_struct(ectx, format, field.shape, field.offset, option_scratch_offset);
        return;
    }

    match field.shape.scalar_type() {
        Some(ScalarType::String) => {
            format.emit_read_string(ectx, field.offset);
        }
        Some(st) => {
            format.emit_read_scalar(ectx, field.offset, st);
        }
        None => panic!(
            "unsupported field type: {} (scalar_type={:?})",
            field.shape.type_identifier,
            field.shape.scalar_type()
        ),
    }
}

/// Emit deserialization code for a single element (used by Vec loop).
/// Out pointer has already been redirected to the element slot at offset 0.
fn emit_elem(
    ectx: &mut EmitCtx,
    format: &dyn Format,
    elem_shape: &'static Shape,
    elem_label: Option<DynamicLabel>,
    option_scratch_offset: u32,
) {
    if let Some(label) = elem_label {
        ectx.emit_call_emitted_func(label, 0);
    } else if is_struct(elem_shape) && format.supports_inline_nested() {
        emit_inline_struct(ectx, format, elem_shape, 0, option_scratch_offset);
    } else {
        match elem_shape.scalar_type() {
            Some(ScalarType::String) => {
                format.emit_read_string(ectx, 0);
            }
            Some(st) => {
                format.emit_read_scalar(ectx, 0, st);
            }
            None => panic!(
                "unsupported Vec element type: {}",
                elem_shape.type_identifier,
            ),
        }
    }
}

/// Inline a nested struct's fields into the parent function.
///
/// Instead of emitting a function call, we collect the nested struct's fields,
/// adjust their offsets by `base_offset`, and emit them directly via the format's
/// `emit_struct_fields`. This recurses for deeply nested structs.
fn emit_inline_struct(
    ectx: &mut EmitCtx,
    format: &dyn Format,
    shape: &'static Shape,
    base_offset: usize,
    option_scratch_offset: u32,
) {
    let inner_fields = collect_fields(shape);
    let adjusted: Vec<FieldEmitInfo> = inner_fields
        .into_iter()
        .map(|f| FieldEmitInfo {
            offset: base_offset + f.offset,
            shape: f.shape,
            name: f.name,
            required_index: f.required_index,
        })
        .collect();

    // No nested labels — all struct fields will recurse into emit_inline_struct.
    let no_nested = vec![None; adjusted.len()];

    format.emit_struct_fields(ectx, &adjusted, &mut |ectx, field| {
        emit_field(ectx, format, field, &adjusted, &no_nested, option_scratch_offset);
    });
}

/// Compute the maximum Option inner type size across a shape tree.
/// Returns 0 if no Option fields are found.
fn max_option_inner_size(shape: &'static Shape, visited: &mut std::collections::HashSet<*const Shape>) -> usize {
    let key = shape as *const Shape;
    if !visited.insert(key) {
        return 0;
    }

    let mut max_size = 0usize;

    // Walk through List element types too.
    if let Def::List(list_def) = &shape.def {
        max_size = max_size.max(max_option_inner_size(list_def.t, visited));
    }

    match &shape.ty {
        Type::User(UserType::Struct(st)) => {
            for f in st.fields {
                let field_shape = f.shape();
                if let Some(opt_def) = get_option_def(field_shape) {
                    let inner_size = opt_def.t.layout.sized_layout().expect(
                        "Option inner type must be Sized"
                    ).size();
                    max_size = max_size.max(inner_size);
                    // Also recurse into the inner type
                    max_size = max_size.max(max_option_inner_size(opt_def.t, visited));
                }
                max_size = max_size.max(max_option_inner_size(field_shape, visited));
            }
        }
        Type::User(UserType::Enum(enum_type)) => {
            for v in enum_type.variants {
                for f in v.data.fields {
                    let field_shape = f.shape();
                    if let Some(opt_def) = get_option_def(field_shape) {
                        let inner_size = opt_def.t.layout.sized_layout().expect(
                            "Option inner type must be Sized"
                        ).size();
                        max_size = max_size.max(inner_size);
                        max_size = max_size.max(max_option_inner_size(opt_def.t, visited));
                    }
                    max_size = max_size.max(max_option_inner_size(field_shape, visited));
                }
            }
        }
        _ => {}
    }

    max_size
}

/// Check if a shape tree contains any List (Vec<T>) types.
fn has_list_in_tree(shape: &'static Shape, visited: &mut std::collections::HashSet<*const Shape>) -> bool {
    let key = shape as *const Shape;
    if !visited.insert(key) {
        return false;
    }

    if matches!(&shape.def, Def::List(_)) {
        return true;
    }

    match &shape.ty {
        Type::User(UserType::Struct(st)) => {
            for f in st.fields {
                let target = option_inner_or_self(f.shape());
                if has_list_in_tree(target, visited) {
                    return true;
                }
            }
        }
        Type::User(UserType::Enum(enum_type)) => {
            for v in enum_type.variants {
                for f in v.data.fields {
                    let target = option_inner_or_self(f.shape());
                    if has_list_in_tree(target, visited) {
                        return true;
                    }
                }
            }
        }
        _ => {}
    }

    false
}

/// Compile a deserializer for the given shape and format.
pub fn compile_deser(shape: &'static Shape, format: &dyn Format) -> CompiledDeser {
    // Compute extra stack space. For structs, pass fields. For enums, we need
    // the max across all variant bodies. Use empty fields for enums since JSON
    // enum handling computes its own stack needs.
    let mut format_extra = if matches!(&shape.def, Def::List(_)) {
        // Vec<T> as root shape — format needs stack space for vec loop state.
        format.extra_stack_space(&[])
    } else {
        match &shape.ty {
            Type::User(UserType::Struct(_)) => {
                let fields = collect_fields(shape);
                format.extra_stack_space(&fields)
            }
            Type::User(UserType::Enum(enum_type)) => {
                // For enums, the format might need extra stack for variant body
                // deserialization (e.g., JSON struct variant key matching).
                // Compute the max across all variants.
                let mut max_extra = 0u32;
                for v in enum_type.variants {
                    let fields: Vec<FieldEmitInfo> = v.data.fields.iter().enumerate().map(|(j, f)| {
                        FieldEmitInfo {
                            offset: f.offset,
                            shape: f.shape(),
                            name: f.name,
                            required_index: j,
                        }
                    }).collect();
                    max_extra = max_extra.max(format.extra_stack_space(&fields));
                }
                max_extra
            }
            _ => panic!("unsupported root shape: {}", shape.type_identifier),
        }
    };

    // If the shape tree contains any Vec types, ensure we have enough stack
    // for Vec loop state (buf_ptr, count/cap, counter, saved_out).
    if has_list_in_tree(shape, &mut std::collections::HashSet::new()) {
        format_extra = format_extra.max(format.vec_extra_stack_space());
    }

    // Compute Option scratch space: 8 bytes for saved out pointer + max inner T size.
    // The scratch area lives after the base frame and format's extra stack.
    // Stack layout from sp:
    //   [0..BASE_FRAME)         callee-saved registers
    //   [BASE_FRAME..BASE_FRAME+format_extra)  format-specific data (bitset, key_ptr, etc.)
    //   [BASE_FRAME+format_extra..BASE_FRAME+format_extra+8)  saved out pointer
    //   [BASE_FRAME+format_extra+8..BASE_FRAME+format_extra+8+max_inner)  inner T scratch
    //
    // option_scratch_offset is the absolute sp offset where inner T is written.
    // The emit_redirect_out_to_stack method saves old out at scratch_offset-8.
    let max_inner = max_option_inner_size(shape, &mut std::collections::HashSet::new());
    let (option_scratch_offset, extra_stack) = if max_inner > 0 {
        let option_extra = 8 + max_inner as u32;
        let base_frame = crate::arch::BASE_FRAME;
        let scratch_offset = base_frame + format_extra + 8;
        (scratch_offset, format_extra + option_extra)
    } else {
        (0, format_extra)
    };

    let mut compiler = Compiler::new(extra_stack, option_scratch_offset, format);
    compiler.compile_shape(shape);

    // Get the entry offset for the root shape.
    let key = shape as *const Shape;
    let entry_offset = compiler.shapes[&key]
        .offset
        .expect("root shape was not fully compiled");

    let buf = compiler.ectx.finalize();
    let func: unsafe extern "C" fn(*mut u8, *mut crate::context::DeserContext) =
        unsafe { core::mem::transmute(buf.ptr(entry_offset)) };

    CompiledDeser {
        buf,
        entry: entry_offset,
        func,
    }
}
