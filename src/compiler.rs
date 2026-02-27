use std::collections::HashMap;

use dynasmrt::{AssemblyOffset, DynamicLabel};
use facet::{EnumRepr, ScalarType, Shape, Type, UserType};

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
}

impl<'fmt> Compiler<'fmt> {
    fn new(extra_stack: u32, format: &'fmt dyn Format) -> Self {
        Compiler {
            ectx: EmitCtx::new(extra_stack),
            format,
            shapes: HashMap::new(),
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
        let nested: Vec<Option<DynamicLabel>> = if inline {
            fields
                .iter()
                .map(|f| {
                    if matches!(&f.shape.ty, Type::User(UserType::Enum(_))) {
                        Some(self.compile_shape(f.shape))
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            fields
                .iter()
                .map(|f| {
                    if is_composite(f.shape) {
                        Some(self.compile_shape(f.shape))
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
        let ectx = &mut self.ectx;

        format.emit_struct_fields(ectx, &fields, &mut |ectx, field| {
            emit_field(ectx, format, field, &fields, &nested);
        });

        self.ectx.end_func(error_exit);

        // Mark as finished with the resolved offset.
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
        let nested_labels: Vec<Vec<Option<DynamicLabel>>> = if inline {
            variants
                .iter()
                .map(|v| {
                    v.fields
                        .iter()
                        .map(|f| {
                            if matches!(&f.shape.ty, Type::User(UserType::Enum(_))) {
                                Some(self.compile_shape(f.shape))
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
                            if is_composite(f.shape) {
                                Some(self.compile_shape(f.shape))
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
                    emit_field(ectx, format, field, &variant.fields, nested);
                });
                return;
            }

            for field in &variant.fields {
                emit_field(ectx, format, field, &variant.fields, nested);
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
                        emit_field(ectx, format, field, &variant.fields, nested);
                    },
                );
            }

            // Untagged: not this milestone
            (_, _, true) => {
                panic!("untagged enums not yet supported by fad");
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

/// Returns true if the shape is a struct type.
fn is_struct(shape: &'static Shape) -> bool {
    matches!(&shape.ty, Type::User(UserType::Struct(_)))
}

/// Returns true if the shape needs its own compiled function (struct or enum).
fn is_composite(shape: &'static Shape) -> bool {
    matches!(
        &shape.ty,
        Type::User(UserType::Struct(_) | UserType::Enum(_))
    )
}

/// Emit code for a single field, dispatching to nested struct calls, inline
/// expansion, or scalar intrinsics.
fn emit_field(
    ectx: &mut EmitCtx,
    format: &dyn Format,
    field: &FieldEmitInfo,
    all_fields: &[FieldEmitInfo],
    nested: &[Option<DynamicLabel>],
) {
    // Find this field's index by matching offset.
    let idx = all_fields
        .iter()
        .position(|f| f.offset == field.offset)
        .expect("field not found in all_fields");

    if let Some(label) = nested[idx] {
        // r[impl deser.nested-struct]
        // r[impl deser.nested-struct.offset]
        ectx.emit_call_emitted_func(label, field.offset as u32);
        return;
    }

    // r[impl deser.nested-struct]
    // r[impl deser.nested-struct.offset]
    if is_struct(field.shape) && format.supports_inline_nested() {
        emit_inline_struct(ectx, format, field.shape, field.offset);
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
        emit_field(ectx, format, field, &adjusted, &no_nested);
    });
}

/// Compile a deserializer for the given shape and format.
pub fn compile_deser(shape: &'static Shape, format: &dyn Format) -> CompiledDeser {
    // Compute extra stack space. For structs, pass fields. For enums, we need
    // the max across all variant bodies. Use empty fields for enums since JSON
    // enum handling computes its own stack needs.
    let extra_stack = match &shape.ty {
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
    };

    let mut compiler = Compiler::new(extra_stack, format);
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
