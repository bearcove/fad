use std::collections::HashMap;

use dynasmrt::{AssemblyOffset, DynamicLabel};
use facet::{ScalarType, Shape, Type, UserType};

use crate::arch::EmitCtx;
use crate::format::{FieldEmitInfo, Format};

/// A compiled deserializer. Owns the executable buffer containing JIT'd machine code.
pub struct CompiledDeser {
    _buf: dynasmrt::ExecutableBuffer,
    func: unsafe extern "C" fn(*mut u8, *mut crate::context::DeserContext),
}

impl CompiledDeser {
    pub(crate) fn func(&self) -> unsafe extern "C" fn(*mut u8, *mut crate::context::DeserContext) {
        self.func
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
            // Either already finished or in-progress (forward ref). Either way,
            // the label is valid for emitting a call.
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

        // Collect fields for this shape.
        let fields: Vec<FieldEmitInfo> = match &shape.ty {
            Type::User(UserType::Struct(st)) => st
                .fields
                .iter()
                .enumerate()
                .map(|(i, f)| FieldEmitInfo {
                    offset: f.offset,
                    shape: f.shape(),
                    name: f.name,
                    required_index: i,
                })
                .collect(),
            _ => panic!("unsupported shape: {}", shape.type_identifier),
        };

        // Depth-first: compile all nested struct fields before this function.
        // For non-recursive types, their labels will already be bound (backward call).
        // For recursive back-edges, the label is allocated but not bound (forward ref).
        let nested: Vec<Option<DynamicLabel>> = fields
            .iter()
            .map(|f| {
                if is_struct(f.shape) {
                    Some(self.compile_shape(f.shape))
                } else {
                    None
                }
            })
            .collect();

        // Bind the entry label at the function start, then emit prologue.
        self.ectx.bind_label(entry_label);
        let (entry_offset, error_exit) = self.ectx.begin_func();

        let format = self.format;
        let ectx = &mut self.ectx;

        format.emit_struct_fields(ectx, &fields, &mut |ectx, field| {
            emit_field_with_nested(ectx, format, field, &fields, &nested);
        });

        self.ectx.end_func(error_exit);

        // Mark as finished with the resolved offset.
        self.shapes.get_mut(&key).unwrap().offset = Some(entry_offset);

        entry_label
    }
}

/// Returns true if the shape is a struct type.
fn is_struct(shape: &'static Shape) -> bool {
    matches!(&shape.ty, Type::User(UserType::Struct(_)))
}

/// Emit code for a single field, dispatching to nested struct calls or scalar intrinsics.
fn emit_field_with_nested(
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

/// Compile a deserializer for the given shape and format.
pub fn compile_deser(shape: &'static Shape, format: &dyn Format) -> CompiledDeser {
    // Compute extra stack from the top-level shape's fields.
    let fields: Vec<FieldEmitInfo> = match &shape.ty {
        Type::User(UserType::Struct(st)) => st
            .fields
            .iter()
            .enumerate()
            .map(|(i, f)| FieldEmitInfo {
                offset: f.offset,
                shape: f.shape(),
                name: f.name,
                required_index: i,
            })
            .collect(),
        _ => panic!("unsupported shape: {}", shape.type_identifier),
    };

    let extra_stack = format.extra_stack_space(&fields);
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

    CompiledDeser { _buf: buf, func }
}
