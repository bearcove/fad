use facet::{ScalarType, Shape, Type, UserType};

use crate::arch::EmitCtx;
use crate::format::{FieldEmitInfo, Format};

// [impl compiler.shape-walk]

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

/// Compile a deserializer for the given shape and format.
///
/// Currently supports: structs with u32 and String fields.
pub fn compile_deser(shape: &'static Shape, format: &dyn Format) -> CompiledDeser {
    let mut ectx = EmitCtx::new();

    match &shape.ty {
        Type::User(UserType::Struct(st)) => {
            let fields: Vec<FieldEmitInfo> = st
                .fields
                .iter()
                .map(|f| FieldEmitInfo {
                    offset: f.offset,
                    shape: f.shape(),
                    name: f.name,
                })
                .collect();

            format.emit_struct_fields(&mut ectx, &fields, &mut |ectx, field| {
                emit_field(ectx, format, field);
            });
        }
        _ => panic!("unsupported shape: {}", shape.type_identifier),
    }

    let entry = ectx.entry;
    let buf = ectx.finalize();
    let func: unsafe extern "C" fn(*mut u8, *mut crate::context::DeserContext) =
        unsafe { core::mem::transmute(buf.ptr(entry)) };

    CompiledDeser { _buf: buf, func }
}

fn emit_field(ectx: &mut EmitCtx, format: &dyn Format, field: &FieldEmitInfo) {
    match field.shape.scalar_type() {
        Some(ScalarType::U32) => {
            format.emit_read_u32(ectx, field.offset);
        }
        Some(ScalarType::String) => {
            format.emit_read_string(ectx, field.offset);
        }
        _ => panic!(
            "unsupported field type: {} (scalar_type={:?})",
            field.shape.type_identifier,
            field.shape.scalar_type()
        ),
    }
}
