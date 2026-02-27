use facet::ScalarType;

use crate::arch::EmitCtx;
use crate::format::{FieldEmitInfo, Format};
use crate::intrinsics;

// r[impl deser.postcard.struct]

/// Postcard wire format — fields in declaration order, varint-encoded integers,
/// length-prefixed strings.
pub struct FadPostcard;

impl Format for FadPostcard {
    fn emit_struct_fields(
        &self,
        ectx: &mut EmitCtx,
        fields: &[FieldEmitInfo],
        emit_field: &mut dyn FnMut(&mut EmitCtx, &FieldEmitInfo),
    ) {
        // Postcard: fields are serialized in declaration order, no keys.
        for field in fields {
            emit_field(ectx, field);
        }
    }

    // r[impl deser.postcard.scalar.varint]
    // r[impl deser.postcard.scalar.float]
    // r[impl deser.postcard.scalar.bool]
    fn emit_read_scalar(&self, ectx: &mut EmitCtx, offset: usize, scalar_type: ScalarType) {
        let fn_ptr: *const u8 = match scalar_type {
            ScalarType::U8 => intrinsics::fad_read_u8 as _,
            ScalarType::U16 => intrinsics::fad_read_u16 as _,
            ScalarType::U32 => intrinsics::fad_read_varint_u32 as _,
            ScalarType::U64 => intrinsics::fad_read_u64 as _,
            ScalarType::I8 => intrinsics::fad_read_i8 as _,
            ScalarType::I16 => intrinsics::fad_read_i16 as _,
            ScalarType::I32 => intrinsics::fad_read_i32 as _,
            ScalarType::I64 => intrinsics::fad_read_i64 as _,
            ScalarType::F32 => intrinsics::fad_read_f32 as _,
            ScalarType::F64 => intrinsics::fad_read_f64 as _,
            ScalarType::Bool => intrinsics::fad_read_bool as _,
            _ => panic!("unsupported postcard scalar: {:?}", scalar_type),
        };
        ectx.emit_call_intrinsic(fn_ptr, offset as u32);
    }

    fn emit_read_string(&self, ectx: &mut EmitCtx, offset: usize) {
        let fn_ptr = intrinsics::fad_read_postcard_string as *const u8;
        ectx.emit_call_intrinsic(fn_ptr, offset as u32);
    }
}
