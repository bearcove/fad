use facet::ScalarType;

use crate::arch::EmitCtx;
use crate::format::{FieldEmitInfo, Format};
use crate::intrinsics;

// r[impl deser.postcard.struct]

/// Postcard wire format — fields in declaration order, varint-encoded integers,
/// length-prefixed strings.
pub struct FadPostcard;

impl Format for FadPostcard {
    fn supports_inline_nested(&self) -> bool {
        true
    }

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
        let offset = offset as u32;
        match scalar_type {
            // Tier 1: fixed-size, fully inlined
            ScalarType::U8 | ScalarType::I8 => ectx.emit_inline_read_byte(offset),
            ScalarType::Bool => ectx.emit_inline_read_bool(offset),
            ScalarType::F32 => ectx.emit_inline_read_f32(offset),
            ScalarType::F64 => ectx.emit_inline_read_f64(offset),

            // Tier 2: varint fast path + intrinsic slow path
            ScalarType::U16 => ectx.emit_inline_varint_fast_path(
                offset, 2, false, intrinsics::fad_read_u16 as _,
            ),
            ScalarType::U32 => ectx.emit_inline_varint_fast_path(
                offset, 4, false, intrinsics::fad_read_varint_u32 as _,
            ),
            ScalarType::U64 => ectx.emit_inline_varint_fast_path(
                offset, 8, false, intrinsics::fad_read_u64 as _,
            ),
            ScalarType::I16 => ectx.emit_inline_varint_fast_path(
                offset, 2, true, intrinsics::fad_read_i16 as _,
            ),
            ScalarType::I32 => ectx.emit_inline_varint_fast_path(
                offset, 4, true, intrinsics::fad_read_i32 as _,
            ),
            ScalarType::I64 => ectx.emit_inline_varint_fast_path(
                offset, 8, true, intrinsics::fad_read_i64 as _,
            ),
            _ => panic!("unsupported postcard scalar: {:?}", scalar_type),
        }
    }

    fn emit_read_string(&self, ectx: &mut EmitCtx, offset: usize) {
        let fn_ptr = intrinsics::fad_read_postcard_string as *const u8;
        ectx.emit_call_intrinsic(fn_ptr, offset as u32);
    }
}
