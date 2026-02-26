use crate::arch::EmitCtx;
use crate::format::{FieldEmitInfo, Format};
use crate::intrinsics;

// [impl deser.postcard.field-order]

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

    fn emit_read_u32(&self, ectx: &mut EmitCtx, offset: usize) {
        let fn_ptr = intrinsics::fad_read_varint_u32 as *const u8;
        ectx.emit_call_intrinsic(fn_ptr, offset as u32);
    }

    fn emit_read_string(&self, ectx: &mut EmitCtx, offset: usize) {
        let fn_ptr = intrinsics::fad_read_postcard_string as *const u8;
        ectx.emit_call_intrinsic(fn_ptr, offset as u32);
    }
}
