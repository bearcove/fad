use facet::ScalarType;

use crate::arch::EmitCtx;
use crate::format::{FieldEmitInfo, Format, VariantEmitInfo};
use crate::intrinsics;

// r[impl deser.postcard.struct]

/// Postcard wire format — fields in declaration order, varint-encoded integers,
/// length-prefixed strings.
pub struct FadPostcard;

impl Format for FadPostcard {
    fn extra_stack_space(&self, _fields: &[FieldEmitInfo]) -> u32 {
        // We need at least 8 bytes of scratch space on the stack for:
        // - varint slow path temp (used by emit_read_postcard_discriminant
        //   and emit_inline_postcard_string)
        8
    }

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
        ectx.emit_inline_postcard_string(
            offset as u32,
            intrinsics::fad_read_varint_u32 as *const u8,
            intrinsics::fad_postcard_validate_and_alloc_string as *const u8,
        );
    }

    // r[impl deser.postcard.enum]
    // r[impl deser.postcard.enum.dispatch]
    // r[impl deser.postcard.enum.unit]
    fn emit_enum(
        &self,
        ectx: &mut EmitCtx,
        variants: &[VariantEmitInfo],
        emit_variant_body: &mut dyn FnMut(&mut EmitCtx, &VariantEmitInfo),
    ) {
        // Read varint discriminant into scratch register (w9 on aarch64, r10d on x64).
        // Uses fad_read_varint_u32 as slow path intrinsic.
        ectx.emit_read_postcard_discriminant(intrinsics::fad_read_varint_u32 as *const u8);

        let done_label = ectx.new_label();
        let variant_labels: Vec<_> = variants.iter().map(|_| ectx.new_label()).collect();

        // Linear compare chain: cmp discriminant, N; branch if equal to variant N.
        for (i, variant) in variants.iter().enumerate() {
            ectx.emit_cmp_imm_branch_eq(variant.index as u32, variant_labels[i]);
        }

        // Default: unknown discriminant.
        ectx.emit_unknown_variant_error();

        // Variant handlers.
        for (i, variant) in variants.iter().enumerate() {
            ectx.bind_label(variant_labels[i]);
            emit_variant_body(ectx, variant);
            ectx.emit_branch(done_label);
        }

        ectx.bind_label(done_label);
    }
}
