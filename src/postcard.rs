use dynasmrt::DynamicLabel;
use facet::ScalarType;

use crate::arch::{EmitCtx, BASE_FRAME};
use crate::format::{FieldEmitInfo, Format, VariantEmitInfo};
use crate::intrinsics;
use crate::malum::VecOffsets;

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

    fn emit_read_string(&self, ectx: &mut EmitCtx, offset: usize, string_offsets: &crate::malum::StringOffsets) {
        ectx.emit_inline_postcard_string_malum(
            offset as u32,
            string_offsets,
            intrinsics::fad_read_varint_u32 as *const u8,
            intrinsics::fad_string_validate_alloc_copy as *const u8,
        );
    }

    // r[impl deser.postcard.option]
    fn emit_option(
        &self,
        ectx: &mut EmitCtx,
        offset: usize,
        init_none_fn: *const u8,
        init_some_fn: *const u8,
        scratch_offset: u32,
        emit_inner: &mut dyn FnMut(&mut EmitCtx),
    ) {
        // Postcard Option: 0x00 = None, 0x01 = Some(T)
        let none_label = ectx.new_label();
        let done_label = ectx.new_label();

        // Read tag byte from input into a stack slot (the save slot area).
        let tag_slot = scratch_offset - 8;
        ectx.emit_inline_read_byte_to_stack(tag_slot);

        // Compare: if tag == 0, branch to None
        ectx.emit_stack_byte_cmp_branch(tag_slot, 0, none_label);

        // === Some path ===
        // Redirect out to the scratch area and deserialize inner T
        ectx.emit_redirect_out_to_stack(scratch_offset);
        emit_inner(ectx);
        ectx.emit_restore_out(scratch_offset);

        // Call init_some(init_some_fn, out + offset, scratch)
        ectx.emit_call_option_init_some(
            intrinsics::fad_option_init_some as *const u8,
            init_some_fn,
            offset as u32,
            scratch_offset,
        );
        ectx.emit_branch(done_label);

        // === None path ===
        ectx.bind_label(none_label);
        ectx.emit_call_option_init_none(
            intrinsics::fad_option_init_none as *const u8,
            init_none_fn,
            offset as u32,
        );

        ectx.bind_label(done_label);
    }

    fn vec_extra_stack_space(&self) -> u32 {
        // varint scratch (8) + saved_out (8) + buf (8) + count (8) + cursor (8) + end (8) = 48
        48
    }

    // r[impl deser.postcard.seq]
    // r[impl seq.malum.postcard]
    #[allow(clippy::too_many_arguments)]
    fn emit_vec(
        &self,
        ectx: &mut EmitCtx,
        offset: usize,
        elem_shape: &'static facet::Shape,
        _elem_label: Option<DynamicLabel>,
        vec_offsets: &VecOffsets,
        _option_scratch_offset: u32,
        emit_elem: &mut dyn FnMut(&mut EmitCtx),
    ) {
        let elem_layout = elem_shape
            .layout
            .sized_layout()
            .expect("Vec element must be Sized");
        let elem_size = elem_layout.size() as u32;
        let elem_align = elem_layout.align() as u32;

        // Stack slot offsets (relative to sp)
        let saved_out_slot = BASE_FRAME + 8;
        let buf_slot = BASE_FRAME + 16;
        let count_slot = BASE_FRAME + 24;
        let save_x23_slot = BASE_FRAME + 32;  // save/restore callee-saved x23 (cursor)
        let save_x24_slot = BASE_FRAME + 40;  // save/restore callee-saved x24 (end)

        let empty_label = ectx.new_label();
        let done_label = ectx.new_label();
        let loop_done_label = ectx.new_label(); // loop finished → write Vec to output
        let loop_label = ectx.new_label();
        let error_cleanup = ectx.new_label();

        // Read element count (varint → w9)
        ectx.emit_read_postcard_discriminant(intrinsics::fad_read_varint_u32 as *const u8);

        // r[impl seq.malum.empty]
        // If count == 0, write empty Vec and skip
        ectx.emit_cbz_count(empty_label);

        // Save count to stack before alloc call (caller-saved reg, clobbered by call)
        ectx.emit_save_count_to_stack(count_slot);

        // Allocate: fad_vec_alloc(ctx, count, elem_size, elem_align)
        // count is in w9, result in x0
        ectx.emit_call_vec_alloc(
            intrinsics::fad_vec_alloc as *const u8,
            elem_size,
            elem_align,
        );

        // Init loop: save x23/x24, set x23=cursor=buf, x24=end
        ectx.emit_vec_loop_init_cursor(
            saved_out_slot,
            buf_slot,
            count_slot,
            save_x23_slot,
            save_x24_slot,
            elem_size,
        );

        // === Loop body ===
        ectx.bind_label(loop_label);

        // Check if we can use the tight varint loop (writes directly to cursor
        // register, no mov to out, slow path out-of-line).
        let varint_info = elem_shape.scalar_type().and_then(|st| {
            match st {
                ScalarType::U16 => Some((2, false, intrinsics::fad_read_u16 as *const u8)),
                ScalarType::U32 => Some((4, false, intrinsics::fad_read_varint_u32 as *const u8)),
                ScalarType::U64 => Some((8, false, intrinsics::fad_read_u64 as *const u8)),
                ScalarType::I16 => Some((2, true, intrinsics::fad_read_i16 as *const u8)),
                ScalarType::I32 => Some((4, true, intrinsics::fad_read_i32 as *const u8)),
                ScalarType::I64 => Some((8, true, intrinsics::fad_read_i64 as *const u8)),
                _ => None,
            }
        });

        if let Some((store_width, zigzag, intrinsic_fn_ptr)) = varint_info {
            // Tight varint loop: writes directly to cursor register,
            // no mov x21/r14, slow path placed out-of-line.
            ectx.emit_vec_varint_loop(
                store_width,
                zigzag,
                intrinsic_fn_ptr,
                elem_size,
                save_x24_slot,
                loop_label,
                loop_done_label,
                error_cleanup,
            );
        } else {
            // Generic path for non-varint elements (strings, structs, etc.)
            ectx.emit_vec_loop_load_cursor(save_x23_slot);

            // Redirect error_exit to error_cleanup during element emission.
            let saved_error_exit = ectx.error_exit;
            ectx.error_exit = error_cleanup;

            emit_elem(ectx);

            ectx.error_exit = saved_error_exit;

            ectx.emit_vec_loop_advance_no_error_check(
                save_x24_slot,
                elem_size,
                loop_label,
            );
        }

        // === Write Vec to output (reached after loop completes) ===
        ectx.bind_label(loop_done_label);
        // For postcard, len == cap == count (exact allocation)
        ectx.emit_vec_restore_callee_saved(save_x23_slot, save_x24_slot);
        ectx.emit_vec_store(
            offset as u32,
            saved_out_slot,
            buf_slot,
            count_slot, // len = count
            count_slot, // cap = count
            vec_offsets,
        );
        ectx.emit_branch(done_label);

        // === Empty path (x23/x24 were never modified) ===
        ectx.bind_label(empty_label);
        ectx.emit_vec_store_empty_with_align(offset as u32, elem_align, vec_offsets);
        ectx.emit_branch(done_label);

        // === Error cleanup ===
        ectx.bind_label(error_cleanup);
        ectx.emit_vec_restore_callee_saved(save_x23_slot, save_x24_slot);
        ectx.emit_vec_error_cleanup(
            intrinsics::fad_vec_free as *const u8,
            saved_out_slot,
            buf_slot,
            count_slot,
            elem_size,
            elem_align,
        );

        ectx.bind_label(done_label);
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
