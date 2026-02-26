use crate::arch::EmitCtx;
use crate::format::{FieldEmitInfo, Format};
use crate::json_intrinsics;

// r[impl deser.json.struct]
// r[impl deser.json.struct.unknown-keys]

// Stack layout (extra_stack = 32, offsets from sp):
//   [sp+0..48)   callee-saved registers (base frame)
//   [sp+48..56)  bitset (u64) — tracks which required fields have been seen
//   [sp+56..64)  key_ptr (*const u8) — borrowed pointer into input
//   [sp+64..72)  key_len (usize)
//   [sp+72..80)  comma_or_end result (u8 + padding)
const BITSET_OFFSET: u32 = 48;
const KEY_PTR_OFFSET: u32 = 56;
const KEY_LEN_OFFSET: u32 = 64;
const RESULT_BYTE_OFFSET: u32 = 72;

/// JSON wire format — key-value pairs, linear key dispatch.
pub struct FadJson;

impl Format for FadJson {
    fn extra_stack_space(&self, _fields: &[FieldEmitInfo]) -> u32 {
        32
    }

    fn emit_struct_fields(
        &self,
        ectx: &mut EmitCtx,
        fields: &[FieldEmitInfo],
        emit_field: &mut dyn FnMut(&mut EmitCtx, &FieldEmitInfo),
    ) {
        let after_loop = ectx.new_label();
        let loop_top = ectx.new_label();
        let empty_object = ectx.new_label();

        // Zero the bitset
        ectx.emit_zero_stack_slot(BITSET_OFFSET);

        // expect '{'
        ectx.emit_call_intrinsic_ctx_only(
            json_intrinsics::fad_json_expect_object_start as *const u8,
        );

        // peek after whitespace — check for empty object
        ectx.emit_call_intrinsic_ctx_and_stack_out(
            json_intrinsics::fad_json_peek_after_ws as *const u8,
            RESULT_BYTE_OFFSET,
        );
        // If peek == '}', branch to empty_object handler
        ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, b'}', empty_object);

        // === loop_top ===
        ectx.bind_label(loop_top);

        // Read key → (key_ptr, key_len) on stack
        ectx.emit_call_intrinsic_ctx_and_two_stack_outs(
            json_intrinsics::fad_json_read_key as *const u8,
            KEY_PTR_OFFSET,
            KEY_LEN_OFFSET,
        );

        // expect ':'
        ectx.emit_call_intrinsic_ctx_only(json_intrinsics::fad_json_expect_colon as *const u8);

        // Linear key dispatch chain
        let after_dispatch = ectx.new_label();
        let match_labels: Vec<_> = fields.iter().map(|_| ectx.new_label()).collect();
        let unknown_key = ectx.new_label();

        // Emit comparisons: for each field, compare key and branch if match
        for (i, field) in fields.iter().enumerate() {
            let name_bytes = field.name.as_bytes();
            ectx.emit_call_pure_4arg(
                json_intrinsics::fad_json_key_equals as *const u8,
                KEY_PTR_OFFSET,
                KEY_LEN_OFFSET,
                name_bytes.as_ptr(),
                name_bytes.len() as u32,
            );
            ectx.emit_cbnz_x0(match_labels[i]);
        }

        // No match → unknown key: skip value
        ectx.emit_branch(unknown_key);

        // === match handlers ===
        for (i, field) in fields.iter().enumerate() {
            ectx.bind_label(match_labels[i]);
            emit_field(ectx, field);
            ectx.emit_set_bit_on_stack(BITSET_OFFSET, field.required_index as u32);
            ectx.emit_branch(after_dispatch);
        }

        // === unknown key handler ===
        ectx.bind_label(unknown_key);
        ectx.emit_call_intrinsic_ctx_only(json_intrinsics::fad_json_skip_value as *const u8);

        // === after_dispatch ===
        ectx.bind_label(after_dispatch);

        // comma_or_end → result byte: 0 = comma (continue), 1 = '}' (done)
        ectx.emit_call_intrinsic_ctx_and_stack_out(
            json_intrinsics::fad_json_comma_or_end_object as *const u8,
            RESULT_BYTE_OFFSET,
        );
        // If result == 1 ('}'), jump to after_loop
        ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, 1, after_loop);
        // Otherwise, loop back
        ectx.emit_branch(loop_top);

        // === empty_object: advance past '}' and fall through to after_loop ===
        ectx.bind_label(empty_object);
        ectx.emit_advance_cursor_by(1);

        // === after_loop ===
        ectx.bind_label(after_loop);

        // Check that all required fields were seen
        let expected_mask = (1u64 << fields.len()) - 1;
        ectx.emit_check_bitset(BITSET_OFFSET, expected_mask);
    }

    fn emit_read_u32(&self, ectx: &mut EmitCtx, offset: usize) {
        ectx.emit_call_intrinsic(
            json_intrinsics::fad_json_read_u32 as *const u8,
            offset as u32,
        );
    }

    fn emit_read_string(&self, ectx: &mut EmitCtx, offset: usize) {
        ectx.emit_call_intrinsic(
            json_intrinsics::fad_json_read_string_value as *const u8,
            offset as u32,
        );
    }
}
