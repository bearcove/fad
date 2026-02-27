use dynasmrt::DynamicLabel;
use facet::{ScalarType, Type, UserType};

use crate::arch::EmitCtx;
use crate::context::ErrorCode;
use crate::format::{FieldEmitInfo, Format, VariantEmitInfo, VariantKind};
use crate::json_intrinsics;
use crate::solver::LoweredSolver;

// r[impl deser.json.struct]
// r[impl deser.json.struct.unknown-keys]

// Stack layout (extra_stack = 48, offsets from sp):
//   [sp+0..48)   callee-saved registers (base frame)
//   [sp+48..56)  bitset (u64) — tracks which required fields have been seen
//   [sp+56..64)  key_ptr (*const u8) — borrowed pointer into input
//   [sp+64..72)  key_len (usize)
//   [sp+72..80)  comma_or_end result (u8 + padding)
//   [sp+80..88)  saved_cursor — saved input_ptr for solver restore
//   [sp+88..96)  candidates — solver bitmask for object bucket disambiguation
const BITSET_OFFSET: u32 = 48;
const KEY_PTR_OFFSET: u32 = 56;
const KEY_LEN_OFFSET: u32 = 64;
const RESULT_BYTE_OFFSET: u32 = 72;
const SAVED_CURSOR_OFFSET: u32 = 80;
const CANDIDATES_OFFSET: u32 = 88;

/// JSON wire format — key-value pairs, linear key dispatch.
pub struct FadJson;

impl Format for FadJson {
    fn extra_stack_space(&self, _fields: &[FieldEmitInfo]) -> u32 {
        48
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

    // r[impl deser.json.scalar.integer]
    // r[impl deser.json.scalar.float]
    // r[impl deser.json.scalar.bool]
    fn emit_read_scalar(&self, ectx: &mut EmitCtx, offset: usize, scalar_type: ScalarType) {
        let fn_ptr: *const u8 = match scalar_type {
            ScalarType::U8 => json_intrinsics::fad_json_read_u8 as _,
            ScalarType::U16 => json_intrinsics::fad_json_read_u16 as _,
            ScalarType::U32 => json_intrinsics::fad_json_read_u32 as _,
            ScalarType::U64 => json_intrinsics::fad_json_read_u64 as _,
            ScalarType::I8 => json_intrinsics::fad_json_read_i8 as _,
            ScalarType::I16 => json_intrinsics::fad_json_read_i16 as _,
            ScalarType::I32 => json_intrinsics::fad_json_read_i32 as _,
            ScalarType::I64 => json_intrinsics::fad_json_read_i64 as _,
            ScalarType::F32 => json_intrinsics::fad_json_read_f32 as _,
            ScalarType::F64 => json_intrinsics::fad_json_read_f64 as _,
            ScalarType::Bool => json_intrinsics::fad_json_read_bool as _,
            _ => panic!("unsupported JSON scalar: {:?}", scalar_type),
        };
        ectx.emit_call_intrinsic(fn_ptr, offset as u32);
    }

    fn emit_read_string(&self, ectx: &mut EmitCtx, offset: usize) {
        ectx.emit_call_intrinsic(
            json_intrinsics::fad_json_read_string_value as *const u8,
            offset as u32,
        );
    }

    // r[impl deser.json.enum.external]
    // r[impl deser.json.enum.external.unit-as-string]
    // r[impl deser.json.enum.external.struct-variant]
    // r[impl deser.json.enum.external.tuple-variant]
    fn emit_enum(
        &self,
        ectx: &mut EmitCtx,
        variants: &[VariantEmitInfo],
        emit_variant_body: &mut dyn FnMut(&mut EmitCtx, &VariantEmitInfo),
    ) {
        let done_label = ectx.new_label();
        let object_path = ectx.new_label();

        // Peek at first non-whitespace byte to determine path.
        ectx.emit_call_intrinsic_ctx_and_stack_out(
            json_intrinsics::fad_json_peek_after_ws as *const u8,
            RESULT_BYTE_OFFSET,
        );

        // If peek == '"', it's a bare string (unit variant).
        // Otherwise, expect an object.
        ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, b'{', object_path);

        // ══════════════════════════════════════════════════════════════════
        // Bare string path: read the quoted string, match unit variants.
        // ══════════════════════════════════════════════════════════════════
        ectx.emit_call_intrinsic_ctx_and_two_stack_outs(
            json_intrinsics::fad_json_read_key as *const u8,
            KEY_PTR_OFFSET,
            KEY_LEN_OFFSET,
        );

        // Compare against unit variant names.
        let unit_labels: Vec<_> = variants
            .iter()
            .filter(|v| v.kind == VariantKind::Unit)
            .map(|v| (v, ectx.new_label()))
            .collect();

        for (variant, label) in &unit_labels {
            let name_bytes = variant.name.as_bytes();
            ectx.emit_call_pure_4arg(
                json_intrinsics::fad_json_key_equals as *const u8,
                KEY_PTR_OFFSET,
                KEY_LEN_OFFSET,
                name_bytes.as_ptr(),
                name_bytes.len() as u32,
            );
            ectx.emit_cbnz_x0(*label);
        }

        // No match → unknown variant.
        ectx.emit_unknown_variant_error();

        // Unit variant handlers.
        for (variant, label) in &unit_labels {
            ectx.bind_label(*label);
            emit_variant_body(ectx, variant);
            ectx.emit_branch(done_label);
        }

        // ══════════════════════════════════════════════════════════════════
        // Object path: { "VariantName": value }
        // ══════════════════════════════════════════════════════════════════
        ectx.bind_label(object_path);

        // Consume '{'
        ectx.emit_call_intrinsic_ctx_only(
            json_intrinsics::fad_json_expect_object_start as *const u8,
        );

        // Read the variant name key.
        ectx.emit_call_intrinsic_ctx_and_two_stack_outs(
            json_intrinsics::fad_json_read_key as *const u8,
            KEY_PTR_OFFSET,
            KEY_LEN_OFFSET,
        );

        // Consume ':'
        ectx.emit_call_intrinsic_ctx_only(json_intrinsics::fad_json_expect_colon as *const u8);

        // Compare against all variant names.
        let variant_labels: Vec<_> = variants.iter().map(|_| ectx.new_label()).collect();

        for (i, variant) in variants.iter().enumerate() {
            let name_bytes = variant.name.as_bytes();
            ectx.emit_call_pure_4arg(
                json_intrinsics::fad_json_key_equals as *const u8,
                KEY_PTR_OFFSET,
                KEY_LEN_OFFSET,
                name_bytes.as_ptr(),
                name_bytes.len() as u32,
            );
            ectx.emit_cbnz_x0(variant_labels[i]);
        }

        // No match → unknown variant.
        ectx.emit_unknown_variant_error();

        // Variant handlers.
        let expect_end_label = ectx.new_label();
        for (i, variant) in variants.iter().enumerate() {
            ectx.bind_label(variant_labels[i]);
            match variant.kind {
                VariantKind::Unit => {
                    // Unit variant inside object: { "Cat": null } — skip the value.
                    emit_variant_body(ectx, variant);
                    ectx.emit_call_intrinsic_ctx_only(
                        json_intrinsics::fad_json_skip_value as *const u8,
                    );
                }
                VariantKind::Struct | VariantKind::Tuple => {
                    // Struct variant: { "Dog": { "name": "Rex", ... } }
                    // Tuple variant (newtype): { "Parrot": "Polly" }
                    // emit_variant_body handles both — for struct it calls
                    // emit_struct_fields, for tuple it emits the single field.
                    emit_variant_body(ectx, variant);
                }
            }
            ectx.emit_branch(expect_end_label);
        }

        // After variant body: expect closing '}'
        ectx.bind_label(expect_end_label);
        ectx.emit_call_intrinsic_ctx_only(
            json_intrinsics::fad_json_expect_object_end as *const u8,
        );

        ectx.bind_label(done_label);
    }

    // r[impl deser.json.enum.adjacent]
    // r[impl deser.json.enum.adjacent.key-order]
    // r[impl deser.json.enum.adjacent.unit-variant]
    // r[impl deser.json.enum.adjacent.tuple-variant]
    fn emit_enum_adjacent(
        &self,
        ectx: &mut EmitCtx,
        variants: &[VariantEmitInfo],
        tag_key: &'static str,
        content_key: &'static str,
        emit_variant_body: &mut dyn FnMut(&mut EmitCtx, &VariantEmitInfo),
    ) {
        let done_label = ectx.new_label();

        // expect '{'
        ectx.emit_call_intrinsic_ctx_only(
            json_intrinsics::fad_json_expect_object_start as *const u8,
        );

        // Read first key
        ectx.emit_call_intrinsic_ctx_and_two_stack_outs(
            json_intrinsics::fad_json_read_key as *const u8,
            KEY_PTR_OFFSET,
            KEY_LEN_OFFSET,
        );

        // Verify it equals tag_key, error if not
        let tag_key_bytes = tag_key.as_bytes();
        ectx.emit_call_pure_4arg(
            json_intrinsics::fad_json_key_equals as *const u8,
            KEY_PTR_OFFSET,
            KEY_LEN_OFFSET,
            tag_key_bytes.as_ptr(),
            tag_key_bytes.len() as u32,
        );
        let tag_ok = ectx.new_label();
        ectx.emit_cbnz_x0(tag_ok);
        ectx.emit_call_intrinsic_ctx_only(
            json_intrinsics::fad_json_error_expected_tag_key as *const u8,
        );
        ectx.bind_label(tag_ok);

        // expect ':'
        ectx.emit_call_intrinsic_ctx_only(json_intrinsics::fad_json_expect_colon as *const u8);

        // Read variant name string
        ectx.emit_call_intrinsic_ctx_and_two_stack_outs(
            json_intrinsics::fad_json_read_key as *const u8,
            KEY_PTR_OFFSET,
            KEY_LEN_OFFSET,
        );

        // Variant name dispatch (linear compare chain)
        let variant_labels: Vec<_> = variants.iter().map(|_| ectx.new_label()).collect();
        for (i, variant) in variants.iter().enumerate() {
            let name_bytes = variant.name.as_bytes();
            ectx.emit_call_pure_4arg(
                json_intrinsics::fad_json_key_equals as *const u8,
                KEY_PTR_OFFSET,
                KEY_LEN_OFFSET,
                name_bytes.as_ptr(),
                name_bytes.len() as u32,
            );
            ectx.emit_cbnz_x0(variant_labels[i]);
        }
        ectx.emit_unknown_variant_error();

        // Variant handlers
        for (i, variant) in variants.iter().enumerate() {
            ectx.bind_label(variant_labels[i]);
            match variant.kind {
                VariantKind::Unit => {
                    // Unit variant: discriminant only, content key is optional.
                    emit_variant_body(ectx, variant);

                    // comma_or_end: '}' means done, ',' means content key follows
                    ectx.emit_call_intrinsic_ctx_and_stack_out(
                        json_intrinsics::fad_json_comma_or_end_object as *const u8,
                        RESULT_BYTE_OFFSET,
                    );
                    let unit_done = ectx.new_label();
                    ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, 1, unit_done);

                    // Had comma → read content_key, ':', skip value (typically null), expect '}'
                    ectx.emit_call_intrinsic_ctx_and_two_stack_outs(
                        json_intrinsics::fad_json_read_key as *const u8,
                        KEY_PTR_OFFSET,
                        KEY_LEN_OFFSET,
                    );
                    ectx.emit_call_intrinsic_ctx_only(
                        json_intrinsics::fad_json_expect_colon as *const u8,
                    );
                    ectx.emit_call_intrinsic_ctx_only(
                        json_intrinsics::fad_json_skip_value as *const u8,
                    );
                    ectx.emit_call_intrinsic_ctx_only(
                        json_intrinsics::fad_json_expect_object_end as *const u8,
                    );

                    ectx.bind_label(unit_done);
                }
                VariantKind::Struct | VariantKind::Tuple => {
                    // Non-unit: expect comma, content key, colon, then variant body, then '}'
                    // comma_or_end → expect ','
                    ectx.emit_call_intrinsic_ctx_and_stack_out(
                        json_intrinsics::fad_json_comma_or_end_object as *const u8,
                        RESULT_BYTE_OFFSET,
                    );
                    // (If we got '}' here, the next read_key will error — that's fine)

                    // Read content key
                    ectx.emit_call_intrinsic_ctx_and_two_stack_outs(
                        json_intrinsics::fad_json_read_key as *const u8,
                        KEY_PTR_OFFSET,
                        KEY_LEN_OFFSET,
                    );

                    // Verify it equals content_key (optional check for better errors)
                    let ck_bytes = content_key.as_bytes();
                    ectx.emit_call_pure_4arg(
                        json_intrinsics::fad_json_key_equals as *const u8,
                        KEY_PTR_OFFSET,
                        KEY_LEN_OFFSET,
                        ck_bytes.as_ptr(),
                        ck_bytes.len() as u32,
                    );
                    // We don't error on mismatch for now — just proceed.
                    // TODO: add ExpectedContentKey error for stricter validation.

                    // expect ':'
                    ectx.emit_call_intrinsic_ctx_only(
                        json_intrinsics::fad_json_expect_colon as *const u8,
                    );

                    // Dispatch variant body (writes discriminant + deserializes struct/tuple)
                    emit_variant_body(ectx, variant);

                    // expect '}'
                    ectx.emit_call_intrinsic_ctx_only(
                        json_intrinsics::fad_json_expect_object_end as *const u8,
                    );
                }
            }
            ectx.emit_branch(done_label);
        }

        ectx.bind_label(done_label);
    }

    // r[impl deser.json.enum.internal]
    // r[impl deser.json.enum.internal.struct-only]
    // r[impl deser.json.enum.internal.unit-variant]
    fn emit_enum_internal(
        &self,
        ectx: &mut EmitCtx,
        variants: &[VariantEmitInfo],
        tag_key: &'static str,
        emit_variant_discriminant: &mut dyn FnMut(&mut EmitCtx, &VariantEmitInfo),
        emit_variant_field: &mut dyn FnMut(&mut EmitCtx, &VariantEmitInfo, &FieldEmitInfo),
    ) {
        let done_label = ectx.new_label();

        // expect '{'
        ectx.emit_call_intrinsic_ctx_only(
            json_intrinsics::fad_json_expect_object_start as *const u8,
        );

        // Read first key
        ectx.emit_call_intrinsic_ctx_and_two_stack_outs(
            json_intrinsics::fad_json_read_key as *const u8,
            KEY_PTR_OFFSET,
            KEY_LEN_OFFSET,
        );

        // Verify it equals tag_key
        let tag_key_bytes = tag_key.as_bytes();
        ectx.emit_call_pure_4arg(
            json_intrinsics::fad_json_key_equals as *const u8,
            KEY_PTR_OFFSET,
            KEY_LEN_OFFSET,
            tag_key_bytes.as_ptr(),
            tag_key_bytes.len() as u32,
        );
        let tag_ok = ectx.new_label();
        ectx.emit_cbnz_x0(tag_ok);
        ectx.emit_call_intrinsic_ctx_only(
            json_intrinsics::fad_json_error_expected_tag_key as *const u8,
        );
        ectx.bind_label(tag_ok);

        // expect ':'
        ectx.emit_call_intrinsic_ctx_only(json_intrinsics::fad_json_expect_colon as *const u8);

        // Read variant name string
        ectx.emit_call_intrinsic_ctx_and_two_stack_outs(
            json_intrinsics::fad_json_read_key as *const u8,
            KEY_PTR_OFFSET,
            KEY_LEN_OFFSET,
        );

        // Variant name dispatch
        let variant_labels: Vec<_> = variants.iter().map(|_| ectx.new_label()).collect();
        for (i, variant) in variants.iter().enumerate() {
            let name_bytes = variant.name.as_bytes();
            ectx.emit_call_pure_4arg(
                json_intrinsics::fad_json_key_equals as *const u8,
                KEY_PTR_OFFSET,
                KEY_LEN_OFFSET,
                name_bytes.as_ptr(),
                name_bytes.len() as u32,
            );
            ectx.emit_cbnz_x0(variant_labels[i]);
        }
        ectx.emit_unknown_variant_error();

        // Variant handlers
        for (i, variant) in variants.iter().enumerate() {
            ectx.bind_label(variant_labels[i]);
            match variant.kind {
                VariantKind::Unit => {
                    emit_variant_discriminant(ectx, variant);
                    // Expect closing '}' (possibly after comma + unknown keys)
                    ectx.emit_call_intrinsic_ctx_only(
                        json_intrinsics::fad_json_expect_object_end as *const u8,
                    );
                }
                VariantKind::Struct => {
                    emit_variant_discriminant(ectx, variant);
                    // Remaining keys are the variant's fields — use continuation
                    self.emit_struct_fields_continuation(
                        ectx,
                        &variant.fields,
                        &mut |ectx, field| {
                            emit_variant_field(ectx, variant, field);
                        },
                    );
                }
                VariantKind::Tuple => {
                    // r[impl deser.json.enum.internal.struct-only]
                    panic!(
                        "internally tagged enums do not support tuple variants \
                         (variant \"{}\")",
                        variant.name
                    );
                }
            }
            ectx.emit_branch(done_label);
        }

        ectx.bind_label(done_label);
    }

    // r[impl deser.json.enum.untagged]
    // r[impl deser.json.enum.untagged.bucket]
    // r[impl deser.json.enum.untagged.peek]
    // r[impl deser.json.enum.untagged.object-solver]
    // r[impl deser.json.enum.untagged.string-trie]
    // r[impl deser.json.enum.untagged.scalar-unique]
    fn emit_enum_untagged(
        &self,
        ectx: &mut EmitCtx,
        variants: &[VariantEmitInfo],
        emit_variant_body: &mut dyn FnMut(&mut EmitCtx, &VariantEmitInfo),
    ) {
        // ══════════════════════════════════════════════════════════════════
        // Layer 1: Bucket variants by JSON value type (JIT-compile time)
        // ══════════════════════════════════════════════════════════════════

        let mut object_variants: Vec<usize> = Vec::new();
        let mut string_variants: Vec<usize> = Vec::new();
        let mut bool_variants: Vec<usize> = Vec::new();
        let mut number_variants: Vec<usize> = Vec::new();
        let null_variants: Vec<usize> = Vec::new();

        for (i, variant) in variants.iter().enumerate() {
            match variant.kind {
                VariantKind::Unit => {
                    string_variants.push(i);
                }
                VariantKind::Struct => {
                    object_variants.push(i);
                }
                VariantKind::Tuple => {
                    assert!(
                        variant.fields.len() == 1,
                        "untagged tuple variant {} has {} fields, expected 1",
                        variant.name,
                        variant.fields.len()
                    );
                    let inner_shape = variant.fields[0].shape;
                    match &inner_shape.ty {
                        Type::User(UserType::Struct(_)) => {
                            object_variants.push(i);
                        }
                        _ => match inner_shape.scalar_type() {
                            Some(ScalarType::String) => string_variants.push(i),
                            Some(ScalarType::Bool) => bool_variants.push(i),
                            Some(
                                ScalarType::U8
                                | ScalarType::U16
                                | ScalarType::U32
                                | ScalarType::U64
                                | ScalarType::I8
                                | ScalarType::I16
                                | ScalarType::I32
                                | ScalarType::I64
                                | ScalarType::F32
                                | ScalarType::F64,
                            ) => number_variants.push(i),
                            _ => panic!(
                                "unsupported inner type for untagged tuple variant {}: {}",
                                variant.name, inner_shape.type_identifier
                            ),
                        },
                    }
                }
            }
        }

        // Validate scalar bucket uniqueness
        assert!(
            bool_variants.len() <= 1,
            "untagged enum has {} bool variants — at most 1 allowed",
            bool_variants.len()
        );
        assert!(
            number_variants.len() <= 1,
            "untagged enum has {} number variants — at most 1 allowed",
            number_variants.len()
        );
        let string_newtype_count = string_variants
            .iter()
            .filter(|&&i| variants[i].kind == VariantKind::Tuple)
            .count();
        assert!(
            string_newtype_count <= 1,
            "untagged enum has {} newtype String variants — at most 1 allowed",
            string_newtype_count
        );

        // ══════════════════════════════════════════════════════════════════
        // Layer 2: Peek dispatch (emitted code)
        // ══════════════════════════════════════════════════════════════════

        let done_label = ectx.new_label();

        // Peek at first non-whitespace byte
        ectx.emit_call_intrinsic_ctx_and_stack_out(
            json_intrinsics::fad_json_peek_after_ws as *const u8,
            RESULT_BYTE_OFFSET,
        );

        let object_label = ectx.new_label();
        let string_label = ectx.new_label();
        let bool_label = ectx.new_label();
        let number_label = ectx.new_label();
        let null_label = ectx.new_label();

        if !object_variants.is_empty() {
            ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, b'{', object_label);
        }
        if !string_variants.is_empty() {
            ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, b'"', string_label);
        }
        if !bool_variants.is_empty() {
            ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, b't', bool_label);
            ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, b'f', bool_label);
        }
        if !null_variants.is_empty() {
            ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, b'n', null_label);
        }
        if !number_variants.is_empty() {
            ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, b'-', number_label);
            for d in b'0'..=b'9' {
                ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, d, number_label);
            }
        }

        // No bucket matched
        ectx.emit_unknown_variant_error();

        // ══════════════════════════════════════════════════════════════════
        // Layer 3: Within-bucket disambiguation
        // ══════════════════════════════════════════════════════════════════

        // ── Object bucket ────────────────────────────────────────────────
        if !object_variants.is_empty() {
            ectx.bind_label(object_label);
            if object_variants.len() == 1 {
                emit_variant_body(ectx, &variants[object_variants[0]]);
            } else {
                let solver_variants: Vec<(usize, &VariantEmitInfo)> = object_variants
                    .iter()
                    .map(|&i| (i, &variants[i]))
                    .collect();
                let solver = LoweredSolver::build(&solver_variants);
                self.emit_object_solver(ectx, &solver, variants, done_label, emit_variant_body);
            }
            ectx.emit_branch(done_label);
        }

        // ── String bucket ────────────────────────────────────────────────
        if !string_variants.is_empty() {
            ectx.bind_label(string_label);

            let has_newtype = string_variants
                .iter()
                .any(|&i| variants[i].kind == VariantKind::Tuple);

            if has_newtype {
                // Save cursor so we can restore for the newtype String fallback.
                // read_key consumes the string, but emit_variant_body for the
                // newtype needs to read it again via read_string_value.
                ectx.emit_save_input_ptr(SAVED_CURSOR_OFFSET);
            }

            // Read the string as a borrowed key (ptr + len)
            ectx.emit_call_intrinsic_ctx_and_two_stack_outs(
                json_intrinsics::fad_json_read_key as *const u8,
                KEY_PTR_OFFSET,
                KEY_LEN_OFFSET,
            );

            // Try unit variant names
            let unit_labels: Vec<_> = string_variants
                .iter()
                .filter(|&&i| variants[i].kind == VariantKind::Unit)
                .map(|&i| (i, ectx.new_label()))
                .collect();

            for &(vi, label) in &unit_labels {
                let name_bytes = variants[vi].name.as_bytes();
                ectx.emit_call_pure_4arg(
                    json_intrinsics::fad_json_key_equals as *const u8,
                    KEY_PTR_OFFSET,
                    KEY_LEN_OFFSET,
                    name_bytes.as_ptr(),
                    name_bytes.len() as u32,
                );
                ectx.emit_cbnz_x0(label);
            }

            // No unit name matched
            if let Some(&nt_idx) = string_variants
                .iter()
                .find(|&&i| variants[i].kind == VariantKind::Tuple)
            {
                // Restore cursor to before the string, then call emit_variant_body
                // which will re-read it via read_string_value (allocating a String).
                ectx.emit_restore_input_ptr(SAVED_CURSOR_OFFSET);
                emit_variant_body(ectx, &variants[nt_idx]);
                ectx.emit_branch(done_label);
            } else {
                ectx.emit_unknown_variant_error();
            }

            // Unit variant handlers
            for &(vi, label) in &unit_labels {
                ectx.bind_label(label);
                emit_variant_body(ectx, &variants[vi]);
                ectx.emit_branch(done_label);
            }
        }

        // ── Bool bucket ──────────────────────────────────────────────────
        if !bool_variants.is_empty() {
            ectx.bind_label(bool_label);
            emit_variant_body(ectx, &variants[bool_variants[0]]);
            ectx.emit_branch(done_label);
        }

        // ── Number bucket ────────────────────────────────────────────────
        if !number_variants.is_empty() {
            ectx.bind_label(number_label);
            emit_variant_body(ectx, &variants[number_variants[0]]);
            ectx.emit_branch(done_label);
        }

        // ── Null bucket ──────────────────────────────────────────────────
        if !null_variants.is_empty() {
            ectx.bind_label(null_label);
            emit_variant_body(ectx, &variants[null_variants[0]]);
            ectx.emit_branch(done_label);
        }

        ectx.bind_label(done_label);
    }

    fn emit_struct_fields_continuation(
        &self,
        ectx: &mut EmitCtx,
        fields: &[FieldEmitInfo],
        emit_field: &mut dyn FnMut(&mut EmitCtx, &FieldEmitInfo),
    ) {
        let after_loop = ectx.new_label();
        let loop_top = ectx.new_label();

        // Zero the bitset
        ectx.emit_zero_stack_slot(BITSET_OFFSET);

        // We're already inside the object, right after reading "tag_key": "VariantName".
        // Next is either ',' (more fields) or '}' (no variant fields).
        ectx.emit_call_intrinsic_ctx_and_stack_out(
            json_intrinsics::fad_json_comma_or_end_object as *const u8,
            RESULT_BYTE_OFFSET,
        );
        ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, 1, after_loop);

        // === loop_top ===
        ectx.bind_label(loop_top);

        // Read key
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

        ectx.emit_branch(unknown_key);

        // Match handlers
        for (i, field) in fields.iter().enumerate() {
            ectx.bind_label(match_labels[i]);
            emit_field(ectx, field);
            ectx.emit_set_bit_on_stack(BITSET_OFFSET, field.required_index as u32);
            ectx.emit_branch(after_dispatch);
        }

        // Unknown key handler
        ectx.bind_label(unknown_key);
        ectx.emit_call_intrinsic_ctx_only(json_intrinsics::fad_json_skip_value as *const u8);

        // After dispatch
        ectx.bind_label(after_dispatch);

        // comma_or_end
        ectx.emit_call_intrinsic_ctx_and_stack_out(
            json_intrinsics::fad_json_comma_or_end_object as *const u8,
            RESULT_BYTE_OFFSET,
        );
        ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, 1, after_loop);
        ectx.emit_branch(loop_top);

        // === after_loop ===
        ectx.bind_label(after_loop);

        // Check that all required fields were seen
        let expected_mask = (1u64 << fields.len()) - 1;
        ectx.emit_check_bitset(BITSET_OFFSET, expected_mask);
    }
}

impl FadJson {
    /// Emit the object-bucket solver for untagged enum disambiguation.
    ///
    /// Two-pass approach:
    /// 1. Save cursor, scan all keys (without parsing values), AND per-key masks
    ///    into the candidates bitmask until popcount == 1 or end of object.
    /// 2. Restore cursor, dispatch to the resolved variant's body.
    fn emit_object_solver(
        &self,
        ectx: &mut EmitCtx,
        solver: &LoweredSolver,
        variants: &[VariantEmitInfo],
        done_label: DynamicLabel,
        emit_variant_body: &mut dyn FnMut(&mut EmitCtx, &VariantEmitInfo),
    ) {
        let resolve_label = ectx.new_label();
        let error_no_match = ectx.new_label();
        let error_ambiguous = ectx.new_label();
        let scan_loop = ectx.new_label();

        // ── Pass 1: Save cursor, scan keys, narrow candidates ────────

        // Save cursor (input_ptr) before consuming the object
        ectx.emit_save_input_ptr(SAVED_CURSOR_OFFSET);

        // Initialize candidates bitmask to all candidates set
        ectx.emit_store_imm64_to_stack(CANDIDATES_OFFSET, solver.initial_mask);

        // Consume '{'
        ectx.emit_call_intrinsic_ctx_only(
            json_intrinsics::fad_json_expect_object_start as *const u8,
        );

        // Check for empty object → resolve immediately
        ectx.emit_call_intrinsic_ctx_and_stack_out(
            json_intrinsics::fad_json_peek_after_ws as *const u8,
            RESULT_BYTE_OFFSET,
        );
        let empty_object = ectx.new_label();
        ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, b'}', empty_object);

        // ── scan_loop ────────────────────────────────────────────────
        ectx.bind_label(scan_loop);

        // Read key
        ectx.emit_call_intrinsic_ctx_and_two_stack_outs(
            json_intrinsics::fad_json_read_key as *const u8,
            KEY_PTR_OFFSET,
            KEY_LEN_OFFSET,
        );

        // Consume ':'
        ectx.emit_call_intrinsic_ctx_only(json_intrinsics::fad_json_expect_colon as *const u8);

        // Skip the value (we don't need it during the scan pass)
        ectx.emit_call_intrinsic_ctx_only(json_intrinsics::fad_json_skip_value as *const u8);

        // Linear key dispatch: for each known key, AND its mask into candidates.
        // Unknown keys don't narrow (no AND).
        let after_key_dispatch = ectx.new_label();
        let key_labels: Vec<_> = solver.key_masks.iter().map(|_| ectx.new_label()).collect();

        for (i, &(name, _mask)) in solver.key_masks.iter().enumerate() {
            let name_bytes = name.as_bytes();
            ectx.emit_call_pure_4arg(
                json_intrinsics::fad_json_key_equals as *const u8,
                KEY_PTR_OFFSET,
                KEY_LEN_OFFSET,
                name_bytes.as_ptr(),
                name_bytes.len() as u32,
            );
            ectx.emit_cbnz_x0(key_labels[i]);
        }
        // Unknown key — don't narrow, go to check
        ectx.emit_branch(after_key_dispatch);

        // Key match handlers: AND the mask
        for (i, &(_name, mask)) in solver.key_masks.iter().enumerate() {
            ectx.bind_label(key_labels[i]);
            ectx.emit_and_imm64_on_stack(CANDIDATES_OFFSET, mask);
            ectx.emit_branch(after_key_dispatch);
        }

        ectx.bind_label(after_key_dispatch);

        // Check candidates: popcount == 1 → resolve early
        ectx.emit_popcount_eq1_branch(CANDIDATES_OFFSET, resolve_label);
        // popcount == 0 → no match
        ectx.emit_stack_zero_branch(CANDIDATES_OFFSET, error_no_match);

        // comma_or_end: '}' → resolve, ',' → continue scanning
        ectx.emit_call_intrinsic_ctx_and_stack_out(
            json_intrinsics::fad_json_comma_or_end_object as *const u8,
            RESULT_BYTE_OFFSET,
        );
        ectx.emit_stack_byte_cmp_branch(RESULT_BYTE_OFFSET, 1, resolve_label);
        ectx.emit_branch(scan_loop);

        // Empty object → resolve with whatever candidates remain
        ectx.bind_label(empty_object);
        ectx.emit_advance_cursor_by(1); // consume '}'
        // Fall through to resolve

        // ── Pass 2: Resolve — restore cursor, dispatch to variant ────
        ectx.bind_label(resolve_label);

        // Restore cursor to before the '{'
        ectx.emit_restore_input_ptr(SAVED_CURSOR_OFFSET);

        // For each candidate bit position, check if that bit is set and
        // dispatch to the variant body.
        for (bit, &orig_variant_idx) in solver.candidate_to_variant.iter().enumerate() {
            let variant_label = ectx.new_label();
            ectx.emit_test_bit_branch(CANDIDATES_OFFSET, bit as u32, variant_label);
            // Not this candidate, try next
            let skip_label = ectx.new_label();
            ectx.emit_branch(skip_label);

            ectx.bind_label(variant_label);
            emit_variant_body(ectx, &variants[orig_variant_idx]);
            ectx.emit_branch(done_label);

            ectx.bind_label(skip_label);
        }

        // If we get here, no candidate bit was set — shouldn't happen but be safe
        ectx.emit_branch(error_no_match);

        // ── Error paths ──────────────────────────────────────────────
        ectx.bind_label(error_no_match);
        ectx.emit_error(ErrorCode::UnknownVariant);

        ectx.bind_label(error_ambiguous);
        ectx.emit_error(ErrorCode::AmbiguousVariant);
    }
}
