use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi, DynamicLabel, AssemblyOffset};

use crate::context::{CTX_ERROR_CODE, CTX_INPUT_PTR, CTX_INPUT_END};

pub type Assembler = dynasmrt::aarch64::Assembler;

/// Base frame size: 3 pairs of callee-saved registers = 48 bytes.
const BASE_FRAME: u32 = 48;

/// Emission context — wraps the assembler plus bookkeeping labels.
pub struct EmitCtx {
    pub ops: Assembler,
    pub error_exit: DynamicLabel,
    pub entry: AssemblyOffset,
    /// Total frame size (base + extra, 16-byte aligned).
    pub frame_size: u32,
}

// Register assignments (all callee-saved):
//   x19 = cached input_ptr
//   x20 = cached input_end
//   x21 = out pointer
//   x22 = ctx pointer

impl EmitCtx {
    /// Create a new EmitCtx. Does not emit any code.
    ///
    /// `extra_stack` is the number of additional bytes the format needs on the
    /// stack (e.g. 32 for JSON's bitset + key_ptr + key_len + peek_byte).
    /// The total frame is rounded up to 16-byte alignment.
    ///
    /// Call `begin_func()` to emit a function prologue.
    pub fn new(extra_stack: u32) -> Self {
        let frame_size = (BASE_FRAME + extra_stack + 15) & !15;
        let mut ops = Assembler::new().expect("failed to create assembler");
        let error_exit = ops.new_dynamic_label();
        let entry = AssemblyOffset(0);

        EmitCtx {
            ops,
            error_exit,
            entry,
            frame_size,
        }
    }

    /// Emit a function prologue. Returns the entry offset and a fresh error_exit label.
    ///
    /// The returned error_exit label must be passed to `end_func` when done emitting
    /// this function's body.
    ///
    /// # Register assignments after prologue
    /// - x19 = cached input_ptr
    /// - x20 = cached input_end
    /// - x21 = out pointer
    /// - x22 = ctx pointer
    pub fn begin_func(&mut self) -> (AssemblyOffset, DynamicLabel) {
        let error_exit = self.ops.new_dynamic_label();
        let entry = self.ops.offset();
        let frame_size = self.frame_size;

        // We emit the prologue with sub sp + stp instead of the pre-index form,
        // because frame_size is dynamic and dynasm doesn't support runtime
        // immediates in pre-index stp.
        dynasm!(self.ops
            ; .arch aarch64
            ; sub sp, sp, #frame_size
            ; stp x29, x30, [sp]
            ; stp x19, x20, [sp, #16]
            ; stp x21, x22, [sp, #32]
            ; add x29, sp, #0

            // Save arguments to callee-saved registers
            ; mov x21, x0              // x21 = out
            ; mov x22, x1              // x22 = ctx

            // Cache input cursor from ctx
            ; ldr x19, [x22, #CTX_INPUT_PTR]  // x19 = ctx.input_ptr
            ; ldr x20, [x22, #CTX_INPUT_END]  // x20 = ctx.input_end
        );

        self.error_exit = error_exit;
        (entry, error_exit)
    }

    /// Emit the success epilogue and error exit for the current function.
    ///
    /// `error_exit` must be the label returned by the corresponding `begin_func` call.
    pub fn end_func(&mut self, error_exit: DynamicLabel) {
        let frame_size = self.frame_size;

        dynasm!(self.ops
            ; .arch aarch64
            // Success path: flush cursor, restore registers, return
            ; str x19, [x22, #CTX_INPUT_PTR]
            ; ldp x21, x22, [sp, #32]
            ; ldp x19, x20, [sp, #16]
            ; ldp x29, x30, [sp]
            ; add sp, sp, #frame_size
            ; ret

            // Error exit: just restore and return (error is already in ctx.error)
            ; =>error_exit
            ; ldp x21, x22, [sp, #32]
            ; ldp x19, x20, [sp, #16]
            ; ldp x29, x30, [sp]
            ; add sp, sp, #frame_size
            ; ret
        );
    }

    /// Emit a call to another emitted function.
    ///
    /// Convention: x0 = out + field_offset, x1 = ctx (same as our entry convention).
    /// Flushes cursor before call, reloads after, checks error.
    ///
    /// r[impl callconv.inter-function]
    pub fn emit_call_emitted_func(&mut self, label: DynamicLabel, field_offset: u32) {
        let error_exit = self.error_exit;

        dynasm!(self.ops
            ; .arch aarch64
            // Flush cached cursor back to ctx
            ; str x19, [x22, #CTX_INPUT_PTR]

            // Set up arguments: x0 = out + field_offset, x1 = ctx
            ; add x0, x21, #field_offset
            ; mov x1, x22

            // Call the emitted function via PC-relative branch-and-link
            ; bl =>label

            // Reload cached cursor from ctx (callee may have advanced it)
            ; ldr x19, [x22, #CTX_INPUT_PTR]

            // Check error: if ctx.error.code != 0, branch to error exit
            ; ldr w9, [x22, #CTX_ERROR_CODE]
            ; cbnz w9, =>error_exit
        );
    }

    /// Emit a call to an intrinsic function.
    ///
    /// Before the call: flushes the cached input_ptr back to ctx.
    /// Sets up args: x0 = ctx, x1 = out + field_offset.
    /// After the call: reloads input_ptr from ctx, checks error slot.
    pub fn emit_call_intrinsic(&mut self, fn_ptr: *const u8, field_offset: u32) {
        let error_exit = self.error_exit;
        let ptr_val = fn_ptr as u64;

        dynasm!(self.ops
            ; .arch aarch64
            // Flush cached cursor back to ctx
            ; str x19, [x22, #CTX_INPUT_PTR]

            // Set up arguments: x0 = ctx, x1 = out + field_offset
            ; mov x0, x22
            ; add x1, x21, #field_offset

            // Load function pointer into x8 and call
            ; movz x8, #((ptr_val) & 0xFFFF) as u32
            ; movk x8, #((ptr_val >> 16) & 0xFFFF) as u32, LSL #16
            ; movk x8, #((ptr_val >> 32) & 0xFFFF) as u32, LSL #32
            ; movk x8, #((ptr_val >> 48) & 0xFFFF) as u32, LSL #48
            ; blr x8

            // Reload cached cursor from ctx (intrinsic may have advanced it)
            ; ldr x19, [x22, #CTX_INPUT_PTR]

            // Check error: if ctx.error.code != 0, branch to error exit
            ; ldr w9, [x22, #CTX_ERROR_CODE]
            ; cbnz w9, =>error_exit
        );
    }

    /// Emit a call to an intrinsic that takes only ctx as argument.
    /// Flushes cursor, calls, reloads cursor, checks error.
    pub fn emit_call_intrinsic_ctx_only(&mut self, fn_ptr: *const u8) {
        let error_exit = self.error_exit;
        let ptr_val = fn_ptr as u64;

        dynasm!(self.ops
            ; .arch aarch64
            ; str x19, [x22, #CTX_INPUT_PTR]
            ; mov x0, x22
            ; movz x8, #((ptr_val) & 0xFFFF) as u32
            ; movk x8, #((ptr_val >> 16) & 0xFFFF) as u32, LSL #16
            ; movk x8, #((ptr_val >> 32) & 0xFFFF) as u32, LSL #32
            ; movk x8, #((ptr_val >> 48) & 0xFFFF) as u32, LSL #48
            ; blr x8
            ; ldr x19, [x22, #CTX_INPUT_PTR]
            ; ldr w9, [x22, #CTX_ERROR_CODE]
            ; cbnz w9, =>error_exit
        );
    }

    /// Emit a call to an intrinsic that takes (ctx, &mut stack_slot).
    /// x0 = ctx, x1 = sp + sp_offset. Flushes/reloads cursor, checks error.
    pub fn emit_call_intrinsic_ctx_and_stack_out(&mut self, fn_ptr: *const u8, sp_offset: u32) {
        let error_exit = self.error_exit;
        let ptr_val = fn_ptr as u64;

        dynasm!(self.ops
            ; .arch aarch64
            ; str x19, [x22, #CTX_INPUT_PTR]
            ; mov x0, x22
            ; add x1, sp, #sp_offset
            ; movz x8, #((ptr_val) & 0xFFFF) as u32
            ; movk x8, #((ptr_val >> 16) & 0xFFFF) as u32, LSL #16
            ; movk x8, #((ptr_val >> 32) & 0xFFFF) as u32, LSL #32
            ; movk x8, #((ptr_val >> 48) & 0xFFFF) as u32, LSL #48
            ; blr x8
            ; ldr x19, [x22, #CTX_INPUT_PTR]
            ; ldr w9, [x22, #CTX_ERROR_CODE]
            ; cbnz w9, =>error_exit
        );
    }

    /// Emit a call to an intrinsic that takes (ctx, out_ptr1, out_ptr2).
    /// x0 = ctx, x1 = sp + sp_offset1, x2 = sp + sp_offset2.
    pub fn emit_call_intrinsic_ctx_and_two_stack_outs(
        &mut self,
        fn_ptr: *const u8,
        sp_offset1: u32,
        sp_offset2: u32,
    ) {
        let error_exit = self.error_exit;
        let ptr_val = fn_ptr as u64;

        dynasm!(self.ops
            ; .arch aarch64
            ; str x19, [x22, #CTX_INPUT_PTR]
            ; mov x0, x22
            ; add x1, sp, #sp_offset1
            ; add x2, sp, #sp_offset2
            ; movz x8, #((ptr_val) & 0xFFFF) as u32
            ; movk x8, #((ptr_val >> 16) & 0xFFFF) as u32, LSL #16
            ; movk x8, #((ptr_val >> 32) & 0xFFFF) as u32, LSL #32
            ; movk x8, #((ptr_val >> 48) & 0xFFFF) as u32, LSL #48
            ; blr x8
            ; ldr x19, [x22, #CTX_INPUT_PTR]
            ; ldr w9, [x22, #CTX_ERROR_CODE]
            ; cbnz w9, =>error_exit
        );
    }

    /// Emit a call to a pure function (no ctx, no flush/reload/error-check).
    /// Used for key_equals: x0=key_ptr, x1=key_len, x2=expected_ptr, x3=expected_len.
    /// Return value is in x0.
    pub fn emit_call_pure_4arg(
        &mut self,
        fn_ptr: *const u8,
        arg0_sp_offset: u32,
        arg1_sp_offset: u32,
        expected_ptr: *const u8,
        expected_len: u32,
    ) {
        let ptr_val = fn_ptr as u64;
        let expected_addr = expected_ptr as u64;

        dynasm!(self.ops
            ; .arch aarch64
            ; ldr x0, [sp, #arg0_sp_offset]
            ; ldr x1, [sp, #arg1_sp_offset]
            ; movz x2, #((expected_addr) & 0xFFFF) as u32
            ; movk x2, #((expected_addr >> 16) & 0xFFFF) as u32, LSL #16
            ; movk x2, #((expected_addr >> 32) & 0xFFFF) as u32, LSL #32
            ; movk x2, #((expected_addr >> 48) & 0xFFFF) as u32, LSL #48
            ; movz x3, expected_len
            ; movz x8, #((ptr_val) & 0xFFFF) as u32
            ; movk x8, #((ptr_val >> 16) & 0xFFFF) as u32, LSL #16
            ; movk x8, #((ptr_val >> 32) & 0xFFFF) as u32, LSL #32
            ; movk x8, #((ptr_val >> 48) & 0xFFFF) as u32, LSL #48
            ; blr x8
        );
    }

    /// Allocate a new dynamic label.
    pub fn new_label(&mut self) -> DynamicLabel {
        self.ops.new_dynamic_label()
    }

    /// Bind a dynamic label at the current position.
    pub fn bind_label(&mut self, label: DynamicLabel) {
        dynasm!(self.ops
            ; .arch aarch64
            ; =>label
        );
    }

    /// Emit an unconditional branch to the given label.
    pub fn emit_branch(&mut self, label: DynamicLabel) {
        dynasm!(self.ops
            ; .arch aarch64
            ; b =>label
        );
    }

    /// Emit `cbnz x0, label` — branch if x0 is nonzero.
    pub fn emit_cbnz_x0(&mut self, label: DynamicLabel) {
        dynasm!(self.ops
            ; .arch aarch64
            ; cbnz x0, =>label
        );
    }

    /// Zero a 64-bit stack slot at sp + offset.
    pub fn emit_zero_stack_slot(&mut self, sp_offset: u32) {
        dynasm!(self.ops
            ; .arch aarch64
            ; str xzr, [sp, #sp_offset]
        );
    }

    /// Load a byte from sp + sp_offset, compare with byte_val, branch if equal.
    pub fn emit_stack_byte_cmp_branch(&mut self, sp_offset: u32, byte_val: u8, label: DynamicLabel) {
        let byte_val = byte_val as u32;
        dynasm!(self.ops
            ; .arch aarch64
            ; ldrb w9, [sp, #sp_offset]
            ; cmp w9, #byte_val
            ; b.eq =>label
        );
    }

    /// Set bit `bit_index` in a 64-bit stack slot at sp + sp_offset.
    pub fn emit_set_bit_on_stack(&mut self, sp_offset: u32, bit_index: u32) {
        let mask = 1u64 << bit_index;
        let mask_lo = (mask & 0xFFFF) as u32;
        let mask_hi = ((mask >> 16) & 0xFFFF) as u32;

        dynasm!(self.ops
            ; .arch aarch64
            ; ldr x9, [sp, #sp_offset]
        );

        if mask <= 0xFFFF {
            dynasm!(self.ops
                ; .arch aarch64
                ; movz x10, #mask_lo
            );
        } else {
            dynasm!(self.ops
                ; .arch aarch64
                ; movz x10, #mask_lo
                ; movk x10, #mask_hi, LSL #16
            );
        }

        dynasm!(self.ops
            ; .arch aarch64
            ; orr x9, x9, x10
            ; str x9, [sp, #sp_offset]
        );
    }

    /// Check that the 64-bit stack slot at sp + sp_offset equals expected_mask.
    /// If not, set MissingRequiredField error and branch to error_exit.
    pub fn emit_check_bitset(&mut self, sp_offset: u32, expected_mask: u64) {
        let error_exit = self.error_exit;
        let ok_label = self.ops.new_dynamic_label();
        let mask_lo = (expected_mask & 0xFFFF) as u32;
        let mask_hi = ((expected_mask >> 16) & 0xFFFF) as u32;
        let error_code = crate::context::ErrorCode::MissingRequiredField as u32;

        dynasm!(self.ops
            ; .arch aarch64
            ; ldr x9, [sp, #sp_offset]
        );

        if expected_mask <= 0xFFFF {
            dynasm!(self.ops
                ; .arch aarch64
                ; movz x10, #mask_lo
            );
        } else {
            dynasm!(self.ops
                ; .arch aarch64
                ; movz x10, #mask_lo
                ; movk x10, #mask_hi, LSL #16
            );
        }

        dynasm!(self.ops
            ; .arch aarch64
            ; cmp x9, x10
            ; b.eq =>ok_label
            // Not all required fields were seen — write error and bail
            ; movz w9, #error_code
            ; str w9, [x22, #CTX_ERROR_CODE]
            ; b =>error_exit
            ; =>ok_label
        );
    }

    // ── Inline scalar reads ──────────────────────────────────────────
    //
    // These emit scalar decode logic directly into the instruction stream,
    // using cached registers (x19=input_ptr, x20=input_end, x21=out,
    // x22=ctx). No cursor flush/reload, no indirect call.

    /// Emit inline code to read a single byte (u8 or i8) and store to out+offset.
    pub fn emit_inline_read_byte(&mut self, offset: u32) {
        let error_exit = self.error_exit;
        let eof_label = self.ops.new_dynamic_label();

        dynasm!(self.ops
            ; .arch aarch64
            // Bounds check: need at least 1 byte
            ; cmp x19, x20
            ; b.hs =>eof_label
            // Load, store, advance
            ; ldrb w9, [x19]
            ; strb w9, [x21, #offset]
            ; add x19, x19, #1
        );

        // Defer cold eof path
        let done_label = self.ops.new_dynamic_label();
        dynasm!(self.ops
            ; .arch aarch64
            ; b =>done_label
            ; =>eof_label
            ; movz w9, crate::context::ErrorCode::UnexpectedEof as u32
            ; str w9, [x22, #CTX_ERROR_CODE]
            ; b =>error_exit
            ; =>done_label
        );
    }

    /// Emit inline code to read a bool and store to out+offset.
    /// Validates the byte is 0 or 1.
    pub fn emit_inline_read_bool(&mut self, offset: u32) {
        let error_exit = self.error_exit;
        let eof_label = self.ops.new_dynamic_label();
        let invalid_label = self.ops.new_dynamic_label();
        let done_label = self.ops.new_dynamic_label();

        dynasm!(self.ops
            ; .arch aarch64
            // Bounds check
            ; cmp x19, x20
            ; b.hs =>eof_label
            // Load byte
            ; ldrb w9, [x19]
            // Validate: must be 0 or 1
            ; cmp w9, #1
            ; b.hi =>invalid_label
            // Store and advance
            ; strb w9, [x21, #offset]
            ; add x19, x19, #1
            ; b =>done_label

            // Cold: invalid bool
            ; =>invalid_label
            ; movz w9, crate::context::ErrorCode::InvalidBool as u32
            ; str w9, [x22, #CTX_ERROR_CODE]
            ; b =>error_exit

            // Cold: eof
            ; =>eof_label
            ; movz w9, crate::context::ErrorCode::UnexpectedEof as u32
            ; str w9, [x22, #CTX_ERROR_CODE]
            ; b =>error_exit

            ; =>done_label
        );
    }

    /// Emit inline code to read 4 LE bytes (f32) and store to out+offset.
    pub fn emit_inline_read_f32(&mut self, offset: u32) {
        let error_exit = self.error_exit;
        let eof_label = self.ops.new_dynamic_label();
        let done_label = self.ops.new_dynamic_label();

        dynasm!(self.ops
            ; .arch aarch64
            // Bounds check: need 4 bytes
            ; sub x9, x20, x19
            ; cmp x9, #4
            ; b.lo =>eof_label
            // Load 4 bytes (unaligned ok on aarch64), store, advance
            ; ldr w9, [x19]
            ; str w9, [x21, #offset]
            ; add x19, x19, #4
            ; b =>done_label

            ; =>eof_label
            ; movz w9, crate::context::ErrorCode::UnexpectedEof as u32
            ; str w9, [x22, #CTX_ERROR_CODE]
            ; b =>error_exit

            ; =>done_label
        );
    }

    /// Emit inline code to read 8 LE bytes (f64) and store to out+offset.
    pub fn emit_inline_read_f64(&mut self, offset: u32) {
        let error_exit = self.error_exit;
        let eof_label = self.ops.new_dynamic_label();
        let done_label = self.ops.new_dynamic_label();

        dynasm!(self.ops
            ; .arch aarch64
            // Bounds check: need 8 bytes
            ; sub x9, x20, x19
            ; cmp x9, #8
            ; b.lo =>eof_label
            // Load 8 bytes, store, advance
            ; ldr x9, [x19]
            ; str x9, [x21, #offset]
            ; add x19, x19, #8
            ; b =>done_label

            ; =>eof_label
            ; movz w9, crate::context::ErrorCode::UnexpectedEof as u32
            ; str w9, [x22, #CTX_ERROR_CODE]
            ; b =>error_exit

            ; =>done_label
        );
    }

    /// Emit inline varint fast path: if the first byte has bit 7 clear
    /// (single-byte varint, value < 128), store directly and advance.
    /// Otherwise fall through to a full intrinsic call for multi-byte decode.
    ///
    /// `store_width`: 2 (u16/i16), 4 (u32/i32), or 8 (u64/i64).
    /// `zigzag`: true for signed types (apply zigzag decode on fast path).
    /// `intrinsic_fn_ptr`: full varint decode intrinsic for the slow path.
    pub fn emit_inline_varint_fast_path(
        &mut self,
        offset: u32,
        store_width: u32,
        zigzag: bool,
        intrinsic_fn_ptr: *const u8,
    ) {
        let error_exit = self.error_exit;
        let eof_label = self.ops.new_dynamic_label();
        let slow_path = self.ops.new_dynamic_label();
        let done_label = self.ops.new_dynamic_label();
        let ptr_val = intrinsic_fn_ptr as u64;

        // Bounds check: need at least 1 byte
        dynasm!(self.ops
            ; .arch aarch64
            ; cmp x19, x20
            ; b.hs =>eof_label
            // Load first byte
            ; ldrb w9, [x19]
            // Test continuation bit (bit 7)
            ; tbnz w9, #7, =>slow_path
            // Fast path: single-byte varint, value in w9 (0..127)
            ; add x19, x19, #1
        );

        if zigzag {
            // Zigzag decode: decoded = (v >> 1) ^ -(v & 1)
            dynasm!(self.ops
                ; .arch aarch64
                ; lsr w10, w9, #1          // w10 = v >> 1
                ; and w11, w9, #1          // w11 = v & 1
                ; neg w11, w11             // w11 = -(v & 1) = 0 or 0xFFFFFFFF
                ; eor w9, w10, w11         // w9 = decoded value
            );
        }

        // Store based on width
        match store_width {
            2 => dynasm!(self.ops ; .arch aarch64 ; strh w9, [x21, #offset]),
            4 => dynasm!(self.ops ; .arch aarch64 ; str w9, [x21, #offset]),
            8 => dynasm!(self.ops ; .arch aarch64 ; str x9, [x21, #offset]),
            _ => panic!("unsupported varint store width: {store_width}"),
        }

        dynasm!(self.ops
            ; .arch aarch64
            ; b =>done_label

            // Slow path: multi-byte varint, call intrinsic
            ; =>slow_path
            // Flush cached cursor back to ctx
            ; str x19, [x22, #CTX_INPUT_PTR]
            // Set up arguments: x0 = ctx, x1 = out + offset
            ; mov x0, x22
            ; add x1, x21, #offset
            // Load function pointer and call
            ; movz x8, #((ptr_val) & 0xFFFF) as u32
            ; movk x8, #((ptr_val >> 16) & 0xFFFF) as u32, LSL #16
            ; movk x8, #((ptr_val >> 32) & 0xFFFF) as u32, LSL #32
            ; movk x8, #((ptr_val >> 48) & 0xFFFF) as u32, LSL #48
            ; blr x8
            // Reload cursor and check error
            ; ldr x19, [x22, #CTX_INPUT_PTR]
            ; ldr w9, [x22, #CTX_ERROR_CODE]
            ; cbnz w9, =>error_exit
            ; b =>done_label

            // Cold: eof
            ; =>eof_label
            ; movz w9, crate::context::ErrorCode::UnexpectedEof as u32
            ; str w9, [x22, #CTX_ERROR_CODE]
            ; b =>error_exit

            ; =>done_label
        );
    }

    // ── Enum support ──────────────────────────────────────────────────

    // r[impl deser.enum.set-variant]

    /// Write a discriminant value to [out + 0].
    /// `size` is 1, 2, 4, or 8 bytes (from EnumRepr).
    pub fn emit_write_discriminant(&mut self, value: i64, size: u32) {
        let val = value as u64;
        // Load immediate into w9/x9
        if size <= 4 {
            let val32 = val as u32;
            if val32 <= 0xFFFF {
                dynasm!(self.ops ; .arch aarch64 ; movz w9, val32);
            } else {
                let lo = val32 & 0xFFFF;
                let hi = (val32 >> 16) & 0xFFFF;
                dynasm!(self.ops ; .arch aarch64
                    ; movz w9, lo
                    ; movk w9, hi, LSL #16
                );
            }
        } else {
            dynasm!(self.ops ; .arch aarch64
                ; movz x9, #((val) & 0xFFFF) as u32
                ; movk x9, #((val >> 16) & 0xFFFF) as u32, LSL #16
                ; movk x9, #((val >> 32) & 0xFFFF) as u32, LSL #32
                ; movk x9, #((val >> 48) & 0xFFFF) as u32, LSL #48
            );
        }
        // Store to [out + 0]
        match size {
            1 => dynasm!(self.ops ; .arch aarch64 ; strb w9, [x21]),
            2 => dynasm!(self.ops ; .arch aarch64 ; strh w9, [x21]),
            4 => dynasm!(self.ops ; .arch aarch64 ; str w9, [x21]),
            8 => dynasm!(self.ops ; .arch aarch64 ; str x9, [x21]),
            _ => panic!("unsupported discriminant size: {size}"),
        }
    }

    // r[impl deser.postcard.enum.dispatch]

    /// Read a postcard varint discriminant into w9 (kept in register for
    /// dispatch, not stored to memory).
    ///
    /// On the fast path (single-byte, value < 128): 4 instructions.
    /// On the slow path: calls the intrinsic, which writes to a temporary
    /// on the stack, then loads the result into w9.
    ///
    /// After this, the caller emits `emit_cmp_imm_branch_eq` for each variant.
    pub fn emit_read_postcard_discriminant(&mut self, slow_intrinsic: *const u8) {
        let error_exit = self.error_exit;
        let eof_label = self.ops.new_dynamic_label();
        let slow_path = self.ops.new_dynamic_label();
        let done_label = self.ops.new_dynamic_label();
        let ptr_val = slow_intrinsic as u64;

        dynasm!(self.ops
            ; .arch aarch64
            // Bounds check
            ; cmp x19, x20
            ; b.hs =>eof_label
            // Load first byte
            ; ldrb w9, [x19]
            // Test continuation bit
            ; tbnz w9, #7, =>slow_path
            // Fast path: single-byte varint
            ; add x19, x19, #1
            ; b =>done_label

            // Slow path: call full varint decode intrinsic
            // We need the value in w9 after the call, so we use the stack
            // as a temporary (reuse slot at sp+48 which is the extra stack area).
            // The intrinsic writes to its out arg.
            ; =>slow_path
            ; str x19, [x22, #CTX_INPUT_PTR]
            ; mov x0, x22
            ; add x1, sp, #48            // temp u32 on stack
            ; movz x8, #((ptr_val) & 0xFFFF) as u32
            ; movk x8, #((ptr_val >> 16) & 0xFFFF) as u32, LSL #16
            ; movk x8, #((ptr_val >> 32) & 0xFFFF) as u32, LSL #32
            ; movk x8, #((ptr_val >> 48) & 0xFFFF) as u32, LSL #48
            ; blr x8
            ; ldr x19, [x22, #CTX_INPUT_PTR]
            ; ldr w10, [x22, #CTX_ERROR_CODE]
            ; cbnz w10, =>error_exit
            ; ldr w9, [sp, #48]           // load decoded discriminant
            ; b =>done_label

            ; =>eof_label
            ; movz w9, crate::context::ErrorCode::UnexpectedEof as u32
            ; str w9, [x22, #CTX_ERROR_CODE]
            ; b =>error_exit

            ; =>done_label
        );
    }

    /// Compare w9 (discriminant) with immediate `imm` and branch to `label`
    /// if equal.
    pub fn emit_cmp_imm_branch_eq(&mut self, imm: u32, label: DynamicLabel) {
        dynasm!(self.ops
            ; .arch aarch64
            ; cmp w9, #imm
            ; b.eq =>label
        );
    }

    /// Emit a branch-to-error for unknown variant (sets UnknownVariant error code).
    pub fn emit_unknown_variant_error(&mut self) {
        let error_exit = self.error_exit;
        let error_code = crate::context::ErrorCode::UnknownVariant as u32;

        dynasm!(self.ops
            ; .arch aarch64
            ; movz w9, #error_code
            ; str w9, [x22, #CTX_ERROR_CODE]
            ; b =>error_exit
        );
    }

    /// Advance the cached cursor by n bytes (inline, no function call).
    pub fn emit_advance_cursor_by(&mut self, n: u32) {
        dynasm!(self.ops
            ; .arch aarch64
            ; add x19, x19, #n
        );
    }

    /// Commit and finalize the assembler, returning the executable buffer.
    ///
    /// All functions must have been completed with `end_func` before calling this.
    pub fn finalize(mut self) -> dynasmrt::ExecutableBuffer {
        self.ops.commit().expect("failed to commit assembly");
        self.ops.finalize().expect("failed to finalize assembly")
    }
}
