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
