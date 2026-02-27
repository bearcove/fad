use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi, DynamicLabel, AssemblyOffset};

use crate::context::{CTX_ERROR_CODE, CTX_INPUT_PTR, CTX_INPUT_END};

pub type Assembler = dynasmrt::x64::Assembler;

/// Base frame size: rbp + pad + 4 callee-saved regs = 48 bytes.
const BASE_FRAME: u32 = 48;

/// Emission context — wraps the assembler plus bookkeeping labels.
pub struct EmitCtx {
    pub ops: Assembler,
    pub error_exit: DynamicLabel,
    pub entry: AssemblyOffset,
    /// Total frame size (base + extra, 16-byte aligned).
    pub frame_size: u32,
}

// Register assignments (System V AMD64 ABI, all callee-saved):
//   r12 = cached input_ptr
//   r13 = cached input_end
//   r14 = out pointer
//   r15 = ctx pointer
//
// Scratch (caller-saved):
//   rax = fn ptr loads, return values
//   r10, r11 = temporaries
//
// Arguments to intrinsics:
//   rdi = 1st arg (ctx)
//   rsi = 2nd arg
//   rdx = 3rd arg
//   rcx = 4th arg

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
    /// - r12 = cached input_ptr
    /// - r13 = cached input_end
    /// - r14 = out pointer
    /// - r15 = ctx pointer
    pub fn begin_func(&mut self) -> (AssemblyOffset, DynamicLabel) {
        let error_exit = self.ops.new_dynamic_label();
        let entry = self.ops.offset();
        let frame_size = self.frame_size;

        // On entry: rsp is 8-mod-16 (return address was pushed by `call`).
        // push rbp → rsp is now 16-byte aligned.
        // sub rsp, frame_size → stays 16-byte aligned (frame_size is multiple of 16).
        dynasm!(self.ops
            ; .arch x64
            ; push rbp
            ; sub rsp, frame_size as i32
            ; mov [rsp], rbp
            ; mov [rsp + 16], r12
            ; mov [rsp + 24], r13
            ; mov [rsp + 32], r14
            ; mov [rsp + 40], r15

            // Save arguments to callee-saved registers
            ; mov r14, rdi              // r14 = out
            ; mov r15, rsi              // r15 = ctx

            // Cache input cursor from ctx
            ; mov r12, [r15 + CTX_INPUT_PTR as i32]   // r12 = ctx.input_ptr
            ; mov r13, [r15 + CTX_INPUT_END as i32]   // r13 = ctx.input_end
        );

        self.error_exit = error_exit;
        (entry, error_exit)
    }

    /// Emit the success epilogue and error exit for the current function.
    ///
    /// `error_exit` must be the label returned by the corresponding `begin_func` call.
    pub fn end_func(&mut self, error_exit: DynamicLabel) {
        let frame_size = self.frame_size as i32;

        dynasm!(self.ops
            ; .arch x64
            // Success path: flush cursor, restore registers, return
            ; mov [r15 + CTX_INPUT_PTR as i32], r12
            ; mov r15, [rsp + 40]
            ; mov r14, [rsp + 32]
            ; mov r13, [rsp + 24]
            ; mov r12, [rsp + 16]
            ; mov rbp, [rsp]
            ; add rsp, frame_size
            ; pop rbp
            ; ret

            // Error exit: just restore and return (error is already in ctx.error)
            ; =>error_exit
            ; mov r15, [rsp + 40]
            ; mov r14, [rsp + 32]
            ; mov r13, [rsp + 24]
            ; mov r12, [rsp + 16]
            ; mov rbp, [rsp]
            ; add rsp, frame_size
            ; pop rbp
            ; ret
        );
    }

    /// Emit a call to another emitted function.
    ///
    /// Convention: rdi = out + field_offset, rsi = ctx (same as our entry convention).
    /// Flushes cursor before call, reloads after, checks error.
    ///
    /// r[impl callconv.inter-function]
    pub fn emit_call_emitted_func(&mut self, label: DynamicLabel, field_offset: u32) {
        let error_exit = self.error_exit;

        dynasm!(self.ops
            ; .arch x64
            // Flush cached cursor back to ctx
            ; mov [r15 + CTX_INPUT_PTR as i32], r12

            // Set up arguments: rdi = out + field_offset, rsi = ctx
            ; lea rdi, [r14 + field_offset as i32]
            ; mov rsi, r15

            // Call the emitted function via PC-relative call
            ; call =>label

            // Reload cached cursor from ctx (callee may have advanced it)
            ; mov r12, [r15 + CTX_INPUT_PTR as i32]

            // Check error: if ctx.error.code != 0, branch to error exit
            ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
            ; test r10d, r10d
            ; jnz =>error_exit
        );
    }

    /// Emit a call to an intrinsic function.
    ///
    /// Before the call: flushes the cached input_ptr back to ctx.
    /// Sets up args: rdi = ctx, rsi = out + field_offset.
    /// After the call: reloads input_ptr from ctx, checks error slot.
    pub fn emit_call_intrinsic(&mut self, fn_ptr: *const u8, field_offset: u32) {
        let error_exit = self.error_exit;
        let ptr_val = fn_ptr as i64;

        dynasm!(self.ops
            ; .arch x64
            // Flush cached cursor back to ctx
            ; mov [r15 + CTX_INPUT_PTR as i32], r12

            // Set up arguments: rdi = ctx, rsi = out + field_offset
            ; mov rdi, r15
            ; lea rsi, [r14 + field_offset as i32]

            // Load function pointer into rax and call
            ; mov rax, QWORD ptr_val
            ; call rax

            // Reload cached cursor from ctx (intrinsic may have advanced it)
            ; mov r12, [r15 + CTX_INPUT_PTR as i32]

            // Check error: if ctx.error.code != 0, branch to error exit
            ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
            ; test r10d, r10d
            ; jnz =>error_exit
        );
    }

    /// Emit a call to an intrinsic that takes only ctx as argument.
    /// Flushes cursor, calls, reloads cursor, checks error.
    pub fn emit_call_intrinsic_ctx_only(&mut self, fn_ptr: *const u8) {
        let error_exit = self.error_exit;
        let ptr_val = fn_ptr as i64;

        dynasm!(self.ops
            ; .arch x64
            ; mov [r15 + CTX_INPUT_PTR as i32], r12
            ; mov rdi, r15
            ; mov rax, QWORD ptr_val
            ; call rax
            ; mov r12, [r15 + CTX_INPUT_PTR as i32]
            ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
            ; test r10d, r10d
            ; jnz =>error_exit
        );
    }

    /// Emit a call to an intrinsic that takes (ctx, &mut stack_slot).
    /// rdi = ctx, rsi = rsp + sp_offset. Flushes/reloads cursor, checks error.
    pub fn emit_call_intrinsic_ctx_and_stack_out(&mut self, fn_ptr: *const u8, sp_offset: u32) {
        let error_exit = self.error_exit;
        let ptr_val = fn_ptr as i64;

        dynasm!(self.ops
            ; .arch x64
            ; mov [r15 + CTX_INPUT_PTR as i32], r12
            ; mov rdi, r15
            ; lea rsi, [rsp + sp_offset as i32]
            ; mov rax, QWORD ptr_val
            ; call rax
            ; mov r12, [r15 + CTX_INPUT_PTR as i32]
            ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
            ; test r10d, r10d
            ; jnz =>error_exit
        );
    }

    /// Emit a call to an intrinsic that takes (ctx, out_ptr1, out_ptr2).
    /// rdi = ctx, rsi = rsp + sp_offset1, rdx = rsp + sp_offset2.
    pub fn emit_call_intrinsic_ctx_and_two_stack_outs(
        &mut self,
        fn_ptr: *const u8,
        sp_offset1: u32,
        sp_offset2: u32,
    ) {
        let error_exit = self.error_exit;
        let ptr_val = fn_ptr as i64;

        dynasm!(self.ops
            ; .arch x64
            ; mov [r15 + CTX_INPUT_PTR as i32], r12
            ; mov rdi, r15
            ; lea rsi, [rsp + sp_offset1 as i32]
            ; lea rdx, [rsp + sp_offset2 as i32]
            ; mov rax, QWORD ptr_val
            ; call rax
            ; mov r12, [r15 + CTX_INPUT_PTR as i32]
            ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
            ; test r10d, r10d
            ; jnz =>error_exit
        );
    }

    /// Emit a call to a pure function (no ctx, no flush/reload/error-check).
    /// Used for key_equals: rdi=key_ptr, rsi=key_len, rdx=expected_ptr, rcx=expected_len.
    /// Return value is in rax.
    pub fn emit_call_pure_4arg(
        &mut self,
        fn_ptr: *const u8,
        arg0_sp_offset: u32,
        arg1_sp_offset: u32,
        expected_ptr: *const u8,
        expected_len: u32,
    ) {
        let ptr_val = fn_ptr as i64;
        let expected_addr = expected_ptr as i64;

        dynasm!(self.ops
            ; .arch x64
            ; mov rdi, [rsp + arg0_sp_offset as i32]
            ; mov rsi, [rsp + arg1_sp_offset as i32]
            ; mov rdx, QWORD expected_addr
            ; mov ecx, expected_len as i32
            ; mov rax, QWORD ptr_val
            ; call rax
        );
    }

    /// Allocate a new dynamic label.
    pub fn new_label(&mut self) -> DynamicLabel {
        self.ops.new_dynamic_label()
    }

    /// Bind a dynamic label at the current position.
    pub fn bind_label(&mut self, label: DynamicLabel) {
        dynasm!(self.ops
            ; .arch x64
            ; =>label
        );
    }

    /// Emit an unconditional branch to the given label.
    pub fn emit_branch(&mut self, label: DynamicLabel) {
        dynasm!(self.ops
            ; .arch x64
            ; jmp =>label
        );
    }

    /// Emit `test rax, rax; jnz label` — branch if rax is nonzero.
    pub fn emit_cbnz_x0(&mut self, label: DynamicLabel) {
        dynasm!(self.ops
            ; .arch x64
            ; test rax, rax
            ; jnz =>label
        );
    }

    /// Zero a 64-bit stack slot at rsp + offset.
    pub fn emit_zero_stack_slot(&mut self, sp_offset: u32) {
        dynasm!(self.ops
            ; .arch x64
            ; mov QWORD [rsp + sp_offset as i32], 0
        );
    }

    /// Load a byte from rsp + sp_offset, compare with byte_val, branch if equal.
    pub fn emit_stack_byte_cmp_branch(&mut self, sp_offset: u32, byte_val: u8, label: DynamicLabel) {
        let byte_val = byte_val as i32;
        dynasm!(self.ops
            ; .arch x64
            ; movzx r10d, BYTE [rsp + sp_offset as i32]
            ; cmp r10d, byte_val
            ; je =>label
        );
    }

    /// Set bit `bit_index` in a 64-bit stack slot at rsp + sp_offset.
    pub fn emit_set_bit_on_stack(&mut self, sp_offset: u32, bit_index: u32) {
        let mask = (1u64 << bit_index) as i64;

        dynasm!(self.ops
            ; .arch x64
            ; mov rax, [rsp + sp_offset as i32]
            ; mov r10, QWORD mask
            ; or rax, r10
            ; mov [rsp + sp_offset as i32], rax
        );
    }

    /// Check that the 64-bit stack slot at rsp + sp_offset equals expected_mask.
    /// If not, set MissingRequiredField error and branch to error_exit.
    pub fn emit_check_bitset(&mut self, sp_offset: u32, expected_mask: u64) {
        let error_exit = self.error_exit;
        let ok_label = self.ops.new_dynamic_label();
        let expected_mask = expected_mask as i64;
        let error_code = crate::context::ErrorCode::MissingRequiredField as i32;

        dynasm!(self.ops
            ; .arch x64
            ; mov rax, [rsp + sp_offset as i32]
            ; mov r10, QWORD expected_mask
            ; cmp rax, r10
            ; je =>ok_label
            // Not all required fields were seen — write error and bail
            ; mov DWORD [r15 + CTX_ERROR_CODE as i32], error_code
            ; jmp =>error_exit
            ; =>ok_label
        );
    }

    /// Advance the cached cursor by n bytes (inline, no function call).
    pub fn emit_advance_cursor_by(&mut self, n: u32) {
        dynasm!(self.ops
            ; .arch x64
            ; add r12, n as i32
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
