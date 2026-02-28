use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi, DynamicLabel, AssemblyOffset};

use crate::context::{CTX_ERROR_CODE, CTX_INPUT_PTR, CTX_INPUT_END};

pub type Assembler = dynasmrt::x64::Assembler;

/// Base frame size: rbp + pad + 4 callee-saved regs = 48 bytes.
pub const BASE_FRAME: u32 = 48;

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

    // ── Inline scalar reads ──────────────────────────────────────────
    //
    // These emit scalar decode logic directly into the instruction stream,
    // using cached registers (r12=input_ptr, r13=input_end, r14=out,
    // r15=ctx). No cursor flush/reload, no indirect call.

    /// Emit inline code to read a single byte (u8 or i8) and store to out+offset.
    pub fn emit_inline_read_byte(&mut self, offset: u32) {
        let error_exit = self.error_exit;
        let eof_label = self.ops.new_dynamic_label();
        let done_label = self.ops.new_dynamic_label();

        dynasm!(self.ops
            ; .arch x64
            // Bounds check: need at least 1 byte
            ; cmp r12, r13
            ; jae =>eof_label
            // Load, store, advance
            ; movzx r10d, BYTE [r12]
            ; mov BYTE [r14 + offset as i32], r10b
            ; add r12, 1
            ; jmp =>done_label

            ; =>eof_label
            ; mov DWORD [r15 + CTX_ERROR_CODE as i32], crate::context::ErrorCode::UnexpectedEof as i32
            ; jmp =>error_exit

            ; =>done_label
        );
    }

    /// Emit inline code to read a single byte from input and store to rsp+offset (stack slot).
    pub fn emit_inline_read_byte_to_stack(&mut self, sp_offset: u32) {
        let error_exit = self.error_exit;
        let eof_label = self.ops.new_dynamic_label();
        let done_label = self.ops.new_dynamic_label();

        dynasm!(self.ops
            ; .arch x64
            ; cmp r12, r13
            ; jae =>eof_label
            ; movzx r10d, BYTE [r12]
            ; mov BYTE [rsp + sp_offset as i32], r10b
            ; add r12, 1
            ; jmp =>done_label
            ; =>eof_label
            ; mov DWORD [r15 + CTX_ERROR_CODE as i32], crate::context::ErrorCode::UnexpectedEof as i32
            ; jmp =>error_exit
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
            ; .arch x64
            // Bounds check
            ; cmp r12, r13
            ; jae =>eof_label
            // Load byte
            ; movzx r10d, BYTE [r12]
            // Validate: must be 0 or 1
            ; cmp r10d, 1
            ; ja =>invalid_label
            // Store and advance
            ; mov BYTE [r14 + offset as i32], r10b
            ; add r12, 1
            ; jmp =>done_label

            // Cold: invalid bool
            ; =>invalid_label
            ; mov DWORD [r15 + CTX_ERROR_CODE as i32], crate::context::ErrorCode::InvalidBool as i32
            ; jmp =>error_exit

            // Cold: eof
            ; =>eof_label
            ; mov DWORD [r15 + CTX_ERROR_CODE as i32], crate::context::ErrorCode::UnexpectedEof as i32
            ; jmp =>error_exit

            ; =>done_label
        );
    }

    /// Emit inline code to read 4 LE bytes (f32) and store to out+offset.
    pub fn emit_inline_read_f32(&mut self, offset: u32) {
        let error_exit = self.error_exit;
        let eof_label = self.ops.new_dynamic_label();
        let done_label = self.ops.new_dynamic_label();

        dynasm!(self.ops
            ; .arch x64
            // Bounds check: need 4 bytes
            ; mov r10, r13
            ; sub r10, r12
            ; cmp r10, 4
            ; jb =>eof_label
            // Load 4 bytes, store, advance
            ; mov r10d, DWORD [r12]
            ; mov DWORD [r14 + offset as i32], r10d
            ; add r12, 4
            ; jmp =>done_label

            ; =>eof_label
            ; mov DWORD [r15 + CTX_ERROR_CODE as i32], crate::context::ErrorCode::UnexpectedEof as i32
            ; jmp =>error_exit

            ; =>done_label
        );
    }

    /// Emit inline code to read 8 LE bytes (f64) and store to out+offset.
    pub fn emit_inline_read_f64(&mut self, offset: u32) {
        let error_exit = self.error_exit;
        let eof_label = self.ops.new_dynamic_label();
        let done_label = self.ops.new_dynamic_label();

        dynasm!(self.ops
            ; .arch x64
            // Bounds check: need 8 bytes
            ; mov r10, r13
            ; sub r10, r12
            ; cmp r10, 8
            ; jb =>eof_label
            // Load 8 bytes, store, advance
            ; mov r10, QWORD [r12]
            ; mov QWORD [r14 + offset as i32], r10
            ; add r12, 8
            ; jmp =>done_label

            ; =>eof_label
            ; mov DWORD [r15 + CTX_ERROR_CODE as i32], crate::context::ErrorCode::UnexpectedEof as i32
            ; jmp =>error_exit

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
        let ptr_val = intrinsic_fn_ptr as i64;

        // Bounds check: need at least 1 byte
        dynasm!(self.ops
            ; .arch x64
            ; cmp r12, r13
            ; jae =>eof_label
            // Load first byte
            ; movzx r10d, BYTE [r12]
            // Test continuation bit (bit 7)
            ; test r10d, 0x80
            ; jnz =>slow_path
            // Fast path: single-byte varint, value in r10d (0..127)
            ; add r12, 1
        );

        if zigzag {
            // Zigzag decode: decoded = (v >> 1) ^ -(v & 1)
            dynasm!(self.ops
                ; .arch x64
                ; mov r11d, r10d
                ; shr r11d, 1              // r11d = v >> 1
                ; and r10d, 1              // r10d = v & 1
                ; neg r10d                 // r10d = -(v & 1) = 0 or 0xFFFFFFFF
                ; xor r10d, r11d           // r10d = decoded value
            );
        }

        // Store based on width
        match store_width {
            2 => dynasm!(self.ops ; .arch x64 ; mov WORD [r14 + offset as i32], r10w),
            4 => dynasm!(self.ops ; .arch x64 ; mov DWORD [r14 + offset as i32], r10d),
            8 => dynasm!(self.ops ; .arch x64 ; mov QWORD [r14 + offset as i32], r10),
            _ => panic!("unsupported varint store width: {store_width}"),
        }

        dynasm!(self.ops
            ; .arch x64
            ; jmp =>done_label

            // Slow path: multi-byte varint, call intrinsic
            ; =>slow_path
            // Flush cached cursor back to ctx
            ; mov [r15 + CTX_INPUT_PTR as i32], r12
            // Set up arguments: rdi = ctx, rsi = out + offset
            ; mov rdi, r15
            ; lea rsi, [r14 + offset as i32]
            // Load function pointer and call
            ; mov rax, QWORD ptr_val
            ; call rax
            // Reload cursor and check error
            ; mov r12, [r15 + CTX_INPUT_PTR as i32]
            ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
            ; test r10d, r10d
            ; jnz =>error_exit
            ; jmp =>done_label

            // Cold: eof
            ; =>eof_label
            ; mov DWORD [r15 + CTX_ERROR_CODE as i32], crate::context::ErrorCode::UnexpectedEof as i32
            ; jmp =>error_exit

            ; =>done_label
        );
    }

    // ── Inline string reads ────────────────────────────────────────────

    /// Emit inline postcard string deserialization.
    ///
    /// Inlines the varint length decode (fast path) and bounds check,
    /// then calls a lean intrinsic for UTF-8 validation + allocation only.
    /// Cursor advance is also inlined after the call.
    ///
    /// Stack usage: rsp+48 holds the string length (u32) across the call.
    /// This is within the extra stack area (postcard requests 8 bytes).
    pub fn emit_inline_postcard_string(
        &mut self,
        offset: u32,
        slow_varint_intrinsic: *const u8,
        validate_alloc_intrinsic: *const u8,
    ) {
        let error_exit = self.error_exit;
        let eof_label = self.ops.new_dynamic_label();
        let varint_slow = self.ops.new_dynamic_label();
        let have_length = self.ops.new_dynamic_label();
        let done_label = self.ops.new_dynamic_label();
        let varint_ptr = slow_varint_intrinsic as i64;
        let alloc_ptr = validate_alloc_intrinsic as i64;

        // Step 1: Inline varint length decode
        // Fast path: single-byte varint (length < 128)
        dynasm!(self.ops
            ; .arch x64
            // Bounds check: need at least 1 byte for the varint
            ; cmp r12, r13
            ; jae =>eof_label
            // Load first byte
            ; movzx r10d, BYTE [r12]
            // Test continuation bit (bit 7)
            ; test r10d, 0x80
            ; jnz =>varint_slow
            // Fast path: single-byte varint, length in r10d
            ; add r12, 1
            ; jmp =>have_length

            // Slow path: multi-byte varint length
            ; =>varint_slow
            ; mov [r15 + CTX_INPUT_PTR as i32], r12
            ; mov rdi, r15
            ; lea rsi, [rsp + 48]            // temp u32 on stack
            ; mov rax, QWORD varint_ptr
            ; call rax
            ; mov r12, [r15 + CTX_INPUT_PTR as i32]
            ; mov r11d, [r15 + CTX_ERROR_CODE as i32]
            ; test r11d, r11d
            ; jnz =>error_exit
            ; mov r10d, DWORD [rsp + 48]    // load decoded length
            ; jmp =>have_length
        );

        // Step 2: Inline bounds check + call validate+alloc + advance cursor
        dynasm!(self.ops
            ; .arch x64
            ; =>have_length
            // r10d = string length. Save to stack so it survives the call.
            ; mov DWORD [rsp + 48], r10d

            // Bounds check: remaining >= length
            ; mov r11, r13
            ; sub r11, r12                  // r11 = remaining bytes
            ; cmp r11, r10                  // remaining >= length?
            ; jb =>eof_label

            // Call validate_and_alloc_string(ctx, out+offset, input_ptr, length)
            ; mov [r15 + CTX_INPUT_PTR as i32], r12
            ; mov rdi, r15                  // arg0: ctx
            ; lea rsi, [r14 + offset as i32] // arg1: out + offset
            ; mov rdx, r12                  // arg2: data_ptr = current input_ptr
            ; mov ecx, r10d                 // arg3: data_len = length
            ; mov rax, QWORD alloc_ptr
            ; call rax

            // Check error
            ; mov r11d, [r15 + CTX_ERROR_CODE as i32]
            ; test r11d, r11d
            ; jnz =>error_exit

            // Advance cursor by string length (reload from stack)
            ; mov r10d, DWORD [rsp + 48]
            ; add r12, r10                  // input_ptr += length
            ; jmp =>done_label

            // Cold: eof / bounds check failure
            ; =>eof_label
            ; mov DWORD [r15 + CTX_ERROR_CODE as i32], crate::context::ErrorCode::UnexpectedEof as i32
            ; jmp =>error_exit

            ; =>done_label
        );
    }

    // ── Enum support ──────────────────────────────────────────────────

    // r[impl deser.enum.set-variant]

    /// Write a discriminant value to [out + 0].
    /// `size` is 1, 2, 4, or 8 bytes (from EnumRepr).
    pub fn emit_write_discriminant(&mut self, value: i64, size: u32) {
        match size {
            1 => dynasm!(self.ops ; .arch x64
                ; mov BYTE [r14], value as i8
            ),
            2 => dynasm!(self.ops ; .arch x64
                ; mov WORD [r14], value as i16
            ),
            4 => dynasm!(self.ops ; .arch x64
                ; mov DWORD [r14], value as i32
            ),
            8 => {
                let val = value;
                dynasm!(self.ops ; .arch x64
                    ; mov rax, QWORD val
                    ; mov QWORD [r14], rax
                );
            }
            _ => panic!("unsupported discriminant size: {size}"),
        }
    }

    // r[impl deser.postcard.enum.dispatch]

    /// Read a postcard varint discriminant into r10d (kept in register for
    /// dispatch, not stored to memory).
    ///
    /// After this, the caller emits `emit_cmp_imm_branch_eq` for each variant.
    /// The discriminant value is in r10d.
    pub fn emit_read_postcard_discriminant(&mut self, slow_intrinsic: *const u8) {
        let error_exit = self.error_exit;
        let eof_label = self.ops.new_dynamic_label();
        let slow_path = self.ops.new_dynamic_label();
        let done_label = self.ops.new_dynamic_label();
        let ptr_val = slow_intrinsic as i64;

        dynasm!(self.ops
            ; .arch x64
            // Bounds check
            ; cmp r12, r13
            ; jae =>eof_label
            // Load first byte
            ; movzx r10d, BYTE [r12]
            // Test continuation bit
            ; test r10d, 0x80
            ; jnz =>slow_path
            // Fast path: single-byte varint
            ; add r12, 1
            ; jmp =>done_label

            // Slow path: call full varint decode intrinsic
            ; =>slow_path
            ; mov [r15 + CTX_INPUT_PTR as i32], r12
            ; mov rdi, r15
            ; lea rsi, [rsp + 48]             // temp u32 on stack
            ; mov rax, QWORD ptr_val
            ; call rax
            ; mov r12, [r15 + CTX_INPUT_PTR as i32]
            ; mov r11d, [r15 + CTX_ERROR_CODE as i32]
            ; test r11d, r11d
            ; jnz =>error_exit
            ; mov r10d, DWORD [rsp + 48]      // load decoded discriminant
            ; jmp =>done_label

            ; =>eof_label
            ; mov DWORD [r15 + CTX_ERROR_CODE as i32], crate::context::ErrorCode::UnexpectedEof as i32
            ; jmp =>error_exit

            ; =>done_label
        );
    }

    /// Compare r10d (discriminant) with immediate `imm` and branch to `label`
    /// if equal.
    pub fn emit_cmp_imm_branch_eq(&mut self, imm: u32, label: DynamicLabel) {
        dynasm!(self.ops
            ; .arch x64
            ; cmp r10d, imm as i32
            ; je =>label
        );
    }

    /// Emit a branch-to-error for unknown variant (sets UnknownVariant error code).
    pub fn emit_unknown_variant_error(&mut self) {
        let error_exit = self.error_exit;
        let error_code = crate::context::ErrorCode::UnknownVariant as i32;

        dynasm!(self.ops
            ; .arch x64
            ; mov DWORD [r15 + CTX_ERROR_CODE as i32], error_code
            ; jmp =>error_exit
        );
    }

    /// Save the cached input_ptr (r12) to a stack slot.
    pub fn emit_save_input_ptr(&mut self, stack_offset: u32) {
        dynasm!(self.ops
            ; .arch x64
            ; mov [rsp + stack_offset as i32], r12
        );
    }

    /// Restore the cached input_ptr (r12) from a stack slot.
    pub fn emit_restore_input_ptr(&mut self, stack_offset: u32) {
        dynasm!(self.ops
            ; .arch x64
            ; mov r12, [rsp + stack_offset as i32]
        );
    }

    /// Store a 64-bit immediate into a stack slot at rsp + offset.
    pub fn emit_store_imm64_to_stack(&mut self, stack_offset: u32, value: u64) {
        let val = value as i64;
        dynasm!(self.ops
            ; .arch x64
            ; mov rax, QWORD val
            ; mov [rsp + stack_offset as i32], rax
        );
    }

    /// AND a 64-bit immediate into a stack slot at rsp + offset.
    /// Loads the slot, ANDs with the immediate, stores back.
    pub fn emit_and_imm64_on_stack(&mut self, stack_offset: u32, mask: u64) {
        let mask_val = mask as i64;
        dynasm!(self.ops
            ; .arch x64
            ; mov rax, [rsp + stack_offset as i32]
            ; mov r10, QWORD mask_val
            ; and rax, r10
            ; mov [rsp + stack_offset as i32], rax
        );
    }

    /// Check if the stack slot at rsp + offset has exactly one bit set (popcount == 1).
    /// If so, branch to `label`.
    pub fn emit_popcount_eq1_branch(&mut self, stack_offset: u32, label: DynamicLabel) {
        dynasm!(self.ops
            ; .arch x64
            ; mov rax, [rsp + stack_offset as i32]
            // popcount == 1 iff (x & (x-1)) == 0 && x != 0
            ; lea r10, [rax - 1]
            ; test rax, r10        // sets Z if (x & (x-1)) == 0
            ; jnz >skip            // more than 1 bit
            ; test rax, rax        // check nonzero
            ; jnz =>label          // exactly 1 bit
            ; skip:
        );
    }

    /// Check if the stack slot at rsp + offset is zero. If so, branch to `label`.
    pub fn emit_stack_zero_branch(&mut self, stack_offset: u32, label: DynamicLabel) {
        dynasm!(self.ops
            ; .arch x64
            ; mov rax, [rsp + stack_offset as i32]
            ; test rax, rax
            ; jz =>label
        );
    }

    /// Load the stack slot at rsp + offset, test if bit `bit_index` is set,
    /// and branch to `label` if so.
    pub fn emit_test_bit_branch(&mut self, stack_offset: u32, bit_index: u32, label: DynamicLabel) {
        let mask = (1u64 << bit_index) as i64;
        dynasm!(self.ops
            ; .arch x64
            ; mov rax, [rsp + stack_offset as i32]
            ; mov r10, QWORD mask
            ; test rax, r10
            ; jnz =>label
        );
    }

    /// Emit an error (write error code to ctx, jump to error_exit).
    pub fn emit_error(&mut self, code: crate::context::ErrorCode) {
        let error_exit = self.error_exit;
        let error_code = code as i32;
        dynasm!(self.ops
            ; .arch x64
            ; mov DWORD [r15 + CTX_ERROR_CODE as i32], error_code
            ; jmp =>error_exit
        );
    }

    /// Advance the cached cursor by n bytes (inline, no function call).
    pub fn emit_advance_cursor_by(&mut self, n: u32) {
        dynasm!(self.ops
            ; .arch x64
            ; add r12, n as i32
        );
    }

    // ── Option support ────────────────────────────────────────────────

    /// Save the current `out` pointer (r14) and redirect it to a stack scratch area.
    pub fn emit_redirect_out_to_stack(&mut self, scratch_offset: u32) {
        dynasm!(self.ops
            ; .arch x64
            ; mov [rsp + (scratch_offset as i32 - 8)], r14
            ; lea r14, [rsp + scratch_offset as i32]
        );
    }

    /// Restore the `out` pointer (r14) from the saved slot.
    pub fn emit_restore_out(&mut self, scratch_offset: u32) {
        dynasm!(self.ops
            ; .arch x64
            ; mov r14, [rsp + (scratch_offset as i32 - 8)]
        );
    }

    /// Call fad_option_init_none(init_none_fn, out + offset).
    /// Does not touch ctx or the cursor.
    pub fn emit_call_option_init_none(&mut self, wrapper_fn: *const u8, init_none_fn: *const u8, offset: u32) {
        let wrapper_val = wrapper_fn as i64;
        let init_none_val = init_none_fn as i64;

        dynasm!(self.ops
            ; .arch x64
            // arg0 (rdi): init_none_fn pointer
            ; mov rdi, QWORD init_none_val
            // arg1 (rsi): out + offset
            ; lea rsi, [r14 + offset as i32]
            // Call wrapper
            ; mov rax, QWORD wrapper_val
            ; call rax
        );
    }

    /// Call fad_option_init_some(init_some_fn, out + offset, sp + scratch_offset).
    /// Does not touch ctx or the cursor.
    pub fn emit_call_option_init_some(
        &mut self,
        wrapper_fn: *const u8,
        init_some_fn: *const u8,
        offset: u32,
        scratch_offset: u32,
    ) {
        let wrapper_val = wrapper_fn as i64;
        let init_some_val = init_some_fn as i64;

        dynasm!(self.ops
            ; .arch x64
            // arg0 (rdi): init_some_fn pointer
            ; mov rdi, QWORD init_some_val
            // arg1 (rsi): out + offset
            ; lea rsi, [r14 + offset as i32]
            // arg2 (rdx): scratch area on stack
            ; lea rdx, [rsp + scratch_offset as i32]
            // Call wrapper
            ; mov rax, QWORD wrapper_val
            ; call rax
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
