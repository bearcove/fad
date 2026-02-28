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

    // ── Vec support ──────────────────────────────────────────────────

    /// Call fad_vec_alloc with a constant count (for JSON initial allocation).
    ///
    /// Result (buf pointer) is in rax.
    pub fn emit_call_json_vec_initial_alloc(
        &mut self,
        alloc_fn: *const u8,
        count: u32,
        elem_size: u32,
        elem_align: u32,
    ) {
        let error_exit = self.error_exit;
        let ptr_val = alloc_fn as i64;

        dynasm!(self.ops
            ; .arch x64
            // Flush cursor
            ; mov [r15 + CTX_INPUT_PTR as i32], r12
            // Args: rdi=ctx, rsi=count, rdx=elem_size, rcx=elem_align
            ; mov rdi, r15
            ; mov esi, count as i32
            ; mov edx, elem_size as i32
            ; mov ecx, elem_align as i32
            // Call
            ; mov rax, QWORD ptr_val
            ; call rax
            // Reload cursor
            ; mov r12, [r15 + CTX_INPUT_PTR as i32]
            // Check error
            ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
            ; test r10d, r10d
            ; jnz =>error_exit
        );
    }

    /// Initialize JSON Vec loop state: buf from rax, len=0, cap=initial_cap.
    pub fn emit_json_vec_loop_init(
        &mut self,
        saved_out_slot: u32,
        buf_slot: u32,
        len_slot: u32,
        cap_slot: u32,
        initial_cap: u32,
    ) {
        dynasm!(self.ops
            ; .arch x64
            ; mov [rsp + saved_out_slot as i32], r14  // save out pointer
            ; mov [rsp + buf_slot as i32], rax         // buf = alloc result
            ; mov QWORD [rsp + len_slot as i32], 0     // len = 0
            ; mov DWORD [rsp + cap_slot as i32], initial_cap as i32
            ; mov DWORD [rsp + (cap_slot as i32 + 4)], 0  // zero upper 32 bits
        );
    }

    /// Check ctx.error.code and branch to label if nonzero.
    pub fn emit_check_error_branch(&mut self, label: DynamicLabel) {
        dynasm!(self.ops
            ; .arch x64
            ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
            ; test r10d, r10d
            ; jnz =>label
        );
    }

    /// Save the count register (w9 on aarch64, r10d on x64) to a stack slot.
    ///
    /// Used to preserve the count across a function call (r10 is caller-saved).
    pub fn emit_save_count_to_stack(&mut self, slot: u32) {
        dynasm!(self.ops
            ; .arch x64
            ; mov [rsp + slot as i32], r10
        );
    }

    /// Call fad_vec_alloc(ctx, count, elem_size, elem_align).
    ///
    /// count is in r10d (from emit_read_postcard_discriminant or JSON parse).
    /// Result (buf pointer) is in rax.
    ///
    /// **Important**: r10 is caller-saved and will be clobbered by the call.
    /// The count must be saved to a stack slot before calling this.
    pub fn emit_call_vec_alloc(
        &mut self,
        alloc_fn: *const u8,
        elem_size: u32,
        elem_align: u32,
    ) {
        let error_exit = self.error_exit;
        let ptr_val = alloc_fn as i64;

        dynasm!(self.ops
            ; .arch x64
            // Flush cursor
            ; mov [r15 + CTX_INPUT_PTR as i32], r12
            // Args: rdi=ctx, rsi=count(r10), rdx=elem_size, rcx=elem_align
            ; mov rdi, r15
            ; mov rsi, r10             // count (zero-extended from r10d)
            ; mov edx, elem_size as i32
            ; mov ecx, elem_align as i32
            // Call
            ; mov rax, QWORD ptr_val
            ; call rax
            // Reload cursor
            ; mov r12, [r15 + CTX_INPUT_PTR as i32]
            // Check error
            ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
            ; test r10d, r10d
            ; jnz =>error_exit
        );
    }

    /// Call fad_vec_grow(ctx, old_buf, len, old_cap, new_cap, elem_size, elem_align).
    ///
    /// Reads old_buf, len, old_cap from stack slots. Computes new_cap = old_cap * 2.
    /// After call: new buf pointer is in rax.
    pub fn emit_call_vec_grow(
        &mut self,
        grow_fn: *const u8,
        buf_slot: u32,
        len_slot: u32,
        cap_slot: u32,
        elem_size: u32,
        elem_align: u32,
    ) {
        let error_exit = self.error_exit;
        let ptr_val = grow_fn as i64;

        dynasm!(self.ops
            ; .arch x64
            // Flush cursor
            ; mov [r15 + CTX_INPUT_PTR as i32], r12
            // Compute new_cap = old_cap * 2
            ; mov r10, [rsp + cap_slot as i32]
            ; shl r10, 1                         // new_cap = old_cap << 1
            // Args: rdi=ctx, rsi=old_buf, rdx=len, rcx=old_cap, r8=new_cap, r9=elem_size
            // 7th arg (elem_align) goes on the stack per System V ABI
            ; mov rdi, r15
            ; mov rsi, [rsp + buf_slot as i32]
            ; mov rdx, [rsp + len_slot as i32]
            ; mov rcx, [rsp + cap_slot as i32]
            ; mov r8, r10                         // new_cap
            ; mov r9d, elem_size as i32
            // Push 7th arg (elem_align) onto the stack
            ; push elem_align as i32
            // Call
            ; mov rax, QWORD ptr_val
            ; call rax
            // Pop the 7th arg
            ; add rsp, 8
            // Reload cursor
            ; mov r12, [r15 + CTX_INPUT_PTR as i32]
            // Check error
            ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
            ; test r10d, r10d
            ; jnz =>error_exit
            // Update buf and cap on stack
            ; mov [rsp + buf_slot as i32], rax    // new buf
            ; mov r10, [rsp + cap_slot as i32]
            ; shl r10, 1
            ; mov [rsp + cap_slot as i32], r10    // new cap
        );
    }

    /// Initialize Vec loop state after allocation.
    ///
    /// Saves: out pointer, buf (alloc result in rax), counter=0.
    /// Count must already be stored at count_slot (via emit_save_r10_to_stack).
    /// Initialize Vec loop state with cursor in rbx (callee-saved), end on stack.
    ///
    /// Saves rbx to stack, then sets rbx = buf (cursor), end_slot = buf + count * elem_size.
    /// Also saves out pointer and buf for final Vec store.
    pub fn emit_vec_loop_init_cursor(
        &mut self,
        saved_out_slot: u32,
        buf_slot: u32,
        count_slot: u32,
        save_rbx_slot: u32,
        end_slot: u32,
        elem_size: u32,
    ) {
        dynasm!(self.ops
            ; .arch x64
            ; mov [rsp + saved_out_slot as i32], r14  // save out pointer
            ; mov [rsp + buf_slot as i32], rax        // buf = alloc result
            // Save callee-saved rbx
            ; mov [rsp + save_rbx_slot as i32], rbx
            // rbx = cursor = buf
            ; mov rbx, rax
            // end = buf + count * elem_size
            ; mov r10, [rsp + count_slot as i32]
            ; imul r10, r10, elem_size as i32
            ; add r10, rax
            ; mov [rsp + end_slot as i32], r10
        );
    }

    /// Set out = cursor (r14 = rbx). Single register move.
    pub fn emit_vec_loop_load_cursor(&mut self, _save_rbx_slot: u32) {
        dynasm!(self.ops
            ; .arch x64
            ; mov r14, rbx
        );
    }

    /// Advance cursor register, check error, branch back if cursor < end.
    /// Cursor advance is register-only; end compare reads from stack (x64 can cmp reg, [mem]).
    pub fn emit_vec_loop_advance_cursor(
        &mut self,
        _save_rbx_slot: u32,
        end_slot: u32,
        elem_size: u32,
        loop_label: DynamicLabel,
        error_cleanup_label: DynamicLabel,
    ) {
        dynasm!(self.ops
            ; .arch x64
            // Check error from element deserialization
            ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
            ; test r10d, r10d
            ; jnz =>error_cleanup_label
            // cursor += elem_size (register only)
            ; add rbx, elem_size as i32
            // Compare with end (cmp reg, [mem] — single instruction on x64)
            ; cmp rbx, [rsp + end_slot as i32]
            ; jb =>loop_label
        );
    }

    /// Advance the cursor register and loop back, without checking the error flag.
    ///
    /// Use this when all error paths within the loop body branch directly to
    /// the error cleanup label (e.g. via redirected `error_exit`), making
    /// the per-iteration error check redundant.
    pub fn emit_vec_loop_advance_no_error_check(
        &mut self,
        end_slot: u32,
        elem_size: u32,
        loop_label: DynamicLabel,
    ) {
        dynasm!(self.ops
            ; .arch x64
            ; add rbx, elem_size as i32
            ; cmp rbx, [rsp + end_slot as i32]
            ; jb =>loop_label
        );
    }

    /// Restore rbx from stack. Must be called on every exit path from a Vec loop.
    pub fn emit_vec_restore_callee_saved(&mut self, save_rbx_slot: u32, _end_slot: u32) {
        dynasm!(self.ops
            ; .arch x64
            ; mov rbx, [rsp + save_rbx_slot as i32]
        );
    }

    /// Emit Vec loop header: compute slot = buf + i * elem_size, set out = slot.
    ///
    /// Used by JSON where buf can change on growth and index-based access is needed.
    pub fn emit_vec_loop_slot(&mut self, buf_slot: u32, counter_slot: u32, elem_size: u32) {
        dynasm!(self.ops
            ; .arch x64
            ; mov r14, [rsp + buf_slot as i32]         // buf
            ; mov r10, [rsp + counter_slot as i32]     // i
            ; imul r10, r10, elem_size as i32          // i * elem_size
            ; add r14, r10                              // out = buf + i * elem_size
        );
    }

    /// Write Vec fields (ptr, len, cap) to out + base_offset using discovered offsets.
    /// Reads buf and count from stack slots.
    pub fn emit_vec_store(
        &mut self,
        base_offset: u32,
        saved_out_slot: u32,
        buf_slot: u32,
        len_slot: u32,
        cap_slot: u32,
        offsets: &crate::malum::VecOffsets,
    ) {
        let ptr_off = (base_offset + offsets.ptr_offset) as i32;
        let len_off = (base_offset + offsets.len_offset) as i32;
        let cap_off = (base_offset + offsets.cap_offset) as i32;

        dynasm!(self.ops
            ; .arch x64
            // Restore out pointer
            ; mov r14, [rsp + saved_out_slot as i32]
            // Load values and store at discovered offsets
            ; mov rax, [rsp + buf_slot as i32]
            ; mov [r14 + ptr_off], rax
            ; mov rax, [rsp + len_slot as i32]
            ; mov [r14 + len_off], rax
            ; mov rax, [rsp + cap_slot as i32]
            ; mov [r14 + cap_off], rax
        );
    }

    /// Write an empty Vec to out + base_offset with proper dangling pointer.
    pub fn emit_vec_store_empty_with_align(
        &mut self,
        base_offset: u32,
        elem_align: u32,
        offsets: &crate::malum::VecOffsets,
    ) {
        let ptr_off = (base_offset + offsets.ptr_offset) as i32;
        let len_off = (base_offset + offsets.len_offset) as i32;
        let cap_off = (base_offset + offsets.cap_offset) as i32;

        // Vec::new() writes: ptr = NonNull::dangling() = elem_align as *mut T, len = 0, cap = 0.
        dynasm!(self.ops
            ; .arch x64
            ; mov QWORD [r14 + ptr_off], elem_align as i32
            ; mov QWORD [r14 + len_off], 0
            ; mov QWORD [r14 + cap_off], 0
        );
    }

    /// Emit error cleanup for Vec: free the buffer and branch to error exit.
    /// Called when element deserialization fails mid-loop.
    pub fn emit_vec_error_cleanup(
        &mut self,
        free_fn: *const u8,
        saved_out_slot: u32,
        buf_slot: u32,
        cap_slot: u32,
        elem_size: u32,
        elem_align: u32,
    ) {
        let error_exit = self.error_exit;
        let ptr_val = free_fn as i64;

        dynasm!(self.ops
            ; .arch x64
            // Restore out pointer first
            ; mov r14, [rsp + saved_out_slot as i32]
            // Args: rdi=buf, rsi=cap, rdx=elem_size, rcx=elem_align
            ; mov rdi, [rsp + buf_slot as i32]
            ; mov rsi, [rsp + cap_slot as i32]
            ; mov edx, elem_size as i32
            ; mov ecx, elem_align as i32
            // Call fad_vec_free
            ; mov rax, QWORD ptr_val
            ; call rax
            ; jmp =>error_exit
        );
    }

    /// Compare the count register (w9 on aarch64, r10d on x64) with zero
    /// and branch to label if equal.
    pub fn emit_cbz_count(&mut self, label: DynamicLabel) {
        dynasm!(self.ops
            ; .arch x64
            ; test r10d, r10d
            ; jz =>label
        );
    }

    /// Compare two stack slot values and branch if equal (len == cap for growth check).
    pub fn emit_cmp_stack_slots_branch_eq(
        &mut self,
        slot_a: u32,
        slot_b: u32,
        label: DynamicLabel,
    ) {
        dynasm!(self.ops
            ; .arch x64
            ; mov rax, [rsp + slot_a as i32]
            ; cmp rax, [rsp + slot_b as i32]
            ; je =>label
        );
    }

    /// Increment a stack slot value by 1.
    pub fn emit_inc_stack_slot(&mut self, slot: u32) {
        dynasm!(self.ops
            ; .arch x64
            ; mov rax, [rsp + slot as i32]
            ; add rax, 1
            ; mov [rsp + slot as i32], rax
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
