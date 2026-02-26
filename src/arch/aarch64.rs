use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi, DynamicLabel, AssemblyOffset};

use crate::context::{CTX_ERROR_CODE, CTX_INPUT_END, CTX_INPUT_PTR};

pub type Assembler = dynasmrt::aarch64::Assembler;

/// Emission context — wraps the assembler plus bookkeeping labels.
pub struct EmitCtx {
    pub ops: Assembler,
    pub error_exit: DynamicLabel,
    pub entry: AssemblyOffset,
}

// Register assignments (all callee-saved):
//   x19 = cached input_ptr
//   x20 = cached input_end
//   x21 = out pointer
//   x22 = ctx pointer

impl EmitCtx {
    /// Create a new EmitCtx and emit the function prologue.
    ///
    /// The generated function has signature:
    ///   `unsafe extern "C" fn(out: *mut u8, ctx: *mut DeserContext)`
    ///
    /// Prologue saves callee-saved registers and caches input_ptr/input_end.
    pub fn new() -> Self {
        let mut ops = Assembler::new().expect("failed to create assembler");
        let error_exit = ops.new_dynamic_label();
        let entry = ops.offset();

        dynasm!(ops
            ; .arch aarch64
            // Save frame pointer + link register, and callee-saved x19-x22
            // We need 6 registers saved: x29, x30, x19, x20, x21, x22 = 48 bytes
            // Round up to 48 (already 16-byte aligned: 48 = 3*16)
            ; stp x29, x30, [sp, #-48]!
            ; stp x19, x20, [sp, #16]
            ; stp x21, x22, [sp, #32]
            ; mov x29, sp

            // Save arguments to callee-saved registers
            ; mov x21, x0              // x21 = out
            ; mov x22, x1              // x22 = ctx

            // Cache input cursor from ctx
            ; ldr x19, [x22, #CTX_INPUT_PTR]  // x19 = ctx.input_ptr
            ; ldr x20, [x22, #CTX_INPUT_END]  // x20 = ctx.input_end
        );

        EmitCtx {
            ops,
            error_exit,
            entry,
        }
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

    /// Emit the success epilogue and error exit path. Finalizes the assembler.
    pub fn finalize(mut self) -> dynasmrt::ExecutableBuffer {
        let error_exit = self.error_exit;

        dynasm!(self.ops
            ; .arch aarch64
            // Success path: flush cursor, restore registers, return
            ; str x19, [x22, #CTX_INPUT_PTR]
            ; ldp x21, x22, [sp, #32]
            ; ldp x19, x20, [sp, #16]
            ; ldp x29, x30, [sp], #48
            ; ret

            // Error exit: just restore and return (error is already in ctx.error)
            ; =>error_exit
            ; ldp x21, x22, [sp, #32]
            ; ldp x19, x20, [sp, #16]
            ; ldp x29, x30, [sp], #48
            ; ret
        );

        self.ops.commit().expect("failed to commit assembly");
        self.ops.finalize().expect("failed to finalize assembly")
    }
}
