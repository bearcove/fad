use dynasmrt::AssemblyOffset;

use crate::linearize::LinearIr;

pub struct LinearBackendResult {
    pub buf: dynasmrt::ExecutableBuffer,
    pub entry: AssemblyOffset,
}

pub fn compile_linear_ir(ir: &LinearIr) -> LinearBackendResult {
    #[cfg(target_arch = "x86_64")]
    {
        compile_linear_ir_x64(ir)
    }

    #[cfg(target_arch = "aarch64")]
    {
        compile_linear_ir_aarch64(ir)
    }
}

#[cfg(target_arch = "x86_64")]
fn compile_linear_ir_x64(ir: &LinearIr) -> LinearBackendResult {
    use dynasmrt::{DynamicLabel, DynasmApi, DynasmLabelApi, dynasm};

    use crate::arch::{BASE_FRAME, EmitCtx};
    use crate::ir::Width;
    use crate::linearize::{BinOpKind, LabelId, LinearOp, UnaryOpKind};
    use crate::recipe::{Op, Recipe, Slot};

    struct FunctionCtx {
        error_exit: DynamicLabel,
        data_results: Vec<crate::ir::VReg>,
    }

    struct Lowerer {
        ectx: EmitCtx,
        labels: Vec<DynamicLabel>,
        lambda_labels: Vec<DynamicLabel>,
        slot_base: u32,
        vreg_base: u32,
        entry: Option<AssemblyOffset>,
        current_func: Option<FunctionCtx>,
        x9_cached_vreg: Option<crate::ir::VReg>,
        const_vregs: Vec<Option<u64>>,
    }

    #[derive(Clone, Copy)]
    enum IntrinsicArg {
        VReg(crate::ir::VReg),
        OutField(u32),
        OutStack(u32),
    }

    impl Lowerer {
        fn new(ir: &LinearIr) -> Self {
            let slot_base = BASE_FRAME;
            let slot_bytes = ir.slot_count * 8;
            let vreg_base = slot_base + slot_bytes;
            let vreg_bytes = ir.vreg_count * 8;
            let extra_stack = slot_bytes + vreg_bytes + 8;

            let mut ectx = EmitCtx::new(extra_stack);

            let labels: Vec<DynamicLabel> = (0..ir.label_count).map(|_| ectx.new_label()).collect();

            let mut lambda_max = 0usize;
            for op in &ir.ops {
                match op {
                    LinearOp::FuncStart { lambda_id, .. } => {
                        lambda_max = lambda_max.max(lambda_id.index() as usize);
                    }
                    LinearOp::CallLambda { target, .. } => {
                        lambda_max = lambda_max.max(target.index() as usize);
                    }
                    _ => {}
                }
            }
            let lambda_labels: Vec<DynamicLabel> =
                (0..=lambda_max).map(|_| ectx.new_label()).collect();

            Self {
                ectx,
                labels,
                lambda_labels,
                slot_base,
                vreg_base,
                entry: None,
                current_func: None,
                x9_cached_vreg: None,
                const_vregs: vec![None; ir.vreg_count as usize],
            }
        }

        fn vreg_off(&self, v: crate::ir::VReg) -> u32 {
            self.vreg_base + (v.index() as u32) * 8
        }

        fn slot_off(&self, s: crate::ir::SlotId) -> u32 {
            self.slot_base + (s.index() as u32) * 8
        }

        fn label(&self, label: LabelId) -> DynamicLabel {
            self.labels[label.index() as usize]
        }

        fn emit_recipe_ops(&mut self, ops: Vec<Op>) {
            self.ectx.emit_recipe(&Recipe {
                ops,
                label_count: 0,
            });
        }

        fn emit_load_vreg_r10(&mut self, v: crate::ir::VReg) {
            let off = self.vreg_off(v) as i32;
            dynasm!(self.ectx.ops
                ; .arch x64
                ; mov r10, [rsp + off]
            );
        }

        fn emit_store_r10_to_vreg(&mut self, v: crate::ir::VReg) {
            let off = self.vreg_off(v) as i32;
            dynasm!(self.ectx.ops
                ; .arch x64
                ; mov [rsp + off], r10
            );
        }

        fn emit_read_from_field(&mut self, dst: crate::ir::VReg, offset: u32, width: Width) {
            let off = offset as i32;
            match width {
                Width::W1 => dynasm!(self.ectx.ops ; .arch x64 ; movzx r10d, BYTE [r14 + off]),
                Width::W2 => dynasm!(self.ectx.ops ; .arch x64 ; movzx r10d, WORD [r14 + off]),
                Width::W4 => dynasm!(self.ectx.ops ; .arch x64 ; mov r10d, DWORD [r14 + off]),
                Width::W8 => dynasm!(self.ectx.ops ; .arch x64 ; mov r10, QWORD [r14 + off]),
            }
            self.emit_store_r10_to_vreg(dst);
        }

        fn emit_write_to_field(&mut self, src: crate::ir::VReg, offset: u32, width: Width) {
            self.emit_load_vreg_r10(src);
            let off = offset as i32;
            match width {
                Width::W1 => dynasm!(self.ectx.ops ; .arch x64 ; mov BYTE [r14 + off], r10b),
                Width::W2 => dynasm!(self.ectx.ops ; .arch x64 ; mov WORD [r14 + off], r10w),
                Width::W4 => dynasm!(self.ectx.ops ; .arch x64 ; mov DWORD [r14 + off], r10d),
                Width::W8 => dynasm!(self.ectx.ops ; .arch x64 ; mov QWORD [r14 + off], r10),
            }
        }

        fn emit_save_out_ptr(&mut self, dst: crate::ir::VReg) {
            let off = self.vreg_off(dst) as i32;
            dynasm!(self.ectx.ops
                ; .arch x64
                ; mov [rsp + off], r14
            );
        }

        fn emit_set_out_ptr(&mut self, src: crate::ir::VReg) {
            let off = self.vreg_off(src) as i32;
            dynasm!(self.ectx.ops
                ; .arch x64
                ; mov r14, [rsp + off]
            );
        }

        fn emit_slot_addr(&mut self, dst: crate::ir::VReg, slot: crate::ir::SlotId) {
            let dst_off = self.vreg_off(dst) as i32;
            let slot_off = self.slot_off(slot) as i32;
            dynasm!(self.ectx.ops
                ; .arch x64
                ; lea r10, [rsp + slot_off]
                ; mov [rsp + dst_off], r10
            );
        }

        fn emit_read_bytes(&mut self, dst: crate::ir::VReg, count: u32) {
            self.emit_recipe_ops(vec![Op::BoundsCheck { count }]);
            match count {
                1 => dynasm!(self.ectx.ops ; .arch x64 ; movzx r10d, BYTE [r12]),
                2 => dynasm!(self.ectx.ops ; .arch x64 ; movzx r10d, WORD [r12]),
                4 => dynasm!(self.ectx.ops ; .arch x64 ; mov r10d, DWORD [r12]),
                8 => dynasm!(self.ectx.ops ; .arch x64 ; mov r10, QWORD [r12]),
                _ => panic!("unsupported ReadBytes count: {count}"),
            }
            self.emit_store_r10_to_vreg(dst);
            self.ectx.emit_advance_cursor_by(count);
        }

        fn emit_peek_byte(&mut self, dst: crate::ir::VReg) {
            self.emit_recipe_ops(vec![Op::BoundsCheck { count: 1 }]);
            dynasm!(self.ectx.ops ; .arch x64 ; movzx r10d, BYTE [r12]);
            self.emit_store_r10_to_vreg(dst);
        }

        fn emit_binop(
            &mut self,
            kind: BinOpKind,
            dst: crate::ir::VReg,
            lhs: crate::ir::VReg,
            rhs: crate::ir::VReg,
        ) {
            self.emit_load_vreg_r10(lhs);
            let rhs_off = self.vreg_off(rhs) as i32;
            match kind {
                BinOpKind::Add => dynasm!(self.ectx.ops ; .arch x64 ; add r10, [rsp + rhs_off]),
                BinOpKind::Sub => dynasm!(self.ectx.ops ; .arch x64 ; sub r10, [rsp + rhs_off]),
                BinOpKind::And => dynasm!(self.ectx.ops ; .arch x64 ; and r10, [rsp + rhs_off]),
                BinOpKind::Or => dynasm!(self.ectx.ops ; .arch x64 ; or r10, [rsp + rhs_off]),
                BinOpKind::Xor => dynasm!(self.ectx.ops ; .arch x64 ; xor r10, [rsp + rhs_off]),
                BinOpKind::CmpNe => dynasm!(self.ectx.ops
                    ; .arch x64
                    ; cmp r10, [rsp + rhs_off]
                    ; setne r10b
                    ; movzx r10, r10b
                ),
                BinOpKind::Shr => dynasm!(self.ectx.ops
                    ; .arch x64
                    ; mov rcx, [rsp + rhs_off]
                    ; shr r10, cl
                ),
                BinOpKind::Shl => dynasm!(self.ectx.ops
                    ; .arch x64
                    ; mov rcx, [rsp + rhs_off]
                    ; shl r10, cl
                ),
            }
            self.emit_store_r10_to_vreg(dst);
        }

        fn emit_unary(&mut self, kind: UnaryOpKind, dst: crate::ir::VReg, src: crate::ir::VReg) {
            self.emit_load_vreg_r10(src);
            match kind {
                UnaryOpKind::ZigzagDecode { wide: true } => {
                    dynasm!(self.ectx.ops
                        ; .arch x64
                        ; mov r11, r10
                        ; shr r11, 1
                        ; and r10, 1
                        ; neg r10
                        ; xor r10, r11
                    );
                }
                UnaryOpKind::ZigzagDecode { wide: false } => {
                    dynasm!(self.ectx.ops
                        ; .arch x64
                        ; mov r11d, r10d
                        ; shr r11d, 1
                        ; and r10d, 1
                        ; neg r10d
                        ; xor r10d, r11d
                    );
                }
                UnaryOpKind::SignExtend { from_width } => match from_width {
                    Width::W1 => dynasm!(self.ectx.ops ; .arch x64 ; movsx r10, r10b),
                    Width::W2 => dynasm!(self.ectx.ops ; .arch x64 ; movsx r10, r10w),
                    Width::W4 => dynasm!(self.ectx.ops ; .arch x64 ; movsxd r10, r10d),
                    Width::W8 => {}
                },
            }
            self.emit_store_r10_to_vreg(dst);
        }

        fn emit_branch_if(&mut self, cond: crate::ir::VReg, target: DynamicLabel, invert: bool) {
            self.emit_load_vreg_r10(cond);
            if invert {
                dynasm!(self.ectx.ops
                    ; .arch x64
                    ; test r10, r10
                    ; jz =>target
                );
            } else {
                dynasm!(self.ectx.ops
                    ; .arch x64
                    ; test r10, r10
                    ; jnz =>target
                );
            }
        }

        fn emit_jump_table(
            &mut self,
            predicate: crate::ir::VReg,
            labels: &[LabelId],
            default: LabelId,
        ) {
            self.emit_load_vreg_r10(predicate);
            for (index, label) in labels.iter().enumerate() {
                let target = self.label(*label);
                dynasm!(self.ectx.ops
                    ; .arch x64
                    ; cmp r10d, index as i32
                    ; je =>target
                );
            }
            self.ectx.emit_branch(self.label(default));
        }

        fn emit_load_intrinsic_arg_rsi(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v) as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; mov rsi, [rsp + off]);
                }
                IntrinsicArg::OutField(offset) => {
                    let off = offset as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; lea rsi, [r14 + off]);
                }
                IntrinsicArg::OutStack(offset) => {
                    let off = offset as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; lea rsi, [rsp + off]);
                }
            }
        }

        fn emit_load_intrinsic_arg_rdx(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v) as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; mov rdx, [rsp + off]);
                }
                IntrinsicArg::OutField(offset) => {
                    let off = offset as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; lea rdx, [r14 + off]);
                }
                IntrinsicArg::OutStack(offset) => {
                    let off = offset as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; lea rdx, [rsp + off]);
                }
            }
        }

        fn emit_load_intrinsic_arg_rcx(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v) as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; mov rcx, [rsp + off]);
                }
                IntrinsicArg::OutField(offset) => {
                    let off = offset as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; lea rcx, [r14 + off]);
                }
                IntrinsicArg::OutStack(offset) => {
                    let off = offset as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; lea rcx, [rsp + off]);
                }
            }
        }

        fn emit_load_intrinsic_arg_r8(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v) as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; mov r8, [rsp + off]);
                }
                IntrinsicArg::OutField(offset) => {
                    let off = offset as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; lea r8, [r14 + off]);
                }
                IntrinsicArg::OutStack(offset) => {
                    let off = offset as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; lea r8, [rsp + off]);
                }
            }
        }

        fn emit_load_intrinsic_arg_r9(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v) as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; mov r9, [rsp + off]);
                }
                IntrinsicArg::OutField(offset) => {
                    let off = offset as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; lea r9, [r14 + off]);
                }
                IntrinsicArg::OutStack(offset) => {
                    let off = offset as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; lea r9, [rsp + off]);
                }
            }
        }

        fn emit_load_intrinsic_arg_r10(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v) as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; mov r10, [rsp + off]);
                }
                IntrinsicArg::OutField(offset) => {
                    let off = offset as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; lea r10, [r14 + off]);
                }
                IntrinsicArg::OutStack(offset) => {
                    let off = offset as i32;
                    dynasm!(self.ectx.ops ; .arch x64 ; lea r10, [rsp + off]);
                }
            }
        }

        fn emit_call_intrinsic_with_args(&mut self, fn_ptr: *const u8, args: &[IntrinsicArg]) {
            use crate::context::{CTX_ERROR_CODE, CTX_INPUT_PTR};

            let error_exit = self
                .current_func
                .as_ref()
                .expect("CallIntrinsic outside function")
                .error_exit;

            // Flush cursor before call.
            dynasm!(self.ectx.ops
                ; .arch x64
                ; mov [r15 + CTX_INPUT_PTR as i32], r12
            );

            #[cfg(not(windows))]
            {
                if args.len() > 5 {
                    panic!(
                        "unsupported CallIntrinsic arity in linear backend adapter: {} args (+ctx)",
                        args.len()
                    );
                }
                dynasm!(self.ectx.ops ; .arch x64 ; mov rdi, r15);
                if let Some(arg) = args.first().copied() {
                    self.emit_load_intrinsic_arg_rsi(arg);
                }
                if let Some(arg) = args.get(1).copied() {
                    self.emit_load_intrinsic_arg_rdx(arg);
                }
                if let Some(arg) = args.get(2).copied() {
                    self.emit_load_intrinsic_arg_rcx(arg);
                }
                if let Some(arg) = args.get(3).copied() {
                    self.emit_load_intrinsic_arg_r8(arg);
                }
                if let Some(arg) = args.get(4).copied() {
                    self.emit_load_intrinsic_arg_r9(arg);
                }
            }

            #[cfg(windows)]
            {
                if args.len() > 5 {
                    panic!(
                        "unsupported CallIntrinsic arity in linear backend adapter: {} args (+ctx)",
                        args.len()
                    );
                }
                dynasm!(self.ectx.ops ; .arch x64 ; mov rcx, r15);
                if let Some(arg) = args.first().copied() {
                    self.emit_load_intrinsic_arg_rdx(arg);
                }
                if let Some(arg) = args.get(1).copied() {
                    self.emit_load_intrinsic_arg_r8(arg);
                }
                if let Some(arg) = args.get(2).copied() {
                    self.emit_load_intrinsic_arg_r9(arg);
                }
                if let Some(arg) = args.get(3).copied() {
                    self.emit_load_intrinsic_arg_r10(arg);
                    dynasm!(self.ectx.ops ; .arch x64 ; mov [rsp + 32], r10);
                }
                if let Some(arg) = args.get(4).copied() {
                    self.emit_load_intrinsic_arg_r10(arg);
                    dynasm!(self.ectx.ops ; .arch x64 ; mov [rsp + 40], r10);
                }
            }

            let ptr_val = fn_ptr as i64;
            dynasm!(self.ectx.ops
                ; .arch x64
                ; mov rax, QWORD ptr_val
                ; call rax
            );

            // Reload cursor and branch to error exit if needed.
            dynasm!(self.ectx.ops
                ; .arch x64
                ; mov r12, [r15 + CTX_INPUT_PTR as i32]
                ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
                ; test r10d, r10d
                ; jnz =>error_exit
            );
        }

        // r[impl ir.intrinsics]
        fn emit_call_intrinsic(
            &mut self,
            func: crate::ir::IntrinsicFn,
            args: &[crate::ir::VReg],
            dst: Option<crate::ir::VReg>,
            field_offset: u32,
        ) {
            let fn_ptr = func.0 as *const u8;
            match dst {
                Some(dst) => {
                    if args.is_empty() {
                        // Legacy stack-out intrinsic ABI: fn(ctx, &mut out)
                        let out_offset = self.vreg_off(dst);
                        self.ectx.emit_store_imm64_to_stack(out_offset, 0);
                        self.emit_call_intrinsic_with_args(
                            fn_ptr,
                            &[IntrinsicArg::OutStack(out_offset)],
                        );
                    } else {
                        // Return-value intrinsic ABI: fn(ctx, args...) -> value
                        let call_args: Vec<IntrinsicArg> =
                            args.iter().copied().map(IntrinsicArg::VReg).collect();
                        self.emit_call_intrinsic_with_args(fn_ptr, &call_args);
                        let out_offset = self.vreg_off(dst) as i32;
                        dynasm!(self.ectx.ops
                            ; .arch x64
                            ; mov [rsp + out_offset], rax
                        );
                    }
                }
                None => {
                    let mut call_args: Vec<IntrinsicArg> =
                        args.iter().copied().map(IntrinsicArg::VReg).collect();
                    // Side-effect intrinsic ABI: fn(ctx, args..., out+field_offset)
                    call_args.push(IntrinsicArg::OutField(field_offset));
                    self.emit_call_intrinsic_with_args(fn_ptr, &call_args);
                }
            }
        }

        fn emit_store_incoming_lambda_args(&mut self, data_args: &[crate::ir::VReg]) {
            #[cfg(not(windows))]
            const MAX_LAMBDA_DATA_ARGS: usize = 4;
            #[cfg(windows)]
            const MAX_LAMBDA_DATA_ARGS: usize = 2;

            if data_args.len() > MAX_LAMBDA_DATA_ARGS {
                panic!(
                    "x64 CallLambda supports at most {MAX_LAMBDA_DATA_ARGS} data args, got {}",
                    data_args.len()
                );
            }

            for (i, &arg) in data_args.iter().enumerate() {
                let off = self.vreg_off(arg) as i32;
                #[cfg(not(windows))]
                match i {
                    0 => dynasm!(self.ectx.ops ; .arch x64 ; mov [rsp + off], rdx),
                    1 => dynasm!(self.ectx.ops ; .arch x64 ; mov [rsp + off], rcx),
                    2 => dynasm!(self.ectx.ops ; .arch x64 ; mov [rsp + off], r8),
                    3 => dynasm!(self.ectx.ops ; .arch x64 ; mov [rsp + off], r9),
                    _ => unreachable!(),
                }
                #[cfg(windows)]
                match i {
                    0 => dynasm!(self.ectx.ops ; .arch x64 ; mov [rsp + off], r8),
                    1 => dynasm!(self.ectx.ops ; .arch x64 ; mov [rsp + off], r9),
                    _ => unreachable!(),
                }
            }
        }

        fn emit_load_lambda_results_to_ret_regs(&mut self, data_results: &[crate::ir::VReg]) {
            if data_results.len() > 2 {
                panic!(
                    "x64 CallLambda supports at most 2 data results, got {}",
                    data_results.len()
                );
            }

            if let Some(&v) = data_results.first() {
                let off = self.vreg_off(v) as i32;
                dynasm!(self.ectx.ops ; .arch x64 ; mov rax, [rsp + off]);
            }
            if let Some(&v) = data_results.get(1) {
                let off = self.vreg_off(v) as i32;
                dynasm!(self.ectx.ops ; .arch x64 ; mov rdx, [rsp + off]);
            }
        }

        fn emit_call_lambda(
            &mut self,
            label: DynamicLabel,
            args: &[crate::ir::VReg],
            results: &[crate::ir::VReg],
        ) {
            use crate::context::{CTX_ERROR_CODE, CTX_INPUT_PTR};

            #[cfg(not(windows))]
            const MAX_LAMBDA_DATA_ARGS: usize = 4;
            #[cfg(windows)]
            const MAX_LAMBDA_DATA_ARGS: usize = 2;
            if args.len() > MAX_LAMBDA_DATA_ARGS {
                panic!(
                    "x64 CallLambda supports at most {MAX_LAMBDA_DATA_ARGS} data args, got {}",
                    args.len()
                );
            }
            if results.len() > 2 {
                panic!(
                    "x64 CallLambda supports at most 2 data results, got {}",
                    results.len()
                );
            }

            let error_exit = self
                .current_func
                .as_ref()
                .expect("CallLambda outside function")
                .error_exit;
            dynasm!(self.ectx.ops
                ; .arch x64
                ; mov [r15 + CTX_INPUT_PTR as i32], r12
            );
            #[cfg(not(windows))]
            dynasm!(self.ectx.ops ; .arch x64 ; lea rdi, [r14] ; mov rsi, r15);
            #[cfg(windows)]
            dynasm!(self.ectx.ops ; .arch x64 ; lea rcx, [r14] ; mov rdx, r15);

            for (i, &arg) in args.iter().enumerate() {
                let off = self.vreg_off(arg) as i32;
                #[cfg(not(windows))]
                match i {
                    0 => dynasm!(self.ectx.ops ; .arch x64 ; mov rdx, [rsp + off]),
                    1 => dynasm!(self.ectx.ops ; .arch x64 ; mov rcx, [rsp + off]),
                    2 => dynasm!(self.ectx.ops ; .arch x64 ; mov r8, [rsp + off]),
                    3 => dynasm!(self.ectx.ops ; .arch x64 ; mov r9, [rsp + off]),
                    _ => unreachable!(),
                }
                #[cfg(windows)]
                match i {
                    0 => dynasm!(self.ectx.ops ; .arch x64 ; mov r8, [rsp + off]),
                    1 => dynasm!(self.ectx.ops ; .arch x64 ; mov r9, [rsp + off]),
                    _ => unreachable!(),
                }
            }

            dynasm!(self.ectx.ops ; .arch x64 ; call =>label);
            dynasm!(self.ectx.ops
                ; .arch x64
                ; mov r12, [r15 + CTX_INPUT_PTR as i32]
                ; mov r10d, [r15 + CTX_ERROR_CODE as i32]
                ; test r10d, r10d
                ; jnz =>error_exit
            );

            if let Some(&dst) = results.first() {
                let off = self.vreg_off(dst) as i32;
                dynasm!(self.ectx.ops ; .arch x64 ; mov [rsp + off], rax);
            }
            if let Some(&dst) = results.get(1) {
                let off = self.vreg_off(dst) as i32;
                dynasm!(self.ectx.ops ; .arch x64 ; mov [rsp + off], rdx);
            }
        }

        fn run(mut self, ir: &LinearIr) -> LinearBackendResult {
            for op in &ir.ops {
                match op {
                    LinearOp::FuncStart {
                        lambda_id,
                        data_args,
                        data_results,
                        ..
                    } => {
                        let label = self.lambda_labels[lambda_id.index() as usize];
                        self.ectx.bind_label(label);
                        let (entry_offset, error_exit) = self.ectx.begin_func();
                        if lambda_id.index() == 0 {
                            self.entry = Some(entry_offset);
                        }
                        self.current_func = Some(FunctionCtx {
                            error_exit,
                            data_results: data_results.clone(),
                        });
                        self.const_vregs.fill(None);
                        self.x9_cached_vreg = None;
                        self.emit_store_incoming_lambda_args(data_args);
                    }
                    LinearOp::FuncEnd => {
                        let func = self
                            .current_func
                            .take()
                            .expect("FuncEnd without active function");
                        self.emit_load_lambda_results_to_ret_regs(&func.data_results);
                        self.ectx.end_func(func.error_exit);
                    }
                    LinearOp::Label(label) => self.ectx.bind_label(self.label(*label)),
                    LinearOp::Branch(target) => self.ectx.emit_branch(self.label(*target)),
                    LinearOp::BranchIf { cond, target } => {
                        self.emit_branch_if(*cond, self.label(*target), false);
                    }
                    LinearOp::BranchIfZero { cond, target } => {
                        self.emit_branch_if(*cond, self.label(*target), true);
                    }
                    LinearOp::JumpTable {
                        predicate,
                        labels,
                        default,
                    } => {
                        self.emit_jump_table(*predicate, labels, *default);
                    }

                    LinearOp::Const { dst, value } => {
                        self.ectx
                            .emit_store_imm64_to_stack(self.vreg_off(*dst), *value);
                    }
                    LinearOp::Copy { dst, src } => {
                        self.emit_recipe_ops(vec![
                            Op::LoadFromStack {
                                dst: Slot::A,
                                sp_offset: self.vreg_off(*src),
                                width: crate::recipe::Width::W8,
                            },
                            Op::StoreToStack {
                                src: Slot::A,
                                sp_offset: self.vreg_off(*dst),
                                width: crate::recipe::Width::W8,
                            },
                        ]);
                    }
                    LinearOp::BinOp { op, dst, lhs, rhs } => self.emit_binop(*op, *dst, *lhs, *rhs),
                    LinearOp::UnaryOp { op, dst, src } => self.emit_unary(*op, *dst, *src),

                    LinearOp::BoundsCheck { count } => {
                        self.emit_recipe_ops(vec![Op::BoundsCheck { count: *count }]);
                    }
                    LinearOp::ReadBytes { dst, count } => self.emit_read_bytes(*dst, *count),
                    LinearOp::PeekByte { dst } => self.emit_peek_byte(*dst),
                    LinearOp::AdvanceCursor { count } => self.ectx.emit_advance_cursor_by(*count),
                    LinearOp::AdvanceCursorBy { src } => {
                        self.emit_recipe_ops(vec![
                            Op::LoadFromStack {
                                dst: Slot::A,
                                sp_offset: self.vreg_off(*src),
                                width: crate::recipe::Width::W8,
                            },
                            Op::AdvanceCursorBySlot { slot: Slot::A },
                        ]);
                    }
                    LinearOp::SaveCursor { dst } => {
                        self.ectx.emit_save_cursor_to_stack(self.vreg_off(*dst));
                    }
                    LinearOp::RestoreCursor { src } => {
                        self.ectx.emit_restore_input_ptr(self.vreg_off(*src));
                    }

                    LinearOp::WriteToField { src, offset, width } => {
                        self.emit_write_to_field(*src, *offset, *width);
                    }
                    LinearOp::ReadFromField { dst, offset, width } => {
                        self.emit_read_from_field(*dst, *offset, *width);
                    }
                    LinearOp::SaveOutPtr { dst } => {
                        self.emit_save_out_ptr(*dst);
                    }
                    LinearOp::SetOutPtr { src } => {
                        self.emit_set_out_ptr(*src);
                    }
                    LinearOp::SlotAddr { dst, slot } => {
                        self.emit_slot_addr(*dst, *slot);
                    }
                    LinearOp::WriteToSlot { slot, src } => {
                        self.emit_recipe_ops(vec![
                            Op::LoadFromStack {
                                dst: Slot::A,
                                sp_offset: self.vreg_off(*src),
                                width: crate::recipe::Width::W8,
                            },
                            Op::StoreToStack {
                                src: Slot::A,
                                sp_offset: self.slot_off(*slot),
                                width: crate::recipe::Width::W8,
                            },
                        ]);
                    }
                    LinearOp::ReadFromSlot { dst, slot } => {
                        self.emit_recipe_ops(vec![
                            Op::LoadFromStack {
                                dst: Slot::A,
                                sp_offset: self.slot_off(*slot),
                                width: crate::recipe::Width::W8,
                            },
                            Op::StoreToStack {
                                src: Slot::A,
                                sp_offset: self.vreg_off(*dst),
                                width: crate::recipe::Width::W8,
                            },
                        ]);
                    }

                    LinearOp::CallIntrinsic {
                        func,
                        args,
                        dst,
                        field_offset,
                    } => {
                        self.emit_call_intrinsic(*func, args, *dst, *field_offset);
                    }
                    LinearOp::CallPure { .. } => {
                        panic!("unsupported CallPure in linear backend adapter");
                    }

                    LinearOp::ErrorExit { code } => {
                        self.ectx.emit_error(*code);
                    }

                    LinearOp::SimdStringScan { .. } | LinearOp::SimdWhitespaceSkip => {
                        panic!("unsupported SIMD op in linear backend adapter");
                    }

                    LinearOp::CallLambda {
                        target,
                        args,
                        results,
                    } => {
                        let label = self.lambda_labels[target.index() as usize];
                        self.emit_call_lambda(label, args, results);
                    }
                }
            }

            if self.current_func.is_some() {
                panic!("unterminated function: missing FuncEnd");
            }

            let entry = self.entry.expect("missing root FuncStart for lambda 0");
            let buf = self.ectx.finalize();
            LinearBackendResult { buf, entry }
        }
    }

    Lowerer::new(ir).run(ir)
}

#[cfg(target_arch = "aarch64")]
fn compile_linear_ir_aarch64(ir: &LinearIr) -> LinearBackendResult {
    use dynasmrt::{DynamicLabel, DynasmApi, DynasmLabelApi, dynasm};

    use crate::arch::{BASE_FRAME, EmitCtx};
    use crate::ir::Width;
    use crate::linearize::{BinOpKind, LabelId, LinearOp, UnaryOpKind};
    use crate::recipe::{Op, Recipe};

    struct FunctionCtx {
        error_exit: DynamicLabel,
        data_results: Vec<crate::ir::VReg>,
    }

    struct Lowerer {
        ectx: EmitCtx,
        labels: Vec<DynamicLabel>,
        lambda_labels: Vec<DynamicLabel>,
        slot_base: u32,
        vreg_base: u32,
        entry: Option<AssemblyOffset>,
        current_func: Option<FunctionCtx>,
        const_vregs: Vec<Option<u64>>,
        vreg_regs: Vec<Option<AllocReg>>,
        reg_vregs: [Option<crate::ir::VReg>; 5],
        reg_dirty: [bool; 5],
        use_positions: Vec<Vec<usize>>,
        use_cursor: Vec<usize>,
    }

    #[derive(Clone, Copy)]
    enum IntrinsicArg {
        VReg(crate::ir::VReg),
        OutField(u32),
        OutStack(u32),
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    enum AllocReg {
        X11,
        X12,
        X13,
        X14,
        X15,
    }

    impl AllocReg {
        const ALL: [AllocReg; 5] = [
            AllocReg::X11,
            AllocReg::X12,
            AllocReg::X13,
            AllocReg::X14,
            AllocReg::X15,
        ];

        const fn index(self) -> usize {
            match self {
                AllocReg::X11 => 0,
                AllocReg::X12 => 1,
                AllocReg::X13 => 2,
                AllocReg::X14 => 3,
                AllocReg::X15 => 4,
            }
        }
    }

    impl Lowerer {
        fn push_op_uses(op: &LinearOp, out: &mut Vec<crate::ir::VReg>) {
            match op {
                LinearOp::BinOp { lhs, rhs, .. } => {
                    out.push(*lhs);
                    out.push(*rhs);
                }
                LinearOp::UnaryOp { src, .. }
                | LinearOp::Copy { src, .. }
                | LinearOp::AdvanceCursorBy { src }
                | LinearOp::RestoreCursor { src }
                | LinearOp::WriteToField { src, .. }
                | LinearOp::SetOutPtr { src }
                | LinearOp::WriteToSlot { src, .. }
                | LinearOp::BranchIf { cond: src, .. }
                | LinearOp::BranchIfZero { cond: src, .. } => out.push(*src),
                LinearOp::JumpTable { predicate, .. } => out.push(*predicate),
                LinearOp::CallIntrinsic { args, .. }
                | LinearOp::CallPure { args, .. }
                | LinearOp::CallLambda { args, .. } => out.extend(args.iter().copied()),
                LinearOp::SimdStringScan { pos, kind } => {
                    out.push(*pos);
                    out.push(*kind);
                }
                LinearOp::Const { .. }
                | LinearOp::BoundsCheck { .. }
                | LinearOp::ReadBytes { .. }
                | LinearOp::PeekByte { .. }
                | LinearOp::AdvanceCursor { .. }
                | LinearOp::SaveCursor { .. }
                | LinearOp::ReadFromField { .. }
                | LinearOp::SaveOutPtr { .. }
                | LinearOp::SlotAddr { .. }
                | LinearOp::ReadFromSlot { .. }
                | LinearOp::Label(_)
                | LinearOp::Branch(_)
                | LinearOp::ErrorExit { .. }
                | LinearOp::SimdWhitespaceSkip
                | LinearOp::FuncStart { .. }
                | LinearOp::FuncEnd => {}
            }
        }

        fn new(ir: &LinearIr) -> Self {
            let slot_base = BASE_FRAME;
            let slot_bytes = ir.slot_count * 8;
            let vreg_base = slot_base + slot_bytes;
            let vreg_bytes = ir.vreg_count * 8;
            let extra_stack = slot_bytes + vreg_bytes + 8;

            let mut ectx = EmitCtx::new(extra_stack);
            let labels: Vec<DynamicLabel> = (0..ir.label_count).map(|_| ectx.new_label()).collect();

            let mut lambda_max = 0usize;
            for op in &ir.ops {
                match op {
                    LinearOp::FuncStart { lambda_id, .. } => {
                        lambda_max = lambda_max.max(lambda_id.index() as usize);
                    }
                    LinearOp::CallLambda { target, .. } => {
                        lambda_max = lambda_max.max(target.index() as usize);
                    }
                    _ => {}
                }
            }
            let lambda_labels: Vec<DynamicLabel> =
                (0..=lambda_max).map(|_| ectx.new_label()).collect();

            let mut use_positions: Vec<Vec<usize>> = vec![Vec::new(); ir.vreg_count as usize];
            let mut used = Vec::new();
            for (op_index, op) in ir.ops.iter().enumerate() {
                used.clear();
                Self::push_op_uses(op, &mut used);
                for &v in &used {
                    use_positions[v.index()].push(op_index);
                }
            }

            Self {
                ectx,
                labels,
                lambda_labels,
                slot_base,
                vreg_base,
                entry: None,
                current_func: None,
                const_vregs: vec![None; ir.vreg_count as usize],
                vreg_regs: vec![None; ir.vreg_count as usize],
                reg_vregs: [None; 5],
                reg_dirty: [false; 5],
                use_positions,
                use_cursor: vec![0; ir.vreg_count as usize],
            }
        }

        fn vreg_off(&self, v: crate::ir::VReg) -> u32 {
            self.vreg_base + (v.index() as u32) * 8
        }

        fn slot_off(&self, s: crate::ir::SlotId) -> u32 {
            self.slot_base + (s.index() as u32) * 8
        }

        fn label(&self, label: LabelId) -> DynamicLabel {
            self.labels[label.index() as usize]
        }

        fn next_use(&self, v: crate::ir::VReg) -> Option<usize> {
            let idx = self.use_cursor[v.index()];
            self.use_positions[v.index()].get(idx).copied()
        }

        fn consume_use(&mut self, v: crate::ir::VReg, op_index: usize) {
            let cursor = &mut self.use_cursor[v.index()];
            let positions = &self.use_positions[v.index()];
            while *cursor < positions.len() && positions[*cursor] < op_index {
                *cursor += 1;
            }
            if *cursor < positions.len() && positions[*cursor] == op_index {
                *cursor += 1;
            }
        }

        fn consume_op_uses(&mut self, op_index: usize, op: &LinearOp) {
            let mut used = Vec::new();
            Self::push_op_uses(op, &mut used);
            for v in used {
                self.consume_use(v, op_index);
            }
        }

        fn emit_mov_x9_from_alloc(&mut self, reg: AllocReg) {
            match reg {
                AllocReg::X11 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x11),
                AllocReg::X12 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x12),
                AllocReg::X13 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x13),
                AllocReg::X14 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x14),
                AllocReg::X15 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x15),
            }
        }

        fn emit_mov_x10_from_alloc(&mut self, reg: AllocReg) {
            match reg {
                AllocReg::X11 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x11),
                AllocReg::X12 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x12),
                AllocReg::X13 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x13),
                AllocReg::X14 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x14),
                AllocReg::X15 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x15),
            }
        }

        fn emit_mov_alloc_from_x9(&mut self, reg: AllocReg) {
            match reg {
                AllocReg::X11 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x11, x9),
                AllocReg::X12 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x12, x9),
                AllocReg::X13 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x13, x9),
                AllocReg::X14 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x14, x9),
                AllocReg::X15 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x15, x9),
            }
        }

        fn spill_reg(&mut self, reg: AllocReg) {
            let idx = reg.index();
            if let Some(v) = self.reg_vregs[idx] {
                self.vreg_regs[v.index()] = None;
                self.reg_vregs[idx] = None;
                self.reg_dirty[idx] = false;
            }
        }

        // r[impl ir.regalloc.no-boundary-flush]
        fn flush_all_vregs(&mut self) {
            for &reg in &AllocReg::ALL {
                self.spill_reg(reg);
            }
            self.const_vregs.fill(None);
        }

        fn pick_victim_reg(&self) -> AllocReg {
            let mut best: Option<(AllocReg, bool, Option<usize>)> = None;
            for &reg in &AllocReg::ALL {
                let idx = reg.index();
                match self.reg_vregs[idx] {
                    None => return reg,
                    Some(v) => {
                        let next = self.next_use(v);
                        let clean = !self.reg_dirty[idx];
                        if best.is_none() {
                            best = Some((reg, clean, next));
                            continue;
                        }
                        let (_, best_clean, best_next) = best.expect("just set");
                        let better = match (next, best_next) {
                            (None, None) => clean && !best_clean,
                            (None, Some(_)) => true,
                            (Some(_), None) => false,
                            (Some(a), Some(b)) => a > b || (a == b && clean && !best_clean),
                        };
                        if better {
                            best = Some((reg, clean, next));
                        }
                    }
                }
            }
            best.expect("allocator must have at least one register").0
        }

        fn ensure_dst_reg(&mut self, dst: crate::ir::VReg) -> AllocReg {
            if let Some(reg) = self.vreg_regs[dst.index()] {
                return reg;
            }

            let reg = self.pick_victim_reg();
            self.spill_reg(reg);
            self.vreg_regs[dst.index()] = Some(reg);
            self.reg_vregs[reg.index()] = Some(dst);
            self.reg_dirty[reg.index()] = false;
            reg
        }

        fn emit_recipe_ops(&mut self, ops: Vec<Op>) {
            self.flush_all_vregs();
            self.ectx.emit_recipe(&Recipe {
                ops,
                label_count: 0,
            });
        }

        fn emit_load_vreg_x9(&mut self, v: crate::ir::VReg) {
            if let Some(reg) = self.vreg_regs[v.index()] {
                self.emit_mov_x9_from_alloc(reg);
            } else {
                let off = self.vreg_off(v);
                dynasm!(self.ectx.ops
                    ; .arch aarch64
                    ; ldr x9, [sp, #off]
                );
            }
        }

        fn emit_load_vreg_x10(&mut self, v: crate::ir::VReg) {
            if let Some(reg) = self.vreg_regs[v.index()] {
                self.emit_mov_x10_from_alloc(reg);
            } else {
                let off = self.vreg_off(v);
                dynasm!(self.ectx.ops
                    ; .arch aarch64
                    ; ldr x10, [sp, #off]
                );
            }
        }

        fn emit_store_x9_to_vreg(&mut self, v: crate::ir::VReg) {
            let off = self.vreg_off(v);
            dynasm!(self.ectx.ops ; .arch aarch64 ; str x9, [sp, #off]);
            let reg = self.ensure_dst_reg(v);
            self.emit_mov_alloc_from_x9(reg);
            self.reg_dirty[reg.index()] = false;
        }

        fn emit_load_u32_w10(&mut self, value: u32) {
            let lo = (value & 0xFFFF) as u32;
            let hi = ((value >> 16) & 0xFFFF) as u32;
            dynasm!(self.ectx.ops ; .arch aarch64 ; movz w10, #lo);
            if value > 0xFFFF {
                dynasm!(self.ectx.ops ; .arch aarch64 ; movk w10, #hi, LSL #16);
            }
        }

        fn emit_load_u64_x10(&mut self, value: u64) {
            let p0 = (value & 0xFFFF) as u32;
            let p1 = ((value >> 16) & 0xFFFF) as u32;
            let p2 = ((value >> 32) & 0xFFFF) as u32;
            let p3 = ((value >> 48) & 0xFFFF) as u32;
            dynasm!(self.ectx.ops ; .arch aarch64 ; movz x10, #p0);
            if p1 != 0 {
                dynasm!(self.ectx.ops ; .arch aarch64 ; movk x10, #p1, LSL #16);
            }
            if p2 != 0 {
                dynasm!(self.ectx.ops ; .arch aarch64 ; movk x10, #p2, LSL #32);
            }
            if p3 != 0 {
                dynasm!(self.ectx.ops ; .arch aarch64 ; movk x10, #p3, LSL #48);
            }
        }

        fn emit_load_u64_x9(&mut self, value: u64) {
            self.emit_load_u64_x10(value);
            dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x10);
        }

        fn const_of(&self, v: crate::ir::VReg) -> Option<u64> {
            self.const_vregs[v.index()]
        }

        fn set_const(&mut self, v: crate::ir::VReg, value: Option<u64>) {
            self.const_vregs[v.index()] = value;
        }

        fn emit_read_from_field(&mut self, dst: crate::ir::VReg, offset: u32, width: Width) {
            match width {
                Width::W1 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldrb w9, [x21, #offset]),
                Width::W2 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldrh w9, [x21, #offset]),
                Width::W4 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr w9, [x21, #offset]),
                Width::W8 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x9, [x21, #offset]),
            }
            self.emit_store_x9_to_vreg(dst);
            self.set_const(dst, None);
        }

        fn emit_write_to_field(&mut self, src: crate::ir::VReg, offset: u32, width: Width) {
            self.emit_load_vreg_x9(src);
            match width {
                Width::W1 => dynasm!(self.ectx.ops ; .arch aarch64 ; strb w9, [x21, #offset]),
                Width::W2 => dynasm!(self.ectx.ops ; .arch aarch64 ; strh w9, [x21, #offset]),
                Width::W4 => dynasm!(self.ectx.ops ; .arch aarch64 ; str w9, [x21, #offset]),
                Width::W8 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x9, [x21, #offset]),
            }
        }

        fn emit_save_out_ptr(&mut self, dst: crate::ir::VReg) {
            dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x21);
            self.emit_store_x9_to_vreg(dst);
            self.set_const(dst, None);
        }

        fn emit_set_out_ptr(&mut self, src: crate::ir::VReg) {
            self.emit_load_vreg_x9(src);
            dynasm!(self.ectx.ops ; .arch aarch64 ; mov x21, x9);
        }

        fn emit_slot_addr(&mut self, dst: crate::ir::VReg, slot: crate::ir::SlotId) {
            let slot_off = self.slot_off(slot);
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; add x9, sp, #slot_off
            );
            self.emit_store_x9_to_vreg(dst);
            self.set_const(dst, None);
        }

        fn emit_read_bytes(&mut self, dst: crate::ir::VReg, count: u32) {
            self.emit_recipe_ops(vec![Op::BoundsCheck { count }]);
            match count {
                1 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldrb w9, [x19]),
                2 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldrh w9, [x19]),
                4 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr w9, [x19]),
                8 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x9, [x19]),
                _ => panic!("unsupported ReadBytes count: {count}"),
            }
            self.emit_store_x9_to_vreg(dst);
            self.set_const(dst, None);
            self.ectx.emit_advance_cursor_by(count);
        }

        fn emit_peek_byte(&mut self, dst: crate::ir::VReg) {
            self.emit_recipe_ops(vec![Op::BoundsCheck { count: 1 }]);
            dynasm!(self.ectx.ops ; .arch aarch64 ; ldrb w9, [x19]);
            self.emit_store_x9_to_vreg(dst);
            self.set_const(dst, None);
        }

        fn emit_binop(
            &mut self,
            kind: BinOpKind,
            dst: crate::ir::VReg,
            lhs: crate::ir::VReg,
            rhs: crate::ir::VReg,
        ) {
            self.emit_load_vreg_x9(lhs);
            let rhs_const = self.const_of(rhs);
            match kind {
                BinOpKind::Add => {
                    if let Some(c) = rhs_const
                        && c <= 4095
                    {
                        dynasm!(self.ectx.ops ; .arch aarch64 ; add x9, x9, c as u32);
                    } else {
                        self.emit_load_vreg_x10(rhs);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; add x9, x9, x10);
                    }
                }
                BinOpKind::Sub => {
                    if let Some(c) = rhs_const
                        && c <= 4095
                    {
                        dynasm!(self.ectx.ops ; .arch aarch64 ; sub x9, x9, c as u32);
                    } else {
                        self.emit_load_vreg_x10(rhs);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; sub x9, x9, x10);
                    }
                }
                BinOpKind::And => {
                    if matches!(rhs_const, Some(0x7f | 0x7e | 0x80 | 0x1)) {
                        let c = rhs_const.expect("just matched Some");
                        dynasm!(self.ectx.ops ; .arch aarch64 ; and x9, x9, c);
                    } else {
                        self.emit_load_vreg_x10(rhs);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; and x9, x9, x10);
                    }
                }
                BinOpKind::Or => {
                    self.emit_load_vreg_x10(rhs);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; orr x9, x9, x10);
                }
                BinOpKind::Xor => {
                    self.emit_load_vreg_x10(rhs);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; eor x9, x9, x10);
                }
                BinOpKind::CmpNe => {
                    if let Some(c) = rhs_const
                        && c <= 4095
                    {
                        dynasm!(self.ectx.ops ; .arch aarch64 ; cmp x9, c as u32);
                    } else if let Some(c) = rhs_const {
                        self.emit_load_u64_x10(c);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; cmp x9, x10);
                    } else {
                        self.emit_load_vreg_x10(rhs);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; cmp x9, x10);
                    }
                    dynasm!(self.ectx.ops ; .arch aarch64 ; cset x9, ne);
                }
                BinOpKind::Shr => {
                    if let Some(c) = rhs_const
                        && c <= 63
                    {
                        dynasm!(self.ectx.ops ; .arch aarch64 ; lsr x9, x9, c as u32);
                    } else {
                        self.emit_load_vreg_x10(rhs);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; lsr x9, x9, x10);
                    }
                }
                BinOpKind::Shl => {
                    if let Some(c) = rhs_const
                        && c <= 63
                    {
                        dynasm!(self.ectx.ops ; .arch aarch64 ; lsl x9, x9, c as u32);
                    } else {
                        self.emit_load_vreg_x10(rhs);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; lsl x9, x9, x10);
                    }
                }
            }
            self.emit_store_x9_to_vreg(dst);
            self.set_const(dst, None);
        }

        fn emit_unary(&mut self, kind: UnaryOpKind, dst: crate::ir::VReg, src: crate::ir::VReg) {
            self.emit_load_vreg_x9(src);
            match kind {
                UnaryOpKind::ZigzagDecode { wide: true } => {
                    dynasm!(self.ectx.ops
                        ; .arch aarch64
                        ; lsr x10, x9, #1
                        ; and x16, x9, #1
                        ; neg x16, x16
                        ; eor x9, x10, x16
                    );
                }
                UnaryOpKind::ZigzagDecode { wide: false } => {
                    dynasm!(self.ectx.ops
                        ; .arch aarch64
                        ; lsr w10, w9, #1
                        ; and w16, w9, #1
                        ; neg w16, w16
                        ; eor w9, w10, w16
                    );
                }
                UnaryOpKind::SignExtend { from_width } => match from_width {
                    Width::W1 => dynasm!(self.ectx.ops ; .arch aarch64 ; sxtb x9, w9),
                    Width::W2 => dynasm!(self.ectx.ops ; .arch aarch64 ; sxth x9, w9),
                    Width::W4 => dynasm!(self.ectx.ops ; .arch aarch64 ; sxtw x9, w9),
                    Width::W8 => {}
                },
            }
            self.emit_store_x9_to_vreg(dst);
            self.set_const(dst, None);
        }

        fn emit_branch_if(&mut self, cond: crate::ir::VReg, target: DynamicLabel, invert: bool) {
            self.flush_all_vregs();
            self.emit_load_vreg_x9(cond);
            if invert {
                dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x9, =>target);
            } else {
                dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x9, =>target);
            }
        }

        fn emit_jump_table(
            &mut self,
            predicate: crate::ir::VReg,
            labels: &[LabelId],
            default: LabelId,
        ) {
            self.flush_all_vregs();
            self.emit_load_vreg_x9(predicate);
            for (index, label) in labels.iter().enumerate() {
                let target = self.label(*label);
                self.emit_load_u32_w10(index as u32);
                dynasm!(self.ectx.ops
                    ; .arch aarch64
                    ; cmp w9, w10
                    ; b.eq =>target
                );
            }
            self.ectx.emit_branch(self.label(default));
        }

        fn emit_load_intrinsic_arg_x1(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x1, [sp, #off]);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x1, x21, #offset);
                }
                IntrinsicArg::OutStack(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x1, sp, #offset);
                }
            }
        }

        fn emit_load_intrinsic_arg_x2(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x2, [sp, #off]);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x2, x21, #offset);
                }
                IntrinsicArg::OutStack(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x2, sp, #offset);
                }
            }
        }

        fn emit_load_intrinsic_arg_x3(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x3, [sp, #off]);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x3, x21, #offset);
                }
                IntrinsicArg::OutStack(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x3, sp, #offset);
                }
            }
        }

        fn emit_load_intrinsic_arg_x4(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x4, [sp, #off]);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x4, x21, #offset);
                }
                IntrinsicArg::OutStack(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x4, sp, #offset);
                }
            }
        }

        fn emit_load_intrinsic_arg_x5(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x5, [sp, #off]);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x5, x21, #offset);
                }
                IntrinsicArg::OutStack(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x5, sp, #offset);
                }
            }
        }

        fn emit_load_intrinsic_arg_x6(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x6, [sp, #off]);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x6, x21, #offset);
                }
                IntrinsicArg::OutStack(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x6, sp, #offset);
                }
            }
        }

        fn emit_load_intrinsic_arg_x7(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg(v) => {
                    let off = self.vreg_off(v);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x7, [sp, #off]);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x7, x21, #offset);
                }
                IntrinsicArg::OutStack(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x7, sp, #offset);
                }
            }
        }

        fn emit_call_intrinsic_with_args(&mut self, fn_ptr: *const u8, args: &[IntrinsicArg]) {
            use crate::context::{CTX_ERROR_CODE, CTX_INPUT_PTR};

            if args.len() > 7 {
                panic!(
                    "unsupported CallIntrinsic arity in linear backend adapter: {} args (+ctx)",
                    args.len()
                );
            }

            let error_exit = self
                .current_func
                .as_ref()
                .expect("CallIntrinsic outside function")
                .error_exit;

            self.flush_all_vregs();

            // Flush cursor before call.
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; str x19, [x22, #CTX_INPUT_PTR]
                ; mov x0, x22
            );

            if let Some(arg) = args.first().copied() {
                self.emit_load_intrinsic_arg_x1(arg);
            }
            if let Some(arg) = args.get(1).copied() {
                self.emit_load_intrinsic_arg_x2(arg);
            }
            if let Some(arg) = args.get(2).copied() {
                self.emit_load_intrinsic_arg_x3(arg);
            }
            if let Some(arg) = args.get(3).copied() {
                self.emit_load_intrinsic_arg_x4(arg);
            }
            if let Some(arg) = args.get(4).copied() {
                self.emit_load_intrinsic_arg_x5(arg);
            }
            if let Some(arg) = args.get(5).copied() {
                self.emit_load_intrinsic_arg_x6(arg);
            }
            if let Some(arg) = args.get(6).copied() {
                self.emit_load_intrinsic_arg_x7(arg);
            }

            let ptr = fn_ptr as u64;
            let p0 = (ptr & 0xFFFF) as u32;
            let p1 = ((ptr >> 16) & 0xFFFF) as u32;
            let p2 = ((ptr >> 32) & 0xFFFF) as u32;
            let p3 = ((ptr >> 48) & 0xFFFF) as u32;
            dynasm!(self.ectx.ops ; .arch aarch64 ; movz x16, #p0);
            if p1 != 0 {
                dynasm!(self.ectx.ops ; .arch aarch64 ; movk x16, #p1, LSL #16);
            }
            if p2 != 0 {
                dynasm!(self.ectx.ops ; .arch aarch64 ; movk x16, #p2, LSL #32);
            }
            if p3 != 0 {
                dynasm!(self.ectx.ops ; .arch aarch64 ; movk x16, #p3, LSL #48);
            }
            dynasm!(self.ectx.ops ; .arch aarch64 ; blr x16);

            // Reload cursor and branch to error exit if needed.
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; ldr x19, [x22, #CTX_INPUT_PTR]
                ; ldr w9, [x22, #CTX_ERROR_CODE]
                ; cbnz w9, =>error_exit
            );
        }

        // r[impl ir.intrinsics]
        fn emit_call_intrinsic(
            &mut self,
            func: crate::ir::IntrinsicFn,
            args: &[crate::ir::VReg],
            dst: Option<crate::ir::VReg>,
            field_offset: u32,
        ) {
            let fn_ptr = func.0 as *const u8;
            match dst {
                Some(dst) => {
                    if args.is_empty() {
                        // Legacy stack-out intrinsic ABI: fn(ctx, &mut out)
                        let out_offset = self.vreg_off(dst);
                        self.ectx.emit_store_imm64_to_stack(out_offset, 0);
                        self.emit_call_intrinsic_with_args(
                            fn_ptr,
                            &[IntrinsicArg::OutStack(out_offset)],
                        );
                        dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x9, [sp, #out_offset]);
                        self.emit_store_x9_to_vreg(dst);
                    } else {
                        // Return-value intrinsic ABI: fn(ctx, args...) -> value
                        let call_args: Vec<IntrinsicArg> =
                            args.iter().copied().map(IntrinsicArg::VReg).collect();
                        self.emit_call_intrinsic_with_args(fn_ptr, &call_args);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x0);
                        self.emit_store_x9_to_vreg(dst);
                    }
                }
                None => {
                    let mut call_args: Vec<IntrinsicArg> =
                        args.iter().copied().map(IntrinsicArg::VReg).collect();
                    // Side-effect intrinsic ABI: fn(ctx, args..., out+field_offset)
                    call_args.push(IntrinsicArg::OutField(field_offset));
                    self.emit_call_intrinsic_with_args(fn_ptr, &call_args);
                }
            }
        }

        fn emit_store_incoming_lambda_args(&mut self, data_args: &[crate::ir::VReg]) {
            const MAX_LAMBDA_DATA_ARGS: usize = 6;
            if data_args.len() > MAX_LAMBDA_DATA_ARGS {
                panic!(
                    "aarch64 CallLambda supports at most {MAX_LAMBDA_DATA_ARGS} data args, got {}",
                    data_args.len()
                );
            }

            for (i, &arg) in data_args.iter().enumerate() {
                let off = self.vreg_off(arg);
                match i {
                    0 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x2, [sp, #off]),
                    1 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x3, [sp, #off]),
                    2 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x4, [sp, #off]),
                    3 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x5, [sp, #off]),
                    4 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x6, [sp, #off]),
                    5 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x7, [sp, #off]),
                    _ => unreachable!(),
                }
            }
        }

        fn emit_load_lambda_results_to_ret_regs(&mut self, data_results: &[crate::ir::VReg]) {
            if data_results.len() > 2 {
                panic!(
                    "aarch64 CallLambda supports at most 2 data results, got {}",
                    data_results.len()
                );
            }

            if let Some(&v) = data_results.first() {
                let off = self.vreg_off(v);
                dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x0, [sp, #off]);
            }
            if let Some(&v) = data_results.get(1) {
                let off = self.vreg_off(v);
                dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x1, [sp, #off]);
            }
        }

        fn emit_call_lambda(
            &mut self,
            label: DynamicLabel,
            args: &[crate::ir::VReg],
            results: &[crate::ir::VReg],
        ) {
            use crate::context::{CTX_ERROR_CODE, CTX_INPUT_PTR};

            const MAX_LAMBDA_DATA_ARGS: usize = 6;
            if args.len() > MAX_LAMBDA_DATA_ARGS {
                panic!(
                    "aarch64 CallLambda supports at most {MAX_LAMBDA_DATA_ARGS} data args, got {}",
                    args.len()
                );
            }
            if results.len() > 2 {
                panic!(
                    "aarch64 CallLambda supports at most 2 data results, got {}",
                    results.len()
                );
            }

            let error_exit = self
                .current_func
                .as_ref()
                .expect("CallLambda outside function")
                .error_exit;
            self.flush_all_vregs();
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; str x19, [x22, #CTX_INPUT_PTR]
            );
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; mov x0, x21
                ; mov x1, x22
            );

            for (i, &arg) in args.iter().enumerate() {
                let off = self.vreg_off(arg);
                match i {
                    0 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x2, [sp, #off]),
                    1 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x3, [sp, #off]),
                    2 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x4, [sp, #off]),
                    3 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x5, [sp, #off]),
                    4 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x6, [sp, #off]),
                    5 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x7, [sp, #off]),
                    _ => unreachable!(),
                }
            }

            dynasm!(self.ectx.ops ; .arch aarch64 ; bl =>label);
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; ldr x19, [x22, #CTX_INPUT_PTR]
                ; ldr w9, [x22, #CTX_ERROR_CODE]
                ; cbnz w9, =>error_exit
            );

            if let Some(&dst) = results.first() {
                dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x0);
                self.emit_store_x9_to_vreg(dst);
            }
            if let Some(&dst) = results.get(1) {
                dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x1);
                self.emit_store_x9_to_vreg(dst);
            }
        }

        fn run(mut self, ir: &LinearIr) -> LinearBackendResult {
            for (op_index, op) in ir.ops.iter().enumerate() {
                match op {
                    LinearOp::FuncStart {
                        lambda_id,
                        data_args,
                        data_results,
                        ..
                    } => {
                        self.flush_all_vregs();
                        let label = self.lambda_labels[lambda_id.index() as usize];
                        self.ectx.bind_label(label);
                        let (entry_offset, error_exit) = self.ectx.begin_func();
                        if lambda_id.index() == 0 {
                            self.entry = Some(entry_offset);
                        }
                        self.current_func = Some(FunctionCtx {
                            error_exit,
                            data_results: data_results.clone(),
                        });
                        self.emit_store_incoming_lambda_args(data_args);
                    }
                    LinearOp::FuncEnd => {
                        self.flush_all_vregs();
                        let func = self
                            .current_func
                            .take()
                            .expect("FuncEnd without active function");
                        self.emit_load_lambda_results_to_ret_regs(&func.data_results);
                        self.ectx.end_func(func.error_exit);
                    }
                    LinearOp::Label(label) => {
                        self.flush_all_vregs();
                        self.ectx.bind_label(self.label(*label));
                    }
                    LinearOp::Branch(target) => {
                        self.flush_all_vregs();
                        self.ectx.emit_branch(self.label(*target));
                    }
                    LinearOp::BranchIf { cond, target } => {
                        self.emit_branch_if(*cond, self.label(*target), false);
                    }
                    LinearOp::BranchIfZero { cond, target } => {
                        self.emit_branch_if(*cond, self.label(*target), true);
                    }
                    LinearOp::JumpTable {
                        predicate,
                        labels,
                        default,
                    } => {
                        self.emit_jump_table(*predicate, labels, *default);
                    }

                    LinearOp::Const { dst, value } => {
                        self.emit_load_u64_x9(*value);
                        self.emit_store_x9_to_vreg(*dst);
                        self.set_const(*dst, Some(*value));
                    }
                    LinearOp::Copy { dst, src } => {
                        self.emit_load_vreg_x9(*src);
                        self.emit_store_x9_to_vreg(*dst);
                        self.set_const(*dst, self.const_of(*src));
                    }
                    LinearOp::BinOp { op, dst, lhs, rhs } => self.emit_binop(*op, *dst, *lhs, *rhs),
                    LinearOp::UnaryOp { op, dst, src } => self.emit_unary(*op, *dst, *src),

                    LinearOp::BoundsCheck { count } => {
                        self.emit_recipe_ops(vec![Op::BoundsCheck { count: *count }]);
                    }
                    LinearOp::ReadBytes { dst, count } => self.emit_read_bytes(*dst, *count),
                    LinearOp::PeekByte { dst } => self.emit_peek_byte(*dst),
                    LinearOp::AdvanceCursor { count } => self.ectx.emit_advance_cursor_by(*count),
                    LinearOp::AdvanceCursorBy { src } => {
                        self.emit_load_vreg_x9(*src);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; add x19, x19, x9);
                    }
                    LinearOp::SaveCursor { dst } => {
                        dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x19);
                        self.emit_store_x9_to_vreg(*dst);
                        self.set_const(*dst, None);
                    }
                    LinearOp::RestoreCursor { src } => {
                        self.emit_load_vreg_x9(*src);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; mov x19, x9);
                    }

                    LinearOp::WriteToField { src, offset, width } => {
                        self.emit_write_to_field(*src, *offset, *width);
                    }
                    LinearOp::ReadFromField { dst, offset, width } => {
                        self.emit_read_from_field(*dst, *offset, *width);
                        self.set_const(*dst, None);
                    }
                    LinearOp::SaveOutPtr { dst } => {
                        self.emit_save_out_ptr(*dst);
                        self.set_const(*dst, None);
                    }
                    LinearOp::SetOutPtr { src } => {
                        self.emit_set_out_ptr(*src);
                    }
                    LinearOp::SlotAddr { dst, slot } => {
                        self.emit_slot_addr(*dst, *slot);
                        self.set_const(*dst, None);
                    }
                    LinearOp::WriteToSlot { slot, src } => {
                        self.emit_load_vreg_x9(*src);
                        let off = self.slot_off(*slot);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; str x9, [sp, #off]);
                    }
                    LinearOp::ReadFromSlot { dst, slot } => {
                        let off = self.slot_off(*slot);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x9, [sp, #off]);
                        self.emit_store_x9_to_vreg(*dst);
                        self.set_const(*dst, None);
                    }

                    LinearOp::CallIntrinsic {
                        func,
                        args,
                        dst,
                        field_offset,
                    } => {
                        self.emit_call_intrinsic(*func, args, *dst, *field_offset);
                        if let Some(dst) = dst {
                            self.set_const(*dst, None);
                        }
                    }
                    LinearOp::CallPure { .. } => {
                        panic!("unsupported CallPure in linear backend adapter");
                    }

                    LinearOp::ErrorExit { code } => {
                        self.flush_all_vregs();
                        self.ectx.emit_error(*code);
                    }

                    LinearOp::SimdStringScan { .. } | LinearOp::SimdWhitespaceSkip => {
                        panic!("unsupported SIMD op in linear backend adapter");
                    }

                    LinearOp::CallLambda {
                        target,
                        args,
                        results,
                    } => {
                        let label = self.lambda_labels[target.index() as usize];
                        self.emit_call_lambda(label, args, results);
                        for &r in results {
                            self.set_const(r, None);
                        }
                    }
                }
                self.consume_op_uses(op_index, op);
            }

            if self.current_func.is_some() {
                panic!("unterminated function: missing FuncEnd");
            }

            let entry = self.entry.expect("missing root FuncStart for lambda 0");
            let buf = self.ectx.finalize();
            LinearBackendResult { buf, entry }
        }
    }

    Lowerer::new(ir).run(ir)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler;
    use crate::context::{DeserContext, ErrorCode};
    use crate::ir::{IntrinsicFn, IrBuilder, IrOp, Width};
    use crate::linearize::linearize;
    use facet::Facet;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Facet, PartialEq)]
    struct ScalarVec {
        values: Vec<u32>,
    }

    fn run_u32_decoder(ir: &LinearIr, input: &[u8]) -> (u32, DeserContext) {
        let deser = compiler::compile_linear_ir_decoder(ir, false);
        let mut out = core::mem::MaybeUninit::<u32>::uninit();
        let mut ctx = DeserContext::from_bytes(input);
        unsafe {
            (deser.func())(out.as_mut_ptr() as *mut u8, &mut ctx);
            (out.assume_init(), ctx)
        }
    }

    fn run_u64_decoder(ir: &LinearIr, input: &[u8]) -> (u64, DeserContext) {
        let deser = compiler::compile_linear_ir_decoder(ir, false);
        let mut out = core::mem::MaybeUninit::<u64>::uninit();
        let mut ctx = DeserContext::from_bytes(input);
        unsafe {
            (deser.func())(out.as_mut_ptr() as *mut u8, &mut ctx);
            (out.assume_init(), ctx)
        }
    }

    #[test]
    fn linear_backend_reads_u32_from_cursor() {
        let mut builder = IrBuilder::new(<u32 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            rb.bounds_check(4);
            let v = rb.read_bytes(4);
            rb.write_to_field(v, 0, Width::W4);
            rb.set_results(&[]);
        }

        let mut func = builder.finish();
        let lin = linearize(&mut func);
        let (value, ctx) = run_u32_decoder(&lin, &[0x78, 0x56, 0x34, 0x12]);

        assert_eq!(ctx.error.code, 0);
        assert_eq!(value, 0x1234_5678);
    }

    #[test]
    fn linear_backend_call_intrinsic_stack_out() {
        let mut builder = IrBuilder::new(<u32 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            let v = rb
                .call_intrinsic(
                    IntrinsicFn(crate::intrinsics::fad_read_varint_u32 as *const () as usize),
                    &[],
                    0,
                    true,
                )
                .expect("varint read should produce output");
            rb.write_to_field(v, 0, Width::W4);
            rb.set_results(&[]);
        }

        let mut func = builder.finish();
        let lin = linearize(&mut func);
        let (value, ctx) = run_u32_decoder(&lin, &[0xac, 0x02]);

        assert_eq!(ctx.error.code, 0);
        assert_eq!(value, 300);
    }

    #[test]
    fn linear_backend_bounds_check_sets_eof() {
        let mut builder = IrBuilder::new(<u32 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            rb.bounds_check(4);
            let v = rb.read_bytes(4);
            rb.write_to_field(v, 0, Width::W4);
            rb.set_results(&[]);
        }

        let mut func = builder.finish();
        let lin = linearize(&mut func);
        let (_value, ctx) = run_u32_decoder(&lin, &[0x01, 0x02]);

        assert_eq!(ctx.error.code, ErrorCode::UnexpectedEof as u32);
    }

    #[test]
    fn linear_backend_two_way_gamma_branch() {
        let mut builder = IrBuilder::new(<u32 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            let pred = rb.const_val(1);
            rb.gamma(pred, &[], 2, |branch_idx, br| {
                let value = if branch_idx == 0 { 10 } else { 20 };
                let v = br.const_val(value);
                br.write_to_field(v, 0, Width::W4);
                br.set_results(&[]);
            });
            rb.set_results(&[]);
        }

        let mut func = builder.finish();
        let lin = linearize(&mut func);
        let (value, ctx) = run_u32_decoder(&lin, &[]);

        assert_eq!(ctx.error.code, 0);
        assert_eq!(value, 20);
    }

    #[test]
    fn linear_backend_jump_table_gamma_branch() {
        let mut builder = IrBuilder::new(<u32 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            let pred = rb.const_val(2);
            rb.gamma(pred, &[], 3, |branch_idx, br| {
                let value = match branch_idx {
                    0 => 111,
                    1 => 222,
                    2 => 333,
                    _ => unreachable!(),
                };
                let v = br.const_val(value);
                br.write_to_field(v, 0, Width::W4);
                br.set_results(&[]);
            });
            rb.set_results(&[]);
        }

        let mut func = builder.finish();
        let lin = linearize(&mut func);
        let (value, ctx) = run_u32_decoder(&lin, &[]);

        assert_eq!(ctx.error.code, 0);
        assert_eq!(value, 333);
    }

    #[test]
    fn linear_backend_call_intrinsic_with_args_return_value() {
        unsafe extern "C" fn add3(
            _ctx: *mut crate::context::DeserContext,
            a: u64,
            b: u64,
            c: u64,
        ) -> u64 {
            a + b + c
        }

        let mut builder = IrBuilder::new(<u64 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            let a = rb.const_val(11);
            let b = rb.const_val(7);
            let c = rb.const_val(5);
            let out = rb
                .call_intrinsic(IntrinsicFn(add3 as *const () as usize), &[a, b, c], 0, true)
                .expect("return-value intrinsic should produce output");
            rb.write_to_field(out, 0, Width::W8);
            rb.set_results(&[]);
        }

        let mut func = builder.finish();
        let lin = linearize(&mut func);
        let (value, ctx) = run_u64_decoder(&lin, &[]);

        assert_eq!(ctx.error.code, 0);
        assert_eq!(value, 23);
    }

    #[test]
    fn linear_backend_call_intrinsic_with_args_and_out_ptr() {
        unsafe extern "C" fn write_scaled_sum(
            _ctx: *mut crate::context::DeserContext,
            x: u64,
            y: u64,
            out: *mut u64,
        ) {
            unsafe { *out = x * 10 + y };
        }

        let mut builder = IrBuilder::new(<u64 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            let x = rb.const_val(9);
            let y = rb.const_val(4);
            rb.call_intrinsic(
                IntrinsicFn(write_scaled_sum as *const () as usize),
                &[x, y],
                0,
                false,
            );
            rb.set_results(&[]);
        }

        let mut func = builder.finish();
        let lin = linearize(&mut func);
        let (value, ctx) = run_u64_decoder(&lin, &[]);

        assert_eq!(ctx.error.code, 0);
        assert_eq!(value, 94);
    }

    #[test]
    fn linear_backend_call_lambda_with_data_args_and_results() {
        let mut builder = IrBuilder::new(<u64 as facet::Facet>::SHAPE);
        let child = builder.create_lambda_with_data_args(<u64 as facet::Facet>::SHAPE, 1);
        {
            let mut rb = builder.lambda_region(child);
            let arg = rb.region_args(1)[0];
            let one = rb.const_val(1);
            let sum = rb.binop(IrOp::Add, arg, one);
            rb.set_results(&[sum]);
        }
        {
            let mut rb = builder.root_region();
            let x = rb.const_val(41);
            let out = rb.apply(child, &[x], 1);
            rb.write_to_field(out[0], 0, Width::W8);
            rb.set_results(&[]);
        }

        let mut func = builder.finish();
        let lin = linearize(&mut func);
        let (value, ctx) = run_u64_decoder(&lin, &[]);

        assert_eq!(ctx.error.code, 0);
        assert_eq!(value, 42);
    }

    #[test]
    fn linear_backend_vec_u32_matches_legacy_and_serde() {
        let expected = ScalarVec {
            values: (0..2048).map(|i| i as u32).collect(),
        };
        let bytes = postcard::to_allocvec(&expected).expect("serialize vec");

        let legacy = crate::compile_decoder_legacy(ScalarVec::SHAPE, &crate::postcard::FadPostcard);
        let ir = crate::compile_decoder_via_ir(ScalarVec::SHAPE, &crate::postcard::FadPostcard);

        let legacy_out = crate::deserialize::<ScalarVec>(&legacy, &bytes).expect("legacy decode");
        let ir_out = crate::deserialize::<ScalarVec>(&ir, &bytes).expect("ir decode");
        let serde_out = postcard::from_bytes::<ScalarVec>(&bytes).expect("serde decode");

        assert_eq!(legacy_out, expected);
        assert_eq!(ir_out, expected);
        assert_eq!(serde_out, expected);
        assert_eq!(ir_out, legacy_out);
    }
}
