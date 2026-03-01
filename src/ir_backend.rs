use dynasmrt::AssemblyOffset;

use crate::linearize::LinearIr;

pub struct LinearBackendResult {
    pub buf: dynasmrt::ExecutableBuffer,
    pub entry: AssemblyOffset,
}

pub fn compile_linear_ir(ir: &LinearIr) -> LinearBackendResult {
    let alloc = crate::regalloc_engine::allocate_linear_ir(ir)
        .unwrap_or_else(|err| panic!("regalloc2 allocation failed: {err}"));
    compile_linear_ir_with_alloc(ir, &alloc)
}

pub fn compile_linear_ir_with_alloc(
    ir: &LinearIr,
    alloc: &crate::regalloc_engine::AllocatedProgram,
) -> LinearBackendResult {
    let max_spillslots = alloc
        .functions
        .iter()
        .map(|f| f.num_spillslots)
        .max()
        .unwrap_or(0);

    #[cfg(target_arch = "x86_64")]
    {
        compile_linear_ir_x64(ir, max_spillslots)
    }

    #[cfg(target_arch = "aarch64")]
    {
        compile_linear_ir_aarch64(ir, max_spillslots, alloc)
    }
}

#[cfg(target_arch = "x86_64")]
fn compile_linear_ir_x64(ir: &LinearIr, _max_spillslots: usize) -> LinearBackendResult {
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
fn compile_linear_ir_aarch64(
    ir: &LinearIr,
    max_spillslots: usize,
    alloc: &crate::regalloc_engine::AllocatedProgram,
) -> LinearBackendResult {
    use dynasmrt::{DynamicLabel, DynasmApi, DynasmLabelApi, dynasm};
    use regalloc2::{Allocation, Edit, InstPosition};
    use std::collections::HashMap;

    use crate::arch::{BASE_FRAME, EmitCtx};
    use crate::ir::Width;
    use crate::linearize::{BinOpKind, LabelId, LinearOp, UnaryOpKind};
    use crate::recipe::{Op, Recipe};

    struct FunctionCtx {
        error_exit: DynamicLabel,
        data_results: Vec<crate::ir::VReg>,
        lambda_id: crate::ir::LambdaId,
    }

    #[derive(Default)]
    struct LambdaEditMap {
        before: HashMap<usize, Vec<(Allocation, Allocation)>>,
        after: HashMap<usize, Vec<(Allocation, Allocation)>>,
    }

    #[derive(Default)]
    struct LambdaEdgeEditMap {
        before: HashMap<(usize, usize), Vec<(Allocation, Allocation)>>,
        after: HashMap<(usize, usize), Vec<(Allocation, Allocation)>>,
    }

    struct EdgeTrampoline {
        label: DynamicLabel,
        target: DynamicLabel,
        moves: Vec<(Allocation, Allocation)>,
    }

    struct Lowerer {
        ectx: EmitCtx,
        labels: Vec<DynamicLabel>,
        lambda_labels: Vec<DynamicLabel>,
        slot_base: u32,
        spill_base: u32,
        entry: Option<AssemblyOffset>,
        current_func: Option<FunctionCtx>,
        const_vregs: Vec<Option<u64>>,
        edits_by_lambda: HashMap<u32, LambdaEditMap>,
        edge_edits_by_lambda: HashMap<u32, LambdaEdgeEditMap>,
        allocs_by_lambda: HashMap<u32, HashMap<usize, Vec<Allocation>>>,
        entry_arg_allocs_by_lambda: HashMap<u32, Vec<Vec<Allocation>>>,
        return_result_allocs_by_lambda: HashMap<u32, Vec<Allocation>>,
        edge_trampoline_labels: HashMap<(u32, usize, usize), DynamicLabel>,
        edge_trampolines: Vec<EdgeTrampoline>,
        current_inst_allocs: Option<Vec<Allocation>>,
        current_lambda_linear_op_index: usize,
    }

    #[derive(Clone, Copy)]
    enum IntrinsicArg {
        VReg {
            vreg: crate::ir::VReg,
            operand_index: usize,
        },
        OutField(u32),
    }

    impl Lowerer {
        fn new(
            ir: &LinearIr,
            max_spillslots: usize,
            alloc: &crate::regalloc_engine::AllocatedProgram,
        ) -> Self {
            let slot_base = BASE_FRAME;
            let slot_bytes = ir.slot_count * 8;
            let spill_base = slot_base + slot_bytes;
            let spill_bytes = max_spillslots as u32 * 8;
            let extra_stack = slot_bytes + spill_bytes + 8;

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

            let mut edits_by_lambda = HashMap::<u32, LambdaEditMap>::new();
            let mut edge_edits_by_lambda = HashMap::<u32, LambdaEdgeEditMap>::new();
            let mut allocs_by_lambda = HashMap::<u32, HashMap<usize, Vec<Allocation>>>::new();
            let mut return_result_allocs_by_lambda = HashMap::<u32, Vec<Allocation>>::new();
            for func in &alloc.functions {
                let lambda_id = func.lambda_id.index() as u32;
                let lambda_entry = edits_by_lambda.entry(lambda_id).or_default();
                let lambda_edge_entry = edge_edits_by_lambda.entry(lambda_id).or_default();
                let allocs_entry = allocs_by_lambda.entry(lambda_id).or_default();
                return_result_allocs_by_lambda
                    .entry(lambda_id)
                    .or_insert_with(|| func.return_result_allocs.clone());
                for (prog_point, edit) in &func.edits {
                    let Some(Some(linear_op_index)) =
                        func.inst_linear_op_indices.get(prog_point.inst().index())
                    else {
                        continue;
                    };
                    let Edit::Move { from, to } = edit;
                    let bucket = match prog_point.pos() {
                        InstPosition::Before => &mut lambda_entry.before,
                        InstPosition::After => &mut lambda_entry.after,
                    };
                    bucket
                        .entry(*linear_op_index)
                        .or_default()
                        .push((*from, *to));
                }
                for edge_edit in &func.edge_edits {
                    let key = (edge_edit.from_linear_op_index, edge_edit.succ_index);
                    let bucket = match edge_edit.pos {
                        InstPosition::Before => &mut lambda_edge_entry.before,
                        InstPosition::After => &mut lambda_edge_entry.after,
                    };
                    bucket
                        .entry(key)
                        .or_default()
                        .push((edge_edit.from, edge_edit.to));
                }
                for (inst_index, maybe_linear_op_index) in
                    func.inst_linear_op_indices.iter().copied().enumerate()
                {
                    let Some(linear_op_index) = maybe_linear_op_index else {
                        continue;
                    };
                    let Some(inst_allocs) = func.inst_allocs.get(inst_index) else {
                        continue;
                    };
                    allocs_entry.insert(linear_op_index, inst_allocs.clone());
                }
            }
            let mut entry_arg_allocs_by_lambda = HashMap::<u32, Vec<Vec<Allocation>>>::new();
            let ra_program = crate::regalloc_mir::lower_linear_ir(ir);
            for ra_func in &ra_program.funcs {
                let lambda_id = ra_func.lambda_id.index() as u32;
                let mut per_arg_allocs = vec![Vec::new(); ra_func.data_args.len()];
                if let Some(allocs_for_lambda) = allocs_by_lambda.get(&lambda_id) {
                    let mut arg_pos_by_vreg = HashMap::<usize, usize>::new();
                    for (arg_pos, vreg) in ra_func.data_args.iter().copied().enumerate() {
                        arg_pos_by_vreg.insert(vreg.index(), arg_pos);
                    }

                    for block in &ra_func.blocks {
                        for inst in &block.insts {
                            let Some(inst_allocs) = allocs_for_lambda.get(&inst.linear_op_index)
                            else {
                                continue;
                            };
                            for (operand_index, operand) in inst.operands.iter().enumerate() {
                                if operand.kind != crate::regalloc_mir::OperandKind::Use {
                                    continue;
                                }
                                let Some(&arg_pos) = arg_pos_by_vreg.get(&operand.vreg.index())
                                else {
                                    continue;
                                };
                                let Some(&alloc) = inst_allocs.get(operand_index) else {
                                    continue;
                                };
                                if alloc.is_none() {
                                    continue;
                                }
                                if !per_arg_allocs[arg_pos].contains(&alloc) {
                                    per_arg_allocs[arg_pos].push(alloc);
                                }
                            }
                        }

                        if let Some(term_linear_op_index) = block.term_linear_op_index
                            && let Some(term_allocs) = allocs_for_lambda.get(&term_linear_op_index)
                        {
                            let term_uses: Vec<crate::ir::VReg> = match &block.term {
                                crate::regalloc_mir::RaTerminator::BranchIf { cond, .. }
                                | crate::regalloc_mir::RaTerminator::BranchIfZero {
                                    cond, ..
                                } => vec![*cond],
                                crate::regalloc_mir::RaTerminator::JumpTable {
                                    predicate, ..
                                } => vec![*predicate],
                                crate::regalloc_mir::RaTerminator::Return
                                | crate::regalloc_mir::RaTerminator::ErrorExit { .. }
                                | crate::regalloc_mir::RaTerminator::Branch { .. } => Vec::new(),
                            };
                            for (operand_index, vreg) in term_uses.into_iter().enumerate() {
                                let Some(&arg_pos) = arg_pos_by_vreg.get(&vreg.index()) else {
                                    continue;
                                };
                                let Some(&alloc) = term_allocs.get(operand_index) else {
                                    continue;
                                };
                                if alloc.is_none() {
                                    continue;
                                }
                                if !per_arg_allocs[arg_pos].contains(&alloc) {
                                    per_arg_allocs[arg_pos].push(alloc);
                                }
                            }
                        }
                    }
                }
                entry_arg_allocs_by_lambda.insert(lambda_id, per_arg_allocs);
            }

            Self {
                ectx,
                labels,
                lambda_labels,
                slot_base,
                spill_base,
                entry: None,
                current_func: None,
                const_vregs: vec![None; ir.vreg_count as usize],
                edits_by_lambda,
                edge_edits_by_lambda,
                allocs_by_lambda,
                entry_arg_allocs_by_lambda,
                return_result_allocs_by_lambda,
                edge_trampoline_labels: HashMap::new(),
                edge_trampolines: Vec::new(),
                current_inst_allocs: None,
                current_lambda_linear_op_index: 0,
            }
        }

        fn slot_off(&self, s: crate::ir::SlotId) -> u32 {
            self.slot_base + (s.index() as u32) * 8
        }

        fn label(&self, label: LabelId) -> DynamicLabel {
            self.labels[label.index() as usize]
        }

        fn emit_mov_x9_from_preg(&mut self, preg: regalloc2::PReg) -> bool {
            if preg.class() != regalloc2::RegClass::Int {
                return false;
            }
            match preg.hw_enc() {
                0 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x0),
                1 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x1),
                2 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x2),
                3 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x3),
                4 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x4),
                5 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x5),
                6 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x6),
                7 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x7),
                8 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x8),
                9 => {}
                10 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x10),
                11 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x11),
                12 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x12),
                13 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x13),
                14 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x14),
                15 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x15),
                16 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x16),
                17 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x17),
                _ => return false,
            }
            true
        }

        fn emit_mov_preg_from_x9(&mut self, preg: regalloc2::PReg) -> bool {
            if preg.class() != regalloc2::RegClass::Int {
                return false;
            }
            match preg.hw_enc() {
                0 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x0, x9),
                1 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x1, x9),
                2 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x2, x9),
                3 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x3, x9),
                4 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x4, x9),
                5 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x5, x9),
                6 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x6, x9),
                7 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x7, x9),
                8 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x8, x9),
                9 => {}
                10 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x9),
                11 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x11, x9),
                12 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x12, x9),
                13 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x13, x9),
                14 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x14, x9),
                15 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x15, x9),
                16 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x16, x9),
                17 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x17, x9),
                _ => return false,
            }
            true
        }

        fn emit_store_stack_from_preg(&mut self, preg: regalloc2::PReg, off: u32) -> bool {
            if preg.class() != regalloc2::RegClass::Int {
                return false;
            }
            match preg.hw_enc() {
                0 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x0, [sp, #off]),
                1 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x1, [sp, #off]),
                2 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x2, [sp, #off]),
                3 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x3, [sp, #off]),
                4 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x4, [sp, #off]),
                5 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x5, [sp, #off]),
                6 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x6, [sp, #off]),
                7 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x7, [sp, #off]),
                8 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x8, [sp, #off]),
                9 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x9, [sp, #off]),
                10 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x10, [sp, #off]),
                11 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x11, [sp, #off]),
                12 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x12, [sp, #off]),
                13 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x13, [sp, #off]),
                14 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x14, [sp, #off]),
                15 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x15, [sp, #off]),
                16 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x16, [sp, #off]),
                17 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x17, [sp, #off]),
                _ => return false,
            }
            true
        }

        fn emit_load_preg_from_stack(&mut self, preg: regalloc2::PReg, off: u32) -> bool {
            if preg.class() != regalloc2::RegClass::Int {
                return false;
            }
            match preg.hw_enc() {
                0 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x0, [sp, #off]),
                1 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x1, [sp, #off]),
                2 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x2, [sp, #off]),
                3 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x3, [sp, #off]),
                4 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x4, [sp, #off]),
                5 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x5, [sp, #off]),
                6 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x6, [sp, #off]),
                7 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x7, [sp, #off]),
                8 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x8, [sp, #off]),
                9 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x9, [sp, #off]),
                10 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x10, [sp, #off]),
                11 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x11, [sp, #off]),
                12 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x12, [sp, #off]),
                13 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x13, [sp, #off]),
                14 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x14, [sp, #off]),
                15 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x15, [sp, #off]),
                16 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x16, [sp, #off]),
                17 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x17, [sp, #off]),
                _ => return false,
            }
            true
        }

        fn spill_off(&self, slot: regalloc2::SpillSlot) -> u32 {
            self.spill_base + (slot.index() as u32) * 8
        }

        fn emit_edit_move(&mut self, from: Allocation, to: Allocation) {
            if from == to || from.is_none() || to.is_none() {
                return;
            }

            match (from.as_reg(), from.as_stack(), to.as_reg(), to.as_stack()) {
                (Some(from_reg), None, Some(to_reg), None) => {
                    if from_reg == to_reg {
                        return;
                    }
                    if from_reg.hw_enc() == 9 {
                        let _ = self.emit_mov_preg_from_x9(to_reg);
                        return;
                    }
                    if to_reg.hw_enc() == 9 {
                        let _ = self.emit_mov_x9_from_preg(from_reg);
                        return;
                    }
                    if !self.emit_mov_x9_from_preg(from_reg) {
                        return;
                    }
                    let _ = self.emit_mov_preg_from_x9(to_reg);
                }
                (Some(from_reg), None, None, Some(to_stack)) => {
                    let off = self.spill_off(to_stack);
                    if self.emit_store_stack_from_preg(from_reg, off) {
                        return;
                    }
                    if !self.emit_mov_x9_from_preg(from_reg) {
                        return;
                    }
                    dynasm!(self.ectx.ops ; .arch aarch64 ; str x9, [sp, #off]);
                }
                (None, Some(from_stack), Some(to_reg), None) => {
                    let off = self.spill_off(from_stack);
                    if self.emit_load_preg_from_stack(to_reg, off) {
                        return;
                    }
                    dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x9, [sp, #off]);
                    let _ = self.emit_mov_preg_from_x9(to_reg);
                }
                (None, Some(from_stack), None, Some(to_stack)) => {
                    if from_stack == to_stack {
                        return;
                    }
                    let from_off = self.spill_off(from_stack);
                    let to_off = self.spill_off(to_stack);
                    dynasm!(self.ectx.ops
                        ; .arch aarch64
                        ; ldr x9, [sp, #from_off]
                        ; str x9, [sp, #to_off]
                    );
                }
                _ => {}
            }
        }

        fn apply_regalloc_edits(&mut self, linear_op_index: usize, pos: InstPosition) {
            let lambda_id = match self.current_func.as_ref() {
                Some(func) => func.lambda_id.index() as u32,
                None => return,
            };

            let edits = self
                .edits_by_lambda
                .get(&lambda_id)
                .and_then(|by_lambda| match pos {
                    InstPosition::Before => by_lambda.before.get(&linear_op_index),
                    InstPosition::After => by_lambda.after.get(&linear_op_index),
                })
                .cloned()
                .unwrap_or_default();

            if edits.is_empty() {
                return;
            }

            self.flush_all_vregs();
            for (from, to) in edits {
                self.emit_edit_move(from, to);
            }
        }

        fn has_edge_edits(&self, linear_op_index: usize, succ_index: usize) -> bool {
            let Some(lambda_id) = self
                .current_func
                .as_ref()
                .map(|f| f.lambda_id.index() as u32)
            else {
                return false;
            };
            let Some(by_lambda) = self.edge_edits_by_lambda.get(&lambda_id) else {
                return false;
            };
            let key = (linear_op_index, succ_index);
            by_lambda.before.contains_key(&key) || by_lambda.after.contains_key(&key)
        }

        fn edge_target_label(
            &mut self,
            linear_op_index: usize,
            succ_index: usize,
            actual_target: DynamicLabel,
        ) -> DynamicLabel {
            let Some(lambda_id) = self
                .current_func
                .as_ref()
                .map(|f| f.lambda_id.index() as u32)
            else {
                return actual_target;
            };
            let Some(by_lambda) = self.edge_edits_by_lambda.get(&lambda_id) else {
                return actual_target;
            };
            let key = (linear_op_index, succ_index);
            let has_edits =
                by_lambda.before.contains_key(&key) || by_lambda.after.contains_key(&key);
            if !has_edits {
                return actual_target;
            }

            let cache_key = (lambda_id, linear_op_index, succ_index);
            if let Some(label) = self.edge_trampoline_labels.get(&cache_key).copied() {
                return label;
            }

            let mut moves = Vec::new();
            if let Some(before) = by_lambda.before.get(&key) {
                moves.extend(before.iter().copied());
            }
            if let Some(after) = by_lambda.after.get(&key) {
                // Edge blocks only contain an unconditional branch; both before/after edits
                // must execute before branching to the real CFG target.
                moves.extend(after.iter().copied());
            }

            let label = self.ectx.new_label();
            self.edge_trampoline_labels.insert(cache_key, label);
            self.edge_trampolines.push(EdgeTrampoline {
                label,
                target: actual_target,
                moves,
            });
            label
        }

        fn emit_edge_trampolines(&mut self) {
            let trampolines = std::mem::take(&mut self.edge_trampolines);
            for trampoline in trampolines {
                self.ectx.bind_label(trampoline.label);
                if !trampoline.moves.is_empty() {
                    self.flush_all_vregs();
                    for (from, to) in trampoline.moves {
                        self.emit_edit_move(from, to);
                    }
                }
                self.ectx.emit_branch(trampoline.target);
            }
        }

        // r[impl ir.regalloc.no-boundary-flush]
        fn flush_all_vregs(&mut self) {
            self.const_vregs.fill(None);
        }

        fn emit_recipe_ops(&mut self, ops: Vec<Op>) {
            self.flush_all_vregs();
            self.ectx.emit_recipe(&Recipe {
                ops,
                label_count: 0,
            });
        }

        fn current_alloc(&self, operand_index: usize) -> Allocation {
            self.current_inst_allocs
                .as_ref()
                .and_then(|allocs| allocs.get(operand_index).copied())
                .unwrap_or_else(|| {
                    panic!("missing regalloc allocation for operand index {operand_index}")
                })
        }

        fn emit_load_x9_from_allocation(&mut self, alloc: Allocation) {
            if let Some(reg) = alloc.as_reg() {
                assert!(
                    self.emit_mov_x9_from_preg(reg),
                    "unsupported register allocation class {:?} for x9 load",
                    reg.class()
                );
                return;
            }
            if let Some(slot) = alloc.as_stack() {
                let off = self.spill_off(slot);
                dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x9, [sp, #off]);
                return;
            }
            panic!("unexpected none allocation for x9 load");
        }

        fn emit_load_x10_from_allocation(&mut self, alloc: Allocation) {
            if let Some(reg) = alloc.as_reg() {
                assert!(
                    reg.class() == regalloc2::RegClass::Int,
                    "unsupported register allocation class {:?} for x10 load",
                    reg.class()
                );
                match reg.hw_enc() {
                    0 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x0),
                    1 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x1),
                    2 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x2),
                    3 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x3),
                    4 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x4),
                    5 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x5),
                    6 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x6),
                    7 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x7),
                    8 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x8),
                    9 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x9),
                    10 => {}
                    11 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x11),
                    12 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x12),
                    13 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x13),
                    14 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x14),
                    15 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x15),
                    16 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x16),
                    17 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x10, x17),
                    _ => panic!("unsupported hw_enc {} for x10 load", reg.hw_enc()),
                }
                return;
            }
            if let Some(slot) = alloc.as_stack() {
                let off = self.spill_off(slot);
                dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x10, [sp, #off]);
                return;
            }
            panic!("unexpected none allocation for x10 load");
        }

        fn emit_store_x9_to_allocation(&mut self, alloc: Allocation) -> bool {
            if let Some(reg) = alloc.as_reg() {
                return self.emit_mov_preg_from_x9(reg);
            }
            if let Some(slot) = alloc.as_stack() {
                let off = self.spill_off(slot);
                dynasm!(self.ectx.ops ; .arch aarch64 ; str x9, [sp, #off]);
                return true;
            }
            false
        }

        fn emit_load_use_x9(&mut self, v: crate::ir::VReg, operand_index: usize) {
            let _ = v;
            let alloc = self.current_alloc(operand_index);
            self.emit_load_x9_from_allocation(alloc);
        }

        fn emit_load_use_x10(&mut self, v: crate::ir::VReg, operand_index: usize) {
            let _ = v;
            let alloc = self.current_alloc(operand_index);
            self.emit_load_x10_from_allocation(alloc);
        }

        fn emit_store_def_x9(&mut self, _v: crate::ir::VReg, operand_index: usize) {
            let alloc = self.current_alloc(operand_index);
            let _ = self.emit_store_x9_to_allocation(alloc);
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
            self.emit_store_def_x9(dst, 0);
            self.set_const(dst, None);
        }

        fn emit_write_to_field(&mut self, src: crate::ir::VReg, offset: u32, width: Width) {
            self.emit_load_use_x9(src, 0);
            match width {
                Width::W1 => dynasm!(self.ectx.ops ; .arch aarch64 ; strb w9, [x21, #offset]),
                Width::W2 => dynasm!(self.ectx.ops ; .arch aarch64 ; strh w9, [x21, #offset]),
                Width::W4 => dynasm!(self.ectx.ops ; .arch aarch64 ; str w9, [x21, #offset]),
                Width::W8 => dynasm!(self.ectx.ops ; .arch aarch64 ; str x9, [x21, #offset]),
            }
        }

        fn emit_save_out_ptr(&mut self, dst: crate::ir::VReg) {
            dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x21);
            self.emit_store_def_x9(dst, 0);
            self.set_const(dst, None);
        }

        fn emit_set_out_ptr(&mut self, src: crate::ir::VReg) {
            self.emit_load_use_x9(src, 0);
            dynasm!(self.ectx.ops ; .arch aarch64 ; mov x21, x9);
        }

        fn emit_slot_addr(&mut self, dst: crate::ir::VReg, slot: crate::ir::SlotId) {
            let slot_off = self.slot_off(slot);
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; add x9, sp, #slot_off
            );
            self.emit_store_def_x9(dst, 0);
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
            self.emit_store_def_x9(dst, 0);
            self.set_const(dst, None);
            self.ectx.emit_advance_cursor_by(count);
        }

        fn emit_peek_byte(&mut self, dst: crate::ir::VReg) {
            self.emit_recipe_ops(vec![Op::BoundsCheck { count: 1 }]);
            dynasm!(self.ectx.ops ; .arch aarch64 ; ldrb w9, [x19]);
            self.emit_store_def_x9(dst, 0);
            self.set_const(dst, None);
        }

        fn emit_binop(
            &mut self,
            kind: BinOpKind,
            dst: crate::ir::VReg,
            lhs: crate::ir::VReg,
            rhs: crate::ir::VReg,
        ) {
            self.emit_load_use_x9(lhs, 0);
            let rhs_const = self.const_of(rhs);
            match kind {
                BinOpKind::Add => {
                    if let Some(c) = rhs_const
                        && c <= 4095
                    {
                        dynasm!(self.ectx.ops ; .arch aarch64 ; add x9, x9, c as u32);
                    } else {
                        self.emit_load_use_x10(rhs, 1);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; add x9, x9, x10);
                    }
                }
                BinOpKind::Sub => {
                    if let Some(c) = rhs_const
                        && c <= 4095
                    {
                        dynasm!(self.ectx.ops ; .arch aarch64 ; sub x9, x9, c as u32);
                    } else {
                        self.emit_load_use_x10(rhs, 1);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; sub x9, x9, x10);
                    }
                }
                BinOpKind::And => {
                    if matches!(rhs_const, Some(0x7f | 0x7e | 0x80 | 0x1)) {
                        let c = rhs_const.expect("just matched Some");
                        dynasm!(self.ectx.ops ; .arch aarch64 ; and x9, x9, c);
                    } else {
                        self.emit_load_use_x10(rhs, 1);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; and x9, x9, x10);
                    }
                }
                BinOpKind::Or => {
                    self.emit_load_use_x10(rhs, 1);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; orr x9, x9, x10);
                }
                BinOpKind::Xor => {
                    self.emit_load_use_x10(rhs, 1);
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
                        self.emit_load_use_x10(rhs, 1);
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
                        self.emit_load_use_x10(rhs, 1);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; lsr x9, x9, x10);
                    }
                }
                BinOpKind::Shl => {
                    if let Some(c) = rhs_const
                        && c <= 63
                    {
                        dynasm!(self.ectx.ops ; .arch aarch64 ; lsl x9, x9, c as u32);
                    } else {
                        self.emit_load_use_x10(rhs, 1);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; lsl x9, x9, x10);
                    }
                }
            }
            self.emit_store_def_x9(dst, 2);
            self.set_const(dst, None);
        }

        fn emit_unary(&mut self, kind: UnaryOpKind, dst: crate::ir::VReg, src: crate::ir::VReg) {
            self.emit_load_use_x9(src, 0);
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
            self.emit_store_def_x9(dst, 1);
            self.set_const(dst, None);
        }

        fn emit_branch_if(&mut self, cond: crate::ir::VReg, target: DynamicLabel, invert: bool) {
            let _ = cond;
            let alloc = self.current_alloc(0);
            if let Some(reg) = alloc.as_reg() {
                assert!(
                    reg.class() == regalloc2::RegClass::Int,
                    "unsupported register allocation class {:?} for branch condition",
                    reg.class()
                );
                match (reg.hw_enc(), invert) {
                    (0, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x0, =>target),
                    (0, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x0, =>target),
                    (1, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x1, =>target),
                    (1, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x1, =>target),
                    (2, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x2, =>target),
                    (2, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x2, =>target),
                    (3, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x3, =>target),
                    (3, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x3, =>target),
                    (4, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x4, =>target),
                    (4, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x4, =>target),
                    (5, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x5, =>target),
                    (5, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x5, =>target),
                    (6, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x6, =>target),
                    (6, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x6, =>target),
                    (7, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x7, =>target),
                    (7, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x7, =>target),
                    (8, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x8, =>target),
                    (8, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x8, =>target),
                    (9, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x9, =>target),
                    (9, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x9, =>target),
                    (10, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x10, =>target),
                    (10, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x10, =>target),
                    (11, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x11, =>target),
                    (11, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x11, =>target),
                    (12, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x12, =>target),
                    (12, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x12, =>target),
                    (13, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x13, =>target),
                    (13, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x13, =>target),
                    (14, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x14, =>target),
                    (14, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x14, =>target),
                    (15, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x15, =>target),
                    (15, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x15, =>target),
                    (16, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x16, =>target),
                    (16, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x16, =>target),
                    (17, false) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x17, =>target),
                    (17, true) => dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x17, =>target),
                    _ => panic!("unsupported hw_enc {} for branch condition", reg.hw_enc()),
                }
                return;
            }
            if let Some(slot) = alloc.as_stack() {
                let off = self.spill_off(slot);
                dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x9, [sp, #off]);
                if invert {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; cbz x9, =>target);
                } else {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; cbnz x9, =>target);
                }
                return;
            }
            panic!("unexpected none allocation for branch condition");
        }

        fn emit_jump_table(
            &mut self,
            predicate: crate::ir::VReg,
            labels: &[LabelId],
            default: LabelId,
            linear_op_index: usize,
        ) {
            self.emit_load_use_x9(predicate, 0);
            for (index, label) in labels.iter().enumerate() {
                let target = self.edge_target_label(linear_op_index, index, self.label(*label));
                self.emit_load_u32_w10(index as u32);
                dynasm!(self.ectx.ops
                    ; .arch aarch64
                    ; cmp w9, w10
                    ; b.eq =>target
                );
            }
            let default_succ_index = labels.len();
            let default_target =
                self.edge_target_label(linear_op_index, default_succ_index, self.label(default));
            self.ectx.emit_branch(default_target);
        }

        fn emit_load_intrinsic_arg_x1(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg {
                    vreg,
                    operand_index,
                } => {
                    self.emit_load_use_x9(vreg, operand_index);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; mov x1, x9);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x1, x21, #offset);
                }
            }
        }

        fn emit_load_intrinsic_arg_x2(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg {
                    vreg,
                    operand_index,
                } => {
                    self.emit_load_use_x9(vreg, operand_index);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; mov x2, x9);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x2, x21, #offset);
                }
            }
        }

        fn emit_load_intrinsic_arg_x3(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg {
                    vreg,
                    operand_index,
                } => {
                    self.emit_load_use_x9(vreg, operand_index);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; mov x3, x9);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x3, x21, #offset);
                }
            }
        }

        fn emit_load_intrinsic_arg_x4(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg {
                    vreg,
                    operand_index,
                } => {
                    self.emit_load_use_x9(vreg, operand_index);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; mov x4, x9);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x4, x21, #offset);
                }
            }
        }

        fn emit_load_intrinsic_arg_x5(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg {
                    vreg,
                    operand_index,
                } => {
                    self.emit_load_use_x9(vreg, operand_index);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; mov x5, x9);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x5, x21, #offset);
                }
            }
        }

        fn emit_load_intrinsic_arg_x6(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg {
                    vreg,
                    operand_index,
                } => {
                    self.emit_load_use_x9(vreg, operand_index);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; mov x6, x9);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x6, x21, #offset);
                }
            }
        }

        fn emit_load_intrinsic_arg_x7(&mut self, arg: IntrinsicArg) {
            match arg {
                IntrinsicArg::VReg {
                    vreg,
                    operand_index,
                } => {
                    self.emit_load_use_x9(vreg, operand_index);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; mov x7, x9);
                }
                IntrinsicArg::OutField(offset) => {
                    dynasm!(self.ectx.ops ; .arch aarch64 ; add x7, x21, #offset);
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
                    let dst_operand_index = args.len();
                    // Return-value intrinsic ABI: fn(ctx, args...) -> value
                    let call_args: Vec<IntrinsicArg> = args
                        .iter()
                        .copied()
                        .enumerate()
                        .map(|(i, vreg)| IntrinsicArg::VReg {
                            vreg,
                            operand_index: i,
                        })
                        .collect();
                    self.emit_call_intrinsic_with_args(fn_ptr, &call_args);
                    dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x0);
                    self.emit_store_def_x9(dst, dst_operand_index);
                }
                None => {
                    let mut call_args: Vec<IntrinsicArg> = args
                        .iter()
                        .copied()
                        .enumerate()
                        .map(|(i, vreg)| IntrinsicArg::VReg {
                            vreg,
                            operand_index: i,
                        })
                        .collect();
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

            // Seed regalloc locations for entry block params directly from ABI arg regs x2..x7.
            let lambda_id = self
                .current_func
                .as_ref()
                .expect("emit_store_incoming_lambda_args without active function")
                .lambda_id
                .index() as u32;
            for arg_index in 0..data_args.len() {
                match arg_index {
                    0 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x2),
                    1 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x3),
                    2 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x4),
                    3 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x5),
                    4 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x6),
                    5 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x7),
                    _ => unreachable!(),
                }
                let per_arg_allocs = self
                    .entry_arg_allocs_by_lambda
                    .get(&lambda_id)
                    .and_then(|all| all.get(arg_index))
                    .cloned()
                    .unwrap_or_default();
                for alloc in per_arg_allocs {
                    let _ = self.emit_store_x9_to_allocation(alloc);
                }
            }
        }

        fn emit_load_lambda_results_to_ret_regs(
            &mut self,
            lambda_id: crate::ir::LambdaId,
            data_results: &[crate::ir::VReg],
        ) {
            if data_results.len() > 2 {
                panic!(
                    "aarch64 CallLambda supports at most 2 data results, got {}",
                    data_results.len()
                );
            }

            let result_allocs = self
                .return_result_allocs_by_lambda
                .get(&(lambda_id.index() as u32))
                .cloned()
                .unwrap_or_default();
            assert!(
                result_allocs.len() >= data_results.len(),
                "missing return allocation mapping for lambda {:?}: need {}, got {}",
                lambda_id,
                data_results.len(),
                result_allocs.len()
            );

            if let Some(&alloc) = result_allocs.first() {
                self.emit_load_x9_from_allocation(alloc);
                dynasm!(self.ectx.ops ; .arch aarch64 ; mov x0, x9);
            }
            if let Some(&alloc) = result_allocs.get(1) {
                self.emit_load_x9_from_allocation(alloc);
                dynasm!(self.ectx.ops ; .arch aarch64 ; mov x1, x9);
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
                self.emit_load_use_x9(arg, i);
                match i {
                    0 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x2, x9),
                    1 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x3, x9),
                    2 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x4, x9),
                    3 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x5, x9),
                    4 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x6, x9),
                    5 => dynasm!(self.ectx.ops ; .arch aarch64 ; mov x7, x9),
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
                self.emit_store_def_x9(dst, args.len());
            }
            if let Some(&dst) = results.get(1) {
                dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x1);
                self.emit_store_def_x9(dst, args.len() + 1);
            }
        }

        fn run(mut self, ir: &LinearIr) -> LinearBackendResult {
            for op in &ir.ops {
                let linear_op_index = if self.current_func.is_some()
                    && !matches!(op, LinearOp::FuncStart { .. } | LinearOp::FuncEnd)
                {
                    Some(self.current_lambda_linear_op_index)
                } else {
                    None
                };
                self.current_inst_allocs = linear_op_index.and_then(|lin_idx| {
                    let lambda_id = self.current_func.as_ref()?.lambda_id.index() as u32;
                    self.allocs_by_lambda
                        .get(&lambda_id)
                        .and_then(|by_lambda| by_lambda.get(&lin_idx))
                        .cloned()
                });
                if let Some(linear_op_index) = linear_op_index {
                    self.apply_regalloc_edits(linear_op_index, InstPosition::Before);
                }

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
                            lambda_id: *lambda_id,
                        });
                        self.current_lambda_linear_op_index = 0;
                        self.emit_store_incoming_lambda_args(data_args);
                    }
                    LinearOp::FuncEnd => {
                        self.flush_all_vregs();
                        let func = self
                            .current_func
                            .take()
                            .expect("FuncEnd without active function");
                        self.emit_load_lambda_results_to_ret_regs(
                            func.lambda_id,
                            &func.data_results,
                        );
                        self.ectx.end_func(func.error_exit);
                    }
                    LinearOp::Label(label) => {
                        self.flush_all_vregs();
                        self.ectx.bind_label(self.label(*label));
                    }
                    LinearOp::Branch(target) => {
                        let target_label = if let Some(lin_idx) = linear_op_index {
                            self.edge_target_label(lin_idx, 0, self.label(*target))
                        } else {
                            self.label(*target)
                        };
                        self.ectx.emit_branch(target_label);
                    }
                    LinearOp::BranchIf { cond, target } => {
                        let lin_idx = linear_op_index
                            .expect("BranchIf should have linear op index inside function");
                        let taken_target = self.edge_target_label(lin_idx, 0, self.label(*target));
                        if self.has_edge_edits(lin_idx, 1) {
                            let fallthrough_cont = self.ectx.new_label();
                            let fallthrough_target =
                                self.edge_target_label(lin_idx, 1, fallthrough_cont);
                            self.emit_branch_if(*cond, taken_target, false);
                            self.ectx.emit_branch(fallthrough_target);
                            self.ectx.bind_label(fallthrough_cont);
                        } else {
                            self.emit_branch_if(*cond, taken_target, false);
                        }
                    }
                    LinearOp::BranchIfZero { cond, target } => {
                        let lin_idx = linear_op_index
                            .expect("BranchIfZero should have linear op index inside function");
                        let taken_target = self.edge_target_label(lin_idx, 0, self.label(*target));
                        if self.has_edge_edits(lin_idx, 1) {
                            let fallthrough_cont = self.ectx.new_label();
                            let fallthrough_target =
                                self.edge_target_label(lin_idx, 1, fallthrough_cont);
                            self.emit_branch_if(*cond, taken_target, true);
                            self.ectx.emit_branch(fallthrough_target);
                            self.ectx.bind_label(fallthrough_cont);
                        } else {
                            self.emit_branch_if(*cond, taken_target, true);
                        }
                    }
                    LinearOp::JumpTable {
                        predicate,
                        labels,
                        default,
                    } => {
                        let lin_idx = linear_op_index
                            .expect("JumpTable should have linear op index inside function");
                        self.emit_jump_table(*predicate, labels, *default, lin_idx);
                    }

                    LinearOp::Const { dst, value } => {
                        self.emit_load_u64_x9(*value);
                        self.emit_store_def_x9(*dst, 0);
                        self.set_const(*dst, Some(*value));
                    }
                    LinearOp::Copy { dst, src } => {
                        let from = self.current_alloc(0);
                        let to = self.current_alloc(1);
                        self.emit_edit_move(from, to);
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
                        self.emit_load_use_x9(*src, 0);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; add x19, x19, x9);
                    }
                    LinearOp::SaveCursor { dst } => {
                        dynasm!(self.ectx.ops ; .arch aarch64 ; mov x9, x19);
                        self.emit_store_def_x9(*dst, 0);
                        self.set_const(*dst, None);
                    }
                    LinearOp::RestoreCursor { src } => {
                        self.emit_load_use_x9(*src, 0);
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
                        self.emit_load_use_x9(*src, 0);
                        let off = self.slot_off(*slot);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; str x9, [sp, #off]);
                    }
                    LinearOp::ReadFromSlot { dst, slot } => {
                        let off = self.slot_off(*slot);
                        dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x9, [sp, #off]);
                        self.emit_store_def_x9(*dst, 0);
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
                if let Some(linear_op_index) = linear_op_index {
                    self.apply_regalloc_edits(linear_op_index, InstPosition::After);
                    self.current_lambda_linear_op_index += 1;
                }
                self.current_inst_allocs = None;
            }

            if self.current_func.is_some() {
                panic!("unterminated function: missing FuncEnd");
            }

            self.emit_edge_trampolines();

            let entry = self.entry.expect("missing root FuncStart for lambda 0");
            let buf = self.ectx.finalize();
            LinearBackendResult { buf, entry }
        }
    }

    Lowerer::new(ir, max_spillslots, alloc).run(ir)
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
    fn linear_backend_call_intrinsic_zero_arg_return_value() {
        unsafe extern "C" fn return_300(_ctx: *mut crate::context::DeserContext) -> u64 {
            300
        }

        let mut builder = IrBuilder::new(<u32 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            let v = rb
                .call_intrinsic(IntrinsicFn(return_300 as *const () as usize), &[], 0, true)
                .expect("intrinsic should produce output");
            rb.write_to_field(v, 0, Width::W4);
            rb.set_results(&[]);
        }

        let mut func = builder.finish();
        let lin = linearize(&mut func);
        let (value, ctx) = run_u32_decoder(&lin, &[]);

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
