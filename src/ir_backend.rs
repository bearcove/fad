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
    }

    struct Lowerer {
        ectx: EmitCtx,
        labels: Vec<DynamicLabel>,
        lambda_labels: Vec<DynamicLabel>,
        slot_base: u32,
        vreg_base: u32,
        entry: Option<AssemblyOffset>,
        current_func: Option<FunctionCtx>,
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

        fn run(mut self, ir: &LinearIr) -> LinearBackendResult {
            for op in &ir.ops {
                match op {
                    LinearOp::FuncStart { lambda_id, .. } => {
                        let label = self.lambda_labels[lambda_id.index() as usize];
                        self.ectx.bind_label(label);
                        let (entry_offset, error_exit) = self.ectx.begin_func();
                        if lambda_id.index() == 0 {
                            self.entry = Some(entry_offset);
                        }
                        self.current_func = Some(FunctionCtx { error_exit });
                    }
                    LinearOp::FuncEnd => {
                        let func = self
                            .current_func
                            .take()
                            .expect("FuncEnd without active function");
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
                        if !args.is_empty() || !results.is_empty() {
                            panic!(
                                "CallLambda with data args/results is not supported yet: args={}, results={}",
                                args.len(),
                                results.len()
                            );
                        }
                        let label = self.lambda_labels[target.index() as usize];
                        self.ectx.emit_call_emitted_func(label, 0);
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
    use crate::recipe::{Op, Recipe, Slot};

    struct FunctionCtx {
        error_exit: DynamicLabel,
    }

    struct Lowerer {
        ectx: EmitCtx,
        labels: Vec<DynamicLabel>,
        lambda_labels: Vec<DynamicLabel>,
        slot_base: u32,
        vreg_base: u32,
        entry: Option<AssemblyOffset>,
        current_func: Option<FunctionCtx>,
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

        fn emit_load_vreg_x9(&mut self, v: crate::ir::VReg) {
            let off = self.vreg_off(v);
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; ldr x9, [sp, #off]
            );
        }

        fn emit_store_x9_to_vreg(&mut self, v: crate::ir::VReg) {
            let off = self.vreg_off(v);
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; str x9, [sp, #off]
            );
        }

        fn emit_load_u32_w10(&mut self, value: u32) {
            let lo = (value & 0xFFFF) as u32;
            let hi = ((value >> 16) & 0xFFFF) as u32;
            dynasm!(self.ectx.ops ; .arch aarch64 ; movz w10, #lo);
            if value > 0xFFFF {
                dynasm!(self.ectx.ops ; .arch aarch64 ; movk w10, #hi, LSL #16);
            }
        }

        fn emit_read_from_field(&mut self, dst: crate::ir::VReg, offset: u32, width: Width) {
            match width {
                Width::W1 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldrb w9, [x21, #offset]),
                Width::W2 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldrh w9, [x21, #offset]),
                Width::W4 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr w9, [x21, #offset]),
                Width::W8 => dynasm!(self.ectx.ops ; .arch aarch64 ; ldr x9, [x21, #offset]),
            }
            self.emit_store_x9_to_vreg(dst);
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
            let off = self.vreg_off(dst);
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; str x21, [sp, #off]
            );
        }

        fn emit_set_out_ptr(&mut self, src: crate::ir::VReg) {
            let off = self.vreg_off(src);
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; ldr x21, [sp, #off]
            );
        }

        fn emit_slot_addr(&mut self, dst: crate::ir::VReg, slot: crate::ir::SlotId) {
            let dst_off = self.vreg_off(dst);
            let slot_off = self.slot_off(slot);
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; add x9, sp, #slot_off
                ; str x9, [sp, #dst_off]
            );
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
            self.ectx.emit_advance_cursor_by(count);
        }

        fn emit_peek_byte(&mut self, dst: crate::ir::VReg) {
            self.emit_recipe_ops(vec![Op::BoundsCheck { count: 1 }]);
            dynasm!(self.ectx.ops ; .arch aarch64 ; ldrb w9, [x19]);
            self.emit_store_x9_to_vreg(dst);
        }

        fn emit_binop(
            &mut self,
            kind: BinOpKind,
            dst: crate::ir::VReg,
            lhs: crate::ir::VReg,
            rhs: crate::ir::VReg,
        ) {
            self.emit_load_vreg_x9(lhs);
            let rhs_off = self.vreg_off(rhs);
            dynasm!(self.ectx.ops
                ; .arch aarch64
                ; ldr x10, [sp, #rhs_off]
            );
            match kind {
                BinOpKind::Add => dynasm!(self.ectx.ops ; .arch aarch64 ; add x9, x9, x10),
                BinOpKind::Sub => dynasm!(self.ectx.ops ; .arch aarch64 ; sub x9, x9, x10),
                BinOpKind::And => dynasm!(self.ectx.ops ; .arch aarch64 ; and x9, x9, x10),
                BinOpKind::Or => dynasm!(self.ectx.ops ; .arch aarch64 ; orr x9, x9, x10),
                BinOpKind::Xor => dynasm!(self.ectx.ops ; .arch aarch64 ; eor x9, x9, x10),
                BinOpKind::Shr => dynasm!(self.ectx.ops ; .arch aarch64 ; lsr x9, x9, x10),
                BinOpKind::Shl => dynasm!(self.ectx.ops ; .arch aarch64 ; lsl x9, x9, x10),
            }
            self.emit_store_x9_to_vreg(dst);
        }

        fn emit_unary(&mut self, kind: UnaryOpKind, dst: crate::ir::VReg, src: crate::ir::VReg) {
            self.emit_load_vreg_x9(src);
            match kind {
                UnaryOpKind::ZigzagDecode { wide: true } => {
                    dynasm!(self.ectx.ops
                        ; .arch aarch64
                        ; lsr x10, x9, #1
                        ; and x11, x9, #1
                        ; neg x11, x11
                        ; eor x9, x10, x11
                    );
                }
                UnaryOpKind::ZigzagDecode { wide: false } => {
                    dynasm!(self.ectx.ops
                        ; .arch aarch64
                        ; lsr w10, w9, #1
                        ; and w11, w9, #1
                        ; neg w11, w11
                        ; eor w9, w10, w11
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
        }

        fn emit_branch_if(&mut self, cond: crate::ir::VReg, target: DynamicLabel, invert: bool) {
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
                    } else {
                        // Return-value intrinsic ABI: fn(ctx, args...) -> value
                        let call_args: Vec<IntrinsicArg> =
                            args.iter().copied().map(IntrinsicArg::VReg).collect();
                        self.emit_call_intrinsic_with_args(fn_ptr, &call_args);
                        let out_offset = self.vreg_off(dst);
                        dynasm!(self.ectx.ops
                            ; .arch aarch64
                            ; str x0, [sp, #out_offset]
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

        fn run(mut self, ir: &LinearIr) -> LinearBackendResult {
            for op in &ir.ops {
                match op {
                    LinearOp::FuncStart { lambda_id, .. } => {
                        let label = self.lambda_labels[lambda_id.index() as usize];
                        self.ectx.bind_label(label);
                        let (entry_offset, error_exit) = self.ectx.begin_func();
                        if lambda_id.index() == 0 {
                            self.entry = Some(entry_offset);
                        }
                        self.current_func = Some(FunctionCtx { error_exit });
                    }
                    LinearOp::FuncEnd => {
                        let func = self
                            .current_func
                            .take()
                            .expect("FuncEnd without active function");
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
                        if !args.is_empty() || !results.is_empty() {
                            panic!(
                                "CallLambda with data args/results is not supported yet: args={}, results={}",
                                args.len(),
                                results.len()
                            );
                        }
                        let label = self.lambda_labels[target.index() as usize];
                        self.ectx.emit_call_emitted_func(label, 0);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler;
    use crate::context::{DeserContext, ErrorCode};
    use crate::ir::{IntrinsicFn, IrBuilder, Width};
    use crate::linearize::linearize;

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
}
