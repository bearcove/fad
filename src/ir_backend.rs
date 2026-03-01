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

        fn emit_call_intrinsic(
            &mut self,
            func: crate::ir::IntrinsicFn,
            args: &[crate::ir::VReg],
            dst: Option<crate::ir::VReg>,
            field_offset: u32,
        ) {
            let fn_ptr = func.0 as *const u8;
            if !args.is_empty() {
                panic!(
                    "unsupported CallIntrinsic with args in linear backend adapter: {} args",
                    args.len()
                );
            }

            match dst {
                Some(dst) => {
                    let out_offset = self.vreg_off(dst);
                    self.ectx.emit_store_imm64_to_stack(out_offset, 0);
                    self.emit_recipe_ops(vec![Op::CallIntrinsicStackOut {
                        fn_ptr,
                        sp_offset: out_offset,
                    }]);
                }
                None => {
                    self.emit_recipe_ops(vec![Op::CallIntrinsic {
                        fn_ptr,
                        field_offset,
                    }]);
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

                    LinearOp::CallLambda { .. } => {
                        panic!("unsupported CallLambda in linear backend adapter");
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

        fn emit_call_intrinsic(
            &mut self,
            func: crate::ir::IntrinsicFn,
            args: &[crate::ir::VReg],
            dst: Option<crate::ir::VReg>,
            field_offset: u32,
        ) {
            let fn_ptr = func.0 as *const u8;
            if !args.is_empty() {
                panic!(
                    "unsupported CallIntrinsic with args in linear backend adapter: {} args",
                    args.len()
                );
            }

            match dst {
                Some(dst) => {
                    let out_offset = self.vreg_off(dst);
                    self.ectx.emit_store_imm64_to_stack(out_offset, 0);
                    self.emit_recipe_ops(vec![Op::CallIntrinsicStackOut {
                        fn_ptr,
                        sp_offset: out_offset,
                    }]);
                }
                None => {
                    self.emit_recipe_ops(vec![Op::CallIntrinsic {
                        fn_ptr,
                        field_offset,
                    }]);
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

                    LinearOp::CallLambda { .. } => {
                        panic!("unsupported CallLambda in linear backend adapter");
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
}
