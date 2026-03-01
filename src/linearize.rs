//! Linearizer: converts the RVSDG into a flat instruction sequence.
//!
//! The RVSDG is a tree of regions and nodes. The linearizer walks this tree,
//! topologically sorts each region's nodes, and emits a flat `Vec<LinearOp>`
//! with explicit labels and branches for control flow (gamma/theta).

use std::collections::VecDeque;
use std::fmt;

use crate::context::ErrorCode;
use crate::ir::{
    Id, IrFunc, IrOp, LambdaId, Node, NodeId, NodeKind, PortKind, PortSource, RegionId, SlotId,
    VReg, Width,
};

// ─── Label ID ────────────────────────────────────────────────────────────────

/// Marker type for label IDs.
pub struct LabelMarker;
/// A label in the linear instruction sequence.
pub type LabelId = Id<LabelMarker>;

// ─── BinOpKind / UnaryOpKind ─────────────────────────────────────────────────

/// Binary operation kind for linear IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Sub,
    And,
    Or,
    Shr,
    Shl,
    Xor,
}

/// Unary operation kind for linear IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOpKind {
    ZigzagDecode { wide: bool },
    SignExtend { from_width: Width },
}

// ─── IntrinsicFn re-export ───────────────────────────────────────────────────

use crate::ir::IntrinsicFn;

// ─── LinearOp ────────────────────────────────────────────────────────────────

/// A single instruction in the linearized IR.
///
/// Each variant corresponds to an RVSDG `IrOp`, but flattened into a linear
/// sequence with explicit labels and branches for control flow.
// r[impl ir.linearize]
#[derive(Debug, Clone)]
pub enum LinearOp {
    // ── Values ──
    Const {
        dst: VReg,
        value: u64,
    },
    BinOp {
        op: BinOpKind,
        dst: VReg,
        lhs: VReg,
        rhs: VReg,
    },
    UnaryOp {
        op: UnaryOpKind,
        dst: VReg,
        src: VReg,
    },
    /// Copy a value between virtual registers (for gamma merge / theta feedback).
    Copy {
        dst: VReg,
        src: VReg,
    },

    // ── Cursor ──
    BoundsCheck {
        count: u32,
    },
    ReadBytes {
        dst: VReg,
        count: u32,
    },
    PeekByte {
        dst: VReg,
    },
    AdvanceCursor {
        count: u32,
    },
    AdvanceCursorBy {
        src: VReg,
    },
    SaveCursor {
        dst: VReg,
    },
    RestoreCursor {
        src: VReg,
    },

    // ── Output ──
    WriteToField {
        src: VReg,
        offset: u32,
        width: Width,
    },
    ReadFromField {
        dst: VReg,
        offset: u32,
        width: Width,
    },
    SaveOutPtr {
        dst: VReg,
    },
    SetOutPtr {
        src: VReg,
    },

    // ── Stack ──
    SlotAddr {
        dst: VReg,
        slot: SlotId,
    },
    WriteToSlot {
        slot: SlotId,
        src: VReg,
    },
    ReadFromSlot {
        dst: VReg,
        slot: SlotId,
    },

    // ── Calls ──
    CallIntrinsic {
        func: IntrinsicFn,
        args: Vec<VReg>,
        dst: Option<VReg>,
        field_offset: u32,
    },
    CallPure {
        func: IntrinsicFn,
        args: Vec<VReg>,
        dst: VReg,
    },

    // ── Control flow ──
    Label(LabelId),
    Branch(LabelId),
    /// Branch if condition is nonzero.
    BranchIf {
        cond: VReg,
        target: LabelId,
    },
    /// Branch if condition is zero.
    BranchIfZero {
        cond: VReg,
        target: LabelId,
    },
    /// Jump table: jump to `labels[predicate]`, or to `default` if out of range.
    JumpTable {
        predicate: VReg,
        labels: Vec<LabelId>,
        default: LabelId,
    },

    // ── Error ──
    ErrorExit {
        code: ErrorCode,
    },

    // ── SIMD ──
    SimdStringScan {
        pos: VReg,
        kind: VReg,
    },
    SimdWhitespaceSkip,

    // ── Function structure ──
    FuncStart {
        lambda_id: LambdaId,
        shape: &'static facet::Shape,
    },
    FuncEnd,
    CallLambda {
        target: LambdaId,
        args: Vec<VReg>,
        results: Vec<VReg>,
    },
}

// ─── LinearIr ────────────────────────────────────────────────────────────────

/// The linearized form of an RVSDG function.
pub struct LinearIr {
    /// The flat instruction sequence.
    pub ops: Vec<LinearOp>,
    /// Total number of labels allocated.
    pub label_count: u32,
    /// Total number of virtual registers.
    pub vreg_count: u32,
    /// Total number of stack slots.
    pub slot_count: u32,
}

// ─── Linearizer state ────────────────────────────────────────────────────────

struct Linearizer<'a> {
    func: &'a IrFunc,
    ops: Vec<LinearOp>,
    label_count: u32,
}

impl<'a> Linearizer<'a> {
    fn new(func: &'a IrFunc) -> Self {
        Self {
            func,
            ops: Vec::new(),
            label_count: 0,
        }
    }

    fn fresh_label(&mut self) -> LabelId {
        let id = LabelId::new(self.label_count);
        self.label_count += 1;
        id
    }

    fn emit(&mut self, op: LinearOp) {
        self.ops.push(op);
    }

    /// Resolve a PortSource to the VReg it produces.
    fn resolve_vreg(&self, source: PortSource) -> VReg {
        match source {
            PortSource::Node(output_ref) => {
                let node = &self.func.nodes[output_ref.node];
                node.outputs[output_ref.index as usize]
                    .vreg
                    .expect("data port must have vreg assigned")
            }
            PortSource::RegionArg(arg_ref) => {
                let region = &self.func.regions[arg_ref.region];
                region.args[arg_ref.index as usize]
                    .vreg
                    .expect("data region arg must have vreg assigned")
            }
        }
    }

    // ─── Topological sort ────────────────────────────────────────────

    // r[impl ir.linearize.schedule]
    /// Topologically sort a region's nodes respecting data + state edges.
    fn topo_sort(&self, region_id: RegionId) -> Vec<NodeId> {
        let region = &self.func.regions[region_id];
        if region.nodes.is_empty() {
            return Vec::new();
        }

        // Build in-degree count for each node in this region.
        // A node's in-degree = number of its inputs that come from other nodes
        // in the same region.
        let node_set: std::collections::HashSet<NodeId> = region.nodes.iter().copied().collect();

        // Map NodeId -> position in region.nodes for O(1) lookup.
        let mut node_pos: std::collections::HashMap<NodeId, usize> =
            std::collections::HashMap::new();
        for (i, &nid) in region.nodes.iter().enumerate() {
            node_pos.insert(nid, i);
        }

        let n = region.nodes.len();
        let mut in_degree = vec![0u32; n];
        // adjacency: for each node position, list of node positions that depend on it
        let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); n];

        for (i, &nid) in region.nodes.iter().enumerate() {
            let node = &self.func.nodes[nid];
            for input in &node.inputs {
                if let PortSource::Node(output_ref) = input.source
                    && let Some(&dep_pos) = node_pos.get(&output_ref.node)
                    && node_set.contains(&output_ref.node)
                {
                    in_degree[i] += 1;
                    dependents[dep_pos].push(i);
                }
            }
        }

        // Kahn's algorithm with a queue (preserves insertion order for ties).
        let mut queue = VecDeque::new();
        for (i, &deg) in in_degree.iter().enumerate() {
            if deg == 0 {
                queue.push_back(i);
            }
        }

        let mut sorted = Vec::with_capacity(n);
        while let Some(pos) = queue.pop_front() {
            sorted.push(region.nodes[pos]);
            for &dep in &dependents[pos] {
                in_degree[dep] -= 1;
                if in_degree[dep] == 0 {
                    queue.push_back(dep);
                }
            }
        }

        assert_eq!(
            sorted.len(),
            n,
            "cycle detected in region's node dependencies"
        );
        sorted
    }

    // ─── Region linearization ────────────────────────────────────────

    fn linearize_region(&mut self, region_id: RegionId) {
        let sorted = self.topo_sort(region_id);
        for node_id in sorted {
            self.linearize_node(node_id);
        }
    }

    // ─── Node linearization ─────────────────────────────────────────

    fn linearize_node(&mut self, node_id: NodeId) {
        let kind = {
            let node = &self.func.nodes[node_id];
            node.kind.clone_for_linearize()
        };

        match kind {
            NodeKindRef::Simple(op) => self.linearize_simple(node_id, op),
            NodeKindRef::Gamma { regions } => self.linearize_gamma(node_id, &regions),
            NodeKindRef::Theta { body } => self.linearize_theta(node_id, body),
            NodeKindRef::Lambda {
                body,
                shape,
                lambda_id,
            } => {
                self.linearize_lambda(body, shape, lambda_id);
            }
            NodeKindRef::Apply { target } => self.linearize_apply(node_id, target),
        }
    }

    fn linearize_simple(&mut self, node_id: NodeId, op: &IrOp) {
        let node = &self.func.nodes[node_id];

        // Helper: get the VReg of data output at index.
        let data_dst =
            |idx: usize| -> VReg { node.outputs[idx].vreg.expect("data output must have vreg") };

        // Helper: resolve data input at index.
        let data_in = |idx: usize| -> VReg {
            let input = &node.inputs[idx];
            assert_eq!(input.kind, PortKind::Data);
            self.resolve_vreg(input.source)
        };

        match op {
            // ── Constants ──
            IrOp::Const { value } => {
                self.emit(LinearOp::Const {
                    dst: data_dst(0),
                    value: *value,
                });
            }

            // ── Binary arithmetic ──
            IrOp::Add => self.emit_binop(BinOpKind::Add, node),
            IrOp::Sub => self.emit_binop(BinOpKind::Sub, node),
            IrOp::And => self.emit_binop(BinOpKind::And, node),
            IrOp::Or => self.emit_binop(BinOpKind::Or, node),
            IrOp::Shr => self.emit_binop(BinOpKind::Shr, node),
            IrOp::Shl => self.emit_binop(BinOpKind::Shl, node),
            IrOp::Xor => self.emit_binop(BinOpKind::Xor, node),

            // ── Unary ──
            IrOp::ZigzagDecode { wide } => {
                self.emit(LinearOp::UnaryOp {
                    op: UnaryOpKind::ZigzagDecode { wide: *wide },
                    dst: data_dst(0),
                    src: data_in(0),
                });
            }
            IrOp::SignExtend { from_width } => {
                self.emit(LinearOp::UnaryOp {
                    op: UnaryOpKind::SignExtend {
                        from_width: *from_width,
                    },
                    dst: data_dst(0),
                    src: data_in(0),
                });
            }

            // ── Cursor ops ──
            IrOp::BoundsCheck { count } => {
                self.emit(LinearOp::BoundsCheck { count: *count });
            }
            IrOp::ReadBytes { count } => {
                self.emit(LinearOp::ReadBytes {
                    dst: data_dst(0),
                    count: *count,
                });
            }
            IrOp::PeekByte => {
                self.emit(LinearOp::PeekByte { dst: data_dst(0) });
            }
            IrOp::AdvanceCursor { count } => {
                self.emit(LinearOp::AdvanceCursor { count: *count });
            }
            IrOp::AdvanceCursorBy => {
                self.emit(LinearOp::AdvanceCursorBy { src: data_in(0) });
            }
            IrOp::SaveCursor => {
                self.emit(LinearOp::SaveCursor { dst: data_dst(0) });
            }
            IrOp::RestoreCursor => {
                self.emit(LinearOp::RestoreCursor { src: data_in(0) });
            }

            // ── Output ops ──
            IrOp::WriteToField { offset, width } => {
                self.emit(LinearOp::WriteToField {
                    src: data_in(0),
                    offset: *offset,
                    width: *width,
                });
            }
            IrOp::ReadFromField { offset, width } => {
                self.emit(LinearOp::ReadFromField {
                    dst: data_dst(0),
                    offset: *offset,
                    width: *width,
                });
            }
            IrOp::SaveOutPtr => {
                self.emit(LinearOp::SaveOutPtr { dst: data_dst(0) });
            }
            IrOp::SetOutPtr => {
                self.emit(LinearOp::SetOutPtr { src: data_in(0) });
            }

            // ── Stack ops ──
            IrOp::SlotAddr { slot } => {
                self.emit(LinearOp::SlotAddr {
                    dst: data_dst(0),
                    slot: *slot,
                });
            }
            IrOp::WriteToSlot { slot } => {
                self.emit(LinearOp::WriteToSlot {
                    slot: *slot,
                    src: data_in(0),
                });
            }
            IrOp::ReadFromSlot { slot } => {
                self.emit(LinearOp::ReadFromSlot {
                    dst: data_dst(0),
                    slot: *slot,
                });
            }

            // ── Call ops ──
            IrOp::CallIntrinsic {
                func,
                arg_count,
                has_result,
                field_offset,
            } => {
                let args: Vec<VReg> = (0..*arg_count as usize).map(&data_in).collect();
                let dst = if *has_result { Some(data_dst(0)) } else { None };
                self.emit(LinearOp::CallIntrinsic {
                    func: *func,
                    args,
                    dst,
                    field_offset: *field_offset,
                });
            }
            IrOp::CallPure { func, arg_count } => {
                let args: Vec<VReg> = (0..*arg_count as usize).map(&data_in).collect();
                self.emit(LinearOp::CallPure {
                    func: *func,
                    args,
                    dst: data_dst(0),
                });
            }

            // ── Error ──
            IrOp::ErrorExit { code } => {
                self.emit(LinearOp::ErrorExit { code: *code });
            }

            // ── SIMD ──
            IrOp::SimdStringScan => {
                self.emit(LinearOp::SimdStringScan {
                    pos: data_dst(0),
                    kind: data_dst(1),
                });
            }
            IrOp::SimdWhitespaceSkip => {
                self.emit(LinearOp::SimdWhitespaceSkip);
            }
        }
    }

    fn emit_binop(&mut self, op: BinOpKind, node: &Node) {
        let dst = node.outputs[0].vreg.expect("binop must have vreg");
        let lhs = self.resolve_vreg(node.inputs[0].source);
        let rhs = self.resolve_vreg(node.inputs[1].source);
        self.emit(LinearOp::BinOp { op, dst, lhs, rhs });
    }

    // ─── Gamma (conditional) ─────────────────────────────────────────

    fn linearize_gamma(&mut self, node_id: NodeId, regions: &[RegionId]) {
        let node = &self.func.nodes[node_id];
        let branch_count = regions.len();

        // The predicate is the first data input.
        let predicate = self.resolve_vreg(node.inputs[0].source);

        // Allocate labels: one per branch + merge label.
        let branch_labels: Vec<LabelId> = (0..branch_count).map(|_| self.fresh_label()).collect();
        let merge_label = self.fresh_label();

        // Emit JumpTable if > 2 branches, or BranchIfZero for 2-branch case.
        if branch_count == 2 {
            // predicate==0 → branch 0, predicate!=0 → branch 1
            self.emit(LinearOp::BranchIfZero {
                cond: predicate,
                target: branch_labels[0],
            });
            self.emit(LinearOp::Branch(branch_labels[1]));
        } else {
            // General case: jump table
            self.emit(LinearOp::JumpTable {
                predicate,
                labels: branch_labels.clone(),
                default: branch_labels[branch_count - 1],
            });
        }

        // Determine the data output count from the gamma node.
        let data_output_count = node
            .outputs
            .iter()
            .filter(|o| o.kind == PortKind::Data)
            .count();

        // Emit each branch.
        for (branch_idx, &region_id) in regions.iter().enumerate() {
            self.emit(LinearOp::Label(branch_labels[branch_idx]));

            // Emit copies for passthrough inputs → region args.
            self.emit_gamma_entry_copies(node, region_id);

            // Linearize the branch body.
            self.linearize_region(region_id);

            // Emit copies for region results → gamma output vregs.
            self.emit_gamma_exit_copies(node_id, region_id, data_output_count);

            // Branch to merge (skip for last branch — it falls through).
            if branch_idx < branch_count - 1 {
                self.emit(LinearOp::Branch(merge_label));
            }
        }

        self.emit(LinearOp::Label(merge_label));
    }

    /// Emit Copy ops for passthrough data inputs entering a gamma branch region.
    fn emit_gamma_entry_copies(&mut self, node: &Node, region_id: RegionId) {
        let region = &self.func.regions[region_id];
        // Inputs layout: [predicate, passthrough..., cursor_state, output_state]
        // Region args layout: [passthrough..., cursor_state, output_state]
        // Skip predicate (input 0), skip state inputs at the end.
        let passthrough_count = node.inputs.len() - 3; // minus predicate, cursor, output

        for i in 0..passthrough_count {
            let src_input = &node.inputs[i + 1]; // +1 to skip predicate
            if src_input.kind == PortKind::Data {
                let src_vreg = self.resolve_vreg(src_input.source);
                if let Some(dst_vreg) = region.args[i].vreg
                    && src_vreg != dst_vreg
                {
                    self.emit(LinearOp::Copy {
                        dst: dst_vreg,
                        src: src_vreg,
                    });
                }
            }
        }
    }

    /// Emit Copy ops for gamma branch results → gamma node output vregs.
    fn emit_gamma_exit_copies(
        &mut self,
        node_id: NodeId,
        region_id: RegionId,
        data_output_count: usize,
    ) {
        let region = &self.func.regions[region_id];
        let node = &self.func.nodes[node_id];
        // Region results: [data..., cursor_state, output_state]
        // Gamma outputs: [data..., cursor_state, output_state]
        for i in 0..data_output_count {
            let result = &region.results[i];
            if result.kind == PortKind::Data {
                let src_vreg = self.resolve_vreg(result.source);
                let dst_vreg = node.outputs[i]
                    .vreg
                    .expect("gamma data output must have vreg");
                if src_vreg != dst_vreg {
                    self.emit(LinearOp::Copy {
                        dst: dst_vreg,
                        src: src_vreg,
                    });
                }
            }
        }
    }

    // ─── Theta (loop) ────────────────────────────────────────────────

    fn linearize_theta(&mut self, node_id: NodeId, body: RegionId) {
        let node = &self.func.nodes[node_id];
        let body_region = &self.func.regions[body];

        // Theta inputs: [loop_vars..., cursor_state, output_state]
        // Body args: [loop_vars..., cursor_state, output_state]
        // Body results: [predicate, loop_vars..., cursor_state, output_state]
        // Theta outputs: [loop_vars..., cursor_state, output_state]

        let total_inputs = node.inputs.len();
        let loop_var_count = total_inputs - 2; // minus cursor_state, output_state

        // Emit copies for initial loop var values → body region args.
        for i in 0..loop_var_count {
            let input = &node.inputs[i];
            if input.kind == PortKind::Data {
                let src_vreg = self.resolve_vreg(input.source);
                if let Some(dst_vreg) = body_region.args[i].vreg
                    && src_vreg != dst_vreg
                {
                    self.emit(LinearOp::Copy {
                        dst: dst_vreg,
                        src: src_vreg,
                    });
                }
            }
        }

        // Loop top label.
        let loop_top = self.fresh_label();
        let loop_exit = self.fresh_label();
        self.emit(LinearOp::Label(loop_top));

        // Linearize the body.
        self.linearize_region(body);

        // Body results: [predicate, loop_vars..., cursor_state, output_state]
        // predicate: 0 = exit, nonzero = continue
        let predicate_source = body_region.results[0].source;
        let predicate_vreg = self.resolve_vreg(predicate_source);

        // Emit copies for body results → body region args (feedback).
        // Results[1..1+loop_var_count] → args[0..loop_var_count]
        for i in 0..loop_var_count {
            let result = &body_region.results[i + 1]; // +1 to skip predicate
            if result.kind == PortKind::Data {
                let src_vreg = self.resolve_vreg(result.source);
                if let Some(dst_vreg) = body_region.args[i].vreg
                    && src_vreg != dst_vreg
                {
                    self.emit(LinearOp::Copy {
                        dst: dst_vreg,
                        src: src_vreg,
                    });
                }
            }
        }

        // Branch back to loop top if predicate is nonzero.
        self.emit(LinearOp::BranchIf {
            cond: predicate_vreg,
            target: loop_top,
        });

        self.emit(LinearOp::Label(loop_exit));

        // Emit copies for final loop var values → theta output vregs.
        // After the loop exits, the body's region args hold the final values
        // (or the body results, depending on convention). We use body args
        // since the feedback copies already wrote there.
        for i in 0..loop_var_count {
            if let Some(src_vreg) = body_region.args[i].vreg
                && let Some(dst_vreg) = node.outputs[i].vreg
                && src_vreg != dst_vreg
            {
                self.emit(LinearOp::Copy {
                    dst: dst_vreg,
                    src: src_vreg,
                });
            }
        }
    }

    // ─── Lambda ──────────────────────────────────────────────────────

    fn linearize_lambda(
        &mut self,
        body: RegionId,
        shape: &'static facet::Shape,
        lambda_id: LambdaId,
    ) {
        self.emit(LinearOp::FuncStart { lambda_id, shape });
        self.linearize_region(body);
        self.emit(LinearOp::FuncEnd);
    }

    // ─── Apply ───────────────────────────────────────────────────────

    fn linearize_apply(&mut self, node_id: NodeId, target: LambdaId) {
        let node = &self.func.nodes[node_id];
        let args: Vec<VReg> = node
            .inputs
            .iter()
            .filter(|i| i.kind == PortKind::Data)
            .map(|i| self.resolve_vreg(i.source))
            .collect();
        let results: Vec<VReg> = node
            .outputs
            .iter()
            .filter(|o| o.kind == PortKind::Data)
            .filter_map(|o| o.vreg)
            .collect();
        self.emit(LinearOp::CallLambda {
            target,
            args,
            results,
        });
    }
}

/// A lightweight enum mirroring NodeKind but owning the data needed
/// for linearization (avoids borrow issues with self.func).
enum NodeKindRef<'a> {
    Simple(&'a IrOp),
    Gamma {
        regions: Vec<RegionId>,
    },
    Theta {
        body: RegionId,
    },
    Lambda {
        body: RegionId,
        shape: &'static facet::Shape,
        lambda_id: LambdaId,
    },
    Apply {
        target: LambdaId,
    },
}

impl NodeKind {
    fn clone_for_linearize(&self) -> NodeKindRef<'_> {
        match self {
            NodeKind::Simple(op) => NodeKindRef::Simple(op),
            NodeKind::Gamma { regions } => NodeKindRef::Gamma {
                regions: regions.clone(),
            },
            NodeKind::Theta { body } => NodeKindRef::Theta { body: *body },
            NodeKind::Lambda {
                body,
                shape,
                lambda_id,
            } => NodeKindRef::Lambda {
                body: *body,
                shape,
                lambda_id: *lambda_id,
            },
            NodeKind::Apply { target } => NodeKindRef::Apply { target: *target },
        }
    }
}

// ─── VReg assignment pass ────────────────────────────────────────────────────

/// Assign VRegs to all data output ports and region args that don't have one.
fn assign_vregs(func: &mut IrFunc) {
    // Assign to all node output ports.
    let node_count = func.nodes.len();
    for i in 0..node_count {
        let node_id = NodeId::new(i as u32);
        let output_count = func.nodes[node_id].outputs.len();
        for j in 0..output_count {
            if func.nodes[node_id].outputs[j].kind == PortKind::Data
                && func.nodes[node_id].outputs[j].vreg.is_none()
            {
                let vreg = func.fresh_vreg();
                func.nodes[node_id].outputs[j].vreg = Some(vreg);
            }
        }
    }

    // Assign to all region args.
    let region_count = func.regions.len();
    for i in 0..region_count {
        let region_id = RegionId::new(i as u32);
        let arg_count = func.regions[region_id].args.len();
        for j in 0..arg_count {
            if func.regions[region_id].args[j].kind == PortKind::Data
                && func.regions[region_id].args[j].vreg.is_none()
            {
                let vreg = func.fresh_vreg();
                func.regions[region_id].args[j].vreg = Some(vreg);
            }
        }
    }
}

// ─── Public API ──────────────────────────────────────────────────────────────

/// Linearize an RVSDG function into a flat instruction sequence.
pub fn linearize(func: &mut IrFunc) -> LinearIr {
    // Pass 1: ensure all data ports have VRegs.
    assign_vregs(func);

    // Pass 2: walk the RVSDG and emit linear ops.
    let mut lin = Linearizer::new(func);
    lin.linearize_node(func.root);

    LinearIr {
        ops: lin.ops,
        label_count: lin.label_count,
        vreg_count: func.vreg_count(),
        slot_count: func.slot_count(),
    }
}

// ─── Display ─────────────────────────────────────────────────────────────────

impl fmt::Display for LinearIr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for op in &self.ops {
            // Labels get no indentation, everything else gets 2 spaces.
            match op {
                LinearOp::Label(label) => {
                    writeln!(f, "L{}:", label.index())?;
                }
                LinearOp::FuncStart { lambda_id, shape } => {
                    writeln!(
                        f,
                        "func λ{} ({}):",
                        lambda_id.index(),
                        shape.type_identifier
                    )?;
                }
                LinearOp::FuncEnd => {
                    writeln!(f, "end")?;
                }
                _ => {
                    write!(f, "  ")?;
                    fmt_op(f, op)?;
                    writeln!(f)?;
                }
            }
        }
        Ok(())
    }
}

fn fmt_vreg(f: &mut fmt::Formatter<'_>, v: VReg) -> fmt::Result {
    write!(f, "v{}", v.index())
}

fn fmt_op(f: &mut fmt::Formatter<'_>, op: &LinearOp) -> fmt::Result {
    match op {
        LinearOp::Const { dst, value } => {
            fmt_vreg(f, *dst)?;
            write!(f, " = const {value}")
        }
        LinearOp::BinOp { op, dst, lhs, rhs } => {
            fmt_vreg(f, *dst)?;
            write!(f, " = ")?;
            fmt_vreg(f, *lhs)?;
            write!(f, " {op:?} ")?;
            fmt_vreg(f, *rhs)
        }
        LinearOp::UnaryOp { op, dst, src } => {
            fmt_vreg(f, *dst)?;
            write!(f, " = {op:?} ")?;
            fmt_vreg(f, *src)
        }
        LinearOp::Copy { dst, src } => {
            fmt_vreg(f, *dst)?;
            write!(f, " = copy ")?;
            fmt_vreg(f, *src)
        }
        LinearOp::BoundsCheck { count } => write!(f, "bounds_check {count}"),
        LinearOp::ReadBytes { dst, count } => {
            fmt_vreg(f, *dst)?;
            write!(f, " = read_bytes {count}")
        }
        LinearOp::PeekByte { dst } => {
            fmt_vreg(f, *dst)?;
            write!(f, " = peek_byte")
        }
        LinearOp::AdvanceCursor { count } => write!(f, "advance {count}"),
        LinearOp::AdvanceCursorBy { src } => {
            write!(f, "advance_by ")?;
            fmt_vreg(f, *src)
        }
        LinearOp::SaveCursor { dst } => {
            fmt_vreg(f, *dst)?;
            write!(f, " = save_cursor")
        }
        LinearOp::RestoreCursor { src } => {
            write!(f, "restore_cursor ")?;
            fmt_vreg(f, *src)
        }
        LinearOp::WriteToField { src, offset, width } => {
            write!(f, "store [{offset}:{width}] ")?;
            fmt_vreg(f, *src)
        }
        LinearOp::ReadFromField { dst, offset, width } => {
            fmt_vreg(f, *dst)?;
            write!(f, " = load [{offset}:{width}]")
        }
        LinearOp::SaveOutPtr { dst } => {
            fmt_vreg(f, *dst)?;
            write!(f, " = save_out_ptr")
        }
        LinearOp::SetOutPtr { src } => {
            write!(f, "set_out_ptr ")?;
            fmt_vreg(f, *src)
        }
        LinearOp::SlotAddr { dst, slot } => {
            fmt_vreg(f, *dst)?;
            write!(f, " = slot_addr {}", slot.index())
        }
        LinearOp::WriteToSlot { slot, src } => {
            write!(f, "slot[{}] = ", slot.index())?;
            fmt_vreg(f, *src)
        }
        LinearOp::ReadFromSlot { dst, slot } => {
            fmt_vreg(f, *dst)?;
            write!(f, " = slot[{}]", slot.index())
        }
        LinearOp::CallIntrinsic {
            func,
            args,
            dst,
            field_offset,
        } => {
            if let Some(d) = dst {
                fmt_vreg(f, *d)?;
                write!(f, " = ")?;
            }
            write!(f, "call_intrinsic {func}(")?;
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                fmt_vreg(f, *a)?;
            }
            write!(f, ") @{field_offset}")
        }
        LinearOp::CallPure { func, args, dst } => {
            fmt_vreg(f, *dst)?;
            write!(f, " = call_pure {func}(")?;
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                fmt_vreg(f, *a)?;
            }
            write!(f, ")")
        }
        LinearOp::Branch(target) => write!(f, "br L{}", target.index()),
        LinearOp::BranchIf { cond, target } => {
            write!(f, "br_if ")?;
            fmt_vreg(f, *cond)?;
            write!(f, " L{}", target.index())
        }
        LinearOp::BranchIfZero { cond, target } => {
            write!(f, "br_zero ")?;
            fmt_vreg(f, *cond)?;
            write!(f, " L{}", target.index())
        }
        LinearOp::JumpTable {
            predicate,
            labels,
            default,
        } => {
            write!(f, "jump_table ")?;
            fmt_vreg(f, *predicate)?;
            write!(f, " [")?;
            for (i, l) in labels.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "L{}", l.index())?;
            }
            write!(f, "] default L{}", default.index())
        }
        LinearOp::ErrorExit { code } => write!(f, "error_exit {code:?}"),
        LinearOp::SimdStringScan { pos, kind } => {
            fmt_vreg(f, *pos)?;
            write!(f, ", ")?;
            fmt_vreg(f, *kind)?;
            write!(f, " = simd_string_scan")
        }
        LinearOp::SimdWhitespaceSkip => write!(f, "simd_whitespace_skip"),
        LinearOp::CallLambda {
            target,
            args,
            results,
        } => {
            if !results.is_empty() {
                for (i, r) in results.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    fmt_vreg(f, *r)?;
                }
                write!(f, " = ")?;
            }
            write!(f, "call λ{}(", target.index())?;
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                fmt_vreg(f, *a)?;
            }
            write!(f, ")")
        }
        // FuncStart/FuncEnd/Label handled in Display for LinearIr
        LinearOp::Label(_) | LinearOp::FuncStart { .. } | LinearOp::FuncEnd => {
            unreachable!("handled in Display for LinearIr")
        }
    }
}

impl fmt::Debug for LinearIr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "LinearIr {{")?;
        writeln!(
            f,
            "  labels: {}, vregs: {}, slots: {}",
            self.label_count, self.vreg_count, self.slot_count
        )?;
        for op in &self.ops {
            writeln!(f, "  {op:?}")?;
        }
        writeln!(f, "}}")
    }
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{IrBuilder, IrOp, Width};

    #[test]
    fn linearize_simple_chain() {
        // BoundsCheck(4) → ReadBytes(4) → WriteToField(offset=0, W4)
        let mut builder = IrBuilder::new(<u32 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            rb.bounds_check(4);
            let data = rb.read_bytes(4);
            rb.write_to_field(data, 0, Width::W4);
            rb.set_results(&[]);
        }
        let mut func = builder.finish();
        let ir = linearize(&mut func);

        // Expected: FuncStart, BoundsCheck(4), ReadBytes(4), WriteToField, FuncEnd
        assert!(matches!(ir.ops[0], LinearOp::FuncStart { .. }));
        assert!(matches!(ir.ops[1], LinearOp::BoundsCheck { count: 4 }));
        assert!(matches!(ir.ops[2], LinearOp::ReadBytes { count: 4, .. }));
        assert!(matches!(
            ir.ops[3],
            LinearOp::WriteToField {
                offset: 0,
                width: Width::W4,
                ..
            }
        ));
        assert!(matches!(ir.ops[4], LinearOp::FuncEnd));
        assert_eq!(ir.ops.len(), 5);
    }

    #[test]
    fn linearize_gamma_two_branches() {
        // Gamma with predicate, 2 branches:
        //   branch 0: const 42 → result
        //   branch 1: const 99 → result
        let mut builder = IrBuilder::new(<u32 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            let pred = rb.const_val(0);
            let results = rb.gamma(pred, &[], 2, |branch_idx, bb| {
                let val = if branch_idx == 0 {
                    bb.const_val(42)
                } else {
                    bb.const_val(99)
                };
                bb.set_results(&[val]);
            });
            assert_eq!(results.len(), 1);
            rb.write_to_field(results[0], 0, Width::W4);
            rb.set_results(&[]);
        }
        let mut func = builder.finish();
        let ir = linearize(&mut func);

        // Verify structure: FuncStart, Const(pred), BranchIfZero, Branch,
        //   Label(0), Const(42), Copy, Branch(merge), Label(1), Const(99), Copy, Label(merge), ...
        let display = format!("{ir}");
        assert!(
            display.contains("br_zero"),
            "should have BranchIfZero for 2-branch gamma:\n{display}"
        );
        assert!(
            display.contains("const 42"),
            "branch 0 should produce 42:\n{display}"
        );
        assert!(
            display.contains("const 99"),
            "branch 1 should produce 99:\n{display}"
        );
    }

    #[test]
    fn linearize_theta_loop() {
        // Theta: count down from 5 to 0.
        // loop_var = counter
        // body: counter - 1, predicate = counter > 0
        let mut builder = IrBuilder::new(<u32 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            let init_count = rb.const_val(5);
            let one = rb.const_val(1);
            let _results = rb.theta(&[init_count, one], |bb| {
                let args = bb.region_args(2);
                let counter = args[0];
                let one = args[1];
                let new_counter = bb.binop(IrOp::Sub, counter, one);
                // predicate = new_counter (0=exit)
                bb.set_results(&[new_counter, new_counter, one]);
            });
            rb.set_results(&[]);
        }
        let mut func = builder.finish();
        let ir = linearize(&mut func);

        let display = format!("{ir}");
        assert!(
            display.contains("br_if"),
            "should have BranchIf back-edge:\n{display}"
        );
        assert!(
            display.contains("Sub"),
            "should have subtraction:\n{display}"
        );
    }

    #[test]
    fn linearize_call_intrinsic() {
        use crate::intrinsics;
        use crate::ir::IntrinsicFn;

        let mut builder = IrBuilder::new(<bool as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            rb.bounds_check(1);
            rb.call_intrinsic(
                IntrinsicFn(intrinsics::fad_read_bool as *const () as usize),
                &[],
                0,
                false,
            );
            rb.set_results(&[]);
        }
        let mut func = builder.finish();
        let ir = linearize(&mut func);

        let has_call = ir
            .ops
            .iter()
            .any(|op| matches!(op, LinearOp::CallIntrinsic { .. }));
        assert!(has_call, "should contain CallIntrinsic");
    }

    #[test]
    fn linearize_display() {
        let mut builder = IrBuilder::new(<u32 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            rb.bounds_check(4);
            let data = rb.read_bytes(4);
            rb.write_to_field(data, 0, Width::W4);
            rb.set_results(&[]);
        }
        let mut func = builder.finish();
        let ir = linearize(&mut func);

        let display = format!("{ir}");
        assert!(
            display.contains("func"),
            "display should start with func:\n{display}"
        );
        assert!(
            display.contains("bounds_check 4"),
            "display should contain bounds_check:\n{display}"
        );
        assert!(
            display.contains("read_bytes 4"),
            "display should contain read_bytes:\n{display}"
        );
        assert!(
            display.contains("store [0:W4]"),
            "display should contain store:\n{display}"
        );
        assert!(
            display.contains("end"),
            "display should end with end:\n{display}"
        );
    }
}
