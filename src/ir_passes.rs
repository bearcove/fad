use std::collections::{HashMap, HashSet};

use crate::ir::{
    InputPort, IrFunc, LambdaId, Node, NodeId, NodeKind, OutputRef, PortSource, Region,
    RegionArgRef, RegionId, RegionResult,
};

const MAX_INLINE_NODES_SINGLE_USE: usize = 256;
const MAX_INLINE_NODES_MULTI_USE: usize = 64;
const MAX_INLINE_CALL_SITES_MULTI_USE: usize = 4;

// r[impl ir.passes]
pub fn run_default_passes(func: &mut IrFunc) {
    inline_apply_pass(func);
}

fn inline_apply_pass(func: &mut IrFunc) {
    loop {
        let live_nodes = collect_live_nodes(func);
        let lambda_owner = build_region_owner_map(func);
        let call_sites = count_apply_call_sites(func, &live_nodes);
        let lambda_sizes = lambda_sizes(func);
        let mut changed = false;

        let candidates: Vec<NodeId> = func
            .nodes
            .iter()
            .filter_map(|(nid, node)| {
                if !live_nodes.contains(&nid) {
                    return None;
                }
                let NodeKind::Apply { target } = node.kind else {
                    return None;
                };
                let caller_lambda = *lambda_owner
                    .get(&node.region)
                    .expect("every region should belong to exactly one lambda");
                if should_inline(caller_lambda, target, &call_sites, &lambda_sizes) {
                    Some(nid)
                } else {
                    None
                }
            })
            .collect();

        for apply in candidates {
            if inline_one_apply(func, apply) {
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }
}

fn should_inline(
    caller_lambda: LambdaId,
    target: LambdaId,
    call_sites: &HashMap<LambdaId, usize>,
    lambda_sizes: &HashMap<LambdaId, usize>,
) -> bool {
    if caller_lambda == target {
        return false;
    }
    let size = lambda_sizes.get(&target).copied().unwrap_or(0);
    let uses = call_sites.get(&target).copied().unwrap_or(0);

    if uses <= 1 {
        size <= MAX_INLINE_NODES_SINGLE_USE
    } else {
        size <= MAX_INLINE_NODES_MULTI_USE && uses <= MAX_INLINE_CALL_SITES_MULTI_USE
    }
}

fn count_apply_call_sites(func: &IrFunc, live_nodes: &HashSet<NodeId>) -> HashMap<LambdaId, usize> {
    let mut out = HashMap::new();
    for (nid, node) in func.nodes.iter() {
        if !live_nodes.contains(&nid) {
            continue;
        }
        if let NodeKind::Apply { target } = node.kind {
            *out.entry(target).or_insert(0) += 1;
        }
    }
    out
}

fn lambda_sizes(func: &IrFunc) -> HashMap<LambdaId, usize> {
    let mut out = HashMap::new();
    for (idx, node_id) in func.lambdas.iter().copied().enumerate() {
        let lambda = LambdaId::new(idx as u32);
        let body = match &func.nodes[node_id].kind {
            NodeKind::Lambda { body, .. } => *body,
            _ => unreachable!("lambda registry must only contain lambda nodes"),
        };
        out.insert(lambda, count_region_nodes_recursive(func, body));
    }
    out
}

fn count_region_nodes_recursive(func: &IrFunc, region: RegionId) -> usize {
    let mut total = 0usize;
    let mut stack = vec![region];
    while let Some(rid) = stack.pop() {
        let reg = &func.regions[rid];
        total += reg.nodes.len();
        for &nid in &reg.nodes {
            match &func.nodes[nid].kind {
                NodeKind::Gamma { regions } => {
                    for &sub in regions {
                        stack.push(sub);
                    }
                }
                NodeKind::Theta { body } => stack.push(*body),
                _ => {}
            }
        }
    }
    total
}

fn build_region_owner_map(func: &IrFunc) -> HashMap<RegionId, LambdaId> {
    let mut out = HashMap::new();
    for (idx, node_id) in func.lambdas.iter().copied().enumerate() {
        let lambda = LambdaId::new(idx as u32);
        let body = match &func.nodes[node_id].kind {
            NodeKind::Lambda { body, .. } => *body,
            _ => unreachable!("lambda registry must only contain lambda nodes"),
        };
        collect_region_owners(func, body, lambda, &mut out);
    }
    out
}

fn collect_live_nodes(func: &IrFunc) -> HashSet<NodeId> {
    let mut live_regions = HashSet::new();
    let mut stack = Vec::new();
    for node_id in func.lambdas.iter().copied() {
        if let NodeKind::Lambda { body, .. } = &func.nodes[node_id].kind {
            stack.push(*body);
        }
    }

    let mut live_nodes = HashSet::new();
    while let Some(region) = stack.pop() {
        if !live_regions.insert(region) {
            continue;
        }
        for &nid in &func.regions[region].nodes {
            live_nodes.insert(nid);
            match &func.nodes[nid].kind {
                NodeKind::Gamma { regions } => {
                    for &sub in regions {
                        stack.push(sub);
                    }
                }
                NodeKind::Theta { body } => stack.push(*body),
                _ => {}
            }
        }
    }
    live_nodes
}

fn collect_region_owners(
    func: &IrFunc,
    start: RegionId,
    owner: LambdaId,
    out: &mut HashMap<RegionId, LambdaId>,
) {
    let mut stack = vec![start];
    while let Some(region) = stack.pop() {
        if out.insert(region, owner).is_some() {
            continue;
        }
        for &nid in &func.regions[region].nodes {
            match &func.nodes[nid].kind {
                NodeKind::Gamma { regions } => {
                    for &sub in regions {
                        stack.push(sub);
                    }
                }
                NodeKind::Theta { body } => stack.push(*body),
                _ => {}
            }
        }
    }
}

struct CloneCtx {
    top_old_region: RegionId,
    top_arg_sources: Vec<PortSource>,
    node_map: HashMap<NodeId, NodeId>,
    region_map: HashMap<RegionId, RegionId>,
    top_new_nodes: Vec<NodeId>,
}

fn remap_source(source: PortSource, ctx: &CloneCtx) -> PortSource {
    match source {
        PortSource::Node(out) => {
            let new_node = *ctx
                .node_map
                .get(&out.node)
                .expect("cloned source node should be available");
            PortSource::Node(OutputRef {
                node: new_node,
                index: out.index,
            })
        }
        PortSource::RegionArg(arg) => {
            if arg.region == ctx.top_old_region {
                ctx.top_arg_sources[arg.index as usize]
            } else {
                let region = *ctx
                    .region_map
                    .get(&arg.region)
                    .expect("cloned source region should be available");
                PortSource::RegionArg(RegionArgRef {
                    region,
                    index: arg.index,
                })
            }
        }
    }
}

fn clone_region_into(
    func: &mut IrFunc,
    old_region: RegionId,
    new_region: RegionId,
    ctx: &mut CloneCtx,
) {
    let is_top = old_region == ctx.top_old_region;
    if !is_top {
        ctx.region_map.insert(old_region, new_region);
    }

    let old_nodes = func.regions[old_region].nodes.clone();

    for old_node in old_nodes {
        let old_inputs = func.nodes[old_node].inputs.clone();
        let old_outputs = func.nodes[old_node].outputs.clone();
        let old_kind = match &func.nodes[old_node].kind {
            NodeKind::Simple(op) => NodeKind::Simple(op.clone()),
            NodeKind::Apply { target } => NodeKind::Apply { target: *target },
            NodeKind::Gamma { regions } => NodeKind::Gamma {
                regions: regions.clone(),
            },
            NodeKind::Theta { body } => NodeKind::Theta { body: *body },
            NodeKind::Lambda { .. } => {
                panic!("lambda nodes cannot appear inside lambda body regions");
            }
        };

        let mut remapped_subregions = Vec::new();
        let kind = match old_kind {
            NodeKind::Gamma { regions } => {
                for old_sub in regions {
                    let old_reg = &func.regions[old_sub];
                    let new_sub = func.regions.push(Region {
                        args: old_reg.args.clone(),
                        results: Vec::new(),
                        nodes: Vec::new(),
                    });
                    clone_region_into(func, old_sub, new_sub, ctx);
                    remapped_subregions.push(new_sub);
                }
                NodeKind::Gamma {
                    regions: remapped_subregions,
                }
            }
            NodeKind::Theta { body } => {
                let old_reg = &func.regions[body];
                let new_body = func.regions.push(Region {
                    args: old_reg.args.clone(),
                    results: Vec::new(),
                    nodes: Vec::new(),
                });
                clone_region_into(func, body, new_body, ctx);
                NodeKind::Theta { body: new_body }
            }
            other => other,
        };

        let inputs: Vec<InputPort> = old_inputs
            .into_iter()
            .map(|inp| InputPort {
                kind: inp.kind,
                source: remap_source(inp.source, ctx),
            })
            .collect();

        let new_node = func.nodes.push(Node {
            region: new_region,
            inputs,
            outputs: old_outputs,
            kind,
        });
        ctx.node_map.insert(old_node, new_node);

        if is_top {
            ctx.top_new_nodes.push(new_node);
        } else {
            func.regions[new_region].nodes.push(new_node);
        }
    }

    if !is_top {
        let old_results = func.regions[old_region].results.clone();
        func.regions[new_region].results = old_results
            .into_iter()
            .map(|result| RegionResult {
                kind: result.kind,
                source: remap_source(result.source, ctx),
            })
            .collect();
    }
}

fn replace_output_use(func: &mut IrFunc, from: OutputRef, to: PortSource) {
    let from_src = PortSource::Node(from);
    let node_ids: Vec<NodeId> = func.nodes.iter().map(|(id, _)| id).collect();
    for id in node_ids {
        for inp in &mut func.nodes[id].inputs {
            if inp.source == from_src {
                inp.source = to;
            }
        }
    }

    let region_ids: Vec<RegionId> = func.regions.iter().map(|(id, _)| id).collect();
    for rid in region_ids {
        for result in &mut func.regions[rid].results {
            if result.source == from_src {
                result.source = to;
            }
        }
    }
}

fn inline_one_apply(func: &mut IrFunc, apply: NodeId) -> bool {
    let (caller_region, target, top_arg_sources, output_count) = {
        let node = &func.nodes[apply];
        let NodeKind::Apply { target } = node.kind else {
            return false;
        };
        let args: Vec<PortSource> = node.inputs.iter().map(|inp| inp.source).collect();
        (node.region, target, args, node.outputs.len())
    };

    let callee_body = func.lambda_body(target);
    if top_arg_sources.len() != func.regions[callee_body].args.len() {
        panic!(
            "apply argument count mismatch for lambda @{}: got {}, expected {}",
            target.index(),
            top_arg_sources.len(),
            func.regions[callee_body].args.len()
        );
    }

    let mut ctx = CloneCtx {
        top_old_region: callee_body,
        top_arg_sources,
        node_map: HashMap::new(),
        region_map: HashMap::new(),
        top_new_nodes: Vec::new(),
    };
    clone_region_into(func, callee_body, caller_region, &mut ctx);

    let mapped_results: Vec<PortSource> = func.regions[callee_body]
        .results
        .iter()
        .map(|result| remap_source(result.source, &ctx))
        .collect();
    if mapped_results.len() != output_count {
        panic!(
            "apply output count mismatch for lambda @{}: got {}, expected {}",
            target.index(),
            output_count,
            mapped_results.len()
        );
    }

    for (idx, source) in mapped_results.into_iter().enumerate() {
        replace_output_use(
            func,
            OutputRef {
                node: apply,
                index: idx as u16,
            },
            source,
        );
    }

    let Some(pos) = func.regions[caller_region]
        .nodes
        .iter()
        .position(|&n| n == apply)
    else {
        return false;
    };
    let mut new_nodes = Vec::with_capacity(
        func.regions[caller_region].nodes.len() + ctx.top_new_nodes.len().saturating_sub(1),
    );
    new_nodes.extend_from_slice(&func.regions[caller_region].nodes[..pos]);
    new_nodes.extend(ctx.top_new_nodes);
    new_nodes.extend_from_slice(&func.regions[caller_region].nodes[pos + 1..]);
    func.regions[caller_region].nodes = new_nodes;

    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{IrBuilder, NodeKind, Width};

    #[test]
    fn inlines_simple_apply() {
        let mut builder = IrBuilder::new(<u32 as facet::Facet>::SHAPE);
        let child = builder.create_lambda(<u8 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.lambda_region(child);
            rb.bounds_check(1);
            let b = rb.read_bytes(1);
            rb.write_to_field(b, 0, Width::W1);
            rb.set_results(&[]);
        }
        {
            let mut rb = builder.root_region();
            let _ = rb.apply(child, &[], 0);
            rb.set_results(&[]);
        }
        let mut func = builder.finish();

        let before_live = collect_live_nodes(&func);
        let before_apply = func
            .nodes
            .iter()
            .filter(|(id, n)| before_live.contains(id) && matches!(n.kind, NodeKind::Apply { .. }))
            .count();
        assert_eq!(before_apply, 1);

        run_default_passes(&mut func);

        let after_live = collect_live_nodes(&func);
        let after_apply = func
            .nodes
            .iter()
            .filter(|(id, n)| after_live.contains(id) && matches!(n.kind, NodeKind::Apply { .. }))
            .count();
        assert_eq!(after_apply, 0);
    }

    #[test]
    fn does_not_inline_recursive_backedge() {
        let mut builder = IrBuilder::new(<u8 as facet::Facet>::SHAPE);
        {
            let mut rb = builder.root_region();
            let _ = rb.apply(LambdaId::new(0), &[], 0);
            rb.set_results(&[]);
        }
        let mut func = builder.finish();

        run_default_passes(&mut func);

        let live = collect_live_nodes(&func);
        let apply_count = func
            .nodes
            .iter()
            .filter(|(id, n)| live.contains(id) && matches!(n.kind, NodeKind::Apply { .. }))
            .count();
        assert_eq!(apply_count, 1);
    }
}
