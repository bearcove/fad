use crate::format::VariantEmitInfo;

// r[impl deser.json.enum.untagged.object-solver]

/// A solver state machine lowered for JIT emission.
///
/// Built at JIT-compile time from the struct variants in an untagged enum's
/// object bucket. The solver narrows a candidate bitmask by ANDing per-key
/// masks as keys are scanned. When popcount == 1, we've identified the variant.
///
/// At runtime this is just a u64 in a stack slot, narrowed by AND with
/// baked-in immediates. No heap, no function calls.
pub struct LoweredSolver {
    /// Number of candidates (object-bucket variants). Max 64.
    pub num_candidates: usize,
    /// Initial candidate bitmask (all candidates set).
    pub initial_mask: u64,
    /// Per-key masks: (field_name, bitmask).
    /// Bit i is set if candidate i has this field.
    /// Unknown keys (not in this list) don't narrow the candidates.
    pub key_masks: Vec<(&'static str, u64)>,
    /// Maps candidate bit position → index into the original variants slice.
    pub candidate_to_variant: Vec<usize>,
}

impl LoweredSolver {
    /// Build a solver from the struct variants in the object bucket.
    ///
    /// `object_variants` is a list of (original_variant_index, variant_info) pairs.
    /// The bit position in the bitmask corresponds to position in this slice.
    pub fn build(object_variants: &[(usize, &VariantEmitInfo)]) -> Self {
        assert!(
            object_variants.len() <= 64,
            "untagged enum object bucket has {} variants, max 64",
            object_variants.len()
        );

        let num_candidates = object_variants.len();
        let initial_mask = if num_candidates == 64 {
            !0u64
        } else {
            (1u64 << num_candidates) - 1
        };

        // Collect all unique field names across all candidates.
        let mut all_names: Vec<&'static str> = Vec::new();
        for (_, variant) in object_variants {
            for field in &variant.fields {
                if !all_names.contains(&field.name) {
                    all_names.push(field.name);
                }
            }
        }
        all_names.sort();

        // Build inverted index: for each field name, which candidates have it.
        let key_masks: Vec<(&'static str, u64)> = all_names
            .into_iter()
            .map(|name| {
                let mut mask = 0u64;
                for (bit, (_, variant)) in object_variants.iter().enumerate() {
                    if variant.fields.iter().any(|f| f.name == name) {
                        mask |= 1u64 << bit;
                    }
                }
                (name, mask)
            })
            .collect();

        let candidate_to_variant: Vec<usize> =
            object_variants.iter().map(|(idx, _)| *idx).collect();

        LoweredSolver {
            num_candidates,
            initial_mask,
            key_masks,
            candidate_to_variant,
        }
    }
}
