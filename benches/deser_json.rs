use divan::{Bencher, black_box};
use facet::Facet;
use std::sync::LazyLock;

fn main() {
    divan::main();
}

// ── Shared types ────────────────────────────────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct FriendSerde {
    age: u32,
    name: String,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct FriendFacet {
    age: u32,
    name: String,
}

// ── Encoded test data ───────────────────────────────────────────────────────

static JSON_INPUT: &[u8] = br#"{"age": 42, "name": "Alice"}"#;

// ── Cached compiled deserializers (compilation cost amortized) ───────────────

static FAD_DESER: LazyLock<fad::compiler::CompiledDeser> = LazyLock::new(|| {
    fad::compile_deser(FriendFacet::SHAPE, &fad::json::FadJson)
});

static FACET_JSON_JIT_HANDLE: LazyLock<
    facet_format::jit::CompiledFormatDeserializer<
        FriendFacet,
        facet_json::JsonParser<'static>,
    >,
> = LazyLock::new(|| {
    facet_format::jit::get_format_deserializer()
        .expect("FriendFacet should be Tier-2 compatible")
});

// ── Benchmarks ──────────────────────────────────────────────────────────────

mod flat_struct {
    use super::*;

    /// serde_json — the incumbent
    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<FriendSerde>(black_box(JSON_INPUT)).unwrap())
        });
    }

    /// facet-json tier-0 (reflection-based, no JIT)
    #[divan::bench]
    fn facet_json_tier0(bencher: Bencher) {
        bencher.bench(|| {
            black_box(facet_json::from_slice::<FriendFacet>(black_box(JSON_INPUT)).unwrap())
        });
    }

    /// facet-json tier-2 JIT (Cranelift-compiled, cached handle)
    #[divan::bench]
    fn facet_json_jit2(bencher: Bencher) {
        let handle = &*FACET_JSON_JIT_HANDLE;
        bencher.bench(|| {
            let mut parser = facet_json::JsonParser::new(black_box(JSON_INPUT));
            black_box(handle.deserialize(&mut parser).unwrap())
        });
    }

    /// fad (dynasmrt JIT, compiled once, cached)
    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_DESER;
        bencher.bench(|| {
            black_box(fad::deserialize::<FriendFacet>(deser, black_box(JSON_INPUT)).unwrap())
        });
    }
}
