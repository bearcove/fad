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

// ── Cached compiled deserializer (compilation cost amortized) ───────────────

static FAD_DESER: LazyLock<fad::compiler::CompiledDeser> = LazyLock::new(|| {
    fad::compile_deser(FriendFacet::SHAPE, &fad::json::FadJson)
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

    /// fad (dynasmrt JIT, compiled once, cached)
    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_DESER;
        bencher.bench(|| {
            black_box(fad::deserialize::<FriendFacet>(deser, black_box(JSON_INPUT)).unwrap())
        });
    }
}
