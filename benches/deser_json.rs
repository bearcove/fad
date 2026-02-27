use divan::{Bencher, black_box};
use facet::Facet;
use std::sync::LazyLock;

fn main() {
    divan::main();
}

// ── Shared types: flat ──────────────────────────────────────────────────────

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

// ── Shared types: nested ────────────────────────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct AddressSerde {
    city: String,
    zip: u32,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct PersonSerde {
    name: String,
    age: u32,
    address: AddressSerde,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct AddressFacet {
    city: String,
    zip: u32,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct PersonFacet {
    name: String,
    age: u32,
    address: AddressFacet,
}

// ── Shared types: deeply nested ─────────────────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct InnerSerde {
    x: u32,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct MiddleSerde {
    inner: InnerSerde,
    y: u32,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct OuterSerde {
    middle: MiddleSerde,
    z: u32,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct InnerFacet {
    x: u32,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct MiddleFacet {
    inner: InnerFacet,
    y: u32,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct OuterFacet {
    middle: MiddleFacet,
    z: u32,
}

// ── Encoded test data ───────────────────────────────────────────────────────

static FLAT_JSON: &[u8] = br#"{"age": 42, "name": "Alice"}"#;

static NESTED_JSON: &[u8] =
    br#"{"name": "Alice", "age": 30, "address": {"city": "Portland", "zip": 97201}}"#;

static DEEP_JSON: &[u8] = br#"{"middle": {"inner": {"x": 1}, "y": 2}, "z": 3}"#;

// ── Cached compiled deserializers ───────────────────────────────────────────

static FAD_FLAT: LazyLock<fad::compiler::CompiledDeser> = LazyLock::new(|| {
    fad::compile_deser(FriendFacet::SHAPE, &fad::json::FadJson)
});

static FAD_NESTED: LazyLock<fad::compiler::CompiledDeser> = LazyLock::new(|| {
    fad::compile_deser(PersonFacet::SHAPE, &fad::json::FadJson)
});

static FAD_DEEP: LazyLock<fad::compiler::CompiledDeser> = LazyLock::new(|| {
    fad::compile_deser(OuterFacet::SHAPE, &fad::json::FadJson)
});

static FACET_JSON_JIT_FLAT: LazyLock<
    facet_format::jit::CompiledFormatDeserializer<
        FriendFacet,
        facet_json::JsonParser<'static>,
    >,
> = LazyLock::new(|| {
    facet_format::jit::get_format_deserializer()
        .expect("FriendFacet should be Tier-2 compatible")
});

// ── Benchmarks: flat struct ─────────────────────────────────────────────────

mod flat_struct {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<FriendSerde>(black_box(FLAT_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn facet_json_tier0(bencher: Bencher) {
        bencher.bench(|| {
            black_box(facet_json::from_slice::<FriendFacet>(black_box(FLAT_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn facet_json_jit2(bencher: Bencher) {
        let handle = &*FACET_JSON_JIT_FLAT;
        bencher.bench(|| {
            let mut parser = facet_json::JsonParser::new(black_box(FLAT_JSON));
            black_box(handle.deserialize(&mut parser).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_FLAT;
        bencher.bench(|| {
            black_box(fad::deserialize::<FriendFacet>(deser, black_box(FLAT_JSON)).unwrap())
        });
    }
}

// ── Benchmarks: nested struct ───────────────────────────────────────────────

mod nested_struct {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<PersonSerde>(black_box(NESTED_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn facet_json_tier0(bencher: Bencher) {
        bencher.bench(|| {
            black_box(facet_json::from_slice::<PersonFacet>(black_box(NESTED_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_NESTED;
        bencher.bench(|| {
            black_box(fad::deserialize::<PersonFacet>(deser, black_box(NESTED_JSON)).unwrap())
        });
    }
}

// ── Benchmarks: deeply nested struct ────────────────────────────────────────

#[divan::bench_group(sample_size = 65536)]
mod deep_nested_struct {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<OuterSerde>(black_box(DEEP_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn facet_json_tier0(bencher: Bencher) {
        bencher.bench(|| {
            black_box(facet_json::from_slice::<OuterFacet>(black_box(DEEP_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_DEEP;
        bencher.bench(|| {
            black_box(fad::deserialize::<OuterFacet>(deser, black_box(DEEP_JSON)).unwrap())
        });
    }
}
