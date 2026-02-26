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

static ENCODED: LazyLock<Vec<u8>> = LazyLock::new(|| {
    postcard::to_allocvec(&FriendSerde {
        age: 42,
        name: "Alice".into(),
    })
    .unwrap()
});

// ── Cached compiled deserializers (compilation cost amortized) ───────────────

static FAD_DESER: LazyLock<fad::compiler::CompiledDeser> = LazyLock::new(|| {
    fad::compile_deser(FriendFacet::SHAPE, &fad::postcard::FadPostcard)
});

static FACET_JIT_HANDLE: LazyLock<
    facet_format::jit::CompiledFormatDeserializer<
        FriendFacet,
        facet_postcard::PostcardParser<'static>,
    >,
> = LazyLock::new(|| {
    facet_format::jit::get_format_deserializer()
        .expect("FriendFacet should be Tier-2 compatible")
});

// ── Benchmarks ──────────────────────────────────────────────────────────────

mod flat_struct {
    use super::*;

    /// postcard crate (serde-based) — the incumbent
    #[divan::bench]
    fn postcard_serde(bencher: Bencher) {
        let data = &*ENCODED;
        bencher.bench(|| {
            black_box(postcard::from_bytes::<FriendSerde>(black_box(data)).unwrap())
        });
    }

    /// facet-postcard tier-0 (reflection-based, no JIT)
    #[divan::bench]
    fn facet_postcard_tier0(bencher: Bencher) {
        let data = &*ENCODED;
        bencher.bench(|| {
            black_box(facet_postcard::from_slice::<FriendFacet>(black_box(data)).unwrap())
        });
    }

    /// facet-postcard tier-2 JIT (Cranelift-compiled, cached handle)
    #[divan::bench]
    fn facet_postcard_jit2(bencher: Bencher) {
        let data = &*ENCODED;
        let handle = &*FACET_JIT_HANDLE;
        bencher.bench(|| {
            let mut parser = facet_postcard::PostcardParser::new(black_box(data));
            black_box(handle.deserialize(&mut parser).unwrap())
        });
    }

    /// fad (dynasmrt JIT, compiled once, cached)
    #[divan::bench]
    fn fad(bencher: Bencher) {
        let data = &*ENCODED;
        let deser = &*FAD_DESER;
        bencher.bench(|| {
            black_box(fad::deserialize::<FriendFacet>(deser, black_box(data)).unwrap())
        });
    }
}
