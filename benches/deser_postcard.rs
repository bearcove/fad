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

// ── Shared types: flatten ─────────────────────────────────────────────────
// serde+postcard can't do flatten: #[serde(flatten)] calls deserialize_any,
// which postcard returns WontImplement from (no self-describing type info).
// fad handles this fine since it knows the schema at JIT compile time.

#[derive(facet::Facet, Debug, PartialEq)]
struct MetadataFacet {
    version: u32,
    author: String,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct DocumentFacet {
    title: String,
    #[facet(flatten)]
    meta: MetadataFacet,
}

// ── Shared types: enum ────────────────────────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
#[repr(u8)]
enum AnimalSerde {
    Cat,
    Dog { name: String, good_boy: bool },
    Parrot(String),
}

#[derive(facet::Facet, Debug, PartialEq)]
#[repr(u8)]
enum AnimalFacet {
    Cat,
    Dog { name: String, good_boy: bool },
    Parrot(String),
}

// ── Shared types: vec of scalars ──────────────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct ScalarVecSerde {
    values: Vec<u32>,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct ScalarVecFacet {
    values: Vec<u32>,
}

// ── Shared types: vec of structs ──────────────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct StructVecSerde {
    friends: Vec<FriendSerde>,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct StructVecFacet {
    friends: Vec<FriendFacet>,
}

// ── Encoded test data ───────────────────────────────────────────────────────

static FLAT_ENCODED: LazyLock<Vec<u8>> = LazyLock::new(|| {
    postcard::to_allocvec(&FriendSerde {
        age: 42,
        name: "Alice".into(),
    })
    .unwrap()
});

static NESTED_ENCODED: LazyLock<Vec<u8>> = LazyLock::new(|| {
    postcard::to_allocvec(&PersonSerde {
        name: "Alice".into(),
        age: 30,
        address: AddressSerde {
            city: "Portland".into(),
            zip: 97201,
        },
    })
    .unwrap()
});

static DEEP_ENCODED: LazyLock<Vec<u8>> = LazyLock::new(|| {
    postcard::to_allocvec(&OuterSerde {
        middle: MiddleSerde {
            inner: InnerSerde { x: 1 },
            y: 2,
        },
        z: 3,
    })
    .unwrap()
});

// Equivalent non-flattened struct for producing the test bytes
// (same wire format: fields inlined in declaration order).
#[derive(serde::Serialize)]
struct DocumentSerdeFlat {
    title: String,
    version: u32,
    author: String,
}

static FLATTEN_ENCODED: LazyLock<Vec<u8>> = LazyLock::new(|| {
    postcard::to_allocvec(&DocumentSerdeFlat {
        title: "Hello".into(),
        version: 1,
        author: "Amos".into(),
    })
    .unwrap()
});

static ENUM_ENCODED: LazyLock<Vec<u8>> = LazyLock::new(|| {
    postcard::to_allocvec(&AnimalSerde::Dog {
        name: "Rex".into(),
        good_boy: true,
    })
    .unwrap()
});

static VEC_SCALAR_SMALL_ENCODED: LazyLock<Vec<u8>> = LazyLock::new(|| {
    postcard::to_allocvec(&ScalarVecSerde {
        values: vec![1, 2, 3],
    })
    .unwrap()
});

static VEC_SCALAR_MEDIUM_ENCODED: LazyLock<Vec<u8>> = LazyLock::new(|| {
    postcard::to_allocvec(&ScalarVecSerde {
        values: (0..100).collect(),
    })
    .unwrap()
});

static VEC_STRUCT_ENCODED: LazyLock<Vec<u8>> = LazyLock::new(|| {
    postcard::to_allocvec(&StructVecSerde {
        friends: vec![
            FriendSerde {
                age: 25,
                name: "Alice".into(),
            },
            FriendSerde {
                age: 30,
                name: "Bob".into(),
            },
            FriendSerde {
                age: 35,
                name: "Charlie".into(),
            },
        ],
    })
    .unwrap()
});

// ── Cached compiled deserializers ───────────────────────────────────────────

static FAD_FLAT: LazyLock<fad::compiler::CompiledDeser> =
    LazyLock::new(|| fad::compile_deser(FriendFacet::SHAPE, &fad::postcard::FadPostcard));

static FAD_NESTED: LazyLock<fad::compiler::CompiledDeser> =
    LazyLock::new(|| fad::compile_deser(PersonFacet::SHAPE, &fad::postcard::FadPostcard));

static FAD_DEEP: LazyLock<fad::compiler::CompiledDeser> =
    LazyLock::new(|| fad::compile_deser(OuterFacet::SHAPE, &fad::postcard::FadPostcard));

static FAD_FLATTEN: LazyLock<fad::compiler::CompiledDeser> =
    LazyLock::new(|| fad::compile_deser(DocumentFacet::SHAPE, &fad::postcard::FadPostcard));

static FAD_ENUM: LazyLock<fad::compiler::CompiledDeser> =
    LazyLock::new(|| fad::compile_deser(AnimalFacet::SHAPE, &fad::postcard::FadPostcard));

static FAD_VEC_SCALAR: LazyLock<fad::compiler::CompiledDeser> =
    LazyLock::new(|| fad::compile_deser(ScalarVecFacet::SHAPE, &fad::postcard::FadPostcard));

static FAD_VEC_STRUCT: LazyLock<fad::compiler::CompiledDeser> =
    LazyLock::new(|| fad::compile_deser(StructVecFacet::SHAPE, &fad::postcard::FadPostcard));

static FACET_JIT_FLAT: LazyLock<
    facet_format::jit::CompiledFormatDeserializer<
        FriendFacet,
        facet_postcard::PostcardParser<'static>,
    >,
> = LazyLock::new(|| {
    facet_format::jit::get_format_deserializer().expect("FriendFacet should be Tier-2 compatible")
});

// ── Benchmarks: flat struct ─────────────────────────────────────────────────

mod flat_struct {
    use super::*;

    #[divan::bench]
    fn postcard_serde(bencher: Bencher) {
        let data = &*FLAT_ENCODED;
        bencher.bench(|| black_box(postcard::from_bytes::<FriendSerde>(black_box(data)).unwrap()));
    }

    #[divan::bench]
    fn facet_postcard_tier0(bencher: Bencher) {
        let data = &*FLAT_ENCODED;
        bencher.bench(|| {
            black_box(facet_postcard::from_slice::<FriendFacet>(black_box(data)).unwrap())
        });
    }

    #[divan::bench]
    fn facet_postcard_jit2(bencher: Bencher) {
        let data = &*FLAT_ENCODED;
        let handle = &*FACET_JIT_FLAT;
        bencher.bench(|| {
            let mut parser = facet_postcard::PostcardParser::new(black_box(data));
            black_box(handle.deserialize(&mut parser).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let data = &*FLAT_ENCODED;
        let deser = &*FAD_FLAT;
        bencher
            .bench(|| black_box(fad::deserialize::<FriendFacet>(deser, black_box(data)).unwrap()));
    }
}

// ── Benchmarks: nested struct ───────────────────────────────────────────────

mod nested_struct {
    use super::*;

    #[divan::bench]
    fn postcard_serde(bencher: Bencher) {
        let data = &*NESTED_ENCODED;
        bencher.bench(|| black_box(postcard::from_bytes::<PersonSerde>(black_box(data)).unwrap()));
    }

    #[divan::bench]
    fn facet_postcard_tier0(bencher: Bencher) {
        let data = &*NESTED_ENCODED;
        bencher.bench(|| {
            black_box(facet_postcard::from_slice::<PersonFacet>(black_box(data)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let data = &*NESTED_ENCODED;
        let deser = &*FAD_NESTED;
        bencher
            .bench(|| black_box(fad::deserialize::<PersonFacet>(deser, black_box(data)).unwrap()));
    }
}

// ── Benchmarks: deeply nested struct ────────────────────────────────────────

// sample_size=65536 so that sub-nanosecond benchmarks don't hit the timer
// precision floor (~41ns on Apple Silicon).
#[divan::bench_group(sample_size = 65536)]
mod deep_nested_struct {
    use super::*;

    #[divan::bench]
    fn postcard_serde(bencher: Bencher) {
        let data = &*DEEP_ENCODED;
        bencher.bench(|| black_box(postcard::from_bytes::<OuterSerde>(black_box(data)).unwrap()));
    }

    #[divan::bench]
    fn facet_postcard_tier0(bencher: Bencher) {
        let data = &*DEEP_ENCODED;
        bencher.bench(|| {
            black_box(facet_postcard::from_slice::<OuterFacet>(black_box(data)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let data = &*DEEP_ENCODED;
        let deser = &*FAD_DEEP;
        bencher
            .bench(|| black_box(fad::deserialize::<OuterFacet>(deser, black_box(data)).unwrap()));
    }
}

// ── Benchmarks: flatten ───────────────────────────────────────────────────

mod flatten {
    use super::*;

    // No postcard_serde bench: serde+postcard returns WontImplement for flatten.

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let data = &*FLATTEN_ENCODED;
        let deser = &*FAD_FLATTEN;
        bencher.bench(|| {
            black_box(fad::deserialize::<DocumentFacet>(deser, black_box(data)).unwrap())
        });
    }
}

// ── Benchmarks: enum (struct variant) ─────────────────────────────────────

#[divan::bench_group(sample_size = 65536)]
mod enum_struct_variant {
    use super::*;

    #[divan::bench]
    fn postcard_serde(bencher: Bencher) {
        let data = &*ENUM_ENCODED;
        bencher.bench(|| black_box(postcard::from_bytes::<AnimalSerde>(black_box(data)).unwrap()));
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let data = &*ENUM_ENCODED;
        let deser = &*FAD_ENUM;
        bencher
            .bench(|| black_box(fad::deserialize::<AnimalFacet>(deser, black_box(data)).unwrap()));
    }
}

// ── Benchmarks: Vec<u32> (3 elements) ─────────────────────────────────────

// Vec benchmarks allocate per iteration — force enough iters to amortize
// the ~41ns timer precision on Apple Silicon.
#[divan::bench_group(sample_size = 1000, sample_count = 1000)]
mod vec_scalar_small {
    use super::*;

    #[divan::bench]
    fn postcard_serde(bencher: Bencher) {
        let data = &*VEC_SCALAR_SMALL_ENCODED;
        bencher
            .bench(|| black_box(postcard::from_bytes::<ScalarVecSerde>(black_box(data)).unwrap()));
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let data = &*VEC_SCALAR_SMALL_ENCODED;
        let deser = &*FAD_VEC_SCALAR;
        bencher.bench(|| {
            black_box(fad::deserialize::<ScalarVecFacet>(deser, black_box(data)).unwrap())
        });
    }
}

// ── Benchmarks: Vec<u32> (100 elements) ───────────────────────────────────

#[divan::bench_group(sample_size = 1000, sample_count = 1000)]
mod vec_scalar_medium {
    use super::*;

    #[divan::bench]
    fn postcard_serde(bencher: Bencher) {
        let data = &*VEC_SCALAR_MEDIUM_ENCODED;
        bencher
            .bench(|| black_box(postcard::from_bytes::<ScalarVecSerde>(black_box(data)).unwrap()));
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let data = &*VEC_SCALAR_MEDIUM_ENCODED;
        let deser = &*FAD_VEC_SCALAR;
        bencher.bench(|| {
            black_box(fad::deserialize::<ScalarVecFacet>(deser, black_box(data)).unwrap())
        });
    }
}

// ── Benchmarks: Vec<Friend> (3 structs) ───────────────────────────────────

#[divan::bench_group(sample_size = 1000, sample_count = 1000)]
mod vec_struct {
    use super::*;

    #[divan::bench]
    fn postcard_serde(bencher: Bencher) {
        let data = &*VEC_STRUCT_ENCODED;
        bencher
            .bench(|| black_box(postcard::from_bytes::<StructVecSerde>(black_box(data)).unwrap()));
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let data = &*VEC_STRUCT_ENCODED;
        let deser = &*FAD_VEC_STRUCT;
        bencher.bench(|| {
            black_box(fad::deserialize::<StructVecFacet>(deser, black_box(data)).unwrap())
        });
    }
}
