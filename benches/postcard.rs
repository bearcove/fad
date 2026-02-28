use divan::{Bencher, black_box};
use facet::Facet;
use std::sync::LazyLock;

fn main() {
    divan::main();
}

// ── Benchmark macro ─────────────────────────────────────────────────────────
//
// postcard_bench!(group_name, Type, value_expr)         — deser only
// postcard_bench!(group_name, Type, value_expr, +ser)   — deser + ser

macro_rules! postcard_bench {
    ($name:ident, $Type:ty, $value:expr) => {
        #[divan::bench_group]
        mod $name {
            use super::*;
            use std::sync::LazyLock;

            fn make_value() -> $Type { $value }

            static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
                ::postcard::to_allocvec(&make_value()).unwrap()
            });

            static DECODER: LazyLock<fad::compiler::CompiledDecoder> = LazyLock::new(|| {
                fad::compile_decoder(<$Type>::SHAPE, &fad::postcard::FadPostcard)
            });

            postcard_bench!(@deser $Type);
        }
    };

    ($name:ident, $Type:ty, $value:expr, +ser) => {
        #[divan::bench_group]
        mod $name {
            use super::*;
            use std::sync::LazyLock;

            fn make_value() -> $Type { $value }

            static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
                ::postcard::to_allocvec(&make_value()).unwrap()
            });

            static DECODER: LazyLock<fad::compiler::CompiledDecoder> = LazyLock::new(|| {
                fad::compile_decoder(<$Type>::SHAPE, &fad::postcard::FadPostcard)
            });

            static ENCODER: LazyLock<fad::compiler::CompiledEncoder> = LazyLock::new(|| {
                fad::compile_encoder(<$Type>::SHAPE, &fad::postcard::FadPostcard)
            });

            postcard_bench!(@deser $Type);
            postcard_bench!(@ser $Type);
        }
    };

    (@deser $Type:ty) => {
        #[divan::bench]
        fn serde_deser(bencher: Bencher) {
            let data = &*DATA;
            bencher.bench(|| {
                black_box(::postcard::from_bytes::<$Type>(black_box(data)).unwrap())
            });
        }

        #[divan::bench]
        fn facet_deser(bencher: Bencher) {
            let data = &*DATA;
            bencher.bench(|| {
                black_box(facet_postcard::from_slice::<$Type>(black_box(data)).unwrap())
            });
        }

        #[divan::bench]
        fn fad_deser(bencher: Bencher) {
            let data = &*DATA;
            let deser = &*DECODER;
            bencher.bench(|| {
                black_box(fad::deserialize::<$Type>(deser, black_box(data)).unwrap())
            });
        }
    };

    (@ser $Type:ty) => {
        #[divan::bench]
        fn serde_ser(bencher: Bencher) {
            let val = make_value();
            bencher.bench(|| {
                black_box(::postcard::to_allocvec(black_box(&val)).unwrap())
            });
        }

        #[divan::bench]
        fn fad_ser(bencher: Bencher) {
            let val = make_value();
            let enc = &*ENCODER;
            bencher.bench(|| {
                black_box(fad::serialize(enc, black_box(&val)))
            });
        }
    };
}

// ── Types ───────────────────────────────────────────────────────────────────

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Friend {
    age: u32,
    name: String,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Address {
    city: String,
    zip: u32,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Person {
    name: String,
    age: u32,
    address: Address,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Inner {
    x: u32,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Middle {
    inner: Inner,
    y: u32,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Outer {
    middle: Middle,
    z: u32,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct ManyStrings {
    first: String,
    last: String,
    email: String,
    city: String,
    country: String,
    bio: String,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct AllIntegers {
    a_u8: u8,
    a_u16: u16,
    a_u32: u32,
    a_u64: u64,
    a_i8: i8,
    a_i16: i16,
    a_i32: i32,
    a_i64: i64,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Floats {
    a_f32: f32,
    a_f64: f64,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct BoolField {
    value: bool,
}

// ── Enums ───────────────────────────────────────────────────────────────────

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
#[repr(u8)]
enum Animal {
    Cat,
    Dog { name: String, good_boy: bool },
    Parrot(String),
}

// ── Flatten ─────────────────────────────────────────────────────────────────
// serde+postcard can't do flatten (#[serde(flatten)] calls deserialize_any,
// which postcard returns WontImplement from). fad handles it fine.

#[derive(Debug, PartialEq, Facet)]
struct Metadata {
    version: u32,
    author: String,
}

#[derive(Debug, PartialEq, Facet)]
struct DocumentFacet {
    title: String,
    #[facet(flatten)]
    meta: Metadata,
}

// Equivalent flat struct for producing the test bytes.
#[derive(serde::Serialize)]
struct DocumentFlat {
    title: String,
    version: u32,
    author: String,
}

// ── Collections ─────────────────────────────────────────────────────────────

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct ScalarVec {
    values: Vec<u32>,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct StructVec {
    friends: Vec<Friend>,
}

// ── Struct benchmarks (deser + ser) ─────────────────────────────────────────

postcard_bench!(flat_struct, Friend, Friend {
    age: 42,
    name: "Alice".into(),
}, +ser);

postcard_bench!(nested_struct, Person, Person {
    name: "Alice".into(),
    age: 30,
    address: Address { city: "Portland".into(), zip: 97201 },
}, +ser);

postcard_bench!(deep_struct, Outer, Outer {
    middle: Middle { inner: Inner { x: 1 }, y: 2 },
    z: 3,
}, +ser);

postcard_bench!(many_strings, ManyStrings, ManyStrings {
    first: "Alice".into(),
    last: "Johnson".into(),
    email: "alice@example.com".into(),
    city: "Portland".into(),
    country: "USA".into(),
    bio: "Software engineer".into(),
}, +ser);

// ── Scalar benchmarks (deser + ser) ─────────────────────────────────────────

postcard_bench!(all_integers, AllIntegers, AllIntegers {
    a_u8: 255,
    a_u16: 65535,
    a_u32: 1_000_000,
    a_u64: 1_000_000_000_000,
    a_i8: -128,
    a_i16: -32768,
    a_i32: -1_000_000,
    a_i64: -1_000_000_000_000,
}, +ser);

postcard_bench!(floats, Floats, Floats {
    a_f32: 3.14,
    a_f64: 2.718281828459045,
}, +ser);

postcard_bench!(bool_field, BoolField, BoolField {
    value: true,
}, +ser);

// ── Enum benchmark (deser only — encoder Phase 1 doesn't support enums) ─────

postcard_bench!(enum_struct_variant, Animal, Animal::Dog {
    name: "Rex".into(),
    good_boy: true,
});

// ── Flatten benchmark (deser only, fad-only — serde+postcard can't flatten) ─

#[divan::bench_group]
mod flatten {
    use super::*;

    static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
        ::postcard::to_allocvec(&DocumentFlat {
            title: "Hello".into(),
            version: 1,
            author: "Amos".into(),
        }).unwrap()
    });

    static DECODER: LazyLock<fad::compiler::CompiledDecoder> = LazyLock::new(|| {
        fad::compile_decoder(DocumentFacet::SHAPE, &fad::postcard::FadPostcard)
    });

    #[divan::bench]
    fn fad_deser(bencher: Bencher) {
        let data = &*DATA;
        let deser = &*DECODER;
        bencher.bench(|| {
            black_box(fad::deserialize::<DocumentFacet>(deser, black_box(data)).unwrap())
        });
    }
}

// ── Collection benchmarks (deser only) ──────────────────────────────────────

postcard_bench!(vec_scalar_small, ScalarVec, ScalarVec {
    values: vec![1, 2, 3],
});

postcard_bench!(vec_scalar_medium, ScalarVec, ScalarVec {
    values: (0..100).collect(),
});

postcard_bench!(vec_struct, StructVec, StructVec {
    friends: vec![
        Friend { age: 25, name: "Alice".into() },
        Friend { age: 30, name: "Bob".into() },
        Friend { age: 35, name: "Charlie".into() },
    ],
});
