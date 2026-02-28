use divan::{Bencher, black_box};
use facet::Facet;
use std::sync::LazyLock;

fn main() {
    divan::main();
}

// ── Benchmark macro ─────────────────────────────────────────────────────────
//
// json_bench!(group_name, Type, value_expr)         — deser only
// json_bench!(group_name, Type, value_expr, +ser)   — deser + ser

macro_rules! json_bench {
    ($name:ident, $Type:ty, $value:expr) => {
        #[divan::bench_group]
        mod $name {
            use super::*;
            use std::sync::LazyLock;

            fn make_value() -> $Type { $value }

            static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
                serde_json::to_vec(&make_value()).unwrap()
            });

            static DECODER: LazyLock<fad::compiler::CompiledDecoder> = LazyLock::new(|| {
                fad::compile_decoder(<$Type>::SHAPE, &fad::json::FadJson)
            });

            json_bench!(@deser $Type);
        }
    };

    ($name:ident, $Type:ty, $value:expr, +ser) => {
        #[divan::bench_group]
        mod $name {
            use super::*;
            use std::sync::LazyLock;

            fn make_value() -> $Type { $value }

            static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
                serde_json::to_vec(&make_value()).unwrap()
            });

            static DECODER: LazyLock<fad::compiler::CompiledDecoder> = LazyLock::new(|| {
                fad::compile_decoder(<$Type>::SHAPE, &fad::json::FadJson)
            });

            static ENCODER: LazyLock<fad::compiler::CompiledEncoder> = LazyLock::new(|| {
                fad::compile_encoder(<$Type>::SHAPE, &fad::json::FadJsonEncoder)
            });

            json_bench!(@deser $Type);
            json_bench!(@ser $Type);
        }
    };

    (@deser $Type:ty) => {
        #[divan::bench]
        fn serde_deser(bencher: Bencher) {
            let data = &*DATA;
            bencher.bench(|| {
                black_box(serde_json::from_slice::<$Type>(black_box(data)).unwrap())
            });
        }

        #[divan::bench]
        fn facet_deser(bencher: Bencher) {
            let data = &*DATA;
            bencher.bench(|| {
                black_box(facet_json::from_slice::<$Type>(black_box(data)).unwrap())
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
                black_box(serde_json::to_vec(black_box(&val)).unwrap())
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
struct LongString {
    content: String,
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

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
#[serde(tag = "type", content = "data")]
#[facet(tag = "type", content = "data")]
#[repr(u8)]
enum AdjAnimal {
    Cat,
    Dog { name: String, good_boy: bool },
    Parrot(String),
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
#[serde(tag = "type")]
#[facet(tag = "type")]
#[repr(u8)]
enum IntAnimal {
    Cat,
    Dog { name: String, good_boy: bool },
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
#[serde(untagged)]
#[facet(untagged)]
#[repr(u8)]
enum Untagged {
    Cat,
    Dog { name: String, good_boy: bool },
    Parrot(String),
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
#[serde(untagged)]
#[facet(untagged)]
#[repr(u8)]
enum ConfigEnum {
    Database { host: String, port: u32 },
    Redis { host: String, db: u32 },
}

// ── Flatten ─────────────────────────────────────────────────────────────────

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Metadata {
    version: u32,
    author: String,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Document {
    title: String,
    #[serde(flatten)]
    #[facet(flatten)]
    meta: Metadata,
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

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct StringVec {
    names: Vec<String>,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct StringBag {
    values: Vec<String>,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct ScalarMap {
    scores: std::collections::HashMap<String, u32>,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct StructMap {
    roster: std::collections::HashMap<String, Friend>,
}

// ── Struct benchmarks (deser + ser) ─────────────────────────────────────────

json_bench!(flat_struct, Friend, Friend {
    age: 42,
    name: "Alice".into(),
}, +ser);

json_bench!(nested_struct, Person, Person {
    name: "Alice".into(),
    age: 30,
    address: Address { city: "Portland".into(), zip: 97201 },
}, +ser);

json_bench!(deep_struct, Outer, Outer {
    middle: Middle { inner: Inner { x: 1 }, y: 2 },
    z: 3,
}, +ser);

json_bench!(many_strings, ManyStrings, ManyStrings {
    first: "Alice".into(),
    last: "Johnson".into(),
    email: "alice@example.com".into(),
    city: "Portland".into(),
    country: "USA".into(),
    bio: "Software engineer".into(),
}, +ser);

json_bench!(long_string, LongString, LongString {
    content: "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.".into(),
}, +ser);

// ── Scalar benchmarks (deser + ser) ─────────────────────────────────────────

json_bench!(all_integers, AllIntegers, AllIntegers {
    a_u8: 255,
    a_u16: 65535,
    a_u32: 1_000_000,
    a_u64: 1_000_000_000_000,
    a_i8: -128,
    a_i16: -32768,
    a_i32: -1_000_000,
    a_i64: -1_000_000_000_000,
}, +ser);

json_bench!(floats, Floats, Floats {
    a_f32: 3.14,
    a_f64: 2.718281828459045,
}, +ser);

json_bench!(bool_field, BoolField, BoolField {
    value: true,
}, +ser);

// ── Enum benchmarks (deser only — encoder Phase 1 doesn't support enums) ──

json_bench!(enum_external, Animal, Animal::Dog {
    name: "Rex".into(),
    good_boy: true,
});

json_bench!(enum_adjacent, AdjAnimal, AdjAnimal::Dog {
    name: "Rex".into(),
    good_boy: true,
});

json_bench!(enum_internal, IntAnimal, IntAnimal::Dog {
    name: "Rex".into(),
    good_boy: true,
});

json_bench!(enum_untagged, Untagged, Untagged::Dog {
    name: "Rex".into(),
    good_boy: true,
});

json_bench!(enum_untagged_solver, ConfigEnum, ConfigEnum::Database {
    host: "localhost".into(),
    port: 5432,
});

// ── Flatten benchmark (deser only) ──────────────────────────────────────────

json_bench!(flatten, Document, Document {
    title: "Hello".into(),
    meta: Metadata { version: 1, author: "Amos".into() },
});

// ── Collection benchmarks (deser only — encoder Phase 1 doesn't support vecs/maps)

json_bench!(vec_scalar_small, ScalarVec, ScalarVec {
    values: vec![1, 2, 3],
});

json_bench!(vec_scalar_medium, ScalarVec, ScalarVec {
    values: (0..100).collect(),
});

json_bench!(vec_struct, StructVec, StructVec {
    friends: vec![
        Friend { age: 25, name: "Alice".into() },
        Friend { age: 30, name: "Bob".into() },
        Friend { age: 35, name: "Charlie".into() },
    ],
});

json_bench!(vec_string, StringVec, StringVec {
    names: vec![
        "alice".into(), "bob".into(), "carol".into(), "dave".into(),
        "eve".into(), "frank".into(), "grace".into(), "henry".into(),
    ],
});

json_bench!(map_small, ScalarMap, ScalarMap {
    scores: [("alice", 42), ("bob", 7), ("carol", 99), ("dave", 15)]
        .into_iter().map(|(k, v)| (k.into(), v)).collect(),
});

json_bench!(map_medium, ScalarMap, ScalarMap {
    scores: (0..16u32).map(|i| (format!("player{i}"), i * 10)).collect(),
});

json_bench!(map_struct, StructMap, StructMap {
    roster: [
        ("alice", Friend { age: 25, name: "Alice".into() }),
        ("bob", Friend { age: 30, name: "Bob".into() }),
        ("carol", Friend { age: 35, name: "Carol".into() }),
    ].into_iter().map(|(k, v)| (k.into(), v)).collect(),
});

// ── String-heavy payload (1024 strings, tests trusted UTF-8 path) ───────────

json_bench!(string_heavy, StringBag, StringBag {
    values: (0..1024)
        .map(|i| format!("user-{i:04}-alpha-beta-gamma-delta-epsilon-zeta-theta-lambda"))
        .collect(),
});

// ── String escaping benchmarks ──────────────────────────────────────────────
// These use hand-crafted JSON with escape sequences, not the macro
// (the value → JSON → deser cycle wouldn't produce escape sequences).

#[divan::bench_group]
mod string_escapes {
    use super::*;

    static ESCAPE_JSON: &[u8] = br#"{"age": 42, "name": "hello\nworld\t\"escaped\"\u0041"}"#;

    static DECODER: LazyLock<fad::compiler::CompiledDecoder> =
        LazyLock::new(|| fad::compile_decoder(Friend::SHAPE, &fad::json::FadJson));

    #[divan::bench]
    fn serde_deser(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<Friend>(black_box(ESCAPE_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad_deser(bencher: Bencher) {
        let deser = &*DECODER;
        bencher.bench(|| {
            black_box(fad::deserialize::<Friend>(deser, black_box(ESCAPE_JSON)).unwrap())
        });
    }
}

#[divan::bench_group]
mod string_heavy_escapes {
    use super::*;

    static ESCAPE_JSON: &[u8] = br#"{"first": "Alice \"Al\" Johnson", "last": "O\u0027Brien", "email": "alice+test@example.com", "city": "New\nYork", "country": "U\u0053A", "bio": "Writes code\t& drinks coffee"}"#;

    static DECODER: LazyLock<fad::compiler::CompiledDecoder> =
        LazyLock::new(|| fad::compile_decoder(ManyStrings::SHAPE, &fad::json::FadJson));

    #[divan::bench]
    fn serde_deser(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<ManyStrings>(black_box(ESCAPE_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad_deser(bencher: Bencher) {
        let deser = &*DECODER;
        bencher.bench(|| {
            black_box(fad::deserialize::<ManyStrings>(deser, black_box(ESCAPE_JSON)).unwrap())
        });
    }
}
