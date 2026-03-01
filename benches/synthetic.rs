#[path = "harness.rs"]
mod harness;

#[macro_use]
#[path = "common/bench_macros.rs"]
mod bench_macros;

use facet::Facet;
use std::hint::black_box;
use std::sync::LazyLock;

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

// ── Tuples and arrays ────────────────────────────────────────────────────────

type Pair = (u32, String);
type Triple = (u32, u64, f64);

// ── Option wrappers ──────────────────────────────────────────────────────────

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct OptionScalar {
    value: Option<u32>,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct OptionStr {
    value: Option<String>,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct OptionStruct {
    value: Option<Friend>,
}

// ── Enums (JSON only — postcard enum tagging differs) ───────────────────────

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

// ── Flatten (JSON: serde+fad, postcard: fad only) ───────────────────────────

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

// ── Postcard enum (uses plain repr(u8) tagging) ────────────────────────────

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
#[repr(u8)]
enum PostcardAnimal {
    Cat,
    Dog { name: String, good_boy: bool },
    Parrot(String),
}

// ── Flatten helper for postcard (serde can't flatten, so we serialize flat) ─

#[derive(serde::Serialize)]
struct DocumentFlat {
    title: String,
    version: u32,
    author: String,
}

// ═════════════════════════════════════════════════════════════════════════════
// Main — register all benchmarks and run
// ═════════════════════════════════════════════════════════════════════════════

fn main() {
    let mut v: Vec<harness::Bench> = Vec::new();

    // ── Structs (deser + ser, both formats) ─────────────────────────────────

    bench!(v, flat_struct, Friend, Friend {
        age: 42,
        name: "Alice".into(),
    }, +ser, +ir);

    bench!(v, nested_struct, Person, Person {
        name: "Alice".into(),
        age: 30,
        address: Address { city: "Portland".into(), zip: 97201 },
    }, +ser);

    bench!(v, deep_struct, Outer, Outer {
        middle: Middle { inner: Inner { x: 1 }, y: 2 },
        z: 3,
    }, +ser, +ir);

    bench!(v, many_strings, ManyStrings, ManyStrings {
        first: "Alice".into(),
        last: "Johnson".into(),
        email: "alice@example.com".into(),
        city: "Portland".into(),
        country: "USA".into(),
        bio: "Software engineer".into(),
    }, +ser);

    bench!(v, long_string, LongString, LongString {
        content: "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.".into(),
    }, +ser);

    // ── Scalars (deser + ser, both formats) ─────────────────────────────────

    bench!(v, all_integers, AllIntegers, AllIntegers {
        a_u8: 255,
        a_u16: 65535,
        a_u32: 1_000_000,
        a_u64: 1_000_000_000_000,
        a_i8: -128,
        a_i16: -32768,
        a_i32: -1_000_000,
        a_i64: -1_000_000_000_000,
    }, +ser);

    bench!(v, floats, Floats, Floats {
        a_f32: 3.14,
        a_f64: 2.718281828459045,
    }, +ser);

    bench!(v, bool_field, BoolField, BoolField {
        value: true,
    }, +ser);

    // ── Enums (JSON only, deser only) ───────────────────────────────────────

    bench!(
        v,
        enum_external,
        Animal,
        Animal::Dog {
            name: "Rex".into(),
            good_boy: true,
        },
        json_only
    );

    bench!(
        v,
        enum_adjacent,
        AdjAnimal,
        AdjAnimal::Dog {
            name: "Rex".into(),
            good_boy: true,
        },
        json_only
    );

    bench!(
        v,
        enum_internal,
        IntAnimal,
        IntAnimal::Dog {
            name: "Rex".into(),
            good_boy: true,
        },
        json_only
    );

    bench!(
        v,
        enum_untagged,
        Untagged,
        Untagged::Dog {
            name: "Rex".into(),
            good_boy: true,
        },
        json_only
    );

    bench!(
        v,
        enum_untagged_solver,
        ConfigEnum,
        ConfigEnum::Database {
            host: "localhost".into(),
            port: 5432,
        },
        json_only
    );

    // ── Postcard enum (manual — uses plain repr(u8) tagging) ────────────────
    {
        static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
            ::postcard::to_allocvec(&PostcardAnimal::Dog {
                name: "Rex".into(),
                good_boy: true,
            })
            .unwrap()
        });
        static DECODER: LazyLock<fad::compiler::CompiledDecoder> = LazyLock::new(|| {
            fad::compile_decoder(PostcardAnimal::SHAPE, &fad::postcard::FadPostcard)
        });

        v.push(harness::Bench {
            name: "postcard_enum/serde_deser".into(),
            func: Box::new(|runner| {
                let data = &*DATA;
                runner.run(|| {
                    black_box(::postcard::from_bytes::<PostcardAnimal>(black_box(data)).unwrap());
                });
            }),
        });
        v.push(harness::Bench {
            name: "postcard_enum/fad_dynasm_deser".into(),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*DECODER;
                runner.run(|| {
                    black_box(fad::deserialize::<PostcardAnimal>(deser, black_box(data)).unwrap());
                });
            }),
        });
    }

    // ── Flatten (JSON: full, postcard: fad-only) ────────────────────────────

    bench!(
        v,
        flatten,
        Document,
        Document {
            title: "Hello".into(),
            meta: Metadata {
                version: 1,
                author: "Amos".into()
            },
        },
        json_only
    );

    // Postcard flatten: serde can't do it, but fad can.
    {
        static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
            ::postcard::to_allocvec(&DocumentFlat {
                title: "Hello".into(),
                version: 1,
                author: "Amos".into(),
            })
            .unwrap()
        });
        static DECODER: LazyLock<fad::compiler::CompiledDecoder> =
            LazyLock::new(|| fad::compile_decoder(Document::SHAPE, &fad::postcard::FadPostcard));

        v.push(harness::Bench {
            name: "postcard_flatten/fad_dynasm_deser".into(),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*DECODER;
                runner.run(|| {
                    black_box(fad::deserialize::<Document>(deser, black_box(data)).unwrap());
                });
            }),
        });
    }

    // ── Tuples and arrays ────────────────────────────────────────────────────

    bench!(v, tuple_pair, Pair, (42u32, "Alice".to_string()));
    bench!(v, tuple_triple, Triple, (42u32, 1_000_000u64, 3.14f64));
    bench!(v, array_u32_8, [u32; 8], [1, 2, 3, 4, 5, 6, 7, 8]);
    bench!(v, array_f64_4, [f64; 4], [1.0, 2.5, 3.14, 99.9]);

    // ── Options (both formats, deser only — encoder doesn't support Option yet) ──

    bench!(v, option_none, OptionScalar, OptionScalar { value: None }, +ir);
    bench!(
        v,
        option_scalar,
        OptionScalar,
        OptionScalar { value: Some(42) },
        +ir
    );
    bench!(
        v,
        option_string,
        OptionStr,
        OptionStr {
            value: Some("hello world".into())
        },
        +ir
    );
    bench!(
        v,
        option_struct,
        OptionStruct,
        OptionStruct {
            value: Some(Friend {
                age: 25,
                name: "Alice".into()
            }),
        },
        +ir
    );

    // ── Collections (both formats, deser only — encoder doesn't support Vec yet) ─

    bench!(
        v,
        vec_scalar_small,
        ScalarVec,
        ScalarVec {
            values: vec![1, 2, 3],
        },
        +ir
    );

    bench!(
        v,
        vec_scalar_medium,
        ScalarVec,
        ScalarVec {
            values: (0..100).collect(),
        },
        +ir
    );

    bench!(
        v,
        vec_scalar_large,
        ScalarVec,
        ScalarVec {
            values: (0..10_000).collect(),
        },
        +ir
    );

    bench!(
        v,
        vec_struct,
        StructVec,
        StructVec {
            friends: vec![
                Friend {
                    age: 25,
                    name: "Alice".into()
                },
                Friend {
                    age: 30,
                    name: "Bob".into()
                },
                Friend {
                    age: 35,
                    name: "Charlie".into()
                },
            ],
        }
        ,
        +ir
    );

    bench!(
        v,
        vec_string,
        StringVec,
        StringVec {
            names: vec![
                "alice".into(),
                "bob".into(),
                "carol".into(),
                "dave".into(),
                "eve".into(),
                "frank".into(),
                "grace".into(),
                "henry".into(),
            ],
        },
        +ir
    );

    bench!(
        v,
        map_small,
        ScalarMap,
        ScalarMap {
            scores: [("alice", 42), ("bob", 7), ("carol", 99), ("dave", 15)]
                .into_iter()
                .map(|(k, v)| (k.into(), v))
                .collect(),
        }
    );

    bench!(
        v,
        map_medium,
        ScalarMap,
        ScalarMap {
            scores: (0..16u32).map(|i| (format!("player{i}"), i * 10)).collect(),
        }
    );

    bench!(
        v,
        map_struct,
        StructMap,
        StructMap {
            roster: [
                (
                    "alice",
                    Friend {
                        age: 25,
                        name: "Alice".into()
                    }
                ),
                (
                    "bob",
                    Friend {
                        age: 30,
                        name: "Bob".into()
                    }
                ),
                (
                    "carol",
                    Friend {
                        age: 35,
                        name: "Carol".into()
                    }
                ),
            ]
            .into_iter()
            .map(|(k, v)| (k.into(), v))
            .collect(),
        }
    );

    // ── String-heavy (1024 strings) ──────────────────────────────────────────

    bench!(
        v,
        string_heavy,
        StringBag,
        StringBag {
            values: (0..1024)
                .map(|i| format!("user-{i:04}-alpha-beta-gamma-delta-epsilon-zeta-theta-lambda"))
                .collect(),
        },
        +ir
    );

    // ── String escaping (JSON only — values contain chars serde will escape) ─

    bench!(
        v,
        string_escapes,
        Friend,
        Friend {
            age: 42,
            name: "hello\nworld\t\"escaped\"".into(),
        },
        json_only
    );

    bench!(
        v,
        string_heavy_escapes,
        ManyStrings,
        ManyStrings {
            first: "Alice \"Al\" Johnson".into(),
            last: "O'Brien".into(),
            email: "alice+test@example.com".into(),
            city: "New\nYork".into(),
            country: "USA".into(),
            bio: "Writes code\t& drinks coffee".into(),
        },
        json_only
    );

    harness::run_benchmarks(v);
}
