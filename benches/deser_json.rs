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

// ── Shared types: enum (externally tagged) ──────────────────────────────

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

// ── Shared types: flatten ─────────────────────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct MetadataSerde {
    version: u32,
    author: String,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct DocumentSerde {
    title: String,
    #[serde(flatten)]
    meta: MetadataSerde,
}

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

// ── Shared types: enum (adjacently tagged) ────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
#[serde(tag = "type", content = "data")]
#[repr(u8)]
enum AdjAnimalSerde {
    Cat,
    Dog { name: String, good_boy: bool },
    Parrot(String),
}

#[derive(facet::Facet, Debug, PartialEq)]
#[facet(tag = "type", content = "data")]
#[repr(u8)]
enum AdjAnimalFacet {
    Cat,
    Dog { name: String, good_boy: bool },
    Parrot(String),
}

// ── Shared types: enum (internally tagged) ────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
#[serde(tag = "type")]
#[repr(u8)]
enum IntAnimalSerde {
    Cat,
    Dog { name: String, good_boy: bool },
}

#[derive(facet::Facet, Debug, PartialEq)]
#[facet(tag = "type")]
#[repr(u8)]
enum IntAnimalFacet {
    Cat,
    Dog { name: String, good_boy: bool },
}

// ── Shared types: enum (untagged, mixed buckets) ──────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
#[serde(untagged)]
#[repr(u8)]
enum UntaggedSerde {
    Cat,
    Dog { name: String, good_boy: bool },
    Parrot(String),
}

#[derive(facet::Facet, Debug, PartialEq)]
#[facet(untagged)]
#[repr(u8)]
enum UntaggedFacet {
    Cat,
    Dog { name: String, good_boy: bool },
    Parrot(String),
}

// ── Shared types: enum (untagged, solver — multiple struct variants) ──────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
#[serde(untagged)]
#[repr(u8)]
enum ConfigSerde {
    Database { host: String, port: u32 },
    Redis { host: String, db: u32 },
}

#[derive(facet::Facet, Debug, PartialEq)]
#[facet(untagged)]
#[repr(u8)]
enum ConfigFacet {
    Database { host: String, port: u32 },
    Redis { host: String, db: u32 },
}

// ── Shared types: enum (untagged, solver — value-type evidence) ─────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
#[serde(untagged)]
#[repr(u8)]
enum ValueTypedSerde {
    NumField { value: u32 },
    StrField { value: String },
}

#[derive(facet::Facet, Debug, PartialEq)]
#[facet(untagged)]
#[repr(u8)]
enum ValueTypedFacet {
    NumField { value: u32 },
    StrField { value: String },
}

// ── Shared types: enum (untagged, solver — nested key evidence) ─────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct SuccessPayloadSerde {
    items: u32,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct ErrorPayloadSerde {
    message: String,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
#[serde(untagged)]
#[repr(u8)]
enum ApiResponseSerde {
    Success {
        status: u32,
        data: SuccessPayloadSerde,
    },
    Error {
        status: u32,
        data: ErrorPayloadSerde,
    },
}

#[derive(facet::Facet, Debug, PartialEq)]
struct SuccessPayloadFacet {
    items: u32,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct ErrorPayloadFacet {
    message: String,
}

#[derive(facet::Facet, Debug, PartialEq)]
#[facet(untagged)]
#[repr(u8)]
enum ApiResponseFacet {
    Success {
        status: u32,
        data: SuccessPayloadFacet,
    },
    Error {
        status: u32,
        data: ErrorPayloadFacet,
    },
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

// ── Shared types: string-heavy payload ────────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct StringBagSerde {
    values: Vec<String>,
}

#[derive(Facet, Debug, PartialEq)]
struct StringBagFacet {
    values: Vec<String>,
}

// ── Encoded test data ───────────────────────────────────────────────────────

static FLAT_JSON: &[u8] = br#"{"age": 42, "name": "Alice"}"#;

static NESTED_JSON: &[u8] =
    br#"{"name": "Alice", "age": 30, "address": {"city": "Portland", "zip": 97201}}"#;

static DEEP_JSON: &[u8] = br#"{"middle": {"inner": {"x": 1}, "y": 2}, "z": 3}"#;

static ENUM_JSON: &[u8] = br#"{"Dog": {"name": "Rex", "good_boy": true}}"#;

static FLATTEN_JSON: &[u8] = br#"{"title": "Hello", "version": 1, "author": "Amos"}"#;

static ADJ_ENUM_JSON: &[u8] = br#"{"type": "Dog", "data": {"name": "Rex", "good_boy": true}}"#;

static INT_ENUM_JSON: &[u8] = br#"{"type": "Dog", "name": "Rex", "good_boy": true}"#;

static UNTAGGED_STRUCT_JSON: &[u8] = br#"{"name": "Rex", "good_boy": true}"#;

static UNTAGGED_SOLVER_JSON: &[u8] = br#"{"host": "localhost", "port": 5432}"#;

static UNTAGGED_VALUE_TYPE_JSON: &[u8] = br#"{"value": 42}"#;

static UNTAGGED_NESTED_KEY_JSON: &[u8] = br#"{"status": 200, "data": {"items": 5}}"#;

static VEC_SCALAR_SMALL_JSON: &[u8] = br#"{"values": [1, 2, 3]}"#;

static VEC_SCALAR_MEDIUM_JSON: LazyLock<Vec<u8>> = LazyLock::new(|| {
    serde_json::to_vec(&ScalarVecSerde {
        values: (0..100).collect(),
    })
    .unwrap()
});

static VEC_STRUCT_JSON: &[u8] = br#"{"friends": [{"age": 25, "name": "Alice"}, {"age": 30, "name": "Bob"}, {"age": 35, "name": "Charlie"}]}"#;

static STRING_HEAVY_JSON_BYTES: LazyLock<Vec<u8>> = LazyLock::new(|| {
    let values = (0..1024)
        .map(|i| format!("user-{i:04}-alpha-beta-gamma-delta-epsilon-zeta-theta-lambda"))
        .collect();
    serde_json::to_vec(&StringBagSerde { values }).unwrap()
});

static STRING_HEAVY_JSON_TEXT: LazyLock<String> =
    LazyLock::new(|| String::from_utf8(STRING_HEAVY_JSON_BYTES.clone()).unwrap());

// ── Cached compiled deserializers ───────────────────────────────────────────

static FAD_FLAT: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(FriendFacet::SHAPE, &fad::json::FadJson));

static FAD_NESTED: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(PersonFacet::SHAPE, &fad::json::FadJson));

static FAD_DEEP: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(OuterFacet::SHAPE, &fad::json::FadJson));

static FAD_ENUM: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(AnimalFacet::SHAPE, &fad::json::FadJson));

static FAD_FLATTEN: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(DocumentFacet::SHAPE, &fad::json::FadJson));

static FAD_ADJ_ENUM: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(AdjAnimalFacet::SHAPE, &fad::json::FadJson));

static FAD_INT_ENUM: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(IntAnimalFacet::SHAPE, &fad::json::FadJson));

static FAD_UNTAGGED: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(UntaggedFacet::SHAPE, &fad::json::FadJson));

static FAD_UNTAGGED_SOLVER: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(ConfigFacet::SHAPE, &fad::json::FadJson));

static FAD_UNTAGGED_VALUE_TYPE: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(ValueTypedFacet::SHAPE, &fad::json::FadJson));

static FAD_UNTAGGED_NESTED_KEY: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(ApiResponseFacet::SHAPE, &fad::json::FadJson));

static FAD_VEC_SCALAR: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(ScalarVecFacet::SHAPE, &fad::json::FadJson));

static FAD_VEC_STRUCT: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(StructVecFacet::SHAPE, &fad::json::FadJson));

static FAD_STRING_HEAVY: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(StringBagFacet::SHAPE, &fad::json::FadJson));

static FACET_JSON_JIT_FLAT: LazyLock<
    facet_format::jit::CompiledFormatDeserializer<FriendFacet, facet_json::JsonParser<'static>>,
> = LazyLock::new(|| {
    facet_format::jit::get_format_deserializer().expect("FriendFacet should be Tier-2 compatible")
});

// ── Benchmarks: flat struct ─────────────────────────────────────────────────

#[divan::bench_group]
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

#[divan::bench_group]
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

#[divan::bench_group]
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

// ── Benchmarks: enum (externally tagged, struct variant) ──────────────────

#[divan::bench_group]
mod enum_external {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<AnimalSerde>(black_box(ENUM_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_ENUM;
        bencher.bench(|| {
            black_box(fad::deserialize::<AnimalFacet>(deser, black_box(ENUM_JSON)).unwrap())
        });
    }
}

// ── Benchmarks: flatten ───────────────────────────────────────────────────

#[divan::bench_group]
mod flatten {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<DocumentSerde>(black_box(FLATTEN_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_FLATTEN;
        bencher.bench(|| {
            black_box(fad::deserialize::<DocumentFacet>(deser, black_box(FLATTEN_JSON)).unwrap())
        });
    }
}

// ── Benchmarks: enum (adjacently tagged, struct variant) ──────────────────

#[divan::bench_group]
mod enum_adjacent {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<AdjAnimalSerde>(black_box(ADJ_ENUM_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_ADJ_ENUM;
        bencher.bench(|| {
            black_box(fad::deserialize::<AdjAnimalFacet>(deser, black_box(ADJ_ENUM_JSON)).unwrap())
        });
    }
}

// ── Benchmarks: enum (internally tagged, struct variant) ──────────────────

#[divan::bench_group]
mod enum_internal {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<IntAnimalSerde>(black_box(INT_ENUM_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_INT_ENUM;
        bencher.bench(|| {
            black_box(fad::deserialize::<IntAnimalFacet>(deser, black_box(INT_ENUM_JSON)).unwrap())
        });
    }
}

// ── Benchmarks: enum (untagged, struct variant via peek dispatch) ─────────

#[divan::bench_group]
mod enum_untagged {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(
                serde_json::from_slice::<UntaggedSerde>(black_box(UNTAGGED_STRUCT_JSON)).unwrap(),
            )
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_UNTAGGED;
        bencher.bench(|| {
            black_box(
                fad::deserialize::<UntaggedFacet>(deser, black_box(UNTAGGED_STRUCT_JSON)).unwrap(),
            )
        });
    }
}

// ── Benchmarks: enum (untagged, solver — key presence only) ───────────────

#[divan::bench_group]
mod enum_untagged_solver {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(
                serde_json::from_slice::<ConfigSerde>(black_box(UNTAGGED_SOLVER_JSON)).unwrap(),
            )
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_UNTAGGED_SOLVER;
        bencher.bench(|| {
            black_box(
                fad::deserialize::<ConfigFacet>(deser, black_box(UNTAGGED_SOLVER_JSON)).unwrap(),
            )
        });
    }
}

// ── Benchmarks: enum (untagged, solver — value-type evidence) ─────────────

#[divan::bench_group]
mod enum_untagged_value_type {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(
                serde_json::from_slice::<ValueTypedSerde>(black_box(UNTAGGED_VALUE_TYPE_JSON))
                    .unwrap(),
            )
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_UNTAGGED_VALUE_TYPE;
        bencher.bench(|| {
            black_box(
                fad::deserialize::<ValueTypedFacet>(deser, black_box(UNTAGGED_VALUE_TYPE_JSON))
                    .unwrap(),
            )
        });
    }
}

// ── Benchmarks: enum (untagged, solver — nested key evidence) ─────────────

#[divan::bench_group]
mod enum_untagged_nested_key {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(
                serde_json::from_slice::<ApiResponseSerde>(black_box(UNTAGGED_NESTED_KEY_JSON))
                    .unwrap(),
            )
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_UNTAGGED_NESTED_KEY;
        bencher.bench(|| {
            black_box(
                fad::deserialize::<ApiResponseFacet>(deser, black_box(UNTAGGED_NESTED_KEY_JSON))
                    .unwrap(),
            )
        });
    }
}

// ── Benchmarks: Vec<u32> (3 elements) ─────────────────────────────────────

#[divan::bench_group]
mod vec_scalar_small {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(
                serde_json::from_slice::<ScalarVecSerde>(black_box(VEC_SCALAR_SMALL_JSON)).unwrap(),
            )
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_VEC_SCALAR;
        bencher.bench(|| {
            black_box(
                fad::deserialize::<ScalarVecFacet>(deser, black_box(VEC_SCALAR_SMALL_JSON))
                    .unwrap(),
            )
        });
    }
}

// ── Benchmarks: Vec<u32> (100 elements) ───────────────────────────────────

#[divan::bench_group]
mod vec_scalar_medium {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        let data = &*VEC_SCALAR_MEDIUM_JSON;
        bencher.bench(|| {
            black_box(serde_json::from_slice::<ScalarVecSerde>(black_box(data)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let data = &*VEC_SCALAR_MEDIUM_JSON;
        let deser = &*FAD_VEC_SCALAR;
        bencher.bench(|| {
            black_box(fad::deserialize::<ScalarVecFacet>(deser, black_box(data)).unwrap())
        });
    }
}

// ── Benchmarks: Vec<Friend> (3 structs) ───────────────────────────────────

#[divan::bench_group]
mod vec_struct {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<StructVecSerde>(black_box(VEC_STRUCT_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_VEC_STRUCT;
        bencher.bench(|| {
            black_box(
                fad::deserialize::<StructVecFacet>(deser, black_box(VEC_STRUCT_JSON)).unwrap(),
            )
        });
    }
}

// ── Shared types: HashMap<String, u32> ────────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct MapSerde {
    scores: std::collections::HashMap<String, u32>,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct MapFacet {
    scores: std::collections::HashMap<String, u32>,
}

// ── Encoded test data: maps ─────────────────────────────────────────────────

static MAP_SMALL_JSON: &[u8] =
    br#"{"scores": {"alice": 42, "bob": 7, "carol": 99, "dave": 15}}"#;

static MAP_MEDIUM_JSON: LazyLock<Vec<u8>> = LazyLock::new(|| {
    let scores = (0..16u32)
        .map(|i| (format!("player{i}"), i * 10))
        .collect();
    serde_json::to_vec(&MapSerde { scores }).unwrap()
});

// ── Cached compiled deserializers: maps ─────────────────────────────────────

static FAD_MAP: LazyLock<fad::compiler::CompiledDeser> = LazyLock::new(|| {
    fad::compile_deser(MapFacet::SHAPE, &fad::json::FadJson)
});

// ── Benchmarks: HashMap<String, u32> (4 entries) ──────────────────────────

#[divan::bench_group]
mod map_small {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(
                serde_json::from_slice::<MapSerde>(black_box(MAP_SMALL_JSON)).unwrap(),
            )
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_MAP;
        bencher.bench(|| {
            black_box(fad::deserialize::<MapFacet>(deser, black_box(MAP_SMALL_JSON)).unwrap())
        });
    }
}

// ── Benchmarks: HashMap<String, u32> (16 entries) ─────────────────────────

#[divan::bench_group]
mod map_medium {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        let data = &*MAP_MEDIUM_JSON;
        bencher.bench(|| {
            black_box(serde_json::from_slice::<MapSerde>(black_box(data)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let data = &*MAP_MEDIUM_JSON;
        let deser = &*FAD_MAP;
        bencher.bench(|| {
            black_box(fad::deserialize::<MapFacet>(deser, black_box(data)).unwrap())
        });
    }
}

// ── Shared types: HashMap<String, Friend> ─────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct MapStructSerde {
    roster: std::collections::HashMap<String, FriendSerde>,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct MapStructFacet {
    roster: std::collections::HashMap<String, FriendFacet>,
}

static MAP_STRUCT_JSON: &[u8] = br#"{"roster": {"alice": {"age": 25, "name": "Alice"}, "bob": {"age": 30, "name": "Bob"}, "carol": {"age": 35, "name": "Carol"}}}"#;

static FAD_MAP_STRUCT: LazyLock<fad::compiler::CompiledDeser> = LazyLock::new(|| {
    fad::compile_deser(MapStructFacet::SHAPE, &fad::json::FadJson)
});

// ── Benchmarks: HashMap<String, Friend> (3 entries) ────────────────────────

#[divan::bench_group]
mod map_struct {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(
                serde_json::from_slice::<MapStructSerde>(black_box(MAP_STRUCT_JSON)).unwrap(),
            )
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_MAP_STRUCT;
        bencher.bench(|| {
            black_box(
                fad::deserialize::<MapStructFacet>(deser, black_box(MAP_STRUCT_JSON)).unwrap(),
            )
        });
    }
}

// ── Shared types: string-heavy structs ────────────────────────────────────

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct ManyStringsSerde {
    first: String,
    last: String,
    email: String,
    city: String,
    country: String,
    bio: String,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct ManyStringsFacet {
    first: String,
    last: String,
    email: String,
    city: String,
    country: String,
    bio: String,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct LongStringSerde {
    content: String,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct LongStringFacet {
    content: String,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct StringVecSerde {
    names: Vec<String>,
}

#[derive(facet::Facet, Debug, PartialEq)]
struct StringVecFacet {
    names: Vec<String>,
}

// ── Encoded test data: strings ──────────────────────────────────────────────

static MANY_STRINGS_JSON: &[u8] = br#"{"first": "Alice", "last": "Johnson", "email": "alice@example.com", "city": "Portland", "country": "USA", "bio": "Software engineer"}"#;

static LONG_STRING_JSON: &[u8] = br#"{"content": "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."}"#;

static VEC_STRING_JSON: &[u8] = br#"{"names": ["alice", "bob", "carol", "dave", "eve", "frank", "grace", "henry"]}"#;

static HEAVY_ESCAPE_JSON: &[u8] = br#"{"first": "Alice \"Al\" Johnson", "last": "O\u0027Brien", "email": "alice+test@example.com", "city": "New\nYork", "country": "U\u0053A", "bio": "Writes code\t& drinks coffee"}"#;

// ── Cached compiled deserializers: strings ──────────────────────────────────

static FAD_MANY_STRINGS: LazyLock<fad::compiler::CompiledDeser> = LazyLock::new(|| {
    fad::compile_deser(ManyStringsFacet::SHAPE, &fad::json::FadJson)
});

static FAD_LONG_STRING: LazyLock<fad::compiler::CompiledDeser> = LazyLock::new(|| {
    fad::compile_deser(LongStringFacet::SHAPE, &fad::json::FadJson)
});

static FAD_VEC_STRING: LazyLock<fad::compiler::CompiledDeser> = LazyLock::new(|| {
    fad::compile_deser(StringVecFacet::SHAPE, &fad::json::FadJson)
});

// ── Benchmarks: 6 short string fields (fast path) ─────────────────────────

#[divan::bench_group]
mod string_many_fields {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<ManyStringsSerde>(black_box(MANY_STRINGS_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_MANY_STRINGS;
        bencher.bench(|| {
            black_box(fad::deserialize::<ManyStringsFacet>(deser, black_box(MANY_STRINGS_JSON)).unwrap())
        });
    }
}

// ── Benchmarks: single long string field (fast path scanning) ─────────────

#[divan::bench_group]
mod string_long_value {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<LongStringSerde>(black_box(LONG_STRING_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_LONG_STRING;
        bencher.bench(|| {
            black_box(fad::deserialize::<LongStringFacet>(deser, black_box(LONG_STRING_JSON)).unwrap())
        });
    }
}

// ── Benchmarks: Vec<String> (8 elements) ──────────────────────────────────

#[divan::bench_group]
mod string_vec {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<StringVecSerde>(black_box(VEC_STRING_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_VEC_STRING;
        bencher.bench(|| {
            black_box(fad::deserialize::<StringVecFacet>(deser, black_box(VEC_STRING_JSON)).unwrap())
        });
    }
}

// ── Benchmarks: 6 string fields with escape sequences (slow path) ─────────

#[divan::bench_group]
mod string_heavy_escapes {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<ManyStringsSerde>(black_box(HEAVY_ESCAPE_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_MANY_STRINGS;
        bencher.bench(|| {
            black_box(fad::deserialize::<ManyStringsFacet>(deser, black_box(HEAVY_ESCAPE_JSON)).unwrap())
        });
    }
}

// ── Benchmarks: string with escape sequences ───────────────────────────

static ESCAPE_JSON: &[u8] = br#"{"age": 42, "name": "hello\nworld\t\"escaped\"\u0041"}"#;

static FAD_ESCAPE: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(FriendFacet::SHAPE, &fad::json::FadJson));

#[divan::bench_group]
mod string_escapes {
    use super::*;

    #[divan::bench]
    fn serde_json(bencher: Bencher) {
        bencher.bench(|| {
            black_box(serde_json::from_slice::<FriendSerde>(black_box(ESCAPE_JSON)).unwrap())
        });
    }

    #[divan::bench]
    fn fad(bencher: Bencher) {
        let deser = &*FAD_ESCAPE;
        bencher.bench(|| {
            black_box(fad::deserialize::<FriendFacet>(deser, black_box(ESCAPE_JSON)).unwrap())
        });
    }
}

// ── Benchmarks: string-heavy payload (trusted UTF-8 path) ────────────────

#[divan::bench_group]
mod string_heavy_utf8_trust {
    use super::*;

    #[divan::bench]
    fn serde_json_from_slice(bencher: Bencher) {
        let data = &*STRING_HEAVY_JSON_BYTES;
        bencher.bench(|| {
            black_box(serde_json::from_slice::<StringBagSerde>(black_box(data)).unwrap())
        });
    }

    #[divan::bench]
    fn fad_from_bytes(bencher: Bencher) {
        let data = &*STRING_HEAVY_JSON_BYTES;
        let deser = &*FAD_STRING_HEAVY;
        bencher.bench(|| {
            black_box(fad::deserialize::<StringBagFacet>(deser, black_box(data)).unwrap())
        });
    }

    #[divan::bench]
    fn fad_from_str(bencher: Bencher) {
        let data = &*STRING_HEAVY_JSON_TEXT;
        let deser = &*FAD_STRING_HEAVY;
        bencher
            .bench(|| black_box(fad::from_str::<StringBagFacet>(deser, black_box(data)).unwrap()));
    }
}
