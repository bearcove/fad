//! Deser/ser cases

const TYPES_RS: &str = r#"
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
struct BoolField {
    value: bool,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct ScalarVec {
    values: Vec<u32>,
}

type Pair = (u32, String);
"#;

const CASES: &[Case] = &[
    Case {
        name: "flat_struct",
        ty: "Friend",
        value: r#"Friend { age: 42, name: "Alice".into() }"#,
        mode: BenchMode::SerIr,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: true,
    },
    Case {
        name: "nested_struct",
        ty: "Person",
        value: r#"Person { name: "Alice".into(), age: 30, address: Address { city: "Portland".into(), zip: 97201 } }"#,
        mode: BenchMode::Ser,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: false,
    },
    Case {
        name: "deep_struct",
        ty: "Outer",
        value: r#"Outer { middle: Middle { inner: Inner { x: 1 }, y: 2 }, z: 3 }"#,
        mode: BenchMode::SerIr,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: true,
    },
    Case {
        name: "all_integers",
        ty: "AllIntegers",
        value: r#"AllIntegers { a_u8: 255, a_u16: 65535, a_u32: 1_000_000, a_u64: 1_000_000_000_000, a_i8: -128, a_i16: -32768, a_i32: -1_000_000, a_i64: -1_000_000_000_000 }"#,
        mode: BenchMode::Ser,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: false,
    },
    Case {
        name: "bool_field",
        ty: "BoolField",
        value: r#"BoolField { value: true }"#,
        mode: BenchMode::Ser,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: false,
    },
    Case {
        name: "tuple_pair",
        ty: "Pair",
        value: r#"(42u32, "Alice".to_string())"#,
        mode: BenchMode::Default,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: false,
    },
    Case {
        name: "vec_scalar_small",
        ty: "ScalarVec",
        value: r#"ScalarVec { values: (0..16).map(|i| i as u32).collect() }"#,
        mode: BenchMode::Ir,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: true,
    },
    Case {
        name: "vec_scalar_large",
        ty: "ScalarVec",
        value: r#"ScalarVec { values: (0..2048).map(|i| i as u32).collect() }"#,
        mode: BenchMode::Ir,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: true,
    },
];
