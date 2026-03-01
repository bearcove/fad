#[path = "harness.rs"]
mod harness;

#[macro_use]
#[path = "common/bench_macros.rs"]
mod bench_macros;

use facet::Facet;
use std::hint::black_box;
use std::sync::LazyLock;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Facet)]
struct Friend {
    age: u32,
    name: String,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Facet)]
struct Inner {
    x: u32,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Facet)]
struct Middle {
    inner: Inner,
    y: u32,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Facet)]
struct Outer {
    middle: Middle,
    z: u32,
}

fn main() {
    let mut benches: Vec<harness::Bench> = Vec::new();

    bench!(
        benches,
        ir_vs_legacy_friend,
        Friend,
        Friend {
            age: 42,
            name: "Alice".into(),
        },
        postcard_legacy_ir_compile
    );

    bench!(
        benches,
        ir_vs_legacy_nested,
        Outer,
        Outer {
            middle: Middle {
                inner: Inner { x: 7 },
                y: 99,
            },
            z: 123,
        },
        postcard_legacy_ir
    );

    harness::run_benchmarks(benches);
}
