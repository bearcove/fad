//! Callgrind profiling example — HashMap<String, Struct> deserialization.
//!
//! cargo build --profile profiling --example profile_map_struct
//! valgrind --tool=callgrind --callgrind-out-file=callgrind.serde_json \
//!     ./target/profiling/examples/profile_map_struct serde_json
//! valgrind --tool=callgrind --callgrind-out-file=callgrind.fad \
//!     ./target/profiling/examples/profile_map_struct fad

use facet::Facet;
use std::collections::HashMap;
use std::hint::black_box;

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct FriendSerde {
    age: u32,
    name: String,
}

#[derive(Facet, Debug, PartialEq)]
struct FriendFacet {
    age: u32,
    name: String,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct MapStructSerde {
    roster: HashMap<String, FriendSerde>,
}

#[derive(Facet, Debug, PartialEq)]
struct MapStructFacet {
    roster: HashMap<String, FriendFacet>,
}

static INPUT: &[u8] = br#"{"roster": {"alice": {"age": 25, "name": "Alice"}, "bob": {"age": 30, "name": "Bob"}, "carol": {"age": 35, "name": "Carol"}}}"#;

const ITERS: usize = 1000;

fn main() {
    let mode = std::env::args()
        .nth(1)
        .expect("usage: profile_map_struct <serde_json|fad>");

    match mode.as_str() {
        "serde_json" => {
            for _ in 0..ITERS {
                black_box(serde_json::from_slice::<MapStructSerde>(black_box(INPUT)).unwrap());
            }
        }
        "fad" => {
            let deser = fad::compile_decoder(MapStructFacet::SHAPE, &fad::json::FadJson);
            for _ in 0..ITERS {
                black_box(fad::deserialize::<MapStructFacet>(&deser, black_box(INPUT)).unwrap());
            }
        }
        other => panic!("unknown mode: {other}"),
    }
}
