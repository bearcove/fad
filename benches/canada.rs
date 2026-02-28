//! Benchmark parsing canada.json (GeoJSON) from nativejson-benchmark.
//!
//! Tests deeply nested arrays of floating-point coordinates.

use divan::{Bencher, black_box};
use facet::Facet;
use serde::Deserialize;
use std::sync::LazyLock;

fn main() {
    divan::main();
}

// =============================================================================
// Types for canada.json (GeoJSON)
// =============================================================================

#[derive(Debug, Deserialize, Facet)]
struct FeatureCollection {
    #[serde(rename = "type")]
    #[facet(rename = "type")]
    type_: String,
    features: Vec<Feature>,
}

#[derive(Debug, Deserialize, Facet)]
struct Feature {
    #[serde(rename = "type")]
    #[facet(rename = "type")]
    type_: String,
    properties: Properties,
    geometry: Geometry,
}

#[derive(Debug, Deserialize, Facet)]
struct Properties {
    name: String,
}

#[derive(Debug, Deserialize, Facet)]
struct Geometry {
    #[serde(rename = "type")]
    #[facet(rename = "type")]
    type_: String,
    coordinates: Vec<Vec<Vec<f64>>>,
}

// =============================================================================
// Data loading
// =============================================================================

fn decompress(compressed: &[u8]) -> Vec<u8> {
    let mut decompressed = Vec::new();
    brotli::BrotliDecompress(&mut std::io::Cursor::new(compressed), &mut decompressed)
        .expect("Failed to decompress fixture");
    decompressed
}

static CANADA_JSON: LazyLock<Vec<u8>> = LazyLock::new(|| {
    let compressed = include_bytes!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/fixtures/canada.json.br"
    ));
    decompress(compressed)
});

// =============================================================================
// Cached compiled deserializers
// =============================================================================

static FAD_CANADA: LazyLock<fad::compiler::CompiledDecoder> =
    LazyLock::new(|| fad::compile_decoder(FeatureCollection::SHAPE, &fad::json::FadJson));

// =============================================================================
// Benchmarks
// =============================================================================

#[divan::bench]
fn serde_json(bencher: Bencher) {
    let data = &*CANADA_JSON;
    bencher
        .bench(|| black_box(serde_json::from_slice::<FeatureCollection>(black_box(data)).unwrap()));
}

#[divan::bench]
fn facet_json_tier0(bencher: Bencher) {
    let data = &*CANADA_JSON;
    bencher
        .bench(|| black_box(facet_json::from_slice::<FeatureCollection>(black_box(data)).unwrap()));
}

#[divan::bench]
fn fad(bencher: Bencher) {
    let data = &*CANADA_JSON;
    let deser = &*FAD_CANADA;
    bencher.bench(|| {
        black_box(fad::deserialize::<FeatureCollection>(deser, black_box(data)).unwrap())
    });
}
