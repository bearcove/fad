# kajit

JIT deserializer for Rust. Generates native machine code at runtime from
[facet](https://github.com/facet-rs/facet) type reflection. No proc macros,
no schema files — `#[derive(Facet)]` on your types is all it needs.

## How it works

kajit walks a type's `Shape` (facet's reflection metadata) at startup and emits
aarch64 or x86_64 machine code via [dynasmrt](https://crates.io/crates/dynasmrt).
One function per type, composed recursively. Nested structs get inlined or called
depending on the format. The generated code runs directly against input bytes with
no intermediate representation.

Inline decode sequences (bounds check, load, zigzag decode, store) are expressed
once as platform-neutral recipes (`src/recipe.rs`) and lowered to arch-specific
instructions by each backend.

## What it supports

**Formats:**

- Postcard — varint integers, zigzag signed integers, length-prefixed strings,
  raw bytes for u8/i8, IEEE 754 LE floats
- JSON — objects, arrays, all escape sequences including `\uXXXX` and surrogate
  pairs, key-order-independent field matching

**Types:**

- All primitive scalars: `bool`, `u8`–`u64`, `i8`–`i64`, `f32`, `f64`
- `String` (UTF-8 validated, heap-allocated)
- Structs (flat, nested, `#[facet(flatten)]`)
- Enums with `#[repr(u8)]`: unit, struct, and tuple variants
- Enum tagging: external, adjacent, internal, untagged
- `Option<T>`
- `Vec<T>` with exact-capacity allocation

**Architectures:**

- aarch64 (native on Apple Silicon, CI on depot.dev ARM runners)
- x86_64 (CI on depot.dev, local testing via Docker on aarch64 hosts)

## Performance

kajit aims to generate the same quality of code a human would write by hand for each
specific type. Varint decoding has a single-byte fast path inlined into the
hot loop; multi-byte varints fall through to an intrinsic. Vec loops over
scalar elements write directly to the output buffer with no intermediate
copies. String deserialization discovers `String`'s field layout at compile
time and writes `(ptr, len, cap)` directly.

Benchmarks are in `benches/deser_postcard.rs` and `benches/deser_json.rs`,
runnable with `cargo bench`.

## License

Licensed under either of:

- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)

at your option.
