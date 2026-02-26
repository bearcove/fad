# fad specification

fad yearns for fast deserialization in Rust, based on JIT compilation of
serialization/deserialization code based on [facet] reflection.

[facet]: https://github.com/facet-rs/facet

## Context

serde relies on proc macros to generate code that can serialize/deserialize Rust
values. That code is generic, instantiated with a format crate (JSON, Serde, etc.),
resulting in very fast ser/deser.

facet relies on proc macros to expose type information (layout, fields, vtables etc.).
That information is available at runtime for format crates to ser/deser (with the
help of a layer like `facet-reflect`) arbitrary Rust values.

a naive reflection-based approach (most facet-format crates) yields code that's 
between 5-50x slower than the serde approach.

facet-format-jit showed that emitting machine code at runtime, caching it per
(type, format, direction) combination can be competitive or even faster than serde,
while allowing (in theory) faster builds, and decoupling "deriving the trait" from
"generating ser/deser code for it".

however facet-format-jit was not built rigorously from the ground up for correctness,
and should probably not used in any production Rust code — it is disabled by default
for a reason.

fast, facet-based, postcard & JSON de/ser would be extremely valuable for tools
like the [roam](https://github.com/bearcove/roam) RPC framework, or the
[moire](https://github.com/bearcove/moire) instrumentation framework.

fad is an attempt at building JIT facet de/ser that isn't wildly unsafe or impractical.

## API surface

```rust
use facet::Facet;
// this implements a `fad` trait
use fad_json::FadJson;

// A random type that happens to derive Facet
#[derive(Facet)]
struct MyDocument {
    users: Vec<User>,
    birthdays: HashMap<u32, String>,
    // etc.
}

let deser = fad::compile_deser(<MyDocument as Facet>::SHAPE, FadJson)?;
let doc = MaybeUninit::<MyDocument>::uninit();

unsafe { deser.call(&mut doc, r#"{ "users": [], "birthdays": {} }"#.as_bytes()); };
let doc = unsafe { doc.assume_init() };
// doc.users is len 0, etc.
```
