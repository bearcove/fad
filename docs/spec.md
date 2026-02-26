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

## Shape intelligence vs format intelligence

The whole trick of fad is to put together what we know about a shape with what we know
about a format.

For example, let's say our input shape is:

```rust
struct Friend {
    age: u32,
    name: String,
}
```

We know the name, type, layout, etc. of both fields. We know how to allocate
such a value, how to partially construct it, how to check that it's been fully
constructed, how to drop it etc. etc. That's the shape intelligence.

When it comes to JSON, we know that...

```json
{
  "name": "Didier",
  "age": 432
}
```

...objects are delimited by `{` and `}`, that keys are always strings
(double-quoted), keys are separated from values by colons, etc. etc. This is
format intelligence.

On the other hand, when it comes to
[postcard](https://postcard.jamesmunns.com/), we know that keys come in order,
have no 'name' (they're not maps, they're known lists of fields, conceptually):
postcard is non-self-describing.

fad should be able to combine shape intelligence with format intelligence to
generate "IR" (intermediate representation) that can be either interpreted or
lowered to aarch64 or x86_64 machine code, via [dynasmrt](https://crates.io/crates/dynasmrt).

## What should that IR look like??

Do we get raw pointers or not? How close to assembly is it? How many intrinsics
(implemented as Rust functions, called from the machine code) do we allow ourselves?

For simplification, we'll assume that:

  * All inputs are `&[u8]` and they are a complete document (no streaming)
  
The format struct like `FadJson` above must implement a trait that the fad
compiler can call to generate IR.

## A way too trivial case

```rust
fn compile_deser(shape: &Shape) -> Program {
    let code = Code::new();
    
    if shape.is_struct() {
        fmt.emit_begin_object(&mut code);
        
        let field_set = shape.field_set();
        let floop = code.loop();
        fmt.emit_peek_is_end_object();
        fmt.emit_if_true_break_loop(floop);
        
        let key = fmt.emit_read_key();
        let entry = field_set.get_entry(key); // fixme: emit code that does this, don't do it at compile time
        // if no entry, error out (unnown field)
        if entry {
            let value = fmt.emit_read_value(); // erm that depends on the type of the entry? how does recursion work?
            field_set.set(value);
        }
        
        field_set.emit_assert_all_set(&mut code);
    }
    code
}
```
