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

The compiler walks the shape tree at JIT-compile time and emits IR for each
node. The recursion is in the host (Rust) code — the emitted code is flat.

```rust
fn compile_deser(shape: &Shape, fmt: &dyn Format, code: &mut Code) {
    match shape.kind() {
        ShapeKind::Struct(fields) => {
            fmt.emit_expect_begin_object(code);
            // `build_field_dispatch` runs at JIT-compile time: all field names
            // are known statically, so we bake in a trie/phf over them.
            let dispatch = build_field_dispatch(fields);
            fmt.emit_field_loop(code, dispatch, |code, field| {
                // `field` is known at JIT-compile time; recurse into its shape.
                compile_deser(field.shape, fmt, code);
                emit_store(code, field.offset); // write result into struct slot
            });
            emit_assert_all_required_set(code, fields); // checks stack-allocated bitset
            fmt.emit_expect_end_object(code);
        }
        ShapeKind::Primitive(p) => fmt.emit_read_primitive(code, p),
        ShapeKind::Sequence(elem) => {
            fmt.emit_begin_array(code);
            fmt.emit_array_loop(code, |code| compile_deser(elem.shape, fmt, code));
            fmt.emit_end_array(code);
        }
        // ...
    }
}
```

Key points:

- **Field dispatch** happens in the emitted code against a runtime key, but the
  dispatch table (trie, perfect hash, or sorted array) is built at JIT-compile
  time from the known field names. The host never does a runtime key lookup.

- **Required-field tracking** uses a stack-allocated bitset sized
  `ceil(field_count / 64)` bits, allocated at emitted-function entry.
  `emit_assert_all_required_set` checks it before the function returns.

- **Error handling** TBD: options are two-register return (clean), a caller-
  provided error-slot pointer (simple), or setjmp/longjmp (fast happy path,
  ugly). Pick one before implementing.

## Recursive types

Consider:

```rust
struct Node {
    value: i32,
    children: Vec<Node>,
}
```

`Node` contains `Vec<Node>`, which contains `Node`. If `compile_deser` naively
recurses into each field's shape, it loops forever at JIT-compile time — it
never bottoms out.

The fix is standard: track which shapes are currently being compiled, and
treat a back-edge as a call instead of an inline.

```rust
fn compile_deser(
    shape: &'static Shape,
    fmt: &dyn Format,
    cx: &mut CompileContext, // tracks in-progress and finished shapes
) {
    if let Some(func) = cx.finished.get(shape) {
        // Already compiled: emit a direct call to the finished function.
        emit_call(cx.code, func);
        return;
    }

    if cx.in_progress.contains(shape) {
        // Back-edge: emit a call via a forward reference; patch it later.
        let fwd = cx.forward_ref(shape);
        emit_call_fwd(cx.code, fwd);
        return;
    }

    cx.in_progress.insert(shape);
    let func_start = cx.code.current_offset();

    match shape.kind() {
        ShapeKind::Struct(fields) => {
            fmt.emit_expect_begin_object(cx.code);
            let dispatch = build_field_dispatch(fields);
            fmt.emit_field_loop(cx.code, dispatch, |cx, field| {
                compile_deser(field.shape, fmt, cx); // may emit a call, not an inline
                emit_store(cx.code, field.offset);
            });
            emit_assert_all_required_set(cx.code, fields);
            fmt.emit_expect_end_object(cx.code);
        }
        // ...
    }

    cx.in_progress.remove(shape);
    let func = cx.code.finish_func(func_start);
    cx.patch_forward_refs(shape, func); // fix up any calls emitted during back-edges
    cx.finished.insert(shape, func);
}
```

The result for `Node` is two emitted functions:

- `deser_Node(out: *mut Node, input: &[u8])` — reads the struct, calls
  `deser_Vec_Node` for the `children` field.
- `deser_Vec_Node(out: *mut Vec<Node>, input: &[u8])` — allocates the vec,
  loops, calls `deser_Node` for each element.

They call each other exactly like hand-written Rust would. The forward-reference
mechanism is only needed when a type directly or indirectly contains itself
before its own compilation is done — mutual recursion is handled the same way.
