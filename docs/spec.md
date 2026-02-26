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
struct Compiler {
    fmt: Box<dyn Format>,
    code: Code,
    finished: HashMap<&'static Shape, FuncRef>,
    in_progress: HashSet<&'static Shape>,
    forward_refs: HashMap<&'static Shape, Vec<PatchSite>>,
}

impl Compiler {
    fn compile_deser(&mut self, shape: &'static Shape) {
        if let Some(func) = self.finished.get(shape) {
            // Already compiled: emit a direct call to the finished function.
            self.code.emit_call(*func);
            return;
        }

        if self.in_progress.contains(shape) {
            // Back-edge: emit a call via a forward reference; patch it later.
            let site = self.code.emit_call_fwd();
            self.forward_refs.entry(shape).or_default().push(site);
            return;
        }

        self.in_progress.insert(shape);
        let func_start = self.code.current_offset();

        match shape.kind() {
            ShapeKind::Struct(fields) => {
                self.fmt.emit_expect_begin_object(&mut self.code);
                let dispatch = build_field_dispatch(fields);
                // NOTE: fmt methods take &mut self.code separately to avoid
                // a simultaneous borrow of self through the closure.
                self.fmt.emit_field_loop(&mut self.code, dispatch, |code, field| {
                    self.compile_deser(field.shape); // may emit a call, not an inline
                    code.emit_store(field.offset);
                });
                self.code.emit_assert_all_required_set(fields);
                self.fmt.emit_expect_end_object(&mut self.code);
            }
            // ...
        }

        self.in_progress.remove(shape);
        let func = self.code.finish_func(func_start);
        // Fix up any forward-reference call sites that pointed here.
        if let Some(sites) = self.forward_refs.remove(shape) {
            for site in sites {
                self.code.patch(site, func);
            }
        }
        self.finished.insert(shape, func);
    }
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

## emit_field_loop for postcard

Postcard is non-self-describing: fields have no names in the wire format, they
appear in declaration order, and there is no framing around a struct. There is
nothing to dispatch on at runtime — the compiler already knows exactly which
fields come in which order.

So `emit_field_loop` for postcard ignores the `dispatch` table entirely and
emits a straight sequence — one call per field, in order:

```rust
impl Format for FadPostcard {
    fn emit_field_loop(
        &self,
        code: &mut Code,
        dispatch: FieldDispatch, // ignored for postcard
        mut per_field: impl FnMut(&mut Code, &FieldInfo),
    ) {
        // No begin-object marker, no key reading, no branching.
        // Just emit each field deserializer in declaration order.
        for field in dispatch.fields_in_order() {
            per_field(code, field);
        }
    }
}
```

For `Friend { age: u32, name: String }` this produces (in pseudocode):

```
deser_Friend:
    deser_u32(out + offset_of(age))     ; read varint, store
    deser_String(out + offset_of(name)) ; read length-prefixed bytes, store
    ret
```

No loop, no hash lookup, no bitset — postcard structs are always fully present
or the input is truncated (an error). The required-field bitset from the JSON
case is also unnecessary: if any field is missing the input simply runs out,
which is caught by the primitive deserializers returning an error.

## emit_field_loop for JSON

JSON is self-describing: fields arrive as `"key": value` pairs in arbitrary
order, possibly with unknown keys, possibly with missing keys. The emitted code
must:

1. Read a key from the input at runtime.
2. Dispatch to the right field deserializer based on that key.
3. Track which required fields have been seen (the bitset).
4. Loop until `}`.

`emit_field_loop` for JSON builds a compile-time dispatch structure from the
known field names, then emits a runtime loop that uses it:

```rust
impl Format for FadJson {
    fn emit_field_loop(
        &self,
        code: &mut Code,
        dispatch: FieldDispatch,
        mut per_field: impl FnMut(&mut Code, &FieldInfo),
    ) {
        // Allocate a bitset on the emitted function's stack frame:
        // one bit per required field, cleared at function entry.
        let bitset = code.alloc_stack_bitset(dispatch.required_field_count());

        // Build a compile-time trie over the known field names.
        // The trie is baked into the emitted code as a branch tree —
        // no heap allocation at runtime.
        let trie = build_trie(dispatch.fields());

        let loop_label = code.begin_loop();

        // Skip whitespace, peek at next byte.
        // If `}`, break.
        self.emit_skip_whitespace(code);
        self.emit_break_if_end_object(code, loop_label);

        // Read the quoted key into a temporary &[u8] on the stack.
        let key_slot = self.emit_read_key(code);

        self.emit_expect_colon(code);

        // Emit the trie as a branch tree. Each leaf calls per_field for
        // that field and sets the corresponding bit in the bitset.
        // The default branch (unknown key) emits a skip-value call.
        emit_trie_dispatch(code, &trie, key_slot, |code, field| {
            per_field(code, field);
            code.emit_set_bit(bitset, field.required_index);
        });

        // Skip optional trailing comma.
        self.emit_skip_comma(code);

        code.end_loop(loop_label);

        // After the loop: assert all required fields were seen.
        code.emit_assert_bitset_full(bitset, dispatch.required_field_count());
    }
}
```

For `Friend { age: u32, name: String }` this produces (in pseudocode):

```
deser_Friend:
    bitset = 0b00          ; two required fields, neither seen yet
loop:
    skip_whitespace
    if peek() == '}': break
    key = read_quoted_key()
    expect_colon()
    if key == "age":        ; \
        deser_u32(&out.age) ;  trie branch for "age"
        bitset |= 0b01      ; /
    elif key == "name":        ; \
        deser_String(&out.name) ;  trie branch for "name"
        bitset |= 0b10          ; /
    else:
        skip_value()        ; unknown field
    skip_comma()
    goto loop
    assert bitset == 0b11  ; both fields present
    ret
```

The trie branch tree for `"age"` vs `"name"` is emitted as a sequence of
byte comparisons baked into the code — no hash table, no string comparison
function call on the hot path.

## Flatten

`#[facet(flatten)]` (like serde's `#[serde(flatten)]`) merges another struct's
fields into the parent's key namespace. The flattened fields appear at the same
level in the wire format, not nested under a key.

```rust
#[derive(Facet)]
struct Metadata {
    version: u32,
    author: String,
}

#[derive(Facet)]
struct Document {
    title: String,
    #[facet(flatten)]
    meta: Metadata,
}
```

Wire JSON:

```json
{ "title": "Hello", "version": 1, "author": "Amos" }
```

Not:

```json
{ "title": "Hello", "meta": { "version": 1, "author": "Amos" } }
```

### How build_field_dispatch handles it

`build_field_dispatch` walks the struct's fields at JIT-compile time. When it
encounters a flattened field, instead of adding the field itself to the trie, it
recurses into the flattened shape and adds *its* fields — with offsets adjusted
by the flattened field's own offset within the parent.

```rust
fn build_field_dispatch(fields: &[FieldInfo]) -> FieldDispatch {
    let mut flat_fields = Vec::new();
    collect_fields(fields, 0, &mut flat_fields);
    FieldDispatch::new(flat_fields)
}

fn collect_fields(fields: &[FieldInfo], base_offset: usize, out: &mut Vec<FlatField>) {
    for field in fields {
        if field.is_flatten() {
            // Recurse into the flattened shape, accumulating the offset.
            collect_fields(field.shape.fields(), base_offset + field.offset, out);
        } else {
            out.push(FlatField {
                name: field.name,
                shape: field.shape,
                offset: base_offset + field.offset, // absolute offset from outer `out`
                required: !field.is_option(),
            });
        }
    }
}
```

The trie is then built over `flat_fields` exactly as before — `"title"`,
`"version"`, and `"author"` all appear as siblings. The store offsets point
directly into the right place within the outer struct's allocation:

```
deser_Document:
    bitset = 0b000         ; title, version, author — all required
loop:
    ...
    if key == "title":
        deser_String(out + offset_of(Document::title))
        bitset |= 0b001
    elif key == "version":
        deser_u32(out + offset_of(Document::meta) + offset_of(Metadata::version))
        bitset |= 0b010
    elif key == "author":
        deser_String(out + offset_of(Document::meta) + offset_of(Metadata::author))
        bitset |= 0b100
    else:
        skip_value()
    ...
    assert bitset == 0b111
    ret
```

No separate `deser_Metadata` call is emitted — the flattened fields are inlined
into the parent's loop. For postcard, `collect_fields` similarly expands
flattened fields in-order into the sequence, with adjusted offsets.
