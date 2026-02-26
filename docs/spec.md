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
generate machine code at runtime via [dynasmrt](https://crates.io/crates/dynasmrt).

## No IR

An earlier design considered an intermediate representation: the compiler
would emit IR opcodes, and a separate pass would lower them to aarch64 or
x86_64. This adds a layer of indirection that doesn't pay for itself.

The shapes fad deals with — structs, enums, sequences, scalars — map
directly to simple code patterns: loops, branches, calls to intrinsic
functions. There is no optimization pass that benefits from an abstract
representation. No constant folding, no register allocation across basic
blocks, no instruction scheduling. The "optimizations" are all at the shape
level (building a trie at JIT-compile time, choosing a dispatch strategy for
untagged enums) and happen in Rust before any code is emitted.

Instead, the compiler and format crates work with a thin wrapper around
dynasmrt's `Assembler`. Format trait methods like `emit_field_loop` directly
emit machine instructions — real `mov`, `cmp`, `b.eq`, `call` — for the
target architecture. The Rust structs that drive compilation (shapes, field
info, variant info, dispatch tables) *are* the intermediate representation.

This means:

- **Two backends** (aarch64, x86_64) behind `#[cfg(target_arch)]`. Format
  crates must emit for both. In practice the patterns are identical —
  "load byte, compare, branch" — just spelled differently.

- **No interpreter**. The emitted code is always native. This keeps the
  runtime simple and the hot path fast.

- **dynasmrt handles the hard parts**: label management, relocation, memory
  protection (RW→RX), cache flushing on aarch64. fad doesn't need its own
  code buffer abstraction.

- **Intrinsics are Rust functions** called from emitted code via the
  platform's C calling convention. Things like "allocate a Vec", "grow a
  String", "report an error" are too complex to emit inline. The emitted
  code calls into them the same way C code calls libc.

For simplification, we'll assume that:

  * All inputs are `&[u8]` and they are a complete document (no streaming)

The format struct like `FadJson` above must implement a trait that the fad
compiler can call to emit machine code.

## The compiler

The compiler walks the shape tree at JIT-compile time and emits machine code
for each node. The recursion is in the host (Rust) code — the emitted code
is flat.

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

## Enums

```rust
#[derive(Facet)]
enum Animal {
    Cat,
    Dog { name: String, good_boy: bool },
    Parrot(String),
}
```

Enums have three kinds of variants: unit (`Cat`), struct (`Dog`), and tuple
(`Parrot`). The compiler knows all variants and their fields at JIT-compile
time.

### Postcard enums

Postcard encodes enums as a varint discriminant followed by the variant's
payload (if any), in field-declaration order — same as struct fields. The
discriminant is the variant index (0, 1, 2, ...).

Wire bytes for `Animal::Dog { name: "Rex", good_boy: true }`:

```
01              ; variant index 1 (Dog)
03 52 65 78    ; name: length-prefixed "Rex"
01              ; good_boy: true
```

Wire bytes for `Animal::Cat`:

```
00              ; variant index 0 (Cat), no payload
```

Wire bytes for `Animal::Parrot("Polly")`:

```
02                    ; variant index 2 (Parrot)
05 50 6f 6c 6c 79    ; "Polly"
```

The emitted code reads the discriminant, branches to the right variant, then
deserializes its fields in order — exactly like a postcard struct but
preceded by a branch:

```
deser_Animal:
    tag = read_varint()
    switch tag:
        case 0:                                ; Cat
            set_enum_variant(out, 0)
            ret                                ; no fields
        case 1:                                ; Dog
            set_enum_variant(out, 1)
            deser_String(out + offset_of(Dog::name))
            deser_bool(out + offset_of(Dog::good_boy))
            ret
        case 2:                                ; Parrot
            set_enum_variant(out, 2)
            deser_String(out + offset_of(Parrot::0))
            ret
        default:
            error("unknown variant")
```

`set_enum_variant` writes the Rust discriminant value into the enum's tag
slot (known offset and size from the shape). The payload offsets are relative
to the enum's base address, known at JIT-compile time from each variant's
layout.

### JSON enums — externally tagged (default)

The default JSON representation for enums is externally tagged: the variant
name is the key of a single-key object.

```json
"Cat"
```

```json
{ "Dog": { "name": "Rex", "good_boy": true } }
```

```json
{ "Parrot": "Polly" }
```

Unit variants serialize as bare strings. Struct variants become
`{ "VariantName": { fields... } }`. Tuple variants with a single field become
`{ "VariantName": value }`.

The emitted code for externally tagged enums:

```
deser_Animal:
    skip_whitespace()
    if peek() == '"':
        ; Might be a unit variant as a bare string.
        key = read_quoted_string()
        switch key:             ; trie over variant names
            case "Cat":
                set_enum_variant(out, 0)
                ret
            default:
                error("unknown variant")
    expect('{')
    skip_whitespace()
    key = read_quoted_key()
    expect_colon()
    switch key:                 ; trie over variant names
        case "Cat":
            set_enum_variant(out, 0)
            ; Cat might also appear as { "Cat": null } — accept both
            skip_value()
        case "Dog":
            set_enum_variant(out, 1)
            ; Dog is a struct variant — deserialize as object
            deser_Dog_fields(out)
        case "Parrot":
            set_enum_variant(out, 2)
            ; Parrot is a single-field tuple — deserialize the inner value
            deser_String(out + offset_of(Parrot::0))
        default:
            error("unknown variant")
    skip_whitespace()
    expect('}')
    ret
```

The trie over variant names is built at JIT-compile time, same as struct field
dispatch. `deser_Dog_fields` is the same struct-body code as for a standalone
struct (field loop, bitset, trie dispatch over `"name"` and `"good_boy"`).

### JSON enums — adjacently tagged

`#[facet(tag = "type", content = "data")]` (serde calls this adjacently tagged):

```json
{ "type": "Dog", "data": { "name": "Rex", "good_boy": true } }
```

The emitted code reads an object with exactly two known keys (`"type"` and
`"data"`). Since JSON keys can arrive in any order, the emitter handles both
orderings:

```
deser_Animal:
    expect('{')
    ; Read first key
    first_key = read_quoted_key()
    expect_colon()
    if first_key == "type":
        variant = read_quoted_string()    ; e.g. "Dog"
        skip_comma()
        expect_key("data")
        expect_colon()
        dispatch_variant(variant, out)    ; trie branch → deser payload
    elif first_key == "data":
        ; data arrived before type — must buffer or defer.
        ; Option A: buffer the raw JSON value, read type, then parse.
        raw = capture_raw_value()
        skip_comma()
        expect_key("type")
        expect_colon()
        variant = read_quoted_string()
        dispatch_variant_from_raw(variant, out, raw)
    else:
        error("expected \"type\" or \"data\"")
    skip_whitespace()
    expect('}')
    ret
```

The data-before-type case requires buffering the raw value. This is the price
of supporting arbitrary key order in JSON. An alternative is to require
type-first ordering and error otherwise — simpler emitted code, less
compatible.

### JSON enums — internally tagged

`#[facet(tag = "type")]` (no `content`): the tag field lives alongside the
variant's own fields.

```json
{ "type": "Dog", "name": "Rex", "good_boy": true }
```

This only works for struct variants (and unit variants). Tuple variants
don't have named fields for the tag to sit alongside.

The emitted code merges the tag key into the field dispatch trie:

```
deser_Animal:
    expect('{')
    bitset = 0b000         ; type, + variant fields
    variant = UNSET
loop:
    skip_whitespace()
    if peek() == '}': break
    key = read_quoted_key()
    expect_colon()
    if key == "type":
        variant_name = read_quoted_string()
        bitset |= 0b001
    else:
        ; Can't dispatch to a field until we know the variant.
        ; Two strategies:
        ;   1. Require "type" to appear first (simple, fast).
        ;   2. Buffer unknown keys, replay after "type" is known (flexible).
        buffer_key_value(key)
    skip_comma()
    goto loop
    ; After the loop, variant must be known.
    assert variant != UNSET
    switch variant_name:
        case "Cat":
            set_enum_variant(out, 0)
        case "Dog":
            set_enum_variant(out, 1)
            replay_buffered_fields(out, Dog_field_trie)
        ...
    expect('}')
    ret
```

Internally tagged enums are inherently awkward for a JIT compiler: you can't
know which fields to expect until you've seen the tag, but the tag might not
come first. Buffering is the general solution; requiring tag-first is the
fast-path optimization.

### JSON enums — untagged

`#[facet(untagged)]`: no discriminant in the wire format at all. The
deserializer must figure out which variant is present from the value itself.

serde's approach is to buffer the entire value into an intermediate `Content`
enum, then try deserializing each variant from it in order. This loses type
fidelity (integers become `u64` or `i64`, etc.) and is O(variants) attempts.

facet-solver takes a better approach that fad should follow: **analyze variant
shapes at JIT-compile time to build a dispatch strategy, then resolve at
runtime by peeking — not by trial deserialization.**

#### Step 1: classify variants by expected value type

At JIT-compile time, the compiler examines each variant's shape and buckets
it by what JSON value type it expects:

```rust
enum ValueTypeBucket {
    Bool,       // newtype wrapping bool
    Integer,    // newtype wrapping u32, i64, etc.
    Float,      // newtype wrapping f32, f64
    String,     // newtype wrapping String, &str, char, unit variant
    Array,      // newtype wrapping Vec<T>, tuple variant
    Object,     // struct variant, newtype wrapping a struct or map
    Null,       // unit variant (if represented as null)
}
```

For our `Animal` example:

| Variant    | Classification |
|------------|---------------|
| `Cat`      | String (or Null) |
| `Dog { .. }` | Object |
| `Parrot(String)` | String |

#### Step 2: peek at the JSON value type

The emitted code peeks at the first non-whitespace byte to determine the JSON
value type — `{` means object, `"` means string, `[` means array, `t`/`f`
means bool, `n` means null, digit/`-` means number. This is a single byte
comparison, not a parse.

```
deser_Animal:
    skip_whitespace()
    b = peek()
    switch b:
        case '{':  goto object_variants
        case '"':  goto string_variants
        case 'n':  goto null_variants
        default:   error("no variant matches this value type")
```

This eliminates entire categories of variants without touching the input.

#### Step 3: disambiguate within a bucket

Within a bucket, further discrimination depends on the bucket type:

**Object bucket** — if there's only one struct variant (like `Dog`), emit its
deserializer directly. If there are multiple struct variants, use a
constraint-solver approach: scan the top-level keys without parsing values,
and use an inverted index (field name → bitmask of candidate variants) to
narrow down to exactly one variant. This is what facet-solver does with its
`Solver` type.

```
object_variants:
    ; Only Dog expects an object — emit directly.
    set_enum_variant(out, 1)
    deser_Dog_fields(out)
    ret
```

If there were multiple struct variants, the emitted code would scan keys first:

```
object_variants:
    save_pos = input_position()
    candidates = 0b11          ; both StructA and StructB are candidates
    ; Scan top-level keys at depth 1 only
    expect('{')
scan_loop:
    skip_whitespace()
    if peek() == '}': goto resolve
    key = read_quoted_key()
    expect_colon()
    skip_value()               ; skip the value entirely — we only need keys
    candidates &= key_to_candidates[key]   ; inverted index lookup
    if popcount(candidates) == 1: goto resolve
    skip_comma()
    goto scan_loop
resolve:
    input_position = save_pos  ; rewind — now parse for real
    switch candidates:
        case 0b01: deser_StructA(out) ...
        case 0b10: deser_StructB(out) ...
        default: error("ambiguous or no variant matched")
```

The inverted index `key_to_candidates` is built at JIT-compile time from the
known field names of all struct variants in the bucket. Each key maps to a
bitmask of variants that contain that field. ANDing narrows the candidate set.
Typically resolves after 1-2 keys.

**String bucket** — if multiple variants expect strings (like `Cat` as a unit
variant string and `Parrot(String)`), the compiler checks whether they can be
distinguished. Unit variants have a fixed set of known string values; newtype
string variants accept any string. So: read the string, check against the
known unit variant names via trie, and fall through to the newtype variant if
no name matches.

```
string_variants:
    s = read_quoted_string()
    switch s:                   ; trie over unit variant names
        case "Cat":
            set_enum_variant(out, 0)
            ret
    ; No unit variant matched — must be Parrot
    set_enum_variant(out, 2)
    store_string(out + offset_of(Parrot::0), s)
    ret
```

**Scalar buckets** (bool, integer, float) — if only one variant wraps that
scalar type, emit directly. If multiple variants wrap the same scalar type
(e.g., two variants both wrapping `u32`), that's genuinely ambiguous and the
compiler should error at JIT-compile time rather than guess.

#### Full pseudocode for Animal

```
deser_Animal:
    skip_whitespace()
    b = peek()
    if b == '{':
        ; Only Dog expects an object
        set_enum_variant(out, 1)
        deser_Dog_fields(out)
        ret
    if b == '"':
        s = read_quoted_string()
        if s == "Cat":
            set_enum_variant(out, 0)
            ret
        ; Fall through to Parrot
        set_enum_variant(out, 2)
        store_string(out + offset_of(Parrot::0), s)
        ret
    error("no variant matches this value type")
```

No buffering, no trial deserialization, no O(variants) retries. The dispatch
is a peek + trie, same cost structure as externally tagged enums.

## Calling convention

Every emitted deserializer function has the same signature at the machine
level. The caller provides three things:

1. **`out`** — pointer to the output slot (e.g., `*mut Friend`). The callee
   writes deserialized fields at known offsets from this pointer.

2. **`ctx`** — pointer to a `DeserContext` struct that lives on the caller's
   (Rust) stack. This carries the input cursor, error state, and
   format-specific scratch space. Passed by pointer so all emitted functions
   share the same context without copying.

3. **Return** — emitted functions return void. They signal errors by writing
   to the context's error slot and branching to an error exit path. The
   top-level entry point checks the error slot after the emitted code returns.

### DeserContext

```rust
#[repr(C)]
struct DeserContext {
    // Input cursor — all emitted code reads/advances these.
    input_ptr: *const u8,     // current position
    input_end: *const u8,     // one past the last byte

    // Error reporting — set by emitted code or intrinsics on failure.
    error: ErrorSlot,

    // Format-specific scratch space — opaque to the compiler,
    // used by format intrinsics (e.g., JSON key buffering).
    format_state: *mut u8,
}

#[repr(C)]
struct ErrorSlot {
    code: u32,                // 0 = no error, nonzero = error kind
    offset: u32,              // byte offset in input where error occurred
    // Optional: pointer to a heap-allocated error message,
    // written by intrinsics that can afford the allocation.
    detail: *const u8,
    detail_len: usize,
}
```

The context is `#[repr(C)]` so emitted code can access fields at known
offsets. The emitted code loads `ctx.input_ptr` into a register, does its
work, and stores the updated position back before calling an intrinsic or
returning.

### Register assignment

On aarch64:

| Register | Role |
|----------|------|
| `x0`     | `out` — pointer to output slot |
| `x1`     | `ctx` — pointer to `DeserContext` |
| `x19`    | cached `input_ptr` (callee-saved) |
| `x20`    | cached `input_end` (callee-saved) |

On x86_64:

| Register | Role |
|----------|------|
| `rdi`    | `out` — pointer to output slot |
| `rsi`    | `ctx` — pointer to `DeserContext` |
| `r12`    | cached `input_ptr` (callee-saved) |
| `r13`    | cached `input_end` (callee-saved) |

`out` and `ctx` follow the platform's C calling convention for the first two
arguments. This means emitted functions can be called directly from Rust
`extern "C"` code with no thunk.

The input cursor is cached in callee-saved registers for the hot path —
advancing through input bytes is the most frequent operation and must not
require a load/store to `ctx` on every byte. On function entry, the emitted
code loads `ctx.input_ptr` and `ctx.input_end` into the cached registers.
Before calling an intrinsic (which might read or advance the cursor), the
emitted code stores the cached `input_ptr` back to `ctx`. After the
intrinsic returns, it reloads.

### Calling intrinsics

Intrinsics are regular Rust functions with `extern "C"` ABI. They receive
`ctx` as their first argument (so they can read/advance the cursor, report
errors) plus whatever other arguments they need.

```rust
// Example: allocate a chunk for sequence building (see "Sequence construction").
extern "C" fn intrinsic_chunk_alloc(ctx: *mut DeserContext, elem_size: usize, elem_align: usize, capacity: usize) -> *mut u8;

// Example: finalize a chunk chain into a Vec<T>.
extern "C" fn intrinsic_chunk_finalize_vec(ctx: *mut DeserContext, chain: *mut ChunkChain, vec_out: *mut u8, elem_size: usize);

// Example: read a JSON quoted string into a scratch buffer.
extern "C" fn intrinsic_json_read_string(ctx: *mut DeserContext) -> StringRef;
```

The emitted code calls these with a normal `call` instruction to an absolute
address (the function pointer is baked into the emitted code at JIT-compile
time as an immediate or a pc-relative load from a constant pool).

### Calling between emitted functions

When `deser_Node` calls `deser_Vec_Node`, it's a direct call between two
emitted functions. Both use the same convention: `out` in the first argument
register, `ctx` in the second. The caller sets `out` to the address of the
field being deserialized (e.g., `out + offset_of(Node::children)`), keeps
`ctx` as-is, and emits a `call` (or `bl` on aarch64). On return, the caller
reloads the cached input cursor from `ctx` in case the callee advanced it.

## Error handling

Errors are signaled through the context, not through return values. This
keeps the happy path free of branch-on-return-value overhead.

### The error slot approach

When an emitted function or intrinsic encounters an error:

1. Write the error code and input offset to `ctx.error`.
2. Branch to the function's error exit label.

The error exit label is emitted at the end of each function. It restores
callee-saved registers, stores the cached `input_ptr` back to `ctx`, and
returns. The caller then checks `ctx.error.code != 0` and propagates — the
same way, by branching to its own error exit.

```
deser_Friend:
    ; prologue: save callee-saved regs, load cached cursor
    ldr x19, [x1, #CTX_INPUT_PTR]
    ldr x20, [x1, #CTX_INPUT_END]
    ...
    ; call intrinsic (e.g., expect '{')
    str x19, [x1, #CTX_INPUT_PTR]       ; flush cursor
    bl intrinsic_json_expect_lbrace
    ldr x19, [x1, #CTX_INPUT_PTR]       ; reload cursor
    ldr w8, [x1, #CTX_ERROR_CODE]       ; check error
    cbnz w8, .Lerror_exit                ; propagate if set
    ...
.Lerror_exit:
    ; store cursor back, restore callee-saved regs, ret
    str x19, [x1, #CTX_INPUT_PTR]
    ; restore x19, x20 from stack
    ret
```

### Why not setjmp/longjmp?

setjmp/longjmp would make the happy path slightly faster (no error checks
after each intrinsic call) but:

- longjmp skips destructors. If the emitted code has partially constructed
  a value (some fields written, others not), longjmp leaves it in a state
  that can't be safely dropped.
- setjmp has its own cost (saving all registers), and it's called on every
  top-level entry.
- It's harder to provide good error messages (offset, context) when you
  longjmp past everything.

The error-slot approach pays ~1 `cbnz` per intrinsic call on the happy path.
That's one cycle, almost always correctly predicted as not-taken.

### Why not two-register return?

Returning `(value, error)` in two registers is clean but doesn't compose:
the emitted functions write into `out` by pointer, they don't "return" the
deserialized value. And error propagation would still require a branch after
every call — same cost as the error slot, but now the error state is split
between return registers and the context instead of living in one place.

## Format context and scratch space

Format crates sometimes need runtime state that isn't just the input cursor.
JSON needs to:

- Buffer raw values (adjacently tagged enums with data-before-type).
- Buffer key-value pairs (internally tagged enums with tag-not-first).
- Hold a decoded string temporarily (untagged enum string dispatch).

This state lives in the `format_state` pointer inside `DeserContext`. Each
format crate defines its own state struct:

```rust
// For JSON:
#[repr(C)]
struct JsonState {
    // Arena for temporary allocations (buffered values, strings).
    // Bump-allocated, reset after each top-level deserialization.
    arena_base: *mut u8,
    arena_ptr: *mut u8,
    arena_end: *mut u8,

    // Scratch buffer for key scanning (untagged struct disambiguation).
    key_buf: *mut u8,
    key_buf_len: usize,
    key_buf_cap: usize,
}

// For postcard: no state needed — postcard is stateless.
// format_state can be null.
```

### Arena allocation

The arena is bump-allocated: intrinsics call `arena_alloc(ctx, size, align)`
which advances `arena_ptr` and returns the old pointer. If the arena is
exhausted, the intrinsic grows it (realloc the backing allocation). The arena
is reset (ptr = base) after each top-level `deser.call()` — all temporary
allocations are freed in bulk.

This is cheap enough that buffering a JSON value for later replay (adjacently
tagged, internally tagged) is not a performance concern. The buffered data is
just the raw bytes plus a small index of key offsets — no parsing, no
intermediate `Value` type.

### Who allocates the context?

The Rust entry point — the safe wrapper around `deser.call()` — allocates
`DeserContext` on the stack, initializes the input cursor from the `&[u8]`
argument, zero-initializes the error slot, and sets `format_state` to point
to a format-specific state struct (also stack-allocated, or heap-allocated
for the arena backing). Then it calls the emitted function with `out` and
`ctx`.

```rust
impl CompiledDeser {
    pub fn call(&self, out: &mut MaybeUninit<T>, input: &[u8]) -> Result<(), DeserError> {
        let mut json_state = JsonState::new();    // stack + small heap alloc for arena
        let mut ctx = DeserContext {
            input_ptr: input.as_ptr(),
            input_end: input.as_ptr().add(input.len()),
            error: ErrorSlot::default(),
            format_state: &mut json_state as *mut _ as *mut u8,
        };
        unsafe {
            (self.fn_ptr)(out as *mut _ as *mut u8, &mut ctx);
        }
        if ctx.error.code != 0 {
            Err(DeserError::from_slot(&ctx.error, input))
        } else {
            Ok(())
        }
    }
}
```

Postcard passes a null `format_state` (or a zero-sized struct pointer).
If a format needs no scratch space, it pays no allocation cost.

## Sequence construction

Deserializing `Vec<T>`, `HashSet<T>`, or any variable-length collection
requires constructing elements in memory before knowing the final count (for
JSON) or after reading a length prefix that may not be exact (for postcard).

### The problem with Vec::push

The naive approach — allocate a `Vec<T>`, push elements one at a time — has
a fatal flaw for JIT-emitted code: `Vec::push` may reallocate, which
invalidates all pointers into the Vec's buffer. If we're constructing
element N in-place (writing its fields directly to the buffer at a known
offset) and a reallocation moves the buffer, we're writing to freed memory.

We could work around this by always finishing one element before starting the
next. But that forces an extra copy if elements are complex (structs with
many fields) and prevents future optimizations like prefetching.

### Chunk chains

Instead, fad uses a **chunk chain**: a linked list of fixed-size buffers.
Elements are constructed in-place in the current chunk. When a chunk fills
up, a new one is allocated and linked. No existing chunk ever moves.

```rust
#[repr(C)]
struct ChunkChain {
    // Current chunk — elements are written here.
    current: *mut u8,
    current_len: usize,     // elements written so far in this chunk
    current_cap: usize,     // element capacity of this chunk

    // Linked list of full chunks (newest first).
    full_chunks: *mut FullChunk,
    total_len: usize,       // total elements across all chunks
}

#[repr(C)]
struct FullChunk {
    next: *mut FullChunk,
    data: *mut u8,
    len: usize,             // always == capacity (chunk was full)
    cap: usize,
}
```

### How the emitted code uses it

For `Vec<Friend>` deserialization:

```
deser_Vec_Friend:
    ; Allocate the chain (on the format arena or heap).
    ; size_hint comes from the format: postcard gives exact count,
    ; JSON gives 0 (unknown).
    call intrinsic_chain_new(ctx, size_of(Friend), align_of(Friend), size_hint)

    ; chain_ptr is in a callee-saved register or spilled to stack.
loop:
    ; Format-specific: check for end of array.
    ; (JSON: skip_whitespace, break if ']')
    ; (postcard: decrement remaining count, break if 0)

    ; Get a slot to write into. If current chunk is full, this
    ; allocates a new chunk — but never moves existing ones.
    slot = call intrinsic_chain_next_slot(ctx, chain_ptr, size_of(Friend), align_of(Friend))

    ; Deserialize the element in-place at `slot`.
    ; `slot` stays valid for the entire element's construction.
    call deser_Friend(slot, ctx)
    check_error

    ; Mark the slot as committed (increment current_len).
    call intrinsic_chain_commit(ctx, chain_ptr)

    goto loop

    ; Finalize: build the Vec from the chain.
    call intrinsic_chain_to_vec(ctx, chain_ptr, out, size_of(Friend), align_of(Friend))
    ret
```

### Finalization

`intrinsic_chain_to_vec` builds the final `Vec<T>` from the chain:

- **One chunk (common case)**: The chunk's buffer *is* the Vec's buffer.
  Transfer ownership directly — set the Vec's pointer, length, and capacity
  from the chunk. No copy. This requires that the chunk was allocated with
  the same allocator and layout that Vec expects, which the intrinsic
  ensures (using `alloc::alloc::alloc` with the right layout, and checking
  at JIT-compile time that Vec's layout is `{ptr, len, cap}`).

- **Multiple chunks**: Allocate a single buffer of `total_len` capacity,
  `memcpy` each chunk's data into it in order, build the Vec from that.
  This is one copy of the total data — the same cost as if we'd known the
  size upfront and allocated once.

### Why not a size hint + single allocation?

Postcard *does* give an exact element count (length-prefixed). In that case
the chain is initialized with a single chunk of exactly the right capacity,
and finalization is always the zero-copy one-chunk path.

JSON doesn't know the count upfront. A heuristic size hint (e.g., based on
remaining input bytes) could over-allocate. The chunk chain avoids guessing:
start with a reasonable chunk size (e.g., 16 elements), double on each new
chunk, and pay at most one copy at the end.

### Drop safety

If deserialization fails mid-array (error on element N), the chain contains
N-1 fully constructed elements and possibly a partially constructed Nth. The
error path must:

1. Drop the N-1 complete elements (by calling their drop glue in order).
2. Drop any partially constructed fields of element N.
3. Free the chain's buffers.

The chain tracks `current_len` (committed elements) separately from the
write cursor, so it knows exactly how many elements to drop. The partially
constructed element is handled by the same partial-drop mechanism used for
structs (a bitset of which fields were written — see error handling).
