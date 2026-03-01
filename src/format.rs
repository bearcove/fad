use dynasmrt::DynamicLabel;
use facet::{MapDef, ScalarType, StructKind};

use crate::arch::EmitCtx;
use crate::malum::{StringOffsets, VecOffsets};

// r[impl no-ir.format-trait]

/// Resolved default information for a field.
#[derive(Clone, Copy)]
pub struct DefaultInfo {
    /// Pointer to the intrinsic trampoline (fad_field_default_trait or fad_field_default_custom).
    pub trampoline: *const u8,
    /// Pointer to the actual default function (from TypeOps or custom expression).
    pub fn_ptr: *const u8,
    /// For indirect types (generic containers), the shape needed to construct OxPtrUninit.
    /// When Some, the 3-argument trampoline `fad_field_default_indirect` is used.
    pub shape: Option<&'static facet::Shape>,
}

/// Information about a struct field needed during code emission.
pub struct FieldEmitInfo {
    /// Byte offset of this field within the output struct.
    pub offset: usize,
    /// The facet shape of this field.
    pub shape: &'static facet::Shape,
    /// The field name (for formats that use named fields).
    pub name: &'static str,
    /// Index of this field for required-field bitset tracking.
    pub required_index: usize,
    /// If set, this field has a default value and is optional in JSON.
    pub default: Option<DefaultInfo>,
}

// r[impl deser.skip]

/// Information about a skipped field that needs default initialization.
pub struct SkippedFieldInfo {
    /// Byte offset of this field within the output struct.
    pub offset: usize,
    /// Default trampoline + function pointer for initializing this field.
    pub default: DefaultInfo,
}

// r[impl deser.enum.variant-kinds]

/// The kind of an enum variant.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariantKind {
    Unit,
    Tuple,
    Struct,
}

impl VariantKind {
    pub fn from_struct_type(st: &facet::StructType) -> Self {
        match st.kind {
            StructKind::Unit => VariantKind::Unit,
            StructKind::Struct => VariantKind::Struct,
            StructKind::TupleStruct | StructKind::Tuple => VariantKind::Tuple,
        }
    }
}

/// Information about an enum variant needed during code emission.
pub struct VariantEmitInfo {
    /// Variant index (0-based, used as wire discriminant for postcard).
    pub index: usize,
    /// Variant name (for JSON key matching).
    pub name: &'static str,
    /// Rust discriminant value to write to the tag slot.
    pub rust_discriminant: i64,
    /// Fields of this variant (offsets are absolute from enum base).
    pub fields: Vec<FieldEmitInfo>,
    /// Variant kind.
    pub kind: VariantKind,
}

/// A wire format that knows how to emit deserialization code.
///
/// Each method emits machine code into the `EmitCtx` that will, at runtime,
/// read from the input buffer and write to the output struct.
pub trait Decoder {
    /// Extra bytes of stack space this format needs beyond the base frame.
    /// JSON needs space for bitset, key_ptr, key_len, peek_byte.
    fn extra_stack_space(&self, _fields: &[FieldEmitInfo]) -> u32 {
        0
    }

    /// Extra stack bytes needed when deserializing a Vec.
    /// This is separate from `extra_stack_space` because Vec functions
    /// need stack slots for buf, count, counter, and saved out pointer.
    fn vec_extra_stack_space(&self) -> u32 {
        0
    }

    /// Extra stack bytes needed when deserializing a Map.
    /// Uses the same buffer infrastructure as Vec, so the same stack layout.
    fn map_extra_stack_space(&self) -> u32 {
        0
    }

    /// Whether nested struct fields should be inlined into the parent function.
    ///
    /// Positional formats (postcard) return true — nested structs are just
    /// "more fields at an offset", so inlining avoids the function call overhead.
    /// Keyed formats (JSON) return false — each struct level needs its own
    /// key-matching state machine.
    fn supports_inline_nested(&self) -> bool {
        false
    }

    /// Whether this format can safely treat string slices as trusted UTF-8 input.
    fn supports_trusted_utf8_input(&self) -> bool {
        false
    }

    /// Optional IR-lowering view of this decoder.
    ///
    /// Formats that implement [`IrDecoder`] should override this to return
    /// `Some(self)`, allowing runtime backend selection without downcasting.
    fn as_ir_decoder(&self) -> Option<&dyn IrDecoder> {
        None
    }

    /// Emit code to deserialize all fields of a struct.
    ///
    /// The format controls field ordering. For postcard, this just iterates
    /// fields in declaration order. For JSON, this would emit a key-dispatch loop.
    ///
    /// `deny_unknown_fields`: if true, error on unrecognized keys instead of skipping.
    fn emit_struct_fields(
        &self,
        ectx: &mut EmitCtx,
        fields: &[FieldEmitInfo],
        deny_unknown_fields: bool,
        emit_field: &mut dyn FnMut(&mut EmitCtx, &FieldEmitInfo),
    );

    // r[impl scalar.types]

    /// Emit code to read a scalar value and write it to `out + offset`.
    fn emit_read_scalar(&self, ectx: &mut EmitCtx, offset: usize, scalar_type: ScalarType);

    /// Emit code to read a String and write it to `out + offset`.
    ///
    /// `string_offsets` provides the discovered `(ptr, len, cap)` field offsets
    /// within String's memory layout, enabling direct writes.
    fn emit_read_string(
        &self,
        ectx: &mut EmitCtx,
        offset: usize,
        scalar_type: ScalarType,
        string_offsets: &StringOffsets,
    );

    /// Emit code to deserialize an enum value.
    ///
    /// The format reads the wire discriminant (varint for postcard, string key
    /// for JSON), dispatches to the matching variant, and calls
    /// `emit_variant_body` for each variant's payload.
    fn emit_enum(
        &self,
        ectx: &mut EmitCtx,
        variants: &[VariantEmitInfo],
        emit_variant_body: &mut dyn FnMut(&mut EmitCtx, &VariantEmitInfo),
    );

    // r[impl deser.json.enum.adjacent]

    /// Emit code to deserialize an adjacently tagged enum.
    ///
    /// Wire format: `{ "tag_key": "VariantName", "content_key": value }`
    fn emit_enum_adjacent(
        &self,
        _ectx: &mut EmitCtx,
        _variants: &[VariantEmitInfo],
        _tag_key: &'static str,
        _content_key: &'static str,
        _emit_variant_body: &mut dyn FnMut(&mut EmitCtx, &VariantEmitInfo),
    ) {
        panic!("adjacently tagged enums not supported by this format");
    }

    // r[impl deser.json.enum.internal]

    /// Emit code to deserialize an internally tagged enum.
    ///
    /// Wire format: `{ "tag_key": "VariantName", ...variant_fields... }`
    ///
    /// Takes two callbacks:
    /// - `emit_variant_discriminant`: writes the Rust discriminant
    /// - `emit_variant_field`: deserializes one field (compiler resolves nested labels)
    fn emit_enum_internal(
        &self,
        _ectx: &mut EmitCtx,
        _variants: &[VariantEmitInfo],
        _tag_key: &'static str,
        _emit_variant_discriminant: &mut dyn FnMut(&mut EmitCtx, &VariantEmitInfo),
        _emit_variant_field: &mut dyn FnMut(&mut EmitCtx, &VariantEmitInfo, &FieldEmitInfo),
    ) {
        panic!("internally tagged enums not supported by this format");
    }

    // r[impl deser.json.enum.untagged]

    /// Emit code to deserialize an untagged enum.
    ///
    /// Untagged enums use value-type bucketing + peek dispatch to determine which
    /// variant to deserialize. Object bucket disambiguation uses a lowered solver
    /// (bitmask narrowed by key presence).
    fn emit_enum_untagged(
        &self,
        _ectx: &mut EmitCtx,
        _variants: &[VariantEmitInfo],
        _emit_variant_body: &mut dyn FnMut(&mut EmitCtx, &VariantEmitInfo),
    ) {
        panic!("untagged enums not supported by this format");
    }

    /// Emit code to deserialize an Option<T> value.
    ///
    /// The format reads the wire discriminant (postcard: 0x00/0x01 byte, JSON:
    /// null peek), then either:
    /// - None: calls `fad_option_init_none(init_none_fn, out + offset)`
    /// - Some: redirects `out` to a scratch area, calls `emit_inner` to
    ///   deserialize T, then calls `fad_option_init_some(init_some_fn, out + offset, scratch)`
    ///
    /// `scratch_offset`: stack offset where inner T can be temporarily deserialized.
    fn emit_option(
        &self,
        ectx: &mut EmitCtx,
        offset: usize,
        init_none_fn: *const u8,
        init_some_fn: *const u8,
        scratch_offset: u32,
        emit_inner: &mut dyn FnMut(&mut EmitCtx),
    );

    /// Emit code to deserialize struct fields from an already-open JSON object.
    ///
    /// Used by internally tagged enums: after reading the tag key-value pair,
    /// the remaining fields are deserialized from the same object level.
    /// Starts with comma_or_end (no `{`), ends without consuming `}`.
    fn emit_struct_fields_continuation(
        &self,
        _ectx: &mut EmitCtx,
        _fields: &[FieldEmitInfo],
        _deny_unknown_fields: bool,
        _emit_field: &mut dyn FnMut(&mut EmitCtx, &FieldEmitInfo),
    ) {
        panic!("struct fields continuation not supported by this format");
    }

    /// Emit code to deserialize positional (tuple / fixed-size array) fields.
    ///
    /// Unlike `emit_struct_fields`, positional fields have no names and are
    /// read in declaration order — all N elements must be present or an error
    /// occurs. No required-field bitset tracking is needed.
    ///
    /// For JSON: reads `[e0, e1, ..., eN-1]`.
    /// For postcard (default): elements in order, same as `emit_struct_fields`.
    fn emit_positional_fields(
        &self,
        ectx: &mut EmitCtx,
        fields: &[FieldEmitInfo],
        emit_field: &mut dyn FnMut(&mut EmitCtx, &FieldEmitInfo),
    ) {
        for field in fields {
            emit_field(ectx, field);
        }
    }

    /// Emit code to deserialize a `Vec<T>` (sequence of elements).
    ///
    /// The format reads the wire sequence (postcard: varint count + elements,
    /// JSON: `[` elements `]`), allocates a buffer, deserializes elements into it,
    /// and writes the Vec's `(ptr, len, cap)` at `out + offset` using the
    /// discovered `vec_offsets`.
    ///
    /// `emit_elem`: callback to emit deserialization of one element at the current
    /// `out` pointer (which has been redirected to the element slot).
    #[allow(clippy::too_many_arguments)]
    fn emit_vec(
        &self,
        _ectx: &mut EmitCtx,
        _offset: usize,
        _elem_shape: &'static facet::Shape,
        _elem_label: Option<DynamicLabel>,
        _vec_offsets: &VecOffsets,
        _option_scratch_offset: u32,
        _emit_elem: &mut dyn FnMut(&mut EmitCtx),
    ) {
        panic!("Vec deserialization not supported by this format");
    }

    /// Emit code to deserialize a `Map<K, V>` (key-value collection).
    ///
    /// Two-pass approach: collect (K, V) pairs into a temporary buffer, then call
    /// `map_def.vtable.from_pair_slice` to construct the final map in one shot.
    ///
    /// - `map_def`: facet vtable describing the map type and pair layout.
    /// - `emit_key`: callback to emit deserialization of one key at `out` offset 0.
    /// - `emit_value`: callback to emit deserialization of one value at `out` offset 0.
    #[allow(clippy::too_many_arguments)]
    fn emit_map(
        &self,
        _ectx: &mut EmitCtx,
        _offset: usize,
        _map_def: &'static MapDef,
        _k_shape: &'static facet::Shape,
        _v_shape: &'static facet::Shape,
        _k_label: Option<DynamicLabel>,
        _v_label: Option<DynamicLabel>,
        _option_scratch_offset: u32,
        _emit_key: &mut dyn FnMut(&mut EmitCtx),
        _emit_value: &mut dyn FnMut(&mut EmitCtx),
    ) {
        panic!("Map deserialization not supported by this format");
    }
}

// =============================================================================
// IR decoder — RVSDG lowering (coexists with Decoder during migration)
// =============================================================================

use crate::ir::RegionBuilder;

/// Information about a struct field needed during IR lowering.
///
/// IR-side equivalent of [`FieldEmitInfo`]. No function pointers — defaults
/// are represented as IR nodes rather than raw trampolines.
pub struct FieldLowerInfo {
    /// Byte offset of this field within the output struct.
    pub offset: usize,
    /// The facet shape of this field.
    pub shape: &'static facet::Shape,
    /// The field name (for formats that use named fields).
    pub name: &'static str,
    /// Index of this field for required-field bitset tracking.
    pub required_index: usize,
    /// Whether this field has a default value (details resolved during lowering).
    pub has_default: bool,
}

/// Information about an enum variant needed during IR lowering.
///
/// IR-side equivalent of [`VariantEmitInfo`].
pub struct VariantLowerInfo {
    /// Variant index (0-based, used as wire discriminant for postcard).
    pub index: usize,
    /// Variant name (for JSON key matching).
    pub name: &'static str,
    /// Rust discriminant value to write to the tag slot.
    pub rust_discriminant: i64,
    /// Fields of this variant.
    pub fields: Vec<FieldLowerInfo>,
    /// Variant kind.
    pub kind: VariantKind,
}

// r[impl ir.format-trait]
// r[impl ir.format-trait.stateless]

/// A wire format that knows how to lower deserialization logic into RVSDG nodes.
///
/// This trait coexists with [`Decoder`] during the migration period. Formats
/// implement `IrDecoder` to produce IR instead of machine code. The compiler
/// will eventually have a `compile_decoder_via_ir()` path that uses this trait,
/// and once all formats are migrated and benchmarked, `Decoder` is removed.
///
/// Implementations are stateless at JIT-compile time: they produce IR nodes
/// but hold no mutable state between calls. Runtime state lives in the
/// `format_state` pointer inside `DeserContext`.
pub trait IrDecoder {
    // r[impl ir.format-trait.lower]

    /// Lower struct field iteration into RVSDG nodes.
    ///
    /// The format controls field ordering. Positional formats (postcard)
    /// emit sequential reads. Keyed formats (JSON) emit a theta node
    /// containing key dispatch.
    ///
    /// `lower_field` is called for each field — the format decides traversal
    /// order but the compiler decides how to lower each field's value.
    fn lower_struct_fields(
        &self,
        builder: &mut RegionBuilder<'_>,
        fields: &[FieldLowerInfo],
        deny_unknown_fields: bool,
        lower_field: &mut dyn FnMut(&mut RegionBuilder<'_>, &FieldLowerInfo),
    );

    /// Lower a scalar read into RVSDG nodes.
    ///
    /// Produces nodes that read a scalar value from the input and write it
    /// to `out + offset`. The format determines the wire encoding (varint,
    /// fixed-width, text number, etc.).
    fn lower_read_scalar(
        &self,
        builder: &mut RegionBuilder<'_>,
        offset: usize,
        scalar_type: ScalarType,
    );

    /// Lower a string read into RVSDG nodes.
    ///
    /// Produces nodes that read a string from the input, allocate it, and
    /// write it to `out + offset`.
    fn lower_read_string(
        &self,
        builder: &mut RegionBuilder<'_>,
        offset: usize,
        scalar_type: ScalarType,
    );

    /// Lower positional (tuple / fixed-size array) field reads into RVSDG nodes.
    ///
    /// Unlike `lower_struct_fields`, positional fields have no names and are
    /// read in declaration order. Default impl iterates fields sequentially.
    fn lower_positional_fields(
        &self,
        builder: &mut RegionBuilder<'_>,
        fields: &[FieldLowerInfo],
        lower_field: &mut dyn FnMut(&mut RegionBuilder<'_>, &FieldLowerInfo),
    ) {
        for field in fields {
            lower_field(builder, field);
        }
    }

    /// Lower enum deserialization into RVSDG nodes.
    ///
    /// The format reads the wire discriminant, then dispatches to the matching
    /// variant via a gamma node.
    fn lower_enum(
        &self,
        _builder: &mut RegionBuilder<'_>,
        _variants: &[VariantLowerInfo],
        _lower_variant_body: &mut dyn FnMut(&mut RegionBuilder<'_>, &VariantLowerInfo),
    ) {
        panic!("enum lowering not yet implemented for this format");
    }

    /// Lower Option deserialization into RVSDG nodes.
    ///
    /// The format reads None/Some discriminant and produces a gamma node
    /// with two branches.
    fn lower_option(
        &self,
        _builder: &mut RegionBuilder<'_>,
        _offset: usize,
        _init_none_fn: *const u8,
        _init_some_fn: *const u8,
        _lower_inner: &mut dyn FnMut(&mut RegionBuilder<'_>),
    ) {
        panic!("option lowering not yet implemented for this format");
    }

    /// Lower Vec deserialization into RVSDG nodes.
    ///
    /// The format reads the element count, allocates a buffer, and produces
    /// a theta node to loop over elements.
    fn lower_vec(
        &self,
        _builder: &mut RegionBuilder<'_>,
        _offset: usize,
        _elem_shape: &'static facet::Shape,
        _lower_elem: &mut dyn FnMut(&mut RegionBuilder<'_>),
    ) {
        panic!("vec lowering not yet implemented for this format");
    }

    /// Lower Map deserialization into RVSDG nodes.
    ///
    /// The format reads the pair count, allocates a buffer, and produces
    /// a theta node to loop over key-value pairs.
    fn lower_map(
        &self,
        _builder: &mut RegionBuilder<'_>,
        _offset: usize,
        _map_def: &'static MapDef,
        _lower_key: &mut dyn FnMut(&mut RegionBuilder<'_>),
        _lower_value: &mut dyn FnMut(&mut RegionBuilder<'_>),
    ) {
        panic!("map lowering not yet implemented for this format");
    }
}

// =============================================================================
// Encoder — serialization direction
// =============================================================================

/// Information about a struct field needed during encode code emission.
pub struct FieldEncodeInfo {
    /// Byte offset of this field within the input struct.
    pub offset: usize,
    /// The facet shape of this field.
    pub shape: &'static facet::Shape,
    /// The field name (for keyed formats like JSON).
    pub name: &'static str,
}

/// Information about an enum variant needed during encode code emission.
pub struct VariantEncodeInfo {
    /// Variant index (0-based, used as wire discriminant for postcard).
    pub index: usize,
    /// Variant name (for JSON key output).
    pub name: &'static str,
    /// Rust discriminant value to read from the enum's tag slot.
    pub rust_discriminant: i64,
    /// Fields of this variant (offsets are absolute from enum base).
    pub fields: Vec<FieldEncodeInfo>,
    /// Variant kind.
    pub kind: VariantKind,
}

/// A wire format that knows how to emit serialization code.
///
/// Each method emits machine code into the `EmitCtx` that will, at runtime,
/// read from a typed struct and write wire-format bytes to an output buffer.
///
/// The JIT function signature is:
///   `unsafe extern "C" fn(input: *const u8, ctx: *mut EncodeContext)`
/// where `input` points to the struct being serialized, and `ctx` manages
/// the growable output buffer.
pub trait Encoder {
    /// Extra bytes of stack space this format needs beyond the base frame.
    fn extra_stack_space(&self, _fields: &[FieldEncodeInfo]) -> u32 {
        0
    }

    /// Whether nested struct fields should be inlined into the parent function.
    ///
    /// Same semantics as `Decoder::supports_inline_nested`: positional formats
    /// (postcard) inline, keyed formats (JSON) emit separate functions per struct.
    fn supports_inline_nested(&self) -> bool {
        false
    }

    /// Emit code to serialize all fields of a struct.
    ///
    /// The format controls field ordering and framing. For postcard, this iterates
    /// fields in declaration order and encodes each. For JSON, this emits `{`,
    /// key-value pairs with commas, and `}`.
    ///
    /// `emit_field` is called for each field — the encoder decides the traversal
    /// order but the compiler decides how to encode each field's value.
    fn emit_encode_struct_fields(
        &self,
        _ectx: &mut EmitCtx,
        _fields: &[FieldEncodeInfo],
        _emit_field: &mut dyn FnMut(&mut EmitCtx, &FieldEncodeInfo),
    ) {
        panic!("struct serialization not supported by this format");
    }

    /// Emit code to encode a scalar value read from `input + offset`.
    fn emit_encode_scalar(&self, _ectx: &mut EmitCtx, _offset: usize, _scalar_type: ScalarType) {
        panic!("scalar serialization not supported by this format");
    }

    /// Emit code to encode a string read from `input + offset`.
    ///
    /// The encoder reads `(ptr, len)` from the String's memory layout using
    /// `string_offsets`, then writes the format-appropriate representation
    /// (length-prefixed for postcard, quoted+escaped for JSON).
    fn emit_encode_string(
        &self,
        _ectx: &mut EmitCtx,
        _offset: usize,
        _scalar_type: ScalarType,
        _string_offsets: &StringOffsets,
    ) {
        panic!("string serialization not supported by this format");
    }

    /// Emit code to serialize an enum value.
    ///
    /// The encoder reads the Rust discriminant from the enum's tag slot,
    /// dispatches to the matching variant, writes the wire discriminant
    /// (varint for postcard, string key for JSON), then calls `emit_variant_body`
    /// for the variant's payload.
    fn emit_encode_enum(
        &self,
        _ectx: &mut EmitCtx,
        _variants: &[VariantEncodeInfo],
        _emit_variant_body: &mut dyn FnMut(&mut EmitCtx, &VariantEncodeInfo),
    ) {
        panic!("enum serialization not supported by this format");
    }

    /// Emit code to serialize an adjacently tagged enum.
    ///
    /// Wire format: `{ "tag_key": "VariantName", "content_key": value }`
    fn emit_encode_enum_adjacent(
        &self,
        _ectx: &mut EmitCtx,
        _variants: &[VariantEncodeInfo],
        _tag_key: &'static str,
        _content_key: &'static str,
        _emit_variant_body: &mut dyn FnMut(&mut EmitCtx, &VariantEncodeInfo),
    ) {
        panic!("adjacently tagged enum serialization not supported by this format");
    }

    /// Emit code to serialize an internally tagged enum.
    ///
    /// Wire format: `{ "tag_key": "VariantName", ...variant_fields... }`
    fn emit_encode_enum_internal(
        &self,
        _ectx: &mut EmitCtx,
        _variants: &[VariantEncodeInfo],
        _tag_key: &'static str,
        _emit_variant_body: &mut dyn FnMut(&mut EmitCtx, &VariantEncodeInfo),
    ) {
        panic!("internally tagged enum serialization not supported by this format");
    }

    /// Emit code to serialize an untagged enum.
    ///
    /// The encoder reads the Rust discriminant, dispatches to the matching variant,
    /// and serializes the variant body directly without writing any tag.
    fn emit_encode_enum_untagged(
        &self,
        _ectx: &mut EmitCtx,
        _variants: &[VariantEncodeInfo],
        _emit_variant_body: &mut dyn FnMut(&mut EmitCtx, &VariantEncodeInfo),
    ) {
        panic!("untagged enum serialization not supported by this format");
    }

    /// Emit code to serialize an Option<T> value.
    ///
    /// The encoder reads the option discriminant to check None vs Some,
    /// then either writes the None representation (postcard: 0x00, JSON: null)
    /// or writes the Some framing and calls `emit_inner` for the payload.
    fn emit_encode_option(
        &self,
        _ectx: &mut EmitCtx,
        _offset: usize,
        _emit_inner: &mut dyn FnMut(&mut EmitCtx),
    ) {
        panic!("Option serialization not supported by this format");
    }

    /// Emit code to serialize a `Vec<T>`.
    ///
    /// The encoder reads `(ptr, len)` from the Vec's memory layout using
    /// `vec_offsets`, then writes the format framing (postcard: varint count,
    /// JSON: `[` ... `]`) and calls `emit_elem` in a loop for each element.
    fn emit_encode_vec(
        &self,
        _ectx: &mut EmitCtx,
        _offset: usize,
        _elem_shape: &'static facet::Shape,
        _elem_label: Option<DynamicLabel>,
        _vec_offsets: &VecOffsets,
        _emit_elem: &mut dyn FnMut(&mut EmitCtx),
    ) {
        panic!("Vec serialization not supported by this format");
    }

    /// Emit code to serialize a `Map<K, V>`.
    ///
    /// The encoder iterates map entries via the map vtable and calls `emit_key`
    /// and `emit_value` for each pair, with format-appropriate framing.
    #[allow(clippy::too_many_arguments)]
    fn emit_encode_map(
        &self,
        _ectx: &mut EmitCtx,
        _offset: usize,
        _map_def: &'static MapDef,
        _k_shape: &'static facet::Shape,
        _v_shape: &'static facet::Shape,
        _k_label: Option<DynamicLabel>,
        _v_label: Option<DynamicLabel>,
        _emit_key: &mut dyn FnMut(&mut EmitCtx),
        _emit_value: &mut dyn FnMut(&mut EmitCtx),
    ) {
        panic!("Map serialization not supported by this format");
    }
}
