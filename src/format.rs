use dynasmrt::DynamicLabel;
use facet::{ScalarType, StructKind};

use crate::arch::EmitCtx;
use crate::malum::VecOffsets;

// r[impl no-ir.format-trait]

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
pub trait Format {
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

    /// Whether nested struct fields should be inlined into the parent function.
    ///
    /// Positional formats (postcard) return true — nested structs are just
    /// "more fields at an offset", so inlining avoids the function call overhead.
    /// Keyed formats (JSON) return false — each struct level needs its own
    /// key-matching state machine.
    fn supports_inline_nested(&self) -> bool {
        false
    }

    /// Emit code to deserialize all fields of a struct.
    ///
    /// The format controls field ordering. For postcard, this just iterates
    /// fields in declaration order. For JSON, this would emit a key-dispatch loop.
    fn emit_struct_fields(
        &self,
        ectx: &mut EmitCtx,
        fields: &[FieldEmitInfo],
        emit_field: &mut dyn FnMut(&mut EmitCtx, &FieldEmitInfo),
    );

    // r[impl scalar.types]

    /// Emit code to read a scalar value and write it to `out + offset`.
    fn emit_read_scalar(&self, ectx: &mut EmitCtx, offset: usize, scalar_type: ScalarType);

    /// Emit code to read a String and write it to `out + offset`.
    fn emit_read_string(&self, ectx: &mut EmitCtx, offset: usize);

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
        _emit_field: &mut dyn FnMut(&mut EmitCtx, &FieldEmitInfo),
    ) {
        panic!("struct fields continuation not supported by this format");
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
}
