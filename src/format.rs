use facet::ScalarType;

use crate::arch::EmitCtx;

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
}
