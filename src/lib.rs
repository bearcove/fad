pub mod arch;
pub mod compiler;
pub mod context;
pub mod format;
pub mod intrinsics;
pub mod json;
pub mod json_intrinsics;
pub mod postcard;

use compiler::CompiledDeser;
use context::{DeserContext, ErrorCode};

// r[impl api.compile]
/// Compile a deserializer for the given shape and format.
pub fn compile_deser(shape: &'static facet::Shape, format: &dyn format::Format) -> CompiledDeser {
    compiler::compile_deser(shape, format)
}

// r[impl api.output]
/// Deserialize a value of type `T` from the given input bytes using a compiled deserializer.
///
/// # Safety
/// The compiled deserializer must have been compiled for the same shape as `T`.
pub fn deserialize<T: for<'a> facet::Facet<'a>>(
    deser: &CompiledDeser,
    input: &[u8],
) -> Result<T, DeserError> {
    let mut ctx = DeserContext::new(input);

    // Allocate output on the stack as MaybeUninit
    let mut output = core::mem::MaybeUninit::<T>::uninit();

    unsafe {
        (deser.func())(output.as_mut_ptr() as *mut u8, &mut ctx);
    }

    if ctx.error.code != 0 {
        let code: ErrorCode = unsafe { core::mem::transmute(ctx.error.code) };
        return Err(DeserError {
            code,
            offset: ctx.error.offset,
        });
    }

    Ok(unsafe { output.assume_init() })
}

/// Error returned by `deserialize`.
#[derive(Debug)]
pub struct DeserError {
    pub code: ErrorCode,
    pub offset: u32,
}

impl core::fmt::Display for DeserError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{} at offset {}", self.code, self.offset)
    }
}

impl std::error::Error for DeserError {}

#[cfg(test)]
mod tests {
    use super::*;
    use facet::Facet;

    #[derive(Facet, Debug, PartialEq)]
    struct Friend {
        age: u32,
        name: String,
    }

    // r[verify deser.postcard.struct]
    #[test]
    fn postcard_flat_struct() {
        // age=42 → postcard varint 0x2A (42 < 128, so single byte)
        // name="Alice" → varint(5)=0x05 + b"Alice"
        let input = [0x2A, 0x05, b'A', b'l', b'i', b'c', b'e'];
        let deser = compile_deser(Friend::SHAPE, &postcard::FadPostcard);
        let result: Friend = deserialize(&deser, &input).unwrap();
        assert_eq!(
            result,
            Friend {
                age: 42,
                name: "Alice".into()
            }
        );
    }

    // r[verify deser.json.struct]
    #[test]
    fn json_flat_struct() {
        let input = br#"{"age": 42, "name": "Alice"}"#;
        let deser = compile_deser(Friend::SHAPE, &json::FadJson);
        let result: Friend = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            Friend {
                age: 42,
                name: "Alice".into()
            }
        );
    }

    // r[verify deser.json.struct]
    #[test]
    fn json_reversed_key_order() {
        let input = br#"{"name": "Alice", "age": 42}"#;
        let deser = compile_deser(Friend::SHAPE, &json::FadJson);
        let result: Friend = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            Friend {
                age: 42,
                name: "Alice".into()
            }
        );
    }

    // r[verify deser.json.struct.unknown-keys]
    #[test]
    fn json_unknown_keys_skipped() {
        let input = br#"{"age": 42, "extra": true, "name": "Alice"}"#;
        let deser = compile_deser(Friend::SHAPE, &json::FadJson);
        let result: Friend = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            Friend {
                age: 42,
                name: "Alice".into()
            }
        );
    }

    // r[verify deser.json.struct]
    #[test]
    fn json_empty_object_missing_fields() {
        let input = b"{}";
        let deser = compile_deser(Friend::SHAPE, &json::FadJson);
        let result = deserialize::<Friend>(&deser, input);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.code, context::ErrorCode::MissingRequiredField);
    }

    // --- Milestone 4: All scalar types ---

    #[derive(Facet, Debug, PartialEq)]
    struct AllScalars {
        a_bool: bool,
        a_u8: u8,
        a_u16: u16,
        a_u32: u32,
        a_u64: u64,
        a_i8: i8,
        a_i16: i16,
        a_i32: i32,
        a_i64: i64,
        a_f32: f32,
        a_f64: f64,
        a_name: String,
    }

    // r[verify deser.postcard.scalar.varint]
    // r[verify deser.postcard.scalar.float]
    // r[verify deser.postcard.scalar.bool]
    #[test]
    fn postcard_all_scalars() {
        use serde::Serialize;

        #[derive(Serialize)]
        struct AllScalarsSerde {
            a_bool: bool,
            a_u8: u8,
            a_u16: u16,
            a_u32: u32,
            a_u64: u64,
            a_i8: i8,
            a_i16: i16,
            a_i32: i32,
            a_i64: i64,
            a_f32: f32,
            a_f64: f64,
            a_name: String,
        }

        let source = AllScalarsSerde {
            a_bool: true,
            a_u8: 200,
            a_u16: 1000,
            a_u32: 70000,
            a_u64: 1_000_000_000_000,
            a_i8: -42,
            a_i16: -1000,
            a_i32: -70000,
            a_i64: -1_000_000_000_000,
            a_f32: 3.14,
            a_f64: 2.718281828459045,
            a_name: "hello".into(),
        };

        let encoded = ::postcard::to_allocvec(&source).unwrap();
        let deser = compile_deser(AllScalars::SHAPE, &postcard::FadPostcard);
        let result: AllScalars = deserialize(&deser, &encoded).unwrap();

        assert_eq!(result.a_bool, true);
        assert_eq!(result.a_u8, 200);
        assert_eq!(result.a_u16, 1000);
        assert_eq!(result.a_u32, 70000);
        assert_eq!(result.a_u64, 1_000_000_000_000);
        assert_eq!(result.a_i8, -42);
        assert_eq!(result.a_i16, -1000);
        assert_eq!(result.a_i32, -70000);
        assert_eq!(result.a_i64, -1_000_000_000_000);
        assert_eq!(result.a_f32, 3.14);
        assert_eq!(result.a_f64, 2.718281828459045);
        assert_eq!(result.a_name, "hello");
    }

    // r[verify deser.json.scalar.integer]
    // r[verify deser.json.scalar.float]
    // r[verify deser.json.scalar.bool]
    #[test]
    fn json_all_scalars() {
        let input = br#"{
            "a_bool": true,
            "a_u8": 200,
            "a_u16": 1000,
            "a_u32": 70000,
            "a_u64": 1000000000000,
            "a_i8": -42,
            "a_i16": -1000,
            "a_i32": -70000,
            "a_i64": -1000000000000,
            "a_f32": 3.14,
            "a_f64": 2.718281828459045,
            "a_name": "hello"
        }"#;

        let deser = compile_deser(AllScalars::SHAPE, &json::FadJson);
        let result: AllScalars = deserialize(&deser, input).unwrap();

        assert_eq!(result.a_bool, true);
        assert_eq!(result.a_u8, 200);
        assert_eq!(result.a_u16, 1000);
        assert_eq!(result.a_u32, 70000);
        assert_eq!(result.a_u64, 1_000_000_000_000);
        assert_eq!(result.a_i8, -42);
        assert_eq!(result.a_i16, -1000);
        assert_eq!(result.a_i32, -70000);
        assert_eq!(result.a_i64, -1_000_000_000_000);
        assert_eq!(result.a_f32, 3.14);
        assert_eq!(result.a_f64, 2.718281828459045);
        assert_eq!(result.a_name, "hello");
    }

    // r[verify deser.json.scalar.bool]
    #[test]
    fn json_bool_true_false() {
        #[derive(Facet, Debug, PartialEq)]
        struct Bools {
            a: bool,
            b: bool,
        }

        let input = br#"{"a": true, "b": false}"#;
        let deser = compile_deser(Bools::SHAPE, &json::FadJson);
        let result: Bools = deserialize(&deser, input).unwrap();
        assert_eq!(result.a, true);
        assert_eq!(result.b, false);
    }

    // r[verify deser.postcard.scalar.bool]
    #[test]
    fn postcard_bool_true_false() {
        #[derive(Facet, Debug, PartialEq)]
        struct Bools {
            a: bool,
            b: bool,
        }

        // postcard: true=1, false=0
        let input = [1u8, 0u8];
        let deser = compile_deser(Bools::SHAPE, &postcard::FadPostcard);
        let result: Bools = deserialize(&deser, &input).unwrap();
        assert_eq!(result.a, true);
        assert_eq!(result.b, false);
    }

    // r[verify deser.json.scalar.integer]
    #[test]
    fn json_boundary_values() {
        #[derive(Facet, Debug, PartialEq)]
        struct Boundaries {
            u8_max: u8,
            u16_max: u16,
            i8_min: i8,
            i8_max: i8,
            i16_min: i16,
            i32_min: i32,
        }

        let input = br#"{
            "u8_max": 255,
            "u16_max": 65535,
            "i8_min": -128,
            "i8_max": 127,
            "i16_min": -32768,
            "i32_min": -2147483648
        }"#;

        let deser = compile_deser(Boundaries::SHAPE, &json::FadJson);
        let result: Boundaries = deserialize(&deser, input).unwrap();
        assert_eq!(result.u8_max, 255);
        assert_eq!(result.u16_max, 65535);
        assert_eq!(result.i8_min, -128);
        assert_eq!(result.i8_max, 127);
        assert_eq!(result.i16_min, -32768);
        assert_eq!(result.i32_min, -2147483648);
    }

    // r[verify deser.json.scalar.integer]
    #[test]
    fn json_u8_out_of_range() {
        #[derive(Facet, Debug)]
        struct Tiny {
            val: u8,
        }

        let input = br#"{"val": 256}"#;
        let deser = compile_deser(Tiny::SHAPE, &json::FadJson);
        let result = deserialize::<Tiny>(&deser, input);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().code, ErrorCode::NumberOutOfRange);
    }

    // r[verify deser.json.scalar.float]
    #[test]
    fn json_float_scientific() {
        #[derive(Facet, Debug, PartialEq)]
        struct Floats {
            a: f64,
            b: f64,
        }

        let input = br#"{"a": 1.5e2, "b": -3.14}"#;
        let deser = compile_deser(Floats::SHAPE, &json::FadJson);
        let result: Floats = deserialize(&deser, input).unwrap();
        assert_eq!(result.a, 150.0);
        assert_eq!(result.b, -3.14);
    }

    // r[verify deser.postcard.scalar.varint]
    #[test]
    fn postcard_boundary_values() {
        use serde::Serialize;

        #[derive(Facet, Debug, PartialEq)]
        struct Boundaries {
            u8_max: u8,
            u64_big: u64,
            i8_min: i8,
            i64_min: i64,
        }

        #[derive(Serialize)]
        struct BoundariesSerde {
            u8_max: u8,
            u64_big: u64,
            i8_min: i8,
            i64_min: i64,
        }

        let source = BoundariesSerde {
            u8_max: 255,
            u64_big: u64::MAX,
            i8_min: i8::MIN,
            i64_min: i64::MIN,
        };

        let encoded = ::postcard::to_allocvec(&source).unwrap();
        let deser = compile_deser(Boundaries::SHAPE, &postcard::FadPostcard);
        let result: Boundaries = deserialize(&deser, &encoded).unwrap();

        assert_eq!(result.u8_max, 255);
        assert_eq!(result.u64_big, u64::MAX);
        assert_eq!(result.i8_min, i8::MIN);
        assert_eq!(result.i64_min, i64::MIN);
    }
}
