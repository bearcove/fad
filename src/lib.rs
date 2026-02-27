pub mod arch;
pub mod compiler;
pub mod context;
pub mod format;
pub mod intrinsics;
pub mod json;
pub mod json_intrinsics;
pub mod postcard;
pub mod solver;

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

    // --- Milestone 5: Nested structs ---

    #[derive(Facet, Debug, PartialEq)]
    struct Address {
        city: String,
        zip: u32,
    }

    #[derive(Facet, Debug, PartialEq)]
    struct Person {
        name: String,
        age: u32,
        address: Address,
    }

    // r[verify deser.nested-struct]
    // r[verify deser.nested-struct.offset]
    #[test]
    fn postcard_nested_struct() {
        use serde::Serialize;

        #[derive(Serialize)]
        struct AddressSerde {
            city: String,
            zip: u32,
        }

        #[derive(Serialize)]
        struct PersonSerde {
            name: String,
            age: u32,
            address: AddressSerde,
        }

        let source = PersonSerde {
            name: "Alice".into(),
            age: 30,
            address: AddressSerde {
                city: "Portland".into(),
                zip: 97201,
            },
        };

        let encoded = ::postcard::to_allocvec(&source).unwrap();
        let deser = compile_deser(Person::SHAPE, &postcard::FadPostcard);
        let result: Person = deserialize(&deser, &encoded).unwrap();

        assert_eq!(
            result,
            Person {
                name: "Alice".into(),
                age: 30,
                address: Address {
                    city: "Portland".into(),
                    zip: 97201,
                },
            }
        );
    }

    // r[verify deser.nested-struct]
    // r[verify deser.nested-struct.offset]
    #[test]
    fn json_nested_struct() {
        let input = br#"{"name": "Alice", "age": 30, "address": {"city": "Portland", "zip": 97201}}"#;
        let deser = compile_deser(Person::SHAPE, &json::FadJson);
        let result: Person = deserialize(&deser, input).unwrap();

        assert_eq!(
            result,
            Person {
                name: "Alice".into(),
                age: 30,
                address: Address {
                    city: "Portland".into(),
                    zip: 97201,
                },
            }
        );
    }

    // r[verify deser.nested-struct]
    #[test]
    fn json_nested_struct_reversed_keys() {
        let input = br#"{"address": {"zip": 97201, "city": "Portland"}, "age": 30, "name": "Alice"}"#;
        let deser = compile_deser(Person::SHAPE, &json::FadJson);
        let result: Person = deserialize(&deser, input).unwrap();

        assert_eq!(
            result,
            Person {
                name: "Alice".into(),
                age: 30,
                address: Address {
                    city: "Portland".into(),
                    zip: 97201,
                },
            }
        );
    }

    #[derive(Facet, Debug, PartialEq)]
    struct Inner {
        x: u32,
    }

    #[derive(Facet, Debug, PartialEq)]
    struct Middle {
        inner: Inner,
        y: u32,
    }

    #[derive(Facet, Debug, PartialEq)]
    struct Outer {
        middle: Middle,
        z: u32,
    }

    // r[verify deser.nested-struct]
    #[test]
    fn postcard_deeply_nested() {
        use serde::Serialize;

        #[derive(Serialize)]
        struct InnerSerde {
            x: u32,
        }
        #[derive(Serialize)]
        struct MiddleSerde {
            inner: InnerSerde,
            y: u32,
        }
        #[derive(Serialize)]
        struct OuterSerde {
            middle: MiddleSerde,
            z: u32,
        }

        let source = OuterSerde {
            middle: MiddleSerde {
                inner: InnerSerde { x: 1 },
                y: 2,
            },
            z: 3,
        };

        let encoded = ::postcard::to_allocvec(&source).unwrap();
        let deser = compile_deser(Outer::SHAPE, &postcard::FadPostcard);
        let result: Outer = deserialize(&deser, &encoded).unwrap();

        assert_eq!(
            result,
            Outer {
                middle: Middle {
                    inner: Inner { x: 1 },
                    y: 2,
                },
                z: 3,
            }
        );
    }

    // r[verify deser.nested-struct]
    #[test]
    fn json_deeply_nested() {
        let input = br#"{"middle": {"inner": {"x": 1}, "y": 2}, "z": 3}"#;
        let deser = compile_deser(Outer::SHAPE, &json::FadJson);
        let result: Outer = deserialize(&deser, input).unwrap();

        assert_eq!(
            result,
            Outer {
                middle: Middle {
                    inner: Inner { x: 1 },
                    y: 2,
                },
                z: 3,
            }
        );
    }

    // r[verify deser.nested-struct]
    #[test]
    fn postcard_shared_inner_type() {
        use serde::Serialize;

        #[derive(Facet, Debug, PartialEq)]
        struct TwoAddresses {
            home: Address,
            work: Address,
        }

        #[derive(Serialize)]
        struct AddressSerde {
            city: String,
            zip: u32,
        }
        #[derive(Serialize)]
        struct TwoAddressesSerde {
            home: AddressSerde,
            work: AddressSerde,
        }

        let source = TwoAddressesSerde {
            home: AddressSerde {
                city: "Portland".into(),
                zip: 97201,
            },
            work: AddressSerde {
                city: "Seattle".into(),
                zip: 98101,
            },
        };

        let encoded = ::postcard::to_allocvec(&source).unwrap();
        let deser = compile_deser(TwoAddresses::SHAPE, &postcard::FadPostcard);
        let result: TwoAddresses = deserialize(&deser, &encoded).unwrap();

        assert_eq!(
            result,
            TwoAddresses {
                home: Address {
                    city: "Portland".into(),
                    zip: 97201,
                },
                work: Address {
                    city: "Seattle".into(),
                    zip: 98101,
                },
            }
        );
    }

    /// Disassemble a byte slice, marking one offset with a label.
    /// Stops after the second `ret` (to capture both success and error paths).
    fn disasm_bytes(code: &[u8], base_addr: u64, marker_offset: Option<usize>) -> String {
        let mut out = String::new();
        use std::fmt::Write;
        use yaxpeax_arch::{Decoder, U8Reader};

        #[cfg(target_arch = "aarch64")]
        {
            use yaxpeax_arm::armv8::a64::InstDecoder;

            let decoder = InstDecoder::default();
            let mut reader = U8Reader::new(code);
            let mut offset = 0usize;
            let mut ret_count = 0u32;
            while offset + 4 <= code.len() {
                let marker = match marker_offset {
                    Some(m) if m == offset => " <entry>",
                    _ => "",
                };
                match decoder.decode(&mut reader) {
                    Ok(inst) => {
                        let addr = base_addr + offset as u64;
                        writeln!(&mut out, "{addr:12x}:{marker}  {inst}").unwrap();
                        let text = format!("{inst}");
                        if text.trim() == "ret" {
                            ret_count += 1;
                            if ret_count >= 2 {
                                break;
                            }
                        }
                    }
                    Err(e) => {
                        let word =
                            u32::from_le_bytes(code[offset..offset + 4].try_into().unwrap());
                        let addr = base_addr + offset as u64;
                        writeln!(&mut out, "{addr:12x}:{marker}  <{e}> (0x{word:08x})").unwrap();
                    }
                }
                offset += 4;
            }
        }

        #[cfg(target_arch = "x86_64")]
        {
            use yaxpeax_x86::amd64::InstDecoder;

            let decoder = InstDecoder::default();
            let mut reader = U8Reader::new(code);
            let mut offset = 0usize;
            let mut ret_count = 0u32;
            while offset < code.len() {
                let marker = match marker_offset {
                    Some(m) if m == offset => " <entry>",
                    _ => "",
                };
                match decoder.decode(&mut reader) {
                    Ok(inst) => {
                        let len = inst.len().to_const() as usize;
                        let addr = base_addr + offset as u64;
                        writeln!(&mut out, "{addr:12x}:{marker}  {inst}").unwrap();
                        let text = format!("{inst}");
                        if text.trim() == "ret" {
                            ret_count += 1;
                            if ret_count >= 2 {
                                break;
                            }
                        }
                        offset += len;
                    }
                    Err(_) => {
                        let addr = base_addr + offset as u64;
                        writeln!(
                            &mut out,
                            "{addr:12x}:{marker}  <decode error> (0x{:02x})",
                            code[offset]
                        )
                        .unwrap();
                        offset += 1;
                    }
                }
            }
        }

        out
    }

    /// Disassemble a CompiledDeser's JIT code buffer.
    fn disasm_jit(deser: &CompiledDeser) -> String {
        let code = deser.code();
        let base = code.as_ptr() as u64;
        disasm_bytes(code, base, Some(deser.entry_offset()))
    }

    /// Disassemble a native function starting at `fn_ptr` for up to `max_bytes`.
    /// Stops at the second `ret` instruction or when `max_bytes` is exhausted.
    ///
    /// # Safety
    /// `fn_ptr` must point to valid executable code. `max_bytes` must not extend
    /// past the end of the mapped region.
    unsafe fn disasm_native(fn_ptr: *const u8, max_bytes: usize) -> String {
        let code = unsafe { std::slice::from_raw_parts(fn_ptr, max_bytes) };
        disasm_bytes(code, fn_ptr as u64, Some(0))
    }

    #[test]
    fn disasm_postcard_deep_nested() {
        let deser = compile_deser(Outer::SHAPE, &postcard::FadPostcard);
        eprintln!("=== fad postcard deep_nested (Outer) ===\n{}", disasm_jit(&deser));
    }

    #[test]
    fn disasm_postcard_flat() {
        let deser = compile_deser(Friend::SHAPE, &postcard::FadPostcard);
        eprintln!("=== fad postcard flat (Friend) ===\n{}", disasm_jit(&deser));
    }

    #[test]
    fn disasm_json_nested() {
        let deser = compile_deser(Person::SHAPE, &json::FadJson);
        eprintln!("=== fad json nested (Person) ===\n{}", disasm_jit(&deser));
    }

    #[test]
    fn disasm_serde_postcard_deep_nested() {
        // Force monomorphization of the serde path so we can disassemble it
        fn serde_deser(data: &[u8]) -> OuterSerde {
            ::postcard::from_bytes(data).unwrap()
        }

        #[derive(serde::Deserialize, Debug)]
        #[allow(dead_code)]
        struct InnerSerde { x: u32 }
        #[derive(serde::Deserialize, Debug)]
        #[allow(dead_code)]
        struct MiddleSerde { inner: InnerSerde, y: u32 }
        #[derive(serde::Deserialize, Debug)]
        #[allow(dead_code)]
        struct OuterSerde { middle: MiddleSerde, z: u32 }

        let fn_ptr = serde_deser as *const u8;
        let asm = unsafe { disasm_native(fn_ptr, 2048) };
        eprintln!("=== serde postcard deep_nested (OuterSerde) @ {fn_ptr:?} ===\n{asm}");
    }

    // --- Milestone 6: Enums ---

    #[derive(Facet, Debug, PartialEq)]
    #[repr(u8)]
    enum Animal {
        Cat,
        Dog { name: String, good_boy: bool },
        Parrot(String),
    }

    // r[verify deser.postcard.enum]
    // r[verify deser.postcard.enum.unit]
    #[test]
    fn postcard_enum_unit_variant() {
        use serde::Serialize;

        #[derive(Serialize)]
        #[repr(u8)]
        enum AnimalSerde {
            Cat,
            #[allow(dead_code)]
            Dog { name: String, good_boy: bool },
            #[allow(dead_code)]
            Parrot(String),
        }

        let encoded = ::postcard::to_allocvec(&AnimalSerde::Cat).unwrap();
        let deser = compile_deser(Animal::SHAPE, &postcard::FadPostcard);
        let result: Animal = deserialize(&deser, &encoded).unwrap();
        assert_eq!(result, Animal::Cat);
    }

    // r[verify deser.postcard.enum]
    // r[verify deser.postcard.enum.dispatch]
    #[test]
    fn postcard_enum_struct_variant() {
        use serde::Serialize;

        #[derive(Serialize)]
        #[repr(u8)]
        enum AnimalSerde {
            #[allow(dead_code)]
            Cat,
            Dog { name: String, good_boy: bool },
            #[allow(dead_code)]
            Parrot(String),
        }

        let encoded = ::postcard::to_allocvec(&AnimalSerde::Dog {
            name: "Rex".into(),
            good_boy: true,
        }).unwrap();
        let deser = compile_deser(Animal::SHAPE, &postcard::FadPostcard);
        let result: Animal = deserialize(&deser, &encoded).unwrap();
        assert_eq!(result, Animal::Dog { name: "Rex".into(), good_boy: true });
    }

    // r[verify deser.postcard.enum]
    #[test]
    fn postcard_enum_tuple_variant() {
        use serde::Serialize;

        #[derive(Serialize)]
        #[repr(u8)]
        enum AnimalSerde {
            #[allow(dead_code)]
            Cat,
            #[allow(dead_code)]
            Dog { name: String, good_boy: bool },
            Parrot(String),
        }

        let encoded = ::postcard::to_allocvec(&AnimalSerde::Parrot("Polly".into())).unwrap();
        let deser = compile_deser(Animal::SHAPE, &postcard::FadPostcard);
        let result: Animal = deserialize(&deser, &encoded).unwrap();
        assert_eq!(result, Animal::Parrot("Polly".into()));
    }

    // r[verify deser.postcard.enum.dispatch]
    #[test]
    fn postcard_enum_unknown_discriminant() {
        // Discriminant 99 doesn't exist
        let input = [99u8];
        let deser = compile_deser(Animal::SHAPE, &postcard::FadPostcard);
        let result = deserialize::<Animal>(&deser, &input);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().code, ErrorCode::UnknownVariant);
    }

    // r[verify deser.postcard.enum]
    #[test]
    fn postcard_enum_as_struct_field() {
        use serde::Serialize;

        #[derive(Facet, Debug, PartialEq)]
        struct Zoo {
            name: String,
            star: Animal,
        }

        #[derive(Serialize)]
        #[repr(u8)]
        enum AnimalSerde {
            #[allow(dead_code)]
            Cat,
            Dog { name: String, good_boy: bool },
            #[allow(dead_code)]
            Parrot(String),
        }

        #[derive(Serialize)]
        struct ZooSerde {
            name: String,
            star: AnimalSerde,
        }

        let encoded = ::postcard::to_allocvec(&ZooSerde {
            name: "City Zoo".into(),
            star: AnimalSerde::Dog { name: "Rex".into(), good_boy: true },
        }).unwrap();
        let deser = compile_deser(Zoo::SHAPE, &postcard::FadPostcard);
        let result: Zoo = deserialize(&deser, &encoded).unwrap();
        assert_eq!(result, Zoo {
            name: "City Zoo".into(),
            star: Animal::Dog { name: "Rex".into(), good_boy: true },
        });
    }

    // r[verify deser.json.enum.external]
    // r[verify deser.json.enum.external.unit-as-string]
    #[test]
    fn json_enum_unit_as_string() {
        let input = br#""Cat""#;
        let deser = compile_deser(Animal::SHAPE, &json::FadJson);
        let result: Animal = deserialize(&deser, input).unwrap();
        assert_eq!(result, Animal::Cat);
    }

    // r[verify deser.json.enum.external]
    // r[verify deser.json.enum.external.struct-variant]
    #[test]
    fn json_enum_struct_variant() {
        let input = br#"{"Dog": {"name": "Rex", "good_boy": true}}"#;
        let deser = compile_deser(Animal::SHAPE, &json::FadJson);
        let result: Animal = deserialize(&deser, input).unwrap();
        assert_eq!(result, Animal::Dog { name: "Rex".into(), good_boy: true });
    }

    // r[verify deser.json.enum.external]
    // r[verify deser.json.enum.external.tuple-variant]
    #[test]
    fn json_enum_tuple_variant() {
        let input = br#"{"Parrot": "Polly"}"#;
        let deser = compile_deser(Animal::SHAPE, &json::FadJson);
        let result: Animal = deserialize(&deser, input).unwrap();
        assert_eq!(result, Animal::Parrot("Polly".into()));
    }

    // r[verify deser.json.enum.external]
    #[test]
    fn json_enum_unit_in_object() {
        // Unit variant inside object form: { "Cat": null }
        let input = br#"{"Cat": null}"#;
        let deser = compile_deser(Animal::SHAPE, &json::FadJson);
        let result: Animal = deserialize(&deser, input).unwrap();
        assert_eq!(result, Animal::Cat);
    }

    // r[verify deser.json.enum.external]
    #[test]
    fn json_enum_unknown_variant() {
        let input = br#""Snake""#;
        let deser = compile_deser(Animal::SHAPE, &json::FadJson);
        let result = deserialize::<Animal>(&deser, input);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().code, ErrorCode::UnknownVariant);
    }

    // r[verify deser.json.enum.external]
    #[test]
    fn json_enum_as_struct_field() {
        #[derive(Facet, Debug, PartialEq)]
        struct Zoo {
            name: String,
            star: Animal,
        }

        let input = br#"{"name": "City Zoo", "star": {"Dog": {"name": "Rex", "good_boy": true}}}"#;
        let deser = compile_deser(Zoo::SHAPE, &json::FadJson);
        let result: Zoo = deserialize(&deser, input).unwrap();
        assert_eq!(result, Zoo {
            name: "City Zoo".into(),
            star: Animal::Dog { name: "Rex".into(), good_boy: true },
        });
    }

    // r[verify deser.json.enum.external.struct-variant]
    #[test]
    fn json_enum_struct_variant_reversed_keys() {
        let input = br#"{"Dog": {"good_boy": true, "name": "Rex"}}"#;
        let deser = compile_deser(Animal::SHAPE, &json::FadJson);
        let result: Animal = deserialize(&deser, input).unwrap();
        assert_eq!(result, Animal::Dog { name: "Rex".into(), good_boy: true });
    }

    #[test]
    fn disasm_postcard_enum() {
        let deser = compile_deser(Animal::SHAPE, &postcard::FadPostcard);
        eprintln!("=== fad postcard enum (Animal) ===\n{}", disasm_jit(&deser));
    }

    #[test]
    fn disasm_json_enum() {
        let deser = compile_deser(Animal::SHAPE, &json::FadJson);
        eprintln!("=== fad json enum (Animal) ===\n{}", disasm_jit(&deser));
    }

    #[test]
    fn disasm_serde_postcard_enum() {
        fn serde_deser(data: &[u8]) -> AnimalSerde {
            ::postcard::from_bytes(data).unwrap()
        }

        #[derive(serde::Deserialize, Debug)]
        #[allow(dead_code)]
        #[repr(u8)]
        enum AnimalSerde {
            Cat,
            Dog { name: String, good_boy: bool },
            Parrot(String),
        }

        let fn_ptr = serde_deser as *const u8;
        let asm = unsafe { disasm_native(fn_ptr, 2048) };
        eprintln!("=== serde postcard enum (AnimalSerde) @ {fn_ptr:?} ===\n{asm}");
    }

    // --- Milestone 7: Flatten ---

    #[derive(Facet, Debug, PartialEq)]
    struct Metadata {
        version: u32,
        author: String,
    }

    #[derive(Facet, Debug, PartialEq)]
    struct Document {
        title: String,
        #[facet(flatten)]
        meta: Metadata,
    }

    // r[verify deser.flatten]
    // r[verify deser.flatten.offset-accumulation]
    // r[verify deser.flatten.inline]
    #[test]
    fn json_flatten_basic() {
        let input = br#"{"title": "Hello", "version": 1, "author": "Amos"}"#;
        let deser = compile_deser(Document::SHAPE, &json::FadJson);
        let result: Document = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            Document {
                title: "Hello".into(),
                meta: Metadata {
                    version: 1,
                    author: "Amos".into(),
                },
            }
        );
    }

    // r[verify deser.flatten]
    #[test]
    fn json_flatten_reversed_keys() {
        let input = br#"{"author": "Amos", "version": 1, "title": "Hello"}"#;
        let deser = compile_deser(Document::SHAPE, &json::FadJson);
        let result: Document = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            Document {
                title: "Hello".into(),
                meta: Metadata {
                    version: 1,
                    author: "Amos".into(),
                },
            }
        );
    }

    // r[verify deser.flatten]
    #[test]
    fn postcard_flatten_basic() {
        // Postcard flatten: fields appear in declaration order at parent level.
        // Document = { title: String, meta: Metadata { version: u32, author: String } }
        // With flatten, wire order is: title, version, author
        //
        // title="Hi" → varint(2)=0x02 + b"Hi"
        // version=1 → varint 0x01
        // author="A" → varint(1)=0x01 + b"A"
        let input = [0x02, b'H', b'i', 0x01, 0x01, b'A'];
        let deser = compile_deser(Document::SHAPE, &postcard::FadPostcard);
        let result: Document = deserialize(&deser, &input).unwrap();
        assert_eq!(
            result,
            Document {
                title: "Hi".into(),
                meta: Metadata {
                    version: 1,
                    author: "A".into(),
                },
            }
        );
    }

    // r[verify deser.flatten.conflict]
    #[test]
    #[should_panic(expected = "field name collision")]
    fn flatten_name_collision() {
        #[derive(Facet)]
        struct Collider {
            x: u32,
        }

        #[derive(Facet)]
        struct HasCollision {
            x: u32,
            #[facet(flatten)]
            inner: Collider,
        }

        compile_deser(HasCollision::SHAPE, &json::FadJson);
    }

    // --- Milestone 7: Adjacently tagged enums ---

    #[derive(Facet, Debug, PartialEq)]
    #[facet(tag = "type", content = "data")]
    #[repr(u8)]
    enum AdjAnimal {
        Cat,
        Dog { name: String, good_boy: bool },
        Parrot(String),
    }

    // r[verify deser.json.enum.adjacent]
    // r[verify deser.json.enum.adjacent.unit-variant]
    #[test]
    fn json_adjacent_unit_no_content() {
        let input = br#"{"type": "Cat"}"#;
        let deser = compile_deser(AdjAnimal::SHAPE, &json::FadJson);
        let result: AdjAnimal = deserialize(&deser, input).unwrap();
        assert_eq!(result, AdjAnimal::Cat);
    }

    // r[verify deser.json.enum.adjacent]
    // r[verify deser.json.enum.adjacent.unit-variant]
    #[test]
    fn json_adjacent_unit_with_null_content() {
        let input = br#"{"type": "Cat", "data": null}"#;
        let deser = compile_deser(AdjAnimal::SHAPE, &json::FadJson);
        let result: AdjAnimal = deserialize(&deser, input).unwrap();
        assert_eq!(result, AdjAnimal::Cat);
    }

    // r[verify deser.json.enum.adjacent]
    #[test]
    fn json_adjacent_struct_variant() {
        let input = br#"{"type": "Dog", "data": {"name": "Rex", "good_boy": true}}"#;
        let deser = compile_deser(AdjAnimal::SHAPE, &json::FadJson);
        let result: AdjAnimal = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            AdjAnimal::Dog {
                name: "Rex".into(),
                good_boy: true,
            }
        );
    }

    // r[verify deser.json.enum.adjacent]
    #[test]
    fn json_adjacent_struct_variant_reversed_fields() {
        let input = br#"{"type": "Dog", "data": {"good_boy": true, "name": "Rex"}}"#;
        let deser = compile_deser(AdjAnimal::SHAPE, &json::FadJson);
        let result: AdjAnimal = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            AdjAnimal::Dog {
                name: "Rex".into(),
                good_boy: true,
            }
        );
    }

    // r[verify deser.json.enum.adjacent]
    // r[verify deser.json.enum.adjacent.tuple-variant]
    #[test]
    fn json_adjacent_tuple_variant() {
        let input = br#"{"type": "Parrot", "data": "Polly"}"#;
        let deser = compile_deser(AdjAnimal::SHAPE, &json::FadJson);
        let result: AdjAnimal = deserialize(&deser, input).unwrap();
        assert_eq!(result, AdjAnimal::Parrot("Polly".into()));
    }

    // r[verify deser.json.enum.adjacent]
    #[test]
    fn json_adjacent_unknown_variant() {
        let input = br#"{"type": "Snake", "data": null}"#;
        let deser = compile_deser(AdjAnimal::SHAPE, &json::FadJson);
        let result = deserialize::<AdjAnimal>(&deser, input);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().code, ErrorCode::UnknownVariant);
    }

    // r[verify deser.json.enum.adjacent.key-order]
    #[test]
    fn json_adjacent_wrong_first_key() {
        let input = br#"{"data": null, "type": "Cat"}"#;
        let deser = compile_deser(AdjAnimal::SHAPE, &json::FadJson);
        let result = deserialize::<AdjAnimal>(&deser, input);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().code, ErrorCode::ExpectedTagKey);
    }

    // --- Milestone 7: Internally tagged enums ---

    #[derive(Facet, Debug, PartialEq)]
    #[facet(tag = "type")]
    #[repr(u8)]
    enum IntAnimal {
        Cat,
        Dog { name: String, good_boy: bool },
    }

    // r[verify deser.json.enum.internal]
    // r[verify deser.json.enum.internal.unit-variant]
    #[test]
    fn json_internal_unit_variant() {
        let input = br#"{"type": "Cat"}"#;
        let deser = compile_deser(IntAnimal::SHAPE, &json::FadJson);
        let result: IntAnimal = deserialize(&deser, input).unwrap();
        assert_eq!(result, IntAnimal::Cat);
    }

    // r[verify deser.json.enum.internal]
    #[test]
    fn json_internal_struct_variant() {
        let input = br#"{"type": "Dog", "name": "Rex", "good_boy": true}"#;
        let deser = compile_deser(IntAnimal::SHAPE, &json::FadJson);
        let result: IntAnimal = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            IntAnimal::Dog {
                name: "Rex".into(),
                good_boy: true,
            }
        );
    }

    // r[verify deser.json.enum.internal]
    #[test]
    fn json_internal_struct_variant_reversed_fields() {
        let input = br#"{"type": "Dog", "good_boy": true, "name": "Rex"}"#;
        let deser = compile_deser(IntAnimal::SHAPE, &json::FadJson);
        let result: IntAnimal = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            IntAnimal::Dog {
                name: "Rex".into(),
                good_boy: true,
            }
        );
    }

    // r[verify deser.json.enum.internal]
    #[test]
    fn json_internal_unknown_variant() {
        let input = br#"{"type": "Snake"}"#;
        let deser = compile_deser(IntAnimal::SHAPE, &json::FadJson);
        let result = deserialize::<IntAnimal>(&deser, input);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().code, ErrorCode::UnknownVariant);
    }

    // r[verify deser.json.enum.internal]
    #[test]
    fn json_internal_wrong_first_key() {
        let input = br#"{"name": "Rex", "type": "Dog", "good_boy": true}"#;
        let deser = compile_deser(IntAnimal::SHAPE, &json::FadJson);
        let result = deserialize::<IntAnimal>(&deser, input);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().code, ErrorCode::ExpectedTagKey);
    }

    // r[verify deser.json.enum.internal.struct-only]
    #[test]
    #[should_panic(expected = "tuple variants")]
    fn json_internal_tuple_variant_panics() {
        #[derive(Facet)]
        #[facet(tag = "type")]
        #[repr(u8)]
        enum BadInternal {
            #[allow(dead_code)]
            Wrapper(String),
        }

        compile_deser(BadInternal::SHAPE, &json::FadJson);
    }

    // --- Milestone 8: Untagged enums ---

    #[derive(Facet, Debug, PartialEq)]
    #[facet(untagged)]
    #[repr(u8)]
    enum Untagged {
        Cat,
        Dog { name: String, good_boy: bool },
        Parrot(String),
    }

    // r[verify deser.json.enum.untagged]
    // r[verify deser.json.enum.untagged.string-trie]
    #[test]
    fn json_untagged_unit() {
        let input = br#""Cat""#;
        let deser = compile_deser(Untagged::SHAPE, &json::FadJson);
        let result: Untagged = deserialize(&deser, input).unwrap();
        assert_eq!(result, Untagged::Cat);
    }

    // r[verify deser.json.enum.untagged]
    // r[verify deser.json.enum.untagged.bucket]
    // r[verify deser.json.enum.untagged.peek]
    #[test]
    fn json_untagged_struct() {
        let input = br#"{"name": "Rex", "good_boy": true}"#;
        let deser = compile_deser(Untagged::SHAPE, &json::FadJson);
        let result: Untagged = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            Untagged::Dog {
                name: "Rex".into(),
                good_boy: true,
            }
        );
    }

    // r[verify deser.json.enum.untagged]
    // r[verify deser.json.enum.untagged.string-trie]
    #[test]
    fn json_untagged_newtype_string() {
        let input = br#""Polly""#;
        let deser = compile_deser(Untagged::SHAPE, &json::FadJson);
        let result: Untagged = deserialize(&deser, input).unwrap();
        assert_eq!(result, Untagged::Parrot("Polly".into()));
    }

    // r[verify deser.json.enum.untagged]
    #[test]
    fn json_untagged_struct_reversed_keys() {
        let input = br#"{"good_boy": true, "name": "Rex"}"#;
        let deser = compile_deser(Untagged::SHAPE, &json::FadJson);
        let result: Untagged = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            Untagged::Dog {
                name: "Rex".into(),
                good_boy: true,
            }
        );
    }

    // Multi-struct solver test
    #[derive(Facet, Debug, PartialEq)]
    #[facet(untagged)]
    #[repr(u8)]
    enum Config {
        Database { host: String, port: u32 },
        Redis { host: String, db: u32 },
    }

    // r[verify deser.json.enum.untagged.object-solver]
    #[test]
    fn json_untagged_solver_database() {
        let input = br#"{"host": "localhost", "port": 5432}"#;
        let deser = compile_deser(Config::SHAPE, &json::FadJson);
        let result: Config = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            Config::Database {
                host: "localhost".into(),
                port: 5432,
            }
        );
    }

    // r[verify deser.json.enum.untagged.object-solver]
    #[test]
    fn json_untagged_solver_redis() {
        let input = br#"{"host": "localhost", "db": 0}"#;
        let deser = compile_deser(Config::SHAPE, &json::FadJson);
        let result: Config = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            Config::Redis {
                host: "localhost".into(),
                db: 0,
            }
        );
    }

    // r[verify deser.json.enum.untagged.object-solver]
    #[test]
    fn json_untagged_solver_key_order_independent() {
        // Key order doesn't matter — "db" narrows to Redis regardless of position
        let input = br#"{"db": 0, "host": "localhost"}"#;
        let deser = compile_deser(Config::SHAPE, &json::FadJson);
        let result: Config = deserialize(&deser, input).unwrap();
        assert_eq!(
            result,
            Config::Redis {
                host: "localhost".into(),
                db: 0,
            }
        );
    }

    // r[verify deser.json.enum.untagged.scalar-unique]
    #[test]
    fn json_untagged_newtype_number() {
        #[derive(Facet, Debug, PartialEq)]
        #[facet(untagged)]
        #[repr(u8)]
        enum StringOrNum {
            Text(String),
            Num(u32),
        }

        let input = br#"42"#;
        let deser = compile_deser(StringOrNum::SHAPE, &json::FadJson);
        let result: StringOrNum = deserialize(&deser, input).unwrap();
        assert_eq!(result, StringOrNum::Num(42));

        let input = br#""hello""#;
        let deser = compile_deser(StringOrNum::SHAPE, &json::FadJson);
        let result: StringOrNum = deserialize(&deser, input).unwrap();
        assert_eq!(result, StringOrNum::Text("hello".into()));
    }

    // r[verify deser.json.enum.untagged.scalar-unique]
    #[test]
    fn json_untagged_newtype_bool() {
        #[derive(Facet, Debug, PartialEq)]
        #[facet(untagged)]
        #[repr(u8)]
        enum StringOrBool {
            Text(String),
            Flag(bool),
        }

        let input = br#"true"#;
        let deser = compile_deser(StringOrBool::SHAPE, &json::FadJson);
        let result: StringOrBool = deserialize(&deser, input).unwrap();
        assert_eq!(result, StringOrBool::Flag(true));

        let input = br#""hello""#;
        let deser = compile_deser(StringOrBool::SHAPE, &json::FadJson);
        let result: StringOrBool = deserialize(&deser, input).unwrap();
        assert_eq!(result, StringOrBool::Text("hello".into()));
    }

    // r[verify deser.json.enum.untagged.scalar-unique]
    #[test]
    #[should_panic(expected = "number variants")]
    fn json_untagged_ambiguous_number_panics() {
        #[derive(Facet)]
        #[facet(untagged)]
        #[repr(u8)]
        enum BadNum {
            #[allow(dead_code)]
            A(u32),
            #[allow(dead_code)]
            B(i64),
        }

        compile_deser(BadNum::SHAPE, &json::FadJson);
    }

    // r[verify compiler.recursive.one-func-per-shape]
    #[test]
    fn json_shared_inner_type() {
        #[derive(Facet, Debug, PartialEq)]
        struct TwoAddresses {
            home: Address,
            work: Address,
        }

        let input = br#"{"home": {"city": "Portland", "zip": 97201}, "work": {"city": "Seattle", "zip": 98101}}"#;
        let deser = compile_deser(TwoAddresses::SHAPE, &json::FadJson);
        let result: TwoAddresses = deserialize(&deser, input).unwrap();

        assert_eq!(
            result,
            TwoAddresses {
                home: Address {
                    city: "Portland".into(),
                    zip: 97201,
                },
                work: Address {
                    city: "Seattle".into(),
                    zip: 98101,
                },
            }
        );
    }
}
