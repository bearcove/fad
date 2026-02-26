pub mod arch;
pub mod compiler;
pub mod context;
pub mod format;
pub mod intrinsics;
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
}
