use core::fmt;

// r[impl callconv.deser-context]
/// The runtime context passed to every emitted deserializer function.
/// Layout is `#[repr(C)]` so field offsets are stable and can be used from JIT code.
#[repr(C)]
pub struct DeserContext {
    /// Current read position in the input buffer.
    pub input_ptr: *const u8,
    /// One-past-the-end of the input buffer.
    pub input_end: *const u8,
    /// Error slot — checked after each intrinsic call.
    pub error: ErrorSlot,
}

// r[impl error.slot]
/// Error information written by intrinsics when something goes wrong.
#[repr(C)]
pub struct ErrorSlot {
    /// Non-zero means an error occurred.
    pub code: u32,
    /// Byte offset in the input where the error was detected.
    pub offset: u32,
}

// r[impl error.slot]
// r[impl error.propagation]
/// Error codes written into `ErrorSlot.code`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode {
    Ok = 0,
    UnexpectedEof = 1,
    InvalidVarint = 2,
    InvalidUtf8 = 3,
    UnsupportedShape = 4,
    ExpectedObjectStart = 5,
    ExpectedColon = 6,
    ExpectedStringKey = 7,
    UnterminatedString = 8,
    InvalidJsonNumber = 9,
    MissingRequiredField = 10,
    UnexpectedCharacter = 11,
    NumberOutOfRange = 12,
    InvalidBool = 13,
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorCode::Ok => write!(f, "no error"),
            ErrorCode::UnexpectedEof => write!(f, "unexpected end of input"),
            ErrorCode::InvalidVarint => write!(f, "invalid varint encoding"),
            ErrorCode::InvalidUtf8 => write!(f, "invalid UTF-8"),
            ErrorCode::UnsupportedShape => write!(f, "unsupported shape"),
            ErrorCode::ExpectedObjectStart => write!(f, "expected '{{' to start object"),
            ErrorCode::ExpectedColon => write!(f, "expected ':' after key"),
            ErrorCode::ExpectedStringKey => write!(f, "expected '\"' to start key"),
            ErrorCode::UnterminatedString => write!(f, "unterminated string"),
            ErrorCode::InvalidJsonNumber => write!(f, "invalid JSON number"),
            ErrorCode::MissingRequiredField => write!(f, "missing required field"),
            ErrorCode::UnexpectedCharacter => write!(f, "unexpected character"),
            ErrorCode::NumberOutOfRange => write!(f, "number out of range for target type"),
            ErrorCode::InvalidBool => write!(f, "invalid bool value"),
        }
    }
}

// Field offset constants for use from JIT code.
pub const CTX_INPUT_PTR: u32 = core::mem::offset_of!(DeserContext, input_ptr) as u32;
pub const CTX_INPUT_END: u32 = core::mem::offset_of!(DeserContext, input_end) as u32;
pub const CTX_ERROR_CODE: u32 = core::mem::offset_of!(DeserContext, error.code) as u32;
pub const CTX_ERROR_OFFSET: u32 = core::mem::offset_of!(DeserContext, error.offset) as u32;

impl DeserContext {
    /// Create a new context pointing at the given input slice.
    pub fn new(input: &[u8]) -> Self {
        let ptr = input.as_ptr();
        DeserContext {
            input_ptr: ptr,
            input_end: unsafe { ptr.add(input.len()) },
            error: ErrorSlot {
                code: 0,
                offset: 0,
            },
        }
    }

    /// Returns the number of bytes remaining.
    pub fn remaining(&self) -> usize {
        unsafe { self.input_end.offset_from(self.input_ptr) as usize }
    }

    /// Set an error code, recording the current offset from the start of the original input.
    pub fn set_error(&mut self, code: ErrorCode, input_start: *const u8) {
        self.error.code = code as u32;
        self.error.offset = unsafe { self.input_ptr.offset_from(input_start) as u32 };
    }
}
