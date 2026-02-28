use std::borrow::Cow;

use crate::context::{DeserContext, ErrorCode};
use core::num::IntErrorKind;

// r[impl deser.json.struct]

/// Skip JSON whitespace (space, tab, newline, carriage return).
/// Advances ctx.input_ptr past any whitespace.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_skip_ws(ctx: *mut DeserContext) {
    let ctx = unsafe { &mut *ctx };
    while ctx.input_ptr < ctx.input_end {
        let b = unsafe { *ctx.input_ptr };
        match b {
            b' ' | b'\t' | b'\n' | b'\r' => {
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
            }
            _ => break,
        }
    }
}

/// Skip whitespace, then expect and consume '{'.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_expect_object_start(ctx: *mut DeserContext) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::ExpectedObjectStart as u32;
        return;
    }
    let b = unsafe { *ctx.input_ptr };
    if b != b'{' {
        ctx.error.code = ErrorCode::ExpectedObjectStart as u32;
        return;
    }
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
}

/// Skip whitespace, then expect and consume ':'.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_expect_colon(ctx: *mut DeserContext) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::ExpectedColon as u32;
        return;
    }
    let b = unsafe { *ctx.input_ptr };
    if b != b':' {
        ctx.error.code = ErrorCode::ExpectedColon as u32;
        return;
    }
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
}

/// Skip whitespace, then write the next byte to *out without consuming it.
/// If at EOF, sets error.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_peek_after_ws(ctx: *mut DeserContext, out: *mut u8) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    unsafe {
        *out = *ctx.input_ptr;
    }
}

// r[impl deser.json.string.escape]

/// Read 4 hex digits from input, return as u16.
/// Sets error code on failure (invalid hex digit or EOF).
unsafe fn json_read_hex4(ctx: &mut DeserContext) -> u16 {
    if ctx.remaining() < 4 {
        ctx.error.code = ErrorCode::InvalidEscapeSequence as u32;
        return 0;
    }
    let mut val: u16 = 0;
    for _ in 0..4 {
        let b = unsafe { *ctx.input_ptr };
        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
        let digit = match b {
            b'0'..=b'9' => b - b'0',
            b'a'..=b'f' => b - b'a' + 10,
            b'A'..=b'F' => b - b'A' + 10,
            _ => {
                ctx.error.code = ErrorCode::InvalidEscapeSequence as u32;
                return 0;
            }
        };
        val = (val << 4) | digit as u16;
    }
    val
}

/// Process one JSON escape sequence after the `\` has been consumed.
/// Appends the unescaped byte(s) to `buf`.
/// Returns false and sets error code on invalid escape.
unsafe fn json_unescape_one(ctx: &mut DeserContext, buf: &mut Vec<u8>) -> bool {
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return false;
    }
    let esc = unsafe { *ctx.input_ptr };
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
    match esc {
        b'"' => buf.push(b'"'),
        b'\\' => buf.push(b'\\'),
        b'/' => buf.push(b'/'),
        b'b' => buf.push(0x08),
        b'f' => buf.push(0x0C),
        b'n' => buf.push(b'\n'),
        b'r' => buf.push(b'\r'),
        b't' => buf.push(b'\t'),
        b'u' => {
            let cp = unsafe { json_read_hex4(ctx) };
            if ctx.error.code != 0 {
                return false;
            }
            if (0xD800..=0xDBFF).contains(&cp) {
                // High surrogate — expect \uXXXX low surrogate
                if ctx.remaining() < 2 {
                    ctx.error.code = ErrorCode::InvalidEscapeSequence as u32;
                    return false;
                }
                let b0 = unsafe { *ctx.input_ptr };
                let b1 = unsafe { *ctx.input_ptr.add(1) };
                if b0 != b'\\' || b1 != b'u' {
                    ctx.error.code = ErrorCode::InvalidEscapeSequence as u32;
                    return false;
                }
                ctx.input_ptr = unsafe { ctx.input_ptr.add(2) }; // skip \u
                let low = unsafe { json_read_hex4(ctx) };
                if ctx.error.code != 0 {
                    return false;
                }
                if !(0xDC00..=0xDFFF).contains(&low) {
                    ctx.error.code = ErrorCode::InvalidEscapeSequence as u32;
                    return false;
                }
                let codepoint = 0x10000 + ((cp as u32 - 0xD800) << 10) + (low as u32 - 0xDC00);
                // codepoint is always valid here (0x10000..=0x10FFFF)
                let ch = char::from_u32(codepoint).unwrap();
                let mut utf8_buf = [0u8; 4];
                let encoded = ch.encode_utf8(&mut utf8_buf);
                buf.extend_from_slice(encoded.as_bytes());
            } else if (0xDC00..=0xDFFF).contains(&cp) {
                // Lone low surrogate
                ctx.error.code = ErrorCode::InvalidEscapeSequence as u32;
                return false;
            } else {
                // BMP character
                let ch = char::from_u32(cp as u32).unwrap();
                let mut utf8_buf = [0u8; 4];
                let encoded = ch.encode_utf8(&mut utf8_buf);
                buf.extend_from_slice(encoded.as_bytes());
            }
        }
        _ => {
            ctx.error.code = ErrorCode::InvalidEscapeSequence as u32;
            return false;
        }
    }
    true
}

/// Read a JSON string key (must be "..."), writing the pointer and length.
/// Fast path: no escapes — returns borrowed pointer into input (zero-copy).
/// Slow path: escapes found — unescapes into ctx's scratch buffer.
/// Advances past the closing '"'.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_key(
    ctx: *mut DeserContext,
    out_ptr: *mut *const u8,
    out_len: *mut usize,
) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::ExpectedStringKey as u32;
        return;
    }
    if unsafe { *ctx.input_ptr } != b'"' {
        ctx.error.code = ErrorCode::ExpectedStringKey as u32;
        return;
    }
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip opening '"'

    let start = ctx.input_ptr;
    while ctx.input_ptr < ctx.input_end {
        let b = unsafe { *ctx.input_ptr };
        if b == b'"' {
            // Fast path: no escapes — borrow from input
            let len = unsafe { ctx.input_ptr.offset_from(start) as usize };
            unsafe {
                *out_ptr = start;
                *out_len = len;
            }
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip closing '"'
            return;
        }
        if b == b'\\' {
            // Slow path: unescape into scratch buffer
            let prefix_len = unsafe { ctx.input_ptr.offset_from(start) as usize };
            unsafe {
                json_read_key_slow(ctx, start, prefix_len, out_ptr, out_len);
            }
            return;
        }
        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
    }
    ctx.error.code = ErrorCode::UnterminatedString as u32;
}

/// Slow path for key reading: unescapes into a Vec, transfers to ctx scratch buffer.
unsafe fn json_read_key_slow(
    ctx: &mut DeserContext,
    prefix_start: *const u8,
    prefix_len: usize,
    out_ptr: *mut *const u8,
    out_len: *mut usize,
) {
    let mut buf = Vec::with_capacity(prefix_len + 32);
    buf.extend_from_slice(unsafe { core::slice::from_raw_parts(prefix_start, prefix_len) });

    while ctx.input_ptr < ctx.input_end {
        let b = unsafe { *ctx.input_ptr };
        if b == b'"' {
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip closing '"'
            let (ptr, len) = unsafe { ctx.replace_key_scratch(buf) };
            unsafe {
                *out_ptr = ptr;
                *out_len = len;
            }
            return;
        }
        if b == b'\\' {
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip '\'
            if !unsafe { json_unescape_one(ctx, &mut buf) } {
                return; // error set
            }
        } else {
            buf.push(b);
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
        }
    }
    ctx.error.code = ErrorCode::UnterminatedString as u32;
}

/// Pure key comparison: returns 1 if equal, 0 otherwise.
/// Does NOT touch DeserContext — this is a pure function.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_key_equals(
    key_ptr: *const u8,
    key_len: usize,
    expected_ptr: *const u8,
    expected_len: usize,
) -> u64 {
    if key_len != expected_len {
        return 0;
    }
    let key = unsafe { core::slice::from_raw_parts(key_ptr, key_len) };
    let expected = unsafe { core::slice::from_raw_parts(expected_ptr, expected_len) };
    if key == expected { 1 } else { 0 }
}

/// Recursively skip one JSON value (string, number, boolean, null, object, array).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_skip_value(ctx: *mut DeserContext) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let b = unsafe { *ctx.input_ptr };
    match b {
        b'"' => {
            // Skip string
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
            while ctx.input_ptr < ctx.input_end {
                let c = unsafe { *ctx.input_ptr };
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
                if c == b'"' {
                    return;
                }
                if c == b'\\' {
                    // Skip escaped char
                    if ctx.input_ptr < ctx.input_end {
                        let esc = unsafe { *ctx.input_ptr };
                        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
                        if esc == b'u' {
                            // Skip 4 hex digits
                            if ctx.remaining() >= 4 {
                                ctx.input_ptr = unsafe { ctx.input_ptr.add(4) };
                            } else {
                                ctx.error.code = ErrorCode::UnexpectedEof as u32;
                                return;
                            }
                        }
                    }
                }
            }
            ctx.error.code = ErrorCode::UnterminatedString as u32;
        }
        b'{' => {
            // Skip object
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
            unsafe { skip_ws_raw(ctx) };
            if ctx.input_ptr < ctx.input_end && unsafe { *ctx.input_ptr } == b'}' {
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
                return;
            }
            loop {
                // Skip key
                unsafe { fad_json_skip_value(ctx as *mut DeserContext) };
                if ctx.error.code != 0 {
                    return;
                }
                // Expect colon
                unsafe { skip_ws_raw(ctx) };
                if ctx.input_ptr >= ctx.input_end || unsafe { *ctx.input_ptr } != b':' {
                    ctx.error.code = ErrorCode::ExpectedColon as u32;
                    return;
                }
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
                // Skip value
                unsafe { fad_json_skip_value(ctx as *mut DeserContext) };
                if ctx.error.code != 0 {
                    return;
                }
                // Comma or end
                unsafe { skip_ws_raw(ctx) };
                if ctx.input_ptr >= ctx.input_end {
                    ctx.error.code = ErrorCode::UnexpectedEof as u32;
                    return;
                }
                let c = unsafe { *ctx.input_ptr };
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
                if c == b'}' {
                    return;
                }
                if c != b',' {
                    ctx.error.code = ErrorCode::UnexpectedCharacter as u32;
                    return;
                }
            }
        }
        b'[' => {
            // Skip array
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
            unsafe { skip_ws_raw(ctx) };
            if ctx.input_ptr < ctx.input_end && unsafe { *ctx.input_ptr } == b']' {
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
                return;
            }
            loop {
                unsafe { fad_json_skip_value(ctx as *mut DeserContext) };
                if ctx.error.code != 0 {
                    return;
                }
                unsafe { skip_ws_raw(ctx) };
                if ctx.input_ptr >= ctx.input_end {
                    ctx.error.code = ErrorCode::UnexpectedEof as u32;
                    return;
                }
                let c = unsafe { *ctx.input_ptr };
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
                if c == b']' {
                    return;
                }
                if c != b',' {
                    ctx.error.code = ErrorCode::UnexpectedCharacter as u32;
                    return;
                }
            }
        }
        b't' => {
            // true
            if ctx.remaining() >= 4 {
                let s = unsafe { core::slice::from_raw_parts(ctx.input_ptr, 4) };
                if s == b"true" {
                    ctx.input_ptr = unsafe { ctx.input_ptr.add(4) };
                    return;
                }
            }
            ctx.error.code = ErrorCode::UnexpectedCharacter as u32;
        }
        b'f' => {
            // false
            if ctx.remaining() >= 5 {
                let s = unsafe { core::slice::from_raw_parts(ctx.input_ptr, 5) };
                if s == b"false" {
                    ctx.input_ptr = unsafe { ctx.input_ptr.add(5) };
                    return;
                }
            }
            ctx.error.code = ErrorCode::UnexpectedCharacter as u32;
        }
        b'n' => {
            // null
            if ctx.remaining() >= 4 {
                let s = unsafe { core::slice::from_raw_parts(ctx.input_ptr, 4) };
                if s == b"null" {
                    ctx.input_ptr = unsafe { ctx.input_ptr.add(4) };
                    return;
                }
            }
            ctx.error.code = ErrorCode::UnexpectedCharacter as u32;
        }
        b'-' | b'0'..=b'9' => {
            // Skip number
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
            while ctx.input_ptr < ctx.input_end {
                let c = unsafe { *ctx.input_ptr };
                match c {
                    b'0'..=b'9' | b'.' | b'e' | b'E' | b'+' | b'-' => {
                        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
                    }
                    _ => break,
                }
            }
        }
        _ => {
            ctx.error.code = ErrorCode::UnexpectedCharacter as u32;
        }
    }
}

/// Internal whitespace skip that takes &mut DeserContext directly.
unsafe fn skip_ws_raw(ctx: &mut DeserContext) {
    while ctx.input_ptr < ctx.input_end {
        let b = unsafe { *ctx.input_ptr };
        match b {
            b' ' | b'\t' | b'\n' | b'\r' => {
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
            }
            _ => break,
        }
    }
}

/// Skip whitespace, then expect and consume '}'.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_expect_object_end(ctx: *mut DeserContext) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedCharacter as u32;
        return;
    }
    let b = unsafe { *ctx.input_ptr };
    if b != b'}' {
        ctx.error.code = ErrorCode::UnexpectedCharacter as u32;
        return;
    }
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
}

/// Skip whitespace, then expect ',' (write 0 to *out) or '}' (write 1 to *out).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_comma_or_end_object(ctx: *mut DeserContext, out: *mut u8) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let b = unsafe { *ctx.input_ptr };
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
    match b {
        b',' => unsafe { *out = 0 },
        b'}' => unsafe { *out = 1 },
        _ => {
            ctx.error.code = ErrorCode::UnexpectedCharacter as u32;
        }
    }
}

// --- JSON number/bool parsing helpers ---

// r[impl deser.json.scalar.integer]

/// Parse unsigned decimal digits from input, returning the u64 value.
/// Skips leading whitespace. Sets error if no digits found or overflow.
unsafe fn json_parse_unsigned(ctx: &mut DeserContext) -> u64 {
    let mut result: u64 = 0;
    let mut digits = 0u32;

    while ctx.input_ptr < ctx.input_end {
        let b = unsafe { *ctx.input_ptr };
        match b {
            b'0'..=b'9' => {
                let new = result
                    .checked_mul(10)
                    .and_then(|r| r.checked_add((b - b'0') as u64));
                match new {
                    Some(v) => result = v,
                    None => {
                        ctx.error.code = ErrorCode::NumberOutOfRange as u32;
                        return 0;
                    }
                }
                digits += 1;
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
            }
            _ => break,
        }
    }

    if digits == 0 {
        ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
        return 0;
    }

    result
}

/// Parse a possibly-negative decimal integer, returning the i64 value.
/// Skips leading whitespace. Handles optional '-' prefix.
unsafe fn json_parse_signed(ctx: &mut DeserContext) -> i64 {
    let negative = ctx.input_ptr < ctx.input_end && unsafe { *ctx.input_ptr } == b'-';
    if negative {
        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
    }

    let raw = unsafe { json_parse_unsigned(ctx) };
    if ctx.error.code != 0 {
        return 0;
    }

    if negative {
        // i64::MIN magnitude is 9223372036854775808 which is > i64::MAX
        if raw > (i64::MAX as u64) + 1 {
            ctx.error.code = ErrorCode::NumberOutOfRange as u32;
            return 0;
        }
        // Wrapping negate handles i64::MIN correctly
        -(raw as i64)
    } else {
        if raw > i64::MAX as u64 {
            ctx.error.code = ErrorCode::NumberOutOfRange as u32;
            return 0;
        }
        raw as i64
    }
}

/// Extract a JSON number substring (digits, '.', 'e', 'E', '+', '-') without advancing past it,
/// returning the byte slice. The caller parses the number from the slice.
unsafe fn json_extract_number_bytes(ctx: &mut DeserContext) -> &[u8] {
    let start = ctx.input_ptr;
    while ctx.input_ptr < ctx.input_end {
        let b = unsafe { *ctx.input_ptr };
        match b {
            b'0'..=b'9' | b'.' | b'e' | b'E' | b'+' | b'-' => {
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
            }
            _ => break,
        }
    }
    let len = unsafe { ctx.input_ptr.offset_from(start) as usize };
    unsafe { core::slice::from_raw_parts(start, len) }
}

unsafe fn json_parse_u128(ctx: &mut DeserContext) -> u128 {
    let bytes = unsafe { json_extract_number_bytes(ctx) };
    if bytes.is_empty() {
        ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
        return 0;
    }
    let s = match core::str::from_utf8(bytes) {
        Ok(s) => s,
        Err(_) => {
            ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
            return 0;
        }
    };
    match s.parse::<u128>() {
        Ok(v) => v,
        Err(e) => {
            ctx.error.code = match e.kind() {
                IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                    ErrorCode::NumberOutOfRange as u32
                }
                _ => ErrorCode::InvalidJsonNumber as u32,
            };
            0
        }
    }
}

unsafe fn json_parse_i128(ctx: &mut DeserContext) -> i128 {
    let bytes = unsafe { json_extract_number_bytes(ctx) };
    if bytes.is_empty() {
        ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
        return 0;
    }
    let s = match core::str::from_utf8(bytes) {
        Ok(s) => s,
        Err(_) => {
            ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
            return 0;
        }
    };
    match s.parse::<i128>() {
        Ok(v) => v,
        Err(e) => {
            ctx.error.code = match e.kind() {
                IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                    ErrorCode::NumberOutOfRange as u32
                }
                _ => ErrorCode::InvalidJsonNumber as u32,
            };
            0
        }
    }
}

// --- Unsigned integer intrinsics ---

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_u8(ctx: *mut DeserContext, out: *mut u8) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let val = unsafe { json_parse_unsigned(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    if val > u8::MAX as u64 {
        ctx.error.code = ErrorCode::NumberOutOfRange as u32;
        return;
    }
    unsafe { *out = val as u8 };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_u16(ctx: *mut DeserContext, out: *mut u16) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let val = unsafe { json_parse_unsigned(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    if val > u16::MAX as u64 {
        ctx.error.code = ErrorCode::NumberOutOfRange as u32;
        return;
    }
    unsafe { *out = val as u16 };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_u32(ctx: *mut DeserContext, out: *mut u32) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let val = unsafe { json_parse_unsigned(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    if val > u32::MAX as u64 {
        ctx.error.code = ErrorCode::NumberOutOfRange as u32;
        return;
    }
    unsafe { *out = val as u32 };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_u64(ctx: *mut DeserContext, out: *mut u64) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let val = unsafe { json_parse_unsigned(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    unsafe { *out = val };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_u128(ctx: *mut DeserContext, out: *mut u128) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let val = unsafe { json_parse_u128(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    unsafe { *out = val };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_usize(ctx: *mut DeserContext, out: *mut usize) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let val = unsafe { json_parse_u128(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    let v = match usize::try_from(val) {
        Ok(v) => v,
        Err(_) => {
            ctx.error.code = ErrorCode::NumberOutOfRange as u32;
            return;
        }
    };
    unsafe { *out = v };
}

// --- Signed integer intrinsics ---

// r[impl deser.json.scalar.integer]

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_i8(ctx: *mut DeserContext, out: *mut i8) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let val = unsafe { json_parse_signed(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    if val < i8::MIN as i64 || val > i8::MAX as i64 {
        ctx.error.code = ErrorCode::NumberOutOfRange as u32;
        return;
    }
    unsafe { *out = val as i8 };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_i16(ctx: *mut DeserContext, out: *mut i16) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let val = unsafe { json_parse_signed(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    if val < i16::MIN as i64 || val > i16::MAX as i64 {
        ctx.error.code = ErrorCode::NumberOutOfRange as u32;
        return;
    }
    unsafe { *out = val as i16 };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_i32(ctx: *mut DeserContext, out: *mut i32) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let val = unsafe { json_parse_signed(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    if val < i32::MIN as i64 || val > i32::MAX as i64 {
        ctx.error.code = ErrorCode::NumberOutOfRange as u32;
        return;
    }
    unsafe { *out = val as i32 };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_i64(ctx: *mut DeserContext, out: *mut i64) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let val = unsafe { json_parse_signed(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    unsafe { *out = val };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_i128(ctx: *mut DeserContext, out: *mut i128) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let val = unsafe { json_parse_i128(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    unsafe { *out = val };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_isize(ctx: *mut DeserContext, out: *mut isize) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let val = unsafe { json_parse_i128(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    let v = match isize::try_from(val) {
        Ok(v) => v,
        Err(_) => {
            ctx.error.code = ErrorCode::NumberOutOfRange as u32;
            return;
        }
    };
    unsafe { *out = v };
}

// --- Float intrinsics ---

// r[impl deser.json.scalar.float]

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_f32(ctx: *mut DeserContext, out: *mut f32) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let bytes = unsafe { json_extract_number_bytes(ctx) };
    if bytes.is_empty() {
        ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
        return;
    }
    let s = match core::str::from_utf8(bytes) {
        Ok(s) => s,
        Err(_) => {
            ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
            return;
        }
    };
    match s.parse::<f32>() {
        Ok(v) => unsafe { *out = v },
        Err(_) => {
            ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_f64(ctx: *mut DeserContext, out: *mut f64) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let bytes = unsafe { json_extract_number_bytes(ctx) };
    if bytes.is_empty() {
        ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
        return;
    }
    let s = match core::str::from_utf8(bytes) {
        Ok(s) => s,
        Err(_) => {
            ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
            return;
        }
    };
    match s.parse::<f64>() {
        Ok(v) => unsafe { *out = v },
        Err(_) => {
            ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
        }
    }
}

// --- Bool intrinsic ---

// r[impl deser.json.scalar.bool]

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_bool(ctx: *mut DeserContext, out: *mut bool) {
    unsafe { fad_json_skip_ws(ctx) };
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let b = unsafe { *ctx.input_ptr };
    match b {
        b't' => {
            if ctx.remaining() >= 4 {
                let s = unsafe { core::slice::from_raw_parts(ctx.input_ptr, 4) };
                if s == b"true" {
                    ctx.input_ptr = unsafe { ctx.input_ptr.add(4) };
                    unsafe { *out = true };
                    return;
                }
            }
            ctx.error.code = ErrorCode::InvalidBool as u32;
        }
        b'f' => {
            if ctx.remaining() >= 5 {
                let s = unsafe { core::slice::from_raw_parts(ctx.input_ptr, 5) };
                if s == b"false" {
                    ctx.input_ptr = unsafe { ctx.input_ptr.add(5) };
                    unsafe { *out = false };
                    return;
                }
            }
            ctx.error.code = ErrorCode::InvalidBool as u32;
        }
        _ => {
            ctx.error.code = ErrorCode::InvalidBool as u32;
        }
    }
}

/// Parse a JSON string value ("...") and write it to *out as a Rust String.
/// Fast path: no escapes — validates UTF-8, allocates via to_owned().
/// Slow path: escapes found — unescapes into Vec, converts via String::from_utf8.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_string_value(ctx: *mut DeserContext, out: *mut String) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::ExpectedStringKey as u32;
        return;
    }
    if unsafe { *ctx.input_ptr } != b'"' {
        ctx.error.code = ErrorCode::ExpectedStringKey as u32;
        return;
    }
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip opening '"'

    let start = ctx.input_ptr;
    while ctx.input_ptr < ctx.input_end {
        let b = unsafe { *ctx.input_ptr };
        if b == b'"' {
            // Fast path: no escapes
            let len = unsafe { ctx.input_ptr.offset_from(start) as usize };
            let bytes = unsafe { core::slice::from_raw_parts(start, len) };
            match core::str::from_utf8(bytes) {
                Ok(s) => {
                    unsafe { out.write(s.to_owned()) };
                }
                Err(_) => {
                    ctx.error.code = ErrorCode::InvalidUtf8 as u32;
                    return;
                }
            }
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip closing '"'
            return;
        }
        if b == b'\\' {
            // Slow path: unescape into Vec
            let prefix_len = unsafe { ctx.input_ptr.offset_from(start) as usize };
            let s = match unsafe { json_read_string_value_slow_to_string(ctx, start, prefix_len) } {
                Some(s) => s,
                None => return,
            };
            unsafe { out.write(s) };
            return;
        }
        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
    }
    ctx.error.code = ErrorCode::UnterminatedString as u32;
}

/// Parse a JSON string value ("...") and write it as a borrowed `&str`.
///
/// This only supports the no-escape fast path. Escaped strings require
/// transformation and therefore cannot produce a direct borrow.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_str_value(ctx: *mut DeserContext, out: *mut &str) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::ExpectedStringKey as u32;
        return;
    }
    if unsafe { *ctx.input_ptr } != b'"' {
        ctx.error.code = ErrorCode::ExpectedStringKey as u32;
        return;
    }
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip opening '"'

    let start = ctx.input_ptr;
    while ctx.input_ptr < ctx.input_end {
        let b = unsafe { *ctx.input_ptr };
        if b == b'"' {
            let len = unsafe { ctx.input_ptr.offset_from(start) as usize };
            let bytes = unsafe { core::slice::from_raw_parts(start, len) };
            let s = match core::str::from_utf8(bytes) {
                Ok(s) => s,
                Err(_) => {
                    ctx.error.code = ErrorCode::InvalidUtf8 as u32;
                    return;
                }
            };
            let s_static: &'static str = unsafe { core::mem::transmute::<&str, &'static str>(s) };
            unsafe { out.write(s_static) };
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip closing '"'
            return;
        }
        if b == b'\\' {
            // Escapes require unescaping; borrowed &str cannot represent that
            // without allocation.
            ctx.error.code = ErrorCode::InvalidEscapeSequence as u32;
            return;
        }
        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
    }
    ctx.error.code = ErrorCode::UnterminatedString as u32;
}

/// Parse a JSON string value ("...") and write it as `Cow<str>`.
///
/// Fast path (no escapes): `Cow::Borrowed`.
/// Slow path (escapes): `Cow::Owned`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_cow_str_value(
    ctx: *mut DeserContext,
    out: *mut Cow<'static, str>,
) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::ExpectedStringKey as u32;
        return;
    }
    if unsafe { *ctx.input_ptr } != b'"' {
        ctx.error.code = ErrorCode::ExpectedStringKey as u32;
        return;
    }
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip opening '"'

    let start = ctx.input_ptr;
    while ctx.input_ptr < ctx.input_end {
        let b = unsafe { *ctx.input_ptr };
        if b == b'"' {
            let len = unsafe { ctx.input_ptr.offset_from(start) as usize };
            let bytes = unsafe { core::slice::from_raw_parts(start, len) };
            let s = match core::str::from_utf8(bytes) {
                Ok(s) => s,
                Err(_) => {
                    ctx.error.code = ErrorCode::InvalidUtf8 as u32;
                    return;
                }
            };
            let s_static: &'static str = unsafe { core::mem::transmute::<&str, &'static str>(s) };
            unsafe { out.write(Cow::Borrowed(s_static)) };
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip closing '"'
            return;
        }
        if b == b'\\' {
            let prefix_len = unsafe { ctx.input_ptr.offset_from(start) as usize };
            let s = match unsafe { json_read_string_value_slow_to_string(ctx, start, prefix_len) } {
                Some(s) => s,
                None => return,
            };
            unsafe { out.write(Cow::Owned(s)) };
            return;
        }
        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
    }
    ctx.error.code = ErrorCode::UnterminatedString as u32;
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_char(ctx: *mut DeserContext, out: *mut char) {
    let mut s = String::new();
    unsafe { fad_json_read_string_value(ctx, &mut s) };
    let ctx = unsafe { &mut *ctx };
    if ctx.error.code != 0 {
        return;
    }
    let mut chars = s.chars();
    let ch = match chars.next() {
        Some(ch) => ch,
        None => {
            ctx.error.code = ErrorCode::InvalidUtf8 as u32;
            return;
        }
    };
    if chars.next().is_some() {
        ctx.error.code = ErrorCode::InvalidUtf8 as u32;
        return;
    }
    unsafe { *out = ch };
}

/// Slow path for string value reading: unescapes into a Vec, converts to String.
unsafe fn json_read_string_value_slow_to_string(
    ctx: &mut DeserContext,
    prefix_start: *const u8,
    prefix_len: usize,
) -> Option<String> {
    let mut buf = Vec::with_capacity(prefix_len + 32);
    buf.extend_from_slice(unsafe { core::slice::from_raw_parts(prefix_start, prefix_len) });

    while ctx.input_ptr < ctx.input_end {
        let b = unsafe { *ctx.input_ptr };
        if b == b'"' {
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip closing '"'
            match String::from_utf8(buf) {
                Ok(s) => return Some(s),
                Err(_) => {
                    ctx.error.code = ErrorCode::InvalidUtf8 as u32;
                }
            }
            return None;
        }
        if b == b'\\' {
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip '\'
            if !unsafe { json_unescape_one(ctx, &mut buf) } {
                return None; // error set
            }
        } else {
            buf.push(b);
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
        }
    }
    ctx.error.code = ErrorCode::UnterminatedString as u32;
    None
}

/// Set ExpectedTagKey error. Used when the first key in an adjacently/internally
/// tagged enum is not the expected tag key.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_error_expected_tag_key(ctx: *mut DeserContext) {
    let ctx = unsafe { &mut *ctx };
    ctx.error.code = ErrorCode::ExpectedTagKey as u32;
}

// --- JSON array intrinsics ---

/// Skip whitespace, then expect and consume '['.
#[allow(clippy::missing_safety_doc)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_expect_array_start(ctx: *mut DeserContext) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedCharacter as u32;
        return;
    }
    let b = unsafe { *ctx.input_ptr };
    if b != b'[' {
        ctx.error.code = ErrorCode::UnexpectedCharacter as u32;
        return;
    }
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
}

/// Skip whitespace, then expect ',' (write 0 to *out) or ']' (write 1 to *out).
#[allow(clippy::missing_safety_doc)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_comma_or_end_array(ctx: *mut DeserContext, out: *mut u8) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let b = unsafe { *ctx.input_ptr };
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
    match b {
        b',' => unsafe { *out = 0 },
        b']' => unsafe { *out = 1 },
        _ => {
            ctx.error.code = ErrorCode::UnexpectedCharacter as u32;
        }
    }
}
