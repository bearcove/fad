use crate::context::{DeserContext, ErrorCode};

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

/// Read a JSON string key (must be "..."), writing the borrowed pointer and length.
/// Does NOT handle escape sequences — panics if a backslash is encountered.
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
            let len = unsafe { ctx.input_ptr.offset_from(start) as usize };
            unsafe {
                *out_ptr = start;
                *out_len = len;
            }
            ctx.input_ptr = unsafe { ctx.input_ptr.add(1) }; // skip closing '"'
            return;
        }
        if b == b'\\' {
            panic!("fad_json_read_key: escape sequences not yet supported");
        }
        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
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
                        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
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
                if ctx.error.code != 0 { return; }
                // Expect colon
                unsafe { skip_ws_raw(ctx) };
                if ctx.input_ptr >= ctx.input_end || unsafe { *ctx.input_ptr } != b':' {
                    ctx.error.code = ErrorCode::ExpectedColon as u32;
                    return;
                }
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
                // Skip value
                unsafe { fad_json_skip_value(ctx as *mut DeserContext) };
                if ctx.error.code != 0 { return; }
                // Comma or end
                unsafe { skip_ws_raw(ctx) };
                if ctx.input_ptr >= ctx.input_end {
                    ctx.error.code = ErrorCode::UnexpectedEof as u32;
                    return;
                }
                let c = unsafe { *ctx.input_ptr };
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
                if c == b'}' { return; }
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
                if ctx.error.code != 0 { return; }
                unsafe { skip_ws_raw(ctx) };
                if ctx.input_ptr >= ctx.input_end {
                    ctx.error.code = ErrorCode::UnexpectedEof as u32;
                    return;
                }
                let c = unsafe { *ctx.input_ptr };
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
                if c == b']' { return; }
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

/// Parse a JSON integer (unsigned, no decimals) and write it to *out as u32.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_json_read_u32(ctx: *mut DeserContext, out: *mut u32) {
    unsafe {
        fad_json_skip_ws(ctx);
    }
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }

    let mut result: u64 = 0;
    let mut digits = 0u32;

    while ctx.input_ptr < ctx.input_end {
        let b = unsafe { *ctx.input_ptr };
        match b {
            b'0'..=b'9' => {
                result = result * 10 + (b - b'0') as u64;
                if result > u32::MAX as u64 {
                    ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
                    return;
                }
                digits += 1;
                ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
            }
            _ => break,
        }
    }

    if digits == 0 {
        ctx.error.code = ErrorCode::InvalidJsonNumber as u32;
        return;
    }

    unsafe { *out = result as u32 };
}

/// Parse a JSON string value ("...") and write it to *out as a Rust String.
/// Does NOT handle escape sequences — panics on backslash.
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
            panic!("fad_json_read_string_value: escape sequences not yet supported");
        }
        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
    }
    ctx.error.code = ErrorCode::UnterminatedString as u32;
}
