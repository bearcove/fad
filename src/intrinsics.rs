use crate::context::{DeserContext, ErrorCode};

// r[impl callconv.intrinsics]

// --- Varint helpers ---

/// Read a LEB128-encoded unsigned varint, returning up to 64 bits.
/// Sets error on malformed varint or EOF.
unsafe fn read_varint_u64(ctx: &mut DeserContext) -> u64 {
    let mut result: u64 = 0;
    let mut shift: u32 = 0;

    loop {
        if ctx.input_ptr >= ctx.input_end {
            ctx.error.code = ErrorCode::UnexpectedEof as u32;
            return 0;
        }

        let byte = unsafe { *ctx.input_ptr };
        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };

        let value_bits = (byte & 0x7F) as u64;

        if shift >= 63 && (byte & 0x7E) != 0 {
            ctx.error.code = ErrorCode::InvalidVarint as u32;
            return 0;
        }

        result |= value_bits << shift;
        shift += 7;

        if byte & 0x80 == 0 {
            return result;
        }

        if shift >= 70 {
            ctx.error.code = ErrorCode::InvalidVarint as u32;
            return 0;
        }
    }
}

/// Decode a ZigZag-encoded i64 from a u64.
fn zigzag_decode(encoded: u64) -> i64 {
    ((encoded >> 1) as i64) ^ -((encoded & 1) as i64)
}

// --- Unsigned integer intrinsics ---

// r[impl deser.postcard.scalar.bool]

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_bool(ctx: *mut DeserContext, out: *mut bool) {
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let byte = unsafe { *ctx.input_ptr };
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
    match byte {
        0 => unsafe { *out = false },
        1 => unsafe { *out = true },
        _ => ctx.error.code = ErrorCode::InvalidBool as u32,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_u8(ctx: *mut DeserContext, out: *mut u8) {
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let byte = unsafe { *ctx.input_ptr };
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
    unsafe { *out = byte };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_u16(ctx: *mut DeserContext, out: *mut u16) {
    let ctx = unsafe { &mut *ctx };
    let val = unsafe { read_varint_u64(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    if val > u16::MAX as u64 {
        ctx.error.code = ErrorCode::NumberOutOfRange as u32;
        return;
    }
    unsafe { *out = val as u16 };
}

// r[impl deser.postcard.scalar.varint]

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_varint_u32(ctx: *mut DeserContext, out: *mut u32) {
    let ctx = unsafe { &mut *ctx };
    let val = unsafe { read_varint_u64(ctx) };
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
pub unsafe extern "C" fn fad_read_u64(ctx: *mut DeserContext, out: *mut u64) {
    let ctx = unsafe { &mut *ctx };
    let val = unsafe { read_varint_u64(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    unsafe { *out = val };
}

// --- Signed integer intrinsics ---
// i8: raw byte in two's complement (per postcard spec)
// i16/i32/i64: ZigZag + varint

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_i8(ctx: *mut DeserContext, out: *mut i8) {
    let ctx = unsafe { &mut *ctx };
    if ctx.input_ptr >= ctx.input_end {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let byte = unsafe { *ctx.input_ptr };
    ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };
    unsafe { *out = byte as i8 };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_i16(ctx: *mut DeserContext, out: *mut i16) {
    let ctx = unsafe { &mut *ctx };
    let val = unsafe { read_varint_u64(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    let decoded = zigzag_decode(val);
    if decoded < i16::MIN as i64 || decoded > i16::MAX as i64 {
        ctx.error.code = ErrorCode::NumberOutOfRange as u32;
        return;
    }
    unsafe { *out = decoded as i16 };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_i32(ctx: *mut DeserContext, out: *mut i32) {
    let ctx = unsafe { &mut *ctx };
    let val = unsafe { read_varint_u64(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    let decoded = zigzag_decode(val);
    if decoded < i32::MIN as i64 || decoded > i32::MAX as i64 {
        ctx.error.code = ErrorCode::NumberOutOfRange as u32;
        return;
    }
    unsafe { *out = decoded as i32 };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_i64(ctx: *mut DeserContext, out: *mut i64) {
    let ctx = unsafe { &mut *ctx };
    let val = unsafe { read_varint_u64(ctx) };
    if ctx.error.code != 0 {
        return;
    }
    unsafe { *out = zigzag_decode(val) };
}

// --- Float intrinsics (little-endian IEEE 754) ---

// r[impl deser.postcard.scalar.float]

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_f32(ctx: *mut DeserContext, out: *mut f32) {
    let ctx = unsafe { &mut *ctx };
    let remaining = unsafe { ctx.input_end.offset_from(ctx.input_ptr) as usize };
    if remaining < 4 {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let bytes: [u8; 4] = unsafe { core::ptr::read_unaligned(ctx.input_ptr as *const [u8; 4]) };
    ctx.input_ptr = unsafe { ctx.input_ptr.add(4) };
    unsafe { *out = f32::from_le_bytes(bytes) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_f64(ctx: *mut DeserContext, out: *mut f64) {
    let ctx = unsafe { &mut *ctx };
    let remaining = unsafe { ctx.input_end.offset_from(ctx.input_ptr) as usize };
    if remaining < 8 {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }
    let bytes: [u8; 8] = unsafe { core::ptr::read_unaligned(ctx.input_ptr as *const [u8; 8]) };
    ctx.input_ptr = unsafe { ctx.input_ptr.add(8) };
    unsafe { *out = f64::from_le_bytes(bytes) };
}

// --- String intrinsic ---

/// Read a postcard length-prefixed string from the input, write to `*out`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_postcard_string(ctx: *mut DeserContext, out: *mut String) {
    let mut len: u32 = 0;
    unsafe { fad_read_varint_u32(ctx, &mut len) };

    let ctx = unsafe { &mut *ctx };
    if ctx.error.code != 0 {
        return;
    }

    let len = len as usize;

    let remaining = unsafe { ctx.input_end.offset_from(ctx.input_ptr) as usize };
    if remaining < len {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }

    let bytes = unsafe { core::slice::from_raw_parts(ctx.input_ptr, len) };
    let s = match core::str::from_utf8(bytes) {
        Ok(s) => s,
        Err(_) => {
            ctx.error.code = ErrorCode::InvalidUtf8 as u32;
            return;
        }
    };

    unsafe { out.write(s.to_owned()) };
    ctx.input_ptr = unsafe { ctx.input_ptr.add(len) };
}

/// Validate UTF-8 and allocate a String from a raw byte slice, write to `*out`.
///
/// This is the "lean" string intrinsic — it does NOT read the length varint,
/// bounds check the input, or advance the cursor. The JIT inlines those parts.
/// This intrinsic only handles the work that can't be inlined: UTF-8 validation
/// and heap allocation.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_postcard_validate_and_alloc_string(
    ctx: *mut DeserContext,
    out: *mut String,
    data_ptr: *const u8,
    data_len: u32,
) {
    let len = data_len as usize;
    let bytes = unsafe { core::slice::from_raw_parts(data_ptr, len) };
    let s = match core::str::from_utf8(bytes) {
        Ok(s) => s,
        Err(_) => {
            let ctx = unsafe { &mut *ctx };
            ctx.error.code = ErrorCode::InvalidUtf8 as u32;
            return;
        }
    };
    unsafe { out.write(s.to_owned()) };
}
