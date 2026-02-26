use crate::context::{DeserContext, ErrorCode};

// r[impl callconv.intrinsics]

/// Read a postcard varint-encoded u32 from the input, write to `*out`.
///
/// Postcard varints encode 7 bits per byte, MSB is continuation flag.
/// Maximum 5 bytes for u32.
///
/// # Safety
/// `ctx` and `out` must be valid pointers. `ctx.input_ptr`..`ctx.input_end` must be a valid range.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_varint_u32(ctx: *mut DeserContext, out: *mut u32) {
    let ctx = unsafe { &mut *ctx };
    let mut result: u32 = 0;
    let mut shift: u32 = 0;

    loop {
        if ctx.input_ptr >= ctx.input_end {
            ctx.error.code = ErrorCode::UnexpectedEof as u32;
            return;
        }

        let byte = unsafe { *ctx.input_ptr };
        ctx.input_ptr = unsafe { ctx.input_ptr.add(1) };

        // Low 7 bits contribute to the value
        let value_bits = (byte & 0x7F) as u32;

        // Check for overflow: if we're at shift >= 28 and the byte has high bits set
        if shift >= 28 && (byte & 0x70) != 0 {
            ctx.error.code = ErrorCode::InvalidVarint as u32;
            return;
        }

        result |= value_bits << shift;
        shift += 7;

        // If MSB is 0, this is the last byte
        if byte & 0x80 == 0 {
            break;
        }

        if shift >= 35 {
            ctx.error.code = ErrorCode::InvalidVarint as u32;
            return;
        }
    }

    unsafe { *out = result };
}

/// Read a postcard length-prefixed string from the input, write to `*out`.
///
/// Format: varint length, then that many bytes of UTF-8.
///
/// # Safety
/// `ctx` and `out` must be valid pointers. `out` must point to an initialized (possibly zeroed)
/// `String` location. `ctx.input_ptr`..`ctx.input_end` must be a valid range.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_read_postcard_string(ctx: *mut DeserContext, out: *mut String) {
    // Read the length as a varint
    let mut len: u32 = 0;
    unsafe { fad_read_varint_u32(ctx, &mut len) };

    let ctx = unsafe { &mut *ctx };
    if ctx.error.code != 0 {
        return;
    }

    let len = len as usize;

    // Check we have enough bytes
    let remaining = unsafe { ctx.input_end.offset_from(ctx.input_ptr) as usize };
    if remaining < len {
        ctx.error.code = ErrorCode::UnexpectedEof as u32;
        return;
    }

    // Validate UTF-8
    let bytes = unsafe { core::slice::from_raw_parts(ctx.input_ptr, len) };
    let s = match core::str::from_utf8(bytes) {
        Ok(s) => s,
        Err(_) => {
            ctx.error.code = ErrorCode::InvalidUtf8 as u32;
            return;
        }
    };

    // Write the String to the output slot
    unsafe { out.write(s.to_owned()) };

    // Advance cursor
    ctx.input_ptr = unsafe { ctx.input_ptr.add(len) };
}
