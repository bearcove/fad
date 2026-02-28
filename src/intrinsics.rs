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

// --- Option intrinsics ---

/// Initialize an Option with None using the vtable's init_none function.
///
/// Wraps the facet OptionVTable's init_none, which takes wide pointer types
/// (PtrUninit), into a thin `extern "C"` interface callable from JIT code.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_option_init_none(
    init_none_fn: facet::OptionInitNoneFn,
    out: *mut u8,
) {
    let ptr_uninit = facet::PtrUninit::new_sized(out);
    unsafe { (init_none_fn)(ptr_uninit) };
}

/// Initialize an Option with Some(value) using the vtable's init_some function.
///
/// `value_ptr` points to an already-deserialized T. init_some will _move_ it
/// (read + write into the Option), so the caller must not use value_ptr afterwards.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_option_init_some(
    init_some_fn: facet::OptionInitSomeFn,
    out: *mut u8,
    value_ptr: *mut u8,
) {
    let ptr_uninit = facet::PtrUninit::new_sized(out);
    let ptr_mut = facet::PtrMut::new_sized(value_ptr);
    unsafe { (init_some_fn)(ptr_uninit, ptr_mut) };
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

/// Validate UTF-8, allocate raw buffer, and copy bytes. Returns buffer pointer.
///
/// Malum string intrinsic — the JIT writes the returned pointer + len directly
/// into the String's `(ptr, len, cap)` fields at discovered offsets, bypassing
/// the intermediate `String` object.
///
/// Returns:
/// - On success: pointer to allocated buffer containing the string bytes
/// - On empty (data_len == 0): `1 as *mut u8` (dangling aligned pointer)
/// - On error: null pointer (error code set on ctx)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_string_validate_alloc_copy(
    ctx: *mut DeserContext,
    data_ptr: *const u8,
    data_len: u32,
) -> *mut u8 {
    let len = data_len as usize;

    // Empty string: return dangling pointer, JIT writes ptr/0/0.
    if len == 0 {
        return std::mem::align_of::<u8>() as *mut u8;
    }

    // Validate UTF-8.
    let bytes = unsafe { core::slice::from_raw_parts(data_ptr, len) };
    if core::str::from_utf8(bytes).is_err() {
        let ctx = unsafe { &mut *ctx };
        ctx.error.code = ErrorCode::InvalidUtf8 as u32;
        return core::ptr::null_mut();
    }

    // Allocate raw buffer (same allocator String uses).
    let layout = unsafe { std::alloc::Layout::from_size_align_unchecked(len, 1) };
    let buf = unsafe { std::alloc::alloc(layout) };
    if buf.is_null() {
        let ctx = unsafe { &mut *ctx };
        ctx.error.code = ErrorCode::AllocError as u32;
        return core::ptr::null_mut();
    }

    // Copy bytes.
    unsafe { core::ptr::copy_nonoverlapping(data_ptr, buf, len) };
    buf
}

// --- Vec intrinsics ---

// r[impl seq.malum.alloc-compat]

/// Allocate a buffer for `count` elements of `elem_size` bytes, `elem_align` alignment.
///
/// Uses `std::alloc::alloc` with `Layout::from_size_align(count * elem_size, elem_align)` —
/// the same allocator and layout that `Vec<T>` would use, so the resulting buffer can be
/// owned by a Vec and deallocated normally.
///
/// Returns a pointer to the allocated buffer. On allocation failure, sets an error on ctx
/// and returns a null pointer.
///
/// # Safety
/// - `count` must be > 0 (caller handles the empty case)
/// - `elem_size` and `elem_align` must be valid for `Layout::from_size_align`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_vec_alloc(
    ctx: *mut DeserContext,
    count: usize,
    elem_size: usize,
    elem_align: usize,
) -> *mut u8 {
    let size = count.checked_mul(elem_size).unwrap_or(0);
    if size == 0 {
        return core::ptr::null_mut();
    }
    let layout = match std::alloc::Layout::from_size_align(size, elem_align) {
        Ok(layout) => layout,
        Err(_) => {
            let ctx = unsafe { &mut *ctx };
            ctx.error.code = ErrorCode::AllocError as u32;
            return core::ptr::null_mut();
        }
    };
    let ptr = unsafe { std::alloc::alloc(layout) };
    if ptr.is_null() {
        let ctx = unsafe { &mut *ctx };
        ctx.error.code = ErrorCode::AllocError as u32;
    }
    ptr
}

/// Grow a Vec buffer: allocate a new buffer of `new_cap * elem_size`, copy
/// `len * elem_size` bytes from `old_buf`, and deallocate `old_buf`.
///
/// Returns the new buffer pointer. On allocation failure, the old buffer is NOT freed
/// and an error is set on ctx.
///
/// # Safety
/// - `old_buf` must have been allocated with `Layout::from_size_align(old_cap * elem_size, elem_align)`
/// - `len <= old_cap`
/// - `new_cap > old_cap`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_vec_grow(
    ctx: *mut DeserContext,
    old_buf: *mut u8,
    len: usize,
    old_cap: usize,
    new_cap: usize,
    elem_size: usize,
    elem_align: usize,
) -> *mut u8 {
    let new_size = new_cap * elem_size;
    let new_layout = match std::alloc::Layout::from_size_align(new_size, elem_align) {
        Ok(layout) => layout,
        Err(_) => {
            let ctx = unsafe { &mut *ctx };
            ctx.error.code = ErrorCode::AllocError as u32;
            return old_buf;
        }
    };
    let new_buf = unsafe { std::alloc::alloc(new_layout) };
    if new_buf.is_null() {
        let ctx = unsafe { &mut *ctx };
        ctx.error.code = ErrorCode::AllocError as u32;
        return old_buf;
    }

    // Copy existing elements.
    let copy_size = len * elem_size;
    if copy_size > 0 {
        unsafe { core::ptr::copy_nonoverlapping(old_buf, new_buf, copy_size) };
    }

    // Free old buffer.
    let old_size = old_cap * elem_size;
    if old_size > 0 {
        let old_layout = unsafe {
            std::alloc::Layout::from_size_align_unchecked(old_size, elem_align)
        };
        unsafe { std::alloc::dealloc(old_buf, old_layout) };
    }

    new_buf
}

/// Free a Vec buffer. Called on error paths to clean up partially-built Vecs.
///
/// # Safety
/// - `buf` must have been allocated with `Layout::from_size_align(cap * elem_size, elem_align)`
/// - `buf` must not be null (caller checks)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fad_vec_free(
    buf: *mut u8,
    cap: usize,
    elem_size: usize,
    elem_align: usize,
) {
    let size = cap * elem_size;
    if size > 0 && !buf.is_null() {
        let layout = unsafe {
            std::alloc::Layout::from_size_align_unchecked(size, elem_align)
        };
        unsafe { std::alloc::dealloc(buf, layout) };
    }
}
