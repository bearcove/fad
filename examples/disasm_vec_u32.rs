use facet::Facet;
use std::fmt::Write;
use yaxpeax_arch::{Decoder, U8Reader};

#[derive(Facet, Debug, PartialEq)]
struct Nums {
    vals: Vec<u32>,
}

#[derive(serde::Deserialize, Debug)]
#[allow(dead_code)]
struct NumsSerde {
    vals: Vec<u32>,
}

fn serde_deser(data: &[u8]) -> NumsSerde {
    postcard::from_bytes(data).unwrap()
}

fn main() {
    // === fad JIT ===
    let deser = fad::compile_deser(Nums::SHAPE, &fad::postcard::FadPostcard);
    let code = deser.code();
    let base = code.as_ptr() as u64;
    println!("=== fad postcard Vec<u32> ===");
    println!(
        "{}",
        disasm_bytes(code, base, Some(deser.entry_offset()), false)
    );

    // === serde (follow bl targets) ===
    let fn_ptr = serde_deser as *const u8;
    println!("\n=== serde postcard Vec<u32> @ {fn_ptr:?} ===");
    let main_asm = unsafe { disasm_with_callees(fn_ptr, 4096, 3) };
    println!("{main_asm}");
}

/// Disassemble starting at `fn_ptr`, then recursively disassemble
/// any `bl` targets up to `depth` levels deep.
unsafe fn disasm_with_callees(fn_ptr: *const u8, max_bytes: usize, depth: u32) -> String {
    let mut out = String::new();
    let mut visited: Vec<u64> = Vec::new();
    disasm_recursive(fn_ptr, max_bytes, depth, &mut visited, &mut out);
    out
}

unsafe fn disasm_recursive(
    fn_ptr: *const u8,
    max_bytes: usize,
    depth: u32,
    visited: &mut Vec<u64>,
    out: &mut String,
) {
    let addr = fn_ptr as u64;
    if visited.contains(&addr) {
        return;
    }
    visited.push(addr);

    let code = unsafe { std::slice::from_raw_parts(fn_ptr, max_bytes) };
    let asm = disasm_bytes(code, addr, Some(0), true);
    writeln!(out, "--- function @ {addr:#x} ---").unwrap();
    out.push_str(&asm);
    out.push('\n');

    if depth == 0 {
        return;
    }

    // Find bl targets and recurse
    let targets = extract_bl_targets(code, addr);
    for target in targets {
        // Only follow targets that look like they're in the same binary
        // (within a reasonable range of the current function)
        let offset = (target as i64) - (addr as i64);
        if offset.unsigned_abs() < 16 * 1024 * 1024 {
            unsafe {
                disasm_recursive(target as *const u8, max_bytes, depth - 1, visited, out);
            }
        }
    }
}

#[cfg(target_arch = "aarch64")]
fn extract_bl_targets(code: &[u8], base_addr: u64) -> Vec<u64> {
    let mut targets = Vec::new();
    let mut offset = 0usize;
    let mut ret_count = 0u32;

    while offset + 4 <= code.len() {
        let word = u32::from_le_bytes(code[offset..offset + 4].try_into().unwrap());
        // bl instruction: bits [31:26] == 0b100101
        if (word >> 26) == 0b100101 {
            // Extract signed 26-bit immediate, shift left 2
            let imm26 = word & 0x03FF_FFFF;
            let imm = if imm26 & (1 << 25) != 0 {
                // Sign extend
                ((imm26 | 0xFC00_0000) as i32) << 2
            } else {
                (imm26 as i32) << 2
            };
            let target = (base_addr as i64 + offset as i64 + imm as i64) as u64;
            if !targets.contains(&target) {
                targets.push(target);
            }
        }
        // Check for ret to stop
        if word == 0xD65F03C0 {
            ret_count += 1;
            if ret_count >= 2 {
                break;
            }
        }
        offset += 4;
    }
    targets
}

#[cfg(target_arch = "x86_64")]
fn extract_bl_targets(code: &[u8], base_addr: u64) -> Vec<u64> {
    use yaxpeax_x86::amd64::InstDecoder;

    let mut targets = Vec::new();
    let decoder = InstDecoder::default();
    let mut reader = U8Reader::new(code);
    let mut offset = 0usize;
    let mut ret_count = 0u32;

    while offset < code.len() {
        match decoder.decode(&mut reader) {
            Ok(inst) => {
                let len = inst.len().to_const() as usize;
                let text = format!("{inst}");
                // Look for "call $+0x..." pattern
                if text.starts_with("call ") {
                    if let Some(target) = parse_call_target(&text, base_addr + offset as u64) {
                        if !targets.contains(&target) {
                            targets.push(target);
                        }
                    }
                }
                if text.trim() == "ret" {
                    ret_count += 1;
                    if ret_count >= 2 {
                        break;
                    }
                }
                offset += len;
            }
            Err(_) => {
                offset += 1;
            }
        }
    }
    targets
}

#[cfg(target_arch = "x86_64")]
fn parse_call_target(text: &str, current_addr: u64) -> Option<u64> {
    // Parse "call $+0x1234" or "call $-0x1234"
    let text = text.trim();
    if let Some(rest) = text.strip_prefix("call $") {
        if let Some(hex) = rest.strip_prefix("+0x") {
            if let Ok(offset) = u64::from_str_radix(hex, 16) {
                return Some(current_addr.wrapping_add(offset));
            }
        } else if let Some(hex) = rest.strip_prefix("-0x") {
            if let Ok(offset) = u64::from_str_radix(hex, 16) {
                return Some(current_addr.wrapping_sub(offset));
            }
        }
    }
    None
}

fn disasm_bytes(
    code: &[u8],
    base_addr: u64,
    marker_offset: Option<usize>,
    stop_at_ret: bool,
) -> String {
    let mut out = String::new();

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
                    if stop_at_ret {
                        let text = format!("{inst}");
                        if text.trim() == "ret" {
                            ret_count += 1;
                            if ret_count >= 2 {
                                break;
                            }
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
                    if stop_at_ret {
                        let text = format!("{inst}");
                        if text.trim() == "ret" {
                            ret_count += 1;
                            if ret_count >= 2 {
                                break;
                            }
                        }
                    }
                    offset += len;
                }
                Err(_) => {
                    let addr = base_addr + offset as u64;
                    writeln!(&mut out, "{addr:12x}:{marker}  <decode error> (0x{:02x})", code[offset]).unwrap();
                    offset += 1;
                }
            }
        }
    }

    out
}
