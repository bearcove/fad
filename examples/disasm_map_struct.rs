//! Disassemble JIT code for JSON HashMap<String, FriendFacet> deserialization.
//!
//! cargo run --example disasm_map_struct

use facet::Facet;
use std::collections::HashMap;
use std::fmt::Write;
use yaxpeax_arch::{Decoder, U8Reader};

#[derive(Facet, Debug, PartialEq)]
struct FriendFacet {
    age: u32,
    name: String,
}

#[derive(Facet, Debug, PartialEq)]
struct MapStructFacet {
    roster: HashMap<String, FriendFacet>,
}

fn main() {
    // Build a symbol table: intrinsic address → name.
    let syms = build_symbol_table();

    let deser = fad::compile_decoder(MapStructFacet::SHAPE, &fad::json::FadJson);
    let code = deser.code();
    let base = code.as_ptr() as u64;
    let entry = deser.entry_offset();

    println!("=== fad JSON HashMap<String, FriendFacet> ===");
    println!("code buffer: {} bytes ({:#x}), entry: {entry:#x}", code.len(), code.len());
    println!();

    println!("=== symbol table ===");
    let mut sorted: Vec<_> = syms.iter().collect();
    sorted.sort_by_key(|(a, _)| *a);
    for (addr, name) in &sorted {
        println!("  {addr:#018x}  {name}");
    }
    println!();

    // Start at entry, follow intra-buffer bl targets recursively.
    let asm = disasm_recursive_from(base + entry as u64, code, base, &syms, 4);
    println!("{asm}");
}

fn build_symbol_table() -> HashMap<u64, &'static str> {
    use fad::intrinsics::{
        fad_map_build, fad_vec_alloc, fad_vec_free, fad_vec_grow,
    };
    use fad::json_intrinsics::{
        fad_json_comma_or_end_array, fad_json_comma_or_end_object, fad_json_expect_colon,
        fad_json_expect_object_start, fad_json_key_equals, fad_json_peek_after_ws,
        fad_json_read_bool, fad_json_read_f32, fad_json_read_f64, fad_json_read_i16,
        fad_json_read_i32, fad_json_read_i64, fad_json_read_i8, fad_json_read_key,
        fad_json_read_string_value, fad_json_read_u16, fad_json_read_u32, fad_json_read_u64,
        fad_json_read_u8, fad_json_skip_value,
    };

    let mut m = HashMap::new();
    macro_rules! sym {
        ($fn:ident) => {
            m.insert($fn as *const () as u64, stringify!($fn));
        };
    }
    sym!(fad_json_expect_object_start);
    sym!(fad_json_peek_after_ws);
    sym!(fad_json_read_key);
    sym!(fad_json_expect_colon);
    sym!(fad_json_key_equals);
    sym!(fad_json_skip_value);
    sym!(fad_json_comma_or_end_object);
    sym!(fad_json_comma_or_end_array);
    sym!(fad_json_read_u8);
    sym!(fad_json_read_u16);
    sym!(fad_json_read_u32);
    sym!(fad_json_read_u64);
    sym!(fad_json_read_i8);
    sym!(fad_json_read_i16);
    sym!(fad_json_read_i32);
    sym!(fad_json_read_i64);
    sym!(fad_json_read_f32);
    sym!(fad_json_read_f64);
    sym!(fad_json_read_bool);
    sym!(fad_json_read_string_value);
    sym!(fad_vec_alloc);
    sym!(fad_vec_grow);
    sym!(fad_vec_free);
    sym!(fad_map_build);
    m
}

fn disasm_recursive_from(
    fn_addr: u64,
    code: &[u8],
    base: u64,
    syms: &HashMap<u64, &'static str>,
    depth: u32,
) -> String {
    let mut out = String::new();
    let mut visited: Vec<u64> = Vec::new();
    disasm_fn(fn_addr, code, base, syms, depth, &mut visited, &mut out);
    out
}

fn disasm_fn(
    fn_addr: u64,
    code: &[u8],
    base: u64,
    syms: &HashMap<u64, &'static str>,
    depth: u32,
    visited: &mut Vec<u64>,
    out: &mut String,
) {
    if visited.contains(&fn_addr) {
        return;
    }
    visited.push(fn_addr);

    let offset = (fn_addr - base) as usize;
    if offset >= code.len() {
        writeln!(out, "--- {fn_addr:#x}: outside buffer, skipping ---").unwrap();
        return;
    }

    writeln!(out, "--- fn @ {fn_addr:#x} (buffer +{offset:#x}) ---").unwrap();
    let (text, bl_targets) = disasm_bytes(&code[offset..], fn_addr, syms, true);
    out.push_str(&text);
    out.push('\n');

    if depth == 0 {
        return;
    }

    // Follow intra-buffer bl targets
    for target in bl_targets {
        if target >= base && target < base + code.len() as u64 {
            disasm_fn(target, code, base, syms, depth - 1, visited, out);
        }
    }
}

// ── AArch64 ──────────────────────────────────────────────────────────────────

#[cfg(target_arch = "aarch64")]
fn disasm_bytes(
    code: &[u8],
    base_addr: u64,
    syms: &HashMap<u64, &'static str>,
    stop_at_ret: bool,
) -> (String, Vec<u64>) {
    use yaxpeax_arm::armv8::a64::InstDecoder;

    let mut out = String::new();
    let mut bl_targets = Vec::new();
    let decoder = InstDecoder::default();
    let mut reader = U8Reader::new(code);
    let mut offset = 0usize;

    // Track x8 constant being loaded (for annotating blr x8 calls).
    let mut x8_val: Option<u64> = None;

    while offset + 4 <= code.len() {
        match decoder.decode(&mut reader) {
            Ok(inst) => {
                let addr = base_addr + offset as u64;
                let text = format!("{inst}");

                // Track constant loading into x8 for blr annotation.
                let annotation = if let Some(sym_note) = annotate_blr(&text, &mut x8_val, syms) {
                    format!("  ; {sym_note}")
                } else {
                    String::new()
                };

                // Collect bl targets for recursive disassembly.
                if text.starts_with("bl ") && !text.starts_with("blr") {
                    if let Some(target) = parse_bl_target(&text, addr) {
                        if !bl_targets.contains(&target) {
                            bl_targets.push(target);
                        }
                    }
                }

                writeln!(&mut out, "  {addr:#014x}  {text}{annotation}").unwrap();

                if stop_at_ret && text.trim() == "ret" {
                    break;
                }
                offset += 4;
            }
            Err(e) => {
                let word = u32::from_le_bytes(code[offset..offset + 4].try_into().unwrap());
                let addr = base_addr + offset as u64;
                writeln!(&mut out, "  {addr:#014x}  <{e}> ({word:#010x})").unwrap();
                x8_val = None;
                offset += 4;
            }
        }
    }

    (out, bl_targets)
}

/// Track `mov x8, #imm` / `movk x8, #imm, lsl #N` sequences and annotate
/// `blr x8` with the resolved symbol name or raw address.
#[cfg(target_arch = "aarch64")]
fn annotate_blr(
    text: &str,
    x8_val: &mut Option<u64>,
    syms: &HashMap<u64, &'static str>,
) -> Option<String> {
    // mov x8, #imm  (sets bits [15:0], zeroes rest)
    if let Some(rest) = text.strip_prefix("mov x8, #") {
        if let Ok(v) = parse_hex_or_dec(rest) {
            *x8_val = Some(v);
        } else {
            *x8_val = None;
        }
        return None;
    }
    // movk x8, #imm, lsl #N
    if let Some(rest) = text.strip_prefix("movk x8, #") {
        if let (Some(cur), Some((imm_str, shift_str))) =
            (x8_val.as_mut(), rest.split_once(", lsl #"))
        {
            if let (Ok(imm), Ok(shift)) = (
                parse_hex_or_dec(imm_str),
                shift_str.parse::<u64>(),
            ) {
                let mask = !(0xffffu64 << shift);
                *cur = (*cur & mask) | (imm << shift);
            } else {
                *x8_val = None;
            }
        } else {
            *x8_val = None;
        }
        return None;
    }
    // blr x8 — emit annotation
    if text.trim() == "blr x8" {
        if let Some(addr) = x8_val.take() {
            let name = syms.get(&addr).copied().unwrap_or("<unknown>");
            return Some(format!("{name} ({addr:#x})"));
        }
        *x8_val = None;
        return Some("<blr x8, x8 unknown>".to_string());
    }
    // Any other instruction that clobbers x8 clears our tracking.
    if text.contains("x8") && !text.starts_with("mov x8") && !text.starts_with("movk x8") {
        *x8_val = None;
    }
    None
}

#[cfg(target_arch = "aarch64")]
fn parse_hex_or_dec(s: &str) -> Result<u64, ()> {
    if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        u64::from_str_radix(hex, 16).map_err(|_| ())
    } else {
        s.parse::<u64>().map_err(|_| ())
    }
}

#[cfg(target_arch = "aarch64")]
fn parse_bl_target(text: &str, current_addr: u64) -> Option<u64> {
    // "bl $+0x..." or "bl $-0x..."
    let rest = text.strip_prefix("bl ")?;
    if let Some(hex) = rest.strip_prefix("$+0x") {
        let off = u64::from_str_radix(hex, 16).ok()?;
        Some(current_addr.wrapping_add(off))
    } else if let Some(hex) = rest.strip_prefix("$-0x") {
        let off = u64::from_str_radix(hex, 16).ok()?;
        Some(current_addr.wrapping_sub(off))
    } else {
        None
    }
}

// ── x86-64 ───────────────────────────────────────────────────────────────────

#[cfg(target_arch = "x86_64")]
fn disasm_bytes(
    code: &[u8],
    base_addr: u64,
    syms: &HashMap<u64, &'static str>,
    stop_at_ret: bool,
) -> (String, Vec<u64>) {
    use yaxpeax_arch::LengthedInstruction;
    use yaxpeax_x86::amd64::InstDecoder;

    let mut out = String::new();
    let mut call_targets = Vec::new();
    let decoder = InstDecoder::default();
    let mut reader = U8Reader::new(code);
    let mut offset = 0usize;
    let mut rax_val: Option<u64> = None;

    while offset < code.len() {
        match decoder.decode(&mut reader) {
            Ok(inst) => {
                let len = inst.len().to_const() as usize;
                let addr = base_addr + offset as u64;
                let text = format!("{inst}");

                let annotation = if text.trim() == "call rax" {
                    if let Some(a) = rax_val {
                        let name = syms.get(&a).copied().unwrap_or("<unknown>");
                        format!("  ; {name} ({a:#x})")
                    } else {
                        String::new()
                    }
                } else {
                    // Track `mov rax, QWORD imm64`
                    if let Some(rest) = text.strip_prefix("mov rax, ") {
                        if let Some(hex) = rest.strip_prefix("0x") {
                            rax_val = u64::from_str_radix(hex, 16).ok();
                        } else {
                            rax_val = None;
                        }
                    } else if text.contains("rax") {
                        rax_val = None;
                    }
                    String::new()
                };

                if text.starts_with("call ") && !text.contains("rax") {
                    if let Some(t) = parse_call_target(&text, addr) {
                        if !call_targets.contains(&t) {
                            call_targets.push(t);
                        }
                    }
                }
                writeln!(&mut out, "  {addr:#014x}  {text}{annotation}").unwrap();
                if stop_at_ret && text.trim() == "ret" {
                    break;
                }
                offset += len;
            }
            Err(_) => {
                let addr = base_addr + offset as u64;
                writeln!(&mut out, "  {addr:#014x}  <err> (0x{:02x})", code[offset]).unwrap();
                offset += 1;
            }
        }
    }
    (out, call_targets)
}

#[cfg(target_arch = "x86_64")]
fn parse_call_target(text: &str, current_addr: u64) -> Option<u64> {
    let rest = text.strip_prefix("call ")?;
    if let Some(hex) = rest.strip_prefix("$+0x") {
        Some(current_addr.wrapping_add(u64::from_str_radix(hex, 16).ok()?))
    } else if let Some(hex) = rest.strip_prefix("$-0x") {
        Some(current_addr.wrapping_sub(u64::from_str_radix(hex, 16).ok()?))
    } else {
        None
    }
}
