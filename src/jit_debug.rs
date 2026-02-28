//! GDB JIT Interface — registers JIT-compiled code with debuggers.
//!
//! Generates a minimal in-memory ELF with symbol table entries so that GDB/LLDB
//! can display function names in backtraces instead of `???`.
//!
//! Reference: <https://sourceware.org/gdb/current/onlinedocs/gdb.html/JIT-Interface.html>

use std::io::Write;
use std::sync::Mutex;

// ---------------------------------------------------------------------------
// GDB JIT interface types
// ---------------------------------------------------------------------------

const JIT_NOACTION: u32 = 0;
const JIT_REGISTER_FN: u32 = 1;
const JIT_UNREGISTER_FN: u32 = 2;

#[repr(C)]
struct JitCodeEntry {
    next: *mut JitCodeEntry,
    prev: *mut JitCodeEntry,
    symfile_addr: *const u8,
    symfile_size: u64,
}

#[repr(C)]
struct JitDescriptor {
    version: u32,
    action_flag: u32,
    relevant_entry: *mut JitCodeEntry,
    first_entry: *mut JitCodeEntry,
}

// SAFETY: The linked list is protected by DESCRIPTOR_LOCK.
unsafe impl Send for JitDescriptor {}
unsafe impl Sync for JitDescriptor {}

#[unsafe(no_mangle)]
static mut __jit_debug_descriptor: JitDescriptor = JitDescriptor {
    version: 1,
    action_flag: JIT_NOACTION,
    relevant_entry: std::ptr::null_mut(),
    first_entry: std::ptr::null_mut(),
};

#[unsafe(no_mangle)]
#[inline(never)]
extern "C" fn __jit_debug_register_code() {
    // GDB sets a breakpoint here. The body must not be optimized away.
    unsafe { std::ptr::read_volatile(&0u8) };
}

static DESCRIPTOR_LOCK: Mutex<()> = Mutex::new(());

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// A symbol entry for the JIT symbol table.
pub struct JitSymbolEntry {
    pub name: String,
    pub offset: usize,
    pub size: usize,
}

/// Owns the GDB JIT registration. Unregisters on drop.
pub struct JitRegistration {
    entry: *mut JitCodeEntry,
    _elf: Vec<u8>,
}

// SAFETY: The JitCodeEntry is heap-allocated and only accessed under DESCRIPTOR_LOCK.
unsafe impl Send for JitRegistration {}
unsafe impl Sync for JitRegistration {}

impl Drop for JitRegistration {
    fn drop(&mut self) {
        let _lock = DESCRIPTOR_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        unsafe {
            let entry = &mut *self.entry;

            // Unlink from the doubly-linked list.
            if !entry.prev.is_null() {
                (*entry.prev).next = entry.next;
            } else {
                __jit_debug_descriptor.first_entry = entry.next;
            }
            if !entry.next.is_null() {
                (*entry.next).prev = entry.prev;
            }

            __jit_debug_descriptor.action_flag = JIT_UNREGISTER_FN;
            __jit_debug_descriptor.relevant_entry = self.entry;
            __jit_debug_register_code();

            // Free the entry.
            drop(Box::from_raw(self.entry));
        }
    }
}

/// Register JIT-compiled code with the debugger.
///
/// `buf_base` is the start of the executable buffer, `buf_len` its length.
/// `symbols` contains (name, offset, size) for each function in the buffer.
///
/// Returns a `JitRegistration` that keeps the registration alive.
pub fn register_jit_code(
    buf_base: *const u8,
    buf_len: usize,
    symbols: &[JitSymbolEntry],
) -> JitRegistration {
    let elf = build_elf(buf_base as u64, buf_len, symbols);

    let entry = Box::into_raw(Box::new(JitCodeEntry {
        next: std::ptr::null_mut(),
        prev: std::ptr::null_mut(),
        symfile_addr: elf.as_ptr(),
        symfile_size: elf.len() as u64,
    }));

    let _lock = DESCRIPTOR_LOCK.lock().unwrap_or_else(|e| e.into_inner());
    unsafe {
        // Prepend to linked list.
        let old_first = __jit_debug_descriptor.first_entry;
        (*entry).next = old_first;
        if !old_first.is_null() {
            (*old_first).prev = entry;
        }
        __jit_debug_descriptor.first_entry = entry;
        __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;
        __jit_debug_descriptor.relevant_entry = entry;
        __jit_debug_register_code();
    }

    write_perf_map(buf_base, symbols);

    JitRegistration { entry, _elf: elf }
}

// ---------------------------------------------------------------------------
// perf map file — /tmp/perf-<pid>.map
// ---------------------------------------------------------------------------

fn write_perf_map(buf_base: *const u8, symbols: &[JitSymbolEntry]) {
    let path = format!("/tmp/perf-{}.map", std::process::id());
    let Ok(mut f) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
    else {
        return;
    };
    for sym in symbols {
        let addr = buf_base as usize + sym.offset;
        let _ = writeln!(f, "{addr:x} {:x} {}", sym.size, sym.name);
    }
}

// ---------------------------------------------------------------------------
// Minimal ELF64 builder
// ---------------------------------------------------------------------------

// ELF constants
const ELFMAG: [u8; 4] = [0x7f, b'E', b'L', b'F'];
const ELFCLASS64: u8 = 2;
const ELFDATA2LSB: u8 = 1;
const EV_CURRENT: u8 = 1;
const ET_EXEC: u16 = 2;
const SHT_NULL: u32 = 0;
const SHT_PROGBITS: u32 = 1;
const SHT_SYMTAB: u32 = 2;
const SHT_STRTAB: u32 = 3;
const SHF_ALLOC: u64 = 0x2;
const SHF_EXECINSTR: u64 = 0x4;
const STB_GLOBAL: u8 = 1;
const STT_FUNC: u8 = 2;

const EHDR_SIZE: usize = 64;
const SHDR_SIZE: usize = 64;
const SYM_SIZE: usize = 24;
const NUM_SECTIONS: usize = 5; // null, .text, .symtab, .strtab, .shstrtab

#[cfg(target_arch = "x86_64")]
const EM_MACHINE: u16 = 0x3E; // EM_X86_64

#[cfg(target_arch = "aarch64")]
const EM_MACHINE: u16 = 0xB7; // EM_AARCH64

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
const EM_MACHINE: u16 = 0;

fn build_elf(text_addr: u64, text_len: usize, symbols: &[JitSymbolEntry]) -> Vec<u8> {
    // Build .strtab (symbol name strings)
    let mut strtab = vec![0u8]; // index 0 = empty string
    let mut name_offsets = Vec::with_capacity(symbols.len());
    for sym in symbols {
        name_offsets.push(strtab.len() as u32);
        strtab.extend_from_slice(sym.name.as_bytes());
        strtab.push(0);
    }

    // Build .shstrtab (section name strings)
    let mut shstrtab = vec![0u8];
    let sh_name_null = 0u32;
    let sh_name_text = shstrtab.len() as u32;
    shstrtab.extend_from_slice(b".text\0");
    let sh_name_symtab = shstrtab.len() as u32;
    shstrtab.extend_from_slice(b".symtab\0");
    let sh_name_strtab = shstrtab.len() as u32;
    shstrtab.extend_from_slice(b".strtab\0");
    let sh_name_shstrtab = shstrtab.len() as u32;
    shstrtab.extend_from_slice(b".shstrtab\0");

    // Build .symtab
    // Entry 0: null symbol
    let num_syms = 1 + symbols.len();
    let symtab_size = num_syms * SYM_SIZE;
    let mut symtab = Vec::with_capacity(symtab_size);
    // Null symbol (24 zero bytes)
    symtab.extend_from_slice(&[0u8; SYM_SIZE]);
    for (i, sym) in symbols.iter().enumerate() {
        // st_name (u32)
        symtab.extend_from_slice(&name_offsets[i].to_le_bytes());
        // st_info (u8): binding=STB_GLOBAL, type=STT_FUNC
        symtab.push((STB_GLOBAL << 4) | STT_FUNC);
        // st_other (u8)
        symtab.push(0);
        // st_shndx (u16): section index 1 = .text
        symtab.extend_from_slice(&1u16.to_le_bytes());
        // st_value (u64): absolute address
        let addr = text_addr + sym.offset as u64;
        symtab.extend_from_slice(&addr.to_le_bytes());
        // st_size (u64)
        symtab.extend_from_slice(&(sym.size as u64).to_le_bytes());
    }

    // Layout: ELF header | section headers | .symtab | .strtab | .shstrtab
    let shdr_offset = EHDR_SIZE;
    let data_offset = EHDR_SIZE + NUM_SECTIONS * SHDR_SIZE;
    let symtab_off = data_offset;
    let strtab_off = symtab_off + symtab.len();
    let shstrtab_off = strtab_off + strtab.len();
    let total_size = shstrtab_off + shstrtab.len();

    let mut elf = Vec::with_capacity(total_size);

    // ----- ELF header (64 bytes) -----
    elf.extend_from_slice(&ELFMAG);           // e_ident[0..4]
    elf.push(ELFCLASS64);                     // e_ident[4]
    elf.push(ELFDATA2LSB);                    // e_ident[5]
    elf.push(EV_CURRENT);                     // e_ident[6]
    elf.extend_from_slice(&[0u8; 9]);         // e_ident[7..16] padding
    elf.extend_from_slice(&ET_EXEC.to_le_bytes());         // e_type
    elf.extend_from_slice(&EM_MACHINE.to_le_bytes());      // e_machine
    elf.extend_from_slice(&1u32.to_le_bytes());            // e_version
    elf.extend_from_slice(&0u64.to_le_bytes());            // e_entry
    elf.extend_from_slice(&0u64.to_le_bytes());            // e_phoff
    elf.extend_from_slice(&(shdr_offset as u64).to_le_bytes()); // e_shoff
    elf.extend_from_slice(&0u32.to_le_bytes());            // e_flags
    elf.extend_from_slice(&(EHDR_SIZE as u16).to_le_bytes());   // e_ehsize
    elf.extend_from_slice(&0u16.to_le_bytes());            // e_phentsize
    elf.extend_from_slice(&0u16.to_le_bytes());            // e_phnum
    elf.extend_from_slice(&(SHDR_SIZE as u16).to_le_bytes());   // e_shentsize
    elf.extend_from_slice(&(NUM_SECTIONS as u16).to_le_bytes()); // e_shnum
    elf.extend_from_slice(&4u16.to_le_bytes());            // e_shstrndx (index of .shstrtab)
    debug_assert_eq!(elf.len(), EHDR_SIZE);

    // ----- Section headers -----

    // [0] SHT_NULL
    write_shdr(&mut elf, sh_name_null, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0);

    // [1] .text — points at the JIT buffer in memory (no data in ELF)
    write_shdr(
        &mut elf,
        sh_name_text,
        SHT_PROGBITS,
        SHF_ALLOC | SHF_EXECINSTR,
        text_addr,
        0,                // sh_offset: no data in file
        text_len as u64,
        0,
        0,
        16,
        0,
    );

    // [2] .symtab
    write_shdr(
        &mut elf,
        sh_name_symtab,
        SHT_SYMTAB,
        0,
        0,
        symtab_off as u64,
        symtab.len() as u64,
        3,              // sh_link = .strtab section index
        1,              // sh_info = index of first non-local symbol
        8,
        SYM_SIZE as u64,
    );

    // [3] .strtab
    write_shdr(
        &mut elf,
        sh_name_strtab,
        SHT_STRTAB,
        0,
        0,
        strtab_off as u64,
        strtab.len() as u64,
        0,
        0,
        1,
        0,
    );

    // [4] .shstrtab
    write_shdr(
        &mut elf,
        sh_name_shstrtab,
        SHT_STRTAB,
        0,
        0,
        shstrtab_off as u64,
        shstrtab.len() as u64,
        0,
        0,
        1,
        0,
    );

    debug_assert_eq!(elf.len(), data_offset);

    // ----- Section data -----
    elf.extend_from_slice(&symtab);
    elf.extend_from_slice(&strtab);
    elf.extend_from_slice(&shstrtab);

    debug_assert_eq!(elf.len(), total_size);
    elf
}

#[allow(clippy::too_many_arguments)]
fn write_shdr(
    buf: &mut Vec<u8>,
    sh_name: u32,
    sh_type: u32,
    sh_flags: u64,
    sh_addr: u64,
    sh_offset: u64,
    sh_size: u64,
    sh_link: u32,
    sh_info: u32,
    sh_addralign: u64,
    sh_entsize: u64,
) {
    buf.extend_from_slice(&sh_name.to_le_bytes());
    buf.extend_from_slice(&sh_type.to_le_bytes());
    buf.extend_from_slice(&sh_flags.to_le_bytes());
    buf.extend_from_slice(&sh_addr.to_le_bytes());
    buf.extend_from_slice(&sh_offset.to_le_bytes());
    buf.extend_from_slice(&sh_size.to_le_bytes());
    buf.extend_from_slice(&sh_link.to_le_bytes());
    buf.extend_from_slice(&sh_info.to_le_bytes());
    buf.extend_from_slice(&sh_addralign.to_le_bytes());
    buf.extend_from_slice(&sh_entsize.to_le_bytes());
}
