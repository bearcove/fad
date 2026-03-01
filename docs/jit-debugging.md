# JIT debugging with LLDB (macOS)

This project registers generated code with the GDB JIT interface so LLDB can
resolve JIT symbol names.

## Prerequisite

Enable LLDB's GDB JIT loader (off by default on macOS):

```text
settings set plugin.jit-loader.gdb.enable on
```

## Minimal workflow

1. Start LLDB on the test binary:

```text
lldb target/debug/deps/fad-<hash>
```

2. Set a breakpoint on JIT registration:

```text
breakpoint set -n __jit_debug_register_code
```

3. Run the test:

```text
run --exact ir_backend::tests::linear_backend_vec_u32_matches_legacy_and_serde --nocapture
```

4. Confirm a JIT image is loaded:

```text
image list
```

You should see an entry like `JIT(0x...)`.

5. Resolve symbols explicitly (recommended even if backtrace is present):

```text
image lookup -a $pc
image lookup -rn 'fad::decode::'
image lookup -rn 'fad::encode::'
```

## Why explicit lookup matters

Even with JIT symbols registered, `thread backtrace` may still show a raw PC at
the top JIT frame. This repo currently emits a minimal JIT ELF (`.text` +
symbol table) and not full DWARF/unwind metadata, so explicit `image lookup`
is the most reliable way to resolve JIT PCs to symbol names.

