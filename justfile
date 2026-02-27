# cf. https://github.com/casey/just

default: list

list:
    just --list

# Run all checks (compile + test)
check:
    cargo check
    cargo nextest run

# Run tests on the host architecture
test *args:
    cargo nextest run {{ args }}

# Run clippy
clippy:
    cargo clippy --all-targets -- -D warnings

# Run tests on x86_64 via Docker (for use on aarch64 macOS)
test-x86_64:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "Building and testing on linux/amd64..."
    docker run --rm \
        --platform linux/amd64 \
        -v "$(pwd)":/workspace \
        -v "$(pwd)/target/x86_64-docker":/workspace/target \
        -w /workspace \
        rust:latest \
        cargo test
    echo "x86_64 tests passed."

# Run tests on both architectures (host + x86_64 via Docker)
test-all: test test-x86_64

# Run benchmarks
bench-postcard:
    cargo bench --bench deser_postcard

bench-json:
    cargo bench --bench deser_json

bench: bench-postcard bench-json
