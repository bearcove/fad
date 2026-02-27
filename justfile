image_name := "fad-test-x86_64"

# Build the x86_64 Docker test image
docker-build:
    docker build -t {{image_name}} .

# Run tests in x86_64 Docker container
docker-test: docker-build
    docker run --rm -v {{justfile_directory()}}:/workspace -w /workspace {{image_name}} cargo nextest run

# Run a shell in the x86_64 Docker container
docker-shell: docker-build
    docker run --rm -it -v {{justfile_directory()}}:/workspace -w /workspace {{image_name}} bash

# Run benchmarks in x86_64 Docker container
docker-bench: docker-build
    docker run --rm -v {{justfile_directory()}}:/workspace -w /workspace {{image_name}} cargo bench

# Run tests natively
test:
    cargo nextest run

# Run benchmarks natively
bench:
    cargo bench

# Check + clippy
check:
    cargo check
    cargo clippy -- -D warnings
