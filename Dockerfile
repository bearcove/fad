FROM rust:latest

RUN apt-get update -qq \
    && apt-get install -y -qq cmake >/dev/null 2>&1 \
    && rm -rf /var/lib/apt/lists/*

RUN cargo install cargo-nextest --locked \
    && cargo install cargo-insta --locked
