// Shared bench macro definitions for bench targets under benches/.

// bench!(v, name, Type, value)                — json + postcard, deser only
// bench!(v, name, Type, value, +ser)          — json + postcard, deser + ser
// bench!(v, name, Type, value, json_only)     — json only, deser only
// bench!(v, name, Type, value, postcard_legacy_ir)         — postcard legacy vs IR, deser only
// bench!(v, name, Type, value, postcard_legacy_ir_compile) — postcard legacy vs IR, deser + compile

macro_rules! bench {
    ($v:ident, $name:ident, $Type:ty, $value:expr) => {{
        let group = stringify!($name);
        bench!(@json $v, group, $Type, $value);
        bench!(@postcard $v, group, $Type, $value);
    }};

    ($v:ident, $name:ident, $Type:ty, $value:expr, +ser) => {{
        let group = stringify!($name);
        bench!(@json_ser $v, group, $Type, $value);
        bench!(@postcard_ser $v, group, $Type, $value);
    }};

    ($v:ident, $name:ident, $Type:ty, $value:expr, json_only) => {{
        let group = stringify!($name);
        bench!(@json $v, group, $Type, $value);
    }};

    ($v:ident, $name:ident, $Type:ty, $value:expr, postcard_legacy_ir) => {{
        let group = stringify!($name);
        bench!(@postcard_legacy_ir $v, group, $Type, $value, false);
    }};

    ($v:ident, $name:ident, $Type:ty, $value:expr, postcard_legacy_ir_compile) => {{
        let group = stringify!($name);
        bench!(@postcard_legacy_ir $v, group, $Type, $value, true);
    }};

    // ── JSON (deser only) ───────────────────────────────────────────────

    (@json $v:ident, $group:expr, $Type:ty, $value:expr) => {{
        static DATA: LazyLock<String> = LazyLock::new(|| {
            serde_json::to_string(&{ $value }).unwrap()
        });
        static DECODER: LazyLock<fad::compiler::CompiledDecoder> = LazyLock::new(|| {
            fad::compile_decoder(<$Type>::SHAPE, &fad::json::FadJson)
        });

        let prefix = format!("{}/json", $group);

        $v.push(harness::Bench {
            name: format!("{prefix}/serde_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                runner.run(|| { black_box(serde_json::from_str::<$Type>(black_box(data)).unwrap()); });
            }),
        });
        $v.push(harness::Bench {
            name: format!("{prefix}/fad_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*DECODER;
                runner.run(|| { black_box(fad::from_str::<$Type>(deser, black_box(data)).unwrap()); });
            }),
        });
    }};

    // ── JSON (deser + ser) ──────────────────────────────────────────────

    (@json_ser $v:ident, $group:expr, $Type:ty, $value:expr) => {{
        static DATA: LazyLock<String> = LazyLock::new(|| {
            serde_json::to_string(&{ $value }).unwrap()
        });
        static DECODER: LazyLock<fad::compiler::CompiledDecoder> = LazyLock::new(|| {
            fad::compile_decoder(<$Type>::SHAPE, &fad::json::FadJson)
        });
        static ENCODER: LazyLock<fad::compiler::CompiledEncoder> = LazyLock::new(|| {
            fad::compile_encoder(<$Type>::SHAPE, &fad::json::FadJsonEncoder)
        });

        let prefix = format!("{}/json", $group);

        $v.push(harness::Bench {
            name: format!("{prefix}/serde_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                runner.run(|| { black_box(serde_json::from_str::<$Type>(black_box(data)).unwrap()); });
            }),
        });
        $v.push(harness::Bench {
            name: format!("{prefix}/fad_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*DECODER;
                runner.run(|| { black_box(fad::from_str::<$Type>(deser, black_box(data)).unwrap()); });
            }),
        });
        $v.push(harness::Bench {
            name: format!("{prefix}/serde_ser"),
            func: Box::new(|runner| {
                let val: $Type = $value;
                runner.run(|| { black_box(serde_json::to_vec(black_box(&val)).unwrap()); });
            }),
        });
        $v.push(harness::Bench {
            name: format!("{prefix}/fad_ser"),
            func: Box::new(|runner| {
                let val: $Type = $value;
                let enc = &*ENCODER;
                runner.run(|| { black_box(fad::serialize(enc, black_box(&val))); });
            }),
        });
    }};

    // ── Postcard (deser only) ───────────────────────────────────────────

    (@postcard $v:ident, $group:expr, $Type:ty, $value:expr) => {{
        static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
            ::postcard::to_allocvec(&{ $value }).unwrap()
        });
        static DECODER: LazyLock<fad::compiler::CompiledDecoder> = LazyLock::new(|| {
            fad::compile_decoder(<$Type>::SHAPE, &fad::postcard::FadPostcard)
        });

        let prefix = format!("{}/postcard", $group);

        $v.push(harness::Bench {
            name: format!("{prefix}/serde_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                runner.run(|| { black_box(::postcard::from_bytes::<$Type>(black_box(data)).unwrap()); });
            }),
        });
        $v.push(harness::Bench {
            name: format!("{prefix}/fad_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*DECODER;
                runner.run(|| { black_box(fad::deserialize::<$Type>(deser, black_box(data)).unwrap()); });
            }),
        });
    }};

    // ── Postcard (deser + ser) ──────────────────────────────────────────

    (@postcard_ser $v:ident, $group:expr, $Type:ty, $value:expr) => {{
        static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
            ::postcard::to_allocvec(&{ $value }).unwrap()
        });
        static DECODER: LazyLock<fad::compiler::CompiledDecoder> = LazyLock::new(|| {
            fad::compile_decoder(<$Type>::SHAPE, &fad::postcard::FadPostcard)
        });
        static ENCODER: LazyLock<fad::compiler::CompiledEncoder> = LazyLock::new(|| {
            fad::compile_encoder(<$Type>::SHAPE, &fad::postcard::FadPostcard)
        });

        let prefix = format!("{}/postcard", $group);

        $v.push(harness::Bench {
            name: format!("{prefix}/serde_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                runner.run(|| { black_box(::postcard::from_bytes::<$Type>(black_box(data)).unwrap()); });
            }),
        });
        $v.push(harness::Bench {
            name: format!("{prefix}/fad_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*DECODER;
                runner.run(|| { black_box(fad::deserialize::<$Type>(deser, black_box(data)).unwrap()); });
            }),
        });
        $v.push(harness::Bench {
            name: format!("{prefix}/serde_ser"),
            func: Box::new(|runner| {
                let val: $Type = $value;
                runner.run(|| { black_box(::postcard::to_allocvec(black_box(&val)).unwrap()); });
            }),
        });
        $v.push(harness::Bench {
            name: format!("{prefix}/fad_ser"),
            func: Box::new(|runner| {
                let val: $Type = $value;
                let enc = &*ENCODER;
                runner.run(|| { black_box(fad::serialize(enc, black_box(&val))); });
            }),
        });
    }};

    // ── Postcard legacy vs IR (deser, optional compile) ──────────────────

    (@postcard_legacy_ir $v:ident, $group:expr, $Type:ty, $value:expr, $with_compile:expr) => {{
        static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
            ::postcard::to_allocvec(&{ $value }).unwrap()
        });
        static LEGACY_DECODER: LazyLock<fad::compiler::CompiledDecoder> = LazyLock::new(|| {
            fad::compile_decoder_legacy(<$Type>::SHAPE, &fad::postcard::FadPostcard)
        });
        static IR_DECODER: LazyLock<fad::compiler::CompiledDecoder> = LazyLock::new(|| {
            fad::compile_decoder_via_ir(<$Type>::SHAPE, &fad::postcard::FadPostcard)
        });

        let prefix = format!("{}/postcard", $group);

        $v.push(harness::Bench {
            name: format!("{prefix}/legacy_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*LEGACY_DECODER;
                runner.run(|| {
                    black_box(fad::deserialize::<$Type>(deser, black_box(data)).unwrap());
                });
            }),
        });

        $v.push(harness::Bench {
            name: format!("{prefix}/ir_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*IR_DECODER;
                runner.run(|| {
                    black_box(fad::deserialize::<$Type>(deser, black_box(data)).unwrap());
                });
            }),
        });

        if $with_compile {
            $v.push(harness::Bench {
                name: format!("{prefix}/legacy_compile"),
                func: Box::new(|runner| {
                    runner.run(|| {
                        black_box(fad::compile_decoder_legacy(
                            <$Type>::SHAPE,
                            &fad::postcard::FadPostcard,
                        ));
                    });
                }),
            });

            $v.push(harness::Bench {
                name: format!("{prefix}/ir_compile"),
                func: Box::new(|runner| {
                    runner.run(|| {
                        black_box(fad::compile_decoder_via_ir(
                            <$Type>::SHAPE,
                            &fad::postcard::FadPostcard,
                        ));
                    });
                }),
            });
        }
    }};
}
