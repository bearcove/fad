// Shared bench macro definitions for bench targets under benches/.

// bench!(v, name, Type, value)                — json + postcard, deser only
// bench!(v, name, Type, value, +ser)          — json + postcard, deser + ser
// bench!(v, name, Type, value, +ir)           — json deser + postcard serde/kajit_dynasm/kajit_ir deser
// bench!(v, name, Type, value, +ser, +ir)     — json deser+ser + postcard serde/kajit_dynasm/kajit_ir deser + serde/kajit_dynasm ser
// bench!(v, name, Type, value, json_only)     — json only, deser only
// bench!(v, name, Type, value, postcard_legacy_ir)         — postcard serde + kajit_dynasm + kajit_ir, deser only
// bench!(v, name, Type, value, postcard_legacy_ir_compile) — postcard kajit_dynasm vs kajit_ir, deser + compile

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

    ($v:ident, $name:ident, $Type:ty, $value:expr, +ir) => {{
        let group = stringify!($name);
        bench!(@json $v, group, $Type, $value);
        bench!(@postcard_legacy_ir $v, group, $Type, $value, false);
    }};

    ($v:ident, $name:ident, $Type:ty, $value:expr, +ser, +ir) => {{
        let group = stringify!($name);
        bench!(@json_ser $v, group, $Type, $value);
        bench!(@postcard_ser_ir $v, group, $Type, $value);
    }};

    ($v:ident, $name:ident, $Type:ty, $value:expr, +ir, +ser) => {{
        let group = stringify!($name);
        bench!(@json_ser $v, group, $Type, $value);
        bench!(@postcard_ser_ir $v, group, $Type, $value);
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
        static DECODER: LazyLock<kajit::compiler::CompiledDecoder> = LazyLock::new(|| {
            kajit::compile_decoder_legacy(<$Type>::SHAPE, &kajit::json::KajitJson)
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
            name: format!("{prefix}/kajit_dynasm_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*DECODER;
                runner.run(|| { black_box(kajit::from_str::<$Type>(deser, black_box(data)).unwrap()); });
            }),
        });
    }};

    // ── JSON (deser + ser) ──────────────────────────────────────────────

    (@json_ser $v:ident, $group:expr, $Type:ty, $value:expr) => {{
        static DATA: LazyLock<String> = LazyLock::new(|| {
            serde_json::to_string(&{ $value }).unwrap()
        });
        static DECODER: LazyLock<kajit::compiler::CompiledDecoder> = LazyLock::new(|| {
            kajit::compile_decoder_legacy(<$Type>::SHAPE, &kajit::json::KajitJson)
        });
        static ENCODER: LazyLock<kajit::compiler::CompiledEncoder> = LazyLock::new(|| {
            kajit::compile_encoder(<$Type>::SHAPE, &kajit::json::KajitJsonEncoder)
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
            name: format!("{prefix}/kajit_dynasm_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*DECODER;
                runner.run(|| { black_box(kajit::from_str::<$Type>(deser, black_box(data)).unwrap()); });
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
            name: format!("{prefix}/kajit_dynasm_ser"),
            func: Box::new(|runner| {
                let val: $Type = $value;
                let enc = &*ENCODER;
                runner.run(|| { black_box(kajit::serialize(enc, black_box(&val))); });
            }),
        });
    }};

    // ── Postcard (deser only) ───────────────────────────────────────────

    (@postcard $v:ident, $group:expr, $Type:ty, $value:expr) => {{
        static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
            ::postcard::to_allocvec(&{ $value }).unwrap()
        });
        static DECODER: LazyLock<kajit::compiler::CompiledDecoder> = LazyLock::new(|| {
            kajit::compile_decoder_legacy(<$Type>::SHAPE, &kajit::postcard::KajitPostcard)
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
            name: format!("{prefix}/kajit_dynasm_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*DECODER;
                runner.run(|| { black_box(kajit::deserialize::<$Type>(deser, black_box(data)).unwrap()); });
            }),
        });
    }};

    // ── Postcard (deser + ser) ──────────────────────────────────────────

    (@postcard_ser $v:ident, $group:expr, $Type:ty, $value:expr) => {{
        static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
            ::postcard::to_allocvec(&{ $value }).unwrap()
        });
        static DECODER: LazyLock<kajit::compiler::CompiledDecoder> = LazyLock::new(|| {
            kajit::compile_decoder_legacy(<$Type>::SHAPE, &kajit::postcard::KajitPostcard)
        });
        static ENCODER: LazyLock<kajit::compiler::CompiledEncoder> = LazyLock::new(|| {
            kajit::compile_encoder(<$Type>::SHAPE, &kajit::postcard::KajitPostcard)
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
            name: format!("{prefix}/kajit_dynasm_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*DECODER;
                runner.run(|| { black_box(kajit::deserialize::<$Type>(deser, black_box(data)).unwrap()); });
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
            name: format!("{prefix}/kajit_dynasm_ser"),
            func: Box::new(|runner| {
                let val: $Type = $value;
                let enc = &*ENCODER;
                runner.run(|| { black_box(kajit::serialize(enc, black_box(&val))); });
            }),
        });
    }};

    // ── Postcard (deser + ser, with kajit_dynasm + kajit_ir side-by-side) ───

    (@postcard_ser_ir $v:ident, $group:expr, $Type:ty, $value:expr) => {{
        static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
            ::postcard::to_allocvec(&{ $value }).unwrap()
        });
        static LEGACY_DECODER: LazyLock<kajit::compiler::CompiledDecoder> = LazyLock::new(|| {
            kajit::compile_decoder_legacy(<$Type>::SHAPE, &kajit::postcard::KajitPostcard)
        });
        static IR_DECODER: LazyLock<kajit::compiler::CompiledDecoder> = LazyLock::new(|| {
            kajit::compile_decoder_via_ir(<$Type>::SHAPE, &kajit::postcard::KajitPostcard)
        });
        static ENCODER: LazyLock<kajit::compiler::CompiledEncoder> = LazyLock::new(|| {
            kajit::compile_encoder(<$Type>::SHAPE, &kajit::postcard::KajitPostcard)
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
            name: format!("{prefix}/kajit_dynasm_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*LEGACY_DECODER;
                runner.run(|| { black_box(kajit::deserialize::<$Type>(deser, black_box(data)).unwrap()); });
            }),
        });
        $v.push(harness::Bench {
            name: format!("{prefix}/kajit_ir_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*IR_DECODER;
                runner.run(|| { black_box(kajit::deserialize::<$Type>(deser, black_box(data)).unwrap()); });
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
            name: format!("{prefix}/kajit_dynasm_ser"),
            func: Box::new(|runner| {
                let val: $Type = $value;
                let enc = &*ENCODER;
                runner.run(|| { black_box(kajit::serialize(enc, black_box(&val))); });
            }),
        });
    }};

    // ── Postcard serde + kajit_dynasm vs kajit_ir (deser, optional compile) ──

    (@postcard_legacy_ir $v:ident, $group:expr, $Type:ty, $value:expr, $with_compile:expr) => {{
        static DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
            ::postcard::to_allocvec(&{ $value }).unwrap()
        });
        static LEGACY_DECODER: LazyLock<kajit::compiler::CompiledDecoder> = LazyLock::new(|| {
            kajit::compile_decoder_legacy(<$Type>::SHAPE, &kajit::postcard::KajitPostcard)
        });
        static IR_DECODER: LazyLock<kajit::compiler::CompiledDecoder> = LazyLock::new(|| {
            kajit::compile_decoder_via_ir(<$Type>::SHAPE, &kajit::postcard::KajitPostcard)
        });

        let prefix = format!("{}/postcard", $group);

        $v.push(harness::Bench {
            name: format!("{prefix}/serde_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                runner.run(|| {
                    black_box(::postcard::from_bytes::<$Type>(black_box(data)).unwrap());
                });
            }),
        });

        $v.push(harness::Bench {
            name: format!("{prefix}/kajit_dynasm_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*LEGACY_DECODER;
                runner.run(|| {
                    black_box(kajit::deserialize::<$Type>(deser, black_box(data)).unwrap());
                });
            }),
        });

        $v.push(harness::Bench {
            name: format!("{prefix}/kajit_ir_deser"),
            func: Box::new(|runner| {
                let data = &*DATA;
                let deser = &*IR_DECODER;
                runner.run(|| {
                    black_box(kajit::deserialize::<$Type>(deser, black_box(data)).unwrap());
                });
            }),
        });

        if $with_compile {
            $v.push(harness::Bench {
                name: format!("{prefix}/kajit_dynasm_compile"),
                func: Box::new(|runner| {
                    runner.run(|| {
                        black_box(kajit::compile_decoder_legacy(
                            <$Type>::SHAPE,
                            &kajit::postcard::KajitPostcard,
                        ));
                    });
                }),
            });

            $v.push(harness::Bench {
                name: format!("{prefix}/kajit_ir_compile"),
                func: Box::new(|runner| {
                    runner.run(|| {
                        black_box(kajit::compile_decoder_via_ir(
                            <$Type>::SHAPE,
                            &kajit::postcard::KajitPostcard,
                        ));
                    });
                }),
            });
        }
    }};
}
