use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy)]
enum BenchMode {
    Default,
    Ser,
    Ir,
    SerIr,
}

struct Case {
    name: &'static str,
    ty: &'static str,
    value: &'static str,
    mode: BenchMode,
    json_test: bool,
    postcard_test: bool,
    postcard_ir_test: bool,
}

struct IrOptCase {
    name: &'static str,
    ir: &'static str,
    must_not_contain_after: &'static [&'static str],
    must_contain_after: &'static [&'static str],
    input: &'static [u8],
}

struct IrPostRegCase {
    name: &'static str,
    ir: &'static str,
    must_contain_linear: &'static [&'static str],
    max_total_edits: usize,
    input: &'static [u8],
    expected: u8,
}

const IR_POSTREG_CASES: &[IrPostRegCase] = &[
    IrPostRegCase {
        name: "cmpne_gamma_branch",
        ir: r#"
lambda @0 (shape: "u8") {
  region {
    args: [%cs, %os]
    n0 = BoundsCheck(1) [%cs:arg] -> [%cs]
    n1 = ReadBytes(1) [%cs:n0] -> [v0, %cs]
    n2 = Const(0x0) [] -> [v1]
    n3 = CmpNe [v0, v1] -> [v2]
    n4 = gamma [
      pred: v2
      in0: %cs:n1
      in1: %os:arg
    ] {
      branch 0:
        region {
          args: [%cs, %os]
          n5 = Const(0x3) [] -> [v3]
          results: [v3, %cs:arg, %os:arg]
        }
      branch 1:
        region {
          args: [%cs, %os]
          n6 = Const(0x7) [] -> [v4]
          results: [v4, %cs:arg, %os:arg]
        }
    } -> [v5, %cs, %os]
    n7 = WriteToField(offset=0, W1) [v5, %os:n4] -> [%os]
    results: [%cs:n4, %os:n7]
  }
}
"#,
        must_contain_linear: &["CmpNe", "br_zero"],
        max_total_edits: 64,
        input: &[1],
        expected: 7,
    },
    IrPostRegCase {
        name: "and_cmpne_gamma_branch",
        ir: r#"
lambda @0 (shape: "u8") {
  region {
    args: [%cs, %os]
    n0 = BoundsCheck(2) [%cs:arg] -> [%cs]
    n1 = ReadBytes(1) [%cs:n0] -> [v0, %cs]
    n2 = ReadBytes(1) [%cs:n1] -> [v1, %cs]
    n3 = Const(0x0) [] -> [v2]
    n4 = CmpNe [v0, v2] -> [v3]
    n5 = CmpNe [v1, v2] -> [v4]
    n6 = And [v3, v4] -> [v5]
    n7 = gamma [
      pred: v5
      in0: %cs:n2
      in1: %os:arg
    ] {
      branch 0:
        region {
          args: [%cs, %os]
          n8 = Const(0x4) [] -> [v6]
          results: [v6, %cs:arg, %os:arg]
        }
      branch 1:
        region {
          args: [%cs, %os]
          n9 = Const(0x9) [] -> [v7]
          results: [v7, %cs:arg, %os:arg]
        }
    } -> [v8, %cs, %os]
    n10 = WriteToField(offset=0, W1) [v8, %os:n7] -> [%os]
    results: [%cs:n7, %os:n10]
  }
}
"#,
        must_contain_linear: &["And", "CmpNe", "br_zero"],
        max_total_edits: 96,
        input: &[1, 1],
        expected: 9,
    },
    IrPostRegCase {
        name: "theta_then_gamma_edit_budget",
        ir: r#"
lambda @0 (shape: "u8") {
  region {
    args: [%cs, %os]
    n0 = Const(0x3) [] -> [v0]
    n1 = Const(0x1) [] -> [v1]
    n2 = theta [v0, v1, %cs:arg, %os:arg] {
      region {
        args: [arg0, arg1, %cs, %os]
        n3 = Sub [arg0, arg1] -> [v2]
        results: [v2, v2, arg1, %cs:arg, %os:arg]
      }
    } -> [v3, v4, %cs, %os]
    n4 = gamma [
      pred: v3
      in0: %cs:n2
      in1: %os:n2
    ] {
      branch 0:
        region {
          args: [%cs, %os]
          n5 = Const(0xb) [] -> [v5]
          results: [v5, %cs:arg, %os:arg]
        }
      branch 1:
        region {
          args: [%cs, %os]
          n6 = Const(0x16) [] -> [v6]
          results: [v6, %cs:arg, %os:arg]
        }
    } -> [v7, %cs, %os]
    n8 = WriteToField(offset=0, W1) [v7, %os:n4] -> [%os]
    results: [%cs:n4, %os:n8]
  }
}
"#,
        must_contain_linear: &["br_if", "br_zero"],
        max_total_edits: 128,
        input: &[],
        expected: 11,
    },
];

const IR_OPT_CASES: &[IrOptCase] = &[IrOptCase {
    name: "theta_invariant_tree_hoist",
    ir: r#"
lambda @0 (shape: "u8") {
  region {
    args: [%cs, %os]
    n0 = Const(0x4) [] -> [v0]
    n1 = Const(0x1) [] -> [v1]
    n2 = theta [v0, v1, %cs:arg, %os:arg] {
      region {
        args: [arg0, arg1, %cs, %os]
        n3 = Const(0x7) [] -> [v2]
        n4 = Const(0x3) [] -> [v3]
        n5 = Add [v2, v3] -> [v4]
        n6 = Xor [v4, v3] -> [v5]
        n7 = Sub [arg0, arg1] -> [v6]
        n8 = Add [v5, v6] -> [v7]
        results: [v6, v6, arg1, %cs:arg, %os:arg]
      }
    } -> [v8, v9, %cs, %os]
    n9 = WriteToField(offset=0, W1) [v8, %os:n2] -> [%os]
    results: [%cs:n2, %os:n9]
  }
}
"#,
    must_not_contain_after: &[
        "n3 = Const(0x7) [] -> [v2]",
        "n4 = Const(0x3) [] -> [v3]",
        "n5 = Add [v2, v3] -> [v4]",
        "n6 = Xor [v4, v3] -> [v5]",
    ],
    must_contain_after: &[],
    input: &[],
},
IrOptCase {
    name: "theta_loop_variant_not_hoisted",
    ir: r#"
lambda @0 (shape: "u8") {
  region {
    args: [%cs, %os]
    n0 = Const(0x4) [] -> [v0]
    n1 = Const(0x1) [] -> [v1]
    n2 = theta [v0, v1, %cs:arg, %os:arg] {
      region {
        args: [arg0, arg1, %cs, %os]
        n3 = Add [arg0, arg1] -> [v2]
        n4 = Sub [arg0, arg1] -> [v3]
        results: [v3, v3, arg1, %cs:arg, %os:arg]
      }
    } -> [v4, v5, %cs, %os]
    n5 = WriteToField(offset=0, W1) [v4, %os:n2] -> [%os]
    results: [%cs:n2, %os:n5]
  }
}
"#,
    must_not_contain_after: &[],
    must_contain_after: &["n3 = Add [arg0, arg1] -> [v2]"],
    input: &[],
},
IrOptCase {
    name: "bounds_check_chain_coalesce",
    ir: r#"
lambda @0 (shape: "u8") {
  region {
    args: [%cs, %os]
    n0 = BoundsCheck(1) [%cs:arg] -> [%cs]
    n1 = PeekByte [%cs:n0] -> [v0, %cs]
    n2 = BoundsCheck(1) [%cs:n1] -> [%cs]
    n3 = ReadBytes(1) [%cs:n2] -> [v1, %cs]
    n4 = WriteToField(offset=0, W1) [v1, %os:arg] -> [%os]
    results: [%cs:n3, %os:n4]
  }
}
"#,
    must_not_contain_after: &["n2 = BoundsCheck(1) [%cs:n1] -> [%cs]"],
    must_contain_after: &["BoundsCheck(1) [%cs:arg] -> [%cs]"],
    input: &[7],
}];

const TYPES_RS: &str = r#"
#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Friend {
    age: u32,
    name: String,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Address {
    city: String,
    zip: u32,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Person {
    name: String,
    age: u32,
    address: Address,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Inner {
    x: u32,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Middle {
    inner: Inner,
    y: u32,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct Outer {
    middle: Middle,
    z: u32,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct AllIntegers {
    a_u8: u8,
    a_u16: u16,
    a_u32: u32,
    a_u64: u64,
    a_i8: i8,
    a_i16: i16,
    a_i32: i32,
    a_i64: i64,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct BoolField {
    value: bool,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Facet)]
struct ScalarVec {
    values: Vec<u32>,
}

type Pair = (u32, String);
"#;

const CASES: &[Case] = &[
    Case {
        name: "flat_struct",
        ty: "Friend",
        value: r#"Friend { age: 42, name: "Alice".into() }"#,
        mode: BenchMode::SerIr,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: true,
    },
    Case {
        name: "nested_struct",
        ty: "Person",
        value: r#"Person { name: "Alice".into(), age: 30, address: Address { city: "Portland".into(), zip: 97201 } }"#,
        mode: BenchMode::Ser,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: false,
    },
    Case {
        name: "deep_struct",
        ty: "Outer",
        value: r#"Outer { middle: Middle { inner: Inner { x: 1 }, y: 2 }, z: 3 }"#,
        mode: BenchMode::SerIr,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: true,
    },
    Case {
        name: "all_integers",
        ty: "AllIntegers",
        value: r#"AllIntegers { a_u8: 255, a_u16: 65535, a_u32: 1_000_000, a_u64: 1_000_000_000_000, a_i8: -128, a_i16: -32768, a_i32: -1_000_000, a_i64: -1_000_000_000_000 }"#,
        mode: BenchMode::Ser,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: false,
    },
    Case {
        name: "bool_field",
        ty: "BoolField",
        value: r#"BoolField { value: true }"#,
        mode: BenchMode::Ser,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: false,
    },
    Case {
        name: "tuple_pair",
        ty: "Pair",
        value: r#"(42u32, "Alice".to_string())"#,
        mode: BenchMode::Default,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: false,
    },
    Case {
        name: "vec_scalar_small",
        ty: "ScalarVec",
        value: r#"ScalarVec { values: (0..16).map(|i| i as u32).collect() }"#,
        mode: BenchMode::Ir,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: true,
    },
    Case {
        name: "vec_scalar_large",
        ty: "ScalarVec",
        value: r#"ScalarVec { values: (0..2048).map(|i| i as u32).collect() }"#,
        mode: BenchMode::Ir,
        json_test: true,
        postcard_test: true,
        postcard_ir_test: true,
    },
];

fn main() {
    let mut args = std::env::args();
    let _bin = args.next();
    match args.next().as_deref() {
        Some("generate-synthetic") => generate_synthetic(),
        Some("generate-ir-opt-corpus") => generate_ir_opt_corpus(),
        Some("generate-ir-postreg-corpus") => generate_ir_postreg_corpus(),
        _ => {
            eprintln!(
                "usage: cargo run --manifest-path xtask/Cargo.toml -- <generate-synthetic|generate-ir-opt-corpus|generate-ir-postreg-corpus>"
            );
            std::process::exit(2);
        }
    }
}

fn generate_synthetic() {
    let root = workspace_root();
    let bench_path = root.join("benches/synthetic.rs");
    let test_path = root.join("tests/generated_synthetic.rs");
    write_file(&bench_path, &render_bench_file());
    write_file(&test_path, &render_test_file());
    println!(
        "generated:\n- {}\n- {}",
        bench_path.display(),
        test_path.display()
    );
}

fn generate_ir_opt_corpus() {
    let root = workspace_root();
    let test_path = root.join("tests/generated_ir_opt_corpus.rs");
    write_file(&test_path, &render_ir_opt_test_file());
    println!("generated:\n- {}", test_path.display());
}

fn generate_ir_postreg_corpus() {
    let root = workspace_root();
    let test_path = root.join("tests/generated_ir_postreg_corpus.rs");
    write_file(&test_path, &render_ir_postreg_test_file());
    println!("generated:\n- {}", test_path.display());
}

fn render_bench_file() -> String {
    let mut out = String::new();
    out.push_str("// @generated by xtask generate-synthetic. Do not edit manually.\n");
    out.push_str("#[path = \"harness.rs\"]\nmod harness;\n\n");
    out.push_str("#[macro_use]\n#[path = \"common/bench_macros.rs\"]\nmod bench_macros;\n\n");
    out.push_str("use facet::Facet;\nuse std::hint::black_box;\nuse std::sync::LazyLock;\n\n");
    out.push_str(TYPES_RS.trim());
    out.push_str("\n\nfn main() {\n");
    out.push_str("    let mut v: Vec<harness::Bench> = Vec::new();\n\n");
    for case in CASES {
        write!(
            out,
            "    bench!(v, {}, {}, {}",
            case.name, case.ty, case.value
        )
        .unwrap();
        match case.mode {
            BenchMode::Default => {}
            BenchMode::Ser => out.push_str(", +ser"),
            BenchMode::Ir => out.push_str(", +ir"),
            BenchMode::SerIr => out.push_str(", +ser, +ir"),
        }
        out.push_str(");\n");
    }
    out.push_str("\n    harness::run_benchmarks(v);\n}\n");
    out
}

fn render_ir_postreg_test_file() -> String {
    let mut out = String::new();
    out.push_str("// @generated by xtask generate-ir-postreg-corpus. Do not edit manually.\n");
    out.push_str("use facet::Facet;\n\n");
    out.push_str("fn parse_case(ir: &str) -> kajit::ir::IrFunc {\n");
    out.push_str("    let registry = kajit::ir::IntrinsicRegistry::empty();\n");
    out.push_str("    kajit::ir_parse::parse_ir(ir, <u8 as Facet>::SHAPE, &registry)\n");
    out.push_str("        .expect(\"text IR should parse\")\n");
    out.push_str("}\n\n");
    out.push_str("fn run_exec(ir: &str, input: &[u8], with_passes: bool) -> u8 {\n");
    out.push_str("    let mut func = parse_case(ir);\n");
    out.push_str("    if with_passes {\n");
    out.push_str("        kajit::ir_passes::run_default_passes(&mut func);\n");
    out.push_str("    }\n");
    out.push_str("    let linear = kajit::linearize::linearize(&mut func);\n");
    out.push_str("    let dec = kajit::compile_decoder_linear_ir(&linear, false);\n");
    out.push_str("    kajit::deserialize::<u8>(&dec, input).expect(\"decoder should execute\")\n");
    out.push_str("}\n\n");
    out.push_str("fn postreg_artifacts(ir: &str) -> (String, String, usize) {\n");
    out.push_str("    let mut func = parse_case(ir);\n");
    out.push_str("    kajit::ir_passes::run_default_passes(&mut func);\n");
    out.push_str("    let linear = kajit::linearize::linearize(&mut func);\n");
    out.push_str("    let linear_text = format!(\"{linear}\");\n");
    out.push_str("    let ra = kajit::regalloc_mir::lower_linear_ir(&linear);\n");
    out.push_str("    let ra_text = format!(\"{ra}\");\n");
    out.push_str("    let alloc = kajit::regalloc_engine::allocate_program(&ra)\n");
    out.push_str("        .expect(\"regalloc should allocate post-reg corpus case\");\n");
    out.push_str("    let edits: usize = alloc.functions.iter().map(|f| f.edits.len()).sum();\n");
    out.push_str("    (linear_text, ra_text, edits)\n");
    out.push_str("}\n\n");

    for case in IR_POSTREG_CASES {
        let snap_test = format!("ir_postreg_snapshot_{}", case.name);
        let assert_test = format!("ir_postreg_asserts_{}", case.name);
        let exec_test = format!("ir_postreg_exec_{}", case.name);

        write!(
            out,
            "#[test]\nfn {snap_test}() {{\n    let (linear, ra, edits) = postreg_artifacts(r#\"{ir}\"#);\n    insta::assert_snapshot!(\"generated_ir_postreg_linear_{name}\", linear);\n    insta::assert_snapshot!(\"generated_ir_postreg_ra_{name}\", ra);\n    insta::assert_snapshot!(\"generated_ir_postreg_edits_{name}\", format!(\"{{edits}}\"));\n}}\n\n",
            snap_test = snap_test,
            ir = case.ir,
            name = case.name
        )
        .unwrap();

        write!(
            out,
            "#[test]\nfn {assert_test}() {{\n    let (linear, _ra, edits) = postreg_artifacts(r#\"{ir}\"#);\n    assert!(edits <= {max_edits}, \"expected edit budget <= {max_edits}, got {{edits}}\");\n",
            assert_test = assert_test,
            ir = case.ir,
            max_edits = case.max_total_edits
        )
        .unwrap();
        for needle in case.must_contain_linear {
            write!(
                out,
                "    assert!(linear.contains(r#\"{needle}\"#), \"expected linear artifact to contain: {{}}\", r#\"{needle}\"#);\n",
                needle = needle
            )
            .unwrap();
        }
        out.push_str("}\n\n");

        out.push_str(&format!(
            "#[test]\nfn {exec_test}() {{\n",
            exec_test = exec_test
        ));
        out.push_str("    let before = run_exec(r#\"");
        out.push_str(case.ir);
        out.push_str("\"#, &[");
        for (idx, b) in case.input.iter().enumerate() {
            if idx > 0 {
                out.push_str(", ");
            }
            out.push_str(&b.to_string());
        }
        out.push_str("], false);\n");
        out.push_str("    let after = run_exec(r#\"");
        out.push_str(case.ir);
        out.push_str("\"#, &[");
        for (idx, b) in case.input.iter().enumerate() {
            if idx > 0 {
                out.push_str(", ");
            }
            out.push_str(&b.to_string());
        }
        out.push_str("], true);\n");
        out.push_str(&format!(
            "    assert_eq!(after, {}u8, \"optimized output mismatch against expected\");\n",
            case.expected
        ));
        out.push_str("    assert_eq!(after, before, \"optimized and baseline outputs diverged\");\n");
        out.push_str("}\n\n");
    }

    out
}

fn render_ir_opt_test_file() -> String {
    let mut out = String::new();
    out.push_str("// @generated by xtask generate-ir-opt-corpus. Do not edit manually.\n");
    out.push_str("use facet::Facet;\n\n");
    out.push_str("fn parse_case(ir: &str) -> kajit::ir::IrFunc {\n");
    out.push_str("    let registry = kajit::ir::IntrinsicRegistry::empty();\n");
    out.push_str("    kajit::ir_parse::parse_ir(ir, <u8 as Facet>::SHAPE, &registry)\n");
    out.push_str("        .expect(\"text IR should parse\")\n");
    out.push_str("}\n\n");
    out.push_str("fn run_pass(ir: &str) -> (String, String) {\n");
    out.push_str("    let mut func = parse_case(ir);\n");
    out.push_str("    let before = format!(\"{func}\");\n");
    out.push_str("    kajit::ir_passes::run_default_passes(&mut func);\n");
    out.push_str("    let after = format!(\"{func}\");\n");
    out.push_str("    (before, after)\n");
    out.push_str("}\n\n");
    out.push_str("fn run_exec(ir: &str, input: &[u8]) -> u8 {\n");
    out.push_str("    let mut func = parse_case(ir);\n");
    out.push_str("    let linear = kajit::linearize::linearize(&mut func);\n");
    out.push_str("    let dec = kajit::compile_decoder_linear_ir(&linear, false);\n");
    out.push_str("    kajit::deserialize::<u8>(&dec, input).expect(\"decoder should execute\")\n");
    out.push_str("}\n\n");

    for case in IR_OPT_CASES {
        let test_name_snapshot = format!("ir_opt_snapshot_{}", case.name);
        let test_name_asserts = format!("ir_opt_asserts_{}", case.name);
        let test_name_exec = format!("ir_opt_exec_{}", case.name);

        write!(
            out,
            "#[test]\nfn {test_name_snapshot}() {{\n    let (before, after) = run_pass(r#\"{ir}\"#);\n    insta::assert_snapshot!(\"generated_ir_opt_before_{name}\", before);\n    insta::assert_snapshot!(\"generated_ir_opt_after_{name}\", after);\n}}\n\n",
            test_name_snapshot = test_name_snapshot,
            ir = case.ir,
            name = case.name
        )
        .unwrap();

        write!(
            out,
            "#[test]\nfn {test_name_asserts}() {{\n    let (_before, after) = run_pass(r#\"{ir}\"#);\n",
            test_name_asserts = test_name_asserts,
            ir = case.ir
        )
        .unwrap();
        for needle in case.must_not_contain_after {
            write!(
                out,
                "    assert!(!after.contains(r#\"{needle}\"#), \"expected to hoist/remove: {{}}\", r#\"{needle}\"#);\n",
                needle = needle
            )
            .unwrap();
        }
        for needle in case.must_contain_after {
            write!(
                out,
                "    assert!(after.contains(r#\"{needle}\"#), \"expected to keep/preserve: {{}}\", r#\"{needle}\"#);\n",
                needle = needle
            )
            .unwrap();
        }
        out.push_str("}\n\n");

        out.push_str(&format!(
            "#[test]\nfn {test_name_exec}() {{\n",
            test_name_exec = test_name_exec
        ));
        out.push_str("    let before_out = run_exec(r#\"");
        out.push_str(case.ir);
        out.push_str("\"#, &[");
        for (idx, b) in case.input.iter().enumerate() {
            if idx > 0 {
                out.push_str(", ");
            }
            out.push_str(&b.to_string());
        }
        out.push_str("]);\n");
        out.push_str("    let mut optimized = parse_case(r#\"");
        out.push_str(case.ir);
        out.push_str("\"#);\n");
        out.push_str("    kajit::ir_passes::run_default_passes(&mut optimized);\n");
        out.push_str("    let linear = kajit::linearize::linearize(&mut optimized);\n");
        out.push_str("    let dec = kajit::compile_decoder_linear_ir(&linear, false);\n");
        out.push_str("    let after_out = kajit::deserialize::<u8>(&dec, &[");
        for (idx, b) in case.input.iter().enumerate() {
            if idx > 0 {
                out.push_str(", ");
            }
            out.push_str(&b.to_string());
        }
        out.push_str("]).expect(\"optimized decoder should execute\");\n");
        out.push_str("    assert_eq!(after_out, before_out);\n");
        out.push_str("}\n\n");
    }

    out
}

fn render_test_file() -> String {
    let mut out = String::new();
    out.push_str("// @generated by xtask generate-synthetic. Do not edit manually.\n");
    out.push_str("use facet::Facet;\n\n");
    out.push_str(TYPES_RS.trim());
    out.push_str("\n\n");

    out.push_str("fn assert_json_case<T>(value: T)\n");
    out.push_str("where\n");
    out.push_str("    for<'input> T: Facet<'input> + serde::Serialize + serde::de::DeserializeOwned + PartialEq + std::fmt::Debug,\n");
    out.push_str("{\n");
    out.push_str("    let encoded = serde_json::to_string(&value).unwrap();\n");
    out.push_str("    let expected: T = serde_json::from_str(&encoded).unwrap();\n");
    out.push_str("    let decoder = kajit::compile_decoder_legacy(T::SHAPE, &kajit::json::KajitJson);\n");
    out.push_str("    let got: T = kajit::from_str(&decoder, &encoded).unwrap();\n");
    out.push_str("    assert_eq!(got, expected);\n");
    out.push_str("}\n\n");

    out.push_str("fn assert_postcard_case<T>(value: T, with_ir: bool)\n");
    out.push_str("where\n");
    out.push_str("    for<'input> T: Facet<'input> + serde::Serialize + serde::de::DeserializeOwned + PartialEq + std::fmt::Debug,\n");
    out.push_str("{\n");
    out.push_str("    let encoded = postcard::to_allocvec(&value).unwrap();\n");
    out.push_str("    let expected: T = postcard::from_bytes(&encoded).unwrap();\n");
    out.push_str("    let legacy = kajit::compile_decoder_legacy(T::SHAPE, &kajit::postcard::KajitPostcard);\n");
    out.push_str("    let legacy_out: T = kajit::deserialize(&legacy, &encoded).unwrap();\n");
    out.push_str("    assert_eq!(legacy_out, expected);\n");
    out.push_str("    if with_ir {\n");
    out.push_str("        let ir = kajit::compile_decoder_via_ir(T::SHAPE, &kajit::postcard::KajitPostcard);\n");
    out.push_str("        let ir_out: T = kajit::deserialize(&ir, &encoded).unwrap();\n");
    out.push_str("        assert_eq!(ir_out, expected);\n");
    out.push_str("    }\n");
    out.push_str("}\n\n");

    out.push_str("fn assert_ir_ra_snapshots(\n");
    out.push_str("    case: &str,\n");
    out.push_str("    format_label: &str,\n");
    out.push_str("    shape: &'static facet::Shape,\n");
    out.push_str("    decoder: &dyn kajit::format::IrDecoder,\n");
    out.push_str(") {\n");
    out.push_str("    let (ir_text, ra_text) = kajit::debug_ir_and_ra_mir_text(shape, decoder);\n");
    out.push_str("    insta::assert_snapshot!(\n");
    out.push_str("        format!(\n");
    out.push_str("            \"generated_rvsdg_{}_{}_{}\",\n");
    out.push_str("            format_label,\n");
    out.push_str("            case,\n");
    out.push_str("            std::env::consts::ARCH\n");
    out.push_str("        ),\n");
    out.push_str("        ir_text\n");
    out.push_str("    );\n");
    out.push_str("    insta::assert_snapshot!(\n");
    out.push_str("        format!(\n");
    out.push_str("            \"generated_ra_mir_{}_{}_{}\",\n");
    out.push_str("            format_label,\n");
    out.push_str("            case,\n");
    out.push_str("            std::env::consts::ARCH\n");
    out.push_str("        ),\n");
    out.push_str("        ra_text\n");
    out.push_str("    );\n");
    out.push_str("}\n\n");

    for case in CASES {
        if case.json_test {
            write!(
                out,
                "#[test]\nfn generated_json_{}() {{\n    let value: {} = {};\n    assert_json_case(value);\n}}\n\n",
                case.name, case.ty, case.value
            )
            .unwrap();
        }
        if case.postcard_test {
            write!(
                out,
                "#[test]\nfn generated_postcard_{}() {{\n    let value: {} = {};\n    assert_postcard_case(value, {});\n    if {} {{\n        assert_ir_ra_snapshots(\"{}\", \"postcard\", <{}>::SHAPE, &kajit::postcard::KajitPostcard);\n    }}\n}}\n\n",
                case.name,
                case.ty,
                case.value,
                if case.postcard_ir_test { "true" } else { "false" },
                if case.postcard_ir_test { "true" } else { "false" },
                case.name,
                case.ty
            )
            .unwrap();
        }
    }

    out
}

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("xtask should live in workspace/xtask")
        .to_path_buf()
}

fn write_file(path: &Path, content: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent directory");
    }
    fs::write(path, content).expect("write generated file");
}
