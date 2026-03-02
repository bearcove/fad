// IR post-register allocation cases

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
