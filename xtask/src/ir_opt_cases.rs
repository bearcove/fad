//! IR optimization test cases.

use crate::IrOptCase;

pub(crate) const IR_OPT_CASES: &[IrOptCase] = &[
    IrOptCase {
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
    },
];
