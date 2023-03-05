use global_common::{Globals, Mark, SyntaxContext, GLOBALS};

use crate::resolver::resolver;

use super::*;

fn test_transform(input: &str, expected: &str) {
    crate::testing::test_transform(
        |mut program, mut node_id_gen| {
            GLOBALS.set(&Globals::new(), || {
                let unresolved_mark = Mark::new();
                let top_level_mark = Mark::new();

                program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark, false));

                crate::normalize_properties::normalize_properties(&mut program, &mut node_id_gen);
                crate::normalize::normalize(&mut program, &mut node_id_gen);

                let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

                process(&mut program);

                program
            })
        },
        input,
        expected,
    );
}
fn test_transform_in_fn(input: &str, expected: &str) {
    test_transform(
        &format!("function FUNCTION(){{{}}}", input),
        &format!("function FUNCTION(){{{}}}", expected),
    );
}
fn test_same(input: &str) {
    test_transform(input, input);
}
fn test_same_in_fn(input: &str) {
    test_transform_in_fn(input, input);
}

#[test]
fn test_object_literal() {
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
obj1.prop1;

obj1.prop1 = 1;
obj1.prop2 = 2;
",
        "
const obj1 = { a: 1 };
obj1.a;

obj1.a = 1;
obj1.b = 2;
",
    );
}

#[test]
fn test_invalidation() {
    test_same_in_fn(
        "
const obj1 = { prop1: 1 };
obj1.prop1 = a;
",
    );
    test_same_in_fn(
        "
const obj1 = { prop1: 1 };
obj1[1] = a;
",
    );
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
const obj2 = { prop1: obj1 };
",
        "
const obj1 = { prop1: 1 };
const obj2 = { a: obj1 };
",
    );
    test_same_in_fn(
        "
const obj1 = { prop1: 1 };
const obj2 = { prop1: 1 };
const v1 = obj1 || obj2;
",
    );
    test_same_in_fn(
        "
const obj1 = { prop1: 1 };
let v1;
v1 = { prop1: obj1 };
",
    );
    test_same_in_fn(
        "
const obj1 = { prop1: 1 };
const obj2 = { prop1: 1 };
const v1 = a ? obj1 : obj2;
",
    );
}

// TODO: flow rhs type into lhs for assignExpr e.g. 'lhs = rhs';

// #[test]
// fn test_type_flows_through_assignment() {
//     test_transform_in_fn(
//         "
// const obj1 = { prop1: 1 };
// let obj2;
// obj2 = { prop1: 1 };
// obj2 = obj1;
// obj2.prop1;
// ",
//         "
// const obj1 = { a: 1 };
// let obj2;
// obj2 = { a: 1 };
// obj2 = obj1;
// obj2.a;
// ",
//     );
//     test_transform_in_fn(
//         "
// const obj1 = { prop1: 1 };
// let obj2 = { prop1: 1 };
// obj2 = obj1;
// obj2.prop1;
// ",
//         "
// const obj1 = { a: 1 };
// let obj2 = { a: 1 };
// obj2 = obj1;
// obj2.a;
// ",
//     );
//     // TODO: test invalidation works
// }

#[test]
fn test_type_flows_into_new_declaration() {
    // Renaming
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
const obj2 = obj1;
obj1.prop1;
obj2.prop1;
",
        "
const obj1 = { a: 1 };
const obj2 = obj1;
obj1.a;
obj2.a;
",
    );
    // Invalidation
    test_same_in_fn(
        "
const obj1 = { prop1: 1 };
const obj2 = obj1;
obj1.prop1;
obj2.prop1;
window.p = obj2;
",
    );
}

#[test]
fn test_object_literal_assignment_does_not_invalidate() {
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
obj.prop1;

obj = { prop1: 2 };
obj.prop1;
",
        "
let obj = { a: 1 };
obj.a;

obj = { a: 2 };
obj.a;
",
    );
}

// TODO: might require dataflow analysis to track which object is in the var at
// each point in the program.
// #[test]
// fn test_names_reused_for_different_objects_assigned_to_same_var() {
//     test_transform_in_fn(
//         "
// let obj = { prop1: 1 };
// obj.prop1;

// obj = { prop2: 2 };
// obj.prop2;
// ",
//         "
// let obj = { a: 1 };
// obj.a;

// obj = { b: 2 };
// obj.b;
// ",
//     );
// }

// #[test]
// fn test() {
//     test_transform_in_fn(
//         "
//   let variable = {propX: 1};
//   variable.propX;

//   variable = null;

//   variable.propX;

//   variable = {propY: 1};
//   variable.propY;
// ",
//         "
//   let variable = {a: 1};
//   variable.a;

//   variable = null;

//   variable.propX;

//   variable = {a: 1};
//   variable.a;
// ",
//     )
// }
