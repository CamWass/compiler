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

                crate::normalize_shorthand::normalize_shorthand(&mut program, &mut node_id_gen);

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

// TODO: more tests e.g. for branch joins, more invalidations, etc

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
const obj1 = { b: 1 };
obj1.b;

obj1.b = 1;
obj1.a = 2;
",
    );
}

#[test]
fn test_invalidation() {
    test_same_in_fn(
        "
const obj1 = { prop1: 1 };
obj1[1] = a;
",
    );
}

#[test]
fn test_type_flows_through_assignment() {
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
let obj2;
obj2 = { prop1: 1 };
obj2 = obj1;
obj2.prop1;
",
        "
const obj1 = { a: 1 };
let obj2;
obj2 = { a: 1 };
obj2 = obj1;
obj2.a;
",
    );
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
let obj2 = { prop1: 1 };
obj2 = obj1;
obj2.prop1;
",
        "
const obj1 = { a: 1 };
let obj2 = { a: 1 };
obj2 = obj1;
obj2.a;
",
    );
    // TODO: test invalidation works
}

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

#[test]
fn test_names_reused_for_different_objects_assigned_to_same_var() {
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
obj.prop1;

obj = { prop2: 2 };
obj.prop2;
",
        "
let obj = { a: 1 };
obj.a;

obj = { a: 2 };
obj.a;
",
    );
}

#[test]
fn test_objects_are_only_merged_when_their_conflation_is_accessed_accessed() {
    // No reference, no access: conflation but no merge.
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
const obj2 = { prop2: 2 };
const v1 = a ? obj1 : obj2;
    ",
        "
const obj1 = { a: 1 };
const obj2 = { a: 2 };
const v1 = a ? obj1 : obj2;
    ",
    );
    // Reference, but no access: conflation but no merge.
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
const obj2 = { prop2: 2 };
const v1 = a ? obj1 : obj2;
v1;
        ",
        "
const obj1 = { a: 1 };
const obj2 = { a: 2 };
const v1 = a ? obj1 : obj2;
v1;
        ",
    );
    // Access: conflation and merge.
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
const obj2 = { prop2: 2 };
const v1 = a ? obj1 : obj2;
v1.prop1;
    ",
        "
const obj1 = { a: 1 };
const obj2 = { b: 2 };
const v1 = a ? obj1 : obj2;
v1.a;
    ",
    );
    // Invalidation: conflation and merge.
    test_same_in_fn(
        "
const obj1 = { prop1: 1 };
const obj2 = { prop2: 2 };
const v1 = a ? obj1 : obj2;
v1[1];
    ",
    );
}

// TODO: if the lhs of a ||= is an object, then it cannot be false and so the assignment is unreachable.
// If the lhs of a &&= is an object, then it cannot be false and so the assignment is unconditionally executed.
// The objects must not have been invalidated, as this could signal they were conflated with an unknown value,
// which may invalidate the above reasoning.
// Same goes for the corresponding binary expressions.
#[test]
fn test_logical_assign() {
    // No reference, no access: conflation but no merge.
    test_transform_in_fn(
        "
let obj1 = { prop1: 1 };
obj1 ||= cond ? { prop2: 2 } : { prop3: 3 };
    ",
        "
let obj1 = { a: 1 };
obj1 ||= cond ? { a: 2 } : { a: 3 };
    ",
    );
    // Access: conflation and merge.
    test_transform_in_fn(
        "
let obj1 = { prop1: 1 };
obj1 ||= cond ? { prop2: 2 } : { prop3: 3 };
obj1.prop1;
    ",
        "
let obj1 = { a: 1 };
obj1 ||= cond ? { c: 2 } : { b: 3 };
obj1.a;
    ",
    );
}

#[test]
fn test() {
    // TODO: is this behaviour ideal? would it be better to invalidate the prop
    // when it's referenced on an unknown type?
    test_transform_in_fn(
        "
let variable = {propX: 1};
variable.propX;

variable = null;

variable.propX;

variable = {propY: 1};
variable.propY;
",
        "
let variable = {a: 1};
variable.a;

variable = null;

variable.propX;

variable = {a: 1};
variable.a;
",
    );

    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
obj1.prop1 = a;
    ",
        "
const obj1 = { a: 1 };
obj1.a = a;
    ",
    );

    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
const obj2 = { prop1: obj1 };
    ",
        "
const obj1 = { a: 1 };
const obj2 = { a: obj1 };
    ",
    );

    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
let v1;
v1 = { prop1: obj1 };
    ",
        "
const obj1 = { a: 1 };
let v1;
v1 = { a: obj1 };
    ",
    );
}
