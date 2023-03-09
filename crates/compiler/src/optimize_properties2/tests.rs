use global_common::{Globals, Mark, GLOBALS};

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
fn test_same_in_fn(input: &str) {
    test_transform_in_fn(input, input);
}

// TODO: more tests e.g. for branch joins, more invalidations, infinite loops etc

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
obj1[1] = a;
",
    );
    test_same_in_fn(
        "
const obj1 = { prop1: { inner1 : 1 } };
window.foo = obj1;
",
    );
    // Anything assigned to a property of an invalid object should be invalidated.
    test_same_in_fn(
        "
const obj = {};
window.foo = obj;
obj.prop1 = { inner1 : 1 };
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
obj1 ||= cond ? { b: 2 } : { c: 3 };
obj1.a;
    ",
    );
}

#[test]
fn test_destructuring() {
    test_transform_in_fn(
        "
const obj = { prop1: 1 };
( { prop1: v1 } = obj );
",
        "
const obj = { a: 1 };
( { a: v1 } = obj );
",
    );

    test_transform_in_fn(
        "
const obj = { prop1: [1] };
( { prop1: [v1] } = obj );
",
        "
const obj = { a: [1] };
( { a: [v1] } = obj );
",
    );

    // We can't infer anything once we hit non-object destructuring.
    test_same_in_fn(
        "
const array = [ { prop1: 1 } ];
[ { prop1: v1 } ] = array;
    ",
    );
    test_transform_in_fn(
        "
const obj = { prop1: [ { prop2: 2} ] };
( { prop1: [ { prop2: v1 } ] } = obj );
",
        "
const obj = { a: [ { prop2: 2} ] };
( { a: [ { prop2: v1 } ] } = obj );
",
    );

    // Invalidated by dynamic property access.
    test_same_in_fn(
        "
const obj = { prop1: 1 };
( { [expr]: v1 } = obj );
",
    );

    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
( { prop2: obj1.prop1 } = { prop2: 2 } );
",
        "
const obj1 = { a: 1 };
( { a: obj1.a } = { a: 2 } );
",
    );

    test_transform_in_fn(
        "
const obj1 = { prop1: { prop2: 2 } };
( { prop3: obj1.prop1 } = { prop3: { prop4: 4 } } );
",
        "
const obj1 = { a: { a: 2 } };
( { a: obj1.a } = { a: { a: 4 } } );
",
    );

    test_transform_in_fn(
        "
const obj1 = { prop1: 1, prop2: { prop3: 3 } };
const { prop1: v1, ...rest } = obj1;
rest.prop2.prop3;
",
        "
const obj1 = { a: 1, b: { a: 3 } };
const { a: v1, ...rest } = obj1;
rest.b.a;
",
    );

    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
const { prop2 = obj1 } = { prop2: 2 };
prop2.prop1;
",
        "
const obj1 = { prop1: 1 };
const { a: prop2 = obj1 } = { a: 2 };
prop2.prop1;
",
    );

    // obj1 is invalidated because it is conflated with 2 by the destructuring
    // assignment.
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
const { prop2: foo = obj1 } = { prop1: 2, prop2: 2 };
",
        "
const obj1 = { prop1: 1 };
const { b: foo = obj1 } = { a: 2, b: 2 };
",
    );

    // obj1 and `prop2: { prop3: 3 }` are conflated, but are not merged.
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
const { prop2 = obj1 } = { prop2: { prop3: 3 }, prop4: 4 };
",
        "
const obj1 = { a: 1 };
const { a: prop2 = obj1 } = { a: { a: 3 }, b: 4 };
",
    );
    // Same as above, but the prop access causes that conflation to be merged.
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
const { prop2 = obj1 } = { prop2: { prop3: 3 }, prop4: 4 };
prop2.prop1;
",
        "
const obj1 = { a: 1 };
const { a: prop2 = obj1 } = { a: { b: 3 }, b: 4 };
prop2.a;
",
    );

    // Like plain property accesses, undefined properties are ignored.
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
let { prop1, prop2 } = obj;
",
        "
let obj = { a: 1 };
let { a: prop1, prop2: prop2 } = obj;
",
    );
}

#[test]
fn test_for_loops() {
    test_transform_in_fn(
        "
let obj = { count: 0 };
for (let i = 0; i < 5; i++) {
    obj = { count: i };
}
",
        "
let obj = { a: 0 };
for (let i = 0; i < 5; i++) {
    obj = { a: i };
}
",
    );
    test_transform_in_fn(
        "
let obj = { count: 0 };
for (let i = 0; i < 5; i++) {
    obj.count = i;
}
",
        "
let obj = { a: 0 };
for (let i = 0; i < 5; i++) {
    obj.a = i;
}
",
    );
    // Conflation but no merge.
    test_transform_in_fn(
        "
let obj = { count: 0 };
for (let i = 0; i < 5; obj = { prop1: 1, prop2: 2, count: i++ });
",
        "
let obj = { a: 0 };
for (let i = 0; i < 5; obj = { a: 1, b: 2, c: i++ });
",
    );
    // Conflation and merge.
    test_transform_in_fn(
        "
let obj = { count: 0 };
for (let i = 0; i < 5; obj = { prop1: 1, prop2: 2, count: i++ }) {
    obj.count;
}
",
        "
let obj = { c: 0 };
for (let i = 0; i < 5; obj = { a: 1, b: 2, c: i++ }) {
    obj.c;
}
",
    );
}

#[test]
fn test_while_loops() {
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
while (cond) {
    obj = { prop2: 2 };
}
",
        "
let obj = { a: 1 };
while (cond) {
    obj = { a: 2 };
}
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
while (cond) {
    obj = { prop2: 2 };
}
obj.prop1;
",
        "
let obj = { a: 1 };
while (cond) {
    obj = { b: 2 };
}
obj.a;
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
while (obj = { prop2: 2 });
",
        "
let obj = { a: 1 };
while (obj = { a: 2 });
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
while (obj = { prop2: 2 }) {
    obj.prop2;
}
",
        "
let obj = { a: 1 };
while (obj = { a: 2 }) {
    obj.a;
}
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
while (cond1) {
    if (cond3) {
        obj = { prop2: 2 };
    }

    if (cond2) {
        obj = { prop3: 3 };
    }
    obj.prop2;
}
",
        "
let obj = { a: 1 };
while (cond1) {
    if (cond3) {
        obj = { b: 2 };
    }

    if (cond2) {
        obj = { c: 3 };
    }
    obj.b;
}
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
while ( (obj.prop1, (obj = { prop2: 2 })) );
",
        "
let obj = { a: 1 };
while ( (obj.a, (obj = { b: 2 })) );
",
    );
}

#[test]
fn test_do_while_loops() {
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
do {
    obj = { prop2: 2 };
} while (cond);
",
        "
let obj = { a: 1 };
do {
    obj = { a: 2 };
} while (cond);
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
do {
    obj = { prop2: 2 };
} while (cond);
obj.prop1;
",
        "
let obj = { a: 1 };
do {
    obj = { b: 2 };
} while (cond);
obj.a;
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
do {} while (obj = { prop2: 2 });
",
        "
let obj = { a: 1 };
do {} while (obj = { a: 2 });
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
do { obj.prop1; } while (obj = { prop2: 2 });
",
        "
let obj = { a: 1 };
do { obj.a; } while (obj = {b: 2 });
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
do {
    if (cond3) {
        obj = { prop2: 2 };
    }

    if (cond2) {
        obj = { prop3: 3 };
    }
    obj.prop2;
} while (cond1);
",
        "
let obj = { a: 1 };
do {
    if (cond3) {
        obj = { b: 2 };
    }

    if (cond2) {
        obj = { c: 3 };
    }
    obj.b;
} while (cond1);
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
do {} while ( (obj.prop1, (obj = { prop2: 2 })) );
",
        "
let obj = { a: 1 };
do {} while ( (obj.a, (obj = { b: 2 })) );
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
