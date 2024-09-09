use std::fmt::Write;

use global_common::{Globals, Mark, GLOBALS};

use crate::resolver::resolver;

use super::*;

fn test_transform(input: &str, expected: &str) {
    crate::testing::test_transform(
        |mut program, mut program_data| {
            GLOBALS.set(&Globals::new(), || {
                let unresolved_mark = Mark::new();
                let top_level_mark = Mark::new();

                program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

                crate::normalize_properties::normalize_properties(&mut program, &mut program_data);

                let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

                process(&mut program, &mut program_data, unresolved_ctxt);

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
fn test_same(input: &str) {
    test_transform(input, input);
}

fn test_props(obj: &str, props: &[JsWord]) {
    // Test that built-in props are not renamed.
    let mut accesses = String::new();
    for prop in props {
        accesses.write_fmt(format_args!("a.{prop}\n")).unwrap();
    }
    let input = format!("const a = {obj};\n{accesses}");
    test_same(&input);

    // Test that storing value in built-in prop invalidates the value.
    for prop in props {
        test_same(&format!("const a = {obj};\n a.{prop} = {{ prop: 1 }}"));
    }
}

#[test]
fn test_built_in_regex_properties() {
    test_props("/x/", REGEX_PROPERTIES);
}

#[test]
fn test_built_in_string_properties() {
    test_props("''", STRING_PROPERTIES);
}

#[test]
fn test_built_in_num_properties() {
    test_props("1", NUM_PROPERTIES);
}

#[test]
fn test_built_in_object_properties() {
    test_props("true", OBJECT_PROPERTIES);
    test_props("1", OBJECT_PROPERTIES);
    test_props("''", OBJECT_PROPERTIES);
    test_props("1n", OBJECT_PROPERTIES);
    test_props("/a/", OBJECT_PROPERTIES);
    test_props("{}", OBJECT_PROPERTIES);
    test_props("() => {}", OBJECT_PROPERTIES);
}

#[test]
fn test_optional_chain() {
    test_transform(
        "
const obj = { prop: 1 };
obj?.prop;
",
        "
const obj = { a: 1 };
obj?.a;
",
    );

    test_transform(
        "
const obj = { prop: 1 };
obj?.['prop'];
",
        "
const obj = { a: 1 };
obj?.a;
",
    );

    test_transform(
        "
function func(arg) {
    arg.prop2; arg.prop2;
    return arg;
}

const obj = { funcProp: func };
obj?.funcProp?.({ prop1: 1, prop2: 2 }).prop1;
",
        "
function func(arg) {
    arg.a; arg.a;
    return arg;
}

const obj = { a: func };
obj?.a?.({ b: 1, a: 2 }).b;
",
    );
}

#[test]
fn test_spread() {
    // Check the spread target is invalidated.
    test_same(
        "
const obj = { prop: 1 };
func(...obj)
",
    );
    test_same(
        "
const obj = { prop: 1 };
new Foo(...obj);
",
    );
    test_same(
        "
const obj = { prop: 1 };
[...obj];
",
    );

    // Check all other arguments are invalidated too.
    test_same(
        "
function foo() {}
const obj = { prop: 1 };
foo(...unknown, obj);
",
    );
}

#[test]
fn test_primitive_access_does_not_conflate() {
    // Had a bug where the same property name on different objects would be
    // conflated they were accessed on a union with a primitive.
    // e.g. the 'common' property of x and y was conflated, which meant the
    // objects in them were renamed together, rather than independently. This
    // meant that 'prop2' wasn't getting the shortest name, despite being the
    // only property in its object.
    let test = |primitive: &str| {
        test_transform(
            &format!(
                "
const x = {{ common: {{ prop1: 'i', prop3: 'o' }} }} || {primitive};
const y = {{ common: {{ prop2: 'i' }} }} || {primitive};

x.common.prop3; // reference 'prop3' more so it gets a shorter name

x.common; // access 'common' on the union with the primitive
y.common;
    ",
            ),
            &format!(
                "
const x = {{ a: {{ b: 'i', a: 'o' }} }} || {primitive};
const y = {{ a: {{ a: 'i' }} }} || {primitive};

x.a.a;

x.a;
y.a;
    ",
            ),
        );
    };
    test("null");
    test("true");
    test("1");
    test("''");
    test("1n");
    test("/a/");

    test_transform(
        "
const x = { common: { prop1: 'i', prop3: 'o' } } || unknown;
const y = { common: { prop2: 'i' } } || unknown;

x.common.prop3; // reference 'prop3' so it gets a shorter name

x.common; // access 'common' on the union with the primitive
y.common;
",
        "
const x = { common: { a: 'i', prop3: 'o' } } || unknown;
const y = { common: { a: 'i' } } || unknown;

x.common.prop3;

x.common;
y.common;
",
    );
}

#[test]
fn test_complex_props_are_visited() {
    // Had a bug where 'complex' properties were skipped entirely.

    test_transform(
        "
const complex = {
    [({ prop1: 1 }, 'prop2')]: 2
};
",
        "
const complex = {
    [({ a: 1 }, 'prop2')]: 2
};
",
    );
    test_transform(
        "
const { [({ prop1: 1 }, 'prop2')]: thing1 } = thing2;
",
        "
const { [({ a: 1 }, 'prop2')]: thing1 } = thing2;
",
    );
    test_transform(
        "
thing1[({ prop1: 1 }, 'prop2')] = 2;
",
        "
thing1[({ a: 1 }, 'prop2')] = 2;
",
    );
}

#[test]
fn test_unary_operators() {
    test_transform(
        "
function f() {
    const expr = +true || typeof 1 || void 1 || !1 || delete 1;
    const obj = expr || { prop1: 1 };
    obj.prop1;
    return obj;
}
f().prop1;
",
        "
function f() {
    const expr = +true || typeof 1 || void 1 || !1 || delete 1;
    const obj = expr || { a: 1 };
    obj.a;
    return obj;
}
f().a;
",
    );
    // We don't currently track the input type, so can't know the output type
    // for numeric ops.
    test_same(
        "
function f() {
    const obj = -1 || ~1 || { prop1: 1 };
    obj.prop1;
    return obj;
}
f().prop1;
",
    );
}

#[test]
fn test_binary_operators() {
    // The type of an equality/relative operator should be a boolean.
    test_transform(
        "
function f() {
    const equality = v1 == v2 || v1 != v2 || v1 === v2 || v1 !== v2;
    const relative = v1 < v2 || v1 <= v2 || v1 > v2 || v1 >= v2 || v1 in v2 || v1 instanceof v2;
    const obj = equality || relative || { prop1: 1 };
    obj.prop1;
    return obj;
}
f().prop1;
",
        "
function f() {
    const equality = v1 == v2 || v1 != v2 || v1 === v2 || v1 !== v2;
    const relative = v1 < v2 || v1 <= v2 || v1 > v2 || v1 >= v2 || v1 in v2 || v1 instanceof v2;
    const obj = equality || relative || { a: 1 };
    obj.a;
    return obj;
}
f().a;
",
    );
    // We don't currently track the input types, so output type is union of Number/BigInt/String.
    test_same(
        "
function f() {
    const shift = v1 << v2 || v1 >> v2 || v1 >>> v2;
    const numeric = v1 + v2 || v1 - v2 || v1 * v2 || v1 / v2 || v1 % v2 || v1 ** v2;
    const bitwise = v1 | v2 || v1 ^ v2 || v1 & v2;
    const obj = shift || numeric || bitwise || { a: 1 };
    obj.a;
    return obj;
}
f().a;
",
    );

    test_same("unknown in { prop: 1 }");
    test_transform(
        "unknown in { prop: { inner: 1 } }",
        "unknown in { prop: { a: 1 } }",
    );
    test_transform("'prop' in { prop: 1 };", r#" "a" in { a: 1 };"#);
}

#[test]
fn test_nested_unions() {
    // Had a bug where union constituents were always added to the first/outermost
    // union being constructed. This meant `bar` returned `obj1 | obj2 | obj3 | obj4`
    // instead of `obj1 | obj4`.
    test_transform(
        "
function foo(arg) {
    const obj4 = { prop4: 'prop4', zCommon: 'zCommon' };
    return obj4;
}
function bar() {
    const obj1 = { prop1: 'prop1', zCommon: 'zCommon' };
    const obj2 = { prop2: 'prop2', zCommon: 'zCommon' };
    const obj3 = { prop3: 'prop3', zCommon: 'zCommon' };
    
    return obj1 || foo(obj2 || obj3);
}
bar().zCommon;
",
        "
function foo(arg) {
    const obj4 = { b: 'prop4', a: 'zCommon' };
    return obj4;
}
function bar() {
    const obj1 = { b: 'prop1', a: 'zCommon' };
    const obj2 = { a: 'prop2', b: 'zCommon' };
    const obj3 = { a: 'prop3', b: 'zCommon' };
    
    return obj1 || foo(obj2 || obj3);
}
bar().a;
",
    );
}

#[test]
fn test_primitive_types() {
    // Null and undefined are not included in unions.
    test_transform(
        "
let obj1 = { prop1: 1 };
(obj1 || null || undefined).prop1;
let obj2 = { prop2: 2 };
(obj2 || null || undefined).prop2;
",
        "
let obj1 = { a: 1 };
(obj1 || null || undefined).a;
let obj2 = { a: 2 };
(obj2 || null || undefined).a;
",
    );

    // This code is invalid, but still tests useful behaviour.
    test_same_in_fn(
        "
(undefined).prop1;
(undefined).prop2;
(null).prop3;
(null).prop4;
    ",
    );

    // Boolean, number, string, and big int have their own distinct and valid object types.
    test_transform(
        "
(1 || { prop1: 1 }).prop1;
('1' || { prop1: 1 }).prop1;
(true || { prop1: 1 }).prop1;
(1n || { prop1: 1 }).prop1;
",
        "
(1 || { a: 1 }).a;
('1' || { a: 1 }).a;
(true || { a: 1 }).a;
(1n || { a: 1 }).a;
",
    );

    // Build in methods/properties should not be renamed.
    test_same(
        "
(1).toExponential;
(1).toFixed;
(1).toPrecision;

('a').length;
('a').at;
('a').charAt;
('a').charCodeAt;
('a').codePointAt;
('a').concat;
('a').endsWith;
('a').includes;
('a').indexOf;
('a').isWellFormed;
('a').lastIndexOf;
('a').localeCompare;
('a').match;
('a').matchAll;
('a').normalize;
('a').padEnd;
('a').padStart;
('a').repeat;
('a').replace;
('a').replaceAll;
('a').search;
('a').slice;
('a').split;
('a').startsWith;
('a').substr;
('a').substring;
('a').toLocaleLowerCase;
('a').toLocaleUpperCase;
('a').toLowerCase;
('a').toUpperCase;
('a').toWellFormed;
('a').trim;
('a').trimEnd;
('a').trimStart;

(/a/).exec;
(/a/).dotAll;
(/a/).flags;
(/a/).global;
(/a/).hasIndices;
(/a/).ignoreCase;
(/a/).multiline;
(/a/).source;
(/a/).sticky;
(/a/).test;
(/a/).unicode;
(/a/).unicodeSets;
(/a/).lastIndex;
",
    );
    // Same for unions with primitives.
    test_same(
        "
({ constructor: 1 } || 1).constructor;
({ constructor: 1 } || 1n).constructor;
({ constructor: 1 } || true).constructor;
({ constructor: 1 } || 'a').constructor;
",
    );
}

// #[test]
// fn test_calls_do_not_interfere() {
//     test_transform(
//         "
// function foo(a) {
//     let obj = { inner: a };
//     return obj;
// }

// let obj1 = foo({ prop1: 1 }); // { inner: { prop1: 1 } }
// obj1.aProp; obj1.aProp; obj1.aProp;
// obj1.inner.prop1;
// let obj2 = foo({ prop2: 2 }); // { inner: { prop2: 2 } }
// obj2.inner.prop2;
// ",
//         "
// function foo(a) {
//     let obj = { b: a };
//     return obj;
// }

// let obj1 = foo({ a: 1 });
// obj1.a; obj1.a; obj1.a;
// obj1.b.a;
// let obj2 = foo({ a: 2 });
// obj2.b.a;
// ",
//     );
//     test_transform(
//         "
// function foo(a) {
//     let obj = { inner: a };
//     return obj;
// }

// let obj1 = foo({ prop1: 1 }); // { inner: { prop1: 1 } }
// obj1.inner.prop1;
// let obj2 = foo({ prop2: 2 }); // { inner: { prop2: 2 } }
// obj2.inner.prop2;
// ",
//         "
// function foo(a) {
//     let obj = { a: a };
//     return obj;
// }

// let obj1 = foo({ a: 1 });
// obj1.a.a;
// let obj2 = foo({ a: 2 });
// obj2.a.a;
// ",
//     );

//     // Each call to addFoo should get its own object assigned to 'foo' prop.
//     // Each of these objects must share the properties of the original, but all
//     // other properties are not shared (unless accessed on union etc).
//     test_transform(
//         "
// function addFoo(arg) {
//     arg.foo = { foo1: 'f1', foo2: 'f2' };
// }

// const obj1 = {};
// addFoo(obj1);
// obj1.foo.prop1 = 'p1';

// const obj2 = {};
// addFoo(obj2);
// obj2.foo.prop2 = 'p2';
// obj2.foo.prop3 = 'p3';
// ",
//         "
// function addFoo(arg) {
//     arg.a = { a: 'f1', b: 'f2' };
// }

// const obj1 = {};
// addFoo(obj1);
// obj1.a.c = 'p1';

// const obj2 = {};
// addFoo(obj2);
// obj2.a.c = 'p2';
// obj2.a.d = 'p3';
// ",
//     );
//     test_transform(
//         "
// function addFoo(arg) {
//     arg.foo = { foo1: 'f1', foo2: 'f2' };
// }

// const obj1 = {};
// addFoo(obj1);
// obj1.foo.prop1 = 'p1';

// const obj2 = {};
// addFoo(obj2);
// obj2.foo.prop2 = 'p2';
// obj2.foo.prop3 = 'p3';

// const obj = cond ? obj1 : obj2;
// obj.foo.prop1
// ",
//         "
// function addFoo(arg) {
//     arg.a = { b: 'f1', c: 'f2' };
// }

// const obj1 = {};
// addFoo(obj1);
// obj1.a.a = 'p1';

// const obj2 = {};
// addFoo(obj2);
// obj2.a.d = 'p2';
// obj2.a.e = 'p3';

// const obj = cond ? obj1 : obj2;
// obj.a.a
// ",
//     );
//     test_transform(
//         "
// function addFoo(arg) {
//     arg.foo = { foo1: 'f1', foo2: 'f2' };
// }

// const obj1 = {};
// addFoo(obj1);
// obj1.foo.prop1 = 'p1';

// const obj2 = {};
// addFoo(obj2);
// obj2.foo.prop2 = 'p2';
// obj2.foo.prop3 = 'p3';

// const obj = cond ? obj1 : obj2;
// obj.foo.prop2
// ",
//         "
// function addFoo(arg) {
//     arg.a = { b: 'f1', c: 'f2' };
// }

// const obj1 = {};
// addFoo(obj1);
// obj1.a.d = 'p1';

// const obj2 = {};
// addFoo(obj2);
// obj2.a.a = 'p2';
// obj2.a.d = 'p3';

// const obj = cond ? obj1 : obj2;
// obj.a.a
// ",
//     );
// }

#[test]
fn test_functions() {
    test_transform(
        "
function foo(a, b) {
    return cond ? a : b;
}
const obj = { prop3: 3 };
foo({prop1: 1}, obj).prop1;
foo({prop2: 2}, obj).prop2;
",
        "
function foo(a, b) {
    return cond ? a : b;
}
const obj = { c: 3 };
foo({a: 1}, obj).a;
foo({b: 2}, obj).b;
",
    );
    test_transform(
        "
function addInner(a) {
    a.inner = { zCommon: 1, prop3: 3 };
    return a;
}
function getInner(a) {
    if (noInner) {
        return addInner(a).inner;
    } else {
        return a.inner;
    }
}
function f3() {
    let obj = { inner: { zCommon: 1, prop2: 2 } };
    if (cond) {
        return obj;
    }
    const a = getInner(obj);
    a.zCommon;
    return a;
}
const o = f3();
const i = o.inner;
i.zCommon; i.zCommon; i.zCommon;
i.prop3;
o.prop3;
    ",
        "
function addInner(a) {
    a.b = { a: 1, c: 3 };
    return a;
}
function getInner(a) {
    if (noInner) {
        return addInner(a).b;
    } else {
        return a.b;
    }
}
function f3() {
    let obj = { b: { a: 1, d: 2 } };
    if (cond) {
        return obj;
    }
    const a = getInner(obj);
    a.a;
    return a;
}
const o = f3();
const i = o.b;
i.a; i.a; i.a;
i.c;
o.c;
    ",
    );
    test_transform(
        "
function getProp1(arg) {
    arg.hasProp1 = true;
    return { prop1: 1, parent: arg };
}
function getProp2(arg) {
    arg.hasProp2 = true;
    return { prop2: 2, parent: arg };
}
function foo(arg) {
    if (cond) {
        arg.inner = getProp1(arg);
    } else {
        arg.inner = getProp2(arg);
    }
    return arg;
}
foo({ prop: 1 }).inner.prop2;
foo({ prop: 1 }).hasProp2;
    ",
        "
function getProp1(arg) {
    arg.c = true;
    return { c: 1, b: arg };
}
function getProp2(arg) {
    arg.b = true;
    return { a: 2, b: arg };
}
function foo(arg) {
    if (cond) {
        arg.a = getProp1(arg);
    } else {
        arg.a = getProp2(arg);
    }
    return arg;
}
foo({ d: 1 }).a.a;
foo({ d: 1 }).b;
    ",
    );
    test_transform(
        "
function assignProp1(arg) {
    arg.inner = { prop1: 1 };
}
function assignProp2(arg) {
    arg.inner = { prop2: 2 };
}
function foo(arg) {
    if (cond) {
        assignProp1(arg);
    } else {
        assignProp2(arg);
    }
    return arg;
}
foo({ prop: 1 }).inner.prop2;
    ",
        "
function assignProp1(arg) {
    arg.a = { b: 1 };
}
function assignProp2(arg) {
    arg.a = { a: 2 };
}
function foo(arg) {
    if (cond) {
        assignProp1(arg);
    } else {
        assignProp2(arg);
    }
    return arg;
}
foo({ b: 1 }).a.a;
    ",
    );
    test_transform(
        "
function foo7(count) {
  if (cond) {
    return foo8(count + 1);
  } else {
    return { foo7_count: count };
  }
}
function foo8(count) {
  if (cond) {
    return foo7(count + 1);
  } else {
    return { foo8_count: count };
  }
}
const b = foo8(0).foo8_count; // { foo7_count, foo8_count }
    ",
        "
function foo7(count) {
  if (cond) {
    return foo8(count + 1);
  } else {
    return { b: count };
  }
}
function foo8(count) {
  if (cond) {
    return foo7(count + 1);
  } else {
    return { a: count };
  }
}
const b = foo8(0).a;
    ",
    );
    test_transform(
        "
function foo9(count) {
  if (cond) {
    return foo9(count + 1);
  } else {
    return { count: count, other: 1 };
  }
}
const a = foo9(0); // { count, other }
a.count; a.other; a.other;
    ",
        "
function foo9(count) {
  if (cond) {
    return foo9(count + 1);
  } else {
    return { b: count, a: 1 };
  }
}
const a = foo9(0);
a.b; a.a; a.a;
    ",
    );
    // 'rotate' can return any on its 3 params
    test_transform(
        "
function rotate(a, b, c) {
    if (cond) {
        return rotate(c, a, b);
    } else {
        return a;
    }
}
const res = rotate( { prop1: 1 }, { prop2: 2 }, { prop3: 3 } );
res.prop2;
",
        "
function rotate(a, b, c) {
    if (cond) {
        return rotate(c, a, b);
    } else {
        return a;
    }
}
const res = rotate( { b: 1 }, { a: 2 }, { b: 3 } );
res.a;
",
    );
    test_transform(
        "
function inner() {
    return { prop1: 1, prop2: 2};
}
function outer() {
    return inner();
}
const a = outer().prop2;
    ",
        "
function inner() {
    return { b: 1, a: 2};
}
function outer() {
    return inner();
}
const a = outer().a;
    ",
    );
    test_transform(
        "
function identity(a) {
    return a;
}
function f2() { // returns obj or obj.inner
    let obj = { inner: { prop1: 1 } };
    const a = identity(obj); // obj
    if (cond) {
        return a.inner;
    }
    const b = identity(obj); // obj
    return b;
}
function f3() {
    f2().inner;
    f2().prop1;
}
    ",
        "
function identity(a) {
    return a;
}
function f2() { // returns obj or obj.inner
    let obj = { a: { b: 1 } };
    const a = identity(obj); // obj
    if (cond) {
        return a.a;
    }
    const b = identity(obj); // obj
    return b;
}
function f3() {
    f2().a;
    f2().b;
}
    ",
    );
    // Two mutually recursive functions that only return the 'obj' param.
    test_transform(
        "
function f7(num, obj) {
    if (cond) {
        return f8(obj, num);
    } else {
        return obj;
    }
}
function f8(obj, num) {
    if (cond) {
        return f7(num, obj);
    } else {
        return obj;
    }
}
const obj = { prop1: 1, prop2: 2};
const res = f8(obj, 123).prop2;
    ",
        "
function f7(num, obj) {
    if (cond) {
        return f8(obj, num);
    } else {
        return obj;
    }
}
function f8(obj, num) {
    if (cond) {
        return f7(num, obj);
    } else {
        return obj;
    }
}
const obj = { b: 1, a: 2};
const res = f8(obj, 123).a;
    ",
    );
    // Recursive function that conditionally flips params.
    test_transform(
        "
function f1(a, b) {
    if (cond) {
        return f1(b, a);
    } else {
        return a;
    }
}
f1({ prop1: 1 }, { prop2: 2 }).prop2;
    ",
        "
function f1(a, b) {
    if (cond) {
        return f1(b, a);
    } else {
        return a;
    }
}
f1({ b: 1 }, { a: 2 }).a;
    ",
    );
    test_transform(
        "
function foo(a) {
    a.common;
}
foo({ prop1: 1, common: true });
foo({ prop2: 2, common: true });
    ",
        "
function foo(a) {
    a.a;
}
foo({ b: 1, a: true });
foo({ b: 2, a: true });
    ",
    );
    test_transform(
        "
function foo(a) {
    return a;
}
const obj1 = { prop1: 1, prop2: 2 };
obj1.prop1;
foo(obj1).prop2;
const obj2 = { prop3: 3, prop4: 4 };
obj2.prop3;
foo(obj2).prop4;
",
        "
function foo(a) {
    return a;
}
const obj1 = { a: 1, b: 2 };
obj1.a;
foo(obj1).b;
const obj2 = { a: 3, c: 4 };
obj2.a;
foo(obj2).c;
",
    );
    test_transform(
        "
function foo() {
    return { prop: { prop1: 1, prop2: 2 } };
}
function bar() {
    foo().prop.prop1;
    return foo().prop;
}
bar().prop2;
",
        "
function foo() {
    return { a: { a: 1, b: 2 } };
}
function bar() {
    foo().a.a;
    return foo().a;
}
bar().b;
",
    );
    test_transform(
        "
function thing() {
    const obj = { prop1: 1, prop2: 2 };
    obj.prop1; obj.prop1; obj.prop1;
    return obj;
}
thing().prop2;
",
        "
function thing() {
    const obj = { a: 1, b: 2 };
    obj.a; obj.a; obj.a;
    return obj;
}
thing().b;
",
    );
    test_transform(
        "
function foo(obj, prop) {
    obj.inner = prop;
}

function bar() {
    let obj = {};
    let prop = { prop1: 1 };
    for (; foo(obj, prop);) {
      prop = { prop2: 2 };
    }
    obj.inner.prop1;
}
bar();
",
        "
function foo(obj, prop) {
    obj.a = prop;
}

function bar() {
    let obj = {};
    let prop = { a: 1 };
    for (; foo(obj, prop);) {
      prop = { b: 2 };
    }
    obj.a.a;
}
bar();
",
    );
}

#[test]
fn test_implicit_return() {
    // Functions always return something, even if they return undefined implicitly.
    test_transform(
        "
function foo() {}
const thing = foo() || { prop: 1 };
thing.prop;
",
        "
function foo() {}
const thing = foo() || { a: 1 };
thing.a;
",
    );
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
obj1[foo] = a;
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
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
const obj2 = { prop1: 1 };
window.obj2 = obj2;
",
        "
const obj1 = { a: 1 };
const obj2 = { prop1: 1 };
window.obj2 = obj2;
",
    );
    // Access creates union with invalidated object; renaming not possible
    test_same_in_fn(
        "
let obj = { prop1: 1 };
window.foo = obj;
while (cond) {
    obj.prop1;
    obj = { prop1: 2 };
    obj.prop1;
}
",
    );
    // TODO: requires control flow/SSA
    //    // Always reassigned before access; renaming possible
    //    test_transform_in_fn(
    //        "
    //let obj = { prop1: 1 };
    //window.foo = obj;
    //while (cond) {
    //    obj = { prop1: 2 };
    //    obj.prop1;
    //}
    //",
    //        "
    //let obj = { prop1: 1 };
    //window.foo = obj;
    //while (cond) {
    //    obj = { a: 2 };
    //    obj.a;
    //}
    //",
    //    );
    // The call to window.func may access/change the properties of obj, so we can't know
    // if other code depends on the name 'prop1'.
    test_same_in_fn(
        "
let obj = {};
window.func(obj);
let thing = obj;
if (cond) {
    thing = {prop1:1};
}
thing.prop1;
",
    );
    // Had a bug where object `a` was invalidated instead of `{ [v]: 1 }`.
    test_transform(
        "
function f() {
    let a = { prop1: 1 };
    return a, { [v]: 1 }, a;
}
f().prop1;
",
        "
function f() {
    let a = { a: 1 };
    return a, { [v]: 1 }, a;
}
f().a;
",
    );
}

#[test]
fn test_yield_invalidation() {
    test_same(
        "
function* f() {
    yield { prop: 1 };
}
",
    );
}

#[test]
fn test_throw_invalidation() {
    test_same("throw { prop: 1 };");
}

#[test]
fn test_catch_invalidation() {
    test_same(
        "
try {} catch (e) {
    (e || { prop: 1 }).prop
}
",
    );
}

#[test]
fn test_for_in_invalidation() {
    test_same("for (var x in { prop: 1 }) {}");
}

#[test]
fn test_for_of_invalidation() {
    test_same("for (var x of { prop: 1 }) {}");
}

#[test]
fn test_array_element_invalidation() {
    test_same("[ { prop: 1 } ];");
}

#[test]
fn test_new_expr_invalidation() {
    test_same(
        "
const a = { prop1: 1 };
new a( { prop2: 2 } );
",
    );
}

#[test]
fn test_top_level_return_invalidation() {
    test_same("return { prop: 1 };");
}

#[test]
fn test_unknown_caller_arg_invalidation() {
    test_same("func({ prop: 1 });");
}

#[test]
fn test_unknown_or_invalid_assign_target_invalidation() {
    test_same(
        "
    unknown = { prop1: 1 };

    unknown.thing1 = { prop2: 2 };
    
    const obj = { inner: { prop3: 3 } };
    unknown.thing2 = obj;
",
    );
}

#[test]
fn test_function_invalidation() {
    // We can't track how return value is used, so it must be invalidated.
    test_same(
        "
function func() {
    return { prop: 1 };
}
window.func = func;
func().prop;
",
    );
    // func may be called with unknown params.
    test_same(
        "
function func(param) {
    param.prop;
}
window.func = func;
func({ prop: 1 });
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

// TODO: requires control flow/SSA
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

// obj = { a: 2 };
// obj.a;
// ",
//     );
// }

#[test]
fn test_props_only_conflated_when_union_is_accessed() {
    // No reference, no access: no conflation.
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
    // Reference, but no access: no conflation.
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
    // Access: conflation.
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
    // Invalidation.
    test_same_in_fn(
        "
const obj1 = { prop1: 1 };
const obj2 = { prop2: 2 };
const v1 = a ? obj1 : obj2;
v1[foo];
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
    // No reference, no access: no conflation.
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
    // Access: conflation.
    test_transform_in_fn(
        "
let obj1 = { prop1: 1 };
obj1 ||= cond ? { prop2: 2 } : { prop3: 3 };
obj1.prop1;
    ",
        "
let obj1 = { a: 1 };
obj1 ||= cond ? { b: 2 } : { b: 3 };
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
const obj1 = { a: 1 };
const { a: prop2 = obj1 } = { a: 2 };
prop2.a;
",
    );

    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
const { prop2: foo = obj1 } = { prop1: 2, prop2: 2 };
",
        "
const obj1 = { a: 1 };
const { a: foo = obj1 } = { b: 2, a: 2 };
",
    );

    // obj1 and `prop2: { prop3: 3 }` are not conflated.
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
    // Same as above, but the prop access causes conflation.
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

    // undefined property.
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
let { prop1, prop2 } = obj;
",
        "
let obj = { a: 1 };
let { a: prop1, b: prop2 } = obj;
",
    );
    // The value of the assign expression should be the RHS.
    test_transform_in_fn(
        "
({ prop1: prop = def } = { prop1: 1, prop1: 1, prop2: 2 }).prop2;
",
        "
({ a: prop = def } = { a: 1, a: 1, b: 2,  }).b;
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
    // No conflation.
    test_transform_in_fn(
        "
let obj = { count: 0 };
for (let i = 0; i < 5; obj = { prop1: 1, prop2: 2, count: i++ });
",
        "
let obj = { a: 0 };
for (let i = 0; i < 5; obj = { b: 1, c: 2, a: i++ });
",
    );
    // Conflation.
    test_transform_in_fn(
        "
let obj = { count: 0 };
for (let i = 0; i < 5; obj = { prop1: 1, prop2: 2, count: i++ }) {
    obj.count;
}
",
        "
let obj = { a: 0 };
for (let i = 0; i < 5; obj = { b: 1, c: 2, a: i++ }) {
    obj.a;
}
",
    );
}

#[test]
fn test_while_loops() {
    // TODO: requires control flow/SSA
    //    test_transform_in_fn(
    //        "
    //let obj = { prop1: 1 };
    //while (cond) {
    //    obj = { prop2: 2 };
    //}
    //",
    //        "
    //let obj = { a: 1 };
    //while (cond) {
    //    obj = { a: 2 };
    //}
    //",
    //    );
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
    // TODO: requires control flow/SSA
    //    test_transform_in_fn(
    //        "
    //let obj = { prop1: 1 };
    //while (obj = { prop2: 2 });
    //",
    //        "
    //let obj = { a: 1 };
    //while (obj = { a: 2 });
    //",
    //    );
    //    test_transform_in_fn(
    //        "
    //let obj = { prop1: 1 };
    //while (obj = { prop2: 2 }) {
    //    obj.prop2;
    //}
    //",
    //        "
    //let obj = { a: 1 };
    //while (obj = { a: 2 }) {
    //    obj.a;
    //}
    //",
    //    );
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
let obj = { b: 1 };
while (cond1) {
    if (cond3) {
        obj = { a: 2 };
    }

    if (cond2) {
        obj = { b: 3 };
    }
    obj.a;
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
let obj = { b: 1 };
do {
    if (cond3) {
        obj = { a: 2 };
    }

    if (cond2) {
        obj = { b: 3 };
    }
    obj.a;
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
fn test_name_assignment() {
    // Independent objects; all props have same number of references;
    // names assigned lexicographically. zzzCommon gets longer name.
    test_transform_in_fn(
        "
let obj1 = { zzzCommon: true, aProp: 1 };
let obj2 = { zzzCommon: true, aProp: 1 };
",
        "
let obj1 = { b: true, a: 1 };
let obj2 = { b: true, a: 1 };
",
    );
    // Same as above, but with a union.
    // zzzCommon now has more references, so it gets shorter name across both objects.
    test_transform_in_fn(
        "
let obj1 = { zzzCommon: true, aProp: 1 };
let obj2 = { zzzCommon: true, aProp: 1 };

if (cond) {
    obj1 = obj2;
}
obj1.zzzCommon
",
        "
let obj1 = { a: true, b: 1 };
let obj2 = { a: true, b: 1 };

if (cond) {
    obj1 = obj2;
}
obj1.a
",
    );

    // No prop can have the same name as person_id (it's used as a discriminant).
    // All other props can share names, so long as they are distinct within their
    // defining object.
    test_transform_in_fn(
        "
let obj = { person_id: 1, name: 'jeff', age: 'old', height: '>10' };

if (cond) {
  obj = { pet_id: 1, name: 'doug', age: 'also old', kind: 'dog' };
}

if (obj.person_id !== undefined) {
  // obj is a person
} else {
  // obj is not a person
}
",
        "
let obj = { a: 1, d: 'jeff', b: 'old', c: '>10' };

if (cond) {
  obj = { e: 1, d: 'doug', b: 'also old', c: 'dog' };
}

if (obj.a !== undefined) {
  // obj is a person
} else {
  // obj is not a person
}
",
    )
}

#[test]
fn test_literal_prop_keys() {
    test_transform_in_fn(
        "
let obj = { prop1: 1, 'prop2': 2, };
obj.prop2;
",
        "
let obj = { b: 1, a: 2, };
obj.a;
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1, 'prop2': 2, };
let { 'prop2': foo } = obj;
",
        "
let obj = { b: 1, a: 2, };
let { a: foo } = obj;
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1, 2: 2, };
",
        "
let obj = { b: 1, a: 2, };
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1, 2: 2,};
let { 2: foo } = obj;
",
        "
let obj = { b: 1, a: 2,};
let { a: foo } = obj;
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1, 'prop2': 2, };
obj['prop2'];
",
        "
let obj = { b: 1, a: 2, };
obj.a;
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1, 2: 2, };
obj[2];
",
        "
let obj = { b: 1, a: 2, };
obj.a;
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1, ['foo']: 2, };
obj['foo'];
",
        "
let obj = { b: 1, a: 2, };
obj.a;
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1, [2]: 2, };
obj[2];
",
        "
let obj = { b: 1, a: 2, };
obj.a;
",
    );
    // Big-int
    test_transform_in_fn(
        "
let obj = { prop1: 1, [123n]: 2, 123: 3, '123': 4, ['123']: 5 };
",
        "
let obj = { b: 1, a: 2, a: 3, a: 4, a: 5 };
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1, null: 2, [null]:3, 'null': 4, ['null']:5 };
obj.null;
obj[null];
obj['null'];
",
        "
let obj = { b: 1, a: 2, a:3, a: 4, a:5 };
obj.a;
obj.a;
obj.a;
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1, undefined: 2, [undefined]:3, 'undefined': 4, ['undefined']:5 };
obj.undefined;
obj[undefined];
obj['undefined'];
",
        "
let obj = { b: 1, a: 2, a:3, a: 4, a:5 };
obj.a;
obj.a;
obj.a;
",
    );
    // The identifier `undefined` doesn't always refer to the value of the same name
    test_same_in_fn(
        "
let undefined = 'something';
let obj = { prop1: 1, [undefined]:3, };
",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1, NaN: 2, [NaN]:3, 'NaN': 4, ['NaN']:5 };
obj.NaN;
obj[NaN];
obj['NaN'];
",
        "
let obj = { b: 1, a: 2, a:3, a: 4, a:5 };
obj.a;
obj.a;
obj.a;
",
    );
    // The identifier `NaN` doesn't always refer to the value of the same name
    test_same_in_fn(
        "
let NaN = 'something';
let obj = { prop1: 1, [NaN]:3 };
",
    );
    // Keys are parsed as the same number, so use the same string for indexing.
    test_transform_in_fn(
        "
let obj = { 0xF00D: 1, 0xf00D: 2, 0170015: 3, 61453: 4 };
",
        "
let obj = { a: 1, a: 2, a: 3, a: 4 };
",
    );
    // Keys are parsed as the same number, so use the same string for indexing.
    test_transform_in_fn(
        "
let obj = { 1.3333333333333339: 1, 1.333333333333334: 2 };
",
        "
let obj = { a: 1, a: 2 };
",
    );
}

#[test]
fn test() {
    // TODO: requires control flow/SSA
    //    test_transform_in_fn(
    //        "
    //let variable = {propX: 1};
    //variable.propX;
    //
    //variable = null;
    //
    //variable.propX;
    //
    //variable = {propY: 1};
    //variable.propY;
    //",
    //        "
    //let variable = {a: 1};
    //variable.a;
    //
    //variable = null;
    //
    //variable.propX;
    //
    //variable = {a: 1};
    //variable.a;
    //",
    //    );

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
    test_transform_in_fn(
        "
const obj1 = { prop1: 1 };
let v1;
let v2 = v1 = obj1;
obj1.prop1;
v1.prop1;
v2.prop1;
    ",
        "
const obj1 = { a: 1 };
let v1;
let v2 = v1 = obj1;
obj1.a;
v1.a;
v2.a;
    ",
    );
    test_transform_in_fn(
        "
({ prop1: 1 }.prop1);
    ",
        "
({ a: 1 }.a);
    ",
    );
    test_transform_in_fn(
        "
let obj1 = { prop1: 1 };
obj1.prop2 || (obj1.prop2 = 2);
    ",
        "
let obj1 = { b: 1 };
obj1.a || (obj1.a = 2);
    ",
    );
    test_transform_in_fn(
        "
let obj1 = { prop1: 1 };
obj1.self = obj1;
    ",
        "
let obj1 = { a: 1 };
obj1.b = obj1;
    ",
    );
    test_transform_in_fn(
        "
let obj1 = { prop1: { prop2: 2 } };
let obj2 = { prop3: obj1, prop4: obj1.prop1 };
    ",
        "
let obj1 = { a: { a: 2 } };
let obj2 = { a: obj1, b: obj1.a };
    ",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
cond ? ((obj = { prop2: 2 }, 1)) : null;
    ",
        "
let obj = { a: 1 };
cond ? ((obj = { a: 2 }, 1)) : null;
    ",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };
cond ? (obj = { prop2: 2 }) : null;
    ",
        "
let obj = { a: 1 };
cond ? (obj = { a: 2 }) : null;
    ",
    );
    test_transform_in_fn(
        "
let obj = { prop1: 1 };

obj = { prop1: 1, prop2: 2 };

if (cond) {
    obj = { prop3: 3, prop1: 1, };
}

obj.prop1;

if (cond) {
    if (cond) {
        obj = { prop4: 4, prop1: 1, };
    }
    obj.prop1;
    obj = { prop5: 5, prop1: 1, };
}

obj.prop1;
    ",
        "
let obj = { a: 1 };

obj = { a: 1, b: 2 };

if (cond) {
    obj = { b: 3, a: 1, };
}

obj.a;

if (cond) {
    if (cond) {
        obj = { b: 4, a: 1, };
    }
    obj.a;
    obj = { b: 5, a: 1, };
}

obj.a;
    ",
    );
    // Taken from TSC, this previously broke call resolution;
    // exact output isn't important, so long as it compiles.
    test_same(
        "
function getTypeFromTypeNode() {
    return getConditionalFlowTypeOfType();
}
function getTypeFromTypeNodeWorker(e) {
    getTypeFromTypeNode(e.type);
}
function instantiateMappedType() {
    if (a) instantiateMappedType();
    while (true) {
        var e = getTypeFromTypeNode(root.node);
        if (!e) return;
        root = e;
    }
}
",
    );
}
