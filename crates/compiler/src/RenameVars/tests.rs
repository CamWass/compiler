use global_common::{Globals, Mark, SyntaxContext, GLOBALS};

use crate::resolver::resolver;

use super::*;

fn test_transform(input: &str, expected: &str) {
    crate::testing::test_transform(
        |mut program, _| {
            GLOBALS.set(&Globals::new(), || {
                let unresolved_mark = Mark::new();
                let top_level_mark = Mark::new();

                program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

                let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

                process(&mut program, unresolved_ctxt);

                program
            })
        },
        input,
        expected,
    );
}
fn test_same(input: &str) {
    test_transform(input, input);
}

// fn test_local_var_indices(input: &str, expected: &str) {
//     crate::testing::test_transform(
//         |mut program, _| {
//             GLOBALS.set(&Globals::new(), || {
//                 let unresolved_mark = Mark::new();
//                 let top_level_mark = Mark::new();

//                 program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

//                 let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

//                 rename_debug(&mut program, unresolved_ctxt);

//                 program
//             })
//         },
//         input,
//         expected,
//     );
// }

// #[test]
// fn function_scope_creation() {
//     // Had a bug where the BlockStmt of a function would be a sub-scope of the
//     // function scope. This meant that for `var` decls in the body were being
//     // declared in the wrong scope because their scope wasn't a hoist scope.
//     // This meant the scope index was incorrectly calculated and caused an
//     // out-of-bounds index panic.
//     test_local_var_indices(
//         "
// function func() {
//     const foo = 1;
//     var bar = 2;
// }",
//         "
// function func() {
//     const L0 = 1;
//     var L1 = 2; // The bug caused this to also be renamed L0
// }",
//     )
// }

#[test]
fn test_collisions_with_unresolved() {
    test_transform(
        "
// a is a super-duper-important variable from the environment with value 5.
console.log(a); // should print 5, don't want to rename `foo` to `a`.
function foo() {}
",
        "
console.log(a);
function b() {}
",
    );
}

#[test]
fn unresolved_binding_ident() {
    // Had a bug where the binding ident `ts` in `ts = {}` was creating a slot,
    // even though `ts` is unresolved.
    test_transform(
        "(function (ts) {}(ts || (ts = {})));",
        "(function (a) {}(ts || (ts = {})));",
    )
}

// Tests from closure:

#[test]
fn testRenameSimple() {
    test_transform(
        "function Foo(v1, v2) {return v1;} Foo();",
        "function b(a, c) {return a;} b();",
    );
}

#[test]
fn testRenameGlobals() {
    test_transform(
        "var Foo; var Bar, y; function x() { Bar++; }",
        "var b; var a, c; function d() { a++; }",
    );
}

#[test]
fn testRenameLocals() {
    test_transform(
        "(function (v1, v2) {}); (function (v3, v4) {});",
        "(function (a, b) {}); (function (a, b) {});",
    );
    test_transform(
        "function f1(v1, v2) {}; function f2(v3, v4) {};",
        "function c(a, b) {}; function d(a, b) {};",
    );
}

#[test]
fn testRenameLocals_let() {
    test_transform(
        "(function () { let var1 = 0; let another = 1; });",
        "(function () { let a = 0; let b = 1; });",
    );
}

#[test]
fn testRenameLocals_const() {
    test_transform(
        "(function () { const var1 = 0; const another = 1; });",
        "(function () { const a = 0; const b = 1; });",
    );
}

#[test]
fn testRenameParamsWithLeadingUnderscores() {
    test_transform("(function (_v1, _v2) {});", "(function (a, b) {});");
}

#[test]
fn testRenameRedeclaredGlobals() {
    test_transform(
        "
function f1(v1, v2) {f1()};
function f1(v3, v4) {f1()};",
        "
function a(b, c) {a()};
function a(b, c) {a()};",
    );

    // TODO:

    //   localRenamingOnly = true;

    //   test_transform(
    //       lines(
    //           "function f1(v1, v2) {f1()};",
    //           "/** @suppress {duplicate} */",
    //           "function f1(v3, v4) {f1()};"),
    //       lines(
    //           "function f1(a, b) {f1()};",
    //           "/** @suppress {duplicate} */",
    //           "function f1(a, b) {f1()};"));
}

// TODO:
// #[test]
// fn testRecursiveFunctions1() {
//     test_transform(
//         "
// var walk = function walk(node, aFunction) {
//     walk(node, aFunction);
// };",
//         "
// var a = function a(b, c) {
//     a(b, c);
// };",
//     );

// //     //   localRenamingOnly = true;

// //     //   test_transform("var walk = function walk(node, aFunction) {" +
// //     //        "  walk(node, aFunction);" +
// //     //        "};",
// //     //        "var walk = function walk(a, b) {" +
// //     //        "  walk(a, b);" +
// //     //        "};");
// // }

#[test]
fn testRenameLocalsClashingWithGlobals() {
    test_transform(
        "function a(v1, v2) {return v1;} a();",
        "function b(a, c) {return a;} b();",
    );
}

#[test]
fn testRenameNested() {
    test_transform(
        "function f1(v1, v2) { (function(v3, v4) {}) }",
        "function e(a, b) { (function(c, d) {}) }",
    );
    test_transform(
        "function f1(v1, v2) { function f2(v3, v4) {} }",
        "function f(a, b) { function c(d, e) {} }",
    );
}

// TODO:
// #[test]
// fn testBleedingRecursiveFunctions1() {
//   // On IE, bleeding functions will interfere with each other if
//   // they are in the same scope. In the below example, we want to be
//   // sure that a and b get separate names.
//   test_transform("var x = function a(x) { return x ? 1 : a(1); };" +
//        "var y = function b(x) { return x ? 2 : b(2); };",
//        "var c = function b(a) { return a ? 1 : b(1); };" +
//        "var e = function d(a) { return a ? 2 : d(2); };");
// }

// TODO:
// #[test]
// fn testBleedingRecursiveFunctions2() {
//   test_transform(
//       lines(
//           "function f() {",
//           "  var x = function a(x) { return x ? 1 : a(1); };",
//           "  var y = function b(x) { return x ? 2 : b(2); };",
//           "}"),
//       lines(
//           "function d() {",
//           "  var e = function a(b) { return b ? 1 : a(1); };",
//           "  var f = function c(a) { return a ? 2 : c(2); };",
//           "}"));
// }

// TODO:
// #[test]
// fn testBleedingRecursiveFunctions3() {
//   test_transform(
//       lines(
//           "function f() {",
//           "  var x = function a(x) { return x ? 1 : a(1); };",
//           "  var y = function b(x) { return x ? 2 : b(2); };",
//           "  var z = function c(x) { return x ? y : c(2); };",
//           "}"),
//       lines(
//           "function f() {",
//           "  var g = function a(c) { return c ? 1 : a(1); };",
//           "  var d = function b(a) { return a ? 2 : b(2); };",
//           "  var h = function e(b) { return b ? d : e(2); };",
//           "}"));
// }

#[test]
fn testBleedingFunctionInBlocks() {
    test_transform(
        "
if (true) {
    var x = function a(x) {return x;}
}",
        "
if (true) {
    var c = function b(a) {return a;}
}",
    );
}

#[test]
fn testDoNotRenameArguments() {
    test_same("function a() { arguments; }");
}

#[test]
fn testRenameWithNameOverlap() {
    test_transform(
        "var a = 1; var b = 2; b + b;",
        "var b = 1; var a = 2; a + a;",
    );
}

#[test]
fn testNamingBasedOnOrderOfOccurrence() {
    test_transform(
        "
var q,p,m,n,l,k; try { } catch(r) {try {} catch(s) {}}; var t = q + q;",
        "
var a,d,e,f,g,h; try { } catch(b) {try {} catch(c) {}}; var i = a + a;",
    );
    test_transform("
(function(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,$){});
var a4,a3,a2,a1,b4,b3,b2,b1,ab,ac,ad,fg;
function foo(){};",
       "
(function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,$){});
var aa,ba,ca,da,ea,fa,ga,ha,ia,ja,ka,la;
function ma(){};");
}

#[test]
fn testTryCatchLifeTime() {
    test_transform(
        "
var q,p,m,n,l,k; (function (r) {}); try { } catch(s) {}; var t = q + q;",
        "
var a,c,d,e,f,g; (function (b) {}); try { } catch(b) {}; var h = a + a;",
    );

    test_transform(
        "try {try {} catch(p) {}} catch(s) {};",
        "try {try {} catch(a) {}} catch(a) {};",
    );

    test_transform(
        "
try {
    try { 
    } catch(p) {
        try { 
        } catch(r) {}
    }
} catch(s) {
    try { 
    } catch(q) {}
};",
        "
try {
    try { 
    } catch(a) {
        try { 
        } catch(b) {}
    }
} catch(a) {
    try {
    } catch(b) {}
};",
    );
}

#[test]
fn testArrowFunctions() {
    test_transform("foo => {return foo + 3;}", "a => {return a + 3;}");

    test_transform(
        "(foo, bar) => {return foo + bar + 3;}",
        "(a, b) => {return a + b + 3;}",
    );
}

#[test]
fn testClasses() {
    test_transform("class fooBar {}", "class a {}");

    test_transform(
        "
class fooBar {
    constructor(foo, bar) {
        this.foo = foo;
        this.bar = bar;
    }
}
var x = new fooBar(2, 3);",
        "
class c {
    constructor(a, b) {
        this.foo = a;
        this.bar = b;
    }
}
var d = new c(2, 3);",
    );

    test_transform(
        "
class fooBar {
    constructor(foo, bar) {
        this.foo = foo;
        this.bar = bar;
    }
    func(x) {
        return this.foo + x;
    }
}
var x = new fooBar(2,3);
var abcd = x.func(5);",
        "
class c {
    constructor(a, b) {
        this.foo = a;
        this.bar = b;
    }
    func(a) {
        return this.foo + a;
    }
}
var d = new c(2,3);
var e = d.func(5);",
    );
}

#[test]
fn testLetConst() {
    test_transform("let xyz;", "let a;");

    test_transform("const xyz = 1;", "const a = 1");

    test_transform(
        "
let zyx = 1; {
    const xyz = 1;
    let zyx = 2;
    zyx = 3;
}
let xyz = 'potato';
zyx = 4;",
        "
let b = 1; {
    const c = 1;
    let a = 2;
    a = 3;
}
let d = 'potato';
b = 4;",
    );
}

#[test]
fn testGenerators() {
    test_transform(
        "
function* gen() {
    var xyz = 3;
    yield xyz + 4;
}
gen().next()",
        "
function* b() {
    var a = 3;
    yield a + 4;
}
b().next()",
    );
}

#[test]
fn testForOf() {
    test_transform("for (var item of items) {}", "for (var a of items) {}");
}

#[test]
fn testTemplateStrings() {
    test_transform(
        "var name = 'Foo';
          `My name is ${name}`;",
        "var a = 'Foo';
          `My name is ${a}`;",
    );
}

#[test]
fn testArrayDestructuring() {
    test_transform("var [x, y, z] = [1, 2, 3];", "var [a, b, c] = [1, 2, 3];");
}

#[test]
fn testObjectDestructuring() {
    // TODO: Teach RenameVars to take advantage of shorthand properties by
    // building up a Map from var name strings to property name multisets.  We
    // should be able to treat this similar to the "previous names" map, where
    // we preferentially pick names with the most lined-up properties, provided
    // the property names are short (should be easy enough to do the math).
    // Note, the same property name could get different var names in different
    // scopes, so we probably need to do the comparison per scope.
    // Also, this is only relevant if language_out >= ES6.
    test_transform(
        "
var obj = {p: 5, h: false};
var {p, h} = obj;",
        "
var a = {p: 5, h: false};
var {p: b, h: c} = a;",
    );

    test_transform(
        "
var obj = {p: 5, h: false};
var {p: x, h: y} = obj;",
        "
var a = {p: 5, h: false};
var {p: b, h: c} = a;",
    );
}

#[test]
fn testDefaultFunction() {
    test_transform(
        "
function f(x, y=12) {
    return x * y;
}",
        "
function c(a, b=12) {
    return a * b;
}",
    );
}

#[test]
fn testRestFunction() {
    test_transform(
        "
function f(x, ...y) {
    return x * y[0];
}",
        "
function c(a, ...b) {
    return a * b[0];
}",
    );
}

#[test]
fn testObjectLiterals() {
    test_transform(
        "
var objSuper = {
    f: 'potato'
};
var obj = {
    __proto__: objSuper,
    g: false,
    x() {
        return super.f;
    }
};
obj.x();",
        "
var a = {
    f: 'potato'
};
var b = {
    __proto__: a,
    g: false,
    x() {
        return super.f;
    }
};
b.x();",
    );
}

// TODO:
// #[test]
// fn testImport1() {
//   ignoreWarnings(LOAD_WARNING);
//   test_transform("import name from './other.js'; use(name);", "import a from './other.js'; use(a);");

//   test_transform(
//       "import * as name from './other.js'; use(name);",
//       "import * as a from './other.js'; use(a);");

//   test_transform(
//       "import {default as name} from './other.js'; use(name);",
//       "import {default as a} from './other.js'; use(a);");
// }

// TODO:
// #[test]
// fn testImport2() {
//   ignoreWarnings(LOAD_WARNING);
//   withNormalize = true;
//   test_transform(
//       "import {name} from './other.js'; use(name);",
//       "import {name as a} from './other.js'; use(a);");
// }
