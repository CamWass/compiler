use global_common::{Globals, Mark, SyntaxContext, GLOBALS};

use crate::resolver::resolver;

use super::*;

fn test_transform(input: &str, expected: &str) {
    crate::testing::test_transform(
        |mut program, mut program_data| {
            GLOBALS.set(&Globals::new(), || {
                let unresolved_mark = Mark::new();
                let top_level_mark = Mark::new();

                crate::normalize_properties::normalize_properties(&mut program, &mut program_data);

                program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

                let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

                rename_vars(&mut program, unresolved_ctxt);

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

fn test_local_var_indices(input: &str, expected: &str) {
    crate::testing::test_transform(
        |mut program, mut program_data| {
            GLOBALS.set(&Globals::new(), || {
                let unresolved_mark = Mark::new();
                let top_level_mark = Mark::new();

                crate::normalize_properties::normalize_properties(&mut program, &mut program_data);

                program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

                let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

                rename_debug(&mut program, unresolved_ctxt);

                program
            })
        },
        input,
        expected,
    );
}

#[test]
fn function_scope_creation() {
    // Had a bug where the BlockStmt of a function would be a sub-scope of the
    // function scope. This meant that for `var` decls in the body were being
    // declared in the wrong scope because their scope wasn't a hoist scope.
    // This meant the scope index was incorrectly calculated and caused an
    // out-of-bounds index panic.
    test_local_var_indices(
        "
function func() {
    const foo = 1;
    var bar = 2;
}",
        "
function func() {
    const L0 = 1;
    var L1 = 2; // The bug caused this to also be renamed L0
}",
    )
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
        "function a(b, c) {return b;} a();",
    );
}

#[test]
fn testRenameGlobals() {
    test_transform(
        "var Foo; var Bar, y; function x() { Bar++; }",
        "var a; var b, c; function d() { b++; }",
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

// TODO:
// #[test]
// fn testRenameLocalsToSame() {
//   preferStableNames = true;
//   test_same("(function(a) {})");
//   test_same("(function(a, b) {})");
//   test_same("(function(a, b, c) {})");
//   test_same("(function() { var a; })");
//   test_same("(function() { var a, b; })");
//   test_same("(function() { var a, b, c; })");
// }

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
        "function a(b, c) {return b;} a();",
    );
}

#[test]
fn testRenameNested() {
    test_transform(
        "function f1(v1, v2) { (function(v3, v4) {}) }",
        "function a(b, c) { (function(d, e) {}) }",
    );
    test_transform(
        "function f1(v1, v2) { function f2(v3, v4) {} }",
        "function a(b, c) { function d(e, f) {} }",
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
    var b = function c(a) {return a;}
}",
    );
}

// TODO:
// #[test]
// fn testRenameWithExterns1() {
//   String externs = "var foo;";
//   test_transform(
//       externs(externs),
//       srcs("var bar; foo(bar);"),
//       expected("var a; foo(a);"));
// }

// TODO:
// #[test]
// fn testRenameWithExterns2() {
//   String externs = "var a;";
//   test_transform(
//       externs(externs),
//       srcs("var b = 5"),
//       expected("var b = 5"));
// }

// TODO:
// #[test]
// fn testDoNotRenameExportedName() {
//   test_same("_foo()");
// }

#[test]
fn testDoNotRenameArguments() {
    test_same("function a() { arguments; }");
}

#[test]
fn testRenameWithNameOverlap() {
    test_same("var a = 1; var b = 2; b + b;");
}

// TODO:
// #[test]
// fn testRenameWithPrefix1() {
//   prefix = "PRE_";
//   test_transform("function Foo(v1, v2) {return v1} Foo();",
//       "function PRE_(a, b) {return a} PRE_();");
//   prefix = DEFAULT_PREFIX;

// }

// TODO:
// #[test]
// fn testRenameWithPrefix2() {
//   prefix = "PRE_";
//   test_transform("function Foo(v1, v2) {var v3 = v1 + v2; return v3;} Foo();",
//       "function PRE_(a, b) {var c = a + b; return c;} PRE_();");
//   prefix = DEFAULT_PREFIX;
// }

// TODO:
// #[test]
// fn testRenameWithPrefix3() {
//   prefix = "a";
//   test_transform("function Foo() {return 1;}" +
//        "function Bar() {" +
//        "  var a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z," +
//        "      A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,aa,ab;" +
//        "  Foo();" +
//        "} Bar();",

//       "function a() {return 1;}" +
//        "function aa() {" +
//        "  var b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A," +
//        "      B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,$,ba,ca;" +
//        "  a();" +
//        "} aa();");
//   prefix = DEFAULT_PREFIX;
// }

#[test]
fn testNamingBasedOnOrderOfOccurrence() {
    test_transform(
        "
var q,p,m,n,l,k; try { } catch(r) {try {} catch(s) {}}; var t = q + q;",
        "
var a,b,c,d,e,f; try { } catch(g) {try {} catch(h) {}}; var i = a + a;",
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

// TODO:
// #[test]
// fn testStableRenameSimple() {
//   VariableMap expectedVariableMap = makeVariableMap(
//       "Foo", "a", "L 0", "b", "L 1", "c");
//   testRenameMap("function Foo(v1, v2) {return v1;} Foo();",
//                 "function a(b, c) {return b;} a();", expectedVariableMap);

//   expectedVariableMap = makeVariableMap(
//       "Foo", "a", "L 0", "b", "L 1", "c", "L 2", "d");
//   testRenameMapUsingOldMap("function Foo(v1, v2, v3) {return v1;} Foo();",
//        "function a(b, c, d) {return b;} a();", expectedVariableMap);
// }

// TODO:
// #[test]
// fn testStableRenameGlobals() {
//   VariableMap expectedVariableMap = makeVariableMap(
//       "Foo", "a", "Bar", "b", "y", "c", "x", "d");
//   testRenameMap("var Foo; var Bar, y; function x() { Bar++; }",
//                 "var a; var b, c; function d() { b++; }",
//                 expectedVariableMap);

//   expectedVariableMap = makeVariableMap(
//       "Foo", "a", "Bar", "b", "y", "c", "x", "d", "Baz", "f", "L 0" , "e");
//   testRenameMapUsingOldMap(
//       "var Foo, Baz; var Bar, y; function x(R) { return R + Bar++; }",
//       "var a, f; var b, c; function d(e) { return e + b++; }",
//       expectedVariableMap);
// }

// TODO:
// #[test]
// fn testStableRenameWithPointlesslyAnonymousFunctions() {
//   VariableMap expectedVariableMap = makeVariableMap("L 0", "a", "L 1", "b");
//   testRenameMap("(function (v1, v2) {}); (function (v3, v4) {});",
//                 "(function (a, b) {}); (function (a, b) {});",
//                 expectedVariableMap);

//   expectedVariableMap = makeVariableMap("L 0", "a", "L 1", "b", "L 2", "c");
//   testRenameMapUsingOldMap("(function (v0, v1, v2) {});" +
//                            "(function (v3, v4) {});",
//                            "(function (a, b, c) {});" +
//                            "(function (a, b) {});",
//                            expectedVariableMap);
// }

// TODO:
// #[test]
// fn testStableRenameLocalsClashingWithGlobals() {
//   test_transform("function a(v1, v2) {return v1;} a();",
//        "function a(b, c) {return b;} a();");
//   previouslyUsedMap = renameVars.getVariableMap();
//   test_transform("function bar(){return;}function a(v1, v2) {return v1;} a();",
//        "function d(){return;}function a(b, c) {return b;} a();");
// }

// TODO:
// #[test]
// fn testStableRenameNested() {
//   VariableMap expectedVariableMap = makeVariableMap(
//       "f1", "a", "L 0", "b", "L 1", "c", "L 2", "d", "L 3", "e");
//   testRenameMap("function f1(v1, v2) { (function(v3, v4) {}) }",
//                 "function a(b, c) { (function(d, e) {}) }",
//                 expectedVariableMap);

//   expectedVariableMap = makeVariableMap(
//       "f1", "a", "L 0", "b", "L 1", "c", "L 2", "d", "L 3", "e", "L 4", "f");
//   testRenameMapUsingOldMap(
//       "function f1(v1, v2) { (function(v3, v4, v5) {}) }",
//       "function a(b, c) { (function(d, e, f) {}) }",
//       expectedVariableMap);
// }

// TODO:
// #[test]
// fn testStableRenameWithExterns1() {
//   String externs = "var foo;";
//   test_transform(
//       externs(externs),
//       srcs("var bar; foo(bar);"),
//       expected("var a; foo(a);"));
//   previouslyUsedMap = renameVars.getVariableMap();
//   test_transform(
//       externs(externs),
//       srcs("var bar, baz; foo(bar, baz);"),
//       expected("var a, b; foo(a, b);"));
// }

// TODO:
// #[test]
// fn testStableRenameWithExterns2() {
//   String externs = "var a;";
//   test_transform(
//       externs(externs),
//       srcs("var b = 5"),
//       expected("var b = 5"));
//   previouslyUsedMap = renameVars.getVariableMap();
//   test_transform(
//       externs(externs),
//       srcs("var b = 5, catty = 9;"),
//       expected("var b = 5, c=9;"));
// }

// TODO:
// #[test]
// fn testStableRenameWithNameOverlap() {
//   test_same("var a = 1; var b = 2; b + b;");
//   previouslyUsedMap = renameVars.getVariableMap();
//   test_same("var a = 1; var c, b = 2; b + b;");
// }

// TODO:
// #[test]
// fn testStableRenameWithAnonymousFunctions() {
//   VariableMap expectedVariableMap = makeVariableMap("L 0", "a", "foo", "b");
//   testRenameMap("function foo(bar){return bar;}foo(function(h){return h;});",
//                 "function b(a){return a}b(function(a){return a;})",
//                 expectedVariableMap);

//   expectedVariableMap = makeVariableMap("foo", "b", "L 0", "a", "L 1", "c");
//   testRenameMapUsingOldMap(
//       "function foo(bar) {return bar;}foo(function(g,h) {return g+h;});",
//       "function b(a){return a}b(function(a,c){return a+c;})",
//       expectedVariableMap);
// }

// TODO:
// #[test]
// fn testStableRenameSimpleExternsChanges() {
//   VariableMap expectedVariableMap = makeVariableMap(
//       "Foo", "a", "L 0", "b", "L 1", "c");
//   testRenameMap("function Foo(v1, v2) {return v1;} Foo();",
//                 "function a(b, c) {return b;} a();", expectedVariableMap);

//   expectedVariableMap = makeVariableMap("L 0", "b", "L 1", "c", "L 2", "a");
//   String externs = "var Foo;";
//   testRenameMapUsingOldMap(externs,
//                            "function Foo(v1, v2, v0) {return v1;} Foo();",
//                            "function Foo(b, c, a) {return b;} Foo();",
//                            expectedVariableMap);
// }

// TODO:
// #[test]
// fn testStableRenameSimpleLocalNameExterned() {
//   test_transform("function Foo(v1, v2) {return v1;} Foo();",
//        "function a(b, c) {return b;} a();");

//   previouslyUsedMap = renameVars.getVariableMap();

//   String externs = "var b;";
//   test_transform(
//       externs(externs),
//       srcs("function Foo(v1, v2) {return v1;} Foo(b);"),
//       expected("function a(d, c) {return d;} a(b);"));
// }

// TODO:
// #[test]
// fn testStableRenameSimpleGlobalNameExterned() {
//   test_transform("function Foo(v1, v2) {return v1;} Foo();",
//        "function a(b, c) {return b;} a();");

//   previouslyUsedMap = renameVars.getVariableMap();

//   String externs = "var Foo;";
//   test_transform(
//       externs(externs),
//       srcs("function Foo(v1, v2, v0) {return v1;} Foo();"),
//       expected("function Foo(b, c, a) {return b;} Foo();"));
// }

// TODO:
// #[test]
// fn testStableRenameWithPrefix1AndUnstableLocalNames() {
//   prefix = "PRE_";
//   test_transform("function Foo(v1, v2) {return v1} Foo();",
//        "function PRE_(a, b) {return a} PRE_();");

//   previouslyUsedMap = renameVars.getVariableMap();

//   prefix = "PRE_";
//   test_transform("function Foo(v0, v1, v2) {return v1} Foo();",
//        "function PRE_(a, b, c) {return b} PRE_();");
// }

// TODO:
// #[test]
// fn testStableRenameWithPrefix2() {
//   prefix = "a";
//   test_transform("function Foo() {return 1;}" +
//        "function Bar() {" +
//        "  var a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z," +
//        "      A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,aa,ab;" +
//        "  Foo();" +
//        "} Bar();",

//        "function a() {return 1;}" +
//        "function aa() {" +
//        "  var b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A," +
//        "      B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,$,ba,ca;" +
//        "  a();" +
//        "} aa();");

//   previouslyUsedMap = renameVars.getVariableMap();

//   prefix = "a";
//   test_transform("function Foo() {return 1;}" +
//        "function Baz() {return 1;}" +
//        "function Bar() {" +
//        "  var a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z," +
//        "      A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,aa,ab;" +
//        "  Foo();" +
//        "} Bar();",

//        "function a() {return 1;}" +
//        "function ab() {return 1;}" +
//        "function aa() {" +
//        "  var b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A," +
//        "      B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,$,ba,ca;" +
//        "  a();" +
//        "} aa();");
// }

// TODO:
// #[test]
// fn testContrivedExampleWhereConsistentRenamingIsWorse() {
//   previouslyUsedMap = makeVariableMap(
//       "Foo", "LongString", "L 0", "b", "L 1", "c");

//   test_transform("function Foo(v1, v2) {return v1;} Foo();",
//        "function LongString(b, c) {return b;} LongString();");

//   previouslyUsedMap = renameVars.getVariableMap();
//   VariableMap expectedVariableMap = makeVariableMap(
//       "Foo", "LongString", "L 0", "b", "L 1", "c");
//   assertVariableMapsEqual(expectedVariableMap, previouslyUsedMap);
// }

// TODO:
// #[test]
// fn testPrevUsedMapWithDuplicates() {
//   try {
//     makeVariableMap("Foo", "z", "Bar", "z");
//     test_same("");
//     throw new AssertionError();
//   } catch (java.lang.IllegalArgumentException expected) {
//   }
// }

// TODO:
// #[test]
// fn testExportSimpleSymbolReservesName() {
//   test_transform("var goog, x; goog.exportSymbol('a', x);",
//        "var a, b; a.exportSymbol('a', b);");
//   withClosurePass = true;
//   test_transform("var goog, x; goog.exportSymbol('a', x);",
//        "var b, c; b.exportSymbol('a', c);");
// }

// TODO:
// #[test]
// fn testExportComplexSymbolReservesName() {
//   test_transform("var goog, x; goog.exportSymbol('a.b', x);",
//        "var a, b; a.exportSymbol('a.b', b);");
//   withClosurePass = true;
//   test_transform("var goog, x; goog.exportSymbol('a.b', x);",
//        "var b, c; b.exportSymbol('a.b', c);");
// }

// TODO:
// #[test]
// fn testExportToNonStringDoesntExplode() {
//   withClosurePass = true;
//   test_transform("var goog, a, b; goog.exportSymbol(a, b);",
//        "var a, b, c; a.exportSymbol(b, c);");
// }

// TODO:
// #[test]
// fn testDollarSignSuperExport1() {
//   useGoogleCodingConvention = false;
//   // See http://blickly.github.io/closure-compiler-issues/#32
//   test_transform("var x = function($super,duper,$fantastic){}",
//        "var c = function($super,    a,        b){}");

//   localRenamingOnly = false;
//   test_transform("var $super = 1", "var a = 1");

//   useGoogleCodingConvention = true;
//   test_transform("var x = function($super,duper,$fantastic){}",
//        "var c = function($super,a,b){}");
// }

// TODO:
// #[test]
// fn testDollarSignSuperExport2() {
//   withNormalize = true;

//   useGoogleCodingConvention = false;
//   // See http://blickly.github.io/closure-compiler-issues/#32
//   test_transform("var x = function($super,duper,$fantastic){};" +
//           "var y = function($super,duper){};",
//        "var c = function($super,    a,         b){};" +
//           "var d = function($super,    a){};");

//   localRenamingOnly = false;
//   test_transform("var $super = 1", "var a = 1");

//   useGoogleCodingConvention = true;
//   test_transform("var x = function($super,duper,$fantastic){};" +
//           "var y = function($super,duper){};",
//        "var c = function($super,   a,    b         ){};" +
//           "var d = function($super,a){};");
// }

// TODO:
// #[test]
// fn testBias() {
//   nameGenerator = new DefaultNameGenerator(new HashSet<String>(), "", null);
//   nameGenerator.favors("AAAAAAAAHH");
//   test_transform("var x, y", "var A, H");
// }

// TODO:
// #[test]
// fn testPseudoNames() {
//   generatePseudoNames = false;
//   // See http://blickly.github.io/closure-compiler-issues/#32
//   test_transform("var foo = function(a, b, c){}",
//        "var d = function(a, b, c){}");

//   generatePseudoNames = true;
//   test_transform("var foo = function(a, b, c){}",
//        "var $foo$$ = function($a$$, $b$$, $c$$){}");

//   test_transform("var a = function(a, b, c){}",
//        "var $a$$ = function($a$$, $b$$, $c$$){}");
// }

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
class a {
    constructor(b, c) {
        this.foo = b;
        this.bar = c;
    }
}
var d = new a(2, 3);",
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
class b {
    constructor(a, c) {
        this.foo = a;
        this.bar = c;
    }
    func(a) {
        return this.foo + a;
    }
}
var d = new b(2,3);
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
let a = 1; {
    const c = 1;
    let b = 2;
    b = 3;
}
let d = 'potato';
a = 4;",
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
function* a() {
    var b = 3;
    yield b + 4;
}
a().next()",
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
    // TODO(sdh): Teach RenameVars to take advantage of shorthand properties by
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

// fn testRenameMapUsingOldMap(String input, String expected,
//                                       VariableMap expectedMap) {
//   previouslyUsedMap = renameVars.getVariableMap();
//   testRenameMap("", input, expected, expectedMap);
// }

// fn testRenameMapUsingOldMap(String externs, String input,
//                                       String expected,
//                                       VariableMap expectedMap) {
//   previouslyUsedMap = renameVars.getVariableMap();
//   testRenameMap(externs, input, expected, expectedMap);
// }

// fn testRenameMap(String input, String expected,
//                            VariableMap expectedRenameMap) {
//   testRenameMap("", input, expected, expectedRenameMap);
// }

// fn testRenameMap(String externs, String input, String expected,
//                            VariableMap expectedRenameMap) {
//   test_transform(
//       externs(externs),
//       srcs(input),
//       expected(expected));
//   VariableMap renameMap = renameVars.getVariableMap();
//   assertVariableMapsEqual(expectedRenameMap, renameMap);
// }

// TODO:
// #[test]
// fn testPreferStableNames() {
//   preferStableNames = true;
//   // Locals in scopes with too many local variables (>1000) should
//   // not receive temporary names (eg, 'L 123').  These locals will
//   // appear in the name maps with the same name as in the code (eg,
//   // 'a0' in this case).
//   test_transform(createManyVarFunction(1000), null);
//   assertThat(renameVars.getVariableMap().lookupNewName("a0")).isNull();
//   assertThat(renameVars.getVariableMap().lookupNewName("L 0")).isEqualTo("b");
//   test_transform(createManyVarFunction(1001), null);
//   assertThat(renameVars.getVariableMap().lookupNewName("a0")).isEqualTo("b");
//   assertThat(renameVars.getVariableMap().lookupNewName("L 0")).isNull();

//   // With {@code preferStableNames} off locals should
//   // unconditionally receive temporary names.
//   preferStableNames = false;
//   test_transform(createManyVarFunction(1000), null);
//   assertThat(renameVars.getVariableMap().lookupNewName("a0")).isNull();
//   assertThat(renameVars.getVariableMap().lookupNewName("L 0")).isEqualTo("b");
//   test_transform(createManyVarFunction(1001), null);
//   assertThat(renameVars.getVariableMap().lookupNewName("a0")).isNull();
//   assertThat(renameVars.getVariableMap().lookupNewName("L 0")).isEqualTo("b");
// }

// private static String createManyVarFunction(int numVars) {
//   List<String> locals = new ArrayList<>();
//   for (int i = 0; i < numVars; i++) {
//     locals.add("a" + i);
//   }
//   return "function foo() { var " + Joiner.on(",").join(locals) + "; }";
// }

// private VariableMap makeVariableMap(String... keyValPairs) {
//   checkArgument(keyValPairs.length % 2 == 0);

//   ImmutableMap.Builder<String, String> renameMap = ImmutableMap.builder();
//   for (int i = 0; i < keyValPairs.length; i += 2) {
//     renameMap.put(keyValPairs[i], keyValPairs[i + 1]);
//   }

//   return new VariableMap(renameMap.buildOrThrow());
// }

// private static void assertVariableMapsEqual(VariableMap a, VariableMap b) {
//   Map<String, String> ma = a.getOriginalNameToNewNameMap();
//   Map<String, String> mb = b.getOriginalNameToNewNameMap();
//   assertWithMessage("VariableMaps not equal").that(mb).isEqualTo(ma);
// }
