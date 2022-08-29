use std::fmt::Write;

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

                crate::normalize::normalize(&mut program, &mut node_id_gen);

                let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

                coalesce_variable_names(&mut program, unresolved_ctxt, &mut node_id_gen);

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

// TODO: spacing/indentation of tests

#[test]
fn testSimple() {
    inFunction(
        "
    var x; var y; x=1; x; y=1; y; return y",
        "
    var x;        x=1; x; x=1; x; return x",
    );

    inFunction(
        "
    var x,y; x=1; x; y=1; y",
        "
    var x  ; x=1; x; x=1; x",
    );

    inFunctionSame("var x; var y; x=1; y=2; y; x");

    inFunction(
        "
    y=0; var x, y; y; x=0; x",
        "
    y=0; var y   ; y; y=0;y",
    );

    inFunction(
        "
    var x,y; x=1; y=x; y",
        "
    var x  ; x=1; x=x; x",
    );

    inFunction(
        "
    var x,y; x=1; y=x+1; y",
        "
    var x  ; x=1; x=x+1; x",
    );

    inFunction(
        "
    x=1; x; y=2; y; var x; var y",
        "
    x=1; x; x=2; x; var x",
    );

    inFunction(
        "
    var x=1; var y=x+1; return y",
        "
    var x=1;     x=x+1; return x",
    );

    inFunctionSame("var x=1; var y=0; x = x + 1; y");

    inFunction(
        "
    var x=1; x+=1;     var y=0; y",
        "
    var x=1; x = x + 1;     x=0; x",
    );

    inFunction(
        "
    var x=1; foo(bar(x+=1));     var y=0; y",
        "
    var x=1; foo(bar(x = x + 1));    x=0; x",
    );

    inFunctionSame("var y; var x=1; f(x = x + 1, y)");

    inFunctionSame("var x; var y; y = y + 1, y, x = 1; x");
}

#[test]
fn testCoaleseLetAndConst() {
    inFunction(
        "let x; const y = 1; x = y + 1; return x",
        // `let` must become `var`, because we might be coalescing
        // variables declared in different blocks.
        // See testLetAndConstDifferentBlock()
        "var x;       x = 1; x = x + 1; return x",
    );
}

#[test]
fn testLetAndConstDifferentBlock() {
    inFunction(
        "if(1) { const x = 0; x } else { let y = 0; y }",
        "if(1) {   var x = 0; x } else {     x = 0; x }",
    );
}

#[test]
fn testCoalesceLetRequiresInitWithinALoop() {
    inFunction(
        "
for (let i = 0; i < 3; ++i) {
    let something;
    if (i == 0) {
        const x = 'hi';
        alert(x);
        something = x + ' there';
    }
    alert(something);
}",
        "
for (let i = 0; i < 3; ++i) {
    // we must initialize `something` on each loop iteration
    var something = void 0;
    if (i == 0) {
        something = 'hi';
        alert(something); // always alerts 'hi'
        something = something + ' there';
    }
    alert(something);
}",
    );
}

#[test]
fn testMergeThreeVarNames() {
    inFunction(
        "var x,y,z; x=1; x; y=1; y; z=1; z",
        "var x    ; x=1; x; x=1; x; x=1; x",
    );
}

#[test]
fn testDifferentBlock() {
    inFunction(
        "if(1) { var x = 0; x } else { var y = 0; y }",
        "if(1) { var x = 0; x } else {     x = 0; x }",
    );
}

#[test]
fn testLoops() {
    inFunctionSame("var x; for ( ; 1; ) { x; x = 1; var y = 1; y }");

    inFunction(
        "var y = 1; y; for ( ; 1; ) { var x = 1; x }",
        "var y = 1; y; for ( ; 1; ) {     y = 1; y }",
    );
}

#[test]
fn testEscaped() {
    inFunctionSame("function f() { x } var x = 1; x; var y = 0; y; f()");
}

#[test]
fn testFor() {
    inFunction(
        "
        var x = 1; x; for (;;) { var y } y = 1; y",
        "
        var x = 1; x; for (;;) {       } x = 1; x",
    );
}

#[test]
fn testForIn() {
    // We lose some precision here, unless we have "branched-backward-dataflow".
    inFunctionSame("var x = 1; var k; x; var y; for ( y in k ) { y }");

    inFunction(
        "var x = 1,     k; x; y = 1; for (var y in k) { y }",
        "var x = 1; var k; x; x = 1; for (    x in k) { x }",
    );

    inFunctionSame("function f(param){ var foo; for([foo] in arr) {} param }");
}

#[test]
fn testForLoopCoalesceWithFollowingCode() {
    inFunction(
        "for (;;) { const a = 3; } const y = 1; y;",
        "for (;;) { var   a = 3; }       a = 1; a;",
    );
    inFunction(
        "for (let a = 3;;) { a; } const y = 1; y;",
        "for (var a = 3;;) { a; }       a = 1; a;",
    );
    inFunction(
        "for (const x in k) { x; } const y = 1; y;",
        "for (var   x in k) { x; }       x = 1; x;",
    );
    inFunction(
        "for (let x in k) { x; } const y = 1; y;",
        "for (var x in k) { x; }       x = 1; x;",
    );
    inFunction(
        "for (const x of k) { x; } const y = 1; y;",
        "for (var   x of k) { x; }       x = 1; x;",
    );
    inFunction(
        "for (let x of k) { x; } const y = 1; y;",
        "for (var x of k) { x; }       x = 1; x;",
    );
}

#[test]
fn testForOf() {
    // We lose some precision here, unless we have "branched-backward-dataflow".
    inFunctionSame("var x = 1; var k; x; var y; for ( y of k ) { y }");

    inFunction(
        "var x = 1,     k; x; y = 1; for (var y of k) { y }",
        "var x = 1; var k; x; x = 1; for (    x of k) { x }",
    );

    inFunctionSame("function f(param){ var foo; for([foo] of arr) {} param }");
}

// TODO:
// #[test]
// fn testLoopInductionVar() {
//   inFunction(
//       "for(var x = 0; x < 10; x++){}"
//           + "for(var y = 0; y < 10; y++){}"
//           + "for(var z = 0; z < 10; z++){}",
//       "var x=0;" + "for(;x<10;x++);" + "x=0;" + "for(;x<10;x++);" + "x=0;" + "for(;x<10;x++) {}");

//   inFunction(
//       "for(var x = 0; x < 10; x++){z} for(var y = 0, z = 0; y < 10; y++){z}",
//       "var x=0;for(;x<10;x++)z;x=0;var z=0;for(;x<10;x++)z");

//   inFunction("var x = 1; x; for (var y; y=1; ) {y}", "var x = 1; x; for ( ; x=1; ) {x}");

//   inFunction("var x = 1; x; y = 1; while(y) var y; y", "var x = 1; x; x = 1; for (;x;); x");

//   // It's not the job of the coalesce variables pass to remove unused labels
//   inFunction("var x = 1; x; f:var y; y=1", "var x = 1; x; f:{} x=1");
// }

#[test]
fn testSwitchCase() {
    inFunction(
        "var x = 1; switch(x) { case 1: var y; case 2: } y = 1; y",
        "var x = 1; switch(x) { case 1:        case 2: } x = 1; x",
    );
}

#[test]
fn testDuplicatedVar() {
    // Is there a shorter version without multiple declarations?
    inFunction(
        "z = 1; var x = 0; x; z; var y = 2,     z = 1; y; z;",
        "z = 1; var x = 0; x; z;     x = 2; var z = 1; x; z;",
    );
}

#[test]
fn testTryCatch() {
    inFunctionSame("try {} catch (e) { } var x = 4; x;");
    inFunctionSame("var x = 4; x; try {} catch (e) { }");
}

#[test]
fn testDeadAssignment() {
    inFunctionSame("var x = 6; var y; y = 4 ; x");
    inFunctionSame("var y = 3; y = y + 4; x");
    inFunctionSame("var y = 3; y = y + 1; x");
    inFunctionSame("y = 3; var x; var y = 1 ; x");
}

#[test]
fn testParameter() {
    test_transform(
        "function FUNC(param) {var x = 0; x}",
        "function FUNC(param) {param = 0; param}",
    );
}

#[test]
fn testParameter2() {
    // Make sure two formal parameter name never merges.
    test_same("function FUNC(x,y) {x = 0; x; y = 0; y}");
    test_same("function FUNC(x,y,z) {x = 0; x; y = 0; z = 0; z}");
}

#[test]
fn testParameter3() {
    // Make sure the formal parameter declaration is consider a def.
    test_same("function FUNC(x) {var y; y = 0; x; y}");
}

// TODO:
// #[test]
// fn testParameter4a() {
//   // Make sure that we do not merge two-arg functions because of the
//   // IE sort bug (see comments in computeEscaped)
//   setLanguageOut(LanguageMode.ECMASCRIPT3);
//   test_transform(
//       "function FUNC(x, y) {var a,b; y; a=0; a; x; b=0; b}",
//       "function FUNC(x, y) {var a;   y; a=0; a; x; a=0; a}");
// }

// TODO:
// #[test]
// fn testParameter4b() {
//   // Go ahead and merge, if language-out is ES5, because that means IE8 does
//   // not need to be supported.
//   setLanguageOut(LanguageMode.ECMASCRIPT5);
//   test_transform(
//       "function FUNC(x, y) {var a,b; y; a=0; a; x; b=0; b}",
//       "function FUNC(x, y) {         y; y=0; y; x; x=0; x}");
// }

#[test]
fn testParameter5() {
    // Merge parameters
    test_transform(
        "function FUNC(x, y, z) {var a,b; y; a=0; a; x; b=0; b}",
        "function FUNC(x, y, z) {         y; y=0; y; x; x=0; x}",
    );
}

#[test]
fn testLiveRangeChangeWithinCfgNode() {
    inFunctionSame("var x; var y; x = 1, y = 2, y, x");
    inFunctionSame("var x; var y; x = 1,x; y");

    // We lose some precisions within the node itself.
    inFunctionSame("var x; var y; y = 1, y, x = 1; x");

    inFunction(
        "var x; var y; y = 1; y, x = 1; x",
        "var x; x = 1; x, x = 1; x",
    );

    inFunctionSame("var x; var y; y = 1, x = 1, x, y = y + 1, y");

    inFunctionSame("var x; var y; y = 1, x = 1, x, y = y + 1, y");
}

#[test]
fn testLiveRangeChangeWithinCfgNode2() {
    inFunctionSame("var x; var y; var a; var b; y = 1, a = 1, y, a, x = 1, b = 1; x; b");

    inFunction(
        "
var x; var y; var a; var b; y = 1, a = 1, y, a, x = 1; x; b = 1; b",
        "
var x; var y; var a;        y = 1, a = 1, y, a, x = 1; x; x = 1; x",
    );

    inFunction(
        "var x; var y; var a; var b; y = 1, a = 1, y, x = 1; a; x; b = 1; b",
        "var x; var y; var a;        y = 1, a = 1, y, x = 1; a; x; x = 1; x",
    );
}

#[test]
fn testFunctionNameReuse() {
    inFunctionSame("function x() {}; x(); var y = 1; y");

    inFunctionSame("function x() { } x(); var y = 1; y");

    inFunctionSame("function x() { } x(); var y = 1; y");

    // Can't merge because of possible escape.
    inFunctionSame("function x() {return x}; x(); var y = 1; y");

    inFunctionSame("function x() {} var y = 1; y; x");

    inFunctionSame("function x() { } var y = 1; y; x");

    inFunctionSame("function x() { } var y = 1; y; x = 1; x");

    inFunctionSame("function x() {} var y = 1; y; x = 1; x");
}

#[test]
fn testBug65688660() {
    test_transform(
        "
function f(param) {
    if (true) {
        const b1 = [];
        for (const [key, value] of []) {}
    }
    if (true) {
        const b2 = [];
        for (const kv of []) {
            const key2 = kv.key;
        }
    }
}",
        "
function f(param) {
    if (true) {
        param = [];
        for (const [key, value] of []) {}
    }
    if (true) {
        param = [];
        for (const kv of []) {
            param = kv.key;
        }
    }
}",
    );
}

#[test]
fn testBug1401831() {
    // Verify that we don't wrongly merge "opt_a2" and "i" without considering
    // arguments[0] aliasing it.
    test_same(
        "
function f(opt_a2){
    var buffer;
    if(opt_a2){
        var i=0;
        for(;i<arguments.length;i++) {
            buffer=buffer+arguments[i];
        }
    }
    return buffer;
}",
    );
}

// Code inside a class is automatically in strict mode, so duplicated parameter names are not
// allowed.
#[test]
fn testBug64898400() {
    test_same("class C { f(a, b, c) {} }");
    test_same("class C { f(a, b=0, c=0) {} }");
}

#[test]
fn testDontCoalesceClassDeclarationsWithConstDeclaration() {
    test_same(
        "
function f() {
    class A {}
    const b = {};
    return b;
}",
    );
}

#[test]
fn testDontCoalesceClassDeclarationsWithDestructuringDeclaration() {
    // See https://github.com/google/closure-compiler/issues/3019 - this used to cause a syntax
    // error by coalescing `B` and `C` without converting `class B {}` to a non-block-scoped
    // declaration.
    test_same(
        "
function f(obj) {
    class B {}
    console.log(B);
    const {default: C} = obj;
    return {obj, C};
}",
    );
}

#[test]
fn testObjDestructuringConst() {
    test_transform(
        "
function f(obj) {
    {
        const {foo: foo} = obj;
        alert(foo);
    }
}",
        "
function f(obj) {
    {
        ({foo: obj} = obj);
        alert(obj);
    }
}",
    );
}

#[test]
fn testObjDestructuringConstWithMultipleDeclarations() {
    test_transform(
        "function f(obj) {
            {
              const {foo: foo} = obj;
              alert(foo);
            }
            {
              const {bar: bar} = obj;
              alert(bar);
            }
          }",
        "function f(obj) {
            {
              const {foo: foo} = obj;
              alert(foo);
            }
            {
              ({bar: obj} = obj);
              alert(obj);
            }
          }",
    );
}

#[test]
fn testObjDestructuringConstWithMultipleLvaluesInDecl() {
    test_same(
        "function f() {
            const obj = {};
            const {prop1: foo, prop2: bar} = obj;
            alert(foo);
          }",
    );
}

#[test]
fn testObjDestructuringVar() {
    test_same(
        "function f(param) {
            var {prop1: foo, prop2: bar} = param;
            alert(foo);
          }",
    );
}

#[test]
fn testObjDestructuringVarInAsyncFn() {
    test_same(
        "async function f(param) {
            var {prop1: foo, prop2: bar} = param;
            alert(foo);
          }",
    );
}

#[test]
fn testObjDestructuringVarInGeneratorFn() {
    test_same(
        "function *f(param) {
            var {prop1: foo, prop2: bar} = param;
            alert(foo);
          }",
    );
}

#[test]
fn testObjDestructuringVarInAsyncGeneratorFn() {
    // TODO:
    //   setAcceptedLanguage(LanguageMode.ECMASCRIPT_NEXT);
    test_same(
        "async function *f(param) {
            var {prop1: foo, prop2: bar} = param;
            alert(foo);
          }",
    );
}

#[test]
fn testLetWithSingleLValuesInForLoopCoalesced() {
    test_transform(
        "function f(x) {
            for (let y = x + 1;;) {
              alert(y);
            }
          }",
        "function f(x) {
            for (x = x + 1;;) {
              alert(x);
            }
          }",
    );
}

#[test]
fn testLetWithMultipleLValuesInForLoopNotCoalesced() {
    test_same(
        "function f(x) {
            for (let y = x + 1, z = 0;;) {
              alert(y + z);
            }
          }",
    );
}

#[test]
fn testConstDestructuringDeclInForOf_dropsConst() {
    test_transform(
        "function f(param) { for (let [y] of []) {} }",
        "function f(param) { for ([param] of []) {} }",
    );
}

#[test]
fn testConstDestructuringInForOfCoalescedWithUseInBlock() {
    // TODO(b/121276933): coalesce `x` and `y`
    inFunctionSame("var x = 1; for (let [y] of iter) { y }");
}

#[test]
fn testReplaceRhsOfDestructuringDeclaration() {
    inFunction(
        "let unused = 0; let arr = [1, 2, 3]; const [a, b, c] = arr; alert(a + b + c);",
        "var unused = 0; unused = [1, 2, 3]; const [a, b, c] = unused; alert(a + b + c);",
    );
}

// TODO:
// #[test]
// fn testReplaceRhsOfDestructuringDeclaration_withPseudoNames() {
//   usePseudoName = true;
//   inFunction(
//       "let unused = 0; let arr = [1, 2, 3]; const [a, b, c] = arr; alert(a + b + c);",
//       lines(
//           "var arr_unused = 0;",
//           "arr_unused = [1, 2, 3];",
//           "const [a, b, c] = arr_unused;",
//           "alert(a + b + c);"));
// }

#[test]
fn testDestructuringDefaultValue() {
    test_same("function f(param) {  var a;  [a = param] = {};  param;  }");

    test_transform(
        "
        function f(param) {  var a;  [a = param] = {};  a;  }",
        "
        function f(param) {  [param = param] = {};  param;  }",
    );
}

#[test]
fn testSpread_ofArray_consideredRead() {
    test_same(
        "function f() {
            var a = 9;
            var b = 6;
          
            ([...a]); // Read `a`.
            return b;
          }",
    );

    test_transform(
        "function f() {
            var a = 9;
            read(a);
         
            var b = 6;
            ([...b]); // Read `b`.
          }",
        "function f() {
            var a = 9;
            read(a);
          
            a = 6;
            ([...a]);
          }",
    );
}

#[test]
fn testSpread_ofObject_consideredRead() {
    test_same(
        "function f() {
            var a = 9;
            var b = 6;
          
            ({...a}); // Read `a`.
            return b;
          }",
    );

    test_transform(
        "function f() {
            var a = 9;
            read(a);
          
            var b = 6;
            ({...b}); // Read `b`.
          }",
        "function f() {
            var a = 9;
            read(a);
          
            a = 6;
            ({...a});
          }",
    );
}

#[test]
fn testRest_fromArray_consideredWrite() {
    test_same(
        "function f() {
            var a = 9;
            var b = 6;
          
            ([...a] = itr); // Write `a`.
            return b;
          }",
    );

    test_transform(
        "function f() {
            var a = 9;
            read(a);
          
            var b = 6;
            ([...b] = itr); // Write `b`.
          }",
        "function f() {
            var a = 9;
            read(a);
          
            a = 6;
            ([...a] = itr);
          }",
    );
}

#[test]
fn testRest_fromObject_consideredWrite() {
    test_same(
        "function f() {
            var a = 9;
            var b = 6;
          
            ({...a} = obj); // Write `a`.
            return b;
          }",
    );

    test_transform(
        "function f() {
            var a = 9;
            read(a);
          
            var b = 6;
            ({...b} = itr); // Write `b`.
          }",
        "function f() {
            var a = 9;
            read(a);
          
            a = 6;
            ({...a} = itr);
          }",
    );
}

#[test]
fn testDestructuringEvaluationOrder() {
    // Since the "a = 5" assignment is evaluated before "a = param" (which is
    // conditionally evaluated), we must not coalesce param and a.
    test_same("function f(param) { var a; [a = param] = (a = 5, {});  a; }");
}

// We would normally coalesce 'key' with 'collidesWithKey', but in doing so we'd change the 'let'
// on line 2 to a 'var' which would cause the inner function to capture the wrong value of 'val'.
#[test]
fn testCaptureLet() {
    test_same(
        "function f(param) {
            for (let [key, val] of foo()) {
              param = (x) => { return val(x); };
            }
            let collidesWithKey = 5;
            return param(collidesWithKey);
          }",
    );
}

// TODO:
// Compare to the earlier case. Since the two-lvalue declaration `var [key, val]` gets normalized
// we still coalesce `key` with `collidesWithKey`.
// #[test]
// fn testCaptureVar() {
//     test_transform(
//         "function f(param) {
//             for (var [key, val] of foo()) {
//               param = (x) => { return val(x); };
//             }
//             let collidesWithKey = 5;
//             return param(collidesWithKey);
//           }",
//         "function f(param) {
//             var key;
//             var val;
//             for ([key, val] of foo()) {
//               param = (x) => { return val(x); };
//             }
//             key = 5;
//             return param(key);
//           }",
//     );
// }

#[test]
fn testDestructuring() {
    test_same(
        "function f() {
            const [x, y] = foo(5);
            let z = foo(x);
            return x;
          }",
    );
}

#[test]
fn testDeterministic() {
    // Make the variable interference graph a pentagon.
    //         a - b
    //        /     \
    //        e     c
    //         \   /
    //           d
    // The coloring partitioning would be:
    //  a = { a, c }
    //  b = { b, d }
    //  e = { e }
    inFunction(
        "
var a;
var b;
var c;
var d;
var e;
a=1; b=1; a; b;
b=1; c=1; b; c;
c=1; d=1; c; d;
d=1; e=1; d; e;
e=1; a=1; e; a;",
        "
var a;
var b;
var e;
a=1; b=1; a; b;
b=1; a=1; b; a;
a=1; b=1; a; b;
b=1; e=1; b; e;
e=1; a=1; e; a;",
    );

    // If we favor "d" first by declaring "d" earlier,
    // the coloring partitioning would be:
    //  b = { b, e }
    //  d = { d, a }
    //  c = { c }
    inFunction(
        "
var d,a,b,c,e;
a=1; b=1; a; b;
b=1; c=1; b; c;
c=1; d=1; c; d;
d=1; e=1; d; e;
e=1; a=1; e; a;",
        "
var d;var b;var c;
d=1;b=1;d;b;
b=1;c=1;b;c;
c=1;d=1;c;d;
d=1;b=1;d;b;
b=1;d=1;b;d",
    );
}

// Sometimes live range can be cross even within a VAR declaration.
#[test]
fn testVarLiveRangeCross() {
    inFunction("var a={}; var b=a.S(); b", "var a={};     a=a.S(); a");

    inFunction(
        "var a = {}; var b = a.S(),     c = b.SS(); b; c",
        "var a = {};     a = a.S(); var c = a.SS(); a; c",
    );

    inFunction(
        "var a={}; var b=a.S(); var c=a.SS(); var d=a.SSS(); b; c; d",
        "var a={}; var b=a.S(); var c=a.SS();     a=a.SSS(); b; c; a",
    );

    inFunction(
        "var a={}; var b=a.S(); var c=a.SS(); var d=a.SSS(); b; c; d",
        "var a={}; var b=a.S(); var c=a.SS();     a=a.SSS(); b; c; a",
    );

    inFunctionSame("var a={}; d=1; d; var b=a.S(); var c=a.SS(); var d=a.SSS(); b; c; d");
}

#[test]
fn testBug1445366() {
    // An assignment might not be complete if the RHS throws an exception.
    inFunctionSame(
        "var iframe = getFrame();
          try {
            var win = iframe.contentWindow;
          } catch (e) {
          } finally {
            if (win) {
              this.setupWinUtil_();
            } else {
              this.load();
            }
          }",
    );

    // Verify that we can still coalesce it if there are no handlers.
    inFunction(
        "var iframe = getFrame();
          var win = iframe.contentWindow;
          if (win) {
            this.setupWinUtil_();
          } else {
            this.load();
          }",
        "var iframe = getFrame();
          iframe = iframe.contentWindow;
          if (iframe) {
            this.setupWinUtil_();
          } else {
            this.load();
          }",
    );
}

// Parameter 'e' is never used, but if we coalesce 'command' with 'e' then the 'if (command)'
// check will produce an incorrect result if none of the 'case' statements is executed.
#[test]
fn testCannotReuseAnyParamsBug() {
    test_same(
        "function handleKeyboardShortcut(e, key, isModifierPressed) {
            if (!isModifierPressed) {
              return false;
            }
            var command;
            switch (key) {
              case 'b': // Ctrl+B
                command = COMMAND.BOLD;
                break;
              case 'i': // Ctrl+I
                command = COMMAND.ITALIC;
                break;
              case 'u': // Ctrl+U
                command = COMMAND.UNDERLINE;
                break;
              case 's': // Ctrl+S
                return true;
            }
          
            if (command) {
              this.fieldObject.execCommand(command);
              return true;
            }
          
            return false;
          };",
    );
}

#[test]
fn testCannotReuseAnyParamsBugWithDestructuring() {
    test_same(
        "function handleKeyboardShortcut({type: type}, key, isModifierPressed) {
        if (!isModifierPressed) {
          return false;
        }
        var command;
        switch (key) {
          case 'b': // Ctrl+B
            command = COMMAND.BOLD;
            break;
          case 'i': // Ctrl+I
            command = COMMAND.ITALIC;
            break;
          case 'u': // Ctrl+U
            command = COMMAND.UNDERLINE;
            break;
          case 's': // Ctrl+S
            return true;
        }

        if (command) {
          this.fieldObject.execCommand(command);
          return true;
        }

        return false;
      };",
    );
}

#[test]
fn testForInWithAssignment() {
    inFunction(
        "function f(commands) {
            var k, v, ref;
            for (k in ref = commands) {
              v = ref[k];
              alert(k + ':' + v);
            }
          }",
        "function f(commands){
          var k;
          var ref;
          for(k in ref = commands) {
            commands=ref[k];
            alert(k+':'+commands)
          }}",
    );
}

// TODO:
// #[test]
// fn testUsePseudoNames() {
//   usePseudoName = true;
//   inFunction("var x   = 0; print(x  ); var   y = 1; print(  y)",
//              "var x_y = 0; print(x_y);     x_y = 1; print(x_y)");

//   inFunction(
//       "var x_y = 1; var x    = 0; print(x   ); var     y = 1; print(   y); print(x_y);",
//       "var x_y = 1; var x_y$ = 0; print(x_y$);      x_y$ = 1; print(x_y$); print(x_y);");

//   inFunction(
//       lines(
//           "var x_y = 1; ",
//           "function f() {",
//           "  var x    = 0;",
//           "  print(x  );",
//           "  var y = 1;",
//           "  print( y);",
//           "  print(x_y);",
//           "}"),
//       lines(
//           "function f(){",
//           "  var x_y$=0;",
//           "  print(x_y$);",
//           "  x_y$=1;",
//           "  print(x_y$);",
//           "  print(x_y)",
//           "}",
//           "var x_y=1"));

//   inFunction(
//       lines(
//           "var x   = 0;",
//           "print(x  );",
//           "var   y = 1;",
//           "print(  y); ",
//           "var closure_var;",
//           "function bar() {",
//           "  print(closure_var);",
//           "}"),
//       lines(
//           "function bar(){",
//           "  print(closure_var)",
//           "}",
//           "var x_y=0;",
//           "print(x_y);",
//           "x_y=1;",
//           "print(x_y);",
//           "var closure_var"));
// }

// TODO:
// #[test]
// fn testUsePseudoNamesWithLets() {
//   usePseudoName = true;
//   inFunction(
//       lines(
//           "var x_y = 1; ",
//           "function f() {",
//           "  let x    = 0;",
//           "  print(x  );",
//           "  let y = 1;",
//           "  print( y);",
//           "  print(x_y);",
//           "}"),
//       lines(
//           "function f(){",
//           "  var x_y$=0;",
//           "  print(x_y$);",
//           "  x_y$ = 1;",
//           "  print(x_y$);",
//           "  print(x_y)",
//           "}",
//           "var x_y=1"));
// }

#[test]
fn testMaxVars() {
    let mut code = String::new();
    for i in 0..(crate::LiveVariablesAnalysis::MAX_VARIABLES_TO_ANALYZE + 1) {
        write!(&mut code, "var x{} = 0; print(x{});", i, i);
    }
    inFunctionSame(&code);
}

// Testing Es6 features
#[test]
fn testCoalesceInInnerBlock() {
    inFunction("{ var x = 1; var y = 2; y }", "{ var x = 1;     x = 2; x }");

    inFunction("var x = 1; var y = 2; y;", "var x = 1;     x = 2; x;");
}

#[test]
fn testLetSimple() {
    inFunction("let x = 0; x; let y = 5; y", "var x = 0; x;     x = 5; x");

    inFunction(
        "var x = 1; var y = 2; { let z = 3; y; }",
        "var x = 1;     x = 2; { let z = 3; x; }",
    );

    // First let in a block - It is unsafe for { let x = 0; x; } let y = 1; to be coalesced as
    // { let x = 0; x; } x = 1; because x will be out of scope outside of the inner scope!
    inFunction(
        "{ let x = 0; x; } let y = 5; y;",
        "{ var x = 0; x; }     x = 5; x;",
    );

    // The following situation will never happen in practice because at this point, the code has
    // been normalized so no two variables will have the same name
    // --> var x = 1; var y = 2; { let x = 3; y }
}

#[test]
fn testLetDifferentBlocks() {
    inFunction(
        "var x = 0; if (1) { let y = 1; x } else { let z = 1; x }",
        "var x = 0; if (1) { var y = 1; x } else {     y = 1; x }",
    );

    inFunction(
        "var x = 0; if (1) { let y = 1; y } else { let z = 1 + x; z }",
        "var x = 0; if (1) {     x = 1; x } else {     x = 1 + x; x }",
    );

    inFunction(
        "var x = 0; if (1) { let y = 1; y } else { let z = 1; z }; x",
        "var x = 0; if (1) { var y = 1; y } else {     y = 1; y }; x",
    );

    inFunction(
        "if (1) { var x = 0; let y = 1; y + x} else { let z = 1; z } y;",
        "if (1) { var x = 0; let y = 1; y + x} else {     x = 1; x } y;",
    );

    inFunction(
        "if(1) { var x = 0; x } else { var y = 0; y }",
        "if(1) { var x = 0; x } else {     x = 0; x }",
    );

    inFunction(
        "if (a) {
             return a;
           } else {
             let b = a;
             let c = 1;
             return c;
           }
           return a;",
        "if (a) {
              return a;
            } else {
              var b = a;
                  b = 1;
              return b;
            }
            return a;",
    );
}

#[test]
fn testLetWhileLoops() {
    // Simple
    // It violates the temporal deadzone for let x = 1; while (1) { x = 2; x; let x = 0; x } to
    // be output.
    inFunctionSame("let x = 1; for(;1;) { x; x = 2; let y = 0; y }");

    inFunctionSame("let x = 1; for(;1;) { x = 2; x; let y = 0; y } x;");
}

#[test]
fn testLetForLoops() {
    // TODO (simranarora) We should get rid of declaration hoisting from the normalize pass.
    // Right now, because of declaration hoisting, this following test reads the expected code as:
    // var x = 1; for ( ; x < 10; x++) { let y = 2; x + y } x = 3
    inFunction(
        "for (let x = 1; x < 10; x ++) { let y = 2; x + y; } let z = 3;",
        "for(var x=1;x<10;x++){let y=2;x+y}x=3",
    );

    inFunction(
        "var w = 0; for (let x = 1; x < 10; x ++) { let y = 2; x + y; } var z = 3;",
        "var w = 0; for (    w = 1; w < 10; w ++) { let y = 2; w + y; }     w = 3;",
    );

    // Closure capture of the let variable
    // Here z should not be coalesced because variables used in functions are considered escaped
    // and this pass does not touch any escaped variables
    inFunctionSame("let x = 3; for (let z = 1; z < 10; z++) { use(() => {z}); }");

    inFunctionSame("for (let x = 1; x < 10; x++) { use(() => {x}); } let z = 3;");

    // Multiple lets declared in loop head
    inFunctionSame("for (let x = 1, y = 2, z = 3; (x + z) < 10; x ++) { x + z; }");

    // Here the variable y is redeclared because the variable z in the header of the for-loop has
    // not been declared before
    inFunctionSame("let y = 2; for (let x = 1, z = 3; (x + z) < 10; x ++) { x + z; }");
}

#[test]
fn testArrowFunctions() {
    inFunction(
        "var x = 1; var y = () => { let z = 0; z }",
        "var x = 1;     x = () => { let z = 0; z }",
    );

    inFunction(
        "var x = 1; var y = () => { let z = 0; z }; y();",
        "var x = 1;     x = () => { let z = 0; z }; x();",
    );

    inFunction(
        "var x = 1; var y = () => { let z = 0; z }; x;",
        "var x = 1; var y = () => { let z = 0; z }; x;",
    );

    inFunction(
        "var x = () => { let z = 0; let y = 1; y }",
        "var x = () => { var z = 0;     z = 1; z }",
    );

    inFunction(
        "
        var x = 1; var y = 2; var f = () => x + 1",
        "
        var x = 1; var y = 2;     y = () => { return x + 1; }",
    );

    // Coalesce with arrow function parameters
    inFunction(
        "
    (x) => { var y = 1; y; }",
        "
    (x) => {     x = 1; x; }",
    );

    inFunction(
        "
    (x) => { let y = 1; y; }",
        "
    (x) => {     x = 1; x; }",
    );
}

#[test]
fn testCodeWithTwoFunctions() {
    // We only want to coalesce within a function, not across functions
    test_transform(
        "function FUNC1() {
            var x = 1; 
            var y = 2; 
                    y; 
          }
          function FUNC2() {
            var z = 3; 
            var w = 4; 
                    w; 
          }",
        "function FUNC1() {
            var x = 1; 
                x = 2; 
                    x; 
          }
          function FUNC2() {
            var z = 3; 
                z = 4; 
                    z; 
          }",
    );

    // Two arrow functions
    test_transform(
        "
() => { var x = 1; var y = 2; y; };
() => { var z = 3; var w = 4; w; };",
        "
() => { var x = 1;     x = 2; x; };
() => { var z = 3;     z = 4; z; };",
    );
}

// TODO: normalization
// #[test]
// fn testNestedFunctionCoalescing() {
//     test_transform(
//         "
// function FUNC1() {
//     var x = 1;
//     var y = 2;
//             y;
//     function FUNC2() {
//         var z = 3;
//         var w = 4;
//                 w;
//     }
// }",
//         "
// function FUNC1() {
//     function FUNC2() {
//         var z = 3;
//             z = 4;
//                 z
//     }
//     var x = 1;
//     x = 2;
//     x
// }",
//     );
// }

fn inFunctionSame(src: &str) {
    test_same(&format!("function FUNC(){{{src}}}"));
}

fn inFunction(src: &str, expected: &str) {
    test_transform(
        &format!("function FUNC(){{{src}}}"),
        &format!("function FUNC(){{{expected}}}"),
    );
}
