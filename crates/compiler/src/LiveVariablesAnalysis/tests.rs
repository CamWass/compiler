use ast::*;
use ecma_visit::{noop_visit_type, Visit, VisitMutWith, VisitWith};
use global_common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    FileName, Globals, Mark, SourceMap, SyntaxContext, GLOBALS,
};
use parser::{Parser, Syntax};
use rustc_hash::FxHashMap;
use swc_atoms::JsWord;

use crate::control_flow::{
    node::Node,
    ControlFlowAnalysis::{ControlFlowAnalysis, ControlFlowRoot},
};
use crate::resolver::resolver;
use crate::DataFlowAnalysis::LinearFlowState;
use crate::Id;
use crate::{find_vars::find_vars_declared_in_fn, utils::unwrap_as};

use super::LiveVariablesAnalysis;

/**
 * Tests for {@link LiveVariablesAnalysis}. Test cases are snippets of a function and assertions are
 * made at the instruction labeled with {@code X}.
 */

#[test]
fn testStraightLine() {
    // A sample of simple straight line of code with different liveness changes.
    assertNotLiveBeforeX("X:var a;", "a");
    assertNotLiveAfterX("X:var a;", "a");
    assertNotLiveAfterX("X:var a=1;", "a");
    assertLiveAfterX("X:var a=1; a()", "a");
    assertNotLiveBeforeX("X:var a=1; a()", "a");
    assertLiveBeforeX("var a;X:a;", "a");
    assertLiveBeforeX("var a;X:a=a+1;", "a");
    assertLiveBeforeX("var a;X:a+=1;", "a");
    assertLiveBeforeX("var a;X:a++;", "a");
    assertNotLiveAfterX("var a,b;X:b();", "a");
    assertNotLiveBeforeX("var a,b;X:b();", "a");
    assertLiveBeforeX("var a,b;X:b(a);", "a");
    assertLiveBeforeX("var a,b;X:b(1,2,3,b(a + 1));", "a");
    assertNotLiveBeforeX("var a,b;X:a=1;b(a)", "a");
    assertNotLiveAfterX("var a,b;X:b(a);b()", "a");
    assertLiveBeforeX("var a,b;X:b();b=1;a()", "b");
    assertLiveAfterX("X:a();var a;a()", "a");
    assertNotLiveAfterX("X:a();var a=1;a()", "a");
    assertLiveBeforeX("var a,b;X:a,b=1", "a");
}

#[test]
fn testProperties() {
    // Reading property of a local variable makes that variable live.
    assertLiveBeforeX("var a,b;X:a.P;", "a");

    // Assigning to a property doesn't kill "a". It makes it live instead.
    assertLiveBeforeX("var a,b;X:a.P=1;b()", "a");
    assertLiveBeforeX("var a,b;X:a.P.Q=1;b()", "a");

    // An "a" in a different context.
    assertNotLiveAfterX("var a,b;X:b.P.Q.a=1;", "a");

    assertLiveBeforeX("var a,b;X:b.P.Q=a;", "a");
}

#[test]
fn testConditions() {
    // Reading the condition makes the variable live.
    assertLiveBeforeX("var a,b;X:if(a){}", "a");
    assertLiveBeforeX("var a,b;X:if(a||b) {}", "a");
    assertLiveBeforeX("var a,b;X:if(b||a) {}", "a");
    assertLiveBeforeX("var a,b;X:if(b||b(a)) {}", "a");
    assertNotLiveAfterX("var a,b;X:b();if(a) {}", "b");

    // We can kill within a condition as well.
    assertNotLiveAfterX("var a,b;X:a();if(a=b){}a()", "a");
    assertNotLiveAfterX("var a,b;X:a();while(a=b){}a()", "a");

    // The kill can be "conditional" due to short circuit.
    assertNotLiveAfterX("var a,b;X:a();if((a=b)&&b){}a()", "a");
    assertNotLiveAfterX("var a,b;X:a();while((a=b)&&b){}a()", "a");
    assertLiveBeforeX("var a,b;a();X:if(b&&(a=b)){}a()", "a"); // Assumed live.
    assertLiveBeforeX("var a,b;a();X:if(a&&(a=b)){}a()", "a");
    assertLiveBeforeX("var a,b;a();X:while(b&&(a=b)){}a()", "a");
    assertLiveBeforeX("var a,b;a();X:while(a&&(a=b)){}a()", "a");
}

#[test]
fn nullishCoalesce() {
    // Reading the condition makes the variable live.
    assertLiveBeforeX("var a,b;X:if(a??b) {}", "a");
    assertLiveBeforeX("var a,b;X:if(b??a) {}", "a");
    assertLiveBeforeX("var a,b;X:if(b??b(a)) {}", "a");

    // Unconditionally killed on lhs of ??
    assertNotLiveAfterX("var a,b;X:a();if((a=b)??b){}a()", "a");
    assertNotLiveAfterX("var a,b;X:a();while((a=b)??b){}a()", "a");

    // The kill can be "conditional" due to short circuit.
    assertLiveBeforeX("var a,b; X:if(b??(a=b)){}a()", "a"); // Assumed live.
    assertLiveBeforeX("var a,b; X:if(a??(a=b)){}a()", "a");
    assertLiveBeforeX("var a,b; X:while(b??(a=b)){}a()", "a");
    assertLiveBeforeX("var a,b; X:while(a??(a=b)){}a()", "a");
}

#[test]
fn logicalAssignment() {
    // TODO: comment
    // This pattern is normalized away
    assertLiveBeforeX("var a,b;X:a??=b", "a");
    assertLiveBeforeX("var a,b;X:a??=b", "b");
}

#[test]
fn optChainGetProp() {
    // Reading the var on lhs of opt chain makes the variable live.
    assertNotLiveBeforeX("var a,b; X:if(b) {}", "a");
    assertLiveBeforeX("var a,b; X:if(a?.b) {}", "a");

    // Reading a prop with the same name as var does not make the var live
    assertNotLiveBeforeX("var a,b;X:if(b?.a) {}", "a");

    // unconditional kill on lhs of ?.
    assertNotLiveAfterX("var a,b;X:a();if((a=c)?.b){} a()", "a");
    assertNotLiveAfterX("var a,b;X:a();while((a=b)?.b){} a()", "a");
}

#[test]
fn optChainCall() {
    // conditionally accessing var keeps it live
    assertLiveBeforeX("var a,b; X:if(b?.(a)){}", "a");

    // unconditionally overwriting a var kills it
    assertNotLiveAfterX("var a,b; X:a(); if((a=b)?.b()){} a()", "a");

    // conditionally overwriting var does not kill it
    assertLiveBeforeX("var a,b; X:if(b?.(a=c)){} a();", "a");

    // conditional overwrite on rhs of ?. does not kill the var
    assertLiveBeforeX("var a,b; X:if(b?.(a=b)){}a()", "a"); // Assumed live.
    assertLiveBeforeX("var a,b; X:if(a?.(a=b)){}a()", "a");
    assertLiveBeforeX("var a,b; X:while(b?.(a=b)){}a()", "a");
    assertLiveBeforeX("var a,b; X:while(a?.(a=b)){}a()", "a");
}

#[test]
fn optChainGetElem() {
    // conditionally accessing var keeps it live
    assertLiveBeforeX("var a,b; X:if(b?.[a]) {}", "a");

    // unconditionally overwriting a var kills it
    assertNotLiveAfterX("var a,b; X:a(); if((a=b)?.[b]){} a()", "a");

    // conditionally overwriting var does not kill it
    assertLiveBeforeX("var a,b; X:if(b?.[a=c]) {} a();", "a");

    // conditional overwrite on rhs of ?. does not kill the var
    assertLiveBeforeX("var a,b; X:if(b?.[a=b]){}a()", "a"); // Assumed live.
    assertLiveBeforeX("var a,b; X:if(a?.[a=b]){}a()", "a");
    assertLiveBeforeX("var a,b; X:while(b?.[a=b]){}a()", "a");
    assertLiveBeforeX("var a,b; X:while(a?.[a=b]){}a()", "a");
}

#[test]
fn testArrays() {
    assertLiveBeforeX("var a;X:a[1]", "a");
    assertLiveBeforeX("var a,b;X:b[a]", "a");
    assertLiveBeforeX("var a,b;X:b[1,2,3,4,b(a)]", "a");
    assertLiveBeforeX("var a,b;X:b=[a,'a']", "a");
    assertNotLiveBeforeX("var a,b;X:a=[];b(a)", "a");

    // Element assignment doesn't kill the array.
    assertLiveBeforeX("var a;X:a[1]=1", "a");
}

#[test]
fn testTwoPaths() {
    // Both Paths.
    assertLiveBeforeX("var a,b;X:if(b){b(a)}else{b(a)};", "a");

    // Only one path.
    assertLiveBeforeX("var a,b;X:if(b){b(b)}else{b(a)};", "a");
    assertLiveBeforeX("var a,b;X:if(b){b(a)}else{b(b)};", "a");

    // None of the paths.
    assertNotLiveAfterX("var a,b;X:if(b){b(b)}else{b(b)};", "a");

    // At the very end.
    assertLiveBeforeX("var a,b;X:if(b){b(b)}else{b(b)}a();", "a");

    // The loop might or might not be executed.
    assertLiveBeforeX("var a;X:while(param1){a()};", "a");
    assertLiveBeforeX("var a;X:while(param1){a=1};a()", "a");

    // Same idea with if.
    assertLiveBeforeX("var a;X:if(param1){a()};", "a");
    assertLiveBeforeX("var a;X:if(param1){a=1};a()", "a");

    // This is different in DO. We know for sure at least one iteration is
    // executed.
    assertNotLiveAfterX("X:var a;do{a=1}while(param1);a()", "a");
}

#[test]
fn testThreePaths() {
    assertLiveBeforeX("var a;X:if(1){}else if(2){}else{a()};", "a");
    assertLiveBeforeX("var a;X:if(1){}else if(2){a()}else{};", "a");
    assertLiveBeforeX("var a;X:if(1){a()}else if(2){}else{};", "a");
    assertLiveBeforeX("var a;X:if(1){}else if(2){}else{};a()", "a");
}

#[test]
fn testHooks() {
    assertLiveBeforeX("var a;X:1?a=1:1;a()", "a");

    // Unfortunately, we cannot prove the following because we assume there is
    // no control flow within a hook (i.e. no joins / set unions).
    // assertNotLiveAfterX("var a;X:1?a=1:a=2;a", "a");
    assertLiveBeforeX("var a,b;X:b=1?a:2", "a");
}

#[test]
fn testForLoops() {
    // Induction variable should not be live after the loop.
    assertNotLiveBeforeX("var a,b;for(a=0;a<9;a++){b(a)};X:b", "a");
    assertNotLiveBeforeX("var a,b;for(a in b){a()};X:b", "a");
    assertNotLiveBeforeX("var a,b;for(a in b){a()};X:a", "b");
    assertLiveBeforeX("var b;for(var a in b){X:a()};", "a");

    // It should be live within the loop even if it is not used.
    assertLiveBeforeX("var a,b;for(a=0;a<9;a++){X:1}", "a");
    assertLiveAfterX("var a,b;for(a in b){X:b};", "a");
    // For-In should serve as a gen as well.
    assertLiveBeforeX("var a,b; X:for(a in b){ }", "a");

    // "a in b" should kill "a" before it.
    // Can't prove this unless we have branched backward DFA.
    // assertNotLiveAfterX("var a,b;X:b;for(a in b){a()};", "a");

    // Unless it is used before.
    assertLiveBeforeX("var a,b;X:a();b();for(a in b){a()};", "a");

    // Initializer
    assertLiveBeforeX("var a,b;X:b;for(b=a;;){};", "a");
    assertNotLiveBeforeX("var a,b;X:a;for(b=a;;){b()};b();", "b");
}

#[test]
fn testForOfLoopsVar() {
    assertLiveBeforeX("var a; for (a of [1, 2, 3]) {X:{}}", "a");
    assertLiveAfterX("for (var a of [1, 2, 3]) {X:{}}", "a");
    assertLiveBeforeX("var a,b; for (var y of a = [0, 1, 2]) { X:a[y] }", "a");
}

#[test]
fn testForOfLoopsDestructuring() {
    assertLiveBeforeX(
        "var key, value; X:for ([key, value] of arr) {value;} value;",
        "value",
    );
    assertLiveBeforeX("let x = 3; X:for (var [y = x] of arr) { y; }", "x");
    assertLiveBeforeX("for (let [key, value] of arr) { X: key; value; }", "key");
}

#[test]
fn testForAwaitOfLoopsVar() {
    assertLiveBeforeXAsync("var a; for await (a of [1, 2, 3]) {X:{}}", "a", true);
    assertLiveAfterXAsync("for await (var a of [1, 2, 3]) {X:{}}", "a", true);
    assertLiveBeforeXAsync(
        "var a,b; for await (var y of a = [0, 1, 2]) { X:a[y] }",
        "a",
        true,
    );
}

#[test]
fn testForAwaitOfLoopsDestructuring() {
    assertLiveBeforeXAsync(
        "var key, value; X:for await ([key, value] of arr) {value;} value;",
        "value",
        true,
    );
    assertLiveBeforeXAsync(
        "let x = 3; X:for await (var [y = x] of arr) { y; }",
        "x",
        true,
    );
    assertLiveBeforeXAsync(
        "for await (let [key, value] of arr) { X: key; value; }",
        "key",
        true,
    );
}

#[test]
fn testNestedLoops() {
    assertLiveBeforeX("var a;X:while(1){while(1){a()}}", "a");
    assertLiveBeforeX("var a;X:while(1){while(1){while(1){a()}}}", "a");
    assertLiveBeforeX("var a;X:while(1){while(1){a()};a=1}", "a");
    assertLiveAfterX("var a;while(1){while(1){a()};X:a=1;}", "a");
    assertLiveAfterX("var a;while(1){X:a=1;while(1){a()}}", "a");
    assertNotLiveBeforeX(
        "var a;X:1;do{do{do{a=1;}while(1)}while(1)}while(1);a()",
        "a",
    );
}

#[test]
fn testSwitches() {
    assertLiveBeforeX("var a,b;X:switch(a){}", "a");
    assertLiveBeforeX("var a,b;X:switch(b){case(a):break;}", "a");
    assertLiveBeforeX("var a,b;X:switch(b){case(b):case(a):break;}", "a");
    assertNotLiveBeforeX(
        "var a,b;X:switch(b){case 1:a=1;break;default:a=2;break};a()",
        "a",
    );

    assertLiveBeforeX("var a,b;X:switch(b){default:a();break;}", "a");
}

#[test]
fn testAssignAndReadInCondition() {
    // BUG #1358904
    // Technically, this isn't exactly true....but we haven't model control flow
    // within an instruction.
    assertLiveBeforeX("var a, b; X: if ((a = this) && (b = a)) {}", "a");
    assertNotLiveBeforeX("var a, b; X: a = 1, b = 1;", "a");
    assertNotLiveBeforeX("var a; X: a = 1, a = 1;", "a");
}

#[test]
fn testParam() {
    // Unused parameter should not be live.
    assertNotLiveAfterX("var a;X:a()", "param1");
    assertLiveBeforeX("var a;X:a(param1)", "param1");
    assertNotLiveAfterX("var a;X:a();a(param2)", "param1");
}

#[test]
fn testExpressionInForIn() {
    assertLiveBeforeX("var a = [0]; X:for (a[1] in foo) { }", "a");
}

#[test]
fn testArgumentsArray() {
    // Check that use of arguments forces the parameters into the
    // escaped set.
    assertEscaped("arguments[0]", "param1");
    assertNotEscaped("arguments[0]", "param2");
    assertNotEscaped("arguments[0]", "param3");

    assertEscaped("var args = arguments", "param1");
    assertNotEscaped("var args = arguments", "param2");
    assertNotEscaped("var args = arguments", "param3");

    assertNotEscaped("arguments = []", "param1");
    assertNotEscaped("arguments = []", "param2");
    assertNotEscaped("arguments = []", "param3");

    assertEscaped("arguments[0] = 1", "param1");
    assertNotEscaped("arguments[0] = 1", "param2");
    assertNotEscaped("arguments[0] = 1", "param3");

    assertEscaped("arguments[arguments[0]] = 1", "param1");
    assertNotEscaped("arguments[arguments[0]] = 1", "param2");
    assertNotEscaped("arguments[arguments[0]] = 1", "param3");
}

// TODO: see https://github.com/google/closure-compiler/issues/3973
// #[test]
// fn testArgumentsArray_doesNotEscape_destructuredParams() {
//     // These cases also cover a crash related to assuming all RESTs have a NAME child.
//     // assertNotEscaped("function f([a]) { arguments; }", "a");
//     // assertNotEscaped("function f([a] = []) { arguments; }", "a");
//     // assertNotEscaped("function f(...[a]) { arguments; }", "a");
//     // assertNotEscaped("function f({a}) { arguments; }", "a");
//     // assertNotEscaped("function f({a} = {}) { arguments; }", "a");
// }

#[test]
fn testTryCatchFinally() {
    assertLiveAfterX("var a; try {X:a=1} finally {a}", "a");
    assertLiveAfterX("var a; try {a()} catch(e) {X:a=1} finally {a}", "a");
    // Because the outer catch doesn't catch any exceptions at all, the read of
    // "a" within the catch block should not make "a" live.
    assertNotLiveAfterX(
        "var a = 1; try { try {a()} catch(e) {X:1} } catch(E) {a}",
        "a",
    );
    assertLiveAfterX("var a; while(1) { try {X:a=1;break} finally {a}}", "a");
}

#[test]
fn testForInAssignment() {
    assertLiveBeforeX("var a,b; for (var y in a = b) { X:a[y] }", "a");
    // No one refers to b after the first iteration.
    assertNotLiveBeforeX("var a,b; for (var y in a = b) { X:a[y] }", "b");
    assertLiveBeforeX("var a,b; for (var y in a = b) { X:a[y] }", "y");
    assertLiveAfterX("var a,b; for (var y in a = b) { a[y]; X: y();}", "a");
}

#[test]
fn testExceptionThrowingAssignments() {
    assertLiveBeforeX("try{var a; X:a=foo();a} catch(e) {e()}", "a");
    assertLiveBeforeX("try{X:var a=foo();a} catch(e) {e()}", "a");
    assertLiveBeforeX("try{X:var a=foo()} catch(e) {e(a)}", "a");
}

#[test]
fn testInnerFunctions() {
    assertLiveBeforeX("function a() {}; X: a()", "a");
    assertNotLiveBeforeX("X:; function a() {}", "a");
    assertLiveBeforeX("a = function(){}; function a() {}; X: a()", "a");
    // NOTE: function a() {} has no CFG node representation since it is not
    // part of the control execution.
    assertLiveAfterX("X: a = function(){}; function a() {}; a()", "a");
    assertNotLiveBeforeX("X: a = function(){}; function a() {}; a()", "a");
}

#[test]
fn testEscaped() {
    assertEscaped("var a;function b(){a()}", "a");
    assertEscaped("var a;function b(){param1()}", "param1");
    assertEscaped("var a;function b(){function c(){a()}}", "a");
    assertEscaped("var a;function b(){param1.x = function() {a()}}", "a");
    assertNotEscaped("var a;function b(){var c; c()}", "c");
    assertNotEscaped("var a;function f(){function b(){var c;c()}}", "c");
    assertNotEscaped("var a;function b(){};a()", "a");
    assertNotEscaped("var a;function f(){function b(){}}a()", "a");
    assertNotEscaped("var a;function b(){var a;a()};a()", "a");

    // Escaped by exporting.
    //   assertEscaped("var _x", "_x");
}

// ES6 does not require separate handling for catch because the catch block is already recognized
// by the scope creator
#[test]
fn testNotEscapedWithCatch() {
    assertEscaped("try{} catch(e){}", "e");
}

#[test]
fn testEscapedLiveness() {
    assertNotLiveBeforeX("var a;X:a();function b(){a()}", "a");
}

#[test]
fn testBug1449316() {
    assertLiveBeforeX("try {var x=[]; X:var y=x[0]} finally {foo()}", "x");
}

#[test]
fn testSimpleLet() {
    // a is defined after X and not used
    assertNotLiveBeforeDecl("let a;", "a");
    assertNotLiveAfterDecl("let a;", "a");
    assertNotLiveAfterDecl("let a=1;", "a");

    // a is used and defined after X
    assertLiveAfterDecl("let a=1; a()", "a");
    assertNotLiveBeforeDecl("let a=1; a()", "a");

    // no assignment to x; let is initialized with undefined
    assertLiveBeforeX("let a;X:a;", "a");
    assertNotLiveAfterX("let a,b;X:b();", "a");
    assertLiveBeforeX("let a,b;X:b(a);", "a");
    assertNotLiveBeforeX("let a,b;X:a=1;b(a)", "a");
    assertNotLiveAfterX("let a,b;X:b(a);b()", "a");
    assertLiveBeforeX("let a,b;X:b();b=1;a()", "b");

    // let initialized afterX
    assertLiveAfterX("X:a();let a;a()", "a");
    assertNotLiveAfterX("X:a();let a=1;a()", "a");
}

#[test]
fn testLetInnerBlock() {
    assertNotLiveAfterX("let x; { X:x = 2; let y; }", "x");
}

#[test]
fn testSimpleConst() {
    // a is defined after X and not used
    assertLiveBeforeX("const a = 4; X:a;", "a");
    assertNotLiveBeforeDecl("let a = 1;", "a");
    assertNotLiveBeforeDecl("const a = 1;", "a");
    assertNotLiveAfterDecl("const a = 1;", "a");
}

#[test]
fn testArrayDestructuring() {
    assertLiveBeforeX("var [a, b] = [1, 2]; X:a;", "a");
    assertNotLiveBeforeX("X: var [...a] = f();", "a");
    assertNotEscaped("var [a, ...b] = [1, 2];", "b");
    assertNotEscaped("var [a, ...b] = [1, 2];", "a");
    assertNotEscaped("var [a, ,b] = [1, 2, 3];", "a");
    assertNotEscaped("var [a, ,b] = [1, 2, 3];", "b");
    assertNotLiveBeforeX("var x = 3; X: [x] = [4]; x;", "x");
    assertLiveBeforeX("var x = {}; X: [x.a] = [3]; x.a;", "x");
    assertLiveBeforeX("var x = []; X: var [c] = x;", "x");
}

#[test]
fn testObjectDestructuring() {
    assertLiveBeforeX("var {a: x, b: y} = g(); X:x", "x");
    assertNotLiveBeforeX("X: var {a: x, b: y} = g();", "y");
    assertNotEscaped("var {a: x, b: y} = g()", "x");
    assertNotEscaped("var {a: x, b: y} = g()", "y");
    assertNotEscaped("var {a: x = 3, b: y} = g();", "x");
    assertNotLiveBeforeX("var x = {}; X: ({x} = {}); x;", "x");
    assertLiveBeforeX("var x = {}; X: ({a: x.a} = {}); x.a;", "x");
    assertLiveBeforeX("var x = {}; X: var {c} = x;", "x");
}

#[test]
fn testComplexDestructuringPattern() {
    assertLiveBeforeX("var x = 3; X: var [y = x] = [];", "x");
    assertLiveBeforeX("var x = 3, y; X: [y = x] = [];", "x");
    assertLiveBeforeX("var x = 3; X: var {y = x} = {};", "x");
    assertLiveBeforeX("var x = 3; X: var {key: y = x} = {};", "x");
    assertLiveBeforeX("var x = 3; X: var {[x + x]: foo} = obj; x;", "x");
    assertLiveBeforeX("var x = 3; X: var {[x + x]: x} = obj; x;", "x");
}

#[test]
fn testComplicatedDeclaration() {
    assertNotEscaped("var a = 1, {b: b} = f(), c = g()", "a");
    assertNotEscaped("var a = 1, {b: b} = f(), c = g()", "b");
    assertNotEscaped("var a = 1, {b: b} = f(), c = g()", "c");
}

fn assertLiveBeforeX(src: &str, var: &str) {
    assertLiveBeforeXAsync(src, var, false);
}

fn assertLiveBeforeXAsync(src: &str, var: &str, is_async: bool) {
    with_liveness(src, is_async, |liveness, vars| {
        let state = getFlowStateAtX(liveness);
        assert!(
            state.is_some(),
            "Label X should be in the input program: `{}`",
            src
        );

        let in_ = &liveness.data_flow_analysis.inner[state.unwrap().in_];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_before = in_.isLive(liveness.getVarIndex(&var_id).unwrap());

        assert!(is_live_before, "Variable `{}` should be live before X", var);
    });
}

fn assertLiveAfterX(src: &str, var: &str) {
    assertLiveAfterXAsync(src, var, false);
}

fn assertLiveAfterXAsync(src: &str, var: &str, is_async: bool) {
    with_liveness(src, is_async, |liveness, vars| {
        let state = getFlowStateAtX(liveness);
        assert!(
            state.is_some(),
            "Label X should be in the input program: `{}`",
            src
        );

        let out = &liveness.data_flow_analysis.inner[state.unwrap().out];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_after = out.isLive(liveness.getVarIndex(&var_id).unwrap());

        assert!(is_live_after, "Variable `{}` should be live after X", var);
    });
}

fn assertNotLiveAfterX(src: &str, var: &str) {
    with_liveness(src, false, |liveness, vars| {
        let state = getFlowStateAtX(liveness);
        assert!(
            state.is_some(),
            "Label X should be in the input program: `{}`",
            src
        );

        let out = &liveness.data_flow_analysis.inner[state.unwrap().out];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_after = out.isLive(liveness.getVarIndex(&var_id).unwrap());

        assert!(
            !is_live_after,
            "Variable `{}` should not be live after X",
            var
        );
    });
}

fn assertNotLiveBeforeX(src: &str, var: &str) {
    with_liveness(src, false, |liveness, vars| {
        let state = getFlowStateAtX(liveness);
        assert!(
            state.is_some(),
            "Label X should be in the input program: `{}`",
            src
        );

        let in_ = &liveness.data_flow_analysis.inner[state.unwrap().in_];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_before = in_.isLive(liveness.getVarIndex(&var_id).unwrap());

        assert!(
            !is_live_before,
            "Variable `{}` should not be live before X",
            var
        );
    });
}

fn assertLiveAfterDecl(src: &str, var: &str) {
    with_liveness(src, false, |liveness, vars| {
        let state = getFlowStateAtDeclaration(liveness, var);
        assert!(state.is_some(), "Variable `{}` should be declared", var);

        let out = &liveness.data_flow_analysis.inner[state.unwrap().out];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_after = out.isLive(liveness.getVarIndex(&var_id).unwrap());

        assert!(
            is_live_after,
            "Variable `{}` should be live after its declaration",
            var
        );
    });
}

fn assertNotLiveAfterDecl(src: &str, var: &str) {
    with_liveness(src, false, |liveness, vars| {
        let state = getFlowStateAtDeclaration(liveness, var);
        assert!(state.is_some(), "Variable `{}` should be declared", var);

        let out = &liveness.data_flow_analysis.inner[state.unwrap().out];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_after = out.isLive(liveness.getVarIndex(&var_id).unwrap());

        assert!(
            !is_live_after,
            "Variable `{}` should be live after its declaration",
            var
        );
    });
}

fn assertNotLiveBeforeDecl(src: &str, var: &str) {
    with_liveness(src, false, |liveness, vars| {
        let state = getFlowStateAtDeclaration(liveness, var);
        assert!(state.is_some(), "Variable `{}` should be declared", var);

        let in_ = &liveness.data_flow_analysis.inner[state.unwrap().in_];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_before = in_.isLive(liveness.getVarIndex(&var_id).unwrap());

        assert!(
            !is_live_before,
            "Variable `{}` should not be live before its declaration",
            var
        );
    });
}

fn with_liveness<F>(src: &str, is_async: bool, mut op: F)
where
    F: FnMut(&mut LiveVariablesAnalysis<Function>, &FxHashMap<JsWord, Id>),
{
    GLOBALS.set(&Globals::new(), || {
        // Set up test case
        let src = if is_async {
            format!(
                "async function _FUNCTION(param1, param2 = 1, ...param3){{{}}}",
                src
            )
        } else {
            format!(
                "function _FUNCTION(param1, param2 = 1, ...param3){{{}}}",
                src
            )
        };
        let mut program = Program::Script(parse_script(&src));

        let unresolved_mark = Mark::new();
        let top_level_mark = Mark::new();

        program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark, false));

        let script = unwrap_as!(program, Program::Script(s), s);

        let function = match script.body.first() {
            Some(Stmt::Decl(Decl::Fn(f))) => &f.function,
            _ => unreachable!(),
        };

        // Control flow graph
        let cfa = ControlFlowAnalysis::analyze(ControlFlowRoot::Function(function), false);

        // All variables declared in function
        let allVarsDeclaredInFunction = find_vars_declared_in_fn(function, false);

        let vars = allVarsDeclaredInFunction
            .ordered_vars
            .iter()
            .map(|id| (id.0.clone(), id.clone()))
            .collect::<FxHashMap<JsWord, Id>>();

        let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

        // Compute liveness of variables
        let mut liveness =
            LiveVariablesAnalysis::new(cfa, function, allVarsDeclaredInFunction, unresolved_ctxt);
        liveness.data_flow_analysis.analyze();

        op(&mut liveness, &vars);
    });
}

fn getFlowStateAtX<'a>(
    liveness: &'a LiveVariablesAnalysis<Function>,
) -> Option<&'a LinearFlowState> {
    let mut v = FlowStateFinder {
        liveness,
        flow_state: None,
        predicate: |stmt| {
            if let Stmt::Labeled(labeled) = stmt {
                if &labeled.label.sym == "X" {
                    let body = Node::from(labeled.body.as_ref());
                    return liveness
                        .data_flow_analysis
                        .inner
                        .cfg
                        .node_annotations
                        .get(&body);
                }
            }
            None
        },
    };
    liveness
        .data_flow_analysis
        .inner
        .cfg
        .entry
        .visit_with(&mut v);
    v.flow_state
}

struct FlowStateFinder<'a, 'ast, P>
where
    P: Fn(&'ast Stmt) -> Option<&'a LinearFlowState>,
{
    liveness: &'a LiveVariablesAnalysis<'ast, 'a, Function>,
    flow_state: Option<&'a LinearFlowState>,
    predicate: P,
}

impl<'a, 'ast, P> Visit<'ast> for FlowStateFinder<'a, 'ast, P>
where
    P: Fn(&'ast Stmt) -> Option<&'a LinearFlowState>,
{
    noop_visit_type!();

    // fn visit_labeled_stmt(&mut self, node: &'ast LabeledStmt) {
    //     if self.flow_state.is_none() {
    //         if &node.label.sym == "X" {
    //             let body = Node::from(node.body.as_ref());
    //             self.flow_state = self
    //                 .liveness
    //                 .data_flow_analysis
    //                 .cfg
    //                 .node_annotations
    //                 .get(&body);
    //         } else {
    //             node.body.visit_with(self);
    //         }
    //     }
    // }

    fn visit_stmt(&mut self, node: &'ast Stmt) {
        if self.flow_state.is_none() {
            let new_flow_state = (self.predicate)(node);
            if new_flow_state.is_some() {
                self.flow_state = new_flow_state;
            } else {
                node.visit_children_with(self);
            }
        }
    }
    fn visit_expr(&mut self, node: &'ast Expr) {
        if self.flow_state.is_none() {
            node.visit_children_with(self);
        }
    }
}

/**
 * Use this for lexical declarations which can't be labelled; e.g. `LABEL: let x = 0;` is invalid
 * syntax.
 */
fn getFlowStateAtDeclaration<'a>(
    liveness: &'a LiveVariablesAnalysis<Function>,
    name: &str,
) -> Option<&'a LinearFlowState> {
    let mut v = FlowStateFinder {
        liveness,
        flow_state: None,
        predicate: |stmt| {
            if let Stmt::Decl(Decl::Var(d)) = stmt {
                assert!(d.decls.len() == 1);
                let decl = d.decls.first().unwrap();
                if let Pat::Ident(n) = &decl.name {
                    if &n.id.sym == name {
                        let decl = Node::VarDecl(d);
                        return liveness
                            .data_flow_analysis
                            .inner
                            .cfg
                            .node_annotations
                            .get(&decl);
                    }
                }
            }
            None
        },
    };
    liveness
        .data_flow_analysis
        .inner
        .cfg
        .entry
        .visit_with(&mut v);
    v.flow_state
}

fn assertEscaped(src: &str, name: &str) {
    with_liveness(src, false, |liveness, vars| {
        let var = var_name_to_id(name, vars);
        let escaped = var.and_then(|v| liveness.data_flow_analysis.inner.escaped.get(&v));
        assert!(
            escaped.is_some(),
            "Variable {} should be in the escaped local list.",
            name
        );
    });
}

fn assertNotEscaped(src: &str, name: &str) {
    with_liveness(src, false, |liveness, vars| {
        let var = var_name_to_id(name, vars);
        let escaped = var.and_then(|v| liveness.data_flow_analysis.inner.escaped.get(&v));
        assert!(
            escaped.is_none(),
            "Variable {} should not be in the escaped local list.",
            name
        );
    });
}

fn parse_script(input: &str) -> Script {
    let cm = Lrc::<SourceMap>::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

    let fm = cm.new_source_file(FileName::Real("input".into()), input.into());

    let mut p = Parser::new(Syntax::Es(Default::default()), &fm, Default::default());
    let res = match p.parse_script() {
        Ok(p) => p,
        Err(e) => {
            e.into_diagnostic(&handler).emit();
            panic!("Failed to parse");
        }
    };

    let mut error = false;

    for e in p.take_errors() {
        e.into_diagnostic(&handler).emit();
        error = true;
    }

    if error {
        panic!("Failed to parse");
    }

    res
}

fn var_name_to_id(var: &str, vars: &FxHashMap<JsWord, Id>) -> Option<Id> {
    vars.get(&JsWord::from(var)).cloned()
}
