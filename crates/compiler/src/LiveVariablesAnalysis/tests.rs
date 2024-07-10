use ast::*;
use atoms::JsWord;
use ecma_visit::{Visit, VisitMutWith, VisitWith};
use global_common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    FileName, Globals, Mark, SourceMap, SyntaxContext, GLOBALS,
};
use parser::{Parser, Syntax};
use rustc_hash::FxHashMap;

use crate::control_flow::node::Node;
use crate::control_flow::ControlFlowAnalysis::{ControlFlowAnalysis, ControlFlowRoot};
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
    assert_not_live_before_x("X:var a;", "a");
    assert_not_live_after_x("X:var a;", "a");
    assert_not_live_after_x("X:var a=1;", "a");
    assert_live_after_x("X:var a=1; a()", "a");
    assert_not_live_before_x("X:var a=1; a()", "a");
    assert_live_before_x("var a;X:a;", "a");
    assert_live_before_x("var a;X:a=a+1;", "a");
    assert_live_before_x("var a;X:a+=1;", "a");
    assert_live_before_x("var a;X:a++;", "a");
    assert_not_live_after_x("var a,b;X:b();", "a");
    assert_not_live_before_x("var a,b;X:b();", "a");
    assert_live_before_x("var a,b;X:b(a);", "a");
    assert_live_before_x("var a,b;X:b(1,2,3,b(a + 1));", "a");
    assert_not_live_before_x("var a,b;X:a=1;b(a)", "a");
    assert_not_live_after_x("var a,b;X:b(a);b()", "a");
    assert_live_before_x("var a,b;X:b();b=1;a()", "b");
    assert_live_after_x("X:a();var a;a()", "a");
    assert_not_live_after_x("X:a();var a=1;a()", "a");
    assert_live_before_x("var a,b;X:a,b=1", "a");
}

#[test]
fn testProperties() {
    // Reading property of a local variable makes that variable live.
    assert_live_before_x("var a,b;X:a.P;", "a");

    // Assigning to a property doesn't kill "a". It makes it live instead.
    assert_live_before_x("var a,b;X:a.P=1;b()", "a");
    assert_live_before_x("var a,b;X:a.P.Q=1;b()", "a");

    // An "a" in a different context.
    assert_not_live_after_x("var a,b;X:b.P.Q.a=1;", "a");

    assert_live_before_x("var a,b;X:b.P.Q=a;", "a");
}

#[test]
fn testConditions() {
    // Reading the condition makes the variable live.
    assert_live_before_x("var a,b;X:if(a){}", "a");
    assert_live_before_x("var a,b;X:if(a||b) {}", "a");
    assert_live_before_x("var a,b;X:if(b||a) {}", "a");
    assert_live_before_x("var a,b;X:if(b||b(a)) {}", "a");
    assert_not_live_after_x("var a,b;X:b();if(a) {}", "b");

    // We can kill within a condition as well.
    assert_not_live_after_x("var a,b;X:a();if(a=b){}a()", "a");
    assert_not_live_after_x("var a,b;X:a();while(a=b){}a()", "a");

    // The kill can be "conditional" due to short circuit.
    assert_not_live_after_x("var a,b;X:a();if((a=b)&&b){}a()", "a");
    assert_not_live_after_x("var a,b;X:a();while((a=b)&&b){}a()", "a");
    assert_live_before_x("var a,b;a();X:if(b&&(a=b)){}a()", "a"); // Assumed live.
    assert_live_before_x("var a,b;a();X:if(a&&(a=b)){}a()", "a");
    assert_live_before_x("var a,b;a();X:while(b&&(a=b)){}a()", "a");
    assert_live_before_x("var a,b;a();X:while(a&&(a=b)){}a()", "a");
}

#[test]
fn nullishCoalesce() {
    // Reading the condition makes the variable live.
    assert_live_before_x("var a,b;X:if(a??b) {}", "a");
    assert_live_before_x("var a,b;X:if(b??a) {}", "a");
    assert_live_before_x("var a,b;X:if(b??b(a)) {}", "a");

    // Unconditionally killed on lhs of ??
    assert_not_live_after_x("var a,b;X:a();if((a=b)??b){}a()", "a");
    assert_not_live_after_x("var a,b;X:a();while((a=b)??b){}a()", "a");

    // The kill can be "conditional" due to short circuit.
    assert_live_before_x("var a,b; X:if(b??(a=b)){}a()", "a"); // Assumed live.
    assert_live_before_x("var a,b; X:if(a??(a=b)){}a()", "a");
    assert_live_before_x("var a,b; X:while(b??(a=b)){}a()", "a");
    assert_live_before_x("var a,b; X:while(a??(a=b)){}a()", "a");
}

#[test]
fn logicalAssignment() {
    // TODO: comment
    // This pattern is normalized away
    assert_live_before_x("var a,b;X:a??=b", "a");
    assert_live_before_x("var a,b;X:a??=b", "b");
}

#[test]
fn optChainGetProp() {
    // Reading the var on lhs of opt chain makes the variable live.
    assert_not_live_before_x("var a,b; X:if(b) {}", "a");
    assert_live_before_x("var a,b; X:if(a?.b) {}", "a");

    // Reading a prop with the same name as var does not make the var live
    assert_not_live_before_x("var a,b;X:if(b?.a) {}", "a");

    // unconditional kill on lhs of ?.
    assert_not_live_after_x("var a,b;X:a();if((a=c)?.b){} a()", "a");
    assert_not_live_after_x("var a,b;X:a();while((a=b)?.b){} a()", "a");
}

#[test]
fn optChainCall() {
    // conditionally accessing var keeps it live
    assert_live_before_x("var a,b; X:if(b?.(a)){}", "a");

    // unconditionally overwriting a var kills it
    assert_not_live_after_x("var a,b; X:a(); if((a=b)?.b()){} a()", "a");

    // conditionally overwriting var does not kill it
    assert_live_before_x("var a,b; X:if(b?.(a=c)){} a();", "a");

    // conditional overwrite on rhs of ?. does not kill the var
    assert_live_before_x("var a,b; X:if(b?.(a=b)){}a()", "a"); // Assumed live.
    assert_live_before_x("var a,b; X:if(a?.(a=b)){}a()", "a");
    assert_live_before_x("var a,b; X:while(b?.(a=b)){}a()", "a");
    assert_live_before_x("var a,b; X:while(a?.(a=b)){}a()", "a");
}

#[test]
fn optChainGetElem() {
    // conditionally accessing var keeps it live
    assert_live_before_x("var a,b; X:if(b?.[a]) {}", "a");

    // unconditionally overwriting a var kills it
    assert_not_live_after_x("var a,b; X:a(); if((a=b)?.[b]){} a()", "a");

    // conditionally overwriting var does not kill it
    assert_live_before_x("var a,b; X:if(b?.[a=c]) {} a();", "a");

    // conditional overwrite on rhs of ?. does not kill the var
    assert_live_before_x("var a,b; X:if(b?.[a=b]){}a()", "a"); // Assumed live.
    assert_live_before_x("var a,b; X:if(a?.[a=b]){}a()", "a");
    assert_live_before_x("var a,b; X:while(b?.[a=b]){}a()", "a");
    assert_live_before_x("var a,b; X:while(a?.[a=b]){}a()", "a");
}

#[test]
fn testArrays() {
    assert_live_before_x("var a;X:a[1]", "a");
    assert_live_before_x("var a,b;X:b[a]", "a");
    assert_live_before_x("var a,b;X:b[1,2,3,4,b(a)]", "a");
    assert_live_before_x("var a,b;X:b=[a,'a']", "a");
    assert_not_live_before_x("var a,b;X:a=[];b(a)", "a");

    // Element assignment doesn't kill the array.
    assert_live_before_x("var a;X:a[1]=1", "a");
}

#[test]
fn testTwoPaths() {
    // Both Paths.
    assert_live_before_x("var a,b;X:if(b){b(a)}else{b(a)};", "a");

    // Only one path.
    assert_live_before_x("var a,b;X:if(b){b(b)}else{b(a)};", "a");
    assert_live_before_x("var a,b;X:if(b){b(a)}else{b(b)};", "a");

    // None of the paths.
    assert_not_live_after_x("var a,b;X:if(b){b(b)}else{b(b)};", "a");

    // At the very end.
    assert_live_before_x("var a,b;X:if(b){b(b)}else{b(b)}a();", "a");

    // The loop might or might not be executed.
    assert_live_before_x("var a;X:while(param1){a()};", "a");
    assert_live_before_x("var a;X:while(param1){a=1};a()", "a");

    // Same idea with if.
    assert_live_before_x("var a;X:if(param1){a()};", "a");
    assert_live_before_x("var a;X:if(param1){a=1};a()", "a");

    // This is different in DO. We know for sure at least one iteration is
    // executed.
    assert_not_live_after_x("X:var a;do{a=1}while(param1);a()", "a");
}

#[test]
fn testThreePaths() {
    assert_live_before_x("var a;X:if(1){}else if(2){}else{a()};", "a");
    assert_live_before_x("var a;X:if(1){}else if(2){a()}else{};", "a");
    assert_live_before_x("var a;X:if(1){a()}else if(2){}else{};", "a");
    assert_live_before_x("var a;X:if(1){}else if(2){}else{};a()", "a");
}

#[test]
fn testHooks() {
    assert_live_before_x("var a;X:1?a=1:1;a()", "a");

    // Unfortunately, we cannot prove the following because we assume there is
    // no control flow within a hook (i.e. no joins / set unions).
    // assertNotLiveAfterX("var a;X:1?a=1:a=2;a", "a");
    assert_live_before_x("var a,b;X:b=1?a:2", "a");
}

#[test]
fn testForLoops() {
    // Induction variable should not be live after the loop.
    assert_not_live_before_x("var a,b;for(a=0;a<9;a++){b(a)};X:b", "a");
    assert_not_live_before_x("var a,b;for(a in b){a()};X:b", "a");
    assert_not_live_before_x("var a,b;for(a in b){a()};X:a", "b");
    assert_live_before_x("var b;for(var a in b){X:a()};", "a");

    // It should be live within the loop even if it is not used.
    assert_live_before_x("var a,b;for(a=0;a<9;a++){X:1}", "a");
    assert_live_after_x("var a,b;for(a in b){X:b};", "a");
    // For-In should serve as a gen as well.
    assert_live_before_x("var a,b; X:for(a in b){ }", "a");

    // "a in b" should kill "a" before it.
    // Can't prove this unless we have branched backward DFA.
    // assertNotLiveAfterX("var a,b;X:b;for(a in b){a()};", "a");

    // Unless it is used before.
    assert_live_before_x("var a,b;X:a();b();for(a in b){a()};", "a");

    // Initializer
    assert_live_before_x("var a,b;X:b;for(b=a;;){};", "a");
    assert_not_live_before_x("var a,b;X:a;for(b=a;;){b()};b();", "b");
}

#[test]
fn testForOfLoopsVar() {
    assert_live_before_x("var a; for (a of [1, 2, 3]) {X:{}}", "a");
    assert_live_after_x("for (var a of [1, 2, 3]) {X:{}}", "a");
    assert_live_before_x("var a,b; for (var y of a = [0, 1, 2]) { X:a[y] }", "a");
}

#[test]
fn testForOfLoopsDestructuring() {
    assert_live_before_x(
        "var key, value; X:for ([key, value] of arr) {value;} value;",
        "value",
    );
    assert_live_before_x("let x = 3; X:for (var [y = x] of arr) { y; }", "x");
    assert_live_before_x("for (let [key, value] of arr) { X: key; value; }", "key");
}

#[test]
fn testForAwaitOfLoopsVar() {
    assert_live_before_xasync("var a; for await (a of [1, 2, 3]) {X:{}}", "a", true);
    assert_live_after_xasync("for await (var a of [1, 2, 3]) {X:{}}", "a", true);
    assert_live_before_xasync(
        "var a,b; for await (var y of a = [0, 1, 2]) { X:a[y] }",
        "a",
        true,
    );
}

#[test]
fn testForAwaitOfLoopsDestructuring() {
    assert_live_before_xasync(
        "var key, value; X:for await ([key, value] of arr) {value;} value;",
        "value",
        true,
    );
    assert_live_before_xasync(
        "let x = 3; X:for await (var [y = x] of arr) { y; }",
        "x",
        true,
    );
    assert_live_before_xasync(
        "for await (let [key, value] of arr) { X: key; value; }",
        "key",
        true,
    );
}

#[test]
fn testNestedLoops() {
    assert_live_before_x("var a;X:while(1){while(1){a()}}", "a");
    assert_live_before_x("var a;X:while(1){while(1){while(1){a()}}}", "a");
    assert_live_before_x("var a;X:while(1){while(1){a()};a=1}", "a");
    assert_live_after_x("var a;while(1){while(1){a()};X:a=1;}", "a");
    assert_live_after_x("var a;while(1){X:a=1;while(1){a()}}", "a");
    assert_not_live_before_x(
        "var a;X:1;do{do{do{a=1;}while(1)}while(1)}while(1);a()",
        "a",
    );
}

#[test]
fn testSwitches() {
    assert_live_before_x("var a,b;X:switch(a){}", "a");
    assert_live_before_x("var a,b;X:switch(b){case(a):break;}", "a");
    assert_live_before_x("var a,b;X:switch(b){case(b):case(a):break;}", "a");
    assert_not_live_before_x(
        "var a,b;X:switch(b){case 1:a=1;break;default:a=2;break};a()",
        "a",
    );

    assert_live_before_x("var a,b;X:switch(b){default:a();break;}", "a");
}

#[test]
fn testAssignAndReadInCondition() {
    // BUG #1358904
    // Technically, this isn't exactly true....but we haven't model control flow
    // within an instruction.
    assert_live_before_x("var a, b; X: if ((a = this) && (b = a)) {}", "a");
    assert_not_live_before_x("var a, b; X: a = 1, b = 1;", "a");
    assert_not_live_before_x("var a; X: a = 1, a = 1;", "a");
}

#[test]
fn testParam() {
    // Unused parameter should not be live.
    assert_not_live_after_x("var a;X:a()", "param1");
    assert_live_before_x("var a;X:a(param1)", "param1");
    assert_not_live_after_x("var a;X:a();a(param2)", "param1");
}

#[test]
fn testExpressionInForIn() {
    assert_live_before_x("var a = [0]; X:for (a[1] in foo) { }", "a");
}

#[test]
fn testArgumentsArray() {
    // Check that use of arguments forces the parameters into the
    // escaped set.
    assert_escaped("arguments[0]", "param1");
    assert_not_escaped("arguments[0]", "param2");
    assert_not_escaped("arguments[0]", "param3");

    assert_escaped("var args = arguments", "param1");
    assert_not_escaped("var args = arguments", "param2");
    assert_not_escaped("var args = arguments", "param3");

    assert_not_escaped("arguments = []", "param1");
    assert_not_escaped("arguments = []", "param2");
    assert_not_escaped("arguments = []", "param3");

    assert_escaped("arguments[0] = 1", "param1");
    assert_not_escaped("arguments[0] = 1", "param2");
    assert_not_escaped("arguments[0] = 1", "param3");

    assert_escaped("arguments[arguments[0]] = 1", "param1");
    assert_not_escaped("arguments[arguments[0]] = 1", "param2");
    assert_not_escaped("arguments[arguments[0]] = 1", "param3");
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
    assert_live_after_x("var a; try {X:a=1} finally {a}", "a");
    assert_live_after_x("var a; try {a()} catch(e) {X:a=1} finally {a}", "a");
    // Because the outer catch doesn't catch any exceptions at all, the read of
    // "a" within the catch block should not make "a" live.
    assert_not_live_after_x(
        "var a = 1; try { try {a()} catch(e) {X:1} } catch(E) {a}",
        "a",
    );
    assert_live_after_x("var a; while(1) { try {X:a=1;break} finally {a}}", "a");
}

#[test]
fn testForInAssignment() {
    assert_live_before_x("var a,b; for (var y in a = b) { X:a[y] }", "a");
    // No one refers to b after the first iteration.
    assert_not_live_before_x("var a,b; for (var y in a = b) { X:a[y] }", "b");
    assert_live_before_x("var a,b; for (var y in a = b) { X:a[y] }", "y");
    assert_live_after_x("var a,b; for (var y in a = b) { a[y]; X: y();}", "a");
}

#[test]
fn testExceptionThrowingAssignments() {
    assert_live_before_x("try{var a; X:a=foo();a} catch(e) {e()}", "a");
    assert_live_before_x("try{X:var a=foo();a} catch(e) {e()}", "a");
    assert_live_before_x("try{X:var a=foo()} catch(e) {e(a)}", "a");
}

#[test]
fn testInnerFunctions() {
    assert_live_before_x("function a() {}; X: a()", "a");
    assert_not_live_before_x("X:; function a() {}", "a");
    assert_live_before_x("a = function(){}; function a() {}; X: a()", "a");
    // NOTE: function a() {} has no CFG node representation since it is not
    // part of the control execution.
    assert_live_after_x("X: a = function(){}; function a() {}; a()", "a");
    assert_not_live_before_x("X: a = function(){}; function a() {}; a()", "a");
}

#[test]
fn testEscaped() {
    assert_escaped("var a;function b(){a()}", "a");
    assert_escaped("var a;function b(){param1()}", "param1");
    assert_escaped("var a;function b(){function c(){a()}}", "a");
    assert_escaped("var a;function b(){param1.x = function() {a()}}", "a");
    assert_not_escaped("var a;function b(){var c; c()}", "c");
    assert_not_escaped("var a;function f(){function b(){var c;c()}}", "c");
    assert_not_escaped("var a;function b(){};a()", "a");
    assert_not_escaped("var a;function f(){function b(){}}a()", "a");
    assert_not_escaped("var a;function b(){var a;a()};a()", "a");

    // Escaped by exporting.
    //   assertEscaped("var _x", "_x");
}

// ES6 does not require separate handling for catch because the catch block is already recognized
// by the scope creator
#[test]
fn testNotEscapedWithCatch() {
    assert_escaped("try{} catch(e){}", "e");
}

#[test]
fn testEscapedLiveness() {
    assert_not_live_before_x("var a;X:a();function b(){a()}", "a");
}

#[test]
fn testBug1449316() {
    assert_live_before_x("try {var x=[]; X:var y=x[0]} finally {foo()}", "x");
}

#[test]
fn testSimpleLet() {
    // a is defined after X and not used
    assert_not_live_before_decl("let a;", "a");
    assert_not_live_after_decl("let a;", "a");
    assert_not_live_after_decl("let a=1;", "a");

    // a is used and defined after X
    assert_live_after_decl("let a=1; a()", "a");
    assert_not_live_before_decl("let a=1; a()", "a");

    // no assignment to x; let is initialized with undefined
    assert_live_before_x("let a;X:a;", "a");
    assert_not_live_after_x("let a,b;X:b();", "a");
    assert_live_before_x("let a,b;X:b(a);", "a");
    assert_not_live_before_x("let a,b;X:a=1;b(a)", "a");
    assert_not_live_after_x("let a,b;X:b(a);b()", "a");
    assert_live_before_x("let a,b;X:b();b=1;a()", "b");

    // let initialized afterX
    assert_live_after_x("X:a();let a;a()", "a");
    assert_not_live_after_x("X:a();let a=1;a()", "a");
}

#[test]
fn testLetInnerBlock() {
    assert_not_live_after_x("let x; { X:x = 2; let y; }", "x");
}

#[test]
fn testSimpleConst() {
    // a is defined after X and not used
    assert_live_before_x("const a = 4; X:a;", "a");
    assert_not_live_before_decl("let a = 1;", "a");
    assert_not_live_before_decl("const a = 1;", "a");
    assert_not_live_after_decl("const a = 1;", "a");
}

#[test]
fn testArrayDestructuring() {
    assert_live_before_x("var [a, b] = [1, 2]; X:a;", "a");
    assert_not_live_before_x("X: var [...a] = f();", "a");
    assert_not_escaped("var [a, ...b] = [1, 2];", "b");
    assert_not_escaped("var [a, ...b] = [1, 2];", "a");
    assert_not_escaped("var [a, ,b] = [1, 2, 3];", "a");
    assert_not_escaped("var [a, ,b] = [1, 2, 3];", "b");
    assert_not_live_before_x("var x = 3; X: [x] = [4]; x;", "x");
    assert_live_before_x("var x = {}; X: [x.a] = [3]; x.a;", "x");
    assert_live_before_x("var x = []; X: var [c] = x;", "x");
}

#[test]
fn testObjectDestructuring() {
    assert_live_before_x("var {a: x, b: y} = g(); X:x", "x");
    assert_not_live_before_x("X: var {a: x, b: y} = g();", "y");
    assert_not_escaped("var {a: x, b: y} = g()", "x");
    assert_not_escaped("var {a: x, b: y} = g()", "y");
    assert_not_escaped("var {a: x = 3, b: y} = g();", "x");
    assert_not_live_before_x("var x = {}; X: ({x} = {}); x;", "x");
    assert_live_before_x("var x = {}; X: ({a: x.a} = {}); x.a;", "x");
    assert_live_before_x("var x = {}; X: var {c} = x;", "x");
}

#[test]
fn testComplexDestructuringPattern() {
    assert_live_before_x("var x = 3; X: var [y = x] = [];", "x");
    assert_live_before_x("var x = 3, y; X: [y = x] = [];", "x");
    assert_live_before_x("var x = 3; X: var {y = x} = {};", "x");
    assert_live_before_x("var x = 3; X: var {key: y = x} = {};", "x");
    assert_live_before_x("var x = 3; X: var {[x + x]: foo} = obj; x;", "x");
    assert_live_before_x("var x = 3; X: var {[x + x]: x} = obj; x;", "x");
}

#[test]
fn testComplicatedDeclaration() {
    assert_not_escaped("var a = 1, {b: b} = f(), c = g()", "a");
    assert_not_escaped("var a = 1, {b: b} = f(), c = g()", "b");
    assert_not_escaped("var a = 1, {b: b} = f(), c = g()", "c");
}

fn assert_live_before_x(src: &str, var: &str) {
    assert_live_before_xasync(src, var, false);
}

fn assert_live_before_xasync(src: &str, var: &str, is_async: bool) {
    with_liveness(src, is_async, |liveness, vars| {
        let state = get_flow_state_at_x(liveness);
        assert!(
            state.is_some(),
            "Label X should be in the input program: `{}`",
            src
        );

        let in_ = &liveness.data_flow_analysis.inner[state.unwrap().in_];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_before = in_.is_live(liveness.get_var_index(&var_id).unwrap());

        assert!(is_live_before, "Variable `{}` should be live before X", var);
    });
}

fn assert_live_after_x(src: &str, var: &str) {
    assert_live_after_xasync(src, var, false);
}

fn assert_live_after_xasync(src: &str, var: &str, is_async: bool) {
    with_liveness(src, is_async, |liveness, vars| {
        let state = get_flow_state_at_x(liveness);
        assert!(
            state.is_some(),
            "Label X should be in the input program: `{}`",
            src
        );

        let out = &liveness.data_flow_analysis.inner[state.unwrap().out];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_after = out.is_live(liveness.get_var_index(&var_id).unwrap());

        assert!(is_live_after, "Variable `{}` should be live after X", var);
    });
}

fn assert_not_live_after_x(src: &str, var: &str) {
    with_liveness(src, false, |liveness, vars| {
        let state = get_flow_state_at_x(liveness);
        assert!(
            state.is_some(),
            "Label X should be in the input program: `{}`",
            src
        );

        let out = &liveness.data_flow_analysis.inner[state.unwrap().out];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_after = out.is_live(liveness.get_var_index(&var_id).unwrap());

        assert!(
            !is_live_after,
            "Variable `{}` should not be live after X",
            var
        );
    });
}

fn assert_not_live_before_x(src: &str, var: &str) {
    with_liveness(src, false, |liveness, vars| {
        let state = get_flow_state_at_x(liveness);
        assert!(
            state.is_some(),
            "Label X should be in the input program: `{}`",
            src
        );

        let in_ = &liveness.data_flow_analysis.inner[state.unwrap().in_];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_before = in_.is_live(liveness.get_var_index(&var_id).unwrap());

        assert!(
            !is_live_before,
            "Variable `{}` should not be live before X",
            var
        );
    });
}

fn assert_live_after_decl(src: &str, var: &str) {
    with_liveness(src, false, |liveness, vars| {
        let state = get_flow_state_at_declaration(liveness, var);
        assert!(state.is_some(), "Variable `{}` should be declared", var);

        let out = &liveness.data_flow_analysis.inner[state.unwrap().out];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_after = out.is_live(liveness.get_var_index(&var_id).unwrap());

        assert!(
            is_live_after,
            "Variable `{}` should be live after its declaration",
            var
        );
    });
}

fn assert_not_live_after_decl(src: &str, var: &str) {
    with_liveness(src, false, |liveness, vars| {
        let state = get_flow_state_at_declaration(liveness, var);
        assert!(state.is_some(), "Variable `{}` should be declared", var);

        let out = &liveness.data_flow_analysis.inner[state.unwrap().out];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_after = out.is_live(liveness.get_var_index(&var_id).unwrap());

        assert!(
            !is_live_after,
            "Variable `{}` should be live after its declaration",
            var
        );
    });
}

fn assert_not_live_before_decl(src: &str, var: &str) {
    with_liveness(src, false, |liveness, vars| {
        let state = get_flow_state_at_declaration(liveness, var);
        assert!(state.is_some(), "Variable `{}` should be declared", var);

        let in_ = &liveness.data_flow_analysis.inner[state.unwrap().in_];
        let var_id = var_name_to_id(var, vars).unwrap();
        let is_live_before = in_.is_live(liveness.get_var_index(&var_id).unwrap());

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

        program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

        let script = unwrap_as!(program, Program::Script(s), s);

        let function = match script.body.first() {
            Some(Stmt::Decl(Decl::Fn(f))) => &f.function,
            _ => unreachable!(),
        };

        // Control flow graph
        let cfa = ControlFlowAnalysis::analyze(ControlFlowRoot::Function(function), false);

        // All variables declared in function
        let all_vars_declared_in_function = find_vars_declared_in_fn(function, false);

        let vars = all_vars_declared_in_function
            .ordered_vars
            .iter()
            .map(|id| (id.0.clone(), id.clone()))
            .collect::<FxHashMap<JsWord, Id>>();

        let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

        // Compute liveness of variables
        let mut liveness = LiveVariablesAnalysis::new(
            cfa.cfg,
            &cfa.node_priorities,
            function,
            all_vars_declared_in_function,
            unresolved_ctxt,
        );
        liveness.data_flow_analysis.analyze();

        op(&mut liveness, &vars);
    });
}

fn get_flow_state_at_x(liveness: &LiveVariablesAnalysis<Function>) -> Option<LinearFlowState> {
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
    v.flow_state.cloned()
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
fn get_flow_state_at_declaration<'a>(
    liveness: &'a LiveVariablesAnalysis<Function>,
    name: &str,
) -> Option<LinearFlowState> {
    let mut v = FlowStateFinder {
        liveness,
        flow_state: None,
        predicate: |stmt| {
            if let Stmt::Decl(Decl::Var(d)) = stmt {
                assert!(d.decls.len() == 1);
                let decl = d.decls.first().unwrap();
                if let Pat::Ident(n) = &decl.name {
                    if &n.id.sym == name {
                        let decl = Node::from(d);
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
    v.flow_state.cloned()
}

fn assert_escaped(src: &str, name: &str) {
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

fn assert_not_escaped(src: &str, name: &str) {
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
