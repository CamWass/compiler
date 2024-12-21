use ast::*;
use ecma_visit::{Visit, VisitWith};
use global_common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    FileName, Globals, SourceMap, GLOBALS,
};
use parser::{Parser, Syntax};

use crate::control_flow::{
    ControlFlowAnalysis::{ControlFlowAnalysis, ControlFlowRoot},
    ControlFlowGraph::DefaultPrinter,
};
use crate::Id;
use crate::{find_vars::find_vars_declared_in_fn, utils::unwrap_as};

use super::*;

/**
 * Tests for {@link MaybeReachingVariableUse}.
 *
 * <p>Each test case consist of a short code snippet that has an instruction labeled with `D:` and
 * one or more with label starting with `U:`. When `assertMatch` is called, the test suite verifies
 * that all the uses with label starting with `U:` is a may be reaching use of the definition label
 * at `D:`.
 */
// TODO(rishipal): Consider classifying these tests based on the position of `D` and `U` in input.

// Illustrates that the label D needn't correspond to a definition of `x` in these tests, but is
// symbolic of any program point at which we want to check the use U is upward exposed (reachable)
#[test]
fn testReachableUseUAtProgramPointD() {
    assert_match("var x = 1; D: var y = 2; y; U:x");
    assert_not_match("D: var y = 1; var x = 2; y; U:x");
}

#[test]
fn testStraightLine() {
    assert_match("D:var x=1; U: x");
    assert_match("var x; D:x=1; U: x");
    assert_not_match("D:var x=1; x = 2; U: x");
    assert_match("var x=1; D:x=2; U: x");
    assert_not_match("U:x; D:var x = 1");
    assert_match("D: var x = 1; var y = 2; y; U:x");
}

#[test]
fn testIf() {
    assert_match("var x; if(a){ D:x=1 }else { x=2 }; U:x");
    assert_match("var x; if(a){ x=1 }else { D:x=2 }; U:x");
    assert_match("D:var x=1; if(a){ U1: x }else { U2: x };");
    // Original def is redefined along all paths to the use, hence not reachable.
    assert_not_match("D: var x; if(a){ x=1 }else { x=2 }; U:x");
}

#[test]
fn testLoops() {
    assert_match("var x=0; while(a){ D:x=1 }; U:x");
    assert_match("var x=0; for(;cond;) { D:x=1 }; U:x");

    // statically infinite loops don't have a CFG edge to the end
    assert_not_match("var x=0; for(;;) { D:x=1 }; U:x");
    assert_not_match("var x=0; for(;true;) { D:x=1 }; U:x");
    assert_not_match("var x=0; while (true) { D:x=1 }; U:x");
    // even un-obscured(exposed) defs before the infinite loop don't reach their uses
    assert_not_match("D: var x=0; while (true) { y=1 }; U:x");

    assert_match("D:var x=1; while(a) { U:x }");
    assert_match("D:var x=1; for(;;)  { U:x }");
}

// This test shows that MaybeReachingVariableUseTest does not use data flow(values) in
// conditionals but relies only on static CFG edges to find whether a use reaches a def.
// TODO(rishipal): Make Control flow analysis smarter about short circuiting and update this test.
#[test]
fn testShortCircuiting_usesOnlyCFGEdges() {
    // Even though `(x=1)` will never execute at runtime in the following cases, it is conditionally
    // executed in  static analysis (=no dataflow) , i.e. there exists a static CFG path from D->U
    // that does not redefine `x`. Hence we add U to the "may-be" reaching use of D.
    assert_match("var x=0; D: var y = false && (x=1); U:x");
    assert_match("var x=0; D: var y = true || (x=1); U:x");
    assert_match("var x=0; var y=0; D:(y=0)&&(x=1); U:x");

    // Even though `(x=1)` will always execute at runtime, it is conditionally executed in  static
    // analysis (=no dataflow) , i.e. there exists a static CFG path from D->U which adds U to the
    // "may-be" reaching use of D.
    assert_match("D: var x=0; var y = true && (x=1); U:x");
    assert_match("D: var x=0; var y = false || (x=1); U:x");
}

// #[test]
fn testConditional() {
    // Def on LHS is unconditional
    assert_match("var x=0; var y; D:(x=1)&&y; U:x");
    assert_not_match("D: var x=0; var y; (x=1)&&(y); U:x");

    // Even though `(x=1)` will always execute at runtime, it is conditionally executed in static
    // analysis (=no dataflow) , i.e. there exists a static CFG path from D->U that does not
    // redefine `x`. Hence we add U to the "may-be" reaching use of D.
    assert_match("D: var x=0; var y=0; (y=1)&&((y=2)||(x=1)); U:x");
    assert_match("D: var x=0; var y=1; (y)&&(x=1); U:x");
}

#[test]
fn nullishCoalesce() {
    // LHS always executed
    assert_match("var x=0; var y; D:(x=1)??y; U:x");
    assert_not_match("D: var x=0; var y; (x=1)??(y); U:x");

    // Even though `(x=1)` will always execute at runtime as `y` is undefined, it is conditionally
    // executed in  static analysis (=no dataflow) , i.e. there exists a static CFG path from D->U
    // that does not redefine `x`. Hence we add U to the "may-be" reaching use of D.
    assert_match("var x=0; var y; D:y??(x=1); U:x");
    assert_match("D: var x=0; var y; y??(x=1); U:x");
    assert_match("D: var x=0; var y; (y)??((y)||(x=1)); U:x");

    // Even though `(x=1)` will never execute at runtime in the following cases, it is conditionally
    // executed in  static analysis (=no dataflow), i.e. there exists a static CFG path from D->U
    // that does not redefine `x`. Hence we add U to the "may-be" reaching use of D.
    assert_match("var x=0; var y=1; D:(y=1)??(x=1); U:x");
    assert_match("D: var x=0; var y; (y)&&((y)??(x=1)); U:x");
}

#[test]
fn optChain() {
    // LHS always executed
    assert_match("var x=0; var y; D:(x=1)?.y; U:x");
    assert_match("var x=0; var y=0; D:(x=1)?.(y=0); U:x");
    assert_not_match("D: var x=0; var y=0; (x=1)?.(y=0); U:x");

    // one of the paths will reach the definition
    assert_match("var x=0; var y=0; D:(y=0)?.(x=1); U:x");
    assert_match("var x=0; var y; D:y?.(x=1); U:x");

    // Flow analysis isn't smart enough to recognize that `(x=1)` here gets short circuited and
    // never executes at runtime
    assert_match("var x=0; var y; D: y?.(true||(x=1)); U:x");

    // RHS not always executed due to short circuiting
    assert_match("D: var x=0; var y; y?.((y=2)||(x=1)); U:x");
    assert_match("D: var x=0; var y; y?.(x=1); U:x");
}

#[test]
fn testUseAndDefInSameInstruction() {
    assert_not_match("D:var x=0; U:x=1,x");
    assert_match("D:var x=0; U:x,x=1");
}

#[test]
fn testAssignmentInExpressions() {
    assert_match("var x=0; D:foo(bar(x=1)); U:x");
    assert_match("var x=0; D:foo(bar + (x = 1)); U:x");
}

#[test]
fn testHook() {
    assert_match("var x=0; D:foo() ? x=1 : bar(); U:x");
    assert_match("var x=0; D:foo() ? x=1 : x=2; U:x");
    // TODO(rishipal): Fix this test. The U should not be reachable to D as D is obscured by redef.
    assert_match("D: var x=0; foo() ? x=1 : x=2; U:x");
}

#[test]
fn testAssignmentOps() {
    assert_not_match("D: var x = 0; U: x = 100");
    assert_match("D: var x = 0; U: x += 100");
    assert_match("D: var x = 0; U: x -= 100");
    assert_not_match("D: var x = 0; x+=10; U:x");
}

#[test]
fn testInc() {
    assert_match("D: var x = 0; U:x++");
    assert_match("var x = 0; D:x++; U:x");
    // TODO(rishipal): Fix this test. The U should not be reachable to D as D is obscured by redef.
    assert_match("D: var x = 0; x++; U:x");
}

#[test]
fn testForIn() {
    // Uses within FOR-IN header are hard to test. They are covered
    // by the tests in the flow sensitive inliner.
    assert_not_match("D: var x = [], foo; U: for (x in foo) { }");
    assert_not_match("D: var x = [], foo; for (x in foo) { U:x }");
    assert_match("var x = [], foo; D: for (x in foo) { U:x }");
    assert_match("var foo; D: for (let x in foo) { U:x }");
    assert_match("var foo; D: for (const x in foo) { U:x }");
    assert_match("D: var x = 1, foo; U: x; U: for (let [z = x] in foo) {}");
    assert_match("D: var x = 1, foo; U: x; for (let [x] in foo) {}");
}

#[test]
fn testForOf() {
    assert_not_match("D: var x = [], foo; U: for (x of foo) { }");
    assert_not_match("D: var x = [], foo; for (x of foo) { U:x }");
    assert_match("var x = [], foo; D: for (x of foo) { U:x }");
    assert_match("var foo; D: for (let x of foo) { U:x }");
    assert_match("var foo; D: for (const x of foo) { U:x }");
    assert_match("D: var x = 1, foo; U: x; U: for (let [z = x] of foo) {}");
    assert_match("D: var x = 1, foo; U: x; for (let [x] of foo) {}");
}

#[test]
fn testForAwaitOf() {
    assert_not_async_match("D: var x = [], foo; U: for await (x of foo) { }");
    assert_not_async_match("D: var x = [], foo; for await (x of foo) { U:x }");
    assert_async_match("var x = [], foo; D: for await (x of foo) { U:x }");
    assert_async_match("var foo; D: for await (let x of foo) { U:x }");
    assert_async_match("var foo; D: for await (const x of foo) { U:x }");
    assert_async_match("D: var x = 1, foo; U: x; U: for await (let [z = x] of foo) {}");
    assert_async_match("D: var x = 1, foo; U: x; for await (let [x] of foo) {}");
}

#[test]
fn testTryCatch() {
    assert_match(
        "
D: var x = 1;
try { U: var y = foo() + x; } catch (e) {}
U: var z = x;",
    );

    assert_match(
        "
D: var x = 1;
try { x=2; U: var y = foo() + x; } catch (e) {} ",
    );

    // TODO(rishipal): Fix this test. The U should not be reachable to D as D is obscured by redef.
    assert_match(
        "
D: var x = 1;
try { x=2; U: var y = foo() + x; } catch (e) {}
U:x;",
    );
}

#[test]
fn testDestructuring() {
    assert_match("D: var x = 1; U: var [y = x] = [];");
    assert_match("D: var x = 1; var y; U: [y = x] = [];");
    assert_match("D: var [x] = []; U: x;");
    assert_match("var x; x = 3; D: [x] = 5; U: x;");
    assert_not_match("D: var x; x = 3; [x] = 5; U: x;");
}

fn assert_match(src: &str) {
    assert_match_inner(src, false);
}

fn assert_async_match(src: &str) {
    assert_match_inner(src, true);
}

/** The def of `x` at D: may be used by the read of `x` at U:. */
fn assert_match_inner(src: &str, is_async: bool) {
    with_maybe_reaching_uses(src, is_async, |function, maybe_reaching_uses| {
        let extracted_info = extract_def_and_uses_from_input_labels(function, "x");
        let computed_uses = get_computed_uses(maybe_reaching_uses, &extracted_info);
        let extracted_uses = extracted_info.extracted_uses;
        assert!(extracted_uses.iter().all(|u| computed_uses.contains(u)));
    });
}

fn assert_not_match(src: &str) {
    assert_not_match_inner(src, false);
}

fn assert_not_async_match(src: &str) {
    assert_not_match_inner(src, true);
}

/** The def of `x` at D: is not used by the read of `x` at U:. */
fn assert_not_match_inner(src: &str, is_async: bool) {
    with_maybe_reaching_uses(src, is_async, |function, maybe_reaching_uses| {
        let extracted_info = extract_def_and_uses_from_input_labels(function, "x");
        let computed_uses = get_computed_uses(maybe_reaching_uses, &extracted_info);
        let extracted_uses = extracted_info.extracted_uses;
        assert!(extracted_uses.iter().any(|u| !computed_uses.contains(u)));
    });
}

/**
 * Returns may-be-reaching uses of definition of variable `x` on the node extracted at label `D:`.
 */
fn get_computed_uses<'ast>(
    reaching_use: &MaybeReachingResult<'ast>,
    extracted_info: &ExtractedInfo,
) -> Vec<NodeId> {
    reaching_use
        .get_uses(&extracted_info.var_id, extracted_info.extracted_def)
        .cloned()
        .unwrap_or_default()
}

/**
 * Runs `MaybeReachingVariableUse` pass to compute and store the may-be-reaching uses for all
 * definitions of each variable in the test source.
 */
fn with_maybe_reaching_uses<F>(src: &str, is_async: bool, mut op: F)
where
    F: FnMut(&Function, &MaybeReachingResult<'_>),
{
    GLOBALS.set(&Globals::new(), || {
        // Set up test case
        let async_str = if is_async { "async" } else { "" };
        let source = format!("{async_str} function _FUNCTION(param1, param2){{{src}}}",);
        let program = Program::Script(parse_script(&source));

        // let unresolved_mark = Mark::new();
        // let top_level_mark = Mark::new();

        // program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark, false));

        // let mut node_id_gen = NodeIdGen::default();

        // crate::normalize_properties::normalize_properties(&mut program, &mut node_id_gen);
        // crate::normalize::normalize(&mut program, &mut node_id_gen);

        let script = unwrap_as!(program, Program::Script(s), s);

        let function = match script.body.first() {
            Some(Stmt::Decl(Decl::Fn(f))) => &f.function,
            _ => unreachable!(),
        };

        // Control flow graph
        let cfa = ControlFlowAnalysis::analyze(ControlFlowRoot::Function(function), false);

        // All variables declared in function
        let all_vars_declared_in_function = find_vars_declared_in_fn(function, false);

        let result = MaybeReachingVariableUse::new(
            cfa.cfg,
            &cfa.node_priorities,
            function,
            all_vars_declared_in_function,
        )
        .analyze();

        result
            .cfg
            .print_full_with_annotations::<DefaultPrinter>(None);

        op(function, &result);
    });
}

fn parse_script(input: &str) -> Script {
    let cm = Lrc::<SourceMap>::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

    let fm = cm.new_source_file(FileName::Real("input".into()), input.into());

    let mut program_data = Default::default();
    let mut p = Parser::new(Syntax::Es(Default::default()), &fm, &mut program_data);
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

// Run `LabelFinder` to find the `D:` and `U:` labels and save the def and uses of `x`
fn extract_def_and_uses_from_input_labels<'ast>(
    function: &'ast Function,
    var_name: &'static str,
) -> ExtractedInfo<'ast> {
    let mut extractor = InfoExtractor::new(var_name);
    function.visit_with(&mut extractor);

    assert!(
        extractor.extracted_def.is_some(),
        "Code should have an instruction labeled D"
    );
    assert!(
        !extractor.extracted_uses.is_empty(),
        "Code should have an instruction label starting with U"
    );

    assert!(
        extractor.var_id.is_some(),
        "Code should have a variable called {}",
        var_name
    );

    ExtractedInfo {
        extracted_def: extractor.extracted_def.unwrap(),
        extracted_uses: extractor.extracted_uses,
        var_id: extractor.var_id.unwrap(),
    }
}

struct ExtractedInfo<'ast> {
    // Def and uses extracted from `D:` and `U:` labels respectively
    extracted_def: Node<'ast>,
    extracted_uses: Vec<NodeId>,

    var_id: Id,
}

/** Finds the D: and U: label and store which node they point to. */
struct InfoExtractor<'ast> {
    // Def and uses extracted from `D:` and `U:` labels respectively
    extracted_def: Option<Node<'ast>>,
    extracted_uses: Vec<NodeId>,

    var_name: &'static str,
    var_id: Option<Id>,
}

impl InfoExtractor<'_> {
    fn new(var_name: &'static str) -> Self {
        Self {
            extracted_def: None,
            extracted_uses: Vec::new(),
            var_name,
            var_id: None,
        }
    }
}

impl<'ast> Visit<'ast> for InfoExtractor<'ast> {
    fn visit_binding_ident(&mut self, node: &'ast BindingIdent) {
        if &node.id.sym == self.var_name {
            self.var_id = Some(node.id.to_id());
        }
    }

    fn visit_labeled_stmt(&mut self, node: &'ast LabeledStmt) {
        if &node.label.sym == "D" {
            assert!(self.extracted_def == None, "Multiple D: labels in test src");
            self.extracted_def = Some(Node::from(node.body.as_ref()));
        } else if node.label.sym.starts_with("U") {
            self.extracted_uses.push(node.body.node_id());
        }
        node.body.visit_with(self);
    }
}
