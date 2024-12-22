use ast::*;
use ecma_visit::VisitMutWith;
use global_common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    FileName, Globals, Mark, SourceMap, SyntaxContext, GLOBALS,
};
use index::vec::IndexVec;
use parser::{Parser, Syntax};
use rustc_hash::FxHashMap;
use std::sync::atomic::AtomicU32;

use crate::control_flow::{ControlFlowAnalysis::*, ControlFlowGraph::Branch};
use crate::resolver::resolver;
use crate::DataFlowAnalysis::LinearFlowState;
use crate::Id;
use crate::LiveVariablesAnalysis::LiveVariablesAnalysis;
use crate::{find_vars::find_vars_declared_in_fn, utils::unwrap_as};

use super::*;

/*
 * A test suite with a very small programming language that has two types of instructions: {@link
 * BranchInstruction} and {@link ArithmeticInstruction}. Test cases must construct a small program
 * with these instructions and manually put each instruction in a {@code ControlFlowGraph}.
 *
 */

/**
 * Operations supported by ArithmeticInstruction.
 */
#[allow(dead_code)]
#[derive(Copy, Clone, Eq, Hash, PartialEq, Debug)]
enum Operation {
    ADD,
    SUB,
    DIV,
    MUL,
}

/**
 * A simple value.
 */
#[derive(Copy, Clone, Eq, Hash, PartialEq, Debug)]
enum Value {
    Variable(Variable),
    NumberValue(NumberValue),
}

/**
 * A variable.
 */
#[derive(Copy, Clone, Eq, Hash, PartialEq, Debug)]
struct Variable {
    name: &'static str,
}

impl Variable {
    fn new(name: &'static str) -> Self {
        Self { name }
    }
}

/**
 *  A number constant.
 */
#[derive(Copy, Clone, Eq, Hash, PartialEq, Debug)]
struct NumberValue {
    value: usize,
}

impl NumberValue {
    fn new(value: usize) -> Self {
        Self { value }
    }
}

/**
 * An instruction of the dummy program.
 */
#[derive(Copy, Clone, Eq, Hash, PartialEq, Debug)]
enum Instruction {
    ImplicitReturn,
    ArithmeticInstruction(ArithmeticInstruction),
    BranchInstruction(BranchInstruction),
}

impl Instruction {
    fn new_assign_number_to_variable_instruction(res: Variable, num: usize) -> Instruction {
        Instruction::ArithmeticInstruction(ArithmeticInstruction {
            result: res,
            operand1: Value::NumberValue(NumberValue::new(num)),
            operation: Operation::ADD,
            operand2: Value::NumberValue(NumberValue::new(0)),
        })
    }

    fn new_assign_variable_to_variable_instruction(lhs: Variable, rhs: Variable) -> Instruction {
        Instruction::ArithmeticInstruction(ArithmeticInstruction {
            result: lhs,
            operand1: Value::Variable(rhs),
            operation: Operation::ADD,
            operand2: Value::NumberValue(NumberValue::new(0)),
        })
    }
}

impl CfgNode for Instruction {
    fn implicit_return() -> Self {
        Instruction::ImplicitReturn
    }
}

/**
 * Basic arithmetic instruction that only takes the form of:
 *
 * <pre>
 * Result = Operand1 operator Operand2
 * </pre>
 */
#[derive(Copy, Clone, Eq, Hash, PartialEq, Debug)]
struct ArithmeticInstruction {
    operation: Operation,
    operand1: Value,
    operand2: Value,
    result: Variable,
}

/**
 * Branch instruction based on a {@link Value} as a condition.
 */
#[derive(Copy, Clone, Eq, Hash, PartialEq, Debug)]
struct BranchInstruction {
    condition: Value,
}

impl BranchInstruction {
    fn new(condition: Value) -> Self {
        Self { condition }
    }
}

/**
 * A lattice to represent constant states. Each variable of the program will
 * have a lattice defined as:
 *
 * <pre>
 *        TOP
 *   / / |         \
 *  0  1 2 3 ..... MAX_VALUE
 *  \  \ |         /
 *       BOTTOM
 * </pre>
 *
 * Where BOTTOM represents the variable is not a constant.
 * <p>
 * This class will represent a product lattice of each variable's lattice. The
 * whole lattice is store in a {@code HashMap}. If variable {@code x} is
 * defined to be constant 10. The map will contain the value 10 with the
 * variable {@code x} as key. Otherwise, {@code x} is not a constant.
 */
#[derive(Default, Clone, PartialEq, Debug)]
struct ConstPropLatticeElement {
    const_map: FxHashMap<Value, usize>,
    is_top: bool,
}

impl ConstPropLatticeElement {
    fn new(is_top: bool) -> Self {
        Self {
            const_map: Default::default(),
            is_top,
        }
    }
}

impl LatticeElement for ConstPropLatticeElement {}
impl Annotation for ConstPropLatticeElement {}

trait HasLatticeElements<L> {
    fn get_lattice_element(&self, element: LatticeElementId) -> &L;
}

struct ConstPropJoinOp<I> {
    result: Option<ConstPropLatticeElement>,
    _phantom: PhantomData<I>,
}

impl<I> Default for ConstPropJoinOp<I> {
    fn default() -> Self {
        Self {
            result: None,
            _phantom: PhantomData,
        }
    }
}

impl<I> ConstPropJoinOp<I> {
    fn apply(a: &ConstPropLatticeElement, b: &ConstPropLatticeElement) -> ConstPropLatticeElement {
        let mut result = ConstPropLatticeElement::default();
        // By the definition of TOP of the lattice.
        if a.is_top {
            return a.clone();
        }
        if b.is_top {
            return b.clone();
        }
        // Do the join for each variable's lattice.
        for var in a.const_map.keys() {
            if let Some(number) = b.const_map.get(var) {
                // The result will contain that variable as a known constant
                // if both lattice has that variable the same constant.
                if a.const_map[var] == *number {
                    result.const_map.insert(*var, *number);
                }
            }
        }
        result
    }
}

impl<I> FlowJoiner<ConstPropLatticeElement, I> for ConstPropJoinOp<I>
where
    I: HasLatticeElements<ConstPropLatticeElement>,
{
    fn join_flow(&mut self, inner: &mut I, input: LatticeElementId) {
        self.result = Some(if let Some(result) = &self.result {
            ConstPropJoinOp::<I>::apply(result, inner.get_lattice_element(input))
        } else {
            // TODO: this will make a copy of the lattice element, so changes will not be synced between 'input' and 'result'.
            inner.get_lattice_element(input).clone()
        });
    }

    fn finish(self) -> ConstPropLatticeElement {
        self.result.unwrap()
    }
}

struct DummyConstPropagationInner {
    lattice_elements: IndexVec<LatticeElementId, ConstPropLatticeElement>,
    cfg: ControlFlowGraph<Instruction, LinearFlowState>,
}

impl HasLatticeElements<ConstPropLatticeElement> for DummyConstPropagationInner {
    fn get_lattice_element(&self, element: LatticeElementId) -> &ConstPropLatticeElement {
        &self.lattice_elements[element]
    }
}

impl
    DataFlowAnalysisInner<
        Instruction,
        ConstPropLatticeElement,
        ConstPropJoinOp<DummyConstPropagationInner>,
    > for DummyConstPropagationInner
{
    fn add_lattice_element(&mut self, element: ConstPropLatticeElement) -> LatticeElementId {
        self.lattice_elements.push(element)
    }

    fn is_forward(&self) -> bool {
        true
    }

    fn create_entry_lattice(&mut self) -> LatticeElementId {
        self.add_lattice_element(ConstPropLatticeElement::default())
    }

    fn create_initial_estimate_lattice(&mut self) -> LatticeElementId {
        self.add_lattice_element(ConstPropLatticeElement::new(true))
    }

    fn create_flow_joiner(&self) -> ConstPropJoinOp<DummyConstPropagationInner> {
        ConstPropJoinOp::default()
    }

    fn flow_through(&mut self, node: Instruction, input: LatticeElementId) -> LatticeElementId {
        let elem = match node {
            Instruction::BranchInstruction(_) => self.lattice_elements[input].clone(),
            Instruction::ArithmeticInstruction(n) => {
                flow_through_arithmetic_instruction(n, &self.lattice_elements[input])
            }
            _ => unreachable!(),
        };
        self.add_lattice_element(elem)
    }
    fn cfg(&self) -> &ControlFlowGraph<Instruction, LinearFlowState> {
        &self.cfg
    }
    fn cfg_mut(&mut self) -> &mut ControlFlowGraph<Instruction, LinearFlowState> {
        &mut self.cfg
    }
}

impl Index<LatticeElementId> for DummyConstPropagationInner {
    type Output = ConstPropLatticeElement;

    fn index(&self, index: LatticeElementId) -> &Self::Output {
        &self.lattice_elements[index]
    }
}

fn flow_through_arithmetic_instruction(
    a_inst: ArithmeticInstruction,
    input: &ConstPropLatticeElement,
) -> ConstPropLatticeElement {
    let mut out = input.clone();
    // Try to see if left is a number. If it is a variable, it might already
    // be a constant coming in.
    let mut left_const = None;
    if let Value::NumberValue(n) = a_inst.operand1 {
        left_const = Some(n.value);
    } else {
        if let Some(n) = input.const_map.get(&a_inst.operand1) {
            left_const = Some(*n);
        }
    }

    // Do the same thing to the right.
    let mut right_const = None;
    if let Value::NumberValue(n) = a_inst.operand2 {
        right_const = Some(n.value);
    } else {
        if let Some(n) = input.const_map.get(&a_inst.operand2) {
            right_const = Some(*n);
        }
    }

    // If both are known constant we can perform the operation.
    if let (Some(left_const), Some(right_const)) = (left_const, right_const) {
        let const_result = match a_inst.operation {
            Operation::ADD => left_const + right_const,
            Operation::SUB => left_const - right_const,
            Operation::DIV => left_const * right_const,
            Operation::MUL => left_const / right_const,
        };
        // Put it in the map. (Possibly replacing the existing constant value)
        out.const_map
            .insert(Value::Variable(a_inst.result), const_result);
    } else {
        // If we cannot find a constant for it
        out.const_map.remove(&Value::Variable(a_inst.result));
    }
    out
}

/**
 * A simple forward constant propagation.
 */
struct DummyConstPropagation<'p> {
    data_flow_analysis: DataFlowAnalysis<
        'p,
        Instruction,
        DummyConstPropagationInner,
        ConstPropLatticeElement,
        ConstPropJoinOp<DummyConstPropagationInner>,
    >,
}

impl<'p> DummyConstPropagation<'p> {
    fn new(
        cfg: ControlFlowGraph<Instruction, LinearFlowState>,
        node_priorities: &'p [NodePriority],
    ) -> Self {
        Self {
            data_flow_analysis: DataFlowAnalysis::new(
                DummyConstPropagationInner {
                    lattice_elements: IndexVec::default(),
                    cfg,
                },
                node_priorities,
            ),
        }
    }

    fn analyze(&mut self) {
        self.data_flow_analysis.analyze();
    }

    fn verify_in_has(&self, node: Instruction, var: Variable, constant: Option<usize>) {
        let f_state = &self.data_flow_analysis.inner.cfg.node_annotations[&node];
        veritfy_lattice_element_has(&self.data_flow_analysis.inner[f_state.in_], var, constant);
    }

    fn verify_out_has(&self, node: Instruction, var: Variable, constant: Option<usize>) {
        let f_state = &self.data_flow_analysis.inner.cfg.node_annotations[&node];
        veritfy_lattice_element_has(&self.data_flow_analysis.inner[f_state.out], var, constant);
    }
}

fn veritfy_lattice_element_has(
    el: &ConstPropLatticeElement,
    var: Variable,
    constant: Option<usize>,
) {
    if let Some(constant) = constant {
        assert_eq!(el.const_map.get(&Value::Variable(var)), Some(&constant));
    } else {
        assert!(!el.const_map.contains_key(&Value::Variable(var)));
    }
}

#[test]
fn testSimpleIf() {
    // if (a) { b = 1; } else { b = 1; } c = b;
    let a = Variable::new("a");
    let b = Variable::new("b");
    let c = Variable::new("c");
    let n1 = Instruction::BranchInstruction(BranchInstruction::new(Value::Variable(a)));
    let n2 = Instruction::new_assign_number_to_variable_instruction(b, 1);
    let n3 = Instruction::new_assign_number_to_variable_instruction(b, 1);
    let n4 = Instruction::new_assign_variable_to_variable_instruction(c, b);
    let mut cfg = ControlFlowGraph::new(n1);

    cfg.create_edge(n1, Branch::False, n2);
    cfg.create_edge(n1, Branch::True, n3);
    cfg.create_edge(n2, Branch::Unconditional, n4);
    cfg.create_edge(n3, Branch::Unconditional, n4);

    let cfa = ControlFlowAnalysisResult {
        node_priorities: cfg.graph.node_indices().map(|i| i.index() as u32).collect(),
        cfg,
    };

    let mut const_prop = DummyConstPropagation::new(cfa.cfg, &cfa.node_priorities);
    const_prop.analyze();

    // We cannot conclude anything from if (a).
    const_prop.verify_in_has(n1, a, None);
    const_prop.verify_in_has(n1, b, None);
    const_prop.verify_in_has(n1, c, None);
    const_prop.verify_out_has(n1, a, None);
    const_prop.verify_out_has(n1, b, None);
    const_prop.verify_out_has(n1, c, None);

    // We can conclude b = 1 after the instruction.
    const_prop.verify_in_has(n2, a, None);
    const_prop.verify_in_has(n2, b, None);
    const_prop.verify_in_has(n2, c, None);
    const_prop.verify_out_has(n2, a, None);
    const_prop.verify_out_has(n2, b, Some(1));
    const_prop.verify_out_has(n2, c, None);

    // Same as above.
    const_prop.verify_in_has(n3, a, None);
    const_prop.verify_in_has(n3, b, None);
    const_prop.verify_in_has(n3, c, None);
    const_prop.verify_out_has(n3, a, None);
    const_prop.verify_out_has(n3, b, Some(1));
    const_prop.verify_out_has(n3, c, None);

    // After the merge we should still have b = 1.
    const_prop.verify_in_has(n4, a, None);
    const_prop.verify_in_has(n4, b, Some(1));
    const_prop.verify_in_has(n4, c, None);
    const_prop.verify_out_has(n4, a, None);
    // After the instruction both b and c are 1.
    const_prop.verify_out_has(n4, b, Some(1));
    const_prop.verify_out_has(n4, c, Some(1));
}

#[test]
fn testSimpleLoop() {
    // a = 0; do { a = a + 1 } while (b); c = a;
    let a = Variable::new("a");
    let b = Variable::new("b");
    let c = Variable::new("c");
    let n1 = Instruction::new_assign_number_to_variable_instruction(a, 0);
    let n2 = Instruction::ArithmeticInstruction(ArithmeticInstruction {
        result: a,
        operand1: Value::Variable(a),
        operation: Operation::ADD,
        operand2: Value::NumberValue(NumberValue::new(1)),
    });
    let n3 = Instruction::BranchInstruction(BranchInstruction::new(Value::Variable(b)));
    let n4 = Instruction::new_assign_variable_to_variable_instruction(c, a);
    let mut cfg = ControlFlowGraph::new(n1);
    cfg.create_edge(n1, Branch::Unconditional, n2);
    cfg.create_edge(n2, Branch::Unconditional, n3);
    cfg.create_edge(n3, Branch::True, n2);
    cfg.create_edge(n3, Branch::False, n4);

    let cfa = ControlFlowAnalysisResult {
        node_priorities: cfg.graph.node_indices().map(|i| i.index() as u32).collect(),
        cfg,
    };

    let mut const_prop = DummyConstPropagation::new(cfa.cfg, &cfa.node_priorities);
    // This will also show that the framework terminates properly.
    const_prop.data_flow_analysis.analyze();

    // a = 0 is the only thing we know.
    const_prop.verify_in_has(n1, a, None);
    const_prop.verify_in_has(n1, b, None);
    const_prop.verify_in_has(n1, c, None);
    const_prop.verify_out_has(n1, a, Some(0));
    const_prop.verify_out_has(n1, b, None);
    const_prop.verify_out_has(n1, c, None);

    // Nothing is provable in this program, so confirm that we haven't
    // erroneously "proven" something.
    const_prop.verify_in_has(n2, a, None);
    const_prop.verify_in_has(n2, b, None);
    const_prop.verify_in_has(n2, c, None);
    const_prop.verify_out_has(n2, a, None);
    const_prop.verify_out_has(n2, b, None);
    const_prop.verify_out_has(n2, c, None);

    const_prop.verify_in_has(n3, a, None);
    const_prop.verify_in_has(n3, b, None);
    const_prop.verify_in_has(n3, c, None);
    const_prop.verify_out_has(n3, a, None);
    const_prop.verify_out_has(n3, b, None);
    const_prop.verify_out_has(n3, c, None);

    const_prop.verify_in_has(n4, a, None);
    const_prop.verify_in_has(n4, b, None);
    const_prop.verify_in_has(n4, c, None);
    const_prop.verify_out_has(n4, a, None);
    const_prop.verify_out_has(n4, b, None);
    const_prop.verify_out_has(n4, c, None);
}

// tests for computeEscaped method

#[test]
fn testEscaped() {
    assert!(
        computeEscapedLocals(
            "
function f() {
    var x = 0;
    setTimeout(function() { x++; });
    alert(x);
}",
        )
        .len()
            == 1,
    );
    //   assert(computeEscapedLocals("function f() {var _x}").len() == 1);
    assert!(computeEscapedLocals("function f() {try{} catch(e){}}").len() == 1);
}

#[test]
fn testEscapedFunctionLayered() {
    assert!(computeEscapedLocals(
        "
function f() {
    function ff() {
        var x = 0; 
        setTimeout(function() { x++; });
        alert(x);
    }
}"
    )
    .is_empty());
}

#[test]
fn testEscapedLetConstSimple() {
    assert!(computeEscapedLocals("function f() { let x = 0; x ++; x; }").is_empty());
}

#[test]
fn testEscapedFunctionAssignment() {
    assert!(computeEscapedLocals("function f() {var x = function () { return 1; }; }").is_empty());
    assert!(computeEscapedLocals("function f() {var x = function (y) { return y; }; }").is_empty());
    assert!(computeEscapedLocals("function f() {let x = function () { return 1; }; }").is_empty());
}

#[test]
fn testEscapedArrowFunction() {
    // When the body of the arrow fn is analyzed, x is considered an escaped var. When the outer
    // block containing "const value ..." is analyzed, 'x' is not considered an escaped var
    assert!(computeEscapedLocals(
        "
function f() {
    const value = () => {
        var x = 0; 
        setTimeout(function() { x++; }); 
        alert(x);
    };
}",
    )
    .is_empty(),);
}

// test computeEscaped helper method that returns the liveness analysis performed by the
// LiveVariablesAnalysis class
fn computeEscapedLocals(src: &str) -> FxHashSet<Id> {
    GLOBALS.set(&Globals::new(), || {
        let mut program = Program::Script(parse_script(src));

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

        let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

        // Compute liveness of variables
        let liveness = LiveVariablesAnalysis::new(
            cfa.cfg,
            &cfa.node_priorities,
            function,
            all_vars_declared_in_function,
            unresolved_ctxt,
        );
        liveness.analyze().0.escaped_locals
    })
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

#[derive(Debug)]
struct DivergentAnalysisInner {
    lattice_elements: IndexVec<LatticeElementId, Step>,
    counts: FxHashMap<Counter, usize>,
    cfg: ControlFlowGraph<Counter, LinearFlowState>,
}

impl DataFlowAnalysisInner<Counter, Step, DivergentFlowJoiner> for DivergentAnalysisInner {
    fn add_lattice_element(&mut self, element: Step) -> LatticeElementId {
        self.lattice_elements.push(element)
    }

    fn is_forward(&self) -> bool {
        true
    }

    fn create_entry_lattice(&mut self) -> LatticeElementId {
        self.add_lattice_element(Step::new())
    }

    fn create_initial_estimate_lattice(&mut self) -> LatticeElementId {
        self.add_lattice_element(Step::new())
    }

    fn create_flow_joiner(&self) -> DivergentFlowJoiner {
        DivergentFlowJoiner(Step::new())
    }

    fn flow_through(&mut self, node: Counter, input: LatticeElementId) -> LatticeElementId {
        *self.counts.entry(node).or_default() += 1;
        input
    }

    fn cfg(&self) -> &ControlFlowGraph<Counter, LinearFlowState> {
        &self.cfg
    }
    fn cfg_mut(&mut self) -> &mut ControlFlowGraph<Counter, LinearFlowState> {
        &mut self.cfg
    }
}

impl Index<LatticeElementId> for DivergentAnalysisInner {
    type Output = Step;

    fn index(&self, index: LatticeElementId) -> &Self::Output {
        &self.lattice_elements[index]
    }
}

struct DivergentFlowJoiner(Step);

impl FlowJoiner<Step, DivergentAnalysisInner> for DivergentFlowJoiner {
    fn join_flow(&mut self, _: &mut DivergentAnalysisInner, _: LatticeElementId) {}

    fn finish(self) -> Step {
        self.0
    }
}

/// Field is meaningless, but is necessary (and should be unique) for equality.
#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
struct Counter(u32);

impl CfgNode for Counter {
    fn implicit_return() -> Self {
        Counter(u32::MAX)
    }
}

static STEP_ID: AtomicU32 = AtomicU32::new(0);

/// Field is meaningless, but is necessary (and should be unique) for equality.
#[derive(Debug, PartialEq)]
struct Step(u32);

impl Step {
    fn new() -> Self {
        Self(STEP_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

impl LatticeElement for Step {}

impl Annotation for Step {}

struct DivergentAnalysis<'p> {
    data_flow_analysis:
        DataFlowAnalysis<'p, Counter, DivergentAnalysisInner, Step, DivergentFlowJoiner>,
}

impl<'p> DivergentAnalysis<'p> {
    fn new(
        cfg: ControlFlowGraph<Counter, LinearFlowState>,
        node_priorities: &'p [NodePriority],
    ) -> Self {
        Self {
            data_flow_analysis: DataFlowAnalysis::new(
                DivergentAnalysisInner {
                    lattice_elements: IndexVec::default(),
                    counts: FxHashMap::default(),
                    cfg,
                },
                node_priorities,
            ),
        }
    }
}

#[test]
fn testMaxIterationsExceededException() {
    let entrypoint = Counter(0);
    let mut cfg = ControlFlowGraph::new(entrypoint);
    cfg.create_edge(entrypoint, Branch::Unconditional, entrypoint);

    let cfa = ControlFlowAnalysisResult {
        node_priorities: vec![0, 1],
        cfg,
    };

    let mut const_prop = DivergentAnalysis::new(cfa.cfg, &cfa.node_priorities);

    assert!(const_prop.data_flow_analysis.analyze_inner().is_err());

    assert_eq!(
        const_prop.data_flow_analysis.inner.counts[&entrypoint],
        MAX_STEPS_PER_NODE + 1
    );
}
