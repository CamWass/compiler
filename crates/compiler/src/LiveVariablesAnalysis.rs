use std::ops::Index;

use ast::*;
use atoms::js_word;
use ecma_visit::{Visit, VisitWith};
use global_common::SyntaxContext;
use index::{bit_set::BitSet, vec::IndexVec};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::control_flow::ControlFlowAnalysis::NodePriority;
use crate::control_flow::ControlFlowGraph::{Branch, ControlFlowGraph};
use crate::control_flow::{node::Node, ControlFlowGraph::Annotation};
use crate::find_vars::*;
use crate::{DataFlowAnalysis::*, Id, ToId};

#[cfg(test)]
mod tests;

// TODO: might be able to use this to pre-allocate or as an upper bound to lower
// memory usage of e.g. bitsets.
pub const MAX_VARIABLES_TO_ANALYZE: usize = 100;

pub struct LiveVariablesAnalysisResult {
    pub escaped_locals: FxHashSet<Id>,
    pub scopeVariables: FxHashMap<Id, VarId>,
    pub orderedVars: IndexVec<VarId, Id>,
    pub params: FxHashSet<VarId>,
    pub fn_and_class_names: FxHashSet<VarId>,
    pub lattice_elements: IndexVec<LatticeElementId, LiveVariableLattice>,
}

/**
 * Compute the "liveness" of all local variables. A variable is "live" at a point of a program if
 * the value it is currently holding might be read later. Otherwise, the variable is considered
 * "dead" if we know for sure that it will no longer be read. Dead variables are candidates for dead
 * assignment elimination and variable name sharing. The worst case safe assumption is to assume
 * that all variables are live. In that case, we will have no opportunity for optimizations. This is
 * especially the case within a TRY block when an assignment is not guaranteed to take place. We
 * bail out by assuming that all variables are live.
 *
 * <p>Due to the possibility of inner functions and closures, certain "local" variables can escape
 * the function. These variables will be considered as global and they can be retrieved with {@link
 * #getEscapedLocals()}.
 */
pub struct LiveVariablesAnalysis<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    data_flow_analysis: DataFlowAnalysis<
        'a,
        Node<'ast>,
        Inner<'ast, 'a, T>,
        LiveVariableLattice,
        LiveVariableJoinOp,
    >,
}

impl<'ast, 'a, T> LiveVariablesAnalysis<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    /**
     * Live Variables Analysis using the ES6 scope creator. This analysis should only be done on
     * function where jsScope is the function scope. If we call LiveVariablesAnalysis from the
     * function scope of our pass, we can pass a null value for the JsScopeChild, but if we call it
     * from the function block scope, then JsScopeChild will be the function block scope.
     *
     * <p>We call from the function scope when the pass requires us to traverse nodes beginning at the
     * function parameters, and it from the function block scope when we are ignoring function
     * parameters.
     *
     * @param cfg
     * @param jsScope the function scope
     * @param jsScopeChild null or function block scope
     * @param compiler
     * @param scopeCreator Es6 Scope creator
     * @param allVarsDeclaredInFunction mapping of names to vars of everything reachable in a function
     */
    pub fn new(
        cfg: ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,
        nodePriorities: &'a [NodePriority],
        fn_scope: &'a T,
        allVarsDeclaredInFunction: AllVarsDeclaredInFunction,
        unresolved_ctxt: SyntaxContext,
    ) -> Self {
        let inner = Inner {
            unresolved_ctxt,
            num_vars: allVarsDeclaredInFunction.ordered_vars.len(),
            fn_scope,

            escaped: computeEscaped(
                fn_scope,
                &allVarsDeclaredInFunction.scopeVariables,
                allVarsDeclaredInFunction.catch_vars,
            ),
            scopeVariables: allVarsDeclaredInFunction.scopeVariables,
            orderedVars: allVarsDeclaredInFunction.ordered_vars,
            params: allVarsDeclaredInFunction.params,
            fn_and_class_names: allVarsDeclaredInFunction.fn_and_class_names,
            lattice_elements: IndexVec::default(),
            cfg,
        };
        let data_flow_analysis = DataFlowAnalysis::new(inner, nodePriorities);

        Self { data_flow_analysis }
    }

    pub fn analyze(
        mut self,
    ) -> (
        LiveVariablesAnalysisResult,
        ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,
    ) {
        self.data_flow_analysis.analyze();

        let liveness_result = LiveVariablesAnalysisResult {
            escaped_locals: self.data_flow_analysis.inner.escaped,
            scopeVariables: self.data_flow_analysis.inner.scopeVariables,
            orderedVars: self.data_flow_analysis.inner.orderedVars,
            params: self.data_flow_analysis.inner.params,
            fn_and_class_names: self.data_flow_analysis.inner.fn_and_class_names,
            lattice_elements: self.data_flow_analysis.inner.lattice_elements,
        };
        (liveness_result, self.data_flow_analysis.inner.cfg)
    }

    fn getVarIndex(&self, var: &Id) -> Option<VarId> {
        self.data_flow_analysis.inner.getVarIndex(var)
    }
}

#[derive(Debug)]
struct Inner<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    unresolved_ctxt: SyntaxContext,
    num_vars: usize,
    fn_scope: &'a T,

    escaped: FxHashSet<Id>,
    // Maps the variable name to it's position
    // in this jsScope were we to combine the function and function body scopes. The Integer
    // represents the equivalent of the variable index property within a scope
    scopeVariables: FxHashMap<Id, VarId>,
    // obtain variables in the order in which they appear in the code
    orderedVars: IndexVec<VarId, Id>,
    params: FxHashSet<VarId>,
    fn_and_class_names: FxHashSet<VarId>,

    lattice_elements: IndexVec<LatticeElementId, LiveVariableLattice>,

    cfg: ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,
}

impl<'ast, 'a, T> Inner<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    fn getVarIndex(&self, var: &Id) -> Option<VarId> {
        self.scopeVariables.get(var).copied()
    }

    /**
     * Computes the GEN and KILL set.
     *
     * @param n Root node.
     * @param gen Local variables that are live because of the instruction at {@code n} will be added
     *     to this set.
     * @param kill Local variables that are killed because of the instruction at {@code n} will be
     *     added to this set.
     * @param conditional {@code true} if any assignments encountered are conditionally executed.
     *     These assignments might not kill a variable.
     */
    fn computeGenKill<'b>(
        &mut self,
        n: Node<'ast>,
        gen: &'b mut BitSet<VarId>,
        kill: &'b mut BitSet<VarId>,
        conditional: bool,
    ) {
        let mut v = GenKillComputer {
            unresolved_ctxt: self.unresolved_ctxt,
            gen,
            kill,
            conditional,
            analysis: self,
            in_lhs: false,
            in_destructuring: false,
        };

        n.visit_with(&mut v);
    }

    fn addToSetIfLocal(&mut self, name: &Id, set: &mut BitSet<VarId>) {
        if !self.scopeVariables.contains_key(name) {
            return;
        }

        if !self.escaped.contains(name) {
            set.insert(self.getVarIndex(name).unwrap());
        }
    }

    /**
     * Give up computing liveness of formal parameters by putting all the simple parameters in the
     * escaped set.
     *
     * <p>This only applies to simple parameters, that is NAMEs, because other parameter syntaxes
     * never need to be escaped in this way. The known applications of this method are for uses of
     * `arguments`, and for IE8. In a function with non-simple paremeters, `arguments` is not
     * parameter-mapped, and so referencing it doesn't escape paremeters. IE8 just doess't support
     * non-simple parameters.
     *
     * <p>We could actaully continue tracking simple parameters if any parameter is non-simple, but it
     * wasn't worth the complexity or cost to do so.
     *
     * @see https://tc39.github.io/ecma262/#sec-functiondeclarationinstantiation
     */
    fn markAllParametersEscaped(&mut self) {
        for param in self.fn_scope.params() {
            if let Pat::Ident(name) = param {
                self.escaped.insert(name.to_id());
            }
        }
    }
}

struct GenKillComputer<'ast, 'a, 'b, T>
where
    T: FunctionLike<'a>,
{
    unresolved_ctxt: SyntaxContext,
    gen: &'b mut BitSet<VarId>,
    kill: &'b mut BitSet<VarId>,
    conditional: bool,
    analysis: &'b mut Inner<'ast, 'a, T>,
    in_lhs: bool,
    in_destructuring: bool,
}

impl<'a, 'b, 'ast, T> Visit<'ast> for GenKillComputer<'ast, 'a, 'b, T>
where
    T: FunctionLike<'a>,
{
    // Don't enter any new control nodes. They will be handled by later.
    fn visit_block_stmt(&mut self, _: &'ast BlockStmt) {}
    fn visit_for_stmt(&mut self, node: &'ast ForStmt) {
        node.test.visit_with(self);
    }
    fn visit_switch_case(&mut self, node: &'ast SwitchCase) {
        node.test.visit_with(self);
    }
    fn visit_switch_stmt(&mut self, node: &'ast SwitchStmt) {
        node.discriminant.visit_with(self);
    }

    fn visit_for_in_stmt(&mut self, node: &'ast ForInStmt) {
        debug_assert!(!self.in_lhs);
        let lhs = match &node.left {
            // for (var x in y) {...}
            VarDeclOrPat::VarDecl(v) => {
                assert!(v.decls.len() == 1);
                &v.decls.first().unwrap().name
            }
            // for (x in y) {...}
            VarDeclOrPat::Pat(p) => p,
        };

        // Note that the LHS may never be assigned to or evaluated, like in:
        //   for (x in []) {}
        // so should not be killed.
        self.in_lhs = true;
        lhs.visit_with(self);
        self.in_lhs = false;

        // rhs is executed only once so we don't go into it every loop.
    }
    fn visit_for_of_stmt(&mut self, node: &'ast ForOfStmt) {
        debug_assert!(!self.in_lhs);
        let lhs = match &node.left {
            // for (var x in y) {...}
            VarDeclOrPat::VarDecl(v) => {
                assert!(v.decls.len() == 1);
                &v.decls.first().unwrap().name
            }
            // for (x in y) {...}
            VarDeclOrPat::Pat(p) => p,
        };

        // Note that the LHS may never be assigned to or evaluated, like in:
        //   for (x in []) {}
        // so should not be killed.
        self.in_lhs = true;
        lhs.visit_with(self);
        self.in_lhs = false;

        // rhs is executed only once so we don't go into it every loop.
    }

    fn visit_var_declarator(&mut self, node: &'ast VarDeclarator) {
        debug_assert!(!self.in_lhs);
        if let Pat::Ident(name) = &node.name {
            if let Some(init) = &node.init {
                init.visit_with(self);
                if !self.conditional {
                    self.analysis.addToSetIfLocal(&name.to_id(), self.kill);
                }
            }
            return;
        }
        // Destructuring pattern.

        if !self.conditional {
            for lhsNode in &find_pat_ids(&node.name) {
                self.analysis.addToSetIfLocal(lhsNode, self.kill);
            }
        }
        self.in_lhs = true;
        node.name.visit_with(self);
        self.in_lhs = false;
        node.init.visit_with(self);
    }

    fn visit_bin_expr(&mut self, node: &'ast BinExpr) {
        debug_assert!(!self.in_lhs);
        if matches!(
            node.op,
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr | BinaryOp::NullishCoalescing
        ) {
            node.left.visit_with(self);
            // RHS may short circuit.
            let old_cond = self.conditional;
            self.conditional = true;
            node.right.visit_with(self);
            self.conditional = old_cond;
        } else {
            node.visit_children_with(self);
        }
    }

    fn visit_opt_chain_expr(&mut self, node: &'ast OptChainExpr) {
        debug_assert!(!self.in_lhs);
        match node.expr.as_ref() {
            Expr::Member(e) => {
                e.obj.visit_with(self);
                // RHS may short circuit.
                let old_cond = self.conditional;
                self.conditional = true;
                e.prop.visit_with(self);
                self.conditional = old_cond;
            }
            Expr::Call(e) => {
                e.callee.visit_with(self);
                // Unlike an optionally chained member expr, an optionally chained
                // call expr can have multiple children on rhs (arguments) which
                // get executed conditionally.
                let old_cond = self.conditional;
                self.conditional = true;
                e.args.visit_with(self);
                self.conditional = old_cond;
            }
            _ => unreachable!(),
        }
    }

    fn visit_cond_expr(&mut self, node: &'ast CondExpr) {
        debug_assert!(!self.in_lhs);
        node.test.visit_with(self);
        // Assume both sides are conditional.
        let old_cond = self.conditional;
        self.conditional = true;
        node.cons.visit_with(self);
        node.alt.visit_with(self);
        self.conditional = old_cond;
    }

    // Only add/visit names in destructuring patterns if they're not lvalues.
    // e.g. "x" in "const {foo = x} = obj;"
    // fn visit_pat(&mut self, node: &'ast Pat) {
    //     match node {
    //         Pat::Ident(_) | Pat::Array(_) | Pat::Object(_) | Pat::Expr(_) => {
    //             node.visit_children_with(self)
    //         }
    //         Pat::Rest(_) => {}
    //         Pat::Assign(p) => {
    //             p.right.visit_with(self);
    //         }
    //         Pat::Invalid(_) => unreachable!(),
    //     }
    // }
    fn visit_array_pat(&mut self, node: &'ast ArrayPat) {
        let old = self.in_destructuring;
        self.in_destructuring = true;
        node.visit_children_with(self);
        self.in_destructuring = old;
    }
    fn visit_object_pat(&mut self, node: &'ast ObjectPat) {
        let old = self.in_destructuring;
        self.in_destructuring = true;
        node.visit_children_with(self);
        self.in_destructuring = old;
    }
    fn visit_assign_pat(&mut self, node: &'ast AssignPat) {
        let old = self.in_lhs;
        self.in_lhs = true;

        node.left.visit_with(self);

        self.in_lhs = false;

        node.right.visit_with(self);

        self.in_lhs = old;
    }
    fn visit_key_value_pat_prop(&mut self, node: &'ast KeyValuePatProp) {
        let old = self.in_lhs;
        self.in_lhs = false;
        node.key.visit_with(self);
        self.in_lhs = true;
        node.value.visit_with(self);
        self.in_lhs = old;
    }
    fn visit_assign_pat_prop(&mut self, node: &'ast AssignPatProp) {
        let old = self.in_lhs;
        self.in_lhs = true;
        node.key.visit_with(self);
        self.in_lhs = false;
        node.value.visit_with(self);
        self.in_lhs = old;
    }
    fn visit_rest_pat(&mut self, node: &'ast RestPat) {
        let old = self.in_lhs;
        self.in_lhs = true;
        node.arg.visit_with(self);
        self.in_lhs = old;
    }

    // fn visit_binding_ident(&mut self, node: &'ast BindingIdent) {
    //     // Only add names in destructuring patterns if they're not lvalues.
    //     // e.g. "x" in "const {foo = x} = obj;"
    //     if !self.in_destructuring {
    //         // if !(self.in_lhs && self.in_destructuring) {
    //         self.analysis
    //             .addToSetIfLocal(&node.to_id(), self.gen, "generated", Node::from(node));
    //     }
    // }

    fn visit_ident(&mut self, node: &'ast Ident) {
        if !(self.in_destructuring && self.in_lhs) {
            if node.sym == js_word!("arguments") && node.ctxt == self.unresolved_ctxt {
                self.analysis.markAllParametersEscaped();
            } else {
                self.analysis.addToSetIfLocal(&node.to_id(), self.gen);
            }
        }
    }

    fn visit_assign_expr(&mut self, node: &'ast AssignExpr) {
        debug_assert!(!self.in_lhs);
        let mut handle_ident_lhs = |lhs: &Ident| {
            if !self.conditional {
                self.analysis.addToSetIfLocal(&lhs.to_id(), self.kill);
            }
            if node.op != AssignOp::Assign {
                // assignments such as a += 1 reads a.
                self.analysis.addToSetIfLocal(&lhs.to_id(), self.gen);
            }
            node.right.visit_with(self);
        };

        match &node.left {
            PatOrExpr::Pat(left) => {
                if let Pat::Ident(lhs) = left.as_ref() {
                    handle_ident_lhs(&lhs.id);
                    return;
                }

                if node.op == AssignOp::Assign && matches!(&**left, Pat::Array(_) | Pat::Object(_))
                {
                    if !self.conditional {
                        for lhsNode in &find_pat_ids(&left) {
                            self.analysis.addToSetIfLocal(lhsNode, self.kill);
                        }
                    }
                }
                self.in_lhs = true;
                node.left.visit_with(self);
                self.in_lhs = false;
                node.right.visit_with(self);
            }
            PatOrExpr::Expr(left) => {
                if let Expr::Ident(lhs) = left.as_ref() {
                    handle_ident_lhs(&lhs);
                    return;
                }
                self.in_lhs = true;
                node.left.visit_with(self);
                self.in_lhs = false;
                node.right.visit_with(self);
            }
        }
    }

    fn visit_param(&mut self, node: &'ast Param) {
        debug_assert!(!self.in_lhs);
        self.in_lhs = true;
        node.pat.visit_with(self);
        self.in_lhs = false;
    }
    fn visit_param_without_decorators(&mut self, node: &'ast ParamWithoutDecorators) {
        debug_assert!(!self.in_lhs);
        self.in_lhs = true;
        node.pat.visit_with(self);
        self.in_lhs = false;
    }
    fn visit_catch_clause(&mut self, node: &'ast CatchClause) {
        debug_assert!(!self.in_lhs);
        self.in_lhs = true;
        node.param.visit_with(self);
        self.in_lhs = false;
    }

    fn visit_expr(&mut self, node: &'ast Expr) {
        let old = self.in_lhs;
        self.in_lhs = false;
        node.visit_children_with(self);
        self.in_lhs = old;
    }
}

impl<'ast, 'a, T> DataFlowAnalysisInner<Node<'ast>, LiveVariableLattice, LiveVariableJoinOp>
    for Inner<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    fn add_lattice_element(&mut self, element: LiveVariableLattice) -> LatticeElementId {
        self.lattice_elements.push(element)
    }

    fn isForward(&self) -> bool {
        false
    }

    fn createEntryLattice(&mut self) -> LatticeElementId {
        self.add_lattice_element(LiveVariableLattice::new(self.num_vars))
    }

    fn createInitialEstimateLattice(&mut self) -> LatticeElementId {
        self.createEntryLattice()
    }

    fn createFlowJoiner(&self) -> LiveVariableJoinOp {
        LiveVariableJoinOp::new(self.num_vars)
    }

    fn flowThrough(&mut self, node: Node<'ast>, input: LatticeElementId) -> LatticeElementId {
        let mut gen = BitSet::new_empty(self.num_vars);
        let mut kill = BitSet::new_empty(self.num_vars);

        // Make kills conditional if the node can end abruptly by an exception.
        let conditional = self
            .cfg
            .graph
            .edges(self.cfg.map[&node])
            .any(|e| *e.weight() == Branch::ON_EX);
        self.computeGenKill(node, &mut gen, &mut kill, conditional);

        if gen.is_empty() && kill.is_empty() {
            // No changes compared to input.
            input
        } else {
            let mut new_live_set = self[input].liveSet.clone();
            // L_in = L_out - Kill + Gen
            new_live_set.subtract(&kill);
            new_live_set.union(&gen);
            if new_live_set != self[input].liveSet {
                self.add_lattice_element(LiveVariableLattice {
                    liveSet: new_live_set,
                })
            } else {
                // No changes compared to input.
                input
            }
        }
    }

    fn cfg(&self) -> &ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId> {
        &self.cfg
    }
    fn cfg_mut(&mut self) -> &mut ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId> {
        &mut self.cfg
    }
}

impl<'a, T> Index<LatticeElementId> for Inner<'_, 'a, T>
where
    T: FunctionLike<'a>,
{
    type Output = LiveVariableLattice;

    fn index(&self, index: LatticeElementId) -> &Self::Output {
        &self.lattice_elements[index]
    }
}

struct LiveVariableJoinOp {
    result: LiveVariableLattice,
}

impl LiveVariableJoinOp {
    fn new(num_vars: usize) -> Self {
        Self {
            result: LiveVariableLattice::new(num_vars),
        }
    }
}

impl<'ast, 'a, T> FlowJoiner<LiveVariableLattice, Inner<'ast, 'a, T>> for LiveVariableJoinOp
where
    T: FunctionLike<'a>,
{
    fn joinFlow(&mut self, inner: &mut Inner<'ast, 'a, T>, input: LatticeElementId) {
        self.result
            .liveSet
            .union(&inner.lattice_elements[input].liveSet);
    }

    fn finish(self) -> LiveVariableLattice {
        self.result
    }
}

/**
 * The lattice that stores the liveness of all local variables at a given point in the program.
 * The whole lattice is the power set of all local variables and a variable is live if it is in
 * the set.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct LiveVariableLattice {
    liveSet: BitSet<VarId>,
}

impl LiveVariableLattice {
    fn new(num_vars: usize) -> Self {
        Self {
            liveSet: BitSet::new_empty(num_vars),
        }
    }

    pub fn isLive(&self, index: VarId) -> bool {
        self.liveSet.contains(index)
    }

    /// Returns the index of the first bit that is set to true that occurs
    /// on or after the specified starting index.
    pub fn next_set_bits(&self, fromIndex: VarId) -> impl Iterator<Item = VarId> + '_ {
        // TODO: maybe add a method to BitSet that allows us to get the iter from
        // a specific index without having to skip.
        self.liveSet.iter().skip(fromIndex.as_usize() + 1)
    }

    pub fn set_bits(&self) -> impl Iterator<Item = VarId> + '_ {
        self.liveSet.iter()
    }
}

impl Annotation for LiveVariableLattice {}
impl LatticeElement for LiveVariableLattice {}
