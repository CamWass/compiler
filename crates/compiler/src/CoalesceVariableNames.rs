use ast::*;
use ecma_visit::{noop_visit_mut_type, Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::DUMMY_SP;
use global_common::{util::take::Take, SyntaxContext};
use index::bit_set::{BitMatrix, BitSet};
use petgraph::graph::{NodeIndex, UnGraph};
use rustc_hash::FxHashMap;

use crate::control_flow::{
    node::Node,
    ControlFlowAnalysis::{ControlFlowAnalysis, ControlFlowRoot},
    ControlFlowGraph::ControlFlowGraph,
};
use crate::graph::GraphColoring::{GreedyGraphColoring, SubGraph};
use crate::utils::unwrap_as;
use crate::DataFlowAnalysis::{LatticeElementId, LinearFlowState};
use crate::Id;
use crate::LiveVariablesAnalysis::*;
use crate::{find_vars::*, ToId};

#[cfg(test)]
mod tests;

/// For debug purposes, when merging variable `foo` and `bar` to `foo`, rename both variable to `foo_bar`.
const USE_PSEUDO_NAMES: bool = false;

/**
 * Reuse variable names if possible.
 *
 * <p>For example, from <code>var x = 1; print(x); var y = 2; print(y); </code>
 * to <code>var x = 1; print(x); x = 2; print(x)</code>. The benefits are
 * slightly shorter code because of the removed <code>var<code> declaration,
 * less unique variables in hope for better renaming, and finally better gzip
 * compression.
 *
 * <p>The pass operates similar to a typical register allocator found in an
 * optimizing compiler by first computing live ranges with
 * {@link LiveVariablesAnalysis} and a variable interference graph. Then it uses
 * graph coloring in {@link GraphColoring} to determine which two variables can
 * be merge together safely.
 */
pub struct CoalesceVariableNames<'a> {
    // Maps a color (represented by an integer) to a variable. If, for example,
    // the color 5 is mapped to "foo". Then any other variables colored with the
    // color 5 will now use the name "foo".
    coloring: GreedyGraphColoring<Id>,
    map: FxHashMap<Id, NodeIndex>,
    unresolved_ctxt: SyntaxContext,
    node_id_gen: &'a mut ast::NodeIdGen,
    in_loop_body: bool,
}

pub fn coalesce_variable_names<'a>(
    ast: &mut Program,
    unresolved_ctxt: SyntaxContext,
    node_id_gen: &'a mut ast::NodeIdGen,
) {
    let mut v = GlobalVisitor {
        unresolved_ctxt,
        node_id_gen,
    };
    ast.visit_mut_with(&mut v);
}

// TODO: this is only a macro to appease the borrow checker. Ideally it would be a fn.
macro_rules! handle_fn {
    ($parent_visitor:ident, $function:ident) => {
        // Skip lets and consts that have multiple variables declared in them, otherwise this produces
        // incorrect semantics. See test case "testCapture".
        // Skipping vars technically isn't needed for correct semantics, but works around a Safari
        // bug for var redeclarations (https://github.com/google/closure-compiler/issues/3164)
        let allVarsDeclaredInFunction = find_vars_declared_in_fn($function, true);

        if MAX_VARIABLES_TO_ANALYZE > allVarsDeclaredInFunction.ordered_vars.len() {
            // This fn is analyzable, create new visitor to do so.

            let cfa = ControlFlowAnalysis::analyze(ControlFlowRoot::from(&*$function), false);
            let (liveness, cfg) =
                LiveVariablesAnalysis::new(cfa.cfg, &cfa.nodePriorities, $function, allVarsDeclaredInFunction, $parent_visitor.unresolved_ctxt).analyze();

            // TODO:
            // if (FeatureSet.ES3.contains(compiler.getOptions().getOutputFeatureSet())) {
            //     // If the function has exactly 2 params, mark them as escaped. This is a work-around for a
            //     // bug in IE 8 and below, where it throws an exception if you write to the parameters of the
            //     // callback in a sort(). See http://blickly.github.io/closure-compiler-issues/#58 and
            //     // https://www.zachleat.com/web/array-sort/
            //     Node enclosingFunction = scope.getRootNode();
            //     if (NodeUtil.getFunctionParameters(enclosingFunction).hasTwoChildren()) {
            //       liveness.markAllParametersEscaped();
            //     }
            // }

            // The interference graph has the function's variables as its nodes and any interference
            // between the variables as the edges. Interference between two variables means that they are
            // alive at overlapping times, which means that their variable names cannot be coalesced.
            let (interferenceGraph, map) = computeVariableNamesInterferenceGraph(&cfg, &liveness);

            // Color any interfering variables with different colors and any variables that can be safely
            // coalesced wih the same color.
            let mut coloring = GreedyGraphColoring::new();
            coloring.color(
                interferenceGraph.node_weights().cloned().collect(),
                |a, b| {
                    liveness.scopeVariables[a].cmp(&liveness.scopeVariables[b])
                },
                |node| {
                    let node_index = map[node];
                    let degree = interferenceGraph.neighbors(node_index).count();
                    degree
                },
                || SimpleSubGraph {
                    graph: &interferenceGraph,
                    map: &map,
                    nodes: Vec::new(),
                },
            );

            let mut v = CoalesceVariableNames {
                unresolved_ctxt: $parent_visitor.unresolved_ctxt,
                node_id_gen: $parent_visitor.node_id_gen,
                coloring,
                map,
                in_loop_body: false,
            };
            $function.visit_mut_children_with(&mut v);
        } else {
            // This fn is not analyzable, continue traversal with parent visitor.
            $function.visit_mut_children_with($parent_visitor);
        }
    };
}

enum CoalesceResult {
    /// The identifier has been renamed.
    NameIsCoalesceTarget,
    /// The identifier's name will be used to rename other identifiers.
    NameIsCoalesceSource,
    None,
}

// For a var to participate in coalescing, it must be the only lvalue in its
// declaration. For `let` and `const` declarations, this is necessary for
// correctness (see test case "testCapture"). Skipping `var`s technically isn't
// needed for correct semantics, but works around a Safari bug for var redeclarations (https://github.com/google/closure-compiler/issues/3164)
// For `var` declarations, this works around a Safari bug (https://bugs.webkit.org/show_bug.cgi?id=182414).
//
// For each var decl we encounter, we compare its coalesced name with its original one:
//
// If the names are the same, the var is a coalesce source and its name will
// replace other vars, so we:
//  - convert `const` or `let` declarations to `var` declarations to ensure the
//    name is declared before any new and existing uses of the name, even if
//    they are in different block scopes (because `var` decls are hoisted to
//    function scopes).
// If the names differ, the var is a coalesce target, so we:
// - rename all references to it with the new name.
// - convert any decls into assignments, or remove the decl if it has no initializer.

impl CoalesceVariableNames<'_> {
    fn maybe_coalesce_name(&mut self, name: &mut Ident) -> CoalesceResult {
        let id = name.to_id();
        if self.map.contains_key(&id) {
            let coalescedVar = self.coloring.getPartitionSuperNode(&id);

            if &id != coalescedVar {
                // Rename.
                name.sym = coalescedVar.0.clone();
                name.span.ctxt = coalescedVar.1;

                // The name is replaced with another - it's a target.
                CoalesceResult::NameIsCoalesceTarget
            } else if self.coloring.color_count(&id) > 1 {
                // The coalesced name is itself and will be propogated to other nodes - it's a source.
                CoalesceResult::NameIsCoalesceSource
            } else {
                // The coalesced name is itself, and it will not be propogated to other nodes. Nothing to do.
                CoalesceResult::None
            }
        } else {
            // This is not a local.
            CoalesceResult::None
        }
    }

    fn visit_loop_body(&mut self, body: &mut Stmt) {
        match body {
            Stmt::Block(b) => {
                let old = self.in_loop_body;
                self.in_loop_body = true;
                self.handle_stmt_list(&mut b.stmts);
                self.in_loop_body = old;
            }
            _ => unreachable!(),
        }
    }

    fn handle_enhanced_for(&mut self, left: &mut VarDeclOrPat, right: &mut Expr, body: &mut Stmt) {
        if let VarDeclOrPat::VarDecl(var_decl) = left {
            assert!(var_decl.decls.len() == 1);
            let decl = var_decl.decls.first_mut().unwrap();
            debug_assert!(decl.init.is_none());

            if let Pat::Ident(lhs) = &mut decl.name {
                match self.maybe_coalesce_name(&mut lhs.id) {
                    CoalesceResult::NameIsCoalesceTarget => {
                        // convert `for (let x of ...` to `for (x of ...`
                        left.map_with_mut(|left| {
                            let mut var_decl = unwrap_as!(left, VarDeclOrPat::VarDecl(d), d);
                            let decl = var_decl.decls.pop().unwrap();
                            VarDeclOrPat::Pat(decl.name)
                        });
                    }
                    CoalesceResult::NameIsCoalesceSource => {
                        // Convert `const` or `let` declarations to `var` declarations.
                        var_decl.kind = VarDeclKind::Var;
                    }
                    CoalesceResult::None => {}
                }
                left.visit_mut_with(self);
            } else if find_pat_ids(&decl.name).len() == 1 {
                // Destructuring with one LHS.

                let lhs = find_first_LHS_ident(&mut decl.name).unwrap();

                match self.maybe_coalesce_name(lhs) {
                    CoalesceResult::NameIsCoalesceTarget => {
                        // convert `for (let [x] of ...` to `for ([x] of ...`
                        left.map_with_mut(|left| {
                            let mut var_decl = unwrap_as!(left, VarDeclOrPat::VarDecl(d), d);
                            let decl = var_decl.decls.pop().unwrap();
                            VarDeclOrPat::Pat(decl.name)
                        });
                    }
                    CoalesceResult::NameIsCoalesceSource => {
                        // Convert `const` or `let` declarations to `var` declarations.
                        var_decl.kind = VarDeclKind::Var;
                    }
                    CoalesceResult::None => {}
                }
                left.visit_mut_with(self);
            } else {
                // Destructuring with multiple LHS.
                left.visit_mut_with(self);
            }
        } else {
            left.visit_mut_with(self);
        }

        right.visit_mut_with(self);
        self.visit_loop_body(body);
    }

    /// For while and do-while loops.
    fn handle_simple_loop(&mut self, test: &mut Expr, body: &mut Stmt) {
        test.visit_mut_with(self);
        self.visit_loop_body(body);
    }

    fn handle_stmt_list(&mut self, stmts: &mut Vec<Stmt>) {
        stmts.retain_mut(|stmt| match stmt {
            Stmt::Decl(Decl::Var(var_decl)) => {
                assert!(var_decl.decls.len() == 1);
                let decl = var_decl.decls.first_mut().unwrap();

                if let Pat::Ident(lhs) = &mut decl.name {
                    match self.maybe_coalesce_name(&mut lhs.id) {
                        CoalesceResult::NameIsCoalesceTarget => {
                            if decl.init.is_some() {
                                // Replace decl with assignment e.g. `let x = 0;` to `x = 0;`.
                                stmt.map_with_mut(|stmt| {
                                    let mut var_decl =
                                        unwrap_as!(stmt, Stmt::Decl(Decl::Var(d)), d);
                                    let decl = var_decl.decls.pop().unwrap();
                                    let name = unwrap_as!(decl.name, Pat::Ident(n), n);
                                    Stmt::Expr(ExprStmt {
                                        node_id: self.node_id_gen.next(),
                                        span: DUMMY_SP,
                                        expr: Box::new(Expr::Assign(AssignExpr {
                                            node_id: self.node_id_gen.next(),
                                            span: DUMMY_SP,
                                            op: AssignOp::Assign,
                                            left: PatOrExpr::Expr(Box::new(Expr::Ident(name.id))),
                                            right: decl.init.unwrap(),
                                        })),
                                    })
                                });
                            } else {
                                // Remove empty decl e.g. `var x;`.
                                return false;
                            }
                        }
                        CoalesceResult::NameIsCoalesceSource => {
                            // Convert `const` or `let` declarations to `var` declarations.

                            if var_decl.kind == VarDeclKind::Let
                                && decl.init.is_none()
                                && self.in_loop_body
                            {
                                // We need to make sure that within a loop:
                                //
                                // `let x;`
                                // becomes
                                // `var x = void 0;`
                                //
                                // If we don't we won't be correctly resetting the variable to undefined on each loop
                                // iteration once we turn it into a var declaration.
                                //
                                // Note that all other cases will already have an initializer.
                                // const x = 1; // constant requires an initializer
                                // let {x, y} = obj; // destructuring requires an initializer
                                // let [x, y] = iterable; // destructuring requires an initializer
                                decl.init = Some(Box::new(Expr::Unary(UnaryExpr {
                                    node_id: self.node_id_gen.next(),
                                    span: DUMMY_SP,
                                    op: UnaryOp::Void,
                                    arg: Box::new(Expr::Lit(Lit::Num(Number {
                                        node_id: self.node_id_gen.next(),
                                        span: DUMMY_SP,
                                        value: 0.0,
                                        raw: None,
                                    }))),
                                })));
                            }

                            var_decl.kind = VarDeclKind::Var;
                        }
                        CoalesceResult::None => {}
                    }
                    stmt.visit_mut_children_with(self);
                } else if find_pat_ids(&decl.name).len() == 1 {
                    // Destructuring with one LHS.

                    let lhs = find_first_LHS_ident(&mut decl.name).unwrap();

                    match self.maybe_coalesce_name(lhs) {
                        CoalesceResult::NameIsCoalesceTarget => {
                            // convert `const [x] = arr` to `([x] = arr)`
                            stmt.map_with_mut(|stmt| {
                                let mut var_decl = unwrap_as!(stmt, Stmt::Decl(Decl::Var(d)), d);
                                let decl = var_decl.decls.pop().unwrap();

                                Stmt::Expr(ExprStmt {
                                    node_id: self.node_id_gen.next(),
                                    span: DUMMY_SP,
                                    expr: Box::new(Expr::Paren(ParenExpr {
                                        node_id: self.node_id_gen.next(),
                                        span: DUMMY_SP,
                                        expr: Box::new(Expr::Assign(AssignExpr {
                                            node_id: self.node_id_gen.next(),
                                            span: DUMMY_SP,
                                            op: AssignOp::Assign,
                                            left: PatOrExpr::Pat(Box::new(decl.name)),
                                            right: decl.init.unwrap(),
                                        })),
                                    })),
                                })
                            });
                        }
                        CoalesceResult::NameIsCoalesceSource => {
                            // Convert `const` or `let` declarations to `var` declarations.
                            var_decl.kind = VarDeclKind::Var;
                        }
                        CoalesceResult::None => {}
                    }
                    stmt.visit_mut_children_with(self);
                } else {
                    // Destructuring with multiple LHS.
                    stmt.visit_mut_children_with(self);
                }
                true
            }
            _ => {
                stmt.visit_mut_with(self);
                true
            }
        });
    }
}

impl<'ast> VisitMut<'ast> for CoalesceVariableNames<'_> {
    noop_visit_mut_type!();

    fn visit_mut_function(&mut self, node: &'ast mut Function) {
        handle_fn!(self, node);
    }
    fn visit_mut_constructor(&mut self, node: &'ast mut Constructor) {
        handle_fn!(self, node);
    }
    fn visit_mut_arrow_expr(&mut self, node: &'ast mut ArrowExpr) {
        handle_fn!(self, node);
    }
    fn visit_mut_getter_prop(&mut self, node: &'ast mut GetterProp) {
        handle_fn!(self, node);
    }
    fn visit_mut_setter_prop(&mut self, node: &'ast mut SetterProp) {
        handle_fn!(self, node);
    }

    // TODO: visit_mut_module_items if we change CoalesceVariableNames to operate
    // on non-function scopes (i.e. global).

    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        self.handle_stmt_list(stmts);
    }

    fn visit_mut_for_stmt(&mut self, node: &'ast mut ForStmt) {
        if let Some(VarDeclOrExpr::VarDecl(var_decl)) = &mut node.init {
            if var_decl.decls.len() == 1 {
                let decl = var_decl.decls.first_mut().unwrap();

                if let Pat::Ident(lhs) = &mut decl.name {
                    match self.maybe_coalesce_name(&mut lhs.id) {
                        CoalesceResult::NameIsCoalesceTarget => {
                            node.init.map_with_mut(|init| {
                                let mut var_decl =
                                    unwrap_as!(init, Some(VarDeclOrExpr::VarDecl(d)), d);
                                let decl = var_decl.decls.pop().unwrap();

                                if decl.init.is_some() {
                                    // Replace decl with assignment e.g. `let x = 0;` to `x = 0;`.
                                    let name = unwrap_as!(decl.name, Pat::Ident(n), n);
                                    Some(VarDeclOrExpr::Expr(Box::new(Expr::Assign(AssignExpr {
                                        node_id: self.node_id_gen.next(),
                                        span: DUMMY_SP,
                                        op: AssignOp::Assign,
                                        left: PatOrExpr::Expr(Box::new(Expr::Ident(name.id))),
                                        right: decl.init.unwrap(),
                                    }))))
                                } else {
                                    // Remove empty decl e.g. `var x;`.
                                    None
                                }
                            });
                        }
                        CoalesceResult::NameIsCoalesceSource => {
                            // Convert `const` or `let` declarations to `var` declarations.
                            var_decl.kind = VarDeclKind::Var;
                        }
                        CoalesceResult::None => {}
                    }
                    node.init.visit_mut_with(self);
                } else if find_pat_ids(&decl.name).len() == 1 {
                    // Destructuring with one LHS.

                    let lhs = find_first_LHS_ident(&mut decl.name).unwrap();

                    match self.maybe_coalesce_name(lhs) {
                        CoalesceResult::NameIsCoalesceTarget => {
                            // convert `const [x] = arr` to `[x] = arr`
                            node.init.map_with_mut(|init| {
                                let mut var_decl =
                                    unwrap_as!(init, Some(VarDeclOrExpr::VarDecl(d)), d);
                                let decl = var_decl.decls.pop().unwrap();

                                Some(VarDeclOrExpr::Expr(Box::new(Expr::Assign(AssignExpr {
                                    node_id: self.node_id_gen.next(),
                                    span: DUMMY_SP,
                                    op: AssignOp::Assign,
                                    left: PatOrExpr::Pat(Box::new(decl.name)),
                                    right: decl.init.unwrap(),
                                }))))
                            });
                        }
                        CoalesceResult::NameIsCoalesceSource => {
                            // Convert `const` or `let` declarations to `var` declarations.
                            var_decl.kind = VarDeclKind::Var;
                        }
                        CoalesceResult::None => {}
                    }
                    node.init.visit_mut_with(self);
                } else {
                    // Destructuring with multiple LHS.
                    node.init.visit_mut_with(self);
                }
            } else {
                // Var decls with multiple names do not participate in name coalescing.
                node.init.visit_mut_with(self);
            }
        } else {
            node.init.visit_mut_with(self);
        }

        node.test.visit_mut_with(self);
        node.update.visit_mut_with(self);
        self.visit_loop_body(node.body.as_mut());
    }
    fn visit_mut_for_in_stmt(&mut self, node: &'ast mut ForInStmt) {
        self.handle_enhanced_for(&mut node.left, &mut node.right, node.body.as_mut());
    }
    fn visit_mut_for_of_stmt(&mut self, node: &'ast mut ForOfStmt) {
        self.handle_enhanced_for(&mut node.left, &mut node.right, node.body.as_mut());
    }
    fn visit_mut_while_stmt(&mut self, node: &'ast mut WhileStmt) {
        self.handle_simple_loop(node.test.as_mut(), node.body.as_mut());
    }
    fn visit_mut_do_while_stmt(&mut self, node: &'ast mut DoWhileStmt) {
        self.handle_simple_loop(node.test.as_mut(), node.body.as_mut());
    }

    fn visit_mut_ident(&mut self, node: &'ast mut Ident) {
        self.maybe_coalesce_name(node);
    }
}

/// A simple implementation of [`SubGraph`] that calculates adjacency by iterating
/// over a node's neighbors.
struct SimpleSubGraph<'a> {
    graph: &'a UnGraph<Id, ()>,
    map: &'a FxHashMap<Id, NodeIndex>,
    nodes: Vec<NodeIndex>,
}

impl SubGraph<Id> for SimpleSubGraph<'_> {
    fn isIndependentOf(&self, value: &Id) -> bool {
        for &n in &self.nodes {
            if self.graph.neighbors(n).any(|n| &self.graph[n] == value) {
                return false;
            }
        }
        true
    }

    fn addNode(&mut self, value: Id) {
        self.nodes.push(self.map[&value]);
    }
}

/**
 * In order to determine when it is appropriate to coalesce two variables, we use a live variables
 * analysis to make sure they are not alive at the same time. We take every CFG node and determine
 * which pairs of variables are alive at the same time. These pairs are set to true in a bit map.
 * We take every pairing of variables and use the bit map to check if the two variables are alive
 * at the same time. If two variables are alive at the same time, we create an edge between them
 * in the interference graph. The interference graph is the input to a graph coloring algorithm
 * that ensures any interfering variables are marked in different color groups, while variables
 * that can safely be coalesced are assigned the same color group.
 *
 * @param cfg
 * @param escaped we don't want to coalesce any escaped variables
 * @return graph with variable nodes and edges representing variable interference
 */
fn computeVariableNamesInterferenceGraph<'ast>(
    cfg: &ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,
    liveness: &LiveVariablesAnalysisResult,
) -> (UnGraph<Id, ()>, FxHashMap<Id, NodeIndex>) {
    let mut map = FxHashMap::default();
    let mut interferenceGraph = UnGraph::default();

    // First create a node for each non-escaped variable. We add these nodes in the order in which
    // they appear in the code because we want the names that appear earlier in the code to be used
    // when coalescing to variables that appear later in the code.
    let orderedVariables = &liveness.orderedVars;

    // The VarIds that have a corresponding node in the interference graph.
    let mut interferenceGraphNodes = BitSet::new_empty(orderedVariables.len());

    // The paris of variables that interfere and should have an edge in the interference graph.
    let mut interfering_vars = BitMatrix::new(orderedVariables.len(), orderedVariables.len());

    for (vIndex, v) in orderedVariables.iter_enumerated() {
        if liveness.escaped_locals.contains(v) {
            continue;
        }

        if liveness.fn_and_class_names.contains(&vIndex) {
            continue;
        }

        let node_index = interferenceGraph.add_node(v.clone());
        map.insert(v.clone(), node_index);
        interferenceGraphNodes.insert(vIndex);
    }

    // Go through every CFG node in the program and look at variables that are live.
    // Set the pair of live variables in interferenceBitSet so we can add an edge between them.
    for cfgNode in cfg.graph.node_weights() {
        if *cfgNode == cfg.implicit_return {
            continue;
        }

        let state = &cfg.node_annotations[cfgNode];

        // Check the live states and add edge when possible. An edge between two variables
        // means that they are alive at overlapping times, which means that their
        // variable names cannot be coalesced.
        let livein = &liveness.lattice_elements[state.in_];
        for i in livein.set_bits() {
            for j in livein.next_set_bits(i) {
                interfering_vars.insert(i, j);
            }
        }
        let liveout = &liveness.lattice_elements[state.out];
        for i in liveout.set_bits() {
            for j in liveout.next_set_bits(i) {
                interfering_vars.insert(i, j);
            }
        }

        let liveRangeChecker = LiveRangeChecker::check(*cfgNode, &state, &liveness);
        liveRangeChecker.setCrossingVariables(&mut interfering_vars);
    }

    // Go through each variable and try to connect them.
    for (v1Index, v1) in orderedVariables.iter_enumerated() {
        for (v2Index, v2) in orderedVariables.iter_enumerated() {
            // Skip duplicate pairs. Also avoid merging a variable with itself.
            if v1Index >= v2Index {
                continue;
            }

            if !interferenceGraphNodes.contains(v1Index)
                || !interferenceGraphNodes.contains(v2Index)
            {
                // Skip nodes that were not added. They are globals and escaped locals.
                continue;
            }

            if interfering_vars.contains(v1Index, v2Index)
                || liveness.params.contains(&v1Index) && liveness.params.contains(&v2Index)
            {
                // Add an edge between variable pairs that are both parameters
                // because we don't want parameters to share a name.
                let v1 = map[v1];
                let v2 = map[v2];
                if !interferenceGraph.contains_edge(v1, v2) {
                    interferenceGraph.add_edge(v1, v2, ());
                }
            }
        }
    }
    (interferenceGraph, map)
}

/// This separate visitor is necessary because [`CoalesceVariableNames`] can't
/// run on the global scope, so this visitor searches the global scope for
/// functions and tries to run [`CoalesceVariableNames`] on them, without
/// touching any global variables.
struct GlobalVisitor<'a> {
    unresolved_ctxt: SyntaxContext,
    node_id_gen: &'a mut ast::NodeIdGen,
}

impl<'ast> VisitMut<'ast> for GlobalVisitor<'_> {
    noop_visit_mut_type!();

    fn visit_mut_function(&mut self, node: &'ast mut Function) {
        handle_fn!(self, node);
    }
    fn visit_mut_constructor(&mut self, node: &'ast mut Constructor) {
        handle_fn!(self, node);
    }
    fn visit_mut_arrow_expr(&mut self, node: &'ast mut ArrowExpr) {
        handle_fn!(self, node);
    }
    fn visit_mut_getter_prop(&mut self, node: &'ast mut GetterProp) {
        handle_fn!(self, node);
    }
    fn visit_mut_setter_prop(&mut self, node: &'ast mut SetterProp) {
        handle_fn!(self, node);
    }
}

/// Used to find written and read variables in the same CFG node so that the variable pairs can be
/// marked as interfering in an interference bit map. Indices of written and read variables are put
/// in a list. These two lists are used to mark each written variable as "crossing" all read
/// variables.
struct LiveRangeChecker<'a> {
    state: &'a LinearFlowState,
    /// Indices of written variables.
    isAssignToList: Vec<VarId>,
    /// Indices of read variables.
    isReadFromList: Vec<VarId>,

    liveness: &'a LiveVariablesAnalysisResult,
}

impl<'a> LiveRangeChecker<'a> {
    fn check(
        root: Node,
        state: &'a LinearFlowState,
        liveness: &'a LiveVariablesAnalysisResult,
    ) -> Self {
        let mut checker = Self {
            state,
            isAssignToList: Vec::default(),
            isReadFromList: Vec::default(),
            liveness,
        };
        root.visit_with(&mut checker);
        checker
    }

    fn setCrossingVariables(&self, interfering_vars: &mut BitMatrix<VarId, VarId>) {
        for &iWrittenVar in &self.isAssignToList {
            for &iReadVar in &self.isReadFromList {
                interfering_vars.insert(iWrittenVar, iReadVar);
                interfering_vars.insert(iReadVar, iWrittenVar);
            }
        }
    }

    fn visit(&mut self, name: Id, is_read_from: bool, is_assigned_to: bool) {
        if is_assigned_to {
            if let Some(var_id) = self.liveness.scopeVariables.get(&name) {
                self.isAssignToList.push(*var_id);
            }
        }
        if !self.isAssignToList.is_empty() {
            for (var_id, var) in self.liveness.orderedVars.iter_enumerated() {
                let out = &self.liveness.lattice_elements[self.state.out];
                let varOutLive = out.isLive(var_id);
                if varOutLive || is_read_from && var == &name {
                    self.isReadFromList.push(var_id);
                }
            }
        }
    }
}

impl Visit<'_> for LiveRangeChecker<'_> {
    fn visit_ident(&mut self, node: &Ident) {
        self.visit(node.to_id(), true, false);
    }

    fn visit_binding_ident(&mut self, node: &BindingIdent) {
        self.visit(node.to_id(), false, true);
    }

    fn visit_assign_expr(&mut self, node: &AssignExpr) {
        let lhs_ident = match &node.left {
            PatOrExpr::Expr(lhs) => match lhs.as_ref() {
                Expr::Ident(lhs) => Some(lhs.to_id()),
                _ => None,
            },
            PatOrExpr::Pat(lhs) => match lhs.as_ref() {
                Pat::Ident(lhs) => Some(lhs.to_id()),
                _ => None,
            },
        };
        if let Some(lhs) = lhs_ident {
            node.right.visit_with(self);
            // All assign ops except for plain assigns read from the variable as
            // well as writing.
            let is_read_from = node.op != AssignOp::Assign;
            self.visit(lhs, is_read_from, true);
        } else {
            // Evaluate the rhs of a destructuring assignment before the lhs.
            node.right.visit_with(self);
            node.left.visit_with(self);
        }
    }

    fn visit_var_declarator(&mut self, node: &VarDeclarator) {
        if let Pat::Ident(lhs) = &node.name {
            if node.init.is_some() {
                node.init.visit_with(self);
                // A var decl with an initializer assign a value to the name.\
                self.visit(lhs.to_id(), false, true);
            } else {
                self.visit(lhs.to_id(), true, false);
            }
        } else {
            // Evaluate the rhs of a destructuring declaration before the lhs.
            node.init.visit_with(self);
            node.name.visit_with(self);
        }
    }

    fn visit_assign_pat(&mut self, node: &AssignPat) {
        // Visit RHS before LHS to match evaluation order.
        node.right.visit_with(self);
        node.left.visit_with(self);
    }

    // Don't enter any new control nodes. They will be handled by later
    // LiveRangeCheckers.
    fn visit_block_stmt(&mut self, _node: &BlockStmt) {}
    fn visit_for_stmt(&mut self, node: &ForStmt) {
        node.test.visit_with(self);
    }
    fn visit_for_in_stmt(&mut self, node: &ForInStmt) {
        node.left.visit_with(self);
    }
    fn visit_for_of_stmt(&mut self, node: &ForOfStmt) {
        node.left.visit_with(self);
    }
    fn visit_switch_case(&mut self, node: &SwitchCase) {
        node.test.visit_with(self);
    }
    fn visit_switch_stmt(&mut self, node: &SwitchStmt) {
        node.discriminant.visit_with(self);
    }
}
