use ast::*;
use common::DUMMY_SP;
use index::bit_set::{BitMatrix, BitSet};
use petgraph::matrix_graph::NodeIndex;
use petgraph::matrix_graph::UnMatrix;
use rustc_hash::FxHashMap;
use visit::{Visit, VisitMut, VisitMutWith, VisitWith};

use crate::DataFlowAnalysis::LinearFlowState;
use crate::LiveVariablesAnalysis::{
    LiveVariablesAnalysis, LiveVariablesAnalysisResult, MAX_VARIABLES_TO_ANALYZE,
};
use crate::control_flow::{
    ControlFlowAnalysis::{ControlFlowAnalysis, ControlFlowRoot},
    ControlFlowGraph::ControlFlowGraph,
    node::Node,
};
use crate::find_vars::{
    DeclFinder, FunctionLike, VarId, find_first_lhs_ident, find_pat_ids, find_vars_declared_in_fn,
};
use crate::graph::GraphColoring::GraphColouring;
use crate::utils::unwrap_as;

#[cfg(test)]
mod tests;

/// Reuse variable names if possible.
///
/// For example, from `var x = 1; print(x); var y = 2; print(y);`
/// to `var x = 1; print(x); x = 2; print(x)`. The benefits are
/// slightly shorter code because of the removed `var` declaration,
/// less unique variables in hope for better renaming, and finally better gzip
/// compression.
///
/// The pass operates similar to a typical register allocator found in an
/// optimizing compiler by first computing live ranges with
/// [LiveVariablesAnalysis] and a variable interference graph. Then it uses
/// [graph colouring][GraphColouring] to determine which two variables can be
/// merge together safely.
pub struct CoalesceVariableNames<'a> {
    // Maps a colour (represented by an integer) to a variable. If, for example,
    // the colour 5 is mapped to "foo". Then any other variables coloured with the
    // colour 5 will now use the name "foo".
    coloring: GraphColouring<NameId>,
    map: FxHashMap<NameId, NodeIndex>,

    program_data: &'a mut ast::ProgramData,
    in_loop_body: bool,
}

pub fn coalesce_variable_names(ast: &mut Program, program_data: &mut ast::ProgramData) {
    let mut v = GlobalVisitor { program_data };
    ast.visit_mut_with(&mut v);
}

trait ParentVisitor {
    fn program_data(&mut self) -> &mut ProgramData;
}

impl ParentVisitor for CoalesceVariableNames<'_> {
    fn program_data(&mut self) -> &mut ProgramData {
        self.program_data
    }
}

impl ParentVisitor for GlobalVisitor<'_> {
    fn program_data(&mut self) -> &mut ProgramData {
        self.program_data
    }
}

fn handle_fn<'c, T, V>(function: &'c mut T, parent_visitor: &'c mut V)
where
    T: FunctionLike
        + VisitMutWith<'c, V>
        + for<'ast> VisitWith<'ast, DeclFinder>
        + VisitMutWith<'c, CoalesceVariableNames<'c>>,
    for<'x> ControlFlowRoot<'x>: From<&'x T>,
    V: VisitMut<'c> + ParentVisitor,
{
    // Skip lets and consts that have multiple variables declared in them, otherwise this produces
    // incorrect semantics. See test case "testCapture".
    // Skipping vars technically isn't needed for correct semantics, but works around a Safari
    // bug for var redeclarations (https://github.com/google/closure-compiler/issues/3164)
    let all_vars_declared_in_func = find_vars_declared_in_fn(function, true);

    if MAX_VARIABLES_TO_ANALYZE > all_vars_declared_in_func.ordered_vars.len() {
        // This fn is analysable, create new visitor to do so.

        let cfa = ControlFlowAnalysis::analyze(ControlFlowRoot::from(&*function), false);
        let (liveness, cfg) = LiveVariablesAnalysis::new(
            cfa.cfg,
            &cfa.node_priorities,
            function,
            all_vars_declared_in_func,
        )
        .analyze();

        // The interference graph has the function's variables as its nodes and any interference
        // between the variables as the edges. Interference between two variables means that they are
        // alive at overlapping times, which means that their variable names cannot be coalesced.
        let (interference_graph, map) = compute_variable_names_interference_graph(&cfg, &liveness);

        // Colour any interfering variables with different colours and any variables that can be safely
        // coalesced wih the same color.
        let coloring = GraphColouring::new(interference_graph, |a, b| {
            liveness.scope_variables[a].cmp(&liveness.scope_variables[b])
        });

        let mut v = CoalesceVariableNames {
            program_data: parent_visitor.program_data(),
            coloring,
            map,
            in_loop_body: false,
        };
        function.visit_mut_children_with(&mut v);
    } else {
        // This fn is not analysable, continue traversal with parent visitor.
        function.visit_mut_children_with(parent_visitor);
    }
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
        let id = name.name;
        if self.map.contains_key(&id) {
            let coalesced_var = *self.coloring.get_partition_super_node(&id);

            if id != coalesced_var {
                // Rename.
                name.name = coalesced_var;

                // The name is replaced with another - it's a target.
                CoalesceResult::NameIsCoalesceTarget
            } else if self.coloring.colour_count(&id) > 1 {
                // The coalesced name is itself and will be propagated to other nodes - it's a source.
                CoalesceResult::NameIsCoalesceSource
            } else {
                // The coalesced name is itself, and it will not be propagated to other nodes. Nothing to do.
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
                        let var_decl = unwrap_as!(left, VarDeclOrPat::VarDecl(d), d);
                        let decl = var_decl.decls.pop().unwrap();
                        *left = VarDeclOrPat::Pat(decl.name);
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

                let lhs = find_first_lhs_ident(&mut decl.name).unwrap();

                match self.maybe_coalesce_name(lhs) {
                    CoalesceResult::NameIsCoalesceTarget => {
                        // convert `for (let [x] of ...` to `for ([x] of ...`
                        let var_decl = unwrap_as!(left, VarDeclOrPat::VarDecl(d), d);
                        let decl = var_decl.decls.pop().unwrap();
                        *left = VarDeclOrPat::Pat(decl.name);
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
                // TODO: THIS IS NOT TRUE ATM - we don't yet normalize like closure does.
                assert!(var_decl.decls.len() == 1);
                let decl = var_decl.decls.first_mut().unwrap();

                if let Pat::Ident(lhs) = &mut decl.name {
                    match self.maybe_coalesce_name(&mut lhs.id) {
                        CoalesceResult::NameIsCoalesceTarget => {
                            if decl.init.is_some() {
                                // Replace decl with assignment e.g. `let x = 0;` to `x = 0;`.
                                let var_decl = unwrap_as!(stmt, Stmt::Decl(Decl::Var(d)), d);
                                let decl = var_decl.decls.pop().unwrap();
                                let name = unwrap_as!(decl.name, Pat::Ident(n), n);
                                *stmt = Stmt::Expr(ExprStmt {
                                    node_id: self.program_data.new_id_from(decl.node_id),
                                    expr: Box::new(Expr::Assign(AssignExpr {
                                        node_id: self.program_data.new_id_from(decl.node_id),
                                        op: AssignOp::Assign,
                                        left: PatOrExpr::Expr(Box::new(Expr::Ident(name.id))),
                                        right: decl.init.unwrap(),
                                    })),
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
                                    node_id: self.program_data.new_id(DUMMY_SP),
                                    op: UnaryOp::Void,
                                    arg: Box::new(Expr::Lit(Lit::Num(Number {
                                        node_id: self.program_data.new_id(DUMMY_SP),
                                        value: 0.0,
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

                    let lhs = find_first_lhs_ident(&mut decl.name).unwrap();

                    match self.maybe_coalesce_name(lhs) {
                        CoalesceResult::NameIsCoalesceTarget => {
                            // convert `const [x] = arr` to `([x] = arr)`
                            let var_decl = unwrap_as!(stmt, Stmt::Decl(Decl::Var(d)), d);
                            let decl = var_decl.decls.pop().unwrap();

                            *stmt = Stmt::Expr(ExprStmt {
                                node_id: self.program_data.new_id_from(decl.node_id),
                                expr: Box::new(Expr::Assign(AssignExpr {
                                    node_id: self.program_data.new_id_from(decl.node_id),
                                    op: AssignOp::Assign,
                                    left: PatOrExpr::Pat(Box::new(decl.name)),
                                    right: decl.init.unwrap(),
                                })),
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

impl VisitMut<'_> for CoalesceVariableNames<'_> {
    fn visit_mut_function(&mut self, node: &mut Function) {
        handle_fn(node, self);
    }
    fn visit_mut_constructor(&mut self, node: &mut Constructor) {
        handle_fn(node, self);
    }
    fn visit_mut_arrow_expr(&mut self, node: &mut ArrowExpr) {
        handle_fn(node, self);
    }
    fn visit_mut_getter_prop(&mut self, node: &mut GetterProp) {
        handle_fn(node, self);
    }
    fn visit_mut_setter_prop(&mut self, node: &mut SetterProp) {
        handle_fn(node, self);
    }

    // TODO: visit_mut_module_items if we change CoalesceVariableNames to operate
    // on non-function scopes (i.e. global).

    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        self.handle_stmt_list(stmts);
    }

    fn visit_mut_for_stmt(&mut self, node: &mut ForStmt) {
        if let Some(VarDeclOrExpr::VarDecl(var_decl)) = node.init.as_deref_mut() {
            // TODO: this can contain multiple decls!
            if var_decl.decls.len() == 1 {
                let decl = var_decl.decls.first_mut().unwrap();

                if let Pat::Ident(lhs) = &mut decl.name {
                    match self.maybe_coalesce_name(&mut lhs.id) {
                        CoalesceResult::NameIsCoalesceTarget => {
                            let var_decl = unwrap_as!(
                                node.init.as_deref_mut(),
                                Some(VarDeclOrExpr::VarDecl(d)),
                                d
                            );
                            let decl = var_decl.decls.pop().unwrap();

                            if let Some(init) = decl.init {
                                // Replace decl with assignment e.g. `let x = 0;` to `x = 0;`.
                                let name = unwrap_as!(decl.name, Pat::Ident(n), n);
                                node.init = Some(Box::new(VarDeclOrExpr::Expr(Box::new(
                                    Expr::Assign(AssignExpr {
                                        node_id: self.program_data.new_id_from(decl.node_id),
                                        op: AssignOp::Assign,
                                        left: PatOrExpr::Expr(Box::new(Expr::Ident(name.id))),
                                        right: init,
                                    }),
                                ))));
                            } else {
                                // Remove empty decl e.g. `var x;`.
                                node.init = None;
                            }
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

                    let lhs = find_first_lhs_ident(&mut decl.name).unwrap();

                    match self.maybe_coalesce_name(lhs) {
                        CoalesceResult::NameIsCoalesceTarget => {
                            // convert `const [x] = arr` to `[x] = arr`
                            let var_decl = unwrap_as!(
                                node.init.as_deref_mut(),
                                Some(VarDeclOrExpr::VarDecl(d)),
                                d
                            );
                            let decl = var_decl.decls.pop().unwrap();

                            node.init = Some(Box::new(VarDeclOrExpr::Expr(Box::new(
                                Expr::Assign(AssignExpr {
                                    node_id: self.program_data.new_id_from(decl.node_id),
                                    op: AssignOp::Assign,
                                    left: PatOrExpr::Pat(Box::new(decl.name)),
                                    right: decl.init.unwrap(),
                                }),
                            ))));
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
    fn visit_mut_for_in_stmt(&mut self, node: &mut ForInStmt) {
        self.handle_enhanced_for(&mut node.left, &mut node.right, node.body.as_mut());
    }
    fn visit_mut_for_of_stmt(&mut self, node: &mut ForOfStmt) {
        self.handle_enhanced_for(&mut node.left, &mut node.right, node.body.as_mut());
    }
    fn visit_mut_while_stmt(&mut self, node: &mut WhileStmt) {
        self.handle_simple_loop(node.test.as_mut(), node.body.as_mut());
    }
    fn visit_mut_do_while_stmt(&mut self, node: &mut DoWhileStmt) {
        self.handle_simple_loop(node.test.as_mut(), node.body.as_mut());
    }

    fn visit_mut_ident(&mut self, node: &mut Ident) {
        self.maybe_coalesce_name(node);
    }
}

/// In order to determine when it is appropriate to coalesce two variables, we use a live variables
/// analysis to make sure they are not alive at the same time. We take every CFG node and determine
/// which pairs of variables are alive at the same time. These pairs are set to true in a bit map.
/// We take every pairing of variables and use the bit map to check if the two variables are alive
/// at the same time. If two variables are alive at the same time, we create an edge between them
/// in the interference graph. The interference graph is the input to a graph coloring algorithm
/// that ensures any interfering variables are marked in different color groups, while variables
/// that can safely be coalesced are assigned the same color group.
fn compute_variable_names_interference_graph(
    cfg: &ControlFlowGraph<Node, LinearFlowState>,
    liveness: &LiveVariablesAnalysisResult,
) -> (UnMatrix<NameId, ()>, FxHashMap<NameId, NodeIndex>) {
    let mut map = FxHashMap::default();
    let mut interference_graph = UnMatrix::default();

    // First create a node for each non-escaped variable. We add these nodes in the order in which
    // they appear in the code because we want the names that appear earlier in the code to be used
    // when coalescing to variables that appear later in the code.
    let ordered_variables = &liveness.ordered_vars;

    // The VarIds that have a corresponding node in the interference graph.
    let mut interference_graph_nodes = BitSet::new_empty(ordered_variables.len());

    // The pairs of variables that interfere and should have an edge in the interference graph.
    let mut interfering_vars = BitMatrix::new(ordered_variables.len(), ordered_variables.len());

    for (v_index, v) in ordered_variables.iter_enumerated() {
        if liveness.escaped_locals.contains(v) {
            continue;
        }

        if liveness.fn_and_class_names.contains(&v_index) {
            continue;
        }

        let node_index = interference_graph.add_node(v.clone());
        map.insert(v.clone(), node_index);
        interference_graph_nodes.insert(v_index);
    }

    // Go through every CFG node in the program and look at variables that are live.
    // Set the pair of live variables in interferenceBitSet so we can add an edge between them.
    for cfg_node in cfg.graph.node_weights() {
        if *cfg_node == cfg.implicit_return {
            continue;
        }

        let state = &cfg.node_annotations[cfg_node];

        // Check the live states and add edge when possible. An edge between two variables
        // means that they are alive at overlapping times, which means that their
        // variable names cannot be coalesced.
        let live_in = &liveness.lattice_elements[state.in_];
        let mut in_live_vars = live_in.live_vars();
        while let Some(i) = in_live_vars.next() {
            for j in in_live_vars.clone() {
                interfering_vars.insert(i, j);
            }
        }
        let live_out = &liveness.lattice_elements[state.out];
        let mut out_live_vars = live_out.live_vars();
        while let Some(i) = out_live_vars.next() {
            for j in out_live_vars.clone() {
                interfering_vars.insert(i, j);
            }
        }

        let live_range_checker = LiveRangeChecker::check(*cfg_node, state, liveness);
        live_range_checker.set_crossing_variables(&mut interfering_vars);
    }

    // Go through each variable and try to connect them.
    let mut ordered_variables = ordered_variables.iter_enumerated();
    while let Some((v1_idx, v1)) = ordered_variables.next() {
        for (v2_idx, v2) in ordered_variables.clone() {
            if !interference_graph_nodes.contains(v1_idx)
                || !interference_graph_nodes.contains(v2_idx)
            {
                // Skip nodes that were not added. They are globals and escaped locals.
                continue;
            }

            if interfering_vars.contains(v1_idx, v2_idx)
                || liveness.params.contains(&v1_idx) && liveness.params.contains(&v2_idx)
            {
                // Add an edge between variable pairs that are both parameters
                // because we don't want parameters to share a name.
                let v1 = map[v1];
                let v2 = map[v2];
                interference_graph.add_edge(v1, v2, ());
            }
        }
    }
    (interference_graph, map)
}

/// This separate visitor is necessary because [`CoalesceVariableNames`] can't
/// run on the global scope, so this visitor searches the global scope for
/// functions and tries to run [`CoalesceVariableNames`] on them, without
/// touching any global variables.
struct GlobalVisitor<'a> {
    program_data: &'a mut ast::ProgramData,
}

impl VisitMut<'_> for GlobalVisitor<'_> {
    fn visit_mut_function(&mut self, node: &mut Function) {
        handle_fn(node, self);
    }
    fn visit_mut_constructor(&mut self, node: &mut Constructor) {
        handle_fn(node, self);
    }
    fn visit_mut_arrow_expr(&mut self, node: &mut ArrowExpr) {
        handle_fn(node, self);
    }
    fn visit_mut_getter_prop(&mut self, node: &mut GetterProp) {
        handle_fn(node, self);
    }
    fn visit_mut_setter_prop(&mut self, node: &mut SetterProp) {
        handle_fn(node, self);
    }
}

/// Used to find written and read variables in the same CFG node so that the variable pairs can be
/// marked as interfering in an interference bit map. Indices of written and read variables are put
/// in a list. These two lists are used to mark each written variable as "crossing" all read
/// variables.
struct LiveRangeChecker<'a> {
    state: &'a LinearFlowState,
    /// Indices of written variables.
    is_assign_to_list: Vec<VarId>,
    /// Indices of read variables.
    is_read_from_list: Vec<VarId>,

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
            is_assign_to_list: Vec::default(),
            is_read_from_list: Vec::default(),
            liveness,
        };
        root.visit_with(&mut checker);

        if !checker.is_assign_to_list.is_empty() {
            let out = &checker.liveness.lattice_elements[checker.state.out];
            checker.is_read_from_list.extend(out.live_vars());
        }

        checker
    }

    fn set_crossing_variables(&self, interfering_vars: &mut BitMatrix<VarId, VarId>) {
        for &written_var in &self.is_assign_to_list {
            for &read_var in &self.is_read_from_list {
                interfering_vars.insert(written_var, read_var);
                interfering_vars.insert(read_var, written_var);
            }
        }
    }

    fn visit_ident(&mut self, name: NameId, is_read_from: bool, is_assigned_to: bool) {
        if is_assigned_to {
            if let Some(var_id) = self.liveness.scope_variables.get(&name) {
                self.is_assign_to_list.push(*var_id);
            }
        }
        if !self.is_assign_to_list.is_empty() {
            if is_read_from && let Some(var_id) = self.liveness.scope_variables.get(&name) {
                self.is_read_from_list.push(*var_id);
            }
        }
    }
}

impl Visit<'_> for LiveRangeChecker<'_> {
    fn visit_ident(&mut self, node: &Ident) {
        self.visit_ident(node.name, true, false);
    }

    // TODO: do we need to visit class/fn names, which aren't BindingIdents?
    fn visit_binding_ident(&mut self, node: &BindingIdent) {
        self.visit_ident(node.id.name, false, true);
    }

    fn visit_member_expr(&mut self, node: &MemberExpr) {
        node.obj.visit_with(self);
        if node.computed {
            node.prop.visit_with(self);
        }
    }

    fn visit_assign_expr(&mut self, node: &AssignExpr) {
        let lhs_ident = match &node.left {
            PatOrExpr::Expr(lhs) => match lhs.as_ref() {
                Expr::Ident(lhs) => Some(lhs.name),
                _ => None,
            },
            PatOrExpr::Pat(lhs) => match lhs.as_ref() {
                Pat::Ident(lhs) => Some(lhs.id.name),
                _ => None,
            },
        };
        if let Some(lhs) = lhs_ident {
            node.right.visit_with(self);
            // All assign ops except for plain assigns read from the variable as
            // well as writing.
            let is_read_from = node.op != AssignOp::Assign;
            self.visit_ident(lhs, is_read_from, true);
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
                self.visit_ident(lhs.id.name, false, true);
            } else {
                self.visit_ident(lhs.id.name, true, false);
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

    // TODO: this incorrectly assumes that the bodies of if/for etc are only
    // BlockStmts, but they can be any statement.
    // Also need to double check the handling of switch cases, which create CFG
    // nodes.
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
