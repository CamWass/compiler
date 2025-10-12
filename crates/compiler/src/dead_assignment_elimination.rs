use ast::*;
use ecma_visit::VisitWith;
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::SyntaxContext;
use rustc_hash::FxHashMap;

use crate::control_flow::ControlFlowAnalysis::ControlFlowAnalysis;
use crate::control_flow::ControlFlowAnalysis::ControlFlowRoot;
use crate::find_vars::find_vars_declared_in_fn;
use crate::find_vars::DeclFinder;
use crate::find_vars::FunctionLike;
use crate::find_vars::VarId;
use crate::utils::unwrap_as;
use crate::DataFlowAnalysis::LinearFlowState;
use crate::LiveVariablesAnalysis::LiveVariablesAnalysis;
use crate::LiveVariablesAnalysis::LiveVariablesAnalysisResult;
use crate::LiveVariablesAnalysis::MAX_VARIABLES_TO_ANALYZE;
use crate::ToId;

// TODO: default parameter values?
// e.g. the default param assignment is dead here:
// function foo(a = 1) {
//   a = 2;
//   return a;
// }
//
//

pub fn process(ast: &mut Program, program_data: &mut ProgramData, unresolved_ctxt: SyntaxContext) {
    // let cfa = ControlFlowAnalysis::<()>::analyze(ControlFlowRoot::from(&*ast), false);
    // cfa.cfg.print_simple_with_annotations(&program_data);

    let mut visitor = Driver {
        program_data,
        unresolved_ctxt,
        function_stack: Vec::new(),
    };
    ast.visit_mut_with(&mut visitor);
}

struct DeadAssignmentElimination<'a> {
    program_data: &'a mut ProgramData,
    live_variable_analysis: LiveVariablesAnalysisResult,
    cfg_node_states: FxHashMap<NodeId, LinearFlowState>,
    current_state: LinearFlowState,
}

impl DeadAssignmentElimination<'_> {
    fn is_var_live_before_current_cfg_node(&self, var: VarId) -> bool {
        let in_state = &self.live_variable_analysis.lattice_elements[self.current_state.in_];

        in_state.is_live(var)
    }

    fn is_var_live_after_current_cfg_node(&self, var: VarId) -> bool {
        let out_state = &self.live_variable_analysis.lattice_elements[self.current_state.out];

        out_state.is_live(var)
    }

    fn with_control_flow_node(
        &mut self,
        node_id: NodeId,
        mut op: impl FnMut(&mut DeadAssignmentElimination),
    ) {
        let new_state = *self
            .cfg_node_states
            .get(&node_id)
            .expect("node should have cfg node");

        let old_state = self.current_state;

        self.current_state = new_state;

        op(self);

        self.current_state = old_state;
    }
}

impl VisitMut<'_> for DeadAssignmentElimination<'_> {
    fn visit_mut_function(&mut self, _: &mut Function) {}
    fn visit_mut_constructor(&mut self, _: &mut Constructor) {}
    fn visit_mut_arrow_expr(&mut self, _: &mut ArrowExpr) {}
    fn visit_mut_getter_prop(&mut self, _: &mut GetterProp) {}
    fn visit_mut_setter_prop(&mut self, _: &mut SetterProp) {}

    fn visit_mut_stmt(&mut self, node: &mut Stmt) {
        self.with_control_flow_node(node.node_id(), |visitor| {
            node.visit_mut_children_with(visitor);
        })
    }

    // TODO: handle entering other control flow nodes

    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        let mut i = 0;
        while i < stmts.len() {
            let stmt = &mut stmts[i];

            let mut can_optimise = false;

            self.with_control_flow_node(stmt.node_id(), |visitor| {
                stmt.visit_mut_children_with(visitor);

                let Stmt::Decl(Decl::Var(var)) = stmt else {
                    return;
                };

                debug_assert_eq!(
                    var.decls.len(),
                    1,
                    "var should have exactly one declarator post-normalisation"
                );

                if var.kind == VarDeclKind::Const {
                    // We can't remove the RHS of a const, since that creates an
                    // invalid AST.
                    return;
                }

                if var.decls[0].init.is_none() {
                    // Can't optimise declarator with no RHS.
                    return;
                }

                let Pat::Ident(lhs) = &var.decls[0].name else {
                    // Can't optimise declarator that declares multiple names.
                    return;
                };

                let name = lhs.to_id();
                let Some(&var) = visitor.live_variable_analysis.scope_variables.get(&name) else {
                    // Not declared in the current function.
                    return;
                };

                if visitor
                    .live_variable_analysis
                    .escaped_locals
                    .contains(&name)
                {
                    // Local variable that might be escaped due to closures.
                    return;
                }

                if visitor.is_var_live_after_current_cfg_node(var) {
                    // Variable is not dead.
                    return;
                }

                can_optimise = true;
            });

            if can_optimise {
                // We can optimise! Remove the RHS from the var decl and replace
                // it after the var decl statement.

                let var = unwrap_as!(stmt, Stmt::Decl(Decl::Var(v)), v);
                let decl = var.decls.first_mut().unwrap();
                let rhs = decl.init.take().unwrap();

                let rhs_stmt = Stmt::Expr(ExprStmt {
                    node_id: self.program_data.new_id_from(rhs.node_id()),
                    expr: rhs,
                });

                if i == stmts.len() - 1 {
                    stmts.push(rhs_stmt);
                } else {
                    stmts.insert(i + 1, rhs_stmt);
                }

                // Skip over the new statement, since we've already visited it
                // (as the RHS).
                i += 1;
            }

            i += 1;
        }
    }
}

struct Driver<'a> {
    unresolved_ctxt: SyntaxContext,
    program_data: &'a mut ast::ProgramData,
    function_stack: Vec<FunctionData>,
}

impl Driver<'_> {
    fn handle_fn<T>(&mut self, node: &mut T)
    where
        T: FunctionLike
            + GetNodeId
            + for<'b> VisitWith<'b, DeclFinder>
            + for<'b> VisitMutWith<'b, DeadAssignmentElimination<'b>>,
        for<'b> ControlFlowRoot<'b>: From<&'b T>,
    {
        if let Some(function_data) = self.function_stack.last_mut() {
            function_data.contains_function = true;
        }

        self.function_stack.push(FunctionData::default());
        node.params_mut().for_each(|p| p.visit_mut_with(self));
        node.body_mut().visit_mut_with(self);
        let function_data = self.function_stack.pop().unwrap();

        // TODO: should we find and skip the specific variables that are
        // escaped, optimising the rest, rather than baling for the whole
        // function?
        // We are not going to do any dead assignment elimination in when there
        // is at least one inner function because in most browsers, when there
        // is a closure, ALL the variables are saved (escaped).
        if function_data.contains_function {
            return;
        }

        // We don't do any dead assignment elimination if there are no assigns
        // to eliminate. :)
        if !function_data.contains_removable_assign {
            return;
        }

        let node_id = node.node_id();

        let all_vars_declared_in_func = find_vars_declared_in_fn(&*node, false);

        if all_vars_declared_in_func.ordered_vars.len() > MAX_VARIABLES_TO_ANALYZE {
            return;
        }

        let cfa = ControlFlowAnalysis::analyze(ControlFlowRoot::from(&*node), false);
        let (liveness, cfg) = LiveVariablesAnalysis::new(
            cfa.cfg,
            &cfa.node_priorities,
            node,
            all_vars_declared_in_func,
            self.unresolved_ctxt,
        )
        .analyze();

        let cfg_node_states = cfg
            .node_annotations
            .into_iter()
            .map(|(k, v)| (k.node_id, v))
            .collect::<FxHashMap<_, _>>();

        let current_state = *cfg_node_states
            .get(&node_id)
            .expect("function should have cfg node");

        let mut visitor = DeadAssignmentElimination {
            program_data: self.program_data,
            live_variable_analysis: liveness,
            cfg_node_states,
            current_state,
        };

        node.visit_mut_with(&mut visitor);
    }
}

impl VisitMut<'_> for Driver<'_> {
    fn visit_mut_function(&mut self, node: &mut Function) {
        self.handle_fn(node);
    }
    fn visit_mut_constructor(&mut self, node: &mut Constructor) {
        self.handle_fn(node);
    }
    fn visit_mut_arrow_expr(&mut self, node: &mut ArrowExpr) {
        self.handle_fn(node);
    }
    fn visit_mut_getter_prop(&mut self, node: &mut GetterProp) {
        self.handle_fn(node);
    }
    fn visit_mut_setter_prop(&mut self, node: &mut SetterProp) {
        self.handle_fn(node);
    }

    fn visit_mut_for_stmt(&mut self, node: &mut ForStmt) {
        node.test.visit_mut_with(self);
        node.update.visit_mut_with(self);
        node.body.visit_mut_with(self);

        if let Some(VarDeclOrExpr::VarDecl(init)) = &mut node.init {
            // We can't optimise assignments in the variable declaration of a
            // for loop, since there's not a spot directly following the
            // variable where we can move the RHS.
            // So, we skip visit_mut_var_decl and visit children directly, to
            // prevent this VarDecl from being marked as optimisable.
            init.visit_mut_children_with(self);
        } else {
            node.init.visit_mut_with(self);
        }
    }

    fn visit_mut_for_in_stmt(&mut self, node: &mut ForInStmt) {
        node.right.visit_mut_children_with(self);
        node.body.visit_mut_children_with(self);

        if let VarDeclOrPat::VarDecl(init) = &mut node.left {
            // We can't optimise assignments in the variable declaration of a
            // for loop, since there's not a spot directly following the
            // variable where we can move the RHS.
            // So, we skip visit_mut_var_decl and visit children directly, to
            // prevent this VarDecl from being marked as optimisable.
            init.visit_mut_children_with(self);
        } else {
            node.left.visit_mut_with(self);
        }
    }

    fn visit_mut_for_of_stmt(&mut self, node: &mut ForOfStmt) {
        node.right.visit_mut_children_with(self);
        node.body.visit_mut_children_with(self);

        if let VarDeclOrPat::VarDecl(init) = &mut node.left {
            // We can't optimise assignments in the variable declaration of a
            // for loop, since there's not a spot directly following the
            // variable where we can move the RHS.
            // So, we skip visit_mut_var_decl and visit children directly, to
            // prevent this VarDecl from being marked as optimisable.
            init.visit_mut_children_with(self);
        } else {
            node.left.visit_mut_with(self);
        }
    }

    fn visit_mut_var_decl(&mut self, node: &mut VarDecl) {
        node.visit_mut_children_with(self);

        if let Some(function_data) = self.function_stack.last_mut() {
            // We can't remove the RHS of a const, since that creates an invalid
            // AST.
            if node.kind != VarDeclKind::Const {
                let has_decl_with_ident_lhs =
                    node.decls.iter().any(|d| matches!(d.name, Pat::Ident(_)));

                if has_decl_with_ident_lhs {
                    function_data.contains_removable_assign = true;
                }
            }
        }
    }

    fn visit_mut_expr(&mut self, node: &mut Expr) {
        node.visit_mut_children_with(self);

        if let Some(function_data) = self.function_stack.last_mut() {
            if let Expr::Assign(assign) = node {
                let lhs_is_ident = match &assign.left {
                    PatOrExpr::Expr(lhs) => matches!(lhs.as_ref(), Expr::Ident(_)),
                    PatOrExpr::Pat(lhs) => matches!(lhs.as_ref(), Pat::Ident(_)),
                };

                if lhs_is_ident {
                    function_data.contains_removable_assign = true;
                }
            }

            if let Expr::Update(expr) = node {
                if matches!(expr.arg.as_ref(), Expr::Ident(_)) {
                    function_data.contains_removable_assign = true;
                }
            }
        }
    }
}

#[derive(Default, Debug)]
struct FunctionData {
    contains_function: bool,
    contains_removable_assign: bool,
}
