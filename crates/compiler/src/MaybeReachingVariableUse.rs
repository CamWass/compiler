#![allow(dead_code)]
#![allow(unused_variables)]

use std::ops::Index;

use ast::*;
use ecma_visit::{Visit, VisitWith};
use index::vec::IndexVec;
use petgraph::Direction::Outgoing;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::control_flow::ControlFlowAnalysis::NodePriority;
use crate::control_flow::ControlFlowGraph::{Branch, ControlFlowGraph};
use crate::control_flow::{node::Node, util::MultiMap, ControlFlowGraph::Annotation};
use crate::find_vars::{AllVarsDeclaredInFunction, FunctionLike, VarId};
use crate::DataFlowAnalysis::*;
use crate::{Id, ToId};

#[cfg(test)]
mod tests;

// TODO: temp pub fields:
#[derive(Debug)]
pub struct MaybeReachingResult<'ast> {
    pub scope_variables: FxHashMap<Id, VarId>,
    pub ordered_vars: IndexVec<VarId, Id>,
    pub lattice_elements: IndexVec<LatticeElementId, ReachingUses>,
    pub cfg: ControlFlowGraph<Node<'ast>, LinearFlowState>,
}

impl MaybeReachingResult<'_> {
    /**
     * Gets a list of nodes that may be using the value assigned to {@code name} in {@code defNode}.
     * {@code defNode} must be one of the control flow graph nodes.
     *
     * @param name name of the variable. It can only be names of local variable that are not function
     *     parameters, escaped variables or variables declared in catch.
     * @param defNode the control flow graph node that may assign a value to {@code name}
     * @return the list of upward exposed uses of the variable {@code name} at defNode.
     */
    pub fn get_uses(&self, name: &Id, def_node: Node) -> Option<&Vec<NodeId>> {
        if let Some(var) = self.scope_variables.get(name) {
            let ann = self.cfg.node_annotations.get(&def_node).unwrap();
            self.lattice_elements[ann.out].may_use_map.get(*var)
        } else {
            None
        }
    }
}

pub struct MaybeReachingVariableUse<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    data_flow_analysis:
        DataFlowAnalysis<'a, Node<'ast>, Inner<'ast, 'a, T>, ReachingUses, ReachingUsesJoinOp>,
}

impl<'ast, 'a, T> MaybeReachingVariableUse<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    pub fn new(
        cfg: ControlFlowGraph<Node<'ast>, LinearFlowState>,
        node_priorities: &'a [NodePriority],
        fn_scope: &'a T,
        all_vars_declared_in_function: AllVarsDeclaredInFunction,
    ) -> Self {
        let inner = Inner {
            num_vars: all_vars_declared_in_function.ordered_vars.len(),
            fn_scope,

            escaped: compute_escaped(
                fn_scope,
                &all_vars_declared_in_function.scope_variables,
                all_vars_declared_in_function.catch_vars,
            ),
            scope_variables: all_vars_declared_in_function.scope_variables,
            ordered_vars: all_vars_declared_in_function.ordered_vars,
            params: all_vars_declared_in_function.params,
            fn_and_class_names: all_vars_declared_in_function.fn_and_class_names,
            lattice_elements: IndexVec::default(),
            cfg,
        };
        let data_flow_analysis = DataFlowAnalysis::new(inner, node_priorities);

        Self { data_flow_analysis }
    }

    pub fn analyze(mut self) -> MaybeReachingResult<'ast> {
        self.data_flow_analysis.analyze();

        MaybeReachingResult {
            scope_variables: self.data_flow_analysis.inner.scope_variables,
            ordered_vars: self.data_flow_analysis.inner.ordered_vars,
            lattice_elements: self.data_flow_analysis.inner.lattice_elements,
            cfg: self.data_flow_analysis.inner.cfg,
        }
    }
}

struct Inner<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    num_vars: usize,
    fn_scope: &'a T,

    escaped: FxHashSet<Id>,
    // Maps the variable name to it's position
    // in this jsScope were we to combine the function and function body scopes. The Integer
    // represents the equivalent of the variable index property within a scope
    scope_variables: FxHashMap<Id, VarId>,
    // obtain variables in the order in which they appear in the code
    ordered_vars: IndexVec<VarId, Id>,
    params: FxHashSet<VarId>,
    fn_and_class_names: FxHashSet<VarId>,

    lattice_elements: IndexVec<LatticeElementId, ReachingUses>,
    cfg: ControlFlowGraph<Node<'ast>, LinearFlowState>,
}

impl<'ast, 'a, T> Inner<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    fn has_exception_handler(&self, cfg_node: Node<'ast>) -> bool {
        self.cfg
            .graph
            .edges_directed(self.cfg.map[&cfg_node], Outgoing)
            .any(|e| *e.weight() == Branch::Exception)
    }

    /**
     * Given a cfgNode, updates the output LatticeElement at that node by finding and storing all
     * variables and their uses that are upward exposed at the cfgNode.
     *
     * @param n The explorer node which searches for variables
     * @param cfgNode The CFG node for which the upward exposed variables are being searched.
     * @param conditional Whether {@code n} is only conditionally evaluated given that {@code cfgNode}
     *     is evaluated. Do not remove conditionally redefined variables from the reaching uses set.
     */
    fn compute_may_use(
        &mut self,
        n: Node<'ast>,
        cfg_node: Node<'ast>,
        output: &mut ReachingUses,
        conditional: bool,
    ) {
        let mut v = ReachingUseFinder {
            output,
            conditional,
            analysis: self,
            cfg_node,
            in_lhs: false,
            in_destructuring: false,
        };
        n.visit_with(&mut v);
    }

    /**
     * Sets the variable for the given name to the node value in the upward exposed lattice. Do
     * nothing if the variable name is one of the escaped variable.
     */
    fn add_to_use_if_local(&mut self, name: &Ident, node: Node<'ast>, usage: &mut ReachingUses) {
        let id = name.to_id();
        if let Some(var) = self.scope_variables.get(&id) {
            if !self.escaped.contains(&id) {
                usage.may_use_map.put(*var, node.node_id);
            }
        }
    }

    /**
     * Removes the variable for the given name from the node value in the upward exposed lattice. Do
     * nothing if the variable name is one of the escaped variable.
     */
    fn remove_from_use_if_local(&mut self, name: &Ident, usage: &mut ReachingUses) {
        let id = name.to_id();
        if let Some(var) = self.scope_variables.get(&id) {
            if !self.escaped.contains(&id) {
                usage.may_use_map.remove(var);
            }
        }
    }
}

struct ReachingUseFinder<'ast, 'a, 'b, T>
where
    T: FunctionLike<'a>,
{
    output: &'b mut ReachingUses,
    conditional: bool,
    analysis: &'b mut Inner<'ast, 'a, T>,
    cfg_node: Node<'ast>,
    in_lhs: bool,
    in_destructuring: bool,
}

impl<'a, T> Visit<'_> for ReachingUseFinder<'_, 'a, '_, T>
where
    T: FunctionLike<'a>,
{
    // Don't enter any new control nodes. They will be handled by later.
    fn visit_block_stmt(&mut self, _: &BlockStmt) {}
    fn visit_switch_case(&mut self, node: &SwitchCase) {
        node.test.visit_with(self);
    }
    fn visit_switch_stmt(&mut self, node: &SwitchStmt) {
        node.discriminant.visit_with(self);
    }

    // TODO: may be able to revert in_lhs and in_destructuring tracking in favour of
    // the original strategy of treating all binding identifiers found by visit_binding_ident
    // as "self.in_lhs && self.in_destructuring" (since other cases are handled by parent visitors).
    // More tests are needed to confirm.

    fn visit_ident(&mut self, node: &Ident) {
        if self.in_lhs && self.in_destructuring {
            if !self.conditional {
                self.analysis.remove_from_use_if_local(node, self.output);
            }
        } else {
            self.analysis
                .add_to_use_if_local(node, self.cfg_node, self.output);
        }
    }

    // TODO: these can be removed
    // fn visit_while_stmt(&mut self, node: &WhileStmt) {
    //     node.test.visit_with(self);
    // }
    // fn visit_do_while_stmt(&mut self, node: &DoWhileStmt) {
    //     node.test.visit_with(self);
    // }
    // fn visit_if_stmt(&mut self, node: &IfStmt) {
    //     node.test.visit_with(self);
    // }
    // fn visit_for_stmt(&mut self, node: &ForStmt) {
    //     node.test.visit_with(self);
    // }

    fn visit_for_in_stmt(&mut self, node: &ForInStmt) {
        let lhs = match &node.left {
            VarDeclOrPat::VarDecl(n) => &n.decls.first().unwrap().name,
            VarDeclOrPat::Pat(n) => n,
        };
        if let Pat::Ident(lhs) = lhs {
            if !self.conditional {
                self.analysis.remove_from_use_if_local(&lhs.id, self.output);
            }
        } else {
            debug_assert!(matches!(lhs, Pat::Object(_) | Pat::Array(_)));
            let old = self.conditional;
            self.conditional = true;
            self.in_lhs = true;
            lhs.visit_with(self);
            self.in_lhs = false;
            self.conditional = old;
        }
        node.right.visit_with(self);
    }
    fn visit_for_of_stmt(&mut self, node: &ForOfStmt) {
        let lhs = match &node.left {
            VarDeclOrPat::VarDecl(n) => &n.decls.first().unwrap().name,
            VarDeclOrPat::Pat(n) => n,
        };
        if let Pat::Ident(lhs) = lhs {
            if !self.conditional {
                self.analysis.remove_from_use_if_local(&lhs.id, self.output);
            }
        } else {
            debug_assert!(matches!(lhs, Pat::Object(_) | Pat::Array(_)));
            let old = self.conditional;
            self.conditional = true;
            self.in_lhs = true;
            lhs.visit_with(self);
            self.in_lhs = false;
            self.conditional = old;
        }
        node.right.visit_with(self);
    }

    fn visit_bin_expr(&mut self, node: &BinExpr) {
        if matches!(
            node.op,
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr | BinaryOp::NullishCoalescing
        ) {
            let old = self.conditional;
            self.conditional = true;
            node.right.visit_with(self);
            self.conditional = old;
        } else {
            node.right.visit_with(self);
        }
        node.left.visit_with(self);
    }

    fn visit_opt_chain_expr(&mut self, node: &OptChainExpr) {
        match node.expr.as_ref() {
            Expr::Member(n) => {
                let old = self.conditional;
                self.conditional = true;
                n.prop.visit_with(self);
                self.conditional = old;
                n.obj.visit_with(self);
            }
            Expr::Call(n) => {
                // As args are evaluated in AST order, we traverse in reverse AST order for backward
                // dataflow analysis.
                let old = self.conditional;
                self.conditional = true;
                for arg in n.args.iter().rev() {
                    arg.visit_with(self);
                }
                self.conditional = old;
                n.callee.visit_with(self);
            }
            _ => unreachable!(),
        }
    }

    fn visit_cond_expr(&mut self, node: &CondExpr) {
        let old = self.conditional;
        self.conditional = true;
        node.alt.visit_with(self);
        node.cons.visit_with(self);
        self.conditional = old;
        node.test.visit_with(self);
    }

    fn visit_var_declarator(&mut self, node: &VarDeclarator) {
        match &node.name {
            Pat::Ident(name) => {
                node.init.visit_with(self);
                if !self.conditional {
                    self.analysis
                        .remove_from_use_if_local(&name.id, self.output);
                }
            }
            _ => {
                // Note: since destructuring is evaluated in reverse AST order, we traverse the first
                // child before the second in order to do our backwards data flow analysis.
                self.in_lhs = true;
                node.name.visit_with(self);
                self.in_lhs = false;
                node.init.visit_with(self);
            }
        }
    }

    fn visit_assign_pat(&mut self, node: &AssignPat) {
        match node.left.as_ref() {
            Pat::Ident(left) => {
                // assigning to the name occurs after evaluating the default value
                if !self.conditional {
                    self.analysis
                        .remove_from_use_if_local(&left.id, self.output);
                }
                let old = self.conditional;
                self.conditional = true;
                let old_lhs = self.in_lhs;
                self.in_lhs = false;
                node.right.visit_with(self);
                self.conditional = old;
                self.in_lhs = old_lhs;
            }
            Pat::Array(_) | Pat::Object(_) => {
                let old_lhs = self.in_lhs;
                self.in_lhs = true;
                node.left.visit_with(self);
                self.in_lhs = false;
                let old = self.conditional;
                self.conditional = true;
                node.right.visit_with(self);
                self.conditional = old;
                self.in_lhs = old_lhs;
            }
            _ => {
                let old = self.conditional;
                self.conditional = true;
                let old_lhs = self.in_lhs;
                self.in_lhs = false;
                node.right.visit_with(self);
                self.conditional = old;
                self.in_lhs = true;
                node.left.visit_with(self);
                self.in_lhs = old_lhs;
            }
        }
    }

    fn visit_assign_expr(&mut self, node: &AssignExpr) {
        let lhs_ident = match &node.left {
            PatOrExpr::Expr(n) => match n.as_ref() {
                Expr::Ident(n) => Some(n),
                _ => None,
            },
            PatOrExpr::Pat(n) => match n.as_ref() {
                Pat::Ident(n) => Some(&n.id),
                Pat::Expr(_) => todo!(),
                _ => None,
            },
        };

        let is_assign = node.op == AssignOp::Assign;
        let is_logical_assign = matches!(
            node.op,
            AssignOp::AndAssign | AssignOp::OrAssign | AssignOp::NullishAssign
        );

        let left_is_destructuring_pat = match &node.left {
            PatOrExpr::Expr(_) => false,
            PatOrExpr::Pat(n) => matches!(n.as_ref(), Pat::Array(_) | Pat::Object(_)),
        };

        if let Some(lhs_ident) = lhs_ident {
            if !is_logical_assign && !self.conditional {
                self.analysis
                    .remove_from_use_if_local(lhs_ident, self.output);
            }

            // In case of a += "Hello". There is a read of a.
            if !is_assign {
                self.analysis
                    .add_to_use_if_local(lhs_ident, self.cfg_node, self.output);
            }

            self.in_lhs = false;
            if is_logical_assign {
                let old = self.conditional;
                self.conditional = true;
                node.right.visit_with(self);
                self.conditional = old;
            } else {
                node.right.visit_with(self);
            }
        } else if is_logical_assign && left_is_destructuring_pat {
            // Note: the rhs of destructuring is evaluated before the lhs
            self.in_lhs = true;
            node.left.visit_with(self);
            self.in_lhs = false;
            node.right.visit_with(self);
        } else {
            self.in_lhs = false;
            node.right.visit_with(self);
            self.in_lhs = true;
            node.left.visit_with(self);
        }
    }

    //   default:
    //       /*
    //        * We want to traverse in reverse order because we want the LAST
    //        * definition in the sub-tree.
    //        */
    //       for (Node c = n.getLastChild(); c != null; c = c.getPrevious()) {
    //         computeMayUse(c, cfgNode, output, conditional);
    //       }

    fn visit_class(&mut self, node: &Class) {
        todo!(stringify!(Class));
    }

    fn visit_extends_clause(&mut self, node: &ExtendsClause) {
        todo!(stringify!(ExtendsClause));
    }

    fn visit_class_prop(&mut self, node: &ClassProp) {
        todo!(stringify!(ClassProp));
    }

    fn visit_private_prop(&mut self, node: &PrivateProp) {
        todo!(stringify!(PrivateProp));
    }

    fn visit_class_method(&mut self, node: &ClassMethod) {
        todo!(stringify!(ClassMethod));
    }

    fn visit_private_method(&mut self, node: &PrivateMethod) {
        todo!(stringify!(PrivateMethod));
    }

    fn visit_constructor(&mut self, node: &Constructor) {
        todo!(stringify!(Constructor));
    }

    fn visit_decorator(&mut self, node: &Decorator) {
        unimplemented!("Decorators not supported");
    }

    fn visit_fn_decl(&mut self, node: &FnDecl) {
        node.function.visit_with(self);
        node.ident.visit_with(self);
    }

    fn visit_class_decl(&mut self, node: &ClassDecl) {
        todo!(stringify!(ClassDecl));
    }

    fn visit_var_decl(&mut self, node: &VarDecl) {
        for decl in node.decls.iter().rev() {
            decl.visit_with(self);
        }
    }

    // fn visit_this_expr(&mut self, node: &ThisExpr) {
    //     todo!(stringify!(ThisExpr));
    // }

    fn visit_array_lit(&mut self, node: &ArrayLit) {
        for element in node.elems.iter().rev() {
            element.visit_with(self);
        }
    }

    fn visit_object_lit(&mut self, node: &ObjectLit) {
        for prop in node.props.iter().rev() {
            prop.visit_with(self);
        }
    }

    // fn visit_spread_element(&mut self, node: &SpreadElement) {
    //     todo!(stringify!(SpreadElement));
    // }

    // fn visit_unary_expr(&mut self, node: &UnaryExpr) {
    //     todo!(stringify!(UnaryExpr));
    // }

    // fn visit_update_expr(&mut self, node: &UpdateExpr) {
    //     todo!(stringify!(UpdateExpr));
    // }

    fn visit_fn_expr(&mut self, node: &FnExpr) {
        todo!(stringify!(FnExpr));
    }

    fn visit_class_expr(&mut self, node: &ClassExpr) {
        todo!(stringify!(ClassExpr));
    }

    fn visit_member_expr(&mut self, node: &MemberExpr) {
        node.prop.visit_with(self);
        node.obj.visit_with(self);
    }

    fn visit_call_expr(&mut self, node: &CallExpr) {
        for arg in node.args.iter().rev() {
            arg.visit_with(self);
        }
        node.callee.visit_with(self);
    }

    fn visit_new_expr(&mut self, node: &NewExpr) {
        for arg in node.args.iter().rev() {
            arg.visit_with(self);
        }
        node.callee.visit_with(self);
    }

    fn visit_seq_expr(&mut self, node: &SeqExpr) {
        for expr in node.exprs.iter().rev() {
            expr.visit_with(self);
        }
    }

    fn visit_arrow_expr(&mut self, node: &ArrowExpr) {
        todo!(stringify!(ArrowExpr));
    }

    // fn visit_yield_expr(&mut self, node: &YieldExpr) {
    //     todo!(stringify!(YieldExpr));
    // }

    fn visit_meta_prop_expr(&mut self, node: &MetaPropExpr) {
        todo!(stringify!(MetaPropExpr));
    }

    // fn visit_await_expr(&mut self, node: &AwaitExpr) {
    //     todo!(stringify!(AwaitExpr));
    // }

    fn visit_tpl(&mut self, node: &Tpl) {
        todo!(stringify!(Tpl));
    }

    fn visit_tagged_tpl(&mut self, node: &TaggedTpl) {
        todo!(stringify!(TaggedTpl));
    }

    fn visit_tpl_element(&mut self, node: &TplElement) {
        todo!(stringify!(TplElement));
    }

    // fn visit_super(&mut self, node: &Super) {
    //     todo!(stringify!(Super));
    // }

    fn visit_function(&mut self, node: &Function) {
        node.body.visit_with(self);
        for param in node.params.iter().rev() {
            param.visit_with(self);
        }
    }

    fn visit_param(&mut self, node: &Param) {
        self.in_lhs = true;
        node.pat.visit_with(self);
        self.in_lhs = false;
    }

    fn visit_param_without_decorators(&mut self, node: &ParamWithoutDecorators) {
        self.in_lhs = true;
        node.pat.visit_with(self);
        self.in_lhs = false;
    }

    // fn visit_private_name(&mut self, node: &PrivateName) {
    //     todo!(stringify!(PrivateName));
    // }

    // fn visit_invalid(&mut self, node: &Invalid) {
    //     todo!(stringify!(Invalid));
    // }

    // fn visit_str(&mut self, node: &Str) {
    //     todo!(stringify!(Str));
    // }

    // fn visit_bool(&mut self, node: &Bool) {
    //     todo!(stringify!(Bool));
    // }

    // fn visit_null(&mut self, node: &Null) {
    //     todo!(stringify!(Null));
    // }

    // fn visit_number(&mut self, node: &Number) {
    //     todo!(stringify!(Number));
    // }

    // fn visit_big_int(&mut self, node: &BigInt) {
    //     todo!(stringify!(BigInt));
    // }

    // fn visit_regex(&mut self, node: &Regex) {
    //     todo!(stringify!(Regex));
    // }

    fn visit_export_default_expr(&mut self, node: &ExportDefaultExpr) {
        todo!(stringify!(ExportDefaultExpr));
    }

    fn visit_export_decl(&mut self, node: &ExportDecl) {
        todo!(stringify!(ExportDecl));
    }

    fn visit_import_decl(&mut self, node: &ImportDecl) {
        todo!(stringify!(ImportDecl));
    }

    fn visit_export_all(&mut self, node: &ExportAll) {
        todo!(stringify!(ExportAll));
    }

    fn visit_named_export(&mut self, node: &NamedExport) {
        todo!(stringify!(NamedExport));
    }

    fn visit_export_default_decl(&mut self, node: &ExportDefaultDecl) {
        todo!(stringify!(ExportDefaultDecl));
    }

    fn visit_import_default_specifier(&mut self, node: &ImportDefaultSpecifier) {
        todo!(stringify!(ImportDefaultSpecifier));
    }

    fn visit_import_star_as_specifier(&mut self, node: &ImportStarAsSpecifier) {
        todo!(stringify!(ImportStarAsSpecifier));
    }

    fn visit_import_named_specifier(&mut self, node: &ImportNamedSpecifier) {
        todo!(stringify!(ImportNamedSpecifier));
    }

    fn visit_export_namespace_specifier(&mut self, node: &ExportNamespaceSpecifier) {
        todo!(stringify!(ExportNamespaceSpecifier));
    }

    fn visit_export_default_specifier(&mut self, node: &ExportDefaultSpecifier) {
        todo!(stringify!(ExportDefaultSpecifier));
    }

    fn visit_export_named_specifier(&mut self, node: &ExportNamedSpecifier) {
        todo!(stringify!(ExportNamedSpecifier));
    }

    fn visit_script(&mut self, node: &Script) {
        todo!(stringify!(Script));
    }

    fn visit_module(&mut self, node: &Module) {
        todo!(stringify!(Module));
    }

    fn visit_array_pat(&mut self, node: &ArrayPat) {
        let old = self.in_destructuring;
        self.in_destructuring = true;
        for elem in node.elems.iter().rev() {
            elem.visit_with(self);
        }
        self.in_destructuring = old;
    }

    fn visit_object_pat(&mut self, node: &ObjectPat) {
        let old = self.in_destructuring;
        self.in_destructuring = true;
        for prop in node.props.iter().rev() {
            prop.visit_with(self);
        }
        self.in_destructuring = old;
    }

    fn visit_rest_pat(&mut self, node: &RestPat) {
        let old = self.in_lhs;
        self.in_lhs = true;
        node.arg.visit_with(self);
        self.in_lhs = old;
    }

    fn visit_key_value_pat_prop(&mut self, node: &KeyValuePatProp) {
        let old = self.in_lhs;
        self.in_lhs = true;
        node.value.visit_with(self);
        self.in_lhs = false;
        node.key.visit_with(self);
        self.in_lhs = old;
    }

    fn visit_key_value_prop(&mut self, node: &KeyValueProp) {
        node.value.visit_with(self);
        node.key.visit_with(self);
    }

    fn visit_assign_prop(&mut self, node: &AssignProp) {
        todo!(stringify!(AssignProp));
    }

    fn visit_getter_prop(&mut self, node: &GetterProp) {
        node.body.visit_with(self);
        node.key.visit_with(self);
    }

    fn visit_setter_prop(&mut self, node: &SetterProp) {
        node.body.visit_with(self);
        node.param.visit_with(self);
        node.key.visit_with(self);
    }

    fn visit_method_prop(&mut self, node: &MethodProp) {
        node.function.visit_with(self);
        node.key.visit_with(self);
    }

    // fn visit_computed_prop_name(&mut self, node: &ComputedPropName) {
    //     todo!(stringify!(ComputedPropName));
    // }

    // fn visit_spread_assignment(&mut self, node: &SpreadAssignment) {
    //     todo!(stringify!(SpreadAssignment));
    // }

    // fn visit_expr_stmt(&mut self, node: &ExprStmt) {
    //     todo!(stringify!(ExprStmt));
    // }

    // fn visit_empty_stmt(&mut self, node: &EmptyStmt) {
    //     todo!(stringify!(EmptyStmt));
    // }

    // fn visit_debugger_stmt(&mut self, node: &DebuggerStmt) {
    //     todo!(stringify!(DebuggerStmt));
    // }

    fn visit_with_stmt(&mut self, node: &WithStmt) {
        todo!(stringify!(WithStmt));
    }

    // fn visit_return_stmt(&mut self, node: &ReturnStmt) {
    //     todo!(stringify!(ReturnStmt));
    // }

    fn visit_labeled_stmt(&mut self, node: &LabeledStmt) {
        todo!(stringify!(LabeledStmt));
    }

    // fn visit_break_stmt(&mut self, node: &BreakStmt) {
    //     todo!(stringify!(BreakStmt));
    // }

    // fn visit_continue_stmt(&mut self, node: &ContinueStmt) {
    //     todo!(stringify!(ContinueStmt));
    // }

    // fn visit_if_stmt(&mut self, node: &IfStmt) {
    //     todo!(stringify!(IfStmt));
    // }

    // fn visit_throw_stmt(&mut self, node: &ThrowStmt) {
    //     todo!(stringify!(ThrowStmt));
    // }

    fn visit_try_stmt(&mut self, node: &TryStmt) {
        node.finalizer.visit_with(self);
        node.handler.visit_with(self);
        node.block.visit_with(self);
    }

    // fn visit_while_stmt(&mut self, node: &WhileStmt) {
    //     todo!(stringify!(WhileStmt));
    // }

    // fn visit_do_while_stmt(&mut self, node: &DoWhileStmt) {
    //     todo!(stringify!(DoWhileStmt));
    // }

    // fn visit_for_stmt(&mut self, node: &ForStmt) {
    //     todo!(stringify!(ForStmt));
    // }

    fn visit_catch_clause(&mut self, node: &CatchClause) {
        self.in_lhs = false;
        node.body.visit_with(self);
        self.in_lhs = true;
        node.param.visit_with(self);
    }

    fn visit_expr(&mut self, node: &Expr) {
        let old = self.in_lhs;
        self.in_lhs = false;
        node.visit_children_with(self);
        self.in_lhs = old;
    }
}

impl<'ast, 'a, T> DataFlowAnalysisInner<Node<'ast>, ReachingUses, ReachingUsesJoinOp>
    for Inner<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    fn add_lattice_element(&mut self, element: ReachingUses) -> LatticeElementId {
        self.lattice_elements.push(element)
    }

    fn is_forward(&self) -> bool {
        false
    }

    fn create_entry_lattice(&mut self) -> LatticeElementId {
        self.add_lattice_element(ReachingUses::default())
    }

    fn create_initial_estimate_lattice(&mut self) -> LatticeElementId {
        self.create_entry_lattice()
    }

    fn create_flow_joiner(&self) -> ReachingUsesJoinOp {
        ReachingUsesJoinOp::default()
    }

    /**
     * Computes the new LatticeElement for a given node given its LatticeElement from previous
     * iteration.
     *
     * @param n node
     * @param input - Backward dataflow analyses compute their LatticeElement bottom-up (i.e.
     *     LinearFlowState.out to LinearFlowState.in). See {@link DataFlowAnalysis#flow(DiGraphNode)}.
     *     Here param `input` is the readonly input LinearFlowState.out that was constructed as
     *     `LinearFlowState.in` in the previous iteration, or the initial lattice element if this is
     *     the first iteration.
     */
    fn flow_through(&mut self, node: Node<'ast>, input: LatticeElementId) -> LatticeElementId {
        let mut output = self.lattice_elements[input].clone();

        // If there's an ON_EX edge, this cfgNode may or may not get executed.
        // We can express this concisely by just pretending this happens in
        // a conditional.
        let conditional = self.has_exception_handler(node);
        self.compute_may_use(node, node, &mut output, conditional);

        if output != self.lattice_elements[input] {
            self.add_lattice_element(output)
        } else {
            // No changes compared to input.
            input
        }
    }

    fn cfg(&self) -> &ControlFlowGraph<Node<'ast>, LinearFlowState> {
        &self.cfg
    }
    fn cfg_mut(&mut self) -> &mut ControlFlowGraph<Node<'ast>, LinearFlowState> {
        &mut self.cfg
    }
}

impl<'a, T> Index<LatticeElementId> for Inner<'_, 'a, T>
where
    T: FunctionLike<'a>,
{
    type Output = ReachingUses;

    fn index(&self, index: LatticeElementId) -> &Self::Output {
        &self.lattice_elements[index]
    }
}

// TODO: temp pub:
/**
 * May use definition lattice representation. It captures a product lattice for each local
 * (non-escaped) variable. The sub-lattice is a n + 2 power set element lattice with all the Nodes
 * in the program, TOP and BOTTOM. This is better explained with an example:
 *
 * <p>Consider: A sub-lattice element representing the variable A represented by { N_4, N_5} where
 * N_x is a Node in the program. This implies at that particular point in the program the content
 * of A is "upward exposed" at point N_4 and N_5.
 *
 * <p>Example:
 *
 * <p><code>
 *
 * A = 1;
 * ...
 * N_3:
 * N_4: print(A);
 * N_5: y = A;
 * N_6: A = 1;
 * N_7: print(A);
 * </code>
 *
 * <p>At N_3, reads of A in {N_4, N_5} are said to be upward exposed.
 */
#[derive(Default, Clone, PartialEq, Debug)]
pub struct ReachingUses {
    // Maps variables to all their uses that are upward exposed at the current cfgNode.
    may_use_map: MultiMap<VarId, NodeId>,
    // public ReachingUses() {}

    // /**
    //  * Copy constructor.
    //  *
    //  * @param other The constructed object is a replicated copy of this element.
    //  */
    // public ReachingUses(ReachingUses other) {
    //   this.mayUseMap.putAll(other.mayUseMap);
    // }

    // @Override
    // public boolean equals(Object other) {
    //   return (other instanceof ReachingUses)
    //       && ((ReachingUses) other).mayUseMap.equals(this.mayUseMap);
    // }

    // @Override
    // public int hashCode() {
    //   return mayUseMap.hashCode();
    // }
}

impl Annotation for ReachingUses {}
impl LatticeElement for ReachingUses {}

/**
 * The join is a simple union because of the "may be" nature of the analysis.
 *
 * <p>Consider: A = 1; if (x) { A = 2 }; alert(A);
 *
 * <p>The read of A "may be" exposed to A = 1 in the beginning.
 */
#[derive(Default)]
struct ReachingUsesJoinOp {
    result: ReachingUses,
}

impl<'ast, 'a, T> FlowJoiner<ReachingUses, Inner<'ast, 'a, T>> for ReachingUsesJoinOp
where
    T: FunctionLike<'a>,
{
    fn join_flow(&mut self, inner: &mut Inner<'ast, 'a, T>, input: LatticeElementId) {
        for (k, v) in inner.lattice_elements[input].may_use_map.iter() {
            for v in v {
                self.result.may_use_map.put(*k, *v);
            }
        }
        // TODO:
        //   self.result.mayUseMap.putAll(uses.mayUseMap);
    }

    fn finish(self) -> ReachingUses {
        self.result
    }
}
