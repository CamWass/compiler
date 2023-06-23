use ast::*;
use global_common::SyntaxContext;
use rustc_hash::FxHashMap;
use swc_atoms::{js_word, JsWord};

use crate::control_flow::node::{Node, NodeKind};
use crate::control_flow::ControlFlowGraph::*;
use crate::utils::unwrap_as;
use crate::{Id, ToId};

use super::{is_simple_prop_name, FnId, PropKey, SimpleCFG, Store};

/// Builds a map containing the [`Steps`][Step] required to symbolically evaluate
/// each node in the given function.
pub(super) fn create_step_map<'ast>(
    store: &mut Store<'ast>,
    func: FnId,
) -> FxHashMap<NodeId, Vec<Step>> {
    let mut map = FxHashMap::default();

    let graph = &store.static_fn_data[func].cfg.graph;

    for node in graph.node_indices() {
        // Make assignments conditional if the node can end abruptly by an exception.
        let conditional = graph.edges(node).any(|e| *e.weight() == Branch::ON_EX);

        let mut steps = Vec::new();

        let mut analyser = Analyser {
            steps: &mut steps,
            cfg: &store.static_fn_data[func].cfg,
            unresolved_ctxt: store.unresolved_ctxt,
        };
        analyser.init(graph[node], conditional);

        if !steps.is_empty() {
            map.insert(graph[node].node_id, steps);
        }
    }

    map
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A place where a value can be stored.
pub enum LValue {
    /// Named variable.
    Var(Id),
    /// Property of the current RValue.
    RValueProp(JsWord),
    /// Property of the object type that represents the given object literal.
    ObjectProp(NodeId, JsWord),
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// The value of an expression.
pub enum RValue {
    NullOrVoid,
    /// Pointer to the value stored in the given named variable.
    Var(Id),
    /// Object type that represents the given object literal.
    Object(NodeId),
    /// Property of the current RValue.
    Prop(JsWord),
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// An abstract instruction representing part of a a JavaScript program.
/// Each instruction is largely stateless, and depends on the state created by
/// the previous steps.
pub enum Step {
    /// Stores the given value in the RValue register.
    StoreRValue(Option<RValue>),
    /// Stores the given value in the LValue register.
    StoreLValue(Option<LValue>),
    /// Assigns the value in the RValue register to the value in the LValue register.
    Assign(bool),
    /// Invalidates the value in the RValue register.
    InvalidateRValue,
    /// Invalidates the value in the LValue register.
    InvalidateLValue,
    /// Begins a new call, with the specified number of argument, by pushing it
    /// onto the call creation stack.
    StartCall(usize),
    /// Stores the value in the RValue register as the next argument to the current call.
    StoreArg,
    /// Executes a call, popping it from the stack and storing the result in the
    /// RValue register.
    Call,
    /// Returns the value in the RValue register. Execution will stop after this.
    Return,
    /// Creates a new union, pushing it onto the union creation stack.
    StartUnion,
    /// Adds the value in the RValue register to the current union.
    PushToUnion,
    /// Finalises the current union, popping it from the stack ans storing in
    /// the the RValue register.
    StoreUnion,
    /// Pushes a new RValue register onto the RValue stack. The current RValue
    /// is copied into the new register. Used to save values before visiting
    /// nested expressions.
    SaveRValue,
    /// Pops the current RValue register from the stack, restoring the previous
    /// RValue register and its value.
    RestoreRValue,
}

#[derive(Debug)]
struct Analyser<'a, 'ast> {
    steps: &'a mut Vec<Step>,
    cfg: &'a SimpleCFG<'ast>,
    unresolved_ctxt: SyntaxContext,
}

impl<'ast> Analyser<'_, 'ast> {
    /// Records the given step. Redundant steps may be skipped.
    fn push(&mut self, step: Step) {
        // Redundant store elimination
        if let Some(Step::StoreRValue(old_value)) = self.steps.last() {
            if let Step::StoreRValue(new_value) = &step {
                match (old_value, new_value) {
                    (None, None) => return,
                    (Some(RValue::NullOrVoid), Some(RValue::NullOrVoid)) => return,
                    // (Some(RValue::Register(_)), Some(RValue::Register(_))) => todo!(),
                    (Some(RValue::Object(o1)), Some(RValue::Object(o2))) if o1 == o2 => return,
                    _ => {}
                }
            }
        }

        self.steps.push(step);
    }

    /// Invalidate the RValue register.
    fn invalidate_r_value(&mut self) {
        self.push(Step::InvalidateRValue);
    }

    /// Invalidate the LValue register.
    fn invalidate_l_value(&mut self) {
        self.push(Step::InvalidateLValue);
    }

    fn record_assignment(
        &mut self,
        lhs: Node<'ast>,
        rhs: &'ast Expr,
        conditional: bool,
        op: AssignOp,
    ) {
        let conditional_assign = matches!(
            op,
            AssignOp::AndAssign | AssignOp::OrAssign | AssignOp::NullishAssign
        );
        match lhs.kind {
            NodeKind::ArrayPat(_) | NodeKind::ObjectPat(_) => {
                debug_assert!(!conditional_assign, "invalid assignment target");
                self.handle_destructuring(lhs, rhs, conditional)
            }
            _ => {
                self.visit_and_get_r_value(Node::from(rhs), conditional);
                self.steps.push(Step::SaveRValue);
                let conditional = conditional || conditional_assign;
                self.visit_and_get_slot(lhs, conditional);
                self.steps.push(Step::RestoreRValue);

                self.push(Step::Assign(conditional));
            }
        }
    }

    fn handle_destructuring(&mut self, lhs: Node<'ast>, rhs: &'ast Expr, conditional: bool) {
        self.visit_and_get_r_value(Node::from(rhs), conditional);
        self.visit_destructuring(lhs, conditional);
    }

    fn visit_destructuring(&mut self, lhs: Node<'ast>, conditional: bool) {
        match lhs.kind {
            NodeKind::ObjectPat(lhs) => {
                let has_complex_props = lhs.props.iter().any(|p| match p {
                    ObjectPatProp::KeyValue(p) => {
                        !is_simple_prop_name(&p.key, self.unresolved_ctxt)
                    }
                    ObjectPatProp::Assign(_) | ObjectPatProp::Rest(_) => false,
                });
                if has_complex_props {
                    self.invalidate_r_value();
                    for prop in &lhs.props {
                        self.visit_destructuring(Node::from(prop), conditional);
                    }
                } else {
                    for prop in &lhs.props {
                        match prop {
                            ObjectPatProp::KeyValue(prop) => {
                                let key = PropKey::from_prop_name(&prop.key, self.unresolved_ctxt)
                                    .unwrap();

                                if matches!(prop.value.as_ref(), Pat::Ident(_) | Pat::Expr(_)) {
                                    self.push(Step::SaveRValue);
                                    self.visit_and_get_slot(
                                        Node::from(prop.value.as_ref()),
                                        conditional,
                                    );
                                    self.push(Step::RestoreRValue);
                                    self.push(Step::SaveRValue);
                                    self.push(Step::StoreRValue(Some(RValue::Prop(key.0))));
                                    self.push(Step::Assign(conditional));
                                    self.push(Step::RestoreRValue);
                                } else {
                                    self.push(Step::SaveRValue);
                                    self.push(Step::StoreRValue(Some(RValue::Prop(key.0))));
                                    self.visit_destructuring(
                                        Node::from(prop.value.as_ref()),
                                        conditional,
                                    );
                                    self.push(Step::RestoreRValue);
                                }
                            }
                            ObjectPatProp::Assign(_) => {
                                unreachable!("removed by normalization");
                            }
                            ObjectPatProp::Rest(rest) => {
                                debug_assert!(lhs.props.last().unwrap() == prop);

                                // TODO: throw error, don't panic.
                                // The argument of an object pattern's rest element must be an identifier.
                                let arg = unwrap_as!(rest.arg.as_ref(), Pat::Ident(i), i);

                                // TODO: this is imprecise - rest patterns create a new, distinct, object, which has the remaining non-destructured
                                // properties copied over. These properties must have the same names as those in the original object after renaming.
                                // But since they are distinct objects, we don't want to conflate them.
                                let id = arg.to_id();
                                self.push(Step::StoreLValue(Some(LValue::Var(id))));
                                self.push(Step::Assign(conditional));
                            }
                        }
                    }
                }
            }

            NodeKind::BindingIdent(lhs) => {
                let id = lhs.to_id();
                self.push(Step::StoreLValue(Some(LValue::Var(id))));
                self.push(Step::Assign(conditional));
            }
            NodeKind::ArrayPat(lhs) => {
                self.invalidate_r_value();
                for element in lhs.elems.iter().filter_map(|e| e.as_ref()) {
                    if let Pat::Expr(elem) = element {
                        self.visit_and_get_slot(Node::from(elem.as_ref()), conditional);
                        self.invalidate_l_value();
                    } else {
                        self.visit_destructuring(Node::from(element), conditional);
                    }
                }
            }
            NodeKind::RestPat(lhs) => {
                self.invalidate_r_value();
                self.visit_destructuring(Node::from(lhs.arg.as_ref()), conditional);
            }
            NodeKind::AssignPat(lhs) => {
                self.push(Step::StartUnion);
                self.push(Step::PushToUnion);
                self.visit_and_get_r_value(Node::from(lhs.right.as_ref()), true);
                self.push(Step::PushToUnion);
                self.push(Step::StoreUnion);

                self.visit_destructuring(Node::from(lhs.left.as_ref()), conditional);
            }
            NodeKind::KeyValuePatProp(lhs) => {
                self.invalidate_r_value();
                if matches!(lhs.value.as_ref(), Pat::Ident(_) | Pat::Expr(_)) {
                    self.visit_and_get_slot(Node::from(lhs.value.as_ref()), conditional);
                    self.invalidate_l_value();
                } else {
                    self.visit_destructuring(Node::from(lhs.value.as_ref()), conditional);
                }
            }
            NodeKind::AssignPatProp(_) => {
                unreachable!("removed by normalization");
            }

            _ => {
                dbg!(lhs);
                unreachable!();
            }
        }
    }

    /// May change the current RValue, so use [`Step::SaveRValue`] if that matters.
    fn visit_and_get_slot(&mut self, node: Node<'ast>, conditional: bool) {
        match node.kind {
            NodeKind::Ident(ident) | NodeKind::BindingIdent(BindingIdent { id: ident, .. }) => {
                let id = ident.to_id();
                self.push(Step::StoreLValue(Some(LValue::Var(id))));
            }
            NodeKind::MemberExpr(node) => {
                self.visit_and_get_r_value(Node::from(&node.obj), conditional);
                if let Some(prop) =
                    PropKey::from_expr(&node.prop, self.unresolved_ctxt, node.computed)
                {
                    self.push(Step::StoreLValue(Some(LValue::RValueProp(prop.0))));
                } else {
                    self.invalidate_r_value();
                    self.visit_and_get_r_value(Node::from(node.prop.as_ref()), conditional);
                    self.push(Step::StoreLValue(None));
                }
            }
            _ => unreachable!(),
        }
    }

    fn visit_and_get_r_value(&mut self, node: Node<'ast>, conditional: bool) {
        match node.kind {
            // Don't traverse into new control flow nodes.
            NodeKind::FnExpr(_) | NodeKind::ArrowExpr(_) => {}

            NodeKind::AssignExpr(node) => {
                self.record_assignment(Node::from(&node.left), &node.right, conditional, node.op);
            }

            NodeKind::Class(_) => todo!(),
            NodeKind::ExtendsClause(_) => todo!(),
            NodeKind::ClassProp(_) => todo!(),
            NodeKind::PrivateProp(_) => todo!(),
            NodeKind::ClassMethod(_) => todo!(),
            NodeKind::PrivateMethod(_) => todo!(),
            NodeKind::Constructor(_) => todo!(),
            NodeKind::VarDeclarator(node) => {
                let lhs = Node::from(&node.name);
                if let Some(rhs) = &node.init {
                    self.record_assignment(lhs, rhs, conditional, AssignOp::Assign);
                } else {
                    self.visit_and_get_r_value(lhs, conditional);
                }
            }
            NodeKind::ThisExpr(_) => {
                self.push(Step::StoreRValue(None));
            }
            NodeKind::ArrayLit(node) => {
                for element in &node.elems {
                    if let Some(element) = element {
                        self.visit_and_get_r_value(Node::from(element), conditional);
                        // Can't track once it's in the array.
                        self.invalidate_r_value();
                    }
                }
                self.push(Step::StoreRValue(None));
            }
            NodeKind::ObjectLit(node) => {
                let is_simple_obj_lit = node.props.iter().all(|p| match p {
                    Prop::KeyValue(p) => is_simple_prop_name(&p.key, self.unresolved_ctxt),
                    _ => false,
                });

                if is_simple_obj_lit {
                    for prop in &node.props {
                        let prop = unwrap_as!(prop, Prop::KeyValue(p), p);
                        let key = PropKey::from_prop_name(&prop.key, self.unresolved_ctxt).unwrap();

                        self.visit_and_get_r_value(Node::from(prop.value.as_ref()), conditional);

                        self.push(Step::StoreLValue(Some(LValue::ObjectProp(
                            node.node_id,
                            key.0,
                        ))));
                        self.push(Step::Assign(conditional));
                    }
                } else {
                    self.invalidate_r_value();
                }
                self.push(Step::StoreRValue(Some(RValue::Object(node.node_id))));
            }
            NodeKind::SpreadElement(node) => {
                self.visit_and_get_r_value(Node::from(node.expr.as_ref()), conditional);
                self.invalidate_r_value();
                self.push(Step::StoreRValue(None));
            }
            NodeKind::UnaryExpr(node) => {
                self.visit_and_get_r_value(Node::from(node.arg.as_ref()), conditional);
                self.push(Step::StoreRValue(None));
            }
            NodeKind::UpdateExpr(node) => {
                self.visit_and_get_r_value(Node::from(node.arg.as_ref()), conditional);
                self.push(Step::StoreRValue(None));
            }
            NodeKind::BinExpr(node) => {
                match node.op {
                    BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::NullishCoalescing => {
                        // TODO: if LHS is object, then we know if RHS will execute.
                        self.push(Step::StartUnion);
                        self.visit_and_get_r_value(Node::from(node.left.as_ref()), conditional);
                        self.push(Step::PushToUnion);
                        self.visit_and_get_r_value(Node::from(node.right.as_ref()), true);
                        self.push(Step::PushToUnion);
                        self.push(Step::StoreUnion);
                    }
                    _ => {
                        self.visit_and_get_r_value(Node::from(node.left.as_ref()), conditional);
                        self.visit_and_get_r_value(Node::from(node.right.as_ref()), conditional);
                        self.push(Step::StoreRValue(None));
                    }
                }
            }
            NodeKind::ClassExpr(_) => todo!(),
            NodeKind::MemberExpr(node) => {
                self.visit_and_get_r_value(Node::from(&node.obj), conditional);
                if let Some(prop) =
                    PropKey::from_expr(&node.prop, self.unresolved_ctxt, node.computed)
                {
                    self.push(Step::StoreRValue(Some(RValue::Prop(prop.0))));
                } else {
                    self.invalidate_r_value();
                    self.visit_and_get_r_value(Node::from(node.prop.as_ref()), conditional);
                    self.push(Step::StoreRValue(None));
                }
            }
            NodeKind::CondExpr(node) => {
                self.visit_and_get_r_value(Node::from(node.test.as_ref()), conditional);
                self.push(Step::StartUnion);
                self.visit_and_get_r_value(Node::from(node.cons.as_ref()), true);
                self.push(Step::PushToUnion);
                self.visit_and_get_r_value(Node::from(node.alt.as_ref()), true);
                self.push(Step::PushToUnion);
                self.push(Step::StoreUnion);
            }
            NodeKind::CallExpr(node) => {
                self.visit_and_get_r_value(Node::from(&node.callee), conditional);
                self.push(Step::StartCall(node.args.len()));
                for arg in &node.args {
                    self.visit_and_get_r_value(Node::from(arg), conditional);
                    self.push(Step::StoreArg);
                }
                self.push(Step::Call);
            }
            NodeKind::NewExpr(node) => {
                self.visit_and_get_r_value(Node::from(node.callee.as_ref()), conditional);
                self.invalidate_r_value();

                if let Some(args) = &node.args {
                    for arg in args {
                        self.visit_and_get_r_value(Node::from(arg), conditional);
                        self.invalidate_r_value();
                    }
                }
                self.push(Step::StoreRValue(None));
            }
            NodeKind::SeqExpr(node) => {
                debug_assert!(!node.exprs.is_empty());

                let mut i = 0;
                while i < node.exprs.len() - 1 {
                    self.visit_and_get_r_value(Node::from(node.exprs[i].as_ref()), conditional);
                    i += 1;
                }

                self.visit_and_get_r_value(Node::from(node.exprs[i].as_ref()), conditional);
            }
            NodeKind::YieldExpr(node) => {
                if let Some(arg) = &node.arg {
                    self.visit_and_get_r_value(Node::from(arg.as_ref()), conditional);
                    self.invalidate_r_value();
                }
                self.push(Step::StoreRValue(None));
            }
            NodeKind::AwaitExpr(node) => {
                self.visit_and_get_r_value(Node::from(node.arg.as_ref()), conditional)
            }
            NodeKind::Tpl(node) => {
                for expr in &node.exprs {
                    self.visit_and_get_r_value(Node::from(expr.as_ref()), conditional);
                }
                self.push(Step::StoreRValue(None));
            }
            NodeKind::TaggedTpl(node) => {
                self.visit_and_get_r_value(Node::from(node.tag.as_ref()), conditional);
                for expr in &node.tpl.exprs {
                    self.visit_and_get_r_value(Node::from(expr.as_ref()), conditional);
                    // Expressions in tagged templates can be accessed by the tag function.
                    self.invalidate_r_value();
                }
                self.push(Step::StoreRValue(None));
            }
            NodeKind::ParenExpr(node) => {
                self.visit_and_get_r_value(Node::from(node.expr.as_ref()), conditional)
            }
            NodeKind::Super(_) => todo!(),
            NodeKind::OptChainExpr(_) => todo!(),
            NodeKind::Function(_) => todo!(),
            NodeKind::Param(_) => todo!(),
            NodeKind::ParamWithoutDecorators(_) => todo!(),
            NodeKind::BindingIdent(node) => {
                self.visit_and_get_r_value(Node::from(&node.id), conditional)
            }
            NodeKind::Ident(node) => {
                if node.span.ctxt == self.unresolved_ctxt && node.sym == js_word!("undefined") {
                    self.push(Step::StoreRValue(Some(RValue::NullOrVoid)));
                } else {
                    let id = node.to_id();
                    self.push(Step::StoreRValue(Some(RValue::Var(id))));
                }
            }
            NodeKind::PrivateName(_) => todo!(),

            NodeKind::Null(_) => {
                self.push(Step::StoreRValue(Some(RValue::NullOrVoid)));
            }

            NodeKind::Str(_)
            | NodeKind::Bool(_)
            | NodeKind::Number(_)
            | NodeKind::BigInt(_)
            | NodeKind::Regex(_)
            | NodeKind::TplElement(_)
            | NodeKind::MetaPropExpr(_) => {
                self.push(Step::StoreRValue(None));
            }

            NodeKind::ImportDefaultSpecifier(_) => todo!(),
            NodeKind::ImportStarAsSpecifier(_) => todo!(),
            NodeKind::ImportNamedSpecifier(_) => todo!(),
            NodeKind::ExportNamespaceSpecifier(_) => todo!(),
            NodeKind::ExportDefaultSpecifier(_) => todo!(),
            NodeKind::ExportNamedSpecifier(_) => todo!(),
            NodeKind::ComputedPropName(node) => {
                self.visit_and_get_r_value(Node::from(node.expr.as_ref()), conditional);
            }
            NodeKind::CatchClause(_) => todo!(),

            // This function is only called on expressions (and their children),
            // so it can't reach e.g. statements. TypeScript and JSX should have
            // been removed by now as well.
            _ => unreachable!(),
        }
    }

    /// Initiates the analysis of the given node.
    fn init(&mut self, node: Node<'ast>, conditional: bool) {
        match node.kind {
            NodeKind::IfStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.test.as_ref()), conditional);
            }
            NodeKind::ExprStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.expr.as_ref()), conditional);
            }
            NodeKind::BlockStmt(_) => {}
            NodeKind::ForStmt(node) => {
                if let Some(test) = &node.test {
                    self.visit_and_get_r_value(Node::from(test.as_ref()), conditional);
                }
            }
            NodeKind::Function(_) => {}
            NodeKind::VarDecl(node) => {
                for decl in &node.decls {
                    self.visit_and_get_r_value(Node::from(decl), conditional);
                }
            }
            NodeKind::ThisExpr(_)
            | NodeKind::ArrayLit(_)
            | NodeKind::ObjectLit(_)
            | NodeKind::SpreadElement(_)
            | NodeKind::UnaryExpr(_)
            | NodeKind::UpdateExpr(_)
            | NodeKind::BinExpr(_)
            | NodeKind::FnExpr(_)
            | NodeKind::ClassExpr(_)
            | NodeKind::AssignExpr(_)
            | NodeKind::MemberExpr(_)
            | NodeKind::CondExpr(_)
            | NodeKind::CallExpr(_)
            | NodeKind::NewExpr(_)
            | NodeKind::SeqExpr(_)
            | NodeKind::ArrowExpr(_)
            | NodeKind::YieldExpr(_)
            | NodeKind::MetaPropExpr(_)
            | NodeKind::AwaitExpr(_)
            | NodeKind::Tpl(_)
            | NodeKind::TaggedTpl(_)
            | NodeKind::TplElement(_)
            | NodeKind::ParenExpr(_)
            | NodeKind::Super(_)
            | NodeKind::OptChainExpr(_)
            | NodeKind::BindingIdent(_)
            | NodeKind::Ident(_)
            | NodeKind::PrivateName(_) => {
                self.visit_and_get_r_value(node, conditional);
            }

            NodeKind::ImplicitReturn => {
                self.push(Step::StoreRValue(Some(RValue::NullOrVoid)));
                self.push(Step::Return);
            }
            NodeKind::Class(_) => todo!(),
            NodeKind::ExtendsClause(_) => todo!(),
            NodeKind::ClassProp(_) => todo!(),
            NodeKind::PrivateProp(_) => todo!(),
            NodeKind::ClassMethod(_) => todo!(),
            NodeKind::PrivateMethod(_) => todo!(),
            NodeKind::Constructor(_) => todo!(),
            NodeKind::Decorator(_) => todo!(),
            NodeKind::FnDecl(_) => todo!(),
            NodeKind::ClassDecl(_) => todo!(),
            NodeKind::VarDeclarator(_) => todo!(),
            NodeKind::Param(_) => todo!(),
            NodeKind::ParamWithoutDecorators(_) => todo!(),
            NodeKind::ExportDefaultExpr(_) => todo!(),
            NodeKind::ExportDecl(_) => todo!(),
            NodeKind::ImportDecl(_) => todo!(),
            NodeKind::ExportAll(_) => todo!(),
            NodeKind::NamedExport(_) => todo!(),
            NodeKind::ExportDefaultDecl(_) => todo!(),
            NodeKind::ImportDefaultSpecifier(_) => todo!(),
            NodeKind::ImportStarAsSpecifier(_) => todo!(),
            NodeKind::ImportNamedSpecifier(_) => todo!(),
            NodeKind::ExportNamespaceSpecifier(_) => todo!(),
            NodeKind::ExportDefaultSpecifier(_) => todo!(),
            NodeKind::ExportNamedSpecifier(_) => todo!(),
            NodeKind::Script(_) => {}
            NodeKind::Module(_) => {}
            NodeKind::ArrayPat(_) => todo!(),
            NodeKind::ObjectPat(_) => todo!(),
            NodeKind::AssignPat(_) => todo!(),
            NodeKind::RestPat(_) => todo!(),
            NodeKind::KeyValuePatProp(_) => todo!(),
            NodeKind::AssignPatProp(_) => todo!(),
            NodeKind::KeyValueProp(_) => todo!(),
            NodeKind::AssignProp(_) => todo!(),
            NodeKind::GetterProp(_) => {}
            NodeKind::SetterProp(_) => {}
            NodeKind::ComputedPropName(_) => todo!(),
            NodeKind::SpreadAssignment(_) => todo!(),
            NodeKind::DebuggerStmt(_) => {}
            NodeKind::WithStmt(_) => todo!(),
            NodeKind::ReturnStmt(node) => {
                if let Some(arg) = &node.arg {
                    self.visit_and_get_r_value(Node::from(arg.as_ref()), conditional);
                    self.push(Step::Return);
                } else {
                    self.push(Step::StoreRValue(Some(RValue::NullOrVoid)));
                    self.push(Step::Return);
                }
            }
            NodeKind::LabeledStmt(_) => {}
            NodeKind::SwitchStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.discriminant.as_ref()), conditional);
            }
            NodeKind::ThrowStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.arg.as_ref()), conditional);
                self.invalidate_r_value();
                if self.cfg.get_successors(Node::from(node)).count() == 0 {
                    self.push(Step::Return);
                }
            }
            NodeKind::TryStmt(_) => {}
            NodeKind::WhileStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.test.as_ref()), conditional);
            }
            NodeKind::DoWhileStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.test.as_ref()), conditional);
            }
            NodeKind::ForInStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.right.as_ref()), conditional);
                self.invalidate_r_value();
            }
            NodeKind::ForOfStmt(_) => todo!(),
            NodeKind::SwitchCase(node) => {
                if let Some(test) = &node.test {
                    self.visit_and_get_r_value(Node::from(test.as_ref()), conditional);
                }
            }
            NodeKind::CatchClause(node) => {
                if let Some(param) = &node.param {
                    match param {
                        Pat::Array(_) | Pat::Object(_) => {
                            self.push(Step::StoreRValue(None));
                            self.visit_destructuring(Node::from(param), conditional)
                        }
                        _ => {
                            self.visit_and_get_slot(Node::from(param), conditional);
                            self.push(Step::StoreRValue(None));
                            self.push(Step::Assign(conditional));
                        }
                    }
                }
            }

            NodeKind::Str(_)
            | NodeKind::Bool(_)
            | NodeKind::Null(_)
            | NodeKind::Number(_)
            | NodeKind::BigInt(_)
            | NodeKind::Regex(_)
            | NodeKind::EmptyStmt(_)
            | NodeKind::BreakStmt(_)
            | NodeKind::ContinueStmt(_) => {}

            _ => unreachable!("{:#?}", node),
        }
    }
}
