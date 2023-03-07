use std::ops::Index;

use ast::*;
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use index::vec::{Idx, IndexVec};
use rustc_hash::{FxHashMap, FxHashSet};
use swc_atoms::JsWord;

use crate::control_flow::node::Node;
use crate::control_flow::ControlFlowAnalysis::*;
use crate::control_flow::ControlFlowGraph::*;
use crate::find_vars::*;
use crate::utils::unwrap_as;
use crate::DefaultNameGenerator::DefaultNameGenerator;
use crate::{DataFlowAnalysis::*, Id, ToId};

#[cfg(test)]
mod tests;

pub fn process(ast: &mut ast::Program) {
    let mut visitor = GlobalVisitor::default();
    ast.visit_with(&mut visitor);

    let mut renamer = Renamer {
        rename_map: visitor.rename_map,
    };
    ast.visit_mut_with(&mut renamer);
}

fn handle_fn<'a, T>(rename_map: &mut FxHashMap<NodeId, JsWord>, node: &'a T)
where
    T: FunctionLike<'a> + VisitWith<'a, DeclFinder>,
    &'a T: Into<ControlFlowRoot<'a>>,
{
    let vars = find_vars_declared_in_fn(node, false);

    let cfa = ControlFlowAnalysis::analyze(node.into(), false);

    let result = Analysis::new(cfa, vars.scopeVariables).analyze();

    // result.1.print_full_with_annotations::<DefaultPrinter>(None);

    // dbg!(result
    //     .0
    //     .lattice_elements
    //     .iter_enumerated()
    //     .collect::<Vec<_>>());

    let end = result
        .1
        .node_annotations
        .get(&result.1.implicit_return)
        .unwrap();
    let end = &result.0.lattice_elements[end.in_];
    // dbg!(end);

    for entity in end.entities.values() {
        if let Entity::Object(object) = entity {
            if object.invalidated {
                continue;
            }
            let mut name_gen = DefaultNameGenerator::default();
            for (_, prop) in &object.properties {
                let new_name = name_gen.generateNextName();
                for &reference in &prop.references {
                    rename_map.insert(reference, new_name.clone());
                }
            }
        }
    }

    node.visit_body_with(&mut FnVisitor { rename_map });
}

struct FnVisitor<'a> {
    rename_map: &'a mut FxHashMap<NodeId, JsWord>,
}

impl Visit<'_> for FnVisitor<'_> {
    fn visit_function(&mut self, node: &Function) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_constructor(&mut self, node: &Constructor) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_arrow_expr(&mut self, node: &ArrowExpr) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_getter_prop(&mut self, node: &GetterProp) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_setter_prop(&mut self, node: &SetterProp) {
        handle_fn(&mut self.rename_map, node);
    }
}

#[derive(Default)]
struct GlobalVisitor {
    rename_map: FxHashMap<NodeId, JsWord>,
}

impl Visit<'_> for GlobalVisitor {
    fn visit_function(&mut self, node: &Function) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_constructor(&mut self, node: &Constructor) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_arrow_expr(&mut self, node: &ArrowExpr) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_getter_prop(&mut self, node: &GetterProp) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_setter_prop(&mut self, node: &SetterProp) {
        handle_fn(&mut self.rename_map, node);
    }
}

#[derive(Debug)]
pub struct AnalysisResult {
    pub lattice_elements: IndexVec<LatticeElementId, Lattice>,
}

pub struct Analysis<'ast> {
    data_flow_analysis: DataFlowAnalysis<Node<'ast>, Inner<'ast>, Lattice, JoinOp>,
}

impl<'ast> Analysis<'ast> {
    pub fn new(
        cfa: ControlFlowAnalysisResult<Node<'ast>, LinearFlowState, LatticeElementId>,
        vars: FxHashMap<Id, VarId>,
    ) -> Self {
        let inner = Inner {
            lattice_elements: IndexVec::default(),
            cfg: cfa.cfg,

            vars,
            id_gen: IdGen::new(),
        };
        let data_flow_analysis = DataFlowAnalysis::new(inner, cfa.nodePriorities);

        Self { data_flow_analysis }
    }

    pub fn analyze(
        mut self,
    ) -> (
        AnalysisResult,
        ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,
    ) {
        self.data_flow_analysis.analyze();

        let result = AnalysisResult {
            lattice_elements: self.data_flow_analysis.inner.lattice_elements,
        };
        (result, self.data_flow_analysis.inner.cfg)
    }
}

#[derive(Debug)]
struct Inner<'ast> {
    lattice_elements: IndexVec<LatticeElementId, Lattice>,
    cfg: ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,

    vars: FxHashMap<Id, VarId>,
    id_gen: IdGen,
}

impl<'ast> Inner<'ast> {
    fn analyze_node<'a>(&mut self, n: Node<'ast>, lattice: &'a mut Lattice, conditional: bool) {
        let mut v = Analyser {
            analysis: self,
            lattice,
        };

        v.init(n, conditional);
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
/// Points to an [`Entity`]. Its identity is based on its source, so only one
/// [`Entity`] is ever created per source.
enum Pointer {
    ObjectLit(NodeId),
}

index::newtype_index!(struct EntityId { .. });

#[derive(Debug)]
struct IdGen {
    entity_id: EntityId,
}

impl IdGen {
    fn new() -> Self {
        Self {
            entity_id: EntityId::from_u32(0),
        }
    }

    fn next_entity_id(&mut self) -> EntityId {
        let id = self.entity_id;
        self.entity_id.increment_by(1);
        id
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Property {
    value: Option<Pointer>,
    references: FxHashSet<NodeId>,
}

#[derive(Debug, Clone, PartialEq)]
enum Entity {
    Object(Object),
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Object {
    properties: FxHashMap<JsWord, Property>,
    invalidated: bool,
}

#[derive(Debug, Clone, PartialEq)]
struct Assignment {
    rhs: Option<Pointer>,
}

/// A place where a variable can be stored.
enum Slot<'ast> {
    Var(Id),
    Prop(Pointer, &'ast Ident),
}

struct Analyser<'ast, 'a> {
    analysis: &'a mut Inner<'ast>,
    lattice: &'a mut Lattice,
}

impl<'ast, 'a> Analyser<'ast, 'a> {
    /// Adds a property with the given name and value to the entity that `obj`
    /// points to. If a property with that name already exists, it is replaced,
    /// unless this assignment is conditional, in which case the property is
    /// assigned a conflation of the old and new values.
    fn record_prop_assignment(
        &mut self,
        obj: Pointer,
        name: &Ident,
        value: Option<Pointer>,
        conditional: bool,
    ) {
        // println!(
        //     "declaring prop `{:#?}` with value `{:#?}` on object `{:#?}` ",
        //     prop, value, obj
        // );
        let object = self.lattice.get_object_mut(obj, &mut self.analysis.id_gen);

        match object.properties.get(&name.sym) {
            Some(existing) => {
                let value = if !conditional {
                    // supersede
                    value
                } else {
                    // conflate
                    let id1 = existing.value;
                    self.lattice
                        .record_conflation(id1, value, &mut self.analysis.id_gen)
                };
                let existing = self
                    .lattice
                    .get_object_mut(obj, &mut self.analysis.id_gen)
                    .properties
                    .get_mut(&name.sym)
                    .unwrap();
                existing.value = value;
                existing.references.insert(name.node_id);
            }
            None => {
                // initialize
                let mut references = FxHashSet::default();
                references.insert(name.node_id);
                object
                    .properties
                    .insert(name.sym.clone(), Property { value, references });
            }
        }
    }

    /// Assigns the given value to the variable with the given name. Any existing
    /// value is replaced, unless this assignment is conditional, in which case
    /// the variable is assigned a conflation of the old and new values.
    fn record_var_assignment(
        &mut self,
        name: Id,
        rhs: Option<Pointer>,
        conditional: bool,
    ) -> Option<Pointer> {
        if let Some(rhs_object) = rhs {
            if let Some(existing) = self.lattice.var_assignments.get(&name) {
                let rhs = if !conditional {
                    // supersede
                    Some(rhs_object)
                } else {
                    // conflate
                    self.lattice.record_conflation(
                        existing.rhs,
                        Some(rhs_object),
                        &mut self.analysis.id_gen,
                    )
                };
                let assign = Assignment { rhs };
                self.lattice.var_assignments.insert(name, assign);
            } else {
                // initialize
                let assign = Assignment {
                    rhs: Some(rhs_object),
                };
                self.lattice.var_assignments.insert(name, assign);
            }
            Some(rhs_object)
        } else {
            // Non-object assignment
            let assign = Assignment { rhs: None };
            self.lattice.var_assignments.insert(name, assign);
            None
        }
    }

    fn assign_to_slot(
        &mut self,
        slot: Option<Slot<'ast>>,
        rhs: Option<Pointer>,
        conditional: bool,
    ) -> Option<Pointer> {
        match slot {
            Some(Slot::Var(lhs)) => self.record_var_assignment(lhs, rhs, conditional),
            Some(Slot::Prop(obj, prop)) => {
                self.record_prop_assignment(obj, prop, rhs, conditional);
                Some(obj)
            }
            None => {
                // Unknown/invalid assignment target.
                self.invalidate(rhs);
                rhs
            }
        }
    }

    /// Records an assignment of the form `LHS OP RHS` e.g.
    /// ```js
    /// let a = 1
    /// ```
    /// ```js
    /// a.b = c
    /// ```
    /// ```js
    /// a ||= b
    /// ```
    /// ```js
    /// a().b ||= c
    /// ```
    ///
    /// `conditional` - Whether the entire assignment (LHS and RHS) is conditionally executed.
    fn record_assignment(
        &mut self,
        lhs: Node<'ast>,
        rhs: &'ast Expr,
        conditional: bool,
        op: AssignOp,
    ) -> Option<Pointer> {
        // println!(
        //     "recording assignment of value `{:#?}` to var `{:#?}` ",
        //     rhs, lhs
        // );
        let conditional_assign = matches!(
            op,
            AssignOp::AndAssign | AssignOp::OrAssign | AssignOp::NullishAssign
        );
        match lhs {
            Node::ArrayPat(_) | Node::ObjectPat(_) => {
                debug_assert!(!conditional_assign, "invalid assignment target");
                self.handle_destructuring(lhs, rhs, conditional)
            }
            _ => {
                let lhs = self.visit_and_get_slot(lhs, conditional);
                let conditional = conditional || conditional_assign;
                let rhs = self.visit_and_get_object(Node::from(rhs), conditional);

                self.assign_to_slot(lhs, rhs, conditional)
            }
        }
    }

    fn handle_destructuring(
        &mut self,
        lhs: Node<'ast>,
        rhs: &'ast Expr,
        conditional: bool,
    ) -> Option<Pointer> {
        let rhs = self.visit_and_get_object(Node::from(rhs), conditional);
        self.visit_destructuring(lhs, rhs, conditional);
        rhs
    }

    fn invalidate_slot(&mut self, node: Node<'ast>, conditional: bool) {
        let lhs = self.visit_and_get_slot(node, conditional);

        match lhs {
            Some(Slot::Var(lhs)) => {
                if let Some(existing) = self.lattice.var_assignments.get(&lhs) {
                    self.invalidate(existing.rhs);
                }
            }
            Some(Slot::Prop(obj, prop)) => {
                let object = self.lattice.get_object_mut(obj, &mut self.analysis.id_gen);

                if let Some(existing) = object.properties.get(&prop.sym) {
                    let value = existing.value;
                    self.invalidate(value);
                }
            }
            None => {}
        }
    }

    fn invalidate(&mut self, obj: Option<Pointer>) {
        if let Some(obj) = obj {
            self.lattice.invalidate(obj, &mut self.analysis.id_gen);
        }
    }

    fn visit_destructuring(&mut self, lhs: Node<'ast>, rhs: Option<Pointer>, conditional: bool) {
        match lhs {
            Node::ObjectPat(lhs) => {
                let has_non_ident_props = lhs.props.iter().any(|p| match p {
                    ObjectPatProp::KeyValue(p) => !matches!(&p.key, PropName::Ident(_)),
                    ObjectPatProp::Assign(_) | ObjectPatProp::Rest(_) => false,
                });
                if let Some(rhs) = rhs {
                    if has_non_ident_props {
                        self.lattice.invalidate(rhs, &mut self.analysis.id_gen);
                    }
                    let rhs_obj = self.lattice.get_object_mut(rhs, &mut self.analysis.id_gen);
                    if !rhs_obj.invalidated {
                        for prop in &lhs.props {
                            match prop {
                                ObjectPatProp::KeyValue(prop) => {
                                    let key = unwrap_as!(&prop.key, PropName::Ident(k), k);
                                    let rhs_obj =
                                        self.lattice.get_object_mut(rhs, &mut self.analysis.id_gen);
                                    let rhs_prop = rhs_obj.properties.get_mut(&key.sym);
                                    let rhs_value = rhs_prop.as_ref().and_then(|p| p.value);
                                    if let Some(rhs_prop) = rhs_prop {
                                        rhs_prop.references.insert(key.node_id);
                                    }
                                    if matches!(prop.value.as_ref(), Pat::Ident(_) | Pat::Expr(_)) {
                                        let slot = self.visit_and_get_slot(
                                            Node::from(prop.value.as_ref()),
                                            conditional,
                                        );
                                        self.assign_to_slot(slot, rhs_value, conditional);
                                    } else {
                                        self.visit_destructuring(
                                            Node::from(prop.value.as_ref()),
                                            rhs_value,
                                            conditional,
                                        );
                                    }
                                }
                                ObjectPatProp::Assign(_) => {
                                    unreachable!("removed my normalization");
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
                                    if self.analysis.vars.contains_key(&id) {
                                        self.record_var_assignment(id, Some(rhs), conditional);
                                    }
                                }
                            }
                        }
                        return;
                    }
                }
                for prop in &lhs.props {
                    self.visit_destructuring(Node::from(prop), None, conditional);
                }
            }

            Node::BindingIdent(lhs) => {
                let id = lhs.to_id();
                if self.analysis.vars.contains_key(&id) {
                    self.record_var_assignment(id, rhs, conditional);
                }
            }
            Node::ArrayPat(lhs) => {
                self.invalidate(rhs);
                for element in lhs.elems.iter().filter_map(|e| e.as_ref()) {
                    if let Pat::Expr(elem) = element {
                        self.invalidate_slot(Node::from(elem.as_ref()), conditional);
                    } else {
                        self.visit_destructuring(Node::from(element), None, conditional);
                    }
                }
            }
            Node::RestPat(lhs) => {
                self.invalidate(rhs);
                self.visit_destructuring(Node::from(lhs.arg.as_ref()), None, conditional);
            }
            Node::AssignPat(lhs) => {
                let default_value = self.visit_and_get_object(Node::from(lhs.right.as_ref()), true);

                let result =
                    self.lattice
                        .record_conflation(rhs, default_value, &mut self.analysis.id_gen);

                self.visit_destructuring(Node::from(lhs.left.as_ref()), result, conditional);
            }
            Node::KeyValuePatProp(lhs) => {
                self.invalidate(rhs);
                if matches!(lhs.value.as_ref(), Pat::Ident(_) | Pat::Expr(_)) {
                    self.invalidate_slot(Node::from(lhs.value.as_ref()), conditional);
                } else {
                    self.visit_destructuring(Node::from(lhs.value.as_ref()), None, conditional);
                }
            }
            Node::AssignPatProp(_) => {
                unreachable!("removed my normalization");
            }

            _ => {
                dbg!(lhs);
                unreachable!();
            }
        }
    }

    /// Visits the given expression. If the expression resolves to a valid assignment target, a [`Slot`]
    /// representing that target is returned.
    fn visit_and_get_slot(&mut self, node: Node<'ast>, conditional: bool) -> Option<Slot<'ast>> {
        match node {
            Node::Ident(node) => {
                let id = node.to_id();
                if self.analysis.vars.contains_key(&id) {
                    Some(Slot::Var(id))
                } else {
                    None
                }
            }
            Node::BindingIdent(node) => self.visit_and_get_slot(Node::Ident(&node.id), conditional),
            Node::MemberExpr(node) => {
                let obj = self.visit_and_get_object(Node::from(&node.obj), conditional);
                if node.computed {
                    self.invalidate(obj);
                    self.visit_and_get_slot(Node::from(node.prop.as_ref()), conditional);
                    None
                } else {
                    obj.map(|obj| {
                        let prop = unwrap_as!(node.prop.as_ref(), Expr::Ident(i), i);
                        Slot::Prop(obj, prop)
                    })
                }
            }

            // All other nodes cannot evaluate to a reference, and should return None (but we still
            // need to visit their children).
            _ => {
                self.visit_and_get_object(node, conditional);
                None
            }
        }
    }

    fn visit_and_get_object(&mut self, node: Node<'ast>, conditional: bool) -> Option<Pointer> {
        match node {
            // Don't traverse into new control flow nodes.
            Node::FnExpr(_) | Node::ArrowExpr(_) => None,

            Node::AssignExpr(node) => {
                self.record_assignment(Node::from(&node.left), &node.right, conditional, node.op)
            }

            Node::Class(_) => todo!(),
            Node::ExtendsClause(_) => todo!(),
            Node::ClassProp(_) => todo!(),
            Node::PrivateProp(_) => todo!(),
            Node::ClassMethod(_) => todo!(),
            Node::PrivateMethod(_) => todo!(),
            Node::Constructor(_) => todo!(),
            Node::VarDeclarator(node) => {
                let lhs = Node::from(&node.name);
                if let Some(rhs) = &node.init {
                    self.record_assignment(lhs, rhs, conditional, AssignOp::Assign);
                } else {
                    self.visit_and_get_object(lhs, conditional);
                }
                None
            }
            Node::ThisExpr(_) => todo!(),
            Node::ArrayLit(node) => {
                for element in &node.elems {
                    if let Some(element) = element {
                        let obj = self.visit_and_get_object(Node::from(element), conditional);
                        // Can't track once it's in the array.
                        self.invalidate(obj);
                    }
                }
                None
            }
            Node::ObjectLit(node) => {
                let pointer = Pointer::ObjectLit(node.node_id);
                if self.lattice.object_map.contains_key(&pointer) {
                    Some(pointer)
                } else {
                let is_simple_obj_lit = node.props.iter().all(|p| match p {
                    Prop::KeyValue(p) => matches!(&p.key, PropName::Ident(_)),
                    _ => false,
                });

                if is_simple_obj_lit {
                    let mut obj = Object::default();
                    obj.properties.reserve(node.props.len());

                    let new_entity_id = self.analysis.id_gen.next_entity_id();
                    self.lattice
                        .entities
                        .insert(new_entity_id, Entity::Object(obj));

                        self.lattice.object_map.insert(pointer, new_entity_id);

                    for prop in &node.props {
                        let prop = unwrap_as!(prop, Prop::KeyValue(p), p);
                        let key = unwrap_as!(&prop.key, PropName::Ident(k), k);
                            let value = self
                                .visit_and_get_object(Node::from(prop.value.as_ref()), conditional);

                            self.record_prop_assignment(pointer, key, value, conditional);
                    }

                        Some(pointer)
                } else {
                    todo!();
                    }
                }
            }
            Node::SpreadElement(node) => {
                let obj = self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional);
                self.invalidate(obj);
                None
            }
            Node::UnaryExpr(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional);
                None
            }
            Node::UpdateExpr(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional);
                None
            }
            Node::BinExpr(node) => {
                match node.op {
                    BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::NullishCoalescing => {
                        // TODO: if LHS is object, then we know if RHS will execute.
                        let left =
                            self.visit_and_get_object(Node::from(node.left.as_ref()), conditional);
                        let right =
                            self.visit_and_get_object(Node::from(node.right.as_ref()), true);
                        self.lattice
                            .record_conflation(left, right, &mut self.analysis.id_gen)
                    }
                    _ => {
                        self.visit_and_get_object(Node::from(node.left.as_ref()), conditional);
                        self.visit_and_get_object(Node::from(node.right.as_ref()), conditional);
                        None
                    }
                }
            }
            Node::ClassExpr(_) => todo!(),
            Node::MemberExpr(node) => {
                let obj = self.visit_and_get_object(Node::from(&node.obj), conditional);
                if node.computed {
                    self.invalidate(obj);
                    self.visit_and_get_object(Node::from(node.prop.as_ref()), conditional);
                    None
                } else {
                    if let Some(obj) = obj {
                        let ident = unwrap_as!(node.prop.as_ref(), Expr::Ident(i), i);
                        let object = self.lattice.get_object_mut(obj, &mut self.analysis.id_gen);
                        if let Some(prop) = object.properties.get_mut(&ident.sym) {
                            prop.references.insert(ident.node_id);
                            prop.value
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
            }
            Node::CondExpr(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
                let cons = self.visit_and_get_object(Node::from(node.cons.as_ref()), true);
                let alt = self.visit_and_get_object(Node::from(node.alt.as_ref()), true);
                self.lattice
                    .record_conflation(cons, alt, &mut self.analysis.id_gen)
            }
            Node::CallExpr(node) => {
                let callee = self.visit_and_get_object(Node::from(&node.callee), conditional);
                self.invalidate(callee);

                for arg in &node.args {
                    let obj = self.visit_and_get_object(Node::from(arg), conditional);
                    self.invalidate(obj);
                }
                None
            }
            Node::NewExpr(node) => {
                let callee =
                    self.visit_and_get_object(Node::from(node.callee.as_ref()), conditional);
                self.invalidate(callee);

                if let Some(args) = &node.args {
                    for arg in args {
                        let obj = self.visit_and_get_object(Node::from(arg), conditional);
                        self.invalidate(obj);
                    }
                }
                None
            }
            Node::SeqExpr(node) => {
                debug_assert!(node.exprs.len() > 0);

                let mut i = 0;
                while i < node.exprs.len() - 1 {
                    self.visit_and_get_object(Node::from(node.exprs[i].as_ref()), conditional);
                    i += 1;
                }

                self.visit_and_get_object(Node::from(node.exprs[i].as_ref()), conditional)
            }
            Node::YieldExpr(node) => {
                if let Some(arg) = &node.arg {
                    let arg = self.visit_and_get_object(Node::from(arg.as_ref()), conditional);
                    self.invalidate(arg);
                }
                None
            }
            Node::AwaitExpr(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional)
            }
            Node::Tpl(node) => {
                for expr in &node.exprs {
                    self.visit_and_get_object(Node::from(expr.as_ref()), conditional);
                }
                None
            }
            Node::TaggedTpl(node) => {
                self.visit_and_get_object(Node::from(node.tag.as_ref()), conditional);
                for expr in &node.tpl.exprs {
                    let obj = self.visit_and_get_object(Node::from(expr.as_ref()), conditional);
                    // Expressions in tagged templates can be accessed by the tag function.
                    self.invalidate(obj);
                }
                None
            }
            Node::ParenExpr(node) => {
                self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional)
            }
            Node::Super(_) => todo!(),
            Node::OptChainExpr(_) => todo!(),
            Node::Function(_) => todo!(),
            Node::Param(_) => todo!(),
            Node::ParamWithoutDecorators(_) => todo!(),
            Node::BindingIdent(node) => {
                self.visit_and_get_object(Node::Ident(&node.id), conditional)
            }
            Node::Ident(node) => {
                let id = node.to_id();
                self.lattice
                    .var_assignments
                    .get(&id)
                    .and_then(|assign| assign.rhs)
            }
            Node::PrivateName(_) => todo!(),

            Node::Str(_)
            | Node::Bool(_)
            | Node::Null(_)
            | Node::Number(_)
            | Node::BigInt(_)
            | Node::Regex(_)
            | Node::TplElement(_)
            | Node::MetaPropExpr(_) => None,

            Node::ImportDefaultSpecifier(_) => todo!(),
            Node::ImportStarAsSpecifier(_) => todo!(),
            Node::ImportNamedSpecifier(_) => todo!(),
            Node::ExportNamespaceSpecifier(_) => todo!(),
            Node::ExportDefaultSpecifier(_) => todo!(),
            Node::ExportNamedSpecifier(_) => todo!(),
            Node::ComputedPropName(node) => {
                self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional);
                None
            }
            Node::SwitchCase(_) => todo!(),
            Node::CatchClause(_) => todo!(),

            // This function is only called on expressions (and their children),
            // so it can't reach e.g. statements. TypeScript and JSX should have
            // been removed by now as well.
            _ => unreachable!(),
        }
    }

    fn init(&mut self, node: Node<'ast>, conditional: bool) {
        match node {
            Node::IfStmt(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
            }
            Node::ExprStmt(node) => {
                self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional);
            }
            Node::BlockStmt(_) => {}
            Node::Function(_) => {
                // TODO: params:
            }
            Node::VarDecl(node) => {
                debug_assert_eq!(node.decls.len(), 1);
                self.visit_and_get_object(Node::from(node.decls.first().unwrap()), conditional);
            }
            _ => todo!("{:#?}", node),
        }
    }
}

impl<'ast> DataFlowAnalysisInner<Node<'ast>, Lattice, JoinOp> for Inner<'ast> {
    fn add_lattice_element(&mut self, element: Lattice) -> LatticeElementId {
        self.lattice_elements.push(element)
    }

    fn isForward(&self) -> bool {
        true
    }

    fn createEntryLattice(&mut self) -> LatticeElementId {
        self.add_lattice_element(Lattice::default())
    }

    fn createInitialEstimateLattice(&mut self) -> LatticeElementId {
        self.createEntryLattice()
    }

    fn createFlowJoiner(&self) -> JoinOp {
        JoinOp::new()
    }

    fn flowThrough(&mut self, node: Node<'ast>, input: LatticeElementId) -> LatticeElementId {
        // Make assignments conditional if the node can end abruptly by an exception.
        let conditional = self
            .cfg
            .graph
            .edges(self.cfg.map[&node])
            .any(|e| *e.weight() == Branch::ON_EX);

        // TODO: use Cow to avoid cloning and doing equality checks for nodes
        // that don't change the state. If the Cow is still Cow::Borrowed after
        // the analysis then there is no need for equality checks.
        let mut new = self.lattice_elements[input].clone();

        self.analyze_node(node, &mut new, conditional);

        if new != self.lattice_elements[input] {
            self.add_lattice_element(new)
        } else {
            // No changes compared to input.
            input
        }
    }

    fn cfg(&self) -> &ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId> {
        &self.cfg
    }
    fn cfg_mut(&mut self) -> &mut ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId> {
        &mut self.cfg
    }
}

impl Index<LatticeElementId> for Inner<'_> {
    type Output = Lattice;

    fn index(&self, index: LatticeElementId) -> &Self::Output {
        &self.lattice_elements[index]
    }
}

struct JoinOp {
    result: Lattice,
}

impl JoinOp {
    fn new() -> Self {
        Self {
            result: Lattice::default(),
        }
    }

    fn record_var_assignment(&mut self, lhs: &Id, incoming: &Assignment, id_gen: &mut IdGen) {
        // println!("JOIN: record_var_assignment: id: {:?}", lhs);
        if let Some(existing) = self.result.var_assignments.get(lhs) {
            // conflate
            // println!(
            //     "JOIN: record_var_assignment: conflating {:?} and {:?}",
            //     existing.rhs, incoming.rhs
            // );
            let conflation = self
                .result
                .record_conflation(existing.rhs, incoming.rhs, id_gen);
            self.result
                .var_assignments
                .insert(lhs.clone(), Assignment { rhs: conflation });
        } else {
            // initialize with incoming
            // println!(
            //     "JOIN: record_var_assignment: initializing {:?}",
            //     incoming.rhs
            // );
            self.result
                .var_assignments
                .insert(lhs.clone(), incoming.clone());
        }
    }

    /// Merges an incoming [`Entity`] into an existing one.
    fn merge_entities(&mut self, existing: EntityId, incoming: &Entity, id_gen: &mut IdGen) {
        match incoming {
            Entity::Object(incoming) => {
                let existing_entity = unwrap_as!(
                    self.result.entities.get_mut(&existing),
                    Some(Entity::Object(o)),
                    o
                );
                existing_entity.invalidated |= incoming.invalidated;

                for (key, prop) in &incoming.properties {
                    let existing_entity = unwrap_as!(
                        self.result.entities.get_mut(&existing),
                        Some(Entity::Object(o)),
                        o
                    );
                    match existing_entity.properties.get(key) {
                        Some(existing_prop) => {
                            // conflate
                            let existing_value = existing_prop.value;
                            let value =
                                self.result
                                    .record_conflation(existing_value, prop.value, id_gen);

                            let existing_entity = unwrap_as!(
                                self.result.entities.get_mut(&existing),
                                Some(Entity::Object(o)),
                                o
                            );
                            let existing_prop = existing_entity.properties.get_mut(key).unwrap();
                            existing_prop.value = value;
                            existing_prop
                                .references
                                .extend(prop.references.iter().cloned());
                        }
                        None => {
                            // initialize
                            existing_entity.properties.insert(key.clone(), prop.clone());
                        }
                    }
                }
            }
        }
    }
}

impl<'ast> FlowJoiner<Lattice, Inner<'ast>> for JoinOp {
    fn joinFlow(&mut self, inner: &mut Inner<'ast>, input: LatticeElementId) {
        let input = &inner.lattice_elements[input];

        // println!("======================JOIN:");
        // dbg!(&self.result);
        // dbg!(input);

        for (obj_id, entity_id) in &input.object_map {
            if let Some(existing) = self.result.object_map.get(obj_id) {
                let incoming = &input.entities[entity_id];
                self.merge_entities(*existing, incoming, &mut inner.id_gen);
            } else {
                self.result.object_map.insert(*obj_id, *entity_id);
                self.result
                    .entities
                    .insert(*entity_id, input.entities[entity_id].clone());
            }
        }

        for (lhs, incoming) in &input.var_assignments {
            self.record_var_assignment(lhs, incoming, &mut inner.id_gen);
        }

        // dbg!(&self.result);
        // println!("======================:");
    }

    fn finish(self) -> Lattice {
        self.result
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Lattice {
    entities: FxHashMap<EntityId, Entity>,
    object_map: FxHashMap<Pointer, EntityId>,
    var_assignments: FxHashMap<Id, Assignment>,
    // Records conflations that are yet to be merged.
    outstanding_conflations: FxHashMap<Pointer, FxHashSet<Pointer>>,
}

impl Lattice {
    /// Conflates the [`Entities`][`Entity`] that `id1` and `id2` point to.
    ///
    /// Note that this merely records the conflation - the objects are not merged
    /// until the conflation is used as a single object (e.g. property is accessed).
    fn record_conflation(
        &mut self,
        id1: Option<Pointer>,
        id2: Option<Pointer>,
        id_gen: &mut IdGen,
    ) -> Option<Pointer> {
        // println!("conflating objects `{:#?}` and `{:#?}` ", id1, id2);
        // TODO: invalidate if either is None. This can be avoided if we have a Some() and a None,
        // and the None represents a literal/primitive, in which case we know its properties and
        // the conflated object's props will be the properties of the non-primitive argument (Some)
        // subtract the primitive's props.
        // This is simplified if the primitive is null/undefined, since they have no props.
        let (id1, id2) = match (id1, id2) {
            (None, None) => return None,
            (None, Some(id2)) => {
                self.invalidate(id2, id_gen);
                return Some(id2);
            }
            (Some(id1), None) => {
                self.invalidate(id1, id_gen);
                return Some(id1);
            }
            (Some(id1), Some(id2)) => (id1, id2),
        };

        let entity_id1 = *self.object_map.get(&id1).unwrap();
        let entity_id2 = *self.object_map.get(&id2).unwrap();

        if entity_id1 != entity_id2 {
            self.outstanding_conflations
                .entry(id1)
                .or_default()
                .insert(id2);
            self.outstanding_conflations
                .entry(id2)
                .or_default()
                .insert(id1);
        }

        // Just return either of the inputs - they'll be treated as the same
        // object from now on.
        Some(id1)
    }

    /// Performs any outstanding conflations for the [`Entity`] that `obj` points to.
    /// This recursively merges the constituents into a single object.
    fn ensure_conflated(&mut self, obj: Pointer, id_gen: &mut IdGen) {
        // Find all objects that will be conflated.
        let mut queue = vec![obj];
        let mut flattened = FxHashSet::default();
        while let Some(obj) = queue.pop() {
            flattened.insert(obj);

            if let Some(constituents) = self.outstanding_conflations.remove(&obj) {
                for constituent in constituents {
                    if !flattened.contains(&constituent) {
                        queue.push(constituent);
                    }
                }
            }
        }

        if flattened.len() == 1 {
            // Nothing to conflate.
            return;
        }

        let new_entity_id = id_gen.next_entity_id();

        let mut result = Object::default();

        for constituent in flattened {
            // Update mapping so pointers point to new conflation.
            if let Some(old_entity_id) = self.object_map.insert(constituent, new_entity_id) {
            match self.entities.remove(&old_entity_id).unwrap() {
                Entity::Object(incoming) => {
                        result.invalidated |= incoming.invalidated;

                    for (key, prop) in incoming.properties {
                            match result.properties.entry(key) {
                                std::collections::hash_map::Entry::Occupied(mut entry) => {
                                // conflate
                                    let existing = entry.get().value;
                                    let value =
                                        self.record_conflation(existing, prop.value, id_gen);
                                    entry.get_mut().value = value;
                                    entry.get_mut().references.extend(prop.references);
                            }
                                std::collections::hash_map::Entry::Vacant(entry) => {
                                // initialize
                                    entry.insert(prop);
                                }
                            }
                            }
                        }
                    }
                }
            }

        self.entities.insert(new_entity_id, Entity::Object(result));
    }

    /// Returns the canonical representation of the [`Entity`] that `obj` points to.
    /// This involves performing any outstanding conflations.
    fn get_object_mut(&mut self, obj: Pointer, id_gen: &mut IdGen) -> &mut Object {
        self.ensure_conflated(obj, id_gen);
        let entity_id = *self.object_map.get(&obj).unwrap();
        unwrap_as!(
            self.entities.get_mut(&entity_id).unwrap(),
            Entity::Object(o),
            o
        )
    }

    /// Recursively invalidates the [`Entity`] that `obj` points to.
    /// This involves performing any outstanding conflations.
    fn invalidate(&mut self, obj: Pointer, id_gen: &mut IdGen) {
        // println!("invalidating object `{:#?}`", obj);
        self.get_object_mut(obj, id_gen).invalidated = true;

        let mut queue = self
            .get_object_mut(obj, id_gen)
            .properties
            .values()
            .flat_map(|p| p.value)
            .collect::<Vec<_>>();
        let mut done = FxHashSet::default();

        while let Some(obj) = queue.pop() {
            if done.contains(&obj) {
                continue;
            }

            let object = self.get_object_mut(obj, id_gen);

            object.invalidated = true;

            for obj in object.properties.values().flat_map(|p| p.value) {
                if !done.contains(&obj) {
                    queue.push(obj);
                }
            }

            done.insert(obj);
        }
    }
}

impl Annotation for Lattice {}
impl LatticeElement for Lattice {}

struct Renamer {
    rename_map: FxHashMap<NodeId, JsWord>,
}

impl VisitMut<'_> for Renamer {
    fn visit_mut_ident(&mut self, node: &mut Ident) {
        if let Some(new_name) = self.rename_map.get(&node.node_id) {
            node.sym = new_name.clone();
        }
    }
}
