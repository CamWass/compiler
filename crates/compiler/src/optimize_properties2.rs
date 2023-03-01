use std::ops::Index;

use ast::*;
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use index::vec::Idx;
use index::vec::IndexVec;
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

index::newtype_index!(struct ObjectId { .. });
index::newtype_index!(struct EntityId { .. });

#[derive(Debug)]
struct IdGen {
    entity_id: EntityId,
    object_id: ObjectId,
}

impl IdGen {
    fn new() -> Self {
        Self {
            entity_id: EntityId::from_u32(0),
            object_id: ObjectId::from_u32(0),
        }
    }

    fn next_entity_id(&mut self) -> EntityId {
        let id = self.entity_id;
        self.entity_id.increment_by(1);
        id
    }
    fn next_object_id(&mut self) -> ObjectId {
        let id = self.object_id;
        self.object_id.increment_by(1);
        id
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Property {
    value: Option<ObjectId>,
    references: FxHashSet<NodeId>,
}

#[derive(Debug, Clone, PartialEq)]
enum Entity {
    Conflation(FxHashSet<ObjectId>),
    Object(Object),
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Object {
    properties: FxHashMap<Id, Property>,
    invalidated: bool,
}

#[derive(Debug, Clone, PartialEq)]
struct Assignment {
    rhs: Option<ObjectId>,
}

/// A place where a variable can be stored.
enum Slot<'ast> {
    Var(Id),
    Prop(ObjectId, &'ast Ident),
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
        obj: ObjectId,
        name: &Ident,
        value: Option<ObjectId>,
        conditional: bool,
    ) {
        // println!(
        //     "declaring prop `{:#?}` with value `{:#?}` on object `{:#?}` ",
        //     prop, value, obj
        // );
        let object = self.lattice.get_object_mut(obj, &mut self.analysis.id_gen);
        let name_id = name.to_id();

        match object.properties.get(&name_id) {
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
                    .get_mut(&name_id)
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
                    .insert(name_id, Property { value, references });
            }
        }
    }

    /// Assigns the given value to the variable with the given name. Any existing
    /// value is replaced, unless this assignment is conditional, in which case
    /// the variable is assigned a conflation of the old and new values.
    fn record_var_assignment(
        &mut self,
        name: Id,
        rhs: Option<ObjectId>,
        conditional: bool,
    ) -> Option<ObjectId> {
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
    ///
    /// `conditional_assign` - Whether the RHS is conditionally executed.
    fn record_assignment(
        &mut self,
        lhs: Node<'ast>,
        rhs: Node<'ast>,
        conditional: bool,
        conditional_assign: bool,
    ) -> Option<ObjectId> {
        // println!(
        //     "recording assignment of value `{:#?}` to var `{:#?}` ",
        //     rhs, lhs
        // );
        let lhs = self.visit_and_get_slot(lhs, conditional);
        let conditional = conditional || conditional_assign;
        let rhs = self.visit_and_get_object(rhs, conditional);

        match lhs {
            Some(Slot::Var(lhs)) => self.record_var_assignment(lhs, rhs, conditional),
            Some(Slot::Prop(obj, prop)) => {
                self.record_prop_assignment(obj, prop, rhs, conditional);
                Some(obj)
            }
            None => {
                // Unknown/invalid assignment target.
                if let Some(rhs) = rhs {
                    self.lattice.invalidate(rhs, &mut self.analysis.id_gen);
                }
                rhs
            }
        }
    }

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
                    if let Some(obj) = obj {
                        self.lattice.invalidate(obj, &mut self.analysis.id_gen);
                    }
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

            // Don't traverse into new control flow nodes.
            Node::FnExpr(_) | Node::ArrowExpr(_) => None,

            // Terminals.
            Node::Str(_)
            | Node::Bool(_)
            | Node::Null(_)
            | Node::Number(_)
            | Node::BigInt(_)
            | Node::Regex(_) => None,

            Node::Class(_) => todo!(),
            Node::ExtendsClause(_) => todo!(),
            Node::ClassProp(_) => todo!(),
            Node::PrivateProp(_) => todo!(),
            Node::ClassMethod(_) => todo!(),
            Node::PrivateMethod(_) => todo!(),
            Node::Constructor(_) => todo!(),
            Node::ThisExpr(_) => todo!(),
            Node::ArrayLit(_) => todo!(),
            Node::ObjectLit(_) => todo!(),
            Node::SpreadElement(_) => todo!(),
            Node::UnaryExpr(_) => todo!(),
            Node::UpdateExpr(_) => todo!(),
            Node::BinExpr(_) => todo!(),
            Node::ClassExpr(_) => todo!(),
            Node::AssignExpr(_) => todo!(),
            Node::CondExpr(_) => todo!(),
            Node::CallExpr(_) => todo!(),
            Node::NewExpr(_) => todo!(),
            Node::SeqExpr(_) => todo!(),
            Node::YieldExpr(_) => todo!(),
            Node::MetaPropExpr(_) => todo!(),
            Node::AwaitExpr(_) => todo!(),
            Node::Tpl(_) => todo!(),
            Node::TaggedTpl(_) => todo!(),
            Node::TplElement(_) => todo!(),
            Node::ParenExpr(_) => todo!(),
            Node::Super(_) => todo!(),
            Node::OptChainExpr(_) => todo!(),
            Node::Function(_) => todo!(),
            Node::Param(_) => todo!(),
            Node::ParamWithoutDecorators(_) => todo!(),
            Node::PrivateName(_) => todo!(),
            Node::ExportDefaultExpr(_) => todo!(),
            Node::ExportDecl(_) => todo!(),
            Node::ImportDecl(_) => todo!(),
            Node::ExportAll(_) => todo!(),
            Node::NamedExport(_) => todo!(),
            Node::ExportDefaultDecl(_) => todo!(),
            Node::ImportDefaultSpecifier(_) => todo!(),
            Node::ImportStarAsSpecifier(_) => todo!(),
            Node::ImportNamedSpecifier(_) => todo!(),
            Node::ExportNamespaceSpecifier(_) => todo!(),
            Node::ExportDefaultSpecifier(_) => todo!(),
            Node::ExportNamedSpecifier(_) => todo!(),
            Node::ArrayPat(_) => todo!(),
            Node::ObjectPat(_) => todo!(),
            Node::AssignPat(_) => todo!(),
            Node::RestPat(_) => todo!(),
            Node::KeyValuePatProp(_) => todo!(),
            Node::AssignPatProp(_) => todo!(),
            Node::KeyValueProp(_) => todo!(),
            Node::AssignProp(_) => todo!(),
            Node::GetterProp(_) => todo!(),
            Node::SetterProp(_) => todo!(),
            Node::MethodProp(_) => todo!(),
            Node::ComputedPropName(_) => todo!(),
            Node::SpreadAssignment(_) => todo!(),
            Node::SwitchCase(_) => todo!(),
            Node::CatchClause(_) => todo!(),

            // This function is only called on expressions (and their children),
            // so it can't reach e.g. statements. TypeScript and JSX should have
            // been removed by now as well.
            _ => unreachable!(),
        }
    }

    fn visit_and_get_object(&mut self, node: Node<'ast>, conditional: bool) -> Option<ObjectId> {
        match node {
            // Don't traverse into new control flow nodes.
            Node::FnExpr(_) | Node::ArrowExpr(_) => None,

            Node::AssignExpr(node) => {
                let expr_lhs = match &node.left {
                    PatOrExpr::Expr(_) => true,
                    PatOrExpr::Pat(n) => match n.as_ref() {
                        Pat::Expr(_) => true,
                        Pat::Ident(_) => true,
                        _ => false,
                    },
                };

                let conditional_assign = matches!(
                    node.op,
                    AssignOp::AndAssign | AssignOp::OrAssign | AssignOp::NullishAssign
                );

                if expr_lhs {
                    self.record_assignment(
                        Node::from(&node.left),
                        Node::from(node.right.as_ref()),
                        conditional,
                        conditional_assign,
                    )
                } else {
                    todo!("destructuring");
                }
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
                    debug_assert!(!matches!(&node.name, Pat::Expr(_)));
                    let lhs_ident = matches!(&node.name, Pat::Ident(_));

                    if lhs_ident {
                        self.record_assignment(lhs, Node::from(rhs.as_ref()), conditional, false)
                    } else {
                        todo!("destructuring");
                    }
                } else {
                    self.visit_and_get_object(lhs, conditional);
                    None
                }
            }
            Node::ThisExpr(_) => todo!(),
            Node::ArrayLit(_) => todo!(),
            Node::ObjectLit(node) => {
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

                    let next_object_id = self.analysis.id_gen.next_object_id();
                    self.lattice
                        .object_map
                        .insert(next_object_id, new_entity_id);

                    for prop in &node.props {
                        let prop = unwrap_as!(prop, Prop::KeyValue(p), p);
                        let key = unwrap_as!(&prop.key, PropName::Ident(k), k);
                        let value =
                            self.visit_and_get_object(Node::from(prop.value.as_ref()), conditional);

                        self.record_prop_assignment(next_object_id, key, value, conditional);
                    }

                    Some(next_object_id)
                } else {
                    None
                }
            }
            Node::SpreadElement(_) => todo!(),
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
                    if let Some(obj) = obj {
                        self.lattice.invalidate(obj, &mut self.analysis.id_gen);
                    }
                    self.visit_and_get_object(Node::from(node.prop.as_ref()), conditional);
                    None
                } else {
                    if let Some(obj) = obj {
                        let ident = unwrap_as!(node.prop.as_ref(), Expr::Ident(i), i);
                        let id = ident.to_id();
                        let object = self.lattice.get_object_mut(obj, &mut self.analysis.id_gen);
                        if let Some(prop) = object.properties.get_mut(&id) {
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
            Node::CallExpr(_) => todo!(),
            Node::NewExpr(_) => todo!(),
            Node::SeqExpr(node) => {
                debug_assert!(node.exprs.len() > 0);

                let mut i = 0;
                while i < node.exprs.len() - 1 {
                    self.visit_and_get_object(Node::from(node.exprs[i].as_ref()), conditional);
                    i += 1;
                }

                self.visit_and_get_object(Node::from(node.exprs[i].as_ref()), conditional)
            }
            Node::YieldExpr(_) => todo!(),
            Node::MetaPropExpr(_) => todo!(),
            Node::AwaitExpr(_) => todo!(),
            Node::Tpl(_) => todo!(),
            Node::TaggedTpl(_) => todo!(),
            Node::TplElement(_) => todo!(),
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
            | Node::Regex(_) => None,

            Node::ExportDefaultExpr(_) => todo!(),
            Node::ExportDecl(_) => todo!(),
            Node::ImportDecl(_) => todo!(),
            Node::ExportAll(_) => todo!(),
            Node::NamedExport(_) => todo!(),
            Node::ExportDefaultDecl(_) => todo!(),
            Node::ImportDefaultSpecifier(_) => todo!(),
            Node::ImportStarAsSpecifier(_) => todo!(),
            Node::ImportNamedSpecifier(_) => todo!(),
            Node::ExportNamespaceSpecifier(_) => todo!(),
            Node::ExportDefaultSpecifier(_) => todo!(),
            Node::ExportNamedSpecifier(_) => todo!(),
            Node::Script(_) => todo!(),
            Node::Module(_) => todo!(),
            Node::ArrayPat(_) => todo!(),
            Node::ObjectPat(_) => todo!(),
            Node::AssignPat(_) => todo!(),
            Node::RestPat(_) => todo!(),
            Node::KeyValuePatProp(_) => todo!(),
            Node::AssignPatProp(_) => todo!(),
            Node::KeyValueProp(_) => todo!(),
            Node::AssignProp(_) => todo!(),
            Node::GetterProp(_) => todo!(),
            Node::SetterProp(_) => todo!(),
            Node::MethodProp(_) => todo!(),
            Node::ComputedPropName(_) => todo!(),
            Node::SpreadAssignment(_) => todo!(),
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
}

impl<'ast> FlowJoiner<Lattice, Inner<'ast>> for JoinOp {
    fn joinFlow(&mut self, inner: &mut Inner<'ast>, input: LatticeElementId) {
        let input = &inner.lattice_elements[input];

        // println!("======================JOIN:");
        // dbg!(&self.result);
        // dbg!(input);

        for (obj_id, entity_id) in &input.object_map {
            if let Some(existing) = self.result.object_map.get(obj_id) {
                debug_assert_eq!(&input.entities[entity_id], &self.result.entities[existing]);
            } else {
                self.result.object_map.insert(*obj_id, *entity_id);
            }
        }

        for (&entity_id, entity) in &input.entities {
            match self.result.entities.entry(entity_id) {
                std::collections::hash_map::Entry::Occupied(entry) => {
                    debug_assert_eq!(entity, entry.get());
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(entity.clone());
                }
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
    object_map: FxHashMap<ObjectId, EntityId>,
    var_assignments: FxHashMap<Id, Assignment>,
}

impl Lattice {
    /// Conflates the [`Entities`][`Entity`] that `id1` and `id2` point to.
    ///
    /// Note that this merely records the conflation - the objects are not merged
    /// until the conflation is used as a single object (e.g. property is accessed).
    fn record_conflation(
        &mut self,
        id1: Option<ObjectId>,
        id2: Option<ObjectId>,
        id_gen: &mut IdGen,
    ) -> Option<ObjectId> {
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

        if entity_id1 == entity_id2 {
            // Already been conflated.
            todo!("reachable?");
            // return;
        }

        let mut objects = FxHashSet::default();
        objects.insert(id1);
        objects.insert(id2);
        let conflation = Entity::Conflation(objects);

        let new_entity_id = id_gen.next_entity_id();
        self.entities.insert(new_entity_id, conflation);

        let next_object_id = id_gen.next_object_id();
        self.object_map.insert(next_object_id, new_entity_id);

        Some(next_object_id)
    }

    /// Performs any outstanding conflations for the [`Entity`] that `obj` points to.
    /// This recursively merges the constituents into a single object.
    fn ensure_conflated(&mut self, obj: ObjectId, id_gen: &mut IdGen) {
        let entity_id = self.object_map.get(&obj).unwrap();

        let entity = self.entities.get_mut(entity_id).unwrap();

        let entity = match entity {
            Entity::Conflation(_) => std::mem::replace(entity, Entity::Object(Object::default())),
            Entity::Object(_) => return,
        };
        let mut queue = unwrap_as!(entity, Entity::Conflation(o), o)
            .into_iter()
            .collect::<Vec<_>>();
        let mut done = FxHashSet::default();

        // println!("conflating objects `{:#?}`", &queue);

        while let Some(constituent) = queue.pop() {
            if done.contains(&constituent) {
                continue;
            }

            let entity_id = self.object_map.get(&constituent).unwrap();
            match self.entities.remove(entity_id).unwrap() {
                Entity::Conflation(objects) => {
                    for obj in objects {
                        if !done.contains(&obj) {
                            queue.push(obj);
                        }
                    }
                }
                Entity::Object(incoming) => {
                    self.get_object_mut(obj, id_gen).invalidated |= incoming.invalidated;

                    for (key, prop) in incoming.properties {
                        match self.get_object_mut(obj, id_gen).properties.get(&key) {
                            Some(existing) => {
                                // conflate
                                let existing = existing.value;
                                let value = self.record_conflation(existing, prop.value, id_gen);
                                let existing = self
                                    .get_object_mut(obj, id_gen)
                                    .properties
                                    .get_mut(&key)
                                    .unwrap();
                                existing.value = value;
                                existing.references.extend(prop.references);
                            }
                            None => {
                                // initialize
                                self.get_object_mut(obj, id_gen)
                                    .properties
                                    .insert(key, prop);
                            }
                        }
                    }
                }
            }

            done.insert(obj);
        }
    }

    /// Returns the canonical representation of the [`Entity`] that `obj` points to.
    /// This involves performing any outstanding conflations.
    fn get_object_mut(&mut self, obj: ObjectId, id_gen: &mut IdGen) -> &mut Object {
        let entity_id = *self.object_map.get(&obj).unwrap();

        self.ensure_conflated(obj, id_gen);
        unwrap_as!(
            self.entities.get_mut(&entity_id).unwrap(),
            Entity::Object(o),
            o
        )
    }

    /// Invalidates the [`Entity`] that `obj` points to.
    /// This involves performing any outstanding conflations.
    fn invalidate(&mut self, obj: ObjectId, id_gen: &mut IdGen) {
        // println!("invalidating object `{:#?}`", obj);
        self.get_object_mut(obj, id_gen).invalidated = true;
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
