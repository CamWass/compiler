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

    let result = Analysis::new(cfa, node, vars).analyze();

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

    for entity in end.objects.values() {
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
    pub escaped_locals: FxHashSet<Id>,
    pub scopeVariables: FxHashMap<Id, VarId>,
    pub orderedVars: IndexVec<VarId, Id>,
    pub params: FxHashSet<VarId>,
    pub fn_and_class_names: FxHashSet<VarId>,
    pub lattice_elements: IndexVec<LatticeElementId, Lattice>,
}

pub struct Analysis<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    data_flow_analysis: DataFlowAnalysis<Node<'ast>, Inner<'ast, 'a, T>, Lattice, TODOJoinOp>,
}

impl<'ast, 'a, T> Analysis<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    pub fn new(
        cfa: ControlFlowAnalysisResult<Node<'ast>, LinearFlowState, LatticeElementId>,
        fn_scope: &'a T,
        allVarsDeclaredInFunction: AllVarsDeclaredInFunction,
    ) -> Self {
        let inner = Inner {
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
            cfg: cfa.cfg,

            next_TODO_ID: TODOId::from_u32(0),
            next_object_ID: ObjectId::from_u32(0),
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
            escaped_locals: self.data_flow_analysis.inner.escaped,
            scopeVariables: self.data_flow_analysis.inner.scopeVariables,
            orderedVars: self.data_flow_analysis.inner.orderedVars,
            params: self.data_flow_analysis.inner.params,
            fn_and_class_names: self.data_flow_analysis.inner.fn_and_class_names,
            lattice_elements: self.data_flow_analysis.inner.lattice_elements,
        };
        (result, self.data_flow_analysis.inner.cfg)
    }
}

#[derive(Debug)]
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
    scopeVariables: FxHashMap<Id, VarId>,
    // obtain variables in the order in which they appear in the code
    orderedVars: IndexVec<VarId, Id>,
    params: FxHashSet<VarId>,
    fn_and_class_names: FxHashSet<VarId>,

    lattice_elements: IndexVec<LatticeElementId, Lattice>,

    cfg: ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,

    next_TODO_ID: TODOId,
    next_object_ID: ObjectId,
}

impl<'ast, 'a, T> Inner<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
    fn computeGenKill<'b>(&mut self, n: Node<'ast>, lattice: &'b mut Lattice, conditional: bool) {
        let mut v = GenKillComputer {
            analysis: self,
            lattice,
        };

        v.init(n, conditional);
    }
}

index::newtype_index!(struct ObjectId { .. });
index::newtype_index!(struct TODOId { .. });
index::newtype_index!(struct DeclarationId { .. });

#[derive(Debug, Clone, PartialEq)]
struct Property {
    value: Option<ObjectId>,
    references: FxHashSet<NodeId>,
    conditional: bool,
}

#[derive(Debug, Clone, PartialEq)]
enum Entity {
    Conflation(FxHashSet<ObjectId>),
    Object(Object),
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Object {
    // TODO: allow, and recursively conflate, nested props.
    properties: FxHashMap<Id, Property>,
    invalidated: bool,
}

#[derive(Debug, Clone, PartialEq)]
struct Assignment {
    rhs: Option<ObjectId>,
}

enum Slot<'ast> {
    Var(Id),
    Prop(ObjectId, &'ast Ident),
}

struct GenKillComputer<'ast, 'a, 'b, T>
where
    T: FunctionLike<'a>,
{
    analysis: &'b mut Inner<'ast, 'a, T>,

    lattice: &'b mut Lattice,
}

impl<'ast, 'a, 'b, T> GenKillComputer<'ast, 'a, 'b, T>
where
    T: FunctionLike<'a>,
{
    fn ensure_conflated(&mut self, obj: ObjectId) {
        let TODO_ID = self.lattice.object_map.get(&obj).unwrap();

        let entity = self.lattice.objects.get_mut(TODO_ID).unwrap();

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

            let TODO_ID = self.lattice.object_map.get(&constituent).unwrap();
            match self.lattice.objects.remove(TODO_ID).unwrap() {
                Entity::Conflation(objects) => {
                    for obj in objects {
                        if !done.contains(&obj) {
                            queue.push(obj);
                        }
                    }
                }
                Entity::Object(incoming) => {
                    self.get_object_mut(obj).invalidated |= incoming.invalidated;

                    for (key, prop) in incoming.properties {
                        match self.get_object_mut(obj).properties.get(&key) {
                            Some(existing) => {
                                // conflate
                                let existing = existing.value;
                                let value = self.conflate_objects(existing, prop.value);
                                let existing =
                                    self.get_object_mut(obj).properties.get_mut(&key).unwrap();
                                existing.value = value;
                                existing.conditional |= prop.conditional;
                                existing.references.extend(prop.references);
                            }
                            None => {
                                // initialize
                                self.get_object_mut(obj).properties.insert(key, prop);
                            }
                        }
                    }
                }
            }

            done.insert(obj);
        }
    }

    fn get_object_mut(&mut self, obj: ObjectId) -> &mut Object {
        let TODO_ID = *self.lattice.object_map.get(&obj).unwrap();

        self.ensure_conflated(obj);
        unwrap_as!(
            self.lattice.objects.get_mut(&TODO_ID).unwrap(),
            Entity::Object(o),
            o
        )
    }

    fn declare_property(
        &mut self,
        obj: ObjectId,
        prop: &Ident,
        value: Option<ObjectId>,
        conditional: bool,
    ) {
        // println!(
        //     "declaring prop `{:#?}` with value `{:#?}` on object `{:#?}` ",
        //     prop, value, obj
        // );
        let object = self.get_object_mut(obj);
        let prop_id = prop.to_id();

        match object.properties.get(&prop_id) {
            Some(existing) => {
                let value = if !conditional {
                    // supersede
                    value
                } else {
                    // conflate
                    let id1 = existing.value;
                    self.conflate_objects(id1, value)
                };
                let existing = self
                    .get_object_mut(obj)
                    .properties
                    .get_mut(&prop_id)
                    .unwrap();
                existing.value = value;
                existing.conditional = conditional;
                existing.references.insert(prop.node_id);
            }
            None => {
                // initialize
                let mut references = FxHashSet::default();
                references.insert(prop.node_id);
                object.properties.insert(
                    prop_id,
                    Property {
                        value,
                        references,
                        conditional,
                    },
                );
            }
        }
    }

    fn invalidate(&mut self, obj: ObjectId) {
        // println!("invalidating object `{:#?}`", obj);
        self.get_object_mut(obj).invalidated = true;
    }

    fn conflate_objects(
        &mut self,
        id1: Option<ObjectId>,
        id2: Option<ObjectId>,
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
                self.invalidate(id2);
                return Some(id2);
            }
            (Some(id1), None) => {
                self.invalidate(id1);
                return Some(id1);
            }
            (Some(id1), Some(id2)) => (id1, id2),
        };

        let TODO_ID1 = *self.lattice.object_map.get(&id1).unwrap();
        let TODO_ID2 = *self.lattice.object_map.get(&id2).unwrap();

        if TODO_ID1 == TODO_ID2 {
            // Already been conflated.
            todo!("reachable?");
            // return;
        }

        let mut objects = FxHashSet::default();
        objects.insert(id1);
        objects.insert(id2);
        let conflation = Entity::Conflation(objects);

        let new_TODO_ID = self.analysis.next_TODO_ID;
        self.analysis.next_TODO_ID.increment_by(1);
        self.lattice.objects.insert(new_TODO_ID, conflation);

        let next_object_ID = self.analysis.next_object_ID;
        self.analysis.next_object_ID.increment_by(1);
        self.lattice.object_map.insert(next_object_ID, new_TODO_ID);

        Some(next_object_ID)
    }

    fn record_var_assignment(
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
            Some(Slot::Var(lhs)) => {
                if let Some(rhs_object) = rhs {
                    if let Some(existing) = self.lattice.var_assignments.get(&lhs) {
                        let rhs = if !conditional {
                            // supersede
                            Some(rhs_object)
                        } else {
                            // conflate
                            self.conflate_objects(existing.rhs, Some(rhs_object))
                        };
                        let assign = Assignment { rhs };
                        self.lattice.var_assignments.insert(lhs, assign);
                    } else {
                        // initialize
                        let assign = Assignment {
                            rhs: Some(rhs_object),
                        };
                        self.lattice.var_assignments.insert(lhs, assign);
                    }
                    Some(rhs_object)
                } else {
                    // Non-object assignment
                    let assign = Assignment { rhs: None };
                    self.lattice.var_assignments.insert(lhs, assign);
                    None
                }
            }
            Some(Slot::Prop(obj, prop)) => {
                self.declare_property(obj, prop, rhs, conditional);
                Some(obj)
            }
            None => {
                if let Some(rhs) = rhs {
                    self.invalidate(rhs);
                }
                rhs
            }
        }
    }

    fn visit_and_get_slot(&mut self, node: Node<'ast>, conditional: bool) -> Option<Slot<'ast>> {
        match node {
            Node::Ident(node) => {
                let id = node.to_id();
                if self.analysis.scopeVariables.contains_key(&id) {
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
                        self.invalidate(obj);
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
                    PatOrExpr::Expr(n) => true,
                    PatOrExpr::Pat(n) => match n.as_ref() {
                        Pat::Expr(n) => true,
                        Pat::Ident(i) => true,
                        _ => false,
                    },
                };

                let conditional_assign = matches!(
                    node.op,
                    AssignOp::AndAssign | AssignOp::OrAssign | AssignOp::NullishAssign
                );

                if expr_lhs {
                    self.record_var_assignment(
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
                        self.record_var_assignment(
                            lhs,
                            Node::from(rhs.as_ref()),
                            conditional,
                            false,
                        )
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

                    let new_TODO_ID = self.analysis.next_TODO_ID;
                    self.analysis.next_TODO_ID.increment_by(1);
                    self.lattice
                        .objects
                        .insert(new_TODO_ID, Entity::Object(obj));

                    let next_object_ID = self.analysis.next_object_ID;
                    self.analysis.next_object_ID.increment_by(1);
                    self.lattice.object_map.insert(next_object_ID, new_TODO_ID);

                    for prop in &node.props {
                        let prop = unwrap_as!(prop, Prop::KeyValue(p), p);
                        let key = unwrap_as!(&prop.key, PropName::Ident(k), k);
                        let value =
                            self.visit_and_get_object(Node::from(prop.value.as_ref()), conditional);

                        self.declare_property(next_object_ID, key, value, conditional);
                    }

                    Some(next_object_ID)
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
                        self.conflate_objects(left, right)
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
                        self.invalidate(obj);
                    }
                    self.visit_and_get_object(Node::from(node.prop.as_ref()), conditional);
                    None
                } else {
                    if let Some(obj) = obj {
                        let ident = unwrap_as!(node.prop.as_ref(), Expr::Ident(i), i);
                        let id = ident.to_id();
                        if let Some(prop) = self.get_object_mut(obj).properties.get_mut(&id) {
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
                self.conflate_objects(cons, alt)
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

impl<'ast, 'a, T> DataFlowAnalysisInner<Node<'ast>, Lattice, TODOJoinOp> for Inner<'ast, 'a, T>
where
    T: FunctionLike<'a>,
{
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

    fn createFlowJoiner(&self) -> TODOJoinOp {
        TODOJoinOp::new()
    }

    fn flowThrough(&mut self, node: Node<'ast>, input: LatticeElementId) -> LatticeElementId {
        // Make assignments conditional if the node can end abruptly by an exception.
        let conditional = self
            .cfg
            .graph
            .edges(self.cfg.map[&node])
            .any(|e| *e.weight() == Branch::ON_EX);

        let mut new = self.lattice_elements[input].clone();

        self.computeGenKill(node, &mut new, conditional);

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

impl<'a, T> Index<LatticeElementId> for Inner<'_, 'a, T>
where
    T: FunctionLike<'a>,
{
    type Output = Lattice;

    fn index(&self, index: LatticeElementId) -> &Self::Output {
        &self.lattice_elements[index]
    }
}

struct TODOJoinOp {
    result: Lattice,
}

impl TODOJoinOp {
    fn new() -> Self {
        Self {
            result: Lattice::default(),
        }
    }

    fn ensure_conflated(
        &mut self,
        obj: ObjectId,
        next_TODO_ID: &mut TODOId,
        next_object_ID: &mut ObjectId,
    ) {
        let TODO_ID = self.result.object_map.get(&obj).unwrap();

        let entity = self.result.objects.get_mut(TODO_ID).unwrap();

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

            let TODO_ID = self.result.object_map.get(&constituent).unwrap();
            match self.result.objects.remove(TODO_ID).unwrap() {
                Entity::Conflation(objects) => {
                    for obj in objects {
                        if !done.contains(&obj) {
                            queue.push(obj);
                        }
                    }
                }
                Entity::Object(incoming) => {
                    self.get_object_mut(obj, next_TODO_ID, next_object_ID)
                        .invalidated |= incoming.invalidated;

                    for (key, prop) in incoming.properties {
                        match self
                            .get_object_mut(obj, next_TODO_ID, next_object_ID)
                            .properties
                            .get(&key)
                        {
                            Some(existing) => {
                                // conflate
                                let existing = existing.value;
                                let value = self.conflate_objects(
                                    existing,
                                    prop.value,
                                    next_TODO_ID,
                                    next_object_ID,
                                );
                                let existing = self
                                    .get_object_mut(obj, next_TODO_ID, next_object_ID)
                                    .properties
                                    .get_mut(&key)
                                    .unwrap();
                                existing.value = value;
                                existing.conditional |= prop.conditional;
                                existing.references.extend(prop.references);
                            }
                            None => {
                                // initialize
                                self.get_object_mut(obj, next_TODO_ID, next_object_ID)
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

    fn get_object_mut(
        &mut self,
        obj: ObjectId,
        next_TODO_ID: &mut TODOId,
        next_object_ID: &mut ObjectId,
    ) -> &mut Object {
        let TODO_ID = *self.result.object_map.get(&obj).unwrap();

        self.ensure_conflated(obj, next_TODO_ID, next_object_ID);
        unwrap_as!(
            self.result.objects.get_mut(&TODO_ID).unwrap(),
            Entity::Object(o),
            o
        )
    }

    fn invalidate(
        &mut self,
        obj: ObjectId,
        next_TODO_ID: &mut TODOId,
        next_object_ID: &mut ObjectId,
    ) {
        self.get_object_mut(obj, next_TODO_ID, next_object_ID)
            .invalidated = true;
    }

    fn conflate_objects(
        &mut self,
        id1: Option<ObjectId>,
        id2: Option<ObjectId>,
        next_TODO_ID: &mut TODOId,
        next_object_ID: &mut ObjectId,
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
                self.invalidate(id2, next_TODO_ID, next_object_ID);
                return Some(id2);
            }
            (Some(id1), None) => {
                self.invalidate(id1, next_TODO_ID, next_object_ID);
                return Some(id1);
            }
            (Some(id1), Some(id2)) => (id1, id2),
        };

        let TODO_ID1 = *self.result.object_map.get(&id1).unwrap();
        let TODO_ID2 = *self.result.object_map.get(&id2).unwrap();

        if TODO_ID1 == TODO_ID2 {
            // Already been conflated.
            todo!("reachable?");
            // return;
        }

        let mut objects = FxHashSet::default();
        objects.insert(id1);
        objects.insert(id2);
        let conflation = Entity::Conflation(objects);

        let new_TODO_ID = *next_TODO_ID;
        next_TODO_ID.increment_by(1);
        self.result.objects.insert(new_TODO_ID, conflation);

        let mut next_object_ID = *next_object_ID;
        next_object_ID.increment_by(1);
        self.result.object_map.insert(next_object_ID, new_TODO_ID);

        Some(next_object_ID)
    }

    fn record_var_assignment(
        &mut self,
        lhs: &Id,
        incoming: &Assignment,
        next_TODO_ID: &mut TODOId,
        next_object_ID: &mut ObjectId,
    ) {
        // println!("JOIN: record_var_assignment: id: {:?}", lhs);
        if let Some(existing) = self.result.var_assignments.get(lhs) {
            // conflate
            // println!(
            //     "JOIN: record_var_assignment: conflating {:?} and {:?}",
            //     existing.rhs, incoming.rhs
            // );
            let conflation =
                self.conflate_objects(existing.rhs, incoming.rhs, next_TODO_ID, next_object_ID);
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

impl<'ast, 'a, T> FlowJoiner<Lattice, Inner<'ast, 'a, T>> for TODOJoinOp
where
    T: FunctionLike<'a>,
{
    fn joinFlow(&mut self, inner: &mut Inner<'ast, 'a, T>, input: LatticeElementId) {
        let input = &inner.lattice_elements[input];

        // println!("======================JOIN:");
        // dbg!(&self.result);
        // dbg!(input);

        for (obj_id, TODO_ID) in &input.object_map {
            if let Some(existing) = self.result.object_map.get(obj_id) {
                debug_assert_eq!(&input.objects[TODO_ID], &self.result.objects[existing]);
                // self.conflate_TODOS(TODO_ID, existing, &mut inner.next_TODO_ID);
            } else {
                self.result.object_map.insert(*obj_id, *TODO_ID);
            }
        }

        for (&TODO_ID, object) in &input.objects {
            match self.result.objects.entry(TODO_ID) {
                std::collections::hash_map::Entry::Occupied(entry) => {
                    debug_assert_eq!(object, entry.get());
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(object.clone());
                }
            }
        }

        for (lhs, incoming) in &input.var_assignments {
            self.record_var_assignment(
                lhs,
                incoming,
                &mut inner.next_TODO_ID,
                &mut inner.next_object_ID,
            );
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
    objects: FxHashMap<TODOId, Entity>,
    object_map: FxHashMap<ObjectId, TODOId>,
    var_assignments: FxHashMap<Id, Assignment>,
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
