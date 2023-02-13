use std::collections::hash_map::Entry;

use ast::*;
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::Span;
use index::{newtype_index, vec::IndexVec};
use rustc_hash::{FxHashMap, FxHashSet};
use swc_atoms::JsWord;

use crate::find_vars::*;
use crate::utils::unwrap_as;
use crate::DefaultNameGenerator::DefaultNameGenerator;
use crate::{Id, ToId};

pub fn process(ast: &mut ast::Program) {
    let mut visitor = MainVisitor::default();
    ast.visit_with(&mut visitor);

    let mut renamer = Renamer {
        rename_map: visitor.rename_map,
    };
    ast.visit_mut_with(&mut renamer);
}

#[derive(Default)]
struct MainVisitor {
    rename_map: FxHashMap<NodeId, JsWord>,
}

impl MainVisitor {
    fn update_renaming_map(&mut self, types: IndexVec<TypeId, Type>) {
        for ty in types.into_iter().filter(|t| t.valid) {
            let mut name_gen = DefaultNameGenerator::default();
            for prop in ty.property_info {
                let new_name = name_gen.generateNextName();
                for reference in prop.references {
                    self.rename_map.insert(reference.0, new_name.clone());
                }
            }
        }
    }
}

macro_rules! visit_fn {
    ($f:ident, $t:ident) => {
        fn $f(&mut self, node: &ast::$t) {
            let all_vars = find_vars_declared_in_fn(node, false);
            let escaped = computeEscaped(node, &all_vars.scopeVariables, all_vars.catch_vars);
            let params = all_vars.params;
            let valid_vars = all_vars
                .scopeVariables
                .into_iter()
                .filter(|entry| !params.contains(&entry.1) && !escaped.contains(&entry.0))
                .map(|entry| entry.0)
                .collect::<FxHashSet<_>>();

            dbg!(&valid_vars);
            let mut v = InnerVisitor::default();
            node.body.visit_children_with(&mut v);
            dbg!(&v);
            self.update_renaming_map(v.types);

            node.visit_children_with(self);
        }
    };
}

impl Visit<'_> for MainVisitor {
    visit_fn!(visit_function, Function);
    visit_fn!(visit_constructor, Constructor);
    visit_fn!(visit_arrow_expr, ArrowExpr);
    visit_fn!(visit_getter_prop, GetterProp);
    visit_fn!(visit_setter_prop, SetterProp);
}

newtype_index!(struct PropertyId { .. });

#[derive(Default, Debug)]
struct Property {
    references: FxHashSet<(NodeId, Span)>,
}

#[derive(Debug)]
struct Type {
    property_info: IndexVec<PropertyId, Property>,
    properties: FxHashMap<Id, PropertyId>,
    valid: bool,
    invalidated_by: Option<(NodeId, Span)>,
}

impl Type {
    fn new() -> Self {
        Self {
            property_info: Default::default(),
            properties: Default::default(),
            valid: true,
            invalidated_by: None,
        }
    }

    fn declare_property(&mut self, name: &Ident) {
        if !self.valid {
            return;
        }
        let id = name.to_id();
        match self.properties.entry(id) {
            Entry::Occupied(slot) => {
                let prop = slot.get();
                self.property_info[*prop]
                    .references
                    .insert((name.node_id, name.span));
            }
            Entry::Vacant(slot) => {
                let mut prop_info = Property::default();
                prop_info.references.insert((name.node_id, name.span));
                let prop = self.property_info.push(prop_info);
                slot.insert(prop);
            }
        }
    }

    fn reference_property(&mut self, name: &Ident) {
        if !self.valid {
            return;
        }
        let id = name.to_id();
        if let Some(prop) = self.properties.get(&id) {
            self.property_info[*prop]
                .references
                .insert((name.node_id, name.span));
        }
    }
}

newtype_index!(struct TypeId { .. });

#[derive(Default, Debug)]
struct InnerVisitor {
    types: IndexVec<TypeId, Type>,
    vars: FxHashMap<Id, TypeId>,
}

impl InnerVisitor {
    fn is_valid_type(&self, ty: TypeId) -> bool {
        self.types[ty].valid
    }

    fn invalidate_var(&mut self, name: &Ident) {
        let id = name.to_id();
        if let Some(ty) = self.vars.get(&id) {
            let ty = &mut self.types[*ty];
            if ty.valid {
                ty.valid = false;
                ty.invalidated_by = Some((name.node_id, name.span));
            }
        }
    }

    /// Populates the given set with all of the types that could be the result of the given expression.
    ///
    /// Examples:
    /// - `++x` has zero resulting types (because it does not return a reference to `x`).
    /// - `x` has one type, that of `x`.
    /// - `x || y` has two possible types: `x`'s type, and `y`'s type (we don't yet narrow it down to just `y`).
    fn extract_vars(&self, expr: &Expr, vars: &mut FxHashSet<&Ident>) {
        match expr {
            Expr::This(_) => todo!(),
            Expr::Array(_) => todo!(),
            Expr::Object(_) => todo!(),
            Expr::Fn(_) => todo!(),
            Expr::Unary(_) => todo!(),
            Expr::Update(_) => todo!(),
            Expr::Bin(_) => todo!(),
            Expr::Assign(_) => todo!(),
            Expr::Member(_) => todo!(),
            Expr::Cond(_) => todo!(),
            Expr::Call(_) => todo!(),
            Expr::New(_) => todo!(),
            Expr::Seq(_) => todo!(),
            Expr::Ident(_) => todo!(),
            Expr::Lit(_) => todo!(),
            Expr::Tpl(_) => todo!(),
            Expr::TaggedTpl(_) => todo!(),
            Expr::Arrow(_) => todo!(),
            Expr::Class(_) => todo!(),
            Expr::Yield(_) => todo!(),
            Expr::MetaProp(_) => todo!(),
            Expr::Await(_) => todo!(),
            Expr::Paren(_) => todo!(),
            Expr::JSXMember(_) => todo!(),
            Expr::JSXNamespacedName(_) => todo!(),
            Expr::JSXEmpty(_) => todo!(),
            Expr::JSXElement(_) => todo!(),
            Expr::JSXFragment(_) => todo!(),
            Expr::PrivateName(_) => todo!(),
            Expr::OptChain(_) => todo!(),

            Expr::Invalid(_)
            | Expr::TsTypeAssertion(_)
            | Expr::TsConstAssertion(_)
            | Expr::TsNonNull(_)
            | Expr::TsAs(_) => unreachable!(),
        }
    }

    /// Invalidates any vars that `expr` could evaluate to. Also visits `expr`.
    fn invalidate_if_var(&mut self, expr: &Expr) {
        match expr {
            Expr::Ident(n) => self.invalidate_var(n),

            Expr::This(n) => n.visit_with(self),
            Expr::Array(n) => n.visit_with(self),
            Expr::Object(n) => n.visit_with(self),
            Expr::Fn(n) => n.visit_with(self),
            Expr::Unary(n) => n.visit_with(self),
            Expr::Update(n) => n.visit_with(self),
            Expr::Arrow(n) => n.visit_with(self),
            Expr::Tpl(n) => n.visit_with(self),
            Expr::TaggedTpl(n) => n.visit_with(self),
            Expr::Class(n) => n.visit_with(self),
            Expr::Member(n) => n.visit_with(self),

            // Terminal nodes; no need to visit.
            Expr::MetaProp(_) | Expr::Lit(_) => {}

            Expr::Paren(n) => {
                let mut vars = FxHashSet::default();
                self.extract_vars(&n.expr, &mut vars);
                for var in vars {
                    self.invalidate_var(var);
                }
                n.visit_with(self);
            }
            Expr::Bin(n) => match n.op {
                BinaryOp::EqEq
                | BinaryOp::NotEq
                | BinaryOp::EqEqEq
                | BinaryOp::NotEqEq
                | BinaryOp::Lt
                | BinaryOp::LtEq
                | BinaryOp::Gt
                | BinaryOp::GtEq
                | BinaryOp::LShift
                | BinaryOp::RShift
                | BinaryOp::ZeroFillRShift
                | BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod
                | BinaryOp::BitOr
                | BinaryOp::BitXor
                | BinaryOp::BitAnd
                | BinaryOp::Exp => {
                    n.visit_with(self);
                }

                BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::NullishCoalescing => {
                    let mut vars = FxHashSet::default();
                    self.extract_vars(&n.left, &mut vars);
                    self.extract_vars(&n.right, &mut vars);
                    for var in vars {
                        self.invalidate_var(var);
                    }
                    n.visit_with(self);
                }

                BinaryOp::In => todo!(),
                BinaryOp::InstanceOf => todo!(),
            },
            Expr::Cond(n) => {
                let mut vars = FxHashSet::default();
                self.extract_vars(&n.cons, &mut vars);
                self.extract_vars(&n.alt, &mut vars);
                for var in vars {
                    self.invalidate_var(var);
                }
                n.visit_with(self);
            }
            Expr::Seq(n) => {
                if let Some(last) = n.exprs.last() {
                    let mut vars = FxHashSet::default();
                    self.extract_vars(last, &mut vars);
                    for var in vars {
                        self.invalidate_var(var);
                    }
                }
                n.visit_with(self);
            }

            Expr::Assign(_) => todo!(),
            Expr::Call(_) => todo!(),
            Expr::New(_) => todo!(),
            Expr::Yield(_) => todo!(),
            Expr::Await(_) => todo!(),
            Expr::JSXMember(_) => todo!(),
            Expr::JSXNamespacedName(_) => todo!(),
            Expr::JSXEmpty(_) => todo!(),
            Expr::JSXElement(_) => todo!(),
            Expr::JSXFragment(_) => todo!(),
            Expr::PrivateName(_) => todo!(),
            Expr::OptChain(_) => todo!(),

            Expr::Invalid(_)
            | Expr::TsTypeAssertion(_)
            | Expr::TsConstAssertion(_)
            | Expr::TsNonNull(_)
            | Expr::TsAs(_) => unreachable!(),
        }
    }
}

impl Visit<'_> for InnerVisitor {
    // Don't visit nested functions.
    fn visit_function(&mut self, _node: &Function) {}
    fn visit_constructor(&mut self, _node: &Constructor) {}
    fn visit_arrow_expr(&mut self, _node: &ArrowExpr) {}
    fn visit_getter_prop(&mut self, _node: &GetterProp) {}
    fn visit_setter_prop(&mut self, _node: &SetterProp) {}

    fn visit_member_expr(&mut self, node: &MemberExpr) {
        if let ExprOrSuper::Expr(obj) = &node.obj {
            if let Expr::Ident(obj) = obj.as_ref() {
                if !node.computed {
                    let prop = unwrap_as!(node.prop.as_ref(), Expr::Ident(p), p);
                    let var_id = obj.to_id();
                    if let Some(ty) = self.vars.get(&var_id) {
                        if self.is_valid_type(*ty) {
                            self.types[*ty].reference_property(prop);
                            return;
                        }
                    }
                } else {
                    self.invalidate_if_var(node.prop.as_ref());
                    return;
                }
            }
        }

        node.visit_children_with(self);
    }

    fn visit_assign_expr(&mut self, node: &AssignExpr) {
        if let PatOrExpr::Expr(lhs) = &node.left {
            if let Expr::Ident(lhs) = lhs.as_ref() {
                self.invalidate_var(lhs);
                node.right.visit_with(self);
                return;
            }
        }

        // AddAssign is the only op that is not guaranteed to declare a prop.
        if node.op == AssignOp::AddAssign {
            todo!();
            // println!("Not simple assignment expr");
            // node.visit_children_with(self);
            // return;
        }

        let lhs_expr = match &node.left {
            PatOrExpr::Expr(n) => Some(n.as_ref()),
            PatOrExpr::Pat(n) => match n.as_ref() {
                // TODO: why does this exist? All it seems to do is cause me confusion.
                Pat::Expr(n) => Some(n.as_ref()),
                _ => None,
            },
        };
        if let Some(Expr::Member(lhs)) = lhs_expr {
            if let ExprOrSuper::Expr(obj) = &lhs.obj {
                if let Expr::Ident(obj) = obj.as_ref() {
                    if !lhs.computed {
                        let prop = unwrap_as!(lhs.prop.as_ref(), Expr::Ident(p), p);
                        let var_id = obj.to_id();
                        if let Some(ty) = self.vars.get(&var_id) {
                            if self.is_valid_type(*ty) {
                                if is_simple_rhs(&node.right) {
                                    self.types[*ty].declare_property(prop);
                                    // Still need to visit RHS in case it invalidates any variables.
                                    node.right.visit_with(self);
                                    return;
                                }
                            }
                        }
                    }
                }
            }
        }

        node.left.visit_with(self);
        self.invalidate_if_var(&node.right);
    }

    fn visit_var_decl(&mut self, node: &VarDecl) {
        if node.decls.len() != 1 {
            unreachable!("Normalization should have prevented this");
        }

        let decl = node.decls.first().unwrap();

        if let Pat::Ident(lhs) = &decl.name {
            if let Some(rhs) = &decl.init {
                if let Expr::Object(rhs) = rhs.as_ref() {
                    let rhs_is_simple_obj_lit = rhs.props.iter().all(|p| match p {
                        Prop::KeyValue(p) => !matches!(&p.key, PropName::Computed(_)),
                        _ => false,
                    });

                    if rhs_is_simple_obj_lit {
                        let id = lhs.to_id();
                        if self.vars.contains_key(&id) {
                            todo!("Redeclaration of block-scoped variable");
                        }

                        let mut ty = Type::new();
                        for prop in &rhs.props {
                            let prop = unwrap_as!(&prop, Prop::KeyValue(p), p);
                            match &prop.key {
                                PropName::Ident(i) => ty.declare_property(i),
                                PropName::Str(_) | PropName::Num(_) => todo!(),
                                PropName::Computed(_) => unreachable!(),
                            }
                        }
                        let ty = self.types.push(ty);
                        self.vars.insert(id, ty);
                        return;
                    }
                }
            }
        }

        decl.visit_children_with(self);
    }

    fn visit_extends_clause(&mut self, node: &ExtendsClause) {
        self.invalidate_if_var(&node.super_class);
    }

    fn visit_class_prop(&mut self, node: &ClassProp) {
        node.key.visit_with(self);
        if let Some(value) = &node.value {
            self.invalidate_if_var(value);
        }
        node.decorators.visit_with(self);
    }

    fn visit_private_prop(&mut self, node: &PrivateProp) {
        node.key.visit_with(self);
        if let Some(value) = &node.value {
            self.invalidate_if_var(value);
        }
        node.decorators.visit_with(self);
    }

    fn visit_decorator(&mut self, node: &Decorator) {
        todo!();
    }

    fn visit_var_declarator(&mut self, node: &VarDeclarator) {
        unreachable!("handled by VarDecl");
    }
    fn visit_spread_element(&mut self, node: &SpreadElement) {
        self.invalidate_if_var(&node.expr);
    }
    fn visit_bin_expr(&mut self, node: &BinExpr) {
        todo!();
    }
    fn visit_yield_expr(&mut self, node: &YieldExpr) {
        // TODO: may be unnecessary:
        if let Some(arg) = &node.arg {
            self.invalidate_if_var(arg);
        }
    }
    fn visit_await_expr(&mut self, node: &AwaitExpr) {
        // TODO: may be unnecessary:
        self.invalidate_if_var(&node.arg);
    }
    fn visit_expr_or_spread(&mut self, node: &ExprOrSpread) {
        match node {
            ExprOrSpread::Expr(n) => self.invalidate_if_var(n),
            ExprOrSpread::Spread(n) => n.visit_with(self),
        }
    }
    fn visit_block_stmt_or_expr(&mut self, node: &BlockStmtOrExpr) {
        todo!();
    }
    fn visit_opt_chain_expr(&mut self, node: &OptChainExpr) {
        todo!();
    }
    fn visit_jsx_expr(&mut self, node: &JSXExpr) {
        todo!();
    }
    fn visit_jsx_spread_child(&mut self, node: &JSXSpreadChild) {
        todo!();
    }
    fn visit_export_default_expr(&mut self, node: &ExportDefaultExpr) {
        todo!();
    }
    fn visit_binding_ident(&mut self, node: &BindingIdent) {
        self.invalidate_var(&node.id);
    }
    fn visit_prop(&mut self, node: &Prop) {
        // Should have been removed by normalization.
        debug_assert!(!matches!(node, Prop::Shorthand(_)));
        node.visit_children_with(self);
    }
    fn visit_assign_pat_prop(&mut self, node: &AssignPatProp) {
        self.invalidate_var(&node.key);
        node.value.visit_with(self);
    }
    fn visit_key_value_prop(&mut self, node: &KeyValueProp) {
        node.key.visit_with(self);
        self.invalidate_if_var(&node.value);
    }
    fn visit_assign_prop(&mut self, node: &AssignProp) {
        todo!();
    }
    fn visit_spread_assignment(&mut self, node: &SpreadAssignment) {
        self.invalidate_if_var(&node.expr);
    }
    fn visit_with_stmt(&mut self, node: &WithStmt) {
        todo!();
    }
    fn visit_return_stmt(&mut self, node: &ReturnStmt) {
        if let Some(arg) = &node.arg {
            self.invalidate_if_var(arg);
        }
    }
    fn visit_throw_stmt(&mut self, node: &ThrowStmt) {
        todo!();
    }
    fn visit_for_in_stmt(&mut self, node: &ForInStmt) {
        self.invalidate_if_var(&node.right);
        node.left.visit_with(self);
        node.body.visit_with(self);
    }
    fn visit_for_of_stmt(&mut self, node: &ForOfStmt) {
        self.invalidate_if_var(&node.right);
        node.left.visit_with(self);
        node.body.visit_with(self);
    }
    fn visit_var_decl_or_expr(&mut self, node: &VarDeclOrExpr) {
        todo!();
    }
    fn visit_ts_export_assignment(&mut self, node: &TsExportAssignment) {
        todo!();
    }
    fn visit_jsx_object(&mut self, node: &JSXObject) {
        todo!();
    }
    fn visit_jsx_namespaced_name(&mut self, node: &JSXNamespacedName) {
        todo!();
    }
    fn visit_jsx_element_name(&mut self, node: &JSXElementName) {
        todo!();
    }
    fn visit_jsx_attr_name(&mut self, node: &JSXAttrName) {
        todo!();
    }
    fn visit_import_default_specifier(&mut self, node: &ImportDefaultSpecifier) {
        todo!();
    }
    fn visit_import_star_as_specifier(&mut self, node: &ImportStarAsSpecifier) {
        todo!();
    }
    fn visit_import_named_specifier(&mut self, node: &ImportNamedSpecifier) {
        todo!();
    }
    fn visit_export_namespace_specifier(&mut self, node: &ExportNamespaceSpecifier) {
        todo!();
    }
    fn visit_export_default_specifier(&mut self, node: &ExportDefaultSpecifier) {
        todo!();
    }
    fn visit_export_named_specifier(&mut self, node: &ExportNamedSpecifier) {
        todo!();
    }
}

fn is_simple_rhs(expr: &Expr) -> bool {
    match expr {
        Expr::Lit(n) => match n {
            Lit::Str(_)
            | Lit::Bool(_)
            | Lit::Null(_)
            | Lit::Num(_)
            | Lit::BigInt(_)
            | Lit::Regex(_) => true,
            Lit::JSXText(_) => todo!(),
        },

        // TODO:
        Expr::PrivateName(_) | Expr::Ident(_) => false,

        Expr::Paren(n) => is_simple_rhs(&n.expr),
        Expr::TsNonNull(n) => is_simple_rhs(&n.expr),
        Expr::TsAs(n) => is_simple_rhs(&n.expr),

        Expr::This(_)
        | Expr::Array(_)
        | Expr::Object(_)
        | Expr::Fn(_)
        | Expr::Unary(_)
        | Expr::Update(_)
        | Expr::Bin(_)
        | Expr::Assign(_)
        | Expr::Member(_)
        | Expr::Cond(_)
        | Expr::Call(_)
        | Expr::New(_)
        | Expr::Seq(_)
        | Expr::Tpl(_)
        | Expr::TaggedTpl(_)
        | Expr::Arrow(_)
        | Expr::Class(_)
        | Expr::Yield(_)
        | Expr::MetaProp(_)
        | Expr::Await(_)
        | Expr::JSXMember(_)
        | Expr::JSXNamespacedName(_)
        | Expr::JSXEmpty(_)
        | Expr::JSXElement(_)
        | Expr::JSXFragment(_)
        | Expr::TsTypeAssertion(_)
        | Expr::TsConstAssertion(_)
        | Expr::OptChain(_)
        | Expr::Invalid(_) => false,
    }
}

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
