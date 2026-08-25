use std::collections::hash_map::Entry;

use ast::*;
use index::vec::IndexVec;
use rustc_hash::{FxHashMap, FxHashSet};
use visit::{Visit, VisitMut, VisitMutWith, VisitWith};

// TODO: everything in this file would probably be mre clear if written with
// walk functions rather than visitors.

#[derive(Default, Debug)]
pub struct AllVarsDeclaredInFunction {
    pub scope_variables: FxHashMap<NameId, VarId>,
    pub ordered_vars: IndexVec<VarId, NameId>,
    /// All variables bound in catch params e.g. `x` in `try {} catch(x) {}`
    pub catch_vars: FxHashSet<NameId>,
    // TODO: bitset?
    pub params: FxHashSet<VarId>,
    // TODO: bitset?
    pub fn_and_class_names: FxHashSet<VarId>,
}

/// `skip_multi_decl_destructuring` - Whether to skip collecting bindings from
/// var decls with destructuring patterns that declare multiple names. e.g.
/// ```js
/// const a = o; // Only one binding
/// const {b} = o; // Only one binding
/// const {c, d} = o; // Multiple bindings; optionally skipped
/// const e = 1, f = 2; // Multiple bindings; optionally skipped
/// ```
pub fn find_vars_declared_in_fn<'ast, T>(
    function: &'ast T,
    skip_multi_decl_destructuring: bool,
) -> AllVarsDeclaredInFunction
where
    T: FunctionLike + VisitWith<'ast, DeclFinder>,
{
    let mut v = DeclFinder {
        vars: AllVarsDeclaredInFunction::default(),
        in_catch_binding: false,
        in_param: false,
        skip_multi_decl_destructuring,
    };
    function.visit_children_with(&mut v);
    v.vars
}

pub struct DeclFinder {
    vars: AllVarsDeclaredInFunction,
    in_catch_binding: bool,
    in_param: bool,
    skip_multi_decl_destructuring: bool,
}

impl DeclFinder {
    fn record_var(&mut self, id: NameId) {
        let var_id = match self.vars.scope_variables.entry(id) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let var_id = self.vars.ordered_vars.push(id);
                entry.insert(var_id);
                var_id
            }
        };
        if self.in_catch_binding {
            self.vars.catch_vars.insert(id);
        }
        if self.in_param {
            self.vars.params.insert(var_id);
        }
    }

    fn record_decl_name(&mut self, name: NameId) {
        if let Entry::Vacant(entry) = self.vars.scope_variables.entry(name) {
            let var_id = self.vars.ordered_vars.push(name);
            entry.insert(var_id);
            self.vars.fn_and_class_names.insert(var_id);
        }
    }
}

/*
TODO: does the current impl incorrectly collect properties?
e.g.
function f() {
    class A {
        prop = 1;
    }

    let a = {
        prop: 1
    };
}
"prop"s should not be collected
*/
impl Visit<'_> for DeclFinder {
    // Don't visit nested functions.
    fn visit_function(&mut self, _node: &Function) {}
    fn visit_constructor(&mut self, _node: &Constructor) {}
    fn visit_arrow_expr(&mut self, _node: &ArrowExpr) {}
    fn visit_getter_prop(&mut self, _node: &GetterProp) {}
    fn visit_setter_prop(&mut self, _node: &SetterProp) {}

    // Function names are in scope.
    fn visit_fn_decl(&mut self, node: &FnDecl) {
        self.record_decl_name(node.ident.name);
    }

    fn visit_class_decl(&mut self, node: &ClassDecl) {
        self.record_decl_name(node.ident.name);
    }

    fn visit_catch_clause(&mut self, n: &CatchClause) {
        self.in_catch_binding = true;
        n.param.visit_with(self);
        self.in_catch_binding = false;
        n.body.visit_with(self);
    }

    // Expressions can't declare new vars.
    fn visit_expr(&mut self, _node: &Expr) {}
    fn visit_prop_name(&mut self, _node: &PropName) {}

    fn visit_param(&mut self, e: &Param) {
        self.in_param = true;
        e.pat.visit_with(self);
        self.in_param = false;
    }

    fn visit_binding_ident(&mut self, node: &BindingIdent) {
        self.record_var(node.id.name);
    }

    fn visit_var_decl(&mut self, node: &VarDecl) {
        if self.skip_multi_decl_destructuring {
            if node.decls.len() == 1 {
                node.decls.first().unwrap().visit_with(self);
            }
        } else {
            node.decls.visit_with(self);
        }
    }

    fn visit_var_declarator(&mut self, v: &VarDeclarator) {
        let names = find_pat_ids(&v.name);
        if self.skip_multi_decl_destructuring {
            if names.len() == 1 {
                for name in names {
                    self.record_var(name.0);
                }
            }
        } else {
            for name in names {
                self.record_var(name.0);
            }
        }
    }
}

/// Finds all **binding** idents of variables.
struct DestructuringFinder<'a> {
    found: &'a mut Vec<(NameId, NodeId)>,
}

/// Finds all **binding** idents of `node`.
pub fn find_pat_ids(node: &Pat) -> Vec<(NameId, NodeId)> {
    let mut found = vec![];

    {
        let mut v = DestructuringFinder { found: &mut found };
        node.visit_with(&mut v);
    }

    found
}

impl Visit<'_> for DestructuringFinder<'_> {
    /// No-op (we don't care about expressions)
    fn visit_expr(&mut self, _: &Expr) {}
    fn visit_prop_name(&mut self, _: &PropName) {}

    fn visit_binding_ident(&mut self, i: &BindingIdent) {
        self.found.push((i.id.name, i.id.node_id));
    }
}

/// Finds the first LHS ident in a pattern.
struct FindFirstLHSIdent<'ast> {
    found: Option<&'ast mut Ident>,
}

/// Finds the first LHS ident in a pattern.
pub fn find_first_lhs_ident(node: &mut Pat) -> Option<&mut Ident> {
    let mut v = FindFirstLHSIdent { found: None };
    node.visit_mut_with(&mut v);

    v.found
}

impl<'ast> VisitMut<'ast> for FindFirstLHSIdent<'ast> {
    /// No-op (we don't care about expressions)
    fn visit_mut_expr(&mut self, _: &'ast mut Expr) {}
    fn visit_mut_prop_name(&mut self, _: &'ast mut PropName) {}

    fn visit_mut_pat(&mut self, node: &'ast mut Pat) {
        if self.found.is_none() {
            node.visit_mut_children_with(self);
        }
    }

    fn visit_mut_binding_ident(&mut self, i: &'ast mut BindingIdent) {
        if self.found.is_none() {
            self.found = Some(&mut i.id);
        }
    }
}

pub trait FunctionLike {
    fn params(&self) -> &[Param];
    fn params_mut(&mut self) -> &mut [Param];

    fn body(&self) -> &BlockStmt;
    fn body_mut(&mut self) -> &mut BlockStmt;
}

impl FunctionLike for Function {
    fn params(&self) -> &[Param] {
        &self.params
    }

    fn params_mut(&mut self) -> &mut [Param] {
        &mut self.params
    }

    fn body(&self) -> &BlockStmt {
        &self.body
    }

    fn body_mut(&mut self) -> &mut BlockStmt {
        &mut self.body
    }
}
impl FunctionLike for Constructor {
    fn params(&self) -> &[Param] {
        &self.params
    }

    fn params_mut(&mut self) -> &mut [Param] {
        &mut self.params
    }

    fn body(&self) -> &BlockStmt {
        &self.body
    }

    fn body_mut(&mut self) -> &mut BlockStmt {
        &mut self.body
    }
}
impl FunctionLike for ArrowExpr {
    fn params(&self) -> &[Param] {
        &self.params
    }

    fn params_mut(&mut self) -> &mut [Param] {
        &mut self.params
    }

    fn body(&self) -> &BlockStmt {
        &self.body
    }

    fn body_mut(&mut self) -> &mut BlockStmt {
        &mut self.body
    }
}
impl FunctionLike for GetterProp {
    fn params(&self) -> &[Param] {
        &[]
    }

    fn params_mut(&mut self) -> &mut [Param] {
        &mut []
    }

    fn body(&self) -> &BlockStmt {
        &self.body
    }

    fn body_mut(&mut self) -> &mut BlockStmt {
        &mut self.body
    }
}
impl FunctionLike for SetterProp {
    fn params(&self) -> &[Param] {
        std::slice::from_ref(&self.param)
    }

    fn params_mut(&mut self) -> &mut [Param] {
        std::slice::from_mut(&mut self.param)
    }

    fn body(&self) -> &BlockStmt {
        &self.body
    }

    fn body_mut(&mut self) -> &mut BlockStmt {
        &mut self.body
    }
}

index::newtype_index!(pub VarId);

/*

TODO: test for DeclFinder

const {a} = {};

*/
