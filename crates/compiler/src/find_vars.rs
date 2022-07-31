use std::hash::BuildHasherDefault;

use ast::*;
use ecma_visit::{noop_visit_type, Visit, VisitMut, VisitMutWith, VisitWith};
use indexmap::IndexSet;
use rustc_hash::{FxHashSet, FxHasher};

use crate::{Id, ToId};

pub type FxIndexSet<T> = IndexSet<T, BuildHasherDefault<FxHasher>>;

#[derive(Default)]
pub struct AllVarsDeclaredInFunction {
    pub ordered_vars: FxIndexSet<Id>,
    /// All variables bound in catch params e.g. `x` in `try {} catch(x) {}`
    pub catch_vars: FxHashSet<Id>,
}

pub fn find_vars_declared_in_fn<'ast, T>(function: &'ast T) -> AllVarsDeclaredInFunction
where
    T: FunctionLike<'ast> + VisitWith<'ast, DeclFinder>,
{
    let mut v = DeclFinder::default();
    function.visit_children_with(&mut v);
    v.vars
}

#[derive(Default)]
pub struct DeclFinder {
    vars: AllVarsDeclaredInFunction,
    in_catch_binding: bool,
}

impl DeclFinder {
    fn record_var(&mut self, id: Id) {
        self.vars.ordered_vars.insert(id.clone());
        if self.in_catch_binding {
            self.vars.catch_vars.insert(id);
        }
    }
}

impl<'ast> Visit<'ast> for DeclFinder {
    noop_visit_type!();

    // Don't visit nested functions.
    fn visit_function(&mut self, node: &Function) {}
    fn visit_constructor(&mut self, node: &Constructor) {}
    fn visit_arrow_expr(&mut self, node: &ArrowExpr) {}
    fn visit_getter_prop(&mut self, node: &GetterProp) {}
    fn visit_setter_prop(&mut self, node: &SetterProp) {}

    // Function names are in scope.
    fn visit_fn_decl(&mut self, node: &FnDecl) {
        self.record_var(node.ident.to_id());
    }

    fn visit_catch_clause(&mut self, n: &CatchClause) {
        let old = self.in_catch_binding;
        self.in_catch_binding = true;
        n.param.visit_with(self);

        self.in_catch_binding = old;
        n.body.visit_with(self);
    }

    fn visit_expr(&mut self, e: &Expr) {}

    fn visit_param(&mut self, e: &Param) {
        e.pat.visit_with(self);
    }

    fn visit_param_without_decorators(&mut self, e: &ParamWithoutDecorators) {
        e.pat.visit_with(self);
    }

    fn visit_binding_ident(&mut self, node: &BindingIdent) {
        self.record_var(node.to_id());
    }

    fn visit_assign_pat_prop(&mut self, p: &AssignPatProp) {
        self.record_var(p.key.to_id());
    }

    fn visit_var_declarator(&mut self, v: &VarDeclarator) {
        v.name.visit_with(self);
    }
}

pub trait FunctionLike<'a> {
    type ParamIter: Iterator<Item = &'a Pat>;

    fn visit_body_with<'ast, V>(&'ast self, visitor: &mut V)
    where
        V: Visit<'ast>;

    fn visit_mut_body_with<'ast, V>(&'ast mut self, visitor: &mut V)
    where
        V: VisitMut<'ast>;

    fn params(&'a self) -> Self::ParamIter;
}

macro_rules! visit_body {
    () => {
        fn visit_body_with<'ast, V>(&'ast self, visitor: &mut V)
        where
            V: Visit<'ast>,
        {
            self.body.visit_with(visitor);
        }
        fn visit_mut_body_with<'ast, V>(&'ast mut self, visitor: &mut V)
        where
            V: VisitMut<'ast>,
        {
            self.body.visit_mut_with(visitor);
        }
    };
}

fn get_pat_of_param<'a>(param: &'a Param) -> &'a Pat {
    &param.pat
}
impl<'a> FunctionLike<'a> for Function {
    type ParamIter = std::iter::Map<std::slice::Iter<'a, Param>, fn(&'a Param) -> &'a Pat>;

    visit_body!();

    fn params(&'a self) -> Self::ParamIter {
        self.params.iter().map(get_pat_of_param)
    }
}
fn get_pat_of_param_or_ts_param_prop<'a>(param: &'a ParamOrTsParamProp) -> &'a Pat {
    match param {
        ParamOrTsParamProp::Param(p) => &p.pat,
        ParamOrTsParamProp::TsParamProp(_) => unreachable!("removed by earlier pass"),
    }
}
impl<'a> FunctionLike<'a> for Constructor {
    type ParamIter = std::iter::Map<
        std::slice::Iter<'a, ParamOrTsParamProp>,
        fn(&'a ParamOrTsParamProp) -> &'a Pat,
    >;

    visit_body!();

    fn params(&'a self) -> Self::ParamIter {
        self.params.iter().map(get_pat_of_param_or_ts_param_prop)
    }
}
fn get_pat_of_param_without_decorators<'a>(param: &'a ParamWithoutDecorators) -> &'a Pat {
    &param.pat
}
impl<'a> FunctionLike<'a> for ArrowExpr {
    type ParamIter = std::iter::Map<
        std::slice::Iter<'a, ParamWithoutDecorators>,
        fn(&'a ParamWithoutDecorators) -> &'a Pat,
    >;

    visit_body!();

    fn params(&'a self) -> Self::ParamIter {
        self.params.iter().map(get_pat_of_param_without_decorators)
    }
}
impl<'a> FunctionLike<'a> for GetterProp {
    type ParamIter = std::iter::Empty<&'a Pat>;

    visit_body!();

    fn params(&'a self) -> Self::ParamIter {
        std::iter::empty()
    }
}
impl<'a> FunctionLike<'a> for SetterProp {
    type ParamIter = std::iter::Once<&'a Pat>;

    visit_body!();

    fn params(&'a self) -> Self::ParamIter {
        std::iter::once(&self.param.pat)
    }
}

/*

TODO: test for DeclFinder

const {a} = {};

*/
