use crate::Id;
use ast::*;
use atoms::JsWord;
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::{Mark, SyntaxContext};
use rustc_hash::FxHashSet;
// use tracing::{debug, span, Level};

use scope::{IdentType, ScopeKind};

#[cfg(test)]
mod tests;

mod scope;

const LOG: bool = false && cfg!(debug_assertions);

// TODO: ParamWithoutDecorators and other differences from SWC's AST.

/// # When to run
///
/// The resolver expects 'clean' ast. You can get clean ast by parsing, or by
/// removing all syntax context in ast nodes.
///
/// # What does it do
///
/// Firstly all scopes (fn, block) has it's own SyntaxContext.
/// Resolver visits all identifiers in module, and look for binding identifies
/// in the scope. Those identifiers now have the SyntaxContext of scope (fn,
/// block). While doing so, resolver tries to resolve normal identifiers (no
/// hygiene info) as a reference to identifier of scope. If the resolver find
/// suitable variable, the identifier reference will have same context as the
/// variable.
///
///
/// # Panics
///
/// `top_level_mark` should not be root.
///
/// # Example
///
/// ```js
/// let a = 1;
/// {
///     let a = 2;
///     use(a);
/// }
/// use(a)
/// ```
///
/// resolver does
///
/// 1.  Define `a` with top level context.
///
/// 2.  Found a block, so visit block with a new syntax context.
///
/// 3. Defined `a` with syntax context of the block statement.
////
/// 4. Found usage of `a`, and determines that it's reference to `a` in the
/// block. So the reference to `a` will have same syntax context as `a` in the
/// block.
///
/// 5. Found usage of `a` (last line), and determines that it's a
/// reference to top-level `a`, and change syntax context of `a` on last line to
/// top-level syntax context.
///
///
/// # Parameters
///
/// ## `unresolved_mark`
///
/// [Mark] applied to unresolved references.
///
/// A pass should accept this [Mark] if it's going to generate a refernce to
/// globals like `require`.
///
/// e.g. `common_js` pass generates calls to `require`, and this should not
/// be shadowed by a declaration named `require` in the same file.
/// So it uses this value.
///
/// ## `top_level_mark`
///
/// [Mark] applied to top-level bindings.
///
/// **NOTE**: This is **not** globals. This is for top level items declared by
/// users.
///
/// A pass should accept this [Mark] if it requires user-defined top-level
/// items.
///
/// e.g. `jsx` pass requires to call `React` imported by the user.
///
/// ```js
/// import React from 'react';
/// ```
///
/// In the code above, `React` has this [Mark]. `jsx` passes need to
/// reference this [Mark], so it accpets this.
///
/// This [Mark] should be used for referencing top-level bindings written by
/// user. If you are going to create a binding, use `private_ident`
/// instead.
///
/// In other words, **this [Mark] should not be used for determining if a
/// variable is top-level.** This is simply a configuration of the `resolver`
/// pass.
///
/// # FAQ
///
/// ## Does a pair `(JsWord, SyntaxContext)` always uniquely identifiers a
/// variable binding?
///
/// Yes, but multiple variables can have the exactly same name.
///
/// In the code below,
///
/// ```js
/// var a = 1, a = 2;
/// ```
///
/// both of them have the same name, so the `(JsWord, SyntaxContext)` pair will
/// be also identical.
pub fn resolver<'ast>(unresolved_mark: Mark, top_level_mark: Mark) -> impl VisitMut<'ast> {
    assert_ne!(
        unresolved_mark,
        Mark::root(),
        "Marker provided to resolver should not be the root mark"
    );

    Resolver {
        current: Scope::new(ScopeKind::Fn, top_level_mark, None),
        ident_type: IdentType::Ref,
        config: InnerConfig { unresolved_mark },
    }
}

#[derive(Debug, Clone)]
struct Scope<'a> {
    /// Parent scope of the scope
    parent: Option<&'a Scope<'a>>,

    /// Kind of the scope.
    kind: ScopeKind,

    /// [Mark] of the current scope.
    mark: Mark,

    /// All declarations in the scope
    declared_symbols: FxHashSet<JsWord>,
}

impl<'a> Scope<'a> {
    pub fn new(kind: ScopeKind, mark: Mark, parent: Option<&'a Scope<'a>>) -> Self {
        Scope {
            parent,
            kind,
            mark,
            declared_symbols: Default::default(),
        }
    }

    fn is_declared(&self, symbol: &JsWord) -> bool {
        if self.declared_symbols.contains(symbol) {
            return true;
        }

        self.parent.map_or(false, |p| p.is_declared(symbol))
    }
}

/// # Phases
///
/// ## Hoisting phase
///
/// ## Resolving phase
struct Resolver<'a> {
    current: Scope<'a>,
    ident_type: IdentType,

    config: InnerConfig,
}

#[derive(Debug, Clone, Copy)]
struct InnerConfig {
    unresolved_mark: Mark,
}

impl<'a> Resolver<'a> {
    #[cfg(test)]
    fn new(current: Scope<'a>, config: InnerConfig) -> Self {
        Resolver {
            current,
            ident_type: IdentType::Ref,
            config,
        }
    }

    fn with_child<F>(&self, kind: ScopeKind, op: F)
    where
        F: for<'aa> FnOnce(&mut Resolver<'aa>),
    {
        let mut child = Resolver {
            current: Scope::new(kind, Mark::new(), Some(&self.current)),
            ident_type: IdentType::Ref,
            config: self.config,
        };

        op(&mut child);
    }

    fn visit_mut_stmt_within_child_scope(&mut self, s: &mut Stmt) {
        self.with_child(ScopeKind::Block, |child| {
            child.visit_mut_stmt_within_same_scope(s)
        });
    }

    fn visit_mut_stmt_within_same_scope(&mut self, s: &mut Stmt) {
        match s {
            Stmt::Block(s) => {
                s.visit_mut_children_with(self);
            }
            _ => s.visit_mut_with(self),
        }
    }

    /// Returns a [Mark] for an identifier reference.
    fn mark_for_ref(&self, sym: &JsWord) -> Option<Mark> {
        self.mark_for_ref_inner(sym, false)
    }

    fn mark_for_ref_inner(&self, sym: &JsWord, stop_an_fn_scope: bool) -> Option<Mark> {
        let mut mark = self.current.mark;
        let mut scope = Some(&self.current);

        while let Some(cur) = scope {
            if cur.declared_symbols.contains(sym) {
                if mark == Mark::root() {
                    return None;
                }
                return Some(mark);
            }

            if cur.kind == ScopeKind::Fn && stop_an_fn_scope {
                return None;
            }

            if let Some(parent) = &cur.parent {
                mark = parent.mark;
            }
            scope = cur.parent;
        }

        None
    }

    /// Modifies a binding identifier.
    fn modify(&mut self, ident: &mut Ident, kind: Option<VarDeclKind>) {
        // if cfg!(debug_assertions) && LOG {
        //     debug!(
        //         "Binding (type = {}) {}{:?} {:?}",
        //         self.in_type,
        //         ident.sym,
        //         ident.span.ctxt(),
        //         kind
        //     );
        // }

        if ident.ctxt != SyntaxContext::empty() {
            return;
        }

        let mark = self.current.mark;

        self.current.declared_symbols.insert(ident.sym.clone());

        if mark != Mark::root() {
            ident.ctxt = ident.ctxt.apply_mark(mark);
            // if cfg!(debug_assertions) && LOG {
            //     debug!("\t-> {:?}", span.ctxt());
            // }
        }
    }
}

macro_rules! track_ident_mut {
    () => {
        fn visit_mut_export_specifier(&mut self, s: &mut ExportSpecifier) {
            let old = self.ident_type;
            self.ident_type = IdentType::Ref;
            s.visit_mut_children_with(self);
            self.ident_type = old;
        }

        fn visit_mut_import_specifier(&mut self, s: &mut ImportSpecifier) {
            let old = self.ident_type;
            self.ident_type = IdentType::Binding;

            match s {
                ImportSpecifier::Named(ImportNamedSpecifier { imported: None, .. })
                | ImportSpecifier::Namespace(..)
                | ImportSpecifier::Default(..) => s.visit_mut_children_with(self),
                ImportSpecifier::Named(s) => s.local.visit_mut_with(self),
            };

            self.ident_type = old;
        }

        fn visit_mut_getter_prop(&mut self, f: &mut GetterProp) {
            let old = self.ident_type;
            self.ident_type = IdentType::Ref;
            f.key.visit_mut_with(self);
            self.ident_type = old;

            f.body.visit_mut_with(self);
        }

        // impl<'a> Fold for $T<'a> {
        //     fn fold(&mut self, f: GetterProp) -> GetterProp {
        //         let body = f.body.visit_mut_with(self);

        //         GetterProp { body, ..c }
        //     }
        // }

        fn visit_mut_labeled_stmt(&mut self, s: &mut LabeledStmt) {
            let old = self.ident_type;
            self.ident_type = IdentType::Label;
            s.label.visit_mut_with(self);
            self.ident_type = old;

            s.body.visit_mut_with(self);
        }

        fn visit_mut_break_stmt(&mut self, s: &mut BreakStmt) {
            let old = self.ident_type;
            self.ident_type = IdentType::Label;
            s.label.visit_mut_with(self);
            self.ident_type = old;
        }

        fn visit_mut_continue_stmt(&mut self, s: &mut ContinueStmt) {
            let old = self.ident_type;
            self.ident_type = IdentType::Label;
            s.label.visit_mut_with(self);
            self.ident_type = old;
        }

        fn visit_mut_key_value_pat_prop(&mut self, n: &mut KeyValuePatProp) {
            n.key.visit_mut_with(self);
            n.value.visit_mut_with(self);
        }

        fn visit_mut_class(&mut self, c: &mut Class) {
            let old = self.ident_type;
            self.ident_type = IdentType::Ref;
            c.decorators.visit_mut_with(self);

            self.ident_type = IdentType::Ref;
            if let Some(extends) = &mut c.extends {
                extends.super_class.visit_mut_with(self);
            }

            self.ident_type = old;

            c.body.visit_mut_with(self);
        }

        fn visit_mut_prop_name(&mut self, n: &mut PropName) {
            match n {
                PropName::Computed(c) => {
                    c.visit_mut_with(self);
                }
                _ => {}
            }
        }
    };
}

impl<'a> VisitMut<'_> for Resolver<'a> {
    fn visit_mut_member_expr(&mut self, n: &mut MemberExpr) {
        n.obj.visit_mut_with(self);
        if n.computed {
            n.prop.visit_mut_with(self);
        }
    }

    track_ident_mut!();

    fn visit_mut_arrow_expr(&mut self, e: &mut ArrowExpr) {
        self.with_child(ScopeKind::Fn, |child| {
            let old = child.ident_type;
            child.ident_type = IdentType::Binding;
            e.params.visit_mut_with(child);
            child.ident_type = old;

            e.body.stmts.visit_mut_with(child);
        });
    }

    fn visit_mut_binding_ident(&mut self, i: &mut BindingIdent) {
        let ident_type = self.ident_type;

        i.id.visit_mut_with(self);

        self.ident_type = ident_type;
    }

    fn visit_mut_block_stmt(&mut self, block: &mut BlockStmt) {
        self.with_child(ScopeKind::Block, |child| {
            block.visit_mut_children_with(child);
        })
    }

    fn visit_mut_catch_clause(&mut self, c: &mut CatchClause) {
        // Child folder

        self.with_child(ScopeKind::Fn, |child| {
            child.ident_type = IdentType::Binding;
            c.param.visit_mut_with(child);
            child.ident_type = IdentType::Ref;

            c.body.visit_mut_children_with(child);
        });
    }

    fn visit_mut_class_decl(&mut self, n: &mut ClassDecl) {
        self.modify(&mut n.ident, None);

        n.class.decorators.visit_mut_with(self);

        // Create a child scope. The class name is only accessible within the class.

        self.with_child(ScopeKind::Fn, |child| {
            child.ident_type = IdentType::Ref;

            n.class.visit_mut_with(child);
        });
    }

    fn visit_mut_class_expr(&mut self, n: &mut ClassExpr) {
        // Create a child scope. The class name is only accessible within the class.

        self.with_child(ScopeKind::Fn, |child| {
            child.ident_type = IdentType::Binding;
            n.ident.visit_mut_with(child);
            child.ident_type = IdentType::Ref;

            n.class.visit_mut_with(child);
        });
    }

    fn visit_mut_class_method(&mut self, m: &mut ClassMethod) {
        m.key.visit_mut_with(self);

        for p in m.function.params.iter_mut() {
            p.decorators.visit_mut_with(self);
        }

        self.with_child(ScopeKind::Fn, |child| m.function.visit_mut_with(child));
    }

    fn visit_mut_class_prop(&mut self, p: &mut ClassProp) {
        p.decorators.visit_mut_with(self);

        if let PropName::Computed(key) = &mut p.key {
            let old = self.ident_type;
            self.ident_type = IdentType::Binding;
            key.expr.visit_mut_with(self);
            self.ident_type = old;
        }

        let old = self.ident_type;
        self.ident_type = IdentType::Ref;
        p.value.visit_mut_with(self);
        self.ident_type = old;
    }

    fn visit_mut_constructor(&mut self, c: &mut Constructor) {
        self.with_child(ScopeKind::Fn, |child| {
            let old = child.ident_type;
            child.ident_type = IdentType::Binding;
            c.params.visit_mut_with(child);
            child.ident_type = old;

            c.body.visit_mut_children_with(child);
        });
    }

    fn visit_mut_decl(&mut self, decl: &mut Decl) {
        decl.visit_mut_children_with(self)
    }

    fn visit_mut_export_default_decl(&mut self, e: &mut ExportDefaultDecl) {
        // Treat default exported functions and classes as declarations
        // even though they are parsed as expressions.
        match &mut e.decl {
            DefaultDecl::Fn(f) => {
                if f.ident.is_some() {
                    self.with_child(ScopeKind::Fn, |child| {
                        f.function.visit_mut_with(child);
                    });
                } else {
                    f.visit_mut_with(self)
                }
            }
            DefaultDecl::Class(c) => {
                // Skip class expression visitor to treat as a declaration.
                c.class.visit_mut_with(self)
            }
        }
    }

    fn visit_mut_export_default_expr(&mut self, node: &mut ExportDefaultExpr) {
        node.expr.visit_mut_with(self);
    }

    fn visit_mut_export_named_specifier(&mut self, e: &mut ExportNamedSpecifier) {
        e.visit_mut_children_with(self);
    }

    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        // let _span = if LOG {
        //     Some(span!(Level::ERROR, "visit_mut_expr").entered())
        // } else {
        //     None
        // };

        let old = self.ident_type;
        self.ident_type = IdentType::Ref;
        expr.visit_mut_children_with(self);
        self.ident_type = old;
    }

    fn visit_mut_fn_decl(&mut self, node: &mut FnDecl) {
        // We don't fold this as Hoister handles this.

        node.function.decorators.visit_mut_with(self);

        self.with_child(ScopeKind::Fn, |child| node.function.visit_mut_with(child));
    }

    fn visit_mut_fn_expr(&mut self, e: &mut FnExpr) {
        e.function.decorators.visit_mut_with(self);

        self.with_child(ScopeKind::Fn, |child| {
            if let Some(ident) = &mut e.ident {
                child.modify(ident, None)
            }
            e.function.visit_mut_with(child);
        });
    }

    fn visit_mut_for_in_stmt(&mut self, n: &mut ForInStmt) {
        self.with_child(ScopeKind::Block, |child| {
            n.left.visit_mut_with(child);
            n.right.visit_mut_with(child);

            child.visit_mut_stmt_within_child_scope(&mut *n.body);
        });
    }

    fn visit_mut_for_of_stmt(&mut self, n: &mut ForOfStmt) {
        self.with_child(ScopeKind::Block, |child| {
            n.left.visit_mut_with(child);
            n.right.visit_mut_with(child);

            child.visit_mut_stmt_within_child_scope(&mut *n.body);
        });
    }

    fn visit_mut_for_stmt(&mut self, n: &mut ForStmt) {
        self.with_child(ScopeKind::Block, |child| {
            child.ident_type = IdentType::Binding;
            n.init.visit_mut_with(child);
            child.ident_type = IdentType::Ref;
            n.test.visit_mut_with(child);
            child.ident_type = IdentType::Ref;
            n.update.visit_mut_with(child);

            child.visit_mut_stmt_within_child_scope(&mut *n.body);
        });
    }

    fn visit_mut_function(&mut self, f: &mut Function) {
        self.ident_type = IdentType::Ref;
        f.decorators.visit_mut_with(self);

        self.ident_type = IdentType::Binding;
        f.params.visit_mut_with(self);

        self.ident_type = IdentType::Ref;
        // Prevent creating new scope.
        f.body.visit_mut_children_with(self);
    }

    fn visit_mut_ident(&mut self, i: &mut Ident) {
        if i.ctxt != SyntaxContext::empty() {
            return;
        }

        match self.ident_type {
            IdentType::Binding => self.modify(i, None),
            IdentType::Ref => {
                // if cfg!(debug_assertions) && LOG {
                //     debug!(
                //         "IdentRef (type = {}) {}{:?}",
                //         self.in_type,
                //         sym,
                //         span.ctxt()
                //     );
                // }

                if i.ctxt != SyntaxContext::empty() {
                    return;
                }

                if let Some(mark) = self.mark_for_ref(&i.sym) {
                    // if cfg!(debug_assertions) && LOG {
                    //     debug!("\t -> {:?}", span.ctxt());
                    // }
                    i.ctxt = i.ctxt.apply_mark(mark);
                } else {
                    // if cfg!(debug_assertions) && LOG {
                    //     debug!("\t -> Unresolved");
                    // }

                    // if cfg!(debug_assertions) && LOG {
                    //     debug!("\t -> {:?}", span.ctxt());
                    // }

                    i.ctxt = i.ctxt.apply_mark(self.config.unresolved_mark);
                    // Support hoisting
                    self.modify(i, None)
                }
            }
            // We currently does not touch labels
            IdentType::Label => {}
        }
    }

    fn visit_mut_import_decl(&mut self, n: &mut ImportDecl) {
        // Always resolve the import declaration identifiers even if it's type only.
        // We need to analyze these identifiers for type stripping purposes.
        self.ident_type = IdentType::Binding;
        n.visit_mut_children_with(self);
    }

    fn visit_mut_import_named_specifier(&mut self, s: &mut ImportNamedSpecifier) {
        let old = self.ident_type;
        self.ident_type = IdentType::Binding;
        s.local.visit_mut_with(self);
        self.ident_type = old;
    }

    /// Ignore.
    ///
    /// See https://github.com/swc-project/swc/issues/2854
    fn visit_mut_jsx_attr_name(&mut self, _: &mut JSXAttrName) {}

    fn visit_mut_method_prop(&mut self, m: &mut MethodProp) {
        m.key.visit_mut_with(self);

        // Child folder
        self.with_child(ScopeKind::Fn, |child| m.function.visit_mut_with(child));
    }

    fn visit_mut_module_items(&mut self, stmts: &mut Vec<ModuleItem>) {
        // Phase 1: Handle hoisting
        {
            let mut hoister = Hoister {
                resolver: self,
                kind: None,
                in_block: false,
                in_catch_body: false,
                catch_param_decls: Default::default(),
                excluded_from_catch: Default::default(),
            };
            stmts.visit_mut_with(&mut hoister)
        }

        // Phase 2.
        stmts.visit_mut_children_with(self)
    }

    fn visit_mut_named_export(&mut self, e: &mut NamedExport) {
        if e.src.is_some() {
            return;
        }

        e.visit_mut_children_with(self);
    }

    fn visit_mut_object_lit(&mut self, o: &mut ObjectLit) {
        self.with_child(ScopeKind::Block, |child| {
            o.visit_mut_children_with(child);
        });
    }

    fn visit_mut_param(&mut self, param: &mut Param) {
        self.ident_type = IdentType::Binding;
        param.visit_mut_children_with(self);
    }

    fn visit_mut_pat(&mut self, p: &mut Pat) {
        p.visit_mut_children_with(self);
    }

    fn visit_mut_assign_pat(&mut self, node: &mut AssignPat) {
        node.left.visit_mut_with(self);
        node.right.visit_mut_with(self);
    }

    fn visit_mut_rest_pat(&mut self, node: &mut RestPat) {
        node.arg.visit_mut_with(self);
    }

    fn visit_mut_private_method(&mut self, m: &mut PrivateMethod) {
        m.key.visit_mut_with(self);

        {
            // Child folder

            self.with_child(ScopeKind::Fn, |child| m.function.visit_mut_with(child));
        }
    }

    fn visit_mut_private_name(&mut self, _: &mut PrivateName) {}

    fn visit_mut_setter_prop(&mut self, n: &mut SetterProp) {
        n.key.visit_mut_with(self);

        {
            self.with_child(ScopeKind::Fn, |child| {
                child.ident_type = IdentType::Binding;
                n.param.visit_mut_with(child);
                n.body.visit_mut_with(child);
            });
        };
    }

    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        // let _span = if LOG {
        //     Some(span!(Level::ERROR, "visit_mut_stmts").entered())
        // } else {
        //     None
        // };

        // Phase 1: Handle hoisting
        {
            // let _span = if LOG {
            //     Some(span!(Level::ERROR, "hoist").entered())
            // } else {
            //     None
            // };

            let mut hoister = Hoister {
                resolver: self,
                kind: None,
                in_block: false,
                in_catch_body: false,
                catch_param_decls: Default::default(),
                excluded_from_catch: Default::default(),
            };
            stmts.visit_mut_with(&mut hoister)
        }

        // Phase 2.
        stmts.visit_mut_children_with(self)
    }

    fn visit_mut_switch_stmt(&mut self, s: &mut SwitchStmt) {
        s.discriminant.visit_mut_with(self);

        self.with_child(ScopeKind::Block, |child| {
            s.cases.visit_mut_with(child);
        });
    }

    fn visit_mut_var_decl(&mut self, decl: &mut VarDecl) {
        decl.decls.visit_mut_with(self);
    }

    fn visit_mut_var_declarator(&mut self, decl: &mut VarDeclarator) {
        // order is important

        let old_type = self.ident_type;
        self.ident_type = IdentType::Binding;
        decl.name.visit_mut_with(self);
        self.ident_type = old_type;

        decl.init.visit_mut_children_with(self);
    }
}

/// The folder which handles var / function hoisting.
struct Hoister<'a, 'b> {
    resolver: &'a mut Resolver<'b>,
    kind: Option<VarDeclKind>,
    /// Hoister should not touch let / const in the block.
    in_block: bool,

    in_catch_body: bool,

    excluded_from_catch: FxHashSet<JsWord>,
    catch_param_decls: FxHashSet<JsWord>,
}

impl Hoister<'_, '_> {
    fn add_pat_id(&mut self, id: &mut Ident) {
        if self.in_catch_body {
            // If we have a binding, it's different variable.
            if self.resolver.mark_for_ref_inner(&id.sym, true).is_some()
                && self.catch_param_decls.contains(&id.sym)
            {
                return;
            }

            self.excluded_from_catch.insert(id.sym.clone());
        } else {
            // Behavior is different
            if self.catch_param_decls.contains(&id.sym)
                && !self.excluded_from_catch.contains(&id.sym)
            {
                return;
            }
        }

        self.resolver.modify(id, self.kind)
    }
}

impl VisitMut<'_> for Hoister<'_, '_> {
    #[inline]
    fn visit_mut_arrow_expr(&mut self, _: &mut ArrowExpr) {}

    fn visit_mut_assign_pat_prop(&mut self, node: &mut AssignPatProp) {
        node.visit_mut_children_with(self);

        self.add_pat_id(&mut node.key);
    }

    fn visit_mut_block_stmt(&mut self, n: &mut BlockStmt) {
        let old_in_block = self.in_block;
        self.in_block = true;
        n.visit_mut_children_with(self);
        self.in_block = old_in_block;
    }

    /// The code below prints "PASS"
    ///
    /// ```js
    ///
    ///      var a = "PASS";
    ///      try {
    ///          throw "FAIL1";
    ///          } catch (a) {
    ///          var a = "FAIL2";
    ///      }
    ///      console.log(a);
    /// ```
    ///
    /// While the code below does not throw **ReferenceError** for `b`
    ///
    /// ```js
    ///
    ///      b()
    ///      try {
    ///      } catch (b) {
    ///          var b;
    ///      }
    /// ```
    ///
    /// while the code below throws **ReferenceError**
    ///
    /// ```js
    ///
    ///      b()
    ///      try {
    ///      } catch (b) {
    ///      }
    /// ```
    #[inline]
    fn visit_mut_catch_clause(&mut self, c: &mut CatchClause) {
        let old_exclude = self.excluded_from_catch.clone();
        self.excluded_from_catch = Default::default();

        let old_in_catch_body = self.in_catch_body;

        let params: Vec<Id> = find_pat_ids(&c.param);

        self.catch_param_decls
            .extend(params.into_iter().map(|v| v.0));

        self.in_catch_body = true;
        c.body.visit_mut_with(self);

        let orig = self.catch_param_decls.clone();

        // let mut excluded = find_ids::<_, Id>(&c.body);

        // excluded.retain(|id| {
        //     // If we already have a declartion named a, `var a` in the catch body is
        //     // different var.

        //     self.resolver.mark_for_ref(&id.0).is_none()
        // });

        self.in_catch_body = false;
        c.param.visit_mut_with(self);

        self.catch_param_decls = orig;

        self.in_catch_body = old_in_catch_body;
        self.excluded_from_catch = old_exclude;
    }

    fn visit_mut_class_decl(&mut self, node: &mut ClassDecl) {
        if self.in_block {
            return;
        }
        self.resolver
            .modify(&mut node.ident, Some(VarDeclKind::Let));
    }

    #[inline]
    fn visit_mut_constructor(&mut self, _: &mut Constructor) {}

    #[inline]
    fn visit_mut_decl(&mut self, decl: &mut Decl) {
        decl.visit_mut_children_with(self);
    }

    fn visit_mut_export_default_decl(&mut self, node: &mut ExportDefaultDecl) {
        // Treat default exported functions and classes as declarations
        // even though they are parsed as expressions.
        match &mut node.decl {
            DefaultDecl::Fn(f) => {
                if let Some(id) = &mut f.ident {
                    self.resolver.modify(id, Some(VarDeclKind::Var));
                }

                f.visit_mut_with(self)
            }
            DefaultDecl::Class(c) => {
                if let Some(id) = &mut c.ident {
                    self.resolver.modify(id, Some(VarDeclKind::Let));
                }

                c.visit_mut_with(self)
            }
        }
    }

    #[inline]
    fn visit_mut_expr(&mut self, _: &mut Expr) {}

    fn visit_mut_fn_decl(&mut self, node: &mut FnDecl) {
        if self.catch_param_decls.contains(&node.ident.sym) {
            return;
        }

        if self.in_block {
            // If we are in nested block, and variable named `foo` is declared, we should
            // ignore function foo while handling upper scopes.
            if self.resolver.current.is_declared(&node.ident.sym) {
                return;
            }
        }

        self.resolver
            .modify(&mut node.ident, Some(VarDeclKind::Var));
    }

    #[inline]
    fn visit_mut_function(&mut self, _: &mut Function) {}

    fn visit_mut_import_default_specifier(&mut self, n: &mut ImportDefaultSpecifier) {
        n.visit_mut_children_with(self);

        self.resolver.modify(&mut n.local, None);
    }

    fn visit_mut_import_named_specifier(&mut self, n: &mut ImportNamedSpecifier) {
        n.visit_mut_children_with(self);

        self.resolver.modify(&mut n.local, None);
    }

    fn visit_mut_import_star_as_specifier(&mut self, n: &mut ImportStarAsSpecifier) {
        n.visit_mut_children_with(self);

        self.resolver.modify(&mut n.local, None);
    }

    #[inline]
    fn visit_mut_param(&mut self, _: &mut Param) {}

    fn visit_mut_pat(&mut self, node: &mut Pat) {
        match node {
            Pat::Ident(i) => {
                self.add_pat_id(&mut i.id);
            }

            _ => node.visit_mut_children_with(self),
        }
    }

    #[inline]
    fn visit_mut_pat_or_expr(&mut self, _: &mut PatOrExpr) {}

    #[inline]
    fn visit_mut_setter_prop(&mut self, _: &mut SetterProp) {}

    fn visit_mut_switch_stmt(&mut self, s: &mut SwitchStmt) {
        s.discriminant.visit_mut_with(self);

        let old_in_block = self.in_block;
        self.in_block = true;
        s.cases.visit_mut_with(self);
        self.in_block = old_in_block;
    }

    #[inline]
    fn visit_mut_tagged_tpl(&mut self, _: &mut TaggedTpl) {}

    #[inline]
    fn visit_mut_tpl(&mut self, _: &mut Tpl) {}

    fn visit_mut_var_decl(&mut self, node: &mut VarDecl) {
        if self.in_block {
            match node.kind {
                VarDeclKind::Const | VarDeclKind::Let => return,
                _ => {}
            }
        }

        let old_kind = self.kind;
        self.kind = Some(node.kind);

        node.visit_mut_children_with(self);

        self.kind = old_kind;
    }

    fn visit_mut_var_decl_or_expr(&mut self, n: &mut VarDeclOrExpr) {
        match n {
            VarDeclOrExpr::VarDecl(VarDecl {
                kind: VarDeclKind::Let,
                ..
            })
            | VarDeclOrExpr::VarDecl(VarDecl {
                kind: VarDeclKind::Const,
                ..
            }) => {}
            _ => {
                n.visit_mut_children_with(self);
            }
        }
    }

    fn visit_mut_var_decl_or_pat(&mut self, n: &mut VarDeclOrPat) {
        match n {
            VarDeclOrPat::VarDecl(VarDecl {
                kind: VarDeclKind::Let,
                ..
            })
            | VarDeclOrPat::VarDecl(VarDecl {
                kind: VarDeclKind::Const,
                ..
            }) => {}
            // Hoister should not handle lhs of for in statement below
            //
            // const b = [];
            // {
            //   let a;
            //   for (a in b) {
            //     console.log(a);
            //   }
            // }
            VarDeclOrPat::Pat(..) => {}
            _ => {
                n.visit_mut_children_with(self);
            }
        }
    }

    #[inline]
    fn visit_mut_var_declarator(&mut self, node: &mut VarDeclarator) {
        node.name.visit_mut_with(self);
    }

    /// should visit var decls first, cause var decl may appear behind the
    /// usage. this can deal with code below:
    /// ```js
    /// try {} catch (Ic) {
    ///   throw Ic;
    /// }
    /// var Ic;
    /// ```
    /// the `Ic` defined by catch param and the `Ic` defined by `var Ic` are
    /// different variables.
    /// If we deal with the `var Ic` first, we can know
    /// that there is already an global declaration of Ic when deal with the try
    /// block.
    fn visit_mut_module_items(&mut self, items: &mut Vec<ModuleItem>) {
        let mut other_items = vec![];

        for item in items {
            match item {
                ModuleItem::Stmt(Stmt::Decl(
                    Decl::Var(VarDecl {
                        kind: VarDeclKind::Var,
                        ..
                    })
                    | Decl::Fn(..),
                ))
                | ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
                    decl:
                        Decl::Var(VarDecl {
                            kind: VarDeclKind::Var,
                            ..
                        })
                        | Decl::Fn(..),
                    ..
                })) => {
                    item.visit_mut_with(self);
                }
                _ => {
                    other_items.push(item);
                }
            }
        }

        for other_item in other_items {
            other_item.visit_mut_with(self);
        }
    }

    /// see docs for `self.visit_mut_module_items`
    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        let mut other_stmts = vec![];

        for item in stmts {
            match item {
                Stmt::Decl(Decl::Var(VarDecl {
                    kind: VarDeclKind::Var,
                    ..
                }))
                | Stmt::Decl(Decl::Fn(..)) => {
                    item.visit_mut_with(self);
                }
                _ => {
                    other_stmts.push(item);
                }
            }
        }

        for other_stmt in other_stmts {
            other_stmt.visit_mut_with(self);
        }
    }
}

/// Finds all **binding** idents of variables.
struct DestructuringFinder<'a> {
    pub found: &'a mut Vec<Id>,
}

/// Finds all **binding** idents of `node`.
fn find_pat_ids<'ast, T>(node: &'ast T) -> Vec<Id>
where
    T: for<'any> VisitWith<'ast, DestructuringFinder<'any>>,
{
    let mut found = vec![];

    {
        let mut v = DestructuringFinder { found: &mut found };
        node.visit_with(&mut v);
    }

    found
}

impl<'a> Visit<'_> for DestructuringFinder<'a> {
    /// No-op (we don't care about expressions)
    fn visit_expr(&mut self, _: &Expr) {}

    fn visit_ident(&mut self, i: &Ident) {
        self.found.push((i.sym.clone(), i.ctxt));
    }

    /// No-op (we don't care about expressions)
    fn visit_prop_name(&mut self, _: &PropName) {}
}
