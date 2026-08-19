use ast::*;
use rustc_hash::{FxHashMap, FxHashSet};
use visit::{VisitMut, VisitMutWith};

// TODO: even though it wil error at runtime, lexical declarations can be
// accessed before declaration for the purpose of binding.

// TODO: get some tests.

pub fn resolve(program: &mut Program, program_data: &mut ProgramData) {
    let mut resolver = Resolver {
        program_data,
        scopes: vec![Scope {
            names: FxHashMap::default(),
        }],
        in_decl: false,
        processed_names: FxHashSet::default(),
    };

    program.visit_mut_with(&mut resolver);
}

struct Resolver<'d> {
    program_data: &'d mut ProgramData,
    scopes: Vec<Scope>,
    in_decl: bool,
    processed_names: FxHashSet<NameId>,
}

impl Resolver<'_> {
    fn handle_reference(&mut self, name: &mut NameId) {
        if !name.is_unresolved() {
            return;
        }

        for scope in self.scopes.iter().rev() {
            if let Some(new_name) = scope.names.get(&name) {
                *name = *new_name;
                return;
            }
        }
    }

    fn handle_decl(&mut self, name: &mut NameId) {
        let cur_scope = &mut self
            .scopes
            .last_mut()
            .expect("there's always the global scope");

        if !cur_scope.names.contains_key(name) {
            if self.processed_names.insert(*name) {
                cur_scope
                    .names
                    .insert(*name, ProgramData::mark_resolved(*name));
            } else {
                let new_name = self.program_data.new_resolved_name_from(*name);
                cur_scope.names.insert(*name, new_name);
            }
        }

        self.handle_reference(name);
    }

    /// Runs `op` with a new scope on the stack.
    fn with_scope<F>(&mut self, mut op: F)
    where
        F: FnMut(&mut Self),
    {
        self.scopes.push(Scope {
            names: FxHashMap::default(),
        });
        op(self);
        self.scopes.pop();
    }
}

impl VisitMut<'_> for Resolver<'_> {
    fn visit_mut_binding_ident(&mut self, i: &mut BindingIdent) {
        if self.in_decl {
            self.handle_decl(&mut i.id.name);
        } else {
            self.handle_reference(&mut i.id.name);
        }
    }

    fn visit_mut_member_expr(&mut self, node: &mut MemberExpr) {
        node.obj.visit_mut_with(self);
        if node.computed {
            node.prop.visit_mut_with(self);
        }
    }

    fn visit_mut_var_declarator(&mut self, node: &mut VarDeclarator) {
        node.init.visit_mut_with(self);

        let old = self.in_decl;
        self.in_decl = true;
        node.name.visit_mut_with(self);
        self.in_decl = old;
    }
    fn visit_mut_param(&mut self, node: &mut Param) {
        let old = self.in_decl;
        self.in_decl = true;
        node.visit_mut_children_with(self);
        self.in_decl = old;
    }

    fn visit_mut_expr(&mut self, node: &mut Expr) {
        let old_in_decl = self.in_decl;
        self.in_decl = false;
        match node {
            Expr::Ident(ident) => {
                self.handle_reference(&mut ident.name);
            }
            _ => {
                node.visit_mut_children_with(self);
            }
        }
        self.in_decl = old_in_decl;
    }

    fn visit_mut_fn_decl(&mut self, node: &mut FnDecl) {
        node.visit_mut_children_with(self);
    }
    fn visit_mut_class_decl(&mut self, node: &mut ClassDecl) {
        self.handle_decl(&mut node.ident.name);
        node.visit_mut_children_with(self);
    }

    fn visit_mut_fn_expr(&mut self, node: &mut FnExpr) {
        self.with_scope(|visitor| {
            if let Some(name) = &mut node.ident {
                visitor.handle_decl(&mut name.name);
            }
            node.visit_mut_children_with(visitor);
        });
    }
    fn visit_mut_class_expr(&mut self, node: &mut ClassExpr) {
        self.with_scope(|visitor| {
            if let Some(name) = &mut node.ident {
                visitor.handle_decl(&mut name.name);
            }
            node.visit_mut_children_with(visitor);
        });
    }

    // For functions, we don't want to use the BlockStmt visitor since it
    // creates a new, non-hoist scope. Instead, we want the function body and
    // params to share the same function hoist scope. So we use
    // visit_mut_children_with instead of visit_with.
    fn visit_mut_function(&mut self, node: &mut Function) {
        self.with_scope(|visitor| {
            for stmt in &mut node.body.stmts {
                hoist_declarations(stmt, &mut |name| {
                    visitor.handle_decl(name);
                });
            }
            node.params.visit_mut_with(visitor);
            node.body.visit_mut_children_with(visitor);
        });
    }
    fn visit_mut_constructor(&mut self, node: &mut Constructor) {
        self.with_scope(|visitor| {
            for stmt in &mut node.body.stmts {
                hoist_declarations(stmt, &mut |name| {
                    visitor.handle_decl(name);
                });
            }
            node.params.visit_mut_with(visitor);
            node.body.visit_mut_children_with(visitor);
        });
    }
    fn visit_mut_arrow_expr(&mut self, node: &mut ArrowExpr) {
        self.with_scope(|visitor| {
            for stmt in &mut node.body.stmts {
                hoist_declarations(stmt, &mut |name| {
                    visitor.handle_decl(name);
                });
            }
            node.params.visit_mut_with(visitor);
            node.body.visit_mut_children_with(visitor);
        });
    }
    fn visit_mut_getter_prop(&mut self, node: &mut GetterProp) {
        self.with_scope(|visitor| {
            for stmt in &mut node.body.stmts {
                hoist_declarations(stmt, &mut |name| {
                    visitor.handle_decl(name);
                });
            }
            node.key.visit_mut_with(visitor);
            node.body.visit_mut_children_with(visitor);
        });
    }
    fn visit_mut_setter_prop(&mut self, node: &mut SetterProp) {
        self.with_scope(|visitor| {
            for stmt in &mut node.body.stmts {
                hoist_declarations(stmt, &mut |name| {
                    visitor.handle_decl(name);
                });
            }
            node.key.visit_mut_with(visitor);
            node.param.visit_mut_with(visitor);
            node.body.visit_mut_children_with(visitor);
        });
    }
    // A CatchClause isn't a function, but it has a param.
    fn visit_mut_catch_clause(&mut self, node: &mut CatchClause) {
        self.with_scope(|visitor| {
            let old = visitor.in_decl;
            visitor.in_decl = true;
            node.param.visit_mut_with(visitor);
            visitor.in_decl = old;
            node.body.visit_mut_children_with(visitor);
        });
    }

    fn visit_mut_block_stmt(&mut self, node: &mut BlockStmt) {
        self.with_scope(|visitor| {
            node.stmts.visit_mut_with(visitor);
        });
    }

    fn visit_mut_script(&mut self, node: &mut Script) {
        self.with_scope(|visitor| {
            for stmt in &mut node.body {
                hoist_declarations(stmt, &mut |name| {
                    visitor.handle_decl(name);
                });
            }
            node.visit_mut_children_with(visitor);
        });
    }
    fn visit_mut_module(&mut self, node: &mut Module) {
        self.with_scope(|visitor| {
            for stmt in &mut node.body {
                match stmt {
                    ModuleItem::ModuleDecl(_) => todo!(),
                    ModuleItem::Stmt(stmt) => {
                        hoist_declarations(stmt, &mut |name| {
                            visitor.handle_decl(name);
                        });
                    }
                }
            }
            node.visit_mut_children_with(visitor);
        });
    }
}

#[derive(Debug)]
struct Scope {
    names: FxHashMap<NameId, NameId>,
}

fn hoist_declarations(stmt: &mut Stmt, op: &mut impl FnMut(&mut NameId)) {
    match stmt {
        Stmt::Block(block) => block
            .stmts
            .iter_mut()
            .for_each(|s| hoist_declarations(s, op)),

        Stmt::If(if_stmt) => {
            hoist_declarations(&mut if_stmt.cons, op);

            if let Some(alt) = if_stmt.alt.as_deref_mut() {
                hoist_declarations(alt, op);
            }
        }

        Stmt::While(WhileStmt { body, .. })
        | Stmt::DoWhile(DoWhileStmt { body, .. })
        | Stmt::With(WithStmt { body, .. })
        | Stmt::Labeled(LabeledStmt { body, .. }) => hoist_declarations(body, op),

        Stmt::For(ForStmt { body, init, .. }) => {
            if let Some(init) = init {
                if let VarDeclOrExpr::VarDecl(decl) = init.as_mut() {
                    match decl.kind {
                        VarDeclKind::Var => {
                            for decl in &mut decl.decls {
                                find_pat_ids(&mut decl.name, op);
                            }
                        }
                        VarDeclKind::Let | VarDeclKind::Const => {}
                    }
                }
            }
            hoist_declarations(body, op);
        }
        Stmt::ForIn(ForInStmt { body, left, .. }) | Stmt::ForOf(ForOfStmt { body, left, .. }) => {
            if let VarDeclOrPat::VarDecl(decl) = left.as_mut() {
                match decl.kind {
                    VarDeclKind::Var => {
                        for decl in &mut decl.decls {
                            find_pat_ids(&mut decl.name, op);
                        }
                    }
                    VarDeclKind::Let | VarDeclKind::Const => {}
                }
            }
            hoist_declarations(body, op);
        }

        Stmt::Try(try_stmt) => {
            try_stmt
                .block
                .stmts
                .iter_mut()
                .for_each(|s| hoist_declarations(s, op));

            if let Some(handler) = &mut try_stmt.handler {
                handler
                    .body
                    .stmts
                    .iter_mut()
                    .for_each(|s| hoist_declarations(s, op));
            }

            if let Some(finalizer) = &mut try_stmt.finalizer {
                finalizer
                    .stmts
                    .iter_mut()
                    .for_each(|s| hoist_declarations(s, op));
            }
        }

        Stmt::Switch(switch) => {
            switch
                .cases
                .iter_mut()
                .for_each(|c| c.cons.iter_mut().for_each(|s| hoist_declarations(s, op)));
        }

        Stmt::Decl(decl) => match decl {
            Decl::Class(_) => {}
            Decl::Fn(decl) => {
                op(&mut decl.ident.name);
            }
            Decl::Var(decl) => match decl.kind {
                VarDeclKind::Var => {
                    for decl in &mut decl.decls {
                        find_pat_ids(&mut decl.name, op);
                    }
                }
                VarDeclKind::Let | VarDeclKind::Const => {}
            },
        },

        Stmt::Empty(_)
        | Stmt::Debugger(_)
        | Stmt::Return(_)
        | Stmt::Continue(_)
        | Stmt::Throw(_)
        | Stmt::Expr(_)
        | Stmt::Break(_) => {}
    }
}

fn find_pat_ids(pat: &mut Pat, op: &mut impl FnMut(&mut NameId)) {
    match pat {
        Pat::Ident(ident) => {
            op(&mut ident.id.name);
        }
        Pat::Array(array) => {
            for el in &mut array.elems {
                if let Some(el) = el {
                    find_pat_ids(el, op);
                }
            }
        }
        Pat::Rest(rest) => find_pat_ids(&mut rest.arg, op),
        Pat::Object(object) => {
            for prop in &mut object.props {
                match prop {
                    ObjectPatProp::KeyValue(kv_prop) => find_pat_ids(&mut kv_prop.value, op),
                    ObjectPatProp::Rest(rest) => find_pat_ids(&mut rest.arg, op),
                }
            }
        }
        Pat::Assign(assign) => find_pat_ids(&mut assign.left, op),

        Pat::Expr(_) => {}

        Pat::Invalid(_) => unreachable!(),
    }
}
