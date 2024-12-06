use ast::*;
use atoms::JsWord;
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::SyntaxContext;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{DefaultNameGenerator::DefaultNameGenerator, Id, ToId};

#[cfg(test)]
mod tests;

pub fn process(ast: &mut Program, unresolved_ctxt: SyntaxContext) {
    let (rename_map, var_info) = analyse(ast, unresolved_ctxt);

    // Actually assign the new names.
    let mut renamer = Renamer {
        rename_map,
        var_info,
        unresolved_ctxt,
    };
    ast.visit_mut_with(&mut renamer);
}

fn analyse(
    ast: &Program,
    unresolved_ctxt: SyntaxContext,
) -> (FxHashMap<u32, JsWord>, FxHashMap<Id, VarInfo>) {
    let mut analyser = Analyser {
        scopes: Default::default(),
        max_depth: 0,
        in_var_decl: false,
        var_info: Default::default(),
        unresolved_ctxt,
        global_declarations: 0,
        unresolved_references: FxHashSet::default(),
    };
    ast.visit_with(&mut analyser);

    let number_of_local_slots = analyser.max_depth + 1;

    let num_slots = (number_of_local_slots + analyser.global_declarations) as usize;
    let mut slots = vec![Slot::default(); num_slots];

    for info in analyser.var_info.values_mut() {
        debug_assert!(info.slot != u32::MAX, "All variables should be declared");
        if info.global {
            // Globals are appended at the end, so update slot indices.
            info.slot += number_of_local_slots;
        }
        slots[info.slot as usize].reference_count += info.reference_count;
        slots[info.slot as usize].depth = info.slot;
    }

    debug_assert!(slots.iter().is_sorted_by_key(|x| x.depth));

    slots.sort_by(|a, b| b.reference_count.cmp(&a.reference_count));

    let mut name_gen = DefaultNameGenerator::new(analyser.unresolved_references);
    let mut rename_map = FxHashMap::with_capacity_and_hasher(num_slots, Default::default());
    for slot in slots {
        rename_map.insert(slot.depth, name_gen.generate_next_name());
    }

    (rename_map, analyser.var_info)
}

#[derive(Default, Clone, Copy)]
struct Slot {
    reference_count: u32,
    depth: u32,
}

struct VarInfo {
    reference_count: u32,
    slot: u32,
    global: bool,
}

impl Default for VarInfo {
    fn default() -> Self {
        Self {
            reference_count: 0,
            slot: u32::MAX,
            global: false,
        }
    }
}

struct Analyser {
    scopes: Vec<Scope>,
    max_depth: u32,
    /// Whether we are visiting the names in a `var` decl.
    in_var_decl: bool,
    unresolved_ctxt: SyntaxContext,
    var_info: FxHashMap<Id, VarInfo>,
    global_declarations: u32,
    unresolved_references: FxHashSet<JsWord>,
}

impl Analyser {
    /// Records a reference to an [`Id`].
    fn handle_reference(&mut self, name: &Id) {
        if name.1 == self.unresolved_ctxt {
            self.unresolved_references.insert(name.0.clone());
            return;
        }
        if name.1 == SyntaxContext::empty() {
            // This isn't a variable reference.
            return;
        }
        self.var_info
            .entry(name.clone())
            .or_default()
            .reference_count += 1;
    }

    /// Records a declaration of (and reference to) an [`Id`].
    fn handle_decl(&mut self, name: Id) {
        if name.1 == self.unresolved_ctxt {
            self.unresolved_references.insert(name.0);
            return;
        }
        if name.1 == SyntaxContext::empty() {
            // This isn't a variable reference.
            return;
        }
        let info = self.var_info.entry(name.clone()).or_default();
        info.reference_count += 1;

        if info.slot == u32::MAX {
            let scope_pos = if self.in_var_decl {
                // There is always the global scope (which is a hoist scope).
                self.scopes.iter().rposition(|s| s.is_hoist_scope)
            } else {
                self.scopes.len().checked_sub(1)
            };
            if let Some(scope_pos) = scope_pos {
                // Local.
                let depth = self.scopes[..=scope_pos]
                    .iter()
                    .map(|s| s.num_declarations)
                    .sum::<u32>();

                if depth > self.max_depth {
                    self.max_depth = depth;
                }

                info.slot = depth;
                self.scopes[scope_pos].num_declarations += 1;
            } else {
                // Global.
                info.global = true;
                info.slot = self.global_declarations;
                self.global_declarations += 1;
            }
        }
    }

    /// Runs `op` with a new scope on the stack.
    fn with_scope<F>(&mut self, is_hoist_scope: bool, mut op: F)
    where
        F: FnMut(&mut Self),
    {
        self.scopes.push(Scope {
            num_declarations: 0,
            is_hoist_scope,
        });
        op(self);
        self.scopes.pop();
    }
}

impl Visit<'_> for Analyser {
    fn visit_ident(&mut self, i: &Ident) {
        self.handle_reference(&i.to_id());
    }

    fn visit_binding_ident(&mut self, i: &BindingIdent) {
        self.handle_decl(i.to_id());
    }

    fn visit_var_decl(&mut self, node: &VarDecl) {
        if node.kind == VarDeclKind::Var {
            let old = self.in_var_decl;
            self.in_var_decl = true;
            node.decls.visit_with(self);
            self.in_var_decl = old;
        } else {
            node.decls.visit_with(self);
        }
    }

    fn visit_expr(&mut self, node: &Expr) {
        let old = self.in_var_decl;
        self.in_var_decl = false;
        node.visit_children_with(self);
        self.in_var_decl = old;
    }

    fn visit_fn_decl(&mut self, node: &FnDecl) {
        self.handle_decl(node.ident.to_id());
        node.function.visit_with(self);
    }
    fn visit_class_decl(&mut self, node: &ClassDecl) {
        self.handle_decl(node.ident.to_id());
        node.class.visit_with(self);
    }

    fn visit_fn_expr(&mut self, node: &FnExpr) {
        if let Some(name) = &node.ident {
            self.handle_decl(name.to_id());
        }
        node.function.visit_with(self);
    }
    fn visit_class_expr(&mut self, node: &ClassExpr) {
        if let Some(name) = &node.ident {
            self.handle_decl(name.to_id());
        }
        node.class.visit_with(self);
    }

    // For functions, we don't want to use the BlockStmt visitor since it
    // creates a new, non-hoist scope. Instead, we want the function body and
    // params to share the same function hoist scope. So we use
    // visit_children_with instead of visit_with.
    // Also, if the body is an Option<BlockStmt>, then we have to call
    // visit_children_with on the unwrapped BlockStmt (visiting the stmts)
    // rather than on the Option (which would visit the BlockStmt).
    // See test case `function_scope_creation` for a bug caused by this.
    fn visit_function(&mut self, node: &Function) {
        self.with_scope(true, |visitor| {
            node.params.visit_with(visitor);
            node.body.visit_children_with(visitor);
        });
    }
    fn visit_constructor(&mut self, node: &Constructor) {
        self.with_scope(true, |visitor| {
            node.params.visit_with(visitor);
            node.body.visit_children_with(visitor);
        });
    }
    fn visit_arrow_expr(&mut self, node: &ArrowExpr) {
        self.with_scope(true, |visitor| {
            node.params.visit_with(visitor);
            node.body.visit_children_with(visitor);
        });
    }
    fn visit_getter_prop(&mut self, node: &GetterProp) {
        self.with_scope(true, |visitor| {
            node.key.visit_with(visitor);
            node.body.visit_children_with(visitor);
        });
    }
    fn visit_setter_prop(&mut self, node: &SetterProp) {
        self.with_scope(true, |visitor| {
            node.key.visit_with(visitor);
            node.param.visit_with(visitor);
            node.body.visit_children_with(visitor);
        });
    }
    // A CatchClause isn't a function, but it has a param.
    fn visit_catch_clause(&mut self, node: &CatchClause) {
        self.with_scope(false, |visitor| {
            node.param.visit_with(visitor);
            node.body.visit_children_with(visitor);
        });
    }

    fn visit_block_stmt(&mut self, node: &BlockStmt) {
        self.with_scope(false, |visitor| {
            node.stmts.visit_with(visitor);
        });
    }
}

struct Scope {
    num_declarations: u32,
    is_hoist_scope: bool,
}

struct Renamer {
    rename_map: FxHashMap<u32, JsWord>,
    var_info: FxHashMap<Id, VarInfo>,
    unresolved_ctxt: SyntaxContext,
}

impl VisitMut<'_> for Renamer {
    fn visit_mut_ident(&mut self, node: &mut Ident) {
        if node.ctxt == self.unresolved_ctxt || node.ctxt == SyntaxContext::empty() {
            // These names were skipped in the analysis and won't be renamed.
            return;
        }
        let id = node.to_id();
        if let Some(info) = self.var_info.get(&id) {
            if let Some(new_name) = self.rename_map.get(&info.slot) {
                node.sym = new_name.clone();
            }
        }
    }
}
