use std::collections::hash_map::Entry;

use ast::*;
use atoms::JsWord;
use indexmap::IndexSet;
use rustc_hash::{FxHashMap, FxHashSet};
use visit::{Visit, VisitMut, VisitMutWith, VisitWith};

use crate::name_generator::NameGenerator;

#[cfg(test)]
mod tests;

pub fn process(ast: &mut Program, program_data: &mut TransformerProgramData) {
    let (rename_map, slot_map) = analyse(ast, program_data);

    // Actually assign the new names.
    let mut renamer = Renamer {
        rename_map,
        slot_map,
    };
    ast.visit_mut_with(&mut renamer);
}

fn analyse(
    ast: &Program,
    program_data: &mut TransformerProgramData,
) -> (FxHashMap<SlotId, NameId>, FxHashMap<NameId, SlotId>) {
    let mut analyser = Analyser {
        cur_scope: ScopeId(0),
        // Global scope is hoist scope.
        cur_hoist_scope: ScopeId(0),
        scopes: vec![Scope {
            names: IndexSet::default(),
            parent: None,
        }],
        in_var_decl: false,
        in_decl: false,
        reference_count: FxHashMap::default(),
        unresolved_references: FxHashSet::default(),
        order_of_occurrence: IndexSet::default(),
        program_data,
    };
    ast.visit_with(&mut analyser);

    let mut slots = FxHashMap::<SlotId, Slot>::default();
    let mut slot_map = FxHashMap::default();

    for scope in &analyser.scopes {
        let mut base_depth = 0;

        let mut cur = scope;

        while let Some(parent) = cur.parent {
            let parent = &analyser.scopes[parent.0];
            base_depth += parent.names.len();
            cur = parent;
        }

        for (i, name) in scope.names.iter().enumerate() {
            debug_assert!(!slot_map.contains_key(name));

            let slot = SlotId(base_depth + i + 1);

            slot_map.insert(name.clone(), slot);

            let reference_count = *analyser
                .reference_count
                .get(name)
                .expect("every collected var should be referenced");

            match slots.entry(slot) {
                Entry::Occupied(mut occupied_entry) => {
                    occupied_entry.get_mut().reference_count += reference_count;
                }
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(Slot {
                        reference_count: reference_count,
                        order_of_occurrence: analyser
                            .order_of_occurrence
                            .get_index_of(name)
                            .expect("every collected var should have order"),
                    });
                }
            }
        }
    }

    let mut slots: Vec<_> = slots.into_iter().collect();

    // Sort by reference count descending, breaking ties by order of occurrence
    // ascending.
    slots.sort_by(|a, b| {
        b.1.reference_count
            .cmp(&a.1.reference_count)
            .then(a.1.order_of_occurrence.cmp(&b.1.order_of_occurrence))
    });

    let slot_count = slots.len();

    let mut name_gen = NameGenerator::new(analyser.unresolved_references);
    let mut rename_map = FxHashMap::with_capacity_and_hasher(slot_count, Default::default());
    for (slot, _ref_count) in slots {
        let new_name = program_data.new_resolved_name(name_gen.generate_next_name());
        rename_map.insert(slot, new_name);
    }

    (rename_map, slot_map)
}

struct Analyser<'d> {
    cur_scope: ScopeId,
    cur_hoist_scope: ScopeId,
    scopes: Vec<Scope>,
    /// Whether we are visiting the names in a `var` decl.
    in_var_decl: bool,
    in_decl: bool,
    reference_count: FxHashMap<NameId, usize>,
    unresolved_references: FxHashSet<JsWord>,
    order_of_occurrence: IndexSet<NameId>,
    program_data: &'d mut TransformerProgramData,
}

impl Analyser<'_> {
    // TODO: this is probably picking up e.g. MemberExpr Idents.
    /// Records a reference to a [`NameId`].
    fn handle_reference(&mut self, name: NameId) {
        if name.is_unresolved() {
            self.unresolved_references
                .insert(self.program_data.get_name_text(name).clone());
            return;
        }
        *self.reference_count.entry(name.clone()).or_default() += 1;
        self.order_of_occurrence.insert(name.clone());
    }

    /// Records a declaration of a [`NameId`].
    fn handle_decl(&mut self, name: NameId) {
        // We only visit variable declarations, so the identifiers should all be
        // resolved.
        debug_assert!(!name.is_unresolved());

        let scope_pos = if self.in_var_decl {
            self.cur_hoist_scope
        } else {
            self.cur_scope
        };

        let scope = &mut self.scopes[scope_pos.0];

        scope.names.insert(name);
    }

    /// Runs `op` with a new scope on the stack.
    fn with_scope<F>(&mut self, is_hoist_scope: bool, mut op: F)
    where
        F: FnMut(&mut Self),
    {
        let next_scope_id = ScopeId(self.scopes.len());
        let prev = self.cur_scope;
        self.scopes.push(Scope {
            parent: Some(prev),
            names: IndexSet::default(),
        });
        self.cur_scope = next_scope_id;
        let prev_hoist_scope = self.cur_hoist_scope;
        if is_hoist_scope {
            self.cur_hoist_scope = next_scope_id;
        }
        op(self);
        self.cur_scope = prev;
        if is_hoist_scope {
            self.cur_hoist_scope = prev_hoist_scope;
        }
    }
}

impl Visit<'_> for Analyser<'_> {
    fn visit_ident(&mut self, i: &Ident) {
        self.handle_reference(i.name);
    }

    fn visit_binding_ident(&mut self, i: &BindingIdent) {
        if self.in_decl {
            self.handle_decl(i.id.name);
        }
        i.id.visit_with(self);
    }

    fn visit_var_declarator(&mut self, node: &VarDeclarator) {
        node.init.visit_with(self);

        let old = self.in_decl;
        self.in_decl = true;
        node.name.visit_with(self);
        self.in_decl = old;
    }
    fn visit_param(&mut self, node: &Param) {
        let old = self.in_decl;
        self.in_decl = true;
        node.visit_children_with(self);
        self.in_decl = old;
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
        let old_in_decl = self.in_decl;
        self.in_decl = false;
        let old_in_var_decl = self.in_var_decl;
        self.in_var_decl = false;
        node.visit_children_with(self);
        self.in_var_decl = old_in_var_decl;
        self.in_decl = old_in_decl;
    }

    fn visit_fn_decl(&mut self, node: &FnDecl) {
        self.handle_decl(node.ident.name);
        node.visit_children_with(self);
    }
    fn visit_class_decl(&mut self, node: &ClassDecl) {
        self.handle_decl(node.ident.name);
        node.visit_children_with(self);
    }

    fn visit_fn_expr(&mut self, node: &FnExpr) {
        self.with_scope(false, |visitor| {
            if let Some(name) = &node.ident {
                visitor.handle_decl(name.name);
            }
            node.visit_children_with(visitor);
        });
    }
    fn visit_class_expr(&mut self, node: &ClassExpr) {
        self.with_scope(false, |visitor| {
            if let Some(name) = &node.ident {
                visitor.handle_decl(name.name);
            }
            node.visit_children_with(visitor);
        });
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
            let old = visitor.in_decl;
            visitor.in_decl = true;
            node.param.visit_with(visitor);
            visitor.in_decl = old;
            node.body.visit_children_with(visitor);
        });
    }

    fn visit_block_stmt(&mut self, node: &BlockStmt) {
        self.with_scope(false, |visitor| {
            node.stmts.visit_with(visitor);
        });
    }

    fn visit_program(&mut self, node: &Program) {
        self.with_scope(true, |visitor| {
            node.visit_children_with(visitor);
        });
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct SlotId(usize);

struct Slot {
    order_of_occurrence: usize,
    reference_count: usize,
}

#[derive(Debug, Clone, Copy)]
struct ScopeId(usize);

#[derive(Debug)]
struct Scope {
    parent: Option<ScopeId>,
    names: IndexSet<NameId>,
}

struct Renamer {
    /// Slot index -> new name.
    rename_map: FxHashMap<SlotId, NameId>,
    slot_map: FxHashMap<NameId, SlotId>,
}

impl VisitMut<'_> for Renamer {
    fn visit_mut_ident(&mut self, node: &mut Ident) {
        if node.name.is_unresolved() {
            // These names were skipped in the analysis and won't be renamed.
            debug_assert!(!self.slot_map.contains_key(&node.name));
            return;
        }
        if let Some(slot) = self.slot_map.get(&node.name) {
            let new_name = self
                .rename_map
                .get(slot)
                .expect("all slots should have new names")
                .clone();

            node.name = new_name;
        }
    }
}
