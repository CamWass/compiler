use ast::*;
use atoms::{js_word, JsWord};
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::{Mark, SyntaxContext};
use index::vec::IndexVec;
use rustc_hash::{FxHashMap, FxHashSet};
use std::{cmp::Ordering, collections::hash_map::Entry};

use crate::{DefaultNameGenerator::DefaultNameGenerator, Id, ToId};

#[cfg(test)]
mod tests;

/// Renames all the variables names into short names, to reduce code size and
/// also to obfuscate the code.
pub fn rename_vars(ast: &mut Program, unresolved_ctxt: SyntaxContext) {
    // TODO:
    // this.externNames = NodeUtil.collectExternVariableNames(this.compiler, externs);

    let analysis = VarAnalyzer::analyze(ast, unresolved_ctxt);

    let mut renamer = RenameVars::new(analysis);
    ast.visit_mut_with(&mut renamer);
}

/// # For testing:
/// Renames variables to display our internal representation of them. Globals
/// keep their original names, while local variables are renamed to their
/// scope-index.
#[allow(dead_code)]
fn rename_debug(ast: &mut Program, unresolved_ctxt: SyntaxContext) {
    // TODO:
    // this.externNames = NodeUtil.collectExternVariableNames(this.compiler, externs);

    let analysis = VarAnalyzer::analyze(ast, unresolved_ctxt);

    let mut rename_map = FxHashMap::default();
    for (id, slot) in analysis.slots.iter_enumerated() {
        if let SlotDebugInfo::Local(index_in_scope) = &slot.debug_info {
            let new_name = JsWord::from(format!("L{}", index_in_scope));
            rename_map.insert(id, new_name);
        } else {
            // Nothing to do for globals - they keep their original name.
        }
    }
    let mut renamer = RenameVars {
        rename_map,
        new_ctxt: SyntaxContext::empty().apply_mark(Mark::fresh(Mark::root())),
        slots: analysis.slots,
        slot_map: analysis.slot_map,
    };
    ast.visit_mut_with(&mut renamer);
}

struct RenameVars {
    rename_map: FxHashMap<SlotId, JsWord>,
    new_ctxt: SyntaxContext,
    slots: IndexVec<SlotId, Slot>,
    slot_map: FxHashMap<Id, SlotId>,
}

impl RenameVars {
    fn new(
        VarAnalysis {
            slot_map,
            slots,
            reserved_names,
            global_count,
        }: VarAnalysis,
    ) -> Self {
        // TODO:
        // Make sure that new names don't overlap with extern names.
        // reservedNames.addAll(externNames);

        let mut vars_by_frequency = slots.indices().collect::<Vec<_>>();
        vars_by_frequency.sort_unstable_by(|&a, &b| order_slots_by_frequency(&slots[a], &slots[b]));

        let mut renamer = RenameVars {
            rename_map: FxHashMap::default(),
            new_ctxt: SyntaxContext::empty().apply_mark(Mark::fresh(Mark::root())),
            slots,
            slot_map,
        };

        // Assign names to slots, sorted by descending frequency to minimize code size.
        renamer.assign_names(vars_by_frequency, reserved_names, global_count as usize);

        renamer
    }

    /// Determines new names for the slots based on frequency and source code order.
    fn assign_names(
        &mut self,
        slots_by_frequency: Vec<SlotId>,
        reserved_names: FxHashSet<JsWord>,
        global_count: usize,
    ) {
        let mut name_gen = DefaultNameGenerator::new(reserved_names);

        // Generated global names and the queue of slots to assign them to.
        let mut names_for_globals = Vec::with_capacity(global_count);
        let mut global_slots = Vec::with_capacity(global_count);

        for s in slots_by_frequency {
            // TODO:
            //   if (externNames.contains(a.oldName)) {
            //     continue;
            //   }

            if self.slots[s].local {
                // Slots for local variables are immediately assigned a name.
                let new_name = name_gen.generateAndReserveNextName();
                self.rename_map.insert(s, new_name);
            } else {
                // We delay finalizing the new names for global slots until we
                // know how many names we have of each length.
                let new_name = name_gen.generateAndReserveNextName();
                global_slots.push(s);
                names_for_globals.push(new_name);
            }
        }

        debug_assert_eq!(names_for_globals.len(), global_count);
        debug_assert_eq!(global_slots.len(), global_count);

        // Now that we have a list of global slots, and a list of new names for
        // them, we assign the new names as follows:
        // 1) The most frequent slots get the shorter names.
        // 2) If N number of slots are going to be assigned names of the same
        //    length, we assign the N names based on the order at which the slots
        //    first appear in the source. This makes the output somewhat less
        //    random, because symbols declared close together are assigned names
        //    that are quite similar. With this heuristic, the output is more
        //    compressible.
        //    For instance, the output may look like:
        //    var da = "..", ea = "..";
        //    function fa() { .. } function ga() { .. }

        let num_global_slots = names_for_globals.len();
        let mut i = 0;
        while i < num_global_slots {
            let mut slots_by_occurrence = Vec::new();

            // Add the k most frequent global slots to the set, where k is the
            // number of new names of the same length.
            let name_len = names_for_globals[i].len();
            let mut j = i;
            while j < num_global_slots && names_for_globals[j].len() == name_len {
                slots_by_occurrence.push(global_slots[j]);
                j += 1;
            }

            // Since our k new names are of the same length, assigning by
            // frequency has no effect. Instead, we assign by order of occurence.
            slots_by_occurrence.sort_unstable_by(|&a, &b| {
                order_slots_by_occurence(&self.slots[a], &self.slots[b])
            });

            // Now, finalize the assignments for the k names.
            for s in slots_by_occurrence {
                self.rename_map.insert(s, names_for_globals[i].clone());
                i += 1;
            }
        }
    }
}

impl VisitMut<'_> for RenameVars {
    fn visit_mut_ident(&mut self, node: &mut Ident) {
        let id = node.to_id();
        if let Some(slot) = self.slot_map.get(&id) {
            if let Some(new_name) = self.rename_map.get(slot) {
                node.sym = new_name.clone();
                node.span.ctxt = self.new_ctxt;
            }
        }
    }
}

index::newtype_index! {
    pub struct SlotId {
        DEBUG_FORMAT = "SlotId({})"
    }
}

/// A slot represents variables that can share the same name. Each global
/// variable gets its own slot, while all local variables at the same depth
/// share a slot. e.g.
/// ```js
/// function Foo(fa, fb) {
///   var c = function(d, e) { return fa; }
/// }
/// ```
/// The indices are: fa:0, fb:1, c:2, d:3, e:4
///
/// This way, local variable names are reused in each global function. e.g.
/// ```js
/// function x(a,b) { ... }
/// function y(a,b,c) { ... }
/// ```
#[derive(Debug)]
struct Slot {
    local: bool,
    /// The number of references to this slot.
    count: u32,
    order_of_occurrence: u32,
    /// Only for debug and testing.
    debug_info: SlotDebugInfo,
}

#[derive(Debug)]
enum SlotDebugInfo {
    /// Index in scope.
    Local(usize),
    Global,
}

#[derive(Debug)]
struct Scope {
    /// The index of the next variable to be declared in this scope.
    cur_index: u32,
    /// Whether this scope corresponds to a function scope where `var` decls can
    /// be hoisted to.
    is_hoist_scope: bool,
}

/// Information about an identifier, which may, or may not, be a variable name (e.g. it could be a property name).
#[derive(Debug)]
struct VarInfo {
    /// The number of references to this identifier.
    count: u32,
    order_of_occurrence: u32,
}

/// Visits identifiers to build a mapping from [`Ids`][Id] to [`Slots`][Slot].
#[derive(Debug)]
struct VarAnalyzer {
    /// Information about each [`Id`] we encounter
    var_info: FxHashMap<Id, VarInfo>,
    scopes: Vec<Scope>,
    /// Whether we are visiting the names in a `var` decl.
    in_var_decl: bool,
    analysis: VarAnalysis,
    /// The number of vars we've seen.
    var_count: u32,
    unresolved_ctxt: SyntaxContext,
    /// Maps the scope-index of a local to the [`SlotId`] for all locals of the
    /// same index. i.e. the [`SlotId`] at `index_to_slot_map[n]` represents all
    /// locals at depth `n`.
    index_to_slot_map: Vec<SlotId>,
}

#[derive(Default, Debug)]
struct VarAnalysis {
    slot_map: FxHashMap<Id, SlotId>,
    slots: IndexVec<SlotId, Slot>,
    reserved_names: FxHashSet<JsWord>,
    global_count: u32,
}

impl VarAnalyzer {
    fn analyze(program: &Program, unresolved_ctxt: SyntaxContext) -> VarAnalysis {
        let mut analyzer = Self {
            var_info: FxHashMap::default(),
            // Global scope:
            scopes: vec![Scope {
                cur_index: 0,
                is_hoist_scope: true,
            }],
            in_var_decl: false,
            analysis: VarAnalysis::default(),
            var_count: 0,
            unresolved_ctxt,
            index_to_slot_map: Vec::new(),
        };

        program.visit_with(&mut analyzer);

        debug_assert!(
            analyzer
                .analysis
                .slots
                .iter()
                .all(|s| s.count == 0 && s.order_of_occurrence == u32::MAX),
            "Slots should not be initialized yet"
        );

        for (name, slot) in analyzer.analysis.slot_map.iter() {
            let info = analyzer.var_info.get(name).unwrap();

            analyzer.analysis.slots[*slot].count += info.count;
            let order_of_occurrence = std::cmp::min(
                analyzer.analysis.slots[*slot].order_of_occurrence,
                info.order_of_occurrence,
            );
            analyzer.analysis.slots[*slot].order_of_occurrence = order_of_occurrence;
        }

        analyzer.analysis
    }

    /// Records a reference to an [`Id`].
    fn handle_reference(&mut self, name: &Id) {
        if name.1 == self.unresolved_ctxt {
            // Never rename references to the arguments array
            if name.0 == js_word!("arguments") {
                self.analysis.reserved_names.insert(js_word!("arguments"));
            }
            // We're only interested in tracking names that we'll see the
            // declarations for.
            return;
        }

        match self.var_info.get_mut(&name) {
            Some(info) => {
                info.count += 1;
            }
            None => {
                self.var_info.insert(
                    name.clone(),
                    VarInfo {
                        count: 1,
                        order_of_occurrence: self.var_count,
                    },
                );
                self.var_count += 1;
            }
        }
    }

    /// Records a declaration of (and reference to) an [`Id`].
    fn handle_decl(&mut self, name: Id) {
        if name.1 == self.unresolved_ctxt {
            // We're only interested in tracking names that we'll see the
            // declarations for.
            return;
        }

        self.handle_reference(&name);

        let scope_pos = if self.in_var_decl {
            // There is always the global scope (which is a hoist scope) so it's ok to unwrap.
            self.scopes.iter().rposition(|s| s.is_hoist_scope).unwrap()
        } else {
            self.scopes.len() - 1
        };
        // 0 is global scope.
        let local = scope_pos != 0;

        if let Entry::Vacant(entry_for_name) = self.analysis.slot_map.entry(name) {
            let slot_id = if local {
                // Slots for local variables are identified by their index in
                // their scope; Every var at a given depth is mapped to the same slot.

                // Skip the global scope
                let index_in_scope = self.scopes[..=scope_pos]
                    .iter()
                    .skip(1)
                    .map(|s| s.cur_index)
                    .sum::<u32>() as usize;

                let slot_for_index = match self.index_to_slot_map.get(index_in_scope) {
                    Some(slot) => *slot,
                    None => {
                        let slot_id = self.analysis.slots.push(Slot {
                            local,
                            // These will be finalized at the end of the traversal.
                            count: 0,
                            order_of_occurrence: u32::MAX,
                            debug_info: SlotDebugInfo::Local(index_in_scope),
                        });
                        self.index_to_slot_map.push(slot_id);
                        debug_assert!(self.index_to_slot_map[index_in_scope] == slot_id);
                        slot_id
                    }
                };
                slot_for_index
            } else {
                // Slots for globals are identified by their name.

                self.analysis.global_count += 1;
                let slot_id_for_global = self.analysis.slots.push(Slot {
                    local,
                    // These will be finalized at the end of the traversal.
                    count: 0,
                    order_of_occurrence: u32::MAX,
                    debug_info: SlotDebugInfo::Global,
                });
                slot_id_for_global
            };
            entry_for_name.insert(slot_id);
            self.scopes[scope_pos].cur_index += 1;
        }
    }

    /// Runs `op` with a new scope on the stack.
    fn with_scope<F>(&mut self, is_hoist_scope: bool, mut op: F)
    where
        F: FnMut(&mut Self),
    {
        self.scopes.push(Scope {
            cur_index: 0,
            is_hoist_scope,
        });
        op(self);
        self.scopes.pop();
    }
}

impl Visit<'_> for VarAnalyzer {
    fn visit_ident(&mut self, i: &Ident) {
        self.handle_reference(&i.to_id());
    }

    fn visit_binding_ident(&mut self, i: &BindingIdent) {
        self.handle_decl(i.to_id());
    }
    // The key of AssignPatProp is LHS but is an Ident, but won't be caught by
    // the BindingIdent visitor above.
    fn visit_assign_pat_prop(&mut self, node: &AssignPatProp) {
        self.handle_decl(node.key.to_id())
    }

    fn visit_var_decl(&mut self, node: &VarDecl) {
        if node.kind == VarDeclKind::Var {
            for decl in &node.decls {
                self.in_var_decl = true;
                decl.name.visit_with(self);
                self.in_var_decl = false;
                decl.init.visit_with(self);
            }
        } else {
            node.decls.visit_with(self);
        }
    }

    fn visit_expr(&mut self, node: &Expr) {
        // Destructuring patterns can contain exprs that we don't want to
        // consider a part of the var decl.
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
            node.body.visit_children_with(visitor);
        });
    }
    fn visit_setter_prop(&mut self, node: &SetterProp) {
        self.with_scope(true, |visitor| {
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

/// Sorts [`Slot`] objects by their reference count (descending), breaking ties
/// by their order of occurrence in the source (ascending) to ensure a
/// deterministic total ordering.
fn order_slots_by_frequency(a: &Slot, b: &Slot) -> Ordering {
    let result = b.count.cmp(&a.count);
    if result.is_eq() {
        // Break a tie using the order in which the variable first appears in
        // the source.
        order_slots_by_occurence(a, b)
    } else {
        result
    }
}

/// Sorts [`Slot`] objects by their order of occurrence in the source (ascending).
fn order_slots_by_occurence(a: &Slot, b: &Slot) -> Ordering {
    a.order_of_occurrence.cmp(&b.order_of_occurrence)
}
