use std::convert::TryInto;

use ast::*;
use atoms::{js_word, JsWord};
use global_common::SyntaxContext;
use index::bit_set::GrowableBitSet;
use index::vec::IndexVec;
use rustc_hash::FxHashMap;

use crate::control_flow::node::{Node, NodeKind};
use crate::control_flow::ControlFlowGraph::*;
use crate::find_vars::VarId;
use crate::utils::unwrap_as;

use super::simple_set::IndexSet;
use super::{is_simple_prop_name, FnId, Id, NameId, PropKey, SimpleCFG, StaticFunctionData};

/// Controls whether optimisations are applied to the generated steps. Should
/// be enabled by default, but disabling may be useful for debugging.
const OPTIMISE: bool = true;

/// Builds a map containing the [`Steps`][Step] required to symbolically evaluate
/// each node in the given function.
pub(super) fn create_step_map(
    static_fn_data: &IndexVec<FnId, StaticFunctionData>,
    unresolved_ctxt: SyntaxContext,
    func: FnId,
    function_map: &FxHashMap<NodeId, FnId>,
    names: &mut IndexSet<NameId, JsWord>,
    vars: &mut IndexSet<VarId, Id>,
    builder: &mut StepBuilder,
) -> (Vec<Step>, Vec<(u32, u32)>, bool) {
    let cfg = &static_fn_data[func].cfg;
    let graph = &cfg.graph;

    let mut steps = Vec::new();
    let mut map: Vec<(u32, u32)> = vec![(0, 0); graph.node_count()];

    let mut references_env_vars = false;

    // TODO: skip unreachable nodes?
    for node in graph.node_indices() {
        if node == cfg.implicit_return_index {
            continue;
        }

        // Make assignments conditional if the node can end abruptly by an exception.
        let conditional = graph.edges(node).any(|e| *e.weight() == Branch::ON_EX);

        builder.reset();

        let mut analyser = Analyser {
            step_builder: builder,
            cfg: &static_fn_data[func].cfg,
            unresolved_ctxt,
            function_map,
            names,
            vars,
            references_env_vars: &mut references_env_vars,
            param_end: static_fn_data[func].param_end,
        };
        analyser.init(graph[node], conditional);

        if OPTIMISE {
            builder.optimise();
        }

        map[node.index()] = builder.build(&mut steps);
    }

    (steps, map, references_env_vars)
}

#[derive(Debug)]
pub(super) struct StepBuilder {
    // Steps for the current node.
    step_buffer: Vec<Step>,

    /// Used by `push` method.
    /// The vec is empty when the current RValue is unknown.
    cur_builder_r_value: Vec<Option<RValue>>,
    /// Used by `push` method.
    cur_builder_l_value: Option<LValue>,

    // Used for optimisations:
    /// Stack of `(value, save_pos, overwritten)`.
    /// Used to remove useless SaveRValue/RestoreRValue steps.
    r_value_store_stack: Vec<(Option<Option<RValue>>, usize, bool)>,

    /// Set of indices of steps that will be removed.
    indices_to_remove: GrowableBitSet<usize>,

    l_value_stores_to_set_to_none: Vec<usize>,
}

impl StepBuilder {
    pub fn new() -> Self {
        Self {
            step_buffer: Vec::new(),
            r_value_store_stack: Vec::new(),
            indices_to_remove: GrowableBitSet::new_empty(),
            l_value_stores_to_set_to_none: Vec::new(),
            // These should be the same as Machine's initial values.
            cur_builder_r_value: vec![None],
            cur_builder_l_value: None,
        }
    }

    /// Resets the state of the builder, allowing it to be used again.
    fn reset(&mut self) {
        let Self {
            step_buffer,
            r_value_store_stack,
            indices_to_remove,
            cur_builder_r_value,
            cur_builder_l_value,
            l_value_stores_to_set_to_none,
        } = self;
        step_buffer.clear();
        r_value_store_stack.clear();
        indices_to_remove.clear();
        // optimise should clear this by itself.
        debug_assert!(l_value_stores_to_set_to_none.is_empty());
        // These have special initial states.
        cur_builder_r_value.clear();
        cur_builder_r_value.push(None);
        *cur_builder_l_value = None;
    }

    /// Appends the builder's steps to `steps` and returns the `(start, end)`
    /// indices of the new steps.
    fn build(&mut self, steps: &mut Vec<Step>) -> (u32, u32) {
        let start = steps.len().try_into().unwrap();
        steps.append(&mut self.step_buffer);
        let end = steps.len().try_into().unwrap();
        (start, end)
    }

    /// Records the given step.
    fn push(&mut self, step: Step) {
        if OPTIMISE {
            // Remove any immediately previous StoreRValue steps whose RValue is
            // superseded by the new step. We use a loop in case there are
            // multiple steps to remove e.g. if the previous steps stored a
            // property of a variable.
            if let Step::StoreRValue(new_value) = &step {
                while let Some(previous) = self.step_buffer.last() {
                    if let Step::StoreRValue(_) = previous {
                        // If the new value does not depend on the previous one, the old
                        // one is dead and can be replaced.
                        let overwrite = match new_value {
                            Some(v) => !v.is_relative(),
                            None => true,
                        };
                        if overwrite {
                            // Remove the step.
                            let s = self.step_buffer.pop();
                            debug_assert!(s.is_some());
                            // Remove its RValue from the stack.
                            let v = self.cur_builder_r_value.pop();
                            debug_assert!(v.is_some());
                            continue;
                        }
                    }
                    break;
                }
            }

            // Don't record a step if it's equivalent to the previous one.
            if let Some(previous) = self.step_buffer.last() {
                if steps_are_duplicates(&step, previous) {
                    return;
                }
            }

            // Remove steps based on tracked register values.
            match step {
                Step::RestoreRValue => {
                    if let Some(Step::SaveRValue) = self.step_buffer.last() {
                        // RestoreRValue immediately after SaveRValue means they
                        // are both useless.

                        self.step_buffer.pop(); // remove SaveRValue
                        return;
                    }
                    // We don't track the saved/restored values in this function,
                    // so the current RValue is now unknown.
                    self.cur_builder_r_value.clear();
                }
                Step::StoreLValue(new) => {
                    // Don't record StoreLValue if stores the same LValue as the
                    // previous one. LValues do not refer to each other (not
                    // relative), so simple equality works here.
                    if new == self.cur_builder_l_value {
                        return;
                    }
                    // All properties of an invalid pointer are invalid.
                    if Some(&None) == self.cur_builder_r_value.last() {
                        if let Some(LValue::RValueProp(_)) = new {
                            if self.cur_builder_l_value != None {
                                self.step_buffer.push(Step::StoreLValue(None));
                            }
                            return;
                        }
                    }
                    self.cur_builder_l_value = new;
                }
                // These create dynamic RValues we can't/don't track, so the
                // current RValue is now unknown.
                Step::StoreUnion | Step::Call => {
                    self.cur_builder_r_value.clear();
                }
                Step::StoreRValue(new) => {
                    // Don't record StoreRValue if stores the same RValue as the
                    // previous one. RValues can evaluate to different values at
                    // different points in the program e.g. if they access a
                    // variable. These non-static RValues cannot be considered
                    // equivalent for this optimisation.
                    if Some(&new) == self.cur_builder_r_value.last() {
                        if new.is_none() || new.unwrap().is_static() {
                            // Static; remove
                            return;
                        }
                        // Non-static; keep
                    }
                    // All properties of an invalid pointer are invalid.
                    if Some(&None) == self.cur_builder_r_value.last() {
                        if let Some(RValue::Prop(_)) = new {
                            return;
                        }
                    }
                    self.cur_builder_r_value.push(new);
                }
                // Invalidating or assigning to an unknown LValue has no effect.
                Step::Assign(_) | Step::InvalidateLValue => {
                    if self.cur_builder_l_value.is_none() {
                        return;
                    }
                }
                // Invalidating an unknown/primitive RValue has no effect.
                Step::InvalidateRValue => {
                    if let Some(r_value) = self.cur_builder_r_value.last() {
                        let is_primitive = r_value.is_none()
                            || match r_value.unwrap() {
                                RValue::Var(_)
                                | RValue::Object(_)
                                | RValue::Prop(_)
                                | RValue::Fn(_) => false,

                                RValue::NullOrVoid
                                | RValue::String
                                | RValue::Boolean
                                | RValue::Number
                                | RValue::BigInt => true,
                            };
                        if is_primitive {
                            return;
                        }
                    } else {
                        return;
                    }
                }
                Step::StoreArg
                | Step::Return
                | Step::StartUnion
                | Step::PushToUnion
                | Step::SaveRValue
                | Step::StartCall(_) => {}
            }
        }

        self.step_buffer.push(step);
    }

    /// Optimises the builder's steps.
    fn optimise(&mut self) {
        debug_assert!(OPTIMISE);

        if !self.step_buffer.is_empty() {
            // Start of the step list. All steps before this have been removed.
            let mut start = 0;
            // End of the step list. All steps after this have been removed.
            let mut end = self.step_buffer.len();
            let mut removed_count = 0;
            let mut changed = true;

            /// Removes the step at `pos`.
            macro_rules! remove_step {
                ($pos:expr, $debug_msg:literal, $($p:pat)|+) => {
                    debug_assert!(matches!(self.step_buffer[$pos], $($p)|+));
                    remove_step(
                        $pos,
                        &mut start,
                        &mut end,
                        &mut removed_count,
                        &mut self.indices_to_remove,
                        &self.step_buffer,
                        $debug_msg,
                        stringify!($($p)|+),
                    );
                };
            }

            while changed && removed_count != self.step_buffer.len() {
                changed = false;

                // Backwards pass. Primarily removes steps whose effects are
                // superseded by future steps before they are observed.
                if removed_count != self.step_buffer.len() {
                    // True after an RValue store. False after an RValue read.
                    let mut remove_r_stores = true;
                    // True after an LValue store. False after an LValue read.
                    let mut remove_l_stores = true;

                    let iter_start = start;
                    for (pos, step) in self.step_buffer[start..end].iter().enumerate().rev() {
                        let pos = pos + iter_start;
                        if self.indices_to_remove.contains(pos) {
                            continue;
                        }
                        let last = pos == end - 1;
                        match step {
                            Step::RestoreRValue => {
                                // RestoreRValue is effectively a StoreRValue. We can
                                // remove it (and the corresponding SaveRValue) if...
                                if last {
                                    // .. it's the last step, in which case its
                                    // value is never read. Or...
                                    let save_pos = self.find_r_value_save(pos, start);
                                    remove_step!(pos, "last RestoreRValue", Step::RestoreRValue);
                                    remove_step!(save_pos, "last RestoreRValue", Step::SaveRValue);
                                } else if remove_r_stores {
                                    // ... we are removing RValue stores, in which case
                                    // its value will be overwritten before its read.
                                    let save_pos = self.find_r_value_save(pos, start);
                                    changed = true;
                                    remove_step!(
                                        pos,
                                        "remove_stores RestoreRValue",
                                        Step::RestoreRValue
                                    );
                                    remove_step!(
                                        save_pos,
                                        "remove_stores RestoreRValue",
                                        Step::SaveRValue
                                    );
                                } else {
                                    // Can't remove the save/restore, but this means
                                    // that RValue stores between them will be
                                    // overwritten and can be removed.
                                    remove_r_stores = true;
                                }
                            }
                            Step::SaveRValue => {
                                // If a SaveRValue has't been removed above, that
                                // means the value it saves is read and so we want
                                // to keep the stores whose RValue is saved.
                                remove_r_stores = false;
                            }
                            Step::StoreLValue(v) => {
                                // We can remove a StoreLValue if...
                                if last {
                                    // .. it's the last step, in which case its
                                    // value is never read. Or...
                                    remove_step!(pos, "last", Step::StoreLValue(_));
                                } else if remove_l_stores {
                                    // ... we are removing LValue stores, in which case
                                    // its value will be overwritten before its read.
                                    changed = true;
                                    remove_step!(pos, "remove_l_stores", Step::StoreLValue(_));
                                } else {
                                    // Can't remove this StoreLValue, but that means it
                                    // overwrites previous ones, which can be removed.
                                    remove_l_stores = true;
                                    // If the LValue depends on previous RValues, make
                                    // sure we don't remove them.
                                    if v.is_none() || !v.unwrap().depends_on_r_value() {
                                        // Non-relative - does not rely on RValue.
                                    } else {
                                        // Depends on RValue.
                                        remove_r_stores = false;
                                    }
                                }
                            }
                            Step::StoreRValue(v) => {
                                // We can remove a StoreRValue if...
                                if last {
                                    // .. it's the last step, in which case its
                                    // value is never read. Or...
                                    remove_step!(pos, "last", Step::StoreRValue(_));
                                } else if remove_r_stores {
                                    // ... we are removing RValue stores, in which case
                                    // its value will be overwritten before its read.
                                    changed = true;
                                    remove_step!(pos, "remove_stores", Step::StoreRValue(_));
                                } else {
                                    // Can't remove this StoreRValue. If it doesn't depend
                                    // on previous RValues, then it overwrites them and the
                                    // can be removed.
                                    let relative = match v {
                                        Some(v) => v.is_relative(),
                                        None => false,
                                    };
                                    if !relative {
                                        remove_r_stores = true;
                                    }
                                }
                            }

                            Step::StartUnion => {}

                            Step::StoreUnion => {
                                // StoreUnion is an RValue store, so it can be
                                // removed if its unread or overwritten.
                                if !(last || remove_r_stores) {
                                    remove_r_stores = false;
                                    continue;
                                }

                                // Dead StoreUnion. Need to find the corresponding
                                // StartUnion and PushToUnions. We keep track of
                                // the current union that any future PushToUnion's
                                // correspond to by recording when unions "open"
                                // (StoreUnion since we iterate backwards) and
                                // "close" (StartUnion). The count starts at 1.
                                // Any PushToUnion we encounter while count is 1
                                // needs to be removed. Every StoreUnion increments
                                // the count and every StartUnion decrements it.
                                // When the count is 0, we have found the closing
                                // StoreUnion for our union and can stop.

                                let mut open_unions: u32 = 1;
                                let mut record_union_pushes = true;

                                let iter_start = start;
                                for (union_pos, step) in
                                    self.step_buffer[start..pos].iter().enumerate().rev()
                                {
                                    let union_pos = union_pos + iter_start;
                                    if self.indices_to_remove.contains(union_pos) {
                                        continue;
                                    }
                                    match step {
                                        Step::StartUnion => {
                                            open_unions -= 1;
                                            if open_unions == 0 {
                                                // Found the closing StoreUnion for our
                                                // union; stop
                                                remove_step!(
                                                    union_pos,
                                                    "closing",
                                                    Step::StartUnion
                                                );
                                                break;
                                            } else if open_unions == 1 {
                                                // Re-entered target union; resume recording.
                                                record_union_pushes = true;
                                            }
                                        }
                                        Step::PushToUnion => {
                                            // Record pushes so they can be removed.
                                            if record_union_pushes {
                                                remove_step!(
                                                    union_pos,
                                                    "intermediary",
                                                    Step::PushToUnion
                                                );
                                            }
                                        }
                                        Step::StoreUnion => {
                                            // Entered a union other than the target.
                                            // Don't record any pushes as they don't
                                            // correspond to our union and may not be dead.
                                            record_union_pushes = false;
                                            open_unions += 1;
                                        }
                                        _ => {}
                                    }
                                }
                                debug_assert!(open_unions == 0);
                                debug_assert!(record_union_pushes == true);

                                // Remove original StoreUnion step.
                                changed = true;
                                remove_step!(pos, "opening", Step::StoreUnion);
                            }

                            Step::Call => {
                                // Call is an RValue store, so overwrites any
                                // previous ones.
                                remove_r_stores = true;
                            }

                            Step::Assign(_) => {
                                // Assigns read the RValue and LValue, so we
                                // can't remove their stores.
                                remove_r_stores = false;
                                remove_l_stores = false;
                            }
                            Step::InvalidateLValue => {
                                // Reads the LValue, so we can't remove LValue stores.
                                remove_l_stores = false;
                            }

                            // These read the RValue, so we can't remove RValue stores.
                            Step::InvalidateRValue
                            | Step::Return
                            | Step::StartCall(_)
                            | Step::StoreArg
                            | Step::PushToUnion => {
                                remove_r_stores = false;
                            }
                        }
                    }
                }

                // Forwards pass. Primarily removes steps that don't have an
                // observed effect on the tracked registers.
                if removed_count != self.step_buffer.len() {
                    debug_assert!(self.l_value_stores_to_set_to_none.is_empty());

                    let mut cur_step_l_value = None;
                    // None signifies a dynamic value such as a union or call result.
                    let mut cur_step_r_value: Option<Option<RValue>> = Some(None);
                    self.r_value_store_stack.clear();

                    let iter_start = start;
                    for (pos, step) in self.step_buffer[start..end].iter().enumerate() {
                        let pos = pos + iter_start;
                        if self.indices_to_remove.contains(pos) {
                            continue;
                        }
                        let first = pos == start;
                        match step {
                            Step::SaveRValue => {
                                // Record the saved value and the position of the save.
                                let value = match cur_step_r_value {
                                    Some(Some(v)) if !v.is_static() => None,
                                    _ => cur_step_r_value,
                                };
                                self.r_value_store_stack.push((value, pos, false));
                            }
                            Step::StoreLValue(new) => {
                                // Remove StoreLValues that store the same LValue as the
                                // previous one. LValues do not refer to each other (not
                                // relative), so simple equality works here.
                                if *new == cur_step_l_value {
                                    if !first {
                                        changed = true;
                                    }
                                    remove_step!(pos, "forward", Step::StoreLValue(_));
                                    continue;
                                }
                                // All properties of an invalid pointer are invalid.
                                if Some(None) == cur_step_r_value {
                                    if let Some(LValue::RValueProp(_)) = new {
                                        if cur_step_l_value == None {
                                            if !first {
                                                changed = true;
                                            }
                                            remove_step!(pos, "forward prop", Step::StoreLValue(_));
                                        } else {
                                            self.l_value_stores_to_set_to_none.push(pos);
                                        }
                                        continue;
                                    }
                                }
                                cur_step_l_value = *new;
                            }
                            Step::RestoreRValue => {
                                // If the restored value is the same as the current RValue,
                                // then the save/restore is useless and they can be removed.
                                let (saved, store_pos, overwritten) =
                                    self.r_value_store_stack.pop().unwrap();

                                // The RestoreRValue/SaveRValue can be removed if...
                                if !overwritten {
                                    // ...the RValue is not modified between them. Or...
                                    changed = true;
                                    remove_step!(
                                        pos,
                                        "unmodified RestoreRValue",
                                        Step::RestoreRValue
                                    );
                                    remove_step!(
                                        store_pos,
                                        "unmodified RestoreRValue",
                                        Step::SaveRValue
                                    );
                                    cur_step_r_value = saved;
                                } else if saved.is_some() && saved == cur_step_r_value {
                                    // ...the modified value is equivalent to the saved value.
                                    debug_assert!(
                                        saved.unwrap().is_none()
                                            || saved.unwrap().unwrap().is_static()
                                    );
                                    changed = true;
                                    remove_step!(pos, "equal RestoreRValue", Step::RestoreRValue);
                                    remove_step!(
                                        store_pos,
                                        "equal RestoreRValue",
                                        Step::SaveRValue
                                    );
                                } else {
                                    if let Some((_, _, overwritten)) =
                                        self.r_value_store_stack.last_mut()
                                    {
                                        *overwritten = true;
                                    }
                                    cur_step_r_value = saved;
                                }
                            }
                            // These create dynamic RValues we can't/don't track.
                            Step::StoreUnion | Step::Call => {
                                if let Some((_, _, overwritten)) =
                                    self.r_value_store_stack.last_mut()
                                {
                                    *overwritten = true;
                                }
                                cur_step_r_value = None;
                            }
                            Step::StoreRValue(new) => {
                                // Remove StoreRValues that store the same RValue as the
                                // previous one. RValues can evaluate to different values
                                // at different points in the program e.g. if they access
                                // a variable. These non-static RValues cannot be
                                // considered equivalent for this optimisation.
                                if Some(*new) == cur_step_r_value {
                                    if new.is_none() || new.unwrap().is_static() {
                                        // Static; remove
                                        if !first {
                                            changed = true;
                                        }
                                        remove_step!(pos, "forward", Step::StoreRValue(_));
                                    }
                                    // Non-static; keep
                                } else if Some(None) == cur_step_r_value {
                                    // All properties of an invalid pointer are invalid.
                                    if let Some(RValue::Prop(_)) = new {
                                        if !first {
                                            changed = true;
                                        }
                                        remove_step!(pos, "forward prop", Step::StoreRValue(_));
                                    } else {
                                        cur_step_r_value = Some(*new);
                                    }
                                } else {
                                    cur_step_r_value = Some(*new);
                                }
                                if let Some((_, _, overwritten)) =
                                    self.r_value_store_stack.last_mut()
                                {
                                    *overwritten = true;
                                }
                            }
                            // Invalidating or assigning to an unknown LValue has no effect.
                            Step::Assign(_) | Step::InvalidateLValue => {
                                if cur_step_l_value.is_none() {
                                    if !first {
                                        changed = true;
                                    }
                                    remove_step!(
                                        pos,
                                        "forward",
                                        Step::Assign(_) | Step::InvalidateLValue
                                    );
                                }
                            }
                            // Invalidating an unknown/primitive RValue has no effect.
                            Step::InvalidateRValue => {
                                let mut can_invalidate = true;
                                if let Some(r_value) = cur_step_r_value {
                                    can_invalidate = r_value.is_some()
                                        && match r_value.unwrap() {
                                            RValue::Var(_)
                                            | RValue::Object(_)
                                            | RValue::Prop(_)
                                            | RValue::Fn(_) => true,

                                            RValue::NullOrVoid
                                            | RValue::String
                                            | RValue::Boolean
                                            | RValue::Number
                                            | RValue::BigInt => false,
                                        };
                                }
                                if !can_invalidate {
                                    if !first {
                                        changed = true;
                                    }
                                    remove_step!(pos, "forward", Step::InvalidateRValue);
                                }
                            }

                            // Remove unions that only store one constituent. The value
                            // might come from explicit stores between the StartUnion and
                            // StoreUnion, or it may implicitly come from the current RValue.
                            // e.g.
                            // implicit: [StartUnion, PushToUnion, StoreUnion]
                            // explicit: [StartUnion, StoreRValue, PushToUnion, StoreUnion]
                            Step::StartUnion => {
                                let iter_start = pos + 1;
                                let next = self.step_buffer[iter_start..end]
                                    .iter()
                                    .enumerate()
                                    .map(|(p, s)| (p + iter_start, s))
                                    .filter(|(p, _)| !self.indices_to_remove.contains(*p));

                                // Skip over any explicit stores.
                                let mut next =
                                    next.skip_while(|(_, s)| matches!(s, Step::StoreRValue(_)));

                                // We already matched the StartUnion and any explicit stores.
                                // Now look for the closing PushToUnion and StoreUnion.
                                if let Some((push_pos, Step::PushToUnion)) = next.next() {
                                    if let Some((store_pos, Step::StoreUnion)) = next.next() {
                                        changed = true;
                                        remove_step!(pos, "forward StartUnion", Step::StartUnion);
                                        remove_step!(
                                            push_pos,
                                            "forward PushToUnion",
                                            Step::PushToUnion
                                        );
                                        remove_step!(
                                            store_pos,
                                            "forward StoreUnion",
                                            Step::StoreUnion
                                        );
                                    }
                                }
                            }

                            Step::StoreArg
                            | Step::Return
                            | Step::PushToUnion
                            | Step::StartCall(_) => {}
                        }
                    }

                    for pos in &self.l_value_stores_to_set_to_none {
                        let v = unwrap_as!(&mut self.step_buffer[*pos], Step::StoreLValue(v), v);
                        *v = None;
                    }
                    self.l_value_stores_to_set_to_none.clear();
                }

                // Remove consecutive steps that are duplicates.
                if self.step_buffer.len() - removed_count > 1 {
                    debug_assert!(!self.indices_to_remove.contains(start));
                    let mut cur = &self.step_buffer[start];
                    let iter_start = start + 1;
                    for (pos, step) in self.step_buffer[iter_start..end].iter().enumerate() {
                        let pos = pos + iter_start;
                        if self.indices_to_remove.contains(pos) {
                            continue;
                        }
                        if steps_are_duplicates(cur, step) {
                            changed = true;
                            remove_step!(pos, "dedup", _);
                        } else {
                            cur = step;
                        }
                    }
                }
            }
        }

        // Remove useless steps identified by the analysis.
        let mut pos = 0;
        let indices_to_remove = &self.indices_to_remove;
        self.step_buffer.retain(|_| {
            let remove = indices_to_remove.contains(pos);
            pos += 1;
            !remove
        });

        debug_assert!(self
            .step_buffer
            .windows(2)
            .all(|w| !steps_are_duplicates(&w[0], &w[1])))
    }

    /// Returns the position in `step_buffer` of the [`SaveRValue`][Step::SaveRValue]
    /// that corresponds to the [`RestoreRValue`][Step::RestoreRValue] at
    /// `restore_pos`. Note: this scans backwards from `restore_pos`, so `end`
    /// should to the left (<=) of `restore_pos`.
    fn find_r_value_save(&self, restore_pos: usize, end: usize) -> usize {
        let mut open_saves: u32 = 1;
        for (pos, step) in self.step_buffer[end..restore_pos].iter().enumerate().rev() {
            let pos = pos + end;
            if self.indices_to_remove.contains(pos) {
                continue;
            }
            match step {
                Step::SaveRValue => {
                    open_saves -= 1;
                    if open_saves == 0 {
                        // Found corresponding SaveRValue.
                        return pos;
                    }
                }
                Step::RestoreRValue => {
                    open_saves += 1;
                }
                _ => {}
            }
        }

        // Every RestoreRValue should have a corresponding SaveRValue.
        unreachable!("No matching SaveRValue found");
    }
}

/// Removes the step at `pos` from `step_buffer`.
fn remove_step(
    pos: usize,
    start: &mut usize,
    end: &mut usize,
    removed_count: &mut usize,
    indices_to_remove: &mut GrowableBitSet<usize>,
    step_buffer: &Vec<Step>,
    msg: &'static str,
    step_pattern: &'static str,
) {
    const DEBUG: bool = false;
    if DEBUG {
        println!(
            "removing step {:#?} at pos {} due to: '{} {}'",
            &step_buffer[pos], pos, msg, step_pattern
        );
    }

    debug_assert!(!indices_to_remove.contains(pos));

    *removed_count += 1;
    indices_to_remove.insert(pos);

    let first = pos == *start;
    let last = pos == *end - 1;

    // If the removed step was at the ends of the buffer, adjust the bounds so
    // we don't visit it again.
    if last {
        while *end > *start && indices_to_remove.contains(*end) {
            *end = end.saturating_sub(1);
        }
    } else if first {
        while *end > *start && indices_to_remove.contains(*start) {
            *start += 1;
        }
    }
}

/// Returns `true` if the consecutive steps `a` and `b` are duplicates and can be merged.
fn steps_are_duplicates(a: &Step, b: &Step) -> bool {
    a == b
        && match a {
            // Only non-relative register stores are idempotent and can be merged.
            Step::StoreRValue(v) => v.is_none() || !v.unwrap().is_relative(),
            // LValues cannot depend on each other.
            Step::StoreLValue(_) => true,

            // These are idempotent and can be merged.
            Step::Assign(_)
            | Step::InvalidateRValue
            | Step::InvalidateLValue
            | Step::Return
            | Step::PushToUnion => true,

            // These have side effects and cannot be merged.
            Step::StartCall(_)
            | Step::StoreArg
            | Step::Call
            | Step::StartUnion
            | Step::StoreUnion
            | Step::SaveRValue
            | Step::RestoreRValue => false,
        }
}

pub(super) static IMPLICIT_RETURN_STEPS: [Step; 2] =
    [Step::StoreRValue(Some(RValue::NullOrVoid)), Step::Return];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// A place where a value can be stored.
pub(super) enum LValue {
    /// Named variable.
    Var(VarId),
    /// Property of the current RValue.
    RValueProp(NameId),
    /// Property of the object type that represents the given object literal.
    ObjectProp(NodeId, NameId),
}

impl LValue {
    /// Returns true if this LValue depends on the current RValue.
    fn depends_on_r_value(&self) -> bool {
        match self {
            LValue::RValueProp(_) => true,
            LValue::Var(_) | LValue::ObjectProp(_, _) => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// The value of an expression.
pub(super) enum RValue {
    NullOrVoid,
    /// Pointer to the value stored in the given named variable.
    Var(VarId),
    /// Object type that represents the given object literal.
    Object(NodeId),
    /// Property of the current RValue.
    Prop(NameId),
    String,
    Boolean,
    Number,
    BigInt,
    Fn(FnId),
}

impl RValue {
    /// Returns true if this RValue depends on previous steps.
    fn is_relative(&self) -> bool {
        match self {
            RValue::Prop(_) => true,
            RValue::NullOrVoid
            | RValue::Var(_)
            | RValue::Object(_)
            | RValue::String
            | RValue::Boolean
            | RValue::Number
            | RValue::BigInt
            | RValue::Fn(_) => false,
        }
    }
    /// Returns true if the RValue is static while evaluating a node (i.e. its
    /// value does not change).
    fn is_static(&self) -> bool {
        match self {
            RValue::NullOrVoid
            | RValue::Object(_)
            | RValue::String
            | RValue::Boolean
            | RValue::Number
            | RValue::BigInt
            | RValue::Fn(_) => true,

            RValue::Var(_) | RValue::Prop(_) => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// An abstract instruction representing part of a a JavaScript program.
/// Each instruction is largely stateless, and depends on the state created by
/// the previous steps.
pub(super) enum Step {
    /// Stores the given value in the RValue register.
    StoreRValue(Option<RValue>),
    /// Stores the given value in the LValue register.
    StoreLValue(Option<LValue>),
    /// Assigns the value in the RValue register to the value in the LValue register.
    Assign(bool),
    /// Invalidates the value in the RValue register.
    InvalidateRValue,
    /// Invalidates the value in the LValue register.
    InvalidateLValue,
    /// Begins a new call, with the specified number of arguments, by pushing it
    /// onto the call creation stack.
    StartCall(usize),
    /// Stores the value in the RValue register as the next argument to the current call.
    StoreArg,
    /// Executes a call, popping it from the stack and storing the result in the
    /// RValue register.
    Call,
    /// Returns the value in the RValue register. Execution will stop after this.
    Return,
    /// Creates a new union, pushing it onto the union creation stack.
    StartUnion,
    /// Adds the value in the RValue register to the current union.
    PushToUnion,
    /// Finalises the current union, popping it from the stack and storing in
    /// the the RValue register.
    StoreUnion,
    /// Pushes a new RValue register onto the RValue stack. The current RValue
    /// is copied into the new register. Used to save values before visiting
    /// nested expressions.
    SaveRValue,
    /// Pops the current RValue register from the stack, restoring the previous
    /// RValue register and its value.
    RestoreRValue,
}

#[derive(Debug)]
struct Analyser<'a, 'ast> {
    step_builder: &'a mut StepBuilder,
    cfg: &'a SimpleCFG<'ast>,
    unresolved_ctxt: SyntaxContext,
    function_map: &'a FxHashMap<NodeId, FnId>,
    names: &'a mut IndexSet<NameId, JsWord>,
    vars: &'a mut IndexSet<VarId, Id>,
    references_env_vars: &'a mut bool,
    param_end: u32,
}

impl<'ast> Analyser<'_, 'ast> {
    /// Records the given step.
    fn push(&mut self, step: Step) {
        self.step_builder.push(step);
    }

    /// Invalidate the RValue register.
    fn invalidate_r_value(&mut self) {
        self.push(Step::InvalidateRValue);
    }

    /// Invalidate the LValue register.
    fn invalidate_l_value(&mut self) {
        self.push(Step::InvalidateLValue);
    }

    fn get_var_id_from_ident(&mut self, ident: &Ident) -> Option<VarId> {
        if ident.span.ctxt == self.unresolved_ctxt {
            None
        } else {
            let name = self.names.get_index(&ident.sym).unwrap();
            let id = Id(name, ident.span.ctxt);
            let id = self.vars.get_index(&id).unwrap();
            if id.as_u32() < self.param_end {
                *self.references_env_vars = true;
            }
            Some(id)
        }
    }

    fn record_assignment(
        &mut self,
        lhs: Node<'ast>,
        rhs: &'ast Expr,
        conditional: bool,
        op: AssignOp,
    ) {
        let conditional_assign = matches!(
            op,
            AssignOp::AndAssign | AssignOp::OrAssign | AssignOp::NullishAssign
        );
        match lhs.kind {
            NodeKind::ArrayPat(_) | NodeKind::ObjectPat(_) => {
                debug_assert!(!conditional_assign, "invalid assignment target");
                self.handle_destructuring(lhs, rhs, conditional)
            }
            _ => {
                self.visit_and_get_r_value(Node::from(rhs), conditional);

                // TODO: skip SaveRValue/RestoreRValue for simple LHS
                self.push(Step::SaveRValue);
                let conditional = conditional || conditional_assign;
                self.visit_and_get_slot(lhs, conditional);
                self.push(Step::RestoreRValue);

                self.push(Step::Assign(conditional));
            }
        }
    }

    fn handle_destructuring(&mut self, lhs: Node<'ast>, rhs: &'ast Expr, conditional: bool) {
        self.visit_and_get_r_value(Node::from(rhs), conditional);
        self.visit_destructuring(lhs, conditional);
    }

    fn visit_destructuring(&mut self, lhs: Node<'ast>, conditional: bool) {
        match lhs.kind {
            NodeKind::ObjectPat(lhs) => {
                let has_complex_props = lhs.props.iter().any(|p| match p {
                    ObjectPatProp::KeyValue(p) => {
                        !is_simple_prop_name(&p.key, self.unresolved_ctxt)
                    }
                    ObjectPatProp::Assign(_) | ObjectPatProp::Rest(_) => false,
                });
                if has_complex_props {
                    self.invalidate_r_value();
                    for prop in &lhs.props {
                        self.visit_destructuring(Node::from(prop), conditional);
                    }
                } else {
                    for prop in &lhs.props {
                        match prop {
                            ObjectPatProp::KeyValue(prop) => {
                                let key = PropKey::from_prop_name(
                                    &prop.key,
                                    self.unresolved_ctxt,
                                    &mut self.names,
                                )
                                .unwrap();

                                if matches!(prop.value.as_ref(), Pat::Ident(_) | Pat::Expr(_)) {
                                    self.push(Step::SaveRValue);
                                    self.visit_and_get_slot(
                                        Node::from(prop.value.as_ref()),
                                        conditional,
                                    );
                                    self.push(Step::RestoreRValue);
                                    self.push(Step::SaveRValue);
                                    self.push(Step::StoreRValue(Some(RValue::Prop(key.0))));
                                    self.push(Step::Assign(conditional));
                                    self.push(Step::RestoreRValue);
                                } else {
                                    self.push(Step::SaveRValue);
                                    self.push(Step::StoreRValue(Some(RValue::Prop(key.0))));
                                    self.visit_destructuring(
                                        Node::from(prop.value.as_ref()),
                                        conditional,
                                    );
                                    self.push(Step::RestoreRValue);
                                }
                            }
                            ObjectPatProp::Assign(_) => {
                                unreachable!("removed by normalization");
                            }
                            ObjectPatProp::Rest(rest) => {
                                debug_assert!(lhs.props.last().unwrap() == prop);

                                // TODO: throw error, don't panic.
                                // The argument of an object pattern's rest element must be an identifier.
                                let arg = unwrap_as!(rest.arg.as_ref(), Pat::Ident(i), i);

                                // TODO: this is imprecise - rest patterns create a new, distinct, object, which has the remaining non-destructured
                                // properties copied over. These properties must have the same names as those in the original object after renaming.
                                // But since they are distinct objects, we don't want to conflate them.
                                let value = self.get_var_id_from_ident(&arg.id).map(LValue::Var);
                                self.push(Step::StoreLValue(value));
                                self.push(Step::Assign(conditional));
                            }
                        }
                    }
                }
            }

            NodeKind::BindingIdent(lhs) => {
                let value = self.get_var_id_from_ident(&lhs.id).map(LValue::Var);
                self.push(Step::StoreLValue(value));
                self.push(Step::Assign(conditional));
            }
            NodeKind::ArrayPat(lhs) => {
                self.invalidate_r_value();
                for element in lhs.elems.iter().filter_map(|e| e.as_ref()) {
                    if let Pat::Expr(elem) = element {
                        self.visit_and_get_slot(Node::from(elem.as_ref()), conditional);
                        self.invalidate_l_value();
                    } else {
                        self.visit_destructuring(Node::from(element), conditional);
                    }
                }
            }
            NodeKind::RestPat(lhs) => {
                self.invalidate_r_value();
                self.visit_destructuring(Node::from(lhs.arg.as_ref()), conditional);
            }
            NodeKind::AssignPat(lhs) => {
                self.push(Step::StartUnion);
                self.push(Step::PushToUnion);
                self.visit_and_get_r_value(Node::from(lhs.right.as_ref()), true);
                self.push(Step::PushToUnion);
                self.push(Step::StoreUnion);

                self.visit_destructuring(Node::from(lhs.left.as_ref()), conditional);
            }
            NodeKind::KeyValuePatProp(lhs) => {
                self.invalidate_r_value();
                if matches!(lhs.value.as_ref(), Pat::Ident(_) | Pat::Expr(_)) {
                    self.visit_and_get_slot(Node::from(lhs.value.as_ref()), conditional);
                    self.invalidate_l_value();
                } else {
                    self.visit_destructuring(Node::from(lhs.value.as_ref()), conditional);
                }
            }
            NodeKind::AssignPatProp(_) => {
                unreachable!("removed by normalization");
            }

            _ => {
                dbg!(lhs);
                unreachable!();
            }
        }
    }

    /// May change the current RValue, so use [`Step::SaveRValue`] if that matters.
    fn visit_and_get_slot(&mut self, node: Node<'ast>, conditional: bool) {
        match node.kind {
            NodeKind::Ident(ident) | NodeKind::BindingIdent(BindingIdent { id: ident, .. }) => {
                let value = self.get_var_id_from_ident(ident).map(LValue::Var);
                self.push(Step::StoreLValue(value));
            }
            NodeKind::MemberExpr(node) => {
                self.visit_and_get_r_value(Node::from(&node.obj), conditional);
                if let Some(prop) = PropKey::from_expr(
                    &node.prop,
                    self.unresolved_ctxt,
                    node.computed,
                    &mut self.names,
                ) {
                    self.push(Step::StoreLValue(Some(LValue::RValueProp(prop.0))));
                } else {
                    self.invalidate_r_value();
                    self.visit_and_get_r_value(Node::from(node.prop.as_ref()), conditional);
                    self.push(Step::StoreLValue(None));
                }
            }
            _ => unreachable!(),
        }
    }

    fn visit_and_get_r_value(&mut self, node: Node<'ast>, conditional: bool) {
        match node.kind {
            NodeKind::FnExpr(f) => {
                let f = *self.function_map.get(&f.function.node_id).unwrap();
                self.push(Step::StoreRValue(Some(RValue::Fn(f))));
                // Don't traverse into new control flow nodes.
            }
            NodeKind::ArrowExpr(_) => {
                let f = *self.function_map.get(&node.node_id).unwrap();
                self.push(Step::StoreRValue(Some(RValue::Fn(f))));
                // Don't traverse into new control flow nodes.
            }

            NodeKind::AssignExpr(node) => {
                self.record_assignment(Node::from(&node.left), &node.right, conditional, node.op);
            }

            NodeKind::Class(_) => todo!(),
            NodeKind::ExtendsClause(_) => todo!(),
            NodeKind::ClassProp(_) => todo!(),
            NodeKind::PrivateProp(_) => todo!(),
            NodeKind::ClassMethod(_) => todo!(),
            NodeKind::PrivateMethod(_) => todo!(),
            NodeKind::Constructor(_) => todo!(),
            NodeKind::VarDeclarator(node) => {
                let lhs = Node::from(&node.name);
                if let Some(rhs) = &node.init {
                    self.record_assignment(lhs, rhs, conditional, AssignOp::Assign);
                } else {
                    self.visit_and_get_r_value(lhs, conditional);
                }
            }
            NodeKind::ThisExpr(_) => {
                self.push(Step::StoreRValue(None));
            }
            NodeKind::ArrayLit(node) => {
                for element in &node.elems {
                    if let Some(element) = element {
                        self.visit_and_get_r_value(Node::from(element), conditional);
                        // Can't track once it's in the array.
                        self.invalidate_r_value();
                    }
                }
                self.push(Step::StoreRValue(None));
            }
            NodeKind::ObjectLit(node) => {
                let is_simple_obj_lit = node.props.iter().all(|p| match p {
                    Prop::KeyValue(p) => is_simple_prop_name(&p.key, self.unresolved_ctxt),
                    _ => false,
                });

                if is_simple_obj_lit {
                    for prop in &node.props {
                        let prop = unwrap_as!(prop, Prop::KeyValue(p), p);
                        let key = PropKey::from_prop_name(
                            &prop.key,
                            self.unresolved_ctxt,
                            &mut self.names,
                        )
                        .unwrap();

                        self.visit_and_get_r_value(Node::from(prop.value.as_ref()), conditional);

                        self.push(Step::StoreLValue(Some(LValue::ObjectProp(
                            node.node_id,
                            key.0,
                        ))));
                        self.push(Step::Assign(conditional));
                    }
                    self.push(Step::StoreRValue(Some(RValue::Object(node.node_id))));
                } else {
                    self.push(Step::StoreRValue(Some(RValue::Object(node.node_id))));
                    self.invalidate_r_value();
                }
            }
            NodeKind::SpreadElement(node) => {
                self.visit_and_get_r_value(Node::from(node.expr.as_ref()), conditional);
                self.invalidate_r_value();
                self.push(Step::StoreRValue(None));
            }
            NodeKind::UnaryExpr(node) => {
                self.visit_and_get_r_value(Node::from(node.arg.as_ref()), conditional);
                // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-unary-operators
                let value = match node.op {
                    UnaryOp::Plus => Some(RValue::Number),
                    UnaryOp::TypeOf => Some(RValue::String),
                    UnaryOp::Void => Some(RValue::NullOrVoid),
                    UnaryOp::Bang | UnaryOp::Delete => Some(RValue::Boolean),
                    // Output type depends in input type.
                    UnaryOp::Minus | UnaryOp::Tilde => None,
                };
                self.push(Step::StoreRValue(value));
            }
            NodeKind::UpdateExpr(node) => {
                self.visit_and_get_r_value(Node::from(node.arg.as_ref()), conditional);
                self.push(Step::StoreRValue(None));
            }
            NodeKind::BinExpr(node) => {
                match node.op {
                    BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::NullishCoalescing => {
                        // TODO: if LHS is object, then we know if RHS will execute.
                        self.push(Step::StartUnion);
                        self.visit_and_get_r_value(Node::from(node.left.as_ref()), conditional);
                        self.push(Step::PushToUnion);
                        self.visit_and_get_r_value(Node::from(node.right.as_ref()), true);
                        self.push(Step::PushToUnion);
                        self.push(Step::StoreUnion);
                    }
                    _ => {
                        self.visit_and_get_r_value(Node::from(node.left.as_ref()), conditional);
                        self.visit_and_get_r_value(Node::from(node.right.as_ref()), conditional);
                        let value = match node.op {
                            // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-equality-operators
                            BinaryOp::EqEq
                            | BinaryOp::NotEq
                            | BinaryOp::EqEqEq
                            | BinaryOp::NotEqEq => Some(RValue::Boolean),
                            // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-relational-operators
                            BinaryOp::Lt
                            | BinaryOp::LtEq
                            | BinaryOp::Gt
                            | BinaryOp::GtEq
                            | BinaryOp::In
                            | BinaryOp::InstanceOf => Some(RValue::Boolean),
                            // The output type of these ops is either Number or BigInt,
                            // depending on the input types. Since we treat Number and
                            // BigInt as different objects, and we don't know the input
                            // types, we can't know the output types. See:
                            // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-applystringornumericbinaryoperator
                            BinaryOp::LShift
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
                            | BinaryOp::Exp => None,

                            BinaryOp::LogicalOr
                            | BinaryOp::LogicalAnd
                            | BinaryOp::NullishCoalescing => unreachable!("handled above"),
                        };
                        self.push(Step::StoreRValue(value));
                    }
                }
            }
            NodeKind::ClassExpr(_) => todo!(),
            NodeKind::MemberExpr(node) => {
                self.visit_and_get_r_value(Node::from(&node.obj), conditional);
                if let Some(prop) = PropKey::from_expr(
                    &node.prop,
                    self.unresolved_ctxt,
                    node.computed,
                    &mut self.names,
                ) {
                    self.push(Step::StoreRValue(Some(RValue::Prop(prop.0))));
                } else {
                    self.invalidate_r_value();
                    self.visit_and_get_r_value(Node::from(node.prop.as_ref()), conditional);
                    self.push(Step::StoreRValue(None));
                }
            }
            NodeKind::CondExpr(node) => {
                self.visit_and_get_r_value(Node::from(node.test.as_ref()), conditional);
                self.push(Step::StartUnion);
                self.visit_and_get_r_value(Node::from(node.cons.as_ref()), true);
                self.push(Step::PushToUnion);
                self.visit_and_get_r_value(Node::from(node.alt.as_ref()), true);
                self.push(Step::PushToUnion);
                self.push(Step::StoreUnion);
            }
            NodeKind::CallExpr(node) => {
                self.visit_and_get_r_value(Node::from(&node.callee), conditional);
                self.push(Step::StartCall(node.args.len()));
                for arg in &node.args {
                    self.visit_and_get_r_value(Node::from(arg), conditional);
                    self.push(Step::StoreArg);
                }
                self.push(Step::Call);
            }
            NodeKind::NewExpr(node) => {
                self.visit_and_get_r_value(Node::from(node.callee.as_ref()), conditional);
                self.invalidate_r_value();

                if let Some(args) = &node.args {
                    for arg in args {
                        self.visit_and_get_r_value(Node::from(arg), conditional);
                        self.invalidate_r_value();
                    }
                }
                self.push(Step::StoreRValue(None));
            }
            NodeKind::SeqExpr(node) => {
                debug_assert!(!node.exprs.is_empty());

                let mut i = 0;
                while i < node.exprs.len() - 1 {
                    self.visit_and_get_r_value(Node::from(node.exprs[i].as_ref()), conditional);
                    i += 1;
                }

                self.visit_and_get_r_value(Node::from(node.exprs[i].as_ref()), conditional);
            }
            NodeKind::YieldExpr(node) => {
                if let Some(arg) = &node.arg {
                    self.visit_and_get_r_value(Node::from(arg.as_ref()), conditional);
                    self.invalidate_r_value();
                }
                self.push(Step::StoreRValue(None));
            }
            NodeKind::AwaitExpr(node) => {
                self.visit_and_get_r_value(Node::from(node.arg.as_ref()), conditional)
            }
            NodeKind::Tpl(node) => {
                for expr in &node.exprs {
                    self.visit_and_get_r_value(Node::from(expr.as_ref()), conditional);
                }
                self.push(Step::StoreRValue(None));
            }
            NodeKind::TaggedTpl(node) => {
                self.visit_and_get_r_value(Node::from(node.tag.as_ref()), conditional);
                for expr in &node.tpl.exprs {
                    self.visit_and_get_r_value(Node::from(expr.as_ref()), conditional);
                    // Expressions in tagged templates can be accessed by the tag function.
                    self.invalidate_r_value();
                }
                self.push(Step::StoreRValue(None));
            }
            NodeKind::ParenExpr(node) => {
                self.visit_and_get_r_value(Node::from(node.expr.as_ref()), conditional)
            }
            NodeKind::Super(_) => todo!(),
            NodeKind::OptChainExpr(_) => todo!(),
            NodeKind::Function(_) => todo!(),
            NodeKind::Param(_) => todo!(),
            NodeKind::ParamWithoutDecorators(_) => todo!(),
            NodeKind::BindingIdent(node) => {
                self.visit_and_get_r_value(Node::from(&node.id), conditional)
            }
            NodeKind::Ident(node) => {
                if node.span.ctxt == self.unresolved_ctxt && node.sym == js_word!("undefined") {
                    self.push(Step::StoreRValue(Some(RValue::NullOrVoid)));
                } else {
                    let value = self.get_var_id_from_ident(node).map(RValue::Var);
                    self.push(Step::StoreRValue(value));
                }
            }
            NodeKind::PrivateName(_) => todo!(),

            NodeKind::Null(_) => {
                self.push(Step::StoreRValue(Some(RValue::NullOrVoid)));
            }

            NodeKind::Str(_) => {
                self.push(Step::StoreRValue(Some(RValue::String)));
            }
            NodeKind::Bool(_) => {
                self.push(Step::StoreRValue(Some(RValue::Boolean)));
            }
            NodeKind::Number(_) => {
                self.push(Step::StoreRValue(Some(RValue::Number)));
            }
            NodeKind::BigInt(_) => {
                self.push(Step::StoreRValue(Some(RValue::BigInt)));
            }

            NodeKind::Regex(_) | NodeKind::TplElement(_) | NodeKind::MetaPropExpr(_) => {
                self.push(Step::StoreRValue(None));
            }

            NodeKind::ImportDefaultSpecifier(_) => todo!(),
            NodeKind::ImportStarAsSpecifier(_) => todo!(),
            NodeKind::ImportNamedSpecifier(_) => todo!(),
            NodeKind::ExportNamespaceSpecifier(_) => todo!(),
            NodeKind::ExportDefaultSpecifier(_) => todo!(),
            NodeKind::ExportNamedSpecifier(_) => todo!(),
            NodeKind::ComputedPropName(node) => {
                self.visit_and_get_r_value(Node::from(node.expr.as_ref()), conditional);
            }
            NodeKind::CatchClause(_) => todo!(),

            // This function is only called on expressions (and their children),
            // so it can't reach e.g. statements. TypeScript and JSX should have
            // been removed by now as well.
            _ => unreachable!(),
        }
    }

    /// Initiates the analysis of the given node.
    fn init(&mut self, node: Node<'ast>, conditional: bool) {
        match node.kind {
            NodeKind::IfStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.test.as_ref()), conditional);
            }
            NodeKind::ExprStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.expr.as_ref()), conditional);
            }
            NodeKind::BlockStmt(_) => {}
            NodeKind::ForStmt(node) => {
                if let Some(test) = &node.test {
                    self.visit_and_get_r_value(Node::from(test.as_ref()), conditional);
                }
            }
            NodeKind::Function(_) => {}
            NodeKind::VarDecl(node) => {
                for decl in &node.decls {
                    self.visit_and_get_r_value(Node::from(decl), conditional);
                }
            }
            NodeKind::ThisExpr(_)
            | NodeKind::ArrayLit(_)
            | NodeKind::ObjectLit(_)
            | NodeKind::SpreadElement(_)
            | NodeKind::UnaryExpr(_)
            | NodeKind::UpdateExpr(_)
            | NodeKind::BinExpr(_)
            | NodeKind::FnExpr(_)
            | NodeKind::ClassExpr(_)
            | NodeKind::AssignExpr(_)
            | NodeKind::MemberExpr(_)
            | NodeKind::CondExpr(_)
            | NodeKind::CallExpr(_)
            | NodeKind::NewExpr(_)
            | NodeKind::SeqExpr(_)
            | NodeKind::ArrowExpr(_)
            | NodeKind::YieldExpr(_)
            | NodeKind::MetaPropExpr(_)
            | NodeKind::AwaitExpr(_)
            | NodeKind::Tpl(_)
            | NodeKind::TaggedTpl(_)
            | NodeKind::TplElement(_)
            | NodeKind::ParenExpr(_)
            | NodeKind::Super(_)
            | NodeKind::OptChainExpr(_)
            | NodeKind::BindingIdent(_)
            | NodeKind::Ident(_)
            | NodeKind::PrivateName(_) => {
                self.visit_and_get_r_value(node, conditional);
            }

            NodeKind::ImplicitReturn => {
                unreachable!("analyser should not be called on ImplicitReturn");
            }
            NodeKind::Class(_) => todo!(),
            NodeKind::ExtendsClause(_) => todo!(),
            NodeKind::ClassProp(_) => todo!(),
            NodeKind::PrivateProp(_) => todo!(),
            NodeKind::ClassMethod(_) => todo!(),
            NodeKind::PrivateMethod(_) => todo!(),
            NodeKind::Constructor(_) => todo!(),
            NodeKind::Decorator(_) => todo!(),
            NodeKind::FnDecl(_) => todo!(),
            NodeKind::ClassDecl(_) => todo!(),
            NodeKind::VarDeclarator(_) => todo!(),
            NodeKind::Param(_) => todo!(),
            NodeKind::ParamWithoutDecorators(_) => todo!(),
            NodeKind::ExportDefaultExpr(_) => todo!(),
            NodeKind::ExportDecl(_) => todo!(),
            NodeKind::ImportDecl(_) => todo!(),
            NodeKind::ExportAll(_) => todo!(),
            NodeKind::NamedExport(_) => todo!(),
            NodeKind::ExportDefaultDecl(_) => todo!(),
            NodeKind::ImportDefaultSpecifier(_) => todo!(),
            NodeKind::ImportStarAsSpecifier(_) => todo!(),
            NodeKind::ImportNamedSpecifier(_) => todo!(),
            NodeKind::ExportNamespaceSpecifier(_) => todo!(),
            NodeKind::ExportDefaultSpecifier(_) => todo!(),
            NodeKind::ExportNamedSpecifier(_) => todo!(),
            NodeKind::Script(_) => {}
            NodeKind::Module(_) => {}
            NodeKind::ArrayPat(_) => todo!(),
            NodeKind::ObjectPat(_) => todo!(),
            NodeKind::AssignPat(_) => todo!(),
            NodeKind::RestPat(_) => todo!(),
            NodeKind::KeyValuePatProp(_) => todo!(),
            NodeKind::AssignPatProp(_) => todo!(),
            NodeKind::KeyValueProp(_) => todo!(),
            NodeKind::AssignProp(_) => todo!(),
            NodeKind::GetterProp(_) => {}
            NodeKind::SetterProp(_) => {}
            NodeKind::ComputedPropName(_) => todo!(),
            NodeKind::SpreadAssignment(_) => todo!(),
            NodeKind::DebuggerStmt(_) => {}
            NodeKind::WithStmt(_) => todo!(),
            NodeKind::ReturnStmt(node) => {
                if let Some(arg) = &node.arg {
                    self.visit_and_get_r_value(Node::from(arg.as_ref()), conditional);
                    self.push(Step::Return);
                } else {
                    self.push(Step::StoreRValue(Some(RValue::NullOrVoid)));
                    self.push(Step::Return);
                }
            }
            NodeKind::LabeledStmt(_) => {}
            NodeKind::SwitchStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.discriminant.as_ref()), conditional);
            }
            NodeKind::ThrowStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.arg.as_ref()), conditional);
                self.invalidate_r_value();
                if self.cfg.get_successors(Node::from(node)).count() == 0 {
                    self.push(Step::Return);
                }
            }
            NodeKind::TryStmt(_) => {}
            NodeKind::WhileStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.test.as_ref()), conditional);
            }
            NodeKind::DoWhileStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.test.as_ref()), conditional);
            }
            NodeKind::ForInStmt(node) => {
                self.visit_and_get_r_value(Node::from(node.right.as_ref()), conditional);
                self.invalidate_r_value();
            }
            NodeKind::ForOfStmt(_) => todo!(),
            NodeKind::SwitchCase(node) => {
                if let Some(test) = &node.test {
                    self.visit_and_get_r_value(Node::from(test.as_ref()), conditional);
                }
            }
            NodeKind::CatchClause(node) => {
                if let Some(param) = &node.param {
                    match param {
                        Pat::Array(_) | Pat::Object(_) => {
                            self.push(Step::StoreRValue(None));
                            self.visit_destructuring(Node::from(param), conditional)
                        }
                        _ => {
                            self.visit_and_get_slot(Node::from(param), conditional);
                            self.push(Step::StoreRValue(None));
                            self.push(Step::Assign(conditional));
                        }
                    }
                }
            }

            NodeKind::Str(_)
            | NodeKind::Bool(_)
            | NodeKind::Null(_)
            | NodeKind::Number(_)
            | NodeKind::BigInt(_)
            | NodeKind::Regex(_)
            | NodeKind::EmptyStmt(_)
            | NodeKind::BreakStmt(_)
            | NodeKind::ContinueStmt(_) => {}

            _ => unreachable!("{:#?}", node),
        }
    }
}
