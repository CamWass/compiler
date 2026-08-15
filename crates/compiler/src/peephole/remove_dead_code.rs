use ast::*;
use common::{DUMMY_SP, SyntaxContext, util::take::Take};
use visit::{VisitMut, VisitMutWith};

use crate::{
    find_vars::find_pat_ids,
    node_util::{
        constructorCallHasSideEffects, expr_may_have_side_effects,
        function_call_may_have_side_effects, get_boolean_value, isLiteralValue, isPureIterable,
    },
    peephole::fold_constants::evaluateComparison,
    utils::unwrap_as,
};

pub fn process(ast: &mut Program, program_data: &mut ProgramData, unresolved_ctxt: SyntaxContext) {
    let mut visitor = Visitor {
        program_data,
        unresolved_ctxt,
    };
    ast.visit_mut_with(&mut visitor);
}

struct Visitor<'a> {
    program_data: &'a mut ProgramData,
    unresolved_ctxt: SyntaxContext,
}

#[derive(PartialEq)]
enum OptimiseExprResult {
    /// The expression has side-effects and must remain.
    Keep,
    // The expression has no side-effects and should be removed.
    Remove,
}

enum OptimiseSwitchResult {
    Keep(Vec<Stmt>),
    Replace(Vec<Stmt>),
}

impl Visitor<'_> {
    // TODO: clean up, notably all of the OptimiseExprResult <-> mapping with
    // ifs etc.
    /// Simplifies the expression in-place, returning whether the expression can
    /// be removed from its parent.
    fn simplify_unused_expr(&mut self, expr: &mut Expr) -> OptimiseExprResult {
        use OptimiseExprResult::*;

        match expr {
            Expr::Cond(cond) => {
                // Try to remove one or more of the conditional children and transform the HOOK to an
                // equivalent operation. Remember that if either value branch still exists, the result of
                // the predicate expression is being used, and so cannot be removed.
                //    x() ? foo() : 1 --> x() && foo()
                //    x() ? 1 : foo() --> x() || foo()
                //    x() ? 1 : 1 --> x()
                //    x ? 1 : 1 --> null
                let true_branch = self.simplify_unused_expr(&mut cond.cons);
                let false_branch = self.simplify_unused_expr(&mut cond.alt);
                if true_branch == Remove && false_branch == Keep {
                    *expr = Expr::Bin(BinExpr {
                        node_id: self.program_data.new_id_from(cond.node_id),
                        op: BinaryOp::LogicalOr,
                        left: cond.test.take(),
                        right: cond.alt.take(),
                    });

                    return Keep;
                } else if true_branch == Keep && false_branch == Remove {
                    *expr = Expr::Bin(BinExpr {
                        node_id: self.program_data.new_id_from(cond.node_id),
                        op: BinaryOp::LogicalAnd,
                        left: cond.test.take(),
                        right: cond.cons.take(),
                    });

                    return Keep;
                } else if true_branch == Remove && false_branch == Remove {
                    let condition = self.simplify_unused_expr(&mut cond.test);
                    if condition == Keep {
                        *expr = cond.test.as_mut().take();
                        return Keep;
                    } else {
                        return Remove;
                    }
                } else {
                    return Keep;
                }
            }
            Expr::Bin(bin) => match bin.op {
                BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::NullishCoalescing => {
                    // Try to remove the second operand from a AND, OR, and COALESCE operations. Remember that
                    // if the second
                    // child still exists, the result of the first expression is being used, and so cannot be
                    // removed.
                    //    x() ?? f --> x()
                    //    x() || f --> x()
                    //    x() && f --> x()
                    let rhs = self.simplify_unused_expr(&mut bin.right);
                    if rhs == Remove {
                        // Don't bother adding a second child to make the AST valid; this op is going to be
                        // deleted. We just need to collect any side-effects from the predicate first child.
                        let lhs = self.simplify_unused_expr(&mut bin.left);
                        if lhs == Keep {
                            *expr = bin.left.as_mut().take();
                            return Keep;
                        } else {
                            return Remove;
                        }
                    } else {
                        return Keep;
                    }
                }

                _ => {}
            },
            Expr::Fn(_) | Expr::Arrow(_) => {
                // Functions that aren't being invoked are dead. If they were invoked we'd see the CALL
                // before arriving here. We don't want to look at any children since they'll never execute.
                return Remove;
            }
            _ => {}
        }

        match expr {
            Expr::This(_) => Remove,
            Expr::Array(array) => {
                // TODO: for array elements and call/new args, we produce a flat
                // array if there are any impure spread elements, but closure
                // produces a SeqExpr of the elements, with each spread wrapped
                // in its own array. This is more verbose, but might allow for
                // further optimisation since a sequence of expressions is
                // simpler than an array.
                array.elems.retain_mut(|el| match el {
                    Some(el) => match el {
                        ExprOrSpread::Spread(spread) => {
                            if isPureIterable(&spread.expr) {
                                let remove_expr = self.simplify_unused_expr(&mut spread.expr);
                                if remove_expr == Remove { false } else { true }
                            } else {
                                true
                            }
                        }
                        ExprOrSpread::Expr(expr) => {
                            let remove_expr = self.simplify_unused_expr(expr);
                            if remove_expr == Remove { false } else { true }
                        }
                    },
                    None => false,
                });

                if array.elems.is_empty() {
                    return Remove;
                }

                let has_spreads = array
                    .elems
                    .iter()
                    .any(|a| matches!(a, Some(ExprOrSpread::Spread(_))));

                if has_spreads {
                    return Keep;
                }

                if array.elems.len() == 1 {
                    *expr = unwrap_as!(
                        array.elems.first_mut(),
                        Some(Some(ExprOrSpread::Expr(e))),
                        e.as_mut().take()
                    );
                    Keep
                } else {
                    *expr = Expr::Seq(SeqExpr {
                        node_id: self.program_data.new_id_from(array.node_id),
                        exprs: array
                            .elems
                            .take()
                            .into_iter()
                            .map(|a| *unwrap_as!(a, Some(ExprOrSpread::Expr(e)), e))
                            .collect(),
                    });
                    Keep
                }
            }
            Expr::Object(obj) => {
                let mut side_effects = Vec::new();

                for prop in &mut obj.props {
                    match prop {
                        Prop::Assign(_) => unreachable!(),
                        Prop::KeyValue(KeyValueProp { key, value, .. }) => {
                            match key {
                                PropName::Ident(_)
                                | PropName::Str(_)
                                | PropName::Num(_)
                                | PropName::BigInt(_) => {}
                                PropName::Computed(computed) => {
                                    let remove_key = self.simplify_unused_expr(&mut computed.expr);
                                    if remove_key == Keep {
                                        side_effects.push(computed.expr.as_mut().take());
                                    }
                                }
                            }

                            let remove_value = self.simplify_unused_expr(value);
                            if remove_value == Keep {
                                side_effects.push(value.as_mut().take());
                            }
                        }
                        Prop::Getter(GetterProp { key, .. })
                        | Prop::Setter(SetterProp { key, .. })
                        | Prop::Method(MethodProp { key, .. }) => match key {
                            PropName::Ident(_)
                            | PropName::Str(_)
                            | PropName::Num(_)
                            | PropName::BigInt(_) => {}
                            PropName::Computed(computed) => {
                                let remove_key = self.simplify_unused_expr(&mut computed.expr);
                                if remove_key == Keep {
                                    side_effects.push(computed.expr.as_mut().take());
                                }
                            }
                        },
                        Prop::Spread(spread) => {
                            let remove_arg = self.simplify_unused_expr(&mut spread.expr);
                            if remove_arg == Keep {
                                side_effects.push(spread.expr.as_mut().take());
                            }
                        }
                    }
                }

                if side_effects.is_empty() {
                    Remove
                } else if side_effects.len() == 1 {
                    *expr = side_effects.into_iter().next().unwrap();
                    Keep
                } else {
                    *expr = Expr::Seq(SeqExpr {
                        node_id: self.program_data.new_id_from(obj.node_id),
                        exprs: side_effects,
                    });
                    Keep
                }
            }

            Expr::Unary(unary) => {
                if unary.op == UnaryOp::Delete {
                    Keep
                } else {
                    let remove_arg = self.simplify_unused_expr(&mut unary.arg);
                    if remove_arg == Remove {
                        Remove
                    } else {
                        *expr = unary.arg.as_mut().take();
                        Keep
                    }
                }
            }
            Expr::Update(_) => Keep,
            Expr::Bin(bin) => {
                assert!(
                    !matches!(
                        bin.op,
                        BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::NullishCoalescing
                    ),
                    "handled above"
                );

                let can_remove_lhs = self.simplify_unused_expr(&mut bin.left);
                let can_remove_rhs = self.simplify_unused_expr(&mut bin.right);

                if can_remove_lhs == Remove && can_remove_rhs == Keep {
                    *expr = bin.right.as_mut().take();
                    Keep
                } else if can_remove_lhs == Keep && can_remove_rhs == Remove {
                    *expr = bin.left.as_mut().take();
                    Keep
                } else if can_remove_lhs == Remove && can_remove_rhs == Remove {
                    Remove
                } else {
                    *expr = Expr::Seq(SeqExpr {
                        node_id: self.program_data.new_id_from(bin.node_id),
                        exprs: vec![bin.left.as_mut().take(), bin.right.as_mut().take()],
                    });
                    Keep
                }
            }
            Expr::Assign(_) => Keep,
            Expr::Member(member) => {
                let remove_prop = self.simplify_unused_expr(&mut member.prop);

                let remove_obj = match &mut member.obj {
                    ExprOrSuper::Super(_) => Remove,
                    ExprOrSuper::Expr(expr) => self.simplify_unused_expr(expr),
                };

                if remove_obj == Keep && remove_prop == Remove {
                    *expr = unwrap_as!(&mut member.obj, ExprOrSuper::Expr(e), e.as_mut().take());
                    Keep
                } else if remove_obj == Remove && remove_prop == Keep {
                    *expr = member.prop.as_mut().take();
                    Keep
                } else if remove_obj == Remove && remove_prop == Remove {
                    Remove
                } else {
                    Keep
                }
            }

            Expr::Call(call) => {
                let call_may_have_side_effects = match &call.callee {
                    ExprOrSuper::Super(_) => true,
                    ExprOrSuper::Expr(callee) => {
                        function_call_may_have_side_effects(callee, self.unresolved_ctxt)
                    }
                };

                if call_may_have_side_effects {
                    return Keep;
                }

                call.args.retain_mut(|el| match el {
                    ExprOrSpread::Spread(spread) => {
                        if isPureIterable(&spread.expr) {
                            let remove_expr = self.simplify_unused_expr(&mut spread.expr);
                            if remove_expr == Remove { false } else { true }
                        } else {
                            true
                        }
                    }
                    ExprOrSpread::Expr(expr) => {
                        let remove_expr = self.simplify_unused_expr(expr);
                        if remove_expr == Remove { false } else { true }
                    }
                });

                if call.args.is_empty() {
                    return Remove;
                }

                let has_spreads = call
                    .args
                    .iter()
                    .any(|a| matches!(a, ExprOrSpread::Spread(_)));

                if has_spreads {
                    *expr = Expr::Array(ArrayLit {
                        node_id: self.program_data.new_id_from(call.node_id),
                        elems: call.args.take().into_iter().map(Some).collect(),
                    });

                    return Keep;
                }

                if call.args.len() == 1 {
                    *expr = unwrap_as!(
                        call.args.first_mut(),
                        Some(ExprOrSpread::Expr(e)),
                        e.as_mut().take()
                    );
                    Keep
                } else {
                    *expr = Expr::Seq(SeqExpr {
                        node_id: self.program_data.new_id(DUMMY_SP),
                        exprs: call
                            .args
                            .take()
                            .into_iter()
                            .map(|a| *unwrap_as!(a, ExprOrSpread::Expr(e), e))
                            .collect(),
                    });
                    Keep
                }
            }
            Expr::New(new) => {
                let constructor_may_have_side_effects =
                    constructorCallHasSideEffects(new, self.unresolved_ctxt);

                if constructor_may_have_side_effects {
                    return Keep;
                }

                let Some(args) = &mut new.args else {
                    return Remove;
                };

                args.retain_mut(|el| match el {
                    ExprOrSpread::Spread(spread) => {
                        if isPureIterable(&spread.expr) {
                            let remove_expr = self.simplify_unused_expr(&mut spread.expr);
                            if remove_expr == Remove { false } else { true }
                        } else {
                            true
                        }
                    }
                    ExprOrSpread::Expr(expr) => {
                        let remove_expr = self.simplify_unused_expr(expr);
                        if remove_expr == Remove { false } else { true }
                    }
                });

                if args.is_empty() {
                    return Remove;
                }

                let has_spreads = args.iter().any(|a| matches!(a, ExprOrSpread::Spread(_)));

                if has_spreads {
                    *expr = Expr::Array(ArrayLit {
                        node_id: self.program_data.new_id_from(new.node_id),
                        elems: args.take().into_iter().map(Some).collect(),
                    });

                    return Keep;
                }

                if args.len() == 1 {
                    *expr = unwrap_as!(
                        args.first_mut(),
                        Some(ExprOrSpread::Expr(e)),
                        e.as_mut().take()
                    );
                    Keep
                } else {
                    *expr = Expr::Seq(SeqExpr {
                        node_id: self.program_data.new_id(DUMMY_SP),
                        exprs: args
                            .take()
                            .into_iter()
                            .map(|a| *unwrap_as!(a, ExprOrSpread::Expr(e), e))
                            .collect(),
                    });
                    Keep
                }
            }
            Expr::Seq(seq) => {
                seq.exprs.retain_mut(|expr| {
                    let remove_expr = self.simplify_unused_expr(expr);
                    if remove_expr == Remove { false } else { true }
                });

                if seq.exprs.len() == 1 {
                    *expr = unwrap_as!(seq.exprs.first_mut(), Some(e), e.take());
                    Keep
                } else if seq.exprs.is_empty() {
                    Remove
                } else {
                    Keep
                }
            }
            Expr::Ident(_) => Remove,
            Expr::Lit(_) => Remove,
            Expr::Tpl(tpl) => {
                tpl.exprs.retain_mut(|expr| {
                    let remove_expr = self.simplify_unused_expr(expr);
                    if remove_expr == Remove { false } else { true }
                });

                if tpl.exprs.is_empty() {
                    Remove
                } else if tpl.exprs.len() == 1 {
                    *expr = tpl.exprs.first_mut().unwrap().take();
                    Keep
                } else {
                    *expr = Expr::Seq(SeqExpr {
                        node_id: self.program_data.new_id(DUMMY_SP),
                        exprs: tpl.exprs.take().into_iter().collect(),
                    });
                    Keep
                }
            }
            Expr::TaggedTpl(tpl) => {
                let tag_call_may_have_side_effects =
                    function_call_may_have_side_effects(&tpl.tag, self.unresolved_ctxt);

                if tag_call_may_have_side_effects {
                    return Keep;
                }

                tpl.tpl.exprs.retain_mut(|expr| {
                    let remove_expr = self.simplify_unused_expr(expr);
                    if remove_expr == Remove { false } else { true }
                });

                if tpl.tpl.exprs.is_empty() {
                    Remove
                } else if tpl.tpl.exprs.len() == 1 {
                    *expr = tpl.tpl.exprs.first_mut().unwrap().take();
                    Keep
                } else {
                    *expr = Expr::Seq(SeqExpr {
                        node_id: self.program_data.new_id(DUMMY_SP),
                        exprs: tpl.tpl.exprs.take().into_iter().collect(),
                    });
                    Keep
                }
            }
            Expr::Class(class) => {
                let mut side_effects = Vec::new();

                if let Some(extends) = &mut class.class.extends {
                    let remove_extends = self.simplify_unused_expr(&mut extends.super_class);
                    if remove_extends == Keep {
                        side_effects.push(extends.super_class.as_mut().take());
                    }
                }

                for member in &mut class.class.body {
                    match member {
                        ClassMember::Method(class_method) => {
                            if let PropName::Computed(key) = &mut class_method.key {
                                let remove_key = self.simplify_unused_expr(&mut key.expr);
                                if remove_key == Keep {
                                    side_effects.push(key.expr.as_mut().take());
                                }
                            }
                        }
                        ClassMember::PrivateMethod(_) | ClassMember::Constructor(_) => {}
                        ClassMember::ClassProp(class_prop) => {
                            if let Some(value) = &mut class_prop.value {
                                if class_prop.is_static {
                                    // TODO: static props can access the
                                    // ClassExpr name and cause the ClassExpr to
                                    // have side-effects. We should normalise
                                    // static prop initialisers like closure
                                    // does to avoid this.
                                    todo!();
                                }

                                let remove_value = self.simplify_unused_expr(value);
                                if remove_value == Keep {
                                    side_effects.push(value.as_mut().take());
                                }
                            }

                            if let PropName::Computed(key) = &mut class_prop.key {
                                let remove_key = self.simplify_unused_expr(&mut key.expr);
                                if remove_key == Keep {
                                    side_effects.push(key.expr.as_mut().take());
                                }
                            }
                        }
                        ClassMember::PrivateProp(private_prop) => {
                            if let Some(value) = &mut private_prop.value {
                                if private_prop.is_static {
                                    // TODO: static props can access the
                                    // ClassExpr name and cause the ClassExpr to
                                    // have side-effects. We should normalise
                                    // static prop initialisers like closure
                                    // does to avoid this.
                                    todo!();
                                }

                                let remove_value = self.simplify_unused_expr(value);
                                if remove_value == Keep {
                                    side_effects.push(value.as_mut().take());
                                }
                            }
                        }
                    }
                }

                if side_effects.is_empty() {
                    Remove
                } else if side_effects.len() == 1 {
                    *expr = side_effects.into_iter().next().unwrap();
                    Keep
                } else {
                    *expr = Expr::Seq(SeqExpr {
                        node_id: self.program_data.new_id_from(class.node_id),
                        exprs: side_effects,
                    });
                    Keep
                }
            }
            Expr::Yield(_) => Keep,
            Expr::MetaProp(_) => Remove,
            Expr::Await(_) => Keep,
            Expr::PrivateName(_) => Remove,
            Expr::OptChain(opt) => {
                let remove_expr = self.simplify_unused_expr(&mut opt.expr);
                if remove_expr == Remove { Remove } else { Keep }
            }

            Expr::Fn(_) | Expr::Arrow(_) | Expr::Cond(_) => unreachable!("handled above"),
            Expr::Invalid(_) => unreachable!(),
        }
    }

    /// Simplifies the switch statement in-place, returning whether the
    /// whole statement can be removed.
    fn simplify_switch_stmt(&mut self, stmt: &mut Stmt) -> OptimiseSwitchResult {
        let switch = unwrap_as!(stmt, Stmt::Switch(s), s);

        let mut extracted_vars = Vec::new();

        self.tryOptimizeDefaultCase(switch, &mut extracted_vars);

        let has_default_case = switch.cases.iter().any(SwitchCase::is_default);
        let default_case_is_last =
            matches!(switch.cases.last(), Some(SwitchCase { test: None, .. }));

        // Generally, it is unsafe to remove other cases when the default case is not the last one.
        if (!has_default_case || default_case_is_last)
            && areAllCaseTagsLiterals(&switch.cases, self.unresolved_ctxt)
        {
            // TODO: this comment is not accurate - empty default should have
            // been removed above. We have either no default case, or default
            // is last, so it's safe to remove other cases.
            // First, remove empty cases where possible: always empty default cases; or when there is
            // no default case, other empty cases that are not the first matching case, may be
            // removable as well.
            let mut foundMatchingCase = false;
            let mut i = 0;
            while i < switch.cases.len() {
                if let Some(test) = &switch.cases[i].test {
                    foundMatchingCase = isFirstSwitchMatch(
                        foundMatchingCase,
                        &switch.discriminant,
                        test,
                        self.unresolved_ctxt,
                    );
                    if !foundMatchingCase
                        && !expr_may_have_side_effects(test, self.unresolved_ctxt)
                        && self.isUselessCase(&switch.cases, i)
                    {
                        self.collect_vars_declared_in_switch_case(
                            &switch.cases[i],
                            &mut extracted_vars,
                        );
                        switch.cases.remove(i);
                        continue;
                    }
                } else {
                    foundMatchingCase = false;
                }

                i += 1;
            }

            // Next, optimize switches with constant condition
            if isLiteralValue(&switch.discriminant, false, self.unresolved_ctxt) {
                let mut found_matching_case = false;
                // Remove cases until you find one that may match
                while let Some(first) = switch.cases.first() {
                    if let Some(test) = &first.test {
                        let caseMatches = evaluateComparison(
                            BinaryOp::EqEqEq,
                            &switch.discriminant,
                            test,
                            self.unresolved_ctxt,
                        );
                        if caseMatches == Some(true) {
                            found_matching_case = true;
                            break;
                        } else if caseMatches == None {
                            break;
                        }

                        // Case definitely doesn't match - remove.

                        self.collect_vars_declared_in_switch_case(
                            &switch.cases[0],
                            &mut extracted_vars,
                        );
                        switch.cases.remove(0);
                        continue;
                    } else {
                        break;
                    }
                }

                if found_matching_case {
                    // Skip cases until you find one whose last stm is a removable break
                    let mut i = 0;
                    while i < switch.cases.len() {
                        let mut isLastStmRemovableBreak = false;
                        if let Some(last) = switch.cases[i].cons.last()
                            && isExit(last)
                        {
                            if matches!(last, Stmt::Break(BreakStmt { label: None, .. })) {
                                switch.cases[i].cons.pop();
                            }
                            // TODO: is this var name misleading/incorrect? At
                            // this point stmt could be return/continue/throw or
                            // labelled break.
                            isLastStmRemovableBreak = true;
                        }

                        if i != 0 {
                            let [matching, cur] = switch.cases.get_disjoint_mut([0, i]).unwrap();

                            // Remove the fallthrough case labels
                            matching.cons.append(&mut cur.cons);
                            self.collect_vars_declared_in_switch_case(cur, &mut extracted_vars);
                            switch.cases.remove(i);
                        } else {
                            i += 1;
                        }

                        if isLastStmRemovableBreak {
                            break;
                        }
                    }

                    // Remove any remaining cases
                    for case in switch.cases.drain(i..) {
                        self.collect_vars_declared_in_switch_case(&case, &mut extracted_vars);
                    }

                    // If there is one case left, we may be able to fold it
                    if switch.cases.len() == 1 {
                        return self.tryRemoveSwitchWithSingleCase(stmt, false, extracted_vars);
                    }
                }
            }
        }

        // Last, try to remove the entire switch if possible
        self.tryRemoveSwitch(stmt, extracted_vars)
    }

    fn tryRemoveSwitch(
        &mut self,
        stmt: &mut Stmt,
        extracted_vars: Vec<Stmt>,
    ) -> OptimiseSwitchResult {
        let switch = unwrap_as!(stmt, Stmt::Switch(s), s);

        let has_no_cases = switch.cases.is_empty();
        let has_only_default_case =
            matches!(switch.cases.as_slice(), [SwitchCase { test: None, .. }]);

        if has_no_cases {
            // Remove the switch if there are no remaining cases.
            *stmt = Stmt::Expr(ExprStmt {
                node_id: self.program_data.new_id(DUMMY_SP),
                expr: switch.discriminant.take(),
            });

            OptimiseSwitchResult::Keep(extracted_vars)
        } else if has_only_default_case {
            match switch.discriminant.as_ref() {
                // Before removing switch, we must preserve the switch condition if it is a call
                Expr::Call(_) => self.tryRemoveSwitchWithSingleCase(stmt, true, extracted_vars),
                Expr::OptChain(opt) if matches!(opt.expr.as_ref(), Expr::Call(_)) => {
                    self.tryRemoveSwitchWithSingleCase(stmt, true, extracted_vars)
                }

                _ => self.tryRemoveSwitchWithSingleCase(stmt, false, extracted_vars),
            }
        } else {
            OptimiseSwitchResult::Keep(extracted_vars)
        }
    }

    fn tryRemoveSwitchWithSingleCase(
        &mut self,
        stmt: &mut Stmt,
        shouldHoistCondition: bool,
        extracted_vars: Vec<Stmt>,
    ) -> OptimiseSwitchResult {
        let switch = unwrap_as!(stmt, Stmt::Switch(s), s);

        let case = switch.cases.first_mut().unwrap();

        fn contains_unlabelled_break(stmt: &Stmt) -> bool {
            match stmt {
                Stmt::Break(break_stmt) => matches!(break_stmt, BreakStmt { label: None, .. }),

                Stmt::Block(block_stmt) => block_stmt.stmts.iter().any(contains_unlabelled_break),
                Stmt::If(if_stmt) => {
                    contains_unlabelled_break(&if_stmt.cons)
                        || if_stmt
                            .alt
                            .as_ref()
                            .is_some_and(|s| contains_unlabelled_break(s))
                }

                // Any unlabelled break in an inner switch or loop corresponds
                // to that inner construct, not the outer switch that we're
                // scanning.
                Stmt::Switch(_)
                | Stmt::While(_)
                | Stmt::DoWhile(_)
                | Stmt::For(_)
                | Stmt::ForIn(_)
                | Stmt::ForOf(_) => false,

                Stmt::Try(try_stmt) => {
                    try_stmt.block.stmts.iter().any(contains_unlabelled_break)
                        || try_stmt.handler.as_ref().is_some_and(|catch| {
                            catch.body.stmts.iter().any(contains_unlabelled_break)
                        })
                        || try_stmt.finalizer.as_ref().is_some_and(|finalizer| {
                            finalizer.stmts.iter().any(contains_unlabelled_break)
                        })
                }

                Stmt::With(WithStmt { body, .. }) | Stmt::Labeled(LabeledStmt { body, .. }) => {
                    contains_unlabelled_break(body)
                }

                Stmt::Empty(_)
                | Stmt::Debugger(_)
                | Stmt::Return(_)
                | Stmt::Continue(_)
                | Stmt::Throw(_)
                | Stmt::Decl(_)
                | Stmt::Expr(_) => false,
            }
        }

        // If the last statement in the case is an unlabelled break, remove it.
        if let Some(Stmt::Break(BreakStmt { label: None, .. })) = case.cons.last() {
            case.cons.pop();
        }

        // Back off if the switch contains statements like "if (a) { break; }"
        if case.cons.iter().any(contains_unlabelled_break) {
            return OptimiseSwitchResult::Keep(extracted_vars);
        }

        let mut replacement = extracted_vars;

        replacement.append(&mut case.cons);

        if shouldHoistCondition {
            replacement.insert(
                0,
                Stmt::Expr(ExprStmt {
                    node_id: self.program_data.new_id(DUMMY_SP),
                    expr: switch.discriminant.take(),
                }),
            );
        }

        OptimiseSwitchResult::Replace(replacement)
    }

    fn tryOptimizeDefaultCase(&mut self, switch: &mut SwitchStmt, extracted_vars: &mut Vec<Stmt>) {
        let Some(last) = switch.cases.last() else {
            return;
        };

        // In general it's not safe to remove other cases when the default is
        // not last.
        if last.is_default() {
            // Remove any cases that fall-through to the default case.

            let other_cases = switch.cases.iter().rev().skip(1);

            let is_fallthrough = |case: &&SwitchCase| {
                case.cons.is_empty()
                    && case
                        .test
                        .as_ref()
                        .is_none_or(|test| !expr_may_have_side_effects(test, self.unresolved_ctxt))
            };

            let num_fallthrough_cases_to_remove = other_cases.take_while(is_fallthrough).count();

            if num_fallthrough_cases_to_remove > 0 {
                let default_case_index = switch.cases.len() - 1;
                // We checked that these cases are empty above, so there're no
                // side-effects to preserve and we can just drop them.
                switch.cases.drain(
                    default_case_index - num_fallthrough_cases_to_remove..default_case_index,
                );
            }
        }

        let default_index = switch.cases.iter().position(SwitchCase::is_default);

        // Remove the default case if we can
        if let Some(default_index) = default_index {
            if self.isUselessCase(&switch.cases, default_index) {
                self.collect_vars_declared_in_switch_case(
                    &switch.cases[default_index],
                    extracted_vars,
                );
                switch.cases.remove(default_index);
            }
        }
    }

    /**
     * The function assumes that when checking a CASE node there is no DEFAULT_CASE node in the
     * SWITCH, or the DEFAULT_CASE is the last case in the SWITCH.
     *
     * @return Whether the CASE or DEFAULT_CASE block does anything useful.
     */
    fn isUselessCase(&self, cases: &[SwitchCase], case_idx: usize) -> bool {
        let is_last = case_idx == cases.len() - 1;
        let prev_case = case_idx.checked_sub(1).and_then(|idx| cases.get(idx));
        let default_case = cases.iter().find(|c| c.is_default());

        // A case isn't useless if a previous case falls through to it unless it happens to be the last
        // case in the switch.
        if !is_last && let Some(prev_case) = prev_case {
            // Prev case will definitely fall through if it has no statements,
            // and will definitely not fall though if its las statement is an
            // exit. We assume it will fall through if the last statement is not
            // an exit.
            if prev_case.cons.last().is_none_or(|s| !isExit(s)) {
                return false;
            }
        }

        for (i, executing_case) in cases[case_idx..].iter().enumerate() {
            // We only expect a DEFAULT case if the case we are checking is the
            // DEFAULT case. Otherwise, we assume the DEFAULT case has already
            // been removed.
            assert!(case_idx == case_idx + i || !executing_case.is_default());
            if !executing_case.is_default()
                && expr_may_have_side_effects(
                    executing_case.test.as_ref().unwrap(),
                    self.unresolved_ctxt,
                )
            {
                // The case falls through to a case whose condition has a potential side-effect,
                // removing the candidate case would skip that side-effect, so don't.
                return false;
            }
            for stmt in &executing_case.cons {
                // If this is a block with a unlabelled break, it is useless.
                match stmt {
                    Stmt::Break(break_stmt) => {
                        // A case with a single unlabelled break is useless if it is the default case or if
                        // there is no default case. A break to a different control structure isn't useless.
                        return break_stmt.label.is_none()
                            && default_case.is_none_or(|c| c.node_id == executing_case.node_id);
                    }
                    // TODO: uninitialised let is probably ok too?
                    Stmt::Decl(Decl::Var(VarDecl {
                        kind: VarDeclKind::Var,
                        decls,
                        ..
                    })) => {
                        if decls.iter().all(|d| d.init.is_none()) {
                            // Variable declarations without initializations are OK.
                            continue;
                        }
                        return false;
                    }
                    _ => {
                        return false;
                    }
                }
            }

            // Look at the fallthrough case.
        }

        true
    }

    fn collect_vars_declared_in_switch_case(
        &mut self,
        case: &SwitchCase,
        extracted_vars: &mut Vec<Stmt>,
    ) {
        for stmt in &case.cons {
            self.collect_vars_declared_in_stmt(stmt, extracted_vars);
        }
    }

    fn collect_vars_declared_in_stmt(&mut self, stmt: &Stmt, extracted_vars: &mut Vec<Stmt>) {
        match stmt {
            Stmt::While(WhileStmt { body, .. })
            | Stmt::DoWhile(DoWhileStmt { body, .. })
            | Stmt::For(ForStmt { body, .. })
            | Stmt::ForIn(ForInStmt { body, .. })
            | Stmt::ForOf(ForOfStmt { body, .. })
            | Stmt::With(WithStmt { body, .. })
            | Stmt::Labeled(LabeledStmt { body, .. }) => {
                self.collect_vars_declared_in_stmt(body, extracted_vars);
            }

            Stmt::Block(block_stmt) => {
                for stmt in &block_stmt.stmts {
                    self.collect_vars_declared_in_stmt(stmt, extracted_vars);
                }
            }
            Stmt::If(if_stmt) => {
                self.collect_vars_declared_in_stmt(&if_stmt.cons, extracted_vars);
                if let Some(alt) = &if_stmt.alt {
                    self.collect_vars_declared_in_stmt(alt, extracted_vars);
                }
            }

            Stmt::Switch(switch) => {
                for case in &switch.cases {
                    for stmt in &case.cons {
                        self.collect_vars_declared_in_stmt(stmt, extracted_vars);
                    }
                }
            }

            Stmt::Try(try_stmt) => {
                for stmt in &try_stmt.block.stmts {
                    self.collect_vars_declared_in_stmt(stmt, extracted_vars);
                }

                if let Some(handler) = &try_stmt.handler {
                    for stmt in &handler.body.stmts {
                        self.collect_vars_declared_in_stmt(stmt, extracted_vars);
                    }
                }

                if let Some(finalizer) = &try_stmt.finalizer {
                    for stmt in &finalizer.stmts {
                        self.collect_vars_declared_in_stmt(stmt, extracted_vars);
                    }
                }
            }

            Stmt::Decl(decl) => match decl {
                Decl::Class(_) | Decl::Fn(_) => {}
                Decl::Var(var_decl) => {
                    if var_decl.kind == VarDeclKind::Var {
                        for decl in &var_decl.decls {
                            let names = find_pat_ids(&decl.name);
                            extracted_vars.extend(names.into_iter().map(|(name, name_node_id)| {
                                Stmt::Decl(Decl::Var(VarDecl {
                                    node_id: self.program_data.new_id_from(name_node_id),
                                    kind: VarDeclKind::Var,
                                    decls: vec![VarDeclarator {
                                        node_id: self.program_data.new_id_from(name_node_id),
                                        name: Pat::Ident(BindingIdent {
                                            id: Ident {
                                                node_id: self
                                                    .program_data
                                                    .new_id_from(name_node_id),
                                                sym: name.0,
                                                ctxt: name.1,
                                            },
                                        }),
                                        init: None,
                                    }],
                                }))
                            }));
                        }
                    }
                }
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
}

impl VisitMut<'_> for Visitor<'_> {
    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        let mut i = 0;
        while i < stmts.len() {
            stmts[i].visit_mut_with(self);

            // Inline block contents into parent statement list.
            if matches!(&stmts[i], Stmt::Block(_)) {
                let stmt = stmts.remove(i);
                let block = unwrap_as!(stmt, Stmt::Block(b), b);
                let num_stmts = block.stmts.len();
                // Insert the new statements at the index of the old one to preserve ordering.
                stmts.splice(i..i, block.stmts);

                if num_stmts > 0 {
                    // Skip over the new stmts.
                    i += num_stmts - 1;
                    continue;
                } else {
                    i += 1;
                    continue;
                }
            }

            if let Stmt::Expr(stmt) = &mut stmts[i] {
                let remove = self.simplify_unused_expr(&mut stmt.expr);

                if remove == OptimiseExprResult::Remove {
                    stmts.remove(i);
                    continue;
                } else {
                    i += 1;
                    continue;
                }
            }

            if let Stmt::Switch(_) = &mut stmts[i] {
                let result = self.simplify_switch_stmt(&mut stmts[i]);

                match result {
                    OptimiseSwitchResult::Keep(new_stmts) => {
                        let num_new_stmts = new_stmts.len();
                        stmts.splice(i..i, new_stmts);

                        // Skip over current and new stmts.
                        i += num_new_stmts + 1;
                        continue;
                    }
                    OptimiseSwitchResult::Replace(replacements) => {
                        let num_replacements = replacements.len();
                        stmts.splice(i..=i, replacements);

                        if num_replacements > 0 {
                            // Skip over the new stmts.
                            i += num_replacements;
                            continue;
                        } else {
                            i += 1;
                            continue;
                        }
                    }
                }
            }

            i += 1;
        }
    }

    fn visit_mut_module_items(&mut self, items: &mut Vec<ModuleItem>) {
        let mut i = 0;
        while i < items.len() {
            items[i].visit_mut_with(self);

            // Inline block contents into parent statement list.
            if matches!(&items[i], ModuleItem::Stmt(Stmt::Block(_))) {
                let stmt = items.remove(i);
                let block = unwrap_as!(stmt, ModuleItem::Stmt(Stmt::Block(b)), b);
                let num_stmts = block.stmts.len();
                // Insert the new statements at the index of the old one to preserve ordering.
                items.splice(i..i, block.stmts.into_iter().map(ModuleItem::Stmt));

                if num_stmts > 0 {
                    // Skip over the new stmts.
                    i += num_stmts - 1;
                    continue;
                } else {
                    i += 1;
                    continue;
                }
            }

            if let ModuleItem::Stmt(Stmt::Expr(stmt)) = &mut items[i] {
                let remove = self.simplify_unused_expr(&mut stmt.expr);

                if remove == OptimiseExprResult::Remove {
                    items.remove(i);
                    continue;
                } else {
                    i += 1;
                    continue;
                }
            }

            if let ModuleItem::Stmt(stmt @ Stmt::Switch(_)) = &mut items[i] {
                let result = self.simplify_switch_stmt(stmt);

                match result {
                    OptimiseSwitchResult::Keep(new_stmts) => {
                        let num_new_stmts = new_stmts.len();
                        items.splice(i..i, new_stmts.into_iter().map(ModuleItem::Stmt));

                        // Skip over current and new stmts.
                        i += num_new_stmts + 1;
                        continue;
                    }
                    OptimiseSwitchResult::Replace(replacements) => {
                        let num_replacements = replacements.len();
                        items.splice(i..=i, replacements.into_iter().map(ModuleItem::Stmt));

                        if num_replacements > 0 {
                            // Skip over the new stmts.
                            i += num_replacements;
                            continue;
                        } else {
                            i += 1;
                            continue;
                        }
                    }
                }
            }

            i += 1;
        }
    }

    fn visit_mut_expr(&mut self, node: &mut Expr) {
        match node {
            Expr::Assign(expr) => {
                expr.visit_mut_children_with(self);

                if expr.op == AssignOp::Assign {
                    let left_ident = match &expr.left {
                        PatOrExpr::Expr(left) => match left.as_ref() {
                            Expr::Ident(left) => Some(left),
                            _ => None,
                        },
                        PatOrExpr::Pat(left) => match left.as_ref() {
                            Pat::Ident(left) => Some(&left.id),
                            _ => None,
                        },
                    };

                    let right_ident = match expr.right.as_ref() {
                        Expr::Ident(right) => Some(right),
                        _ => None,
                    };

                    if let Some(left) = left_ident
                        && let Some(right) = right_ident
                    {
                        if left.sym == right.sym && left.ctxt == right.ctxt {
                            // Identity assignment e.g. `a = a`.
                            *node = expr.right.as_mut().take();
                            return;
                        }
                    }

                    let is_left_empty_destructuring = match &expr.left {
                        PatOrExpr::Pat(left) => match left.as_ref() {
                            Pat::Array(left) => left.elems.is_empty(),
                            Pat::Object(left) => left.props.is_empty(),
                            _ => false,
                        },
                        PatOrExpr::Expr(_) => false,
                    };

                    if is_left_empty_destructuring {
                        // `[] = RHS` or `{} = RHS` become just `RHS`.
                        // Note: this can potentially change the program's
                        // observable behaviour - if `RHS` is not iterable, then
                        // `[] = RHS` will throw but `RHS` will not.
                        *node = expr.right.as_mut().take();
                        return;
                    }
                }
            }
            Expr::Cond(cond) => {
                cond.visit_mut_children_with(self);

                let condition_value = get_boolean_value(&cond.test, self.unresolved_ctxt);

                if condition_value.is_none() {
                    // If the result nodes are equivalent, then one of the nodes can be
                    // removed and it doesn't matter which.
                    if !cond.cons.eq_ignoring_node_id(&cond.alt) {
                        // We can't remove branches otherwise!

                        return;
                    }
                }

                let cond = unwrap_as!(node.take(), Expr::Cond(c), c);

                // Transform "(a = 2) ? x =2 : y" into "a=2,x=2"
                let branchToKeep = if condition_value == Some(true) {
                    cond.cons
                } else {
                    cond.alt
                };

                let condition_has_side_effects =
                    expr_may_have_side_effects(&cond.test, self.unresolved_ctxt);
                let replacement = if condition_has_side_effects {
                    Expr::Seq(SeqExpr {
                        node_id: self.program_data.new_id_from(cond.node_id),
                        exprs: vec![*cond.test, *branchToKeep],
                    })
                } else {
                    *branchToKeep
                };

                *node = replacement;
            }
            Expr::Seq(seq) => {
                seq.visit_mut_children_with(self);

                let mut i = 0;
                let last_idx = seq.exprs.len() - 1;
                seq.exprs.retain_mut(|expr| {
                    let is_last = i == last_idx;

                    i += 1;

                    if is_last {
                        return true;
                    }

                    if self.simplify_unused_expr(expr) == OptimiseExprResult::Remove {
                        false
                    } else {
                        true
                    }
                });

                if seq.exprs.len() == 1 {
                    *node = seq.exprs[0].take();
                }
            }
            _ => node.visit_mut_children_with(self),
        }
    }
}

/**
 * @return Whether the node is a control flow exit from the current block.
 */
fn isExit(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Break(_) | Stmt::Continue(_) | Stmt::Return(_) | Stmt::Throw(_) => true,
        Stmt::Switch(s) => isSwitchExit(s),
        Stmt::Try(t) => isTryExit(t),
        _ => false,
    }
}

/**
 * @return Whether the block is a control flow exit from the block containing current switch or
 *     try..catch statement.
 */
fn isUnconditionalBlockExit(block: &[Stmt]) -> bool {
    // checkState(!n.getParent().isLabel(), n);

    // Last statement must lead out of the block.
    let Some(last_stmt) = block.last() else {
        return false;
    };

    match last_stmt {
        Stmt::Break(break_stmt) => {
            if break_stmt.label.is_none() {
                return false;
            }
            // Last statement is OK - continue with checking others.
        }
        Stmt::Return(_) | Stmt::Throw(_) => {
            // Last statement is OK - continue with checking others.
        }
        _ => {
            return false;
        }
    }

    // Other statements can be anything except for unlabeled "break". But for simplicity, don't go
    // into inner blocks and complex constructs - instead, allow only the simplest statements.
    for stmt in &block[..block.len() - 1] {
        match stmt {
            Stmt::Break(break_stmt) => {
                if break_stmt.label.is_none() {
                    return false;
                }
                // This break is OK - continue with checking others.
            }
            // TODO: classes should be ok - can't contain breaks, even in static
            // blocks.
            Stmt::Return(_)
            | Stmt::Throw(_)
            | Stmt::Decl(Decl::Fn(_) | Decl::Var(_))
            | Stmt::Expr(_) => {
                // This statement is OK - continue with checking others.
            }
            _ => {
                return false;
            }
        }
    }

    return true;
}

/** Return true if the switch always "exits" (return, throw, etc). */
fn isSwitchExit(switch: &SwitchStmt) -> bool {
    let mut hasDefaultCase = false;

    for (i, case) in switch.cases.iter().enumerate() {
        if case.is_default() {
            hasDefaultCase = true;
        }

        let is_last_case = i == switch.cases.len() - 1;
        if (case.cons.is_empty() || is_last_case) && !isUnconditionalBlockExit(&case.cons) {
            return false;
        }
    }

    hasDefaultCase
}

/** Return true if the try..catch always "exits" (return, throw, etc). */
fn isTryExit(try_stmt: &TryStmt) -> bool {
    // TODO: You can have try..finally right? i.e a try with two children.
    // finally - regardless of the behavior of the other blocks,
    // an exit from the finally with guarantee that behavior.
    if try_stmt.handler.is_some()
        && let Some(finalizer) = &try_stmt.finalizer
    {
        if isUnconditionalBlockExit(&finalizer.stmts) {
            return true;
        }
    }
    // try
    if !isUnconditionalBlockExit(&try_stmt.block.stmts) {
        return false;
    }
    // catch
    try_stmt
        .handler
        .as_ref()
        .is_none_or(|h| isUnconditionalBlockExit(&h.body.stmts))
}

fn areAllCaseTagsLiterals(cases: &[SwitchCase], unresolved_ctxt: SyntaxContext) -> bool {
    for case in cases {
        if let Some(test) = &case.test {
            if !isLiteralValue(test, false, unresolved_ctxt) {
                return false;
            }
        } else {
            // Default case.
        }
    }

    true
}

fn isFirstSwitchMatch(
    foundMatchingCase: bool,
    condition: &Expr,
    tag: &Expr,
    unresolved_ctxt: SyntaxContext,
) -> bool {
    if foundMatchingCase {
        return false;
    }
    evaluateComparison(BinaryOp::EqEqEq, condition, tag, unresolved_ctxt) == Some(true)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::resolver;
    use common::{GLOBALS, Globals, Mark};

    #[test]
    fn test_block() {
        test_transform("{{foo()}}", "foo()");
        test_transform("{foo();{}}", "foo()");
        test_transform("{{foo()}{}}", "foo()");
        test_transform("{{foo()}{bar()}}", "foo();bar()");

        test_transform("{let x}", "let x");
        test_same("function f() {let x}");
        test_transform("{const x = 1}", "const x = 1;");
        test_transform("{x = 2; y = 4; let z;}", "x = 2; y = 4; let z;");
        test_transform("{'hi'; let x;}", "let x;");
        test_transform("{x = 4; {let y}}", "x = 4; let y;");
        test_transform("{class C {}} {class C {}}", "class C {} class C {}");
        test_transform("{label: {var x}}", "label: {var x}");
        // `{label: let x}` is a syntax error
        test_transform("{label: {var x;} let y;}", "label: {var x;} let y;");
    }

    #[test]
    fn test_block_with_many_children() {
        test_transform(
            "{var x; var y; var z; class Foo { constructor() { var a; { var b; } } } }",
            "var x;var y;var z;class Foo { constructor() { var a;var b} }",
        );
        test_transform(
            "{var x; var y; var z; { { var a; { var b; } } } }",
            "var x;var y;var z; var a;var b",
        );
    }

    //       #[test]
    //   fn  testRemoveNoOpLabelledStatement() {
    //     test_transform("a: break a;", "");
    //     test_transform("a: { break a; }", "");

    //     test_transform( //
    //         "a: { break a; console.log('unreachable'); }", //
    //         "");
    //     test_transform( //
    //         "a: { break a; var x = 1; } x = 2;", //
    //         "var x; x = 2;");

    //     test_same("b: { var x = 1; } x = 2;");
    //     test_same("a: b: { var x = 1; } x = 2;");
    //   }

    //   #[test]
    //   fn  testRemoveUselessLabelWithFollowingBreak() {
    //     test_transform("a:b: break b;", "");
    //     // Note: the break is only removed if the parent
    //     // is the break target.
    //     test_same("a:b: break a;");
    //   }

    //   #[test]
    //   fn  testFoldBlock() {
    //     test_transform("{if(false)foo(); {bar()}}", "bar()");
    //     test_transform("{if(false)if(false)if(false)foo(); {bar()}}", "bar()");

    //     test_transform("{'hi'}", "");
    //     test_transform("{x==3}", "");
    //     test_transform("{`hello ${foo}`}", "");
    //     test_transform("{ (function(){x++}) }", "");
    //     test_same("function f(){return;}");
    //     test_transform("function f(){return 3;}", "function f(){return 3}");
    //     test_same("function f(){if(x)return; x=3; return; }");
    //     test_transform("{x=3;;;y=2;;;}", "x=3;y=2");

    //     // Cases to test for empty block.
    //     test_transform("while(x()){x}", "while(x());");
    //     test_transform("while(x()){x()}", "while(x())x()");
    //     test_transform("for(x=0;x<100;x++){x}", "for(x=0;x<100;x++);");
    //     test_transform("for(x in y){x}", "for(x in y);");
    //     test_transform("for (x of y) {x}", "for(x of y);");
    //     test_same("for (let x = 1; x <10; x++ ) {}");
    //     test_same("for (var x = 1; x <10; x++ ) {}");
    //   }

    //   #[test]
    //   fn  testFoldBlockWithDeclaration_notNormalized() {
    //     disableNormalize();
    //     disableComputeSideEffects();

    //     test_same("{let x}");
    //     test_same("function f() {let x}");
    //     test_same("{const x = 1}");
    //     test_same("{x = 2; y = 4; let z;}");
    //     test_transform("{'hi'; let x;}", "{let x}");
    //     test_transform("{x = 4; {let y}}", "x = 4; {let y}");
    //     test_same("{class C {}} {class C {}}");
    //     test_transform("{label: var x}", "label: var x");
    //     // `{label: let x}` is a syntax error
    //     test_same("{label: var x; let y;}");
    //   }

    //   #[test]
    //   fn  testFoldBlockWithDeclaration_normalized() {
    //     test_transform("{let x}", "let x");
    //     test_same("function f() {let x}");
    //     test_transform("{const x = 1}", "const x = 1;");
    //     test_transform("{x = 2; y = 4; let z;}", "x = 2; y = 4; let z;");
    //     test_transform("{'hi'; let x;}", "let x;");
    //     test_transform("{x = 4; {let y}}", "x = 4; let y;");
    //     test_transform("{class C {}} {class C {}}", "class C {} class C$jscomp$1 {}");
    //     test_transform("{label: var x}", "label: var x");
    //     // `{label: let x}` is a syntax error
    //     test_transform("{label: var x; let y;}", "label: var x; let y;");
    //   }

    //   /** Try to remove spurious blocks with multiple children */
    //   #[test]
    //   fn  testFoldBlocksWithManyChildren() {
    //     test_transform("function f() { if (false) {} }", "function f(){}");
    //     test_transform("function f() { { if (false) {} if (true) {} {} } }", "function f(){}");
    //     test_transform(
    //         "{var x; var y; var z; class Foo { constructor() { var a; { var b; } } } }",
    //         "var x;var y;var z;class Foo { constructor() { var a;var b} }");
    //     test_transform("{var x; var y; var z; { { var a; { var b; } } } }", "var x;var y;var z; var a;var b");
    //   }

    //   #[test]
    //   fn  testIf() {
    //     test_transform("if (1){ x=1; } else { x = 2;}", "x=1");
    //     test_transform("if (false){ x = 1; } else { x = 2; }", "x=2");
    //     test_transform("if (undefined){ x = 1; } else { x = 2; }", "x=2");
    //     test_transform("if (null){ x = 1; } else { x = 2; }", "x=2");
    //     test_transform("if (void 0){ x = 1; } else { x = 2; }", "x=2");
    //     test_transform("if (void foo()){ x = 1; } else { x = 2; }", "foo();x=2");
    //     test_transform("if (false){ x = 1; } else if (true) { x = 3; } else { x = 2; }", "x=3");
    //     test_transform("if (x){ x = 1; } else if (false) { x = 3; }", "if(x)x=1");
    //   }

    #[test]
    fn test_conditional_expression() {
        test_transform("true ? a() : b()", "a()");
        test_transform("false ? a() : b()", "b()");

        test_transform("a() ? b() : true", "a() && b()");
        test_transform("a() ? true : b()", "a() || b()");

        test_transform("(a = true) ? b() : c()", "a = true, b()");
        test_transform("(a = false) ? b() : c()", "a = false, c()");
        test_transform(
            "do {f()} while((a = true) ? b() : c())",
            "do {f()} while((a = true) , b())",
        );
        test_transform(
            "do {f()} while((a = false) ? b() : c())",
            "do {f()} while((a = false) , c())",
        );

        test_transform("var x = (true) ? 1 : 0", "var x=1");
        test_transform(
            "var y = (true) ? ((false) ? 12 : (cond ? 1 : 2)) : 13",
            "var y=cond?1:2",
        );

        test_same("var z=x?void 0:y()");
        test_same("z=x?void 0:y()");
        test_same("z*=x?void 0:y()");

        test_same("var z=x?y():void 0");
        test_same("(w?x:void 0).y=z");
        test_same("(w?x:void 0).y+=z");

        test_transform("y = (x ? void 0 : void 0)", "y = void 0");
        test_transform("y = (x ? f() : f())", "y = f()");
        test_transform("(function(){}) ? function(){} : function(){}", "");

        test_transform("1 ? 2 : 3", "");

        test_transform("x ? a() : 3", "x && a()");

        test_transform("x ? 2 : a()", "x || a()");

        test_same("x ? a() : b()");

        test_transform("a() ? 1 : 2", "a()");

        test_transform("a() ? b() : 2", "a() && b()");

        test_transform("a() ? 1 : b()", "a() || b()");

        test_same("a() ? b() : c()");

        test_transform("true ? a() : (function f() {})()", "a()");
        test_transform(
            "false ? a() : (function f() {alert(x)})()",
            "(function f() {alert(x)})()",
        );
        test_transform("((function () {}), true) ? a() : b()", "a()");
        test_transform(
            "((function () {alert(x)})(), true) ? a() : b()",
            "(function(){alert(x)})(),a()",
        );
    }

    //   #[test]
    //   fn  testConstantConditionWithSideEffect1() {
    //     test_transform("if (b=true) x=1;", "b=true;x=1");
    //     test_transform("if (b=/ab/) x=1;", "b=/ab/;x=1");
    //     test_transform("if (b=/ab/){ x=1; } else { x=2; }", "b=/ab/;x=1");
    //     test_transform("var b;b=/ab/;if(b)x=1;", "var b;b=/ab/;x=1");
    //     test_same("var b;b=f();if(b)x=1;");
    //     test_transform("var b=/ab/;if(b)x=1;", "var b=/ab/;x=1");
    //     test_same("var b=f();if(b)x=1;");
    //     test_same("b=b++;if(b)x=b;");
    //     test_transform("(b=0,b=1);if(b)x=b;", "b=0,b=1;if(b)x=b;");
    //     test_transform("b=1;if(foo,b)x=b;", "b=1;x=b;");
    //     test_same("b=1;if(foo=1,b)x=b;");
    //   }

    #[test]
    fn testConstantConditionWithSideEffect2() {
        test_transform("(b=true)?x=1:x=2;", "b=true,x=1");
        test_transform("(b=false)?x=1:x=2;", "b=false,x=2");
        // test_transform("if (b=/ab/) x=1;", "b=/ab/;x=1");
        // test_transform("var b;b=/ab/;(b)?x=1:x=2;", "var b;b=/ab/;x=1");
        // test_same("var b;b=f();(b)?x=1:x=2;");
        // test_transform("var b=/ab/;(b)?x=1:x=2;", "var b=/ab/;x=1");
        // test_same("var b=f();(b)?x=1:x=2;");
    }

    //   #[test]
    //   fn  testConstantConditionWithSideEffect_coalesce() {
    //     test_transform("b = null; b ?? (x = 1)", "b = null; void 0 ?? (x = 1)");
    //     test_transform("b = undefined; b ?? (x = 1)", "b = undefined; void 0 ?? (x = 1)");
    //     test_transform("b = (fn(), null); b ?? (x = 1)", "b = (fn(), null); void 0 ?? (x = 1)");

    //     test_transform("b = 34; b ?? (x = 1)", "b = 34; 0 ?? (x = 1)");
    //     test_transform("b = 'test'; b ?? (x = 1)", "b = 'test'; 0 ?? (x = 1)");
    //     test_transform("b = []; b ?? (x = 1)", "b = []; 0 ?? (x = 1)");
    //     test_transform("b = (fn(), 0); b ?? (x = 1)", " b= (fn(), 0); 0 ?? (x = 1)");

    //     test_same("b = fn(); b ?? (x = 1)");
    //   }

    //   #[test]
    //   fn  testVarLifting() {
    //     test_transform("if(true)var a", "var a");
    //     test_transform("if(false)var a", "var a");

    //     // More var lifting tests in PeepholeIntegrationTests
    //   }

    //   #[test]
    //   fn  testLetConstLifting() {
    //     test_transform("if(true) {const x = 1}", "const x = 1;");
    //     test_transform("if(false) {const x = 1}", "");
    //     test_transform("if(true) {let x}", "let x;");
    //     test_transform("if(false) {let x}", "");
    //     test_transform("if(false) {const x = 1;  function f() { return x; }}", "");
    //   }

    //   #[test]
    //   fn  testLetConstLifting_removePartOfBlock() {
    //     test_transform(
    //         "
    //         function f() {
    //           return 0;
    //           let x = 0;
    //           x++;
    //         }
    //         ",
    //         "
    //         function f() {
    //           let x;
    //           return 0;
    //         }
    //         ");
    //     test_transform(
    //         "
    //         function f() {
    //           return 0;
    //           const [x, y, [[z]]] = 1;
    //         }
    //         ",
    //         "
    //         function f() {
    //           let x;
    //           let y;
    //           let z;
    //           return 0;
    //         }
    //         ");
    //     test_transform(
    //         "
    //         function f() {
    //           return 0;
    //           const C = class Bar {};
    //         }
    //         ",
    //         "
    //         function f() {
    //           let C;
    //           return 0;
    //         }
    //         ");
    //   }

    //   #[test]
    //   fn  testLetConstLifting_removePartOfBlock_withHoistedFunction() {
    //     test_transform(
    //         "
    //         function f() {
    //           return 0;
    //           // Everything after this is dead code, except for the function declaration, which is
    //           // hoisted. `foo` could in theory reference x, y, or C. Add a stub 'let' declaration for
    //           // each to avoid violating the invariant that all NAME nodes in the AST are declared.
    //           const x = 1;
    //           let y;
    //           class C {}
    //           function foo() {}
    //         }
    //         ",
    //         "
    //         function f() {
    //           function foo() {}
    //           let C;
    //           let y;
    //           let x;
    //           return 0;
    //         }
    //         ");
    //     test_transform(
    //         "
    //         function f(param) {
    //           return 0;
    //           // everything after this is dead code.
    //           if (param) {
    //             function foo() {}
    //             const x = 1;
    //             let y;
    //             class C {}
    //           }
    //         }
    //         ",
    //         "
    //         function f(param) {
    //           return 0;
    //         }
    //         ");
    //     test_transform(
    //         "
    //         function f(param) {
    //           if (param) {
    //             return 0;
    //             const x = 1;
    //           }
    //           return 1;
    //         }
    //         ",
    //         "
    //         function f(param) {
    //           if (param) {
    //             let x;
    //             return 0;
    //           }
    //           return 1;
    //         }
    //         ");
    //   }

    //   #[test]
    //   fn  testFoldUselessFor() {
    //     test_transform("for(;false;) { foo() }", "");
    //     test_transform("for(;void 0;) { foo() }", "");
    //     test_transform("for(;undefined;) { foo() }", "");
    //     test_transform("for(;true;) foo() ", "for(;;) foo() ");
    //     test_same("for(;;) foo()");
    //     test_transform("for(;false;) { var a = 0; }", "var a");
    //     test_transform("for(;false;) { const a = 0; }", "");
    //     test_transform("for(;false;) { let a = 0; }", "");

    //     // Make sure it plays nice with minimizing
    //     test_transform("for(;false;) { foo(); continue }", "");

    //     test_transform("l1:for(;false;) {  }", "");
    //   }

    //   #[test]
    //   fn  testFoldUselessDo() {
    //     test_transform("do { foo() } while(false);", "foo()");
    //     test_transform("do { foo() } while(void 0);", "foo()");
    //     test_transform("do { foo() } while(undefined);", "foo()");
    //     test_same("do { foo() } while(true);");
    //     test_transform("do { var a = 0; } while(false);", "var a=0");

    //     test_transform("do { var a = 0; } while(!{a:foo()});", "var a=0;foo()");

    //     // Can't fold with break or continues.
    //     test_same("do { foo(); continue; } while(0)");
    //     test_same("do { try { foo() } catch (e) { break; } } while (0);");
    //     test_same("do { foo(); break; } while(0)");
    //     test_transform("do { for (;;) {foo(); continue;} } while(0)", "for (;;) {foo(); continue;}");
    //     test_same("l1: do { for (;;) { foo() } } while(0)");
    //     test_transform("do { switch (1) { default: foo(); break} } while(0)", "foo();");
    //     test_transform(
    //         "do { switch (1) { default: foo(); continue} } while(0)",
    //         "do { foo(); continue } while(0)");

    //     test_transform(
    //         "l1: { do { x = 1; break l1; } while (0); x = 2; }", //
    //         "l1: { x = 1; break l1; }");

    //     test_transform("do { x = 1; } while (x = 0);", "x = 1; x = 0;");
    //     test_transform(
    //         "let x = 1; (function() { do { let x = 2; } while (x = 10, false); })();",
    //         "let x = 1; (function() { let x$jscomp$1 = 2; x = 10 })();");
    //   }

    //   #[test]
    //   fn  testFoldEmptyDo() {
    //     test_transform("do { } while(true);", "for (;;);");
    //   }

    //   #[test]
    //   fn  testMinimizeLoop_withConstantCondition_vanillaFor() {
    //     test_transform("for(;true;) foo()", "for(;;) foo()");
    //     test_transform("for(;0;) foo()", "");
    //     test_transform("for(;0.0;) foo()", "");
    //     test_transform("for(;NaN;) foo()", "");
    //     test_transform("for(;null;) foo()", "");
    //     test_transform("for(;undefined;) foo()", "");
    //     test_transform("for(;'';) foo()", "");
    //   }

    //   #[test]
    //   fn  testMinimizeLoop_withConstantCondition_doWhile() {
    //     test_transform("do { foo(); } while (true)", "do { foo(); } while (true);");
    //     test_transform("do { foo(); } while (0)", "foo();");
    //     test_transform("do { foo(); } while (0.0)", "foo();");
    //     test_transform("do { foo(); } while (NaN)", "foo();");
    //     test_transform("do { foo(); } while (null)", "foo();");
    //     test_transform("do { foo(); } while (undefined)", "foo();");
    //     test_transform("do { foo(); } while ('')", "foo();");
    //   }

    #[test]
    fn testFoldConstantCommaExpressions() {
        // test_transform("if (true, false) {foo()}", "");
        // test_transform("if (false, true) {foo()}", "foo()");
        test_transform("true, foo()", "foo()");
        test_transform("true, foo?.()", "foo?.()");
        test_transform("(1 + 2 + ''), foo()", "foo()");
        test_transform("(1 + 2 + ''), foo?.()", "foo?.()");
    }

    #[test]
    fn testRemoveUselessOps1() {
        test_same("(function () { f(); })();");
    }

    #[test]
    fn testCallSideEffectsPreserved() {
        // Functions calls known to be free of side effects are removed.
        test_transform("Math.random()", "");
        test_transform("Math?.random()", "");
        test_transform("Math.random(f() + g())", "f(),g();");
        test_transform("Math?.random(f() + g())", "f(),g();");
        test_transform("Math.random(f(),g(),h())", "f(),g(),h();");
        test_transform("Math?.random(f(),g(),h())", "f(),g(),h();");

        // Calls to functions with unknown side-effects are preserved.
        test_same("f();");
        test_same("f?.();");
        test_same("(function () { f(); })();");
    }

    #[test]
    fn testRemoveUselessOps2() {
        // There are four place where expression results are discarded:
        //  - a top-level expression EXPR_RESULT
        //  - the LHS of a COMMA
        //  - the FOR init expression
        //  - the FOR increment expression

        // We know that this function has no side effects because of the
        // PureFunctionIdentifier.
        // test_transform("(function () {})();", "");

        // Uncalled function expressions are removed
        test_transform("(function () {});", "");
        test_transform("(function f() {});", "");
        test_transform("(function* f() {})", "");
        // ... including any code they contain.
        test_transform("(function () {foo();});", "");

        // Useless operators are removed.
        test_transform("+f()", "f()");
        test_transform("+f?.()", "f?.()");
        test_transform("a=(+f(),g())", "a=(f(),g())");
        test_transform("a=(+f?.(),g())", "a=(f?.(),g())");
        test_transform("a=(true,g())", "a=g()");
        test_transform("f(),true", "f()");
        test_transform("f() + g()", "f(),g()");

        // test_transform("for(;;+f()){}", "for(;;f()){}");
        // test_transform("for(+f();;g()){}", "for(f();;g()){}");
        // test_transform("for(;;Math.random(f(),g(),h())){}", "for(;;f(),g(),h()){}");

        // The optimization cascades into conditional expressions:
        test_transform("g() && +f()", "g() && f()");
        test_transform("g() || +f()", "g() || f()");
        test_transform("x ? g() : +f()", "x ? g() : f()");

        test_transform("+x()", "x()");
        test_transform("+x() * 2", "x()");
        test_transform("-(+x() * 2)", "x()");
        test_transform("2 -(+x() * 2)", "x()");
        test_transform("x().foo", "x()");
        test_same("x().foo()");

        test_same("x++");
        test_same("++x");
        test_same("x--");
        test_same("--x");
        test_same("x = 2");
        test_same("x *= 2");

        // Sanity check, other expression are left alone.
        test_same("function f() {}");
        test_same("var x;");
    }

    #[test]
    fn testOptimizeSwitch() {
        test_transform("switch(a){}", "");
        test_transform("switch(foo()){}", "foo()");
        test_transform("switch(a){default:}", "");
        test_transform("switch(a){default:break;}", "");
        test_transform("switch(a){default:var b;break;}", "var b");
        test_transform("switch(a){case 1: default:}", "");
        test_transform("switch(a){default: case 1:}", "");
        test_transform("switch(a){default: break; case 1:break;}", "");
        test_transform(
            "switch(a){default: var b; break; case 1: var c; break;}",
            "var b; var c;",
        );
        test_transform("var x=1; switch(x) { case 1: var y; }", "var x=1; var y;");

        // Can't remove cases if a default exists and is not the last case.
        test_same("function f() {switch(a){default: return; case 1: break;}}");
        test_same("function f() {switch(1){default: return; case 1: break;}}"); // foldable
        test_same("function f() {switch(a){case 1: foo();}}");
        test_same("function f() {switch(a){case 3: case 2: case 1: foo();}}");

        test_transform(
            "function f() {switch(a){case 2: case 1: default: foo();}}",
            "function f() { foo(); }",
        );
        // In general it's not safe to remove cases that fall through to default
        // when default is not last.
        // TODO: we can do better here since the case tests are all constants.
        test_same("switch(a){case 1: default:break; case 2: foo()}");
        test_same("switch(a){case 1: goo(); default:break; case 2: foo()}");

        // TODO: optimise the useless "case 2"
        test_same("switch(a){case 1: goo(); case 2:break; case 3: foo()}");

        // Can't remove unused code with a "var" in it.
        test_transform("switch(1){case 2: var x=0;}", "var x;");
        test_transform(
            "
switch ('repeated') {
    case 'repeated':
        foo();
        break;
    case 'repeated':
        var x=0;
        break;
}",
            "var x; foo();",
        );

        // Can't remove cases if something useful is done.
        test_same("switch(a){case 1: var c =2; break;}");
        test_same("function f() {switch(a){case 1: return;}}");
        test_same("x:{switch(a){case 1: break x;}}");

        test_transform(
            "
switch ('foo') {
    case 'foo':
        foo();
        break;
    case 'bar':
        bar();
        break;
}",
            "foo();",
        );
        test_transform(
            "
switch ('noMatch') {
case 'foo':
    foo();
    break;
case 'bar':
    bar();
    break;
}",
            "",
        );
        test_transform(
            "
switch ('fallThru') {
    case 'fallThru':
        if (foo(123) > 0) {
            foobar(1);
            break;
        }
        foobar(2);
    case 'bar':
        bar();
}",
            "
switch ('fallThru') {
    case 'fallThru':
        if (foo(123) > 0) {
            foobar(1);
            break;
        }
        foobar(2);
        bar();
}",
        );
        test_transform(
            "
switch ('fallThru') {
    case 'fallThru':
        foo();
    case 'bar':
        bar();
}",
            "
foo();
bar();",
        );
        test_transform(
            "
switch ('hasDefaultCase') {
    case 'foo':
        foo();
        break;
    default:
        bar();
        break;
}",
            "bar();",
        );
        test_transform(
            "
switch ('repeated') {
    case 'repeated':
        foo();
        break;
    case 'repeated':
        bar();
        break;
}",
            "foo();",
        );
        test_same(
            "
switch ('foo') {
    case 'bar':
        bar();
        break;
    case notConstant:
        foobar();
        break;
    case 'foo':
        foo();
        break;
}",
        );
        test_transform(
            "
switch (1) {
    case 1:
        foo();
        break;
    case 2:
        bar();
        break;
}",
            "foo();",
        );
        test_transform(
            "
switch (1) {
    case 1.1:
        foo();
        break;
    case 2:
        bar();
        break;
}",
            "",
        );
        test_transform(
            "
switch (0) {
    case NaN:
        foobar();
        break;
    case -0.0:
        foo();
        break;
    case 2:
        bar();
        break;
}",
            "foo();",
        );
        //         test_same(
        //             "
        // switch ('\\v') {
        //     case '\\u000B':
        //         foo();
        // }",
        //         );
        test_transform(
            "
switch ('empty') {
    case 'empty':
    case 'foo':
        foo();
}",
            "foo()",
        );

        test_transform(
            "
let x;
switch (use(x)) {
    default: {let y;}
}",
            "
let x;
use(x);
let y;",
        );

        test_transform(
            "
let x;
switch (use?.(x)) {
    default: {let y;}
}",
            "
let x;
use?.(x);
let y;",
        );

        test_transform(
            "
let x;
switch (use(x)) {
    default: let y;
}",
            "
let x;
use(x);
let y;",
        );
    }

    #[test]
    fn testOptimizeSwitchBug335145701() {
        test_same(
            "
function foo() { alert('foo()'); }
switch (1) {
    case 1: break;
    case foo(): break;
}",
        );

        test_same(
            "
function foo() { alert('foo()'); }
switch (1) {
    case 0: break;
    case 1: break;
    case foo(): break;
}",
        );

        test_same(
            "
function foo() { alert('foo()'); }
switch (1) {
    case 0: alert('bar'); break;
    case 1: break;
    case foo(): break;
}",
        );

        test_same(
            "
function foo() { alert('foo()'); return 1; }
switch (1) {
    case 0: break;
    case foo(): break;
    case 2: break;
}",
        );

        test_transform(
            "
function foo() { alert('foo()'); }
switch (1) {
    case foo(): break;
    case (0,1): break;
}",
            "
function foo() { alert('foo()'); }
switch (1) {
    case foo(): break;
    case 1: break;
}",
        );

        test_same(
            "
function foo() { alert('foo()'); }
switch (x) {
    case 1: break;
    case foo(): break;
}",
        );

        test_transform(
            "
// not valid to remove the useless case 1,
// it would cause the default to run and it has side-effects
switch (1) {
    case 1: break;
    default:
    bar();
    break;
}",
            "",
        );

        test_same(
            "
function foo() { alert('foo()'); }
switch (1) {
    case 0: alert('bar'); break;
    case 1: break;
    case foo(): break;
}",
        );

        test_same(
            "
function foo() { alert('foo()'); }
switch (bar()) {
    case 1: break;
    case foo(): break;
}",
        );

        test_transform(
            "
// is not valid to remove the first useless case 1,
// because it matches and the second should not run
switch (1) {
    case 1: break;
    case 1: bar(); break;
}",
            "",
        );
    }

    #[test]
    fn testOptimizeSwitchBug11536863() {
        test_transform(
            "
outer: {
    switch (2) {
        case 2:
            f();
            break outer;
        }
}",
            "outer: {f(); break outer;}",
        );
    }

    // `a[b]` could trigger a getter or setter, and have side effects. However, we always assume it
    // does not (even though it's unsound) because the code size cost of assuming all GETELEM nodes
    // have side effects is unacceptable.
    #[test]
    fn testUnusedGetElemRemoved() {
        test_transform("a[b]", "");
        test_transform("a?.[b]", "");
    }

    #[test]
    fn testOptimizeSwitch2() {
        test_transform(
            "
outer: switch (2) {
    case 2:
        f();
        break outer;
}",
            "outer: {f(); break outer;}",
        );
    }

    #[test]
    fn testOptimizeSwitch3() {
        test_transform(
            "
switch (1) {
    case 1:
    case 2:
    case 3: {
        break;
    }
    case 4:
    case 5:
    case 6:
    default:
        fail('Should not get here');
        break;
}",
            "",
        );
    }

    #[test]
    fn testOptimizeSwitchWithLabellessBreak() {
        test_transform(
            "
function f() {
    switch('x') {
        case 'x': var x = 1; break;
        case 'y': break;
    }
}",
            "function f() { var x = 1; }",
        );

        // TODO: Optimise switch to if.
        test_same(
            "
function f() {
    switch(x) {
        case 'y': break;
        default: var x = 1;
    }
}
            ",
        );

        test_transform(
            "
var exit;
switch ('a') {
    case 'a':
        break;
    default:
        exit = 21;
        break;
}
switch(exit) {
    case 21: throw 'x';
    default : console.log('good');
}",
            "
var exit;
switch(exit) {
    case 21: throw 'x';
    default : console.log('good');
}
            ",
        );

        test_transform(
            "
let x = 1;
switch('x') {
    case 'x': let x = 2; break;
}",
            "
let x = 1;
let x = 2
            ",
        );
    }

    //     #[test]
    //     fn testOptimizeSwitchWithLabelledBreak() {
    //         test_transform(
    //             "
    // function f() {
    //     label:
    //     switch('x') {
    //         case 'x': break label;
    //         case 'y': throw f;
    //     }
    // }",
    //             "function f() { }",
    //         );

    //         test_transform(
    //             "
    // function f() {
    //     label:
    //     switch('x') {
    //         case 'x': break label;
    //         default: throw f;
    //     }
    // }",
    //             "function f() { }",
    //         );
    //     }

    #[test]
    fn testOptimizeSwitchWithReturn() {
        test_transform(
            "
function f() {
    switch('x') {
        case 'x': return 1;
        case 'y': return 2;
    }
}",
            "function f() { return 1; }",
        );

        test_transform(
            "
function f() {
    let x = 1;
    switch('x') {
        case 'x': { let x = 2; } return 3;
        case 'y': return 4;
    }
}",
            // TODO: need to run a hygiene pass...
            "
function f() {
    let x = 1;
    let x = 2;
    return 3;
}",
        );
    }

    #[test]
    fn testOptimizeSwitchWithThrow() {
        test_transform(
            "
function f() {
    switch('x') {
        case 'x': throw f;
        case 'y': throw f;
    }
}",
            "function f() { throw f; }",
        );
    }

    #[test]
    fn testOptimizeSwitchWithContinue() {
        test_transform(
            "
function f() {
    for (;;) {
        switch('x') {
            case 'x': continue;
            case 'y': continue;
        }
    }
}",
            "function f() { for (;;) { continue; } }",
        );
    }

    #[test]
    fn testOptimizeSwitchWithDefaultCaseWithFallthru() {
        test_same(
            "
function f() {
    switch(a) {
        case 'x':
        case foo():
        default: return 3
    }
}",
        );
    }

    // GitHub issue #1722: https://github.com/google/closure-compiler/issues/1722
    #[test]
    fn testOptimizeSwitchWithDefaultCase() {
        test_transform(
            "
function f() {
    switch('x') {
        case 'x': return 1;
        case 'y': return 2;
        default: return 3
    }
}",
            "function f() { return 1; }",
        );

        test_transform(
            "
switch ('hasDefaultCase') {
    case 'foo':
        foo();
        break;
    default:
        bar();
        break;
}",
            "bar();",
        );

        test_same("switch (x) { default: if (a) { break; } bar(); }");

        // Potentially foldable
        test_same(
            "
switch (x) {
    case x:
        foo();
        break;
    default:
        if (a) { break; }
        bar();
}",
        );

        //         test_transform(
        //             "
        // switch ('hasDefaultCase') {
        //     case 'foo':
        //         foo();
        //         break;
        //     default:
        //         if (true) { break; }
        //         bar();
        // }",
        //             "",
        //         );

        test_transform(
            "
switch ('hasDefaultCase') {
    case 'foo':
        foo();
        break;
    default:
        if (a) { break; }
        bar();
}",
            "switch ('hasDefaultCase') { default: if (a) { break; } bar(); }",
        );

        test_transform(
            "
l: switch ('hasDefaultCase') {
    case 'foo':
        foo();
        break;
    default:
        if (a) { break l; }
        bar();
        break;
}",
            "l:{ if (a) { break l; } bar(); }",
        );

        test_transform(
            "
switch ('hasDefaultCase') {
    case 'foo':
        bar();
        break;
    default:
        foo();
        break;
}",
            "foo();",
        );

        test_transform("switch (a()) { default: bar(); break;}", "a(); bar();");

        test_transform("switch (a?.()) { default: bar(); break;}", "a?.(); bar();");

        test_transform("switch (a()) { default: break; bar();}", "a();");

        test_transform(
            "
loop:
for (;;) {
    switch (a()) {
    default:
        bar();
        break loop;
    }
}",
            "loop: for (;;) { a(); bar(); break loop; }",
        );
    }

    // #[test]
    // fn testTreatSwitchAsExit() {
    //     test_transform(
    //         "a: {switch(x){ case 1: case 2: break a; default: foo(); break a;} bar(); }",
    //         "a: {switch(x){ case 1: case 2: break a; default: foo(); break a;} }",
    //     );
    // }

    #[test]
    fn testDontTreatSwitchAsExit() {
        test_same("a: {switch(x){ case 1:break a; default: b: {foo(); break b;}} bar(); }");

        test_same("a: {switch(x){ case 1:break a; default: foo(); break;} bar(); }");

        test_same("a: {b: { switch(x){ case 1:break a; default: foo(); break b;} } bar(); }");

        test_same("a: {switch(x){ case 1: b: { foo(); break b; } default: break a; } bar(); }");

        test_same("a: {switch(x){ case 1: break a; } bar(); }");

        test_same("a: {switch(x){ case 1: if (y) { break; } default: break a;} bar(); }");

        test_same("a: {switch(x){ case 1: if (y) { break; } break a; default: break a;} bar(); }");
    }

    //   #[test]
    //   fn  testTreatTryAsExit() {
    //     test_transform(
    //         "a: {try { foo(); break a; } catch (e) { foo(); break a; } bar(); }",
    //         "a: {try { foo(); break a; } catch (e) { foo(); break a; } }");

    //     test_transform(
    //         "a: {try { foo(); } finally { foo(); break a; } bar(); }",
    //         "a: {try { foo(); } finally { foo(); break a; } }");

    //     test_transform(
    //         "a: {try { foo(); break a; } finally { foo(); } bar(); }",
    //         "a: {try { foo(); break a; } finally { foo(); } }");
    //   }

    #[test]
    fn testDontTreatTryAsExit() {
        test_same("a: {try { foo(); break a; } catch (e) { foo(); } bar(); }");

        test_same("a: {try { foo(); break a; } catch (e) { } bar(); }");

        test_same("a: {try { foo(); } catch (e) { foo(); break a; } bar(); }");

        test_same("a: {try { foo(); } finally { foo(); } bar(); }");

        test_same("a: {try { b: { foo(); break b; } } finally { foo(); } bar(); }");

        test_same("a: { b: { try { foo(); break a; } finally { foo(); } } bar(); }");
    }

    #[test]
    fn testRemoveNumber() {
        test_transform("3", "");
    }

    #[test]
    fn testRemoveVarGet1() {
        test_transform("a", "");
    }

    #[test]
    fn testRemoveVarGet2() {
        test_transform("var a = 1;a", "var a = 1");
    }

    #[test]
    fn testRemoveUnusedGetProp() {
        test_transform("var a = {};a.b", "var a = {}");
    }

    #[test]
    fn testRemoveUnusedOptChainGetProp() {
        test_transform("var a = {};a?.b", "var a = {}");
    }

    #[test]
    fn testRemoveUnusedGetProp2() {
        test_transform("var a = {};a.b=1;a.b", "var a = {};a.b=1");
    }

    #[test]
    fn testRemoveUnusedOptChainGetProp2() {
        test_transform("var a = {};a.b=1;a?.b", "var a = {};a.b=1");
    }

    #[test]
    fn testRemovePrototypeGet1() {
        test_transform("var a = {};a.prototype.b", "var a = {}");
    }

    #[test]
    fn testRemoveOptChainPrototypeGet1() {
        test_transform("var a = {};a?.prototype.b", "var a = {}");
    }

    #[test]
    fn testRemovePrototypeGet2() {
        test_transform(
            "var a = {};a.prototype.b = 1;a.prototype.b",
            "var a = {};a.prototype.b = 1",
        );
    }

    #[test]
    fn testRemoveOptChainPrototypeGet2() {
        test_transform(
            "var a = {};a.prototype.b = 1;a?.prototype.b",
            "var a = {};a.prototype.b = 1",
        );
    }

    #[test]
    fn testNotRemovePrototypeGet2() {
        test_same("var a = {};a.prototype.b = 1; let x = a.prototype.b");
    }

    #[test]
    fn testNotRemoveOptChainPrototypeGet2() {
        test_same("var a = {};a.prototype.b = 1; let x = a?.prototype.b");
    }

    #[test]
    fn testRemoveAdd1() {
        test_transform("1 + 2", "");
    }

    #[test]
    fn testNoRemoveVar1() {
        test_same("var a = 1");
    }

    #[test]
    fn testNoRemoveVar2() {
        test_same("var a = 1, b = 2");
    }

    #[test]
    fn testNoRemoveAssign1() {
        test_same("a = 1");
    }

    #[test]
    fn testNoRemoveAssign2() {
        test_same("a = b = 1");
    }

    #[test]
    fn testNoRemoveAssign3() {
        test_transform("1 + (a = 2)", "a = 2");
    }

    #[test]
    fn testNoRemoveAssign4() {
        test_same("x.a = 1");
    }

    #[test]
    fn testNoRemoveAssign5() {
        test_same("x.a = x.b = 1");
    }

    #[test]
    fn testNoRemoveAssign6() {
        test_transform("1 + (x.a = 2)", "x.a = 2");
    }

    #[test]
    fn testNoRemoveCall1() {
        test_same("a()");
    }

    #[test]
    fn testNoRemoveOptChainCall1() {
        test_same("a?.()");
    }

    #[test]
    fn testNoRemoveCall2() {
        test_transform("a()+b()", "a(),b()");
    }

    #[test]
    fn testNoRemoveOptChainCall2() {
        test_transform("a?.()+b?.()", "a?.(),b?.()");
    }

    #[test]
    fn testNoRemoveCall3() {
        test_same("a() && b()");
    }

    #[test]
    fn testNoRemoveOptChainCall3() {
        test_same("a?.() && b?.()");
    }

    #[test]
    fn testNoRemoveCall4() {
        test_same("a() || b()");
    }

    #[test]
    fn testNoRemoveOptChainCall4() {
        test_same("a?.() || b?.()");
    }

    #[test]
    fn testNoRemoveCall4NullishCoalesce() {
        test_same("a() ?? b()");
    }

    #[test]
    fn testNoRemoveOptChainCall4NullishCoalesce() {
        test_same("a?.() ?? b?.()");
    }

    #[test]
    fn testNoRemoveCall5NullishCoalesce() {
        test_transform("a() ?? 1", "a()");
    }

    #[test]
    fn testNoRemoveOptChainCall5NullishCoalesce() {
        test_transform("a?.() ?? 1", "a?.()");
    }

    #[test]
    fn testNoRemoveCall6NullishCoalesce() {
        test_same("1 ?? a()");
    }

    #[test]
    fn testNoRemoveCall5() {
        test_transform("a() || 1", "a()");
    }

    #[test]
    fn testNoRemoveCall6() {
        test_same("1 || a()");
    }

    #[test]
    fn testNoRemoveThrow1() {
        test_same("function f(){throw a()}");
    }

    #[test]
    fn testNoRemoveThrow2() {
        test_same("function f(){throw a}");
    }

    #[test]
    fn testNoRemoveThrow3() {
        test_same("function f(){throw 10}");
    }

    //   #[test]
    //   fn  testRedundantIfRemoved() {
    //     test_transform("if(x()) 1", "x()");
    //   }

    //   #[test]
    //   fn  testRemoveInControlStructure3() {
    //     test_transform("for(1;2;3) 4", "for(;;);");
    //   }

    #[test]
    fn testShortCircuit1() {
        test_same("1 && a()");
    }

    #[test]
    fn testShortCircuit2NullishCoalesce() {
        test_transform("1 ?? a() ?? 2", "1 ?? a()");
    }

    #[test]
    fn testShortCircuit3NullishCoalesce() {
        test_transform("a() ?? 1 ?? 2", "a()");
    }

    #[test]
    fn testShortCircuit4NullishCoalesce() {
        test_same("a() ?? 1 ?? b()");
    }

    #[test]
    fn testShortCircuit2() {
        test_transform("1 && a() && 2", "1 && a()");
    }

    #[test]
    fn testShortCircuit3() {
        test_transform("a() && 1 && 2", "a()");
    }

    #[test]
    fn testShortCircuit4() {
        test_same("a() && 1 && b()");
    }

    #[test]
    fn testComplex1() {
        test_transform("1 && a() + b() + c()", "1 && (a(), b(), c())");
    }

    #[test]
    fn testComplex2() {
        test_transform("1 && (a() ? b() : 1)", "1 && (a() && b())");
    }

    #[test]
    fn testComplex3() {
        test_transform("1 && (a() ? b() : 1 + c())", "1 && (a() ? b() : c())");
    }

    #[test]
    fn testComplex4() {
        test_transform("1 && (a() ? 1 : 1 + c())", "1 && (a() || c())");
    }

    #[test]
    fn testComplex5() {
        // can't simplify LHS of short circuit statements with side effects
        test_same("(a() ? 1 : 1 + c()) && foo()");
    }

    #[test]
    fn testNoRemoveFunctionDeclaration1() {
        test_same("function foo(){}");
    }

    #[test]
    fn testNoRemoveFunctionDeclaration2() {
        test_same("var foo = function (){}");
    }

    #[test]
    fn testNoSimplifyFunctionArgs1() {
        test_same("f(1 + 2, 3 + g())");
    }

    #[test]
    fn testNoSimplifyFunctionArgs2() {
        test_same("1 && f(1 + 2, 3 + g())");
    }

    #[test]
    fn testNoSimplifyFunctionArgs3() {
        test_same("1 && foo(a() ? b() : 1 + c())");
    }

    #[test]
    fn testNoRemoveInherits1() {
        test_same("var a = {}; this.b = {}; var goog = {}; goog.inherits(b, a)");
    }

    //   #[test]
    //   fn  testNoRemoveInherits2() {
    //     test_transform(
    //         "var a = {}; this.b = {}; var goog = {}; goog.inherits(b, a) + 1",
    //         "var a = {}; this.b = {}; var goog = {}; goog.inherits(b, a)");
    //   }

    #[test]
    fn testNoRemoveInherits3() {
        test_same("this.a = {}; var b = {}; b.inherits(a);");
    }

    //   #[test]
    //   fn  testNoRemoveInherits4() {
    //     test_transform("this.a = {}; var b = {}; b.inherits(a) + 1;", "this.a = {}; var b = {}; b.inherits(a)");
    //   }

    //   #[test]
    //   fn  testRemoveFromLabel1() {
    //     test_transform("LBL: void 0", "");
    //   }

    //   #[test]
    //   fn  testRemoveFromLabel2() {
    //     test_transform("LBL: foo() + 1 + bar()", "LBL: foo(),bar()");
    //   }

    #[test]
    fn testCall() {
        test_same("foo(0)");
        // We use a function with no side-effects, otherwise the entire invocation would be preserved.
        test_transform("Math.sin(0);", "");
        test_transform("1 + Math.sin(0);", "");
    }

    #[test]
    fn testCall_containingSpread() {
        // We use a function with no side-effects, otherwise the entire invocation would be preserved.
        test_transform("Math.sin(...c)", "([...c])");
        test_transform("Math.sin(4, ...c, a)", "([...c])");
        test_transform("Math.sin(foo(), ...c, bar())", "[foo(), ...c, bar()]");
        test_transform("Math.sin(...a, b, ...c)", "[...a, ...c]");
        test_transform("Math.sin(...b, ...c)", "[...b, ...c]");
    }

    #[test]
    fn testOptChainCall_containingSpread() {
        // We use a function with no side-effects, otherwise the entire invocation would be preserved.
        test_transform("Math?.sin(...c)", "([...c])");
        test_transform("Math?.sin(4, ...c, a)", "([...c])");
        test_transform("Math?.sin(foo(), ...c, bar())", "[foo(), ...c, bar()]");
        test_transform("Math?.sin(...a, b, ...c)", "[...a, ...c]");
        test_transform("Math?.sin(...b, ...c)", "[...b, ...c]");
    }

    #[test]
    fn testNew() {
        test_same("new foo(0)");
        // We use a function with no side-effects, otherwise the entire invocation would be preserved.
        test_transform("new Date;", "");
        test_transform("1 + new Date;", "");
    }

    #[test]
    fn testNew_containingSpread() {
        // We use a function with no side-effects, otherwise the entire invocation would be preserved.
        test_transform("new Date(...c)", "([...c])");
        test_transform("new Date(4, ...c, a)", "([...c])");
        test_transform("new Date(foo(), ...c, bar())", "[foo(), ...c, bar()]");
        test_transform("new Date(...a, b, ...c)", "[...a, ...c]");
        test_transform("new Date(...b, ...c)", "[...b, ...c]");
    }

    #[test]
    fn testTaggedTemplateLit_simpleTemplate() {
        test_same("foo`Simple`");
        // We use a function with no side-effects, otherwise the entire invocation would be preserved.
        test_transform("Math.sin`Simple`", "");
        test_transform("1 + Math.sin`Simple`", "");
    }

    #[test]
    fn testTaggedTemplateLit_substitutingTemplate() {
        test_same("foo`Complex ${butSafe}`");
        // We use a function with no side-effects, otherwise the entire invocation would be preserved.
        test_transform("Math.sin`Complex ${butSafe}`", "");
        test_transform("Math.sin`Complex ${andDangerous()}`", "andDangerous()");
    }

    #[test]
    fn testFoldAssign() {
        // test_transform("x=x", "");
        test_same("x=xy");
        test_same("x=x + 1");
        test_same("x.a=x.a");
        test_transform("var y=(x=x)", "var y=x");
        test_transform("y=1 + (x=x)", "y=1 + x");
    }

    //   #[test]
    //   fn  testTryCatchFinally() {
    //     test_same("try {foo()} catch (e) {bar()}");
    //     test_same("try { try {foo()} catch (e) {bar()}} catch (x) {bar()}");
    //     test_transform("try {var x = 1} finally {}", "var x = 1;");
    //     test_same("try {var x = 1} finally {x()}");
    //     test_transform( //
    //         "function f() { return; try{ var x = 1; }finally{} }", "function f() { var x; return; }");
    //     test_transform("try {} finally {x()}", "x()");
    //     test_transform("try {} catch (e) { bar()} finally {x()}", "x()");
    //     test_transform("try {} catch (e) { bar()}", "");
    //     test_transform("try {} catch (e) { var a = 0; } finally {x()}", "var a; x()");
    //     test_transform("try {} catch (e) {}", "");
    //     test_transform("try {} finally {}", "");
    //     test_transform("try {} catch (e) {} finally {}", "");
    //     test_transform("L1:try {} catch (e) {} finally {}", "");
    //     test_transform("L2:L1:try {} catch (e) {} finally {}", "");
    //   }

    #[test]
    fn testObjectLiteral() {
        test_transform("({})", "");
        test_transform("({a:1})", "");
        test_transform("({a:foo()})", "foo()");
        test_transform("({'a':foo()})", "foo()");
        // Object-spread may trigger getters, but we assume they are
        // side-effect-free.
        test_transform("({...a})", "");
        test_transform("({...foo()})", "foo()");
    }

    #[test]
    fn testArrayLiteral() {
        test_transform("([])", "");
        test_transform("([1])", "");
        test_transform("([a])", "");
        test_transform("([foo()])", "foo()");
    }

    #[test]
    fn testArrayLiteral_containingSpread() {
        test_same("([...c])");
        test_transform("([4, ...c, a])", "([...c])");
        test_transform("([foo(), ...c, bar()])", "[foo(), ...c, bar()]");
        test_transform("([...a, b, ...c])", "[...a, ...c]");
        test_same("([...b, ...c])"); // It would also be fine if the spreads were split apart.
    }

    #[test]
    fn testAwait() {
        test_same("async function f() { await something(); }");
        test_same("async function f() { await some.thing(); }");
    }

    //   #[test]
    //   fn  testEmptyPatternInDeclarationRemoved() {
    //     test_transform("var [] = [];", "");
    //     test_transform("let [] = [];", "");
    //     test_transform("const [] = [];", "");
    //     test_transform("var {} = [];", "");
    //     test_transform("var [] = foo();", "foo()");
    //   }

    //   #[test]
    //   fn  testEmptyArrayPatternInAssignRemoved() {
    //     test_transform("({} = {});", "");
    //     test_transform("({} = foo());", "foo()");
    //     test_transform("[] = [];", "");
    //     test_transform("[] = foo();", "foo()");
    //   }

    #[test]
    fn testEmptyPatternInParamsNotRemoved() {
        test_same("function f([], a) {}");
        test_same("function f({}, a) {}");
    }

    #[test]
    fn testEmptyPatternInForOfLoopNotRemoved() {
        test_same("for (let [] of foo()) {}");
        test_same("for (const [] of foo()) {}");
        test_same("for ([] of foo()) {}");
        test_same("for ({} of foo()) {}");
    }

    //   #[test]
    //   fn  testEmptySlotInArrayPatternRemoved() {
    //     test_transform("[,,] = foo();", "foo()");
    //     test_transform("[a,b,,] = foo();", "[a,b] = foo();");
    //     test_transform("[a,[],b,[],[]] = foo();", "[a,[],b] = foo();");
    //     test_transform("[a,{},b,{},{}] = foo();", "[a,{},b] = foo();");
    //     test_transform("function f([,,,]) {}", "function f([]) {}");
    //     test_same("[[], [], [], ...rest] = foo()");
    //   }

    //   #[test]
    //   fn  testEmptySlotInArrayPatternWithDefaultValueMaybeRemoved() {
    //     test_transform("[a,[] = 0] = [];", "[a] = [];");
    //     test_same("[a,[] = foo()] = [];");
    //   }

    //   #[test]
    //   fn  testEmptyKeyInObjectPatternRemoved() {
    //     test_transform("const {f: {}} = {};", "");
    //     test_transform("const {f: []} = {};", "");
    //     test_transform("const {f: {}, g} = {};", "const {g} = {};");
    //     test_transform("const {f: [], g} = {};", "const {g} = {};");
    //     test_same("const {[foo()]: {}} = {};");
    //   }

    //   #[test]
    //   fn  testEmptyKeyInObjectPatternWithDefaultValueMaybeRemoved() {
    //     test_transform("const {f: {} = 0} = {};", "");
    //     // In theory the following case could be reduced to `foo()`, but that gets more complicated to
    //     // implement for object patterns with multiple keys with side effects.
    //     // Instead the pass backs off for any default with a possible side effect
    //     test_same("const {f: {} = foo()} = {};");
    //   }

    #[test]
    fn testEmptyKeyInObjectPatternNotRemovedWithObjectRest() {
        test_same("const {f: {}, ...g} = foo()");
        test_same("const {f: [], ...g} = foo()");
    }

    //   #[test]
    //   fn  testUndefinedDefaultParameterRemoved() {
    //     test_transform(
    //         "function f(x=undefined,y) {  }", //
    //         "function f(x,y)             {  }");
    //     test_transform(
    //         "function f(x,y=undefined,z) {  }", //
    //         "function f(x,y          ,z) {  }");
    //     test_transform(
    //         "function f(x=undefined,y=undefined,z=undefined) {  }", //
    //         "function f(x,          y,          z)           {  }");
    //   }

    //   #[test]
    //   fn  testPureVoidDefaultParameterRemoved() {
    //     test_transform(
    //         "function f(x = void 0) {  }", //
    //         "function f(x         ) {  }");
    //     test_transform(
    //         "function f(x = void \"XD\") {  }", //
    //         "function f(x              ) {  }");
    //     test_transform(
    //         "function f(x = void f()) {  }", //
    //         "function f(x)            {  }");
    //   }

    #[test]
    fn testNoDefaultParameterNotRemoved() {
        test_same("function f(x,y) {  }");
        test_same("function f(x) {  }");
        test_same("function f() {  }");
    }

    #[test]
    fn testEffectfulDefaultParameterNotRemoved() {
        test_same("function f(x = void console.log(1)) {  }");
        test_same("function f(x = void f()) { alert(x); }");
    }

    //   #[test]
    //   fn  testDestructuringUndefinedDefaultParameter() {
    //     test_transform(
    //         "function f({a=undefined,b=1,c}) {  }", //
    //         "function f({a          ,b=1,c}) {  }");
    //     test_transform(
    //         "function f({a={},b=0}=undefined) {  }", //
    //         "function f({a={},b=0}) {  }");
    //     test_transform(
    //         "function f({a=undefined,b=0}) {  }", //
    //         "function f({a,b=0}) {  }");
    //     test_transform(
    //         " function f({a: {b = undefined}}) {  }", //
    //         " function f({a: {b}}) {  }");
    //     test_same("function f({a,b}) {  }");
    //     test_same("function f({a=0, b=1}) {  }");
    //     test_same("function f({a=0,b=0}={}) {  }");
    //     test_same("function f({a={},b=0}={}) {  }");
    //   }

    //   #[test]
    //   fn  testUndefinedDefaultObjectPatterns() {
    //     test_transform(
    //         "const {a = undefined} = obj;", //
    //         "const {a} = obj;");
    //     test_transform(
    //         "const {a = void 0} = obj;", //
    //         "const {a} = obj;");
    //   }

    //   #[test]
    //   fn  testDoNotRemoveGetterOnlyAccess() {
    //     test_same(
    //         "
    //         var a = {
    //           get property() {}
    //         };
    //         a.property;
    //         ");

    //     test_same(
    //         "
    //         var a = {
    //           get property() {}
    //         };
    //         a?.property;
    //         ");

    //     test_same(
    //         "
    //         var a = {};
    //         Object.defineProperty(a, 'property', {
    //           get() {}
    //         });
    //         a.property;
    //         ");

    //     test_same(
    //         "
    //         var a = {};
    //         Object.defineProperty(a, 'property', {
    //           get() {}
    //         });
    //         a?.property;
    //         ");
    //   }

    //   #[test]
    //   fn  testDoNotRemoveNestedGetterOnlyAccess() {
    //     test_same(
    //         "
    //         var a = {
    //           b: { get property() {} }
    //         };
    //         a.b.property;
    //         ");
    //   }

    //   #[test]
    //   fn  testRemoveAfterNestedGetterOnlyAccess() {
    //     test_transform(
    //         "
    //         var a = {
    //           b: { get property() {} }
    //         };
    //         a.b.property.d.e;
    //         ",
    //         "
    //         var a = {
    //           b: { get property() {} }
    //         };
    //         a.b.property;
    //         ");
    //   }

    //   #[test]
    //   fn  testFoldLabelledEmptyBlock() {
    //     test_transform("a:{}", "");
    //     test_transform("a:b:{}", "");
    //     test_transform("a:b:c:{}", "");
    //   }

    //   #[test]
    //   fn  testRetainSetterOnlyAccess() {
    //     test_same(
    //         "
    //         var a = {
    //           set property(v) {}
    //         };
    //         a.property;
    //         ");

    //     test_same(
    //         "
    //         var a = {
    //           set property(v) {}
    //         };
    //         a?.property;
    //         ");
    //   }

    //   #[test]
    //   fn  testDoNotRemoveGetterSetterAccess() {
    //     test_same(
    //         "
    //         var a = {
    //           get property() {},
    //           set property(x) {}
    //         };
    //         a.property;
    //         ");
    //   }

    //   #[test]
    //   fn  testDoNotRemoveSetSetterToGetter() {
    //     test_same(
    //         "
    //         var a = {
    //           get property() {},
    //           set property(x) {}
    //         };
    //         a.property = a.property;
    //         ");
    //   }

    //   #[test]
    //   fn  testDoNotRemoveAccessIfOtherPropertyIsGetter() {
    //     test_same(
    //         "
    //         var a = {
    //           get property() {}
    //         };
    //         var b = {
    //           property: 0,
    //         };
    //         // This pass should be conservative and not remove this since it sees a getter for
    //         // "property"
    //         b.property;
    //         ");

    //     test_same(
    //         "
    //         var a = {};
    //         Object.defineProperty(a, 'property', {
    //           get() {}
    //         });
    //         var b = {
    //           property: 0,
    //         };
    //         b.property;
    //         ");
    //   }

    //   #[test]
    //   fn  testFunctionCallReferencesGetterIsNotRemoved() {
    //     test_same(
    //         "
    //         var a = {
    //           get property() {}
    //         };
    //         function foo() { a.property; }
    //         foo();
    //         ");
    //   }

    //   #[test]
    //   fn  testFunctionCallReferencesSetterIsNotRemoved() {
    //     test_same(
    //         "
    //         var a = {
    //           set property(v) {}
    //         };
    //         function foo() { a.property = 0; }
    //         foo();
    //         ");
    //   }

    #[test]
    fn testClassField() {
        test_transform(
            "
class C {
    f1 = (5,2);
}
",
            "
class C {
    f1 = 2;
}
",
        );
    }

    #[test]
    fn testThis() {
        test_transform(
            "
class C {
    constructor() {
    this.f1 = (5,2);
    }
}
",
            "
class C {
    constructor() {
    this.f1 = 2;
    }
}
",
        );
    }

    //   #[test]
    //   fn  testClassStaticBlock() {
    //     test_transform(
    //         "
    //         class C {
    //           static {
    //           }
    //         }
    //         ",
    //         "
    //         class C {
    //         }
    //         ");

    //     test_same(
    //         "
    //         class C {
    //           static {
    //             this.x = 0;
    //           }
    //         }
    //         ");
    //   }

    // #[test]
    // fn testRemoveUnreachableOptionalChainingCall() {
    //     test_transform("(null)?.();", "");
    //     test_transform("(void 0)?.();", "");
    //     test_transform("(undefined)?.();", "");
    //     test_transform("(void 0)?.(0)", "");
    //     test_transform("(void 0)?.(function f() {})", "");
    //     test_transform("(null)?.x;", "");
    //     test_transform("(void 0)?.x;", "");
    //     test_transform("(null)?.['x'];", "");
    //     test_transform("(void 0)?.['x'];", "");
    //     test_transform("(null)?.[x];", "");
    //     test_transform("(void 0)?.[x];", "");
    //     // arguments with unknown side effects are also removed
    //     test_transform("(void 0)?.(f(), g())", "");

    //     // void arguments with unknown side effects are preserved
    //     test_transform("(void f())?.();", "f();");
    //     test_transform("g((void f())?.());", "g(void f());");

    //     test_same("(f(), null)?.()");
    //     test_same("f?.()");
    //     test_transform("a?.x;", "");
    //     test_transform("a?.['x'];", "");
    // }

    #[test]
    fn testRemoveUnusedVoid() {
        // remove void at statement level
        test_transform("void 0;", "");
        test_transform("void foo();", "foo();");
        // preserve void when passed somewhere else
        test_same("use(void 0);");
        test_same("use(void foo());");
        test_same("use(() => void foo());");

        test_transform("void use(() => void foo());", "use(() => void foo());");
    }

    //   private void testInFn(String js, String expected) {
    //     String pre = "function f() {";
    //     String post = "}";
    //     test(pre + js + post, pre + expected + post);
    //   }

    //   private void testInLoop(String js, String expected) {
    //     testInLoop(js, "", expected);
    //   }

    //   private void testInLoop(String js, String expectedBeforeLoop, String expected) {
    //     String pre = "for (;;) {";
    //     String post = "}";
    //     test(pre + js + post, expectedBeforeLoop + pre + expected + post);
    //   }

    //   #[test]
    //   fn  testRemoveDeadStatements1() {
    //     test("throw 1; x;", "throw 1;");
    //     test("throw 1; alert(1)", "throw 1;");
    //     test("throw 1; var x = 1", "var x; throw 1;");
    //   }

    //   #[test]
    //   fn  testRemoveDeadStatements2() {
    //     testInFn("return; x;", "return;");
    //     testInFn("return; alert(1)", "return;");
    //     testInFn("return; var x = 1", "var x; return;");
    //   }

    //   #[test]
    //   fn  testRemoveDeadStatements3() {
    //     testInLoop("break; x;", "break;");
    //     testInLoop("break; alert(1)", "break;");
    //     testInLoop("break; var x = 1", "var x;", "break;");
    //   }

    //   #[test]
    //   fn  testRemoveDeadStatements4() {
    //     testInLoop("continue; x;", "continue;");
    //     testInLoop("continue; alert(1)", "continue;");
    //     testInLoop("continue; var x = 1", "var x;", "continue;");
    //   }

    //   #[test]
    //   fn  testRemovalRequiresRedeclaration() {
    //     test( //
    //         "while(1) { break; var x = 1}", //
    //         "var x; for(;;) { break }");
    //     test( //
    //         "while(1) { break; var x=1; var y=1 }", //
    //         "var y; var x; for(;;) { break }");
    //     test( //
    //         "while(1) { break; var [x, [[[y]]]] = [];}", //
    //         "var y; var x; for(;;) { break }");
    //   }

    //   #[test]
    //   fn  testRemovalRequiresRedeclaration_normalizeDisabled() {
    //     disableNormalize();
    //     disableComputeSideEffects();
    //     test( //
    //         "while(1) { break; var x = 1}", //
    //         "var x; while (1) { break; }");
    //     test( //
    //         "while(1) { break; var x=1; var y=1 }", //
    //         "var y; var x; while (1) { break; }");
    //     test( //
    //         "while(1) { break; var [x, [[[y]]]] = [];}", //
    //         "var y; var x; while (1) { break; }");
    //   }

    //   #[test]
    //   fn  testRemoveDo() {
    //     test("do { print(1); break } while(1)", "do { print(1); break } while(1)");
    //     test("while(1) { break; do { print(1); break } while(1) }", "for (;;) { break; }");
    //   }

    //   #[test]
    //   fn  testSwitchCase() {
    //     test(
    //         "function f() { switch(x) { case 1: break; default: return 5; foo()}}",
    //         "function f() { switch(x) { case 1: break; default: return 5;}}");
    //     test(
    //         "function f() { switch(x) { default: return; case 1: foo(); bar()}}",
    //         "function f() { switch(x) { default: return; case 1: foo(); bar()}}");
    //     test(
    //         "function f() { switch(x) { default: return; case 1: return 5;bar()}}",
    //         "function f() { switch(x) { default: return; case 1: return 5;}}");
    //   }

    fn test_transform(input: &str, expected: &str) {
        crate::testing::test_transform(
            |mut program, program_data| {
                GLOBALS.set(&Globals::new(), || {
                    let unresolved_mark = Mark::new();
                    let top_level_mark = Mark::new();

                    program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

                    crate::normalize::add_blocks_to_stmt_contexts(&mut program, program_data);

                    let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

                    // TODO: I feel it wouldbe cleaner to only test one
                    // iteration, and test each of the two desired steps
                    // individually e.g. test A->B and B->C rather than A->C.
                    process(&mut program, program_data, unresolved_ctxt);
                    process(&mut program, program_data, unresolved_ctxt);

                    program
                })
            },
            input,
            expected,
        );
    }
    fn test_same(input: &str) {
        test_transform(input, input);
    }
}
