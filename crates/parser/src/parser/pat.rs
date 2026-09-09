//! 13.3.3 Destructuring Binding Patterns
use super::{expression::MaybeParenPatOrExprOrSpread, *};
use expression::MaybeParenSpreadElement;
use util::AssignProps;

impl Parser<'_> {
    pub(super) fn parse_opt_binding_ident(&mut self) -> PResult<Option<Ident>> {
        let ctx = self.ctx();
        if (self.input.cur().is_word() && !self.input.cur().is_reserved_word(ctx))
            || (self.input.syntax().typescript() && self.is(tok!("this")))
        {
            self.parse_binding_ident().map(Some)
        } else {
            Ok(None)
        }
    }

    /// babel: `parseBindingIdentifier`
    ///
    /// spec: `BindingIdentifier`
    pub(super) fn parse_binding_ident(&mut self) -> PResult<Ident> {
        // "yield" and "await" is **lexically** accepted.
        let ident = self.parse_ident(true, true)?;
        if ident.name == id_for_built_in!("arguments") || ident.name == id_for_built_in!("eval") {
            self.emit_strict_mode_err(
                get_span!(self, ident.node_id),
                SyntaxError::EvalAndArgumentsInStrict,
            );
        }
        if self.ctx().in_async() && ident.name == id_for_built_in!("await") {
            self.emit_err(get_span!(self, ident.node_id), SyntaxError::ExpectedIdent);
        }
        if self.ctx().in_generator() && ident.name == id_for_built_in!("yield") {
            self.emit_err(get_span!(self, ident.node_id), SyntaxError::ExpectedIdent);
        }

        Ok(ident)
    }

    pub(super) fn parse_binding_pat_or_ident(&mut self) -> PResult<BindingPatOrIdent> {
        match self.input.cur() {
            t if t.is_word() || t == tok!("yield") => self
                .parse_binding_ident()
                .map(|i| BindingPatOrIdent::Ident(BindingIdent::from_ident(i))),
            tok!('[') => self.parse_array_binding_pat().map(BindingPatOrIdent::Array),
            tok!('{') => self
                .parse_object_binding_pat()
                .map(BindingPatOrIdent::Object),
            Token::Error => {
                let error = self.input.expect_error_token_and_bump();
                return Err(error);
            }
            _ => unexpected!(self, "yield, an identifier, [ or {"),
        }
    }

    pub(super) fn parse_binding_element(&mut self) -> PResult<BindingElement> {
        let start = self.input.cur_pos();
        let target = self.parse_binding_pat_or_ident()?;

        let init = if self.eat(tok!('=')) {
            let right = self
                .include_in_expr(true)
                .parse_assignment_expr(&mut AssignProps::Emit)?
                .unwrap();

            if self.ctx().in_declare() {
                self.emit_err(self.span(start), SyntaxError::TS2371);
            }

            Some(right)
        } else {
            None
        };

        Ok(BindingElement {
            node_id: node_id!(self, self.span(start)),
            target,
            init,
        })
    }

    fn parse_array_binding_pat(&mut self) -> PResult<ArrayBindingPat> {
        let start = self.input.cur_pos();

        self.assert_and_bump(tok!('['));

        let mut elems = vec![];
        let mut rest = None;
        let mut comma = 0;

        while !self.is(tok!(']')) {
            if self.eat(tok!(',')) {
                comma += 1;
                continue;
            }
            if comma > 0 {
                // One comma is used for separating elements
                let cnt = if elems.is_empty() { comma } else { comma - 1 };
                elems.reserve(cnt);
                for _ in 0..cnt {
                    elems.push(None);
                }
                comma = 0;
            }
            let start = self.input.cur_pos();

            if self.eat(tok!("...")) {
                let pat = self.parse_binding_pat_or_ident()?;
                rest = Some(BindingRestElement {
                    node_id: node_id!(self, self.span(start)),
                    arg: Box::new(pat.into()),
                });
                // Trailing comma isn't allowed
                break;
            }

            elems.push(self.parse_binding_element().map(Some)?);
        }

        expect!(self, ']');
        // TS optional.
        if self.input.syntax().dts() || self.ctx().in_declare() {
            self.eat(tok!('?'));
        }

        Ok(ArrayBindingPat {
            node_id: node_id!(self, self.span(start)),
            elems,
            rest,
        })
    }

    pub(super) fn eat_any_ts_modifier(&mut self) -> PResult<bool> {
        let has_modifier = self.syntax().typescript()
            && matches!(
                self.input.cur(),
                Token::Public | Token::Protected | Token::Private | Token::Readonly
            )
            && (matches!(self.input.peek(), Some(t) if t.is_word())
                || self.peeked_is(tok!('{'))
                || self.peeked_is(tok!('[')));
        if has_modifier {
            let _ = self.parse_ts_modifier(&[
                Token::Public,
                Token::Protected,
                Token::Private,
                Token::Readonly,
            ]);
        }

        Ok(has_modifier)
    }

    /// spec: 'FormalParameter'
    fn parse_formal_param_pat(&mut self) -> PResult<BindingElement> {
        let start = self.input.cur_pos();

        let has_modifier = self.eat_any_ts_modifier()?;

        let pat_start = self.input.cur_pos();
        let mut pat = self.parse_binding_element()?;

        if self.input.syntax().typescript() {
            let mut opt = false;

            if self.eat(tok!('?')) {
                if pat.init.is_none() {
                    opt = true;
                } else if self.input.syntax().dts() || self.ctx().in_declare() {
                } else {
                    syntax_error!(
                        self,
                        self.input.prev_span(),
                        SyntaxError::TsBindingPatCannotBeOptional
                    );
                }
            }

            if pat.init.is_some() {
                if self.try_parse_ts_type_ann()?.is_some() {
                    let new = Span::new(pat_start, self.input.prev_span().hi);
                    set_span!(self, pat.node_id, new);
                    self.emit_err(new, SyntaxError::TSTypeAnnotationAfterAssign);
                }
            } else {
                let new_type_ann = self.try_parse_ts_type_ann()?;
                if new_type_ann.is_some() {
                    let hi = self.input.prev_span().hi;
                    set_span!(self, pat.node_id, Span::new(pat_start, hi));
                }
            }

            // In TS, the  type annotation can come before the "=" e.g.
            // "function foo(a:number = 1) {}".
            if self.eat(tok!('=')) {
                // `=` cannot follow optional parameter.
                if opt {
                    self.emit_err(get_span!(self, pat.node_id()), SyntaxError::TS1015);
                }

                let right = self.parse_assignment_expr(&mut AssignProps::Emit)?.unwrap();
                if self.ctx().in_declare() {
                    self.emit_err(self.span(start), SyntaxError::TS2371);
                }

                if pat.init.is_none() {
                    pat.init = Some(right);
                }
            }
        }

        if has_modifier {
            self.emit_err(self.span(start), SyntaxError::TS2369);
            return Ok(pat);
        }

        Ok(pat)
    }

    pub(super) fn parse_constructor_params(
        &mut self,
    ) -> PResult<(FunctionParams, Vec<(NameId, Span)>)> {
        let mut first = true;
        let mut params = vec![];
        let mut rest = None;
        let mut props = vec![];

        while !self.is(tok!(')')) {
            if first {
                first = false;
            } else {
                expect!(self, ',');
                // Handle trailing comma.
                if self.is(tok!(')')) {
                    break;
                }
            }

            let param_start = self.input.cur_pos();

            if self.eat(tok!("...")) {
                let pat = self.parse_binding_pat_or_ident()?;
                // Type annotation.
                if self.input.syntax().typescript() && self.is(tok!(':')) {
                    self.parse_ts_type_ann(true)?;
                }

                rest = Some(BindingRestElement {
                    node_id: node_id!(self, self.span(param_start)),
                    arg: Box::new(pat),
                });

                break;
            }

            let (param, prop) = self.parse_constructor_param(param_start)?;
            if let Some(prop) = prop {
                props.push((prop, get_span!(self, param.node_id)));
            }
            params.push(param);
        }

        Ok((
            FunctionParams {
                params,
                rest_param: rest,
            },
            props,
        ))
    }

    fn parse_constructor_param(
        &mut self,
        param_start: BytePos,
    ) -> PResult<(Param, Option<NameId>)> {
        let (has_accessibility, is_override, readonly) = if self.input.syntax().typescript() {
            let has_accessibility = self.parse_access_modifier()?;
            (
                has_accessibility,
                self.parse_ts_modifier(&[Token::Override])?.is_some(),
                self.parse_ts_modifier(&[Token::Readonly])?.is_some(),
            )
        } else {
            (false, false, false)
        };
        let pat = self.parse_formal_param_pat()?;
        let prop = if !has_accessibility && !is_override && !readonly {
            None
        } else {
            let prop = match &pat.target {
                BindingPatOrIdent::Array(_) | BindingPatOrIdent::Object(_) => {
                    syntax_error!(
                        self,
                        get_span!(self, pat.node_id()),
                        SyntaxError::TsInvalidParamPropPat
                    )
                }
                BindingPatOrIdent::Ident(binding_ident) => binding_ident.id.name,
            };
            Some(prop)
        };
        Ok((
            Param {
                node_id: node_id!(self, self.span(param_start)),
                pat,
            },
            prop,
        ))
    }

    pub(super) fn parse_formal_params(&mut self) -> PResult<FunctionParams> {
        let mut first = true;
        let mut params = vec![];
        let mut rest = None;
        let mut seen_dot3 = false;

        while !self.is(tok!(')')) {
            if first {
                first = false;
            } else {
                if !seen_dot3 {
                    expect!(self, ',');
                } else {
                    // We are handling error.

                    self.eat(tok!(','));
                }

                // Handle trailing comma.
                if self.is(tok!(')')) {
                    break;
                }
            }

            let param_start = self.input.cur_pos();

            if self.eat(tok!("...")) {
                seen_dot3 = true;

                let pat = self.parse_binding_pat_or_ident()?;

                if self.eat(tok!('=')) {
                    let _right = self.parse_assignment_expr(&mut AssignProps::Emit)?.unwrap();
                    self.emit_err(get_span!(self, pat.node_id()), SyntaxError::TS1048);
                }

                // Type annotation.
                if self.input.syntax().typescript() && self.is(tok!(':')) {
                    self.parse_ts_type_ann(true)?;
                }

                let pat_span = self.span(param_start);
                rest = Some(BindingRestElement {
                    node_id: node_id!(self, pat_span),
                    arg: Box::new(pat),
                });

                if self.is(tok!(',')) {
                    if self.peeked_is(tok!(')')) {
                        syntax_error!(self, SyntaxError::CommaAfterRestElement);
                    } else {
                        syntax_error!(self, pat_span, SyntaxError::NonLastRestParam);
                    }
                }

                if self.syntax().typescript() && self.eat(tok!('?')) {
                    self.emit_err(self.input.prev_span(), SyntaxError::TS1047);
                }
            } else {
                let pat = self.parse_formal_param_pat()?;

                params.push(Param {
                    node_id: node_id!(self, self.span(param_start)),
                    pat,
                });
            }
        }

        Ok(FunctionParams {
            params,
            rest_param: rest,
        })
    }

    pub(super) fn parse_unique_formal_params(&mut self) -> PResult<FunctionParams> {
        // FIXME(swc): This is wrong
        self.parse_formal_params()
    }
}

impl Parser<'_> {
    pub(super) fn reparse_expr_as_assign_target(&mut self, expr: Box<Expr>) -> AssignTarget {
        match *expr {
            Expr::Member(member_expr) => {
                return AssignTarget::Simple(SimpleAssignTarget::Member(member_expr));
            }
            Expr::Ident(ident) => {
                return AssignTarget::Simple(SimpleAssignTarget::Ident(
                    self.reparse_ident_as_binding_ident(ident),
                ));
            }
            _ => {}
        }

        let parenthesised = self.parenthesised_exprs.contains(&expr.node_id());

        if parenthesised {
            // TODO: better error.
            self.emit_err(get_span!(self, expr.node_id()), SyntaxError::InvalidPat);

            return self.create_invalid_assign_target();
        }

        match *expr {
            Expr::Object(obj) => AssignTarget::AssignmentPat(AssignmentPat::Object(
                self.reparse_object_as_assignment_pat(obj),
            )),
            Expr::Array(array) => AssignTarget::AssignmentPat(AssignmentPat::Array(
                self.reparse_array_as_assignment_pat(array),
            )),

            _ => {
                self.emit_err(get_span!(self, expr.node_id()), SyntaxError::InvalidPat);

                self.create_invalid_assign_target()
            }
        }
    }

    fn reparse_object_as_assignment_pat(&mut self, expr: ObjectLit) -> ObjectAssignmentPat {
        // TODO emit errors for incorrect rest.
        let mut rest = None;
        ObjectAssignmentPat {
            node_id: node_id_from!(self, expr.node_id),
            props: expr
                .props
                .into_iter()
                .filter_map(|prop| match prop {
                    Prop::KeyValue(kv_prop) => {
                        let target = self.reparse_expr_as_assignment_element(kv_prop.value);
                        let lo = get_span!(self, kv_prop.key.node_id()).lo;
                        let hi = get_span!(self, target.node_id()).hi;
                        let span = Span::new(lo, hi);
                        Some(AssignmentProperty {
                            node_id: node_id!(self, span),
                            prop: kv_prop.key,
                            target,
                        })
                    }
                    Prop::Assign(assign_prop) => {
                        let assign_pat = AssignmentElement {
                            node_id: node_id_from!(self, assign_prop.node_id),
                            target: Box::new(AssignTarget::Simple(SimpleAssignTarget::Ident(
                                BindingIdent {
                                    id: assign_prop.key.clone_node(program_data!(self).data()),
                                },
                            ))),
                            init: Some(assign_prop.value),
                        };
                        Some(AssignmentProperty {
                            node_id: node_id_from!(self, assign_prop.node_id),
                            prop: PropName::Ident(assign_prop.key),
                            target: assign_pat,
                        })
                    }
                    Prop::Spread(SpreadAssignment { expr, node_id, .. }) => {
                        rest = Some(AssignmentRest {
                            node_id: node_id_from!(self, node_id),
                            arg: Box::new(self.reparse_expr_as_assign_target(expr)),
                        });

                        None
                    }
                    _ => {
                        self.emit_err(get_span!(self, prop.node_id()), SyntaxError::InvalidPat);
                        None
                    }
                })
                .collect(),
            rest,
        }
    }

    fn reparse_array_as_assignment_pat(&mut self, expr: ArrayLit) -> ArrayAssignmentPat {
        let mut exprs = expr.elems;
        let array_id = expr.node_id;

        let span = get_span!(self, expr.node_id);

        if exprs.is_empty() {
            return ArrayAssignmentPat {
                node_id: node_id!(self, span),
                elems: vec![],
                rest: None,
            };
        }

        // Trailing comma may exist. We should remove those commas.
        let count_of_trailing_comma = exprs.iter().rev().take_while(|e| e.is_none()).count();

        let len = exprs.len();
        let mut params = Vec::with_capacity(exprs.len() - count_of_trailing_comma);

        // Comma or other pattern cannot follow a rest pattern.
        let idx_of_rest_not_allowed = if count_of_trailing_comma == 0 {
            len - 1
        } else {
            // last element is comma, so rest is not allowed for every pattern element.
            len - count_of_trailing_comma
        };

        for expr in exprs.drain(..idx_of_rest_not_allowed) {
            match expr {
                Some(ExprOrSpread::Spread(spread)) => {
                    if self.syntax().early_errors() {
                        self.emit_err(
                            get_span!(self, spread.node_id),
                            SyntaxError::NonLastRestParam,
                        )
                    }
                }
                Some(ExprOrSpread::Expr(expr)) => {
                    params.push(Some(self.reparse_expr_as_assignment_element(expr)));
                }
                None => params.push(None),
            }
        }

        // Now that we are reparsing this array as a pattern, any commas
        // we found directly after a spread element are now errors. We
        // only bother tracking/reporting the first violation.
        if let Some(trailing_comma_span) = self.trailing_commas_after_rest.get(&array_id) {
            self.emit_err(*trailing_comma_span, SyntaxError::CommaAfterRestElement);
        }

        let mut rest = None;

        if count_of_trailing_comma == 0 {
            let expr = exprs.into_iter().next().unwrap();
            match expr {
                // Rest
                Some(ExprOrSpread::Spread(SpreadElement { expr, .. })) => {
                    let pat = self.reparse_expr_as_assign_target(expr);
                    rest = Some(AssignmentRest {
                        node_id: node_id_from!(self, pat.node_id()),
                        arg: Box::new(pat),
                    })
                }
                Some(ExprOrSpread::Expr(expr)) => {
                    params.push(Some(self.reparse_expr_as_assignment_element(expr)));
                }
                // TODO: syntax error if last element is ellison and ...rest exists.
                None => {}
            }
        }
        ArrayAssignmentPat {
            node_id: node_id!(self, span),
            elems: params,
            rest,
        }
    }

    fn reparse_expr_as_assignment_element(&mut self, expr: Box<Expr>) -> AssignmentElement {
        debug_assert!(!self.input.syntax().dts());

        let span = get_span!(self, expr.node_id());

        let parenthesised = self.parenthesised_exprs.contains(&expr.node_id());

        if parenthesised {
            self.emit_err(span, SyntaxError::InvalidPat);
            return self.create_invalid_assignment_element();
        }

        match *expr {
            Expr::Assign(AssignExpr {
                node_id,
                op: AssignOp::Assign,
                left,
                right,
            }) => AssignmentElement {
                node_id: node_id_from!(self, node_id),
                target: left,
                init: Some(right),
            },
            Expr::Object(obj) => AssignmentElement {
                node_id: node_id_from!(self, obj.node_id),
                target: Box::new(AssignTarget::AssignmentPat(AssignmentPat::Object(
                    self.reparse_object_as_assignment_pat(obj),
                ))),
                init: None,
            },
            Expr::Array(array) => AssignmentElement {
                node_id: node_id_from!(self, array.node_id),
                target: Box::new(AssignTarget::AssignmentPat(AssignmentPat::Array(
                    self.reparse_array_as_assignment_pat(array),
                ))),
                init: None,
            },
            Expr::Ident(ident) => AssignmentElement {
                node_id: node_id_from!(self, ident.node_id),
                target: Box::new(AssignTarget::Simple(SimpleAssignTarget::Ident(
                    self.reparse_ident_as_binding_ident(ident),
                ))),
                init: None,
            },
            Expr::Member(member) => AssignmentElement {
                node_id: node_id_from!(self, member.node_id),
                target: Box::new(AssignTarget::Simple(SimpleAssignTarget::Member(member))),
                init: None,
            },

            _ => {
                self.emit_err(span, SyntaxError::InvalidPat);

                self.create_invalid_assignment_element()
            }
        }
    }

    pub(super) fn reparse_expr_as_simple_assign_target(
        &mut self,
        expr: Box<Expr>,
    ) -> SimpleAssignTarget {
        match *expr {
            Expr::Member(member_expr) => SimpleAssignTarget::Member(member_expr),
            Expr::Ident(ident) => {
                SimpleAssignTarget::Ident(self.reparse_ident_as_binding_ident(ident))
            }
            _ => {
                // TODO: error message mentions for-in loop.
                self.emit_err(get_span!(self, expr.node_id()), SyntaxError::TS2406);

                self.create_invalid_simple_assign_target()
            }
        }
    }

    // We don't take `MaybeParen` here since that would require preserving parens while
    // parsing patterns, so we use `state.parenthesised_exprs`.
    /// This does not return 'rest' pattern because non-last parameter cannot be
    /// rest.
    pub(super) fn reparse_expr_as_binding_element(&mut self, expr: Box<Expr>) -> BindingElement {
        debug_assert!(!self.input.syntax().dts());

        let span = get_span!(self, expr.node_id());

        let parenthesised = self.parenthesised_exprs.contains(&expr.node_id());

        if parenthesised {
            self.emit_err(span, SyntaxError::InvalidPat);
            return self.create_invalid_binding_element();
        }

        match *expr {
            Expr::Assign(AssignExpr {
                node_id,
                op: AssignOp::Assign,
                left,
                right,
            }) => match *left {
                AssignTarget::Simple(simple_assign_target) => match simple_assign_target {
                    SimpleAssignTarget::Ident(binding_ident) => BindingElement {
                        node_id: node_id_from!(self, node_id),
                        target: BindingPatOrIdent::Ident(binding_ident),
                        init: Some(right),
                    },
                    SimpleAssignTarget::Member(member_expr) => {
                        self.emit_err(
                            get_span!(self, member_expr.node_id),
                            SyntaxError::InvalidPat,
                        );

                        self.create_invalid_binding_element()
                    }
                },
                AssignTarget::AssignmentPat(left) => match left {
                    AssignmentPat::Array(array_assignment_pat) => BindingElement {
                        node_id: node_id_from!(self, node_id),
                        target: BindingPatOrIdent::Array(
                            self.reparse_array_assignment_pat_as_array_binding_pat(
                                array_assignment_pat,
                            ),
                        ),
                        init: Some(right),
                    },
                    AssignmentPat::Object(object_assignment_pat) => BindingElement {
                        node_id: node_id_from!(self, node_id),
                        target: BindingPatOrIdent::Object(
                            self.reparse_object_assignment_pat_as_object_binding_pat(
                                object_assignment_pat,
                            ),
                        ),
                        init: Some(right),
                    },
                },
            },
            Expr::Object(obj) => BindingElement {
                node_id: node_id_from!(self, obj.node_id),
                target: BindingPatOrIdent::Object(self.reparse_object_as_binding_object(obj)),
                init: None,
            },
            Expr::Ident(ident) => BindingElement {
                node_id: node_id_from!(self, ident.node_id),
                target: BindingPatOrIdent::Ident(self.reparse_ident_as_binding_ident(ident)),
                init: None,
            },
            Expr::Array(array) => BindingElement {
                node_id: node_id_from!(self, array.node_id),
                target: BindingPatOrIdent::Array(self.reparse_array_as_binding_pat(array)),
                init: None,
            },

            _ => {
                self.emit_err(span, SyntaxError::InvalidPat);

                self.create_invalid_binding_element()
            }
        }
    }

    fn reparse_assignment_element_as_binding_element(
        &mut self,
        assignment_element: AssignmentElement,
    ) -> BindingElement {
        match *assignment_element.target {
            AssignTarget::Simple(simple_assign_target) => match simple_assign_target {
                SimpleAssignTarget::Ident(binding_ident) => BindingElement {
                    node_id: node_id_from!(self, assignment_element.node_id),
                    target: BindingPatOrIdent::Ident(binding_ident),
                    init: assignment_element.init,
                },
                SimpleAssignTarget::Member(member_expr) => {
                    self.emit_err(
                        get_span!(self, member_expr.node_id),
                        SyntaxError::InvalidPat,
                    );
                    self.create_invalid_binding_element()
                }
            },
            AssignTarget::AssignmentPat(assignment_pat) => match assignment_pat {
                AssignmentPat::Array(array_assignment_pat) => BindingElement {
                    node_id: node_id_from!(self, assignment_element.node_id),
                    target: BindingPatOrIdent::Array(
                        self.reparse_array_assignment_pat_as_array_binding_pat(
                            array_assignment_pat,
                        ),
                    ),
                    init: assignment_element.init,
                },
                AssignmentPat::Object(object_assignment_pat) => BindingElement {
                    node_id: node_id_from!(self, assignment_element.node_id),
                    target: BindingPatOrIdent::Object(
                        self.reparse_object_assignment_pat_as_object_binding_pat(
                            object_assignment_pat,
                        ),
                    ),
                    init: assignment_element.init,
                },
            },
        }
    }

    fn reparse_object_assignment_pat_as_object_binding_pat(
        &mut self,
        assignment_pat: ObjectAssignmentPat,
    ) -> ObjectBindingPat {
        ObjectBindingPat {
            node_id: node_id_from!(self, assignment_pat.node_id),
            props: assignment_pat
                .props
                .into_iter()
                .map(|prop| {
                    let target = self.reparse_assignment_element_as_binding_element(prop.target);
                    BindingProperty {
                        node_id: node_id_from!(self, prop.node_id),
                        prop: prop.prop,
                        target: Box::new(target),
                    }
                })
                .collect(),
            rest: assignment_pat.rest.map(|rest| {
                let arg = match *rest.arg {
                    AssignTarget::Simple(simple_assign_target) => match simple_assign_target {
                        SimpleAssignTarget::Ident(binding_ident) => binding_ident,
                        SimpleAssignTarget::Member(_) => {
                            self.emit_err(get_span!(self, rest.node_id), SyntaxError::InvalidPat);
                            self.create_invalid_binding_ident()
                        }
                    },
                    AssignTarget::AssignmentPat(_) => {
                        self.emit_err(get_span!(self, rest.node_id), SyntaxError::InvalidPat);
                        self.create_invalid_binding_ident()
                    }
                };
                BindingRestProperty {
                    node_id: node_id_from!(self, rest.node_id),
                    arg: Box::new(arg),
                }
            }),
        }
    }

    fn reparse_array_assignment_pat_as_array_binding_pat(
        &mut self,
        assignment_pat: ArrayAssignmentPat,
    ) -> ArrayBindingPat {
        ArrayBindingPat {
            node_id: node_id_from!(self, assignment_pat.node_id),
            elems: assignment_pat
                .elems
                .into_iter()
                .map(|el| el.map(|el| self.reparse_assignment_element_as_binding_element(el)))
                .collect(),
            rest: assignment_pat.rest.map(|rest| {
                let arg = match *rest.arg {
                    AssignTarget::Simple(simple_assign_target) => match simple_assign_target {
                        SimpleAssignTarget::Ident(binding_ident) => {
                            BindingPatOrIdent::Ident(binding_ident)
                        }
                        SimpleAssignTarget::Member(_) => {
                            self.emit_err(get_span!(self, rest.node_id), SyntaxError::InvalidPat);
                            self.create_invalid_binding_pat_or_ident()
                        }
                    },
                    AssignTarget::AssignmentPat(assignment_pat) => match assignment_pat {
                        AssignmentPat::Array(array_assignment_pat) => BindingPatOrIdent::Array(
                            self.reparse_array_assignment_pat_as_array_binding_pat(
                                array_assignment_pat,
                            ),
                        ),
                        AssignmentPat::Object(object_assignment_pat) => BindingPatOrIdent::Object(
                            self.reparse_object_assignment_pat_as_object_binding_pat(
                                object_assignment_pat,
                            ),
                        ),
                    },
                };
                BindingRestElement {
                    node_id: node_id_from!(self, rest.node_id),
                    arg: Box::new(arg),
                }
            }),
        }
    }

    fn reparse_object_as_binding_object(&mut self, expr: ObjectLit) -> ObjectBindingPat {
        // TODO emit errors for incorrect rest.
        let mut rest = None;
        ObjectBindingPat {
            node_id: node_id_from!(self, expr.node_id),
            props: expr
                .props
                .into_iter()
                .filter_map(|prop| match prop {
                    Prop::KeyValue(kv_prop) => {
                        let target = self.reparse_expr_as_binding_element(kv_prop.value);
                        let lo = get_span!(self, kv_prop.key.node_id()).lo;
                        let hi = get_span!(self, target.node_id()).hi;
                        let span = Span::new(lo, hi);
                        Some(BindingProperty {
                            node_id: node_id!(self, span),
                            prop: kv_prop.key,
                            target: Box::new(target),
                        })
                    }
                    Prop::Assign(assign_prop) => {
                        let target = BindingElement {
                            node_id: node_id_from!(self, assign_prop.node_id),
                            target: BindingPatOrIdent::Ident(BindingIdent {
                                id: assign_prop.key.clone_node(program_data!(self).data()),
                            }),
                            init: Some(assign_prop.value),
                        };
                        Some(BindingProperty {
                            node_id: node_id_from!(self, assign_prop.node_id),
                            prop: PropName::Ident(assign_prop.key),
                            target: Box::new(target),
                        })
                    }
                    Prop::Spread(SpreadAssignment { expr, node_id, .. }) => {
                        rest = Some(BindingRestProperty {
                            node_id: node_id_from!(self, node_id),
                            arg: Box::new(self.reparse_expr_as_binding_ident(*expr)),
                        });

                        None
                    }
                    _ => {
                        self.emit_err(get_span!(self, prop.node_id()), SyntaxError::InvalidPat);
                        None
                    }
                })
                .collect(),
            rest,
        }
    }

    fn reparse_array_as_binding_pat(&mut self, expr: ArrayLit) -> ArrayBindingPat {
        let mut exprs = expr.elems;
        let array_id = expr.node_id;

        let span = get_span!(self, expr.node_id);

        if exprs.is_empty() {
            return ArrayBindingPat {
                node_id: node_id!(self, span),
                elems: vec![],
                rest: None,
            };
        }

        // Trailing comma may exist. We should remove those commas.
        let count_of_trailing_comma = exprs.iter().rev().take_while(|e| e.is_none()).count();

        let len = exprs.len();
        let mut params = Vec::with_capacity(exprs.len() - count_of_trailing_comma);

        // Comma or other pattern cannot follow a rest pattern.
        let idx_of_rest_not_allowed = if count_of_trailing_comma == 0 {
            len - 1
        } else {
            // last element is comma, so rest is not allowed for every pattern element.
            len - count_of_trailing_comma
        };

        for expr in exprs.drain(..idx_of_rest_not_allowed) {
            match expr {
                Some(ExprOrSpread::Spread(spread)) => {
                    if self.syntax().early_errors() {
                        self.emit_err(
                            get_span!(self, spread.node_id),
                            SyntaxError::NonLastRestParam,
                        )
                    }
                }
                Some(ExprOrSpread::Expr(expr)) => {
                    params.push(Some(self.reparse_expr_as_binding_element(expr)));
                }
                None => params.push(None),
            }
        }

        // Now that we are reparsing this array as a pattern, any commas
        // we found directly after a spread element are now errors. We
        // only bother tracking/reporting the first violation.
        if let Some(trailing_comma_span) = self.trailing_commas_after_rest.get(&array_id) {
            self.emit_err(*trailing_comma_span, SyntaxError::CommaAfterRestElement);
        }

        let mut rest = None;

        if count_of_trailing_comma == 0 {
            let expr = exprs.into_iter().next().unwrap();
            match expr {
                // Rest
                Some(ExprOrSpread::Spread(SpreadElement { expr, .. })) => {
                    let pat = self.reparse_expr_as_binding_pat_or_ident(*expr);
                    rest = Some(BindingRestElement {
                        node_id: node_id_from!(self, pat.node_id()),
                        arg: Box::new(pat),
                    })
                }
                Some(ExprOrSpread::Expr(expr)) => {
                    params.push(Some(self.reparse_expr_as_binding_element(expr)))
                }
                // TODO: syntax error if last element is ellison and ...rest exists.
                None => {}
            }
        }
        ArrayBindingPat {
            node_id: node_id!(self, span),
            elems: params,
            rest,
        }
    }

    pub(super) fn reparse_expr_as_binding_pat_or_ident(&mut self, expr: Expr) -> BindingPatOrIdent {
        match expr {
            Expr::Object(obj) => {
                BindingPatOrIdent::Object(self.reparse_object_as_binding_object(obj))
            }
            Expr::Ident(ident) => {
                BindingPatOrIdent::Ident(self.reparse_ident_as_binding_ident(ident))
            }
            Expr::Array(array) => {
                BindingPatOrIdent::Array(self.reparse_array_as_binding_pat(array))
            }
            _ => {
                self.emit_err(get_span!(self, expr.node_id()), SyntaxError::InvalidPat);
                self.create_invalid_binding_pat_or_ident()
            }
        }
    }

    fn reparse_expr_as_binding_ident(&mut self, expr: Expr) -> BindingIdent {
        match expr {
            Expr::Ident(ident) => self.reparse_ident_as_binding_ident(ident),

            _ => {
                self.emit_err(get_span!(self, expr.node_id()), SyntaxError::InvalidPat);
                self.create_invalid_binding_ident()
            }
        }
    }

    fn reparse_ident_as_binding_ident(&mut self, ident: Ident) -> BindingIdent {
        let is_eval_or_arguments =
            ident.name == id_for_built_in!("eval") || ident.name == id_for_built_in!("arguments");

        if is_eval_or_arguments {
            // TODO: this error message only mentions 'arguments'.
            // We should have a different message for eval.
            self.emit_strict_mode_err(get_span!(self, ident.node_id), SyntaxError::TS1100);
        }

        BindingIdent::from_ident(ident)
    }

    pub(super) fn parse_paren_items_as_params(
        &mut self,
        mut exprs: Vec<MaybeParenPatOrExprOrSpread>,
    ) -> PResult<(Vec<BindingElement>, Option<BindingRestElement>)> {
        let len = exprs.len();
        if len == 0 {
            return Ok((vec![], None));
        }

        let mut params = Vec::with_capacity(len);

        for expr in exprs.drain(..len - 1) {
            match expr {
                MaybeParenPatOrExprOrSpread::Spread(MaybeParenSpreadElement {
                    node_id, ..
                })
                | MaybeParenPatOrExprOrSpread::BindingRestElement(BindingRestElement {
                    node_id,
                    ..
                }) => {
                    if self.syntax().early_errors() {
                        let span = get_span!(self, node_id);
                        syntax_error!(self, span, SyntaxError::NonLastRestParam)
                    }
                }
                MaybeParenPatOrExprOrSpread::Expr(expr) => {
                    params.push(self.reparse_expr_as_binding_element(expr.unwrap()));
                }
                MaybeParenPatOrExprOrSpread::BindingElement(pat) => params.push(pat),
            }
        }

        let mut rest = None;

        debug_assert_eq!(exprs.len(), 1);
        let expr = exprs.into_iter().next().unwrap();
        match expr {
            // Rest
            MaybeParenPatOrExprOrSpread::Spread(MaybeParenSpreadElement { expr, .. }) => {
                let pat = self.reparse_expr_as_binding_pat_or_ident(*expr.unwrap());
                rest = Some(BindingRestElement {
                    node_id: node_id_from!(self, pat.node_id()),
                    arg: Box::new(pat),
                });
            }
            MaybeParenPatOrExprOrSpread::Expr(expr) => {
                params.push(self.reparse_expr_as_binding_element(expr.unwrap()))
            }
            MaybeParenPatOrExprOrSpread::BindingElement(pat) => params.push(pat),
            MaybeParenPatOrExprOrSpread::BindingRestElement(binding_rest_element) => {
                rest = Some(binding_rest_element)
            }
        }

        Ok((params, rest))
    }
}
