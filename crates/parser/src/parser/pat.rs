//! 13.3.3 Destructuring Binding Patterns
use super::{expression::PatOrExprOrSpread, util::ExprExt, *};
use crate::token::AssignOpToken;
use atoms::js_word;
use global_common::Spanned;

impl<I: Tokens> Parser<I> {
    pub(super) fn parse_opt_binding_ident(&mut self) -> PResult<Option<BindingIdent>> {
        trace_cur!(self, parse_opt_binding_ident);

        if is!(self, BindingIdent) || (self.input.syntax().typescript() && is!(self, "this")) {
            self.parse_binding_ident().map(Some)
        } else {
            Ok(None)
        }
    }

    /// babel: `parseBindingIdentifier`
    ///
    /// spec: `BindingIdentifier`
    pub(super) fn parse_binding_ident(&mut self) -> PResult<BindingIdent> {
        trace_cur!(self, parse_binding_ident);

        // "yield" and "await" is **lexically** accepted.
        let ident = self.parse_ident(true, true)?;
        if ident.sym == js_word!("arguments") || ident.sym == js_word!("eval") {
            self.emit_strict_mode_err(ident.span, SyntaxError::EvalAndArgumentsInStrict);
        }
        if self.ctx().in_async && ident.sym == js_word!("await") {
            self.emit_err(ident.span, SyntaxError::ExpectedIdent);
        }
        if self.ctx().in_generator && ident.sym == js_word!("yield") {
            self.emit_err(ident.span, SyntaxError::ExpectedIdent);
        }

        Ok(BindingIdent::from_ident(ident, node_id!(self)))
    }

    pub(super) fn parse_binding_pat_or_ident(&mut self) -> PResult<Pat> {
        trace_cur!(self, parse_binding_pat_or_ident);

        match *cur!(self, true)? {
            tok!("yield") | Word(..) => self.parse_binding_ident().map(Pat::Ident),
            tok!('[') => self.parse_array_binding_pat(),
            tok!('{') => self.parse_object(),
            // tok!('(') => {
            //     bump!(self);
            //     let pat = self.parse_binding_pat_or_ident()?;
            //     expect!(self, ')');
            //     Ok(pat)
            // }
            _ => unexpected!(self, "yield, an identifier, [ or {"),
        }
    }

    /// babel: `parseBindingAtom`
    pub(super) fn parse_binding_element(&mut self) -> PResult<Pat> {
        trace_cur!(self, parse_binding_element);

        let start = self.input.cur_pos();
        let left = self.parse_binding_pat_or_ident()?;

        if self.input.eat(&tok!('=')) {
            let right = self.include_in_expr(true).parse_assignment_expr()?;

            if self.ctx().in_declare {
                self.emit_err(span!(self, start), SyntaxError::TS2371);
            }

            return Ok(Pat::Assign(AssignPat {
                node_id: node_id!(self),
                span: span!(self, start),
                left: Box::new(left),
                right,
            }));
        }

        Ok(left)
    }

    fn parse_array_binding_pat(&mut self) -> PResult<Pat> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!('['));

        let mut elems = vec![];
        let mut comma = 0;

        while !eof!(self) && !self.input.is(&tok!(']')) {
            if self.input.eat(&tok!(',')) {
                comma += 1;
                continue;
            }
            if comma > 0 {
                // One comma is used for separating elements
                let cnt = if elems.is_empty() { comma } else { comma - 1 };
                elems.reserve(cnt);
                for _ in 0..cnt {
                    elems.push(None)
                }
                comma = 0;
            }
            let start = self.input.cur_pos();

            if self.input.eat(&tok!("...")) {
                let dot3_token = span!(self, start);

                let pat = self.parse_binding_pat_or_ident()?;
                let pat = Pat::Rest(RestPat {
                    node_id: node_id!(self),
                    span: span!(self, start),
                    dot3_token,
                    arg: Box::new(pat),
                });
                elems.push(Some(pat));
                // Trailing comma isn't allowed
                break;
            } else {
                elems.push(self.parse_binding_element().map(Some)?);
            }
        }

        expect!(self, ']');
        let optional = (self.input.syntax().dts() || self.ctx().in_declare) && eat!(self, '?');

        Ok(Pat::Array(ArrayPat {
            node_id: node_id!(self),
            span: span!(self, start),
            elems,
        }))
    }

    pub(super) fn eat_any_ts_modifier(&mut self) -> PResult<bool> {
        let has_modifier = self.syntax().typescript()
            && matches!(
                *cur!(self, false)?,
                Word(Word::Ident(
                    js_word!("public")
                        | js_word!("protected")
                        | js_word!("private")
                        | js_word!("readonly")
                ))
            )
            && (peeked_is!(self, IdentName) || peeked_is!(self, '{') || peeked_is!(self, '['));
        if has_modifier {
            let _ = self.parse_ts_modifier(&["public", "protected", "private", "readonly"]);
        }

        Ok(has_modifier)
    }

    /// spec: 'FormalParameter'
    ///
    /// babel: `parseAssignableListItem`
    pub(super) fn parse_formal_param_pat(&mut self) -> PResult<Pat> {
        let start = self.input.cur_pos();

        let has_modifier = self.eat_any_ts_modifier()?;

        let pat_start = self.input.cur_pos();
        let mut pat = self.parse_binding_element()?;
        let mut opt = false;

        if self.input.syntax().typescript() {
            if eat!(self, '?') {
                match pat {
                    Pat::Ident(_) | Pat::Array(_) | Pat::Object(_) => {
                        opt = true;
                    }
                    _ if self.input.syntax().dts() || self.ctx().in_declare => {}
                    _ => {
                        syntax_error!(
                            self,
                            self.input.prev_span(),
                            SyntaxError::TsBindingPatCannotBeOptional
                        );
                    }
                }
            }

            match pat {
                Pat::Array(ArrayPat { ref mut span, .. })
                | Pat::Ident(BindingIdent {
                    id: Ident { ref mut span, .. },
                    ..
                })
                | Pat::Object(ObjectPat { ref mut span, .. })
                | Pat::Rest(RestPat { ref mut span, .. }) => {
                    let new_type_ann = self.try_parse_ts_type_ann()?;
                    if new_type_ann.is_some() {
                        *span = Span::new(pat_start, self.input.prev_span().hi, Default::default());
                    }
                }
                Pat::Assign(AssignPat { ref mut span, .. }) => {
                    if (self.try_parse_ts_type_ann()?).is_some() {
                        *span = Span::new(pat_start, self.input.prev_span().hi, Default::default());
                        self.emit_err(*span, SyntaxError::TSTypeAnnotationAfterAssign);
                    }
                }
                Pat::Invalid(..) => {}
                _ => unreachable!("invalid syntax: Pat: {:?}", pat),
            }
        }

        let pat = if self.input.eat(&tok!('=')) {
            // `=` cannot follow optional parameter.
            if opt {
                self.emit_err(pat.span(), SyntaxError::TS1015);
            }

            let right = self.parse_assignment_expr()?;
            if self.ctx().in_declare {
                self.emit_err(span!(self, start), SyntaxError::TS2371);
            }

            Pat::Assign(AssignPat {
                node_id: node_id!(self),
                span: span!(self, start),
                left: Box::new(pat),
                right,
            })
        } else {
            pat
        };

        if has_modifier {
            self.emit_err(span!(self, start), SyntaxError::TS2369);
            return Ok(pat);
        }

        Ok(pat)
    }

    pub(super) fn parse_constructor_params(
        &mut self,
    ) -> PResult<(Vec<Param>, Vec<(JsWord, Span)>)> {
        let mut first = true;
        let mut params = vec![];
        let mut props = vec![];

        while !eof!(self) && !self.input.is(&tok!(')')) {
            if first {
                first = false;
            } else {
                expect!(self, ',');
                // Handle trailing comma.
                if self.input.is(&tok!(')')) {
                    break;
                }
            }

            let param_start = self.input.cur_pos();
            let decorators = self.parse_decorators(false)?;
            let pat_start = self.input.cur_pos();

            if self.input.eat(&tok!("...")) {
                let dot3_token = span!(self, pat_start);

                let pat = self.parse_binding_pat_or_ident()?;
                let type_ann = if self.input.syntax().typescript() && self.input.is(&tok!(':')) {
                    let cur_pos = self.input.cur_pos();
                    Some(self.parse_ts_type_ann(true, cur_pos)?)
                } else {
                    None
                };

                let pat = Pat::Rest(RestPat {
                    node_id: node_id!(self),
                    span: span!(self, pat_start),
                    dot3_token,
                    arg: Box::new(pat),
                });
                params.push(Param {
                    node_id: node_id!(self),
                    span: span!(self, param_start),
                    decorators,
                    pat,
                });
                break;
            } else {
                let (param, prop) = self.parse_constructor_param(param_start, decorators)?;
                if let Some(prop) = prop {
                    props.push((prop, param.span));
                }
                params.push(param);
            }
        }

        Ok((params, props))
    }

    fn parse_constructor_param(
        &mut self,
        param_start: BytePos,
        decorators: Vec<Decorator>,
    ) -> PResult<(Param, Option<JsWord>)> {
        let (has_accessibility, is_override, readonly) = if self.input.syntax().typescript() {
            let has_accessibility = self.parse_access_modifier()?;
            (
                has_accessibility,
                self.parse_ts_modifier(&["override"])?.is_some(),
                self.parse_ts_modifier(&["readonly"])?.is_some(),
            )
        } else {
            (false, false, false)
        };
        let pat = self.parse_formal_param_pat()?;
        let prop = if !has_accessibility && !is_override && !readonly {
            None
        } else {
            let prop = match &pat {
                Pat::Ident(i) => i.id.sym.clone(),
                Pat::Assign(a) => match a.left.as_ref() {
                    Pat::Ident(i) => i.id.sym.clone(),
                    _ => syntax_error!(self, pat.span(), SyntaxError::TsInvalidParamPropPat),
                },
                node => syntax_error!(self, node.span(), SyntaxError::TsInvalidParamPropPat),
            };
            Some(prop)
        };
        Ok((
            Param {
                node_id: node_id!(self),
                span: span!(self, param_start),
                decorators,
                pat,
            },
            prop,
        ))
    }

    pub(super) fn parse_formal_params(&mut self) -> PResult<Vec<Param>> {
        let mut first = true;
        let mut params = vec![];
        let mut dot3_token = Span::default();

        while !eof!(self) && !self.input.is(&tok!(')')) {
            if first {
                first = false;
            } else {
                if dot3_token.is_dummy() {
                    expect!(self, ',');
                } else {
                    // We are handling error.

                    self.input.eat(&tok!(','));
                }

                // Handle trailing comma.
                if self.input.is(&tok!(')')) {
                    break;
                }
            }

            let param_start = self.input.cur_pos();

            let decorators = self.parse_decorators(false)?;
            let pat_start = self.input.cur_pos();

            let pat = if self.input.eat(&tok!("...")) {
                dot3_token = span!(self, pat_start);

                let mut pat = self.parse_binding_pat_or_ident()?;

                if self.input.eat(&tok!('=')) {
                    let right = self.parse_assignment_expr()?;
                    self.emit_err(pat.span(), SyntaxError::TS1048);
                    pat = Pat::Assign(AssignPat {
                        node_id: node_id!(self),
                        span: span!(self, pat_start),
                        left: Box::new(pat),
                        right,
                    });
                }

                let type_ann = if self.input.syntax().typescript() && is!(self, ':') {
                    let cur_pos = self.input.cur_pos();
                    let ty = self.parse_ts_type_ann(true, cur_pos)?;
                    Some(ty)
                } else {
                    None
                };

                let pat = Pat::Rest(RestPat {
                    node_id: node_id!(self),
                    span: span!(self, pat_start),
                    dot3_token,
                    arg: Box::new(pat),
                });

                if is!(self, ',') {
                    if self.input.peeked_is(&tok!(')')) {
                        syntax_error!(self, SyntaxError::CommaAfterRestElement);
                    } else {
                        syntax_error!(self, pat.span(), SyntaxError::NonLastRestParam);
                    }
                }

                if self.syntax().typescript() && eat!(self, '?') {
                    self.emit_err(self.input.prev_span(), SyntaxError::TS1047);
                }

                pat
            } else {
                self.parse_formal_param_pat()?
            };

            params.push(Param {
                node_id: node_id!(self),
                span: span!(self, param_start),
                decorators,
                pat,
            });
        }

        Ok(params)
    }

    pub(super) fn parse_unique_formal_params(&mut self) -> PResult<Vec<Param>> {
        // FIXME(swc): This is wrong
        self.parse_formal_params()
    }
}

///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatType {
    BindingPat,
    BindingElement,
    /// AssignmentPattern
    AssignPat,
    AssignElement,
}

impl PatType {
    pub fn element(self) -> Self {
        match self {
            PatType::BindingPat | PatType::BindingElement => PatType::BindingElement,
            PatType::AssignPat | PatType::AssignElement => PatType::AssignElement,
        }
    }
}

impl<I: Tokens> Parser<I> {
    /// This does not return 'rest' pattern because non-last parameter cannot be
    /// rest.
    pub(super) fn reparse_expr_as_pat(&mut self, pat_ty: PatType, expr: Box<Expr>) -> PResult<Pat> {
        if let Expr::Invalid(i) = *expr {
            return Ok(Pat::Invalid(i));
        }

        if pat_ty == PatType::AssignPat {
            match *expr {
                Expr::Object(..) | Expr::Array(..) => {
                    // It is a Syntax Error if LeftHandSideExpression is either
                    // an ObjectLiteral or an ArrayLiteral
                    // and LeftHandSideExpression cannot
                    // be reparsed as an AssignmentPattern.
                }

                _ => {
                    self.check_assign_target(&expr, true);
                }
            }
        }

        self.reparse_expr_as_pat_inner(pat_ty, expr)
    }

    pub(super) fn reparse_expr_as_pat_inner(
        &mut self,
        pat_ty: PatType,
        expr: Box<Expr>,
    ) -> PResult<Pat> {
        // In dts, we do not reparse.
        debug_assert!(!self.input.syntax().dts());

        let span = expr.span();

        if pat_ty == PatType::AssignPat {
            match *expr {
                Expr::Object(..) | Expr::Array(..) => {
                    // It is a Syntax Error if LeftHandSideExpression is either
                    // an ObjectLiteral or an ArrayLiteral
                    // and LeftHandSideExpression cannot
                    // be reparsed as an AssignmentPattern.
                }

                _ => match *expr {
                    // It is a Syntax Error if the LeftHandSideExpression is
                    // CoverParenthesizedExpressionAndArrowParameterList:(Expression) and
                    // Expression derives a phrase that would produce a Syntax Error according
                    // to these rules if that phrase were substituted for
                    // LeftHandSideExpression. This rule is recursively applied.
                    Expr::Paren(..) => {
                        return Ok(Pat::Expr(expr));
                    }
                    Expr::Ident(i) => {
                        return Ok(Pat::Ident(BindingIdent::from_ident(i, node_id!(self))))
                    }
                    _ => {
                        return Ok(Pat::Expr(expr));
                    }
                },
            }
        }

        // AssignmentElement:
        //      DestructuringAssignmentTarget Initializer[+In]?
        //
        // DestructuringAssignmentTarget:
        //      LeftHandSideExpression
        if pat_ty == PatType::AssignElement {
            match *expr {
                Expr::Array(..) | Expr::Object(..) => {}

                Expr::Member(..)
                | Expr::Call(..)
                | Expr::New(..)
                | Expr::Lit(..)
                | Expr::Ident(..)
                | Expr::Fn(..)
                | Expr::Class(..)
                | Expr::Tpl(..) => {
                    if !expr.is_valid_simple_assignment_target(self.ctx().strict) {
                        self.emit_err(span, SyntaxError::NotSimpleAssign)
                    }
                    match *expr {
                        Expr::Ident(i) => {
                            return Ok(Pat::Ident(BindingIdent::from_ident(i, node_id!(self))))
                        }
                        _ => {
                            return Ok(Pat::Expr(expr));
                        }
                    }
                }

                // It's special because of optional initializer
                Expr::Assign(..) => {}

                _ => self.emit_err(span, SyntaxError::InvalidPat),
            }
        }

        match *expr {
            Expr::Paren(..) => {
                self.emit_err(span, SyntaxError::InvalidPat);
                Ok(Pat::Invalid(Invalid {
                    node_id: node_id!(self),
                    span,
                }))
            }
            Expr::Assign(
                assign_expr @ AssignExpr {
                    op: AssignOpToken::Assign,
                    ..
                },
            ) => {
                let AssignExpr {
                    span, left, right, ..
                } = assign_expr;
                Ok(Pat::Assign(AssignPat {
                    node_id: node_id!(self),
                    span,
                    left: match left {
                        PatOrExpr::Expr(left) => Box::new(self.reparse_expr_as_pat(pat_ty, left)?),
                        PatOrExpr::Pat(left) => left,
                    },
                    right,
                }))
            }
            Expr::Object(ObjectLit { span, props, .. }) => {
                // {}
                Ok(Pat::Object(ObjectPat {
                    node_id: node_id!(self),
                    span,
                    props: props
                        .into_iter()
                        .map(|prop| {
                            let span = prop.span();
                            match prop {
                                Prop::Shorthand(id) => Ok(ObjectPatProp::Assign(AssignPatProp {
                                    node_id: node_id!(self),
                                    span: id.span(),
                                    key: id,
                                    value: None,
                                })),
                                Prop::KeyValue(kv_prop) => {
                                    Ok(ObjectPatProp::KeyValue(KeyValuePatProp {
                                        node_id: node_id!(self),
                                        key: kv_prop.key,
                                        value: Box::new(self.reparse_expr_as_pat(
                                            pat_ty.element(),
                                            kv_prop.value,
                                        )?),
                                    }))
                                }
                                Prop::Assign(assign_prop) => {
                                    Ok(ObjectPatProp::Assign(AssignPatProp {
                                        node_id: node_id!(self),
                                        span,
                                        key: assign_prop.key,
                                        value: Some(assign_prop.value),
                                    }))
                                }
                                Prop::Spread(SpreadAssignment {
                                    dot3_token, expr, ..
                                }) => {
                                    Ok(ObjectPatProp::Rest(RestPat {
                                        node_id: node_id!(self),
                                        span,
                                        dot3_token,
                                        // FIXME: is BindingPat correct?
                                        arg: Box::new(
                                            self.reparse_expr_as_pat(PatType::BindingPat, expr)?,
                                        ),
                                    }))
                                }
                                _ => syntax_error!(self, prop.span(), SyntaxError::InvalidPat),
                            }
                        })
                        .collect::<PResult<_>>()?,
                }))
            }
            Expr::Ident(ident) => Ok(Pat::Ident(BindingIdent::from_ident(ident, node_id!(self)))),
            Expr::Member(..) => Ok(Pat::Expr(expr)),
            Expr::Array(ArrayLit {
                elems: mut exprs,
                span: array_span,
                ..
            }) => {
                if exprs.is_empty() {
                    return Ok(Pat::Array(ArrayPat {
                        node_id: node_id!(self),
                        span,
                        elems: vec![],
                    }));
                }

                // Trailing comma may exist. We should remove those commas.
                let count_of_trailing_comma =
                    exprs.iter().rev().take_while(|e| e.is_none()).count();

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
                                syntax_error!(self, spread.span(), SyntaxError::NonLastRestParam)
                            }
                        }
                        Some(ExprOrSpread::Expr(expr)) => {
                            params.push(self.reparse_expr_as_pat(pat_ty.element(), expr).map(Some)?)
                        }
                        None => params.push(None),
                    }
                }

                // Now that we are reparsing this array as a pattern, any commas
                // we found directly after a spread element are now errors. We
                // only bother tracking/reporting the first violation.
                if let Some(trailing_comma_span) =
                    self.state.trailing_commas_after_rest.get(&array_span)
                {
                    syntax_error!(
                        self,
                        *trailing_comma_span,
                        SyntaxError::CommaAfterRestElement
                    );
                }

                if count_of_trailing_comma == 0 {
                    let expr = exprs.into_iter().next().unwrap();
                    let last = match expr {
                        // Rest
                        Some(ExprOrSpread::Spread(SpreadElement {
                            dot3_token, expr, ..
                        })) => {
                            // TODO(swc): is BindingPat correct?
                            let expr_span = expr.span();
                            self.reparse_expr_as_pat(pat_ty.element(), expr)
                                .map(|pat| {
                                    Pat::Rest(RestPat {
                                        node_id: node_id!(self),
                                        span: expr_span,
                                        dot3_token,
                                        arg: Box::new(pat),
                                    })
                                })
                                .map(Some)?
                        }
                        Some(ExprOrSpread::Expr(expr)) => {
                            // TODO(swc): is BindingPat correct?
                            self.reparse_expr_as_pat(pat_ty.element(), expr).map(Some)?
                        }
                        // TODO(swc): syntax error if last element is ellison and ...rest exists.
                        None => None,
                    };
                    params.push(last);
                }
                Ok(Pat::Array(ArrayPat {
                    node_id: node_id!(self),
                    span,
                    elems: params,
                }))
            }

            // Invalid patterns.
            // Note that assignment expression with '=' is valid, and handled above.
            Expr::Lit(..) | Expr::Assign(..) => {
                self.emit_err(span, SyntaxError::InvalidPat);
                Ok(Pat::Invalid(Invalid {
                    node_id: node_id!(self),
                    span,
                }))
            }

            Expr::Yield(..) if self.ctx().in_generator => {
                self.emit_err(span, SyntaxError::InvalidPat);
                Ok(Pat::Invalid(Invalid {
                    node_id: node_id!(self),
                    span,
                }))
            }

            _ => {
                self.emit_err(span, SyntaxError::InvalidPat);

                Ok(Pat::Invalid(Invalid {
                    node_id: node_id!(self),
                    span,
                }))
            }
        }
    }

    pub(super) fn parse_paren_items_as_params(
        &mut self,
        mut exprs: Vec<PatOrExprOrSpread>,
    ) -> PResult<Vec<Pat>> {
        let pat_ty = PatType::BindingPat;

        let len = exprs.len();
        if len == 0 {
            return Ok(vec![]);
        }

        let mut params = Vec::with_capacity(len);

        for expr in exprs.drain(..len - 1) {
            match expr {
                PatOrExprOrSpread::ExprOrSpread(ExprOrSpread::Spread(_))
                | PatOrExprOrSpread::Pat(Pat::Rest(..)) => {
                    if self.syntax().early_errors() {
                        syntax_error!(self, expr.span(), SyntaxError::NonLastRestParam)
                    }
                }
                PatOrExprOrSpread::ExprOrSpread(ExprOrSpread::Expr(expr)) => {
                    params.push(self.reparse_expr_as_pat(pat_ty, expr)?)
                }
                PatOrExprOrSpread::Pat(pat) => params.push(pat),
            }
        }

        debug_assert_eq!(exprs.len(), 1);
        let expr = exprs.into_iter().next().unwrap();
        let last = match expr {
            // Rest
            PatOrExprOrSpread::ExprOrSpread(ExprOrSpread::Spread(SpreadElement {
                dot3_token,
                expr,
                ..
            })) => {
                let expr_span = expr.span();
                self.reparse_expr_as_pat(pat_ty, expr).map(|pat| {
                    Pat::Rest(RestPat {
                        node_id: node_id!(self),
                        span: expr_span,
                        dot3_token,
                        arg: Box::new(pat),
                    })
                })?
            }
            PatOrExprOrSpread::ExprOrSpread(ExprOrSpread::Expr(expr)) => {
                self.reparse_expr_as_pat(pat_ty, expr)?
            }
            PatOrExprOrSpread::Pat(pat) => pat,
        };
        params.push(last);

        Ok(params)
    }
}
