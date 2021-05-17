//! 13.3.3 Destructuring Binding Patterns
use crate::{
    ast::*,
    parser::{
         expression::PatOrExprOrSpread, util::ExprExt, Parser, Tokens,
    },
    token::{AssignOpToken, Word},
};
use global_common::{BytePos, Span, Spanned};
use swc_atoms::js_word;

impl<'a, I: Tokens> Parser<I> {
    pub(super) fn parse_opt_binding_ident(&mut self) -> Option<BindingIdent> {
        // trace_cur!(self, parse_opt_binding_ident);

        if is!(self, BindingIdent) {
            Some(self.parse_binding_ident())
        } else {
            None
        }

        // if is!(self, BindingIdent) || (self.input.syntax().typescript() && is!(self, "this")) {
        //     Some(self.parse_binding_ident())
        // } else {
        //     None
        // }
    }

    /// babel: `parseBindingIdentifier`
    ///
    /// spec: `BindingIdentifier`
    pub(super) fn parse_binding_ident(&mut self) -> BindingIdent {
        // trace_cur!(self, parse_binding_ident);

        // "yield" and "await" is **lexically** accepted.
        let ident = self.parse_ident(true, true);
        if ident.sym == js_word!("arguments") || ident.sym == js_word!("eval") {
            // self.emit_strict_mode_err(ident.span, SyntaxError::EvalAndArgumentsInStrict);
            panic!("EvalAndArgumentsInStrict at {:?}", ident.span);
        }
        if self.ctx().in_async && ident.sym == js_word!("await") {
            // self.emit_err(ident.span, SyntaxError::ExpectedIdent);
            panic!("ExpectedIdent at {:?}", ident.span);
        }
        if self.ctx().in_generator && ident.sym == js_word!("yield") {
            // self.emit_err(ident.span, SyntaxError::ExpectedIdent);
            panic!("ExpectedIdent at {:?}", ident.span);
        }

        ident.into()
    }

    pub(super) fn parse_binding_pat_or_ident(&mut self) -> Pat {
        // trace_cur!(self, parse_binding_pat_or_ident);

        match *cur!(self, true) {
            tok!("yield") | Word(..) => Pat::from(self.parse_binding_ident()),
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
    pub(super) fn parse_binding_element(&mut self) -> Pat {
        // trace_cur!(self, parse_binding_element);

        let start = self.input.cur_pos();
        let left = self.parse_binding_pat_or_ident();

        if self.input.eat(&tok!('=')) {
            let right = self.include_in_expr(true).parse_assignment_expr();

            if self.ctx().in_declare {
                // self.emit_err(span!(self, start), SyntaxError::TS2371);
                panic!("TS2371 at {:?}", span!(self, start));
            }

            return Pat::Assign(AssignPat {
                span: span!(self, start),
                left: Box::new(left),
                right,
                type_ann: None,
            });
        }

        left
    }

    fn parse_array_binding_pat(&mut self) -> Pat {
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
                elems.extend(std::iter::repeat(None).take(cnt));
                comma = 0;
            }
            let start = self.input.cur_pos();

            if self.input.eat(&tok!("...")) {
                let dot3_token = span!(self, start);

                let pat = self.parse_binding_pat_or_ident();
                let pat = Pat::Rest(RestPat {
                    span: span!(self, start),
                    dot3_token,
                    arg: Box::new(pat),
                    type_ann: None,
                });
                elems.push(Some(pat));
                // Trailing comma isn't allowed
                break;
            } else {
                elems.push(Some(self.parse_binding_element()));
            }
        }

        expect!(self, ']');
        let optional = false;
        // let optional =
        //     (self.input.syntax().dts() || self.ctx().in_declare) && self.input.eat(&tok!('?'));

        Pat::Array(ArrayPat {
            span: span!(self, start),
            elems,
            optional,
            type_ann: None,
        })
    }

    // pub(super) fn eat_any_ts_modifier(&mut self) -> bool {
    //     let has_modifier = self.syntax().typescript()
    //         && match *cur!(self, false)? {
    //             Word(Word::Ident(js_word!("public")))
    //             | Word(Word::Ident(js_word!("protected")))
    //             | Word(Word::Ident(js_word!("private")))
    //             | Word(Word::Ident(js_word!("readonly"))) => true,
    //             _ => false,
    //         }
    //         && (peeked_is!(self, IdentName) || peeked_is!(self, '{') || peeked_is!(self, '['));
    //     if has_modifier {
    //         let _ = self.parse_ts_modifier(&["public", "protected", "private", "readonly"]);
    //     }

    //     return has_modifier;
    // }

    /// spec: 'FormalParameter'
    ///
    /// babel: `parseAssignableListItem`
    pub(super) fn parse_formal_param_pat(&mut self) -> Pat {
        let start = self.input.cur_pos();

        // let has_modifier = self.eat_any_ts_modifier();

        // let pat_start = self.input.cur_pos();
        let pat = self.parse_binding_element();
        let opt = false;

        // if self.input.syntax().typescript() {
        //     if eat!(self, '?') {
        //         match pat {
        //             Pat::Ident(BindingIdent {
        //                 id:
        //                     Ident {
        //                         ref mut optional, ..
        //                     },
        //                 ..
        //             })
        //             | Pat::Array(ArrayPat {
        //                 ref mut optional, ..
        //             })
        //             | Pat::Object(ObjectPat {
        //                 ref mut optional, ..
        //             }) => {
        //                 *optional = true;
        //                 opt = true;
        //             }
        //             _ if self.input.syntax().dts() || self.ctx().in_declare => {}
        //             _ => {
        //                 syntax_error!(
        //                     self,
        //                     self.input.prev_span(),
        //                     SyntaxError::TsBindingPatCannotBeOptional
        //                 );
        //             }
        //         }
        //     }
        //     match pat {
        //         Pat::Array(ArrayPat {
        //             ref mut type_ann,
        //             ref mut span,
        //             ..
        //         })
        //         | Pat::Ident(BindingIdent {
        //             ref mut type_ann,
        //             id: Ident { ref mut span, .. },
        //             ..
        //         })
        //         | Pat::Object(ObjectPat {
        //             ref mut type_ann,
        //             ref mut span,
        //             ..
        //         })
        //         | Pat::Rest(RestPat {
        //             ref mut type_ann,
        //             ref mut span,
        //             ..
        //         }) => {
        //             let new_type_ann = self.try_parse_ts_type_ann();
        //             if new_type_ann.is_some() {
        //                 *span = Span::new(pat_start, self.input.prev_span().hi, Default::default());
        //             }
        //             *type_ann = new_type_ann;
        //         }
        //         Pat::Assign(AssignPat {
        //             ref mut type_ann,
        //             ref mut span,
        //             ..
        //         }) => {
        //             if let Some(_) = self.try_parse_ts_type_ann() {
        //                 *span = Span::new(pat_start, self.input.prev_span().hi, Default::default());
        //                 self.emit_err(*span, SyntaxError::TSTypeAnnotationAfterAssign);
        //             }
        //         }
        //         Pat::Invalid(..) => {}
        //         _ => unreachable!("invalid syntax: Pat: {:?}", pat),
        //     }
        // }

        let pat = if self.input.eat(&tok!('=')) {
            // `=` cannot follow optional parameter.
            if opt {
                // self.emit_err(pat.span(), SyntaxError::TS1015);
                panic!(
                    "Parameter cannot have question mark and initializer at {:?}",
                    pat.span()
                );
            }

            let right = self.parse_assignment_expr();
            if self.ctx().in_declare {
                // self.emit_err(span!(self, start), SyntaxError::TS2371);
                panic!("A parameter initializer is only allowed in a function or constructor implementation. {:?}", span!(self, start));
            }

            Pat::Assign(AssignPat {
                span: span!(self, start),
                left: Box::new(pat),
                type_ann: None,
                right,
            })
        } else {
            pat
        };

        // if has_modifier {
        //     self.emit_err(span!(self, start), SyntaxError::TS2369);
        //     return pat;
        // }

        pat
    }

    pub(super) fn parse_constructor_params(&mut self) -> Vec<ParamOrTsParamProp> {
        let mut first = true;
        let mut params = vec![];

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
            let decorators = self.parse_decorators(false);
            let pat_start = self.input.cur_pos();

            if self.input.eat(&tok!("...")) {
                let dot3_token = span!(self, pat_start);

                let pat = self.parse_binding_pat_or_ident();
                let type_ann = None;
                // let type_ann = if self.input.syntax().typescript() && self.input.is(&tok!(':')) {
                //     let cur_pos = self.input.cur_pos();
                //     Some(self.parse_ts_type_ann(/* eat_colon */ true, cur_pos))
                // } else {
                //     None
                // };

                let pat = Pat::Rest(RestPat {
                    span: span!(self, pat_start),
                    dot3_token,
                    arg: Box::new(pat),
                    type_ann,
                });
                params.push(ParamOrTsParamProp::Param(Param {
                    span: span!(self, param_start),
                    decorators,
                    pat,
                }));
                break;
            } else {
                params.push(self.parse_constructor_param(param_start, decorators));
            }
        }

        params
    }

    fn parse_constructor_param(
        &mut self,
        param_start: BytePos,
        decorators: Vec<Decorator>,
    ) -> ParamOrTsParamProp {
        let (accessibility, readonly) = (None, false);
        // let (accessibility, readonly) = if self.input.syntax().typescript() {
        //     let accessibility = self.parse_access_modifier();
        //     (
        //         accessibility,
        //         self.parse_ts_modifier(&["readonly"]).is_some(),
        //     )
        // } else {
        //     (None, false)
        // };
        if accessibility == None && !readonly {
            let pat = self.parse_formal_param_pat();
            ParamOrTsParamProp::Param(Param {
                span: span!(self, param_start),
                decorators,
                pat,
            })
        } else {
            let param = match self.parse_formal_param_pat() {
                Pat::Ident(i) => TsParamPropParam::Ident(i),
                Pat::Assign(a) => TsParamPropParam::Assign(a),
                node => {
                    // syntax_error!(self, node.span(), SyntaxError::TsInvalidParamPropPat)
                    panic!("TsInvalidParamPropPat at {:?}", node.span());
                }
            };
            ParamOrTsParamProp::TsParamProp(TsParamProp {
                span: span!(self, param_start),
                accessibility,
                readonly,
                decorators,
                param,
            })
        }
    }

    pub(super) fn parse_formal_params(&mut self) -> Vec<Param> {
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

            if !dot3_token.is_dummy() {
                // self.emit_err(dot3_token, SyntaxError::TS1014);
                panic!("TS1014 at {:?}", dot3_token);
            }

            let decorators = self.parse_decorators(false);
            let pat_start = self.input.cur_pos();

            let pat = if self.input.eat(&tok!("...")) {
                dot3_token = span!(self, pat_start);

                let pat = self.parse_binding_pat_or_ident();

                if self.input.eat(&tok!('=')) {
                    // let right = self.parse_assignment_expr();
                    // self.emit_err(pat.span(), SyntaxError::TS1048);
                    panic!("TS1048 at {:?}", pat.span());
                    // pat = AssignPat {
                    //     span: span!(self, pat_start),
                    //     left: Box::new(pat),
                    //     right,
                    //     type_ann: None,
                    // }
                    // .into();
                }

                let type_ann = None;
                // let type_ann = if self.input.syntax().typescript() && is!(self, ':') {
                //     let cur_pos = self.input.cur_pos();
                //     let ty = self.parse_ts_type_ann(/* eat_colon */ true, cur_pos);
                //     Some(ty)
                // } else {
                //     None
                // };

                let pat = Pat::Rest(RestPat {
                    span: span!(self, pat_start),
                    dot3_token,
                    arg: Box::new(pat),
                    type_ann,
                });

                // if self.syntax().typescript() && eat!(self, '?') {
                //     self.emit_err(self.input.prev_span(), SyntaxError::TS1047);
                //     //
                // }

                pat
            } else {
                self.parse_formal_param_pat()
            };

            params.push(Param {
                span: span!(self, param_start),
                decorators,
                pat,
            });
        }

        params
    }

    pub(super) fn parse_unique_formal_params(&mut self) -> Vec<Param> {
        // FIXME: This is wrong
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

impl<'a, I: Tokens> Parser<I> {
    /// This does not return 'rest' pattern because non-last parameter cannot be
    /// rest.
    pub(super) fn reparse_expr_as_pat(&mut self, pat_ty: PatType, expr: Box<Expr>) -> Pat {
        if let Expr::Invalid(i) = *expr {
            return Pat::Invalid(i);
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
                    self.check_assign_target(&expr/*, true*/);
                }
            }
        }

        self.reparse_expr_as_pat_inner(pat_ty, expr)
    }

    pub(super) fn reparse_expr_as_pat_inner(&mut self, pat_ty: PatType, expr: Box<Expr>) -> Pat {
        // In dts, we do not reparse.
        // debug_assert!(!self.input.syntax().dts());

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
                        return Pat::Expr(expr);
                    }
                    Expr::Ident(i) => return i.into(),
                    _ => {
                        return Pat::Expr(expr);
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
                        // self.emit_err(span, SyntaxError::NotSimpleAssign)
                        panic!("NotSimpleAssign at {:?}", span);
                    }
                    match *expr {
                        Expr::Ident(i) => return i.into(),
                        _ => {
                            return Pat::Expr(expr);
                        }
                    }
                }

                // It's special because of optional initializer
                Expr::Assign(..) => {}

                _ => {
                    // self.emit_err(span, SyntaxError::InvalidPat)
                    panic!("InvalidPat at {:?}", span);
                }
            }
        }

        match *expr {
            Expr::Paren(..) => {
                // self.emit_err(span, SyntaxError::InvalidPat);
                panic!("InvalidPat at {:?}", span);
                // Pat::Invalid(Invalid { span })
            }
            Expr::Assign(
                assign_expr
                @
                AssignExpr {
                    op: AssignOpToken::Assign,
                    ..
                },
            ) => {
                let AssignExpr {
                    span, left, right, ..
                } = assign_expr;
                Pat::Assign(AssignPat {
                    span,
                    left: match left {
                        PatOrExpr::Expr(left) => Box::new(self.reparse_expr_as_pat(pat_ty, left)),
                        PatOrExpr::Pat(left) => left,
                    },
                    right,
                    type_ann: None,
                })
            }
            Expr::Object(ObjectLit { span, props }) => {
                // {}
                Pat::Object(ObjectPat {
                    span,
                    props: props
                        .into_iter()
                        .map(|prop| {
                            let span = prop.span();
                            match prop {
                                PropOrSpread::Prop(prop) => match *prop {
                                    Prop::Shorthand(id) => ObjectPatProp::Assign(AssignPatProp {
                                        span: id.span(),
                                        key: id,
                                        value: None,
                                    }),
                                    Prop::KeyValue(kv_prop) => {
                                        ObjectPatProp::KeyValue(KeyValuePatProp {
                                            key: kv_prop.key,
                                            value: Box::new(self.reparse_expr_as_pat(
                                                pat_ty.element(),
                                                kv_prop.value,
                                            )),
                                        })
                                    }
                                    Prop::Assign(assign_prop) => {
                                        ObjectPatProp::Assign(AssignPatProp {
                                            span,
                                            key: assign_prop.key,
                                            value: Some(assign_prop.value),
                                        })
                                    }
                                    _ => {
                                        // syntax_error!(self, prop.span(), SyntaxError::InvalidPat)
                                        panic!("InvalidPat at {:?}", prop.span());
                                    }
                                },

                                PropOrSpread::Spread(SpreadElement { dot3_token, expr }) => {
                                    ObjectPatProp::Rest(RestPat {
                                        span,
                                        dot3_token,
                                        // FIXME: is BindingPat correct?
                                        arg: Box::new(
                                            self.reparse_expr_as_pat(PatType::BindingPat, expr),
                                        ),
                                        type_ann: None,
                                    })
                                }
                            }
                        })
                        .collect(),
                    optional: false,
                    type_ann: None,
                })
            }
            Expr::Ident(ident) => ident.into(),
            Expr::Member(..) => Pat::Expr(expr),
            Expr::Array(ArrayLit {
                elems: mut exprs, ..
            }) => {
                if exprs.is_empty() {
                    return Pat::Array(ArrayPat {
                        span,
                        elems: vec![],
                        optional: false,
                        type_ann: None,
                    });
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
                        Some(
                            expr
                            @
                            ExprOrSpread {
                                spread: Some(..), ..
                            },
                        ) => {
                            // if self.syntax().early_errors() {
                            //     syntax_error!(self, expr.span(), SyntaxError::NonLastRestParam)
                            // }
                            panic!("NonLastRestParam at {:?}", expr.span());
                        }
                        Some(ExprOrSpread { expr, .. }) => {
                            params.push(Some(self.reparse_expr_as_pat(pat_ty.element(), expr)))
                        }
                        None => params.push(None),
                    }
                }

                if count_of_trailing_comma == 0 {
                    let expr = exprs.into_iter().next().unwrap();
                    let last = match expr {
                        // Rest
                        Some(ExprOrSpread {
                            spread: Some(dot3_token),
                            expr,
                        }) => {
                            // TODO: is BindingPat correct?
                            let expr_span = expr.span();
                            let arg = self.reparse_expr_as_pat(pat_ty.element(), expr);

                            Some(Pat::Rest(RestPat {
                                span: expr_span,
                                dot3_token,
                                arg: Box::new(arg),
                                type_ann: None,
                            }))
                        }
                        Some(ExprOrSpread { expr, .. }) => {
                            // TODO: is BindingPat correct?
                            Some(self.reparse_expr_as_pat(pat_ty.element(), expr))
                        }
                        // TODO: syntax error if last element is ellison and ...rest exists.
                        None => None,
                    };
                    params.push(last);
                }
                Pat::Array(ArrayPat {
                    span,
                    elems: params,
                    optional: false,
                    type_ann: None,
                })
            }

            // Invalid patterns.
            // Note that assignment expression with '=' is valid, and handled above.
            Expr::Lit(..) | Expr::Assign(..) => {
                // self.emit_err(span, SyntaxError::InvalidPat);
                panic!("InvalidPat at {:?}", span);
                // Pat::Invalid(Invalid { span })
            }

            Expr::Yield(..) if self.ctx().in_generator => {
                // self.emit_err(span, SyntaxError::InvalidPat);
                panic!("InvalidPat at {:?}", span);
                // Pat::Invalid(Invalid { span })
            }

            _ => {
                // self.emit_err(span, SyntaxError::InvalidPat);
                panic!("InvalidPat at {:?}", span);

                // Pat::Invalid(Invalid { span })
            }
        }
    }

    pub(super) fn parse_paren_items_as_params(
        &mut self,
        mut exprs: Vec<PatOrExprOrSpread>,
    ) -> Vec<Pat> {
        let pat_ty = PatType::BindingPat;

        let len = exprs.len();
        if len == 0 {
            return vec![];
        }

        let mut params = Vec::with_capacity(len);

        for expr in exprs.drain(..len - 1) {
            match expr {
                PatOrExprOrSpread::ExprOrSpread(ExprOrSpread {
                    spread: Some(..), ..
                })
                | PatOrExprOrSpread::Pat(Pat::Rest(..)) => {
                    panic!("NonLastRestParam at {:?}", expr.span());
                    // if self.syntax().early_errors() {
                    //     // syntax_error!(self, expr.span(), SyntaxError::NonLastRestParam)
                    //     panic!("NonLastRestParam at {:?}", expr.span());
                    // }
                }
                PatOrExprOrSpread::ExprOrSpread(ExprOrSpread {
                    spread: None, expr, ..
                }) => params.push(self.reparse_expr_as_pat(pat_ty, expr)),
                PatOrExprOrSpread::Pat(pat) => params.push(pat),
            }
        }

        debug_assert_eq!(exprs.len(), 1);
        let expr = exprs.into_iter().next().unwrap();
        let last = match expr {
            // Rest
            PatOrExprOrSpread::ExprOrSpread(ExprOrSpread {
                spread: Some(dot3_token),
                expr,
            }) => {
                let expr_span = expr.span();
                let arg = self.reparse_expr_as_pat(pat_ty, expr);

                Pat::Rest(RestPat {
                    span: expr_span,
                    dot3_token,
                    arg: Box::new(arg),
                    type_ann: None,
                })
            }
            PatOrExprOrSpread::ExprOrSpread(ExprOrSpread { expr, .. }) => {
                self.reparse_expr_as_pat(pat_ty, expr)
            }
            PatOrExprOrSpread::Pat(pat) => pat,
        };
        params.push(last);

        params
    }
}
