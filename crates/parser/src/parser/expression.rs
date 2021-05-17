use super::*;
use crate::{
    parser::{pat::PatType, util::ExprExt},
    token::{AssignOpToken, Token, Word},
};
use either::Either;
use global_common::{ast_node, Span, Spanned};
use swc_atoms::js_word;

mod ops;

// A recursive descent parser operates by defining functions for all
// syntactic elements, and recursively calling those, each function
// advancing the input stream and returning an AST node. Precedence
// of constructs (for example, the fact that `!x[1]` means `!(x[1])`
// instead of `(!x)[1]` is handled by the fact that the parser
// function that parses unary prefix operators is called first, and
// in turn calls the function that parses `[]` subscripts — that
// way, it'll receive the node for `x[1]` already parsed, and wraps
// *that* in the unary operator node.
//
// Acorn uses an [operator precedence parser][opp] to handle binary
// operator precedence, because it is much more compact than using
// the technique outlined above, which uses different, nesting
// functions to specify precedence, for all of the ten binary
// precedence levels that JavaScript defines.
//
// [opp]: http://en.wikipedia.org/wiki/Operator-precedence_parser

impl<'a, I: Tokens> Parser<I> {
    // https://tc39.es/ecma262/#prod-Expression
    pub(super) fn parse_expr(&mut self) -> Box<Expr> {
        let start = self.input.cur_pos();
        let expr = self.parse_assignment_expr();

        if self.input.is(&tok!(',')) {
            let mut exprs = vec![expr];

            while self.input.eat(&tok!(',')) {
                exprs.push(self.parse_assignment_expr());
            }

            return Box::new(Expr::Seq(SeqExpr {
                span: span!(self, start),
                exprs,
            }));
        }

        expr
    }

    ///`parseMaybeAssign` (overridden)
    pub(super) fn parse_assignment_expr(&mut self) -> Box<Expr> {
        // trace_cur!(self, parse_assignment_expr);

        // if self.input.syntax().typescript() {
        //     // Note: When the JSX plugin is on, type assertions (`<T> x`) aren't valid
        //     // syntax.

        //     if is!(self, JSXTagStart) {
        //         let cur_context = self.input.token_context().current();
        //         debug_assert_eq!(cur_context, Some(TokenContext::JSXOpeningTag));
        //         // Only time j_oTag is pushed is right after j_expr.
        //         debug_assert_eq!(
        //             self.input.token_context().0[self.input.token_context().len() - 2],
        //             TokenContext::JSXExpr
        //         );

        //         let res = self.try_parse_ts(|p| p.parse_assignment_expr_base().map(Some));
        //         if let Some(res) = res {
        //             return Ok(res);
        //         } else {
        //             debug_assert_eq!(
        //                 self.input.token_context().current(),
        //                 Some(TokenContext::JSXOpeningTag)
        //             );
        //             self.input.token_context_mut().pop();
        //             debug_assert_eq!(
        //                 self.input.token_context().current(),
        //                 Some(TokenContext::JSXExpr)
        //             );
        //             self.input.token_context_mut().pop();
        //         }
        //     }
        // }

        // if self.input.syntax().typescript()
        //     && (is_one_of!(self, '<', JSXTagStart))
        //     && peeked_is!(self, IdentName)
        // {
        //     let res = self.try_parse_ts(|p| {
        //         let type_parameters = p.parse_ts_type_params();
        //         let mut arrow = p.parse_assignment_expr_base();
        //         match *arrow {
        //             Expr::Arrow(ArrowExpr {
        //                 ref mut span,
        //                 ref mut type_params,
        //                 ..
        //             }) => {
        //                 *span = Span::new(type_parameters.span.lo, span.hi, Default::default());
        //                 *type_params = Some(type_parameters);
        //             }
        //             _ => unexpected!(p, "("),
        //         }
        //         Ok(Some(arrow))
        //     });
        //     if let Some(res) = res {
        //         return Ok(res);
        //     }
        // }

        self.parse_assignment_expr_base()
    }

    /// Parse an assignment expression. This includes applications of
    /// operators like `+=`.
    ///
    /// `parseMaybeAssign`
    fn parse_assignment_expr_base(&mut self) -> Box<Expr> {
        // trace_cur!(self, parse_assignment_expr_base);

        if self.ctx().in_generator && is!(self, "yield") {
            return self.parse_yield_expr();
        }

        let start = self.input.cur_pos();

        self.state.potential_arrow_start = match *cur!(self, true) {
            Word(Word::Ident(..)) | tok!('(') | tok!("yield") => Some(start),
            _ => None,
        };

        // Try to parse conditional expression.
        let cond = self.parse_cond_expr();

        return_if_arrow!(self, cond);

        match *cond {
            // if cond is conditional expression but not left-hand-side expression,
            // just return it.
            Expr::Cond(..) | Expr::Bin(..) | Expr::Unary(..) | Expr::Update(..) => return cond,
            _ => {}
        }

        self.finish_assignment_expr(start, cond)
    }

    fn finish_assignment_expr(&mut self, start: BytePos, cond: Box<Expr>) -> Box<Expr> {
        // trace_cur!(self, finish_assignment_expr);

        match self.input.cur() {
            Some(&Token::AssignOp(op)) => {
                let left = if op == AssignOpToken::Assign {
                    let pat = self.reparse_expr_as_pat(PatType::AssignPat, cond);

                    PatOrExpr::Pat(Box::new(pat))
                } else {
                    //It is an early Reference Error if IsValidSimpleAssignmentTarget of
                    // LeftHandSideExpression is false.
                    // if !self.input.syntax().typescript()
                    //     && !cond.is_valid_simple_assignment_target(self.ctx().strict)
                    // {
                    //     self.emit_err(cond.span(), SyntaxError::NotSimpleAssign)
                    // }
                    if !cond.is_valid_simple_assignment_target(self.ctx().strict) {
                        // self.emit_err(cond.span(), SyntaxError::NotSimpleAssign)
                        panic!("NotSimpleAssign at {:?}", cond.span());
                    }
                    // let is_eval_or_arguments = match *cond {
                    //     Expr::Ident(ref i) => {
                    //         i.sym == js_word!("eval") || i.sym == js_word!("arguments")
                    //     }
                    //     _ => false,
                    // };
                    // if self.input.syntax().typescript() && is_eval_or_arguments {
                    //     self.emit_strict_mode_err(cond.span(), SyntaxError::TS1100);
                    // }

                    // TODO
                    PatOrExpr::Expr(cond)
                };

                self.input.bump();
                let right = self.parse_assignment_expr();
                Box::new(Expr::Assign(AssignExpr {
                    span: span!(self, start),
                    op,
                    // TODO:
                    left,
                    right,
                }))
            }
            _ => cond,
        }
    }

    /// Spec: 'ConditionalExpression'
    fn parse_cond_expr(&mut self) -> Box<Expr> {
        // trace_cur!(self, parse_cond_expr);

        let start = self.input.cur_pos();

        let test = self.parse_bin_expr();
        return_if_arrow!(self, test);

        if self.input.eat(&tok!('?')) {
            let ctx = Context {
                in_cond_expr: true,
                include_in_expr: true,
                ..self.ctx()
            };
            let cons = self.with_ctx(ctx).parse_assignment_expr();
            expect!(self, ':');
            let ctx = Context {
                in_cond_expr: true,
                ..self.ctx()
            };
            let alt = self.with_ctx(ctx).parse_assignment_expr();
            let span = Span::new(start, alt.span().hi());
            Box::new(Expr::Cond(CondExpr {
                span,
                test,
                cons,
                alt,
            }))
        } else {
            test
        }
    }

    /// Parse a primary expression or arrow function
    #[allow(clippy::cognitive_complexity)]
    pub(super) fn parse_primary_expr(&mut self) -> Box<Expr> {
        // trace_cur!(self, parse_primary_expr);

        let _ = self.input.cur();
        let start = self.input.cur_pos();

        let can_be_arrow = self
            .state
            .potential_arrow_start
            .map(|s| s == start)
            .unwrap_or(false);

        match self.input.cur() {
            Some(tok) => match tok {
                tok!("this") => {
                    self.input.bump();
                    return Box::new(Expr::This(ThisExpr {
                        span: span!(self, start),
                    }));
                }

                tok!("import") => {
                    let import = self.parse_ident_name();
                    if is!(self, '.') {
                        return Box::new(Expr::MetaProp(self.parse_import_meta_prop(import)));
                    }

                    return self.parse_dynamic_import(start, import);
                }

                tok!("async") => {
                    if self.input.peeked_is(&tok!("function"))
                        && !self.input.has_linebreak_between_cur_and_peeked()
                    {
                        // handle `async function` expression
                        return self.parse_async_fn_expr();
                    }

                    // if can_be_arrow && self.input.syntax().typescript() && peeked_is!(self, '<') {
                    //     // try parsing `async<T>() => {}`
                    //     if let Some(res) = self.try_parse_ts(|p| {
                    //         let start = cur_pos!(p);
                    //         assert_and_bump!(p, "async");
                    //         p.try_parse_ts_generic_async_arrow_fn(start)
                    //     }) {
                    //         return Ok(Box::new(Expr::Arrow(res)));
                    //     }
                    // }

                    if can_be_arrow && self.input.peeked_is(&tok!('(')) {
                        expect!(self, "async");
                        let async_span = self.input.prev_span();
                        return self.parse_paren_expr_or_arrow_fn(can_be_arrow, Some(async_span));
                    }
                }

                tok!('[') => {
                    return self.parse_array_lit();
                }

                tok!('{') => {
                    return self.parse_object();
                }

                // Handle FunctionExpression and GeneratorExpression
                tok!("function") => {
                    return self.parse_fn_expr();
                }

                // Literals
                tok!("null")
                | tok!("true")
                | tok!("false")
                | Token::Num(..)
                | Token::BigInt(..)
                | Token::Str { .. } => {
                    return Box::new(Expr::Lit(self.parse_lit()));
                }

                // Regexp
                Token::Regex(..) => match self.input.bump() {
                    Token::Regex(exp, flags) => {
                        return Box::new(Expr::Lit(Lit::Regex(Regex {
                            span: span!(self, start),
                            exp,
                            flags,
                        })));
                    }
                    _ => unreachable!(),
                },

                tok!('`') => {
                    // parse template literal
                    todo!();
                    // return Box::new(Expr::Tpl(self.parse_tpl()));
                }

                tok!('(') => {
                    return self.parse_paren_expr_or_arrow_fn(can_be_arrow, None);
                }

                _ => {}
            },
            None => {}
        }

        let decorators = self.parse_decorators(false);

        if is!(self, "class") {
            return self.parse_class_expr(start, decorators);
        }

        if is!(self, "let")
            /*|| (self.input.syntax().typescript() && is!(self, IdentName))*/
            || is!(self, IdentRef)
        {
            // TODO: Handle [Yield, Await]
            let id = self.parse_ident_name();
            match id.sym {
                //                    js_word!("eval") | js_word!("arguments") => {
                //                        self.emit_err(id.span,
                // SyntaxError::EvalAndArgumentsInStrict)
                // }
                js_word!("yield")
                | js_word!("static")
                | js_word!("implements")
                | js_word!("let")
                | js_word!("package")
                | js_word!("private")
                | js_word!("protected")
                | js_word!("public") => {
                    // self.emit_strict_mode_err(
                    //     self.input.prev_span(),
                    //     SyntaxError::InvalidIdentInStrict,
                    // );
                    panic!("InvalidIdentInStrict at {:?}", self.input.prev_span());
                }
                _ => {}
            }

            if can_be_arrow && id.sym == js_word!("async") && is!(self, BindingIdent) {
                // async a => body
                let arg = Pat::from(self.parse_binding_ident());
                let params = vec![arg];
                expect!(self, "=>");
                let body = self.parse_fn_body(true, false);

                return Box::new(Expr::Arrow(ArrowExpr {
                    span: span!(self, start),
                    body,
                    params,
                    is_async: true,
                    is_generator: false,
                    return_type: None,
                    type_params: None,
                }));
            } else if can_be_arrow
                && !self.input.had_line_break_before_cur()
                && self.input.eat(&tok!("=>"))
            {
                let params = vec![id.into()];
                let body = self.parse_fn_body(false, false);

                return Box::new(Expr::Arrow(ArrowExpr {
                    span: span!(self, start),
                    body,
                    params,
                    is_async: false,
                    is_generator: false,
                    // TODO
                    return_type: None,
                    // TODO
                    type_params: None,
                }));
            } else {
                return Box::new(Expr::Ident(id));
            }
        }

        unexpected!(
            self,
            "this, import, async, function, [ for array literal, { for object literal, @ for \
             decorator, function, class, null, true, false, number, bigint, string, regexp, ` for \
             template literal, (, or an identifier"
        )
    }

    fn parse_array_lit(&mut self) -> Box<Expr> {
        // trace_cur!(self, parse_array_lit);

        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!('['));
        let mut elems = vec![];

        while !eof!(self) && !is!(self, ']') {
            if is!(self, ',') {
                expect!(self, ',');
                elems.push(None);
                continue;
            }
            let elem = self.include_in_expr(true).parse_expr_or_spread();
            elems.push(Some(elem));
            if is!(self, ',') {
                expect!(self, ',');
            }
        }

        expect!(self, ']');

        let span = span!(self, start);
        Box::new(Expr::Array(ArrayLit { span, elems }))
    }

    /// `parseImportMetaProperty`
    pub(super) fn parse_import_meta_prop(&mut self, import: Ident) -> MetaPropExpr {
        let meta = import;

        expect!(self, '.');

        let prop = if is!(self, "meta") {
            self.parse_ident_name()
        } else {
            unexpected!(self, "meta");
        };

        MetaPropExpr { meta, prop }
    }

    fn parse_subscripts(&mut self, mut obj: ExprOrSuper, no_call: bool) -> Box<Expr> {
        loop {
            obj = match self.parse_subscript(obj, no_call) {
                (expr, false) => return expr,
                (expr, true) => ExprOrSuper::Expr(expr),
            }
        }
    }

    /// returned bool is true if this method should be called again.
    #[allow(clippy::cognitive_complexity)]
    fn parse_subscript(&mut self, obj: ExprOrSuper, no_call: bool) -> (Box<Expr>, bool) {
        let start = obj.span().lo();

        // if self.input.syntax().typescript() {
        //     if !self.input.had_line_break_before_cur() && is!(self, '!') {
        //         self.input.set_expr_allowed(false);
        //         assert_and_bump!(self, '!');

        //         let expr = match obj {
        //             ExprOrSuper::Super(..) => unimplemented!("super!"),
        //             ExprOrSuper::Expr(expr) => expr,
        //         };
        //         return Ok((
        //             Box::new(Expr::TsNonNull(TsNonNullExpr {
        //                 span: span!(self, start),
        //                 expr,
        //             })),
        //             true,
        //         ));
        //     }

        //     if {
        //         match obj {
        //             ExprOrSuper::Expr(..) => true,
        //             // super() cannot be generic
        //             _ => false,
        //         }
        //     } && is!(self, '<')
        //     {
        //         let obj_ref = &obj;
        //         // tsTryParseAndCatch is expensive, so avoid if not necessary.
        //         // There are number of things we are going to "maybe" parse, like type arguments
        //         // on tagged template expressions. If any of them fail, walk it back and
        //         // continue.
        //         let result = self.try_parse_ts(|p| {
        //             if !no_call
        //                 && p.at_possible_async(match obj_ref {
        //                     ExprOrSuper::Expr(ref expr) => &*expr,
        //                     _ => unreachable!(),
        //                 })
        //             {
        //                 // Almost certainly this is a generic async function `async <T>() => ...
        //                 // But it might be a call with a type argument `async<T>();`
        //                 let async_arrow_fn = p.try_parse_ts_generic_async_arrow_fn(start);
        //                 if let Some(async_arrow_fn) = async_arrow_fn {
        //                     return Ok(Some((Box::new(Expr::Arrow(async_arrow_fn)), true)));
        //                 }
        //             }

        //             let type_args = p.parse_ts_type_args();

        //             if !no_call && is!(p, '(') {
        //                 // possibleAsync always false here, because we would have handled it
        //                 // above. (won't be any undefined arguments)
        //                 let args = p.parse_args(is_import(&obj));

        //                 Ok(Some((
        //                     Box::new(Expr::Call(CallExpr {
        //                         span: span!(p, start),
        //                         callee: obj_ref.clone(),
        //                         type_args: Some(type_args),
        //                         args,
        //                     })),
        //                     true,
        //                 )))
        //             } else if is!(p, '`') {
        //                 p.parse_tagged_tpl(
        //                     match *obj_ref {
        //                         ExprOrSuper::Expr(ref obj) => obj.clone(),
        //                         _ => unreachable!(),
        //                     },
        //                     Some(type_args),
        //                 )
        //                 .map(|expr| (Box::new(Expr::TaggedTpl(expr)), true))
        //                 .map(Some)
        //             } else {
        //                 if no_call {
        //                     unexpected!(p, "`")
        //                 } else {
        //                     unexpected!(p, "( or `")
        //                 }
        //             }
        //         });
        //         if let Some(result) = result {
        //             return Ok(result);
        //         }
        //     }
        // }

        let question_dot_token = if is!(self, '?') && self.input.peeked_is(&tok!('.')) {
            let start = self.input.cur_pos();
            self.input.eat(&tok!('?'));
            Some(span!(self, start))
        } else {
            None
        };

        /// Wrap with optional chaining
        macro_rules! wrap {
            ($e:expr) => {{
                if let Some(question_dot_token) = question_dot_token {
                    Expr::OptChain(OptChainExpr {
                        span: span!(self, start),
                        question_dot_token,
                        expr: Box::new($e),
                    })
                } else {
                    $e
                }
            }};
        }

        // $obj[name()]
        if (question_dot_token.is_some()
            && is!(self, '.')
            && self.input.peeked_is(&tok!('['))
            && self.input.eat(&tok!('.'))
            && self.input.eat(&tok!('[')))
            || self.input.eat(&tok!('['))
        {
            let prop = self.include_in_expr(true).parse_expr();
            expect!(self, ']');
            let span = Span::new(obj.span().lo(), self.input.last_pos());
            debug_assert_eq!(obj.span().lo(), span.lo());

            return (
                Box::new(wrap!(Expr::Member(MemberExpr {
                    span,
                    obj,
                    prop,
                    computed: true,
                }))),
                true,
            );
        }

        if (question_dot_token.is_some()
            && self.input.is(&tok!('.'))
            && self.input.peeked_is(&tok!('('))
            && self.input.eat(&tok!('.')))
            || (!no_call && (self.input.is(&tok!('('))))
        {
            let args = self.parse_args(is_import(&obj));
            return (
                Box::new(wrap!(Expr::Call(CallExpr {
                    span: span!(self, start),
                    callee: obj,
                    args,
                    type_args: None,
                }))),
                true,
            );
        }

        // member expression
        // $obj.name
        if self.input.eat(&tok!('.')) {
            let prop: Box<Expr> = Box::new(match self.parse_maybe_private_name() {
                Either::Left(p) => Expr::PrivateName(p),
                Either::Right(i) => Expr::Ident(i),
            });
            let span = span!(self, obj.span().lo());
            debug_assert_eq!(obj.span().lo(), span.lo());
            debug_assert_eq!(prop.span().hi(), span.hi());

            return (
                Box::new(wrap!(Expr::Member(MemberExpr {
                    span,
                    obj,

                    prop,
                    computed: false,
                }))),
                true,
            );
        }

        match obj {
            ExprOrSuper::Expr(expr) => {
                // MemberExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
                if is!(self, '`') {
                    todo!();
                    // let tpl = self.parse_tagged_tpl(expr, None);
                    // return (Box::new(Expr::TaggedTpl(tpl)), true);
                }

                (expr, false)
            }
            ExprOrSuper::Super(..) => {
                if no_call {
                    // syntax_error!(self, self.input.cur_span(), SyntaxError::InvalidSuperCall);
                    panic!("InvalidSuperCall at {:?}", self.input.cur_span());
                }
                // syntax_error!(self, self.input.cur_span(), SyntaxError::InvalidSuper);
                panic!("InvalidSuper at {:?}", self.input.cur_span());
            }
        }
    }

    /// Parse call, dot, and `[]`-subscript expressions.
    pub(super) fn parse_lhs_expr(&mut self) -> Box<Expr> {
        let start = self.input.cur_pos();

        // // parse jsx
        // if self.input.syntax().jsx() {
        //     fn into_expr(e: Either<JSXFragment, JSXElement>) -> Box<Expr> {
        //         match e {
        //             Either::Left(l) => Box::new(l.into()),
        //             Either::Right(r) => Box::new(Box::new(r).into()),
        //         }
        //     }
        //     match *cur!(self, true) {
        //         Token::JSXText { .. } => {
        //             return self
        //                 .parse_jsx_text()
        //                 .map(Lit::JSXText)
        //                 .map(Expr::Lit)
        //                 .map(Box::new);
        //         }
        //         Token::JSXTagStart => {
        //             return self.parse_jsx_element().map(into_expr);
        //         }
        //         _ => {}
        //     }

        //     if is!(self, '<') && !peeked_is!(self, '!') {
        //         // In case we encounter an lt token here it will always be the start of
        //         // jsx as the lt sign is not allowed in places that expect an expression

        //         // FIXME:
        //         // self.finishToken(tt.jsxTagStart);

        //         return self.parse_jsx_element().map(into_expr);
        //     }
        // }

        // `super()` can't be handled from parse_new_expr()
        if self.input.eat(&tok!("super")) {
            let obj = ExprOrSuper::Super(Super {
                span: span!(self, start),
            });
            return self.parse_subscripts(obj, false);
        }

        let callee = self.parse_new_expr();
        return_if_arrow!(self, callee);

        let type_args = None;
        // let type_args = if self.input.syntax().typescript() && is!(self, '<') {
        //     self.try_parse_ts(|p| {
        //         let type_args = p.parse_ts_type_args();
        //         if is!(p, '(') {
        //             Ok(Some(type_args))
        //         } else {
        //             Ok(None)
        //         }
        //     })
        // } else {
        //     None
        // };

        if let Expr::New(ne @ NewExpr { args: None, .. }) = *callee {
            // If this is parsed using 'NewExpression' rule, just return it.
            // Because it's not left-recursive.
            if type_args.is_some() {
                // This fails with `expected (`
                expect!(self, '(');
            }
            debug_assert!(
                !self.input.is(&tok!('(')),
                "parse_new_expr() should eat paren if it exists"
            );
            return Box::new(Expr::New(NewExpr { type_args, ..ne }));
        }
        // 'CallExpr' rule contains 'MemberExpr (...)',
        // and 'MemberExpr' rule contains 'new MemberExpr (...)'

        if is!(self, '(') {
            // This is parsed using production MemberExpression,
            // which is left-recursive.
            let callee = ExprOrSuper::Expr(callee);
            let args = self.parse_args(is_import(&callee));

            let call_expr = Box::new(Expr::Call(CallExpr {
                span: span!(self, start),

                callee,
                args,
                type_args,
            }));

            return self.parse_subscripts(ExprOrSuper::Expr(call_expr), false);
        }
        if type_args.is_some() {
            // This fails
            expect!(self, '(');
        }

        // This is parsed using production 'NewExpression', which contains
        // 'MemberExpression'
        callee
    }

    pub(super) fn parse_expr_or_pat(&mut self) -> Box<Expr> {
        self.parse_expr()
    }

    #[allow(clippy::cognitive_complexity)]
    pub(super) fn parse_args_or_pats(&mut self) -> Vec<PatOrExprOrSpread> {
        // trace_cur!(self, parse_args_or_pats);

        expect!(self, '(');

        let mut first = true;
        let mut items = vec![];
        // let mut rest_span = None;

        // TODO(kdy1): optimize (once we parsed a pattern, we can parse everything else
        // as a pattern instead of reparsing)
        while !eof!(self) && !is!(self, ')') {
            if first {
                if is!(self, "async") {
                    // https://github.com/swc-project/swc/issues/410
                    self.state.potential_arrow_start = Some(self.input.cur_pos());
                    let expr = self.parse_assignment_expr();
                    expect!(self, ')');
                    return vec![PatOrExprOrSpread::ExprOrSpread(ExprOrSpread {
                        expr,
                        spread: None,
                    })];
                }
            } else {
                expect!(self, ',');
                // Handle trailing comma.
                if is!(self, ')') {
                    break;
                }
            }

            let start = self.input.cur_pos();
            self.state.potential_arrow_start = Some(start);
            let modifier_start = start;

            // let has_modifier = self.eat_any_ts_modifier();
            let has_modifier = false;
            // let pat_start = self.input.cur_pos();

            let arg = self.include_in_expr(true).parse_expr_or_spread();
            // let mut arg = {
            //     if self.input.syntax().typescript()
            //         && (is!(self, IdentRef) || (is!(self, "...") && peeked_is!(self, IdentRef)))
            //     {
            //         let spread = if eat!(self, "...") {
            //             Some(self.input.prev_span())
            //         } else {
            //             None
            //         };

            //         // At here, we use parse_bin_expr() instead of parse_assignment_expr()
            //         // because `x?: number` should not be parsed as a conditional expression
            //         let expr = if spread.is_some() {
            //             self.include_in_expr(true).parse_bin_expr()
            //         } else {
            //             let mut expr = self.parse_bin_expr();

            //             if let Ok(&Token::AssignOp(..)) = cur!(self, false) {
            //                 expr = self.finish_assignment_expr(start, expr)
            //             }

            //             expr
            //         };

            //         ExprOrSpread { spread, expr }
            //     } else {
            //         self.include_in_expr(true).parse_expr_or_spread()
            //     }
            // };

            // let optional = if self.input.syntax().typescript() {
            //     if is!(self, '?') {
            //         if peeked_is!(self, ',')
            //             || peeked_is!(self, ':')
            //             || peeked_is!(self, ')')
            //             || peeked_is!(self, '=')
            //         {
            //             assert_and_bump!(self, '?');
            //             let _ = cur!(self, false);
            //             if arg.spread.is_some() {
            //                 self.emit_err(self.input.prev_span(), SyntaxError::TS1047);
            //             }
            //             match *arg.expr {
            //                 Expr::Ident(..) => {}
            //                 _ => {
            //                     syntax_error!(
            //                         self,
            //                         arg.span(),
            //                         SyntaxError::TsBindingPatCannotBeOptional
            //                     )
            //                 }
            //             }
            //             true
            //         } else if match arg {
            //             ExprOrSpread { spread: None, .. } => true,
            //             _ => false,
            //         } {
            //             expect!(self, '?');
            //             let test = arg.expr;
            //             let ctx = Context {
            //                 in_cond_expr: true,
            //                 include_in_expr: true,
            //                 ..self.ctx()
            //             };
            //             let cons = self.with_ctx(ctx).parse_assignment_expr();
            //             expect!(self, ':');
            //             let ctx = Context {
            //                 in_cond_expr: true,
            //                 ..self.ctx()
            //             };
            //             let alt = self.with_ctx(ctx).parse_assignment_expr();

            //             arg = ExprOrSpread {
            //                 spread: None,
            //                 expr: Box::new(Expr::Cond(CondExpr {
            //                     span: Span::new(start, alt.span().hi(), Default::default()),

            //                     test,
            //                     cons,
            //                     alt,
            //                 })),
            //             };

            //             false
            //         } else {
            //             false
            //         }
            //     } else {
            //         false
            //     }
            // } else {
            //     false
            // };

            // if optional || (self.input.syntax().typescript() && is!(self, ':')) {
            //     let start = self.input.cur_pos();

            //     // TODO: `async(...args?: any[]) : any => {}`
            //     //
            //     // if self.input.syntax().typescript() && optional && arg.spread.is_some() {
            //     //     self.emit_err(self.input.prev_span(), SyntaxError::TS1047)
            //     // }

            //     let mut pat = self.reparse_expr_as_pat(PatType::BindingPat, arg.expr);
            //     if optional {
            //         match pat {
            //             Pat::Ident(ref mut i) => i.id.optional = true,
            //             _ => unreachable!(),
            //         }
            //     }
            //     if let Some(span) = arg.spread {
            //         if let Some(rest_span) = rest_span {
            //             if self.syntax().early_errors() {
            //                 // Rest pattern must be last one.
            //                 syntax_error!(self, rest_span, SyntaxError::NonLastRestParam);
            //             }
            //         }
            //         rest_span = Some(span);
            //         pat = Pat::Rest(RestPat {
            //             span: span!(self, pat_start),
            //             dot3_token: span,
            //             arg: Box::new(pat),
            //             type_ann: None,
            //         });
            //     }
            //     match pat {
            //         Pat::Ident(BindingIdent {
            //             id: Ident { ref mut span, .. },
            //             ref mut type_ann,
            //             ..
            //         })
            //         | Pat::Array(ArrayPat {
            //             ref mut type_ann,
            //             ref mut span,
            //             ..
            //         })
            //         | Pat::Assign(AssignPat {
            //             ref mut type_ann,
            //             ref mut span,
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
            //                 *span =
            //                     Span::new(pat_start, self.input.prev_span().hi, Default::default());
            //             }
            //             *type_ann = new_type_ann;
            //         }
            //         Pat::Expr(ref expr) => unreachable!("invalid pattern: Expr({:?})", expr),
            //         Pat::Invalid(ref i) => {
            //             // We don't have to panic here.
            //             // See: https://github.com/swc-project/swc/issues/1170
            //             //
            //             // Also, as an exact error is added to the errors while
            //             // creating `Invalid`, we don't have to emit a new
            //             // error.
            //         }
            //     }

            //     if eat!(self, '=') {
            //         let right = self.parse_assignment_expr();
            //         pat = Pat::Assign(AssignPat {
            //             span: span!(self, pat_start),
            //             left: Box::new(pat),
            //             right,
            //             type_ann: None,
            //         });
            //     }

            //     if has_modifier {
            //         self.emit_err(span!(self, modifier_start), SyntaxError::TS2369);
            //     }

            //     items.push(PatOrExprOrSpread::Pat(pat))
            // } else {
            //     if has_modifier {
            //         self.emit_err(span!(self, modifier_start), SyntaxError::TS2369);
            //     }

            //     items.push(PatOrExprOrSpread::ExprOrSpread(arg));
            // }

            if has_modifier {
                // self.emit_err(span!(self, modifier_start), SyntaxError::TS2369);
                panic!("TS2369 at {:?}", span!(self, modifier_start));
            }

            items.push(PatOrExprOrSpread::ExprOrSpread(arg));

            // https://github.com/swc-project/swc/issues/433
            if first && self.input.eat(&tok!("=>")) && {
                debug_assert_eq!(items.len(), 1);
                match items[0] {
                    PatOrExprOrSpread::ExprOrSpread(ExprOrSpread { ref expr, .. })
                    | PatOrExprOrSpread::Pat(Pat::Expr(ref expr)) => match **expr {
                        Expr::Ident(..) => true,
                        _ => false,
                    },
                    PatOrExprOrSpread::Pat(Pat::Ident(..)) => true,
                    _ => false,
                }
            } {
                let params = self
                    .parse_paren_items_as_params(items)
                    .into_iter()
                    .collect();

                let body: BlockStmtOrExpr = self.parse_fn_body(false, false);
                expect!(self, ')');
                let span = span!(self, start);

                return vec![PatOrExprOrSpread::ExprOrSpread(ExprOrSpread {
                    expr: Box::new(
                        ArrowExpr {
                            span,
                            body,
                            is_async: false,
                            is_generator: false,
                            params,
                            type_params: None,
                            return_type: None,
                        }
                        .into(),
                    ),
                    spread: None,
                })];
            }

            first = false;
        }

        expect!(self, ')');
        items
    }

    /// `is_new_expr`: true iff we are parsing production 'NewExpression'.
    fn parse_member_expr_or_new_expr(&mut self, is_new_expr: bool) -> Box<Expr> {
        // trace_cur!(self, parse_member_expr_or_new_expr);

        let start = self.input.cur_pos();
        if self.input.eat(&tok!("new")) {
            let span_of_new = span!(self, start);
            if self.input.eat(&tok!('.')) {
                let start_of_target = self.input.cur_pos();
                if self.input.eat(&tok!("target")) {
                    let expr = Box::new(Expr::MetaProp(MetaPropExpr {
                        meta: Ident::new(js_word!("new"), span_of_new),
                        prop: Ident::new(js_word!("target"), span!(self, start_of_target)),
                    }));

                    return self.parse_subscripts(ExprOrSuper::Expr(expr), true);
                }

                unexpected!(self, "target")
            }

            // 'NewExpression' allows new call without paren.
            let callee = self.parse_member_expr_or_new_expr(is_new_expr);
            return_if_arrow!(self, callee);

            let type_args = None;
            // let type_args = if self.input.syntax().typescript() && is!(self, '<') {
            //     self.try_parse_ts(|p| {
            //         let args = p.parse_ts_type_args();
            //         if !is!(p, '(') {
            //             // This will fail
            //             expect!(p, '(');
            //         }
            //         Ok(Some(args))
            //     })
            // } else {
            //     None
            // };

            if !is_new_expr || is!(self, '(') {
                // Parsed with 'MemberExpression' production.
                let args = Some(self.parse_args(false));

                let new_expr = ExprOrSuper::Expr(Box::new(Expr::New(NewExpr {
                    span: span!(self, start),
                    callee,
                    args,
                    type_args,
                })));

                // We should parse subscripts for MemberExpression.
                // Because it's left recursive.
                return self.parse_subscripts(new_expr, true);
            }

            // Parsed with 'NewExpression' production.

            return Box::new(Expr::New(NewExpr {
                span: span!(self, start),
                callee,
                args: None,
                type_args,
            }));
        }

        if self.input.eat(&tok!("super")) {
            let base = ExprOrSuper::Super(Super {
                span: span!(self, start),
            });
            return self.parse_subscripts(base, true);
        }
        let obj = self.parse_primary_expr();
        return_if_arrow!(self, obj);

        self.parse_subscripts(ExprOrSuper::Expr(obj), true)
    }

    /// Parse `NewExpression`.
    /// This includes `MemberExpression`.
    pub(super) fn parse_new_expr(&mut self) -> Box<Expr> {
        // trace_cur!(self, parse_new_expr);

        self.parse_member_expr_or_new_expr(true)
    }

    /// Parse `Arguments[Yield, Await]`
    pub(super) fn parse_args(&mut self, is_dynamic_import: bool) -> Vec<ExprOrSpread> {
        // trace_cur!(self, parse_args);

        let start = self.input.cur_pos();
        expect!(self, '(');

        let mut first = true;
        let mut expr_or_spreads = vec![];

        while !eof!(self) && !is!(self, ')') {
            if first {
                first = false;
            } else {
                expect!(self, ',');
                // Handle trailing comma.
                if is!(self, ')') {
                    if is_dynamic_import {
                        // syntax_error!(
                        //     self,
                        //     span!(self, start),
                        //     SyntaxError::TrailingCommaInsideImport
                        // )
                        panic!("TrailingCommaInsideImport at {:?}", span!(self, start));
                    }

                    break;
                }
            }

            expr_or_spreads.push(self.include_in_expr(true).parse_expr_or_spread());
        }

        expect!(self, ')');
        expr_or_spreads
    }

    /// AssignmentExpression[+In, ?Yield, ?Await]
    /// ...AssignmentExpression[+In, ?Yield, ?Await]
    pub(super) fn parse_expr_or_spread(&mut self) -> ExprOrSpread {
        // trace_cur!(self, parse_expr_or_spread);

        let start = self.input.cur_pos();

        if self.input.eat(&tok!("...")) {
            let spread = Some(span!(self, start));
            let expr = self.include_in_expr(true).parse_assignment_expr();
            ExprOrSpread { spread, expr }
        } else {
            let expr = self.parse_assignment_expr();
            ExprOrSpread { spread: None, expr }
        }
    }

    /// Parse paren expression or arrow function expression.
    fn parse_paren_expr_or_arrow_fn(
        &mut self,
        can_be_arrow: bool,
        async_span: Option<Span>,
    ) -> Box<Expr> {
        // trace_cur!(self, parse_paren_expr_or_arrow_fn);

        let expr_start = async_span.map(|x| x.lo()).unwrap_or(self.input.cur_pos());

        // At this point, we can't know if it's parenthesized
        // expression or head of arrow function.
        // But as all patterns of javascript is subset of
        // expressions, we can parse both as expression.

        let paren_items = self.include_in_expr(true).parse_args_or_pats();
        let has_pattern = paren_items.iter().any(|item| match item {
            PatOrExprOrSpread::Pat(..) => true,
            _ => false,
        });

        // This is slow path. We handle arrow in conditional expression.
        // if self.syntax().typescript() && self.ctx().in_cond_expr && is!(self, ':') {
        //     // TODO: Remove clone
        //     let items_ref = &paren_items;
        //     if let Some(expr) = self.try_parse_ts(|p| {
        //         let return_type = p.parse_ts_type_or_type_predicate_ann(&tok!(':'));

        //         expect!(p, "=>");

        //         let params = p
        //             .parse_paren_items_as_params(items_ref.clone())
        //             .into_iter()
        //             .collect();

        //         let body: BlockStmtOrExpr = p.parse_fn_body(async_span.is_some(), false);

        //         Ok(Some(Box::new(Expr::Arrow(ArrowExpr {
        //             span: span!(p, expr_start),
        //             is_async: async_span.is_some(),
        //             is_generator: false,
        //             params,
        //             body,
        //             return_type: Some(return_type),
        //             type_params: None,
        //         }))))
        //     }) {
        //         return Ok(expr);
        //     }
        // }

        let return_type = None;

        // let return_type = if !self.ctx().in_cond_expr
        //     && self.input.syntax().typescript()
        //     && is!(self, ':')
        //     && !self.ctx().in_case_cond
        // {
        //     Some(self.parse_ts_type_or_type_predicate_ann(&tok!(':')))
        // } else {
        //     None
        // };

        // we parse arrow function at here, to handle it efficiently.
        if has_pattern || return_type.is_some() || is!(self, "=>") {
            if self.input.had_line_break_before_cur() {
                // syntax_error!(
                //     self,
                //     span!(self, expr_start),
                //     SyntaxError::LineBreakBeforeArrow
                // );
                panic!("LineBreakBeforeArrow at {:?}", span!(self, expr_start));
            }
            if !can_be_arrow {
                // syntax_error!(self, span!(self, expr_start), SyntaxError::ArrowNotAllowed);
                panic!("ArrowNotAllowed at {:?}", span!(self, expr_start));
            }
            expect!(self, "=>");

            let params = self
                .parse_paren_items_as_params(paren_items)
                .into_iter()
                .collect();

            let body: BlockStmtOrExpr = self.parse_fn_body(async_span.is_some(), false);
            let arrow_expr = ArrowExpr {
                span: span!(self, expr_start),
                is_async: async_span.is_some(),
                is_generator: false,
                params,
                body,
                return_type,
                type_params: None,
            };
            match arrow_expr.body {
                BlockStmtOrExpr::BlockStmt(..) => match self.input.cur() {
                    Some(&Token::BinOp(..)) => {
                        // ) is required
                        panic!("TS1005 at {:?}", self.input.cur_span());
                        // self.emit_err(self.input.cur_span(), SyntaxError::TS1005);
                        // let errored_expr =
                        //     self.parse_bin_op_recursively(Box::new(arrow_expr.into()), 0);

                        // if !is!(self, ';') {
                        //     // ; is required
                        //     self.emit_err(self.input.cur_span(), SyntaxError::TS1005);
                        // }

                        // return Ok(errored_expr);
                    }
                    _ => {}
                },
                _ => {}
            }
            return Box::new(Expr::Arrow(arrow_expr));
        }

        let expr_or_spreads = paren_items
            .into_iter()
            .map(|item| {
                match item {
                    PatOrExprOrSpread::ExprOrSpread(e) => e,
                    _ => {
                        // syntax_error!(self, item.span(), SyntaxError::InvalidExpr)
                        panic!("InvalidExpr at {:?}", item.span());
                    }
                }
            })
            .collect::<Vec<_>>();
        if let Some(async_span) = async_span {
            // It's a call expression
            return Box::new(Expr::Call(CallExpr {
                span: span!(self, async_span.lo()),
                callee: ExprOrSuper::Expr(Box::new(Expr::Ident(Ident::new(
                    "async".into(),
                    async_span,
                )))),
                args: expr_or_spreads,
                type_args: None,
            }));
        }

        // It was not head of arrow function.

        if expr_or_spreads.is_empty() {
            // syntax_error!(
            //     self,
            //     Span::new(expr_start, last_pos!(self), Default::default()),
            //     SyntaxError::EmptyParenExpr
            // );
            panic!(
                "EmptyParenExpr at {:?}",
                Span::new(expr_start, self.input.last_pos())
            );
        }

        // TODO: Verify that invalid expression like {a = 1} does not exists.

        // ParenthesizedExpression cannot contain spread.
        if expr_or_spreads.len() == 1 {
            let expr = match expr_or_spreads.into_iter().next().unwrap() {
                ExprOrSpread {
                    spread: Some(..),
                    ref expr,
                } => {
                    // syntax_error!(self, expr.span(), SyntaxError::SpreadInParenExpr)
                    panic!("SpreadInParenExpr at {:?}", expr.span());
                }
                ExprOrSpread { expr, .. } => expr,
            };
            Box::new(Expr::Paren(ParenExpr {
                span: span!(self, expr_start),
                expr,
            }))
        } else {
            debug_assert!(expr_or_spreads.len() >= 2);

            let mut exprs = Vec::with_capacity(expr_or_spreads.len());
            for expr in expr_or_spreads {
                match expr {
                    ExprOrSpread {
                        spread: Some(..),
                        ref expr,
                    } => {
                        // syntax_error!(self, expr.span(), SyntaxError::SpreadInParenExpr)
                        panic!("SpreadInParenExpr at {:?}", expr.span());
                    }
                    ExprOrSpread { expr, .. } => exprs.push(expr),
                }
            }
            debug_assert!(exprs.len() >= 2);

            // span of sequence expression should not include '(', ')'
            let seq_expr = Box::new(Expr::Seq(SeqExpr {
                span: Span::new(
                    exprs.first().unwrap().span().lo(),
                    exprs.last().unwrap().span().hi(),
                ),
                exprs,
            }));
            Box::new(Expr::Paren(ParenExpr {
                span: span!(self, expr_start),
                expr: seq_expr,
            }))
        }
    }

    // TODO: replace this with whatever babel uses:
    pub(super) fn verify_expr(&mut self, expr: Box<Expr>) -> Box<Expr> {
        // todo!();
        expr
    }
}

#[ast_node]
pub(in crate::parser) enum PatOrExprOrSpread {
    Pat(Pat),
    ExprOrSpread(ExprOrSpread),
}

/// simple leaf methods.

impl<'a, I: Tokens> Parser<I> {
    fn parse_yield_expr(&mut self) -> Box<Expr> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("yield"));
        debug_assert!(self.ctx().in_generator);

        // Spec says
        // YieldExpression cannot be used within the FormalParameters of a generator
        // function because any expressions that are part of FormalParameters are
        // evaluated before the resulting generator object is in a resumable state.
        if self.ctx().in_parameters {
            // syntax_error!(self, self.input.prev_span(), SyntaxError::YieldParamInGen)
            panic!("YieldParamInGen at {:?}", self.input.prev_span());
        }

        if is!(self, ';')
            || (!self.input.is(&tok!('*'))
                && !self.input.cur().map(Token::starts_expr).unwrap_or(true))
        {
            Box::new(Expr::Yield(YieldExpr {
                span: span!(self, start),
                arg: None,
                delegate: false,
            }))
        } else {
            let has_star = self.input.eat(&tok!('*'));
            let arg = self.parse_assignment_expr();

            Box::new(Expr::Yield(YieldExpr {
                span: span!(self, start),
                arg: Some(arg),
                delegate: has_star,
            }))
        }
    }

    /// 12.2.5 Array Initializer
    pub(super) fn parse_lit(&mut self) -> Lit {
        let start = self.input.cur_pos();

        let v = match *cur!(self, true) {
            Word(Word::Null) => {
                self.input.bump();
                let span = span!(self, start);
                Lit::Null(Null { span })
            }
            Word(Word::True) | Word(Word::False) => {
                let value = self.input.is(&tok!("true"));
                self.input.bump();
                let span = span!(self, start);

                Lit::Bool(Bool { span, value })
            }
            Token::Str { .. } => match self.input.bump() {
                Token::Str { value, has_escape } => Lit::Str(Str {
                    span: span!(self, start),
                    value,
                    has_escape,
                    kind: StrKind::Normal {
                        contains_quote: true,
                    },
                }),
                _ => unreachable!(),
            },
            Token::Num(..) => match self.input.bump() {
                Token::Num(value) => Lit::Num(Number {
                    span: span!(self, start),
                    value,
                }),
                _ => unreachable!(),
            },
            Token::BigInt(..) => match self.input.bump() {
                Token::BigInt(value) => Lit::BigInt(BigInt {
                    span: span!(self, start),
                    value,
                }),
                _ => unreachable!(),
            },
            _ => unreachable!("parse_lit should not be called"),
        };
        v
    }

    pub(super) fn parse_dynamic_import(
        &mut self,
        start: BytePos,
        import_ident: Ident,
    ) -> Box<Expr> {
        // if !self.input.syntax().dynamic_import() {
        //     // syntax_error!(self, span!(self, start), SyntaxError::DynamicImport);
        //     panic!("DynamicImport at {:?}", span!(self, start));
        // }

        let args = self.parse_args(true);
        let import = Box::new(Expr::Call(CallExpr {
            span: span!(self, start),
            callee: ExprOrSuper::Expr(Box::new(Expr::Ident(import_ident))),
            args,
            type_args: Default::default(),
        }));

        self.parse_subscripts(ExprOrSuper::Expr(import), true)
    }

    pub(super) fn check_assign_target(&mut self, expr: &Expr /*, deny_call: bool*/) {
        if !expr.is_valid_simple_assignment_target(self.ctx().strict) {
            // self.emit_err(expr.span(), SyntaxError::TS2406);
            panic!("TS2406 at {:?}", expr.span());
        }

        // // We follow behavior of tsc
        // if self.input.syntax().typescript() && self.syntax().early_errors() {
        //     let is_eval_or_arguments = match *expr {
        //         Expr::Ident(ref i) => i.sym == js_word!("eval") || i.sym == js_word!("arguments"),
        //         _ => false,
        //     };

        //     if is_eval_or_arguments {
        //         self.emit_strict_mode_err(expr.span(), SyntaxError::TS1100);
        //     }

        //     fn should_deny(e: &Expr, deny_call: bool) -> bool {
        //         match e {
        //             Expr::Lit(..) => false,
        //             Expr::Call(..) => deny_call,
        //             Expr::Bin(..) => false,
        //             Expr::Paren(ref p) => should_deny(&p.expr, deny_call),

        //             _ => true,
        //         }
        //     }

        //     // It is an early Reference Error if LeftHandSideExpression is neither
        //     // an ObjectLiteral nor an ArrayLiteral and
        //     // IsValidSimpleAssignmentTarget of LeftHandSideExpression is false.
        //     if !is_eval_or_arguments
        //         && !expr.is_valid_simple_assignment_target(self.ctx().strict)
        //         && should_deny(&expr, deny_call)
        //     {
        //         self.emit_err(expr.span(), SyntaxError::TS2406);
        //     }
        // } else {
        //     if !expr.is_valid_simple_assignment_target(self.ctx().strict) {
        //         self.emit_err(expr.span(), SyntaxError::TS2406);
        //     }
        // }
    }
}

fn is_import(obj: &ExprOrSuper) -> bool {
    match *obj {
        ExprOrSuper::Expr(ref expr) => match **expr {
            Expr::Ident(Ident {
                sym: js_word!("import"),
                ..
            }) => true,
            _ => false,
        },
        _ => false,
    }
}
