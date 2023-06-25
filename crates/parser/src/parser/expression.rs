use super::{pat::PatType, util::ExprExt, *};
use crate::{lexer::TokenContext, token::AssignOpToken};
use atoms::js_word;
use either::Either;
use global_common::{Pos, Spanned};

mod ops;
mod verifier;

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

impl<I: Tokens> Parser<I> {
    // https://tc39.es/ecma262/#prod-Expression
    pub fn parse_expr(&mut self) -> PResult<Box<Expr>> {
        trace_cur!(self, parse_expr);

        let start = self.input.cur_pos();
        let expr = self.parse_assignment_expr()?;

        if self.input.is(&tok!(',')) {
            let mut exprs = vec![expr];

            while self.input.eat(&tok!(',')) {
                exprs.push(self.parse_assignment_expr()?);
            }

            return Ok(Box::new(Expr::Seq(SeqExpr {
                node_id: node_id!(self),
                span: span!(self, start),
                exprs,
            })));
        }

        Ok(expr)
    }

    ///`parseMaybeAssign` (overridden)
    pub(super) fn parse_assignment_expr(&mut self) -> PResult<Box<Expr>> {
        trace_cur!(self, parse_assignment_expr);

        if self.input.syntax().typescript() {
            // Note: When the JSX plugin is on, type assertions (`<T> x`) aren't valid
            // syntax.

            if is!(self, JSXTagStart) {
                let cur_context = self.input.token_context().current();
                debug_assert_eq!(cur_context, Some(TokenContext::JSXOpeningTag));
                // Only time j_oTag is pushed is right after j_expr.
                debug_assert_eq!(
                    self.input.token_context().0[self.input.token_context().len() - 2],
                    TokenContext::JSXExpr
                );

                let res = self.try_parse_ts(|p| p.parse_assignment_expr_base().map(Some));
                if let Some(res) = res {
                    return Ok(res);
                } else {
                    debug_assert_eq!(
                        self.input.token_context().current(),
                        Some(TokenContext::JSXOpeningTag)
                    );
                    self.input.token_context_mut().pop();
                    debug_assert_eq!(
                        self.input.token_context().current(),
                        Some(TokenContext::JSXExpr)
                    );
                    self.input.token_context_mut().pop();
                }
            }
        }

        if self.input.syntax().typescript()
            && (is_one_of!(self, '<', JSXTagStart))
            && peeked_is!(self, IdentName)
        {
            let res = self.try_parse_ts(|p| {
                let start = p.input.cur_pos();
                let type_parameters = p.parse_ts_type_params()?;
                let mut arrow = p.parse_assignment_expr_base()?;
                match *arrow {
                    Expr::Arrow(ArrowExpr {
                        ref mut span,
                        ref mut type_params,
                        ..
                    }) => {
                        *span = Span::new(start, span.hi, Default::default());
                        *type_params = Some(type_parameters);
                    }
                    _ => unexpected!(p, "("),
                }
                Ok(Some(arrow))
            });
            if let Some(res) = res {
                return Ok(res);
            }
        }

        self.parse_assignment_expr_base()
    }

    /// Parse an assignment expression. This includes applications of
    /// operators like `+=`.
    ///
    /// `parseMaybeAssign`
    fn parse_assignment_expr_base(&mut self) -> PResult<Box<Expr>> {
        trace_cur!(self, parse_assignment_expr_base);

        if self.ctx().in_generator && is!(self, "yield") {
            return self.parse_yield_expr();
        }

        let start = self.input.cur_pos();

        self.state.potential_arrow_start = match *cur!(self, true)? {
            Word(Word::Ident(..)) | tok!('(') | tok!("yield") => Some(start),
            _ => None,
        };

        let potential_arrow_start = self.state.potential_arrow_start;

        // Try to parse conditional expression.
        let cond = self.parse_cond_expr()?;

        return_if_arrow!(potential_arrow_start, cond);

        match *cond {
            // if cond is conditional expression but not left-hand-side expression,
            // just return it.
            Expr::Cond(..) | Expr::Bin(..) | Expr::Unary(..) | Expr::Update(..) => return Ok(cond),
            _ => {}
        }

        self.finish_assignment_expr(start, cond)
    }

    fn finish_assignment_expr(&mut self, start: BytePos, cond: Box<Expr>) -> PResult<Box<Expr>> {
        trace_cur!(self, finish_assignment_expr);

        match cur!(self, false) {
            Ok(&Token::AssignOp(op)) => {
                let left = if op == AssignOpToken::Assign {
                    self.reparse_expr_as_pat(PatType::AssignPat, cond)
                        .map(Box::new)
                        .map(PatOrExpr::Pat)?
                } else {
                    //It is an early Reference Error if IsValidSimpleAssignmentTarget of
                    // LeftHandSideExpression is false.
                    if !self.input.syntax().typescript()
                        && !cond.is_valid_simple_assignment_target(self.ctx().strict)
                    {
                        self.emit_err(cond.span(), SyntaxError::NotSimpleAssign)
                    }
                    let is_eval_or_arguments = match *cond {
                        Expr::Ident(ref i) => {
                            i.sym == js_word!("eval") || i.sym == js_word!("arguments")
                        }
                        _ => false,
                    };
                    if self.input.syntax().typescript() && is_eval_or_arguments {
                        self.emit_strict_mode_err(cond.span(), SyntaxError::TS1100);
                    }

                    // TODO(swc):
                    PatOrExpr::Expr(cond)
                };

                self.input.bump();
                let right = self.parse_assignment_expr()?;
                Ok(Box::new(Expr::Assign(AssignExpr {
                    node_id: node_id!(self),
                    span: span!(self, start),
                    op,
                    // TODO(swc):
                    left,
                    right,
                })))
            }
            _ => Ok(cond),
        }
    }

    /// Spec: 'ConditionalExpression'
    fn parse_cond_expr(&mut self) -> PResult<Box<Expr>> {
        trace_cur!(self, parse_cond_expr);

        let start = self.input.cur_pos();

        let potential_arrow_start = self.state.potential_arrow_start;

        let test = self.parse_bin_expr()?;
        return_if_arrow!(potential_arrow_start, test);

        if eat!(self, '?') {
            let ctx = Context {
                in_cond_expr: true,
                include_in_expr: true,
                ..self.ctx()
            };
            let cons = self.with_ctx(ctx).parse_assignment_expr()?;
            expect!(self, ':');
            let ctx = Context {
                in_cond_expr: true,
                ..self.ctx()
            };
            let alt = self.with_ctx(ctx).parse_assignment_expr()?;
            let span = Span::new(start, alt.span().hi(), Default::default());
            Ok(Box::new(Expr::Cond(CondExpr {
                node_id: node_id!(self),
                span,
                test,
                cons,
                alt,
            })))
        } else {
            Ok(test)
        }
    }

    /// Parse a primary expression or arrow function
    #[allow(clippy::cognitive_complexity)]
    pub(super) fn parse_primary_expr(&mut self) -> PResult<Box<Expr>> {
        trace_cur!(self, parse_primary_expr);

        let _ = self.input.cur();
        let start = self.input.cur_pos();

        let can_be_arrow = self
            .state
            .potential_arrow_start
            .map(|s| s == start)
            .unwrap_or(false);

        if let Some(tok) = self.input.cur() {
            match tok {
                tok!("this") => {
                    self.input.bump();
                    return Ok(Box::new(Expr::This(ThisExpr {
                        node_id: node_id!(self),
                        span: span!(self, start),
                    })));
                }

                tok!("import") => {
                    let import = self.parse_ident_name()?;
                    if self.input.syntax().import_meta() && is!(self, '.') {
                        return self
                            .parse_import_meta_prop(import)
                            .map(Expr::MetaProp)
                            .map(Box::new);
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

                    if can_be_arrow
                        && self.input.syntax().typescript()
                        && self.input.peeked_is(&tok!('<'))
                    {
                        // try parsing `async<T>() => {}`
                        if let Some(res) = self.try_parse_ts(|p| {
                            let start = p.input.cur_pos();
                            p.assert_and_bump(&tok!("async"));
                            p.try_parse_ts_generic_async_arrow_fn(start)
                        }) {
                            return Ok(Box::new(Expr::Arrow(res)));
                        }
                    }

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
                | Token::Num { .. }
                | Token::BigInt(..)
                | Token::Str { .. } => {
                    return Ok(Box::new(Expr::Lit(self.parse_lit()?)));
                }

                // Regexp
                Token::Regex(..) => match self.input.bump() {
                    Token::Regex(exp, flags) => {
                        return Ok(Box::new(Expr::Lit(Lit::Regex(Regex {
                            node_id: node_id!(self),
                            span: span!(self, start),
                            exp,
                            flags,
                        }))));
                    }
                    _ => unreachable!(),
                },

                tok!('`') => {
                    // parse template literal
                    return Ok(Box::new(Expr::Tpl(self.parse_tpl(false)?)));
                }

                tok!('(') => {
                    return self.parse_paren_expr_or_arrow_fn(can_be_arrow, None);
                }

                _ => {}
            }
        }

        let decorators = self.parse_decorators(false)?;

        if is!(self, "class") {
            return self.parse_class_expr(start, decorators);
        }

        if is!(self, "let")
            || (self.input.syntax().typescript() && is!(self, IdentName))
            || is!(self, IdentRef)
        {
            // TODO(swc): Handle [Yield, Await]
            let id = self.parse_ident_name()?;
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
                    self.emit_strict_mode_err(
                        self.input.prev_span(),
                        SyntaxError::InvalidIdentInStrict,
                    );
                }
                _ => {}
            }

            if can_be_arrow && id.sym == js_word!("async") && is!(self, BindingIdent) {
                // async a => body
                let arg = self.parse_binding_ident().map(Pat::from)?;
                let params = vec![ParamWithoutDecorators::from_pat(arg, node_id!(self))];
                expect!(self, "=>");
                let body = self.parse_fn_body(true, false)?;

                return Ok(Box::new(Expr::Arrow(ArrowExpr {
                    node_id: node_id!(self),
                    span: span!(self, start),
                    body,
                    params,
                    is_async: true,
                    return_type: None,
                    type_params: None,
                })));
            } else if can_be_arrow
                && !self.input.had_line_break_before_cur()
                && self.input.eat(&tok!("=>"))
            {
                let params = vec![ParamWithoutDecorators::from_pat(
                    Pat::Ident(BindingIdent::from_ident(id, node_id!(self))),
                    node_id!(self),
                )];
                let body = self.parse_fn_body(false, false)?;

                return Ok(Box::new(Expr::Arrow(ArrowExpr {
                    node_id: node_id!(self),
                    span: span!(self, start),
                    body,
                    params,
                    is_async: false,
                    // TODO(swc):
                    return_type: None,
                    // TODO(swc):
                    type_params: None,
                })));
            } else {
                return Ok(Box::new(Expr::Ident(id)));
            }
        }

        unexpected!(
            self,
            "this, import, async, function, [ for array literal, { for object literal, @ for \
             decorator, function, class, null, true, false, number, bigint, string, regexp, ` for \
             template literal, (, or an identifier"
        )
    }

    fn parse_array_lit(&mut self) -> PResult<Box<Expr>> {
        trace_cur!(self, parse_array_lit);

        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!('['));
        let mut elems = vec![];
        let mut trailing_comma_span = None;

        while !eof!(self) && !is!(self, ']') {
            if is!(self, ',') {
                expect!(self, ',');
                elems.push(None);
                continue;
            }
            let elem = self
                .include_in_expr(true)
                .parse_expr_or_spread()
                .map(Some)?;

            if is!(self, ',') {
                if let Some(ExprOrSpread::Spread(_)) = elem {
                    // We only care about the first trailing comma, so we
                    // will only save this one if it's the first we have
                    // encountered for this array. This prevents the last
                    // trailing comma from being reported rather than the
                    // first.
                    if trailing_comma_span.is_none() {
                        trailing_comma_span = Some(self.input.cur_span());
                    }
                }
                expect!(self, ',');
            }

            elems.push(elem);
        }

        expect!(self, ']');

        let span = span!(self, start);

        if let Some(trailing_comma_span) = trailing_comma_span {
            self.state
                .trailing_commas_after_rest
                .insert(span, trailing_comma_span);
        }

        Ok(Box::new(Expr::Array(ArrayLit {
            node_id: node_id!(self),
            span,
            elems,
        })))
    }

    /// `parseImportMetaProperty`
    pub(super) fn parse_import_meta_prop(&mut self, import: Ident) -> PResult<MetaPropExpr> {
        let meta = import;

        expect!(self, '.');

        let prop = if is!(self, "meta") {
            self.parse_ident_name()?
        } else {
            unexpected!(self, "meta");
        };

        Ok(MetaPropExpr {
            node_id: node_id!(self),
            meta,
            prop,
        })
    }

    fn parse_subscripts(&mut self, mut obj: ExprOrSuper, no_call: bool) -> PResult<Box<Expr>> {
        loop {
            obj = match self.parse_subscript(obj, no_call)? {
                (expr, false) => return Ok(expr),
                (expr, true) => ExprOrSuper::Expr(expr),
            }
        }
    }

    /// returned bool is true if this method should be called again.
    #[allow(clippy::cognitive_complexity)]
    fn parse_subscript(&mut self, obj: ExprOrSuper, no_call: bool) -> PResult<(Box<Expr>, bool)> {
        let start = obj.span().lo();

        if self.input.syntax().typescript() {
            if !self.input.had_line_break_before_cur() && is!(self, '!') {
                self.input.set_expr_allowed(false);
                self.assert_and_bump(&tok!('!'));

                let expr = match obj {
                    ExprOrSuper::Super(..) => unimplemented!("super!"),
                    ExprOrSuper::Expr(expr) => expr,
                };
                return Ok((
                    Box::new(Expr::TsNonNull(TsNonNullExpr {
                        node_id: node_id!(self),
                        span: span!(self, start),
                        expr,
                    })),
                    true,
                ));
            }

            // super() cannot be generic
            if !matches!(obj, ExprOrSuper::Super(_)) && is!(self, '<') {
                let obj_ref = &obj;
                // tsTryParseAndCatch is expensive, so avoid if not necessary.
                // There are number of things we are going to "maybe" parse, like type arguments
                // on tagged template expressions. If any of them fail, walk it back and
                // continue.
                let result = self.try_parse_ts(|p| {
                    if !no_call
                        && p.at_possible_async(match obj_ref {
                            ExprOrSuper::Expr(ref expr) => &*expr,
                            _ => unreachable!(),
                        })?
                    {
                        // Almost certainly this is a generic async function `async <T>() => ...
                        // But it might be a call with a type argument `async<T>();`
                        let async_arrow_fn = p.try_parse_ts_generic_async_arrow_fn(start)?;
                        if let Some(async_arrow_fn) = async_arrow_fn {
                            return Ok(Some((Box::new(Expr::Arrow(async_arrow_fn)), true)));
                        }
                    }

                    let type_args = p.parse_ts_type_args()?;

                    if !no_call && is!(p, '(') {
                        // possibleAsync always false here, because we would have handled it
                        // above. (won't be any undefined arguments)
                        let args = p.parse_args(is_import(&obj))?;

                        Ok(Some((
                            Box::new(Expr::Call(CallExpr {
                                node_id: node_id!(p),
                                span: span!(p, start),
                                callee: obj_ref.clone(),
                                type_args: Some(type_args),
                                args,
                            })),
                            true,
                        )))
                    } else if is!(p, '`') {
                        p.parse_tagged_tpl(
                            match *obj_ref {
                                ExprOrSuper::Expr(ref obj) => obj.clone(),
                                _ => unreachable!(),
                            },
                            Some(type_args),
                        )
                        .map(|expr| (Box::new(Expr::TaggedTpl(expr)), true))
                        .map(Some)
                    } else if no_call {
                        unexpected!(p, "`")
                    } else {
                        unexpected!(p, "( or `")
                    }
                });
                if let Some(result) = result {
                    return Ok(result);
                }
            }
        }

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
                        node_id: node_id!(self),
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
            let prop = self.include_in_expr(true).parse_expr()?;
            expect!(self, ']');
            let span = Span::new(obj.span().lo(), self.input.last_pos(), Default::default());
            debug_assert_eq!(obj.span().lo(), span.lo());

            return Ok((
                Box::new(wrap!(Expr::Member(MemberExpr {
                    node_id: node_id!(self),
                    span,
                    obj,
                    prop,
                    computed: true,
                }))),
                true,
            ));
        }

        if (question_dot_token.is_some()
            && self.input.is(&tok!('.'))
            && self.input.peeked_is(&tok!('('))
            && self.input.eat(&tok!('.')))
            || (!no_call && (self.input.is(&tok!('('))))
        {
            let args = self.parse_args(is_import(&obj))?;
            return Ok((
                Box::new(wrap!(Expr::Call(CallExpr {
                    node_id: node_id!(self),
                    span: span!(self, start),
                    callee: obj,
                    args,
                    type_args: None,
                }))),
                true,
            ));
        }

        // member expression
        // $obj.name
        if self.input.eat(&tok!('.')) {
            let prop: Box<Expr> = Box::new(self.parse_maybe_private_name().map(|e| match e {
                Either::Left(p) => Expr::PrivateName(p),
                Either::Right(i) => Expr::Ident(i),
            })?);
            let span = span!(self, obj.span().lo());
            debug_assert_eq!(obj.span().lo(), span.lo());
            debug_assert_eq!(prop.span().hi(), span.hi());

            return Ok((
                Box::new(wrap!(Expr::Member(MemberExpr {
                    node_id: node_id!(self),
                    span,
                    obj,

                    prop,
                    computed: false,
                }))),
                true,
            ));
        }

        match obj {
            ExprOrSuper::Expr(expr) => {
                // MemberExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
                if is!(self, '`') {
                    let tpl = self.parse_tagged_tpl(expr, None)?;
                    return Ok((Box::new(Expr::TaggedTpl(tpl)), true));
                }

                Ok((expr, false))
            }
            ExprOrSuper::Super(..) => {
                if no_call {
                    syntax_error!(self, self.input.cur_span(), SyntaxError::InvalidSuperCall);
                }
                syntax_error!(self, self.input.cur_span(), SyntaxError::InvalidSuper);
            }
        }
    }

    /// Parse call, dot, and `[]`-subscript expressions.
    pub(super) fn parse_lhs_expr(&mut self) -> PResult<Box<Expr>> {
        let start = self.input.cur_pos();

        // parse jsx
        if self.input.syntax().jsx() {
            fn into_expr(e: Either<JSXFragment, JSXElement>) -> Box<Expr> {
                match e {
                    Either::Left(l) => Box::new(l.into()),
                    Either::Right(r) => Box::new(Box::new(r).into()),
                }
            }
            match *cur!(self, true)? {
                Token::JSXText { .. } => {
                    return self
                        .parse_jsx_text()
                        .map(Lit::JSXText)
                        .map(Expr::Lit)
                        .map(Box::new);
                }
                Token::JSXTagStart => {
                    return self.parse_jsx_element().map(into_expr);
                }
                _ => {}
            }

            if is!(self, '<') && !peeked_is!(self, '!') {
                // In case we encounter an lt token here it will always be the start of
                // jsx as the lt sign is not allowed in places that expect an expression

                // FIXME(swc):
                // self.finishToken(tt.jsxTagStart);

                return self.parse_jsx_element().map(into_expr);
            }
        }

        // `super()` can't be handled from parse_new_expr()
        if self.input.eat(&tok!("super")) {
            let obj = ExprOrSuper::Super(Super {
                node_id: node_id!(self),
                span: span!(self, start),
            });
            return self.parse_subscripts(obj, false);
        }

        let potential_arrow_start = self.state.potential_arrow_start;

        let callee = self.parse_new_expr()?;
        return_if_arrow!(potential_arrow_start, callee);

        let type_args = if self.input.syntax().typescript() && is!(self, '<') {
            self.try_parse_ts(|p| {
                let type_args = p.parse_ts_type_args()?;
                if is!(p, '(') {
                    Ok(Some(type_args))
                } else {
                    Ok(None)
                }
            })
        } else {
            None
        };

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
            return Ok(Box::new(Expr::New(NewExpr { type_args, ..ne })));
        }
        // 'CallExpr' rule contains 'MemberExpr (...)',
        // and 'MemberExpr' rule contains 'new MemberExpr (...)'

        if is!(self, '(') {
            // This is parsed using production MemberExpression,
            // which is left-recursive.
            let callee = ExprOrSuper::Expr(callee);
            let args = self.parse_args(is_import(&callee))?;

            let call_expr = Box::new(Expr::Call(CallExpr {
                node_id: node_id!(self),
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
        Ok(callee)
    }

    pub(super) fn parse_expr_or_pat(&mut self) -> PResult<Box<Expr>> {
        self.parse_expr()
    }

    #[allow(clippy::cognitive_complexity)]
    pub(super) fn parse_args_or_pats(&mut self) -> PResult<Vec<PatOrExprOrSpread>> {
        trace_cur!(self, parse_args_or_pats);

        expect!(self, '(');

        let mut first = true;
        // TODO: maybe merge this with rest_span
        let mut current_item_has_spread = false;
        let mut items = vec![];
        let mut rest_span = None;

        // TODO(kdy1): optimize (once we parsed a pattern, we can parse everything else
        // as a pattern instead of reparsing)
        while !eof!(self) && !is!(self, ')') {
            if first {
                // TODO: this appears to be incorrect. See: (async x => x, 1, y => y)(1);
                if is!(self, "async") {
                    // https://github.com/swc-project/swc/issues/410
                    self.state.potential_arrow_start = Some(self.input.cur_pos());
                    let expr = self.parse_assignment_expr()?;
                    expect!(self, ')');
                    return Ok(vec![PatOrExprOrSpread::ExprOrSpread(ExprOrSpread::Expr(
                        expr,
                    ))]);
                }
            } else {
                if current_item_has_spread && is!(self, ',') {
                    syntax_error!(self, SyntaxError::CommaAfterRestElement);
                }
                expect!(self, ',');
                // Handle trailing comma.
                if is!(self, ')') {
                    break;
                }
            }

            current_item_has_spread = false;

            let start = self.input.cur_pos();
            self.state.potential_arrow_start = Some(start);
            let modifier_start = start;

            let has_modifier = self.eat_any_ts_modifier()?;
            let pat_start = self.input.cur_pos();

            let mut arg = {
                if self.input.syntax().typescript()
                    && (is!(self, IdentRef) || (is!(self, "...") && peeked_is!(self, IdentRef)))
                {
                    let spread = if eat!(self, "...") {
                        Some(self.input.prev_span())
                    } else {
                        None
                    };

                    // Here, we use parse_bin_expr() instead of parse_assignment_expr()
                    // because `x?: number` should not be parsed as a conditional expression
                    let expr = if spread.is_some() {
                        self.include_in_expr(true).parse_bin_expr()?
                    } else {
                        let mut expr = self.parse_bin_expr()?;

                        if let Ok(&Token::AssignOp(..)) = cur!(self, false) {
                            expr = self.finish_assignment_expr(start, expr)?
                        }

                        expr
                    };

                    match spread {
                        Some(dot3_token) => ExprOrSpread::Spread(SpreadElement {
                            node_id: node_id!(self),
                            dot3_token,
                            expr,
                        }),
                        None => ExprOrSpread::Expr(expr),
                    }
                } else {
                    self.include_in_expr(true).parse_expr_or_spread()?
                }
            };

            if matches!(arg, ExprOrSpread::Spread(_)) {
                current_item_has_spread = true;
            }

            let optional = if self.input.syntax().typescript() {
                if is!(self, '?') {
                    if peeked_is!(self, ',')
                        || peeked_is!(self, ':')
                        || peeked_is!(self, ')')
                        || peeked_is!(self, '=')
                    {
                        self.assert_and_bump(&tok!('?'));
                        let _ = cur!(self, false);
                        if current_item_has_spread {
                            self.emit_err(self.input.prev_span(), SyntaxError::TS1047);
                        }
                        match &arg {
                            ExprOrSpread::Spread(SpreadElement { expr, .. })
                            | ExprOrSpread::Expr(expr)
                                if matches!(expr.as_ref(), Expr::Ident(_)) => {}
                            _ => {
                                syntax_error!(
                                    self,
                                    arg.span(),
                                    SyntaxError::TsBindingPatCannotBeOptional
                                )
                            }
                        }
                        true
                    } else if let ExprOrSpread::Expr(test) = arg {
                        expect!(self, '?');
                        let ctx = Context {
                            in_cond_expr: true,
                            include_in_expr: true,
                            ..self.ctx()
                        };
                        let cons = self.with_ctx(ctx).parse_assignment_expr()?;
                        expect!(self, ':');
                        let ctx = Context {
                            in_cond_expr: true,
                            ..self.ctx()
                        };
                        let alt = self.with_ctx(ctx).parse_assignment_expr()?;

                        arg = ExprOrSpread::Expr(Box::new(Expr::Cond(CondExpr {
                            node_id: node_id!(self),
                            span: Span::new(start, alt.span().hi(), Default::default()),

                            test,
                            cons,
                            alt,
                        })));

                        false
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                false
            };

            if optional || (self.input.syntax().typescript() && is!(self, ':')) {
                // TODO(swc): `async(...args?: any[]) : any => {}`
                //
                // if self.input.syntax().typescript() && optional && arg.spread.is_some() {
                //     self.emit_err(self.input.prev_span(), SyntaxError::TS1047)
                // }

                let (expr, spread) = match arg {
                    ExprOrSpread::Spread(SpreadElement {
                        dot3_token, expr, ..
                    }) => (expr, Some(dot3_token)),
                    ExprOrSpread::Expr(expr) => (expr, None),
                };

                let mut pat = self.reparse_expr_as_pat(PatType::BindingPat, expr)?;
                if optional {
                    match pat {
                        Pat::Ident(ref mut i) => i.id.optional = true,
                        _ => unreachable!(),
                    }
                }
                if let Some(span) = spread {
                    if let Some(rest_span) = rest_span {
                        if self.syntax().early_errors() {
                            // Rest pattern must be last one.
                            syntax_error!(self, rest_span, SyntaxError::NonLastRestParam);
                        }
                    }
                    rest_span = Some(span);
                    pat = Pat::Rest(RestPat {
                        node_id: node_id!(self),
                        span: span!(self, pat_start),
                        dot3_token: span,
                        arg: Box::new(pat),
                        type_ann: None,
                    });
                }
                match pat {
                    Pat::Ident(BindingIdent {
                        id: Ident { ref mut span, .. },
                        ref mut type_ann,
                        ..
                    })
                    | Pat::Array(ArrayPat {
                        ref mut type_ann,
                        ref mut span,
                        ..
                    })
                    | Pat::Assign(AssignPat {
                        ref mut type_ann,
                        ref mut span,
                        ..
                    })
                    | Pat::Object(ObjectPat {
                        ref mut type_ann,
                        ref mut span,
                        ..
                    })
                    | Pat::Rest(RestPat {
                        ref mut type_ann,
                        ref mut span,
                        ..
                    }) => {
                        let new_type_ann = self.try_parse_ts_type_ann()?;
                        if new_type_ann.is_some() {
                            *span =
                                Span::new(pat_start, self.input.prev_span().hi, Default::default());
                        }
                        *type_ann = new_type_ann;
                    }
                    Pat::Expr(ref expr) => unreachable!("invalid pattern: Expr({:?})", expr),
                    Pat::Invalid(..) => {
                        // We don't have to panic here.
                        // See: https://github.com/swc-project/swc/issues/1170
                        //
                        // Also, as an exact error is added to the errors while
                        // creating `Invalid`, we don't have to emit a new
                        // error.
                    }
                }

                if eat!(self, '=') {
                    let right = self.parse_assignment_expr()?;
                    pat = Pat::Assign(AssignPat {
                        node_id: node_id!(self),
                        span: span!(self, pat_start),
                        left: Box::new(pat),
                        right,
                        type_ann: None,
                    });
                }

                if has_modifier {
                    self.emit_err(span!(self, modifier_start), SyntaxError::TS2369);
                }

                items.push(PatOrExprOrSpread::Pat(pat))
            } else {
                if has_modifier {
                    self.emit_err(span!(self, modifier_start), SyntaxError::TS2369);
                }

                items.push(PatOrExprOrSpread::ExprOrSpread(arg));
            }

            // https://github.com/swc-project/swc/issues/433
            if first && self.input.eat(&tok!("=>")) && {
                debug_assert_eq!(items.len(), 1);
                match &items[0] {
                    PatOrExprOrSpread::ExprOrSpread(
                        ExprOrSpread::Spread(SpreadElement { expr, .. }) | ExprOrSpread::Expr(expr),
                    )
                    | PatOrExprOrSpread::Pat(Pat::Expr(expr)) => matches!(**expr, Expr::Ident(..)),
                    PatOrExprOrSpread::Pat(Pat::Ident(..)) => true,
                    _ => false,
                }
            } {
                let params = self
                    .parse_paren_items_as_params(items)?
                    .into_iter()
                    .map(|p| ParamWithoutDecorators::from_pat(p, node_id!(self)))
                    .collect();

                let body: BlockStmtOrExpr = self.parse_fn_body(false, false)?;
                expect!(self, ')');
                let span = span!(self, start);

                return Ok(vec![PatOrExprOrSpread::ExprOrSpread(ExprOrSpread::Expr(
                    Box::new(
                        ArrowExpr {
                            node_id: node_id!(self),
                            span,
                            body,
                            is_async: false,
                            params,
                            type_params: None,
                            return_type: None,
                        }
                        .into(),
                    ),
                ))]);
            }

            first = false;
        }

        expect!(self, ')');
        Ok(items)
    }

    /// `is_new_expr`: true iff we are parsing production 'NewExpression'.
    fn parse_member_expr_or_new_expr(&mut self, is_new_expr: bool) -> PResult<Box<Expr>> {
        trace_cur!(self, parse_member_expr_or_new_expr);

        let start = self.input.cur_pos();
        if self.input.eat(&tok!("new")) {
            let span_of_new = span!(self, start);
            if self.input.eat(&tok!('.')) {
                if self.input.eat(&tok!("target")) {
                    let span_of_target = self.input.cur_span();

                    if word_contains_escape(&span_of_target, "target") {
                        syntax_error!(self, span_of_target, SyntaxError::EscapeInNewTarget);
                    }

                    let expr = Box::new(Expr::MetaProp(MetaPropExpr {
                        node_id: node_id!(self),
                        meta: self.new_ident(js_word!("new"), span_of_new),
                        prop: self.new_ident(js_word!("target"), span_of_target),
                    }));

                    return self.parse_subscripts(ExprOrSuper::Expr(expr), true);
                }

                unexpected!(self, "target")
            }

            let potential_arrow_start = self.state.potential_arrow_start;

            // 'NewExpression' allows new call without paren.
            let callee = self.parse_member_expr_or_new_expr(is_new_expr)?;
            return_if_arrow!(potential_arrow_start, callee);

            let type_args = if self.input.syntax().typescript() && is!(self, '<') {
                self.try_parse_ts(|p| {
                    let args = p.parse_ts_type_args()?;
                    if !is!(p, '(') {
                        // This will fail
                        expect!(p, '(');
                    }
                    Ok(Some(args))
                })
            } else {
                None
            };

            if !is_new_expr || is!(self, '(') {
                // Parsed with 'MemberExpression' production.
                let args = self.parse_args(false).map(Some)?;

                let new_expr = ExprOrSuper::Expr(Box::new(Expr::New(NewExpr {
                    node_id: node_id!(self),
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

            return Ok(Box::new(Expr::New(NewExpr {
                node_id: node_id!(self),
                span: span!(self, start),
                callee,
                args: None,
                type_args,
            })));
        }

        if self.input.eat(&tok!("super")) {
            let base = ExprOrSuper::Super(Super {
                node_id: node_id!(self),
                span: span!(self, start),
            });
            return self.parse_subscripts(base, true);
        }

        let potential_arrow_start = self.state.potential_arrow_start;

        let obj = self.parse_primary_expr()?;
        return_if_arrow!(potential_arrow_start, obj);

        self.parse_subscripts(ExprOrSuper::Expr(obj), true)
    }

    /// Parse `NewExpression`.
    /// This includes `MemberExpression`.
    pub(super) fn parse_new_expr(&mut self) -> PResult<Box<Expr>> {
        trace_cur!(self, parse_new_expr);

        self.parse_member_expr_or_new_expr(true)
    }

    /// Parse `Arguments[Yield, Await]`
    pub(super) fn parse_args(&mut self, is_dynamic_import: bool) -> PResult<Vec<ExprOrSpread>> {
        trace_cur!(self, parse_args);

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
                        syntax_error!(
                            self,
                            span!(self, start),
                            SyntaxError::TrailingCommaInsideImport
                        )
                    }

                    break;
                }
            }

            expr_or_spreads.push(self.include_in_expr(true).parse_expr_or_spread()?);
        }

        expect!(self, ')');
        Ok(expr_or_spreads)
    }

    /// AssignmentExpression[+In, ?Yield, ?Await]
    /// ...AssignmentExpression[+In, ?Yield, ?Await]
    pub(super) fn parse_expr_or_spread(&mut self) -> PResult<ExprOrSpread> {
        trace_cur!(self, parse_expr_or_spread);

        let start = self.input.cur_pos();

        if self.input.eat(&tok!("...")) {
            let dot3_token = span!(self, start);
            let node_id = node_id!(self);
            self.include_in_expr(true)
                .parse_assignment_expr()
                .map(|expr| {
                    ExprOrSpread::Spread(SpreadElement {
                        node_id,
                        dot3_token,
                        expr,
                    })
                })
        } else {
            self.parse_assignment_expr().map(ExprOrSpread::Expr)
        }
    }

    /// Parse paren expression or arrow function expression.
    fn parse_paren_expr_or_arrow_fn(
        &mut self,
        can_be_arrow: bool,
        async_span: Option<Span>,
    ) -> PResult<Box<Expr>> {
        trace_cur!(self, parse_paren_expr_or_arrow_fn);

        let expr_start = async_span
            .map(|x| x.lo())
            .unwrap_or_else(|| self.input.cur_pos());

        // At this point, we can't know if it's parenthesized
        // expression or head of arrow function.
        // But as all patterns of javascript is subset of
        // expressions, we can parse both as expression.

        let paren_items = self.include_in_expr(true).parse_args_or_pats()?;
        let has_pattern = paren_items
            .iter()
            .any(|item| matches!(item, PatOrExprOrSpread::Pat(..)));

        // This is slow path. We handle arrow in conditional expression.
        if self.syntax().typescript() && self.ctx().in_cond_expr && is!(self, ':') {
            // TODO(swc): Remove clone
            let items_ref = &paren_items;
            if let Some(expr) = self.try_parse_ts(|p| {
                let return_type = p.parse_ts_type_or_type_predicate_ann(&tok!(':'))?;

                expect!(p, "=>");

                let params = p
                    .parse_paren_items_as_params(items_ref.clone())?
                    .into_iter()
                    .map(|pat| ParamWithoutDecorators::from_pat(pat, node_id!(p)))
                    .collect();

                let body = p.parse_fn_body(async_span.is_some(), false)?;

                Ok(Some(Box::new(Expr::Arrow(ArrowExpr {
                    node_id: node_id!(p),
                    span: span!(p, expr_start),
                    is_async: async_span.is_some(),
                    params,
                    body,
                    return_type: Some(return_type),
                    type_params: None,
                }))))
            }) {
                return Ok(expr);
            }
        }

        let return_type = if !self.ctx().in_cond_expr
            && self.input.syntax().typescript()
            && is!(self, ':')
            && !self.ctx().in_case_cond
        {
            Some(self.parse_ts_type_or_type_predicate_ann(&tok!(':'))?)
        } else {
            None
        };

        // we parse arrow function at here, to handle it efficiently.
        if has_pattern || return_type.is_some() || is!(self, "=>") {
            if self.input.had_line_break_before_cur() {
                syntax_error!(
                    self,
                    span!(self, expr_start),
                    SyntaxError::LineBreakBeforeArrow
                );
            }
            if !can_be_arrow {
                syntax_error!(self, span!(self, expr_start), SyntaxError::ArrowNotAllowed);
            }
            expect!(self, "=>");

            let params = self
                .parse_paren_items_as_params(paren_items)?
                .into_iter()
                .map(|p| ParamWithoutDecorators::from_pat(p, node_id!(self)))
                .collect();

            let body: BlockStmtOrExpr = self.parse_fn_body(async_span.is_some(), false)?;
            let arrow_expr = ArrowExpr {
                node_id: node_id!(self),
                span: span!(self, expr_start),
                is_async: async_span.is_some(),
                params,
                body,
                return_type,
                type_params: None,
            };
            if let BlockStmtOrExpr::BlockStmt(..) = arrow_expr.body {
                if let Some(&Token::BinOp(..)) = self.input.cur() {
                    // ) is required
                    self.emit_err(self.input.cur_span(), SyntaxError::TS1005);
                    let errored_expr =
                        self.parse_bin_op_recursively(Box::new(arrow_expr.into()), 0)?;

                    if !is!(self, ';') {
                        // ; is required
                        self.emit_err(self.input.cur_span(), SyntaxError::TS1005);
                    }

                    return Ok(errored_expr);
                }
            }
            return Ok(Box::new(Expr::Arrow(arrow_expr)));
        }

        let expr_or_spreads = paren_items
            .into_iter()
            .map(|item| -> PResult<_> {
                match item {
                    PatOrExprOrSpread::ExprOrSpread(e) => Ok(e),
                    _ => syntax_error!(self, item.span(), SyntaxError::InvalidExpr),
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        if let Some(async_span) = async_span {
            // It's a call expression
            return Ok(Box::new(Expr::Call(CallExpr {
                node_id: node_id!(self),
                span: span!(self, async_span.lo()),
                callee: ExprOrSuper::Expr(Box::new(Expr::Ident(
                    self.new_ident("async".into(), async_span),
                ))),
                args: expr_or_spreads,
                type_args: None,
            })));
        }

        // It was not head of arrow function.

        if expr_or_spreads.is_empty() {
            syntax_error!(
                self,
                Span::new(expr_start, self.input.last_pos(), Default::default()),
                SyntaxError::EmptyParenExpr
            );
        }

        // TODO(swc): Verify that invalid expression like {a = 1} does not exists.

        // ParenthesizedExpression cannot contain spread.
        if expr_or_spreads.len() == 1 {
            let expr = match expr_or_spreads.into_iter().next().unwrap() {
                ExprOrSpread::Spread(s) => {
                    syntax_error!(self, s.expr.span(), SyntaxError::SpreadInParenExpr)
                }
                ExprOrSpread::Expr(e) => e,
            };
            Ok(Box::new(Expr::Paren(ParenExpr {
                node_id: node_id!(self),
                span: span!(self, expr_start),
                expr,
            })))
        } else {
            debug_assert!(expr_or_spreads.len() >= 2);

            let mut exprs = Vec::with_capacity(expr_or_spreads.len());
            for expr in expr_or_spreads {
                match expr {
                    ExprOrSpread::Spread(_) => {
                        syntax_error!(self, expr.span(), SyntaxError::SpreadInParenExpr)
                    }
                    ExprOrSpread::Expr(e) => exprs.push(e),
                }
            }
            debug_assert!(exprs.len() >= 2);

            // span of sequence expression should not include '(', ')'
            let seq_expr = Box::new(Expr::Seq(SeqExpr {
                node_id: node_id!(self),
                span: Span::new(
                    exprs.first().unwrap().span().lo(),
                    exprs.last().unwrap().span().hi(),
                    Default::default(),
                ),
                exprs,
            }));
            Ok(Box::new(Expr::Paren(ParenExpr {
                node_id: node_id!(self),
                span: span!(self, expr_start),
                expr: seq_expr,
            })))
        }
    }

    #[allow(clippy::vec_box)]
    fn parse_tpl_elements(
        &mut self,
        is_tagged: bool,
    ) -> PResult<(Vec<Box<Expr>>, Vec<TplElement>)> {
        trace_cur!(self, parse_tpl_elements);

        let mut exprs = vec![];

        let cur_elem = self.parse_tpl_element(is_tagged)?;
        let mut is_tail = cur_elem.tail;
        let mut quasis = vec![cur_elem];

        while !is_tail {
            expect!(self, "${");
            exprs.push(self.include_in_expr(true).parse_expr()?);
            expect!(self, '}');
            let elem = self.parse_tpl_element(is_tagged)?;
            is_tail = elem.tail;
            quasis.push(elem);
        }

        Ok((exprs, quasis))
    }

    fn parse_tagged_tpl(
        &mut self,
        tag: Box<Expr>,
        type_params: Option<TsTypeParamInstantiation>,
    ) -> PResult<TaggedTpl> {
        trace_cur!(self, parse_tagged_tpl);

        let tagged_tpl_start = tag.span().lo();

        let tpl = self.parse_tpl(true)?;

        let span = span!(self, tagged_tpl_start);
        Ok(TaggedTpl {
            node_id: node_id!(self),
            span,
            tag,
            type_params,
            tpl,
        })
    }

    pub(super) fn parse_tpl(&mut self, is_tagged: bool) -> PResult<Tpl> {
        trace_cur!(self, parse_tpl);

        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!('`'));

        let (exprs, quasis) = self.parse_tpl_elements(is_tagged)?;

        expect!(self, '`');

        let span = span!(self, start);
        Ok(Tpl {
            node_id: node_id!(self),
            span,
            exprs,
            quasis,
        })
    }

    pub(super) fn parse_tpl_element(&mut self, is_tagged: bool) -> PResult<TplElement> {
        let start = self.input.cur_pos();

        let (raw, cooked) = match *cur!(self, true)? {
            Token::Template { .. } => match self.input.bump() {
                Token::Template {
                    raw,
                    cooked,
                    has_escape,
                } => (
                    Str {
                        node_id: node_id!(self),
                        span: span!(self, start),
                        value: raw,
                        has_escape,
                        kind: StrKind::Normal {
                            contains_quote: false,
                        },
                    },
                    cooked.map(|cooked| Str {
                        node_id: node_id!(self),
                        span: span!(self, start),
                        value: cooked,
                        has_escape,
                        kind: StrKind::Normal {
                            contains_quote: false,
                        },
                    }),
                ),
                _ => unreachable!(),
            },
            _ => unexpected!(self, "template token"),
        };

        if cooked.is_none() && (!is_tagged || self.input.target() < JscTarget::Es2018) {
            syntax_error!(
                self,
                span!(self, start),
                SyntaxError::InvalidEscapeInTemplate
            )
        }

        let tail = is!(self, '`');
        Ok(TplElement {
            node_id: node_id!(self),
            span: span!(self, start),
            raw,
            tail,

            cooked,
        })
    }
}

/// Checks if the given span contains an escape sequence by comparing it's
/// length against that of the passed reference word.
fn word_contains_escape(span: &Span, word: &'static str) -> bool {
    span.hi.to_usize() - span.lo.to_usize() != word.len()
}

#[derive(::ast_node::Spanned, Debug, Clone, PartialEq)]
pub(in crate::parser) enum PatOrExprOrSpread {
    Pat(Pat),
    // TODO: maybe flatten
    ExprOrSpread(ExprOrSpread),
}

/// simple leaf methods.

impl<I: Tokens> Parser<I> {
    fn parse_yield_expr(&mut self) -> PResult<Box<Expr>> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("yield"));
        debug_assert!(self.ctx().in_generator);

        // Spec says
        // YieldExpression cannot be used within the FormalParameters of a generator
        // function because any expressions that are part of FormalParameters are
        // evaluated before the resulting generator object is in a resumable state.
        if self.ctx().in_parameters {
            syntax_error!(self, self.input.prev_span(), SyntaxError::YieldParamInGen)
        }

        if is!(self, ';')
            || (!self.input.is(&tok!('*'))
                && !self.input.cur().map(Token::starts_expr).unwrap_or(true))
        {
            Ok(Box::new(Expr::Yield(YieldExpr {
                node_id: node_id!(self),
                span: span!(self, start),
                arg: None,
                delegate: false,
            })))
        } else {
            let has_star = self.input.eat(&tok!('*'));
            let arg = self.parse_assignment_expr()?;

            Ok(Box::new(Expr::Yield(YieldExpr {
                node_id: node_id!(self),
                span: span!(self, start),
                arg: Some(arg),
                delegate: has_star,
            })))
        }
    }

    fn at_possible_async(&mut self, expr: &Expr) -> PResult<bool> {
        // The following todo is to match the behaviour of babel (and probably the spec).
        // We can use the length of the identifier to check the condition below.
        // If the length is 5 (the len of the word "async"), the ident does not
        // contain any escapes.

        // TODO(swc): !this.state.containsEsc &&

        Ok(self.state.potential_arrow_start == Some(expr.span().lo())
            && matches!(
                *expr,
                Expr::Ident(Ident {
                    sym: js_word!("async"),
                    ..
                })
            ))
    }

    /// 12.2.5 Array Initializer
    pub(super) fn parse_lit(&mut self) -> PResult<Lit> {
        let start = self.input.cur_pos();

        let v = match *cur!(self, true)? {
            Word(Word::Null) => {
                self.input.bump();
                let span = span!(self, start);
                Lit::Null(Null {
                    node_id: node_id!(self),
                    span,
                })
            }
            Word(Word::True) | Word(Word::False) => {
                let value = self.input.is(&tok!("true"));
                self.input.bump();
                let span = span!(self, start);

                Lit::Bool(Bool {
                    node_id: node_id!(self),
                    span,
                    value,
                })
            }
            Token::Str { .. } => match self.input.bump() {
                Token::Str { value, has_escape } => Lit::Str(Str {
                    node_id: node_id!(self),
                    span: span!(self, start),
                    value,
                    has_escape,
                    kind: StrKind::Normal {
                        contains_quote: true,
                    },
                }),
                _ => unreachable!(),
            },
            Token::Num { .. } => match self.input.bump() {
                Token::Num { value, raw } => Lit::Num(Number {
                    node_id: node_id!(self),
                    span: span!(self, start),
                    value,
                    raw: Some(raw),
                }),
                _ => unreachable!(),
            },
            Token::BigInt(..) => match self.input.bump() {
                Token::BigInt(value) => Lit::BigInt(BigInt {
                    node_id: node_id!(self),
                    span: span!(self, start),
                    value,
                }),
                _ => unreachable!(),
            },
            _ => unreachable!("parse_lit should not be called"),
        };
        Ok(v)
    }

    pub(super) fn parse_dynamic_import(
        &mut self,
        start: BytePos,
        import_ident: Ident,
    ) -> PResult<Box<Expr>> {
        if !self.input.syntax().dynamic_import() {
            syntax_error!(self, span!(self, start), SyntaxError::DynamicImport);
        }

        let args = self.parse_args(true)?;
        let import = Box::new(Expr::Call(CallExpr {
            node_id: node_id!(self),
            span: span!(self, start),
            callee: ExprOrSuper::Expr(Box::new(Expr::Ident(import_ident))),
            args,
            type_args: Default::default(),
        }));

        self.parse_subscripts(ExprOrSuper::Expr(import), true)
    }

    pub(super) fn check_assign_target(&mut self, expr: &Expr, deny_call: bool) {
        // We follow behaviour of tsc
        if self.input.syntax().typescript() && self.syntax().early_errors() {
            let is_eval_or_arguments = match *expr {
                Expr::Ident(ref i) => i.sym == js_word!("eval") || i.sym == js_word!("arguments"),
                _ => false,
            };

            if is_eval_or_arguments {
                self.emit_strict_mode_err(expr.span(), SyntaxError::TS1100);
            }

            fn should_deny(e: &Expr, deny_call: bool) -> bool {
                match e {
                    Expr::Lit(..) => false,
                    Expr::Call(..) => deny_call,
                    Expr::Bin(..) => false,
                    Expr::Paren(ref p) => should_deny(&p.expr, deny_call),

                    _ => true,
                }
            }

            // It is an early Reference Error if LeftHandSideExpression is neither
            // an ObjectLiteral nor an ArrayLiteral and
            // IsValidSimpleAssignmentTarget of LeftHandSideExpression is false.
            if !is_eval_or_arguments
                && !expr.is_valid_simple_assignment_target(self.ctx().strict)
                && should_deny(expr, deny_call)
            {
                self.emit_err(expr.span(), SyntaxError::TS2406);
            }
        } else if !expr.is_valid_simple_assignment_target(self.ctx().strict) {
            self.emit_err(expr.span(), SyntaxError::TS2406);
        }
    }
}

fn is_import(obj: &ExprOrSuper) -> bool {
    match *obj {
        ExprOrSuper::Expr(ref expr) => matches!(
            **expr,
            Expr::Ident(Ident {
                sym: js_word!("import"),
                ..
            })
        ),
        _ => false,
    }
}
