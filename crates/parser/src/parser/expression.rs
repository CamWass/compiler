use super::{pat::PatType, util::is_valid_simple_assignment_target, *};
use crate::token::AssignOpToken;
use atoms::js_word;
use either::Either;
use global_common::Pos;

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
    pub(super) fn parse_expr(&mut self) -> PResult<MaybeParen> {
        trace_cur!(self, parse_expr);

        let start = self.input.cur_pos();
        let expr = self.parse_assignment_expr()?;

        if self.input.is(&tok!(',')) {
            let mut exprs = vec![expr.unwrap()];

            while self.input.eat(&tok!(',')) {
                exprs.push(self.parse_assignment_expr()?.unwrap());
            }

            return Ok(Box::new(Expr::Seq(SeqExpr {
                node_id: node_id!(self, span!(self, start)),
                exprs,
            }))
            .into());
        }

        Ok(expr)
    }

    ///`parseMaybeAssign` (overridden)
    pub(super) fn parse_assignment_expr(&mut self) -> PResult<MaybeParen> {
        trace_cur!(self, parse_assignment_expr);

        if self.input.syntax().typescript() && is!(self, '<') && peeked_is!(self, IdentName) {
            let res = self.try_parse_ts(|p| {
                let start = p.input.cur_pos();
                // Type params.
                p.eat_ts_type_params(|_, _| {})?;
                let arrow = p.parse_assignment_expr_base()?;
                match &arrow {
                    MaybeParen::Expr(arrow) => match arrow.as_ref() {
                        Expr::Arrow(ArrowExpr { node_id, .. }) => {
                            let s = get_span!(p, *node_id);
                            set_span!(p, *node_id, Span::new(start, s.hi,));
                        }
                        _ => unexpected!(p, "("),
                    },
                    MaybeParen::Wrapped(_) => unexpected!(p, "("),
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
    fn parse_assignment_expr_base(&mut self) -> PResult<MaybeParen> {
        trace_cur!(self, parse_assignment_expr_base);

        if self.ctx().in_generator && is!(self, "yield") {
            return self.parse_yield_expr().map(From::from);
        }

        let start = self.input.cur_pos();

        self.potential_arrow_start = match *cur!(self, true)? {
            Word(Word::Ident(..)) | tok!('(') | tok!("yield") => Some(start),
            _ => None,
        };

        let potential_arrow_start = self.potential_arrow_start;

        // Try to parse conditional expression.
        let cond = self.parse_cond_expr()?;

        return_if_arrow!(self, potential_arrow_start, cond);

        if let MaybeParen::Expr(cond_expr) = &cond {
            match cond_expr.as_ref() {
                // if cond is conditional expression but not left-hand-side expression,
                // just return it.
                Expr::Cond(..) | Expr::Bin(..) | Expr::Unary(..) | Expr::Update(..) => {
                    return Ok(cond)
                }
                _ => {}
            }
        }

        self.finish_assignment_expr(start, cond)
    }

    fn finish_assignment_expr(&mut self, start: BytePos, cond: MaybeParen) -> PResult<MaybeParen> {
        trace_cur!(self, finish_assignment_expr);

        match cur!(self, false) {
            Ok(&Token::AssignOp(op)) => {
                let left = if op == AssignOpToken::Assign {
                    self.reparse_expr_as_pat(PatType::AssignPat, cond.unwrap())
                        .map(Box::new)
                        .map(PatOrExpr::Pat)?
                } else {
                    //It is an early Reference Error if IsValidSimpleAssignmentTarget of
                    // LeftHandSideExpression is false.
                    if !self.input.syntax().typescript()
                        && !is_valid_simple_assignment_target(cond.inner(), self.ctx().strict)
                    {
                        self.emit_err(
                            get_span!(self, cond.node_id()),
                            SyntaxError::NotSimpleAssign,
                        )
                    }
                    let is_eval_or_arguments = match &cond {
                        MaybeParen::Expr(cond) => match cond.as_ref() {
                            Expr::Ident(i) => {
                                i.sym == js_word!("eval") || i.sym == js_word!("arguments")
                            }
                            _ => false,
                        },
                        MaybeParen::Wrapped(_) => false,
                    };
                    if self.input.syntax().typescript() && is_eval_or_arguments {
                        self.emit_strict_mode_err(
                            get_span!(self, cond.node_id()),
                            SyntaxError::TS1100,
                        );
                    }

                    // TODO(swc):
                    PatOrExpr::Expr(cond.unwrap())
                };

                self.input.bump();
                let right = self.parse_assignment_expr()?.unwrap();
                Ok(Box::new(Expr::Assign(AssignExpr {
                    node_id: node_id!(self, span!(self, start)),
                    op,
                    // TODO(swc):
                    left,
                    right,
                }))
                .into())
            }
            _ => Ok(cond),
        }
    }

    /// Spec: 'ConditionalExpression'
    fn parse_cond_expr(&mut self) -> PResult<MaybeParen> {
        trace_cur!(self, parse_cond_expr);

        let start = self.input.cur_pos();

        let potential_arrow_start = self.potential_arrow_start;

        let test = self.parse_bin_expr()?;
        return_if_arrow!(self, potential_arrow_start, test);

        if eat!(self, '?') {
            let ctx = Context {
                in_cond_expr: true,
                include_in_expr: true,
                ..self.ctx()
            };
            let cons = self.with_ctx(ctx).parse_assignment_expr()?.unwrap();
            expect!(self, ':');
            let ctx = Context {
                in_cond_expr: true,
                ..self.ctx()
            };
            let alt = self.with_ctx(ctx).parse_assignment_expr()?.unwrap();
            let hi = get_span!(self, alt.node_id()).hi();
            let span = Span::new(start, hi);
            Ok(Box::new(Expr::Cond(CondExpr {
                node_id: node_id!(self, span),
                test: test.unwrap(),
                cons,
                alt,
            }))
            .into())
        } else {
            Ok(test)
        }
    }

    /// Parse a primary expression or arrow function
    #[allow(clippy::cognitive_complexity)]
    fn parse_primary_expr(&mut self) -> PResult<MaybeParen> {
        trace_cur!(self, parse_primary_expr);

        let _ = self.input.cur();
        let start = self.input.cur_pos();

        let can_be_arrow = self
            .potential_arrow_start
            .map(|s| s == start)
            .unwrap_or(false);

        if let Some(tok) = self.input.cur() {
            match tok {
                tok!("this") => {
                    self.input.bump();
                    return Ok(Box::new(Expr::This(ThisExpr {
                        node_id: node_id!(self, span!(self, start)),
                    }))
                    .into());
                }

                tok!("import") => {
                    let import = self.parse_ident_name()?;
                    if self.input.syntax().import_meta() && is!(self, '.') {
                        return self
                            .parse_import_meta_prop(import)
                            .map(Expr::MetaProp)
                            .map(Box::new)
                            .map(From::from);
                    }

                    return self.parse_dynamic_import(start, import);
                }

                tok!("async") => {
                    if self.input.peeked_is(&tok!("function"))
                        && !self.input.has_linebreak_between_cur_and_peeked()
                    {
                        // handle `async function` expression
                        return self.parse_async_fn_expr().map(From::from);
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
                            return Ok(Box::new(Expr::Arrow(res)).into());
                        }
                    }

                    if can_be_arrow && self.input.peeked_is(&tok!('(')) {
                        expect!(self, "async");
                        let async_span = self.input.prev_span();
                        return self.parse_paren_expr_or_arrow_fn(can_be_arrow, Some(async_span));
                    }
                }

                tok!('[') => {
                    return self.parse_array_lit().map(From::from);
                }

                tok!('{') => {
                    return self.parse_object::<Box<Expr>>().map(From::from);
                }

                // Handle FunctionExpression and GeneratorExpression
                tok!("function") => {
                    return self.parse_fn_expr().map(From::from);
                }

                // Literals
                tok!("null")
                | tok!("true")
                | tok!("false")
                | Token::Num { .. }
                | Token::BigInt(..)
                | Token::Str { .. } => {
                    return Ok(Box::new(Expr::Lit(self.parse_lit()?)).into());
                }

                // Regexp
                Token::Regex(..) => match self.input.bump() {
                    Token::Regex(exp, flags) => {
                        return Ok(Box::new(Expr::Lit(Lit::Regex(Regex {
                            node_id: node_id!(self, span!(self, start)),
                            exp,
                            flags,
                        })))
                        .into());
                    }
                    _ => unreachable!(),
                },

                tok!('`') => {
                    // parse template literal
                    return Ok(Box::new(Expr::Tpl(self.parse_tpl(false)?)).into());
                }

                tok!('(') => {
                    return self.parse_paren_expr_or_arrow_fn(can_be_arrow, None);
                }

                _ => {}
            }
        }

        if is!(self, "class") {
            return self.parse_class_expr(start).map(From::from);
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
                let arg = self.parse_binding_ident().map(Pat::Ident)?;
                let params = vec![Param::from_pat(arg, program_data!(self))];
                expect!(self, "=>");
                let body = self.parse_fn_body(true, false)?;
                let body = self.make_arrow_fn_block(body);

                return Ok(Box::new(Expr::Arrow(ArrowExpr {
                    node_id: node_id!(self, span!(self, start)),
                    body,
                    params,
                    is_async: true,
                }))
                .into());
            } else if can_be_arrow
                && !self.input.had_line_break_before_cur()
                && self.input.eat(&tok!("=>"))
            {
                let pat = Pat::Ident(BindingIdent::from_ident(id, program_data!(self)));
                let params = vec![Param::from_pat(pat, program_data!(self))];
                let body = self.parse_fn_body(false, false)?;
                let body = self.make_arrow_fn_block(body);

                return Ok(Box::new(Expr::Arrow(ArrowExpr {
                    node_id: node_id!(self, span!(self, start)),
                    body,
                    params,
                    is_async: false,
                }))
                .into());
            } else {
                return Ok(Box::new(Expr::Ident(id)).into());
            }
        }

        unexpected!(
            self,
            "this, import, async, function, [ for array literal, { for object literal, @ for \
             function, class, null, true, false, number, bigint, string, regexp, ` for \
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
                .map(MaybeParenExprOrSpread::unwrap)
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

        let node_id = node_id!(self, span!(self, start));

        if let Some(trailing_comma_span) = trailing_comma_span {
            self.trailing_commas_after_rest
                .insert(node_id, trailing_comma_span);
        }

        Ok(Box::new(Expr::Array(ArrayLit { node_id, elems })))
    }

    /// `parseImportMetaProperty`
    fn parse_import_meta_prop(&mut self, import: Ident) -> PResult<MetaPropExpr> {
        let start = self.input.cur_pos();
        let meta = import;

        expect!(self, '.');

        let prop = if is!(self, "meta") {
            self.parse_ident_name()?
        } else {
            unexpected!(self, "meta");
        };

        let span = Span::new(start, self.input.last_pos());
        Ok(MetaPropExpr {
            node_id: node_id!(self, span),
            meta,
            prop,
        })
    }

    fn parse_subscripts(
        &mut self,
        mut obj: MaybeParenExprOrSuper,
        no_call: bool,
    ) -> PResult<MaybeParen> {
        loop {
            obj = match self.parse_subscript(obj, no_call)? {
                (expr, false) => return Ok(expr),
                (expr, true) => MaybeParenExprOrSuper::Expr(expr),
            }
        }
    }

    /// returned bool is true if this method should be called again.
    #[allow(clippy::cognitive_complexity)]
    fn parse_subscript(
        &mut self,
        obj: MaybeParenExprOrSuper,
        no_call: bool,
    ) -> PResult<(MaybeParen, bool)> {
        let start = get_span!(self, obj.node_id()).lo();

        if self.input.syntax().typescript() {
            if !self.input.had_line_break_before_cur() && is!(self, '!') {
                self.input.set_expr_allowed(false);
                self.assert_and_bump(&tok!('!'));

                let expr = match obj {
                    MaybeParenExprOrSuper::Super(..) => unimplemented!("super!"),
                    MaybeParenExprOrSuper::Expr(expr) => expr,
                };
                return Ok((expr, true));
            }

            // super() cannot be generic
            if !matches!(obj, MaybeParenExprOrSuper::Super(_)) && is!(self, '<') {
                // tsTryParseAndCatch is expensive, so avoid if not necessary.
                // There are number of things we are going to "maybe" parse, like type arguments
                // on tagged template expressions. If any of them fail, walk it back and
                // continue.
                let result = self.try_parse_ts(|p| {
                    if !no_call
                        && p.at_possible_async(match &obj {
                            MaybeParenExprOrSuper::Expr(expr) => expr,
                            _ => unreachable!(),
                        })
                    {
                        // Almost certainly this is a generic async function `async <T>() => ...
                        // But it might be a call with a type argument `async<T>();`
                        let async_arrow_fn = p.try_parse_ts_generic_async_arrow_fn(start)?;
                        if let Some(async_arrow_fn) = async_arrow_fn {
                            return Ok(Some((Box::new(Expr::Arrow(async_arrow_fn)).into(), true)));
                        }
                    }

                    // Type args.
                    p.parse_ts_type_args()?;

                    if !no_call && is!(p, '(') {
                        // possibleAsync always false here, because we would have handled it
                        // above. (won't be any undefined arguments)
                        let args = p.parse_args(is_import(&obj))?;

                        Ok(Some((
                            Box::new(Expr::Call(CallExpr {
                                node_id: node_id!(p, span!(p, start)),
                                callee: obj.clone_node(program_data!(p)).unwrap(),
                                args,
                            }))
                            .into(),
                            true,
                        )))
                    } else if is!(p, '`') {
                        p.parse_tagged_tpl(match &obj {
                            MaybeParenExprOrSuper::Expr(obj) => {
                                obj.clone_node(program_data!(p)).unwrap()
                            }
                            _ => unreachable!(),
                        })
                        .map(|expr| (Box::new(Expr::TaggedTpl(expr)).into(), true))
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

        let has_question_dot_token = if is!(self, '?') && self.input.peeked_is(&tok!('.')) {
            self.input.eat(&tok!('?'));
            true
        } else {
            false
        };

        /// Wrap with optional chaining
        macro_rules! wrap {
            ($e:expr) => {{
                if has_question_dot_token {
                    Expr::OptChain(OptChainExpr {
                        node_id: node_id!(self, span!(self, start)),
                        expr: Box::new($e),
                    })
                } else {
                    $e
                }
            }};
        }

        // $obj[name()]
        if (has_question_dot_token
            && is!(self, '.')
            && self.input.peeked_is(&tok!('['))
            && self.input.eat(&tok!('.'))
            && self.input.eat(&tok!('[')))
            || self.input.eat(&tok!('['))
        {
            let prop = self.include_in_expr(true).parse_expr()?.unwrap();
            expect!(self, ']');
            let obj_span_lo = get_span!(self, obj.node_id()).lo();
            let span = Span::new(obj_span_lo, self.input.last_pos());
            debug_assert_eq!(obj_span_lo, span.lo());

            return Ok((
                Box::new(wrap!(Expr::Member(MemberExpr {
                    node_id: node_id!(self, span),
                    obj: obj.unwrap(),
                    prop,
                    computed: true,
                })))
                .into(),
                true,
            ));
        }

        if (has_question_dot_token
            && self.input.is(&tok!('.'))
            && self.input.peeked_is(&tok!('('))
            && self.input.eat(&tok!('.')))
            || (!no_call && (self.input.is(&tok!('('))))
        {
            let args = self.parse_args(is_import(&obj))?;
            return Ok((
                Box::new(wrap!(Expr::Call(CallExpr {
                    node_id: node_id!(self, span!(self, start)),
                    callee: obj.unwrap(),
                    args,
                })))
                .into(),
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
            let span = span!(self, get_span!(self, obj.node_id()).lo());
            debug_assert_eq!(get_span!(self, obj.node_id()).lo(), span.lo());
            debug_assert_eq!(get_span!(self, prop.node_id()).hi(), span.hi());

            return Ok((
                Box::new(wrap!(Expr::Member(MemberExpr {
                    node_id: node_id!(self, span),
                    obj: obj.unwrap(),

                    prop,
                    computed: false,
                })))
                .into(),
                true,
            ));
        }

        match obj {
            MaybeParenExprOrSuper::Expr(expr) => {
                // MemberExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
                if is!(self, '`') {
                    let tpl = self.parse_tagged_tpl(expr.unwrap())?;
                    return Ok((Box::new(Expr::TaggedTpl(tpl)).into(), true));
                }

                Ok((expr, false))
            }
            MaybeParenExprOrSuper::Super(..) => {
                if no_call {
                    syntax_error!(self, self.input.cur_span(), SyntaxError::InvalidSuperCall);
                }
                syntax_error!(self, self.input.cur_span(), SyntaxError::InvalidSuper);
            }
        }
    }

    /// Parse call, dot, and `[]`-subscript expressions.
    pub(super) fn parse_lhs_expr(&mut self) -> PResult<MaybeParen> {
        let start = self.input.cur_pos();

        // `super()` can't be handled from parse_new_expr()
        if self.input.eat(&tok!("super")) {
            let obj = MaybeParenExprOrSuper::Super(Super {
                node_id: node_id!(self, span!(self, start)),
            });
            return self.parse_subscripts(obj, false);
        }

        let potential_arrow_start = self.potential_arrow_start;

        let callee = self.parse_new_expr()?;
        return_if_arrow!(self, potential_arrow_start, callee);

        let type_args = if self.input.syntax().typescript() && is!(self, '<') {
            self.try_parse_ts(|p| {
                p.parse_ts_type_args()?;
                if is!(p, '(') {
                    Ok(Some(()))
                } else {
                    Ok(None)
                }
            })
        } else {
            None
        };

        if let MaybeParen::Expr(callee_expr) = &callee {
            if let Expr::New(NewExpr { args: None, .. }) = callee_expr.as_ref() {
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
                return Ok(callee);
            }
        }
        // 'CallExpr' rule contains 'MemberExpr (...)',
        // and 'MemberExpr' rule contains 'new MemberExpr (...)'

        if is!(self, '(') {
            // This is parsed using production MemberExpression,
            // which is left-recursive.
            let callee = MaybeParenExprOrSuper::Expr(callee);
            let args = self.parse_args(is_import(&callee))?;

            let call_expr = Box::new(Expr::Call(CallExpr {
                node_id: node_id!(self, span!(self, start)),

                callee: callee.unwrap(),
                args,
            }))
            .into();

            return self.parse_subscripts(MaybeParenExprOrSuper::Expr(call_expr), false);
        }
        if type_args.is_some() {
            // This fails
            expect!(self, '(');
        }

        // This is parsed using production 'NewExpression', which contains
        // 'MemberExpression'
        Ok(callee)
    }

    pub(super) fn parse_expr_or_pat(&mut self) -> PResult<MaybeParen> {
        self.parse_expr()
    }

    #[allow(clippy::cognitive_complexity)]
    fn parse_args_or_pats(&mut self) -> PResult<Vec<MaybeParenPatOrExprOrSpread>> {
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
                    self.potential_arrow_start = Some(self.input.cur_pos());
                    let expr = self.parse_assignment_expr()?;
                    expect!(self, ')');
                    return Ok(vec![MaybeParenPatOrExprOrSpread::Expr(expr)]);
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
            self.potential_arrow_start = Some(start);
            let modifier_start = start;

            let has_modifier = self.eat_any_ts_modifier()?;
            let pat_start = self.input.cur_pos();

            let mut arg = {
                if self.input.syntax().typescript()
                    && (is!(self, IdentRef) || (is!(self, "...") && peeked_is!(self, IdentRef)))
                {
                    let spread_start = if eat!(self, "...") {
                        Some(self.input.prev_span().lo)
                    } else {
                        None
                    };

                    // Here, we use parse_bin_expr() instead of parse_assignment_expr()
                    // because `x?: number` should not be parsed as a conditional expression
                    let expr = if spread_start.is_some() {
                        self.include_in_expr(true).parse_bin_expr()?
                    } else {
                        let mut expr = self.parse_bin_expr()?;

                        if let Ok(&Token::AssignOp(..)) = cur!(self, false) {
                            expr = self.finish_assignment_expr(start, expr)?
                        }

                        expr
                    };

                    match spread_start {
                        Some(spread_start) => {
                            let span = Span::new(spread_start, self.input.cur_pos());
                            MaybeParenExprOrSpread::Spread(MaybeParenSpreadElement {
                                node_id: node_id!(self, span),
                                expr,
                            })
                        }
                        None => MaybeParenExprOrSpread::Expr(expr),
                    }
                } else {
                    self.include_in_expr(true).parse_expr_or_spread()?
                }
            };

            if matches!(arg, MaybeParenExprOrSpread::Spread(_)) {
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
                            MaybeParenExprOrSpread::Spread(MaybeParenSpreadElement {
                                expr,
                                ..
                            })
                            | MaybeParenExprOrSpread::Expr(expr)
                                if matches!(expr, MaybeParen::Expr(e) if matches!(e.as_ref(), Expr::Ident(_))) =>
                                {}
                            _ => {
                                syntax_error!(
                                    self,
                                    get_span!(self, arg.node_id()),
                                    SyntaxError::TsBindingPatCannotBeOptional
                                )
                            }
                        }
                        true
                    } else if let MaybeParenExprOrSpread::Expr(test) = arg {
                        expect!(self, '?');
                        let ctx = Context {
                            in_cond_expr: true,
                            include_in_expr: true,
                            ..self.ctx()
                        };
                        let cons = self.with_ctx(ctx).parse_assignment_expr()?.unwrap();
                        expect!(self, ':');
                        let ctx = Context {
                            in_cond_expr: true,
                            ..self.ctx()
                        };
                        let alt = self.with_ctx(ctx).parse_assignment_expr()?.unwrap();

                        let hi = get_span!(self, alt.node_id()).hi();
                        let span = Span::new(start, hi);
                        arg = MaybeParenExprOrSpread::Expr(
                            Box::new(Expr::Cond(CondExpr {
                                node_id: node_id!(self, span),

                                test: test.unwrap(),
                                cons,
                                alt,
                            }))
                            .into(),
                        );

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
                    MaybeParenExprOrSpread::Spread(MaybeParenSpreadElement {
                        expr,
                        node_id,
                        ..
                    }) => (expr, Some(get_span!(self, node_id))),
                    MaybeParenExprOrSpread::Expr(expr) => (expr, None),
                };

                let mut pat = self.reparse_expr_as_pat(PatType::BindingPat, expr.unwrap())?;

                if let Some(span) = spread {
                    if let Some(rest_span) = rest_span {
                        if self.syntax().early_errors() {
                            // Rest pattern must be last one.
                            syntax_error!(self, rest_span, SyntaxError::NonLastRestParam);
                        }
                    }
                    rest_span = Some(span);
                    pat = Pat::Rest(RestPat {
                        node_id: node_id!(self, span!(self, pat_start)),
                        arg: Box::new(pat),
                    });
                }
                match &pat {
                    Pat::Ident(BindingIdent {
                        id: Ident { node_id, .. },
                        ..
                    })
                    | Pat::Array(ArrayPat { node_id, .. })
                    | Pat::Assign(AssignPat { node_id, .. })
                    | Pat::Object(ObjectPat { node_id, .. })
                    | Pat::Rest(RestPat { node_id, .. }) => {
                        let new_type_ann = self.try_parse_ts_type_ann()?;
                        if new_type_ann.is_some() {
                            set_span!(
                                self,
                                *node_id,
                                Span::new(pat_start, self.input.prev_span().hi,)
                            );
                        }
                    }
                    Pat::Expr(expr) => unreachable!("invalid pattern: Expr({:?})", expr),
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
                    let right = self.parse_assignment_expr()?.unwrap();
                    pat = Pat::Assign(AssignPat {
                        node_id: node_id!(self, span!(self, pat_start)),
                        left: Box::new(pat),
                        right,
                    });
                }

                if has_modifier {
                    self.emit_err(span!(self, modifier_start), SyntaxError::TS2369);
                }

                items.push(MaybeParenPatOrExprOrSpread::Pat(pat))
            } else {
                if has_modifier {
                    self.emit_err(span!(self, modifier_start), SyntaxError::TS2369);
                }

                items.push(match arg {
                    MaybeParenExprOrSpread::Expr(n) => MaybeParenPatOrExprOrSpread::Expr(n),
                    MaybeParenExprOrSpread::Spread(n) => MaybeParenPatOrExprOrSpread::Spread(n),
                });
            }

            // https://github.com/swc-project/swc/issues/433
            if first && self.input.eat(&tok!("=>")) && {
                debug_assert_eq!(items.len(), 1);
                match &items[0] {
                    MaybeParenPatOrExprOrSpread::Spread(MaybeParenSpreadElement {
                        expr, ..
                    })
                    | MaybeParenPatOrExprOrSpread::Expr(expr) => {
                        matches!(expr, MaybeParen::Expr(e) if matches!(e.as_ref(), Expr::Ident(_)))
                    }
                    MaybeParenPatOrExprOrSpread::Pat(Pat::Expr(expr)) => {
                        matches!(**expr, Expr::Ident(..))
                    }
                    MaybeParenPatOrExprOrSpread::Pat(Pat::Ident(..)) => true,
                    _ => false,
                }
            } {
                let params = self
                    .parse_paren_items_as_params(items)?
                    .into_iter()
                    .map(|p| Param::from_pat(p, program_data!(self)))
                    .collect();

                let body: BlockStmtOrExpr = self.parse_fn_body(false, false)?;
                let body = self.make_arrow_fn_block(body);
                expect!(self, ')');
                let span = span!(self, start);

                return Ok(vec![MaybeParenPatOrExprOrSpread::Expr(
                    Box::new(Expr::Arrow(ArrowExpr {
                        node_id: node_id!(self, span),
                        body,
                        is_async: false,
                        params,
                    }))
                    .into(),
                )]);
            }

            first = false;
        }

        expect!(self, ')');
        Ok(items)
    }

    /// `is_new_expr`: true iff we are parsing production 'NewExpression'.
    fn parse_member_expr_or_new_expr(&mut self, is_new_expr: bool) -> PResult<MaybeParen> {
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

                    let span = Span::new(span_of_new.lo, span_of_target.hi);
                    let expr = Box::new(Expr::MetaProp(MetaPropExpr {
                        node_id: node_id!(self, span),
                        meta: self.new_ident(js_word!("new"), span_of_new),
                        prop: self.new_ident(js_word!("target"), span_of_target),
                    }))
                    .into();

                    return self.parse_subscripts(MaybeParenExprOrSuper::Expr(expr), true);
                }

                unexpected!(self, "target")
            }

            let potential_arrow_start = self.potential_arrow_start;

            // 'NewExpression' allows new call without paren.
            let callee = self.parse_member_expr_or_new_expr(is_new_expr)?;
            return_if_arrow!(self, potential_arrow_start, callee);

            let callee = callee.unwrap();

            // Type arguments.
            if self.input.syntax().typescript() && is!(self, '<') {
                self.try_parse_ts(|p| {
                    p.parse_ts_type_args()?;
                    if !is!(p, '(') {
                        // This will fail
                        expect!(p, '(');
                    }
                    Ok(Some(()))
                });
            }

            if !is_new_expr || is!(self, '(') {
                // Parsed with 'MemberExpression' production.
                let args = self.parse_args(false).map(Some)?;

                let new_expr = MaybeParenExprOrSuper::Expr(
                    Box::new(Expr::New(NewExpr {
                        node_id: node_id!(self, span!(self, start)),
                        callee,
                        args,
                    }))
                    .into(),
                );

                // We should parse subscripts for MemberExpression.
                // Because it's left recursive.
                return self.parse_subscripts(new_expr, true);
            }

            // Parsed with 'NewExpression' production.

            return Ok(Box::new(Expr::New(NewExpr {
                node_id: node_id!(self, span!(self, start)),
                callee,
                args: None,
            }))
            .into());
        }

        if self.input.eat(&tok!("super")) {
            let base = MaybeParenExprOrSuper::Super(Super {
                node_id: node_id!(self, span!(self, start)),
            });
            return self.parse_subscripts(base, true);
        }

        let potential_arrow_start = self.potential_arrow_start;

        let obj = self.parse_primary_expr()?;
        return_if_arrow!(self, potential_arrow_start, obj);

        self.parse_subscripts(MaybeParenExprOrSuper::Expr(obj), true)
    }

    /// Parse `NewExpression`.
    /// This includes `MemberExpression`.
    fn parse_new_expr(&mut self) -> PResult<MaybeParen> {
        trace_cur!(self, parse_new_expr);

        self.parse_member_expr_or_new_expr(true)
    }

    /// Parse `Arguments[Yield, Await]`
    fn parse_args(&mut self, is_dynamic_import: bool) -> PResult<Vec<ExprOrSpread>> {
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

            expr_or_spreads.push(self.include_in_expr(true).parse_expr_or_spread()?.unwrap());
        }

        expect!(self, ')');
        Ok(expr_or_spreads)
    }

    /// AssignmentExpression[+In, ?Yield, ?Await]
    /// ...AssignmentExpression[+In, ?Yield, ?Await]
    fn parse_expr_or_spread(&mut self) -> PResult<MaybeParenExprOrSpread> {
        trace_cur!(self, parse_expr_or_spread);

        let start = self.input.cur_pos();

        if self.input.eat(&tok!("...")) {
            let expr = self.include_in_expr(true).parse_assignment_expr()?;
            let span = Span::new(start, self.input.prev_span().hi);
            Ok(MaybeParenExprOrSpread::Spread(MaybeParenSpreadElement {
                node_id: node_id!(self, span),
                expr,
            }))
        } else {
            self.parse_assignment_expr()
                .map(MaybeParenExprOrSpread::Expr)
        }
    }

    /// Parse paren expression or arrow function expression.
    fn parse_paren_expr_or_arrow_fn(
        &mut self,
        can_be_arrow: bool,
        async_span: Option<Span>,
    ) -> PResult<MaybeParen> {
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
            .any(|item| matches!(item, MaybeParenPatOrExprOrSpread::Pat(..)));

        // This is slow path. We handle arrow in conditional expression.
        if self.syntax().typescript() && self.ctx().in_cond_expr && is!(self, ':') {
            // TODO(swc): Remove clone
            let items_ref = &paren_items;
            if let Some(expr) = self.try_parse_ts(|p| {
                // Return type.
                p.parse_ts_type_or_type_predicate_ann(&tok!(':'))?;

                expect!(p, "=>");

                let exprs = items_ref.clone_node(program_data!(p));
                let params = p
                    .parse_paren_items_as_params(exprs)?
                    .into_iter()
                    .map(|pat| Param::from_pat(pat, program_data!(p)))
                    .collect();

                let body = p.parse_fn_body(async_span.is_some(), false)?;
                let body = p.make_arrow_fn_block(body);

                Ok(Some(Box::new(Expr::Arrow(ArrowExpr {
                    node_id: node_id!(p, span!(p, expr_start)),
                    is_async: async_span.is_some(),
                    params,
                    body,
                }))))
            }) {
                return Ok(expr.into());
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
                .map(|p| Param::from_pat(p, program_data!(self)))
                .collect();

            let body: BlockStmtOrExpr = self.parse_fn_body(async_span.is_some(), false)?;
            let is_block = matches!(body, BlockStmtOrExpr::BlockStmt(_));
            let body = self.make_arrow_fn_block(body);
            let arrow_expr = ArrowExpr {
                node_id: node_id!(self, span!(self, expr_start)),
                is_async: async_span.is_some(),
                params,
                body,
            };
            if is_block {
                if let Some(&Token::BinOp(..)) = self.input.cur() {
                    // ) is required
                    self.emit_err(self.input.cur_span(), SyntaxError::TS1005);
                    let errored_expr =
                        self.parse_bin_op_recursively(Box::new(Expr::Arrow(arrow_expr)).into(), 0)?;

                    if !is!(self, ';') {
                        // ; is required
                        self.emit_err(self.input.cur_span(), SyntaxError::TS1005);
                    }

                    return Ok(errored_expr);
                }
            }
            return Ok(Box::new(Expr::Arrow(arrow_expr)).into());
        }

        let expr_or_spreads = paren_items
            .into_iter()
            .map(|item| -> PResult<_> {
                match item {
                    MaybeParenPatOrExprOrSpread::Expr(e) => Ok(ExprOrSpread::Expr(e.unwrap())),
                    MaybeParenPatOrExprOrSpread::Spread(e) => Ok(ExprOrSpread::Spread(e.unwrap())),
                    MaybeParenPatOrExprOrSpread::Pat(p) => {
                        syntax_error!(self, get_span!(self, p.node_id()), SyntaxError::InvalidExpr)
                    }
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        if let Some(async_span) = async_span {
            // It's a call expression
            return Ok(Box::new(Expr::Call(CallExpr {
                node_id: node_id!(self, span!(self, async_span.lo())),
                callee: ExprOrSuper::Expr(Box::new(Expr::Ident(
                    self.new_ident("async".into(), async_span),
                ))),
                args: expr_or_spreads,
            }))
            .into());
        }

        // It was not head of arrow function.

        if expr_or_spreads.is_empty() {
            syntax_error!(
                self,
                Span::new(expr_start, self.input.last_pos(),),
                SyntaxError::EmptyParenExpr
            );
        }

        // TODO(swc): Verify that invalid expression like {a = 1} does not exists.

        // ParenthesizedExpression cannot contain spread.
        if expr_or_spreads.len() == 1 {
            let expr = match expr_or_spreads.into_iter().next().unwrap() {
                ExprOrSpread::Spread(s) => {
                    let span = get_span!(self, s.expr.node_id());
                    syntax_error!(self, span, SyntaxError::SpreadInParenExpr)
                }
                ExprOrSpread::Expr(e) => e,
            };
            self.parenthesised_exprs.insert(expr.node_id());
            Ok(MaybeParen::Wrapped(expr))
        } else {
            debug_assert!(expr_or_spreads.len() >= 2);

            let mut exprs = Vec::with_capacity(expr_or_spreads.len());
            for expr in expr_or_spreads {
                match expr {
                    ExprOrSpread::Spread(e) => {
                        let span = get_span!(self, e.node_id);
                        syntax_error!(self, span, SyntaxError::SpreadInParenExpr)
                    }
                    ExprOrSpread::Expr(e) => exprs.push(e),
                }
            }
            debug_assert!(exprs.len() >= 2);

            // span of sequence expression should not include '(', ')'
            let seq_expr_span = Span::new(
                get_span!(self, exprs.first().unwrap().node_id()).lo(),
                get_span!(self, exprs.last().unwrap().node_id()).hi(),
            );
            let seq_expr = Box::new(Expr::Seq(SeqExpr {
                node_id: node_id!(self, seq_expr_span),
                exprs,
            }));
            self.parenthesised_exprs.insert(seq_expr.node_id());
            Ok(MaybeParen::Wrapped(seq_expr))
        }
    }

    // Rewrite blockless arrow functions to have a block with a single return statement.
    // For example: `(x) => x` becomes `(x) => { return x; }`.
    // This simplifies optimizations as they can now assume all functions have a BLOCK.
    pub(super) fn make_arrow_fn_block(&mut self, block: BlockStmtOrExpr) -> BlockStmt {
        match block {
            BlockStmtOrExpr::BlockStmt(b) => b,
            BlockStmtOrExpr::Expr(expr) => {
                let expr_id = expr.node_id();
                BlockStmt {
                    node_id: node_id_from!(self, expr_id),
                    stmts: vec![Stmt::Return(ReturnStmt {
                        node_id: node_id_from!(self, expr_id),
                        arg: Some(expr),
                    })],
                }
            }
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
        let mut quasis = vec![cur_elem];

        while !is!(self, '`') {
            expect!(self, "${");
            exprs.push(self.include_in_expr(true).parse_expr()?.unwrap());
            expect!(self, '}');
            let elem = self.parse_tpl_element(is_tagged)?;
            quasis.push(elem);
        }

        Ok((exprs, quasis))
    }

    fn parse_tagged_tpl(&mut self, tag: Box<Expr>) -> PResult<TaggedTpl> {
        trace_cur!(self, parse_tagged_tpl);

        let tagged_tpl_start = get_span!(self, tag.node_id()).lo();

        let tpl = self.parse_tpl(true)?;

        let span = span!(self, tagged_tpl_start);
        Ok(TaggedTpl {
            node_id: node_id!(self, span),
            tag,
            tpl,
        })
    }

    fn parse_tpl(&mut self, is_tagged: bool) -> PResult<Tpl> {
        trace_cur!(self, parse_tpl);

        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!('`'));

        let (exprs, quasis) = self.parse_tpl_elements(is_tagged)?;

        expect!(self, '`');

        Ok(Tpl {
            node_id: node_id!(self, span!(self, start)),
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
                        node_id: node_id!(self, span!(self, start)),
                        value: raw,
                        has_escape,
                    },
                    cooked.map(|cooked| Str {
                        node_id: node_id!(self, span!(self, start)),
                        value: cooked,
                        has_escape,
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

        Ok(TplElement {
            node_id: node_id!(self, span!(self, start)),
            raw,
            cooked,
        })
    }
}

/// Checks if the given span contains an escape sequence by comparing it's
/// length against that of the passed reference word.
fn word_contains_escape(span: &Span, word: &'static str) -> bool {
    span.hi.to_usize() - span.lo.to_usize() != word.len()
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
                node_id: node_id!(self, span!(self, start)),
                arg: None,
                delegate: false,
            })))
        } else {
            let has_star = self.input.eat(&tok!('*'));
            let arg = self.parse_assignment_expr()?.unwrap();

            Ok(Box::new(Expr::Yield(YieldExpr {
                node_id: node_id!(self, span!(self, start)),
                arg: Some(arg),
                delegate: has_star,
            })))
        }
    }

    fn at_possible_async(&mut self, expr: &MaybeParen) -> bool {
        // The following todo is to match the behaviour of babel (and probably the spec).
        // We can use the length of the identifier to check the condition below.
        // If the length is 5 (the len of the word "async"), the ident does not
        // contain any escapes.

        // TODO(swc): !this.state.containsEsc &&

        if let MaybeParen::Expr(expr) = &expr {
            match expr.as_ref() {
                Expr::Ident(Ident {
                    sym: js_word!("async"),
                    ..
                }) => {
                    let span = get_span!(self, expr.node_id());
                    self.potential_arrow_start == Some(span.lo())
                }
                _ => false,
            }
        } else {
            false
        }
    }

    /// 12.2.5 Array Initializer
    pub(super) fn parse_lit(&mut self) -> PResult<Lit> {
        let start = self.input.cur_pos();

        let v = match *cur!(self, true)? {
            Word(Word::Null) => {
                self.input.bump();
                Lit::Null(Null {
                    node_id: node_id!(self, span!(self, start)),
                })
            }
            Word(Word::True) | Word(Word::False) => {
                let value = self.input.is(&tok!("true"));
                self.input.bump();

                Lit::Bool(Bool {
                    node_id: node_id!(self, span!(self, start)),
                    value,
                })
            }
            Token::Str { .. } => match self.input.bump() {
                Token::Str { value, has_escape } => Lit::Str(Str {
                    node_id: node_id!(self, span!(self, start)),
                    value,
                    has_escape,
                }),
                _ => unreachable!(),
            },
            Token::Num { .. } => match self.input.bump() {
                Token::Num(value) => Lit::Num(Number {
                    node_id: node_id!(self, span!(self, start)),
                    value,
                }),
                _ => unreachable!(),
            },
            Token::BigInt(..) => match self.input.bump() {
                Token::BigInt(value) => Lit::BigInt(BigInt {
                    node_id: node_id!(self, span!(self, start)),
                    value,
                }),
                _ => unreachable!(),
            },
            _ => unreachable!("parse_lit should not be called"),
        };
        Ok(v)
    }

    fn parse_dynamic_import(&mut self, start: BytePos, import_ident: Ident) -> PResult<MaybeParen> {
        if !self.input.syntax().dynamic_import() {
            syntax_error!(self, span!(self, start), SyntaxError::DynamicImport);
        }

        let args = self.parse_args(true)?;
        let import = Box::new(Expr::Call(CallExpr {
            node_id: node_id!(self, span!(self, start)),
            callee: ExprOrSuper::Expr(Box::new(Expr::Ident(import_ident))),
            args,
        }))
        .into();

        self.parse_subscripts(MaybeParenExprOrSuper::Expr(import), true)
    }

    pub(super) fn check_assign_target(&mut self, expr: &Expr, deny_call: bool) {
        // We follow behaviour of tsc
        if self.input.syntax().typescript() && self.syntax().early_errors() {
            let is_eval_or_arguments = match expr {
                Expr::Ident(i) => i.sym == js_word!("eval") || i.sym == js_word!("arguments"),
                _ => false,
            };

            if is_eval_or_arguments {
                self.emit_strict_mode_err(get_span!(self, expr.node_id()), SyntaxError::TS1100);
            }

            fn should_deny(
                e: &Expr,
                deny_call: bool,
                parenthesised_exprs: &FxHashSet<NodeId>,
            ) -> bool {
                match e {
                    _ if parenthesised_exprs.contains(&e.node_id()) => true,
                    Expr::Lit(..) => false,
                    Expr::Call(..) => deny_call,
                    Expr::Bin(..) => false,

                    _ => true,
                }
            }

            // It is an early Reference Error if LeftHandSideExpression is neither
            // an ObjectLiteral nor an ArrayLiteral and
            // IsValidSimpleAssignmentTarget of LeftHandSideExpression is false.
            if !is_eval_or_arguments
                && !is_valid_simple_assignment_target(expr, self.ctx().strict)
                && should_deny(expr, deny_call, &self.parenthesised_exprs)
            {
                self.emit_err(get_span!(self, expr.node_id()), SyntaxError::TS2406);
            }
        } else if !is_valid_simple_assignment_target(expr, self.ctx().strict) {
            self.emit_err(get_span!(self, expr.node_id()), SyntaxError::TS2406);
        }
    }
}

fn is_import(obj: &MaybeParenExprOrSuper) -> bool {
    match obj {
        MaybeParenExprOrSuper::Expr(expr) => match expr {
            MaybeParen::Expr(expr) => matches!(
                **expr,
                Expr::Ident(Ident {
                    sym: js_word!("import"),
                    ..
                })
            ),
            MaybeParen::Wrapped(_) => false,
        },
        _ => false,
    }
}

pub(super) enum BlockStmtOrExpr {
    BlockStmt(BlockStmt),
    Expr(Box<Expr>),
}

pub(super) enum MaybeParen {
    Expr(Box<Expr>),
    Wrapped(Box<Expr>),
}

impl MaybeParen {
    pub fn unwrap(self) -> Box<Expr> {
        match self {
            MaybeParen::Expr(expr) => expr,
            MaybeParen::Wrapped(expr) => expr,
        }
    }

    pub fn inner(&self) -> &Expr {
        match self {
            MaybeParen::Expr(expr) => expr,
            MaybeParen::Wrapped(expr) => expr,
        }
    }
}

impl From<Box<Expr>> for MaybeParen {
    fn from(value: Box<Expr>) -> Self {
        MaybeParen::Expr(value)
    }
}

impl GetNodeId for MaybeParen {
    fn node_id(&self) -> NodeId {
        match self {
            MaybeParen::Expr(expr) => expr.node_id(),
            MaybeParen::Wrapped(expr) => expr.node_id(),
        }
    }
}

impl CloneNode for MaybeParen {
    fn clone_node(&self, program_data: &mut ProgramData) -> Self {
        match self {
            MaybeParen::Expr(n) => MaybeParen::Expr(n.clone_node(program_data)),
            MaybeParen::Wrapped(n) => MaybeParen::Wrapped(n.clone_node(program_data)),
        }
    }
}

enum MaybeParenExprOrSuper {
    Expr(MaybeParen),
    Super(Super),
}

impl MaybeParenExprOrSuper {
    fn unwrap(self) -> ExprOrSuper {
        match self {
            MaybeParenExprOrSuper::Expr(n) => ExprOrSuper::Expr(n.unwrap()),
            MaybeParenExprOrSuper::Super(n) => ExprOrSuper::Super(n),
        }
    }
}

impl GetNodeId for MaybeParenExprOrSuper {
    fn node_id(&self) -> NodeId {
        match self {
            MaybeParenExprOrSuper::Expr(n) => n.node_id(),
            MaybeParenExprOrSuper::Super(n) => n.node_id(),
        }
    }
}

impl CloneNode for MaybeParenExprOrSuper {
    fn clone_node(&self, program_data: &mut ProgramData) -> Self {
        match self {
            MaybeParenExprOrSuper::Expr(n) => {
                MaybeParenExprOrSuper::Expr(n.clone_node(program_data))
            }
            MaybeParenExprOrSuper::Super(n) => {
                MaybeParenExprOrSuper::Super(n.clone_node(program_data))
            }
        }
    }
}

pub(super) enum MaybeParenPatOrExprOrSpread {
    Pat(Pat),
    Expr(MaybeParen),
    Spread(MaybeParenSpreadElement),
}

impl CloneNode for MaybeParenPatOrExprOrSpread {
    fn clone_node(&self, program_data: &mut ProgramData) -> Self {
        match self {
            MaybeParenPatOrExprOrSpread::Pat(n) => Self::Pat(n.clone_node(program_data)),
            MaybeParenPatOrExprOrSpread::Expr(n) => Self::Expr(n.clone_node(program_data)),
            MaybeParenPatOrExprOrSpread::Spread(n) => Self::Spread(n.clone_node(program_data)),
        }
    }
}

enum MaybeParenExprOrSpread {
    Expr(MaybeParen),
    Spread(MaybeParenSpreadElement),
}

impl MaybeParenExprOrSpread {
    fn unwrap(self) -> ExprOrSpread {
        match self {
            MaybeParenExprOrSpread::Expr(n) => ExprOrSpread::Expr(n.unwrap()),
            MaybeParenExprOrSpread::Spread(n) => ExprOrSpread::Spread(n.unwrap()),
        }
    }
}

impl GetNodeId for MaybeParenExprOrSpread {
    fn node_id(&self) -> NodeId {
        match self {
            MaybeParenExprOrSpread::Expr(n) => n.node_id(),
            MaybeParenExprOrSpread::Spread(n) => n.expr.node_id(),
        }
    }
}

pub(super) struct MaybeParenSpreadElement {
    pub node_id: NodeId,
    pub expr: MaybeParen,
}

impl MaybeParenSpreadElement {
    fn unwrap(self) -> SpreadElement {
        SpreadElement {
            node_id: self.node_id,
            expr: self.expr.unwrap(),
        }
    }
}

impl CloneNode for MaybeParenSpreadElement {
    fn clone_node(&self, program_data: &mut ProgramData) -> Self {
        Self {
            node_id: program_data.new_id_from(self.node_id),
            expr: self.expr.clone_node(program_data),
        }
    }
}
