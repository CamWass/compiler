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
    pub(super) fn parse_expr(&mut self) -> PResult<Box<Expr>> {
        let start = self.input.cur_pos();
        let expr = self.parse_assignment_expr()?;

        if self.input.is(&tok!(',')) {
            let mut exprs = vec![expr];

            while self.input.eat(&tok!(',')) {
                exprs.push(self.parse_assignment_expr()?);
            }

            return Ok(Box::new(Expr::Seq(SeqExpr {
                span: span!(self, start),
                exprs,
            })));
        }

        Ok(expr)
    }

    ///`parseMaybeAssign` (overridden)
    pub(super) fn parse_assignment_expr(&mut self) -> PResult<Box<Expr>> {
        self.parse_assignment_expr_base()
    }

    /// Parse an assignment expression. This includes applications of
    /// operators like `+=`.
    ///
    /// `parseMaybeAssign`
    fn parse_assignment_expr_base(&mut self) -> PResult<Box<Expr>> {
        if self.ctx().in_generator && is!(self, "yield") {
            return self.parse_yield_expr();
        }

        let start = self.input.cur_pos();

        self.state.potential_arrow_start = match *cur!(self, true)? {
            Word(Word::Ident(..)) | tok!('(') | tok!("yield") => Some(start),
            _ => None,
        };

        // Try to parse conditional expression.
        let cond = self.parse_cond_expr()?;

        return_if_arrow!(self, cond);

        match *cond {
            // if cond is conditional expression but not left-hand-side expression,
            // just return it.
            Expr::Cond(..) | Expr::Bin(..) | Expr::Unary(..) | Expr::Update(..) => return Ok(cond),
            _ => {}
        }

        self.finish_assignment_expr(start, cond)
    }

    fn finish_assignment_expr(&mut self, start: BytePos, cond: Box<Expr>) -> PResult<Box<Expr>> {
        match cur!(self, false) {
            Ok(&Token::AssignOp(op)) => {
                let left = if op == AssignOpToken::Assign {
                    self.reparse_expr_as_pat(PatType::AssignPat, cond)
                        .map(Box::new)
                        .map(PatOrExpr::Pat)?
                } else {
                    //It is an early Reference Error if IsValidSimpleAssignmentTarget of
                    // LeftHandSideExpression is false.
                    if !cond.is_valid_simple_assignment_target(self.ctx().strict) {
                        self.emit_err(cond.span(), SyntaxError::NotSimpleAssign)
                    }

                    // TODO(swc):
                    PatOrExpr::Expr(cond)
                };

                self.input.bump();
                let right = self.parse_assignment_expr()?;
                Ok(Box::new(Expr::Assign(AssignExpr {
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
        let start = self.input.cur_pos();

        let test = self.parse_bin_expr()?;
        return_if_arrow!(self, test);

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
                    return Ok(Box::new(Expr::This(ThisExpr {
                        span: span!(self, start),
                    })));
                }

                tok!("import") => {
                    let import = self.parse_ident_name()?;
                    if is!(self, '.') {
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
                    return Ok(Box::new(Expr::Lit(self.parse_lit()?)));
                }

                // Regexp
                Token::Regex(..) => match self.input.bump() {
                    Token::Regex(exp, flags) => {
                        return Ok(Box::new(Expr::Lit(Lit::Regex(Regex {
                            span: span!(self, start),
                            exp,
                            flags,
                        }))));
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

        let decorators = self.parse_decorators(false)?;

        if is!(self, "class") {
            return self.parse_class_expr(start, decorators);
        }

        if is!(self, "let")
            /*|| (self.input.syntax().typescript() && is!(self, IdentName))*/
            || is!(self, IdentRef)
        {
            // TODO: Handle [Yield, Await]
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
                let params = vec![arg];
                expect!(self, "=>");
                let body = self.parse_fn_body(true, false)?;

                return Ok(Box::new(Expr::Arrow(ArrowExpr {
                    span: span!(self, start),
                    body,
                    params,
                    is_async: true,
                    is_generator: false,
                    return_type: None,
                    type_params: None,
                })));
            } else if can_be_arrow
                && !self.input.had_line_break_before_cur()
                && self.input.eat(&tok!("=>"))
            {
                let params = vec![id.into()];
                let body = self.parse_fn_body(false, false)?;

                return Ok(Box::new(Expr::Arrow(ArrowExpr {
                    span: span!(self, start),
                    body,
                    params,
                    is_async: false,
                    is_generator: false,
                    // TODO
                    return_type: None,
                    // TODO
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
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!('['));
        let mut elems = vec![];

        while !eof!(self) && !is!(self, ']') {
            if is!(self, ',') {
                expect!(self, ',');
                elems.push(None);
                continue;
            }
            elems.push(
                self.include_in_expr(true)
                    .parse_expr_or_spread()
                    .map(Some)?,
            );
            if is!(self, ',') {
                expect!(self, ',');
            }
        }

        expect!(self, ']');

        let span = span!(self, start);
        Ok(Box::new(Expr::Array(ArrayLit { span, elems })))
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

        Ok(MetaPropExpr { meta, prop })
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
            let prop = self.include_in_expr(true).parse_expr()?;
            expect!(self, ']');
            let span = Span::new(obj.span().lo(), self.input.last_pos(), Default::default());
            debug_assert_eq!(obj.span().lo(), span.lo());

            return Ok((
                Box::new(wrap!(Expr::Member(MemberExpr {
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
                    todo!();
                    // let tpl = self.parse_tagged_tpl(expr, None);
                    // return (Box::new(Expr::TaggedTpl(tpl)), true);
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

        // `super()` can't be handled from parse_new_expr()
        if self.input.eat(&tok!("super")) {
            let obj = ExprOrSuper::Super(Super {
                span: span!(self, start),
            });
            return self.parse_subscripts(obj, false);
        }

        let callee = self.parse_new_expr()?;
        return_if_arrow!(self, callee);

        let type_args = None;

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
        expect!(self, '(');

        let mut first = true;
        let mut items = vec![];

        // TODO(kdy1): optimize (once we parsed a pattern, we can parse everything else
        // as a pattern instead of reparsing)
        while !eof!(self) && !is!(self, ')') {
            if first {
                if is!(self, "async") {
                    // https://github.com/swc-project/swc/issues/410
                    self.state.potential_arrow_start = Some(self.input.cur_pos());
                    let expr = self.parse_assignment_expr()?;
                    expect!(self, ')');
                    return Ok(vec![PatOrExprOrSpread::ExprOrSpread(ExprOrSpread {
                        expr,
                        spread: None,
                    })]);
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

            let has_modifier = false;

            let arg = self.include_in_expr(true).parse_expr_or_spread()?;

            if has_modifier {
                self.emit_err(span!(self, modifier_start), SyntaxError::TS2369);
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
                    .parse_paren_items_as_params(items)?
                    .into_iter()
                    .collect();

                let body: BlockStmtOrExpr = self.parse_fn_body(false, false)?;
                expect!(self, ')');
                let span = span!(self, start);

                return Ok(vec![PatOrExprOrSpread::ExprOrSpread(ExprOrSpread {
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
                })]);
            }

            first = false;
        }

        expect!(self, ')');
        Ok(items)
    }

    /// `is_new_expr`: true iff we are parsing production 'NewExpression'.
    fn parse_member_expr_or_new_expr(&mut self, is_new_expr: bool) -> PResult<Box<Expr>> {
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
            let callee = self.parse_member_expr_or_new_expr(is_new_expr)?;
            return_if_arrow!(self, callee);

            let type_args = None;

            if !is_new_expr || is!(self, '(') {
                // Parsed with 'MemberExpression' production.
                let args = self.parse_args(false).map(Some)?;

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

            return Ok(Box::new(Expr::New(NewExpr {
                span: span!(self, start),
                callee,
                args: None,
                type_args,
            })));
        }

        if self.input.eat(&tok!("super")) {
            let base = ExprOrSuper::Super(Super {
                span: span!(self, start),
            });
            return self.parse_subscripts(base, true);
        }
        let obj = self.parse_primary_expr()?;
        return_if_arrow!(self, obj);

        self.parse_subscripts(ExprOrSuper::Expr(obj), true)
    }

    /// Parse `NewExpression`.
    /// This includes `MemberExpression`.
    pub(super) fn parse_new_expr(&mut self) -> PResult<Box<Expr>> {
        self.parse_member_expr_or_new_expr(true)
    }

    /// Parse `Arguments[Yield, Await]`
    pub(super) fn parse_args(&mut self, is_dynamic_import: bool) -> PResult<Vec<ExprOrSpread>> {
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
        let start = self.input.cur_pos();

        if self.input.eat(&tok!("...")) {
            let spread = Some(span!(self, start));
            self.include_in_expr(true)
                .parse_assignment_expr()
                .map(|expr| ExprOrSpread { spread, expr })
        } else {
            self.parse_assignment_expr()
                .map(|expr| ExprOrSpread { spread: None, expr })
        }
    }

    /// Parse paren expression or arrow function expression.
    fn parse_paren_expr_or_arrow_fn(
        &mut self,
        can_be_arrow: bool,
        async_span: Option<Span>,
    ) -> PResult<Box<Expr>> {
        let expr_start = async_span.map(|x| x.lo()).unwrap_or(self.input.cur_pos());

        // At this point, we can't know if it's parenthesized
        // expression or head of arrow function.
        // But as all patterns of javascript is subset of
        // expressions, we can parse both as expression.

        let paren_items = self.include_in_expr(true).parse_args_or_pats()?;
        let has_pattern = paren_items.iter().any(|item| match item {
            PatOrExprOrSpread::Pat(..) => true,
            _ => false,
        });

        let return_type = None;

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
                .collect();

            let body: BlockStmtOrExpr = self.parse_fn_body(async_span.is_some(), false)?;
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
                        self.emit_err(self.input.cur_span(), SyntaxError::TS1005);
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
                span: span!(self, async_span.lo()),
                callee: ExprOrSuper::Expr(Box::new(Expr::Ident(Ident::new(
                    "async".into(),
                    async_span,
                )))),
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
                ExprOrSpread {
                    spread: Some(..),
                    ref expr,
                } => {
                    syntax_error!(self, expr.span(), SyntaxError::SpreadInParenExpr)
                }
                ExprOrSpread { expr, .. } => expr,
            };
            Ok(Box::new(Expr::Paren(ParenExpr {
                span: span!(self, expr_start),
                expr,
            })))
        } else {
            debug_assert!(expr_or_spreads.len() >= 2);

            let mut exprs = Vec::with_capacity(expr_or_spreads.len());
            for expr in expr_or_spreads {
                match expr {
                    ExprOrSpread {
                        spread: Some(..),
                        ref expr,
                    } => syntax_error!(self, expr.span(), SyntaxError::SpreadInParenExpr),
                    ExprOrSpread { expr, .. } => exprs.push(expr),
                }
            }
            debug_assert!(exprs.len() >= 2);

            // span of sequence expression should not include '(', ')'
            let seq_expr = Box::new(Expr::Seq(SeqExpr {
                span: Span::new(
                    exprs.first().unwrap().span().lo(),
                    exprs.last().unwrap().span().hi(),
                    Default::default(),
                ),
                exprs,
            }));
            Ok(Box::new(Expr::Paren(ParenExpr {
                span: span!(self, expr_start),
                expr: seq_expr,
            })))
        }
    }

    // TODO: replace this with whatever babel uses:
    pub(super) fn verify_expr(&mut self, expr: Box<Expr>) -> PResult<Box<Expr>> {
        // todo!();
        Ok(expr)
    }
}

#[ast_node]
pub(in crate::parser) enum PatOrExprOrSpread {
    #[tag("*")]
    Pat(Pat),
    #[tag("*")]
    ExprOrSpread(ExprOrSpread),
}

/// simple leaf methods.

impl<'a, I: Tokens> Parser<I> {
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
                span: span!(self, start),
                arg: None,
                delegate: false,
            })))
        } else {
            let has_star = self.input.eat(&tok!('*'));
            let arg = self.parse_assignment_expr()?;

            Ok(Box::new(Expr::Yield(YieldExpr {
                span: span!(self, start),
                arg: Some(arg),
                delegate: has_star,
            })))
        }
    }

    /// 12.2.5 Array Initializer
    pub(super) fn parse_lit(&mut self) -> PResult<Lit> {
        let start = self.input.cur_pos();

        let v = match *cur!(self, true)? {
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
        Ok(v)
    }

    pub(super) fn parse_dynamic_import(
        &mut self,
        start: BytePos,
        import_ident: Ident,
    ) -> PResult<Box<Expr>> {
        let args = self.parse_args(true)?;
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
            self.emit_err(expr.span(), SyntaxError::TS2406);
        }
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
