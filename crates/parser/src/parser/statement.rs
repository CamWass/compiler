use super::{pat::PatType, *};
use crate::{
    context::{Context, ContextFlags, YesMaybe},
    token::{Token, Word},
};
use atoms::js_word;
use expression::MaybeParen;
use global_common::{BytePos, Span};
use statement::typescript::DeclOrEmpty;
use util::AssignProps;

mod module_item;

enum ForHead {
    For {
        init: Option<VarDeclOrExpr>,
        test: Option<Box<Expr>>,
        update: Option<Box<Expr>>,
    },
    ForIn {
        left: VarDeclOrPat,
        right: Box<Expr>,
    },
    ForOf {
        left: VarDeclOrPat,
        right: Box<Expr>,
    },
}

pub(super) trait IsDirective {
    fn as_ref(&self) -> Option<&Stmt>;
    fn is_use_strict(&self) -> bool {
        match self.as_ref() {
            Some(Stmt::Expr(expr)) => match expr.expr.as_ref() {
                Expr::Lit(Lit::Str(Str {
                    value,
                    has_escape: false,
                    ..
                })) => value == "use strict",
                _ => false,
            },
            _ => false,
        }
    }
    fn is_valid_directive(&self) -> bool {
        match self.as_ref() {
            Some(Stmt::Expr(expr)) => matches!(*expr.expr, Expr::Lit(Lit::Str(Str { .. }))),
            _ => false,
        }
    }
}

impl IsDirective for Stmt {
    fn as_ref(&self) -> Option<&Stmt> {
        Some(self)
    }
}

pub(super) trait StmtLikeParser<Type: IsDirective> {
    fn handle_import_export(&mut self, top_level: bool) -> PResult<Option<Type>>;
}

#[derive(PartialEq, Eq)]
pub enum StmtParseCtx {
    IfOrLabel,
    Other,
    None,
}

impl<I: Tokens> StmtLikeParser<Stmt> for Parser<'_, I> {
    fn handle_import_export(&mut self, _: bool) -> PResult<Option<Stmt>> {
        let start = self.input.cur_pos();
        if self.input.syntax().dynamic_import() && is!(self, "import") {
            let expr = self.parse_expr(&mut AssignProps::Emit)?.unwrap();

            eat!(self, ';');

            return Ok(Some(Stmt::Expr(ExprStmt {
                node_id: node_id!(self, span!(self, start)),
                expr,
            })));
        }

        if self.input.syntax().import_meta()
            && is!(self, "import")
            && self.input.peeked_is(&tok!('.'))
        {
            let expr = self.parse_expr(&mut AssignProps::Emit)?.unwrap();

            eat!(self, ';');

            return Ok(Some(Stmt::Expr(ExprStmt {
                node_id: node_id!(self, span!(self, start)),
                expr,
            })));
        }

        syntax_error!(self, SyntaxError::ImportExportInScript);
    }
}

impl<I: Tokens> Parser<'_, I> {
    pub(super) fn parse_block_body<Type>(
        &mut self,
        allow_directives: bool,
        top_level: bool,
        end: Option<&Token>,
    ) -> PResult<Vec<Type>>
    where
        Self: StmtLikeParser<Type>,
        Type: IsDirective + From<Stmt>,
    {
        trace_cur!(self, parse_block_body);

        let old_ctx = self.ctx();

        let mut parsed_non_directive = false;
        let mut has_strict_mode_directive = false;

        let mut stmts = vec![];
        while {
            let c = self.input.cur();
            c != end
        } {
            let stmt = match self.parse_stmt_like(StmtParseCtx::None, top_level)? {
                Some(s) => s,
                None => {
                    parsed_non_directive = true;
                    continue;
                }
            };

            if allow_directives && !parsed_non_directive {
                if stmt.is_valid_directive() {
                    if !has_strict_mode_directive && stmt.is_use_strict() {
                        has_strict_mode_directive = true;

                        let ctx = Context {
                            strict: YesMaybe::Yes,
                            ..old_ctx
                        };
                        self.set_ctx(ctx);
                    }
                } else {
                    parsed_non_directive = true;
                }
            }

            stmts.push(stmt);
        }

        if end.is_some() {
            self.input.bump();
        }

        if !top_level {
            self.set_ctx(old_ctx);
        }

        Ok(stmts)
    }

    fn parse_stmt(&mut self, parse_ctx: StmtParseCtx, top_level: bool) -> PResult<Stmt> {
        trace_cur!(self, parse_stmt);
        debug_assert!(parse_ctx != StmtParseCtx::None);
        // None is only encountered for typescript things that are stripped.
        // These should have been handled by parse_stmt_like returning Err or
        // transpiled code.
        Ok(self.parse_stmt_like(parse_ctx, top_level)?.unwrap())
    }

    fn parse_stmt_list_item(&mut self, top_level: bool) -> PResult<Option<Stmt>> {
        trace_cur!(self, parse_stmt_list_item);
        self.parse_stmt_like(StmtParseCtx::None, top_level)
    }

    /// Parse a statement, declaration or module item.
    fn parse_stmt_like<Type>(
        &mut self,
        parse_ctx: StmtParseCtx,
        top_level: bool,
    ) -> PResult<Option<Type>>
    where
        Self: StmtLikeParser<Type>,
        Type: IsDirective + From<Stmt>,
    {
        trace_cur!(self, parse_stmt_like);
        let start = self.input.cur_pos();

        if is_one_of!(self, "import", "export") {
            return self.handle_import_export(top_level);
        }

        self.parse_stmt_content(start, parse_ctx, top_level)
            .map(|s| s.map(From::from))
    }
    fn parse_stmt_content(
        &mut self,
        start: BytePos,
        parse_ctx: StmtParseCtx,
        top_level: bool,
    ) -> PResult<Option<Stmt>> {
        trace_cur!(self, parse_stmt_content);

        // Most types of statements are recognized by the keyword they
        // start with. Many are trivial to parse, some require a bit of
        // complexity.

        if top_level && is!(self, "await") {
            let valid = self.target() >= JscTarget::Es2017 && self.syntax().top_level_await();

            if !valid {
                self.emit_err(self.input.cur_span(), SyntaxError::TopLevelAwait);
            }

            let expr = self.parse_await_expr()?;
            eat!(self, ';');

            let span = span!(self, start);
            return Ok(Some(Stmt::Expr(ExprStmt {
                node_id: node_id!(self, span),
                expr,
            })));
        }

        if self.input.syntax().typescript() && is!(self, "const") && peeked_is!(self, "enum") {
            self.assert_and_bump(&tok!("const"));
            self.assert_and_bump(&tok!("enum"));
            self.parse_ts_enum_decl()?;
            return Ok(Some(Stmt::Empty(EmptyStmt {
                node_id: node_id!(self, span!(self, start)),
            })));
        }

        match cur!(self, true)? {
            tok!("break") | tok!("continue") => {
                let is_break = self.input.is(&tok!("break"));
                self.input.bump();

                let label = if eat!(self, ';') {
                    None
                } else {
                    let ident = self.parse_label_ident().map(Some)?;
                    expect!(self, ';');
                    ident
                };

                let span = span!(self, start);

                self.verify_break_continue(is_break, &label, span);

                if is_break {
                    return Ok(Some(Stmt::Break(BreakStmt {
                        node_id: node_id!(self, span),
                        label,
                    })));
                } else {
                    return Ok(Some(Stmt::Continue(ContinueStmt {
                        node_id: node_id!(self, span),
                        label,
                    })));
                }
            }
            tok!("debugger") => {
                return self.parse_debugger_stmt(start).map(Some);
            }
            tok!("do") => {
                return self.parse_do_stmt().map(Some);
            }
            tok!("for") => {
                return self.parse_for_stmt().map(Some);
            }
            tok!("function") => {
                if parse_ctx != StmtParseCtx::None {
                    if self.ctx().is_strict() {
                        syntax_error!(self, SyntaxError::StrictFunction);
                    } else if parse_ctx == StmtParseCtx::Other {
                        syntax_error!(self, SyntaxError::SloppyFunction);
                    }
                    // TODO: disallow generator functions in single statement contexts.
                    return self.parse_fn_decl().map(Stmt::Decl).map(Some);
                } else {
                    return self
                        .parse_fn_decl_or_ts_overload_sig()
                        .map(|s| s.map(Stmt::Decl));
                }
            }
            tok!("class") => {
                if parse_ctx != StmtParseCtx::None {
                    syntax_error!(self, SyntaxError::UnexpectedClassInSingleStatementCtx);
                }
                return self
                    .parse_class_decl(start, start)
                    .map(Stmt::Decl)
                    .map(Some);
            }
            tok!("if") => {
                return self.parse_if_stmt().map(Some);
            }
            tok!("return") => {
                return self.parse_return_stmt().map(Some);
            }
            tok!("switch") => {
                return self.parse_switch_stmt().map(Some);
            }
            tok!("throw") => {
                return self.parse_throw_stmt().map(Some);
            }
            // Error recovery
            tok!("catch") => {
                let span = self.input.cur_span();
                self.emit_err(span, SyntaxError::TS1005);

                let _ = self.parse_catch_clause();
                let _ = self.parse_finally_block();

                return Ok(Some(Stmt::Expr(ExprStmt {
                    node_id: node_id!(self, span),
                    expr: Box::new(Expr::Invalid(Invalid {
                        node_id: node_id!(self, span),
                    })),
                })));
            }
            // Error recovery
            tok!("finally") => {
                let span = self.input.cur_span();
                self.emit_err(span, SyntaxError::TS1005);

                let _ = self.parse_finally_block();

                return Ok(Some(Stmt::Expr(ExprStmt {
                    node_id: node_id!(self, span),
                    expr: Box::new(Expr::Invalid(Invalid {
                        node_id: node_id!(self, span),
                    })),
                })));
            }
            tok!("try") => {
                return self.parse_try_stmt().map(Some);
            }
            tok!("var") => {
                let v = self.parse_var_stmt(false)?;
                return Ok(Some(Stmt::Decl(Decl::Var(v))));
            }
            tok!("const") => {
                if parse_ctx != StmtParseCtx::None {
                    syntax_error!(self, SyntaxError::UnexpectedLexicalDeclaration);
                }

                let v = self.parse_var_stmt(false)?;
                return Ok(Some(Stmt::Decl(Decl::Var(v))));
            }
            // 'let' can start an identifier reference.
            tok!("let") => {
                if parse_ctx != StmtParseCtx::None {
                    syntax_error!(self, SyntaxError::UnexpectedLexicalDeclaration);
                }

                let is_keyword = match self.input.peek() {
                    Some(t) => t.follows_keyword_let(),
                    _ => false,
                };

                if is_keyword {
                    let v = self.parse_var_stmt(false)?;
                    return Ok(Some(Stmt::Decl(Decl::Var(v))));
                }
            }
            tok!("while") => {
                return self.parse_while_stmt().map(Some);
            }
            tok!("with") => {
                return self.parse_with_stmt().map(Some);
            }
            tok!('{') => {
                return self.parse_block(false).map(Stmt::Block).map(Some);
            }
            tok!(';') => {
                self.input.bump();
                return Ok(Some(Stmt::Empty(EmptyStmt {
                    node_id: node_id!(self, span!(self, start)),
                })));
            }

            _ => {}
        }

        // Handle async function foo() {}
        if self.input.is(&tok!("async"))
            && self.input.peeked_is(&tok!("function"))
            && !self.input.has_linebreak_between_cur_and_peeked()
        {
            // TODO: what's this?
            // if context.is_some() {
            //     // self.raise(
            //     //     self.lexer.state.start,
            //     //     Errors.AsyncFunctionInSingleStatementContext,
            //     // );
            //     panic!("AsyncFunctionInSingleStatementContext at {:?}", start);
            // }
            // self.input.bump();
            // return self.parseFunctionStatement(node, true, context.is_none());

            if parse_ctx != StmtParseCtx::None {
                todo!("async fn not allowed in single stmt context");
            } else {
                return self.parse_async_fn_decl().map(|s| s.map(Stmt::Decl));
            }
        }

        // If the statement does not start with a statement keyword or a
        // brace, it's an ExpressionStatement or LabeledStatement. We
        // simply start parsing an expression, and afterwards, if the
        // next token is a colon and the expression was a simple
        // Identifier node, we switch to interpreting it as a label.
        let expr = self
            .include_in_expr(true)
            .parse_expr(&mut AssignProps::Emit)?;

        if let MaybeParen::Expr(expr_ref) = &expr {
            if let Expr::Ident(_) = expr_ref.as_ref() {
                if self.input.eat(&tok!(':')) {
                    let ident = match *expr.unwrap() {
                        Expr::Ident(ident) => ident,
                        _ => unreachable!(),
                    };
                    return self.parse_labelled_stmt(ident).map(Some);
                }
            }
        }

        if let MaybeParen::Expr(expr_ref) = &expr {
            if let Expr::Ident(ident) = expr_ref.as_ref() {
                if *ident.sym == js_word!("interface") && self.input.had_line_break_before_cur() {
                    self.emit_strict_mode_err(
                        get_span!(self, ident.node_id),
                        SyntaxError::InvalidIdentInStrict,
                    );

                    eat!(self, ';');

                    return Ok(Some(Stmt::Expr(ExprStmt {
                        node_id: node_id!(self, span!(self, start)),
                        expr: expr.unwrap(),
                    })));
                }

                if self.input.syntax().typescript() {
                    if let Some(decl) = self.parse_ts_expr_stmt(ident)? {
                        return match decl {
                            DeclOrEmpty::Decl(d) => Ok(Some(Stmt::Decl(d))),
                            DeclOrEmpty::Empty => Ok(None),
                        };
                    }
                }

                match ident.sym {
                    js_word!("enum") | js_word!("interface") => {
                        self.emit_strict_mode_err(
                            get_span!(self, ident.node_id),
                            SyntaxError::InvalidIdentInStrict,
                        );
                    }
                    _ => {}
                }

                if self.syntax().typescript() {
                    match ident.sym {
                        js_word!("public") | js_word!("static") | js_word!("abstract") => {
                            if eat!(self, "interface") {
                                self.emit_err(get_span!(self, ident.node_id), SyntaxError::TS2427);
                                self.parse_ts_interface_decl()?;
                                return Ok(Some(Stmt::Empty(EmptyStmt {
                                    node_id: node_id!(self, span!(self, start)),
                                })));
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        if eat!(self, ';') {
            Ok(Some(Stmt::Expr(ExprStmt {
                node_id: node_id!(self, span!(self, start)),
                expr: expr.unwrap(),
            })))
        } else {
            if let Token::BinOp(..) = *cur!(self, false)? {
                self.emit_err(self.input.cur_span(), SyntaxError::TS1005);
                let expr = self.parse_bin_op_recursively(expr, 0)?.unwrap();
                return Ok(Some(Stmt::Expr(ExprStmt {
                    node_id: node_id!(self, span!(self, start)),
                    expr,
                })));
            }

            syntax_error!(
                self,
                SyntaxError::ExpectedSemiForExprStmt {
                    expr: get_span!(self, expr.node_id())
                }
            );
        }
    }

    fn verify_break_continue(&self, is_break: bool, label: &Option<Ident>, span: Span) {
        if is_break {
            if label.is_some() && !self.labels.contains(&label.as_ref().unwrap().sym) {
                self.emit_err(span, SyntaxError::TS1116);
            } else if !self.ctx().is_break_allowed() {
                self.emit_err(span, SyntaxError::TS1105);
            }
        } else if !self.ctx().is_continue_allowed() {
            self.emit_err(span, SyntaxError::TS1115);
        } else if label.is_some() && !self.labels.contains(&label.as_ref().unwrap().sym) {
            self.emit_err(span, SyntaxError::TS1107);
        }
    }

    fn parse_debugger_stmt(&mut self, start: BytePos) -> PResult<Stmt> {
        self.input.bump();
        expect!(self, ';');
        Ok(Stmt::Debugger(DebuggerStmt {
            node_id: node_id!(self, span!(self, start)),
        }))
    }

    fn parse_header_expr(&mut self) -> PResult<MaybeParen> {
        expect!(self, '(');
        let val = self
            .include_in_expr(true)
            .parse_expr(&mut AssignProps::Emit)?;
        expect!(self, ')');
        Ok(val)
    }

    fn parse_do_stmt(&mut self) -> PResult<Stmt> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("do"));

        let ctx = Context {
            flags: self.ctx().flags
                | ContextFlags::is_break_allowed
                | ContextFlags::is_continue_allowed,
            ..self.ctx()
        };

        let body = self
            .with_ctx(ctx)
            .parse_stmt(StmtParseCtx::Other, false)
            .map(Box::new)?;

        expect!(self, "while");
        let test = self.parse_header_expr()?.unwrap();
        self.input.eat(&tok!(';'));

        Ok(Stmt::DoWhile(DoWhileStmt {
            node_id: node_id!(self, span!(self, start)),
            test,
            body,
        }))
    }

    // Disambiguating between a `for` and a `for`/`in` or `for`/`of`
    // loop is non-trivial. Basically, we have to parse the init `var`
    // statement or expression, disallowing the `in` operator, and then check
    // whether the next token is `in` or `of`. When there is no init
    // part (semicolon immediately after the opening parenthesis), it
    // is a regular `for` loop.
    fn parse_for_stmt(&mut self) -> PResult<Stmt> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("for"));

        let await_start = self.input.cur_pos();
        let await_token = if self.input.eat(&tok!("await")) {
            Some(span!(self, await_start))
        } else {
            None
        };

        expect!(self, '(');
        let head = self.parse_for_head()?;
        expect!(self, ')');

        let ctx = Context {
            flags: self.ctx().flags
                | ContextFlags::is_break_allowed
                | ContextFlags::is_continue_allowed,
            ..self.ctx()
        };
        let body = self
            .with_ctx(ctx)
            .parse_stmt(StmtParseCtx::Other, false)
            .map(Box::new)?;

        let span = span!(self, start);
        Ok(match head {
            ForHead::For { init, test, update } => {
                if let Some(await_token) = await_token {
                    syntax_error!(self, await_token, SyntaxError::AwaitForStmt);
                }

                Stmt::For(ForStmt {
                    node_id: node_id!(self, span),
                    init,
                    test,
                    update,
                    body,
                })
            }
            ForHead::ForIn { left, right } => {
                if let Some(await_token) = await_token {
                    syntax_error!(self, await_token, SyntaxError::AwaitForStmt);
                }

                Stmt::ForIn(ForInStmt {
                    node_id: node_id!(self, span),
                    left,
                    right,
                    body,
                })
            }
            ForHead::ForOf { left, right } => Stmt::ForOf(ForOfStmt {
                node_id: node_id!(self, span),
                is_await: await_token.is_some(),
                left,
                right,
                body,
            }),
        })
    }

    fn parse_for_head(&mut self) -> PResult<ForHead> {
        if is_one_of!(self, "const", "var")
            || (self.input.is(&tok!("let")) && peek!(self)?.follows_keyword_let())
        {
            let decl = self.parse_var_stmt(true)?;

            if is_one_of!(self, "of", "in") {
                if decl.decls.len() > 1 {
                    for excess_decl in decl.decls.iter().skip(1) {
                        self.emit_err(
                            get_span!(self, excess_decl.name.node_id()),
                            SyntaxError::TooManyVarInForInHead,
                        );
                    }
                    // TODO: is the following error more accurate/descriptive than the above one?

                    // let span_of_excess_decls = Span::new(
                    //     decl.decls[0].span.lo,
                    //     decl.decls.last().unwrap().span.hi,
                    //     Default::default(),
                    // );
                    // panic!("Too many variable declarations in for in/of head. Expected 1 declaration, found {}. {:?}", decl.decls.len(), span_of_excess_decls);
                } else if decl.decls[0].init.is_some() {
                    self.emit_err(
                        get_span!(self, decl.decls[0].name.node_id()),
                        SyntaxError::VarInitializerInForInHead,
                    );
                }

                return self.parse_for_each_head(VarDeclOrPat::VarDecl(decl));
            }

            expect_exact!(self, ';');
            return self.parse_normal_for_head(Some(VarDeclOrExpr::VarDecl(decl)));
        }

        if self.input.eat(&tok!(';')) {
            return self.parse_normal_for_head(None);
        }

        let mut assign_props = AssignProps::Buffer(Vec::new());
        let init = self
            .include_in_expr(false)
            .parse_expr_or_pat(&mut assign_props)?;

        // for (a of b)
        if is_one_of!(self, "of", "in") {
            let is_in = is!(self, "in");

            let pat = self.reparse_expr_as_pat(PatType::AssignPat, init.unwrap())?;

            // for ({} in foo) is invalid
            if self.input.syntax().typescript() && is_in {
                match pat {
                    Pat::Ident(_) | Pat::Expr(_) => {}
                    _ => self.emit_err(get_span!(self, pat.node_id()), SyntaxError::TS2491),
                }
            }

            return self.parse_for_each_head(VarDeclOrPat::Pat(pat));
        }

        expect_exact!(self, ';');

        let assign_props = match assign_props {
            AssignProps::Buffer(props) => props,
            _ => unreachable!(),
        };
        for prop in assign_props {
            self.emit_err(prop, SyntaxError::AssignProperty);
        }
        self.parse_normal_for_head(Some(VarDeclOrExpr::Expr(init.unwrap())))
    }

    fn parse_for_each_head(&mut self, left: VarDeclOrPat) -> PResult<ForHead> {
        let of = self.input.bump() == tok!("of");
        if of {
            let right = self
                .include_in_expr(true)
                .parse_assignment_expr(&mut AssignProps::Emit)?
                .unwrap();
            Ok(ForHead::ForOf { left, right })
        } else {
            let right = self
                .include_in_expr(true)
                .parse_expr(&mut AssignProps::Emit)?
                .unwrap();
            Ok(ForHead::ForIn { left, right })
        }
    }

    fn parse_normal_for_head(&mut self, init: Option<VarDeclOrExpr>) -> PResult<ForHead> {
        let test = if self.input.eat(&tok!(';')) {
            None
        } else {
            let test = self
                .include_in_expr(true)
                .parse_expr(&mut AssignProps::Emit)
                .map(MaybeParen::unwrap)
                .map(Some)?;
            expect_exact!(self, ';');
            test
        };

        let update = if self.input.is(&tok!(')')) {
            None
        } else {
            self.include_in_expr(true)
                .parse_expr(&mut AssignProps::Emit)
                .map(MaybeParen::unwrap)
                .map(Some)?
        };

        Ok(ForHead::For { init, test, update })
    }

    fn parse_if_stmt(&mut self) -> PResult<Stmt> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("if"));

        // TODO: let test = self.parse_header_expr()?;

        expect!(self, '(');
        let test = self
            .include_in_expr(true)
            .parse_expr(&mut AssignProps::Emit)?
            .unwrap();
        if !eat!(self, ')') {
            self.emit_err(self.input.cur_span(), SyntaxError::TS1005);

            let span = span!(self, start);
            return Ok(Stmt::If(IfStmt {
                node_id: node_id!(self, span),
                test,
                cons: Box::new(Stmt::Expr(ExprStmt {
                    node_id: node_id!(self, span),
                    expr: Box::new(Expr::Invalid(Invalid {
                        node_id: node_id!(self, span),
                    })),
                })),
                alt: Default::default(),
            }));
        }

        let consequent = self
            .parse_stmt(StmtParseCtx::IfOrLabel, false)
            .map(Box::new)?;
        let alternate = if self.input.eat(&tok!("else")) {
            Some(
                self.parse_stmt(StmtParseCtx::IfOrLabel, false)
                    .map(Box::new)?,
            )
        } else {
            None
        };

        Ok(Stmt::If(IfStmt {
            node_id: node_id!(self, span!(self, start)),
            test,
            cons: consequent,
            alt: alternate,
        }))
    }

    fn parse_return_stmt(&mut self) -> PResult<Stmt> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("return"));

        // In `return` (and `break`/`continue`), the keywords with
        // optional arguments, we eagerly look for a semicolon or the
        // possibility to insert one.

        let arg = if is!(self, ';') {
            None
        } else {
            self.include_in_expr(true)
                .parse_expr(&mut AssignProps::Emit)
                .map(MaybeParen::unwrap)
                .map(Some)?
        };

        expect!(self, ';');

        if !self.ctx().in_function() {
            self.emit_err(span!(self, start), SyntaxError::ReturnNotAllowed);
        }
        Ok(Stmt::Return(ReturnStmt {
            node_id: node_id!(self, span!(self, start)),
            arg,
        }))
    }

    fn parse_switch_stmt(&mut self) -> PResult<Stmt> {
        let switch_start = self.input.cur_pos();

        self.assert_and_bump(&tok!("switch"));

        let discriminant = self.parse_header_expr()?.unwrap();
        let mut cases = vec![];
        let mut span_of_previous_default = None;

        expect!(self, '{');

        let ctx = Context {
            flags: self.ctx().flags | ContextFlags::is_break_allowed,
            ..self.ctx()
        };

        self.with_ctx(ctx).parse_with(|parser| {
            while is_one_of!(parser, "case", "default") {
                let mut cons = vec![];
                let is_case = parser.input.is(&tok!("case"));
                let case_start = parser.input.cur_pos();

                parser.input.bump();

                let ctx = Context {
                    flags: parser.ctx().flags
                        | ContextFlags::in_case_cond
                        | ContextFlags::include_in_expr,
                    ..parser.ctx()
                };

                let test = if is_case {
                    parser
                        .with_ctx(ctx)
                        .parse_expr(&mut AssignProps::Emit)
                        .map(MaybeParen::unwrap)
                        .map(Some)?
                } else {
                    if let Some(previous) = span_of_previous_default {
                        syntax_error!(parser, SyntaxError::MultipleDefault { previous });
                    }
                    span_of_previous_default = Some(span!(parser, case_start));

                    None
                };
                expect!(parser, ':');

                while !eof!(parser) && !is_one_of!(parser, "case", "default", '}') {
                    if let Some(s) = parser.parse_stmt_list_item(false)? {
                        cons.push(s);
                    }
                }

                let span = Span::new(case_start, parser.input.prev_span().hi);
                cases.push(SwitchCase {
                    node_id: node_id!(parser, span),
                    test,
                    cons,
                });
            }

            Ok(())
        })?;

        expect!(self, '}');

        Ok(Stmt::Switch(SwitchStmt {
            node_id: node_id!(self, span!(self, switch_start)),
            discriminant,
            cases,
        }))
    }

    fn parse_throw_stmt(&mut self) -> PResult<Stmt> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("throw"));

        if self.input.had_line_break_before_cur() {
            // TODO(swc): Suggest throw arg;
            syntax_error!(self, SyntaxError::LineBreakInThrow);
        }

        let arg = self
            .include_in_expr(true)
            .parse_expr(&mut AssignProps::Emit)?
            .unwrap();
        expect!(self, ';');

        Ok(Stmt::Throw(ThrowStmt {
            node_id: node_id!(self, span!(self, start)),
            arg,
        }))
    }

    fn parse_try_stmt(&mut self) -> PResult<Stmt> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("try"));

        let block = self.parse_block(false)?;

        let catch_start = self.input.cur_pos();
        let handler = self.parse_catch_clause()?;
        let finalizer = self.parse_finally_block()?;

        if handler.is_none() && finalizer.is_none() {
            // self.raise(node.start, Errors.NoCatchOrFinally);
            // TODO: is the babel's error message more descriptive than this:
            self.emit_err(Span::new(catch_start, catch_start), SyntaxError::TS1005);
        }

        Ok(Stmt::Try(TryStmt {
            node_id: node_id!(self, span!(self, start)),
            block,
            handler,
            finalizer,
        }))
    }

    fn parse_catch_clause(&mut self) -> PResult<Option<CatchClause>> {
        let start = self.input.cur_pos();

        Ok(if self.input.eat(&tok!("catch")) {
            let param = self.parse_catch_param()?;

            self.parse_block(false)
                .map(|body| CatchClause {
                    node_id: node_id!(self, span!(self, start)),
                    param,
                    body,
                })
                .map(Some)?
        } else {
            None
        })
    }

    fn parse_finally_block(&mut self) -> PResult<Option<BlockStmt>> {
        Ok(if self.input.eat(&tok!("finally")) {
            self.parse_block(false).map(Some)?
        } else {
            None
        })
    }

    /// Optional since es2019
    fn parse_catch_param(&mut self) -> PResult<Option<Pat>> {
        if eat!(self, '(') {
            let pat = self.parse_binding_pat_or_ident()?;

            // Type annotation.
            if self.syntax().typescript() && eat!(self, ':') {
                // Type annotation.
                self.in_type().parse_with(|parser| parser.parse_ts_type())?;
                // self.emit_err(ty.span(), SyntaxError::TS1196);
            }
            expect!(self, ')');
            Ok(Some(pat))
        } else {
            Ok(None)
        }
    }

    pub(super) fn parse_var_stmt(&mut self, for_loop: bool) -> PResult<VarDecl> {
        let start = self.input.cur_pos();
        let kind = match self.input.bump() {
            tok!("const") => VarDeclKind::Const,
            tok!("let") => VarDeclKind::Let,
            tok!("var") => VarDeclKind::Var,
            _ => unreachable!(),
        };
        let var_span = span!(self, start);
        let should_include_in = kind != VarDeclKind::Var || !for_loop;

        if self.syntax().typescript() && for_loop {
            let res = if is_one_of!(self, "in", "of") {
                self.ts_look_ahead(|parser| {
                    if !eat!(parser, "of") && !eat!(parser, "in") {
                        return Ok(false);
                    }

                    parser.parse_assignment_expr(&mut AssignProps::Emit)?;
                    expect!(parser, ')');

                    Ok(true)
                })
            } else {
                Ok(false)
            };

            match res {
                Ok(true) => {
                    let pos = var_span.hi();
                    let span = Span::new(pos, pos);
                    self.emit_err(span, SyntaxError::TS1123);

                    return Ok(VarDecl {
                        node_id: node_id!(self, span!(self, start)),
                        kind,
                        decls: vec![],
                    });
                }
                Err(..) => {}
                _ => {}
            }
        }

        let mut decls = vec![];
        let mut type_annotations = vec![];
        let mut first = true;
        while first || self.input.eat(&tok!(',')) {
            if first {
                first = false;
            }

            let ctx = if should_include_in {
                Context {
                    flags: self.ctx().flags | ContextFlags::include_in_expr,
                    ..self.ctx()
                }
            } else {
                self.ctx()
            };

            // Handle
            //      var a,;
            //
            // NewLine is ok
            if self.input.is(&tok!(';')) || eof!(self) {
                let prev_span = self.input.prev_span();
                let span = if prev_span == var_span {
                    Span::new(prev_span.hi, prev_span.hi)
                } else {
                    prev_span
                };
                self.emit_err(span, SyntaxError::TS1009);
                break;
            }

            let (decl, type_annotation) = self.with_ctx(ctx).parse_var_declarator(for_loop)?;
            decls.push(decl);
            if let Some(type_ann) = type_annotation {
                type_annotations.push(type_ann);
            }
        }

        if !for_loop && !eat!(self, ';') {
            self.emit_err(self.input.cur_span(), SyntaxError::TS1005);

            let _ = self.parse_expr(&mut AssignProps::Emit);

            while !eat!(self, ';') {
                self.input.bump();
            }
        }

        // Type annotations are not allowed in for-in/for-of variable declarations.
        if for_loop && self.syntax().typescript() && is_one_of!(self, "in", "of") {
            for type_ann in type_annotations {
                self.emit_err(type_ann, SyntaxError::TS2483);
            }
        }

        Ok(VarDecl {
            node_id: node_id!(self, span!(self, start)),
            kind,
            decls,
        })
    }

    fn parse_var_declarator(&mut self, for_loop: bool) -> PResult<(VarDeclarator, Option<Span>)> {
        let start = self.input.cur_pos();

        let name = self.parse_binding_pat_or_ident()?;

        // TS definite.
        if self.input.syntax().typescript() && matches!(name, Pat::Ident(_)) {
            eat!(self, '!');
        }

        // Typescript extension
        let type_annotation = if self.input.syntax().typescript() && is!(self, ':') {
            self.try_parse_ts_type_ann()?
        } else {
            None
        };

        //FIXME(swc): This is wrong. Should check in/of only on first loop.
        let init = if !for_loop || !is_one_of!(self, "in", "of") {
            if self.input.eat(&tok!('=')) {
                Some(self.parse_assignment_expr(&mut AssignProps::Emit)?.unwrap())
            } else {
                // Destructuring bindings require initializers, but
                // typescript allows `declare` vars not to have initializers.
                if self.ctx().in_declare() {
                    None
                } else {
                    match name {
                        Pat::Ident(..) => None,
                        _ => {
                            syntax_error!(self, span!(self, start), SyntaxError::PatVarWithoutInit)
                        }
                    }
                }
            }
        } else {
            // e.g. for(let a;;)
            None
        };

        Ok((
            VarDeclarator {
                node_id: node_id!(self, span!(self, start)),
                name,
                init,
            },
            type_annotation,
        ))
    }

    fn parse_while_stmt(&mut self) -> PResult<Stmt> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("while"));

        let test = self.parse_header_expr()?.unwrap();

        let ctx = Context {
            flags: self.ctx().flags
                | ContextFlags::is_break_allowed
                | ContextFlags::is_continue_allowed,
            ..self.ctx()
        };
        let body = self
            .with_ctx(ctx)
            .parse_stmt(StmtParseCtx::Other, false)
            .map(Box::new)?;

        Ok(Stmt::While(WhileStmt {
            node_id: node_id!(self, span!(self, start)),
            test,
            body,
        }))
    }

    fn parse_with_stmt(&mut self) -> PResult<Stmt> {
        if self.syntax().typescript() {
            let span = self.input.cur_span();
            self.emit_err(span, SyntaxError::TS2410);
        }

        {
            let span = self.input.cur_span();
            self.emit_strict_mode_err(span, SyntaxError::WithInStrict);
        }

        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("with"));

        let obj = self.parse_header_expr()?.unwrap();

        let ctx = Context {
            flags: self.ctx().flags | ContextFlags::in_function,
            ..self.ctx()
        };
        let body = self
            .with_ctx(ctx)
            .parse_stmt(StmtParseCtx::Other, false)
            .map(Box::new)?;

        Ok(Stmt::With(WithStmt {
            node_id: node_id!(self, span!(self, start)),
            obj,
            body,
        }))
    }

    pub(super) fn parse_block(&mut self, allow_directives: bool) -> PResult<BlockStmt> {
        let start = self.input.cur_pos();

        if allow_directives {
            self.input.convert_strict_mode_errors_to_module_errors();
        }

        expect!(self, '{');

        let stmts = self.parse_block_body(allow_directives, false, Some(&tok!('}')))?;

        let span = span!(self, start);
        Ok(BlockStmt {
            node_id: node_id!(self, span),
            stmts,
        })
    }

    fn parse_labelled_stmt(&mut self, label: Ident) -> PResult<Stmt> {
        let ctx = Context {
            flags: self.ctx().flags | ContextFlags::is_break_allowed,
            ..self.ctx()
        };

        self.with_ctx(ctx).parse_with(|parser| {
            for existing_label in &parser.labels {
                if label.sym == *existing_label {
                    parser.emit_err(
                        get_span!(parser, label.node_id),
                        SyntaxError::DuplicateLabel(label.sym.clone()),
                    );
                }
            }
            parser.labels.push(label.sym.clone());

            let body = Box::new(if parser.input.is(&tok!("function")) {
                let f = parser.parse_fn_decl()?;
                if let Decl::Fn(f) = &f {
                    if f.function.is_generator() {
                        syntax_error!(
                            p,
                            get_span!(parser, f.node_id),
                            SyntaxError::LabelledGenerator
                        )
                    }
                }

                Stmt::Decl(f)
            } else {
                parser.parse_stmt(StmtParseCtx::IfOrLabel, false)?
            });

            {
                let pos = parser.labels.iter().position(|v| v == &label.sym);
                if let Some(pos) = pos {
                    parser.labels.remove(pos);
                }
            }

            Ok(Stmt::Labeled(LabeledStmt {
                node_id: node_id!(parser, span!(parser, get_span!(parser, label.node_id).lo())),
                label,
                body,
            }))
        })
    }
}
