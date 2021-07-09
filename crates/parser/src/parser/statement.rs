use super::{pat::PatType, *};
use crate::{
    context::{Context, YesMaybe},
    token::{Token, Word},
};
use global_common::{BytePos, Span, Spanned};
use swc_atoms::js_word;

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
            Some(&Stmt::Expr(ref expr)) => match *expr.expr {
                Expr::Lit(Lit::Str(Str {
                    ref value,
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
            Some(&Stmt::Expr(ref expr)) => match *expr.expr {
                Expr::Lit(Lit::Str(Str { .. })) => true,
                _ => false,
            },
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
    fn handle_import_export(
        &mut self,
        top_level: bool,
        decorators: Vec<Decorator>,
    ) -> PResult<Type>;
}

#[derive(PartialEq, Eq)]
pub enum StmtParseCtx {
    IfOrLabel,
    Other,
    None,
}

impl<I: Tokens> StmtLikeParser<Stmt> for Parser<I> {
    fn handle_import_export(&mut self, _: bool, _: Vec<Decorator>) -> PResult<Stmt> {
        let start = self.input.cur_pos();
        if self.input.syntax().dynamic_import() && is!(self, "import") {
            let expr = self.parse_expr()?;

            eat!(self, ';');

            return Ok(ExprStmt {
                span: span!(self, start),
                expr,
            }
            .into());
        }

        if self.input.syntax().import_meta()
            && is!(self, "import")
            && self.input.peeked_is(&tok!('.'))
        {
            let expr = self.parse_expr()?;

            eat!(self, ';');

            return Ok(ExprStmt {
                span: span!(self, start),
                expr,
            }
            .into());
        }

        syntax_error!(self, SyntaxError::ImportExportInScript);
    }
}

impl<I: Tokens> Parser<I> {
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
            let stmt = self.parse_stmt_like(StmtParseCtx::None, top_level)?;

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

    pub fn parse_stmt(&mut self, parse_ctx: StmtParseCtx, top_level: bool) -> PResult<Stmt> {
        trace_cur!(self, parse_stmt);
        self.parse_stmt_like(parse_ctx, top_level)
    }

    fn parse_stmt_list_item(&mut self, top_level: bool) -> PResult<Stmt> {
        trace_cur!(self, parse_stmt_list_item);
        self.parse_stmt_like(StmtParseCtx::None, top_level)
    }

    /// Parse a statement, declaration or module item.
    fn parse_stmt_like<Type>(&mut self, parse_ctx: StmtParseCtx, top_level: bool) -> PResult<Type>
    where
        Self: StmtLikeParser<Type>,
        Type: IsDirective + From<Stmt>,
    {
        trace_cur!(self, parse_stmt_like);
        let start = self.input.cur_pos();
        let decorators = self.parse_decorators(true)?;

        if is_one_of!(self, "import", "export") {
            return self.handle_import_export(top_level, decorators);
        }

        self.parse_stmt_content(start, parse_ctx, top_level, decorators)
            .map(From::from)
    }
    fn parse_stmt_content(
        &mut self,
        start: BytePos,
        parse_ctx: StmtParseCtx,
        top_level: bool,
        decorators: Vec<Decorator>,
    ) -> PResult<Stmt> {
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
            return Ok(Stmt::Expr(ExprStmt { span, expr }));
        }

        if self.input.syntax().typescript() && is!(self, "const") && peeked_is!(self, "enum") {
            self.assert_and_bump(&tok!("const"));
            self.assert_and_bump(&tok!("enum"));
            return self
                .parse_ts_enum_decl(start, true)
                .map(Decl::from)
                .map(Stmt::from);
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
                    return Ok(Stmt::Break(BreakStmt { span, label }));
                } else {
                    return Ok(Stmt::Continue(ContinueStmt { span, label }));
                }
            }
            tok!("debugger") => {
                return self.parse_debugger_stmt(start);
            }
            tok!("do") => {
                return self.parse_do_stmt();
            }
            tok!("for") => {
                return self.parse_for_stmt();
            }
            tok!("function") => {
                if parse_ctx != StmtParseCtx::None {
                    if self.ctx().is_strict() {
                        syntax_error!(self, SyntaxError::StrictFunction);
                    } else if parse_ctx == StmtParseCtx::Other {
                        syntax_error!(self, SyntaxError::SloppyFunction);
                    }
                }
                return self.parse_fn_decl(decorators).map(Stmt::from);
            }
            tok!("class") => {
                if parse_ctx != StmtParseCtx::None {
                    syntax_error!(self, SyntaxError::UnexpectedClassInSingleStatementCtx);
                }
                return self
                    .parse_class_decl(start, start, decorators)
                    .map(Stmt::from);
            }
            tok!("if") => {
                return self.parse_if_stmt();
            }
            tok!("return") => {
                return self.parse_return_stmt();
            }
            tok!("switch") => {
                return self.parse_switch_stmt();
            }
            tok!("throw") => {
                return self.parse_throw_stmt();
            }
            // Error recovery
            tok!("catch") => {
                let span = self.input.cur_span();
                self.emit_err(span, SyntaxError::TS1005);

                let _ = self.parse_catch_clause();
                let _ = self.parse_finally_block();

                return Ok(Stmt::Expr(ExprStmt {
                    span,
                    expr: Box::new(Expr::Invalid(Invalid { span })),
                }));
            }
            // Error recovery
            tok!("finally") => {
                let span = self.input.cur_span();
                self.emit_err(span, SyntaxError::TS1005);

                let _ = self.parse_finally_block();

                return Ok(Stmt::Expr(ExprStmt {
                    span,
                    expr: Box::new(Expr::Invalid(Invalid { span })),
                }));
            }
            tok!("try") => {
                return self.parse_try_stmt();
            }
            tok!("var") => {
                let v = self.parse_var_stmt(false)?;
                return Ok(Stmt::Decl(Decl::Var(v)));
            }
            tok!("const") => {
                if parse_ctx != StmtParseCtx::None {
                    syntax_error!(self, SyntaxError::UnexpectedLexicalDeclaration);
                }

                let v = self.parse_var_stmt(false)?;
                return Ok(Stmt::Decl(Decl::Var(v)));
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
                    return Ok(Stmt::Decl(Decl::Var(v)));
                }
            }
            tok!("while") => {
                return self.parse_while_stmt();
            }
            tok!("with") => {
                return self.parse_with_stmt();
            }
            tok!('{') => {
                return self.parse_block(false).map(Stmt::Block);
            }
            tok!(';') => {
                self.input.bump();
                return Ok(Stmt::Empty(EmptyStmt {
                    span: span!(self, start),
                }));
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

            return self.parse_async_fn_decl(decorators).map(From::from);
        }

        // If the statement does not start with a statement keyword or a
        // brace, it's an ExpressionStatement or LabeledStatement. We
        // simply start parsing an expression, and afterwards, if the
        // next token is a colon and the expression was a simple
        // Identifier node, we switch to interpreting it as a label.
        let expr = self.include_in_expr(true).parse_expr()?;

        let expr = match *expr {
            Expr::Ident(ident) => {
                if self.input.eat(&tok!(':')) {
                    return self.parse_labelled_stmt(ident);
                }
                Box::new(Expr::Ident(ident))
            }
            _ => self.verify_expr(expr),
        };
        if let Expr::Ident(ref ident) = *expr {
            if *ident.sym == js_word!("interface") && self.input.had_line_break_before_cur() {
                self.emit_strict_mode_err(ident.span, SyntaxError::InvalidIdentInStrict);

                eat!(self, ';');

                return Ok(Stmt::Expr(ExprStmt {
                    span: span!(self, start),
                    expr,
                }));
            }

            if self.input.syntax().typescript() {
                if let Some(decl) = self.parse_ts_expr_stmt(decorators, ident.clone())? {
                    return Ok(Stmt::Decl(decl));
                }
            }
        }

        if let Expr::Ident(Ident { ref sym, span, .. }) = *expr {
            match *sym {
                js_word!("enum") | js_word!("interface") => {
                    self.emit_strict_mode_err(span, SyntaxError::InvalidIdentInStrict);
                }
                _ => {}
            }
        }

        if self.syntax().typescript() {
            match *expr {
                Expr::Ident(ref i) => match i.sym {
                    js_word!("public") | js_word!("static") | js_word!("abstract") => {
                        if eat!(self, "interface") {
                            self.emit_err(i.span, SyntaxError::TS2427);
                            return self
                                .parse_ts_interface_decl(start)
                                .map(Decl::from)
                                .map(Stmt::from);
                        }
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        if eat!(self, ';') {
            Ok(Stmt::Expr(ExprStmt {
                span: span!(self, start),
                expr,
            }))
        } else {
            if let Token::BinOp(..) = *cur!(self, false)? {
                self.emit_err(self.input.cur_span(), SyntaxError::TS1005);
                let expr = self.parse_bin_op_recursively(expr, 0)?;
                return Ok(ExprStmt {
                    span: span!(self, start),
                    expr,
                }
                .into());
            }

            syntax_error!(
                self,
                SyntaxError::ExpectedSemiForExprStmt { expr: expr.span() }
            );
        }
    }

    fn verify_break_continue(&self, is_break: bool, label: &Option<Ident>, span: Span) {
        if is_break {
            if label.is_some() && !self.state.labels.contains(&label.as_ref().unwrap().sym) {
                self.emit_err(span, SyntaxError::TS1116);
            } else if !self.ctx().is_break_allowed {
                self.emit_err(span, SyntaxError::TS1105);
            }
        } else if !self.ctx().is_continue_allowed {
            self.emit_err(span, SyntaxError::TS1115);
        } else if label.is_some() && !self.state.labels.contains(&label.as_ref().unwrap().sym) {
            self.emit_err(span, SyntaxError::TS1107);
        }
    }

    fn parse_debugger_stmt(&mut self, start: BytePos) -> PResult<Stmt> {
        self.input.bump();
        expect!(self, ';');
        Ok(Stmt::Debugger(DebuggerStmt {
            span: span!(self, start),
        }))
    }

    fn parse_header_expr(&mut self) -> PResult<Box<Expr>> {
        expect!(self, '(');
        let val = self.include_in_expr(true).parse_expr()?;
        expect!(self, ')');
        Ok(val)
    }

    fn parse_do_stmt(&mut self) -> PResult<Stmt> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("do"));

        let ctx = Context {
            is_break_allowed: true,
            is_continue_allowed: true,
            ..self.ctx()
        };

        let body = self
            .with_ctx(ctx)
            .parse_stmt(StmtParseCtx::Other, false)
            .map(Box::new)?;

        expect!(self, "while");
        let test = self.parse_header_expr()?;
        self.input.eat(&tok!(';'));

        Ok(Stmt::DoWhile(DoWhileStmt {
            span: span!(self, start),
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
            is_break_allowed: true,
            is_continue_allowed: true,
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
                    span,
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
                    span,
                    left,
                    right,
                    body,
                })
            }
            ForHead::ForOf { left, right } => Stmt::ForOf(ForOfStmt {
                span,
                await_token,
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
                        self.emit_err(excess_decl.name.span(), SyntaxError::TooManyVarInForInHead);
                    }
                    // TODO: is the following error more accurate/descriptive than the above one?

                    // let span_of_excess_decls = Span::new(
                    //     decl.decls[0].span.lo,
                    //     decl.decls.last().unwrap().span.hi,
                    //     Default::default(),
                    // );
                    // panic!("Too many variable declarations in for in/of head. Expected 1 declaration, found {}. {:?}", decl.decls.len(), span_of_excess_decls);
                } else {
                    if decl.decls[0].init.is_some() {
                        self.emit_err(
                            decl.decls[0].name.span(),
                            SyntaxError::VarInitializerInForInHead,
                        );
                    }

                    if self.syntax().typescript() {
                        let type_ann = match decl.decls[0].name {
                            Pat::Ident(ref v) => Some(&v.type_ann),
                            Pat::Array(ref v) => Some(&v.type_ann),
                            Pat::Assign(ref v) => Some(&v.type_ann),
                            Pat::Rest(ref v) => Some(&v.type_ann),
                            Pat::Object(ref v) => Some(&v.type_ann),
                            _ => None,
                        };

                        if let Some(type_ann) = type_ann {
                            if type_ann.is_some() {
                                self.emit_err(decl.decls[0].name.span(), SyntaxError::TS2483);
                            }
                        }
                    }
                }

                return self.parse_for_each_head(VarDeclOrPat::VarDecl(decl));
            }

            expect_exact!(self, ';');
            return self.parse_normal_for_head(Some(VarDeclOrExpr::VarDecl(decl)));
        }

        let init = if self.input.eat(&tok!(';')) {
            return self.parse_normal_for_head(None);
        } else {
            self.include_in_expr(false).parse_expr_or_pat()?
        };

        // for (a of b)
        if is_one_of!(self, "of", "in") {
            let is_in = is!(self, "in");

            let pat = self.reparse_expr_as_pat(PatType::AssignPat, init)?;

            // for ({} in foo) is invalid
            if self.input.syntax().typescript() && is_in {
                match pat {
                    Pat::Ident(..) => {}
                    Pat::Expr(..) => {}
                    ref v => self.emit_err(v.span(), SyntaxError::TS2491),
                }
            }

            return self.parse_for_each_head(VarDeclOrPat::Pat(pat));
        }

        expect_exact!(self, ';');

        let init = self.verify_expr(init);
        self.parse_normal_for_head(Some(VarDeclOrExpr::Expr(init)))
    }

    fn parse_for_each_head(&mut self, left: VarDeclOrPat) -> PResult<ForHead> {
        let of = self.input.bump() == tok!("of");
        if of {
            let right = self.include_in_expr(true).parse_assignment_expr()?;
            Ok(ForHead::ForOf { left, right })
        } else {
            let right = self.include_in_expr(true).parse_expr()?;
            Ok(ForHead::ForIn { left, right })
        }
    }

    fn parse_normal_for_head(&mut self, init: Option<VarDeclOrExpr>) -> PResult<ForHead> {
        let test = if self.input.eat(&tok!(';')) {
            None
        } else {
            let test = self.include_in_expr(true).parse_expr().map(Some)?;
            expect_exact!(self, ';');
            test
        };

        let update = if self.input.is(&tok!(')')) {
            None
        } else {
            self.include_in_expr(true).parse_expr().map(Some)?
        };

        Ok(ForHead::For { init, test, update })
    }

    fn parse_if_stmt(&mut self) -> PResult<Stmt> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("if"));

        // TODO: let test = self.parse_header_expr()?;

        expect!(self, '(');
        let test = self.include_in_expr(true).parse_expr()?;
        if !eat!(self, ')') {
            self.emit_err(self.input.cur_span(), SyntaxError::TS1005);

            let span = span!(self, start);
            return Ok(Stmt::If(IfStmt {
                span,
                test,
                cons: Box::new(Stmt::Expr(ExprStmt {
                    span,
                    expr: Box::new(Expr::Invalid(Invalid { span })),
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
            span: span!(self, start),
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
            self.include_in_expr(true).parse_expr().map(Some)?
        };

        expect!(self, ';');

        if !self.ctx().in_function {
            self.emit_err(span!(self, start), SyntaxError::ReturnNotAllowed);
        }
        Ok(Stmt::Return(ReturnStmt {
            span: span!(self, start),
            arg,
        }))
    }

    fn parse_switch_stmt(&mut self) -> PResult<Stmt> {
        let switch_start = self.input.cur_pos();

        self.assert_and_bump(&tok!("switch"));

        let discriminant = self.parse_header_expr()?;
        let mut cases = vec![];
        let mut span_of_previous_default = None;

        expect!(self, '{');

        let ctx = Context {
            is_break_allowed: true,
            ..self.ctx()
        };

        self.with_ctx(ctx).parse_with(|parser| {
            while is_one_of!(parser, "case", "default") {
                let mut cons = vec![];
                let is_case = parser.input.is(&tok!("case"));
                let case_start = parser.input.cur_pos();

                parser.input.bump();

                let ctx = Context {
                    in_case_cond: true,
                    include_in_expr: true,
                    ..parser.ctx()
                };

                let test = if is_case {
                    parser.with_ctx(ctx).parse_expr().map(Some)?
                } else {
                    if let Some(previous) = span_of_previous_default {
                        syntax_error!(parser, SyntaxError::MultipleDefault { previous });
                    }
                    span_of_previous_default = Some(span!(parser, case_start));

                    None
                };
                expect!(parser, ':');

                while !eof!(parser) && !is_one_of!(parser, "case", "default", '}') {
                    cons.push(parser.parse_stmt_list_item(false)?);
                }

                cases.push(SwitchCase {
                    span: Span::new(case_start, parser.input.prev_span().hi, Default::default()),
                    test,
                    cons,
                });
            }

            Ok(())
        })?;

        expect!(self, '}');

        Ok(Stmt::Switch(SwitchStmt {
            span: span!(self, switch_start),
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

        let arg = self.include_in_expr(true).parse_expr()?;
        expect!(self, ';');

        Ok(Stmt::Throw(ThrowStmt {
            span: span!(self, start),
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
            self.emit_err(
                Span::new(catch_start, catch_start, Default::default()),
                SyntaxError::TS1005,
            );
        }

        Ok(Stmt::Try(TryStmt {
            span: span!(self, start),
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
                    span: span!(self, start),
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
            let mut pat = self.parse_binding_pat_or_ident()?;

            let type_ann_start = self.input.cur_pos();

            if self.syntax().typescript() && eat!(self, ':') {
                let ctx = Context {
                    in_type: true,
                    ..self.ctx()
                };

                let ty = self
                    .with_ctx(ctx)
                    .parse_with(|parser| parser.parse_ts_type())?;
                // self.emit_err(ty.span(), SyntaxError::TS1196);

                match &mut pat {
                    Pat::Ident(BindingIdent { type_ann, .. })
                    | Pat::Array(ArrayPat { type_ann, .. })
                    | Pat::Rest(RestPat { type_ann, .. })
                    | Pat::Object(ObjectPat { type_ann, .. })
                    | Pat::Assign(AssignPat { type_ann, .. }) => {
                        *type_ann = Some(TsTypeAnn {
                            span: span!(self, type_ann_start),
                            type_ann: ty,
                        });
                    }
                    Pat::Invalid(_) => {}
                    Pat::Expr(_) => {}
                }
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

                    parser.parse_assignment_expr()?;
                    expect!(parser, ')');

                    Ok(true)
                })
            } else {
                Ok(false)
            };

            match res {
                Ok(true) => {
                    let pos = var_span.hi();
                    let span = Span::new(pos, pos, Default::default());
                    self.emit_err(span, SyntaxError::TS1123);

                    return Ok(VarDecl {
                        span: span!(self, start),
                        kind,
                        declare: false,
                        decls: vec![],
                    });
                }
                Err(..) => {}
                _ => {}
            }
        }

        let mut decls = vec![];
        let mut first = true;
        while first || self.input.eat(&tok!(',')) {
            if first {
                first = false;
            }

            let ctx = if should_include_in {
                Context {
                    include_in_expr: true,
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
                    Span::new(prev_span.hi, prev_span.hi, Default::default())
                } else {
                    prev_span
                };
                self.emit_err(span, SyntaxError::TS1009);
                break;
            }

            decls.push(self.with_ctx(ctx).parse_var_declarator(for_loop)?);
        }

        if !for_loop && !eat!(self, ';') {
            self.emit_err(self.input.cur_span(), SyntaxError::TS1005);

            let _ = self.parse_expr();

            while !eat!(self, ';') {
                self.input.bump();
            }
        }

        Ok(VarDecl {
            span: span!(self, start),
            declare: false,
            kind,
            decls,
        })
    }

    fn parse_var_declarator(&mut self, for_loop: bool) -> PResult<VarDeclarator> {
        let start = self.input.cur_pos();

        let mut name = self.parse_binding_pat_or_ident()?;

        let definite = if self.input.syntax().typescript() {
            match name {
                Pat::Ident(..) => eat!(self, '!'),
                _ => false,
            }
        } else {
            false
        };

        // Typescript extension
        if self.input.syntax().typescript() && is!(self, ':') {
            let type_annotation = self.try_parse_ts_type_ann()?;
            match name {
                Pat::Array(ArrayPat {
                    ref mut type_ann, ..
                })
                | Pat::Assign(AssignPat {
                    ref mut type_ann, ..
                })
                | Pat::Ident(BindingIdent {
                    ref mut type_ann, ..
                })
                | Pat::Object(ObjectPat {
                    ref mut type_ann, ..
                })
                | Pat::Rest(RestPat {
                    ref mut type_ann, ..
                }) => {
                    *type_ann = type_annotation;
                }
                _ => unreachable!("invalid syntax: Pat: {:?}", name),
            }
        }

        //FIXME(swc): This is wrong. Should check in/of only on first loop.
        let init = if !for_loop || !is_one_of!(self, "in", "of") {
            if self.input.eat(&tok!('=')) {
                let expr = self.parse_assignment_expr()?;
                let expr = self.verify_expr(expr);

                Some(expr)
            } else {
                // Destructuring bindings require initializers, but
                // typescript allows `declare` vars not to have initializers.
                if self.ctx().in_declare {
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

        Ok(VarDeclarator {
            span: span!(self, start),
            name,
            init,
            definite,
        })
    }

    fn parse_while_stmt(&mut self) -> PResult<Stmt> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("while"));

        let test = self.parse_header_expr()?;

        let ctx = Context {
            is_break_allowed: true,
            is_continue_allowed: true,
            ..self.ctx()
        };
        let body = self
            .with_ctx(ctx)
            .parse_stmt(StmtParseCtx::Other, false)
            .map(Box::new)?;

        Ok(Stmt::While(WhileStmt {
            span: span!(self, start),
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

        let obj = self.parse_header_expr()?;

        let ctx = Context {
            in_function: true,
            ..self.ctx()
        };
        let body = self
            .with_ctx(ctx)
            .parse_stmt(StmtParseCtx::Other, false)
            .map(Box::new)?;

        Ok(Stmt::With(WithStmt {
            span: span!(self, start),
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
        Ok(BlockStmt { span, stmts })
    }

    fn parse_labelled_stmt(&mut self, label: Ident) -> PResult<Stmt> {
        let ctx = Context {
            is_break_allowed: true,
            ..self.ctx()
        };

        self.with_ctx(ctx).parse_with(|parser| {
            for existing_label in &parser.state.labels {
                if label.sym == *existing_label {
                    parser.emit_err(label.span, SyntaxError::DuplicateLabel(label.sym.clone()));
                }
            }
            parser.state.labels.push(label.sym.clone());

            let body = Box::new(if parser.input.is(&tok!("function")) {
                let f = parser.parse_fn_decl(vec![])?;
                match f {
                    Decl::Fn(FnDecl {
                        function:
                            Function {
                                span,
                                is_generator: true,
                                ..
                            },
                        ..
                    }) => syntax_error!(p, span, SyntaxError::LabelledGenerator),
                    _ => {}
                }

                f.into()
            } else {
                parser.parse_stmt(StmtParseCtx::IfOrLabel, false)?
            });

            {
                let pos = parser.state.labels.iter().position(|v| v == &label.sym);
                if let Some(pos) = pos {
                    parser.state.labels.remove(pos);
                }
            }

            Ok(Stmt::Labeled(LabeledStmt {
                span: span!(parser, label.span.lo()),
                label,
                body,
            }))
        })
    }
}
