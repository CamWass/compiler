use super::Lexer;
use crate::{
    JscTarget, Syntax,
    context::{Context, YesMaybe, YesNoMaybe},
    error::Error,
    token::*,
};
use common::BytePos;

pub struct LexerCheckpoint {
    cur: usize,
    ctx: Context,
    state: State,
}

impl Lexer<'_> {
    pub fn checkpoint(&self) -> LexerCheckpoint {
        let Lexer {
            cur,
            bytes: _,
            start_pos: _,
            ctx,
            state,
            syntax: _,
            target: _,
            buf: _,
            errors: _,
            module_errors: _,
            strict_errors: _,
            program_data: _,
        } = self;

        LexerCheckpoint {
            cur: *cur,
            ctx: *ctx,
            state: state.clone(),
        }
    }

    pub fn rewind(&mut self, checkpoint: LexerCheckpoint) {
        let LexerCheckpoint { cur, ctx, state } = checkpoint;

        self.cur = cur;
        self.ctx = ctx;
        self.state = state;
    }

    pub fn set_ctx(&mut self, ctx: Context) {
        self.ctx = ctx;
    }

    pub fn ctx(&self) -> Context {
        self.ctx
    }

    pub fn syntax(&self) -> Syntax {
        self.syntax
    }
    pub fn target(&self) -> JscTarget {
        self.target
    }

    pub fn start_pos(&self) -> BytePos {
        self.start_pos
    }

    pub fn token_context(&self) -> &TokenContexts {
        &self.state.context
    }

    pub fn set_token_context(&mut self, c: TokenContexts) {
        self.state.context = c;
    }

    pub fn add_error(&mut self, error: Error) {
        self.errors.push(error);
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        std::mem::take(&mut self.errors)
    }

    /// Add an error for code which is only invalid in module mode.
    ///
    /// If [Context].module is true, implementers should immediately move the
    /// error to the general error buffer.
    /// If it is false, implementers should buffer the error until they are certain
    /// whether they are parsing a module or not. If they are parsing a module,
    /// the buffered strict errors should be moved to the general error buffer.
    /// If they are parsing a script, they should discard all buffered module errors.
    pub fn add_module_mode_error(&mut self, error: Error) {
        add_module_mode_error(
            self.ctx.module,
            &mut self.module_errors,
            &mut self.errors,
            error,
        );
    }

    /// Add an error for a strict mode violation.
    ///
    /// If [Context].strict is true, implementers should immediately move the
    /// error to the general error buffer.
    /// If it is false, implementers should buffer the error until they are certain
    /// whether the current block of code is in strict mode or not. If they are
    /// certain it is strict, the buffered strict errors should be moved to the
    /// general error buffer. If they are certain it is **NOT** strict, they
    /// should discard all buffered strict errors.
    pub fn add_strict_mode_error(&mut self, error: Error) {
        match self.ctx.strict {
            YesMaybe::Yes => {
                // Definitely in strict mode, immediately add error.
                self.add_error(error);
            }
            YesMaybe::Maybe => {
                // Not yet sure if we are in strict mode, buffer error.
                self.strict_errors.push(error);
            }
        }
    }

    /// Converts buffered strict mode errors into module errors.
    pub fn convert_strict_mode_errors_to_module_errors(&mut self) {
        // Even once we have stopped parsing directives, we still can not be
        // certain of strict mode because we may later discover that we are
        // paring a module, which requires us to reinterpret the code using
        // strict mode. Therefore, rather than discarding any trapped strict
        // mode errors, we convert them into module errors. The logic above, in
        // add_module_mode_error, will decide whether to discard, buffer, or add
        // the error to the main error buffer, depending on if we are certain
        // whether we are paring a module or not.

        for error in self.strict_errors.drain(..) {
            add_module_mode_error(
                self.ctx.module,
                &mut self.module_errors,
                &mut self.errors,
                error,
            );
        }
    }

    pub fn convert_strict_mode_errors_to_standard_errors(&mut self) {
        if !self.strict_errors.is_empty() {
            self.errors.append(&mut self.strict_errors);
        }
    }

    pub fn convert_module_errors_to_standard_errors(&mut self) {
        if !self.module_errors.is_empty() {
            self.errors.append(&mut self.module_errors);
        }
    }
}

/// Add an error for code which is only invalid in module mode.
///
/// If [Context].module is true, implementers should immediately move the
/// error to the general error buffer.
/// If it is false, implementers should buffer the error until they are certain
/// whether they are parsing a module or not. If they are parsing a module,
/// the buffered strict errors should be moved to the general error buffer.
/// If they are parsing a script, they should discard all buffered module errors.
pub fn add_module_mode_error(
    in_module: YesNoMaybe,
    module_errors: &mut Vec<Error>,
    errors: &mut Vec<Error>,
    error: Error,
) {
    match in_module {
        YesNoMaybe::Yes => {
            // Definitely in a module, immediately add error.
            errors.push(error);
        }
        YesNoMaybe::No => {
            // Definitely not in a module, discard error.
        }
        YesNoMaybe::Maybe => {
            // Not yet sure if we are in a module, buffer error.
            module_errors.push(error);
        }
    }
}

/// The algorithm used to determine whether a regexp can appear at a
/// given point in the program is loosely based on sweet.js' approach.
/// See https://github.com/mozilla/sweet.js/wiki/design
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenContext {
    BraceStmt,
    BraceExpr,
    TplQuasi,
    ParenStmt {
        /// Is this `for` loop?
        is_for_loop: bool,
    },
    ParenExpr,
    Tpl {
        /// Start of a template literal.
        start: BytePos,
    },
    FnExpr,
}

impl TokenContext {
    fn preserve_space(&self) -> bool {
        matches!(self, Self::Tpl { .. })
    }

    fn is_expr(&self) -> bool {
        matches!(
            self,
            Self::BraceExpr | Self::TplQuasi | Self::ParenExpr | Self::Tpl { .. } | Self::FnExpr
        )
    }
}

#[derive(Clone, Default)]
pub struct TokenContexts(pub(crate) Vec<TokenContext>);
impl TokenContexts {
    /// Returns true if following `LBrace` token is `block statement` according
    /// to  `ctx`, `prev`, `is_expr_allowed`.
    fn is_brace_block(&self, prev: Token, had_line_break: bool, is_expr_allowed: bool) -> bool {
        if prev == Token::Colon {
            match self.current() {
                Some(TokenContext::BraceStmt) => return true,
                // `{ a: {} }`
                //     ^ ^
                Some(TokenContext::BraceExpr) => return false,
                _ => {}
            };
        }

        match prev {
            //  function a() {
            //      return { a: "" };
            //  }
            //  function a() {
            //      return
            //      {
            //          function b(){}
            //      };
            //  }
            Token::Return | Token::Yield => {
                return had_line_break;
            }

            Token::Else | Token::Semi | Token::Eof | Token::RParen => {
                return true;
            }

            // If previous token was `{`
            Token::LBrace => return self.current() == Some(TokenContext::BraceStmt),

            // `class C<T> { ... }`
            Token::Lt | Token::Gt => return true,
            _ => {}
        }

        !is_expr_allowed
    }

    fn len(&self) -> usize {
        self.0.len()
    }
    fn pop(&mut self) -> Option<TokenContext> {
        self.0.pop()
    }
    pub fn current(&self) -> Option<TokenContext> {
        self.0.last().copied()
    }
    fn push(&mut self, t: TokenContext) {
        self.0.push(t);
    }
}

#[derive(Clone)]
pub struct State {
    /// Whether a line break exists between previous token and new token.
    pub had_line_break: bool,

    pub token_type: Token,
    pub token_data: Option<TokenData>,

    // TODO: would be nice to remove this:
    // The context stack is used to superficially track syntactic
    // context to predict whether a regular expression is allowed in a
    // given position.
    pub context: TokenContexts,
    pub is_expr_allowed: bool,
}

impl State {
    pub fn new() -> Self {
        Self {
            had_line_break: true,
            token_type: Token::Eof,
            token_data: None,

            context: TokenContexts(vec![TokenContext::BraceStmt]),
            is_expr_allowed: true,
        }
    }

    pub fn can_skip_space(&self) -> bool {
        !self
            .context
            .current()
            .map(|t| t.preserve_space())
            .unwrap_or(false)
    }

    pub fn last_was_tpl_element(&self) -> bool {
        self.token_type == Token::Template
    }

    pub fn update(&mut self, start: BytePos, next: Token) {
        let prev = std::mem::replace(&mut self.token_type, next);

        self.is_expr_allowed = Self::is_expr_allowed_on_next(
            &mut self.context,
            // self.syntax,
            prev,
            start,
            next,
            self.had_line_break,
            self.is_expr_allowed,
        );
    }

    /// `is_expr_allowed`: previous value.
    /// `start`: start of newly produced token.
    fn is_expr_allowed_on_next(
        context: &mut TokenContexts,
        // syntax: Syntax,
        prev: Token,
        start: BytePos,
        next: Token,
        had_line_break: bool,
        is_expr_allowed: bool,
    ) -> bool {
        let is_next_keyword = next.is_keyword();

        if is_next_keyword && prev == Token::Dot {
            false
        } else {
            if next == tok!(')') || next == tok!('}') {
                // TODO: Verify
                if context.len() == 1 {
                    return true;
                }

                let out = context.pop().unwrap();

                // let a = function(){}
                if out == TokenContext::BraceStmt && context.current() == Some(TokenContext::FnExpr)
                {
                    context.pop();
                    return false;
                }

                // ${} in template
                if out == TokenContext::TplQuasi {
                    match context.current() {
                        Some(TokenContext::Tpl { .. }) => return false,
                        _ => return true,
                    }
                }

                // expression cannot follow expression
                !out.is_expr()
            } else if next == tok!("function") {
                // This is required to lex
                // `x = function(){}/42/i`
                if is_expr_allowed && !context.is_brace_block(prev, had_line_break, is_expr_allowed)
                {
                    context.push(TokenContext::FnExpr);
                }
                false
            } else if next == tok!("of")
                && Some(TokenContext::ParenStmt { is_for_loop: true }) == context.current()
            {
                // e.g. for (a of _) => true
                !prev.before_expr()
            } else if next == Token::Ident || next.is_known_ident() {
                // variable declaration
                match prev {
                    // handle automatic semicolon insertion.
                    Token::Let | Token::Const | Token::Var if had_line_break => true,
                    _ => false,
                }
            } else if next == tok!('{') {
                let next_ctxt = if context.is_brace_block(prev, had_line_break, is_expr_allowed) {
                    TokenContext::BraceStmt
                } else {
                    TokenContext::BraceExpr
                };
                context.push(next_ctxt);

                true
            } else if next == tok!("${") {
                context.push(TokenContext::TplQuasi);
                true
            } else if next == tok!('(') {
                // if, for, with, while is statement

                context.push(match prev {
                    Token::If | Token::With | Token::While => {
                        TokenContext::ParenStmt { is_for_loop: false }
                    }
                    Token::For => TokenContext::ParenStmt { is_for_loop: true },
                    _ => TokenContext::ParenExpr,
                });
                true
            } else if next == tok!("++") || next == tok!("--") {
                is_expr_allowed
            } else if next == tok!('`') {
                // If we are in template, ` terminates template.
                if let Some(TokenContext::Tpl { .. }) = context.current() {
                    context.pop();
                } else {
                    context.push(TokenContext::Tpl { start });
                }
                false
            } else {
                next.before_expr()
            }
        }
    }
}
