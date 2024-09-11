use super::Lexer;
use crate::{
    context::{Context, YesMaybe, YesNoMaybe},
    error::Error,
    token::*,
    JscTarget, Syntax, Tokens,
};
use global_common::BytePos;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    Template,
    Dot,
    Colon,
    LBrace,
    RParen,
    Semi,
    BinOp(BinOpToken),
    Keyword(Keyword),
    Other {
        before_expr: bool,
        can_have_trailing_comment: bool,
    },
}
impl TokenType {
    pub fn before_expr(self) -> bool {
        match self {
            TokenType::Template | TokenType::Dot | TokenType::RParen => false,

            TokenType::Colon | TokenType::LBrace | TokenType::Semi => true,
            TokenType::BinOp(b) => b.before_expr(),
            TokenType::Keyword(k) => k.before_expr(),
            TokenType::Other { before_expr, .. } => before_expr,
        }
    }
}

impl From<&Token> for TokenType {
    fn from(t: &Token) -> Self {
        match *t {
            Token::Template { .. } => TokenType::Template,
            Token::Dot => TokenType::Dot,
            Token::Colon => TokenType::Colon,
            Token::LBrace => TokenType::LBrace,
            Token::RParen => TokenType::RParen,
            Token::Semi => TokenType::Semi,
            Token::BinOp(op) => TokenType::BinOp(op),

            Token::Word(Word::Keyword(k)) => TokenType::Keyword(k),
            _ => TokenType::Other {
                before_expr: t.before_expr(),
                can_have_trailing_comment: matches!(
                    *t,
                    Token::Num { .. }
                        | Token::Str { .. }
                        | Token::Word(Word::Ident(..))
                        | Token::DollarLBrace
                        | Token::Regex(..)
                        | Token::BigInt(..)
                        | Token::RBrace
                ),
            },
        }
    }
}

impl Tokens for Lexer<'_> {
    fn set_ctx(&mut self, ctx: Context) {
        if ctx.is_module() && !self.module_errors.borrow().is_empty() {
            let mut module_errors = self.module_errors.borrow_mut();
            self.errors.borrow_mut().append(&mut *module_errors);
        }

        if ctx.is_strict() && !self.strict_errors.borrow().is_empty() {
            let mut strict_errors = self.strict_errors.borrow_mut();
            self.errors.borrow_mut().append(&mut *strict_errors);
        }

        self.ctx = ctx
    }

    fn ctx(&self) -> Context {
        self.ctx
    }

    fn syntax(&self) -> Syntax {
        self.syntax
    }
    fn target(&self) -> JscTarget {
        self.target
    }

    fn set_expr_allowed(&mut self, allow: bool) {
        self.set_expr_allowed(allow)
    }

    fn token_context(&self) -> &TokenContexts {
        &self.state.context
    }

    fn set_token_context(&mut self, c: TokenContexts) {
        self.state.context = c;
    }

    fn add_error(&self, error: Error) {
        self.errors.borrow_mut().push(error);
    }

    fn take_errors(&mut self) -> Vec<Error> {
        std::mem::take(&mut self.errors.borrow_mut())
    }

    fn add_module_mode_error(&self, error: Error) {
        match self.ctx.module {
            YesNoMaybe::Yes => {
                // Definitely in a module, immediately add error.
                self.add_error(error);
            }
            YesNoMaybe::No => {
                // Definitely not in a module, discard error.
            }
            YesNoMaybe::Maybe => {
                // Not yet sure if we are in a module, buffer error.
                self.module_errors.borrow_mut().push(error);
            }
        }
    }

    fn add_strict_mode_error(&self, error: Error) {
        match self.ctx.strict {
            YesMaybe::Yes => {
                // Definitely in strict mode, immediately add error.
                self.add_error(error);
            }
            YesMaybe::Maybe => {
                // Not yet sure if we are in strict mode, buffer error.
                self.strict_errors.borrow_mut().push(error);
            }
        }
    }

    fn convert_strict_mode_errors_to_module_errors(&mut self) {
        // Even once we have stopped parsing directives, we still can not be
        // certain of strict mode because we may later discover that we are
        // paring a module, which requires us to reinterpret the code using
        // strict mode. Therefore, rather than discarding any trapped strict
        // mode errors, we convert them into module errors. The logic above, in
        // add_module_mode_error, will decide whether to discard, buffer, or add
        // the error to the main error buffer, depending on if we are certain
        // whether we are paring a module or not.

        let mut strict_errors = self.strict_errors.borrow_mut();

        for error in strict_errors.drain(..) {
            self.add_module_mode_error(error);
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
    fn is_brace_block(
        &self,
        prev: Option<TokenType>,
        had_line_break: bool,
        is_expr_allowed: bool,
    ) -> bool {
        if let Some(TokenType::Colon) = prev {
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
            Some(TokenType::Keyword(Return)) | Some(TokenType::Keyword(Yield)) => {
                return had_line_break;
            }

            Some(TokenType::Keyword(Else))
            | Some(TokenType::Semi)
            | None
            | Some(TokenType::RParen) => {
                return true;
            }

            // If previous token was `{`
            Some(TokenType::LBrace) => return self.current() == Some(TokenContext::BraceStmt),

            // `class C<T> { ... }`
            Some(TokenType::BinOp(Lt)) | Some(TokenType::BinOp(Gt)) => return true,
            _ => {}
        }

        !is_expr_allowed
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn pop(&mut self) -> Option<TokenContext> {
        self.0.pop()
    }
    pub fn current(&self) -> Option<TokenContext> {
        self.0.last().cloned()
    }
    fn push(&mut self, t: TokenContext) {
        self.0.push(t);
    }
}

#[derive(Clone)]
pub struct State {
    /// Whether a line break exists between previous token and new token.
    pub had_line_break: bool,

    token_type: Option<TokenType>,

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
            token_type: None,

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
        matches!(self.token_type, Some(TokenType::Template))
    }

    pub fn update(&mut self, start: BytePos, next: &Token) {
        let prev = self.token_type.take();
        self.token_type = Some(TokenType::from(next));

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
        prev: Option<TokenType>,
        start: BytePos,
        next: &Token,
        had_line_break: bool,
        is_expr_allowed: bool,
    ) -> bool {
        let is_next_keyword = matches!(*next, Word(Word::Keyword(..)));

        if is_next_keyword && prev == Some(TokenType::Dot) {
            false
        } else {
            match *next {
                tok!(')') | tok!('}') => {
                    // TODO(swc): Verify
                    if context.len() == 1 {
                        return true;
                    }

                    let out = context.pop().unwrap();

                    // let a = function(){}
                    if out == TokenContext::BraceStmt
                        && context.current() == Some(TokenContext::FnExpr)
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
                }

                tok!("function") => {
                    // This is required to lex
                    // `x = function(){}/42/i`
                    if is_expr_allowed
                        && !context.is_brace_block(prev, had_line_break, is_expr_allowed)
                    {
                        context.push(TokenContext::FnExpr);
                    }
                    false
                }

                // for (a of b) {}
                tok!("of")
                    if Some(TokenContext::ParenStmt { is_for_loop: true }) == context.current() =>
                {
                    // e.g. for (a of _) => true
                    !prev
                        .expect("context.current() if ParenStmt, so prev token cannot be None")
                        .before_expr()
                }

                Word(Word::Ident(..)) => {
                    // variable declaration
                    match prev {
                        Some(prev) => match prev {
                            // handle automatic semicolon insertion.
                            TokenType::Keyword(Let)
                            | TokenType::Keyword(Const)
                            | TokenType::Keyword(Var)
                                if had_line_break =>
                            {
                                true
                            }
                            _ => false,
                        },
                        _ => false,
                    }
                }

                tok!('{') => {
                    let next_ctxt = if context.is_brace_block(prev, had_line_break, is_expr_allowed)
                    {
                        TokenContext::BraceStmt
                    } else {
                        TokenContext::BraceExpr
                    };
                    context.push(next_ctxt);

                    true
                }

                tok!("${") => {
                    context.push(TokenContext::TplQuasi);
                    true
                }

                tok!('(') => {
                    // if, for, with, while is statement

                    context.push(match prev {
                        Some(TokenType::Keyword(k)) => match k {
                            If | With | While => TokenContext::ParenStmt { is_for_loop: false },
                            For => TokenContext::ParenStmt { is_for_loop: true },
                            _ => TokenContext::ParenExpr,
                        },
                        _ => TokenContext::ParenExpr,
                    });
                    true
                }

                // remains unchanged.
                tok!("++") | tok!("--") => is_expr_allowed,

                tok!('`') => {
                    // If we are in template, ` terminates template.
                    if let Some(TokenContext::Tpl { .. }) = context.current() {
                        context.pop();
                    } else {
                        context.push(TokenContext::Tpl { start });
                    }
                    false
                }

                _ => next.before_expr(),
            }
        }
    }
}
