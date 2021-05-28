use super::Lexer;
use crate::{context::Context, error::Error, token::*, JscTarget, Tokens};
use enum_kind::Kind;
use global_common::{input::Input, BytePos};
use std::mem::take;

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
    JSXName,
    JSXText,
    JSXTagStart,
    JSXTagEnd,
    Other {
        before_expr: bool,
        can_have_trailing_comment: bool,
    },
}
impl TokenType {
    pub fn before_expr(self) -> bool {
        match self {
            TokenType::JSXName
            | TokenType::JSXTagStart
            | TokenType::JSXTagEnd
            | TokenType::Template
            | TokenType::Dot
            | TokenType::RParen => false,

            TokenType::JSXText | TokenType::Colon | TokenType::LBrace | TokenType::Semi => true,
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
            Token::JSXTagEnd => TokenType::JSXTagEnd,
            Token::JSXTagStart => TokenType::JSXTagStart,
            Token::JSXText { .. } => TokenType::JSXText,
            Token::JSXName { .. } => TokenType::JSXName,
            Token::BinOp(op) => TokenType::BinOp(op),

            Token::Word(Word::Keyword(k)) => TokenType::Keyword(k),
            _ => TokenType::Other {
                before_expr: t.before_expr(),
                can_have_trailing_comment: match *t {
                    Token::Num(..)
                    | Token::Str { .. }
                    | Token::Word(Word::Ident(..))
                    | Token::DollarLBrace
                    | Token::Regex(..)
                    | Token::BigInt(..)
                    | Token::JSXText { .. }
                    | Token::RBrace => true,

                    _ => false,
                },
            },
        }
    }
}

impl<I: Input> Tokens for Lexer<I> {
    fn set_ctx(&mut self, ctx: Context) {
        if ctx.module && !self.module_errors.borrow().is_empty() {
            let mut module_errors = self.module_errors.borrow_mut();
            self.errors.borrow_mut().append(&mut *module_errors);
        }
        self.ctx = ctx
    }

    fn ctx(&self) -> Context {
        self.ctx
    }

    // fn syntax(&self) -> Syntax {
    //     self.syntax
    // }
    fn target(&self) -> JscTarget {
        self.target
    }

    fn set_expr_allowed(&mut self, allow: bool) {
        self.set_expr_allowed(allow)
    }

    fn token_context(&self) -> &TokenContexts {
        &self.state.context
    }
    fn token_context_mut(&mut self) -> &mut TokenContexts {
        &mut self.state.context
    }

    fn set_token_context(&mut self, c: TokenContexts) {
        self.state.context = c;
    }

    fn add_error(&self, error: Error) {
        self.errors.borrow_mut().push(error);
    }

    fn take_errors(&mut self) -> Vec<Error> {
        take(&mut self.errors.borrow_mut())
    }

    fn add_module_mode_error(&self, error: Error) {
        if self.ctx.module {
            self.add_error(error);
            return;
        }
        self.module_errors.borrow_mut().push(error);
    }
}

/// The algorithm used to determine whether a regexp can appear at a
/// given point in the program is loosely based on sweet.js' approach.
/// See https://github.com/mozilla/sweet.js/wiki/design
#[derive(Debug, Clone, Copy, PartialEq, Eq, Kind)]
#[kind(function(is_expr = "bool", preserve_space = "bool"))]
pub enum TokenContext {
    BraceStmt,
    #[kind(is_expr)]
    BraceExpr,
    #[kind(is_expr)]
    TplQuasi,
    ParenStmt {
        /// Is this `for` loop?
        is_for_loop: bool,
    },
    #[kind(is_expr)]
    ParenExpr,
    #[kind(is_expr, preserve_space)]
    Tpl {
        /// Start of a template literal.
        start: BytePos,
    },
    #[kind(is_expr)]
    FnExpr,
    JSXOpeningTag,
    JSXClosingTag,
    #[kind(is_expr, preserve_space)]
    JSXExpr,
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

    pub token_type: Option<TokenType>,

    pub start: BytePos,

    // Position information for the previous token
    pub last_tok_end: BytePos,

    // The context stack is used to superficially track syntactic
    // context to predict whether a regular expression is allowed in a
    // given position.
    pub context: TokenContexts,
    pub is_expr_allowed: bool,

    // Used to signal to callers of `read_word` whether the word
    // contained any escape sequences. This is needed because words with
    // escape sequences must not be interpreted as keywords.
    pub contains_esc: bool,
}

impl State {
    pub fn new() -> Self {
        Self {
            had_line_break: false,
            token_type: None,
            start: BytePos(0),

            last_tok_end: BytePos(0),
            context: TokenContexts(vec![TokenContext::BraceStmt]),
            is_expr_allowed: true,
            contains_esc: false,
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
        match self.token_type {
            Some(TokenType::Template) => true,
            _ => false,
        }
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
        let is_next_keyword = match *next {
            Word(Word::Keyword(..)) => true,
            _ => false,
        };

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

                // tok!('/') if syntax.jsx() && prev == Some(TokenType::JSXTagStart) => {
                //     context.pop();
                //     context.pop(); // do not consider JSX expr -> JSX open tag -> ... anymore
                //     context.push(TokenContext::JSXClosingTag); // reconsider as closing tag context
                //     false
                // }
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
