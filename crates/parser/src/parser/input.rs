use crate::{
    JscTarget, Syntax,
    context::Context,
    lexer::{Lexer, LexerCheckpoint, TokenContexts},
    parser::Parser,
    token::{Token, TokenAndSpan},
};
use ast::ProgramData;
use common::{BytePos, Span};

/// This struct is responsible for managing current token and peeked token.
pub struct Buffer<'src> {
    iter: Lexer<'src>,
    /// Span of the previous token.
    prev_span: Span,
    pub cur: TokenAndSpan,
    /// Peeked token
    next: Option<TokenAndSpan>,
}

impl<'d> Parser<'d> {
    pub fn input(&mut self) -> &mut Lexer<'d> {
        &mut self.input.iter
    }
}

pub struct BufferCheckpoint {
    lexer_cp: LexerCheckpoint,
    prev_span: Span,
    cur: TokenAndSpan,
    next: Option<TokenAndSpan>,
}

impl<'d> Buffer<'d> {
    pub fn checkpoint(&self) -> BufferCheckpoint {
        let Buffer {
            iter,
            prev_span,
            cur,
            next,
        } = self;

        BufferCheckpoint {
            lexer_cp: iter.checkpoint(),
            prev_span: *prev_span,
            cur: cur.clone(),
            next: next.clone(),
        }
    }

    pub fn rewind(&mut self, checkpoint: BufferCheckpoint) {
        let BufferCheckpoint {
            lexer_cp,
            prev_span,
            cur,
            next,
        } = checkpoint;

        self.iter.rewind(lexer_cp);
        self.prev_span = prev_span;
        self.cur = cur;
        self.next = next;
    }

    pub fn new(lexer: Lexer<'d>) -> Self {
        let start_pos = lexer.start_pos();
        let prev_span = Span::new(start_pos, start_pos);
        Buffer {
            iter: lexer,
            cur: TokenAndSpan {
                token: Token::Eof,
                had_line_break: false,
                span: prev_span,
            },
            prev_span,
            next: None,
        }
    }

    pub fn store(&mut self, token: Token) {
        debug_assert!(self.next.is_none());
        debug_assert!(self.cur.token != Token::Eof);
        let span = self.prev_span;

        self.cur = TokenAndSpan {
            span,
            token,
            had_line_break: false,
        };
    }

    #[cold]
    #[inline(never)]
    pub fn dump_cur(&mut self) -> String {
        format!("{:?}", self.cur.token)
    }

    pub fn first_bump(&mut self) {
        let first_token = self.iter.first_token();
        self.prev_span = self.cur.span;
        self.cur = first_token;
    }

    /// Returns current token.
    pub fn bump(&mut self) -> Token {
        let next = if self.next.is_none() {
            self.iter.next_token()
        } else {
            let Some(next) = self.next.take() else {
                unreachable!();
            };
            next
        };
        let prev = std::mem::replace(&mut self.cur, next);
        self.prev_span = prev.span;
        prev.token
    }

    pub fn peek(&mut self) -> Option<&Token> {
        debug_assert!(
            self.cur.token != Token::Eof,
            "parser should not call peek() without knowing current token"
        );

        if self.next.is_none() {
            self.next = Some(self.iter.next_token());
        }

        self.next.as_ref().map(|ts| &ts.token)
    }

    /// Returns true on eof.
    pub fn had_line_break_before_cur(&mut self) -> bool {
        self.cur.had_line_break
    }

    /// This returns true on eof.
    pub fn has_linebreak_between_cur_and_peeked(&mut self) -> bool {
        let _ = self.peek();
        self.next
            .as_ref()
            .map(|item| item.had_line_break)
            .unwrap_or({
                // return true on eof.
                true
            })
    }

    #[inline]
    pub fn cur(&mut self) -> &Token {
        &self.cur.token
    }

    #[inline]
    pub fn is(&mut self, expected: Token) -> bool {
        self.cur.token == expected
    }

    #[inline]
    pub fn peeked_is(&mut self, expected: Token) -> bool {
        match self.peek() {
            Some(t) => expected == *t,
            _ => false,
        }
    }

    #[inline]
    pub fn eat(&mut self, expected: Token) -> bool {
        let v = self.is(expected);
        if v {
            self.bump();
        }
        v
    }

    /// Returns start of current token.
    #[inline]
    pub fn cur_pos(&mut self) -> BytePos {
        self.cur.span.lo
    }

    #[inline]
    pub fn cur_span(&self) -> Span {
        self.cur.span
    }

    /// Returns last byte position of previous token.
    #[inline]
    pub fn last_pos(&self) -> BytePos {
        self.prev_span.hi
    }

    /// Returns span of the previous token.
    #[inline]
    pub fn prev_span(&self) -> Span {
        self.prev_span
    }

    #[inline]
    pub(crate) fn get_ctx(&self) -> Context {
        self.iter.ctx()
    }

    #[inline]
    pub(crate) fn set_ctx(&mut self, ctx: Context) {
        self.iter.set_ctx(ctx);
    }

    #[inline]
    pub fn syntax(&self) -> Syntax {
        self.iter.syntax()
    }
    #[inline]
    pub fn target(&self) -> JscTarget {
        self.iter.target()
    }
    /// Converts buffered strict mode errors into module errors.
    #[inline]
    pub(crate) fn convert_strict_mode_errors_to_module_errors(&mut self) {
        self.iter.convert_strict_mode_errors_to_module_errors();
    }

    #[inline]
    pub(crate) fn set_expr_allowed(&mut self, allow: bool) {
        self.iter.set_expr_allowed(allow);
    }

    #[inline]
    pub(crate) fn token_context(&self) -> &TokenContexts {
        self.iter.token_context()
    }
    #[inline]
    pub(crate) fn set_token_context(&mut self, c: TokenContexts) {
        self.iter.set_token_context(c);
    }

    pub(crate) fn program_data(&self) -> &ProgramData {
        self.iter.program_data
    }
    pub(crate) fn program_data_mut(&mut self) -> &mut ProgramData {
        self.iter.program_data
    }
}
