use crate::{
    parser::Parser,
    token::{Token, TokenAndSpan},
};
use global_common::{BytePos, Span, DUMMY_SP};
use std::vec::IntoIter;

/// This struct is responsible for managing current token and peeked token.
pub struct Buffer {
    iter: <Vec<TokenAndSpan> as IntoIterator>::IntoIter,
    /// Span of the previous token.
    prev_span: Span,
    cur: Option<TokenAndSpan>,
    /// Peeked token
    next: Option<TokenAndSpan>,
}

impl Parser {
    pub fn input(&mut self) -> &mut IntoIter<TokenAndSpan> {
        &mut self.input.iter
    }
}

impl Buffer {
    pub fn new(tokens: Vec<TokenAndSpan>) -> Self {
        Buffer {
            iter: tokens.into_iter(),
            cur: None,
            prev_span: DUMMY_SP,
            next: None,
        }
    }

    #[inline]
    fn bump_inner(&mut self) -> Option<Token> {
        let prev = self.cur.take();
        self.prev_span = match prev {
            Some(TokenAndSpan { span, .. }) => span,
            _ => self.prev_span,
        };

        // If we have peeked a token, take it instead of calling iter.next()
        self.cur = self.next.take().or_else(|| self.iter.next());

        prev.map(|it| it.token)
    }

    #[cold]
    #[inline(never)]
    pub fn dump_cur(&mut self) -> String {
        match self.cur() {
            Some(v) => format!("{:?}", v),
            None => "<eof>".to_string(),
        }
    }

    /// Returns current token.
    pub fn bump(&mut self) -> Token {
        #[cold]
        #[inline(never)]
        fn invalid_state() -> ! {
            unreachable!(
                "Current token is `None`. Parser should not call bump() without knowing current \
                 token"
            )
        }

        let prev = match self.cur.take() {
            Some(t) => t,
            None => invalid_state(),
        };
        self.prev_span = prev.span;

        prev.token
    }

    pub fn knows_cur(&self) -> bool {
        self.cur.is_some()
    }

    pub fn peek(&mut self) -> Option<&Token> {
        debug_assert!(
            self.cur.is_some(),
            "parser should not call peek() without knowing current token"
        );

        if self.next.is_none() {
            self.next = self.iter.next();
        }

        self.next.as_ref().map(|ts| &ts.token)
    }

    // / Returns true on eof.
    pub fn had_line_break_before_cur(&mut self) -> bool {
        self.cur();

        self.cur
            .as_ref()
            .map(|it| it.had_line_break)
            .unwrap_or_else(|| true)
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

    /// Get current token. Returns `None` only on eof.
    #[inline]
    pub fn cur(&mut self) -> Option<&Token> {
        if self.cur.is_none() {
            self.bump_inner();
        }
        self.cur.as_ref().map(|item| &item.token)
    }

    #[inline]
    pub fn is(&mut self, expected: &Token) -> bool {
        match self.cur() {
            Some(t) => *expected == *t,
            _ => false,
        }
    }

    #[inline]
    pub fn peeked_is(&mut self, expected: &Token) -> bool {
        match self.peek() {
            Some(t) => *expected == *t,
            _ => false,
        }
    }

    #[inline]
    pub fn eat(&mut self, expected: &Token) -> bool {
        let v = self.is(expected);
        if v {
            self.bump();
        }
        v
    }

    /// Returns start of current token.
    #[inline]
    pub fn cur_pos(&mut self) -> BytePos {
        let _ = self.cur();
        self.cur
            .as_ref()
            .map(|item| item.span.lo)
            .unwrap_or_else(|| {
                // eof
                self.last_pos()
            })
    }

    #[inline]
    pub fn cur_span(&self) -> Span {
        let data = self
            .cur
            .as_ref()
            .map(|item| item.span)
            .unwrap_or(self.prev_span);

        Span::new(data.lo, data.hi, data.ctxt)
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
}
