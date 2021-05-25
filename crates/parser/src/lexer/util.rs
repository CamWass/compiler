use super::{LexResult, Lexer};
use crate::{
    error::{Error, SyntaxError},
    Tokens,
};
use global_common::{input::Input, BytePos, Span, SyntaxContext};

/// See https://tc39.github.io/ecma262/#sec-line-terminators
pub fn is_line_break(ch: char) -> bool {
    matches!(
        ch,
        char_literals::LINE_FEED
            | char_literals::CARRIAGE_RETURN
            | char_literals::LINE_SEPARATOR
            | char_literals::PARAGRAPH_SEPARATOR
    )
}

// https://tc39.github.io/ecma262/#sec-white-space
pub fn is_whitespace(ch: char) -> bool {
    matches!(
        ch,
        char_literals::CHARACTER_TABULATION
            | char_literals::LINE_TABULATION
            | char_literals::FORM_FEED
            | char_literals::SPACE
            | char_literals::NON_BREAKING_SPACE
            | char_literals::OGHAM_SPACE_MARK
            | char_literals::EN_QUAD
            | char_literals::EM_QUAD
            | char_literals::EN_SPACE
            | char_literals::EM_SPACE
            | char_literals::THREE_PER_EM_SPACE
            | char_literals::FOUR_PER_EM_SPACE
            | char_literals::SIX_PER_EM_SPACE
            | char_literals::FIGURE_SPACE
            | char_literals::PUNCTUATION_SPACE
            | char_literals::THIN_SPACE
            | char_literals::HAIR_SPACE
            | char_literals::NARROW_NO_BREAK_SPACE
            | char_literals::MEDIUM_MATHEMATICAL_SPACE
            | char_literals::IDEOGRAPHIC_SPACE
            | char_literals::ZERO_WIDTH_NO_BREAK_SPACE
    )
}

pub mod char_literals {
    pub const BACKSPACE: char = '\u{0008}';
    pub const SPACE: char = '\u{0020}';
    pub const LINE_FEED: char = '\u{000a}';
    pub const LINE_SEPARATOR: char = '\u{2028}';
    pub const CARRIAGE_RETURN: char = '\u{000d}';
    pub const FORM_FEED: char = '\u{000c}';
    pub const PARAGRAPH_SEPARATOR: char = '\u{2029}';
    pub const NON_BREAKING_SPACE: char = '\u{00a0}';
    pub const CHARACTER_TABULATION: char = '\u{0009}';
    pub const LINE_TABULATION: char = '\u{000b}';
    pub const OGHAM_SPACE_MARK: char = '\u{1680}';
    pub const EN_QUAD: char = '\u{2000}';
    pub const EM_QUAD: char = '\u{2001}';
    pub const EN_SPACE: char = '\u{2002}';
    pub const EM_SPACE: char = '\u{2003}';
    pub const THREE_PER_EM_SPACE: char = '\u{2004}';
    pub const FOUR_PER_EM_SPACE: char = '\u{2005}';
    pub const SIX_PER_EM_SPACE: char = '\u{2006}';
    pub const FIGURE_SPACE: char = '\u{2007}';
    pub const PUNCTUATION_SPACE: char = '\u{2008}';
    pub const THIN_SPACE: char = '\u{2009}';
    pub const HAIR_SPACE: char = '\u{200a}';
    pub const ZERO_WIDTH_NON_JOINER: char = '\u{200c}';
    pub const ZERO_WIDTH_JOINER: char = '\u{200d}';
    pub const NARROW_NO_BREAK_SPACE: char = '\u{202f}';
    pub const MEDIUM_MATHEMATICAL_SPACE: char = '\u{205f}';
    pub const IDEOGRAPHIC_SPACE: char = '\u{3000}';
    pub const ZERO_WIDTH_NO_BREAK_SPACE: char = '\u{feff}';
}

pub mod char_bytes {
    pub const LINE_FEED: u8 = 10;
}

pub fn is_valid_regex_flag(ch: char) -> bool {
    matches!(ch, 'g' | 'm' | 's' | 'i' | 'y' | 'u')
}

impl<I: Input> Lexer<I> {
    pub(super) fn span(&self, start: BytePos) -> Span {
        let end = self.last_pos();
        debug_assert!(
            start <= end,
            "assertion failed: (span.start <= span.end). start = {}, end = {}",
            start.0,
            end.0
        );

        Span {
            lo: start,
            hi: end,
            ctxt: SyntaxContext::empty(),
        }
    }

    pub(super) fn bump(&mut self) {
        self.input.bump()
    }

    pub(super) fn is(&mut self, c: u8) -> bool {
        self.input.is_byte(c)
    }

    pub(super) fn eat(&mut self, c: u8) -> bool {
        self.input.eat_byte(c)
    }

    pub(super) fn cur(&mut self) -> Option<char> {
        self.input.cur()
    }
    pub(super) fn peek(&mut self) -> Option<char> {
        self.input.peek()
    }
    pub(super) fn peek_ahead(&mut self) -> Option<char> {
        self.input.peek_ahead()
    }

    pub(super) fn cur_pos(&mut self) -> BytePos {
        self.input.cur_pos()
    }
    pub(super) fn last_pos(&self) -> BytePos {
        self.input.last_pos()
    }

    /// Shorthand for `let span = self.span(start); self.error_span(span)`
    #[cold]
    #[inline(never)]
    pub(super) fn error<T>(&mut self, start: BytePos, kind: SyntaxError) -> LexResult<T> {
        let span = self.span(start);
        self.error_span(Span::new(span.lo, span.hi, span.ctxt), kind)
    }

    #[cold]
    #[inline(never)]
    pub(super) fn error_span<T>(&mut self, span: Span, kind: SyntaxError) -> LexResult<T> {
        Err(Error {
            error: Box::new((span, kind)),
        })
    }

    #[cold]
    #[inline(never)]
    pub(super) fn emit_error(&mut self, start: BytePos, kind: SyntaxError) {
        let span = self.span(start);
        self.emit_error_span(Span::new(span.lo, span.hi, span.ctxt), kind)
    }

    #[cold]
    #[inline(never)]
    pub(super) fn emit_error_span(&mut self, span: Span, kind: SyntaxError) {
        let err = Error {
            error: Box::new((span, kind)),
        };
        self.errors.borrow_mut().push(err);
    }

    #[cold]
    #[inline(never)]
    pub(super) fn emit_strict_mode_error(&mut self, start: BytePos, kind: SyntaxError) {
        let span = self.span(start);
        self.emit_strict_mode_error_span(Span::new(span.lo, span.hi, span.ctxt), kind)
    }

    #[cold]
    #[inline(never)]
    pub(super) fn emit_strict_mode_error_span(&mut self, span: Span, kind: SyntaxError) {
        if self.ctx.strict {
            self.emit_error_span(span, kind);
            return;
        }

        let err = Error {
            error: Box::new((span, kind)),
        };

        self.add_module_mode_error(err);
    }

    #[cold]
    #[inline(never)]
    pub(super) fn emit_module_mode_error(&mut self, start: BytePos, kind: SyntaxError) {
        let span = self.span(start);
        self.emit_module_mode_error_span(Span::new(span.lo, span.hi, span.ctxt), kind)
    }

    /// Some codes are valid in a strict mode script  but invalid in module
    /// code.
    #[cold]
    #[inline(never)]
    pub(super) fn emit_module_mode_error_span(&mut self, span: Span, kind: SyntaxError) {
        let err = Error {
            error: Box::new((span, kind)),
        };

        self.add_module_mode_error(err);
    }

    /// Expects current char to be '/' and next char to be '*'.
    pub(super) fn skip_block_comment(&mut self) -> LexResult<()> {
        let start = self.cur_pos();

        debug_assert_eq!(self.cur(), Some('/'));
        debug_assert_eq!(self.peek(), Some('*'));

        self.bump();
        self.bump();

        while let Some(ch) = self.cur() {
            if ch == '*' && self.peek() == Some('/') {
                self.bump(); // '*'
                self.bump(); // '/'
                return Ok(());
            }

            if is_line_break(ch) {
                self.state.had_line_break = true;
            }

            self.bump();
        }

        self.error(start, SyntaxError::UnterminatedBlockComment)?
    }

    pub(super) fn skip_line_comment(&mut self, start_skip: usize) {
        for _ in 0..start_skip {
            self.bump();
        }

        while let Some(ch) = self.cur() {
            self.bump();
            if is_line_break(ch) {
                self.state.had_line_break = true;
                break;
            }
        }
    }

    /// Skip comments or whitespaces.
    ///
    /// See https://tc39.github.io/ecma262/#sec-white-space
    pub(super) fn skip_space(&mut self) -> LexResult<()> {
        while let Some(ch) = self.cur() {
            match ch {
                char_literals::SPACE
                | char_literals::NON_BREAKING_SPACE
                | char_literals::CHARACTER_TABULATION => {
                    self.bump();
                }

                char_literals::CARRIAGE_RETURN
                | char_literals::LINE_FEED
                | char_literals::LINE_SEPARATOR
                | char_literals::PARAGRAPH_SEPARATOR => {
                    self.bump();
                    self.state.had_line_break = true;
                }

                '/' => match self.input.peek() {
                    Some('/') => {
                        self.skip_line_comment(2);
                    }
                    Some('*') => {
                        self.skip_block_comment()?;
                    }
                    _ => return Ok(()),
                },

                _ => {
                    if is_whitespace(ch) {
                        self.bump();
                    } else {
                        return Ok(());
                    }
                }
            }
        }

        Ok(())
    }
}
