use super::{Dispatch, LexResult, Lexer, DISPATCHER};
use crate::{
    error::{Error, SyntaxError},
    Tokens,
};
use global_common::{chars::char_literals, BytePos, Pos, Span, SyntaxContext};

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

// Checks if the char is a whitespace char that spans multiple utf8 bytes.
// https://tc39.github.io/ecma262/#sec-white-space
pub fn is_multi_byte_whitespace(ch: char) -> bool {
    matches!(
        ch,
        char_literals::NON_BREAKING_SPACE
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

// Checks if the byte is the first utf8 byte of a unicode whitespace char.
// Used for short circuiting whitespace checks.
fn is_unicode_whitespace_start(byte: u8) -> bool {
    matches!(
        byte,
        0xC2 //   NBSP
        | 0xEF // BOM
        | 0xE1 // Ogham space mark
        | 0xE2 // En quad .. Hair space, narrow no break space, mathematical space
        | 0xE3 // Ideographic space
    )
}

pub mod char_bytes {
    pub const LINE_FEED: u8 = 10;
    pub const CARRIAGE_RETURN: u8 = 13;
}

pub fn is_valid_regex_flag(ch: char) -> bool {
    matches!(ch, 'g' | 'm' | 's' | 'i' | 'y' | 'u')
}

impl Lexer<'_> {
    /// Returns the remaining portion of the input as a str.
    #[inline(always)]
    fn as_str(&self) -> &str {
        // debug_assert!(unsafe { std::str::from_utf8(self.bytes.get_unchecked(self.cur..)).is_ok() });

        // Safety: We know this is safe because we require the input to the lexer
        // to be valid utf8 and cur always points to a character boundary.
        unsafe { std::str::from_utf8_unchecked(self.bytes.get_unchecked(self.cur..)) }
    }

    /// Gets the current char in the input.
    #[inline]
    pub(super) fn cur(&self) -> Option<char> {
        self.as_str().chars().next()
    }

    /// Gets the current char in the input without checking if it exists.
    /// It is undefined behaviour to call this at the end of the input.
    #[inline]
    pub(super) fn cur_unchecked(&self) -> char {
        debug_assert!(self.cur().is_some());

        match self.cur() {
            Some(c) => c,
            None => {
                // Safety: the caller is required to only call this when they
                // know we are not at the end of the input. This means that
                // there will always be a least one byte remaining, and since
                // all strings must be valid UTF-8, the remaining byte(s) will
                // always constitute a valid character.
                unsafe {
                    core::hint::unreachable_unchecked();
                }
            }
        }
    }

    // Get the next char and advance the cursor.
    #[inline]
    pub(super) fn next_char(&mut self) -> char {
        debug_assert!(self.cur().is_some());

        let char = self.cur_unchecked();
        self.cur += char.len_utf8();
        char
    }

    #[inline]
    pub(super) fn cur_byte(&mut self) -> Option<u8> {
        self.bytes.get(self.cur).copied()
    }

    #[inline]
    pub(super) fn prev_byte(&mut self) -> Option<u8> {
        self.bytes.get(self.cur - 1).copied()
    }

    #[inline]
    pub(super) fn peek_nth(&self, n: usize) -> Option<u8> {
        self.bytes.get(self.cur + n).copied()
    }

    #[inline]
    pub(super) fn advance(&mut self, amount: usize) {
        self.cur += amount;
    }

    #[inline]
    pub(super) fn bump(&mut self) {
        debug_assert!(self.cur().is_some());

        let char = self.cur_unchecked();

        self.cur += char.len_utf8();
    }

    #[inline]
    pub(super) fn is_at_start(&self) -> bool {
        self.cur_pos() == self.start_pos
    }

    #[inline]
    pub(super) fn cur_pos(&self) -> BytePos {
        self.start_pos + BytePos::from_usize(self.cur)
    }

    #[inline]
    pub(super) fn slice_to_cur(&self, start: BytePos) -> &str {
        std::str::from_utf8(&self.bytes[start.to_usize()..self.cur]).unwrap()
    }

    pub(super) fn uncons_while_chars<F>(&mut self, mut pred: F) -> &str
    where
        F: FnMut(char) -> bool,
    {
        // debug_assert!(unsafe { std::str::from_utf8(self.bytes.get_unchecked(self.cur..)).is_ok() });

        // Safety: We know this is safe because we require the input to the lexer
        // to be valid utf8 and cur always points to a character boundary.
        let s = unsafe { std::str::from_utf8_unchecked(self.bytes.get_unchecked(self.cur..)) };

        let mut last = 0;

        for c in s.chars() {
            if pred(c) {
                last += c.len_utf8();
            } else {
                break;
            }
        }
        let ret = &s[..last];

        self.cur += last;

        ret
    }

    pub(super) fn uncons_while_byte<F>(&mut self, mut pred: F) -> &str
    where
        F: FnMut(u8) -> bool,
    {
        let start = self.cur;

        for &b in self.bytes[self.cur..].iter() {
            if pred(b) {
                self.cur += 1;
            } else {
                break;
            }
        }

        // debug_assert!(unsafe {
        //     std::str::from_utf8(self.bytes.get_unchecked(start..self.cur)).is_ok()
        // });

        unsafe { std::str::from_utf8_unchecked(self.bytes.get_unchecked(start..self.cur)) }
    }

    #[inline]
    pub(super) fn reset_to(&mut self, to: BytePos) {
        self.cur = to.to_usize();
    }

    #[inline]
    pub(super) fn is(&self, c: u8) -> bool {
        matches!(self.bytes.get(self.cur).copied(),Some(ch) if ch == c)
    }

    #[inline]
    pub(super) fn eat(&mut self, c: u8) -> bool {
        if self.is(c) {
            self.cur += 1;
            true
        } else {
            false
        }
    }
}

impl Lexer<'_> {
    pub(super) fn span(&self, start: BytePos) -> Span {
        let end = self.cur_pos();
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
        if self.ctx.is_strict() {
            self.emit_error_span(span, kind);
            return;
        }

        let err = Error {
            error: Box::new((span, kind)),
        };

        self.add_strict_mode_error(err);
    }

    #[cold]
    #[inline(never)]
    pub(super) fn emit_module_mode_error(&mut self, start: BytePos, kind: SyntaxError) {
        let span = self.span(start);
        self.emit_module_mode_error_span(Span::new(span.lo, span.hi, span.ctxt), kind)
    }

    /// Some code is valid in a strict mode script but invalid in a module.
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

        debug_assert!(self.is(b'/'));
        debug_assert_eq!(self.peek_nth(1), Some(b'*'));

        self.advance(2); // "/*"

        while let Some(ch) = self.cur() {
            if ch == '*' && self.peek_nth(1) == Some(b'/') {
                self.advance(2); // "*/"
                return Ok(());
            }

            if is_line_break(ch) {
                self.state.had_line_break = true;
            }

            self.bump();
        }

        self.error(start, SyntaxError::UnterminatedBlockComment)?
    }

    pub(super) fn skip_line_comment(&mut self, start_skip_bytes: usize) {
        self.advance(start_skip_bytes);

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
        while let Some(byte) = self.cur_byte() {
            match DISPATCHER[byte as usize] {
                Dispatch::WHS => {
                    if byte == char_bytes::LINE_FEED || byte == char_bytes::CARRIAGE_RETURN {
                        self.state.had_line_break = true;
                    }
                    self.advance(1);
                }
                Dispatch::SLH => match self.peek_nth(1) {
                    Some(b'/') => {
                        self.skip_line_comment(2);
                    }
                    Some(b'*') => {
                        self.skip_block_comment()?;
                    }
                    _ => return Ok(()),
                },
                Dispatch::UNI => {
                    // Try to short circuit the branch by checking the first
                    // byte of the potential unicode space.
                    if byte > 0xC1 && is_unicode_whitespace_start(byte) {
                        let ch = self.cur_unchecked();

                        if ch == char_literals::LINE_SEPARATOR
                            || ch == char_literals::PARAGRAPH_SEPARATOR
                        {
                            self.state.had_line_break = true;
                            self.cur += ch.len_utf8();
                        } else if is_multi_byte_whitespace(ch) {
                            self.cur += ch.len_utf8();
                        } else {
                            return Ok(());
                        }
                    } else {
                        return Ok(());
                    }
                }
                _ => return Ok(()),
            }
        }

        Ok(())
    }
}
