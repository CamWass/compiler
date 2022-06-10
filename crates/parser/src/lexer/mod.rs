pub mod identifier;
mod number;
mod state;
mod util;

use crate::{
    context::Context,
    error::{Error, SyntaxError},
    token::*,
    JscTarget, Syntax,
};
use ast::op;
use global_common::{chars::char_literals, BytePos, SourceFile, Span};
use identifier::{is_ident_part, is_ident_start};
use state::State;
pub use state::{TokenContext, TokenContexts};
use std::{cell::RefCell, iter::FusedIterator, rc::Rc};
use swc_atoms::JsWord;
use util::{char_bytes, is_line_break, is_valid_regex_flag};

pub(crate) type LexResult<T> = Result<T, Error>;

#[derive(Clone)]
pub struct Lexer<'src> {
    /// Index of current byte in `self.bytes`.
    cur: usize,
    bytes: &'src [u8],
    start_pos: BytePos,

    pub(crate) ctx: Context,
    state: State,
    pub(crate) syntax: Syntax,
    pub(crate) target: JscTarget,
    buf: String,

    errors: Rc<RefCell<Vec<Error>>>,
    module_errors: Rc<RefCell<Vec<Error>>>,
    strict_errors: Rc<RefCell<Vec<Error>>>,
}

impl FusedIterator for Lexer<'_> {}

impl Iterator for Lexer<'_> {
    type Item = TokenAndSpan;
    fn next(&mut self) -> Option<Self::Item> {
        let mut start = self.cur_pos();

        let res = (|| -> Result<Option<_>, _> {
            // Skip the space after the previous token, so that the next one's
            // `start` will point to the right position.
            if self.state.can_skip_space() {
                self.skip_space()?;
                start = self.cur_pos();
            };

            if let Some(TokenContext::Tpl {
                start: start_pos_of_tpl,
            }) = self.state.context.current()
            {
                return self.read_tmpl_token(start_pos_of_tpl).map(Some);
            }

            if self.syntax.typescript() && self.ctx.in_type {
                if self.eat(b'<') {
                    return Ok(Some(tok!('<')));
                } else if self.eat(b'>') {
                    return Ok(Some(tok!('>')));
                }
            }

            self.read_token()
        })();

        let token = match res.map_err(Token::Error).map_err(Some) {
            Ok(t) => t,
            Err(e) => e,
        };

        if let Some(token) = &token {
            self.state.update(start, token);
        }

        let had_line_break = self.state.had_line_break;
        self.state.had_line_break = false;

        token.map(|token| {
            // Attach span to token.
            TokenAndSpan {
                token,
                had_line_break,
                span: self.span(start),
            }
        })
    }
}

impl<'src> Lexer<'src> {
    pub fn new(syntax: Syntax, target: JscTarget, input: &'src SourceFile) -> Self {
        Lexer {
            cur: 0,
            bytes: input.src.as_bytes(),
            start_pos: input.start_pos,

            state: State::new(),
            syntax,
            target,
            ctx: Default::default(),
            errors: Default::default(),
            module_errors: Default::default(),
            strict_errors: Default::default(),
            buf: String::with_capacity(16),
        }
    }

    /// Utility method to reuse buffer.
    fn with_buf<F, Ret>(&mut self, op: F) -> LexResult<Ret>
    where
        F: FnOnce(&mut Lexer<'_>, &mut String) -> LexResult<Ret>,
    {
        let mut buf = std::mem::take(&mut self.buf);
        buf.clear();

        let res = op(self, &mut buf);

        self.buf = buf;

        res
    }

    fn read_token(&mut self) -> LexResult<Option<Token>> {
        let b = match self.cur_byte() {
            Some(ch) => ch,
            None => {
                return Ok(None);
            }
        };

        // A lookup table of `byte -> fn(l: &mut Lexer) -> Token` is slower than
        // this approach. The speed difference comes from the difference in
        // table size - a function pointer takes 64 (usize) bits, resulting in a
        // 64 * 256 = 16kb table vs a repr(u8) enum variant which takes 8 bits,
        // resulting in a 8 * 256 = 2kb table. The smaller table more easily
        // fits into the cpu cache, while the 16kb table will be ejected from
        // the cache more often leading to slowdowns. The smaller table also
        // allows for more aggressive optimizations regarding how to map the
        // match to instructions.
        let dispatched = Self::lookup(b);

        match dispatched {
            // The interpretation of a dot depends on whether it is followed
            // by a digit or another two dots.
            PRD => self.read_token_dot(),
            // Punctuation tokens.
            PNO => {
                self.advance(1);
                Ok(Some(LParen))
            }
            PNC => {
                self.advance(1);
                Ok(Some(RParen))
            }
            SEM => {
                self.advance(1);
                Ok(Some(Semi))
            }
            COM => {
                self.advance(1);
                Ok(Some(Comma))
            }
            BTO => {
                self.advance(1);
                Ok(Some(LBracket))
            }
            BTC => {
                self.advance(1);
                Ok(Some(RBracket))
            }
            BEO => {
                self.advance(1);
                Ok(Some(LBrace))
            }
            BEC => {
                self.advance(1);
                Ok(Some(RBrace))
            }
            COL => {
                self.advance(1);
                Ok(Some(Colon))
            }
            QST => Ok(Some(self.read_token_question())),
            TPL => {
                self.advance(1);
                Ok(Some(BackQuote))
            }
            ZER => {
                (match self.peek_nth(1) {
                    // '0x', '0X' - hex number
                    Some(b'x') | Some(b'X') => self.read_radix_number(16),
                    // '0o', '0O' - octal number
                    Some(b'o') | Some(b'O') => self.read_radix_number(8),
                    // '0b', '0B' - binary number
                    Some(b'b') | Some(b'B') => self.read_radix_number(2),

                    _ => self.read_number(false),
                })
                .map(Some)
            }
            // Anything else beginning with a digit is an integer, octal
            // number, or float.
            DIG => self.read_number(false).map(Some),

            // Quotes produce strings.
            QOT => self.read_string(b).map(Some),

            SLH => self.read_token_slash().map(Some),
            PRC | MUL => Ok(Some(self.read_token_mult_modulo(b))),
            PIP | AMP => Ok(Some(self.read_token_pipe_amp(b))),
            CRT => Ok(Some(self.read_token_caret())),
            PLS | MIN => self.read_token_plus_min(b),
            LSS | MOR => self.read_token_lt_gt(b),
            EQL | EXL => Ok(Some(self.read_token_eq_excl(b))),
            TLD => {
                self.advance(1);
                Ok(Some(tok!('~')))
            }
            AT_ => {
                self.advance(1);
                Ok(Some(At))
            }
            HAS => Ok(self.read_token_number_sign()),
            // Identifier or keyword. '\uXXXX' sequences are allowed in
            // identifiers, so '\' also dispatches to that.
            IDT | BSL => self.read_ident_or_keyword().map(Some),

            _ => {
                let ch = self.cur_unchecked();

                if is_ident_start(ch) {
                    // Identifier or keyword.
                    self.read_ident_or_keyword().map(Some)
                } else {
                    // unexpected character
                    self.bump();
                    let start = self.cur_pos();
                    self.error_span(pos_span(start), SyntaxError::UnexpectedChar { c: ch })?
                }
            }
        }
    }

    fn read_token_number_sign(&mut self) -> Option<Token> {
        debug_assert!(self.is(b'#'));

        if self.is_at_start() && self.read_token_interpreter() {
            return None;
        }

        debug_assert!(self.is(b'#'));

        self.advance(1); // '#'
        Some(tok!('#'))
    }

    fn read_token_dot(&mut self) -> LexResult<Option<Token>> {
        debug_assert!(self.is(b'.'));

        let next = match self.peek_nth(1) {
            Some(next) => next,
            None => {
                self.advance(1); // '.'
                return Ok(Some(tok!('.')));
            }
        };

        if next.is_ascii_digit() {
            return self.read_number(true).map(Some);
        }

        if next == b'.' && self.peek_nth(1) == Some(b'.') {
            self.advance(3); // "..."
            Ok(Some(tok!("...")))
        } else {
            self.advance(1); // "."
            Ok(Some(tok!('.')))
        }
    }

    fn read_token_slash(&mut self) -> LexResult<Token> {
        debug_assert!(self.is(b'/'));

        // Regex
        if self.state.is_expr_allowed {
            return self.read_regexp();
        }

        // Divide operator
        self.advance(1); // '/'

        if self.eat(b'=') {
            Ok(tok!("/="))
        } else {
            Ok(tok!('/'))
        }
    }

    fn read_token_interpreter(&mut self) -> bool {
        debug_assert!(self.is(b'#'));

        if !self.is_at_start() {
            return false;
        }

        let start = self.cur_pos();
        self.advance(1); // '#'
        if self.eat(b'!') {
            while let Some(ch) = self.cur() {
                if is_line_break(ch) {
                    return true;
                } else {
                    self.bump();
                }
            }
        }

        self.reset_to(start);
        false
    }

    fn read_token_mult_modulo(&mut self, ch: u8) -> Token {
        debug_assert!(ch == b'*' || ch == b'%');
        debug_assert!(self.is(ch));

        let is_mul = ch == b'*';
        self.advance(1); // '*' or '%'
        let mut token = if is_mul { BinOp(Mul) } else { BinOp(Mod) };

        // check for **
        if is_mul && self.eat(b'*') {
            token = BinOp(Exp)
        }

        if self.eat(b'=') {
            token = match token {
                BinOp(Mul) => AssignOp(MulAssign),
                BinOp(Mod) => AssignOp(ModAssign),
                BinOp(Exp) => AssignOp(ExpAssign),
                _ => unreachable!(),
            }
        }

        token
    }

    fn read_token_pipe_amp(&mut self, ch: u8) -> Token {
        debug_assert!(ch == b'|' || ch == b'&');
        debug_assert!(self.is(ch));

        self.advance(1); // '|' or '&'
        let token = if ch == b'&' { BitAnd } else { BitOr };

        // '|=', '&='
        if self.eat(b'=') {
            return AssignOp(match token {
                BitAnd => BitAndAssign,
                BitOr => BitOrAssign,
                _ => unreachable!(),
            });
        }

        // '||', '&&'
        if self.eat(ch) {
            if self.eat(b'=') {
                return AssignOp(match token {
                    BitAnd => op!("&&="),
                    BitOr => op!("||="),
                    _ => unreachable!(),
                });
            }

            return BinOp(match token {
                BitAnd => LogicalAnd,
                BitOr => LogicalOr,
                _ => unreachable!(),
            });
        }

        BinOp(token)
    }

    fn read_token_caret(&mut self) -> Token {
        debug_assert!(self.is(b'^'));
        // Bitwise xor
        self.advance(1); // '^'
        if self.eat(b'=') {
            AssignOp(BitXorAssign)
        } else {
            BinOp(BitXor)
        }
    }

    fn read_token_plus_min(&mut self, ch: u8) -> LexResult<Option<Token>> {
        debug_assert!(ch == b'+' || ch == b'-');
        debug_assert!(self.is(ch));

        let start = self.cur_pos();

        self.advance(1); // '+' or '-'

        if self.eat(ch) {
            // '++', '--'

            // Handle '-->' line comment
            if self.state.had_line_break && ch == b'-' && self.eat(b'>') {
                self.emit_module_mode_error(start, SyntaxError::LegacyCommentInModule);
                self.skip_line_comment(0);
                self.skip_space()?;
                self.read_token()
            } else if ch == b'+' {
                Ok(Some(PlusPlus))
            } else {
                Ok(Some(MinusMinus))
            }
        } else if self.eat(b'=') {
            // '+=', '-='
            Ok(Some(AssignOp(if ch == b'+' {
                AddAssign
            } else {
                SubAssign
            })))
        } else {
            // '+', '-'
            Ok(Some(BinOp(if ch == b'+' { Add } else { Sub })))
        }
    }

    fn read_token_lt_gt(&mut self, ch: u8) -> LexResult<Option<Token>> {
        debug_assert!(ch == b'<' || ch == b'>');
        debug_assert!(self.is(ch));

        let start = self.cur_pos();

        self.advance(1); // '<' or '>'

        // `<!--`, an XML-style comment that should be interpreted as a line comment
        if ch == b'<'
            && self.is(b'!')
            && self.peek_nth(1) == Some(b'-')
            && self.peek_nth(2) == Some(b'-')
        {
            self.skip_line_comment(3);
            self.skip_space()?;
            self.emit_module_mode_error(start, SyntaxError::LegacyCommentInModule);

            return self.read_token();
        }

        let mut op = if ch == b'<' { Lt } else { Gt };

        // '<<', '>>'
        if self.eat(ch) {
            op = if ch == b'<' { LShift } else { RShift };

            //'>>>'
            if ch == b'>' && self.eat(ch) {
                op = ZeroFillRShift;
            }
        }

        let token = if self.eat(b'=') {
            match op {
                Lt => BinOp(LtEq),
                Gt => BinOp(GtEq),
                LShift => AssignOp(LShiftAssign),
                RShift => AssignOp(RShiftAssign),
                ZeroFillRShift => AssignOp(ZeroFillRShiftAssign),
                _ => unreachable!(),
            }
        } else {
            BinOp(op)
        };

        Ok(Some(token))
    }

    fn read_token_eq_excl(&mut self, ch: u8) -> Token {
        debug_assert!(ch == b'=' || ch == b'!');
        debug_assert!(self.is(ch));

        self.advance(1); // '=' or '!'

        if self.eat(b'=') {
            // "=="

            if self.eat(b'=') {
                if ch == b'!' {
                    // '!=='
                    BinOp(NotEqEq)
                } else {
                    // '==='
                    BinOp(EqEqEq)
                }
            } else if ch == b'!' {
                // '!='
                BinOp(NotEq)
            } else {
                // '=='
                BinOp(EqEq)
            }
        } else if ch == b'=' && self.eat(b'>') {
            // "=>"

            Arrow
        } else if ch == b'!' {
            // '!'
            Bang
        } else {
            // '='
            AssignOp(Assign)
        }
    }

    fn read_token_question(&mut self) -> Token {
        debug_assert!(self.is(b'?'));

        if self.peek_nth(1) == Some(b'?') {
            if self.peek_nth(2) == Some(b'=') {
                self.advance(3); // '??='
                tok!("??=")
            } else {
                self.advance(2); // '??'
                tok!("??")
            }
        } else {
            self.advance(1); // '?'
            tok!('?')
        }
    }

    fn read_regexp(&mut self) -> LexResult<Token> {
        debug_assert!(self.is(b'/'));

        let start = self.cur_pos();

        self.advance(1); // '/'

        let mut escaped = false;
        let mut in_class = false;

        while let Some(ch) = self.cur() {
            if is_line_break(ch) {
                // Regex literal cannot span multiple lines
                self.error(start, SyntaxError::UnterminatedRegxp)?;
            }

            if escaped {
                escaped = false;
            } else {
                if ch == '[' {
                    in_class = true;
                } else if ch == ']' && in_class {
                    in_class = false;
                } else if ch == '/' && !in_class {
                    break;
                }
                escaped = ch == '\\';
            }
            self.bump();
        }

        if !self.is(b'/') {
            // Reached end of input without seeing closing '/'
            self.error(start, SyntaxError::UnterminatedRegxp)?;
        }

        let content_start = start + BytePos(1);

        let content = self.slice_to_cur(content_start).into();

        self.advance(1); // '/'

        // TODO: Use bit_flags?
        // 6 is the number of valid flags.
        let mut mods = String::with_capacity(6);

        while let Some(ch) = self.cur() {
            if is_valid_regex_flag(ch) {
                if mods.find(ch).is_some() {
                    self.error(self.cur_pos(), SyntaxError::DuplicateRegExpFlags)?
                }
            } else if is_ident_part(ch) || ch == '\\' {
                self.error(self.cur_pos(), SyntaxError::MalformedRegExpFlags)?
            } else {
                break;
            }

            self.bump();
            mods.push(ch);
        }

        Ok(Regex(content, mods.into()))
    }

    fn read_code_point(&mut self) -> LexResult<char> {
        let start = self.cur_pos();
        let val = self.read_int_u32(16, 0, false);

        if let Some(val) = val {
            if 0x0010_FFFF >= val {
                if let Some(ch) = std::char::from_u32(val) {
                    return Ok(ch);
                }
            }
        }

        self.error(start, SyntaxError::InvalidCodePoint)?
    }

    fn read_unicode_escape(&mut self, start: BytePos) -> LexResult<char> {
        if self.eat(b'{') {
            let ch = self.read_code_point()?;

            if !self.eat(b'}') {
                self.error(start, SyntaxError::InvalidUnicodeEscape)?
            }

            Ok(ch)
        } else {
            self.read_hex_char(start, 4)
        }
    }

    /// See https://tc39.github.io/ecma262/#sec-literals-string-literals
    fn read_string(&mut self, quote: u8) -> LexResult<Token> {
        debug_assert!(quote == b'\'' || quote == b'"');
        debug_assert!(self.is(quote));

        let start = self.cur_pos();
        self.advance(1); // ' or "

        self.with_buf(|lexer, out| {
            let mut has_escape = false;

            while let Some(ch) = {
                // Optimization
                {
                    let s = lexer.uncons_while_byte(|b| {
                        b != quote
                            && b != b'\\'
                            && b != char_bytes::LINE_FEED
                            && b != char_bytes::CARRIAGE_RETURN
                    });
                    out.push_str(s);
                }
                lexer.cur_byte()
            } {
                match ch {
                    ch if ch == quote => {
                        lexer.advance(1); // ' or "
                        return Ok(Token::Str {
                            value: out.as_str().into(),
                            has_escape,
                        });
                    }
                    b'\\' => {
                        if let Some(s) = lexer.read_escaped_char(false)? {
                            out.push(s);
                        }
                        has_escape = true
                    }
                    char_bytes::LINE_FEED | char_bytes::CARRIAGE_RETURN => {
                        // String literals cannot span multiple lines.
                        // LINE_SEPARATOR and PARAGRAPH_SEPARATOR are permitted.
                        let pos = lexer.cur_pos();
                        lexer.error(pos, SyntaxError::UnterminatedStrLit)?
                    }
                    _ => {
                        out.push(lexer.next_char());
                    }
                }
            }

            // Reached end of input without seeing closing quote.
            lexer.error(start, SyntaxError::UnterminatedStrLit)?
        })
    }

    // Used to read escaped characters.
    // TODO: handle templates
    fn read_escaped_char(&mut self, in_template: bool) -> LexResult<Option<char>> {
        debug_assert!(self.is(b'\\'));

        let start = self.cur_pos();
        self.advance(1); // '\'
        let ch = match self.cur() {
            Some(ch) => ch,
            None => self.error_span(pos_span(start), SyntaxError::InvalidStrEscape)?,
        };
        self.bump();

        macro_rules! invalid_escape {
            () => {{
                if in_template {
                    self.error(start, SyntaxError::InvalidEscapeInTemplate)?
                } else {
                    self.emit_strict_mode_error(start, SyntaxError::InvalidNumericEscapeInStrict);
                }
            }};
        }

        match ch {
            // Line feed
            'n' => Ok(Some('\n')),
            // Carriage return
            'r' => Ok(Some('\r')),
            'x' => self.read_hex_char(start, 2).map(Some),
            'u' => self.read_unicode_escape(start).map(Some),
            // Tab
            't' => Ok(Some('\t')),
            // Backspace
            'b' => Ok(Some(char_literals::BACKSPACE)),
            // Vertical tab
            'v' => Ok(Some(char_literals::LINE_TABULATION)),
            'f' => Ok(Some(char_literals::FORM_FEED)),
            char_literals::CARRIAGE_RETURN => {
                if self.is(char_bytes::LINE_FEED) {
                    self.advance(1);
                }

                Ok(None)
            }
            char_literals::LINE_FEED => Ok(None),
            char_literals::LINE_SEPARATOR | char_literals::PARAGRAPH_SEPARATOR => Ok(None),
            '8' | '9' => {
                invalid_escape!();
                Ok(None)
            }
            '0'..='7' => {
                let mut value = ch as u32 - '0' as u32;

                let first_digit = value;

                macro_rules! check {
                    ($more_than_one_digit:literal) => {{
                        if value > 0 || $more_than_one_digit || self.is(b'8') || self.is(b'9') {
                            invalid_escape!();
                        }
                    }};
                }

                match self.cur().and_then(|ch| ch.to_digit(8)) {
                    Some(v) => {
                        value = value * 8 + v;
                        self.advance(1);
                    }
                    _ => unsafe {
                        check!(false);

                        //  Spec: OctalDigit [lookahead ∉ OctalDigit]
                        return Ok(Some(std::char::from_u32_unchecked(value)));
                    },
                }

                match self.cur().and_then(|ch| ch.to_digit(8)) {
                    Some(v) => {
                        if first_digit > 3 {
                            // Spec: FourToSeven OctalDigit
                            // At this point we have a 3 digit octal number,
                            // with 4, 5, 6, or 7 as the first digit. The
                            // largest octal escape allowed is 377 (255 decimal).
                            // So, at this point we know the number is too
                            // large, and we don't include the third digit.
                        } else {
                            // Spec: ZeroToThree OctalDigit OctalDigit
                            value = value * 8 + v;
                            self.advance(1);
                        }
                    }
                    _ => unsafe {
                        check!(true);

                        // Spec: ZeroToThree OctalDigit [lookahead ∉ OctalDigit]
                        return Ok(Some(std::char::from_u32_unchecked(value)));
                    },
                }

                unsafe {
                    check!(true);

                    // Spec:
                    // FourToSeven OctalDigit
                    // ZeroToThree OctalDigit OctalDigit
                    Ok(Some(std::char::from_u32_unchecked(value)))
                }
            }
            _ => Ok(Some(ch)),
        }
    }

    // Used to read character escape sequences ('\x', '\u').
    fn read_hex_char(&mut self, start: BytePos, len: u8) -> LexResult<char> {
        debug_assert!(len == 2 || len == 4);

        let val = self.read_int_u32(16, len, false);

        if let Some(val) = val {
            if let Some(ch) = std::char::from_u32(val) {
                return Ok(ch);
            }
        }

        self.error(start, SyntaxError::ExpectedHexChars { count: len })?
    }

    // Read an identifier.
    fn read_word(&mut self) -> LexResult<(Word, bool)> {
        debug_assert!(
            self.is(b'\\') || (self.cur().is_some() && is_ident_start(self.cur().unwrap()))
        );

        let mut first = true;

        self.with_buf(|lexer, buf| {
            let mut has_escape = false;

            while let Some(ch) = {
                // Optimization
                {
                    let s = lexer.uncons_while_chars(is_ident_part);
                    if !s.is_empty() {
                        first = false;
                    }
                    buf.push_str(s)
                }

                lexer.cur_byte()
            } {
                match ch {
                    // unicode escape
                    b'\\' => {
                        let start = lexer.cur_pos();

                        lexer.advance(1); // '\'

                        if !lexer.eat(b'u') {
                            lexer.error_span(pos_span(start), SyntaxError::ExpectedUnicodeEscape)?
                        }

                        let ch = lexer.read_unicode_escape(start)?;

                        let valid = if first {
                            is_ident_start(ch)
                        } else {
                            is_ident_part(ch)
                        };

                        if !valid {
                            lexer.emit_error(start, SyntaxError::InvalidIdentChar);
                        }

                        buf.push(ch);

                        has_escape = true;
                    }

                    _ => {
                        break;
                    }
                }
                first = false;
            }
            let value = Word::from(buf.as_str());

            Ok((value, has_escape))
        })
    }

    // Read an identifier or keyword token. Will check for reserved
    // words when necessary.
    // See https://tc39.github.io/ecma262/#sec-names-and-keywords
    fn read_ident_or_keyword(&mut self) -> LexResult<Token> {
        debug_assert!(
            self.is(b'\\') || (self.cur().is_some() && is_ident_start(self.cur().unwrap()))
        );

        let start = self.cur_pos();

        let (word, has_esc) = self.read_word()?;

        // Note: ctx is stored in lexer because of this error.
        // 'await' and 'yield' may have semantic of reserved word, which means lexer
        // should know context or parser should handle this error. Our approach to this
        // problem is former one.
        if has_esc && self.ctx.is_reserved(&word) {
            self.error(
                start,
                SyntaxError::EscapeInReservedWord { word: word.into() },
            )?
        } else {
            Ok(Word(word))
        }
    }

    // TODO: Verify that the raw value is spec compliant/look at swc/babel's implementations.
    // TODO: use uncons_while
    fn read_tmpl_token(&mut self, start_of_tpl: BytePos) -> LexResult<Token> {
        enum CookedType {
            None,
            SameAsRaw,
            DifferentFromRaw(String),
        }

        let start = self.cur_pos();
        let mut cooked_chunk_start = start;

        let mut has_escape = false;
        let mut cooked = CookedType::SameAsRaw;

        while let Some(c) = self.cur_byte() {
            if c == b'`' || (c == b'$' && self.peek_nth(1) == Some(b'{')) {
                if start == self.cur_pos() && self.state.last_was_tpl_element() {
                    if c == b'$' {
                        self.advance(2); // '${'
                        return Ok(tok!("${"));
                    } else {
                        self.advance(1); // '`'
                        return Ok(tok!('`'));
                    }
                }

                let raw = self.slice_to_cur(start);

                let cooked = match cooked {
                    CookedType::SameAsRaw => Some(raw.into()),
                    CookedType::DifferentFromRaw(ref mut existing_cooked) => {
                        let chunk = self.slice_to_cur(cooked_chunk_start);
                        existing_cooked.push_str(chunk);
                        Some(JsWord::from(existing_cooked.as_str()))
                    }
                    CookedType::None => None,
                };

                // TODO(swc): Handle error
                return Ok(Template {
                    cooked,
                    raw: raw.into(),
                    has_escape,
                });
            }

            if c == b'\\' {
                has_escape = true;

                match cooked {
                    CookedType::SameAsRaw => {
                        let new_cooked = String::from(self.slice_to_cur(start));

                        cooked = CookedType::DifferentFromRaw(new_cooked);
                    }
                    CookedType::DifferentFromRaw(ref mut existing_cooked) => {
                        let new_chunk = self.slice_to_cur(cooked_chunk_start);
                        existing_cooked.push_str(new_chunk);
                    }
                    _ => {}
                }

                match self.read_escaped_char(true) {
                    Ok(Some(s)) => {
                        if let CookedType::DifferentFromRaw(ref mut existing_cooked) = cooked {
                            existing_cooked.push(s);

                            cooked_chunk_start = self.cur_pos();
                        }
                    }
                    Ok(None) => {}
                    Err(..) => {
                        cooked = CookedType::None;
                    }
                }
            } else if is_line_break(self.cur_unchecked()) {
                self.state.had_line_break = true;
                if c == b'\r' && self.peek_nth(1) == Some(b'\n') {
                    match cooked {
                        CookedType::SameAsRaw => {
                            let mut new_cooked = String::from(self.slice_to_cur(start));
                            new_cooked.push('\n');

                            cooked = CookedType::DifferentFromRaw(new_cooked);
                        }
                        CookedType::DifferentFromRaw(ref mut existing_cooked) => {
                            let new_chunk = self.slice_to_cur(cooked_chunk_start);
                            existing_cooked.push_str(new_chunk);
                            existing_cooked.push('\n');
                        }
                        _ => {}
                    }

                    self.advance(2); // '\r\n'

                    if let CookedType::DifferentFromRaw(..) = cooked {
                        cooked_chunk_start = self.cur_pos();
                    }
                } else {
                    self.bump();
                }
            } else {
                self.bump();
            }
        }

        self.error(start_of_tpl, SyntaxError::UnterminatedTpl)?
    }

    fn lookup(byte: u8) -> Dispatch {
        // Safety: The lookup table maps all values of u8, so its impossible for
        // a u8 to be out of bounds.
        unsafe { *DISPATCHER.get_unchecked(byte as usize) }
    }

    pub fn set_expr_allowed(&mut self, allow: bool) {
        self.state.is_expr_allowed = allow;
    }
}

// Every handler a byte coming in could be mapped to.
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u8)]
pub(self) enum Dispatch {
    ERR,
    WHS,
    EXL,
    QOT,
    IDT,
    HAS,
    PRC,
    AMP,
    PNO,
    PNC,
    MUL,
    PLS,
    COM,
    MIN,
    PRD,
    SLH,
    ZER,
    DIG,
    COL,
    SEM,
    LSS,
    EQL,
    MOR,
    QST,
    AT_,
    BTO,
    BSL,
    BTC,
    CRT,
    TPL,
    BEO,
    PIP,
    BEC,
    TLD,
    UNI,
}
use Dispatch::*;

// A lookup table mapping any incoming byte to a handler function.
pub(self) static DISPATCHER: [Dispatch; 256] = [
    //0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
    ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, WHS, WHS, WHS, WHS, WHS, ERR, ERR, // 0
    ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // 1
    WHS, EXL, QOT, HAS, IDT, PRC, AMP, QOT, PNO, PNC, MUL, PLS, COM, MIN, PRD, SLH, // 2
    ZER, DIG, DIG, DIG, DIG, DIG, DIG, DIG, DIG, DIG, COL, SEM, LSS, EQL, MOR, QST, // 3
    AT_, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, // 4
    IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, BTO, BSL, BTC, CRT, IDT, // 5
    TPL, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, // 6
    IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, BEO, PIP, BEC, TLD, ERR, // 7
    UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, // 8
    UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, // 9
    UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, // A
    UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, // B
    UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, // C
    UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, // D
    UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, // E
    UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, UNI, // F
];

fn pos_span(p: BytePos) -> Span {
    Span::new(p, p, Default::default())
}
