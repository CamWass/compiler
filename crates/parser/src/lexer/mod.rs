mod number;
mod state;
mod util;

use std::iter::FusedIterator;

use crate::{
    JscTarget, Syntax,
    context::Context,
    error::{Error, SyntaxError},
    token::*,
};
use ast::{NameId, ParserProgramData, id_for_built_in};
use atoms::JsWord;
use bitflags::bitflags;
use common::{BytePos, SourceFile, Span, chars::char_literals};
use number::{NonDecRadix, Radix};
use state::State;
pub(crate) use state::{LexerCheckpoint, TokenContext, TokenContexts};
use util::{char_bytes, is_line_break};

type LexResult<T> = Result<T, Error>;

pub struct Lexer<'src> {
    /// Index of current byte in `self.bytes`.
    cur: usize,
    bytes: &'src [u8],
    start_pos: BytePos,

    ctx: Context,
    state: State,
    syntax: Syntax,
    target: JscTarget,
    buf: String,

    errors: Vec<Error>,
    module_errors: Vec<Error>,
    strict_errors: Vec<Error>,

    pub program_data: &'src mut ParserProgramData,
}

impl FusedIterator for Lexer<'_> {}

impl Iterator for Lexer<'_> {
    type Item = TokenAndSpan;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next_token();
        if next.token == Token::Eof {
            None
        } else {
            Some(next)
        }
    }
}

impl<'src> Lexer<'src> {
    pub fn new(
        syntax: Syntax,
        target: JscTarget,
        input: &'src SourceFile,
        program_data: &'src mut ParserProgramData,
    ) -> Self {
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

            program_data,
        }
    }

    pub fn first_token(&mut self) -> TokenAndSpan {
        self.read_token_interpreter();

        self.next_token()
    }

    pub fn next_token(&mut self) -> TokenAndSpan {
        let mut start = self.cur_pos();

        let res = (|| {
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
                return self.read_tmpl_token(start_pos_of_tpl);
            }

            if self.syntax.typescript() && self.ctx.in_type() {
                if self.eat(b'<') {
                    return Ok(tok!('<'));
                } else if self.eat(b'>') {
                    return Ok(tok!('>'));
                }
            }

            self.read_token()
        })();

        let token = match res.map_err(|e| self.make_error_token(e)) {
            Ok(t) => t,
            Err(e) => e,
        };

        self.state.update(start, token);

        let had_line_break = self.state.had_line_break;
        self.state.had_line_break = false;

        TokenAndSpan {
            token,
            had_line_break,
            span: self.span(start),
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

    fn read_token(&mut self) -> LexResult<Token> {
        let Some(b) = self.cur_byte() else {
            return Ok(Token::Eof);
        };

        // TODO: re-evaluate:
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
                Ok(Token::LParen)
            }
            PNC => {
                self.advance(1);
                Ok(Token::RParen)
            }
            SEM => {
                self.advance(1);
                Ok(Token::Semi)
            }
            COM => {
                self.advance(1);
                Ok(Token::Comma)
            }
            BTO => {
                self.advance(1);
                Ok(Token::LBracket)
            }
            BTC => {
                self.advance(1);
                Ok(Token::RBracket)
            }
            BEO => {
                self.advance(1);
                Ok(Token::LBrace)
            }
            BEC => {
                self.advance(1);
                Ok(Token::RBrace)
            }
            COL => {
                self.advance(1);
                Ok(Token::Colon)
            }
            QST => Ok(self.read_token_question()),
            TPL => {
                self.advance(1);
                Ok(Token::BackQuote)
            }
            ZER => {
                match self.peek_nth(1) {
                    // '0x', '0X' - hex number
                    Some(b'x') | Some(b'X') => self.read_radix_number(NonDecRadix::Hex),
                    // '0o', '0O' - octal number
                    Some(b'o') | Some(b'O') => self.read_radix_number(NonDecRadix::Oct),
                    // '0b', '0B' - binary number
                    Some(b'b') | Some(b'B') => self.read_radix_number(NonDecRadix::Bin),

                    _ => self.read_number(false),
                }
            }
            // Anything else beginning with a digit is an integer, octal
            // number, or float.
            DIG => self.read_number(false),

            // Quotes produce strings.
            QOT => self.read_string(b),

            SLH => self.read_token_slash(),
            PRC | MUL => Ok(self.read_token_mult_modulo(b)),
            PIP | AMP => Ok(self.read_token_pipe_amp(b)),
            CRT => Ok(self.read_token_caret()),
            PLS | MIN => self.read_token_plus_min(b),
            LSS | MOR => self.read_token_lt_gt(b),
            EQL | EXL => Ok(self.read_token_eq_excl(b)),
            TLD => {
                self.advance(1);
                Ok(Token::Tilde)
            }
            AT_ => {
                self.advance(1);
                Ok(Token::At)
            }
            HAS => Ok(self.read_token_number_sign()),
            // Identifier or keyword. '\uXXXX' sequences are allowed in
            // identifiers, so '\' also dispatches to that.
            IDT | BSL => self.read_ident_or_keyword(),

            _ => {
                let ch = self.cur_unchecked();

                if ast::Ident::is_valid_start(ch) {
                    // Identifier or keyword.
                    self.read_ident_or_keyword()
                } else {
                    // unexpected character
                    self.bump();
                    let start = self.cur_pos();
                    Lexer::error_span(pos_span(start), SyntaxError::UnexpectedChar { c: ch })?
                }
            }
        }
    }

    fn read_token_number_sign(&mut self) -> Token {
        debug_assert!(self.is(b'#'));

        self.advance(1); // '#'
        tok!('#')
    }

    fn read_token_dot(&mut self) -> LexResult<Token> {
        debug_assert!(self.is(b'.'));

        let Some(next) = self.peek_nth(1) else {
            self.advance(1); // '.'
            return Ok(tok!('.'));
        };

        if next.is_ascii_digit() {
            return self.read_number(true);
        }

        if next == b'.' && self.peek_nth(1) == Some(b'.') {
            self.advance(3); // "..."
            Ok(tok!("..."))
        } else {
            self.advance(1); // "."
            Ok(tok!('.'))
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
        if !self.is(b'#') || self.peek_nth(1) != Some(b'!') {
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
        let mut token = if is_mul { Token::Mul } else { Token::Mod };

        // check for **
        if is_mul && self.eat(b'*') {
            token = Token::Exp;
        }

        if self.eat(b'=') {
            token = match token {
                Token::Mul => Token::MulAssign,
                Token::Mod => Token::ModAssign,
                Token::Exp => Token::ExpAssign,
                _ => unreachable!(),
            };
        }

        token
    }

    fn read_token_pipe_amp(&mut self, ch: u8) -> Token {
        debug_assert!(ch == b'|' || ch == b'&');
        debug_assert!(self.is(ch));

        self.advance(1); // '|' or '&'
        let token = if ch == b'&' {
            Token::BitAnd
        } else {
            Token::BitOr
        };

        // '|=', '&='
        if self.eat(b'=') {
            return match token {
                Token::BitAnd => Token::BitAndAssign,
                Token::BitOr => Token::BitOrAssign,
                _ => unreachable!(),
            };
        }

        // '||', '&&'
        if self.eat(ch) {
            if self.eat(b'=') {
                return match token {
                    Token::BitAnd => Token::AndAssign,
                    Token::BitOr => Token::OrAssign,
                    _ => unreachable!(),
                };
            }

            return match token {
                Token::BitAnd => Token::LogicalAnd,
                Token::BitOr => Token::LogicalOr,
                _ => unreachable!(),
            };
        }

        token
    }

    fn read_token_caret(&mut self) -> Token {
        debug_assert!(self.is(b'^'));
        // Bitwise xor
        self.advance(1); // '^'
        if self.eat(b'=') {
            Token::BitXorAssign
        } else {
            Token::BitXor
        }
    }

    fn read_token_plus_min(&mut self, ch: u8) -> LexResult<Token> {
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
                Ok(Token::PlusPlus)
            } else {
                Ok(Token::MinusMinus)
            }
        } else if self.eat(b'=') {
            // '+=', '-='
            Ok(if ch == b'+' {
                Token::AddAssign
            } else {
                Token::SubAssign
            })
        } else {
            // '+', '-'
            Ok(if ch == b'+' { Token::Add } else { Token::Sub })
        }
    }

    fn read_token_lt_gt(&mut self, ch: u8) -> LexResult<Token> {
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

        let mut op = if ch == b'<' { Token::Lt } else { Token::Gt };

        // '<<', '>>'
        if self.eat(ch) {
            op = if ch == b'<' {
                Token::LShift
            } else {
                Token::RShift
            };

            //'>>>'
            if ch == b'>' && self.eat(ch) {
                op = Token::ZeroFillRShift;
            }
        }

        let token = if self.eat(b'=') {
            match op {
                Token::Lt => Token::LtEq,
                Token::Gt => Token::GtEq,
                Token::LShift => Token::LShiftAssign,
                Token::RShift => Token::RShiftAssign,
                Token::ZeroFillRShift => Token::ZeroFillRShiftAssign,
                _ => unreachable!(),
            }
        } else {
            op
        };

        Ok(token)
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
                    Token::NotEqEq
                } else {
                    // '==='
                    Token::EqEqEq
                }
            } else if ch == b'!' {
                // '!='
                Token::NotEq
            } else {
                // '=='
                Token::EqEq
            }
        } else if ch == b'=' && self.eat(b'>') {
            // "=>"

            Token::Arrow
        } else if ch == b'!' {
            // '!'
            Token::Bang
        } else {
            // '='
            Token::Assign
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

        self.advance(1); // '/'

        let mut flags = RegexFlags::empty();

        while let Some(ch) = self.cur() {
            let flag = match ch {
                'd' => Some(RegexFlags::D),
                'g' => Some(RegexFlags::G),
                'i' => Some(RegexFlags::I),
                'm' => Some(RegexFlags::M),
                's' => Some(RegexFlags::S),
                'u' => Some(RegexFlags::U),
                'v' => Some(RegexFlags::V),
                'y' => Some(RegexFlags::Y),
                _ => None,
            };
            if let Some(flag) = flag {
                if flags.contains(flag) {
                    self.error(self.cur_pos(), SyntaxError::DuplicateRegExpFlags)?;
                } else {
                    flags.insert(flag);
                }
            } else if ast::Ident::is_valid_continue(ch) || ch == '\\' {
                self.error(self.cur_pos(), SyntaxError::MalformedRegExpFlags)?;
            } else {
                break;
            }

            self.bump();
        }

        Ok(Token::Regex)
    }

    fn read_code_point(&mut self) -> LexResult<char> {
        let start = self.cur_pos();
        let val = self.read_int_u32(Radix::Hex, 0, false);

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
                self.error(start, SyntaxError::InvalidUnicodeEscape)?;
            }

            Ok(ch)
        } else {
            self.read_hex_char(start, EscapeSequenceLength::Unicode)
        }
    }

    /// See https://tc39.github.io/ecma262/#sec-literals-string-literals
    fn read_string(&mut self, quote: u8) -> LexResult<Token> {
        debug_assert!(quote == b'\'' || quote == b'"');
        debug_assert!(self.is(quote));

        let start = self.cur_pos();
        self.advance(1); // ' or "

        self.with_buf(|lexer, out| {
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
                        return Ok(lexer.make_str_token(Box::new(String::from(out.as_str()))));
                    }
                    b'\\' => {
                        if let Some(s) = lexer.read_escaped_char(false)? {
                            out.push(s);
                        }
                    }
                    char_bytes::LINE_FEED | char_bytes::CARRIAGE_RETURN => {
                        // String literals cannot span multiple lines.
                        // LINE_SEPARATOR and PARAGRAPH_SEPARATOR are permitted.
                        let pos = lexer.cur_pos();
                        lexer.error(pos, SyntaxError::UnterminatedStrLit)?;
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
            None => Lexer::error_span(pos_span(start), SyntaxError::InvalidStrEscape)?,
        };
        self.bump();

        macro_rules! invalid_escape {
            () => {{
                if in_template {
                    // Note: we don't emit the error here since invalid escapes
                    // are allowed for tagged templates in newer ECMAScript
                    // versions - we'll emit the error in the parser if
                    // necessary, once we have the necessary context.
                    Lexer::error_span(pos_span(start), SyntaxError::InvalidEscapeInTemplate)?
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
            'x' => self
                .read_hex_char(start, EscapeSequenceLength::Hex)
                .map(Some),
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
    fn read_hex_char(&mut self, start: BytePos, len: EscapeSequenceLength) -> LexResult<char> {
        let val = self.read_int_u32(Radix::Hex, len as usize, false);

        if let Some(val) = val {
            if let Some(ch) = std::char::from_u32(val) {
                return Ok(ch);
            }
        }

        self.error(start, SyntaxError::ExpectedHexChars { count: len as u8 })?
    }

    // Read an identifier.
    fn read_word(&mut self) -> LexResult<(NameId, bool)> {
        debug_assert!(
            self.is(b'\\')
                || (self.cur().is_some() && ast::Ident::is_valid_start(self.cur().unwrap()))
        );

        let mut first = true;

        self.with_buf(|lexer, buf| {
            let mut has_escape = false;

            while let Some(ch) = {
                // Optimization
                {
                    let s = lexer.uncons_while_chars(ast::Ident::is_valid_continue);
                    if !s.is_empty() {
                        first = false;
                    }
                    buf.push_str(s);
                }

                lexer.cur_byte()
            } {
                match ch {
                    // unicode escape
                    b'\\' => {
                        let start = lexer.cur_pos();

                        lexer.advance(1); // '\'

                        if !lexer.eat(b'u') {
                            Lexer::error_span(pos_span(start), SyntaxError::ExpectedUnicodeEscape)?;
                        }

                        let ch = lexer.read_unicode_escape(start)?;

                        let valid = if first {
                            ast::Ident::is_valid_start(ch)
                        } else {
                            ast::Ident::is_valid_continue(ch)
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
            let value = lexer.program_data.intern_name(JsWord::from(buf.as_str()));

            Ok((value, has_escape))
        })
    }

    // Read an identifier or keyword token. Will check for reserved
    // words when necessary.
    // See https://tc39.github.io/ecma262/#sec-names-and-keywords
    fn read_ident_or_keyword(&mut self) -> LexResult<Token> {
        debug_assert!(
            self.is(b'\\')
                || (self.cur().is_some() && ast::Ident::is_valid_start(self.cur().unwrap()))
        );

        let start = self.cur_pos();

        let (word, has_esc) = self.read_word()?;

        // Note: ctx is stored in lexer because of this error.
        // 'await' and 'yield' may have semantic of reserved word, which means lexer
        // should know context or parser should handle this error. Our approach to this
        // problem is former one.
        if has_esc && self.ctx.is_reserved(word) {
            // TODO: mark this and others as cold?
            let word = self.program_data.get_name_text(word).clone();
            self.error(start, SyntaxError::EscapeInReservedWord { word })?
        } else {
            if word <= id_for_built_in!("override") {
                // The inner u32s for these NameIds have the same value as the
                // corresponding Tokens as u8s. For example:
                // id_for_built_in!("await").as_u32() == 0 == Token::Await as u8
                // This should compile down to a no-op: https://rust.godbolt.org/z/EYaYqGdrj
                Ok(match word {
                    id_for_built_in!("await") => Token::Await,
                    id_for_built_in!("break") => Token::Break,
                    id_for_built_in!("case") => Token::Case,
                    id_for_built_in!("catch") => Token::Catch,
                    id_for_built_in!("continue") => Token::Continue,
                    id_for_built_in!("debugger") => Token::Debugger,
                    id_for_built_in!("default") => Token::Default,
                    id_for_built_in!("do") => Token::Do,
                    id_for_built_in!("else") => Token::Else,
                    id_for_built_in!("finally") => Token::Finally,
                    id_for_built_in!("for") => Token::For,
                    id_for_built_in!("function") => Token::Function,
                    id_for_built_in!("if") => Token::If,
                    id_for_built_in!("return") => Token::Return,
                    id_for_built_in!("switch") => Token::Switch,
                    id_for_built_in!("throw") => Token::Throw,
                    id_for_built_in!("try") => Token::Try,
                    id_for_built_in!("var") => Token::Var,
                    id_for_built_in!("let") => Token::Let,
                    id_for_built_in!("const") => Token::Const,
                    id_for_built_in!("while") => Token::While,
                    id_for_built_in!("with") => Token::With,
                    id_for_built_in!("new") => Token::New,
                    id_for_built_in!("this") => Token::This,
                    id_for_built_in!("super") => Token::Super,
                    id_for_built_in!("class") => Token::Class,
                    id_for_built_in!("extends") => Token::Extends,
                    id_for_built_in!("export") => Token::Export,
                    id_for_built_in!("import") => Token::Import,
                    id_for_built_in!("yield") => Token::Yield,
                    id_for_built_in!("in") => Token::In,
                    id_for_built_in!("instanceof") => Token::InstanceOf,
                    id_for_built_in!("typeof") => Token::TypeOf,
                    id_for_built_in!("void") => Token::Void,
                    id_for_built_in!("delete") => Token::Delete,
                    id_for_built_in!("null") => Token::Null,
                    id_for_built_in!("true") => Token::True,
                    id_for_built_in!("false") => Token::False,
                    id_for_built_in!("async") => Token::Async,
                    id_for_built_in!("as") => Token::As,
                    id_for_built_in!("from") => Token::From,
                    id_for_built_in!("of") => Token::Of,
                    id_for_built_in!("static") => Token::Static,
                    id_for_built_in!("target") => Token::Target,
                    id_for_built_in!("asserts") => Token::Asserts,
                    id_for_built_in!("implements") => Token::Implements,
                    id_for_built_in!("is") => Token::Is,
                    id_for_built_in!("keyof") => Token::Keyof,
                    id_for_built_in!("unique") => Token::Unique,
                    id_for_built_in!("object") => Token::Object,
                    id_for_built_in!("global") => Token::Global,
                    id_for_built_in!("enum") => Token::Enum,
                    id_for_built_in!("readonly") => Token::Readonly,
                    id_for_built_in!("abstract") => Token::Abstract,
                    id_for_built_in!("infer") => Token::Infer,
                    id_for_built_in!("any") => Token::Any,
                    id_for_built_in!("boolean") => Token::Boolean,
                    id_for_built_in!("bigint") => Token::Bigint,
                    id_for_built_in!("intrinsic") => Token::Intrinsic,
                    id_for_built_in!("never") => Token::Never,
                    id_for_built_in!("number") => Token::Number,
                    id_for_built_in!("string") => Token::String,
                    id_for_built_in!("symbol") => Token::Symbol,
                    id_for_built_in!("unknown") => Token::Unknown,
                    id_for_built_in!("interface") => Token::Interface,
                    id_for_built_in!("declare") => Token::Declare,
                    id_for_built_in!("undefined") => Token::Undefined,
                    id_for_built_in!("meta") => Token::Meta,
                    id_for_built_in!("type") => Token::Type,
                    id_for_built_in!("assert") => Token::Assert,
                    id_for_built_in!("get") => Token::Get,
                    id_for_built_in!("set") => Token::Set,
                    id_for_built_in!("public") => Token::Public,
                    id_for_built_in!("protected") => Token::Protected,
                    id_for_built_in!("private") => Token::Private,
                    id_for_built_in!("package") => Token::Package,
                    id_for_built_in!("override") => Token::Override,
                    _ => unreachable!(),
                })
            } else {
                Ok(self.make_ident_token(word))
            }
        }
    }

    // TODO: Verify that the raw value is spec compliant/look at swc/babel's implementations.
    // TODO: use uncons_while
    fn read_tmpl_token(&mut self, start_of_tpl: BytePos) -> LexResult<Token> {
        let start = self.cur_pos();

        let mut has_invalid_escape = false;

        while let Some(c) = self.cur_byte() {
            if c == b'`' || (c == b'$' && self.peek_nth(1) == Some(b'{')) {
                if start == self.cur_pos() && self.state.last_was_tpl_element() {
                    if c == b'$' {
                        self.advance(2); // '${'
                        return Ok(tok!("${"));
                    }

                    self.advance(1); // '`'
                    return Ok(tok!('`'));
                }

                let raw = self.slice_to_cur(start);

                return Ok(self.make_tpl_token(Box::new(String::from(raw)), has_invalid_escape));
            }

            if c == b'\\' {
                if self.read_escaped_char(true).is_err() {
                    has_invalid_escape = true;
                };
            } else if is_line_break(self.cur_unchecked()) {
                self.state.had_line_break = true;
                if c == b'\r' && self.peek_nth(1) == Some(b'\n') {
                    self.advance(2); // '\r\n'
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
enum Dispatch {
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
static DISPATCHER: [Dispatch; 256] = [
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
    Span::new(p, p)
}

#[derive(Clone, Copy)]
enum EscapeSequenceLength {
    Hex = 2,
    Unicode = 4,
}

bitflags! {
    #[derive(Clone, Copy)]
    pub struct RegexFlags: u8 {
        const D = 1 << 0;
        const G = 1 << 1;
        const I = 1 << 2;
        const M = 1 << 3;
        const S = 1 << 4;
        const U = 1 << 5;
        const V = 1 << 6;
        const Y = 1 << 7;
    }
}
