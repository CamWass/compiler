pub mod identifier;
mod number;
mod state;
mod util;

use crate::{
    context::Context,
    error::{Error, SyntaxError},
    token::*,
    JscTarget,
};
use ast::op;
use global_common::{input::Input, BytePos, Span};
use identifier::{is_ident_part, is_ident_start};
use state::State;
pub use state::{TokenContext, TokenContexts};
use std::{cell::RefCell, iter::FusedIterator, rc::Rc};
use swc_atoms::JsWord;
use util::{char_bytes, char_literals, is_line_break, is_valid_regex_flag};

pub(crate) type LexResult<T> = Result<T, Error>;

#[derive(Clone)]
pub struct Lexer<I: Input> {
    pub(crate) ctx: Context,
    input: I,
    state: State,
    pub(crate) target: JscTarget,
    errors: Rc<RefCell<Vec<Error>>>,
    module_errors: Rc<RefCell<Vec<Error>>>,
}

impl<I: Input> FusedIterator for Lexer<I> {}

impl<I: Input> Iterator for Lexer<I> {
    type Item = TokenAndSpan;
    fn next(&mut self) -> Option<Self::Item> {
        let mut start = self.cur_pos();

        let res = (|| -> Result<Option<_>, _> {
            self.state.had_line_break = false;

            // Skip the space after the previous token, so that the next one's
            // `start` will point to the right position.
            if self.state.can_skip_space() {
                self.skip_space()?;
                start = self.input.cur_pos();
            };

            self.state.start = start;

            if let Some(TokenContext::Tpl {
                start: start_pos_of_tpl,
            }) = self.state.context.current()
            {
                self.read_tmpl_token(start_pos_of_tpl).map(Some)
            } else {
                self.read_token()
            }
        })();

        let token = match res.map_err(Token::Error).map_err(Some) {
            Ok(t) => t,
            Err(e) => e,
        };

        if let Some(ref token) = token {
            self.state.update(start, &token);
            self.state.last_tok_end = self.last_pos();
        }

        token.map(|token| {
            // Attach span to token.
            TokenAndSpan {
                token,
                had_line_break: self.state.had_line_break,
                span: self.span(start),
            }
        })
    }
}

impl<I: Input> Lexer<I> {
    pub fn new(target: JscTarget, input: I) -> Self {
        Lexer {
            state: State::new(),
            input,
            target,
            ctx: Default::default(),
            errors: Default::default(),
            module_errors: Default::default(),
        }
    }

    fn read_token(&mut self) -> LexResult<Option<Token>> {
        let ch = match self.cur() {
            Some(c) => c,
            None => {
                return Ok(None);
            }
        };

        match ch {
            // The interpretation of a dot depends on whether it is followed
            // by a digit or another two dots.
            '.' => self.read_token_dot(),
            // Punctuation tokens.
            '(' => {
                self.bump();
                Ok(Some(LParen))
            }
            ')' => {
                self.bump();
                Ok(Some(RParen))
            }
            ';' => {
                self.bump();
                Ok(Some(Semi))
            }
            ',' => {
                self.bump();
                Ok(Some(Comma))
            }
            '[' => {
                self.bump();
                Ok(Some(LBracket))
            }
            ']' => {
                self.bump();
                Ok(Some(RBracket))
            }
            '{' => {
                self.bump();
                Ok(Some(LBrace))
            }
            '}' => {
                self.bump();
                Ok(Some(RBrace))
            }
            ':' => {
                self.bump();
                Ok(Some(Colon))
            }
            '?' => self.read_token_question(),
            '`' => {
                self.bump();
                Ok(Some(BackQuote))
            }
            '0' => {
                (match self.peek() {
                    // '0x', '0X' - hex number
                    Some('x') | Some('X') => self.read_radix_number(16),
                    // '0o', '0O' - octal number
                    Some('o') | Some('O') => self.read_radix_number(8),
                    // '0b', '0B' - binary number
                    Some('b') | Some('B') => self.read_radix_number(2),

                    _ => self.read_number(false),
                })
                .map(Some)
            }
            // Anything else beginning with a digit is an integer, octal
            // number, or float.
            '1'..='9' => self.read_number(false).map(Some),

            // Quotes produce strings.
            '"' | '\'' => self.read_string(ch).map(Some),

            '/' => self.read_token_slash().map(Some),
            '%' | '*' => self.read_token_mult_modulo(ch).map(Some),
            '|' | '&' => self.read_token_pipe_amp(ch).map(Some),
            '^' => self.read_token_caret().map(Some),
            '+' | '-' => self.read_token_plus_min(ch),
            '<' | '>' => self.read_token_lt_gt(ch),
            '=' | '!' => self.read_token_eq_excl(ch).map(Some),
            '~' => {
                self.bump();
                Ok(Some(tok!('~')))
            }
            '@' => {
                self.bump();
                Ok(Some(At))
            }
            '#' => self.read_token_number_sign(),
            // Identifier or keyword. '\uXXXX' sequences are allowed in
            // identifiers, so '\' also dispatches to that.
            ch if ch == '\\' || is_ident_start(ch) => self.read_ident_or_keyword().map(Some),

            // unexpected character
            _ => {
                self.input.bump();
                let start = self.cur_pos();
                self.error_span(pos_span(start), SyntaxError::UnexpectedChar { c: ch })?
            }
        }
    }

    fn read_token_number_sign(&mut self) -> LexResult<Option<Token>> {
        debug_assert!(self.cur() == Some('#'));

        if self.input.is_at_start() && self.read_token_interpreter() {
            return Ok(None);
        }

        self.bump(); // '#'
        Ok(Some(tok!('#')))
    }

    fn read_token_dot(&mut self) -> LexResult<Option<Token>> {
        debug_assert!(self.cur() == Some('.'));

        let next = match self.peek() {
            Some(next) => next,
            None => {
                self.bump(); // '.'
                return Ok(Some(tok!('.')));
            }
        };

        if next.is_ascii_digit() {
            return self.read_number(true).map(Some);
        }

        self.bump(); // 1st '.'

        if next == '.' && self.peek_ahead() == Some('.') {
            self.bump(); // 2nd '.'
            self.bump(); // 3rd '.'
            Ok(Some(tok!("...")))
        } else {
            Ok(Some(tok!('.')))
        }
    }

    fn read_token_slash(&mut self) -> LexResult<Token> {
        debug_assert_eq!(self.cur(), Some('/'));

        // Regex
        if self.state.is_expr_allowed {
            return self.read_regexp();
        }

        // Divide operator
        self.bump();

        if self.eat(b'=') {
            Ok(tok!("/="))
        } else {
            Ok(tok!('/'))
        }
    }

    fn read_token_interpreter(&mut self) -> bool {
        debug_assert!(self.cur() == Some('#'));

        if !self.input.is_at_start() {
            return false;
        }

        let start = self.cur_pos();
        self.bump(); // '#'
        if self.is(b'!') {
            self.bump(); // '!'
            while let Some(c) = self.cur() {
                if is_line_break(c) {
                    return true;
                } else {
                    self.bump();
                }
            }
        }

        self.input.reset_to(start);
        false
    }

    fn read_token_mult_modulo(&mut self, ch: char) -> LexResult<Token> {
        debug_assert!(ch == '*' || ch == '%');
        debug_assert!(self.cur() == Some(ch));

        let is_mul = ch == '*';
        self.bump();
        let mut token = if is_mul { BinOp(Mul) } else { BinOp(Mod) };

        // check for **
        if is_mul && self.is(b'*') {
            self.bump();
            token = BinOp(Exp)
        }

        if self.is(b'=') {
            self.bump();
            token = match token {
                BinOp(Mul) => AssignOp(MulAssign),
                BinOp(Mod) => AssignOp(ModAssign),
                BinOp(Exp) => AssignOp(ExpAssign),
                _ => unreachable!(),
            }
        }

        Ok(token)
    }

    fn read_token_pipe_amp(&mut self, ch: char) -> LexResult<Token> {
        debug_assert!(ch == '|' || ch == '&');
        debug_assert!(self.cur() == Some(ch));

        self.bump();
        let token = if ch == '&' { BitAnd } else { BitOr };

        // '|=', '&='
        if self.is(b'=') {
            self.bump();

            return Ok(AssignOp(match token {
                BitAnd => BitAndAssign,
                BitOr => BitOrAssign,
                _ => unreachable!(),
            }));
        }

        // '||', '&&'
        if self.is(ch as u8) {
            self.bump();

            if self.is(b'=') {
                self.bump();
                return Ok(AssignOp(match token {
                    BitAnd => op!("&&="),
                    BitOr => op!("||="),
                    _ => unreachable!(),
                }));
            }

            return Ok(BinOp(match token {
                BitAnd => LogicalAnd,
                BitOr => LogicalOr,
                _ => unreachable!(),
            }));
        }

        Ok(BinOp(token))
    }

    fn read_token_caret(&mut self) -> LexResult<Token> {
        debug_assert!(self.cur() == Some('^'));
        // Bitwise xor
        self.bump(); // '^'
        if self.is(b'=') {
            self.bump(); // '='
            Ok(AssignOp(BitXorAssign))
        } else {
            Ok(BinOp(BitXor))
        }
    }

    fn read_token_plus_min(&mut self, ch: char) -> LexResult<Option<Token>> {
        debug_assert!(ch == '+' || ch == '-');
        debug_assert!(self.cur() == Some(ch));

        let start = self.input.cur_pos();

        self.bump(); // '+' or '-'

        if self.is(ch as u8) {
            // '++', '--'
            self.bump();

            // Handle '-->' line comment
            if !self.ctx.module
            // TODO: reassess this line:
                && (self.state.had_line_break || self.state.last_tok_end == BytePos(0))
                && ch == '-'
                && self.eat(b'>')
            {
                self.emit_module_mode_error(start, SyntaxError::LegacyCommentInModule);
                self.skip_line_comment(0);
                self.skip_space()?;
                self.read_token()
            } else if ch == '+' {
                Ok(Some(PlusPlus))
            } else {
                Ok(Some(MinusMinus))
            }
        } else if self.is(b'=') {
            // '+=', '-='
            self.bump();
            Ok(Some(AssignOp(if ch == '+' {
                AddAssign
            } else {
                SubAssign
            })))
        } else {
            // '+', '-'
            Ok(Some(BinOp(if ch == '+' { Add } else { Sub })))
        }
    }

    fn read_token_lt_gt(&mut self, ch: char) -> LexResult<Option<Token>> {
        debug_assert!(ch == '<' || ch == '>');
        debug_assert!(self.cur() == Some(ch));

        let start = self.cur_pos();

        self.bump(); // '<' or '>'

        // `<!--`, an XML-style comment that should be interpreted as a line comment
        if !self.ctx.module
            && ch == '<'
            && self.is(b'!')
            && self.peek() == Some('-')
            && self.peek_ahead() == Some('-')
        {
            self.skip_line_comment(4);
            self.skip_space()?;
            self.emit_module_mode_error(start, SyntaxError::LegacyCommentInModule);

            return self.read_token();
        }

        let mut op = if ch == '<' { Lt } else { Gt };

        // '<<', '>>'
        if self.is(ch as u8) {
            self.bump();
            op = if ch == '<' { LShift } else { RShift };

            //'>>>'
            if ch == '>' && self.is(ch as u8) {
                self.bump();
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

    fn read_token_eq_excl(&mut self, ch: char) -> LexResult<Token> {
        debug_assert!(ch == '=' || ch == '!');
        debug_assert!(self.cur() == Some(ch));

        self.bump(); // '=' or '!'

        if self.is(b'=') {
            // "=="
            self.bump();

            if self.is(b'=') {
                self.bump();
                if ch == '!' {
                    // '!=='
                    Ok(BinOp(NotEqEq))
                } else {
                    // '==='
                    Ok(BinOp(EqEqEq))
                }
            } else if ch == '!' {
                // '!='
                Ok(BinOp(NotEq))
            } else {
                // '=='
                Ok(BinOp(EqEq))
            }
        } else if ch == '=' && self.is(b'>') {
            // "=>"
            self.bump();

            Ok(Arrow)
        } else if ch == '!' {
            // '!'
            Ok(Bang)
        } else {
            // '='
            Ok(AssignOp(Assign))
        }
    }

    fn read_token_question(&mut self) -> LexResult<Option<Token>> {
        debug_assert!(self.cur() == Some('?'));

        self.bump(); // '?'

        let next = self.peek();

        match self.cur() {
            Some('?') => {
                self.bump(); // 2nd '?'
                if next == Some('=') {
                    self.bump(); // '='
                    Ok(Some(tok!("??=")))
                } else {
                    Ok(Some(tok!("??")))
                }
            }
            _ => Ok(Some(tok!('?'))),
        }
    }

    fn read_regexp(&mut self) -> LexResult<Token> {
        debug_assert!(self.cur() == Some('/'));

        let start = self.cur_pos();

        self.bump();

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

        let content = self.input.slice_to_cur(content_start).into();

        self.bump(); // '/'

        // 6 is the number of valid flags.
        let mut mods = String::with_capacity(6);

        while let Some(ch) = self.cur() {
            if is_valid_regex_flag(ch) {
                if mods.find(ch).is_some() {
                    // self.raise(self.state.pos + 1, Errors.DuplicateRegExpFlags);
                    panic!("DuplicateRegExpFlags at {:?}", self.cur_pos() + BytePos(1));
                }
            } else if is_ident_part(ch) || ch == '\\' {
                // self.raise(self.state.pos + 1, Errors.MalformedRegExpFlags);
                panic!("MalformedRegExpFlags at {:?}", self.cur_pos() + BytePos(1));
            } else {
                break;
            }

            self.bump();
            mods.push(ch);
        }

        Ok(Regex(content, mods.into()))
    }

    fn read_code_point(&mut self) -> LexResult<char> {
        debug_assert!(self.cur().is_some() && self.cur().unwrap().is_digit(16));

        let start = self.cur_pos();
        let val = self.read_int_u32(16, 0, false)?;

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
        debug_assert!(self.is(b'{') || (self.cur().is_some() && self.cur().unwrap().is_digit(16)));

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
    fn read_string(&mut self, quote: char) -> LexResult<Token> {
        debug_assert!(quote == '\'' || quote == '"');
        debug_assert!(self.cur() == Some(quote));

        let start = self.cur_pos();
        self.bump(); // ' or "

        let mut out = String::new();
        let mut chunk_start = self.cur_pos();

        let mut has_escape = false;
        while let Some(ch) = self.cur() {
            if ch == quote {
                break;
            } else if ch == '\\' {
                out.push_str(self.input.slice_to_cur(chunk_start));

                if let Some(c) = self.read_escaped_char(false)? {
                    out.push(c);
                }

                has_escape = true;

                chunk_start = self.cur_pos();
            } else if ch == char_literals::LINE_SEPARATOR
                || ch == char_literals::PARAGRAPH_SEPARATOR
            {
                self.bump();
            } else if is_line_break(ch) {
                // String literals cannot span multiple lines.
                let pos = self.cur_pos();
                self.error(pos, SyntaxError::UnterminatedStrLit)?
            } else {
                self.bump();
            }
        }

        if !self.is(quote as u8) {
            // Reached end of input without seeing closing quote (' or ")
            self.error(start, SyntaxError::UnterminatedStrLit)?
        }

        out.push_str(self.input.slice_to_cur(chunk_start));
        self.bump(); // ' or "
        Ok(Token::Str {
            value: out.into(),
            has_escape,
        })
    }

    // Used to read escaped characters.
    // TODO: handle templates
    fn read_escaped_char(&mut self, in_template: bool) -> LexResult<Option<char>> {
        debug_assert!(self.cur() == Some('\\'));

        let start = self.cur_pos();
        self.bump(); // '\'
        let ch = match self.cur() {
            Some(c) => c,
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
            char_literals::CARRIAGE_RETURN | char_literals::LINE_FEED => {
                if ch == char_literals::CARRIAGE_RETURN && self.is(char_bytes::LINE_FEED) {
                    self.bump();
                }

                Ok(None)
            }
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

                match self.cur().and_then(|c| c.to_digit(8)) {
                    Some(v) => {
                        value = value * 8 + v;
                        self.bump();
                    }
                    _ => unsafe {
                        check!(false);

                        //  Spec: OctalDigit [lookahead ∉ OctalDigit]
                        return Ok(Some(std::char::from_u32_unchecked(value)));
                    },
                }

                match self.cur().and_then(|c| c.to_digit(8)) {
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
                            self.bump();
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
                    return Ok(Some(std::char::from_u32_unchecked(value)));
                };
            }
            _ => Ok(Some(ch)),
        }
    }

    // Used to read character escape sequences ('\x', '\u').
    fn read_hex_char(&mut self, start: BytePos, len: u8) -> LexResult<char> {
        debug_assert!(self.cur().is_some() && self.cur().unwrap().is_digit(16));
        debug_assert!(len == 2 || len == 4);

        let val = self.read_int_u32(16, len, false)?;

        if let Some(val) = val {
            if let Some(ch) = std::char::from_u32(val) {
                return Ok(ch);
            }
        }

        self.error(start, SyntaxError::ExpectedHexChars { count: len })?
    }

    // Read an identifier, and return it as a string.
    fn read_word(&mut self) -> LexResult<(JsWord, bool)> {
        debug_assert!(
            self.cur() == Some('\\')
                || (self.cur().is_some() && is_ident_start(self.cur().unwrap()))
        );

        // TODO: push chunks of source text into String rather than one char at a
        // time. These chunk would be between any escape sequences (like babel).

        let mut word = String::new();
        let mut contains_esc = false;
        let start = self.cur_pos();

        while let Some(ch) = self.cur() {
            if is_ident_part(ch) {
                self.bump();
                word.push(ch);
            } else if ch == '\\' {
                contains_esc = true;

                let esc_start = self.cur_pos();

                self.bump(); // '\'
                if !self.is(b'u') {
                    self.error_span(pos_span(esc_start), SyntaxError::ExpectedUnicodeEscape)?
                }

                self.bump(); // 'u'

                let ch = self.read_unicode_escape(esc_start)?;

                let valid = if self.cur_pos() == start {
                    is_ident_start(ch)
                } else {
                    is_ident_part(ch)
                };

                if !valid {
                    self.emit_error(esc_start, SyntaxError::InvalidIdentChar);
                }

                word.push(ch);
            } else {
                break;
            }
        }

        if word.is_empty() {
            Ok((JsWord::from(self.input.slice_to_cur(start)), contains_esc))
        } else {
            Ok((JsWord::from(word), contains_esc))
        }
    }

    // Read an identifier or keyword token. Will check for reserved
    // words when necessary.
    // See https://tc39.github.io/ecma262/#sec-names-and-keywords
    fn read_ident_or_keyword(&mut self) -> LexResult<Token> {
        debug_assert!(
            self.cur() == Some('\\')
                || (self.cur().is_some() && is_ident_start(self.cur().unwrap()))
        );

        let start = self.cur_pos();

        let (text, contains_esc) = self.read_word()?;

        let word = Word::from(text);

        // Note: ctx is store in lexer because of this error.
        // 'await' and 'yield' may have semantic of reserved word, which means lexer
        // should know context or parser should handle this error. Our approach to this
        // problem is former one.
        if contains_esc && self.ctx.is_reserved(&word) {
            self.error(
                start,
                SyntaxError::EscapeInReservedWord { word: word.into() },
            )?
        } else {
            Ok(Word(word))
        }
    }

    // TODO: Verify that the raw value is spec compliant/look at swc/babel's implementations.
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

        while let Some(c) = self.cur() {
            if c == '`' || (c == '$' && self.peek() == Some('{')) {
                if start == self.cur_pos() && self.state.last_was_tpl_element() {
                    if c == '$' {
                        self.bump();
                        self.bump();
                        return Ok(tok!("${"));
                    } else {
                        self.bump();
                        return Ok(tok!('`'));
                    }
                }

                let raw = self.input.slice_to_cur(start);

                let cooked = match cooked {
                    CookedType::SameAsRaw => Some(raw.into()),
                    CookedType::DifferentFromRaw(ref mut existing_cooked) => {
                        let chunk = self.input.slice_to_cur(cooked_chunk_start);
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

            if c == '\\' {
                has_escape = true;

                match cooked {
                    CookedType::SameAsRaw => {
                        let new_cooked = String::from(self.input.slice_to_cur(start));

                        cooked = CookedType::DifferentFromRaw(new_cooked);
                    }
                    CookedType::DifferentFromRaw(ref mut existing_cooked) => {
                        let new_chunk = self.input.slice_to_cur(cooked_chunk_start);
                        existing_cooked.push_str(new_chunk);
                    }
                    _ => {}
                }

                match self.read_escaped_char(true) {
                    Ok(Some(s)) => match cooked {
                        CookedType::DifferentFromRaw(ref mut existing_cooked) => {
                            existing_cooked.push(s);

                            cooked_chunk_start = self.cur_pos();
                        }
                        _ => {}
                    },
                    Ok(None) => {}
                    Err(..) => {
                        cooked = CookedType::None;
                    }
                }
            } else if is_line_break(c) {
                self.state.had_line_break = true;
                if c == '\r' && self.peek() == Some('\n') {
                    match cooked {
                        CookedType::SameAsRaw => {
                            let mut new_cooked = String::from(self.input.slice_to_cur(start));
                            new_cooked.push('\n');

                            cooked = CookedType::DifferentFromRaw(new_cooked);
                        }
                        CookedType::DifferentFromRaw(ref mut existing_cooked) => {
                            let new_chunk = self.input.slice_to_cur(cooked_chunk_start);
                            existing_cooked.push_str(new_chunk);
                            existing_cooked.push('\n');
                        }
                        _ => {}
                    }

                    self.bump(); // '\r'
                    self.bump(); // '\n'

                    match cooked {
                        CookedType::DifferentFromRaw(..) => {
                            cooked_chunk_start = self.cur_pos();
                        }
                        _ => {}
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

    pub fn set_expr_allowed(&mut self, allow: bool) {
        self.state.is_expr_allowed = allow;
    }
}

fn pos_span(p: BytePos) -> Span {
    Span::new(p, p, Default::default())
}
