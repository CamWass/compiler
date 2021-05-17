pub mod identifier;
mod number;
mod state;
mod util;

use crate::{context::Context, op, token::*};
use global_common::{input::Input, BytePos, Pos};
use identifier::{is_ident_part, is_ident_start};
use state::State;
pub use state::{TokenContext, TokenContexts};
use std::iter::FusedIterator;
use swc_atoms::JsWord;
use util::{char_bytes, char_literals, is_iterator, is_line_break, is_valid_regex_flag};

#[derive(Clone)]
pub struct Lexer<I: Input> {
    state: State,
    input: I,
    pub(crate) ctx: Context,
}

impl<I: Input> FusedIterator for Lexer<I> {}

impl<I: Input> Iterator for Lexer<I> {
    type Item = TokenAndSpan;
    fn next(&mut self) -> Option<Self::Item> {
        self.state.last_tok_start = self.state.start;
        self.state.had_line_break = false;

        // Skip the space after the previous token, so that the next one's
        // `start` will point to the right position.
        if self.state.can_skip_space() {
            self.skip_space();
        };

        let start = self.cur_pos();
        self.state.start = start;

        let token = self.read_token();

        if let Some(ref token) = token {
            self.state.update(start, &token);
            self.state.last_tok_end = self.last_pos();
        }

        let span = self.span(start);

        token.map(|token| {
            // Attach span to token.
            TokenAndSpan {
                token,
                had_line_break: self.state.had_line_break,
                span,
            }
        })
    }
}

impl<I: Input> Lexer<I> {
    pub fn new(input: I) -> Self {
        Lexer {
            state: State::new(),
            input,
            ctx: Default::default(),
        }
    }

    fn read_token(&mut self) -> Option<Token> {
        let ch = match self.cur() {
            Some(c) => c,
            None => {
                return None;
            }
        };

        match ch {
            // The interpretation of a dot depends on whether it is followed
            // by a digit or another two dots.
            '.' => Some(self.read_token_dot()),
            // Punctuation tokens.
            '(' => {
                self.bump();
                Some(LParen)
            }
            ')' => {
                self.bump();
                Some(RParen)
            }
            ';' => {
                self.bump();
                Some(Semi)
            }
            ',' => {
                self.bump();
                Some(Comma)
            }
            '[' => {
                self.bump();
                Some(LBracket)
            }
            ']' => {
                self.bump();
                Some(RBracket)
            }
            '{' => {
                self.bump();
                Some(LBrace)
            }
            '}' => {
                self.bump();
                Some(RBrace)
            }
            ':' => {
                self.bump();
                Some(Colon)
            }
            '?' => Some(self.read_token_question()),
            '`' => {
                self.bump();
                Some(BackQuote)
            }
            '0' => {
                Some(match self.peek() {
                    // '0x', '0X' - hex number
                    Some('x') | Some('X') => self.read_radix_number(16),
                    // '0o', '0O' - octal number
                    Some('o') | Some('O') => self.read_radix_number(8),
                    // '0b', '0B' - binary number
                    Some('b') | Some('B') => self.read_radix_number(2),

                    _ => self.read_number(false),
                })
            }
            // Anything else beginning with a digit is an integer, octal
            // number, or float.
            '1'..='9' => Some(self.read_number(false)),

            // Quotes produce strings.
            '"' | '\'' => Some(self.read_string(ch)),

            '/' => Some(self.read_token_slash()),
            '%' | '*' => Some(self.read_token_mult_modulo(ch)),
            '|' | '&' => Some(self.read_token_pipe_amp(ch)),
            '^' => Some(self.read_token_caret()),
            '+' | '-' => self.read_token_plus_min(ch),
            '<' | '>' => self.read_token_lt_gt(ch),
            '=' | '!' => Some(self.read_token_eq_excl(ch)),
            '~' => {
                self.bump();
                Some(tok!('~'))
            }
            '@' => {
                self.bump();
                Some(At)
            }
            '#' => self.read_token_number_sign(),
            // Identifier or keyword. '\uXXXX' sequences are allowed in
            // identifiers, so '\' also dispatches to that.
            ch if ch == '\\' || is_ident_start(ch) => Some(self.read_ident_or_keyword()),
            _ => {
                // throw self.raise(
                //   self.state.pos,
                //   Errors.InvalidOrUnexpectedToken,
                //   String.fromCodePoint(code),
                // );
                panic!(
                    "InvalidOrUnexpectedToken `{:#?}` at position {:?}",
                    std::char::from_u32(ch.into()),
                    self.cur_pos()
                );
            }
        }
    }

    fn read_token_number_sign(&mut self) -> Option<Token> {
        debug_assert!(self.cur() == Some('#'));

        if self.input.is_at_start() && self.read_token_interpreter() {
            return None;
        }

        if let Some(next_char) = self.peek() {
            if next_char.is_ascii_digit() {
                panic!("UnexpectedDigitAfterHash at {:?}", self.cur_pos());
                // throw self.raise(self.state.pos, Errors.UnexpectedDigitAfterHash);
            }
        }

        self.bump(); // '#'
        Some(tok!('#'))
    }

    fn read_token_dot(&mut self) -> Token {
        debug_assert!(self.cur() == Some('.'));

        let next = match self.peek() {
            Some(next) => next,
            None => {
                self.bump(); // '.'
                return tok!('.');
            }
        };

        if next.is_ascii_digit() {
            return self.read_number(true);
        }

        self.bump(); // 1st '.'

        if next == '.' && self.peek_ahead() == Some('.') {
            self.bump(); // 2nd '.'
            self.bump(); // 3rd '.'
            tok!("...")
        } else {
            tok!('.')
        }
    }

    fn read_token_slash(&mut self) -> Token {
        debug_assert_eq!(self.cur(), Some('/'));

        // Regex
        if self.state.is_expr_allowed {
            return self.read_regexp();
        }

        // Divide operator
        self.bump();

        if self.eat(b'=') {
            tok!("/=")
        } else {
            tok!('/')
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

    fn read_token_mult_modulo(&mut self, ch: char) -> Token {
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

        token
    }

    fn read_token_pipe_amp(&mut self, ch: char) -> Token {
        debug_assert!(ch == '|' || ch == '&');
        debug_assert!(self.cur() == Some(ch));

        self.bump();
        let token = if ch == '&' { BitAnd } else { BitOr };

        // '|=', '&='
        if self.is(b'=') {
            self.bump();

            return AssignOp(match token {
                BitAnd => BitAndAssign,
                BitOr => BitOrAssign,
                _ => unreachable!(),
            });
        }

        // '||', '&&'
        if self.is(ch as u8) {
            self.bump();

            if self.is(b'=') {
                self.bump();
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
        debug_assert!(self.cur() == Some('^'));
        // Bitwise xor
        self.bump(); // '^'
        if self.is(b'=') {
            self.bump(); // '='
            AssignOp(BitXorAssign)
        } else {
            BinOp(BitXor)
        }
    }

    fn read_token_plus_min(&mut self, ch: char) -> Option<Token> {
        debug_assert!(ch == '+' || ch == '-');
        debug_assert!(self.cur() == Some(ch));

        self.bump(); // '+' or '-'

        if self.is(ch as u8) {
            // '++', '--'
            self.bump();

            // Handle '-->' line comment
            if !self.ctx.module
                && (self.state.had_line_break || self.state.last_tok_end == BytePos(0))
                && ch == '-'
                && self.eat(b'>')
            {
                self.skip_line_comment(0);
                self.skip_space();
                self.read_token()
            } else if ch == '+' {
                Some(PlusPlus)
            } else {
                Some(MinusMinus)
            }
        } else if self.is(b'=') {
            // '+=', '-='
            self.bump();
            Some(AssignOp(if ch == '+' { AddAssign } else { SubAssign }))
        } else {
            // '+', '-'
            Some(BinOp(if ch == '+' { Add } else { Sub }))
        }
    }

    fn read_token_lt_gt(&mut self, ch: char) -> Option<Token> {
        debug_assert!(ch == '<' || ch == '>');
        debug_assert!(self.cur() == Some(ch));

        self.bump(); // '<' or '>'

        // `<!--`, an XML-style comment that should be interpreted as a line comment
        if !self.ctx.module
            && ch == '<'
            && self.is(b'!')
            && self.peek() == Some('-')
            && self.peek_ahead() == Some('-')
        {
            self.skip_line_comment(4);
            self.skip_space();
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

        Some(token)
    }

    fn read_token_eq_excl(&mut self, ch: char) -> Token {
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
                    BinOp(NotEqEq)
                } else {
                    // '==='
                    BinOp(EqEqEq)
                }
            } else if ch == '!' {
                // '!='
                BinOp(NotEq)
            } else {
                // '=='
                BinOp(EqEq)
            }
        } else if ch == '=' && self.is(b'>') {
            // "=>"
            self.bump();

            Arrow
        } else if ch == '!' {
            // '!'
            Bang
        } else {
            // '='
            AssignOp(Assign)
        }
    }

    fn read_token_question(&mut self) -> Token {
        debug_assert!(self.cur() == Some('?'));

        self.bump(); // '?'

        let next = self.peek();

        match self.cur() {
            Some('?') => {
                self.bump(); // 2nd '?'
                if next == Some('=') {
                    self.bump(); // '='
                    tok!("??=")
                } else {
                    tok!("??")
                }
            }
            _ => {
                tok!('?')
            }
        }
    }

    fn read_regexp(&mut self) -> Token {
        debug_assert!(self.cur() == Some('/'));

        let start = self.cur_pos();

        self.bump();

        let mut escaped = false;
        let mut in_class = false;

        while let Some(ch) = self.cur() {
            if is_line_break(ch) {
                // Regex literal cannot span multiple lines
                // throw self.raise(start, Errors.UnterminatedRegExp);
                panic!("UnterminatedRegExp at {:?}", start);
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
            panic!("UnterminatedRegExp at {:?}", start);
        }

        let content = self.input.slice_to_cur(start.to_usize() + 1).into();

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

        Regex(content, mods.into())
    }

    fn read_code_point(&mut self) -> char {
        debug_assert!(self.cur().is_some() && self.cur().unwrap().is_digit(16));

        let start = self.cur_pos();
        let val = self.read_int_u32(16, 0, false);

        if let Some(val) = val {
            if 0x0010_FFFF >= val {
                if let Some(ch) = std::char::from_u32(val) {
                    return ch;
                }
            }
        }

        // self.raise(codePos, Errors.InvalidCodePoint);
        panic!("InvalidCodePoint at {:?}", start);
    }

    fn read_unicode_escape(&mut self) -> char {
        debug_assert!(self.is(b'{') || (self.cur().is_some() && self.cur().unwrap().is_digit(16)));

        if self.eat(b'{') {
            let code_pos = self.cur_pos();

            let ch = self.read_code_point();

            if !self.eat(b'}') {
                // self.raise(codePos, Errors.InvalidCodePoint);
                panic!("InvalidCodePoint at {:?}", code_pos);
            }

            ch
        } else {
            self.read_hex_char(4)
        }
    }

    /// See https://tc39.github.io/ecma262/#sec-literals-string-literals
    fn read_string(&mut self, quote: char) -> Token {
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
                out.push_str(self.input.slice_to_cur(chunk_start.to_usize()));

                if let Some(c) = self.read_escaped_char() {
                    out.push(c);
                }

                has_escape = true;

                chunk_start = self.cur_pos();
            } else if ch == char_literals::LINE_SEPARATOR
                || ch == char_literals::PARAGRAPH_SEPARATOR
            {
                self.bump();
                self.state.cur_line += 1;
                self.state.line_start = self.cur_pos();
            } else if is_line_break(ch) {
                // throw self.raise(self.state.start, Errors.UnterminatedString);
                panic!("UnterminatedString at {:?}", self.cur_pos());
            } else {
                self.bump();
            }
        }

        if !self.is(quote as u8) {
            // Reached end of input without seeing closing quote (' or ")
            panic!("UnterminatedRegExp at {:?}", start);
        }

        out.push_str(self.input.slice_to_cur(chunk_start.to_usize()));
        self.bump(); // ' or "
        Token::Str {
            value: out.into(),
            has_escape,
        }
    }

    // // Reads template string tokens.

    // readTmplToken(): void {
    //   let out = "",
    //     chunkStart = self.state.pos,
    //     containsInvalid = false;
    //   for (;;) {
    //     if (self.state.pos >= self.length) {
    //       throw self.raise(self.state.start, Errors.UnterminatedTemplate);
    //     }
    //     const ch = self.input.charCodeAt(self.state.pos);
    //     if (
    //       ch === charCodes.graveAccent ||
    //       (ch === charCodes.dollarSign &&
    //         self.input.charCodeAt(self.state.pos + 1) ===
    //           charCodes.leftCurlyBrace)
    //     ) {
    //       if (self.state.pos === self.state.start && self.match(&TokenTypes::template)) {
    //         if (ch === charCodes.dollarSign) {
    //           self.state.pos += 2;
    //           self.finishToken(&TokenTypes::dollarBraceL);
    //           return;
    //         } else {
    //           self.state.pos += 1;
    //           self.finishToken(&TokenTypes::backQuote);
    //           return;
    //         }
    //       }
    //       out += self.input.slice(chunkStart, self.state.pos);
    //       self.finishToken(&TokenTypes::template, containsInvalid ? null : out);
    //       return;
    //     }
    //     if (ch === charCodes.backslash) {
    //       out += self.input.slice(chunkStart, self.state.pos);
    //       const escaped = self.readEscapedChar(true);
    //       if (escaped === null) {
    //         containsInvalid = true;
    //       } else {
    //         out += escaped;
    //       }
    //       chunkStart = self.state.pos;
    //     } else if (isNewLine(ch)) {
    //       out += self.input.slice(chunkStart, self.state.pos);
    //       self.state.pos += 1;
    //       switch (ch) {
    //         case charCodes.carriageReturn:
    //           if (self.input.charCodeAt(self.state.pos) === charCodes.lineFeed) {
    //             self.state.pos += 1;
    //           }
    //         // fall through
    //         case charCodes.lineFeed:
    //           out += "\n";
    //           break;
    //         default:
    //           out += String.fromCharCode(ch);
    //           break;
    //       }
    //       ++self.state.curLine;
    //       self.state.lineStart = self.state.pos;
    //       chunkStart = self.state.pos;
    //     } else {
    //       self.state.pos += 1;
    //     }
    //   }
    // }

    // Used to read escaped characters
    fn read_escaped_char(&mut self) -> Option<char> {
        debug_assert!(self.cur() == Some('\\'));
        self.bump(); // '\'
        let ch = match self.cur() {
            Some(c) => c,
            None => return None,
        };
        self.bump();

        match ch {
            // Line feed
            'n' => Some('\n'),
            // Carriage return
            'r' => Some('\r'),
            'x' => Some(self.read_hex_char(2)),
            'u' => Some(self.read_unicode_escape()),
            // Tab
            't' => Some('\t'),
            // Backspace
            'b' => Some(char_literals::BACKSPACE),
            // Vertical tab
            'v' => Some(char_literals::LINE_TABULATION),
            'f' => Some(char_literals::FORM_FEED),
            char_literals::CARRIAGE_RETURN | char_literals::LINE_FEED => {
                if ch == char_literals::CARRIAGE_RETURN && self.is(char_bytes::LINE_FEED) {
                    self.bump();
                }

                self.state.line_start = self.cur_pos();
                self.state.cur_line += 1;

                None
            }
            char_literals::LINE_SEPARATOR | char_literals::PARAGRAPH_SEPARATOR => None,
            '8' | '9' => {
                todo!();
                // if in_template {
                //     None
                // } else {
                //     // self.recordStrictModeErrors(
                //     //   self.state.pos - 1,
                //     //   Errors.StrictNumericEscape,
                //     // );
                //     panic!()
                // }
            }
            '0'..='7' => {
                // let codePos = self.state.pos - 1;

                //   let first_c = if ch == '0' {
                //     match self.peek() {
                //         Some(next) if next.is_digit(8) => ch,
                //         // \0 is not an octal literal nor decimal literal.
                //         _ => return Ok(Some('\u{0000}'.into())),
                //     }
                // } else {
                //     ch
                // };

                let mut value = match ch.to_digit(8) {
                    Some(v) => {
                        self.bump();
                        v
                    }
                    _ => return None,
                };

                match self.cur().and_then(|c| c.to_digit(8)) {
                    Some(v) => {
                        value = value * 8 + v;
                        self.bump();
                    }
                    _ => unsafe { return Some(std::char::from_u32_unchecked(value)) },
                }

                match self.cur().and_then(|c| c.to_digit(8)) {
                    Some(v) => {
                        value = value * 8 + v;
                        self.bump();
                    }
                    _ => unsafe { return Some(std::char::from_u32_unchecked(value)) },
                }

                // TODO:
                //  let next_char_is_8_or_9 = match self.peek() {
                //    Some(c) if c=='8' || c=='9' => true,
                //    _ => false
                //  };
                //   if (
                //     octalStr !== "0" || next_char_is_8_or_9
                //   ) {
                //     if (inTemplate) {
                //       return null;
                //     } else {
                //       self.recordStrictModeErrors(codePos, Errors.StrictNumericEscape);
                //     }
                //   }

                unsafe { return Some(std::char::from_u32_unchecked(value)) };
            }
            _ => Some(ch),
        }
    }

    // Used to read character escape sequences ('\x', '\u').
    fn read_hex_char(&mut self, len: u8) -> char {
        debug_assert!(self.cur().is_some() && self.cur().unwrap().is_digit(16));
        debug_assert!(len == 2 || len == 4);

        let val = self.read_int_u32(16, len, false);

        if let Some(val) = val {
            if let Some(ch) = std::char::from_u32(val) {
                return ch;
            }
        }

        // self.raise(codePos, Errors.InvalidEscapeSequence);
        panic!("InvalidEscapeSequence at {:?}", self.cur_pos());
    }

    // Read an identifier, and return it as a string.
    fn read_word(&mut self) -> (JsWord, bool) {
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
            if is_ident_part(ch) || (self.state.is_iterator && ch == '@') {
                self.bump();
                word.push(ch);
            } else if ch == '\\' {
                contains_esc = true;

                let esc_start = self.cur_pos();

                self.bump(); // '\'
                if self.cur() != Some('u') {
                    // self.raise(self.state.pos, Errors.MissingUnicodeEscape);
                    panic!("MissingUnicodeEscape at {:?}", self.cur_pos());
                    // continue;
                }

                self.bump(); // 'u'

                let ch = self.read_unicode_escape();

                let valid = if self.cur_pos() == start {
                    is_ident_start(ch)
                } else {
                    is_ident_part(ch)
                };

                if !valid {
                    // self.raise(escStart, Errors.EscapedCharNotAnIdentifier);
                    panic!("EscapedCharNotAnIdentifier at {:?}", esc_start);
                }

                word.push(ch);
            } else {
                break;
            }
        }

        if word.is_empty() {
            (
                JsWord::from(self.input.slice_to_cur(start.to_usize())),
                contains_esc,
            )
        } else {
            (JsWord::from(word), contains_esc)
        }
    }

    // Read an identifier or keyword token. Will check for reserved
    // words when necessary.
    // See https://tc39.github.io/ecma262/#sec-names-and-keywords
    fn read_ident_or_keyword(&mut self) -> Token {
        debug_assert!(
            self.cur() == Some('\\')
                || (self.cur().is_some() && is_ident_start(self.cur().unwrap()))
        );

        let start = self.cur_pos();

        let (text, contains_esc) = self.read_word();

        // TODO: find test cases for this:
        // Allow @@iterator and @@asyncIterator as a identifier only inside type
        if self.state.is_iterator && (!is_iterator(&text)/* || !self.state.in_type*/) {
            // self.raise(self.state.pos, Errors.InvalidIdentifier, word);
            panic!("InvalidIdentifier '{}' at {:?}", &text, self.cur_pos());
        }

        let word = Word::from(text);

        // Note: ctx is store in lexer because of this error.
        // 'await' and 'yield' may have semantic of reserved word, which means lexer
        // should know context or parser should handle this error. Our approach to this
        // problem is former one.
        if contains_esc && self.ctx.is_reserved(&word) {
            // self.error(
            //     start,
            //     SyntaxError::EscapeInReservedWord { word: word.into() },
            // )?
            // self.raise(self.state.start, Errors.InvalidEscapedReservedWord, kw);
            panic!("InvalidEscapedReservedWord '{:?}' at {:?}", word, start);
        } else {
            Word(word)
        }
    }

    pub fn set_expr_allowed(&mut self, allow: bool) {
        self.state.is_expr_allowed = allow;
    }
}
