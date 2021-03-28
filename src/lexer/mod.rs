mod context;
mod identifier;
mod input;
mod state;
//TODO: temp pub
pub mod types;

extern crate regex;

use regex::bytes::Regex;

use state::State;

use identifier::{is_ident_part, is_ident_start};

use types::{TokenType, TokenTypes};

use crate::parser::{
  ast::{Comment, NodeKind},
  location::{Position, SourceLocation},
};

use crate::options::{Options, SourceType};

use context::TokContext;

#[derive(Debug)]
pub enum TokenValue<'a> {
  ByteSlice(&'a [u8]),
  Numeric(usize),
  String(String),
  Regex(&'a str, String),
}

// Object type used to represent tokens. Note that normally, tokens
// simply exist as properties on the parser object. This is only
// used for the onToken callback and the external tokenizer.
pub struct Token<'a> {
  kind: &'a TokenType<'a>,
  value: Option<&'a TokenValue<'a>>,
  start: usize,
  end: usize,
  // loc: SourceLocation,
}

impl<'a> Token<'a> {
  pub fn new(state: &'a State) -> Self {
    Self {
      kind: &state.kind,
      value: state.value.as_ref(),
      start: state.start,
      end: state.end,
      // loc: new SourceLocation(state.startLoc, state.endLoc),
    }
  }
}

// pub struct Lexer<'src> {
//   bytes: &'src [u8],
//   index: usize,
// }

// impl<'src> Lexer<'src> {
//   pub fn from_str(string: &'src str) -> Self {
//     Self {
//       bytes: string.as_bytes(),
//       index: 0,
//     }
//   }

//   // Get the next byte and advance the index.
//   pub fn next(&mut self) -> Option<&u8> {
//     self.index += 1;
//     self.bytes.get(self.index)
//   }
// }

fn is_iterator(word: &str) -> bool {
  word == "@@iterator" || word == "@@asyncIterator"
}

lazy_static! {
  // Matches a whole line break (where CRLF is considered a single
// line break). Used to count lines.
  static ref LINEBREAK: Regex =Regex::new(r"\r\n?|[\n\u2028\u2029]").unwrap();
}

/// See https://tc39.github.io/ecma262/#sec-line-terminators
fn is_line_break(ch: char) -> bool {
  matches!(ch, '\r' | '\n' | '\u{2028}' | '\u{2029}')
}

// https://tc39.github.io/ecma262/#sec-line-terminators
// fn isNewLine(code: u8) -> bool {
//   match code {
//         10 // lineFeed
//         |
//         13 //carriageReturn
//     |
//          8232 // lineSeparator
//         |8233 // paragraphSeparator
//         => {
//           true
//         },
//         _ => false
//     }
// }

// https://tc39.github.io/ecma262/#sec-line-terminators
fn is_new_line(ch: char) -> bool {
  match ch {
    '\u{000A}' | // lineFeed
    '\u{000D}' | // carriageReturn
    '\u{2028}' | // lineSeparator
    '\u{2029}' => // paragraphSeparator
      true,

    _ => false
  }
}

// export let skipWhiteSpace = /(?:\s|\/\/.*|\/\*[^]*?\*\/)*/g;

// https://tc39.github.io/ecma262/#sec-white-space
fn is_whitespace(ch: char) -> bool {
  match ch {
    '\u{0009}' | // CHARACTER TABULATION
    '\u{000b}' | // LINE TABULATION
    '\u{000c}' | // FORM FEED
    '\u{0020}' | // Space
    '\u{00A0}' | // Non-breaking space
    '\u{1680}' | // OGHAM SPACE MARK
    '\u{2000}' | // EN QUAD
    '\u{2001}' | // EM QUAD
    '\u{2002}' | // EN SPACE
    '\u{2003}' | // EM SPACE
    '\u{2004}' | // THREE-PER-EM SPACE
    '\u{2005}' | // FOUR-PER-EM SPACE
    '\u{2006}' | // SIX-PER-EM SPACE
    '\u{2007}' | // FIGURE SPACE
    '\u{2008}' | // PUNCTUATION SPACE
    '\u{2009}' | // THIN SPACE
    '\u{200a}' | // HAIR SPACE
    '\u{202f}' | // NARROW NO-BREAK SPACE
    '\u{205f}' | // MEDIUM MATHEMATICAL SPACE
    '\u{3000}' | // IDEOGRAPHIC SPACE
    '\u{feff}' => // ZERO WIDTH NO-BREAK SPACE
      true,

    _ => false
  }
}

mod CharCodes {
  pub const LINE_FEED: u32 = 10; // '\n'
}

// import type { Options } from "../options";
// import * as N from "../types";
// import type { Position } from "../util/location";
// import * as charCodes from "charcodes";
// import { isIdentifierStart, isIdentifierChar } from "../util/identifier";
// import { types as tt, keywords as keywordTypes, type TokenType } from "./types";
// import { type TokContext, types as ct } from "./context";
// import ParserErrors, { Errors } from "../parser/error";
// import { SourceLocation } from "../util/location";
// import {
//   lineBreak,
//   lineBreakG,
//   isNewLine,
//   isWhitespace,
//   skipWhiteSpace,
// } from "../util/whitespace";
// import State from "./state";

fn is_valid_regex_flag(ch: char) -> bool {
  matches!(ch, 'g' | 'm' | 's' | 'i' | 'y' | 'u')
}

fn is_forbidden_numeric_separator_sibling(c: char, radix: u8) -> bool {
  if radix == 16 {
    // These characters are forbidden from being an immediate sibling of
    // a NumericLiteralSeparator '_' in hex numbers.
    matches!(c, '.' | 'X' | '_' | 'x')
  } else {
    // These characters are forbidden from being an immediate sibling of
    // a NumericLiteralSeparator '_' in decimal, binary, and octal numbers.
    matches!(c, '.' | 'B' | 'E' | 'O' | '_' | 'b' | 'e' | 'o')
  }
}

#[rustfmt::skip]
fn is_allowed_numeric_separator_siblings(c: char, radix: u8) -> bool {
  if radix == 16 {
    // Hex
    // 0 - 9, A - F, a - f,
    matches!(
      c,
      '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' |
      'A' | 'B' | 'C' | 'D' | 'E' | 'F' |
      'a' | 'b' | 'c' | 'd' | 'e' | 'f'
    )
  } else if radix == 10 {
    // Dec
    // 0 - 9
    matches!(
      c,
      '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    )
  } else if radix == 8 {
    // Oct
    // 0 - 7
    matches!(
      c,
      '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7'
    )
  } else {
    // Bin
    // 0 - 1
    matches!(
      c,
      '0' | '1'
    )
  }
}

enum TokenOrComment<'a> {
  Token(Token<'a>),
  Comment(Comment),
}

pub struct Lexer<'a> {
  // Forward-declarations
  // parser/util.js
  /*::
  +hasPrecedingLineBreak: () => bool;
  +unexpected: (pos?: ?number, messageOrType?: string | TokenType) => empty;
  +expectPlugin: (name: string, pos?: ?number) => true;
  */
  is_lookahead: bool,

  // Token store.
  tokens: Vec<TokenOrComment<'a>>,
  pub state: State<'a>,
  input: &'a str,
  length: usize,
  options: &'a Options,
  in_module: bool,
}

impl<'a> Lexer<'a> {
  pub fn new(options: &'a Options, input: &'a str) -> Self {
    // super();
    Lexer {
      state: State::new(options),
      // self.state.init(options);
      input,
      length: input.len(),
      is_lookahead: false,
      tokens: Vec::new(),
      options,
      in_module: options.source_type == SourceType::Module,
    }
  }

  fn pushToken(&mut self, token: TokenOrComment<'a>) {
    // Pop out invalid tokens trapped by try-catch parsing.
    // Those parsing branches are mainly created by typescript and flow plugins.
    self.tokens.truncate(self.state.tokens_length);
    self.tokens.push(token);
    self.state.tokens_length += 1;
  }

  // // Move to the next token

  // next(): void {
  //   if (!self.isLookahead) {
  //     self.checkKeywordEscapes();
  //     if (self.options.tokens) {
  //       self.pushToken(new Token(self.state));
  //     }
  //   }

  //   self.state.lastTokEnd = self.state.end;
  //   self.state.lastTokStart = self.state.start;
  //   self.state.lastTokEndLoc = self.state.endLoc;
  //   self.state.lastTokStartLoc = self.state.startLoc;
  //   self.nextToken();
  // }

  pub fn peek(&self) -> Option<char> {
    self.input.chars().nth(self.state.pos + 1)
  }

  fn peek_ahead(&self) -> Option<char> {
    self.input.chars().nth(self.state.pos + 2)
  }

  // // TODO

  // eat(type: TokenType): bool {
  //   if (self.match(type)) {
  //     self.next();
  //     return true;
  //   } else {
  //     return false;
  //   }
  // }

  // // TODO

  // match(type: TokenType): bool {
  //   return self.state.type === type;
  // }

  // // TODO

  // lookahead(): State {
  //   const old = self.state;
  //   self.state = old.clone(true);

  //   self.isLookahead = true;
  //   self.next();
  //   self.isLookahead = false;

  //   const curr = self.state;
  //   self.state = old;
  //   return curr;
  // }

  // nextTokenStart(): number {
  //   return self.nextTokenStartSince(self.state.pos);
  // }

  // nextTokenStartSince(pos: number): number {
  //   skipWhiteSpace.lastIndex = pos;
  //   const skip = skipWhiteSpace.exec(self.input);
  //   // $FlowIgnore: The skipWhiteSpace ensures to match any string
  //   return pos + skip[0].length;
  // }

  // lookaheadCharCode(): number {
  //   return self.input.charCodeAt(self.nextTokenStart());
  // }

  // // Toggle strict mode. Re-reads the next number or string to please
  // // pedantic tests (`"use strict"; 010;` should fail).

  // setStrict(strict: bool): void {
  //   self.state.strict = strict;
  //   if (strict) {
  //     // Throw an error for any string decimal escape found before/immediately
  //     // after a "use strict" directive. Strict mode will be set at parse
  //     // time for any literals that occur after the next node of the strict
  //     // directive.
  //     self.state.strictErrors.forEach((message, pos) =>
  //       /* eslint-disable @babel/development-internal/dry-error-messages */
  //       self.raise(pos, message),
  //     );
  //     self.state.strictErrors.clear();
  //   }
  // }

  fn curContext(&self) -> Option<&TokContext> {
    self.state.context.last()
  }

  // Read a single token, updating the parser object's token-related
  // properties.

  pub fn next_token(&mut self) {
    // let curContext = self.curContext();
    // if let Some(context) = curContext {
    //   if !context.preserve_space {
    //     self.skip_space();
    //   }
    // }
    // TODO: the following line is just for testing, replace with above.
    self.skip_space();
    self.state.start = self.state.pos;
    self.state.start_loc = self.state.curPosition();
    if self.state.pos >= self.length {
      self.finish_token(&TokenTypes::eof, None);
      return;
    }

    // let override = curContext?.override;
    // if (override) {
    // override(self);
    // } else {

    if let Some(ch) = self.input.chars().nth(self.state.pos) {
      self.get_token_from_code(ch);
    } else {
      panic!();
    }

    // }
  }

  fn has_preceding_line_break(&self) -> bool {
    let slice = &self.input[self.state.last_tok_end..self.state.start];
    LINEBREAK.is_match(slice.as_bytes())
  }

  fn push_comment(
    &mut self,
    block: bool,
    text: &str,
    start: usize,
    end: usize,
    start_loc: Position,
    end_loc: Position,
  ) {
    let kind = if block {
      NodeKind::CommentBlock
    } else {
      NodeKind::CommentLine
    };

    let comment = Comment {
      kind,
      value: String::from(text),
      start,
      end,
      loc: SourceLocation {
        start: start_loc,
        end: end_loc,
        filename: None,
      },
    };

    if self.options.tokens {
      self.pushToken(TokenOrComment::Comment(comment.clone()));
    }
    self.state.comments.push(comment);
    // TODO:
    // self.addComment(comment);
  }

  fn skip_block_comment(&mut self) {
    let start_loc = self.state.curPosition();
    let start = self.state.pos;
    let end = match self.input[self.state.pos + 2..].find("*/") {
      Some(n) => n,
      _ => panic!("UnterminatedComment at {}", start),
    };

    self.state.pos = end + 2;

    let text = &self.input[start..self.state.pos];

    for mat in LINEBREAK.find_iter(text.as_bytes()) {
      self.state.cur_line += 1;
      self.state.line_start = mat.end();
    }

    // If we are doing a lookahead right now we need to advance the position (above code)
    // but we do not want to push the comment to the state.
    if !self.is_lookahead {
      self.push_comment(
        true,
        &self.input[start + 2..end],
        start,
        self.state.pos,
        start_loc,
        self.state.curPosition(),
      );
    }
  }

  fn skip_line_comment(&mut self, start_skip: usize) {
    let start = self.state.pos;
    let start_loc = self.state.curPosition();
    self.state.pos += start_skip;
    let mut ch = self.input.chars().nth(self.state.pos);
    if self.state.pos < self.length {
      self.state.pos += 1;
      while ch.is_some() && !is_new_line(ch.unwrap()) && self.state.pos < self.length {
        ch = self.input.chars().nth(self.state.pos);
        self.state.pos += 1;
      }
    }

    // If we are doing a lookahead right now we need to advance the position (above code)
    // but we do not want to push the comment to the state.
    if !self.is_lookahead {
      self.push_comment(
        false,
        &self.input[start + start_skip..self.state.pos],
        start,
        self.state.pos,
        start_loc,
        self.state.curPosition(),
      );
    }
  }

  // Called at the start of the parse and after every token. Skips
  // whitespace and comments, and.

  fn skip_space(&mut self) {
    while let Some(ch) = self.input.chars().nth(self.state.pos) {
      match ch {
        // Space
        // Non-breaking space
        // Tab
        '\u{0020}' | '\u{00A0}' | '\u{0009}' => {
          self.state.pos += 1;
          
        }
        // Carriage return
        // Line feed
        // Line separator
        // Paragraph separator
        '\u{000D}' | '\u{000A}' | '\u{2028}' | '\u{2029}' => {
          if ch == '\u{000D}' && self.peek() == Some('\u{000A}') {
            self.state.pos += 1;
          }

          self.state.pos += 1;
          self.state.cur_line += 1;
          self.state.line_start = self.state.pos;

          let ch = self.input.chars().nth(self.state.pos);
          let code = match ch {
            Some(c) => Some(c as u32),
            None => None
          };

          
        }

        '/' => match self.peek() {
          Some('*') => {
            self.skip_block_comment();
            // return;
          }

          Some('/') => {
            self.skip_line_comment(2);
            // return;
          }

          _ => return,
        },
        _ => {
          if is_whitespace(ch) {
            self.state.pos += 1;
          } else {
            return;
          }
        }
      }
    }
  }

  // Called at the end of every token. Sets `end`, `val`, and
  // maintains `context` and `exprAllowed`, and skips the space after
  // the token, so that the next one's `start` will point at the
  // right position.

  fn finish_token(&mut self, kind: &'a TokenType, val: Option<TokenValue<'a>>) {
    self.state.end = self.state.pos;
    self.state.end_loc = self.state.curPosition();
    let prev_kind = self.state.kind;
    self.state.kind = kind;
    self.state.value = val;

    if !self.is_lookahead {
      self.update_context(prev_kind)
    };
  }

  // ### Token reading

  // This is the function that is called to fetch the next token. It
  // is somewhat obscure, because it works in character codes rather
  // than characters, and because operator parsing has been inlined
  // into it.
  //
  // All in the name of speed.

  // number sign is "#"
  fn read_token_number_sign(&mut self) {
    if self.state.pos == 0 && self.readToken_interpreter() {
      return;
    }

    // let next = self.peek();
    if self.peek().is_some() {
      panic!("UnexpectedDigitAfterHash {}", self.state.pos);
      // throw self.raise(self.state.pos, Errors.UnexpectedDigitAfterHash);
    }

    // if
    //   next == charCodes.leftCurlyBrace ||
    //   (next == charCodes.leftSquareBracket && self.hasPlugin("recordAndTuple"))
    //  {
    //   // When we see `#{`, it is likely to be a hash record.
    //   // However we don't yell at `#[` since users may intend to use "computed private fields",
    //   // which is not allowed in the spec. Throwing expecting recordAndTuple is
    //   // misleading
    //   self.expectPlugin("recordAndTuple");
    //   if (self.getPluginOption("recordAndTuple", "syntaxType") !== "hash") {
    //     throw self.raise(
    //       self.state.pos,
    //       next === charCodes.leftCurlyBrace
    //         ? Errors.RecordExpressionHashIncorrectStartSyntaxType
    //         : Errors.TupleExpressionHashIncorrectStartSyntaxType,
    //     );
    //   }

    //   if next == charCodes.leftCurlyBrace {
    //     // #{
    //     self.finishToken(&TokenTypes::braceHashL, None);
    //   } else {
    //     // #[
    //     self.finishToken(&TokenTypes::bracketHashL, None);
    //   }
    //   self.state.pos += 2;
    // } else {
    self.finish_op(&TokenTypes::hash, 1);
    // }
  }

  fn readToken_dot(&mut self) {
    let next = self.peek();
    if let Some(next_char) = next {
      if next_char.is_ascii_digit() {
        self.read_number(true);
        return;
      }
    }

    if next == Some('.') && self.peek_ahead() == Some('.') {
      self.state.pos += 3;
      self.finish_token(&TokenTypes::ellipsis, None);
    } else {
      self.state.pos += 1;
      self.finish_token(&TokenTypes::dot, None);
    }
  }

  fn readToken_slash(&mut self) {
    // '/'
    if self.state.expr_allowed && !self.state.in_type {
      self.state.pos += 1;
      self.read_regexp();
      return;
    }

    let next = self.peek();
    if next == Some('-') {
      self.finish_op(&TokenTypes::assign, 2);
    } else {
      self.finish_op(&TokenTypes::slash, 1);
    }
  }

  fn readToken_interpreter(&mut self) -> bool {
    if self.state.pos != 0 || self.length < 2 {
      return false;
    };

    let start = self.state.pos;

    let mut ch = self.peek();
    if ch != Some('!') {
      return false;
    };

    self.state.pos += 2;

    while ch.is_some() && !is_new_line(ch.unwrap()) && self.state.pos < self.length {
      ch = self.input.chars().nth(self.state.pos);
      self.state.pos += 1;
    }

    let value = &self.input[start + 2..self.state.pos];

    let s = String::from(value);

    self.finish_token(
      &TokenTypes::interpreterDirective,
      Some(TokenValue::String(s)),
    );

    true
  }

  fn read_token_mult_modulo(&mut self, ch: char) {
    // '%*'
    let mut kind = if ch == '*' {
      &TokenTypes::star
    } else {
      &TokenTypes::modulo
    };
    let mut width = 1;
    let mut next = self.peek();
    let expr_allowed = self.state.expr_allowed;

    // Exponentiation operator **
    if ch == '*' && next == Some('*') {
      width += 1;
      next = self.peek_ahead();
      kind = &TokenTypes::exponent;
    }

    if next == Some('=') && !expr_allowed {
      width += 1;
      kind = &TokenTypes::assign;
    }

    self.finish_op(kind, width);
  }

  fn read_token_pipe_amp(&mut self, ch: char) {
    // '||' '&&' '||=' '&&='
    let next = self.peek();

    if let Some(next_char) = next {
      if next_char == ch {
        if self.peek_ahead() == Some('=') {
          self.finish_op(&TokenTypes::assign, 3);
        } else {
          let kind = if ch == '|' {
            &TokenTypes::logicalOR
          } else {
            &TokenTypes::logicalAND
          };
          self.finish_op(kind, 2);
        }
        return;
      }
    }

    if ch == '|' {
      // '|>'
      if next == Some('>') {
        self.finish_op(&TokenTypes::pipeline, 2);
        return;
      }
      // '|}'
      // if
      //   self.hasPlugin("recordAndTuple") &&
      //   next === charCodes.rightCurlyBrace
      //  {
      //   if self.getPluginOption("recordAndTuple", "syntaxType") !== "bar" {
      //     throw self.raise(
      //       self.state.pos,
      //       Errors.RecordExpressionBarIncorrectEndSyntaxType,
      //     );
      //   }

      //   self.finishOp(&TokenTypes::braceBarR, 2);
      //   return;
      // }

      // '|]'
      // if
      //   self.hasPlugin("recordAndTuple") &&
      //   next == charCodes.rightSquareBracket
      //  {
      //   if self.getPluginOption("recordAndTuple", "syntaxType") !== "bar" {
      //     throw self.raise(
      //       self.state.pos,
      //       Errors.TupleExpressionBarIncorrectEndSyntaxType,
      //     );
      //   }

      //   self.finishOp(&TokenTypes::bracketBarR, 2);
      //   return;
      // }
    }

    if next == Some('=') {
      self.finish_op(&TokenTypes::assign, 2);
      return;
    }

    let kind = if ch == '|' {
      &TokenTypes::bitwiseOR
    } else {
      &TokenTypes::bitwiseAND
    };

    self.finish_op(kind, 1);
  }

  fn read_token_caret(&mut self) {
    // '^'
    if self.peek() == Some('=') {
      self.finish_op(&TokenTypes::assign, 2);
    } else {
      self.finish_op(&TokenTypes::bitwiseXOR, 1);
    }
  }

  fn read_token_plus_min(&mut self, ch: char) {
    // '+-'
    let next = self.peek();

    if let Some(next_char) = next {
      if next_char == ch {
        if ch == '-'
          && !self.in_module
          && self.peek_ahead() == Some('>')
          && (self.state.last_tok_end == 0 || self.has_preceding_line_break())
        {
          // A `-->` line comment
          self.skip_line_comment(3);
          self.skip_space();
          self.next_token();
          return;
        }
        self.finish_op(&TokenTypes::incDec, 2);
        return;
      }
    }

    if next == Some('=') {
      self.finish_op(&TokenTypes::assign, 2);
    } else {
      self.finish_op(&TokenTypes::plusMin, 1);
    }
  }

  fn read_token_lt_gt(&mut self, ch: char) {
    // '<>'
    let next = self.peek();
    let mut size = 1;

    if let Some(next_char) = next {
      if ch == next_char {
        size = if ch == '>' && self.peek_ahead() == Some('>') {
          3
        } else {
          2
        };
        if self.input.chars().nth(self.state.pos + size) == Some('=') {
          self.finish_op(&TokenTypes::assign, size + 1);
          return;
        }
        self.finish_op(&TokenTypes::bitShift, size);
        return;
      }
    }

    if next == Some('!')
      && ch == '<'
      && !self.in_module
      && self.peek_ahead() == Some('-')
      && self.input.chars().nth(self.state.pos + 3) == Some('-')
    {
      // `<!--`, an XML-style comment that should be interpreted as a line comment
      self.skip_line_comment(4);
      self.skip_space();
      self.next_token();
      return;
    }

    if next == Some('=') {
      // <= | >=
      size = 2;
    }

    self.finish_op(&TokenTypes::relational, size);
  }

  fn read_token_eq_excl(&mut self, ch: char) {
    // '=!'
    let next = self.peek();

    if next == Some('=') {
      let size = if self.peek_ahead() == Some('=') { 3 } else { 2 };
      self.finish_op(&TokenTypes::equality, size);
    } else if ch == '=' && next == Some('>') {
      // '=>'
      self.state.pos += 2;
      self.finish_token(&TokenTypes::arrow, None);
    } else {
      let kind = if ch == '=' {
        &TokenTypes::eq
      } else {
        &TokenTypes::bang
      };
      self.finish_op(kind, 1);
    }
  }

  fn read_token_question(&mut self) {
    // '?'
    let next = self.peek();
    let next2 = self.peek_ahead();

    let is_next2_a_digit = match next2 {
      Some(c) => match c {
        // TODO: use char.isdigit or CharCodes::is_digit
        '0'..='9' => true,
        _ => false,
      },
      _ => false,
    };

    if next == Some('?') {
      if next2 == Some('=') {
        // '??='
        self.finish_op(&TokenTypes::assign, 3);
      } else {
        // '??'
        self.finish_op(&TokenTypes::nullishCoalescing, 2);
      }
    } else if next == Some('?') && !is_next2_a_digit {
      // '.' not followed by a number
      self.state.pos += 2;
      self.finish_token(&TokenTypes::questionDot, None);
    } else {
      self.state.pos += 1;
      self.finish_token(&TokenTypes::question, None);
    }
  }

  fn get_token_from_code(&mut self, ch: char) {
    match ch {
      // The interpretation of a dot depends on whether it is followed
      // by a digit or another two dots.
      '.' => {
        self.readToken_dot();
      }
      // Punctuation tokens.
      '(' => {
        self.state.pos += 1;
        self.finish_token(&TokenTypes::parenL, None);
      }
      ')' => {
        self.state.pos += 1;
        self.finish_token(&TokenTypes::parenR, None);
      }
      ';' => {
        self.state.pos += 1;
        self.finish_token(&TokenTypes::semi, None);
      }
      ',' => {
        self.state.pos += 1;
        self.finish_token(&TokenTypes::comma, None);
      }
      '[' => {
        // if
        //   self.hasPlugin("recordAndTuple") &&
        //   self.input.charCodeAt(self.state.pos + 1) == charCodes.verticalBar
        //  {
        //   if self.getPluginOption("recordAndTuple", "syntaxType") != "bar" {
        //     throw self.raise(
        //       self.state.pos,
        //       Errors.TupleExpressionBarIncorrectStartSyntaxType,
        //     );
        //   }

        //   // [|
        //   self.finishToken(&TokenTypes::bracketBarL);
        //   self.state.pos += 2;
        // } else {
        self.state.pos += 1;
        self.finish_token(&TokenTypes::bracketL, None);
        // }
      }
      ']' => {
        self.state.pos += 1;
        self.finish_token(&TokenTypes::bracketR, None);
      }
      '{' => {
        // if (
        //   self.hasPlugin("recordAndTuple") &&
        //   self.input.charCodeAt(self.state.pos + 1) === charCodes.verticalBar
        // ) {
        //   if (self.getPluginOption("recordAndTuple", "syntaxType") !== "bar") {
        //     throw self.raise(
        //       self.state.pos,
        //       Errors.RecordExpressionBarIncorrectStartSyntaxType,
        //     );
        //   }

        //   // {|
        //   self.finishToken(&TokenTypes::braceBarL);
        //   self.state.pos += 2;
        // } else {
        self.state.pos += 1;
        self.finish_token(&TokenTypes::braceL, None);
        // }
      }
      '}' => {
        self.state.pos += 1;
        self.finish_token(&TokenTypes::braceR, None);
      }
      ':' => {
        // if (
        //   self.hasPlugin("functionBind") &&
        //   self.input.charCodeAt(self.state.pos + 1) === charCodes.colon
        // ) {
        //   self.finishOp(&TokenTypes::doubleColon, 2);
        // } else {
        self.state.pos += 1;
        self.finish_token(&TokenTypes::colon, None);
        // }
      }
      '?' => {
        self.read_token_question();
      }
      '`' => {
        self.state.pos += 1;
        self.finish_token(&TokenTypes::backQuote, None);
      }
      '0' => {
        let next = self.peek();

        match next {
          Some(c) => {
            // '0x', '0X' - hex number
            if c == 'x' || c == 'X' {
              self.read_radix_number(16);
              return;
            }
            // '0o', '0O' - octal number
            if c == 'o' || c == 'O' {
              self.read_radix_number(8);
              return;
            }
            // '0b', '0B' - binary number
            if c == 'b' || c == 'B' {
              self.read_radix_number(2);
            }
          }
          None => panic!(),
        }
      }
      // Anything else beginning with a digit is an integer, octal
      // number, or float. (fall through)
      '1'..='9' => {
        self.read_number(false);
      }

      // Quotes produce strings.
      '"' | '\'' => {
        self.read_string(ch);
      }

      // Operators are parsed inline in tiny state machines. '=' (charCodes.equalsTo) is
      // often referred to. `finishOp` simply skips the amount of
      // characters it is given as second argument, and returns a token
      // of the type given by its first argument.
      '/' => {
        self.readToken_slash();
      }
      '%' | '*' => {
        self.read_token_mult_modulo(ch);
      }
      '|' | '&' => {
        self.read_token_pipe_amp(ch);
      }
      '^' => self.read_token_caret(),
      '+' | '-' => self.read_token_plus_min(ch),
      '<' | '>' => self.read_token_lt_gt(ch),
      '=' | '!' => self.read_token_eq_excl(ch),
      '~' => self.finish_op(&TokenTypes::tilde, 1),
      '@' => {
        self.state.pos += 1;
        self.finish_token(&TokenTypes::at, None);
      }
      '#' => self.read_token_number_sign(),
      '\\' => self.read_word(),
      _ => {
        if is_ident_start(ch) {
          self.read_word();
        } else {
          // throw self.raise(
          //   self.state.pos,
          //   Errors.InvalidOrUnexpectedToken,
          //   String.fromCodePoint(code),
          // );
          panic!(
            "InvalidOrUnexpectedToken `{:#?}` at position {}",
            std::char::from_u32(ch.into()),
            self.state.pos
          );
        }
      }
    }
  }

  fn finish_op(&mut self, kind: &'static TokenType, size: usize) {
    let slice = &self.input[self.state.pos..self.state.pos + size];
    let string = String::from(slice);
    self.state.pos += size;
    self.finish_token(kind, Some(TokenValue::String(string)));
  }

  fn read_regexp(&mut self) {
    let start = self.state.pos;
    let mut escaped = false;
    let mut in_class = false;
    loop {
      if self.state.pos >= self.length {
        // throw self.raise(start, Errors.UnterminatedRegExp);
      }
      let ch = self.input.chars().nth(self.state.pos);
      if ch.is_some() && is_line_break(ch.unwrap()) {
        // throw self.raise(start, Errors.UnterminatedRegExp);
      }
      if escaped {
        escaped = false;
      } else {
        if ch == Some('[') {
          in_class = true;
        } else if ch == Some(']') && in_class {
          in_class = false;
        } else if ch == Some('/') && !in_class {
          break;
        }
        escaped = ch == Some('\\');
      }
      self.state.pos += 1;
    }
    let content = &self.input[start..self.state.pos];
    self.state.pos += 1;

    let mut mods = String::new();

    while self.state.pos < self.length {
      if let Some(ch) = self.input.chars().nth(self.state.pos) {
        if is_valid_regex_flag(ch) {
          if mods.find(ch).is_some() {
            // self.raise(self.state.pos + 1, Errors.DuplicateRegExpFlags);
          }
        } else if is_ident_part(ch) || ch == '\\' {
          // self.raise(self.state.pos + 1, Errors.MalformedRegExpFlags);
        } else {
          break;
        }

        self.state.pos += 1;
        mods.push(ch);
      } else {
        break;
      }
    }

    let val = TokenValue::Regex(content, mods);

    self.finish_token(&TokenTypes::regexp, Some(val));
  }

  // Read an integer in the given radix. Return null if zero digits
  // were read, the integer value otherwise. When `len` is given, self
  // will return `null` unless the integer has exactly `len` digits.
  // When `forceLen` is `true`, it means that we already know that in case
  // of a malformed number we have to skip `len` characters anyway, instead
  // of bailing out early. For example, in "\u{123Z}" we want to read up to }
  // anyway, while in "\u00Z" we will stop at Z instead of consuming four
  // characters (and thus the closing quote).

  fn read_int(
    &mut self,
    radix: u8,
    len: Option<u8>,
    force_len: bool,
    allow_num_separator: bool,
  ) -> Option<u32> {
    let start = self.state.pos;

    let mut invalid = false;
    let mut total = 0;

    let mut i = 0;

    loop {
      if let Some(length) = len {
        if i < length {
          break;
        }
      }

      let code = match self.input.chars().nth(self.state.pos) {
        Some(ch) => Some(ch as u32),
        None => None,
      };

      if code == Some(b'_' as u32) {
        let prev = self.input.chars().nth(self.state.pos - 1).unwrap();
        let next = self.peek();
        if next.is_none()
          || !is_allowed_numeric_separator_siblings(next.unwrap(), radix)
          || is_forbidden_numeric_separator_sibling(prev, radix)
          || is_forbidden_numeric_separator_sibling(next.unwrap(), radix)
        {
          // self.raise(self.state.pos, Errors.UnexpectedNumericSeparator);
          panic!("UnexpectedNumericSeparator at {}", self.state.pos);
        }

        if !allow_num_separator {
          // self.raise(self.state.pos, Errors.NumericSeparatorInEscapeSequence);
          panic!("NumericSeparatorInEscapeSequence at {}", self.state.pos);
        }

        // Ignore self _ character
        self.state.pos += 1;
        continue;
      }

      let val = if let Some(c) = code {
        if c >= b'a' as u32 {
          Some(c - b'a' as u32 + CharCodes::LINE_FEED)
        } else if c >= b'A' as u32 {
          Some(c - b'A' as u32 + CharCodes::LINE_FEED)
        } else if unsafe { std::char::from_u32_unchecked(c).is_ascii_digit() } {
          Some(c - b'0' as u32) // 0-9
        } else {
          None
        }
      } else {
        None
      };

      let mut v = 0;

      match val {
        Some(value) => {
          if value >= radix as u32 {
            // If we are in "errorRecovery" mode and we found a digit which is too big,
            // don't break the loop.
            if self.options.error_recovery && value <= 9 {
              // value = 0;
              // self.raise(self.state.start + i + 2, Errors.InvalidDigit, radix);
              panic!(
                "InvalidDigit with radix '{}' at {}",
                radix,
                self.state.start + i as usize + 2
              );
            } else if force_len {
              v = 0;
              invalid = true;
            } else {
              v = value;
              break;
            }
          }
        }
        None => {
          if force_len {
            v = 0;
            invalid = true;
          } else {
            break;
          }
        }
      };

      self.state.pos += 1;
      total = total * radix as u32 + v as u32;

      i += 1;
    }

    if self.state.pos == start
      || (len.is_some() && self.state.pos - start != len.unwrap() as usize)
      || invalid
    {
      return None;
    }

    Some(total)
  }

  fn read_radix_number(&mut self, radix: u8) {
    let start = self.state.pos;
    let mut is_big_int = false;

    self.state.pos += 2; // 0x
    let val = self.read_int(radix, None, false, true);
    if val.is_none() {
      // self.raise(self.state.start + 2, Errors.InvalidDigit, radix);
      panic!(
        "InvalidDigit with radix '{}' at pos {}",
        radix,
        self.state.pos + 2
      );
    }
    let next = self.input.chars().nth(self.state.pos);

    if next == Some('n') {
      self.state.pos += 1;
      is_big_int = true;
    } else if next == Some('m') {
      // throw self.raise(start, Errors.InvalidDecimal);
      panic!("InvalidDecimal at {}", start);
    }

    if let Some(next) = self.input.chars().nth(self.state.pos) {
      if is_ident_start(next) {
        // throw self.raise(self.state.pos, Errors.NumberIdentifier);
        panic!("NumberIdentifier at {}", self.state.pos);
      }
    }

    if is_big_int {
      // remove "_" for numeric literal separator, and trailing  `n`
      let s = self.input[start..self.state.pos].replace(&['_', 'n'][..], "");
      self.finish_token(&TokenTypes::bigint, Some(TokenValue::String(s)));
      return;
    }

    let token_value = Some(TokenValue::Numeric(val.unwrap() as usize));

    self.finish_token(&TokenTypes::num, token_value);
  }

  // Read an integer, octal integer, or floating-point number.

  fn read_number(&mut self, starts_with_dot: bool) {
    let start = self.state.pos;
    let mut is_float = false;
    let mut is_big_int = false;
    // let mut isDecimal = false;
    // let mut hasExponent = false;
    let mut is_octal = false;

    if !starts_with_dot && self.read_int(10, None, false, true).is_none() {
      // self.raise(start, Errors.InvalidNumber);
      panic!("InvalidNumber at {}", start);
    }
    let has_leading_zero =
      self.state.pos - start >= 2 && self.input.chars().nth(start) == Some('0');

    if has_leading_zero {
      let integer = &self.input[start..self.state.pos];
      // TODO:
      // self.recordStrictModeErrors(start, Errors.StrictOctalLiteral);
      if !self.state.strict {
        // disallow numeric separators in non octal decimals and legacy octal likes
        if let Some(underscore_pos) = integer.find('_') {
          if underscore_pos > 0 {
            // self.raise(underscorePos + start, Errors.ZeroDigitNumericSeparator);
            panic!("ZeroDigitNumericSeparator at {}", underscore_pos + start);
          }
        }
      }

      is_octal = has_leading_zero && !integer.contains(&['8', '9'][..]);
    }

    let mut next = self.input.chars().nth(self.state.pos);
    if next == Some('.') && !is_octal {
      self.state.pos += 1;
      self.read_int(10, None, false, true);
      is_float = true;
      next = self.input.chars().nth(self.state.pos);
    }

    if (next == Some('E') || next == Some('e')) && !is_octal {
      self.state.pos += 1;
      next = self.input.chars().nth(self.state.pos);
      if next == Some('+') || next == Some('-') {
        self.state.pos += 1;
      }
      if self.read_int(10, None, false, true).is_none() {
        // self.raise(start, Errors.InvalidOrMissingExponent);
        panic!("InvalidOrMissingExponent at {}", start);
      }
      is_float = true;
      // hasExponent = true;
      next = self.input.chars().nth(self.state.pos);
    }

    if next == Some('n') {
      // disallow floats, legacy octal syntax and non octal decimals
      // new style octal ("0o") is handled in self.readRadixNumber
      if is_float || has_leading_zero {
        // self.raise(start, Errors.InvalidBigIntLiteral);
        panic!("InvalidBigIntLiteral at {}", start);
      }
      self.state.pos += 1;
      is_big_int = true;
    }

    // if next == Some('m') {
    //   self.expectPlugin("decimal", self.state.pos);
    //   if (hasExponent || hasLeadingZero) {
    //     self.raise(start, Errors.InvalidDecimal);
    //   }
    //   self.state.pos += 1;
    //   isDecimal = true;
    // }

    next = self.input.chars().nth(self.state.pos);

    if next.is_some() && is_ident_start(next.unwrap()) {
      // throw self.raise(self.state.pos, Errors.NumberIdentifier);
      panic!("NumberIdentifier at {}", self.state.pos);
    }

    // remove "_" for numeric literal separator, and trailing `m` or `n`
    let s = self.input[start..self.state.pos].replace(&['_', 'm', 'n'][..], "");

    if is_big_int {
      self.finish_token(&TokenTypes::bigint, Some(TokenValue::String(s)));
      return;
    }

    // if (isDecimal) {
    //   self.finishToken(&TokenTypes::decimal, Some(TokenValue::String( s)));
    //   return;
    // }

    //TODO:
    // let val = isOctal ? parseInt(s, 8) : parseFloat(s);
    // self.finishToken(&TokenTypes::num, val);
    self.finish_token(&TokenTypes::num, Some(TokenValue::String(s)));
  }

  // Read a string value, interpreting backslash-escapes.

  fn read_code_point(&mut self, throw_on_invalid: bool) -> Option<char> {
    let ch = self.input.chars().nth(self.state.pos);

    if ch == Some('{') {
      self.state.pos += 1;
      let code_pos = self.state.pos;

      let closing_brace_index = match self.input[self.state.pos..].chars().position(|c| c == '}') {
        Some(i) => {
          if i > u8::MAX as usize {
            panic!();
          } else {
            i as u8
          }
        }
        None => {
          panic!("unterminated escape sequence");
        }
      };

      let code = self.read_hex_char(closing_brace_index, true, throw_on_invalid);
      self.state.pos += 1;
      if let Some(c) = code {
        if c as u32 > 0x10ffff {
          if throw_on_invalid {
            // self.raise(codePos, Errors.InvalidCodePoint);
            panic!("InvalidCodePoint at {}", code_pos);
          } else {
            return None;
          }
        }
      }

      code
    } else {
      self.read_hex_char(4, false, throw_on_invalid)
    }
  }

  fn read_string(&mut self, quote: char) {
    self.state.pos += 1;
    let mut out = String::new();
    let mut chunkStart = self.state.pos;
    loop {
      if self.state.pos >= self.length {
        // throw self.raise(self.state.start, Errors.UnterminatedString);
        panic!("UnterminatedString at {}", self.state.pos);
      }

      if let Some(ch) = self.input.chars().nth(self.state.pos) {
        if ch == quote {
          break;
        }
        if ch == '\\' {
          out.push_str(&self.input[chunkStart..self.state.pos]);
          // $FlowFixMe
          if let Some(c) = self.read_escaped_char(false) {
            out.push(c);
          }

          chunkStart = self.state.pos;
        } else if ch == '\u{2028}' || ch == '\u{2029}' {
          // lineSeparator
          // paragraphSeparator
          self.state.pos += 1;
          self.state.cur_line += 1;
          self.state.line_start = self.state.pos;
        } else if is_new_line(ch) {
          // throw self.raise(self.state.start, Errors.UnterminatedString);
          panic!("UnterminatedString at {}", self.state.pos);
        } else {
          self.state.pos += 1;
        }
      } else {
        self.state.pos += 1;
      }
    }
    out.push_str(&self.input[chunkStart..self.state.pos]);
    self.state.pos += 1;
    self.finish_token(&TokenTypes::string, Some(TokenValue::String(out)));
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

  // recordStrictModeErrors(pos: number, message: string) {
  //   if (self.state.strict && !self.state.strictErrors.has(pos)) {
  //     self.raise(pos, message);
  //   } else {
  //     self.state.strictErrors.set(pos, message);
  //   }
  // }

  // Used to read escaped characters
  fn read_escaped_char(&mut self, in_template: bool) -> Option<char> {
    self.state.pos += 1;
    let throw_on_invalid = !in_template;
    let ch = match self.input.chars().nth(self.state.pos) {
      Some(c) => c,
      None => return None,
    };
    self.state.pos += 1;

    match ch {
      // Line feed
      'n' => Some('\n'),
      // Carriage return
      'r' => Some('\r'),
      'x' => match self.read_hex_char(2, false, throw_on_invalid) {
        Some(code) => Some(code),
        None => None,
      },
      'u' => match self.read_code_point(throw_on_invalid) {
        Some(code) => Some(code),
        None => None,
      },
      // Tab
      't' => Some('\t'),
      // Backspace
      'b' => Some('\u{0008}'),
      // Vertical tab
      'v' => Some('\u{000b}'),
      // Form feed
      'f' => Some('\u{000c}'),
      // Carriage return
      // Line feed
      '\u{000d}' | '\u{000a}' => {
        if ch == '\u{000d}' && self.input.chars().nth(self.state.pos) == Some('\u{000a}') {
          self.state.pos += 1;
        }

        self.state.line_start = self.state.pos;
        self.state.cur_line += 1;

        None
      }
      // Line separator
      // Paragraph separator
      '\u{2028}' | '\u{2029}' => None,
      _ => {
        if ch == '8' || ch == '9' {
          if in_template {
            return None;
          } else {
            // self.recordStrictModeErrors(
            //   self.state.pos - 1,
            //   Errors.StrictNumericEscape,
            // );
            panic!()
          }
        }

        if ('0'..='7').contains(&ch) {
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
              self.state.pos += 1;
              v
            }
            _ => return None,
          };

          match self
            .input
            .chars()
            .nth(self.state.pos)
            .and_then(|c| c.to_digit(8))
          {
            Some(v) => {
              value = value * 8 + v;
              self.state.pos += 1;
            }
            _ => unsafe { return Some(std::char::from_u32_unchecked(value)) },
          }

          match self
            .input
            .chars()
            .nth(self.state.pos)
            .and_then(|c| c.to_digit(8))
          {
            Some(v) => {
              value = value * 8 + v;
              self.state.pos += 1;
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

        Some(ch)
      }
    }
  }

  // Used to read character escape sequences ('\x', '\u').

  fn read_hex_char(&mut self, len: u8, force_len: bool, throw_on_invalid: bool) -> Option<char> {
    let code_pos = self.state.pos;
    let n = self.read_int(16, Some(len), force_len, false);
    if n.is_none() {
      if throw_on_invalid {
        // self.raise(codePos, Errors.InvalidEscapeSequence);
        panic!("InvalidEscapeSequence at {}", self.state.pos);
      } else {
        self.state.pos = code_pos - 1;
      }
    }

    match n {
      Some(num) => match std::char::from_u32(num) {
        Some(c) => Some(c),
        None => None,
      },
      None => None,
    }
  }

  // Read an identifier, and return it as a string. Sets `self.state.containsEsc`
  // to whether the word contained a '\u' escape.
  //
  // Incrementally adds only escaped chars, adding other chunks as-is
  // as a micro-optimization.

  fn read_word1(&mut self) -> String {
    let mut word = String::new();
    self.state.contains_esc = false;
    let start = self.state.pos;
    let mut chunk_start = self.state.pos;

    while self.state.pos < self.length {
      if let Some(ch) = self.input.chars().nth(self.state.pos) {
        if is_ident_part(ch) {
          self.state.pos += if ch as u32 <= 0xffff { 1 } else { 2 };
        } else if self.state.is_iterator && ch == '@' {
          self.state.pos += 1;
        } else if ch == '\\' {
          self.state.contains_esc = true;

          word.push_str(&self.input[chunk_start..self.state.pos]);
          let esc_start = self.state.pos;

          self.state.pos += 1;
          if self.input.chars().nth(self.state.pos) != Some('u') {
            // self.raise(self.state.pos, Errors.MissingUnicodeEscape);
            panic!("MissingUnicodeEscape at {}", self.state.pos);
            // continue;
          }

          self.state.pos += 1;
          if let Some(esc) = self.read_code_point(true) {
            if self.state.pos == start {
              if !is_ident_start(esc) {
                // self.raise(escStart, Errors.EscapedCharNotAnIdentifier);
                panic!("EscapedCharNotAnIdentifier at {}", esc_start);
              }
            } else if !is_ident_part(esc) {
              // self.raise(escStart, Errors.EscapedCharNotAnIdentifier);
              panic!("EscapedCharNotAnIdentifier at {}", esc_start);
            }

            word.push(esc);
          }
          chunk_start = self.state.pos;
        } else {
          break;
        }
      } else {
        break;
      }
    }
    word.push_str(&self.input[chunk_start..self.state.pos]);
    word
  }

  // Read an identifier or keyword token. Will check for reserved
  // words when necessary.
  // See https://tc39.github.io/ecma262/#sec-names-and-keywords

  fn read_word(&mut self) {
    let word = self.read_word1();

    let keyword = match word.as_ref() {
      "break" => Some(&TokenTypes::KW_break),
      "case" => Some(&TokenTypes::KW_case),
      "catch" => Some(&TokenTypes::KW_catch),
      "continue" => Some(&TokenTypes::KW_continue),
      "debugger" => Some(&TokenTypes::KW_debugger),
      "default" => Some(&TokenTypes::KW_default),
      "do" => Some(&TokenTypes::KW_do),
      "else" => Some(&TokenTypes::KW_else),
      "finally" => Some(&TokenTypes::KW_finally),
      "for" => Some(&TokenTypes::KW_for),
      "function" => Some(&TokenTypes::KW_function),
      "if" => Some(&TokenTypes::KW_if),
      "return" => Some(&TokenTypes::KW_return),
      "switch" => Some(&TokenTypes::KW_switch),
      "throw" => Some(&TokenTypes::KW_throw),
      "try" => Some(&TokenTypes::KW_try),
      "var" => Some(&TokenTypes::KW_var),
      "const" => Some(&TokenTypes::KW_const),
      "while" => Some(&TokenTypes::KW_while),
      "with" => Some(&TokenTypes::KW_with),
      "new" => Some(&TokenTypes::KW_new),
      "this" => Some(&TokenTypes::KW_this),
      "super" => Some(&TokenTypes::KW_super),
      "class" => Some(&TokenTypes::KW_class),
      "extends" => Some(&TokenTypes::KW_extends),
      "export" => Some(&TokenTypes::KW_export),
      "import" => Some(&TokenTypes::KW_import),
      "null" => Some(&TokenTypes::KW_null),
      "true" => Some(&TokenTypes::KW_true),
      "false" => Some(&TokenTypes::KW_false),
      "in" => Some(&TokenTypes::KW_in),
      "instanceof" => Some(&TokenTypes::KW_instanceof),
      "typeof" => Some(&TokenTypes::KW_typeof),
      "void" => Some(&TokenTypes::KW_void),
      "delete" => Some(&TokenTypes::KW_delete),
      _ => None,
    };

    let kind = match keyword {
      Some(kind) => &kind,
      None => &TokenTypes::name,
    };

    // Allow @@iterator and @@asyncIterator as a identifier only inside type
    if self.state.is_iterator && (!is_iterator(&word) || !self.state.in_type) {
      // self.raise(self.state.pos, Errors.InvalidIdentifier, word);
      panic!("InvalidIdentifier '{}' at {}", word, self.state.pos);
    }

    self.finish_token(kind, Some(TokenValue::String(word)));
  }

  // checkKeywordEscapes(): void {
  //   const kw = self.state.type.keyword;
  //   if (kw && self.state.containsEsc) {
  //     self.raise(self.state.start, Errors.InvalidEscapedReservedWord, kw);
  //   }
  // }

  // braceIsBlock(prevType: TokenType): bool {
  //   const parent = self.curContext();
  //   if (parent === ct.functionExpression || parent === ct.functionStatement) {
  //     return true;
  //   }
  //   if (
  //     prevType === TokenTypes::colon &&
  //     (parent === ct.braceStatement || parent === ct.braceExpression)
  //   ) {
  //     return !parent.isExpr;
  //   }

  //   // The check for `TokenTypes::name && exprAllowed` detects whether we are
  //   // after a `yield` or `of` construct. See the `updateContext` for
  //   // `TokenTypes::name`.
  //   if (
  //     prevType === TokenTypes::_return ||
  //     (prevType === TokenTypes::name && self.state.exprAllowed)
  //   ) {
  //     return self.hasPrecedingLineBreak();
  //   }

  //   if (
  //     prevType === TokenTypes::_else ||
  //     prevType === TokenTypes::semi ||
  //     prevType === TokenTypes::eof ||
  //     prevType === TokenTypes::parenR ||
  //     prevType === TokenTypes::arrow
  //   ) {
  //     return true;
  //   }

  //   if (prevType === TokenTypes::braceL) {
  //     return parent === ct.braceStatement;
  //   }

  //   if (
  //     prevType === TokenTypes::_var ||
  //     prevType === TokenTypes::_const ||
  //     prevType === TokenTypes::name
  //   ) {
  //     return false;
  //   }

  //   if (prevType === TokenTypes::relational) {
  //     // `class C<T> { ... }`
  //     return true;
  //   }

  //   return !self.state.exprAllowed;
  // }

  fn update_context(&mut self, prev_type: &'a TokenType) {
    let kind = &self.state.kind;
    // let update;

    if kind.keyword.is_some()
      && (prev_type == &TokenTypes::dot || prev_type == &TokenTypes::questionDot)
    {
      self.state.expr_allowed = false;
    }
    /*else if ((update = kind.updateContext)) {
      update.call(self, prevType);
    }*/
    else {
      self.state.expr_allowed = kind.before_expr;
    }
  }
}
