use crate::{
    JscTarget, Syntax,
    context::Context,
    error::Error,
    lexer::{Lexer, LexerCheckpoint, TokenContexts},
    parser::Parser,
    token::{Token, TokenAndSpan, TokenData},
};
use ast::{NameId, ProgramData, id_for_built_in};
use atoms::JsWord;
use common::{BytePos, Span};
use num_bigint::BigUint;

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
    pub fn dump_cur(&self) -> String {
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

    pub fn peek(&mut self) -> Option<Token> {
        debug_assert!(
            self.cur.token != Token::Eof,
            "parser should not call peek() without knowing current token"
        );

        if self.next.is_none() {
            self.next = Some(self.iter.next_token());
        }

        self.next.as_ref().map(|ts| ts.token)
    }

    /// Returns true on eof.
    pub fn had_line_break_before_cur(&self) -> bool {
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
    pub fn cur(&self) -> Token {
        self.cur.token
    }

    #[inline]
    pub fn is(&self, expected: Token) -> bool {
        self.cur.token == expected
    }

    #[inline]
    pub fn peeked_is(&mut self, expected: Token) -> bool {
        match self.peek() {
            Some(t) => expected == t,
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
    pub fn cur_pos(&self) -> BytePos {
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

    pub fn take_word(&mut self) -> NameId {
        if self.cur() == Token::Ident {
            if let Some(TokenData::Ident(word)) = self.iter.take_token_data() {
                word
            } else {
                unreachable!();
            }
        } else {
            match self.cur() {
                Token::Await => id_for_built_in!("await"),
                Token::Break => id_for_built_in!("break"),
                Token::Case => id_for_built_in!("case"),
                Token::Catch => id_for_built_in!("catch"),
                Token::Continue => id_for_built_in!("continue"),
                Token::Debugger => id_for_built_in!("debugger"),
                Token::Default => id_for_built_in!("default"),
                Token::Do => id_for_built_in!("do"),
                Token::Else => id_for_built_in!("else"),
                Token::Finally => id_for_built_in!("finally"),
                Token::For => id_for_built_in!("for"),
                Token::Function => id_for_built_in!("function"),
                Token::If => id_for_built_in!("if"),
                Token::Return => id_for_built_in!("return"),
                Token::Switch => id_for_built_in!("switch"),
                Token::Throw => id_for_built_in!("throw"),
                Token::Try => id_for_built_in!("try"),
                Token::Var => id_for_built_in!("var"),
                Token::Let => id_for_built_in!("let"),
                Token::Const => id_for_built_in!("const"),
                Token::While => id_for_built_in!("while"),
                Token::With => id_for_built_in!("with"),
                Token::New => id_for_built_in!("new"),
                Token::This => id_for_built_in!("this"),
                Token::Super => id_for_built_in!("super"),
                Token::Class => id_for_built_in!("class"),
                Token::Extends => id_for_built_in!("extends"),
                Token::Export => id_for_built_in!("export"),
                Token::Import => id_for_built_in!("import"),
                Token::Yield => id_for_built_in!("yield"),
                Token::In => id_for_built_in!("in"),
                Token::InstanceOf => id_for_built_in!("instanceof"),
                Token::TypeOf => id_for_built_in!("typeof"),
                Token::Void => id_for_built_in!("void"),
                Token::Delete => id_for_built_in!("delete"),
                Token::Null => id_for_built_in!("null"),
                Token::True => id_for_built_in!("true"),
                Token::False => id_for_built_in!("false"),
                Token::Async => id_for_built_in!("async"),
                Token::As => id_for_built_in!("as"),
                Token::From => id_for_built_in!("from"),
                Token::Of => id_for_built_in!("of"),
                Token::Static => id_for_built_in!("static"),
                Token::Target => id_for_built_in!("target"),
                Token::Asserts => id_for_built_in!("asserts"),
                Token::Implements => id_for_built_in!("implements"),
                Token::Is => id_for_built_in!("is"),
                Token::Keyof => id_for_built_in!("keyof"),
                Token::Unique => id_for_built_in!("unique"),
                Token::Object => id_for_built_in!("object"),
                Token::Global => id_for_built_in!("global"),
                Token::Enum => id_for_built_in!("enum"),
                Token::Readonly => id_for_built_in!("readonly"),
                Token::Abstract => id_for_built_in!("abstract"),
                Token::Infer => id_for_built_in!("infer"),
                Token::Any => id_for_built_in!("any"),
                Token::Boolean => id_for_built_in!("boolean"),
                Token::Bigint => id_for_built_in!("bigint"),
                Token::Intrinsic => id_for_built_in!("intrinsic"),
                Token::Never => id_for_built_in!("never"),
                Token::Number => id_for_built_in!("number"),
                Token::String => id_for_built_in!("string"),
                Token::Symbol => id_for_built_in!("symbol"),
                Token::Unknown => id_for_built_in!("unknown"),
                Token::Interface => id_for_built_in!("interface"),
                Token::Declare => id_for_built_in!("declare"),
                Token::Undefined => id_for_built_in!("undefined"),
                Token::Meta => id_for_built_in!("meta"),
                Token::Type => id_for_built_in!("type"),
                Token::Assert => id_for_built_in!("assert"),
                Token::Get => id_for_built_in!("get"),
                Token::Set => id_for_built_in!("set"),
                Token::Public => id_for_built_in!("public"),
                Token::Protected => id_for_built_in!("protected"),
                Token::Private => id_for_built_in!("private"),
                Token::Package => id_for_built_in!("package"),
                Token::Override => id_for_built_in!("override"),

                _ => unreachable!(),
            }
        }
    }

    pub fn expect_word_token_and_bump(&mut self) -> NameId {
        let word = if self.cur() == Token::Ident {
            if let Some(TokenData::Ident(word)) = self.iter.take_token_data() {
                word
            } else {
                unreachable!();
            }
        } else {
            match self.cur() {
                Token::Await => id_for_built_in!("await"),
                Token::Break => id_for_built_in!("break"),
                Token::Case => id_for_built_in!("case"),
                Token::Catch => id_for_built_in!("catch"),
                Token::Continue => id_for_built_in!("continue"),
                Token::Debugger => id_for_built_in!("debugger"),
                Token::Default => id_for_built_in!("default"),
                Token::Do => id_for_built_in!("do"),
                Token::Else => id_for_built_in!("else"),
                Token::Finally => id_for_built_in!("finally"),
                Token::For => id_for_built_in!("for"),
                Token::Function => id_for_built_in!("function"),
                Token::If => id_for_built_in!("if"),
                Token::Return => id_for_built_in!("return"),
                Token::Switch => id_for_built_in!("switch"),
                Token::Throw => id_for_built_in!("throw"),
                Token::Try => id_for_built_in!("try"),
                Token::Var => id_for_built_in!("var"),
                Token::Let => id_for_built_in!("let"),
                Token::Const => id_for_built_in!("const"),
                Token::While => id_for_built_in!("while"),
                Token::With => id_for_built_in!("with"),
                Token::New => id_for_built_in!("new"),
                Token::This => id_for_built_in!("this"),
                Token::Super => id_for_built_in!("super"),
                Token::Class => id_for_built_in!("class"),
                Token::Extends => id_for_built_in!("extends"),
                Token::Export => id_for_built_in!("export"),
                Token::Import => id_for_built_in!("import"),
                Token::Yield => id_for_built_in!("yield"),
                Token::In => id_for_built_in!("in"),
                Token::InstanceOf => id_for_built_in!("instanceof"),
                Token::TypeOf => id_for_built_in!("typeof"),
                Token::Void => id_for_built_in!("void"),
                Token::Delete => id_for_built_in!("delete"),
                Token::Null => id_for_built_in!("null"),
                Token::True => id_for_built_in!("true"),
                Token::False => id_for_built_in!("false"),
                Token::Async => id_for_built_in!("async"),
                Token::As => id_for_built_in!("as"),
                Token::From => id_for_built_in!("from"),
                Token::Of => id_for_built_in!("of"),
                Token::Static => id_for_built_in!("static"),
                Token::Target => id_for_built_in!("target"),
                Token::Asserts => id_for_built_in!("asserts"),
                Token::Implements => id_for_built_in!("implements"),
                Token::Is => id_for_built_in!("is"),
                Token::Keyof => id_for_built_in!("keyof"),
                Token::Unique => id_for_built_in!("unique"),
                Token::Object => id_for_built_in!("object"),
                Token::Global => id_for_built_in!("global"),
                Token::Enum => id_for_built_in!("enum"),
                Token::Readonly => id_for_built_in!("readonly"),
                Token::Abstract => id_for_built_in!("abstract"),
                Token::Infer => id_for_built_in!("infer"),
                Token::Any => id_for_built_in!("any"),
                Token::Boolean => id_for_built_in!("boolean"),
                Token::Bigint => id_for_built_in!("bigint"),
                Token::Intrinsic => id_for_built_in!("intrinsic"),
                Token::Never => id_for_built_in!("never"),
                Token::Number => id_for_built_in!("number"),
                Token::String => id_for_built_in!("string"),
                Token::Symbol => id_for_built_in!("symbol"),
                Token::Unknown => id_for_built_in!("unknown"),
                Token::Interface => id_for_built_in!("interface"),
                Token::Declare => id_for_built_in!("declare"),
                Token::Undefined => id_for_built_in!("undefined"),
                Token::Meta => id_for_built_in!("meta"),
                Token::Type => id_for_built_in!("type"),
                Token::Assert => id_for_built_in!("assert"),
                Token::Get => id_for_built_in!("get"),
                Token::Set => id_for_built_in!("set"),
                Token::Public => id_for_built_in!("public"),
                Token::Protected => id_for_built_in!("protected"),
                Token::Private => id_for_built_in!("private"),
                Token::Package => id_for_built_in!("package"),
                Token::Override => id_for_built_in!("override"),

                _ => unreachable!(),
            }
        };
        self.bump();
        word
    }

    pub fn expect_error_token_and_bump(&mut self) -> Error {
        let ret = if let Some(TokenData::Error(error)) = self.iter.take_token_data() {
            error
        } else {
            unreachable!();
        };
        self.bump();
        ret
    }

    pub fn expect_str_token_and_bump(&mut self) -> JsWord {
        let ret = if let Some(TokenData::Str { value }) = self.iter.take_token_data() {
            value
        } else {
            unreachable!();
        };
        self.bump();
        ret
    }

    pub fn expect_num_token_and_bump(&mut self) -> f64 {
        let ret = if let Some(TokenData::Num(value)) = self.iter.take_token_data() {
            value
        } else {
            unreachable!();
        };
        self.bump();
        ret
    }

    pub fn expect_big_int_token_and_bump(&mut self) -> Box<BigUint> {
        let ret = if let Some(TokenData::BigInt(value)) = self.iter.take_token_data() {
            value
        } else {
            unreachable!();
        };
        self.bump();
        ret
    }

    pub fn expect_template_token_and_bump(&mut self) -> (JsWord, bool) {
        let ret = if let Some(TokenData::Template {
            raw,
            has_invalid_escape,
        }) = self.iter.take_token_data()
        {
            (raw, has_invalid_escape)
        } else {
            unreachable!();
        };
        self.bump();
        ret
    }

    pub fn cur_string(&self) -> &str {
        self.iter.slice_to_cur(self.cur_span().lo)
    }
}
