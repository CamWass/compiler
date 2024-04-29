pub(crate) use self::{AssignOpToken::*, BinOpToken::*, Keyword::*, Token::*};
use crate::error::Error;
pub(crate) use ast::AssignOp as AssignOpToken;
use ast::BinaryOp;
use atoms::{js_word, JsWord};
use global_common::{Span, Spanned};
use num_bigint::BigInt as BigIntValue;
use std::{
    borrow::Cow,
    fmt::{self, Debug, Display, Formatter},
};

#[derive(Clone, PartialEq)]
pub enum Token {
    /// Identifier, "null", "true", "false".
    ///
    /// Contains `null` and ``
    Word(Word),

    /// '=>'
    Arrow,

    /// '#'
    Hash,

    /// '@'
    At,
    /// '.'
    Dot,

    /// '...'
    DotDotDot,
    /// '!'
    Bang,

    /// '('
    LParen,
    /// ')'
    RParen,
    /// `[`
    LBracket,
    /// ']'
    RBracket,
    /// '{'
    LBrace,
    /// '}'
    RBrace,

    /// ';'
    Semi,
    /// ','
    Comma,

    /// '`'
    BackQuote,
    Template {
        raw: JsWord,
        cooked: Option<JsWord>,
        has_escape: bool,
    },
    /// ':'
    Colon,
    /// '::'
    ColonColon,
    ///
    BinOp(BinOpToken),
    ///
    AssignOp(AssignOpToken),

    /// '${'
    DollarLBrace,

    /// '?'
    QuestionMark,

    /// `++`
    PlusPlus,
    /// `--`
    MinusMinus,

    /// `~`
    Tilde,

    /// String literal. Span of this token contains quote.
    Str {
        value: JsWord,
        /// This field exists because 'use\x20strict' is **not** an use strict
        /// directive.
        has_escape: bool,
    },

    /// Regexp literal.
    Regex(JsWord, JsWord),

    /// TODO: Make Num as enum and separate decimal, binary, ..etc
    Num {
        value: f64,
        raw: JsWord,
    },

    BigInt(BigIntValue),

    JSXName {
        name: JsWord,
    },
    JSXText {
        raw: JsWord,
    },
    JSXTagStart,
    JSXTagEnd,

    Shebang(JsWord),
    Error(Error),
}

impl Token {
    pub fn starts_expr(&self) -> bool {
        match self {
            Token::Word(w) => w.starts_expr(),
            BinOp(o) => o.starts_expr(),

            Bang
            | LParen
            | LBracket
            | LBrace
            | BackQuote
            | DollarLBrace
            | PlusPlus
            | MinusMinus
            | Tilde
            | Str { .. }
            | Regex(_, _)
            | Num { .. }
            | BigInt(_)
            | JSXTagStart => true,

            _ => false,
        }
    }

    pub fn before_expr(&self) -> bool {
        match self {
            Token::Word(w) => w.before_expr(),
            BinOp(o) => o.before_expr(),

            Arrow
            | DotDotDot
            | Bang
            | LParen
            | LBracket
            | LBrace
            | Semi
            | Comma
            | Colon
            | ColonColon
            | AssignOp(_)
            | DollarLBrace
            | QuestionMark
            | PlusPlus
            | MinusMinus
            | Tilde
            | JSXText { .. } => true,

            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum BinOpToken {
    /// `==`
    EqEq,
    /// `!=`
    NotEq,
    /// `===`
    EqEqEq,
    /// `!==`
    NotEqEq,
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `>`
    Gt,
    /// `>=`
    GtEq,
    /// `<<`
    LShift,
    /// `>>`
    RShift,
    /// `>>>`
    ZeroFillRShift,

    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,

    /// `|`
    BitOr,
    /// `^`
    BitXor,
    /// `&`
    BitAnd,

    // /// `in`
    // #[kind(precedence = "7")]
    // In,
    // /// `instanceof`
    // #[kind(precedence = "7")]
    // InstanceOf,
    /// `**`
    Exp,

    /// `||`
    LogicalOr,
    /// `&&`
    LogicalAnd,

    /// `??`
    NullishCoalescing,
}

impl BinOpToken {
    pub const fn before_expr(self) -> bool {
        true
    }

    fn starts_expr(&self) -> bool {
        match self {
            Self::Add | Self::Sub => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenAndSpan {
    pub token: Token,
    /// Had a line break before this token?
    pub had_line_break: bool,
    pub span: Span,
}

impl Spanned for TokenAndSpan {
    #[inline(always)]
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Word {
    Keyword(Keyword),

    Null,
    True,
    False,

    Ident(JsWord),
}

impl Word {
    fn starts_expr(&self) -> bool {
        match self {
            Word::Keyword(k) => k.starts_expr(),
            _ => true,
        }
    }

    fn before_expr(&self) -> bool {
        match self {
            Word::Keyword(k) => k.before_expr(),
            _ => false,
        }
    }
}

impl From<JsWord> for Word {
    fn from(i: JsWord) -> Self {
        match i {
            js_word!("null") => Word::Null,
            js_word!("true") => Word::True,
            js_word!("false") => Word::False,
            js_word!("await") => Await.into(),
            js_word!("break") => Break.into(),
            js_word!("case") => Case.into(),
            js_word!("catch") => Catch.into(),
            js_word!("continue") => Continue.into(),
            js_word!("debugger") => Debugger.into(),
            js_word!("default") => Default_.into(),
            js_word!("do") => Do.into(),
            js_word!("export") => Export.into(),
            js_word!("else") => Else.into(),
            js_word!("finally") => Finally.into(),
            js_word!("for") => For.into(),
            js_word!("function") => Function.into(),
            js_word!("if") => If.into(),
            js_word!("return") => Return.into(),
            js_word!("switch") => Switch.into(),
            js_word!("throw") => Throw.into(),
            js_word!("try") => Try.into(),
            js_word!("var") => Var.into(),
            js_word!("let") => Let.into(),
            js_word!("const") => Const.into(),
            js_word!("while") => While.into(),
            js_word!("with") => With.into(),
            js_word!("new") => New.into(),
            js_word!("this") => This.into(),
            js_word!("super") => Super.into(),
            js_word!("class") => Class.into(),
            js_word!("extends") => Extends.into(),
            js_word!("import") => Import.into(),
            js_word!("yield") => Yield.into(),
            js_word!("in") => In.into(),
            js_word!("instanceof") => InstanceOf.into(),
            js_word!("typeof") => TypeOf.into(),
            js_word!("void") => Void.into(),
            js_word!("delete") => Delete.into(),
            _ => Word::Ident(i),
        }
    }
}

impl From<&str> for Word {
    fn from(s: &str) -> Self {
        match s {
            "null" => Word::Null,
            "true" => Word::True,
            "false" => Word::False,
            "await" => Await.into(),
            "break" => Break.into(),
            "case" => Case.into(),
            "catch" => Catch.into(),
            "continue" => Continue.into(),
            "debugger" => Debugger.into(),
            "default" => Default_.into(),
            "do" => Do.into(),
            "export" => Export.into(),
            "else" => Else.into(),
            "finally" => Finally.into(),
            "for" => For.into(),
            "function" => Function.into(),
            "if" => If.into(),
            "return" => Return.into(),
            "switch" => Switch.into(),
            "throw" => Throw.into(),
            "try" => Try.into(),
            "var" => Var.into(),
            "let" => Let.into(),
            "const" => Const.into(),
            "while" => While.into(),
            "with" => With.into(),
            "new" => New.into(),
            "this" => This.into(),
            "super" => Super.into(),
            "class" => Class.into(),
            "extends" => Extends.into(),
            "import" => Import.into(),
            "yield" => Yield.into(),
            "in" => In.into(),
            "instanceof" => InstanceOf.into(),
            "typeof" => TypeOf.into(),
            "void" => Void.into(),
            "delete" => Delete.into(),
            _ => Word::Ident(s.into()),
        }
    }
}

impl From<Keyword> for Word {
    fn from(kwd: Keyword) -> Self {
        Word::Keyword(kwd)
    }
}

impl From<Word> for JsWord {
    fn from(w: Word) -> Self {
        match w {
            Word::Keyword(k) => match k {
                Await => js_word!("await"),
                Break => js_word!("break"),
                Case => js_word!("case"),
                Catch => js_word!("catch"),
                Continue => js_word!("continue"),
                Debugger => js_word!("debugger"),
                Default_ => js_word!("default"),
                Do => js_word!("do"),
                Else => js_word!("else"),

                Finally => js_word!("finally"),
                For => js_word!("for"),

                Function => js_word!("function"),

                If => js_word!("if"),

                Return => js_word!("return"),

                Switch => js_word!("switch"),

                Throw => js_word!("throw"),

                Try => js_word!("try"),
                Var => js_word!("var"),
                Let => js_word!("let"),
                Const => js_word!("const"),
                While => js_word!("while"),
                With => js_word!("with"),

                New => js_word!("new"),
                This => js_word!("this"),
                Super => js_word!("super"),

                Class => js_word!("class"),

                Extends => js_word!("extends"),

                Export => js_word!("export"),
                Import => js_word!("import"),

                Yield => js_word!("yield"),

                In => js_word!("in"),
                InstanceOf => js_word!("instanceof"),

                TypeOf => js_word!("typeof"),

                Void => js_word!("void"),

                Delete => js_word!("delete"),
            },

            Word::Null => js_word!("null"),
            Word::True => js_word!("true"),
            Word::False => js_word!("false"),

            Word::Ident(w) => w,
        }
    }
}

impl Debug for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Word::Ident(ref s) => Display::fmt(s, f),
            _ => {
                let s: JsWord = self.clone().into();
                Display::fmt(&s, f)
            }
        }
    }
}

/// Keywords
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Keyword {
    /// Spec says this might be identifier.
    Await,

    Break,
    Case,
    Catch,
    Continue,
    Debugger,
    Default_,
    Do,
    Else,

    Finally,
    For,

    Function,

    If,

    Return,

    Switch,

    Throw,

    Try,
    Var,
    Let,
    Const,
    While,
    With,

    New,
    This,
    Super,

    Class,

    Extends,

    Export,
    Import,

    /// Spec says this might be identifier.
    Yield,

    In,
    InstanceOf,

    TypeOf,

    Void,

    Delete,
}

impl Keyword {
    fn into_js_word(self) -> JsWord {
        match self {
            Await => js_word!("await"),
            Break => js_word!("break"),
            Case => js_word!("case"),
            Catch => js_word!("catch"),
            Continue => js_word!("continue"),
            Debugger => js_word!("debugger"),
            Default_ => js_word!("default"),
            Do => js_word!("do"),
            Else => js_word!("else"),

            Finally => js_word!("finally"),
            For => js_word!("for"),

            Function => js_word!("function"),

            If => js_word!("if"),

            Return => js_word!("return"),

            Switch => js_word!("switch"),

            Throw => js_word!("throw"),

            Try => js_word!("try"),
            Var => js_word!("var"),
            Let => js_word!("let"),
            Const => js_word!("const"),
            While => js_word!("while"),
            With => js_word!("with"),

            New => js_word!("new"),
            This => js_word!("this"),
            Super => js_word!("super"),

            Class => js_word!("class"),

            Extends => js_word!("extends"),

            Export => js_word!("export"),
            Import => js_word!("import"),

            Yield => js_word!("yield"),

            In => js_word!("in"),
            InstanceOf => js_word!("instanceof"),

            TypeOf => js_word!("typeof"),

            Void => js_word!("void"),

            Delete => js_word!("delete"),
        }
    }

    pub fn before_expr(&self) -> bool {
        match self {
            Await | Case | Default_ | Do | Else | Return | Throw | New | Extends | Yield | In
            | InstanceOf | TypeOf | Void | Delete => true,
            _ => false,
        }
    }

    fn starts_expr(&self) -> bool {
        match self {
            Await | Function | Throw | New | This | Super | Class | Import | Yield | TypeOf
            | Void | Delete => true,
            _ => false,
        }
    }
}

impl Debug for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "keyword '{}'", self.into_js_word())?;

        Ok(())
    }
}

impl From<BinOpToken> for BinaryOp {
    fn from(t: BinOpToken) -> Self {
        use self::BinaryOp::*;
        match t {
            BinOpToken::EqEq => EqEq,
            BinOpToken::NotEq => NotEq,
            BinOpToken::EqEqEq => EqEqEq,
            BinOpToken::NotEqEq => NotEqEq,
            BinOpToken::Lt => Lt,
            BinOpToken::LtEq => LtEq,
            BinOpToken::Gt => Gt,
            BinOpToken::GtEq => GtEq,
            BinOpToken::LShift => LShift,
            BinOpToken::RShift => RShift,
            BinOpToken::ZeroFillRShift => ZeroFillRShift,
            BinOpToken::Add => Add,
            BinOpToken::Sub => Sub,
            BinOpToken::Mul => Mul,
            BinOpToken::Div => Div,
            BinOpToken::Mod => Mod,
            BinOpToken::BitOr => BitOr,
            BinOpToken::BitXor => BitXor,
            BinOpToken::BitAnd => BitAnd,
            BinOpToken::LogicalOr => LogicalOr,
            BinOpToken::LogicalAnd => LogicalAnd,
            BinOpToken::Exp => Exp,
            BinOpToken::NullishCoalescing => NullishCoalescing,
        }
    }
}

impl Token {
    /// Returns true if `self` can follow keyword let.
    ///
    /// e.g. `let a = xx;`, `let {a:{}} = 1`
    pub(crate) fn follows_keyword_let(&self) -> bool {
        match *self {
            // This is required to recognize `let let` in strict mode.
            tok!("let") => true,

            tok!('{') | tok!('[') | Word(Word::Ident(..)) | tok!("yield") | tok!("await") => true,

            _ => false,
        }
    }
}

impl Word {
    pub(crate) fn cow(&self) -> Cow<JsWord> {
        match *self {
            Word::Keyword(k) => Cow::Owned(k.into_js_word()),
            Word::Ident(ref w) => Cow::Borrowed(w),
            Word::False => Cow::Owned(js_word!("false")),
            Word::True => Cow::Owned(js_word!("true")),
            Word::Null => Cow::Owned(js_word!("null")),
        }
    }
}

impl Debug for Token {
    /// This method is called only in the case of parsing failure.
    #[cold]
    #[inline(never)]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Word(w) => write!(f, "{:?}", w)?,
            Arrow => write!(f, "=>")?,
            Hash => write!(f, "#")?,
            At => write!(f, "@")?,
            Dot => write!(f, ".")?,
            DotDotDot => write!(f, "...")?,
            Bang => write!(f, "!")?,
            LParen => write!(f, "(")?,
            RParen => write!(f, ")")?,
            LBracket => write!(f, "[")?,
            RBracket => write!(f, "]")?,
            LBrace => write!(f, "{{")?,
            RBrace => write!(f, "}}")?,
            Semi => write!(f, ";")?,
            Comma => write!(f, ",")?,
            BackQuote => write!(f, "`")?,
            Template { raw, .. } => write!(f, "template token ({})", raw)?,
            Colon => write!(f, ":")?,
            ColonColon => write!(f, "::")?,
            BinOp(op) => write!(f, "{}", BinaryOp::from(*op).as_str())?,
            AssignOp(op) => write!(f, "{}", op.as_str())?,
            DollarLBrace => write!(f, "${{")?,
            QuestionMark => write!(f, "?")?,
            PlusPlus => write!(f, "++")?,
            MinusMinus => write!(f, "--")?,
            Tilde => write!(f, "~")?,
            Str { value, .. } => write!(f, "string literal ({})", value)?,
            Regex(exp, flags) => write!(f, "regexp literal ({}, {})", exp, flags)?,
            Num { value, raw } => write!(f, "numeric literal ({}, {})", value, raw)?,
            BigInt(..) => write!(f, "bigint literal")?,
            JSXName { name } => write!(f, "jsx name ({})", name)?,
            JSXText { raw } => write!(f, "jsx text ({})", raw)?,
            JSXTagStart => write!(f, "< (jsx tag start)")?,
            JSXTagEnd => write!(f, "> (jsx tag end)")?,
            Shebang(_) => write!(f, "#!")?,
            Token::Error(_) => write!(f, "<lexing error>")?,
        }

        Ok(())
    }
}
