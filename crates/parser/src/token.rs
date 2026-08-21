pub(crate) use self::{AssignOpToken::*, BinOpToken::*};
use crate::{
    context::{Context, ContextFlags},
    error::Error,
};
pub(crate) use ast::AssignOp as AssignOpToken;
use ast::{BinaryOp, NameId, RegexFlags};
use atoms::JsWord;
use common::Span;
use num_bigint::BigUint;
use std::fmt::{Debug, Display};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Token {
    /// Spec says this might be identifier.
    Await,

    Break,
    Case,
    Catch,
    Continue,
    Debugger,
    Default,
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

    Null,
    True,
    False,

    Ident,

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
    Template,
    /// ':'
    Colon,
    /// '::'
    ColonColon,
    // TODO: flatten to make Token 1 byte
    BinOp(BinOpToken),
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
    Str,

    /// Regexp literal.
    Regex,

    Num,

    BigInt,

    Error,

    //
    Async,
    As,
    From,
    Of,
    Static,
    Target,
    Asserts,
    Implements,
    Is,
    Keyof,
    Unique,
    Object,
    Global,
    Enum,
    Readonly,
    Abstract,
    Infer,
    Any,
    Boolean,
    Bigint,
    Intrinsic,
    Never,
    Number,
    String,
    Symbol,
    Unknown,
    Interface,
    Declare,
    Undefined,
    Meta,
    Type,
    Assert,
    Get,
    Set,
    Public,
    Protected,
    Private,
    Package,
    Override,

    Eof,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Token::Str => "<string literal>",
            Token::Num => "<number literal>",
            Token::BigInt => "<bigint literal>",
            Token::Regex => "<regexp literal>",
            Token::Template => "<template literal>",
            Token::Ident => "<identifier>",
            Token::Error => "<error>",
            _ => &self.to_string(),
        };
        f.write_str(s)
    }
}

impl Display for Token {
    #[cold]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::LBracket => "[",
            Token::RBracket => "]",
            Token::Semi => ";",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::Colon => ":",
            Token::QuestionMark => "?",
            Token::Bang => "!",
            Token::Tilde => "~",
            Token::At => "@",
            Token::Hash => "#",
            Token::BackQuote => "`",
            Token::Arrow => "=>",
            Token::DotDotDot => "...",
            Token::PlusPlus => "++",
            Token::MinusMinus => "--",
            Token::BinOp(b) => BinaryOp::from(*b).as_str(),
            Token::DollarLBrace => "${",
            Token::Str => "string literal",
            Token::Num => "numeric literal",
            Token::BigInt => "bigint literal",
            Token::Regex => "regexp literal",
            Token::Template => "template token",
            Token::Error => "<lexing error>",
            Token::Ident => "ident",
            Token::Await => "await",
            Token::Break => "break",
            Token::Case => "case",
            Token::Catch => "catch",
            Token::Class => "class",
            Token::Const => "const",
            Token::Continue => "continue",
            Token::Debugger => "debugger",
            Token::Default => "default",
            Token::Delete => "delete",
            Token::Do => "do",
            Token::Else => "else",
            Token::Export => "export",
            Token::Extends => "extends",
            Token::False => "false",
            Token::Finally => "finally",
            Token::For => "for",
            Token::Function => "function",
            Token::If => "if",
            Token::Import => "import",
            Token::In => "in",
            Token::InstanceOf => "instanceOf",
            Token::Let => "let",
            Token::New => "new",
            Token::Null => "null",
            Token::Return => "return",
            Token::Super => "super",
            Token::Switch => "switch",
            Token::This => "this",
            Token::Throw => "throw",
            Token::True => "true",
            Token::Try => "try",
            Token::TypeOf => "typeOf",
            Token::Var => "var",
            Token::Void => "void",
            Token::While => "while",
            Token::With => "with",
            Token::Yield => "yield",
            Token::Abstract => "abstract",
            Token::Any => "any",
            Token::As => "as",
            Token::Asserts => "asserts",
            Token::Assert => "assert",
            Token::Async => "async",
            Token::Bigint => "bigint",
            Token::Boolean => "boolean",
            Token::Declare => "declare",
            Token::Enum => "enum",
            Token::From => "from",
            Token::Get => "get",
            Token::Global => "global",
            Token::Implements => "implements",
            Token::Interface => "interface",
            Token::Intrinsic => "intrinsic",
            Token::Is => "is",
            Token::Keyof => "keyof",
            Token::Never => "never",
            Token::Number => "number",
            Token::Object => "object",
            Token::Of => "of",
            Token::Override => "override",
            Token::Package => "package",
            Token::Private => "private",
            Token::Protected => "protected",
            Token::Public => "public",
            Token::Readonly => "readonly",
            Token::Set => "set",
            Token::Static => "static",
            Token::String => "string",
            Token::Symbol => "symbol",
            Token::Type => "type",
            Token::Undefined => "undefined",
            Token::Unique => "unique",
            Token::Unknown => "unknown",
            Token::Infer => "infer",
            Token::Meta => "meta",
            Token::Target => "target",
            Token::Eof => "<eof>",
            Token::ColonColon => "::",
            Token::AssignOp(assign_op) => assign_op.as_str(),
        };
        f.write_str(s)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum TokenData {
    Ident(NameId),

    Template {
        raw: JsWord,
        has_invalid_escape: bool,
    },

    /// String literal. Span of this token contains quote.
    Str {
        value: JsWord,
    },

    Regex(JsWord, RegexFlags),

    Num(f64),

    BigInt(BigUint),

    Error(Error),
}

impl Token {
    pub fn starts_expr(&self) -> bool {
        match self {
            Token::Async
            | Token::As
            | Token::From
            | Token::Of
            | Token::Static
            | Token::Target
            | Token::Asserts
            | Token::Implements
            | Token::Is
            | Token::Keyof
            | Token::Unique
            | Token::Object
            | Token::Global
            | Token::Enum
            | Token::Readonly
            | Token::Abstract
            | Token::Infer
            | Token::Any
            | Token::Boolean
            | Token::Bigint
            | Token::Intrinsic
            | Token::Never
            | Token::Number
            | Token::String
            | Token::Symbol
            | Token::Unknown
            | Token::Interface
            | Token::Declare
            | Token::Undefined
            | Token::Meta
            | Token::Type
            | Token::Assert
            | Token::Get
            | Token::Set
            | Token::Public
            | Token::Protected
            | Token::Private
            | Token::Package
            | Token::Override => true,

            Token::Null | Token::True | Token::False | Token::Ident => true,

            Token::Await
            | Token::Function
            | Token::Throw
            | Token::New
            | Token::This
            | Token::Super
            | Token::Class
            | Token::Import
            | Token::Yield
            | Token::TypeOf
            | Token::Void
            | Token::Delete => true,

            Token::Break
            | Token::Case
            | Token::Catch
            | Token::Continue
            | Token::Debugger
            | Token::Default
            | Token::Do
            | Token::Else
            | Token::Finally
            | Token::For
            | Token::If
            | Token::Return
            | Token::Switch
            | Token::Try
            | Token::Var
            | Token::Let
            | Token::Const
            | Token::While
            | Token::With
            | Token::Extends
            | Token::Export
            | Token::In
            | Token::InstanceOf => false,

            Token::BinOp(o) => o.starts_expr(),

            Token::Bang
            | Token::LParen
            | Token::LBracket
            | Token::LBrace
            | Token::BackQuote
            | Token::DollarLBrace
            | Token::PlusPlus
            | Token::MinusMinus
            | Token::Tilde
            | Token::Str
            | Token::Regex
            | Token::Num
            | Token::BigInt => true,

            _ => false,
        }
    }

    pub fn before_expr(&self) -> bool {
        match self {
            Token::Await
            | Token::Case
            | Token::Default
            | Token::Do
            | Token::Else
            | Token::Return
            | Token::Throw
            | Token::New
            | Token::Extends
            | Token::Yield
            | Token::In
            | Token::InstanceOf
            | Token::TypeOf
            | Token::Void
            | Token::Delete => true,

            Token::Break
            | Token::Catch
            | Token::Continue
            | Token::Debugger
            | Token::Finally
            | Token::For
            | Token::Function
            | Token::If
            | Token::Switch
            | Token::Try
            | Token::Var
            | Token::Let
            | Token::Const
            | Token::While
            | Token::With
            | Token::This
            | Token::Super
            | Token::Class
            | Token::Export
            | Token::Import => false,

            Token::Async
            | Token::As
            | Token::From
            | Token::Of
            | Token::Static
            | Token::Target
            | Token::Asserts
            | Token::Implements
            | Token::Is
            | Token::Keyof
            | Token::Unique
            | Token::Object
            | Token::Global
            | Token::Enum
            | Token::Readonly
            | Token::Abstract
            | Token::Infer
            | Token::Any
            | Token::Boolean
            | Token::Bigint
            | Token::Intrinsic
            | Token::Never
            | Token::Number
            | Token::String
            | Token::Symbol
            | Token::Unknown
            | Token::Interface
            | Token::Declare
            | Token::Undefined
            | Token::Meta
            | Token::Type
            | Token::Assert
            | Token::Get
            | Token::Set
            | Token::Public
            | Token::Protected
            | Token::Private
            | Token::Package
            | Token::Override => false,

            Token::Null | Token::True | Token::False | Token::Ident => false,

            Token::BinOp(o) => o.before_expr(),

            Token::Arrow
            | Token::DotDotDot
            | Token::Bang
            | Token::LParen
            | Token::LBracket
            | Token::LBrace
            | Token::Semi
            | Token::Comma
            | Token::Colon
            | Token::ColonColon
            | Token::AssignOp(_)
            | Token::DollarLBrace
            | Token::QuestionMark
            | Token::PlusPlus
            | Token::MinusMinus
            | Token::Tilde => true,

            _ => false,
        }
    }

    pub fn is_word(&self) -> bool {
        match self {
            Token::Await
            | Token::Break
            | Token::Case
            | Token::Catch
            | Token::Continue
            | Token::Debugger
            | Token::Default
            | Token::Do
            | Token::Else
            | Token::Finally
            | Token::For
            | Token::Function
            | Token::If
            | Token::Return
            | Token::Switch
            | Token::Throw
            | Token::Try
            | Token::Var
            | Token::Let
            | Token::Const
            | Token::While
            | Token::With
            | Token::New
            | Token::This
            | Token::Super
            | Token::Class
            | Token::Extends
            | Token::Export
            | Token::Import
            | Token::Yield
            | Token::In
            | Token::InstanceOf
            | Token::TypeOf
            | Token::Void
            | Token::Delete
            | Token::Null
            | Token::True
            | Token::False
            | Token::Ident => true,

            Token::Async
            | Token::As
            | Token::From
            | Token::Of
            | Token::Static
            | Token::Target
            | Token::Asserts
            | Token::Implements
            | Token::Is
            | Token::Keyof
            | Token::Unique
            | Token::Object
            | Token::Global
            | Token::Enum
            | Token::Readonly
            | Token::Abstract
            | Token::Infer
            | Token::Any
            | Token::Boolean
            | Token::Bigint
            | Token::Intrinsic
            | Token::Never
            | Token::Number
            | Token::String
            | Token::Symbol
            | Token::Unknown
            | Token::Interface
            | Token::Declare
            | Token::Undefined
            | Token::Meta
            | Token::Type
            | Token::Assert
            | Token::Get
            | Token::Set
            | Token::Public
            | Token::Protected
            | Token::Private
            | Token::Package
            | Token::Override => true,

            _ => false,
        }
    }

    pub fn is_keyword(&self) -> bool {
        match self {
            Token::Await
            | Token::Break
            | Token::Case
            | Token::Catch
            | Token::Continue
            | Token::Debugger
            | Token::Default
            | Token::Do
            | Token::Else
            | Token::Finally
            | Token::For
            | Token::Function
            | Token::If
            | Token::Return
            | Token::Switch
            | Token::Throw
            | Token::Try
            | Token::Var
            | Token::Let
            | Token::Const
            | Token::While
            | Token::With
            | Token::New
            | Token::This
            | Token::Super
            | Token::Class
            | Token::Extends
            | Token::Export
            | Token::Import
            | Token::Yield
            | Token::In
            | Token::InstanceOf
            | Token::TypeOf
            | Token::Void
            | Token::Delete => true,
            _ => false,
        }
    }

    pub fn is_reserved_word(self, ctx: Context) -> bool {
        match self {
            Token::Let | Token::Static => ctx.is_strict(),
            Token::Await => ctx.flags.contains(ContextFlags::in_async) || ctx.is_strict(),
            Token::Yield => ctx.flags.contains(ContextFlags::in_generator) || ctx.is_strict(),

            Token::Null
            | Token::True
            | Token::False
            | Token::Break
            | Token::Case
            | Token::Catch
            | Token::Continue
            | Token::Debugger
            | Token::Default
            | Token::Do
            | Token::Export
            | Token::Else
            | Token::Finally
            | Token::For
            | Token::Function
            | Token::If
            | Token::Return
            | Token::Switch
            | Token::Throw
            | Token::Try
            | Token::Var
            | Token::Const
            | Token::While
            | Token::With
            | Token::New
            | Token::This
            | Token::Super
            | Token::Class
            | Token::Extends
            | Token::Import
            | Token::In
            | Token::InstanceOf
            | Token::TypeOf
            | Token::Void
            | Token::Delete => true,

            // Future reserved word
            Token::Enum => true,

            Token::Implements
            | Token::Package
            | Token::Protected
            | Token::Interface
            | Token::Private
            | Token::Public
                if ctx.is_strict() =>
            {
                true
            }

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
        matches!(self, Self::Add | Self::Sub)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenAndSpan {
    pub token: Token,
    /// Had a line break before this token?
    pub had_line_break: bool,
    pub span: Span,
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

            tok!('{') | tok!('[') | Token::Ident | tok!("yield") | tok!("await") => true,

            Token::Async
            | Token::As
            | Token::From
            | Token::Of
            | Token::Static
            | Token::Target
            | Token::Asserts
            | Token::Implements
            | Token::Is
            | Token::Keyof
            | Token::Unique
            | Token::Object
            | Token::Global
            | Token::Enum
            | Token::Readonly
            | Token::Abstract
            | Token::Infer
            | Token::Any
            | Token::Boolean
            | Token::Bigint
            | Token::Intrinsic
            | Token::Never
            | Token::Number
            | Token::String
            | Token::Symbol
            | Token::Unknown
            | Token::Interface
            | Token::Declare
            | Token::Undefined
            | Token::Meta
            | Token::Type
            | Token::Assert
            | Token::Get
            | Token::Set
            | Token::Public
            | Token::Protected
            | Token::Private
            | Token::Package
            | Token::Override => true,

            _ => false,
        }
    }
}
