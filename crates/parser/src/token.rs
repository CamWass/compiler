use crate::{
    context::{Context, ContextFlags},
    error::Error,
};
pub(crate) use ast::AssignOp;
use ast::{BinaryOp, NameId, RegexFlags};
use atoms::JsWord;
use common::Span;
use num_bigint::BigUint;
use std::fmt::{Debug, Display};

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Token {
    // The lexer relies on this section having the lowest discriminants (i.e.
    // coming first in the enum) and having the same ordering as the
    // corresponding NameIds, so converting between them is a no-op.
    // --Start--
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
    Yield,
    In,
    InstanceOf,
    TypeOf,
    Void,
    Delete,
    Null,
    True,
    False,

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
    // --End--
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

    EqEq,
    NotEq,
    EqEqEq,
    NotEqEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    LShift,
    RShift,
    ZeroFillRShift,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitOr,
    BitXor,
    BitAnd,
    Exp,
    LogicalOr,
    LogicalAnd,
    NullishCoalescing,

    /// `=`
    Assign,
    /// `+=`
    AddAssign,
    /// `-=`
    SubAssign,
    /// `*=`
    MulAssign,
    /// `/=`
    DivAssign,
    /// `%=`
    ModAssign,
    /// `<<=`
    LShiftAssign,
    /// `>>=`
    RShiftAssign,
    /// `>>>=`
    ZeroFillRShiftAssign,
    /// `|=`
    BitOrAssign,
    /// `^=`
    BitXorAssign,
    /// `&=`
    BitAndAssign,
    /// `**=`
    ExpAssign,
    /// `&&=`
    AndAssign,
    /// `||=`
    OrAssign,
    /// `??=`
    NullishAssign,

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
            Token::EqEq => "==",
            Token::NotEq => "!",
            Token::EqEqEq => "===",
            Token::NotEqEq => "!==",
            Token::Lt => "<",
            Token::LtEq => "<=",
            Token::Gt => ">",
            Token::GtEq => ">=",
            Token::LShift => "<<",
            Token::RShift => ">>",
            Token::ZeroFillRShift => ">>>",
            Token::Add => "+",
            Token::Sub => "-",
            Token::Mul => "*",
            Token::Div => "/",
            Token::Mod => "%",
            Token::BitOr => "|",
            Token::BitXor => "^",
            Token::BitAnd => "&",
            Token::Exp => "**",
            Token::LogicalOr => "||",
            Token::LogicalAnd => "&&",
            Token::NullishCoalescing => "??",
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
            Token::Assign => "=",
            Token::AddAssign => "+=",
            Token::SubAssign => "-=",
            Token::MulAssign => "*=",
            Token::DivAssign => "/=",
            Token::ModAssign => "%=",
            Token::LShiftAssign => "<<=",
            Token::RShiftAssign => ">>=",
            Token::ZeroFillRShiftAssign => ">>>=",
            Token::BitOrAssign => "|=",
            Token::BitXorAssign => "^=",
            Token::BitAndAssign => "&=",
            Token::ExpAssign => "**=",
            Token::AndAssign => "&&=",
            Token::OrAssign => "||=",
            Token::NullishAssign => "??=",
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

    BigInt(Box<BigUint>),

    Error(Error),
}

impl Token {
    pub fn is_known_ident(&self) -> bool {
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
            _ => false,
        }
    }

    pub fn starts_expr(&self) -> bool {
        match self {
            Token::Ident
            | Token::Null
            | Token::True
            | Token::False
            | Token::Await
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
            | Token::Delete
            | Token::Add
            | Token::Sub
            | Token::Bang
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
            _ => self.is_known_ident(),
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
            | Token::Delete
            | Token::Arrow
            | Token::DotDotDot
            | Token::Bang
            | Token::LParen
            | Token::LBracket
            | Token::LBrace
            | Token::Semi
            | Token::Comma
            | Token::Colon
            | Token::ColonColon
            | Token::DollarLBrace
            | Token::QuestionMark
            | Token::PlusPlus
            | Token::MinusMinus
            | Token::Tilde => true,
            _ => self.as_bin_op().is_some() || self.as_assign_op().is_some(),
        }
    }

    pub fn is_word(&self) -> bool {
        match self {
            Token::Null | Token::True | Token::False | Token::Ident => true,
            _ => self.is_keyword() || self.is_known_ident(),
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

    pub fn as_assign_op(&self) -> Option<AssignOp> {
        match self {
            Token::Assign => Some(AssignOp::Assign),
            Token::AddAssign => Some(AssignOp::AddAssign),
            Token::SubAssign => Some(AssignOp::SubAssign),
            Token::MulAssign => Some(AssignOp::MulAssign),
            Token::DivAssign => Some(AssignOp::DivAssign),
            Token::ModAssign => Some(AssignOp::ModAssign),
            Token::LShiftAssign => Some(AssignOp::LShiftAssign),
            Token::RShiftAssign => Some(AssignOp::RShiftAssign),
            Token::ZeroFillRShiftAssign => Some(AssignOp::ZeroFillRShiftAssign),
            Token::BitOrAssign => Some(AssignOp::BitOrAssign),
            Token::BitXorAssign => Some(AssignOp::BitXorAssign),
            Token::BitAndAssign => Some(AssignOp::BitAndAssign),
            Token::ExpAssign => Some(AssignOp::ExpAssign),
            Token::AndAssign => Some(AssignOp::AndAssign),
            Token::OrAssign => Some(AssignOp::OrAssign),
            Token::NullishAssign => Some(AssignOp::NullishAssign),
            _ => None,
        }
    }

    pub fn as_bin_op(&self) -> Option<BinaryOp> {
        match self {
            Token::EqEq => Some(BinaryOp::EqEq),
            Token::NotEq => Some(BinaryOp::NotEq),
            Token::EqEqEq => Some(BinaryOp::EqEqEq),
            Token::NotEqEq => Some(BinaryOp::NotEqEq),
            Token::Lt => Some(BinaryOp::Lt),
            Token::LtEq => Some(BinaryOp::LtEq),
            Token::Gt => Some(BinaryOp::Gt),
            Token::GtEq => Some(BinaryOp::GtEq),
            Token::LShift => Some(BinaryOp::LShift),
            Token::RShift => Some(BinaryOp::RShift),
            Token::ZeroFillRShift => Some(BinaryOp::ZeroFillRShift),
            Token::Add => Some(BinaryOp::Add),
            Token::Sub => Some(BinaryOp::Sub),
            Token::Mul => Some(BinaryOp::Mul),
            Token::Div => Some(BinaryOp::Div),
            Token::Mod => Some(BinaryOp::Mod),
            Token::BitOr => Some(BinaryOp::BitOr),
            Token::BitXor => Some(BinaryOp::BitXor),
            Token::BitAnd => Some(BinaryOp::BitAnd),
            Token::Exp => Some(BinaryOp::Exp),
            Token::LogicalOr => Some(BinaryOp::LogicalOr),
            Token::LogicalAnd => Some(BinaryOp::LogicalAnd),
            Token::NullishCoalescing => Some(BinaryOp::NullishCoalescing),

            _ => None,
        }
    }

    pub fn is_binary_op(&self) -> bool {
        self.as_bin_op().is_some()
    }

    /// Returns true if `self` can follow keyword let.
    ///
    /// e.g. `let a = xx;`, `let {a:{}} = 1`
    pub(crate) fn follows_keyword_let(&self) -> bool {
        match self {
            // This is required to recognize `let let` in strict mode.
            tok!("let") => true,
            tok!('{') | tok!('[') | Token::Ident | tok!("yield") | tok!("await") => true,
            _ => self.is_known_ident(),
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
