macro_rules! tok {
    ('`') => {
        crate::token::Token::BackQuote
    };
    (';') => {
        crate::token::Token::Semi
    };
    ('#') => {
        crate::token::Token::Hash
    };

    ('&') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::BitAnd)
    };
    ('+') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Add)
    };
    ('-') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Sub)
    };
    ("??") => {
        crate::token::Token::BinOp(crate::token::BinOpToken::NullishCoalescing)
    };
    ('~') => {
        crate::token::Token::Tilde
    };
    ('!') => {
        crate::token::Token::Bang
    };
    ("??=") => {
        crate::token::Token::AssignOp(crate::token::AssignOpToken::NullishAssign)
    };

    ('|') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::BitOr)
    };

    (',') => {
        crate::token::Token::Comma
    };
    ('?') => {
        crate::token::Token::QuestionMark
    };
    (':') => {
        crate::token::Token::Colon
    };
    ('.') => {
        crate::token::Token::Dot
    };
    ("=>") => {
        crate::token::Token::Arrow
    };
    ("...") => {
        crate::token::Token::DotDotDot
    };
    ("${") => {
        crate::token::Token::DollarLBrace
    };

    ('*') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Mul)
    };
    ('/') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Div)
    };
    ("/=") => {
        crate::token::Token::AssignOp(DivAssign)
    };
    ('<') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Lt)
    };
    ('>') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Gt)
    };

    ("++") => {
        crate::token::Token::PlusPlus
    };
    ("--") => {
        crate::token::Token::MinusMinus
    };

    ('=') => {
        crate::token::Token::AssignOp(crate::token::AssignOpToken::Assign)
    };

    ('(') => {
        crate::token::Token::LParen
    };
    (')') => {
        crate::token::Token::RParen
    };
    ('{') => {
        crate::token::Token::LBrace
    };
    ('}') => {
        crate::token::Token::RBrace
    };
    ('[') => {
        crate::token::Token::LBracket
    };
    (']') => {
        crate::token::Token::RBracket
    };

    ("async") => {
        crate::token::Token::Async
    };
    ("as") => {
        crate::token::Token::As
    };
    ("await") => {
        crate::token::Token::Await
    };
    ("break") => {
        crate::token::Token::Break
    };
    ("case") => {
        crate::token::Token::Case
    };
    ("catch") => {
        crate::token::Token::Catch
    };
    ("class") => {
        crate::token::Token::Class
    };
    ("const") => {
        crate::token::Token::Const
    };
    ("continue") => {
        crate::token::Token::Continue
    };
    ("debugger") => {
        crate::token::Token::Debugger
    };
    ("default") => {
        crate::token::Token::Default
    };
    ("delete") => {
        crate::token::Token::Delete
    };
    ("do") => {
        crate::token::Token::Do
    };
    ("else") => {
        crate::token::Token::Else
    };
    ("export") => {
        crate::token::Token::Export
    };
    ("extends") => {
        crate::token::Token::Extends
    };
    ("false") => {
        crate::token::Token::False
    };
    ("finally") => {
        crate::token::Token::Finally
    };
    ("for") => {
        crate::token::Token::For
    };
    ("from") => {
        crate::token::Token::From
    };
    ("function") => {
        crate::token::Token::Function
    };
    ("if") => {
        crate::token::Token::If
    };
    ("in") => {
        crate::token::Token::In
    };
    ("import") => {
        crate::token::Token::Import
    };
    ("let") => {
        crate::token::Token::Let
    };
    ("new") => {
        crate::token::Token::New
    };
    ("null") => {
        crate::token::Token::Null
    };
    ("of") => {
        crate::token::Token::Of
    };
    ("return") => {
        crate::token::Token::Return
    };
    ("super") => {
        crate::token::Token::Super
    };
    ("static") => {
        crate::token::Token::Static
    };
    ("switch") => {
        crate::token::Token::Switch
    };
    ("target") => {
        crate::token::Token::Target
    };
    ("this") => {
        crate::token::Token::This
    };
    ("throw") => {
        crate::token::Token::Throw
    };
    ("true") => {
        crate::token::Token::True
    };
    ("try") => {
        crate::token::Token::Try
    };
    ("typeof") => {
        crate::token::Token::TypeOf
    };
    ("var") => {
        crate::token::Token::Var
    };
    ("void") => {
        crate::token::Token::Void
    };
    ("while") => {
        crate::token::Token::While
    };
    ("with") => {
        crate::token::Token::With
    };
    ("yield") => {
        crate::token::Token::Yield
    };

    // ----------
    // Typescript
    // ----------
    ("asserts") => {
        crate::token::Token::Asserts
    };
    ("implements") => {
        crate::token::Token::Implements
    };
    ("is") => {
        crate::token::Token::Is
    };
    ("keyof") => {
        crate::token::Token::Keyof
    };
    ("unique") => {
        crate::token::Token::Unique
    };
    ("object") => {
        crate::token::Token::Object
    };
    ("global") => {
        crate::token::Token::Global
    };
    ("enum") => {
        crate::token::Token::Enum
    };
    ("readonly") => {
        crate::token::Token::Readonly
    };
    ("abstract") => {
        crate::token::Token::Abstract
    };
    ("infer") => {
        crate::token::Token::Infer
    };
    ("any") => {
        crate::token::Token::Any
    };
    ("boolean") => {
        crate::token::Token::Boolean
    };
    ("bigint") => {
        crate::token::Token::Bigint
    };
    ("intrinsic") => {
        crate::token::Token::Intrinsic
    };
    ("never") => {
        crate::token::Token::Never
    };
    ("number") => {
        crate::token::Token::Number
    };
    ("string") => {
        crate::token::Token::String
    };
    ("symbol") => {
        crate::token::Token::Symbol
    };
    ("unknown") => {
        crate::token::Token::Unknown
    };
    ("interface") => {
        crate::token::Token::Interface
    };
    ("declare") => {
        crate::token::Token::Declare
    };
    ("undefined") => {
        crate::token::Token::Undefined
    };
    ("meta") => {
        crate::token::Token::Meta
    };
    ("type") => {
        crate::token::Token::Type
    };
    ("assert") => {
        crate::token::Token::Assert
    };
    ("get") => {
        crate::token::Token::Get
    };
    ("set") => {
        crate::token::Token::Set
    };
}
