use clone_node::CloneNode;

#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, CloneNode)]
pub enum BinaryOp {
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

    /// `||`
    LogicalOr,

    /// `&&`
    LogicalAnd,

    /// `in`
    In,
    /// `instanceof`
    InstanceOf,

    /// `**`
    Exp,

    /// `??`
    NullishCoalescing,
}

impl BinaryOp {
    pub fn precedence(self) -> u8 {
        match self {
            BinaryOp::EqEq => 6,
            BinaryOp::NotEq => 6,
            BinaryOp::EqEqEq => 6,
            BinaryOp::NotEqEq => 6,
            BinaryOp::Lt => 7,
            BinaryOp::LtEq => 7,
            BinaryOp::Gt => 7,
            BinaryOp::GtEq => 7,
            BinaryOp::LShift => 8,
            BinaryOp::RShift => 8,
            BinaryOp::ZeroFillRShift => 8,

            BinaryOp::Add => 9,
            BinaryOp::Sub => 9,
            BinaryOp::Mul => 10,
            BinaryOp::Div => 10,
            BinaryOp::Mod => 10,

            BinaryOp::BitOr => 3,
            BinaryOp::BitXor => 4,

            BinaryOp::BitAnd => 5,

            BinaryOp::LogicalOr => 1,

            BinaryOp::LogicalAnd => 2,
            BinaryOp::In => 7,
            BinaryOp::InstanceOf => 7,

            BinaryOp::Exp => 11,

            BinaryOp::NullishCoalescing => 1,
        }
    }
}

impl BinaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            BinaryOp::EqEq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::EqEqEq => "===",
            BinaryOp::NotEqEq => "!==",
            BinaryOp::Lt => "<",
            BinaryOp::LtEq => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::GtEq => ">=",
            BinaryOp::LShift => "<<",
            BinaryOp::RShift => ">>",
            BinaryOp::ZeroFillRShift => ">>>",
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::BitAnd => "&",
            BinaryOp::LogicalOr => "||",
            BinaryOp::LogicalAnd => "&&",
            BinaryOp::In => "in",
            BinaryOp::InstanceOf => "instanceof",
            BinaryOp::Exp => "**",
            BinaryOp::NullishCoalescing => "??",
        }
    }
}

impl std::fmt::Debug for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = self.as_str();
        std::fmt::Debug::fmt(s, f)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, CloneNode)]
pub enum AssignOp {
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
}

impl AssignOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            AssignOp::Assign => "=",
            AssignOp::AddAssign => "+=",
            AssignOp::SubAssign => "-=",
            AssignOp::MulAssign => "*=",
            AssignOp::DivAssign => "/=",
            AssignOp::ModAssign => "%=",
            AssignOp::LShiftAssign => "<<=",
            AssignOp::RShiftAssign => ">>=",
            AssignOp::ZeroFillRShiftAssign => ">>>=",
            AssignOp::BitOrAssign => "|=",
            AssignOp::BitXorAssign => "^=",
            AssignOp::BitAndAssign => "&=",
            AssignOp::ExpAssign => "**=",
            AssignOp::AndAssign => "&&=",
            AssignOp::OrAssign => "||=",
            AssignOp::NullishAssign => "??=",
        }
    }
}

impl std::fmt::Debug for AssignOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = self.as_str();
        std::fmt::Debug::fmt(s, f)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, CloneNode)]
pub enum UpdateOp {
    /// `++`
    PlusPlus,
    /// `--`
    MinusMinus,
}

impl UpdateOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            UpdateOp::PlusPlus => "++",
            UpdateOp::MinusMinus => "--",
        }
    }
}

impl std::fmt::Debug for UpdateOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = self.as_str();
        std::fmt::Debug::fmt(s, f)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, CloneNode)]
pub enum UnaryOp {
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `!`
    Bang,
    /// `~`
    Tilde,
    /// `typeof`
    TypeOf,
    /// `void`
    Void,
    /// `delete`
    Delete,
}

impl UnaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnaryOp::Minus => "-",
            UnaryOp::Plus => "+",
            UnaryOp::Bang => "!",
            UnaryOp::Tilde => "~",
            UnaryOp::TypeOf => "typeof",
            UnaryOp::Void => "void",
            UnaryOp::Delete => "delete",
        }
    }
}

impl std::fmt::Debug for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = self.as_str();
        std::fmt::Debug::fmt(s, f)
    }
}
