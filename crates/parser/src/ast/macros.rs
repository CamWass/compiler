/// Creates a corresponding operator.
///
/// Unary +,- is `op!(unary, "+")`, `op!(unary, "-")`.
///
/// Binary +,- is `op!(bin, "+")`, `op!(bin, "-")`.
#[macro_export]
macro_rules! op {
    (unary,"-") => {
        $crate::ast::UnaryOp::Minus
    };
    (unary,"+") => {
        $crate::ast::UnaryOp::Plus
    };
    ("!") => {
        $crate::ast::UnaryOp::Bang
    };
    ("~") => {
        $crate::ast::UnaryOp::Tilde
    };
    ("typeof") => {
        $crate::ast::UnaryOp::TypeOf
    };
    ("void") => {
        $crate::ast::UnaryOp::Void
    };
    ("delete") => {
        $crate::ast::UnaryOp::Delete
    };

    ("++") => {
        $crate::ast::UpdateOp::PlusPlus
    };
    ("--") => {
        $crate::ast::UpdateOp::MinusMinus
    };

    ("==") => {
        $crate::ast::BinaryOp::EqEq
    };
    ("!=") => {
        $crate::ast::BinaryOp::NotEq
    };
    ("===") => {
        $crate::ast::BinaryOp::EqEqEq
    };
    ("!==") => {
        $crate::ast::BinaryOp::NotEqEq
    };
    ("<") => {
        $crate::ast::BinaryOp::Lt
    };
    ("<=") => {
        $crate::ast::BinaryOp::LtEq
    };
    (">") => {
        $crate::ast::BinaryOp::Gt
    };
    (">=") => {
        $crate::ast::BinaryOp::GtEq
    };
    ("<<") => {
        $crate::ast::BinaryOp::LShift
    };
    (">>") => {
        $crate::ast::BinaryOp::RShift
    };
    (">>>") => {
        $crate::ast::BinaryOp::ZeroFillRShift
    };
    (bin,"+") => {
        $crate::ast::BinaryOp::Add
    };
    (bin,"-") => {
        $crate::ast::BinaryOp::Sub
    };
    ("*") => {
        $crate::ast::BinaryOp::Mul
    };
    ("/") => {
        $crate::ast::BinaryOp::Div
    };
    ("%") => {
        $crate::ast::BinaryOp::Mod
    };
    ("|") => {
        $crate::ast::BinaryOp::BitOr
    };
    ("^") => {
        $crate::ast::BinaryOp::BitXor
    };
    ("&") => {
        $crate::ast::BinaryOp::BitAnd
    };
    ("||") => {
        $crate::ast::BinaryOp::LogicalOr
    };
    ("&&") => {
        $crate::ast::BinaryOp::LogicalAnd
    };
    ("in") => {
        $crate::ast::BinaryOp::In
    };
    ("instanceof") => {
        $crate::ast::BinaryOp::InstanceOf
    };
    ("**") => {
        $crate::ast::BinaryOp::Exp
    };
    ("??") => {
        $crate::ast::BinaryOp::NullishCoalescing
    };

    ("=") => {
        $crate::ast::AssignOp::Assign
    };
    ("+=") => {
        $crate::ast::AssignOp::AddAssign
    };
    ("-=") => {
        $crate::ast::AssignOp::SubAssign
    };
    ("*=") => {
        $crate::ast::AssignOp::MulAssign
    };
    ("/=") => {
        $crate::ast::AssignOp::DivAssign
    };
    ("%=") => {
        $crate::ast::AssignOp::ModAssign
    };
    ("<<=") => {
        $crate::ast::AssignOp::LShiftAssign
    };
    (">>=") => {
        $crate::ast::AssignOp::RShiftAssign
    };
    (">>>=") => {
        $crate::ast::AssignOp::ZeroFillRShiftAssign
    };
    ("|=") => {
        $crate::ast::AssignOp::BitOrAssign
    };
    ("^=") => {
        $crate::ast::AssignOp::BitXorAssign
    };
    ("&=") => {
        $crate::ast::AssignOp::BitAndAssign
    };
    ("**=") => {
        $crate::ast::AssignOp::ExpAssign
    };
    ("&&=") => {
        $crate::ast::AssignOp::AndAssign
    };
    ("||=") => {
        $crate::ast::AssignOp::OrAssign
    };
    ("??=") => {
        $crate::ast::AssignOp::NullishAssign
    };
}
