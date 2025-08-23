use ast::*;
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::util::take::Take;

use crate::utils::unwrap_as;

// TODO: move logic into codegen

pub fn denormalize(ast: &mut Program) {
    let mut v = Denormalize {};
    ast.visit_mut_with(&mut v);
}

struct Denormalize {}

impl VisitMut<'_> for Denormalize {
    fn visit_mut_assign_expr(&mut self, node: &mut AssignExpr) {
        node.visit_mut_children_with(self);

        let assign_lhs = match &node.left {
            PatOrExpr::Expr(lhs) => match lhs.as_ref() {
                Expr::Ident(lhs) => lhs,
                _ => return,
            },
            PatOrExpr::Pat(lhs) => match lhs.as_ref() {
                Pat::Ident(lhs) => &lhs.id,
                _ => return,
            },
        };

        if let Expr::Bin(assign_rhs) = node.right.as_ref() {
            if let Expr::Ident(bin_lhs) = assign_rhs.left.as_ref() {
                if assign_lhs.sym == bin_lhs.sym {
                    // We have something of the form `lhs = lhs X foo` where `X`
                    // is a bin op and `foo` is some expr.
                    let new_assign_op = match assign_rhs.op {
                        BinaryOp::Add => AssignOp::AddAssign,
                        BinaryOp::Sub => AssignOp::SubAssign,
                        BinaryOp::Mul => AssignOp::MulAssign,
                        BinaryOp::Div => AssignOp::DivAssign,
                        BinaryOp::Mod => AssignOp::ModAssign,
                        BinaryOp::LShift => AssignOp::LShiftAssign,
                        BinaryOp::RShift => AssignOp::RShiftAssign,
                        BinaryOp::ZeroFillRShift => AssignOp::ZeroFillRShiftAssign,
                        BinaryOp::BitOr => AssignOp::BitOrAssign,
                        BinaryOp::BitXor => AssignOp::BitXorAssign,
                        BinaryOp::BitAnd => AssignOp::BitAndAssign,
                        BinaryOp::Exp => AssignOp::ExpAssign,
                        _ => return,
                    };

                    node.op = new_assign_op;
                    node.right
                        .as_mut()
                        .map_with_mut(|right| *unwrap_as!(right, Expr::Bin(e), e).right);
                }
            }
        }
    }
}
