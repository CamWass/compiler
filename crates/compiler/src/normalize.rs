use ast::*;
use common::util::take::Take;
use visit::{VisitMut, VisitMutWith};

use crate::utils::unwrap_as;

pub fn normalize(ast: &mut Program, program_data: &mut ast::TransformerProgramData) {
    // Split var decls.
    {
        let mut v = VarSplitter { program_data };
        ast.visit_mut_with(&mut v);
    }
    // Normalize shorthand assignments.
    {
        let mut v = NormalizeAssignShorthand { program_data };
        ast.visit_mut_with(&mut v);
    }
}

// TODO: how useful is this if it can't simplify all shorthand assigns? Would
// it be better to just make passes handle shorthand, as many already do?
/// Converts shorthand assignments to plain assignments with a binary expr. E.g.
/// ```js
/// a += 1;
/// b *= 2;
/// ```
/// to
/// ```js
/// a = a + 1;
/// b = b * 2;
/// ```
struct NormalizeAssignShorthand<'a> {
    program_data: &'a mut ast::TransformerProgramData,
}

impl VisitMut<'_> for NormalizeAssignShorthand<'_> {
    fn visit_mut_assign_expr(&mut self, node: &mut AssignExpr) {
        node.visit_mut_children_with(self);

        let lhs_ident = match &node.left {
            PatOrExpr::Expr(lhs) => match lhs.as_ref() {
                Expr::Ident(lhs) => lhs,
                _ => return,
            },
            PatOrExpr::Pat(lhs) => match lhs.as_ref() {
                Pat::Ident(lhs) => &lhs.id,
                _ => return,
            },
        };

        let op = match node.op {
            AssignOp::AddAssign => BinaryOp::Add,
            AssignOp::SubAssign => BinaryOp::Sub,
            AssignOp::MulAssign => BinaryOp::Mul,
            AssignOp::DivAssign => BinaryOp::Div,
            AssignOp::ModAssign => BinaryOp::Mod,
            AssignOp::LShiftAssign => BinaryOp::LShift,
            AssignOp::RShiftAssign => BinaryOp::RShift,
            AssignOp::ZeroFillRShiftAssign => BinaryOp::ZeroFillRShift,
            AssignOp::BitOrAssign => BinaryOp::BitOr,
            AssignOp::BitXorAssign => BinaryOp::BitXor,
            AssignOp::BitAndAssign => BinaryOp::BitAnd,
            AssignOp::ExpAssign => BinaryOp::Exp,

            // Not shorthand.
            AssignOp::Assign => return,

            AssignOp::AndAssign => BinaryOp::LogicalAnd,
            AssignOp::OrAssign => BinaryOp::LogicalOr,
            AssignOp::NullishAssign => BinaryOp::NullishCoalescing,
        };

        debug_assert!(
            lhs_ident.name.is_unresolved(),
            "normalise should run before resolver"
        );

        let right_id = self.program_data.new_id_from(node.node_id);
        node.right.as_mut().map_with_mut(|right| {
            Expr::Bin(BinExpr {
                node_id: right_id,
                op,
                left: Box::new(Expr::Ident(Ident {
                    node_id: self.program_data.new_id_from(lhs_ident.node_id),
                    name: lhs_ident.name,
                })),
                right: Box::new(right),
            })
        });
        node.op = AssignOp::Assign;
    }
}

/// Splits var decl statements with multiple declarations into separate statements.
/// E.g.
/// ```js
/// let a, b;
/// ```
/// to
/// ```js
/// let a;
/// let b;
/// ```
struct VarSplitter<'a> {
    program_data: &'a mut ast::TransformerProgramData,
}

impl VisitMut<'_> for VarSplitter<'_> {
    // Handle statement lists: If a var stmt has multiple declarations, we replace
    // it with a new stmt for each declaration. E.g. `let a, b;` -> `let a; let b`;

    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        let mut i = 0;
        while i < stmts.len() {
            stmts[i].visit_mut_with(self);

            if matches!(&stmts[i], Stmt::Decl(Decl::Var(v)) if v.decls.len() > 1) {
                let stmt = stmts.remove(i);
                let var = unwrap_as!(stmt, Stmt::Decl(Decl::Var(v)), v);
                let kind = var.kind;
                let num_decls = var.decls.len();
                // Insert the new statements at the index of the old one to preserve ordering.
                stmts.splice(
                    i..i,
                    var.decls.into_iter().map(|decl| {
                        Stmt::Decl(Decl::Var(VarDecl {
                            node_id: self.program_data.new_id_from(decl.node_id),
                            kind,
                            decls: vec![decl],
                        }))
                    }),
                );
                // Skip over the new stmts.
                i += num_decls - 1;
            } else {
                i += 1;
            }
        }
    }
    fn visit_mut_module_items(&mut self, items: &mut Vec<ModuleItem>) {
        let mut i = 0;
        while i < items.len() {
            items[i].visit_mut_with(self);

            if matches!(&items[i], ModuleItem::Stmt(Stmt::Decl(Decl::Var(v))) if v.decls.len() > 1)
            {
                let stmt = items.remove(i);
                let var = unwrap_as!(stmt, ModuleItem::Stmt(Stmt::Decl(Decl::Var(v))), v);
                let kind = var.kind;
                let num_decls = var.decls.len();
                items.splice(
                    i..i,
                    var.decls.into_iter().map(|decl| {
                        ModuleItem::Stmt(Stmt::Decl(Decl::Var(VarDecl {
                            node_id: self.program_data.new_id_from(decl.node_id),
                            kind,
                            decls: vec![decl],
                        })))
                    }),
                );
                // Skip over the new stmts.
                i += num_decls - 1;
            } else {
                i += 1;
            }
        }
    }
}

// TODO: tests

/*
const a = 1, b = () => {const c = 3, d = 4;};
*/
