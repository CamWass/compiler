use ast::*;
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::{util::take::Take, SyntaxContext};

use crate::utils::unwrap_as;

// TODO: colours:
// TODO: preserve spans?

pub fn normalize(ast: &mut Program, program_data: &mut ast::ProgramData) {
    // Add blocks to single-statement contexts.
    {
        let mut v = BlockCreator { program_data };
        ast.visit_mut_with(&mut v);
    }
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
    program_data: &'a mut ast::ProgramData,
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

            AssignOp::AndAssign | AssignOp::OrAssign | AssignOp::NullishAssign => {
                todo!("normalize logical assign")
            }
        };

        let right_id = self.program_data.new_id_from(node.node_id);
        node.right.as_mut().map_with_mut(|right| {
            Expr::Bin(BinExpr {
                node_id: right_id,
                op,
                left: Box::new(Expr::Ident(Ident {
                    node_id: self.program_data.new_id_from(lhs_ident.node_id),
                    sym: lhs_ident.sym.clone(),
                    ctxt: SyntaxContext::empty(),
                })),
                right: Box::new(right),
            })
        });
        node.op = AssignOp::Assign;
    }
}

/// Visits statements that are in single-statement contexts (e.g. for loop body).
/// If the statement is not a [BlockStmt], it is replaced with a [BlockStmt]
/// containing the statement as its only child. E.g.
/// ```js
/// for (;;) foo();
/// ```
/// to
/// ```js
/// for (;;) {foo();}
/// ```
/// We don't have to wory abount block scoping because lexical (block-scoped)
/// declarations are forbidden in single-statement contexts, so placing the
/// statement in a block is always safe.
struct BlockCreator<'a> {
    program_data: &'a mut ast::ProgramData,
}

impl BlockCreator<'_> {
    fn handle_single_stmt(&mut self, stmt: &mut Stmt) {
        stmt.visit_mut_with(self);

        if !matches!(stmt, Stmt::Block(_)) {
            stmt.map_with_mut(|stmt| {
                Stmt::Block(BlockStmt {
                    node_id: self.program_data.new_id_from(stmt.node_id()),
                    stmts: vec![stmt],
                })
            });
        }
    }
}

impl VisitMut<'_> for BlockCreator<'_> {
    // Rewrite blockless arrow functions to have a block with a single return statement.
    // For example: `(x) => x` becomes `(x) => { return x; }`.
    // This simplifies optimizations as they can now assume all functions have a BLOCK.
    fn visit_mut_arrow_expr(&mut self, node: &mut ArrowExpr) {
        if matches!(node.body, BlockStmtOrExpr::Expr(_)) {
            node.body.map_with_mut(|body| {
                let expr = unwrap_as!(body, BlockStmtOrExpr::Expr(e), e);
                let expr_id = expr.node_id();
                BlockStmtOrExpr::BlockStmt(BlockStmt {
                    node_id: self.program_data.new_id_from(expr_id),
                    stmts: vec![Stmt::Return(ReturnStmt {
                        node_id: self.program_data.new_id_from(expr_id),
                        arg: Some(expr),
                    })],
                })
            });
        }
    }

    fn visit_mut_with_stmt(&mut self, node: &mut WithStmt) {
        self.handle_single_stmt(node.body.as_mut());
    }
    fn visit_mut_labeled_stmt(&mut self, node: &mut LabeledStmt) {
        node.body.visit_mut_with(self);

        // Loop labels must remain in place as named continues are not allowed
        // for labeled blocks. E.g. is is invalid to change:
        //
        // foo: for (a in b) {
        //   continue foo;
        // }
        //
        // into:
        //
        // foo: {
        //   for (a in b) {
        //     continue foo;
        //   }
        // }
        if !matches!(
            node.body.as_ref(),
            Stmt::Labeled(_)
                | Stmt::Block(_)
                | Stmt::For(_)
                | Stmt::ForIn(_)
                | Stmt::ForOf(_)
                | Stmt::While(_)
                | Stmt::DoWhile(_)
        ) {
            node.body.as_mut().map_with_mut(|stmt| {
                Stmt::Block(BlockStmt {
                    node_id: self.program_data.new_id_from(stmt.node_id()),
                    stmts: vec![stmt],
                })
            });
        }
    }
    fn visit_mut_if_stmt(&mut self, node: &mut IfStmt) {
        self.handle_single_stmt(node.cons.as_mut());
        if let Some(alt) = &mut node.alt {
            self.handle_single_stmt(alt.as_mut());
        }
    }
    fn visit_mut_while_stmt(&mut self, node: &mut WhileStmt) {
        self.handle_single_stmt(node.body.as_mut());
    }
    fn visit_mut_do_while_stmt(&mut self, node: &mut DoWhileStmt) {
        self.handle_single_stmt(node.body.as_mut());
    }
    fn visit_mut_for_stmt(&mut self, node: &mut ForStmt) {
        self.handle_single_stmt(node.body.as_mut());
    }
    fn visit_mut_for_in_stmt(&mut self, node: &mut ForInStmt) {
        self.handle_single_stmt(node.body.as_mut());
    }
    fn visit_mut_for_of_stmt(&mut self, node: &mut ForOfStmt) {
        self.handle_single_stmt(node.body.as_mut());
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
    program_data: &'a mut ast::ProgramData,
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

// // TODO: doc comment
// struct DuplicateDeclRemover<'a> {
//     node_id_gen: &'a mut ast::NodeIdGen,
// }

// macro_rules! handle_scope {
//     ([$([$name:ident, $N:ident]$(,)?)*]) => {
//         $(
//             #[inline]
//             fn $name(&mut self, n: &mut ast::$N) {
//                 // Start a new namespace for label names.
//                 self.namespace_stack.push(LabelNamespace::default());
//                 n.visit_mut_children_with(self);
//                 self.namespace_stack.pop();
//             }
//         )*
//     };
// }

// // TODO:
// // remove empty decls that follow another decl

// impl VisitMut<'_> for DuplicateDeclRemover<'_> {
//     noop_visit_mut_type!();

//     fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
//         stmts.retain_mut(f)
//     }

//     // Note:
//     // Program is not a function; it creates the initial/global scope.
//     // ClassMethod, PrivateMethod, and MethodProp are handled by their Function child.
//     handle_scope!([
//         [visit_mut_program, Program],
//         [visit_mut_function, Function],
//         [visit_mut_constructor, Constructor],
//         [visit_mut_setter_prop, SetterProp],
//         [visit_mut_getter_prop, GetterProp],
//         [visit_mut_arrow_expr, ArrowExpr],
//     ]);
// }

// // TODO: doc comment
// struct VarAnalyser {
//     vars: FxHashMap<Id, Vec<NodeId>>,
// }

// TODO: tests

/*
const a = 1, b = () => {const c = 3, d = 4;};
*/
