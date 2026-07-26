use ast::*;
use ecma_visit::{VisitMut, VisitMutWith};

pub fn process(ast: &mut Program) {
    let mut visitor = Visitor;
    ast.visit_mut_with(&mut visitor);
}

struct Visitor;

impl VisitMut<'_> for Visitor {
    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        expr.visit_mut_children_with(self);

        let Expr::Bin(bin_expr) = expr else {
            return;
        };

        let new_op = match bin_expr.op {
            BinaryOp::EqEqEq => BinaryOp::EqEq,
            BinaryOp::NotEqEq => BinaryOp::NotEq,
            _ => return,
        };

        let is_lhs_typeof = matches!(
            bin_expr.left.as_ref(),
            Expr::Unary(UnaryExpr {
                op: UnaryOp::TypeOf,
                ..
            })
        );

        let is_rhs_typeof = matches!(
            bin_expr.right.as_ref(),
            Expr::Unary(UnaryExpr {
                op: UnaryOp::TypeOf,
                ..
            })
        );

        let is_lhs_string_lit = matches!(bin_expr.left.as_ref(), Expr::Lit(Lit::Str(_)));
        let is_rhs_string_lit = matches!(bin_expr.right.as_ref(), Expr::Lit(Lit::Str(_)));

        // `typeof x === ""` -> `typeof x == ""`
        // `typeof x !== ""` -> `typeof x != ""`
        if is_lhs_typeof && is_rhs_string_lit {
            bin_expr.op = new_op;
        }

        // `"" === typeof x` -> `"" == typeof x`
        // `"" !== typeof x` -> `"" != typeof x`
        if is_lhs_string_lit && is_rhs_typeof {
            bin_expr.op = new_op;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::resolver;
    use global_common::{GLOBALS, Globals, Mark};

    #[test]
    fn test_strict_typeof_equality() {
        test_transform("typeof foo === 'string'", "typeof foo == 'string'");
        test_transform("typeof foo !== 'string'", "typeof foo != 'string'");

        test_transform("'string' === typeof foo", "'string' == typeof foo");
        test_transform("'string' !== typeof foo", "'string' != typeof foo");
    }

    fn test_transform(input: &str, expected: &str) {
        crate::testing::test_transform(
            |mut program, _program_data| {
                GLOBALS.set(&Globals::new(), || {
                    let unresolved_mark = Mark::new();
                    let top_level_mark = Mark::new();

                    program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

                    process(&mut program);

                    program
                })
            },
            input,
            expected,
        );
    }
}
