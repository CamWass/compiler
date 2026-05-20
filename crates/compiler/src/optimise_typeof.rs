use ast::*;
use atoms::js_word;
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::SyntaxContext;

use crate::optimize_properties::{analyse, graph::Graph, Id, Pointer, PointerId, Store};

pub fn process(ast: &mut Program, program_data: &mut ProgramData, unresolved_ctxt: SyntaxContext) {
    let (store, points_to) = analyse(ast, unresolved_ctxt);

    let mut visitor = Visitor {
        program_data,
        unresolved_ctxt,
        store,
        points_to,
    };
    ast.visit_mut_with(&mut visitor);
}

struct Visitor<'a> {
    program_data: &'a mut ProgramData,
    unresolved_ctxt: SyntaxContext,
    store: Store,
    points_to: Graph,
}

impl VisitMut<'_> for Visitor<'_> {
    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        expr.visit_mut_children_with(self);

        let Expr::Bin(bin_expr) = expr else {
            return;
        };

        let typeof_arg;
        let type_string;

        if let Expr::Unary(lhs) = bin_expr.left.as_ref() {
            if lhs.op != UnaryOp::TypeOf {
                return;
            }

            if let Expr::Ident(arg) = lhs.arg.as_ref() {
                if let Expr::Lit(Lit::Str(str)) = bin_expr.right.as_ref() {
                    type_string = str;
                    typeof_arg = arg;
                } else {
                    return;
                }
            } else {
                return;
            }
        } else if let Expr::Lit(Lit::Str(str)) = bin_expr.right.as_ref() {
            if let Expr::Unary(lhs) = bin_expr.left.as_ref() {
                if lhs.op != UnaryOp::TypeOf {
                    return;
                }

                if let Expr::Ident(arg) = lhs.arg.as_ref() {
                    type_string = str;
                    typeof_arg = arg;
                } else {
                    return;
                }
            } else {
                return;
            }
        } else {
            return;
        }

        let type_string_pointer = match type_string.value.as_ref() {
            "undefined" => {}
            "object" => {}
            "boolean" => {}
            "number" => {}
            "bigint" => {}
            "string" => {}
            "symbol" => {}
            "function" => {}
            _ => return,
        };

        let typeof_arg_name = Id::new(typeof_arg, &mut self.store.names);
        let typeof_arg_var = self.store.vars.get_index(&typeof_arg_name).unwrap();
        let typeof_arg_var = self
            .store
            .pointers
            .get_index(&Pointer::Var(typeof_arg_var))
            .unwrap();

        if self.store.invalid_pointers.contains(typeof_arg_var) {
            return;
        }

        let Some(typeof_arg_points_to) = self.points_to.get_immutable(typeof_arg_var) else {
            return;
        };

        // TODO: non-strict equivalence (!= and ==).

        if bin_expr.op == BinaryOp::EqEqEq {
            if type_string.value == js_word!("string") {
                if typeof_arg_points_to.len() == 1
                    && typeof_arg_points_to.contains(PointerId::STRING)
                {
                    *expr = Expr::Lit(Lit::Bool(Bool {
                        node_id: self.program_data.new_id_from(expr.node_id()),
                        value: true,
                    }));
                }
            }
        } else if bin_expr.op == BinaryOp::NotEqEq {
        }
    }

    // fn visit_mut_unary_expr(&mut self, node: &mut UnaryExpr) {
    //     node.visit_mut_children_with(self);

    //     if node.op != UnaryOp::TypeOf {
    //         return;
    //     }

    //     let Expr::Ident(rhs) = node.arg.as_ref() else {
    //         return;
    //     };

    //     let name = Id::new(rhs, &mut self.store.names);
    //     let var = self.store.vars.get_index(&name).unwrap();
    //     let var = self.store.pointers.get_index(&Pointer::Var(var)).unwrap();

    //     if self.store.invalid_pointers.contains(var) {
    //         return;
    //     }

    //     let Some(points_to) = self.points_to.get_immutable(var) else {
    //         return;
    //     };

    //     assert!(!points_to.contains(PointerId::UNKNOWN));

    //     for pointer in points_to.iter() {}
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::resolver;
    use global_common::{Globals, Mark, GLOBALS};

    #[test]
    fn test_simple() {
        test_transform(
            "
const foo = 'some string';
typeof foo === 'string';
",
            "
const foo = 'some string';
true;
",
        );
    }

    // TODO: Do we propagate null/undefined accurately enough to deduce that a variable definitely is/isn't null/undefined?

    fn test_transform(input: &str, expected: &str) {
        crate::testing::test_transform(
            |mut program, program_data| {
                GLOBALS.set(&Globals::new(), || {
                    let unresolved_mark = Mark::new();
                    let top_level_mark = Mark::new();

                    program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

                    let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

                    process(&mut program, program_data, unresolved_ctxt);

                    program
                })
            },
            input,
            expected,
        );
    }
    fn test_same(input: &str) {
        test_transform(input, input);
    }
}
