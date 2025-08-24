use ast::*;
use atoms::{js_word, JsWord};
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::{util::take::Take, Span, SyntaxContext, DUMMY_SP};
use rustc_hash::FxHashMap;

use crate::{
    optimize_properties::{analyse, ExprContext, Pointer},
    utils::unwrap_as,
};

pub fn process(
    ast: &mut ast::Program,
    program_data: &mut ProgramData,
    unresolved_ctxt: SyntaxContext,
) {
    let (store, points_to) = analyse(ast, unresolved_ctxt);

    let mut stmt_inline_map: FxHashMap<NodeId, NodeId> = FxHashMap::default();
    let mut expr_inline_map: FxHashMap<NodeId, NodeId> = FxHashMap::default();
    let mut functions_to_collect = FxHashMap::default();

    for (call, callee, ctxt) in &store.calls {
        let callees = points_to.get_immutable(*callee);
        let Some(callees) = callees else {
            continue;
        };
        if callees.len() != 1 {
            continue;
        }
        let callee = callees.iter().next().unwrap();
        if store.invalid_pointers.contains(callee) {
            continue;
        }
        let Pointer::Fn(func) = store.pointers[callee] else {
            continue;
        };

        functions_to_collect.insert(func, *ctxt);

        match ctxt {
            ExprContext::Expression => {
                expr_inline_map.insert(*call, func);
            }
            ExprContext::Statement => {
                stmt_inline_map.insert(*call, func);
            }
        }
    }

    struct BodyCollector<'a> {
        stmt_bodies: FxHashMap<NodeId, Vec<Stmt>>,
        expr_bodies: FxHashMap<NodeId, Expr>,
        program_data: &'a mut ProgramData,
        functions_to_collect: &'a FxHashMap<NodeId, ExprContext>,
        unresolved_ctxt: SyntaxContext,

        references_this: bool,
        references_self: bool,
        current_function_name: Option<JsWord>,
        return_count: usize,
    }

    impl<'ast> Visit<'ast> for BodyCollector<'_> {
        fn visit_function(&mut self, n: &Function) {
            let old_references_this = self.references_this;
            n.visit_children_with(self);
            self.references_this = old_references_this
        }
        fn visit_constructor(&mut self, n: &Constructor) {
            let old_references_this = self.references_this;
            n.visit_children_with(self);
            self.references_this = old_references_this
        }
        fn visit_arrow_expr(&mut self, n: &ArrowExpr) {
            let old_references_this = self.references_this;
            n.visit_children_with(self);
            self.references_this = old_references_this
        }
        fn visit_getter_prop(&mut self, n: &GetterProp) {
            let old_references_this = self.references_this;
            n.visit_children_with(self);
            self.references_this = old_references_this
        }
        fn visit_setter_prop(&mut self, n: &SetterProp) {
            let old_references_this = self.references_this;
            n.visit_children_with(self);
            self.references_this = old_references_this
        }

        fn visit_this_expr(&mut self, _n: &ThisExpr) {
            self.references_this = true;
        }

        fn visit_ident(&mut self, n: &Ident) {
            if Some(&n.sym) == self.current_function_name.as_ref() {
                self.references_self = true;
            }
        }

        fn visit_return_stmt(&mut self, n: &ReturnStmt) {
            n.arg.visit_with(self);

            self.return_count += 1;
        }

        fn visit_fn_decl(&mut self, n: &FnDecl) {
            let old_fn_name = self.current_function_name.clone();
            let old_references_self = self.references_self;
            let old_return_count = self.return_count;

            self.current_function_name = Some(n.ident.sym.clone());
            self.references_self = false;
            self.return_count = 0;

            // Visit function's children directly to bypass the Function visitor
            // above, since that visitor will create a new scope for tracking
            // `this` usage.
            n.function.visit_children_with(self);

            let references_self = self.references_self;
            let return_count = self.return_count;

            self.current_function_name = old_fn_name;
            self.references_self = old_references_self;
            self.return_count = old_return_count;

            if n.function.is_async() || n.function.is_generator() {
                return;
            }

            if self.references_this || references_self {
                return;
            }

            if n.function.params.len() > 0 {
                return;
            }

            if return_count > 1 {
                return;
            }

            let ctxt = match self.functions_to_collect.get(&n.function.node_id) {
                Some(ctxt) => *ctxt,
                None => return,
            };

            if ctxt == ExprContext::Expression {
                if n.function.body.stmts.is_empty() {
                    // Only the implicit return, which is the same as explicitly
                    // returning `undefined`.
                    let body = make_undefined(self.program_data, self.unresolved_ctxt);
                    self.expr_bodies.insert(n.function.node_id, body);
                    return;
                }

                let all_but_last_are_expr = n
                    .function
                    .body
                    .stmts
                    .iter()
                    .rev()
                    .skip(1)
                    .all(|s| matches!(s, Stmt::Expr(_)));

                if all_but_last_are_expr {
                    if let Some(Stmt::Return(last)) = n.function.body.stmts.last() {
                        let tail = match &last.arg {
                            Some(arg) => arg.as_ref().clone_node(self.program_data),
                            None => make_undefined(self.program_data, self.unresolved_ctxt),
                        };

                        let body = convert_statements_to_expressions(
                            &n.function.body.stmts[..n.function.body.stmts.len() - 1],
                            self.program_data,
                            self.program_data.get_span(n.function.body.node_id),
                            tail,
                        );

                        self.expr_bodies.insert(n.function.node_id, body);

                        return;
                    }

                    if let Some(Stmt::Expr(last)) = n.function.body.stmts.last() {
                        // All statements are expressions.

                        let tail = last.expr.as_ref().clone_node(self.program_data);

                        let body = convert_statements_to_expressions(
                            &n.function.body.stmts[..(n.function.body.stmts.len() - 1)],
                            self.program_data,
                            self.program_data.get_span(n.function.body.node_id),
                            tail,
                        );

                        self.expr_bodies.insert(n.function.node_id, body);

                        return;
                    }
                }

                return;
            }

            if ctxt == ExprContext::Statement {
                if n.function.body.stmts.is_empty() {
                    // Only the implicit return, which is the same as explicitly
                    // returning `undefined`.
                    self.stmt_bodies.insert(n.function.node_id, Vec::new());
                    return;
                }

                let mut body = n.function.body.stmts.clone_node(self.program_data);
                remove_return_stmts(&mut body, self.program_data);

                self.stmt_bodies.insert(n.function.node_id, body);

                return;
            }
        }
    }

    let (expr_bodies, stmt_bodies) = {
        let mut v = BodyCollector {
            expr_bodies: FxHashMap::default(),
            stmt_bodies: FxHashMap::default(),
            program_data,
            functions_to_collect: &functions_to_collect,
            unresolved_ctxt,
            references_this: false,
            references_self: false,
            current_function_name: None,
            return_count: 0,
        };
        ast.visit_with(&mut v);
        (v.expr_bodies, v.stmt_bodies)
    };

    struct Inliner<'a> {
        expr_bodies: FxHashMap<NodeId, Expr>,
        stmt_bodies: FxHashMap<NodeId, Vec<Stmt>>,
        program_data: &'a mut ProgramData,
        stmt_inline_map: FxHashMap<NodeId, NodeId>,
        expr_inline_map: FxHashMap<NodeId, NodeId>,
    }

    impl<'ast> VisitMut<'ast> for Inliner<'_> {
        // Handle single-statement contexts.
        fn visit_mut_stmt(&mut self, stmt: &'ast mut Stmt) {
            stmt.visit_mut_children_with(self);

            if let Stmt::Expr(expr_stmt) = stmt {
                if let Some(func) = self.stmt_inline_map.get(&expr_stmt.expr.node_id()) {
                    if let Some(body) = self.stmt_bodies.remove(func) {
                        if body.is_empty() {
                            *stmt = Stmt::Empty(EmptyStmt {
                                node_id: self.program_data.new_id_from(expr_stmt.node_id),
                            });

                            return;
                        }

                        if body.len() == 1 {
                            let first = body.into_iter().next().unwrap();
                            *stmt = first;

                            return;
                        }

                        *stmt = Stmt::Block(BlockStmt {
                            node_id: self.program_data.new_id_from(expr_stmt.node_id),
                            stmts: body,
                        });
                    }
                };
            }
        }

        fn visit_mut_stmts(&mut self, stmts: &'ast mut Vec<Stmt>) {
            let mut i = stmts.len();

            while i > 0 {
                i -= 1;

                let stmt = &mut stmts[i];

                // Skip the single statement visitor above.
                stmt.visit_mut_children_with(self);

                if let Stmt::Expr(expr_stmt) = stmt {
                    if let Some(func) = self.stmt_inline_map.get(&expr_stmt.expr.node_id()) {
                        if let Some(body) = self.stmt_bodies.remove(func) {
                            if body.is_empty() {
                                stmts.remove(i);

                                continue;
                            }

                            if body.len() == 1 {
                                let first = body.into_iter().next().unwrap();
                                *stmt = first;

                                continue;
                            }

                            stmts.splice(i..=i, body.into_iter());
                        }
                    };
                }
            }
        }

        fn visit_mut_expr(&mut self, n: &'ast mut Expr) {
            let node_id = n.node_id();
            if let Some(func) = self.expr_inline_map.get(&node_id) {
                if let Some(body) = self.expr_bodies.remove(func) {
                    *n = body;
                } else {
                    n.visit_mut_children_with(self);
                }
            }
        }
    }

    let mut inliner = Inliner {
        expr_bodies,
        stmt_bodies,
        program_data,
        expr_inline_map,
        stmt_inline_map,
    };
    ast.visit_mut_with(&mut inliner);
}

/// Panics if the statements are not all [`ExprStmt`].
fn convert_statements_to_expressions(
    statements: &[Stmt],
    program_data: &mut ProgramData,
    block_span: Span,
    tail: Expr,
) -> Expr {
    if statements.len() > 0 {
        let mut expr = SeqExpr {
            exprs: Vec::with_capacity(statements.len() + 1),
            node_id: program_data.new_id(block_span),
        };

        expr.exprs.extend(statements.iter().map(|s| {
            unwrap_as!(s, Stmt::Expr(ExprStmt { expr, .. }), expr).clone_node(program_data)
        }));

        expr.exprs.push(Box::new(tail));

        Expr::Seq(expr)
    } else {
        tail
    }
}

fn make_undefined(program_data: &mut ProgramData, unresolved_ctxt: SyntaxContext) -> Expr {
    Expr::Ident(Ident {
        node_id: program_data.new_id(DUMMY_SP),
        sym: js_word!("undefined"),
        ctxt: unresolved_ctxt,
    })
}

fn remove_return_stmts(stmts: &mut [Stmt], program_data: &mut ProgramData) {
    let mut v = ReturnRemover { program_data };

    stmts.iter_mut().for_each(|s| s.visit_mut_with(&mut v));
}

struct ReturnRemover<'a> {
    program_data: &'a mut ProgramData,
}

impl<'ast> VisitMut<'ast> for ReturnRemover<'_> {
    fn visit_mut_stmt(&mut self, n: &mut Stmt) {
        if let Stmt::Return(ret) = n {
            if let Some(arg) = ret.arg.as_mut() {
                *n = Stmt::Expr(ExprStmt {
                    node_id: self.program_data.new_id_from(ret.node_id),
                    expr: arg.take(),
                });
            } else {
                *n = Stmt::Empty(EmptyStmt {
                    node_id: self.program_data.new_id_from(ret.node_id),
                })
            }
        } else {
            n.visit_mut_children_with(self)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::resolver;
    use global_common::{Globals, Mark, GLOBALS};

    #[test]
    fn test_inline_simple_return() {
        test_transform(
            "
function func() {
    return 1;
}
func();
",
            "
function func() {
    return 1;
}
1;
",
        );
    }

    #[test]
    fn test_inline_through_higher_order_function() {
        test_transform(
            "
function func1(f) {
    return f();
}
function func2() {
    return 1;
}
func1(func2);
",
            "
function func1(f) {
    return 1;
}
function func2() {
    return 1;
}
func1(func2);
",
        );

        test_transform(
            "
function func1(f) {
    if (cond) {
        doThingA();
    } else {
        doThingB();
    }
    return f();
}
function func2() {
    return 1;
}
func1(func2);
func1(func2);
",
            "
function func1(f) {
    if (cond) {
        doThingA();
    } else {
        doThingB();
    }
    return 1;
}
function func2() {
    return 1;
}
func1(func2);
func1(func2);
",
        );
    }

    #[test]
    fn test_inline_through_object_property() {
        test_transform(
            "
function func() {
    return 1;
}
const obj = { func };
obj.func()
",
            "
function func() {
    return 1;
}
const obj = { func };
1
",
        );
    }

    #[test]
    fn inline_empty_return() {
        test_transform(
            "
function func() {
    return;
}
func();
",
            "
function func() {
    return;
}
",
        );
    }

    #[test]
    fn inline_non_literal() {
        test_transform(
            "
function func() {
    return { prop: 1 };
}
func();
",
            "
function func() {
    return { prop: 1 };
}
({ prop: 1 });
",
        );

        test_transform(
            "
function func() {
    return someFunc();
}
func();
",
            "
function func() {
    return someFunc();
}
someFunc();
",
        );
    }

    #[test]
    fn inline_implicit_return() {
        test_transform(
            "
function func() {}
func();
",
            "
function func() {}
",
        );

        test_transform(
            "
function func() {}
if (1) func();
",
            "
function func() {}
if (1);
",
        );

        test_transform(
            "
function func() {
    1;
}
func();
",
            "
function func() {
    1;
}
1;
",
        );
    }

    #[test]
    fn test_inline_multiple_statements() {
        test_transform(
            "
function func() {
    if (1);
    return 2;
}
func();
",
            "
function func() {
    if (1);
    return 2;
}
if (1);
2;
",
        );
    }

    #[test]
    fn test_inline_multiple_statements_into_single_statement_context() {
        test_transform(
            "
function func() {
    if (1);
    return 2;
}
if (1) func();
",
            "
function func() {
    if (1);
    return 2;
}
if (1) {
    if (1);
    2;
}
",
        );
    }

    #[test]
    fn test_removes_returns() {
        test_transform(
            "
function func() {
    if (1) return 1;
}
func();
",
            "
function func() {
    if (1) return 1;
}
    if (1) 1;
",
        );
    }

    #[test]
    fn test_inline_expressions_into_expression_context() {
        test_transform(
            "
function func() {
    return 1;
}
const a = func();
",
            "
function func() {
    return 1;
}
const a = 1;
",
        );
    }

    #[test]
    fn test_inline_multiple_expressions_into_expression_context() {
        test_transform(
            "
function func() {
    1;
    return 2;
}
const a = func();
",
            "
function func() {
    1;
    return 2;
}
const a = (1, 2);
",
        );
    }

    #[test]
    fn test_does_not_inline_statements_into_expression_context() {
        test_same(
            "
function func() {
    if (1);
    return 1;
}
const a = func();
",
        );
    }

    #[test]
    fn test_does_not_inline_fn_with_multiple_callees() {
        test_same(
            "
function func() {
    return 1;
}
(func || 1)();
",
        );
    }

    #[test]
    fn test_does_not_inline_fn_with_param() {
        test_same(
            "
function func(a) {
    return 1;
}
func();
",
        );
    }

    #[test]
    fn test_does_not_inline_fn_with_this() {
        test_same(
            "
function func() {
    this;
}
func();
",
        );

        test_same(
            "
function func() {
    return this;
}
func();
",
        );
    }

    #[test]
    fn test_does_not_inline_self_referential_fn() {
        test_same(
            "
function func() {
    func;
}
func();
",
        );

        test_same(
            "
function func() {
    return func;
}
func();
",
        );
    }

    #[test]
    fn test_does_not_inline_async_function() {
        test_same(
            "
async function func() {
    return 1;
}
func();
",
        );
    }

    #[test]
    fn test_does_not_inline_generator_function() {
        test_same(
            "
function* func() {
    return 1;
}
func();
",
        );
    }

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
