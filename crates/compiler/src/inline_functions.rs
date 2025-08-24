use ast::*;
use atoms::{js_word, JsWord};
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::{SyntaxContext, DUMMY_SP};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::optimize_properties::{analyse, Pointer};

pub fn process(
    ast: &mut ast::Program,
    program_data: &mut ProgramData,
    unresolved_ctxt: SyntaxContext,
) {
    let (store, points_to) = analyse(ast, unresolved_ctxt);

    let mut inline_map: FxHashMap<NodeId, NodeId> = FxHashMap::default();
    let mut functions_to_collect = FxHashSet::default();

    for (call, callee) in &store.calls {
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

        functions_to_collect.insert(func);

        inline_map.insert(*call, func);
    }

    struct BodyCollector<'a> {
        bodies: FxHashMap<NodeId, Option<Expr>>,
        program_data: &'a mut ProgramData,
        functions_to_collect: &'a FxHashSet<NodeId>,
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

            if !self.functions_to_collect.contains(&n.function.node_id) {
                return;
            }

            if n.function.params.len() > 0 {
                return;
            }

            if n.function.body.stmts.is_empty() {
                // Only the implicit return, which is the same as explicitly
                // returning `undefined`.
                self.bodies.insert(n.function.node_id, None);
                return;
            }

            if return_count == 1 {
                if let [Stmt::Return(r)] = n.function.body.stmts.as_slice() {
                    let arg = r
                        .arg
                        .as_ref()
                        .map(|a| a.as_ref().clone_node(self.program_data));
                    self.bodies.insert(n.function.node_id, arg);
                }
            }
        }
    }

    let bodies = {
        let mut v = BodyCollector {
            bodies: FxHashMap::default(),
            program_data,
            functions_to_collect: &functions_to_collect,
            references_this: false,
            references_self: false,
            current_function_name: None,
            return_count: 0,
        };
        ast.visit_with(&mut v);
        v.bodies
    };

    struct Inliner<'a> {
        bodies: FxHashMap<NodeId, Option<Expr>>,
        program_data: &'a mut ProgramData,
        inline_map: FxHashMap<NodeId, NodeId>,
        unresolved_ctxt: SyntaxContext,
    }

    impl<'ast> VisitMut<'ast> for Inliner<'_> {
        fn visit_mut_expr(&mut self, n: &'ast mut Expr) {
            let node_id = n.node_id();
            if let Some(func) = self.inline_map.get(&node_id) {
                if let Some(body) = self.bodies.remove(func) {
                    let body = match body {
                        Some(expr) => expr,
                        None => Expr::Ident(Ident {
                            node_id: self.program_data.new_id(DUMMY_SP),
                            sym: js_word!("undefined"),
                            ctxt: self.unresolved_ctxt,
                        }),
                    };
                    *n = body;
                } else {
                    n.visit_mut_children_with(self);
                }
            }
        }
    }

    let mut inliner = Inliner {
        bodies,
        program_data,
        inline_map,
        unresolved_ctxt,
    };
    ast.visit_mut_with(&mut inliner);
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
undefined;
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
undefined;
",
        );
    }

    #[test]
    fn test_does_not_inline_fn_with_that_is_not_simple_return() {
        test_same(
            "
function func() {
    1;
}
func();
",
        );

        test_same(
            "
function func() {
    1;
    return 2;
}
func();
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
