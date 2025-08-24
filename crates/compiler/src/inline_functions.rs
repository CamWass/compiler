use ast::*;
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::SyntaxContext;
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
        bodies: FxHashMap<NodeId, Lit>,
        program_data: &'a mut ProgramData,
        functions_to_collect: &'a FxHashSet<NodeId>,
    }

    impl<'ast> Visit<'ast> for BodyCollector<'_> {
        fn visit_fn_decl(&mut self, n: &'ast FnDecl) {
            n.visit_children_with(self);

            if n.function.is_async() || n.function.is_generator() {
                return;
            }

            if !self.functions_to_collect.contains(&n.function.node_id) {
                return;
            }

            if n.function.params.len() > 0 {
                return;
            }

            if let [Stmt::Return(r)] = n.function.body.stmts.as_slice() {
                if let Some(Expr::Lit(lit)) = r.arg.as_deref() {
                    self.bodies
                        .insert(n.function.node_id, lit.clone_node(self.program_data));
                }
            }
        }
    }

    let bodies = {
        let mut v = BodyCollector {
            bodies: FxHashMap::default(),
            program_data,
            functions_to_collect: &functions_to_collect,
        };
        ast.visit_with(&mut v);
        v.bodies
    };

    struct Inliner<'a> {
        bodies: FxHashMap<NodeId, Lit>,
        program_data: &'a mut ProgramData,
        inline_map: FxHashMap<NodeId, NodeId>,
    }

    impl<'ast> VisitMut<'ast> for Inliner<'_> {
        fn visit_mut_expr(&mut self, n: &'ast mut Expr) {
            let node_id = n.node_id();
            if let Some(func) = self.inline_map.get(&node_id) {
                if let Some(body) = self.bodies.get(func) {
                    *n = Expr::Lit(body.clone_node(self.program_data));
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
    fn test_does_not_inline_fn_with_that_is_not_simple_return() {
        test_same(
            "
function func() {}
func();
",
        );

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
