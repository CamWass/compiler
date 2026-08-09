use ast::*;
use common::{SyntaxContext, util::take::Take};
use visit::{VisitMut, VisitMutWith};

use crate::{
    node_util::{expr_may_have_side_effects, get_boolean_value},
    utils::unwrap_as,
};

pub fn process(ast: &mut Program, program_data: &mut ProgramData, unresolved_ctxt: SyntaxContext) {
    let mut visitor = Visitor {
        program_data,
        unresolved_ctxt,
    };
    ast.visit_mut_with(&mut visitor);
}

struct Visitor<'a> {
    program_data: &'a mut ProgramData,
    unresolved_ctxt: SyntaxContext,
}

impl VisitMut<'_> for Visitor<'_> {
    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        let mut i = 0;
        while i < stmts.len() {
            stmts[i].visit_mut_with(self);

            // Inline block contents into parent statement list.
            if matches!(&stmts[i], Stmt::Block(_)) {
                let stmt = stmts.remove(i);
                let block = unwrap_as!(stmt, Stmt::Block(b), b);
                let num_stmts = block.stmts.len();
                // Insert the new statements at the index of the old one to preserve ordering.
                stmts.splice(i..i, block.stmts);

                if num_stmts > 0 {
                    // Skip over the new stmts.
                    i += num_stmts - 1;
                    continue;
                }
            }

            i += 1;
        }
    }

    fn visit_mut_module_items(&mut self, items: &mut Vec<ModuleItem>) {
        let mut i = 0;
        while i < items.len() {
            items[i].visit_mut_with(self);

            // Inline block contents into parent statement list.
            if matches!(&items[i], ModuleItem::Stmt(Stmt::Block(_))) {
                let stmt = items.remove(i);
                let block = unwrap_as!(stmt, ModuleItem::Stmt(Stmt::Block(b)), b);
                let num_stmts = block.stmts.len();
                // Insert the new statements at the index of the old one to preserve ordering.
                items.splice(i..i, block.stmts.into_iter().map(ModuleItem::Stmt));

                if num_stmts > 0 {
                    // Skip over the new stmts.
                    i += num_stmts - 1;
                    continue;
                }
            }

            i += 1;
        }
    }

    fn visit_mut_expr(&mut self, node: &mut Expr) {
        match node {
            Expr::Assign(expr) => {
                expr.visit_mut_children_with(self);

                if expr.op == AssignOp::Assign {
                    let left_ident = match &expr.left {
                        PatOrExpr::Expr(left) => match left.as_ref() {
                            Expr::Ident(left) => Some(left),
                            _ => None,
                        },
                        PatOrExpr::Pat(left) => match left.as_ref() {
                            Pat::Ident(left) => Some(&left.id),
                            _ => None,
                        },
                    };

                    let right_ident = match expr.right.as_ref() {
                        Expr::Ident(right) => Some(right),
                        _ => None,
                    };

                    if let Some(left) = left_ident
                        && let Some(right) = right_ident
                    {
                        if left.sym == right.sym && left.ctxt == right.ctxt {
                            // Identity assignment e.g. `a = a`.
                            *node = expr.right.as_mut().take();
                            return;
                        }
                    }

                    let is_left_empty_destructuring = match &expr.left {
                        PatOrExpr::Pat(left) => match left.as_ref() {
                            Pat::Array(left) => left.elems.is_empty(),
                            Pat::Object(left) => left.props.is_empty(),
                            _ => false,
                        },
                        PatOrExpr::Expr(_) => false,
                    };

                    if is_left_empty_destructuring {
                        // `[] = RHS` or `{} = RHS` become just `RHS`.
                        // Note: this can potentially change the program's
                        // observable behaviour - if `RHS` is not iterable, then
                        // `[] = RHS` will throw but `RHS` will not.
                        *node = expr.right.as_mut().take();
                        return;
                    }
                }
            }
            Expr::Cond(cond) => {
                cond.visit_mut_children_with(self);

                let condition_value = get_boolean_value(&cond.test, self.unresolved_ctxt);

                if condition_value.is_none() {
                    // If the result nodes are equivalent, then one of the nodes can be
                    // removed and it doesn't matter which.
                    if !cond.cons.eq_ignoring_node_id(&cond.alt) {
                        // We can't remove branches otherwise!

                        return;
                    }
                }

                let cond = unwrap_as!(node.take(), Expr::Cond(c), c);

                // Transform "(a = 2) ? x =2 : y" into "a=2,x=2"
                let branchToKeep = if condition_value == Some(true) {
                    cond.cons
                } else {
                    cond.alt
                };

                let condition_has_side_effects =
                    expr_may_have_side_effects(&cond.test, self.unresolved_ctxt);
                let replacement = if condition_has_side_effects {
                    Expr::Seq(SeqExpr {
                        node_id: self.program_data.new_id_from(cond.node_id),
                        exprs: vec![cond.test, branchToKeep],
                    })
                } else {
                    *branchToKeep
                };

                *node = replacement;
            }
            _ => node.visit_mut_children_with(self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::resolver;
    use common::{GLOBALS, Globals, Mark};

    #[test]
    fn test_block() {
        test_transform("{{foo()}}", "foo()");
        test_transform("{foo();{}}", "foo()");
        test_transform("{{foo()}{}}", "foo()");
        test_transform("{{foo()}{bar()}}", "foo();bar()");

        test_transform("{let x}", "let x");
        test_same("function f() {let x}");
        test_transform("{const x = 1}", "const x = 1;");
        test_transform("{x = 2; y = 4; let z;}", "x = 2; y = 4; let z;");
        test_transform("{'hi'; let x;}", "'hi'; let x;");
        test_transform("{x = 4; {let y}}", "x = 4; let y;");
        test_transform("{class C {}} {class C {}}", "class C {} class C {}");
        test_transform("{label: var x}", "label: var x");
        // `{label: let x}` is a syntax error
        test_transform("{label: var x; let y;}", "label: var x; let y;");
    }

    #[test]
    fn test_block_with_many_children() {
        test_transform(
            "{var x; var y; var z; class Foo { constructor() { var a; { var b; } } } }",
            "var x;var y;var z;class Foo { constructor() { var a;var b} }",
        );
        test_transform(
            "{var x; var y; var z; { { var a; { var b; } } } }",
            "var x;var y;var z; var a;var b",
        );
    }

    //       #[test]
    //   fn  testRemoveNoOpLabelledStatement() {
    //     test_transform("a: break a;", "");
    //     test_transform("a: { break a; }", "");

    //     test_transform( //
    //         "a: { break a; console.log('unreachable'); }", //
    //         "");
    //     test_transform( //
    //         "a: { break a; var x = 1; } x = 2;", //
    //         "var x; x = 2;");

    //     test_same("b: { var x = 1; } x = 2;");
    //     test_same("a: b: { var x = 1; } x = 2;");
    //   }

    //   #[test]
    //   fn  testRemoveUselessLabelWithFollowingBreak() {
    //     test_transform("a:b: break b;", "");
    //     // Note: the break is only removed if the parent
    //     // is the break target.
    //     test_same("a:b: break a;");
    //   }

    //   #[test]
    //   fn  testFoldBlock() {
    //     test_transform("{{foo()}}", "foo()");
    //     test_transform("{foo();{}}", "foo()");
    //     test_transform("{{foo()}{}}", "foo()");
    //     test_transform("{{foo()}{bar()}}", "foo();bar()");
    //     test_transform("{if(false)foo(); {bar()}}", "bar()");
    //     test_transform("{if(false)if(false)if(false)foo(); {bar()}}", "bar()");

    //     test_transform("{'hi'}", "");
    //     test_transform("{x==3}", "");
    //     test_transform("{`hello ${foo}`}", "");
    //     test_transform("{ (function(){x++}) }", "");
    //     test_same("function f(){return;}");
    //     test_transform("function f(){return 3;}", "function f(){return 3}");
    //     test_same("function f(){if(x)return; x=3; return; }");
    //     test_transform("{x=3;;;y=2;;;}", "x=3;y=2");

    //     // Cases to test for empty block.
    //     test_transform("while(x()){x}", "while(x());");
    //     test_transform("while(x()){x()}", "while(x())x()");
    //     test_transform("for(x=0;x<100;x++){x}", "for(x=0;x<100;x++);");
    //     test_transform("for(x in y){x}", "for(x in y);");
    //     test_transform("for (x of y) {x}", "for(x of y);");
    //     test_same("for (let x = 1; x <10; x++ ) {}");
    //     test_same("for (var x = 1; x <10; x++ ) {}");
    //   }

    //   #[test]
    //   fn  testFoldBlockWithDeclaration_notNormalized() {
    //     disableNormalize();
    //     disableComputeSideEffects();

    //     test_same("{let x}");
    //     test_same("function f() {let x}");
    //     test_same("{const x = 1}");
    //     test_same("{x = 2; y = 4; let z;}");
    //     test_transform("{'hi'; let x;}", "{let x}");
    //     test_transform("{x = 4; {let y}}", "x = 4; {let y}");
    //     test_same("{class C {}} {class C {}}");
    //     test_transform("{label: var x}", "label: var x");
    //     // `{label: let x}` is a syntax error
    //     test_same("{label: var x; let y;}");
    //   }

    //   #[test]
    //   fn  testFoldBlockWithDeclaration_normalized() {
    //     test_transform("{let x}", "let x");
    //     test_same("function f() {let x}");
    //     test_transform("{const x = 1}", "const x = 1;");
    //     test_transform("{x = 2; y = 4; let z;}", "x = 2; y = 4; let z;");
    //     test_transform("{'hi'; let x;}", "let x;");
    //     test_transform("{x = 4; {let y}}", "x = 4; let y;");
    //     test_transform("{class C {}} {class C {}}", "class C {} class C$jscomp$1 {}");
    //     test_transform("{label: var x}", "label: var x");
    //     // `{label: let x}` is a syntax error
    //     test_transform("{label: var x; let y;}", "label: var x; let y;");
    //   }

    //   /** Try to remove spurious blocks with multiple children */
    //   #[test]
    //   fn  testFoldBlocksWithManyChildren() {
    //     test_transform("function f() { if (false) {} }", "function f(){}");
    //     test_transform("function f() { { if (false) {} if (true) {} {} } }", "function f(){}");
    //     test_transform(
    //         "{var x; var y; var z; class Foo { constructor() { var a; { var b; } } } }",
    //         "var x;var y;var z;class Foo { constructor() { var a;var b} }");
    //     test_transform("{var x; var y; var z; { { var a; { var b; } } } }", "var x;var y;var z; var a;var b");
    //   }

    //   #[test]
    //   fn  testIf() {
    //     test_transform("if (1){ x=1; } else { x = 2;}", "x=1");
    //     test_transform("if (false){ x = 1; } else { x = 2; }", "x=2");
    //     test_transform("if (undefined){ x = 1; } else { x = 2; }", "x=2");
    //     test_transform("if (null){ x = 1; } else { x = 2; }", "x=2");
    //     test_transform("if (void 0){ x = 1; } else { x = 2; }", "x=2");
    //     test_transform("if (void foo()){ x = 1; } else { x = 2; }", "foo();x=2");
    //     test_transform("if (false){ x = 1; } else if (true) { x = 3; } else { x = 2; }", "x=3");
    //     test_transform("if (x){ x = 1; } else if (false) { x = 3; }", "if(x)x=1");
    //   }

    #[test]
    fn test_conditional_expression() {
        test_transform("true ? a() : b()", "a()");
        test_transform("false ? a() : b()", "b()");

        // test_transform("a() ? b() : true", "a() && b()");
        // test_transform("a() ? true : b()", "a() || b()");

        test_transform("(a = true) ? b() : c()", "a = true, b()");
        test_transform("(a = false) ? b() : c()", "a = false, c()");
        test_transform(
            "do {f()} while((a = true) ? b() : c())",
            "do {f()} while((a = true) , b())",
        );
        test_transform(
            "do {f()} while((a = false) ? b() : c())",
            "do {f()} while((a = false) , c())",
        );

        test_transform("var x = (true) ? 1 : 0", "var x=1");
        test_transform(
            "var y = (true) ? ((false) ? 12 : (cond ? 1 : 2)) : 13",
            "var y=cond?1:2",
        );

        test_same("var z=x?void 0:y()");
        test_same("z=x?void 0:y()");
        test_same("z*=x?void 0:y()");

        test_same("var z=x?y():void 0");
        test_same("(w?x:void 0).y=z");
        test_same("(w?x:void 0).y+=z");

        test_transform("y = (x ? void 0 : void 0)", "y = void 0");
        test_transform("y = (x ? f() : f())", "y = f()");
        // test_transform("(function(){}) ? function(){} : function(){}", "");

        // test_transform("1 ? 2 : 3", "");

        // test_transform("x ? a() : 3", "x && a()");

        // test_transform("x ? 2 : a()", "x || a()");

        test_same("x ? a() : b()");

        // test_transform("a() ? 1 : 2", "a()");

        // test_transform("a() ? b() : 2", "a() && b()");

        // test_transform("a() ? 1 : b()", "a() || b()");

        test_same("a() ? b() : c()");

        test_transform("true ? a() : (function f() {})()", "a()");
        test_transform(
            "false ? a() : (function f() {alert(x)})()",
            "(function f() {alert(x)})()",
        );
        test_transform("((function () {}), true) ? a() : b()", "a()");
        // test_transform(
        //     "((function () {alert(x)})(), true) ? a() : b()",
        //     "(function(){alert(x)})(),a()",
        // );
    }

    //   #[test]
    //   fn  testConstantConditionWithSideEffect1() {
    //     test_transform("if (b=true) x=1;", "b=true;x=1");
    //     test_transform("if (b=/ab/) x=1;", "b=/ab/;x=1");
    //     test_transform("if (b=/ab/){ x=1; } else { x=2; }", "b=/ab/;x=1");
    //     test_transform("var b;b=/ab/;if(b)x=1;", "var b;b=/ab/;x=1");
    //     test_same("var b;b=f();if(b)x=1;");
    //     test_transform("var b=/ab/;if(b)x=1;", "var b=/ab/;x=1");
    //     test_same("var b=f();if(b)x=1;");
    //     test_same("b=b++;if(b)x=b;");
    //     test_transform("(b=0,b=1);if(b)x=b;", "b=0,b=1;if(b)x=b;");
    //     test_transform("b=1;if(foo,b)x=b;", "b=1;x=b;");
    //     test_same("b=1;if(foo=1,b)x=b;");
    //   }

    //   #[test]
    //   fn  testConstantConditionWithSideEffect2() {
    //     test_transform("(b=true)?x=1:x=2;", "b=true,x=1");
    //     test_transform("(b=false)?x=1:x=2;", "b=false,x=2");
    //     test_transform("if (b=/ab/) x=1;", "b=/ab/;x=1");
    //     test_transform("var b;b=/ab/;(b)?x=1:x=2;", "var b;b=/ab/;x=1");
    //     test_same("var b;b=f();(b)?x=1:x=2;");
    //     test_transform("var b=/ab/;(b)?x=1:x=2;", "var b=/ab/;x=1");
    //     test_same("var b=f();(b)?x=1:x=2;");
    //   }

    //   #[test]
    //   fn  testConstantConditionWithSideEffect_coalesce() {
    //     test_transform("b = null; b ?? (x = 1)", "b = null; void 0 ?? (x = 1)");
    //     test_transform("b = undefined; b ?? (x = 1)", "b = undefined; void 0 ?? (x = 1)");
    //     test_transform("b = (fn(), null); b ?? (x = 1)", "b = (fn(), null); void 0 ?? (x = 1)");

    //     test_transform("b = 34; b ?? (x = 1)", "b = 34; 0 ?? (x = 1)");
    //     test_transform("b = 'test'; b ?? (x = 1)", "b = 'test'; 0 ?? (x = 1)");
    //     test_transform("b = []; b ?? (x = 1)", "b = []; 0 ?? (x = 1)");
    //     test_transform("b = (fn(), 0); b ?? (x = 1)", " b= (fn(), 0); 0 ?? (x = 1)");

    //     test_same("b = fn(); b ?? (x = 1)");
    //   }

    //   #[test]
    //   fn  testVarLifting() {
    //     test_transform("if(true)var a", "var a");
    //     test_transform("if(false)var a", "var a");

    //     // More var lifting tests in PeepholeIntegrationTests
    //   }

    //   #[test]
    //   fn  testLetConstLifting() {
    //     test_transform("if(true) {const x = 1}", "const x = 1;");
    //     test_transform("if(false) {const x = 1}", "");
    //     test_transform("if(true) {let x}", "let x;");
    //     test_transform("if(false) {let x}", "");
    //     test_transform("if(false) {const x = 1;  function f() { return x; }}", "");
    //   }

    //   #[test]
    //   fn  testLetConstLifting_removePartOfBlock() {
    //     test_transform(
    //         """
    //         function f() {
    //           return 0;
    //           let x = 0;
    //           x++;
    //         }
    //         """,
    //         """
    //         function f() {
    //           let x;
    //           return 0;
    //         }
    //         """);
    //     test_transform(
    //         """
    //         function f() {
    //           return 0;
    //           const [x, y, [[z]]] = 1;
    //         }
    //         """,
    //         """
    //         function f() {
    //           let x;
    //           let y;
    //           let z;
    //           return 0;
    //         }
    //         """);
    //     test_transform(
    //         """
    //         function f() {
    //           return 0;
    //           const C = class Bar {};
    //         }
    //         """,
    //         """
    //         function f() {
    //           let C;
    //           return 0;
    //         }
    //         """);
    //   }

    //   #[test]
    //   fn  testLetConstLifting_removePartOfBlock_withHoistedFunction() {
    //     test_transform(
    //         """
    //         function f() {
    //           return 0;
    //           // Everything after this is dead code, except for the function declaration, which is
    //           // hoisted. `foo` could in theory reference x, y, or C. Add a stub 'let' declaration for
    //           // each to avoid violating the invariant that all NAME nodes in the AST are declared.
    //           const x = 1;
    //           let y;
    //           class C {}
    //           function foo() {}
    //         }
    //         """,
    //         """
    //         function f() {
    //           function foo() {}
    //           let C;
    //           let y;
    //           let x;
    //           return 0;
    //         }
    //         """);
    //     test_transform(
    //         """
    //         function f(param) {
    //           return 0;
    //           // everything after this is dead code.
    //           if (param) {
    //             function foo() {}
    //             const x = 1;
    //             let y;
    //             class C {}
    //           }
    //         }
    //         """,
    //         """
    //         function f(param) {
    //           return 0;
    //         }
    //         """);
    //     test_transform(
    //         """
    //         function f(param) {
    //           if (param) {
    //             return 0;
    //             const x = 1;
    //           }
    //           return 1;
    //         }
    //         """,
    //         """
    //         function f(param) {
    //           if (param) {
    //             let x;
    //             return 0;
    //           }
    //           return 1;
    //         }
    //         """);
    //   }

    //   #[test]
    //   fn  testFoldUselessFor() {
    //     test_transform("for(;false;) { foo() }", "");
    //     test_transform("for(;void 0;) { foo() }", "");
    //     test_transform("for(;undefined;) { foo() }", "");
    //     test_transform("for(;true;) foo() ", "for(;;) foo() ");
    //     test_same("for(;;) foo()");
    //     test_transform("for(;false;) { var a = 0; }", "var a");
    //     test_transform("for(;false;) { const a = 0; }", "");
    //     test_transform("for(;false;) { let a = 0; }", "");

    //     // Make sure it plays nice with minimizing
    //     test_transform("for(;false;) { foo(); continue }", "");

    //     test_transform("l1:for(;false;) {  }", "");
    //   }

    //   #[test]
    //   fn  testFoldUselessDo() {
    //     test_transform("do { foo() } while(false);", "foo()");
    //     test_transform("do { foo() } while(void 0);", "foo()");
    //     test_transform("do { foo() } while(undefined);", "foo()");
    //     test_same("do { foo() } while(true);");
    //     test_transform("do { var a = 0; } while(false);", "var a=0");

    //     test_transform("do { var a = 0; } while(!{a:foo()});", "var a=0;foo()");

    //     // Can't fold with break or continues.
    //     test_same("do { foo(); continue; } while(0)");
    //     test_same("do { try { foo() } catch (e) { break; } } while (0);");
    //     test_same("do { foo(); break; } while(0)");
    //     test_transform("do { for (;;) {foo(); continue;} } while(0)", "for (;;) {foo(); continue;}");
    //     test_same("l1: do { for (;;) { foo() } } while(0)");
    //     test_transform("do { switch (1) { default: foo(); break} } while(0)", "foo();");
    //     test_transform(
    //         "do { switch (1) { default: foo(); continue} } while(0)",
    //         "do { foo(); continue } while(0)");

    //     test_transform(
    //         "l1: { do { x = 1; break l1; } while (0); x = 2; }", //
    //         "l1: { x = 1; break l1; }");

    //     test_transform("do { x = 1; } while (x = 0);", "x = 1; x = 0;");
    //     test_transform(
    //         "let x = 1; (function() { do { let x = 2; } while (x = 10, false); })();",
    //         "let x = 1; (function() { let x$jscomp$1 = 2; x = 10 })();");
    //   }

    //   #[test]
    //   fn  testFoldEmptyDo() {
    //     test_transform("do { } while(true);", "for (;;);");
    //   }

    //   #[test]
    //   fn  testMinimizeLoop_withConstantCondition_vanillaFor() {
    //     test_transform("for(;true;) foo()", "for(;;) foo()");
    //     test_transform("for(;0;) foo()", "");
    //     test_transform("for(;0.0;) foo()", "");
    //     test_transform("for(;NaN;) foo()", "");
    //     test_transform("for(;null;) foo()", "");
    //     test_transform("for(;undefined;) foo()", "");
    //     test_transform("for(;'';) foo()", "");
    //   }

    //   #[test]
    //   fn  testMinimizeLoop_withConstantCondition_doWhile() {
    //     test_transform("do { foo(); } while (true)", "do { foo(); } while (true);");
    //     test_transform("do { foo(); } while (0)", "foo();");
    //     test_transform("do { foo(); } while (0.0)", "foo();");
    //     test_transform("do { foo(); } while (NaN)", "foo();");
    //     test_transform("do { foo(); } while (null)", "foo();");
    //     test_transform("do { foo(); } while (undefined)", "foo();");
    //     test_transform("do { foo(); } while ('')", "foo();");
    //   }

    //   #[test]
    //   fn  testFoldConstantCommaExpressions() {
    //     test_transform("if (true, false) {foo()}", "");
    //     test_transform("if (false, true) {foo()}", "foo()");
    //     test_transform("true, foo()", "foo()");
    //     test_transform("true, foo?.()", "foo?.()");
    //     test_transform("(1 + 2 + ''), foo()", "foo()");
    //     test_transform("(1 + 2 + ''), foo?.()", "foo?.()");
    //   }

    #[test]
    fn testRemoveUselessOps1() {
        test_same("(function () { f(); })();");
    }

    //   #[test]
    //   fn  testCallSideEffectsPreserved() {
    //     // Functions calls known to be free of side effects are removed.
    //     test_transform("Math.random()", "");
    //     test_transform("Math?.random()", "");
    //     test_transform("Math.random(f() + g())", "f(),g();");
    //     test_transform("Math?.random(f() + g())", "f(),g();");
    //     test_transform("Math.random(f(),g(),h())", "f(),g(),h();");
    //     test_transform("Math?.random(f(),g(),h())", "f(),g(),h();");

    //     // Calls to functions with unknown side-effects are preserved.
    //     test_same("f();");
    //     test_same("f?.();");
    //     test_same("(function () { f(); })();");
    //   }

    //   #[test]
    //   fn  testRemoveUselessOps2() {
    //     // There are four place where expression results are discarded:
    //     //  - a top-level expression EXPR_RESULT
    //     //  - the LHS of a COMMA
    //     //  - the FOR init expression
    //     //  - the FOR increment expression

    //     // We know that this function has no side effects because of the
    //     // PureFunctionIdentifier.
    //     test_transform("(function () {})();", "");

    //     // Uncalled function expressions are removed
    //     test_transform("(function () {});", "");
    //     test_transform("(function f() {});", "");
    //     test_transform("(function* f() {})", "");
    //     // ... including any code they contain.
    //     test_transform("(function () {foo();});", "");

    //     // Useless operators are removed.
    //     test_transform("+f()", "f()");
    //     test_transform("+f?.()", "f?.()");
    //     test_transform("a=(+f(),g())", "a=(f(),g())");
    //     test_transform("a=(+f?.(),g())", "a=(f?.(),g())");
    //     test_transform("a=(true,g())", "a=g()");
    //     test_transform("f(),true", "f()");
    //     test_transform("f() + g()", "f(),g()");

    //     test_transform("for(;;+f()){}", "for(;;f()){}");
    //     test_transform("for(+f();;g()){}", "for(f();;g()){}");
    //     test_transform("for(;;Math.random(f(),g(),h())){}", "for(;;f(),g(),h()){}");

    //     // The optimization cascades into conditional expressions:
    //     test_transform("g() && +f()", "g() && f()");
    //     test_transform("g() || +f()", "g() || f()");
    //     test_transform("x ? g() : +f()", "x ? g() : f()");

    //     test_transform("+x()", "x()");
    //     test_transform("+x() * 2", "x()");
    //     test_transform("-(+x() * 2)", "x()");
    //     test_transform("2 -(+x() * 2)", "x()");
    //     test_transform("x().foo", "x()");
    //     test_same("x().foo()");

    //     test_same("x++");
    //     test_same("++x");
    //     test_same("x--");
    //     test_same("--x");
    //     test_same("x = 2");
    //     test_same("x *= 2");

    //     // Sanity check, other expression are left alone.
    //     test_same("function f() {}");
    //     test_same("var x;");
    //   }

    //   #[test]
    //   fn  testOptimizeSwitch() {
    //     test_transform("switch(a){}", "");
    //     test_transform("switch(foo()){}", "foo()");
    //     test_transform("switch(a){default:}", "");
    //     test_transform("switch(a){default:break;}", "");
    //     test_transform("switch(a){default:var b;break;}", "var b");
    //     test_transform("switch(a){case 1: default:}", "");
    //     test_transform("switch(a){default: case 1:}", "");
    //     test_transform("switch(a){default: break; case 1:break;}", "");
    //     test_transform("switch(a){default: var b; break; case 1: var c; break;}", "var c; var b;");
    //     test_transform("var x=1; switch(x) { case 1: var y; }", "var y; var x=1;");

    //     // Can't remove cases if a default exists and is not the last case.
    //     test_same("function f() {switch(a){default: return; case 1: break;}}");
    //     test_same("function f() {switch(1){default: return; case 1: break;}}"); // foldable
    //     test_same("function f() {switch(a){case 1: foo();}}");
    //     test_same("function f() {switch(a){case 3: case 2: case 1: foo();}}");

    //     test_transform("function f() {switch(a){case 2: case 1: default: foo();}}", "function f() { foo(); }");
    //     test_transform("switch(a){case 1: default:break; case 2: foo()}", "switch(a){case 2: foo()}");
    //     test_same("switch(a){case 1: goo(); default:break; case 2: foo()}");

    //     // TODO: optimise the useless "case 2"
    //     test_same("switch(a){case 1: goo(); case 2:break; case 3: foo()}");

    //     // Can't remove unused code with a "var" in it.
    //     test_transform("switch(1){case 2: var x=0;}", "var x;");
    //     test_transform(
    //         """
    //         switch ('repeated') {
    //         case 'repeated':
    //           foo();
    //           break;
    //         case 'repeated':
    //           var x=0;
    //           break;
    //         }
    //         """,
    //         "var x; foo();");

    //     // Can't remove cases if something useful is done.
    //     test_same("switch(a){case 1: var c =2; break;}");
    //     test_same("function f() {switch(a){case 1: return;}}");
    //     test_same("x:switch(a){case 1: break x;}");

    //     test_transform(
    //         """
    //         switch ('foo') {
    //         case 'foo':
    //           foo();
    //           break;
    //         case 'bar':
    //           bar();
    //           break;
    //         }
    //         """,
    //         "foo();");
    //     test_transform(
    //         """
    //         switch ('noMatch') {
    //         case 'foo':
    //           foo();
    //           break;
    //         case 'bar':
    //           bar();
    //           break;
    //         }
    //         """,
    //         "");
    //     test_transform(
    //         """
    //         switch ('fallThru') {
    //         case 'fallThru':
    //           if (foo(123) > 0) {
    //             foobar(1);
    //             break;
    //           }
    //           foobar(2);
    //         case 'bar':
    //           bar();
    //         }
    //         """,
    //         """
    //         switch ('fallThru') {
    //         case 'fallThru':
    //           if (foo(123) > 0) {
    //             foobar(1);
    //             break;
    //           }
    //           foobar(2);
    //           bar();
    //         }
    //         """);
    //     test_transform(
    //         """
    //         switch ('fallThru') {
    //         case 'fallThru':
    //           foo();
    //         case 'bar':
    //           bar();
    //         }
    //         """,
    //         """
    //         foo();
    //         bar();
    //         """);
    //     test_transform(
    //         """
    //         switch ('hasDefaultCase') {
    //           case 'foo':
    //             foo();
    //             break;
    //           default:
    //             bar();
    //             break;
    //         }
    //         """,
    //         "bar();");
    //     test_transform(
    //         """
    //         switch ('repeated') {
    //         case 'repeated':
    //           foo();
    //           break;
    //         case 'repeated':
    //           bar();
    //           break;
    //         }
    //         """,
    //         "foo();");
    //     test_same(
    //         """
    //         switch ('foo') {
    //         case 'bar':
    //           bar();
    //           break;
    //         case notConstant:
    //           foobar();
    //           break;
    //         case 'foo':
    //           foo();
    //           break;
    //         }
    //         """);
    //     test_transform(
    //         """
    //         switch (1) {
    //         case 1:
    //           foo();
    //           break;
    //         case 2:
    //           bar();
    //           break;
    //         }
    //         """,
    //         "foo();");
    //     test_transform(
    //         """
    //         switch (1) {
    //         case 1.1:
    //           foo();
    //           break;
    //         case 2:
    //           bar();
    //           break;
    //         }
    //         """,
    //         "");
    //     test_transform(
    //         """
    //         switch (0) {
    //         case NaN:
    //           foobar();
    //           break;
    //         case -0.0:
    //           foo();
    //           break;
    //         case 2:
    //           bar();
    //           break;
    //         }
    //         """,
    //         "foo();");
    //     test_same(
    //         """
    //         switch ('\\v') {
    //         case '\\u000B':
    //           foo();
    //         }
    //         """);
    //     test_transform(
    //         """
    //         switch ('empty') {
    //         case 'empty':
    //         case 'foo':
    //           foo();
    //         }
    //         """,
    //         "foo()");

    //     test_transform(
    //         """
    //         let x;
    //         switch (use(x)) {
    //           default: {let y;}
    //         }
    //         """,
    //         """
    //         let x;
    //         use(x);
    //         let y;
    //         """);

    //     test_transform(
    //         """
    //         let x;
    //         switch (use?.(x)) {
    //           default: {let y;}
    //         }
    //         """,
    //         """
    //         let x;
    //         use?.(x);
    //         let y;
    //         """);

    //     test_transform(
    //         """
    //         let x;
    //         switch (use(x)) {
    //           default: let y;
    //         }
    //         """,
    //         """
    //         let x;
    //         use(x);
    //         let y;
    //         """);
    //   }

    //   #[test]
    //   fn  testOptimizeSwitchBug335145701() {
    //     test_same(
    //         """
    //         function foo() { alert('foo()'); }
    //         switch (1) {
    //           case 1: break;
    //           case foo(): break;
    //         }
    //         """);

    //     test_same(
    //         """
    //         function foo() { alert('foo()'); }
    //         switch (1) {
    //           case 0: break;
    //           case 1: break;
    //           case foo(): break;
    //         }
    //         """);

    //     test_same(
    //         """
    //         function foo() { alert('foo()'); }
    //         switch (1) {
    //           case 0: alert('bar'); break;
    //           case 1: break;
    //           case foo(): break;
    //         }
    //         """);

    //     test_same(
    //         """
    //         function foo() { alert('foo()'); return 1; }
    //         switch (1) {
    //           case 0: break;
    //           case foo(): break;
    //           case 2: break;
    //         }
    //         """);

    //     test_transform(
    //         """
    //         function foo() { alert('foo()'); }
    //         switch (1) {
    //           case foo(): break;
    //           case (0,1): break;
    //         }
    //         """,
    //         """
    //         function foo() { alert('foo()'); }
    //         switch (1) {
    //           case foo(): break;
    //           case 1: break;
    //         }
    //         """);

    //     test_same(
    //         """
    //         function foo() { alert('foo()'); }
    //         switch (x) {
    //           case 1: break;
    //           case foo(): break;
    //         }
    //         """);

    //     test_transform(
    //         """
    //         // not valid to remove the useless case 1,
    //         // it would cause the default to run and it has side-effects
    //         switch (1) {
    //           case 1: break;
    //           default:
    //             bar();
    //             break;
    //         }
    //         """,
    //         "");

    //     test_same(
    //         """
    //         function foo() { alert('foo()'); }
    //         switch (1) {
    //           case 0: alert('bar'); break;
    //           case 1: break;
    //           case foo(): break;
    //         }
    //         """);

    //     test_same(
    //         """
    //             function foo() { alert('foo()'); }
    //             switch (bar()) {
    //               case 1: break;
    //               case foo(): break;
    //             }
    //         """);

    //     test_transform(
    //         """
    //         // is not valid to remove the first useless case 1,
    //         // because it matches and the second should not run
    //         switch (1) {
    //           case 1: break;
    //           case 1: bar(); break;
    //         }
    //         """,
    //         "");
    //   }

    //   #[test]
    //   fn  testOptimizeSwitchBug11536863() {
    //     test_transform(
    //         """
    //         outer: {
    //           switch (2) {
    //             case 2:
    //               f();
    //               break outer;
    //           }
    //         }
    //         """,
    //         "outer: {f(); break outer;}");
    //   }

    //   // `a[b]` could trigger a getter or setter, and have side effects. However, we always assume it
    //   // does not (even though it's unsound) because the code size cost of assuming all GETELEM nodes
    //   // have side effects is unacceptable.
    //   #[test]
    //   fn  testUnusedGetElemRemoved() {
    //     test_transform("a[b]", "");
    //     test_transform("a?.[b]", "");
    //   }

    //   #[test]
    //   fn  testOptimizeSwitch2() {
    //     test_transform(
    //         """
    //         outer: switch (2) {
    //           case 2:
    //             f();
    //             break outer;
    //         }
    //         """,
    //         "outer: {f(); break outer;}");
    //   }

    //   #[test]
    //   fn  testOptimizeSwitch3() {
    //     test_transform(
    //         """
    //         switch (1) {
    //           case 1:
    //           case 2:
    //           case 3: {
    //             break;
    //           }
    //           case 4:
    //           case 5:
    //           case 6:
    //           default:
    //             fail('Should not get here');
    //             break;
    //         }
    //         """,
    //         "");
    //   }

    //   #[test]
    //   fn  testOptimizeSwitchWithLabellessBreak() {
    //     test_transform(
    //         """
    //         function f() {
    //           switch('x') {
    //             case 'x': var x = 1; break;
    //             case 'y': break;
    //           }
    //         }
    //         """,
    //         "function f() { var x = 1; }");

    //     // TODO: Optimise switch to if.
    //     test_same(
    //         """
    //         function f() {
    //           switch(x) {
    //             case 'y': break;
    //             default: var x = 1;
    //           }
    //         }
    //         """);

    //     test_transform(
    //         """
    //         var exit;
    //         switch ('a') {
    //           case 'a':
    //             break;
    //           default:
    //             exit = 21;
    //             break;
    //         }
    //         switch(exit) {
    //           case 21: throw 'x';
    //           default : console.log('good');
    //         }
    //         """,
    //         """
    //         var exit;
    //         switch(exit) {
    //           case 21: throw 'x';
    //           default : console.log('good');
    //         }
    //         """);

    //     test_transform(
    //         """
    //         let x = 1;
    //         switch('x') {
    //           case 'x': let x = 2; break;
    //         }
    //         """,
    //         """
    //         let x = 1;
    //         let x$jscomp$1 = 2
    //         """);
    //   }

    //   #[test]
    //   fn  testOptimizeSwitchWithLabelledBreak() {
    //     test_transform(
    //         """
    //         function f() {
    //           label:
    //           switch('x') {
    //             case 'x': break label;
    //             case 'y': throw f;
    //           }
    //         }
    //         """,
    //         "function f() { }");

    //     test_transform(
    //         """
    //         function f() {
    //           label:
    //           switch('x') {
    //             case 'x': break label;
    //             default: throw f;
    //           }
    //         }
    //         """,
    //         "function f() { }");
    //   }

    //   #[test]
    //   fn  testOptimizeSwitchWithReturn() {
    //     test_transform(
    //         """
    //         function f() {
    //           switch('x') {
    //             case 'x': return 1;
    //             case 'y': return 2;
    //           }
    //         }
    //         """,
    //         "function f() { return 1; }");

    //     test_transform(
    //         """
    //         function f() {
    //           let x = 1;
    //           switch('x') {
    //             case 'x': { let x = 2; } return 3;
    //             case 'y': return 4;
    //           }
    //         }
    //         """,
    //         """
    //         function f() {
    //           let x = 1;
    //           let x$jscomp$1 = 2;
    //           return 3;
    //         }
    //         """);
    //   }

    //   #[test]
    //   fn  testOptimizeSwitchWithThrow() {
    //     test_transform(
    //         """
    //         function f() {
    //           switch('x') {
    //             case 'x': throw f;
    //             case 'y': throw f;
    //           }
    //         }
    //         """,
    //         "function f() { throw f; }");
    //   }

    //   #[test]
    //   fn  testOptimizeSwitchWithContinue() {
    //     test_transform(
    //         """
    //         function f() {
    //           for (;;) {
    //             switch('x') {
    //               case 'x': continue;
    //               case 'y': continue;
    //             }
    //           }
    //         }
    //         """,
    //         "function f() { for (;;) { continue; } }");
    //   }

    //   #[test]
    //   fn  testOptimizeSwitchWithDefaultCaseWithFallthru() {
    //     test_same(
    //         """
    //         function f() {
    //           switch(a) {
    //             case 'x':
    //             case foo():
    //             default: return 3
    //           }
    //         }
    //         """);
    //   }

    //   // GitHub issue #1722: https://github.com/google/closure-compiler/issues/1722
    //   #[test]
    //   fn  testOptimizeSwitchWithDefaultCase() {
    //     test_transform(
    //         """
    //         function f() {
    //           switch('x') {
    //             case 'x': return 1;
    //             case 'y': return 2;
    //             default: return 3
    //          }
    //         }
    //         """,
    //         "function f() { return 1; }");

    //     test_transform(
    //         """
    //         switch ('hasDefaultCase') {
    //           case 'foo':
    //             foo();
    //             break;
    //           default:
    //             bar();
    //             break;
    //         }
    //         """,
    //         "bar();");

    //     test_same("switch (x) { default: if (a) { break; } bar(); }");

    //     // Potentially foldable
    //     test_same(
    //         """
    //         switch (x) {
    //           case x:
    //             foo();
    //             break;
    //           default:
    //             if (a) { break; }
    //             bar();
    //         }
    //         """);

    //     test_transform(
    //         """
    //         switch ('hasDefaultCase') {
    //           case 'foo':
    //             foo();
    //             break;
    //           default:
    //             if (true) { break; }
    //             bar();
    //         }
    //         """,
    //         "");

    //     test_transform(
    //         """
    //         switch ('hasDefaultCase') {
    //           case 'foo':
    //             foo();
    //             break;
    //           default:
    //             if (a) { break; }
    //             bar();
    //         }
    //         """,
    //         "switch ('hasDefaultCase') { default: if (a) { break; } bar(); }");

    //     test_transform(
    //         """
    //         l: switch ('hasDefaultCase') {
    //           case 'foo':
    //             foo();
    //             break;
    //           default:
    //             if (a) { break l; }
    //             bar();
    //             break;
    //         }
    //         """,
    //         "l:{ if (a) { break l; } bar(); }");

    //     test_transform(
    //         """
    //         switch ('hasDefaultCase') {
    //           case 'foo':
    //             bar();
    //             break;
    //           default:
    //             foo();
    //             break;
    //         }
    //         """,
    //         "foo();");

    //     test_transform("switch (a()) { default: bar(); break;}", "a(); bar();");

    //     test_transform("switch (a?.()) { default: bar(); break;}", "a?.(); bar();");

    //     test_transform("switch (a()) { default: break; bar();}", "a();");

    //     test_transform(
    //         """
    //         loop:
    //         for (;;) {
    //           switch (a()) {
    //             default:
    //               bar();
    //               break loop;
    //           }
    //         }
    //         """,
    //         "loop: for (;;) { a(); bar(); break loop; }");
    //   }

    //   #[test]
    //   fn  testTreatSwitchAsExit() {
    //     test_transform(
    //         "a: {switch(x){ case 1: case 2: break a; default: foo(); break a;} bar(); }",
    //         "a: {switch(x){ case 1: case 2: break a; default: foo(); break a;} }");
    //   }

    #[test]
    fn testDontTreatSwitchAsExit() {
        test_same("a: {switch(x){ case 1:break a; default: b: {foo(); break b;}} bar(); }");

        test_same("a: {switch(x){ case 1:break a; default: foo(); break;} bar(); }");

        test_same("a: {b: switch(x){ case 1:break a; default: foo(); break b;} bar(); }");

        test_same("a: {switch(x){ case 1: b: { foo(); break b; } default: break a; } bar(); }");

        test_same("a: {switch(x){ case 1: break a; } bar(); }");

        test_same("a: {switch(x){ case 1: if (y) { break; } default: break a;} bar(); }");

        test_same("a: {switch(x){ case 1: if (y) { break; } break a; default: break a;} bar(); }");
    }

    //   #[test]
    //   fn  testTreatTryAsExit() {
    //     test_transform(
    //         "a: {try { foo(); break a; } catch (e) { foo(); break a; } bar(); }",
    //         "a: {try { foo(); break a; } catch (e) { foo(); break a; } }");

    //     test_transform(
    //         "a: {try { foo(); } finally { foo(); break a; } bar(); }",
    //         "a: {try { foo(); } finally { foo(); break a; } }");

    //     test_transform(
    //         "a: {try { foo(); break a; } finally { foo(); } bar(); }",
    //         "a: {try { foo(); break a; } finally { foo(); } }");
    //   }

    #[test]
    fn testDontTreatTryAsExit() {
        test_same("a: {try { foo(); break a; } catch (e) { foo(); } bar(); }");

        test_same("a: {try { foo(); break a; } catch (e) { } bar(); }");

        test_same("a: {try { foo(); } catch (e) { foo(); break a; } bar(); }");

        test_same("a: {try { foo(); } finally { foo(); } bar(); }");

        test_same("a: {try { b: { foo(); break b; } } finally { foo(); } bar(); }");

        test_same("a: { b: try { foo(); break a; } finally { foo(); } bar(); }");
    }

    //   #[test]
    //   fn  testRemoveNumber() {
    //     test_transform("3", "");
    //   }

    //   #[test]
    //   fn  testRemoveVarGet1() {
    //     test_transform("a", "");
    //   }

    //   #[test]
    //   fn  testRemoveVarGet2() {
    //     test_transform("var a = 1;a", "var a = 1");
    //   }

    //   #[test]
    //   fn  testRemoveUnusedGetProp() {
    //     test_transform("var a = {};a.b", "var a = {}");
    //   }

    //   #[test]
    //   fn  testRemoveUnusedOptChainGetProp() {
    //     test_transform("var a = {};a?.b", "var a = {}");
    //   }

    //   #[test]
    //   fn  testRemoveUnusedGetProp2() {
    //     test_transform("var a = {};a.b=1;a.b", "var a = {};a.b=1");
    //   }

    //   #[test]
    //   fn  testRemoveUnusedOptChainGetProp2() {
    //     test_transform("var a = {};a.b=1;a?.b", "var a = {};a.b=1");
    //   }

    //   #[test]
    //   fn  testRemovePrototypeGet1() {
    //     test_transform("var a = {};a.prototype.b", "var a = {}");
    //   }

    //   #[test]
    //   fn  testRemoveOptChainPrototypeGet1() {
    //     test_transform("var a = {};a?.prototype.b", "var a = {}");
    //   }

    //   #[test]
    //   fn  testRemovePrototypeGet2() {
    //     test_transform("var a = {};a.prototype.b = 1;a.prototype.b", "var a = {};a.prototype.b = 1");
    //   }

    //   #[test]
    //   fn  testRemoveOptChainPrototypeGet2() {
    //     test_transform("var a = {};a.prototype.b = 1;a?.prototype.b", "var a = {};a.prototype.b = 1");
    //   }

    #[test]
    fn testNotRemovePrototypeGet2() {
        test_same("var a = {};a.prototype.b = 1; let x = a.prototype.b");
    }

    #[test]
    fn testNotRemoveOptChainPrototypeGet2() {
        test_same("var a = {};a.prototype.b = 1; let x = a?.prototype.b");
    }

    //   #[test]
    //   fn  testRemoveAdd1() {
    //     test_transform("1 + 2", "");
    //   }

    #[test]
    fn testNoRemoveVar1() {
        test_same("var a = 1");
    }

    #[test]
    fn testNoRemoveVar2() {
        test_same("var a = 1, b = 2");
    }

    #[test]
    fn testNoRemoveAssign1() {
        test_same("a = 1");
    }

    #[test]
    fn testNoRemoveAssign2() {
        test_same("a = b = 1");
    }

    //   #[test]
    //   fn  testNoRemoveAssign3() {
    //     test_transform("1 + (a = 2)", "a = 2");
    //   }

    #[test]
    fn testNoRemoveAssign4() {
        test_same("x.a = 1");
    }

    #[test]
    fn testNoRemoveAssign5() {
        test_same("x.a = x.b = 1");
    }

    //   #[test]
    //   fn  testNoRemoveAssign6() {
    //     test_transform("1 + (x.a = 2)", "x.a = 2");
    //   }

    #[test]
    fn testNoRemoveCall1() {
        test_same("a()");
    }

    #[test]
    fn testNoRemoveOptChainCall1() {
        test_same("a?.()");
    }

    //   #[test]
    //   fn  testNoRemoveCall2() {
    //     test_transform("a()+b()", "a(),b()");
    //   }

    //   #[test]
    //   fn  testNoRemoveOptChainCall2() {
    //     test_transform("a?.()+b?.()", "a?.(),b?.()");
    //   }

    #[test]
    fn testNoRemoveCall3() {
        test_same("a() && b()");
    }

    #[test]
    fn testNoRemoveOptChainCall3() {
        test_same("a?.() && b?.()");
    }

    #[test]
    fn testNoRemoveCall4() {
        test_same("a() || b()");
    }

    #[test]
    fn testNoRemoveOptChainCall4() {
        test_same("a?.() || b?.()");
    }

    #[test]
    fn testNoRemoveCall4NullishCoalesce() {
        test_same("a() ?? b()");
    }

    #[test]
    fn testNoRemoveOptChainCall4NullishCoalesce() {
        test_same("a?.() ?? b?.()");
    }

    //   #[test]
    //   fn  testNoRemoveCall5NullishCoalesce() {
    //     test_transform("a() ?? 1", "a()");
    //   }

    //   #[test]
    //   fn  testNoRemoveOptChainCall5NullishCoalesce() {
    //     test_transform("a?.() ?? 1", "a?.()");
    //   }

    #[test]
    fn testNoRemoveCall6NullishCoalesce() {
        test_same("1 ?? a()");
    }

    //   #[test]
    //   fn  testNoRemoveCall5() {
    //     test_transform("a() || 1", "a()");
    //   }

    #[test]
    fn testNoRemoveCall6() {
        test_same("1 || a()");
    }

    #[test]
    fn testNoRemoveThrow1() {
        test_same("function f(){throw a()}");
    }

    #[test]
    fn testNoRemoveThrow2() {
        test_same("function f(){throw a}");
    }

    #[test]
    fn testNoRemoveThrow3() {
        test_same("function f(){throw 10}");
    }

    //   #[test]
    //   fn  testRedundantIfRemoved() {
    //     test_transform("if(x()) 1", "x()");
    //   }

    //   #[test]
    //   fn  testRemoveInControlStructure3() {
    //     test_transform("for(1;2;3) 4", "for(;;);");
    //   }

    //   #[test]
    //   fn  testShortCircuit1() {
    //     test_same("1 && a()");
    //   }

    //   #[test]
    //   fn  testShortCircuit2NullishCoalesce() {
    //     test_transform("1 ?? a() ?? 2", "1 ?? a()");
    //   }

    //   #[test]
    //   fn  testShortCircuit3NullishCoalesce() {
    //     test_transform("a() ?? 1 ?? 2", "a()");
    //   }

    #[test]
    fn testShortCircuit4NullishCoalesce() {
        test_same("a() ?? 1 ?? b()");
    }

    //   #[test]
    //   fn  testShortCircuit2() {
    //     test_transform("1 && a() && 2", "1 && a()");
    //   }

    //   #[test]
    //   fn  testShortCircuit3() {
    //     test_transform("a() && 1 && 2", "a()");
    //   }

    #[test]
    fn testShortCircuit4() {
        test_same("a() && 1 && b()");
    }

    //   #[test]
    //   fn  testComplex1() {
    //     test_transform("1 && a() + b() + c()", "1 && (a(), b(), c())");
    //   }

    //   #[test]
    //   fn  testComplex2() {
    //     test_transform("1 && (a() ? b() : 1)", "1 && (a() && b())");
    //   }

    //   #[test]
    //   fn  testComplex3() {
    //     test_transform("1 && (a() ? b() : 1 + c())", "1 && (a() ? b() : c())");
    //   }

    //   #[test]
    //   fn  testComplex4() {
    //     test_transform("1 && (a() ? 1 : 1 + c())", "1 && (a() || c())");
    //   }

    #[test]
    fn testComplex5() {
        // can't simplify LHS of short circuit statements with side effects
        test_same("(a() ? 1 : 1 + c()) && foo()");
    }

    #[test]
    fn testNoRemoveFunctionDeclaration1() {
        test_same("function foo(){}");
    }

    #[test]
    fn testNoRemoveFunctionDeclaration2() {
        test_same("var foo = function (){}");
    }

    #[test]
    fn testNoSimplifyFunctionArgs1() {
        test_same("f(1 + 2, 3 + g())");
    }

    #[test]
    fn testNoSimplifyFunctionArgs2() {
        test_same("1 && f(1 + 2, 3 + g())");
    }

    #[test]
    fn testNoSimplifyFunctionArgs3() {
        test_same("1 && foo(a() ? b() : 1 + c())");
    }

    #[test]
    fn testNoRemoveInherits1() {
        test_same("var a = {}; this.b = {}; var goog = {}; goog.inherits(b, a)");
    }

    //   #[test]
    //   fn  testNoRemoveInherits2() {
    //     test_transform(
    //         "var a = {}; this.b = {}; var goog = {}; goog.inherits(b, a) + 1",
    //         "var a = {}; this.b = {}; var goog = {}; goog.inherits(b, a)");
    //   }

    #[test]
    fn testNoRemoveInherits3() {
        test_same("this.a = {}; var b = {}; b.inherits(a);");
    }

    //   #[test]
    //   fn  testNoRemoveInherits4() {
    //     test_transform("this.a = {}; var b = {}; b.inherits(a) + 1;", "this.a = {}; var b = {}; b.inherits(a)");
    //   }

    //   #[test]
    //   fn  testRemoveFromLabel1() {
    //     test_transform("LBL: void 0", "");
    //   }

    //   #[test]
    //   fn  testRemoveFromLabel2() {
    //     test_transform("LBL: foo() + 1 + bar()", "LBL: foo(),bar()");
    //   }

    //   #[test]
    //   fn  testCall() {
    //     test_same("foo(0)");
    //     // We use a function with no side-effects, otherwise the entire invocation would be preserved.
    //     test_transform("Math.sin(0);", "");
    //     test_transform("1 + Math.sin(0);", "");
    //   }

    //   #[test]
    //   fn  testCall_containingSpread() {
    //     // We use a function with no side-effects, otherwise the entire invocation would be preserved.
    //     test_transform("Math.sin(...c)", "([...c])");
    //     test_transform("Math.sin(4, ...c, a)", "([...c])");
    //     test_transform("Math.sin(foo(), ...c, bar())", "(foo(), [...c], bar())");
    //     test_transform("Math.sin(...a, b, ...c)", "([...a], [...c])");
    //     test_transform("Math.sin(...b, ...c)", "([...b], [...c])");
    //   }

    //   #[test]
    //   fn  testOptChainCall_containingSpread() {
    //     // We use a function with no side-effects, otherwise the entire invocation would be preserved.
    //     test_transform("Math?.sin(...c)", "([...c])");
    //     test_transform("Math?.sin(4, ...c, a)", "([...c])");
    //     test_transform("Math?.sin(foo(), ...c, bar())", "(foo(), [...c], bar())");
    //     test_transform("Math?.sin(...a, b, ...c)", "([...a], [...c])");
    //     test_transform("Math?.sin(...b, ...c)", "([...b], [...c])");
    //   }

    //   #[test]
    //   fn  testNew() {
    //     test_same("new foo(0)");
    //     // We use a function with no side-effects, otherwise the entire invocation would be preserved.
    //     test_transform("new Date;", "");
    //     test_transform("1 + new Date;", "");
    //   }

    //   #[test]
    //   fn  testNew_containingSpread() {
    //     // We use a function with no side-effects, otherwise the entire invocation would be preserved.
    //     test_transform("new Date(...c)", "([...c])");
    //     test_transform("new Date(4, ...c, a)", "([...c])");
    //     test_transform("new Date(foo(), ...c, bar())", "(foo(), [...c], bar())");
    //     test_transform("new Date(...a, b, ...c)", "([...a], [...c])");
    //     test_transform("new Date(...b, ...c)", "([...b], [...c])");
    //   }

    //   #[test]
    //   fn  testTaggedTemplateLit_simpleTemplate() {
    //     test_same("foo`Simple`");
    //     // We use a function with no side-effects, otherwise the entire invocation would be preserved.
    //     test_transform("Math.sin`Simple`", "");
    //     test_transform("1 + Math.sin`Simple`", "");
    //   }

    //   #[test]
    //   fn  testTaggedTemplateLit_substitutingTemplate() {
    //     test_same("foo`Complex ${butSafe}`");
    //     // We use a function with no side-effects, otherwise the entire invocation would be preserved.
    //     test_transform("Math.sin`Complex ${butSafe}`", "");
    //     test_transform("Math.sin`Complex ${andDangerous()}`", "andDangerous()");
    //   }

    #[test]
    fn testFoldAssign() {
        // test_transform("x=x", "");
        test_same("x=xy");
        test_same("x=x + 1");
        test_same("x.a=x.a");
        test_transform("var y=(x=x)", "var y=x");
        test_transform("y=1 + (x=x)", "y=1 + x");
    }

    //   #[test]
    //   fn  testTryCatchFinally() {
    //     test_same("try {foo()} catch (e) {bar()}");
    //     test_same("try { try {foo()} catch (e) {bar()}} catch (x) {bar()}");
    //     test_transform("try {var x = 1} finally {}", "var x = 1;");
    //     test_same("try {var x = 1} finally {x()}");
    //     test_transform( //
    //         "function f() { return; try{ var x = 1; }finally{} }", "function f() { var x; return; }");
    //     test_transform("try {} finally {x()}", "x()");
    //     test_transform("try {} catch (e) { bar()} finally {x()}", "x()");
    //     test_transform("try {} catch (e) { bar()}", "");
    //     test_transform("try {} catch (e) { var a = 0; } finally {x()}", "var a; x()");
    //     test_transform("try {} catch (e) {}", "");
    //     test_transform("try {} finally {}", "");
    //     test_transform("try {} catch (e) {} finally {}", "");
    //     test_transform("L1:try {} catch (e) {} finally {}", "");
    //     test_transform("L2:L1:try {} catch (e) {} finally {}", "");
    //   }

    //   #[test]
    //   fn  testObjectLiteral() {
    //     test_transform("({})", "");
    //     test_transform("({a:1})", "");
    //     test_transform("({a:foo()})", "foo()");
    //     test_transform("({'a':foo()})", "foo()");
    //     // Object-spread may tigger getters.
    //     test_same("({...a})");
    //     test_same("({...foo()})");
    //   }

    //   #[test]
    //   fn  testArrayLiteral() {
    //     test_transform("([])", "");
    //     test_transform("([1])", "");
    //     test_transform("([a])", "");
    //     test_transform("([foo()])", "foo()");
    //   }

    //   #[test]
    //   fn  testArrayLiteral_containingSpread() {
    //     test_same("([...c])");
    //     test_transform("([4, ...c, a])", "([...c])");
    //     test_transform("([foo(), ...c, bar()])", "(foo(), [...c], bar())");
    //     test_transform("([...a, b, ...c])", "([...a], [...c])");
    //     test_same("([...b, ...c])"); // It would also be fine if the spreads were split apart.
    //   }

    //   #[test]
    //   fn  testAwait() {
    //     test_same("async function f() { await something(); }");
    //     test_same("async function f() { await some.thing(); }");
    //   }

    //   #[test]
    //   fn  testEmptyPatternInDeclarationRemoved() {
    //     test_transform("var [] = [];", "");
    //     test_transform("let [] = [];", "");
    //     test_transform("const [] = [];", "");
    //     test_transform("var {} = [];", "");
    //     test_transform("var [] = foo();", "foo()");
    //   }

    //   #[test]
    //   fn  testEmptyArrayPatternInAssignRemoved() {
    //     test_transform("({} = {});", "");
    //     test_transform("({} = foo());", "foo()");
    //     test_transform("[] = [];", "");
    //     test_transform("[] = foo();", "foo()");
    //   }

    #[test]
    fn testEmptyPatternInParamsNotRemoved() {
        test_same("function f([], a) {}");
        test_same("function f({}, a) {}");
    }

    #[test]
    fn testEmptyPatternInForOfLoopNotRemoved() {
        test_same("for (let [] of foo()) {}");
        test_same("for (const [] of foo()) {}");
        test_same("for ([] of foo()) {}");
        test_same("for ({} of foo()) {}");
    }

    //   #[test]
    //   fn  testEmptySlotInArrayPatternRemoved() {
    //     test_transform("[,,] = foo();", "foo()");
    //     test_transform("[a,b,,] = foo();", "[a,b] = foo();");
    //     test_transform("[a,[],b,[],[]] = foo();", "[a,[],b] = foo();");
    //     test_transform("[a,{},b,{},{}] = foo();", "[a,{},b] = foo();");
    //     test_transform("function f([,,,]) {}", "function f([]) {}");
    //     test_same("[[], [], [], ...rest] = foo()");
    //   }

    //   #[test]
    //   fn  testEmptySlotInArrayPatternWithDefaultValueMaybeRemoved() {
    //     test_transform("[a,[] = 0] = [];", "[a] = [];");
    //     test_same("[a,[] = foo()] = [];");
    //   }

    //   #[test]
    //   fn  testEmptyKeyInObjectPatternRemoved() {
    //     test_transform("const {f: {}} = {};", "");
    //     test_transform("const {f: []} = {};", "");
    //     test_transform("const {f: {}, g} = {};", "const {g} = {};");
    //     test_transform("const {f: [], g} = {};", "const {g} = {};");
    //     test_same("const {[foo()]: {}} = {};");
    //   }

    //   #[test]
    //   fn  testEmptyKeyInObjectPatternWithDefaultValueMaybeRemoved() {
    //     test_transform("const {f: {} = 0} = {};", "");
    //     // In theory the following case could be reduced to `foo()`, but that gets more complicated to
    //     // implement for object patterns with multiple keys with side effects.
    //     // Instead the pass backs off for any default with a possible side effect
    //     test_same("const {f: {} = foo()} = {};");
    //   }

    #[test]
    fn testEmptyKeyInObjectPatternNotRemovedWithObjectRest() {
        test_same("const {f: {}, ...g} = foo()");
        test_same("const {f: [], ...g} = foo()");
    }

    //   #[test]
    //   fn  testUndefinedDefaultParameterRemoved() {
    //     test_transform(
    //         "function f(x=undefined,y) {  }", //
    //         "function f(x,y)             {  }");
    //     test_transform(
    //         "function f(x,y=undefined,z) {  }", //
    //         "function f(x,y          ,z) {  }");
    //     test_transform(
    //         "function f(x=undefined,y=undefined,z=undefined) {  }", //
    //         "function f(x,          y,          z)           {  }");
    //   }

    //   #[test]
    //   fn  testPureVoidDefaultParameterRemoved() {
    //     test_transform(
    //         "function f(x = void 0) {  }", //
    //         "function f(x         ) {  }");
    //     test_transform(
    //         "function f(x = void \"XD\") {  }", //
    //         "function f(x              ) {  }");
    //     test_transform(
    //         "function f(x = void f()) {  }", //
    //         "function f(x)            {  }");
    //   }

    #[test]
    fn testNoDefaultParameterNotRemoved() {
        test_same("function f(x,y) {  }");
        test_same("function f(x) {  }");
        test_same("function f() {  }");
    }

    #[test]
    fn testEffectfulDefaultParameterNotRemoved() {
        test_same("function f(x = void console.log(1)) {  }");
        test_same("function f(x = void f()) { alert(x); }");
    }

    //   #[test]
    //   fn  testDestructuringUndefinedDefaultParameter() {
    //     test_transform(
    //         "function f({a=undefined,b=1,c}) {  }", //
    //         "function f({a          ,b=1,c}) {  }");
    //     test_transform(
    //         "function f({a={},b=0}=undefined) {  }", //
    //         "function f({a={},b=0}) {  }");
    //     test_transform(
    //         "function f({a=undefined,b=0}) {  }", //
    //         "function f({a,b=0}) {  }");
    //     test_transform(
    //         " function f({a: {b = undefined}}) {  }", //
    //         " function f({a: {b}}) {  }");
    //     test_same("function f({a,b}) {  }");
    //     test_same("function f({a=0, b=1}) {  }");
    //     test_same("function f({a=0,b=0}={}) {  }");
    //     test_same("function f({a={},b=0}={}) {  }");
    //   }

    //   #[test]
    //   fn  testUndefinedDefaultObjectPatterns() {
    //     test_transform(
    //         "const {a = undefined} = obj;", //
    //         "const {a} = obj;");
    //     test_transform(
    //         "const {a = void 0} = obj;", //
    //         "const {a} = obj;");
    //   }

    //   #[test]
    //   fn  testDoNotRemoveGetterOnlyAccess() {
    //     test_same(
    //         """
    //         var a = {
    //           get property() {}
    //         };
    //         a.property;
    //         """);

    //     test_same(
    //         """
    //         var a = {
    //           get property() {}
    //         };
    //         a?.property;
    //         """);

    //     test_same(
    //         """
    //         var a = {};
    //         Object.defineProperty(a, 'property', {
    //           get() {}
    //         });
    //         a.property;
    //         """);

    //     test_same(
    //         """
    //         var a = {};
    //         Object.defineProperty(a, 'property', {
    //           get() {}
    //         });
    //         a?.property;
    //         """);
    //   }

    //   #[test]
    //   fn  testDoNotRemoveNestedGetterOnlyAccess() {
    //     test_same(
    //         """
    //         var a = {
    //           b: { get property() {} }
    //         };
    //         a.b.property;
    //         """);
    //   }

    //   #[test]
    //   fn  testRemoveAfterNestedGetterOnlyAccess() {
    //     test_transform(
    //         """
    //         var a = {
    //           b: { get property() {} }
    //         };
    //         a.b.property.d.e;
    //         """,
    //         """
    //         var a = {
    //           b: { get property() {} }
    //         };
    //         a.b.property;
    //         """);
    //   }

    //   #[test]
    //   fn  testFoldLabelledEmptyBlock() {
    //     test_transform("a:{}", "");
    //     test_transform("a:b:{}", "");
    //     test_transform("a:b:c:{}", "");
    //   }

    //   #[test]
    //   fn  testRetainSetterOnlyAccess() {
    //     test_same(
    //         """
    //         var a = {
    //           set property(v) {}
    //         };
    //         a.property;
    //         """);

    //     test_same(
    //         """
    //         var a = {
    //           set property(v) {}
    //         };
    //         a?.property;
    //         """);
    //   }

    //   #[test]
    //   fn  testDoNotRemoveGetterSetterAccess() {
    //     test_same(
    //         """
    //         var a = {
    //           get property() {},
    //           set property(x) {}
    //         };
    //         a.property;
    //         """);
    //   }

    //   #[test]
    //   fn  testDoNotRemoveSetSetterToGetter() {
    //     test_same(
    //         """
    //         var a = {
    //           get property() {},
    //           set property(x) {}
    //         };
    //         a.property = a.property;
    //         """);
    //   }

    //   #[test]
    //   fn  testDoNotRemoveAccessIfOtherPropertyIsGetter() {
    //     test_same(
    //         """
    //         var a = {
    //           get property() {}
    //         };
    //         var b = {
    //           property: 0,
    //         };
    //         // This pass should be conservative and not remove this since it sees a getter for
    //         // "property"
    //         b.property;
    //         """);

    //     test_same(
    //         """
    //         var a = {};
    //         Object.defineProperty(a, 'property', {
    //           get() {}
    //         });
    //         var b = {
    //           property: 0,
    //         };
    //         b.property;
    //         """);
    //   }

    //   #[test]
    //   fn  testFunctionCallReferencesGetterIsNotRemoved() {
    //     test_same(
    //         """
    //         var a = {
    //           get property() {}
    //         };
    //         function foo() { a.property; }
    //         foo();
    //         """);
    //   }

    //   #[test]
    //   fn  testFunctionCallReferencesSetterIsNotRemoved() {
    //     test_same(
    //         """
    //         var a = {
    //           set property(v) {}
    //         };
    //         function foo() { a.property = 0; }
    //         foo();
    //         """);
    //   }

    //   #[test]
    //   fn  testClassField() {
    //     test_transform(
    //         """
    //         class C {
    //           f1 = (5,2);
    //         }
    //         """,
    //         """
    //         class C {
    //           f1 = 2;
    //         }
    //         """);
    //   }

    //   #[test]
    //   fn  testThis() {
    //     test_transform(
    //         """
    //         class C {
    //           constructor() {
    //             this.f1 = (5,2);
    //           }
    //         }
    //         """,
    //         """
    //         class C {
    //           constructor() {
    //             this.f1 = 2;
    //           }
    //         }
    //         """);
    //   }

    //   #[test]
    //   fn  testClassStaticBlock() {
    //     test_transform(
    //         """
    //         class C {
    //           static {
    //           }
    //         }
    //         """,
    //         """
    //         class C {
    //         }
    //         """);

    //     test_same(
    //         """
    //         class C {
    //           static {
    //             this.x = 0;
    //           }
    //         }
    //         """);
    //   }

    //   #[test]
    //   fn  testRemoveUnreachableOptionalChainingCall() {
    //     test_transform("(null)?.();", "");
    //     test_transform("(void 0)?.();", "");
    //     test_transform("(undefined)?.();", "");
    //     test_transform("(void 0)?.(0)", "");
    //     test_transform("(void 0)?.(function f() {})", "");
    //     test_transform("(null)?.x;", "");
    //     test_transform("(void 0)?.x;", "");
    //     test_transform("(null)?.['x'];", "");
    //     test_transform("(void 0)?.['x'];", "");
    //     test_transform("(null)?.[x];", "");
    //     test_transform("(void 0)?.[x];", "");
    //     // arguments with unknown side effects are also removed
    //     test_transform("(void 0)?.(f(), g())", "");

    //     // void arguments with unknown side effects are preserved
    //     test_transform("(void f())?.();", "f();");
    //     test_transform("g((void f())?.());", "g(void f());");

    //     test_same("(f(), null)?.()");
    //     test_same("f?.()");
    //     test_transform("a?.x;", "");
    //     test_transform("a?.['x'];", "");
    //   }

    //   #[test]
    //   fn  testRemoveUnusedVoid() {
    //     // remove void at statement level
    //     test_transform("void 0;", "");
    //     test_transform("void foo();", "foo();");
    //     // preserve void when passed somewhere else
    //     test_same("use(void 0);");
    //     test_same("use(void foo());");
    //     test_same("use(() => void foo());");

    //     test_transform("void use(() => void foo());", "use(() => void foo());");
    //   }

    //   private void testInFn(String js, String expected) {
    //     String pre = "function f() {";
    //     String post = "}";
    //     test(pre + js + post, pre + expected + post);
    //   }

    //   private void testInLoop(String js, String expected) {
    //     testInLoop(js, "", expected);
    //   }

    //   private void testInLoop(String js, String expectedBeforeLoop, String expected) {
    //     String pre = "for (;;) {";
    //     String post = "}";
    //     test(pre + js + post, expectedBeforeLoop + pre + expected + post);
    //   }

    //   #[test]
    //   fn  testRemoveDeadStatements1() {
    //     test("throw 1; x;", "throw 1;");
    //     test("throw 1; alert(1)", "throw 1;");
    //     test("throw 1; var x = 1", "var x; throw 1;");
    //   }

    //   #[test]
    //   fn  testRemoveDeadStatements2() {
    //     testInFn("return; x;", "return;");
    //     testInFn("return; alert(1)", "return;");
    //     testInFn("return; var x = 1", "var x; return;");
    //   }

    //   #[test]
    //   fn  testRemoveDeadStatements3() {
    //     testInLoop("break; x;", "break;");
    //     testInLoop("break; alert(1)", "break;");
    //     testInLoop("break; var x = 1", "var x;", "break;");
    //   }

    //   #[test]
    //   fn  testRemoveDeadStatements4() {
    //     testInLoop("continue; x;", "continue;");
    //     testInLoop("continue; alert(1)", "continue;");
    //     testInLoop("continue; var x = 1", "var x;", "continue;");
    //   }

    //   #[test]
    //   fn  testRemovalRequiresRedeclaration() {
    //     test( //
    //         "while(1) { break; var x = 1}", //
    //         "var x; for(;;) { break }");
    //     test( //
    //         "while(1) { break; var x=1; var y=1 }", //
    //         "var y; var x; for(;;) { break }");
    //     test( //
    //         "while(1) { break; var [x, [[[y]]]] = [];}", //
    //         "var y; var x; for(;;) { break }");
    //   }

    //   #[test]
    //   fn  testRemovalRequiresRedeclaration_normalizeDisabled() {
    //     disableNormalize();
    //     disableComputeSideEffects();
    //     test( //
    //         "while(1) { break; var x = 1}", //
    //         "var x; while (1) { break; }");
    //     test( //
    //         "while(1) { break; var x=1; var y=1 }", //
    //         "var y; var x; while (1) { break; }");
    //     test( //
    //         "while(1) { break; var [x, [[[y]]]] = [];}", //
    //         "var y; var x; while (1) { break; }");
    //   }

    //   #[test]
    //   fn  testRemoveDo() {
    //     test("do { print(1); break } while(1)", "do { print(1); break } while(1)");
    //     test("while(1) { break; do { print(1); break } while(1) }", "for (;;) { break; }");
    //   }

    //   #[test]
    //   fn  testSwitchCase() {
    //     test(
    //         "function f() { switch(x) { case 1: break; default: return 5; foo()}}",
    //         "function f() { switch(x) { case 1: break; default: return 5;}}");
    //     test(
    //         "function f() { switch(x) { default: return; case 1: foo(); bar()}}",
    //         "function f() { switch(x) { default: return; case 1: foo(); bar()}}");
    //     test(
    //         "function f() { switch(x) { default: return; case 1: return 5;bar()}}",
    //         "function f() { switch(x) { default: return; case 1: return 5;}}");
    //   }

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
