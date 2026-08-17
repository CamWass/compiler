use ast::*;
use visit::{VisitMut, VisitMutWith};

pub fn process(ast: &mut Program) {
    let mut visitor = Visitor;
    ast.visit_mut_with(&mut visitor);
}

struct Visitor;

impl VisitMut<'_> for Visitor {
    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        stmts.visit_mut_children_with(self);

        merge_consecutive_in_place(
            stmts,
            |s| match s {
                Stmt::Decl(Decl::Var(v)) => Some(v),
                _ => None,
            },
            |acc, next| {
                if acc.kind == next.kind {
                    acc.decls.append(&mut next.decls);
                    true
                } else {
                    false
                }
            },
        );
    }

    fn visit_mut_module_items(&mut self, items: &mut Vec<ModuleItem>) {
        items.visit_mut_children_with(self);

        merge_consecutive_in_place(
            items,
            |s| match s {
                ModuleItem::Stmt(Stmt::Decl(Decl::Var(v))) => Some(v),
                _ => None,
            },
            |acc, next| {
                if acc.kind == next.kind {
                    acc.decls.append(&mut next.decls);
                    true
                } else {
                    false
                }
            },
        );
    }
}

fn merge_consecutive_in_place<T, U>(
    vec: &mut Vec<T>,
    mut map_fn: impl FnMut(&mut T) -> Option<&mut U>,
    mut try_merge: impl FnMut(&mut U, &mut U) -> bool,
) {
    vec.dedup_by(|next, acc| match (map_fn(acc), map_fn(next)) {
        (Some(acc), Some(next)) => try_merge(acc, next),
        _ => false,
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::resolve;
    use common::{GLOBALS, Globals, Mark};

    #[test]
    fn test_collapsing() {
        // Basic collapsing.
        test_transform("var a;var b;", "var a,b;");

        // With initial values.
        test_transform("var a = 1;var b = 1;", "var a=1,b=1;");

        // Already collapsed.
        test_same("var a, b;");

        // Already collapsed with values.
        test_same("var a = 1, b = 1;");

        // Some already collapsed.
        test_transform("var a;var b, c;var d;", "var a,b,c,d;");

        // Some already collapsed with values.
        test_transform(
            "var a = 1;var b = 2, c = 3;var d = 4;",
            "var a=1,b=2,c=3,d=4;",
        );

        test_transform(
            "var x = 2; foo(x); x = 3; x = 1; var y = 2; var z = 4; x = 5",
            "var x = 2; foo(x); x = 3; x = 1; var y = 2, z = 4; x = 5",
        );
    }

    #[test]
    fn test_collapsing_let_const() {
        // Basic collapsing.
        test_transform("let a; let b;", "let a,b;");

        // With initial values.
        test_transform("const a = 1; const b = 1;", "const a=1,b=1;");

        // Already collapsed.
        test_same("let a, b;");

        // Already collapsed with values.
        test_same("let a = 1, b = 1;");

        // Some already collapsed.
        test_transform("let a; let b, c; let d;", "let a,b,c,d;");

        // Some already collapsed with values.
        test_transform(
            "let a = 1; let b = 2, c = 3; let d = 4;",
            "let a=1,b=2,c=3,d=4;",
        );
    }

    #[test]
    fn test_mixed_declaration_types() {
        test_same("let x = 1; var y = 2; const z = 3");

        test_same("let x = 1; var y = 2; let z = 3;");

        test_transform(
            "let x = 1; let z = 3; var y = 2;",
            "let x = 1, z = 3; var y = 2;",
        );

        test_transform(
            "let x = 1; let y = 2; var z = 3; var a = 4;",
            "let x = 1, y = 2; var z = 3, a = 4",
        );
    }

    #[test]
    fn test_if_else_var_declarations() {
        test_same("if (x) var a = 1; else var b = 2;");
        test_same("if (x) { let a = 1; } else { let b = 2; }");
    }

    #[test]
    fn test_interweaving_stmts_are_preserved() {
        test_transform("var x; var y = 3; x = 5;", "var x, y = 3; x = 5;");

        test_same("var x; x = 5; var z = 7;");

        test_transform(
            "var x; var y = 3; x = 5; var z = 7;",
            "var x, y = 3; x = 5; var z = 7;",
        );

        test_transform(
            "var a = 1; var x; var y = 3; x = 5;",
            "var a = 1, x, y = 3; x = 5;",
        );
    }

    #[test]
    fn test_in_function() {
        test_transform(
            "function f() { let x = 1; let y = 2; let z = 3; x + y + z; }",
            "function f() { let x = 1, y = 2, z = 3; x + y + z; } ",
        );

        test_transform(
            "() => {let x = 1; let y = 2; x + y; }",
            "() => {let x = 1, y = 2; x + y; }",
        );

        test_transform(
            "var x = 1; function f() { let x = 1; let y = 2; x + y; }",
            "var x = 1; function f() { let x = 1, y = 2; x + y } ",
        );
    }

    fn test_transform(input: &str, expected: &str) {
        crate::testing::test_transform(
            |mut program, _program_data| {
                GLOBALS.set(&Globals::new(), || {
                    let unresolved_mark = Mark::new();

                    resolve(&mut program, unresolved_mark);

                    process(&mut program);

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
