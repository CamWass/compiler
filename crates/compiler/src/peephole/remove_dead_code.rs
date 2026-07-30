use ast::*;
use visit::{VisitMut, VisitMutWith};

use crate::utils::unwrap_as;

pub fn process(ast: &mut Program) {
    let mut visitor = Visitor;
    ast.visit_mut_with(&mut visitor);
}

struct Visitor;

impl VisitMut<'_> for Visitor {
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
                stmts.splice(i..i, block.stmts.into_iter());

                if num_stmts > 0 {
                    // Skip over the new stmts.
                    i += num_stmts - 1;
                } else {
                    i += 1;
                }
            } else {
                i += 1;
            }
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
                items.splice(
                    i..i,
                    block.stmts.into_iter().map(|stmt| ModuleItem::Stmt(stmt)),
                );

                if num_stmts > 0 {
                    // Skip over the new stmts.
                    i += num_stmts - 1;
                } else {
                    i += 1;
                }
            } else {
                i += 1;
            }
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
    fn test_same(input: &str) {
        test_transform(input, input);
    }
}
