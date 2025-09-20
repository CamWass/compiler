use ast::*;
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::{util::take::Take, DUMMY_SP};

use crate::utils::unwrap_as;

pub fn process(ast: &mut Program, program_data: &mut ProgramData) {
    let mut visitor = Fuse { program_data };
    ast.visit_mut_with(&mut visitor);
}

struct Fuse<'a> {
    program_data: &'a mut ProgramData,
}

impl VisitMut<'_> for Fuse<'_> {
    fn visit_mut_block_stmt(&mut self, node: &mut BlockStmt) {
        node.visit_mut_children_with(self);

        if !can_fuse_into_one_statement(node) {
            return;
        }

        let seq = fuse_statements(node, self.program_data);
        let last = node.stmts.last_mut().unwrap();
        fuse_expression_into_control_flow_statement(seq, last, self.program_data);
    }

    fn visit_mut_function(&mut self, node: &mut Function) {
        node.params.visit_mut_with(self);
        // Skip the BlockStmt visitor.
        node.body.visit_mut_children_with(self);
    }
    fn visit_mut_constructor(&mut self, node: &mut Constructor) {
        node.params.visit_mut_with(self);
        // Skip the BlockStmt visitor.
        node.body.visit_mut_children_with(self);
    }
    fn visit_mut_arrow_expr(&mut self, node: &mut ArrowExpr) {
        node.params.visit_mut_with(self);
        // Skip the BlockStmt visitor.
        node.body.visit_mut_children_with(self);
    }
    fn visit_mut_getter_prop(&mut self, node: &mut GetterProp) {
        node.key.visit_mut_with(self);
        // Skip the BlockStmt visitor.
        node.body.visit_mut_children_with(self);
    }
    fn visit_mut_setter_prop(&mut self, node: &mut SetterProp) {
        node.key.visit_mut_with(self);
        node.param.visit_mut_with(self);
        // Skip the BlockStmt visitor.
        node.body.visit_mut_children_with(self);
    }
}

fn can_fuse_into_one_statement(block: &BlockStmt) -> bool {
    if block.stmts.len() < 2 {
        // Nothing to do.
        return false;
    }

    if block
        .stmts
        .iter()
        .rev()
        .skip(1)
        .any(|s| !matches!(s, Stmt::Expr(_)))
    {
        return false;
    }

    is_fusable_control_statement(block.stmts.last().unwrap())
}

fn is_fusable_control_statement(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::If(_) | Stmt::Throw(_) | Stmt::Switch(_) | Stmt::Expr(_) => true,
        // We don't want to add a new return value.
        Stmt::Return(r) => r.arg.is_some(),
        // Avoid cases where we have for(var x;_;_) { ....
        Stmt::For(f) => !matches!(f.init, Some(VarDeclOrExpr::VarDecl(_))),
        // Avoid cases where we have for(var x = foo() in a) { ....
        // TODO: make this more precise:
        // Stmt::ForIn(f) => !may_have_side_effects(&f.left),
        Stmt::ForIn(f) => matches!(&f.left, VarDeclOrPat::Pat(Pat::Ident(_))),
        Stmt::Labeled(l) => is_fusable_control_statement(&l.body),
        Stmt::Block(b) => b
            .stmts
            .first()
            .map(is_fusable_control_statement)
            .unwrap_or_default(),
        _ => false,
    }
}

/// Given a block, fuse a list of statements with comma's.
fn fuse_statements(block: &mut BlockStmt, program_data: &mut ProgramData) -> SeqExpr {
    let exprs = block
        .stmts
        .drain(..(block.stmts.len() - 1))
        .map(|s| unwrap_as!(s, Stmt::Expr(e), e).expr)
        .collect();
    SeqExpr {
        node_id: program_data.new_id(DUMMY_SP),
        exprs,
    }
}

fn fuse_expression_into_control_flow_statement(
    seq: SeqExpr,
    last: &mut Stmt,
    program_data: &mut ProgramData,
) {
    // Now we are just left with two statements. The comma tree of the first
    // n - 1 statements (which can be used in an expression) and the last
    // statement. We perform specific fusion based on the last statement's type.
    match last {
        Stmt::If(IfStmt { test: expr, .. })
        | Stmt::Return(ReturnStmt {
            arg: Some(expr), ..
        })
        | Stmt::Throw(ThrowStmt { arg: expr, .. })
        | Stmt::Switch(SwitchStmt {
            discriminant: expr, ..
        })
        | Stmt::Expr(ExprStmt { expr, .. })
        | Stmt::For(ForStmt {
            init: Some(VarDeclOrExpr::Expr(expr)),
            ..
        }) => {
            fuse_exprs(seq, expr);
        }
        Stmt::For(f @ ForStmt { init: None, .. }) => {
            f.init = Some(VarDeclOrExpr::Expr(Box::new(Expr::Seq(seq))));
        }
        Stmt::ForIn(f) => {
            fuse_exprs(seq, &mut f.right);
        }
        Stmt::Labeled(l) => {
            fuse_expression_into_control_flow_statement(seq, &mut l.body, program_data);
        }
        Stmt::Block(b) => {
            if let Some(first) = b.stmts.first_mut() {
                fuse_expression_into_control_flow_statement(seq, first, program_data);
            } else {
                b.stmts.push(Stmt::Expr(ExprStmt {
                    node_id: program_data.new_id(DUMMY_SP),
                    expr: Box::new(Expr::Seq(seq)),
                }));
            }
        }
        _ => unreachable!("{:?}", last),
    }
}

fn fuse_exprs(mut seq: SeqExpr, other: &mut Box<Expr>) {
    seq.exprs.push(other.take());
    *other.as_mut() = Expr::Seq(seq);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::resolver;
    use global_common::{Globals, Mark, GLOBALS};

    #[test]
    fn test_nothing_to_do() {
        fuse_same("");
        fuse_same("a");
        fuse_same("a()");
        fuse_same("if(a()){}");
    }

    #[test]
    fn test_fold_block_with_statements() {
        fuse("a;b;c", "a,b,c");
        fuse("a();b();c();", "a(),b(),c()");
        fuse("a(),b();c(),d()", "a(),b(),c(),d()");
        fuse("a();b(),c(),d()", "a(),b(),c(),d()");
        fuse("a(),b(),c();d()", "a(),b(),c(),d()");
    }

    #[test]
    fn test_fold_block_into_if() {
        fuse("a;b;c;if(x){}", "if(a,b,c,x){}");
        fuse("a;b;c;if(x,y){}else{}", "if(a,b,c,x,y){}else{}");
        fuse("a;b;c;if(x,y){}", "if(a,b,c,x,y){}");
        fuse("a;b;c;if(x,y,z){}", "if(a,b,c,x,y,z){}");

        // Can't fuse if there are statements after the IF.
        fuse_same("a();if(a()){}a()");
    }

    #[test]
    fn test_fold_block_return() {
        fuse("a;b;c;return x", "return a,b,c,x");
        fuse("a;b;c;return x+y", "return a,b,c,x+y");

        // DeadAssignmentElimination would have cleaned it up anyway.
        fuse_same("a;b;c;return x;a;b;c");
    }

    #[test]
    fn test_fold_block_throw() {
        fuse("a;b;c;throw x", "throw a,b,c,x");
        fuse("a;b;c;throw x+y", "throw a,b,c,x+y");
        fuse_same("a;b;c;throw x;a;b;c");
    }

    #[test]
    fn test_fold_switch() {
        fuse("a;b;c;switch(x){}", "switch(a,b,c,x){}");
    }

    #[test]
    fn test_fuse_into_for_in() {
        fuse("a;b;c;for(x in y){}", "for(x in a,b,c,y){}");
    }

    #[test]
    fn test_fuse_into_vanilla_for1() {
        fuse("a;b;c;for(;g;){}", "for(a,b,c;g;){}");
        fuse("a;b;c;for(d;g;){}", "for(a,b,c,d;g;){}");
        fuse("a;b;c;for(d,e;g;){}", "for(a,b,c,d,e;g;){}");
        fuse_same("a();for(var x;g;){}");
    }

    #[test]
    fn test_fuse_into_vanilla_for2() {
        fuse_same("a;b;c;for(var d;g;){}");
        fuse_same("a;b;c;for(let d;g;){}");
        fuse_same("a;b;c;for(const d = 5;g;){}");
    }

    #[test]
    fn test_fuse_into_label() {
        fuse("a;b;c;label:for(x in y){}", "label:for(x in a,b,c,y){}");
        fuse("a;b;c;label:for(;g;){}", "label:for(a,b,c;g;){}");
        fuse("a;b;c;l1:l2:l3:for(;g;){}", "l1:l2:l3:for(a,b,c;g;){}");
        fuse_same("a;b;c;label:while(true){}");
    }

    #[test]
    fn test_fuse_into_block() {
        fuse("a;b;c;{d;e;f}", "{a,b,c,d,e,f}");
        fuse(
            "a;b; label: { if(q) break label; bar(); }",
            "label: { if(a,b,q) break label; bar(); }",
        );
        fuse_same("a;b;c;{var x;d;e;}");
        fuse_same("a;b;c;label:{break label;d;e;}");

        test_transform(
            "function f(a) { if (COND) { a; { b; let a = 1; } } }",
            "function f(a) { if (COND) { { a,b; let a = 1; } } }",
        );
        test_transform(
            "function f(a) { if (COND) { a; { b; let otherVariable = 1; } } }",
            "function f(a) { if (COND) {  { a,b; let otherVariable = 1; } } }",
        );
    }

    #[test]
    fn test_no_fuse_into_while() {
        fuse_same("a;b;c;while(x){}");
    }

    #[test]
    fn test_no_fuse_into_do() {
        fuse_same("a;b;c;do{}while(x)");
    }

    #[test]
    fn test_no_global_scope_changes() {
        test_same("a,b,c");
    }

    #[test]
    fn test_no_function_block_changes() {
        test_same("function foo() { a,b,c }");
    }

    fn test_transform(input: &str, expected: &str) {
        crate::testing::test_transform(
            |mut program, program_data| {
                GLOBALS.set(&Globals::new(), || {
                    let unresolved_mark = Mark::new();
                    let top_level_mark = Mark::new();

                    program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

                    process(&mut program, program_data);

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

    fn fuse(before: &str, after: &str) {
        test_transform(
            &format!("function F(){{if(CONDITION){{{before}}}}}"),
            &format!("function F(){{if(CONDITION){{{after}}}}}"),
        );
    }
    fn fuse_same(code: &str) {
        test_same(&format!("function F(){{if(CONDITION){{{code}}}}}"));
    }
}
