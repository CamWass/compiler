use super::*;
use ecma_visit::{noop_visit_type, Node, Visit, VisitWith};
use global_common::{Span, Spanned, DUMMY_SP};

impl<'ast, I: Tokens> Parser<'ast, I> {
    pub(in crate::parser) fn verify_expr(&mut self, expr: Expr<'ast>) -> Expr<'ast> {
        // let mut v = Verifier { errors: vec![] };

        // v.visit_expr(&expr, &Invalid { span: DUMMY_SP } as _);

        // for (span, error) in v.errors {
        //     self.emit_err(span, error);
        // }

        todo!();

        expr
    }
}

// pub(super) struct Verifier {
//     pub errors: Vec<(Span, SyntaxError)>,
// }

// impl Visit for Verifier {
//     noop_visit_type!();

//     fn visit_assign_prop(&mut self, p: &AssignProp, _: &dyn Node) {
//         self.errors.push((p.span(), SyntaxError::AssignProperty));
//     }

//     fn visit_expr(&mut self, e: &Expr, _: &dyn Node) {
//         match *e {
//             Expr::Fn(..) | Expr::Arrow(..) => {}
//             _ => e.visit_children_with(self),
//         }
//     }
// }
