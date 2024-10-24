use super::*;
use ecma_visit::{Visit, VisitWith};
use global_common::Span;

impl<I: Tokens> Parser<I> {
    pub(in crate::parser) fn verify_expr(&mut self, expr: &Expr) {
        let mut v = Verifier {
            errors: vec![],
            parser: self,
        };

        v.visit_expr(expr);

        for (span, error) in v.errors {
            self.emit_err(span, error);
        }
    }
}

pub(super) struct Verifier<'a, I: Tokens> {
    pub errors: Vec<(Span, SyntaxError)>,
    parser: &'a Parser<I>,
}

impl<I: Tokens> Visit<'_> for Verifier<'_, I> {
    fn visit_assign_prop(&mut self, p: &AssignProp) {
        self.errors.push((
            get_span!(self.parser, p.node_id),
            SyntaxError::AssignProperty,
        ));
    }

    fn visit_expr(&mut self, e: &Expr) {
        match *e {
            Expr::Fn(..) | Expr::Arrow(..) => {}
            _ => e.visit_children_with(self),
        }
    }
}
