use super::*;
use ecma_visit::{noop_visit_type, Visit, VisitWith};
use global_common::{Span, Spanned};

impl<I: Tokens> Parser<I> {
    pub(in crate::parser) fn verify_expr(&mut self, expr: Box<Expr>) -> Box<Expr> {
        let mut v = Verifier { errors: vec![] };

        v.visit_expr(&expr);

        for (span, error) in v.errors {
            self.emit_err(span, error);
        }

        expr
    }
}

pub(super) struct Verifier {
    pub errors: Vec<(Span, SyntaxError)>,
}

impl Visit<'_> for Verifier {
    noop_visit_type!();

    fn visit_assign_prop(&mut self, p: &AssignProp) {
        self.errors.push((p.span(), SyntaxError::AssignProperty));
    }

    fn visit_expr(&mut self, e: &Expr) {
        match *e {
            Expr::Fn(..) | Expr::Arrow(..) => {}
            _ => e.visit_children_with(self),
        }
    }
}
