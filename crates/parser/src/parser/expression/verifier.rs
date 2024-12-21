use super::*;

impl<I: Tokens> Parser<'_, I> {
    pub(in crate::parser) fn verify_expr(&mut self, expr: &Expr) {
        if let Expr::Object(e) = expr {
            for prop in &e.props {
                if let Prop::Assign(prop) = prop {
                    self.emit_err(get_span!(self, prop.node_id), SyntaxError::AssignProperty);
                }
            }
        }
    }
}
