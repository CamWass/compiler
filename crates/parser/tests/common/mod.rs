use ast::*;
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::util::take::Take;

/// Normalizes certain expressions, drops node IDs, and optionally drops spans.
pub struct Normalizer {
    pub drop_span: bool,
    pub is_test262: bool,
}

impl VisitMut<'_> for Normalizer {
    fn visit_mut_expr(&mut self, e: &mut Expr) {
        e.visit_mut_children_with(self);

        match e.take() {
            Expr::New(n @ NewExpr { args: None, .. }) if self.is_test262 => {
                *e = Expr::New(NewExpr {
                    args: Some(vec![]),
                    ..n
                });
            }
            // Flatten comma expressions.
            Expr::Seq(SeqExpr { mut exprs, .. }) => {
                let need_work = exprs.iter().any(|n| matches!(**n, Expr::Seq(..)));

                if need_work {
                    exprs = exprs.into_iter().fold(vec![], |mut v, e| {
                        match *e {
                            Expr::Seq(SeqExpr { exprs, .. }) => v.extend(exprs),
                            _ => v.push(e),
                        }
                        v
                    });
                }
                *e = Expr::Seq(SeqExpr {
                    node_id: NodeId::DUMMY,
                    exprs,
                });
            }
            expr => *e = expr,
        }
    }

    fn visit_mut_number(&mut self, n: &mut Number) {
        n.visit_mut_children_with(self);

        let val = serde_json::Number::from_f64(n.value);
        let val = match val {
            Some(v) => v,
            None => return,
        };

        match val.as_f64() {
            Some(value) => {
                n.value = value;
            }
            None => {}
        }
    }

    fn visit_mut_pat(&mut self, node: &mut Pat) {
        node.visit_mut_children_with(self);

        if let Pat::Expr(expr) = node {
            match *expr.take() {
                Expr::Ident(id) => {
                    *node = Pat::Ident(BindingIdent {
                        node_id: NodeId::DUMMY,
                        id,
                    });
                }
                expr => {
                    *node = Pat::Expr(Box::new(expr));
                }
            }
        }
    }

    fn visit_mut_pat_or_expr(&mut self, node: &mut PatOrExpr) {
        node.visit_mut_children_with(self);

        match node {
            PatOrExpr::Expr(expr) => match *expr.take() {
                Expr::Ident(id) => {
                    *node = PatOrExpr::Pat(Box::new(Pat::Ident(BindingIdent {
                        node_id: NodeId::DUMMY,
                        id,
                    })))
                }
                expr => *node = PatOrExpr::Expr(Box::new(expr)),
            },
            PatOrExpr::Pat(pat) => match *pat.take() {
                Pat::Expr(expr) => *node = PatOrExpr::Expr(expr),
                pat => *node = PatOrExpr::Pat(Box::new(pat)),
            },
        }
    }

    fn visit_mut_prop_name(&mut self, n: &mut PropName) {
        n.visit_mut_children_with(self);

        if !self.is_test262 {
            return;
        }

        match n {
            PropName::Ident(Ident { sym, .. }) => {
                *n = PropName::Str(Str {
                    node_id: NodeId::DUMMY,
                    value: sym.clone(),
                    has_escape: false,
                })
            }
            PropName::Num(num) => {
                *n = PropName::Str(Str {
                    node_id: NodeId::DUMMY,
                    value: num.to_string().into(),
                    has_escape: false,
                })
            }
            _ => {}
        }
    }

    fn visit_mut_node_id(&mut self, node_id: &mut NodeId) {
        *node_id = NodeId::DUMMY;
    }

    fn visit_mut_str(&mut self, s: &mut Str) {
        if self.is_test262 {
            s.has_escape = false;
        }
        s.node_id = NodeId::DUMMY;
    }
}
