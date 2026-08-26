use std::collections::hash_map::Entry;

use ast::*;
use common::util::take::Take;
use rustc_hash::FxHashMap;
use visit::{VisitMut, VisitMutWith};

/// Normalizes certain expressions, drops node IDs, and optionally drops spans.
pub struct Normalizer<'d> {
    pub drop_span: bool,
    pub is_test262: bool,
    pub other_program_data: &'d mut ParserProgramData,
    pub name_map: FxHashMap<String, NameId>,
    pub fresh_program_data: ParserProgramData,
}

impl VisitMut<'_> for Normalizer<'_> {
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
                let need_work = exprs.iter().any(|n| matches!(*n, Expr::Seq(..)));

                if need_work {
                    exprs = exprs.into_iter().fold(vec![], |mut v, e| {
                        match e {
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
        let Some(val) = val else { return };

        if let Some(value) = val.as_f64() {
            n.value = value;
        }
    }

    fn visit_mut_pat(&mut self, node: &mut Pat) {
        node.visit_mut_children_with(self);

        if let Pat::Expr(expr) = node {
            match *expr.take() {
                Expr::Ident(id) => {
                    *node = Pat::Ident(BindingIdent { id });
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
                    *node = PatOrExpr::Pat(Box::new(Pat::Ident(BindingIdent { id })));
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
        if !self.is_test262 {
            n.visit_mut_children_with(self);
            return;
        }

        match n {
            PropName::Ident(Ident { name, .. }) => {
                *n = PropName::Str(Str {
                    node_id: NodeId::DUMMY,
                    value: Box::new(self.other_program_data.get_name_text(*name).to_string()),
                });
            }
            PropName::Num(num) => {
                *n = PropName::Str(Str {
                    node_id: NodeId::DUMMY,
                    value: num.to_string().into(),
                });
            }
            _ => {}
        }
        n.visit_mut_children_with(self);
    }

    fn visit_mut_node_id(&mut self, node_id: &mut NodeId) {
        *node_id = NodeId::DUMMY;
    }

    fn visit_mut_name_id(&mut self, name_id: &mut NameId) {
        // We use the `other_program_data` (the one from the parser) to get the
        // text of the name, then we insert that into `fresh_program_data` to
        // get a canonical NameId.
        // This is necessary since e.g. some test262 tests require us to treat
        // Ident PropNames as String PropNames, but each version' AST has a
        // different number of Idents and thus different NameIds.
        match self
            .name_map
            .entry(self.other_program_data.get_name_text(*name_id).to_string())
        {
            Entry::Occupied(occupied_entry) => *name_id = *occupied_entry.get(),
            Entry::Vacant(vacant_entry) => {
                let new_name = self
                    .fresh_program_data
                    .intern_name(vacant_entry.key().into());
                vacant_entry.insert(new_name);
                *name_id = new_name;
            }
        }
    }

    fn visit_mut_str(&mut self, s: &mut Str) {
        s.node_id = NodeId::DUMMY;
    }
}
