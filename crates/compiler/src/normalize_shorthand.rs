use ast::*;
use ecma_visit::{noop_visit_mut_type, VisitMut, VisitMutWith};
use global_common::util::take::Take;

use crate::utils::unwrap_as;

/// Should be run before most other passes.
pub fn normalize_shorthand(ast: &mut Program, node_id_gen: &mut ast::NodeIdGen) {
    let mut v = NormalizeShortHand { node_id_gen };
    ast.visit_mut_with(&mut v);
}

struct NormalizeShortHand<'a> {
    node_id_gen: &'a mut NodeIdGen,
}

impl VisitMut<'_> for NormalizeShortHand<'_> {
    noop_visit_mut_type!();

    fn visit_mut_prop(&mut self, prop: &mut Prop) {
        prop.visit_mut_children_with(self);

        if matches!(prop, Prop::Shorthand(_)) {
            prop.map_with_mut(|prop| {
                let id = unwrap_as!(prop, Prop::Shorthand(p), p);
                Prop::KeyValue(KeyValueProp {
                    node_id: self.node_id_gen.next(),
                    key: PropName::Ident(id.clone()),
                    value: Box::new(Expr::Ident(Ident {
                        node_id: self.node_id_gen.next(),
                        ..id
                    })),
                })
            });
        }
    }

    fn visit_mut_object_pat_prop(&mut self, node: &mut ObjectPatProp) {
        node.visit_mut_children_with(self);

        if matches!(node, ObjectPatProp::Assign(p) if p.value.is_none()) {
            node.map_with_mut(|prop| {
                let prop = unwrap_as!(prop, ObjectPatProp::Assign(p), p);

                ObjectPatProp::KeyValue(KeyValuePatProp {
                    node_id: self.node_id_gen.next(),
                    key: PropName::Ident(prop.key.clone()),
                    value: Box::new(Pat::Ident(BindingIdent {
                        node_id: self.node_id_gen.next(),
                        id: Ident {
                            node_id: self.node_id_gen.next(),
                            ..prop.key
                        },
                        type_ann: None,
                    })),
                })
            });
        }
    }
}
