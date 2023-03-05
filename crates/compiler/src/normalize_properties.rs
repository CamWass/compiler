use ast::*;
use ecma_visit::{noop_visit_mut_type, VisitMut, VisitMutWith};
use global_common::util::take::Take;

use crate::utils::unwrap_as;

/// Should be run before most other passes.
pub fn normalize_properties(ast: &mut Program, node_id_gen: &mut ast::NodeIdGen) {
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
                    key: PropName::Ident(Ident {
                        node_id: self.node_id_gen.next(),
                        optional: false,
                        span: id.span,
                        sym: id.sym.clone(),
                    }),
                    value: Box::new(Expr::Ident(Ident {
                        node_id: self.node_id_gen.next(),
                        optional: false,
                        span: id.span,
                        sym: id.sym,
                    })),
                })
            });
        }
    }

    fn visit_mut_object_pat_prop(&mut self, node: &mut ObjectPatProp) {
        node.visit_mut_children_with(self);

        // Maps:
        // (1) `let { a = b } = obj` ---> `let { a: a = b } = obj`
        // (2) `let { a } = obj`     ---> `let { a: a } = obj`

        if matches!(node, ObjectPatProp::Assign(p)) {
            node.map_with_mut(|prop| {
                let prop = unwrap_as!(prop, ObjectPatProp::Assign(p), p);

                if let Some(value) = prop.value {
                    // (1):

                    let assign_pat = AssignPat {
                        node_id: self.node_id_gen.next(),
                        span: prop.span,
                        left: Box::new(Pat::Ident(BindingIdent {
                            node_id: self.node_id_gen.next(),
                            id: Ident {
                                node_id: self.node_id_gen.next(),
                                optional: false,
                                span: prop.key.span,
                                sym: prop.key.sym.clone(),
                            },
                            type_ann: None,
                        })),
                        right: value,
                        type_ann: None,
                    };

                    ObjectPatProp::KeyValue(KeyValuePatProp {
                        node_id: self.node_id_gen.next(),
                        key: PropName::Ident(Ident {
                            node_id: self.node_id_gen.next(),
                            optional: false,
                            span: prop.key.span,
                            sym: prop.key.sym,
                        }),
                        value: Box::new(Pat::Assign(assign_pat)),
                    })
                } else {
                    // (2):

                    ObjectPatProp::KeyValue(KeyValuePatProp {
                        node_id: self.node_id_gen.next(),
                        key: PropName::Ident(Ident {
                            node_id: self.node_id_gen.next(),
                            optional: false,
                            span: prop.key.span,
                            sym: prop.key.sym.clone(),
                        }),
                        value: Box::new(Pat::Ident(BindingIdent {
                            node_id: self.node_id_gen.next(),
                            id: Ident {
                                node_id: self.node_id_gen.next(),
                                optional: false,
                                span: prop.key.span,
                                sym: prop.key.sym,
                            },
                            type_ann: None,
                        })),
                    })
                }
            });
        }
    }
}
