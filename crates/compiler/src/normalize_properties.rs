use ast::*;
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::{util::take::Take, SyntaxContext};

use crate::utils::unwrap_as;

/// Should be run before most other passes.
pub fn normalize_properties(ast: &mut Program, program_data: &mut ast::ProgramData) {
    let mut v = NormalizeShortHand { program_data };
    ast.visit_mut_with(&mut v);
}

struct NormalizeShortHand<'a> {
    program_data: &'a mut ProgramData,
}

impl VisitMut<'_> for NormalizeShortHand<'_> {
    fn visit_mut_prop(&mut self, prop: &mut Prop) {
        prop.visit_mut_children_with(self);

        if matches!(prop, Prop::Shorthand(_)) {
            prop.map_with_mut(|prop| {
                let id = unwrap_as!(prop, Prop::Shorthand(p), p);
                Prop::KeyValue(KeyValueProp {
                    node_id: self.program_data.new_id_from(id.node_id),
                    key: PropName::Ident(Ident {
                        node_id: self.program_data.new_id_from(id.node_id),
                        sym: id.sym.clone(),
                        ctxt: id.ctxt,
                    }),
                    value: Box::new(Expr::Ident(Ident {
                        node_id: self.program_data.new_id_from(id.node_id),
                        sym: id.sym,
                        ctxt: SyntaxContext::empty(),
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

        if matches!(node, ObjectPatProp::Assign(_)) {
            node.map_with_mut(|prop| {
                let prop = unwrap_as!(prop, ObjectPatProp::Assign(p), p);

                if let Some(value) = prop.value {
                    // (1):

                    let assign_pat = AssignPat {
                        node_id: self.program_data.new_id_from(prop.node_id),
                        left: Box::new(Pat::Ident(BindingIdent {
                            node_id: self.program_data.new_id_from(prop.key.node_id),
                            id: Ident {
                                node_id: self.program_data.new_id_from(prop.key.node_id),
                                sym: prop.key.sym.clone(),
                                ctxt: prop.key.ctxt,
                            },
                        })),
                        right: value,
                    };

                    ObjectPatProp::KeyValue(KeyValuePatProp {
                        node_id: self.program_data.new_id_from(prop.node_id),
                        key: PropName::Ident(Ident {
                            node_id: self.program_data.new_id_from(prop.key.node_id),
                            sym: prop.key.sym,
                            ctxt: SyntaxContext::empty(),
                        }),
                        value: Box::new(Pat::Assign(assign_pat)),
                    })
                } else {
                    // (2):

                    ObjectPatProp::KeyValue(KeyValuePatProp {
                        node_id: self.program_data.new_id_from(prop.node_id),
                        key: PropName::Ident(Ident {
                            node_id: self.program_data.new_id_from(prop.key.node_id),
                            sym: prop.key.sym.clone(),
                            ctxt: SyntaxContext::empty(),
                        }),
                        value: Box::new(Pat::Ident(BindingIdent {
                            node_id: self.program_data.new_id_from(prop.key.node_id),
                            id: Ident {
                                node_id: self.program_data.new_id_from(prop.key.node_id),
                                sym: prop.key.sym,
                                ctxt: prop.key.ctxt,
                            },
                        })),
                    })
                }
            });
        }
    }
}
