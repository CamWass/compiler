use ecma_visit::{noop_visit_mut_type, VisitMut, VisitMutWith};

pub struct Normalize {}

impl VisitMut for Normalize {
    noop_visit_mut_type!();

    fn visit_mut_prop(&mut self, prop: &mut ast::Prop) {
        prop.visit_mut_children_with(self);

        if let ast::Prop::Shorthand(ast::Ident { sym, span, .. }) = prop {
            todo!();
            // *prop = ast::Prop::KeyValue(ast::KeyValueProp {
            //     key: ast::PropName::Ident(ast::Ident::new(sym.clone(), *span)),
            //     value: Box::new(ast::Ident::new(sym.clone(), *span).into()),
            // });
        }
    }
}
