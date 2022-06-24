use super::ColorGraphNode::PropAssociation;
use super::ColorGraphNodeFactory::ColorGraphNodeFactory;
use super::PropertyClustering::{PropertyClustering, PropertyClusteringId};
use crate::ast::{GetNodeId, NodeId};
use crate::colors::ColorId;
use crate::utils::unwrap_as;
use ecma_visit::{Visit, VisitWith};
use index::vec::IndexVec;
use rustc_hash::FxHashMap;
use swc_atoms::JsWord;

pub struct ColorFindPropertyReferences<'col, 'nf> {
    propIndex: FxHashMap<JsWord, PropertyClusteringId>,
    colorGraphNodeFactory: &'nf mut ColorGraphNodeFactory<'col>,
    propertyClusterings: IndexVec<PropertyClusteringId, PropertyClustering>,
}

impl<'col, 'nf> ColorFindPropertyReferences<'col, 'nf> {
    pub fn find_prop_references(
        colorGraphNodeFactory: &'nf mut ColorGraphNodeFactory<'col>,
        ast: &::ast::Program,
    ) -> (
        FxHashMap<JsWord, PropertyClusteringId>,
        IndexVec<PropertyClusteringId, PropertyClustering>,
    ) {
        let mut prop_ref_finder = Self {
            propIndex: FxHashMap::default(),
            colorGraphNodeFactory,
            propertyClusterings: IndexVec::default(),
        };

        ast.visit_with(&mut prop_ref_finder);

        (
            prop_ref_finder.propIndex,
            prop_ref_finder.propertyClusterings,
        )
    }

    fn new_property_clustering(&mut self, name: JsWord) -> PropertyClusteringId {
        let clustering = PropertyClustering::new(name);
        self.propertyClusterings.push(clustering)
    }

    /// Update all datastructures as necessary to consider property use `site` from type `owner`.
    fn registerPropertyUse(
        &mut self,
        ident_node_id: NodeId,
        ident_sym: &JsWord,
        owner: Option<ColorId>,
    ) {
        let prop = match self.propIndex.get(&ident_sym) {
            Some(p) => *p,
            None => {
                let clustering = self.new_property_clustering(ident_sym.clone());
                self.propIndex.insert(ident_sym.clone(), clustering);
                clustering
            }
        };
        let flatOwner = self.colorGraphNodeFactory.createNode(owner);

        // Set the initial condition for flowing this property along the graph.
        self.colorGraphNodeFactory[flatOwner]
            .associatedProps
            .insert(prop, PropAssociation::AST);
        // Make sure there's a cluster for this name/type combination.
        self.propertyClusterings[prop].clusters.add(flatOwner);
        // Record the site to rename once clusters are found. If it's an extern, we won't rename anyway.
        self.propertyClusterings[prop]
            .useSites
            .insert(ident_node_id, flatOwner);

        // TODO:
        // Track the cluster of types whose properties must keep their original name after
        // disambiguation. Note: an "enum type" is the type of an enum object like "{STOP: 0, GO: 1}".
        // if site.isFromExterns()
        // {
        //     self.propertyClusterings[prop].registerOriginalNameType(flatOwner);
        // }
    }

    fn handle_class(&mut self, class: &ast::Class, class_color: ColorId) {
        for member in &class.body {
            let prop = match member {
                ast::ClassMember::Constructor(_) => todo!(),
                ast::ClassMember::Method(m) => {
                    if let ast::PropName::Ident(i) = &m.key {
                        Some((i, m.is_static))
                    } else {
                        None
                    }
                }
                ast::ClassMember::PrivateMethod(m) => Some((&m.key.id, m.is_static)),
                ast::ClassMember::ClassProp(p) => {
                    if let ast::PropName::Ident(i) = &p.key {
                        Some((i, p.is_static))
                    } else {
                        None
                    }
                }
                ast::ClassMember::PrivateProp(p) => Some((&p.key.id, p.is_static)),
                ast::ClassMember::TsIndexSignature(_) | ast::ClassMember::Empty(_) => None,
            };

            if let Some((id, is_static)) = prop {
                let owner = if is_static {
                    self.colorGraphNodeFactory.colours.colors[class_color]
                        .staticType
                        .unwrap()
                } else {
                    class_color
                };
                // let owner = match member {
                //     ast::ClassMember::Constructor(_) => todo!(),
                //     ast::ClassMember::Method(_)
                //     | ast::ClassMember::PrivateMethod(_)
                //     | ast::ClassMember::ClassProp(_)
                //     | ast::ClassMember::PrivateProp(_) => class_color,
                //     _ => unreachable!(),
                // };
                self.registerPropertyUse(id.node_id, &id.sym, Some(owner));
            }
        }
        self.colorGraphNodeFactory.createNode(Some(class_color));
    }
}

// TODO: noop for types
impl Visit for ColorFindPropertyReferences<'_, '_> {
    fn visit_member_expr(&mut self, node: &ast::MemberExpr) {
        node.visit_children_with(self);
        if !node.computed {
            let prop = unwrap_as!(node.prop.as_ref(), ast::Expr::Ident(i), i);
            let owner = self
                .colorGraphNodeFactory
                .colours
                .get_color_of_node(node.obj.node_id());
            self.registerPropertyUse(prop.node_id, &prop.sym, owner);
        }
    }

    fn visit_object_lit(&mut self, node: &ast::ObjectLit) {
        node.visit_children_with(self);
        let owner = self
            .colorGraphNodeFactory
            .colours
            .get_color_of_node(node.node_id);
        for prop in &node.props {
            let key = match prop {
                ast::Prop::KeyValue(prop) => &prop.key,
                ast::Prop::Getter(prop) => &prop.key,
                ast::Prop::Setter(prop) => &prop.key,
                ast::Prop::Method(prop) => &prop.key,
                ast::Prop::Spread(_) => continue,
                ast::Prop::Assign(_) => unreachable!(),
                ast::Prop::Shorthand(_) => unreachable!("removed by normalization"),
            };

            if let ast::PropName::Ident(i) = key {
                self.registerPropertyUse(i.node_id, &i.sym, owner);
            }
        }
    }

    fn visit_class_decl(&mut self, node: &ast::ClassDecl) {
        node.visit_children_with(self);
        let class_color = self
            .colorGraphNodeFactory
            .colours
            .get_color_of_node(node.node_id)
            .unwrap();
        self.handle_class(&node.class, class_color);
    }

    fn visit_class_expr(&mut self, node: &ast::ClassExpr) {
        node.visit_children_with(self);
        let class_color = self
            .colorGraphNodeFactory
            .colours
            .get_color_of_node(node.node_id)
            .unwrap();
        self.handle_class(&node.class, class_color);
    }

    fn visit_call_expr(&mut self, node: &ast::CallExpr) {
        // TODO: objectDefineProperty
        // if isBindableObjectDefinePropertyCall(node.as_ref()) {
        //     node.type_args.visit_with(self);
        //     node.callee.visit_with(self);

        //     // Checked by isBindableObjectDefinePropertyCall
        //     assert!(node.args.len() == 3);
        //     // The obect the prop is defined on.
        //     node.args[0].visit_with(self);
        //     // Name of prop.
        //     node.args[1].visit_with(self);
        //     // The 3rd arg (the descriptor object) is handled below.
        //     todo!();
        // } else {
        node.visit_children_with(self);
        // }
    }

    fn visit_object_pat(&mut self, node: &ast::ObjectPat) {
        node.visit_children_with(self);
        todo!();
    }

    // fn visit_function(&mut self, node: &Rc<ast::Function>) {
    //     node.visit_children_with(self);
    //     // TODO:
    // }

    fn visit_ts_interface_decl(&mut self, node: &ast::TsInterfaceDecl) {
        node.visit_children_with(self);
        let interface_color = self
            .colorGraphNodeFactory
            .colours
            .get_color_of_node(node.node_id)
            .unwrap();
        for element in &node.body.body {
            let key = match element {
                ast::TsTypeElement::TsPropertySignature(n) => Some(&n.key),
                ast::TsTypeElement::TsGetterSignature(n) => Some(&n.key),
                ast::TsTypeElement::TsSetterSignature(n) => Some(&n.key),
                ast::TsTypeElement::TsMethodSignature(n) => Some(&n.key),
                ast::TsTypeElement::TsCallSignatureDecl(_)
                | ast::TsTypeElement::TsConstructSignatureDecl(_) => None,
                ast::TsTypeElement::TsIndexSignature(n) => todo!(),
            };

            if let Some(ast::PropName::Ident(id)) = key {
                self.registerPropertyUse(id.node_id, &id.sym, Some(interface_color));
            }
        }
        self.colorGraphNodeFactory.createNode(Some(interface_color));
    }
}

// TODO: tests from closure
