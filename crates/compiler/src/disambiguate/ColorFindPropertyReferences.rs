use std::rc::Rc;

use super::ColorGraphNode::PropAssociation;
use super::ColorGraphNodeFactory::ColorGraphNodeFactory;
use super::PropertyClustering::{PropertyClustering, PropertyClusteringId};
use crate::colors::color_registry::ColorRegistry;
use crate::colors::ColorId;
use crate::node::{Bind, BoundNode};
use crate::types::TypeId;
use crate::utils::{isBindableObjectDefinePropertyCall, unwrap_as};
use crate::visit::{Visit, VisitWith};
use crate::{ast, CompProgram};
use ahash::AHashMap;
use index::vec::IndexVec;
use std::fmt::Debug;
use swc_atoms::JsWord;

pub struct ColorFindPropertyReferences<'col, 'nf> {
    propIndex: AHashMap<JsWord, PropertyClusteringId>,
    colorGraphNodeFactory: &'nf mut ColorGraphNodeFactory<'col>,
    propertyClusterings: IndexVec<PropertyClusteringId, PropertyClustering>,
}

impl<'col, 'nf> ColorFindPropertyReferences<'col, 'nf> {
    pub fn find_prop_references(
        colorGraphNodeFactory: &'nf mut ColorGraphNodeFactory<'col>,
        program: &CompProgram,
    ) -> (
        AHashMap<JsWord, PropertyClusteringId>,
        IndexVec<PropertyClusteringId, PropertyClustering>,
    ) {
        let mut prop_ref_finder = Self {
            propIndex: AHashMap::default(),
            colorGraphNodeFactory,
            propertyClusterings: IndexVec::default(),
        };

        program.visit_with(&mut prop_ref_finder, None);

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
    fn registerPropertyUse(&mut self, site: &ast::Ident, owner: Option<ColorId>) {
        let prop = match self.propIndex.get(&site.sym) {
            Some(p) => *p,
            None => {
                let clustering = self.new_property_clustering(site.sym.clone());
                self.propIndex.insert(site.sym.clone(), clustering);
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
            .insert(site.node_id, flatOwner);

        // TODO:
        // Track the cluster of types whose properties must keep their original name after
        // disambiguation. Note: an "enum type" is the type of an enum object like "{STOP: 0, GO: 1}".
        // if site.isFromExterns()
        // {
        //     self.propertyClusterings[prop].registerOriginalNameType(flatOwner);
        // }
    }
}

// TODO: noop for types
impl Visit for ColorFindPropertyReferences<'_, '_> {
    fn visit_member_expr(&mut self, node: &Rc<ast::MemberExpr>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent.clone());
        if !node.computed {
            let bound_node = node.bind_to_opt_parent(parent);
            let prop = unwrap_as!(&node.prop, ast::Expr::Ident(i), i);
            let obj = node.obj.bind(bound_node.clone());
            let owner = self.colorGraphNodeFactory.colours.get_color_of_node(&obj);
            self.registerPropertyUse(&prop, owner);
        }
    }

    fn visit_object_lit(&mut self, node: &Rc<ast::ObjectLit>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent.clone());
        if let Some(BoundNode::CallExpr(c)) = parent {
            todo!();
        }
        let bound_node = node.bind_to_opt_parent(parent);
        let owner = self
            .colorGraphNodeFactory
            .colours
            .get_color_of_node(&bound_node);
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
                self.registerPropertyUse(i, owner);
            }
        }
    }

    fn visit_class(&mut self, node: &Rc<ast::Class>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent.clone());
        let class = node.bind_to_opt_parent(parent);
        let class_decl = class.parent().clone().unwrap();
        let class_color = self
            .colorGraphNodeFactory
            .colours
            .get_color_of_node(&class_decl)
            .unwrap();
        for member in &node.body {
            let res = match member {
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

            if let Some((id, is_static)) = res {
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
                self.registerPropertyUse(id, Some(owner));
            }
        }
        self.colorGraphNodeFactory.createNode(Some(class_color));
    }

    fn visit_call_expr(&mut self, node: &Rc<ast::CallExpr>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent);

        if isBindableObjectDefinePropertyCall(node.as_ref()) {
            todo!();
        }
    }

    fn visit_object_pat(&mut self, node: &Rc<ast::ObjectPat>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent);
        todo!();
    }

    // fn visit_function(&mut self, node: &Rc<ast::Function>, parent: Option<BoundNode>) {
    //     node.visit_children_with(self, parent.clone());
    //     // TODO:
    // }

    fn visit_ts_interface_decl(
        &mut self,
        node: &Rc<ast::TsInterfaceDecl>,
        parent: Option<BoundNode>,
    ) {
        node.visit_children_with(self, parent.clone());
        let interface = node.bind_to_opt_parent(parent);
        let interface_color = self
            .colorGraphNodeFactory
            .colours
            .get_color_of_node(&interface)
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
                self.registerPropertyUse(id, Some(interface_color));
            }
        }
        self.colorGraphNodeFactory.createNode(Some(interface_color));
    }
}
