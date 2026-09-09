use crate::{
    AssignTarget, GetNodeId, NodeId, ProgramData, SimpleAssignTarget, expr::Expr,
    ident::BindingIdent, prop::PropName,
};
use clone_node::CloneNode;
use node_eq::NodeEq;
use node_id::GetNodeIdMacro;
use serde::Serialize;

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub enum BindingPat {
    Array(ArrayBindingPat),
    Object(ObjectBindingPat),
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct ArrayBindingPat {
    pub node_id: NodeId,

    pub elems: Vec<Option<BindingElement>>,
    pub rest: Option<BindingRestElement>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct ObjectBindingPat {
    pub node_id: NodeId,

    pub props: Vec<BindingProperty>,
    pub rest: Option<BindingRestProperty>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct BindingProperty {
    pub node_id: NodeId,

    pub prop: PropName,
    pub target: Box<BindingElement>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct BindingRestProperty {
    pub node_id: NodeId,

    pub arg: Box<BindingIdent>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct BindingRestElement {
    pub node_id: NodeId,

    pub arg: Box<BindingPatOrIdent>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct BindingElement {
    pub node_id: NodeId,

    pub target: BindingPatOrIdent,
    pub init: Option<Box<Expr>>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub enum BindingPatOrIdent {
    Array(ArrayBindingPat),
    Object(ObjectBindingPat),
    Ident(BindingIdent),
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub enum AssignmentPat {
    Array(ArrayAssignmentPat),
    Object(ObjectAssignmentPat),
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct ArrayAssignmentPat {
    pub node_id: NodeId,

    pub elems: Vec<Option<AssignmentElement>>,
    pub rest: Option<AssignmentRest>,
}

impl ArrayAssignmentPat {
    pub fn from_array_binding_pat(
        array_binding_pat: ArrayBindingPat,
        program_data: &mut ProgramData,
    ) -> Self {
        ArrayAssignmentPat {
            node_id: program_data.new_id_from(array_binding_pat.node_id),
            elems: array_binding_pat
                .elems
                .into_iter()
                .map(|elem| {
                    elem.map(|elem| AssignmentElement::from_binding_element(elem, program_data))
                })
                .collect(),
            rest: array_binding_pat.rest.map(|rest| AssignmentRest {
                node_id: program_data.new_id_from(rest.node_id),
                arg: Box::new(AssignTarget::from_binding_pat_or_ident(
                    *rest.arg,
                    program_data,
                )),
            }),
        }
    }
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct ObjectAssignmentPat {
    pub node_id: NodeId,

    pub props: Vec<AssignmentProperty>,
    pub rest: Option<AssignmentRest>,
}

impl ObjectAssignmentPat {
    pub fn from_object_binding_pat(
        object_binding_pat: ObjectBindingPat,
        program_data: &mut ProgramData,
    ) -> Self {
        ObjectAssignmentPat {
            node_id: program_data.new_id_from(object_binding_pat.node_id),
            props: object_binding_pat
                .props
                .into_iter()
                .map(|prop| AssignmentProperty {
                    node_id: program_data.new_id_from(prop.node_id),
                    prop: prop.prop,
                    target: AssignmentElement::from_binding_element(*prop.target, program_data),
                })
                .collect(),
            rest: object_binding_pat.rest.map(|rest| AssignmentRest {
                node_id: program_data.new_id_from(rest.node_id),
                arg: Box::new(AssignTarget::Simple(SimpleAssignTarget::Ident(*rest.arg))),
            }),
        }
    }
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct AssignmentProperty {
    pub node_id: NodeId,

    pub prop: PropName,
    pub target: AssignmentElement,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct AssignmentRest {
    pub node_id: NodeId,

    pub arg: Box<AssignTarget>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct AssignmentElement {
    pub node_id: NodeId,

    pub target: Box<AssignTarget>,
    pub init: Option<Box<Expr>>,
}

impl AssignmentElement {
    pub fn from_binding_element(
        binding_element: BindingElement,
        program_data: &mut ProgramData,
    ) -> Self {
        match binding_element.target {
            BindingPatOrIdent::Array(array_binding_pat) => AssignmentElement {
                node_id: program_data.new_id_from(binding_element.node_id),
                target: Box::new(AssignTarget::AssignmentPat(AssignmentPat::Array(
                    ArrayAssignmentPat::from_array_binding_pat(array_binding_pat, program_data),
                ))),
                init: binding_element.init,
            },
            BindingPatOrIdent::Object(object_binding_pat) => AssignmentElement {
                node_id: program_data.new_id_from(binding_element.node_id),
                target: Box::new(AssignTarget::AssignmentPat(AssignmentPat::Object(
                    ObjectAssignmentPat::from_object_binding_pat(object_binding_pat, program_data),
                ))),
                init: binding_element.init,
            },
            BindingPatOrIdent::Ident(binding_ident) => AssignmentElement {
                node_id: program_data.new_id_from(binding_element.node_id),
                target: Box::new(AssignTarget::Simple(SimpleAssignTarget::Ident(
                    binding_ident,
                ))),
                init: binding_element.init,
            },
        }
    }
}
