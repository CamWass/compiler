use crate::{
    expr::Expr,
    function::Function,
    ident::Ident,
    lit::{Number, Str},
    stmt::BlockStmt,
    GetNodeId, NodeId, ParamWithoutDecorators,
};
use ast_node::ast_node;
use atoms::js_word;
use global_common::{util::take::Take, SyntaxContext};

#[ast_node]
#[derive(Eq, Hash)]
pub enum Prop {
    /// `a` in `{ a, }`
    Shorthand(Ident),

    /// `key: value` in `{ key: value, }`
    KeyValue(KeyValueProp),

    /// This is **invalid** for object literal.
    Assign(AssignProp),

    Getter(GetterProp),

    Setter(SetterProp),

    Method(MethodProp),

    /// Spread properties, e.g., `{a: 1, ...obj, b: 2}`.
    Spread(SpreadAssignment),
}

impl Take for Prop {
    fn dummy() -> Self {
        Self::Shorthand(Ident {
            node_id: NodeId::DUMMY,
            sym: js_word!(""),
            ctxt: SyntaxContext::empty(),
        })
    }
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct KeyValueProp {
    pub node_id: NodeId,
    pub key: PropName,
    pub value: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct AssignProp {
    pub node_id: NodeId,
    pub key: Ident,
    pub value: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct GetterProp {
    pub node_id: NodeId,
    pub key: PropName,
    pub body: BlockStmt,
}
#[ast_node]
#[derive(Eq, Hash)]
pub struct SetterProp {
    pub node_id: NodeId,
    pub key: PropName,
    // TODO:
    pub param: ParamWithoutDecorators,
    pub body: BlockStmt,
}
#[ast_node]
#[derive(Eq, Hash)]
pub struct MethodProp {
    pub node_id: NodeId,

    pub key: PropName,
    pub function: Function,
}

#[ast_node]
#[derive(Eq, Hash)]
pub enum PropName {
    Ident(Ident),
    /// String literal.
    Str(Str),
    /// Numeric literal.
    Num(Number),
    Computed(ComputedPropName),
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct ComputedPropName {
    pub node_id: NodeId,

    pub expr: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct SpreadAssignment {
    pub node_id: NodeId,
    pub expr: Box<Expr>,
}
