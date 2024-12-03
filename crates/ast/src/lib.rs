#![deny(unreachable_patterns)]
#![deny(trivial_casts)]
#![deny(trivial_numeric_casts)]
#![deny(unreachable_pub)]
#![deny(variant_size_differences)]

pub use self::{
    class::{
        Class, ClassMember, ClassMethod, ClassProp, Constructor, ExtendsClause, MethodKind,
        PrivateMethod, PrivateProp,
    },
    decl::{ClassDecl, Decl, FnDecl, VarDecl, VarDeclKind, VarDeclarator},
    expr::{
        ArrayLit, ArrowExpr, AssignExpr, AwaitExpr, BinExpr, CallExpr, ClassExpr, CondExpr, Expr,
        ExprOrSpread, ExprOrSuper, FnExpr, MemberExpr, MetaPropExpr, NewExpr, ObjectLit,
        OptChainExpr, PatOrExpr, SeqExpr, SpreadElement, Super, TaggedTpl, ThisExpr, Tpl,
        TplElement, UnaryExpr, UpdateExpr, YieldExpr,
    },
    function::{FnFlags, Function, Param},
    ident::{BindingIdent, Ident, IdentExt, PrivateName},
    lit::{BigInt, Bool, Lit, Null, Number, Regex, Str, StrKind},
    module::{Module, ModuleItem, Program, Script},
    module_decl::{
        DefaultDecl, ExportAll, ExportDecl, ExportDefaultDecl, ExportDefaultExpr,
        ExportDefaultSpecifier, ExportNamedSpecifier, ExportNamespaceSpecifier, ExportSpecifier,
        ImportDecl, ImportDefaultSpecifier, ImportNamedSpecifier, ImportSpecifier,
        ImportStarAsSpecifier, ModuleDecl, NamedExport,
    },
    operators::{AssignOp, BinaryOp, UnaryOp, UpdateOp},
    pat::{ArrayPat, AssignPat, KeyValuePatProp, ObjectPat, ObjectPatProp, Pat, RestPat},
    prop::{
        AssignProp, ComputedPropName, GetterProp, KeyValueProp, MethodProp, Prop, PropName,
        SetterProp, SpreadAssignment,
    },
    stmt::{
        BlockStmt, BreakStmt, CatchClause, ContinueStmt, DebuggerStmt, DoWhileStmt, EmptyStmt,
        ExprStmt, ForInStmt, ForOfStmt, ForStmt, IfStmt, LabeledStmt, ReturnStmt, Stmt, SwitchCase,
        SwitchStmt, ThrowStmt, TryStmt, VarDeclOrExpr, VarDeclOrPat, WhileStmt, WithStmt,
    },
};
use atoms::JsWord;
use clone_node::CloneNode;
use global_common::{Span, SyntaxContext};
use index::vec::IndexVec;
use node_id::GetNodeIdMacro;

#[macro_use]
mod macros;
mod class;
mod decl;
mod expr;
mod function;
mod ident;
mod lit;
mod module;
mod module_decl;
mod operators;
mod pat;
mod prop;
mod stmt;

index::newtype_index!(pub struct NodeId { .. });

impl NodeId {
    pub const DUMMY: NodeId = NodeId::MAX;
}

pub trait GetNodeId {
    fn node_id(&self) -> NodeId;
}

impl<T> GetNodeId for &T
where
    T: GetNodeId,
{
    fn node_id(&self) -> NodeId {
        (*self).node_id()
    }
}

impl<T> GetNodeId for Box<T>
where
    T: GetNodeId,
{
    fn node_id(&self) -> NodeId {
        self.as_ref().node_id()
    }
}

#[derive(Debug, Default)]
pub struct ProgramData {
    spans: IndexVec<NodeId, Span>,
}

impl ProgramData {
    pub fn new_id(&mut self, span: Span) -> NodeId {
        self.spans.push(span)
    }

    pub fn new_id_from(&mut self, other: NodeId) -> NodeId {
        let other = self.spans[other];
        self.spans.push(other)
    }

    pub fn get_span(&self, node: NodeId) -> Span {
        self.spans[node]
    }

    pub fn set_span(&mut self, node: NodeId, span: Span) {
        self.spans[node] = span;
    }
}

/// Represents a invalid node.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Invalid {
    pub node_id: NodeId,
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Default)]
pub enum EsVersion {
    Es3,
    #[default]
    Es5,
    Es2015,
    Es2016,
    Es2017,
    Es2018,
    Es2019,
    Es2020,
    Es2021,
    Es2022,
    EsNext,
}

impl EsVersion {
    pub const fn latest() -> Self {
        EsVersion::EsNext
    }
}

/// Because [`NodeId`]s must be unique, nodes do not implement [`Clone`] directly,
/// as that would make it too easy to forget to update the [`NodeId`]s of the
/// cloned nodes. Using [`clone_node`][CloneNode::clone_node] ensures that all
/// [`NodeId`]s remain unique.
pub trait CloneNode {
    /// Clone the node. All new nodes will have unique [`NodeId`]s.
    fn clone_node(&self, program_data: &mut ProgramData) -> Self;
}

impl<T: CloneNode> CloneNode for Option<T> {
    fn clone_node(&self, program_data: &mut ProgramData) -> Self {
        self.as_ref().map(|v| v.clone_node(program_data))
    }
}
impl<T: CloneNode> CloneNode for Box<T> {
    fn clone_node(&self, program_data: &mut ProgramData) -> Self {
        Box::new(self.as_ref().clone_node(program_data))
    }
}
impl<T: CloneNode> CloneNode for Vec<T> {
    fn clone_node(&self, program_data: &mut ProgramData) -> Self {
        self.iter().map(|v| v.clone_node(program_data)).collect()
    }
}

macro_rules! impl_clone_node {
    ($t:ty) => {
        impl CloneNode for $t {
            fn clone_node(&self, _: &mut ProgramData) -> Self {
                self.clone()
            }
        }
    };
}

impl_clone_node!(bool);
impl_clone_node!(f64);
impl_clone_node!(JsWord);
impl_clone_node!(SyntaxContext);
impl_clone_node!(num_bigint::BigInt);
