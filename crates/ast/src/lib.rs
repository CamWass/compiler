#![deny(unreachable_patterns)]
#![deny(trivial_casts)]
#![deny(trivial_numeric_casts)]
#![deny(unreachable_pub)]
// #![deny(variant_size_differences)]

pub use self::{
    class::{
        Class, ClassMember, ClassMethod, ClassProp, Constructor, Decorator, ExtendsClause,
        MethodKind, PrivateMethod, PrivateProp,
    },
    decl::{ClassDecl, Decl, FnDecl, VarDecl, VarDeclKind, VarDeclarator},
    expr::{
        ArrayLit, ArrowExpr, AssignExpr, AwaitExpr, BinExpr, BlockStmtOrExpr, CallExpr, ClassExpr,
        CondExpr, Expr, ExprOrSpread, ExprOrSuper, FnExpr, MemberExpr, MetaPropExpr, NewExpr,
        ObjectLit, OptChainExpr, ParenExpr, PatOrExpr, SeqExpr, SpreadElement, Super, TaggedTpl,
        ThisExpr, Tpl, TplElement, UnaryExpr, UpdateExpr, YieldExpr,
    },
    function::{Function, Param, ParamWithoutDecorators},
    ident::{BindingIdent, Ident, IdentExt, PrivateName},
    jsx::{
        JSXAttr, JSXAttrName, JSXAttrOrSpread, JSXAttrValue, JSXClosingElement, JSXClosingFragment,
        JSXElement, JSXElementChild, JSXElementName, JSXEmptyExpr, JSXExpr, JSXExprContainer,
        JSXFragment, JSXMemberExpr, JSXNamespacedName, JSXObject, JSXOpeningElement,
        JSXOpeningFragment, JSXSpreadChild, JSXText,
    },
    lit::{BigInt, Bool, Lit, Null, Number, Regex, Str, StrKind},
    module::{Module, ModuleItem, Program, Script},
    module_decl::{
        DefaultDecl, ExportAll, ExportDecl, ExportDefaultDecl, ExportDefaultExpr,
        ExportDefaultSpecifier, ExportNamedSpecifier, ExportNamespaceSpecifier, ExportSpecifier,
        ImportDecl, ImportDefaultSpecifier, ImportNamedSpecifier, ImportSpecifier,
        ImportStarAsSpecifier, ModuleDecl, NamedExport,
    },
    operators::{AssignOp, BinaryOp, UnaryOp, UpdateOp},
    pat::{
        ArrayPat, AssignPat, AssignPatProp, KeyValuePatProp, ObjectPat, ObjectPatProp, Pat, RestPat,
    },
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
use ast_node::ast_node;
use atoms::JsWord;
use global_common::{Span, SyntaxContext, DUMMY_SP};
use rustc_hash::FxHashMap;

#[macro_use]
mod macros;
mod class;
mod decl;
mod expr;
mod function;
mod ident;
mod jsx;
mod lit;
mod module;
mod module_decl;
mod operators;
mod pat;
mod prop;
mod stmt;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct NodeId(u32);

impl NodeId {
    pub const DUMMY: NodeId = NodeId(u32::MAX);
}

pub trait GetNodeId {
    fn node_id(&self) -> NodeId;
}

impl<T> GetNodeId for Box<T>
where
    T: GetNodeId,
{
    fn node_id(&self) -> NodeId {
        self.as_ref().node_id()
    }
}

#[derive(Debug)]
pub struct ProgramData {
    node_id_gen: NodeIdGen,
    spans: FxHashMap<NodeId, Span>,
}

impl ProgramData {
    pub fn new_id(&mut self, span: Span) -> NodeId {
        let id = self.node_id_gen.next();
        if span != DUMMY_SP {
            self.spans.insert(id, span);
        }
        id
    }

    pub fn new_id_from(&mut self, other: NodeId) -> NodeId {
        let id = self.node_id_gen.next();
        let other_span = self.spans.get(&other).copied();
        if let Some(other_span) = other_span {
            self.spans.insert(id, other_span);
        }
        id
    }

    pub fn get_span(&self, node: NodeId) -> Span {
        self.spans.get(&node).copied().unwrap_or(DUMMY_SP)
    }

    pub fn set_span(&mut self, node: NodeId, span: Span) {
        if span != DUMMY_SP {
            self.spans.insert(node, span);
        }
    }
}

#[derive(Clone, Debug)]
struct NodeIdGen {
    cur: NodeId,
}

impl Default for NodeIdGen {
    fn default() -> Self {
        Self { cur: NodeId(0) }
    }
}

impl NodeIdGen {
    fn next(&mut self) -> NodeId {
        // Incrementing after we take the id ensures that NodeId(0) is used.
        let id = self.cur;
        self.cur.0 += 1;
        id
    }
}

/// Represents a invalid node.
#[ast_node]
#[derive(Eq, Hash)]
pub struct Invalid {
    pub node_id: NodeId,
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum EsVersion {
    Es3,
    Es5,
    Es2015,
    Es2016,
    Es2017,
    Es2018,
    Es2019,
    Es2020,
}

impl EsVersion {
    /// Get the latest version. This is `es2020` for now, but it will be changed
    /// if a new version of specification is released.
    pub const fn latest() -> Self {
        EsVersion::Es2020
    }
}

impl Default for EsVersion {
    fn default() -> Self {
        EsVersion::Es5
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
