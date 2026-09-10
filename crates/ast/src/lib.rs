#![deny(unreachable_patterns)]
#![deny(trivial_casts)]
#![deny(trivial_numeric_casts)]
#![deny(unreachable_pub)]
#![deny(variant_size_differences)]
#![deny(unused)]
#![feature(iter_order_by)]
// TODO:
#![recursion_limit = "256"]

pub use self::{
    class::{
        Class, ClassMember, ClassMethod, ClassProp, Constructor, ExtendsClause, MethodKind,
        PrivateMethod, PrivateProp,
    },
    decl::{ClassDecl, Decl, FnDecl, VarDecl, VarDeclKind, VarDeclarator},
    expr::{
        ArrayLit, ArrowExpr, AssignExpr, AssignTarget, AwaitExpr, BinExpr, CallExpr, ClassExpr,
        CondExpr, Expr, ExprOrSpread, ExprOrSuper, FnExpr, MemberExpr, MetaPropExpr, NewExpr,
        ObjectLit, OptChainBase, OptChainExpr, SeqExpr, SimpleAssignTarget, SpreadElement, Super,
        TaggedTpl, ThisExpr, Tpl, TplElement, TplString, UnaryExpr, UpdateExpr, YieldExpr,
    },
    function::{FnFlags, Function, FunctionParams, Param},
    ident::{BindingIdent, Ident, PrivateName},
    lit::{BigInt, Bool, Lit, Null, Number, Regex, Str},
    module::{Module, ModuleItem, Program, Script},
    module_decl::{
        DefaultDecl, ExportAll, ExportDecl, ExportDefaultDecl, ExportDefaultExpr,
        ExportDefaultSpecifier, ExportNamedSpecifier, ExportNamespaceSpecifier, ExportSpecifier,
        ImportDecl, ImportDefaultSpecifier, ImportNamedSpecifier, ImportSpecifier,
        ImportStarAsSpecifier, ModuleDecl, NamedExport,
    },
    operators::{AssignOp, BinaryOp, UnaryOp, UpdateOp},
    pat::{
        ArrayAssignmentPat, ArrayBindingPat, AssignmentElement, AssignmentPat, AssignmentProperty,
        AssignmentRest, BindingElement, BindingPat, BindingPatOrIdent, BindingProperty,
        BindingRestElement, BindingRestProperty, ObjectAssignmentPat, ObjectBindingPat,
    },
    program_data::*,
    prop::{
        AssignProp, ComputedPropName, GetterProp, KeyValueProp, MethodProp, Prop, PropName,
        SetterProp, SpreadAssignment,
    },
    stmt::{
        BlockStmt, BreakStmt, CatchClause, ContinueStmt, DebuggerStmt, DoWhileStmt, EmptyStmt,
        ExprStmt, ForInStmt, ForOfStmt, ForStmt, IfStmt, LabeledStmt, ReturnStmt, Stmt, SwitchCase,
        SwitchStmt, ThrowStmt, TryStmt, TryStmtTail, VarDeclOrAssignTarget, VarDeclOrExpr,
        WhileStmt, WithStmt,
    },
};
use big_int::BigUintValue;
pub use paste;

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
mod program_data;
mod prop;
mod stmt;

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

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Default)]
pub enum EsVersion {
    Es3,
    Es5,
    Es2015,
    Es2016,
    Es2017,
    Es2018,
    Es2019,
    Es2020,
    Es2021,
    Es2022,
    #[default]
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
impl_clone_node!(BigUintValue);
impl_clone_node!(NameId);
impl_clone_node!(String);

pub trait NodeEq {
    fn eq_ignoring_node_id(&self, other: &Self) -> bool;
}

impl<T: NodeEq> NodeEq for Option<T> {
    fn eq_ignoring_node_id(&self, other: &Self) -> bool {
        match (self, other) {
            (None, None) => true,
            (None, Some(_)) => false,
            (Some(_), None) => false,
            (Some(a), Some(b)) => a.eq_ignoring_node_id(b),
        }
    }
}
impl<T: NodeEq> NodeEq for Vec<T> {
    fn eq_ignoring_node_id(&self, other: &Self) -> bool {
        self.iter().eq_by(other.iter(), NodeEq::eq_ignoring_node_id)
    }
}
impl<T: NodeEq> NodeEq for Box<T> {
    fn eq_ignoring_node_id(&self, other: &Self) -> bool {
        self.as_ref().eq_ignoring_node_id(other)
    }
}

macro_rules! impl_eq_ignoring_node_id {
    ($t:ty) => {
        impl NodeEq for $t {
            fn eq_ignoring_node_id(&self, other: &Self) -> bool {
                self == other
            }
        }
    };
}

impl_eq_ignoring_node_id!(bool);
impl_eq_ignoring_node_id!(f64);
impl_eq_ignoring_node_id!(BigUintValue);
impl_eq_ignoring_node_id!(NameId);
impl_eq_ignoring_node_id!(String);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_node_eq_vec() {
        assert!(vec![true, false].eq_ignoring_node_id(&vec![true, false]));
        assert!(!vec![true, false].eq_ignoring_node_id(&vec![false, true]));
    }

    #[test]
    fn test_node_eq_option() {
        assert!(None::<bool>.eq_ignoring_node_id(&None));
        assert!(Some(true).eq_ignoring_node_id(&Some(true)));
        assert!(!Some(false).eq_ignoring_node_id(&Some(true)));
        assert!(!Some(false).eq_ignoring_node_id(&None));
    }

    #[test]
    fn test_node_eq_node() {
        assert!(
            Null {
                node_id: NodeId::from_u32(0)
            }
            .eq_ignoring_node_id(&Null {
                node_id: NodeId::from_u32(1)
            })
        );
    }
}
