#![deny(unreachable_patterns)]
#![deny(trivial_casts)]
#![deny(trivial_numeric_casts)]
#![deny(unreachable_pub)]
#![deny(variant_size_differences)]
#![deny(unused)]
#![feature(iter_order_by)]
// TODO:
#![recursion_limit = "256"]

use std::collections::hash_map::Entry;

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
use atoms::{JsWord, js_word};
use clone_node::CloneNode;
use common::Span;
use index::vec::IndexVec;
use node_eq::NodeEq;
use node_id::GetNodeIdMacro;
pub use paste;
use rustc_hash::FxHashMap;
use serde::Serialize;

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

index::newtype_index!(pub NodeId, Serialize);

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

index::newtype_index!(pub NameId, Serialize);

impl NameId {
    pub const DUMMY: NameId = NameId::MAX;

    pub fn is_unresolved(self) -> bool {
        (self.0 & (1 << (u32::BITS - 1))) == 0
    }
}

// TODO: the counting logic makes this macro produce deeply nested expressions.
// Should probably optimise.
macro_rules! make_built_ins {
    ($($name:tt),* $(,)?) => {
        make_built_ins!(@internal self; (0u32); (); (); (); $($name),*);
    };

    (
        @internal $self:ident;
        ($count:expr);
        ($($arms:tt)*);
        ($($pushes:tt)*);
        ($($consts:tt)*);
    ) => {
        $crate::paste::paste! {
            $($consts)*

            #[macro_export]
            macro_rules! id_for_built_in {
                $($arms)*
            }
        }

        impl ProgramData {
            fn add_built_ins(&mut $self) {
                $($pushes)*
            }
        }
    };

    (
        @internal $self:ident;
        ($idx:expr);
        ($($arms:tt)*);
        ($($pushes:tt)*);
        ($($consts:tt)*);
        $head:tt $(, $tail:tt)*
    ) => {
        make_built_ins!(
            @internal $self;
            ($idx + 1u32);
            (
                $($arms)*
                // Use $crate::paste so consuming crates don't need `paste` in Cargo.toml
                ($head) => {
                    $crate::paste::paste! { $crate::[< $head _ID >] }
                };
            );
            (
                $($pushes)*
                let id = $self.names.push(js_word!($head));
                $self.name_to_id_map.insert(js_word!($head), id);
            );
            (
                $($consts)*
                #[allow(non_upper_case_globals)]
                pub const [< $head _ID >]: NameId = NameId::from_u32($idx);
            );
            $($tail),*
        );
    };
}

// TODO: maybe re-order this so e.g. reserved names are in a dense range, so we
// use simplified condition checks instead of matching.
// Also consider lookup tables, since the built-ins fit within a u8.
make_built_ins!(
    // The lexer relies on this section having the lowest values (i.e. starting
    // at zero) and having the same ordering as the corresponding Tokens, so
    // converting between them is a no-op.
    // --Start--
    "await",
    "break",
    "case",
    "catch",
    "continue",
    "debugger",
    "default",
    "do",
    "else",
    "finally",
    "for",
    "function",
    "if",
    "return",
    "switch",
    "throw",
    "try",
    "var",
    "let",
    "const",
    "while",
    "with",
    "new",
    "this",
    "super",
    "class",
    "extends",
    "export",
    "import",
    "yield",
    "in",
    "instanceof",
    "typeof",
    "void",
    "delete",
    "null",
    "true",
    "false",
    "async",
    //
    "as",
    "from",
    "of",
    "static",
    "target",
    "asserts",
    "implements",
    "is",
    "keyof",
    "unique",
    "object",
    "global",
    "enum",
    "readonly",
    "abstract",
    "infer",
    "any",
    "boolean",
    "bigint",
    "intrinsic",
    "never",
    "number",
    "string",
    "symbol",
    "unknown",
    "interface",
    "declare",
    "undefined",
    "meta",
    "type",
    "assert",
    "get",
    "set",
    "public",
    "protected",
    "private",
    "package",
    "override",
    // --End--
    "__defineGetter__",
    "__defineSetter__",
    "__lookupGetter__",
    "__lookupSetter__",
    "__proto__",
    "abs",
    "acos",
    "acosh",
    "arguments",
    "Array",
    "asin",
    "asinh",
    "at",
    "atan",
    "atan2",
    "atanh",
    "BigInt",
    "Boolean",
    "cbrt",
    "ceil",
    "charAt",
    "charCodeAt",
    "codePointAt",
    "concat",
    "constructor",
    "cos",
    "cosh",
    "Date",
    "dotAll",
    "endsWith",
    "Error",
    "eval",
    "exec",
    "exp",
    "expm1",
    "flags",
    "floor",
    "hasIndices",
    "hasOwnProperty",
    "hypot",
    "ignoreCase",
    "includes",
    "indexOf",
    "Infinity",
    "isPrototypeOf",
    "isWellFormed",
    "lastIndex",
    "lastIndexOf",
    "length",
    "localeCompare",
    "log",
    "log10",
    "log1p",
    "log2",
    "match",
    "matchAll",
    "Math",
    "max",
    "min",
    "module",
    "multiline",
    "namespace",
    "NaN",
    "normalize",
    "Number",
    "Object",
    "padEnd",
    "padStart",
    "pow",
    "propertyIsEnumerable",
    "prototype",
    "random",
    "RegExp",
    "repeat",
    "replace",
    "replaceAll",
    "round",
    "search",
    "sign",
    "sin",
    "sinh",
    "slice",
    "source",
    "split",
    "sqrt",
    "startsWith",
    "sticky",
    "String",
    "substr",
    "substring",
    "tan",
    "tanh",
    "test",
    "toExponential",
    "toFixed",
    "toLocaleLowerCase",
    "toLocaleString",
    "toLocaleUpperCase",
    "toLowerCase",
    "toPrecision",
    "toString",
    "toUpperCase",
    "toWellFormed",
    "trim",
    "trimEnd",
    "trimStart",
    "trunc",
    "unicode",
    "unicodeSets",
    "valueOf",
    "XMLHttpRequest",
);

#[derive(Debug)]
pub struct ProgramData {
    spans: IndexVec<NodeId, Span>,
    name_to_id_map: FxHashMap<JsWord, NameId>,
    names: IndexVec<NameId, JsWord>,
}

impl Default for ProgramData {
    fn default() -> Self {
        let mut data = Self {
            spans: Default::default(),
            name_to_id_map: Default::default(),
            names: Default::default(),
        };

        data.add_built_ins();

        data
    }
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

    // TODO: maybe split ProgramData into `ParseProgramData` and
    // `TransformProgramData` or something, so each stage of the compilation
    // only has access to the 'correct'/idiomatic methods for interacting with
    // names at that stage.

    pub fn get_id_for_name(&mut self, name: JsWord) -> NameId {
        match self.name_to_id_map.entry(name) {
            Entry::Occupied(occupied_entry) => *occupied_entry.get(),
            Entry::Vacant(vacant_entry) => {
                let id = self.names.push(vacant_entry.key().clone());
                vacant_entry.insert(id);
                id
            }
        }
    }

    pub fn get_name_for_id(&self, id: NameId) -> &JsWord {
        let id = NameId::from_u32(id.0 & !(1 << (u32::BITS - 1)));
        &self.names[id]
    }

    pub fn mark_resolved(id: NameId) -> NameId {
        NameId::from_u32(id.0 | 1 << (u32::BITS - 1))
    }

    pub fn new_resolved_name_from(&mut self, id: NameId) -> NameId {
        Self::mark_resolved(self.names.push(self.get_name_for_id(id).clone()))
    }

    // TODO:
    /// Only for testing - hacky
    pub fn find_latest_id_for_name(&self, name: &JsWord) -> Option<NameId> {
        self.names
            .iter_enumerated()
            .rev()
            .find(|(_, candidate_name)| *candidate_name == name)
            .map(|(i, _)| Self::mark_resolved(i))
    }
}

/// Represents a invalid node.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize, Eq, Hash)]
pub struct Invalid {
    pub node_id: NodeId,
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Default)]
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
impl_clone_node!(JsWord);
impl_clone_node!(num_bigint::BigUint);
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
        self.iter()
            .eq_by(other.iter(), |a, b| a.eq_ignoring_node_id(b))
    }
}
impl<T: NodeEq> NodeEq for Box<T> {
    fn eq_ignoring_node_id(&self, other: &Self) -> bool {
        self.as_ref().eq_ignoring_node_id(&other)
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
impl_eq_ignoring_node_id!(JsWord);
impl_eq_ignoring_node_id!(num_bigint::BigUint);
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
        assert!(Null { node_id: NodeId(0) }.eq_ignoring_node_id(&Null { node_id: NodeId(1) }));
    }
}
