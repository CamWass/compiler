#![deny(unreachable_patterns)]
#![deny(trivial_casts)]
#![deny(trivial_numeric_casts)]
#![deny(unreachable_pub)]
#![deny(variant_size_differences)]
#![deny(unused)]
#![feature(iter_order_by)]
// TODO:
#![recursion_limit = "256"]

use std::{
    borrow::Cow,
    hash::{BuildHasher, BuildHasherDefault, Hash, Hasher},
};

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
use clone_node::CloneNode;
use common::Span;
use hashbrown::HashTable;
use index::vec::IndexVec;
use node_eq::NodeEq;
use node_id::GetNodeIdMacro;
pub use paste;
use rustc_hash::FxHasher;
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
                // TODO: possible to optimise since there are no existing
                // entries, but might not be worth it.
                $self.get_id_for_name($head.into());
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
    names: IndexVec<NameId, String>,
    // Maps NameId -> () (We store IDs as keys, look up strings from `names`)
    name_to_id: HashTable<NameId>,
    hasher: BuildHasherDefault<FxHasher>,
}

// TODO: audit string related methods - we can use Cow in more places.

impl ProgramData {
    fn new() -> Self {
        let mut data = Self {
            spans: Default::default(),
            names: Default::default(),
            name_to_id: Default::default(),
            hasher: Default::default(),
        };

        data.add_built_ins();

        data
    }

    fn new_id(&mut self, span: Span) -> NodeId {
        self.spans.push(span)
    }

    pub fn new_id_from(&mut self, other: NodeId) -> NodeId {
        let other = self.spans[other];
        self.spans.push(other)
    }

    fn get_span(&self, node: NodeId) -> Span {
        self.spans[node]
    }

    fn set_span(&mut self, node: NodeId, span: Span) {
        self.spans[node] = span;
    }

    fn get_id_for_name(&mut self, name: Cow<str>) -> NameId {
        let mut hasher = self.hasher.build_hasher();
        name.hash(&mut hasher);
        let name_hash = hasher.finish();

        // TODO: can we use HashTable entry here instead?
        let entry = self
            .name_to_id
            .find_entry(name_hash, |&id| self.names[id] == name);

        match entry {
            Ok(occ) => *occ.get(),
            Err(absent) => {
                let id = self.names.push(name.to_string());

                let names = &self.names;
                let hasher_builder = &self.hasher;

                absent
                    .into_table()
                    .insert_unique(name_hash, id, |&stored_id| {
                        let mut h = hasher_builder.build_hasher();
                        // TODO: store hashes along side strings so we don't re-hash.
                        names[stored_id].hash(&mut h);
                        h.finish()
                    });

                id
            }
        }
    }

    fn get_name_for_id(&self, id: NameId) -> &str {
        let id = NameId::from_u32(id.0 & !(1 << (u32::BITS - 1)));
        &self.names[id]
    }

    // TODO: ideally this isn't pub
    pub fn mark_resolved(id: NameId) -> NameId {
        NameId::from_u32(id.0 | 1 << (u32::BITS - 1))
    }
}

#[derive(Debug)]
pub struct ParserProgramData(ProgramData);

impl Default for ParserProgramData {
    fn default() -> Self {
        Self(ProgramData::new())
    }
}

impl ParserProgramData {
    pub fn data(&mut self) -> &mut ProgramData {
        &mut self.0
    }

    pub fn into_transformer_program_data(self) -> TransformerProgramData {
        TransformerProgramData(self.0)
    }

    pub fn into_codegen_program_data(self) -> CodegenProgramData {
        CodegenProgramData(self.0)
    }

    pub fn new_id(&mut self, span: Span) -> NodeId {
        self.0.new_id(span)
    }

    pub fn new_id_from(&mut self, other: NodeId) -> NodeId {
        self.0.new_id_from(other)
    }

    pub fn get_span(&self, node: NodeId) -> Span {
        self.0.get_span(node)
    }

    pub fn set_span(&mut self, node: NodeId, span: Span) {
        self.0.set_span(node, span)
    }

    pub fn intern_name(&mut self, name: Cow<str>) -> NameId {
        self.0.get_id_for_name(name)
    }

    pub fn get_name_text(&self, name: NameId) -> &str {
        self.0.get_name_for_id(name)
    }
}

#[derive(Debug)]
pub struct TransformerProgramData(ProgramData);

impl TransformerProgramData {
    pub fn data(&mut self) -> &mut ProgramData {
        &mut self.0
    }

    pub fn into_codegen_program_data(self) -> CodegenProgramData {
        CodegenProgramData(self.0)
    }

    pub fn into_testing_program_data(self) -> TestingProgramData {
        TestingProgramData(self.0)
    }

    pub fn new_id(&mut self, span: Span) -> NodeId {
        self.0.new_id(span)
    }

    pub fn new_id_from(&mut self, other: NodeId) -> NodeId {
        self.0.new_id_from(other)
    }

    pub fn get_span(&self, node: NodeId) -> Span {
        self.0.get_span(node)
    }

    pub fn intern_name(&mut self, name: Cow<str>) -> NameId {
        self.0.get_id_for_name(name)
    }

    pub fn get_name_text(&self, name: NameId) -> &str {
        self.0.get_name_for_id(name)
    }

    pub fn new_resolved_name(&mut self, name: &str) -> NameId {
        ProgramData::mark_resolved(self.0.names.push(String::from(name)))
    }

    pub fn new_resolved_name_from(&mut self, id: NameId) -> NameId {
        ProgramData::mark_resolved(self.0.names.push(String::from(self.0.get_name_for_id(id))))
    }
}

#[derive(Debug)]
pub struct CodegenProgramData(ProgramData);

impl CodegenProgramData {
    pub fn get_span(&self, node: NodeId) -> Span {
        self.0.get_span(node)
    }

    pub fn get_name_text(&self, name: NameId) -> &str {
        self.0.get_name_for_id(name)
    }
}

#[derive(Debug)]
pub struct TestingProgramData(ProgramData);

impl TestingProgramData {
    pub fn get_name_text(&self, name: NameId) -> &str {
        self.0.get_name_for_id(name)
    }

    // TODO:
    /// Only for testing - hacky
    pub fn find_latest_id_for_name(&self, name: &str) -> Option<NameId> {
        self.0
            .names
            .iter_enumerated()
            .rev()
            .find(|(_, candidate_name)| *candidate_name == name)
            .map(|(i, _)| ProgramData::mark_resolved(i))
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
