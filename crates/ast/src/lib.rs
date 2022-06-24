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
    function::{Function, Param, ParamOrTsParamProp, ParamWithoutDecorators},
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
    typescript::{
        Accessibility, TruePlusMinus, TsAmbientParam, TsAmbientParamPat, TsArrayType, TsAsExpr,
        TsCallSignatureDecl, TsConditionalType, TsConstAssertion, TsConstructSignatureDecl,
        TsConstructorType, TsEntityName, TsEnumDecl, TsEnumMember, TsEnumMemberId,
        TsExportAssignment, TsExprWithTypeArgs, TsExternalModuleRef, TsFnOrConstructorType,
        TsFnType, TsGetterSignature, TsImportEqualsDecl, TsImportType, TsIndexSignature,
        TsIndexedAccessType, TsInferType, TsInterfaceBody, TsInterfaceDecl, TsIntersectionType,
        TsKeywordType, TsKeywordTypeKind, TsLit, TsLitType, TsMappedType, TsMethodSignature,
        TsModuleBlock, TsModuleDecl, TsModuleName, TsModuleRef, TsNamespaceBody, TsNamespaceDecl,
        TsNamespaceExportDecl, TsNonNullExpr, TsOptionalType, TsParamProp, TsParamPropParam,
        TsParenthesizedType, TsPropertySignature, TsQualifiedName, TsRestType, TsSetterSignature,
        TsThisType, TsThisTypeOrIdent, TsTplLitType, TsTupleElement, TsTupleType, TsType,
        TsTypeAliasDecl, TsTypeAnn, TsTypeAssertion, TsTypeElement, TsTypeLit, TsTypeOperator,
        TsTypeOperatorOp, TsTypeParamDecl, TsTypeParamInstantiation, TsTypePredicate, TsTypeQuery,
        TsTypeQueryExpr, TsTypeRef, TsUnionOrIntersectionType, TsUnionType,
    },
};
use ast_node::ast_node;
use global_common::{EqIgnoreSpan, Span};
use index::{newtype_index, vec::Idx};

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
mod typescript;

newtype_index! {
    pub struct NodeId {
        derive [EqIgnoreSpan]
        DEBUG_FORMAT = "NodeId({})"
    }
}

pub trait GetNodeId {
    fn node_id(&self) -> NodeId;
}

#[derive(Clone)]
pub struct NodeIdGen {
    cur: NodeId,
}

impl Default for NodeIdGen {
    fn default() -> Self {
        Self {
            cur: NodeId::from_u32(0),
        }
    }
}

impl NodeIdGen {
    pub fn next(&mut self) -> NodeId {
        // Incrementing after we take the id ensures that NodeId(0) is used.
        let id = self.cur;
        self.cur.increment_by(1);
        id
    }
}

/// Represents a invalid node.
#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct Invalid {
    pub node_id: NodeId,

    pub span: Span,
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
