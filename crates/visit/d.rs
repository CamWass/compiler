#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2018::*;
#[macro_use]
extern crate std;
#[doc(hidden)]
pub extern crate ast;
use ast::*;
use global_common::{pass::CompilerPass, Span, DUMMY_SP};
use global_visit::{define, AndThen, Repeat, Repeated};
use num_bigint::BigInt as BigIntValue;
use std::{any::Any, borrow::Cow, fmt::Debug};
use swc_atoms::JsWord;
impl<'ast, V: Visit<'ast>> VisitWith<'ast, V> for AstNode<'ast> {
    fn visit_with(&'ast self, v: &mut V) {
        match self {
            AstNode::Class(a) => a.visit_with(v),
            AstNode::ClassProp(a) => a.visit_with(v),
            AstNode::PrivateProp(a) => a.visit_with(v),
            AstNode::ClassMethod(a) => a.visit_with(v),
            AstNode::PrivateMethod(a) => a.visit_with(v),
            AstNode::Constructor(a) => a.visit_with(v),
            AstNode::Decorator(a) => a.visit_with(v),
            AstNode::FnDecl(a) => a.visit_with(v),
            AstNode::ClassDecl(a) => a.visit_with(v),
            AstNode::VarDecl(a) => a.visit_with(v),
            AstNode::VarDeclarator(a) => a.visit_with(v),
            AstNode::ThisExpr(a) => a.visit_with(v),
            AstNode::ArrayLit(a) => a.visit_with(v),
            AstNode::ObjectLit(a) => a.visit_with(v),
            AstNode::SpreadElement(a) => a.visit_with(v),
            AstNode::UnaryExpr(a) => a.visit_with(v),
            AstNode::UpdateExpr(a) => a.visit_with(v),
            AstNode::BinExpr(a) => a.visit_with(v),
            AstNode::FnExpr(a) => a.visit_with(v),
            AstNode::ClassExpr(a) => a.visit_with(v),
            AstNode::AssignExpr(a) => a.visit_with(v),
            AstNode::MemberExpr(a) => a.visit_with(v),
            AstNode::CondExpr(a) => a.visit_with(v),
            AstNode::CallExpr(a) => a.visit_with(v),
            AstNode::NewExpr(a) => a.visit_with(v),
            AstNode::SeqExpr(a) => a.visit_with(v),
            AstNode::ArrowExpr(a) => a.visit_with(v),
            AstNode::YieldExpr(a) => a.visit_with(v),
            AstNode::MetaPropExpr(a) => a.visit_with(v),
            AstNode::AwaitExpr(a) => a.visit_with(v),
            AstNode::Tpl(a) => a.visit_with(v),
            AstNode::TaggedTpl(a) => a.visit_with(v),
            AstNode::TplElement(a) => a.visit_with(v),
            AstNode::ParenExpr(a) => a.visit_with(v),
            AstNode::Super(a) => a.visit_with(v),
            AstNode::ExprOrSpread(a) => a.visit_with(v),
            AstNode::OptChainExpr(a) => a.visit_with(v),
            AstNode::Function(a) => a.visit_with(v),
            AstNode::Param(a) => a.visit_with(v),
            AstNode::BindingIdent(a) => a.visit_with(v),
            AstNode::Ident(a) => a.visit_with(v),
            AstNode::PrivateName(a) => a.visit_with(v),
            AstNode::JSXMemberExpr(a) => a.visit_with(v),
            AstNode::JSXNamespacedName(a) => a.visit_with(v),
            AstNode::JSXEmptyExpr(a) => a.visit_with(v),
            AstNode::JSXExprContainer(a) => a.visit_with(v),
            AstNode::JSXSpreadChild(a) => a.visit_with(v),
            AstNode::JSXOpeningElement(a) => a.visit_with(v),
            AstNode::JSXClosingElement(a) => a.visit_with(v),
            AstNode::JSXAttr(a) => a.visit_with(v),
            AstNode::JSXText(a) => a.visit_with(v),
            AstNode::JSXElement(a) => a.visit_with(v),
            AstNode::JSXFragment(a) => a.visit_with(v),
            AstNode::JSXOpeningFragment(a) => a.visit_with(v),
            AstNode::JSXClosingFragment(a) => a.visit_with(v),
            AstNode::Invalid(a) => a.visit_with(v),
            AstNode::Str(a) => a.visit_with(v),
            AstNode::Bool(a) => a.visit_with(v),
            AstNode::Null(a) => a.visit_with(v),
            AstNode::Number(a) => a.visit_with(v),
            AstNode::BigInt(a) => a.visit_with(v),
            AstNode::Regex(a) => a.visit_with(v),
            AstNode::ExportDefaultExpr(a) => a.visit_with(v),
            AstNode::ExportDecl(a) => a.visit_with(v),
            AstNode::ImportDecl(a) => a.visit_with(v),
            AstNode::ExportAll(a) => a.visit_with(v),
            AstNode::NamedExport(a) => a.visit_with(v),
            AstNode::ExportDefaultDecl(a) => a.visit_with(v),
            AstNode::ImportDefaultSpecifier(a) => a.visit_with(v),
            AstNode::ImportStarAsSpecifier(a) => a.visit_with(v),
            AstNode::ImportNamedSpecifier(a) => a.visit_with(v),
            AstNode::ExportNamespaceSpecifier(a) => a.visit_with(v),
            AstNode::ExportDefaultSpecifier(a) => a.visit_with(v),
            AstNode::ExportNamedSpecifier(a) => a.visit_with(v),
            AstNode::Script(a) => a.visit_with(v),
            AstNode::Module(a) => a.visit_with(v),
            AstNode::ArrayPat(a) => a.visit_with(v),
            AstNode::ObjectPat(a) => a.visit_with(v),
            AstNode::AssignPat(a) => a.visit_with(v),
            AstNode::RestPat(a) => a.visit_with(v),
            AstNode::KeyValuePatProp(a) => a.visit_with(v),
            AstNode::AssignPatProp(a) => a.visit_with(v),
            AstNode::KeyValueProp(a) => a.visit_with(v),
            AstNode::AssignProp(a) => a.visit_with(v),
            AstNode::GetterProp(a) => a.visit_with(v),
            AstNode::SetterProp(a) => a.visit_with(v),
            AstNode::MethodProp(a) => a.visit_with(v),
            AstNode::ComputedPropName(a) => a.visit_with(v),
            AstNode::BlockStmt(a) => a.visit_with(v),
            AstNode::ExprStmt(a) => a.visit_with(v),
            AstNode::EmptyStmt(a) => a.visit_with(v),
            AstNode::DebuggerStmt(a) => a.visit_with(v),
            AstNode::WithStmt(a) => a.visit_with(v),
            AstNode::ReturnStmt(a) => a.visit_with(v),
            AstNode::LabeledStmt(a) => a.visit_with(v),
            AstNode::BreakStmt(a) => a.visit_with(v),
            AstNode::ContinueStmt(a) => a.visit_with(v),
            AstNode::IfStmt(a) => a.visit_with(v),
            AstNode::SwitchStmt(a) => a.visit_with(v),
            AstNode::ThrowStmt(a) => a.visit_with(v),
            AstNode::TryStmt(a) => a.visit_with(v),
            AstNode::WhileStmt(a) => a.visit_with(v),
            AstNode::DoWhileStmt(a) => a.visit_with(v),
            AstNode::ForStmt(a) => a.visit_with(v),
            AstNode::ForInStmt(a) => a.visit_with(v),
            AstNode::ForOfStmt(a) => a.visit_with(v),
            AstNode::SwitchCase(a) => a.visit_with(v),
            AstNode::CatchClause(a) => a.visit_with(v),
            AstNode::TsTypeAnn(a) => a.visit_with(v),
            AstNode::TsTypeParamDecl(a) => a.visit_with(v),
            AstNode::TsTypeParam(a) => a.visit_with(v),
            AstNode::TsTypeParamInstantiation(a) => a.visit_with(v),
            AstNode::TsParamProp(a) => a.visit_with(v),
            AstNode::TsQualifiedName(a) => a.visit_with(v),
            AstNode::TsCallSignatureDecl(a) => a.visit_with(v),
            AstNode::TsConstructSignatureDecl(a) => a.visit_with(v),
            AstNode::TsPropertySignature(a) => a.visit_with(v),
            AstNode::TsGetterSignature(a) => a.visit_with(v),
            AstNode::TsSetterSignature(a) => a.visit_with(v),
            AstNode::TsMethodSignature(a) => a.visit_with(v),
            AstNode::TsIndexSignature(a) => a.visit_with(v),
            AstNode::TsKeywordType(a) => a.visit_with(v),
            AstNode::TsThisType(a) => a.visit_with(v),
            AstNode::TsFnType(a) => a.visit_with(v),
            AstNode::TsConstructorType(a) => a.visit_with(v),
            AstNode::TsTypeRef(a) => a.visit_with(v),
            AstNode::TsTypePredicate(a) => a.visit_with(v),
            AstNode::TsTypeQuery(a) => a.visit_with(v),
            AstNode::TsImportType(a) => a.visit_with(v),
            AstNode::TsTypeLit(a) => a.visit_with(v),
            AstNode::TsArrayType(a) => a.visit_with(v),
            AstNode::TsTupleType(a) => a.visit_with(v),
            AstNode::TsTupleElement(a) => a.visit_with(v),
            AstNode::TsOptionalType(a) => a.visit_with(v),
            AstNode::TsRestType(a) => a.visit_with(v),
            AstNode::TsUnionType(a) => a.visit_with(v),
            AstNode::TsIntersectionType(a) => a.visit_with(v),
            AstNode::TsConditionalType(a) => a.visit_with(v),
            AstNode::TsInferType(a) => a.visit_with(v),
            AstNode::TsParenthesizedType(a) => a.visit_with(v),
            AstNode::TsTypeOperator(a) => a.visit_with(v),
            AstNode::TsIndexedAccessType(a) => a.visit_with(v),
            AstNode::TsMappedType(a) => a.visit_with(v),
            AstNode::TsLitType(a) => a.visit_with(v),
            AstNode::TsTplLitType(a) => a.visit_with(v),
            AstNode::TsInterfaceDecl(a) => a.visit_with(v),
            AstNode::TsInterfaceBody(a) => a.visit_with(v),
            AstNode::TsExprWithTypeArgs(a) => a.visit_with(v),
            AstNode::TsTypeAliasDecl(a) => a.visit_with(v),
            AstNode::TsEnumDecl(a) => a.visit_with(v),
            AstNode::TsEnumMember(a) => a.visit_with(v),
            AstNode::TsModuleDecl(a) => a.visit_with(v),
            AstNode::TsModuleBlock(a) => a.visit_with(v),
            AstNode::TsNamespaceDecl(a) => a.visit_with(v),
            AstNode::TsImportEqualsDecl(a) => a.visit_with(v),
            AstNode::TsExternalModuleRef(a) => a.visit_with(v),
            AstNode::TsExportAssignment(a) => a.visit_with(v),
            AstNode::TsNamespaceExportDecl(a) => a.visit_with(v),
            AstNode::TsAsExpr(a) => a.visit_with(v),
            AstNode::TsTypeAssertion(a) => a.visit_with(v),
            AstNode::TsNonNullExpr(a) => a.visit_with(v),
            AstNode::TsConstAssertion(a) => a.visit_with(v),
        }
    }
    fn visit_children_with(&'ast self, visitor: &mut V) {
        self.visit_with(visitor)
    }
}
impl<A, B> Fold for AndThen<A, B>
where
    A: Fold,
    B: Fold,
{
    #[inline(always)]
    fn fold_module(&mut self, n: Module) -> Module {
        let n = self.first.fold_module(n);
        self.second.fold_module(n)
    }
    #[inline(always)]
    fn fold_script(&mut self, n: Script) -> Script {
        let n = self.first.fold_script(n);
        self.second.fold_script(n)
    }
}
impl<'ast, A, B> Visit<'ast> for AndThen<A, B>
where
    A: Visit<'ast>,
    B: Visit<'ast>,
{
    fn visit_module(&mut self, n: &'ast Module) {
        self.first.visit_module(n);
        self.second.visit_module(n);
    }
    fn visit_script(&mut self, n: &'ast Script) {
        self.first.visit_script(n);
        self.second.visit_script(n);
    }
}
impl<V> Fold for Repeat<V>
where
    V: Fold + Repeated,
{
    fn fold_module(&mut self, mut node: Module) -> Module {
        loop {
            self.pass.reset();
            node = node.fold_with(&mut self.pass);
            if !self.pass.changed() {
                break;
            }
        }
        node
    }
    fn fold_script(&mut self, mut node: Script) -> Script {
        loop {
            self.pass.reset();
            node = node.fold_with(&mut self.pass);
            if !self.pass.changed() {
                break;
            }
        }
        node
    }
}
/// Not a public api.
struct SpanRemover;
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::fmt::Debug for SpanRemover {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match *self {
            SpanRemover => {
                let debug_trait_builder =
                    &mut ::core::fmt::Formatter::debug_tuple(f, "SpanRemover");
                ::core::fmt::DebugTuple::finish(debug_trait_builder)
            }
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::clone::Clone for SpanRemover {
    #[inline]
    fn clone(&self) -> SpanRemover {
        {
            *self
        }
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::marker::Copy for SpanRemover {}
impl ::core::marker::StructuralPartialEq for SpanRemover {}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::PartialEq for SpanRemover {
    #[inline]
    fn eq(&self, other: &SpanRemover) -> bool {
        match *other {
            SpanRemover => match *self {
                SpanRemover => true,
            },
        }
    }
}
impl ::core::marker::StructuralEq for SpanRemover {}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::cmp::Eq for SpanRemover {
    #[inline]
    #[doc(hidden)]
    fn assert_receiver_is_total_eq(&self) -> () {
        {}
    }
}
#[automatically_derived]
#[allow(unused_qualifications)]
impl ::core::default::Default for SpanRemover {
    #[inline]
    fn default() -> SpanRemover {
        SpanRemover {}
    }
}
/// Returns a `Fold` which changes all span into `DUMMY_SP`.
pub fn span_remover() -> impl Debug + Fold + Copy + Eq + Default + 'static {
    SpanRemover
}
impl Fold for SpanRemover {
    fn fold_span(&mut self, _: Span) -> Span {
        DUMMY_SP
    }
}
