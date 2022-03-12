use global_common::Spanned;

use crate::ast;
use crate::{Visit, VisitWith};
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

#[derive(PartialEq, Eq, Clone)]
pub enum Parameter {
    Param(Rc<Param>),
    ParamWithoutDecorators(Rc<ParamWithoutDecorators>),
    TsAmbientParam(Rc<TsAmbientParam>),
    TsParamProp(Rc<TsParamProp>),
}

impl Parameter {
    pub fn parent(&self) -> BoundNode {
        match self {
            Parameter::Param(p) => p.parent.clone().unwrap(),
            Parameter::ParamWithoutDecorators(p) => p.parent.clone().unwrap(),
            Parameter::TsAmbientParam(p) => p.parent.clone().unwrap(),
            Parameter::TsParamProp(p) => p.parent.clone().unwrap(),
        }
    }
    pub fn pat(&self) -> ast::Pat {
        match self {
            Parameter::Param(p) => p.pat.clone(),
            Parameter::ParamWithoutDecorators(p) => p.pat.clone(),
            Parameter::TsAmbientParam(p) => p.pat.clone().into(),
            Parameter::TsParamProp(p) => p.param.clone().into(),
        }
    }
}

macro_rules! impl_parameter {
    ($ident:ident, $ty:ty) => {
        impl From<Rc<$ty>> for Parameter {
            fn from(param: Rc<$ty>) -> Self {
                Self::$ident(param)
            }
        }
        impl PartialEq<&Rc<ast::$ident>> for Parameter {
            fn eq(&self, other: &&Rc<ast::$ident>) -> bool {
                match self {
                    Self::$ident(p) => &p.node == *other,
                    _ => false,
                }
            }
        }
    };
}
impl_parameter!(Param, Param);
impl_parameter!(ParamWithoutDecorators, ParamWithoutDecorators);
impl_parameter!(TsAmbientParam, TsAmbientParam);
impl_parameter!(TsParamProp, TsParamProp);

impl From<Parameter> for BoundNode {
    fn from(param: Parameter) -> Self {
        match param {
            Parameter::Param(n) => n.into(),
            Parameter::ParamWithoutDecorators(n) => n.into(),
            Parameter::TsAmbientParam(n) => n.into(),
            Parameter::TsParamProp(n) => n.into(),
        }
    }
}

pub trait IsRestParam {
    fn is_rest_param(&self) -> bool;
}

impl IsRestParam for Parameter {
    fn is_rest_param(&self) -> bool {
        match self {
            Parameter::Param(p) => matches!(p.pat, ast::Pat::Rest(_)),
            Parameter::ParamWithoutDecorators(p) => matches!(p.pat, ast::Pat::Rest(_)),
            Parameter::TsAmbientParam(p) => matches!(p.pat, ast::TsAmbientParamPat::Rest(_)),
            Parameter::TsParamProp(_) => false,
        }
    }
}

impl IsRestParam for BoundNode {
    fn is_rest_param(&self) -> bool {
        match self {
            BoundNode::Param(p) => matches!(p.pat, ast::Pat::Rest(_)),
            BoundNode::ParamWithoutDecorators(p) => matches!(p.pat, ast::Pat::Rest(_)),
            BoundNode::TsAmbientParam(p) => matches!(p.pat, ast::TsAmbientParamPat::Rest(_)),
            BoundNode::TsParamProp(_) => false,
            _ => false,
        }
    }
}

pub trait Bind {
    fn bind(&self, parent: BoundNode) -> BoundNode {
        self.bind_to_opt_parent(Some(parent))
    }
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode;
}

impl Bind for ast::Stmt {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::Stmt::Block(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::Empty(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::Debugger(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::With(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::Return(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::Labeled(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::Break(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::Continue(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::If(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::Switch(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::Throw(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::Try(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::While(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::DoWhile(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::For(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::ForIn(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::ForOf(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::Decl(s) => s.bind_to_opt_parent(parent),
            ast::Stmt::Expr(s) => s.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::Decl {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::Decl::Class(d) => d.bind_to_opt_parent(parent),
            ast::Decl::Fn(d) => d.bind_to_opt_parent(parent),
            ast::Decl::Var(d) => d.bind_to_opt_parent(parent),
            ast::Decl::TsInterface(d) => d.bind_to_opt_parent(parent),
            ast::Decl::TsTypeAlias(d) => d.bind_to_opt_parent(parent),
            ast::Decl::TsEnum(d) => d.bind_to_opt_parent(parent),
            ast::Decl::TsModule(d) => d.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::Expr {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::Expr::This(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Array(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Object(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Fn(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Unary(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Update(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Bin(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Assign(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Member(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Cond(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Call(e) => e.bind_to_opt_parent(parent),
            ast::Expr::New(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Seq(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Ident(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Lit(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Tpl(e) => e.bind_to_opt_parent(parent),
            ast::Expr::TaggedTpl(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Arrow(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Class(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Yield(e) => e.bind_to_opt_parent(parent),
            ast::Expr::MetaProp(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Await(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Paren(e) => e.bind_to_opt_parent(parent),
            ast::Expr::JSXMember(e) => e.bind_to_opt_parent(parent),
            ast::Expr::JSXNamespacedName(e) => e.bind_to_opt_parent(parent),
            ast::Expr::JSXEmpty(e) => e.bind_to_opt_parent(parent),
            ast::Expr::JSXElement(e) => e.bind_to_opt_parent(parent),
            ast::Expr::JSXFragment(e) => e.bind_to_opt_parent(parent),
            ast::Expr::TsTypeAssertion(e) => e.bind_to_opt_parent(parent),
            ast::Expr::TsConstAssertion(e) => e.bind_to_opt_parent(parent),
            ast::Expr::TsNonNull(e) => e.bind_to_opt_parent(parent),
            ast::Expr::TsAs(e) => e.bind_to_opt_parent(parent),
            ast::Expr::PrivateName(e) => e.bind_to_opt_parent(parent),
            ast::Expr::OptChain(e) => e.bind_to_opt_parent(parent),
            ast::Expr::Invalid(e) => e.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::Lit {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::Lit::Str(l) => l.bind_to_opt_parent(parent),
            ast::Lit::Bool(l) => l.bind_to_opt_parent(parent),
            ast::Lit::Null(l) => l.bind_to_opt_parent(parent),
            ast::Lit::Num(l) => l.bind_to_opt_parent(parent),
            ast::Lit::BigInt(l) => l.bind_to_opt_parent(parent),
            ast::Lit::Regex(l) => l.bind_to_opt_parent(parent),
            ast::Lit::JSXText(l) => l.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::VarDeclOrExpr {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::VarDeclOrExpr::VarDecl(n) => n.bind_to_opt_parent(parent),
            ast::VarDeclOrExpr::Expr(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::VarDeclOrPat {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::VarDeclOrPat::VarDecl(n) => n.bind_to_opt_parent(parent),
            ast::VarDeclOrPat::Pat(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::Pat {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::Pat::Ident(p) => p.bind_to_opt_parent(parent),
            ast::Pat::Array(p) => p.bind_to_opt_parent(parent),
            ast::Pat::Rest(p) => p.bind_to_opt_parent(parent),
            ast::Pat::Object(p) => p.bind_to_opt_parent(parent),
            ast::Pat::Assign(p) => p.bind_to_opt_parent(parent),
            ast::Pat::Invalid(p) => p.bind_to_opt_parent(parent),
            ast::Pat::Expr(p) => p.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::ModuleItem {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::ModuleItem::ModuleDecl(i) => i.bind_to_opt_parent(parent),
            ast::ModuleItem::Stmt(i) => i.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::ModuleDecl {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::ModuleDecl::Import(d) => d.bind_to_opt_parent(parent),
            ast::ModuleDecl::ExportDecl(d) => d.bind_to_opt_parent(parent),
            ast::ModuleDecl::ExportNamed(d) => d.bind_to_opt_parent(parent),
            ast::ModuleDecl::ExportDefaultDecl(d) => d.bind_to_opt_parent(parent),
            ast::ModuleDecl::ExportDefaultExpr(d) => d.bind_to_opt_parent(parent),
            ast::ModuleDecl::ExportAll(d) => d.bind_to_opt_parent(parent),
            ast::ModuleDecl::TsImportEquals(d) => d.bind_to_opt_parent(parent),
            ast::ModuleDecl::TsExportAssignment(d) => d.bind_to_opt_parent(parent),
            ast::ModuleDecl::TsNamespaceExport(d) => d.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::ClassMember {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::ClassMember::Constructor(m) => m.bind_to_opt_parent(parent),
            ast::ClassMember::Method(m) => m.bind_to_opt_parent(parent),
            ast::ClassMember::PrivateMethod(m) => m.bind_to_opt_parent(parent),
            ast::ClassMember::ClassProp(m) => m.bind_to_opt_parent(parent),
            ast::ClassMember::PrivateProp(m) => m.bind_to_opt_parent(parent),
            ast::ClassMember::TsIndexSignature(m) => m.bind_to_opt_parent(parent),
            ast::ClassMember::Empty(m) => m.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::PropName {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::PropName::Ident(p) => p.bind_to_opt_parent(parent),
            ast::PropName::Str(p) => p.bind_to_opt_parent(parent),
            ast::PropName::Num(p) => p.bind_to_opt_parent(parent),
            ast::PropName::BigInt(p) => p.bind_to_opt_parent(parent),
            ast::PropName::Computed(p) => p.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::ParamOrTsParamProp {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::ParamOrTsParamProp::TsParamProp(p) => p.bind_to_opt_parent(parent),
            ast::ParamOrTsParamProp::Param(p) => p.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::Prop {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::Prop::Shorthand(p) => p.bind_to_opt_parent(parent),
            ast::Prop::KeyValue(p) => p.bind_to_opt_parent(parent),
            ast::Prop::Assign(p) => p.bind_to_opt_parent(parent),
            ast::Prop::Getter(p) => p.bind_to_opt_parent(parent),
            ast::Prop::Setter(p) => p.bind_to_opt_parent(parent),
            ast::Prop::Method(p) => p.bind_to_opt_parent(parent),
            ast::Prop::Spread(p) => p.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::ExprOrSuper {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::ExprOrSuper::Super(n) => n.bind_to_opt_parent(parent),
            ast::ExprOrSuper::Expr(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::PatOrExpr {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::PatOrExpr::Expr(n) => n.bind_to_opt_parent(parent),
            ast::PatOrExpr::Pat(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::BlockStmtOrExpr {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::BlockStmtOrExpr::BlockStmt(n) => n.bind_to_opt_parent(parent),
            ast::BlockStmtOrExpr::Expr(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::Program {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::Program::Module(p) => p.bind_to_opt_parent(parent),
            ast::Program::Script(p) => p.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::ObjectPatProp {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::ObjectPatProp::KeyValue(n) => n.bind_to_opt_parent(parent),
            ast::ObjectPatProp::Assign(n) => n.bind_to_opt_parent(parent),
            ast::ObjectPatProp::Rest(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsType {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsType::TsKeywordType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsThisType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsFnOrConstructorType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsTypeRef(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsTypeQuery(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsTypeLit(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsArrayType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsTupleType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsOptionalType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsRestType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsUnionOrIntersectionType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsConditionalType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsInferType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsParenthesizedType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsTypeOperator(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsIndexedAccessType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsMappedType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsLitType(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsTypePredicate(t) => t.bind_to_opt_parent(parent),
            ast::TsType::TsImportType(t) => t.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsUnionOrIntersectionType {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsUnionOrIntersectionType::TsUnionType(t) => t.bind_to_opt_parent(parent),
            ast::TsUnionOrIntersectionType::TsIntersectionType(t) => t.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsFnOrConstructorType {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsFnOrConstructorType::TsFnType(t) => t.bind_to_opt_parent(parent),
            ast::TsFnOrConstructorType::TsConstructorType(t) => t.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsTypeElement {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsTypeElement::TsCallSignatureDecl(t) => t.bind_to_opt_parent(parent),
            ast::TsTypeElement::TsConstructSignatureDecl(t) => t.bind_to_opt_parent(parent),
            ast::TsTypeElement::TsPropertySignature(t) => t.bind_to_opt_parent(parent),
            ast::TsTypeElement::TsGetterSignature(t) => t.bind_to_opt_parent(parent),
            ast::TsTypeElement::TsSetterSignature(t) => t.bind_to_opt_parent(parent),
            ast::TsTypeElement::TsMethodSignature(t) => t.bind_to_opt_parent(parent),
            ast::TsTypeElement::TsIndexSignature(t) => t.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsAmbientParamPat {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsAmbientParamPat::Ident(p) => p.bind_to_opt_parent(parent),
            ast::TsAmbientParamPat::Array(p) => p.bind_to_opt_parent(parent),
            ast::TsAmbientParamPat::Rest(p) => p.bind_to_opt_parent(parent),
            ast::TsAmbientParamPat::Object(p) => p.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsEntityName {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsEntityName::TsQualifiedName(n) => n.bind_to_opt_parent(parent),
            ast::TsEntityName::Ident(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsThisTypeOrIdent {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsThisTypeOrIdent::TsThisType(n) => n.bind_to_opt_parent(parent),
            ast::TsThisTypeOrIdent::Ident(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsNamespaceBody {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsNamespaceBody::TsModuleBlock(b) => b.bind_to_opt_parent(parent),
            ast::TsNamespaceBody::TsNamespaceDecl(b) => b.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsModuleName {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsModuleName::Ident(n) => n.bind_to_opt_parent(parent),
            ast::TsModuleName::Str(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsLit {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsLit::BigInt(l) => l.bind_to_opt_parent(parent),
            ast::TsLit::Number(l) => l.bind_to_opt_parent(parent),
            ast::TsLit::Str(l) => l.bind_to_opt_parent(parent),
            ast::TsLit::Bool(l) => l.bind_to_opt_parent(parent),
            ast::TsLit::Tpl(l) => l.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::JSXObject {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::JSXObject::JSXMemberExpr(n) => n.bind_to_opt_parent(parent),
            ast::JSXObject::Ident(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::JSXExpr {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::JSXExpr::JSXEmptyExpr(n) => n.bind_to_opt_parent(parent),
            ast::JSXExpr::Expr(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::JSXElementName {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::JSXElementName::Ident(n) => n.bind_to_opt_parent(parent),
            ast::JSXElementName::JSXMemberExpr(n) => n.bind_to_opt_parent(parent),
            ast::JSXElementName::JSXNamespacedName(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::JSXAttrOrSpread {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::JSXAttrOrSpread::JSXAttr(n) => n.bind_to_opt_parent(parent),
            ast::JSXAttrOrSpread::SpreadElement(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::JSXAttrName {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::JSXAttrName::Ident(n) => n.bind_to_opt_parent(parent),
            ast::JSXAttrName::JSXNamespacedName(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::JSXAttrValue {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::JSXAttrValue::Lit(n) => n.bind_to_opt_parent(parent),
            ast::JSXAttrValue::JSXExprContainer(n) => n.bind_to_opt_parent(parent),
            ast::JSXAttrValue::JSXElement(n) => n.bind_to_opt_parent(parent),
            ast::JSXAttrValue::JSXFragment(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::JSXElementChild {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::JSXElementChild::JSXText(n) => n.bind_to_opt_parent(parent),
            ast::JSXElementChild::JSXExprContainer(n) => n.bind_to_opt_parent(parent),
            ast::JSXElementChild::JSXSpreadChild(n) => n.bind_to_opt_parent(parent),
            ast::JSXElementChild::JSXElement(n) => n.bind_to_opt_parent(parent),
            ast::JSXElementChild::JSXFragment(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::DefaultDecl {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::DefaultDecl::Class(n) => n.bind_to_opt_parent(parent),
            ast::DefaultDecl::Fn(n) => n.bind_to_opt_parent(parent),
            ast::DefaultDecl::TsInterfaceDecl(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::ImportSpecifier {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::ImportSpecifier::Named(n) => n.bind_to_opt_parent(parent),
            ast::ImportSpecifier::Default(n) => n.bind_to_opt_parent(parent),
            ast::ImportSpecifier::Namespace(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::ExportSpecifier {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::ExportSpecifier::Namespace(n) => n.bind_to_opt_parent(parent),
            ast::ExportSpecifier::Default(n) => n.bind_to_opt_parent(parent),
            ast::ExportSpecifier::Named(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsParamPropParam {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsParamPropParam::Ident(n) => n.bind_to_opt_parent(parent),
            ast::TsParamPropParam::Assign(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsTypeQueryExpr {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsTypeQueryExpr::TsEntityName(n) => n.bind_to_opt_parent(parent),
            ast::TsTypeQueryExpr::Import(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsEnumMemberId {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsEnumMemberId::Ident(n) => n.bind_to_opt_parent(parent),
            ast::TsEnumMemberId::Str(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::TsModuleRef {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsModuleRef::TsEntityName(n) => n.bind_to_opt_parent(parent),
            ast::TsModuleRef::TsExternalModuleRef(n) => n.bind_to_opt_parent(parent),
        }
    }
}

impl Bind for ast::ExprOrSpread {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::ExprOrSpread::Spread(n) => n.bind_to_opt_parent(parent),
            ast::ExprOrSpread::Expr(n) => n.bind_to_opt_parent(parent),
        }
    }
}
// impl Bind for ast:: {
//     fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
//         match self {

//         }
//     }
// }

macro_rules! make {
    ($($field:ident,)*) => {
        // Enum declaration:
        #[derive(PartialEq, Eq, Hash, Clone)]
        pub enum BoundNode {
            $($field(Rc<$field>),)*
        }
        #[derive(PartialEq, Eq, Hash, Clone)]
        pub enum Node {
            $($field(Rc<ast::$field>),)*
        }

        // Enum impls:

        impl fmt::Debug for BoundNode {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                // let (name, span) = match self {
                //     $(
                //         BoundNode::$field(a) => (stringify!($field), a.span()),
                //     )*
                // };
                // f.write_fmt(format_args!("({}, {:?})", name, span))

                // todo!();

                //  match self {
                //     $(
                //         BoundNode::$field(_) => f.write_str(stringify!($field)),
                //     )*
                // }

                match self {
                    $(
                        BoundNode::$field(_) => f.write_fmt(format_args!("({}, {:?})", stringify!($field), self.span())),
                    )*
                }

            }
        }

        impl fmt::Debug for Node {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        Node::$field(_) => f.write_fmt(format_args!("({}, {:?})", stringify!($field), self.span())),
                    )*
                }

            }
        }

        impl BoundNode {
            pub fn parent(&self) -> Option<Self> {
                match self {
                    $(
                        Self::$field(node) => {node.parent.clone()},
                    )*
                }
            }
            pub fn visit_with<V:Visit>(&self, visitor: &mut V) {
                match self {
                    $(
                        Self::$field(node) => {node.node.visit_with(visitor, node.parent.clone())},
                    )*
                }
            }
        }

        $(
            impl From<Rc<$field>> for BoundNode {
                fn from(other: Rc<$field>) -> BoundNode {
                    BoundNode::$field(other)
                }
            }

            impl From<Rc<$field>> for Node {
                fn from(other: Rc<$field>) -> Node {
                    Node::$field(other.node.clone())
                }
            }

            impl From<Rc<ast::$field>> for Node {
                fn from(other: Rc<ast::$field>) -> Node {
                    Node::$field(other)
                }
            }
        )*

        // Node structs and impls:
        $(
            impl Bind for Rc<ast::$field> {
                fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
                    BoundNode::$field(Rc::new($field {
                        node: self.clone(),
                        parent,
                    }))
                }
            }

            #[derive(PartialEq, Hash, Debug)]
            pub struct $field {
                pub node: Rc<ast::$field>,
                pub parent: Option<BoundNode>,
            }

            impl $field {
                pub fn new(node: Rc<ast::$field>, parent: Option<BoundNode>) -> Rc<Self> {
                    Rc::new(Self {node, parent})
                }
            }

            impl Eq for $field {}

            impl Deref for $field {
                type Target = ast::$field;

                fn deref(&self) -> &Self::Target {
                    self.node.as_ref()
                }
            }

            // impl fmt::Debug for $field {
            //     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            //         f.write_str(stringify!($field))
            //     }
            // }
        )*

        impl From<BoundNode> for Node {
            fn from(other: BoundNode) -> Node {
                match other {
                    $(
                        BoundNode::$field(node) => {Node::$field(node.node.clone())},
                    )*
                }
            }
        }
    };
}

make!(
    // class
    Class,
    ClassProp,
    PrivateProp,
    ClassMethod,
    PrivateMethod,
    Constructor,
    Decorator,
    // decl
    FnDecl,
    ClassDecl,
    VarDecl,
    VarDeclarator,
    // Expr
    ThisExpr,
    ArrayLit,
    ObjectLit,
    SpreadElement,
    UnaryExpr,
    UpdateExpr,
    BinExpr,
    FnExpr,
    ClassExpr,
    AssignExpr,
    MemberExpr,
    CondExpr,
    CallExpr,
    NewExpr,
    SeqExpr,
    ArrowExpr,
    YieldExpr,
    MetaPropExpr,
    AwaitExpr,
    Tpl,
    TaggedTpl,
    TplElement,
    ParenExpr,
    Super,
    OptChainExpr,
    // function
    Function,
    Param,
    ParamWithoutDecorators,
    //ident
    BindingIdent,
    Ident,
    PrivateName,
    //jsx
    JSXMemberExpr,
    JSXNamespacedName,
    JSXEmptyExpr,
    JSXExprContainer,
    JSXSpreadChild,
    JSXOpeningElement,
    JSXClosingElement,
    JSXAttr,
    JSXText,
    JSXElement,
    JSXFragment,
    JSXOpeningFragment,
    JSXClosingFragment,
    //lib
    Invalid,
    // Lit
    Str,
    Bool,
    Null,
    Number,
    BigInt,
    Regex,
    // module_decl
    ExportDefaultExpr,
    ExportDecl,
    ImportDecl,
    ExportAll,
    NamedExport,
    ExportDefaultDecl,
    ImportDefaultSpecifier,
    ImportStarAsSpecifier,
    ImportNamedSpecifier,
    ExportNamespaceSpecifier,
    ExportDefaultSpecifier,
    ExportNamedSpecifier,
    //module
    Script,
    Module,
    //pat
    ArrayPat,
    ObjectPat,
    AssignPat,
    RestPat,
    KeyValuePatProp,
    AssignPatProp,
    //prop
    KeyValueProp,
    AssignProp,
    GetterProp,
    SetterProp,
    MethodProp,
    ComputedPropName,
    SpreadAssignment,
    //stmt
    BlockStmt,
    ExprStmt,
    EmptyStmt,
    DebuggerStmt,
    WithStmt,
    ReturnStmt,
    LabeledStmt,
    BreakStmt,
    ContinueStmt,
    IfStmt,
    SwitchStmt,
    ThrowStmt,
    TryStmt,
    WhileStmt,
    DoWhileStmt,
    ForStmt,
    ForInStmt,
    ForOfStmt,
    SwitchCase,
    CatchClause,
    // typescript
    TsTypeAnn,
    TsTypeParamDecl,
    TsTypeParam,
    TsTypeParamInstantiation,
    TsParamProp,
    TsQualifiedName,
    TsCallSignatureDecl,
    TsConstructSignatureDecl,
    TsPropertySignature,
    TsGetterSignature,
    TsSetterSignature,
    TsMethodSignature,
    TsIndexSignature,
    TsKeywordType,
    TsThisType,
    TsAmbientParam,
    TsFnType,
    TsConstructorType,
    TsTypeRef,
    TsTypePredicate,
    TsTypeQuery,
    TsImportType,
    TsTypeLit,
    TsArrayType,
    TsTupleType,
    TsTupleElement,
    TsOptionalType,
    TsRestType,
    TsUnionType,
    TsIntersectionType,
    TsConditionalType,
    TsInferType,
    TsParenthesizedType,
    TsTypeOperator,
    TsIndexedAccessType,
    TsMappedType,
    TsLitType,
    TsTplLitType,
    TsInterfaceDecl,
    TsInterfaceBody,
    TsExprWithTypeArgs,
    TsTypeAliasDecl,
    TsEnumDecl,
    TsEnumMember,
    TsModuleDecl,
    TsModuleBlock,
    TsNamespaceDecl,
    TsImportEqualsDecl,
    TsExternalModuleRef,
    TsExportAssignment,
    TsNamespaceExportDecl,
    TsAsExpr,
    TsTypeAssertion,
    TsNonNullExpr,
    TsConstAssertion,
);

macro_rules! impl_span {
    ($ty:ty) => {
        impl $ty {
            pub fn span(&self) -> global_common::Span {
                match self {
                    Self::Class(n) => n.span,
                    Self::ClassProp(n) => n.span,
                    Self::PrivateProp(n) => n.span,
                    Self::ClassMethod(n) => n.span,
                    Self::PrivateMethod(n) => n.span,
                    Self::Constructor(n) => n.span,
                    Self::Decorator(n) => n.span,
                    Self::FnDecl(n) => n.function.span,
                    Self::ClassDecl(c) => c.class.span,
                    Self::VarDecl(n) => n.span,
                    Self::VarDeclarator(n) => n.span,
                    Self::ThisExpr(n) => n.span,
                    Self::ArrayLit(n) => n.span,
                    Self::ObjectLit(n) => n.span,
                    Self::SpreadElement(n) => n.dot3_token.with_hi(n.expr.span().hi()),
                    Self::UnaryExpr(n) => n.span,
                    Self::UpdateExpr(n) => n.span,
                    Self::BinExpr(n) => n.span,
                    Self::FnExpr(n) => n.function.span,
                    Self::ClassExpr(c) => c.class.span,
                    Self::AssignExpr(n) => n.span,
                    Self::MemberExpr(n) => n.span,
                    Self::CondExpr(n) => n.span,
                    Self::CallExpr(n) => n.span,
                    Self::NewExpr(n) => n.span,
                    Self::SeqExpr(n) => n.span,
                    Self::ArrowExpr(n) => n.span,
                    Self::YieldExpr(n) => n.span,
                    Self::MetaPropExpr(n) => n.meta.span.with_hi(n.prop.span.hi()),
                    Self::AwaitExpr(n) => n.span,
                    Self::Tpl(n) => n.span,
                    Self::TaggedTpl(n) => n.span,
                    Self::TplElement(n) => n.span,
                    Self::ParenExpr(n) => n.span,
                    Self::Super(n) => n.span,
                    Self::OptChainExpr(n) => n.span,
                    Self::Function(n) => n.span,
                    Self::Param(n) => n.span,
                    Self::ParamWithoutDecorators(n) => n.pat.span(),
                    Self::BindingIdent(n) => n.id.span,
                    Self::Ident(n) => n.span,
                    Self::PrivateName(n) => n.span,
                    Self::JSXMemberExpr(_) => todo!("JSXMemberExpr"),
                    Self::JSXNamespacedName(_) => todo!("JSXNamespacedName"),
                    Self::JSXEmptyExpr(n) => n.span,
                    Self::JSXExprContainer(n) => n.span,
                    Self::JSXSpreadChild(n) => n.span,
                    Self::JSXOpeningElement(n) => n.span,
                    Self::JSXClosingElement(n) => n.span,
                    Self::JSXAttr(n) => n.span,
                    Self::JSXText(n) => n.span,
                    Self::JSXElement(n) => n.span,
                    Self::JSXFragment(n) => n.span,
                    Self::JSXOpeningFragment(n) => n.span,
                    Self::JSXClosingFragment(n) => n.span,
                    Self::Invalid(n) => n.span,
                    Self::Str(n) => n.span,
                    Self::Bool(n) => n.span,
                    Self::Null(n) => n.span,
                    Self::Number(n) => n.span,
                    Self::BigInt(n) => n.span,
                    Self::Regex(n) => n.span,
                    Self::ExportDefaultExpr(n) => n.span,
                    Self::ExportDecl(n) => n.span,
                    Self::ImportDecl(n) => n.span,
                    Self::ExportAll(n) => n.span,
                    Self::NamedExport(n) => n.span,
                    Self::ExportDefaultDecl(n) => n.span,
                    Self::ImportDefaultSpecifier(n) => n.span,
                    Self::ImportStarAsSpecifier(n) => n.span,
                    Self::ImportNamedSpecifier(n) => n.span,
                    Self::ExportNamespaceSpecifier(n) => n.span,
                    Self::ExportDefaultSpecifier(_) => todo!("ExportDefaultSpecifier"),
                    Self::ExportNamedSpecifier(n) => n.span,
                    Self::Script(n) => n.span,
                    Self::Module(n) => n.span,
                    Self::ArrayPat(n) => n.span,
                    Self::ObjectPat(n) => n.span,
                    Self::AssignPat(n) => n.span,
                    Self::RestPat(n) => n.span,
                    Self::KeyValuePatProp(n) => n.key.span().with_hi(n.value.span().hi()),
                    Self::AssignPatProp(n) => n.span,
                    Self::KeyValueProp(n) => n.key.span().with_hi(n.value.span().hi()),
                    Self::AssignProp(n) => n.key.span.with_hi(n.value.span().hi()),
                    Self::GetterProp(n) => n.span,
                    Self::SetterProp(n) => n.span,
                    Self::MethodProp(n) => n.function.span,
                    Self::ComputedPropName(n) => n.span,
                    Self::SpreadAssignment(n) => n.dot3_token.with_hi(n.expr.span().hi()),
                    Self::BlockStmt(n) => n.span,
                    Self::ExprStmt(n) => n.span,
                    Self::EmptyStmt(n) => n.span,
                    Self::DebuggerStmt(n) => n.span,
                    Self::WithStmt(n) => n.span,
                    Self::ReturnStmt(n) => n.span,
                    Self::LabeledStmt(n) => n.span,
                    Self::BreakStmt(n) => n.span,
                    Self::ContinueStmt(n) => n.span,
                    Self::IfStmt(n) => n.span,
                    Self::SwitchStmt(n) => n.span,
                    Self::ThrowStmt(n) => n.span,
                    Self::TryStmt(n) => n.span,
                    Self::WhileStmt(n) => n.span,
                    Self::DoWhileStmt(n) => n.span,
                    Self::ForStmt(n) => n.span,
                    Self::ForInStmt(n) => n.span,
                    Self::ForOfStmt(n) => n.span,
                    Self::SwitchCase(n) => n.span,
                    Self::CatchClause(n) => n.span,
                    Self::TsTypeAnn(n) => n.span,
                    Self::TsTypeParamDecl(n) => n.span,
                    Self::TsTypeParam(n) => n.span,
                    Self::TsTypeParamInstantiation(n) => n.span,
                    Self::TsParamProp(n) => n.span,
                    Self::TsQualifiedName(n) => n.left.span().with_hi(n.right.span.hi()),
                    Self::TsCallSignatureDecl(n) => n.span,
                    Self::TsConstructSignatureDecl(n) => n.span,
                    Self::TsPropertySignature(n) => n.span,
                    Self::TsGetterSignature(n) => n.span,
                    Self::TsSetterSignature(n) => n.span,
                    Self::TsMethodSignature(n) => n.span,
                    Self::TsIndexSignature(n) => n.span,
                    Self::TsKeywordType(n) => n.span,
                    Self::TsThisType(n) => n.span,
                    Self::TsAmbientParam(n) => n.pat.span(),
                    Self::TsFnType(n) => n.span,
                    Self::TsConstructorType(n) => n.span,
                    Self::TsTypeRef(n) => n.span,
                    Self::TsTypePredicate(n) => n.span,
                    Self::TsTypeQuery(n) => n.span,
                    Self::TsImportType(n) => n.span,
                    Self::TsTypeLit(n) => n.span,
                    Self::TsArrayType(n) => n.span,
                    Self::TsTupleType(n) => n.span,
                    Self::TsTupleElement(n) => n.span,
                    Self::TsOptionalType(n) => n.span,
                    Self::TsRestType(n) => n.span,
                    Self::TsUnionType(n) => n.span,
                    Self::TsIntersectionType(n) => n.span,
                    Self::TsConditionalType(n) => n.span,
                    Self::TsInferType(n) => n.span,
                    Self::TsParenthesizedType(n) => n.span,
                    Self::TsTypeOperator(n) => n.span,
                    Self::TsIndexedAccessType(n) => n.span,
                    Self::TsMappedType(n) => n.span,
                    Self::TsLitType(n) => n.span,
                    Self::TsTplLitType(n) => n.span,
                    Self::TsInterfaceDecl(n) => n.span,
                    Self::TsInterfaceBody(n) => n.span,
                    Self::TsExprWithTypeArgs(n) => n.span,
                    Self::TsTypeAliasDecl(n) => n.span,
                    Self::TsEnumDecl(n) => n.span,
                    Self::TsEnumMember(n) => n.span,
                    Self::TsModuleDecl(n) => n.span,
                    Self::TsModuleBlock(n) => n.span,
                    Self::TsNamespaceDecl(n) => n.span,
                    Self::TsImportEqualsDecl(n) => n.span,
                    Self::TsExternalModuleRef(n) => n.span,
                    Self::TsExportAssignment(n) => n.span,
                    Self::TsNamespaceExportDecl(n) => n.span,
                    Self::TsAsExpr(n) => n.span,
                    Self::TsTypeAssertion(n) => n.span,
                    Self::TsNonNullExpr(n) => n.span,
                    Self::TsConstAssertion(n) => n.span,
                }
            }
        }
    };
}

impl_span!(BoundNode);
impl_span!(Node);

impl From<ast::Pat> for Node {
    fn from(other: ast::Pat) -> Self {
        match other {
            ast::Pat::Ident(n) => Node::BindingIdent(n),
            ast::Pat::Array(n) => Node::ArrayPat(n),
            ast::Pat::Rest(n) => Node::RestPat(n),
            ast::Pat::Object(n) => Node::ObjectPat(n),
            ast::Pat::Assign(n) => Node::AssignPat(n),
            ast::Pat::Invalid(n) => Node::Invalid(n),
            ast::Pat::Expr(n) => Node::from(n),
        }
    }
}

impl From<ast::Expr> for Node {
    fn from(other: ast::Expr) -> Self {
        match other {
            ast::Expr::This(n) => Node::ThisExpr(n),
            ast::Expr::Array(n) => Node::ArrayLit(n),
            ast::Expr::Object(n) => Node::ObjectLit(n),
            ast::Expr::Fn(n) => Node::FnExpr(n),
            ast::Expr::Unary(n) => Node::UnaryExpr(n),
            ast::Expr::Update(n) => Node::UpdateExpr(n),
            ast::Expr::Bin(n) => Node::BinExpr(n),
            ast::Expr::Assign(n) => Node::AssignExpr(n),
            ast::Expr::Member(n) => Node::MemberExpr(n),
            ast::Expr::Cond(n) => Node::CondExpr(n),
            ast::Expr::Call(n) => Node::CallExpr(n),
            ast::Expr::New(n) => Node::NewExpr(n),
            ast::Expr::Seq(n) => Node::SeqExpr(n),
            ast::Expr::Ident(n) => Node::Ident(n),
            ast::Expr::Lit(n) => Node::from(n),
            ast::Expr::Tpl(n) => Node::Tpl(n),
            ast::Expr::TaggedTpl(n) => Node::TaggedTpl(n),
            ast::Expr::Arrow(n) => Node::ArrowExpr(n),
            ast::Expr::Class(n) => Node::ClassExpr(n),
            ast::Expr::Yield(n) => Node::YieldExpr(n),
            ast::Expr::MetaProp(n) => Node::MetaPropExpr(n),
            ast::Expr::Await(n) => Node::AwaitExpr(n),
            ast::Expr::Paren(n) => Node::ParenExpr(n),
            ast::Expr::JSXMember(n) => Node::JSXMemberExpr(n),
            ast::Expr::JSXNamespacedName(n) => Node::JSXNamespacedName(n),
            ast::Expr::JSXEmpty(n) => Node::JSXEmptyExpr(n),
            ast::Expr::JSXElement(n) => Node::JSXElement(n),
            ast::Expr::JSXFragment(n) => Node::JSXFragment(n),
            ast::Expr::TsTypeAssertion(n) => Node::TsTypeAssertion(n),
            ast::Expr::TsConstAssertion(n) => Node::TsConstAssertion(n),
            ast::Expr::TsNonNull(n) => Node::TsNonNullExpr(n),
            ast::Expr::TsAs(n) => Node::TsAsExpr(n),
            ast::Expr::PrivateName(n) => Node::PrivateName(n),
            ast::Expr::OptChain(n) => Node::OptChainExpr(n),
            ast::Expr::Invalid(n) => Node::Invalid(n),
        }
    }
}

impl From<ast::Lit> for Node {
    fn from(other: ast::Lit) -> Self {
        match other {
            ast::Lit::Str(n) => Node::Str(n),
            ast::Lit::Bool(n) => Node::Bool(n),
            ast::Lit::Null(n) => Node::Null(n),
            ast::Lit::Num(n) => Node::Number(n),
            ast::Lit::BigInt(n) => Node::BigInt(n),
            ast::Lit::Regex(n) => Node::Regex(n),
            ast::Lit::JSXText(n) => Node::JSXText(n),
        }
    }
}

impl From<ast::ParamOrTsParamProp> for Node {
    fn from(other: ast::ParamOrTsParamProp) -> Self {
        match other {
            ast::ParamOrTsParamProp::TsParamProp(n) => Node::TsParamProp(n),
            ast::ParamOrTsParamProp::Param(n) => Node::Param(n),
        }
    }
}

impl From<ast::BlockStmtOrExpr> for Node {
    fn from(other: ast::BlockStmtOrExpr) -> Self {
        match other {
            ast::BlockStmtOrExpr::BlockStmt(n) => Node::BlockStmt(n),
            ast::BlockStmtOrExpr::Expr(n) => Node::from(n),
        }
    }
}

impl From<ast::Prop> for Node {
    fn from(other: ast::Prop) -> Self {
        match other {
            ast::Prop::Shorthand(n) => Node::Ident(n),
            ast::Prop::KeyValue(n) => Node::KeyValueProp(n),
            ast::Prop::Assign(n) => Node::AssignProp(n),
            ast::Prop::Getter(n) => Node::GetterProp(n),
            ast::Prop::Setter(n) => Node::SetterProp(n),
            ast::Prop::Method(n) => Node::MethodProp(n),
            ast::Prop::Spread(n) => Node::SpreadAssignment(n),
        }
    }
}

impl From<ast::ExprOrSuper> for Node {
    fn from(other: ast::ExprOrSuper) -> Self {
        match other {
            ast::ExprOrSuper::Super(n) => Node::Super(n),
            ast::ExprOrSuper::Expr(n) => Node::from(n),
        }
    }
}

impl From<ast::TsAmbientParamPat> for Node {
    fn from(other: ast::TsAmbientParamPat) -> Self {
        match other {
            ast::TsAmbientParamPat::Ident(n) => Node::BindingIdent(n),
            ast::TsAmbientParamPat::Array(n) => Node::ArrayPat(n),
            ast::TsAmbientParamPat::Rest(n) => Node::RestPat(n),
            ast::TsAmbientParamPat::Object(n) => Node::ObjectPat(n),
        }
    }
}
impl From<ast::TsType> for Node {
    fn from(other: ast::TsType) -> Self {
        match other {
            ast::TsType::TsKeywordType(n) => Node::TsKeywordType(n),
            ast::TsType::TsThisType(n) => Node::TsThisType(n),
            ast::TsType::TsFnOrConstructorType(n) => Node::from(n),
            ast::TsType::TsTypeRef(n) => Node::TsTypeRef(n),
            ast::TsType::TsTypeQuery(n) => Node::TsTypeQuery(n),
            ast::TsType::TsTypeLit(n) => Node::TsTypeLit(n),
            ast::TsType::TsArrayType(n) => Node::TsArrayType(n),
            ast::TsType::TsTupleType(n) => Node::TsTupleType(n),
            ast::TsType::TsOptionalType(n) => Node::TsOptionalType(n),
            ast::TsType::TsRestType(n) => Node::TsRestType(n),
            ast::TsType::TsUnionOrIntersectionType(n) => Node::from(n),
            ast::TsType::TsConditionalType(n) => Node::TsConditionalType(n),
            ast::TsType::TsInferType(n) => Node::TsInferType(n),
            ast::TsType::TsParenthesizedType(n) => Node::TsParenthesizedType(n),
            ast::TsType::TsTypeOperator(n) => Node::TsTypeOperator(n),
            ast::TsType::TsIndexedAccessType(n) => Node::TsIndexedAccessType(n),
            ast::TsType::TsMappedType(n) => Node::TsMappedType(n),
            ast::TsType::TsLitType(n) => Node::TsLitType(n),
            ast::TsType::TsTypePredicate(n) => Node::TsTypePredicate(n),
            ast::TsType::TsImportType(n) => Node::TsImportType(n),
        }
    }
}
impl From<ast::TsFnOrConstructorType> for Node {
    fn from(other: ast::TsFnOrConstructorType) -> Self {
        match other {
            ast::TsFnOrConstructorType::TsFnType(n) => Node::TsFnType(n),
            ast::TsFnOrConstructorType::TsConstructorType(n) => Node::TsConstructorType(n),
        }
    }
}
impl From<ast::TsUnionOrIntersectionType> for Node {
    fn from(other: ast::TsUnionOrIntersectionType) -> Self {
        match other {
            ast::TsUnionOrIntersectionType::TsUnionType(n) => Node::TsUnionType(n),
            ast::TsUnionOrIntersectionType::TsIntersectionType(n) => Node::TsIntersectionType(n),
        }
    }
}
impl From<ast::TsEntityName> for Node {
    fn from(other: ast::TsEntityName) -> Self {
        match other {
            ast::TsEntityName::TsQualifiedName(n) => Node::TsQualifiedName(n),
            ast::TsEntityName::Ident(n) => Node::Ident(n),
        }
    }
}
impl From<ast::ExprOrSpread> for Node {
    fn from(other: ast::ExprOrSpread) -> Self {
        match other {
            ast::ExprOrSpread::Spread(n) => Node::SpreadElement(n),
            ast::ExprOrSpread::Expr(n) => Node::from(n),
        }
    }
}
// impl From<ast::> for Node {
//     fn from(other: ast::) -> Self {
//         match other {

//         }
//     }
// }
