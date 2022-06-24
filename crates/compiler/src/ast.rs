use crate::node::{Bind, BoundNode};
use ast;
pub use ast::{GetNodeId, NodeId};
use ast_convert::define;
use global_common::{integer_decode::integer_decode, EqIgnoreSpan, Span, Spanned};
use num_bigint::BigInt as BigIntValue;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use swc_atoms::JsWord;

impl Eq for Number {}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.span.hash(state);
        integer_decode(self.value).hash(state);
    }
}

impl From<Expr> for ExprOrSuper {
    fn from(expr: Expr) -> ExprOrSuper {
        ExprOrSuper::Expr(expr)
    }
}

impl From<TsAmbientParamPat> for Pat {
    fn from(other: TsAmbientParamPat) -> Self {
        match other {
            TsAmbientParamPat::Ident(n) => Pat::Ident(n),
            TsAmbientParamPat::Array(n) => Pat::Array(n),
            TsAmbientParamPat::Rest(n) => Pat::Rest(n),
            TsAmbientParamPat::Object(n) => Pat::Object(n),
        }
    }
}

impl From<TsParamPropParam> for Pat {
    fn from(other: TsParamPropParam) -> Self {
        match other {
            TsParamPropParam::Ident(n) => Pat::Ident(n),
            TsParamPropParam::Assign(n) => Pat::Assign(n),
        }
    }
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::This(n) => n.span,
            Expr::Array(n) => n.span,
            Expr::Object(n) => n.span,
            Expr::Fn(n) => n.function.span,
            Expr::Unary(n) => n.span,
            Expr::Update(n) => n.span,
            Expr::Bin(n) => n.span,
            Expr::Assign(n) => n.span,
            Expr::Member(n) => n.span,
            Expr::Cond(n) => n.span,
            Expr::Call(n) => n.span,
            Expr::New(n) => n.span,
            Expr::Seq(n) => n.span,
            Expr::Ident(n) => n.span,
            Expr::Lit(n) => n.span(),
            Expr::Tpl(n) => n.span,
            Expr::TaggedTpl(n) => n.span,
            Expr::Arrow(n) => n.span,
            Expr::Class(n) => n.class.span,
            Expr::Yield(n) => n.span,
            Expr::MetaProp(_) => todo!(),
            Expr::Await(n) => n.span,
            Expr::Paren(n) => n.span,
            Expr::JSXMember(_) => todo!(),
            Expr::JSXNamespacedName(_) => todo!(),
            Expr::JSXEmpty(n) => n.span,
            Expr::JSXElement(n) => n.span,
            Expr::JSXFragment(n) => n.span,
            Expr::TsTypeAssertion(n) => n.span,
            Expr::TsConstAssertion(n) => n.span,
            Expr::TsNonNull(n) => n.span,
            Expr::TsAs(n) => n.span,
            Expr::PrivateName(n) => n.span,
            Expr::OptChain(n) => n.span,
            Expr::Invalid(n) => n.span,
        }
    }
}

impl Spanned for Lit {
    fn span(&self) -> Span {
        match self {
            Lit::Str(n) => n.span,
            Lit::Bool(n) => n.span,
            Lit::Null(n) => n.span,
            Lit::Num(n) => n.span,
            Lit::BigInt(n) => n.span,
            Lit::Regex(n) => n.span,
            Lit::JSXText(n) => n.span,
        }
    }
}

impl Spanned for PropName {
    fn span(&self) -> Span {
        match self {
            PropName::Ident(n) => n.span,
            PropName::Str(n) => n.span,
            PropName::Num(n) => n.span,
            PropName::Computed(n) => n.span,
        }
    }
}

impl Spanned for Pat {
    fn span(&self) -> Span {
        match self {
            Pat::Ident(n) => n.id.span,
            Pat::Array(n) => n.span,
            Pat::Rest(n) => n.span,
            Pat::Object(n) => n.span,
            Pat::Assign(n) => n.span,
            Pat::Invalid(n) => n.span,
            Pat::Expr(n) => n.span(),
        }
    }
}

impl Spanned for TsEntityName {
    fn span(&self) -> Span {
        match self {
            TsEntityName::TsQualifiedName(n) => n.left.span().with_hi(n.right.span.hi()),
            TsEntityName::Ident(n) => n.span,
        }
    }
}

impl Spanned for TsAmbientParamPat {
    fn span(&self) -> Span {
        match self {
            TsAmbientParamPat::Ident(n) => n.id.span,
            TsAmbientParamPat::Array(n) => n.span,
            TsAmbientParamPat::Rest(n) => n.span,
            TsAmbientParamPat::Object(n) => n.span,
        }
    }
}

impl Spanned for Program {
    fn span(&self) -> Span {
        match self {
            Program::Module(n) => n.span,
            Program::Script(n) => n.span,
        }
    }
}

impl Prop {
    pub fn name(&self, parent: BoundNode) -> BoundNode {
        match self {
            Prop::Shorthand(n) => n.bind(parent),
            Prop::KeyValue(n) => n.key.bind(n.bind(parent)),
            Prop::Assign(n) => todo!(),
            Prop::Getter(n) => n.key.bind(n.bind(parent)),
            Prop::Setter(n) => n.key.bind(n.bind(parent)),
            Prop::Method(n) => n.key.bind(n.bind(parent)),
            Prop::Spread(n) => todo!(),
        }
    }
}

define!({
    pub struct Class {
        pub node_id: NodeId,
        pub span: Span,
        pub decorators: Vec<Decorator>,
        pub body: Vec<ClassMember>,
        pub is_abstract: bool,
        pub type_params: Option<Vec<TsTypeParamDecl>>,
        pub extends: Option<ExtendsClause>,
        pub implements: Vec<TsExprWithTypeArgs>,
    }

    pub struct ExtendsClause {
        pub node_id: NodeId,
        pub span: Span,
        pub super_class: Box<Expr>,
        pub super_type_params: Option<TsTypeParamInstantiation>,
    }

    pub enum ClassMember {
        Constructor(Constructor),
        Method(ClassMethod),
        PrivateMethod(PrivateMethod),
        ClassProp(ClassProp),
        PrivateProp(PrivateProp),
        TsIndexSignature(TsIndexSignature),
        Empty(EmptyStmt),
    }

    pub struct ClassProp {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PropName,
        pub value: Option<Box<Expr>>,
        pub type_ann: Option<TsTypeAnn>,
        pub is_static: bool,
        pub decorators: Vec<Decorator>,
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
        pub readonly: bool,
        pub declare: bool,
        pub definite: bool,
    }
    pub struct PrivateProp {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PrivateName,
        pub value: Option<Box<Expr>>,
        pub type_ann: Option<TsTypeAnn>,
        pub is_static: bool,
        pub decorators: Vec<Decorator>,
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
        pub readonly: bool,
        pub definite: bool,
    }
    pub struct ClassMethod {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PropName,
        pub function: Function,
        pub kind: MethodKind,
        pub is_static: bool,
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
    }
    pub struct PrivateMethod {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PrivateName,
        pub function: Function,
        pub kind: MethodKind,
        pub is_static: bool,
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
    }
    pub struct Constructor {
        pub node_id: NodeId,
        pub span: Span,
        pub params: Vec<ParamOrTsParamProp>,
        pub body: Option<BlockStmt>,
        pub accessibility: Option<Accessibility>,
        pub is_optional: bool,
    }
    pub struct Decorator {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub enum MethodKind {
        Method,
        Getter,
        Setter,
    }
    pub enum Decl {
        Class(ClassDecl),
        Fn(FnDecl),
        Var(VarDecl),
        TsInterface(TsInterfaceDecl),
        TsTypeAlias(TsTypeAliasDecl),
        TsEnum(TsEnumDecl),
        TsModule(TsModuleDecl),
    }
    pub struct FnDecl {
        pub node_id: NodeId,
        pub ident: Ident,
        pub declare: bool,
        pub function: Function,
    }
    pub struct ClassDecl {
        pub node_id: NodeId,
        pub ident: Ident,
        pub declare: bool,
        pub class: Class,
    }
    pub struct VarDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub kind: VarDeclKind,
        pub declare: bool,
        pub decls: Vec<VarDeclarator>,
    }
    pub enum VarDeclKind {
        Var,
        Let,
        Const,
    }
    pub struct VarDeclarator {
        pub node_id: NodeId,
        pub span: Span,
        pub name: Pat,
        pub init: Option<Box<Expr>>,
        pub definite: bool,
    }
    pub enum Expr {
        This(ThisExpr),
        Array(ArrayLit),
        Object(ObjectLit),
        Fn(FnExpr),
        Unary(UnaryExpr),
        Update(UpdateExpr),
        Bin(BinExpr),
        Assign(AssignExpr),
        Member(MemberExpr),
        Cond(CondExpr),
        Call(CallExpr),
        New(NewExpr),
        Seq(SeqExpr),
        Ident(Ident),
        Lit(Lit),
        Tpl(Tpl),
        TaggedTpl(TaggedTpl),
        Arrow(ArrowExpr),
        Class(ClassExpr),
        Yield(YieldExpr),
        MetaProp(MetaPropExpr),
        Await(AwaitExpr),
        Paren(ParenExpr),
        JSXMember(JSXMemberExpr),
        JSXNamespacedName(JSXNamespacedName),
        JSXEmpty(JSXEmptyExpr),
        JSXElement(Box<JSXElement>),
        JSXFragment(JSXFragment),
        TsTypeAssertion(TsTypeAssertion),
        TsConstAssertion(TsConstAssertion),
        TsNonNull(TsNonNullExpr),
        TsAs(TsAsExpr),
        PrivateName(PrivateName),
        OptChain(OptChainExpr),
        Invalid(Invalid),
    }
    pub struct ThisExpr {
        pub node_id: NodeId,
        pub span: Span,
    }
    pub struct ArrayLit {
        pub node_id: NodeId,
        pub span: Span,
        pub elems: Vec<Option<ExprOrSpread>>,
    }
    pub struct ObjectLit {
        pub node_id: NodeId,
        pub span: Span,
        pub props: Vec<Prop>,
    }
    pub struct SpreadElement {
        pub node_id: NodeId,
        pub dot3_token: Span,
        pub expr: Box<Expr>,
    }
    pub struct UnaryExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub op: UnaryOp,
        pub arg: Box<Expr>,
    }
    pub struct UpdateExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub op: UpdateOp,
        pub prefix: bool,
        pub arg: Box<Expr>,
    }
    pub struct BinExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub op: BinaryOp,
        pub left: Box<Expr>,
        pub right: Box<Expr>,
    }
    pub struct FnExpr {
        pub node_id: NodeId,
        pub ident: Option<Ident>,
        pub function: Function,
    }
    pub struct ClassExpr {
        pub node_id: NodeId,
        pub ident: Option<Ident>,
        pub class: Class,
    }
    pub struct AssignExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub op: AssignOp,
        pub left: PatOrExpr,
        pub right: Box<Expr>,
    }
    pub struct MemberExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub obj: ExprOrSuper,
        pub prop: Box<Expr>,
        pub computed: bool,
    }
    pub struct CondExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub test: Box<Expr>,
        pub cons: Box<Expr>,
        pub alt: Box<Expr>,
    }
    pub struct CallExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub callee: ExprOrSuper,
        pub args: Vec<ExprOrSpread>,
        pub type_args: Option<TsTypeParamInstantiation>,
    }
    pub struct NewExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub callee: Box<Expr>,
        pub args: Option<Vec<ExprOrSpread>>,
        pub type_args: Option<TsTypeParamInstantiation>,
    }
    pub struct SeqExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub exprs: Vec<Box<Expr>>,
    }
    pub struct ArrowExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub params: Vec<ParamWithoutDecorators>,
        pub body: BlockStmtOrExpr,
        pub is_async: bool,
        pub type_params: Option<Vec<TsTypeParamDecl>>,
        pub return_type: Option<TsTypeAnn>,
    }
    pub struct YieldExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub arg: Option<Box<Expr>>,
        pub delegate: bool,
    }
    pub struct MetaPropExpr {
        pub node_id: NodeId,
        pub meta: Ident,
        pub prop: Ident,
    }
    pub struct AwaitExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub arg: Box<Expr>,
    }
    pub struct Tpl {
        pub node_id: NodeId,
        pub span: Span,
        pub exprs: Vec<Box<Expr>>,
        pub quasis: Vec<TplElement>,
    }
    pub struct TaggedTpl {
        pub node_id: NodeId,
        pub span: Span,
        pub tag: Box<Expr>,
        pub type_params: Option<TsTypeParamInstantiation>,
        pub tpl: Tpl,
    }
    pub struct TplElement {
        pub node_id: NodeId,
        pub span: Span,
        pub tail: bool,
        pub cooked: Option<Str>,
        pub raw: Str,
    }
    pub struct ParenExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub enum ExprOrSuper {
        Super(Super),
        Expr(Box<Expr>),
    }
    pub struct Super {
        pub node_id: NodeId,
        pub span: Span,
    }
    pub enum ExprOrSpread {
        Spread(SpreadElement),
        Expr(Box<Expr>),
    }
    pub enum BlockStmtOrExpr {
        BlockStmt(BlockStmt),
        Expr(Box<Expr>),
    }
    pub enum PatOrExpr {
        Expr(Box<Expr>),
        Pat(Box<Pat>),
    }
    pub struct OptChainExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub question_dot_token: Span,
        pub expr: Box<Expr>,
    }
    pub struct Function {
        pub node_id: NodeId,
        pub params: Vec<Param>,
        pub decorators: Vec<Decorator>,
        pub span: Span,
        pub body: Option<BlockStmt>,
        pub is_generator: bool,
        pub is_async: bool,
        pub type_params: Option<Vec<TsTypeParamDecl>>,
        pub return_type: Option<TsTypeAnn>,
    }
    pub struct Param {
        pub node_id: NodeId,
        pub span: Span,
        pub decorators: Vec<Decorator>,
        pub pat: Pat,
    }
    pub struct ParamWithoutDecorators {
        pub node_id: NodeId,
        pub pat: Pat,
    }
    pub enum ParamOrTsParamProp {
        TsParamProp(TsParamProp),
        Param(Param),
    }
    pub struct BindingIdent {
        pub node_id: NodeId,
        pub id: Ident,
        pub type_ann: Option<TsTypeAnn>,
    }
    pub struct Ident {
        pub node_id: NodeId,
        pub span: Span,
        pub sym: JsWord,
        pub optional: bool,
    }
    pub struct PrivateName {
        pub node_id: NodeId,
        pub span: Span,
        pub id: Ident,
    }
    pub enum JSXObject {
        JSXMemberExpr(Box<JSXMemberExpr>),
        Ident(Ident),
    }
    pub struct JSXMemberExpr {
        pub node_id: NodeId,
        pub obj: JSXObject,
        pub prop: Ident,
    }
    pub struct JSXNamespacedName {
        pub node_id: NodeId,
        pub ns: Ident,
        pub name: Ident,
    }
    pub struct JSXEmptyExpr {
        pub node_id: NodeId,
        pub span: Span,
    }
    pub struct JSXExprContainer {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: JSXExpr,
    }
    pub enum JSXExpr {
        JSXEmptyExpr(JSXEmptyExpr),
        Expr(Box<Expr>),
    }
    pub struct JSXSpreadChild {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub enum JSXElementName {
        Ident(Ident),
        JSXMemberExpr(JSXMemberExpr),
        JSXNamespacedName(JSXNamespacedName),
    }
    pub struct JSXOpeningElement {
        pub node_id: NodeId,
        pub name: JSXElementName,
        pub span: Span,
        pub attrs: Vec<JSXAttrOrSpread>,
        pub self_closing: bool,
        pub type_args: Option<TsTypeParamInstantiation>,
    }
    pub enum JSXAttrOrSpread {
        JSXAttr(JSXAttr),
        SpreadElement(SpreadElement),
    }
    pub struct JSXClosingElement {
        pub node_id: NodeId,
        pub span: Span,
        pub name: JSXElementName,
    }
    pub struct JSXAttr {
        pub node_id: NodeId,
        pub span: Span,
        pub name: JSXAttrName,
        pub value: Option<JSXAttrValue>,
    }
    pub enum JSXAttrName {
        Ident(Ident),
        JSXNamespacedName(JSXNamespacedName),
    }
    pub enum JSXAttrValue {
        Lit(Lit),
        JSXExprContainer(JSXExprContainer),
        JSXElement(Box<JSXElement>),
        JSXFragment(JSXFragment),
    }
    pub struct JSXText {
        pub node_id: NodeId,
        pub span: Span,
        pub value: JsWord,
        pub raw: JsWord,
    }
    pub struct JSXElement {
        pub node_id: NodeId,
        pub span: Span,
        pub opening: JSXOpeningElement,
        pub children: Vec<JSXElementChild>,
        pub closing: Option<JSXClosingElement>,
    }
    pub enum JSXElementChild {
        JSXText(JSXText),
        JSXExprContainer(JSXExprContainer),
        JSXSpreadChild(JSXSpreadChild),
        JSXElement(Box<JSXElement>),
        JSXFragment(JSXFragment),
    }
    pub struct JSXFragment {
        pub node_id: NodeId,
        pub span: Span,
        pub opening: JSXOpeningFragment,
        pub children: Vec<JSXElementChild>,
        pub closing: JSXClosingFragment,
    }
    pub struct JSXOpeningFragment {
        pub node_id: NodeId,
        pub span: Span,
    }
    pub struct JSXClosingFragment {
        pub node_id: NodeId,
        pub span: Span,
    }
    pub struct Invalid {
        pub node_id: NodeId,
        pub span: Span,
    }
    pub enum Lit {
        Str(Str),
        Bool(Bool),
        Null(Null),
        Num(Number),
        BigInt(BigInt),
        Regex(Regex),
        JSXText(JSXText),
    }
    pub struct BigInt {
        pub node_id: NodeId,
        pub span: Span,
        pub value: BigIntValue,
    }
    pub struct Str {
        pub node_id: NodeId,
        pub span: Span,
        pub value: JsWord,
        pub has_escape: bool,
        pub kind: StrKind,
    }
    pub struct Bool {
        pub node_id: NodeId,
        pub span: Span,
        pub value: bool,
    }
    pub struct Null {
        pub node_id: NodeId,
        pub span: Span,
    }
    pub struct Regex {
        pub node_id: NodeId,
        pub span: Span,
        pub exp: JsWord,
        pub flags: JsWord,
    }
    pub struct Number {
        pub node_id: NodeId,
        pub span: Span,
        pub value: f64,
        pub raw: Option<JsWord>,
    }
    pub enum StrKind {
        Normal { contains_quote: bool },
        Synthesized,
    }
    pub enum Program {
        Module(Module),
        Script(Script),
    }
    pub struct Module {
        pub node_id: NodeId,
        pub span: Span,
        pub body: Vec<ModuleItem>,
        pub shebang: Option<JsWord>,
    }
    pub struct Script {
        pub node_id: NodeId,
        pub span: Span,
        pub body: Vec<Stmt>,
        pub shebang: Option<JsWord>,
    }
    pub enum ModuleItem {
        ModuleDecl(ModuleDecl),
        Stmt(Stmt),
    }
    pub enum ModuleDecl {
        Import(ImportDecl),
        ExportDecl(ExportDecl),
        ExportNamed(NamedExport),
        ExportDefaultDecl(ExportDefaultDecl),
        ExportDefaultExpr(ExportDefaultExpr),
        ExportAll(ExportAll),
        TsImportEquals(TsImportEqualsDecl),
        TsExportAssignment(TsExportAssignment),
        TsNamespaceExport(TsNamespaceExportDecl),
    }
    pub struct ExportDefaultExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct ExportDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub decl: Decl,
    }
    pub struct ImportDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub specifiers: Vec<ImportSpecifier>,
        pub src: Str,
        pub type_only: bool,
        pub asserts: Option<ObjectLit>,
    }
    pub struct ExportAll {
        pub node_id: NodeId,
        pub span: Span,
        pub src: Str,
        pub asserts: Option<ObjectLit>,
    }
    pub struct NamedExport {
        pub node_id: NodeId,
        pub span: Span,
        pub specifiers: Vec<ExportSpecifier>,
        pub src: Option<Str>,
        pub type_only: bool,
        pub asserts: Option<ObjectLit>,
    }
    pub struct ExportDefaultDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub decl: DefaultDecl,
    }
    pub enum DefaultDecl {
        Class(ClassExpr),
        Fn(FnExpr),
        TsInterfaceDecl(TsInterfaceDecl),
    }
    pub enum ImportSpecifier {
        Named(ImportNamedSpecifier),
        Default(ImportDefaultSpecifier),
        Namespace(ImportStarAsSpecifier),
    }
    pub struct ImportDefaultSpecifier {
        pub node_id: NodeId,
        pub span: Span,
        pub local: Ident,
    }
    pub struct ImportStarAsSpecifier {
        pub node_id: NodeId,
        pub span: Span,
        pub local: Ident,
    }
    pub struct ImportNamedSpecifier {
        pub node_id: NodeId,
        pub span: Span,
        pub local: Ident,
        pub imported: Option<Ident>,
    }
    pub enum ExportSpecifier {
        Namespace(ExportNamespaceSpecifier),
        Default(ExportDefaultSpecifier),
        Named(ExportNamedSpecifier),
    }
    pub struct ExportNamespaceSpecifier {
        pub node_id: NodeId,
        pub span: Span,
        pub name: Ident,
    }
    pub struct ExportDefaultSpecifier {
        pub node_id: NodeId,
        pub exported: Ident,
    }
    pub struct ExportNamedSpecifier {
        pub node_id: NodeId,
        pub span: Span,
        pub orig: Ident,
        pub exported: Option<Ident>,
    }
    pub enum BinaryOp {
        EqEq,
        NotEq,
        EqEqEq,
        NotEqEq,
        Lt,
        LtEq,
        Gt,
        GtEq,
        LShift,
        RShift,
        ZeroFillRShift,
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        BitOr,
        BitXor,
        BitAnd,
        LogicalOr,
        LogicalAnd,
        In,
        InstanceOf,
        Exp,
        NullishCoalescing,
    }
    pub enum AssignOp {
        Assign,
        AddAssign,
        SubAssign,
        MulAssign,
        DivAssign,
        ModAssign,
        LShiftAssign,
        RShiftAssign,
        ZeroFillRShiftAssign,
        BitOrAssign,
        BitXorAssign,
        BitAndAssign,
        ExpAssign,
        AndAssign,
        OrAssign,
        NullishAssign,
    }

    pub enum UpdateOp {
        PlusPlus,
        MinusMinus,
    }
    pub enum UnaryOp {
        Minus,
        Plus,
        Bang,
        Tilde,
        TypeOf,
        Void,
        Delete,
    }
    pub enum Pat {
        Ident(BindingIdent),
        Array(ArrayPat),
        Rest(RestPat),
        Object(ObjectPat),
        Assign(AssignPat),
        Invalid(Invalid),
        Expr(Box<Expr>),
    }
    pub struct ArrayPat {
        pub node_id: NodeId,
        pub span: Span,
        pub elems: Vec<Option<Pat>>,
        pub optional: bool,
        pub type_ann: Option<TsTypeAnn>,
    }
    pub struct ObjectPat {
        pub node_id: NodeId,
        pub span: Span,
        pub props: Vec<ObjectPatProp>,
        pub optional: bool,
        pub type_ann: Option<TsTypeAnn>,
    }
    pub struct AssignPat {
        pub node_id: NodeId,
        pub span: Span,
        pub left: Box<Pat>,
        pub right: Box<Expr>,
        pub type_ann: Option<TsTypeAnn>,
    }
    pub struct RestPat {
        pub node_id: NodeId,
        pub span: Span,
        pub dot3_token: Span,
        pub arg: Box<Pat>,
        pub type_ann: Option<TsTypeAnn>,
    }
    pub enum ObjectPatProp {
        KeyValue(KeyValuePatProp),
        Assign(AssignPatProp),
        Rest(RestPat),
    }
    pub struct KeyValuePatProp {
        pub node_id: NodeId,
        pub key: PropName,
        pub value: Box<Pat>,
    }
    pub struct AssignPatProp {
        pub node_id: NodeId,
        pub span: Span,
        pub key: Ident,
        pub value: Option<Box<Expr>>,
    }
    pub enum Prop {
        Shorthand(Ident),
        KeyValue(KeyValueProp),
        Assign(AssignProp),
        Getter(GetterProp),
        Setter(SetterProp),
        Method(MethodProp),
        Spread(SpreadAssignment),
    }
    pub struct KeyValueProp {
        pub node_id: NodeId,
        pub key: PropName,
        pub value: Box<Expr>,
    }
    pub struct AssignProp {
        pub node_id: NodeId,
        pub key: Ident,
        pub value: Box<Expr>,
    }
    pub struct GetterProp {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PropName,
        pub type_ann: Option<TsTypeAnn>,
        pub body: Option<BlockStmt>,
    }
    pub struct SetterProp {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PropName,
        pub param: ParamWithoutDecorators,
        pub body: Option<BlockStmt>,
    }
    pub struct MethodProp {
        pub node_id: NodeId,
        pub key: PropName,
        pub function: Function,
    }
    pub enum PropName {
        Ident(Ident),
        Str(Str),
        Num(Number),
        Computed(ComputedPropName),
    }
    pub struct ComputedPropName {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct SpreadAssignment {
        pub node_id: NodeId,
        pub dot3_token: Span,
        pub expr: Box<Expr>,
    }
    pub struct BlockStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub stmts: Vec<Stmt>,
    }
    pub enum Stmt {
        Block(BlockStmt),
        Empty(EmptyStmt),
        Debugger(DebuggerStmt),
        With(WithStmt),
        Return(ReturnStmt),
        Labeled(LabeledStmt),
        Break(BreakStmt),
        Continue(ContinueStmt),
        If(IfStmt),
        Switch(SwitchStmt),
        Throw(ThrowStmt),
        Try(TryStmt),
        While(WhileStmt),
        DoWhile(DoWhileStmt),
        For(ForStmt),
        ForIn(ForInStmt),
        ForOf(ForOfStmt),
        Decl(Decl),
        Expr(ExprStmt),
    }
    pub struct ExprStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct EmptyStmt {
        pub node_id: NodeId,
        pub span: Span,
    }
    pub struct DebuggerStmt {
        pub node_id: NodeId,
        pub span: Span,
    }
    pub struct WithStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub obj: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct ReturnStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub arg: Option<Box<Expr>>,
    }
    pub struct LabeledStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub label: Ident,
        pub body: Box<Stmt>,
    }
    pub struct BreakStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub label: Option<Ident>,
    }
    pub struct ContinueStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub label: Option<Ident>,
    }
    pub struct IfStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub test: Box<Expr>,
        pub cons: Box<Stmt>,
        pub alt: Option<Box<Stmt>>,
    }
    pub struct SwitchStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub discriminant: Box<Expr>,
        pub cases: Vec<SwitchCase>,
    }
    pub struct ThrowStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub arg: Box<Expr>,
    }
    pub struct TryStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub block: BlockStmt,
        pub handler: Option<CatchClause>,
        pub finalizer: Option<BlockStmt>,
    }
    pub struct WhileStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub test: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct DoWhileStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub test: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct ForStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub init: Option<VarDeclOrExpr>,
        pub test: Option<Box<Expr>>,
        pub update: Option<Box<Expr>>,
        pub body: Box<Stmt>,
    }
    pub struct ForInStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub left: VarDeclOrPat,
        pub right: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct ForOfStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub await_token: Option<Span>,
        pub left: VarDeclOrPat,
        pub right: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct SwitchCase {
        pub node_id: NodeId,
        pub span: Span,
        pub test: Option<Box<Expr>>,
        pub cons: Vec<Stmt>,
    }
    pub struct CatchClause {
        pub node_id: NodeId,
        pub span: Span,
        pub param: Option<Pat>,
        pub body: BlockStmt,
    }
    pub enum VarDeclOrPat {
        VarDecl(VarDecl),
        Pat(Pat),
    }
    pub enum VarDeclOrExpr {
        VarDecl(VarDecl),
        Expr(Box<Expr>),
    }
    pub struct TsTypeAnn {
        pub node_id: NodeId,
        pub span: Span,
        pub type_ann: Box<TsType>,
    }
    pub struct TsTypeParamDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub name: Ident,
        pub constraint: Option<Box<TsType>>,
        pub default: Option<Box<TsType>>,
    }
    pub struct TsTypeParamInstantiation {
        pub node_id: NodeId,
        pub span: Span,
        pub params: Vec<Box<TsType>>,
    }
    pub struct TsParamProp {
        pub node_id: NodeId,
        pub span: Span,
        pub decorators: Vec<Decorator>,
        pub accessibility: Option<Accessibility>,
        pub is_override: bool,
        pub readonly: bool,
        pub param: TsParamPropParam,
    }
    pub enum TsParamPropParam {
        Ident(BindingIdent),
        Assign(AssignPat),
    }
    pub struct TsQualifiedName {
        pub node_id: NodeId,
        pub left: TsEntityName,
        pub right: Ident,
    }
    pub enum TsEntityName {
        TsQualifiedName(Box<TsQualifiedName>),
        Ident(Ident),
    }
    pub enum TsTypeElement {
        TsCallSignatureDecl(TsCallSignatureDecl),
        TsConstructSignatureDecl(TsConstructSignatureDecl),
        TsPropertySignature(TsPropertySignature),
        TsGetterSignature(TsGetterSignature),
        TsSetterSignature(TsSetterSignature),
        TsMethodSignature(TsMethodSignature),
        TsIndexSignature(TsIndexSignature),
    }
    pub struct TsCallSignatureDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub params: Vec<TsAmbientParam>,
        pub type_ann: Option<TsTypeAnn>,
        pub type_params: Option<Vec<TsTypeParamDecl>>,
    }
    pub struct TsConstructSignatureDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub params: Vec<TsAmbientParam>,
        pub type_ann: Option<TsTypeAnn>,
        pub type_params: Option<Vec<TsTypeParamDecl>>,
    }
    pub struct TsPropertySignature {
        pub node_id: NodeId,
        pub span: Span,
        pub readonly: bool,
        pub key: PropName,
        pub optional: bool,
        pub type_ann: Option<TsTypeAnn>,
    }

    pub struct TsGetterSignature {
        pub node_id: NodeId,
        pub span: Span,
        pub readonly: bool,
        pub key: PropName,
        pub optional: bool,
        pub type_ann: Option<TsTypeAnn>,
    }

    pub struct TsSetterSignature {
        pub node_id: NodeId,
        pub span: Span,
        pub readonly: bool,
        pub key: PropName,
        pub optional: bool,
        pub param: TsAmbientParam,
    }
    pub struct TsMethodSignature {
        pub node_id: NodeId,
        pub span: Span,
        pub readonly: bool,
        pub key: PropName,
        pub optional: bool,
        pub params: Vec<TsAmbientParam>,
        pub type_ann: Option<TsTypeAnn>,
        pub type_params: Option<Vec<TsTypeParamDecl>>,
    }
    pub struct TsIndexSignature {
        pub node_id: NodeId,
        pub params: Vec<TsAmbientParam>,
        pub type_ann: Option<TsTypeAnn>,
        pub readonly: bool,
        pub is_static: bool,
        pub span: Span,
    }
    pub enum TsType {
        TsKeywordType(TsKeywordType),
        TsThisType(TsThisType),
        TsFnOrConstructorType(TsFnOrConstructorType),
        TsTypeRef(TsTypeRef),
        TsTypeQuery(TsTypeQuery),
        TsTypeLit(TsTypeLit),
        TsArrayType(TsArrayType),
        TsTupleType(TsTupleType),
        TsOptionalType(TsOptionalType),
        TsRestType(TsRestType),
        TsUnionOrIntersectionType(TsUnionOrIntersectionType),
        TsConditionalType(TsConditionalType),
        TsInferType(TsInferType),
        TsParenthesizedType(TsParenthesizedType),
        TsTypeOperator(TsTypeOperator),
        TsIndexedAccessType(TsIndexedAccessType),
        TsMappedType(TsMappedType),
        TsLitType(TsLitType),
        TsTypePredicate(TsTypePredicate),
        TsImportType(TsImportType),
    }
    pub enum TsFnOrConstructorType {
        TsFnType(TsFnType),
        TsConstructorType(TsConstructorType),
    }
    pub struct TsKeywordType {
        pub node_id: NodeId,
        pub span: Span,
        pub kind: TsKeywordTypeKind,
    }
    pub enum TsKeywordTypeKind {
        TsAnyKeyword,
        TsUnknownKeyword,
        TsNumberKeyword,
        TsObjectKeyword,
        TsBooleanKeyword,
        TsBigIntKeyword,
        TsStringKeyword,
        TsSymbolKeyword,
        TsVoidKeyword,
        TsUndefinedKeyword,
        TsNullKeyword,
        TsNeverKeyword,
        TsIntrinsicKeyword,
    }
    pub struct TsThisType {
        pub node_id: NodeId,
        pub span: Span,
    }
    pub struct TsAmbientParam {
        pub node_id: NodeId,
        pub pat: TsAmbientParamPat,
    }
    pub enum TsAmbientParamPat {
        Ident(BindingIdent),
        Array(ArrayPat),
        Rest(RestPat),
        Object(ObjectPat),
    }
    pub struct TsFnType {
        pub node_id: NodeId,
        pub span: Span,
        pub params: Vec<TsAmbientParam>,
        pub type_params: Option<Vec<TsTypeParamDecl>>,
        pub type_ann: TsTypeAnn,
    }
    pub struct TsConstructorType {
        pub node_id: NodeId,
        pub span: Span,
        pub params: Vec<TsAmbientParam>,
        pub type_params: Option<Vec<TsTypeParamDecl>>,
        pub type_ann: TsTypeAnn,
        pub is_abstract: bool,
    }
    pub struct TsTypeRef {
        pub node_id: NodeId,
        pub span: Span,
        pub type_name: TsEntityName,
        pub type_params: Option<TsTypeParamInstantiation>,
    }
    pub struct TsTypePredicate {
        pub node_id: NodeId,
        pub span: Span,
        pub asserts: bool,
        pub param_name: TsThisTypeOrIdent,
        pub type_ann: Option<TsTypeAnn>,
    }
    pub enum TsThisTypeOrIdent {
        TsThisType(TsThisType),
        Ident(Ident),
    }
    pub struct TsTypeQuery {
        pub node_id: NodeId,
        pub span: Span,
        pub expr_name: TsTypeQueryExpr,
    }
    pub enum TsTypeQueryExpr {
        TsEntityName(TsEntityName),
        Import(TsImportType),
    }
    pub struct TsImportType {
        pub node_id: NodeId,
        pub span: Span,
        pub arg: Str,
        pub qualifier: Option<TsEntityName>,
        pub type_args: Option<TsTypeParamInstantiation>,
    }
    pub struct TsTypeLit {
        pub node_id: NodeId,
        pub span: Span,
        pub members: Vec<TsTypeElement>,
    }
    pub struct TsArrayType {
        pub node_id: NodeId,
        pub span: Span,
        pub elem_type: Box<TsType>,
    }

    pub struct TsTupleType {
        pub node_id: NodeId,
        pub span: Span,
        pub elem_types: Vec<TsTupleElement>,
    }

    pub struct TsTupleElement {
        pub node_id: NodeId,
        pub span: Span,
        pub label: Option<Pat>,
        pub ty: TsType,
    }

    pub struct TsOptionalType {
        pub node_id: NodeId,
        pub span: Span,
        pub type_ann: Box<TsType>,
    }
    pub struct TsRestType {
        pub node_id: NodeId,
        pub span: Span,
        pub type_ann: Box<TsType>,
    }
    pub enum TsUnionOrIntersectionType {
        TsUnionType(TsUnionType),
        TsIntersectionType(TsIntersectionType),
    }
    pub struct TsUnionType {
        pub node_id: NodeId,
        pub span: Span,
        pub types: Vec<Box<TsType>>,
    }
    pub struct TsIntersectionType {
        pub node_id: NodeId,
        pub span: Span,
        pub types: Vec<Box<TsType>>,
    }
    pub struct TsConditionalType {
        pub node_id: NodeId,
        pub span: Span,
        pub check_type: Box<TsType>,
        pub extends_type: Box<TsType>,
        pub true_type: Box<TsType>,
        pub false_type: Box<TsType>,
    }
    pub struct TsInferType {
        pub node_id: NodeId,
        pub span: Span,
        pub type_param: TsTypeParamDecl,
    }
    pub struct TsParenthesizedType {
        pub node_id: NodeId,
        pub span: Span,
        pub type_ann: Box<TsType>,
    }
    pub struct TsTypeOperator {
        pub node_id: NodeId,
        pub span: Span,
        pub op: TsTypeOperatorOp,
        pub type_ann: Box<TsType>,
    }
    pub enum TsTypeOperatorOp {
        KeyOf,
        Unique,
        ReadOnly,
    }
    pub struct TsIndexedAccessType {
        pub node_id: NodeId,
        pub span: Span,
        pub readonly: bool,
        pub obj_type: Box<TsType>,
        pub index_type: Box<TsType>,
    }
    pub enum TruePlusMinus {
        True,
        Plus,
        Minus,
    }
    pub struct TsMappedType {
        pub node_id: NodeId,
        pub span: Span,
        pub readonly: Option<TruePlusMinus>,
        pub type_param: TsTypeParamDecl,
        pub name_type: Option<Box<TsType>>,
        pub optional: Option<TruePlusMinus>,
        pub type_ann: Option<Box<TsType>>,
    }
    pub struct TsLitType {
        pub node_id: NodeId,
        pub span: Span,
        pub lit: TsLit,
    }
    pub enum TsLit {
        BigInt(BigInt),
        Number(Number),
        Str(Str),
        Bool(Bool),
        Tpl(TsTplLitType),
    }
    pub struct TsTplLitType {
        pub node_id: NodeId,
        pub span: Span,
        pub types: Vec<Box<TsType>>,
        pub quasis: Vec<TplElement>,
    }
    pub struct TsInterfaceDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub id: Ident,
        pub declare: bool,
        pub type_params: Option<Vec<TsTypeParamDecl>>,
        pub extends: Vec<TsExprWithTypeArgs>,
        pub body: TsInterfaceBody,
    }
    pub struct TsInterfaceBody {
        pub node_id: NodeId,
        pub span: Span,
        pub body: Vec<TsTypeElement>,
    }
    pub struct TsExprWithTypeArgs {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: TsEntityName,
        pub type_args: Option<TsTypeParamInstantiation>,
    }
    pub struct TsTypeAliasDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub declare: bool,
        pub id: Ident,
        pub type_params: Option<Vec<TsTypeParamDecl>>,
        pub type_ann: Box<TsType>,
    }
    pub struct TsEnumDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub declare: bool,
        pub is_const: bool,
        pub id: Ident,
        pub members: Vec<TsEnumMember>,
    }
    pub struct TsEnumMember {
        pub node_id: NodeId,
        pub span: Span,
        pub id: TsEnumMemberId,
        pub init: Option<Box<Expr>>,
    }
    pub enum TsEnumMemberId {
        Ident(Ident),
        Str(Str),
    }
    pub struct TsModuleDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub declare: bool,
        pub global: bool,
        pub id: TsModuleName,
        pub body: Option<TsNamespaceBody>,
    }
    pub enum TsNamespaceBody {
        TsModuleBlock(TsModuleBlock),
        TsNamespaceDecl(TsNamespaceDecl),
    }
    pub struct TsModuleBlock {
        pub node_id: NodeId,
        pub span: Span,
        pub body: Vec<ModuleItem>,
    }
    pub struct TsNamespaceDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub declare: bool,
        pub global: bool,
        pub id: Ident,
        pub body: Box<TsNamespaceBody>,
    }
    pub enum TsModuleName {
        Ident(Ident),
        Str(Str),
    }
    pub struct TsImportEqualsDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub declare: bool,
        pub is_export: bool,
        pub is_type_only: bool,
        pub id: Ident,
        pub module_ref: TsModuleRef,
    }
    pub enum TsModuleRef {
        TsEntityName(TsEntityName),
        TsExternalModuleRef(TsExternalModuleRef),
    }
    pub struct TsExternalModuleRef {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Str,
    }
    pub struct TsExportAssignment {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct TsNamespaceExportDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub id: Ident,
    }
    pub struct TsAsExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Box<Expr>,
        pub type_ann: Box<TsType>,
    }
    pub struct TsTypeAssertion {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Box<Expr>,
        pub type_ann: Box<TsType>,
    }
    pub struct TsNonNullExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub enum Accessibility {
        Public,
        Protected,
        Private,
    }
    pub struct TsConstAssertion {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Box<Expr>,
    }
});
