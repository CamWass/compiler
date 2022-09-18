use crate::ast::*;
use crate::node::{Bind, BoundNode};
use std::rc::Rc;
use visit2::define;

define!({
    // TODO: ensure field order matches evaluation order in ECMA/TSC:
    pub struct Class {
        pub node_id: NodeId,
        pub span: Span,
        pub decorators: Vec<Rc<Decorator>>,
        pub is_abstract: bool,
        pub type_params: Option<Vec<Rc<TsTypeParamDecl>>>,
        pub extends: Option<Rc<ExtendsClause>>,
        pub implements: Vec<Rc<TsExprWithTypeArgs>>,
        pub body: Vec<ClassMember>,
        pub cached_hash: u8,
    }
    pub struct ExtendsClause {
        pub node_id: NodeId,
        pub span: Span,
        pub super_class: Expr,
        pub super_type_params: Option<Rc<TsTypeParamInstantiation>>,
        pub cached_hash: u8,
    }
    pub enum ClassMember {
        Constructor(Rc<Constructor>),
        Method(Rc<ClassMethod>),
        PrivateMethod(Rc<PrivateMethod>),
        ClassProp(Rc<ClassProp>),
        PrivateProp(Rc<PrivateProp>),
        TsIndexSignature(Rc<TsIndexSignature>),
        Empty(Rc<EmptyStmt>),
    }
    pub struct ClassProp {
        pub node_id: NodeId,
        pub span: Span,
        pub decorators: Vec<Rc<Decorator>>,
        pub is_static: bool,
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
        pub readonly: bool,
        pub declare: bool,
        pub definite: bool,
        pub key: PropName,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub value: Option<Expr>,
        pub cached_hash: u8,
    }
    pub struct PrivateProp {
        pub node_id: NodeId,
        pub span: Span,
        pub decorators: Vec<Rc<Decorator>>,
        pub is_static: bool,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
        pub accessibility: Option<Accessibility>,
        pub readonly: bool,
        pub definite: bool,
        pub key: Rc<PrivateName>,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub value: Option<Expr>,
        pub cached_hash: u8,
    }
    // TODO: ensure field order matches evaluation order in ECMA/TSC:
    pub struct ClassMethod {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PropName,
        pub function: Rc<Function>,
        pub kind: MethodKind,
        pub is_static: bool,
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
        pub cached_hash: u8,
    }
    // TODO: ensure field order matches evaluation order in ECMA/TSC:
    pub struct PrivateMethod {
        pub node_id: NodeId,
        pub span: Span,
        pub key: Rc<PrivateName>,
        pub function: Rc<Function>,
        pub kind: MethodKind,
        pub is_static: bool,
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
        pub cached_hash: u8,
    }
    pub struct Constructor {
        pub node_id: NodeId,
        pub span: Span,
        pub params: Vec<ParamOrTsParamProp>,
        pub body: Option<Rc<BlockStmt>>,
        pub accessibility: Option<Accessibility>,
        pub is_optional: bool,
        pub cached_hash: u8,
    }
    pub struct Decorator {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Expr,
        pub cached_hash: u8,
    }
    pub enum MethodKind {
        Method,
        Getter,
        Setter,
    }
    pub enum Decl {
        Class(Rc<ClassDecl>),
        Fn(Rc<FnDecl>),
        Var(Rc<VarDecl>),
        TsInterface(Rc<TsInterfaceDecl>),
        TsTypeAlias(Rc<TsTypeAliasDecl>),
        TsEnum(Rc<TsEnumDecl>),
        TsModule(Rc<TsModuleDecl>),
    }
    // TODO: ensure field order matches evaluation order in ECMA/TSC:
    pub struct FnDecl {
        pub node_id: NodeId,
        pub ident: Rc<Ident>,
        pub declare: bool,
        pub function: Rc<Function>,
        pub cached_hash: u8,
    }
    // TODO: ensure field order matches evaluation order in ECMA/TSC:
    pub struct ClassDecl {
        pub node_id: NodeId,
        pub ident: Rc<Ident>,
        pub declare: bool,
        pub class: Rc<Class>,
        pub cached_hash: u8,
    }
    pub struct VarDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub kind: VarDeclKind,
        pub declare: bool,
        pub decls: Vec<Rc<VarDeclarator>>,
        pub cached_hash: u8,
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
        pub init: Option<Expr>,
        pub definite: bool,
        pub cached_hash: u8,
    }
    pub enum Expr {
        This(Rc<ThisExpr>),
        Array(Rc<ArrayLit>),
        Object(Rc<ObjectLit>),
        Fn(Rc<FnExpr>),
        Unary(Rc<UnaryExpr>),
        Update(Rc<UpdateExpr>),
        Bin(Rc<BinExpr>),
        Assign(Rc<AssignExpr>),
        Member(Rc<MemberExpr>),
        Cond(Rc<CondExpr>),
        Call(Rc<CallExpr>),
        New(Rc<NewExpr>),
        Seq(Rc<SeqExpr>),
        Ident(Rc<Ident>),
        Lit(Lit),
        Tpl(Rc<Tpl>),
        TaggedTpl(Rc<TaggedTpl>),
        Arrow(Rc<ArrowExpr>),
        Class(Rc<ClassExpr>),
        Yield(Rc<YieldExpr>),
        MetaProp(Rc<MetaPropExpr>),
        Await(Rc<AwaitExpr>),
        Paren(Rc<ParenExpr>),
        JSXMember(Rc<JSXMemberExpr>),
        JSXNamespacedName(Rc<JSXNamespacedName>),
        JSXEmpty(Rc<JSXEmptyExpr>),
        JSXElement(Rc<JSXElement>),
        JSXFragment(Rc<JSXFragment>),
        TsTypeAssertion(Rc<TsTypeAssertion>),
        TsConstAssertion(Rc<TsConstAssertion>),
        TsNonNull(Rc<TsNonNullExpr>),
        TsAs(Rc<TsAsExpr>),
        PrivateName(Rc<PrivateName>),
        OptChain(Rc<OptChainExpr>),
        Invalid(Rc<Invalid>),
    }
    pub struct ThisExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub cached_hash: u8,
    }
    pub struct ArrayLit {
        pub node_id: NodeId,
        pub span: Span,
        pub elems: Vec<Option<Rc<ExprOrSpread>>>,
        pub cached_hash: u8,
    }
    pub struct ObjectLit {
        pub node_id: NodeId,
        pub span: Span,
        pub props: Vec<Prop>,
        pub cached_hash: u8,
    }
    pub struct SpreadElement {
        pub node_id: NodeId,
        pub dot3_token: Span,
        pub expr: Expr,
        pub cached_hash: u8,
    }
    pub struct UnaryExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub op: UnaryOp,
        pub arg: Expr,
        pub cached_hash: u8,
    }
    pub struct UpdateExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub op: UpdateOp,
        pub prefix: bool,
        pub arg: Expr,
        pub cached_hash: u8,
    }
    pub struct BinExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub left: Expr,
        pub op: BinaryOp,
        pub right: Expr,
        pub cached_hash: u8,
    }
    // TODO: ensure field order matches evaluation order in ECMA/TSC:
    pub struct FnExpr {
        pub node_id: NodeId,
        pub ident: Option<Rc<Ident>>,
        pub function: Rc<Function>,
        pub cached_hash: u8,
    }
    // TODO: ensure field order matches evaluation order in ECMA/TSC:
    pub struct ClassExpr {
        pub node_id: NodeId,
        pub ident: Option<Rc<Ident>>,
        pub class: Rc<Class>,
        pub cached_hash: u8,
    }
    pub struct AssignExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub op: AssignOp,
        pub left: PatOrExpr,
        pub right: Expr,
        pub cached_hash: u8,
    }
    pub struct MemberExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub obj: ExprOrSuper,
        pub prop: Expr,
        pub computed: bool,
        pub cached_hash: u8,
    }
    pub struct CondExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub test: Expr,
        pub cons: Expr,
        pub alt: Expr,
        pub cached_hash: u8,
    }
    pub struct CallExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub callee: ExprOrSuper,
        pub type_args: Option<Rc<TsTypeParamInstantiation>>,
        pub args: Vec<Rc<ExprOrSpread>>,
        pub cached_hash: u8,
    }
    pub struct NewExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub callee: Expr,
        pub type_args: Option<Rc<TsTypeParamInstantiation>>,
        pub args: Option<Vec<Rc<ExprOrSpread>>>,
        pub cached_hash: u8,
    }
    pub struct SeqExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub exprs: Vec<Expr>,
        pub cached_hash: u8,
    }
    pub struct ArrowExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub is_async: bool,
        pub type_params: Option<Vec<Rc<TsTypeParamDecl>>>,
        pub params: Vec<Pat>,
        pub return_type: Option<Rc<TsTypeAnn>>,
        pub body: BlockStmtOrExpr,
        pub cached_hash: u8,
    }
    pub struct YieldExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub arg: Option<Expr>,
        pub delegate: bool,
        pub cached_hash: u8,
    }
    pub struct MetaPropExpr {
        pub node_id: NodeId,
        pub meta: Rc<Ident>,
        pub prop: Rc<Ident>,
        pub cached_hash: u8,
    }
    pub struct AwaitExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub arg: Expr,
        pub cached_hash: u8,
    }
    // TODO: ensure field order matches evaluation order in ECMA/TSC:
    pub struct Tpl {
        pub node_id: NodeId,
        pub span: Span,
        pub exprs: Vec<Expr>,
        pub quasis: Vec<Rc<TplElement>>,
        pub cached_hash: u8,
    }
    pub struct TaggedTpl {
        pub node_id: NodeId,
        pub span: Span,
        pub tag: Expr,
        pub type_params: Option<Rc<TsTypeParamInstantiation>>,
        pub tpl: Rc<Tpl>,
        pub cached_hash: u8,
    }
    pub struct TplElement {
        pub node_id: NodeId,
        pub span: Span,
        pub tail: bool,
        pub cooked: Option<Rc<Str>>,
        pub raw: Rc<Str>,
        pub cached_hash: u8,
    }
    pub struct ParenExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Expr,
        pub cached_hash: u8,
    }
    pub enum ExprOrSuper {
        Super(Rc<Super>),
        Expr(Expr),
    }
    pub struct Super {
        pub node_id: NodeId,
        pub span: Span,
        pub cached_hash: u8,
    }
    pub enum ExprOrSpread {
        Spread(Rc<SpreadElement>),
        Expr(Expr),
    }
    pub enum BlockStmtOrExpr {
        BlockStmt(Rc<BlockStmt>),
        Expr(Expr),
    }
    pub enum PatOrExpr {
        Expr(Expr),
        Pat(Pat),
    }
    pub struct OptChainExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub question_dot_token: Span,
        pub expr: Expr,
        pub cached_hash: u8,
    }
    // TODO: ensure field order matches evaluation order in ECMA/TSC:
    pub struct Function {
        pub node_id: NodeId,
        pub params: Vec<Rc<Param>>,
        pub decorators: Vec<Rc<Decorator>>,
        pub span: Span,
        pub body: Option<Rc<BlockStmt>>,
        pub is_generator: bool,
        pub is_async: bool,
        pub type_params: Option<Vec<Rc<TsTypeParamDecl>>>,
        pub return_type: Option<Rc<TsTypeAnn>>,
        pub cached_hash: u8,
    }
    pub struct Param {
        pub node_id: NodeId,
        pub span: Span,
        pub decorators: Vec<Rc<Decorator>>,
        pub pat: Pat,
        pub cached_hash: u8,
    }
    pub struct ParamWithoutDecorators {
        pub node_id: NodeId,
        pub pat: Pat,
        pub cached_hash: u8,
    }
    pub enum ParamOrTsParamProp {
        TsParamProp(Rc<TsParamProp>),
        Param(Rc<Param>),
    }
    pub struct BindingIdent {
        pub node_id: NodeId,
        pub id: Rc<Ident>,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub cached_hash: u8,
    }
    pub struct Ident {
        pub node_id: NodeId,
        pub span: Span,
        pub sym: JsWord,
        pub optional: bool,
        pub cached_hash: u8,
    }
    pub struct PrivateName {
        pub node_id: NodeId,
        pub span: Span,
        pub id: Rc<Ident>,
        pub cached_hash: u8,
    }
    pub enum JSXObject {
        JSXMemberExpr(Rc<JSXMemberExpr>),
        Ident(Rc<Ident>),
    }
    pub struct JSXMemberExpr {
        pub node_id: NodeId,
        pub obj: JSXObject,
        pub prop: Rc<Ident>,
        pub cached_hash: u8,
    }
    pub struct JSXNamespacedName {
        pub node_id: NodeId,
        pub ns: Rc<Ident>,
        pub name: Rc<Ident>,
        pub cached_hash: u8,
    }
    pub struct JSXEmptyExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub cached_hash: u8,
    }
    pub struct JSXExprContainer {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: JSXExpr,
        pub cached_hash: u8,
    }
    pub enum JSXExpr {
        JSXEmptyExpr(Rc<JSXEmptyExpr>),
        Expr(Expr),
    }
    pub struct JSXSpreadChild {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Expr,
        pub cached_hash: u8,
    }
    pub enum JSXElementName {
        Ident(Rc<Ident>),
        JSXMemberExpr(Rc<JSXMemberExpr>),
        JSXNamespacedName(Rc<JSXNamespacedName>),
    }
    pub struct JSXOpeningElement {
        pub node_id: NodeId,
        pub span: Span,
        pub name: JSXElementName,
        pub type_args: Option<Rc<TsTypeParamInstantiation>>,
        pub attrs: Vec<JSXAttrOrSpread>,
        pub self_closing: bool,
        pub cached_hash: u8,
    }
    pub enum JSXAttrOrSpread {
        JSXAttr(Rc<JSXAttr>),
        SpreadElement(Rc<SpreadElement>),
    }
    pub struct JSXClosingElement {
        pub node_id: NodeId,
        pub span: Span,
        pub name: JSXElementName,
        pub cached_hash: u8,
    }
    pub struct JSXAttr {
        pub node_id: NodeId,
        pub span: Span,
        pub name: JSXAttrName,
        pub value: Option<JSXAttrValue>,
        pub cached_hash: u8,
    }
    pub enum JSXAttrName {
        Ident(Rc<Ident>),
        JSXNamespacedName(Rc<JSXNamespacedName>),
    }
    pub enum JSXAttrValue {
        Lit(Lit),
        JSXExprContainer(Rc<JSXExprContainer>),
        JSXElement(Rc<JSXElement>),
        JSXFragment(Rc<JSXFragment>),
    }
    pub struct JSXText {
        pub node_id: NodeId,
        pub span: Span,
        pub value: JsWord,
        pub raw: JsWord,
        pub cached_hash: u8,
    }
    pub struct JSXElement {
        pub node_id: NodeId,
        pub span: Span,
        pub opening: Rc<JSXOpeningElement>,
        pub children: Vec<JSXElementChild>,
        pub closing: Option<Rc<JSXClosingElement>>,
        pub cached_hash: u8,
    }
    pub enum JSXElementChild {
        JSXText(Rc<JSXText>),
        JSXExprContainer(Rc<JSXExprContainer>),
        JSXSpreadChild(Rc<JSXSpreadChild>),
        JSXElement(Rc<JSXElement>),
        JSXFragment(Rc<JSXFragment>),
    }
    pub struct JSXFragment {
        pub node_id: NodeId,
        pub span: Span,
        pub opening: Rc<JSXOpeningFragment>,
        pub children: Vec<JSXElementChild>,
        pub closing: Rc<JSXClosingFragment>,
        pub cached_hash: u8,
    }
    pub struct JSXOpeningFragment {
        pub node_id: NodeId,
        pub span: Span,
        pub cached_hash: u8,
    }
    pub struct JSXClosingFragment {
        pub node_id: NodeId,
        pub span: Span,
        pub cached_hash: u8,
    }
    pub struct Invalid {
        pub node_id: NodeId,
        pub span: Span,
        pub cached_hash: u8,
    }
    pub enum Lit {
        Str(Rc<Str>),
        Bool(Rc<Bool>),
        Null(Rc<Null>),
        Num(Rc<Number>),
        BigInt(Rc<BigInt>),
        Regex(Rc<Regex>),
        JSXText(Rc<JSXText>),
    }
    pub struct BigInt {
        pub node_id: NodeId,
        pub span: Span,
        pub value: BigIntValue,
        pub cached_hash: u8,
    }
    pub struct Str {
        pub node_id: NodeId,
        pub span: Span,
        pub value: JsWord,
        pub has_escape: bool,
        pub kind: StrKind,
        pub cached_hash: u8,
    }
    pub struct Bool {
        pub node_id: NodeId,
        pub span: Span,
        pub value: bool,
        pub cached_hash: u8,
    }
    pub struct Null {
        pub node_id: NodeId,
        pub span: Span,
        pub cached_hash: u8,
    }
    pub struct Regex {
        pub node_id: NodeId,
        pub span: Span,
        pub exp: JsWord,
        pub flags: JsWord,
        pub cached_hash: u8,
    }
    pub struct Number {
        pub node_id: NodeId,
        pub span: Span,
        pub value: f64,
        pub raw: Option<JsWord>,
        pub cached_hash: u8,
    }
    pub enum Program {
        Module(Rc<Module>),
        Script(Rc<Script>),
    }
    pub struct Module {
        pub node_id: NodeId,
        pub span: Span,
        pub body: Vec<ModuleItem>,
        pub shebang: Option<JsWord>,
        pub cached_hash: u8,
    }
    pub struct Script {
        pub node_id: NodeId,
        pub span: Span,
        pub body: Vec<Stmt>,
        pub shebang: Option<JsWord>,
        pub cached_hash: u8,
    }
    pub enum ModuleItem {
        ModuleDecl(ModuleDecl),
        Stmt(Stmt),
    }
    pub enum ModuleDecl {
        Import(Rc<ImportDecl>),
        ExportDecl(Rc<ExportDecl>),
        ExportNamed(Rc<NamedExport>),
        ExportDefaultDecl(Rc<ExportDefaultDecl>),
        ExportDefaultExpr(Rc<ExportDefaultExpr>),
        ExportAll(Rc<ExportAll>),
        TsImportEquals(Rc<TsImportEqualsDecl>),
        TsExportAssignment(Rc<TsExportAssignment>),
        TsNamespaceExport(Rc<TsNamespaceExportDecl>),
    }
    pub struct ExportDefaultExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Expr,
        pub cached_hash: u8,
    }
    pub struct ExportDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub decl: Decl,
        pub cached_hash: u8,
    }
    pub struct ImportDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub specifiers: Vec<ImportSpecifier>,
        pub src: Rc<Str>,
        pub type_only: bool,
        pub asserts: Option<Rc<ObjectLit>>,
        pub cached_hash: u8,
    }
    pub struct ExportAll {
        pub node_id: NodeId,
        pub span: Span,
        pub src: Rc<Str>,
        pub asserts: Option<Rc<ObjectLit>>,
        pub cached_hash: u8,
    }
    pub struct NamedExport {
        pub node_id: NodeId,
        pub span: Span,
        pub specifiers: Vec<ExportSpecifier>,
        pub src: Option<Rc<Str>>,
        pub type_only: bool,
        pub asserts: Option<Rc<ObjectLit>>,
        pub cached_hash: u8,
    }
    pub struct ExportDefaultDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub decl: DefaultDecl,
        pub cached_hash: u8,
    }
    pub enum DefaultDecl {
        Class(Rc<ClassExpr>),
        Fn(Rc<FnExpr>),
        TsInterfaceDecl(Rc<TsInterfaceDecl>),
    }
    pub enum ImportSpecifier {
        Named(Rc<ImportNamedSpecifier>),
        Default(Rc<ImportDefaultSpecifier>),
        Namespace(Rc<ImportStarAsSpecifier>),
    }
    pub struct ImportDefaultSpecifier {
        pub node_id: NodeId,
        pub span: Span,
        pub local: Rc<Ident>,
        pub cached_hash: u8,
    }
    pub struct ImportStarAsSpecifier {
        pub node_id: NodeId,
        pub span: Span,
        pub local: Rc<Ident>,
        pub cached_hash: u8,
    }
    pub struct ImportNamedSpecifier {
        pub node_id: NodeId,
        pub span: Span,
        pub local: Rc<Ident>,
        pub imported: Option<Rc<Ident>>,
        pub cached_hash: u8,
    }
    pub enum ExportSpecifier {
        Namespace(Rc<ExportNamespaceSpecifier>),
        Default(Rc<ExportDefaultSpecifier>),
        Named(Rc<ExportNamedSpecifier>),
    }
    pub struct ExportNamespaceSpecifier {
        pub node_id: NodeId,
        pub span: Span,
        pub name: Rc<Ident>,
        pub cached_hash: u8,
    }
    pub struct ExportDefaultSpecifier {
        pub node_id: NodeId,
        pub exported: Rc<Ident>,
        pub cached_hash: u8,
    }
    pub struct ExportNamedSpecifier {
        pub node_id: NodeId,
        pub span: Span,
        pub orig: Rc<Ident>,
        pub exported: Option<Rc<Ident>>,
        pub cached_hash: u8,
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
        Ident(Rc<BindingIdent>),
        Array(Rc<ArrayPat>),
        Rest(Rc<RestPat>),
        Object(Rc<ObjectPat>),
        Assign(Rc<AssignPat>),
        Invalid(Rc<Invalid>),
        Expr(Expr),
    }
    pub struct ArrayPat {
        pub node_id: NodeId,
        pub span: Span,
        pub elems: Vec<Option<Pat>>,
        pub optional: bool,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub cached_hash: u8,
    }
    pub struct ObjectPat {
        pub node_id: NodeId,
        pub span: Span,
        pub props: Vec<ObjectPatProp>,
        pub optional: bool,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub cached_hash: u8,
    }
    pub struct AssignPat {
        pub node_id: NodeId,
        pub span: Span,
        pub left: Pat,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub right: Expr,
        pub cached_hash: u8,
    }
    pub struct RestPat {
        pub node_id: NodeId,
        pub span: Span,
        pub dot3_token: Span,
        pub arg: Pat,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub cached_hash: u8,
    }
    pub enum ObjectPatProp {
        KeyValue(Rc<KeyValuePatProp>),
        Assign(Rc<AssignPatProp>),
        Rest(Rc<RestPat>),
    }
    pub struct KeyValuePatProp {
        pub node_id: NodeId,
        pub key: PropName,
        pub value: Pat,
        pub cached_hash: u8,
    }
    pub struct AssignPatProp {
        pub node_id: NodeId,
        pub span: Span,
        pub key: Rc<Ident>,
        pub value: Option<Expr>,
        pub cached_hash: u8,
    }
    pub enum Prop {
        Shorthand(Rc<Ident>),
        KeyValue(Rc<KeyValueProp>),
        Assign(Rc<AssignProp>),
        Getter(Rc<GetterProp>),
        Setter(Rc<SetterProp>),
        Method(Rc<MethodProp>),
        Spread(Rc<SpreadAssignment>),
    }
    pub struct KeyValueProp {
        pub node_id: NodeId,
        pub key: PropName,
        pub value: Expr,
        pub cached_hash: u8,
    }
    pub struct AssignProp {
        pub node_id: NodeId,
        pub key: Rc<Ident>,
        pub value: Expr,
        pub cached_hash: u8,
    }
    pub struct GetterProp {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PropName,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub body: Option<Rc<BlockStmt>>,
        pub cached_hash: u8,
    }
    pub struct SetterProp {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PropName,
        pub param: Pat,
        pub body: Option<Rc<BlockStmt>>,
        pub cached_hash: u8,
    }
    pub struct MethodProp {
        pub node_id: NodeId,
        pub key: PropName,
        pub function: Rc<Function>,
        pub cached_hash: u8,
    }
    pub struct SpreadAssignment {
        pub node_id: NodeId,
        pub dot3_token: Span,
        pub expr: Expr,
        pub cached_hash: u8,
    }
    pub enum PropName {
        Ident(Rc<Ident>),
        Str(Rc<Str>),
        Num(Rc<Number>),
        Computed(Rc<ComputedPropName>),
    }
    pub struct ComputedPropName {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Expr,
        pub cached_hash: u8,
    }
    pub struct BlockStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub stmts: Vec<Stmt>,
        pub cached_hash: u8,
    }
    pub enum Stmt {
        Block(Rc<BlockStmt>),
        Empty(Rc<EmptyStmt>),
        Debugger(Rc<DebuggerStmt>),
        With(Rc<WithStmt>),
        Return(Rc<ReturnStmt>),
        Labeled(Rc<LabeledStmt>),
        Break(Rc<BreakStmt>),
        Continue(Rc<ContinueStmt>),
        If(Rc<IfStmt>),
        Switch(Rc<SwitchStmt>),
        Throw(Rc<ThrowStmt>),
        Try(Rc<TryStmt>),
        While(Rc<WhileStmt>),
        DoWhile(Rc<DoWhileStmt>),
        For(Rc<ForStmt>),
        ForIn(Rc<ForInStmt>),
        ForOf(Rc<ForOfStmt>),
        Decl(Decl),
        Expr(Rc<ExprStmt>),
    }
    pub struct ExprStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Expr,
        pub cached_hash: u8,
    }
    pub struct EmptyStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub cached_hash: u8,
    }
    pub struct DebuggerStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub cached_hash: u8,
    }
    pub struct WithStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub obj: Expr,
        pub body: Stmt,
        pub cached_hash: u8,
    }
    pub struct ReturnStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub arg: Option<Expr>,
        pub cached_hash: u8,
    }
    pub struct LabeledStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub label: Rc<Ident>,
        pub body: Stmt,
        pub cached_hash: u8,
    }
    pub struct BreakStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub label: Option<Rc<Ident>>,
        pub cached_hash: u8,
    }
    pub struct ContinueStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub label: Option<Rc<Ident>>,
        pub cached_hash: u8,
    }
    pub struct IfStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub test: Expr,
        pub cons: Stmt,
        pub alt: Option<Stmt>,
        pub cached_hash: u8,
    }
    pub struct SwitchStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub discriminant: Expr,
        pub cases: Vec<Rc<SwitchCase>>,
        pub cached_hash: u8,
    }
    pub struct ThrowStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub arg: Expr,
        pub cached_hash: u8,
    }
    pub struct TryStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub block: Rc<BlockStmt>,
        pub handler: Option<Rc<CatchClause>>,
        pub finalizer: Option<Rc<BlockStmt>>,
        pub cached_hash: u8,
    }
    pub struct WhileStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub test: Expr,
        pub body: Stmt,
        pub cached_hash: u8,
    }
    pub struct DoWhileStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub body: Stmt,
        pub test: Expr,
        pub cached_hash: u8,
    }
    pub struct ForStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub init: Option<VarDeclOrExpr>,
        pub test: Option<Expr>,
        pub update: Option<Expr>,
        pub body: Stmt,
        pub cached_hash: u8,
    }
    pub struct ForInStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub left: VarDeclOrPat,
        pub right: Expr,
        pub body: Stmt,
        pub cached_hash: u8,
    }
    pub struct ForOfStmt {
        pub node_id: NodeId,
        pub span: Span,
        pub await_token: Option<Span>,
        pub left: VarDeclOrPat,
        pub right: Expr,
        pub body: Stmt,
        pub cached_hash: u8,
    }
    pub struct SwitchCase {
        pub node_id: NodeId,
        pub span: Span,
        pub test: Option<Expr>,
        pub cons: Vec<Stmt>,
        pub cached_hash: u8,
    }
    pub struct CatchClause {
        pub node_id: NodeId,
        pub span: Span,
        pub param: Option<Pat>,
        pub body: Rc<BlockStmt>,
        pub cached_hash: u8,
    }
    pub enum VarDeclOrPat {
        VarDecl(Rc<VarDecl>),
        Pat(Pat),
    }
    pub enum VarDeclOrExpr {
        VarDecl(Rc<VarDecl>),
        Expr(Expr),
    }
    pub struct TsTypeAnn {
        pub node_id: NodeId,
        pub span: Span,
        pub type_ann: TsType,
        pub cached_hash: u8,
    }
    pub struct TsTypeParamDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub name: Rc<Ident>,
        pub constraint: Option<TsType>,
        pub default: Option<TsType>,
        pub cached_hash: u8,
    }
    pub struct TsTypeParamInstantiation {
        pub node_id: NodeId,
        pub span: Span,
        pub params: Vec<TsType>,
        pub cached_hash: u8,
    }
    pub struct TsParamProp {
        pub node_id: NodeId,
        pub span: Span,
        pub decorators: Vec<Rc<Decorator>>,
        pub accessibility: Option<Accessibility>,
        pub is_override: bool,
        pub readonly: bool,
        pub param: TsParamPropParam,
        pub cached_hash: u8,
    }
    pub enum TsParamPropParam {
        Ident(Rc<BindingIdent>),
        Assign(Rc<AssignPat>),
    }
    pub struct TsQualifiedName {
        pub node_id: NodeId,
        pub left: TsEntityName,
        pub right: Rc<Ident>,
        pub cached_hash: u8,
    }
    pub enum TsEntityName {
        TsQualifiedName(Rc<TsQualifiedName>),
        Ident(Rc<Ident>),
    }
    pub enum TsTypeElement {
        TsCallSignatureDecl(Rc<TsCallSignatureDecl>),
        TsConstructSignatureDecl(Rc<TsConstructSignatureDecl>),
        TsPropertySignature(Rc<TsPropertySignature>),
        TsGetterSignature(Rc<TsGetterSignature>),
        TsSetterSignature(Rc<TsSetterSignature>),
        TsMethodSignature(Rc<TsMethodSignature>),
        TsIndexSignature(Rc<TsIndexSignature>),
    }
    pub struct TsCallSignatureDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub type_params: Option<Vec<Rc<TsTypeParamDecl>>>,
        pub params: Vec<TsAmbientParam>,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub cached_hash: u8,
    }
    pub struct TsConstructSignatureDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub type_params: Option<Vec<Rc<TsTypeParamDecl>>>,
        pub params: Vec<TsAmbientParam>,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub cached_hash: u8,
    }
    pub struct TsPropertySignature {
        pub node_id: NodeId,
        pub span: Span,
        pub readonly: bool,
        pub key: PropName,
        pub optional: bool,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub cached_hash: u8,
    }
    pub struct TsGetterSignature {
        pub node_id: NodeId,
        pub span: Span,
        pub readonly: bool,
        pub key: PropName,
        pub optional: bool,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub cached_hash: u8,
    }
    pub struct TsSetterSignature {
        pub node_id: NodeId,
        pub span: Span,
        pub readonly: bool,
        pub key: PropName,
        pub optional: bool,
        pub param: TsAmbientParam,
        pub cached_hash: u8,
    }
    pub struct TsMethodSignature {
        pub node_id: NodeId,
        pub span: Span,
        pub readonly: bool,
        pub key: PropName,
        pub optional: bool,
        pub type_params: Option<Vec<Rc<TsTypeParamDecl>>>,
        pub params: Vec<TsAmbientParam>,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub cached_hash: u8,
    }
    pub struct TsIndexSignature {
        pub node_id: NodeId,
        pub params: Vec<TsAmbientParam>,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub readonly: bool,
        pub is_static: bool,
        pub span: Span,
        pub cached_hash: u8,
    }
    pub enum TsType {
        TsKeywordType(Rc<TsKeywordType>),
        TsThisType(Rc<TsThisType>),
        TsFnOrConstructorType(TsFnOrConstructorType),
        TsTypeRef(Rc<TsTypeRef>),
        TsTypeQuery(Rc<TsTypeQuery>),
        TsTypeLit(Rc<TsTypeLit>),
        TsArrayType(Rc<TsArrayType>),
        TsTupleType(Rc<TsTupleType>),
        TsOptionalType(Rc<TsOptionalType>),
        TsRestType(Rc<TsRestType>),
        TsUnionOrIntersectionType(TsUnionOrIntersectionType),
        TsConditionalType(Rc<TsConditionalType>),
        TsInferType(Rc<TsInferType>),
        TsParenthesizedType(Rc<TsParenthesizedType>),
        TsTypeOperator(Rc<TsTypeOperator>),
        TsIndexedAccessType(Rc<TsIndexedAccessType>),
        TsMappedType(Rc<TsMappedType>),
        TsLitType(Rc<TsLitType>),
        TsTypePredicate(Rc<TsTypePredicate>),
        TsImportType(Rc<TsImportType>),
    }
    pub enum TsFnOrConstructorType {
        TsFnType(Rc<TsFnType>),
        TsConstructorType(Rc<TsConstructorType>),
    }
    pub struct TsKeywordType {
        pub node_id: NodeId,
        pub span: Span,
        pub kind: TsKeywordTypeKind,
        pub cached_hash: u8,
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
        pub cached_hash: u8,
    }
    pub struct TsAmbientParam {
        pub node_id: NodeId,
        pub pat: TsAmbientParamPat,
        pub cached_hash: u8,
    }
    pub enum TsAmbientParamPat {
        Ident(Rc<BindingIdent>),
        Array(Rc<ArrayPat>),
        Rest(Rc<RestPat>),
        Object(Rc<ObjectPat>),
    }
    pub struct TsFnType {
        pub node_id: NodeId,
        pub span: Span,
        pub type_params: Option<Vec<Rc<TsTypeParamDecl>>>,
        pub params: Vec<TsAmbientParam>,
        pub type_ann: Rc<TsTypeAnn>,
        pub cached_hash: u8,
    }
    pub struct TsConstructorType {
        pub node_id: NodeId,
        pub span: Span,
        pub type_params: Option<Vec<Rc<TsTypeParamDecl>>>,
        pub params: Vec<TsAmbientParam>,
        pub type_ann: Rc<TsTypeAnn>,
        pub is_abstract: bool,
        pub cached_hash: u8,
    }
    pub struct TsTypeRef {
        pub node_id: NodeId,
        pub span: Span,
        pub type_name: TsEntityName,
        pub type_params: Option<Rc<TsTypeParamInstantiation>>,
        pub cached_hash: u8,
    }
    pub struct TsTypePredicate {
        pub node_id: NodeId,
        pub span: Span,
        pub asserts: bool,
        pub param_name: TsThisTypeOrIdent,
        pub type_ann: Option<Rc<TsTypeAnn>>,
        pub cached_hash: u8,
    }
    pub enum TsThisTypeOrIdent {
        TsThisType(Rc<TsThisType>),
        Ident(Rc<Ident>),
    }
    pub struct TsTypeQuery {
        pub node_id: NodeId,
        pub span: Span,
        pub expr_name: TsTypeQueryExpr,
        pub cached_hash: u8,
    }
    pub enum TsTypeQueryExpr {
        TsEntityName(TsEntityName),
        Import(Rc<TsImportType>),
    }
    pub struct TsImportType {
        pub node_id: NodeId,
        pub span: Span,
        pub arg: Rc<Str>,
        pub qualifier: Option<TsEntityName>,
        pub type_args: Option<Rc<TsTypeParamInstantiation>>,
        pub cached_hash: u8,
    }
    pub struct TsTypeLit {
        pub node_id: NodeId,
        pub span: Span,
        pub members: Vec<TsTypeElement>,
        pub cached_hash: u8,
    }
    pub struct TsArrayType {
        pub node_id: NodeId,
        pub span: Span,
        pub elem_type: TsType,
        pub cached_hash: u8,
    }
    pub struct TsTupleType {
        pub node_id: NodeId,
        pub span: Span,
        pub elem_types: Vec<Rc<TsTupleElement>>,
        pub cached_hash: u8,
    }
    pub struct TsTupleElement {
        pub node_id: NodeId,
        pub span: Span,
        pub label: Option<Pat>,
        pub ty: TsType,
        pub cached_hash: u8,
    }
    pub struct TsOptionalType {
        pub node_id: NodeId,
        pub span: Span,
        pub type_ann: TsType,
        pub cached_hash: u8,
    }
    pub struct TsRestType {
        pub node_id: NodeId,
        pub span: Span,
        pub type_ann: TsType,
        pub cached_hash: u8,
    }
    pub enum TsUnionOrIntersectionType {
        TsUnionType(Rc<TsUnionType>),
        TsIntersectionType(Rc<TsIntersectionType>),
    }
    pub struct TsUnionType {
        pub node_id: NodeId,
        pub span: Span,
        pub types: Vec<TsType>,
        pub cached_hash: u8,
    }
    pub struct TsIntersectionType {
        pub node_id: NodeId,
        pub span: Span,
        pub types: Vec<TsType>,
        pub cached_hash: u8,
    }
    pub struct TsConditionalType {
        pub node_id: NodeId,
        pub span: Span,
        pub check_type: TsType,
        pub extends_type: TsType,
        pub true_type: TsType,
        pub false_type: TsType,
        pub cached_hash: u8,
    }
    pub struct TsInferType {
        pub node_id: NodeId,
        pub span: Span,
        pub type_param: Rc<TsTypeParamDecl>,
        pub cached_hash: u8,
    }
    pub struct TsParenthesizedType {
        pub node_id: NodeId,
        pub span: Span,
        pub type_ann: TsType,
        pub cached_hash: u8,
    }
    pub struct TsTypeOperator {
        pub node_id: NodeId,
        pub span: Span,
        pub op: TsTypeOperatorOp,
        pub type_ann: TsType,
        pub cached_hash: u8,
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
        pub obj_type: TsType,
        pub index_type: TsType,
        pub cached_hash: u8,
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
        pub type_param: Rc<TsTypeParamDecl>,
        pub name_type: Option<TsType>,
        pub optional: Option<TruePlusMinus>,
        pub type_ann: Option<TsType>,
        pub cached_hash: u8,
    }
    pub struct TsLitType {
        pub node_id: NodeId,
        pub span: Span,
        pub lit: TsLit,
        pub cached_hash: u8,
    }
    pub enum TsLit {
        BigInt(Rc<BigInt>),
        Number(Rc<Number>),
        Str(Rc<Str>),
        Bool(Rc<Bool>),
        Tpl(Rc<TsTplLitType>),
    }
    pub struct TsTplLitType {
        pub node_id: NodeId,
        pub span: Span,
        pub types: Vec<TsType>,
        pub quasis: Vec<Rc<TplElement>>,
        pub cached_hash: u8,
    }
    pub struct TsInterfaceDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub id: Rc<Ident>,
        pub declare: bool,
        pub type_params: Option<Vec<Rc<TsTypeParamDecl>>>,
        pub extends: Vec<Rc<TsExprWithTypeArgs>>,
        pub body: Rc<TsInterfaceBody>,
        pub cached_hash: u8,
    }
    pub struct TsInterfaceBody {
        pub node_id: NodeId,
        pub span: Span,
        pub body: Vec<TsTypeElement>,
        pub cached_hash: u8,
    }
    pub struct TsExprWithTypeArgs {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: TsEntityName,
        pub type_args: Option<Rc<TsTypeParamInstantiation>>,
        pub cached_hash: u8,
    }
    pub struct TsTypeAliasDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub declare: bool,
        pub id: Rc<Ident>,
        pub type_params: Option<Vec<Rc<TsTypeParamDecl>>>,
        pub type_ann: TsType,
        pub cached_hash: u8,
    }
    pub struct TsEnumDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub declare: bool,
        pub is_const: bool,
        pub id: Rc<Ident>,
        pub members: Vec<Rc<TsEnumMember>>,
        pub cached_hash: u8,
    }
    pub struct TsEnumMember {
        pub node_id: NodeId,
        pub span: Span,
        pub id: TsEnumMemberId,
        pub init: Option<Expr>,
        pub cached_hash: u8,
    }
    pub enum TsEnumMemberId {
        Ident(Rc<Ident>),
        Str(Rc<Str>),
    }
    pub struct TsModuleDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub declare: bool,
        pub global: bool,
        pub id: TsModuleName,
        pub body: Option<TsNamespaceBody>,
        pub cached_hash: u8,
    }
    pub enum TsNamespaceBody {
        TsModuleBlock(Rc<TsModuleBlock>),
        TsNamespaceDecl(Rc<TsNamespaceDecl>),
    }
    pub struct TsModuleBlock {
        pub node_id: NodeId,
        pub span: Span,
        pub body: Vec<ModuleItem>,
        pub cached_hash: u8,
    }
    pub struct TsNamespaceDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub declare: bool,
        pub global: bool,
        pub id: Rc<Ident>,
        pub body: TsNamespaceBody,
        pub cached_hash: u8,
    }
    pub enum TsModuleName {
        Ident(Rc<Ident>),
        Str(Rc<Str>),
    }
    pub struct TsImportEqualsDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub declare: bool,
        pub is_export: bool,
        pub is_type_only: bool,
        pub id: Rc<Ident>,
        pub module_ref: TsModuleRef,
        pub cached_hash: u8,
    }
    pub enum TsModuleRef {
        TsEntityName(TsEntityName),
        TsExternalModuleRef(Rc<TsExternalModuleRef>),
    }
    pub struct TsExternalModuleRef {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Rc<Str>,
        pub cached_hash: u8,
    }
    pub struct TsExportAssignment {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Expr,
        pub cached_hash: u8,
    }
    pub struct TsNamespaceExportDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub id: Rc<Ident>,
        pub cached_hash: u8,
    }
    pub struct TsAsExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Expr,
        pub type_ann: TsType,
        pub cached_hash: u8,
    }
    pub struct TsTypeAssertion {
        pub node_id: NodeId,
        pub span: Span,
        pub type_ann: TsType,
        pub expr: Expr,
        pub cached_hash: u8,
    }
    pub struct TsNonNullExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Expr,
        pub cached_hash: u8,
    }
    pub enum Accessibility {
        Public,
        Protected,
        Private,
    }
    pub struct TsConstAssertion {
        pub node_id: NodeId,
        pub span: Span,
        pub expr: Expr,
        pub cached_hash: u8,
    }
});
