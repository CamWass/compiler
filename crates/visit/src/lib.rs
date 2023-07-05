// This is not a public api.
#[doc(hidden)]
pub extern crate ast;

use ast::*;
use atoms::JsWord;
use global_common::{Span, DUMMY_SP};
use global_visit::{define, AndThen, Repeat, Repeated};
use num_bigint::BigInt as BigIntValue;
use std::{any::Any, fmt::Debug};

/// Visitable nodes.
pub trait Node: Any {}

impl<T: ?Sized> Node for T where T: Any {}

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

impl<V> VisitMut<'_> for Repeat<V>
where
    V: for<'a> VisitMut<'a> + Repeated,
{
    fn visit_mut_program(&mut self, node: &mut Program) {
        loop {
            self.pass.reset();
            node.visit_mut_with(&mut self.pass);

            if !self.pass.changed() {
                break;
            }
        }
    }

    fn visit_mut_module(&mut self, node: &mut Module) {
        loop {
            self.pass.reset();
            node.visit_mut_with(&mut self.pass);

            if !self.pass.changed() {
                break;
            }
        }
    }

    fn visit_mut_script(&mut self, node: &mut Script) {
        loop {
            self.pass.reset();
            node.visit_mut_with(&mut self.pass);

            if !self.pass.changed() {
                break;
            }
        }
    }
}

/// Not a public api.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct SpanRemover;

/// Returns a `Fold` which changes all span into `DUMMY_SP`.
pub fn span_remover() -> impl Debug + Fold + Copy + Eq + Default + 'static {
    SpanRemover
}

impl Fold for SpanRemover {
    fn fold_span(&mut self, _: Span) -> Span {
        DUMMY_SP
    }
}

#[macro_export]
macro_rules! assert_eq_ignore_span {
    ($l:expr, $r:expr) => {{
        use $crate::FoldWith;
        let l = $l.fold_with(&mut $crate::span_remover());
        let r = $r.fold_with(&mut $crate::span_remover());

        assert_eq!(l, r);
    }};

    ($l:expr, $r:expr, $($tts:tt)*) => {{
        use $crate::FoldWith;
        let l = $l.fold_with(&mut $crate::span_remover());
        let r = $r.fold_with(&mut $crate::span_remover());

        assert_eq!(l, r, $($tts)*);
    }};
}

// pub fn as_folder<V>(v: V) -> Folder<V>
// where
//     V: VisitMut,
// {
//     Folder(v)
// }

// /// Wrap a [VisitMut] as a [Fold]
// #[derive(Debug, Clone, Copy)]
// pub struct Folder<V: VisitMut>(V);

// impl<V> Repeated for Folder<V>
// where
//     V: Repeated + VisitMut,
// {
//     #[inline(always)]
//     fn changed(&self) -> bool {
//         self.0.changed()
//     }

//     #[inline(always)]
//     fn reset(&mut self) {
//         self.0.reset();
//     }
// }

// impl<V> CompilerPass for Folder<V>
// where
//     V: VisitMut + CompilerPass,
// {
//     fn name() -> Cow<'static, str> {
//         V::name()
//     }
// }

// macro_rules! method {
//     ($name:ident, $T:ty) => {
//         #[inline(always)]
//         fn $name(&mut self, mut n: $T) -> $T {
//             n.visit_mut_with(&mut self.0);
//             n
//         }
//     };
// }

// impl<V> Fold for Folder<V>
// where
//     V: VisitMut,
// {
//     method!(fold_ident, Ident);
//     method!(fold_span, Span);

//     method!(fold_expr, Expr);
//     method!(fold_decl, Decl);
//     method!(fold_stmt, Stmt);
//     method!(fold_pat, Pat);

//     method!(fold_module, Module);
//     method!(fold_script, Script);
//     method!(fold_program, Program);
// }

define!({
    pub struct Class {
        pub node_id: NodeId,
        pub span: Span,
        pub decorators: Vec<Decorator>,
        pub extends: Option<ExtendsClause>,
        pub body: Vec<ClassMember>,
    }

    pub struct ExtendsClause {
        pub node_id: NodeId,
        pub span: Span,
        pub super_class: Box<Expr>,
    }

    pub enum ClassMember {
        Constructor(Constructor),
        Method(ClassMethod),
        PrivateMethod(PrivateMethod),
        ClassProp(ClassProp),
        PrivateProp(PrivateProp),
        Empty(EmptyStmt),
    }

    pub struct ClassProp {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PropName,
        pub value: Option<Box<Expr>>,
        pub is_static: bool,
        pub decorators: Vec<Decorator>,
    }
    pub struct PrivateProp {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PrivateName,
        pub value: Option<Box<Expr>>,
        pub is_static: bool,
        pub decorators: Vec<Decorator>,
    }
    pub struct ClassMethod {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PropName,
        pub function: Function,
        pub kind: MethodKind,
        pub is_static: bool,
    }
    pub struct PrivateMethod {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PrivateName,
        pub function: Function,
        pub kind: MethodKind,
        pub is_static: bool,
    }
    pub struct Constructor {
        pub node_id: NodeId,
        pub span: Span,
        pub params: Vec<Param>,
        pub body: BlockStmt,
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
    }
    pub struct FnDecl {
        pub node_id: NodeId,
        pub ident: Ident,
        pub function: Function,
    }
    pub struct ClassDecl {
        pub node_id: NodeId,
        pub ident: Ident,
        pub class: Class,
    }
    pub struct VarDecl {
        pub node_id: NodeId,
        pub span: Span,
        pub kind: VarDeclKind,
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
    }
    pub struct NewExpr {
        pub node_id: NodeId,
        pub span: Span,
        pub callee: Box<Expr>,
        pub args: Option<Vec<ExprOrSpread>>,
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
        pub body: BlockStmt,
        pub is_generator: bool,
        pub is_async: bool,
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

    pub struct BindingIdent {
        pub node_id: NodeId,
        pub id: Ident,
    }

    pub struct Ident {
        pub node_id: NodeId,
        pub span: Span,
        pub sym: JsWord,
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
    }
    pub struct ObjectPat {
        pub node_id: NodeId,
        pub span: Span,
        pub props: Vec<ObjectPatProp>,
    }
    pub struct AssignPat {
        pub node_id: NodeId,
        pub span: Span,
        pub left: Box<Pat>,
        pub right: Box<Expr>,
    }
    pub struct RestPat {
        pub node_id: NodeId,
        pub span: Span,
        pub dot3_token: Span,
        pub arg: Box<Pat>,
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
        pub body: BlockStmt,
    }
    pub struct SetterProp {
        pub node_id: NodeId,
        pub span: Span,
        pub key: PropName,
        pub param: ParamWithoutDecorators,
        pub body: BlockStmt,
    }
    pub struct MethodProp {
        pub node_id: NodeId,
        pub key: PropName,
        pub function: Function,
    }
    pub struct SpreadAssignment {
        pub node_id: NodeId,
        pub dot3_token: Span,
        pub expr: Box<Expr>,
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
});
