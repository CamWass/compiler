// This is not a public api.
#[doc(hidden)]
pub extern crate ast;

use ast::*;
use atoms::JsWord;
use global_common::SyntaxContext;
use global_visit::{define, AndThen, Repeat, Repeated};
use num_bigint::BigInt as BigIntValue;
use std::any::Any;

/// Visitable nodes.
pub trait Node: Any {}

impl<T: ?Sized> Node for T where T: Any {}

// impl<A, B> Fold for AndThen<A, B>
// where
//     A: Fold,

//     B: Fold,
// {
//     #[inline(always)]
//     fn fold_module(&mut self, n: Module) -> Module {
//         let n = self.first.fold_module(n);
//         self.second.fold_module(n)
//     }

//     #[inline(always)]
//     fn fold_script(&mut self, n: Script) -> Script {
//         let n = self.first.fold_script(n);
//         self.second.fold_script(n)
//     }
// }

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

// impl<V> Fold for Repeat<V>
// where
//     V: Fold + Repeated,
// {
//     fn fold_module(&mut self, mut node: Module) -> Module {
//         loop {
//             self.pass.reset();
//             node = node.fold_with(&mut self.pass);

//             if !self.pass.changed() {
//                 break;
//             }
//         }

//         node
//     }

//     fn fold_script(&mut self, mut node: Script) -> Script {
//         loop {
//             self.pass.reset();
//             node = node.fold_with(&mut self.pass);

//             if !self.pass.changed() {
//                 break;
//             }
//         }

//         node
//     }
// }

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
        pub decorators: Vec<Decorator>,
        pub extends: Option<ExtendsClause>,
        pub body: Vec<ClassMember>,
    }

    pub struct ExtendsClause {
        pub node_id: NodeId,
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
        pub key: PropName,
        pub value: Option<Box<Expr>>,
        pub is_static: bool,
        pub decorators: Vec<Decorator>,
    }
    pub struct PrivateProp {
        pub node_id: NodeId,
        pub key: PrivateName,
        pub value: Option<Box<Expr>>,
        pub is_static: bool,
        pub decorators: Vec<Decorator>,
    }
    pub struct ClassMethod {
        pub node_id: NodeId,
        pub key: PropName,
        pub function: Function,
        pub kind: MethodKind,
        pub is_static: bool,
    }
    pub struct PrivateMethod {
        pub node_id: NodeId,
        pub key: PrivateName,
        pub function: Function,
        pub kind: MethodKind,
        pub is_static: bool,
    }
    pub struct Constructor {
        pub node_id: NodeId,
        pub params: Vec<Param>,
        pub body: BlockStmt,
    }
    pub struct Decorator {
        pub node_id: NodeId,
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
        PrivateName(PrivateName),
        OptChain(OptChainExpr),
        Invalid(Invalid),
    }
    pub struct ThisExpr {
        pub node_id: NodeId,
    }
    pub struct ArrayLit {
        pub node_id: NodeId,
        pub elems: Vec<Option<ExprOrSpread>>,
    }
    pub struct ObjectLit {
        pub node_id: NodeId,
        pub props: Vec<Prop>,
    }
    pub struct SpreadElement {
        pub node_id: NodeId,
        pub expr: Box<Expr>,
    }
    pub struct UnaryExpr {
        pub node_id: NodeId,
        pub op: UnaryOp,
        pub arg: Box<Expr>,
    }
    pub struct UpdateExpr {
        pub node_id: NodeId,
        pub op: UpdateOp,
        pub prefix: bool,
        pub arg: Box<Expr>,
    }
    pub struct BinExpr {
        pub node_id: NodeId,
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
        pub op: AssignOp,
        pub left: PatOrExpr,
        pub right: Box<Expr>,
    }
    pub struct MemberExpr {
        pub node_id: NodeId,
        pub obj: ExprOrSuper,
        pub prop: Box<Expr>,
        pub computed: bool,
    }
    pub struct CondExpr {
        pub node_id: NodeId,
        pub test: Box<Expr>,
        pub cons: Box<Expr>,
        pub alt: Box<Expr>,
    }
    pub struct CallExpr {
        pub node_id: NodeId,
        pub callee: ExprOrSuper,
        pub args: Vec<ExprOrSpread>,
    }
    pub struct NewExpr {
        pub node_id: NodeId,
        pub callee: Box<Expr>,
        pub args: Option<Vec<ExprOrSpread>>,
    }
    pub struct SeqExpr {
        pub node_id: NodeId,
        pub exprs: Vec<Box<Expr>>,
    }
    pub struct ArrowExpr {
        pub node_id: NodeId,
        pub params: Vec<ParamWithoutDecorators>,
        pub body: BlockStmt,
        pub is_async: bool,
    }
    pub struct YieldExpr {
        pub node_id: NodeId,
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
        pub arg: Box<Expr>,
    }
    pub struct Tpl {
        pub node_id: NodeId,
        pub exprs: Vec<Box<Expr>>,
        pub quasis: Vec<TplElement>,
    }
    pub struct TaggedTpl {
        pub node_id: NodeId,
        pub tag: Box<Expr>,
        pub tpl: Tpl,
    }
    pub struct TplElement {
        pub node_id: NodeId,
        pub tail: bool,
        pub cooked: Option<Str>,
        pub raw: Str,
    }
    pub enum ExprOrSuper {
        Super(Super),
        Expr(Box<Expr>),
    }
    pub struct Super {
        pub node_id: NodeId,
    }
    pub enum ExprOrSpread {
        Spread(SpreadElement),
        Expr(Box<Expr>),
    }
    pub enum PatOrExpr {
        Expr(Box<Expr>),
        Pat(Box<Pat>),
    }
    pub struct OptChainExpr {
        pub node_id: NodeId,
        pub expr: Box<Expr>,
    }
    pub struct Function {
        pub node_id: NodeId,
        pub params: Vec<Param>,
        pub decorators: Vec<Decorator>,
        pub body: BlockStmt,
        pub is_generator: bool,
        pub is_async: bool,
    }
    pub struct Param {
        pub node_id: NodeId,
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
        pub sym: JsWord,
        pub ctxt: SyntaxContext,
    }

    pub struct PrivateName {
        pub node_id: NodeId,
        pub id: Ident,
    }

    pub struct Invalid {
        pub node_id: NodeId,
    }
    pub enum Lit {
        Str(Str),
        Bool(Bool),
        Null(Null),
        Num(Number),
        BigInt(BigInt),
        Regex(Regex),
    }
    pub struct BigInt {
        pub node_id: NodeId,
        pub value: BigIntValue,
    }
    pub struct Str {
        pub node_id: NodeId,
        pub value: JsWord,
        pub has_escape: bool,
        pub kind: StrKind,
    }
    pub struct Bool {
        pub node_id: NodeId,
        pub value: bool,
    }
    pub struct Null {
        pub node_id: NodeId,
    }
    pub struct Regex {
        pub node_id: NodeId,
        pub exp: JsWord,
        pub flags: JsWord,
    }
    pub struct Number {
        pub node_id: NodeId,
        pub value: f64,
        pub raw: Option<JsWord>,
    }
    pub enum Program {
        Module(Module),
        Script(Script),
    }
    pub struct Module {
        pub node_id: NodeId,
        pub body: Vec<ModuleItem>,
        pub shebang: Option<JsWord>,
    }
    pub struct Script {
        pub node_id: NodeId,
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
        pub expr: Box<Expr>,
    }
    pub struct ExportDecl {
        pub node_id: NodeId,
        pub decl: Decl,
    }
    pub struct ImportDecl {
        pub node_id: NodeId,
        pub specifiers: Vec<ImportSpecifier>,
        pub src: Str,
        pub asserts: Option<ObjectLit>,
    }
    pub struct ExportAll {
        pub node_id: NodeId,
        pub src: Str,
        pub asserts: Option<ObjectLit>,
    }
    pub struct NamedExport {
        pub node_id: NodeId,
        pub specifiers: Vec<ExportSpecifier>,
        pub src: Option<Str>,
        pub asserts: Option<ObjectLit>,
    }
    pub struct ExportDefaultDecl {
        pub node_id: NodeId,
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
        pub local: Ident,
    }
    pub struct ImportStarAsSpecifier {
        pub node_id: NodeId,
        pub local: Ident,
    }
    pub struct ImportNamedSpecifier {
        pub node_id: NodeId,
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
        pub name: Ident,
    }
    pub struct ExportDefaultSpecifier {
        pub node_id: NodeId,
        pub exported: Ident,
    }
    pub struct ExportNamedSpecifier {
        pub node_id: NodeId,
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
        pub elems: Vec<Option<Pat>>,
    }
    pub struct ObjectPat {
        pub node_id: NodeId,
        pub props: Vec<ObjectPatProp>,
    }
    pub struct AssignPat {
        pub node_id: NodeId,
        pub left: Box<Pat>,
        pub right: Box<Expr>,
    }
    pub struct RestPat {
        pub node_id: NodeId,
        pub arg: Box<Pat>,
    }
    pub enum ObjectPatProp {
        KeyValue(KeyValuePatProp),
        Rest(RestPat),
    }
    pub struct KeyValuePatProp {
        pub node_id: NodeId,
        pub key: PropName,
        pub value: Box<Pat>,
    }
    pub enum Prop {
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
        pub key: PropName,
        pub body: BlockStmt,
    }
    pub struct SetterProp {
        pub node_id: NodeId,
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
        pub expr: Box<Expr>,
    }
    pub enum PropName {
        Ident(Ident),
        Str(Str),
        Num(Number),
        Computed(ComputedPropName),
        BigInt(BigInt),
    }
    pub struct ComputedPropName {
        pub node_id: NodeId,
        pub expr: Box<Expr>,
    }
    pub struct BlockStmt {
        pub node_id: NodeId,
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
        pub expr: Box<Expr>,
    }
    pub struct EmptyStmt {
        pub node_id: NodeId,
    }
    pub struct DebuggerStmt {
        pub node_id: NodeId,
    }
    pub struct WithStmt {
        pub node_id: NodeId,
        pub obj: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct ReturnStmt {
        pub node_id: NodeId,
        pub arg: Option<Box<Expr>>,
    }
    pub struct LabeledStmt {
        pub node_id: NodeId,
        pub label: Ident,
        pub body: Box<Stmt>,
    }
    pub struct BreakStmt {
        pub node_id: NodeId,
        pub label: Option<Ident>,
    }
    pub struct ContinueStmt {
        pub node_id: NodeId,
        pub label: Option<Ident>,
    }
    pub struct IfStmt {
        pub node_id: NodeId,
        pub test: Box<Expr>,
        pub cons: Box<Stmt>,
        pub alt: Option<Box<Stmt>>,
    }
    pub struct SwitchStmt {
        pub node_id: NodeId,
        pub discriminant: Box<Expr>,
        pub cases: Vec<SwitchCase>,
    }
    pub struct ThrowStmt {
        pub node_id: NodeId,
        pub arg: Box<Expr>,
    }
    pub struct TryStmt {
        pub node_id: NodeId,
        pub block: BlockStmt,
        pub handler: Option<CatchClause>,
        pub finalizer: Option<BlockStmt>,
    }
    pub struct WhileStmt {
        pub node_id: NodeId,
        pub test: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct DoWhileStmt {
        pub node_id: NodeId,
        pub test: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct ForStmt {
        pub node_id: NodeId,
        pub init: Option<VarDeclOrExpr>,
        pub test: Option<Box<Expr>>,
        pub update: Option<Box<Expr>>,
        pub body: Box<Stmt>,
    }
    pub struct ForInStmt {
        pub node_id: NodeId,
        pub left: VarDeclOrPat,
        pub right: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct ForOfStmt {
        pub node_id: NodeId,
        pub is_await: bool,
        pub left: VarDeclOrPat,
        pub right: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct SwitchCase {
        pub node_id: NodeId,
        pub test: Option<Box<Expr>>,
        pub cons: Vec<Stmt>,
    }
    pub struct CatchClause {
        pub node_id: NodeId,
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
