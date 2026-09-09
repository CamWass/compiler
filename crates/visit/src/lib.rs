#![deny(unused)]

use ast::*;
use visit_macros::define;

define!({
    pub struct Class {
        pub node_id: NodeId,
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
    }

    pub struct ClassProp {
        pub node_id: NodeId,
        pub key: PropName,
        pub value: Option<Box<Expr>>,
        pub is_static: bool,
    }
    pub struct PrivateProp {
        pub node_id: NodeId,
        pub key: PrivateName,
        pub value: Option<Box<Expr>>,
        pub is_static: bool,
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
        pub params: FunctionParams,
        pub body: BlockStmt,
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
        pub function: Box<Function>,
    }
    pub struct ClassDecl {
        pub node_id: NodeId,
        pub ident: Ident,
        pub class: Box<Class>,
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
        pub name: BindingPatOrIdent,
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
        Arrow(Box<ArrowExpr>),
        Class(Box<ClassExpr>),
        Yield(YieldExpr),
        MetaProp(MetaPropExpr),
        Await(AwaitExpr),
        PrivateName(PrivateName),
        OptChain(OptChainExpr),
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
        pub arg: Box<SimpleAssignTarget>,
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
        pub left: Box<AssignTarget>,
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
        pub exprs: Vec<Expr>,
    }
    pub struct ArrowExpr {
        pub node_id: NodeId,
        pub params: FunctionParams,
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
        pub exprs: Vec<Expr>,
        pub quasis: Vec<TplElement>,
    }
    pub struct TaggedTpl {
        pub node_id: NodeId,
        pub tag: Box<Expr>,
        pub tpl: Box<Tpl>,
    }
    pub struct TplElement {
        pub node_id: NodeId,
        pub value: TplString,
    }
    pub enum TplString {
        Cooked(Box<String>),
        Raw(Box<String>),
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
    pub enum AssignTarget {
        Simple(SimpleAssignTarget),
        AssignmentPat(AssignmentPat),
    }
    pub enum SimpleAssignTarget {
        Ident(BindingIdent),
        Member(MemberExpr),
    }
    pub struct OptChainExpr {
        pub node_id: NodeId,
        pub base: Box<OptChainBase>,
    }
    pub enum OptChainBase {
        Call(CallExpr),
        Member(MemberExpr),
    }
    pub struct Function {
        pub node_id: NodeId,
        pub params: FunctionParams,
        pub body: BlockStmt,
        pub flags: FnFlags,
    }
    pub struct Param {
        pub node_id: NodeId,
        pub pat: BindingElement,
    }
    pub struct FunctionParams {
        pub params: Vec<Param>,
        pub rest_param: Option<BindingRestElement>,
    }

    pub struct BindingIdent {
        pub id: Ident,
    }

    pub struct Ident {
        pub node_id: NodeId,
        pub name: NameId,
    }

    pub struct PrivateName {
        pub node_id: NodeId,
        pub id: Ident,
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
        pub value: BigUintValue,
    }
    pub struct Str {
        pub node_id: NodeId,
        pub value: Box<String>,
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
        pub raw: Box<String>,
    }
    pub struct Number {
        pub node_id: NodeId,
        pub value: f64,
    }
    pub enum Program {
        Module(Module),
        Script(Script),
    }
    pub struct Module {
        pub node_id: NodeId,
        pub body: Vec<ModuleItem>,
    }
    pub struct Script {
        pub node_id: NodeId,
        pub body: Vec<Stmt>,
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
    pub enum BindingPat {
        Array(ArrayBindingPat),
        Object(ObjectBindingPat),
    }
    pub struct ArrayBindingPat {
        pub node_id: NodeId,
        pub elems: Vec<Option<BindingElement>>,
        pub rest: Option<BindingRestElement>,
    }
    pub struct ObjectBindingPat {
        pub node_id: NodeId,
        pub props: Vec<BindingProperty>,
        pub rest: Option<BindingRestProperty>,
    }
    pub struct BindingProperty {
        pub node_id: NodeId,
        pub prop: PropName,
        pub target: Box<BindingElement>,
    }
    pub struct BindingRestProperty {
        pub node_id: NodeId,
        pub arg: Box<BindingIdent>,
    }
    pub struct BindingRestElement {
        pub node_id: NodeId,
        pub arg: Box<BindingPatOrIdent>,
    }
    pub struct BindingElement {
        pub node_id: NodeId,
        pub target: BindingPatOrIdent,
        pub init: Option<Box<Expr>>,
    }
    pub enum BindingPatOrIdent {
        Array(ArrayBindingPat),
        Object(ObjectBindingPat),
        Ident(BindingIdent),
    }
    pub enum AssignmentPat {
        Array(ArrayAssignmentPat),
        Object(ObjectAssignmentPat),
    }
    pub struct ArrayAssignmentPat {
        pub node_id: NodeId,
        pub elems: Vec<Option<AssignmentElement>>,
        pub rest: Option<AssignmentRest>,
    }
    pub struct ObjectAssignmentPat {
        pub node_id: NodeId,
        pub props: Vec<AssignmentProperty>,
        pub rest: Option<AssignmentRest>,
    }
    pub struct AssignmentProperty {
        pub node_id: NodeId,
        pub prop: PropName,
        pub target: AssignmentElement,
    }
    pub struct AssignmentRest {
        pub node_id: NodeId,
        pub arg: Box<AssignTarget>,
    }
    pub struct AssignmentElement {
        pub node_id: NodeId,
        pub target: Box<AssignTarget>,
        pub init: Option<Box<Expr>>,
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
        pub param: Param,
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
        Try(Box<TryStmt>),
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
        pub body: Box<BlockStmt>,
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
        pub cons: Box<BlockStmt>,
        pub alt: Option<Box<BlockStmt>>,
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
        pub tail: TryStmtTail,
    }
    pub enum TryStmtTail {
        Catch(CatchClause),
        Finally(Box<BlockStmt>),
        CatchFinally(CatchClause, Box<BlockStmt>),
    }
    pub struct WhileStmt {
        pub node_id: NodeId,
        pub test: Box<Expr>,
        pub body: Box<BlockStmt>,
    }
    pub struct DoWhileStmt {
        pub node_id: NodeId,
        pub test: Box<Expr>,
        pub body: Box<BlockStmt>,
    }
    pub struct ForStmt {
        pub node_id: NodeId,
        pub init: Option<Box<VarDeclOrExpr>>,
        pub test: Option<Box<Expr>>,
        pub update: Option<Box<Expr>>,
        pub body: Box<BlockStmt>,
    }
    pub struct ForInStmt {
        pub node_id: NodeId,
        pub left: Box<VarDeclOrAssignTarget>,
        pub right: Box<Expr>,
        pub body: Box<BlockStmt>,
    }
    pub struct ForOfStmt {
        pub node_id: NodeId,
        pub is_await: bool,
        pub left: Box<VarDeclOrAssignTarget>,
        pub right: Box<Expr>,
        pub body: Box<BlockStmt>,
    }
    pub struct SwitchCase {
        pub node_id: NodeId,
        pub test: Option<Box<Expr>>,
        pub cons: Vec<Stmt>,
    }
    pub struct CatchClause {
        pub node_id: NodeId,
        pub param: Option<BindingPatOrIdent>,
        pub body: BlockStmt,
    }
    pub enum VarDeclOrAssignTarget {
        VarDecl(VarDecl),
        AssignTarget(AssignTarget),
    }
    pub enum VarDeclOrExpr {
        VarDecl(VarDecl),
        Expr(Box<Expr>),
    }
});
