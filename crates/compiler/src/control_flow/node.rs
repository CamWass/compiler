use ast::{GetNodeId, NodeId};
use global_common::{Spanned, DUMMY_SP};
use std::fmt;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};

pub trait CfgNode: Copy + Eq + Hash + Debug {
    fn implicit_return() -> Self;
}

impl CfgNode for Node<'_> {
    fn implicit_return() -> Self {
        Node {
            node_id: None,
            kind: NodeKind::ImplicitReturn,
        }
    }
}

impl Node<'_> {
    pub fn is_function_like(&self) -> bool {
        matches!(
            self.kind,
            NodeKind::Function(_)
                | NodeKind::Constructor(_)
                | NodeKind::ArrowExpr(_)
                | NodeKind::GetterProp(_)
                | NodeKind::SetterProp(_)
        )
    }

    pub fn is_implicit_return(&self) -> bool {
        let r = self.node_id == None;
        debug_assert!(!r || matches!(self.kind, NodeKind::ImplicitReturn));
        r
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Node<'ast> {
    pub node_id: Option<NodeId>,
    pub kind: NodeKind<'ast>,
}

impl Node<'_> {
    pub const ImplicitReturn: Node<'_> = Node {
        node_id: None,
        kind: NodeKind::ImplicitReturn,
    };
}

impl PartialEq<Node<'_>> for Node<'_> {
    fn eq(&self, other: &Node<'_>) -> bool {
        self.node_id == other.node_id
    }
}

impl Eq for Node<'_> {}

impl Hash for Node<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.node_id.hash(state);
    }
}

impl<'ast> Node<'ast> {
    pub fn visit_with<V: ecma_visit::Visit<'ast>>(&self, v: &mut V) {
        self.kind.visit_with(v);
    }
}

impl Spanned for Node<'_> {
    fn span(&self) -> global_common::Span {
        self.kind.span()
    }
}

macro_rules! make {
    ($($field:ident,)*) => {
        #[derive(Copy, Clone)]
        pub enum NodeKind<'ast> {
            ImplicitReturn,
            $($field(&'ast ::ast::$field),)*
        }

        impl <'ast> fmt::Debug for NodeKind<'ast>  {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                 match self {
                    NodeKind::ImplicitReturn => f.write_str("ImplicitReturn"),
                    $(
                        NodeKind::$field(_) => f.write_fmt(format_args!("({}, Span({}..{}))", stringify!($field), self.span().lo.0, self.span().hi.0)),
                    )*
                }

            }
        }

        $(
            impl<'ast> From<&'ast ::ast::$field> for Node<'ast> {
                fn from(other: &'ast ::ast::$field) -> Node<'ast> {
                    Node {
                        node_id: Some(other.node_id()),
                        kind: NodeKind::$field(other)
                    }
                }
            }
        )*

        impl <'ast> NodeKind<'ast> {
            pub fn visit_with<V: ecma_visit::Visit<'ast>>(&self, v: &mut V) {
                use ecma_visit::VisitWith;
                match self {
                    NodeKind::ImplicitReturn=>{},
                    $(NodeKind::$field(n) => n.visit_with(v),)*
                }
            }
        }

        impl Spanned for NodeKind<'_> {
            fn span(&self) -> global_common::Span {
                match self {
                    NodeKind::ImplicitReturn=>DUMMY_SP,
                    $(NodeKind::$field(n) => n.span(),)*
                }
            }
        }

    };
}

make!(
    // class
    Class,
    ExtendsClause,
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

impl<'ast> From<&'ast ::ast::Expr> for Node<'ast> {
    fn from(other: &'ast ::ast::Expr) -> Node<'ast> {
        match other {
            ::ast::Expr::This(e) => Node::from(e),
            ::ast::Expr::Array(e) => Node::from(e),
            ::ast::Expr::Object(e) => Node::from(e),
            ::ast::Expr::Fn(e) => Node::from(e),
            ::ast::Expr::Unary(e) => Node::from(e),
            ::ast::Expr::Update(e) => Node::from(e),
            ::ast::Expr::Bin(e) => Node::from(e),
            ::ast::Expr::Assign(e) => Node::from(e),
            ::ast::Expr::Member(e) => Node::from(e),
            ::ast::Expr::Cond(e) => Node::from(e),
            ::ast::Expr::Call(e) => Node::from(e),
            ::ast::Expr::New(e) => Node::from(e),
            ::ast::Expr::Seq(e) => Node::from(e),
            ::ast::Expr::Ident(e) => Node::from(e),
            ::ast::Expr::Lit(e) => Node::from(e),
            ::ast::Expr::Tpl(e) => Node::from(e),
            ::ast::Expr::TaggedTpl(e) => Node::from(e),
            ::ast::Expr::Arrow(e) => Node::from(e),
            ::ast::Expr::Class(e) => Node::from(e),
            ::ast::Expr::Yield(e) => Node::from(e),
            ::ast::Expr::MetaProp(e) => Node::from(e),
            ::ast::Expr::Await(e) => Node::from(e),
            ::ast::Expr::Paren(e) => Node::from(e),
            ::ast::Expr::JSXMember(e) => Node::from(e),
            ::ast::Expr::JSXNamespacedName(e) => Node::from(e),
            ::ast::Expr::JSXEmpty(e) => Node::from(e),
            ::ast::Expr::JSXElement(e) => Node::from(e.as_ref()),
            ::ast::Expr::JSXFragment(e) => Node::from(e),
            ::ast::Expr::TsTypeAssertion(e) => Node::from(e),
            ::ast::Expr::TsConstAssertion(e) => Node::from(e),
            ::ast::Expr::TsNonNull(e) => Node::from(e),
            ::ast::Expr::TsAs(e) => Node::from(e),
            ::ast::Expr::PrivateName(e) => Node::from(e),
            ::ast::Expr::OptChain(e) => Node::from(e),
            ::ast::Expr::Invalid(e) => Node::from(e),
        }
    }
}

impl<'ast> From<&'ast ::ast::Lit> for Node<'ast> {
    fn from(other: &'ast ::ast::Lit) -> Node<'ast> {
        match other {
            ::ast::Lit::Str(l) => Node::from(l),
            ::ast::Lit::Bool(l) => Node::from(l),
            ::ast::Lit::Null(l) => Node::from(l),
            ::ast::Lit::Num(l) => Node::from(l),
            ::ast::Lit::BigInt(l) => Node::from(l),
            ::ast::Lit::Regex(l) => Node::from(l),
            ::ast::Lit::JSXText(l) => Node::from(l),
        }
    }
}

impl<'ast> From<&'ast ::ast::Stmt> for Node<'ast> {
    fn from(other: &'ast ::ast::Stmt) -> Node<'ast> {
        match other {
            ::ast::Stmt::Block(s) => Node::from(s),
            ::ast::Stmt::Empty(s) => Node::from(s),
            ::ast::Stmt::Debugger(s) => Node::from(s),
            ::ast::Stmt::With(s) => Node::from(s),
            ::ast::Stmt::Return(s) => Node::from(s),
            ::ast::Stmt::Labeled(s) => Node::from(s),
            ::ast::Stmt::Break(s) => Node::from(s),
            ::ast::Stmt::Continue(s) => Node::from(s),
            ::ast::Stmt::If(s) => Node::from(s),
            ::ast::Stmt::Switch(s) => Node::from(s),
            ::ast::Stmt::Throw(s) => Node::from(s),
            ::ast::Stmt::Try(s) => Node::from(s),
            ::ast::Stmt::While(s) => Node::from(s),
            ::ast::Stmt::DoWhile(s) => Node::from(s),
            ::ast::Stmt::For(s) => Node::from(s),
            ::ast::Stmt::ForIn(s) => Node::from(s),
            ::ast::Stmt::ForOf(s) => Node::from(s),
            ::ast::Stmt::Decl(s) => Node::from(s),
            ::ast::Stmt::Expr(s) => Node::from(s),
        }
    }
}

impl<'ast> From<&'ast ::ast::Decl> for Node<'ast> {
    fn from(other: &'ast ::ast::Decl) -> Node<'ast> {
        match other {
            ::ast::Decl::Class(d) => Node::from(d),
            ::ast::Decl::Fn(d) => Node::from(d),
            ::ast::Decl::Var(d) => Node::from(d),
            ::ast::Decl::TsInterface(d) => Node::from(d),
            ::ast::Decl::TsTypeAlias(d) => Node::from(d),
            ::ast::Decl::TsEnum(d) => Node::from(d),
            ::ast::Decl::TsModule(d) => Node::from(d),
        }
    }
}

impl<'ast> From<&'ast ::ast::Program> for Node<'ast> {
    fn from(other: &'ast ::ast::Program) -> Node<'ast> {
        match other {
            ::ast::Program::Module(p) => Node::from(p),
            ::ast::Program::Script(p) => Node::from(p),
        }
    }
}

impl<'ast> From<&'ast ::ast::VarDeclOrPat> for Node<'ast> {
    fn from(other: &'ast ::ast::VarDeclOrPat) -> Node<'ast> {
        match other {
            ::ast::VarDeclOrPat::VarDecl(n) => Node::from(n),
            ::ast::VarDeclOrPat::Pat(n) => Node::from(n),
        }
    }
}

impl<'ast> From<&'ast ::ast::Pat> for Node<'ast> {
    fn from(other: &'ast ::ast::Pat) -> Node<'ast> {
        match other {
            ::ast::Pat::Ident(p) => Node::from(p),
            ::ast::Pat::Array(p) => Node::from(p),
            ::ast::Pat::Rest(p) => Node::from(p),
            ::ast::Pat::Object(p) => Node::from(p),
            ::ast::Pat::Assign(p) => Node::from(p),
            ::ast::Pat::Invalid(p) => Node::from(p),
            ::ast::Pat::Expr(p) => Node::from(&**p),
        }
    }
}

impl<'ast> From<&'ast ::ast::ModuleItem> for Node<'ast> {
    fn from(other: &'ast ::ast::ModuleItem) -> Node<'ast> {
        match other {
            ::ast::ModuleItem::ModuleDecl(n) => Node::from(n),
            ::ast::ModuleItem::Stmt(n) => Node::from(n),
        }
    }
}

impl<'ast> From<&'ast ::ast::ModuleDecl> for Node<'ast> {
    fn from(other: &'ast ::ast::ModuleDecl) -> Node<'ast> {
        match other {
            ::ast::ModuleDecl::Import(n) => Node::from(n),
            ::ast::ModuleDecl::ExportDecl(n) => Node::from(n),
            ::ast::ModuleDecl::ExportNamed(n) => Node::from(n),
            ::ast::ModuleDecl::ExportDefaultDecl(n) => Node::from(n),
            ::ast::ModuleDecl::ExportDefaultExpr(n) => Node::from(n),
            ::ast::ModuleDecl::ExportAll(n) => Node::from(n),
            ::ast::ModuleDecl::TsImportEquals(n) => Node::from(n),
            ::ast::ModuleDecl::TsExportAssignment(n) => Node::from(n),
            ::ast::ModuleDecl::TsNamespaceExport(n) => Node::from(n),
        }
    }
}

impl<'ast> From<&'ast ::ast::BlockStmtOrExpr> for Node<'ast> {
    fn from(other: &'ast ::ast::BlockStmtOrExpr) -> Node<'ast> {
        match other {
            ::ast::BlockStmtOrExpr::BlockStmt(n) => Node::from(n),
            ::ast::BlockStmtOrExpr::Expr(n) => Node::from(&**n),
        }
    }
}

impl<'ast> From<&'ast ::ast::Prop> for Node<'ast> {
    fn from(other: &'ast ::ast::Prop) -> Node<'ast> {
        match other {
            ast::Prop::Shorthand(n) => unreachable!("removed by normalization"),
            ast::Prop::KeyValue(n) => Node::from(n),
            ast::Prop::Assign(n) => Node::from(n),
            ast::Prop::Getter(n) => Node::from(n),
            ast::Prop::Setter(n) => Node::from(n),
            ast::Prop::Method(n) => Node::from(n),
            ast::Prop::Spread(n) => Node::from(n),
        }
    }
}

impl<'ast> From<&'ast ::ast::ExprOrSpread> for Node<'ast> {
    fn from(other: &'ast ::ast::ExprOrSpread) -> Node<'ast> {
        match other {
            ::ast::ExprOrSpread::Spread(n) => Node::from(n),
            ::ast::ExprOrSpread::Expr(n) => Node::from(&**n),
        }
    }
}

impl<'ast> From<&'ast ::ast::ExprOrSuper> for Node<'ast> {
    fn from(other: &'ast ::ast::ExprOrSuper) -> Node<'ast> {
        match other {
            ::ast::ExprOrSuper::Super(n) => Node::from(n),
            ::ast::ExprOrSuper::Expr(n) => Node::from(&**n),
        }
    }
}

impl<'ast> From<&'ast ::ast::PatOrExpr> for Node<'ast> {
    fn from(other: &'ast ::ast::PatOrExpr) -> Node<'ast> {
        match other {
            ast::PatOrExpr::Expr(n) => Node::from(&**n),
            ast::PatOrExpr::Pat(n) => Node::from(&**n),
        }
    }
}

impl<'ast> From<&'ast ::ast::ObjectPatProp> for Node<'ast> {
    fn from(other: &'ast ::ast::ObjectPatProp) -> Node<'ast> {
        match other {
            ast::ObjectPatProp::KeyValue(n) => Node::from(n),
            ast::ObjectPatProp::Assign(n) => Node::from(n),
            ast::ObjectPatProp::Rest(n) => Node::from(n),
        }
    }
}
