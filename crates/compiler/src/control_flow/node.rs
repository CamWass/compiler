use global_common::{Spanned, DUMMY_SP};
use std::fmt;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::mem::discriminant;

pub trait CfgNode: Copy + Eq + Hash + Debug {
    fn implicit_return() -> Self;
}

impl CfgNode for Node<'_> {
    fn implicit_return() -> Self {
        Node::ImplicitReturn
    }
}

impl Node<'_> {
    pub fn is_function_like(&self) -> bool {
        matches!(
            self,
            Node::Function(_)
                | Node::Constructor(_)
                | Node::ArrowExpr(_)
                | Node::GetterProp(_)
                | Node::SetterProp(_)
        )
    }
}

macro_rules! make {
    ($($field:ident,)*) => {
        #[derive(Copy, Clone)]
        pub enum Node<'ast> {
            ImplicitReturn,
            $($field(&'ast ::ast::$field),)*
        }

        impl <'ast> fmt::Debug for Node<'ast>  {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                 match self {
                    Node::ImplicitReturn => f.write_str("ImplicitReturn"),
                    $(
                        Node::$field(_) => f.write_fmt(format_args!("({}, Span({}..{}))", stringify!($field), self.span().lo.0, self.span().hi.0)),
                    )*
                }

            }
        }

        impl <'ast> PartialEq for Node<'ast>  {
            fn eq(&self, other: &Self) -> bool {
                discriminant(self) == discriminant(other) && match (self, other) {
                    (Self::ImplicitReturn, Self::ImplicitReturn) => true,
                    $(
                        (Self::$field(l), Self::$field(r)) => l.node_id == r.node_id,
                    )*
                    _ => unreachable!()
                }
            }
        }

        impl <'ast> Eq for Node<'ast> {}

        impl <'ast> Hash for Node<'ast>  {
            fn hash<H: Hasher>(&self, state: &mut H) {
                discriminant(self).hash(state);
                match self {
                    Self::ImplicitReturn => {},
                    $(
                        Self::$field(l) => l.node_id.hash(state),
                    )*
                }
            }
        }

        $(
            impl<'ast> From<&'ast ::ast::$field> for Node<'ast> {
                fn from(other: &'ast ::ast::$field) -> Node<'ast> {
                    Node::$field(other)
                }
            }
        )*

        impl <'ast> Node<'ast> {
            pub fn visit_with<V: ecma_visit::Visit<'ast>>(&self, v: &mut V) {
                use ecma_visit::VisitWith;
                match self {
                    Node::ImplicitReturn=>{},
                    $(Node::$field(n) => n.visit_with(v),)*
                }
            }
        }

        impl Spanned for Node<'_> {
            fn span(&self) -> global_common::Span {
                match self {
                    Node::ImplicitReturn=>DUMMY_SP,
                    $(Node::$field(n) => n.span(),)*
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
            ::ast::Expr::This(e) => Node::ThisExpr(e),
            ::ast::Expr::Array(e) => Node::ArrayLit(e),
            ::ast::Expr::Object(e) => Node::ObjectLit(e),
            ::ast::Expr::Fn(e) => Node::FnExpr(e),
            ::ast::Expr::Unary(e) => Node::UnaryExpr(e),
            ::ast::Expr::Update(e) => Node::UpdateExpr(e),
            ::ast::Expr::Bin(e) => Node::BinExpr(e),
            ::ast::Expr::Assign(e) => Node::AssignExpr(e),
            ::ast::Expr::Member(e) => Node::MemberExpr(e),
            ::ast::Expr::Cond(e) => Node::CondExpr(e),
            ::ast::Expr::Call(e) => Node::CallExpr(e),
            ::ast::Expr::New(e) => Node::NewExpr(e),
            ::ast::Expr::Seq(e) => Node::SeqExpr(e),
            ::ast::Expr::Ident(e) => Node::Ident(e),
            ::ast::Expr::Lit(e) => Node::from(e),
            ::ast::Expr::Tpl(e) => Node::Tpl(e),
            ::ast::Expr::TaggedTpl(e) => Node::TaggedTpl(e),
            ::ast::Expr::Arrow(e) => Node::ArrowExpr(e),
            ::ast::Expr::Class(e) => Node::ClassExpr(e),
            ::ast::Expr::Yield(e) => Node::YieldExpr(e),
            ::ast::Expr::MetaProp(e) => Node::MetaPropExpr(e),
            ::ast::Expr::Await(e) => Node::AwaitExpr(e),
            ::ast::Expr::Paren(e) => Node::ParenExpr(e),
            ::ast::Expr::JSXMember(e) => Node::JSXMemberExpr(e),
            ::ast::Expr::JSXNamespacedName(e) => Node::JSXNamespacedName(e),
            ::ast::Expr::JSXEmpty(e) => Node::JSXEmptyExpr(e),
            ::ast::Expr::JSXElement(e) => Node::JSXElement(e),
            ::ast::Expr::JSXFragment(e) => Node::JSXFragment(e),
            ::ast::Expr::TsTypeAssertion(e) => Node::TsTypeAssertion(e),
            ::ast::Expr::TsConstAssertion(e) => Node::TsConstAssertion(e),
            ::ast::Expr::TsNonNull(e) => Node::TsNonNullExpr(e),
            ::ast::Expr::TsAs(e) => Node::TsAsExpr(e),
            ::ast::Expr::PrivateName(e) => Node::PrivateName(e),
            ::ast::Expr::OptChain(e) => Node::OptChainExpr(e),
            ::ast::Expr::Invalid(e) => Node::Invalid(e),
        }
    }
}

impl<'ast> From<&'ast ::ast::Lit> for Node<'ast> {
    fn from(other: &'ast ::ast::Lit) -> Node<'ast> {
        match other {
            ::ast::Lit::Str(l) => Node::Str(l),
            ::ast::Lit::Bool(l) => Node::Bool(l),
            ::ast::Lit::Null(l) => Node::Null(l),
            ::ast::Lit::Num(l) => Node::Number(l),
            ::ast::Lit::BigInt(l) => Node::BigInt(l),
            ::ast::Lit::Regex(l) => Node::Regex(l),
            ::ast::Lit::JSXText(l) => Node::JSXText(l),
        }
    }
}

impl<'ast> From<&'ast ::ast::Stmt> for Node<'ast> {
    fn from(other: &'ast ::ast::Stmt) -> Node<'ast> {
        match other {
            ::ast::Stmt::Block(s) => Node::BlockStmt(s),
            ::ast::Stmt::Empty(s) => Node::EmptyStmt(s),
            ::ast::Stmt::Debugger(s) => Node::DebuggerStmt(s),
            ::ast::Stmt::With(s) => Node::WithStmt(s),
            ::ast::Stmt::Return(s) => Node::ReturnStmt(s),
            ::ast::Stmt::Labeled(s) => Node::LabeledStmt(s),
            ::ast::Stmt::Break(s) => Node::BreakStmt(s),
            ::ast::Stmt::Continue(s) => Node::ContinueStmt(s),
            ::ast::Stmt::If(s) => Node::IfStmt(s),
            ::ast::Stmt::Switch(s) => Node::SwitchStmt(s),
            ::ast::Stmt::Throw(s) => Node::ThrowStmt(s),
            ::ast::Stmt::Try(s) => Node::TryStmt(s),
            ::ast::Stmt::While(s) => Node::WhileStmt(s),
            ::ast::Stmt::DoWhile(s) => Node::DoWhileStmt(s),
            ::ast::Stmt::For(s) => Node::ForStmt(s),
            ::ast::Stmt::ForIn(s) => Node::ForInStmt(s),
            ::ast::Stmt::ForOf(s) => Node::ForOfStmt(s),
            ::ast::Stmt::Decl(s) => Node::from(s),
            ::ast::Stmt::Expr(s) => Node::ExprStmt(s),
        }
    }
}

impl<'ast> From<&'ast ::ast::Decl> for Node<'ast> {
    fn from(other: &'ast ::ast::Decl) -> Node<'ast> {
        match other {
            ::ast::Decl::Class(d) => Node::ClassDecl(d),
            ::ast::Decl::Fn(d) => Node::FnDecl(d),
            ::ast::Decl::Var(d) => Node::VarDecl(d),
            ::ast::Decl::TsInterface(d) => Node::TsInterfaceDecl(d),
            ::ast::Decl::TsTypeAlias(d) => Node::TsTypeAliasDecl(d),
            ::ast::Decl::TsEnum(d) => Node::TsEnumDecl(d),
            ::ast::Decl::TsModule(d) => Node::TsModuleDecl(d),
        }
    }
}

impl<'ast> From<&'ast ::ast::Program> for Node<'ast> {
    fn from(other: &'ast ::ast::Program) -> Node<'ast> {
        match other {
            ::ast::Program::Module(p) => Node::Module(p),
            ::ast::Program::Script(p) => Node::Script(p),
        }
    }
}

impl<'ast> From<&'ast ::ast::VarDeclOrPat> for Node<'ast> {
    fn from(other: &'ast ::ast::VarDeclOrPat) -> Node<'ast> {
        match other {
            ::ast::VarDeclOrPat::VarDecl(n) => Node::VarDecl(n),
            ::ast::VarDeclOrPat::Pat(n) => Node::from(n),
        }
    }
}

impl<'ast> From<&'ast ::ast::Pat> for Node<'ast> {
    fn from(other: &'ast ::ast::Pat) -> Node<'ast> {
        match other {
            ::ast::Pat::Ident(p) => Node::BindingIdent(p),
            ::ast::Pat::Array(p) => Node::ArrayPat(p),
            ::ast::Pat::Rest(p) => Node::RestPat(p),
            ::ast::Pat::Object(p) => Node::ObjectPat(p),
            ::ast::Pat::Assign(p) => Node::AssignPat(p),
            ::ast::Pat::Invalid(p) => Node::Invalid(p),
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
            ::ast::ModuleDecl::Import(n) => Node::ImportDecl(n),
            ::ast::ModuleDecl::ExportDecl(n) => Node::ExportDecl(n),
            ::ast::ModuleDecl::ExportNamed(n) => Node::NamedExport(n),
            ::ast::ModuleDecl::ExportDefaultDecl(n) => Node::ExportDefaultDecl(n),
            ::ast::ModuleDecl::ExportDefaultExpr(n) => Node::ExportDefaultExpr(n),
            ::ast::ModuleDecl::ExportAll(n) => Node::ExportAll(n),
            ::ast::ModuleDecl::TsImportEquals(n) => Node::TsImportEqualsDecl(n),
            ::ast::ModuleDecl::TsExportAssignment(n) => Node::TsExportAssignment(n),
            ::ast::ModuleDecl::TsNamespaceExport(n) => Node::TsNamespaceExportDecl(n),
        }
    }
}

impl<'ast> From<&'ast ::ast::BlockStmtOrExpr> for Node<'ast> {
    fn from(other: &'ast ::ast::BlockStmtOrExpr) -> Node<'ast> {
        match other {
            ::ast::BlockStmtOrExpr::BlockStmt(n) => Node::BlockStmt(n),
            ::ast::BlockStmtOrExpr::Expr(n) => Node::from(&**n),
        }
    }
}

impl<'ast> From<&'ast ::ast::Prop> for Node<'ast> {
    fn from(other: &'ast ::ast::Prop) -> Node<'ast> {
        match other {
            ast::Prop::Shorthand(n) => unreachable!("removed by normalization"),
            ast::Prop::KeyValue(n) => Node::KeyValueProp(n),
            ast::Prop::Assign(n) => Node::AssignProp(n),
            ast::Prop::Getter(n) => Node::GetterProp(n),
            ast::Prop::Setter(n) => Node::SetterProp(n),
            ast::Prop::Method(n) => Node::MethodProp(n),
            ast::Prop::Spread(n) => Node::SpreadAssignment(n),
        }
    }
}

impl<'ast> From<&'ast ::ast::ExprOrSpread> for Node<'ast> {
    fn from(other: &'ast ::ast::ExprOrSpread) -> Node<'ast> {
        match other {
            ::ast::ExprOrSpread::Spread(n) => Node::SpreadElement(n),
            ::ast::ExprOrSpread::Expr(n) => Node::from(&**n),
        }
    }
}

impl<'ast> From<&'ast ::ast::ExprOrSuper> for Node<'ast> {
    fn from(other: &'ast ::ast::ExprOrSuper) -> Node<'ast> {
        match other {
            ::ast::ExprOrSuper::Super(n) => Node::Super(n),
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
            ast::ObjectPatProp::KeyValue(n) => Node::KeyValuePatProp(n),
            ast::ObjectPatProp::Assign(n) => Node::AssignPatProp(n),
            ast::ObjectPatProp::Rest(n) => Node::RestPat(n),
        }
    }
}
