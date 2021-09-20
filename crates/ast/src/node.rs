use crate::*;
use std::fmt;
use std::hash::Hash;
use global_common::Spanned;

macro_rules! make_enum {
    ($name:ident, [$($field:ident,)*]) => {
        #[derive(Copy, Clone, PartialEq, Eq, Hash)]
        pub enum $name<'ast> {
            $($field(&'ast $field),)*
        }

        $(
            impl<'ast> From<&'ast $field> for $name<'ast> {
                fn from(other: &'ast $field) -> $name<'ast> {
                    $name::$field(other)
                }
            }
        )*


        impl <'ast> fmt::Debug for $name<'ast>  {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                // let (name, span) = match self {
                //     $(
                //         $name::$field(a) => (stringify!($field), a.span()),
                //     )*
                // };
                // f.write_fmt(format_args!("({}, {:?})", name, span))

                 match self {
                    $(
                        $name::$field(_) => f.write_str(stringify!($field)),
                    )*
                }
                
            }
        }
    };
}

make_enum!(
    AstNode,
    [
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
        ExprOrSpread,
        OptChainExpr,
        // function
        Function,
        Param,
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
    ]
);

impl<'ast> From<&'ast Expr> for AstNode<'ast> {
    fn from(other: &'ast Expr) -> AstNode<'ast> {
        match other {
            Expr::This(e) => AstNode::ThisExpr(e),
            Expr::Array(e) => AstNode::ArrayLit(e),
            Expr::Object(e) => AstNode::ObjectLit(e),
            Expr::Fn(e) => AstNode::FnExpr(e),
            Expr::Unary(e) => AstNode::UnaryExpr(e),
            Expr::Update(e) => AstNode::UpdateExpr(e),
            Expr::Bin(e) => AstNode::BinExpr(e),
            Expr::Assign(e) => AstNode::AssignExpr(e),
            Expr::Member(e) => AstNode::MemberExpr(e),
            Expr::Cond(e) => AstNode::CondExpr(e),
            Expr::Call(e) => AstNode::CallExpr(e),
            Expr::New(e) => AstNode::NewExpr(e),
            Expr::Seq(e) => AstNode::SeqExpr(e),
            Expr::Ident(e) => AstNode::Ident(e),
            Expr::Lit(e) => AstNode::from(e),
            Expr::Tpl(e) => AstNode::Tpl(e),
            Expr::TaggedTpl(e) => AstNode::TaggedTpl(e),
            Expr::Arrow(e) => AstNode::ArrowExpr(e),
            Expr::Class(e) => AstNode::ClassExpr(e),
            Expr::Yield(e) => AstNode::YieldExpr(e),
            Expr::MetaProp(e) => AstNode::MetaPropExpr(e),
            Expr::Await(e) => AstNode::AwaitExpr(e),
            Expr::Paren(e) => AstNode::ParenExpr(e),
            Expr::JSXMember(e) => AstNode::JSXMemberExpr(e),
            Expr::JSXNamespacedName(e) => AstNode::JSXNamespacedName(e),
            Expr::JSXEmpty(e) => AstNode::JSXEmptyExpr(e),
            Expr::JSXElement(e) => AstNode::JSXElement(e),
            Expr::JSXFragment(e) => AstNode::JSXFragment(e),
            Expr::TsTypeAssertion(e) => AstNode::TsTypeAssertion(e),
            Expr::TsConstAssertion(e) => AstNode::TsConstAssertion(e),
            Expr::TsNonNull(e) => AstNode::TsNonNullExpr(e),
            Expr::TsAs(e) => AstNode::TsAsExpr(e),
            Expr::PrivateName(e) => AstNode::PrivateName(e),
            Expr::OptChain(e) => AstNode::OptChainExpr(e),
            Expr::Invalid(e) => AstNode::Invalid(e),
        }
    }
}

impl<'ast> From<&'ast Lit> for AstNode<'ast> {
    fn from(other: &'ast Lit) -> AstNode<'ast> {
        match other {
            Lit::Str(l) => AstNode::Str(l),
            Lit::Bool(l) => AstNode::Bool(l),
            Lit::Null(l) => AstNode::Null(l),
            Lit::Num(l) => AstNode::Number(l),
            Lit::BigInt(l) => AstNode::BigInt(l),
            Lit::Regex(l) => AstNode::Regex(l),
            Lit::JSXText(l) => AstNode::JSXText(l),
        }
    }
}

impl<'ast> From<&'ast Stmt> for AstNode<'ast> {
    fn from(other: &'ast Stmt) -> AstNode<'ast> {
        match other {
            Stmt::Block(s) => AstNode::BlockStmt(s),
            Stmt::Empty(s) => AstNode::EmptyStmt(s),
            Stmt::Debugger(s) => AstNode::DebuggerStmt(s),
            Stmt::With(s) => AstNode::WithStmt(s),
            Stmt::Return(s) => AstNode::ReturnStmt(s),
            Stmt::Labeled(s) => AstNode::LabeledStmt(s),
            Stmt::Break(s) => AstNode::BreakStmt(s),
            Stmt::Continue(s) => AstNode::ContinueStmt(s),
            Stmt::If(s) => AstNode::IfStmt(s),
            Stmt::Switch(s) => AstNode::SwitchStmt(s),
            Stmt::Throw(s) => AstNode::ThrowStmt(s),
            Stmt::Try(s) => AstNode::TryStmt(s),
            Stmt::While(s) => AstNode::WhileStmt(s),
            Stmt::DoWhile(s) => AstNode::DoWhileStmt(s),
            Stmt::For(s) => AstNode::ForStmt(s),
            Stmt::ForIn(s) => AstNode::ForInStmt(s),
            Stmt::ForOf(s) => AstNode::ForOfStmt(s),
            Stmt::Decl(s) => AstNode::from(s),
            Stmt::Expr(s) => AstNode::ExprStmt(s),
        }
    }
}

impl<'ast> From<&'ast Decl> for AstNode<'ast> {
    fn from(other: &'ast Decl) -> AstNode<'ast> {
        match other {
            Decl::Class(d) => AstNode::ClassDecl(d),
            Decl::Fn(d) => AstNode::FnDecl(d),
            Decl::Var(d) => AstNode::VarDecl(d),
            Decl::TsInterface(d) => AstNode::TsInterfaceDecl(d),
            Decl::TsTypeAlias(d) => AstNode::TsTypeAliasDecl(d),
            Decl::TsEnum(d) => AstNode::TsEnumDecl(d),
            Decl::TsModule(d) => AstNode::TsModuleDecl(d),
        }
    }
}

impl<'ast> From<&'ast Program> for AstNode<'ast> {
    fn from(other: &'ast Program) -> AstNode<'ast> {
        match other {
            Program::Module(p) => AstNode::Module(p),
            Program::Script(p) => AstNode::Script(p),
        }
    }
}

impl<'ast> From<&'ast VarDeclOrPat> for AstNode<'ast> {
    fn from(other: &'ast VarDeclOrPat) -> AstNode<'ast> {
        match other {
            VarDeclOrPat::VarDecl(n) => AstNode::VarDecl(n),
            VarDeclOrPat::Pat(n) => AstNode::from(n),
        }
    }
}

impl<'ast> From<&'ast Pat> for AstNode<'ast> {
    fn from(other: &'ast Pat) -> AstNode<'ast> {
        match other {
            Pat::Ident(p) => AstNode::BindingIdent(p),
            Pat::Array(p) => AstNode::ArrayPat(p),
            Pat::Rest(p) => AstNode::RestPat(p),
            Pat::Object(p) => AstNode::ObjectPat(p),
            Pat::Assign(p) => AstNode::AssignPat(p),
            Pat::Invalid(p) => AstNode::Invalid(p),
            Pat::Expr(p) => AstNode::from(&**p),
        }
    }
}

impl<'ast> From<&'ast ModuleItem> for AstNode<'ast> {
    fn from(other: &'ast ModuleItem) -> AstNode<'ast> {
        match other {
            ModuleItem::ModuleDecl(n) => AstNode::from(n),
            ModuleItem::Stmt(n) => AstNode::from(n),
        }
    }
}

impl<'ast> From<&'ast ModuleDecl> for AstNode<'ast> {
    fn from(other: &'ast ModuleDecl) -> AstNode<'ast> {
        match other {
            ModuleDecl::Import(n) => AstNode::ImportDecl(n),
            ModuleDecl::ExportDecl(n) => AstNode::ExportDecl(n),
            ModuleDecl::ExportNamed(n) => AstNode::NamedExport(n),
            ModuleDecl::ExportDefaultDecl(n) => AstNode::ExportDefaultDecl(n),
            ModuleDecl::ExportDefaultExpr(n) => AstNode::ExportDefaultExpr(n),
            ModuleDecl::ExportAll(n) => AstNode::ExportAll(n),
            ModuleDecl::TsImportEquals(n) => AstNode::TsImportEqualsDecl(n),
            ModuleDecl::TsExportAssignment(n) => AstNode::TsExportAssignment(n),
            ModuleDecl::TsNamespaceExport(n) => AstNode::TsNamespaceExportDecl(n),
        }
    }
}


