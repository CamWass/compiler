use global_common::Spanned;

use crate::ast;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

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

impl Bind for ast::PropOrSpread {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::PropOrSpread::Spread(n) => n.bind_to_opt_parent(parent),
            ast::PropOrSpread::Prop(n) => n.bind_to_opt_parent(parent),
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

impl Bind for ast::TsFnParam {
    fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
        match self {
            ast::TsFnParam::Ident(p) => p.bind_to_opt_parent(parent),
            ast::TsFnParam::Array(p) => p.bind_to_opt_parent(parent),
            ast::TsFnParam::Rest(p) => p.bind_to_opt_parent(parent),
            ast::TsFnParam::Object(p) => p.bind_to_opt_parent(parent),
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

// impl Bind for ast:: {
//     fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
//         match self {

//         }
//     }
// }

macro_rules! make_enum {
    ($name:ident, [$($field:ident,)*]) => {
        // Enum declaration:
        #[derive(PartialEq, Eq, Hash, Clone, Debug)]
        pub enum $name {
            $($field(Rc<$field>),)*
        }

        // Enum impls:

        // impl fmt::Debug for $name {
        //     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //         // let (name, span) = match self {
        //         //     $(
        //         //         $name::$field(a) => (stringify!($field), a.span()),
        //         //     )*
        //         // };
        //         // f.write_fmt(format_args!("({}, {:?})", name, span))

        //         // todo!();

        //          match self {
        //             $(
        //                 $name::$field(_) => f.write_str(stringify!($field)),
        //             )*
        //         }

        //     }
        // }

        impl $name {
            pub fn parent(&self) -> Option<Self> {
                match self {
                    $(
                        Self::$field(node) => {node.parent.clone()},
                    )*
                }
            }
        }

        // Structs and struct impls:
        $(
            impl Bind for Rc<ast::$field> {
                fn bind_to_opt_parent(&self, parent: Option<BoundNode>) -> BoundNode {
                    BoundNode::$field(Rc::new($field {
                        node: self.clone(),
                        parent: parent,
                    }))
                }
            }

            impl From<Rc<$field>> for $name {
                fn from(other: Rc<$field>) -> $name {
                    $name::$field(other)
                }
            }

            #[derive(PartialEq, Hash)]
            pub struct $field {
                pub node: Rc<ast::$field>,
                pub parent: Option<$name>,
            }

            impl Eq for $field {}

            impl Deref for $field {
                type Target = ast::$field;

                fn deref(&self) -> &Self::Target {
                    self.node.as_ref()
                }
            }

            impl fmt::Debug for $field {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str(stringify!($field))
                }
            }
        )*
    };
}

make_enum!(
    BoundNode,
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

impl BoundNode {
    pub fn span(&self) -> Option<global_common::Span> {
        match self {
            BoundNode::Class(n) => Some(n.span),
            BoundNode::ClassProp(n) => Some(n.span),
            BoundNode::PrivateProp(n) => Some(n.span),
            BoundNode::ClassMethod(n) => Some(n.span),
            BoundNode::PrivateMethod(n) => Some(n.span),
            BoundNode::Constructor(n) => Some(n.span),
            BoundNode::Decorator(n) => Some(n.span),
            BoundNode::FnDecl(_) => None,
            BoundNode::ClassDecl(_) => None,
            BoundNode::VarDecl(n) => Some(n.span),
            BoundNode::VarDeclarator(n) => Some(n.span),
            BoundNode::ThisExpr(n) => Some(n.span),
            BoundNode::ArrayLit(n) => Some(n.span),
            BoundNode::ObjectLit(n) => Some(n.span),
            BoundNode::SpreadElement(_) => None,
            BoundNode::UnaryExpr(n) => Some(n.span),
            BoundNode::UpdateExpr(n) => Some(n.span),
            BoundNode::BinExpr(n) => Some(n.span),
            BoundNode::FnExpr(_) => None,
            BoundNode::ClassExpr(_) => None,
            BoundNode::AssignExpr(n) => Some(n.span),
            BoundNode::MemberExpr(n) => Some(n.span),
            BoundNode::CondExpr(n) => Some(n.span),
            BoundNode::CallExpr(n) => Some(n.span),
            BoundNode::NewExpr(n) => Some(n.span),
            BoundNode::SeqExpr(n) => Some(n.span),
            BoundNode::ArrowExpr(n) => Some(n.span),
            BoundNode::YieldExpr(n) => Some(n.span),
            BoundNode::MetaPropExpr(_) => None,
            BoundNode::AwaitExpr(n) => Some(n.span),
            BoundNode::Tpl(n) => Some(n.span),
            BoundNode::TaggedTpl(n) => Some(n.span),
            BoundNode::TplElement(n) => Some(n.span),
            BoundNode::ParenExpr(n) => Some(n.span),
            BoundNode::Super(n) => Some(n.span),
            BoundNode::ExprOrSpread(_) => None,
            BoundNode::OptChainExpr(n) => Some(n.span),
            BoundNode::Function(n) => Some(n.span),
            BoundNode::Param(n) => Some(n.span),
            BoundNode::BindingIdent(_) => None,
            BoundNode::Ident(n) => Some(n.span),
            BoundNode::PrivateName(n) => Some(n.span),
            BoundNode::JSXMemberExpr(_) => None,
            BoundNode::JSXNamespacedName(_) => None,
            BoundNode::JSXEmptyExpr(n) => Some(n.span),
            BoundNode::JSXExprContainer(n) => Some(n.span),
            BoundNode::JSXSpreadChild(n) => Some(n.span),
            BoundNode::JSXOpeningElement(n) => Some(n.span),
            BoundNode::JSXClosingElement(n) => Some(n.span),
            BoundNode::JSXAttr(n) => Some(n.span),
            BoundNode::JSXText(n) => Some(n.span),
            BoundNode::JSXElement(n) => Some(n.span),
            BoundNode::JSXFragment(n) => Some(n.span),
            BoundNode::JSXOpeningFragment(n) => Some(n.span),
            BoundNode::JSXClosingFragment(n) => Some(n.span),
            BoundNode::Invalid(n) => Some(n.span),
            BoundNode::Str(n) => Some(n.span),
            BoundNode::Bool(n) => Some(n.span),
            BoundNode::Null(n) => Some(n.span),
            BoundNode::Number(n) => Some(n.span),
            BoundNode::BigInt(n) => Some(n.span),
            BoundNode::Regex(n) => Some(n.span),
            BoundNode::ExportDefaultExpr(n) => Some(n.span),
            BoundNode::ExportDecl(n) => Some(n.span),
            BoundNode::ImportDecl(n) => Some(n.span),
            BoundNode::ExportAll(n) => Some(n.span),
            BoundNode::NamedExport(n) => Some(n.span),
            BoundNode::ExportDefaultDecl(n) => Some(n.span),
            BoundNode::ImportDefaultSpecifier(n) => Some(n.span),
            BoundNode::ImportStarAsSpecifier(n) => Some(n.span),
            BoundNode::ImportNamedSpecifier(n) => Some(n.span),
            BoundNode::ExportNamespaceSpecifier(n) => Some(n.span),
            BoundNode::ExportDefaultSpecifier(_) => None,
            BoundNode::ExportNamedSpecifier(n) => Some(n.span),
            BoundNode::Script(n) => Some(n.span),
            BoundNode::Module(n) => Some(n.span),
            BoundNode::ArrayPat(n) => Some(n.span),
            BoundNode::ObjectPat(n) => Some(n.span),
            BoundNode::AssignPat(n) => Some(n.span),
            BoundNode::RestPat(n) => Some(n.span),
            BoundNode::KeyValuePatProp(_) => None,
            BoundNode::AssignPatProp(n) => Some(n.span),
            BoundNode::KeyValueProp(_) => None,
            BoundNode::AssignProp(_) => None,
            BoundNode::GetterProp(n) => Some(n.span),
            BoundNode::SetterProp(n) => Some(n.span),
            BoundNode::MethodProp(_) => None,
            BoundNode::ComputedPropName(n) => Some(n.span),
            BoundNode::BlockStmt(n) => Some(n.span),
            BoundNode::ExprStmt(n) => Some(n.span),
            BoundNode::EmptyStmt(n) => Some(n.span),
            BoundNode::DebuggerStmt(n) => Some(n.span),
            BoundNode::WithStmt(n) => Some(n.span),
            BoundNode::ReturnStmt(n) => Some(n.span),
            BoundNode::LabeledStmt(n) => Some(n.span),
            BoundNode::BreakStmt(n) => Some(n.span),
            BoundNode::ContinueStmt(n) => Some(n.span),
            BoundNode::IfStmt(n) => Some(n.span),
            BoundNode::SwitchStmt(n) => Some(n.span),
            BoundNode::ThrowStmt(n) => Some(n.span),
            BoundNode::TryStmt(n) => Some(n.span),
            BoundNode::WhileStmt(n) => Some(n.span),
            BoundNode::DoWhileStmt(n) => Some(n.span),
            BoundNode::ForStmt(n) => Some(n.span),
            BoundNode::ForInStmt(n) => Some(n.span),
            BoundNode::ForOfStmt(n) => Some(n.span),
            BoundNode::SwitchCase(n) => Some(n.span),
            BoundNode::CatchClause(n) => Some(n.span),
            BoundNode::TsTypeAnn(n) => Some(n.span),
            BoundNode::TsTypeParamDecl(n) => Some(n.span),
            BoundNode::TsTypeParam(n) => Some(n.span),
            BoundNode::TsTypeParamInstantiation(n) => Some(n.span),
            BoundNode::TsParamProp(n) => Some(n.span),
            BoundNode::TsQualifiedName(_) => None,
            BoundNode::TsCallSignatureDecl(n) => Some(n.span),
            BoundNode::TsConstructSignatureDecl(n) => Some(n.span),
            BoundNode::TsPropertySignature(n) => Some(n.span),
            BoundNode::TsGetterSignature(n) => Some(n.span),
            BoundNode::TsSetterSignature(n) => Some(n.span),
            BoundNode::TsMethodSignature(n) => Some(n.span),
            BoundNode::TsIndexSignature(n) => Some(n.span),
            BoundNode::TsKeywordType(n) => Some(n.span),
            BoundNode::TsThisType(n) => Some(n.span),
            BoundNode::TsFnType(n) => Some(n.span),
            BoundNode::TsConstructorType(n) => Some(n.span),
            BoundNode::TsTypeRef(n) => Some(n.span),
            BoundNode::TsTypePredicate(n) => Some(n.span),
            BoundNode::TsTypeQuery(n) => Some(n.span),
            BoundNode::TsImportType(n) => Some(n.span),
            BoundNode::TsTypeLit(n) => Some(n.span),
            BoundNode::TsArrayType(n) => Some(n.span),
            BoundNode::TsTupleType(n) => Some(n.span),
            BoundNode::TsTupleElement(n) => Some(n.span),
            BoundNode::TsOptionalType(n) => Some(n.span),
            BoundNode::TsRestType(n) => Some(n.span),
            BoundNode::TsUnionType(n) => Some(n.span),
            BoundNode::TsIntersectionType(n) => Some(n.span),
            BoundNode::TsConditionalType(n) => Some(n.span),
            BoundNode::TsInferType(n) => Some(n.span),
            BoundNode::TsParenthesizedType(n) => Some(n.span),
            BoundNode::TsTypeOperator(n) => Some(n.span),
            BoundNode::TsIndexedAccessType(n) => Some(n.span),
            BoundNode::TsMappedType(n) => Some(n.span),
            BoundNode::TsLitType(n) => Some(n.span),
            BoundNode::TsTplLitType(n) => Some(n.span),
            BoundNode::TsInterfaceDecl(n) => Some(n.span),
            BoundNode::TsInterfaceBody(n) => Some(n.span),
            BoundNode::TsExprWithTypeArgs(n) => Some(n.span),
            BoundNode::TsTypeAliasDecl(n) => Some(n.span),
            BoundNode::TsEnumDecl(n) => Some(n.span),
            BoundNode::TsEnumMember(n) => Some(n.span),
            BoundNode::TsModuleDecl(n) => Some(n.span),
            BoundNode::TsModuleBlock(n) => Some(n.span),
            BoundNode::TsNamespaceDecl(n) => Some(n.span),
            BoundNode::TsImportEqualsDecl(n) => Some(n.span),
            BoundNode::TsExternalModuleRef(n) => Some(n.span),
            BoundNode::TsExportAssignment(n) => Some(n.span),
            BoundNode::TsNamespaceExportDecl(n) => Some(n.span),
            BoundNode::TsAsExpr(n) => Some(n.span),
            BoundNode::TsTypeAssertion(n) => Some(n.span),
            BoundNode::TsNonNullExpr(n) => Some(n.span),
            BoundNode::TsConstAssertion(n) => Some(n.span),
        }
    }
}
