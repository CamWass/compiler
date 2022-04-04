use crate::ast::{self, Expr, ExprOrSuper};
use crate::node::*;
use crate::types::*;
use crate::Visit;
use bitflags::bitflags;
use global_common::EqIgnoreSpan;
use std::mem;
use std::rc::Rc;
use swc_atoms::{js_word, JsWord};

macro_rules! unwrap_as {
    ($expr:expr, $pat:pat, $res:expr) => {
        match $expr {
            $pat => $res,
            _ => unreachable!(),
        }
    };
}

pub(crate) use unwrap_as;

/// Creates a new `AHashMap` and optionally inserts initial entries.
/// This macro exists because `AHashMap` does not `impl From<[(K, V); N]>` like std's `HashMap` does.
#[macro_export]
macro_rules! new_ahash_map {
    [$( ($key:expr, $value:expr $(,)?)$(,)? )*] => {{
        std::array::IntoIter::new([$( ($key, $value), )*]).collect::<AHashMap<_,_>>()
    }};
}

pub use new_ahash_map;

#[macro_export]
macro_rules! hash {
    [$( $val:expr$(,)? )*] => {{
        let mut s = ahash::AHasher::default();
        $(std::hash::Hash::hash(&$val, &mut s);)*
        std::hash::Hasher::finish(&s)
    }}
}

pub use hash;

pub fn getSourceFileOfNode(node: BoundNode) -> BoundNode {
    findAncestor(Some(node), |n| {
        Some(matches!(n, BoundNode::Script(_) | BoundNode::Module(_)))
    })
    .unwrap()
}

/// Iterates through the parent chain of a node and performs the callback on each parent until the callback
/// returns `Some(true)`, then returns that value.
/// If no such value is found, it applies the callback until the parent pointer is `None` or the callback returns `None`
/// At that point findAncestor returns `None`.
pub fn findAncestor<C>(mut node: Option<BoundNode>, mut callback: C) -> Option<BoundNode>
where
    C: FnMut(&BoundNode) -> Option<bool>,
{
    while let Some(n) = node {
        let result = callback(&n);
        if let Some(result) = result {
            if result {
                return Some(n);
            }
        } else {
            return None;
        }
        node = n.parent();
    }
    None
}

pub fn isFunctionLike(node: Option<&BoundNode>) -> bool {
    match node {
        Some(node) => isFunctionLikeKind(node),
        None => false,
    }
}

fn isFunctionLikeKind(node: &BoundNode) -> bool {
    match node {
        // | BoundNode::JSDocSignature(_)
        // | BoundNode::JSDocFunctionType(_)
        BoundNode::TsMethodSignature(_)
        | BoundNode::TsCallSignatureDecl(_)
        | BoundNode::TsConstructSignatureDecl(_)
        | BoundNode::TsIndexSignature(_)
        | BoundNode::TsFnType(_)
        | BoundNode::TsConstructorType(_) => true,
        _ => isFunctionLikeDeclarationKind(node),
    }
}

fn isFunctionLikeDeclarationKind(node: &BoundNode) -> bool {
    // TODO: get/set accessors
    matches!(
        node,
        BoundNode::FnDecl(_)
            | BoundNode::Constructor(_)
            | BoundNode::ClassMethod(_)
            | BoundNode::PrivateMethod(_)
            | BoundNode::FnExpr(_)
            | BoundNode::ArrowExpr(_)
    )
}

pub fn isObjectLiteralOrClassExpressionMethodOrAccessor(node: &BoundNode) -> bool {
    matches!(
        node,
        BoundNode::ClassMethod(_)
            | BoundNode::PrivateMethod(_)
            | BoundNode::GetterProp(_)
            | BoundNode::SetterProp(_)
            | BoundNode::MethodProp(_)
    )
}

pub fn getImmediatelyInvokedFunctionExpression(func: BoundNode) -> Option<Rc<CallExpr>> {
    if matches!(func, BoundNode::FnExpr(_) | BoundNode::ArrowExpr(_)) {
        let mut prev = func.clone();
        let mut parent = func.parent();
        while let Some(p @ BoundNode::ParenExpr(_)) = parent {
            prev = p.clone();
            parent = p.parent();
        }
        if let Some(parent @ BoundNode::CallExpr(c)) = &parent {
            if c.callee.bind(parent.clone()) == prev {
                return Some(c.clone());
            }
        }
    }
    None
}

/**
 * Determines whether a node is a property or element access expression for `this`.
 */
pub fn isThisProperty(node: &BoundNode) -> bool {
    match node {
        BoundNode::MemberExpr(m) => matches!(m.obj, ExprOrSuper::Expr(Expr::This(_))),
        _ => false,
    }
}

bitflags! {
    pub struct OuterExpressionKinds: u8 {
        const Parentheses = 1 << 0;
        const TypeAssertions = 1 << 1;
        const NonNullAssertions = 1 << 2;
        // const PartiallyEmittedExpressions = 1 << 3;

        const Assertions = Self::TypeAssertions.bits | Self::NonNullAssertions.bits;
        const All = Self::Parentheses.bits | Self::Assertions.bits/* | Self::PartiallyEmittedExpressions.bits*/;

        const ExcludeJSDocTypeAssertion = 1 << 4;
    }
}

// fn isOuterExpression(node: BoundNode, kinds: OuterExpressionKinds) -> bool {
//     todo!();
//     // match node {
//     //     BoundNode::ParenExpr(_) => {
//     //         if (kinds & OuterExpressionKinds.ExcludeJSDocTypeAssertion
//     //             && isJSDocTypeAssertion(node))
//     //         {
//     //             return false;
//     //         }
//     //         return (kinds & OuterExpressionKinds.Parentheses) != 0;
//     //     }
//     //     BoundNode::TsTypeAssertion(_) | BoundNode::TsAsExpr(_) => {
//     //         return (kinds & OuterExpressionKinds.TypeAssertions) != 0;
//     //     }
//     //     BoundNode::TsNonNullExpr(_) => {
//     //         return (kinds & OuterExpressionKinds.NonNullAssertions) != 0;
//     //     }
//     // }
//     // return false;
// }

// fn skipOuterExpressions(mut node: BoundNode, kinds: OuterExpressionKinds) -> BoundNode {
//     todo!();
//     // while isOuterExpression(node, kinds) {
//     //     node = node.expression;
//     // }
//     // node
// }

pub fn skipParenthesesOfNode(mut node: BoundNode) -> BoundNode {
    while let BoundNode::ParenExpr(p) = &node {
        node = p.expr.bind(node.clone());
    }
    node
}

pub fn skipParenthesesOfExpr(mut expr: &Expr) -> &Expr {
    while let Expr::Paren(p) = expr {
        expr = &p.expr;
    }
    expr
}

// a node is delete target iff. it is MemberExpr with parentheses skipped
pub fn isDeleteTarget(mut node: BoundNode) -> bool {
    if matches!(node, BoundNode::MemberExpr(_)) {
        return false;
    }
    node = walkUpParenthesizedExpressions(node.parent().unwrap());
    matches!(node, BoundNode::UnaryExpr(e) if e.op == ast::UnaryOp::Delete)
}

/// True if `name` is the name of a declaration node
pub fn isDeclarationName(name: BoundNode) -> bool {
    if !matches!(name, BoundNode::Script(_) | BoundNode::Module(_)) {
        if !isBindingPattern(&name) {
            if let Some(parent) = name.parent() {
                if let BoundNode::TsTypeParamDecl(_) = parent {
                    todo!();
                    // return (node.parent && node.parent.kind !== SyntaxKind.JSDocTemplateTag) || isInJSFile(node);
                }
                // Shorthand property. e.g. `a` in `{ a, }`.
                if let BoundNode::Ident(_) = name {
                    if let BoundNode::ObjectLit(_) = parent {
                        return true;
                    }
                }
                return match &parent {
                    // TODO:
                    // BoundNode::ClassStaticBlockDeclaration(_)|
                    // BoundNode::ExportSpecifier(_)|
                    // BoundNode::ImportClause(_)|
                    // BoundNode::ImportEqualsDeclaration(_)|
                    // BoundNode::ImportSpecifier(_)|
                    // BoundNode::JsxAttribute(_)|
                    // BoundNode::NamespaceExportDeclaration(_)|
                    // BoundNode::NamespaceImport(_)|
                    // BoundNode::NamespaceExport(_)|
                    // BoundNode::JSDocTypedefTag(_)|
                    // BoundNode::JSDocCallbackTag(_)|
                    // BoundNode::JSDocPropertyTag(_)|
                    // BoundNode::TsTypeParamDecl(_)|
                    // BoundNode::BindingIdent(n) => n.id.bind(parent.clone()) == name,
                    BoundNode::RestPat(n) => n.arg.bind(parent.clone()) == name,
                    BoundNode::AssignPat(n) => n.left.bind(parent.clone()) == name,
                    BoundNode::ClassDecl(n) => n.ident.bind(parent.clone()) == name,
                    BoundNode::ClassExpr(n) => {
                        n.ident.as_ref().map(|n| n.bind(parent.clone())) == Some(name)
                    }
                    BoundNode::TsEnumDecl(n) => n.id.bind(parent.clone()) == name,
                    BoundNode::TsEnumMember(n) => n.id.bind(parent.clone()) == name,
                    BoundNode::FnDecl(n) => n.ident.bind(parent.clone()) == name,
                    BoundNode::FnExpr(n) => {
                        n.ident.as_ref().map(|n| n.bind(parent.clone())) == Some(name)
                    }
                    BoundNode::GetterProp(n) => n.key.bind(parent.clone()) == name,
                    BoundNode::TsInterfaceDecl(n) => n.id.bind(parent.clone()) == name,
                    BoundNode::PrivateMethod(n) => n.key.bind(parent.clone()) == name,
                    BoundNode::ClassMethod(n) => n.key.bind(parent.clone()) == name,
                    BoundNode::MethodProp(n) => n.key.bind(parent.clone()) == name,
                    BoundNode::TsMethodSignature(n) => n.key.bind(parent.clone()) == name,
                    BoundNode::TsModuleDecl(n) => n.id.bind(parent.clone()) == name,
                    BoundNode::Param(n) => n.pat.bind(parent.clone()) == name,
                    BoundNode::ParamWithoutDecorators(n) => n.pat.bind(parent.clone()) == name,
                    BoundNode::TsAmbientParam(n) => n.pat.bind(parent.clone()) == name,
                    BoundNode::TsParamProp(n) => n.param.bind(parent.clone()) == name,
                    BoundNode::KeyValueProp(n) => n.key.bind(parent.clone()) == name,
                    BoundNode::ClassProp(n) => n.key.bind(parent.clone()) == name,
                    BoundNode::PrivateProp(n) => n.key.bind(parent.clone()) == name,
                    BoundNode::TsPropertySignature(n) => n.key.bind(parent.clone()) == name,
                    BoundNode::SetterProp(n) => n.key.bind(parent.clone()) == name,
                    BoundNode::TsTypeAliasDecl(n) => n.id.bind(parent.clone()) == name,
                    BoundNode::VarDeclarator(n) => n.name.bind(parent.clone()) == name,
                    _ => false,
                };
            }
        }
    }
    false
}

pub fn isLiteralComputedPropertyDeclarationName(node: BoundNode) -> bool {
    if let Some(parent) = node.parent() {
        if let BoundNode::ComputedPropName(p) = &parent {
            if isStringOrNumericLiteralLike(&p.expr) {
                return isDeclaration(&parent.parent().unwrap());
            }
        }
    }
    false
}

pub fn isLogicalOrCoalescingAssignmentOperator(op: ast::AssignOp) -> bool {
    matches!(
        op,
        ast::AssignOp::OrAssign | ast::AssignOp::AndAssign | ast::AssignOp::NullishAssign
    )
}

pub fn isDottedName(node: &ExprOrSuper) -> bool {
    match node {
        ExprOrSuper::Expr(expr) => match expr {
            Expr::Ident(_) | Expr::This(_) | Expr::MetaProp(_) => true,
            Expr::Member(e) => isDottedName(&e.obj),
            Expr::Paren(e) if isDottedName(&e.expr.clone().into()) => true,
            _ => false,
        },
        ExprOrSuper::Super(_) => true,
    }
}

fn isStringLiteralLike(expr: &Expr) -> bool {
    match expr {
        Expr::Lit(ast::Lit::Str(_)) => true,
        Expr::Tpl(tpl) => tpl.exprs.is_empty(),
        _ => false,
    }
}

pub fn isParameterDeclaration(node: BoundNode) -> bool {
    // todo: debug assert that node is "VariableLikeDeclaration" (from tsc).
    let root = getRootDeclaration(node);
    matches!(
        root,
        BoundNode::Param(_)
            | BoundNode::ParamWithoutDecorators(_)
            | BoundNode::TsAmbientParam(_)
            | BoundNode::TsParamProp(_)
    )
}

pub fn getRootDeclaration(mut node: BoundNode) -> BoundNode {
    while matches!(
        node,
        BoundNode::BindingIdent(_)
            | BoundNode::ArrayPat(_)
            | BoundNode::RestPat(_)
            | BoundNode::ObjectPat(_)
            | BoundNode::AssignPat(_)
            | BoundNode::KeyValuePatProp(_)
            | BoundNode::AssignPatProp(_)
    ) {
        match node.parent() {
            Some(n) => node = n,
            None => unreachable!("all of the matched nodes are guaranteed to have a parent"),
        }
    }

    node
}

pub fn setValueDeclaration(symbol: &mut Symbol, node: BoundNode) {
    if let Some(valueDeclaration) = &symbol.valueDeclaration() {
        // TODO:
        if
        /* !(node.flags.intersects(NodeFlags::Ambient)
        && !(valueDeclaration.flags.intersects(NodeFlags::Ambient)))
        && */
        (isAssignmentDeclaration(valueDeclaration) && !isAssignmentDeclaration(&node))
            || (mem::discriminant(valueDeclaration) != mem::discriminant(&node)
                && isEffectiveModuleDeclaration(valueDeclaration))
        {
            // other kinds of value declarations take precedence over modules and assignment declarations
            *symbol.valueDeclaration_mut() = Some(node);
        }
    } else {
        *symbol.valueDeclaration_mut() = Some(node);
    }
}

pub trait PushIfUnique<T>
where
    T: PartialEq,
{
    /// Returns whether the value was added.
    fn push_if_unique(&mut self, elem: T) -> bool;
}

impl<T> PushIfUnique<T> for Vec<T>
where
    T: PartialEq,
{
    fn push_if_unique(&mut self, value: T) -> bool {
        let is_present = self.iter().any(|v| v == &value);
        if is_present {
            false
        } else {
            self.push(value);
            true
        }
    }
}

/// Unlike `PushIfUnique`, this can take `None` as an input, and returns a new vec.
pub fn appendIfUnique<T>(vec: Option<Vec<T>>, toAdd: T) -> Vec<T>
where
    T: PartialEq,
{
    if let Some(mut vec) = vec {
        vec.push_if_unique(toAdd);
        vec
    } else {
        vec![toAdd]
    }
}

pub fn isStatic(node: &BoundNode) -> bool {
    // https://tc39.es/ecma262/#sec-static-semantics-isstatic
    // TODO: static blocks
    // isClassElement(node) && hasStaticModifier(node) || isClassStaticBlockDeclaration(node)
    match node {
        BoundNode::Constructor(_) => true,
        BoundNode::ClassMethod(m) => m.is_static,
        BoundNode::PrivateMethod(m) => m.is_static,
        BoundNode::ClassProp(p) => p.is_static,
        BoundNode::PrivateProp(p) => p.is_static,
        BoundNode::TsIndexSignature(i) => i.is_static,
        _ => false,
    }
}

#[derive(Debug)]
pub enum DeclName {
    Ident(Rc<ast::Ident>),
    BindingIdent(Rc<ast::BindingIdent>),
    PrivateName(Rc<ast::PrivateName>),
    String(Rc<ast::Str>),
    NoSubstitutionTemplate(Rc<ast::Tpl>),
    Number(Rc<ast::Number>),
    ComputedProperty(Expr),
    ElementAccessExpression(Expr),
    ArrayPt(Rc<ast::ArrayPat>),
    ObjectPat(Rc<ast::ObjectPat>),
    EntityNameExpression(ast::TsEntityName),
}

fn getNonAssignedNameOfDeclaration(declaration: &BoundNode) -> Option<DeclName> {
    // TODO: remove varient matches that map to None (e.g. TsIndexSignature's).
    // These are only here to prevent the 'unknown node' message during
    // development and can be removed in favour of the default branch, which
    // also returns None.
    match declaration {
        BoundNode::Ident(i) => Some(DeclName::Ident(i.node.clone())),
        // TODO: JSDOC:
        // BoundNode::JSDocPropertyTag(_)|
        // BoundNode::JSDocParameterTag(_)=> {
        //     const { name } = declaration as JSDocPropertyLikeTag;
        //     if (name.kind === SyntaxKind.QualifiedName) {
        //         return name.right;
        //     }
        //     break;
        // }
        BoundNode::CallExpr(_) => {
            todo!();
            // let arg = call.arg;
            // match getAssignmentDeclarationKind(arg) {
            //     AssignmentDeclarationKind::ExportsProperty
            //     | AssignmentDeclarationKind::ThisProperty
            //     | AssignmentDeclarationKind::Property
            //     | AssignmentDeclarationKind::PrototypeProperty => {
            //         return getElementOrPropertyAccessArgumentExpressionOrName(
            //             (arg as BinaryExpression).left as AccessExpression,
            //         );
            //     }
            //     AssignmentDeclarationKind::ObjectDefinePropertyValue
            //     | AssignmentDeclarationKind::ObjectDefinePropertyExports
            //     | AssignmentDeclarationKind::ObjectDefinePrototypeProperty => {
            //         return (arg as BindableObjectDefinePropertyCall).arguments[1];
            //     }
            //     _ => return None,
            // }
        }
        BoundNode::BinExpr(_) => {
            todo!();
            // const expr = declaration as BinaryExpression;
            // switch (getAssignmentDeclarationKind(expr)) {
            //     case AssignmentDeclarationKind.ExportsProperty:
            //     case AssignmentDeclarationKind.ThisProperty:
            //     case AssignmentDeclarationKind.Property:
            //     case AssignmentDeclarationKind.PrototypeProperty:
            //         return getElementOrPropertyAccessArgumentExpressionOrName((expr as BinaryExpression).left as AccessExpression);
            //     case AssignmentDeclarationKind.ObjectDefinePropertyValue:
            //     case AssignmentDeclarationKind.ObjectDefinePropertyExports:
            //     case AssignmentDeclarationKind.ObjectDefinePrototypeProperty:
            //         return (expr as BindableObjectDefinePropertyCall).arguments[1];
            //     default:
            //         return undefined;
            // }
        }
        // TODO: JSDOC
        // BoundNode::JSDocTypedefTag(_) => {
        //     return getNameOfJSDocTypedef(declaration as JSDocTypedefTag);
        // }
        // BoundNode::JSDocEnumTag(_) => {
        //     return nameForNamelessJSDocTypedef(declaration as JSDocEnumTag);
        // }
        BoundNode::TsExportAssignment(e) => {
            if let Expr::Ident(i) = &e.expr {
                Some(DeclName::Ident(i.clone()))
            } else {
                None
            }
        }
        // TODO:
        // BoundNode::ElementAccessExpression(_) => {
        //     let expr = declaration as ElementAccessExpression;
        //     if (isBindableStaticElementAccessExpression(expr)) {
        //         return expr.argumentExpression;
        //     }
        // }

        // TODO:
        // MissingDeclaration
        BoundNode::ClassDecl(decl) => Some(DeclName::Ident(decl.ident.clone())),
        // TODO:
        // InterfaceDeclaration
        BoundNode::TsInterfaceDecl(i) => Some(DeclName::Ident(i.id.clone())),

        // TODO:
        // EnumDeclaration
        // ModuleDeclaration
        // ImportEqualsDeclaration
        // NamespaceExportDeclaration
        // ExportDeclaration
        // ExportAssignment

        // TypeParameterDeclaration

        // CallSignatureDeclaration
        // ConstructSignatureDeclaration
        // FunctionLikeDeclarationBase
        // MethodSignature
        // IndexSignatureDeclaration
        // FunctionOrConstructorTypeNodeBase
        // JSDocFunctionType
        BoundNode::VarDeclarator(d) => match &d.name {
            ast::Pat::Ident(i) => Some(DeclName::BindingIdent(i.clone())),
            ast::Pat::Array(_) => todo!(),
            ast::Pat::Object(_) => todo!(),
            _ => unreachable!(),
        },
        BoundNode::Param(p) => match &p.pat {
            ast::Pat::Ident(i) => Some(DeclName::BindingIdent(i.clone())),
            _ => todo!(),
        },
        BoundNode::ParamWithoutDecorators(p) => match &p.pat {
            ast::Pat::Ident(i) => Some(DeclName::BindingIdent(i.clone())),
            _ => todo!(),
        },
        BoundNode::TsAmbientParam(p) => match &p.pat {
            ast::TsAmbientParamPat::Ident(i) => Some(DeclName::BindingIdent(i.clone())),
            ast::TsAmbientParamPat::Rest(p) => match &p.arg {
                ast::Pat::Ident(i) => Some(DeclName::BindingIdent(i.clone())),
                _ => todo!(),
            },
            _ => todo!(),
        },
        BoundNode::TsParamProp(p) => match &p.param {
            ast::TsParamPropParam::Ident(i) => Some(DeclName::BindingIdent(i.clone())),
            _ => todo!(),
        },
        // TODO:
        // BindingElement
        // ObjectLiteralElement
        // PropertyLikeDeclaration
        // ClassLikeDeclarationBase
        // ClassElement
        // TypeElement
        // EnumMember
        // ImportClause
        // NamespaceImport
        // NamespaceExport
        // ImportSpecifier
        // ExportSpecifier
        BoundNode::ClassProp(prop) => {
            if prop.computed {
                Some(DeclName::ComputedProperty(prop.key.clone()))
            } else {
                match &prop.key {
                    Expr::Ident(i) => Some(DeclName::Ident(i.clone())),
                    Expr::Lit(ast::Lit::Str(s)) => Some(DeclName::String(s.clone())),
                    Expr::Lit(ast::Lit::Num(n)) => Some(DeclName::Number(n.clone())),
                    Expr::Lit(ast::Lit::BigInt(_)) => todo!(),
                    _ => unreachable!(),
                }
            }
        }
        BoundNode::ClassMethod(method) => match &method.key {
            ast::PropName::Ident(i) => Some(DeclName::Ident(i.clone())),
            ast::PropName::Str(s) => Some(DeclName::String(s.clone())),
            ast::PropName::Num(n) => Some(DeclName::Number(n.clone())),
            ast::PropName::BigInt(_) => todo!(),
            ast::PropName::Computed(c) => Some(DeclName::ComputedProperty(c.expr.clone())),
        },
        BoundNode::TsModuleDecl(d) => match &d.id {
            ast::TsModuleName::Ident(i) => Some(DeclName::Ident(i.clone())),
            ast::TsModuleName::Str(s) => Some(DeclName::String(s.clone())),
        },
        BoundNode::RestPat(p) => match &p.arg {
            ast::Pat::Ident(i) => Some(DeclName::BindingIdent(i.clone())),
            _ => todo!(),
        },
        BoundNode::BindingIdent(i) => Some(DeclName::BindingIdent(i.node.clone())),
        BoundNode::TsTypeParamDecl(p) => Some(DeclName::Ident(p.name.clone())),
        BoundNode::TsFnType(_) => None,
        BoundNode::TsConstructorType(_) => None,
        BoundNode::TsCallSignatureDecl(_) => None,
        BoundNode::TsConstructSignatureDecl(_) => None,
        BoundNode::TsIndexSignature(_) => None,
        BoundNode::TsPropertySignature(s) => {
            if s.computed {
                Some(DeclName::ComputedProperty(s.key.clone()))
            } else {
                match &s.key {
                    Expr::Ident(i) => Some(DeclName::Ident(i.clone())),
                    Expr::Lit(ast::Lit::Str(s)) => Some(DeclName::String(s.clone())),
                    Expr::Lit(ast::Lit::Num(n)) => Some(DeclName::Number(n.clone())),
                    Expr::Lit(ast::Lit::BigInt(_)) => todo!(),
                    _ => unreachable!(),
                }
            }
        }
        BoundNode::TsMethodSignature(s) => {
            if s.computed {
                Some(DeclName::ComputedProperty(s.key.clone()))
            } else {
                match &s.key {
                    Expr::Ident(i) => Some(DeclName::Ident(i.clone())),
                    Expr::Lit(ast::Lit::Str(s)) => Some(DeclName::String(s.clone())),
                    Expr::Lit(ast::Lit::Num(n)) => Some(DeclName::Number(n.clone())),
                    Expr::Lit(ast::Lit::BigInt(_)) => todo!(),
                    _ => unreachable!(),
                }
            }
        }
        BoundNode::TsTypeAliasDecl(a) => Some(DeclName::Ident(a.id.clone())),
        BoundNode::KeyValueProp(p) => match &p.key {
            ast::PropName::Ident(i) => Some(DeclName::Ident(i.clone())),
            ast::PropName::Str(s) => Some(DeclName::String(s.clone())),
            ast::PropName::Num(n) => Some(DeclName::Number(n.clone())),
            ast::PropName::BigInt(_) => todo!(),
            ast::PropName::Computed(c) => Some(DeclName::ComputedProperty(c.expr.clone())),
        },
        BoundNode::FnDecl(d) => Some(DeclName::Ident(d.ident.clone())),
        _ => {
            println!("==============WARNING==============");
            println!(
                "getNonAssignedNameOfDeclaration: Unknown node passed. Node: {:?}",
                declaration
            );
            println!("===================================");
            println!();
            None
            // return (declaration as NamedDeclaration).name;
        }
    }
}

pub fn getNameOfDeclaration(declaration: &BoundNode) -> Option<DeclName> {
    let name = getNonAssignedNameOfDeclaration(declaration);
    if name.is_some() {
        return name;
    }

    match declaration {
        BoundNode::FnExpr(_) | BoundNode::ArrowExpr(_) | BoundNode::ClassExpr(_) => {
            todo!();
            // getAssignedName(declaration)
        }
        _ => None,
    }
}

// fn getAssignedName(node: BoundNode) -> Option<JsWord> {
//     node.parent().map(|parent| match parent {
//         // TODO:
//         // BoundNode::isPropertyAssignment(_) => parent.name,
//         // BoundNode::isBindingElement(_) => parent.name,
//         BoundNode::BinExpr(e) if node == e.right => match e.left {
//             BoundNode::Ident(i) => i.sym,
//             BoundNode::MemberExpr(e) => match e.prop {
//                 BoundNode::Ident(i) => i.sym,
//                 _ => None,
//             },
//         },
//         BoundNode::VarDeclarator(d) => {
//             if let ast::Pat::Ident(name) = d.name {
//                 name.sym
//             } else {
//                 None
//             }
//         }
//         _ => None,
//     })
// }

fn isAssignmentDeclaration(decl: &BoundNode) -> bool {
    matches!(
        decl,
        BoundNode::BinExpr(_)
            | BoundNode::MemberExpr(_)
            // TODO: BindingIdent?
            | BoundNode::Ident(_)
            | BoundNode::CallExpr(_)
    )
}

/// An effective module (namespace) declaration is either
/// 1. An actual declaration: namespace X { ... }
/// 2. A Javascript declaration, which is:
///    An identifier in a nested property access expression: Y in `X.Y.Z = { ... }`
fn isEffectiveModuleDeclaration(node: &BoundNode) -> bool {
    matches!(node, BoundNode::TsModuleDecl(_) | BoundNode::Ident(_))
}

bitflags! {
    pub struct FunctionFlags: u8 {
        const Normal = 0;             // Function is a normal function
        const Generator = 1 << 0;     // Function is a generator function or async generator function
        const Async = 1 << 1;         // Function is an async function or an async generator function
        const Invalid = 1 << 2;       // Function is a signature or overload and does not have a body.
        const AsyncGenerator = Self::Async.bits | Self::Generator.bits; // Function is an async generator function
    }
}

pub fn getFunctionFlags(node: Option<&BoundNode>) -> FunctionFlags {
    if let Some(node) = node {
        let mut flags = FunctionFlags::Normal;
        match node {
            BoundNode::TsCallSignatureDecl(_)
            | BoundNode::TsConstructSignatureDecl(_)
            | BoundNode::TsMethodSignature(_)
            | BoundNode::TsIndexSignature(_)
            | BoundNode::TsFnType(_)
            | BoundNode::TsConstructorType(_)
            | BoundNode::TsGetterSignature(_)
            | BoundNode::TsSetterSignature(_) => {
                flags |= FunctionFlags::Invalid;
            }
            // TODO: jsdoc:
            // BoundNode::JSDocFunctionType(n) => {},
            BoundNode::Constructor(n) => {
                if n.body.is_none() {
                    flags |= FunctionFlags::Invalid;
                }
            }
            BoundNode::GetterProp(n) => {
                if n.body.is_none() {
                    flags |= FunctionFlags::Invalid;
                }
            }
            BoundNode::SetterProp(n) => {
                if n.body.is_none() {
                    flags |= FunctionFlags::Invalid;
                }
            }
            BoundNode::FnDecl(n) => {
                if n.function.is_generator {
                    flags |= FunctionFlags::Generator;
                }
                if n.function.is_async {
                    flags |= FunctionFlags::Async;
                }

                if n.function.body.is_none() {
                    flags |= FunctionFlags::Invalid;
                }
            }
            BoundNode::FnExpr(n) => {
                if n.function.is_generator {
                    flags |= FunctionFlags::Generator;
                }
                if n.function.is_async {
                    flags |= FunctionFlags::Async;
                }

                if n.function.body.is_none() {
                    flags |= FunctionFlags::Invalid;
                }
            }
            BoundNode::PrivateMethod(n) => {
                if n.function.is_generator {
                    flags |= FunctionFlags::Generator;
                }
                if n.function.is_async {
                    flags |= FunctionFlags::Async;
                }

                if n.function.body.is_none() {
                    flags |= FunctionFlags::Invalid;
                }
            }
            BoundNode::ClassMethod(n) => {
                if n.function.is_generator {
                    flags |= FunctionFlags::Generator;
                }
                if n.function.is_async {
                    flags |= FunctionFlags::Async;
                }

                if n.function.body.is_none() {
                    flags |= FunctionFlags::Invalid;
                }
            }
            BoundNode::MethodProp(n) => {
                if n.function.is_generator {
                    flags |= FunctionFlags::Generator;
                }
                if n.function.is_async {
                    flags |= FunctionFlags::Async;
                }

                if n.function.body.is_none() {
                    flags |= FunctionFlags::Invalid;
                }
            }
            BoundNode::ArrowExpr(n) => {
                if n.is_async {
                    flags |= FunctionFlags::Async;
                }
            }
            _ => unreachable!(),
        }

        flags
    } else {
        FunctionFlags::Invalid
    }
}

pub fn isStringOrNumericLiteralLike(expr: &Expr) -> bool {
    isStringLiteralLike(&expr) || matches!(expr, Expr::Lit(ast::Lit::Num(_)))
}

fn isSignedNumericLiteral(node: &Expr) -> bool {
    if let Expr::Unary(e) = node {
        if e.op == ast::UnaryOp::Plus || e.op == ast::UnaryOp::Minus {
            return matches!(e.arg, Expr::Lit(ast::Lit::Num(_)));
        }
    }
    false
}

// pub fn isDynamicName(name: &Expr) -> bool {
//     !isStringOrNumericLiteralLike(name) && !isSignedNumericLiteral(name)
// }

/// Within a computed name, a name is dynamic if all of the following are true:
///   1. The declaration has a computed property name.
///   2. The computed name is *not* expressed as a StringLiteral.
///   3. The computed name is *not* expressed as a NumericLiteral.
///   4. The computed name is *not* expressed as a PlusToken or MinusToken
///      immediately followed by a NumericLiteral.
pub fn hasDynamicName(declaration: &BoundNode) -> bool {
    let decl_name = getNameOfDeclaration(declaration);
    if let Some(decl_name) = decl_name {
        let expr = match decl_name {
            DeclName::ComputedProperty(e) => e,
            DeclName::ElementAccessExpression(e) => e,
            _ => return false,
        };
        return !isStringOrNumericLiteralLike(&expr) && !isSignedNumericLiteral(&expr);
    }
    false
}

fn isDynamicName(name: DeclName) -> bool {
    let expr = match &name {
        DeclName::ComputedProperty(e) => e,
        DeclName::ElementAccessExpression(e) => skipParenthesesOfExpr(e),
        _ => return false,
    };
    !isStringOrNumericLiteralLike(expr) && !isSignedNumericLiteral(expr)
}

pub fn get_text_of_no_substitution_template(tpl: &ast::Tpl) -> JsWord {
    let mut s = String::with_capacity((tpl.span.hi.0 - tpl.span.lo.0) as usize);
    for q in &tpl.quasis {
        s.push_str(q.raw.value.as_ref());
    }
    s.into()
}

pub fn isPartOfTypeQuery(mut node: BoundNode) -> bool {
    while matches!(node, BoundNode::TsQualifiedName(_) | BoundNode::Ident(_)) {
        node = match node.parent() {
            Some(p) => p,
            None => return false,
        }
    }
    matches!(node, BoundNode::TsTypeQuery(_))
}

pub fn isBlockOrCatchScoped(node: BoundNode) -> bool {
    let decl = getRootDeclaration(node);
    match decl {
        BoundNode::VarDecl(d)
            if d.kind == ast::VarDeclKind::Const || d.kind == ast::VarDeclKind::Let =>
        {
            true
        }
        BoundNode::CatchClause(_) => true,
        _ => false,
    }
}

pub fn isCatchClauseVariableDeclarationOrBindingElement(declaration: BoundNode) -> bool {
    fn is_pat(n: &BoundNode) -> bool {
        matches!(
            n,
            BoundNode::BindingIdent(_)
                | BoundNode::ArrayPat(_)
                | BoundNode::RestPat(_)
                | BoundNode::ObjectPat(_)
                | BoundNode::AssignPat(_)
        )
    }
    let root_pat = walkUp(declaration, is_pat);
    is_pat(&root_pat) && matches!(root_pat.parent(), Some(BoundNode::CatchClause(_)))
}

pub fn isAmbientModule(node: &BoundNode) -> bool {
    if let BoundNode::TsModuleDecl(d) = node {
        if let ast::TsModuleName::Str(_) = d.id {
            return true;
        } else {
            return isGlobalScopeAugmentation(d.as_ref());
        }
    }
    false
}

pub fn isGlobalScopeAugmentation(module: &TsModuleDecl) -> bool {
    module.global
}

/**
 * Remove extra underscore from escaped identifier text content.
 *
 * @param identifier The escaped identifier text.
 * @returns The unescaped identifier text.
 */
pub fn unescapeLeadingUnderscores(identifier: &JsWord) -> &JsWord {
    // todo:
    identifier
    // const id = identifier as string;
    // return id.length >= 3 && id.charCodeAt(0) === CharacterCodes._ && id.charCodeAt(1) === CharacterCodes._ && id.charCodeAt(2) === CharacterCodes._ ? id.substr(1) : id;
}

// export function idText(identifierOrPrivateName: Identifier | PrivateIdentifier): string {
//     return unescapeLeadingUnderscores(identifierOrPrivateName.escapedText);
// }
pub fn symbolName(_symbol: &Symbol) -> JsWord {
    todo!();
    // if (symbol.valueDeclaration
    //     && isPrivateIdentifierClassElementDeclaration(symbol.valueDeclaration))
    // {
    //     return idText(symbol.valueDeclaration.name);
    // }
    // return unescapeLeadingUnderscores(symbol.escapedName);
}

pub fn isClassElement(node: &BoundNode) -> bool {
    // TODO: static blocks:
    //| BoundNode::ClassStaticBlockDeclaration(_)
    matches!(
        node,
        BoundNode::Constructor(_)
            | BoundNode::ClassMethod(_)
            | BoundNode::PrivateMethod(_)
            | BoundNode::ClassProp(_)
            | BoundNode::PrivateProp(_)
            | BoundNode::TsIndexSignature(_)
            | BoundNode::EmptyStmt(_)
    )
}

fn isClassLike(node: &BoundNode) -> bool {
    matches!(node, BoundNode::ClassDecl(_) | BoundNode::ClassExpr(_))
}

pub fn isBindingPattern(node: &BoundNode) -> bool {
    matches!(node, BoundNode::ArrayPat(_) | BoundNode::ObjectPat(_))
}

/** Get the initializer, taking into account defaulted Javascript initializers */
fn getEffectiveInitializer(node: &BoundNode) -> Option<BoundNode> {
    let (name, initializer) = match node {
        BoundNode::VarDeclarator(n) => (&n.name, n.init.as_ref().map(|i| i.bind(node.clone()))),
        _ => {
            todo!();
            // unreachable!();
        }
    };
    if isBoundNodeInJSFile(node) {
        if let Some(init @ BoundNode::BinExpr(init_expr)) = &initializer {
            if init_expr.op == ast::BinaryOp::LogicalOr
                || init_expr.op == ast::BinaryOp::NullishCoalescing
            {
                if isEntityNameExpression(&name.clone().into())
                    && isSameEntityName(&ast::PatOrExpr::Pat(name.clone()), &init_expr.left)
                {
                    return Some(init_expr.right.bind(init.clone()));
                }
            }
        }
    }

    initializer
}

/** Get the declaration initializer when it is container-like (See getExpandoInitializer). */
pub fn getDeclaredExpandoInitializer(node: &BoundNode) -> Option<BoundNode> {
    let init = getEffectiveInitializer(node);
    init.and_then(|init| {
        let name = match node {
            BoundNode::VarDeclarator(n) => n.name.clone().into(),
            _ => {
                todo!();
                // unreachable!();
            }
        };
        getExpandoInitializer(init, isPrototypeAccess(&name))
    })
}

fn hasExpandoValueProperty(node: Rc<ObjectLit>, isPrototypeAssignment: bool) -> Option<BoundNode> {
    for p in &node.props {
        if let ast::Prop::KeyValue(prop) = p {
            if let ast::PropName::Ident(key) = &prop.key {
                if key.sym == JsWord::from("value") {
                    return getExpandoInitializer(
                        prop.value.bind(prop.bind(node.clone().into())),
                        isPrototypeAssignment,
                    );
                }
            }
        }
    }
    None
}

/**
 * Get the assignment 'initializer' -- the righthand side-- when the initializer is container-like (See getExpandoInitializer).
 * We treat the right hand side of assignments with container-like initializers as declarations.
 */
pub fn getAssignedExpandoInitializer(node: Option<BoundNode>) -> Option<BoundNode> {
    if let Some(node) = node {
        if let Some(parent @ BoundNode::AssignExpr(assign)) = &node.parent() {
            if assign.op == ast::AssignOp::Assign {
                let isPrototypeAssignment = matches!(&assign.left, ast::PatOrExpr::Expr(e) if isPrototypeAccess(&e.clone().into()));
                return getExpandoInitializer(
                    assign.right.bind(parent.clone()),
                    isPrototypeAssignment,
                )
                .or_else(|| {
                    getDefaultedExpandoInitializer(
                        &assign.left,
                        &assign.right.bind(parent.clone()),
                        isPrototypeAssignment,
                    )
                });
            }
        }
        if let BoundNode::CallExpr(call) = &node {
            if let Some(c) = asBindableObjectDefinePropertyCall(call) {
                return hasExpandoValueProperty(
                    c.descriptor,
                    c.prop_string == Some(JsWord::from("prototype")),
                );
            }
        }
    }
    None
}

/**
 * Recognized expando initializers are:
 * 1. (function() {})() -- IIFEs
 * 2. function() { } -- Function expressions
 * 3. class { } -- Class expressions
 * 4. {} -- Empty object literals
 * 5. { ... } -- Non-empty object literals, when used to initialize a prototype, like `C.prototype = { m() { } }`
 *
 * This function returns the provided initializer, or undefined if it is not valid.
 */
pub fn getExpandoInitializer(
    initializer: BoundNode,
    isPrototypeAssignment: bool,
) -> Option<BoundNode> {
    match &initializer {
        BoundNode::CallExpr(call) => {
            if let ExprOrSuper::Expr(callee) = &call.callee {
                let e = skipParenthesesOfExpr(callee);
                if matches!(e, Expr::Fn(_) | Expr::Arrow(_)) {
                    return Some(initializer);
                }
            }
        }
        BoundNode::FnExpr(_) | BoundNode::ClassExpr(_) | BoundNode::ArrowExpr(_) => {
            return Some(initializer)
        }
        BoundNode::ObjectLit(obj) => {
            if obj.props.is_empty() || isPrototypeAssignment {
                return Some(initializer);
            }
        }
        _ => {}
    }
    None
}

/**
 * A defaulted expando initializer matches the pattern
 * `Lhs = Lhs || ExpandoInitializer`
 * or `var Lhs = Lhs || ExpandoInitializer`
 *
 * The second Lhs is required to be the same as the first except that it may be prefixed with
 * 'window.', 'global.' or 'self.' The second Lhs is otherwise ignored by the binder and checker.
 */
fn getDefaultedExpandoInitializer(
    name: &ast::PatOrExpr,
    initializer: &BoundNode,
    isPrototypeAssignment: bool,
) -> Option<BoundNode> {
    if let BoundNode::BinExpr(init) = initializer {
        if init.op == ast::BinaryOp::LogicalOr || init.op == ast::BinaryOp::NullishCoalescing {
            let e =
                getExpandoInitializer(init.right.bind(initializer.clone()), isPrototypeAssignment);
            if e.is_some() && isSameEntityName(name, &init.left) {
                return e;
            }
        }
    }
    None
}

// export function isDefaultedExpandoInitializer(node: BinaryExpression) {
//     const name = isVariableDeclaration(node.parent) ? node.parent.name :
//         isBinaryExpression(node.parent) && node.parent.operatorToken.kind === SyntaxKind.EqualsToken ? node.parent.left :
//         undefined;
//     return name && getExpandoInitializer(node.right, isPrototypeAccess(name)) && isEntityNameExpression(name) && isSameEntityName(name, node.left);
// }

// /** Given an expando initializer, return its declaration name, or the left-hand side of the assignment if it's part of an assignment declaration. */
// export function getNameOfExpando(node: Declaration): DeclarationName | undefined {
//     if (isBinaryExpression(node.parent)) {
//         const parent = ((node.parent.operatorToken.kind === SyntaxKind.BarBarToken || node.parent.operatorToken.kind === SyntaxKind.QuestionQuestionToken) && isBinaryExpression(node.parent.parent)) ? node.parent.parent : node.parent;
//         if (parent.operatorToken.kind === SyntaxKind.EqualsToken && isIdentifier(parent.left)) {
//             return parent.left;
//         }
//     }
//     else if (isVariableDeclaration(node.parent)) {
//         return node.parent.name;
//     }
// }

/**
 * Is the 'declared' name the same as the one in the initializer?
 * @return true for identical entity names, as well as ones where the initializer is prefixed with
 * 'window', 'self' or 'global'. For example:
 *
 * var my = my || {}
 * var min = window.min || {}
 * my.app = self.my.app || class { }
 */
pub fn isSameEntityName(name: &ast::PatOrExpr, initializer: &Expr) -> bool {
    // TODO: pat::BindingIdent?
    if let ast::PatOrExpr::Expr(name) = name {
        if isPropertyNameLiteral(&name) && isPropertyNameLiteral(&initializer) {
            // TODO: I think we need to use the node's text here, so [0] and ["0"] are
            // treated as equal, like TSC possibly does.
            return name.eq_ignore_span(&initializer);
        }
        if let Expr::Ident(name) = name {
            if isLiteralLikeAccess(&initializer) {
                let initializer = unwrap_as!(&initializer, Expr::Member(r), r);
                if matches!(initializer.obj, ExprOrSuper::Expr(Expr::This(_)))
                    || matches!(&initializer.obj, ExprOrSuper::Expr(Expr::Ident(i)) if i.sym == JsWord::from("window")||i.sym == JsWord::from("self")||i.sym == js_word!("global"))
                {
                    if let Expr::PrivateName(_) = initializer.prop {
                        // TODO:
                        panic!(
                            "Unexpected PrivateIdentifier in name expression with literal-like access."
                        );
                    }
                    return isSameEntityName(
                        &ast::PatOrExpr::Expr(Expr::Ident(name.clone())),
                        &initializer.prop,
                    );
                }
            }
        }
        if isLiteralLikeAccess(&name) && isLiteralLikeAccess(&initializer) {
            let name = unwrap_as!(&name, Expr::Member(r), r);
            let initializer = unwrap_as!(&initializer, Expr::Member(r), r);
            return getElementOrPropertyAccessName(name)
                == getElementOrPropertyAccessName(initializer)
                && match (&name.obj, &initializer.obj) {
                    (ExprOrSuper::Super(_), ExprOrSuper::Super(_)) => true,
                    (ExprOrSuper::Expr(e1), ExprOrSuper::Expr(e2)) => {
                        isSameEntityName(&ast::PatOrExpr::Expr(e1.clone()), e2)
                    }
                    _ => false,
                };
        }
    }
    false
}

fn getRightMostAssignedExpression(mut expr: &Expr) -> &Expr {
    while let Expr::Assign(assign) = expr {
        expr = &assign.right;
    }
    expr
}

fn isExportsIdentifier(expr: &Expr) -> bool {
    matches!(expr, Expr::Ident(i) if &i.sym == "exports")
}

fn isModuleIdentifier(expr: &ExprOrSuper) -> bool {
    matches!(expr, ExprOrSuper::Expr(Expr::Ident(i)) if &i.sym == "module")
}

fn isModuleExportsAccessExpression(expr: &Expr) -> bool {
    if let Expr::Member(member_expr) = expr {
        return (!member_expr.computed || isLiteralLikeElementAccess(&expr))
            && isModuleIdentifier(&member_expr.obj)
            && getElementOrPropertyAccessName(&member_expr)
                .map(|s| &s == "exports")
                .unwrap_or_default();
    }
    false
}

/// Given a BinaryExpression, returns SpecialPropertyAssignmentKind for the various kinds of property
/// assignments we treat as special in the binder
pub fn getAssignmentDeclarationKind(expr: &Expr) -> AssignmentDeclarationKind {
    let special = getAssignmentDeclarationKindWorker(expr);
    // TODO:
    if special == AssignmentDeclarationKind::Property
    /*|| isBoundNodeInJSFile(expr)*/
    {
        special
    } else {
        AssignmentDeclarationKind::None
    }
}

/// Object.defineProperty(obj, prop, descriptor)
///
/// See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperty
pub struct BindableObjectDefinePropertyCall {
    pub prop: BoundNode,
    pub prop_string: Option<JsWord>,
    pub descriptor: Rc<ObjectLit>,
}

/// See `BindableObjectDefinePropertyCall` for the definition of a "BindableObjectDefinePropertyCall".
pub fn asBindableObjectDefinePropertyCall(
    expr: &Rc<CallExpr>,
) -> Option<BindableObjectDefinePropertyCall> {
    if expr.args.len() == 3 {
        if let ExprOrSuper::Expr(Expr::Member(member_expr)) = &expr.callee {
            if !member_expr.computed {
                if let Expr::Ident(member_prop) = &member_expr.prop {
                    if let ExprOrSuper::Expr(Expr::Ident(member_obj)) = &member_expr.obj {
                        if member_obj.sym == js_word!("Object")
                            && member_prop.sym == JsWord::from("defineProperty")
                            && matches!(&expr.args[0], ast::ExprOrSpread::Expr(obj) if  isBindableStaticNameExpression(
                                &obj.clone().into(),
                                true,
                            ))
                        {
                            if let ast::ExprOrSpread::Expr(Expr::Object(descriptor)) = &expr.args[2]
                            {
                                let prop = match &expr.args[1] {
                                    ast::ExprOrSpread::Spread(s) => {
                                        s.expr.bind(expr.args[1].bind(expr.clone().into()))
                                    }
                                    ast::ExprOrSpread::Expr(e) => e.bind(expr.clone().into()),
                                };
                                let prop_string = match &prop {
                                    BoundNode::Number(_) => None,
                                    BoundNode::Str(s) => Some(s.value.clone()),
                                    BoundNode::Tpl(tpl) if tpl.exprs.is_empty() => {
                                        tpl.quasis.first().map(|q| {
                                            q.cooked
                                                .as_ref()
                                                .map(|s| s.value.clone())
                                                .unwrap_or(q.raw.value.clone())
                                        })
                                    }
                                    _ => return None,
                                };
                                return Some(BindableObjectDefinePropertyCall {
                                    prop,
                                    prop_string,
                                    descriptor: ObjectLit::new(
                                        descriptor.clone(),
                                        Some(expr.clone().into()),
                                    ),
                                });
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// See `BindableObjectDefinePropertyCall` for the definition of a "BindableObjectDefinePropertyCall".
pub fn isBindableObjectDefinePropertyCall(expr: &ast::CallExpr) -> bool {
    if expr.args.len() == 3 {
        if let ExprOrSuper::Expr(Expr::Member(member_expr)) = &expr.callee {
            if !member_expr.computed {
                if let Expr::Ident(member_prop) = &member_expr.prop {
                    if let ExprOrSuper::Expr(Expr::Ident(member_obj)) = &member_expr.obj {
                        return member_obj.sym == js_word!("Object")
                            && member_prop.sym == JsWord::from("defineProperty")
                            && matches!(&expr.args[0], ast::ExprOrSpread::Expr(e) if isBindableStaticNameExpression(
                                &e.clone().into(),
                                true,
                            ))
                            && matches!(&expr.args[1], ast::ExprOrSpread::Expr(e) if isStringOrNumericLiteralLike(e))
                            && matches!(expr.args[2], ast::ExprOrSpread::Expr(Expr::Object(_)));
                    }
                }
            }
        }
    }
    false
}

/// x.y OR x[0]
pub fn isLiteralLikeAccess(expr: &Expr) -> bool {
    if let Expr::Member(m) = expr {
        !m.computed || isStringOrNumericLiteralLike(&m.prop)
    } else {
        false
    }
}

/// x[0] OR x['a'] OR x[Symbol.y]
pub fn isLiteralLikeElementAccess(node: &Expr) -> bool {
    if let Expr::Member(m) = node {
        m.computed && isStringOrNumericLiteralLike(&m.prop)
    } else {
        false
    }
}

/// Any series of property and element accesses.
pub fn isBindableStaticAccessExpression(node: &Node, excludeThisKeyword: bool) -> bool {
    if let Node::MemberExpr(member_expr) = node {
        if !member_expr.computed {
            // if !excludeThisKeyword
            //     && matches!(member_expr.obj, ExprOrSuper::Expr(Expr::This(_)))
            // {
            //     return true;
            // } else if matches!(member_expr.prop, Expr::Ident(_))
            //     && isBindableStaticNameExpression(member_expr.obj, true)
            // {
            //     return true;
            // } else if isBindableStaticElementAccessExpression(node, excludeThisKeyword) {
            //     return true;
            // }
            return (!excludeThisKeyword
                && matches!(member_expr.obj, ExprOrSuper::Expr(Expr::This(_)))
                || matches!(member_expr.prop, Expr::Ident(_))
                    && isBindableStaticNameExpression(&member_expr.obj.clone().into(), true))
                || isBindableStaticElementAccessExpression(node, excludeThisKeyword);
        }
    }
    false
}

/// Any series of property and element accesses, ending in a literal element access
pub fn isBindableStaticElementAccessExpression(node: &Node, excludeThisKeyword: bool) -> bool {
    if let Node::MemberExpr(member_expr) = node {
        if member_expr.computed {
            return (!excludeThisKeyword
                && matches!(member_expr.obj, ExprOrSuper::Expr(Expr::This(_))))
                || isEntityNameExpression(&member_expr.obj.clone().into())
                || isBindableStaticAccessExpression(&member_expr.obj.clone().into(), true);
        }
    }
    false
}

pub fn isBindableStaticNameExpression(node: &Node, excludeThisKeyword: bool) -> bool {
    isEntityNameExpression(node) || isBindableStaticAccessExpression(node, excludeThisKeyword)
}

// export function getNameOrArgument(expr: PropertyAccessExpression | LiteralLikeElementAccessExpression) {
//     if (isPropertyAccessExpression(expr)) {
//         return expr.name;
//     }
//     return expr.argumentExpression;
// }

fn getAssignmentDeclarationKindWorker(expr: &Expr) -> AssignmentDeclarationKind {
    match expr {
        Expr::Call(call_expr) => {
            if !isBindableObjectDefinePropertyCall(call_expr.as_ref()) {
                return AssignmentDeclarationKind::None;
            }
            let entityName = match call_expr.args.first().unwrap() {
                ast::ExprOrSpread::Spread(s) => &s.expr,
                ast::ExprOrSpread::Expr(expr) => expr,
            };
            if isExportsIdentifier(entityName) || isModuleExportsAccessExpression(entityName) {
                return AssignmentDeclarationKind::ObjectDefinePropertyExports;
            }
            if isBindableStaticAccessExpression(&entityName.clone().into(), false)
                && getElementOrPropertyAccessName(unwrap_as!(entityName, Expr::Member(m), m))
                    .map(|s| &s == "prototype")
                    .unwrap_or_default()
            {
                return AssignmentDeclarationKind::ObjectDefinePrototypeProperty;
            }
            AssignmentDeclarationKind::ObjectDefinePropertyValue
        }
        Expr::Assign(assign_expr) => {
            if let ast::PatOrExpr::Expr(Expr::Member(left_member_expr)) = &assign_expr.left {
                if isVoidZero(getRightMostAssignedExpression(&expr)) {
                    return AssignmentDeclarationKind::None;
                }
                if isBindableStaticNameExpression(&left_member_expr.obj.clone().into(), true)
                    && getElementOrPropertyAccessName(left_member_expr.as_ref())
                        .map(|s| &s == "prototype")
                        .unwrap_or_default()
                    && matches!(
                        getInitializerOfAssignmentExpression(assign_expr.as_ref()),
                        Expr::Object(_)
                    )
                {
                    // F.prototype = { ... }
                    return AssignmentDeclarationKind::Prototype;
                }
                getAssignmentDeclarationPropertyAccessKind(left_member_expr)
            } else {
                AssignmentDeclarationKind::None
            }
        }
        _ => unreachable!(),
    }
}

fn isVoidZero(expr: &Expr) -> bool {
    if let Expr::Unary(expr) = expr {
        if expr.op == ast::UnaryOp::Void {
            if let Expr::Lit(ast::Lit::Num(num)) = &expr.arg {
                // TODO: we use the number's value while TSC uses its text (`node.expression.text == "0"`).
                // This means TSC only accepts `void 0`, while we accept `void 0` and others, such as `void 0.0`.
                return num.value == 0.0;
            }
        }
    }
    false
}

// /**
//  * Does not handle signed numeric names like `a[+0]` - handling those would require handling prefix unary expressions
//  * throughout late binding handling as well, which is awkward (but ultimately probably doable if there is demand)
//  */
// fn getElementOrPropertyAccessArgumentExpressionOrName(node: Rc<MemberExpr>) -> Option<BoundNode> {
//     if !node.computed {
//         match node.prop {
//             Expr::Ident(i) => return i,
//             _ => unreachable!(),
//         }
//     }
//     let arg = skipParentheses(node.prop);
//     if isNumericLiteral(arg) || isStringLiteralLike(arg) {
//         return arg;
//     }
//     todo!();
//     // node
// }

fn getElementOrPropertyAccessName(node: &ast::MemberExpr) -> Option<JsWord> {
    if !node.computed {
        match &node.prop {
            Expr::Ident(i) => return Some(i.sym.clone()),
            _ => unreachable!(),
        }
    }
    todo!()
}

pub fn getAssignmentDeclarationPropertyAccessKind(
    lhs: &Rc<ast::MemberExpr>,
) -> AssignmentDeclarationKind {
    let lhs_expr = Expr::Member(lhs.clone());
    if matches!(&lhs.obj, ExprOrSuper::Expr(Expr::This(_))) {
        return AssignmentDeclarationKind::ThisProperty;
    } else if isModuleExportsAccessExpression(&lhs_expr) {
        // module.exports = expr
        return AssignmentDeclarationKind::ModuleExports;
    } else if isBindableStaticNameExpression(&lhs.obj.clone().into(), true) {
        if isPrototypeAccess(&lhs.obj.clone().into()) {
            // F.G....prototype.x = expr
            return AssignmentDeclarationKind::PrototypeProperty;
        }

        let mut nextToLast = lhs;
        while !matches!(nextToLast.obj, ExprOrSuper::Expr(Expr::Ident(_))) {
            nextToLast = match &nextToLast.obj {
                ExprOrSuper::Expr(Expr::Member(m)) => m,
                _ => unreachable!(),
            }
        }
        let id = match &nextToLast.obj {
            ExprOrSuper::Expr(Expr::Ident(i)) => i,
            _ => unreachable!(),
        };
        if (&id.sym == "exports" ||
            &id.sym == "module" && getElementOrPropertyAccessName(nextToLast).map(|s| &s == "exports").unwrap_or_default()) &&
            // ExportsProperty does not support binding with computed names
            isBindableStaticAccessExpression(&lhs_expr.clone().into(), false)
        {
            // exports.name = expr OR module.exports.name = expr OR exports["name"] = expr ...
            return AssignmentDeclarationKind::ExportsProperty;
        }
        if isBindableStaticNameExpression(&lhs_expr.into(), true)
            || (lhs.computed && isDynamicName(DeclName::ElementAccessExpression(lhs.prop.clone())))
        {
            // F.G...x = expr
            return AssignmentDeclarationKind::Property;
        }
    }

    AssignmentDeclarationKind::None
}

// fn getInitializerOfBinaryExpression(expr: BinaryExpression) {
//     while (isBinaryExpression(expr.right)) {
//         expr = expr.right;
//     }
//     return expr.right;
// }

fn getInitializerOfAssignmentExpression(expr: &ast::AssignExpr) -> &Expr {
    let mut expr = &expr.right;
    while let Expr::Assign(assign) = expr {
        expr = &assign.right;
    }
    expr
}

pub fn isEntityNameExpression(node: &Node) -> bool {
    matches!(node, Node::Ident(_)) || isPropertyAccessEntityNameExpression(node)
}

pub fn isPropertyAccessEntityNameExpression(node: &Node) -> bool {
    if let Node::MemberExpr(member_expr) = node {
        if !member_expr.computed {
            if matches!(member_expr.prop, Expr::Ident(_)) {
                return isEntityNameExpression(&member_expr.obj.clone().into());
            }
        }
    }
    false
}

pub fn isPrototypeAccess(node: &Node) -> bool {
    isBindableStaticAccessExpression(node, false)
        && getElementOrPropertyAccessName(unwrap_as!(&node, Node::MemberExpr(m), m))
            .map(|s| &s == "prototype")
            .unwrap_or_default()
}

fn isPropertyNameLiteral(expr: &Expr) -> bool {
    match expr {
        Expr::Ident(_) | Expr::Lit(ast::Lit::Str(_) | ast::Lit::Num(_)) => true,
        Expr::Tpl(t) => t.exprs.is_empty(),
        _ => false,
    }
}

fn isDeclaration(node: &BoundNode) -> bool {
    if let BoundNode::TsTypeParamDecl(_) = node {
        todo!();
        // return (node.parent && node.parent.kind !== SyntaxKind.JSDocTemplateTag) || isBoundNodeInJSFile(node);
    }
    match node {
        // TODO:
        // BoundNode::ClassStaticBlockDeclaration(_)|
        // BoundNode::ExportSpecifier(_)|
        // BoundNode::ImportClause(_)|
        // BoundNode::ImportEqualsDeclaration(_)|
        // BoundNode::ImportSpecifier(_)|
        // BoundNode::JsxAttribute(_)|
        // BoundNode::NamespaceExportDeclaration(_)|
        // BoundNode::NamespaceImport(_)|
        // BoundNode::NamespaceExport(_)|
        // BoundNode::JSDocTypedefTag(_)|
        // BoundNode::JSDocCallbackTag(_)|
        // BoundNode::JSDocPropertyTag(_)|
        // BoundNode::TsTypeParamDecl(_)|
        BoundNode::ArrowExpr(_)
        | BoundNode::BindingIdent(_)
        | BoundNode::RestPat(_)
        | BoundNode::AssignPat(_)
        | BoundNode::ClassDecl(_)
        | BoundNode::ClassExpr(_)
        | BoundNode::Constructor(_)
        | BoundNode::TsEnumDecl(_)
        | BoundNode::TsEnumMember(_)
        | BoundNode::FnDecl(_)
        | BoundNode::FnExpr(_)
        | BoundNode::GetterProp(_)
        | BoundNode::TsInterfaceDecl(_)
        | BoundNode::PrivateMethod(_)
        | BoundNode::ClassMethod(_)
        | BoundNode::MethodProp(_)
        | BoundNode::TsMethodSignature(_)
        | BoundNode::TsModuleDecl(_)
        | BoundNode::Param(_)
        | BoundNode::ParamWithoutDecorators(_)
        | BoundNode::TsAmbientParam(_)
        | BoundNode::TsParamProp(_)
        | BoundNode::KeyValueProp(_)
        | BoundNode::ClassProp(_)
        | BoundNode::PrivateProp(_)
        | BoundNode::TsPropertySignature(_)
        | BoundNode::SetterProp(_)
        | BoundNode::TsTypeAliasDecl(_)
        | BoundNode::VarDeclarator(_) => true,
        // Shorthand property. e.g. `a` in `{ a, }`
        BoundNode::Ident(_) => matches!(node.parent(), Some(BoundNode::ObjectLit(_))),
        _ => false,
    }
}

pub fn isRightSideOfQualifiedNameOrPropertyAccessOrJSDocMemberName(node: &BoundNode) -> bool {
    if let Some(parent) = node.parent() {
        return match &parent {
            // TODO: JSDoc
            // BoundNode::JSDocMemberName(n) => n.right == node,
            BoundNode::TsQualifiedName(n) => &n.right.bind(parent.clone()) == node,
            BoundNode::MemberExpr(n) => !n.computed && &n.prop.bind(parent.clone()) == node,

            _ => false,
        };
    }
    false
}

pub fn isBoundNodeInJSFile(_node: &BoundNode) -> bool {
    // TODO: js files
    false
}

pub fn isNodeInJSFile(_node: &Node) -> bool {
    // TODO: js files
    false
}

pub fn nodeIsSynthesized(_node: &BoundNode) -> bool {
    // TODO:
    false
}

pub fn isPrivateIdentifierClassElementDeclaration(node: &BoundNode) -> bool {
    matches!(
        node,
        BoundNode::PrivateMethod(_) | BoundNode::PrivateProp(_)
    )
}

pub fn isEntityName(node: &BoundNode) -> bool {
    matches!(node, BoundNode::Ident(_) | BoundNode::TsQualifiedName(_))
}

pub fn isExpressionNode(node: &BoundNode) -> bool {
    match node {
        // TODO: move down to be with BoundNode::Str
        BoundNode::Tpl(n) if n.exprs.is_empty() => {
            isInExpressionContext(node.parent().unwrap(), Expr::Tpl(n.node.clone()))
        }
        BoundNode::Super(_)
        | BoundNode::Null(_)
        | BoundNode::Bool(_)
        | BoundNode::Regex(_)
        | BoundNode::ArrayLit(_)
        | BoundNode::ObjectLit(_)
        | BoundNode::MemberExpr(_)
        | BoundNode::CallExpr(_)
        | BoundNode::NewExpr(_)
        | BoundNode::TaggedTpl(_)
        | BoundNode::TsAsExpr(_)
        | BoundNode::TsTypeAssertion(_)
        | BoundNode::TsNonNullExpr(_)
        | BoundNode::ParenExpr(_)
        | BoundNode::FnExpr(_)
        | BoundNode::ClassExpr(_)
        | BoundNode::ArrowExpr(_)
        | BoundNode::UnaryExpr(_)
        | BoundNode::BinExpr(_)
        | BoundNode::CondExpr(_)
        | BoundNode::SpreadElement(_)
        | BoundNode::Tpl(_)
        // TODO: jsx
        // | BoundNode::JsxElement(_)
        // | BoundNode::JsxSelfClosingElement(_)
        // | BoundNode::JsxFragment(_)
        | BoundNode::YieldExpr(_)
        | BoundNode::AwaitExpr(_)
        | BoundNode::MetaPropExpr(_) => true,
        BoundNode::TsQualifiedName(_) => {
            todo!();
            // let mut n = node.clone();
            // while let Some(parent @ BoundNode::TsQualifiedName(_)) = n.parent() {
            //     n = parent;
            // }
            // let parent = node.parent().unwrap();
            // matches!(parent, BoundNode::TsTypeQuery(_))
            //     || isJSDocLinkLike(parent)
            //     || isJSDocNameReference(parent)
            //     || isJSDocMemberName(parent)
            //     || isJSXTagName(n)
        }
        // TODO: JSdoc
        // BoundNode::JSDocMemberName(_) => {
        //     while (isJSDocMemberName(node.parent)) {
        //         node = node.parent;
        //     }
        //     node.parent.kind == SyntaxKind.TypeQuery
        //         || isJSDocLinkLike(node.parent)
        //         || isJSDocNameReference(node.parent)
        //         || isJSDocMemberName(node.parent)
        //         || isJSXTagName(node)
        // }
        BoundNode::PrivateName(n) => {
            if let Some(BoundNode::BinExpr(bin)) = node.parent() {
                return bin.op == ast::BinaryOp::In
                    && bin.left == Expr::PrivateName(n.node.clone());
            }
            false
        }
        BoundNode::Ident(i) => {
            // TODO:
            // let parent = node.parent();
            // if parent.kind == SyntaxKind.TypeQuery
            //     || isJSDocLinkLike(parent)
            //     || isJSDocNameReference(parent)
            //     || isJSDocMemberName(parent)
            //     || isJSXTagName(node)
            // {
            //     return true;
            // }
            isInExpressionContext(node.parent().unwrap(), Expr::Ident(i.node.clone()))
        }
        BoundNode::Number(n) => isInExpressionContext(
            node.parent().unwrap(),
            Expr::Lit(ast::Lit::Num(n.node.clone())),
        ),
        BoundNode::BigInt(n) => isInExpressionContext(
            node.parent().unwrap(),
            Expr::Lit(ast::Lit::BigInt(n.node.clone())),
        ),
        BoundNode::Str(n) => isInExpressionContext(
            node.parent().unwrap(),
            Expr::Lit(ast::Lit::Str(n.node.clone())),
        ),
        BoundNode::ThisExpr(n) => {
            isInExpressionContext(node.parent().unwrap(), Expr::This(n.node.clone()))
        }
        _ => false,
    }
}

pub fn isInExpressionContext(parent: BoundNode, expr: Expr) -> bool {
    match parent {
        BoundNode::VarDeclarator(parent) => parent.init == Some(expr),
        // BoundNode::Param(parent) => parent.pat == expr,
        BoundNode::ClassProp(parent) => parent.value == Some(expr),
        BoundNode::PrivateProp(parent) => parent.value == Some(expr),
        // BoundNode::PropertySignature(parent) => parent.initializer == expr,
        BoundNode::TsEnumMember(parent) => parent.init == Some(expr),
        BoundNode::KeyValueProp(parent) => parent.value == expr,
        // BoundNode::BindingElement(parent) => parent.initializer == expr,
        BoundNode::ExprStmt(parent) => parent.expr == expr,
        BoundNode::IfStmt(parent) => parent.test == expr,
        BoundNode::DoWhileStmt(parent) => parent.test == expr,
        BoundNode::WhileStmt(parent) => parent.test == expr,
        BoundNode::ReturnStmt(parent) => parent.arg == Some(expr),
        BoundNode::WithStmt(parent) => parent.obj == expr,
        BoundNode::SwitchStmt(parent) => parent.discriminant == expr,
        BoundNode::SwitchCase(parent) => parent.test == Some(expr),
        BoundNode::ThrowStmt(parent) => parent.arg == expr,
        BoundNode::ForStmt(_parent) => {
            todo!();
            // (parent.init == expr && !matches!(parent.init, Some(ast::VarDeclOrExpr::VarDecl(_))))
            //     || parent.test == expr
            //     || parent.update == expr
        }
        BoundNode::ForInStmt(_parent) => {
            todo!();
            // (parent.left == expr && !matches!(parent.left, ast::VarDeclOrPat::VarDecl(_)))
            //     || parent.right == expr
        }
        BoundNode::ForOfStmt(_parent) => {
            todo!();
            // (parent.left == expr && !matches!(parent.left, ast::VarDeclOrPat::VarDecl(_)))
            //     || parent.right == expr
        }
        BoundNode::TsTypeAssertion(parent) => expr == parent.expr,
        BoundNode::TsAsExpr(parent) => expr == parent.expr,
        // BoundNode::TemplateSpan(parent) => expr == (parent as TemplateSpan).expression,
        BoundNode::ComputedPropName(parent) => expr == parent.expr,
        // BoundNode::Decorator(parent)
        // | BoundNode::JsxExpression(parent)
        // | BoundNode::JsxSpreadAttribute(parent)
        // | BoundNode::SpreadAssignment(parent) => true,
        // BoundNode::ExpressionWithTypeArguments(parent) => {
        //     (parent as ExpressionWithTypeArguments).expression == expr
        //         && isExpressionWithTypeArgumentsInClassExtendsClause(parent)
        // }
        // BoundNode::ShorthandPropertyAssignment(parent) => {
        //     (parent as ShorthandPropertyAssignment).objectAssignmentInitializer == expr
        // }
        _ => {
            // dbg!(parent, expr);
            // todo!()
            // TODO:
            isExpressionNode(&parent)
        }
    }
}

pub fn isImportCall(n: &BoundNode) -> bool {
    matches!(n, BoundNode::CallExpr(e) if matches!(&e.callee, ast::ExprOrSuper::Expr(ast::Expr::Ident(i)) if i.sym == js_word!("import")))
}

pub fn isPartOfTypeNode(node: &BoundNode) -> bool {
    match node {
        BoundNode::TsTypePredicate(_)
        | BoundNode::TsTypeRef(_)
        | BoundNode::TsFnType(_)
        | BoundNode::TsConstructorType(_)
        | BoundNode::TsTypeQuery(_)
        | BoundNode::TsTypeLit(_)
        | BoundNode::TsArrayType(_)
        | BoundNode::TsTupleType(_)
        | BoundNode::TsOptionalType(_)
        | BoundNode::TsRestType(_)
        | BoundNode::TsUnionType(_)
        | BoundNode::TsIntersectionType(_)
        | BoundNode::TsConditionalType(_)
        | BoundNode::TsInferType(_)
        | BoundNode::TsParenthesizedType(_)
        | BoundNode::TsThisType(_)
        | BoundNode::TsTypeOperator(_)
        | BoundNode::TsIndexedAccessType(_)
        | BoundNode::TsMappedType(_)
        | BoundNode::TsLitType(_)
        | BoundNode::TsTupleElement(_)
        | BoundNode::TsTplLitType(_)
        | BoundNode::TsImportType(_) => true,

        BoundNode::TsKeywordType(t) => match t.kind {
            ast::TsKeywordTypeKind::TsAnyKeyword
            | ast::TsKeywordTypeKind::TsUnknownKeyword
            | ast::TsKeywordTypeKind::TsNumberKeyword
            | ast::TsKeywordTypeKind::TsBigIntKeyword
            | ast::TsKeywordTypeKind::TsStringKeyword
            | ast::TsKeywordTypeKind::TsBooleanKeyword
            | ast::TsKeywordTypeKind::TsSymbolKeyword
            | ast::TsKeywordTypeKind::TsObjectKeyword
            | ast::TsKeywordTypeKind::TsUndefinedKeyword
            | ast::TsKeywordTypeKind::TsNeverKeyword => true,
            ast::TsKeywordTypeKind::TsVoidKeyword => {
                !matches!(node.parent(), Some(BoundNode::UnaryExpr(e)) if e.op == ast::UnaryOp::Void)
            }
            _ => false,
        },
        BoundNode::TsExprWithTypeArgs(_) => {
            // TODO:
            true
            // return !isExpressionWithTypeArgumentsInClassExtendsClause(node);
        }
        BoundNode::TsTypeParam(_) => {
            matches!(
                node.parent(),
                Some(BoundNode::TsMappedType(_) | BoundNode::TsInferType(_))
            )
        }
        BoundNode::Ident(_)
        | BoundNode::TsQualifiedName(_)
        | BoundNode::MemberExpr(_)
        | BoundNode::ThisExpr(_) => {
            if matches!(node, BoundNode::MemberExpr(m) if m.computed) {
                return false;
            }
            let mut node = node.clone();
            let parent = node.parent().unwrap();
            // Identifiers and qualified names may be type nodes, depending on their context. Climb
            // above them to find the lowest container
            if let BoundNode::Ident(i) = &node {
                // If the identifier is the RHS of a qualified name, then it's a type iff its parent is.
                if matches!(&parent, BoundNode::TsQualifiedName(n) if n.right == i.node) {
                    node = parent;
                } else if matches!(&parent, BoundNode::MemberExpr(m) if !m.computed && m.prop.bind(parent.clone()) == node)
                {
                    node = parent;
                }
                // At this point, node is either a qualified name or an identifier
                debug_assert!(matches!(node,BoundNode::Ident(_) | BoundNode::TsQualifiedName(_) | BoundNode::MemberExpr(_)),
                "'node' was expected to be a qualified name, identifier or property access in 'isPartOfTypeNode'.");
            }
            let parent = node.parent().unwrap();
            if matches!(parent, BoundNode::TsTypeQuery(_)) {
                return false;
            }
            if let BoundNode::TsImportType(_) = parent {
                todo!();
                // return !(parent as ImportTypeNode).isTypeOf;
            }
            // Do not recursively call isPartOfTypeNode on the parent. In the example:
            //
            //     let a: A.B.C;
            //
            // Calling isPartOfTypeNode would consider the qualified name A.B a type node.
            // Only C and A.B.C are type nodes.
            match &parent {
                BoundNode::TsTypePredicate(_)
                | BoundNode::TsTypeRef(_)
                | BoundNode::TsFnType(_)
                | BoundNode::TsConstructorType(_)
                | BoundNode::TsTypeQuery(_)
                | BoundNode::TsTypeLit(_)
                | BoundNode::TsArrayType(_)
                | BoundNode::TsTupleType(_)
                | BoundNode::TsOptionalType(_)
                | BoundNode::TsRestType(_)
                | BoundNode::TsUnionType(_)
                | BoundNode::TsIntersectionType(_)
                | BoundNode::TsConditionalType(_)
                | BoundNode::TsInferType(_)
                | BoundNode::TsParenthesizedType(_)
                | BoundNode::TsThisType(_)
                | BoundNode::TsTypeOperator(_)
                | BoundNode::TsIndexedAccessType(_)
                | BoundNode::TsMappedType(_)
                | BoundNode::TsLitType(_)
                | BoundNode::TsTupleElement(_)
                | BoundNode::TsTplLitType(_)
                | BoundNode::TsImportType(_) => true,

                BoundNode::TsExprWithTypeArgs(_) => {
                    // TODO:
                    true
                    // return !isExpressionWithTypeArgumentsInClassExtendsClause(node);
                }
                BoundNode::TsTypeParam(n) => {
                    Some(node) == n.constraint.as_ref().map(|t| t.bind(parent.clone()))
                }
                // TODO: jsdoc:
                // BoundNode::JSDocTemplateTag(_) => {
                //     return node == (parent as JSDocTemplateTag).constraint;
                // }
                BoundNode::ClassProp(n) => {
                    Some(node) == n.type_ann.as_ref().map(|t| t.bind(parent.clone()))
                }
                BoundNode::PrivateProp(n) => {
                    Some(node) == n.type_ann.as_ref().map(|t| t.bind(parent.clone()))
                }
                BoundNode::TsPropertySignature(n) => {
                    Some(node) == n.type_ann.as_ref().map(|t| t.bind(parent.clone()))
                }
                BoundNode::Param(n) => {
                    let ty = match &n.pat {
                        ast::Pat::Ident(p) => p.type_ann.as_ref(),
                        ast::Pat::Array(p) => p.type_ann.as_ref(),
                        ast::Pat::Rest(p) => p.type_ann.as_ref(),
                        ast::Pat::Object(p) => p.type_ann.as_ref(),
                        ast::Pat::Assign(p) => p.type_ann.as_ref(),
                        _ => None,
                    };
                    Some(node) == ty.map(|t| t.bind(n.pat.bind(parent.clone())))
                }
                BoundNode::ParamWithoutDecorators(n) => {
                    let ty = match &n.pat {
                        ast::Pat::Ident(p) => p.type_ann.as_ref(),
                        ast::Pat::Array(p) => p.type_ann.as_ref(),
                        ast::Pat::Rest(p) => p.type_ann.as_ref(),
                        ast::Pat::Object(p) => p.type_ann.as_ref(),
                        ast::Pat::Assign(p) => p.type_ann.as_ref(),
                        _ => None,
                    };
                    Some(node) == ty.map(|t| t.bind(n.pat.bind(parent.clone())))
                }
                BoundNode::TsAmbientParam(n) => {
                    let ty = match &n.pat {
                        ast::TsAmbientParamPat::Ident(p) => p.type_ann.as_ref(),
                        ast::TsAmbientParamPat::Array(p) => p.type_ann.as_ref(),
                        ast::TsAmbientParamPat::Rest(p) => p.type_ann.as_ref(),
                        ast::TsAmbientParamPat::Object(p) => p.type_ann.as_ref(),
                    };
                    Some(node) == ty.map(|t| t.bind(n.pat.bind(parent.clone())))
                }
                BoundNode::TsParamProp(n) => {
                    let ty = match &n.param {
                        ast::TsParamPropParam::Ident(p) => p.type_ann.as_ref(),
                        ast::TsParamPropParam::Assign(p) => p.type_ann.as_ref(),
                    };
                    Some(node) == ty.map(|t| t.bind(n.param.bind(parent.clone())))
                }
                BoundNode::VarDeclarator(n) => {
                    let ty = match &n.name {
                        ast::Pat::Ident(p) => p.type_ann.as_ref(),
                        ast::Pat::Array(p) => p.type_ann.as_ref(),
                        ast::Pat::Rest(p) => p.type_ann.as_ref(),
                        ast::Pat::Object(p) => p.type_ann.as_ref(),
                        ast::Pat::Assign(p) => p.type_ann.as_ref(),
                        _ => None,
                    };
                    Some(node) == ty.map(|t| t.bind(n.name.bind(parent.clone())))
                }
                BoundNode::FnDecl(n) => {
                    Some(node)
                        == n.function
                            .return_type
                            .as_ref()
                            .map(|t| t.bind(n.function.bind(parent.clone())))
                }
                BoundNode::FnExpr(n) => {
                    Some(node)
                        == n.function
                            .return_type
                            .as_ref()
                            .map(|t| t.bind(n.function.bind(parent.clone())))
                }
                BoundNode::ArrowExpr(n) => {
                    Some(node) == n.return_type.as_ref().map(|t| t.bind(parent.clone()))
                }
                BoundNode::PrivateMethod(n) => {
                    Some(node)
                        == n.function
                            .return_type
                            .as_ref()
                            .map(|t| t.bind(n.function.bind(parent.clone())))
                }
                BoundNode::ClassMethod(n) => {
                    Some(node)
                        == n.function
                            .return_type
                            .as_ref()
                            .map(|t| t.bind(n.function.bind(parent.clone())))
                }
                BoundNode::MethodProp(n) => {
                    Some(node)
                        == n.function
                            .return_type
                            .as_ref()
                            .map(|t| t.bind(n.function.bind(parent.clone())))
                }
                BoundNode::TsMethodSignature(n) => {
                    Some(node) == n.type_ann.as_ref().map(|t| t.bind(parent.clone()))
                }
                BoundNode::GetterProp(n) => {
                    Some(node) == n.type_ann.as_ref().map(|t| t.bind(parent.clone()))
                }
                BoundNode::TsGetterSignature(n) => {
                    Some(node) == n.type_ann.as_ref().map(|t| t.bind(parent.clone()))
                }
                BoundNode::TsCallSignatureDecl(n) => {
                    Some(node) == n.type_ann.as_ref().map(|t| t.bind(parent.clone()))
                }
                BoundNode::TsConstructSignatureDecl(n) => {
                    Some(node) == n.type_ann.as_ref().map(|t| t.bind(parent.clone()))
                }
                BoundNode::TsIndexSignature(n) => {
                    Some(node) == n.type_ann.as_ref().map(|t| t.bind(parent.clone()))
                }
                BoundNode::TsTypeAssertion(n) => node == n.type_ann.bind(parent.clone()),
                BoundNode::CallExpr(n) => n
                    .type_args
                    .as_ref()
                    .map(|args| {
                        args.params
                            .iter()
                            .any(|a| a.bind(args.bind(parent.clone())) == node)
                    })
                    .unwrap_or_default(),
                BoundNode::NewExpr(n) => n
                    .type_args
                    .as_ref()
                    .map(|args| {
                        args.params
                            .iter()
                            .any(|a| a.bind(args.bind(parent.clone())) == node)
                    })
                    .unwrap_or_default(),
                BoundNode::TaggedTpl(_) => {
                    // TODO (drosen): TaggedTemplateExpressions may eventually support type arguments.
                    false
                }
                _ => false,
            }
        }
        _ => false,
    }
}

// Warning: This has the same semantics as the forEach family of functions,
//          in that traversal terminates in the event that 'visitor' supplies a truthy value.
pub fn forEachReturnStatement(body: &BoundNode, visit_fn: impl FnMut(Rc<ReturnStmt>)) {
    let mut visitor = Visitor { visit_fn };

    return body.visit_with(&mut visitor);

    struct Visitor<F: FnMut(Rc<ReturnStmt>)> {
        visit_fn: F,
    }

    macro_rules! generate_noop_visitors {
        ([$([$name:ident, $N:ident]$(,)?)*]) => {
            $(
                #[inline]
                fn $name(&mut self, _n: &Rc<ast::$N>, _parent: Option<BoundNode>) {}
            )*

        };
    }

    impl<F: FnMut(Rc<ReturnStmt>)> Visit for Visitor<F> {
        fn visit_return_stmt(&mut self, n: &Rc<ast::ReturnStmt>, parent: Option<BoundNode>) {
            (self.visit_fn)(ReturnStmt::new(n.clone(), parent))
        }

        // fn visit_block_stmt(&mut self, n: &Rc<ast::BlockStmt>, parent: Option<BoundNode>) {}
        // fn visit_if_stmt(&mut self, n: &Rc<ast::IfStmt>, parent: Option<BoundNode>) {}
        // fn visit_do_while_stmt(&mut self, n: &Rc<ast::DoWhileStmt>, parent: Option<BoundNode>) {}
        // fn visit_while_stmt(&mut self, n: &Rc<ast::WhileStmt>, parent: Option<BoundNode>) {}
        // fn visit_for_stmt(&mut self, n: &Rc<ast::ForStmt>, parent: Option<BoundNode>) {}
        // fn visit_for_in_stmt(&mut self, n: &Rc<ast::ForInStmt>, parent: Option<BoundNode>) {}
        // fn visit_for_of_stmt(&mut self, n: &Rc<ast::ForOfStmt>, parent: Option<BoundNode>) {}
        // fn visit_with_stmt(&mut self, n: &Rc<ast::WithStmt>, parent: Option<BoundNode>) {}
        // fn visit_switch_stmt(&mut self, n: &Rc<ast::SwitchStmt>, parent: Option<BoundNode>) {}
        // fn visit_switch_case(&mut self, n: &Rc<ast::SwitchCase>, parent: Option<BoundNode>) {}
        // fn visit_labeled_stmt(&mut self, n: &Rc<ast::LabeledStmt>, parent: Option<BoundNode>) {}
        // fn visit_try_stmt(&mut self, n: &Rc<ast::TryStmt>, parent: Option<BoundNode>) {}
        // fn visit_catch_clause(&mut self, n: &Rc<ast::CatchClause>, parent: Option<BoundNode>) {}

        generate_noop_visitors!([
            [visit_class, Class],
            [visit_class_prop, ClassProp],
            [visit_private_prop, PrivateProp],
            [visit_class_method, ClassMethod],
            [visit_private_method, PrivateMethod],
            [visit_constructor, Constructor],
            [visit_decorator, Decorator],
            [visit_fn_decl, FnDecl],
            [visit_class_decl, ClassDecl],
            [visit_var_decl, VarDecl],
            [visit_var_declarator, VarDeclarator],
            [visit_this_expr, ThisExpr],
            [visit_array_lit, ArrayLit],
            [visit_object_lit, ObjectLit],
            [visit_spread_element, SpreadElement],
            [visit_unary_expr, UnaryExpr],
            [visit_update_expr, UpdateExpr],
            [visit_bin_expr, BinExpr],
            [visit_fn_expr, FnExpr],
            [visit_class_expr, ClassExpr],
            [visit_assign_expr, AssignExpr],
            [visit_member_expr, MemberExpr],
            [visit_cond_expr, CondExpr],
            [visit_call_expr, CallExpr],
            [visit_new_expr, NewExpr],
            [visit_seq_expr, SeqExpr],
            [visit_arrow_expr, ArrowExpr],
            [visit_yield_expr, YieldExpr],
            [visit_meta_prop_expr, MetaPropExpr],
            [visit_await_expr, AwaitExpr],
            [visit_tpl, Tpl],
            [visit_tagged_tpl, TaggedTpl],
            [visit_tpl_element, TplElement],
            [visit_paren_expr, ParenExpr],
            [visit_super, Super],
            [visit_opt_chain_expr, OptChainExpr],
            [visit_function, Function],
            [visit_param, Param],
            [visit_param_without_decorators, ParamWithoutDecorators],
            [visit_binding_ident, BindingIdent],
            [visit_ident, Ident],
            [visit_private_name, PrivateName],
            [visit_jsx_member_expr, JSXMemberExpr],
            [visit_jsx_namespaced_name, JSXNamespacedName],
            [visit_jsx_empty_expr, JSXEmptyExpr],
            [visit_jsx_expr_container, JSXExprContainer],
            [visit_jsx_spread_child, JSXSpreadChild],
            [visit_jsx_opening_element, JSXOpeningElement],
            [visit_jsx_closing_element, JSXClosingElement],
            [visit_jsx_attr, JSXAttr],
            [visit_jsx_text, JSXText],
            [visit_jsx_element, JSXElement],
            [visit_jsx_fragment, JSXFragment],
            [visit_jsx_opening_fragment, JSXOpeningFragment],
            [visit_jsx_closing_fragment, JSXClosingFragment],
            [visit_invalid, Invalid],
            [visit_str, Str],
            [visit_bool, Bool],
            [visit_null, Null],
            [visit_number, Number],
            [visit_big_int, BigInt],
            [visit_regex, Regex],
            [visit_export_default_expr, ExportDefaultExpr],
            [visit_export_decl, ExportDecl],
            [visit_import_decl, ImportDecl],
            [visit_export_all, ExportAll],
            [visit_named_export, NamedExport],
            [visit_export_default_decl, ExportDefaultDecl],
            [visit_import_default_specifier, ImportDefaultSpecifier],
            [visit_import_star_as_specifier, ImportStarAsSpecifier],
            [visit_import_named_specifier, ImportNamedSpecifier],
            [visit_export_namespace_specifier, ExportNamespaceSpecifier],
            [visit_export_default_specifier, ExportDefaultSpecifier],
            [visit_export_named_specifier, ExportNamedSpecifier],
            [visit_script, Script],
            [visit_module, Module],
            [visit_array_pat, ArrayPat],
            [visit_object_pat, ObjectPat],
            [visit_assign_pat, AssignPat],
            [visit_rest_pat, RestPat],
            [visit_key_value_pat_prop, KeyValuePatProp],
            [visit_assign_pat_prop, AssignPatProp],
            [visit_key_value_prop, KeyValueProp],
            [visit_assign_prop, AssignProp],
            [visit_getter_prop, GetterProp],
            [visit_setter_prop, SetterProp],
            [visit_method_prop, MethodProp],
            [visit_computed_prop_name, ComputedPropName],
            [visit_spread_assignment, SpreadAssignment],
            [visit_expr_stmt, ExprStmt],
            [visit_empty_stmt, EmptyStmt],
            [visit_debugger_stmt, DebuggerStmt],
            [visit_break_stmt, BreakStmt],
            [visit_continue_stmt, ContinueStmt],
            [visit_throw_stmt, ThrowStmt],
            [visit_ts_type_ann, TsTypeAnn],
            [visit_ts_type_param_decl, TsTypeParamDecl],
            [visit_ts_type_param, TsTypeParam],
            [visit_ts_type_param_instantiation, TsTypeParamInstantiation],
            [visit_ts_param_prop, TsParamProp],
            [visit_ts_qualified_name, TsQualifiedName],
            [visit_ts_call_signature_decl, TsCallSignatureDecl],
            [visit_ts_construct_signature_decl, TsConstructSignatureDecl],
            [visit_ts_property_signature, TsPropertySignature],
            [visit_ts_getter_signature, TsGetterSignature],
            [visit_ts_setter_signature, TsSetterSignature],
            [visit_ts_method_signature, TsMethodSignature],
            [visit_ts_index_signature, TsIndexSignature],
            [visit_ts_keyword_type, TsKeywordType],
            [visit_ts_this_type, TsThisType],
            [visit_ts_ambient_param, TsAmbientParam],
            [visit_ts_fn_type, TsFnType],
            [visit_ts_constructor_type, TsConstructorType],
            [visit_ts_type_ref, TsTypeRef],
            [visit_ts_type_predicate, TsTypePredicate],
            [visit_ts_type_query, TsTypeQuery],
            [visit_ts_import_type, TsImportType],
            [visit_ts_type_lit, TsTypeLit],
            [visit_ts_array_type, TsArrayType],
            [visit_ts_tuple_type, TsTupleType],
            [visit_ts_tuple_element, TsTupleElement],
            [visit_ts_optional_type, TsOptionalType],
            [visit_ts_rest_type, TsRestType],
            [visit_ts_union_type, TsUnionType],
            [visit_ts_intersection_type, TsIntersectionType],
            [visit_ts_conditional_type, TsConditionalType],
            [visit_ts_infer_type, TsInferType],
            [visit_ts_parenthesized_type, TsParenthesizedType],
            [visit_ts_type_operator, TsTypeOperator],
            [visit_ts_indexed_access_type, TsIndexedAccessType],
            [visit_ts_mapped_type, TsMappedType],
            [visit_ts_lit_type, TsLitType],
            [visit_ts_tpl_lit_type, TsTplLitType],
            [visit_ts_interface_decl, TsInterfaceDecl],
            [visit_ts_interface_body, TsInterfaceBody],
            [visit_ts_expr_with_type_args, TsExprWithTypeArgs],
            [visit_ts_type_alias_decl, TsTypeAliasDecl],
            [visit_ts_enum_decl, TsEnumDecl],
            [visit_ts_enum_member, TsEnumMember],
            [visit_ts_module_decl, TsModuleDecl],
            [visit_ts_module_block, TsModuleBlock],
            [visit_ts_namespace_decl, TsNamespaceDecl],
            [visit_ts_import_equals_decl, TsImportEqualsDecl],
            [visit_ts_external_module_ref, TsExternalModuleRef],
            [visit_ts_export_assignment, TsExportAssignment],
            [visit_ts_namespace_export_decl, TsNamespaceExportDecl],
            [visit_ts_as_expr, TsAsExpr],
            [visit_ts_type_assertion, TsTypeAssertion],
            [visit_ts_non_null_expr, TsNonNullExpr],
            [visit_ts_const_assertion, TsConstAssertion],
        ]);
    }
}

pub fn getContainingFunction(node: &BoundNode) -> Option<BoundNode> {
    findAncestor(node.parent(), |n| Some(isFunctionLike(Some(n))))
}

pub fn getThisContainer(mut node: BoundNode, includeArrowFunctions: bool) -> BoundNode {
    debug_assert!(!matches!(node, BoundNode::Script(_) | BoundNode::Module(_)));
    loop {
        // If we never pass in a SourceFile, this should be always be Some, since we'll stop when we reach that.
        node = node.parent().unwrap();
        match node {
            BoundNode::ComputedPropName(_) => {
                let parent = node.parent().unwrap();
                // If the grandparent node is an object literal (as opposed to a class),
                // then the computed property is not a 'this' container.
                // A computed property name in a class needs to be a this container
                // so that we can error on it.
                if let Some(grandparent) = parent.parent() {
                    if isClassLike(&grandparent) {
                        return node;
                    }
                }
                // If this is a computed property, then the parent should not
                // make it a this container. The parent might be a property
                // in an object literal, like a method or accessor. But in order for
                // such a parent to be a this container, the reference must be in
                // the *body* of the container.
                node = parent;
            }
            BoundNode::Decorator(_) => {
                // Decorators are always applied outside of the body of a class or method.
                if let Some(parent) = node.parent() {
                    // TODO: this matches!() can be refined because we then call isClassElement
                    if matches!(
                        parent,
                        BoundNode::Param(_)
                            | BoundNode::ParamWithoutDecorators(_)
                            | BoundNode::TsAmbientParam(_)
                            | BoundNode::TsParamProp(_)
                    ) {
                        let grandparent = parent.parent().unwrap();
                        if isClassElement(&grandparent) {
                            // If the decorator's parent is a Parameter, we resolve the this container from
                            // the grandparent class declaration.
                            node = grandparent;
                            continue;
                        }
                    }

                    if isClassElement(&parent) {
                        // If the decorator's parent is a class element, we resolve the 'this' container
                        // from the parent class declaration.
                        node = parent;
                    }
                }
            }
            BoundNode::ArrowExpr(_) => {
                if !includeArrowFunctions {
                    continue;
                }
                return node;
            }

            // TODO: class static block:
            // | BoundNode::ClassStaticBlockDeclaration(_)
            BoundNode::FnDecl(_)
            | BoundNode::FnExpr(_)
            | BoundNode::TsModuleDecl(_)
            | BoundNode::ClassProp(_)
            | BoundNode::PrivateProp(_)
            | BoundNode::TsPropertySignature(_)
            | BoundNode::PrivateMethod(_)
            | BoundNode::ClassMethod(_)
            | BoundNode::MethodProp(_)
            | BoundNode::TsMethodSignature(_)
            | BoundNode::Constructor(_)
            | BoundNode::TsCallSignatureDecl(_)
            | BoundNode::TsConstructSignatureDecl(_)
            | BoundNode::TsIndexSignature(_)
            | BoundNode::TsEnumDecl(_)
            | BoundNode::Script(_)
            | BoundNode::Module(_) => {
                return node;
            }
            _ => continue,
        }
    }
}

/**
 * Gets the effective type parameters. If the node was parsed in a
 * JavaScript file, gets the type parameters from the `@template` tag from JSDoc.
 */
pub fn getBoundEffectiveTypeParameterDeclarations(node: BoundNode) -> Vec<BoundNode> {
    // TODO: jsdoc:
    // if isJSDocSignature(node) {
    //     return Vec::new();
    // }
    // if isJSDocTypeAlias(node) {
    //     Debug.assert(node.parent.kind == SyntaxKind.JSDocComment);
    //     return flatMap(node.parent.tags, tag => isJSDocTemplateTag(tag) ? tag.typeParameters : undefined);
    // }
    macro_rules! extract_type_params {
        ($type_param_parent:expr, $type_params:expr) => {{
            if let Some(type_params) = &$type_params {
                return type_params
                    .params
                    .iter()
                    .map(|p| p.bind_to_opt_parent($type_param_parent))
                    .collect::<Vec<_>>();
            }
        }};
    }
    match &node {
        // TODO: jsdoc
        // BoundNode::JSDocTemplateTag(_)
        // | BoundNode::JSDocTypedefTag(_)
        // | BoundNode::JSDocCallbackTag(_)
        // | BoundNode::JSDocSignature(_)
        // | BoundNode::JSDocFunctionType(_)
        BoundNode::TsCallSignatureDecl(n) => {
            extract_type_params!(
                n.type_params.as_ref().map(|p| p.bind(node.clone())),
                n.type_params
            )
        }
        BoundNode::TsConstructSignatureDecl(n) => {
            extract_type_params!(
                n.type_params.as_ref().map(|p| p.bind(node.clone())),
                n.type_params
            )
        }
        BoundNode::TsMethodSignature(n) => extract_type_params!(
            n.type_params.as_ref().map(|p| p.bind(node.clone())),
            n.type_params
        ),
        BoundNode::TsFnType(n) => extract_type_params!(
            n.type_params.as_ref().map(|p| p.bind(node.clone())),
            n.type_params
        ),
        BoundNode::TsConstructorType(n) => extract_type_params!(
            n.type_params.as_ref().map(|p| p.bind(node.clone())),
            n.type_params
        ),
        BoundNode::FnDecl(n) => {
            extract_type_params!(
                n.function
                    .type_params
                    .as_ref()
                    .map(|p| p.bind(n.function.bind(node.clone()))),
                n.function.type_params
            )
        }
        BoundNode::PrivateMethod(n) => {
            extract_type_params!(
                n.function
                    .type_params
                    .as_ref()
                    .map(|p| p.bind(n.function.bind(node.clone()))),
                n.function.type_params
            )
        }
        BoundNode::ClassMethod(n) => {
            extract_type_params!(
                n.function
                    .type_params
                    .as_ref()
                    .map(|p| p.bind(n.function.bind(node.clone()))),
                n.function.type_params
            )
        }
        BoundNode::MethodProp(n) => {
            extract_type_params!(
                n.function
                    .type_params
                    .as_ref()
                    .map(|p| p.bind(n.function.bind(node.clone()))),
                n.function.type_params
            )
        }
        BoundNode::FnExpr(n) => {
            extract_type_params!(
                n.function
                    .type_params
                    .as_ref()
                    .map(|p| p.bind(n.function.bind(node.clone()))),
                n.function.type_params
            )
        }
        BoundNode::ArrowExpr(n) => extract_type_params!(
            n.type_params.as_ref().map(|p| p.bind(node.clone())),
            n.type_params
        ),
        BoundNode::ClassDecl(n) => {
            extract_type_params!(
                n.class
                    .type_params
                    .as_ref()
                    .map(|p| p.bind(n.class.bind(node.clone()))),
                n.class.type_params
            )
        }
        BoundNode::ClassExpr(n) => {
            extract_type_params!(
                n.class
                    .type_params
                    .as_ref()
                    .map(|p| p.bind(n.class.bind(node.clone()))),
                n.class.type_params
            )
        }
        BoundNode::TsInterfaceDecl(n) => {
            extract_type_params!(
                n.type_params.as_ref().map(|p| p.bind(node.clone())),
                n.type_params
            )
        }
        BoundNode::TsTypeAliasDecl(n) => extract_type_params!(
            n.type_params.as_ref().map(|p| p.bind(node.clone())),
            n.type_params
        ),
        _ => {}
    }
    // TODO: jsdoc:
    // if isBoundNodeInJSFile(node) {
    //     const decls = getJSDocTypeParameterDeclarations(node);
    //     if (decls.length) {
    //         return decls;
    //     }
    //     const typeTag = getJSDocType(node);
    //     if (typeTag && isFunctionTypeNode(typeTag) && typeTag.typeParameters) {
    //         return typeTag.typeParameters;
    //     }
    // }
    Vec::new()
}
/**
 * Gets the effective type parameters. If the node was parsed in a
 * JavaScript file, gets the type parameters from the `@template` tag from JSDoc.
 */
pub fn getEffectiveTypeParameterDeclarations<'a>(
    node: &'a Node,
) -> Option<&'a Vec<Rc<ast::TsTypeParam>>> {
    // TODO: jsdoc:
    // if isJSDocSignature(node) {
    //     return Vec::new();
    // }
    // if isJSDocTypeAlias(node) {
    //     Debug.assert(node.parent.kind == SyntaxKind.JSDocComment);
    //     return flatMap(node.parent.tags, tag => isJSDocTemplateTag(tag) ? tag.typeParameters : undefined);
    // }
    macro_rules! extract_type_params {
        ($type_params:expr) => {{
            return $type_params.as_ref().map(|d| &d.params);
        }};
    }
    match &node {
        // TODO: jsdoc
        // BoundNode::JSDocTemplateTag(_)
        // | BoundNode::JSDocTypedefTag(_)
        // | BoundNode::JSDocCallbackTag(_)
        // | BoundNode::JSDocSignature(_)
        // | BoundNode::JSDocFunctionType(_)
        Node::TsCallSignatureDecl(n) => extract_type_params!(n.type_params),
        Node::TsConstructSignatureDecl(n) => extract_type_params!(n.type_params),
        Node::TsMethodSignature(n) => extract_type_params!(n.type_params),
        Node::TsFnType(n) => extract_type_params!(n.type_params),
        Node::TsConstructorType(n) => extract_type_params!(n.type_params),
        Node::FnDecl(n) => {
            extract_type_params!(n.function.type_params)
        }
        Node::PrivateMethod(n) => {
            extract_type_params!(n.function.type_params)
        }
        Node::ClassMethod(n) => {
            extract_type_params!(n.function.type_params)
        }
        Node::MethodProp(n) => {
            extract_type_params!(n.function.type_params)
        }
        Node::FnExpr(n) => {
            extract_type_params!(n.function.type_params)
        }
        Node::ArrowExpr(n) => extract_type_params!(n.type_params),
        Node::ClassDecl(n) => {
            extract_type_params!(n.class.type_params)
        }
        Node::ClassExpr(n) => {
            extract_type_params!(n.class.type_params)
        }
        Node::TsInterfaceDecl(n) => extract_type_params!(n.type_params),
        Node::TsTypeAliasDecl(n) => extract_type_params!(n.type_params),
        _ => {}
    }
    // TODO: jsdoc:
    // if isBoundNodeInJSFile(node) {
    //     const decls = getJSDocTypeParameterDeclarations(node);
    //     if (decls.length) {
    //         return decls;
    //     }
    //     const typeTag = getJSDocType(node);
    //     if (typeTag && isFunctionTypeNode(typeTag) && typeTag.typeParameters) {
    //         return typeTag.typeParameters;
    //     }
    // }
    None
}

// pub fn isCallChain(node: &BoundNode) -> bool {
//     // TODO:
//     matches!(node, BoundNode::CallExpr(_))
//         && matches!(node.parent(), Some(BoundNode::OptChainExpr(_))) /*&& !!(node.flags & NodeFlags.OptionalChain)*/
// }

// pub fn isOptionalChain(node: &BoundNode) -> bool {
//         // TODO:
//         matches!(node, BoundNode::OptChainExpr(_))
//     // const kind = node.kind;
//     // return !!(node.flags & NodeFlags.OptionalChain) &&
//     //     (kind === SyntaxKind.PropertyAccessExpression
//     //         || kind === SyntaxKind.ElementAccessExpression
//     //         || kind === SyntaxKind.CallExpression
//     //         || kind === SyntaxKind.NonNullExpression);
// }

// fn isOptionalChainRoot(node: &BoundNode) -> bool {
//     // TODO:
//     isOptionalChain(node)
//     // return isOptionalChain(node) && !isNonNullExpression(node) && !!node.questionDotToken;
// }

// /// Determines whether a node is the expression preceding an optional chain (i.e. `a` in `a?.b`).
// pub fn isExpressionOfOptionalChainRoot(node: &BoundNode) -> bool {
//     // todo:
//     matches!(node.parent(), Some(BoundNode::OptChainExpr(_)))
//     // isOptionalChainRoot(node.parent) && node.parent.expression == node
// }

//     /**
//      * Determines whether a node is the outermost `OptionalChain` in an ECMAScript `OptionalExpression`:
//      *
//      * 1. For `a?.b.c`, the outermost chain is `a?.b.c` (`c` is the end of the chain starting at `a?.`)
//      * 2. For `a?.b!`, the outermost chain is `a?.b` (`b` is the end of the chain starting at `a?.`)
//      * 3. For `(a?.b.c).d`, the outermost chain is `a?.b.c` (`c` is the end of the chain starting at `a?.` since parens end the chain)
//      * 4. For `a?.b.c?.d`, both `a?.b.c` and `a?.b.c?.d` are outermost (`c` is the end of the chain starting at `a?.`, and `d` is
//      *   the end of the chain starting at `c?.`)
//      * 5. For `a?.(b?.c).d`, both `b?.c` and `a?.(b?.c)d` are outermost (`c` is the end of the chain starting at `b`, and `d` is
//      *   the end of the chain starting at `a?.`)
//      */
//     pub fn isOutermostOptionalChain(node: OptionalChain) {
//         // todo
//         return !isOptionalChain(node.parent) // cases 1, 2, and 3
//             || isOptionalChainRoot(node.parent) // case 4
//             || node !== node.parent.expression; // case 5
//         // return !isOptionalChain(node.parent) // cases 1, 2, and 3
//         //     || isOptionalChainRoot(node.parent) // case 4
//         //     || node !== node.parent.expression; // case 5
//     }

pub fn isValueSignatureDeclaration(node: &BoundNode) -> bool {
    matches!(
        node,
        BoundNode::FnExpr(_)
            | BoundNode::ArrowExpr(_)
            | BoundNode::PrivateMethod(_)
            | BoundNode::ClassMethod(_)
            | BoundNode::MethodProp(_)
            | BoundNode::GetterProp(_)
            | BoundNode::SetterProp(_)
            | BoundNode::FnDecl(_)
            | BoundNode::Constructor(_)
    )
}

pub fn hasRestParameter(s: &BoundNode) -> bool {
    let last = match s {
        // todo: jsdoc
        // BoundNode::JSDocSignature
        // | BoundNode::JSDocFunctionType(_)
        BoundNode::TsCallSignatureDecl(n) => n.params.last().map(|p| p.pat.clone().into()),
        BoundNode::TsConstructSignatureDecl(n) => n.params.last().map(|p| p.pat.clone().into()),
        BoundNode::TsMethodSignature(n) => n.params.last().map(|p| p.pat.clone().into()),
        BoundNode::TsIndexSignature(n) => n.params.last().map(|p| p.pat.clone().into()),
        BoundNode::TsFnType(n) => n.params.last().map(|p| p.pat.clone().into()),
        BoundNode::TsConstructorType(n) => n.params.last().map(|p| p.pat.clone().into()),
        BoundNode::FnDecl(n) => n.function.params.last().map(|p| p.pat.clone()),
        BoundNode::PrivateMethod(n) => n.function.params.last().map(|p| p.pat.clone()),
        BoundNode::ClassMethod(n) => n.function.params.last().map(|p| p.pat.clone()),
        BoundNode::MethodProp(n) => n.function.params.last().map(|p| p.pat.clone()),
        BoundNode::Constructor(n) => {
            if let Some(ast::ParamOrTsParamProp::Param(p)) = n.params.last() {
                Some(p.pat.clone())
            } else {
                None
            }
        }
        BoundNode::SetterProp(n) => Some(n.param.pat.clone()),
        BoundNode::FnExpr(n) => n.function.params.last().map(|p| p.pat.clone()),
        BoundNode::ArrowExpr(n) => n.params.last().map(|p| p.pat.clone()),
        _ => None,
    };
    if let Some(last) = last {
        isRestParameter(last)
    } else {
        false
    }
}

// TODO: jsdoc:
// fn isRestParameter(node: ParameterDeclaration | JSDocParameterTag) -> bool {
//     let ty = isJSDocParameterTag(node) ? (node.typeExpression && node.typeExpression.type) : node.type;
//     return (node as ParameterDeclaration).dotDotDotToken !== undefined || !!ty && ty.kind === SyntaxKind.JSDocVariadicType;
// }
fn isRestParameter(node: ast::Pat) -> bool {
    matches!(node, ast::Pat::Rest(_))
}

pub fn getCheckFlags(symbol: &Symbol) -> CheckFlags {
    match symbol {
        Symbol::Base(_) => CheckFlags::default(),
        Symbol::TransientSymbol(s) => s.check_flags,
    }
}

pub fn isInTypeQuery(node: BoundNode) -> bool {
    // TypeScript 1.0 spec (April 2014): 3.6.3
    // A type query consists of the keyword typeof followed by an expression.
    // The expression is restricted to a single identifier or a sequence of identifiers separated by periods
    findAncestor(Some(node), |n| {
        if matches!(n, BoundNode::TsTypeQuery(_)) {
            Some(true)
        } else if matches!(n, BoundNode::Ident(_) | BoundNode::TsQualifiedName(_)) {
            Some(false)
        } else {
            None
        }
    })
    .is_some()
}

pub fn hasThisParameter(signature: &BoundNode) -> bool {
    let first = match signature {
        // todo: jsdoc
        // BoundNode::JSDocSignature
        // | BoundNode::JSDocFunctionType(_)
        BoundNode::TsCallSignatureDecl(n) => n.params.first().map(|p| p.pat.clone().into()),
        BoundNode::TsConstructSignatureDecl(n) => n.params.first().map(|p| p.pat.clone().into()),
        BoundNode::TsMethodSignature(n) => n.params.first().map(|p| p.pat.clone().into()),
        BoundNode::TsIndexSignature(n) => n.params.first().map(|p| p.pat.clone().into()),
        BoundNode::TsFnType(n) => n.params.first().map(|p| p.pat.clone().into()),
        BoundNode::TsConstructorType(n) => n.params.first().map(|p| p.pat.clone().into()),
        BoundNode::FnDecl(n) => n.function.params.first().map(|p| p.pat.clone()),
        BoundNode::PrivateMethod(n) => n.function.params.first().map(|p| p.pat.clone()),
        BoundNode::ClassMethod(n) => n.function.params.first().map(|p| p.pat.clone()),
        BoundNode::MethodProp(n) => n.function.params.first().map(|p| p.pat.clone()),
        BoundNode::Constructor(n) => {
            if let Some(ast::ParamOrTsParamProp::Param(p)) = n.params.first() {
                Some(p.pat.clone())
            } else {
                None
            }
        }
        BoundNode::SetterProp(n) => Some(n.param.pat.clone()),
        BoundNode::FnExpr(n) => n.function.params.first().map(|p| p.pat.clone()),
        BoundNode::ArrowExpr(n) => n.params.first().map(|p| p.pat.clone()),
        _ => None,
    };
    // callback tags do not currently support this parameters
    // TODO: jsdoc:
    // if (signature.parameters.length && !isJSDocSignature(signature)) {
    if let Some(first) = first {
        if parameterIsThisKeyword(&first) {
            return true;
        }
    }
    false
}

pub fn parameterIsThisKeyword(parameter: &ast::Pat) -> bool {
    if let ast::Pat::Ident(i) = parameter {
        return i.sym == js_word!("this");
    }
    false
}

pub fn hasContextSensitiveParameters(node: &Node) -> bool {
    macro_rules! has_param_without_type_ann {
        ($params:expr) => {
            $params
                .iter()
                .any(|p| getEffectiveTypeAnnotationNode(&p.clone().into()).is_none())
        };
    }
    let (first_param, has_param_without_type_ann, has_type_params) = match node {
        Node::FnDecl(n) => (
            n.function.params.first().map(|p| p.pat.clone()),
            has_param_without_type_ann!(n.function.params),
            n.function.type_params.is_some(),
        ),
        Node::PrivateMethod(n) => (
            n.function.params.first().map(|p| p.pat.clone()),
            has_param_without_type_ann!(n.function.params),
            n.function.type_params.is_some(),
        ),
        Node::ClassMethod(n) => (
            n.function.params.first().map(|p| p.pat.clone()),
            has_param_without_type_ann!(n.function.params),
            n.function.type_params.is_some(),
        ),
        Node::MethodProp(n) => (
            n.function.params.first().map(|p| p.pat.clone()),
            has_param_without_type_ann!(n.function.params),
            n.function.type_params.is_some(),
        ),
        Node::GetterProp(_) => (None, false, false),
        Node::SetterProp(n) => (
            Some(n.param.pat.clone()),
            getEffectiveTypeAnnotationNode(&n.param.clone().into()).is_none(),
            false,
        ),
        Node::Constructor(n) => (
            n.params.first().map(|p| match p {
                ast::ParamOrTsParamProp::TsParamProp(p) => match &p.param {
                    ast::TsParamPropParam::Ident(n) => ast::Pat::Ident(n.clone()),
                    ast::TsParamPropParam::Assign(n) => ast::Pat::Assign(n.clone()),
                },
                ast::ParamOrTsParamProp::Param(p) => p.pat.clone(),
            }),
            has_param_without_type_ann!(n.params),
            false,
        ),
        Node::FnExpr(n) => (
            n.function.params.first().map(|p| p.pat.clone()),
            has_param_without_type_ann!(n.function.params),
            n.function.type_params.is_some(),
        ),
        Node::ArrowExpr(n) => (
            n.params.first().map(|p| p.pat.clone()),
            has_param_without_type_ann!(n.params),
            n.type_params.is_some(),
        ),
        _ => unreachable!(),
    };
    // Functions with type parameters are not context sensitive.
    if !has_type_params {
        // Functions with any parameters that lack type annotations are context sensitive.
        if has_param_without_type_ann {
            return true;
        }
        if matches!(node, Node::ArrowExpr(_)) {
            // If the first parameter is not an explicit 'this' parameter, then the function has
            // an implicit 'this' parameter which is subject to contextual typing.
            if !(first_param.is_some() && parameterIsThisKeyword(&first_param.unwrap())) {
                return true;
            }
        }
    }
    false
}

/**
 * Gets the effective type annotation of a variable, parameter, or property. If the node was
 * parsed in a JavaScript file, gets the type annotation from JSDoc.  Also gets the type of
 * functions only the JSDoc case.
 */
pub fn getEffectiveTypeAnnotationNode(node: &Node) -> Option<ast::TsType> {
    if !isNodeInJSFile(&node) && matches!(node, Node::FnDecl(_)) {
        return None;
    }
    let ty = match node {
        Node::BindingIdent(n) => n.type_ann.as_ref().map(|t| t.type_ann.clone()),
        Node::ArrayPat(n) => n.type_ann.as_ref().map(|t| t.type_ann.clone()),
        Node::RestPat(n) => n.type_ann.as_ref().map(|t| t.type_ann.clone()),
        Node::ObjectPat(n) => n.type_ann.as_ref().map(|t| t.type_ann.clone()),
        Node::AssignPat(n) => n.type_ann.as_ref().map(|t| t.type_ann.clone()),
        Node::ClassProp(n) => n.type_ann.as_ref().map(|t| t.type_ann.clone()),
        // TODO: is this correct?
        Node::Param(p) => getEffectiveTypeAnnotationNode(&p.pat.clone().into()),
        // Node::ParamWithoutDecorators(p) => getEffectiveTypeAnnotationNode(&p.pat.clone().into()),
        // Node::TsAmbientParam(p) => getEffectiveTypeAnnotationNode(&p.pat.clone().into()),
        // Node::TsParamPropParam(p) => getEffectiveTypeAnnotationNode(&p.param.clone().into()),
        _ => {
            dbg!(node);
            todo!()
        }
    };
    if ty.is_some() || !isNodeInJSFile(&node) {
        return ty;
    }
    todo!("jsdoc")
    // if isJSDocPropertyLikeTag(node) {
    //     node.typeExpression && node.typeExpression.ty
    // } else {
    //     getJSDocType(node)
    // }
}
/**
 * Gets the effective type annotation of a variable, parameter, or property. If the node was
 * parsed in a JavaScript file, gets the type annotation from JSDoc.  Also gets the type of
 * functions only the JSDoc case.
 */
pub fn getBoundEffectiveTypeAnnotationNode(node: &BoundNode) -> Option<BoundNode> {
    if !isBoundNodeInJSFile(&node) && matches!(node, BoundNode::FnDecl(_)) {
        return None;
    }
    let ty = match node {
        BoundNode::BindingIdent(n) => n
            .type_ann
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(node.clone()))),
        BoundNode::ArrayPat(n) => n
            .type_ann
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(node.clone()))),
        BoundNode::RestPat(n) => n
            .type_ann
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(node.clone()))),
        BoundNode::ObjectPat(n) => n
            .type_ann
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(node.clone()))),
        BoundNode::AssignPat(n) => n
            .type_ann
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(node.clone()))),
        BoundNode::ClassProp(n) => n
            .type_ann
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(node.clone()))),
        // TODO: is this correct?
        BoundNode::Param(p) => getBoundEffectiveTypeAnnotationNode(&p.pat.bind(node.clone())),
        // BoundNode::ParamWithoutDecorators(p) => getBoundEffectiveTypeAnnotationNode(&p.pat.bind(node.clone())),
        BoundNode::TsAmbientParam(p) => {
            getBoundEffectiveTypeAnnotationNode(&p.pat.bind(node.clone()))
        }
        // BoundNode::TsParamPropParam(p) => getBoundEffectiveTypeAnnotationNode(&p.param.bind(node.clone())),
        _ => {
            dbg!(node);
            todo!()
        }
    };
    if ty.is_some() || !isBoundNodeInJSFile(&node) {
        return ty;
    }
    todo!("jsdoc")
    // if isJSDocPropertyLikeTag(node) {
    //     node.typeExpression && node.typeExpression.ty
    // } else {
    //     getJSDocType(node)
    // }
}

/**
 * Gets the effective return type annotation of a signature. If the node was parsed in a
 * JavaScript file, gets the return type annotation from JSDoc.
 */
pub fn getEffectiveReturnTypeNode(node: &Node) -> Option<ast::TsType> {
    // TODO: jsdoc:
    // return isJSDocSignature(node) ?
    //     node.type && node.type.typeExpression && node.type.typeExpression.type :
    //     node.type || (isBoundNodeInJSFile(node) ? getJSDocReturnType(node) : undefined);
    let return_ty = match node {
        // todo: jsdoc
        // BoundNode::JSDocSignature
        // | BoundNode::JSDocFunctionType(_)
        Node::TsCallSignatureDecl(n) => n.type_ann.as_ref().map(|t| t.type_ann.clone()),
        Node::TsConstructSignatureDecl(n) => n.type_ann.as_ref().map(|t| t.type_ann.clone()),
        Node::TsMethodSignature(n) => n.type_ann.as_ref().map(|t| t.type_ann.clone()),
        Node::TsIndexSignature(n) => n.type_ann.as_ref().map(|t| t.type_ann.clone()),
        Node::TsFnType(n) => Some(n.type_ann.type_ann.clone()),
        Node::TsConstructorType(n) => Some(n.type_ann.type_ann.clone()),
        Node::FnDecl(n) => n.function.return_type.as_ref().map(|t| t.type_ann.clone()),
        Node::PrivateMethod(n) => n.function.return_type.as_ref().map(|t| t.type_ann.clone()),
        Node::ClassMethod(n) => n.function.return_type.as_ref().map(|t| t.type_ann.clone()),
        Node::MethodProp(n) => n.function.return_type.as_ref().map(|t| t.type_ann.clone()),
        Node::Constructor(_) | Node::SetterProp(_) | Node::GetterProp(_) => None,
        Node::FnExpr(n) => n.function.return_type.as_ref().map(|t| t.type_ann.clone()),
        Node::ArrowExpr(n) => n.return_type.as_ref().map(|t| t.type_ann.clone()),
        _ => todo!(),
    };

    return_ty
}

/**
 * Gets the effective return type annotation of a signature. If the node was parsed in a
 * JavaScript file, gets the return type annotation from JSDoc.
 */
pub fn getBoundEffectiveReturnTypeNode(node: &BoundNode) -> Option<BoundNode> {
    // TODO: jsdoc:
    // return isJSDocSignature(node) ?
    //     node.type && node.type.typeExpression && node.type.typeExpression.type :
    //     node.type || (isBoundNodeInJSFile(node) ? getJSDocReturnType(node) : undefined);
    let return_ty = match node {
        // todo: jsdoc
        // BoundNode::JSDocSignature
        // | BoundNode::JSDocFunctionType(_)
        BoundNode::TsCallSignatureDecl(n) => n
            .type_ann
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(node.clone()))),
        BoundNode::TsConstructSignatureDecl(n) => n
            .type_ann
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(node.clone()))),
        BoundNode::TsMethodSignature(n) => n
            .type_ann
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(node.clone()))),
        BoundNode::TsIndexSignature(n) => n
            .type_ann
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(node.clone()))),
        BoundNode::TsFnType(n) => Some(n.type_ann.type_ann.bind(n.type_ann.bind(node.clone()))),
        BoundNode::TsConstructorType(n) => {
            Some(n.type_ann.type_ann.bind(n.type_ann.bind(node.clone())))
        }
        BoundNode::FnDecl(n) => n
            .function
            .return_type
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(n.function.bind(node.clone())))),
        BoundNode::PrivateMethod(n) => n
            .function
            .return_type
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(n.function.bind(node.clone())))),
        BoundNode::ClassMethod(n) => n
            .function
            .return_type
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(n.function.bind(node.clone())))),
        BoundNode::MethodProp(n) => n
            .function
            .return_type
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(n.function.bind(node.clone())))),
        BoundNode::Constructor(_) | BoundNode::SetterProp(_) | BoundNode::GetterProp(_) => None,
        BoundNode::FnExpr(n) => n
            .function
            .return_type
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(n.function.bind(node.clone())))),
        BoundNode::ArrowExpr(n) => n
            .return_type
            .as_ref()
            .map(|t| t.type_ann.bind(t.bind(node.clone()))),
        _ => todo!(),
    };

    return_ty
}

pub fn isModuleWithStringLiteralName(node: &BoundNode) -> bool {
    if let BoundNode::TsModuleDecl(m) = node {
        matches!(m.id, ast::TsModuleName::Str(_))
    } else {
        false
    }
}

pub fn isUMDExportSymbol(symbol: Option<&Symbol>) -> bool {
    let first_decl = symbol.and_then(|s| s.declarations().first());
    if let Some(first_decl) = first_decl {
        return matches!(first_decl, BoundNode::TsNamespaceExportDecl(_));
    }
    false
}

pub fn isTypeAlias(node: &BoundNode) -> bool {
    // TODO: jsdoc
    // return isJSDocTypeAlias(node) || isTypeAliasDeclaration(node);
    matches!(node, BoundNode::TsTypeAliasDecl(_))
}

// TODO: may be incorrect
pub fn isFunctionLikeDeclaration(node: &BoundNode) -> bool {
    matches!(
        node,
        BoundNode::FnDecl(_)
            | BoundNode::PrivateMethod(_)
            | BoundNode::ClassMethod(_)
            | BoundNode::MethodProp(_)
            | BoundNode::Constructor(_)
            | BoundNode::GetterProp(_)
            | BoundNode::SetterProp(_)
            | BoundNode::FnExpr(_)
            | BoundNode::ArrowExpr(_)
    )
}

pub fn isInJSFile(_node: Option<&BoundNode>) -> bool {
    // TODO: js files
    false
    // return !!node && !!(node.flags & NodeFlags.JavaScriptFile);
}

pub fn isExternalOrCommonJsModule(file: &BoundNode) -> bool {
    // TODO: common js
    // return (file.externalModuleIndicator || file.commonJsModuleIndicator) !== undefined;
    matches!(file, BoundNode::Module(_))
}

pub fn isPushOrUnshiftIdentifier(sym: &JsWord) -> bool {
    sym == "push" || sym == "unshift"
}

#[derive(PartialEq, Eq)]
pub enum AssignmentKind {
    None,
    Definite,
    Compound,
}

pub fn getAssignmentTargetKind(mut node: BoundNode) -> AssignmentKind {
    let mut parent = node.parent();
    while let Some(p) = parent {
        match p {
            BoundNode::BinExpr(_) => {
                todo!();
                // let binaryOperator = (p as BinaryExpression).operatorToken.kind;
                // return isAssignmentOperator(binaryOperator) && (p as BinaryExpression).left == node ?
                //     binaryOperator == SyntaxKind.EqualsToken || isLogicalOrCoalescingAssignmentOperator(binaryOperator) ? AssignmentKind.Definite : AssignmentKind.Compound :
                //     AssignmentKind.None;
            }
            BoundNode::UpdateExpr(_) => {
                return AssignmentKind::Compound;
            }
            BoundNode::ForInStmt(ref f) => {
                return if f.left.bind(p.clone()) == node {
                    AssignmentKind::Definite
                } else {
                    AssignmentKind::None
                };
            }
            BoundNode::ForOfStmt(ref f) => {
                return if f.left.bind(p.clone()) == node {
                    AssignmentKind::Definite
                } else {
                    AssignmentKind::None
                };
            }
            BoundNode::ParenExpr(_)
            | BoundNode::ArrayLit(_)
            | BoundNode::SpreadElement(_)
            | BoundNode::TsNonNullExpr(_) => {
                node = p;
            }
            // SpreadAssignment:
            BoundNode::SpreadAssignment(_) => {
                node = p.parent().unwrap();
            }
            // ShorthandPropertyAssignment:
            BoundNode::Ident(_) if matches!(p.parent(), Some(BoundNode::ObjectLit(_))) => {
                if p != node {
                    return AssignmentKind::None;
                }
                node = p.parent().unwrap();
            }
            BoundNode::KeyValueProp(ref prop) => {
                if prop.key.bind(p.clone()) == node {
                    return AssignmentKind::None;
                }
                node = p.parent().unwrap();
            }
            _ => return AssignmentKind::None,
        }
        parent = node.parent();
    }
    AssignmentKind::None
}

// A node is an assignment target if it is on the left hand side of an '=' token, if it is parented by a property
// assignment in an object literal that is an assignment target, or if it is parented by an array literal that is
// an assignment target. Examples include 'a = xxx', '{ p: a } = xxx', '[{ a }] = xxx'.
// (Note that `p` is not a target in the above examples, only `a`.)
pub fn isAssignmentTarget(node: BoundNode) -> bool {
    getAssignmentTargetKind(node) != AssignmentKind::None
}

fn walkUp<P>(mut node: BoundNode, predicate: P) -> BoundNode
where
    P: Fn(&BoundNode) -> bool,
{
    // TODO: logic may be incorrect:
    while let Some(parent) = node.parent() {
        if !predicate(&node) {
            break;
        }
        node = parent;
    }
    node
}

pub fn walkUpParenthesizedTypes(node: BoundNode) -> BoundNode {
    walkUp(node, |n| matches!(n, BoundNode::TsParenthesizedType(_)))
}

pub fn walkUpParenthesizedExpressions(node: BoundNode) -> BoundNode {
    walkUp(node, |n| matches!(n, BoundNode::ParenExpr(_)))
}

pub fn getBoundEffectiveConstraintOfTypeParameter(node: &TsTypeParam) -> Option<BoundNode> {
    // todo: jsodc
    // if node.constraint {
    //     node.constraint
    // } else if isJSDocTemplateTag(node.parent) && node == node.parent.typeParameters[0] {
    //     node.parent.constraint
    // } else {
    //     None
    // }
    node.constraint
        .as_ref()
        .map(|c| c.bind(node.parent.clone().unwrap()))
}
pub fn getEffectiveConstraintOfTypeParameter(node: &ast::TsTypeParam) -> Option<&ast::TsType> {
    // todo: jsodc
    // if node.constraint {
    //     node.constraint
    // } else if isJSDocTemplateTag(node.parent) && node == node.parent.typeParameters[0] {
    //     node.parent.constraint
    // } else {
    //     None
    // }
    node.constraint.as_ref()
}

pub fn isStatement(node: &BoundNode) -> bool {
    matches!(
        node,
        BoundNode::BreakStmt(_)
            | BoundNode::ContinueStmt(_)
            | BoundNode::DebuggerStmt(_)
            | BoundNode::DoWhileStmt(_)
            | BoundNode::ExprStmt(_)
            | BoundNode::EmptyStmt(_)
            | BoundNode::ForInStmt(_)
            | BoundNode::ForOfStmt(_)
            | BoundNode::ForStmt(_)
            | BoundNode::IfStmt(_)
            | BoundNode::LabeledStmt(_)
            | BoundNode::ReturnStmt(_)
            | BoundNode::SwitchStmt(_)
            | BoundNode::ThrowStmt(_)
            | BoundNode::TryStmt(_)
            | BoundNode::VarDecl(_)
            | BoundNode::WhileStmt(_)
            | BoundNode::WithStmt(_)
            | BoundNode::FnDecl(_)
            | BoundNode::ClassDecl(_)
            | BoundNode::TsInterfaceDecl(_)
            | BoundNode::TsTypeAliasDecl(_)
            | BoundNode::TsEnumDecl(_)
            | BoundNode::TsModuleDecl(_)
            | BoundNode::ImportDecl(_)
            | BoundNode::TsImportEqualsDecl(_)
            | BoundNode::ExportDecl(_)
            | BoundNode::ExportDefaultDecl(_)
            | BoundNode::TsExportAssignment(_)
            | BoundNode::TsNamespaceExportDecl(_)
            | BoundNode::NamedExport(_)
            | BoundNode::ExportDefaultExpr(_)
            | BoundNode::ExportAll(_)
    ) || isBlockStatement(node)
}

fn isBlockStatement(node: &BoundNode) -> bool {
    if let BoundNode::BlockStmt(n) = node {
        let parent = n.parent.as_ref();
        if let Some(parent) = parent {
            if matches!(parent, BoundNode::TryStmt(_) | BoundNode::CatchClause(_)) {
                return false;
            }
        }
        return !isFunctionLike(parent);
    }
    false
}

pub fn isAliasableExpression(e: &Node) -> bool {
    isEntityNameExpression(e) || matches!(e, Node::ClassExpr(_))
}

pub fn exportAssignmentIsAlias(node: &Node) -> bool {
    let e = getExportAssignmentExpression(node);
    isAliasableExpression(&Node::from(e.clone()))
}

fn getExportAssignmentExpression(node: &Node) -> &Expr {
    match node {
        Node::TsExportAssignment(n) => &n.expr,
        Node::BinExpr(n) => &n.right,
        _ => unreachable!(),
    }
}

/**
 * Returns true if the node is a CallExpression to the identifier 'require' with
 * exactly one argument (of the form 'require("name")').
 * This function does not test if the node is in a JavaScript file or not.
 */
pub fn isRequireCall(callExpression: &Node, requireStringLiteralLikeArgument: bool) -> bool {
    if let Node::CallExpr(c) = callExpression {
        if let ExprOrSuper::Expr(Expr::Ident(callee)) = &c.callee {
            if callee.sym == js_word!("require") {
                if c.args.len() != 1 {
                    return false;
                }
                let arg = &c.args[0];
                return !requireStringLiteralLikeArgument
                    || matches!(arg, ast::ExprOrSpread::Expr(e) if isStringLiteralLike(e));
            }
        }
    }
    false
}

/**
 * Returns true if the node is a VariableDeclaration initialized to a require call (see `isRequireCall`).
 * This function does not test if the node is in a JavaScript file or not.
 */
pub fn isRequireVariableDeclaration(node: BoundNode) -> bool {
    match &node {
        BoundNode::BindingIdent(_)
        | BoundNode::ArrayPat(_)
        | BoundNode::RestPat(_)
        | BoundNode::ObjectPat(_)
        | BoundNode::AssignPat(_) => todo!(),
        _ => {}
    }
    // if (node.kind == SyntaxKind.BindingElement) {
    //     node = node.parent.parent;
    // }
    if let BoundNode::VarDeclarator(n) = node {
        if let Some(init) = &n.init {
            return isRequireCall(&getLeftmostAccessExpression(Node::from(init.clone())), true);
        }
    }
    false
}

fn getLeftmostAccessExpression(mut node: Node) -> Node {
    while let Node::MemberExpr(m) = node {
        node = Node::from(m.obj.clone());
    }
    node
}

pub fn rangeEquals<T: PartialEq>(array1: &[T], array2: &[T], pos: usize, end: usize) -> bool {
    for pos in pos..end {
        if array1.get(pos) != array2.get(pos) {
            return false;
        }

        // At this point, the values are eq, so we only have to check if one is None.
        if array1.get(pos).is_none() {
            return true;
        }
    }
    true
}

#[derive(PartialEq, Eq)]
pub struct ExpressionWithTypeArguments {
    pub expr: BoundNode,
    pub type_args: Option<Rc<TsTypeParamInstantiation>>,
}

pub fn getEffectiveBaseTypeNode(node: BoundNode) -> Option<ExpressionWithTypeArguments> {
    let baseType = match &node {
        BoundNode::ClassDecl(n) => {
            n.class
                .super_class
                .as_ref()
                .map(|s| ExpressionWithTypeArguments {
                    expr: s.bind(n.class.bind(node.clone())),
                    type_args: n.class.super_type_params.as_ref().map(|t| {
                        Rc::new(TsTypeParamInstantiation {
                            node: t.clone(),
                            parent: Some(n.class.bind(node.clone())),
                        })
                    }),
                })
        }
        BoundNode::ClassExpr(n) => {
            n.class
                .super_class
                .as_ref()
                .map(|s| ExpressionWithTypeArguments {
                    expr: s.bind(n.class.bind(node.clone())),
                    type_args: n.class.super_type_params.as_ref().map(|t| {
                        Rc::new(TsTypeParamInstantiation {
                            node: t.clone(),
                            parent: Some(n.class.bind(node.clone())),
                        })
                    }),
                })
        }
        BoundNode::TsInterfaceDecl(n) => n.extends.first().map(|e| ExpressionWithTypeArguments {
            expr: e.expr.bind(e.bind(node.clone())),
            type_args: e.type_args.as_ref().map(|t| {
                Rc::new(TsTypeParamInstantiation {
                    node: t.clone(),
                    parent: Some(e.bind(node.clone())),
                })
            }),
        }),
        _ => None,
    };
    if baseType.is_some() && isBoundNodeInJSFile(&node) {
        todo!();
        // // Prefer an @augments tag because it may have type parameters.
        // const tag = getJSDocAugmentsTag(node);
        // if (tag) {
        //     return tag.class;
        // }
    }
    baseType
}

/** True if has initializer node attached to it. */
pub fn hasInitializer(node: &Node) -> bool {
    match node {
        // Node::VarDeclarator(_) => todo!(),
        // Node::ParameterDeclaration(_) => todo!(),
        // Node::BindingElement(_) => todo!(),
        // Node::PropertyDeclaration(_) => todo!(),
        // Node::PropertyAssignment(_) => todo!(),
        // Node::PropertySignature(_) => todo!(),
        // Node::JsxAttribute(_) => todo!(),
        // Node::ShorthandPropertyAssignment(_) => todo!(),
        // Node::EnumMember(_) => todo!(),
        // Node::JSDocPropertyTag(_) => todo!(),
        // Node::JSDocParameterTag(_) => todo!(),
        // _ => unreachable!()
        _ => todo!(),
    }
}

pub fn isWriteOnlyAccess(node: &BoundNode) -> bool {
    accessKind(node) == AccessKind::Write
}

pub fn isWriteAccess(node: &BoundNode) -> bool {
    accessKind(node) != AccessKind::Read
}

#[derive(PartialEq, Eq)]
enum AccessKind {
    /// Only reads from a variable.
    Read,
    /// Only writes to a variable without using the result. E.g.: `x++;`.
    Write,
    /// Writes to a variable and uses the result as an expression. E.g.: `f(x++);`.
    ReadWrite,
}
fn accessKind(node: &BoundNode) -> AccessKind {
    fn writeOrReadWrite(parent: BoundNode) -> AccessKind {
        // If grandparent is not an ExpressionStatement, this is used as an expression in addition to having a side effect.
        if let Some(grand_parent) = parent.parent() {
            if matches!(
                walkUpParenthesizedExpressions(grand_parent),
                BoundNode::ExprStmt(_)
            ) {
                return AccessKind::Write;
            }
        }
        AccessKind::ReadWrite
    }
    if let Some(parent) = node.parent() {
        match &parent {
            BoundNode::ParenExpr(_) => accessKind(&parent),
            // BoundNode::PostfixUnaryExpression(_) => {
            // BoundNode::PrefixUnaryExpression(_) => {
            //     const { operator } = parent as PrefixUnaryExpression | PostfixUnaryExpression;
            //     return operator == SyntaxKind.PlusPlusToken || operator == SyntaxKind.MinusMinusToken ? writeOrReadWrite() : AccessKind.Read;
            // }
            BoundNode::BinExpr(_) => {
                todo!("is this reachable?");
                // AccessKind::Read
            }
            BoundNode::AssignExpr(e) => {
                // const { left, operatorToken } = parent as BinaryExpression;
                if &e.left.bind(parent.clone()) == node {
                    if e.op == ast::AssignOp::Assign {
                        AccessKind::Write
                    } else {
                        writeOrReadWrite(parent)
                    }
                } else {
                    AccessKind::Read
                }
            }
            BoundNode::MemberExpr(m) if !m.computed => {
                if &m.prop.bind(parent.clone()) != node {
                    AccessKind::Read
                } else {
                    accessKind(&parent)
                }
            }
            BoundNode::KeyValueProp(p) => {
                let parentAccess = accessKind(&parent.clone().parent().unwrap());
                // In `({ x: varname }) = { x: 1 }`, the left `x` is a read, the right `x` is a write.
                if node == &p.key.bind(parent.clone()) {
                    reverseAccessKind(parentAccess)
                } else {
                    parentAccess
                }
            }
            // ShorthandPropertyAssignment
            BoundNode::Ident(_) if matches!(parent.parent(), Some(BoundNode::ObjectLit(_))) => {
                // Assume it's the local variable being accessed, since we don't check public properties for --noUnusedLocals.
                todo!();
                // return node == (parent as ShorthandPropertyAssignment).objectAssignmentInitializer ? AccessKind.Read : accessKind(parent.parent);
            }
            BoundNode::ArrayLit(_) => accessKind(&parent),
            _ => AccessKind::Read,
        }
    } else {
        AccessKind::Read
    }
}
fn reverseAccessKind(a: AccessKind) -> AccessKind {
    match a {
        AccessKind::Read => AccessKind::Write,
        AccessKind::Write => AccessKind::Read,
        AccessKind::ReadWrite => AccessKind::ReadWrite,
    }
}

pub fn getEmitScriptTarget(_compilerOptions: &crate::CompilerOptions) -> ScriptTarget {
    // TODO:
    ScriptTarget::ESNext
    // return compilerOptions.target ||
    //     (compilerOptions.module === ModuleKind.Node12 && ScriptTarget.ES2020) ||
    //     (compilerOptions.module === ModuleKind.NodeNext && ScriptTarget.ESNext) ||
    //     ScriptTarget.ES3;
}

pub fn is_ambient_declaration(node: &BoundNode) -> bool {
    match node {
        _ => {
            dbg!(node);
            todo!();
        }
    }
}

pub fn isBindingElement(node: &BoundNode) -> bool {
    matches!(
        node,
        BoundNode::BindingIdent(_)
            | BoundNode::ArrayPat(_)
            | BoundNode::RestPat(_)
            | BoundNode::ObjectPat(_)
            | BoundNode::AssignPat(_)
    ) && matches!(node.parent(), Some(BoundNode::ArrayPat(_)))
        || matches!(
            node,
            BoundNode::KeyValuePatProp(_) | BoundNode::AssignPatProp(_) | BoundNode::RestPat(_)
        ) && matches!(node.parent(), Some(BoundNode::ObjectPat(_)))
}

/**
 * Performs a binary search, finding the index at which an object with `key` occurs in `array`.
 * If no such index is found, returns the 2's-complement of first index at which
 * `array[index]` exceeds `key`.
 * @param array A sorted array whose first element must be no larger than number
 * @param key The key to be searched for in the array.
 * @param keySelector A callback used to select the search key from each element of `array`.
 * @param keyComparer A callback used to compare two keys in a sorted array.
 * @param offset An offset into `array` at which to start the search.
 */
pub fn binarySearchKey<T: Copy, U: Copy>(
    array: &[T],
    key: U,
    mut keySelector: impl FnMut(T, isize) -> U,
    keyComparer: impl Fn(U, U) -> std::cmp::Ordering,
    offset: Option<isize>,
) -> isize {
    use std::cmp::Ordering::*;
    let mut low = offset.unwrap_or_default();
    let mut high = array.len() as isize - 1;
    while low <= high {
        let middle = low + ((high - low) >> 1);
        let midKey = keySelector(array[middle as usize], middle);
        let cmp = keyComparer(midKey, key);
        if cmp == Less {
            low = middle + 1;
        } else if cmp == Greater {
            high = middle - 1;
        } else {
            return middle;
        }
    }

    !low
}
