use crate::ast;
use crate::node::*;
use crate::types::*;
use bitflags::bitflags;
use std::mem;
use std::rc::Rc;
use swc_atoms::JsWord;

pub fn isFunctionLike(node: &Option<BoundNode>) -> bool {
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
    todo!();
    // match func {
    //     BoundNode::FnExpr(_) | BoundNode::ArrowExpr(_) => {
    //         let mut prev = func;
    //         let mut parent = func.parent();
    //         while let Some(p @ BoundNode::ParenExpr(_)) = parent {
    //             prev = p;
    //             parent = p.parent();
    //         }
    //         // loop {
    //         //     if let Some(p) = parent {
    //         //         if let BoundNode::ParenExpr(_) = *p {
    //         //             prev = p;
    //         //             parent = p.parent();
    //         //             continue;
    //         //         }
    //         //     }

    //         //     break;
    //         // }
    //         if let Some(BoundNode::CallExpr(c)) = parent {
    //             if c.callee == prev {
    //                 return Some(c);
    //             }
    //         }
    //         if (parent.kind === SyntaxKind.CallExpression && (parent as CallExpression).expression === prev) {
    //             return parent as CallExpression;
    //         }
    //     }
    //     _ => {}
    // }

    // None
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

fn isOuterExpression(node: BoundNode, kinds: OuterExpressionKinds) -> bool {
    todo!();
    // match node {
    //     BoundNode::ParenExpr(_) => {
    //         if (kinds & OuterExpressionKinds.ExcludeJSDocTypeAssertion
    //             && isJSDocTypeAssertion(node))
    //         {
    //             return false;
    //         }
    //         return (kinds & OuterExpressionKinds.Parentheses) != 0;
    //     }
    //     BoundNode::TsTypeAssertion(_) | BoundNode::TsAsExpr(_) => {
    //         return (kinds & OuterExpressionKinds.TypeAssertions) != 0;
    //     }
    //     BoundNode::TsNonNullExpr(_) => {
    //         return (kinds & OuterExpressionKinds.NonNullAssertions) != 0;
    //     }
    // }
    // return false;
}

fn skipOuterExpressions(mut node: BoundNode, kinds: OuterExpressionKinds) -> BoundNode {
    todo!();
    // while isOuterExpression(node, kinds) {
    //     node = node.expression;
    // }
    // node
}

pub fn skipParentheses(node: BoundNode /*, excludeJSDocTypeAssertions: bool*/) -> BoundNode {
    // const flags = excludeJSDocTypeAssertions ?
    //     OuterExpressionKinds.Parentheses | OuterExpressionKinds.ExcludeJSDocTypeAssertion :
    //     OuterExpressionKinds.Parentheses;
    let flags = OuterExpressionKinds::Parentheses;
    skipOuterExpressions(node, flags)
}

pub fn isLogicalOrCoalescingAssignmentOperator(op: ast::AssignOp) -> bool {
    matches!(
        op,
        ast::AssignOp::OrAssign | ast::AssignOp::AndAssign | ast::AssignOp::NullishAssign
    )
}

pub fn isDottedName(node: &ast::ExprOrSuper) -> bool {
    match node {
        ast::ExprOrSuper::Expr(expr) => match expr {
            ast::Expr::Ident(_) | ast::Expr::This(_) | ast::Expr::MetaProp(_) => true,
            ast::Expr::Member(e) => isDottedName(&e.obj),
            ast::Expr::Paren(e) if isDottedName(&e.expr.clone().into()) => true,
            _ => false,
        },
        ast::ExprOrSuper::Super(_) => true,
    }
}

fn isStringLiteralLike(expr: &ast::Expr) -> bool {
    match expr {
        ast::Expr::Lit(ast::Lit::Str(_)) => true,
        ast::Expr::Tpl(tpl) => tpl.exprs.is_empty(),
        _ => false,
    }
}

pub fn isParameterDeclaration(node: BoundNode) -> bool {
    // todo: debug assert that node is "VariableLikeDeclaration" (from tsc).
    let root = getRootDeclaration(node);
    matches!(root, BoundNode::Param(_))
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
    fn push_if_unique(&mut self, elem: T);
}

impl<T> PushIfUnique<T> for Vec<T>
where
    T: PartialEq,
{
    fn push_if_unique(&mut self, value: T) {
        let is_present = self.iter().any(|v| v == &value);
        if !is_present {
            self.push(value);
        }
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
        BoundNode::TsIndexSignature(_) | BoundNode::EmptyStmt(_) => false,
        _ => unreachable!("non-class member"),
    }
}

pub enum DeclName {
    Ident(Rc<ast::Ident>),
    PrivateName(Rc<ast::PrivateName>),
    String(Rc<ast::Str>),
    NoSubstitutionTemplate(Rc<ast::Tpl>),
    Number(Rc<ast::Number>),
    ComputedProperty(ast::Expr),
    ElementAccessExpression(ast::Expr),
    ArrayPt(Rc<ast::ArrayPat>),
    ObjectPat(Rc<ast::ObjectPat>),
    EntityNameExpression(ast::TsEntityName),
}

fn getNonAssignedNameOfDeclaration(declaration: BoundNode) -> Option<DeclName> {
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
        BoundNode::TsExportAssignment(ref e) => {
            if let ast::Expr::Ident(i) = &e.expr {
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
        // ClassDeclaration
        // InterfaceDeclaration
        BoundNode::TsInterfaceDecl(ref i) => Some(DeclName::Ident(i.id.clone())),

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
        BoundNode::VarDeclarator(ref d) => match &d.name {
            ast::Pat::Ident(i) => Some(DeclName::Ident(i.id.clone())),
            ast::Pat::Array(_) => todo!(),
            ast::Pat::Object(_) => todo!(),
            _ => unreachable!(),
        },
        BoundNode::Param(ref p) => match &p.pat {
            ast::Pat::Ident(i) => Some(DeclName::Ident(i.id.clone())),
            _ => todo!(),
        },
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
        BoundNode::TsModuleDecl(ref d) => match &d.id {
            ast::TsModuleName::Ident(i) => Some(DeclName::Ident(i.clone())),
            ast::TsModuleName::Str(s) => Some(DeclName::String(s.clone())),
        },
        BoundNode::RestPat(ref p) => match &p.arg {
            ast::Pat::Ident(i) => Some(DeclName::Ident(i.id.clone())),
            _ => todo!(),
        },
        BoundNode::BindingIdent(ref i) => Some(DeclName::Ident(i.id.clone())),
        BoundNode::TsTypeParam(ref p) => Some(DeclName::Ident(p.name.clone())),
        BoundNode::TsFnType(_) => None,
        BoundNode::TsConstructorType(_) => None,
        BoundNode::TsCallSignatureDecl(_) => None,
        BoundNode::TsConstructSignatureDecl(_) => None,
        BoundNode::TsIndexSignature(_) => None,
        BoundNode::TsPropertySignature(ref s) => {
            if s.computed {
                Some(DeclName::ComputedProperty(s.key.clone()))
            } else {
                match &s.key {
                    ast::Expr::Ident(i) => Some(DeclName::Ident(i.clone())),
                    ast::Expr::Lit(ast::Lit::Str(s)) => Some(DeclName::String(s.clone())),
                    ast::Expr::Lit(ast::Lit::Num(n)) => Some(DeclName::Number(n.clone())),
                    ast::Expr::Lit(ast::Lit::BigInt(_)) => todo!(),
                    _ => unreachable!(),
                }
            }
        }
        BoundNode::TsTypeAliasDecl(ref a) => Some(DeclName::Ident(a.id.clone())),
        BoundNode::KeyValueProp(ref p) => match &p.key {
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

pub fn getNameOfDeclaration(declaration: BoundNode) -> Option<DeclName> {
    let name = getNonAssignedNameOfDeclaration(declaration.clone());
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

pub fn isStringOrNumericLiteralLike(expr: &ast::Expr) -> bool {
    isStringLiteralLike(&expr) || matches!(expr, ast::Expr::Lit(ast::Lit::Num(_)))
}

fn isSignedNumericLiteral(node: &ast::Expr) -> bool {
    if let ast::Expr::Unary(e) = node {
        if e.op == ast::UnaryOp::Plus || e.op == ast::UnaryOp::Minus {
            return matches!(e.arg, ast::Expr::Lit(ast::Lit::Num(_)));
        }
    }
    false
}

/// Within a computed name, a name is dynamic if all of the following are true:
///   1. The declaration has a computed property name.
///   2. The computed name is *not* expressed as a StringLiteral.
///   3. The computed name is *not* expressed as a NumericLiteral.
///   4. The computed name is *not* expressed as a PlusToken or MinusToken
///      immediately followed by a NumericLiteral.
pub fn isDynamicName(name: &ast::Expr) -> bool {
    !isStringOrNumericLiteralLike(name) && !isSignedNumericLiteral(name)
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

// /**
//  * Remove extra underscore from escaped identifier text content.
//  *
//  * @param identifier The escaped identifier text.
//  * @returns The unescaped identifier text.
//  */
// export function unescapeLeadingUnderscores(identifier: __String): string {
//     const id = identifier as string;
//     return id.length >= 3 && id.charCodeAt(0) === CharacterCodes._ && id.charCodeAt(1) === CharacterCodes._ && id.charCodeAt(2) === CharacterCodes._ ? id.substr(1) : id;
// }

// export function idText(identifierOrPrivateName: Identifier | PrivateIdentifier): string {
//     return unescapeLeadingUnderscores(identifierOrPrivateName.escapedText);
// }
pub fn symbolName(symbol: &Symbol) -> JsWord {
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

pub fn isBindingPattern(node: &BoundNode) -> bool {
    matches!(node, BoundNode::ArrayPat(_) | BoundNode::ObjectPat(_))
}
