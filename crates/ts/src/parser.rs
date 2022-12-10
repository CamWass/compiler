use std::fmt::Display;
use std::rc::Rc;

use bitflags::bitflags;
use diagnostics::Diagnostics;
use rustc_hash::FxHashSet;
use swc_atoms::JsWord;

use crate::core::*;
use crate::factory::nodeFactory::*;
use crate::factory::nodeTests::*;
use crate::factory::utilitiesPublic::setTextRange;
use crate::node::Node;
use crate::our_types::*;
use crate::our_utils::*;
use crate::path::*;
use crate::scanner::*;
use crate::types::*;
use crate::utilities::*;
use crate::utilitiesPublic::*;

bitflags! {
    #[derive(Default)]
    struct SignatureFlags: u8 {
        const None = 0;
        const Yield = 1 << 0;
        const Await = 1 << 1;
        const Type  = 1 << 2;
        const IgnoreMissingOpenBrace = 1 << 4;
        const JSDoc = 1 << 5;
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SpeculationKind {
    TryParse,
    Lookahead,
    Reparse,
}

// let NodeConstructor: new (kind: SyntaxKind, pos?: number, end?: number) => Node;
// let TokenConstructor: new (kind: SyntaxKind, pos?: number, end?: number) => Node;
// let IdentifierConstructor: new (kind: SyntaxKind, pos?: number, end?: number) => Node;
// let PrivateIdentifierConstructor: new (kind: SyntaxKind, pos?: number, end?: number) => Node;
// let SourceFileConstructor: new (kind: SyntaxKind, pos?: number, end?: number) => Node;

// /**
//  * NOTE: You should not use this, it is only exported to support `createNode` in `~/src/deprecatedCompat/deprecations.ts`.
//  */
// /* @internal */
// export const parseBaseNodeFactory: BaseNodeFactory = {
//     createBaseSourceFileNode: kind => new (SourceFileConstructor || (SourceFileConstructor = objectAllocator.getSourceFileConstructor()))(kind, -1, -1),
//     createBaseIdentifierNode: kind => new (IdentifierConstructor || (IdentifierConstructor = objectAllocator.getIdentifierConstructor()))(kind, -1, -1),
//     createBasePrivateIdentifierNode: kind => new (PrivateIdentifierConstructor || (PrivateIdentifierConstructor = objectAllocator.getPrivateIdentifierConstructor()))(kind, -1, -1),
//     createBaseTokenNode: kind => new (TokenConstructor || (TokenConstructor = objectAllocator.getTokenConstructor()))(kind, -1, -1),
//     createBaseNode: kind => new (NodeConstructor || (NodeConstructor = objectAllocator.getNodeConstructor()))(kind, -1, -1),
// };

// /* @internal */
// export const parseNodeFactory = createNodeFactory(NodeFactoryFlags.NoParenthesizerRules, parseBaseNodeFactory);

// function visitNode<T>(cbNode: (node: Node) => T, node: Node | undefined): T | undefined {
//     return node && cbNode(node);
// }

// function visitNodes<T>(cbNode: (node: Node) => T, cbNodes: ((node: NodeArray<Node>) => T | undefined) | undefined, nodes: NodeArray<Node> | undefined): T | undefined {
//     if (nodes) {
//         if (cbNodes) {
//             return cbNodes(nodes);
//         }
//         for (const node of nodes) {
//             const result = cbNode(node);
//             if (result) {
//                 return result;
//             }
//         }
//     }
// }

// /*@internal*/
// export function isJSDocLikeText(text: string, start: number) {
//     return text.charCodeAt(start + 1) === CharacterCodes.asterisk &&
//         text.charCodeAt(start + 2) === CharacterCodes.asterisk &&
//         text.charCodeAt(start + 3) !== CharacterCodes.slash;
// }

// /**
//  * Invokes a callback for each child of the given node. The 'cbNode' callback is invoked for all child nodes
//  * stored in properties. If a 'cbNodes' callback is specified, it is invoked for embedded arrays; otherwise,
//  * embedded arrays are flattened and the 'cbNode' callback is invoked for each element. If a callback returns
//  * a truthy value, iteration stops and that value is returned. Otherwise, undefined is returned.
//  *
//  * @param node a given node to visit its children
//  * @param cbNode a callback to be invoked for all child nodes
//  * @param cbNodes a callback to be invoked for embedded array
//  *
//  * @remarks `forEachChild` must visit the children of a node in the order
//  * that they appear in the source code. The language service depends on this property to locate nodes by position.
//  */
// export function forEachChild<T>(node: Node, cbNode: (node: Node) => T | undefined, cbNodes?: (nodes: NodeArray<Node>) => T | undefined): T | undefined {
//     if (!node || node.kind <= SyntaxKind::LastToken) {
//         return;
//     }
//     switch (node.kind) {
//         SyntaxKind::QualifiedName:
//             return visitNode(cbNode, (node as QualifiedName).left) ||
//                 visitNode(cbNode, (node as QualifiedName).right);
//         SyntaxKind::TypeParameter:
//             return visitNode(cbNode, (node as TypeParameterDeclaration).name) ||
//                 visitNode(cbNode, (node as TypeParameterDeclaration).constraint) ||
//                 visitNode(cbNode, (node as TypeParameterDeclaration).default) ||
//                 visitNode(cbNode, (node as TypeParameterDeclaration).expression);
//         SyntaxKind::ShorthandPropertyAssignment:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as ShorthandPropertyAssignment).name) ||
//                 visitNode(cbNode, (node as ShorthandPropertyAssignment).questionToken) ||
//                 visitNode(cbNode, (node as ShorthandPropertyAssignment).exclamationToken) ||
//                 visitNode(cbNode, (node as ShorthandPropertyAssignment).equalsToken) ||
//                 visitNode(cbNode, (node as ShorthandPropertyAssignment).objectAssignmentInitializer);
//         SyntaxKind::SpreadAssignment:
//             return visitNode(cbNode, (node as SpreadAssignment).expression);
//         SyntaxKind::Parameter:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as ParameterDeclaration).dotDotDotToken) ||
//                 visitNode(cbNode, (node as ParameterDeclaration).name) ||
//                 visitNode(cbNode, (node as ParameterDeclaration).questionToken) ||
//                 visitNode(cbNode, (node as ParameterDeclaration).type) ||
//                 visitNode(cbNode, (node as ParameterDeclaration).initializer);
//         SyntaxKind::PropertyDeclaration:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as PropertyDeclaration).name) ||
//                 visitNode(cbNode, (node as PropertyDeclaration).questionToken) ||
//                 visitNode(cbNode, (node as PropertyDeclaration).exclamationToken) ||
//                 visitNode(cbNode, (node as PropertyDeclaration).type) ||
//                 visitNode(cbNode, (node as PropertyDeclaration).initializer);
//         SyntaxKind::PropertySignature:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as PropertySignature).name) ||
//                 visitNode(cbNode, (node as PropertySignature).questionToken) ||
//                 visitNode(cbNode, (node as PropertySignature).type) ||
//                 visitNode(cbNode, (node as PropertySignature).initializer);
//         SyntaxKind::PropertyAssignment:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as PropertyAssignment).name) ||
//                 visitNode(cbNode, (node as PropertyAssignment).questionToken) ||
//                 visitNode(cbNode, (node as PropertyAssignment).initializer);
//         SyntaxKind::VariableDeclaration:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as VariableDeclaration).name) ||
//                 visitNode(cbNode, (node as VariableDeclaration).exclamationToken) ||
//                 visitNode(cbNode, (node as VariableDeclaration).type) ||
//                 visitNode(cbNode, (node as VariableDeclaration).initializer);
//         SyntaxKind::BindingElement:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as BindingElement).dotDotDotToken) ||
//                 visitNode(cbNode, (node as BindingElement).propertyName) ||
//                 visitNode(cbNode, (node as BindingElement).name) ||
//                 visitNode(cbNode, (node as BindingElement).initializer);
//         SyntaxKind::FunctionType:
//         SyntaxKind::ConstructorType:
//         SyntaxKind::CallSignature:
//         SyntaxKind::ConstructSignature:
//         SyntaxKind::IndexSignature:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNodes(cbNode, cbNodes, (node as SignatureDeclaration).typeParameters) ||
//                 visitNodes(cbNode, cbNodes, (node as SignatureDeclaration).parameters) ||
//                 visitNode(cbNode, (node as SignatureDeclaration).type);
//         SyntaxKind::MethodDeclaration:
//         SyntaxKind::MethodSignature:
//         SyntaxKind::Constructor:
//         SyntaxKind::GetAccessor:
//         SyntaxKind::SetAccessor:
//         SyntaxKind::FunctionExpression:
//         SyntaxKind::FunctionDeclaration:
//         SyntaxKind::ArrowFunction:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as FunctionLikeDeclaration).asteriskToken) ||
//                 visitNode(cbNode, (node as FunctionLikeDeclaration).name) ||
//                 visitNode(cbNode, (node as FunctionLikeDeclaration).questionToken) ||
//                 visitNode(cbNode, (node as FunctionLikeDeclaration).exclamationToken) ||
//                 visitNodes(cbNode, cbNodes, (node as FunctionLikeDeclaration).typeParameters) ||
//                 visitNodes(cbNode, cbNodes, (node as FunctionLikeDeclaration).parameters) ||
//                 visitNode(cbNode, (node as FunctionLikeDeclaration).type) ||
//                 visitNode(cbNode, (node as ArrowFunction).equalsGreaterThanToken) ||
//                 visitNode(cbNode, (node as FunctionLikeDeclaration).body);
//         SyntaxKind::ClassStaticBlockDeclaration:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as ClassStaticBlockDeclaration).body);
//         SyntaxKind::TypeReference:
//             return visitNode(cbNode, (node as TypeReferenceNode).typeName) ||
//                 visitNodes(cbNode, cbNodes, (node as TypeReferenceNode).typeArguments);
//         SyntaxKind::TypePredicate:
//             return visitNode(cbNode, (node as TypePredicateNode).assertsModifier) ||
//                 visitNode(cbNode, (node as TypePredicateNode).parameterName) ||
//                 visitNode(cbNode, (node as TypePredicateNode).type);
//         SyntaxKind::TypeQuery:
//             return visitNode(cbNode, (node as TypeQueryNode).exprName);
//         SyntaxKind::TypeLiteral:
//             return visitNodes(cbNode, cbNodes, (node as TypeLiteralNode).members);
//         SyntaxKind::ArrayType:
//             return visitNode(cbNode, (node as ArrayTypeNode).elementType);
//         SyntaxKind::TupleType:
//             return visitNodes(cbNode, cbNodes, (node as TupleTypeNode).elements);
//         SyntaxKind::UnionType:
//         SyntaxKind::IntersectionType:
//             return visitNodes(cbNode, cbNodes, (node as UnionOrIntersectionTypeNode).types);
//         SyntaxKind::ConditionalType:
//             return visitNode(cbNode, (node as ConditionalTypeNode).checkType) ||
//                 visitNode(cbNode, (node as ConditionalTypeNode).extendsType) ||
//                 visitNode(cbNode, (node as ConditionalTypeNode).trueType) ||
//                 visitNode(cbNode, (node as ConditionalTypeNode).falseType);
//         SyntaxKind::InferType:
//             return visitNode(cbNode, (node as InferTypeNode).typeParameter);
//         SyntaxKind::ImportType:
//             return visitNode(cbNode, (node as ImportTypeNode).argument) ||
//                 visitNode(cbNode, (node as ImportTypeNode).qualifier) ||
//                 visitNodes(cbNode, cbNodes, (node as ImportTypeNode).typeArguments);
//         SyntaxKind::ParenthesizedType:
//         SyntaxKind::TypeOperator:
//             return visitNode(cbNode, (node as ParenthesizedTypeNode | TypeOperatorNode).type);
//         SyntaxKind::IndexedAccessType:
//             return visitNode(cbNode, (node as IndexedAccessTypeNode).objectType) ||
//                 visitNode(cbNode, (node as IndexedAccessTypeNode).indexType);
//         SyntaxKind::MappedType:
//             return visitNode(cbNode, (node as MappedTypeNode).readonlyToken) ||
//                 visitNode(cbNode, (node as MappedTypeNode).typeParameter) ||
//                 visitNode(cbNode, (node as MappedTypeNode).nameType) ||
//                 visitNode(cbNode, (node as MappedTypeNode).questionToken) ||
//                 visitNode(cbNode, (node as MappedTypeNode).type);
//         SyntaxKind::LiteralType:
//             return visitNode(cbNode, (node as LiteralTypeNode).literal);
//         SyntaxKind::NamedTupleMember:
//             return visitNode(cbNode, (node as NamedTupleMember).dotDotDotToken) ||
//                 visitNode(cbNode, (node as NamedTupleMember).name) ||
//                 visitNode(cbNode, (node as NamedTupleMember).questionToken) ||
//                 visitNode(cbNode, (node as NamedTupleMember).type);
//         SyntaxKind::ObjectBindingPattern:
//         SyntaxKind::ArrayBindingPattern:
//             return visitNodes(cbNode, cbNodes, (node as BindingPattern).elements);
//         SyntaxKind::ArrayLiteralExpression:
//             return visitNodes(cbNode, cbNodes, (node as ArrayLiteralExpression).elements);
//         SyntaxKind::ObjectLiteralExpression:
//             return visitNodes(cbNode, cbNodes, (node as ObjectLiteralExpression).properties);
//         SyntaxKind::PropertyAccessExpression:
//             return visitNode(cbNode, (node as PropertyAccessExpression).expression) ||
//                 visitNode(cbNode, (node as PropertyAccessExpression).questionDotToken) ||
//                 visitNode(cbNode, (node as PropertyAccessExpression).name);
//         SyntaxKind::ElementAccessExpression:
//             return visitNode(cbNode, (node as ElementAccessExpression).expression) ||
//                 visitNode(cbNode, (node as ElementAccessExpression).questionDotToken) ||
//                 visitNode(cbNode, (node as ElementAccessExpression).argumentExpression);
//         SyntaxKind::CallExpression:
//         SyntaxKind::NewExpression:
//             return visitNode(cbNode, (node as CallExpression).expression) ||
//                 visitNode(cbNode, (node as CallExpression).questionDotToken) ||
//                 visitNodes(cbNode, cbNodes, (node as CallExpression).typeArguments) ||
//                 visitNodes(cbNode, cbNodes, (node as CallExpression).arguments);
//         SyntaxKind::TaggedTemplateExpression:
//             return visitNode(cbNode, (node as TaggedTemplateExpression).tag) ||
//                 visitNode(cbNode, (node as TaggedTemplateExpression).questionDotToken) ||
//                 visitNodes(cbNode, cbNodes, (node as TaggedTemplateExpression).typeArguments) ||
//                 visitNode(cbNode, (node as TaggedTemplateExpression).template);
//         SyntaxKind::TypeAssertionExpression:
//             return visitNode(cbNode, (node as TypeAssertion).type) ||
//                 visitNode(cbNode, (node as TypeAssertion).expression);
//         SyntaxKind::ParenthesizedExpression:
//             return visitNode(cbNode, (node as ParenthesizedExpression).expression);
//         SyntaxKind::DeleteExpression:
//             return visitNode(cbNode, (node as DeleteExpression).expression);
//         SyntaxKind::TypeOfExpression:
//             return visitNode(cbNode, (node as TypeOfExpression).expression);
//         SyntaxKind::VoidExpression:
//             return visitNode(cbNode, (node as VoidExpression).expression);
//         SyntaxKind::PrefixUnaryExpression:
//             return visitNode(cbNode, (node as PrefixUnaryExpression).operand);
//         SyntaxKind::YieldExpression:
//             return visitNode(cbNode, (node as YieldExpression).asteriskToken) ||
//                 visitNode(cbNode, (node as YieldExpression).expression);
//         SyntaxKind::AwaitExpression:
//             return visitNode(cbNode, (node as AwaitExpression).expression);
//         SyntaxKind::PostfixUnaryExpression:
//             return visitNode(cbNode, (node as PostfixUnaryExpression).operand);
//         SyntaxKind::BinaryExpression:
//             return visitNode(cbNode, (node as BinaryExpression).left) ||
//                 visitNode(cbNode, (node as BinaryExpression).operatorToken) ||
//                 visitNode(cbNode, (node as BinaryExpression).right);
//         SyntaxKind::AsExpression:
//             return visitNode(cbNode, (node as AsExpression).expression) ||
//                 visitNode(cbNode, (node as AsExpression).type);
//         SyntaxKind::NonNullExpression:
//             return visitNode(cbNode, (node as NonNullExpression).expression);
//         SyntaxKind::MetaProperty:
//             return visitNode(cbNode, (node as MetaProperty).name);
//         SyntaxKind::ConditionalExpression:
//             return visitNode(cbNode, (node as ConditionalExpression).condition) ||
//                 visitNode(cbNode, (node as ConditionalExpression).questionToken) ||
//                 visitNode(cbNode, (node as ConditionalExpression).whenTrue) ||
//                 visitNode(cbNode, (node as ConditionalExpression).colonToken) ||
//                 visitNode(cbNode, (node as ConditionalExpression).whenFalse);
//         SyntaxKind::SpreadElement:
//             return visitNode(cbNode, (node as SpreadElement).expression);
//         SyntaxKind::Block:
//         SyntaxKind::ModuleBlock:
//             return visitNodes(cbNode, cbNodes, (node as Block).statements);
//         SyntaxKind::SourceFile:
//             return visitNodes(cbNode, cbNodes, (node as SourceFile).statements) ||
//                 visitNode(cbNode, (node as SourceFile).endOfFileToken);
//         SyntaxKind::VariableStatement:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as VariableStatement).declarationList);
//         SyntaxKind::VariableDeclarationList:
//             return visitNodes(cbNode, cbNodes, (node as VariableDeclarationList).declarations);
//         SyntaxKind::ExpressionStatement:
//             return visitNode(cbNode, (node as ExpressionStatement).expression);
//         SyntaxKind::IfStatement:
//             return visitNode(cbNode, (node as IfStatement).expression) ||
//                 visitNode(cbNode, (node as IfStatement).thenStatement) ||
//                 visitNode(cbNode, (node as IfStatement).elseStatement);
//         SyntaxKind::DoStatement:
//             return visitNode(cbNode, (node as DoStatement).statement) ||
//                 visitNode(cbNode, (node as DoStatement).expression);
//         SyntaxKind::WhileStatement:
//             return visitNode(cbNode, (node as WhileStatement).expression) ||
//                 visitNode(cbNode, (node as WhileStatement).statement);
//         SyntaxKind::ForStatement:
//             return visitNode(cbNode, (node as ForStatement).initializer) ||
//                 visitNode(cbNode, (node as ForStatement).condition) ||
//                 visitNode(cbNode, (node as ForStatement).incrementor) ||
//                 visitNode(cbNode, (node as ForStatement).statement);
//         SyntaxKind::ForInStatement:
//             return visitNode(cbNode, (node as ForInStatement).initializer) ||
//                 visitNode(cbNode, (node as ForInStatement).expression) ||
//                 visitNode(cbNode, (node as ForInStatement).statement);
//         SyntaxKind::ForOfStatement:
//             return visitNode(cbNode, (node as ForOfStatement).awaitModifier) ||
//                 visitNode(cbNode, (node as ForOfStatement).initializer) ||
//                 visitNode(cbNode, (node as ForOfStatement).expression) ||
//                 visitNode(cbNode, (node as ForOfStatement).statement);
//         SyntaxKind::ContinueStatement:
//         SyntaxKind::BreakStatement:
//             return visitNode(cbNode, (node as BreakOrContinueStatement).label);
//         SyntaxKind::ReturnStatement:
//             return visitNode(cbNode, (node as ReturnStatement).expression);
//         SyntaxKind::WithStatement:
//             return visitNode(cbNode, (node as WithStatement).expression) ||
//                 visitNode(cbNode, (node as WithStatement).statement);
//         SyntaxKind::SwitchStatement:
//             return visitNode(cbNode, (node as SwitchStatement).expression) ||
//                 visitNode(cbNode, (node as SwitchStatement).caseBlock);
//         SyntaxKind::CaseBlock:
//             return visitNodes(cbNode, cbNodes, (node as CaseBlock).clauses);
//         SyntaxKind::CaseClause:
//             return visitNode(cbNode, (node as CaseClause).expression) ||
//                 visitNodes(cbNode, cbNodes, (node as CaseClause).statements);
//         SyntaxKind::DefaultClause:
//             return visitNodes(cbNode, cbNodes, (node as DefaultClause).statements);
//         SyntaxKind::LabeledStatement:
//             return visitNode(cbNode, (node as LabeledStatement).label) ||
//                 visitNode(cbNode, (node as LabeledStatement).statement);
//         SyntaxKind::ThrowStatement:
//             return visitNode(cbNode, (node as ThrowStatement).expression);
//         SyntaxKind::TryStatement:
//             return visitNode(cbNode, (node as TryStatement).tryBlock) ||
//                 visitNode(cbNode, (node as TryStatement).catchClause) ||
//                 visitNode(cbNode, (node as TryStatement).finallyBlock);
//         SyntaxKind::CatchClause:
//             return visitNode(cbNode, (node as CatchClause).variableDeclaration) ||
//                 visitNode(cbNode, (node as CatchClause).block);
//         SyntaxKind::Decorator:
//             return visitNode(cbNode, (node as Decorator).expression);
//         SyntaxKind::ClassDeclaration:
//         SyntaxKind::ClassExpression:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as ClassLikeDeclaration).name) ||
//                 visitNodes(cbNode, cbNodes, (node as ClassLikeDeclaration).typeParameters) ||
//                 visitNodes(cbNode, cbNodes, (node as ClassLikeDeclaration).heritageClauses) ||
//                 visitNodes(cbNode, cbNodes, (node as ClassLikeDeclaration).members);
//         SyntaxKind::InterfaceDeclaration:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as InterfaceDeclaration).name) ||
//                 visitNodes(cbNode, cbNodes, (node as InterfaceDeclaration).typeParameters) ||
//                 visitNodes(cbNode, cbNodes, (node as ClassDeclaration).heritageClauses) ||
//                 visitNodes(cbNode, cbNodes, (node as InterfaceDeclaration).members);
//         SyntaxKind::TypeAliasDeclaration:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as TypeAliasDeclaration).name) ||
//                 visitNodes(cbNode, cbNodes, (node as TypeAliasDeclaration).typeParameters) ||
//                 visitNode(cbNode, (node as TypeAliasDeclaration).type);
//         SyntaxKind::EnumDeclaration:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as EnumDeclaration).name) ||
//                 visitNodes(cbNode, cbNodes, (node as EnumDeclaration).members);
//         SyntaxKind::EnumMember:
//             return visitNode(cbNode, (node as EnumMember).name) ||
//                 visitNode(cbNode, (node as EnumMember).initializer);
//         SyntaxKind::ModuleDeclaration:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as ModuleDeclaration).name) ||
//                 visitNode(cbNode, (node as ModuleDeclaration).body);
//         SyntaxKind::ImportEqualsDeclaration:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as ImportEqualsDeclaration).name) ||
//                 visitNode(cbNode, (node as ImportEqualsDeclaration).moduleReference);
//         SyntaxKind::ImportDeclaration:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as ImportDeclaration).importClause) ||
//                 visitNode(cbNode, (node as ImportDeclaration).moduleSpecifier) ||
//                 visitNode(cbNode, (node as ImportDeclaration).assertClause);
//         SyntaxKind::ImportClause:
//             return visitNode(cbNode, (node as ImportClause).name) ||
//                 visitNode(cbNode, (node as ImportClause).namedBindings);
//         SyntaxKind::AssertClause:
//             return visitNodes(cbNode, cbNodes, (node as AssertClause).elements);
//         SyntaxKind::AssertEntry:
//             return visitNode(cbNode, (node as AssertEntry).name) ||
//                 visitNode(cbNode, (node as AssertEntry).value);
//         SyntaxKind::NamespaceExportDeclaration:
//             return visitNode(cbNode, (node as NamespaceExportDeclaration).name);
//         SyntaxKind::NamespaceImport:
//             return visitNode(cbNode, (node as NamespaceImport).name);
//         SyntaxKind::NamespaceExport:
//             return visitNode(cbNode, (node as NamespaceExport).name);
//         SyntaxKind::NamedImports:
//         SyntaxKind::NamedExports:
//             return visitNodes(cbNode, cbNodes, (node as NamedImportsOrExports).elements);
//         SyntaxKind::ExportDeclaration:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as ExportDeclaration).exportClause) ||
//                 visitNode(cbNode, (node as ExportDeclaration).moduleSpecifier) ||
//                 visitNode(cbNode, (node as ExportDeclaration).assertClause);
//         SyntaxKind::ImportSpecifier:
//         SyntaxKind::ExportSpecifier:
//             return visitNode(cbNode, (node as ImportOrExportSpecifier).propertyName) ||
//                 visitNode(cbNode, (node as ImportOrExportSpecifier).name);
//         SyntaxKind::ExportAssignment:
//             return visitNodes(cbNode, cbNodes, node.decorators) ||
//                 visitNodes(cbNode, cbNodes, node.modifiers) ||
//                 visitNode(cbNode, (node as ExportAssignment).expression);
//         SyntaxKind::TemplateExpression:
//             return visitNode(cbNode, (node as TemplateExpression).head) || visitNodes(cbNode, cbNodes, (node as TemplateExpression).templateSpans);
//         SyntaxKind::TemplateSpan:
//             return visitNode(cbNode, (node as TemplateSpan).expression) || visitNode(cbNode, (node as TemplateSpan).literal);
//         SyntaxKind::TemplateLiteralType:
//             return visitNode(cbNode, (node as TemplateLiteralTypeNode).head) || visitNodes(cbNode, cbNodes, (node as TemplateLiteralTypeNode).templateSpans);
//         SyntaxKind::TemplateLiteralTypeSpan:
//             return visitNode(cbNode, (node as TemplateLiteralTypeSpan).type) || visitNode(cbNode, (node as TemplateLiteralTypeSpan).literal);
//         SyntaxKind::ComputedPropertyName:
//             return visitNode(cbNode, (node as ComputedPropertyName).expression);
//         SyntaxKind::HeritageClause:
//             return visitNodes(cbNode, cbNodes, (node as HeritageClause).types);
//         SyntaxKind::ExpressionWithTypeArguments:
//             return visitNode(cbNode, (node as ExpressionWithTypeArguments).expression) ||
//                 visitNodes(cbNode, cbNodes, (node as ExpressionWithTypeArguments).typeArguments);
//         SyntaxKind::ExternalModuleReference:
//             return visitNode(cbNode, (node as ExternalModuleReference).expression);
//         SyntaxKind::MissingDeclaration:
//             return visitNodes(cbNode, cbNodes, node.decorators);
//         SyntaxKind::CommaListExpression:
//             return visitNodes(cbNode, cbNodes, (node as CommaListExpression).elements);

//         SyntaxKind::JsxElement:
//             return visitNode(cbNode, (node as JsxElement).openingElement) ||
//                 visitNodes(cbNode, cbNodes, (node as JsxElement).children) ||
//                 visitNode(cbNode, (node as JsxElement).closingElement);
//         SyntaxKind::JsxFragment:
//             return visitNode(cbNode, (node as JsxFragment).openingFragment) ||
//                 visitNodes(cbNode, cbNodes, (node as JsxFragment).children) ||
//                 visitNode(cbNode, (node as JsxFragment).closingFragment);
//         SyntaxKind::JsxSelfClosingElement:
//         SyntaxKind::JsxOpeningElement:
//             return visitNode(cbNode, (node as JsxOpeningLikeElement).tagName) ||
//                 visitNodes(cbNode, cbNodes, (node as JsxOpeningLikeElement).typeArguments) ||
//                 visitNode(cbNode, (node as JsxOpeningLikeElement).attributes);
//         SyntaxKind::JsxAttributes:
//             return visitNodes(cbNode, cbNodes, (node as JsxAttributes).properties);
//         SyntaxKind::JsxAttribute:
//             return visitNode(cbNode, (node as JsxAttribute).name) ||
//                 visitNode(cbNode, (node as JsxAttribute).initializer);
//         SyntaxKind::JsxSpreadAttribute:
//             return visitNode(cbNode, (node as JsxSpreadAttribute).expression);
//         SyntaxKind::JsxExpression:
//             return visitNode(cbNode, (node as JsxExpression).dotDotDotToken) ||
//                 visitNode(cbNode, (node as JsxExpression).expression);
//         SyntaxKind::JsxClosingElement:
//             return visitNode(cbNode, (node as JsxClosingElement).tagName);

//         SyntaxKind::OptionalType:
//         SyntaxKind::RestType:
//         SyntaxKind::JSDocTypeExpression:
//         SyntaxKind::JSDocNonNullableType:
//         SyntaxKind::JSDocNullableType:
//         SyntaxKind::JSDocOptionalType:
//         SyntaxKind::JSDocVariadicType:
//             return visitNode(cbNode, (node as OptionalTypeNode | RestTypeNode | JSDocTypeExpression | JSDocTypeReferencingNode).type);
//         SyntaxKind::JSDocFunctionType:
//             return visitNodes(cbNode, cbNodes, (node as JSDocFunctionType).parameters) ||
//                 visitNode(cbNode, (node as JSDocFunctionType).type);
//         SyntaxKind::JSDocComment:
//             return (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined))
//                 || visitNodes(cbNode, cbNodes, (node as JSDoc).tags);
//         SyntaxKind::JSDocSeeTag:
//             return visitNode(cbNode, (node as JSDocSeeTag).tagName) ||
//                 visitNode(cbNode, (node as JSDocSeeTag).name) ||
//                 (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined));
//         SyntaxKind::JSDocNameReference:
//             return visitNode(cbNode, (node as JSDocNameReference).name);
//         SyntaxKind::JSDocMemberName:
//             return visitNode(cbNode, (node as JSDocMemberName).left) ||
//                 visitNode(cbNode, (node as JSDocMemberName).right);
//         SyntaxKind::JSDocParameterTag:
//         SyntaxKind::JSDocPropertyTag:
//             return visitNode(cbNode, (node as JSDocTag).tagName) ||
//                 ((node as JSDocPropertyLikeTag).isNameFirst
//                     ? visitNode(cbNode, (node as JSDocPropertyLikeTag).name) ||
//                         visitNode(cbNode, (node as JSDocPropertyLikeTag).typeExpression) ||
//                         (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined))
//                     : visitNode(cbNode, (node as JSDocPropertyLikeTag).typeExpression) ||
//                         visitNode(cbNode, (node as JSDocPropertyLikeTag).name) ||
//                         (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined)));
//         SyntaxKind::JSDocAuthorTag:
//             return visitNode(cbNode, (node as JSDocTag).tagName) ||
//                 (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined));
//         SyntaxKind::JSDocImplementsTag:
//             return visitNode(cbNode, (node as JSDocTag).tagName) ||
//                 visitNode(cbNode, (node as JSDocImplementsTag).class) ||
//                 (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined));
//         SyntaxKind::JSDocAugmentsTag:
//             return visitNode(cbNode, (node as JSDocTag).tagName) ||
//                 visitNode(cbNode, (node as JSDocAugmentsTag).class) ||
//                 (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined));
//         SyntaxKind::JSDocTemplateTag:
//             return visitNode(cbNode, (node as JSDocTag).tagName) ||
//                 visitNode(cbNode, (node as JSDocTemplateTag).constraint) ||
//                 visitNodes(cbNode, cbNodes, (node as JSDocTemplateTag).typeParameters) ||
//                 (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined));
//         SyntaxKind::JSDocTypedefTag:
//             return visitNode(cbNode, (node as JSDocTag).tagName) ||
//                 ((node as JSDocTypedefTag).typeExpression &&
//                     (node as JSDocTypedefTag).typeExpression!.kind === SyntaxKind::JSDocTypeExpression
//                     ? visitNode(cbNode, (node as JSDocTypedefTag).typeExpression) ||
//                         visitNode(cbNode, (node as JSDocTypedefTag).fullName) ||
//                         (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined))
//                     : visitNode(cbNode, (node as JSDocTypedefTag).fullName) ||
//                         visitNode(cbNode, (node as JSDocTypedefTag).typeExpression) ||
//                         (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined)));
//         SyntaxKind::JSDocCallbackTag:
//             return visitNode(cbNode, (node as JSDocTag).tagName) ||
//                 visitNode(cbNode, (node as JSDocCallbackTag).fullName) ||
//                 visitNode(cbNode, (node as JSDocCallbackTag).typeExpression) ||
//                 (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined));
//         SyntaxKind::JSDocReturnTag:
//         SyntaxKind::JSDocTypeTag:
//         SyntaxKind::JSDocThisTag:
//         SyntaxKind::JSDocEnumTag:
//             return visitNode(cbNode, (node as JSDocTag).tagName) ||
//                 visitNode(cbNode, (node as JSDocReturnTag | JSDocTypeTag | JSDocThisTag | JSDocEnumTag).typeExpression) ||
//                 (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined));
//         SyntaxKind::JSDocSignature:
//             return forEach((node as JSDocSignature).typeParameters, cbNode) ||
//                 forEach((node as JSDocSignature).parameters, cbNode) ||
//                 visitNode(cbNode, (node as JSDocSignature).type);
//         SyntaxKind::JSDocLink:
//         SyntaxKind::JSDocLinkCode:
//         SyntaxKind::JSDocLinkPlain:
//             return visitNode(cbNode, (node as JSDocLink | JSDocLinkCode | JSDocLinkPlain).name);
//         SyntaxKind::JSDocTypeLiteral:
//             return forEach((node as JSDocTypeLiteral).jsDocPropertyTags, cbNode);
//         SyntaxKind::JSDocTag:
//         SyntaxKind::JSDocClassTag:
//         SyntaxKind::JSDocPublicTag:
//         SyntaxKind::JSDocPrivateTag:
//         SyntaxKind::JSDocProtectedTag:
//         SyntaxKind::JSDocReadonlyTag:
//         SyntaxKind::JSDocDeprecatedTag:
//             return visitNode(cbNode, (node as JSDocTag).tagName)
//                 || (typeof (node as JSDoc).comment === "string" ? undefined : visitNodes(cbNode, cbNodes, (node as JSDoc).comment as NodeArray<JSDocComment> | undefined));
//         SyntaxKind::PartiallyEmittedExpression:
//             return visitNode(cbNode, (node as PartiallyEmittedExpression).expression);
//     }
// }

// /** @internal */
// /**
//  * Invokes a callback for each child of the given node. The 'cbNode' callback is invoked for all child nodes
//  * stored in properties. If a 'cbNodes' callback is specified, it is invoked for embedded arrays; additionally,
//  * unlike `forEachChild`, embedded arrays are flattened and the 'cbNode' callback is invoked for each element.
//  *  If a callback returns a truthy value, iteration stops and that value is returned. Otherwise, undefined is returned.
//  *
//  * @param node a given node to visit its children
//  * @param cbNode a callback to be invoked for all child nodes
//  * @param cbNodes a callback to be invoked for embedded array
//  *
//  * @remarks Unlike `forEachChild`, `forEachChildRecursively` handles recursively invoking the traversal on each child node found,
//  * and while doing so, handles traversing the structure without relying on the callstack to encode the tree structure.
//  */
// export function forEachChildRecursively<T>(rootNode: Node, cbNode: (node: Node, parent: Node) => T | "skip" | undefined, cbNodes?: (nodes: NodeArray<Node>, parent: Node) => T | "skip" | undefined): T | undefined {
//     const queue: (Node | NodeArray<Node>)[] = gatherPossibleChildren(rootNode);
//     const parents: Node[] = []; // tracks parent references for elements in queue
//     while (parents.length < queue.length) {
//         parents.push(rootNode);
//     }
//     while (queue.length !== 0) {
//         const current = queue.pop()!;
//         const parent = parents.pop()!;
//         if (isArray(current)) {
//             if (cbNodes) {
//                 const res = cbNodes(current, parent);
//                 if (res) {
//                     if (res === "skip") continue;
//                     return res;
//                 }
//             }
//             for (let i = current.length - 1; i >= 0; --i) {
//                 queue.push(current[i]);
//                 parents.push(parent);
//             }
//         }
//         else {
//             const res = cbNode(current, parent);
//             if (res) {
//                 if (res === "skip") continue;
//                 return res;
//             }
//             if (current.kind >= SyntaxKind::FirstNode) {
//                 // add children in reverse order to the queue, so popping gives the first child
//                 for (const child of gatherPossibleChildren(current)) {
//                     queue.push(child);
//                     parents.push(current);
//                 }
//             }
//         }
//     }
// }

// function gatherPossibleChildren(node: Node) {
//     const children: (Node | NodeArray<Node>)[] = [];
//     forEachChild(node, addWorkItem, addWorkItem); // By using a stack above and `unshift` here, we emulate a depth-first preorder traversal
//     return children;

//     function addWorkItem(n: Node | NodeArray<Node>) {
//         children.unshift(n);
//     }
// }

pub fn createSourceFile(
    fileName: Rc<str>,
    sourceText: Rc<str>,
    languageVersion: ScriptTarget,
    setParentNodes: bool,
    mut scriptKind: Option<ScriptKind>,
) -> SourceFile {
    // tracing?.push(tracing.Phase.Parse, "createSourceFile", { path: fileName }, /*separateBeginAndEnd*/ true);
    // performance.mark("beforeParse");

    // perfLogger.logStartParseSourceFile(fileName);
    let result = if languageVersion == ScriptTarget::JSON {
        Parser::parseSourceFile(
            fileName,
            sourceText,
            languageVersion,
            None,
            setParentNodes,
            Some(ScriptKind::JSON),
        )
    } else {
        Parser::parseSourceFile(
            fileName,
            sourceText,
            languageVersion,
            None,
            setParentNodes,
            scriptKind,
        )
    };
    // perfLogger.logStopParseSourceFile();

    // performance.mark("afterParse");
    // performance.measure("Parse", "beforeParse", "afterParse");
    // tracing?.pop();
    result
}

// export function parseIsolatedEntityName(text: string, languageVersion: ScriptTarget): EntityName | undefined {
//     return Parser.parseIsolatedEntityName(text, languageVersion);
// }

// /**
//  * Parse json text into SyntaxTree and return node and parse errors if any
//  * @param fileName
//  * @param sourceText
//  */
// export function parseJsonText(fileName: string, sourceText: string): JsonSourceFile {
//     return Parser.parseJsonText(fileName, sourceText);
// }

// See also `isExternalOrCommonJsModule` in utilities.ts
fn isExternalModule(file: &SourceFile) -> bool {
    file.externalModuleIndicator.is_some()
}

// // Produces a new SourceFile for the 'newText' provided. The 'textChangeRange' parameter
// // indicates what changed between the 'text' that this SourceFile has and the 'newText'.
// // The SourceFile will be created with the compiler attempting to reuse as many nodes from
// // this file as possible.
// //
// // Note: this function mutates nodes from this SourceFile. That means any existing nodes
// // from this SourceFile that are being held onto may change as a result (including
// // becoming detached from any SourceFile).  It is recommended that this SourceFile not
// // be used once 'update' is called on it.
// export function updateSourceFile(sourceFile: SourceFile, newText: string, textChangeRange: TextChangeRange, aggressiveChecks = false): SourceFile {
//     const newSourceFile = IncrementalParser.updateSourceFile(sourceFile, newText, textChangeRange, aggressiveChecks);
//     // Because new source file node is created, it may not have the flag PossiblyContainDynamicImport. This is the if there is no new edit to add dynamic import.
//     // We will manually port the flag to the new source file.
//     (newSourceFile as Mutable<SourceFile>).flags |= (sourceFile.flags & NodeFlags.PermanentlySetIncrementalFlags);
//     return newSourceFile;
// }

// /* @internal */
// export function parseIsolatedJSDocComment(content: string, start?: number, length?: number) {
//     const result = Parser.JSDocParser.parseIsolatedJSDocComment(content, start, length);
//     if (result && result.jsDoc) {
//         // because the jsDocComment was parsed out of the source file, it might
//         // not be covered by the fixupParentReferences.
//         Parser.fixupParentReferences(result.jsDoc);
//     }

//     return result;
// }

// /* @internal */
// // Exposed only for testing.
// export function parseJSDocTypeExpressionForTests(content: string, start?: number, length?: number) {
//     return Parser.JSDocParser.parseJSDocTypeExpressionForTests(content, start, length);
// }

// // Implement the parser as a singleton module.  We do this for perf reasons because creating
// // parser instances can actually be expensive enough to impact us on projects with many source
// // files.
// namespace Parser {
//     // Share a single scanner across all calls to parse a source file.  This helps speed things
//     // up by avoiding the cost of creating/compiling scanners over and over again.
//     const scanner = createScanner(ScriptTarget.Latest, /*skipTrivia*/ true);

const disallowInAndDecoratorContext: NodeFlags = NodeFlags::from_bits_truncate(
    NodeFlags::DisallowInContext.bits() | NodeFlags::DecoratorContext.bits(),
);

//     function countNode(node: Node) {
//         nodeCount++;
//         return node;
//     }

// }

#[derive(Clone)]
struct Parser {
    factory: NodeFactory,
    fileName: Rc<str>,
    sourceFlags: NodeFlags,
    sourceText: Rc<str>,
    languageVersion: ScriptTarget,
    scriptKind: ScriptKind,
    languageVariant: LanguageVariant,
    parseDiagnostics: Vec<DiagnosticWithDetachedLocation>,
    jsDocDiagnostics: Vec<DiagnosticWithDetachedLocation>,
    syntaxCursor: Option<SyntaxCursor>,

    currentToken: SyntaxKind,
    nodeCount: usize,
    // identifiers: FxHashMap<String, String>,
    // privateIdentifiers: FxHashMap<String, String>,
    identifierCount: usize,

    parsingContext: ParsingContext,

    notParenthesizedArrow: FxHashSet<usize>,

    // Flags that dictate what parsing context we're in.  For example:
    // Whether or not we are in strict parsing mode.  All that changes in strict parsing mode is
    // that some tokens that would be considered identifiers may be considered keywords.
    //
    // When adding more parser context flags, consider which is the more common that the
    // flag will be in.  This should be the 'false' state for that flag.  The reason for this is
    // that we don't store data in our nodes unless the value is in the *non-default* state.  So,
    // for example, more often than code 'allows-in' (or doesn't 'disallow-in').  We opt for
    // 'disallow-in' set to 'false'.  Otherwise, if we had 'allowsIn' set to 'true', then almost
    // all nodes would need extra state on them to store this info.
    //
    // Note: 'allowIn' and 'allowYield' track 1:1 with the [in] and [yield] concepts in the ES6
    // grammar specification.
    //
    // An important thing about these context concepts.  By default they are effectively inherited
    // while parsing through every grammar production.  i.e. if you don't change them, then when
    // you parse a sub-production, it will have the same context values as the parent production.
    // This is great most of the time.  After all, consider all the 'expression' grammar productions
    // and how nearly all of them pass along the 'in' and 'yield' context values:
    //
    // EqualityExpression[In, Yield] :
    //      RelationalExpression[?In, ?Yield]
    //      EqualityExpression[?In, ?Yield] == RelationalExpression[?In, ?Yield]
    //      EqualityExpression[?In, ?Yield] != RelationalExpression[?In, ?Yield]
    //      EqualityExpression[?In, ?Yield] === RelationalExpression[?In, ?Yield]
    //      EqualityExpression[?In, ?Yield] !== RelationalExpression[?In, ?Yield]
    //
    // Where you have to be careful is then understanding what the points are in the grammar
    // where the values are *not* passed along.  For example:
    //
    // SingleNameBinding[Yield,GeneratorParameter]
    //      [+GeneratorParameter]BindingIdentifier[Yield] Initializer[In]opt
    //      [~GeneratorParameter]BindingIdentifier[?Yield]Initializer[In, ?Yield]opt
    //
    // Here this is saying that if the GeneratorParameter context flag is set, that we should
    // explicitly set the 'yield' context flag to false before calling into the BindingIdentifier
    // and we should explicitly unset the 'yield' context flag before calling into the Initializer.
    // production.  Conversely, if the GeneratorParameter context flag is not set, then we
    // should leave the 'yield' context flag alone.
    //
    // Getting this all correct is tricky and requires careful reading of the grammar to
    // understand when these values should be changed versus when they should be inherited.
    //
    // Note: it should not be necessary to save/restore these flags during speculative/lookahead
    // parsing.  These context flags are naturally stored and restored through normal recursive
    // descent parsing and unwinding.
    contextFlags: NodeFlags,

    // Indicates whether we are currently parsing top-level statements.
    topLevel: bool,

    // Whether or not we've had a parse error since creating the last AST node.  If we have
    // encountered an error, it will be stored on the next AST node we create.  Parse errors
    // can be broken down into three categories:
    //
    // 1) An error that occurred during scanning.  For example, an unterminated literal, or a
    //    character that was completely not understood.
    //
    // 2) A token was expected, but was not present.  This type of error is commonly produced
    //    by the 'parseExpected' function.
    //
    // 3) A token was present that no parsing function was able to consume.  This type of error
    //    only occurs in the 'abortParsingListOrMoveToNextToken' function when the parser
    //    decides to skip the token.
    //
    // In all of these cases, we want to mark the next node as having had an error before it.
    // With this mark, we can know in incremental settings if this node can be reused, or if
    // we have to reparse it.  If we don't keep this information around, we may just reuse the
    // node.  in that event we would then not produce the same errors as we did before, causing
    // significant confusion problems.
    //
    // Note: it is necessary that this value be saved/restored during speculative/lookahead
    // parsing.  During lookahead parsing, we will often create a node.  That node will have
    // this value attached, and then this value will be set back to 'false'.  If we decide to
    // rewind, we must get back to the same value we had prior to the lookahead.
    //
    // Note: any errors at the end of the file that do not precede a regular node, should get
    // attached to the EOF token.
    parseErrorBeforeNextFinishedNode: bool,

    scanner: Scanner,
}

// #[derive(Default)]
// struct BaseNodeFactoryForParser {
//     base: DefaultBaseNodeFactory,
//     nodeCount: usize,
// }

// impl BaseNodeFactory for BaseNodeFactoryForParser {
//     fn createBaseSourceFileNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T {
//         self.nodeCount += 1;
//         self.base.createBaseSourceFileNode(kind)
//     }
//     fn createBaseIdentifierNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T {
//         self.nodeCount += 1;
//         self.base.createBaseIdentifierNode(kind)
//     }
//     fn createBasePrivateIdentifierNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T {
//         self.nodeCount += 1;
//         self.base.createBasePrivateIdentifierNode(kind)
//     }
//     fn createBaseTokenNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T {
//         self.nodeCount += 1;
//         self.base.createBaseTokenNode(kind)
//     }
//     fn createBaseNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T {
//         self.nodeCount += 1;
//         self.base.createBaseNode(kind)
//     }
// }

impl Parser {
    fn new(
        fileName: Rc<str>,
        sourceText: Rc<str>,
        languageVersion: ScriptTarget,
        syntaxCursor: Option<SyntaxCursor>,
        scriptKind: ScriptKind,
    ) -> Parser {
        let contextFlags = match scriptKind {
            ScriptKind::JS | ScriptKind::JSX => NodeFlags::JavaScriptFile,
            ScriptKind::JSON => NodeFlags::JavaScriptFile | NodeFlags::JsonFile,
            _ => NodeFlags::None,
        };
        let languageVariant = getLanguageVariant(scriptKind);

        let factory = NodeFactory::createNodeFactory(
            NodeFactoryFlags::NoParenthesizerRules
                | NodeFactoryFlags::NoNodeConverters
                | NodeFactoryFlags::NoOriginalNode,
        );

        Self {
            factory,
            fileName: normalizePath(fileName),
            sourceFlags: NodeFlags::empty(),
            sourceText: sourceText.clone(),
            languageVersion,
            scriptKind,
            languageVariant,
            parseDiagnostics: Vec::new(),
            jsDocDiagnostics: Vec::new(),
            syntaxCursor,
            currentToken: SyntaxKind::Unknown,
            nodeCount: 0,
            // identifiers: FxHashMap::default(),
            // privateIdentifiers: FxHashMap::default(),
            identifierCount: 0,
            parsingContext: ParsingContext::SourceElements,
            notParenthesizedArrow: FxHashSet::default(),
            contextFlags,
            topLevel: true,
            parseErrorBeforeNextFinishedNode: false,
            scanner: Scanner::createScanner(
                languageVersion,
                true,
                sourceText,
                languageVariant,
                true,
            ),
        }
    }

    /// Should be called after any operation where the scanner might generate an error.
    fn emit_scanner_errors(&mut self) {
        for error in self.scanner.take_errors() {
            self.parseErrorAtPosition::<u8>(error.start, error.length, error.message, None);
        }
    }
}

impl Parser {
    fn parseSourceFile(
        fileName: Rc<str>,
        sourceText: Rc<str>,
        languageVersion: ScriptTarget,
        syntaxCursor: Option<SyntaxCursor>,
        setParentNodes: bool,
        scriptKind: Option<ScriptKind>,
    ) -> SourceFile {
        let scriptKind = ensureScriptKind(&fileName, scriptKind);
        if scriptKind == ScriptKind::JSON {
            todo!();
            // let result = parseJsonText(
            //     fileName,
            //     sourceText,
            //     languageVersion,
            //     syntaxCursor,
            //     setParentNodes,
            // );
            // convertToObjectWorker(
            //     result,
            //     result.statements[0]?.expression,
            //     result.parseDiagnostics,
            //     /*returnValue*/ false,
            //     /*knownRootOptions*/ undefined,
            //     /*jsonConversionNotifier*/ undefined,
            // );
            // result.referencedFiles = emptyArray;
            // result.typeReferenceDirectives = emptyArray;
            // result.libReferenceDirectives = emptyArray;
            // result.amdDependencies = emptyArray;
            // result.hasNoDefaultLib = false;
            // result.pragmas = emptyMap as ReadonlyPragmaMap;
            // return result;
        }

        let mut parser = Parser::new(
            fileName,
            sourceText,
            languageVersion,
            syntaxCursor,
            scriptKind,
        );

        let result = parser.parseSourceFileWorker(languageVersion, setParentNodes, scriptKind);

        // clearState();

        result
    }

    //     export function parseIsolatedEntityName(content: string, languageVersion: ScriptTarget): EntityName | undefined {
    //         // Choice of `isDeclarationFile` should be arbitrary
    //         initializeState("", content, languageVersion, /*syntaxCursor*/ undefined, ScriptKind.JS);
    //         // Prime the scanner.
    //         nextToken();
    //         const entityName = parseEntityName(/*allowReservedWords*/ true);
    //         const isInvalid = token() === SyntaxKind::EndOfFileToken && !parseDiagnostics.length;
    //         clearState();
    //         return isInvalid ? entityName : undefined;
    //     }

    //     export function parseJsonText(fileName: string, sourceText: string, languageVersion: ScriptTarget = ScriptTarget.ES2015, syntaxCursor?: IncrementalParser.SyntaxCursor, setParentNodes = false): JsonSourceFile {
    //         initializeState(fileName, sourceText, languageVersion, syntaxCursor, ScriptKind.JSON);
    //         sourceFlags = contextFlags;

    //         // Prime the scanner.
    //         nextToken();
    //         const pos = getNodePos();
    //         let statements, endOfFileToken;
    //         if (token() === SyntaxKind::EndOfFileToken) {
    //             statements = createNodeArray([], pos, pos);
    //             endOfFileToken = parseTokenNode<EndOfFileToken>();
    //         }
    //         else {
    //             // Loop and synthesize an ArrayLiteralExpression if there are more than
    //             // one top-level expressions to ensure all input text is consumed.
    //             let expressions: Expression[] | Expression | undefined;
    //             while (token() !== SyntaxKind::EndOfFileToken) {
    //                 let expression;
    //                 switch (token()) {
    //                     SyntaxKind::OpenBracketToken:
    //                         expression = parseArrayLiteralExpression();
    //                         break;
    //                     SyntaxKind::TrueKeyword:
    //                     SyntaxKind::FalseKeyword:
    //                     SyntaxKind::NullKeyword:
    //                         expression = parseTokenNode<BooleanLiteral | NullLiteral>();
    //                         break;
    //                     SyntaxKind::MinusToken:
    //                         if (lookAhead(() => nextToken() === SyntaxKind::NumericLiteral && nextToken() !== SyntaxKind::ColonToken)) {
    //                             expression = parsePrefixUnaryExpression() as JsonMinusNumericLiteral;
    //                         }
    //                         else {
    //                             expression = parseObjectLiteralExpression();
    //                         }
    //                         break;
    //                     SyntaxKind::NumericLiteral:
    //                     SyntaxKind::StringLiteral:
    //                         if (lookAhead(() => nextToken() !== SyntaxKind::ColonToken)) {
    //                             expression = parseLiteralNode() as StringLiteral | NumericLiteral;
    //                             break;
    //                         }
    //                         // falls through
    //                     default:
    //                         expression = parseObjectLiteralExpression();
    //                         break;
    //                 }

    //                 // Error recovery: collect multiple top-level expressions
    //                 if (expressions && isArray(expressions)) {
    //                     expressions.push(expression);
    //                 }
    //                 else if (expressions) {
    //                     expressions = [expressions, expression];
    //                 }
    //                 else {
    //                     expressions = expression;
    //                     if (token() !== SyntaxKind::EndOfFileToken) {
    //                         parseErrorAtCurrentToken(Diagnostics::Unexpected_token);
    //                     }
    //                 }
    //             }

    //             const expression = isArray(expressions) ? finishNode(factory.createArrayLiteralExpression(expressions), pos) : Debug.checkDefined(expressions);
    //             const statement = factory.createExpressionStatement(expression) as JsonObjectExpressionStatement;
    //             finishNode(statement, pos);
    //             statements = createNodeArray([statement], pos);
    //             endOfFileToken = parseExpectedToken(SyntaxKind::EndOfFileToken, Diagnostics::Unexpected_token);
    //         }

    //         // Set source file so that errors will be reported with this file name
    //         const sourceFile = createSourceFile(fileName, ScriptTarget.ES2015, ScriptKind.JSON, /*isDeclaration*/ false, statements, endOfFileToken, sourceFlags);

    //         if (setParentNodes) {
    //             fixupParentReferences(sourceFile);
    //         }

    //         sourceFile.nodeCount = nodeCount;
    //         sourceFile.identifierCount = identifierCount;
    //         sourceFile.identifiers = identifiers;
    //         sourceFile.parseDiagnostics = attachFileToDiagnostics(parseDiagnostics, sourceFile);
    //         if (jsDocDiagnostics) {
    //             sourceFile.jsDocDiagnostics = attachFileToDiagnostics(jsDocDiagnostics, sourceFile);
    //         }

    //         const result = sourceFile as JsonSourceFile;
    //         clearState();
    //         return result;
    //     }

    //     function initializeState(_fileName: string, _sourceText: string, _languageVersion: ScriptTarget, _syntaxCursor: IncrementalParser.SyntaxCursor | undefined, _scriptKind: ScriptKind) {
    //         NodeConstructor = objectAllocator.getNodeConstructor();
    //         TokenConstructor = objectAllocator.getTokenConstructor();
    //         IdentifierConstructor = objectAllocator.getIdentifierConstructor();
    //         PrivateIdentifierConstructor = objectAllocator.getPrivateIdentifierConstructor();
    //         SourceFileConstructor = objectAllocator.getSourceFileConstructor();

    //         fileName = normalizePath(_fileName);
    //         sourceText = _sourceText;
    //         languageVersion = _languageVersion;
    //         syntaxCursor = _syntaxCursor;
    //         scriptKind = _scriptKind;
    //         languageVariant = getLanguageVariant(_scriptKind);

    //         parseDiagnostics = [];
    //         parsingContext = 0;
    //         identifiers = new Map<string, string>();
    //         privateIdentifiers = new Map<string, string>();
    //         identifierCount = 0;
    //         nodeCount = 0;
    //         sourceFlags = 0;
    //         topLevel = true;

    //         switch (scriptKind) {
    //             ScriptKind.JS:
    //             ScriptKind.JSX:
    //                 contextFlags = NodeFlags.JavaScriptFile;
    //                 break;
    //             ScriptKind.JSON:
    //                 contextFlags = NodeFlags.JavaScriptFile | NodeFlags.JsonFile;
    //                 break;
    //             default:
    //                 contextFlags = NodeFlags.None;
    //                 break;
    //         }
    //         parseErrorBeforeNextFinishedNode = false;

    //         // Initialize and prime the scanner before parsing the source elements.
    //         scanner.setText(sourceText);
    //         scanner.setOnError(scanError);
    //         scanner.setScriptTarget(languageVersion);
    //         scanner.setLanguageVariant(languageVariant);
    //     }

    //     function clearState() {
    //         // Clear out the text the scanner is pointing at, so it doesn't keep anything alive unnecessarily.
    //         scanner.clearCommentDirectives();
    //         scanner.setText("");
    //         scanner.setOnError(undefined);

    //         // Clear any data.  We don't want to accidentally hold onto it for too long.
    //         sourceText = undefined!;
    //         languageVersion = undefined!;
    //         syntaxCursor = undefined;
    //         scriptKind = undefined!;
    //         languageVariant = undefined!;
    //         sourceFlags = 0;
    //         parseDiagnostics = undefined!;
    //         jsDocDiagnostics = undefined!;
    //         parsingContext = 0;
    //         identifiers = undefined!;
    //         notParenthesizedArrow = undefined;
    //         topLevel = true;
    //     }

    fn parseSourceFileWorker(
        &mut self,
        languageVersion: ScriptTarget,
        setParentNodes: bool,
        scriptKind: ScriptKind,
    ) -> SourceFile {
        let isDeclarationFile = isDeclarationFileName(&self.fileName);
        if isDeclarationFile {
            self.contextFlags |= NodeFlags::Ambient;
        }

        self.sourceFlags = self.contextFlags;

        // Prime the scanner.
        self.nextToken();

        let statements = self.parseList(ParsingContext::SourceElements, |p| p.parseStatement());
        debug_assert!(self.token() == SyntaxKind::EndOfFileToken);
        let endOfFileToken = self.parseTokenNode::<EndOfFileToken>();
        let endOfFileToken = self.addJSDocComment(endOfFileToken);

        let mut sourceFile = self.createSourceFile(
            self.fileName.clone(),
            languageVersion,
            scriptKind,
            isDeclarationFile,
            statements,
            endOfFileToken,
            self.sourceFlags,
        );
        // TODO: pragmas
        // A member of ReadonlyArray<T> isn't assignable to a member of T[] (and prevents a direct cast) - but this is where we set up those members so they can be readonly in the future
        // processCommentPragmas(sourceFile as {} as PragmaContext, sourceText);
        // processPragmasIntoFields(sourceFile as {} as PragmaContext, reportPragmaDiagnostic);

        // TODO:
        // sourceFile.commentDirectives = scanner.getCommentDirectives();
        // sourceFile.nodeCount = nodeCount;
        // sourceFile.identifierCount = identifierCount;
        // sourceFile.identifiers = self.identifiers;
        // TODO: clones
        sourceFile.parseDiagnostics =
            attachFileToDiagnostics(self.parseDiagnostics.clone(), &sourceFile);
        if !self.jsDocDiagnostics.is_empty() {
            // TODO: clones
            sourceFile.jsDocDiagnostics =
                attachFileToDiagnostics(self.jsDocDiagnostics.clone(), &sourceFile);
        }

        if setParentNodes {
            todo!();
            // fixupParentReferences(sourceFile);
        }

        return sourceFile;

        // fn reportPragmaDiagnostic(pos: number, end: number, diagnostic: DiagnosticMessage) {
        //     parseDiagnostics.push(createDetachedDiagnostic(fileName, pos, end, diagnostic));
        // }
    }

    fn withJSDoc<T: HasJSDoc>(&mut self, node: T, hasJSDoc: bool) -> T {
        if hasJSDoc {
            self.addJSDocComment(node)
        } else {
            node
        }
    }

    //     let hasDeprecatedTag = false;
    fn addJSDocComment<T: HasJSDoc>(&mut self, node: T) -> T {
        debug_assert!(node.js_doc_container().jsDoc.is_none()); // Should only be called once per node

        // TODO: jsdoc
        node
        // let jsDoc = mapDefined(getJSDocCommentRanges(node, sourceText), |comment| {
        //     JSDocParser.parseJSDocComment(node, comment.pos, comment.end - comment.pos)
        // });
        // if (jsDoc.length) {
        //     node.jsDoc = jsDoc;
        // }
        // if hasDeprecatedTag {
        //     hasDeprecatedTag = false;
        //     node.flags |= NodeFlags::Deprecated;
        // }
        // node
    }

    //     function reparseTopLevelAwait(sourceFile: SourceFile) {
    //         const savedSyntaxCursor = syntaxCursor;
    //         const baseSyntaxCursor = IncrementalParser.createSyntaxCursor(sourceFile);
    //         syntaxCursor = { currentNode };

    //         const statements: Statement[] = [];
    //         const savedParseDiagnostics = parseDiagnostics;

    //         parseDiagnostics = [];

    //         let pos = 0;
    //         let start = findNextStatementWithAwait(sourceFile.statements, 0);
    //         while (start !== -1) {
    //             // append all statements between pos and start
    //             const prevStatement = sourceFile.statements[pos];
    //             const nextStatement = sourceFile.statements[start];
    //             addRange(statements, sourceFile.statements, pos, start);
    //             pos = findNextStatementWithoutAwait(sourceFile.statements, start);

    //             // append all diagnostics associated with the copied range
    //             const diagnosticStart = findIndex(savedParseDiagnostics, diagnostic => diagnostic.start >= prevStatement.pos);
    //             const diagnosticEnd = diagnosticStart >= 0 ? findIndex(savedParseDiagnostics, diagnostic => diagnostic.start >= nextStatement.pos, diagnosticStart) : -1;
    //             if (diagnosticStart >= 0) {
    //                 addRange(parseDiagnostics, savedParseDiagnostics, diagnosticStart, diagnosticEnd >= 0 ? diagnosticEnd : undefined);
    //             }

    //             // reparse all statements between start and pos. We skip existing diagnostics for the same range and allow the parser to generate new ones.
    //             speculationHelper(() => {
    //                 const savedContextFlags = contextFlags;
    //                 contextFlags |= NodeFlags.AwaitContext;
    //                 scanner.setTextPos(nextStatement.pos);
    //                 nextToken();

    //                 while (token() !== SyntaxKind::EndOfFileToken) {
    //                     const startPos = scanner.getStartPos();
    //                     const statement = parseListElement(ParsingContext::SourceElements, parseStatement);
    //                     statements.push(statement);
    //                     if (startPos === scanner.getStartPos()) {
    //                         nextToken();
    //                     }

    //                     if (pos >= 0) {
    //                         const nonAwaitStatement = sourceFile.statements[pos];
    //                         if (statement.end === nonAwaitStatement.pos) {
    //                             // done reparsing this section
    //                             break;
    //                         }
    //                         if (statement.end > nonAwaitStatement.pos) {
    //                             // we ate into the next statement, so we must reparse it.
    //                             pos = findNextStatementWithoutAwait(sourceFile.statements, pos + 1);
    //                         }
    //                     }
    //                 }

    //                 contextFlags = savedContextFlags;
    //             }, SpeculationKind.Reparse);

    //             // find the next statement containing an `await`
    //             start = pos >= 0 ? findNextStatementWithAwait(sourceFile.statements, pos) : -1;
    //         }

    //         // append all statements between pos and the end of the list
    //         if (pos >= 0) {
    //             const prevStatement = sourceFile.statements[pos];
    //             addRange(statements, sourceFile.statements, pos);

    //             // append all diagnostics associated with the copied range
    //             const diagnosticStart = findIndex(savedParseDiagnostics, diagnostic => diagnostic.start >= prevStatement.pos);
    //             if (diagnosticStart >= 0) {
    //                 addRange(parseDiagnostics, savedParseDiagnostics, diagnosticStart);
    //             }
    //         }

    //         syntaxCursor = savedSyntaxCursor;
    //         return factory.updateSourceFile(sourceFile, setTextRange(factory.createNodeArray(statements), sourceFile.statements));

    //         function containsPossibleTopLevelAwait(node: Node) {
    //             return !(node.flags & NodeFlags.AwaitContext)
    //                 && !!(node.transformFlags & TransformFlags.ContainsPossibleTopLevelAwait);
    //         }

    //         function findNextStatementWithAwait(statements: NodeArray<Statement>, start: number) {
    //             for (let i = start; i < statements.length; i++) {
    //                 if (containsPossibleTopLevelAwait(statements[i])) {
    //                     return i;
    //                 }
    //             }
    //             return -1;
    //         }

    //         function findNextStatementWithoutAwait(statements: NodeArray<Statement>, start: number) {
    //             for (let i = start; i < statements.length; i++) {
    //                 if (!containsPossibleTopLevelAwait(statements[i])) {
    //                     return i;
    //                 }
    //             }
    //             return -1;
    //         }

    //         function currentNode(position: number) {
    //             const node = baseSyntaxCursor.currentNode(position);
    //             if (topLevel && node && containsPossibleTopLevelAwait(node)) {
    //                 node.intersectsChange = true;
    //             }
    //             return node;
    //         }

    //     }

    //     export function fixupParentReferences(rootNode: Node) {
    //         // normally parent references are set during binding. However, for clients that only need
    //         // a syntax tree, and no semantic features, then the binding process is an unnecessary
    //         // overhead.  This functions allows us to set all the parents, without all the expense of
    //         // binding.
    //         setParentRecursive(rootNode, /*incremental*/ true);
    //     }

    fn createSourceFile(
        &mut self,
        fileName: Rc<str>,
        languageVersion: ScriptTarget,
        scriptKind: ScriptKind,
        isDeclarationFile: bool,
        statements: NodeArray<Statement>,
        endOfFileToken: EndOfFileToken,
        flags: NodeFlags,
    ) -> SourceFile {
        // code from createNode is inlined here so createNode won't have to deal with special of creating source files
        // this is quite rare comparing to other nodes and createNode should be as fast as possible
        let mut sourceFile = self
            .factory
            .createSourceFile(statements, endOfFileToken, flags);
        self.factory
            .node_data_mut(&sourceFile)
            .setTextRangePosWidth(0, self.sourceText.len());
        // TODO:
        // setExternalModuleIndicator(sourceFile);

        // If we parsed this as an external module, it may contain top-level await
        if !isDeclarationFile
            && isExternalModule(&sourceFile)
            && self
                .factory
                .node_data(&sourceFile)
                .transformFlags
                .intersects(TransformFlags::ContainsPossibleTopLevelAwait)
        {
            todo!();
            // sourceFile = reparseTopLevelAwait(sourceFile);
        }

        sourceFile.text = self.sourceText.clone();
        sourceFile.bindDiagnostics = Vec::new();
        sourceFile.bindSuggestionDiagnostics = None;
        sourceFile.languageVersion = languageVersion;
        sourceFile.fileName = fileName;
        sourceFile.languageVariant = getLanguageVariant(scriptKind);
        sourceFile.isDeclarationFile = isDeclarationFile;
        sourceFile.scriptKind = scriptKind;

        sourceFile
    }

    fn setContextFlag(&mut self, val: bool, flag: NodeFlags) {
        if val {
            self.contextFlags |= flag;
        } else {
            self.contextFlags &= !flag;
        }
    }

    fn setDisallowInContext(&mut self, val: bool) {
        self.setContextFlag(val, NodeFlags::DisallowInContext);
    }

    fn setYieldContext(&mut self, val: bool) {
        self.setContextFlag(val, NodeFlags::YieldContext);
    }

    fn setDecoratorContext(&mut self, val: bool) {
        self.setContextFlag(val, NodeFlags::DecoratorContext);
    }

    fn setAwaitContext(&mut self, val: bool) {
        self.setContextFlag(val, NodeFlags::AwaitContext);
    }

    fn doOutsideOfContext<F, T>(&mut self, context: NodeFlags, func: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
    {
        // contextFlagsToClear will contain only the context flags that are
        // currently set that we need to temporarily clear
        // We don't just blindly reset to the previous flags to ensure
        // that we do not mutate cached flags for the incremental
        // parser (ThisNodeHasError, ThisNodeOrAnySubNodesHasError, and
        // HasAggregatedChildData).
        let contextFlagsToClear = context & self.contextFlags;
        if !contextFlagsToClear.is_empty() {
            // clear the requested context flags
            self.setContextFlag(false, contextFlagsToClear);
            let result = func(self);
            // restore the context flags we just cleared
            self.setContextFlag(true, contextFlagsToClear);
            return result;
        }

        // no need to do anything special as we are not in any of the requested contexts
        func(self)
    }

    fn doInsideOfContext<T, F>(&mut self, context: NodeFlags, func: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
    {
        // contextFlagsToSet will contain only the context flags that
        // are not currently set that we need to temporarily enable.
        // We don't just blindly reset to the previous flags to ensure
        // that we do not mutate cached flags for the incremental
        // parser (ThisNodeHasError, ThisNodeOrAnySubNodesHasError, and
        // HasAggregatedChildData).
        let contextFlagsToSet = context & !self.contextFlags;
        if !contextFlagsToSet.is_empty() {
            // set the requested context flags
            self.setContextFlag(true, contextFlagsToSet);
            let result = func(self);
            // reset the context flags we just set
            self.setContextFlag(false, contextFlagsToSet);
            return result;
        }

        // no need to do anything special as we are already in all of the requested contexts
        func(self)
    }

    fn allowInAnd<F, T>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
    {
        self.doOutsideOfContext(NodeFlags::DisallowInContext, func)
    }

    fn disallowInAnd<T, F>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
    {
        self.doInsideOfContext(NodeFlags::DisallowInContext, func)
    }

    fn doInYieldContext<T, F>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
    {
        self.doInsideOfContext(NodeFlags::YieldContext, func)
    }

    fn doInDecoratorContext<T, F>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
    {
        self.doInsideOfContext(NodeFlags::DecoratorContext, func)
    }

    fn doInAwaitContext<T, F>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
    {
        self.doInsideOfContext(NodeFlags::AwaitContext, func)
    }

    fn doOutsideOfAwaitContext<T, F>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
    {
        self.doOutsideOfContext(NodeFlags::AwaitContext, func)
    }

    fn doInYieldAndAwaitContext<T, F>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
    {
        self.doInsideOfContext(NodeFlags::YieldContext | NodeFlags::AwaitContext, func)
    }

    fn doOutsideOfYieldAndAwaitContext<T, F>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
    {
        self.doOutsideOfContext(NodeFlags::YieldContext | NodeFlags::AwaitContext, func)
    }

    fn inContext(&self, flags: NodeFlags) -> bool {
        self.contextFlags.intersects(flags)
    }

    fn inYieldContext(&self) -> bool {
        self.inContext(NodeFlags::YieldContext)
    }

    fn inDisallowInContext(&self) -> bool {
        self.inContext(NodeFlags::DisallowInContext)
    }

    fn inDecoratorContext(&self) -> bool {
        self.inContext(NodeFlags::DecoratorContext)
    }

    fn inAwaitContext(&self) -> bool {
        self.inContext(NodeFlags::AwaitContext)
    }

    fn parseErrorAtCurrentToken<T: Display>(
        &mut self,
        message: DiagnosticMessage,
        arg0: Option<T>,
    ) {
        self.parseErrorAt(
            self.scanner.getTokenPos(),
            self.scanner.getTextPos(),
            message,
            arg0,
        );
    }

    fn parseErrorAtPosition<T: Display>(
        &mut self,
        start: usize,
        length: usize,
        message: DiagnosticMessage,
        arg0: Option<T>,
    ) {
        // Don't report another error if it would just be at the same position as the last error.
        let lastError = self.parseDiagnostics.last();
        if lastError.is_none() || Some(start) != lastError.unwrap().start {
            self.parseDiagnostics.push(createDetachedDiagnostic(
                self.fileName.clone(),
                start,
                length,
                message,
                arg0.map(|a| vec![a]),
            ));
        }

        // Mark that we've encountered an error.  We'll set an appropriate bit on the next
        // node we finish so that it can't be reused incrementally.
        self.parseErrorBeforeNextFinishedNode = true;
    }

    fn parseErrorAt<T: Display>(
        &mut self,
        start: usize,
        end: usize,
        message: DiagnosticMessage,
        arg0: Option<T>,
    ) {
        self.parseErrorAtPosition(start, end - start, message, arg0);
    }

    fn parseErrorAtRange<T: Display>(
        &mut self,
        range: TextRange,
        message: DiagnosticMessage,
        arg0: Option<T>,
    ) {
        self.parseErrorAt(range.pos, range.end, message, arg0);
    }

    fn scanError(&mut self, message: DiagnosticMessage, length: usize) {
        self.parseErrorAtPosition::<u8>(self.scanner.getTextPos(), length, message, None);
    }

    fn getNodePos(&self) -> usize {
        self.scanner.getStartPos()
    }

    fn hasPrecedingJSDocComment(&self) -> bool {
        self.scanner.hasPrecedingJSDocComment()
    }

    // Use this function to access the current token instead of reading the currentToken
    // variable. Since function results aren't narrowed in control flow analysis, this ensures
    // that the type checker doesn't make wrong assumptions about the type of the current
    // token (e.g. a call to nextToken() changes the current token but the checker doesn't
    // reason about this side effect).  Mainstream VMs inline simple functions like this, so
    // there is no performance penalty.
    fn token(&self) -> SyntaxKind {
        self.currentToken
    }

    fn nextTokenWithoutCheck(&mut self) -> SyntaxKind {
        let next = self.scanner.scan();
        self.emit_scanner_errors();
        self.currentToken = next;
        next
    }

    fn nextTokenAnd<T, F>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
    {
        self.nextToken();
        func(self)
    }

    fn nextToken(&mut self) -> SyntaxKind {
        // if the keyword had an escape
        if isKeyword(self.currentToken)
            && (self.scanner.hasUnicodeEscape() || self.scanner.hasExtendedUnicodeEscape())
        {
            // issue a parse error for the escape
            self.parseErrorAt::<u8>(
                self.scanner.getTokenPos(),
                self.scanner.getTextPos(),
                Diagnostics::Keywords_cannot_contain_escape_characters,
                None,
            );
        }
        self.nextTokenWithoutCheck()
    }

    //     function nextTokenJSDoc(): JSDocSyntaxKind {
    //         return currentToken = scanner.scanJsDocToken();
    //     }

    fn reScanGreaterToken(&mut self) -> SyntaxKind {
        self.currentToken = self.scanner.reScanGreaterToken();
        self.emit_scanner_errors();
        self.currentToken
    }

    fn reScanSlashToken(&mut self) -> SyntaxKind {
        self.currentToken = self.scanner.reScanSlashToken();
        self.emit_scanner_errors();
        self.currentToken
    }

    fn reScanTemplateToken(&mut self, isTaggedTemplate: bool) -> SyntaxKind {
        self.currentToken = self.scanner.reScanTemplateToken(isTaggedTemplate);
        self.emit_scanner_errors();
        self.currentToken
    }

    fn reScanTemplateHeadOrNoSubstitutionTemplate(&mut self) -> SyntaxKind {
        self.currentToken = self.scanner.reScanTemplateHeadOrNoSubstitutionTemplate();
        self.emit_scanner_errors();
        self.currentToken
    }

    fn reScanLessThanToken(&mut self) -> SyntaxKind {
        self.currentToken = self.scanner.reScanLessThanToken();
        self.emit_scanner_errors();
        self.currentToken
    }

    //     function reScanHashToken(): SyntaxKind {
    //         return currentToken = scanner.reScanHashToken();
    //     }

    //     function scanJsxIdentifier(): SyntaxKind {
    //         return currentToken = scanner.scanJsxIdentifier();
    //     }

    //     function scanJsxText(): SyntaxKind {
    //         return currentToken = scanner.scanJsxToken();
    //     }

    //     function scanJsxAttributeValue(): SyntaxKind {
    //         return currentToken = scanner.scanJsxAttributeValue();
    //     }

    fn speculationHelper<F, T>(&mut self, callback: F, speculationKind: SpeculationKind) -> T
    where
        F: FnOnce(&mut Parser) -> T,
        T: IsFalsy,
    {
        // Keep track of the state we'll need to rollback to if lookahead fails (or if the
        // caller asked us to always reset our state).
        let saveToken = self.currentToken;
        let saveParseDiagnosticsLength = self.parseDiagnostics.len();
        let saveParseErrorBeforeNextFinishedNode = self.parseErrorBeforeNextFinishedNode;

        // Note: it is not actually necessary to save/restore the context flags here.  That's
        // because the saving/restoring of these flags happens naturally through the recursive
        // descent nature of our parser.  However, we still store this here just so we can
        // assert that invariant holds.
        let saveContextFlags = self.contextFlags;

        let cloned = self.clone();

        // If we're only looking ahead, then tell the scanner to only lookahead as well.
        // Otherwise, if we're actually speculatively parsing, then tell the scanner to do the
        // same.
        let result = if speculationKind != SpeculationKind::TryParse {
            // self.scanner.lookAhead(|| callback(self))
            callback(self)
        } else {
            // self.scanner.tryScan(callback)
            callback(self)
        };

        debug_assert!(saveContextFlags == self.contextFlags);

        // If our callback returned something 'falsy' or we're just looking ahead,
        // then unconditionally restore us to where we were.
        if result.is_falsy() || speculationKind != SpeculationKind::TryParse {
            // self.currentToken = saveToken;
            // if (speculationKind != SpeculationKind::Reparse) {
            //     self.parseDiagnostics.length = saveParseDiagnosticsLength;
            // }
            // self.parseErrorBeforeNextFinishedNode = saveParseErrorBeforeNextFinishedNode;
            *self = cloned;
        }

        result
    }

    /** Invokes the provided callback then unconditionally restores the parser to the state it
     * was in immediately prior to invoking the callback.  The result of invoking the callback
     * is returned from this function.
     */
    fn lookAhead<F, T>(&mut self, callback: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
        T: IsFalsy,
    {
        self.speculationHelper(callback, SpeculationKind::Lookahead)
    }

    /** Invokes the provided callback.  If the callback returns something falsy, then it restores
     * the parser to the state it was in immediately prior to invoking the callback.  If the
     * callback returns something truthy, then the parser state is not rolled back.  The result
     * of invoking the callback is returned from this function.
     */
    fn tryParse<F, T>(&mut self, callback: F) -> T
    where
        F: FnOnce(&mut Parser) -> T,
        T: IsFalsy,
    {
        self.speculationHelper(callback, SpeculationKind::TryParse)
    }

    fn isBindingIdentifier(&self) -> bool {
        if self.token() == SyntaxKind::Identifier {
            return true;
        }

        // `let await`/`let yield` in [Yield] or [Await] are allowed here and disallowed in the binder.
        self.token() > SyntaxKind::LastReservedWord
    }

    // Ignore strict mode flag because we will report an error in type checker instead.
    fn isIdentifier(&self) -> bool {
        if self.token() == SyntaxKind::Identifier {
            return true;
        }

        // If we have a 'yield' keyword, and we're in the [yield] context, then 'yield' is
        // considered a keyword and is not an identifier.
        if self.token() == SyntaxKind::YieldKeyword && self.inYieldContext() {
            return false;
        }

        // If we have a 'await' keyword, and we're in the [Await] context, then 'await' is
        // considered a keyword and is not an identifier.
        if self.token() == SyntaxKind::AwaitKeyword && self.inAwaitContext() {
            return false;
        }

        self.token() > SyntaxKind::LastReservedWord
    }

    fn parseExpected(
        &mut self,
        kind: SyntaxKind,
        diagnosticMessage: Option<DiagnosticMessage>,
        shouldAdvance: Option<bool>,
    ) -> bool {
        if self.token() == kind {
            if shouldAdvance.unwrap_or(true) {
                self.nextToken();
            }
            return true;
        }

        // Report specific message if provided with one.  Otherwise, report generic fallback message.
        if let Some(diagnosticMessage) = diagnosticMessage {
            self.parseErrorAtCurrentToken::<u8>(diagnosticMessage, None);
        } else {
            self.parseErrorAtCurrentToken(Diagnostics::_0_expected, tokenToString(kind));
        }
        false
    }

    // const viableKeywordSuggestions = Object.keys(textToKeywordObj).filter(keyword => keyword.length > 2);

    /**
     * Provides a better error message than the generic "';' expected" if possible for
     * known common variants of a missing semicolon, such as from a mispelled names.
     *
     * @param node Node preceding the expected semicolon location.
     */
    fn parseErrorForMissingSemicolonAfter(&mut self, node: Node) {
        // Tagged template literals are sometimes used in places where only simple strings are allowed, i.e.:
        //   module `M1` {
        //   ^^^^^^^^^^^ This block is parsed as a template literal like module`M1`.
        if let Node::TaggedTemplateExpression(node) = &node {
            let template_range = *self.factory.node_data(&node.template).get_range();
            self.parseErrorAt::<u8>(
                skipTrivia(&self.sourceText, template_range.pos, false, false, false),
                template_range.end,
                Diagnostics::Module_declaration_names_may_only_use_or_quoted_strings,
                None,
            );
            return;
        }

        // Otherwise, if this isn't a well-known keyword-like identifier, give the generic fallback message.
        let expressionText = if let Node::Identifier(node) = &node {
            Some(idText(node))
        } else {
            None
        };
        if expressionText.is_none()
            || !isIdentifierText(
                expressionText.as_ref().unwrap(),
                Some(self.languageVersion),
                None,
            )
        {
            self.parseErrorAtCurrentToken(
                Diagnostics::_0_expected,
                tokenToString(SyntaxKind::SemicolonToken),
            );
            return;
        }
        let expressionText = expressionText.unwrap();

        let node_range = self.factory.node_data(&node).get_range();
        let node_end = node_range.end;

        let pos = skipTrivia(&self.sourceText, node_range.pos, false, false, false);

        // Some known keywords are likely signs of syntax being used improperly.
        match expressionText.as_ref() {
            "const" | "let" | "var" => {
                self.parseErrorAt::<u8>(
                    pos,
                    node_end,
                    Diagnostics::Variable_declaration_not_allowed_at_this_location,
                    None,
                );
                return;
            }
            "declare" => {
                // If a declared node failed to parse, it would have emitted a diagnostic already.
                return;
            }
            "interface" => {
                self.parseErrorForInvalidName(
                    Diagnostics::Interface_name_cannot_be_0,
                    Diagnostics::Interface_must_be_given_a_name,
                    SyntaxKind::OpenBraceToken,
                );
                return;
            }
            "is" => {
                self.parseErrorAt::<u8>(pos, self.scanner.getTextPos(), Diagnostics::A_type_predicate_is_only_allowed_in_return_type_position_for_functions_and_methods,None);
                return;
            }
            "module" | "namespace" => {
                self.parseErrorForInvalidName(
                    Diagnostics::Namespace_name_cannot_be_0,
                    Diagnostics::Namespace_must_be_given_a_name,
                    SyntaxKind::OpenBraceToken,
                );
                return;
            }
            "type" => {
                self.parseErrorForInvalidName(
                    Diagnostics::Type_alias_name_cannot_be_0,
                    Diagnostics::Type_alias_must_be_given_a_name,
                    SyntaxKind::EqualsToken,
                );
                return;
            }
            _ => {}
        }

        // The user alternatively might have misspelled or forgotten to add a space after a common keyword.
        let suggestion =
            getSpellingSuggestion(&expressionText, &viableKeywordSuggestions, |n| Some(n));
        if suggestion.is_some() {
            self.parseErrorAt(
                pos,
                node_end,
                Diagnostics::Unknown_keyword_or_identifier_Did_you_mean_0,
                suggestion,
            );
            return;
        } else {
            let suggestion = self.getSpaceSuggestion(&expressionText);
            if suggestion.is_some() {
                self.parseErrorAt(
                    pos,
                    node_end,
                    Diagnostics::Unknown_keyword_or_identifier_Did_you_mean_0,
                    suggestion,
                );
                return;
            }
        }

        // Unknown tokens are handled with their own errors in the scanner
        if self.token() == SyntaxKind::Unknown {
            return;
        }

        // Otherwise, we know this some kind of unknown word, not just a missing expected semicolon.
        self.parseErrorAt::<u8>(
            pos,
            node_end,
            Diagnostics::Unexpected_keyword_or_identifier,
            None,
        );
    }

    /**
     * Reports a diagnostic error for the current token being an invalid name.
     *
     * @param blankDiagnostic Diagnostic to report for the of the name being blank (matched tokenIfBlankName).
     * @param nameDiagnostic Diagnostic to report for all other cases.
     * @param tokenIfBlankName Current token if the name was invalid for being blank (not provided / skipped).
     */
    fn parseErrorForInvalidName(
        &mut self,
        nameDiagnostic: DiagnosticMessage,
        blankDiagnostic: DiagnosticMessage,
        tokenIfBlankName: SyntaxKind,
    ) {
        if self.token() == tokenIfBlankName {
            self.parseErrorAtCurrentToken::<u8>(blankDiagnostic, None);
        } else {
            let token_value = self.scanner.getTokenValue().to_string();
            self.parseErrorAtCurrentToken(nameDiagnostic, Some(token_value));
        }
    }

    fn getSpaceSuggestion(&self, expressionText: &str) -> Option<String> {
        for keyword in viableKeywordSuggestions {
            if expressionText.len() > keyword.len() + 2 && expressionText.starts_with(keyword) {
                return Some(format!("{} {}", keyword, &expressionText[..keyword.len()]));
            }
        }

        None
    }

    fn parseSemicolonAfterPropertyName(
        &mut self,
        name: &PropertyName,
        ty: &Option<TypeNode>,
        initializer: &Option<Expression>,
    ) {
        if self.token() == SyntaxKind::AtToken && !self.scanner.hasPrecedingLineBreak() {
            self.parseErrorAtCurrentToken::<u8>(Diagnostics::Decorators_must_precede_the_name_and_all_keywords_of_property_declarations,None);
            return;
        }

        if self.token() == SyntaxKind::OpenParenToken {
            self.parseErrorAtCurrentToken::<u8>(
                Diagnostics::Cannot_start_a_function_call_in_a_type_annotation,
                None,
            );
            self.nextToken();
            return;
        }

        if ty.is_some() && !self.canParseSemicolon() {
            if initializer.is_some() {
                self.parseErrorAtCurrentToken(
                    Diagnostics::_0_expected,
                    tokenToString(SyntaxKind::SemicolonToken),
                );
            } else {
                self.parseErrorAtCurrentToken::<u8>(
                    Diagnostics::Expected_for_property_initializer,
                    None,
                );
            }
            return;
        }

        if self.tryParseSemicolon() {
            return;
        }

        // If an initializer was parsed but there is still an error in finding the next semicolon,
        // we generally know there was an error already reported in the initializer...
        //   class Example { a = new Map([), ) }
        //                                ~
        if initializer.is_some() {
            // ...unless we've found the start of a block after a property declaration, in which
            // we can know that regardless of the initializer we should complain on the block.
            //   class Example { a = 0 {} }
            //                         ~
            if self.token() == SyntaxKind::OpenBraceToken {
                self.parseErrorAtCurrentToken(
                    Diagnostics::_0_expected,
                    tokenToString(SyntaxKind::SemicolonToken),
                );
            }

            return;
        }

        self.parseErrorForMissingSemicolonAfter(name.clone().into());
    }

    //     function parseExpectedJSDoc(kind: JSDocSyntaxKind) {
    //         if (token() === kind) {
    //             nextTokenJSDoc();
    //             return true;
    //         }
    //         parseErrorAtCurrentToken(Diagnostics::_0_expected, tokenToString(kind));
    //         return false;
    //     }

    fn parseOptional(&mut self, t: SyntaxKind) -> bool {
        if self.token() == t {
            self.nextToken();
            return true;
        }
        false
    }

    // function parseOptionalToken<TKind extends SyntaxKind>(t: TKind): Token<TKind>;
    // function parseOptionalToken(t: SyntaxKind): Node | undefined {
    fn parseOptionalToken<T: IsNode + IsSimpleTokenNode>(&mut self, t: SyntaxKind) -> Option<T> {
        if self.token() == t {
            return Some(self.parseTokenNode());
        }
        None
    }

    //     function parseOptionalTokenJSDoc<TKind extends JSDocSyntaxKind>(t: TKind): Token<TKind>;
    //     function parseOptionalTokenJSDoc(t: JSDocSyntaxKind): Node | undefined {
    //         if (token() === t) {
    //             return parseTokenNodeJSDoc();
    //         }
    //         return undefined;
    //     }

    fn parseExpectedToken<T, E>(
        &mut self,
        t: SyntaxKind,
        diagnosticMessage: Option<DiagnosticMessage>,
        arg0: Option<E>,
    ) -> T
    where
        T: IsNode + IsSimpleTokenNode,
        E: Display,
    {
        self.parseOptionalToken(t).unwrap_or_else(|| {
            todo!();
            // self.createMissingNode(t, /*reportAtCurrentPosition*/ false, diagnosticMessage || Diagnostics::_0_expected, arg0 || tokenToString(t))
        })
    }

    //     function parseExpectedTokenJSDoc<TKind extends JSDocSyntaxKind>(t: TKind): Token<TKind>;
    //     function parseExpectedTokenJSDoc(t: JSDocSyntaxKind): Node {
    //         return parseOptionalTokenJSDoc(t) ||
    //             createMissingNode(t, /*reportAtCurrentPosition*/ false, Diagnostics::_0_expected, tokenToString(t));
    //     }

    fn parseTokenNode<T: IsNode + IsSimpleTokenNode>(&mut self) -> T {
        let pos = self.getNodePos();
        let kind = self.token();
        self.nextToken();
        let node = self.factory.createToken(kind);
        self.finishNode(node, pos, None)
    }

    //     function parseTokenNodeJSDoc<T extends Node>(): T {
    //         const pos = getNodePos();
    //         const kind = token();
    //         nextTokenJSDoc();
    //         return finishNode(factory.createToken(kind), pos) as T;
    //     }

    fn canParseSemicolon(&self) -> bool {
        // If there's a real semicolon, then we can always parse it out.
        if self.token() == SyntaxKind::SemicolonToken {
            return true;
        }

        // We can parse out an optional semicolon in ASI cases in the following cases.
        self.token() == SyntaxKind::CloseBraceToken
            || self.token() == SyntaxKind::EndOfFileToken
            || self.scanner.hasPrecedingLineBreak()
    }

    fn tryParseSemicolon(&mut self) -> bool {
        if !self.canParseSemicolon() {
            return false;
        }

        if self.token() == SyntaxKind::SemicolonToken {
            // consume the semicolon if it was explicitly provided.
            self.nextToken();
        }

        return true;
    }

    fn parseSemicolon(&mut self) -> bool {
        self.tryParseSemicolon() || self.parseExpected(SyntaxKind::SemicolonToken, None, None)
    }

    fn createNodeArray<T: IsNode>(
        &mut self,
        elements: Vec<T>,
        pos: usize,
        end: Option<usize>,
        hasTrailingComma: bool,
    ) -> NodeArray<T> {
        let mut array = self
            .factory
            .createNodeArray(Some(elements), hasTrailingComma);
        array.setTextRangePosEnd(pos, end.unwrap_or(self.scanner.getStartPos()));
        array
    }

    fn finishNode<T: IsNode>(&mut self, node: T, pos: usize, end: Option<usize>) -> T {
        self.factory
            .node_data_mut(&node)
            .setTextRangePosEnd(pos, end.unwrap_or(self.scanner.getStartPos()));
        if !self.contextFlags.is_empty() {
            self.factory.node_data_mut(&node).flags |= self.contextFlags;
        }

        // Keep track on the node if we encountered an error while parsing it.  If we did, then
        // we cannot reuse the node incrementally.  Once we've marked this node, clear out the
        // flag so that we don't mark any subsequent nodes.
        if self.parseErrorBeforeNextFinishedNode {
            self.parseErrorBeforeNextFinishedNode = false;
            self.factory.node_data_mut(&node).flags |= NodeFlags::ThisNodeHasError;
        }

        node
    }

    //     function createMissingNode<T extends Node>(kind: T["kind"], reportAtCurrentPosition: false, diagnosticMessage?: DiagnosticMessage, arg0?: any): T;
    //     function createMissingNode<T extends Node>(kind: T["kind"], reportAtCurrentPosition: boolean, diagnosticMessage: DiagnosticMessage, arg0?: any): T;
    //     function createMissingNode<T extends Node>(kind: T["kind"], reportAtCurrentPosition: boolean, diagnosticMessage: DiagnosticMessage, arg0?: any): T {
    //         if (reportAtCurrentPosition) {
    //             parseErrorAtPosition(scanner.getStartPos(), 0, diagnosticMessage, arg0);
    //         }
    //         else if (diagnosticMessage) {
    //             parseErrorAtCurrentToken(diagnosticMessage, arg0);
    //         }

    //         const pos = getNodePos();
    //         const result =
    //             kind === SyntaxKind::Identifier ? factory.createIdentifier("", /*typeArguments*/ undefined, /*originalKeywordKind*/ undefined) :
    //             isTemplateLiteralKind(kind) ? factory.createTemplateLiteralLikeNode(kind, "", "", /*templateFlags*/ undefined) :
    //             kind === SyntaxKind::NumericLiteral ? factory.createNumericLiteral("", /*numericLiteralFlags*/ undefined) :
    //             kind === SyntaxKind::StringLiteral ? factory.createStringLiteral("", /*isSingleQuote*/ undefined) :
    //             kind === SyntaxKind::MissingDeclaration ? factory.createMissingDeclaration() :
    //             factory.createToken(kind);
    //         return finishNode(result, pos) as T;
    //     }

    //     function internIdentifier(text: string): string {
    //         let identifier = identifiers.get(text);
    //         if (identifier === undefined) {
    //             identifiers.set(text, identifier = text);
    //         }
    //         return identifier;
    //     }

    // An identifier that starts with two underscores has an extra underscore character prepended to it to avoid issues
    // with magic property names like '__proto__'. The 'identifiers' object is used to share a single string instance for
    // each identifier in order to reduce memory consumption.
    fn createIdentifier(
        &mut self,
        isIdentifier: bool,
        diagnosticMessage: Option<DiagnosticMessage>,
        privateIdentifierDiagnosticMessage: Option<DiagnosticMessage>,
    ) -> Identifier {
        if isIdentifier {
            // identifierCount++;
            let pos = self.getNodePos();
            // Store original token kind if it is not just an Identifier so we can report appropriate error later in type checker
            let originalKeywordKind = self.token();
            let text = JsWord::from(self.scanner.getTokenValue());
            self.nextTokenWithoutCheck();
            let node = self
                .factory
                .createIdentifier(text, None, Some(originalKeywordKind));
            return self.finishNode(node, pos, None);
        }

        if self.token() == SyntaxKind::PrivateIdentifier {
            self.parseErrorAtCurrentToken::<u8>(
                privateIdentifierDiagnosticMessage.unwrap_or(
                    Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                ),
                None,
            );
            return self.createIdentifier(true, None, None);
        }

        if self.token() == SyntaxKind::Unknown
            && self
                .scanner
                .tryScan(|s| s.reScanInvalidIdentifier() == SyntaxKind::Identifier)
        {
            self.emit_scanner_errors();
            // Scanner has already recorded an 'Invalid character' error, so no need to add another from the parser.
            return self.createIdentifier(true, None, None);
        }
        todo!();

        // identifierCount++;
        // // Only for end of file because the error gets reported incorrectly on embedded script tags.
        // const reportAtCurrentPosition = token() === SyntaxKind::EndOfFileToken;

        // const isReservedWord = scanner.isReservedWord();
        // const msgArg = scanner.getTokenText();

        // const defaultMessage = isReservedWord ?
        //     Diagnostics::Identifier_expected_0_is_a_reserved_word_that_cannot_be_used_here :
        //     Diagnostics::Identifier_expected;

        // return createMissingNode<Identifier>(SyntaxKind::Identifier, reportAtCurrentPosition, diagnosticMessage || defaultMessage, msgArg);
    }

    fn parseBindingIdentifier(
        &mut self,
        privateIdentifierDiagnosticMessage: Option<DiagnosticMessage>,
    ) -> Identifier {
        self.createIdentifier(
            self.isBindingIdentifier(),
            None,
            privateIdentifierDiagnosticMessage,
        )
    }

    fn parseIdentifier(
        &mut self,
        diagnosticMessage: Option<DiagnosticMessage>,
        privateIdentifierDiagnosticMessage: Option<DiagnosticMessage>,
    ) -> Identifier {
        self.createIdentifier(
            self.isIdentifier(),
            diagnosticMessage,
            privateIdentifierDiagnosticMessage,
        )
    }

    fn parseIdentifierName(&mut self, diagnosticMessage: Option<DiagnosticMessage>) -> Identifier {
        self.createIdentifier(
            tokenIsIdentifierOrKeyword(self.token()),
            diagnosticMessage,
            None,
        )
    }

    fn isLiteralPropertyName(&self) -> bool {
        tokenIsIdentifierOrKeyword(self.token())
            || self.token() == SyntaxKind::StringLiteral
            || self.token() == SyntaxKind::NumericLiteral
    }

    fn isAssertionKey(&self) -> bool {
        tokenIsIdentifierOrKeyword(self.token()) || self.token() == SyntaxKind::StringLiteral
    }

    fn parsePropertyNameWorker(&mut self, allowComputedPropertyNames: bool) -> PropertyName {
        if self.token() == SyntaxKind::StringLiteral {
            return PropertyName::StringLiteral(Rc::new(self.parseStringLiteral()));
        }
        if self.token() == SyntaxKind::NumericLiteral {
            return PropertyName::NumericLiteral(Rc::new(self.parseNumericLiteral()));
        }
        if allowComputedPropertyNames && self.token() == SyntaxKind::OpenBracketToken {
            return PropertyName::ComputedPropertyName(Rc::new(self.parseComputedPropertyName()));
        }
        if self.token() == SyntaxKind::PrivateIdentifier {
            return PropertyName::PrivateIdentifier(Rc::new(self.parsePrivateIdentifier()));
        }
        PropertyName::Identifier(Rc::new(self.parseIdentifierName(None)))
    }

    fn parsePropertyName(&mut self) -> PropertyName {
        self.parsePropertyNameWorker(true)
    }

    fn parseComputedPropertyName(&mut self) -> ComputedPropertyName {
        // PropertyName [Yield]:
        //      LiteralPropertyName
        //      ComputedPropertyName[?Yield]
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::OpenBracketToken, None, None);
        // We parse any expression (including a comma expression). But the grammar
        // says that only an assignment expression is allowed, so the grammar checker
        // will error if it sees a comma expression.
        let expression = self.allowInAnd(|p| p.parseExpression());
        self.parseExpected(SyntaxKind::CloseBracketToken, None, None);
        let node = self.factory.createComputedPropertyName(expression);
        self.finishNode(node, pos, None)
    }

    //     function internPrivateIdentifier(text: string): string {
    //         let privateIdentifier = privateIdentifiers.get(text);
    //         if (privateIdentifier === undefined) {
    //             privateIdentifiers.set(text, privateIdentifier = text);
    //         }
    //         return privateIdentifier;
    //     }

    fn parsePrivateIdentifier(&mut self) -> PrivateIdentifier {
        let pos = self.getNodePos();
        let node = self
            .factory
            .createPrivateIdentifier(JsWord::from(self.scanner.getTokenText()));
        self.nextToken();
        self.finishNode(node, pos, None)
    }

    fn parseContextualModifier(&mut self, t: SyntaxKind) -> bool {
        self.token() == t && self.tryParse(|p| p.nextTokenCanFollowModifier())
    }

    fn nextTokenIsOnSameLineAndCanFollowModifier(&mut self) -> bool {
        self.nextToken();
        if self.scanner.hasPrecedingLineBreak() {
            return false;
        }
        self.canFollowModifier()
    }

    fn nextTokenCanFollowModifier(&mut self) -> bool {
        match self.token() {
            SyntaxKind::ConstKeyword => {
                // 'const' is only a modifier if followed by 'enum'.
                self.nextToken() == SyntaxKind::EnumKeyword
            }
            SyntaxKind::ExportKeyword => {
                self.nextToken();
                if self.token() == SyntaxKind::DefaultKeyword {
                    return self.lookAhead(|p| p.nextTokenCanFollowDefaultKeyword());
                }
                if self.token() == SyntaxKind::TypeKeyword {
                    return self.lookAhead(|p| p.nextTokenCanFollowExportModifier());
                }
                self.canFollowExportModifier()
            }
            SyntaxKind::DefaultKeyword => self.nextTokenCanFollowDefaultKeyword(),
            SyntaxKind::StaticKeyword | SyntaxKind::GetKeyword | SyntaxKind::SetKeyword => {
                self.nextToken();
                self.canFollowModifier()
            }
            _ => self.nextTokenIsOnSameLineAndCanFollowModifier(),
        }
    }

    fn canFollowExportModifier(&self) -> bool {
        self.token() != SyntaxKind::AsteriskToken
            && self.token() != SyntaxKind::AsKeyword
            && self.token() != SyntaxKind::OpenBraceToken
            && self.canFollowModifier()
    }

    fn nextTokenCanFollowExportModifier(&mut self) -> bool {
        self.nextToken();
        self.canFollowExportModifier()
    }

    fn parseAnyContextualModifier(&mut self) -> bool {
        isModifierKind(self.token()) && self.tryParse(|p| p.nextTokenCanFollowModifier())
    }

    fn canFollowModifier(&self) -> bool {
        self.token() == SyntaxKind::OpenBracketToken
            || self.token() == SyntaxKind::OpenBraceToken
            || self.token() == SyntaxKind::AsteriskToken
            || self.token() == SyntaxKind::DotDotDotToken
            || self.isLiteralPropertyName()
    }

    fn nextTokenCanFollowDefaultKeyword(&mut self) -> bool {
        self.nextToken();
        self.token() == SyntaxKind::ClassKeyword
            || self.token() == SyntaxKind::FunctionKeyword
            || self.token() == SyntaxKind::InterfaceKeyword
            || (self.token() == SyntaxKind::AbstractKeyword
                && self.lookAhead(|p| p.nextTokenIsClassKeywordOnSameLine()))
            || (self.token() == SyntaxKind::AsyncKeyword
                && self.lookAhead(|p| p.nextTokenIsFunctionKeywordOnSameLine()))
    }

    // True if positioned at the start of a list element
    fn isListElement(&mut self, parsingContext: ParsingContext, inErrorRecovery: bool) -> bool {
        let node = self.currentNode(parsingContext);
        if node.is_some() {
            return true;
        }

        match parsingContext {
            ParsingContext::SourceElements
            | ParsingContext::BlockStatements
            | ParsingContext::SwitchClauseStatements => {
                // If we're in error recovery, then we don't want to treat ';' as an empty statement.
                // The problem is that ';' can show up in far too many contexts, and if we see one
                // and assume it's a statement, then we may bail out inappropriately from whatever
                // we're parsing.  For example, if we have a semicolon in the middle of a class, then
                // we really don't want to assume the class is over and we're on a statement in the
                // outer module.  We just want to consume and move on.
                !(self.token() == SyntaxKind::SemicolonToken && inErrorRecovery)
                    && self.isStartOfStatement()
            }
            ParsingContext::SwitchClauses => {
                self.token() == SyntaxKind::CaseKeyword
                    || self.token() == SyntaxKind::DefaultKeyword
            }
            ParsingContext::TypeMembers => self.lookAhead(|p| p.isTypeMemberStart()),
            ParsingContext::ClassMembers => {
                // We allow semicolons as class elements (as specified by ES6) as long as we're
                // not in error recovery.  If we're in error recovery, we don't want an errant
                // semicolon to be treated as a class member (since they're almost always used
                // for statements.
                self.lookAhead(|p| p.isClassMemberStart())
                    || (self.token() == SyntaxKind::SemicolonToken && !inErrorRecovery)
            }
            ParsingContext::EnumMembers => {
                // Include open bracket computed properties. This technically also lets in indexers,
                // which would be a candidate for improved error reporting.
                self.token() == SyntaxKind::OpenBracketToken || self.isLiteralPropertyName()
            }
            ParsingContext::ObjectLiteralMembers => {
                match self.token() {
                    SyntaxKind::OpenBracketToken
                    | SyntaxKind::AsteriskToken
                    | SyntaxKind::DotDotDotToken
                    | SyntaxKind::DotToken => {
                        // Not an object literal member, but don't want to close the object (see `tests/cases/fourslash/completionsDotInObjectLiteral.ts`)
                        true
                    }
                    _ => self.isLiteralPropertyName(),
                }
            }
            ParsingContext::RestProperties => self.isLiteralPropertyName(),
            ParsingContext::ObjectBindingElements => {
                self.token() == SyntaxKind::OpenBracketToken
                    || self.token() == SyntaxKind::DotDotDotToken
                    || self.isLiteralPropertyName()
            }
            ParsingContext::AssertEntries => self.isAssertionKey(),
            ParsingContext::HeritageClauseElement => {
                // If we see `{ ... }` then only consume it as an expression if it is followed by `,` or `{`
                // That way we won't consume the body of a class in its heritage clause.
                if self.token() == SyntaxKind::OpenBraceToken {
                    return self.lookAhead(|p| p.isValidHeritageClauseObjectLiteral());
                }

                if !inErrorRecovery {
                    self.isStartOfLeftHandSideExpression()
                        && !self.isHeritageClauseExtendsOrImplementsKeyword()
                } else {
                    // If we're in error recovery we tighten up what we're willing to match.
                    // That way we don't treat something like "this" as a valid heritage clause
                    // element during recovery.
                    self.isIdentifier() && !self.isHeritageClauseExtendsOrImplementsKeyword()
                }
            }
            ParsingContext::VariableDeclarations => {
                self.isBindingIdentifierOrPrivateIdentifierOrPattern()
            }
            ParsingContext::ArrayBindingElements => {
                self.token() == SyntaxKind::CommaToken
                    || self.token() == SyntaxKind::DotDotDotToken
                    || self.isBindingIdentifierOrPrivateIdentifierOrPattern()
            }
            ParsingContext::TypeParameters => self.isIdentifier(),
            ParsingContext::ArrayLiteralMembers => {
                match self.token() {
                    SyntaxKind::CommaToken | SyntaxKind::DotToken => {
                        // Not an array literal member, but don't want to close the array (see `tests/cases/fourslash/completionsDotInArrayLiteralInObjectLiteral.ts`)
                        true
                    }
                    _ => self.token() == SyntaxKind::DotDotDotToken || self.isStartOfExpression(),
                }
            }
            ParsingContext::ArgumentExpressions => {
                self.token() == SyntaxKind::DotDotDotToken || self.isStartOfExpression()
            }
            ParsingContext::Parameters => self.isStartOfParameter(false),
            ParsingContext::JSDocParameters => self.isStartOfParameter(true),
            ParsingContext::TypeArguments | ParsingContext::TupleElementTypes => {
                self.token() == SyntaxKind::CommaToken || self.isStartOfType(false)
            }
            ParsingContext::HeritageClauses => self.isHeritageClause(),
            ParsingContext::ImportOrExportSpecifiers => tokenIsIdentifierOrKeyword(self.token()),
            ParsingContext::JsxAttributes => {
                tokenIsIdentifierOrKeyword(self.token())
                    || self.token() == SyntaxKind::OpenBraceToken
            }
            ParsingContext::JsxChildren => true,
            _ => unreachable!(),
        }
    }

    fn isValidHeritageClauseObjectLiteral(&mut self) -> bool {
        debug_assert!(self.token() == SyntaxKind::OpenBraceToken);
        if self.nextToken() == SyntaxKind::CloseBraceToken {
            // if we see "extends {}" then only treat the {} as what we're extending (and not
            // the class body) if we have:
            //
            //      extends {} {
            //      extends {},
            //      extends {} extends
            //      extends {} implements

            let next = self.nextToken();
            return next == SyntaxKind::CommaToken
                || next == SyntaxKind::OpenBraceToken
                || next == SyntaxKind::ExtendsKeyword
                || next == SyntaxKind::ImplementsKeyword;
        }

        true
    }

    fn nextTokenIsIdentifier(&mut self) -> bool {
        self.nextToken();
        self.isIdentifier()
    }

    fn nextTokenIsIdentifierOrKeyword(&mut self) -> bool {
        self.nextToken();
        tokenIsIdentifierOrKeyword(self.token())
    }

    fn nextTokenIsIdentifierOrKeywordOrGreaterThan(&mut self) -> bool {
        self.nextToken();
        tokenIsIdentifierOrKeywordOrGreaterThan(self.token())
    }

    fn isHeritageClauseExtendsOrImplementsKeyword(&mut self) -> bool {
        if self.token() == SyntaxKind::ImplementsKeyword
            || self.token() == SyntaxKind::ExtendsKeyword
        {
            return self.lookAhead(|p| p.nextTokenIsStartOfExpression());
        }

        false
    }

    fn nextTokenIsStartOfExpression(&mut self) -> bool {
        self.nextToken();
        self.isStartOfExpression()
    }

    fn nextTokenIsStartOfType(&mut self) -> bool {
        self.nextToken();
        self.isStartOfType(false)
    }

    // True if positioned at a list terminator
    fn isListTerminator(&mut self, kind: ParsingContext) -> bool {
        if self.token() == SyntaxKind::EndOfFileToken {
            // Being at the end of the file ends all lists.
            return true;
        }

        match kind {
            ParsingContext::BlockStatements
            | ParsingContext::SwitchClauses
            | ParsingContext::TypeMembers
            | ParsingContext::ClassMembers
            | ParsingContext::EnumMembers
            | ParsingContext::ObjectLiteralMembers
            | ParsingContext::ObjectBindingElements
            | ParsingContext::ImportOrExportSpecifiers
            | ParsingContext::AssertEntries => self.token() == SyntaxKind::CloseBraceToken,
            ParsingContext::SwitchClauseStatements => {
                self.token() == SyntaxKind::CloseBraceToken
                    || self.token() == SyntaxKind::CaseKeyword
                    || self.token() == SyntaxKind::DefaultKeyword
            }
            ParsingContext::HeritageClauseElement => {
                self.token() == SyntaxKind::OpenBraceToken
                    || self.token() == SyntaxKind::ExtendsKeyword
                    || self.token() == SyntaxKind::ImplementsKeyword
            }
            ParsingContext::VariableDeclarations => self.isVariableDeclaratorListTerminator(),
            ParsingContext::TypeParameters => {
                // Tokens other than '>' are here for better error recovery
                self.token() == SyntaxKind::GreaterThanToken
                    || self.token() == SyntaxKind::OpenParenToken
                    || self.token() == SyntaxKind::OpenBraceToken
                    || self.token() == SyntaxKind::ExtendsKeyword
                    || self.token() == SyntaxKind::ImplementsKeyword
            }
            ParsingContext::ArgumentExpressions => {
                // Tokens other than ')' are here for better error recovery
                self.token() == SyntaxKind::CloseParenToken
                    || self.token() == SyntaxKind::SemicolonToken
            }
            ParsingContext::ArrayLiteralMembers
            | ParsingContext::TupleElementTypes
            | ParsingContext::ArrayBindingElements => self.token() == SyntaxKind::CloseBracketToken,
            ParsingContext::JSDocParameters
            | ParsingContext::Parameters
            | ParsingContext::RestProperties => {
                // Tokens other than ')' and ']' (the latter for index signatures) are here for better error recovery
                self.token() == SyntaxKind::CloseParenToken
                    || self.token() == SyntaxKind::CloseBracketToken /*|| token == SyntaxKind::OpenBraceToken*/
            }
            ParsingContext::TypeArguments => {
                // All other tokens should cause the type-argument to terminate except comma token
                self.token() != SyntaxKind::CommaToken
            }
            ParsingContext::HeritageClauses => {
                self.token() == SyntaxKind::OpenBraceToken
                    || self.token() == SyntaxKind::CloseBraceToken
            }
            ParsingContext::JsxAttributes => {
                self.token() == SyntaxKind::GreaterThanToken
                    || self.token() == SyntaxKind::SlashToken
            }
            ParsingContext::JsxChildren => {
                self.token() == SyntaxKind::LessThanToken
                    && self.lookAhead(|p| p.nextTokenIsSlash())
            }
            _ => false,
        }
    }

    fn isVariableDeclaratorListTerminator(&self) -> bool {
        // If we can consume a semicolon (either explicitly, or with ASI), then consider us done
        // with parsing the list of variable declarators.
        if self.canParseSemicolon() {
            return true;
        }

        // in the where we're parsing the variable declarator of a 'for-in' statement, we
        // are done if we see an 'in' keyword in front of us. Same with for-of
        if self.isInOrOfKeyword(self.token()) {
            return true;
        }

        // ERROR RECOVERY TWEAK:
        // For better error recovery, if we see an '=>' then we just stop immediately.  We've got an
        // arrow function here and it's going to be very unlikely that we'll resynchronize and get
        // another variable declaration.
        if self.token() == SyntaxKind::EqualsGreaterThanToken {
            return true;
        }

        // Keep trying to parse out variable declarators.
        false
    }

    // True if positioned at element or terminator of the current list or any enclosing list
    fn isInSomeParsingContext(&mut self) -> bool {
        // TODO: bit of a hack for length
        for kind in 0..ParsingContext::all().bits.count_ones() {
            let kind = ParsingContext::from_bits_truncate(1 << kind);
            if self.parsingContext.intersects(kind) {
                if self.isListElement(kind, true) || self.isListTerminator(kind) {
                    return true;
                }
            }
        }

        return false;
    }

    // Parses a list of elements
    fn parseList<T: IsNode, F>(&mut self, kind: ParsingContext, mut parseElement: F) -> NodeArray<T>
    where
        F: FnMut(&mut Parser) -> T,
    {
        let saveParsingContext = self.parsingContext;
        self.parsingContext |= kind;
        let mut list = Vec::new();
        let listPos = self.getNodePos();

        while !self.isListTerminator(kind) {
            if self.isListElement(kind, false) {
                list.push(self.parseListElement(kind, &mut parseElement));

                continue;
            }

            if self.abortParsingListOrMoveToNextToken(kind) {
                break;
            }
        }

        self.parsingContext = saveParsingContext;
        self.createNodeArray(list, listPos, None, false)
    }

    fn parseListElement<T, F>(&mut self, parsingContext: ParsingContext, parseElement: &mut F) -> T
    where
        F: FnMut(&mut Parser) -> T,
    {
        let node = self.currentNode(parsingContext);
        if let Some(node) = node {
            todo!();
            // return consumeNode(node) as T;
        }

        parseElement(self)
    }

    fn currentNode(&self, parsingContext: ParsingContext) -> Option<()> {
        // If we don't have a cursor or the parsing context isn't reusable, there's nothing to reuse.
        //
        // If there is an outstanding parse error that we've encountered, but not attached to
        // some node, then we cannot get a node from the old source tree.  This is because we
        // want to mark the next node we encounter as being unusable.
        //
        // Note: This may be too conservative.  Perhaps we could reuse the node and set the bit
        // on it (or its leftmost child) as having the error.  For now though, being conservative
        // is nice and likely won't ever affect perf.
        if self.syntaxCursor.is_none()
            || !self.isReusableParsingContext(parsingContext)
            || self.parseErrorBeforeNextFinishedNode
        {
            return None;
        }

        todo!();

        // const node = syntaxCursor.currentNode(scanner.getStartPos());

        // // Can't reuse a missing node.
        // // Can't reuse a node that intersected the change range.
        // // Can't reuse a node that contains a parse error.  This is necessary so that we
        // // produce the same set of errors again.
        // if (nodeIsMissing(node) || node.intersectsChange || containsParseError(node)) {
        //     return undefined;
        // }

        // // We can only reuse a node if it was parsed under the same strict mode that we're
        // // currently in.  i.e. if we originally parsed a node in non-strict mode, but then
        // // the user added 'using strict' at the top of the file, then we can't use that node
        // // again as the presence of strict mode may cause us to parse the tokens in the file
        // // differently.
        // //
        // // Note: we *can* reuse tokens when the strict mode changes.  That's because tokens
        // // are unaffected by strict mode.  It's just the parser will decide what to do with it
        // // differently depending on what mode it is in.
        // //
        // // This also applies to all our other context flags as well.
        // const nodeContextFlags = node.flags & NodeFlags.ContextFlags;
        // if (nodeContextFlags !== contextFlags) {
        //     return undefined;
        // }

        // // Ok, we have a node that looks like it could be reused.  Now verify that it is valid
        // // in the current list parsing context that we're currently at.
        // if (!canReuseNode(node, parsingContext)) {
        //     return undefined;
        // }

        // if ((node as JSDocContainer).jsDocCache) {
        //     // jsDocCache may include tags from parent nodes, which might have been modified.
        //     (node as JSDocContainer).jsDocCache = undefined;
        // }

        // return node;
    }

    //     function consumeNode(node: Node) {
    //         // Move the scanner so it is after the node we just consumed.
    //         scanner.setTextPos(node.end);
    //         nextToken();
    //         return node;
    //     }

    fn isReusableParsingContext(&self, parsingContext: ParsingContext) -> bool {
        match parsingContext {
            ParsingContext::ClassMembers
            | ParsingContext::SwitchClauses
            | ParsingContext::SourceElements
            | ParsingContext::BlockStatements
            | ParsingContext::SwitchClauseStatements
            | ParsingContext::EnumMembers
            | ParsingContext::TypeMembers
            | ParsingContext::VariableDeclarations
            | ParsingContext::JSDocParameters
            | ParsingContext::Parameters => true,
            _ => false,
        }
    }

    //     function canReuseNode(node: Node, parsingContext: ParsingContext): boolean {
    //         switch (parsingContext) {
    //             ParsingContext::ClassMembers:
    //                 return isReusableClassMember(node);

    //             ParsingContext::SwitchClauses:
    //                 return isReusableSwitchClause(node);

    //             ParsingContext::SourceElements:
    //             ParsingContext::BlockStatements:
    //             ParsingContext::SwitchClauseStatements:
    //                 return isReusableStatement(node);

    //             ParsingContext::EnumMembers:
    //                 return isReusableEnumMember(node);

    //             ParsingContext::TypeMembers:
    //                 return isReusableTypeMember(node);

    //             ParsingContext::VariableDeclarations:
    //                 return isReusableVariableDeclaration(node);

    //             ParsingContext::JSDocParameters:
    //             ParsingContext::Parameters:
    //                 return isReusableParameter(node);

    //             // Any other lists we do not care about reusing nodes in.  But feel free to add if
    //             // you can do so safely.  Danger areas involve nodes that may involve speculative
    //             // parsing.  If speculative parsing is involved with the node, then the range the
    //             // parser reached while looking ahead might be in the edited range (see the example
    //             // in canReuseVariableDeclaratorNode for a good of this).

    //             // ParsingContext::HeritageClauses:
    //             // This would probably be safe to reuse.  There is no speculative parsing with
    //             // heritage clauses.

    //             // ParsingContext::TypeParameters:
    //             // This would probably be safe to reuse.  There is no speculative parsing with
    //             // type parameters.  Note that that's because type *parameters* only occur in
    //             // unambiguous *type* contexts.  While type *arguments* occur in very ambiguous
    //             // *expression* contexts.

    //             // ParsingContext::TupleElementTypes:
    //             // This would probably be safe to reuse.  There is no speculative parsing with
    //             // tuple types.

    //             // Technically, type argument list types are probably safe to reuse.  While
    //             // speculative parsing is involved with them (since type argument lists are only
    //             // produced from speculative parsing a < as a type argument list), we only have
    //             // the types because speculative parsing succeeded.  Thus, the lookahead never
    //             // went past the end of the list and rewound.
    //             // ParsingContext::TypeArguments:

    //             // Note: these are almost certainly not safe to ever reuse.  Expressions commonly
    //             // need a large amount of lookahead, and we should not reuse them as they may
    //             // have actually intersected the edit.
    //             // ParsingContext::ArgumentExpressions:

    //             // This is not safe to reuse for the same reason as the 'AssignmentExpression'
    //             // cases.  i.e. a property assignment may end with an expression, and thus might
    //             // have lookahead far beyond it's old node.
    //             // ParsingContext::ObjectLiteralMembers:

    //             // This is probably not safe to reuse.  There can be speculative parsing with
    //             // type names in a heritage clause.  There can be generic names in the type
    //             // name list, and there can be left hand side expressions (which can have type
    //             // arguments.)
    //             // ParsingContext::HeritageClauseElement:

    //             // Perhaps safe to reuse, but it's unlikely we'd see more than a dozen attributes
    //             // on any given element. Same for children.
    //             // ParsingContext::JsxAttributes:
    //             // ParsingContext::JsxChildren:

    //         }

    //         return false;
    //     }

    //     function isReusableClassMember(node: Node) {
    //         if (node) {
    //             switch (node.kind) {
    //                 SyntaxKind::Constructor:
    //                 SyntaxKind::IndexSignature:
    //                 SyntaxKind::GetAccessor:
    //                 SyntaxKind::SetAccessor:
    //                 SyntaxKind::PropertyDeclaration:
    //                 SyntaxKind::SemicolonClassElement:
    //                     return true;
    //                 SyntaxKind::MethodDeclaration:
    //                     // Method declarations are not necessarily reusable.  An object-literal
    //                     // may have a method calls "constructor(...)" and we must reparse that
    //                     // into an actual .ConstructorDeclaration.
    //                     const methodDeclaration = node as MethodDeclaration;
    //                     const nameIsConstructor = methodDeclaration.name.kind === SyntaxKind::Identifier &&
    //                         methodDeclaration.name.originalKeywordKind === SyntaxKind::ConstructorKeyword;

    //                     return !nameIsConstructor;
    //             }
    //         }

    //         return false;
    //     }

    //     function isReusableSwitchClause(node: Node) {
    //         if (node) {
    //             switch (node.kind) {
    //                 SyntaxKind::CaseClause:
    //                 SyntaxKind::DefaultClause:
    //                     return true;
    //             }
    //         }

    //         return false;
    //     }

    //     function isReusableStatement(node: Node) {
    //         if (node) {
    //             switch (node.kind) {
    //                 SyntaxKind::FunctionDeclaration:
    //                 SyntaxKind::VariableStatement:
    //                 SyntaxKind::Block:
    //                 SyntaxKind::IfStatement:
    //                 SyntaxKind::ExpressionStatement:
    //                 SyntaxKind::ThrowStatement:
    //                 SyntaxKind::ReturnStatement:
    //                 SyntaxKind::SwitchStatement:
    //                 SyntaxKind::BreakStatement:
    //                 SyntaxKind::ContinueStatement:
    //                 SyntaxKind::ForInStatement:
    //                 SyntaxKind::ForOfStatement:
    //                 SyntaxKind::ForStatement:
    //                 SyntaxKind::WhileStatement:
    //                 SyntaxKind::WithStatement:
    //                 SyntaxKind::EmptyStatement:
    //                 SyntaxKind::TryStatement:
    //                 SyntaxKind::LabeledStatement:
    //                 SyntaxKind::DoStatement:
    //                 SyntaxKind::DebuggerStatement:
    //                 SyntaxKind::ImportDeclaration:
    //                 SyntaxKind::ImportEqualsDeclaration:
    //                 SyntaxKind::ExportDeclaration:
    //                 SyntaxKind::ExportAssignment:
    //                 SyntaxKind::ModuleDeclaration:
    //                 SyntaxKind::ClassDeclaration:
    //                 SyntaxKind::InterfaceDeclaration:
    //                 SyntaxKind::EnumDeclaration:
    //                 SyntaxKind::TypeAliasDeclaration:
    //                     return true;
    //             }
    //         }

    //         return false;
    //     }

    //     function isReusableEnumMember(node: Node) {
    //         return node.kind === SyntaxKind::EnumMember;
    //     }

    //     function isReusableTypeMember(node: Node) {
    //         if (node) {
    //             switch (node.kind) {
    //                 SyntaxKind::ConstructSignature:
    //                 SyntaxKind::MethodSignature:
    //                 SyntaxKind::IndexSignature:
    //                 SyntaxKind::PropertySignature:
    //                 SyntaxKind::CallSignature:
    //                     return true;
    //             }
    //         }

    //         return false;
    //     }

    //     function isReusableVariableDeclaration(node: Node) {
    //         if (node.kind !== SyntaxKind::VariableDeclaration) {
    //             return false;
    //         }

    //         // Very subtle incremental parsing bug.  Consider the following code:
    //         //
    //         //      let v = new List < A, B
    //         //
    //         // This is actually legal code.  It's a list of variable declarators "v = new List<A"
    //         // on one side and "B" on the other. If you then change that to:
    //         //
    //         //      let v = new List < A, B >()
    //         //
    //         // then we have a problem.  "v = new List<A" doesn't intersect the change range, so we
    //         // start reparsing at "B" and we completely fail to handle this properly.
    //         //
    //         // In order to prevent this, we do not allow a variable declarator to be reused if it
    //         // has an initializer.
    //         const variableDeclarator = node as VariableDeclaration;
    //         return variableDeclarator.initializer === undefined;
    //     }

    //     function isReusableParameter(node: Node) {
    //         if (node.kind !== SyntaxKind::Parameter) {
    //             return false;
    //         }

    //         // See the comment in isReusableVariableDeclaration for why we do this.
    //         const parameter = node as ParameterDeclaration;
    //         return parameter.initializer === undefined;
    //     }

    // Returns true if we should abort parsing.
    fn abortParsingListOrMoveToNextToken(&mut self, kind: ParsingContext) -> bool {
        self.parsingContextErrors(kind);
        if self.isInSomeParsingContext() {
            return true;
        }

        self.nextToken();
        false
    }

    fn parsingContextErrors(&mut self, context: ParsingContext) {
        match context {
            ParsingContext::SourceElements => {
                return if self.token() == SyntaxKind::DefaultKeyword {
                    self.parseErrorAtCurrentToken(
                        Diagnostics::_0_expected,
                        tokenToString(SyntaxKind::ExportKeyword),
                    )
                } else {
                    self.parseErrorAtCurrentToken::<u8>(
                        Diagnostics::Declaration_or_statement_expected,
                        None,
                    )
                };
            }
            ParsingContext::BlockStatements => {
                return self.parseErrorAtCurrentToken::<u8>(
                    Diagnostics::Declaration_or_statement_expected,
                    None,
                );
            }
            ParsingContext::SwitchClauses => {
                return self
                    .parseErrorAtCurrentToken::<u8>(Diagnostics::case_or_default_expected, None);
            }
            ParsingContext::SwitchClauseStatements => {
                return self.parseErrorAtCurrentToken::<u8>(Diagnostics::Statement_expected, None);
            }
            ParsingContext::RestProperties | ParsingContext::TypeMembers => {
                return self.parseErrorAtCurrentToken::<u8>(
                    Diagnostics::Property_or_signature_expected,
                    None,
                );
            }
            ParsingContext::ClassMembers => {
                return self.parseErrorAtCurrentToken::<u8>(
                    Diagnostics::Unexpected_token_A_constructor_method_accessor_or_property_was_expected,
                    None,
                );
            }
            ParsingContext::EnumMembers => {
                return self
                    .parseErrorAtCurrentToken::<u8>(Diagnostics::Enum_member_expected, None);
            }
            ParsingContext::HeritageClauseElement => {
                return self.parseErrorAtCurrentToken::<u8>(Diagnostics::Expression_expected, None);
            }
            ParsingContext::VariableDeclarations => {
                return if isKeyword(self.token()) {
                    self.parseErrorAtCurrentToken(
                        Diagnostics::_0_is_not_allowed_as_a_variable_declaration_name,
                        tokenToString(self.token()),
                    )
                } else {
                    self.parseErrorAtCurrentToken::<u8>(
                        Diagnostics::Variable_declaration_expected,
                        None,
                    )
                };
            }
            ParsingContext::ObjectBindingElements => {
                return self.parseErrorAtCurrentToken::<u8>(
                    Diagnostics::Property_destructuring_pattern_expected,
                    None,
                );
            }
            ParsingContext::ArrayBindingElements => {
                return self.parseErrorAtCurrentToken::<u8>(
                    Diagnostics::Array_element_destructuring_pattern_expected,
                    None,
                );
            }
            ParsingContext::ArgumentExpressions => {
                return self.parseErrorAtCurrentToken::<u8>(
                    Diagnostics::Argument_expression_expected,
                    None,
                );
            }
            ParsingContext::ObjectLiteralMembers => {
                return self.parseErrorAtCurrentToken::<u8>(
                    Diagnostics::Property_assignment_expected,
                    None,
                );
            }
            ParsingContext::ArrayLiteralMembers => {
                return self.parseErrorAtCurrentToken::<u8>(
                    Diagnostics::Expression_or_comma_expected,
                    None,
                );
            }
            ParsingContext::JSDocParameters => {
                return self.parseErrorAtCurrentToken::<u8>(
                    Diagnostics::Parameter_declaration_expected,
                    None,
                );
            }
            ParsingContext::Parameters => {
                return self.parseErrorAtCurrentToken::<u8>(
                    Diagnostics::Parameter_declaration_expected,
                    None,
                );
            }
            ParsingContext::TypeParameters => {
                return self.parseErrorAtCurrentToken::<u8>(
                    Diagnostics::Type_parameter_declaration_expected,
                    None,
                );
            }
            ParsingContext::TypeArguments => {
                return self
                    .parseErrorAtCurrentToken::<u8>(Diagnostics::Type_argument_expected, None);
            }
            ParsingContext::TupleElementTypes => {
                return self.parseErrorAtCurrentToken::<u8>(Diagnostics::Type_expected, None);
            }
            ParsingContext::HeritageClauses => {
                return self
                    .parseErrorAtCurrentToken::<u8>(Diagnostics::Unexpected_token_expected, None);
            }
            ParsingContext::ImportOrExportSpecifiers => {
                return self.parseErrorAtCurrentToken::<u8>(Diagnostics::Identifier_expected, None);
            }
            ParsingContext::JsxAttributes => {
                return self.parseErrorAtCurrentToken::<u8>(Diagnostics::Identifier_expected, None);
            }
            ParsingContext::JsxChildren => {
                return self.parseErrorAtCurrentToken::<u8>(Diagnostics::Identifier_expected, None);
            }
            _ => unreachable!(),
        }
    }

    // Parses a comma-delimited list of elements
    fn parseDelimitedList<T: IsNode, F>(
        &mut self,
        kind: ParsingContext,
        mut parseElement: F,
        considerSemicolonAsDelimiter: bool,
    ) -> NodeArray<T>
    where
        F: FnMut(&mut Parser) -> T,
    {
        let saveParsingContext = self.parsingContext;
        self.parsingContext |= kind;
        let mut list = Vec::new();
        let listPos = self.getNodePos();

        let mut commaStart = None; // Meaning the previous token was not a comma
        loop {
            if self.isListElement(kind, false) {
                let startPos = self.scanner.getStartPos();
                list.push(self.parseListElement(kind, &mut parseElement));
                commaStart = Some(self.scanner.getTokenPos());

                if self.parseOptional(SyntaxKind::CommaToken) {
                    // No need to check for a zero length node since we know we parsed a comma
                    continue;
                }

                commaStart = None; // Back to the state where the last token was not a comma
                if self.isListTerminator(kind) {
                    break;
                }

                // We didn't get a comma, and the list wasn't terminated, explicitly parse
                // out a comma so we give a good error message.
                self.parseExpected(
                    SyntaxKind::CommaToken,
                    self.getExpectedCommaDiagnostic(kind),
                    None,
                );

                // If the token was a semicolon, and the caller allows that, then skip it and
                // continue.  This ensures we get back on track and don't result in tons of
                // parse errors.  For example, this can happen when people do things like use
                // a semicolon to delimit object literal members.   Note: we'll have already
                // reported an error when we called parseExpected above.
                if considerSemicolonAsDelimiter
                    && self.token() == SyntaxKind::SemicolonToken
                    && !self.scanner.hasPrecedingLineBreak()
                {
                    self.nextToken();
                }
                if startPos == self.scanner.getStartPos() {
                    // What we're parsing isn't actually remotely recognizable as a element and we've consumed no tokens whatsoever
                    // Consume a token to advance the parser in some way and avoid an infinite loop
                    // This can happen when we're speculatively parsing parenthesized expressions which we think may be arrow functions,
                    // or when a modifier keyword which is disallowed as a parameter name (ie, `static` in strict mode) is supplied
                    self.nextToken();
                }
                continue;
            }

            if self.isListTerminator(kind) {
                break;
            }

            if self.abortParsingListOrMoveToNextToken(kind) {
                break;
            }
        }

        self.parsingContext = saveParsingContext;
        // Recording the trailing comma is deliberately done after the previous
        // loop, and not just if we see a list terminator. This is because the list
        // may have ended incorrectly, but it is still important to know if there
        // was a trailing comma.
        // Check if the last token was a comma.
        // Always preserve a trailing comma by marking it on the NodeArray
        self.createNodeArray(list, listPos, None, commaStart.is_some())
    }

    fn getExpectedCommaDiagnostic(&self, kind: ParsingContext) -> Option<DiagnosticMessage> {
        if kind == ParsingContext::EnumMembers {
            Some(Diagnostics::An_enum_member_name_must_be_followed_by_a_or)
        } else {
            None
        }
    }

    //     interface MissingList<T extends Node> extends NodeArray<T> {
    //         isMissingList: true;
    //     }

    fn createMissingList<T: IsNode>(&mut self) -> MissingList<T> {
        // TODO:
        self.createNodeArray(Vec::new(), self.getNodePos(), None, false)
        // let list = createNodeArray<T>([], getNodePos()) as MissingList<T>;
        // list.isMissingList = true;
        // return list;
    }

    //     function isMissingList(arr: NodeArray<Node>): boolean {
    //         return !!(arr as MissingList<Node>).isMissingList;
    //     }

    fn parseBracketedList<T: IsNode, F>(
        &mut self,
        kind: ParsingContext,
        parseElement: F,
        open: SyntaxKind,
        close: SyntaxKind,
    ) -> NodeArray<T>
    where
        F: FnMut(&mut Parser) -> T,
    {
        if self.parseExpected(open, None, None) {
            let result = self.parseDelimitedList(kind, parseElement, false);
            self.parseExpected(close, None, None);
            return result;
        }

        todo!();
        // return createMissingList<T>();
    }

    fn parseEntityName(
        &mut self,
        allowReservedWords: bool,
        diagnosticMessage: Option<DiagnosticMessage>,
    ) -> EntityName {
        enum MutableEntityName {
            Identifier(Identifier),
            QualifiedName(QualifiedName),
        }

        impl Into<EntityName> for MutableEntityName {
            fn into(self) -> EntityName {
                match self {
                    MutableEntityName::Identifier(n) => EntityName::Identifier(Rc::new(n)),
                    MutableEntityName::QualifiedName(n) => EntityName::QualifiedName(Rc::new(n)),
                }
            }
        }

        let pos = self.getNodePos();
        let mut entity = MutableEntityName::Identifier(if allowReservedWords {
            self.parseIdentifierName(diagnosticMessage)
        } else {
            self.parseIdentifier(diagnosticMessage, None)
        });
        let mut dotPos = self.getNodePos();
        while self.parseOptional(SyntaxKind::DotToken) {
            if self.token() == SyntaxKind::LessThanToken {
                // the entity is part of a JSDoc-style generic, so record the trailing dot for later error reporting
                match &mut entity {
                    MutableEntityName::Identifier(n) => n.jsdocDotPos = Some(dotPos),
                    MutableEntityName::QualifiedName(n) => n.jsdocDotPos = Some(dotPos),
                }
                break;
            }
            dotPos = self.getNodePos();
            let right = self.parseRightSideOfDot(allowReservedWords, false);
            let name = self.factory.createQualifiedName(
                entity.into(),
                unwrap_as!(right, MemberName::Identifier(i), i),
            );
            entity = MutableEntityName::QualifiedName(self.finishNode(name, pos, None));
        }
        entity.into()
    }

    //     function createQualifiedName(entity: EntityName, name: Identifier): QualifiedName {
    //         return finishNode(factory.createQualifiedName(entity, name), entity.pos);
    //     }

    fn parseRightSideOfDot(
        &mut self,
        allowIdentifierNames: bool,
        allowPrivateIdentifiers: bool,
    ) -> MemberName {
        // Technically a keyword is valid here as all identifiers and keywords are identifier names.
        // However, often we'll encounter this in error situations when the identifier or keyword
        // is actually starting another valid construct.
        //
        // So, we check for the following specific case:
        //
        //      name.
        //      identifierOrKeyword identifierNameOrKeyword
        //
        // Note: the newlines are important here.  For example, if that above code
        // were rewritten into:
        //
        //      name.identifierOrKeyword
        //      identifierNameOrKeyword
        //
        // Then we would consider it valid.  That's because ASI would take effect and
        // the code would be implicitly: "name.identifierOrKeyword; identifierNameOrKeyword".
        // In the first though, ASI will not take effect because there is not a
        // line terminator after the identifier or keyword.
        if self.scanner.hasPrecedingLineBreak() && tokenIsIdentifierOrKeyword(self.token()) {
            let matchesPattern = self.lookAhead(|p| p.nextTokenIsIdentifierOrKeywordOnSameLine());

            if matchesPattern {
                // Report that we need an identifier.  However, report it right after the dot,
                // and not on the next token.  This is because the next token might actually
                // be an identifier and the error would be quite confusing.
                todo!();
                // return self.createMissingNode::<Identifier>(
                //     SyntaxKind::Identifier,
                //     /*reportAtCurrentPosition*/ true,
                //     Diagnostics::Identifier_expected,
                // );
            }
        }

        if self.token() == SyntaxKind::PrivateIdentifier {
            let node = self.parsePrivateIdentifier();
            return if allowPrivateIdentifiers {
                MemberName::PrivateIdentifier(Rc::new(node))
            } else {
                todo!();
                // self.createMissingNode::<Identifier>(
                //     SyntaxKind::Identifier,
                //     /*reportAtCurrentPosition*/ true,
                //     Diagnostics::Identifier_expected,
                // )
            };
        }

        if allowIdentifierNames {
            MemberName::Identifier(Rc::new(self.parseIdentifierName(None)))
        } else {
            MemberName::Identifier(Rc::new(self.parseIdentifier(None, None)))
        }
    }

    fn parseTemplateSpans(&mut self, isTaggedTemplate: bool) -> NodeArray<TemplateSpan> {
        let pos = self.getNodePos();
        let mut list = Vec::new();
        loop {
            let node = self.parseTemplateSpan(isTaggedTemplate);
            let is_middle = matches!(node.literal, TemplateSpanLiteral::TemplateMiddle(_));
            list.push(node);
            if !is_middle {
                break;
            }
        }
        self.createNodeArray(list, pos, None, false)
    }

    fn parseTemplateExpression(&mut self, isTaggedTemplate: bool) -> TemplateExpression {
        let pos = self.getNodePos();
        let head = self.parseTemplateHead(isTaggedTemplate);
        let templateSpans = self.parseTemplateSpans(isTaggedTemplate);
        let node = self.factory.createTemplateExpression(head, templateSpans);
        self.finishNode(node, pos, None)
    }

    fn parseTemplateType(&mut self) -> TemplateLiteralTypeNode {
        let pos = self.getNodePos();
        let head = self.parseTemplateHead(false);
        let template_spans = self.parseTemplateTypeSpans();
        let node = self.factory.createTemplateLiteralType(head, template_spans);
        self.finishNode(node, pos, None)
    }

    fn parseTemplateTypeSpans(&mut self) -> NodeArray<TemplateLiteralTypeSpan> {
        let pos = self.getNodePos();
        let mut list = Vec::new();
        loop {
            let node = self.parseTemplateTypeSpan();
            let is_middle = matches!(node.literal, TemplateSpanLiteral::TemplateMiddle(_));
            list.push(node);
            if !is_middle {
                break;
            }
        }

        self.createNodeArray(list, pos, None, false)
    }

    fn parseTemplateTypeSpan(&mut self) -> TemplateLiteralTypeSpan {
        let pos = self.getNodePos();
        let ty = self.parseType();
        let literal = self.parseLiteralOfTemplateSpan(false);
        let node = self.factory.createTemplateLiteralTypeSpan(ty, literal);
        self.finishNode(node, pos, None)
    }

    fn parseLiteralOfTemplateSpan(&mut self, isTaggedTemplate: bool) -> TemplateSpanLiteral {
        if self.token() == SyntaxKind::CloseBraceToken {
            self.reScanTemplateToken(isTaggedTemplate);
            self.parseTemplateMiddleOrTemplateTail()
        } else {
            todo!();
            // TODO(rbuckton): Do we need to call `parseExpectedToken` or can we just call `createMissingNode` directly?
            // self.parseExpectedToken(
            //     SyntaxKind::TemplateTail,
            //     Some(Diagnostics::_0_expected),
            //     Some(tokenToString(SyntaxKind::CloseBraceToken)),
            // ) as TemplateTail
        }
    }

    fn parseTemplateSpan(&mut self, isTaggedTemplate: bool) -> TemplateSpan {
        let pos = self.getNodePos();
        let expression = self.allowInAnd(|p| p.parseExpression());
        let literal = self.parseLiteralOfTemplateSpan(isTaggedTemplate);
        let node = self.factory.createTemplateSpan(expression, literal);
        self.finishNode(node, pos, None)
    }

    // fn parseLiteralNode(&mut self) -> LiteralExpression {
    //     self.parseLiteralLikeNode(self.token()) as LiteralExpression;
    // }

    fn parseTemplateHead(&mut self, isTaggedTemplate: bool) -> TemplateHead {
        if isTaggedTemplate {
            self.reScanTemplateHeadOrNoSubstitutionTemplate();
        }
        let rawText = self.getTemplateLiteralRawText(SyntaxKind::TemplateHead);
        let mut node = self.factory.createTemplateHeadUnchecked(
            self.scanner.getTokenValue().into(),
            Some(rawText),
            self.scanner.getTokenFlags() & TokenFlags::TemplateLiteralLikeFlags,
        );

        if self.scanner.hasExtendedUnicodeEscape() {
            node.hasExtendedUnicodeEscape = Some(true);
        }

        if self.scanner.isUnterminated() {
            node.isUnterminated = Some(true);
        }

        self.nextToken();
        node
    }

    fn parseTemplateMiddleOrTemplateTail(&mut self) -> TemplateSpanLiteral {
        match self.token() {
            SyntaxKind::TemplateMiddle => {
                let rawText = self
                    .getTemplateLiteralRawText(SyntaxKind::TemplateMiddle)
                    .into();
                let node = TemplateSpanLiteral::TemplateMiddle(Rc::new(
                    self.factory.createTemplateMiddleUnchecked(
                        self.scanner.getTokenValue().into(),
                        rawText,
                        self.scanner.getTokenFlags() & TokenFlags::TemplateLiteralLikeFlags,
                    ),
                ));

                if self.scanner.hasExtendedUnicodeEscape() {
                    todo!();
                    // node.hasExtendedUnicodeEscape = Some(true);
                }

                if self.scanner.isUnterminated() {
                    todo!();
                    // node.isUnterminated = Some(true);
                }

                self.nextToken();

                node
            }
            SyntaxKind::TemplateTail => {
                let rawText = self
                    .getTemplateLiteralRawText(SyntaxKind::TemplateTail)
                    .into();
                let node = TemplateSpanLiteral::TemplateTail(Rc::new(
                    self.factory.createTemplateTailUnchecked(
                        self.scanner.getTokenValue().into(),
                        rawText,
                        self.scanner.getTokenFlags() & TokenFlags::TemplateLiteralLikeFlags,
                    ),
                ));

                if self.scanner.hasExtendedUnicodeEscape() {
                    todo!();
                    // node.hasExtendedUnicodeEscape = Some(true);
                }

                if self.scanner.isUnterminated() {
                    todo!();
                    // node.isUnterminated = Some(true);
                }

                self.nextToken();

                node
            }
            _ => unreachable!("Template fragment has wrong token kind"),
        }
    }

    fn getTemplateLiteralRawText(&mut self, kind: SyntaxKind) -> Rc<str> {
        let isLast =
            kind == SyntaxKind::NoSubstitutionTemplateLiteral || kind == SyntaxKind::TemplateTail;
        let tokenText = self.scanner.getTokenText();
        let trim_end = if self.scanner.isUnterminated() {
            0
        } else if isLast {
            1
        } else {
            2
        };
        let end = tokenText.len() - trim_end;
        tokenText[1..end].into()
    }

    // TODO: remove
    // fn parseLiteralLikeNode(&mut self, kind: SyntaxKind) -> LiteralLikeNode {
    //     let pos = self.getNodePos();
    //     let node = if self.isTemplateLiteralKind(kind) {
    //         self.factory.createTemplateLiteralLikeNode(
    //             kind,
    //             self.scanner.getTokenValue(),
    //             self.getTemplateLiteralRawText(kind),
    //             self.scanner.getTokenFlags() & TokenFlags::TemplateLiteralLikeFlags,
    //         )
    //     }
    //     // Octal literals are not allowed in strict mode or ES5
    //     // Note that theoretically the following condition would hold true literals like 009,
    //     // which is not octal. But because of how the scanner separates the tokens, we would
    //     // never get a token like this. Instead, we would get 00 and 9 as two separate tokens.
    //     // We also do not need to check for negatives because any prefix operator would be part of a
    //     // parent unary expression.
    //     else if kind == SyntaxKind::NumericLiteral {
    //         todo!("use parseNumericLiteral");
    //     } else if kind == SyntaxKind::StringLiteral {
    //         todo!("use parseStringLiteral");
    //     } else if isLiteralKind(kind) {
    //         self.factory
    //             .createLiteralLikeNode(kind, self.scanner.getTokenValue())
    //     } else {
    //         unreachable!();
    //     };

    //     if self.scanner.hasExtendedUnicodeEscape() {
    //         node.hasExtendedUnicodeEscape = true;
    //     }

    //     if self.scanner.isUnterminated() {
    //         node.isUnterminated = true;
    //     }

    //     self.nextToken();
    //     self.finishNode(node, pos, None)
    // }

    // Extracted from ts `parseLiteralLikeNode`.
    fn parseStringLiteral(&mut self) -> StringLiteral {
        debug_assert!(self.token() == SyntaxKind::StringLiteral);

        let pos = self.getNodePos();
        let mut node = self.factory.createStringLiteral(
            self.scanner.getTokenValue().into(),
            None,
            self.scanner.hasExtendedUnicodeEscape(),
        );

        if self.scanner.hasExtendedUnicodeEscape() {
            node.hasExtendedUnicodeEscape = Some(true);
        }

        if self.scanner.isUnterminated() {
            node.isUnterminated = Some(true);
        }

        self.nextToken();
        self.finishNode(node, pos, None)
    }

    // Extracted from ts `parseLiteralLikeNode`.
    fn parseNumericLiteral(&mut self) -> NumericLiteral {
        debug_assert!(self.token() == SyntaxKind::NumericLiteral);

        let pos = self.getNodePos();
        // Octal literals are not allowed in strict mode or ES5
        // Note that theoretically the following condition would hold true literals like 009,
        // which is not octal. But because of how the scanner separates the tokens, we would
        // never get a token like this. Instead, we would get 00 and 9 as two separate tokens.
        // We also do not need to check for negatives because any prefix operator would be part of a
        // parent unary expression.
        let node = self.factory.createNumericLiteral(
            self.scanner.getTokenValue().into(),
            Some(self.scanner.getNumericLiteralFlags()),
        );

        if self.scanner.hasExtendedUnicodeEscape() {
            todo!("unreachable?");
            // node.hasExtendedUnicodeEscape = true;
        }

        if self.scanner.isUnterminated() {
            todo!("unreachable?");
            // node.isUnterminated = true;
        }

        self.nextToken();
        self.finishNode(node, pos, None)
    }

    // Extracted from ts `parseLiteralLikeNode`.
    fn parseRegularExpressionLiteral(&mut self) -> RegularExpressionLiteral {
        debug_assert!(self.token() == SyntaxKind::RegularExpressionLiteral);

        let pos = self.getNodePos();
        let node = self
            .factory
            .createRegularExpressionLiteral(self.scanner.getTokenValue().into());

        if self.scanner.hasExtendedUnicodeEscape() {
            todo!("unreachable?");
            // node.hasExtendedUnicodeEscape = Some(true);
        }

        if self.scanner.isUnterminated() {
            todo!("unreachable?");
            // node.isUnterminated = Some(true);
        }

        self.nextToken();
        self.finishNode(node, pos, None)
    }

    // Extracted from ts `parseLiteralLikeNode`.
    fn parseBigIntLiteral(&mut self) -> BigIntLiteral {
        debug_assert!(self.token() == SyntaxKind::BigIntLiteral);

        let pos = self.getNodePos();
        let node = self
            .factory
            .createBigIntLiteral(self.scanner.getTokenValue().into());

        if self.scanner.hasExtendedUnicodeEscape() {
            todo!("unreachable?");
            // node.hasExtendedUnicodeEscape = true;
        }

        if self.scanner.isUnterminated() {
            todo!("unreachable?");
            // node.isUnterminated = true;
        }

        self.nextToken();
        self.finishNode(node, pos, None)
    }

    // Extracted from ts `parseLiteralLikeNode`.
    fn parseNoSubstitutionTemplateLiteral(&mut self) -> NoSubstitutionTemplateLiteral {
        debug_assert!(self.token() == SyntaxKind::NoSubstitutionTemplateLiteral);

        let pos = self.getNodePos();
        let rawText = self.getTemplateLiteralRawText(SyntaxKind::NoSubstitutionTemplateLiteral);
        let mut node = self.factory.createNoSubstitutionTemplateLiteralUnchecked(
            self.scanner.getTokenValue().into(),
            Some(rawText),
            self.scanner.getTokenFlags() & TokenFlags::TemplateLiteralLikeFlags,
        );

        if self.scanner.hasExtendedUnicodeEscape() {
            node.hasExtendedUnicodeEscape = Some(true);
        }

        if self.scanner.isUnterminated() {
            node.isUnterminated = Some(true);
        }

        self.nextToken();
        self.finishNode(node, pos, None)
    }

    // TYPES

    fn parseEntityNameOfTypeReference(&mut self) -> EntityName {
        self.parseEntityName(true, Some(Diagnostics::Type_expected))
    }

    fn parseTypeArgumentsOfTypeReference(&mut self) -> Option<NodeArray<TypeNode>> {
        if !self.scanner.hasPrecedingLineBreak()
            && self.reScanLessThanToken() == SyntaxKind::LessThanToken
        {
            Some(self.parseBracketedList(
                ParsingContext::TypeArguments,
                |p| p.parseType(),
                SyntaxKind::LessThanToken,
                SyntaxKind::GreaterThanToken,
            ))
        } else {
            None
        }
    }

    fn parseTypeReference(&mut self) -> TypeReferenceNode {
        let pos = self.getNodePos();
        let name = self.parseEntityNameOfTypeReference();
        let arguments = self.parseTypeArgumentsOfTypeReference();
        let node = self.factory.createTypeReferenceNode(name, arguments);
        self.finishNode(node, pos, None)
    }

    // If true, we should abort parsing an error function.
    fn typeHasArrowFunctionBlockingParseError(&mut self, node: &TypeNode) -> bool {
        match node {
            TypeNode::TypeReferenceNode(n) => nodeIsMissing(Some(&n.typeName)),
            TypeNode::FunctionTypeNode(n) => {
                todo!();
                // isMissingList(n.parameters) || self.typeHasArrowFunctionBlockingParseError(n.ty.as_ref())
            }
            TypeNode::ConstructorTypeNode(n) => {
                todo!();
                // isMissingList(n.parameters) || self.typeHasArrowFunctionBlockingParseError(n.ty.as_ref())
            }
            TypeNode::ParenthesizedTypeNode(n) => {
                self.typeHasArrowFunctionBlockingParseError(&n.ty)
            }
            _ => false,
        }
    }

    fn parseThisTypePredicate(&mut self, lhs: ThisTypeNode) -> TypePredicateNode {
        self.nextToken();
        let ty = self.parseType();
        let pos = self.factory.node_data(&lhs).get_range().pos;
        let node = self.factory.createTypePredicateNode(
            None,
            TypePredicateParameterName::ThisTypeNode(Rc::new(lhs)),
            Some(ty),
        );
        self.finishNode(node, pos, None)
    }

    fn parseThisTypeNode(&mut self) -> ThisTypeNode {
        let pos = self.getNodePos();
        self.nextToken();
        let node = self.factory.createThisTypeNode();
        self.finishNode(node, pos, None)
    }

    fn parseJSDocAllType(&mut self) -> JSDocAllType {
        let pos = self.getNodePos();
        self.nextToken();
        let node = self.factory.createJSDocAllType();
        self.finishNode(node, pos, None)
    }

    fn parseJSDocNonNullableType(&mut self) -> TypeNode {
        let pos = self.getNodePos();
        self.nextToken();
        let argument = self.parseNonArrayType();
        let node = self.factory.createJSDocNonNullableType(argument);
        TypeNode::JSDocNonNullableType(Rc::new(self.finishNode(node, pos, None)))
    }

    fn parseJSDocUnknownOrNullableType(&mut self) -> TypeNode {
        let pos = self.getNodePos();
        // skip the ?
        self.nextToken();

        // Need to lookahead to decide if this is a nullable or unknown type.

        // Here are cases where we'll pick the unknown type:
        //
        //      Foo(?,
        //      { a: ? }
        //      Foo(?)
        //      Foo<?>
        //      Foo(?=
        //      (?|
        match self.token() {
            SyntaxKind::CommaToken
            | SyntaxKind::CloseBraceToken
            | SyntaxKind::CloseParenToken
            | SyntaxKind::GreaterThanToken
            | SyntaxKind::EqualsToken
            | SyntaxKind::BarToken => {
                let node = self.factory.createJSDocUnknownType();
                TypeNode::JSDocUnknownType(Rc::new(self.finishNode(node, pos, None)))
            }
            _ => {
                let ty = self.parseType();
                let node = self.factory.createJSDocNullableType(ty);
                TypeNode::JSDocNullableType(Rc::new(self.finishNode(node, pos, None)))
            }
        }
    }

    fn parseJSDocFunctionType(&mut self) -> TypeNode {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        if self.lookAhead(|p| p.nextTokenIsOpenParen()) {
            self.nextToken();
            let parameters = self.parseParameters(SignatureFlags::Type | SignatureFlags::JSDoc);
            let ty = self.parseReturnType(SyntaxKind::ColonToken, false);
            let node = self.factory.createJSDocFunctionType(parameters, ty);
            let node = self.finishNode(node, pos, None);
            TypeNode::JSDocFunctionType(Rc::new(self.withJSDoc(node, hasJSDoc)))
        } else {
            let typeName = EntityName::Identifier(Rc::new(self.parseIdentifierName(None)));
            let node = self.factory.createTypeReferenceNode(typeName, None);
            TypeNode::TypeReferenceNode(Rc::new(self.finishNode(node, pos, None)))
        }
    }

    fn parseJSDocParameter(&mut self) -> ParameterDeclaration {
        let pos = self.getNodePos();
        let mut name = None;
        if self.token() == SyntaxKind::ThisKeyword || self.token() == SyntaxKind::NewKeyword {
            name = Some(self.parseIdentifierName(None));
            self.parseExpected(SyntaxKind::ColonToken, None, None);
        }
        let ty = self.parseJSDocType();
        let node = self.factory.createParameterDeclaration(
            None,
            None,
            None,
            // TODO:
            // TODO(rbuckton): JSDoc parameters don't have names (except `this`/`new`), should we manufacture an empty identifier?
            BindingName::Identifier(Rc::new(name.unwrap())),
            None,
            Some(ty),
            None,
        );
        self.finishNode(node, pos, None)
    }

    fn parseJSDocType(&mut self) -> TypeNode {
        todo!();
        // self.scanner.setInJSDocType(true);
        // let pos = self.getNodePos();
        // if self.parseOptional(SyntaxKind::ModuleKeyword) {
        //     todo!();
        //     // // TODO(rbuckton): We never set the type for a JSDocNamepathType. What should we put here?
        //     // let moduleTag = factory.createJSDocNamepathType(/*type*/ undefined!);
        //     // terminate: while (true) {
        //     //     switch (token()) {
        //     //         SyntaxKind::CloseBraceToken:
        //     //         SyntaxKind::EndOfFileToken:
        //     //         SyntaxKind::CommaToken:
        //     //         SyntaxKind::WhitespaceTrivia:
        //     //             break terminate;
        //     //         default:
        //     //             nextTokenJSDoc();
        //     //     }
        //     // }

        //     // scanner.setInJSDocType(false);
        //     // return finishNode(moduleTag, pos);
        // }

        // let hasDotDotDot = self.parseOptional(SyntaxKind::DotDotDotToken);
        // let ty = self.parseTypeOrTypePredicate();
        // self.scanner.setInJSDocType(false);
        // if hasDotDotDot {
        //     ty = self.finishNode(self.factory.createJSDocVariadicType(ty), pos, None);
        // }
        // if self.token() == SyntaxKind::EqualsToken {
        //     self.nextToken();
        //     return self.finishNode(self.factory.createJSDocOptionalType(ty), pos, None);
        // }
        // ty
    }

    fn parseTypeQuery(&mut self) -> TypeQueryNode {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::TypeOfKeyword, None, None);
        let expr_name = self.parseEntityName(true, None);
        let node = self.factory.createTypeQueryNode(expr_name);
        self.finishNode(node, pos, None)
    }

    fn parseTypeParameter(&mut self) -> TypeParameterDeclaration {
        let pos = self.getNodePos();
        let name = self.parseIdentifier(None, None);
        let mut constraint = None;
        let mut expression = None;
        if self.parseOptional(SyntaxKind::ExtendsKeyword) {
            // It's not uncommon for people to write improper constraints to a generic.  If the
            // user writes a constraint that is an expression and not an actual type, then parse
            // it out as an expression (so we can recover well), but report that a type is needed
            // instead.
            if self.isStartOfType(false) || !self.isStartOfExpression() {
                constraint = Some(self.parseType());
            } else {
                // It was not a type, and it looked like an expression.  Parse out an expression
                // here so we recover well.  Note: it is important that we call parseUnaryExpression
                // and not parseExpression here.  If the user has:
                //
                //      <T extends "">
                //
                // We do *not* want to consume the `>` as we're consuming the expression for "".
                expression = Some(self.parseUnaryExpressionOrHigher());
            }
        }

        let defaultType = if self.parseOptional(SyntaxKind::EqualsToken) {
            Some(self.parseType())
        } else {
            None
        };
        let mut node =
            self.factory
                .createTypeParameterDeclaration(Rc::new(name), constraint, defaultType);
        node.expression = expression;
        self.finishNode(node, pos, None)
    }

    fn parseTypeParameters(&mut self) -> Option<NodeArray<TypeParameterDeclaration>> {
        if self.token() == SyntaxKind::LessThanToken {
            Some(self.parseBracketedList(
                ParsingContext::TypeParameters,
                |p| p.parseTypeParameter(),
                SyntaxKind::LessThanToken,
                SyntaxKind::GreaterThanToken,
            ))
        } else {
            None
        }
    }

    fn isStartOfParameter(&mut self, isJSDocParameter: bool) -> bool {
        self.token() == SyntaxKind::DotDotDotToken
            || self.isBindingIdentifierOrPrivateIdentifierOrPattern()
            || isModifierKind(self.token())
            || self.token() == SyntaxKind::AtToken
            || self.isStartOfType(!isJSDocParameter)
    }

    fn parseNameOfParameter(&mut self, has_modifiers: bool) -> BindingName {
        // FormalParameter [Yield,Await]:
        //      BindingElement[?Yield,?Await]
        let name = self.parseIdentifierOrPattern(Some(
            Diagnostics::Private_identifiers_cannot_be_used_as_parameters,
        ));
        if getFullWidth(self.factory.node_data(&name)) == 0
            && !has_modifiers
            && isModifierKind(self.token())
        {
            // in cases like
            // 'use strict'
            // function foo(static)
            // isParameter('static') === true, because of isModifier('static')
            // however 'static' is not a legal identifier in a strict mode.
            // so result of this function will be ParameterDeclaration (flags = 0, name = missing, type = undefined, initializer = undefined)
            // and current token will not change => parsing of the enclosing parameter list will last till the end of time (or OOM)
            // to avoid this we'll advance cursor to the next token.
            self.nextToken();
        }
        name
    }

    fn parseParameterInOuterAwaitContext(&mut self) -> ParameterDeclaration {
        self.parseParameterWorker(true)
    }

    fn parseParameter(&mut self) -> ParameterDeclaration {
        self.parseParameterWorker(false)
    }

    fn parseParameterWorker(&mut self, inOuterAwaitContext: bool) -> ParameterDeclaration {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();

        // FormalParameter [Yield,Await]:
        //      BindingElement[?Yield,?Await]

        // Decorators are parsed in the outer [Await] context, the rest of the parameter is parsed in the function's [Await] context.
        let decorators = if inOuterAwaitContext {
            self.doInAwaitContext(|p| p.parseDecorators())
        } else {
            self.parseDecorators()
        };

        if self.token() == SyntaxKind::ThisKeyword {
            let name = BindingName::Identifier(Rc::new(self.createIdentifier(true, None, None)));
            let ty = self.parseTypeAnnotation();
            if let Some(decorators) = &decorators {
                let range = *self.factory.node_data(&decorators[0]).get_range();
                self.parseErrorAtRange::<u8>(
                    range,
                    Diagnostics::Decorators_may_not_be_applied_to_this_parameters,
                    None,
                );
            }
            let node = self
                .factory
                .createParameterDeclaration(decorators, None, None, name, None, ty, None);

            let node = self.finishNode(node, pos, None);
            return self.withJSDoc(node, hasJSDoc);
        }

        let savedTopLevel = self.topLevel;
        self.topLevel = false;
        let modifiers = self.parseModifiers(false, false);
        let dotDotDotToken = self.parseOptionalToken(SyntaxKind::DotDotDotToken);
        let name = self.parseNameOfParameter(modifiers.is_some());
        let questionToken = self.parseOptionalToken(SyntaxKind::QuestionToken);
        let ty = self.parseTypeAnnotation();
        let initializer = self.parseInitializer();
        let node = self.factory.createParameterDeclaration(
            decorators,
            modifiers,
            dotDotDotToken,
            name,
            questionToken,
            ty,
            initializer,
        );
        let node = self.finishNode(node, pos, None);
        let node = self.withJSDoc(node, hasJSDoc);
        self.topLevel = savedTopLevel;
        node
    }

    // function parseReturnType(returnToken: SyntaxKind::EqualsGreaterThanToken, isType: boolean): TypeNode;
    fn parseReturnType(&mut self, returnToken: SyntaxKind, isType: bool) -> Option<TypeNode> {
        debug_assert!(matches!(
            returnToken,
            SyntaxKind::ColonToken | SyntaxKind::EqualsGreaterThanToken
        ));

        if self.shouldParseReturnType(returnToken, isType) {
            Some(self.parseTypeOrTypePredicate())
        } else {
            None
        }
    }

    fn shouldParseReturnType(&mut self, returnToken: SyntaxKind, isType: bool) -> bool {
        debug_assert!(matches!(
            returnToken,
            SyntaxKind::ColonToken | SyntaxKind::EqualsGreaterThanToken
        ));

        if returnToken == SyntaxKind::EqualsGreaterThanToken {
            self.parseExpected(returnToken, None, None);
            true
        } else if self.parseOptional(SyntaxKind::ColonToken) {
            true
        } else if isType && self.token() == SyntaxKind::EqualsGreaterThanToken {
            // This is easy to get backward, especially in type contexts, so parse the type anyway
            self.parseErrorAtCurrentToken(
                Diagnostics::_0_expected,
                tokenToString(SyntaxKind::ColonToken),
            );
            self.nextToken();
            true
        } else {
            false
        }
    }

    fn parseParametersWorker(&mut self, flags: SignatureFlags) -> NodeArray<ParameterDeclaration> {
        // FormalParameters [Yield,Await]: (modified)
        //      [empty]
        //      FormalParameterList[?Yield,Await]
        //
        // FormalParameter[Yield,Await]: (modified)
        //      BindingElement[?Yield,Await]
        //
        // BindingElement [Yield,Await]: (modified)
        //      SingleNameBinding[?Yield,?Await]
        //      BindingPattern[?Yield,?Await]Initializer [In, ?Yield,?Await] opt
        //
        // SingleNameBinding [Yield,Await]:
        //      BindingIdentifier[?Yield,?Await]Initializer [In, ?Yield,?Await] opt
        let savedYieldContext = self.inYieldContext();
        let savedAwaitContext = self.inAwaitContext();

        self.setYieldContext(flags.intersects(SignatureFlags::Yield));
        self.setAwaitContext(flags.intersects(SignatureFlags::Await));

        let parameters = if flags.intersects(SignatureFlags::JSDoc) {
            self.parseDelimitedList(
                ParsingContext::JSDocParameters,
                |p| p.parseJSDocParameter(),
                false,
            )
        } else {
            self.parseDelimitedList(
                ParsingContext::Parameters,
                |p| {
                    if savedAwaitContext {
                        p.parseParameterInOuterAwaitContext()
                    } else {
                        p.parseParameter()
                    }
                },
                false,
            )
        };

        self.setYieldContext(savedYieldContext);
        self.setAwaitContext(savedAwaitContext);

        parameters
    }

    fn parseParameters(&mut self, flags: SignatureFlags) -> NodeArray<ParameterDeclaration> {
        // FormalParameters [Yield,Await]: (modified)
        //      [empty]
        //      FormalParameterList[?Yield,Await]
        //
        // FormalParameter[Yield,Await]: (modified)
        //      BindingElement[?Yield,Await]
        //
        // BindingElement [Yield,Await]: (modified)
        //      SingleNameBinding[?Yield,?Await]
        //      BindingPattern[?Yield,?Await]Initializer [In, ?Yield,?Await] opt
        //
        // SingleNameBinding [Yield,Await]:
        //      BindingIdentifier[?Yield,?Await]Initializer [In, ?Yield,?Await] opt
        if !self.parseExpected(SyntaxKind::OpenParenToken, None, None) {
            todo!();
            // return createMissingList::<ParameterDeclaration>();
        }

        let parameters = self.parseParametersWorker(flags);
        self.parseExpected(SyntaxKind::CloseParenToken, None, None);
        parameters
    }

    fn parseTypeMemberSemicolon(&mut self) {
        // We allow type members to be separated by commas or (possibly ASI) semicolons.
        // First check if it was a comma.  If so, we're done with the member.
        if self.parseOptional(SyntaxKind::CommaToken) {
            return;
        }

        // Didn't have a comma.  We must have a (possible ASI) semicolon.
        self.parseSemicolon();
    }

    fn parseCallSignatureDeclaration(&mut self) -> CallSignatureDeclaration {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();

        let typeParameters = self.parseTypeParameters();
        let parameters = self.parseParameters(SignatureFlags::Type);
        let ty = self.parseReturnType(SyntaxKind::ColonToken, true);
        self.parseTypeMemberSemicolon();
        let node = self
            .factory
            .createCallSignature(typeParameters, parameters, ty);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }
    fn parseConstructSignatureDeclaration(&mut self) -> ConstructSignatureDeclaration {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        self.parseExpected(SyntaxKind::NewKeyword, None, None);

        let typeParameters = self.parseTypeParameters();
        let parameters = self.parseParameters(SignatureFlags::Type);
        let ty = self.parseReturnType(SyntaxKind::ColonToken, true);
        self.parseTypeMemberSemicolon();
        let node = self
            .factory
            .createConstructSignature(typeParameters, parameters, ty);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn isIndexSignature(&mut self) -> bool {
        self.token() == SyntaxKind::OpenBracketToken
            && self.lookAhead(|p| p.isUnambiguouslyIndexSignature())
    }

    fn isUnambiguouslyIndexSignature(&mut self) -> bool {
        // The only allowed sequence is:
        //
        //   [id:
        //
        // However, for error recovery, we also check the following cases:
        //
        //   [...
        //   [id,
        //   [id?,
        //   [id?:
        //   [id?]
        //   [public id
        //   [private id
        //   [protected id
        //   []
        //
        self.nextToken();
        if self.token() == SyntaxKind::DotDotDotToken
            || self.token() == SyntaxKind::CloseBracketToken
        {
            return true;
        }

        if isModifierKind(self.token()) {
            self.nextToken();
            if self.isIdentifier() {
                return true;
            }
        } else if !self.isIdentifier() {
            return false;
        } else {
            // Skip the identifier
            self.nextToken();
        }

        // A colon signifies a well formed indexer
        // A comma should be a badly formed indexer because comma expressions are not allowed
        // in computed properties.
        if self.token() == SyntaxKind::ColonToken || self.token() == SyntaxKind::CommaToken {
            return true;
        }

        // Question mark could be an indexer with an optional property,
        // or it could be a conditional expression in a computed property.
        if self.token() != SyntaxKind::QuestionToken {
            return false;
        }

        // If any of the following tokens are after the question mark, it cannot
        // be a conditional expression, so treat it as an indexer.
        self.nextToken();
        self.token() == SyntaxKind::ColonToken
            || self.token() == SyntaxKind::CommaToken
            || self.token() == SyntaxKind::CloseBracketToken
    }

    fn parseIndexSignatureDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> IndexSignatureDeclaration {
        let parameters = self.parseBracketedList(
            ParsingContext::Parameters,
            |p| p.parseParameter(),
            SyntaxKind::OpenBracketToken,
            SyntaxKind::CloseBracketToken,
        );
        let ty = self.parseTypeAnnotation();
        self.parseTypeMemberSemicolon();
        let node = self
            .factory
            .createIndexSignature(decorators, modifiers, parameters, ty);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parsePropertyOrMethodSignature(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> TypeElement {
        let name = self.parsePropertyName();
        let questionToken = self.parseOptionalToken(SyntaxKind::QuestionToken);
        if self.token() == SyntaxKind::OpenParenToken || self.token() == SyntaxKind::LessThanToken {
            // Method signatures don't exist in expression contexts.  So they have neither
            // [Yield] nor [Await]
            let typeParameters = self.parseTypeParameters();
            let parameters = self.parseParameters(SignatureFlags::Type);
            let ty = self.parseReturnType(SyntaxKind::ColonToken, true);
            let node = self.factory.createMethodSignature(
                modifiers,
                name,
                questionToken,
                typeParameters,
                parameters,
                ty,
            );
            self.parseTypeMemberSemicolon();
            let node = self.finishNode(node, pos, None);
            TypeElement::MethodSignature(Rc::new(self.withJSDoc(node, hasJSDoc)))
        } else {
            let ty = self.parseTypeAnnotation();
            let mut node = self
                .factory
                .createPropertySignature(modifiers, name, questionToken, ty);
            // Although type literal properties cannot not have initializers, we attempt
            // to parse an initializer so we can report in the checker that an interface
            // property or type literal property cannot have an initializer.
            if self.token() == SyntaxKind::EqualsToken {
                node.initializer = self.parseInitializer();
            }
            self.parseTypeMemberSemicolon();
            let node = self.finishNode(node, pos, None);
            TypeElement::PropertySignature(Rc::new(self.withJSDoc(node, hasJSDoc)))
        }
    }

    fn isTypeMemberStart(&mut self) -> bool {
        // Return true if we have the start of a signature member
        if self.token() == SyntaxKind::OpenParenToken
            || self.token() == SyntaxKind::LessThanToken
            || self.token() == SyntaxKind::GetKeyword
            || self.token() == SyntaxKind::SetKeyword
        {
            return true;
        }
        let mut idToken = false;
        // Eat up all modifiers, but hold on to the last one in it is actually an identifier
        while isModifierKind(self.token()) {
            idToken = true;
            self.nextToken();
        }
        // Index signatures and computed property names are type members
        if self.token() == SyntaxKind::OpenBracketToken {
            return true;
        }
        // Try to get the first property-like token following all modifiers
        if self.isLiteralPropertyName() {
            idToken = true;
            self.nextToken();
        }
        // If we were able to get any potential identifier, check that it is
        // the start of a member declaration
        if idToken {
            return self.token() == SyntaxKind::OpenParenToken
                || self.token() == SyntaxKind::LessThanToken
                || self.token() == SyntaxKind::QuestionToken
                || self.token() == SyntaxKind::ColonToken
                || self.token() == SyntaxKind::CommaToken
                || self.canParseSemicolon();
        }
        false
    }

    fn parseTypeMember(&mut self) -> TypeElement {
        if self.token() == SyntaxKind::OpenParenToken || self.token() == SyntaxKind::LessThanToken {
            return TypeElement::CallSignatureDeclaration(Rc::new(
                self.parseCallSignatureDeclaration(),
            ));
        }
        if self.token() == SyntaxKind::NewKeyword
            && self.lookAhead(|p| p.nextTokenIsOpenParenOrLessThan())
        {
            return TypeElement::ConstructSignatureDeclaration(Rc::new(
                self.parseConstructSignatureDeclaration(),
            ));
        }
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        let modifiers = self.parseModifiers(false, false);
        if self.parseContextualModifier(SyntaxKind::GetKeyword) {
            return TypeElement::GetAccessorDeclaration(Rc::new(
                self.parseGetAccessorDeclaration(pos, hasJSDoc, None, modifiers),
            ));
        }

        if self.parseContextualModifier(SyntaxKind::SetKeyword) {
            return TypeElement::SetAccessorDeclaration(Rc::new(
                self.parseSetAccessorDeclaration(pos, hasJSDoc, None, modifiers),
            ));
        }

        if self.isIndexSignature() {
            return TypeElement::IndexSignatureDeclaration(Rc::new(
                self.parseIndexSignatureDeclaration(pos, hasJSDoc, None, modifiers),
            ));
        }
        self.parsePropertyOrMethodSignature(pos, hasJSDoc, modifiers)
    }

    fn nextTokenIsOpenParenOrLessThan(&mut self) -> bool {
        self.nextToken();
        self.token() == SyntaxKind::OpenParenToken || self.token() == SyntaxKind::LessThanToken
    }

    fn nextTokenIsDot(&mut self) -> bool {
        self.nextToken() == SyntaxKind::DotToken
    }

    fn nextTokenIsOpenParenOrLessThanOrDot(&mut self) -> bool {
        match self.nextToken() {
            SyntaxKind::OpenParenToken | SyntaxKind::LessThanToken | SyntaxKind::DotToken => true,
            _ => false,
        }
    }

    fn parseTypeLiteral(&mut self) -> TypeLiteralNode {
        let pos = self.getNodePos();
        let members = self.parseObjectTypeMembers();
        let node = self.factory.createTypeLiteralNode(Some(members));
        self.finishNode(node, pos, None)
    }

    fn parseObjectTypeMembers(&mut self) -> NodeArray<TypeElement> {
        if self.parseExpected(SyntaxKind::OpenBraceToken, None, None) {
            let members = self.parseList(ParsingContext::TypeMembers, |p| p.parseTypeMember());
            self.parseExpected(SyntaxKind::CloseBraceToken, None, None);
            members
        } else {
            todo!();
            // self.createMissingList::<TypeElement>()
        }
    }

    fn isStartOfMappedType(&mut self) -> bool {
        self.nextToken();
        if self.token() == SyntaxKind::PlusToken || self.token() == SyntaxKind::MinusToken {
            return self.nextToken() == SyntaxKind::ReadonlyKeyword;
        }
        if self.token() == SyntaxKind::ReadonlyKeyword {
            self.nextToken();
        }
        self.token() == SyntaxKind::OpenBracketToken
            && self.nextTokenIsIdentifier()
            && self.nextToken() == SyntaxKind::InKeyword
    }

    fn parseMappedTypeParameter(&mut self) -> TypeParameterDeclaration {
        let pos = self.getNodePos();
        let name = self.parseIdentifierName(None);
        self.parseExpected(SyntaxKind::InKeyword, None, None);
        let ty = self.parseType();
        let node = self
            .factory
            .createTypeParameterDeclaration(Rc::new(name), Some(ty), None);
        self.finishNode(node, pos, None)
    }

    fn parseMappedType(&mut self) -> MappedTypeNode {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::OpenBraceToken, None, None);
        let readonlyToken = match self.token() {
            SyntaxKind::ReadonlyKeyword => Some(self.token()),
            SyntaxKind::PlusToken | SyntaxKind::MinusToken => {
                self.parseExpected(SyntaxKind::ReadonlyKeyword, None, None);
                Some(self.token())
            }
            _ => None,
        };
        self.parseExpected(SyntaxKind::OpenBracketToken, None, None);
        let typeParameter = self.parseMappedTypeParameter();
        let nameType = if self.parseOptional(SyntaxKind::AsKeyword) {
            Some(self.parseType())
        } else {
            None
        };
        self.parseExpected(SyntaxKind::CloseBracketToken, None, None);
        let questionToken = match self.token() {
            SyntaxKind::QuestionToken => Some(self.token()),
            SyntaxKind::PlusToken | SyntaxKind::MinusToken => {
                self.parseExpected(SyntaxKind::QuestionToken, None, None);
                Some(self.token())
            }
            _ => None,
        };
        let ty = self.parseTypeAnnotation();
        self.parseSemicolon();
        self.parseExpected(SyntaxKind::CloseBraceToken, None, None);
        let node = self.factory.createMappedTypeNode(
            readonlyToken,
            typeParameter,
            nameType,
            questionToken,
            ty,
        );
        self.finishNode(node, pos, None)
    }

    fn parseTupleElementType(&mut self) -> TypeNode {
        let pos = self.getNodePos();
        if self.parseOptional(SyntaxKind::DotDotDotToken) {
            let node = self.parseType();
            let node = self.factory.createRestTypeNode(node);
            return TypeNode::RestTypeNode(Rc::new(self.finishNode(node, pos, None)));
        }
        let ty = self.parseType();
        if let TypeNode::JSDocNullableType(ty) = &ty {
            if self.factory.node_data(ty).get_range().pos
                == self.factory.node_data(&ty.ty).get_range().pos
            {
                let node = self.factory.createOptionalTypeNode(ty.ty.clone());
                let text_range = *self.factory.node_data(&ty).get_range();
                setTextRange(self.factory.node_data_mut(&node), Some(text_range));
                let flags = self.factory.node_data(&ty).flags;
                self.factory.node_data_mut(&node).flags = flags;
                return TypeNode::OptionalTypeNode(Rc::new(node));
            }
        }

        ty
    }

    fn isNextTokenColonOrQuestionColon(&mut self) -> bool {
        self.nextToken() == SyntaxKind::ColonToken
            || (self.token() == SyntaxKind::QuestionToken
                && self.nextToken() == SyntaxKind::ColonToken)
    }

    fn isTupleElementName(&mut self) -> bool {
        if self.token() == SyntaxKind::DotDotDotToken {
            tokenIsIdentifierOrKeyword(self.nextToken()) && self.isNextTokenColonOrQuestionColon()
        } else {
            tokenIsIdentifierOrKeyword(self.token()) && self.isNextTokenColonOrQuestionColon()
        }
    }

    fn parseTupleElementNameOrTupleElementType(&mut self) -> TypeNode {
        if self.lookAhead(|p| p.isTupleElementName()) {
            let pos = self.getNodePos();
            let hasJSDoc = self.hasPrecedingJSDocComment();
            let dotDotDotToken = self.parseOptionalToken(SyntaxKind::DotDotDotToken);
            let name = Rc::new(self.parseIdentifierName(None));
            let questionToken = self.parseOptionalToken(SyntaxKind::QuestionToken);
            self.parseExpected(SyntaxKind::ColonToken, None, None);
            let ty = self.parseTupleElementType();
            let node = self
                .factory
                .createNamedTupleMember(dotDotDotToken, name, questionToken, ty);
            let node = self.finishNode(node, pos, None);
            TypeNode::NamedTupleMember(Rc::new(self.withJSDoc(node, hasJSDoc)))
        } else {
            self.parseTupleElementType()
        }
    }

    fn parseTupleType(&mut self) -> TupleTypeNode {
        let pos = self.getNodePos();
        let elements = self.parseBracketedList(
            ParsingContext::TupleElementTypes,
            |p| p.parseTupleElementNameOrTupleElementType(),
            SyntaxKind::OpenBracketToken,
            SyntaxKind::CloseBracketToken,
        );
        let node = self.factory.createTupleTypeNode(elements);
        self.finishNode(node, pos, None)
    }

    fn parseParenthesizedType(&mut self) -> TypeNode {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::OpenParenToken, None, None);
        let ty = self.parseType();
        self.parseExpected(SyntaxKind::CloseParenToken, None, None);
        let node = self.factory.createParenthesizedType(ty);
        TypeNode::ParenthesizedTypeNode(Rc::new(self.finishNode(node, pos, None)))
    }

    fn parseModifiersForConstructorType(&mut self) -> Option<NodeArray<Modifier>> {
        let mut modifiers = None;
        if self.token() == SyntaxKind::AbstractKeyword {
            let pos = self.getNodePos();
            self.nextToken();
            let modifier = self.factory.createToken(SyntaxKind::AbstractKeyword);
            let modifier = self.finishNode(modifier, pos, None);
            modifiers = Some(self.createNodeArray(vec![modifier], pos, None, false));
        }
        modifiers
    }

    fn parseFunctionOrConstructorType(&mut self) -> TypeNode {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        let modifiers = self.parseModifiersForConstructorType();
        let isConstructorType = self.parseOptional(SyntaxKind::NewKeyword);
        let typeParameters = self.parseTypeParameters();
        let parameters = self.parseParameters(SignatureFlags::Type);
        let ty = self.parseReturnType(SyntaxKind::EqualsGreaterThanToken, false);
        if isConstructorType {
            let node =
                self.factory
                    .createConstructorTypeNode(modifiers, typeParameters, parameters, ty);
            let node = self.finishNode(node, pos, None);
            TypeNode::ConstructorTypeNode(Rc::new(self.withJSDoc(node, hasJSDoc)))
        } else {
            let mut node = self
                .factory
                .createFunctionTypeNode(typeParameters, parameters, ty);
            node.modifiers = modifiers;
            let node = self.finishNode(node, pos, None);
            TypeNode::FunctionTypeNode(Rc::new(self.withJSDoc(node, hasJSDoc)))
        }
    }

    fn parseKeywordAndNoDot<T>(&mut self) -> Option<T>
    where
        T: IsNode + IsSimpleTokenNode,
    {
        let node = self.parseTokenNode();
        if self.token() == SyntaxKind::DotToken {
            None
        } else {
            Some(node)
        }
    }

    fn parseLiteralTypeNode(&mut self, negative: bool) -> LiteralTypeNode {
        let pos = self.getNodePos();
        if negative {
            self.nextToken();
        }
        let mut expression = match self.token() {
            SyntaxKind::TrueKeyword => {
                LiteralTypeNodeKind::TrueLiteral(Rc::new(self.parseTokenNode()))
            }
            SyntaxKind::FalseKeyword => {
                LiteralTypeNodeKind::FalseLiteral(Rc::new(self.parseTokenNode()))
            }
            SyntaxKind::NullKeyword => {
                LiteralTypeNodeKind::NullLiteral(Rc::new(self.parseTokenNode()))
            }
            SyntaxKind::NumericLiteral => {
                LiteralTypeNodeKind::NumericLiteral(Rc::new(self.parseNumericLiteral()))
            }
            SyntaxKind::StringLiteral => {
                LiteralTypeNodeKind::StringLiteral(Rc::new(self.parseStringLiteral()))
            }
            SyntaxKind::BigIntLiteral => {
                LiteralTypeNodeKind::BigIntLiteral(Rc::new(self.parseBigIntLiteral()))
            }
            SyntaxKind::NoSubstitutionTemplateLiteral => {
                LiteralTypeNodeKind::NoSubstitutionTemplateLiteral(Rc::new(
                    self.parseNoSubstitutionTemplateLiteral(),
                ))
            }
            _ => unreachable!(),
        };
        if negative {
            let expr = match expression {
                LiteralTypeNodeKind::NumericLiteral(n) => UnaryExpression::NumericLiteral(n),
                LiteralTypeNodeKind::BigIntLiteral(n) => UnaryExpression::BigIntLiteral(n),
                _ => unreachable!("only num and bigint can be negated"),
            };
            let new_expr = self
                .factory
                .createPrefixUnaryExpression(SyntaxKind::MinusToken, expr);
            expression = LiteralTypeNodeKind::PrefixUnaryExpression(Rc::new(
                self.finishNode(new_expr, pos, None),
            ));
        }
        let node = self.factory.createLiteralTypeNode(expression);
        self.finishNode(node, pos, None)
    }

    fn isStartOfTypeOfImportType(&mut self) -> bool {
        self.nextToken();
        self.token() == SyntaxKind::ImportKeyword
    }

    fn parseImportType(&mut self) -> ImportTypeNode {
        self.sourceFlags |= NodeFlags::PossiblyContainsDynamicImport;
        let pos = self.getNodePos();
        let isTypeOf = self.parseOptional(SyntaxKind::TypeOfKeyword);
        self.parseExpected(SyntaxKind::ImportKeyword, None, None);
        self.parseExpected(SyntaxKind::OpenParenToken, None, None);
        let ty = self.parseType();
        self.parseExpected(SyntaxKind::CloseParenToken, None, None);
        let qualifier = if self.parseOptional(SyntaxKind::DotToken) {
            Some(self.parseEntityNameOfTypeReference())
        } else {
            None
        };
        let typeArguments = self.parseTypeArgumentsOfTypeReference();
        let node = self
            .factory
            .createImportTypeNode(ty, qualifier, typeArguments, isTypeOf);
        self.finishNode(node, pos, None)
    }

    fn nextTokenIsNumericOrBigIntLiteral(&mut self) -> bool {
        self.nextToken();
        self.token() == SyntaxKind::NumericLiteral || self.token() == SyntaxKind::BigIntLiteral
    }

    fn parseNonArrayType(&mut self) -> TypeNode {
        match self.token() {
            SyntaxKind::AnyKeyword
            | SyntaxKind::UnknownKeyword
            | SyntaxKind::StringKeyword
            | SyntaxKind::NumberKeyword
            | SyntaxKind::BigIntKeyword
            | SyntaxKind::SymbolKeyword
            | SyntaxKind::BooleanKeyword
            | SyntaxKind::UndefinedKeyword
            | SyntaxKind::NeverKeyword
            | SyntaxKind::ObjectKeyword => {
                // TODO: ugly
                let r = match self.token() {
                    SyntaxKind::AnyKeyword => self
                        .tryParse(|p| p.parseKeywordAndNoDot())
                        .map(Rc::new)
                        .map(TypeNode::AnyKeyword),
                    SyntaxKind::UnknownKeyword => self
                        .tryParse(|p| p.parseKeywordAndNoDot())
                        .map(Rc::new)
                        .map(TypeNode::UnknownKeyword),
                    SyntaxKind::StringKeyword => self
                        .tryParse(|p| p.parseKeywordAndNoDot())
                        .map(Rc::new)
                        .map(TypeNode::StringKeyword),
                    SyntaxKind::NumberKeyword => self
                        .tryParse(|p| p.parseKeywordAndNoDot())
                        .map(Rc::new)
                        .map(TypeNode::NumberKeyword),
                    SyntaxKind::BigIntKeyword => self
                        .tryParse(|p| p.parseKeywordAndNoDot())
                        .map(Rc::new)
                        .map(TypeNode::BigIntKeyword),
                    SyntaxKind::SymbolKeyword => self
                        .tryParse(|p| p.parseKeywordAndNoDot())
                        .map(Rc::new)
                        .map(TypeNode::SymbolKeyword),
                    SyntaxKind::BooleanKeyword => self
                        .tryParse(|p| p.parseKeywordAndNoDot())
                        .map(Rc::new)
                        .map(TypeNode::BooleanKeyword),
                    SyntaxKind::UndefinedKeyword => self
                        .tryParse(|p| p.parseKeywordAndNoDot())
                        .map(Rc::new)
                        .map(TypeNode::UndefinedKeyword),
                    SyntaxKind::NeverKeyword => self
                        .tryParse(|p| p.parseKeywordAndNoDot())
                        .map(Rc::new)
                        .map(TypeNode::NeverKeyword),
                    SyntaxKind::ObjectKeyword => self
                        .tryParse(|p| p.parseKeywordAndNoDot())
                        .map(Rc::new)
                        .map(TypeNode::ObjectKeyword),
                    _ => unreachable!(),
                };
                // If these are followed by a dot, then parse these out as a dotted type reference instead.
                r.unwrap_or_else(|| TypeNode::TypeReferenceNode(Rc::new(self.parseTypeReference())))
            }
            SyntaxKind::AsteriskEqualsToken => {
                // If there is '*=', treat it as * followed by postfix =
                self.scanner.reScanAsteriskEqualsToken();
                self.emit_scanner_errors();
                TypeNode::JSDocAllType(Rc::new(self.parseJSDocAllType()))
            }
            SyntaxKind::AsteriskToken => TypeNode::JSDocAllType(Rc::new(self.parseJSDocAllType())),
            SyntaxKind::QuestionQuestionToken => {
                // If there is '??', treat it as prefix-'?' in JSDoc type.
                self.scanner.reScanQuestionToken();
                self.emit_scanner_errors();
                self.parseJSDocUnknownOrNullableType()
            }
            SyntaxKind::QuestionToken => self.parseJSDocUnknownOrNullableType(),
            SyntaxKind::FunctionKeyword => self.parseJSDocFunctionType(),
            SyntaxKind::ExclamationToken => self.parseJSDocNonNullableType(),
            SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::StringLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword
            | SyntaxKind::NullKeyword => {
                TypeNode::LiteralTypeNode(Rc::new(self.parseLiteralTypeNode(false)))
            }
            SyntaxKind::MinusToken => {
                if self.lookAhead(|p| p.nextTokenIsNumericOrBigIntLiteral()) {
                    TypeNode::LiteralTypeNode(Rc::new(self.parseLiteralTypeNode(true)))
                } else {
                    TypeNode::TypeReferenceNode(Rc::new(self.parseTypeReference()))
                }
            }
            SyntaxKind::VoidKeyword => {
                TypeNode::VoidKeyword(Rc::new(self.parseTokenNode::<VoidKeyword>()))
            }
            SyntaxKind::ThisKeyword => {
                let thisKeyword = self.parseThisTypeNode();
                if self.token() == SyntaxKind::IsKeyword && !self.scanner.hasPrecedingLineBreak() {
                    TypeNode::TypePredicateNode(Rc::new(self.parseThisTypePredicate(thisKeyword)))
                } else {
                    TypeNode::ThisTypeNode(Rc::new(thisKeyword))
                }
            }
            SyntaxKind::TypeOfKeyword => {
                if self.lookAhead(|p| p.isStartOfTypeOfImportType()) {
                    TypeNode::ImportTypeNode(Rc::new(self.parseImportType()))
                } else {
                    TypeNode::TypeQueryNode(Rc::new(self.parseTypeQuery()))
                }
            }
            SyntaxKind::OpenBraceToken => {
                if self.lookAhead(|p| p.isStartOfMappedType()) {
                    TypeNode::MappedTypeNode(Rc::new(self.parseMappedType()))
                } else {
                    TypeNode::TypeLiteralNode(Rc::new(self.parseTypeLiteral()))
                }
            }
            SyntaxKind::OpenBracketToken => TypeNode::TupleTypeNode(Rc::new(self.parseTupleType())),
            SyntaxKind::OpenParenToken => self.parseParenthesizedType(),
            SyntaxKind::ImportKeyword => TypeNode::ImportTypeNode(Rc::new(self.parseImportType())),
            SyntaxKind::AssertsKeyword => {
                if self.lookAhead(|p| p.nextTokenIsIdentifierOrKeywordOnSameLine()) {
                    self.parseAssertsTypePredicate()
                } else {
                    TypeNode::TypeReferenceNode(Rc::new(self.parseTypeReference()))
                }
            }
            SyntaxKind::TemplateHead => {
                TypeNode::TemplateLiteralTypeNode(Rc::new(self.parseTemplateType()))
            }
            _ => TypeNode::TypeReferenceNode(Rc::new(self.parseTypeReference())),
        }
    }

    fn isStartOfType(&mut self, inStartOfParameter: bool) -> bool {
        match self.token() {
            SyntaxKind::AnyKeyword
            | SyntaxKind::UnknownKeyword
            | SyntaxKind::StringKeyword
            | SyntaxKind::NumberKeyword
            | SyntaxKind::BigIntKeyword
            | SyntaxKind::BooleanKeyword
            | SyntaxKind::ReadonlyKeyword
            | SyntaxKind::SymbolKeyword
            | SyntaxKind::UniqueKeyword
            | SyntaxKind::VoidKeyword
            | SyntaxKind::UndefinedKeyword
            | SyntaxKind::NullKeyword
            | SyntaxKind::ThisKeyword
            | SyntaxKind::TypeOfKeyword
            | SyntaxKind::NeverKeyword
            | SyntaxKind::OpenBraceToken
            | SyntaxKind::OpenBracketToken
            | SyntaxKind::LessThanToken
            | SyntaxKind::BarToken
            | SyntaxKind::AmpersandToken
            | SyntaxKind::NewKeyword
            | SyntaxKind::StringLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword
            | SyntaxKind::ObjectKeyword
            | SyntaxKind::AsteriskToken
            | SyntaxKind::QuestionToken
            | SyntaxKind::ExclamationToken
            | SyntaxKind::DotDotDotToken
            | SyntaxKind::InferKeyword
            | SyntaxKind::ImportKeyword
            | SyntaxKind::AssertsKeyword
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::TemplateHead => true,
            SyntaxKind::FunctionKeyword => !inStartOfParameter,
            SyntaxKind::MinusToken => {
                !inStartOfParameter && self.lookAhead(|p| p.nextTokenIsNumericOrBigIntLiteral())
            }
            SyntaxKind::OpenParenToken => {
                // Only consider '(' the start of a type if followed by ')', '...', an identifier, a modifier,
                // or something that starts a type. We don't want to consider things like '(1)' a type.
                !inStartOfParameter && self.lookAhead(|p| p.isStartOfParenthesizedOrFunctionType())
            }
            _ => self.isIdentifier(),
        }
    }

    fn isStartOfParenthesizedOrFunctionType(&mut self) -> bool {
        self.nextToken();
        self.token() == SyntaxKind::CloseParenToken
            || self.isStartOfParameter(false)
            || self.isStartOfType(false)
    }

    fn parsePostfixTypeOrHigher(&mut self) -> TypeNode {
        let pos = self.getNodePos();
        let mut ty = self.parseNonArrayType();
        while !self.scanner.hasPrecedingLineBreak() {
            match self.token() {
                SyntaxKind::ExclamationToken => {
                    self.nextToken();
                    let node = self.factory.createJSDocNonNullableType(ty);
                    ty = TypeNode::JSDocNonNullableType(Rc::new(self.finishNode(node, pos, None)));
                }
                SyntaxKind::QuestionToken => {
                    // If next token is start of a type we have a conditional type
                    if self.lookAhead(|p| p.nextTokenIsStartOfType()) {
                        return ty;
                    }
                    self.nextToken();
                    let node = self.factory.createJSDocNullableType(ty);
                    ty = TypeNode::JSDocNullableType(Rc::new(self.finishNode(node, pos, None)));
                }
                SyntaxKind::OpenBracketToken => {
                    self.parseExpected(SyntaxKind::OpenBracketToken, None, None);
                    if self.isStartOfType(false) {
                        let indexType = self.parseType();
                        self.parseExpected(SyntaxKind::CloseBracketToken, None, None);
                        let node = self.factory.createIndexedAccessTypeNode(ty, indexType);
                        ty = TypeNode::IndexedAccessTypeNode(Rc::new(
                            self.finishNode(node, pos, None),
                        ));
                    } else {
                        self.parseExpected(SyntaxKind::CloseBracketToken, None, None);
                        let node = self.factory.createArrayTypeNode(ty);
                        ty = TypeNode::ArrayTypeNode(Rc::new(self.finishNode(node, pos, None)));
                    }
                }
                _ => return ty,
            }
        }
        ty
    }

    fn parseTypeOperator(&mut self, operator: SyntaxKind) -> TypeOperatorNode {
        debug_assert!(matches!(
            operator,
            SyntaxKind::KeyOfKeyword | SyntaxKind::UniqueKeyword | SyntaxKind::ReadonlyKeyword
        ));
        let pos = self.getNodePos();
        self.parseExpected(operator, None, None);
        let ty = self.parseTypeOperatorOrHigher();
        let node = self.factory.createTypeOperatorNode(operator, ty);
        self.finishNode(node, pos, None)
    }

    fn parseTypeParameterOfInferType(&mut self) -> TypeParameterDeclaration {
        let pos = self.getNodePos();
        let name = self.parseIdentifier(None, None);
        let node = self
            .factory
            .createTypeParameterDeclaration(Rc::new(name), None, None);
        self.finishNode(node, pos, None)
    }

    fn parseInferType(&mut self) -> InferTypeNode {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::InferKeyword, None, None);
        let param = self.parseTypeParameterOfInferType();
        let node = self.factory.createInferTypeNode(param);
        self.finishNode(node, pos, None)
    }

    fn parseTypeOperatorOrHigher(&mut self) -> TypeNode {
        let operator = self.token();
        match operator {
            SyntaxKind::KeyOfKeyword | SyntaxKind::UniqueKeyword | SyntaxKind::ReadonlyKeyword => {
                TypeNode::TypeOperatorNode(Rc::new(self.parseTypeOperator(operator)))
            }
            SyntaxKind::InferKeyword => TypeNode::InferTypeNode(Rc::new(self.parseInferType())),
            _ => self.parsePostfixTypeOrHigher(),
        }
    }

    fn parseFunctionOrConstructorTypeToError(&mut self, isInUnionType: bool) -> Option<TypeNode> {
        // the function type and constructor type shorthand notation
        // are not allowed directly in unions and intersections, but we'll
        // try to parse them gracefully and issue a helpful message.
        if self.isStartOfFunctionTypeOrConstructorType() {
            let ty = self.parseFunctionOrConstructorType();
            let diagnostic = if isFunctionTypeNode(&ty) {
                if isInUnionType {
                    Diagnostics::Function_type_notation_must_be_parenthesized_when_used_in_a_union_type
                } else {
                    Diagnostics::Function_type_notation_must_be_parenthesized_when_used_in_an_intersection_type
                }
            } else {
                if isInUnionType {
                    Diagnostics::Constructor_type_notation_must_be_parenthesized_when_used_in_a_union_type
                } else {
                    Diagnostics::Constructor_type_notation_must_be_parenthesized_when_used_in_an_intersection_type
                }
            };
            let range = self.factory.node_data(&ty).range;
            self.parseErrorAtRange::<u8>(range, diagnostic, None);
            return Some(ty);
        }
        None
    }

    fn parseUnionOrIntersectionType<T, U>(
        &mut self,
        operator: SyntaxKind,
        parseConstituentType: T,
        createTypeNode: U,
    ) -> TypeNode
    where
        T: Fn(&mut Parser) -> TypeNode,
        U: Fn(&mut Parser, NodeArray<TypeNode>) -> TypeNode,
    {
        debug_assert!(matches!(
            operator,
            SyntaxKind::BarToken | SyntaxKind::AmpersandToken
        ));
        let pos = self.getNodePos();
        let isUnionType = operator == SyntaxKind::BarToken;
        let hasLeadingOperator = self.parseOptional(operator);
        let ty = if hasLeadingOperator {
            self.parseFunctionOrConstructorTypeToError(isUnionType)
        } else {
            None
        };
        let mut ty = ty.unwrap_or_else(|| parseConstituentType(self));
        if self.token() == operator || hasLeadingOperator {
            let mut types = vec![ty];
            while self.parseOptional(operator) {
                let ty = self
                    .parseFunctionOrConstructorTypeToError(isUnionType)
                    .unwrap_or_else(|| parseConstituentType(self));
                types.push(ty);
            }
            let types = self.createNodeArray(types, pos, None, false);
            let node = createTypeNode(self, types);
            ty = self.finishNode(node, pos, None);
            debug_assert!(matches!(
                ty,
                TypeNode::UnionTypeNode(_) | TypeNode::IntersectionTypeNode(_)
            ));
        }
        ty
    }

    fn parseIntersectionTypeOrHigher(&mut self) -> TypeNode {
        self.parseUnionOrIntersectionType(
            SyntaxKind::AmpersandToken,
            |p| p.parseTypeOperatorOrHigher(),
            |p, types| {
                TypeNode::IntersectionTypeNode(Rc::new(p.factory.createIntersectionTypeNode(types)))
            },
        )
    }

    fn parseUnionTypeOrHigher(&mut self) -> TypeNode {
        self.parseUnionOrIntersectionType(
            SyntaxKind::BarToken,
            |p| p.parseIntersectionTypeOrHigher(),
            |p, types| TypeNode::UnionTypeNode(Rc::new(p.factory.createUnionTypeNode(types))),
        )
    }

    fn nextTokenIsNewKeyword(&mut self) -> bool {
        self.nextToken();
        self.token() == SyntaxKind::NewKeyword
    }

    fn isStartOfFunctionTypeOrConstructorType(&mut self) -> bool {
        if self.token() == SyntaxKind::LessThanToken {
            return true;
        }
        if self.token() == SyntaxKind::OpenParenToken
            && self.lookAhead(|p| p.isUnambiguouslyStartOfFunctionType())
        {
            return true;
        }
        self.token() == SyntaxKind::NewKeyword
            || self.token() == SyntaxKind::AbstractKeyword
                && self.lookAhead(|p| p.nextTokenIsNewKeyword())
    }

    fn skipParameterStart(&mut self) -> bool {
        if isModifierKind(self.token()) {
            // Skip modifiers
            self.parseModifiers(false, false);
        }
        if self.isIdentifier() || self.token() == SyntaxKind::ThisKeyword {
            self.nextToken();
            return true;
        }
        if self.token() == SyntaxKind::OpenBracketToken
            || self.token() == SyntaxKind::OpenBraceToken
        {
            // Return true if we can parse an array or object binding pattern with no errors
            let previousErrorCount = self.parseDiagnostics.len();
            self.parseIdentifierOrPattern(None);
            return previousErrorCount == self.parseDiagnostics.len();
        }
        false
    }

    fn isUnambiguouslyStartOfFunctionType(&mut self) -> bool {
        self.nextToken();
        if self.token() == SyntaxKind::CloseParenToken || self.token() == SyntaxKind::DotDotDotToken
        {
            // ( )
            // ( ...
            return true;
        }
        if self.skipParameterStart() {
            // We successfully skipped modifiers (if any) and an identifier or binding pattern,
            // now see if we have something that indicates a parameter declaration
            if self.token() == SyntaxKind::ColonToken
                || self.token() == SyntaxKind::CommaToken
                || self.token() == SyntaxKind::QuestionToken
                || self.token() == SyntaxKind::EqualsToken
            {
                // ( xxx :
                // ( xxx ,
                // ( xxx ?
                // ( xxx =
                return true;
            }
            if self.token() == SyntaxKind::CloseParenToken {
                self.nextToken();
                if self.token() == SyntaxKind::EqualsGreaterThanToken {
                    // ( xxx ) =>
                    return true;
                }
            }
        }
        false
    }

    fn parseTypeOrTypePredicate(&mut self) -> TypeNode {
        let pos = self.getNodePos();
        let typePredicateVariable = if self.isIdentifier() {
            self.tryParse(|p| p.parseTypePredicatePrefix())
        } else {
            None
        };
        let ty = self.parseType();
        if let Some(typePredicateVariable) = typePredicateVariable {
            let node = TypeNode::TypePredicateNode(Rc::new(self.factory.createTypePredicateNode(
                None,
                TypePredicateParameterName::Identifier(Rc::new(typePredicateVariable)),
                Some(ty),
            )));
            self.finishNode(node, pos, None)
        } else {
            ty
        }
    }

    fn parseTypePredicatePrefix(&mut self) -> Option<Identifier> {
        let id = self.parseIdentifier(None, None);
        if self.token() == SyntaxKind::IsKeyword && !self.scanner.hasPrecedingLineBreak() {
            self.nextToken();
            Some(id)
        } else {
            None
        }
    }

    fn parseAssertsTypePredicate(&mut self) -> TypeNode {
        let pos = self.getNodePos();
        let assertsModifier =
            self.parseExpectedToken::<_, u8>(SyntaxKind::AssertsKeyword, None, None);
        let parameterName = if self.token() == SyntaxKind::ThisKeyword {
            TypePredicateParameterName::ThisTypeNode(Rc::new(self.parseThisTypeNode()))
        } else {
            TypePredicateParameterName::Identifier(Rc::new(self.parseIdentifier(None, None)))
        };
        let ty = if self.parseOptional(SyntaxKind::IsKeyword) {
            Some(self.parseType())
        } else {
            None
        };
        let node = self
            .factory
            .createTypePredicateNode(Some(assertsModifier), parameterName, ty);
        TypeNode::TypePredicateNode(Rc::new(self.finishNode(node, pos, None)))
    }

    fn parseType(&mut self) -> TypeNode {
        // The rules about 'yield' only apply to actual code/expression contexts.  They don't
        // apply to 'type' contexts.  So we disable these parameters here before moving on.
        self.doOutsideOfContext(NodeFlags::TypeExcludesFlags, |p| p.parseTypeWorker(false))
    }

    fn parseTypeWorker(&mut self, noConditionalTypes: bool) -> TypeNode {
        if self.isStartOfFunctionTypeOrConstructorType() {
            return self.parseFunctionOrConstructorType();
        }
        let pos = self.getNodePos();
        let ty = self.parseUnionTypeOrHigher();
        if !noConditionalTypes
            && !self.scanner.hasPrecedingLineBreak()
            && self.parseOptional(SyntaxKind::ExtendsKeyword)
        {
            todo!();
            // The type following 'extends' is not permitted to be another conditional type
            // let extendsType = self.parseTypeWorker(/*noConditionalTypes*/ true);
            // self.parseExpected(SyntaxKind::QuestionToken,None,None);
            // let trueType = self.parseTypeWorker(false);
            // self.parseExpected(SyntaxKind::ColonToken,None,None);
            // let falseType = self.parseTypeWorker(false);
            // return self.finishNode(self.factory.createConditionalTypeNode(ty, extendsType, trueType, falseType), pos,None);
        }
        ty
    }

    fn parseTypeAnnotation(&mut self) -> Option<TypeNode> {
        if self.parseOptional(SyntaxKind::ColonToken) {
            Some(self.parseType())
        } else {
            None
        }
    }

    // EXPRESSIONS
    fn isStartOfLeftHandSideExpression(&mut self) -> bool {
        match self.token() {
            SyntaxKind::ThisKeyword
            | SyntaxKind::SuperKeyword
            | SyntaxKind::NullKeyword
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::StringLiteral
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::TemplateHead
            | SyntaxKind::OpenParenToken
            | SyntaxKind::OpenBracketToken
            | SyntaxKind::OpenBraceToken
            | SyntaxKind::FunctionKeyword
            | SyntaxKind::ClassKeyword
            | SyntaxKind::NewKeyword
            | SyntaxKind::SlashToken
            | SyntaxKind::SlashEqualsToken
            | SyntaxKind::Identifier => true,
            SyntaxKind::ImportKeyword => {
                self.lookAhead(|p| p.nextTokenIsOpenParenOrLessThanOrDot())
            }

            _ => self.isIdentifier(),
        }
    }

    fn isStartOfExpression(&mut self) -> bool {
        if self.isStartOfLeftHandSideExpression() {
            return true;
        }

        match self.token() {
            SyntaxKind::PlusToken
            | SyntaxKind::MinusToken
            | SyntaxKind::TildeToken
            | SyntaxKind::ExclamationToken
            | SyntaxKind::DeleteKeyword
            | SyntaxKind::TypeOfKeyword
            | SyntaxKind::VoidKeyword
            | SyntaxKind::PlusPlusToken
            | SyntaxKind::MinusMinusToken
            | SyntaxKind::LessThanToken
            | SyntaxKind::AwaitKeyword
            | SyntaxKind::YieldKeyword
            | SyntaxKind::PrivateIdentifier => {
                // Yield/await always starts an expression.  Either it is an identifier (in which case
                // it is definitely an expression).  Or it's a keyword (either because we're in
                // a generator or async function, or in strict mode (or both)) and it started a yield or await expression.
                return true;
            }
            _ => {
                // Error tolerance.  If we see the start of some binary operator, we consider
                // that the start of an expression.  That way we'll parse out a missing identifier,
                // give a good message about an identifier being missing, and then consume the
                // rest of the binary expression.
                if self.isBinaryOperator() {
                    return true;
                }

                return self.isIdentifier();
            }
        }
    }

    fn isStartOfExpressionStatement(&mut self) -> bool {
        // As per the grammar, none of '{' or 'function' or 'class' can start an expression statement.
        self.token() != SyntaxKind::OpenBraceToken
            && self.token() != SyntaxKind::FunctionKeyword
            && self.token() != SyntaxKind::ClassKeyword
            && self.token() != SyntaxKind::AtToken
            && self.isStartOfExpression()
    }

    fn parseExpression(&mut self) -> Expression {
        // Expression[in]:
        //      AssignmentExpression[in]
        //      Expression[in] , AssignmentExpression[in]

        // clear the decorator context when parsing Expression, as it should be unambiguous when parsing a decorator
        let saveDecoratorContext = self.inDecoratorContext();
        if saveDecoratorContext {
            self.setDecoratorContext(false);
        }

        let pos = self.getNodePos();
        let mut expr = self.parseAssignmentExpressionOrHigher();
        while let Some(operatorToken) =
            self.parseOptionalToken::<CommaToken>(SyntaxKind::CommaToken)
        {
            let right = self.parseAssignmentExpressionOrHigher();
            expr = Expression::BinaryExpression(Rc::new(self.makeBinaryExpression(
                expr,
                BinaryOperatorToken::CommaToken(Rc::new(operatorToken)),
                right,
                pos,
            )));
        }

        if saveDecoratorContext {
            self.setDecoratorContext(true);
        }
        expr
    }

    fn parseInitializer(&mut self) -> Option<Expression> {
        if self.parseOptional(SyntaxKind::EqualsToken) {
            Some(self.parseAssignmentExpressionOrHigher())
        } else {
            None
        }
    }

    fn parseAssignmentExpressionOrHigher(&mut self) -> Expression {
        //  AssignmentExpression[in,yield]:
        //      1) ConditionalExpression[?in,?yield]
        //      2) LeftHandSideExpression = AssignmentExpression[?in,?yield]
        //      3) LeftHandSideExpression AssignmentOperator AssignmentExpression[?in,?yield]
        //      4) ArrowFunctionExpression[?in,?yield]
        //      5) AsyncArrowFunctionExpression[in,yield,await]
        //      6) [+Yield] YieldExpression[?In]
        //
        // Note: for ease of implementation we treat productions '2' and '3' as the same thing.
        // (i.e. they're both BinaryExpressions with an assignment operator in it).

        // First, do the simple check if we have a YieldExpression (production '6').
        if self.isYieldExpression() {
            return Expression::YieldExpression(Rc::new(self.parseYieldExpression()));
        }

        // Then, check if we have an arrow function (production '4' and '5') that starts with a parenthesized
        // parameter list or is an async arrow function.
        // AsyncArrowFunctionExpression:
        //      1) async[no LineTerminator here]AsyncArrowBindingIdentifier[?Yield][no LineTerminator here]=>AsyncConciseBody[?In]
        //      2) CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await][no LineTerminator here]=>AsyncConciseBody[?In]
        // Production (1) of AsyncArrowFunctionExpression is parsed in "tryParseAsyncSimpleArrowFunctionExpression".
        // And production (2) is parsed in "tryParseParenthesizedArrowFunctionExpression".
        //
        // If we do successfully parse arrow-function, we must *not* recurse for productions 1, 2 or 3. An ArrowFunction is
        // not a LeftHandSideExpression, nor does it start a ConditionalExpression.  So we are done
        // with AssignmentExpression if we see one.
        if let Some(expr) = self.tryParseParenthesizedArrowFunctionExpression() {
            return expr;
        } else if let Some(expr) = self.tryParseAsyncSimpleArrowFunctionExpression() {
            return Expression::ArrowFunction(Rc::new(expr));
        }

        // Now try to see if we're in production '1', '2' or '3'.  A conditional expression can
        // start with a LogicalOrExpression, while the assignment productions can only start with
        // LeftHandSideExpressions.
        //
        // So, first, we try to just parse out a BinaryExpression.  If we get something that is a
        // LeftHandSide or higher, then we can try to parse out the assignment expression part.
        // Otherwise, we try to parse out the conditional expression bit.  We want to allow any
        // binary expression here, so we pass in the 'lowest' precedence here so that it matches
        // and consumes anything.
        let pos = self.getNodePos();
        let expr = self.parseBinaryExpressionOrHigher(OperatorPrecedence::Lowest);

        // To avoid a look-ahead, we did not handle the of an arrow function with a single un-parenthesized
        // parameter ('x => ...') above. We handle it here by checking if the parsed expression was a single
        // identifier and the current token is an arrow.
        if self.token() == SyntaxKind::EqualsGreaterThanToken {
            if let Expression::Identifier(expr) = expr {
                return Expression::ArrowFunction(Rc::new(
                    self.parseSimpleArrowFunctionExpression(pos, expr, None),
                ));
            }
        }

        // Now see if we might be in cases '2' or '3'.
        // If the expression was a LHS expression, and we have an assignment operator, then
        // we're in '2' or '3'. Consume the assignment and return.
        //
        // Note: we call reScanGreaterToken so that we get an appropriately merged token
        // for cases like `> > =` becoming `>>=`
        if isLeftHandSideExpression(expr.clone().into())
            && isAssignmentOperator(self.reScanGreaterToken())
        {
            let operatorToken = self.parseTokenNode();
            let right = self.parseAssignmentExpressionOrHigher();
            return Expression::BinaryExpression(Rc::new(self.makeBinaryExpression(
                expr,
                operatorToken,
                right,
                pos,
            )));
        }

        // It wasn't an assignment or a lambda.  This is a conditional expression:
        self.parseConditionalExpressionRest(expr, pos)
    }

    fn isYieldExpression(&mut self) -> bool {
        if self.token() == SyntaxKind::YieldKeyword {
            // If we have a 'yield' keyword, and this is a context where yield expressions are
            // allowed, then definitely parse out a yield expression.
            if self.inYieldContext() {
                return true;
            }

            // We're in a context where 'yield expr' is not allowed.  However, if we can
            // definitely tell that the user was trying to parse a 'yield expr' and not
            // just a normal expr that start with a 'yield' identifier, then parse out
            // a 'yield expr'.  We can then report an error later that they are only
            // allowed in generator expressions.
            //
            // for example, if we see 'yield(foo)', then we'll have to treat that as an
            // invocation expression of something called 'yield'.  However, if we have
            // 'yield foo' then that is not legal as a normal expression, so we can
            // definitely recognize this as a yield expression.
            //
            // for now we just check if the next token is an identifier.  More heuristics
            // can be added here later as necessary.  We just need to make sure that we
            // don't accidentally consume something legal.
            return self.lookAhead(|p| p.nextTokenIsIdentifierOrKeywordOrLiteralOnSameLine());
        }

        false
    }

    fn nextTokenIsIdentifierOnSameLine(&mut self) -> bool {
        self.nextToken();
        !self.scanner.hasPrecedingLineBreak() && self.isIdentifier()
    }

    fn parseYieldExpression(&mut self) -> YieldExpression {
        let pos = self.getNodePos();

        // YieldExpression[In] :
        //      yield
        //      yield [no LineTerminator here] [Lexical goal InputElementRegExp]AssignmentExpression[?In, Yield]
        //      yield [no LineTerminator here] * [Lexical goal InputElementRegExp]AssignmentExpression[?In, Yield]
        self.nextToken();

        if !self.scanner.hasPrecedingLineBreak()
            && (self.token() == SyntaxKind::AsteriskToken || self.isStartOfExpression())
        {
            let asteriskToken = self.parseOptionalToken(SyntaxKind::AsteriskToken);
            let expression = self.parseAssignmentExpressionOrHigher();
            let node = self
                .factory
                .createYieldExpression(asteriskToken, Some(expression));
            self.finishNode(node, pos, None)
        } else {
            // if the next token is not on the same line as yield.  or we don't have an '*' or
            // the start of an expression, then this is just a simple "yield" expression.
            let node = self.factory.createYieldExpression(None, None);
            self.finishNode(node, pos, None)
        }
    }

    fn parseSimpleArrowFunctionExpression(
        &mut self,
        pos: usize,
        identifier: Rc<Identifier>,
        asyncModifier: Option<NodeArray<Modifier>>,
    ) -> ArrowFunction {
        debug_assert!(
            self.token() == SyntaxKind::EqualsGreaterThanToken,
            "parseSimpleArrowFunctionExpression should only have been called if we had a =>"
        );
        let identifier_pos = self.factory.node_data(&identifier).get_range().pos;
        let parameter = self.factory.createParameterDeclaration(
            None,
            None,
            None,
            BindingName::Identifier(identifier),
            None,
            None,
            None,
        );
        let parameter = self.finishNode(parameter, identifier_pos, None);
        let paremeter_range = *self.factory.node_data(&parameter).get_range();

        let parameters = self.createNodeArray(
            vec![parameter],
            paremeter_range.pos,
            Some(paremeter_range.end),
            false,
        );
        let equalsGreaterThanToken =
            self.parseExpectedToken::<_, u8>(SyntaxKind::EqualsGreaterThanToken, None, None);
        let body = self.parseArrowFunctionExpressionBody(asyncModifier.is_some());
        let node = self.factory.createArrowFunction(
            asyncModifier,
            None,
            parameters,
            None,
            Some(equalsGreaterThanToken),
            body,
        );
        let node = self.finishNode(node, pos, None);
        self.addJSDocComment(node)
    }

    fn tryParseParenthesizedArrowFunctionExpression(&mut self) -> Option<Expression> {
        let triState = self.isParenthesizedArrowFunctionExpression();
        if triState == Tristate::False {
            // It's definitely not a parenthesized arrow function expression.
            return None;
        }

        // If we definitely have an arrow function, then we can just parse one, not requiring a
        // following => or { token. Otherwise, we *might* have an arrow function.  Try to parse
        // it out, but don't allow any ambiguity, and return 'undefined' if this could be an
        // expression instead.
        if triState == Tristate::True {
            self.parseParenthesizedArrowFunctionExpression(true)
                .map(|a| Expression::ArrowFunction(Rc::new(a)))
        } else {
            self.tryParse(|p| p.parsePossibleParenthesizedArrowFunctionExpression())
                .map(|a| Expression::ArrowFunction(Rc::new(a)))
        }
    }

    //  True        -> We definitely expect a parenthesized arrow function here.
    //  False       -> There *cannot* be a parenthesized arrow function here.
    //  Unknown     -> There *might* be a parenthesized arrow function here.
    //                 Speculatively look ahead to be sure, and rollback if not.
    fn isParenthesizedArrowFunctionExpression(&mut self) -> Tristate {
        if self.token() == SyntaxKind::OpenParenToken
            || self.token() == SyntaxKind::LessThanToken
            || self.token() == SyntaxKind::AsyncKeyword
        {
            return self.lookAhead(|p| p.isParenthesizedArrowFunctionExpressionWorker());
        }

        if self.token() == SyntaxKind::EqualsGreaterThanToken {
            // ERROR RECOVERY TWEAK:
            // If we see a standalone => try to parse it as an arrow function expression as that's
            // likely what the user intended to write.
            return Tristate::True;
        }
        // Definitely not a parenthesized arrow function.
        Tristate::False
    }

    fn isParenthesizedArrowFunctionExpressionWorker(&mut self) -> Tristate {
        if self.token() == SyntaxKind::AsyncKeyword {
            self.nextToken();
            if self.scanner.hasPrecedingLineBreak() {
                return Tristate::False;
            }
            if self.token() != SyntaxKind::OpenParenToken
                && self.token() != SyntaxKind::LessThanToken
            {
                return Tristate::False;
            }
        }

        let first = self.token();
        let second = self.nextToken();

        if first == SyntaxKind::OpenParenToken {
            if second == SyntaxKind::CloseParenToken {
                // Simple cases: "() =>", "(): ", and "() {".
                // This is an arrow function with no parameters.
                // The last one is not actually an arrow function,
                // but this is probably what the user intended.
                let third = self.nextToken();
                match third {
                    SyntaxKind::EqualsGreaterThanToken
                    | SyntaxKind::ColonToken
                    | SyntaxKind::OpenBraceToken => return Tristate::True,
                    _ => return Tristate::False,
                }
            }

            // If encounter "([" or "({", this could be the start of a binding pattern.
            // Examples:
            //      ([ x ]) => { }
            //      ({ x }) => { }
            //      ([ x ])
            //      ({ x })
            if second == SyntaxKind::OpenBracketToken || second == SyntaxKind::OpenBraceToken {
                return Tristate::Unknown;
            }

            // Simple case: "(..."
            // This is an arrow function with a rest parameter.
            if second == SyntaxKind::DotDotDotToken {
                return Tristate::True;
            }

            // Check for "(xxx yyy", where xxx is a modifier and yyy is an identifier. This
            // isn't actually allowed, but we want to treat it as a lambda so we can provide
            // a good error message.
            if isModifierKind(second)
                && second != SyntaxKind::AsyncKeyword
                && self.lookAhead(|p| p.nextTokenIsIdentifier())
            {
                return Tristate::True;
            }

            // If we had "(" followed by something that's not an identifier,
            // then this definitely doesn't look like a lambda.  "this" is not
            // valid, but we want to parse it and then give a semantic error.
            if !self.isIdentifier() && second != SyntaxKind::ThisKeyword {
                return Tristate::False;
            }

            match self.nextToken() {
                SyntaxKind::ColonToken => {
                    // If we have something like "(a:", then we must have a
                    // type-annotated parameter in an arrow function expression.
                    return Tristate::True;
                }
                SyntaxKind::QuestionToken => {
                    self.nextToken();
                    // If we have "(a?:" or "(a?," or "(a?=" or "(a?)" then it is definitely a lambda.
                    if self.token() == SyntaxKind::ColonToken
                        || self.token() == SyntaxKind::CommaToken
                        || self.token() == SyntaxKind::EqualsToken
                        || self.token() == SyntaxKind::CloseParenToken
                    {
                        return Tristate::True;
                    }
                    // Otherwise it is definitely not a lambda.
                    return Tristate::False;
                }
                SyntaxKind::CommaToken | SyntaxKind::EqualsToken | SyntaxKind::CloseParenToken => {
                    // If we have "(a," or "(a=" or "(a)" this *could* be an arrow function
                    return Tristate::Unknown;
                }
                _ => {}
            }

            return Tristate::False;
        } else {
            debug_assert!(first == SyntaxKind::LessThanToken);

            // If we have "<" not followed by an identifier,
            // then this definitely is not an arrow function.
            if !self.isIdentifier() {
                return Tristate::False;
            }

            // JSX overrides
            if self.languageVariant == LanguageVariant::JSX {
                let isArrowFunctionInJsx = self.lookAhead(|p| {
                    let third = p.nextToken();
                    if third == SyntaxKind::ExtendsKeyword {
                        let fourth = p.nextToken();
                        match fourth {
                            SyntaxKind::EqualsToken | SyntaxKind::GreaterThanToken => return false,
                            _ => return true,
                        }
                    } else if third == SyntaxKind::CommaToken {
                        return true;
                    }
                    false
                });

                if isArrowFunctionInJsx {
                    return Tristate::True;
                }

                return Tristate::False;
            }

            // This *could* be a parenthesized arrow function.
            return Tristate::Unknown;
        }
    }

    fn parsePossibleParenthesizedArrowFunctionExpression(&mut self) -> Option<ArrowFunction> {
        let tokenPos = self.scanner.getTokenPos();
        if self.notParenthesizedArrow.contains(&tokenPos) {
            return None;
        }

        let result = self.parseParenthesizedArrowFunctionExpression(false);
        if !result.is_none() {
            self.notParenthesizedArrow.insert(tokenPos);
        }

        result
    }

    fn tryParseAsyncSimpleArrowFunctionExpression(&mut self) -> Option<ArrowFunction> {
        // We do a check here so that we won't be doing unnecessarily call to "lookAhead"
        if self.token() == SyntaxKind::AsyncKeyword {
            if self.lookAhead(|p| p.isUnParenthesizedAsyncArrowFunctionWorker()) == Tristate::True {
                let pos = self.getNodePos();
                let asyncModifier = self.parseModifiersForArrowFunction();
                let expr = self.parseBinaryExpressionOrHigher(OperatorPrecedence::Lowest);
                let identifier = unwrap_as!(expr, Expression::Identifier(i), i);
                return Some(self.parseSimpleArrowFunctionExpression(
                    pos,
                    identifier,
                    asyncModifier,
                ));
            }
        }
        None
    }

    fn isUnParenthesizedAsyncArrowFunctionWorker(&mut self) -> Tristate {
        // AsyncArrowFunctionExpression:
        //      1) async[no LineTerminator here]AsyncArrowBindingIdentifier[?Yield][no LineTerminator here]=>AsyncConciseBody[?In]
        //      2) CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await][no LineTerminator here]=>AsyncConciseBody[?In]
        if self.token() == SyntaxKind::AsyncKeyword {
            self.nextToken();
            // If the "async" is followed by "=>" token then it is not a beginning of an async arrow-function
            // but instead a simple arrow-function which will be parsed inside "parseAssignmentExpressionOrHigher"
            if self.scanner.hasPrecedingLineBreak()
                || self.token() == SyntaxKind::EqualsGreaterThanToken
            {
                return Tristate::False;
            }
            // Check for un-parenthesized AsyncArrowFunction
            let expr = self.parseBinaryExpressionOrHigher(OperatorPrecedence::Lowest);
            if !self.scanner.hasPrecedingLineBreak()
                && matches!(expr, Expression::Identifier(_))
                && self.token() == SyntaxKind::EqualsGreaterThanToken
            {
                return Tristate::True;
            }
        }

        Tristate::False
    }

    fn parseParenthesizedArrowFunctionExpression(
        &mut self,
        allowAmbiguity: bool,
    ) -> Option<ArrowFunction> {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        let modifiers = self.parseModifiersForArrowFunction();
        let has_async_modifier = modifiers
            .as_ref()
            .map(|m| m.iter().any(|m| isAsyncModifier(m)))
            .unwrap_or_default();
        let isAsync = if has_async_modifier {
            SignatureFlags::Await
        } else {
            SignatureFlags::None
        };
        // Arrow functions are never generators.
        //
        // If we're speculatively parsing a signature for a parenthesized arrow function, then
        // we have to have a complete parameter list.  Otherwise we might see something like
        // a => (b => c)
        // And think that "(b =>" was actually a parenthesized arrow function with a missing
        // close paren.
        let typeParameters = self.parseTypeParameters();

        let parameters: NodeArray<ParameterDeclaration>;
        if !self.parseExpected(SyntaxKind::OpenParenToken, None, None) {
            if !allowAmbiguity {
                return None;
            }
            todo!();
            // parameters = createMissingList<ParameterDeclaration>();
        } else {
            parameters = self.parseParametersWorker(isAsync);
            if !self.parseExpected(SyntaxKind::CloseParenToken, None, None) && !allowAmbiguity {
                return None;
            }
        }

        let ty = self.parseReturnType(SyntaxKind::ColonToken, false);
        if ty.is_some()
            && !allowAmbiguity
            && self.typeHasArrowFunctionBlockingParseError(ty.as_ref().unwrap())
        {
            return None;
        }

        // Parsing a signature isn't enough.
        // Parenthesized arrow signatures often look like other valid expressions.
        // For instance:
        //  - "(x = 10)" is an assignment expression parsed as a signature with a default parameter value.
        //  - "(x,y)" is a comma expression parsed as a signature with two parameters.
        //  - "a ? (b): c" will have "(b):" parsed as a signature with a return type annotation.
        //  - "a ? (b): function() {}" will too, since function() is a valid JSDoc function type.
        //
        // So we need just a bit of lookahead to ensure that it can only be a signature.
        let hasJSDocFunctionType = ty
            .as_ref()
            .map(|_| {
                todo!();
                // isJSDocFunctionType(t)
            })
            .unwrap_or_default();
        if !allowAmbiguity
            && self.token() != SyntaxKind::EqualsGreaterThanToken
            && (hasJSDocFunctionType || self.token() != SyntaxKind::OpenBraceToken)
        {
            // Returning undefined here will cause our caller to rewind to where we started from.
            return None;
        }

        // If we have an arrow, then try to parse the body. Even if not, try to parse if we
        // have an opening brace, just in we're in an error state.
        let lastToken = self.token();
        let equalsGreaterThanToken =
            self.parseExpectedToken::<_, u8>(SyntaxKind::EqualsGreaterThanToken, None, None);
        let body = if lastToken == SyntaxKind::EqualsGreaterThanToken
            || lastToken == SyntaxKind::OpenBraceToken
        {
            self.parseArrowFunctionExpressionBody(has_async_modifier)
        } else {
            ConciseBody::Expression(Expression::Identifier(Rc::new(
                self.parseIdentifier(None, None),
            )))
        };

        let node = self.factory.createArrowFunction(
            modifiers,
            typeParameters,
            parameters,
            ty,
            Some(equalsGreaterThanToken),
            body,
        );
        let node = self.finishNode(node, pos, None);
        Some(self.withJSDoc(node, hasJSDoc))
    }

    fn parseArrowFunctionExpressionBody(&mut self, isAsync: bool) -> ConciseBody {
        if self.token() == SyntaxKind::OpenBraceToken {
            return ConciseBody::FunctionBody(Rc::new(self.parseFunctionBlock(
                if isAsync {
                    SignatureFlags::Await
                } else {
                    SignatureFlags::None
                },
                None,
            )));
        }

        if self.token() != SyntaxKind::SemicolonToken
            && self.token() != SyntaxKind::FunctionKeyword
            && self.token() != SyntaxKind::ClassKeyword
            && self.isStartOfStatement()
            && !self.isStartOfExpressionStatement()
        {
            // Check if we got a plain statement (i.e. no expression-statements, no function/class expressions/declarations)
            //
            // Here we try to recover from a potential error situation in the where the
            // user meant to supply a block. For example, if the user wrote:
            //
            //  a =>
            //      let v = 0;
            //  }
            //
            // they may be missing an open brace.  Check to see if that's the so we can
            // try to recover better.  If we don't do this, then the next close curly we see may end
            // up preemptively closing the containing construct.
            //
            // Note: even when 'IgnoreMissingOpenBrace' is passed, parseBody will still error.
            return ConciseBody::FunctionBody(Rc::new(self.parseFunctionBlock(
                SignatureFlags::IgnoreMissingOpenBrace
                    | (if isAsync {
                        SignatureFlags::Await
                    } else {
                        SignatureFlags::None
                    }),
                None,
            )));
        }

        let savedTopLevel = self.topLevel;
        self.topLevel = false;
        let node = if isAsync {
            self.doInAwaitContext(|p| p.parseAssignmentExpressionOrHigher())
        } else {
            self.doOutsideOfAwaitContext(|p| p.parseAssignmentExpressionOrHigher())
        };
        self.topLevel = savedTopLevel;
        ConciseBody::Expression(node)
    }

    fn parseConditionalExpressionRest(
        &mut self,
        leftOperand: Expression,
        pos: usize,
    ) -> Expression {
        // Note: we are passed in an expression which was produced from parseBinaryExpressionOrHigher.
        let questionToken =
            match self.parseOptionalToken::<QuestionToken>(SyntaxKind::QuestionToken) {
                Some(t) => t,
                None => return leftOperand,
            };

        // Note: we explicitly 'allowIn' in the whenTrue part of the condition expression, and
        // we do not that for the 'whenFalse' part.
        let whenTrue = self.doOutsideOfContext(disallowInAndDecoratorContext, |p| {
            p.parseAssignmentExpressionOrHigher()
        });
        let colonToken = self.parseExpectedToken::<_, u8>(SyntaxKind::ColonToken, None, None);
        let whenFalse = if nodeIsPresent(Some(&colonToken)) {
            self.parseAssignmentExpressionOrHigher()
        } else {
            todo!();
            // createMissingNode(
            //     SyntaxKind::Identifier,
            //     /*reportAtCurrentPosition*/ false,
            //     Diagnostics::_0_expected,
            //     tokenToString(SyntaxKind::ColonToken),
            // )
        };
        let node = self.factory.createConditionalExpression(
            leftOperand,
            Some(questionToken),
            whenTrue,
            Some(colonToken),
            whenFalse,
        );
        Expression::ConditionalExpression(Rc::new(self.finishNode(node, pos, None)))
    }

    fn parseBinaryExpressionOrHigher(&mut self, precedence: OperatorPrecedence) -> Expression {
        let pos = self.getNodePos();
        let leftOperand = self.parseUnaryExpressionOrHigher();
        self.parseBinaryExpressionRest(precedence, leftOperand, pos)
    }

    fn isInOrOfKeyword(&self, t: SyntaxKind) -> bool {
        t == SyntaxKind::InKeyword || t == SyntaxKind::OfKeyword
    }

    fn parseBinaryExpressionRest(
        &mut self,
        precedence: OperatorPrecedence,
        mut leftOperand: Expression,
        pos: usize,
    ) -> Expression {
        loop {
            // We either have a binary operator here, or we're finished.  We call
            // reScanGreaterToken so that we merge token sequences like > and = into >=

            self.reScanGreaterToken();
            let newPrecedence = getBinaryOperatorPrecedence(self.token());

            // Check the precedence to see if we should "take" this operator
            // - For left associative operator (all operator but **), consume the operator,
            //   recursively call the function below, and parse binaryExpression as a rightOperand
            //   of the caller if the new precedence of the operator is greater then or equal to the current precedence.
            //   For example:
            //      a - b - c;
            //            ^token; leftOperand = b. Return b to the caller as a rightOperand
            //      a * b - c
            //            ^token; leftOperand = b. Return b to the caller as a rightOperand
            //      a - b * c;
            //            ^token; leftOperand = b. Return b * c to the caller as a rightOperand
            // - For right associative operator (**), consume the operator, recursively call the function
            //   and parse binaryExpression as a rightOperand of the caller if the new precedence of
            //   the operator is strictly grater than the current precedence
            //   For example:
            //      a ** b ** c;
            //             ^^token; leftOperand = b. Return b ** c to the caller as a rightOperand
            //      a - b ** c;
            //            ^^token; leftOperand = b. Return b ** c to the caller as a rightOperand
            //      a ** b - c
            //             ^token; leftOperand = b. Return b to the caller as a rightOperand
            let consumeCurrentOperator = if self.token() == SyntaxKind::AsteriskAsteriskToken {
                newPrecedence >= precedence
            } else {
                newPrecedence > precedence
            };

            if !consumeCurrentOperator {
                break;
            }

            if self.token() == SyntaxKind::InKeyword && self.inDisallowInContext() {
                break;
            }

            if self.token() == SyntaxKind::AsKeyword {
                // Make sure we *do* perform ASI for constructs like this:
                //    var x = foo
                //    as (Bar)
                // This should be parsed as an initialized variable, followed
                // by a function call to 'as' with the argument 'Bar'
                if self.scanner.hasPrecedingLineBreak() {
                    break;
                } else {
                    self.nextToken();
                    let right = self.parseType();
                    leftOperand = Expression::AsExpression(Rc::new(
                        self.makeAsExpression(leftOperand, right),
                    ));
                }
            } else {
                let operatorToken = self.parseTokenNode();
                let right = self.parseBinaryExpressionOrHigher(newPrecedence);
                leftOperand = Expression::BinaryExpression(Rc::new(self.makeBinaryExpression(
                    leftOperand,
                    operatorToken,
                    right,
                    pos,
                )));
            }
        }

        leftOperand
    }

    fn isBinaryOperator(&self) -> bool {
        if self.inDisallowInContext() && self.token() == SyntaxKind::InKeyword {
            return false;
        }

        getBinaryOperatorPrecedence(self.token()) > OperatorPrecedence::Comma
    }

    fn makeBinaryExpression(
        &mut self,
        left: Expression,
        operatorToken: BinaryOperatorToken,
        right: Expression,
        pos: usize,
    ) -> BinaryExpression {
        let node = self
            .factory
            .createBinaryExpression(left, operatorToken, right);
        self.finishNode(node, pos, None)
    }

    fn makeAsExpression(&mut self, left: Expression, right: TypeNode) -> AsExpression {
        let pos = self.factory.node_data(&left).get_range().pos;
        let node = self.factory.createAsExpression(left, right);
        self.finishNode(node, pos, None)
    }

    fn parsePrefixUnaryExpression(&mut self) -> PrefixUnaryExpression {
        let pos = self.getNodePos();
        let operator = self.token();
        let expression = self.nextTokenAnd(|p| p.parseSimpleUnaryExpression());
        let node = self
            .factory
            .createPrefixUnaryExpression(operator, expression);
        self.finishNode(node, pos, None)
    }

    fn parseDeleteExpression(&mut self) -> DeleteExpression {
        let pos = self.getNodePos();
        let expression = self.nextTokenAnd(|p| p.parseSimpleUnaryExpression());
        let node = self.factory.createDeleteExpression(expression);
        self.finishNode(node, pos, None)
    }

    fn parseTypeOfExpression(&mut self) -> TypeOfExpression {
        let pos = self.getNodePos();
        let expression = self.nextTokenAnd(|p| p.parseSimpleUnaryExpression());
        let node = self.factory.createTypeOfExpression(expression);
        self.finishNode(node, pos, None)
    }

    fn parseVoidExpression(&mut self) -> VoidExpression {
        let pos = self.getNodePos();
        let expression = self.nextTokenAnd(|p| p.parseSimpleUnaryExpression());
        let node = self.factory.createVoidExpression(expression);
        self.finishNode(node, pos, None)
    }

    fn isAwaitExpression(&mut self) -> bool {
        if self.token() == SyntaxKind::AwaitKeyword {
            if self.inAwaitContext() {
                return true;
            }

            // here we are using similar heuristics as 'isYieldExpression'
            return self.lookAhead(|p| p.nextTokenIsIdentifierOrKeywordOrLiteralOnSameLine());
        }

        false
    }

    fn parseAwaitExpression(&mut self) -> AwaitExpression {
        let pos = self.getNodePos();
        let expression = self.nextTokenAnd(|p| p.parseSimpleUnaryExpression());
        let node = self.factory.createAwaitExpression(expression);
        self.finishNode(node, pos, None)
    }

    /**
     * Parse ES7 exponential expression and await expression
     *
     * ES7 ExponentiationExpression:
     *      1) UnaryExpression[?Yield]
     *      2) UpdateExpression[?Yield] ** ExponentiationExpression[?Yield]
     *
     */
    fn parseUnaryExpressionOrHigher(&mut self) -> Expression {
        /*
         * ES7 UpdateExpression:
         *      1) LeftHandSideExpression[?Yield]
         *      2) LeftHandSideExpression[?Yield][no LineTerminator here]++
         *      3) LeftHandSideExpression[?Yield][no LineTerminator here]--
         *      4) ++UnaryExpression[?Yield]
         *      5) --UnaryExpression[?Yield]
         */
        if self.isUpdateExpression() {
            let pos = self.getNodePos();
            let updateExpression = self.parseUpdateExpression();
            return if self.token() == SyntaxKind::AsteriskAsteriskToken {
                self.parseBinaryExpressionRest(
                    getBinaryOperatorPrecedence(self.token()),
                    updateExpression.into(),
                    pos,
                )
            } else {
                updateExpression.into()
            };
        }

        /*
         * ES7 UnaryExpression:
         *      1) UpdateExpression[?yield]
         *      2) delete UpdateExpression[?yield]
         *      3) void UpdateExpression[?yield]
         *      4) typeof UpdateExpression[?yield]
         *      5) + UpdateExpression[?yield]
         *      6) - UpdateExpression[?yield]
         *      7) ~ UpdateExpression[?yield]
         *      8) ! UpdateExpression[?yield]
         */
        let unaryOperator = self.token();
        let simpleUnaryExpression = self.parseSimpleUnaryExpression();
        if self.token() == SyntaxKind::AsteriskAsteriskToken {
            let range = self.factory.node_data(&simpleUnaryExpression).get_range();
            let pos = skipTrivia(self.sourceText.as_ref(), range.pos, false, false, false);
            let end = range.end;
            if matches!(simpleUnaryExpression, UnaryExpression::TypeAssertion(_)) {
                self.parseErrorAt::<u8>(pos, end, Diagnostics::A_type_assertion_expression_is_not_allowed_in_the_left_hand_side_of_an_exponentiation_expression_Consider_enclosing_the_expression_in_parentheses,None);
            } else {
                self.parseErrorAt(pos, end, Diagnostics::An_unary_expression_with_the_0_operator_is_not_allowed_in_the_left_hand_side_of_an_exponentiation_expression_Consider_enclosing_the_expression_in_parentheses, tokenToString(unaryOperator));
            }
        }
        simpleUnaryExpression.into()
    }

    /**
     * Parse ES7 simple-unary expression or higher:
     *
     * ES7 UnaryExpression:
     *      1) UpdateExpression[?yield]
     *      2) delete UnaryExpression[?yield]
     *      3) void UnaryExpression[?yield]
     *      4) typeof UnaryExpression[?yield]
     *      5) + UnaryExpression[?yield]
     *      6) - UnaryExpression[?yield]
     *      7) ~ UnaryExpression[?yield]
     *      8) ! UnaryExpression[?yield]
     *      9) [+Await] await UnaryExpression[?yield]
     */
    fn parseSimpleUnaryExpression(&mut self) -> UnaryExpression {
        match self.token() {
            SyntaxKind::PlusToken
            | SyntaxKind::MinusToken
            | SyntaxKind::TildeToken
            | SyntaxKind::ExclamationToken => {
                UnaryExpression::PrefixUnaryExpression(Rc::new(self.parsePrefixUnaryExpression()))
            }
            SyntaxKind::DeleteKeyword => {
                UnaryExpression::DeleteExpression(Rc::new(self.parseDeleteExpression()))
            }
            SyntaxKind::TypeOfKeyword => {
                UnaryExpression::TypeOfExpression(Rc::new(self.parseTypeOfExpression()))
            }
            SyntaxKind::VoidKeyword => {
                UnaryExpression::VoidExpression(Rc::new(self.parseVoidExpression()))
            }
            SyntaxKind::LessThanToken => {
                // This is modified UnaryExpression grammar in TypeScript
                //  UnaryExpression (modified):
                //      < type > UnaryExpression
                UnaryExpression::TypeAssertion(Rc::new(self.parseTypeAssertion()))
            }
            SyntaxKind::AwaitKeyword => {
                if self.isAwaitExpression() {
                    return UnaryExpression::AwaitExpression(Rc::new(self.parseAwaitExpression()));
                }
                self.parseUpdateExpression().into()
            }
            _ => self.parseUpdateExpression().into(),
        }
    }

    /**
     * Check if the current token can possibly be an ES7 increment expression.
     *
     * ES7 UpdateExpression:
     *      LeftHandSideExpression[?Yield]
     *      LeftHandSideExpression[?Yield][no LineTerminator here]++
     *      LeftHandSideExpression[?Yield][no LineTerminator here]--
     *      ++LeftHandSideExpression[?Yield]
     *      --LeftHandSideExpression[?Yield]
     */
    fn isUpdateExpression(&self) -> bool {
        // This function is called inside parseUnaryExpression to decide
        // whether to call parseSimpleUnaryExpression or call parseUpdateExpression directly
        match self.token() {
            SyntaxKind::PlusToken
            | SyntaxKind::MinusToken
            | SyntaxKind::TildeToken
            | SyntaxKind::ExclamationToken
            | SyntaxKind::DeleteKeyword
            | SyntaxKind::TypeOfKeyword
            | SyntaxKind::VoidKeyword
            | SyntaxKind::AwaitKeyword => false,
            SyntaxKind::LessThanToken => {
                // If we are not in JSX context, we are parsing TypeAssertion which is an UnaryExpression
                if self.languageVariant != LanguageVariant::JSX {
                    return false;
                }
                // We are in JSX context and the token is part of JSXElement.
                true
            }
            _ => true,
        }
    }

    /**
     * Parse ES7 UpdateExpression. UpdateExpression is used instead of ES6's PostFixExpression.
     *
     * ES7 UpdateExpression[yield]:
     *      1) LeftHandSideExpression[?yield]
     *      2) LeftHandSideExpression[?yield] [[no LineTerminator here]]++
     *      3) LeftHandSideExpression[?yield] [[no LineTerminator here]]--
     *      4) ++LeftHandSideExpression[?yield]
     *      5) --LeftHandSideExpression[?yield]
     * In TypeScript (2), (3) are parsed as PostfixUnaryExpression. (4), (5) are parsed as PrefixUnaryExpression
     */
    fn parseUpdateExpression(&mut self) -> UpdateExpression {
        if self.token() == SyntaxKind::PlusPlusToken || self.token() == SyntaxKind::MinusMinusToken
        {
            let pos = self.getNodePos();
            let operator = self.token();
            let expression = self.nextTokenAnd(|p| p.parseLeftHandSideExpressionOrHigher());
            let node = self
                .factory
                .createPrefixUnaryExpression(operator, UpdateExpression::from(expression).into());
            return UpdateExpression::PrefixUnaryExpression(Rc::new(
                self.finishNode(node, pos, None),
            ));
        } else if self.languageVariant == LanguageVariant::JSX
            && self.token() == SyntaxKind::LessThanToken
            && self.lookAhead(|p| p.nextTokenIsIdentifierOrKeywordOrGreaterThan())
        {
            todo!();
            // JSXElement is part of primaryExpression
            // return parseJsxElementOrSelfClosingElementOrFragment(/*inExpressionContext*/ true);
        }

        let expression = self.parseLeftHandSideExpressionOrHigher();

        debug_assert!(isLeftHandSideExpression(expression.clone().into()));
        if (self.token() == SyntaxKind::PlusPlusToken
            || self.token() == SyntaxKind::MinusMinusToken)
            && !self.scanner.hasPrecedingLineBreak()
        {
            let operator = self.token();
            self.nextToken();
            let pos = self.factory.node_data(&expression).get_range().pos;
            let node = self
                .factory
                .createPostfixUnaryExpression(expression, operator);
            return UpdateExpression::PostfixUnaryExpression(Rc::new(
                self.finishNode(node, pos, None),
            ));
        }

        expression.into()
    }

    fn parseLeftHandSideExpressionOrHigher(&mut self) -> LeftHandSideExpression {
        // Original Ecma:
        // LeftHandSideExpression: See 11.2
        //      NewExpression
        //      CallExpression
        //
        // Our simplification:
        //
        // LeftHandSideExpression: See 11.2
        //      MemberExpression
        //      CallExpression
        //
        // See comment in parseMemberExpressionOrHigher on how we replaced NewExpression with
        // MemberExpression to make our lives easier.
        //
        // to best understand the below code, it's important to see how CallExpression expands
        // out into its own productions:
        //
        // CallExpression:
        //      MemberExpression Arguments
        //      CallExpression Arguments
        //      CallExpression[Expression]
        //      CallExpression.IdentifierName
        //      import (AssignmentExpression)
        //      super Arguments
        //      super.IdentifierName
        //
        // Because of the recursion in these calls, we need to bottom out first. There are three
        // bottom out states we can run into: 1) We see 'super' which must start either of
        // the last two CallExpression productions. 2) We see 'import' which must start import call.
        // 3)we have a MemberExpression which either completes the LeftHandSideExpression,
        // or starts the beginning of the first four CallExpression productions.
        let pos = self.getNodePos();
        let expression = if self.token() == SyntaxKind::ImportKeyword {
            if self.lookAhead(|p| p.nextTokenIsOpenParenOrLessThan()) {
                // We don't want to eagerly consume all import keyword as import call expression so we look ahead to find "("
                // For example:
                //      var foo3 = require("subfolder
                //      import * as foo1 from "module-from-node
                // We want this import to be a statement rather than import call expression
                self.sourceFlags |= NodeFlags::PossiblyContainsDynamicImport;
                // self.parseTokenNode::<PrimaryExpression>()
                todo!();
            } else if self.lookAhead(|p| p.nextTokenIsDot()) {
                // This is an 'import.*' metaproperty (i.e. 'import.meta')
                self.nextToken(); // advance past the 'import'
                self.nextToken(); // advance past the dot
                let name = self.parseIdentifierName(None);
                let expression = self
                    .factory
                    .createMetaProperty(SyntaxKind::ImportKeyword, Rc::new(name));
                let expression = self.finishNode(expression, pos, None);
                self.sourceFlags |= NodeFlags::PossiblyContainsImportMeta;
                LeftHandSideExpression::MetaProperty(Rc::new(expression))
            } else {
                self.parseMemberExpressionOrHigher()
            }
        } else if self.token() == SyntaxKind::SuperKeyword {
            self.parseSuperExpression().into()
        } else {
            self.parseMemberExpressionOrHigher()
        };

        // Now, we *may* be complete.  However, we might have consumed the start of a
        // CallExpression or OptionalExpression.  As such, we need to consume the rest
        // of it here to be complete.
        self.parseCallExpressionRest(pos, expression.into())
    }

    fn parseMemberExpressionOrHigher(&mut self) -> LeftHandSideExpression {
        // Note: to make our lives simpler, we decompose the NewExpression productions and
        // place ObjectCreationExpression and FunctionExpression into PrimaryExpression.
        // like so:
        //
        //   PrimaryExpression : See 11.1
        //      this
        //      Identifier
        //      Literal
        //      ArrayLiteral
        //      ObjectLiteral
        //      (Expression)
        //      FunctionExpression
        //      new MemberExpression Arguments?
        //
        //   MemberExpression : See 11.2
        //      PrimaryExpression
        //      MemberExpression[Expression]
        //      MemberExpression.IdentifierName
        //
        //   CallExpression : See 11.2
        //      MemberExpression
        //      CallExpression Arguments
        //      CallExpression[Expression]
        //      CallExpression.IdentifierName
        //
        // Technically this is ambiguous.  i.e. CallExpression defines:
        //
        //   CallExpression:
        //      CallExpression Arguments
        //
        // If you see: "new Foo()"
        //
        // Then that could be treated as a single ObjectCreationExpression, or it could be
        // treated as the invocation of "new Foo".  We disambiguate that in code (to match
        // the original grammar) by making sure that if we see an ObjectCreationExpression
        // we always consume arguments if they are there. So we treat "new Foo()" as an
        // object creation only, and not at all as an invocation.  Another way to think
        // about this is that for every "new" that we see, we will consume an argument list if
        // it is there as part of the *associated* object creation node.  Any additional
        // argument lists we see, will become invocation expressions.
        //
        // Because there are no other places in the grammar now that refer to FunctionExpression
        // or ObjectCreationExpression, it is safe to push down into the PrimaryExpression
        // production.
        //
        // Because CallExpression and MemberExpression are left recursive, we need to bottom out
        // of the recursion immediately.  So we parse out a primary expression to start with.
        let pos = self.getNodePos();
        let expression = self.parsePrimaryExpression();
        self.parseMemberExpressionRest(pos, MemberExpression::from(expression).into(), true)
    }

    fn parseSuperExpression(&mut self) -> MemberExpression {
        let pos = self.getNodePos();
        let expression = Rc::new(self.parseTokenNode::<SuperExpression>());
        if self.token() == SyntaxKind::LessThanToken {
            let startPos = self.getNodePos();
            let typeArguments = self.tryParse(|p| p.parseTypeArgumentsInExpression());
            if typeArguments.is_some() {
                self.parseErrorAt::<u8>(
                    startPos,
                    self.getNodePos(),
                    Diagnostics::super_may_not_use_type_arguments,
                    None,
                );
            }
        }

        if self.token() == SyntaxKind::OpenParenToken
            || self.token() == SyntaxKind::DotToken
            || self.token() == SyntaxKind::OpenBracketToken
        {
            return MemberExpression::SuperExpression(expression);
        }

        // If we have seen "super" it must be followed by '(' or '.'.
        // If it wasn't then just try to parse out a '.' and report an error.
        self.parseExpectedToken::<DotToken, u8>(
            SyntaxKind::DotToken,
            Some(Diagnostics::super_must_be_followed_by_an_argument_list_or_member_access),
            None,
        );
        // private names will never work with `super` (`super.#foo`), but that's a semantic error, not syntactic
        let name = self.parseRightSideOfDot(true, true);
        let node = self.factory.createPropertyAccessExpression(
            LeftHandSideExpression::SuperExpression(expression),
            name,
        );
        MemberExpression::PropertyAccessExpression(Rc::new(self.finishNode(node, pos, None)))
    }

    //     function parseJsxElementOrSelfClosingElementOrFragment(inExpressionContext: boolean, topInvalidNodePosition?: number, openingTag?: JsxOpeningElement | JsxOpeningFragment): JsxElement | JsxSelfClosingElement | JsxFragment {
    //         const pos = getNodePos();
    //         const opening = parseJsxOpeningOrSelfClosingElementOrOpeningFragment(inExpressionContext);
    //         let result: JsxElement | JsxSelfClosingElement | JsxFragment;
    //         if (opening.kind === SyntaxKind::JsxOpeningElement) {
    //             let children = parseJsxChildren(opening);
    //             let closingElement: JsxClosingElement;

    //             const lastChild: JsxChild | undefined = children[children.length - 1];
    //             if (lastChild?.kind === SyntaxKind::JsxElement
    //                 && !tagNamesAreEquivalent(lastChild.openingElement.tagName, lastChild.closingElement.tagName)
    //                 && tagNamesAreEquivalent(opening.tagName, lastChild.closingElement.tagName)) {
    //                 // when an unclosed JsxOpeningElement incorrectly parses its parent's JsxClosingElement,
    //                 // restructure (<div>(...<span>...</div>)) --> (<div>(...<span>...</>)</div>)
    //                 // (no need to error; the parent will error)
    //                 const end = lastChild.children.end;
    //                 const newLast = finishNode(factory.createJsxElement(
    //                     lastChild.openingElement,
    //                     lastChild.children,
    //                     finishNode(factory.createJsxClosingElement(finishNode(factory.createIdentifier(""), end, end)), end, end)),
    //                 lastChild.openingElement.pos,
    //                 end);

    //                 children = createNodeArray([...children.slice(0, children.length - 1), newLast], children.pos, end);
    //                 closingElement = lastChild.closingElement;
    //             }
    //             else {
    //                 closingElement = parseJsxClosingElement(opening, inExpressionContext);
    //                 if (!tagNamesAreEquivalent(opening.tagName, closingElement.tagName)) {
    //                     if (openingTag && isJsxOpeningElement(openingTag) && tagNamesAreEquivalent(closingElement.tagName, openingTag.tagName)) {
    //                         // opening incorrectly matched with its parent's closing -- put error on opening
    //                         parseErrorAtRange(opening.tagName, Diagnostics::JSX_element_0_has_no_corresponding_closing_tag, getTextOfNodeFromSourceText(sourceText, opening.tagName));
    //                     }
    //                     else {
    //                         // other opening/closing mismatches -- put error on closing
    //                         parseErrorAtRange(closingElement.tagName, Diagnostics::Expected_corresponding_JSX_closing_tag_for_0, getTextOfNodeFromSourceText(sourceText, opening.tagName));
    //                     }
    //                 }
    //             }
    //             result = finishNode(factory.createJsxElement(opening, children, closingElement), pos);
    //         }
    //         else if (opening.kind === SyntaxKind::JsxOpeningFragment) {
    //             result = finishNode(factory.createJsxFragment(opening, parseJsxChildren(opening), parseJsxClosingFragment(inExpressionContext)), pos);
    //         }
    //         else {
    //             Debug.assert(opening.kind === SyntaxKind::JsxSelfClosingElement);
    //             // Nothing else to do for self-closing elements
    //             result = opening;
    //         }

    //         // If the user writes the invalid code '<div></div><div></div>' in an expression context (i.e. not wrapped in
    //         // an enclosing tag), we'll naively try to parse   ^ this as a 'less than' operator and the remainder of the tag
    //         // as garbage, which will cause the formatter to badly mangle the JSX. Perform a speculative parse of a JSX
    //         // element if we see a < token so that we can wrap it in a synthetic binary expression so the formatter
    //         // does less damage and we can report a better error.
    //         // Since JSX elements are invalid < operands anyway, this lookahead parse will only occur in error scenarios
    //         // of one sort or another.
    //         if (inExpressionContext && token() === SyntaxKind::LessThanToken) {
    //             const topBadPos = typeof topInvalidNodePosition === "undefined" ? result.pos : topInvalidNodePosition;
    //             const invalidElement = tryParse(() => parseJsxElementOrSelfClosingElementOrFragment(/*inExpressionContext*/ true, topBadPos));
    //             if (invalidElement) {
    //                 const operatorToken = createMissingNode(SyntaxKind::CommaToken, /*reportAtCurrentPosition*/ false);
    //                 setTextRangePosWidth(operatorToken, invalidElement.pos, 0);
    //                 parseErrorAt(skipTrivia(sourceText, topBadPos), invalidElement.end, Diagnostics::JSX_expressions_must_have_one_parent_element);
    //                 return finishNode(factory.createBinaryExpression(result, operatorToken as Token<SyntaxKind::CommaToken>, invalidElement), pos) as Node as JsxElement;
    //             }
    //         }

    //         return result;
    //     }

    //     function parseJsxText(): JsxText {
    //         const pos = getNodePos();
    //         const node = factory.createJsxText(scanner.getTokenValue(), currentToken === SyntaxKind::JsxTextAllWhiteSpaces);
    //         currentToken = scanner.scanJsxToken();
    //         return finishNode(node, pos);
    //     }

    //     function parseJsxChild(openingTag: JsxOpeningElement | JsxOpeningFragment, token: JsxTokenSyntaxKind): JsxChild | undefined {
    //         switch (token) {
    //             SyntaxKind::EndOfFileToken:
    //                 // If we hit EOF, issue the error at the tag that lacks the closing element
    //                 // rather than at the end of the file (which is useless)
    //                 if (isJsxOpeningFragment(openingTag)) {
    //                     parseErrorAtRange(openingTag, Diagnostics::JSX_fragment_has_no_corresponding_closing_tag);
    //                 }
    //                 else {
    //                     // We want the error span to cover only 'Foo.Bar' in < Foo.Bar >
    //                     // or to cover only 'Foo' in < Foo >
    //                     const tag = openingTag.tagName;
    //                     const start = skipTrivia(sourceText, tag.pos);
    //                     parseErrorAt(start, tag.end, Diagnostics::JSX_element_0_has_no_corresponding_closing_tag, getTextOfNodeFromSourceText(sourceText, openingTag.tagName));
    //                 }
    //                 return undefined;
    //             SyntaxKind::LessThanSlashToken:
    //             SyntaxKind::ConflictMarkerTrivia:
    //                 return undefined;
    //             SyntaxKind::JsxText:
    //             SyntaxKind::JsxTextAllWhiteSpaces:
    //                 return parseJsxText();
    //             SyntaxKind::OpenBraceToken:
    //                 return parseJsxExpression(/*inExpressionContext*/ false);
    //             SyntaxKind::LessThanToken:
    //                 return parseJsxElementOrSelfClosingElementOrFragment(/*inExpressionContext*/ false, /*topInvalidNodePosition*/ undefined, openingTag);
    //             default:
    //                 return Debug.assertNever(token);
    //         }
    //     }

    //     function parseJsxChildren(openingTag: JsxOpeningElement | JsxOpeningFragment): NodeArray<JsxChild> {
    //         const list = [];
    //         const listPos = getNodePos();
    //         const saveParsingContext = parsingContext;
    //         parsingContext |= 1 << ParsingContext::JsxChildren;

    //         while (true) {
    //             const child = parseJsxChild(openingTag, currentToken = scanner.reScanJsxToken());
    //             if (!child) break;
    //             list.push(child);
    //             if (isJsxOpeningElement(openingTag)
    //                 && child?.kind === SyntaxKind::JsxElement
    //                 && !tagNamesAreEquivalent(child.openingElement.tagName, child.closingElement.tagName)
    //                 && tagNamesAreEquivalent(openingTag.tagName, child.closingElement.tagName)) {
    //                 // stop after parsing a mismatched child like <div>...(<span></div>) in order to reattach the </div> higher
    //                 break;
    //             }
    //         }

    //         parsingContext = saveParsingContext;
    //         return createNodeArray(list, listPos);
    //     }

    //     function parseJsxAttributes(): JsxAttributes {
    //         const pos = getNodePos();
    //         return finishNode(factory.createJsxAttributes(parseList(ParsingContext::JsxAttributes, parseJsxAttribute)), pos);
    //     }

    //     function parseJsxOpeningOrSelfClosingElementOrOpeningFragment(inExpressionContext: boolean): JsxOpeningElement | JsxSelfClosingElement | JsxOpeningFragment {
    //         const pos = getNodePos();

    //         parseExpected(SyntaxKind::LessThanToken);

    //         if (token() === SyntaxKind::GreaterThanToken) {
    //             // See below for explanation of scanJsxText
    //             scanJsxText();
    //             return finishNode(factory.createJsxOpeningFragment(), pos);
    //         }
    //         const tagName = parseJsxElementName();
    //         const typeArguments = (contextFlags & NodeFlags.JavaScriptFile) === 0 ? tryParseTypeArguments() : undefined;
    //         const attributes = parseJsxAttributes();

    //         let node: JsxOpeningLikeElement;

    //         if (token() === SyntaxKind::GreaterThanToken) {
    //             // Closing tag, so scan the immediately-following text with the JSX scanning instead
    //             // of regular scanning to avoid treating illegal characters (e.g. '#') as immediate
    //             // scanning errors
    //             scanJsxText();
    //             node = factory.createJsxOpeningElement(tagName, typeArguments, attributes);
    //         }
    //         else {
    //             parseExpected(SyntaxKind::SlashToken);
    //             if (parseExpected(SyntaxKind::GreaterThanToken, /*diagnostic*/ undefined, /*shouldAdvance*/ false)) {
    //                 // manually advance the scanner in order to look for jsx text inside jsx
    //                 if (inExpressionContext) {
    //                     nextToken();
    //                 }
    //                 else {
    //                     scanJsxText();
    //                 }
    //             }
    //             node = factory.createJsxSelfClosingElement(tagName, typeArguments, attributes);
    //         }

    //         return finishNode(node, pos);
    //     }

    //     function parseJsxElementName(): JsxTagNameExpression {
    //         const pos = getNodePos();
    //         scanJsxIdentifier();
    //         // JsxElement can have name in the form of
    //         //      propertyAccessExpression
    //         //      primaryExpression in the form of an identifier and "this" keyword
    //         // We can't just simply use parseLeftHandSideExpressionOrHigher because then we will start consider class,function etc as a keyword
    //         // We only want to consider "this" as a primaryExpression
    //         let expression: JsxTagNameExpression = token() === SyntaxKind::ThisKeyword ?
    //             parseTokenNode<ThisExpression>() : parseIdentifierName();
    //         while (parseOptional(SyntaxKind::DotToken)) {
    //             expression = finishNode(factory.createPropertyAccessExpression(expression, parseRightSideOfDot(/*allowIdentifierNames*/ true, /*allowPrivateIdentifiers*/ false)), pos) as JsxTagNamePropertyAccess;
    //         }
    //         return expression;
    //     }

    //     function parseJsxExpression(inExpressionContext: boolean): JsxExpression | undefined {
    //         const pos = getNodePos();
    //         if (!parseExpected(SyntaxKind::OpenBraceToken)) {
    //             return undefined;
    //         }

    //         let dotDotDotToken: DotDotDotToken | undefined;
    //         let expression: Expression | undefined;
    //         if (token() !== SyntaxKind::CloseBraceToken) {
    //             dotDotDotToken = parseOptionalToken(SyntaxKind::DotDotDotToken);
    //             // Only an AssignmentExpression is valid here per the JSX spec,
    //             // but we can unambiguously parse a comma sequence and provide
    //             // a better error message in grammar checking.
    //             expression = parseExpression();
    //         }
    //         if (inExpressionContext) {
    //             parseExpected(SyntaxKind::CloseBraceToken);
    //         }
    //         else {
    //             if (parseExpected(SyntaxKind::CloseBraceToken, /*message*/ undefined, /*shouldAdvance*/ false)) {
    //                 scanJsxText();
    //             }
    //         }

    //         return finishNode(factory.createJsxExpression(dotDotDotToken, expression), pos);
    //     }

    //     function parseJsxAttribute(): JsxAttribute | JsxSpreadAttribute {
    //         if (token() === SyntaxKind::OpenBraceToken) {
    //             return parseJsxSpreadAttribute();
    //         }

    //         scanJsxIdentifier();
    //         const pos = getNodePos();
    //         return finishNode(
    //             factory.createJsxAttribute(
    //                 parseIdentifierName(),
    //                 token() !== SyntaxKind::EqualsToken ? undefined :
    //                 scanJsxAttributeValue() === SyntaxKind::StringLiteral ? parseLiteralNode() as StringLiteral :
    //                 parseJsxExpression(/*inExpressionContext*/ true)
    //             ),
    //             pos
    //         );
    //     }

    //     function parseJsxSpreadAttribute(): JsxSpreadAttribute {
    //         const pos = getNodePos();
    //         parseExpected(SyntaxKind::OpenBraceToken);
    //         parseExpected(SyntaxKind::DotDotDotToken);
    //         const expression = parseExpression();
    //         parseExpected(SyntaxKind::CloseBraceToken);
    //         return finishNode(factory.createJsxSpreadAttribute(expression), pos);
    //     }

    //     function parseJsxClosingElement(open: JsxOpeningElement, inExpressionContext: boolean): JsxClosingElement {
    //         const pos = getNodePos();
    //         parseExpected(SyntaxKind::LessThanSlashToken);
    //         const tagName = parseJsxElementName();
    //         if (parseExpected(SyntaxKind::GreaterThanToken, /*diagnostic*/ undefined, /*shouldAdvance*/ false)) {
    //             // manually advance the scanner in order to look for jsx text inside jsx
    //             if (inExpressionContext || !tagNamesAreEquivalent(open.tagName, tagName)) {
    //                 nextToken();
    //             }
    //             else {
    //                 scanJsxText();
    //             }
    //         }
    //         return finishNode(factory.createJsxClosingElement(tagName), pos);
    //     }

    //     function parseJsxClosingFragment(inExpressionContext: boolean): JsxClosingFragment {
    //         const pos = getNodePos();
    //         parseExpected(SyntaxKind::LessThanSlashToken);
    //         if (tokenIsIdentifierOrKeyword(token())) {
    //             parseErrorAtRange(parseJsxElementName(), Diagnostics::Expected_corresponding_closing_tag_for_JSX_fragment);
    //         }
    //         if (parseExpected(SyntaxKind::GreaterThanToken, /*diagnostic*/ undefined, /*shouldAdvance*/ false)) {
    //             // manually advance the scanner in order to look for jsx text inside jsx
    //             if (inExpressionContext) {
    //                 nextToken();
    //             }
    //             else {
    //                 scanJsxText();
    //             }
    //         }
    //         return finishNode(factory.createJsxJsxClosingFragment(), pos);
    //     }

    fn parseTypeAssertion(&mut self) -> TypeAssertion {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::LessThanToken, None, None);
        let ty = self.parseType();
        self.parseExpected(SyntaxKind::GreaterThanToken, None, None);
        let expression = self.parseSimpleUnaryExpression();
        let node = self.factory.createTypeAssertion(ty, expression);
        self.finishNode(node, pos, None)
    }

    fn nextTokenIsIdentifierOrKeywordOrOpenBracketOrTemplate(&mut self) -> bool {
        self.nextToken();
        tokenIsIdentifierOrKeyword(self.token())
            || self.token() == SyntaxKind::OpenBracketToken
            || self.isTemplateStartOfTaggedTemplate()
    }

    fn isStartOfOptionalPropertyOrElementAccessChain(&mut self) -> bool {
        self.token() == SyntaxKind::QuestionDotToken
            && self.lookAhead(|p| p.nextTokenIsIdentifierOrKeywordOrOpenBracketOrTemplate())
    }

    fn tryReparseOptionalChain(&mut self, mut node: &LeftHandSideExpression) -> bool {
        if self
            .factory
            .node_data(&node)
            .flags
            .intersects(NodeFlags::OptionalChain)
        {
            return true;
        }
        // check for an optional chain in a non-null expression
        if let LeftHandSideExpression::NonNullExpression(n) = node {
            let mut expr = &n.expression;
            loop {
                if let LeftHandSideExpression::NonNullExpression(n) = expr {
                    if self
                        .factory
                        .node_data(&n)
                        .flags
                        .intersects(NodeFlags::OptionalChain)
                    {
                        expr = &n.expression;
                        continue;
                    }
                }
                break;
            }
            if self
                .factory
                .node_data(&expr)
                .flags
                .intersects(NodeFlags::OptionalChain)
            {
                // this is part of an optional chain. Walk down from `node` to `expression` and set the flag.
                while let LeftHandSideExpression::NonNullExpression(n) = node {
                    self.factory.node_data_mut(&n).flags |= NodeFlags::OptionalChain;
                    node = &n.expression;
                }
                return true;
            }
        }
        return false;
    }

    fn parsePropertyAccessExpressionRest(
        &mut self,
        pos: usize,
        expression: LeftHandSideExpression,
        questionDotToken: Option<QuestionDotToken>,
    ) -> PropertyAccessExpression {
        let name = self.parseRightSideOfDot(true, true);
        let isOptionalChain =
            questionDotToken.is_some() || self.tryReparseOptionalChain(&expression);
        let propertyAccess = if isOptionalChain {
            self.factory
                .createPropertyAccessChain(expression, questionDotToken, name)
        } else {
            self.factory
                .createPropertyAccessExpression(expression, name)
        };
        if isOptionalChain && matches!(propertyAccess.name, MemberName::PrivateIdentifier(_)) {
            let range = *self.factory.node_data(&propertyAccess.name).get_range();
            self.parseErrorAtRange::<u8>(
                range,
                Diagnostics::An_optional_chain_cannot_contain_private_identifiers,
                None,
            );
        }
        self.finishNode(propertyAccess, pos, None)
    }

    fn parseElementAccessExpressionRest(
        &mut self,
        pos: usize,
        expression: LeftHandSideExpression,
        questionDotToken: Option<QuestionDotToken>,
    ) -> ElementAccessExpression {
        let argumentExpression = if self.token() == SyntaxKind::CloseBracketToken {
            todo!();
            // argumentExpression = self.createMissingNode(
            //     SyntaxKind::Identifier,
            //     /*reportAtCurrentPosition*/ true,
            //     Diagnostics::An_element_access_expression_should_take_an_argument,
            // );
        } else {
            self.allowInAnd(|p| p.parseExpression())
        };

        self.parseExpected(SyntaxKind::CloseBracketToken, None, None);

        let indexedAccess = if questionDotToken.is_some()
            || self.tryReparseOptionalChain(&expression)
        {
            self.factory
                .createElementAccessChain(expression, questionDotToken, argumentExpression)
        } else {
            self.factory
                .createElementAccessExpression(expression, argumentExpression)
        };
        self.finishNode(indexedAccess, pos, None)
    }

    fn parseMemberExpressionRest(
        &mut self,
        pos: usize,
        mut expression: LeftHandSideExpression,
        allowOptionalChain: bool,
    ) -> LeftHandSideExpression {
        loop {
            let mut questionDotToken = None;
            let mut isPropertyAccess = false;
            if allowOptionalChain && self.isStartOfOptionalPropertyOrElementAccessChain() {
                questionDotToken = Some(self.parseExpectedToken::<QuestionDotToken, u8>(
                    SyntaxKind::QuestionDotToken,
                    None,
                    None,
                ));
                isPropertyAccess = tokenIsIdentifierOrKeyword(self.token());
            } else {
                isPropertyAccess = self.parseOptional(SyntaxKind::DotToken);
            }

            if isPropertyAccess {
                expression = LeftHandSideExpression::PropertyAccessExpression(Rc::new(
                    self.parsePropertyAccessExpressionRest(pos, expression, questionDotToken),
                ));
                continue;
            }

            if questionDotToken.is_none()
                && self.token() == SyntaxKind::ExclamationToken
                && !self.scanner.hasPrecedingLineBreak()
            {
                self.nextToken();
                let node = self.factory.createNonNullExpression(expression);
                expression = LeftHandSideExpression::NonNullExpression(Rc::new(
                    self.finishNode(node, pos, None),
                ));
                continue;
            }

            // when in the [Decorator] context, we do not parse ElementAccess as it could be part of a ComputedPropertyName
            if (questionDotToken.is_some() || !self.inDecoratorContext())
                && self.parseOptional(SyntaxKind::OpenBracketToken)
            {
                expression = LeftHandSideExpression::ElementAccessExpression(Rc::new(
                    self.parseElementAccessExpressionRest(pos, expression, questionDotToken),
                ));
                continue;
            }

            if self.isTemplateStartOfTaggedTemplate() {
                expression = LeftHandSideExpression::TaggedTemplateExpression(Rc::new(
                    self.parseTaggedTemplateRest(pos, expression, questionDotToken, None),
                ));
                continue;
            }

            return expression;
        }
    }

    fn isTemplateStartOfTaggedTemplate(&self) -> bool {
        self.token() == SyntaxKind::NoSubstitutionTemplateLiteral
            || self.token() == SyntaxKind::TemplateHead
    }

    fn parseTaggedTemplateRest(
        &mut self,
        pos: usize,
        tag: LeftHandSideExpression,
        questionDotToken: Option<QuestionDotToken>,
        typeArguments: Option<NodeArray<TypeNode>>,
    ) -> TaggedTemplateExpression {
        let template = if self.token() == SyntaxKind::NoSubstitutionTemplateLiteral {
            self.reScanTemplateHeadOrNoSubstitutionTemplate();
            TemplateLiteral::NoSubstitutionTemplateLiteral(Rc::new(
                self.parseNoSubstitutionTemplateLiteral(),
            ))
        } else {
            TemplateLiteral::TemplateExpression(Rc::new(self.parseTemplateExpression(true)))
        };
        let tag_has_optional_chain = self
            .factory
            .node_data(&tag)
            .flags
            .intersects(NodeFlags::OptionalChain);
        let mut tagExpression =
            self.factory
                .createTaggedTemplateExpression(tag, typeArguments, template);
        if questionDotToken.is_some() || tag_has_optional_chain {
            self.factory.node_data_mut(&tagExpression).flags |= NodeFlags::OptionalChain;
        }
        tagExpression.questionDotToken = questionDotToken;
        self.finishNode(tagExpression, pos, None)
    }

    fn parseCallExpressionRest(
        &mut self,
        pos: usize,
        mut expression: LeftHandSideExpression,
    ) -> LeftHandSideExpression {
        loop {
            expression = self.parseMemberExpressionRest(pos, expression, true).into();
            let questionDotToken =
                self.parseOptionalToken::<QuestionDotToken>(SyntaxKind::QuestionDotToken);
            // handle 'foo<<T>()'
            // parse template arguments only in TypeScript files (not in JavaScript files).
            if !self.contextFlags.intersects(NodeFlags::JavaScriptFile)
                && (self.token() == SyntaxKind::LessThanToken
                    || self.token() == SyntaxKind::LessThanLessThanToken)
            {
                // See if this is the start of a generic invocation.  If so, consume it and
                // keep checking for postfix expressions.  Otherwise, it's just a '<' that's
                // part of an arithmetic expression.  Break out so we consume it higher in the
                // stack.
                let typeArguments = self.tryParse(|p| p.parseTypeArgumentsInExpression());
                if typeArguments.is_some() {
                    if self.isTemplateStartOfTaggedTemplate() {
                        expression = LeftHandSideExpression::TaggedTemplateExpression(Rc::new(
                            self.parseTaggedTemplateRest(
                                pos,
                                expression,
                                questionDotToken,
                                typeArguments,
                            ),
                        ));
                        continue;
                    }

                    let argumentList = self.parseArgumentList();
                    let callExpr = if questionDotToken.is_some()
                        || self.tryReparseOptionalChain(&expression)
                    {
                        self.factory.createCallChain(
                            expression,
                            questionDotToken,
                            typeArguments,
                            Some(argumentList),
                        )
                    } else {
                        self.factory.createCallExpression(
                            expression,
                            typeArguments,
                            Some(argumentList),
                        )
                    };
                    expression = LeftHandSideExpression::CallExpression(Rc::new(
                        self.finishNode(callExpr, pos, None),
                    ));
                    continue;
                }
            } else if self.token() == SyntaxKind::OpenParenToken {
                let argumentList = self.parseArgumentList();
                let callExpr =
                    if questionDotToken.is_some() || self.tryReparseOptionalChain(&expression) {
                        self.factory.createCallChain(
                            expression,
                            questionDotToken,
                            None,
                            Some(argumentList),
                        )
                    } else {
                        self.factory
                            .createCallExpression(expression, None, Some(argumentList))
                    };
                expression = LeftHandSideExpression::CallExpression(Rc::new(
                    self.finishNode(callExpr, pos, None),
                ));
                continue;
            }
            if questionDotToken.is_some() {
                todo!();
                // // We failed to parse anything, so report a missing identifier here.
                // let name = createMissingNode::<Identifier>(
                //     SyntaxKind::Identifier,
                //     /*reportAtCurrentPosition*/ false,
                //     Diagnostics::Identifier_expected,
                // );
                // expression = self.finishNode(
                //     self.factory
                //         .createPropertyAccessChain(expression, questionDotToken, name),
                //     pos,
                // );
            }
            break;
        }
        expression
    }

    fn parseArgumentList(&mut self) -> NodeArray<Expression> {
        self.parseExpected(SyntaxKind::OpenParenToken, None, None);
        let result = self.parseDelimitedList(
            ParsingContext::ArgumentExpressions,
            |p| p.parseArgumentExpression(),
            false,
        );
        self.parseExpected(SyntaxKind::CloseParenToken, None, None);
        result
    }

    fn parseTypeArgumentsInExpression(&mut self) -> Option<NodeArray<TypeNode>> {
        if self.contextFlags.intersects(NodeFlags::JavaScriptFile) {
            // TypeArguments must not be parsed in JavaScript files to avoid ambiguity with binary operators.
            return None;
        }

        if self.reScanLessThanToken() != SyntaxKind::LessThanToken {
            return None;
        }
        self.nextToken();

        let typeArguments =
            self.parseDelimitedList(ParsingContext::TypeArguments, |p| p.parseType(), false);
        if !self.parseExpected(SyntaxKind::GreaterThanToken, None, None) {
            // If it doesn't have the closing `>` then it's definitely not an type argument list.
            return None;
        }

        // If we have a '<', then only parse this as a argument list if the type arguments
        // are complete and we have an open paren.  if we don't, rewind and return nothing.
        if self.canFollowTypeArgumentsInExpression() {
            Some(typeArguments)
        } else {
            None
        }
    }

    fn canFollowTypeArgumentsInExpression(&self) -> bool {
        match self.token() {
            SyntaxKind::OpenParenToken|                 // foo<x>(
            SyntaxKind::NoSubstitutionTemplateLiteral|  // foo<T> `...`
            SyntaxKind::TemplateHead|                   // foo<T> `...${100}...`
            // these are the only tokens can legally follow a type argument
            // list. So we definitely want to treat them as type arg lists.
            SyntaxKind::DotToken|                       // foo<x>.
            SyntaxKind::CloseParenToken|                // foo<x>)
            SyntaxKind::CloseBracketToken|              // foo<x>]
            SyntaxKind::ColonToken|                     // foo<x>:
            SyntaxKind::SemicolonToken|                 // foo<x>;
            SyntaxKind::QuestionToken|                  // foo<x>?
            SyntaxKind::EqualsEqualsToken|              // foo<x> ==
            SyntaxKind::EqualsEqualsEqualsToken|        // foo<x> ===
            SyntaxKind::ExclamationEqualsToken|         // foo<x> !=
            SyntaxKind::ExclamationEqualsEqualsToken|   // foo<x> !==
            SyntaxKind::AmpersandAmpersandToken|        // foo<x> &&
            SyntaxKind::BarBarToken|                    // foo<x> ||
            SyntaxKind::QuestionQuestionToken|          // foo<x> ??
            SyntaxKind::CaretToken|                     // foo<x> ^
            SyntaxKind::AmpersandToken|                 // foo<x> &
            SyntaxKind::BarToken|                       // foo<x> |
            SyntaxKind::CloseBraceToken|                // foo<x> }
            SyntaxKind::EndOfFileToken=> {              // foo<x>
                // these cases can't legally follow a type arg list.  However, they're not legal
                // expressions either.  The user is probably in the middle of a generic type. So
                // treat it as such.
                true
            }

            SyntaxKind::CommaToken|                     // foo<x>,
            SyntaxKind::OpenBraceToken=> {              // foo<x> {
                // We don't want to treat these as type arguments.  Otherwise we'll parse this
                // as an invocation expression.  Instead, we want to parse out the expression
                // in isolation from the type arguments.
                false
            }
            _ => {
                // Anything else treat as an expression.
                false
            }
        }
    }

    fn parsePrimaryExpression(&mut self) -> PrimaryExpression {
        match self.token() {
            SyntaxKind::NumericLiteral => {
                return PrimaryExpression::NumericLiteral(Rc::new(self.parseNumericLiteral()));
            }
            SyntaxKind::StringLiteral => {
                return PrimaryExpression::StringLiteral(Rc::new(self.parseStringLiteral()));
            }
            SyntaxKind::BigIntLiteral => {
                return PrimaryExpression::BigIntLiteral(Rc::new(self.parseBigIntLiteral()));
            }
            SyntaxKind::NoSubstitutionTemplateLiteral => {
                return PrimaryExpression::NoSubstitutionTemplateLiteral(Rc::new(
                    self.parseNoSubstitutionTemplateLiteral(),
                ));
            }
            SyntaxKind::ThisKeyword => {
                return PrimaryExpression::ThisExpression(Rc::new(
                    self.parseTokenNode::<ThisExpression>(),
                ));
            }
            SyntaxKind::SuperKeyword => {
                return PrimaryExpression::SuperExpression(Rc::new(
                    self.parseTokenNode::<SuperExpression>(),
                ));
            }
            SyntaxKind::NullKeyword => {
                return PrimaryExpression::NullLiteral(Rc::new(
                    self.parseTokenNode::<NullLiteral>(),
                ));
            }
            SyntaxKind::TrueKeyword => {
                return PrimaryExpression::TrueLiteral(Rc::new(
                    self.parseTokenNode::<TrueLiteral>(),
                ));
            }
            SyntaxKind::FalseKeyword => {
                return PrimaryExpression::FalseLiteral(Rc::new(
                    self.parseTokenNode::<FalseLiteral>(),
                ));
            }
            SyntaxKind::OpenParenToken => {
                return PrimaryExpression::ParenthesizedExpression(Rc::new(
                    self.parseParenthesizedExpression(),
                ));
            }
            SyntaxKind::OpenBracketToken => {
                return PrimaryExpression::ArrayLiteralExpression(Rc::new(
                    self.parseArrayLiteralExpression(),
                ));
            }
            SyntaxKind::OpenBraceToken => {
                return PrimaryExpression::ObjectLiteralExpression(Rc::new(
                    self.parseObjectLiteralExpression(),
                ));
            }
            SyntaxKind::AsyncKeyword => {
                // Async arrow functions are parsed earlier in parseAssignmentExpressionOrHigher.
                // If we encounter `async [no LineTerminator here] function` then this is an async
                // function; otherwise, its an identifier.
                if self.lookAhead(|p| p.nextTokenIsFunctionKeywordOnSameLine()) {
                    return PrimaryExpression::FunctionExpression(Rc::new(
                        self.parseFunctionExpression(),
                    ));
                }
            }
            SyntaxKind::ClassKeyword => {
                return PrimaryExpression::ClassExpression(Rc::new(self.parseClassExpression()));
            }
            SyntaxKind::FunctionKeyword => {
                return PrimaryExpression::FunctionExpression(Rc::new(
                    self.parseFunctionExpression(),
                ));
            }
            SyntaxKind::NewKeyword => {
                return self.parseNewExpressionOrNewDotTarget();
            }
            SyntaxKind::SlashToken | SyntaxKind::SlashEqualsToken => {
                if self.reScanSlashToken() == SyntaxKind::RegularExpressionLiteral {
                    return PrimaryExpression::RegularExpressionLiteral(Rc::new(
                        self.parseRegularExpressionLiteral(),
                    ));
                }
            }
            SyntaxKind::TemplateHead => {
                return PrimaryExpression::TemplateExpression(Rc::new(
                    self.parseTemplateExpression(false),
                ));
            }
            SyntaxKind::PrivateIdentifier => {
                return PrimaryExpression::PrivateIdentifier(Rc::new(
                    self.parsePrivateIdentifier(),
                ));
            }
            _ => {}
        }

        PrimaryExpression::Identifier(Rc::new(
            self.parseIdentifier(Some(Diagnostics::Expression_expected), None),
        ))
    }

    fn parseParenthesizedExpression(&mut self) -> ParenthesizedExpression {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        self.parseExpected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allowInAnd(|p| p.parseExpression());
        self.parseExpected(SyntaxKind::CloseParenToken, None, None);
        let node = self.factory.createParenthesizedExpression(expression);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseSpreadElement(&mut self) -> SpreadElement {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::DotDotDotToken, None, None);
        let expression = self.parseAssignmentExpressionOrHigher();
        let node = self.factory.createSpreadElement(expression);
        self.finishNode(node, pos, None)
    }

    fn parseArgumentOrArrayLiteralElement(&mut self) -> Expression {
        if self.token() == SyntaxKind::DotDotDotToken {
            Expression::SpreadElement(Rc::new(self.parseSpreadElement()))
        } else if self.token() == SyntaxKind::CommaToken {
            let node = self.factory.createOmittedExpression();
            Expression::OmittedExpression(Rc::new(self.finishNode(node, self.getNodePos(), None)))
        } else {
            self.parseAssignmentExpressionOrHigher()
        }
    }

    fn parseArgumentExpression(&mut self) -> Expression {
        self.doOutsideOfContext(disallowInAndDecoratorContext, |p| {
            p.parseArgumentOrArrayLiteralElement()
        })
    }

    fn parseArrayLiteralExpression(&mut self) -> ArrayLiteralExpression {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::OpenBracketToken, None, None);
        let multiLine = self.scanner.hasPrecedingLineBreak();
        let elements = self.parseDelimitedList(
            ParsingContext::ArrayLiteralMembers,
            |p| p.parseArgumentOrArrayLiteralElement(),
            false,
        );
        self.parseExpected(SyntaxKind::CloseBracketToken, None, None);
        let node = self
            .factory
            .createArrayLiteralExpression(Some(elements), multiLine);
        self.finishNode(node, pos, None)
    }

    fn parseObjectLiteralElement(&mut self) -> ObjectLiteralElementLike {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();

        if self
            .parseOptionalToken::<DotDotDotToken>(SyntaxKind::DotDotDotToken)
            .is_some()
        {
            let expression = self.parseAssignmentExpressionOrHigher();
            let node = self.factory.createSpreadAssignment(expression);
            let node = self.finishNode(node, pos, None);
            return ObjectLiteralElementLike::SpreadAssignment(Rc::new(
                self.withJSDoc(node, hasJSDoc),
            ));
        }

        let decorators = self.parseDecorators();
        let modifiers = self.parseModifiers(false, false);

        if self.parseContextualModifier(SyntaxKind::GetKeyword) {
            return ObjectLiteralElementLike::GetAccessorDeclaration(Rc::new(
                self.parseGetAccessorDeclaration(pos, hasJSDoc, decorators, modifiers),
            ));
        }
        if self.parseContextualModifier(SyntaxKind::SetKeyword) {
            return ObjectLiteralElementLike::SetAccessorDeclaration(Rc::new(
                self.parseSetAccessorDeclaration(pos, hasJSDoc, decorators, modifiers),
            ));
        }

        let asteriskToken = self.parseOptionalToken(SyntaxKind::AsteriskToken);
        let tokenIsIdentifier = self.isIdentifier();
        let name = self.parsePropertyName();

        // Disallowing of optional property assignments and definite assignment assertion happens in the grammar checker.
        let questionToken = self.parseOptionalToken(SyntaxKind::QuestionToken);
        let exclamationToken = self.parseOptionalToken(SyntaxKind::ExclamationToken);

        if asteriskToken.is_some()
            || self.token() == SyntaxKind::OpenParenToken
            || self.token() == SyntaxKind::LessThanToken
        {
            return ObjectLiteralElementLike::MethodDeclaration(Rc::new(
                self.parseMethodDeclaration(
                    pos,
                    hasJSDoc,
                    decorators,
                    modifiers,
                    asteriskToken,
                    name,
                    questionToken,
                    exclamationToken,
                    None,
                ),
            ));
        }

        // check if it is short-hand property assignment or normal property assignment
        // NOTE: if token is EqualsToken it is interpreted as CoverInitializedName production
        // CoverInitializedName[Yield] :
        //     IdentifierReference[?Yield] Initializer[In, ?Yield]
        // this is necessary because ObjectLiteral productions are also used to cover grammar for ObjectAssignmentPattern
        let isShorthandPropertyAssignment =
            tokenIsIdentifier && (self.token() != SyntaxKind::ColonToken);
        if isShorthandPropertyAssignment {
            let equalsToken = self.parseOptionalToken(SyntaxKind::EqualsToken);
            let objectAssignmentInitializer = if equalsToken.is_some() {
                Some(self.allowInAnd(|p| p.parseAssignmentExpressionOrHigher()))
            } else {
                None
            };
            let mut node = self.factory.createShorthandPropertyAssignment(
                unwrap_as!(name, PropertyName::Identifier(i), i),
                objectAssignmentInitializer,
            );
            // Save equals token for error reporting.
            // TODO(rbuckton): Consider manufacturing this when we need to report an error as it is otherwise not useful.
            node.equalsToken = equalsToken;
            // Decorators, Modifiers, questionToken, and exclamationToken are not supported by property assignments and are reported in the grammar checker
            node.decorators = decorators;
            node.modifiers = modifiers;
            node.questionToken = questionToken;
            node.exclamationToken = exclamationToken;
            let node = self.finishNode(node, pos, None);
            ObjectLiteralElementLike::ShorthandPropertyAssignment(Rc::new(
                self.withJSDoc(node, hasJSDoc),
            ))
        } else {
            self.parseExpected(SyntaxKind::ColonToken, None, None);
            let initializer = self.allowInAnd(|p| p.parseAssignmentExpressionOrHigher());
            let mut node = self.factory.createPropertyAssignment(name, initializer);
            // Decorators, Modifiers, questionToken, and exclamationToken are not supported by property assignments and are reported in the grammar checker
            node.decorators = decorators;
            node.modifiers = modifiers;
            node.questionToken = questionToken;
            node.exclamationToken = exclamationToken;
            let node = self.finishNode(node, pos, None);
            ObjectLiteralElementLike::PropertyAssignment(Rc::new(self.withJSDoc(node, hasJSDoc)))
        }
    }

    fn parseObjectLiteralExpression(&mut self) -> ObjectLiteralExpression {
        let pos = self.getNodePos();
        let openBracePosition = self.scanner.getTokenPos();
        self.parseExpected(SyntaxKind::OpenBraceToken, None, None);
        let multiLine = self.scanner.hasPrecedingLineBreak();
        let properties = self.parseDelimitedList(
            ParsingContext::ObjectLiteralMembers,
            |p| p.parseObjectLiteralElement(),
            true,
        );
        if !self.parseExpected(SyntaxKind::CloseBraceToken, None, None) {
            if let Some(lastError) = self.parseDiagnostics.last_mut() {
                if lastError.code == Diagnostics::_0_expected.code {
                    addRelatedInfo(
                        lastError,
                        [DiagnosticRelatedInformation::Diagnostic(
                            createDetachedDiagnostic::<u8>(
                                self.fileName.clone(),
                                openBracePosition,
                                1,
                                Diagnostics::The_parser_expected_to_find_a_to_match_the_token_here,
                                None,
                            ),
                        )],
                    );
                }
            }
        }
        let node = self
            .factory
            .createObjectLiteralExpression(Some(properties), multiLine);
        self.finishNode(node, pos, None)
    }

    fn parseFunctionExpression(&mut self) -> FunctionExpression {
        // GeneratorExpression:
        //      function* BindingIdentifier [Yield][opt](FormalParameters[Yield]){ GeneratorBody }
        //
        // FunctionExpression:
        //      function BindingIdentifier[opt](FormalParameters){ FunctionBody }
        let savedDecoratorContext = self.inDecoratorContext();
        self.setDecoratorContext(false);

        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        let modifiers = self.parseModifiers(false, false);
        self.parseExpected(SyntaxKind::FunctionKeyword, None, None);
        let asteriskToken = self.parseOptionalToken(SyntaxKind::AsteriskToken);
        let isGenerator = if asteriskToken.is_some() {
            SignatureFlags::Yield
        } else {
            SignatureFlags::None
        };
        let isAsync = modifiers
            .as_ref()
            .map(|m| m.iter().any(|m| isAsyncModifier(m)))
            .unwrap_or_default();
        let isAsync = if isAsync {
            SignatureFlags::Await
        } else {
            SignatureFlags::None
        };
        let name = if !isGenerator.is_empty() && !isAsync.is_empty() {
            self.doInYieldAndAwaitContext(|p| p.parseOptionalBindingIdentifier())
        } else if !isGenerator.is_empty() {
            self.doInYieldContext(|p| p.parseOptionalBindingIdentifier())
        } else if !isAsync.is_empty() {
            self.doInAwaitContext(|p| p.parseOptionalBindingIdentifier())
        } else {
            self.parseOptionalBindingIdentifier()
        };

        let typeParameters = self.parseTypeParameters();
        let parameters = self.parseParameters(isGenerator | isAsync);
        let ty = self.parseReturnType(SyntaxKind::ColonToken, false);
        let body = self.parseFunctionBlock(isGenerator | isAsync, None);

        self.setDecoratorContext(savedDecoratorContext);

        let node = self.factory.createFunctionExpression(
            modifiers,
            asteriskToken,
            name.map(Rc::new),
            typeParameters,
            Some(parameters),
            ty,
            body,
        );
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseOptionalBindingIdentifier(&mut self) -> Option<Identifier> {
        if self.isBindingIdentifier() {
            Some(self.parseBindingIdentifier(None))
        } else {
            None
        }
    }

    fn parseNewExpressionOrNewDotTarget(&mut self) -> PrimaryExpression {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::NewKeyword, None, None);
        if self.parseOptional(SyntaxKind::DotToken) {
            let name = self.parseIdentifierName(None);
            let node = self
                .factory
                .createMetaProperty(SyntaxKind::NewKeyword, Rc::new(name));
            return PrimaryExpression::MetaProperty(Rc::new(self.finishNode(node, pos, None)));
        }

        let expressionPos = self.getNodePos();
        let mut expression: LeftHandSideExpression =
            MemberExpression::from(self.parsePrimaryExpression()).into();
        let mut typeArguments = None;
        loop {
            expression = self.parseMemberExpressionRest(expressionPos, expression.into(), false);
            typeArguments = self.tryParse(|p| p.parseTypeArgumentsInExpression());
            if self.isTemplateStartOfTaggedTemplate() {
                debug_assert!(typeArguments.is_some(),
                        "Expected a type argument list; all plain tagged template starts should be consumed in 'parseMemberExpressionRest'");
                expression = LeftHandSideExpression::TaggedTemplateExpression(Rc::new(
                    self.parseTaggedTemplateRest(
                        expressionPos,
                        expression.into(),
                        None,
                        typeArguments,
                    ),
                ));
                typeArguments = None;
            }
            break;
        }

        let mut argumentsArray = None;
        if self.token() == SyntaxKind::OpenParenToken {
            argumentsArray = Some(self.parseArgumentList());
        } else if typeArguments.is_some() {
            self.parseErrorAt::<u8>(pos, self.scanner.getStartPos(), Diagnostics::A_new_expression_with_type_arguments_must_always_be_followed_by_a_parenthesized_argument_list,None);
        }
        let node =
            self.factory
                .createNewExpression(expression.into(), typeArguments, argumentsArray);
        PrimaryExpression::NewExpression(Rc::new(self.finishNode(node, pos, None)))
    }

    // STATEMENTS
    fn parseBlock(
        &mut self,
        ignoreMissingOpenBrace: bool,
        diagnosticMessage: Option<DiagnosticMessage>,
    ) -> Block {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        let openBracePosition = self.scanner.getTokenPos();
        if self.parseExpected(SyntaxKind::OpenBraceToken, diagnosticMessage, None)
            || ignoreMissingOpenBrace
        {
            let multiLine = self.scanner.hasPrecedingLineBreak();
            let statements =
                self.parseList(ParsingContext::BlockStatements, |p| p.parseStatement());
            if !self.parseExpected(SyntaxKind::CloseBraceToken, None, None) {
                if let Some(lastError) = self.parseDiagnostics.last() {
                    if lastError.code == Diagnostics::_0_expected.code {
                        todo!();
                        // addRelatedInfo(
                        //     lastError,
                        //     createDetachedDiagnostic(fileName, openBracePosition, 1, Diagnostics::The_parser_expected_to_find_a_to_match_the_token_here)
                        // );
                    }
                }
            }
            let node = self.factory.createBlock(statements, Some(multiLine));
            let node = self.finishNode(node, pos, None);
            let result = self.withJSDoc(node, hasJSDoc);
            if self.token() == SyntaxKind::EqualsToken {
                self.parseErrorAtCurrentToken::<u8>(Diagnostics::Declaration_or_statement_expected_This_follows_a_block_of_statements_so_if_you_intended_to_write_a_destructuring_assignment_you_might_need_to_wrap_the_the_whole_assignment_in_parentheses,None);
                self.nextToken();
            }

            return result;
        } else {
            let statements = self.createMissingList::<Statement>();
            let node = self.factory.createBlock(statements, None);
            let node = self.finishNode(node, pos, None);
            return self.withJSDoc(node, hasJSDoc);
        }
    }

    fn parseFunctionBlock(
        &mut self,
        flags: SignatureFlags,
        diagnosticMessage: Option<DiagnosticMessage>,
    ) -> Block {
        let savedYieldContext = self.inYieldContext();
        self.setYieldContext(flags.intersects(SignatureFlags::Yield));

        let savedAwaitContext = self.inAwaitContext();
        self.setAwaitContext(flags.intersects(SignatureFlags::Await));

        let savedTopLevel = self.topLevel;
        self.topLevel = false;

        // We may be in a [Decorator] context when parsing a function expression or
        // arrow function. The body of the function is not in [Decorator] context.
        let saveDecoratorContext = self.inDecoratorContext();
        if saveDecoratorContext {
            self.setDecoratorContext(false);
        }

        let block = self.parseBlock(
            flags.intersects(SignatureFlags::IgnoreMissingOpenBrace),
            diagnosticMessage,
        );

        if saveDecoratorContext {
            self.setDecoratorContext(true);
        }

        self.topLevel = savedTopLevel;
        self.setYieldContext(savedYieldContext);
        self.setAwaitContext(savedAwaitContext);

        block
    }

    fn parseEmptyStatement(&mut self) -> Statement {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        self.parseExpected(SyntaxKind::SemicolonToken, None, None);
        let stmt = self.factory.createEmptyStatement();
        let stmt = self.finishNode(stmt, pos, None);
        let stmt = self.withJSDoc(stmt, hasJSDoc);
        Statement::EmptyStatement(Rc::new(stmt))
    }

    fn parseIfStatement(&mut self) -> IfStatement {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        self.parseExpected(SyntaxKind::IfKeyword, None, None);
        self.parseExpected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allowInAnd(|p| p.parseExpression());
        self.parseExpected(SyntaxKind::CloseParenToken, None, None);
        let thenStatement = self.parseStatement();
        let elseStatement = if self.parseOptional(SyntaxKind::ElseKeyword) {
            Some(self.parseStatement())
        } else {
            None
        };
        let node = self
            .factory
            .createIfStatement(expression, thenStatement, elseStatement);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseDoStatement(&mut self) -> DoStatement {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        self.parseExpected(SyntaxKind::DoKeyword, None, None);
        let statement = self.parseStatement();
        self.parseExpected(SyntaxKind::WhileKeyword, None, None);
        self.parseExpected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allowInAnd(|p| p.parseExpression());
        self.parseExpected(SyntaxKind::CloseParenToken, None, None);

        // From: https://mail.mozilla.org/pipermail/es-discuss/2011-August/016188.html
        // 157 min --- All allen at wirfs-brock.com CONF --- "do{;}while(false)false" prohibited in
        // spec but allowed in consensus reality. Approved -- this is the de-facto standard whereby
        //  do;while(0)x will have a semicolon inserted before x.
        self.parseOptional(SyntaxKind::SemicolonToken);
        let node = self.factory.createDoStatement(statement, expression);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseWhileStatement(&mut self) -> WhileStatement {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        self.parseExpected(SyntaxKind::WhileKeyword, None, None);
        self.parseExpected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allowInAnd(|p| p.parseExpression());
        self.parseExpected(SyntaxKind::CloseParenToken, None, None);
        let statement = self.parseStatement();
        let node = self.factory.createWhileStatement(expression, statement);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseForOrForInOrForOfStatement(&mut self) -> Statement {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        self.parseExpected(SyntaxKind::ForKeyword, None, None);
        let awaitToken = self.parseOptionalToken::<AwaitKeyword>(SyntaxKind::AwaitKeyword);
        self.parseExpected(SyntaxKind::OpenParenToken, None, None);

        let mut initializer = None;
        if self.token() != SyntaxKind::SemicolonToken {
            if self.token() == SyntaxKind::VarKeyword
                || self.token() == SyntaxKind::LetKeyword
                || self.token() == SyntaxKind::ConstKeyword
            {
                initializer = Some(ForInitializer::VariableDeclarationList(Rc::new(
                    self.parseVariableDeclarationList(true),
                )));
            } else {
                initializer = Some(ForInitializer::Expression(
                    self.disallowInAnd(|p| p.parseExpression()),
                ));
            }
        }

        if {
            if awaitToken.is_some() {
                self.parseExpected(SyntaxKind::OfKeyword, None, None)
            } else {
                self.parseOptional(SyntaxKind::OfKeyword)
            }
        } {
            let expression = self.allowInAnd(|p| p.parseAssignmentExpressionOrHigher());
            self.parseExpected(SyntaxKind::CloseParenToken, None, None);
            let body = self.parseStatement();
            let node = self.factory.createForOfStatement(
                awaitToken,
                initializer.unwrap(),
                expression,
                body,
            );
            let node = self.finishNode(node, pos, None);
            let node = self.withJSDoc(node, hasJSDoc);
            Statement::ForOfStatement(Rc::new(node))
        } else if self.parseOptional(SyntaxKind::InKeyword) {
            let expression = self.allowInAnd(|p| p.parseExpression());
            self.parseExpected(SyntaxKind::CloseParenToken, None, None);
            let body = self.parseStatement();
            let node = self
                .factory
                .createForInStatement(initializer.unwrap(), expression, body);
            let node = self.finishNode(node, pos, None);
            let node = self.withJSDoc(node, hasJSDoc);
            Statement::ForInStatement(Rc::new(node))
        } else {
            self.parseExpected(SyntaxKind::SemicolonToken, None, None);
            let condition = if self.token() != SyntaxKind::SemicolonToken
                && self.token() != SyntaxKind::CloseParenToken
            {
                Some(self.allowInAnd(|p| p.parseExpression()))
            } else {
                None
            };
            self.parseExpected(SyntaxKind::SemicolonToken, None, None);
            let incrementor = if self.token() != SyntaxKind::CloseParenToken {
                Some(self.allowInAnd(|p| p.parseExpression()))
            } else {
                None
            };
            self.parseExpected(SyntaxKind::CloseParenToken, None, None);
            let body = self.parseStatement();
            let node = self
                .factory
                .createForStatement(initializer, condition, incrementor, body);
            let node = self.finishNode(node, pos, None);
            let node = self.withJSDoc(node, hasJSDoc);
            Statement::ForStatement(Rc::new(node))
        }
    }

    fn parseBreakStatement(&mut self) -> BreakStatement {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();

        self.parseExpected(SyntaxKind::BreakKeyword, None, None);
        let label = if self.canParseSemicolon() {
            None
        } else {
            Some(Rc::new(self.parseIdentifier(None, None)))
        };

        self.parseSemicolon();
        let node = self.factory.createBreakStatement(label);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseContinueStatement(&mut self) -> ContinueStatement {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();

        self.parseExpected(SyntaxKind::ContinueKeyword, None, None);
        let label = if self.canParseSemicolon() {
            None
        } else {
            Some(Rc::new(self.parseIdentifier(None, None)))
        };

        self.parseSemicolon();
        let node = self.factory.createContinueStatement(label);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseReturnStatement(&mut self) -> ReturnStatement {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        self.parseExpected(SyntaxKind::ReturnKeyword, None, None);
        let expression = if self.canParseSemicolon() {
            None
        } else {
            Some(self.allowInAnd(|p| p.parseExpression()))
        };
        self.parseSemicolon();
        let node = self.factory.createReturnStatement(expression);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseWithStatement(&mut self) -> WithStatement {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        self.parseExpected(SyntaxKind::WithKeyword, None, None);
        self.parseExpected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allowInAnd(|p| p.parseExpression());
        self.parseExpected(SyntaxKind::CloseParenToken, None, None);
        let statement = self.doInsideOfContext(NodeFlags::InWithStatement, |p| p.parseStatement());
        let node = self.factory.createWithStatement(expression, statement);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseCaseClause(&mut self) -> CaseClause {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::CaseKeyword, None, None);
        let expression = self.allowInAnd(|p| p.parseExpression());
        self.parseExpected(SyntaxKind::ColonToken, None, None);
        let statements = self.parseList(ParsingContext::SwitchClauseStatements, |p| {
            p.parseStatement()
        });
        let node = self.factory.createCaseClause(expression, statements);
        self.finishNode(node, pos, None)
    }

    fn parseDefaultClause(&mut self) -> DefaultClause {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::DefaultKeyword, None, None);
        self.parseExpected(SyntaxKind::ColonToken, None, None);
        let statements = self.parseList(ParsingContext::SwitchClauseStatements, |p| {
            p.parseStatement()
        });
        let node = self.factory.createDefaultClause(statements);
        self.finishNode(node, pos, None)
    }

    fn parseCaseOrDefaultClause(&mut self) -> CaseOrDefaultClause {
        if self.token() == SyntaxKind::CaseKeyword {
            CaseOrDefaultClause::CaseClause(Rc::new(self.parseCaseClause()))
        } else {
            CaseOrDefaultClause::DefaultClause(Rc::new(self.parseDefaultClause()))
        }
    }

    fn parseCaseBlock(&mut self) -> CaseBlock {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::OpenBraceToken, None, None);
        let clauses = self.parseList(ParsingContext::SwitchClauses, |p| {
            p.parseCaseOrDefaultClause()
        });
        self.parseExpected(SyntaxKind::CloseBraceToken, None, None);
        let node = self.factory.createCaseBlock(clauses);
        self.finishNode(node, pos, None)
    }

    fn parseSwitchStatement(&mut self) -> SwitchStatement {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        self.parseExpected(SyntaxKind::SwitchKeyword, None, None);
        self.parseExpected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allowInAnd(|p| p.parseExpression());
        self.parseExpected(SyntaxKind::CloseParenToken, None, None);
        let caseBlock = self.parseCaseBlock();
        let node = self.factory.createSwitchStatement(expression, caseBlock);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseThrowStatement(&mut self) -> ThrowStatement {
        // ThrowStatement[Yield] :
        //      throw [no LineTerminator here]Expression[In, ?Yield];

        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        self.parseExpected(SyntaxKind::ThrowKeyword, None, None);

        // Because of automatic semicolon insertion, we need to report error if this
        // throw could be terminated with a semicolon.  Note: we can't call 'parseExpression'
        // directly as that might consume an expression on the following line.
        // Instead, we create a "missing" identifier, but don't report an error. The actual error
        // will be reported in the grammar walker.
        let expression = if self.scanner.hasPrecedingLineBreak() {
            None
        } else {
            Some(self.allowInAnd(|p| p.parseExpression()))
        };
        let expression = match expression {
            Some(e) => e,
            None => {
                // identifierCount++;
                let node = self.factory.createIdentifier(JsWord::from(""), None, None);
                Expression::Identifier(Rc::new(self.finishNode(node, self.getNodePos(), None)))
            }
        };
        if !self.tryParseSemicolon() {
            self.parseErrorForMissingSemicolonAfter(expression.clone().into());
        }
        let node = self.factory.createThrowStatement(expression);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    // TODO: Review for error recovery
    fn parseTryStatement(&mut self) -> TryStatement {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();

        self.parseExpected(SyntaxKind::TryKeyword, None, None);
        let tryBlock = self.parseBlock(false, None);
        let catchClause = if self.token() == SyntaxKind::CatchKeyword {
            Some(self.parseCatchClause())
        } else {
            None
        };

        // If we don't have a catch clause, then we must have a finally clause.  Try to parse
        // one out no matter what.
        let mut finallyBlock = None;
        if catchClause.is_none() || self.token() == SyntaxKind::FinallyKeyword {
            self.parseExpected(SyntaxKind::FinallyKeyword, None, None);
            finallyBlock = Some(self.parseBlock(false, None));
        }
        let node = self
            .factory
            .createTryStatement(tryBlock, catchClause, finallyBlock);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseCatchClause(&mut self) -> CatchClause {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::CatchKeyword, None, None);

        let variableDeclaration;
        if self.parseOptional(SyntaxKind::OpenParenToken) {
            variableDeclaration = Some(self.parseVariableDeclaration(false));
            self.parseExpected(SyntaxKind::CloseParenToken, None, None);
        } else {
            // Keep shape of node to avoid degrading performance.
            variableDeclaration = None;
        }

        let block = self.parseBlock(false, None);
        let node = self.factory.createCatchClause(variableDeclaration, block);
        self.finishNode(node, pos, None)
    }

    fn parseDebuggerStatement(&mut self) -> DebuggerStatement {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        self.parseExpected(SyntaxKind::DebuggerKeyword, None, None);
        self.parseSemicolon();
        let node = self.factory.createDebuggerStatement();
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    /// Returns an `ExpressionStatement` or `LabeledStatement`
    fn parseExpressionOrLabeledStatement(&mut self) -> Statement {
        // Avoiding having to do the lookahead for a labeled statement by just trying to parse
        // out an expression, seeing if it is identifier and then seeing if it is followed by
        // a colon.
        let pos = self.getNodePos();
        let mut hasJSDoc = self.hasPrecedingJSDocComment();
        let hasParen = self.token() == SyntaxKind::OpenParenToken;
        let expression = self.allowInAnd(|p| p.parseExpression());
        if matches!(expression, Expression::Identifier(_))
            && self.parseOptional(SyntaxKind::ColonToken)
        {
            let statement = self.parseStatement();
            let node = self.factory.createLabeledStatement(
                unwrap_as!(expression, Expression::Identifier(i), i),
                statement,
            );
            let node = self.finishNode(node, pos, None);
            Statement::LabeledStatement(Rc::new(self.withJSDoc(node, hasJSDoc)))
        } else {
            if !self.tryParseSemicolon() {
                self.parseErrorForMissingSemicolonAfter(expression.clone().into());
            }
            if hasParen {
                // do not parse the same jsdoc twice
                hasJSDoc = false;
            }
            let node = self.factory.createExpressionStatement(expression);
            let node = self.finishNode(node, pos, None);
            Statement::ExpressionStatement(Rc::new(self.withJSDoc(node, hasJSDoc)))
        }
    }

    fn nextTokenIsIdentifierOrKeywordOnSameLine(&mut self) -> bool {
        self.nextToken();
        tokenIsIdentifierOrKeyword(self.token()) && !self.scanner.hasPrecedingLineBreak()
    }

    fn nextTokenIsClassKeywordOnSameLine(&mut self) -> bool {
        self.nextToken();
        self.token() == SyntaxKind::ClassKeyword && !self.scanner.hasPrecedingLineBreak()
    }

    fn nextTokenIsFunctionKeywordOnSameLine(&mut self) -> bool {
        self.nextToken();
        self.token() == SyntaxKind::FunctionKeyword && !self.scanner.hasPrecedingLineBreak()
    }

    fn nextTokenIsIdentifierOrKeywordOrLiteralOnSameLine(&mut self) -> bool {
        self.nextToken();
        (tokenIsIdentifierOrKeyword(self.token())
            || self.token() == SyntaxKind::NumericLiteral
            || self.token() == SyntaxKind::BigIntLiteral
            || self.token() == SyntaxKind::StringLiteral)
            && !self.scanner.hasPrecedingLineBreak()
    }

    fn isDeclaration(&mut self) -> bool {
        loop {
            match self.token() {
                SyntaxKind::VarKeyword
                | SyntaxKind::LetKeyword
                | SyntaxKind::ConstKeyword
                | SyntaxKind::FunctionKeyword
                | SyntaxKind::ClassKeyword
                | SyntaxKind::EnumKeyword => {
                    return true;
                }

                // 'declare', 'module', 'namespace', 'interface'* and 'type' are all legal JavaScript identifiers;
                // however, an identifier cannot be followed by another identifier on the same line. This is what we
                // count on to parse out the respective declarations. For instance, we exploit this to say that
                //
                //    namespace n
                //
                // can be none other than the beginning of a namespace declaration, but need to respect that JavaScript sees
                //
                //    namespace
                //    n
                //
                // as the identifier 'namespace' on one line followed by the identifier 'n' on another.
                // We need to look one token ahead to see if it permissible to try parsing a declaration.
                //
                // *Note*: 'interface' is actually a strict mode reserved word. So while
                //
                //   "use strict"
                //   interface
                //   I {}
                //
                // could be legal, it would add complexity for very little gain.
                SyntaxKind::InterfaceKeyword | SyntaxKind::TypeKeyword => {
                    return self.nextTokenIsIdentifierOnSameLine();
                }
                SyntaxKind::ModuleKeyword | SyntaxKind::NamespaceKeyword => {
                    return self.nextTokenIsIdentifierOrStringLiteralOnSameLine();
                }
                SyntaxKind::AbstractKeyword
                | SyntaxKind::AsyncKeyword
                | SyntaxKind::DeclareKeyword
                | SyntaxKind::PrivateKeyword
                | SyntaxKind::ProtectedKeyword
                | SyntaxKind::PublicKeyword
                | SyntaxKind::ReadonlyKeyword => {
                    self.nextToken();
                    // ASI takes effect for this modifier.
                    if self.scanner.hasPrecedingLineBreak() {
                        return false;
                    }
                    continue;
                }

                SyntaxKind::GlobalKeyword => {
                    self.nextToken();
                    return self.token() == SyntaxKind::OpenBraceToken
                        || self.token() == SyntaxKind::Identifier
                        || self.token() == SyntaxKind::ExportKeyword;
                }

                SyntaxKind::ImportKeyword => {
                    self.nextToken();
                    return self.token() == SyntaxKind::StringLiteral
                        || self.token() == SyntaxKind::AsteriskToken
                        || self.token() == SyntaxKind::OpenBraceToken
                        || tokenIsIdentifierOrKeyword(self.token());
                }
                SyntaxKind::ExportKeyword => {
                    let mut currentToken = self.nextToken();
                    if currentToken == SyntaxKind::TypeKeyword {
                        currentToken = self.lookAhead(|p| Some(p.nextToken())).unwrap();
                    }
                    if currentToken == SyntaxKind::EqualsToken
                        || currentToken == SyntaxKind::AsteriskToken
                        || currentToken == SyntaxKind::OpenBraceToken
                        || currentToken == SyntaxKind::DefaultKeyword
                        || currentToken == SyntaxKind::AsKeyword
                    {
                        return true;
                    }
                    continue;
                }
                SyntaxKind::StaticKeyword => {
                    self.nextToken();
                    continue;
                }
                _ => {
                    return false;
                }
            }
        }
    }

    fn isStartOfDeclaration(&mut self) -> bool {
        self.lookAhead(|p| p.isDeclaration())
    }

    fn isStartOfStatement(&mut self) -> bool {
        match self.token() {
            SyntaxKind::AtToken
            | SyntaxKind::SemicolonToken
            | SyntaxKind::OpenBraceToken
            | SyntaxKind::VarKeyword
            | SyntaxKind::LetKeyword
            | SyntaxKind::FunctionKeyword
            | SyntaxKind::ClassKeyword
            | SyntaxKind::EnumKeyword
            | SyntaxKind::IfKeyword
            | SyntaxKind::DoKeyword
            | SyntaxKind::WhileKeyword
            | SyntaxKind::ForKeyword
            | SyntaxKind::ContinueKeyword
            | SyntaxKind::BreakKeyword
            | SyntaxKind::ReturnKeyword
            | SyntaxKind::WithKeyword
            | SyntaxKind::SwitchKeyword
            | SyntaxKind::ThrowKeyword
            | SyntaxKind::TryKeyword
            | SyntaxKind::DebuggerKeyword => true,
            // 'catch' and 'finally' do not actually indicate that the code is part of a statement,
            // however, we say they are here so that we may gracefully parse them and error later.
            // falls through
            SyntaxKind::CatchKeyword | SyntaxKind::FinallyKeyword => true,

            SyntaxKind::ImportKeyword => {
                self.isStartOfDeclaration()
                    || self.lookAhead(|p| p.nextTokenIsOpenParenOrLessThanOrDot())
            }
            SyntaxKind::ConstKeyword | SyntaxKind::ExportKeyword => self.isStartOfDeclaration(),
            SyntaxKind::AsyncKeyword
            | SyntaxKind::DeclareKeyword
            | SyntaxKind::InterfaceKeyword
            | SyntaxKind::ModuleKeyword
            | SyntaxKind::NamespaceKeyword
            | SyntaxKind::TypeKeyword
            | SyntaxKind::GlobalKeyword => {
                // When these don't start a declaration, they're an identifier in an expression statement
                true
            }

            SyntaxKind::PublicKeyword
            | SyntaxKind::PrivateKeyword
            | SyntaxKind::ProtectedKeyword
            | SyntaxKind::StaticKeyword
            | SyntaxKind::ReadonlyKeyword => {
                // When these don't start a declaration, they may be the start of a class member if an identifier
                // immediately follows. Otherwise they're an identifier in an expression statement.
                self.isStartOfDeclaration()
                    || !self.lookAhead(|p| p.nextTokenIsIdentifierOrKeywordOnSameLine())
            }
            _ => self.isStartOfExpression(),
        }
    }

    fn nextTokenIsBindingIdentifierOrStartOfDestructuring(&mut self) -> bool {
        self.nextToken();
        self.isBindingIdentifier()
            || self.token() == SyntaxKind::OpenBraceToken
            || self.token() == SyntaxKind::OpenBracketToken
    }

    fn isLetDeclaration(&mut self) -> bool {
        // In ES6 'let' always starts a lexical declaration if followed by an identifier or {
        // or [.
        self.lookAhead(|p| p.nextTokenIsBindingIdentifierOrStartOfDestructuring())
    }

    fn parseStatement(&mut self) -> Statement {
        match self.token() {
            SyntaxKind::SemicolonToken => return self.parseEmptyStatement(),
            SyntaxKind::OpenBraceToken => {
                return Statement::Block(Rc::new(self.parseBlock(false, None)));
            }
            SyntaxKind::VarKeyword => {
                return Statement::VariableStatement(Rc::new(self.parseVariableStatement(
                    self.getNodePos(),
                    self.hasPrecedingJSDocComment(),
                    None,
                    None,
                )));
            }
            SyntaxKind::LetKeyword => {
                if self.isLetDeclaration() {
                    return Statement::VariableStatement(Rc::new(self.parseVariableStatement(
                        self.getNodePos(),
                        self.hasPrecedingJSDocComment(),
                        None,
                        None,
                    )));
                }
            }
            SyntaxKind::FunctionKeyword => {
                return Statement::FunctionDeclaration(Rc::new(self.parseFunctionDeclaration(
                    self.getNodePos(),
                    self.hasPrecedingJSDocComment(),
                    None,
                    None,
                )));
            }
            SyntaxKind::ClassKeyword => {
                return Statement::ClassDeclaration(Rc::new(self.parseClassDeclaration(
                    self.getNodePos(),
                    self.hasPrecedingJSDocComment(),
                    None,
                    None,
                )));
            }
            SyntaxKind::IfKeyword => {
                return Statement::IfStatement(Rc::new(self.parseIfStatement()));
            }
            SyntaxKind::DoKeyword => {
                return Statement::DoStatement(Rc::new(self.parseDoStatement()));
            }
            SyntaxKind::WhileKeyword => {
                return Statement::WhileStatement(Rc::new(self.parseWhileStatement()));
            }
            SyntaxKind::ForKeyword => {
                return self.parseForOrForInOrForOfStatement();
            }
            SyntaxKind::ContinueKeyword => {
                return Statement::ContinueStatement(Rc::new(self.parseContinueStatement()));
            }
            SyntaxKind::BreakKeyword => {
                return Statement::BreakStatement(Rc::new(self.parseBreakStatement()));
            }
            SyntaxKind::ReturnKeyword => {
                return Statement::ReturnStatement(Rc::new(self.parseReturnStatement()));
            }
            SyntaxKind::WithKeyword => {
                return Statement::WithStatement(Rc::new(self.parseWithStatement()));
            }
            SyntaxKind::SwitchKeyword => {
                return Statement::SwitchStatement(Rc::new(self.parseSwitchStatement()));
            }
            SyntaxKind::ThrowKeyword => {
                return Statement::ThrowStatement(Rc::new(self.parseThrowStatement()));
            }
            // Include 'catch' and 'finally' for error recovery.
            SyntaxKind::TryKeyword | SyntaxKind::CatchKeyword | SyntaxKind::FinallyKeyword => {
                return Statement::TryStatement(Rc::new(self.parseTryStatement()));
            }
            SyntaxKind::DebuggerKeyword => {
                return Statement::DebuggerStatement(Rc::new(self.parseDebuggerStatement()));
            }
            SyntaxKind::AtToken => return self.parseDeclaration(),
            SyntaxKind::AsyncKeyword
            | SyntaxKind::InterfaceKeyword
            | SyntaxKind::TypeKeyword
            | SyntaxKind::ModuleKeyword
            | SyntaxKind::NamespaceKeyword
            | SyntaxKind::DeclareKeyword
            | SyntaxKind::ConstKeyword
            | SyntaxKind::EnumKeyword
            | SyntaxKind::ExportKeyword
            | SyntaxKind::ImportKeyword
            | SyntaxKind::PrivateKeyword
            | SyntaxKind::ProtectedKeyword
            | SyntaxKind::PublicKeyword
            | SyntaxKind::AbstractKeyword
            | SyntaxKind::StaticKeyword
            | SyntaxKind::ReadonlyKeyword
            | SyntaxKind::GlobalKeyword => {
                if self.isStartOfDeclaration() {
                    return self.parseDeclaration();
                }
            }
            _ => {}
        }
        self.parseExpressionOrLabeledStatement()
    }

    fn isDeclareModifier(&self, modifier: &Modifier) -> bool {
        modifier.kind() == SyntaxKind::DeclareKeyword
    }

    fn parseDeclaration(&mut self) -> Statement {
        // TODO: Can we hold onto the parsed decorators/modifiers and advance the scanner
        //       if we can't reuse the declaration, so that we don't do this work twice?
        //
        // `parseListElement` attempted to get the reused node at this position,
        // but the ambient context flag was not yet set, so the node appeared
        // not reusable in that context.
        let isAmbient = self
            .lookAhead(|p| {
                p.parseDecorators();
                p.parseModifiers(false, false)
            })
            .unwrap_or_default()
            .iter()
            .any(|m| self.isDeclareModifier(m));
        if isAmbient {
            todo!();
            // let node = self.tryReuseAmbientDeclaration();
            // if let Some(node) = node {
            //     return node;
            // }
        }

        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        let decorators = self.parseDecorators();
        let modifiers = self.parseModifiers(false, false);
        if isAmbient {
            if let Some(modifiers) = &modifiers {
                for m in modifiers.iter() {
                    self.factory.node_data_mut(m).flags |= NodeFlags::Ambient;
                }
            }

            self.doInsideOfContext(NodeFlags::Ambient, move |p| {
                p.parseDeclarationWorker(pos, hasJSDoc, decorators, modifiers)
            })
        } else {
            self.parseDeclarationWorker(pos, hasJSDoc, decorators, modifiers)
        }
    }

    //     function tryReuseAmbientDeclaration(): Statement | undefined {
    //         return doInsideOfContext(NodeFlags.Ambient, () => {
    //             const node = currentNode(parsingContext);
    //             if (node) {
    //                 return consumeNode(node) as Statement;
    //             }
    //         });
    //     }

    fn parseDeclarationWorker(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> Statement {
        match self.token() {
            SyntaxKind::VarKeyword | SyntaxKind::LetKeyword | SyntaxKind::ConstKeyword => {
                return Statement::VariableStatement(Rc::new(
                    self.parseVariableStatement(pos, hasJSDoc, decorators, modifiers),
                ));
            }
            SyntaxKind::FunctionKeyword => {
                return Statement::FunctionDeclaration(Rc::new(
                    self.parseFunctionDeclaration(pos, hasJSDoc, decorators, modifiers),
                ));
            }
            SyntaxKind::ClassKeyword => {
                return Statement::ClassDeclaration(Rc::new(
                    self.parseClassDeclaration(pos, hasJSDoc, decorators, modifiers),
                ));
            }
            SyntaxKind::InterfaceKeyword => {
                return Statement::InterfaceDeclaration(Rc::new(
                    self.parseInterfaceDeclaration(pos, hasJSDoc, decorators, modifiers),
                ));
            }
            SyntaxKind::TypeKeyword => {
                return Statement::TypeAliasDeclaration(Rc::new(
                    self.parseTypeAliasDeclaration(pos, hasJSDoc, decorators, modifiers),
                ));
            }
            SyntaxKind::EnumKeyword => {
                return Statement::EnumDeclaration(Rc::new(
                    self.parseEnumDeclaration(pos, hasJSDoc, decorators, modifiers),
                ));
            }
            SyntaxKind::GlobalKeyword
            | SyntaxKind::ModuleKeyword
            | SyntaxKind::NamespaceKeyword => {
                return Statement::ModuleDeclaration(Rc::new(
                    self.parseModuleDeclaration(pos, hasJSDoc, decorators, modifiers),
                ));
            }
            SyntaxKind::ImportKeyword => {
                return self.parseImportDeclarationOrImportEqualsDeclaration(
                    pos, hasJSDoc, decorators, modifiers,
                );
            }
            SyntaxKind::ExportKeyword => {
                self.nextToken();
                return match self.token() {
                    SyntaxKind::DefaultKeyword | SyntaxKind::EqualsToken => {
                        Statement::ExportAssignment(Rc::new(
                            self.parseExportAssignment(pos, hasJSDoc, decorators, modifiers),
                        ))
                    }
                    SyntaxKind::AsKeyword => Statement::NamespaceExportDeclaration(Rc::new(
                        self.parseNamespaceExportDeclaration(pos, hasJSDoc, decorators, modifiers),
                    )),
                    _ => Statement::ExportDeclaration(Rc::new(
                        self.parseExportDeclaration(pos, hasJSDoc, decorators, modifiers),
                    )),
                };
            }
            _ => {
                todo!();
                // if decorators.is_some() || modifiers.is_some() {
                //     // We reached this point because we encountered decorators and/or modifiers and assumed a declaration
                //     // would follow. For recovery and error reporting purposes, return an incomplete declaration.
                //     const missing = createMissingNode<MissingDeclaration>(SyntaxKind::MissingDeclaration, /*reportAtCurrentPosition*/ true, Diagnostics::Declaration_expected);
                //     setTextRangePos(missing, pos);
                //     missing.decorators = decorators;
                //     missing.modifiers = modifiers;
                //     return missing;
                // }
                // return undefined!; // TODO: GH#18217
            }
        }
    }

    fn nextTokenIsIdentifierOrStringLiteralOnSameLine(&mut self) -> bool {
        self.nextToken();
        !self.scanner.hasPrecedingLineBreak()
            && (self.isIdentifier() || self.token() == SyntaxKind::StringLiteral)
    }

    fn parseFunctionBlockOrSemicolon(
        &mut self,
        flags: SignatureFlags,
        diagnosticMessage: Option<DiagnosticMessage>,
    ) -> Option<Block> {
        if self.token() != SyntaxKind::OpenBraceToken && self.canParseSemicolon() {
            self.parseSemicolon();
            return None;
        }

        Some(self.parseFunctionBlock(flags, diagnosticMessage))
    }

    // DECLARATIONS

    fn parseArrayBindingElement(&mut self) -> ArrayBindingElement {
        let pos = self.getNodePos();
        if self.token() == SyntaxKind::CommaToken {
            let node = self.factory.createOmittedExpression();
            return ArrayBindingElement::OmittedExpression(Rc::new(
                self.finishNode(node, pos, None),
            ));
        }
        let dotDotDotToken = self.parseOptionalToken(SyntaxKind::DotDotDotToken);
        let name = self.parseIdentifierOrPattern(None);
        let initializer = self.parseInitializer();
        let node = self
            .factory
            .createBindingElement(dotDotDotToken, None, name, initializer);
        ArrayBindingElement::BindingElement(Rc::new(self.finishNode(node, pos, None)))
    }

    fn parseObjectBindingElement(&mut self) -> BindingElement {
        let pos = self.getNodePos();
        let dotDotDotToken = self.parseOptionalToken(SyntaxKind::DotDotDotToken);
        let tokenIsIdentifier = self.isBindingIdentifier();
        let mut propertyName = Some(self.parsePropertyName());
        let name;
        if tokenIsIdentifier && self.token() != SyntaxKind::ColonToken {
            name = BindingName::Identifier(unwrap_as!(
                propertyName,
                Some(PropertyName::Identifier(i)),
                i
            ));
            propertyName = None;
        } else {
            self.parseExpected(SyntaxKind::ColonToken, None, None);
            name = self.parseIdentifierOrPattern(None);
        }
        let initializer = self.parseInitializer();
        let node =
            self.factory
                .createBindingElement(dotDotDotToken, propertyName, name, initializer);
        self.finishNode(node, pos, None)
    }

    fn parseObjectBindingPattern(&mut self) -> ObjectBindingPattern {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::OpenBraceToken, None, None);
        let elements = self.parseDelimitedList(
            ParsingContext::ObjectBindingElements,
            |p| p.parseObjectBindingElement(),
            false,
        );
        self.parseExpected(SyntaxKind::CloseBraceToken, None, None);
        let node = self.factory.createObjectBindingPattern(elements);
        self.finishNode(node, pos, None)
    }

    fn parseArrayBindingPattern(&mut self) -> ArrayBindingPattern {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::OpenBracketToken, None, None);
        let elements = self.parseDelimitedList(
            ParsingContext::ArrayBindingElements,
            |p| p.parseArrayBindingElement(),
            false,
        );
        self.parseExpected(SyntaxKind::CloseBracketToken, None, None);
        let node = self.factory.createArrayBindingPattern(elements);
        self.finishNode(node, pos, None)
    }

    fn isBindingIdentifierOrPrivateIdentifierOrPattern(&self) -> bool {
        self.token() == SyntaxKind::OpenBraceToken
            || self.token() == SyntaxKind::OpenBracketToken
            || self.token() == SyntaxKind::PrivateIdentifier
            || self.isBindingIdentifier()
    }

    fn parseIdentifierOrPattern(
        &mut self,
        privateIdentifierDiagnosticMessage: Option<DiagnosticMessage>,
    ) -> BindingName {
        if self.token() == SyntaxKind::OpenBracketToken {
            return BindingName::ArrayBindingPattern(Rc::new(self.parseArrayBindingPattern()));
        }
        if self.token() == SyntaxKind::OpenBraceToken {
            return BindingName::ObjectBindingPattern(Rc::new(self.parseObjectBindingPattern()));
        }
        BindingName::Identifier(Rc::new(
            self.parseBindingIdentifier(privateIdentifierDiagnosticMessage),
        ))
    }

    fn parseVariableDeclarationAllowExclamation(&mut self) -> VariableDeclaration {
        self.parseVariableDeclaration(true)
    }

    fn parseVariableDeclaration(&mut self, allowExclamation: bool) -> VariableDeclaration {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        let name = self.parseIdentifierOrPattern(Some(
            Diagnostics::Private_identifiers_are_not_allowed_in_variable_declarations,
        ));
        let mut exclamationToken = None;
        if allowExclamation
            && matches!(name, BindingName::Identifier(_))
            && self.token() == SyntaxKind::ExclamationToken
            && !self.scanner.hasPrecedingLineBreak()
        {
            exclamationToken = Some(self.parseTokenNode());
        }
        let ty = self.parseTypeAnnotation();
        let initializer = if self.isInOrOfKeyword(self.token()) {
            None
        } else {
            self.parseInitializer()
        };
        let node = self
            .factory
            .createVariableDeclaration(name, exclamationToken, ty, initializer);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseVariableDeclarationList(
        &mut self,
        inForStatementInitializer: bool,
    ) -> VariableDeclarationList {
        let pos = self.getNodePos();

        let flags = match self.token() {
            SyntaxKind::VarKeyword => NodeFlags::empty(),
            SyntaxKind::LetKeyword => NodeFlags::Let,
            SyntaxKind::ConstKeyword => NodeFlags::Const,
            _ => unreachable!(),
        };

        self.nextToken();

        // The user may have written the following:
        //
        //    for (let of X) { }
        //
        // In this case, we want to parse an empty declaration list, and then parse 'of'
        // as a keyword. The reason this is not automatic is that 'of' is a valid identifier.
        // So we need to look ahead to determine if 'of' should be treated as a keyword in
        // this context.
        // The checker will then give an error that there is an empty declaration list.
        let declarations;
        if self.token() == SyntaxKind::OfKeyword
            && self.lookAhead(|p| p.canFollowContextualOfKeyword())
        {
            todo!();
            // declarations = self.createMissingList::<VariableDeclaration>();
        } else {
            let savedDisallowIn = self.inDisallowInContext();
            self.setDisallowInContext(inForStatementInitializer);

            declarations = self.parseDelimitedList(
                ParsingContext::VariableDeclarations,
                |p| {
                    if inForStatementInitializer {
                        p.parseVariableDeclaration(false)
                    } else {
                        p.parseVariableDeclarationAllowExclamation()
                    }
                },
                false,
            );

            self.setDisallowInContext(savedDisallowIn);
        }

        let node = self
            .factory
            .createVariableDeclarationList(declarations, Some(flags));
        self.finishNode(node, pos, None)
    }

    fn canFollowContextualOfKeyword(&mut self) -> bool {
        self.nextTokenIsIdentifier() && self.nextToken() == SyntaxKind::CloseParenToken
    }

    fn parseVariableStatement(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> VariableStatement {
        let declarationList = self.parseVariableDeclarationList(false);
        self.parseSemicolon();
        let mut node = self
            .factory
            .createVariableStatement(modifiers, declarationList);
        // Decorators are not allowed on a variable statement, so we keep track of them to report them in the grammar checker.
        node.decorators = decorators;
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseFunctionDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> FunctionDeclaration {
        let savedAwaitContext = self.inAwaitContext();

        let modifierFlags = modifiersToFlags(modifiers.as_ref());
        self.parseExpected(SyntaxKind::FunctionKeyword, None, None);
        let asteriskToken = self.parseOptionalToken(SyntaxKind::AsteriskToken);
        // We don't parse the name here in await context, instead we will report a grammar error in the checker.
        let name = if modifierFlags.intersects(ModifierFlags::Default) {
            self.parseOptionalBindingIdentifier()
        } else {
            Some(self.parseBindingIdentifier(None))
        };
        let isGenerator = if asteriskToken.is_some() {
            SignatureFlags::Yield
        } else {
            SignatureFlags::None
        };
        let isAsync = if modifierFlags.intersects(ModifierFlags::Async) {
            SignatureFlags::Await
        } else {
            SignatureFlags::None
        };
        let typeParameters = self.parseTypeParameters();
        if modifierFlags.intersects(ModifierFlags::Export) {
            self.setAwaitContext(true);
        }
        let parameters = self.parseParameters(isGenerator | isAsync);
        let ty = self.parseReturnType(SyntaxKind::ColonToken, false);
        let body = self
            .parseFunctionBlockOrSemicolon(isGenerator | isAsync, Some(Diagnostics::or_expected));
        self.setAwaitContext(savedAwaitContext);
        let node = self.factory.createFunctionDeclaration(
            decorators,
            modifiers,
            asteriskToken,
            name.map(Rc::new),
            typeParameters,
            parameters,
            ty,
            body,
        );
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseConstructorName(&mut self) -> bool {
        if self.token() == SyntaxKind::ConstructorKeyword {
            return self.parseExpected(SyntaxKind::ConstructorKeyword, None, None);
        }
        if self.token() == SyntaxKind::StringLiteral
            && self.lookAhead(|p| Some(p.nextToken())) == Some(SyntaxKind::OpenParenToken)
        {
            return self.tryParse(|p| {
                let literalNode = p.parseStringLiteral();
                if literalNode.text.as_ref() == "constructor" {
                    true
                } else {
                    false
                }
            });
        }
        false
    }

    fn tryParseConstructorDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> Option<ConstructorDeclaration> {
        self.tryParse(|p| {
            if p.parseConstructorName() {
                let typeParameters = p.parseTypeParameters();
                let parameters = p.parseParameters(SignatureFlags::None);
                let ty = p.parseReturnType(SyntaxKind::ColonToken, false);
                let body = p.parseFunctionBlockOrSemicolon(
                    SignatureFlags::None,
                    Some(Diagnostics::or_expected),
                );
                let mut node = p
                    .factory
                    .createConstructorDeclaration(decorators, modifiers, parameters, body);
                // Attach `typeParameters` and `type` if they exist so that we can report them in the grammar checker.
                node.typeParameters = typeParameters;
                node.ty = ty;
                let node = p.finishNode(node, pos, None);
                Some(p.withJSDoc(node, hasJSDoc))
            } else {
                None
            }
        })
    }

    fn parseMethodDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
        asteriskToken: Option<AsteriskToken>,
        name: PropertyName,
        questionToken: Option<QuestionToken>,
        exclamationToken: Option<ExclamationToken>,
        diagnosticMessage: Option<DiagnosticMessage>,
    ) -> MethodDeclaration {
        let isGenerator = if asteriskToken.is_some() {
            SignatureFlags::Yield
        } else {
            SignatureFlags::None
        };
        let isAsync = modifiers
            .as_ref()
            .map(|m| m.iter().any(|m| isAsyncModifier(m)))
            .unwrap_or_default();
        let isAsync = if isAsync {
            SignatureFlags::Await
        } else {
            SignatureFlags::None
        };
        let typeParameters = self.parseTypeParameters();
        let parameters = self.parseParameters(isGenerator | isAsync);
        let ty = self.parseReturnType(SyntaxKind::ColonToken, false);
        let body = self.parseFunctionBlockOrSemicolon(isGenerator | isAsync, diagnosticMessage);
        let mut node = self.factory.createMethodDeclaration(
            decorators,
            modifiers,
            asteriskToken,
            name,
            questionToken,
            typeParameters,
            parameters,
            ty,
            body,
        );
        // An exclamation token on a method is invalid syntax and will be handled by the grammar checker
        node.exclamationToken = exclamationToken;
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parsePropertyDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
        name: PropertyName,
        questionToken: Option<QuestionToken>,
    ) -> PropertyDeclaration {
        let exclamationToken = if questionToken.is_none() && !self.scanner.hasPrecedingLineBreak() {
            self.parseOptionalToken(SyntaxKind::ExclamationToken)
        } else {
            None
        };
        let ty = self.parseTypeAnnotation();
        let initializer = self.doOutsideOfContext(
            NodeFlags::YieldContext | NodeFlags::AwaitContext | NodeFlags::DisallowInContext,
            |p| p.parseInitializer(),
        );
        self.parseSemicolonAfterPropertyName(&name, &ty, &initializer);
        let node = self.factory.createPropertyDeclaration(
            decorators,
            modifiers,
            name,
            questionToken,
            exclamationToken,
            ty,
            initializer,
        );
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parsePropertyOrMethodDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> ClassElement {
        let asteriskToken = self.parseOptionalToken(SyntaxKind::AsteriskToken);
        let name = self.parsePropertyName();
        // Note: this is not legal as per the grammar.  But we allow it in the parser and
        // report an error in the grammar checker.
        let questionToken = self.parseOptionalToken(SyntaxKind::QuestionToken);
        if asteriskToken.is_some()
            || self.token() == SyntaxKind::OpenParenToken
            || self.token() == SyntaxKind::LessThanToken
        {
            return ClassElement::MethodDeclaration(Rc::new(self.parseMethodDeclaration(
                pos,
                hasJSDoc,
                decorators,
                modifiers,
                asteriskToken,
                name,
                questionToken,
                None,
                Some(Diagnostics::or_expected),
            )));
        }
        ClassElement::PropertyDeclaration(Rc::new(self.parsePropertyDeclaration(
            pos,
            hasJSDoc,
            decorators,
            modifiers,
            name,
            questionToken,
        )))
    }

    fn parseGetAccessorDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> GetAccessorDeclaration {
        let name = self.parsePropertyName();
        let typeParameters = self.parseTypeParameters();
        let parameters = self.parseParameters(SignatureFlags::None);
        let ty = self.parseReturnType(SyntaxKind::ColonToken, false);
        let body = self.parseFunctionBlockOrSemicolon(SignatureFlags::None, None);
        let mut node = self
            .factory
            .createGetAccessorDeclaration(decorators, modifiers, name, parameters, ty, body);
        // Keep track of `typeParameters` if they were parsed those indicate grammar errors
        node.typeParameters = typeParameters;
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseSetAccessorDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> SetAccessorDeclaration {
        let name = self.parsePropertyName();
        let typeParameters = self.parseTypeParameters();
        let parameters = self.parseParameters(SignatureFlags::None);
        let ty = self.parseReturnType(SyntaxKind::ColonToken, false);
        let body = self.parseFunctionBlockOrSemicolon(SignatureFlags::None, None);
        let mut node = self
            .factory
            .createSetAccessorDeclaration(decorators, modifiers, name, parameters, body);
        // Keep track of `typeParameters` (for both) and `type` (for setters) if they were parsed those indicate grammar errors
        node.typeParameters = typeParameters;
        if ty.is_some() {
            node.ty = ty;
        }
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn isClassMemberStart(&mut self) -> bool {
        let mut idToken = None;

        if self.token() == SyntaxKind::AtToken {
            return true;
        }

        // Eat up all modifiers, but hold on to the last one in it is actually an identifier.
        while isModifierKind(self.token()) {
            idToken = Some(self.token());
            // If the idToken is a class modifier (protected, private, public, and static), it is
            // certain that we are starting to parse class member. This allows better error recovery
            // Example:
            //      public foo() ...     // true
            //      public @dec blah ... // true; we will then report an error later
            //      export public ...    // true; we will then report an error later
            if isClassMemberModifier(self.token()) {
                return true;
            }

            self.nextToken();
        }

        if self.token() == SyntaxKind::AsteriskToken {
            return true;
        }

        // Try to get the first property-like token following all modifiers.
        // This can either be an identifier or the 'get' or 'set' keywords.
        if self.isLiteralPropertyName() {
            idToken = Some(self.token());
            self.nextToken();
        }

        // Index signatures and computed properties are class members; we can parse.
        if self.token() == SyntaxKind::OpenBracketToken {
            return true;
        }

        // If we were able to get any potential identifier...
        if let Some(idToken) = idToken {
            // If we have a non-keyword identifier, or if we have an accessor, then it's safe to parse.
            if !isKeyword(idToken)
                || idToken == SyntaxKind::SetKeyword
                || idToken == SyntaxKind::GetKeyword
            {
                return true;
            }

            // If it *is* a keyword, but not an accessor, check a little farther along
            // to see if it should actually be parsed as a class member.
            match self.token() {
                    SyntaxKind::OpenParenToken |     // Method declaration
                    SyntaxKind::LessThanToken |      // Generic Method declaration
                    SyntaxKind::ExclamationToken |   // Non-null assertion on property name
                    SyntaxKind::ColonToken |         // Type Annotation for declaration
                    SyntaxKind::EqualsToken |        // Initializer for declaration
                    SyntaxKind::QuestionToken => {   // Not valid, but permitted so that it gets caught later on.
                        return true;
                    }
                    _ => {
                        // Covers
                        //  - Semicolons     (declaration termination)
                        //  - Closing braces (end-of-class, must be declaration)
                        //  - End-of-files   (not valid, but permitted so that it gets caught later on)
                        //  - Line-breaks    (enabling *automatic semicolon insertion*)
                        return self.canParseSemicolon();
                    }
                }
        }

        false
    }

    fn parseClassStaticBlockDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<ModifiersArray>,
    ) -> ClassStaticBlockDeclaration {
        self.parseExpectedToken::<StaticKeyword, u8>(SyntaxKind::StaticKeyword, None, None);
        let body = self.parseClassStaticBlockBody();
        let node = self
            .factory
            .createClassStaticBlockDeclaration(decorators, modifiers, body);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseClassStaticBlockBody(&mut self) -> Block {
        let savedYieldContext = self.inYieldContext();
        let savedAwaitContext = self.inAwaitContext();

        self.setYieldContext(false);
        self.setAwaitContext(true);

        let body = self.parseBlock(false, None);

        self.setYieldContext(savedYieldContext);
        self.setAwaitContext(savedAwaitContext);

        body
    }

    fn parseDecoratorExpression(&mut self) -> LeftHandSideExpression {
        if self.inAwaitContext() && self.token() == SyntaxKind::AwaitKeyword {
            // `@await` is is disallowed in an [Await] context, but can cause parsing to go off the rails
            // This simply parses the missing identifier and moves on.
            let pos = self.getNodePos();
            let awaitExpression = LeftHandSideExpression::Identifier(Rc::new(
                self.parseIdentifier(Some(Diagnostics::Expression_expected), None),
            ));
            self.nextToken();
            let memberExpression = self.parseMemberExpressionRest(pos, awaitExpression, true);
            return self.parseCallExpressionRest(pos, memberExpression.into());
        }
        self.parseLeftHandSideExpressionOrHigher()
    }

    fn tryParseDecorator(&mut self) -> Option<Decorator> {
        let pos = self.getNodePos();
        if !self.parseOptional(SyntaxKind::AtToken) {
            return None;
        }
        let expression = self.doInDecoratorContext(|p| p.parseDecoratorExpression());
        let node = self.factory.createDecorator(expression);
        Some(self.finishNode(node, pos, None))
    }

    fn parseDecorators(&mut self) -> Option<NodeArray<Decorator>> {
        let pos = self.getNodePos();
        let mut list = Vec::new();
        while let Some(decorator) = self.tryParseDecorator() {
            list.push(decorator);
        }
        if !list.is_empty() {
            Some(self.createNodeArray(list, pos, None, false))
        } else {
            None
        }
    }

    fn tryParseModifier(
        &mut self,
        permitInvalidConstAsModifier: bool,
        stopOnStartOfClassStaticBlock: bool,
        hasSeenStaticModifier: bool,
    ) -> Option<Modifier> {
        let pos = self.getNodePos();
        let kind = self.token();

        if self.token() == SyntaxKind::ConstKeyword && permitInvalidConstAsModifier {
            // We need to ensure that any subsequent modifiers appear on the same line
            // so that when 'const' is a standalone declaration, we don't issue an error.
            if !self.tryParse(|p| p.nextTokenIsOnSameLineAndCanFollowModifier()) {
                return None;
            }
        } else if stopOnStartOfClassStaticBlock
            && self.token() == SyntaxKind::StaticKeyword
            && self.lookAhead(|p| p.nextTokenIsOpenBrace())
        {
            return None;
        } else if hasSeenStaticModifier && self.token() == SyntaxKind::StaticKeyword {
            return None;
        } else {
            if !self.parseAnyContextualModifier() {
                return None;
            }
        }

        let node = self.factory.createToken(kind);
        Some(self.finishNode(node, pos, None))
    }

    /*
     * There are situations in which a modifier like 'const' will appear unexpectedly, such as on a class member.
     * In those situations, if we are entirely sure that 'const' is not valid on its own (such as when ASI takes effect
     * and turns it into a standalone declaration), then it is better to parse it and report an error later.
     *
     * In such situations, 'permitInvalidConstAsModifier' should be set to true.
     */
    fn parseModifiers(
        &mut self,
        permitInvalidConstAsModifier: bool,
        stopOnStartOfClassStaticBlock: bool,
    ) -> Option<NodeArray<Modifier>> {
        let pos = self.getNodePos();
        let mut list = Vec::new();
        let mut hasSeenStatic = false;
        while let Some(modifier) = self.tryParseModifier(
            permitInvalidConstAsModifier,
            stopOnStartOfClassStaticBlock,
            hasSeenStatic,
        ) {
            if modifier.kind() == SyntaxKind::StaticKeyword {
                hasSeenStatic = true;
            }
            list.push(modifier);
        }
        if !list.is_empty() {
            Some(self.createNodeArray(list, pos, None, false))
        } else {
            None
        }
    }

    fn parseModifiersForArrowFunction(&mut self) -> Option<NodeArray<Modifier>> {
        if self.token() == SyntaxKind::AsyncKeyword {
            let pos = self.getNodePos();
            self.nextToken();
            let modifier = self.factory.createToken(SyntaxKind::AsyncKeyword);
            let modifier = self.finishNode(modifier, pos, None);
            Some(self.createNodeArray(vec![modifier], pos, None, false))
        } else {
            None
        }
    }

    fn parseClassElement(&mut self) -> ClassElement {
        let pos = self.getNodePos();
        if self.token() == SyntaxKind::SemicolonToken {
            self.nextToken();
            let node = self.factory.createSemicolonClassElement();
            return ClassElement::SemicolonClassElement(Rc::new(self.finishNode(node, pos, None)));
        }

        let hasJSDoc = self.hasPrecedingJSDocComment();
        let mut decorators = self.parseDecorators();
        let mut modifiers = self.parseModifiers(true, true);
        if self.token() == SyntaxKind::StaticKeyword && self.lookAhead(|p| p.nextTokenIsOpenBrace())
        {
            return ClassElement::ClassStaticBlockDeclaration(Rc::new(
                self.parseClassStaticBlockDeclaration(pos, hasJSDoc, decorators, modifiers),
            ));
        }

        if self.parseContextualModifier(SyntaxKind::GetKeyword) {
            return ClassElement::GetAccessorDeclaration(Rc::new(
                self.parseGetAccessorDeclaration(pos, hasJSDoc, decorators, modifiers),
            ));
        }

        if self.parseContextualModifier(SyntaxKind::SetKeyword) {
            return ClassElement::SetAccessorDeclaration(Rc::new(
                self.parseSetAccessorDeclaration(pos, hasJSDoc, decorators, modifiers),
            ));
        }

        if self.token() == SyntaxKind::ConstructorKeyword
            || self.token() == SyntaxKind::StringLiteral
        {
            // Inlined `tryParseConstructorDeclaration` to satisfy borrow checker.
            let constructorDeclaration = self.tryParse(|p| {
                if p.parseConstructorName() {
                    let typeParameters = p.parseTypeParameters();
                    let parameters = p.parseParameters(SignatureFlags::None);
                    let ty = p.parseReturnType(SyntaxKind::ColonToken, false);
                    let body = p.parseFunctionBlockOrSemicolon(
                        SignatureFlags::None,
                        Some(Diagnostics::or_expected),
                    );
                    let mut node = p.factory.createConstructorDeclaration(
                        decorators.take(),
                        modifiers.take(),
                        parameters,
                        body,
                    );
                    // Attach `typeParameters` and `type` if they exist so that we can report them in the grammar checker.
                    node.typeParameters = typeParameters;
                    node.ty = ty;
                    let node = p.finishNode(node, pos, None);
                    Some(p.withJSDoc(node, hasJSDoc))
                } else {
                    None
                }
            });

            if let Some(constructorDeclaration) = constructorDeclaration {
                return ClassElement::ConstructorDeclaration(Rc::new(constructorDeclaration));
            }
        }

        if self.isIndexSignature() {
            return ClassElement::IndexSignatureDeclaration(Rc::new(
                self.parseIndexSignatureDeclaration(pos, hasJSDoc, decorators, modifiers),
            ));
        }

        // It is very important that we check this *after* checking indexers because
        // the [ token can start an index signature or a computed property name
        if tokenIsIdentifierOrKeyword(self.token())
            || self.token() == SyntaxKind::StringLiteral
            || self.token() == SyntaxKind::NumericLiteral
            || self.token() == SyntaxKind::AsteriskToken
            || self.token() == SyntaxKind::OpenBracketToken
        {
            let isAmbient = modifiers
                .as_ref()
                .map(|m| m.iter().any(|m| self.isDeclareModifier(m)))
                .unwrap_or_default();
            if isAmbient {
                for m in modifiers.as_ref().unwrap().iter() {
                    self.factory.node_data_mut(m).flags |= NodeFlags::Ambient;
                }
                return self.doInsideOfContext(NodeFlags::Ambient, |p| {
                    p.parsePropertyOrMethodDeclaration(pos, hasJSDoc, decorators, modifiers)
                });
            } else {
                return self.parsePropertyOrMethodDeclaration(pos, hasJSDoc, decorators, modifiers);
            }
        }

        if decorators.is_some() || modifiers.is_some() {
            todo!();
            // treat this as a property declaration with a missing name.
            // let name = createMissingNode::<Identifier>(SyntaxKind::Identifier, /*reportAtCurrentPosition*/ true, Diagnostics::Declaration_expected);
            // return self.parsePropertyDeclaration(pos, hasJSDoc, decorators, modifiers, name, /*questionToken*/ undefined);
        }

        // 'isClassMemberStart' should have hinted not to attempt parsing.
        unreachable!("Should not have attempted to parse class member declaration.");
    }

    fn parseClassExpression(&mut self) -> ClassExpression {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        let savedAwaitContext = self.inAwaitContext();
        self.parseExpected(SyntaxKind::ClassKeyword, None, None);

        // We don't parse the name here in await context, instead we will report a grammar error in the checker.
        let name = self.parseNameOfClassDeclarationOrExpression();
        let typeParameters = self.parseTypeParameters();
        let heritageClauses = self.parseHeritageClauses();

        let members;
        if self.parseExpected(SyntaxKind::OpenBraceToken, None, None) {
            // ClassTail[Yield,Await] : (Modified) See 14.5
            //      ClassHeritage[?Yield,?Await]opt { ClassBody[?Yield,?Await]opt }
            members = self.parseClassMembers();
            self.parseExpected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            todo!();
            // members = self.createMissingList::<ClassElement>();
        }
        self.setAwaitContext(savedAwaitContext);
        let node = self.factory.createClassExpression(
            None,
            None,
            name.map(Rc::new),
            typeParameters,
            heritageClauses,
            members,
        );
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseClassDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> ClassDeclaration {
        let savedAwaitContext = self.inAwaitContext();
        self.parseExpected(SyntaxKind::ClassKeyword, None, None);

        // We don't parse the name here in await context, instead we will report a grammar error in the checker.
        let name = self.parseNameOfClassDeclarationOrExpression();
        let typeParameters = self.parseTypeParameters();
        let has_export_modifier = modifiers
            .as_ref()
            .map(|m| m.iter().any(|m| isExportModifier(m)))
            .unwrap_or_default();
        if has_export_modifier {
            self.setAwaitContext(true);
        }
        let heritageClauses = self.parseHeritageClauses();

        let members;
        if self.parseExpected(SyntaxKind::OpenBraceToken, None, None) {
            // ClassTail[Yield,Await] : (Modified) See 14.5
            //      ClassHeritage[?Yield,?Await]opt { ClassBody[?Yield,?Await]opt }
            members = self.parseClassMembers();
            self.parseExpected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            todo!();
            // members = self.createMissingList::<ClassElement>();
        }
        self.setAwaitContext(savedAwaitContext);
        let node = self.factory.createClassDeclaration(
            decorators,
            modifiers,
            name.map(Rc::new),
            typeParameters,
            heritageClauses,
            members,
        );
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseNameOfClassDeclarationOrExpression(&mut self) -> Option<Identifier> {
        // implements is a future reserved word so
        // 'class implements' might mean either
        // - class expression with omitted name, 'implements' starts heritage clause
        // - class with name 'implements'
        // 'isImplementsClause' helps to disambiguate between these two cases
        if self.isBindingIdentifier() && !self.isImplementsClause() {
            Some(self.createIdentifier(self.isBindingIdentifier(), None, None))
        } else {
            None
        }
    }

    fn isImplementsClause(&mut self) -> bool {
        self.token() == SyntaxKind::ImplementsKeyword
            && self.lookAhead(|p| p.nextTokenIsIdentifierOrKeyword())
    }

    fn parseHeritageClauses(&mut self) -> Option<NodeArray<HeritageClause>> {
        // ClassTail[Yield,Await] : (Modified) See 14.5
        //      ClassHeritage[?Yield,?Await]opt { ClassBody[?Yield,?Await]opt }

        if self.isHeritageClause() {
            return Some(
                self.parseList(ParsingContext::HeritageClauses, |p| p.parseHeritageClause()),
            );
        }

        None
    }

    fn parseHeritageClause(&mut self) -> HeritageClause {
        let pos = self.getNodePos();
        let tok = self.token();
        debug_assert!(tok == SyntaxKind::ExtendsKeyword || tok == SyntaxKind::ImplementsKeyword); // isListElement() should ensure this.
        self.nextToken();
        let types = self.parseDelimitedList(
            ParsingContext::HeritageClauseElement,
            |p| p.parseExpressionWithTypeArguments(),
            false,
        );
        let node = self.factory.createHeritageClause(tok, types);
        self.finishNode(node, pos, None)
    }

    fn parseExpressionWithTypeArguments(&mut self) -> ExpressionWithTypeArguments {
        let pos = self.getNodePos();
        let expression = self.parseLeftHandSideExpressionOrHigher();
        let typeArguments = self.tryParseTypeArguments();
        let node = self
            .factory
            .createExpressionWithTypeArguments(expression, typeArguments);
        self.finishNode(node, pos, None)
    }

    fn tryParseTypeArguments(&mut self) -> Option<NodeArray<TypeNode>> {
        if self.token() == SyntaxKind::LessThanToken {
            Some(self.parseBracketedList(
                ParsingContext::TypeArguments,
                |p| p.parseType(),
                SyntaxKind::LessThanToken,
                SyntaxKind::GreaterThanToken,
            ))
        } else {
            None
        }
    }

    fn isHeritageClause(&self) -> bool {
        self.token() == SyntaxKind::ExtendsKeyword || self.token() == SyntaxKind::ImplementsKeyword
    }

    fn parseClassMembers(&mut self) -> NodeArray<ClassElement> {
        self.parseList(ParsingContext::ClassMembers, |p| p.parseClassElement())
    }

    fn parseInterfaceDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> InterfaceDeclaration {
        self.parseExpected(SyntaxKind::InterfaceKeyword, None, None);
        let name = self.parseIdentifier(None, None);
        let typeParameters = self.parseTypeParameters();
        let heritageClauses = self.parseHeritageClauses();
        let members = self.parseObjectTypeMembers();
        let node = self.factory.createInterfaceDeclaration(
            decorators,
            modifiers,
            name,
            typeParameters,
            heritageClauses,
            members,
        );
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseTypeAliasDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> TypeAliasDeclaration {
        self.parseExpected(SyntaxKind::TypeKeyword, None, None);
        let name = self.parseIdentifier(None, None);
        let typeParameters = self.parseTypeParameters();
        self.parseExpected(SyntaxKind::EqualsToken, None, None);
        let ty = if self.token() == SyntaxKind::IntrinsicKeyword {
            self.tryParse(|p| p.parseKeywordAndNoDot())
                .map(Rc::new)
                .map(TypeNode::IntrinsicKeyword)
        } else {
            None
        };
        let ty = ty.unwrap_or_else(|| self.parseType());
        self.parseSemicolon();
        let node = self.factory.createTypeAliasDeclaration(
            decorators,
            modifiers,
            name,
            typeParameters,
            ty,
        );
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    // In an ambient declaration, the grammar only allows integer literals as initializers.
    // In a non-ambient declaration, the grammar allows uninitialized members only in a
    // ConstantEnumMemberSection, which starts at the beginning of an enum declaration
    // or any time an integer literal initializer is encountered.
    fn parseEnumMember(&mut self) -> EnumMember {
        let pos = self.getNodePos();
        let hasJSDoc = self.hasPrecedingJSDocComment();
        let name = self.parsePropertyName();
        let initializer = self.allowInAnd(|p| p.parseInitializer());
        let node = self.factory.createEnumMember(name, initializer);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseEnumDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> EnumDeclaration {
        self.parseExpected(SyntaxKind::EnumKeyword, None, None);
        let name = self.parseIdentifier(None, None);
        let members;
        if self.parseExpected(SyntaxKind::OpenBraceToken, None, None) {
            members = self.doOutsideOfYieldAndAwaitContext(|p| {
                p.parseDelimitedList(ParsingContext::EnumMembers, |p| p.parseEnumMember(), false)
            });
            self.parseExpected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            todo!();
            // members = createMissingList<EnumMember>();
        }
        let node = self
            .factory
            .createEnumDeclaration(decorators, modifiers, name, members);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseModuleBlock(&mut self) -> ModuleBlock {
        let pos = self.getNodePos();
        let statements;
        if self.parseExpected(SyntaxKind::OpenBraceToken, None, None) {
            statements = self.parseList(ParsingContext::BlockStatements, |p| p.parseStatement());
            self.parseExpected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            todo!();
            // statements = self.createMissingList<Statement>();
        }
        let node = self.factory.createModuleBlock(statements);
        self.finishNode(node, pos, None)
    }

    fn parseModuleOrNamespaceDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
        flags: NodeFlags,
    ) -> ModuleDeclaration {
        // If we are parsing a dotted namespace name, we want to
        // propagate the 'Namespace' flag across the names if set.
        let namespaceFlag = flags & NodeFlags::Namespace;
        let name = self.parseIdentifier(None, None);
        let body = if self.parseOptional(SyntaxKind::DotToken) {
            todo!();
            // self.parseModuleOrNamespaceDeclaration(
            //     self.getNodePos(),
            //     false,
            //     None,
            //     None,
            //     NodeFlags::NestedNamespace | namespaceFlag,
            // ) as NamespaceDeclaration
        } else {
            self.parseModuleBlock()
        };
        let node = self.factory.createModuleDeclaration(
            decorators,
            modifiers,
            ModuleName::Identifier(Rc::new(name)),
            Some(body),
            Some(flags),
        );
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseAmbientExternalModuleDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> ModuleDeclaration {
        let mut flags = NodeFlags::empty();
        let name = if self.token() == SyntaxKind::GlobalKeyword {
            // parse 'global' as name of global scope augmentation
            flags |= NodeFlags::GlobalAugmentation;
            ModuleName::Identifier(Rc::new(self.parseIdentifier(None, None)))
        } else {
            ModuleName::StringLiteral(Rc::new(self.parseStringLiteral()))
        };
        let mut body = None;
        if self.token() == SyntaxKind::OpenBraceToken {
            body = Some(self.parseModuleBlock());
        } else {
            self.parseSemicolon();
        }
        let node =
            self.factory
                .createModuleDeclaration(decorators, modifiers, name, body, Some(flags));
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseModuleDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> ModuleDeclaration {
        let mut flags = NodeFlags::empty();
        if self.token() == SyntaxKind::GlobalKeyword {
            // global augmentation
            return self
                .parseAmbientExternalModuleDeclaration(pos, hasJSDoc, decorators, modifiers);
        } else if self.parseOptional(SyntaxKind::NamespaceKeyword) {
            flags |= NodeFlags::Namespace;
        } else {
            self.parseExpected(SyntaxKind::ModuleKeyword, None, None);
            if self.token() == SyntaxKind::StringLiteral {
                return self
                    .parseAmbientExternalModuleDeclaration(pos, hasJSDoc, decorators, modifiers);
            }
        }
        self.parseModuleOrNamespaceDeclaration(pos, hasJSDoc, decorators, modifiers, flags)
    }

    fn isExternalModuleReference(&mut self) -> bool {
        self.token() == SyntaxKind::RequireKeyword && self.lookAhead(|p| p.nextTokenIsOpenParen())
    }

    fn nextTokenIsOpenParen(&mut self) -> bool {
        self.nextToken() == SyntaxKind::OpenParenToken
    }

    fn nextTokenIsOpenBrace(&mut self) -> bool {
        self.nextToken() == SyntaxKind::OpenBraceToken
    }

    fn nextTokenIsSlash(&mut self) -> bool {
        self.nextToken() == SyntaxKind::SlashToken
    }

    fn parseNamespaceExportDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> NamespaceExportDeclaration {
        self.parseExpected(SyntaxKind::AsKeyword, None, None);
        self.parseExpected(SyntaxKind::NamespaceKeyword, None, None);
        let name = self.parseIdentifier(None, None);
        self.parseSemicolon();
        let mut node = self.factory.createNamespaceExportDeclaration(name);
        // NamespaceExportDeclaration nodes cannot have decorators or modifiers, so we attach them here so we can report them in the grammar checker
        node.decorators = decorators;
        node.modifiers = modifiers;
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseImportDeclarationOrImportEqualsDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> Statement {
        self.parseExpected(SyntaxKind::ImportKeyword, None, None);

        let afterImportPos = self.scanner.getStartPos();

        // We don't parse the identifier here in await context, instead we will report a grammar error in the checker.
        let mut identifier = None;
        if self.isIdentifier() {
            identifier = Some(self.parseIdentifier(None, None));
        }

        let mut isTypeOnly = false;
        if self.token() != SyntaxKind::FromKeyword &&
                // identifier?.escapedText == "type" &&
                (self.isIdentifier() || self.tokenAfterImportDefinitelyProducesImportDeclaration())
        {
            isTypeOnly = true;
            identifier = if self.isIdentifier() {
                Some(self.parseIdentifier(None, None))
            } else {
                None
            };
        }

        if !self.tokenAfterImportedIdentifierDefinitelyProducesImportDeclaration() {
            if let Some(identifier) = identifier {
                return Statement::ImportEqualsDeclaration(Rc::new(
                    self.parseImportEqualsDeclaration(
                        pos, hasJSDoc, decorators, modifiers, identifier, isTypeOnly,
                    ),
                ));
            }
        }

        // ImportDeclaration:
        //  import ImportClause from ModuleSpecifier ;
        //  import ModuleSpecifier;
        let mut importClause = None;
        if identifier.is_some() || // import id
                self.token() == SyntaxKind::AsteriskToken || // import *
                self.token() == SyntaxKind::OpenBraceToken
        // import {
        {
            importClause = Some(self.parseImportClause(identifier, afterImportPos, isTypeOnly));
            self.parseExpected(SyntaxKind::FromKeyword, None, None);
        }
        let moduleSpecifier = self.parseModuleSpecifier();

        let mut assertClause = None;
        if self.token() == SyntaxKind::AssertKeyword && !self.scanner.hasPrecedingLineBreak() {
            assertClause = Some(self.parseAssertClause());
        }

        self.parseSemicolon();
        let node = self.factory.createImportDeclaration(
            decorators,
            modifiers,
            importClause,
            moduleSpecifier,
            assertClause,
        );
        let node = self.finishNode(node, pos, None);
        Statement::ImportDeclaration(Rc::new(self.withJSDoc(node, hasJSDoc)))
    }

    fn parseAssertEntry(&mut self) -> AssertEntry {
        let pos = self.getNodePos();
        let name = if tokenIsIdentifierOrKeyword(self.token()) {
            AssertionKey::Identifier(Rc::new(self.parseIdentifierName(None)))
        } else {
            AssertionKey::StringLiteral(Rc::new(self.parseStringLiteral()))
        };
        self.parseExpected(SyntaxKind::ColonToken, None, None);
        let value = self.parseStringLiteral();
        let node = self.factory.createAssertEntry(name, value);
        self.finishNode(node, pos, None)
    }

    fn parseAssertClause(&mut self) -> AssertClause {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::AssertKeyword, None, None);
        let openBracePosition = self.scanner.getTokenPos();
        self.parseExpected(SyntaxKind::OpenBraceToken, None, None);
        let multiLine = self.scanner.hasPrecedingLineBreak();
        let elements = self.parseDelimitedList(
            ParsingContext::AssertEntries,
            |p| p.parseAssertEntry(),
            true,
        );
        if !self.parseExpected(SyntaxKind::CloseBraceToken, None, None) {
            if let Some(lastError) = self.parseDiagnostics.last() {
                if lastError.code == Diagnostics::_0_expected.code {
                    todo!();
                    // self.addRelatedInfo(
                    //     lastError,
                    //     self.createDetachedDiagnostic(
                    //         self.fileName,
                    //         openBracePosition,
                    //         1,
                    //         Diagnostics::The_parser_expected_to_find_a_to_match_the_token_here,
                    //     ),
                    // );
                }
            }
        }
        let node = self.factory.createAssertClause(elements, Some(multiLine));
        self.finishNode(node, pos, None)
    }

    fn tokenAfterImportDefinitelyProducesImportDeclaration(&self) -> bool {
        self.token() == SyntaxKind::AsteriskToken || self.token() == SyntaxKind::OpenBraceToken
    }

    fn tokenAfterImportedIdentifierDefinitelyProducesImportDeclaration(&self) -> bool {
        // In `import id ___`, the current token decides whether to produce
        // an ImportDeclaration or ImportEqualsDeclaration.
        self.token() == SyntaxKind::CommaToken || self.token() == SyntaxKind::FromKeyword
    }

    fn parseImportEqualsDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
        identifier: Identifier,
        isTypeOnly: bool,
    ) -> ImportEqualsDeclaration {
        self.parseExpected(SyntaxKind::EqualsToken, None, None);
        let moduleReference = self.parseModuleReference();
        self.parseSemicolon();
        let node = self.factory.createImportEqualsDeclaration(
            decorators,
            modifiers,
            isTypeOnly,
            identifier,
            moduleReference,
        );
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseImportClause(
        &mut self,
        identifier: Option<Identifier>,
        pos: usize,
        isTypeOnly: bool,
    ) -> ImportClause {
        // ImportClause:
        //  ImportedDefaultBinding
        //  NameSpaceImport
        //  NamedImports
        //  ImportedDefaultBinding, NameSpaceImport
        //  ImportedDefaultBinding, NamedImports

        // If there was no default import or if there is comma token after default import
        // parse namespace or named imports
        let mut namedBindings = None;
        if identifier.is_none() || self.parseOptional(SyntaxKind::CommaToken) {
            namedBindings = Some(if self.token() == SyntaxKind::AsteriskToken {
                NamedImportBindings::NamespaceImport(Rc::new(self.parseNamespaceImport()))
            } else {
                NamedImportBindings::NamedImports(Rc::new(self.parseNamedImports()))
            });
        }
        let node =
            self.factory
                .createImportClause(isTypeOnly, identifier.map(Rc::new), namedBindings);
        self.finishNode(node, pos, None)
    }

    fn parseModuleReference(&mut self) -> ModuleReference {
        if self.isExternalModuleReference() {
            ModuleReference::ExternalModuleReference(Rc::new(self.parseExternalModuleReference()))
        } else {
            match self.parseEntityName(false, None) {
                EntityName::Identifier(n) => ModuleReference::Identifier(n),
                EntityName::QualifiedName(n) => ModuleReference::QualifiedName(n),
            }
        }
    }

    fn parseExternalModuleReference(&mut self) -> ExternalModuleReference {
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::RequireKeyword, None, None);
        self.parseExpected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.parseModuleSpecifier();
        self.parseExpected(SyntaxKind::CloseParenToken, None, None);
        let node = self.factory.createExternalModuleReference(expression);
        self.finishNode(node, pos, None)
    }

    fn parseModuleSpecifier(&mut self) -> Expression {
        if self.token() == SyntaxKind::StringLiteral {
            Expression::StringLiteral(Rc::new(self.parseStringLiteral()))
        } else {
            // We allow arbitrary expressions here, even though the grammar only allows string
            // literals.  We check to ensure that it is only a string literal later in the grammar
            // check pass.
            self.parseExpression()
        }
    }

    fn parseNamespaceImport(&mut self) -> NamespaceImport {
        // NameSpaceImport:
        //  * as ImportedBinding
        let pos = self.getNodePos();
        self.parseExpected(SyntaxKind::AsteriskToken, None, None);
        self.parseExpected(SyntaxKind::AsKeyword, None, None);
        let name = self.parseIdentifier(None, None);
        let node = self.factory.createNamespaceImport(Rc::new(name));
        self.finishNode(node, pos, None)
    }

    fn parseNamedImports(&mut self) -> NamedImports {
        let pos = self.getNodePos();

        // NamedImports:
        //  { }
        //  { ImportsList }
        //  { ImportsList, }

        // ImportsList:
        //  ImportSpecifier
        //  ImportsList, ImportSpecifier
        let elements = self.parseBracketedList(
            ParsingContext::ImportOrExportSpecifiers,
            |p| p.parseImportSpecifier(),
            SyntaxKind::OpenBraceToken,
            SyntaxKind::CloseBraceToken,
        );
        let node = self.factory.createNamedImports(elements);
        self.finishNode(node, pos, None)
    }

    fn parseNamedExports(&mut self) -> NamedExports {
        let pos = self.getNodePos();

        // NamedImports:
        //  { }
        //  { ImportsList }
        //  { ImportsList, }

        // ImportsList:
        //  ImportSpecifier
        //  ImportsList, ImportSpecifier
        let elements = self.parseBracketedList(
            ParsingContext::ImportOrExportSpecifiers,
            |p| p.parseExportSpecifier(),
            SyntaxKind::OpenBraceToken,
            SyntaxKind::CloseBraceToken,
        );
        let node = self.factory.createNamedExports(elements);
        self.finishNode(node, pos, None)
    }

    fn parseExportSpecifier(&mut self) -> ExportSpecifier {
        let (pos, isTypeOnly, propertyName, name) =
            self.parseImportOrExportSpecifier(SyntaxKind::ExportSpecifier);
        let node = self
            .factory
            .createExportSpecifier(isTypeOnly, propertyName, Rc::new(name));
        self.finishNode(node, pos, None)
    }

    fn parseImportSpecifier(&mut self) -> ImportSpecifier {
        let (pos, isTypeOnly, propertyName, name) =
            self.parseImportOrExportSpecifier(SyntaxKind::ImportSpecifier);
        let node = self
            .factory
            .createImportSpecifier(isTypeOnly, propertyName, Rc::new(name));
        self.finishNode(node, pos, None)
    }

    /// Returns (pos, isTypeOnly, propertyName, name)
    fn parseImportOrExportSpecifier(
        &mut self,
        kind: SyntaxKind,
    ) -> (usize, bool, Option<Identifier>, Identifier) {
        debug_assert!(kind == SyntaxKind::ImportSpecifier || kind == SyntaxKind::ExportSpecifier);
        let pos = self.getNodePos();
        // ImportSpecifier:
        //   BindingIdentifier
        //   IdentifierName as BindingIdentifier
        // ExportSpecifier:
        //   IdentifierName
        //   IdentifierName as IdentifierName
        let mut checkIdentifierIsKeyword = isKeyword(self.token()) && !self.isIdentifier();
        let mut checkIdentifierStart = self.scanner.getTokenPos();
        let mut checkIdentifierEnd = self.scanner.getTextPos();

        let mut parseNameWithKeywordCheck = |parser: &mut Parser| {
            checkIdentifierIsKeyword = isKeyword(parser.token()) && !parser.isIdentifier();
            checkIdentifierStart = parser.scanner.getTokenPos();
            checkIdentifierEnd = parser.scanner.getTextPos();
            parser.parseIdentifierName(None)
        };

        let mut isTypeOnly = false;
        let mut propertyName = None;
        let mut canParseAsKeyword = true;
        let mut name = self.parseIdentifierName(None);
        if &name.escapedText.0 == "type" {
            // If the first token of an import specifier is 'type', there are a lot of possibilities,
            // especially if we see 'as' afterwards:
            //
            // import { type } from "mod";          - isTypeOnly: false,   name: type
            // import { type as } from "mod";       - isTypeOnly: true,    name: as
            // import { type as as } from "mod";    - isTypeOnly: false,   name: as,    propertyName: type
            // import { type as as as } from "mod"; - isTypeOnly: true,    name: as,    propertyName: as
            if self.token() == SyntaxKind::AsKeyword {
                // { type as ...? }
                let firstAs = self.parseIdentifierName(None);
                if self.token() == SyntaxKind::AsKeyword {
                    // { type as as ...? }
                    let secondAs = self.parseIdentifierName(None);
                    if tokenIsIdentifierOrKeyword(self.token()) {
                        // { type as as something }
                        isTypeOnly = true;
                        propertyName = Some(firstAs);
                        name = parseNameWithKeywordCheck(self);
                        canParseAsKeyword = false;
                    } else {
                        // { type as as }
                        propertyName = Some(name);
                        name = secondAs;
                        canParseAsKeyword = false;
                    }
                } else if tokenIsIdentifierOrKeyword(self.token()) {
                    // { type as something }
                    propertyName = Some(name);
                    canParseAsKeyword = false;
                    name = parseNameWithKeywordCheck(self);
                } else {
                    // { type as }
                    isTypeOnly = true;
                    name = firstAs;
                }
            } else if tokenIsIdentifierOrKeyword(self.token()) {
                // { type something ...? }
                isTypeOnly = true;
                name = parseNameWithKeywordCheck(self);
            }
        }

        if canParseAsKeyword && self.token() == SyntaxKind::AsKeyword {
            propertyName = Some(name);
            self.parseExpected(SyntaxKind::AsKeyword, None, None);
            name = parseNameWithKeywordCheck(self);
        }
        if kind == SyntaxKind::ImportSpecifier && checkIdentifierIsKeyword {
            self.parseErrorAt::<u8>(
                checkIdentifierStart,
                checkIdentifierEnd,
                Diagnostics::Identifier_expected,
                None,
            );
        }
        (pos, isTypeOnly, propertyName, name)
    }

    fn parseNamespaceExport(&mut self, pos: usize) -> NamespaceExport {
        let name = self.parseIdentifierName(None);
        let node = self.factory.createNamespaceExport(name);
        self.finishNode(node, pos, None)
    }

    fn parseExportDeclaration(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> ExportDeclaration {
        let savedAwaitContext = self.inAwaitContext();
        self.setAwaitContext(true);
        let mut exportClause = None;
        let mut moduleSpecifier = None;
        let mut assertClause = None;
        let isTypeOnly = self.parseOptional(SyntaxKind::TypeKeyword);
        let namespaceExportPos = self.getNodePos();
        if self.parseOptional(SyntaxKind::AsteriskToken) {
            if self.parseOptional(SyntaxKind::AsKeyword) {
                exportClause = Some(NamedExportBindings::NamespaceExport(Rc::new(
                    self.parseNamespaceExport(namespaceExportPos),
                )));
            }
            self.parseExpected(SyntaxKind::FromKeyword, None, None);
            moduleSpecifier = Some(self.parseModuleSpecifier());
        } else {
            exportClause = Some(NamedExportBindings::NamedExports(Rc::new(
                self.parseNamedExports(),
            )));
            // It is not uncommon to accidentally omit the 'from' keyword. Additionally, in editing scenarios,
            // the 'from' keyword can be parsed as a named export when the export clause is unterminated (i.e. `export { from "moduleName";`)
            // If we don't have a 'from' keyword, see if we have a string literal such that ASI won't take effect.
            if self.token() == SyntaxKind::FromKeyword
                || (self.token() == SyntaxKind::StringLiteral
                    && !self.scanner.hasPrecedingLineBreak())
            {
                self.parseExpected(SyntaxKind::FromKeyword, None, None);
                moduleSpecifier = Some(self.parseModuleSpecifier());
            }
        }
        if moduleSpecifier.is_some()
            && self.token() == SyntaxKind::AssertKeyword
            && !self.scanner.hasPrecedingLineBreak()
        {
            assertClause = Some(self.parseAssertClause());
        }
        self.parseSemicolon();
        self.setAwaitContext(savedAwaitContext);
        let node = self.factory.createExportDeclaration(
            decorators,
            modifiers,
            isTypeOnly,
            exportClause,
            moduleSpecifier,
            assertClause,
        );
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    fn parseExportAssignment(
        &mut self,
        pos: usize,
        hasJSDoc: bool,
        decorators: Option<NodeArray<Decorator>>,
        modifiers: Option<NodeArray<Modifier>>,
    ) -> ExportAssignment {
        let savedAwaitContext = self.inAwaitContext();
        self.setAwaitContext(true);
        let mut isExportEquals = None;
        if self.parseOptional(SyntaxKind::EqualsToken) {
            isExportEquals = Some(true);
        } else {
            self.parseExpected(SyntaxKind::DefaultKeyword, None, None);
        }
        let expression = self.parseAssignmentExpressionOrHigher();
        self.parseSemicolon();
        self.setAwaitContext(savedAwaitContext);
        let node =
            self.factory
                .createExportAssignment(decorators, modifiers, isExportEquals, expression);
        let node = self.finishNode(node, pos, None);
        self.withJSDoc(node, hasJSDoc)
    }

    //     function setExternalModuleIndicator(sourceFile: SourceFile) {
    //         // Try to use the first top-level import/export when available, then
    //         // fall back to looking for an 'import.meta' somewhere in the tree if necessary.
    //         sourceFile.externalModuleIndicator =
    //                 forEach(sourceFile.statements, isAnExternalModuleIndicatorNode) ||
    //                 getImportMetaIfNecessary(sourceFile);
    //     }

    //     function isAnExternalModuleIndicatorNode(node: Node) {
    //         return hasModifierOfKind(node, SyntaxKind::ExportKeyword)
    //             || isImportEqualsDeclaration(node) && ts.isExternalModuleReference(node.moduleReference)
    //             || isImportDeclaration(node)
    //             || isExportAssignment(node)
    //             || isExportDeclaration(node) ? node : undefined;
    //     }

    //     function getImportMetaIfNecessary(sourceFile: SourceFile) {
    //         return sourceFile.flags & NodeFlags.PossiblyContainsImportMeta ?
    //             walkTreeForExternalModuleIndicators(sourceFile) :
    //             undefined;
    //     }

    //     function walkTreeForExternalModuleIndicators(node: Node): Node | undefined {
    //         return isImportMeta(node) ? node : forEachChild(node, walkTreeForExternalModuleIndicators);
    //     }

    //     /** Do not use hasModifier inside the parser; it relies on parent pointers. Use this instead. */
    //     function hasModifierOfKind(node: Node, kind: SyntaxKind) {
    //         return some(node.modifiers, m => m.kind === kind);
    //     }

    //     function isImportMeta(node: Node): boolean {
    //         return isMetaProperty(node) && node.keywordToken === SyntaxKind::ImportKeyword && node.name.escapedText === "meta";
    //     }

    //     export namespace JSDocParser {
    //         export function parseJSDocTypeExpressionForTests(content: string, start: number | undefined, length: number | undefined): { jsDocTypeExpression: JSDocTypeExpression, diagnostics: Diagnostic[] } | undefined {
    //             initializeState("file.js", content, ScriptTarget.Latest, /*_syntaxCursor:*/ undefined, ScriptKind.JS);
    //             scanner.setText(content, start, length);
    //             currentToken = scanner.scan();
    //             const jsDocTypeExpression = parseJSDocTypeExpression();

    //             const sourceFile = createSourceFile("file.js", ScriptTarget.Latest, ScriptKind.JS, /*isDeclarationFile*/ false, [], factory.createToken(SyntaxKind::EndOfFileToken), NodeFlags.None);
    //             const diagnostics = attachFileToDiagnostics(parseDiagnostics, sourceFile);
    //             if (jsDocDiagnostics) {
    //                 sourceFile.jsDocDiagnostics = attachFileToDiagnostics(jsDocDiagnostics, sourceFile);
    //             }

    //             clearState();

    //             return jsDocTypeExpression ? { jsDocTypeExpression, diagnostics } : undefined;
    //         }

    //         // Parses out a JSDoc type expression.
    //         export function parseJSDocTypeExpression(mayOmitBraces?: boolean): JSDocTypeExpression {
    //             const pos = getNodePos();
    //             const hasBrace = (mayOmitBraces ? parseOptional : parseExpected)(SyntaxKind::OpenBraceToken);
    //             const type = doInsideOfContext(NodeFlags.JSDoc, parseJSDocType);
    //             if (!mayOmitBraces || hasBrace) {
    //                 parseExpectedJSDoc(SyntaxKind::CloseBraceToken);
    //             }

    //             const result = factory.createJSDocTypeExpression(type);
    //             fixupParentReferences(result);
    //             return finishNode(result, pos);
    //         }

    //         export function parseJSDocNameReference(): JSDocNameReference {
    //             const pos = getNodePos();
    //             const hasBrace = parseOptional(SyntaxKind::OpenBraceToken);
    //             const p2 = getNodePos();
    //             let entityName: EntityName | JSDocMemberName = parseEntityName(/* allowReservedWords*/ false);
    //             while (token() === SyntaxKind::PrivateIdentifier) {
    //                 reScanHashToken(); // rescan #id as # id
    //                 nextTokenJSDoc(); // then skip the #
    //                 entityName = finishNode(factory.createJSDocMemberName(entityName, parseIdentifier()), p2);
    //             }
    //             if (hasBrace) {
    //                 parseExpectedJSDoc(SyntaxKind::CloseBraceToken);
    //             }

    //             const result = factory.createJSDocNameReference(entityName);
    //             fixupParentReferences(result);
    //             return finishNode(result, pos);
    //         }

    //         export function parseIsolatedJSDocComment(content: string, start: number | undefined, length: number | undefined): { jsDoc: JSDoc, diagnostics: Diagnostic[] } | undefined {
    //             initializeState("", content, ScriptTarget.Latest, /*_syntaxCursor:*/ undefined, ScriptKind.JS);
    //             const jsDoc = doInsideOfContext(NodeFlags.JSDoc, () => parseJSDocCommentWorker(start, length));

    //             const sourceFile = { languageVariant: LanguageVariant.Standard, text: content } as SourceFile;
    //             const diagnostics = attachFileToDiagnostics(parseDiagnostics, sourceFile);
    //             clearState();

    //             return jsDoc ? { jsDoc, diagnostics } : undefined;
    //         }

    //         export function parseJSDocComment(parent: HasJSDoc, start: number, length: number): JSDoc | undefined {
    //             const saveToken = currentToken;
    //             const saveParseDiagnosticsLength = parseDiagnostics.length;
    //             const saveParseErrorBeforeNextFinishedNode = parseErrorBeforeNextFinishedNode;

    //             const comment = doInsideOfContext(NodeFlags.JSDoc, () => parseJSDocCommentWorker(start, length));
    //             setParent(comment, parent);

    //             if (contextFlags & NodeFlags.JavaScriptFile) {
    //                 if (!jsDocDiagnostics) {
    //                     jsDocDiagnostics = [];
    //                 }
    //                 jsDocDiagnostics.push(...parseDiagnostics);
    //             }
    //             currentToken = saveToken;
    //             parseDiagnostics.length = saveParseDiagnosticsLength;
    //             parseErrorBeforeNextFinishedNode = saveParseErrorBeforeNextFinishedNode;
    //             return comment;
    //         }

    //         const enum JSDocState {
    //             BeginningOfLine,
    //             SawAsterisk,
    //             SavingComments,
    //             SavingBackticks, // NOTE: Only used when parsing tag comments
    //         }

    //         const enum PropertyLikeParse {
    //             Property = 1 << 0,
    //             Parameter = 1 << 1,
    //             CallbackParameter = 1 << 2,
    //         }

    //         function parseJSDocCommentWorker(start = 0, length: number | undefined): JSDoc | undefined {
    //             const content = sourceText;
    //             const end = length === undefined ? content.length : start + length;
    //             length = end - start;

    //             Debug.assert(start >= 0);
    //             Debug.assert(start <= end);
    //             Debug.assert(end <= content.length);

    //             // Check for /** (JSDoc opening part)
    //             if (!isJSDocLikeText(content, start)) {
    //                 return undefined;
    //             }

    //             let tags: JSDocTag[];
    //             let tagsPos: number;
    //             let tagsEnd: number;
    //             let linkEnd: number;
    //             let commentsPos: number | undefined;
    //             let comments: string[] = [];
    //             const parts: JSDocComment[] = [];

    //             // + 3 for leading /**, - 5 in total for /** */
    //             return scanner.scanRange(start + 3, length - 5, () => {
    //                 // Initially we can parse out a tag.  We also have seen a starting asterisk.
    //                 // This is so that /** * @type */ doesn't parse.
    //                 let state = JSDocState.SawAsterisk;
    //                 let margin: number | undefined;
    //                 // + 4 for leading '/** '
    //                 // + 1 because the last index of \n is always one index before the first character in the line and coincidentally, if there is no \n before start, it is -1, which is also one index before the first character
    //                 let indent = start - (content.lastIndexOf("\n", start) + 1) + 4;
    //                 function pushComment(text: string) {
    //                     if (!margin) {
    //                         margin = indent;
    //                     }
    //                     comments.push(text);
    //                     indent += text.length;
    //                 }

    //                 nextTokenJSDoc();
    //                 while (parseOptionalJsdoc(SyntaxKind::WhitespaceTrivia));
    //                 if (parseOptionalJsdoc(SyntaxKind::NewLineTrivia)) {
    //                     state = JSDocState.BeginningOfLine;
    //                     indent = 0;
    //                 }
    //                 loop: while (true) {
    //                     switch (token()) {
    //                         SyntaxKind::AtToken:
    //                             if (state === JSDocState.BeginningOfLine || state === JSDocState.SawAsterisk) {
    //                                 removeTrailingWhitespace(comments);
    //                                 if (!commentsPos) commentsPos = getNodePos();
    //                                 addTag(parseTag(indent));
    //                                 // NOTE: According to usejsdoc.org, a tag goes to end of line, except the last tag.
    //                                 // Real-world comments may break this rule, so "BeginningOfLine" will not be a real line beginning
    //                                 // for malformed examples like `/** @param {string} x @returns {number} the length */`
    //                                 state = JSDocState.BeginningOfLine;
    //                                 margin = undefined;
    //                             }
    //                             else {
    //                                 pushComment(scanner.getTokenText());
    //                             }
    //                             break;
    //                         SyntaxKind::NewLineTrivia:
    //                             comments.push(scanner.getTokenText());
    //                             state = JSDocState.BeginningOfLine;
    //                             indent = 0;
    //                             break;
    //                         SyntaxKind::AsteriskToken:
    //                             const asterisk = scanner.getTokenText();
    //                             if (state === JSDocState.SawAsterisk || state === JSDocState.SavingComments) {
    //                                 // If we've already seen an asterisk, then we can no longer parse a tag on this line
    //                                 state = JSDocState.SavingComments;
    //                                 pushComment(asterisk);
    //                             }
    //                             else {
    //                                 // Ignore the first asterisk on a line
    //                                 state = JSDocState.SawAsterisk;
    //                                 indent += asterisk.length;
    //                             }
    //                             break;
    //                         SyntaxKind::WhitespaceTrivia:
    //                             // only collect whitespace if we're already saving comments or have just crossed the comment indent margin
    //                             const whitespace = scanner.getTokenText();
    //                             if (state === JSDocState.SavingComments) {
    //                                 comments.push(whitespace);
    //                             }
    //                             else if (margin !== undefined && indent + whitespace.length > margin) {
    //                                 comments.push(whitespace.slice(margin - indent));
    //                             }
    //                             indent += whitespace.length;
    //                             break;
    //                         SyntaxKind::EndOfFileToken:
    //                             break loop;
    //                         SyntaxKind::OpenBraceToken:
    //                             state = JSDocState.SavingComments;
    //                             const commentEnd = scanner.getStartPos();
    //                             const linkStart = scanner.getTextPos() - 1;
    //                             const link = parseJSDocLink(linkStart);
    //                             if (link) {
    //                                 if (!linkEnd) {
    //                                     removeLeadingNewlines(comments);
    //                                 }
    //                                 parts.push(finishNode(factory.createJSDocText(comments.join("")), linkEnd ?? start, commentEnd));
    //                                 parts.push(link);
    //                                 comments = [];
    //                                 linkEnd = scanner.getTextPos();
    //                                 break;
    //                             }
    //                             // fallthrough if it's not a {@link sequence
    //                         default:
    //                             // Anything else is doc comment text. We just save it. Because it
    //                             // wasn't a tag, we can no longer parse a tag on this line until we hit the next
    //                             // line break.
    //                             state = JSDocState.SavingComments;
    //                             pushComment(scanner.getTokenText());
    //                             break;
    //                     }
    //                     nextTokenJSDoc();
    //                 }
    //                 removeTrailingWhitespace(comments);
    //                 if (parts.length && comments.length) {
    //                     parts.push(finishNode(factory.createJSDocText(comments.join("")), linkEnd ?? start, commentsPos));
    //                 }
    //                 if (parts.length && tags) Debug.assertIsDefined(commentsPos, "having parsed tags implies that the end of the comment span should be set");
    //                 const tagsArray = tags && createNodeArray(tags, tagsPos, tagsEnd);
    //                 return finishNode(factory.createJSDocComment(parts.length ? createNodeArray(parts, start, commentsPos) : comments.length ? comments.join("") : undefined, tagsArray), start, end);
    //             });

    //             function removeLeadingNewlines(comments: string[]) {
    //                 while (comments.length && (comments[0] === "\n" || comments[0] === "\r")) {
    //                     comments.shift();
    //                 }
    //             }

    //             function removeTrailingWhitespace(comments: string[]) {
    //                 while (comments.length && comments[comments.length - 1].trim() === "") {
    //                     comments.pop();
    //                 }
    //             }

    //             function isNextNonwhitespaceTokenEndOfFile(): boolean {
    //                 // We must use infinite lookahead, as there could be any number of newlines :(
    //                 while (true) {
    //                     nextTokenJSDoc();
    //                     if (token() === SyntaxKind::EndOfFileToken) {
    //                         return true;
    //                     }
    //                     if (!(token() === SyntaxKind::WhitespaceTrivia || token() === SyntaxKind::NewLineTrivia)) {
    //                         return false;
    //                     }
    //                 }
    //             }

    //             function skipWhitespace(): void {
    //                 if (token() === SyntaxKind::WhitespaceTrivia || token() === SyntaxKind::NewLineTrivia) {
    //                     if (lookAhead(isNextNonwhitespaceTokenEndOfFile)) {
    //                         return; // Don't skip whitespace prior to EoF (or end of comment) - that shouldn't be included in any node's range
    //                     }
    //                 }
    //                 while (token() === SyntaxKind::WhitespaceTrivia || token() === SyntaxKind::NewLineTrivia) {
    //                     nextTokenJSDoc();
    //                 }
    //             }

    //             function skipWhitespaceOrAsterisk(): string {
    //                 if (token() === SyntaxKind::WhitespaceTrivia || token() === SyntaxKind::NewLineTrivia) {
    //                     if (lookAhead(isNextNonwhitespaceTokenEndOfFile)) {
    //                         return ""; // Don't skip whitespace prior to EoF (or end of comment) - that shouldn't be included in any node's range
    //                     }
    //                 }

    //                 let precedingLineBreak = scanner.hasPrecedingLineBreak();
    //                 let seenLineBreak = false;
    //                 let indentText = "";
    //                 while ((precedingLineBreak && token() === SyntaxKind::AsteriskToken) || token() === SyntaxKind::WhitespaceTrivia || token() === SyntaxKind::NewLineTrivia) {
    //                     indentText += scanner.getTokenText();
    //                     if (token() === SyntaxKind::NewLineTrivia) {
    //                         precedingLineBreak = true;
    //                         seenLineBreak = true;
    //                         indentText = "";
    //                     }
    //                     else if (token() === SyntaxKind::AsteriskToken) {
    //                         precedingLineBreak = false;
    //                     }
    //                     nextTokenJSDoc();
    //                 }
    //                 return seenLineBreak ? indentText : "";
    //             }

    //             function parseTag(margin: number) {
    //                 Debug.assert(token() === SyntaxKind::AtToken);
    //                 const start = scanner.getTokenPos();
    //                 nextTokenJSDoc();

    //                 const tagName = parseJSDocIdentifierName(/*message*/ undefined);
    //                 const indentText = skipWhitespaceOrAsterisk();

    //                 let tag: JSDocTag | undefined;
    //                 switch (tagName.escapedText) {
    //                     "author":
    //                         tag = parseAuthorTag(start, tagName, margin, indentText);
    //                         break;
    //                     "implements":
    //                         tag = parseImplementsTag(start, tagName, margin, indentText);
    //                         break;
    //                     "augments":
    //                     "extends":
    //                         tag = parseAugmentsTag(start, tagName, margin, indentText);
    //                         break;
    //                     "class":
    //                     "constructor":
    //                         tag = parseSimpleTag(start, factory.createJSDocClassTag, tagName, margin, indentText);
    //                         break;
    //                     "public":
    //                         tag = parseSimpleTag(start, factory.createJSDocPublicTag, tagName, margin, indentText);
    //                         break;
    //                     "private":
    //                         tag = parseSimpleTag(start, factory.createJSDocPrivateTag, tagName, margin, indentText);
    //                         break;
    //                     "protected":
    //                         tag = parseSimpleTag(start, factory.createJSDocProtectedTag, tagName, margin, indentText);
    //                         break;
    //                     "readonly":
    //                         tag = parseSimpleTag(start, factory.createJSDocReadonlyTag, tagName, margin, indentText);
    //                         break;
    //                     "override":
    //                         tag = parseSimpleTag(start, factory.createJSDocOverrideTag, tagName, margin, indentText);
    //                         break;
    //                     "deprecated":
    //                         hasDeprecatedTag = true;
    //                         tag = parseSimpleTag(start, factory.createJSDocDeprecatedTag, tagName, margin, indentText);
    //                         break;
    //                     "this":
    //                         tag = parseThisTag(start, tagName, margin, indentText);
    //                         break;
    //                     "enum":
    //                         tag = parseEnumTag(start, tagName, margin, indentText);
    //                         break;
    //                     "arg":
    //                     "argument":
    //                     "param":
    //                         return parseParameterOrPropertyTag(start, tagName, PropertyLikeParse.Parameter, margin);
    //                     "return":
    //                     "returns":
    //                         tag = parseReturnTag(start, tagName, margin, indentText);
    //                         break;
    //                     "template":
    //                         tag = parseTemplateTag(start, tagName, margin, indentText);
    //                         break;
    //                     "type":
    //                         tag = parseTypeTag(start, tagName, margin, indentText);
    //                         break;
    //                     "typedef":
    //                         tag = parseTypedefTag(start, tagName, margin, indentText);
    //                         break;
    //                     "callback":
    //                         tag = parseCallbackTag(start, tagName, margin, indentText);
    //                         break;
    //                     "see":
    //                         tag = parseSeeTag(start, tagName, margin, indentText);
    //                         break;
    //                     default:
    //                         tag = parseUnknownTag(start, tagName, margin, indentText);
    //                         break;
    //                 }
    //                 return tag;
    //             }

    //             function parseTrailingTagComments(pos: number, end: number, margin: number, indentText: string) {
    //                 // some tags, like typedef and callback, have already parsed their comments earlier
    //                 if (!indentText) {
    //                     margin += end - pos;
    //                 }
    //                 return parseTagComments(margin, indentText.slice(margin));
    //             }

    //             function parseTagComments(indent: number, initialMargin?: string): string | NodeArray<JSDocComment> | undefined {
    //                 const commentsPos = getNodePos();
    //                 let comments: string[] = [];
    //                 const parts: JSDocComment[] = [];
    //                 let linkEnd;
    //                 let state = JSDocState.BeginningOfLine;
    //                 let previousWhitespace = true;
    //                 let margin: number | undefined;
    //                 function pushComment(text: string) {
    //                     if (!margin) {
    //                         margin = indent;
    //                     }
    //                     comments.push(text);
    //                     indent += text.length;
    //                 }
    //                 if (initialMargin !== undefined) {
    //                     // jump straight to saving comments if there is some initial indentation
    //                     if (initialMargin !== "") {
    //                         pushComment(initialMargin);
    //                     }
    //                     state = JSDocState.SawAsterisk;
    //                 }
    //                 let tok = token() as JSDocSyntaxKind;
    //                 loop: while (true) {
    //                     switch (tok) {
    //                         SyntaxKind::NewLineTrivia:
    //                             state = JSDocState.BeginningOfLine;
    //                             // don't use pushComment here because we want to keep the margin unchanged
    //                             comments.push(scanner.getTokenText());
    //                             indent = 0;
    //                             break;
    //                         SyntaxKind::AtToken:
    //                             if (state === JSDocState.SavingBackticks
    //                                 || state === JSDocState.SavingComments && (!previousWhitespace || lookAhead(isNextJSDocTokenWhitespace))) {
    //                                 // @ doesn't start a new tag inside ``, and inside a comment, only after whitespace or not before whitespace
    //                                 comments.push(scanner.getTokenText());
    //                                 break;
    //                             }
    //                             scanner.setTextPos(scanner.getTextPos() - 1);
    //                             // falls through
    //                         SyntaxKind::EndOfFileToken:
    //                             // Done
    //                             break loop;
    //                         SyntaxKind::WhitespaceTrivia:
    //                             if (state === JSDocState.SavingComments || state === JSDocState.SavingBackticks) {
    //                                 pushComment(scanner.getTokenText());
    //                             }
    //                             else {
    //                                 const whitespace = scanner.getTokenText();
    //                                 // if the whitespace crosses the margin, take only the whitespace that passes the margin
    //                                 if (margin !== undefined && indent + whitespace.length > margin) {
    //                                     comments.push(whitespace.slice(margin - indent));
    //                                 }
    //                                 indent += whitespace.length;
    //                             }
    //                             break;
    //                         SyntaxKind::OpenBraceToken:
    //                             state = JSDocState.SavingComments;
    //                             const commentEnd = scanner.getStartPos();
    //                             const linkStart = scanner.getTextPos() - 1;
    //                             const link = parseJSDocLink(linkStart);
    //                             if (link) {
    //                                 parts.push(finishNode(factory.createJSDocText(comments.join("")), linkEnd ?? commentsPos, commentEnd));
    //                                 parts.push(link);
    //                                 comments = [];
    //                                 linkEnd = scanner.getTextPos();
    //                             }
    //                             else {
    //                                 pushComment(scanner.getTokenText());
    //                             }
    //                             break;
    //                         SyntaxKind::BacktickToken:
    //                             if (state === JSDocState.SavingBackticks) {
    //                                 state = JSDocState.SavingComments;
    //                             }
    //                             else {
    //                                 state = JSDocState.SavingBackticks;
    //                             }
    //                             pushComment(scanner.getTokenText());
    //                             break;
    //                         SyntaxKind::AsteriskToken:
    //                             if (state === JSDocState.BeginningOfLine) {
    //                                 // leading asterisks start recording on the *next* (non-whitespace) token
    //                                 state = JSDocState.SawAsterisk;
    //                                 indent += 1;
    //                                 break;
    //                             }
    //                             // record the * as a comment
    //                             // falls through
    //                         default:
    //                             if (state !== JSDocState.SavingBackticks) {
    //                                 state = JSDocState.SavingComments; // leading identifiers start recording as well
    //                             }
    //                             pushComment(scanner.getTokenText());
    //                             break;
    //                     }
    //                     previousWhitespace = token() === SyntaxKind::WhitespaceTrivia;
    //                     tok = nextTokenJSDoc();
    //                 }

    //                 removeLeadingNewlines(comments);
    //                 removeTrailingWhitespace(comments);
    //                 if (parts.length) {
    //                     if (comments.length) {
    //                         parts.push(finishNode(factory.createJSDocText(comments.join("")), linkEnd ?? commentsPos));
    //                     }
    //                     return createNodeArray(parts, commentsPos, scanner.getTextPos());
    //                 }
    //                 else if (comments.length) {
    //                     return comments.join("");
    //                 }
    //             }

    //             function isNextJSDocTokenWhitespace() {
    //                 const next = nextTokenJSDoc();
    //                 return next === SyntaxKind::WhitespaceTrivia || next === SyntaxKind::NewLineTrivia;
    //             }

    //             function parseJSDocLink(start: number) {
    //                 const linkType = tryParse(parseJSDocLinkPrefix);
    //                 if (!linkType) {
    //                     return undefined;
    //                 }
    //                 nextTokenJSDoc(); // start at token after link, then skip any whitespace
    //                 skipWhitespace();
    //                 // parseEntityName logs an error for non-identifier, so create a MissingNode ourselves to avoid the error
    //                 const p2 = getNodePos();
    //                 let name: EntityName | JSDocMemberName | undefined = tokenIsIdentifierOrKeyword(token())
    //                     ? parseEntityName(/*allowReservedWords*/ true)
    //                     : undefined;
    //                 if (name) {
    //                     while (token() === SyntaxKind::PrivateIdentifier) {
    //                         reScanHashToken(); // rescan #id as # id
    //                         nextTokenJSDoc(); // then skip the #
    //                         name = finishNode(factory.createJSDocMemberName(name, parseIdentifier()), p2);
    //                     }
    //                 }
    //                 const text = [];
    //                 while (token() !== SyntaxKind::CloseBraceToken && token() !== SyntaxKind::NewLineTrivia && token() !== SyntaxKind::EndOfFileToken) {
    //                     text.push(scanner.getTokenText());
    //                     nextTokenJSDoc();
    //                 }
    //                 const create = linkType === "link" ? factory.createJSDocLink
    //                     : linkType === "linkcode" ? factory.createJSDocLinkCode
    //                     : factory.createJSDocLinkPlain;
    //                 return finishNode(create(name, text.join("")), start, scanner.getTextPos());
    //             }

    //             function parseJSDocLinkPrefix() {
    //                 skipWhitespaceOrAsterisk();
    //                 if (token() === SyntaxKind::OpenBraceToken
    //                     && nextTokenJSDoc() === SyntaxKind::AtToken
    //                     && tokenIsIdentifierOrKeyword(nextTokenJSDoc())) {
    //                     const kind = scanner.getTokenValue();
    //                     if(kind === "link" || kind === "linkcode" || kind === "linkplain") {
    //                         return kind;
    //                     }
    //                 }
    //             }

    //             function parseUnknownTag(start: number, tagName: Identifier, indent: number, indentText: string) {
    //                 return finishNode(factory.createJSDocUnknownTag(tagName, parseTrailingTagComments(start, getNodePos(), indent, indentText)), start);
    //             }

    //             function addTag(tag: JSDocTag | undefined): void {
    //                 if (!tag) {
    //                     return;
    //                 }
    //                 if (!tags) {
    //                     tags = [tag];
    //                     tagsPos = tag.pos;
    //                 }
    //                 else {
    //                     tags.push(tag);
    //                 }
    //                 tagsEnd = tag.end;
    //             }

    //             function tryParseTypeExpression(): JSDocTypeExpression | undefined {
    //                 skipWhitespaceOrAsterisk();
    //                 return token() === SyntaxKind::OpenBraceToken ? parseJSDocTypeExpression() : undefined;
    //             }

    //             function parseBracketNameInPropertyAndParamTag(): { name: EntityName, isBracketed: boolean } {
    //                 // Looking for something like '[foo]', 'foo', '[foo.bar]' or 'foo.bar'
    //                 const isBracketed = parseOptionalJsdoc(SyntaxKind::OpenBracketToken);
    //                 if (isBracketed) {
    //                     skipWhitespace();
    //                 }
    //                 // a markdown-quoted name: `arg` is not legal jsdoc, but occurs in the wild
    //                 const isBackquoted = parseOptionalJsdoc(SyntaxKind::BacktickToken);
    //                 const name = parseJSDocEntityName();
    //                 if (isBackquoted) {
    //                     parseExpectedTokenJSDoc(SyntaxKind::BacktickToken);
    //                 }
    //                 if (isBracketed) {
    //                     skipWhitespace();
    //                     // May have an optional default, e.g. '[foo = 42]'
    //                     if (parseOptionalToken(SyntaxKind::EqualsToken)) {
    //                         parseExpression();
    //                     }

    //                     parseExpected(SyntaxKind::CloseBracketToken);
    //                 }

    //                 return { name, isBracketed };
    //             }

    //             function isObjectOrObjectArrayTypeReference(node: TypeNode): boolean {
    //                 switch (node.kind) {
    //                     SyntaxKind::ObjectKeyword:
    //                         return true;
    //                     SyntaxKind::ArrayType:
    //                         return isObjectOrObjectArrayTypeReference((node as ArrayTypeNode).elementType);
    //                     default:
    //                         return isTypeReferenceNode(node) && ts.isIdentifier(node.typeName) && node.typeName.escapedText === "Object" && !node.typeArguments;
    //                 }
    //             }

    //             function parseParameterOrPropertyTag(start: number, tagName: Identifier, target: PropertyLikeParse, indent: number): JSDocParameterTag | JSDocPropertyTag {
    //                 let typeExpression = tryParseTypeExpression();
    //                 let isNameFirst = !typeExpression;
    //                 skipWhitespaceOrAsterisk();

    //                 const { name, isBracketed } = parseBracketNameInPropertyAndParamTag();
    //                 const indentText = skipWhitespaceOrAsterisk();

    //                 if (isNameFirst && !lookAhead(parseJSDocLinkPrefix)) {
    //                     typeExpression = tryParseTypeExpression();
    //                 }

    //                 const comment = parseTrailingTagComments(start, getNodePos(), indent, indentText);

    //                 const nestedTypeLiteral = target !== PropertyLikeParse.CallbackParameter && parseNestedTypeLiteral(typeExpression, name, target, indent);
    //                 if (nestedTypeLiteral) {
    //                     typeExpression = nestedTypeLiteral;
    //                     isNameFirst = true;
    //                 }
    //                 const result = target === PropertyLikeParse.Property
    //                     ? factory.createJSDocPropertyTag(tagName, name, isBracketed, typeExpression, isNameFirst, comment)
    //                     : factory.createJSDocParameterTag(tagName, name, isBracketed, typeExpression, isNameFirst, comment);
    //                 return finishNode(result, start);
    //             }

    //             function parseNestedTypeLiteral(typeExpression: JSDocTypeExpression | undefined, name: EntityName, target: PropertyLikeParse, indent: number) {
    //                 if (typeExpression && isObjectOrObjectArrayTypeReference(typeExpression.type)) {
    //                     const pos = getNodePos();
    //                     let child: JSDocPropertyLikeTag | JSDocTypeTag | false;
    //                     let children: JSDocPropertyLikeTag[] | undefined;
    //                     while (child = tryParse(() => parseChildParameterOrPropertyTag(target, indent, name))) {
    //                         if (child.kind === SyntaxKind::JSDocParameterTag || child.kind === SyntaxKind::JSDocPropertyTag) {
    //                             children = append(children, child);
    //                         }
    //                     }
    //                     if (children) {
    //                         const literal = finishNode(factory.createJSDocTypeLiteral(children, typeExpression.type.kind === SyntaxKind::ArrayType), pos);
    //                         return finishNode(factory.createJSDocTypeExpression(literal), pos);
    //                     }
    //                 }
    //             }

    //             function parseReturnTag(start: number, tagName: Identifier, indent: number, indentText: string): JSDocReturnTag {
    //                 if (some(tags, isJSDocReturnTag)) {
    //                     parseErrorAt(tagName.pos, scanner.getTokenPos(), Diagnostics::_0_tag_already_specified, tagName.escapedText);
    //                 }

    //                 const typeExpression = tryParseTypeExpression();
    //                 return finishNode(factory.createJSDocReturnTag(tagName, typeExpression, parseTrailingTagComments(start, getNodePos(), indent, indentText)), start);
    //             }

    //             function parseTypeTag(start: number, tagName: Identifier, indent?: number, indentText?: string): JSDocTypeTag {
    //                 if (some(tags, isJSDocTypeTag)) {
    //                     parseErrorAt(tagName.pos, scanner.getTokenPos(), Diagnostics::_0_tag_already_specified, tagName.escapedText);
    //                 }

    //                 const typeExpression = parseJSDocTypeExpression(/*mayOmitBraces*/ true);
    //                 const comments = indent !== undefined && indentText !== undefined ? parseTrailingTagComments(start, getNodePos(), indent, indentText) : undefined;
    //                 return finishNode(factory.createJSDocTypeTag(tagName, typeExpression, comments), start);
    //             }

    //             function parseSeeTag(start: number, tagName: Identifier, indent?: number, indentText?: string): JSDocSeeTag {
    //                 const isLink = lookAhead(() => nextTokenJSDoc() === SyntaxKind::AtToken && tokenIsIdentifierOrKeyword(nextTokenJSDoc()) && scanner.getTokenValue() === "link");
    //                 const nameExpression = isLink ? undefined : parseJSDocNameReference();
    //                 const comments = indent !== undefined && indentText !== undefined ? parseTrailingTagComments(start, getNodePos(), indent, indentText) : undefined;
    //                 return finishNode(factory.createJSDocSeeTag(tagName, nameExpression, comments), start);
    //             }

    //             function parseAuthorTag(start: number, tagName: Identifier, indent: number, indentText: string): JSDocAuthorTag {
    //                 const commentStart = getNodePos();
    //                 const textOnly = parseAuthorNameAndEmail();
    //                 let commentEnd = scanner.getStartPos();
    //                 const comments = parseTrailingTagComments(start, commentEnd, indent, indentText);
    //                 if (!comments) {
    //                     commentEnd = scanner.getStartPos();
    //                 }
    //                 const allParts = typeof comments !== "string"
    //                     ? createNodeArray(concatenate([finishNode(textOnly, commentStart, commentEnd)], comments) as JSDocComment[], commentStart) // cast away readonly
    //                     : textOnly.text + comments;
    //                 return finishNode(factory.createJSDocAuthorTag(tagName, allParts), start);
    //             }

    //             function parseAuthorNameAndEmail(): JSDocText {
    //                 const comments: string[] = [];
    //                 let inEmail = false;
    //                 let token = scanner.getToken();
    //                 while (token !== SyntaxKind::EndOfFileToken && token !== SyntaxKind::NewLineTrivia) {
    //                     if (token === SyntaxKind::LessThanToken) {
    //                         inEmail = true;
    //                     }
    //                     else if (token === SyntaxKind::AtToken && !inEmail) {
    //                         break;
    //                     }
    //                     else if (token === SyntaxKind::GreaterThanToken && inEmail) {
    //                         comments.push(scanner.getTokenText());
    //                         scanner.setTextPos(scanner.getTokenPos() + 1);
    //                         break;
    //                     }
    //                     comments.push(scanner.getTokenText());
    //                     token = nextTokenJSDoc();
    //                 }

    //                 return factory.createJSDocText(comments.join(""));
    //             }

    //             function parseImplementsTag(start: number, tagName: Identifier, margin: number, indentText: string): JSDocImplementsTag {
    //                 const className = parseExpressionWithTypeArgumentsForAugments();
    //                 return finishNode(factory.createJSDocImplementsTag(tagName, className, parseTrailingTagComments(start, getNodePos(), margin, indentText)), start);
    //             }

    //             function parseAugmentsTag(start: number, tagName: Identifier, margin: number, indentText: string): JSDocAugmentsTag {
    //                 const className = parseExpressionWithTypeArgumentsForAugments();
    //                 return finishNode(factory.createJSDocAugmentsTag(tagName, className, parseTrailingTagComments(start, getNodePos(), margin, indentText)), start);
    //             }

    //             function parseExpressionWithTypeArgumentsForAugments(): ExpressionWithTypeArguments & { expression: Identifier | PropertyAccessEntityNameExpression } {
    //                 const usedBrace = parseOptional(SyntaxKind::OpenBraceToken);
    //                 const pos = getNodePos();
    //                 const expression = parsePropertyAccessEntityNameExpression();
    //                 const typeArguments = tryParseTypeArguments();
    //                 const node = factory.createExpressionWithTypeArguments(expression, typeArguments) as ExpressionWithTypeArguments & { expression: Identifier | PropertyAccessEntityNameExpression };
    //                 const res = finishNode(node, pos);
    //                 if (usedBrace) {
    //                     parseExpected(SyntaxKind::CloseBraceToken);
    //                 }
    //                 return res;
    //             }

    //             function parsePropertyAccessEntityNameExpression() {
    //                 const pos = getNodePos();
    //                 let node: Identifier | PropertyAccessEntityNameExpression = parseJSDocIdentifierName();
    //                 while (parseOptional(SyntaxKind::DotToken)) {
    //                     const name = parseJSDocIdentifierName();
    //                     node = finishNode(factory.createPropertyAccessExpression(node, name), pos) as PropertyAccessEntityNameExpression;
    //                 }
    //                 return node;
    //             }

    //             function parseSimpleTag(start: number, createTag: (tagName: Identifier | undefined, comment?: string | NodeArray<JSDocComment>) => JSDocTag, tagName: Identifier, margin: number, indentText: string): JSDocTag {
    //                 return finishNode(createTag(tagName, parseTrailingTagComments(start, getNodePos(), margin, indentText)), start);
    //             }

    //             function parseThisTag(start: number, tagName: Identifier, margin: number, indentText: string): JSDocThisTag {
    //                 const typeExpression = parseJSDocTypeExpression(/*mayOmitBraces*/ true);
    //                 skipWhitespace();
    //                 return finishNode(factory.createJSDocThisTag(tagName, typeExpression, parseTrailingTagComments(start, getNodePos(), margin, indentText)), start);
    //             }

    //             function parseEnumTag(start: number, tagName: Identifier, margin: number, indentText: string): JSDocEnumTag {
    //                 const typeExpression = parseJSDocTypeExpression(/*mayOmitBraces*/ true);
    //                 skipWhitespace();
    //                 return finishNode(factory.createJSDocEnumTag(tagName, typeExpression, parseTrailingTagComments(start, getNodePos(), margin, indentText)), start);
    //             }

    //             function parseTypedefTag(start: number, tagName: Identifier, indent: number, indentText: string): JSDocTypedefTag {
    //                 let typeExpression: JSDocTypeExpression | JSDocTypeLiteral | undefined = tryParseTypeExpression();
    //                 skipWhitespaceOrAsterisk();

    //                 const fullName = parseJSDocTypeNameWithNamespace();
    //                 skipWhitespace();
    //                 let comment = parseTagComments(indent);

    //                 let end: number | undefined;
    //                 if (!typeExpression || isObjectOrObjectArrayTypeReference(typeExpression.type)) {
    //                     let child: JSDocTypeTag | JSDocPropertyTag | false;
    //                     let childTypeTag: JSDocTypeTag | undefined;
    //                     let jsDocPropertyTags: JSDocPropertyTag[] | undefined;
    //                     let hasChildren = false;
    //                     while (child = tryParse(() => parseChildPropertyTag(indent))) {
    //                         hasChildren = true;
    //                         if (child.kind === SyntaxKind::JSDocTypeTag) {
    //                             if (childTypeTag) {
    //                                 parseErrorAtCurrentToken(Diagnostics::A_JSDoc_typedef_comment_may_not_contain_multiple_type_tags);
    //                                 const lastError = lastOrUndefined(parseDiagnostics);
    //                                 if (lastError) {
    //                                     addRelatedInfo(
    //                                         lastError,
    //                                         createDetachedDiagnostic(fileName, 0, 0, Diagnostics::The_tag_was_first_specified_here)
    //                                     );
    //                                 }
    //                                 break;
    //                             }
    //                             else {
    //                                 childTypeTag = child;
    //                             }
    //                         }
    //                         else {
    //                             jsDocPropertyTags = append(jsDocPropertyTags, child);
    //                         }
    //                     }
    //                     if (hasChildren) {
    //                         const isArrayType = typeExpression && typeExpression.type.kind === SyntaxKind::ArrayType;
    //                         const jsdocTypeLiteral = factory.createJSDocTypeLiteral(jsDocPropertyTags, isArrayType);
    //                         typeExpression = childTypeTag && childTypeTag.typeExpression && !isObjectOrObjectArrayTypeReference(childTypeTag.typeExpression.type) ?
    //                             childTypeTag.typeExpression :
    //                             finishNode(jsdocTypeLiteral, start);
    //                         end = typeExpression.end;
    //                     }
    //                 }

    //                 // Only include the characters between the name end and the next token if a comment was actually parsed out - otherwise it's just whitespace
    //                 end = end || comment !== undefined ?
    //                     getNodePos() :
    //                     (fullName ?? typeExpression ?? tagName).end;

    //                 if (!comment) {
    //                     comment = parseTrailingTagComments(start, end, indent, indentText);
    //                 }

    //                 const typedefTag = factory.createJSDocTypedefTag(tagName, typeExpression, fullName, comment);
    //                 return finishNode(typedefTag, start, end);
    //             }

    //             function parseJSDocTypeNameWithNamespace(nested?: boolean) {
    //                 const pos = scanner.getTokenPos();
    //                 if (!tokenIsIdentifierOrKeyword(token())) {
    //                     return undefined;
    //                 }
    //                 const typeNameOrNamespaceName = parseJSDocIdentifierName();
    //                 if (parseOptional(SyntaxKind::DotToken)) {
    //                     const body = parseJSDocTypeNameWithNamespace(/*nested*/ true);
    //                     const jsDocNamespaceNode = factory.createModuleDeclaration(
    //                         /*decorators*/ undefined,
    //                         /*modifiers*/ undefined,
    //                         typeNameOrNamespaceName,
    //                         body,
    //                         nested ? NodeFlags.NestedNamespace : undefined
    //                     ) as JSDocNamespaceDeclaration;
    //                     return finishNode(jsDocNamespaceNode, pos);
    //                 }

    //                 if (nested) {
    //                     typeNameOrNamespaceName.isInJSDocNamespace = true;
    //                 }
    //                 return typeNameOrNamespaceName;
    //             }

    //             function parseCallbackTagParameters(indent: number) {
    //                 const pos = getNodePos();
    //                 let child: JSDocParameterTag | false;
    //                 let parameters;
    //                 while (child = tryParse(() => parseChildParameterOrPropertyTag(PropertyLikeParse.CallbackParameter, indent) as JSDocParameterTag)) {
    //                     parameters = append(parameters, child);
    //                 }
    //                 return createNodeArray(parameters || [], pos);
    //             }

    //             function parseCallbackTag(start: number, tagName: Identifier, indent: number, indentText: string): JSDocCallbackTag {
    //                 const fullName = parseJSDocTypeNameWithNamespace();
    //                 skipWhitespace();
    //                 let comment = parseTagComments(indent);
    //                 const parameters = parseCallbackTagParameters(indent);
    //                 const returnTag = tryParse(() => {
    //                     if (parseOptionalJsdoc(SyntaxKind::AtToken)) {
    //                         const tag = parseTag(indent);
    //                         if (tag && tag.kind === SyntaxKind::JSDocReturnTag) {
    //                             return tag as JSDocReturnTag;
    //                         }
    //                     }
    //                 });
    //                 const typeExpression = finishNode(factory.createJSDocSignature(/*typeParameters*/ undefined, parameters, returnTag), start);
    //                 if (!comment) {
    //                     comment = parseTrailingTagComments(start, getNodePos(), indent, indentText);
    //                 }
    //                 return finishNode(factory.createJSDocCallbackTag(tagName, typeExpression, fullName, comment), start);
    //             }

    //             function escapedTextsEqual(a: EntityName, b: EntityName): boolean {
    //                 while (!ts.isIdentifier(a) || !ts.isIdentifier(b)) {
    //                     if (!ts.isIdentifier(a) && !ts.isIdentifier(b) && a.right.escapedText === b.right.escapedText) {
    //                         a = a.left;
    //                         b = b.left;
    //                     }
    //                     else {
    //                         return false;
    //                     }
    //                 }
    //                 return a.escapedText === b.escapedText;
    //             }

    //             function parseChildPropertyTag(indent: number) {
    //                 return parseChildParameterOrPropertyTag(PropertyLikeParse.Property, indent) as JSDocTypeTag | JSDocPropertyTag | false;
    //             }

    //             function parseChildParameterOrPropertyTag(target: PropertyLikeParse, indent: number, name?: EntityName): JSDocTypeTag | JSDocPropertyTag | JSDocParameterTag | false {
    //                 let canParseTag = true;
    //                 let seenAsterisk = false;
    //                 while (true) {
    //                     switch (nextTokenJSDoc()) {
    //                         SyntaxKind::AtToken:
    //                             if (canParseTag) {
    //                                 const child = tryParseChildTag(target, indent);
    //                                 if (child && (child.kind === SyntaxKind::JSDocParameterTag || child.kind === SyntaxKind::JSDocPropertyTag) &&
    //                                     target !== PropertyLikeParse.CallbackParameter &&
    //                                     name && (ts.isIdentifier(child.name) || !escapedTextsEqual(name, child.name.left))) {
    //                                     return false;
    //                                 }
    //                                 return child;
    //                             }
    //                             seenAsterisk = false;
    //                             break;
    //                         SyntaxKind::NewLineTrivia:
    //                             canParseTag = true;
    //                             seenAsterisk = false;
    //                             break;
    //                         SyntaxKind::AsteriskToken:
    //                             if (seenAsterisk) {
    //                                 canParseTag = false;
    //                             }
    //                             seenAsterisk = true;
    //                             break;
    //                         SyntaxKind::Identifier:
    //                             canParseTag = false;
    //                             break;
    //                         SyntaxKind::EndOfFileToken:
    //                             return false;
    //                     }
    //                 }
    //             }

    //             function tryParseChildTag(target: PropertyLikeParse, indent: number): JSDocTypeTag | JSDocPropertyTag | JSDocParameterTag | false {
    //                 Debug.assert(token() === SyntaxKind::AtToken);
    //                 const start = scanner.getStartPos();
    //                 nextTokenJSDoc();

    //                 const tagName = parseJSDocIdentifierName();
    //                 skipWhitespace();
    //                 let t: PropertyLikeParse;
    //                 switch (tagName.escapedText) {
    //                     "type":
    //                         return target === PropertyLikeParse.Property && parseTypeTag(start, tagName);
    //                     "prop":
    //                     "property":
    //                         t = PropertyLikeParse.Property;
    //                         break;
    //                     "arg":
    //                     "argument":
    //                     "param":
    //                         t = PropertyLikeParse.Parameter | PropertyLikeParse.CallbackParameter;
    //                         break;
    //                     default:
    //                         return false;
    //                 }
    //                 if (!(target & t)) {
    //                     return false;
    //                 }
    //                 return parseParameterOrPropertyTag(start, tagName, target, indent);
    //             }

    //             function parseTemplateTagTypeParameter() {
    //                 const typeParameterPos = getNodePos();
    //                 const isBracketed = parseOptionalJsdoc(SyntaxKind::OpenBracketToken);
    //                 if (isBracketed) {
    //                     skipWhitespace();
    //                 }
    //                 const name = parseJSDocIdentifierName(Diagnostics::Unexpected_token_A_type_parameter_name_was_expected_without_curly_braces);

    //                 let defaultType: TypeNode | undefined;
    //                 if (isBracketed) {
    //                     skipWhitespace();
    //                     parseExpected(SyntaxKind::EqualsToken);
    //                     defaultType = doInsideOfContext(NodeFlags.JSDoc, parseJSDocType);
    //                     parseExpected(SyntaxKind::CloseBracketToken);
    //                 }

    //                 if (nodeIsMissing(name)) {
    //                     return undefined;
    //                 }
    //                 return finishNode(factory.createTypeParameterDeclaration(name, /*constraint*/ undefined, defaultType), typeParameterPos);
    //             }

    //             function parseTemplateTagTypeParameters() {
    //                 const pos = getNodePos();
    //                 const typeParameters = [];
    //                 do {
    //                     skipWhitespace();
    //                     const node = parseTemplateTagTypeParameter();
    //                     if (node !== undefined) {
    //                         typeParameters.push(node);
    //                     }
    //                     skipWhitespaceOrAsterisk();
    //                 } while (parseOptionalJsdoc(SyntaxKind::CommaToken));
    //                 return createNodeArray(typeParameters, pos);
    //             }

    //             function parseTemplateTag(start: number, tagName: Identifier, indent: number, indentText: string): JSDocTemplateTag {
    //                 // The template tag looks like one of the following:
    //                 //   @template T,U,V
    //                 //   @template {Constraint} T
    //                 //
    //                 // According to the [closure docs](https://github.com/google/closure-compiler/wiki/Generic-Types#multiple-bounded-template-types):
    //                 //   > Multiple bounded generics cannot be declared on the same line. For the sake of clarity, if multiple templates share the same
    //                 //   > type bound they must be declared on separate lines.
    //                 //
    //                 // TODO: Determine whether we should enforce this in the checker.
    //                 // TODO: Consider moving the `constraint` to the first type parameter as we could then remove `getEffectiveConstraintOfTypeParameter`.
    //                 // TODO: Consider only parsing a single type parameter if there is a constraint.
    //                 const constraint = token() === SyntaxKind::OpenBraceToken ? parseJSDocTypeExpression() : undefined;
    //                 const typeParameters = parseTemplateTagTypeParameters();
    //                 return finishNode(factory.createJSDocTemplateTag(tagName, constraint, typeParameters, parseTrailingTagComments(start, getNodePos(), indent, indentText)), start);
    //             }

    //             function parseOptionalJsdoc(t: JSDocSyntaxKind): boolean {
    //                 if (token() === t) {
    //                     nextTokenJSDoc();
    //                     return true;
    //                 }
    //                 return false;
    //             }

    //             function parseJSDocEntityName(): EntityName {
    //                 let entity: EntityName = parseJSDocIdentifierName();
    //                 if (parseOptional(SyntaxKind::OpenBracketToken)) {
    //                     parseExpected(SyntaxKind::CloseBracketToken);
    //                     // Note that y[] is accepted as an entity name, but the postfix brackets are not saved for checking.
    //                     // Technically usejsdoc.org requires them for specifying a property of a type equivalent to Array<{ x: ...}>
    //                     // but it's not worth it to enforce that restriction.
    //                 }
    //                 while (parseOptional(SyntaxKind::DotToken)) {
    //                     const name = parseJSDocIdentifierName();
    //                     if (parseOptional(SyntaxKind::OpenBracketToken)) {
    //                         parseExpected(SyntaxKind::CloseBracketToken);
    //                     }
    //                     entity = createQualifiedName(entity, name);
    //                 }
    //                 return entity;
    //             }

    //             function parseJSDocIdentifierName(message?: DiagnosticMessage): Identifier {
    //                 if (!tokenIsIdentifierOrKeyword(token())) {
    //                     return createMissingNode<Identifier>(SyntaxKind::Identifier, /*reportAtCurrentPosition*/ !message, message || Diagnostics::Identifier_expected);
    //                 }

    //                 identifierCount++;
    //                 const pos = scanner.getTokenPos();
    //                 const end = scanner.getTextPos();
    //                 const originalKeywordKind = token();
    //                 const text = internIdentifier(scanner.getTokenValue());
    //                 const result = finishNode(factory.createIdentifier(text, /*typeArguments*/ undefined, originalKeywordKind), pos, end);
    //                 nextTokenJSDoc();
    //                 return result;
    //             }
    //         }
    //     }
}

// namespace IncrementalParser {
//     export function updateSourceFile(sourceFile: SourceFile, newText: string, textChangeRange: TextChangeRange, aggressiveChecks: boolean): SourceFile {
//         aggressiveChecks = aggressiveChecks || Debug.shouldAssert(AssertionLevel.Aggressive);

//         checkChangeRange(sourceFile, newText, textChangeRange, aggressiveChecks);
//         if (textChangeRangeIsUnchanged(textChangeRange)) {
//             // if the text didn't change, then we can just return our current source file as-is.
//             return sourceFile;
//         }

//         if (sourceFile.statements.length === 0) {
//             // If we don't have any statements in the current source file, then there's no real
//             // way to incrementally parse.  So just do a full parse instead.
//             return Parser.parseSourceFile(sourceFile.fileName, newText, sourceFile.languageVersion, /*syntaxCursor*/ undefined, /*setParentNodes*/ true, sourceFile.scriptKind);
//         }

//         // Make sure we're not trying to incrementally update a source file more than once.  Once
//         // we do an update the original source file is considered unusable from that point onwards.
//         //
//         // This is because we do incremental parsing in-place.  i.e. we take nodes from the old
//         // tree and give them new positions and parents.  From that point on, trusting the old
//         // tree at all is not possible as far too much of it may violate invariants.
//         const incrementalSourceFile = sourceFile as Node as IncrementalNode;
//         Debug.assert(!incrementalSourceFile.hasBeenIncrementallyParsed);
//         incrementalSourceFile.hasBeenIncrementallyParsed = true;
//         Parser.fixupParentReferences(incrementalSourceFile);
//         const oldText = sourceFile.text;
//         const syntaxCursor = createSyntaxCursor(sourceFile);

//         // Make the actual change larger so that we know to reparse anything whose lookahead
//         // might have intersected the change.
//         const changeRange = extendToAffectedRange(sourceFile, textChangeRange);
//         checkChangeRange(sourceFile, newText, changeRange, aggressiveChecks);

//         // Ensure that extending the affected range only moved the start of the change range
//         // earlier in the file.
//         Debug.assert(changeRange.span.start <= textChangeRange.span.start);
//         Debug.assert(textSpanEnd(changeRange.span) === textSpanEnd(textChangeRange.span));
//         Debug.assert(textSpanEnd(textChangeRangeNewSpan(changeRange)) === textSpanEnd(textChangeRangeNewSpan(textChangeRange)));

//         // The is the amount the nodes after the edit range need to be adjusted.  It can be
//         // positive (if the edit added characters), negative (if the edit deleted characters)
//         // or zero (if this was a pure overwrite with nothing added/removed).
//         const delta = textChangeRangeNewSpan(changeRange).length - changeRange.span.length;

//         // If we added or removed characters during the edit, then we need to go and adjust all
//         // the nodes after the edit.  Those nodes may move forward (if we inserted chars) or they
//         // may move backward (if we deleted chars).
//         //
//         // Doing this helps us out in two ways.  First, it means that any nodes/tokens we want
//         // to reuse are already at the appropriate position in the new text.  That way when we
//         // reuse them, we don't have to figure out if they need to be adjusted.  Second, it makes
//         // it very easy to determine if we can reuse a node.  If the node's position is at where
//         // we are in the text, then we can reuse it.  Otherwise we can't.  If the node's position
//         // is ahead of us, then we'll need to rescan tokens.  If the node's position is behind
//         // us, then we'll need to skip it or crumble it as appropriate
//         //
//         // We will also adjust the positions of nodes that intersect the change range as well.
//         // By doing this, we ensure that all the positions in the old tree are consistent, not
//         // just the positions of nodes entirely before/after the change range.  By being
//         // consistent, we can then easily map from positions to nodes in the old tree easily.
//         //
//         // Also, mark any syntax elements that intersect the changed span.  We know, up front,
//         // that we cannot reuse these elements.
//         updateTokenPositionsAndMarkElements(incrementalSourceFile,
//             changeRange.span.start, textSpanEnd(changeRange.span), textSpanEnd(textChangeRangeNewSpan(changeRange)), delta, oldText, newText, aggressiveChecks);

//         // Now that we've set up our internal incremental state just proceed and parse the
//         // source file in the normal fashion.  When possible the parser will retrieve and
//         // reuse nodes from the old tree.
//         //
//         // Note: passing in 'true' for setNodeParents is very important.  When incrementally
//         // parsing, we will be reusing nodes from the old tree, and placing it into new
//         // parents.  If we don't set the parents now, we'll end up with an observably
//         // inconsistent tree.  Setting the parents on the new tree should be very fast.  We
//         // will immediately bail out of walking any subtrees when we can see that their parents
//         // are already correct.
//         const result = Parser.parseSourceFile(sourceFile.fileName, newText, sourceFile.languageVersion, syntaxCursor, /*setParentNodes*/ true, sourceFile.scriptKind);
//         result.commentDirectives = getNewCommentDirectives(
//             sourceFile.commentDirectives,
//             result.commentDirectives,
//             changeRange.span.start,
//             textSpanEnd(changeRange.span),
//             delta,
//             oldText,
//             newText,
//             aggressiveChecks
//         );
//         result.impliedNodeFormat = sourceFile.impliedNodeFormat;
//         return result;
//     }

//     function getNewCommentDirectives(
//         oldDirectives: CommentDirective[] | undefined,
//         newDirectives: CommentDirective[] | undefined,
//         changeStart: number,
//         changeRangeOldEnd: number,
//         delta: number,
//         oldText: string,
//         newText: string,
//         aggressiveChecks: boolean
//     ): CommentDirective[] | undefined {
//         if (!oldDirectives) return newDirectives;
//         let commentDirectives: CommentDirective[] | undefined;
//         let addedNewlyScannedDirectives = false;
//         for (const directive of oldDirectives) {
//             const { range, type } = directive;
//             // Range before the change
//             if (range.end < changeStart) {
//                 commentDirectives = append(commentDirectives, directive);
//             }
//             else if (range.pos > changeRangeOldEnd) {
//                 addNewlyScannedDirectives();
//                 // Node is entirely past the change range.  We need to move both its pos and
//                 // end, forward or backward appropriately.
//                 const updatedDirective: CommentDirective = {
//                     range: { pos: range.pos + delta, end: range.end + delta },
//                     type
//                 };
//                 commentDirectives = append(commentDirectives, updatedDirective);
//                 if (aggressiveChecks) {
//                     Debug.assert(oldText.substring(range.pos, range.end) === newText.substring(updatedDirective.range.pos, updatedDirective.range.end));
//                 }
//             }
//             // Ignore ranges that fall in change range
//         }
//         addNewlyScannedDirectives();
//         return commentDirectives;

//         function addNewlyScannedDirectives() {
//             if (addedNewlyScannedDirectives) return;
//             addedNewlyScannedDirectives = true;
//             if (!commentDirectives) {
//                 commentDirectives = newDirectives;
//             }
//             else if (newDirectives) {
//                 commentDirectives.push(...newDirectives);
//             }
//         }
//     }

//     function moveElementEntirelyPastChangeRange(element: IncrementalElement, isArray: boolean, delta: number, oldText: string, newText: string, aggressiveChecks: boolean) {
//         if (isArray) {
//             visitArray(element as IncrementalNodeArray);
//         }
//         else {
//             visitNode(element as IncrementalNode);
//         }
//         return;

//         function visitNode(node: IncrementalNode) {
//             let text = "";
//             if (aggressiveChecks && shouldCheckNode(node)) {
//                 text = oldText.substring(node.pos, node.end);
//             }

//             // Ditch any existing LS children we may have created.  This way we can avoid
//             // moving them forward.
//             if (node._children) {
//                 node._children = undefined;
//             }

//             setTextRangePosEnd(node, node.pos + delta, node.end + delta);

//             if (aggressiveChecks && shouldCheckNode(node)) {
//                 Debug.assert(text === newText.substring(node.pos, node.end));
//             }

//             forEachChild(node, visitNode, visitArray);
//             if (hasJSDocNodes(node)) {
//                 for (const jsDocComment of node.jsDoc!) {
//                     visitNode(jsDocComment as Node as IncrementalNode);
//                 }
//             }
//             checkNodePositions(node, aggressiveChecks);
//         }

//         function visitArray(array: IncrementalNodeArray) {
//             array._children = undefined;
//             setTextRangePosEnd(array, array.pos + delta, array.end + delta);

//             for (const node of array) {
//                 visitNode(node);
//             }
//         }
//     }

//     function shouldCheckNode(node: Node) {
//         switch (node.kind) {
//             SyntaxKind::StringLiteral:
//             SyntaxKind::NumericLiteral:
//             SyntaxKind::Identifier:
//                 return true;
//         }

//         return false;
//     }

//     function adjustIntersectingElement(element: IncrementalElement, changeStart: number, changeRangeOldEnd: number, changeRangeNewEnd: number, delta: number) {
//         Debug.assert(element.end >= changeStart, "Adjusting an element that was entirely before the change range");
//         Debug.assert(element.pos <= changeRangeOldEnd, "Adjusting an element that was entirely after the change range");
//         Debug.assert(element.pos <= element.end);

//         // We have an element that intersects the change range in some way.  It may have its
//         // start, or its end (or both) in the changed range.  We want to adjust any part
//         // that intersects such that the final tree is in a consistent state.  i.e. all
//         // children have spans within the span of their parent, and all siblings are ordered
//         // properly.

//         // We may need to update both the 'pos' and the 'end' of the element.

//         // If the 'pos' is before the start of the change, then we don't need to touch it.
//         // If it isn't, then the 'pos' must be inside the change.  How we update it will
//         // depend if delta is positive or negative. If delta is positive then we have
//         // something like:
//         //
//         //  -------------------AAA-----------------
//         //  -------------------BBBCCCCCCC-----------------
//         //
//         // In this case, we consider any node that started in the change range to still be
//         // starting at the same position.
//         //
//         // however, if the delta is negative, then we instead have something like this:
//         //
//         //  -------------------XXXYYYYYYY-----------------
//         //  -------------------ZZZ-----------------
//         //
//         // In this case, any element that started in the 'X' range will keep its position.
//         // However any element that started after that will have their pos adjusted to be
//         // at the end of the new range.  i.e. any node that started in the 'Y' range will
//         // be adjusted to have their start at the end of the 'Z' range.
//         //
//         // The element will keep its position if possible.  Or Move backward to the new-end
//         // if it's in the 'Y' range.
//         const pos = Math.min(element.pos, changeRangeNewEnd);

//         // If the 'end' is after the change range, then we always adjust it by the delta
//         // amount.  However, if the end is in the change range, then how we adjust it
//         // will depend on if delta is positive or negative.  If delta is positive then we
//         // have something like:
//         //
//         //  -------------------AAA-----------------
//         //  -------------------BBBCCCCCCC-----------------
//         //
//         // In this case, we consider any node that ended inside the change range to keep its
//         // end position.
//         //
//         // however, if the delta is negative, then we instead have something like this:
//         //
//         //  -------------------XXXYYYYYYY-----------------
//         //  -------------------ZZZ-----------------
//         //
//         // In this case, any element that ended in the 'X' range will keep its position.
//         // However any element that ended after that will have their pos adjusted to be
//         // at the end of the new range.  i.e. any node that ended in the 'Y' range will
//         // be adjusted to have their end at the end of the 'Z' range.
//         const end = element.end >= changeRangeOldEnd ?
//             // Element ends after the change range.  Always adjust the end pos.
//             element.end + delta :
//             // Element ends in the change range.  The element will keep its position if
//             // possible. Or Move backward to the new-end if it's in the 'Y' range.
//             Math.min(element.end, changeRangeNewEnd);

//         Debug.assert(pos <= end);
//         if (element.parent) {
//             Debug.assertGreaterThanOrEqual(pos, element.parent.pos);
//             Debug.assertLessThanOrEqual(end, element.parent.end);
//         }

//         setTextRangePosEnd(element, pos, end);
//     }

//     function checkNodePositions(node: Node, aggressiveChecks: boolean) {
//         if (aggressiveChecks) {
//             let pos = node.pos;
//             const visitNode = (child: Node) => {
//                 Debug.assert(child.pos >= pos);
//                 pos = child.end;
//             };
//             if (hasJSDocNodes(node)) {
//                 for (const jsDocComment of node.jsDoc!) {
//                     visitNode(jsDocComment);
//                 }
//             }
//             forEachChild(node, visitNode);
//             Debug.assert(pos <= node.end);
//         }
//     }

//     function updateTokenPositionsAndMarkElements(
//         sourceFile: IncrementalNode,
//         changeStart: number,
//         changeRangeOldEnd: number,
//         changeRangeNewEnd: number,
//         delta: number,
//         oldText: string,
//         newText: string,
//         aggressiveChecks: boolean): void {

//         visitNode(sourceFile);
//         return;

//         function visitNode(child: IncrementalNode) {
//             Debug.assert(child.pos <= child.end);
//             if (child.pos > changeRangeOldEnd) {
//                 // Node is entirely past the change range.  We need to move both its pos and
//                 // end, forward or backward appropriately.
//                 moveElementEntirelyPastChangeRange(child, /*isArray*/ false, delta, oldText, newText, aggressiveChecks);
//                 return;
//             }

//             // Check if the element intersects the change range.  If it does, then it is not
//             // reusable.  Also, we'll need to recurse to see what constituent portions we may
//             // be able to use.
//             const fullEnd = child.end;
//             if (fullEnd >= changeStart) {
//                 child.intersectsChange = true;
//                 child._children = undefined;

//                 // Adjust the pos or end (or both) of the intersecting element accordingly.
//                 adjustIntersectingElement(child, changeStart, changeRangeOldEnd, changeRangeNewEnd, delta);
//                 forEachChild(child, visitNode, visitArray);
//                 if (hasJSDocNodes(child)) {
//                     for (const jsDocComment of child.jsDoc!) {
//                         visitNode(jsDocComment as Node as IncrementalNode);
//                     }
//                 }
//                 checkNodePositions(child, aggressiveChecks);
//                 return;
//             }

//             // Otherwise, the node is entirely before the change range.  No need to do anything with it.
//             Debug.assert(fullEnd < changeStart);
//         }

//         function visitArray(array: IncrementalNodeArray) {
//             Debug.assert(array.pos <= array.end);
//             if (array.pos > changeRangeOldEnd) {
//                 // Array is entirely after the change range.  We need to move it, and move any of
//                 // its children.
//                 moveElementEntirelyPastChangeRange(array, /*isArray*/ true, delta, oldText, newText, aggressiveChecks);
//                 return;
//             }

//             // Check if the element intersects the change range.  If it does, then it is not
//             // reusable.  Also, we'll need to recurse to see what constituent portions we may
//             // be able to use.
//             const fullEnd = array.end;
//             if (fullEnd >= changeStart) {
//                 array.intersectsChange = true;
//                 array._children = undefined;

//                 // Adjust the pos or end (or both) of the intersecting array accordingly.
//                 adjustIntersectingElement(array, changeStart, changeRangeOldEnd, changeRangeNewEnd, delta);
//                 for (const node of array) {
//                     visitNode(node);
//                 }
//                 return;
//             }

//             // Otherwise, the array is entirely before the change range.  No need to do anything with it.
//             Debug.assert(fullEnd < changeStart);
//         }
//     }

//     function extendToAffectedRange(sourceFile: SourceFile, changeRange: TextChangeRange): TextChangeRange {
//         // Consider the following code:
//         //      void foo() { /; }
//         //
//         // If the text changes with an insertion of / just before the semicolon then we end up with:
//         //      void foo() { //; }
//         //
//         // If we were to just use the changeRange a is, then we would not rescan the { token
//         // (as it does not intersect the actual original change range).  Because an edit may
//         // change the token touching it, we actually need to look back *at least* one token so
//         // that the prior token sees that change.
//         const maxLookahead = 1;

//         let start = changeRange.span.start;

//         // the first iteration aligns us with the change start. subsequent iteration move us to
//         // the left by maxLookahead tokens.  We only need to do this as long as we're not at the
//         // start of the tree.
//         for (let i = 0; start > 0 && i <= maxLookahead; i++) {
//             const nearestNode = findNearestNodeStartingBeforeOrAtPosition(sourceFile, start);
//             Debug.assert(nearestNode.pos <= start);
//             const position = nearestNode.pos;

//             start = Math.max(0, position - 1);
//         }

//         const finalSpan = createTextSpanFromBounds(start, textSpanEnd(changeRange.span));
//         const finalLength = changeRange.newLength + (changeRange.span.start - start);

//         return createTextChangeRange(finalSpan, finalLength);
//     }

//     function findNearestNodeStartingBeforeOrAtPosition(sourceFile: SourceFile, position: number): Node {
//         let bestResult: Node = sourceFile;
//         let lastNodeEntirelyBeforePosition: Node | undefined;

//         forEachChild(sourceFile, visit);

//         if (lastNodeEntirelyBeforePosition) {
//             const lastChildOfLastEntireNodeBeforePosition = getLastDescendant(lastNodeEntirelyBeforePosition);
//             if (lastChildOfLastEntireNodeBeforePosition.pos > bestResult.pos) {
//                 bestResult = lastChildOfLastEntireNodeBeforePosition;
//             }
//         }

//         return bestResult;

//         function getLastDescendant(node: Node): Node {
//             while (true) {
//                 const lastChild = getLastChild(node);
//                 if (lastChild) {
//                     node = lastChild;
//                 }
//                 else {
//                     return node;
//                 }
//             }
//         }

//         function visit(child: Node) {
//             if (nodeIsMissing(child)) {
//                 // Missing nodes are effectively invisible to us.  We never even consider them
//                 // When trying to find the nearest node before us.
//                 return;
//             }

//             // If the child intersects this position, then this node is currently the nearest
//             // node that starts before the position.
//             if (child.pos <= position) {
//                 if (child.pos >= bestResult.pos) {
//                     // This node starts before the position, and is closer to the position than
//                     // the previous best node we found.  It is now the new best node.
//                     bestResult = child;
//                 }

//                 // Now, the node may overlap the position, or it may end entirely before the
//                 // position.  If it overlaps with the position, then either it, or one of its
//                 // children must be the nearest node before the position.  So we can just
//                 // recurse into this child to see if we can find something better.
//                 if (position < child.end) {
//                     // The nearest node is either this child, or one of the children inside
//                     // of it.  We've already marked this child as the best so far.  Recurse
//                     // in one of the children is better.
//                     forEachChild(child, visit);

//                     // Once we look at the children of this node, then there's no need to
//                     // continue any further.
//                     return true;
//                 }
//                 else {
//                     Debug.assert(child.end <= position);
//                     // The child ends entirely before this position.  Say you have the following
//                     // (where $ is the position)
//                     //
//                     //      <complex expr 1> ? <complex expr 2> $ : <...> <...>
//                     //
//                     // We would want to find the nearest preceding node in "complex expr 2".
//                     // To support that, we keep track of this node, and once we're done searching
//                     // for a best node, we recurse down this node to see if we can find a good
//                     // result in it.
//                     //
//                     // This approach allows us to quickly skip over nodes that are entirely
//                     // before the position, while still allowing us to find any nodes in the
//                     // last one that might be what we want.
//                     lastNodeEntirelyBeforePosition = child;
//                 }
//             }
//             else {
//                 Debug.assert(child.pos > position);
//                 // We're now at a node that is entirely past the position we're searching for.
//                 // This node (and all following nodes) could never contribute to the result,
//                 // so just skip them by returning 'true' here.
//                 return true;
//             }
//         }
//     }

//     function checkChangeRange(sourceFile: SourceFile, newText: string, textChangeRange: TextChangeRange, aggressiveChecks: boolean) {
//         const oldText = sourceFile.text;
//         if (textChangeRange) {
//             Debug.assert((oldText.length - textChangeRange.span.length + textChangeRange.newLength) === newText.length);

//             if (aggressiveChecks || Debug.shouldAssert(AssertionLevel.VeryAggressive)) {
//                 const oldTextPrefix = oldText.substr(0, textChangeRange.span.start);
//                 const newTextPrefix = newText.substr(0, textChangeRange.span.start);
//                 Debug.assert(oldTextPrefix === newTextPrefix);

//                 const oldTextSuffix = oldText.substring(textSpanEnd(textChangeRange.span), oldText.length);
//                 const newTextSuffix = newText.substring(textSpanEnd(textChangeRangeNewSpan(textChangeRange)), newText.length);
//                 Debug.assert(oldTextSuffix === newTextSuffix);
//             }
//         }
//     }

//     interface IncrementalElement extends ReadonlyTextRange {
//         readonly parent: Node;
//         intersectsChange: boolean;
//         length?: number;
//         _children: Node[] | undefined;
//     }

//     export interface IncrementalNode extends Node, IncrementalElement {
//         hasBeenIncrementallyParsed: boolean;
//     }

//     interface IncrementalNodeArray extends NodeArray<IncrementalNode>, IncrementalElement {
//         length: number;
//     }

//     // Allows finding nodes in the source file at a certain position in an efficient manner.
//     // The implementation takes advantage of the calling pattern it knows the parser will
//     // make in order to optimize finding nodes as quickly as possible.
//     export interface SyntaxCursor {
//         currentNode(position: number): IncrementalNode;
//     }

//     export function createSyntaxCursor(sourceFile: SourceFile): SyntaxCursor {
//         let currentArray: NodeArray<Node> = sourceFile.statements;
//         let currentArrayIndex = 0;

//         Debug.assert(currentArrayIndex < currentArray.length);
//         let current = currentArray[currentArrayIndex];
//         let lastQueriedPosition = InvalidPosition.Value;

//         return {
//             currentNode(position: number) {
//                 // Only compute the current node if the position is different than the last time
//                 // we were asked.  The parser commonly asks for the node at the same position
//                 // twice.  Once to know if can read an appropriate list element at a certain point,
//                 // and then to actually read and consume the node.
//                 if (position !== lastQueriedPosition) {
//                     // Much of the time the parser will need the very next node in the array that
//                     // we just returned a node from.So just simply check for that and move
//                     // forward in the array instead of searching for the node again.
//                     if (current && current.end === position && currentArrayIndex < (currentArray.length - 1)) {
//                         currentArrayIndex++;
//                         current = currentArray[currentArrayIndex];
//                     }

//                     // If we don't have a node, or the node we have isn't in the right position,
//                     // then try to find a viable node at the position requested.
//                     if (!current || current.pos !== position) {
//                         findHighestListElementThatStartsAtPosition(position);
//                     }
//                 }

//                 // Cache this query so that we don't do any extra work if the parser calls back
//                 // into us.  Note: this is very common as the parser will make pairs of calls like
//                 // 'isListElement -> parseListElement'.  If we were unable to find a node when
//                 // called with 'isListElement', we don't want to redo the work when parseListElement
//                 // is called immediately after.
//                 lastQueriedPosition = position;

//                 // Either we don'd have a node, or we have a node at the position being asked for.
//                 Debug.assert(!current || current.pos === position);
//                 return current as IncrementalNode;
//             }
//         };

//         // Finds the highest element in the tree we can find that starts at the provided position.
//         // The element must be a direct child of some node list in the tree.  This way after we
//         // return it, we can easily return its next sibling in the list.
//         function findHighestListElementThatStartsAtPosition(position: number) {
//             // Clear out any cached state about the last node we found.
//             currentArray = undefined!;
//             currentArrayIndex = InvalidPosition.Value;
//             current = undefined!;

//             // Recurse into the source file to find the highest node at this position.
//             forEachChild(sourceFile, visitNode, visitArray);
//             return;

//             function visitNode(node: Node) {
//                 if (position >= node.pos && position < node.end) {
//                     // Position was within this node.  Keep searching deeper to find the node.
//                     forEachChild(node, visitNode, visitArray);

//                     // don't proceed any further in the search.
//                     return true;
//                 }

//                 // position wasn't in this node, have to keep searching.
//                 return false;
//             }

//             function visitArray(array: NodeArray<Node>) {
//                 if (position >= array.pos && position < array.end) {
//                     // position was in this array.  Search through this array to see if we find a
//                     // viable element.
//                     for (let i = 0; i < array.length; i++) {
//                         const child = array[i];
//                         if (child) {
//                             if (child.pos === position) {
//                                 // Found the right node.  We're done.
//                                 currentArray = array;
//                                 currentArrayIndex = i;
//                                 current = child;
//                                 return true;
//                             }
//                             else {
//                                 if (child.pos < position && position < child.end) {
//                                     // Position in somewhere within this child.  Search in it and
//                                     // stop searching in this array.
//                                     forEachChild(child, visitNode, visitArray);
//                                     return true;
//                                 }
//                             }
//                         }
//                     }
//                 }

//                 // position wasn't in this array, have to keep searching.
//                 return false;
//             }
//         }
//     }

//     const enum InvalidPosition {
//         Value = -1
//     }
// }

fn isDeclarationFileName(fileName: &str) -> bool {
    // TODO:
    false
    // return fileExtensionIsOneOf(fileName, [Extension.Dts, Extension.Dmts, Extension.Dcts]);
}

// /*@internal*/
// export interface PragmaContext {
//     languageVersion: ScriptTarget;
//     pragmas?: PragmaMap;
//     checkJsDirective?: CheckJsDirective;
//     referencedFiles: FileReference[];
//     typeReferenceDirectives: FileReference[];
//     libReferenceDirectives: FileReference[];
//     amdDependencies: AmdDependency[];
//     hasNoDefaultLib?: boolean;
//     moduleName?: string;
// }

// /*@internal*/
// export function processCommentPragmas(context: PragmaContext, sourceText: string): void {
//     const pragmas: PragmaPseudoMapEntry[] = [];

//     for (const range of getLeadingCommentRanges(sourceText, 0) || emptyArray) {
//         const comment = sourceText.substring(range.pos, range.end);
//         extractPragmas(pragmas, range, comment);
//     }

//     context.pragmas = new Map() as PragmaMap;
//     for (const pragma of pragmas) {
//         if (context.pragmas.has(pragma.name)) {
//             const currentValue = context.pragmas.get(pragma.name);
//             if (currentValue instanceof Array) {
//                 currentValue.push(pragma.args);
//             }
//             else {
//                 context.pragmas.set(pragma.name, [currentValue, pragma.args]);
//             }
//             continue;
//         }
//         context.pragmas.set(pragma.name, pragma.args);
//     }
// }

// /*@internal*/
// type PragmaDiagnosticReporter = (pos: number, length: number, message: DiagnosticMessage) => void;

// /*@internal*/
// export function processPragmasIntoFields(context: PragmaContext, reportDiagnostic: PragmaDiagnosticReporter): void {
//     context.checkJsDirective = undefined;
//     context.referencedFiles = [];
//     context.typeReferenceDirectives = [];
//     context.libReferenceDirectives = [];
//     context.amdDependencies = [];
//     context.hasNoDefaultLib = false;
//     context.pragmas!.forEach((entryOrList, key) => { // TODO: GH#18217
//         // TODO: The below should be strongly type-guarded and not need casts/explicit annotations, since entryOrList is related to
//         // key and key is constrained to a union; but it's not (see GH#21483 for at least partial fix) :(
//         switch (key) {
//             "reference": {
//                 const referencedFiles = context.referencedFiles;
//                 const typeReferenceDirectives = context.typeReferenceDirectives;
//                 const libReferenceDirectives = context.libReferenceDirectives;
//                 forEach(toArray(entryOrList) as PragmaPseudoMap["reference"][], arg => {
//                     const { types, lib, path } = arg.arguments;
//                     if (arg.arguments["no-default-lib"]) {
//                         context.hasNoDefaultLib = true;
//                     }
//                     else if (types) {
//                         typeReferenceDirectives.push({ pos: types.pos, end: types.end, fileName: types.value });
//                     }
//                     else if (lib) {
//                         libReferenceDirectives.push({ pos: lib.pos, end: lib.end, fileName: lib.value });
//                     }
//                     else if (path) {
//                         referencedFiles.push({ pos: path.pos, end: path.end, fileName: path.value });
//                     }
//                     else {
//                         reportDiagnostic(arg.range.pos, arg.range.end - arg.range.pos, Diagnostics::Invalid_reference_directive_syntax);
//                     }
//                 });
//                 break;
//             }
//             "amd-dependency": {
//                 context.amdDependencies = map(
//                     toArray(entryOrList) as PragmaPseudoMap["amd-dependency"][],
//                     x => ({ name: x.arguments.name, path: x.arguments.path }));
//                 break;
//             }
//             "amd-module": {
//                 if (entryOrList instanceof Array) {
//                     for (const entry of entryOrList) {
//                         if (context.moduleName) {
//                             // TODO: It's probably fine to issue this diagnostic on all instances of the pragma
//                             reportDiagnostic(entry.range.pos, entry.range.end - entry.range.pos, Diagnostics::An_AMD_module_cannot_have_multiple_name_assignments);
//                         }
//                         context.moduleName = (entry as PragmaPseudoMap["amd-module"]).arguments.name;
//                     }
//                 }
//                 else {
//                     context.moduleName = (entryOrList as PragmaPseudoMap["amd-module"]).arguments.name;
//                 }
//                 break;
//             }
//             "ts-nocheck":
//             "ts-check": {
//                 // _last_ of either nocheck or check in a file is the "winner"
//                 forEach(toArray(entryOrList), entry => {
//                     if (!context.checkJsDirective || entry.range.pos > context.checkJsDirective.pos) {
//                         context.checkJsDirective = {
//                             enabled: key === "ts-check",
//                             end: entry.range.end,
//                             pos: entry.range.pos
//                         };
//                     }
//                 });
//                 break;
//             }
//             "jsx":
//             "jsxfrag":
//             "jsximportsource":
//             "jsxruntime":
//                 return; // Accessed directly
//             default: Debug.fail("Unhandled pragma kind"); // Can this be made into an assertNever in the future?
//         }
//     });
// }

// const namedArgRegExCache = new Map<string, RegExp>();
// function getNamedArgRegEx(name: string): RegExp {
//     if (namedArgRegExCache.has(name)) {
//         return namedArgRegExCache.get(name)!;
//     }
//     const result = new RegExp(`(\\s${name}\\s*=\\s*)(?:(?:'([^']*)')|(?:"([^"]*)"))`, "im");
//     namedArgRegExCache.set(name, result);
//     return result;
// }

// const tripleSlashXMLCommentStartRegEx = /^\/\/\/\s*<(\S+)\s.*?\/>/im;
// const singleLinePragmaRegEx = /^\/\/\/?\s*@(\S+)\s*(.*)\s*$/im;
// function extractPragmas(pragmas: PragmaPseudoMapEntry[], range: CommentRange, text: string) {
//     const tripleSlash = range.kind === SyntaxKind::SingleLineCommentTrivia && tripleSlashXMLCommentStartRegEx.exec(text);
//     if (tripleSlash) {
//         const name = tripleSlash[1].toLowerCase() as keyof PragmaPseudoMap; // Technically unsafe cast, but we do it so the below check to make it safe typechecks
//         const pragma = commentPragmas[name] as PragmaDefinition;
//         if (!pragma || !(pragma.kind! & PragmaKindFlags.TripleSlashXML)) {
//             return;
//         }
//         if (pragma.args) {
//             const argument: {[index: string]: string | {value: string, pos: number, end: number}} = {};
//             for (const arg of pragma.args) {
//                 const matcher = getNamedArgRegEx(arg.name);
//                 const matchResult = matcher.exec(text);
//                 if (!matchResult && !arg.optional) {
//                     return; // Missing required argument, don't parse
//                 }
//                 else if (matchResult) {
//                     const value = matchResult[2] || matchResult[3];
//                     if (arg.captureSpan) {
//                         const startPos = range.pos + matchResult.index + matchResult[1].length + 1;
//                         argument[arg.name] = {
//                             value,
//                             pos: startPos,
//                             end: startPos + value.length
//                         };
//                     }
//                     else {
//                         argument[arg.name] = value;
//                     }
//                 }
//             }
//             pragmas.push({ name, args: { arguments: argument, range } } as PragmaPseudoMapEntry);
//         }
//         else {
//             pragmas.push({ name, args: { arguments: {}, range } } as PragmaPseudoMapEntry);
//         }
//         return;
//     }

//     const singleLine = range.kind === SyntaxKind::SingleLineCommentTrivia && singleLinePragmaRegEx.exec(text);
//     if (singleLine) {
//         return addPragmaForMatch(pragmas, range, PragmaKindFlags.SingleLine, singleLine);
//     }

//     if (range.kind === SyntaxKind::MultiLineCommentTrivia) {
//         const multiLinePragmaRegEx = /@(\S+)(\s+.*)?$/gim; // Defined inline since it uses the "g" flag, which keeps a persistent index (for iterating)
//         let multiLineMatch: RegExpExecArray | null;
//         while (multiLineMatch = multiLinePragmaRegEx.exec(text)) {
//             addPragmaForMatch(pragmas, range, PragmaKindFlags.MultiLine, multiLineMatch);
//         }
//     }
// }

// function addPragmaForMatch(pragmas: PragmaPseudoMapEntry[], range: CommentRange, kind: PragmaKindFlags, match: RegExpExecArray) {
//     if (!match) return;
//     const name = match[1].toLowerCase() as keyof PragmaPseudoMap; // Technically unsafe cast, but we do it so they below check to make it safe typechecks
//     const pragma = commentPragmas[name] as PragmaDefinition;
//     if (!pragma || !(pragma.kind! & kind)) {
//         return;
//     }
//     const args = match[2]; // Split on spaces and match up positionally with definition
//     const argument = getNamedPragmaArguments(pragma, args);
//     if (argument === "fail") return; // Missing required argument, fail to parse it
//     pragmas.push({ name, args: { arguments: argument, range } } as PragmaPseudoMapEntry);
//     return;
// }

// function getNamedPragmaArguments(pragma: PragmaDefinition, text: string | undefined): {[index: string]: string} | "fail" {
//     if (!text) return {};
//     if (!pragma.args) return {};
//     const args = trimString(text).split(/\s+/);
//     const argMap: {[index: string]: string} = {};
//     for (let i = 0; i < pragma.args.length; i++) {
//         const argument = pragma.args[i];
//         if (!args[i] && !argument.optional) {
//             return "fail";
//         }
//         if (argument.captureSpan) {
//             return Debug.fail("Capture spans not yet implemented for non-xml pragmas");
//         }
//         argMap[argument.name] = args[i];
//     }
//     return argMap;
// }

// /** @internal */
// export function tagNamesAreEquivalent(lhs: JsxTagNameExpression, rhs: JsxTagNameExpression): boolean {
//     if (lhs.kind !== rhs.kind) {
//         return false;
//     }

//     if (lhs.kind === SyntaxKind::Identifier) {
//         return lhs.escapedText === (rhs as Identifier).escapedText;
//     }

//     if (lhs.kind === SyntaxKind::ThisKeyword) {
//         return true;
//     }

//     // If we are at this statement then we must have PropertyAccessExpression and because tag name in Jsx element can only
//     // take forms of JsxTagNameExpression which includes an identifier, "this" expression, or another propertyAccessExpression
//     // it is safe to the expression property as such. See parseJsxElementName for how we parse tag name in Jsx element
//     return (lhs as PropertyAccessExpression).name.escapedText === (rhs as PropertyAccessExpression).name.escapedText &&
//         tagNamesAreEquivalent((lhs as PropertyAccessExpression).expression as JsxTagNameExpression, (rhs as PropertyAccessExpression).expression as JsxTagNameExpression);
// }

bitflags! {
    pub struct ParsingContext: u32 {
        const None =                     0;
        const SourceElements =           1 << 0; // Elements in source file
        const BlockStatements =          1 << 1; // Statements in block
        const SwitchClauses =            1 << 2; // Clauses in switch statement
        const SwitchClauseStatements =   1 << 3; // Statements in switch clause
        const TypeMembers =              1 << 4; // Members in interface or type literal
        const ClassMembers =             1 << 5; // Members in class declaration
        const EnumMembers =              1 << 6; // Members in enum declaration
        const HeritageClauseElement =    1 << 7; // Elements in a heritage clause
        const VariableDeclarations =     1 << 8; // Variable declarations in variable statement
        const ObjectBindingElements =    1 << 9; // Binding elements in object binding list
        const ArrayBindingElements =     1 << 10; // Binding elements in array binding list
        const ArgumentExpressions =      1 << 11; // Expressions in argument list
        const ObjectLiteralMembers =     1 << 12; // Members in object literal
        const JsxAttributes =            1 << 13; // Attributes in jsx element
        const JsxChildren =              1 << 14; // Things between opening and closing JSX tags
        const ArrayLiteralMembers =      1 << 15; // Members in array literal
        const Parameters =               1 << 16; // Parameters in parameter list
        const JSDocParameters =          1 << 17; // JSDoc parameters in parameter list of JSDoc function type
        const RestProperties =           1 << 18; // Property names in a rest type list
        const TypeParameters =           1 << 19; // Type parameters in type parameter list
        const TypeArguments =            1 << 20; // Type arguments in type argument list
        const TupleElementTypes =        1 << 21; // Element types in tuple element type list
        const HeritageClauses =          1 << 22; // Heritage clauses for a class or interface declaration.
        const ImportOrExportSpecifiers = 1 << 23; // Named import clause's import specifier list,
        const AssertEntries =            1 << 24; // Import entries list.
        // const Count =                    1 << 25; // Number of parsing contexts
    }
}

// TODO: temp placeholder
#[derive(Clone)]
struct SyntaxCursor {}

type MissingList<T> = NodeArray<T>;
