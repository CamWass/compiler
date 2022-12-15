use std::rc::Rc;

use crate::node::*;
use crate::our_types::*;
use crate::scanner::stringToToken;
use crate::types::*;
use crate::utilities::*;
use crate::utilitiesPublic::*;

use bitflags::bitflags;
use rustc_hash::FxHashMap;
use swc_atoms::JsWord;

use super::nodeTests::*;
use super::utilities::*;
use super::utilitiesPublic::setTextRange;

#[derive(Default)]
pub struct NodeFactory {
    flags: NodeFactoryFlags,
    node_data: FxHashMap<NodeId, NodeData>,
    node_id_gen: NodeIdGen,
}

impl NodeFactory {
    pub fn node_data<T: HasNodeId>(&mut self, node: T) -> &NodeData {
        self.node_data.entry(node.node_id()).or_default()
    }

    pub fn node_data_mut<T: HasNodeId>(&mut self, node: T) -> &mut NodeData {
        self.node_data.entry(node.node_id()).or_default()
    }

    pub fn node_data_pick_2_mut<T, U>(
        &mut self,
        node1: T,
        node2: U,
    ) -> (&mut NodeData, &mut NodeData)
    where
        T: HasNodeId,
        U: HasNodeId,
    {
        let key1 = node1.node_id();
        let key2 = node2.node_id();
        self.node_data.entry(key1).or_default();
        self.node_data.entry(key2).or_default();

        let [a, b] = self
            .node_data
            .get_many_mut([&key1, &key2])
            .expect("keys should exist and be unique");
        (a, b)
    }

    /**
     * Creates a `NodeFactory` that can be used to create and update a syntax tree.
     * @param flags Flags that control factory behavior.
     * @param baseFactory A `BaseNodeFactory` used to create the base `Node` objects.
     */
    pub fn createNodeFactory(flags: NodeFactoryFlags) -> Self {
        // let update = if flags.intersects(NodeFactoryFlags::NoOriginalNode) {
        //     updateWithoutOriginal
        // } else {
        //     updateWithOriginal
        // };

        // // Lazily load the parenthesizer, node converters, and some factory methods until they are used.
        // const parenthesizerRules = memoize(() => flags & NodeFactoryFlags.NoParenthesizerRules ? nullParenthesizerRules : createParenthesizerRules(factory));
        // const converters = memoize(() => flags & NodeFactoryFlags.NoNodeConverters ? nullNodeConverters : createNodeConverters(factory));

        // // lazy initializaton of common operator factories
        // const getBinaryCreateFunction = memoizeOne((operator: BinaryOperator) => (left: Expression, right: Expression) => createBinaryExpression(left, operator, right));
        // const getPrefixUnaryCreateFunction = memoizeOne((operator: PrefixUnaryOperator) => (operand: Expression) => createPrefixUnaryExpression(operator, operand));
        // const getPostfixUnaryCreateFunction = memoizeOne((operator: PostfixUnaryOperator) => (operand: Expression) => createPostfixUnaryExpression(operand, operator));
        // const getJSDocPrimaryTypeCreateFunction = memoizeOne(<T extends JSDocType>(kind: T["kind"]) => () => createJSDocPrimaryTypeWorker(kind));
        // const getJSDocUnaryTypeCreateFunction = memoizeOne(<T extends JSDocType & { readonly type: TypeNode | undefined; }>(kind: T["kind"]) => (type: T["type"]) => createJSDocUnaryTypeWorker<T>(kind, type));
        // const getJSDocUnaryTypeUpdateFunction = memoizeOne(<T extends JSDocType & { readonly type: TypeNode | undefined; }>(kind: T["kind"]) => (node: T, type: T["type"]) => updateJSDocUnaryTypeWorker<T>(kind, node, type));
        // const getJSDocSimpleTagCreateFunction = memoizeOne(<T extends JSDocTag>(kind: T["kind"]) => (tagName: Identifier | undefined, comment?: NodeArray<JSDocComment>) => createJSDocSimpleTagWorker(kind, tagName, comment));
        // const getJSDocSimpleTagUpdateFunction = memoizeOne(<T extends JSDocTag>(kind: T["kind"]) => (node: T, tagName: Identifier | undefined, comment?: NodeArray<JSDocComment>) => updateJSDocSimpleTagWorker(kind, node, tagName, comment));
        // const getJSDocTypeLikeTagCreateFunction = memoizeOne(<T extends JSDocTag & { typeExpression?: JSDocTypeExpression }>(kind: T["kind"]) => (tagName: Identifier | undefined, typeExpression?: JSDocTypeExpression, comment?: NodeArray<JSDocComment>) => createJSDocTypeLikeTagWorker(kind, tagName, typeExpression, comment));
        // const getJSDocTypeLikeTagUpdateFunction = memoizeOne(<T extends JSDocTag & { typeExpression?: JSDocTypeExpression }>(kind: T["kind"]) => (node: T, tagName: Identifier | undefined, typeExpression?: JSDocTypeExpression, comment?: NodeArray<JSDocComment>) => updateJSDocTypeLikeTagWorker(kind, node, tagName, typeExpression, comment));

        let factory = NodeFactory {
            flags,
            ..NodeFactory::default()
        };
        factory

        // const factory: NodeFactory = {
        //     get parenthesizer() { return parenthesizerRules(); },
        //     get converters() { return converters(); },
        //     createNodeArray,
        //     createNumericLiteral,
        //     createBigIntLiteral,
        //     createStringLiteral,
        //     createStringLiteralFromNode,
        //     createRegularExpressionLiteral,
        //     createLiteralLikeNode,
        //     createIdentifier,
        //     updateIdentifier,
        //     createTempVariable,
        //     createLoopVariable,
        //     createUniqueName,
        //     getGeneratedNameForNode,
        //     createPrivateIdentifier,
        //     createToken,
        //     createSuper,
        //     createThis,
        //     createNull,
        //     createTrue,
        //     createFalse,
        //     createModifier,
        //     createModifiersFromModifierFlags,
        //     createQualifiedName,
        //     updateQualifiedName,
        //     createComputedPropertyName,
        //     updateComputedPropertyName,
        //     createTypeParameterDeclaration,
        //     updateTypeParameterDeclaration,
        //     createParameterDeclaration,
        //     updateParameterDeclaration,
        //     createDecorator,
        //     updateDecorator,
        //     createPropertySignature,
        //     updatePropertySignature,
        //     createPropertyDeclaration,
        //     updatePropertyDeclaration,
        //     createMethodSignature,
        //     updateMethodSignature,
        //     createMethodDeclaration,
        //     updateMethodDeclaration,
        //     createConstructorDeclaration,
        //     updateConstructorDeclaration,
        //     createGetAccessorDeclaration,
        //     updateGetAccessorDeclaration,
        //     createSetAccessorDeclaration,
        //     updateSetAccessorDeclaration,
        //     createCallSignature,
        //     updateCallSignature,
        //     createConstructSignature,
        //     updateConstructSignature,
        //     createIndexSignature,
        //     updateIndexSignature,
        //     createClassStaticBlockDeclaration,
        //     updateClassStaticBlockDeclaration,
        //     createTemplateLiteralTypeSpan,
        //     updateTemplateLiteralTypeSpan,
        //     createKeywordTypeNode,
        //     createTypePredicateNode,
        //     updateTypePredicateNode,
        //     createTypeReferenceNode,
        //     updateTypeReferenceNode,
        //     createFunctionTypeNode,
        //     updateFunctionTypeNode,
        //     createConstructorTypeNode,
        //     updateConstructorTypeNode,
        //     createTypeQueryNode,
        //     updateTypeQueryNode,
        //     createTypeLiteralNode,
        //     updateTypeLiteralNode,
        //     createArrayTypeNode,
        //     updateArrayTypeNode,
        //     createTupleTypeNode,
        //     updateTupleTypeNode,
        //     createNamedTupleMember,
        //     updateNamedTupleMember,
        //     createOptionalTypeNode,
        //     updateOptionalTypeNode,
        //     createRestTypeNode,
        //     updateRestTypeNode,
        //     createUnionTypeNode,
        //     updateUnionTypeNode,
        //     createIntersectionTypeNode,
        //     updateIntersectionTypeNode,
        //     createConditionalTypeNode,
        //     updateConditionalTypeNode,
        //     createInferTypeNode,
        //     updateInferTypeNode,
        //     createImportTypeNode,
        //     updateImportTypeNode,
        //     createParenthesizedType,
        //     updateParenthesizedType,
        //     createThisTypeNode,
        //     createTypeOperatorNode,
        //     updateTypeOperatorNode,
        //     createIndexedAccessTypeNode,
        //     updateIndexedAccessTypeNode,
        //     createMappedTypeNode,
        //     updateMappedTypeNode,
        //     createLiteralTypeNode,
        //     updateLiteralTypeNode,
        //     createTemplateLiteralType,
        //     updateTemplateLiteralType,
        //     createObjectBindingPattern,
        //     updateObjectBindingPattern,
        //     createArrayBindingPattern,
        //     updateArrayBindingPattern,
        //     createBindingElement,
        //     updateBindingElement,
        //     createArrayLiteralExpression,
        //     updateArrayLiteralExpression,
        //     createObjectLiteralExpression,
        //     updateObjectLiteralExpression,
        //     createPropertyAccessExpression: flags & NodeFactoryFlags.NoIndentationOnFreshPropertyAccess ?
        //         (expression, name) => setEmitFlags(createPropertyAccessExpression(expression, name), EmitFlags.NoIndentation) :
        //         createPropertyAccessExpression,
        //     updatePropertyAccessExpression,
        //     createPropertyAccessChain: flags & NodeFactoryFlags.NoIndentationOnFreshPropertyAccess ?
        //         (expression, questionDotToken, name) => setEmitFlags(createPropertyAccessChain(expression, questionDotToken, name), EmitFlags.NoIndentation) :
        //         createPropertyAccessChain,
        //     updatePropertyAccessChain,
        //     createElementAccessExpression,
        //     updateElementAccessExpression,
        //     createElementAccessChain,
        //     updateElementAccessChain,
        //     createCallExpression,
        //     updateCallExpression,
        //     createCallChain,
        //     updateCallChain,
        //     createNewExpression,
        //     updateNewExpression,
        //     createTaggedTemplateExpression,
        //     updateTaggedTemplateExpression,
        //     createTypeAssertion,
        //     updateTypeAssertion,
        //     createParenthesizedExpression,
        //     updateParenthesizedExpression,
        //     createFunctionExpression,
        //     updateFunctionExpression,
        //     createArrowFunction,
        //     updateArrowFunction,
        //     createDeleteExpression,
        //     updateDeleteExpression,
        //     createTypeOfExpression,
        //     updateTypeOfExpression,
        //     createVoidExpression,
        //     updateVoidExpression,
        //     createAwaitExpression,
        //     updateAwaitExpression,
        //     createPrefixUnaryExpression,
        //     updatePrefixUnaryExpression,
        //     createPostfixUnaryExpression,
        //     updatePostfixUnaryExpression,
        //     createBinaryExpression,
        //     updateBinaryExpression,
        //     createConditionalExpression,
        //     updateConditionalExpression,
        //     createTemplateExpression,
        //     updateTemplateExpression,
        //     createTemplateHead,
        //     createTemplateMiddle,
        //     createTemplateTail,
        //     createNoSubstitutionTemplateLiteral,
        //     createTemplateLiteralLikeNode,
        //     createYieldExpression,
        //     updateYieldExpression,
        //     createSpreadElement,
        //     updateSpreadElement,
        //     createClassExpression,
        //     updateClassExpression,
        //     createOmittedExpression,
        //     createExpressionWithTypeArguments,
        //     updateExpressionWithTypeArguments,
        //     createAsExpression,
        //     updateAsExpression,
        //     createNonNullExpression,
        //     updateNonNullExpression,
        //     createNonNullChain,
        //     updateNonNullChain,
        //     createMetaProperty,
        //     updateMetaProperty,
        //     createTemplateSpan,
        //     updateTemplateSpan,
        //     createSemicolonClassElement,
        //     createBlock,
        //     updateBlock,
        //     createVariableStatement,
        //     updateVariableStatement,
        //     createEmptyStatement,
        //     createExpressionStatement,
        //     updateExpressionStatement,
        //     createIfStatement,
        //     updateIfStatement,
        //     createDoStatement,
        //     updateDoStatement,
        //     createWhileStatement,
        //     updateWhileStatement,
        //     createForStatement,
        //     updateForStatement,
        //     createForInStatement,
        //     updateForInStatement,
        //     createForOfStatement,
        //     updateForOfStatement,
        //     createContinueStatement,
        //     updateContinueStatement,
        //     createBreakStatement,
        //     updateBreakStatement,
        //     createReturnStatement,
        //     updateReturnStatement,
        //     createWithStatement,
        //     updateWithStatement,
        //     createSwitchStatement,
        //     updateSwitchStatement,
        //     createLabeledStatement,
        //     updateLabeledStatement,
        //     createThrowStatement,
        //     updateThrowStatement,
        //     createTryStatement,
        //     updateTryStatement,
        //     createDebuggerStatement,
        //     createVariableDeclaration,
        //     updateVariableDeclaration,
        //     createVariableDeclarationList,
        //     updateVariableDeclarationList,
        //     createFunctionDeclaration,
        //     updateFunctionDeclaration,
        //     createClassDeclaration,
        //     updateClassDeclaration,
        //     createInterfaceDeclaration,
        //     updateInterfaceDeclaration,
        //     createTypeAliasDeclaration,
        //     updateTypeAliasDeclaration,
        //     createEnumDeclaration,
        //     updateEnumDeclaration,
        //     createModuleDeclaration,
        //     updateModuleDeclaration,
        //     createModuleBlock,
        //     updateModuleBlock,
        //     createCaseBlock,
        //     updateCaseBlock,
        //     createNamespaceExportDeclaration,
        //     updateNamespaceExportDeclaration,
        //     createImportEqualsDeclaration,
        //     updateImportEqualsDeclaration,
        //     createImportDeclaration,
        //     updateImportDeclaration,
        //     createImportClause,
        //     updateImportClause,
        //     createAssertClause,
        //     updateAssertClause,
        //     createAssertEntry,
        //     updateAssertEntry,
        //     createNamespaceImport,
        //     updateNamespaceImport,
        //     createNamespaceExport,
        //     updateNamespaceExport,
        //     createNamedImports,
        //     updateNamedImports,
        //     createImportSpecifier,
        //     updateImportSpecifier,
        //     createExportAssignment,
        //     updateExportAssignment,
        //     createExportDeclaration,
        //     updateExportDeclaration,
        //     createNamedExports,
        //     updateNamedExports,
        //     createExportSpecifier,
        //     updateExportSpecifier,
        //     createMissingDeclaration,
        //     createExternalModuleReference,
        //     updateExternalModuleReference,
        //     // lazily load factory members for JSDoc types with similar structure
        //     get createJSDocAllType() { return getJSDocPrimaryTypeCreateFunction<JSDocAllType>(SyntaxKind::JSDocAllType); },
        //     get createJSDocUnknownType() { return getJSDocPrimaryTypeCreateFunction<JSDocUnknownType>(SyntaxKind::JSDocUnknownType); },
        //     get createJSDocNonNullableType() { return getJSDocUnaryTypeCreateFunction<JSDocNonNullableType>(SyntaxKind::JSDocNonNullableType); },
        //     get updateJSDocNonNullableType() { return getJSDocUnaryTypeUpdateFunction<JSDocNonNullableType>(SyntaxKind::JSDocNonNullableType); },
        //     get createJSDocNullableType() { return getJSDocUnaryTypeCreateFunction<JSDocNullableType>(SyntaxKind::JSDocNullableType); },
        //     get updateJSDocNullableType() { return getJSDocUnaryTypeUpdateFunction<JSDocNullableType>(SyntaxKind::JSDocNullableType); },
        //     get createJSDocOptionalType() { return getJSDocUnaryTypeCreateFunction<JSDocOptionalType>(SyntaxKind::JSDocOptionalType); },
        //     get updateJSDocOptionalType() { return getJSDocUnaryTypeUpdateFunction<JSDocOptionalType>(SyntaxKind::JSDocOptionalType); },
        //     get createJSDocVariadicType() { return getJSDocUnaryTypeCreateFunction<JSDocVariadicType>(SyntaxKind::JSDocVariadicType); },
        //     get updateJSDocVariadicType() { return getJSDocUnaryTypeUpdateFunction<JSDocVariadicType>(SyntaxKind::JSDocVariadicType); },
        //     get createJSDocNamepathType() { return getJSDocUnaryTypeCreateFunction<JSDocNamepathType>(SyntaxKind::JSDocNamepathType); },
        //     get updateJSDocNamepathType() { return getJSDocUnaryTypeUpdateFunction<JSDocNamepathType>(SyntaxKind::JSDocNamepathType); },
        //     createJSDocFunctionType,
        //     updateJSDocFunctionType,
        //     createJSDocTypeLiteral,
        //     updateJSDocTypeLiteral,
        //     createJSDocTypeExpression,
        //     updateJSDocTypeExpression,
        //     createJSDocSignature,
        //     updateJSDocSignature,
        //     createJSDocTemplateTag,
        //     updateJSDocTemplateTag,
        //     createJSDocTypedefTag,
        //     updateJSDocTypedefTag,
        //     createJSDocParameterTag,
        //     updateJSDocParameterTag,
        //     createJSDocPropertyTag,
        //     updateJSDocPropertyTag,
        //     createJSDocCallbackTag,
        //     updateJSDocCallbackTag,
        //     createJSDocAugmentsTag,
        //     updateJSDocAugmentsTag,
        //     createJSDocImplementsTag,
        //     updateJSDocImplementsTag,
        //     createJSDocSeeTag,
        //     updateJSDocSeeTag,
        //     createJSDocNameReference,
        //     updateJSDocNameReference,
        //     createJSDocMemberName,
        //     updateJSDocMemberName,
        //     createJSDocLink,
        //     updateJSDocLink,
        //     createJSDocLinkCode,
        //     updateJSDocLinkCode,
        //     createJSDocLinkPlain,
        //     updateJSDocLinkPlain,
        //     // lazily load factory members for JSDoc tags with similar structure
        //     get createJSDocTypeTag() { return getJSDocTypeLikeTagCreateFunction<JSDocTypeTag>(SyntaxKind::JSDocTypeTag); },
        //     get updateJSDocTypeTag() { return getJSDocTypeLikeTagUpdateFunction<JSDocTypeTag>(SyntaxKind::JSDocTypeTag); },
        //     get createJSDocReturnTag() { return getJSDocTypeLikeTagCreateFunction<JSDocReturnTag>(SyntaxKind::JSDocReturnTag); },
        //     get updateJSDocReturnTag() { return getJSDocTypeLikeTagUpdateFunction<JSDocReturnTag>(SyntaxKind::JSDocReturnTag); },
        //     get createJSDocThisTag() { return getJSDocTypeLikeTagCreateFunction<JSDocThisTag>(SyntaxKind::JSDocThisTag); },
        //     get updateJSDocThisTag() { return getJSDocTypeLikeTagUpdateFunction<JSDocThisTag>(SyntaxKind::JSDocThisTag); },
        //     get createJSDocEnumTag() { return getJSDocTypeLikeTagCreateFunction<JSDocEnumTag>(SyntaxKind::JSDocEnumTag); },
        //     get updateJSDocEnumTag() { return getJSDocTypeLikeTagUpdateFunction<JSDocEnumTag>(SyntaxKind::JSDocEnumTag); },
        //     get createJSDocAuthorTag() { return getJSDocSimpleTagCreateFunction<JSDocAuthorTag>(SyntaxKind::JSDocAuthorTag); },
        //     get updateJSDocAuthorTag() { return getJSDocSimpleTagUpdateFunction<JSDocAuthorTag>(SyntaxKind::JSDocAuthorTag); },
        //     get createJSDocClassTag() { return getJSDocSimpleTagCreateFunction<JSDocClassTag>(SyntaxKind::JSDocClassTag); },
        //     get updateJSDocClassTag() { return getJSDocSimpleTagUpdateFunction<JSDocClassTag>(SyntaxKind::JSDocClassTag); },
        //     get createJSDocPublicTag() { return getJSDocSimpleTagCreateFunction<JSDocPublicTag>(SyntaxKind::JSDocPublicTag); },
        //     get updateJSDocPublicTag() { return getJSDocSimpleTagUpdateFunction<JSDocPublicTag>(SyntaxKind::JSDocPublicTag); },
        //     get createJSDocPrivateTag() { return getJSDocSimpleTagCreateFunction<JSDocPrivateTag>(SyntaxKind::JSDocPrivateTag); },
        //     get updateJSDocPrivateTag() { return getJSDocSimpleTagUpdateFunction<JSDocPrivateTag>(SyntaxKind::JSDocPrivateTag); },
        //     get createJSDocProtectedTag() { return getJSDocSimpleTagCreateFunction<JSDocProtectedTag>(SyntaxKind::JSDocProtectedTag); },
        //     get updateJSDocProtectedTag() { return getJSDocSimpleTagUpdateFunction<JSDocProtectedTag>(SyntaxKind::JSDocProtectedTag); },
        //     get createJSDocReadonlyTag() { return getJSDocSimpleTagCreateFunction<JSDocReadonlyTag>(SyntaxKind::JSDocReadonlyTag); },
        //     get updateJSDocReadonlyTag() { return getJSDocSimpleTagUpdateFunction<JSDocReadonlyTag>(SyntaxKind::JSDocReadonlyTag); },
        //     get createJSDocOverrideTag() { return getJSDocSimpleTagCreateFunction<JSDocOverrideTag>(SyntaxKind::JSDocOverrideTag); },
        //     get updateJSDocOverrideTag() { return getJSDocSimpleTagUpdateFunction<JSDocOverrideTag>(SyntaxKind::JSDocOverrideTag); },
        //     get createJSDocDeprecatedTag() { return getJSDocSimpleTagCreateFunction<JSDocDeprecatedTag>(SyntaxKind::JSDocDeprecatedTag); },
        //     get updateJSDocDeprecatedTag() { return getJSDocSimpleTagUpdateFunction<JSDocDeprecatedTag>(SyntaxKind::JSDocDeprecatedTag); },
        //     createJSDocUnknownTag,
        //     updateJSDocUnknownTag,
        //     createJSDocText,
        //     updateJSDocText,
        //     createJSDocComment,
        //     updateJSDocComment,
        //     createJsxElement,
        //     updateJsxElement,
        //     createJsxSelfClosingElement,
        //     updateJsxSelfClosingElement,
        //     createJsxOpeningElement,
        //     updateJsxOpeningElement,
        //     createJsxClosingElement,
        //     updateJsxClosingElement,
        //     createJsxFragment,
        //     createJsxText,
        //     updateJsxText,
        //     createJsxOpeningFragment,
        //     createJsxJsxClosingFragment,
        //     updateJsxFragment,
        //     createJsxAttribute,
        //     updateJsxAttribute,
        //     createJsxAttributes,
        //     updateJsxAttributes,
        //     createJsxSpreadAttribute,
        //     updateJsxSpreadAttribute,
        //     createJsxExpression,
        //     updateJsxExpression,
        //     createCaseClause,
        //     updateCaseClause,
        //     createDefaultClause,
        //     updateDefaultClause,
        //     createHeritageClause,
        //     updateHeritageClause,
        //     createCatchClause,
        //     updateCatchClause,
        //     createPropertyAssignment,
        //     updatePropertyAssignment,
        //     createShorthandPropertyAssignment,
        //     updateShorthandPropertyAssignment,
        //     createSpreadAssignment,
        //     updateSpreadAssignment,
        //     createEnumMember,
        //     updateEnumMember,
        //     createSourceFile,
        //     updateSourceFile,
        //     createBundle,
        //     updateBundle,
        //     createUnparsedSource,
        //     createUnparsedPrologue,
        //     createUnparsedPrepend,
        //     createUnparsedTextLike,
        //     createUnparsedSyntheticReference,
        //     createInputFiles,
        //     createSyntheticExpression,
        //     createSyntaxList,
        //     createNotEmittedStatement,
        //     createPartiallyEmittedExpression,
        //     updatePartiallyEmittedExpression,
        //     createCommaListExpression,
        //     updateCommaListExpression,
        //     createEndOfDeclarationMarker,
        //     createMergeDeclarationMarker,
        //     createSyntheticReferenceExpression,
        //     updateSyntheticReferenceExpression,
        //     cloneNode,

        //     // Lazily load factory methods for common operator factories and utilities
        //     get createComma() { return getBinaryCreateFunction(SyntaxKind::CommaToken); },
        //     get createAssignment() { return getBinaryCreateFunction(SyntaxKind::EqualsToken) as NodeFactory["createAssignment"]; },
        //     get createLogicalOr() { return getBinaryCreateFunction(SyntaxKind::BarBarToken); },
        //     get createLogicalAnd() { return getBinaryCreateFunction(SyntaxKind::AmpersandAmpersandToken); },
        //     get createBitwiseOr() { return getBinaryCreateFunction(SyntaxKind::BarToken); },
        //     get createBitwiseXor() { return getBinaryCreateFunction(SyntaxKind::CaretToken); },
        //     get createBitwiseAnd() { return getBinaryCreateFunction(SyntaxKind::AmpersandToken); },
        //     get createStrictEquality() { return getBinaryCreateFunction(SyntaxKind::EqualsEqualsEqualsToken); },
        //     get createStrictInequality() { return getBinaryCreateFunction(SyntaxKind::ExclamationEqualsEqualsToken); },
        //     get createEquality() { return getBinaryCreateFunction(SyntaxKind::EqualsEqualsToken); },
        //     get createInequality() { return getBinaryCreateFunction(SyntaxKind::ExclamationEqualsToken); },
        //     get createLessThan() { return getBinaryCreateFunction(SyntaxKind::LessThanToken); },
        //     get createLessThanEquals() { return getBinaryCreateFunction(SyntaxKind::LessThanEqualsToken); },
        //     get createGreaterThan() { return getBinaryCreateFunction(SyntaxKind::GreaterThanToken); },
        //     get createGreaterThanEquals() { return getBinaryCreateFunction(SyntaxKind::GreaterThanEqualsToken); },
        //     get createLeftShift() { return getBinaryCreateFunction(SyntaxKind::LessThanLessThanToken); },
        //     get createRightShift() { return getBinaryCreateFunction(SyntaxKind::GreaterThanGreaterThanToken); },
        //     get createUnsignedRightShift() { return getBinaryCreateFunction(SyntaxKind::GreaterThanGreaterThanGreaterThanToken); },
        //     get createAdd() { return getBinaryCreateFunction(SyntaxKind::PlusToken); },
        //     get createSubtract() { return getBinaryCreateFunction(SyntaxKind::MinusToken); },
        //     get createMultiply() { return getBinaryCreateFunction(SyntaxKind::AsteriskToken); },
        //     get createDivide() { return getBinaryCreateFunction(SyntaxKind::SlashToken); },
        //     get createModulo() { return getBinaryCreateFunction(SyntaxKind::PercentToken); },
        //     get createExponent() { return getBinaryCreateFunction(SyntaxKind::AsteriskAsteriskToken); },
        //     get createPrefixPlus() { return getPrefixUnaryCreateFunction(SyntaxKind::PlusToken); },
        //     get createPrefixMinus() { return getPrefixUnaryCreateFunction(SyntaxKind::MinusToken); },
        //     get createPrefixIncrement() { return getPrefixUnaryCreateFunction(SyntaxKind::PlusPlusToken); },
        //     get createPrefixDecrement() { return getPrefixUnaryCreateFunction(SyntaxKind::MinusMinusToken); },
        //     get createBitwiseNot() { return getPrefixUnaryCreateFunction(SyntaxKind::TildeToken); },
        //     get createLogicalNot() { return getPrefixUnaryCreateFunction(SyntaxKind::ExclamationToken); },
        //     get createPostfixIncrement() { return getPostfixUnaryCreateFunction(SyntaxKind::PlusPlusToken); },
        //     get createPostfixDecrement() { return getPostfixUnaryCreateFunction(SyntaxKind::MinusMinusToken); },

        //     // Compound nodes
        //     createImmediatelyInvokedFunctionExpression,
        //     createImmediatelyInvokedArrowFunction,
        //     createVoidZero,
        //     createExportDefault,
        //     createExternalModuleExport,
        //     createTypeCheck,
        //     createMethodCall,
        //     createGlobalMethodCall,
        //     createFunctionBindCall,
        //     createFunctionCallCall,
        //     createFunctionApplyCall,
        //     createArraySliceCall,
        //     createArrayConcatCall,
        //     createObjectDefinePropertyCall,
        //     createReflectGetCall,
        //     createReflectSetCall,
        //     createPropertyDescriptor,
        //     createCallBinding,
        //     createAssignmentTargetWrapper,

        //     // Utilities
        //     inlineExpressions,
        //     getInternalName,
        //     getLocalName,
        //     getExportName,
        //     getDeclarationName,
        //     getNamespaceMemberName,
        //     getExternalModuleOrNamespaceExportName,
        //     restoreOuterExpressions,
        //     restoreEnclosingLabel,
        //     createUseStrictPrologue,
        //     copyPrologue,
        //     copyStandardPrologue,
        //     copyCustomPrologue,
        //     ensureUseStrict,
        //     liftToBlock,
        //     mergeLexicalEnvironment,
        //     updateModifiers,
        // };

        // return factory;
    }

    pub fn createNodeArray<T: IsNode>(
        &mut self,
        elements: Option<Vec<T>>,
        hasTrailingComma: bool,
    ) -> NodeArray<T> {
        let elements = elements.unwrap_or_default();
        // TODO: see updateNodeArray
        // if (elements == undefined || elements == emptyArray) {
        //     elements = [];
        // }
        // else if (isNodeArray(elements)) {
        //     if (hasTrailingComma === undefined || elements.hasTrailingComma === hasTrailingComma) {
        //         // Ensure the transform flags have been aggregated for this NodeArray
        //         if (elements.transformFlags === undefined) {
        //             aggregateChildrenFlags(elements as MutableNodeArray<T>);
        //         }
        //         Debug.attachNodeArrayDebugInfo(elements);
        //         return elements;
        //     }

        //     // This *was* a `NodeArray`, but the `hasTrailingComma` option differs. Recreate the
        //     // array with the same elements, text range, and transform flags but with the updated
        //     // value for `hasTrailingComma`
        //     const array = elements.slice() as MutableNodeArray<T>;
        //     array.pos = elements.pos;
        //     array.end = elements.end;
        //     array.hasTrailingComma = hasTrailingComma;
        //     array.transformFlags = elements.transformFlags;
        //     Debug.attachNodeArrayDebugInfo(array);
        //     return array;
        // }

        // Since the element list of a node array is typically created by starting with an empty array and
        // repeatedly calling push(), the list may not have the optimal memory layout. We invoke slice() for
        // small arrays (1 to 4 elements) to give the VM a chance to allocate an optimal representation.
        let length = elements.len();
        let mut array = NodeArray::from(elements);
        array.setTextRangePosEnd(usize::MAX, usize::MAX);
        array.hasTrailingComma = hasTrailingComma;
        self.aggregateChildrenFlags(&mut array);
        // Debug.attachNodeArrayDebugInfo(array);
        array
    }

    pub fn updateNodeArray<T: IsNode>(
        &mut self,
        elements: Option<NodeArray<T>>,
        hasTrailingComma: Option<bool>,
    ) -> NodeArray<T> {
        if let Some(mut elements) = elements {
            if hasTrailingComma.is_none() || elements.hasTrailingComma == hasTrailingComma.unwrap()
            {
                // Ensure the transform flags have been aggregated for this NodeArray
                if elements.transformFlags.is_none() {
                    self.aggregateChildrenFlags(&mut elements);
                }
                return elements;
            }

            // This *was* a `NodeArray`, but the `hasTrailingComma` option differs. Recreate the
            // array with the same elements, text range, and transform flags but with the updated
            // value for `hasTrailingComma`
            elements.hasTrailingComma = hasTrailingComma.unwrap();
            elements
        } else {
            self.createNodeArray(None, hasTrailingComma.unwrap_or_default())
        }
    }

    //     function createBaseNode<T extends Node>(kind: T["kind"]) {
    //         return baseFactory.createBaseNode(kind) as Mutable<T>;
    //     }

    //     function createBaseDeclaration<T extends Declaration | VariableStatement | ImportDeclaration>(
    //         kind: T["kind"],
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined
    //     ) {
    //         const node = createBaseNode(kind);
    //         node.decorators = asNodeArray(decorators);
    //         node.modifiers = asNodeArray(modifiers);
    //         node.transformFlags |=
    //             propagateChildrenFlags(node.decorators) |
    //             propagateChildrenFlags(node.modifiers);
    //         // NOTE: The following properties are commonly set by the binder and are added here to
    //         // ensure declarations have a stable shape.
    //         node.symbol = undefined!; // initialized by binder
    //         node.localSymbol = undefined; // initialized by binder
    //         node.locals = undefined; // initialized by binder
    //         node.nextContainer = undefined; // initialized by binder
    //         return node;
    //     }

    //     function createBaseNamedDeclaration<T extends NamedDeclaration>(
    //         kind: T["kind"],
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: Identifier | PrivateIdentifier | StringLiteralLike | NumericLiteral | ComputedPropertyName | BindingPattern | string | undefined
    //     ) {
    //         const node = createBaseDeclaration(
    //             kind,
    //             decorators,
    //             modifiers
    //         );
    //         name = asName(name);
    //         node.name = name;

    //         // The PropertyName of a member is allowed to be `await`.
    //         // We don't need to exclude `await` for type signatures since types
    //         // don't propagate child flags.
    //         if (name) {
    //             switch (node.kind) {
    //                 case SyntaxKind::MethodDeclaration:
    //                 case SyntaxKind::GetAccessor:
    //                 case SyntaxKind::SetAccessor:
    //                 case SyntaxKind::PropertyDeclaration:
    //                 case SyntaxKind::PropertyAssignment:
    //                     if (isIdentifier(name)) {
    //                         node.transformFlags |= propagateIdentifierNameFlags(name);
    //                         break;
    //                     }
    //                     // fall through
    //                 default:
    //                     node.transformFlags |= propagateChildFlags(name);
    //                     break;
    //             }
    //         }
    //         return node;
    //     }

    //     function createBaseGenericNamedDeclaration<T extends NamedDeclaration & { typeParameters?: NodeArray<TypeParameterDeclaration> }>(
    //         kind: T["kind"],
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: Identifier | PrivateIdentifier | StringLiteralLike | NumericLiteral | ComputedPropertyName | BindingPattern | string | undefined,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined
    //     ) {
    //         const node = createBaseNamedDeclaration(
    //             kind,
    //             decorators,
    //             modifiers,
    //             name
    //         );
    //         node.typeParameters = asNodeArray(typeParameters);
    //         node.transformFlags |= propagateChildrenFlags(node.typeParameters);
    //         if (typeParameters) node.transformFlags |= TransformFlags::ContainsTypeScript;
    //         return node;
    //     }

    //     function createBaseSignatureDeclaration<T extends SignatureDeclarationBase>(
    //         kind: T["kind"],
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: Identifier | PrivateIdentifier | StringLiteralLike | NumericLiteral | ComputedPropertyName | BindingPattern | string | undefined,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         parameters: readonly ParameterDeclaration[] | undefined,
    //         type: TypeNode | undefined
    //     ) {
    //         const node = createBaseGenericNamedDeclaration(
    //             kind,
    //             decorators,
    //             modifiers,
    //             name,
    //             typeParameters
    //         );
    //         node.parameters = createNodeArray(parameters);
    //         node.type = type;
    //         node.transformFlags |=
    //             propagateChildrenFlags(node.parameters) |
    //             propagateChildFlags(node.type);
    //         if (type) node.transformFlags |= TransformFlags::ContainsTypeScript;
    //         return node;
    //     }

    //     function updateBaseSignatureDeclaration<T extends SignatureDeclarationBase>(updated: Mutable<T>, original: T) {
    //         // copy children used only for error reporting
    //         if (original.typeArguments) updated.typeArguments = original.typeArguments;
    //         return update(updated, original);
    //     }

    //     function createBaseFunctionLikeDeclaration<T extends FunctionLikeDeclaration>(
    //         kind: T["kind"],
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: Identifier | PrivateIdentifier | StringLiteralLike | NumericLiteral | ComputedPropertyName | BindingPattern | string | undefined,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         parameters: readonly ParameterDeclaration[] | undefined,
    //         type: TypeNode | undefined,
    //         body: T["body"]
    //     ) {
    //         const node = createBaseSignatureDeclaration(
    //             kind,
    //             decorators,
    //             modifiers,
    //             name,
    //             typeParameters,
    //             parameters,
    //             type
    //         );
    //         node.body = body;
    //         node.transformFlags |= propagateChildFlags(node.body) & ~TransformFlags::ContainsPossibleTopLevelAwait;
    //         if (!body) node.transformFlags |= TransformFlags::ContainsTypeScript;
    //         return node;
    //     }

    //     function updateBaseFunctionLikeDeclaration<T extends FunctionLikeDeclaration>(updated: Mutable<T>, original: T) {
    //         // copy children used only for error reporting
    //         if (original.exclamationToken) updated.exclamationToken = original.exclamationToken;
    //         if (original.typeArguments) updated.typeArguments = original.typeArguments;
    //         return updateBaseSignatureDeclaration(updated, original);
    //     }

    //     function createBaseInterfaceOrClassLikeDeclaration<T extends InterfaceDeclaration | ClassLikeDeclaration>(
    //         kind: T["kind"],
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: string | Identifier | undefined,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         heritageClauses: readonly HeritageClause[] | undefined
    //     ) {
    //         const node = createBaseGenericNamedDeclaration(
    //             kind,
    //             decorators,
    //             modifiers,
    //             name,
    //             typeParameters
    //         );
    //         node.heritageClauses = asNodeArray(heritageClauses);
    //         node.transformFlags |= propagateChildrenFlags(node.heritageClauses);
    //         return node;
    //     }

    //     function createBaseClassLikeDeclaration<T extends ClassLikeDeclaration>(
    //         kind: T["kind"],
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: string | Identifier | undefined,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         heritageClauses: readonly HeritageClause[] | undefined,
    //         members: readonly ClassElement[]
    //     ) {
    //         const node = createBaseInterfaceOrClassLikeDeclaration(
    //             kind,
    //             decorators,
    //             modifiers,
    //             name,
    //             typeParameters,
    //             heritageClauses
    //         );
    //         node.members = createNodeArray(members);
    //         node.transformFlags |= propagateChildrenFlags(node.members);
    //         return node;
    //     }

    //     function createBaseBindingLikeDeclaration<T extends PropertyDeclaration | VariableDeclaration | ParameterDeclaration | BindingElement>(
    //         kind: T["kind"],
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: string | T["name"] | undefined,
    //         initializer: Expression | undefined
    //     ) {
    //         const node = createBaseNamedDeclaration(
    //             kind,
    //             decorators,
    //             modifiers,
    //             name
    //         );
    //         node.initializer = initializer;
    //         node.transformFlags |= propagateChildFlags(node.initializer);
    //         return node;
    //     }

    //     function createBaseVariableLikeDeclaration<T extends PropertyDeclaration | VariableDeclaration | ParameterDeclaration>(
    //         kind: T["kind"],
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: string | T["name"] | undefined,
    //         type: TypeNode | undefined,
    //         initializer: Expression | undefined
    //     ) {
    //         const node = createBaseBindingLikeDeclaration(
    //             kind,
    //             decorators,
    //             modifiers,
    //             name,
    //             initializer
    //         );
    //         node.type = type;
    //         node.transformFlags |= propagateChildFlags(type);
    //         if (type) node.transformFlags |= TransformFlags::ContainsTypeScript;
    //         return node;
    //     }

    //     //
    //     // Literals
    //     //

    //     function createBaseLiteral<T extends LiteralToken>(
    //         kind: T["kind"],
    //         text: string
    //     ) {
    //         const node = createBaseToken(kind);
    //         node.text = text;
    //         return node;
    //     }

    pub fn createNumericLiteral(
        &mut self,
        value: Rc<str>,
        numericLiteralFlags: Option<TokenFlags>,
    ) -> NumericLiteral {
        let node = NumericLiteral {
            node_id: self.node_id_gen.next(),
            text: value,
            numericLiteralFlags: numericLiteralFlags.unwrap_or_default(),
        };
        if node
            .numericLiteralFlags
            .intersects(TokenFlags::BinaryOrOctalSpecifier)
        {
            self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsES2015;
        }
        node
    }

    pub fn createBigIntLiteral(&mut self, value: Rc<str>) -> BigIntLiteral {
        let node = BigIntLiteral {
            node_id: self.node_id_gen.next(),
            text: value,
            isUnterminated: None,
            hasExtendedUnicodeEscape: None,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsESNext;
        node
    }

    //     function createBaseStringLiteral(text: string, isSingleQuote?: boolean) {
    //         const node = createBaseLiteral<StringLiteral>(SyntaxKind::StringLiteral, text);
    //         node.singleQuote = isSingleQuote;
    //         return node;
    //     }

    pub fn createStringLiteral(
        &mut self,
        text: Rc<str>,
        isSingleQuote: Option<bool>,
        hasExtendedUnicodeEscape: bool,
    ) -> StringLiteral {
        let node = StringLiteral {
            node_id: self.node_id_gen.next(),
            text,
            isUnterminated: None,
            hasExtendedUnicodeEscape: Some(hasExtendedUnicodeEscape),
            singleQuote: isSingleQuote,
        };
        if hasExtendedUnicodeEscape {
            self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsES2015;
        }
        node
    }

    //     // @api
    //     function createStringLiteralFromNode(sourceNode: PropertyNameLiteral): StringLiteral {
    //         const node = createBaseStringLiteral(getTextOfIdentifierOrLiteral(sourceNode), /*isSingleQuote*/ undefined);
    //         node.textSourceNode = sourceNode;
    //         return node;
    //     }

    pub fn createRegularExpressionLiteral(&mut self, text: Rc<str>) -> RegularExpressionLiteral {
        let node = RegularExpressionLiteral {
            node_id: self.node_id_gen.next(),
            text,
            isUnterminated: None,
            hasExtendedUnicodeEscape: None,
        };
        node
    }

    //     // @api
    //     function createLiteralLikeNode(kind: LiteralToken["kind"] | SyntaxKind::JsxTextAllWhiteSpaces, text: string): LiteralToken {
    //         switch (kind) {
    //             case SyntaxKind::NumericLiteral: return createNumericLiteral(text, /*numericLiteralFlags*/ 0);
    //             case SyntaxKind::BigIntLiteral: return createBigIntLiteral(text);
    //             case SyntaxKind::StringLiteral: return createStringLiteral(text, /*isSingleQuote*/ undefined);
    //             case SyntaxKind::JsxText: return createJsxText(text, /*containsOnlyTriviaWhiteSpaces*/ false);
    //             case SyntaxKind::JsxTextAllWhiteSpaces: return createJsxText(text, /*containsOnlyTriviaWhiteSpaces*/ true);
    //             case SyntaxKind::RegularExpressionLiteral: return createRegularExpressionLiteral(text);
    //             case SyntaxKind::NoSubstitutionTemplateLiteral: return createTemplateLiteralLikeNode(kind, text, /*rawText*/ undefined, /*templateFlags*/ 0) as NoSubstitutionTemplateLiteral;
    //         }
    //     }

    //
    // Identifiers
    //

    fn createBaseIdentifier(
        &mut self,
        text: JsWord,
        mut originalKeywordKind: Option<SyntaxKind>,
    ) -> Identifier {
        if originalKeywordKind.is_none() && !text.is_empty() {
            originalKeywordKind = stringToToken(&text);
        }
        if originalKeywordKind == Some(SyntaxKind::Identifier) {
            originalKeywordKind = None;
        }
        let node = Identifier {
            node_id: self.node_id_gen.next(),
            escapedText: escapeLeadingUnderscores(text),
            originalKeywordKind,
            autoGenerateFlags: GeneratedIdentifierFlags::default(),
            jsdocDotPos: None,
        };
        node
    }

    //     function createBaseGeneratedIdentifier(text: string, autoGenerateFlags: GeneratedIdentifierFlags) {
    //         const node = createBaseIdentifier(text, /*originalKeywordKind*/ undefined) as Mutable<GeneratedIdentifier>;
    //         node.autoGenerateFlags = autoGenerateFlags;
    //         node.autoGenerateId = nextAutoGenerateId;
    //         nextAutoGenerateId++;
    //         return node;
    //     }

    // fn createIdentifier(&mut self, text: String, typeArguments: Option<Vec<TypeNode | TypeParameterDeclaration>, originalKeywordKind: Option<SyntaxKind>)-> Identifier {
    pub fn createIdentifier(
        &mut self,
        text: JsWord,
        typeArguments: Option<Vec<()>>,
        originalKeywordKind: Option<SyntaxKind>,
    ) -> Identifier {
        let node = self.createBaseIdentifier(text, originalKeywordKind);
        if let Some(typeArguments) = typeArguments {
            todo!();
            // // NOTE: we do not use `setChildren` here because typeArguments in an identifier do not contribute to transformations
            // node.typeArguments = createNodeArray(typeArguments);
        }
        if node.originalKeywordKind == Some(SyntaxKind::AwaitKeyword) {
            self.node_data_mut(&node).transformFlags |=
                TransformFlags::ContainsPossibleTopLevelAwait;
        }
        node
    }

    //     // @api
    //     function updateIdentifier(node: Identifier, typeArguments?: NodeArray<TypeNode | TypeParameterDeclaration> | undefined): Identifier {
    //         return node.typeArguments !== typeArguments
    //             ? update(createIdentifier(idText(node), typeArguments), node)
    //             : node;
    //     }

    //     // @api
    //     function createTempVariable(recordTempVariable: ((node: Identifier) => void) | undefined, reservedInNestedScopes?: boolean): GeneratedIdentifier {
    //         let flags = GeneratedIdentifierFlags.Auto;
    //         if (reservedInNestedScopes) flags |= GeneratedIdentifierFlags.ReservedInNestedScopes;
    //         const name = createBaseGeneratedIdentifier("", flags);
    //         if (recordTempVariable) {
    //             recordTempVariable(name);
    //         }
    //         return name;
    //     }

    //     /** Create a unique temporary variable for use in a loop. */
    //     // @api
    //     function createLoopVariable(reservedInNestedScopes?: boolean): Identifier {
    //         let flags = GeneratedIdentifierFlags.Loop;
    //         if (reservedInNestedScopes) flags |= GeneratedIdentifierFlags.ReservedInNestedScopes;
    //         return createBaseGeneratedIdentifier("", flags);
    //     }

    //     /** Create a unique name based on the supplied text. */
    //     // @api
    //     function createUniqueName(text: string, flags: GeneratedIdentifierFlags = GeneratedIdentifierFlags.None): Identifier {
    //         Debug.assert(!(flags & GeneratedIdentifierFlags.KindMask), "Argument out of range: flags");
    //         Debug.assert((flags & (GeneratedIdentifierFlags.Optimistic | GeneratedIdentifierFlags.FileLevel)) !== GeneratedIdentifierFlags.FileLevel, "GeneratedIdentifierFlags.FileLevel cannot be set without also setting GeneratedIdentifierFlags.Optimistic");
    //         return createBaseGeneratedIdentifier(text, GeneratedIdentifierFlags.Unique | flags);
    //     }

    //     /** Create a unique name generated for a node. */
    //     // @api
    //     function getGeneratedNameForNode(node: Node | undefined, flags: GeneratedIdentifierFlags = 0): Identifier {
    //         Debug.assert(!(flags & GeneratedIdentifierFlags.KindMask), "Argument out of range: flags");
    //         const name = createBaseGeneratedIdentifier(node && isIdentifier(node) ? idText(node) : "", GeneratedIdentifierFlags.Node | flags);
    //         name.original = node;
    //         return name;
    //     }

    pub fn createPrivateIdentifier(&mut self, text: JsWord) -> PrivateIdentifier {
        debug_assert!(
            text.starts_with("#"),
            "First character of private identifier must be #: {}",
            text
        );
        let node = PrivateIdentifier {
            node_id: self.node_id_gen.next(),
            escapedText: escapeLeadingUnderscores(text),
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsClassFields;
        node
    }

    //
    // Punctuation
    //

    //     function createBaseToken<T extends Node>(kind: T["kind"]) {
    //         return baseFactory.createBaseTokenNode(kind) as Mutable<T>;
    //     }

    //     // @api
    //     function createToken(token: SyntaxKind::SuperKeyword): SuperExpression;
    //     function createToken(token: SyntaxKind::ThisKeyword): ThisExpression;
    //     function createToken(token: SyntaxKind::NullKeyword): NullLiteral;
    //     function createToken(token: SyntaxKind::TrueKeyword): TrueLiteral;
    //     function createToken(token: SyntaxKind::FalseKeyword): FalseLiteral;
    //     function createToken<TKind extends PunctuationSyntaxKind>(token: TKind): PunctuationToken<TKind>;
    //     function createToken<TKind extends KeywordTypeSyntaxKind>(token: TKind): KeywordTypeNode<TKind>;
    //     function createToken<TKind extends ModifierSyntaxKind>(token: TKind): ModifierToken<TKind>;
    //     function createToken<TKind extends KeywordSyntaxKind>(token: TKind): KeywordToken<TKind>;
    //     function createToken<TKind extends SyntaxKind::Unknown | SyntaxKind::EndOfFileToken>(token: TKind): Token<TKind>;
    //     function createToken<TKind extends SyntaxKind>(token: TKind): Token<TKind>;
    //     function createToken<TKind extends SyntaxKind>(token: TKind) {
    pub fn createToken<T: IsNode + IsSimpleTokenNode>(&mut self, token: SyntaxKind) -> T {
        debug_assert!(
            token >= SyntaxKind::FirstToken && token <= SyntaxKind::LastToken,
            "Invalid token"
        );
        debug_assert!(
            token <= SyntaxKind::FirstTemplateToken || token >= SyntaxKind::LastTemplateToken,
            "Invalid token. Use 'createTemplateLiteralLikeNode' to create template literals."
        );
        debug_assert!(
            token <= SyntaxKind::FirstLiteralToken || token >= SyntaxKind::LastLiteralToken,
            "Invalid token. Use 'createLiteralLikeNode' to create literals."
        );
        debug_assert!(
            token != SyntaxKind::Identifier,
            "Invalid token. Use 'createIdentifier' to create identifiers"
        );
        let node = T::try_from_kind(token, self.node_id_gen.next()).unwrap();
        let mut transformFlags = TransformFlags::None;
        match token {
            SyntaxKind::AsyncKeyword => {
                // 'async' modifier is ES2017 (async functions) or ES2018 (async generators)
                transformFlags = TransformFlags::ContainsES2017 | TransformFlags::ContainsES2018;
            }
            SyntaxKind::PublicKeyword
            | SyntaxKind::PrivateKeyword
            | SyntaxKind::ProtectedKeyword
            | SyntaxKind::ReadonlyKeyword
            | SyntaxKind::AbstractKeyword
            | SyntaxKind::DeclareKeyword
            | SyntaxKind::ConstKeyword
            | SyntaxKind::AnyKeyword
            | SyntaxKind::NumberKeyword
            | SyntaxKind::BigIntKeyword
            | SyntaxKind::NeverKeyword
            | SyntaxKind::ObjectKeyword
            | SyntaxKind::OverrideKeyword
            | SyntaxKind::StringKeyword
            | SyntaxKind::BooleanKeyword
            | SyntaxKind::SymbolKeyword
            | SyntaxKind::VoidKeyword
            | SyntaxKind::UnknownKeyword
            | SyntaxKind::UndefinedKeyword => {
                // `undefined` is an Identifier in the expression case.
                transformFlags = TransformFlags::ContainsTypeScript;
            }
            SyntaxKind::SuperKeyword => {
                transformFlags =
                    TransformFlags::ContainsES2015 | TransformFlags::ContainsLexicalSuper;
            }
            SyntaxKind::StaticKeyword => {
                transformFlags = TransformFlags::ContainsES2015;
            }
            SyntaxKind::ThisKeyword => {
                // 'this' indicates a lexical 'this'
                transformFlags = TransformFlags::ContainsLexicalThis;
            }
            _ => {}
        }
        if !transformFlags.is_empty() {
            self.node_data_mut(&node).transformFlags |= transformFlags;
        }
        node
    }

    //     //
    //     // Reserved words
    //     //

    //     // @api
    //     function createSuper() {
    //         return createToken(SyntaxKind::SuperKeyword);
    //     }

    //     // @api
    //     function createThis() {
    //         return createToken(SyntaxKind::ThisKeyword);
    //     }

    //     // @api
    //     function createNull() {
    //         return createToken(SyntaxKind::NullKeyword);
    //     }

    //     // @api
    //     function createTrue() {
    //         return createToken(SyntaxKind::TrueKeyword);
    //     }

    //     // @api
    //     function createFalse() {
    //         return createToken(SyntaxKind::FalseKeyword);
    //     }

    //     //
    //     // Modifiers
    //     //

    //     // @api
    //     function createModifier<T extends ModifierSyntaxKind>(kind: T) {
    //         return createToken(kind);
    //     }

    //     // @api
    //     function createModifiersFromModifierFlags(flags: ModifierFlags) {
    //         const result: Modifier[] = [];
    //         if (flags & ModifierFlags.Export) result.push(createModifier(SyntaxKind::ExportKeyword));
    //         if (flags & ModifierFlags.Ambient) result.push(createModifier(SyntaxKind::DeclareKeyword));
    //         if (flags & ModifierFlags.Default) result.push(createModifier(SyntaxKind::DefaultKeyword));
    //         if (flags & ModifierFlags.Const) result.push(createModifier(SyntaxKind::ConstKeyword));
    //         if (flags & ModifierFlags.Public) result.push(createModifier(SyntaxKind::PublicKeyword));
    //         if (flags & ModifierFlags.Private) result.push(createModifier(SyntaxKind::PrivateKeyword));
    //         if (flags & ModifierFlags.Protected) result.push(createModifier(SyntaxKind::ProtectedKeyword));
    //         if (flags & ModifierFlags.Abstract) result.push(createModifier(SyntaxKind::AbstractKeyword));
    //         if (flags & ModifierFlags.Static) result.push(createModifier(SyntaxKind::StaticKeyword));
    //         if (flags & ModifierFlags.Override) result.push(createModifier(SyntaxKind::OverrideKeyword));
    //         if (flags & ModifierFlags.Readonly) result.push(createModifier(SyntaxKind::ReadonlyKeyword));
    //         if (flags & ModifierFlags.Async) result.push(createModifier(SyntaxKind::AsyncKeyword));
    //         return result;
    //     }

    //     //
    //     // Names
    //     //

    pub fn createQualifiedName(
        &mut self,
        left: EntityName,
        right: Rc<Identifier>,
    ) -> QualifiedName {
        let node = QualifiedName {
            node_id: self.node_id_gen.next(),
            left,
            right,
            jsdocDotPos: None,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.left))
            | self.propagateIdentifierNameFlags(&node.right);
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateQualifiedName(node: QualifiedName, left: EntityName, right: Identifier) {
    //         return node.left !== left
    //             || node.right !== right
    //             ? update(createQualifiedName(left, right), node)
    //             : node;
    //     }

    pub fn createComputedPropertyName(&mut self, expression: Expression) -> ComputedPropertyName {
        let node = ComputedPropertyName {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeExpressionOfComputedPropertyName(expression),
            expression,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.expression))
            | TransformFlags::ContainsES2015
            | TransformFlags::ContainsComputedPropertyName;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateComputedPropertyName(node: ComputedPropertyName, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createComputedPropertyName(expression), node)
    //             : node;
    //     }

    //     //
    //     // Signature elements
    //     //

    pub fn createTypeParameterDeclaration(
        &mut self,
        name: Rc<Identifier>,
        constraint: Option<TypeNode>,
        defaultType: Option<TypeNode>,
    ) -> TypeParameterDeclaration {
        let node = TypeParameterDeclaration {
            node_id: self.node_id_gen.next(),
            name,
            constraint,
            default: defaultType,
            expression: None,
        };

        let transform_flags =
            self.propagateChildFlags(Some(&node.name)) | TransformFlags::ContainsTypeScript;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateTypeParameterDeclaration(node: TypeParameterDeclaration, name: Identifier, constraint: TypeNode | undefined, defaultType: TypeNode | undefined) {
    //         return node.name !== name
    //             || node.constraint !== constraint
    //             || node.default !== defaultType
    //             ? update(createTypeParameterDeclaration(name, constraint, defaultType), node)
    //             : node;
    //     }

    pub fn createParameterDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        dotDotDotToken: Option<Rc<DotDotDotToken>>,
        name: BindingName,
        questionToken: Option<Rc<QuestionToken>>,
        ty: Option<TypeNode>,
        initializer: Option<Expression>,
    ) -> ParameterDeclaration {
        let node = ParameterDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            dotDotDotToken,
            name,
            questionToken,
            ty,
            initializer,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(Some(&node.name))
            | self.propagateChildFlags(node.initializer.as_ref())
            | self.propagateChildFlags(node.ty.as_ref());

        if node.ty.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        if isThisIdentifier(Some(&node.name.clone().into())) {
            transform_flags = TransformFlags::ContainsTypeScript;
        } else {
            transform_flags |= self.propagateChildFlags(node.dotDotDotToken.as_ref())
                | self.propagateChildFlags(node.questionToken.as_ref());
            if node.questionToken.is_some() {
                transform_flags |= TransformFlags::ContainsTypeScript;
            }
            if modifiersToFlags(node.modifiers.as_ref())
                .intersects(ModifierFlags::ParameterPropertyModifier)
            {
                transform_flags |= TransformFlags::ContainsTypeScriptClassSyntax;
            }
            if node.initializer.is_some() || node.dotDotDotToken.is_some() {
                transform_flags |= TransformFlags::ContainsES2015;
            }
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateParameterDeclaration(
    //         node: ParameterDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         dotDotDotToken: DotDotDotToken | undefined,
    //         name: string | BindingName,
    //         questionToken: QuestionToken | undefined,
    //         type: TypeNode | undefined,
    //         initializer: Expression | undefined
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.dotDotDotToken !== dotDotDotToken
    //             || node.name !== name
    //             || node.questionToken !== questionToken
    //             || node.type !== type
    //             || node.initializer !== initializer
    //             ? update(createParameterDeclaration(decorators, modifiers, dotDotDotToken, name, questionToken, type, initializer), node)
    //             : node;
    //     }

    pub fn createDecorator(&mut self, expression: LeftHandSideExpression) -> Decorator {
        let node = Decorator {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeLeftSideOfAccess(expression),
            expression,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.expression))
            | TransformFlags::ContainsTypeScript
            | TransformFlags::ContainsTypeScriptClassSyntax;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateDecorator(node: Decorator, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createDecorator(expression), node)
    //             : node;
    //     }

    //     //
    //     // Type Elements
    //     //

    pub fn createPropertySignature(
        &mut self,
        modifiers: Option<NodeArray<Modifier>>,
        name: PropertyName,
        questionToken: Option<Rc<QuestionToken>>,
        ty: Option<TypeNode>,
    ) -> PropertySignature {
        let node = PropertySignature {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            modifiers: self.asNodeArray(modifiers),
            name,
            questionToken,
            ty,
            initializer: None,
        };
        let mut transform_flags =
            propagateChildrenFlags(node.modifiers.as_ref()) | TransformFlags::ContainsTypeScript;

        // The PropertyName of a member is allowed to be `await`.
        // We don't need to exclude `await` for type signatures since types
        // don't propagate child flags.
        if let PropertyName::Identifier(name) = &node.name {
            transform_flags |= self.propagateIdentifierNameFlags(name);
        } else {
            transform_flags |= self.propagateChildFlags(Some(&node.name));
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updatePropertySignature(
    //         node: PropertySignature,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: PropertyName,
    //         questionToken: QuestionToken | undefined,
    //         type: TypeNode | undefined
    //     ) {
    //         return node.modifiers !== modifiers
    //             || node.name !== name
    //             || node.questionToken !== questionToken
    //             || node.type !== type
    //             ? update(createPropertySignature(modifiers, name, questionToken, type), node)
    //             : node;
    //     }

    pub fn createPropertyDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        name: PropertyName,
        questionToken: Option<Rc<QuestionToken>>,
        exclamationToken: Option<Rc<ExclamationToken>>,
        ty: Option<TypeNode>,
        initializer: Option<Expression>,
    ) -> PropertyDeclaration {
        debug_assert!(questionToken.is_none() || exclamationToken.is_none());

        let node = PropertyDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            name,
            questionToken,
            exclamationToken,
            ty,
            initializer,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(node.initializer.as_ref())
            | self.propagateChildFlags(node.ty.as_ref())
            | self.propagateChildFlags(node.questionToken.as_ref())
            | self.propagateChildFlags(node.exclamationToken.as_ref())
            | TransformFlags::ContainsClassFields;

        // The PropertyName of a member is allowed to be `await`.
        // We don't need to exclude `await` for type signatures since types
        // don't propagate child flags.
        if let PropertyName::Identifier(name) = &node.name {
            transform_flags |= self.propagateIdentifierNameFlags(name);
        } else {
            transform_flags |= self.propagateChildFlags(Some(&node.name));
        }

        if node.ty.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        if isComputedPropertyName(&node.name)
            || modifiersToFlags(node.modifiers.as_ref()).intersects(ModifierFlags::Static)
                && node.initializer.is_some()
        {
            transform_flags |= TransformFlags::ContainsTypeScriptClassSyntax;
        }
        if node.questionToken.is_some()
            || node.exclamationToken.is_some()
            || modifiersToFlags(node.modifiers.as_ref()).intersects(ModifierFlags::Ambient)
        {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updatePropertyDeclaration(
    //         node: PropertyDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: string | PropertyName,
    //         questionOrExclamationToken: QuestionToken | ExclamationToken | undefined,
    //         type: TypeNode | undefined,
    //         initializer: Expression | undefined
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.name !== name
    //             || node.questionToken !== (questionOrExclamationToken !== undefined && isQuestionToken(questionOrExclamationToken) ? questionOrExclamationToken : undefined)
    //             || node.exclamationToken !== (questionOrExclamationToken !== undefined && isExclamationToken(questionOrExclamationToken) ? questionOrExclamationToken : undefined)
    //             || node.type !== type
    //             || node.initializer !== initializer
    //             ? update(createPropertyDeclaration(decorators, modifiers, name, questionOrExclamationToken, type, initializer), node)
    //             : node;
    //     }

    pub fn createMethodSignature(
        &mut self,
        modifiers: Option<NodeArray<Modifier>>,
        name: PropertyName,
        questionToken: Option<Rc<QuestionToken>>,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        ty: Option<TypeNode>,
    ) -> MethodSignature {
        let node = MethodSignature {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            questionToken,
            modifiers: self.asNodeArray(modifiers),
            typeParameters: self.asNodeArray(typeParameters),
            parameters: self.updateNodeArray(Some(parameters), None),
            ty,
            typeArguments: None,
            name,
        };
        let mut transform_flags = propagateChildrenFlags(node.modifiers.as_ref())
            | propagateChildrenFlags(node.typeParameters.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | self.propagateChildFlags(node.ty.as_ref())
            | TransformFlags::ContainsTypeScript;
        // The PropertyName of a member is allowed to be `await`.
        // We don't need to exclude `await` for type signatures since types
        // don't propagate child flags.
        if let PropertyName::Identifier(name) = &node.name {
            transform_flags |= self.propagateIdentifierNameFlags(name);
        } else {
            transform_flags |= self.propagateChildFlags(Some(&node.name));
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateMethodSignature(
    //         node: MethodSignature,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: PropertyName,
    //         questionToken: QuestionToken | undefined,
    //         typeParameters: NodeArray<TypeParameterDeclaration> | undefined,
    //         parameters: NodeArray<ParameterDeclaration>,
    //         type: TypeNode | undefined
    //     ) {
    //         return node.modifiers !== modifiers
    //             || node.name !== name
    //             || node.questionToken !== questionToken
    //             || node.typeParameters !== typeParameters
    //             || node.parameters !== parameters
    //             || node.type !== type
    //             ? updateBaseSignatureDeclaration(createMethodSignature(modifiers, name, questionToken, typeParameters, parameters, type), node)
    //             : node;
    //     }

    pub fn createMethodDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        asteriskToken: Option<Rc<AsteriskToken>>,
        name: PropertyName,
        questionToken: Option<Rc<QuestionToken>>,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        ty: Option<TypeNode>,
        body: Option<Rc<Block>>,
    ) -> MethodDeclaration {
        let node = MethodDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            typeParameters: self.asNodeArray(typeParameters),
            parameters: self.updateNodeArray(Some(parameters), None),
            ty,
            typeArguments: None,
            asteriskToken,
            questionToken,
            name,
            body,
            exclamationToken: None,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | propagateChildrenFlags(node.typeParameters.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | self.propagateChildFlags(node.ty.as_ref())
            | (self.propagateChildFlags(node.body.as_ref())
                & !TransformFlags::ContainsPossibleTopLevelAwait)
            | self.propagateChildFlags(node.asteriskToken.as_ref())
            | self.propagateChildFlags(node.questionToken.as_ref())
            | TransformFlags::ContainsES2015;

        // The PropertyName of a member is allowed to be `await`.
        // We don't need to exclude `await` for type signatures since types
        // don't propagate child flags.
        if let PropertyName::Identifier(name) = &node.name {
            transform_flags |= self.propagateIdentifierNameFlags(name);
        } else {
            transform_flags |= self.propagateChildFlags(Some(&node.name));
        }

        if node.typeParameters.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript
        };

        if node.ty.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript
        };
        if node.body.is_none() {
            transform_flags |= TransformFlags::ContainsTypeScript
        };

        if node.questionToken.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        if modifiersToFlags(node.modifiers.as_ref()).intersects(ModifierFlags::Async) {
            if node.asteriskToken.is_some() {
                transform_flags |= TransformFlags::ContainsES2018;
            } else {
                transform_flags |= TransformFlags::ContainsES2017;
            }
        } else if node.asteriskToken.is_some() {
            transform_flags |= TransformFlags::ContainsGenerator;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateMethodDeclaration(
    //         node: MethodDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         asteriskToken: AsteriskToken | undefined,
    //         name: PropertyName,
    //         questionToken: QuestionToken | undefined,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         parameters: readonly ParameterDeclaration[],
    //         type: TypeNode | undefined,
    //         body: Block | undefined
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.asteriskToken !== asteriskToken
    //             || node.name !== name
    //             || node.questionToken !== questionToken
    //             || node.typeParameters !== typeParameters
    //             || node.parameters !== parameters
    //             || node.type !== type
    //             || node.body !== body
    //             ? updateBaseFunctionLikeDeclaration(createMethodDeclaration(decorators, modifiers, asteriskToken, name, questionToken, typeParameters, parameters, type, body), node)
    //             : node;
    //     }

    pub fn createClassStaticBlockDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        body: Rc<Block>,
    ) -> ClassStaticBlockDeclaration {
        let node = ClassStaticBlockDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            body,
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
        };
        let transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(Some(&node.body))
            | TransformFlags::ContainsClassFields;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateClassStaticBlockDeclaration(
    //         node: ClassStaticBlockDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         body: Block
    //     ): ClassStaticBlockDeclaration {
    //         return node.decorators !== decorators
    //             || node.modifier !== modifiers
    //             || node.body !== body
    //             ? update(createClassStaticBlockDeclaration(decorators, modifiers, body), node)
    //             : node;
    //     }

    pub fn createConstructorDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        body: Option<Rc<Block>>,
    ) -> ConstructorDeclaration {
        let node = ConstructorDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            parameters: self.updateNodeArray(Some(parameters), None),
            typeArguments: None,
            body,
            typeParameters: None,
            ty: None,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | (self.propagateChildFlags(node.body.as_ref())
                & !TransformFlags::ContainsPossibleTopLevelAwait)
            | TransformFlags::ContainsES2015;
        if node.body.is_none() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateConstructorDeclaration(
    //         node: ConstructorDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         parameters: readonly ParameterDeclaration[],
    //         body: Block | undefined
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.parameters !== parameters
    //             || node.body !== body
    //             ? updateBaseFunctionLikeDeclaration(createConstructorDeclaration(decorators, modifiers, parameters, body), node)
    //             : node;
    //     }

    pub fn createGetAccessorDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        name: PropertyName,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        ty: Option<TypeNode>,
        body: Option<Rc<Block>>,
    ) -> GetAccessorDeclaration {
        let node = GetAccessorDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            parameters: self.updateNodeArray(Some(parameters), None),
            questionToken: None,
            ty,
            typeArguments: None,
            name,
            body,
            typeParameters: None,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | self.propagateChildFlags(node.ty.as_ref())
            | (self.propagateChildFlags(node.body.as_ref())
                & !TransformFlags::ContainsPossibleTopLevelAwait);

        // The PropertyName of a member is allowed to be `await`.
        // We don't need to exclude `await` for type signatures since types
        // don't propagate child flags.
        if let PropertyName::Identifier(name) = &node.name {
            transform_flags |= self.propagateIdentifierNameFlags(name);
        } else {
            transform_flags |= self.propagateChildFlags(Some(&node.name));
        }
        if node.ty.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        if node.body.is_none() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateGetAccessorDeclaration(
    //         node: GetAccessorDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: PropertyName,
    //         parameters: readonly ParameterDeclaration[],
    //         type: TypeNode | undefined,
    //         body: Block | undefined
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.name !== name
    //             || node.parameters !== parameters
    //             || node.type !== type
    //             || node.body !== body
    //             ? updateBaseFunctionLikeDeclaration(createGetAccessorDeclaration(decorators, modifiers, name, parameters, type, body), node)
    //             : node;
    //     }

    pub fn createSetAccessorDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        name: PropertyName,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        body: Option<Rc<Block>>,
    ) -> SetAccessorDeclaration {
        let node = SetAccessorDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            questionToken: None,
            parameters: self.updateNodeArray(Some(parameters), None),
            typeArguments: None,
            name,
            body,
            typeParameters: None,
            ty: None,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | (self.propagateChildFlags(node.body.as_ref())
                & !TransformFlags::ContainsPossibleTopLevelAwait);

        // The PropertyName of a member is allowed to be `await`.
        // We don't need to exclude `await` for type signatures since types
        // don't propagate child flags.
        if let PropertyName::Identifier(name) = &node.name {
            transform_flags |= self.propagateIdentifierNameFlags(name);
        } else {
            transform_flags |= self.propagateChildFlags(Some(&node.name));
        }

        if node.body.is_none() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateSetAccessorDeclaration(
    //         node: SetAccessorDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: PropertyName,
    //         parameters: readonly ParameterDeclaration[],
    //         body: Block | undefined
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.name !== name
    //             || node.parameters !== parameters
    //             || node.body !== body
    //             ? updateBaseFunctionLikeDeclaration(createSetAccessorDeclaration(decorators, modifiers, name, parameters, body), node)
    //             : node;
    //     }

    pub fn createCallSignature(
        &mut self,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        ty: Option<TypeNode>,
    ) -> CallSignatureDeclaration {
        let node = CallSignatureDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            typeParameters: self.asNodeArray(typeParameters),
            parameters: self.updateNodeArray(Some(parameters), None),
            ty,
            typeArguments: None,
        };
        let transform_flags = propagateChildrenFlags(node.typeParameters.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | self.propagateChildFlags(node.ty.as_ref())
            | TransformFlags::ContainsTypeScript;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateCallSignature(
    //         node: CallSignatureDeclaration,
    //         typeParameters: NodeArray<TypeParameterDeclaration> | undefined,
    //         parameters: NodeArray<ParameterDeclaration>,
    //         type: TypeNode | undefined
    //     ) {
    //         return node.typeParameters !== typeParameters
    //             || node.parameters !== parameters
    //             || node.type !== type
    //             ? updateBaseSignatureDeclaration(createCallSignature(typeParameters, parameters, type), node)
    //             : node;
    //     }

    pub fn createConstructSignature(
        &mut self,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        ty: Option<TypeNode>,
    ) -> ConstructSignatureDeclaration {
        let node = ConstructSignatureDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            typeParameters: self.asNodeArray(typeParameters),
            parameters: self.updateNodeArray(Some(parameters), None),
            ty,
            typeArguments: None,
        };
        let transform_flags = propagateChildrenFlags(node.typeParameters.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | self.propagateChildFlags(node.ty.as_ref())
            | TransformFlags::ContainsTypeScript;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateConstructSignature(
    //         node: ConstructSignatureDeclaration,
    //         typeParameters: NodeArray<TypeParameterDeclaration> | undefined,
    //         parameters: NodeArray<ParameterDeclaration>,
    //         type: TypeNode | undefined
    //     ) {
    //         return node.typeParameters !== typeParameters
    //             || node.parameters !== parameters
    //             || node.type !== type
    //             ? updateBaseSignatureDeclaration(createConstructSignature(typeParameters, parameters, type), node)
    //             : node;
    //     }

    pub fn createIndexSignature(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        ty: Option<TypeNode>,
    ) -> IndexSignatureDeclaration {
        let node = IndexSignatureDeclaration {
            node_id: self.node_id_gen.next(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            js_doc_container: JSDocContainer::default(),
            questionToken: None,
            parameters: self.updateNodeArray(Some(parameters), None),
            ty,
        };
        let transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | self.propagateChildFlags(node.ty.as_ref())
            | TransformFlags::ContainsTypeScript;

        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateIndexSignature(
    //         node: IndexSignatureDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         parameters: readonly ParameterDeclaration[],
    //         type: TypeNode
    //     ) {
    //         return node.parameters !== parameters
    //             || node.type !== type
    //             || node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             ? updateBaseSignatureDeclaration(createIndexSignature(decorators, modifiers, parameters, type), node)
    //             : node;
    //     }

    pub fn createTemplateLiteralTypeSpan(
        &mut self,
        ty: TypeNode,
        literal: TemplateSpanLiteral,
    ) -> TemplateLiteralTypeSpan {
        let node = TemplateLiteralTypeSpan {
            node_id: self.node_id_gen.next(),
            ty,
            literal,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateTemplateLiteralTypeSpan(node: TemplateLiteralTypeSpan, type: TypeNode, literal: TemplateMiddle | TemplateTail) {
    //         return node.type !== type
    //             || node.literal !== literal
    //             ? update(createTemplateLiteralTypeSpan(type, literal), node)
    //             : node;
    //     }

    //     //
    //     // Types
    //     //

    //     // @api
    //     function createKeywordTypeNode<TKind extends KeywordTypeSyntaxKind>(kind: TKind) {
    //         return createToken(kind);
    //     }

    pub fn createTypePredicateNode(
        &mut self,
        assertsModifier: Option<Rc<AssertsKeyword>>,
        parameterName: TypePredicateParameterName,
        ty: Option<TypeNode>,
    ) -> TypePredicateNode {
        let node = TypePredicateNode {
            node_id: self.node_id_gen.next(),
            assertsModifier,
            parameterName,
            ty,
        };
        self.node_data_mut(&node).transformFlags = TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateTypePredicateNode(node: TypePredicateNode, assertsModifier: AssertsKeyword | undefined, parameterName: Identifier | ThisTypeNode, type: TypeNode | undefined) {
    //         return node.assertsModifier !== assertsModifier
    //             || node.parameterName !== parameterName
    //             || node.type !== type
    //             ? update(createTypePredicateNode(assertsModifier, parameterName, type), node)
    //             : node;
    //     }

    pub fn createTypeReferenceNode(
        &mut self,
        typeName: EntityName,
        typeArguments: Option<NodeArray<TypeNode>>,
    ) -> TypeReferenceNode {
        let node = TypeReferenceNode {
            node_id: self.node_id_gen.next(),
            // TODO:
            // typeArguments: typeArguments && parenthesizerRules().parenthesizeTypeArguments(createNodeArray(typeArguments)),
            typeArguments,
            typeName,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateTypeReferenceNode(node: TypeReferenceNode, typeName: EntityName, typeArguments: NodeArray<TypeNode> | undefined) {
    //         return node.typeName !== typeName
    //             || node.typeArguments !== typeArguments
    //             ? update(createTypeReferenceNode(typeName, typeArguments), node)
    //             : node;
    //     }

    pub fn createFunctionTypeNode(
        &mut self,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        ty: Option<TypeNode>,
    ) -> FunctionTypeNode {
        let node = FunctionTypeNode {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            modifiers: None,
            typeParameters: self.asNodeArray(typeParameters),
            parameters: self.updateNodeArray(Some(parameters), None),
            typeArguments: None,
            ty,
        };
        let transform_flags = propagateChildrenFlags(node.typeParameters.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | self.propagateChildFlags(node.ty.as_ref())
            | TransformFlags::ContainsTypeScript;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateFunctionTypeNode(
    //         node: FunctionTypeNode,
    //         typeParameters: NodeArray<TypeParameterDeclaration> | undefined,
    //         parameters: NodeArray<ParameterDeclaration>,
    //         type: TypeNode | undefined
    //     ) {
    //         return node.typeParameters !== typeParameters
    //             || node.parameters !== parameters
    //             || node.type !== type
    //             ? updateBaseSignatureDeclaration(createFunctionTypeNode(typeParameters, parameters, type), node)
    //             : node;
    //     }

    pub fn createConstructorTypeNode(
        &mut self,
        modifiers: Option<NodeArray<Modifier>>,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        ty: Option<TypeNode>,
    ) -> ConstructorTypeNode {
        let node = ConstructorTypeNode {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            modifiers: self.asNodeArray(modifiers),
            typeParameters: self.asNodeArray(typeParameters),
            parameters: self.updateNodeArray(Some(parameters), None),
            typeArguments: None,
            ty,
        };
        let transform_flags = propagateChildrenFlags(node.modifiers.as_ref())
            | propagateChildrenFlags(node.typeParameters.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | self.propagateChildFlags(node.ty.as_ref())
            | TransformFlags::ContainsTypeScript;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function createConstructorTypeNode(...args: Parameters<typeof createConstructorTypeNode1 | typeof createConstructorTypeNode2>) {
    //         return args.length === 4 ? createConstructorTypeNode1(...args) :
    //             args.length === 3 ? createConstructorTypeNode2(...args) :
    //             Debug.fail("Incorrect number of arguments specified.");
    //     }

    //     function createConstructorTypeNode1(
    //         modifiers: readonly Modifier[] | undefined,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         parameters: readonly ParameterDeclaration[],
    //         type: TypeNode | undefined
    //     ): ConstructorTypeNode {
    //         const node = createBaseSignatureDeclaration<ConstructorTypeNode>(
    //             SyntaxKind::ConstructorType,
    //             /*decorators*/ undefined,
    //             modifiers,
    //             /*name*/ undefined,
    //             typeParameters,
    //             parameters,
    //             type
    //         );
    //         node.transformFlags = TransformFlags::ContainsTypeScript;
    //         return node;
    //     }

    //     /** @deprecated */
    //     function createConstructorTypeNode2(
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         parameters: readonly ParameterDeclaration[],
    //         type: TypeNode | undefined
    //     ): ConstructorTypeNode {
    //         return createConstructorTypeNode1(/*modifiers*/ undefined, typeParameters, parameters, type);
    //     }

    //     // @api
    //     function updateConstructorTypeNode(...args: Parameters<typeof updateConstructorTypeNode1 | typeof updateConstructorTypeNode2>) {
    //         return args.length === 5 ? updateConstructorTypeNode1(...args) :
    //             args.length === 4 ? updateConstructorTypeNode2(...args) :
    //             Debug.fail("Incorrect number of arguments specified.");
    //     }

    //     function updateConstructorTypeNode1(
    //         node: ConstructorTypeNode,
    //         modifiers: readonly Modifier[] | undefined,
    //         typeParameters: NodeArray<TypeParameterDeclaration> | undefined,
    //         parameters: NodeArray<ParameterDeclaration>,
    //         type: TypeNode | undefined
    //     ) {
    //         return node.modifiers !== modifiers
    //             || node.typeParameters !== typeParameters
    //             || node.parameters !== parameters
    //             || node.type !== type
    //             ? updateBaseSignatureDeclaration(createConstructorTypeNode(modifiers, typeParameters, parameters, type), node)
    //             : node;
    //     }

    //     /** @deprecated */
    //     function updateConstructorTypeNode2(
    //         node: ConstructorTypeNode,
    //         typeParameters: NodeArray<TypeParameterDeclaration> | undefined,
    //         parameters: NodeArray<ParameterDeclaration>,
    //         type: TypeNode | undefined
    //     ) {
    //         return updateConstructorTypeNode1(node, node.modifiers, typeParameters, parameters, type);
    //     }

    pub fn createTypeQueryNode(&mut self, exprName: EntityName) -> TypeQueryNode {
        let node = TypeQueryNode {
            node_id: self.node_id_gen.next(),
            exprName,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateTypeQueryNode(node: TypeQueryNode, exprName: EntityName) {
    //         return node.exprName !== exprName
    //             ? update(createTypeQueryNode(exprName), node)
    //             : node;
    //     }

    pub fn createTypeLiteralNode(
        &mut self,
        members: Option<NodeArray<TypeElement>>,
    ) -> TypeLiteralNode {
        let node = TypeLiteralNode {
            node_id: self.node_id_gen.next(),
            members: self.updateNodeArray(members, None),
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateTypeLiteralNode(node: TypeLiteralNode, members: NodeArray<TypeElement>) {
    //         return node.members !== members
    //             ? update(createTypeLiteralNode(members), node)
    //             : node;
    //     }

    pub fn createArrayTypeNode(&mut self, elementType: TypeNode) -> ArrayTypeNode {
        let node = ArrayTypeNode {
            node_id: self.node_id_gen.next(),
            // TODO:
            // elementType: parenthesizerRules().parenthesizeElementTypeOfArrayType(elementType),
            elementType,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateArrayTypeNode(node: ArrayTypeNode, elementType: TypeNode): ArrayTypeNode {
    //         return node.elementType !== elementType
    //             ? update(createArrayTypeNode(elementType), node)
    //             : node;
    //     }

    pub fn createTupleTypeNode(&mut self, elements: NodeArray<TypeNode>) -> TupleTypeNode {
        let node = TupleTypeNode {
            node_id: self.node_id_gen.next(),
            elements: self.updateNodeArray(Some(elements), None),
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateTupleTypeNode(node: TupleTypeNode, elements: readonly (TypeNode | NamedTupleMember)[]) {
    //         return node.elements !== elements
    //             ? update(createTupleTypeNode(elements), node)
    //             : node;
    //     }

    pub fn createNamedTupleMember(
        &mut self,
        dotDotDotToken: Option<Rc<DotDotDotToken>>,
        name: Rc<Identifier>,
        questionToken: Option<Rc<QuestionToken>>,
        ty: TypeNode,
    ) -> NamedTupleMember {
        let node = NamedTupleMember {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            dotDotDotToken,
            name,
            questionToken,
            ty,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateNamedTupleMember(node: NamedTupleMember, dotDotDotToken: DotDotDotToken | undefined, name: Identifier, questionToken: QuestionToken | undefined, type: TypeNode) {
    //         return node.dotDotDotToken !== dotDotDotToken
    //             || node.name !== name
    //             || node.questionToken !== questionToken
    //             || node.type !== type
    //             ? update(createNamedTupleMember(dotDotDotToken, name, questionToken, type), node)
    //             : node;
    //     }

    pub fn createOptionalTypeNode(&mut self, ty: TypeNode) -> OptionalTypeNode {
        let node = OptionalTypeNode {
            node_id: self.node_id_gen.next(),
            // TODO:
            // ty: parenthesizerRules().parenthesizeElementTypeOfArrayType(ty),
            ty,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateOptionalTypeNode(node: OptionalTypeNode, type: TypeNode): OptionalTypeNode {
    //         return node.type !== type
    //             ? update(createOptionalTypeNode(type), node)
    //             : node;
    //     }

    pub fn createRestTypeNode(&mut self, ty: TypeNode) -> RestTypeNode {
        let node = RestTypeNode {
            node_id: self.node_id_gen.next(),
            ty,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateRestTypeNode(node: RestTypeNode, type: TypeNode): RestTypeNode {
    //         return node.type !== type
    //             ? update(createRestTypeNode(type), node)
    //             : node;
    //     }

    //     function createUnionOrIntersectionTypeNode(kind: SyntaxKind::UnionType | SyntaxKind::IntersectionType, types: readonly TypeNode[]) {
    //         const node = createBaseNode<UnionTypeNode | IntersectionTypeNode>(kind);
    //         node.types = parenthesizerRules().parenthesizeConstituentTypesOfUnionOrIntersectionType(types);
    //         node.transformFlags = TransformFlags::ContainsTypeScript;
    //         return node;
    //     }

    //     function updateUnionOrIntersectionTypeNode<T extends UnionOrIntersectionTypeNode>(node: T, types: NodeArray<TypeNode>): T {
    //         return node.types !== types
    //             ? update(createUnionOrIntersectionTypeNode(node.kind, types) as T, node)
    //             : node;
    //     }

    pub fn createUnionTypeNode(&mut self, types: NodeArray<TypeNode>) -> UnionTypeNode {
        let node = UnionTypeNode {
            node_id: self.node_id_gen.next(),
            // TODO:
            // types: parenthesizerRules().parenthesizeConstituentTypesOfUnionOrIntersectionType(types),
            types,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateUnionTypeNode(node: UnionTypeNode, types: NodeArray<TypeNode>) {
    //         return updateUnionOrIntersectionTypeNode(node, types);
    //     }

    pub fn createIntersectionTypeNode(
        &mut self,
        types: NodeArray<TypeNode>,
    ) -> IntersectionTypeNode {
        let node = IntersectionTypeNode {
            node_id: self.node_id_gen.next(),
            // TODO:
            // types: parenthesizerRules().parenthesizeConstituentTypesOfUnionOrIntersectionType(types),
            types,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateIntersectionTypeNode(node: IntersectionTypeNode, types: NodeArray<TypeNode>) {
    //         return updateUnionOrIntersectionTypeNode(node, types);
    //     }

    //     // @api
    //     function createConditionalTypeNode(checkType: TypeNode, extendsType: TypeNode, trueType: TypeNode, falseType: TypeNode) {
    //         const node = createBaseNode<ConditionalTypeNode>(SyntaxKind::ConditionalType);
    //         node.checkType = parenthesizerRules().parenthesizeMemberOfConditionalType(checkType);
    //         node.extendsType = parenthesizerRules().parenthesizeMemberOfConditionalType(extendsType);
    //         node.trueType = trueType;
    //         node.falseType = falseType;
    //         node.transformFlags = TransformFlags::ContainsTypeScript;
    //         return node;
    //     }

    //     // @api
    //     function updateConditionalTypeNode(node: ConditionalTypeNode, checkType: TypeNode, extendsType: TypeNode, trueType: TypeNode, falseType: TypeNode) {
    //         return node.checkType !== checkType
    //             || node.extendsType !== extendsType
    //             || node.trueType !== trueType
    //             || node.falseType !== falseType
    //             ? update(createConditionalTypeNode(checkType, extendsType, trueType, falseType), node)
    //             : node;
    //     }

    pub fn createInferTypeNode(
        &mut self,
        typeParameter: Rc<TypeParameterDeclaration>,
    ) -> InferTypeNode {
        let node = InferTypeNode {
            node_id: self.node_id_gen.next(),
            typeParameter,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateInferTypeNode(node: InferTypeNode, typeParameter: TypeParameterDeclaration) {
    //         return node.typeParameter !== typeParameter
    //             ? update(createInferTypeNode(typeParameter), node)
    //             : node;
    //     }

    pub fn createTemplateLiteralType(
        &mut self,
        head: Rc<TemplateHead>,
        templateSpans: NodeArray<Rc<TemplateLiteralTypeSpan>>,
    ) -> TemplateLiteralTypeNode {
        let node = TemplateLiteralTypeNode {
            node_id: self.node_id_gen.next(),
            head,
            templateSpans: self.updateNodeArray(Some(templateSpans), None),
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateTemplateLiteralType(node: TemplateLiteralTypeNode, head: TemplateHead, templateSpans: readonly TemplateLiteralTypeSpan[]) {
    //         return node.head !== head
    //             || node.templateSpans !== templateSpans
    //             ? update(createTemplateLiteralType(head, templateSpans), node)
    //             : node;
    //     }

    pub fn createImportTypeNode(
        &mut self,
        argument: TypeNode,
        qualifier: Option<EntityName>,
        typeArguments: Option<NodeArray<TypeNode>>,
        isTypeOf: bool,
    ) -> ImportTypeNode {
        let node = ImportTypeNode {
            node_id: self.node_id_gen.next(),
            // TODO:
            // typeArguments: typeArguments && parenthesizerRules().parenthesizeTypeArguments(typeArguments),
            typeArguments,
            isTypeOf,
            argument,
            qualifier,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateImportTypeNode(node: ImportTypeNode, argument: TypeNode, qualifier: EntityName | undefined, typeArguments: readonly TypeNode[] | undefined, isTypeOf = node.isTypeOf) {
    //         return node.argument !== argument
    //             || node.qualifier !== qualifier
    //             || node.typeArguments !== typeArguments
    //             || node.isTypeOf !== isTypeOf
    //             ? update(createImportTypeNode(argument, qualifier, typeArguments, isTypeOf), node)
    //             : node;
    //     }

    pub fn createParenthesizedType(&mut self, ty: TypeNode) -> ParenthesizedTypeNode {
        let node = ParenthesizedTypeNode {
            node_id: self.node_id_gen.next(),
            ty,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateParenthesizedType(node: ParenthesizedTypeNode, type: TypeNode) {
    //         return node.type !== type
    //             ? update(createParenthesizedType(type), node)
    //             : node;
    //     }

    pub fn createThisTypeNode(&mut self) -> ThisTypeNode {
        let node = ThisTypeNode {
            node_id: self.node_id_gen.next(),
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    pub fn createTypeOperatorNode(
        &mut self,
        operator: SyntaxKind,
        ty: TypeNode,
    ) -> TypeOperatorNode {
        debug_assert!(matches!(
            operator,
            SyntaxKind::KeyOfKeyword | SyntaxKind::UniqueKeyword | SyntaxKind::ReadonlyKeyword
        ));
        let node = TypeOperatorNode {
            node_id: self.node_id_gen.next(),
            operator,
            // TODO:
            // ty: parenthesizerRules().parenthesizeMemberOfElementType(ty),
            ty,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateTypeOperatorNode(node: TypeOperatorNode, type: TypeNode) {
    //         return node.type !== type
    //             ? update(createTypeOperatorNode(node.operator, type), node)
    //             : node;
    //     }

    pub fn createIndexedAccessTypeNode(
        &mut self,
        objectType: TypeNode,
        indexType: TypeNode,
    ) -> IndexedAccessTypeNode {
        let node = IndexedAccessTypeNode {
            node_id: self.node_id_gen.next(),
            // TODO:
            // objectType: parenthesizerRules().parenthesizeMemberOfElementType(objectType),
            objectType,
            indexType,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateIndexedAccessTypeNode(node: IndexedAccessTypeNode, objectType: TypeNode, indexType: TypeNode) {
    //         return node.objectType !== objectType
    //             || node.indexType !== indexType
    //             ? update(createIndexedAccessTypeNode(objectType, indexType), node)
    //             : node;
    //     }

    pub fn createMappedTypeNode(
        &mut self,
        readonlyToken: Option<SyntaxKind>,
        typeParameter: Rc<TypeParameterDeclaration>,
        nameType: Option<TypeNode>,
        questionToken: Option<SyntaxKind>,
        ty: Option<TypeNode>,
    ) -> MappedTypeNode {
        debug_assert!(matches!(
            readonlyToken,
            Some(SyntaxKind::ReadonlyKeyword | SyntaxKind::PlusToken | SyntaxKind::MinusToken)
                | None
        ));
        debug_assert!(matches!(
            questionToken,
            Some(SyntaxKind::QuestionToken | SyntaxKind::PlusToken | SyntaxKind::MinusToken) | None
        ));
        let node = MappedTypeNode {
            node_id: self.node_id_gen.next(),
            readonlyToken,
            typeParameter,
            nameType,
            questionToken,
            ty,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateMappedTypeNode(node: MappedTypeNode, readonlyToken: ReadonlyKeyword | PlusToken | MinusToken | undefined, typeParameter: TypeParameterDeclaration, nameType: TypeNode | undefined, questionToken: QuestionToken | PlusToken | MinusToken | undefined, type: TypeNode | undefined): MappedTypeNode {
    //         return node.readonlyToken !== readonlyToken
    //             || node.typeParameter !== typeParameter
    //             || node.nameType !== nameType
    //             || node.questionToken !== questionToken
    //             || node.type !== type
    //             ? update(createMappedTypeNode(readonlyToken, typeParameter, nameType, questionToken, type), node)
    //             : node;
    //     }

    pub fn createLiteralTypeNode(&mut self, literal: LiteralTypeNodeKind) -> LiteralTypeNode {
        let node = LiteralTypeNode {
            node_id: self.node_id_gen.next(),
            literal,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsTypeScript;
        node
    }

    //     // @api
    //     function updateLiteralTypeNode(node: LiteralTypeNode, literal: LiteralTypeNode["literal"]) {
    //         return node.literal !== literal
    //             ? update(createLiteralTypeNode(literal), node)
    //             : node;
    //     }

    //     //
    //     // Binding Patterns
    //     //

    pub fn createObjectBindingPattern(
        &mut self,
        elements: NodeArray<Rc<BindingElement>>,
    ) -> ObjectBindingPattern {
        let node = ObjectBindingPattern {
            node_id: self.node_id_gen.next(),
            elements: self.updateNodeArray(Some(elements), None),
        };
        let mut transform_flags = propagateChildrenFlags(Some(&node.elements))
            | TransformFlags::ContainsES2015
            | TransformFlags::ContainsBindingPattern;
        if transform_flags.intersects(TransformFlags::ContainsRestOrSpread) {
            transform_flags |=
                TransformFlags::ContainsES2018 | TransformFlags::ContainsObjectRestOrSpread;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateObjectBindingPattern(node: ObjectBindingPattern, elements: readonly BindingElement[]) {
    //         return node.elements !== elements
    //             ? update(createObjectBindingPattern(elements), node)
    //             : node;
    //     }

    pub fn createArrayBindingPattern(
        &mut self,
        elements: NodeArray<ArrayBindingElement>,
    ) -> ArrayBindingPattern {
        let node = ArrayBindingPattern {
            node_id: self.node_id_gen.next(),
            elements: self.updateNodeArray(Some(elements), None),
        };
        let transform_flags = propagateChildrenFlags(Some(&node.elements))
            | TransformFlags::ContainsES2015
            | TransformFlags::ContainsBindingPattern;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateArrayBindingPattern(node: ArrayBindingPattern, elements: readonly ArrayBindingElement[]) {
    //         return node.elements !== elements
    //             ? update(createArrayBindingPattern(elements), node)
    //             : node;
    //     }

    pub fn createBindingElement(
        &mut self,
        dotDotDotToken: Option<Rc<DotDotDotToken>>,
        propertyName: Option<PropertyName>,
        name: BindingName,
        initializer: Option<Expression>,
    ) -> BindingElement {
        let node = BindingElement {
            node_id: self.node_id_gen.next(),
            propertyName,
            dotDotDotToken,
            name,
            // TODO:
            // initializer: initializer && parenthesizerRules().parenthesizeExpressionForDisallowedComma(initializer),
            initializer,
        };
        let mut transform_flags = self.propagateChildFlags(Some(&node.name))
            | self.propagateChildFlags(node.initializer.as_ref())
            | self.propagateChildFlags(node.dotDotDotToken.as_ref())
            | TransformFlags::ContainsES2015;
        if let Some(property_name) = &node.propertyName {
            transform_flags |= if let PropertyName::Identifier(property_name) = property_name {
                self.propagateIdentifierNameFlags(property_name)
            } else {
                self.propagateChildFlags(Some(property_name))
            };
        }
        if node.dotDotDotToken.is_some() {
            transform_flags |= TransformFlags::ContainsRestOrSpread;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateBindingElement(node: BindingElement, dotDotDotToken: DotDotDotToken | undefined, propertyName: PropertyName | undefined, name: BindingName, initializer: Expression | undefined) {
    //         return node.propertyName !== propertyName
    //             || node.dotDotDotToken !== dotDotDotToken
    //             || node.name !== name
    //             || node.initializer !== initializer
    //             ? update(createBindingElement(dotDotDotToken, propertyName, name, initializer), node)
    //             : node;
    //     }

    //     //
    //     // Expression
    //     //

    //     function createBaseExpression<T extends Expression>(kind: T["kind"]) {
    //         const node = createBaseNode(kind);
    //         // the following properties are commonly set by the checker/binder
    //         return node;
    //     }

    pub fn createArrayLiteralExpression(
        &mut self,
        elements: Option<NodeArray<Expression>>,
        multiLine: bool,
    ) -> ArrayLiteralExpression {
        // Ensure we add a trailing comma for something like `[NumericLiteral(1), NumericLiteral(2), OmittedExpresion]` so that
        // we end up with `[1, 2, ,]` instead of `[1, 2, ]` otherwise the `OmittedExpression` will just end up being treated like
        // a trailing comma.
        let lastElement = elements.as_ref().and_then(|e| e.last());
        let hasTrailingComma = if lastElement.is_some() && isOmittedExpression(lastElement.unwrap())
        {
            Some(true)
        } else {
            None
        };
        let elementsArray = self.updateNodeArray(elements, hasTrailingComma);

        let node = ArrayLiteralExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // elements: parenthesizerRules().parenthesizeExpressionsOfCommaDelimitedList(elementsArray),
            elements: elementsArray,
            multiLine,
        };

        self.node_data_mut(&node).transformFlags |= propagateChildrenFlags(Some(&node.elements));
        node
    }

    //     // @api
    //     function updateArrayLiteralExpression(node: ArrayLiteralExpression, elements: readonly Expression[]) {
    //         return node.elements !== elements
    //             ? update(createArrayLiteralExpression(elements, node.multiLine), node)
    //             : node;
    //     }

    pub fn createObjectLiteralExpression(
        &mut self,
        properties: Option<NodeArray<ObjectLiteralElementLike>>,
        multiLine: bool,
    ) -> ObjectLiteralExpression {
        let node = ObjectLiteralExpression {
            node_id: self.node_id_gen.next(),
            properties: self.updateNodeArray(properties, None),
            multiLine,
        };
        let transform_flags = propagateChildrenFlags(Some(&node.properties));
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateObjectLiteralExpression(node: ObjectLiteralExpression, properties: readonly ObjectLiteralElementLike[]) {
    //         return node.properties !== properties
    //             ? update(createObjectLiteralExpression(properties, node.multiLine), node)
    //             : node;
    //     }

    pub fn createPropertyAccessExpression(
        &mut self,
        expression: LeftHandSideExpression,
        name: MemberName,
    ) -> PropertyAccessExpression {
        let node = PropertyAccessExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeLeftSideOfAccess(expression),
            expression,
            questionDotToken: None,
            name,
        };
        let mut transform_flags = self.propagateChildFlags(Some(&node.expression));
        if let MemberName::Identifier(name) = &node.name {
            transform_flags |= self.propagateIdentifierNameFlags(name);
        } else {
            transform_flags |= self.propagateChildFlags(Some(&node.name));
        }
        if matches!(node.expression, LeftHandSideExpression::SuperExpression(_)) {
            // super method calls require a lexical 'this'
            // super method calls require 'super' hoisting in ES2017 and ES2018 async functions and async generators
            transform_flags |= TransformFlags::ContainsES2017 | TransformFlags::ContainsES2018;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updatePropertyAccessExpression(node: PropertyAccessExpression, expression: Expression, name: Identifier | PrivateIdentifier) {
    //         if (isPropertyAccessChain(node)) {
    //             return updatePropertyAccessChain(node, expression, node.questionDotToken, cast(name, isIdentifier));
    //         }
    //         return node.expression !== expression
    //             || node.name !== name
    //             ? update(createPropertyAccessExpression(expression, name), node)
    //             : node;
    //     }

    pub fn createPropertyAccessChain(
        &mut self,
        expression: LeftHandSideExpression,
        questionDotToken: Option<Rc<QuestionDotToken>>,
        name: MemberName,
    ) -> PropertyAccessChain {
        let node = PropertyAccessChain {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeLeftSideOfAccess(expression),
            expression,
            questionDotToken,
            name,
        };
        self.node_data_mut(&node).flags |= NodeFlags::OptionalChain;
        let mut transform_flags = TransformFlags::ContainsES2020
            | self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(node.questionDotToken.as_ref());
        if let MemberName::Identifier(name) = &node.name {
            transform_flags |= self.propagateIdentifierNameFlags(name);
        } else {
            transform_flags |= self.propagateChildFlags(Some(&node.name));
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        if self
            .flags
            .intersects(NodeFactoryFlags::NoIndentationOnFreshPropertyAccess)
        {
            todo!();
            // setEmitFlags(node, EmitFlags::NoIndentation)
        } else {
            node
        }
    }

    //     // @api
    //     function updatePropertyAccessChain(node: PropertyAccessChain, expression: Expression, questionDotToken: QuestionDotToken | undefined, name: Identifier | PrivateIdentifier) {
    //         Debug.assert(!!(node.flags & NodeFlags.OptionalChain), "Cannot update a PropertyAccessExpression using updatePropertyAccessChain. Use updatePropertyAccess instead.");
    //         // Because we are updating an existing PropertyAccessChain we want to inherit its emitFlags
    //         // instead of using the default from createPropertyAccess
    //         return node.expression !== expression
    //             || node.questionDotToken !== questionDotToken
    //             || node.name !== name
    //             ? update(createPropertyAccessChain(expression, questionDotToken, name), node)
    //             : node;
    //     }

    pub fn createElementAccessExpression(
        &mut self,
        expression: LeftHandSideExpression,
        index: Expression,
    ) -> ElementAccessExpression {
        let node = ElementAccessExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeLeftSideOfAccess(expression),
            expression,
            questionDotToken: None,
            argumentExpression: index,
        };
        let mut transform_flags = self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(Some(&node.argumentExpression));
        if matches!(node.expression, LeftHandSideExpression::SuperExpression(_)) {
            // super method calls require a lexical 'this'
            // super method calls require 'super' hoisting in ES2017 and ES2018 async functions and async generators
            transform_flags |= TransformFlags::ContainsES2017 | TransformFlags::ContainsES2018;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateElementAccessExpression(node: ElementAccessExpression, expression: Expression, argumentExpression: Expression) {
    //         if (isElementAccessChain(node)) {
    //             return updateElementAccessChain(node, expression, node.questionDotToken, argumentExpression);
    //         }
    //         return node.expression !== expression
    //             || node.argumentExpression !== argumentExpression
    //             ? update(createElementAccessExpression(expression, argumentExpression), node)
    //             : node;
    //     }

    pub fn createElementAccessChain(
        &mut self,
        expression: LeftHandSideExpression,
        questionDotToken: Option<Rc<QuestionDotToken>>,
        index: Expression,
    ) -> ElementAccessChain {
        let node = ElementAccessChain {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeLeftSideOfAccess(expression),
            expression,
            questionDotToken,
            argumentExpression: index,
        };
        self.node_data_mut(&node).flags |= NodeFlags::OptionalChain;
        let transform_flags = self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(node.questionDotToken.as_ref())
            | self.propagateChildFlags(Some(&node.argumentExpression))
            | TransformFlags::ContainsES2020;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        if self
            .flags
            .intersects(NodeFactoryFlags::NoIndentationOnFreshPropertyAccess)
        {
            todo!();
            // setEmitFlags(node, EmitFlags::NoIndentation)
        } else {
            node
        }
    }

    //     // @api
    //     function updateElementAccessChain(node: ElementAccessChain, expression: Expression, questionDotToken: QuestionDotToken | undefined, argumentExpression: Expression) {
    //         Debug.assert(!!(node.flags & NodeFlags.OptionalChain), "Cannot update a ElementAccessExpression using updateElementAccessChain. Use updateElementAccess instead.");
    //         // Because we are updating an existing ElementAccessChain we want to inherit its emitFlags
    //         // instead of using the default from createElementAccess
    //         return node.expression !== expression
    //             || node.questionDotToken !== questionDotToken
    //             || node.argumentExpression !== argumentExpression
    //             ? update(createElementAccessChain(expression, questionDotToken, argumentExpression), node)
    //             : node;
    //     }

    pub fn createCallExpression(
        &mut self,
        expression: LeftHandSideExpression,
        typeArguments: Option<NodeArray<TypeNode>>,
        argumentsArray: Option<NodeArray<Expression>>,
    ) -> CallExpression {
        let node = CallExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeLeftSideOfAccess(expression),
            expression,
            questionDotToken: None,
            typeArguments: self.asNodeArray(typeArguments),
            // TODO:
            // arguments: parenthesizerRules().parenthesizeExpressionsOfCommaDelimitedList(createNodeArray(argumentsArray)),
            arguments: self.updateNodeArray(argumentsArray, None),
        };
        let mut transform_flags = self.propagateChildFlags(Some(&node.expression))
            | propagateChildrenFlags(node.typeArguments.as_ref())
            | propagateChildrenFlags(Some(&node.arguments));
        if node.typeArguments.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        if isImportKeyword(&node.expression) {
            transform_flags |= TransformFlags::ContainsDynamicImport;
        } else if isSuperProperty(&node.expression.clone().into()) {
            transform_flags |= TransformFlags::ContainsLexicalThis;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateCallExpression(node: CallExpression, expression: Expression, typeArguments: readonly TypeNode[] | undefined, argumentsArray: readonly Expression[]) {
    //         if (isCallChain(node)) {
    //             return updateCallChain(node, expression, node.questionDotToken, typeArguments, argumentsArray);
    //         }
    //         return node.expression !== expression
    //             || node.typeArguments !== typeArguments
    //             || node.arguments !== argumentsArray
    //             ? update(createCallExpression(expression, typeArguments, argumentsArray), node)
    //             : node;
    //     }

    pub fn createCallChain(
        &mut self,
        expression: LeftHandSideExpression,
        questionDotToken: Option<Rc<QuestionDotToken>>,
        typeArguments: Option<NodeArray<TypeNode>>,
        argumentsArray: Option<NodeArray<Expression>>,
    ) -> CallChain {
        let node = CallChain {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression:  parenthesizerRules().parenthesizeLeftSideOfAccess(expression),
            expression,
            questionDotToken,
            typeArguments: self.asNodeArray(typeArguments),
            // TODO:
            // arguments: parenthesizerRules().parenthesizeExpressionsOfCommaDelimitedList(createNodeArray(argumentsArray)),
            arguments: self.updateNodeArray(argumentsArray, None),
        };
        self.node_data_mut(&node).flags |= NodeFlags::OptionalChain;
        let mut transform_flags = self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(node.questionDotToken.as_ref())
            | propagateChildrenFlags(node.typeArguments.as_ref())
            | propagateChildrenFlags(Some(&node.arguments))
            | TransformFlags::ContainsES2020;
        if node.typeArguments.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        if isSuperProperty(&node.expression.clone().into()) {
            transform_flags |= TransformFlags::ContainsLexicalThis;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateCallChain(node: CallChain, expression: Expression, questionDotToken: QuestionDotToken | undefined, typeArguments: readonly TypeNode[] | undefined, argumentsArray: readonly Expression[]) {
    //         Debug.assert(!!(node.flags & NodeFlags.OptionalChain), "Cannot update a CallExpression using updateCallChain. Use updateCall instead.");
    //         return node.expression !== expression
    //             || node.questionDotToken !== questionDotToken
    //             || node.typeArguments !== typeArguments
    //             || node.arguments !== argumentsArray
    //             ? update(createCallChain(expression, questionDotToken, typeArguments, argumentsArray), node)
    //             : node;
    //     }

    pub fn createNewExpression(
        &mut self,
        expression: LeftHandSideExpression,
        typeArguments: Option<NodeArray<TypeNode>>,
        argumentsArray: Option<NodeArray<Expression>>,
    ) -> NewExpression {
        let node = NewExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeExpressionOfNew(expression),
            expression,
            typeArguments: typeArguments.map(|a| self.updateNodeArray(Some(a), None)),
            // TODO:
            // arguments: argumentsArray.map(|a| parenthesizerRules().parenthesizeExpressionsOfCommaDelimitedList(a)),
            arguments: argumentsArray,
        };
        let mut transform_flags = self.propagateChildFlags(Some(&node.expression))
            | propagateChildrenFlags(node.typeArguments.as_ref())
            | propagateChildrenFlags(node.arguments.as_ref())
            | TransformFlags::ContainsES2020;
        if node.typeArguments.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateNewExpression(node: NewExpression, expression: Expression, typeArguments: readonly TypeNode[] | undefined, argumentsArray: readonly Expression[] | undefined) {
    //         return node.expression !== expression
    //             || node.typeArguments !== typeArguments
    //             || node.arguments !== argumentsArray
    //             ? update(createNewExpression(expression, typeArguments, argumentsArray), node)
    //             : node;
    //     }

    pub fn createTaggedTemplateExpression(
        &mut self,
        tag: LeftHandSideExpression,
        typeArguments: Option<NodeArray<TypeNode>>,
        template: TemplateLiteral,
    ) -> TaggedTemplateExpression {
        let node = TaggedTemplateExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // tag: parenthesizerRules().parenthesizeLeftSideOfAccess(tag),
            tag,
            typeArguments: self.asNodeArray(typeArguments),
            template,
            questionDotToken: None,
        };
        let mut transform_flags = self.propagateChildFlags(Some(&node.tag))
            | propagateChildrenFlags(node.typeArguments.as_ref())
            | self.propagateChildFlags(Some(&node.template))
            | TransformFlags::ContainsES2015;
        if node.typeArguments.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        if hasInvalidEscape(&node.template) {
            transform_flags |= TransformFlags::ContainsES2018;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateTaggedTemplateExpression(node: TaggedTemplateExpression, tag: Expression, typeArguments: readonly TypeNode[] | undefined, template: TemplateLiteral) {
    //         return node.tag !== tag
    //             || node.typeArguments !== typeArguments
    //             || node.template !== template
    //             ? update(createTaggedTemplateExpression(tag, typeArguments, template), node)
    //             : node;
    //     }

    pub fn createTypeAssertion(
        &mut self,
        ty: TypeNode,
        expression: UnaryExpression,
    ) -> TypeAssertion {
        let node = TypeAssertion {
            node_id: self.node_id_gen.next(),
            ty,
            // TODO:
            // expression: parenthesizerRules().parenthesizeOperandOfPrefixUnary(expression),
            expression,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(Some(&node.ty))
            | TransformFlags::ContainsTypeScript;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateTypeAssertion(node: TypeAssertion, type: TypeNode, expression: Expression) {
    //         return node.type !== type
    //             || node.expression !== expression
    //             ? update(createTypeAssertion(type, expression), node)
    //             : node;
    //     }

    pub fn createParenthesizedExpression(
        &mut self,
        expression: Expression,
    ) -> ParenthesizedExpression {
        let node = ParenthesizedExpression {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            expression,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.expression));
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateParenthesizedExpression(node: ParenthesizedExpression, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createParenthesizedExpression(expression), node)
    //             : node;
    //     }

    pub fn createFunctionExpression(
        &mut self,
        modifiers: Option<NodeArray<Modifier>>,
        asteriskToken: Option<Rc<AsteriskToken>>,
        name: Option<Rc<Identifier>>,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        parameters: Option<NodeArray<Rc<ParameterDeclaration>>>,
        ty: Option<TypeNode>,
        body: Rc<Block>,
    ) -> FunctionExpression {
        let node = FunctionExpression {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            modifiers: self.asNodeArray(modifiers),
            typeParameters: self.asNodeArray(typeParameters),
            parameters: self.updateNodeArray(parameters, None),
            ty,
            typeArguments: None,
            asteriskToken,
            name,
            body,
        };

        let mut transform_flags = propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(node.name.as_ref())
            | propagateChildrenFlags(node.typeParameters.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | self.propagateChildFlags(node.ty.as_ref())
            | (self.propagateChildFlags(Some(&node.body))
                & !TransformFlags::ContainsPossibleTopLevelAwait)
            | self.propagateChildFlags(node.asteriskToken.as_ref());

        if node.typeParameters.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }

        if node.ty.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }

        if node.typeParameters.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        if modifiersToFlags(node.modifiers.as_ref()).intersects(ModifierFlags::Async) {
            if node.asteriskToken.is_some() {
                transform_flags |= TransformFlags::ContainsES2018;
            } else {
                transform_flags |= TransformFlags::ContainsES2017;
            }
        } else if node.asteriskToken.is_some() {
            transform_flags |= TransformFlags::ContainsGenerator;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateFunctionExpression(
    //         node: FunctionExpression,
    //         modifiers: readonly Modifier[] | undefined,
    //         asteriskToken: AsteriskToken | undefined,
    //         name: Identifier | undefined,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         parameters: readonly ParameterDeclaration[],
    //         type: TypeNode | undefined,
    //         body: Block
    //     ) {
    //         return node.name !== name
    //             || node.modifiers !== modifiers
    //             || node.asteriskToken !== asteriskToken
    //             || node.typeParameters !== typeParameters
    //             || node.parameters !== parameters
    //             || node.type !== type
    //             || node.body !== body
    //             ? updateBaseFunctionLikeDeclaration(createFunctionExpression(modifiers, asteriskToken, name, typeParameters, parameters, type, body), node)
    //             : node;
    //     }

    pub fn createArrowFunction(
        &mut self,
        modifiers: Option<NodeArray<Modifier>>,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        ty: Option<TypeNode>,
        equalsGreaterThanToken: Option<Rc<EqualsGreaterThanToken>>,
        body: ConciseBody,
    ) -> ArrowFunction {
        let node = ArrowFunction {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            modifiers: self.asNodeArray(modifiers),
            typeParameters: self.asNodeArray(typeParameters),
            parameters: self.updateNodeArray(Some(parameters), None),
            ty,
            typeArguments: None,
            equalsGreaterThanToken: equalsGreaterThanToken
                .unwrap_or_else(|| Rc::new(self.createToken(SyntaxKind::EqualsGreaterThanToken))),
            // TODO:
            // body: parenthesizerRules().parenthesizeConciseBodyOfArrowFunction(body),
            body,
        };
        let mut transform_flags = propagateChildrenFlags(node.modifiers.as_ref())
            | propagateChildrenFlags(node.typeParameters.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | self.propagateChildFlags(node.ty.as_ref())
            | (self.propagateChildFlags(Some(&node.body))
                & !TransformFlags::ContainsPossibleTopLevelAwait)
            | self.propagateChildFlags(Some(&node.equalsGreaterThanToken))
            | TransformFlags::ContainsES2015;

        if node.typeParameters.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }

        if node.ty.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }

        if modifiersToFlags(node.modifiers.as_ref()).intersects(ModifierFlags::Async) {
            transform_flags |= TransformFlags::ContainsES2017 | TransformFlags::ContainsLexicalThis;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateArrowFunction(
    //         node: ArrowFunction,
    //         modifiers: readonly Modifier[] | undefined,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         parameters: readonly ParameterDeclaration[],
    //         type: TypeNode | undefined,
    //         equalsGreaterThanToken: EqualsGreaterThanToken,
    //         body: ConciseBody
    //     ): ArrowFunction {
    //         return node.modifiers !== modifiers
    //             || node.typeParameters !== typeParameters
    //             || node.parameters !== parameters
    //             || node.type !== type
    //             || node.equalsGreaterThanToken !== equalsGreaterThanToken
    //             || node.body !== body
    //             ? updateBaseFunctionLikeDeclaration(createArrowFunction(modifiers, typeParameters, parameters, type, equalsGreaterThanToken, body), node)
    //             : node;
    //     }

    pub fn createDeleteExpression(&mut self, expression: UnaryExpression) -> DeleteExpression {
        let node = DeleteExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeOperandOfPrefixUnary(expression),
            expression,
        };
        let child_flags = self.propagateChildFlags(Some(&node.expression));
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateDeleteExpression(node: DeleteExpression, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createDeleteExpression(expression), node)
    //             : node;
    //     }

    pub fn createTypeOfExpression(&mut self, expression: UnaryExpression) -> TypeOfExpression {
        let node = TypeOfExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeOperandOfPrefixUnary(expression),
            expression,
        };
        let child_flags = self.propagateChildFlags(Some(&node.expression));
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateTypeOfExpression(node: TypeOfExpression, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createTypeOfExpression(expression), node)
    //             : node;
    //     }

    pub fn createVoidExpression(&mut self, expression: UnaryExpression) -> VoidExpression {
        let node = VoidExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeOperandOfPrefixUnary(expression),
            expression,
        };
        let child_flags = self.propagateChildFlags(Some(&node.expression));
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateVoidExpression(node: VoidExpression, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createVoidExpression(expression), node)
    //             : node;
    //     }

    pub fn createAwaitExpression(&mut self, expression: UnaryExpression) -> AwaitExpression {
        let node = AwaitExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeOperandOfPrefixUnary(expression),
            expression,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.expression))
            | TransformFlags::ContainsES2017
            | TransformFlags::ContainsES2018
            | TransformFlags::ContainsAwait;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateAwaitExpression(node: AwaitExpression, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createAwaitExpression(expression), node)
    //             : node;
    //     }

    pub fn createPrefixUnaryExpression(
        &mut self,
        operator: SyntaxKind,
        operand: UnaryExpression,
    ) -> PrefixUnaryExpression {
        debug_assert!(matches!(
            operator,
            SyntaxKind::PlusPlusToken
                | SyntaxKind::MinusMinusToken
                | SyntaxKind::PlusToken
                | SyntaxKind::MinusToken
                | SyntaxKind::TildeToken
                | SyntaxKind::ExclamationToken
        ));
        let node = PrefixUnaryExpression {
            node_id: self.node_id_gen.next(),
            operator,
            // TODO:
            // operand: parenthesizerRules().parenthesizeOperandOfPrefixUnary(operand),
            operand,
        };
        let mut transform_flags = self.propagateChildFlags(Some(&node.operand));
        // Only set this flag for non-generated identifiers and non-"local" names. See the
        // comment in `visitPreOrPostfixUnaryExpression` in module.ts
        if operator == SyntaxKind::PlusPlusToken || operator == SyntaxKind::MinusMinusToken {
            if let UnaryExpression::Identifier(operand) = &node.operand {
                if !isGeneratedIdentifier(&operand)
                    && !isLocalName(&operand, self.node_data(&operand))
                {
                    transform_flags |= TransformFlags::ContainsUpdateExpressionForIdentifier;
                }
            }
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updatePrefixUnaryExpression(node: PrefixUnaryExpression, operand: Expression) {
    //         return node.operand !== operand
    //             ? update(createPrefixUnaryExpression(node.operator, operand), node)
    //             : node;
    //     }

    pub fn createPostfixUnaryExpression(
        &mut self,
        operand: LeftHandSideExpression,
        operator: SyntaxKind,
    ) -> PostfixUnaryExpression {
        debug_assert!(matches!(
            operator,
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ));
        let node = PostfixUnaryExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // operand: parenthesizerRules().parenthesizeOperandOfPostfixUnary(operand),
            operand,
            operator,
        };

        let mut transform_flags = self.propagateChildFlags(Some(&node.operand));
        // Only set this flag for non-generated identifiers and non-"local" names. See the
        // comment in `visitPreOrPostfixUnaryExpression` in module.ts
        if let LeftHandSideExpression::Identifier(operand) = &node.operand {
            if !isGeneratedIdentifier(operand) && !isLocalName(operand, self.node_data(&operand)) {
                transform_flags |= TransformFlags::ContainsUpdateExpressionForIdentifier;
            }
        }
        self.node_data_mut(&node).transformFlags = transform_flags;
        node
    }

    //     // @api
    //     function updatePostfixUnaryExpression(node: PostfixUnaryExpression, operand: Expression) {
    //         return node.operand !== operand
    //             ? update(createPostfixUnaryExpression(operand, node.operator), node)
    //             : node;
    //     }

    pub fn createBinaryExpression(
        &mut self,
        left: Expression,
        operatorToken: BinaryOperatorToken,
        right: Expression,
    ) -> BinaryExpression {
        let operatorKind = operatorToken.kind();
        let node = BinaryExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // left: parenthesizerRules().parenthesizeLeftSideOfBinary(operatorKind, left),
            left,
            operatorToken,
            // TODO:
            // right: parenthesizerRules().parenthesizeRightSideOfBinary(operatorKind, node.left, right),
            right,
        };
        let mut transform_flags = self.propagateChildFlags(Some(&node.left))
            | self.propagateChildFlags(Some(&node.operatorToken))
            | self.propagateChildFlags(Some(&node.right));
        if operatorKind == SyntaxKind::QuestionQuestionToken {
            transform_flags |= TransformFlags::ContainsES2020;
        } else if operatorKind == SyntaxKind::EqualsToken {
            if let Expression::ObjectLiteralExpression(left) = &node.left {
                transform_flags |= TransformFlags::ContainsES2015
                    | TransformFlags::ContainsES2018
                    | TransformFlags::ContainsDestructuringAssignment
                    | self.propagateAssignmentPatternFlags(
                        BindingOrAssignmentPattern::ObjectLiteralExpression(left.clone()),
                    );
            } else if let Expression::ArrayLiteralExpression(left) = &node.left {
                transform_flags |= TransformFlags::ContainsES2015
                    | TransformFlags::ContainsDestructuringAssignment
                    | self.propagateAssignmentPatternFlags(
                        BindingOrAssignmentPattern::ArrayLiteralExpression(left.clone()),
                    );
            }
        } else if operatorKind == SyntaxKind::AsteriskAsteriskToken
            || operatorKind == SyntaxKind::AsteriskAsteriskEqualsToken
        {
            transform_flags |= TransformFlags::ContainsES2016;
        } else if isLogicalOrCoalescingAssignmentOperator(operatorKind) {
            transform_flags |= TransformFlags::ContainsES2021;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    fn propagateAssignmentPatternFlags(
        &mut self,
        node: BindingOrAssignmentPattern,
    ) -> TransformFlags {
        let node_transform_flags = self.node_data(&node).transformFlags;
        if node_transform_flags.intersects(TransformFlags::ContainsObjectRestOrSpread) {
            return TransformFlags::ContainsObjectRestOrSpread;
        }
        if node_transform_flags.intersects(TransformFlags::ContainsES2018) {
            // check for nested spread assignments, otherwise '{ x: { a, ...b } = foo } = c'
            // will not be correctly interpreted by the ES2018 transformer
            for element in getElementsOfBindingOrAssignmentPattern(node) {
                if let Some(target) = getTargetOfBindingOrAssignmentElement(element) {
                    // TODO: ugly
                    let target = match target {
                        Node::ObjectBindingPattern(n) => {
                            BindingOrAssignmentPattern::ObjectBindingPattern(n)
                        }
                        Node::ObjectLiteralExpression(n) => {
                            BindingOrAssignmentPattern::ObjectLiteralExpression(n)
                        }
                        Node::ArrayBindingPattern(n) => {
                            BindingOrAssignmentPattern::ArrayBindingPattern(n)
                        }
                        Node::ArrayLiteralExpression(n) => {
                            BindingOrAssignmentPattern::ArrayLiteralExpression(n)
                        }
                        _ => continue,
                    };
                    let target_transform_flags = self.node_data(&target).transformFlags;
                    if target_transform_flags.intersects(TransformFlags::ContainsObjectRestOrSpread)
                    {
                        return TransformFlags::ContainsObjectRestOrSpread;
                    }
                    if target_transform_flags.intersects(TransformFlags::ContainsES2018) {
                        let flags = self.propagateAssignmentPatternFlags(target);
                        if !flags.is_empty() {
                            return flags;
                        }
                    }
                }
            }
        }
        TransformFlags::None
    }

    //     // @api
    //     function updateBinaryExpression(node: BinaryExpression, left: Expression, operator: BinaryOperatorToken, right: Expression) {
    //         return node.left !== left
    //             || node.operatorToken !== operator
    //             || node.right !== right
    //             ? update(createBinaryExpression(left, operator, right), node)
    //             : node;
    //     }

    pub fn createConditionalExpression(
        &mut self,
        condition: Expression,
        questionToken: Option<Rc<QuestionToken>>,
        whenTrue: Expression,
        colonToken: Option<Rc<ColonToken>>,
        whenFalse: Expression,
    ) -> ConditionalExpression {
        let node = ConditionalExpression {
            node_id: self.node_id_gen.next(),
            condition,
            // TODO:
            // condition: parenthesizerRules().parenthesizeConditionOfConditionalExpression(condition),
            questionToken: questionToken
                .unwrap_or_else(|| Rc::new(self.createToken(SyntaxKind::QuestionToken))),
            whenTrue,
            // TODO:
            // whenTrue: parenthesizerRules().parenthesizeBranchOfConditionalExpression(whenTrue),
            colonToken: colonToken
                .unwrap_or_else(|| Rc::new(self.createToken(SyntaxKind::ColonToken))),
            whenFalse,
            // TODO:
            // whenFalse: parenthesizerRules().parenthesizeBranchOfConditionalExpression(whenFalse),
        };
        let transformFlags = self.propagateChildFlags(Some(&node.condition))
            | self.propagateChildFlags(Some(&node.questionToken))
            | self.propagateChildFlags(Some(&node.whenTrue))
            | self.propagateChildFlags(Some(&node.colonToken))
            | self.propagateChildFlags(Some(&node.whenFalse));
        self.node_data_mut(&node).transformFlags |= transformFlags;
        node
    }

    //     // @api
    //     function updateConditionalExpression(
    //         node: ConditionalExpression,
    //         condition: Expression,
    //         questionToken: Token<SyntaxKind::QuestionToken>,
    //         whenTrue: Expression,
    //         colonToken: Token<SyntaxKind::ColonToken>,
    //         whenFalse: Expression
    //     ): ConditionalExpression {
    //         return node.condition !== condition
    //             || node.questionToken !== questionToken
    //             || node.whenTrue !== whenTrue
    //             || node.colonToken !== colonToken
    //             || node.whenFalse !== whenFalse
    //             ? update(createConditionalExpression(condition, questionToken, whenTrue, colonToken, whenFalse), node)
    //             : node;
    //     }

    pub fn createTemplateExpression(
        &mut self,
        head: Rc<TemplateHead>,
        templateSpans: NodeArray<Rc<TemplateSpan>>,
    ) -> TemplateExpression {
        let node = TemplateExpression {
            node_id: self.node_id_gen.next(),
            head,
            templateSpans: self.updateNodeArray(Some(templateSpans), None),
        };
        let transform_flags = self.propagateChildFlags(Some(&node.head))
            | propagateChildrenFlags(Some(&node.templateSpans))
            | TransformFlags::ContainsES2015;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateTemplateExpression(node: TemplateExpression, head: TemplateHead, templateSpans: readonly TemplateSpan[]) {
    //         return node.head !== head
    //             || node.templateSpans !== templateSpans
    //             ? update(createTemplateExpression(head, templateSpans), node)
    //             : node;
    //     }

    //     function createTemplateLiteralLikeNodeChecked(kind: TemplateLiteralToken["kind"], text: string | undefined, rawText: string | undefined, templateFlags = TokenFlags.None) {
    //         Debug.assert(!(templateFlags & ~TokenFlags.TemplateLiteralLikeFlags), "Unsupported template flags.");
    //         // NOTE: without the assignment to `undefined`, we don't narrow the initial type of `cooked`.
    //         // eslint-disable-next-line no-undef-init
    //         let cooked: string | object | undefined = undefined;
    //         if (rawText !== undefined && rawText !== text) {
    //             cooked = getCookedText(kind, rawText);
    //             if (typeof cooked === "object") {
    //                 return Debug.fail("Invalid raw text");
    //             }
    //         }
    //         if (text === undefined) {
    //             if (cooked === undefined) {
    //                 return Debug.fail("Arguments 'text' and 'rawText' may not both be undefined.");
    //             }
    //             text = cooked;
    //         }
    //         else if (cooked !== undefined) {
    //             Debug.assert(text === cooked, "Expected argument 'text' to be the normalized (i.e. 'cooked') version of argument 'rawText'.");
    //         }
    //         return createTemplateLiteralLikeNode(kind, text, rawText, templateFlags);
    //     }

    //     // @api
    //     function createTemplateLiteralLikeNode(kind: TemplateLiteralToken["kind"], text: string, rawText: string | undefined, templateFlags: TokenFlags | undefined) {
    //         const node = createBaseToken<TemplateLiteralLikeNode>(kind);
    //         node.text = text;
    //         node.rawText = rawText;
    //         node.templateFlags = templateFlags! & TokenFlags.TemplateLiteralLikeFlags;
    //         node.transformFlags |= TransformFlags::ContainsES2015;
    //         if (node.templateFlags) {
    //             node.transformFlags |= TransformFlags::ContainsES2018;
    //         }
    //         return node;
    //     }

    //     // @api
    //     function createTemplateHead(text: string | undefined, rawText?: string, templateFlags?: TokenFlags) {
    //         return createTemplateLiteralLikeNodeChecked(SyntaxKind::TemplateHead, text, rawText, templateFlags) as TemplateHead;
    //     }

    // Copied from `createTemplateLiteralLikeNode`.
    pub fn createTemplateHeadUnchecked(
        &mut self,
        text: Rc<str>,
        rawText: Option<Rc<str>>,
        templateFlags: TokenFlags,
    ) -> TemplateHead {
        let node = TemplateHead {
            node_id: self.node_id_gen.next(),
            text,
            isUnterminated: None,
            hasExtendedUnicodeEscape: None,
            rawText,
            templateFlags: templateFlags & TokenFlags::TemplateLiteralLikeFlags,
        };
        let mut transform_flags = TransformFlags::ContainsES2015;
        if !node.templateFlags.is_empty() {
            transform_flags |= TransformFlags::ContainsES2018;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function createTemplateMiddle(text: string | undefined, rawText?: string, templateFlags?: TokenFlags) {
    //         return createTemplateLiteralLikeNodeChecked(SyntaxKind::TemplateMiddle, text, rawText, templateFlags) as TemplateMiddle;
    //     }

    pub fn createTemplateMiddleUnchecked(
        &mut self,
        text: Rc<str>,
        rawText: Option<Rc<str>>,
        templateFlags: TokenFlags,
    ) -> TemplateMiddle {
        let node = TemplateMiddle {
            node_id: self.node_id_gen.next(),
            text,
            isUnterminated: None,
            hasExtendedUnicodeEscape: None,
            rawText,
            templateFlags: templateFlags & TokenFlags::TemplateLiteralLikeFlags,
        };
        let mut transform_flags = TransformFlags::ContainsES2015;
        if !node.templateFlags.is_empty() {
            transform_flags |= TransformFlags::ContainsES2018;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function createTemplateTail(text: string | undefined, rawText?: string, templateFlags?: TokenFlags) {
    //         return createTemplateLiteralLikeNodeChecked(SyntaxKind::TemplateTail, text, rawText, templateFlags) as TemplateTail;
    //     }

    pub fn createTemplateTailUnchecked(
        &mut self,
        text: Rc<str>,
        rawText: Option<Rc<str>>,
        templateFlags: TokenFlags,
    ) -> TemplateTail {
        let node = TemplateTail {
            node_id: self.node_id_gen.next(),
            text,
            isUnterminated: None,
            hasExtendedUnicodeEscape: None,
            rawText,
            templateFlags: templateFlags & TokenFlags::TemplateLiteralLikeFlags,
        };
        let mut transform_flags = TransformFlags::ContainsES2015;
        if !node.templateFlags.is_empty() {
            transform_flags |= TransformFlags::ContainsES2018;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function createNoSubstitutionTemplateLiteral(text: string | undefined, rawText?: string, templateFlags?: TokenFlags) {
    //         return createTemplateLiteralLikeNodeChecked(SyntaxKind::NoSubstitutionTemplateLiteral, text, rawText, templateFlags) as NoSubstitutionTemplateLiteral;
    //     }

    // Copied from `createTemplateLiteralLikeNode`.
    pub fn createNoSubstitutionTemplateLiteralUnchecked(
        &mut self,
        text: Rc<str>,
        rawText: Option<Rc<str>>,
        templateFlags: TokenFlags,
    ) -> NoSubstitutionTemplateLiteral {
        let node = NoSubstitutionTemplateLiteral {
            node_id: self.node_id_gen.next(),
            text,
            isUnterminated: None,
            hasExtendedUnicodeEscape: None,
            rawText,
            templateFlags: templateFlags & TokenFlags::TemplateLiteralLikeFlags,
        };
        let mut transform_flags = TransformFlags::ContainsES2015;
        if !node.templateFlags.is_empty() {
            transform_flags |= TransformFlags::ContainsES2018;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    pub fn createYieldExpression(
        &mut self,
        asteriskToken: Option<Rc<AsteriskToken>>,
        expression: Option<Expression>,
    ) -> YieldExpression {
        debug_assert!(
            asteriskToken.is_none() || expression.is_some(),
            "A `YieldExpression` with an asteriskToken must have an expression."
        );
        let node = YieldExpression {
            node_id: self.node_id_gen.next(),
            asteriskToken,
            // TODO:
            // expression: expression && parenthesizerRules().parenthesizeExpressionForDisallowedComma(expression),
            expression,
        };
        let transform_flags = self.propagateChildFlags(node.expression.as_ref())
            | self.propagateChildFlags(node.asteriskToken.as_ref())
            | TransformFlags::ContainsES2015
            | TransformFlags::ContainsES2018
            | TransformFlags::ContainsYield;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateYieldExpression(node: YieldExpression, asteriskToken: AsteriskToken | undefined, expression: Expression) {
    //         return node.expression !== expression
    //             || node.asteriskToken !== asteriskToken
    //             ? update(createYieldExpression(asteriskToken, expression), node)
    //             : node;
    //     }

    pub fn createSpreadElement(&mut self, expression: Expression) -> SpreadElement {
        let node = SpreadElement {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeExpressionForDisallowedComma(expression),
            expression,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.expression))
            | TransformFlags::ContainsES2015
            | TransformFlags::ContainsRestOrSpread;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateSpreadElement(node: SpreadElement, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createSpreadElement(expression), node)
    //             : node;
    //     }

    pub fn createClassExpression(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        name: Option<Rc<Identifier>>,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        heritageClauses: Option<NodeArray<Rc<HeritageClause>>>,
        members: NodeArray<ClassElement>,
    ) -> ClassExpression {
        let node = ClassExpression {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            name,
            typeParameters: self.asNodeArray(typeParameters),
            heritageClauses: self.asNodeArray(heritageClauses),
            members: self.updateNodeArray(Some(members), None),
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(node.name.as_ref())
            | propagateChildrenFlags(node.typeParameters.as_ref())
            | propagateChildrenFlags(node.heritageClauses.as_ref())
            | propagateChildrenFlags(Some(&node.members))
            | TransformFlags::ContainsES2015;

        if node.typeParameters.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateClassExpression(
    //         node: ClassExpression,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: Identifier | undefined,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         heritageClauses: readonly HeritageClause[] | undefined,
    //         members: readonly ClassElement[]
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.name !== name
    //             || node.typeParameters !== typeParameters
    //             || node.heritageClauses !== heritageClauses
    //             || node.members !== members
    //             ? update(createClassExpression(decorators, modifiers, name, typeParameters, heritageClauses, members), node)
    //             : node;
    //     }

    pub fn createOmittedExpression(&mut self) -> OmittedExpression {
        OmittedExpression {
            node_id: self.node_id_gen.next(),
        }
    }

    pub fn createExpressionWithTypeArguments(
        &mut self,
        expression: LeftHandSideExpression,
        typeArguments: Option<NodeArray<TypeNode>>,
    ) -> ExpressionWithTypeArguments {
        let node = ExpressionWithTypeArguments {
            node_id: self.node_id_gen.next(),
            // TODO:
            // typeArguments: typeArguments && parenthesizerRules().parenthesizeTypeArguments(typeArguments),
            typeArguments,
            // TODO:
            // expression: parenthesizerRules().parenthesizeLeftSideOfAccess(expression),
            expression,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.expression))
            | propagateChildrenFlags(node.typeArguments.as_ref())
            | TransformFlags::ContainsES2015;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateExpressionWithTypeArguments(node: ExpressionWithTypeArguments, expression: Expression, typeArguments: readonly TypeNode[] | undefined) {
    //         return node.expression !== expression
    //             || node.typeArguments !== typeArguments
    //             ? update(createExpressionWithTypeArguments(expression, typeArguments), node)
    //             : node;
    //     }

    pub fn createAsExpression(&mut self, expression: Expression, ty: TypeNode) -> AsExpression {
        let node = AsExpression {
            node_id: self.node_id_gen.next(),
            expression,
            ty,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(Some(&node.ty))
            | TransformFlags::ContainsTypeScript;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateAsExpression(node: AsExpression, expression: Expression, type: TypeNode) {
    //         return node.expression !== expression
    //             || node.type !== type
    //             ? update(createAsExpression(expression, type), node)
    //             : node;
    //     }

    pub fn createNonNullExpression(
        &mut self,
        expression: LeftHandSideExpression,
    ) -> NonNullExpression {
        let node = NonNullExpression {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression:parenthesizerRules().parenthesizeLeftSideOfAccess(expression),
            expression,
        };
        let transform_flags =
            self.propagateChildFlags(Some(&node.expression)) | TransformFlags::ContainsTypeScript;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateNonNullExpression(node: NonNullExpression, expression: Expression) {
    //         if (isNonNullChain(node)) {
    //             return updateNonNullChain(node, expression);
    //         }
    //         return node.expression !== expression
    //             ? update(createNonNullExpression(expression), node)
    //             : node;
    //     }

    //     // @api
    //     function createNonNullChain(expression: Expression) {
    //         const node = createBaseExpression<NonNullChain>(SyntaxKind::NonNullExpression);
    //         node.flags |= NodeFlags.OptionalChain;
    //         node.expression = parenthesizerRules().parenthesizeLeftSideOfAccess(expression);
    //         node.transformFlags |=
    //             propagateChildFlags(node.expression) |
    //             TransformFlags::ContainsTypeScript;
    //         return node;
    //     }

    //     // @api
    //     function updateNonNullChain(node: NonNullChain, expression: Expression) {
    //         Debug.assert(!!(node.flags & NodeFlags.OptionalChain), "Cannot update a NonNullExpression using updateNonNullChain. Use updateNonNullExpression instead.");
    //         return node.expression !== expression
    //             ? update(createNonNullChain(expression), node)
    //             : node;
    //     }

    pub fn createMetaProperty(
        &mut self,
        keywordToken: SyntaxKind,
        name: Rc<Identifier>,
    ) -> MetaProperty {
        let node = MetaProperty {
            node_id: self.node_id_gen.next(),
            keywordToken,
            name,
        };
        let mut transform_flags = self.propagateChildFlags(Some(&node.name));
        match keywordToken {
            SyntaxKind::NewKeyword => {
                transform_flags |= TransformFlags::ContainsES2015;
            }
            SyntaxKind::ImportKeyword => {
                transform_flags |= TransformFlags::ContainsESNext;
            }
            _ => unreachable!(),
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateMetaProperty(node: MetaProperty, name: Identifier) {
    //         return node.name !== name
    //             ? update(createMetaProperty(node.keywordToken, name), node)
    //             : node;
    //     }

    //     //
    //     // Misc
    //     //

    pub fn createTemplateSpan(
        &mut self,
        expression: Expression,
        literal: TemplateSpanLiteral,
    ) -> TemplateSpan {
        let node = TemplateSpan {
            node_id: self.node_id_gen.next(),
            expression,
            literal,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(Some(&node.literal))
            | TransformFlags::ContainsES2015;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateTemplateSpan(node: TemplateSpan, expression: Expression, literal: TemplateMiddle | TemplateTail) {
    //         return node.expression !== expression
    //             || node.literal !== literal
    //             ? update(createTemplateSpan(expression, literal), node)
    //             : node;
    //     }

    pub fn createSemicolonClassElement(&mut self) -> SemicolonClassElement {
        let node = SemicolonClassElement {
            node_id: self.node_id_gen.next(),
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsES2015;
        node
    }

    //     //
    //     // Element
    //     //

    pub fn createBlock(
        &mut self,
        statements: NodeArray<Statement>,
        multiLine: Option<bool>,
    ) -> Block {
        let node = Block {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            statements: self.updateNodeArray(Some(statements), None),
            multiLine,
        };
        self.node_data_mut(&node).transformFlags |= propagateChildrenFlags(Some(&node.statements));
        node
    }

    //     // @api
    //     function updateBlock(node: Block, statements: readonly Statement[]) {
    //         return node.statements !== statements
    //             ? update(createBlock(statements, node.multiLine), node)
    //             : node;
    //     }

    pub fn createVariableStatement(
        &mut self,
        modifiers: Option<NodeArray<Modifier>>,
        declarationList: Rc<VariableDeclarationList>,
    ) -> VariableStatement {
        let node = VariableStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: None,
            // TODO:
            // declarationList: isArray(declarationList) ? createVariableDeclarationList(declarationList) : declarationList,
            declarationList,
            modifiers: modifiers.map(|m| self.updateNodeArray(Some(m), None)),
        };
        let mut transform_flags = propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(Some(&node.declarationList));
        if modifiersToFlags(node.modifiers.as_ref()).intersects(ModifierFlags::Ambient) {
            transform_flags = TransformFlags::ContainsTypeScript;
        }
        self.node_data_mut(&node).transformFlags = transform_flags;
        node
    }

    //     // @api
    //     function updateVariableStatement(node: VariableStatement, modifiers: readonly Modifier[] | undefined, declarationList: VariableDeclarationList) {
    //         return node.modifiers !== modifiers
    //             || node.declarationList !== declarationList
    //             ? update(createVariableStatement(modifiers, declarationList), node)
    //             : node;
    //     }

    pub fn createEmptyStatement(&mut self) -> EmptyStatement {
        EmptyStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
        }
    }

    pub fn createExpressionStatement(&mut self, expression: Expression) -> ExpressionStatement {
        let node = ExpressionStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeExpressionOfExpressionStatement(expression),
            expression,
        };
        let child_flags = self.propagateChildFlags(Some(&node.expression));
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateExpressionStatement(node: ExpressionStatement, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createExpressionStatement(expression), node)
    //             : node;
    //     }

    pub fn createIfStatement(
        &mut self,
        expression: Expression,
        thenStatement: Statement,
        elseStatement: Option<Statement>,
    ) -> IfStatement {
        let node = IfStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            expression,
            thenStatement: self.asEmbeddedStatement(thenStatement),
            elseStatement: self.asEmbeddedStatementOptional(elseStatement),
        };
        let child_flags = self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(Some(&node.thenStatement))
            | self.propagateChildFlags(node.elseStatement.as_ref());
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateIfStatement(node: IfStatement, expression: Expression, thenStatement: Statement, elseStatement: Statement | undefined) {
    //         return node.expression !== expression
    //             || node.thenStatement !== thenStatement
    //             || node.elseStatement !== elseStatement
    //             ? update(createIfStatement(expression, thenStatement, elseStatement), node)
    //             : node;
    //     }

    pub fn createDoStatement(
        &mut self,
        statement: Statement,
        expression: Expression,
    ) -> DoStatement {
        let node = DoStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            statement: self.asEmbeddedStatement(statement),
            expression,
        };
        let child_flags = self.propagateChildFlags(Some(&node.statement))
            | self.propagateChildFlags(Some(&node.expression));
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateDoStatement(node: DoStatement, statement: Statement, expression: Expression) {
    //         return node.statement !== statement
    //             || node.expression !== expression
    //             ? update(createDoStatement(statement, expression), node)
    //             : node;
    //     }

    pub fn createWhileStatement(
        &mut self,
        expression: Expression,
        statement: Statement,
    ) -> WhileStatement {
        let node = WhileStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            statement: self.asEmbeddedStatement(statement),
            expression,
        };
        let child_flags = self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(Some(&node.statement));
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateWhileStatement(node: WhileStatement, expression: Expression, statement: Statement) {
    //         return node.expression !== expression
    //             || node.statement !== statement
    //             ? update(createWhileStatement(expression, statement), node)
    //             : node;
    //     }

    pub fn createForStatement(
        &mut self,
        initializer: Option<ForInitializer>,
        condition: Option<Expression>,
        incrementor: Option<Expression>,
        statement: Statement,
    ) -> ForStatement {
        let node = ForStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            statement: self.asEmbeddedStatement(statement),
            initializer,
            condition,
            incrementor,
        };
        let child_flags = self.propagateChildFlags(node.initializer.as_ref())
            | self.propagateChildFlags(node.condition.as_ref())
            | self.propagateChildFlags(node.incrementor.as_ref())
            | self.propagateChildFlags(Some(&node.statement));
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateForStatement(node: ForStatement, initializer: ForInitializer | undefined, condition: Expression | undefined, incrementor: Expression | undefined, statement: Statement) {
    //         return node.initializer !== initializer
    //             || node.condition !== condition
    //             || node.incrementor !== incrementor
    //             || node.statement !== statement
    //             ? update(createForStatement(initializer, condition, incrementor, statement), node)
    //             : node;
    //     }

    pub fn createForInStatement(
        &mut self,
        initializer: ForInitializer,
        expression: Expression,
        statement: Statement,
    ) -> ForInStatement {
        let node = ForInStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            statement: self.asEmbeddedStatement(statement),
            initializer,
            expression,
        };
        let child_flags = self.propagateChildFlags(Some(&node.initializer))
            | self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(Some(&node.statement));
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateForInStatement(node: ForInStatement, initializer: ForInitializer, expression: Expression, statement: Statement) {
    //         return node.initializer !== initializer
    //             || node.expression !== expression
    //             || node.statement !== statement
    //             ? update(createForInStatement(initializer, expression, statement), node)
    //             : node;
    //     }

    pub fn createForOfStatement(
        &mut self,
        awaitModifier: Option<Rc<AwaitKeyword>>,
        initializer: ForInitializer,
        expression: Expression,
        statement: Statement,
    ) -> ForOfStatement {
        let node = ForOfStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            statement: self.asEmbeddedStatement(statement),
            awaitModifier,
            initializer,
            // TODO:
            // expression: parenthesizerRules().parenthesizeExpressionForDisallowedComma(expression),
            expression,
        };
        let mut new_flags = self.propagateChildFlags(node.awaitModifier.as_ref())
            | self.propagateChildFlags(Some(&node.initializer))
            | self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(Some(&node.statement))
            | TransformFlags::ContainsES2015;
        if node.awaitModifier.is_some() {
            new_flags |= TransformFlags::ContainsES2018;
        }
        self.node_data_mut(&node).transformFlags |= new_flags;

        node
    }

    //     // @api
    //     function updateForOfStatement(node: ForOfStatement, awaitModifier: AwaitKeyword | undefined, initializer: ForInitializer, expression: Expression, statement: Statement) {
    //         return node.awaitModifier !== awaitModifier
    //             || node.initializer !== initializer
    //             || node.expression !== expression
    //             || node.statement !== statement
    //             ? update(createForOfStatement(awaitModifier, initializer, expression, statement), node)
    //             : node;
    //     }

    pub fn createContinueStatement(&mut self, label: Option<Rc<Identifier>>) -> ContinueStatement {
        let node = ContinueStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            label,
        };
        let child_flags = self.propagateChildFlags(node.label.as_ref())
            | TransformFlags::ContainsHoistedDeclarationOrCompletion;
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateContinueStatement(node: ContinueStatement, label: Identifier | undefined) {
    //         return node.label !== label
    //             ? update(createContinueStatement(label), node)
    //             : node;
    //     }

    pub fn createBreakStatement(&mut self, label: Option<Rc<Identifier>>) -> BreakStatement {
        let node = BreakStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            label,
        };
        let child_flags = self.propagateChildFlags(node.label.as_ref())
            | TransformFlags::ContainsHoistedDeclarationOrCompletion;
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateBreakStatement(node: BreakStatement, label: Identifier | undefined) {
    //         return node.label !== label
    //             ? update(createBreakStatement(label), node)
    //             : node;
    //     }

    pub fn createReturnStatement(&mut self, expression: Option<Expression>) -> ReturnStatement {
        let node = ReturnStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            expression,
        };
        // return in an ES2018 async generator must be awaited
        let child_flags = self.propagateChildFlags(node.expression.as_ref())
            | TransformFlags::ContainsES2018
            | TransformFlags::ContainsHoistedDeclarationOrCompletion;
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateReturnStatement(node: ReturnStatement, expression: Expression | undefined) {
    //         return node.expression !== expression
    //             ? update(createReturnStatement(expression), node)
    //             : node;
    //     }

    pub fn createWithStatement(
        &mut self,
        expression: Expression,
        statement: Statement,
    ) -> WithStatement {
        let node = WithStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            expression,
            statement: self.asEmbeddedStatement(statement),
        };
        let child_flags = self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(Some(&node.statement));
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateWithStatement(node: WithStatement, expression: Expression, statement: Statement) {
    //         return node.expression !== expression
    //             || node.statement !== statement
    //             ? update(createWithStatement(expression, statement), node)
    //             : node;
    //     }

    pub fn createSwitchStatement(
        &mut self,
        expression: Expression,
        caseBlock: Rc<CaseBlock>,
    ) -> SwitchStatement {
        let node = SwitchStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeExpressionForDisallowedComma(expression),
            expression,
            caseBlock,
        };
        let child_flags = self.propagateChildFlags(Some(&node.expression))
            | self.propagateChildFlags(Some(&node.caseBlock));
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateSwitchStatement(node: SwitchStatement, expression: Expression, caseBlock: CaseBlock) {
    //         return node.expression !== expression
    //             || node.caseBlock !== caseBlock
    //             ? update(createSwitchStatement(expression, caseBlock), node)
    //             : node;
    //     }

    pub fn createLabeledStatement(
        &mut self,
        label: Rc<Identifier>,
        statement: Statement,
    ) -> LabeledStatement {
        let node = LabeledStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            label,
            // TODO:
            // statement: asEmbeddedStatement(statement),
            statement,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.label))
            | self.propagateChildFlags(Some(&node.statement));
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateLabeledStatement(node: LabeledStatement, label: Identifier, statement: Statement) {
    //         return node.label !== label
    //             || node.statement !== statement
    //             ? update(createLabeledStatement(label, statement), node)
    //             : node;
    //     }

    pub fn createThrowStatement(&mut self, expression: Expression) -> ThrowStatement {
        let node = ThrowStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            expression,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.expression));
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateThrowStatement(node: ThrowStatement, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createThrowStatement(expression), node)
    //             : node;
    //     }

    pub fn createTryStatement(
        &mut self,
        tryBlock: Rc<Block>,
        catchClause: Option<Rc<CatchClause>>,
        finallyBlock: Option<Rc<Block>>,
    ) -> TryStatement {
        let node = TryStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            tryBlock,
            catchClause,
            finallyBlock,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.tryBlock))
            | self.propagateChildFlags(node.catchClause.as_ref())
            | self.propagateChildFlags(node.finallyBlock.as_ref());
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateTryStatement(node: TryStatement, tryBlock: Block, catchClause: CatchClause | undefined, finallyBlock: Block | undefined) {
    //         return node.tryBlock !== tryBlock
    //             || node.catchClause !== catchClause
    //             || node.finallyBlock !== finallyBlock
    //             ? update(createTryStatement(tryBlock, catchClause, finallyBlock), node)
    //             : node;
    //     }

    pub fn createDebuggerStatement(&mut self) -> DebuggerStatement {
        DebuggerStatement {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
        }
    }

    pub fn createVariableDeclaration(
        &mut self,
        name: BindingName,
        exclamationToken: Option<Rc<ExclamationToken>>,
        ty: Option<TypeNode>,
        initializer: Option<Expression>,
    ) -> VariableDeclaration {
        ////////////////////////////////////// Inlined createBaseVariableLikeDeclaration
        let node = VariableDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            name,
            exclamationToken,
            ty,
            // TODO:
            // initializer: initializer.map(|i|parenthesizerRules().parenthesizeExpressionForDisallowedComma(i)),
            initializer,
        };
        let mut transform_flags = self.propagateChildFlags(Some(&node.name))
            | self.propagateChildFlags(node.initializer.as_ref())
            | self.propagateChildFlags(node.ty.as_ref())
            | self.propagateChildFlags(node.exclamationToken.as_ref());

        if node.ty.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }

        //////////////////////////////////////

        if node.exclamationToken.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateVariableDeclaration(node: VariableDeclaration, name: BindingName, exclamationToken: ExclamationToken | undefined, type: TypeNode | undefined, initializer: Expression | undefined) {
    //         return node.name !== name
    //             || node.type !== type
    //             || node.exclamationToken !== exclamationToken
    //             || node.initializer !== initializer
    //             ? update(createVariableDeclaration(name, exclamationToken, type, initializer), node)
    //             : node;
    //     }

    pub fn createVariableDeclarationList(
        &mut self,
        declarations: NodeArray<Rc<VariableDeclaration>>,
        flags: Option<NodeFlags>,
    ) -> VariableDeclarationList {
        let flags = flags.unwrap_or_default();
        let node = VariableDeclarationList {
            node_id: self.node_id_gen.next(),
            declarations: self.updateNodeArray(Some(declarations), None),
        };
        self.node_data_mut(&node).flags |= flags & NodeFlags::BlockScoped;
        let mut transform_flags = propagateChildrenFlags(Some(&node.declarations))
            | TransformFlags::ContainsHoistedDeclarationOrCompletion;
        if flags.intersects(NodeFlags::BlockScoped) {
            transform_flags |=
                TransformFlags::ContainsES2015 | TransformFlags::ContainsBlockScopedBinding;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateVariableDeclarationList(node: VariableDeclarationList, declarations: readonly VariableDeclaration[]) {
    //         return node.declarations !== declarations
    //             ? update(createVariableDeclarationList(declarations, node.flags), node)
    //             : node;
    //     }

    pub fn createFunctionDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        asteriskToken: Option<Rc<AsteriskToken>>,
        name: Option<Rc<Identifier>>,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        ty: Option<TypeNode>,
        body: Option<Rc<Block>>,
    ) -> FunctionDeclaration {
        let node = FunctionDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            typeParameters: self.asNodeArray(typeParameters),
            parameters: self.updateNodeArray(Some(parameters), None),
            ty,
            typeArguments: None,
            asteriskToken,
            name,
            body,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(node.name.as_ref())
            | propagateChildrenFlags(node.typeParameters.as_ref())
            | propagateChildrenFlags(Some(&node.parameters))
            | self.propagateChildFlags(node.ty.as_ref())
            | (self.propagateChildFlags(node.body.as_ref())
                & !TransformFlags::ContainsPossibleTopLevelAwait);

        if node.typeParameters.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        if node.ty.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        if node.body.is_none() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        if node.body.is_none()
            || modifiersToFlags(node.modifiers.as_ref()).intersects(ModifierFlags::Ambient)
        {
            transform_flags = TransformFlags::ContainsTypeScript;
        } else {
            transform_flags |= self.propagateChildFlags(node.asteriskToken.as_ref())
                | TransformFlags::ContainsHoistedDeclarationOrCompletion;
            if modifiersToFlags(node.modifiers.as_ref()).intersects(ModifierFlags::Async) {
                if node.asteriskToken.is_some() {
                    transform_flags |= TransformFlags::ContainsES2018;
                } else {
                    transform_flags |= TransformFlags::ContainsES2017;
                }
            } else if node.asteriskToken.is_some() {
                transform_flags |= TransformFlags::ContainsGenerator;
            }
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateFunctionDeclaration(
    //         node: FunctionDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         asteriskToken: AsteriskToken | undefined,
    //         name: Identifier | undefined,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         parameters: readonly ParameterDeclaration[],
    //         type: TypeNode | undefined,
    //         body: Block | undefined
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.asteriskToken !== asteriskToken
    //             || node.name !== name
    //             || node.typeParameters !== typeParameters
    //             || node.parameters !== parameters
    //             || node.type !== type
    //             || node.body !== body
    //             ? updateBaseFunctionLikeDeclaration(createFunctionDeclaration(decorators, modifiers, asteriskToken, name, typeParameters, parameters, type, body), node)
    //             : node;
    //     }

    pub fn createClassDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        name: Option<Rc<Identifier>>,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        heritageClauses: Option<NodeArray<Rc<HeritageClause>>>,
        members: NodeArray<ClassElement>,
    ) -> ClassDeclaration {
        let node = ClassDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            typeParameters: self.asNodeArray(typeParameters),
            heritageClauses: self.asNodeArray(heritageClauses),
            members: self.updateNodeArray(Some(members), None),
            name,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(node.name.as_ref())
            | propagateChildrenFlags(node.typeParameters.as_ref())
            | propagateChildrenFlags(node.heritageClauses.as_ref())
            | propagateChildrenFlags(Some(&node.members));

        if node.typeParameters.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }

        if modifiersToFlags(node.modifiers.as_ref()).intersects(ModifierFlags::Ambient) {
            transform_flags = TransformFlags::ContainsTypeScript;
        } else {
            transform_flags |= TransformFlags::ContainsES2015;
            if transform_flags.intersects(TransformFlags::ContainsTypeScriptClassSyntax) {
                transform_flags |= TransformFlags::ContainsTypeScript;
            }
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateClassDeclaration(
    //         node: ClassDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: Identifier | undefined,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         heritageClauses: readonly HeritageClause[] | undefined,
    //         members: readonly ClassElement[]
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.name !== name
    //             || node.typeParameters !== typeParameters
    //             || node.heritageClauses !== heritageClauses
    //             || node.members !== members
    //             ? update(createClassDeclaration(decorators, modifiers, name, typeParameters, heritageClauses, members), node)
    //             : node;
    //     }

    pub fn createInterfaceDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        name: Rc<Identifier>,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        heritageClauses: Option<NodeArray<Rc<HeritageClause>>>,
        members: NodeArray<TypeElement>,
    ) -> InterfaceDeclaration {
        let node = InterfaceDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            name,
            typeParameters: self.asNodeArray(typeParameters),
            heritageClauses: self.asNodeArray(heritageClauses),
            members: self.updateNodeArray(Some(members), None),
        };
        let transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(Some(&node.name))
            | propagateChildrenFlags(node.typeParameters.as_ref())
            | propagateChildrenFlags(node.heritageClauses.as_ref())
            | TransformFlags::ContainsTypeScript;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateInterfaceDeclaration(
    //         node: InterfaceDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: Identifier,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         heritageClauses: readonly HeritageClause[] | undefined,
    //         members: readonly TypeElement[]
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.name !== name
    //             || node.typeParameters !== typeParameters
    //             || node.heritageClauses !== heritageClauses
    //             || node.members !== members
    //             ? update(createInterfaceDeclaration(decorators, modifiers, name, typeParameters, heritageClauses, members), node)
    //             : node;
    //     }

    pub fn createTypeAliasDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        name: Rc<Identifier>,
        typeParameters: Option<NodeArray<Rc<TypeParameterDeclaration>>>,
        ty: TypeNode,
    ) -> TypeAliasDeclaration {
        let node = TypeAliasDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            name,
            typeParameters: self.asNodeArray(typeParameters),
            ty,
        };
        let transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(Some(&node.name))
            | propagateChildrenFlags(node.typeParameters.as_ref())
            | TransformFlags::ContainsTypeScript;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateTypeAliasDeclaration(
    //         node: TypeAliasDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: Identifier,
    //         typeParameters: readonly TypeParameterDeclaration[] | undefined,
    //         type: TypeNode
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.name !== name
    //             || node.typeParameters !== typeParameters
    //             || node.type !== type
    //             ? update(createTypeAliasDeclaration(decorators, modifiers, name, typeParameters, type), node)
    //             : node;
    //     }

    pub fn createEnumDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        name: Rc<Identifier>,
        members: NodeArray<Rc<EnumMember>>,
    ) -> EnumDeclaration {
        let node = EnumDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            name,
            members: self.updateNodeArray(Some(members), None),
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(Some(&node.name))
            | propagateChildrenFlags(Some(&node.members))
            | TransformFlags::ContainsTypeScript;
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // Enum declarations cannot contain `await`
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateEnumDeclaration(
    //         node: EnumDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: Identifier,
    //         members: readonly EnumMember[]) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.name !== name
    //             || node.members !== members
    //             ? update(createEnumDeclaration(decorators, modifiers, name, members), node)
    //             : node;
    //     }

    pub fn createModuleDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        name: ModuleName,
        // TODO:
        // body: Option<ModuleBody>,
        body: Option<Rc<ModuleBlock>>,
        flags: Option<NodeFlags>,
    ) -> ModuleDeclaration {
        let node = ModuleDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            name,
            body,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref());
        self.node_data_mut(&node).flags |= flags.unwrap_or_default()
            & (NodeFlags::Namespace | NodeFlags::NestedNamespace | NodeFlags::GlobalAugmentation);
        if modifiersToFlags(node.modifiers.as_ref()).intersects(ModifierFlags::Ambient) {
            transform_flags = TransformFlags::ContainsTypeScript;
        } else {
            transform_flags |= self.propagateChildFlags(Some(&node.name))
                | self.propagateChildFlags(node.body.as_ref())
                | TransformFlags::ContainsTypeScript;
        }
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // Module declarations cannot contain `await`.
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateModuleDeclaration(
    //         node: ModuleDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         name: ModuleName,
    //         body: ModuleBody | undefined
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.name !== name
    //             || node.body !== body
    //             ? update(createModuleDeclaration(decorators, modifiers, name, body, node.flags), node)
    //             : node;
    //     }

    pub fn createModuleBlock(&mut self, statements: NodeArray<Statement>) -> ModuleBlock {
        let node = ModuleBlock {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            statements: self.updateNodeArray(Some(statements), None),
        };
        self.node_data_mut(&node).transformFlags |= propagateChildrenFlags(Some(&node.statements));
        node
    }

    //     // @api
    //     function updateModuleBlock(node: ModuleBlock, statements: readonly Statement[]) {
    //         return node.statements !== statements
    //             ? update(createModuleBlock(statements), node)
    //             : node;
    //     }

    pub fn createCaseBlock(&mut self, clauses: NodeArray<CaseOrDefaultClause>) -> CaseBlock {
        let node = CaseBlock {
            node_id: self.node_id_gen.next(),
            clauses: self.updateNodeArray(Some(clauses), None),
        };
        self.node_data_mut(&node).transformFlags |= propagateChildrenFlags(Some(&node.clauses));
        node
    }

    //     // @api
    //     function updateCaseBlock(node: CaseBlock, clauses: readonly CaseOrDefaultClause[]) {
    //         return node.clauses !== clauses
    //             ? update(createCaseBlock(clauses), node)
    //             : node;
    //     }

    pub fn createNamespaceExportDeclaration(
        &mut self,
        name: Rc<Identifier>,
    ) -> NamespaceExportDeclaration {
        let node = NamespaceExportDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            name,
            decorators: None,
            modifiers: None,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.name));
        // Re-assignment looks like a bug; matches TSC.
        let transform_flags = TransformFlags::ContainsTypeScript;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateNamespaceExportDeclaration(node: NamespaceExportDeclaration, name: Identifier) {
    //         return node.name !== name
    //             ? update(createNamespaceExportDeclaration(name), node)
    //             : node;
    //     }

    pub fn createImportEqualsDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        isTypeOnly: bool,
        name: Rc<Identifier>,
        moduleReference: ModuleReference,
    ) -> ImportEqualsDeclaration {
        let node = ImportEqualsDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            name,
            isTypeOnly,
            moduleReference,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(Some(&node.name))
            | self.propagateChildFlags(Some(&node.moduleReference));
        if !matches!(
            node.moduleReference,
            ModuleReference::ExternalModuleReference(_)
        ) {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // Import= declaration is always parsed in an Await context
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateImportEqualsDeclaration(
    //         node: ImportEqualsDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         isTypeOnly: boolean,
    //         name: Identifier,
    //         moduleReference: ModuleReference
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.isTypeOnly !== isTypeOnly
    //             || node.name !== name
    //             || node.moduleReference !== moduleReference
    //             ? update(createImportEqualsDeclaration(decorators, modifiers, isTypeOnly, name, moduleReference), node)
    //             : node;
    //     }

    pub fn createImportDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        importClause: Option<Rc<ImportClause>>,
        moduleSpecifier: Expression,
        assertClause: Option<Rc<AssertClause>>,
    ) -> ImportDeclaration {
        let node = ImportDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            importClause,
            moduleSpecifier,
            assertClause,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(node.importClause.as_ref())
            | self.propagateChildFlags(Some(&node.moduleSpecifier));
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // always parsed in an Await context
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateImportDeclaration(
    //         node: ImportDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         importClause: ImportClause | undefined,
    //         moduleSpecifier: Expression,
    //         assertClause: AssertClause | undefined
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.importClause !== importClause
    //             || node.moduleSpecifier !== moduleSpecifier
    //             || node.assertClause !== assertClause
    //             ? update(createImportDeclaration(decorators, modifiers, importClause, moduleSpecifier, assertClause), node)
    //             : node;
    //     }

    pub fn createImportClause(
        &mut self,
        isTypeOnly: bool,
        name: Option<Rc<Identifier>>,
        namedBindings: Option<NamedImportBindings>,
    ) -> ImportClause {
        let node = ImportClause {
            node_id: self.node_id_gen.next(),
            isTypeOnly,
            name,
            namedBindings,
        };
        let mut transform_flags = self.propagateChildFlags(node.name.as_ref())
            | self.propagateChildFlags(node.namedBindings.as_ref());
        if isTypeOnly {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // always parsed in an Await context
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateImportClause(node: ImportClause, isTypeOnly: boolean, name: Identifier | undefined, namedBindings: NamedImportBindings | undefined) {
    //         return node.isTypeOnly !== isTypeOnly
    //             || node.name !== name
    //             || node.namedBindings !== namedBindings
    //             ? update(createImportClause(isTypeOnly, name, namedBindings), node)
    //             : node;
    //     }

    pub fn createAssertClause(
        &mut self,
        elements: NodeArray<Rc<AssertEntry>>,
        multiLine: Option<bool>,
    ) -> AssertClause {
        let node = AssertClause {
            node_id: self.node_id_gen.next(),
            elements,
            multiLine,
        };

        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsESNext;
        node
    }

    //     // @api
    //     function updateAssertClause(node: AssertClause, elements: NodeArray<AssertEntry>, multiLine?: boolean): AssertClause {
    //         return node.elements !== elements
    //             || node.multiLine !== multiLine
    //             ? update(createAssertClause(elements, multiLine), node)
    //             : node;
    //     }

    pub fn createAssertEntry(
        &mut self,
        name: AssertionKey,
        value: Rc<StringLiteral>,
    ) -> AssertEntry {
        let node = AssertEntry {
            node_id: self.node_id_gen.next(),
            name,
            value,
        };
        self.node_data_mut(&node).transformFlags |= TransformFlags::ContainsESNext;
        node
    }

    //     // @api
    //     function updateAssertEntry(node: AssertEntry, name: AssertionKey, value: StringLiteral): AssertEntry {
    //         return node.name !== name
    //             || node.value !== value
    //             ? update(createAssertEntry(name, value), node)
    //             : node;
    //     }

    pub fn createNamespaceImport(&mut self, name: Rc<Identifier>) -> NamespaceImport {
        let node = NamespaceImport {
            node_id: self.node_id_gen.next(),
            name,
        };
        let mut transform_flags = self.propagateChildFlags(Some(&node.name));
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // always parsed in an Await context
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateNamespaceImport(node: NamespaceImport, name: Identifier) {
    //         return node.name !== name
    //             ? update(createNamespaceImport(name), node)
    //             : node;
    //     }

    pub fn createNamespaceExport(&mut self, name: Rc<Identifier>) -> NamespaceExport {
        let node = NamespaceExport {
            node_id: self.node_id_gen.next(),
            name,
        };
        let mut transform_flags =
            self.propagateChildFlags(Some(&node.name)) | TransformFlags::ContainsESNext;
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // always parsed in an Await context
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateNamespaceExport(node: NamespaceExport, name: Identifier) {
    //         return node.name !== name
    //             ? update(createNamespaceExport(name), node)
    //             : node;
    //     }

    pub fn createNamedImports(&mut self, elements: NodeArray<Rc<ImportSpecifier>>) -> NamedImports {
        let node = NamedImports {
            node_id: self.node_id_gen.next(),
            elements: self.updateNodeArray(Some(elements), None),
        };
        let mut transform_flags = propagateChildrenFlags(Some(&node.elements));
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // always parsed in an Await context
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateNamedImports(node: NamedImports, elements: readonly ImportSpecifier[]) {
    //         return node.elements !== elements
    //             ? update(createNamedImports(elements), node)
    //             : node;
    //     }

    pub fn createImportSpecifier(
        &mut self,
        isTypeOnly: bool,
        propertyName: Option<Rc<Identifier>>,
        name: Rc<Identifier>,
    ) -> ImportSpecifier {
        let node = ImportSpecifier {
            node_id: self.node_id_gen.next(),
            isTypeOnly,
            propertyName,
            name,
        };
        let mut transform_flags = self.propagateChildFlags(node.propertyName.as_ref())
            | self.propagateChildFlags(Some(&node.name));
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // always parsed in an Await context
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateImportSpecifier(node: ImportSpecifier, isTypeOnly: boolean, propertyName: Identifier | undefined, name: Identifier) {
    //         return node.isTypeOnly !== isTypeOnly
    //             || node.propertyName !== propertyName
    //             || node.name !== name
    //             ? update(createImportSpecifier(isTypeOnly, propertyName, name), node)
    //             : node;
    //     }

    pub fn createExportAssignment(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        isExportEquals: Option<bool>,
        expression: Expression,
    ) -> ExportAssignment {
        let expression = if isExportEquals.unwrap_or_default() {
            // TODO:
            // parenthesizerRules().parenthesizeRightSideOfBinary(
            //     SyntaxKind::EqualsToken,
            //     /*leftSide*/ undefined,
            //     expression,
            // )
            expression
        } else {
            // TODO:
            // parenthesizerRules().parenthesizeExpressionOfExportDefault(expression)
            expression
        };
        let node = ExportAssignment {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            isExportEquals,
            expression,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(Some(&node.expression));
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // always parsed in an Await context
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateExportAssignment(
    //         node: ExportAssignment,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         expression: Expression
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.expression !== expression
    //             ? update(createExportAssignment(decorators, modifiers, node.isExportEquals, expression), node)
    //             : node;
    //     }

    pub fn createExportDeclaration(
        &mut self,
        decorators: Option<NodeArray<Rc<Decorator>>>,
        modifiers: Option<NodeArray<Modifier>>,
        isTypeOnly: bool,
        exportClause: Option<NamedExportBindings>,
        moduleSpecifier: Option<Expression>,
        assertClause: Option<Rc<AssertClause>>,
    ) -> ExportDeclaration {
        let node = ExportDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: self.asNodeArray(decorators),
            modifiers: self.asNodeArray(modifiers),
            isTypeOnly,
            exportClause,
            moduleSpecifier,
            assertClause,
        };
        let mut transform_flags = propagateChildrenFlags(node.decorators.as_ref())
            | propagateChildrenFlags(node.modifiers.as_ref())
            | self.propagateChildFlags(node.exportClause.as_ref())
            | self.propagateChildFlags(node.moduleSpecifier.as_ref());
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // always parsed in an Await context
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateExportDeclaration(
    //         node: ExportDeclaration,
    //         decorators: readonly Decorator[] | undefined,
    //         modifiers: readonly Modifier[] | undefined,
    //         isTypeOnly: boolean,
    //         exportClause: NamedExportBindings | undefined,
    //         moduleSpecifier: Expression | undefined,
    //         assertClause: AssertClause | undefined
    //     ) {
    //         return node.decorators !== decorators
    //             || node.modifiers !== modifiers
    //             || node.isTypeOnly !== isTypeOnly
    //             || node.exportClause !== exportClause
    //             || node.moduleSpecifier !== moduleSpecifier
    //             || node.assertClause !== assertClause
    //             ? update(createExportDeclaration(decorators, modifiers, isTypeOnly, exportClause, moduleSpecifier, assertClause), node)
    //             : node;
    //     }

    pub fn createNamedExports(&mut self, elements: NodeArray<Rc<ExportSpecifier>>) -> NamedExports {
        let node = NamedExports {
            node_id: self.node_id_gen.next(),
            elements: self.updateNodeArray(Some(elements), None),
        };
        let mut transform_flags = propagateChildrenFlags(Some(&node.elements));
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // always parsed in an Await context
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateNamedExports(node: NamedExports, elements: readonly ExportSpecifier[]) {
    //         return node.elements !== elements
    //             ? update(createNamedExports(elements), node)
    //             : node;
    //     }

    pub fn createExportSpecifier(
        &mut self,
        isTypeOnly: bool,
        propertyName: Option<Rc<Identifier>>,
        name: Rc<Identifier>,
    ) -> ExportSpecifier {
        let node = ExportSpecifier {
            node_id: self.node_id_gen.next(),
            isTypeOnly,
            propertyName,
            name,
        };
        let mut transform_flags = self.propagateChildFlags(node.propertyName.as_ref())
            | self.propagateChildFlags(Some(&node.name));
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // always parsed in an Await context
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateExportSpecifier(node: ExportSpecifier, isTypeOnly: boolean, propertyName: Identifier | undefined, name: Identifier) {
    //         return node.isTypeOnly !== isTypeOnly
    //             || node.propertyName !== propertyName
    //             || node.name !== name
    //             ? update(createExportSpecifier(isTypeOnly, propertyName, name), node)
    //             : node;
    //     }

    pub fn createMissingDeclaration(&mut self) -> MissingDeclaration {
        MissingDeclaration {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: None,
            modifiers: None,
            name: None,
        }
    }

    //
    // Module references
    //

    pub fn createExternalModuleReference(
        &mut self,
        expression: Expression,
    ) -> ExternalModuleReference {
        let node = ExternalModuleReference {
            node_id: self.node_id_gen.next(),
            expression,
        };
        let mut transform_flags = self.propagateChildFlags(Some(&node.expression));
        transform_flags &= !TransformFlags::ContainsPossibleTopLevelAwait; // always parsed in an Await context
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateExternalModuleReference(node: ExternalModuleReference, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createExternalModuleReference(expression), node)
    //             : node;
    //     }

    //     //
    //     // JSDoc
    //     //

    pub fn createJSDocUnknownType(&mut self) -> JSDocUnknownType {
        JSDocUnknownType {
            node_id: self.node_id_gen.next(),
        }
    }

    pub fn createJSDocAllType(&mut self) -> JSDocAllType {
        JSDocAllType {
            node_id: self.node_id_gen.next(),
        }
    }

    //     // @api
    //     // createJSDocNullableType
    //     // createJSDocOptionalType
    //     // createJSDocVariadicType
    //     // createJSDocNamepathType

    //     function createJSDocUnaryTypeWorker<T extends JSDocType & { readonly type: TypeNode | undefined; }>(kind: T["kind"], type: T["type"]): T {
    //         const node = createBaseNode<T>(kind);
    //         node.type = type;
    //         return node;
    //     }

    pub fn createJSDocNullableType(&mut self, ty: TypeNode) -> JSDocNullableType {
        JSDocNullableType {
            node_id: self.node_id_gen.next(),
            ty,
        }
    }

    pub fn createJSDocNonNullableType(&mut self, ty: TypeNode) -> JSDocNonNullableType {
        JSDocNonNullableType {
            node_id: self.node_id_gen.next(),
            ty,
        }
    }

    //     // @api
    //     // updateJSDocNonNullableType
    //     // updateJSDocNullableType
    //     // updateJSDocOptionalType
    //     // updateJSDocVariadicType
    //     // updateJSDocNamepathType
    //     function updateJSDocUnaryTypeWorker<T extends JSDocType & { readonly type: TypeNode | undefined; }>(kind: T["kind"], node: T, type: T["type"]): T {
    //         return node.type !== type
    //             ? update(createJSDocUnaryTypeWorker(kind, type), node)
    //             : node;
    //     }

    pub fn createJSDocFunctionType(
        &mut self,
        parameters: NodeArray<Rc<ParameterDeclaration>>,
        ty: Option<TypeNode>,
    ) -> JSDocFunctionType {
        let node = JSDocFunctionType {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            parameters: self.updateNodeArray(Some(parameters), None),
            ty,
            typeArguments: None,
        };
        let mut transform_flags = propagateChildrenFlags(Some(&node.parameters))
            | self.propagateChildFlags(node.ty.as_ref());
        if node.ty.is_some() {
            transform_flags |= TransformFlags::ContainsTypeScript;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateJSDocFunctionType(node: JSDocFunctionType, parameters: readonly ParameterDeclaration[], type: TypeNode | undefined): JSDocFunctionType {
    //         return node.parameters !== parameters
    //             || node.type !== type
    //             ? update(createJSDocFunctionType(parameters, type), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocTypeLiteral(propertyTags?: readonly JSDocPropertyLikeTag[], isArrayType = false): JSDocTypeLiteral {
    //         const node = createBaseNode<JSDocTypeLiteral>(SyntaxKind::JSDocTypeLiteral);
    //         node.jsDocPropertyTags = asNodeArray(propertyTags);
    //         node.isArrayType = isArrayType;
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocTypeLiteral(node: JSDocTypeLiteral, propertyTags: readonly JSDocPropertyLikeTag[] | undefined, isArrayType: boolean): JSDocTypeLiteral {
    //         return node.jsDocPropertyTags !== propertyTags
    //             || node.isArrayType !== isArrayType
    //             ? update(createJSDocTypeLiteral(propertyTags, isArrayType), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocTypeExpression(type: TypeNode): JSDocTypeExpression {
    //         const node = createBaseNode<JSDocTypeExpression>(SyntaxKind::JSDocTypeExpression);
    //         node.type = type;
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocTypeExpression(node: JSDocTypeExpression, type: TypeNode): JSDocTypeExpression {
    //         return node.type !== type
    //             ? update(createJSDocTypeExpression(type), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocSignature(typeParameters: readonly JSDocTemplateTag[] | undefined, parameters: readonly JSDocParameterTag[], type?: JSDocReturnTag): JSDocSignature {
    //         const node = createBaseNode<JSDocSignature>(SyntaxKind::JSDocSignature);
    //         node.typeParameters = asNodeArray(typeParameters);
    //         node.parameters = createNodeArray(parameters);
    //         node.type = type;
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocSignature(node: JSDocSignature, typeParameters: readonly JSDocTemplateTag[] | undefined, parameters: readonly JSDocParameterTag[], type: JSDocReturnTag | undefined): JSDocSignature {
    //         return node.typeParameters !== typeParameters
    //             || node.parameters !== parameters
    //             || node.type !== type
    //             ? update(createJSDocSignature(typeParameters, parameters, type), node)
    //             : node;
    //     }

    //     function getDefaultTagName(node: JSDocTag) {
    //         const defaultTagName = getDefaultTagNameForKind(node.kind);
    //         return node.tagName.escapedText === escapeLeadingUnderscores(defaultTagName)
    //             ? node.tagName
    //             : createIdentifier(defaultTagName);
    //     }

    //     // @api
    //     function createBaseJSDocTag<T extends JSDocTag>(kind: T["kind"], tagName: Identifier, comment: string | NodeArray<JSDocComment> | undefined) {
    //         const node = createBaseNode<T>(kind);
    //         node.tagName = tagName;
    //         node.comment = comment;
    //         return node;
    //     }

    //     // @api
    //     function createJSDocTemplateTag(tagName: Identifier | undefined, constraint: JSDocTypeExpression | undefined, typeParameters: readonly TypeParameterDeclaration[], comment?: string | NodeArray<JSDocComment>): JSDocTemplateTag {
    //         const node = createBaseJSDocTag<JSDocTemplateTag>(SyntaxKind::JSDocTemplateTag, tagName ?? createIdentifier("template"), comment);
    //         node.constraint = constraint;
    //         node.typeParameters = createNodeArray(typeParameters);
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocTemplateTag(node: JSDocTemplateTag, tagName: Identifier = getDefaultTagName(node), constraint: JSDocTypeExpression | undefined, typeParameters: readonly TypeParameterDeclaration[], comment: string | NodeArray<JSDocComment> | undefined): JSDocTemplateTag {
    //         return node.tagName !== tagName
    //             || node.constraint !== constraint
    //             || node.typeParameters !== typeParameters
    //             || node.comment !== comment
    //             ? update(createJSDocTemplateTag(tagName, constraint, typeParameters, comment), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocTypedefTag(tagName: Identifier | undefined, typeExpression?: JSDocTypeExpression, fullName?: Identifier | JSDocNamespaceDeclaration, comment?: string | NodeArray<JSDocComment>): JSDocTypedefTag {
    //         const node = createBaseJSDocTag<JSDocTypedefTag>(SyntaxKind::JSDocTypedefTag, tagName ?? createIdentifier("typedef"), comment);
    //         node.typeExpression = typeExpression;
    //         node.fullName = fullName;
    //         node.name = getJSDocTypeAliasName(fullName);
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocTypedefTag(node: JSDocTypedefTag, tagName: Identifier = getDefaultTagName(node), typeExpression: JSDocTypeExpression | undefined, fullName: Identifier | JSDocNamespaceDeclaration | undefined, comment: string | NodeArray<JSDocComment> | undefined): JSDocTypedefTag {
    //         return node.tagName !== tagName
    //             || node.typeExpression !== typeExpression
    //             || node.fullName !== fullName
    //             || node.comment !== comment
    //             ? update(createJSDocTypedefTag(tagName, typeExpression, fullName, comment), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocParameterTag(tagName: Identifier | undefined, name: EntityName, isBracketed: boolean, typeExpression?: JSDocTypeExpression, isNameFirst?: boolean, comment?: string | NodeArray<JSDocComment>): JSDocParameterTag {
    //         const node = createBaseJSDocTag<JSDocParameterTag>(SyntaxKind::JSDocParameterTag, tagName ?? createIdentifier("param"), comment);
    //         node.typeExpression = typeExpression;
    //         node.name = name;
    //         node.isNameFirst = !!isNameFirst;
    //         node.isBracketed = isBracketed;
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocParameterTag(node: JSDocParameterTag, tagName: Identifier = getDefaultTagName(node), name: EntityName, isBracketed: boolean, typeExpression: JSDocTypeExpression | undefined, isNameFirst: boolean, comment: string | NodeArray<JSDocComment> | undefined): JSDocParameterTag {
    //         return node.tagName !== tagName
    //             || node.name !== name
    //             || node.isBracketed !== isBracketed
    //             || node.typeExpression !== typeExpression
    //             || node.isNameFirst !== isNameFirst
    //             || node.comment !== comment
    //             ? update(createJSDocParameterTag(tagName, name, isBracketed, typeExpression, isNameFirst, comment), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocPropertyTag(tagName: Identifier | undefined, name: EntityName, isBracketed: boolean, typeExpression?: JSDocTypeExpression, isNameFirst?: boolean, comment?: string | NodeArray<JSDocComment>): JSDocPropertyTag {
    //         const node = createBaseJSDocTag<JSDocPropertyTag>(SyntaxKind::JSDocPropertyTag, tagName ?? createIdentifier("prop"), comment);
    //         node.typeExpression = typeExpression;
    //         node.name = name;
    //         node.isNameFirst = !!isNameFirst;
    //         node.isBracketed = isBracketed;
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocPropertyTag(node: JSDocPropertyTag, tagName: Identifier = getDefaultTagName(node), name: EntityName, isBracketed: boolean, typeExpression: JSDocTypeExpression | undefined, isNameFirst: boolean, comment: string | NodeArray<JSDocComment> | undefined): JSDocPropertyTag {
    //         return node.tagName !== tagName
    //             || node.name !== name
    //             || node.isBracketed !== isBracketed
    //             || node.typeExpression !== typeExpression
    //             || node.isNameFirst !== isNameFirst
    //             || node.comment !== comment
    //             ? update(createJSDocPropertyTag(tagName, name, isBracketed, typeExpression, isNameFirst, comment), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocCallbackTag(tagName: Identifier | undefined, typeExpression: JSDocSignature, fullName?: Identifier | JSDocNamespaceDeclaration, comment?: string | NodeArray<JSDocComment>): JSDocCallbackTag {
    //         const node = createBaseJSDocTag<JSDocCallbackTag>(SyntaxKind::JSDocCallbackTag, tagName ?? createIdentifier("callback"), comment);
    //         node.typeExpression = typeExpression;
    //         node.fullName = fullName;
    //         node.name = getJSDocTypeAliasName(fullName);
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocCallbackTag(node: JSDocCallbackTag, tagName: Identifier = getDefaultTagName(node), typeExpression: JSDocSignature, fullName: Identifier | JSDocNamespaceDeclaration | undefined, comment: string | NodeArray<JSDocComment> | undefined): JSDocCallbackTag {
    //         return node.tagName !== tagName
    //             || node.typeExpression !== typeExpression
    //             || node.fullName !== fullName
    //             || node.comment !== comment
    //             ? update(createJSDocCallbackTag(tagName, typeExpression, fullName, comment), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocAugmentsTag(tagName: Identifier | undefined, className: JSDocAugmentsTag["class"], comment?: string | NodeArray<JSDocComment>): JSDocAugmentsTag {
    //         const node = createBaseJSDocTag<JSDocAugmentsTag>(SyntaxKind::JSDocAugmentsTag, tagName ?? createIdentifier("augments"), comment);
    //         node.class = className;
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocAugmentsTag(node: JSDocAugmentsTag, tagName: Identifier = getDefaultTagName(node), className: JSDocAugmentsTag["class"], comment: string | NodeArray<JSDocComment> | undefined): JSDocAugmentsTag {
    //         return node.tagName !== tagName
    //             || node.class !== className
    //             || node.comment !== comment
    //             ? update(createJSDocAugmentsTag(tagName, className, comment), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocImplementsTag(tagName: Identifier | undefined, className: JSDocImplementsTag["class"], comment?: string | NodeArray<JSDocComment>): JSDocImplementsTag {
    //         const node = createBaseJSDocTag<JSDocImplementsTag>(SyntaxKind::JSDocImplementsTag, tagName ?? createIdentifier("implements"), comment);
    //         node.class = className;
    //         return node;
    //     }

    //     // @api
    //     function createJSDocSeeTag(tagName: Identifier | undefined, name: JSDocNameReference | undefined, comment?: string | NodeArray<JSDocComment>): JSDocSeeTag {
    //         const node = createBaseJSDocTag<JSDocSeeTag>(SyntaxKind::JSDocSeeTag, tagName ?? createIdentifier("see"), comment);
    //         node.name = name;
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocSeeTag(node: JSDocSeeTag, tagName: Identifier | undefined, name: JSDocNameReference | undefined, comment?: string | NodeArray<JSDocComment>): JSDocSeeTag {
    //         return node.tagName !== tagName
    //             || node.name !== name
    //             || node.comment !== comment
    //             ? update(createJSDocSeeTag(tagName, name, comment), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocNameReference(name: EntityName | JSDocMemberName): JSDocNameReference {
    //         const node = createBaseNode<JSDocNameReference>(SyntaxKind::JSDocNameReference);
    //         node.name = name;
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocNameReference(node: JSDocNameReference, name: EntityName | JSDocMemberName): JSDocNameReference {
    //         return node.name !== name
    //             ? update(createJSDocNameReference(name), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocMemberName(left: EntityName | JSDocMemberName, right: Identifier) {
    //         const node = createBaseNode<JSDocMemberName>(SyntaxKind::JSDocMemberName);
    //         node.left = left;
    //         node.right = right;
    //         node.transformFlags |=
    //             propagateChildFlags(node.left) |
    //             propagateChildFlags(node.right);
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocMemberName(node: JSDocMemberName, left: EntityName | JSDocMemberName, right: Identifier) {
    //         return node.left !== left
    //             || node.right !== right
    //             ? update(createJSDocMemberName(left, right), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocLink(name: EntityName | JSDocMemberName | undefined, text: string): JSDocLink {
    //         const node = createBaseNode<JSDocLink>(SyntaxKind::JSDocLink);
    //         node.name = name;
    //         node.text = text;
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocLink(node: JSDocLink, name: EntityName | JSDocMemberName | undefined, text: string): JSDocLink {
    //         return node.name !== name
    //             ? update(createJSDocLink(name, text), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocLinkCode(name: EntityName | JSDocMemberName | undefined, text: string): JSDocLinkCode {
    //         const node = createBaseNode<JSDocLinkCode>(SyntaxKind::JSDocLinkCode);
    //         node.name = name;
    //         node.text = text;
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocLinkCode(node: JSDocLinkCode, name: EntityName | JSDocMemberName | undefined, text: string): JSDocLinkCode {
    //         return node.name !== name
    //             ? update(createJSDocLinkCode(name, text), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocLinkPlain(name: EntityName | JSDocMemberName | undefined, text: string): JSDocLinkPlain {
    //         const node = createBaseNode<JSDocLinkPlain>(SyntaxKind::JSDocLinkPlain);
    //         node.name = name;
    //         node.text = text;
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocLinkPlain(node: JSDocLinkPlain, name: EntityName | JSDocMemberName | undefined, text: string): JSDocLinkPlain {
    //         return node.name !== name
    //             ? update(createJSDocLinkPlain(name, text), node)
    //             : node;
    //     }

    //     // @api
    //     function updateJSDocImplementsTag(node: JSDocImplementsTag, tagName: Identifier = getDefaultTagName(node), className: JSDocImplementsTag["class"], comment: string | NodeArray<JSDocComment> | undefined): JSDocImplementsTag {
    //         return node.tagName !== tagName
    //             || node.class !== className
    //             || node.comment !== comment
    //             ? update(createJSDocImplementsTag(tagName, className, comment), node)
    //             : node;
    //     }

    //     // @api
    //     // createJSDocAuthorTag
    //     // createJSDocClassTag
    //     // createJSDocPublicTag
    //     // createJSDocPrivateTag
    //     // createJSDocProtectedTag
    //     // createJSDocReadonlyTag
    //     // createJSDocDeprecatedTag
    //     function createJSDocSimpleTagWorker<T extends JSDocTag>(kind: T["kind"], tagName: Identifier | undefined, comment?: string | NodeArray<JSDocComment>) {
    //         const node = createBaseJSDocTag<T>(kind, tagName ?? createIdentifier(getDefaultTagNameForKind(kind)), comment);
    //         return node;
    //     }

    //     // @api
    //     // updateJSDocAuthorTag
    //     // updateJSDocClassTag
    //     // updateJSDocPublicTag
    //     // updateJSDocPrivateTag
    //     // updateJSDocProtectedTag
    //     // updateJSDocReadonlyTag
    //     // updateJSDocDeprecatedTag
    //     function updateJSDocSimpleTagWorker<T extends JSDocTag>(kind: T["kind"], node: T, tagName: Identifier = getDefaultTagName(node), comment: string | NodeArray<JSDocComment> | undefined) {
    //         return node.tagName !== tagName
    //             || node.comment !== comment
    //             ? update(createJSDocSimpleTagWorker(kind, tagName, comment), node) :
    //             node;
    //     }

    //     // @api
    //     // createJSDocTypeTag
    //     // createJSDocReturnTag
    //     // createJSDocThisTag
    //     // createJSDocEnumTag
    //     function createJSDocTypeLikeTagWorker<T extends JSDocTag & { typeExpression?: JSDocTypeExpression }>(kind: T["kind"], tagName: Identifier | undefined, typeExpression?: JSDocTypeExpression, comment?: string | NodeArray<JSDocComment>) {
    //         const node = createBaseJSDocTag<T>(kind, tagName ?? createIdentifier(getDefaultTagNameForKind(kind)), comment);
    //         node.typeExpression = typeExpression;
    //         return node;
    //     }

    //     // @api
    //     // updateJSDocTypeTag
    //     // updateJSDocReturnTag
    //     // updateJSDocThisTag
    //     // updateJSDocEnumTag
    //     function updateJSDocTypeLikeTagWorker<T extends JSDocTag & { typeExpression?: JSDocTypeExpression }>(kind: T["kind"], node: T, tagName: Identifier = getDefaultTagName(node), typeExpression: JSDocTypeExpression | undefined, comment: string | NodeArray<JSDocComment> | undefined) {
    //         return node.tagName !== tagName
    //             || node.typeExpression !== typeExpression
    //             || node.comment !== comment
    //             ? update(createJSDocTypeLikeTagWorker(kind, tagName, typeExpression, comment), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocUnknownTag(tagName: Identifier, comment?: string | NodeArray<JSDocComment>): JSDocUnknownTag {
    //         const node = createBaseJSDocTag<JSDocUnknownTag>(SyntaxKind::JSDocTag, tagName, comment);
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocUnknownTag(node: JSDocUnknownTag, tagName: Identifier, comment: string | NodeArray<JSDocComment> | undefined): JSDocUnknownTag {
    //         return node.tagName !== tagName
    //             || node.comment !== comment
    //             ? update(createJSDocUnknownTag(tagName, comment), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocText(text: string): JSDocText {
    //         const node = createBaseNode<JSDocText>(SyntaxKind::JSDocText);
    //         node.text = text;
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocText(node: JSDocText, text: string): JSDocText {
    //         return node.text !== text
    //             ? update(createJSDocText(text), node)
    //             : node;
    //     }

    //     // @api
    //     function createJSDocComment(comment?: string | NodeArray<JSDocComment> | undefined, tags?: readonly JSDocTag[] | undefined) {
    //         const node = createBaseNode<JSDoc>(SyntaxKind::JSDocComment);
    //         node.comment = comment;
    //         node.tags = asNodeArray(tags);
    //         return node;
    //     }

    //     // @api
    //     function updateJSDocComment(node: JSDoc, comment: string | NodeArray<JSDocComment> | undefined, tags: readonly JSDocTag[] | undefined) {
    //         return node.comment !== comment
    //             || node.tags !== tags
    //             ? update(createJSDocComment(comment, tags), node)
    //             : node;
    //     }

    //     //
    //     // JSX
    //     //

    //     // @api
    //     function createJsxElement(openingElement: JsxOpeningElement, children: readonly JsxChild[], closingElement: JsxClosingElement) {
    //         const node = createBaseNode<JsxElement>(SyntaxKind::JsxElement);
    //         node.openingElement = openingElement;
    //         node.children = createNodeArray(children);
    //         node.closingElement = closingElement;
    //         node.transformFlags |=
    //             propagateChildFlags(node.openingElement) |
    //             propagateChildrenFlags(node.children) |
    //             propagateChildFlags(node.closingElement) |
    //             TransformFlags::ContainsJsx;
    //         return node;
    //     }

    //     // @api
    //     function updateJsxElement(node: JsxElement, openingElement: JsxOpeningElement, children: readonly JsxChild[], closingElement: JsxClosingElement) {
    //         return node.openingElement !== openingElement
    //             || node.children !== children
    //             || node.closingElement !== closingElement
    //             ? update(createJsxElement(openingElement, children, closingElement), node)
    //             : node;
    //     }

    //     // @api
    //     function createJsxSelfClosingElement(tagName: JsxTagNameExpression, typeArguments: readonly TypeNode[] | undefined, attributes: JsxAttributes) {
    //         const node = createBaseNode<JsxSelfClosingElement>(SyntaxKind::JsxSelfClosingElement);
    //         node.tagName = tagName;
    //         node.typeArguments = asNodeArray(typeArguments);
    //         node.attributes = attributes;
    //         node.transformFlags |=
    //             propagateChildFlags(node.tagName) |
    //             propagateChildrenFlags(node.typeArguments) |
    //             propagateChildFlags(node.attributes) |
    //             TransformFlags::ContainsJsx;
    //         if (node.typeArguments) {
    //             node.transformFlags |= TransformFlags::ContainsTypeScript;
    //         }
    //         return node;
    //     }

    //     // @api
    //     function updateJsxSelfClosingElement(node: JsxSelfClosingElement, tagName: JsxTagNameExpression, typeArguments: readonly TypeNode[] | undefined, attributes: JsxAttributes) {
    //         return node.tagName !== tagName
    //             || node.typeArguments !== typeArguments
    //             || node.attributes !== attributes
    //             ? update(createJsxSelfClosingElement(tagName, typeArguments, attributes), node)
    //             : node;
    //     }

    //     // @api
    //     function createJsxOpeningElement(tagName: JsxTagNameExpression, typeArguments: readonly TypeNode[] | undefined, attributes: JsxAttributes) {
    //         const node = createBaseNode<JsxOpeningElement>(SyntaxKind::JsxOpeningElement);
    //         node.tagName = tagName;
    //         node.typeArguments = asNodeArray(typeArguments);
    //         node.attributes = attributes;
    //         node.transformFlags |=
    //             propagateChildFlags(node.tagName) |
    //             propagateChildrenFlags(node.typeArguments) |
    //             propagateChildFlags(node.attributes) |
    //             TransformFlags::ContainsJsx;
    //         if (typeArguments) {
    //             node.transformFlags |= TransformFlags::ContainsTypeScript;
    //         }
    //         return node;
    //     }

    //     // @api
    //     function updateJsxOpeningElement(node: JsxOpeningElement, tagName: JsxTagNameExpression, typeArguments: readonly TypeNode[] | undefined, attributes: JsxAttributes) {
    //         return node.tagName !== tagName
    //             || node.typeArguments !== typeArguments
    //             || node.attributes !== attributes
    //             ? update(createJsxOpeningElement(tagName, typeArguments, attributes), node)
    //             : node;
    //     }

    //     // @api
    //     function createJsxClosingElement(tagName: JsxTagNameExpression) {
    //         const node = createBaseNode<JsxClosingElement>(SyntaxKind::JsxClosingElement);
    //         node.tagName = tagName;
    //         node.transformFlags |=
    //             propagateChildFlags(node.tagName) |
    //             TransformFlags::ContainsJsx;
    //         return node;
    //     }

    //     // @api
    //     function updateJsxClosingElement(node: JsxClosingElement, tagName: JsxTagNameExpression) {
    //         return node.tagName !== tagName
    //             ? update(createJsxClosingElement(tagName), node)
    //             : node;
    //     }

    //     // @api
    //     function createJsxFragment(openingFragment: JsxOpeningFragment, children: readonly JsxChild[], closingFragment: JsxClosingFragment) {
    //         const node = createBaseNode<JsxFragment>(SyntaxKind::JsxFragment);
    //         node.openingFragment = openingFragment;
    //         node.children = createNodeArray(children);
    //         node.closingFragment = closingFragment;
    //         node.transformFlags |=
    //             propagateChildFlags(node.openingFragment) |
    //             propagateChildrenFlags(node.children) |
    //             propagateChildFlags(node.closingFragment) |
    //             TransformFlags::ContainsJsx;
    //         return node;
    //     }

    //     // @api
    //     function updateJsxFragment(node: JsxFragment, openingFragment: JsxOpeningFragment, children: readonly JsxChild[], closingFragment: JsxClosingFragment) {
    //         return node.openingFragment !== openingFragment
    //             || node.children !== children
    //             || node.closingFragment !== closingFragment
    //             ? update(createJsxFragment(openingFragment, children, closingFragment), node)
    //             : node;
    //     }

    //     // @api
    //     function createJsxText(text: string, containsOnlyTriviaWhiteSpaces?: boolean) {
    //         const node = createBaseNode<JsxText>(SyntaxKind::JsxText);
    //         node.text = text;
    //         node.containsOnlyTriviaWhiteSpaces = !!containsOnlyTriviaWhiteSpaces;
    //         node.transformFlags |= TransformFlags::ContainsJsx;
    //         return node;
    //     }

    //     // @api
    //     function updateJsxText(node: JsxText, text: string, containsOnlyTriviaWhiteSpaces?: boolean) {
    //         return node.text !== text
    //             || node.containsOnlyTriviaWhiteSpaces !== containsOnlyTriviaWhiteSpaces
    //             ? update(createJsxText(text, containsOnlyTriviaWhiteSpaces), node)
    //             : node;
    //     }

    //     // @api
    //     function createJsxOpeningFragment() {
    //         const node = createBaseNode<JsxOpeningFragment>(SyntaxKind::JsxOpeningFragment);
    //         node.transformFlags |= TransformFlags::ContainsJsx;
    //         return node;
    //     }

    //     // @api
    //     function createJsxJsxClosingFragment() {
    //         const node = createBaseNode<JsxClosingFragment>(SyntaxKind::JsxClosingFragment);
    //         node.transformFlags |= TransformFlags::ContainsJsx;
    //         return node;
    //     }

    //     // @api
    //     function createJsxAttribute(name: Identifier, initializer: StringLiteral | JsxExpression | undefined) {
    //         const node = createBaseNode<JsxAttribute>(SyntaxKind::JsxAttribute);
    //         node.name = name;
    //         node.initializer = initializer;
    //         node.transformFlags |=
    //             propagateChildFlags(node.name) |
    //             propagateChildFlags(node.initializer) |
    //             TransformFlags::ContainsJsx;
    //         return node;
    //     }

    //     // @api
    //     function updateJsxAttribute(node: JsxAttribute, name: Identifier, initializer: StringLiteral | JsxExpression | undefined) {
    //         return node.name !== name
    //             || node.initializer !== initializer
    //             ? update(createJsxAttribute(name, initializer), node)
    //             : node;
    //     }

    //     // @api
    //     function createJsxAttributes(properties: readonly JsxAttributeLike[]) {
    //         const node = createBaseNode<JsxAttributes>(SyntaxKind::JsxAttributes);
    //         node.properties = createNodeArray(properties);
    //         node.transformFlags |=
    //             propagateChildrenFlags(node.properties) |
    //             TransformFlags::ContainsJsx;
    //         return node;
    //     }

    //     // @api
    //     function updateJsxAttributes(node: JsxAttributes, properties: readonly JsxAttributeLike[]) {
    //         return node.properties !== properties
    //             ? update(createJsxAttributes(properties), node)
    //             : node;
    //     }

    //     // @api
    //     function createJsxSpreadAttribute(expression: Expression) {
    //         const node = createBaseNode<JsxSpreadAttribute>(SyntaxKind::JsxSpreadAttribute);
    //         node.expression = expression;
    //         node.transformFlags |=
    //             propagateChildFlags(node.expression) |
    //             TransformFlags::ContainsJsx;
    //         return node;
    //     }

    //     // @api
    //     function updateJsxSpreadAttribute(node: JsxSpreadAttribute, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createJsxSpreadAttribute(expression), node)
    //             : node;
    //     }

    //     // @api
    //     function createJsxExpression(dotDotDotToken: DotDotDotToken | undefined, expression: Expression | undefined) {
    //         const node = createBaseNode<JsxExpression>(SyntaxKind::JsxExpression);
    //         node.dotDotDotToken = dotDotDotToken;
    //         node.expression = expression;
    //         node.transformFlags |=
    //             propagateChildFlags(node.dotDotDotToken) |
    //             propagateChildFlags(node.expression) |
    //             TransformFlags::ContainsJsx;
    //         return node;
    //     }

    //     // @api
    //     function updateJsxExpression(node: JsxExpression, expression: Expression | undefined) {
    //         return node.expression !== expression
    //             ? update(createJsxExpression(node.dotDotDotToken, expression), node)
    //             : node;
    //     }

    //
    // Clauses
    //

    pub fn createCaseClause(
        &mut self,
        expression: Expression,
        statements: NodeArray<Statement>,
    ) -> CaseClause {
        let node = CaseClause {
            node_id: self.node_id_gen.next(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeExpressionForDisallowedComma(expression),
            expression,
            statements: self.updateNodeArray(Some(statements), None),
        };
        let child_flags = self.propagateChildFlags(Some(&node.expression))
            | propagateChildrenFlags(Some(&node.statements));
        self.node_data_mut(&node).transformFlags |= child_flags;
        node
    }

    //     // @api
    //     function updateCaseClause(node: CaseClause, expression: Expression, statements: readonly Statement[]) {
    //         return node.expression !== expression
    //             || node.statements !== statements
    //             ? update(createCaseClause(expression, statements), node)
    //             : node;
    //     }

    pub fn createDefaultClause(&mut self, statements: NodeArray<Statement>) -> DefaultClause {
        let node = DefaultClause {
            node_id: self.node_id_gen.next(),
            statements: self.updateNodeArray(Some(statements), None),
        };
        self.node_data_mut(&node).transformFlags = propagateChildrenFlags(Some(&node.statements));
        node
    }

    //     // @api
    //     function updateDefaultClause(node: DefaultClause, statements: readonly Statement[]) {
    //         return node.statements !== statements
    //             ? update(createDefaultClause(statements), node)
    //             : node;
    //     }

    pub fn createHeritageClause(
        &mut self,
        token: SyntaxKind,
        types: NodeArray<Rc<ExpressionWithTypeArguments>>,
    ) -> HeritageClause {
        let node = HeritageClause {
            node_id: self.node_id_gen.next(),
            token,
            types: self.updateNodeArray(Some(types), None),
        };
        let mut transform_flags = propagateChildrenFlags(Some(&node.types));
        match token {
            SyntaxKind::ExtendsKeyword => {
                transform_flags |= TransformFlags::ContainsES2015;
            }
            SyntaxKind::ImplementsKeyword => {
                transform_flags |= TransformFlags::ContainsTypeScript;
            }
            _ => unreachable!(),
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateHeritageClause(node: HeritageClause, types: readonly ExpressionWithTypeArguments[]) {
    //         return node.types !== types
    //             ? update(createHeritageClause(node.token, types), node)
    //             : node;
    //     }

    pub fn createCatchClause(
        &mut self,
        variableDeclaration: Option<Rc<VariableDeclaration>>,
        block: Rc<Block>,
    ) -> CatchClause {
        let node = CatchClause {
            node_id: self.node_id_gen.next(),
            variableDeclaration,
            block,
        };
        // if (typeof variableDeclaration === "string" || variableDeclaration && !isVariableDeclaration(variableDeclaration)) {
        //     variableDeclaration = createVariableDeclaration(
        //         variableDeclaration,
        //         /*exclamationToken*/ undefined,
        //         /*type*/ undefined,
        //         /*initializer*/ undefined
        //     );
        // }
        let mut transform_flags = self.propagateChildFlags(node.variableDeclaration.as_ref())
            | self.propagateChildFlags(Some(&node.block));
        if node.variableDeclaration.is_none() {
            transform_flags |= TransformFlags::ContainsES2019;
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateCatchClause(node: CatchClause, variableDeclaration: VariableDeclaration | undefined, block: Block) {
    //         return node.variableDeclaration !== variableDeclaration
    //             || node.block !== block
    //             ? update(createCatchClause(variableDeclaration, block), node)
    //             : node;
    //     }

    //     //
    //     // Property assignments
    //     //

    pub fn createPropertyAssignment(
        &mut self,
        name: PropertyName,
        initializer: Expression,
    ) -> PropertyAssignment {
        let node = PropertyAssignment {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: None,
            modifiers: None,
            name,
            questionToken: None,
            exclamationToken: None,
            // TODO:
            // initializer: parenthesizerRules().parenthesizeExpressionForDisallowedComma(initializer),
            initializer,
        };

        let mut transform_flags = self.propagateChildFlags(Some(&node.name))
            | self.propagateChildFlags(Some(&node.initializer));

        // The PropertyName of a member is allowed to be `await`.
        // We don't need to exclude `await` for type signatures since types
        // don't propagate child flags.
        if let PropertyName::Identifier(name) = &node.name {
            transform_flags |= self.propagateIdentifierNameFlags(name);
        } else {
            transform_flags |= self.propagateChildFlags(Some(&node.name));
        }
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     function finishUpdatePropertyAssignment(updated: Mutable<PropertyAssignment>, original: PropertyAssignment) {
    //         // copy children used only for error reporting
    //         if (original.decorators) updated.decorators = original.decorators;
    //         if (original.modifiers) updated.modifiers = original.modifiers;
    //         if (original.questionToken) updated.questionToken = original.questionToken;
    //         if (original.exclamationToken) updated.exclamationToken = original.exclamationToken;
    //         return update(updated, original);
    //     }

    //     // @api
    //     function updatePropertyAssignment(node: PropertyAssignment, name: PropertyName, initializer: Expression) {
    //         return node.name !== name
    //             || node.initializer !== initializer
    //             ? finishUpdatePropertyAssignment(createPropertyAssignment(name, initializer), node)
    //             : node;
    //     }

    pub fn createShorthandPropertyAssignment(
        &mut self,
        name: Rc<Identifier>,
        objectAssignmentInitializer: Option<Expression>,
    ) -> ShorthandPropertyAssignment {
        let node = ShorthandPropertyAssignment {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            decorators: None,
            modifiers: None,
            name,
            questionToken: None,
            exclamationToken: None,
            equalsToken: None,
            // TODO:
            // objectAssignmentInitializer: objectAssignmentInitializer && parenthesizerRules().parenthesizeExpressionForDisallowedComma(objectAssignmentInitializer),
            objectAssignmentInitializer,
        };

        let transform_flags = self.propagateChildFlags(Some(&node.name))
            | self.propagateChildFlags(node.objectAssignmentInitializer.as_ref())
            | TransformFlags::ContainsES2015;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     function finishUpdateShorthandPropertyAssignment(updated: Mutable<ShorthandPropertyAssignment>, original: ShorthandPropertyAssignment) {
    //         // copy children used only for error reporting
    //         if (original.decorators) updated.decorators = original.decorators;
    //         if (original.modifiers) updated.modifiers = original.modifiers;
    //         if (original.equalsToken) updated.equalsToken = original.equalsToken;
    //         if (original.questionToken) updated.questionToken = original.questionToken;
    //         if (original.exclamationToken) updated.exclamationToken = original.exclamationToken;
    //         return update(updated, original);
    //     }

    //     // @api
    //     function updateShorthandPropertyAssignment(node: ShorthandPropertyAssignment, name: Identifier, objectAssignmentInitializer: Expression | undefined) {
    //         return node.name !== name
    //             || node.objectAssignmentInitializer !== objectAssignmentInitializer
    //             ? finishUpdateShorthandPropertyAssignment(createShorthandPropertyAssignment(name, objectAssignmentInitializer), node)
    //             : node;
    //     }

    pub fn createSpreadAssignment(&mut self, expression: Expression) -> SpreadAssignment {
        let node = SpreadAssignment {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            // TODO:
            // expression: parenthesizerRules().parenthesizeExpressionForDisallowedComma(expression),
            expression,
        };

        let transform_flags = self.propagateChildFlags(Some(&node.expression))
            | TransformFlags::ContainsES2018
            | TransformFlags::ContainsObjectRestOrSpread;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateSpreadAssignment(node: SpreadAssignment, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createSpreadAssignment(expression), node)
    //             : node;
    //     }

    //     //
    //     // Enum
    //     //

    pub fn createEnumMember(
        &mut self,
        name: PropertyName,
        initializer: Option<Expression>,
    ) -> EnumMember {
        let node = EnumMember {
            node_id: self.node_id_gen.next(),
            js_doc_container: JSDocContainer::default(),
            name,
            // TODO:
            // initializer: initializer && parenthesizerRules().parenthesizeExpressionForDisallowedComma(initializer),
            initializer,
        };
        let transform_flags = self.propagateChildFlags(Some(&node.name))
            | self.propagateChildFlags(node.initializer.as_ref())
            | TransformFlags::ContainsTypeScript;
        self.node_data_mut(&node).transformFlags |= transform_flags;
        node
    }

    //     // @api
    //     function updateEnumMember(node: EnumMember, name: PropertyName, initializer: Expression | undefined) {
    //         return node.name !== name
    //             || node.initializer !== initializer
    //             ? update(createEnumMember(name, initializer), node)
    //             : node;
    //     }

    //     //
    //     // Top-level nodes
    //     //

    pub fn createSourceFile(
        &mut self,
        statements: NodeArray<Statement>,
        endOfFileToken: Rc<EndOfFileToken>,
        flags: NodeFlags,
    ) -> SourceFile {
        let node_id = self.node_id_gen.next();
        let eof_flags = self.propagateChildFlags(Some(&endOfFileToken));
        self.node_data_mut(node_id).transformFlags |=
            propagateChildrenFlags(Some(&statements)) | eof_flags;
        self.node_data_mut(node_id).flags |= flags;
        SourceFile {
            node_id,
            statements: self.updateNodeArray(Some(statements), None),
            endOfFileToken,
            fileName: "".into(),
            text: "".into(),
            languageVersion: ScriptTarget::ES3,
            languageVariant: LanguageVariant::Standard,
            scriptKind: ScriptKind::Unknown,
            isDeclarationFile: false,
            hasNoDefaultLib: false,

            externalModuleIndicator: None,
            bindDiagnostics: Vec::new(),
            bindSuggestionDiagnostics: None,
            parseDiagnostics: Vec::new(),
            jsDocDiagnostics: Vec::new(),
        }
    }

    //     function cloneSourceFileWithChanges(
    //         source: SourceFile,
    //         statements: readonly Statement[],
    //         isDeclarationFile: boolean,
    //         referencedFiles: readonly FileReference[],
    //         typeReferences: readonly FileReference[],
    //         hasNoDefaultLib: boolean,
    //         libReferences: readonly FileReference[]
    //     ) {
    //         const node = baseFactory.createBaseSourceFileNode(SyntaxKind::SourceFile) as Mutable<SourceFile>;
    //         for (const p in source) {
    //             if (p === "emitNode" || hasProperty(node, p) || !hasProperty(source, p)) continue;
    //             (node as any)[p] = (source as any)[p];
    //         }
    //         node.flags |= source.flags;
    //         node.statements = createNodeArray(statements);
    //         node.endOfFileToken = source.endOfFileToken;
    //         node.isDeclarationFile = isDeclarationFile;
    //         node.referencedFiles = referencedFiles;
    //         node.typeReferenceDirectives = typeReferences;
    //         node.hasNoDefaultLib = hasNoDefaultLib;
    //         node.libReferenceDirectives = libReferences;
    //         node.transformFlags =
    //             propagateChildrenFlags(node.statements) |
    //             propagateChildFlags(node.endOfFileToken);
    //         node.impliedNodeFormat = source.impliedNodeFormat;
    //         return node;
    //     }

    //     // @api
    //     function updateSourceFile(
    //         node: SourceFile,
    //         statements: readonly Statement[],
    //         isDeclarationFile = node.isDeclarationFile,
    //         referencedFiles = node.referencedFiles,
    //         typeReferenceDirectives = node.typeReferenceDirectives,
    //         hasNoDefaultLib = node.hasNoDefaultLib,
    //         libReferenceDirectives = node.libReferenceDirectives
    //     ) {
    //         return node.statements !== statements
    //             || node.isDeclarationFile !== isDeclarationFile
    //             || node.referencedFiles !== referencedFiles
    //             || node.typeReferenceDirectives !== typeReferenceDirectives
    //             || node.hasNoDefaultLib !== hasNoDefaultLib
    //             || node.libReferenceDirectives !== libReferenceDirectives
    //             ? update(cloneSourceFileWithChanges(node, statements, isDeclarationFile, referencedFiles, typeReferenceDirectives, hasNoDefaultLib, libReferenceDirectives), node)
    //             : node;
    //     }

    //     // @api
    //     function createBundle(sourceFiles: readonly SourceFile[], prepends: readonly (UnparsedSource | InputFiles)[] = emptyArray) {
    //         const node = createBaseNode<Bundle>(SyntaxKind::Bundle);
    //         node.prepends = prepends;
    //         node.sourceFiles = sourceFiles;
    //         return node;
    //     }

    //     // @api
    //     function updateBundle(node: Bundle, sourceFiles: readonly SourceFile[], prepends: readonly (UnparsedSource | InputFiles)[] = emptyArray) {
    //         return node.sourceFiles !== sourceFiles
    //             || node.prepends !== prepends
    //             ? update(createBundle(sourceFiles, prepends), node)
    //             : node;
    //     }

    //     // @api
    //     function createUnparsedSource(prologues: readonly UnparsedPrologue[], syntheticReferences: readonly UnparsedSyntheticReference[] | undefined, texts: readonly UnparsedSourceText[]) {
    //         const node = createBaseNode<UnparsedSource>(SyntaxKind::UnparsedSource);
    //         node.prologues = prologues;
    //         node.syntheticReferences = syntheticReferences;
    //         node.texts = texts;
    //         node.fileName = "";
    //         node.text = "";
    //         node.referencedFiles = emptyArray;
    //         node.libReferenceDirectives = emptyArray;
    //         node.getLineAndCharacterOfPosition = pos => getLineAndCharacterOfPosition(node, pos);
    //         return node;
    //     }

    //     function createBaseUnparsedNode<T extends UnparsedNode>(kind: T["kind"], data?: string) {
    //         const node = createBaseNode(kind);
    //         node.data = data;
    //         return node;
    //     }

    //     // @api
    //     function createUnparsedPrologue(data?: string): UnparsedPrologue {
    //         return createBaseUnparsedNode(SyntaxKind::UnparsedPrologue, data);
    //     }

    //     // @api
    //     function createUnparsedPrepend(data: string | undefined, texts: readonly UnparsedTextLike[]): UnparsedPrepend {
    //         const node = createBaseUnparsedNode<UnparsedPrepend>(SyntaxKind::UnparsedPrepend, data);
    //         node.texts = texts;
    //         return node;
    //     }

    //     // @api
    //     function createUnparsedTextLike(data: string | undefined, internal: boolean): UnparsedTextLike {
    //         return createBaseUnparsedNode(internal ? SyntaxKind::UnparsedInternalText : SyntaxKind::UnparsedText, data);
    //     }

    //     // @api
    //     function createUnparsedSyntheticReference(section: BundleFileHasNoDefaultLib | BundleFileReference): UnparsedSyntheticReference {
    //         const node = createBaseNode<UnparsedSyntheticReference>(SyntaxKind::UnparsedSyntheticReference);
    //         node.data = section.data;
    //         node.section = section;
    //         return node;
    //     }

    //     // @api
    //     function createInputFiles(): InputFiles {
    //         const node = createBaseNode<InputFiles>(SyntaxKind::InputFiles);
    //         node.javascriptText = "";
    //         node.declarationText = "";
    //         return node;
    //     }

    //     //
    //     // Synthetic Nodes (used by checker)
    //     //

    //     // @api
    //     function createSyntheticExpression(type: Type, isSpread = false, tupleNameSource?: ParameterDeclaration | NamedTupleMember) {
    //         const node = createBaseNode<SyntheticExpression>(SyntaxKind::SyntheticExpression);
    //         node.type = type;
    //         node.isSpread = isSpread;
    //         node.tupleNameSource = tupleNameSource;
    //         return node;
    //     }

    //     // @api
    //     function createSyntaxList(children: Node[]) {
    //         const node = createBaseNode<SyntaxList>(SyntaxKind::SyntaxList);
    //         node._children = children;
    //         return node;
    //     }

    //     //
    //     // Transformation nodes
    //     //

    //     /**
    //      * Creates a synthetic statement to act as a placeholder for a not-emitted statement in
    //      * order to preserve comments.
    //      *
    //      * @param original The original statement.
    //      */
    //     // @api
    //     function createNotEmittedStatement(original: Node) {
    //         const node = createBaseNode<NotEmittedStatement>(SyntaxKind::NotEmittedStatement);
    //         node.original = original;
    //         setTextRange(node, original);
    //         return node;
    //     }

    //     /**
    //      * Creates a synthetic expression to act as a placeholder for a not-emitted expression in
    //      * order to preserve comments or sourcemap positions.
    //      *
    //      * @param expression The inner expression to emit.
    //      * @param original The original outer expression.
    //      */
    //     // @api
    //     function createPartiallyEmittedExpression(expression: Expression, original?: Node) {
    //         const node = createBaseNode<PartiallyEmittedExpression>(SyntaxKind::PartiallyEmittedExpression);
    //         node.expression = expression;
    //         node.original = original;
    //         node.transformFlags |=
    //             propagateChildFlags(node.expression) |
    //             TransformFlags::ContainsTypeScript;
    //         setTextRange(node, original);
    //         return node;
    //     }

    //     // @api
    //     function updatePartiallyEmittedExpression(node: PartiallyEmittedExpression, expression: Expression) {
    //         return node.expression !== expression
    //             ? update(createPartiallyEmittedExpression(expression, node.original), node)
    //             : node;
    //     }

    //     function flattenCommaElements(node: Expression): Expression | readonly Expression[] {
    //         if (nodeIsSynthesized(node) && !isParseTreeNode(node) && !node.original && !node.emitNode && !node.id) {
    //             if (isCommaListExpression(node)) {
    //                 return node.elements;
    //             }
    //             if (isBinaryExpression(node) && isCommaToken(node.operatorToken)) {
    //                 return [node.left, node.right];
    //             }
    //         }
    //         return node;
    //     }

    //     // @api
    //     function createCommaListExpression(elements: readonly Expression[]) {
    //         const node = createBaseNode<CommaListExpression>(SyntaxKind::CommaListExpression);
    //         node.elements = createNodeArray(sameFlatMap(elements, flattenCommaElements));
    //         node.transformFlags |= propagateChildrenFlags(node.elements);
    //         return node;
    //     }

    //     // @api
    //     function updateCommaListExpression(node: CommaListExpression, elements: readonly Expression[]) {
    //         return node.elements !== elements
    //             ? update(createCommaListExpression(elements), node)
    //             : node;
    //     }

    //     /**
    //      * Creates a synthetic element to act as a placeholder for the end of an emitted declaration in
    //      * order to properly emit exports.
    //      */
    //     // @api
    //     function createEndOfDeclarationMarker(original: Node) {
    //         const node = createBaseNode<EndOfDeclarationMarker>(SyntaxKind::EndOfDeclarationMarker);
    //         node.emitNode = {} as EmitNode;
    //         node.original = original;
    //         return node;
    //     }

    //     /**
    //      * Creates a synthetic element to act as a placeholder for the beginning of a merged declaration in
    //      * order to properly emit exports.
    //      */
    //     // @api
    //     function createMergeDeclarationMarker(original: Node) {
    //         const node = createBaseNode<MergeDeclarationMarker>(SyntaxKind::MergeDeclarationMarker);
    //         node.emitNode = {} as EmitNode;
    //         node.original = original;
    //         return node;
    //     }

    //     // @api
    //     function createSyntheticReferenceExpression(expression: Expression, thisArg: Expression) {
    //         const node = createBaseNode<SyntheticReferenceExpression>(SyntaxKind::SyntheticReferenceExpression);
    //         node.expression = expression;
    //         node.thisArg = thisArg;
    //         node.transformFlags |=
    //             propagateChildFlags(node.expression) |
    //             propagateChildFlags(node.thisArg);
    //         return node;
    //     }

    //     // @api
    //     function updateSyntheticReferenceExpression(node: SyntheticReferenceExpression, expression: Expression, thisArg: Expression) {
    //         return node.expression !== expression
    //             || node.thisArg !== thisArg
    //             ? update(createSyntheticReferenceExpression(expression, thisArg), node)
    //             : node;
    //     }

    //     // @api
    //     function cloneNode<T extends Node | undefined>(node: T): T;
    //     function cloneNode<T extends Node>(node: T) {
    //         // We don't use "clone" from core.ts here, as we need to preserve the prototype chain of
    //         // the original node. We also need to exclude specific properties and only include own-
    //         // properties (to skip members already defined on the shared prototype).
    //         if (node === undefined) {
    //             return node;
    //         }

    //         const clone =
    //             isSourceFile(node) ? baseFactory.createBaseSourceFileNode(SyntaxKind::SourceFile) as T :
    //             isIdentifier(node) ? baseFactory.createBaseIdentifierNode(SyntaxKind::Identifier) as T :
    //             isPrivateIdentifier(node) ? baseFactory.createBasePrivateIdentifierNode(SyntaxKind::PrivateIdentifier) as T :
    //             !isNodeKind(node.kind) ? baseFactory.createBaseTokenNode(node.kind) as T :
    //             baseFactory.createBaseNode(node.kind) as T;

    //         (clone as Mutable<T>).flags |= (node.flags & ~NodeFlags.Synthesized);
    //         (clone as Mutable<T>).transformFlags = node.transformFlags;
    //         setOriginalNode(clone, node);

    //         for (const key in node) {
    //             if (clone.hasOwnProperty(key) || !node.hasOwnProperty(key)) {
    //                 continue;
    //             }

    //             clone[key] = node[key];
    //         }

    //         return clone;
    //     }

    //     // compound nodes
    //     function createImmediatelyInvokedFunctionExpression(statements: readonly Statement[]): CallExpression;
    //     function createImmediatelyInvokedFunctionExpression(statements: readonly Statement[], param: ParameterDeclaration, paramValue: Expression): CallExpression;
    //     function createImmediatelyInvokedFunctionExpression(statements: readonly Statement[], param?: ParameterDeclaration, paramValue?: Expression) {
    //         return createCallExpression(
    //             createFunctionExpression(
    //                 /*modifiers*/ undefined,
    //                 /*asteriskToken*/ undefined,
    //                 /*name*/ undefined,
    //                 /*typeParameters*/ undefined,
    //                 /*parameters*/ param ? [param] : [],
    //                 /*type*/ undefined,
    //                 createBlock(statements, /*multiLine*/ true)
    //             ),
    //             /*typeArguments*/ undefined,
    //             /*argumentsArray*/ paramValue ? [paramValue] : []
    //         );
    //     }

    //     function createImmediatelyInvokedArrowFunction(statements: readonly Statement[]): CallExpression;
    //     function createImmediatelyInvokedArrowFunction(statements: readonly Statement[], param: ParameterDeclaration, paramValue: Expression): CallExpression;
    //     function createImmediatelyInvokedArrowFunction(statements: readonly Statement[], param?: ParameterDeclaration, paramValue?: Expression) {
    //         return createCallExpression(
    //             createArrowFunction(
    //                 /*modifiers*/ undefined,
    //                 /*typeParameters*/ undefined,
    //                 /*parameters*/ param ? [param] : [],
    //                 /*type*/ undefined,
    //                 /*equalsGreaterThanToken*/ undefined,
    //                 createBlock(statements, /*multiLine*/ true)
    //             ),
    //             /*typeArguments*/ undefined,
    //             /*argumentsArray*/ paramValue ? [paramValue] : []
    //         );
    //     }

    //     function createVoidZero() {
    //         return createVoidExpression(createNumericLiteral("0"));
    //     }

    //     function createExportDefault(expression: Expression) {
    //         return createExportAssignment(
    //             /*decorators*/ undefined,
    //             /*modifiers*/ undefined,
    //             /*isExportEquals*/ false,
    //             expression);
    //     }

    //     function createExternalModuleExport(exportName: Identifier) {
    //         return createExportDeclaration(
    //             /*decorators*/ undefined,
    //             /*modifiers*/ undefined,
    //             /*isTypeOnly*/ false,
    //             createNamedExports([
    //                 createExportSpecifier(/*isTypeOnly*/ false, /*propertyName*/ undefined, exportName)
    //             ])
    //         );
    //     }

    //     //
    //     // Utilities
    //     //

    //     function createTypeCheck(value: Expression, tag: TypeOfTag) {
    //         return tag === "undefined"
    //             ? factory.createStrictEquality(value, createVoidZero())
    //             : factory.createStrictEquality(createTypeOfExpression(value), createStringLiteral(tag));
    //     }

    //     function createMethodCall(object: Expression, methodName: string | Identifier, argumentsList: readonly Expression[]) {
    //         // Preserve the optionality of `object`.
    //         if (isCallChain(object)) {
    //             return createCallChain(
    //                 createPropertyAccessChain(object, /*questionDotToken*/ undefined, methodName),
    //                 /*questionDotToken*/ undefined,
    //                 /*typeArguments*/ undefined,
    //                 argumentsList
    //             );
    //         }
    //         return createCallExpression(
    //             createPropertyAccessExpression(object, methodName),
    //             /*typeArguments*/ undefined,
    //             argumentsList
    //         );
    //     }

    //     function createFunctionBindCall(target: Expression, thisArg: Expression, argumentsList: readonly Expression[]) {
    //         return createMethodCall(target, "bind", [thisArg, ...argumentsList]);
    //     }

    //     function createFunctionCallCall(target: Expression, thisArg: Expression, argumentsList: readonly Expression[]) {
    //         return createMethodCall(target, "call", [thisArg, ...argumentsList]);
    //     }

    //     function createFunctionApplyCall(target: Expression, thisArg: Expression, argumentsExpression: Expression) {
    //         return createMethodCall(target, "apply", [thisArg, argumentsExpression]);
    //     }

    //     function createGlobalMethodCall(globalObjectName: string, methodName: string, argumentsList: readonly Expression[]) {
    //         return createMethodCall(createIdentifier(globalObjectName), methodName, argumentsList);
    //     }

    //     function createArraySliceCall(array: Expression, start?: number | Expression) {
    //         return createMethodCall(array, "slice", start === undefined ? [] : [asExpression(start)]);
    //     }

    //     function createArrayConcatCall(array: Expression, argumentsList: readonly Expression[]) {
    //         return createMethodCall(array, "concat", argumentsList);
    //     }

    //     function createObjectDefinePropertyCall(target: Expression, propertyName: string | Expression, attributes: Expression) {
    //         return createGlobalMethodCall("Object", "defineProperty", [target, asExpression(propertyName), attributes]);
    //     }

    //     function createReflectGetCall(target: Expression, propertyKey: Expression, receiver?: Expression): CallExpression {
    //         return createGlobalMethodCall("Reflect", "get", receiver ? [target, propertyKey, receiver] : [target, propertyKey]);
    //     }

    //     function createReflectSetCall(target: Expression, propertyKey: Expression, value: Expression, receiver?: Expression): CallExpression {
    //         return createGlobalMethodCall("Reflect", "set", receiver ? [target, propertyKey, value, receiver] : [target, propertyKey, value]);
    //     }

    //     function tryAddPropertyAssignment(properties: Push<PropertyAssignment>, propertyName: string, expression: Expression | undefined) {
    //         if (expression) {
    //             properties.push(createPropertyAssignment(propertyName, expression));
    //             return true;
    //         }
    //         return false;
    //     }

    //     function createPropertyDescriptor(attributes: PropertyDescriptorAttributes, singleLine?: boolean) {
    //         const properties: PropertyAssignment[] = [];
    //         tryAddPropertyAssignment(properties, "enumerable", asExpression(attributes.enumerable));
    //         tryAddPropertyAssignment(properties, "configurable", asExpression(attributes.configurable));

    //         let isData = tryAddPropertyAssignment(properties, "writable", asExpression(attributes.writable));
    //         isData = tryAddPropertyAssignment(properties, "value", attributes.value) || isData;

    //         let isAccessor = tryAddPropertyAssignment(properties, "get", attributes.get);
    //         isAccessor = tryAddPropertyAssignment(properties, "set", attributes.set) || isAccessor;

    //         Debug.assert(!(isData && isAccessor), "A PropertyDescriptor may not be both an accessor descriptor and a data descriptor.");
    //         return createObjectLiteralExpression(properties, !singleLine);
    //     }

    //     function updateOuterExpression(outerExpression: OuterExpression, expression: Expression) {
    //         switch (outerExpression.kind) {
    //             case SyntaxKind::ParenthesizedExpression: return updateParenthesizedExpression(outerExpression, expression);
    //             case SyntaxKind::TypeAssertionExpression: return updateTypeAssertion(outerExpression, outerExpression.type, expression);
    //             case SyntaxKind::AsExpression: return updateAsExpression(outerExpression, expression, outerExpression.type);
    //             case SyntaxKind::NonNullExpression: return updateNonNullExpression(outerExpression, expression);
    //             case SyntaxKind::PartiallyEmittedExpression: return updatePartiallyEmittedExpression(outerExpression, expression);
    //         }
    //     }

    //     /**
    //      * Determines whether a node is a parenthesized expression that can be ignored when recreating outer expressions.
    //      *
    //      * A parenthesized expression can be ignored when all of the following are true:
    //      *
    //      * - It's `pos` and `end` are not -1
    //      * - It does not have a custom source map range
    //      * - It does not have a custom comment range
    //      * - It does not have synthetic leading or trailing comments
    //      *
    //      * If an outermost parenthesized expression is ignored, but the containing expression requires a parentheses around
    //      * the expression to maintain precedence, a new parenthesized expression should be created automatically when
    //      * the containing expression is created/updated.
    //      */
    //     function isIgnorableParen(node: Expression) {
    //         return isParenthesizedExpression(node)
    //             && nodeIsSynthesized(node)
    //             && nodeIsSynthesized(getSourceMapRange(node))
    //             && nodeIsSynthesized(getCommentRange(node))
    //             && !some(getSyntheticLeadingComments(node))
    //             && !some(getSyntheticTrailingComments(node));
    //     }

    //     function restoreOuterExpressions(outerExpression: Expression | undefined, innerExpression: Expression, kinds = OuterExpressionKinds.All): Expression {
    //         if (outerExpression && isOuterExpression(outerExpression, kinds) && !isIgnorableParen(outerExpression)) {
    //             return updateOuterExpression(
    //                 outerExpression,
    //                 restoreOuterExpressions(outerExpression.expression, innerExpression)
    //             );
    //         }
    //         return innerExpression;
    //     }

    //     function restoreEnclosingLabel(node: Statement, outermostLabeledStatement: LabeledStatement | undefined, afterRestoreLabelCallback?: (node: LabeledStatement) => void): Statement {
    //         if (!outermostLabeledStatement) {
    //             return node;
    //         }
    //         const updated = updateLabeledStatement(
    //             outermostLabeledStatement,
    //             outermostLabeledStatement.label,
    //             isLabeledStatement(outermostLabeledStatement.statement)
    //                 ? restoreEnclosingLabel(node, outermostLabeledStatement.statement)
    //                 : node
    //         );
    //         if (afterRestoreLabelCallback) {
    //             afterRestoreLabelCallback(outermostLabeledStatement);
    //         }
    //         return updated;
    //     }

    //     function shouldBeCapturedInTempVariable(node: Expression, cacheIdentifiers: boolean): boolean {
    //         const target = skipParentheses(node);
    //         switch (target.kind) {
    //             case SyntaxKind::Identifier:
    //                 return cacheIdentifiers;
    //             case SyntaxKind::ThisKeyword:
    //             case SyntaxKind::NumericLiteral:
    //             case SyntaxKind::BigIntLiteral:
    //             case SyntaxKind::StringLiteral:
    //                 return false;
    //             case SyntaxKind::ArrayLiteralExpression:
    //                 const elements = (target as ArrayLiteralExpression).elements;
    //                 if (elements.length === 0) {
    //                     return false;
    //                 }
    //                 return true;
    //             case SyntaxKind::ObjectLiteralExpression:
    //                 return (target as ObjectLiteralExpression).properties.length > 0;
    //             default:
    //                 return true;
    //         }
    //     }

    //     function createCallBinding(expression: Expression, recordTempVariable: (temp: Identifier) => void, languageVersion?: ScriptTarget, cacheIdentifiers = false): CallBinding {
    //         const callee = skipOuterExpressions(expression, OuterExpressionKinds.All);
    //         let thisArg: Expression;
    //         let target: LeftHandSideExpression;
    //         if (isSuperProperty(callee)) {
    //             thisArg = createThis();
    //             target = callee;
    //         }
    //         else if (isSuperKeyword(callee)) {
    //             thisArg = createThis();
    //             target = languageVersion !== undefined && languageVersion < ScriptTarget.ES2015
    //                 ? setTextRange(createIdentifier("_super"), callee)
    //                 : callee as PrimaryExpression;
    //         }
    //         else if (getEmitFlags(callee) & EmitFlags.HelperName) {
    //             thisArg = createVoidZero();
    //             target = parenthesizerRules().parenthesizeLeftSideOfAccess(callee);
    //         }
    //         else if (isPropertyAccessExpression(callee)) {
    //             if (shouldBeCapturedInTempVariable(callee.expression, cacheIdentifiers)) {
    //                 // for `a.b()` target is `(_a = a).b` and thisArg is `_a`
    //                 thisArg = createTempVariable(recordTempVariable);
    //                 target = createPropertyAccessExpression(
    //                     setTextRange(
    //                         factory.createAssignment(
    //                             thisArg,
    //                             callee.expression
    //                         ),
    //                         callee.expression
    //                     ),
    //                     callee.name
    //                 );
    //                 setTextRange(target, callee);
    //             }
    //             else {
    //                 thisArg = callee.expression;
    //                 target = callee;
    //             }
    //         }
    //         else if (isElementAccessExpression(callee)) {
    //             if (shouldBeCapturedInTempVariable(callee.expression, cacheIdentifiers)) {
    //                 // for `a[b]()` target is `(_a = a)[b]` and thisArg is `_a`
    //                 thisArg = createTempVariable(recordTempVariable);
    //                 target = createElementAccessExpression(
    //                     setTextRange(
    //                         factory.createAssignment(
    //                             thisArg,
    //                             callee.expression
    //                         ),
    //                         callee.expression
    //                     ),
    //                     callee.argumentExpression
    //                 );
    //                 setTextRange(target, callee);
    //             }
    //             else {
    //                 thisArg = callee.expression;
    //                 target = callee;
    //             }
    //         }
    //         else {
    //             // for `a()` target is `a` and thisArg is `void 0`
    //             thisArg = createVoidZero();
    //             target = parenthesizerRules().parenthesizeLeftSideOfAccess(expression);
    //         }

    //         return { target, thisArg };
    //     }

    //     function createAssignmentTargetWrapper(paramName: Identifier, expression: Expression): LeftHandSideExpression {
    //         return createPropertyAccessExpression(
    //             // Explicit parens required because of v8 regression (https://bugs.chromium.org/p/v8/issues/detail?id=9560)
    //             createParenthesizedExpression(
    //                 createObjectLiteralExpression([
    //                     createSetAccessorDeclaration(
    //                         /*decorators*/ undefined,
    //                         /*modifiers*/ undefined,
    //                         "value",
    //                         [createParameterDeclaration(
    //                             /*decorators*/ undefined,
    //                             /*modifiers*/ undefined,
    //                             /*dotDotDotToken*/ undefined,
    //                             paramName,
    //                             /*questionToken*/ undefined,
    //                             /*type*/ undefined,
    //                             /*initializer*/ undefined
    //                         )],
    //                         createBlock([
    //                             createExpressionStatement(expression)
    //                         ])
    //                     )
    //                 ])
    //             ),
    //             "value"
    //         );
    //     }

    //     function inlineExpressions(expressions: readonly Expression[]) {
    //         // Avoid deeply nested comma expressions as traversing them during emit can result in "Maximum call
    //         // stack size exceeded" errors.
    //         return expressions.length > 10
    //             ? createCommaListExpression(expressions)
    //             : reduceLeft(expressions, factory.createComma)!;
    //     }

    //     function getName(node: Declaration | undefined, allowComments?: boolean, allowSourceMaps?: boolean, emitFlags: EmitFlags = 0) {
    //         const nodeName = getNameOfDeclaration(node);
    //         if (nodeName && isIdentifier(nodeName) && !isGeneratedIdentifier(nodeName)) {
    //             // TODO(rbuckton): Does this need to be parented?
    //             const name = setParent(setTextRange(cloneNode(nodeName), nodeName), nodeName.parent);
    //             emitFlags |= getEmitFlags(nodeName);
    //             if (!allowSourceMaps) emitFlags |= EmitFlags.NoSourceMap;
    //             if (!allowComments) emitFlags |= EmitFlags.NoComments;
    //             if (emitFlags) setEmitFlags(name, emitFlags);
    //             return name;
    //         }
    //         return getGeneratedNameForNode(node);
    //     }

    //     /**
    //      * Gets the internal name of a declaration. This is primarily used for declarations that can be
    //      * referred to by name in the body of an ES5 class function body. An internal name will *never*
    //      * be prefixed with an module or namespace export modifier like "exports." when emitted as an
    //      * expression. An internal name will also *never* be renamed due to a collision with a block
    //      * scoped variable.
    //      *
    //      * @param node The declaration.
    //      * @param allowComments A value indicating whether comments may be emitted for the name.
    //      * @param allowSourceMaps A value indicating whether source maps may be emitted for the name.
    //      */
    //     function getInternalName(node: Declaration, allowComments?: boolean, allowSourceMaps?: boolean) {
    //         return getName(node, allowComments, allowSourceMaps, EmitFlags.LocalName | EmitFlags.InternalName);
    //     }

    //     /**
    //      * Gets the local name of a declaration. This is primarily used for declarations that can be
    //      * referred to by name in the declaration's immediate scope (classes, enums, namespaces). A
    //      * local name will *never* be prefixed with an module or namespace export modifier like
    //      * "exports." when emitted as an expression.
    //      *
    //      * @param node The declaration.
    //      * @param allowComments A value indicating whether comments may be emitted for the name.
    //      * @param allowSourceMaps A value indicating whether source maps may be emitted for the name.
    //      */
    //     function getLocalName(node: Declaration, allowComments?: boolean, allowSourceMaps?: boolean) {
    //         return getName(node, allowComments, allowSourceMaps, EmitFlags.LocalName);
    //     }

    //     /**
    //      * Gets the export name of a declaration. This is primarily used for declarations that can be
    //      * referred to by name in the declaration's immediate scope (classes, enums, namespaces). An
    //      * export name will *always* be prefixed with an module or namespace export modifier like
    //      * `"exports."` when emitted as an expression if the name points to an exported symbol.
    //      *
    //      * @param node The declaration.
    //      * @param allowComments A value indicating whether comments may be emitted for the name.
    //      * @param allowSourceMaps A value indicating whether source maps may be emitted for the name.
    //      */
    //     function getExportName(node: Declaration, allowComments?: boolean, allowSourceMaps?: boolean): Identifier {
    //         return getName(node, allowComments, allowSourceMaps, EmitFlags.ExportName);
    //     }

    //     /**
    //      * Gets the name of a declaration for use in declarations.
    //      *
    //      * @param node The declaration.
    //      * @param allowComments A value indicating whether comments may be emitted for the name.
    //      * @param allowSourceMaps A value indicating whether source maps may be emitted for the name.
    //      */
    //     function getDeclarationName(node: Declaration | undefined, allowComments?: boolean, allowSourceMaps?: boolean) {
    //         return getName(node, allowComments, allowSourceMaps);
    //     }

    //     /**
    //      * Gets a namespace-qualified name for use in expressions.
    //      *
    //      * @param ns The namespace identifier.
    //      * @param name The name.
    //      * @param allowComments A value indicating whether comments may be emitted for the name.
    //      * @param allowSourceMaps A value indicating whether source maps may be emitted for the name.
    //      */
    //     function getNamespaceMemberName(ns: Identifier, name: Identifier, allowComments?: boolean, allowSourceMaps?: boolean): PropertyAccessExpression {
    //         const qualifiedName = createPropertyAccessExpression(ns, nodeIsSynthesized(name) ? name : cloneNode(name));
    //         setTextRange(qualifiedName, name);
    //         let emitFlags: EmitFlags = 0;
    //         if (!allowSourceMaps) emitFlags |= EmitFlags.NoSourceMap;
    //         if (!allowComments) emitFlags |= EmitFlags.NoComments;
    //         if (emitFlags) setEmitFlags(qualifiedName, emitFlags);
    //         return qualifiedName;
    //     }

    //     /**
    //      * Gets the exported name of a declaration for use in expressions.
    //      *
    //      * An exported name will *always* be prefixed with an module or namespace export modifier like
    //      * "exports." if the name points to an exported symbol.
    //      *
    //      * @param ns The namespace identifier.
    //      * @param node The declaration.
    //      * @param allowComments A value indicating whether comments may be emitted for the name.
    //      * @param allowSourceMaps A value indicating whether source maps may be emitted for the name.
    //      */
    //     function getExternalModuleOrNamespaceExportName(ns: Identifier | undefined, node: Declaration, allowComments?: boolean, allowSourceMaps?: boolean): Identifier | PropertyAccessExpression {
    //         if (ns && hasSyntacticModifier(node, ModifierFlags.Export)) {
    //             return getNamespaceMemberName(ns, getName(node), allowComments, allowSourceMaps);
    //         }
    //         return getExportName(node, allowComments, allowSourceMaps);
    //     }

    //     /**
    //      * Copies any necessary standard and custom prologue-directives into target array.
    //      * @param source origin statements array
    //      * @param target result statements array
    //      * @param ensureUseStrict boolean determining whether the function need to add prologue-directives
    //      * @param visitor Optional callback used to visit any custom prologue directives.
    //      */
    //     function copyPrologue(source: readonly Statement[], target: Push<Statement>, ensureUseStrict?: boolean, visitor?: (node: Node) => VisitResult<Node>): number {
    //         const offset = copyStandardPrologue(source, target, ensureUseStrict);
    //         return copyCustomPrologue(source, target, offset, visitor);
    //     }

    //     function isUseStrictPrologue(node: ExpressionStatement): boolean {
    //         return isStringLiteral(node.expression) && node.expression.text === "use strict";
    //     }

    //     function createUseStrictPrologue() {
    //         return startOnNewLine(createExpressionStatement(createStringLiteral("use strict"))) as PrologueDirective;
    //     }

    //     /**
    //      * Copies only the standard (string-expression) prologue-directives into the target statement-array.
    //      * @param source origin statements array
    //      * @param target result statements array
    //      * @param ensureUseStrict boolean determining whether the function need to add prologue-directives
    //      */
    //     function copyStandardPrologue(source: readonly Statement[], target: Push<Statement>, ensureUseStrict?: boolean): number {
    //         Debug.assert(target.length === 0, "Prologue directives should be at the first statement in the target statements array");
    //         let foundUseStrict = false;
    //         let statementOffset = 0;
    //         const numStatements = source.length;
    //         while (statementOffset < numStatements) {
    //             const statement = source[statementOffset];
    //             if (isPrologueDirective(statement)) {
    //                 if (isUseStrictPrologue(statement)) {
    //                     foundUseStrict = true;
    //                 }
    //                 target.push(statement);
    //             }
    //             else {
    //                 break;
    //             }
    //             statementOffset++;
    //         }
    //         if (ensureUseStrict && !foundUseStrict) {
    //             target.push(createUseStrictPrologue());
    //         }
    //         return statementOffset;
    //     }

    //     /**
    //      * Copies only the custom prologue-directives into target statement-array.
    //      * @param source origin statements array
    //      * @param target result statements array
    //      * @param statementOffset The offset at which to begin the copy.
    //      * @param visitor Optional callback used to visit any custom prologue directives.
    //      */
    //     function copyCustomPrologue(source: readonly Statement[], target: Push<Statement>, statementOffset: number, visitor?: (node: Node) => VisitResult<Node>, filter?: (node: Node) => boolean): number;
    //     function copyCustomPrologue(source: readonly Statement[], target: Push<Statement>, statementOffset: number | undefined, visitor?: (node: Node) => VisitResult<Node>, filter?: (node: Node) => boolean): number | undefined;
    //     function copyCustomPrologue(source: readonly Statement[], target: Push<Statement>, statementOffset: number | undefined, visitor?: (node: Node) => VisitResult<Node>, filter: (node: Node) => boolean = returnTrue): number | undefined {
    //         const numStatements = source.length;
    //         while (statementOffset !== undefined && statementOffset < numStatements) {
    //             const statement = source[statementOffset];
    //             if (getEmitFlags(statement) & EmitFlags.CustomPrologue && filter(statement)) {
    //                 append(target, visitor ? visitNode(statement, visitor, isStatement) : statement);
    //             }
    //             else {
    //                 break;
    //             }
    //             statementOffset++;
    //         }
    //         return statementOffset;
    //     }

    //     /**
    //      * Ensures "use strict" directive is added
    //      *
    //      * @param statements An array of statements
    //      */
    //     function ensureUseStrict(statements: NodeArray<Statement>): NodeArray<Statement> {
    //         const foundUseStrict = findUseStrictPrologue(statements);

    //         if (!foundUseStrict) {
    //             return setTextRange(createNodeArray<Statement>([createUseStrictPrologue(), ...statements]), statements);
    //         }

    //         return statements;
    //     }

    //     /**
    //      * Lifts a NodeArray containing only Statement nodes to a block.
    //      *
    //      * @param nodes The NodeArray.
    //      */
    //     function liftToBlock(nodes: readonly Node[]): Statement {
    //         Debug.assert(every(nodes, isStatementOrBlock), "Cannot lift nodes to a Block.");
    //         return singleOrUndefined(nodes) as Statement || createBlock(nodes as readonly Statement[]);
    //     }

    //     function findSpanEnd<T>(array: readonly T[], test: (value: T) => boolean, start: number) {
    //         let i = start;
    //         while (i < array.length && test(array[i])) {
    //             i++;
    //         }
    //         return i;
    //     }

    //     function mergeLexicalEnvironment(statements: NodeArray<Statement>, declarations: readonly Statement[] | undefined): NodeArray<Statement>;
    //     function mergeLexicalEnvironment(statements: Statement[], declarations: readonly Statement[] | undefined): Statement[];
    //     function mergeLexicalEnvironment(statements: Statement[] | NodeArray<Statement>, declarations: readonly Statement[] | undefined) {
    //         if (!some(declarations)) {
    //             return statements;
    //         }

    //         // When we merge new lexical statements into an existing statement list, we merge them in the following manner:
    //         //
    //         // Given:
    //         //
    //         // | Left                               | Right                               |
    //         // |------------------------------------|-------------------------------------|
    //         // | [standard prologues (left)]        | [standard prologues (right)]        |
    //         // | [hoisted functions (left)]         | [hoisted functions (right)]         |
    //         // | [hoisted variables (left)]         | [hoisted variables (right)]         |
    //         // | [lexical init statements (left)]   | [lexical init statements (right)]   |
    //         // | [other statements (left)]          |                                     |
    //         //
    //         // The resulting statement list will be:
    //         //
    //         // | Result                              |
    //         // |-------------------------------------|
    //         // | [standard prologues (right)]        |
    //         // | [standard prologues (left)]         |
    //         // | [hoisted functions (right)]         |
    //         // | [hoisted functions (left)]          |
    //         // | [hoisted variables (right)]         |
    //         // | [hoisted variables (left)]          |
    //         // | [lexical init statements (right)]   |
    //         // | [lexical init statements (left)]    |
    //         // | [other statements (left)]           |
    //         //
    //         // NOTE: It is expected that new lexical init statements must be evaluated before existing lexical init statements,
    //         // as the prior transformation may depend on the evaluation of the lexical init statements to be in the correct state.

    //         // find standard prologues on left in the following order: standard directives, hoisted functions, hoisted variables, other custom
    //         const leftStandardPrologueEnd = findSpanEnd(statements, isPrologueDirective, 0);
    //         const leftHoistedFunctionsEnd = findSpanEnd(statements, isHoistedFunction, leftStandardPrologueEnd);
    //         const leftHoistedVariablesEnd = findSpanEnd(statements, isHoistedVariableStatement, leftHoistedFunctionsEnd);

    //         // find standard prologues on right in the following order: standard directives, hoisted functions, hoisted variables, other custom
    //         const rightStandardPrologueEnd = findSpanEnd(declarations, isPrologueDirective, 0);
    //         const rightHoistedFunctionsEnd = findSpanEnd(declarations, isHoistedFunction, rightStandardPrologueEnd);
    //         const rightHoistedVariablesEnd = findSpanEnd(declarations, isHoistedVariableStatement, rightHoistedFunctionsEnd);
    //         const rightCustomPrologueEnd = findSpanEnd(declarations, isCustomPrologue, rightHoistedVariablesEnd);
    //         Debug.assert(rightCustomPrologueEnd === declarations.length, "Expected declarations to be valid standard or custom prologues");

    //         // splice prologues from the right into the left. We do this in reverse order
    //         // so that we don't need to recompute the index on the left when we insert items.
    //         const left = isNodeArray(statements) ? statements.slice() : statements;

    //         // splice other custom prologues from right into left
    //         if (rightCustomPrologueEnd > rightHoistedVariablesEnd) {
    //             left.splice(leftHoistedVariablesEnd, 0, ...declarations.slice(rightHoistedVariablesEnd, rightCustomPrologueEnd));
    //         }

    //         // splice hoisted variables from right into left
    //         if (rightHoistedVariablesEnd > rightHoistedFunctionsEnd) {
    //             left.splice(leftHoistedFunctionsEnd, 0, ...declarations.slice(rightHoistedFunctionsEnd, rightHoistedVariablesEnd));
    //         }

    //         // splice hoisted functions from right into left
    //         if (rightHoistedFunctionsEnd > rightStandardPrologueEnd) {
    //             left.splice(leftStandardPrologueEnd, 0, ...declarations.slice(rightStandardPrologueEnd, rightHoistedFunctionsEnd));
    //         }

    //         // splice standard prologues from right into left (that are not already in left)
    //         if (rightStandardPrologueEnd > 0) {
    //             if (leftStandardPrologueEnd === 0) {
    //                 left.splice(0, 0, ...declarations.slice(0, rightStandardPrologueEnd));
    //             }
    //             else {
    //                 const leftPrologues = new Map<string, boolean>();
    //                 for (let i = 0; i < leftStandardPrologueEnd; i++) {
    //                     const leftPrologue = statements[i] as PrologueDirective;
    //                     leftPrologues.set(leftPrologue.expression.text, true);
    //                 }
    //                 for (let i = rightStandardPrologueEnd - 1; i >= 0; i--) {
    //                     const rightPrologue = declarations[i] as PrologueDirective;
    //                     if (!leftPrologues.has(rightPrologue.expression.text)) {
    //                         left.unshift(rightPrologue);
    //                     }
    //                 }
    //             }
    //         }

    //         if (isNodeArray(statements)) {
    //             return setTextRange(createNodeArray(left, statements.hasTrailingComma), statements);
    //         }

    //         return statements;
    //     }

    //     function updateModifiers<T extends HasModifiers>(node: T, modifiers: readonly Modifier[] | ModifierFlags): T;
    //     function updateModifiers(node: HasModifiers, modifiers: readonly Modifier[] | ModifierFlags) {
    //         if (typeof modifiers === "number") {
    //             modifiers = createModifiersFromModifierFlags(modifiers);
    //         }
    //         return isParameter(node) ? updateParameterDeclaration(node, node.decorators, modifiers, node.dotDotDotToken, node.name, node.questionToken, node.type, node.initializer) :
    //             isPropertySignature(node) ? updatePropertySignature(node, modifiers, node.name, node.questionToken, node.type) :
    //             isPropertyDeclaration(node) ? updatePropertyDeclaration(node, node.decorators, modifiers, node.name, node.questionToken ?? node.exclamationToken, node.type, node.initializer) :
    //             isMethodSignature(node) ? updateMethodSignature(node, modifiers, node.name, node.questionToken, node.typeParameters, node.parameters, node.type) :
    //             isMethodDeclaration(node) ? updateMethodDeclaration(node, node.decorators, modifiers, node.asteriskToken, node.name, node.questionToken, node.typeParameters, node.parameters, node.type, node.body) :
    //             isConstructorDeclaration(node) ? updateConstructorDeclaration(node, node.decorators, modifiers, node.parameters, node.body) :
    //             isGetAccessorDeclaration(node) ? updateGetAccessorDeclaration(node, node.decorators, modifiers, node.name, node.parameters, node.type, node.body) :
    //             isSetAccessorDeclaration(node) ? updateSetAccessorDeclaration(node, node.decorators, modifiers, node.name, node.parameters, node.body) :
    //             isIndexSignatureDeclaration(node) ? updateIndexSignature(node, node.decorators, modifiers, node.parameters, node.type) :
    //             isFunctionExpression(node) ? updateFunctionExpression(node, modifiers, node.asteriskToken, node.name, node.typeParameters, node.parameters, node.type, node.body) :
    //             isArrowFunction(node) ? updateArrowFunction(node, modifiers, node.typeParameters, node.parameters, node.type, node.equalsGreaterThanToken, node.body) :
    //             isClassExpression(node) ? updateClassExpression(node, node.decorators, modifiers, node.name, node.typeParameters, node.heritageClauses, node.members) :
    //             isVariableStatement(node) ? updateVariableStatement(node, modifiers, node.declarationList) :
    //             isFunctionDeclaration(node) ? updateFunctionDeclaration(node, node.decorators, modifiers, node.asteriskToken, node.name, node.typeParameters, node.parameters, node.type, node.body) :
    //             isClassDeclaration(node) ? updateClassDeclaration(node, node.decorators, modifiers, node.name, node.typeParameters, node.heritageClauses, node.members) :
    //             isInterfaceDeclaration(node) ? updateInterfaceDeclaration(node, node.decorators, modifiers, node.name, node.typeParameters, node.heritageClauses, node.members) :
    //             isTypeAliasDeclaration(node) ? updateTypeAliasDeclaration(node, node.decorators, modifiers, node.name, node.typeParameters, node.type) :
    //             isEnumDeclaration(node) ? updateEnumDeclaration(node, node.decorators, modifiers, node.name, node.members) :
    //             isModuleDeclaration(node) ? updateModuleDeclaration(node, node.decorators, modifiers, node.name, node.body) :
    //             isImportEqualsDeclaration(node) ? updateImportEqualsDeclaration(node, node.decorators, modifiers, node.isTypeOnly, node.name, node.moduleReference) :
    //             isImportDeclaration(node) ? updateImportDeclaration(node, node.decorators, modifiers, node.importClause, node.moduleSpecifier, node.assertClause) :
    //             isExportAssignment(node) ? updateExportAssignment(node, node.decorators, modifiers, node.expression) :
    //             isExportDeclaration(node) ? updateExportDeclaration(node, node.decorators, modifiers, node.isTypeOnly, node.exportClause, node.moduleSpecifier, node.assertClause) :
    //             Debug.assertNever(node);
    //     }

    fn asNodeArray<T: IsNode>(&mut self, array: Option<NodeArray<T>>) -> Option<NodeArray<T>> {
        array.map(|a| self.updateNodeArray(Some(a), None))
    }

    //     function asName<T extends DeclarationName | Identifier | BindingName | PropertyName | NoSubstitutionTemplateLiteral | EntityName | ThisTypeNode | undefined>(name: string | T): T | Identifier {
    //         return typeof name === "string" ? createIdentifier(name) :
    //             name;
    //     }

    //     function asExpression<T extends Expression | undefined>(value: string | number | boolean | T): T | StringLiteral | NumericLiteral | BooleanLiteral {
    //         return typeof value === "string" ? createStringLiteral(value) :
    //             typeof value === "number" ? createNumericLiteral(value) :
    //             typeof value === "boolean" ? value ? createTrue() : createFalse() :
    //             value;
    //     }

    //     function asToken<TKind extends SyntaxKind>(value: TKind | Token<TKind>): Token<TKind> {
    //         return typeof value === "number" ? createToken(value) : value;
    //     }

    fn asEmbeddedStatement(&mut self, statement: Statement) -> Statement {
        self.asEmbeddedStatementOptional(Some(statement)).unwrap()
    }
    fn asEmbeddedStatementOptional(&mut self, statement: Option<Statement>) -> Option<Statement> {
        if let Some(Statement::NotEmittedStatement(statement)) = statement {
            let node = self.createEmptyStatement();
            let node = self.setOriginalNode(node, Some(statement.clone().into()));
            let text_range = *self.node_data(&statement).get_range();
            setTextRange(self.node_data_mut(&node), Some(text_range));
            Some(Statement::EmptyStatement(Rc::new(node)))
        } else {
            statement
        }
    }

    fn propagatePropertyNameFlagsOfChild<T: HasNodeId>(
        &mut self,
        node: T,
        transformFlags: TransformFlags,
    ) -> TransformFlags {
        transformFlags
            | (self.node_data(node).transformFlags & TransformFlags::PropertyNamePropagatingFlags)
    }

    fn propagateChildFlags<T: IsNode>(&mut self, child: Option<&T>) -> TransformFlags {
        let child = match child {
            Some(c) => c,
            None => return TransformFlags::None,
        };
        let childFlags = self.node_data(child).transformFlags
            & !getTransformFlagsSubtreeExclusions(child.kind());
        if let Some(name) = child.name() {
            if name.isPropertyName() {
                return self.propagatePropertyNameFlagsOfChild(name, childFlags);
            }
        }
        childFlags
    }

    fn aggregateChildrenFlags<T: IsNode>(&mut self, children: &mut NodeArray<T>) {
        let mut subtreeFlags = TransformFlags::None;
        for child in children.iter() {
            subtreeFlags |= self.propagateChildFlags(Some(child));
        }
        children.transformFlags = Some(subtreeFlags);
    }
}

// namespace ts {
//     let nextAutoGenerateId = 0;

bitflags! {
    #[derive(Default)]
    pub struct NodeFactoryFlags: u8 {
        const None = 0;
        // Disables the parenthesizer rules for the factory.
        const NoParenthesizerRules = 1 << 0;
        // Disables the node converters for the factory.
        const NoNodeConverters = 1 << 1;
        // Ensures new `PropertyAccessExpression` nodes are created with the `NoIndentation` emit flag set.
        const NoIndentationOnFreshPropertyAccess = 1 << 2;
        // Do not set an `original` pointer when updating a node.
        const NoOriginalNode = 1 << 3;
    }
}

//     function updateWithoutOriginal<T extends Node>(updated: T, original: T): T {
//         if (updated !== original) {
//             setTextRange(updated, original);
//         }
//         return updated;
//     }

//     function updateWithOriginal<T extends Node>(updated: T, original: T): T {
//         if (updated !== original) {
//             setOriginalNode(updated, original);
//             setTextRange(updated, original);
//         }
//         return updated;
//     }

//     function getDefaultTagNameForKind(kind: JSDocTag["kind"]): string {
//         switch (kind) {
//             case SyntaxKind::JSDocTypeTag: return "type";
//             case SyntaxKind::JSDocReturnTag: return "returns";
//             case SyntaxKind::JSDocThisTag: return "this";
//             case SyntaxKind::JSDocEnumTag: return "enum";
//             case SyntaxKind::JSDocAuthorTag: return "author";
//             case SyntaxKind::JSDocClassTag: return "class";
//             case SyntaxKind::JSDocPublicTag: return "public";
//             case SyntaxKind::JSDocPrivateTag: return "private";
//             case SyntaxKind::JSDocProtectedTag: return "protected";
//             case SyntaxKind::JSDocReadonlyTag: return "readonly";
//             case SyntaxKind::JSDocOverrideTag: return "override";
//             case SyntaxKind::JSDocTemplateTag: return "template";
//             case SyntaxKind::JSDocTypedefTag: return "typedef";
//             case SyntaxKind::JSDocParameterTag: return "param";
//             case SyntaxKind::JSDocPropertyTag: return "prop";
//             case SyntaxKind::JSDocCallbackTag: return "callback";
//             case SyntaxKind::JSDocAugmentsTag: return "augments";
//             case SyntaxKind::JSDocImplementsTag: return "implements";
//             default:
//                 return Debug.fail(`Unsupported kind: ${Debug.formatSyntaxKind(kind)}`);
//         }
//     }

//     let rawTextScanner: Scanner | undefined;
//     const invalidValueSentinel: object = { };

//     function getCookedText(kind: TemplateLiteralToken["kind"], rawText: string) {
//         if (!rawTextScanner) {
//             rawTextScanner = createScanner(ScriptTarget.Latest, /*skipTrivia*/ false, LanguageVariant.Standard);
//         }
//         switch (kind) {
//             case SyntaxKind::NoSubstitutionTemplateLiteral:
//                 rawTextScanner.setText("`" + rawText + "`");
//                 break;
//             case SyntaxKind::TemplateHead:
//                 // tslint:disable-next-line no-invalid-template-strings
//                 rawTextScanner.setText("`" + rawText + "${");
//                 break;
//             case SyntaxKind::TemplateMiddle:
//                 // tslint:disable-next-line no-invalid-template-strings
//                 rawTextScanner.setText("}" + rawText + "${");
//                 break;
//             case SyntaxKind::TemplateTail:
//                 rawTextScanner.setText("}" + rawText + "`");
//                 break;
//         }

//         let token = rawTextScanner.scan();
//         if (token === SyntaxKind::CloseBraceToken) {
//             token = rawTextScanner.reScanTemplateToken(/*isTaggedTemplate*/ false);
//         }

//         if (rawTextScanner.isUnterminated()) {
//             rawTextScanner.setText(undefined);
//             return invalidValueSentinel;
//         }

//         let tokenValue: string | undefined;
//         switch (token) {
//             case SyntaxKind::NoSubstitutionTemplateLiteral:
//             case SyntaxKind::TemplateHead:
//             case SyntaxKind::TemplateMiddle:
//             case SyntaxKind::TemplateTail:
//                 tokenValue = rawTextScanner.getTokenValue();
//                 break;
//         }

//         if (tokenValue === undefined || rawTextScanner.scan() !== SyntaxKind::EndOfFileToken) {
//             rawTextScanner.setText(undefined);
//             return invalidValueSentinel;
//         }

//         rawTextScanner.setText(undefined);
//         return tokenValue;
//     }

impl NodeFactory {
    fn propagateIdentifierNameFlags(&mut self, node: &Identifier) -> TransformFlags {
        // An IdentifierName is allowed to be `await`
        self.propagateChildFlags(Some(node)) & !TransformFlags::ContainsPossibleTopLevelAwait
    }
}

fn propagateChildrenFlags<T: IsNode>(children: Option<&NodeArray<T>>) -> TransformFlags {
    children
        .and_then(|c| c.transformFlags)
        .unwrap_or(TransformFlags::None)
}

/**
 * Gets the transform flags to exclude when unioning the transform flags of a subtree.
 */
fn getTransformFlagsSubtreeExclusions(kind: SyntaxKind) -> TransformFlags {
    if kind >= SyntaxKind::FirstTypeNode && kind <= SyntaxKind::LastTypeNode {
        return TransformFlags::TypeExcludes;
    }

    match kind {
        SyntaxKind::CallExpression
        | SyntaxKind::NewExpression
        | SyntaxKind::ArrayLiteralExpression => TransformFlags::ArrayLiteralOrCallOrNewExcludes,
        SyntaxKind::ModuleDeclaration => TransformFlags::ModuleExcludes,
        SyntaxKind::Parameter => TransformFlags::ParameterExcludes,
        SyntaxKind::ArrowFunction => TransformFlags::ArrowFunctionExcludes,
        SyntaxKind::FunctionExpression | SyntaxKind::FunctionDeclaration => {
            TransformFlags::FunctionExcludes
        }
        SyntaxKind::VariableDeclarationList => TransformFlags::VariableDeclarationListExcludes,
        SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => TransformFlags::ClassExcludes,
        SyntaxKind::Constructor => TransformFlags::ConstructorExcludes,
        SyntaxKind::PropertyDeclaration => TransformFlags::PropertyExcludes,
        SyntaxKind::MethodDeclaration | SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
            TransformFlags::MethodOrAccessorExcludes
        }
        SyntaxKind::AnyKeyword
        | SyntaxKind::NumberKeyword
        | SyntaxKind::BigIntKeyword
        | SyntaxKind::NeverKeyword
        | SyntaxKind::StringKeyword
        | SyntaxKind::ObjectKeyword
        | SyntaxKind::BooleanKeyword
        | SyntaxKind::SymbolKeyword
        | SyntaxKind::VoidKeyword
        | SyntaxKind::TypeParameter
        | SyntaxKind::PropertySignature
        | SyntaxKind::MethodSignature
        | SyntaxKind::CallSignature
        | SyntaxKind::ConstructSignature
        | SyntaxKind::IndexSignature
        | SyntaxKind::InterfaceDeclaration
        | SyntaxKind::TypeAliasDeclaration => TransformFlags::TypeExcludes,
        SyntaxKind::ObjectLiteralExpression => TransformFlags::ObjectLiteralExcludes,
        SyntaxKind::CatchClause => TransformFlags::CatchClauseExcludes,
        SyntaxKind::ObjectBindingPattern | SyntaxKind::ArrayBindingPattern => {
            TransformFlags::BindingPatternExcludes
        }
        SyntaxKind::TypeAssertionExpression
        | SyntaxKind::AsExpression
        | SyntaxKind::PartiallyEmittedExpression
        | SyntaxKind::ParenthesizedExpression
        | SyntaxKind::SuperKeyword => TransformFlags::OuterExpressionExcludes,
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
            TransformFlags::PropertyAccessExcludes
        }
        _ => TransformFlags::NodeExcludes,
    }
}

//     const baseFactory = createBaseNodeFactory();

//     function makeSynthetic(node: Node) {
//         (node as Mutable<Node>).flags |= NodeFlags.Synthesized;
//         return node;
//     }

//     const syntheticFactory: BaseNodeFactory = {
//         createBaseSourceFileNode: kind => makeSynthetic(baseFactory.createBaseSourceFileNode(kind)),
//         createBaseIdentifierNode: kind => makeSynthetic(baseFactory.createBaseIdentifierNode(kind)),
//         createBasePrivateIdentifierNode: kind => makeSynthetic(baseFactory.createBasePrivateIdentifierNode(kind)),
//         createBaseTokenNode: kind => makeSynthetic(baseFactory.createBaseTokenNode(kind)),
//         createBaseNode: kind => makeSynthetic(baseFactory.createBaseNode(kind)),
//     };

//     export const factory = createNodeFactory(NodeFactoryFlags.NoIndentationOnFreshPropertyAccess, syntheticFactory);

//     export function createUnparsedSourceFile(text: string): UnparsedSource;
//     export function createUnparsedSourceFile(inputFile: InputFiles, type: "js" | "dts", stripInternal?: boolean): UnparsedSource;
//     export function createUnparsedSourceFile(text: string, mapPath: string | undefined, map: string | undefined): UnparsedSource;
//     export function createUnparsedSourceFile(textOrInputFiles: string | InputFiles, mapPathOrType?: string, mapTextOrStripInternal?: string | boolean): UnparsedSource {
//         let stripInternal: boolean | undefined;
//         let bundleFileInfo: BundleFileInfo | undefined;
//         let fileName: string;
//         let text: string | undefined;
//         let length: number | (() => number);
//         let sourceMapPath: string | undefined;
//         let sourceMapText: string | undefined;
//         let getText: (() => string) | undefined;
//         let getSourceMapText: (() => string | undefined) | undefined;
//         let oldFileOfCurrentEmit: boolean | undefined;

//         if (!isString(textOrInputFiles)) {
//             Debug.assert(mapPathOrType === "js" || mapPathOrType === "dts");
//             fileName = (mapPathOrType === "js" ? textOrInputFiles.javascriptPath : textOrInputFiles.declarationPath) || "";
//             sourceMapPath = mapPathOrType === "js" ? textOrInputFiles.javascriptMapPath : textOrInputFiles.declarationMapPath;
//             getText = () => mapPathOrType === "js" ? textOrInputFiles.javascriptText : textOrInputFiles.declarationText;
//             getSourceMapText = () => mapPathOrType === "js" ? textOrInputFiles.javascriptMapText : textOrInputFiles.declarationMapText;
//             length = () => getText!().length;
//             if (textOrInputFiles.buildInfo && textOrInputFiles.buildInfo.bundle) {
//                 Debug.assert(mapTextOrStripInternal === undefined || typeof mapTextOrStripInternal === "boolean");
//                 stripInternal = mapTextOrStripInternal;
//                 bundleFileInfo = mapPathOrType === "js" ? textOrInputFiles.buildInfo.bundle.js : textOrInputFiles.buildInfo.bundle.dts;
//                 oldFileOfCurrentEmit = textOrInputFiles.oldFileOfCurrentEmit;
//             }
//         }
//         else {
//             fileName = "";
//             text = textOrInputFiles;
//             length = textOrInputFiles.length;
//             sourceMapPath = mapPathOrType;
//             sourceMapText = mapTextOrStripInternal as string;
//         }
//         const node = oldFileOfCurrentEmit ?
//             parseOldFileOfCurrentEmit(Debug.assertDefined(bundleFileInfo)) :
//             parseUnparsedSourceFile(bundleFileInfo, stripInternal, length);
//         node.fileName = fileName;
//         node.sourceMapPath = sourceMapPath;
//         node.oldFileOfCurrentEmit = oldFileOfCurrentEmit;
//         if (getText && getSourceMapText) {
//             Object.defineProperty(node, "text", { get: getText });
//             Object.defineProperty(node, "sourceMapText", { get: getSourceMapText });
//         }
//         else {
//             Debug.assert(!oldFileOfCurrentEmit);
//             node.text = text ?? "";
//             node.sourceMapText = sourceMapText;
//         }

//         return node;
//     }

//     function parseUnparsedSourceFile(bundleFileInfo: BundleFileInfo | undefined, stripInternal: boolean | undefined, length: number | (() => number)) {
//         let prologues: UnparsedPrologue[] | undefined;
//         let helpers: UnscopedEmitHelper[] | undefined;
//         let referencedFiles: FileReference[] | undefined;
//         let typeReferenceDirectives: string[] | undefined;
//         let libReferenceDirectives: FileReference[] | undefined;
//         let prependChildren: UnparsedTextLike[] | undefined;
//         let texts: UnparsedSourceText[] | undefined;
//         let hasNoDefaultLib: boolean | undefined;

//         for (const section of bundleFileInfo ? bundleFileInfo.sections : emptyArray) {
//             switch (section.kind) {
//                 case BundleFileSectionKind.Prologue:
//                     prologues = append(prologues, setTextRange(factory.createUnparsedPrologue(section.data), section));
//                     break;
//                 case BundleFileSectionKind.EmitHelpers:
//                     helpers = append(helpers, getAllUnscopedEmitHelpers().get(section.data)!);
//                     break;
//                 case BundleFileSectionKind.NoDefaultLib:
//                     hasNoDefaultLib = true;
//                     break;
//                 case BundleFileSectionKind.Reference:
//                     referencedFiles = append(referencedFiles, { pos: -1, end: -1, fileName: section.data });
//                     break;
//                 case BundleFileSectionKind.Type:
//                     typeReferenceDirectives = append(typeReferenceDirectives, section.data);
//                     break;
//                 case BundleFileSectionKind.Lib:
//                     libReferenceDirectives = append(libReferenceDirectives, { pos: -1, end: -1, fileName: section.data });
//                     break;
//                 case BundleFileSectionKind.Prepend:
//                     let prependTexts: UnparsedTextLike[] | undefined;
//                     for (const text of section.texts) {
//                         if (!stripInternal || text.kind !== BundleFileSectionKind.Internal) {
//                             prependTexts = append(prependTexts, setTextRange(factory.createUnparsedTextLike(text.data, text.kind === BundleFileSectionKind.Internal), text));
//                         }
//                     }
//                     prependChildren = addRange(prependChildren, prependTexts);
//                     texts = append(texts, factory.createUnparsedPrepend(section.data, prependTexts ?? emptyArray));
//                     break;
//                 case BundleFileSectionKind.Internal:
//                     if (stripInternal) {
//                         if (!texts) texts = [];
//                         break;
//                     }
//                     // falls through

//                 case BundleFileSectionKind.Text:
//                     texts = append(texts, setTextRange(factory.createUnparsedTextLike(section.data, section.kind === BundleFileSectionKind.Internal), section));
//                     break;
//                 default:
//                     Debug.assertNever(section);
//             }
//         }

//         if (!texts) {
//             const textNode = factory.createUnparsedTextLike(/*data*/ undefined, /*internal*/ false);
//             setTextRangePosWidth(textNode, 0, typeof length === "function" ? length() : length);
//             texts = [textNode];
//         }

//         const node = parseNodeFactory.createUnparsedSource(prologues ?? emptyArray, /*syntheticReferences*/ undefined, texts);
//         setEachParent(prologues, node);
//         setEachParent(texts, node);
//         setEachParent(prependChildren, node);
//         node.hasNoDefaultLib = hasNoDefaultLib;
//         node.helpers = helpers;
//         node.referencedFiles = referencedFiles || emptyArray;
//         node.typeReferenceDirectives = typeReferenceDirectives;
//         node.libReferenceDirectives = libReferenceDirectives || emptyArray;
//         return node;
//     }

//     function parseOldFileOfCurrentEmit(bundleFileInfo: BundleFileInfo) {
//         let texts: UnparsedTextLike[] | undefined;
//         let syntheticReferences: UnparsedSyntheticReference[] | undefined;
//         for (const section of bundleFileInfo.sections) {
//             switch (section.kind) {
//                 case BundleFileSectionKind.Internal:
//                 case BundleFileSectionKind.Text:
//                     texts = append(texts, setTextRange(factory.createUnparsedTextLike(section.data, section.kind === BundleFileSectionKind.Internal), section));
//                     break;

//                 case BundleFileSectionKind.NoDefaultLib:
//                 case BundleFileSectionKind.Reference:
//                 case BundleFileSectionKind.Type:
//                 case BundleFileSectionKind.Lib:
//                     syntheticReferences = append(syntheticReferences, setTextRange(factory.createUnparsedSyntheticReference(section), section));
//                     break;

//                 // Ignore
//                 case BundleFileSectionKind.Prologue:
//                 case BundleFileSectionKind.EmitHelpers:
//                 case BundleFileSectionKind.Prepend:
//                     break;

//                 default:
//                     Debug.assertNever(section);
//             }
//         }

//         const node = factory.createUnparsedSource(emptyArray, syntheticReferences, texts ?? emptyArray);
//         setEachParent(syntheticReferences, node);
//         setEachParent(texts, node);
//         node.helpers = map(bundleFileInfo.sources && bundleFileInfo.sources.helpers, name => getAllUnscopedEmitHelpers().get(name)!);
//         return node;
//     }

//     // TODO(rbuckton): Move part of this to factory
//     export function createInputFiles(
//         javascriptText: string,
//         declarationText: string
//     ): InputFiles;
//     export function createInputFiles(
//         readFileText: (path: string) => string | undefined,
//         javascriptPath: string,
//         javascriptMapPath: string | undefined,
//         declarationPath: string,
//         declarationMapPath: string | undefined,
//         buildInfoPath: string | undefined
//     ): InputFiles;
//     export function createInputFiles(
//         javascriptText: string,
//         declarationText: string,
//         javascriptMapPath: string | undefined,
//         javascriptMapText: string | undefined,
//         declarationMapPath: string | undefined,
//         declarationMapText: string | undefined
//     ): InputFiles;
//     /*@internal*/
//     export function createInputFiles(
//         javascriptText: string,
//         declarationText: string,
//         javascriptMapPath: string | undefined,
//         javascriptMapText: string | undefined,
//         declarationMapPath: string | undefined,
//         declarationMapText: string | undefined,
//         javascriptPath: string | undefined,
//         declarationPath: string | undefined,
//         buildInfoPath?: string | undefined,
//         buildInfo?: BuildInfo,
//         oldFileOfCurrentEmit?: boolean
//     ): InputFiles;
//     export function createInputFiles(
//         javascriptTextOrReadFileText: string | ((path: string) => string | undefined),
//         declarationTextOrJavascriptPath: string,
//         javascriptMapPath?: string,
//         javascriptMapTextOrDeclarationPath?: string,
//         declarationMapPath?: string,
//         declarationMapTextOrBuildInfoPath?: string,
//         javascriptPath?: string | undefined,
//         declarationPath?: string | undefined,
//         buildInfoPath?: string | undefined,
//         buildInfo?: BuildInfo,
//         oldFileOfCurrentEmit?: boolean
//     ): InputFiles {
//         const node = parseNodeFactory.createInputFiles();
//         if (!isString(javascriptTextOrReadFileText)) {
//             const cache = new Map<string, string | false>();
//             const textGetter = (path: string | undefined) => {
//                 if (path === undefined) return undefined;
//                 let value = cache.get(path);
//                 if (value === undefined) {
//                     value = javascriptTextOrReadFileText(path);
//                     cache.set(path, value !== undefined ? value : false);
//                 }
//                 return value !== false ? value as string : undefined;
//             };
//             const definedTextGetter = (path: string) => {
//                 const result = textGetter(path);
//                 return result !== undefined ? result : `/* Input file ${path} was missing */\r\n`;
//             };
//             let buildInfo: BuildInfo | false;
//             const getAndCacheBuildInfo = (getText: () => string | undefined) => {
//                 if (buildInfo === undefined) {
//                     const result = getText();
//                     buildInfo = result !== undefined ? getBuildInfo(result) : false;
//                 }
//                 return buildInfo || undefined;
//             };
//             node.javascriptPath = declarationTextOrJavascriptPath;
//             node.javascriptMapPath = javascriptMapPath;
//             node.declarationPath = Debug.assertDefined(javascriptMapTextOrDeclarationPath);
//             node.declarationMapPath = declarationMapPath;
//             node.buildInfoPath = declarationMapTextOrBuildInfoPath;
//             Object.defineProperties(node, {
//                 javascriptText: { get() { return definedTextGetter(declarationTextOrJavascriptPath); } },
//                 javascriptMapText: { get() { return textGetter(javascriptMapPath); } }, // TODO:: if there is inline sourceMap in jsFile, use that
//                 declarationText: { get() { return definedTextGetter(Debug.assertDefined(javascriptMapTextOrDeclarationPath)); } },
//                 declarationMapText: { get() { return textGetter(declarationMapPath); } }, // TODO:: if there is inline sourceMap in dtsFile, use that
//                 buildInfo: { get() { return getAndCacheBuildInfo(() => textGetter(declarationMapTextOrBuildInfoPath)); } }
//             });
//         }
//         else {
//             node.javascriptText = javascriptTextOrReadFileText;
//             node.javascriptMapPath = javascriptMapPath;
//             node.javascriptMapText = javascriptMapTextOrDeclarationPath;
//             node.declarationText = declarationTextOrJavascriptPath;
//             node.declarationMapPath = declarationMapPath;
//             node.declarationMapText = declarationMapTextOrBuildInfoPath;
//             node.javascriptPath = javascriptPath;
//             node.declarationPath = declarationPath;
//             node.buildInfoPath = buildInfoPath;
//             node.buildInfo = buildInfo;
//             node.oldFileOfCurrentEmit = oldFileOfCurrentEmit;
//         }
//         return node;
//     }

//     // tslint:disable-next-line variable-name
//     let SourceMapSource: new (fileName: string, text: string, skipTrivia?: (pos: number) => number) => SourceMapSource;

//     /**
//      * Create an external source map source file reference
//      */
//     export function createSourceMapSource(fileName: string, text: string, skipTrivia?: (pos: number) => number): SourceMapSource {
//         return new (SourceMapSource || (SourceMapSource = objectAllocator.getSourceMapSourceConstructor()))(fileName, text, skipTrivia);
//     }

// Utilities

impl NodeFactory {
    pub fn setOriginalNode<T: IsNode>(&mut self, node: T, original: Option<Node>) -> T {
        self.node_data_mut(&node).original = original.clone();
        if let Some(original) = original {
            if let Some(emitNode) = &self.node_data(original).emitNode {
                todo!();
                // self.node_data_mut(node).emitNode = self.mergeEmitNode(emitNode, node.emitNode);
            }
        }
        node
    }
}

//     function mergeEmitNode(sourceEmitNode: EmitNode, destEmitNode: EmitNode | undefined) {
//         const {
//             flags,
//             leadingComments,
//             trailingComments,
//             commentRange,
//             sourceMapRange,
//             tokenSourceMapRanges,
//             constantValue,
//             helpers,
//             startsOnNewLine,
//         } = sourceEmitNode;
//         if (!destEmitNode) destEmitNode = {} as EmitNode;
//         // We are using `.slice()` here in case `destEmitNode.leadingComments` is pushed to later.
//         if (leadingComments) destEmitNode.leadingComments = addRange(leadingComments.slice(), destEmitNode.leadingComments);
//         if (trailingComments) destEmitNode.trailingComments = addRange(trailingComments.slice(), destEmitNode.trailingComments);
//         if (flags) destEmitNode.flags = flags & ~EmitFlags.Immutable;
//         if (commentRange) destEmitNode.commentRange = commentRange;
//         if (sourceMapRange) destEmitNode.sourceMapRange = sourceMapRange;
//         if (tokenSourceMapRanges) destEmitNode.tokenSourceMapRanges = mergeTokenSourceMapRanges(tokenSourceMapRanges, destEmitNode.tokenSourceMapRanges!);
//         if (constantValue !== undefined) destEmitNode.constantValue = constantValue;
//         if (helpers) {
//             for (const helper of helpers) {
//                 destEmitNode.helpers = appendIfUnique(destEmitNode.helpers, helper);
//             }
//         }
//         if (startsOnNewLine !== undefined) destEmitNode.startsOnNewLine = startsOnNewLine;
//         return destEmitNode;
//     }

//     function mergeTokenSourceMapRanges(sourceRanges: (TextRange | undefined)[], destRanges: (TextRange | undefined)[]) {
//         if (!destRanges) destRanges = [];
//         for (const key in sourceRanges) {
//             destRanges[key] = sourceRanges[key];
//         }
//         return destRanges;
//     }
// }
