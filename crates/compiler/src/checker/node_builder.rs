use super::*;

impl Checker {
    //   return {
    //       typeToTypeNode: (type: Type, enclosingDeclaration?: Node, flags?: NodeBuilderFlags, tracker?: SymbolTracker) =>
    //           withContext(enclosingDeclaration, flags, tracker, context => typeToTypeNodeHelper(type, context)),
    //       indexInfoToIndexSignatureDeclaration: (indexInfo: IndexInfo, enclosingDeclaration?: Node, flags?: NodeBuilderFlags, tracker?: SymbolTracker) =>
    //           withContext(enclosingDeclaration, flags, tracker, context => indexInfoToIndexSignatureDeclarationHelper(indexInfo, context, /*typeNode*/ undefined)),
    //       signatureToSignatureDeclaration: (signature: Signature, kind: SignatureDeclaration["kind"], enclosingDeclaration?: Node, flags?: NodeBuilderFlags, tracker?: SymbolTracker) =>
    //           withContext(enclosingDeclaration, flags, tracker, context => signatureToSignatureDeclarationHelper(signature, kind, context)),
    //       symbolToEntityName: (symbol: Symbol, meaning: SymbolFlags, enclosingDeclaration?: Node, flags?: NodeBuilderFlags, tracker?: SymbolTracker) =>
    //           withContext(enclosingDeclaration, flags, tracker, context => symbolToName(symbol, context, meaning, /*expectsIdentifier*/ false)),
    //       symbolToExpression: (symbol: Symbol, meaning: SymbolFlags, enclosingDeclaration?: Node, flags?: NodeBuilderFlags, tracker?: SymbolTracker) =>
    //           withContext(enclosingDeclaration, flags, tracker, context => symbolToExpression(symbol, context, meaning)),
    //       symbolToTypeParameterDeclarations: (symbol: Symbol, enclosingDeclaration?: Node, flags?: NodeBuilderFlags, tracker?: SymbolTracker) =>
    //           withContext(enclosingDeclaration, flags, tracker, context => typeParametersToTypeParameterDeclarations(symbol, context)),
    //       symbolToParameterDeclaration: (symbol: Symbol, enclosingDeclaration?: Node, flags?: NodeBuilderFlags, tracker?: SymbolTracker) =>
    //           withContext(enclosingDeclaration, flags, tracker, context => symbolToParameterDeclaration(symbol, context)),
    //       typeParameterToDeclaration: (parameter: TypeParameter, enclosingDeclaration?: Node, flags?: NodeBuilderFlags, tracker?: SymbolTracker) =>
    //           withContext(enclosingDeclaration, flags, tracker, context => typeParameterToDeclaration(parameter, context)),
    //       symbolTableToDeclarationStatements: (symbolTable: SymbolTable, enclosingDeclaration?: Node, flags?: NodeBuilderFlags, tracker?: SymbolTracker, bundled?: boolean) =>
    //           withContext(enclosingDeclaration, flags, tracker, context => symbolTableToDeclarationStatements(symbolTable, context, bundled)),
    //   };

    //   function wrapSymbolTrackerToReportForContext(context: NodeBuilderContext, tracker: SymbolTracker): SymbolTracker {
    //       const oldTrackSymbol = tracker.trackSymbol;
    //       return {
    //           ...tracker,
    //           reportCyclicStructureError: wrapReportedDiagnostic(tracker.reportCyclicStructureError),
    //           reportInaccessibleThisError: wrapReportedDiagnostic(tracker.reportInaccessibleThisError),
    //           reportInaccessibleUniqueSymbolError: wrapReportedDiagnostic(tracker.reportInaccessibleUniqueSymbolError),
    //           reportLikelyUnsafeImportRequiredError: wrapReportedDiagnostic(tracker.reportLikelyUnsafeImportRequiredError),
    //           reportNonlocalAugmentation: wrapReportedDiagnostic(tracker.reportNonlocalAugmentation),
    //           reportPrivateInBaseOfClassExpression: wrapReportedDiagnostic(tracker.reportPrivateInBaseOfClassExpression),
    //           reportNonSerializableProperty: wrapReportedDiagnostic(tracker.reportNonSerializableProperty),
    //           trackSymbol: oldTrackSymbol && ((...args) => {
    //               const result = oldTrackSymbol(...args);
    //               if (result) {
    //                   context.reportedDiagnostic = true;
    //               }
    //               return result;
    //           }),
    //       };

    //       function wrapReportedDiagnostic<T extends (...args: any[]) => any>(method: T | undefined): T | undefined {
    //           if (!method) {
    //               return method;
    //           }
    //           return (((...args) => {
    //               context.reportedDiagnostic = true;
    //               return method(...args);
    //           }) as T);
    //       }
    //   }

    //   function checkTruncationLength(context: NodeBuilderContext): boolean {
    //       if (context.truncating) return context.truncating;
    //       return context.truncating = context.approximateLength > ((context.flags & NodeBuilderFlags.NoTruncation) ? noTruncationMaximumTruncationLength : defaultMaximumTruncationLength);
    //   }

    //   function typeToTypeNodeHelper(type: Type, context: NodeBuilderContext): TypeNode {
    //       if (cancellationToken && cancellationToken.throwIfCancellationRequested) {
    //           cancellationToken.throwIfCancellationRequested();
    //       }
    //       const inTypeAlias = context.flags & NodeBuilderFlags.InTypeAlias;
    //       context.flags &= ~NodeBuilderFlags.InTypeAlias;

    //       if (!type) {
    //           if (!(context.flags & NodeBuilderFlags.AllowEmptyUnionOrIntersection)) {
    //               context.encounteredError = true;
    //               return undefined!; // TODO: GH#18217
    //           }
    //           context.approximateLength += 3;
    //           return factory.createKeywordTypeNode(SyntaxKind.AnyKeyword);
    //       }

    //       if (!(context.flags & NodeBuilderFlags.NoTypeReduction)) {
    //           type = getReducedType(type);
    //       }

    //       if (type.flags & TypeFlags.Any) {
    //           if (type.aliasSymbol) {
    //               return factory.createTypeReferenceNode(symbolToEntityNameNode(type.aliasSymbol), mapToTypeNodes(type.aliasTypeArguments, context));
    //           }
    //           if (type === unresolvedType) {
    //               return addSyntheticLeadingComment(factory.createKeywordTypeNode(SyntaxKind.AnyKeyword), SyntaxKind.MultiLineCommentTrivia, "unresolved");
    //           }
    //           context.approximateLength += 3;
    //           return factory.createKeywordTypeNode(type === intrinsicMarkerType ? SyntaxKind.IntrinsicKeyword : SyntaxKind.AnyKeyword);
    //       }
    //       if (type.flags & TypeFlags.Unknown) {
    //           return factory.createKeywordTypeNode(SyntaxKind.UnknownKeyword);
    //       }
    //       if (type.flags & TypeFlags.String) {
    //           context.approximateLength += 6;
    //           return factory.createKeywordTypeNode(SyntaxKind.StringKeyword);
    //       }
    //       if (type.flags & TypeFlags.Number) {
    //           context.approximateLength += 6;
    //           return factory.createKeywordTypeNode(SyntaxKind.NumberKeyword);
    //       }
    //       if (type.flags & TypeFlags.BigInt) {
    //           context.approximateLength += 6;
    //           return factory.createKeywordTypeNode(SyntaxKind.BigIntKeyword);
    //       }
    //       if (type.flags & TypeFlags.Boolean && !type.aliasSymbol) {
    //           context.approximateLength += 7;
    //           return factory.createKeywordTypeNode(SyntaxKind.BooleanKeyword);
    //       }
    //       if (type.flags & TypeFlags.EnumLiteral && !(type.flags & TypeFlags.Union)) {
    //           const parentSymbol = getParentOfSymbol(type.symbol)!;
    //           const parentName = symbolToTypeNode(parentSymbol, context, SymbolFlags.Type);
    //           if (getDeclaredTypeOfSymbol(parentSymbol) === type) {
    //               return parentName;
    //           }
    //           const memberName = symbolName(type.symbol);
    //           if (isIdentifierText(memberName, ScriptTarget.ES3)) {
    //               return appendReferenceToType(
    //                   parentName as TypeReferenceNode | ImportTypeNode,
    //                   factory.createTypeReferenceNode(memberName, /*typeArguments*/ undefined)
    //               );
    //           }
    //           if (isImportTypeNode(parentName)) {
    //               (parentName as any).isTypeOf = true; // mutably update, node is freshly manufactured anyhow
    //               return factory.createIndexedAccessTypeNode(parentName, factory.createLiteralTypeNode(factory.createStringLiteral(memberName)));
    //           }
    //           else if (isTypeReferenceNode(parentName)) {
    //               return factory.createIndexedAccessTypeNode(factory.createTypeQueryNode(parentName.typeName), factory.createLiteralTypeNode(factory.createStringLiteral(memberName)));
    //           }
    //           else {
    //               return Debug.fail("Unhandled type node kind returned from `symbolToTypeNode`.");
    //           }
    //       }
    //       if (type.flags & TypeFlags.EnumLike) {
    //           return symbolToTypeNode(type.symbol, context, SymbolFlags.Type);
    //       }
    //       if (type.flags & TypeFlags.StringLiteral) {
    //           context.approximateLength += ((type as StringLiteralType).value.length + 2);
    //           return factory.createLiteralTypeNode(setEmitFlags(factory.createStringLiteral((type as StringLiteralType).value, !!(context.flags & NodeBuilderFlags.UseSingleQuotesForStringLiteralType)), EmitFlags.NoAsciiEscaping));
    //       }
    //       if (type.flags & TypeFlags.NumberLiteral) {
    //           const value = (type as NumberLiteralType).value;
    //           context.approximateLength += ("" + value).length;
    //           return factory.createLiteralTypeNode(value < 0 ? factory.createPrefixUnaryExpression(SyntaxKind.MinusToken, factory.createNumericLiteral(-value)) : factory.createNumericLiteral(value));
    //       }
    //       if (type.flags & TypeFlags.BigIntLiteral) {
    //           context.approximateLength += (pseudoBigIntToString((type as BigIntLiteralType).value).length) + 1;
    //           return factory.createLiteralTypeNode((factory.createBigIntLiteral((type as BigIntLiteralType).value)));
    //       }
    //       if (type.flags & TypeFlags.BooleanLiteral) {
    //           context.approximateLength += (type as IntrinsicType).intrinsicName.length;
    //           return factory.createLiteralTypeNode((type as IntrinsicType).intrinsicName === "true" ? factory.createTrue() : factory.createFalse());
    //       }
    //       if (type.flags & TypeFlags.UniqueESSymbol) {
    //           if (!(context.flags & NodeBuilderFlags.AllowUniqueESSymbolType)) {
    //               if (isValueSymbolAccessible(type.symbol, context.enclosingDeclaration)) {
    //                   context.approximateLength += 6;
    //                   return symbolToTypeNode(type.symbol, context, SymbolFlags.Value);
    //               }
    //               if (context.tracker.reportInaccessibleUniqueSymbolError) {
    //                   context.tracker.reportInaccessibleUniqueSymbolError();
    //               }
    //           }
    //           context.approximateLength += 13;
    //           return factory.createTypeOperatorNode(SyntaxKind.UniqueKeyword, factory.createKeywordTypeNode(SyntaxKind.SymbolKeyword));
    //       }
    //       if (type.flags & TypeFlags.Void) {
    //           context.approximateLength += 4;
    //           return factory.createKeywordTypeNode(SyntaxKind.VoidKeyword);
    //       }
    //       if (type.flags & TypeFlags.Undefined) {
    //           context.approximateLength += 9;
    //           return factory.createKeywordTypeNode(SyntaxKind.UndefinedKeyword);
    //       }
    //       if (type.flags & TypeFlags.Null) {
    //           context.approximateLength += 4;
    //           return factory.createLiteralTypeNode(factory.createNull());
    //       }
    //       if (type.flags & TypeFlags.Never) {
    //           context.approximateLength += 5;
    //           return factory.createKeywordTypeNode(SyntaxKind.NeverKeyword);
    //       }
    //       if (type.flags & TypeFlags.ESSymbol) {
    //           context.approximateLength += 6;
    //           return factory.createKeywordTypeNode(SyntaxKind.SymbolKeyword);
    //       }
    //       if (type.flags & TypeFlags.NonPrimitive) {
    //           context.approximateLength += 6;
    //           return factory.createKeywordTypeNode(SyntaxKind.ObjectKeyword);
    //       }
    //       if (isThisTypeParameter(type)) {
    //           if (context.flags & NodeBuilderFlags.InObjectTypeLiteral) {
    //               if (!context.encounteredError && !(context.flags & NodeBuilderFlags.AllowThisInObjectLiteral)) {
    //                   context.encounteredError = true;
    //               }
    //               if (context.tracker.reportInaccessibleThisError) {
    //                   context.tracker.reportInaccessibleThisError();
    //               }
    //           }
    //           context.approximateLength += 4;
    //           return factory.createThisTypeNode();
    //       }

    //       if (!inTypeAlias && type.aliasSymbol && (context.flags & NodeBuilderFlags.UseAliasDefinedOutsideCurrentScope || isTypeSymbolAccessible(type.aliasSymbol, context.enclosingDeclaration))) {
    //           const typeArgumentNodes = mapToTypeNodes(type.aliasTypeArguments, context);
    //           if (isReservedMemberName(type.aliasSymbol.escapedName) && !(type.aliasSymbol.flags & SymbolFlags.Class)) return factory.createTypeReferenceNode(factory.createIdentifier(""), typeArgumentNodes);
    //           return symbolToTypeNode(type.aliasSymbol, context, SymbolFlags.Type, typeArgumentNodes);
    //       }

    //       const objectFlags = getObjectFlags(type);

    //       if (objectFlags & ObjectFlags.Reference) {
    //           Debug.assert(!!(type.flags & TypeFlags.Object));
    //           return (type as TypeReference).node ? visitAndTransformType(type, typeReferenceToTypeNode) : typeReferenceToTypeNode(type as TypeReference);
    //       }
    //       if (type.flags & TypeFlags.TypeParameter || objectFlags & ObjectFlags.ClassOrInterface) {
    //           if (type.flags & TypeFlags.TypeParameter && contains(context.inferTypeParameters, type)) {
    //               context.approximateLength += (symbolName(type.symbol).length + 6);
    //               return factory.createInferTypeNode(typeParameterToDeclarationWithConstraint(type as TypeParameter, context, /*constraintNode*/ undefined));
    //           }
    //           if (context.flags & NodeBuilderFlags.GenerateNamesForShadowedTypeParams &&
    //               type.flags & TypeFlags.TypeParameter &&
    //               !isTypeSymbolAccessible(type.symbol, context.enclosingDeclaration)) {
    //               const name = typeParameterToName(type, context);
    //               context.approximateLength += idText(name).length;
    //               return factory.createTypeReferenceNode(factory.createIdentifier(idText(name)), /*typeArguments*/ undefined);
    //           }
    //           // Ignore constraint/default when creating a usage (as opposed to declaration) of a type parameter.
    //           return type.symbol
    //               ? symbolToTypeNode(type.symbol, context, SymbolFlags.Type)
    //               : factory.createTypeReferenceNode(factory.createIdentifier("?"), /*typeArguments*/ undefined);
    //       }
    //       if (type.flags & TypeFlags.Union && (type as UnionType).origin) {
    //           type = (type as UnionType).origin!;
    //       }
    //       if (type.flags & (TypeFlags.Union | TypeFlags.Intersection)) {
    //           const types = type.flags & TypeFlags.Union ? formatUnionTypes((type as UnionType).types) : (type as IntersectionType).types;
    //           if (length(types) === 1) {
    //               return typeToTypeNodeHelper(types[0], context);
    //           }
    //           const typeNodes = mapToTypeNodes(types, context, /*isBareList*/ true);
    //           if (typeNodes && typeNodes.length > 0) {
    //               return type.flags & TypeFlags.Union ? factory.createUnionTypeNode(typeNodes) : factory.createIntersectionTypeNode(typeNodes);
    //           }
    //           else {
    //               if (!context.encounteredError && !(context.flags & NodeBuilderFlags.AllowEmptyUnionOrIntersection)) {
    //                   context.encounteredError = true;
    //               }
    //               return undefined!; // TODO: GH#18217
    //           }
    //       }
    //       if (objectFlags & (ObjectFlags.Anonymous | ObjectFlags.Mapped)) {
    //           Debug.assert(!!(type.flags & TypeFlags.Object));
    //           // The type is an object literal type.
    //           return createAnonymousTypeNode(type as ObjectType);
    //       }
    //       if (type.flags & TypeFlags.Index) {
    //           const indexedType = (type as IndexType).type;
    //           context.approximateLength += 6;
    //           const indexTypeNode = typeToTypeNodeHelper(indexedType, context);
    //           return factory.createTypeOperatorNode(SyntaxKind.KeyOfKeyword, indexTypeNode);
    //       }
    //       if (type.flags & TypeFlags.TemplateLiteral) {
    //           const texts = (type as TemplateLiteralType).texts;
    //           const types = (type as TemplateLiteralType).types;
    //           const templateHead = factory.createTemplateHead(texts[0]);
    //           const templateSpans = factory.createNodeArray(
    //               map(types, (t, i) => factory.createTemplateLiteralTypeSpan(
    //                   typeToTypeNodeHelper(t, context),
    //                   (i < types.length - 1 ? factory.createTemplateMiddle : factory.createTemplateTail)(texts[i + 1]))));
    //           context.approximateLength += 2;
    //           return factory.createTemplateLiteralType(templateHead, templateSpans);
    //       }
    //       if (type.flags & TypeFlags.StringMapping) {
    //           const typeNode = typeToTypeNodeHelper((type as StringMappingType).type, context);
    //           return symbolToTypeNode((type as StringMappingType).symbol, context, SymbolFlags.Type, [typeNode]);
    //       }
    //       if (type.flags & TypeFlags.IndexedAccess) {
    //           const objectTypeNode = typeToTypeNodeHelper((type as IndexedAccessType).objectType, context);
    //           const indexTypeNode = typeToTypeNodeHelper((type as IndexedAccessType).indexType, context);
    //           context.approximateLength += 2;
    //           return factory.createIndexedAccessTypeNode(objectTypeNode, indexTypeNode);
    //       }
    //       if (type.flags & TypeFlags.Conditional) {
    //           return visitAndTransformType(type, type => conditionalTypeToTypeNode(type as ConditionalType));
    //       }
    //       if (type.flags & TypeFlags.Substitution) {
    //           return typeToTypeNodeHelper((type as SubstitutionType).baseType, context);
    //       }

    //       return Debug.fail("Should be unreachable.");

    //       function conditionalTypeToTypeNode(type: ConditionalType) {
    //           const checkTypeNode = typeToTypeNodeHelper(type.checkType, context);
    //           const saveInferTypeParameters = context.inferTypeParameters;
    //           context.inferTypeParameters = type.root.inferTypeParameters;
    //           const extendsTypeNode = typeToTypeNodeHelper(type.extendsType, context);
    //           context.inferTypeParameters = saveInferTypeParameters;
    //           const trueTypeNode = typeToTypeNodeOrCircularityElision(getTrueTypeFromConditionalType(type));
    //           const falseTypeNode = typeToTypeNodeOrCircularityElision(getFalseTypeFromConditionalType(type));
    //           context.approximateLength += 15;
    //           return factory.createConditionalTypeNode(checkTypeNode, extendsTypeNode, trueTypeNode, falseTypeNode);
    //       }

    //       function typeToTypeNodeOrCircularityElision(type: Type) {
    //           if (type.flags & TypeFlags.Union) {
    //               if (context.visitedTypes?.has(getTypeId(type))) {
    //                   if (!(context.flags & NodeBuilderFlags.AllowAnonymousIdentifier)) {
    //                       context.encounteredError = true;
    //                       context.tracker?.reportCyclicStructureError?.();
    //                   }
    //                   return createElidedInformationPlaceholder(context);
    //               }
    //               return visitAndTransformType(type, type => typeToTypeNodeHelper(type, context));
    //           }
    //           return typeToTypeNodeHelper(type, context);
    //       }

    //       function createMappedTypeNodeFromType(type: MappedType) {
    //           Debug.assert(!!(type.flags & TypeFlags.Object));
    //           const readonlyToken = type.declaration.readonlyToken ? factory.createToken(type.declaration.readonlyToken.kind) as ReadonlyKeyword | PlusToken | MinusToken : undefined;
    //           const questionToken = type.declaration.questionToken ? factory.createToken(type.declaration.questionToken.kind) as QuestionToken | PlusToken | MinusToken : undefined;
    //           let appropriateConstraintTypeNode: TypeNode;
    //           if (isMappedTypeWithKeyofConstraintDeclaration(type)) {
    //               // We have a { [P in keyof T]: X }
    //               // We do this to ensure we retain the toplevel keyof-ness of the type which may be lost due to keyof distribution during `getConstraintTypeFromMappedType`
    //               appropriateConstraintTypeNode = factory.createTypeOperatorNode(SyntaxKind.KeyOfKeyword, typeToTypeNodeHelper(getModifiersTypeFromMappedType(type), context));
    //           }
    //           else {
    //               appropriateConstraintTypeNode = typeToTypeNodeHelper(getConstraintTypeFromMappedType(type), context);
    //           }
    //           const typeParameterNode = typeParameterToDeclarationWithConstraint(getTypeParameterFromMappedType(type), context, appropriateConstraintTypeNode);
    //           const nameTypeNode = type.declaration.nameType ? typeToTypeNodeHelper(getNameTypeFromMappedType(type)!, context) : undefined;
    //           const templateTypeNode = typeToTypeNodeHelper(removeMissingType(getTemplateTypeFromMappedType(type), !!(getMappedTypeModifiers(type) & MappedTypeModifiers.IncludeOptional)), context);
    //           const mappedTypeNode = factory.createMappedTypeNode(readonlyToken, typeParameterNode, nameTypeNode, questionToken, templateTypeNode);
    //           context.approximateLength += 10;
    //           return setEmitFlags(mappedTypeNode, EmitFlags.SingleLine);
    //       }

    //       function createAnonymousTypeNode(type: ObjectType): TypeNode {
    //           const typeId = type.id;
    //           const symbol = type.symbol;
    //           if (symbol) {
    //               const isInstanceType = isClassInstanceSide(type) ? SymbolFlags.Type : SymbolFlags.Value;
    //               if (isJSConstructor(symbol.valueDeclaration)) {
    //                   // Instance and static types share the same symbol; only add 'typeof' for the static side.
    //                   return symbolToTypeNode(symbol, context, isInstanceType);
    //               }
    //               // Always use 'typeof T' for type of class, enum, and module objects
    //               else if (symbol.flags & SymbolFlags.Class
    //                   && !getBaseTypeVariableOfClass(symbol)
    //                   && !(symbol.valueDeclaration && symbol.valueDeclaration.kind === SyntaxKind.ClassExpression && context.flags & NodeBuilderFlags.WriteClassExpressionAsTypeLiteral) ||
    //                   symbol.flags & (SymbolFlags.Enum | SymbolFlags.ValueModule) ||
    //                   shouldWriteTypeOfFunctionSymbol()) {
    //                   return symbolToTypeNode(symbol, context, isInstanceType);
    //               }
    //               else if (context.visitedTypes?.has(typeId)) {
    //                   // If type is an anonymous type literal in a type alias declaration, use type alias name
    //                   const typeAlias = getTypeAliasForTypeLiteral(type);
    //                   if (typeAlias) {
    //                       // The specified symbol flags need to be reinterpreted as type flags
    //                       return symbolToTypeNode(typeAlias, context, SymbolFlags.Type);
    //                   }
    //                   else {
    //                       return createElidedInformationPlaceholder(context);
    //                   }
    //               }
    //               else {
    //                   return visitAndTransformType(type, createTypeNodeFromObjectType);
    //               }
    //           }
    //           else {
    //               // Anonymous types without a symbol are never circular.
    //               return createTypeNodeFromObjectType(type);
    //           }
    //           function shouldWriteTypeOfFunctionSymbol() {
    //               const isStaticMethodSymbol = !!(symbol.flags & SymbolFlags.Method) &&  // typeof static method
    //                   some(symbol.declarations, declaration => isStatic(declaration));
    //               const isNonLocalFunctionSymbol = !!(symbol.flags & SymbolFlags.Function) &&
    //                   (symbol.parent || // is exported function symbol
    //                       forEach(symbol.declarations, declaration =>
    //                           declaration.parent.kind === SyntaxKind.SourceFile || declaration.parent.kind === SyntaxKind.ModuleBlock));
    //               if (isStaticMethodSymbol || isNonLocalFunctionSymbol) {
    //                   // typeof is allowed only for static/non local functions
    //                   return (!!(context.flags & NodeBuilderFlags.UseTypeOfFunction) || (context.visitedTypes?.has(typeId))) && // it is type of the symbol uses itself recursively
    //                       (!(context.flags & NodeBuilderFlags.UseStructuralFallback) || isValueSymbolAccessible(symbol, context.enclosingDeclaration)); // And the build is going to succeed without visibility error or there is no structural fallback allowed
    //               }
    //           }
    //       }

    //       function visitAndTransformType<T extends TypeNode>(type: Type, transform: (type: Type) => T) {
    //           const typeId = type.id;
    //           const isConstructorObject = getObjectFlags(type) & ObjectFlags.Anonymous && type.symbol && type.symbol.flags & SymbolFlags.Class;
    //           const id = getObjectFlags(type) & ObjectFlags.Reference && (type as TypeReference).node ? "N" + getNodeId((type as TypeReference).node!) :
    //               type.flags & TypeFlags.Conditional ? "N" + getNodeId((type as ConditionalType).root.node) :
    //               type.symbol ? (isConstructorObject ? "+" : "") + getSymbolId(type.symbol) :
    //               undefined;
    //           // Since instantiations of the same anonymous type have the same symbol, tracking symbols instead
    //           // of types allows us to catch circular references to instantiations of the same anonymous type
    //           if (!context.visitedTypes) {
    //               context.visitedTypes = new Set();
    //           }
    //           if (id && !context.symbolDepth) {
    //               context.symbolDepth = new Map();
    //           }

    //           const links = context.enclosingDeclaration && getNodeLinks(context.enclosingDeclaration);
    //           const key = `${getTypeId(type)}|${context.flags}`;
    //           if (links) {
    //               links.serializedTypes ||= new Map();
    //           }
    //           const cachedResult = links?.serializedTypes?.get(key);
    //           if (cachedResult) {
    //               if (cachedResult.truncating) {
    //                   context.truncating = true;
    //               }
    //               context.approximateLength += cachedResult.addedLength;
    //               return deepCloneOrReuseNode(cachedResult) as TypeNode as T;
    //           }

    //           let depth: number | undefined;
    //           if (id) {
    //               depth = context.symbolDepth!.get(id) || 0;
    //               if (depth > 10) {
    //                   return createElidedInformationPlaceholder(context);
    //               }
    //               context.symbolDepth!.set(id, depth + 1);
    //           }
    //           context.visitedTypes.add(typeId);
    //           const startLength = context.approximateLength;
    //           const result = transform(type);
    //           const addedLength = context.approximateLength - startLength;
    //           if (!context.reportedDiagnostic && !context.encounteredError) {
    //               if (context.truncating) {
    //                   (result as any).truncating = true;
    //               }
    //               (result as any).addedLength = addedLength;
    //               links?.serializedTypes?.set(key, result as TypeNode as TypeNode & {truncating?: boolean, addedLength: number});
    //           }
    //           context.visitedTypes.delete(typeId);
    //           if (id) {
    //               context.symbolDepth!.set(id, depth!);
    //           }
    //           return result;

    //           function deepCloneOrReuseNode(node: Node): Node {
    //               if (!nodeIsSynthesized(node) && getParseTreeNode(node) === node) {
    //                   return node;
    //               }
    //               return setTextRange(factory.cloneNode(visitEachChild(node, deepCloneOrReuseNode, nullTransformationContext)), node);
    //           }
    //       }

    //       function createTypeNodeFromObjectType(type: ObjectType): TypeNode {
    //           if (isGenericMappedType(type) || (type as MappedType).containsError) {
    //               return createMappedTypeNodeFromType(type as MappedType);
    //           }

    //           const resolved = resolveStructuredTypeMembers(type);
    //           if (!resolved.properties.length && !resolved.indexInfos.length) {
    //               if (!resolved.callSignatures.length && !resolved.constructSignatures.length) {
    //                   context.approximateLength += 2;
    //                   return setEmitFlags(factory.createTypeLiteralNode(/*members*/ undefined), EmitFlags.SingleLine);
    //               }

    //               if (resolved.callSignatures.length === 1 && !resolved.constructSignatures.length) {
    //                   const signature = resolved.callSignatures[0];
    //                   const signatureNode = signatureToSignatureDeclarationHelper(signature, SyntaxKind.FunctionType, context) as FunctionTypeNode;
    //                   return signatureNode;

    //               }

    //               if (resolved.constructSignatures.length === 1 && !resolved.callSignatures.length) {
    //                   const signature = resolved.constructSignatures[0];
    //                   const signatureNode = signatureToSignatureDeclarationHelper(signature, SyntaxKind.ConstructorType, context) as ConstructorTypeNode;
    //                   return signatureNode;
    //               }
    //           }

    //           const abstractSignatures = filter(resolved.constructSignatures, signature => !!(signature.flags & SignatureFlags.Abstract));
    //           if (some(abstractSignatures)) {
    //               const types = map(abstractSignatures, getOrCreateTypeFromSignature);
    //               // count the number of type elements excluding abstract constructors
    //               const typeElementCount =
    //                   resolved.callSignatures.length +
    //                   (resolved.constructSignatures.length - abstractSignatures.length) +
    //                   resolved.indexInfos.length +
    //                   // exclude `prototype` when writing a class expression as a type literal, as per
    //                   // the logic in `createTypeNodesFromResolvedType`.
    //                   (context.flags & NodeBuilderFlags.WriteClassExpressionAsTypeLiteral ?
    //                       countWhere(resolved.properties, p => !(p.flags & SymbolFlags.Prototype)) :
    //                       length(resolved.properties));
    //               // don't include an empty object literal if there were no other static-side
    //               // properties to write, i.e. `abstract class C { }` becomes `abstract new () => {}`
    //               // and not `(abstract new () => {}) & {}`
    //               if (typeElementCount) {
    //                   // create a copy of the object type without any abstract construct signatures.
    //                   types.push(getResolvedTypeWithoutAbstractConstructSignatures(resolved));
    //               }
    //               return typeToTypeNodeHelper(getIntersectionType(types), context);
    //           }

    //           const savedFlags = context.flags;
    //           context.flags |= NodeBuilderFlags.InObjectTypeLiteral;
    //           const members = createTypeNodesFromResolvedType(resolved);
    //           context.flags = savedFlags;
    //           const typeLiteralNode = factory.createTypeLiteralNode(members);
    //           context.approximateLength += 2;
    //           setEmitFlags(typeLiteralNode, (context.flags & NodeBuilderFlags.MultilineObjectLiterals) ? 0 : EmitFlags.SingleLine);
    //           return typeLiteralNode;
    //       }

    //       function typeReferenceToTypeNode(type: TypeReference) {
    //           let typeArguments: readonly Type[] = getTypeArguments(type);
    //           if (type.target === globalArrayType || type.target === globalReadonlyArrayType) {
    //               if (context.flags & NodeBuilderFlags.WriteArrayAsGenericType) {
    //                   const typeArgumentNode = typeToTypeNodeHelper(typeArguments[0], context);
    //                   return factory.createTypeReferenceNode(type.target === globalArrayType ? "Array" : "ReadonlyArray", [typeArgumentNode]);
    //               }
    //               const elementType = typeToTypeNodeHelper(typeArguments[0], context);
    //               const arrayType = factory.createArrayTypeNode(elementType);
    //               return type.target === globalArrayType ? arrayType : factory.createTypeOperatorNode(SyntaxKind.ReadonlyKeyword, arrayType);
    //           }
    //           else if (type.target.objectFlags & ObjectFlags.Tuple) {
    //               typeArguments = sameMap(typeArguments, (t, i) => removeMissingType(t, !!((type.target as TupleType).elementFlags[i] & ElementFlags.Optional)));
    //               if (typeArguments.length > 0) {
    //                   const arity = getTypeReferenceArity(type);
    //                   const tupleConstituentNodes = mapToTypeNodes(typeArguments.slice(0, arity), context);
    //                   if (tupleConstituentNodes) {
    //                       if ((type.target as TupleType).labeledElementDeclarations) {
    //                           for (let i = 0; i < tupleConstituentNodes.length; i++) {
    //                               const flags = (type.target as TupleType).elementFlags[i];
    //                               tupleConstituentNodes[i] = factory.createNamedTupleMember(
    //                                   flags & ElementFlags.Variable ? factory.createToken(SyntaxKind.DotDotDotToken) : undefined,
    //                                   factory.createIdentifier(unescapeLeadingUnderscores(getTupleElementLabel((type.target as TupleType).labeledElementDeclarations![i]))),
    //                                   flags & ElementFlags.Optional ? factory.createToken(SyntaxKind.QuestionToken) : undefined,
    //                                   flags & ElementFlags.Rest ? factory.createArrayTypeNode(tupleConstituentNodes[i]) :
    //                                   tupleConstituentNodes[i]
    //                               );
    //                           }
    //                       }
    //                       else {
    //                           for (let i = 0; i < Math.min(arity, tupleConstituentNodes.length); i++) {
    //                               const flags = (type.target as TupleType).elementFlags[i];
    //                               tupleConstituentNodes[i] =
    //                                   flags & ElementFlags.Variable ? factory.createRestTypeNode(flags & ElementFlags.Rest ? factory.createArrayTypeNode(tupleConstituentNodes[i]) : tupleConstituentNodes[i]) :
    //                                   flags & ElementFlags.Optional ? factory.createOptionalTypeNode(tupleConstituentNodes[i]) :
    //                                   tupleConstituentNodes[i];
    //                           }
    //                       }
    //                       const tupleTypeNode = setEmitFlags(factory.createTupleTypeNode(tupleConstituentNodes), EmitFlags.SingleLine);
    //                       return (type.target as TupleType).readonly ? factory.createTypeOperatorNode(SyntaxKind.ReadonlyKeyword, tupleTypeNode) : tupleTypeNode;
    //                   }
    //               }
    //               if (context.encounteredError || (context.flags & NodeBuilderFlags.AllowEmptyTuple)) {
    //                   const tupleTypeNode = setEmitFlags(factory.createTupleTypeNode([]), EmitFlags.SingleLine);
    //                   return (type.target as TupleType).readonly ? factory.createTypeOperatorNode(SyntaxKind.ReadonlyKeyword, tupleTypeNode) : tupleTypeNode;
    //               }
    //               context.encounteredError = true;
    //               return undefined!; // TODO: GH#18217
    //           }
    //           else if (context.flags & NodeBuilderFlags.WriteClassExpressionAsTypeLiteral &&
    //               type.symbol.valueDeclaration &&
    //               isClassLike(type.symbol.valueDeclaration) &&
    //               !isValueSymbolAccessible(type.symbol, context.enclosingDeclaration)
    //           ) {
    //               return createAnonymousTypeNode(type);
    //           }
    //           else {
    //               const outerTypeParameters = type.target.outerTypeParameters;
    //               let i = 0;
    //               let resultType: TypeReferenceNode | ImportTypeNode | undefined;
    //               if (outerTypeParameters) {
    //                   const length = outerTypeParameters.length;
    //                   while (i < length) {
    //                       // Find group of type arguments for type parameters with the same declaring container.
    //                       const start = i;
    //                       const parent = getParentSymbolOfTypeParameter(outerTypeParameters[i])!;
    //                       do {
    //                           i++;
    //                       } while (i < length && getParentSymbolOfTypeParameter(outerTypeParameters[i]) === parent);
    //                       // When type parameters are their own type arguments for the whole group (i.e. we have
    //                       // the default outer type arguments), we don't show the group.
    //                       if (!rangeEquals(outerTypeParameters, typeArguments, start, i)) {
    //                           const typeArgumentSlice = mapToTypeNodes(typeArguments.slice(start, i), context);
    //                           const flags = context.flags;
    //                           context.flags |= NodeBuilderFlags.ForbidIndexedAccessSymbolReferences;
    //                           const ref = symbolToTypeNode(parent, context, SymbolFlags.Type, typeArgumentSlice) as TypeReferenceNode | ImportTypeNode;
    //                           context.flags = flags;
    //                           resultType = !resultType ? ref : appendReferenceToType(resultType, ref as TypeReferenceNode);
    //                       }
    //                   }
    //               }
    //               let typeArgumentNodes: readonly TypeNode[] | undefined;
    //               if (typeArguments.length > 0) {
    //                   const typeParameterCount = (type.target.typeParameters || emptyArray).length;
    //                   typeArgumentNodes = mapToTypeNodes(typeArguments.slice(i, typeParameterCount), context);
    //               }
    //               const flags = context.flags;
    //               context.flags |= NodeBuilderFlags.ForbidIndexedAccessSymbolReferences;
    //               const finalRef = symbolToTypeNode(type.symbol, context, SymbolFlags.Type, typeArgumentNodes);
    //               context.flags = flags;
    //               return !resultType ? finalRef : appendReferenceToType(resultType, finalRef as TypeReferenceNode);
    //           }
    //       }

    //       function appendReferenceToType(root: TypeReferenceNode | ImportTypeNode, ref: TypeReferenceNode): TypeReferenceNode | ImportTypeNode {
    //           if (isImportTypeNode(root)) {
    //               // first shift type arguments
    //               let typeArguments = root.typeArguments;
    //               let qualifier = root.qualifier;
    //               if (qualifier) {
    //                   if (isIdentifier(qualifier)) {
    //                       qualifier = factory.updateIdentifier(qualifier, typeArguments);
    //                   }
    //                   else {
    //                       qualifier = factory.updateQualifiedName(qualifier,
    //                           qualifier.left,
    //                           factory.updateIdentifier(qualifier.right, typeArguments));
    //                   }
    //               }
    //               typeArguments = ref.typeArguments;
    //               // then move qualifiers
    //               const ids = getAccessStack(ref);
    //               for (const id of ids) {
    //                   qualifier = qualifier ? factory.createQualifiedName(qualifier, id) : id;
    //               }
    //               return factory.updateImportTypeNode(
    //                   root,
    //                   root.argument,
    //                   qualifier,
    //                   typeArguments,
    //                   root.isTypeOf);
    //           }
    //           else {
    //               // first shift type arguments
    //               let typeArguments = root.typeArguments;
    //               let typeName = root.typeName;
    //               if (isIdentifier(typeName)) {
    //                   typeName = factory.updateIdentifier(typeName, typeArguments);
    //               }
    //               else {
    //                   typeName = factory.updateQualifiedName(typeName,
    //                       typeName.left,
    //                       factory.updateIdentifier(typeName.right, typeArguments));
    //               }
    //               typeArguments = ref.typeArguments;
    //               // then move qualifiers
    //               const ids = getAccessStack(ref);
    //               for (const id of ids) {
    //                   typeName = factory.createQualifiedName(typeName, id);
    //               }
    //               return factory.updateTypeReferenceNode(
    //                   root,
    //                   typeName,
    //                   typeArguments);
    //           }
    //       }

    //       function getAccessStack(ref: TypeReferenceNode): Identifier[] {
    //           let state = ref.typeName;
    //           const ids = [];
    //           while (!isIdentifier(state)) {
    //               ids.unshift(state.right);
    //               state = state.left;
    //           }
    //           ids.unshift(state);
    //           return ids;
    //       }

    //       function createTypeNodesFromResolvedType(resolvedType: ResolvedType): TypeElement[] | undefined {
    //           if (checkTruncationLength(context)) {
    //               return [factory.createPropertySignature(/*modifiers*/ undefined, "...", /*questionToken*/ undefined, /*type*/ undefined)];
    //           }
    //           const typeElements: TypeElement[] = [];
    //           for (const signature of resolvedType.callSignatures) {
    //               typeElements.push(signatureToSignatureDeclarationHelper(signature, SyntaxKind.CallSignature, context) as CallSignatureDeclaration);
    //           }
    //           for (const signature of resolvedType.constructSignatures) {
    //               if (signature.flags & SignatureFlags.Abstract) continue;
    //               typeElements.push(signatureToSignatureDeclarationHelper(signature, SyntaxKind.ConstructSignature, context) as ConstructSignatureDeclaration);
    //           }
    //           for (const info of resolvedType.indexInfos) {
    //               typeElements.push(indexInfoToIndexSignatureDeclarationHelper(info, context, resolvedType.objectFlags & ObjectFlags.ReverseMapped ? createElidedInformationPlaceholder(context) : undefined));
    //           }

    //           const properties = resolvedType.properties;
    //           if (!properties) {
    //               return typeElements;
    //           }

    //           let i = 0;
    //           for (const propertySymbol of properties) {
    //               i++;
    //               if (context.flags & NodeBuilderFlags.WriteClassExpressionAsTypeLiteral) {
    //                   if (propertySymbol.flags & SymbolFlags.Prototype) {
    //                       continue;
    //                   }
    //                   if (getDeclarationModifierFlagsFromSymbol(propertySymbol) & (ModifierFlags.Private | ModifierFlags.Protected) && context.tracker.reportPrivateInBaseOfClassExpression) {
    //                       context.tracker.reportPrivateInBaseOfClassExpression(unescapeLeadingUnderscores(propertySymbol.escapedName));
    //                   }
    //               }
    //               if (checkTruncationLength(context) && (i + 2 < properties.length - 1)) {
    //                   typeElements.push(factory.createPropertySignature(/*modifiers*/ undefined, `... ${properties.length - i} more ...`, /*questionToken*/ undefined, /*type*/ undefined));
    //                   addPropertyToElementList(properties[properties.length - 1], context, typeElements);
    //                   break;
    //               }
    //               addPropertyToElementList(propertySymbol, context, typeElements);

    //           }
    //           return typeElements.length ? typeElements : undefined;
    //       }
    //   }

    //   function createElidedInformationPlaceholder(context: NodeBuilderContext) {
    //       context.approximateLength += 3;
    //       if (!(context.flags & NodeBuilderFlags.NoTruncation)) {
    //           return factory.createTypeReferenceNode(factory.createIdentifier("..."), /*typeArguments*/ undefined);
    //       }
    //       return factory.createKeywordTypeNode(SyntaxKind.AnyKeyword);
    //   }

    //   function shouldUsePlaceholderForProperty(propertySymbol: Symbol, context: NodeBuilderContext) {
    //       // Use placeholders for reverse mapped types we've either already descended into, or which
    //       // are nested reverse mappings within a mapping over a non-anonymous type. The later is a restriction mostly just to
    //       // reduce the blowup in printback size from doing, eg, a deep reverse mapping over `Window`.
    //       // Since anonymous types usually come from expressions, this allows us to preserve the output
    //       // for deep mappings which likely come from expressions, while truncating those parts which
    //       // come from mappings over library functions.
    //       return !!(getCheckFlags(propertySymbol) & CheckFlags.ReverseMapped)
    //           && (
    //               contains(context.reverseMappedStack, propertySymbol as ReverseMappedSymbol)
    //               || (
    //                   context.reverseMappedStack?.[0]
    //                   && !(getObjectFlags(last(context.reverseMappedStack).propertyType) & ObjectFlags.Anonymous)
    //               )
    //           );
    //   }

    //   function addPropertyToElementList(propertySymbol: Symbol, context: NodeBuilderContext, typeElements: TypeElement[]) {
    //       const propertyIsReverseMapped = !!(getCheckFlags(propertySymbol) & CheckFlags.ReverseMapped);
    //       const propertyType = shouldUsePlaceholderForProperty(propertySymbol, context) ?
    //           anyType : getNonMissingTypeOfSymbol(propertySymbol);
    //       const saveEnclosingDeclaration = context.enclosingDeclaration;
    //       context.enclosingDeclaration = undefined;
    //       if (context.tracker.trackSymbol && getCheckFlags(propertySymbol) & CheckFlags.Late && isLateBoundName(propertySymbol.escapedName)) {
    //           if (propertySymbol.declarations) {
    //               const decl = first(propertySymbol.declarations);
    //               if (hasLateBindableName(decl)) {
    //                   if (isBinaryExpression(decl)) {
    //                       const name = getNameOfDeclaration(decl);
    //                       if (name && isElementAccessExpression(name) && isPropertyAccessEntityNameExpression(name.argumentExpression)) {
    //                           trackComputedName(name.argumentExpression, saveEnclosingDeclaration, context);
    //                       }
    //                   }
    //                   else {
    //                       trackComputedName(decl.name.expression, saveEnclosingDeclaration, context);
    //                   }
    //               }
    //           }
    //           else if (context.tracker?.reportNonSerializableProperty) {
    //               context.tracker.reportNonSerializableProperty(symbolToString(propertySymbol));
    //           }
    //       }
    //       context.enclosingDeclaration = propertySymbol.valueDeclaration || propertySymbol.declarations?.[0] || saveEnclosingDeclaration;
    //       const propertyName = getPropertyNameNodeForSymbol(propertySymbol, context);
    //       context.enclosingDeclaration = saveEnclosingDeclaration;
    //       context.approximateLength += (symbolName(propertySymbol).length + 1);
    //       const optionalToken = propertySymbol.flags & SymbolFlags.Optional ? factory.createToken(SyntaxKind.QuestionToken) : undefined;
    //       if (propertySymbol.flags & (SymbolFlags.Function | SymbolFlags.Method) && !getPropertiesOfObjectType(propertyType).length && !isReadonlySymbol(propertySymbol)) {
    //           const signatures = getSignaturesOfType(filterType(propertyType, t => !(t.flags & TypeFlags.Undefined)), SignatureKind.Call);
    //           for (const signature of signatures) {
    //               const methodDeclaration = signatureToSignatureDeclarationHelper(signature, SyntaxKind.MethodSignature, context, { name: propertyName, questionToken: optionalToken }) as MethodSignature;
    //               typeElements.push(preserveCommentsOn(methodDeclaration));
    //           }
    //       }
    //       else {
    //           let propertyTypeNode: TypeNode;
    //           if (shouldUsePlaceholderForProperty(propertySymbol, context)) {
    //               propertyTypeNode = createElidedInformationPlaceholder(context);
    //           }
    //           else {
    //               if (propertyIsReverseMapped) {
    //                   context.reverseMappedStack ||= [];
    //                   context.reverseMappedStack.push(propertySymbol as ReverseMappedSymbol);
    //               }
    //               propertyTypeNode = propertyType ? serializeTypeForDeclaration(context, propertyType, propertySymbol, saveEnclosingDeclaration) : factory.createKeywordTypeNode(SyntaxKind.AnyKeyword);
    //               if (propertyIsReverseMapped) {
    //                   context.reverseMappedStack!.pop();
    //               }
    //           }

    //           const modifiers = isReadonlySymbol(propertySymbol) ? [factory.createToken(SyntaxKind.ReadonlyKeyword)] : undefined;
    //           if (modifiers) {
    //               context.approximateLength += 9;
    //           }
    //           const propertySignature = factory.createPropertySignature(
    //               modifiers,
    //               propertyName,
    //               optionalToken,
    //               propertyTypeNode);

    //           typeElements.push(preserveCommentsOn(propertySignature));
    //       }

    //       function preserveCommentsOn<T extends Node>(node: T) {
    //           if (some(propertySymbol.declarations, d => d.kind === SyntaxKind.JSDocPropertyTag)) {
    //               const d = propertySymbol.declarations?.find(d => d.kind === SyntaxKind.JSDocPropertyTag)! as JSDocPropertyTag;
    //               const commentText = getTextOfJSDocComment(d.comment);
    //               if (commentText) {
    //                   setSyntheticLeadingComments(node, [{ kind: SyntaxKind.MultiLineCommentTrivia, text: "*\n * " + commentText.replace(/\n/g, "\n * ") + "\n ", pos: -1, end: -1, hasTrailingNewLine: true }]);
    //               }
    //           }
    //           else if (propertySymbol.valueDeclaration) {
    //               // Copy comments to node for declaration emit
    //               setCommentRange(node, propertySymbol.valueDeclaration);
    //           }
    //           return node;
    //       }
    //   }

    //   function mapToTypeNodes(types: readonly Type[] | undefined, context: NodeBuilderContext, isBareList?: boolean): TypeNode[] | undefined {
    //       if (some(types)) {
    //           if (checkTruncationLength(context)) {
    //               if (!isBareList) {
    //                   return [factory.createTypeReferenceNode("...", /*typeArguments*/ undefined)];
    //               }
    //               else if (types.length > 2) {
    //                   return [
    //                       typeToTypeNodeHelper(types[0], context),
    //                       factory.createTypeReferenceNode(`... ${types.length - 2} more ...`, /*typeArguments*/ undefined),
    //                       typeToTypeNodeHelper(types[types.length - 1], context)
    //                   ];
    //               }
    //           }
    //           const mayHaveNameCollisions = !(context.flags & NodeBuilderFlags.UseFullyQualifiedType);
    //           /** Map from type reference identifier text to [type, index in `result` where the type node is] */
    //           const seenNames = mayHaveNameCollisions ? createUnderscoreEscapedMultiMap<[Type, number]>() : undefined;
    //           const result: TypeNode[] = [];
    //           let i = 0;
    //           for (const type of types) {
    //               i++;
    //               if (checkTruncationLength(context) && (i + 2 < types.length - 1)) {
    //                   result.push(factory.createTypeReferenceNode(`... ${types.length - i} more ...`, /*typeArguments*/ undefined));
    //                   const typeNode = typeToTypeNodeHelper(types[types.length - 1], context);
    //                   if (typeNode) {
    //                       result.push(typeNode);
    //                   }
    //                   break;
    //               }
    //               context.approximateLength += 2; // Account for whitespace + separator
    //               const typeNode = typeToTypeNodeHelper(type, context);
    //               if (typeNode) {
    //                   result.push(typeNode);
    //                   if (seenNames && isIdentifierTypeReference(typeNode)) {
    //                       seenNames.add(typeNode.typeName.escapedText, [type, result.length - 1]);
    //                   }
    //               }
    //           }

    //           if (seenNames) {
    //               // To avoid printing types like `[Foo, Foo]` or `Bar & Bar` where
    //               // occurrences of the same name actually come from different
    //               // namespaces, go through the single-identifier type reference nodes
    //               // we just generated, and see if any names were generated more than
    //               // once while referring to different types. If so, regenerate the
    //               // type node for each entry by that name with the
    //               // `UseFullyQualifiedType` flag enabled.
    //               const saveContextFlags = context.flags;
    //               context.flags |= NodeBuilderFlags.UseFullyQualifiedType;
    //               seenNames.forEach(types => {
    //                   if (!arrayIsHomogeneous(types, ([a], [b]) => typesAreSameReference(a, b))) {
    //                       for (const [type, resultIndex] of types) {
    //                           result[resultIndex] = typeToTypeNodeHelper(type, context);
    //                       }
    //                   }
    //               });
    //               context.flags = saveContextFlags;
    //           }

    //           return result;
    //       }
    //   }

    //   function typesAreSameReference(a: Type, b: Type): boolean {
    //       return a === b
    //           || !!a.symbol && a.symbol === b.symbol
    //           || !!a.aliasSymbol && a.aliasSymbol === b.aliasSymbol;
    //   }

    //   function indexInfoToIndexSignatureDeclarationHelper(indexInfo: IndexInfo, context: NodeBuilderContext, typeNode: TypeNode | undefined): IndexSignatureDeclaration {
    //       const name = getNameFromIndexInfo(indexInfo) || "x";
    //       const indexerTypeNode = typeToTypeNodeHelper(indexInfo.keyType, context);

    //       const indexingParameter = factory.createParameterDeclaration(
    //           /*decorators*/ undefined,
    //           /*modifiers*/ undefined,
    //           /*dotDotDotToken*/ undefined,
    //           name,
    //           /*questionToken*/ undefined,
    //           indexerTypeNode,
    //           /*initializer*/ undefined);
    //       if (!typeNode) {
    //           typeNode = typeToTypeNodeHelper(indexInfo.type || anyType, context);
    //       }
    //       if (!indexInfo.type && !(context.flags & NodeBuilderFlags.AllowEmptyIndexInfoType)) {
    //           context.encounteredError = true;
    //       }
    //       context.approximateLength += (name.length + 4);
    //       return factory.createIndexSignature(
    //           /*decorators*/ undefined,
    //           indexInfo.isReadonly ? [factory.createToken(SyntaxKind.ReadonlyKeyword)] : undefined,
    //           [indexingParameter],
    //           typeNode);
    //   }

    //   interface SignatureToSignatureDeclarationOptions {
    //       modifiers?: readonly Modifier[];
    //       name?: PropertyName;
    //       questionToken?: QuestionToken;
    //       privateSymbolVisitor?: (s: Symbol) => void;
    //       bundledImports?: boolean;
    //   }

    //   function signatureToSignatureDeclarationHelper(signature: Signature, kind: SignatureDeclaration["kind"], context: NodeBuilderContext, options?: SignatureToSignatureDeclarationOptions): SignatureDeclaration {
    //       const suppressAny = context.flags & NodeBuilderFlags.SuppressAnyReturnType;
    //       if (suppressAny) context.flags &= ~NodeBuilderFlags.SuppressAnyReturnType; // suppress only toplevel `any`s
    //       context.approximateLength += 3; // Usually a signature contributes a few more characters than this, but 3 is the minimum
    //       let typeParameters: TypeParameterDeclaration[] | undefined;
    //       let typeArguments: TypeNode[] | undefined;
    //       if (context.flags & NodeBuilderFlags.WriteTypeArgumentsOfSignature && signature.target && signature.mapper && signature.target.typeParameters) {
    //           typeArguments = signature.target.typeParameters.map(parameter => typeToTypeNodeHelper(instantiateType(parameter, signature.mapper), context));
    //       }
    //       else {
    //           typeParameters = signature.typeParameters && signature.typeParameters.map(parameter => typeParameterToDeclaration(parameter, context));
    //       }

    //       const expandedParams = getExpandedParameters(signature, /*skipUnionExpanding*/ true)[0];
    //       // If the expanded parameter list had a variadic in a non-trailing position, don't expand it
    //       const parameters = (some(expandedParams, p => p !== expandedParams[expandedParams.length - 1] && !!(getCheckFlags(p) & CheckFlags.RestParameter)) ? signature.parameters : expandedParams).map(parameter => symbolToParameterDeclaration(parameter, context, kind === SyntaxKind.Constructor, options?.privateSymbolVisitor, options?.bundledImports));
    //       if (signature.thisParameter) {
    //           const thisParameter = symbolToParameterDeclaration(signature.thisParameter, context);
    //           parameters.unshift(thisParameter);
    //       }

    //       let returnTypeNode: TypeNode | undefined;
    //       const typePredicate = getTypePredicateOfSignature(signature);
    //       if (typePredicate) {
    //           const assertsModifier = typePredicate.kind === TypePredicateKind.AssertsThis || typePredicate.kind === TypePredicateKind.AssertsIdentifier ?
    //               factory.createToken(SyntaxKind.AssertsKeyword) :
    //               undefined;
    //           const parameterName = typePredicate.kind === TypePredicateKind.Identifier || typePredicate.kind === TypePredicateKind.AssertsIdentifier ?
    //               setEmitFlags(factory.createIdentifier(typePredicate.parameterName), EmitFlags.NoAsciiEscaping) :
    //               factory.createThisTypeNode();
    //           const typeNode = typePredicate.type && typeToTypeNodeHelper(typePredicate.type, context);
    //           returnTypeNode = factory.createTypePredicateNode(assertsModifier, parameterName, typeNode);
    //       }
    //       else {
    //           const returnType = getReturnTypeOfSignature(signature);
    //           if (returnType && !(suppressAny && isTypeAny(returnType))) {
    //               returnTypeNode = serializeReturnTypeForSignature(context, returnType, signature, options?.privateSymbolVisitor, options?.bundledImports);
    //           }
    //           else if (!suppressAny) {
    //               returnTypeNode = factory.createKeywordTypeNode(SyntaxKind.AnyKeyword);
    //           }
    //       }
    //       let modifiers = options?.modifiers;
    //       if ((kind === SyntaxKind.ConstructorType) && signature.flags & SignatureFlags.Abstract) {
    //           const flags = modifiersToFlags(modifiers);
    //           modifiers = factory.createModifiersFromModifierFlags(flags | ModifierFlags.Abstract);
    //       }

    //       const node =
    //           kind === SyntaxKind.CallSignature ? factory.createCallSignature(typeParameters, parameters, returnTypeNode) :
    //           kind === SyntaxKind.ConstructSignature ? factory.createConstructSignature(typeParameters, parameters, returnTypeNode) :
    //           kind === SyntaxKind.MethodSignature ? factory.createMethodSignature(modifiers, options?.name ?? factory.createIdentifier(""), options?.questionToken, typeParameters, parameters, returnTypeNode) :
    //           kind === SyntaxKind.MethodDeclaration ? factory.createMethodDeclaration(/*decorators*/ undefined, modifiers, /*asteriskToken*/ undefined, options?.name ?? factory.createIdentifier(""), /*questionToken*/ undefined, typeParameters, parameters, returnTypeNode, /*body*/ undefined) :
    //           kind === SyntaxKind.Constructor ? factory.createConstructorDeclaration(/*decorators*/ undefined, modifiers, parameters, /*body*/ undefined) :
    //           kind === SyntaxKind.GetAccessor ? factory.createGetAccessorDeclaration(/*decorators*/ undefined, modifiers, options?.name ?? factory.createIdentifier(""), parameters, returnTypeNode, /*body*/ undefined) :
    //           kind === SyntaxKind.SetAccessor ? factory.createSetAccessorDeclaration(/*decorators*/ undefined, modifiers, options?.name ?? factory.createIdentifier(""), parameters, /*body*/ undefined) :
    //           kind === SyntaxKind.IndexSignature ? factory.createIndexSignature(/*decorators*/ undefined, modifiers, parameters, returnTypeNode) :
    //           kind === SyntaxKind.JSDocFunctionType ? factory.createJSDocFunctionType(parameters, returnTypeNode) :
    //           kind === SyntaxKind.FunctionType ? factory.createFunctionTypeNode(typeParameters, parameters, returnTypeNode ?? factory.createTypeReferenceNode(factory.createIdentifier(""))) :
    //           kind === SyntaxKind.ConstructorType ? factory.createConstructorTypeNode(modifiers, typeParameters, parameters, returnTypeNode ?? factory.createTypeReferenceNode(factory.createIdentifier(""))) :
    //           kind === SyntaxKind.FunctionDeclaration ? factory.createFunctionDeclaration(/*decorators*/ undefined, modifiers, /*asteriskToken*/ undefined, options?.name ? cast(options.name, isIdentifier) : factory.createIdentifier(""), typeParameters, parameters, returnTypeNode, /*body*/ undefined) :
    //           kind === SyntaxKind.FunctionExpression ? factory.createFunctionExpression(modifiers, /*asteriskToken*/ undefined, options?.name ? cast(options.name, isIdentifier) : factory.createIdentifier(""), typeParameters, parameters, returnTypeNode, factory.createBlock([])) :
    //           kind === SyntaxKind.ArrowFunction ? factory.createArrowFunction(modifiers, typeParameters, parameters, returnTypeNode, /*equalsGreaterThanToken*/ undefined, factory.createBlock([])) :
    //           Debug.assertNever(kind);

    //       if (typeArguments) {
    //           node.typeArguments = factory.createNodeArray(typeArguments);
    //       }

    //       return node;
    //   }

    //   function typeParameterToDeclarationWithConstraint(type: TypeParameter, context: NodeBuilderContext, constraintNode: TypeNode | undefined): TypeParameterDeclaration {
    //       const savedContextFlags = context.flags;
    //       context.flags &= ~NodeBuilderFlags.WriteTypeParametersInQualifiedName; // Avoids potential infinite loop when building for a claimspace with a generic
    //       const name = typeParameterToName(type, context);
    //       const defaultParameter = getDefaultFromTypeParameter(type);
    //       const defaultParameterNode = defaultParameter && typeToTypeNodeHelper(defaultParameter, context);
    //       context.flags = savedContextFlags;
    //       return factory.createTypeParameterDeclaration(name, constraintNode, defaultParameterNode);
    //   }

    //   function typeParameterToDeclaration(type: TypeParameter, context: NodeBuilderContext, constraint = getConstraintOfTypeParameter(type)): TypeParameterDeclaration {
    //       const constraintNode = constraint && typeToTypeNodeHelper(constraint, context);
    //       return typeParameterToDeclarationWithConstraint(type, context, constraintNode);
    //   }

    //   function symbolToParameterDeclaration(parameterSymbol: Symbol, context: NodeBuilderContext, preserveModifierFlags?: boolean, privateSymbolVisitor?: (s: Symbol) => void, bundledImports?: boolean): ParameterDeclaration {
    //       let parameterDeclaration: ParameterDeclaration | JSDocParameterTag | undefined = getDeclarationOfKind<ParameterDeclaration>(parameterSymbol, SyntaxKind.Parameter);
    //       if (!parameterDeclaration && !isTransientSymbol(parameterSymbol)) {
    //           parameterDeclaration = getDeclarationOfKind<JSDocParameterTag>(parameterSymbol, SyntaxKind.JSDocParameterTag);
    //       }

    //       let parameterType = getTypeOfSymbol(parameterSymbol);
    //       if (parameterDeclaration && isRequiredInitializedParameter(parameterDeclaration)) {
    //           parameterType = getOptionalType(parameterType);
    //       }
    //       if ((context.flags & NodeBuilderFlags.NoUndefinedOptionalParameterType) && parameterDeclaration && !isJSDocParameterTag(parameterDeclaration) && isOptionalUninitializedParameter(parameterDeclaration)) {
    //           parameterType = getTypeWithFacts(parameterType, TypeFacts.NEUndefined);
    //       }
    //       const parameterTypeNode = serializeTypeForDeclaration(context, parameterType, parameterSymbol, context.enclosingDeclaration, privateSymbolVisitor, bundledImports);

    //       const modifiers = !(context.flags & NodeBuilderFlags.OmitParameterModifiers) && preserveModifierFlags && parameterDeclaration && parameterDeclaration.modifiers ? parameterDeclaration.modifiers.map(factory.cloneNode) : undefined;
    //       const isRest = parameterDeclaration && isRestParameter(parameterDeclaration) || getCheckFlags(parameterSymbol) & CheckFlags.RestParameter;
    //       const dotDotDotToken = isRest ? factory.createToken(SyntaxKind.DotDotDotToken) : undefined;
    //       const name = parameterDeclaration ? parameterDeclaration.name ?
    //           parameterDeclaration.name.kind === SyntaxKind.Identifier ? setEmitFlags(factory.cloneNode(parameterDeclaration.name), EmitFlags.NoAsciiEscaping) :
    //           parameterDeclaration.name.kind === SyntaxKind.QualifiedName ? setEmitFlags(factory.cloneNode(parameterDeclaration.name.right), EmitFlags.NoAsciiEscaping) :
    //           cloneBindingName(parameterDeclaration.name) :
    //           symbolName(parameterSymbol) :
    //           symbolName(parameterSymbol);
    //       const isOptional = parameterDeclaration && isOptionalParameter(parameterDeclaration) || getCheckFlags(parameterSymbol) & CheckFlags.OptionalParameter;
    //       const questionToken = isOptional ? factory.createToken(SyntaxKind.QuestionToken) : undefined;
    //       const parameterNode = factory.createParameterDeclaration(
    //           /*decorators*/ undefined,
    //           modifiers,
    //           dotDotDotToken,
    //           name,
    //           questionToken,
    //           parameterTypeNode,
    //           /*initializer*/ undefined);
    //       context.approximateLength += symbolName(parameterSymbol).length + 3;
    //       return parameterNode;

    //       function cloneBindingName(node: BindingName): BindingName {
    //           return elideInitializerAndSetEmitFlags(node) as BindingName;
    //           function elideInitializerAndSetEmitFlags(node: Node): Node {
    //               if (context.tracker.trackSymbol && isComputedPropertyName(node) && isLateBindableName(node)) {
    //                   trackComputedName(node.expression, context.enclosingDeclaration, context);
    //               }
    //               let visited = visitEachChild(node, elideInitializerAndSetEmitFlags, nullTransformationContext, /*nodesVisitor*/ undefined, elideInitializerAndSetEmitFlags)!;
    //               if (isBindingElement(visited)) {
    //                   visited = factory.updateBindingElement(
    //                       visited,
    //                       visited.dotDotDotToken,
    //                       visited.propertyName,
    //                       visited.name,
    //                       /*initializer*/ undefined);
    //               }
    //               if (!nodeIsSynthesized(visited)) {
    //                   visited = factory.cloneNode(visited);
    //               }
    //               return setEmitFlags(visited, EmitFlags.SingleLine | EmitFlags.NoAsciiEscaping);
    //           }
    //       }
    //   }

    //   function trackComputedName(accessExpression: EntityNameOrEntityNameExpression, enclosingDeclaration: Node | undefined, context: NodeBuilderContext) {
    //       if (!context.tracker.trackSymbol) return;
    //       // get symbol of the first identifier of the entityName
    //       const firstIdentifier = getFirstIdentifier(accessExpression);
    //       const name = resolveName(firstIdentifier, firstIdentifier.escapedText, SymbolFlags.Value | SymbolFlags.ExportValue, /*nodeNotFoundErrorMessage*/ undefined, /*nameArg*/ undefined, /*isUse*/ true);
    //       if (name) {
    //           context.tracker.trackSymbol(name, enclosingDeclaration, SymbolFlags.Value);
    //       }
    //   }

    pub fn lookupSymbolChain(
        &mut self,
        symbol: SymbolId,
        context: &NodeBuilderContext,
        meaning: SymbolFlags,
        yieldModuleSymbol: bool,
    ) -> Vec<SymbolId> {
        // println!("");
        // context.tracker.trackSymbol!(symbol, context.enclosingDeclaration, meaning); // TODO: GH#18217
        self.lookupSymbolChainWorker(symbol, context, meaning, yieldModuleSymbol)
    }

    fn lookupSymbolChainWorker(
        &mut self,
        symbol: SymbolId,
        context: &NodeBuilderContext,
        meaning: SymbolFlags,
        yieldModuleSymbol: bool,
    ) -> Vec<SymbolId> {
        /** @param endOfChain Set to false for recursive calls; non-recursive calls should always output something. */
        fn getSymbolChain(
            checker: &mut Checker,
            symbol: SymbolId,
            context: &NodeBuilderContext,
            meaning: SymbolFlags,
            yieldModuleSymbol: bool,
            endOfChain: bool,
        ) -> Option<Vec<SymbolId>> {
            let mut accessibleSymbolChain = checker.getAccessibleSymbolChain(
                Some(symbol),
                context.enclosingDeclaration.clone(),
                meaning,
                context
                    .flags
                    .intersects(NodeBuilderFlags::UseOnlyExternalAliasing),
                &mut Default::default(),
            );
            // dbg!(&accessibleSymbolChain);
            // TODO: remove:
            // let parentSpecifiers: Vec<Option<JsWord>>;
            if accessibleSymbolChain.is_none()
                || checker.needsQualification(
                    accessibleSymbolChain.as_ref().unwrap()[0],
                    context.enclosingDeclaration.clone(),
                    if accessibleSymbolChain.as_ref().unwrap().len() == 1 {
                        meaning
                    } else {
                        getQualifiedLeftMeaning(meaning)
                    },
                )
            {
                // Go up and add our parent.
                let parents = checker.getContainersOfSymbol(
                    if let Some(accessibleSymbolChain) = &accessibleSymbolChain {
                        accessibleSymbolChain[0]
                    } else {
                        symbol
                    },
                    context.enclosingDeclaration.clone(),
                    meaning,
                );
                // dbg!(&parents);
                if let Some(parents) = &parents {
                    if !parents.is_empty() {
                        // TODO optimise whole block
                        // let parentSpecifiers:Vec<JsWord> = parents
                        //     .iter()
                        //     .map(|&s| {
                        //         if checker.symbols[s]
                        //             .declarations()
                        //             .iter()
                        //             .any(hasNonGlobalAugmentationExternalModuleSymbol)
                        //         {
                        //             todo!()
                        //             // checker.getSpecifierForModuleSymbol(s, context)
                        //         } else {
                        //             None
                        //         }
                        //     })
                        //     .collect::<Vec<_>>();
                        let indices = (0..parents.len()).collect::<Vec<_>>();
                        // indices.sort_unstable_by(|a, b| {
                        //     let specifierA = parentSpecifiers[*a];
                        //     let specifierB = parentSpecifiers[*b];
                        //     if (specifierA && specifierB) {
                        //         let isBRelative = pathIsRelative(specifierB);
                        //         if (pathIsRelative(specifierA) == isBRelative) {
                        //             // Both relative or both non-relative, sort by number of parts
                        //             return moduleSpecifiers.countPathComponents(specifierA)
                        //                 - moduleSpecifiers.countPathComponents(specifierB);
                        //         }
                        //         if (isBRelative) {
                        //             // A is non-relative, B is relative: prefer A
                        //             return std::cmp::Ordering::Less;
                        //         }
                        //         // A is relative, B is non-relative: prefer B
                        //         return std::cmp::Ordering::Greater;
                        //     }
                        //     return std::cmp::Ordering::Equal;
                        // });
                        // dbg!(indices.iter().map(|i| parents[*i]).collect::<Vec<_>>());
                        for parent in indices.iter().map(|i| parents[*i]) {
                            let parentChain = getSymbolChain(
                                checker,
                                parent,
                                context,
                                getQualifiedLeftMeaning(meaning),
                                yieldModuleSymbol,
                                false,
                            );
                            if let Some(mut parentChain) = parentChain {
                                if let Some(exports) = checker.symbols[parent].exports() {
                                    if let Some(e) = checker.symbol_tables[*exports]
                                        .get(&InternalSymbolName::ExportEquals.into())
                                    {
                                        if checker.getSymbolIfSameReference(*e, symbol).is_some() {
                                            // parentChain root _is_ symbol - symbol is a module export=, so it kinda looks like it's own parent
                                            // No need to lookup an alias for the symbol in itself
                                            accessibleSymbolChain = Some(parentChain);
                                            break;
                                        }
                                    }
                                }
                                if let Some(accessibleSymbolChain) = &mut accessibleSymbolChain {
                                    accessibleSymbolChain.splice(0..0, parentChain);
                                } else {
                                    parentChain.push(
                                        checker
                                            .getAliasForSymbolInContainer(parent, symbol)
                                            .unwrap_or(symbol),
                                    );
                                    accessibleSymbolChain = Some(parentChain);
                                }
                                break;
                            }
                        }
                    }
                }
            }

            // println!("accessibleSymbolChain: {:?}", &accessibleSymbolChain);

            if accessibleSymbolChain.is_some() {
                return accessibleSymbolChain;
            }
            if
            // If this is the last part of outputting the symbol, always output. The cases apply only to parent symbols.
            endOfChain ||
                // If a parent symbol is an anonymous type, don't write it.
                !checker.symbols[symbol].flags().intersects(SymbolFlags::TypeLiteral | SymbolFlags::ObjectLiteral)
            {
                // If a parent symbol is an external module, don't write it. (We prefer just `x` vs `"foo/bar".x`.)
                if !endOfChain
                    && !yieldModuleSymbol
                    && checker.symbols[symbol]
                        .declarations()
                        .iter()
                        .any(hasNonGlobalAugmentationExternalModuleSymbol)
                {
                    return None;
                }
                return Some(vec![symbol]);
            }
            None
        }

        // Try to get qualified name if the symbol is not a type parameter and there is an enclosing declaration.
        let isTypeParameter = self.symbols[symbol]
            .flags()
            .intersects(SymbolFlags::TypeParameter);
        if !isTypeParameter
            && (context.enclosingDeclaration.is_some()
                || context
                    .flags
                    .intersects(NodeBuilderFlags::UseFullyQualifiedType))
            && !context
                .flags
                .intersects(NodeBuilderFlags::DoNotIncludeSymbolChain)
        {
            let chain =
                getSymbolChain(self, symbol, context, meaning, yieldModuleSymbol, true).unwrap();
            debug_assert!(!chain.is_empty());
            chain
        } else {
            vec![symbol]
        }
    }

    //   function typeParametersToTypeParameterDeclarations(symbol: Symbol, context: NodeBuilderContext) {
    //       let typeParameterNodes: NodeArray<TypeParameterDeclaration> | undefined;
    //       const targetSymbol = getTargetSymbol(symbol);
    //       if (targetSymbol.flags & (SymbolFlags.Class | SymbolFlags.Interface | SymbolFlags.TypeAlias)) {
    //           typeParameterNodes = factory.createNodeArray(map(getLocalTypeParametersOfClassOrInterfaceOrTypeAlias(symbol), tp => typeParameterToDeclaration(tp, context)));
    //       }
    //       return typeParameterNodes;
    //   }

    //   function lookupTypeParameterNodes(chain: Symbol[], index: number, context: NodeBuilderContext) {
    //       Debug.assert(chain && 0 <= index && index < chain.length);
    //       const symbol = chain[index];
    //       const symbolId = getSymbolId(symbol);
    //       if (context.typeParameterSymbolList?.has(symbolId)) {
    //           return undefined;
    //       }
    //       (context.typeParameterSymbolList || (context.typeParameterSymbolList = new Set())).add(symbolId);
    //       let typeParameterNodes: readonly TypeNode[] | readonly TypeParameterDeclaration[] | undefined;
    //       if (context.flags & NodeBuilderFlags.WriteTypeParametersInQualifiedName && index < (chain.length - 1)) {
    //           const parentSymbol = symbol;
    //           const nextSymbol = chain[index + 1];
    //           if (getCheckFlags(nextSymbol) & CheckFlags.Instantiated) {
    //               const params = getTypeParametersOfClassOrInterface(
    //                   parentSymbol.flags & SymbolFlags.Alias ? resolveAlias(parentSymbol) : parentSymbol
    //               );
    //               typeParameterNodes = mapToTypeNodes(map(params, t => getMappedType(t, (nextSymbol as TransientSymbol).mapper!)), context);
    //           }
    //           else {
    //               typeParameterNodes = typeParametersToTypeParameterDeclarations(symbol, context);
    //           }
    //       }
    //       return typeParameterNodes;
    //   }

    //   /**
    //    * Given A[B][C][D], finds A[B]
    //    */
    //   function getTopmostIndexedAccessType(top: IndexedAccessTypeNode): IndexedAccessTypeNode {
    //       if (isIndexedAccessTypeNode(top.objectType)) {
    //           return getTopmostIndexedAccessType(top.objectType);
    //       }
    //       return top;
    //   }

    //   function getSpecifierForModuleSymbol(symbol: Symbol, context: NodeBuilderContext) {
    //       let file = getDeclarationOfKind<SourceFile>(symbol, SyntaxKind.SourceFile);
    //       if (!file) {
    //           const equivalentFileSymbol = firstDefined(symbol.declarations, d => getFileSymbolIfFileSymbolExportEqualsContainer(d, symbol));
    //           if (equivalentFileSymbol) {
    //               file = getDeclarationOfKind<SourceFile>(equivalentFileSymbol, SyntaxKind.SourceFile);
    //           }
    //       }
    //       if (file && file.moduleName !== undefined) {
    //           // Use the amd name if it is available
    //           return file.moduleName;
    //       }
    //       if (!file) {
    //           if (context.tracker.trackReferencedAmbientModule) {
    //               const ambientDecls = filter(symbol.declarations, isAmbientModule);
    //               if (length(ambientDecls)) {
    //                   for (const decl of ambientDecls!) {
    //                       context.tracker.trackReferencedAmbientModule(decl, symbol);
    //                   }
    //               }
    //           }
    //           if (ambientModuleSymbolRegex.test(symbol.escapedName as string)) {
    //               return (symbol.escapedName as string).substring(1, (symbol.escapedName as string).length - 1);
    //           }
    //       }
    //       if (!context.enclosingDeclaration || !context.tracker.moduleResolverHost) {
    //           // If there's no context declaration, we can't lookup a non-ambient specifier, so we just use the symbol name
    //           if (ambientModuleSymbolRegex.test(symbol.escapedName as string)) {
    //               return (symbol.escapedName as string).substring(1, (symbol.escapedName as string).length - 1);
    //           }
    //           return getSourceFileOfNode(getNonAugmentationDeclaration(symbol)!).fileName; // A resolver may not be provided for baselines and errors - in those cases we use the fileName in full
    //       }
    //       const contextFile = getSourceFileOfNode(getOriginalNode(context.enclosingDeclaration));
    //       const links = getSymbolLinks(symbol);
    //       let specifier = links.specifierCache && links.specifierCache.get(contextFile.path);
    //       if (!specifier) {
    //           const isBundle = !!outFile(compilerOptions);
    //           // For declaration bundles, we need to generate absolute paths relative to the common source dir for imports,
    //           // just like how the declaration emitter does for the ambient module declarations - we can easily accomplish this
    //           // using the `baseUrl` compiler option (which we would otherwise never use in declaration emit) and a non-relative
    //           // specifier preference
    //           const { moduleResolverHost } = context.tracker;
    //           const specifierCompilerOptions = isBundle ? { ...compilerOptions, baseUrl: moduleResolverHost.getCommonSourceDirectory() } : compilerOptions;
    //           specifier = first(moduleSpecifiers.getModuleSpecifiers(
    //               symbol,
    //               checker,
    //               specifierCompilerOptions,
    //               contextFile,
    //               moduleResolverHost,
    //               { importModuleSpecifierPreference: isBundle ? "non-relative" : "project-relative", importModuleSpecifierEnding: isBundle ? "minimal" : undefined },
    //           ));
    //           links.specifierCache ??= new Map();
    //           links.specifierCache.set(contextFile.path, specifier);
    //       }
    //       return specifier;
    //   }

    //   function symbolToEntityNameNode(symbol: Symbol): EntityName {
    //       const identifier = factory.createIdentifier(unescapeLeadingUnderscores(symbol.escapedName));
    //       return symbol.parent ? factory.createQualifiedName(symbolToEntityNameNode(symbol.parent), identifier) : identifier;
    //   }

    //   function symbolToTypeNode(symbol: Symbol, context: NodeBuilderContext, meaning: SymbolFlags, overrideTypeArguments?: readonly TypeNode[]): TypeNode {
    //       const chain = lookupSymbolChain(symbol, context, meaning, !(context.flags & NodeBuilderFlags.UseAliasDefinedOutsideCurrentScope)); // If we're using aliases outside the current scope, dont bother with the module

    //       const isTypeOf = meaning === SymbolFlags.Value;
    //       if (some(chain[0].declarations, hasNonGlobalAugmentationExternalModuleSymbol)) {
    //           // module is root, must use `ImportTypeNode`
    //           const nonRootParts = chain.length > 1 ? createAccessFromSymbolChain(chain, chain.length - 1, 1) : undefined;
    //           const typeParameterNodes = overrideTypeArguments || lookupTypeParameterNodes(chain, 0, context);
    //           const specifier = getSpecifierForModuleSymbol(chain[0], context);
    //           if (!(context.flags & NodeBuilderFlags.AllowNodeModulesRelativePaths) && getEmitModuleResolutionKind(compilerOptions) !== ModuleResolutionKind.Classic && specifier.indexOf("/node_modules/") >= 0) {
    //               // If ultimately we can only name the symbol with a reference that dives into a `node_modules` folder, we should error
    //               // since declaration files with these kinds of references are liable to fail when published :(
    //               context.encounteredError = true;
    //               if (context.tracker.reportLikelyUnsafeImportRequiredError) {
    //                   context.tracker.reportLikelyUnsafeImportRequiredError(specifier);
    //               }
    //           }
    //           const lit = factory.createLiteralTypeNode(factory.createStringLiteral(specifier));
    //           if (context.tracker.trackExternalModuleSymbolOfImportTypeNode) context.tracker.trackExternalModuleSymbolOfImportTypeNode(chain[0]);
    //           context.approximateLength += specifier.length + 10; // specifier + import("")
    //           if (!nonRootParts || isEntityName(nonRootParts)) {
    //               if (nonRootParts) {
    //                   const lastId = isIdentifier(nonRootParts) ? nonRootParts : nonRootParts.right;
    //                   lastId.typeArguments = undefined;
    //               }
    //               return factory.createImportTypeNode(lit, nonRootParts as EntityName, typeParameterNodes as readonly TypeNode[], isTypeOf);
    //           }
    //           else {
    //               const splitNode = getTopmostIndexedAccessType(nonRootParts);
    //               const qualifier = (splitNode.objectType as TypeReferenceNode).typeName;
    //               return factory.createIndexedAccessTypeNode(factory.createImportTypeNode(lit, qualifier, typeParameterNodes as readonly TypeNode[], isTypeOf), splitNode.indexType);
    //           }
    //       }

    //       const entityName = createAccessFromSymbolChain(chain, chain.length - 1, 0);
    //       if (isIndexedAccessTypeNode(entityName)) {
    //           return entityName; // Indexed accesses can never be `typeof`
    //       }
    //       if (isTypeOf) {
    //           return factory.createTypeQueryNode(entityName);
    //       }
    //       else {
    //           const lastId = isIdentifier(entityName) ? entityName : entityName.right;
    //           const lastTypeArgs = lastId.typeArguments;
    //           lastId.typeArguments = undefined;
    //           return factory.createTypeReferenceNode(entityName, lastTypeArgs as NodeArray<TypeNode>);
    //       }

    //       function createAccessFromSymbolChain(chain: Symbol[], index: number, stopper: number): EntityName | IndexedAccessTypeNode {
    //           const typeParameterNodes = index === (chain.length - 1) ? overrideTypeArguments : lookupTypeParameterNodes(chain, index, context);
    //           const symbol = chain[index];

    //           const parent = chain[index - 1];
    //           let symbolName: string | undefined;
    //           if (index === 0) {
    //               context.flags |= NodeBuilderFlags.InInitialEntityName;
    //               symbolName = getNameOfSymbolAsWritten(symbol, context);
    //               context.approximateLength += (symbolName ? symbolName.length : 0) + 1;
    //               context.flags ^= NodeBuilderFlags.InInitialEntityName;
    //           }
    //           else {
    //               if (parent && getExportsOfSymbol(parent)) {
    //                   const exports = getExportsOfSymbol(parent);
    //                   forEachEntry(exports, (ex, name) => {
    //                       if (getSymbolIfSameReference(ex, symbol) && !isLateBoundName(name) && name !== InternalSymbolName.ExportEquals) {
    //                           symbolName = unescapeLeadingUnderscores(name);
    //                           return true;
    //                       }
    //                   });
    //               }
    //           }
    //           if (!symbolName) {
    //               symbolName = getNameOfSymbolAsWritten(symbol, context);
    //           }
    //           context.approximateLength += symbolName.length + 1;

    //           if (!(context.flags & NodeBuilderFlags.ForbidIndexedAccessSymbolReferences) && parent &&
    //               getMembersOfSymbol(parent) && getMembersOfSymbol(parent).get(symbol.escapedName) &&
    //               getSymbolIfSameReference(getMembersOfSymbol(parent).get(symbol.escapedName)!, symbol)) {
    //               // Should use an indexed access
    //               const LHS = createAccessFromSymbolChain(chain, index - 1, stopper);
    //               if (isIndexedAccessTypeNode(LHS)) {
    //                   return factory.createIndexedAccessTypeNode(LHS, factory.createLiteralTypeNode(factory.createStringLiteral(symbolName)));
    //               }
    //               else {
    //                   return factory.createIndexedAccessTypeNode(factory.createTypeReferenceNode(LHS, typeParameterNodes as readonly TypeNode[]), factory.createLiteralTypeNode(factory.createStringLiteral(symbolName)));
    //               }
    //           }

    //           const identifier = setEmitFlags(factory.createIdentifier(symbolName, typeParameterNodes), EmitFlags.NoAsciiEscaping);
    //           identifier.symbol = symbol;

    //           if (index > stopper) {
    //               const LHS = createAccessFromSymbolChain(chain, index - 1, stopper);
    //               if (!isEntityName(LHS)) {
    //                   return Debug.fail("Impossible construct - an export of an indexed access cannot be reachable");
    //               }
    //               return factory.createQualifiedName(LHS, identifier);
    //           }
    //           return identifier;
    //       }
    //   }

    //   function typeParameterShadowsNameInScope(escapedName: __String, context: NodeBuilderContext, type: TypeParameter) {
    //       const result = resolveName(context.enclosingDeclaration, escapedName, SymbolFlags.Type, /*nameNotFoundArg*/ undefined, escapedName, /*isUse*/ false);
    //       if (result) {
    //           if (result.flags & SymbolFlags.TypeParameter && result === type.symbol) {
    //               return false;
    //           }
    //           return true;
    //       }
    //       return false;
    //   }

    //   function typeParameterToName(type: TypeParameter, context: NodeBuilderContext) {
    //       if (context.flags & NodeBuilderFlags.GenerateNamesForShadowedTypeParams && context.typeParameterNames) {
    //           const cached = context.typeParameterNames.get(getTypeId(type));
    //           if (cached) {
    //               return cached;
    //           }
    //       }
    //       let result = symbolToName(type.symbol, context, SymbolFlags.Type, /*expectsIdentifier*/ true);
    //       if (!(result.kind & SyntaxKind.Identifier)) {
    //           return factory.createIdentifier("(Missing type parameter)");
    //       }
    //       if (context.flags & NodeBuilderFlags.GenerateNamesForShadowedTypeParams) {
    //           const rawtext = result.escapedText as string;
    //           let i = context.typeParameterNamesByTextNextNameCount?.get(rawtext) || 0;
    //           let text = rawtext;
    //           while (context.typeParameterNamesByText?.has(text) || typeParameterShadowsNameInScope(text as __String, context, type)) {
    //               i++;
    //               text = `${rawtext}_${i}`;
    //           }
    //           if (text !== rawtext) {
    //               result = factory.createIdentifier(text, result.typeArguments);
    //           }
    //           // avoiding iterations of the above loop turns out to be worth it when `i` starts to get large, so we cache the max
    //           // `i` we've used thus far, to save work later
    //           (context.typeParameterNamesByTextNextNameCount ||= new Map()).set(rawtext, i);
    //           (context.typeParameterNames ||= new Map()).set(getTypeId(type), result);
    //           (context.typeParameterNamesByText ||= new Set()).add(rawtext);
    //       }
    //       return result;
    //   }

    //   function symbolToName(symbol: Symbol, context: NodeBuilderContext, meaning: SymbolFlags, expectsIdentifier: true): Identifier;
    //   function symbolToName(symbol: Symbol, context: NodeBuilderContext, meaning: SymbolFlags, expectsIdentifier: false): EntityName;
    //   function symbolToName(symbol: Symbol, context: NodeBuilderContext, meaning: SymbolFlags, expectsIdentifier: boolean): EntityName {
    //       const chain = lookupSymbolChain(symbol, context, meaning);

    //       if (expectsIdentifier && chain.length !== 1
    //           && !context.encounteredError
    //           && !(context.flags & NodeBuilderFlags.AllowQualifiedNameInPlaceOfIdentifier)) {
    //           context.encounteredError = true;
    //       }
    //       return createEntityNameFromSymbolChain(chain, chain.length - 1);

    //       function createEntityNameFromSymbolChain(chain: Symbol[], index: number): EntityName {
    //           const typeParameterNodes = lookupTypeParameterNodes(chain, index, context);
    //           const symbol = chain[index];

    //           if (index === 0) {
    //               context.flags |= NodeBuilderFlags.InInitialEntityName;
    //           }
    //           const symbolName = getNameOfSymbolAsWritten(symbol, context);
    //           if (index === 0) {
    //               context.flags ^= NodeBuilderFlags.InInitialEntityName;
    //           }

    //           const identifier = setEmitFlags(factory.createIdentifier(symbolName, typeParameterNodes), EmitFlags.NoAsciiEscaping);
    //           identifier.symbol = symbol;

    //           return index > 0 ? factory.createQualifiedName(createEntityNameFromSymbolChain(chain, index - 1), identifier) : identifier;
    //       }
    //   }

    //   function symbolToExpression(symbol: Symbol, context: NodeBuilderContext, meaning: SymbolFlags) {
    //       const chain = lookupSymbolChain(symbol, context, meaning);

    //       return createExpressionFromSymbolChain(chain, chain.length - 1);

    //       function createExpressionFromSymbolChain(chain: Symbol[], index: number): Expression {
    //           const typeParameterNodes = lookupTypeParameterNodes(chain, index, context);
    //           const symbol = chain[index];

    //           if (index === 0) {
    //               context.flags |= NodeBuilderFlags.InInitialEntityName;
    //           }
    //           let symbolName = getNameOfSymbolAsWritten(symbol, context);
    //           if (index === 0) {
    //               context.flags ^= NodeBuilderFlags.InInitialEntityName;
    //           }
    //           let firstChar = symbolName.charCodeAt(0);

    //           if (isSingleOrDoubleQuote(firstChar) && some(symbol.declarations, hasNonGlobalAugmentationExternalModuleSymbol)) {
    //               return factory.createStringLiteral(getSpecifierForModuleSymbol(symbol, context));
    //           }
    //           const canUsePropertyAccess = firstChar === CharacterCodes.hash ?
    //               symbolName.length > 1 && isIdentifierStart(symbolName.charCodeAt(1), languageVersion) :
    //               isIdentifierStart(firstChar, languageVersion);
    //           if (index === 0 || canUsePropertyAccess) {
    //               const identifier = setEmitFlags(factory.createIdentifier(symbolName, typeParameterNodes), EmitFlags.NoAsciiEscaping);
    //               identifier.symbol = symbol;

    //               return index > 0 ? factory.createPropertyAccessExpression(createExpressionFromSymbolChain(chain, index - 1), identifier) : identifier;
    //           }
    //           else {
    //               if (firstChar === CharacterCodes.openBracket) {
    //                   symbolName = symbolName.substring(1, symbolName.length - 1);
    //                   firstChar = symbolName.charCodeAt(0);
    //               }
    //               let expression: Expression | undefined;
    //               if (isSingleOrDoubleQuote(firstChar)) {
    //                   expression = factory.createStringLiteral(
    //                       symbolName
    //                           .substring(1, symbolName.length - 1)
    //                           .replace(/\\./g, s => s.substring(1)),
    //                       firstChar === CharacterCodes.singleQuote);
    //               }
    //               else if (("" + +symbolName) === symbolName) {
    //                   expression = factory.createNumericLiteral(+symbolName);
    //               }
    //               if (!expression) {
    //                   expression = setEmitFlags(factory.createIdentifier(symbolName, typeParameterNodes), EmitFlags.NoAsciiEscaping);
    //                   expression.symbol = symbol;
    //               }
    //               return factory.createElementAccessExpression(createExpressionFromSymbolChain(chain, index - 1), expression);
    //           }
    //       }
    //   }

    //   function isStringNamed(d: Declaration) {
    //       const name = getNameOfDeclaration(d);
    //       return !!name && isStringLiteral(name);
    //   }

    //   function isSingleQuotedStringNamed(d: Declaration) {
    //       const name = getNameOfDeclaration(d);
    //       return !!(name && isStringLiteral(name) && (name.singleQuote || !nodeIsSynthesized(name) && startsWith(getTextOfNode(name, /*includeTrivia*/ false), "'")));
    //   }

    //   function getPropertyNameNodeForSymbol(symbol: Symbol, context: NodeBuilderContext) {
    //       const singleQuote = !!length(symbol.declarations) && every(symbol.declarations, isSingleQuotedStringNamed);
    //       const fromNameType = getPropertyNameNodeForSymbolFromNameType(symbol, context, singleQuote);
    //       if (fromNameType) {
    //           return fromNameType;
    //       }
    //       const rawName = unescapeLeadingUnderscores(symbol.escapedName);
    //       const stringNamed = !!length(symbol.declarations) && every(symbol.declarations, isStringNamed);
    //       return createPropertyNameNodeForIdentifierOrLiteral(rawName, stringNamed, singleQuote);
    //   }

    //   // See getNameForSymbolFromNameType for a stringy equivalent
    //   function getPropertyNameNodeForSymbolFromNameType(symbol: Symbol, context: NodeBuilderContext, singleQuote?: boolean) {
    //       const nameType = getSymbolLinks(symbol).nameType;
    //       if (nameType) {
    //           if (nameType.flags & TypeFlags.StringOrNumberLiteral) {
    //               const name = "" + (nameType as StringLiteralType | NumberLiteralType).value;
    //               if (!isIdentifierText(name, getEmitScriptTarget(compilerOptions)) && !isNumericLiteralName(name)) {
    //                   return factory.createStringLiteral(name, !!singleQuote);
    //               }
    //               if (isNumericLiteralName(name) && startsWith(name, "-")) {
    //                   return factory.createComputedPropertyName(factory.createNumericLiteral(+name));
    //               }
    //               return createPropertyNameNodeForIdentifierOrLiteral(name);
    //           }
    //           if (nameType.flags & TypeFlags.UniqueESSymbol) {
    //               return factory.createComputedPropertyName(symbolToExpression((nameType as UniqueESSymbolType).symbol, context, SymbolFlags.Value));
    //           }
    //       }
    //   }

    //   function createPropertyNameNodeForIdentifierOrLiteral(name: string, stringNamed?: boolean, singleQuote?: boolean) {
    //       return isIdentifierText(name, getEmitScriptTarget(compilerOptions)) ? factory.createIdentifier(name) :
    //           !stringNamed && isNumericLiteralName(name) && +name >= 0 ? factory.createNumericLiteral(+name) :
    //           factory.createStringLiteral(name, !!singleQuote);
    //   }

    //   function cloneNodeBuilderContext(context: NodeBuilderContext): NodeBuilderContext {
    //       const initial: NodeBuilderContext = { ...context };
    //       // Make type parameters created within this context not consume the name outside this context
    //       // The symbol serializer ends up creating many sibling scopes that all need "separate" contexts when
    //       // it comes to naming things - within a normal `typeToTypeNode` call, the node builder only ever descends
    //       // through the type tree, so the only cases where we could have used distinct sibling scopes was when there
    //       // were multiple generic overloads with similar generated type parameter names
    //       // The effect:
    //       // When we write out
    //       // export const x: <T>(x: T) => T
    //       // export const y: <T>(x: T) => T
    //       // we write it out like that, rather than as
    //       // export const x: <T>(x: T) => T
    //       // export const y: <T_1>(x: T_1) => T_1
    //       if (initial.typeParameterNames) {
    //           initial.typeParameterNames = new Map(initial.typeParameterNames);
    //       }
    //       if (initial.typeParameterNamesByText) {
    //           initial.typeParameterNamesByText = new Set(initial.typeParameterNamesByText);
    //       }
    //       if (initial.typeParameterSymbolList) {
    //           initial.typeParameterSymbolList = new Set(initial.typeParameterSymbolList);
    //       }
    //       initial.tracker = wrapSymbolTrackerToReportForContext(initial, initial.tracker);
    //       return initial;
    //   }

    //   function getDeclarationWithTypeAnnotation(symbol: Symbol, enclosingDeclaration: Node | undefined) {
    //       return symbol.declarations && find(symbol.declarations, s => !!getEffectiveTypeAnnotationNode(s) && (!enclosingDeclaration || !!findAncestor(s, n => n === enclosingDeclaration)));
    //   }

    //   function existingTypeNodeIsNotReferenceOrIsReferenceWithCompatibleTypeArgumentCount(existing: TypeNode, type: Type) {
    //       return !(getObjectFlags(type) & ObjectFlags.Reference) || !isTypeReferenceNode(existing) || length(existing.typeArguments) >= getMinTypeArgumentCount((type as TypeReference).target.typeParameters);
    //   }

    //   /**
    //    * Unlike `typeToTypeNodeHelper`, this handles setting up the `AllowUniqueESSymbolType` flag
    //    * so a `unique symbol` is returned when appropriate for the input symbol, rather than `typeof sym`
    //    */
    //   function serializeTypeForDeclaration(context: NodeBuilderContext, type: Type, symbol: Symbol, enclosingDeclaration: Node | undefined, includePrivateSymbol?: (s: Symbol) => void, bundled?: boolean) {
    //       if (!isErrorType(type) && enclosingDeclaration) {
    //           const declWithExistingAnnotation = getDeclarationWithTypeAnnotation(symbol, enclosingDeclaration);
    //           if (declWithExistingAnnotation && !isFunctionLikeDeclaration(declWithExistingAnnotation) && !isGetAccessorDeclaration(declWithExistingAnnotation)) {
    //               // try to reuse the existing annotation
    //               const existing = getEffectiveTypeAnnotationNode(declWithExistingAnnotation)!;
    //               if (getTypeFromTypeNode(existing) === type && existingTypeNodeIsNotReferenceOrIsReferenceWithCompatibleTypeArgumentCount(existing, type)) {
    //                   const result = serializeExistingTypeNode(context, existing, includePrivateSymbol, bundled);
    //                   if (result) {
    //                       return result;
    //                   }
    //               }
    //           }
    //       }
    //       const oldFlags = context.flags;
    //       if (type.flags & TypeFlags.UniqueESSymbol &&
    //           type.symbol === symbol && (!context.enclosingDeclaration || some(symbol.declarations, d => getSourceFileOfNode(d) === getSourceFileOfNode(context.enclosingDeclaration!)))) {
    //           context.flags |= NodeBuilderFlags.AllowUniqueESSymbolType;
    //       }
    //       const result = typeToTypeNodeHelper(type, context);
    //       context.flags = oldFlags;
    //       return result;
    //   }

    //   function serializeReturnTypeForSignature(context: NodeBuilderContext, type: Type, signature: Signature, includePrivateSymbol?: (s: Symbol) => void, bundled?: boolean) {
    //       if (!isErrorType(type) && context.enclosingDeclaration) {
    //           const annotation = signature.declaration && getEffectiveReturnTypeNode(signature.declaration);
    //           if (!!findAncestor(annotation, n => n === context.enclosingDeclaration) && annotation) {
    //               const annotated = getTypeFromTypeNode(annotation);
    //               const thisInstantiated = annotated.flags & TypeFlags.TypeParameter && (annotated as TypeParameter).isThisType ? instantiateType(annotated, signature.mapper) : annotated;
    //               if (thisInstantiated === type && existingTypeNodeIsNotReferenceOrIsReferenceWithCompatibleTypeArgumentCount(annotation, type)) {
    //                   const result = serializeExistingTypeNode(context, annotation, includePrivateSymbol, bundled);
    //                   if (result) {
    //                       return result;
    //                   }
    //               }
    //           }
    //       }
    //       return typeToTypeNodeHelper(type, context);
    //   }

    //   function trackExistingEntityName<T extends EntityNameOrEntityNameExpression>(node: T, context: NodeBuilderContext, includePrivateSymbol?: (s: Symbol) => void) {
    //       let introducesError = false;
    //       const leftmost = getFirstIdentifier(node);
    //       if (isInJSFile(node) && (isExportsIdentifier(leftmost) || isModuleExportsAccessExpression(leftmost.parent) || (isQualifiedName(leftmost.parent) && isModuleIdentifier(leftmost.parent.left) && isExportsIdentifier(leftmost.parent.right)))) {
    //           introducesError = true;
    //           return { introducesError, node };
    //       }
    //       const sym = resolveEntityName(leftmost, SymbolFlags.All, /*ignoreErrors*/ true, /*dontResolveALias*/ true);
    //       if (sym) {
    //           if (isSymbolAccessible(sym, context.enclosingDeclaration, SymbolFlags.All, /*shouldComputeAliasesToMakeVisible*/ false).accessibility !== SymbolAccessibility.Accessible) {
    //               introducesError = true;
    //           }
    //           else {
    //               context.tracker?.trackSymbol?.(sym, context.enclosingDeclaration, SymbolFlags.All);
    //               includePrivateSymbol?.(sym);
    //           }
    //           if (isIdentifier(node)) {
    //               const name = sym.flags & SymbolFlags.TypeParameter ? typeParameterToName(getDeclaredTypeOfSymbol(sym), context) : factory.cloneNode(node);
    //               name.symbol = sym; // for quickinfo, which uses identifier symbol information
    //               return { introducesError, node: setEmitFlags(setOriginalNode(name, node), EmitFlags.NoAsciiEscaping) };
    //           }
    //       }

    //       return { introducesError, node };
    //   }

    //   function serializeExistingTypeNode(context: NodeBuilderContext, existing: TypeNode, includePrivateSymbol?: (s: Symbol) => void, bundled?: boolean) {
    //       if (cancellationToken && cancellationToken.throwIfCancellationRequested) {
    //           cancellationToken.throwIfCancellationRequested();
    //       }
    //       let hadError = false;
    //       const file = getSourceFileOfNode(existing);
    //       const transformed = visitNode(existing, visitExistingNodeTreeSymbols);
    //       if (hadError) {
    //           return undefined;
    //       }
    //       return transformed === existing ? setTextRange(factory.cloneNode(existing), existing) : transformed;

    //       function visitExistingNodeTreeSymbols<T extends Node>(node: T): Node {
    //           // We don't _actually_ support jsdoc namepath types, emit `any` instead
    //           if (isJSDocAllType(node) || node.kind === SyntaxKind.JSDocNamepathType) {
    //               return factory.createKeywordTypeNode(SyntaxKind.AnyKeyword);
    //           }
    //           if (isJSDocUnknownType(node)) {
    //               return factory.createKeywordTypeNode(SyntaxKind.UnknownKeyword);
    //           }
    //           if (isJSDocNullableType(node)) {
    //               return factory.createUnionTypeNode([visitNode(node.type, visitExistingNodeTreeSymbols), factory.createLiteralTypeNode(factory.createNull())]);
    //           }
    //           if (isJSDocOptionalType(node)) {
    //               return factory.createUnionTypeNode([visitNode(node.type, visitExistingNodeTreeSymbols), factory.createKeywordTypeNode(SyntaxKind.UndefinedKeyword)]);
    //           }
    //           if (isJSDocNonNullableType(node)) {
    //               return visitNode(node.type, visitExistingNodeTreeSymbols);
    //           }
    //           if (isJSDocVariadicType(node)) {
    //               return factory.createArrayTypeNode(visitNode((node as JSDocVariadicType).type, visitExistingNodeTreeSymbols));
    //           }
    //           if (isJSDocTypeLiteral(node)) {
    //               return factory.createTypeLiteralNode(map(node.jsDocPropertyTags, t => {
    //                   const name = isIdentifier(t.name) ? t.name : t.name.right;
    //                   const typeViaParent = getTypeOfPropertyOfType(getTypeFromTypeNode(node), name.escapedText);
    //                   const overrideTypeNode = typeViaParent && t.typeExpression && getTypeFromTypeNode(t.typeExpression.type) !== typeViaParent ? typeToTypeNodeHelper(typeViaParent, context) : undefined;

    //                   return factory.createPropertySignature(
    //                       /*modifiers*/ undefined,
    //                       name,
    //                       t.isBracketed || t.typeExpression && isJSDocOptionalType(t.typeExpression.type) ? factory.createToken(SyntaxKind.QuestionToken) : undefined,
    //                       overrideTypeNode || (t.typeExpression && visitNode(t.typeExpression.type, visitExistingNodeTreeSymbols)) || factory.createKeywordTypeNode(SyntaxKind.AnyKeyword)
    //                   );
    //               }));
    //           }
    //           if (isTypeReferenceNode(node) && isIdentifier(node.typeName) && node.typeName.escapedText === "") {
    //               return setOriginalNode(factory.createKeywordTypeNode(SyntaxKind.AnyKeyword), node);
    //           }
    //           if ((isExpressionWithTypeArguments(node) || isTypeReferenceNode(node)) && isJSDocIndexSignature(node)) {
    //               return factory.createTypeLiteralNode([factory.createIndexSignature(
    //                   /*decorators*/ undefined,
    //                   /*modifiers*/ undefined,
    //                   [factory.createParameterDeclaration(
    //                       /*decorators*/ undefined,
    //                       /*modifiers*/ undefined,
    //                       /*dotdotdotToken*/ undefined,
    //                       "x",
    //                       /*questionToken*/ undefined,
    //                       visitNode(node.typeArguments![0], visitExistingNodeTreeSymbols)
    //                   )],
    //                   visitNode(node.typeArguments![1], visitExistingNodeTreeSymbols)
    //               )]);
    //           }
    //           if (isJSDocFunctionType(node)) {
    //               if (isJSDocConstructSignature(node)) {
    //                   let newTypeNode: TypeNode | undefined;
    //                   return factory.createConstructorTypeNode(
    //                       node.modifiers,
    //                       visitNodes(node.typeParameters, visitExistingNodeTreeSymbols),
    //                       mapDefined(node.parameters, (p, i) => p.name && isIdentifier(p.name) && p.name.escapedText === "new" ? (newTypeNode = p.type, undefined) : factory.createParameterDeclaration(
    //                           /*decorators*/ undefined,
    //                           /*modifiers*/ undefined,
    //                           getEffectiveDotDotDotForParameter(p),
    //                           getNameForJSDocFunctionParameter(p, i),
    //                           p.questionToken,
    //                           visitNode(p.type, visitExistingNodeTreeSymbols),
    //                           /*initializer*/ undefined
    //                       )),
    //                       visitNode(newTypeNode || node.type, visitExistingNodeTreeSymbols) || factory.createKeywordTypeNode(SyntaxKind.AnyKeyword)
    //                   );
    //               }
    //               else {
    //                   return factory.createFunctionTypeNode(
    //                       visitNodes(node.typeParameters, visitExistingNodeTreeSymbols),
    //                       map(node.parameters, (p, i) => factory.createParameterDeclaration(
    //                           /*decorators*/ undefined,
    //                           /*modifiers*/ undefined,
    //                           getEffectiveDotDotDotForParameter(p),
    //                           getNameForJSDocFunctionParameter(p, i),
    //                           p.questionToken,
    //                           visitNode(p.type, visitExistingNodeTreeSymbols),
    //                           /*initializer*/ undefined
    //                       )),
    //                       visitNode(node.type, visitExistingNodeTreeSymbols) || factory.createKeywordTypeNode(SyntaxKind.AnyKeyword)
    //                   );
    //               }
    //           }
    //           if (isTypeReferenceNode(node) && isInJSDoc(node) && (!existingTypeNodeIsNotReferenceOrIsReferenceWithCompatibleTypeArgumentCount(node, getTypeFromTypeNode(node)) || getIntendedTypeFromJSDocTypeReference(node) || unknownSymbol === resolveTypeReferenceName(node, SymbolFlags.Type, /*ignoreErrors*/ true))) {
    //               return setOriginalNode(typeToTypeNodeHelper(getTypeFromTypeNode(node), context), node);
    //           }
    //           if (isLiteralImportTypeNode(node)) {
    //               const nodeSymbol = getNodeLinks(node).resolvedSymbol;
    //               if (isInJSDoc(node) &&
    //                   nodeSymbol &&
    //                   (
    //                       // The import type resolved using jsdoc fallback logic
    //                       (!node.isTypeOf && !(nodeSymbol.flags & SymbolFlags.Type)) ||
    //                       // The import type had type arguments autofilled by js fallback logic
    //                       !(length(node.typeArguments) >= getMinTypeArgumentCount(getLocalTypeParametersOfClassOrInterfaceOrTypeAlias(nodeSymbol)))
    //                   )
    //               ) {
    //                   return setOriginalNode(typeToTypeNodeHelper(getTypeFromTypeNode(node), context), node);
    //               }
    //               return factory.updateImportTypeNode(
    //                   node,
    //                   factory.updateLiteralTypeNode(node.argument, rewriteModuleSpecifier(node, node.argument.literal)),
    //                   node.qualifier,
    //                   visitNodes(node.typeArguments, visitExistingNodeTreeSymbols, isTypeNode),
    //                   node.isTypeOf
    //               );
    //           }

    //           if (isEntityName(node) || isEntityNameExpression(node)) {
    //               const { introducesError, node: result } = trackExistingEntityName(node, context, includePrivateSymbol);
    //               hadError = hadError || introducesError;
    //               if (result !== node) {
    //                   return result;
    //               }
    //           }

    //           if (file && isTupleTypeNode(node) && (getLineAndCharacterOfPosition(file, node.pos).line === getLineAndCharacterOfPosition(file, node.end).line)) {
    //               setEmitFlags(node, EmitFlags.SingleLine);
    //           }

    //           return visitEachChild(node, visitExistingNodeTreeSymbols, nullTransformationContext);

    //           function getEffectiveDotDotDotForParameter(p: ParameterDeclaration) {
    //               return p.dotDotDotToken || (p.type && isJSDocVariadicType(p.type) ? factory.createToken(SyntaxKind.DotDotDotToken) : undefined);
    //           }

    //           /** Note that `new:T` parameters are not handled, but should be before calling this function. */
    //           function getNameForJSDocFunctionParameter(p: ParameterDeclaration, index: number) {
    //               return p.name && isIdentifier(p.name) && p.name.escapedText === "this" ? "this"
    //                   : getEffectiveDotDotDotForParameter(p) ? `args`
    //                   : `arg${index}`;
    //           }

    //           function rewriteModuleSpecifier(parent: ImportTypeNode, lit: StringLiteral) {
    //               if (bundled) {
    //                   if (context.tracker && context.tracker.moduleResolverHost) {
    //                       const targetFile = getExternalModuleFileFromDeclaration(parent);
    //                       if (targetFile) {
    //                           const getCanonicalFileName = createGetCanonicalFileName(!!host.useCaseSensitiveFileNames);
    //                           const resolverHost = {
    //                               getCanonicalFileName,
    //                               getCurrentDirectory: () => context.tracker.moduleResolverHost!.getCurrentDirectory(),
    //                               getCommonSourceDirectory: () => context.tracker.moduleResolverHost!.getCommonSourceDirectory()
    //                           };
    //                           const newName = getResolvedExternalModuleName(resolverHost, targetFile);
    //                           return factory.createStringLiteral(newName);
    //                       }
    //                   }
    //               }
    //               else {
    //                   if (context.tracker && context.tracker.trackExternalModuleSymbolOfImportTypeNode) {
    //                       const moduleSym = resolveExternalModuleNameWorker(lit, lit, /*moduleNotFoundError*/ undefined);
    //                       if (moduleSym) {
    //                           context.tracker.trackExternalModuleSymbolOfImportTypeNode(moduleSym);
    //                       }
    //                   }
    //               }
    //               return lit;
    //           }
    //       }
    //   }

    //   function symbolTableToDeclarationStatements(symbolTable: SymbolTable, context: NodeBuilderContext, bundled?: boolean): Statement[] {
    //       const serializePropertySymbolForClass = makeSerializePropertySymbol<ClassElement>(factory.createPropertyDeclaration, SyntaxKind.MethodDeclaration, /*useAcessors*/ true);
    //       const serializePropertySymbolForInterfaceWorker = makeSerializePropertySymbol<TypeElement>((_decorators, mods, name, question, type) => factory.createPropertySignature(mods, name, question, type), SyntaxKind.MethodSignature, /*useAcessors*/ false);

    //       // TODO: Use `setOriginalNode` on original declaration names where possible so these declarations see some kind of
    //       // declaration mapping

    //       // We save the enclosing declaration off here so it's not adjusted by well-meaning declaration
    //       // emit codepaths which want to apply more specific contexts (so we can still refer to the root real declaration
    //       // we're trying to emit from later on)
    //       const enclosingDeclaration = context.enclosingDeclaration!;
    //       let results: Statement[] = [];
    //       const visitedSymbols = new Set<number>();
    //       const deferredPrivatesStack: ESMap<SymbolId, Symbol>[] = [];
    //       const oldcontext = context;
    //       context = {
    //           ...oldcontext,
    //           usedSymbolNames: new Set(oldcontext.usedSymbolNames),
    //           remappedSymbolNames: new Map(),
    //           tracker: {
    //               ...oldcontext.tracker,
    //               trackSymbol: (sym, decl, meaning) => {
    //                   const accessibleResult = isSymbolAccessible(sym, decl, meaning, /*computeAliases*/ false);
    //                   if (accessibleResult.accessibility === SymbolAccessibility.Accessible) {
    //                       // Lookup the root symbol of the chain of refs we'll use to access it and serialize it
    //                       const chain = lookupSymbolChainWorker(sym, context, meaning);
    //                       if (!(sym.flags & SymbolFlags.Property)) {
    //                           includePrivateSymbol(chain[0]);
    //                       }
    //                   }
    //                   else if (oldcontext.tracker && oldcontext.tracker.trackSymbol) {
    //                       return oldcontext.tracker.trackSymbol(sym, decl, meaning);
    //                   }
    //                   return false;
    //               },
    //           },
    //       };
    //       context.tracker = wrapSymbolTrackerToReportForContext(context, context.tracker);
    //       forEachEntry(symbolTable, (symbol, name) => {
    //           const baseName = unescapeLeadingUnderscores(name);
    //           void getInternalSymbolName(symbol, baseName); // Called to cache values into `usedSymbolNames` and `remappedSymbolNames`
    //       });
    //       let addingDeclare = !bundled;
    //       const exportEquals = symbolTable.get(InternalSymbolName.ExportEquals);
    //       if (exportEquals && symbolTable.size > 1 && exportEquals.flags & SymbolFlags.Alias) {
    //           symbolTable = createSymbolTable();
    //           // Remove extraneous elements from root symbol table (they'll be mixed back in when the target of the `export=` is looked up)
    //           symbolTable.set(InternalSymbolName.ExportEquals, exportEquals);
    //       }

    //       visitSymbolTable(symbolTable);
    //       return mergeRedundantStatements(results);

    //       function isIdentifierAndNotUndefined(node: Node | undefined): node is Identifier {
    //           return !!node && node.kind === SyntaxKind.Identifier;
    //       }

    //       function getNamesOfDeclaration(statement: Statement): Identifier[] {
    //           if (isVariableStatement(statement)) {
    //               return filter(map(statement.declarationList.declarations, getNameOfDeclaration), isIdentifierAndNotUndefined);
    //           }
    //           return filter([getNameOfDeclaration(statement as DeclarationStatement)], isIdentifierAndNotUndefined);
    //       }

    //       function flattenExportAssignedNamespace(statements: Statement[]) {
    //           const exportAssignment = find(statements, isExportAssignment);
    //           const nsIndex = findIndex(statements, isModuleDeclaration);
    //           let ns = nsIndex !== -1 ? statements[nsIndex] as ModuleDeclaration : undefined;
    //           if (ns && exportAssignment && exportAssignment.isExportEquals &&
    //               isIdentifier(exportAssignment.expression) && isIdentifier(ns.name) && idText(ns.name) === idText(exportAssignment.expression) &&
    //               ns.body && isModuleBlock(ns.body)) {
    //               // Pass 0: Correct situations where a module has both an `export = ns` and multiple top-level exports by stripping the export modifiers from
    //               //  the top-level exports and exporting them in the targeted ns, as can occur when a js file has both typedefs and `module.export` assignments
    //               const excessExports = filter(statements, s => !!(getEffectiveModifierFlags(s) & ModifierFlags.Export));
    //               const name = ns.name;
    //               let body = ns.body;
    //               if (length(excessExports)) {
    //                   ns = factory.updateModuleDeclaration(
    //                       ns,
    //                       ns.decorators,
    //                       ns.modifiers,
    //                       ns.name,
    //                       body = factory.updateModuleBlock(
    //                           body,
    //                           factory.createNodeArray([...ns.body.statements, factory.createExportDeclaration(
    //                               /*decorators*/ undefined,
    //                               /*modifiers*/ undefined,
    //                               /*isTypeOnly*/ false,
    //                               factory.createNamedExports(map(flatMap(excessExports, e => getNamesOfDeclaration(e)), id => factory.createExportSpecifier(/*isTypeOnly*/ false, /*alias*/ undefined, id))),
    //                               /*moduleSpecifier*/ undefined
    //                           )])
    //                       )
    //                   );
    //                   statements = [...statements.slice(0, nsIndex), ns, ...statements.slice(nsIndex + 1)];
    //               }

    //               // Pass 1: Flatten `export namespace _exports {} export = _exports;` so long as the `export=` only points at a single namespace declaration
    //               if (!find(statements, s => s !== ns && nodeHasName(s, name))) {
    //                   results = [];
    //                   // If the namespace contains no export assignments or declarations, and no declarations flagged with `export`, then _everything_ is exported -
    //                   // to respect this as the top level, we need to add an `export` modifier to everything
    //                   const mixinExportFlag = !some(body.statements, s => hasSyntacticModifier(s, ModifierFlags.Export) || isExportAssignment(s) || isExportDeclaration(s));
    //                   forEach(body.statements, s => {
    //                       addResult(s, mixinExportFlag ? ModifierFlags.Export : ModifierFlags.None); // Recalculates the ambient (and export, if applicable from above) flag
    //                   });
    //                   statements = [...filter(statements, s => s !== ns && s !== exportAssignment), ...results];
    //               }
    //           }
    //           return statements;
    //       }

    //       function mergeExportDeclarations(statements: Statement[]) {
    //           // Pass 2: Combine all `export {}` declarations
    //           const exports = filter(statements, d => isExportDeclaration(d) && !d.moduleSpecifier && !!d.exportClause && isNamedExports(d.exportClause)) as ExportDeclaration[];
    //           if (length(exports) > 1) {
    //               const nonExports = filter(statements, d => !isExportDeclaration(d) || !!d.moduleSpecifier || !d.exportClause);
    //               statements = [...nonExports, factory.createExportDeclaration(
    //                   /*decorators*/ undefined,
    //                   /*modifiers*/ undefined,
    //                   /*isTypeOnly*/ false,
    //                   factory.createNamedExports(flatMap(exports, e => cast(e.exportClause, isNamedExports).elements)),
    //                   /*moduleSpecifier*/ undefined
    //               )];
    //           }
    //           // Pass 2b: Also combine all `export {} from "..."` declarations as needed
    //           const reexports = filter(statements, d => isExportDeclaration(d) && !!d.moduleSpecifier && !!d.exportClause && isNamedExports(d.exportClause)) as ExportDeclaration[];
    //           if (length(reexports) > 1) {
    //               const groups = group(reexports, decl => isStringLiteral(decl.moduleSpecifier!) ? ">" + decl.moduleSpecifier.text : ">");
    //               if (groups.length !== reexports.length) {
    //                   for (const group of groups) {
    //                       if (group.length > 1) {
    //                           // remove group members from statements and then merge group members and add back to statements
    //                           statements = [
    //                               ...filter(statements, s => group.indexOf(s as ExportDeclaration) === -1),
    //                               factory.createExportDeclaration(
    //                                   /*decorators*/ undefined,
    //                                   /*modifiers*/ undefined,
    //                                   /*isTypeOnly*/ false,
    //                                   factory.createNamedExports(flatMap(group, e => cast(e.exportClause, isNamedExports).elements)),
    //                                   group[0].moduleSpecifier
    //                               )
    //                           ];
    //                       }
    //                   }
    //               }
    //           }
    //           return statements;
    //       }

    //       function inlineExportModifiers(statements: Statement[]) {
    //           // Pass 3: Move all `export {}`'s to `export` modifiers where possible
    //           const index = findIndex(statements, d => isExportDeclaration(d) && !d.moduleSpecifier && !d.assertClause && !!d.exportClause && isNamedExports(d.exportClause));
    //           if (index >= 0) {
    //               const exportDecl = statements[index] as ExportDeclaration & { readonly exportClause: NamedExports };
    //               const replacements = mapDefined(exportDecl.exportClause.elements, e => {
    //                   if (!e.propertyName) {
    //                       // export {name} - look thru `statements` for `name`, and if all results can take an `export` modifier, do so and filter it
    //                       const indices = indicesOf(statements);
    //                       const associatedIndices = filter(indices, i => nodeHasName(statements[i], e.name));
    //                       if (length(associatedIndices) && every(associatedIndices, i => canHaveExportModifier(statements[i]))) {
    //                           for (const index of associatedIndices) {
    //                               statements[index] = addExportModifier(statements[index] as Extract<HasModifiers, Statement>);
    //                           }
    //                           return undefined;
    //                       }
    //                   }
    //                   return e;
    //               });
    //               if (!length(replacements)) {
    //                   // all clauses removed, remove the export declaration
    //                   orderedRemoveItemAt(statements, index);
    //               }
    //               else {
    //                   // some items filtered, others not - update the export declaration
    //                   statements[index] = factory.updateExportDeclaration(
    //                       exportDecl,
    //                       exportDecl.decorators,
    //                       exportDecl.modifiers,
    //                       exportDecl.isTypeOnly,
    //                       factory.updateNamedExports(
    //                           exportDecl.exportClause,
    //                           replacements
    //                       ),
    //                       exportDecl.moduleSpecifier,
    //                       exportDecl.assertClause
    //                   );
    //               }
    //           }
    //           return statements;
    //       }

    //       function mergeRedundantStatements(statements: Statement[]) {
    //           statements = flattenExportAssignedNamespace(statements);
    //           statements = mergeExportDeclarations(statements);
    //           statements = inlineExportModifiers(statements);

    //           // Not a cleanup, but as a final step: If there is a mix of `export` and non-`export` declarations, but no `export =` or `export {}` add a `export {};` so
    //           // declaration privacy is respected.
    //           if (enclosingDeclaration &&
    //               ((isSourceFile(enclosingDeclaration) && isExternalOrCommonJsModule(enclosingDeclaration)) || isModuleDeclaration(enclosingDeclaration)) &&
    //               (!some(statements, isExternalModuleIndicator) || (!hasScopeMarker(statements) && some(statements, needsScopeMarker)))) {
    //               statements.push(createEmptyExports(factory));
    //           }
    //           return statements;
    //       }

    //       function canHaveExportModifier(node: Statement): node is Extract<HasModifiers, Statement> {
    //           return isEnumDeclaration(node) ||
    //                   isVariableStatement(node) ||
    //                   isFunctionDeclaration(node) ||
    //                   isClassDeclaration(node) ||
    //                   (isModuleDeclaration(node) && !isExternalModuleAugmentation(node) && !isGlobalScopeAugmentation(node)) ||
    //                   isInterfaceDeclaration(node) ||
    //                   isTypeDeclaration(node);
    //       }

    //       function addExportModifier(node: Extract<HasModifiers, Statement>) {
    //           const flags = (getEffectiveModifierFlags(node) | ModifierFlags.Export) & ~ModifierFlags.Ambient;
    //           return factory.updateModifiers(node, flags);
    //       }

    //       function removeExportModifier(node: Extract<HasModifiers, Statement>) {
    //           const flags = getEffectiveModifierFlags(node) & ~ModifierFlags.Export;
    //           return factory.updateModifiers(node, flags);
    //       }

    //       function visitSymbolTable(symbolTable: SymbolTable, suppressNewPrivateContext?: boolean, propertyAsAlias?: boolean) {
    //           if (!suppressNewPrivateContext) {
    //               deferredPrivatesStack.push(new Map());
    //           }
    //           symbolTable.forEach((symbol: Symbol) => {
    //               serializeSymbol(symbol, /*isPrivate*/ false, !!propertyAsAlias);
    //           });
    //           if (!suppressNewPrivateContext) {
    //               // deferredPrivates will be filled up by visiting the symbol table
    //               // And will continue to iterate as elements are added while visited `deferredPrivates`
    //               // (As that's how a map iterator is defined to work)
    //               deferredPrivatesStack[deferredPrivatesStack.length - 1].forEach((symbol: Symbol) => {
    //                   serializeSymbol(symbol, /*isPrivate*/ true, !!propertyAsAlias);
    //               });
    //               deferredPrivatesStack.pop();
    //           }
    //       }

    //       function serializeSymbol(symbol: Symbol, isPrivate: boolean, propertyAsAlias: boolean) {
    //           // cache visited list based on merged symbol, since we want to use the unmerged top-level symbol, but
    //           // still skip reserializing it if we encounter the merged product later on
    //           const visitedSym = getMergedSymbol(symbol);
    //           if (visitedSymbols.has(getSymbolId(visitedSym))) {
    //               return; // Already printed
    //           }
    //           visitedSymbols.add(getSymbolId(visitedSym));
    //           // Only actually serialize symbols within the correct enclosing declaration, otherwise do nothing with the out-of-context symbol
    //           const skipMembershipCheck = !isPrivate; // We only call this on exported symbols when we know they're in the correct scope
    //           if (skipMembershipCheck || (!!length(symbol.declarations) && some(symbol.declarations, d => !!findAncestor(d, n => n === enclosingDeclaration)))) {
    //               const oldContext = context;
    //               context = cloneNodeBuilderContext(context);
    //               const result = serializeSymbolWorker(symbol, isPrivate, propertyAsAlias);
    //               if (context.reportedDiagnostic) {
    //                   oldcontext.reportedDiagnostic = context.reportedDiagnostic; // hoist diagnostic result into outer context
    //               }
    //               context = oldContext;
    //               return result;
    //           }
    //       }

    //       // Synthesize declarations for a symbol - might be an Interface, a Class, a Namespace, a Type, a Variable (const, let, or var), an Alias
    //       // or a merge of some number of those.
    //       // An interesting challenge is ensuring that when classes merge with namespaces and interfaces, is keeping
    //       // each symbol in only one of the representations
    //       // Also, synthesizing a default export of some kind
    //       // If it's an alias: emit `export default ref`
    //       // If it's a property: emit `export default _default` with a `_default` prop
    //       // If it's a class/interface/function: emit a class/interface/function with a `default` modifier
    //       // These forms can merge, eg (`export default 12; export default interface A {}`)
    //       function serializeSymbolWorker(symbol: Symbol, isPrivate: boolean, propertyAsAlias: boolean) {
    //           const symbolName = unescapeLeadingUnderscores(symbol.escapedName);
    //           const isDefault = symbol.escapedName === InternalSymbolName.Default;
    //           if (isPrivate && !(context.flags & NodeBuilderFlags.AllowAnonymousIdentifier) && isStringANonContextualKeyword(symbolName) && !isDefault) {
    //               // Oh no. We cannot use this symbol's name as it's name... It's likely some jsdoc had an invalid name like `export` or `default` :(
    //               context.encounteredError = true;
    //               // TODO: Issue error via symbol tracker?
    //               return; // If we need to emit a private with a keyword name, we're done for, since something else will try to refer to it by that name
    //           }
    //           let needsPostExportDefault = isDefault && !!(
    //                  symbol.flags & SymbolFlags.ExportDoesNotSupportDefaultModifier
    //               || (symbol.flags & SymbolFlags.Function && length(getPropertiesOfType(getTypeOfSymbol(symbol))))
    //           ) && !(symbol.flags & SymbolFlags.Alias); // An alias symbol should preclude needing to make an alias ourselves
    //           let needsExportDeclaration = !needsPostExportDefault && !isPrivate && isStringANonContextualKeyword(symbolName) && !isDefault;
    //           // `serializeVariableOrProperty` will handle adding the export declaration if it is run (since `getInternalSymbolName` will create the name mapping), so we need to ensuer we unset `needsExportDeclaration` if it is
    //           if (needsPostExportDefault || needsExportDeclaration) {
    //               isPrivate = true;
    //           }
    //           const modifierFlags = (!isPrivate ? ModifierFlags.Export : 0) | (isDefault && !needsPostExportDefault ? ModifierFlags.Default : 0);
    //           const isConstMergedWithNS = symbol.flags & SymbolFlags.Module &&
    //               symbol.flags & (SymbolFlags.BlockScopedVariable | SymbolFlags.FunctionScopedVariable | SymbolFlags.Property) &&
    //               symbol.escapedName !== InternalSymbolName.ExportEquals;
    //           const isConstMergedWithNSPrintableAsSignatureMerge = isConstMergedWithNS && isTypeRepresentableAsFunctionNamespaceMerge(getTypeOfSymbol(symbol), symbol);
    //           if (symbol.flags & (SymbolFlags.Function | SymbolFlags.Method) || isConstMergedWithNSPrintableAsSignatureMerge) {
    //               serializeAsFunctionNamespaceMerge(getTypeOfSymbol(symbol), symbol, getInternalSymbolName(symbol, symbolName), modifierFlags);
    //           }
    //           if (symbol.flags & SymbolFlags.TypeAlias) {
    //               serializeTypeAlias(symbol, symbolName, modifierFlags);
    //           }
    //           // Need to skip over export= symbols below - json source files get a single `Property` flagged
    //           // symbol of name `export=` which needs to be handled like an alias. It's not great, but it is what it is.
    //           if (symbol.flags & (SymbolFlags.BlockScopedVariable | SymbolFlags.FunctionScopedVariable | SymbolFlags.Property)
    //               && symbol.escapedName !== InternalSymbolName.ExportEquals
    //               && !(symbol.flags & SymbolFlags.Prototype)
    //               && !(symbol.flags & SymbolFlags.Class)
    //               && !isConstMergedWithNSPrintableAsSignatureMerge) {
    //               if (propertyAsAlias) {
    //                   const createdExport = serializeMaybeAliasAssignment(symbol);
    //                   if (createdExport) {
    //                       needsExportDeclaration = false;
    //                       needsPostExportDefault = false;
    //                   }
    //               }
    //               else {
    //                   const type = getTypeOfSymbol(symbol);
    //                   const localName = getInternalSymbolName(symbol, symbolName);
    //                   if (!(symbol.flags & SymbolFlags.Function) && isTypeRepresentableAsFunctionNamespaceMerge(type, symbol)) {
    //                       // If the type looks like a function declaration + ns could represent it, and it's type is sourced locally, rewrite it into a function declaration + ns
    //                       serializeAsFunctionNamespaceMerge(type, symbol, localName, modifierFlags);
    //                   }
    //                   else {
    //                       // A Class + Property merge is made for a `module.exports.Member = class {}`, and it doesn't serialize well as either a class _or_ a property symbol - in fact, _it behaves like an alias!_
    //                       // `var` is `FunctionScopedVariable`, `const` and `let` are `BlockScopedVariable`, and `module.exports.thing =` is `Property`
    //                       const flags = !(symbol.flags & SymbolFlags.BlockScopedVariable) ? undefined
    //                           : isConstVariable(symbol) ? NodeFlags.Const
    //                           : NodeFlags.Let;
    //                       const name = (needsPostExportDefault || !(symbol.flags & SymbolFlags.Property)) ? localName : getUnusedName(localName, symbol);
    //                       let textRange: Node | undefined = symbol.declarations && find(symbol.declarations, d => isVariableDeclaration(d));
    //                       if (textRange && isVariableDeclarationList(textRange.parent) && textRange.parent.declarations.length === 1) {
    //                           textRange = textRange.parent.parent;
    //                       }
    //                       const propertyAccessRequire = symbol.declarations?.find(isPropertyAccessExpression);
    //                       if (propertyAccessRequire && isBinaryExpression(propertyAccessRequire.parent) && isIdentifier(propertyAccessRequire.parent.right)
    //                           && type.symbol?.valueDeclaration && isSourceFile(type.symbol.valueDeclaration)) {
    //                           const alias = localName === propertyAccessRequire.parent.right.escapedText ? undefined : propertyAccessRequire.parent.right;
    //                           addResult(
    //                               factory.createExportDeclaration(
    //                                   /*decorators*/ undefined,
    //                                   /*modifiers*/ undefined,
    //                                   /*isTypeOnly*/ false,
    //                                   factory.createNamedExports([factory.createExportSpecifier(/*isTypeOnly*/ false, alias, localName)])
    //                               ),
    //                               ModifierFlags.None
    //                           );
    //                           context.tracker.trackSymbol!(type.symbol, context.enclosingDeclaration, SymbolFlags.Value);
    //                       }
    //                       else {
    //                           const statement = setTextRange(factory.createVariableStatement(/*modifiers*/ undefined, factory.createVariableDeclarationList([
    //                               factory.createVariableDeclaration(name, /*exclamationToken*/ undefined, serializeTypeForDeclaration(context, type, symbol, enclosingDeclaration, includePrivateSymbol, bundled))
    //                           ], flags)), textRange);
    //                           addResult(statement, name !== localName ? modifierFlags & ~ModifierFlags.Export : modifierFlags);
    //                           if (name !== localName && !isPrivate) {
    //                               // We rename the variable declaration we generate for Property symbols since they may have a name which
    //                               // conflicts with a local declaration. For example, given input:
    //                               // ```
    //                               // function g() {}
    //                               // module.exports.g = g
    //                               // ```
    //                               // In such a situation, we have a local variable named `g`, and a separate exported variable named `g`.
    //                               // Naively, we would emit
    //                               // ```
    //                               // function g() {}
    //                               // export const g: typeof g;
    //                               // ```
    //                               // That's obviously incorrect - the `g` in the type annotation needs to refer to the local `g`, but
    //                               // the export declaration shadows it.
    //                               // To work around that, we instead write
    //                               // ```
    //                               // function g() {}
    //                               // const g_1: typeof g;
    //                               // export { g_1 as g };
    //                               // ```
    //                               // To create an export named `g` that does _not_ shadow the local `g`
    //                               addResult(
    //                                   factory.createExportDeclaration(
    //                                       /*decorators*/ undefined,
    //                                       /*modifiers*/ undefined,
    //                                       /*isTypeOnly*/ false,
    //                                       factory.createNamedExports([factory.createExportSpecifier(/*isTypeOnly*/ false, name, localName)])
    //                                   ),
    //                                   ModifierFlags.None
    //                               );
    //                               needsExportDeclaration = false;
    //                               needsPostExportDefault = false;
    //                           }
    //                       }
    //                   }
    //               }
    //           }
    //           if (symbol.flags & SymbolFlags.Enum) {
    //               serializeEnum(symbol, symbolName, modifierFlags);
    //           }
    //           if (symbol.flags & SymbolFlags.Class) {
    //               if (symbol.flags & SymbolFlags.Property
    //                   && symbol.valueDeclaration
    //                   && isBinaryExpression(symbol.valueDeclaration.parent)
    //                   && isClassExpression(symbol.valueDeclaration.parent.right)) {
    //                   // Looks like a `module.exports.Sub = class {}` - if we serialize `symbol` as a class, the result will have no members,
    //                   // since the classiness is actually from the target of the effective alias the symbol is. yes. A BlockScopedVariable|Class|Property
    //                   // _really_ acts like an Alias, and none of a BlockScopedVariable, Class, or Property. This is the travesty of JS binding today.
    //                   serializeAsAlias(symbol, getInternalSymbolName(symbol, symbolName), modifierFlags);
    //               }
    //               else {
    //                   serializeAsClass(symbol, getInternalSymbolName(symbol, symbolName), modifierFlags);
    //               }
    //           }
    //           if ((symbol.flags & (SymbolFlags.ValueModule | SymbolFlags.NamespaceModule) && (!isConstMergedWithNS || isTypeOnlyNamespace(symbol))) || isConstMergedWithNSPrintableAsSignatureMerge) {
    //               serializeModule(symbol, symbolName, modifierFlags);
    //           }
    //           // The class meaning serialization should handle serializing all interface members
    //           if (symbol.flags & SymbolFlags.Interface && !(symbol.flags & SymbolFlags.Class)) {
    //               serializeInterface(symbol, symbolName, modifierFlags);
    //           }
    //           if (symbol.flags & SymbolFlags.Alias) {
    //               serializeAsAlias(symbol, getInternalSymbolName(symbol, symbolName), modifierFlags);
    //           }
    //           if (symbol.flags & SymbolFlags.Property && symbol.escapedName === InternalSymbolName.ExportEquals) {
    //               serializeMaybeAliasAssignment(symbol);
    //           }
    //           if (symbol.flags & SymbolFlags.ExportStar) {
    //               // synthesize export * from "moduleReference"
    //               // Straightforward - only one thing to do - make an export declaration
    //               if (symbol.declarations) {
    //                   for (const node of symbol.declarations) {
    //                       const resolvedModule = resolveExternalModuleName(node, (node as ExportDeclaration).moduleSpecifier!);
    //                       if (!resolvedModule) continue;
    //                       addResult(factory.createExportDeclaration(/*decorators*/ undefined, /*modifiers*/ undefined, /*isTypeOnly*/ false, /*exportClause*/ undefined, factory.createStringLiteral(getSpecifierForModuleSymbol(resolvedModule, context))), ModifierFlags.None);
    //                   }
    //               }
    //           }
    //           if (needsPostExportDefault) {
    //               addResult(factory.createExportAssignment(/*decorators*/ undefined, /*modifiers*/ undefined, /*isExportAssignment*/ false, factory.createIdentifier(getInternalSymbolName(symbol, symbolName))), ModifierFlags.None);
    //           }
    //           else if (needsExportDeclaration) {
    //               addResult(factory.createExportDeclaration(
    //                   /*decorators*/ undefined,
    //                   /*modifiers*/ undefined,
    //                   /*isTypeOnly*/ false,
    //                   factory.createNamedExports([factory.createExportSpecifier(/*isTypeOnly*/ false, getInternalSymbolName(symbol, symbolName), symbolName)])
    //               ), ModifierFlags.None);
    //           }
    //       }

    //       function includePrivateSymbol(symbol: Symbol) {
    //           if (some(symbol.declarations, isParameterDeclaration)) return;
    //           Debug.assertIsDefined(deferredPrivatesStack[deferredPrivatesStack.length - 1]);
    //           getUnusedName(unescapeLeadingUnderscores(symbol.escapedName), symbol); // Call to cache unique name for symbol
    //           // Blanket moving (import) aliases into the root private context should work, since imports are not valid within namespaces
    //           // (so they must have been in the root to begin with if they were real imports) cjs `require` aliases (an upcoming feature)
    //           // will throw a wrench in this, since those may have been nested, but we'll need to synthesize them in the outer scope
    //           // anyway, as that's the only place the import they translate to is valid. In such a case, we might need to use a unique name
    //           // for the moved import; which hopefully the above `getUnusedName` call should produce.
    //           const isExternalImportAlias = !!(symbol.flags & SymbolFlags.Alias) && !some(symbol.declarations, d =>
    //               !!findAncestor(d, isExportDeclaration) ||
    //               isNamespaceExport(d) ||
    //               (isImportEqualsDeclaration(d) && !isExternalModuleReference(d.moduleReference))
    //           );
    //           deferredPrivatesStack[isExternalImportAlias ? 0 : (deferredPrivatesStack.length - 1)].set(getSymbolId(symbol), symbol);
    //       }

    //       function isExportingScope(enclosingDeclaration: Node) {
    //           return ((isSourceFile(enclosingDeclaration) && (isExternalOrCommonJsModule(enclosingDeclaration) || isJsonSourceFile(enclosingDeclaration))) ||
    //               (isAmbientModule(enclosingDeclaration) && !isGlobalScopeAugmentation(enclosingDeclaration)));
    //       }

    //       // Prepends a `declare` and/or `export` modifier if the context requires it, and then adds `node` to `result` and returns `node`
    //       function addResult(node: Statement, additionalModifierFlags: ModifierFlags) {
    //           if (canHaveModifiers(node)) {
    //               let newModifierFlags: ModifierFlags = ModifierFlags.None;
    //               const enclosingDeclaration = context.enclosingDeclaration &&
    //                   (isJSDocTypeAlias(context.enclosingDeclaration) ? getSourceFileOfNode(context.enclosingDeclaration) : context.enclosingDeclaration);
    //               if (additionalModifierFlags & ModifierFlags.Export &&
    //                   enclosingDeclaration && (isExportingScope(enclosingDeclaration) || isModuleDeclaration(enclosingDeclaration)) &&
    //                   canHaveExportModifier(node)
    //               ) {
    //                   // Classes, namespaces, variables, functions, interfaces, and types should all be `export`ed in a module context if not private
    //                   newModifierFlags |= ModifierFlags.Export;
    //               }
    //               if (addingDeclare && !(newModifierFlags & ModifierFlags.Export) &&
    //                   (!enclosingDeclaration || !(enclosingDeclaration.flags & NodeFlags.Ambient)) &&
    //                   (isEnumDeclaration(node) || isVariableStatement(node) || isFunctionDeclaration(node) || isClassDeclaration(node) || isModuleDeclaration(node))) {
    //                   // Classes, namespaces, variables, enums, and functions all need `declare` modifiers to be valid in a declaration file top-level scope
    //                   newModifierFlags |= ModifierFlags.Ambient;
    //               }
    //               if ((additionalModifierFlags & ModifierFlags.Default) && (isClassDeclaration(node) || isInterfaceDeclaration(node) || isFunctionDeclaration(node))) {
    //                   newModifierFlags |= ModifierFlags.Default;
    //               }
    //               if (newModifierFlags) {
    //                   node = factory.updateModifiers(node, newModifierFlags | getEffectiveModifierFlags(node));
    //               }
    //           }
    //           results.push(node);
    //       }

    //       function serializeTypeAlias(symbol: Symbol, symbolName: string, modifierFlags: ModifierFlags) {
    //           const aliasType = getDeclaredTypeOfTypeAlias(symbol);
    //           const typeParams = getSymbolLinks(symbol).typeParameters;
    //           const typeParamDecls = map(typeParams, p => typeParameterToDeclaration(p, context));
    //           const jsdocAliasDecl = symbol.declarations?.find(isJSDocTypeAlias);
    //           const commentText = getTextOfJSDocComment(jsdocAliasDecl ? jsdocAliasDecl.comment || jsdocAliasDecl.parent.comment : undefined);
    //           const oldFlags = context.flags;
    //           context.flags |= NodeBuilderFlags.InTypeAlias;
    //           const oldEnclosingDecl = context.enclosingDeclaration;
    //           context.enclosingDeclaration = jsdocAliasDecl;
    //           const typeNode = jsdocAliasDecl && jsdocAliasDecl.typeExpression
    //               && isJSDocTypeExpression(jsdocAliasDecl.typeExpression)
    //               && serializeExistingTypeNode(context, jsdocAliasDecl.typeExpression.type, includePrivateSymbol, bundled)
    //               || typeToTypeNodeHelper(aliasType, context);
    //           addResult(setSyntheticLeadingComments(
    //               factory.createTypeAliasDeclaration(/*decorators*/ undefined, /*modifiers*/ undefined, getInternalSymbolName(symbol, symbolName), typeParamDecls, typeNode),
    //               !commentText ? [] : [{ kind: SyntaxKind.MultiLineCommentTrivia, text: "*\n * " + commentText.replace(/\n/g, "\n * ") + "\n ", pos: -1, end: -1, hasTrailingNewLine: true }]
    //           ), modifierFlags);
    //           context.flags = oldFlags;
    //           context.enclosingDeclaration = oldEnclosingDecl;
    //       }

    //       function serializeInterface(symbol: Symbol, symbolName: string, modifierFlags: ModifierFlags) {
    //           const interfaceType = getDeclaredTypeOfClassOrInterface(symbol);
    //           const localParams = getLocalTypeParametersOfClassOrInterfaceOrTypeAlias(symbol);
    //           const typeParamDecls = map(localParams, p => typeParameterToDeclaration(p, context));
    //           const baseTypes = getBaseTypes(interfaceType);
    //           const baseType = length(baseTypes) ? getIntersectionType(baseTypes) : undefined;
    //           const members = flatMap<Symbol, TypeElement>(getPropertiesOfType(interfaceType), p => serializePropertySymbolForInterface(p, baseType));
    //           const callSignatures = serializeSignatures(SignatureKind.Call, interfaceType, baseType, SyntaxKind.CallSignature) as CallSignatureDeclaration[];
    //           const constructSignatures = serializeSignatures(SignatureKind.Construct, interfaceType, baseType, SyntaxKind.ConstructSignature) as ConstructSignatureDeclaration[];
    //           const indexSignatures = serializeIndexSignatures(interfaceType, baseType);

    //           const heritageClauses = !length(baseTypes) ? undefined : [factory.createHeritageClause(SyntaxKind.ExtendsKeyword, mapDefined(baseTypes, b => trySerializeAsTypeReference(b, SymbolFlags.Value)))];
    //           addResult(factory.createInterfaceDeclaration(
    //               /*decorators*/ undefined,
    //               /*modifiers*/ undefined,
    //               getInternalSymbolName(symbol, symbolName),
    //               typeParamDecls,
    //               heritageClauses,
    //               [...indexSignatures, ...constructSignatures, ...callSignatures, ...members]
    //           ), modifierFlags);
    //       }

    //       function getNamespaceMembersForSerialization(symbol: Symbol) {
    //           return !symbol.exports ? [] : filter(arrayFrom(symbol.exports.values()), isNamespaceMember);
    //       }

    //       function isTypeOnlyNamespace(symbol: Symbol) {
    //           return every(getNamespaceMembersForSerialization(symbol), m => !(resolveSymbol(m).flags & SymbolFlags.Value));
    //       }

    //       function serializeModule(symbol: Symbol, symbolName: string, modifierFlags: ModifierFlags) {
    //           const members = getNamespaceMembersForSerialization(symbol);
    //           // Split NS members up by declaration - members whose parent symbol is the ns symbol vs those whose is not (but were added in later via merging)
    //           const locationMap = arrayToMultiMap(members, m => m.parent && m.parent === symbol ? "real" : "merged");
    //           const realMembers = locationMap.get("real") || emptyArray;
    //           const mergedMembers = locationMap.get("merged") || emptyArray;
    //           // TODO: `suppressNewPrivateContext` is questionable -we need to simply be emitting privates in whatever scope they were declared in, rather
    //           // than whatever scope we traverse to them in. That's a bit of a complex rewrite, since we're not _actually_ tracking privates at all in advance,
    //           // so we don't even have placeholders to fill in.
    //           if (length(realMembers)) {
    //               const localName = getInternalSymbolName(symbol, symbolName);
    //               serializeAsNamespaceDeclaration(realMembers, localName, modifierFlags, !!(symbol.flags & (SymbolFlags.Function | SymbolFlags.Assignment)));
    //           }
    //           if (length(mergedMembers)) {
    //               const containingFile = getSourceFileOfNode(context.enclosingDeclaration);
    //               const localName = getInternalSymbolName(symbol, symbolName);
    //               const nsBody = factory.createModuleBlock([factory.createExportDeclaration(
    //                   /*decorators*/ undefined,
    //                   /*modifiers*/ undefined,
    //                   /*isTypeOnly*/ false,
    //                   factory.createNamedExports(mapDefined(filter(mergedMembers, n => n.escapedName !== InternalSymbolName.ExportEquals), s => {
    //                       const name = unescapeLeadingUnderscores(s.escapedName);
    //                       const localName = getInternalSymbolName(s, name);
    //                       const aliasDecl = s.declarations && getDeclarationOfAliasSymbol(s);
    //                       if (containingFile && (aliasDecl ? containingFile !== getSourceFileOfNode(aliasDecl) : !some(s.declarations, d => getSourceFileOfNode(d) === containingFile))) {
    //                           context.tracker?.reportNonlocalAugmentation?.(containingFile, symbol, s);
    //                           return undefined;
    //                       }
    //                       const target = aliasDecl && getTargetOfAliasDeclaration(aliasDecl, /*dontRecursivelyResolve*/ true);
    //                       includePrivateSymbol(target || s);
    //                       const targetName = target ? getInternalSymbolName(target, unescapeLeadingUnderscores(target.escapedName)) : localName;
    //                       return factory.createExportSpecifier(/*isTypeOnly*/ false, name === targetName ? undefined : targetName, name);
    //                   }))
    //               )]);
    //               addResult(factory.createModuleDeclaration(
    //                   /*decorators*/ undefined,
    //                   /*modifiers*/ undefined,
    //                   factory.createIdentifier(localName),
    //                   nsBody,
    //                   NodeFlags.Namespace
    //               ), ModifierFlags.None);
    //           }
    //       }

    //       function serializeEnum(symbol: Symbol, symbolName: string, modifierFlags: ModifierFlags) {
    //           addResult(factory.createEnumDeclaration(
    //               /*decorators*/ undefined,
    //               factory.createModifiersFromModifierFlags(isConstEnumSymbol(symbol) ? ModifierFlags.Const : 0),
    //               getInternalSymbolName(symbol, symbolName),
    //               map(filter(getPropertiesOfType(getTypeOfSymbol(symbol)), p => !!(p.flags & SymbolFlags.EnumMember)), p => {
    //                   // TODO: Handle computed names
    //                   // I hate that to get the initialized value we need to walk back to the declarations here; but there's no
    //                   // other way to get the possible const value of an enum member that I'm aware of, as the value is cached
    //                   // _on the declaration_, not on the declaration's symbol...
    //                   const initializedValue = p.declarations && p.declarations[0] && isEnumMember(p.declarations[0]) ? getConstantValue(p.declarations[0]) : undefined;
    //                   return factory.createEnumMember(unescapeLeadingUnderscores(p.escapedName), initializedValue === undefined ? undefined :
    //                       typeof initializedValue === "string" ? factory.createStringLiteral(initializedValue) :
    //                       factory.createNumericLiteral(initializedValue));
    //               })
    //           ), modifierFlags);
    //       }

    //       function serializeAsFunctionNamespaceMerge(type: Type, symbol: Symbol, localName: string, modifierFlags: ModifierFlags) {
    //           const signatures = getSignaturesOfType(type, SignatureKind.Call);
    //           for (const sig of signatures) {
    //               // Each overload becomes a separate function declaration, in order
    //               const decl = signatureToSignatureDeclarationHelper(sig, SyntaxKind.FunctionDeclaration, context, { name: factory.createIdentifier(localName), privateSymbolVisitor: includePrivateSymbol, bundledImports: bundled }) as FunctionDeclaration;
    //               addResult(setTextRange(decl, getSignatureTextRangeLocation(sig)), modifierFlags);
    //           }
    //           // Module symbol emit will take care of module-y members, provided it has exports
    //           if (!(symbol.flags & (SymbolFlags.ValueModule | SymbolFlags.NamespaceModule) && !!symbol.exports && !!symbol.exports.size)) {
    //               const props = filter(getPropertiesOfType(type), isNamespaceMember);
    //               serializeAsNamespaceDeclaration(props, localName, modifierFlags, /*suppressNewPrivateContext*/ true);
    //           }
    //       }

    //       function getSignatureTextRangeLocation(signature: Signature) {
    //           if (signature.declaration && signature.declaration.parent) {
    //               if (isBinaryExpression(signature.declaration.parent) && getAssignmentDeclarationKind(signature.declaration.parent) === AssignmentDeclarationKind.Property) {
    //                   return signature.declaration.parent;
    //               }
    //               // for expressions assigned to `var`s, use the `var` as the text range
    //               if (isVariableDeclaration(signature.declaration.parent) && signature.declaration.parent.parent) {
    //                   return signature.declaration.parent.parent;
    //               }
    //           }
    //           return signature.declaration;
    //       }

    //       function serializeAsNamespaceDeclaration(props: readonly Symbol[], localName: string, modifierFlags: ModifierFlags, suppressNewPrivateContext: boolean) {
    //           if (length(props)) {
    //               const localVsRemoteMap = arrayToMultiMap(props, p =>
    //                   !length(p.declarations) || some(p.declarations, d =>
    //                       getSourceFileOfNode(d) === getSourceFileOfNode(context.enclosingDeclaration!)
    //                   ) ? "local" : "remote"
    //               );
    //               const localProps = localVsRemoteMap.get("local") || emptyArray;
    //               // handle remote props first - we need to make an `import` declaration that points at the module containing each remote
    //               // prop in the outermost scope (TODO: a namespace within a namespace would need to be appropriately handled by this)
    //               // Example:
    //               // import Foo_1 = require("./exporter");
    //               // export namespace ns {
    //               //     import Foo = Foo_1.Foo;
    //               //     export { Foo };
    //               //     export const c: number;
    //               // }
    //               // This is needed because in JS, statements like `const x = require("./f")` support both type and value lookup, even if they're
    //               // normally just value lookup (so it functions kinda like an alias even when it's not an alias)
    //               // _Usually_, we'll simply print the top-level as an alias instead of a `var` in such situations, however is is theoretically
    //               // possible to encounter a situation where a type has members from both the current file and other files - in those situations,
    //               // emit akin to the above would be needed.

    //               // Add a namespace
    //               // Create namespace as non-synthetic so it is usable as an enclosing declaration
    //               let fakespace = parseNodeFactory.createModuleDeclaration(/*decorators*/ undefined, /*modifiers*/ undefined, factory.createIdentifier(localName), factory.createModuleBlock([]), NodeFlags.Namespace);
    //               setParent(fakespace, enclosingDeclaration as SourceFile | NamespaceDeclaration);
    //               fakespace.locals = createSymbolTable(props);
    //               fakespace.symbol = props[0].parent!;

    //               const oldResults = results;
    //               results = [];
    //               const oldAddingDeclare = addingDeclare;
    //               addingDeclare = false;
    //               const subcontext = { ...context, enclosingDeclaration: fakespace };
    //               const oldContext = context;
    //               context = subcontext;
    //               // TODO: implement handling for the localVsRemoteMap.get("remote") - should be difficult to trigger (see comment above), as only interesting cross-file js merges should make this possible
    //               visitSymbolTable(createSymbolTable(localProps), suppressNewPrivateContext, /*propertyAsAlias*/ true);
    //               context = oldContext;
    //               addingDeclare = oldAddingDeclare;
    //               const declarations = results;
    //               results = oldResults;
    //               // replace namespace with synthetic version
    //               const defaultReplaced = map(declarations, d => isExportAssignment(d) && !d.isExportEquals && isIdentifier(d.expression) ? factory.createExportDeclaration(
    //                   /*decorators*/ undefined,
    //                   /*modifiers*/ undefined,
    //                   /*isTypeOnly*/ false,
    //                   factory.createNamedExports([factory.createExportSpecifier(/*isTypeOnly*/ false, d.expression, factory.createIdentifier(InternalSymbolName.Default))])
    //               ) : d);
    //               const exportModifierStripped = every(defaultReplaced, d => hasSyntacticModifier(d, ModifierFlags.Export)) ? map(defaultReplaced, removeExportModifier) : defaultReplaced;
    //               fakespace = factory.updateModuleDeclaration(
    //                   fakespace,
    //                   fakespace.decorators,
    //                   fakespace.modifiers,
    //                   fakespace.name,
    //                   factory.createModuleBlock(exportModifierStripped));
    //               addResult(fakespace, modifierFlags); // namespaces can never be default exported
    //           }
    //       }

    //       function isNamespaceMember(p: Symbol) {
    //           return !!(p.flags & (SymbolFlags.Type | SymbolFlags.Namespace | SymbolFlags.Alias)) ||
    //               !(p.flags & SymbolFlags.Prototype || p.escapedName === "prototype" || p.valueDeclaration && isStatic(p.valueDeclaration) && isClassLike(p.valueDeclaration.parent));
    //       }

    //       function sanitizeJSDocImplements(clauses: readonly ExpressionWithTypeArguments[]): ExpressionWithTypeArguments[] | undefined {
    //           const result = mapDefined(clauses, e => {
    //               const oldEnclosing = context.enclosingDeclaration;
    //               context.enclosingDeclaration = e;
    //               let expr = e.expression;
    //               if (isEntityNameExpression(expr)) {
    //                   if (isIdentifier(expr) && idText(expr) === "") {
    //                       return cleanup(/*result*/ undefined); // Empty heritage clause, should be an error, but prefer emitting no heritage clauses to reemitting the empty one
    //                   }
    //                   let introducesError: boolean;
    //                   ({ introducesError, node: expr } = trackExistingEntityName(expr, context, includePrivateSymbol));
    //                   if (introducesError) {
    //                       return cleanup(/*result*/ undefined);
    //                   }
    //               }
    //               return cleanup(factory.createExpressionWithTypeArguments(expr,
    //                   map(e.typeArguments, a =>
    //                       serializeExistingTypeNode(context, a, includePrivateSymbol, bundled)
    //                       || typeToTypeNodeHelper(getTypeFromTypeNode(a), context)
    //                   )
    //               ));

    //               function cleanup<T>(result: T): T {
    //                   context.enclosingDeclaration = oldEnclosing;
    //                   return result;
    //               }
    //           });
    //           if (result.length === clauses.length) {
    //               return result;
    //           }
    //           return undefined;
    //       }

    //       function serializeAsClass(symbol: Symbol, localName: string, modifierFlags: ModifierFlags) {
    //           const originalDecl = symbol.declarations?.find(isClassLike);
    //           const oldEnclosing = context.enclosingDeclaration;
    //           context.enclosingDeclaration = originalDecl || oldEnclosing;
    //           const localParams = getLocalTypeParametersOfClassOrInterfaceOrTypeAlias(symbol);
    //           const typeParamDecls = map(localParams, p => typeParameterToDeclaration(p, context));
    //           const classType = getDeclaredTypeOfClassOrInterface(symbol);
    //           const baseTypes = getBaseTypes(classType);
    //           const originalImplements = originalDecl && getEffectiveImplementsTypeNodes(originalDecl);
    //           const implementsExpressions = originalImplements && sanitizeJSDocImplements(originalImplements)
    //               || mapDefined(getImplementsTypes(classType), serializeImplementedType);
    //           const staticType = getTypeOfSymbol(symbol);
    //           const isClass = !!staticType.symbol?.valueDeclaration && isClassLike(staticType.symbol.valueDeclaration);
    //           const staticBaseType = isClass
    //               ? getBaseConstructorTypeOfClass(staticType as InterfaceType)
    //               : anyType;
    //           const heritageClauses = [
    //               ...!length(baseTypes) ? [] : [factory.createHeritageClause(SyntaxKind.ExtendsKeyword, map(baseTypes, b => serializeBaseType(b, staticBaseType, localName)))],
    //               ...!length(implementsExpressions) ? [] : [factory.createHeritageClause(SyntaxKind.ImplementsKeyword, implementsExpressions)]
    //           ];
    //           const symbolProps = getNonInterhitedProperties(classType, baseTypes, getPropertiesOfType(classType));
    //           const publicSymbolProps = filter(symbolProps, s => {
    //               // `valueDeclaration` could be undefined if inherited from
    //               // a union/intersection base type, but inherited properties
    //               // don't matter here.
    //               const valueDecl = s.valueDeclaration;
    //               return !!valueDecl && !(isNamedDeclaration(valueDecl) && isPrivateIdentifier(valueDecl.name));
    //           });
    //           const hasPrivateIdentifier = some(symbolProps, s => {
    //               // `valueDeclaration` could be undefined if inherited from
    //               // a union/intersection base type, but inherited properties
    //               // don't matter here.
    //               const valueDecl = s.valueDeclaration;
    //               return !!valueDecl && isNamedDeclaration(valueDecl) && isPrivateIdentifier(valueDecl.name);
    //           });
    //           // Boil down all private properties into a single one.
    //           const privateProperties = hasPrivateIdentifier ?
    //               [factory.createPropertyDeclaration(
    //                   /*decorators*/ undefined,
    //                   /*modifiers*/ undefined,
    //                   factory.createPrivateIdentifier("#private"),
    //                   /*questionOrExclamationToken*/ undefined,
    //                   /*type*/ undefined,
    //                   /*initializer*/ undefined,
    //               )] :
    //               emptyArray;
    //           const publicProperties = flatMap<Symbol, ClassElement>(publicSymbolProps, p => serializePropertySymbolForClass(p, /*isStatic*/ false, baseTypes[0]));
    //           // Consider static members empty if symbol also has function or module meaning - function namespacey emit will handle statics
    //           const staticMembers = flatMap(
    //               filter(getPropertiesOfType(staticType), p => !(p.flags & SymbolFlags.Prototype) && p.escapedName !== "prototype" && !isNamespaceMember(p)),
    //               p => serializePropertySymbolForClass(p, /*isStatic*/ true, staticBaseType));
    //           // When we encounter an `X.prototype.y` assignment in a JS file, we bind `X` as a class regardless as to whether
    //           // the value is ever initialized with a class or function-like value. For cases where `X` could never be
    //           // created via `new`, we will inject a `private constructor()` declaration to indicate it is not createable.
    //           const isNonConstructableClassLikeInJsFile =
    //               !isClass &&
    //               !!symbol.valueDeclaration &&
    //               isInJSFile(symbol.valueDeclaration) &&
    //               !some(getSignaturesOfType(staticType, SignatureKind.Construct));
    //           const constructors = isNonConstructableClassLikeInJsFile ?
    //               [factory.createConstructorDeclaration(/*decorators*/ undefined, factory.createModifiersFromModifierFlags(ModifierFlags.Private), [], /*body*/ undefined)] :
    //               serializeSignatures(SignatureKind.Construct, staticType, staticBaseType, SyntaxKind.Constructor) as ConstructorDeclaration[];
    //           const indexSignatures = serializeIndexSignatures(classType, baseTypes[0]);
    //           context.enclosingDeclaration = oldEnclosing;
    //           addResult(setTextRange(factory.createClassDeclaration(
    //               /*decorators*/ undefined,
    //               /*modifiers*/ undefined,
    //               localName,
    //               typeParamDecls,
    //               heritageClauses,
    //               [...indexSignatures, ...staticMembers, ...constructors, ...publicProperties, ...privateProperties]
    //           ), symbol.declarations && filter(symbol.declarations, d => isClassDeclaration(d) || isClassExpression(d))[0]), modifierFlags);
    //       }

    //       function getSomeTargetNameFromDeclarations(declarations: Declaration[] | undefined) {
    //           return firstDefined(declarations, d => {
    //               if (isImportSpecifier(d) || isExportSpecifier(d)) {
    //                   return idText(d.propertyName || d.name);
    //               }
    //               if (isBinaryExpression(d) || isExportAssignment(d)) {
    //                   const expression = isExportAssignment(d) ? d.expression : d.right;
    //                   if (isPropertyAccessExpression(expression)) {
    //                       return idText(expression.name);
    //                   }
    //               }
    //               if (isAliasSymbolDeclaration(d)) {
    //                   // This is... heuristic, at best. But it's probably better than always printing the name of the shorthand ambient module.
    //                   const name = getNameOfDeclaration(d);
    //                   if (name && isIdentifier(name)) {
    //                       return idText(name);
    //                   }
    //               }
    //               return undefined;
    //           });
    //       }

    //       function serializeAsAlias(symbol: Symbol, localName: string, modifierFlags: ModifierFlags) {
    //           // synthesize an alias, eg `export { symbolName as Name }`
    //           // need to mark the alias `symbol` points at
    //           // as something we need to serialize as a private declaration as well
    //           const node = getDeclarationOfAliasSymbol(symbol);
    //           if (!node) return Debug.fail();
    //           const target = getMergedSymbol(getTargetOfAliasDeclaration(node, /*dontRecursivelyResolve*/ true));
    //           if (!target) {
    //               return;
    //           }
    //           // If `target` refers to a shorthand module symbol, the name we're trying to pull out isn;t recoverable from the target symbol
    //           // In such a scenario, we must fall back to looking for an alias declaration on `symbol` and pulling the target name from that
    //           let verbatimTargetName = isShorthandAmbientModuleSymbol(target) && getSomeTargetNameFromDeclarations(symbol.declarations) || unescapeLeadingUnderscores(target.escapedName);
    //           if (verbatimTargetName === InternalSymbolName.ExportEquals && (getESModuleInterop(compilerOptions) || compilerOptions.allowSyntheticDefaultImports)) {
    //               // target refers to an `export=` symbol that was hoisted into a synthetic default - rename here to match
    //               verbatimTargetName = InternalSymbolName.Default;
    //           }
    //           const targetName = getInternalSymbolName(target, verbatimTargetName);
    //           includePrivateSymbol(target); // the target may be within the same scope - attempt to serialize it first
    //           switch (node.kind) {
    //               case SyntaxKind.BindingElement:
    //                   if (node.parent?.parent?.kind === SyntaxKind.VariableDeclaration) {
    //                       // const { SomeClass } = require('./lib');
    //                       const specifier = getSpecifierForModuleSymbol(target.parent || target, context); // './lib'
    //                       const { propertyName } = node as BindingElement;
    //                       addResult(factory.createImportDeclaration(
    //                           /*decorators*/ undefined,
    //                           /*modifiers*/ undefined,
    //                           factory.createImportClause(/*isTypeOnly*/ false, /*name*/ undefined, factory.createNamedImports([factory.createImportSpecifier(
    //                               /*isTypeOnly*/ false,
    //                               propertyName && isIdentifier(propertyName) ? factory.createIdentifier(idText(propertyName)) : undefined,
    //                               factory.createIdentifier(localName)
    //                           )])),
    //                           factory.createStringLiteral(specifier),
    //                           /*importClause*/ undefined
    //                       ), ModifierFlags.None);
    //                       break;
    //                   }
    //                   // We don't know how to serialize this (nested?) binding element
    //                   Debug.failBadSyntaxKind(node.parent?.parent || node, "Unhandled binding element grandparent kind in declaration serialization");
    //                   break;
    //               case SyntaxKind.ShorthandPropertyAssignment:
    //                   if (node.parent?.parent?.kind === SyntaxKind.BinaryExpression) {
    //                       // module.exports = { SomeClass }
    //                       serializeExportSpecifier(
    //                           unescapeLeadingUnderscores(symbol.escapedName),
    //                           targetName
    //                       );
    //                   }
    //                   break;
    //               case SyntaxKind.VariableDeclaration:
    //                   // commonjs require: const x = require('y')
    //                   if (isPropertyAccessExpression((node as VariableDeclaration).initializer!)) {
    //                       // const x = require('y').z
    //                       const initializer = (node as VariableDeclaration).initializer! as PropertyAccessExpression; // require('y').z
    //                       const uniqueName = factory.createUniqueName(localName); // _x
    //                       const specifier = getSpecifierForModuleSymbol(target.parent || target, context); // 'y'
    //                       // import _x = require('y');
    //                       addResult(factory.createImportEqualsDeclaration(
    //                           /*decorators*/ undefined,
    //                           /*modifiers*/ undefined,
    //                           /*isTypeOnly*/ false,
    //                           uniqueName,
    //                           factory.createExternalModuleReference(factory.createStringLiteral(specifier))
    //                       ), ModifierFlags.None);
    //                       // import x = _x.z
    //                       addResult(factory.createImportEqualsDeclaration(
    //                           /*decorators*/ undefined,
    //                           /*modifiers*/ undefined,
    //                           /*isTypeOnly*/ false,
    //                           factory.createIdentifier(localName),
    //                           factory.createQualifiedName(uniqueName, initializer.name as Identifier),
    //                       ), modifierFlags);
    //                       break;
    //                   }
    //                   // else fall through and treat commonjs require just like import=
    //               case SyntaxKind.ImportEqualsDeclaration:
    //                   // This _specifically_ only exists to handle json declarations - where we make aliases, but since
    //                   // we emit no declarations for the json document, must not refer to it in the declarations
    //                   if (target.escapedName === InternalSymbolName.ExportEquals && some(target.declarations, isJsonSourceFile)) {
    //                       serializeMaybeAliasAssignment(symbol);
    //                       break;
    //                   }
    //                   // Could be a local `import localName = ns.member` or
    //                   // an external `import localName = require("whatever")`
    //                   const isLocalImport = !(target.flags & SymbolFlags.ValueModule) && !isVariableDeclaration(node);
    //                   addResult(factory.createImportEqualsDeclaration(
    //                       /*decorators*/ undefined,
    //                       /*modifiers*/ undefined,
    //                       /*isTypeOnly*/ false,
    //                       factory.createIdentifier(localName),
    //                       isLocalImport
    //                           ? symbolToName(target, context, SymbolFlags.All, /*expectsIdentifier*/ false)
    //                           : factory.createExternalModuleReference(factory.createStringLiteral(getSpecifierForModuleSymbol(target, context)))
    //                   ), isLocalImport ? modifierFlags : ModifierFlags.None);
    //                   break;
    //               case SyntaxKind.NamespaceExportDeclaration:
    //                   // export as namespace foo
    //                   // TODO: Not part of a file's local or export symbol tables
    //                   // Is bound into file.symbol.globalExports instead, which we don't currently traverse
    //                   addResult(factory.createNamespaceExportDeclaration(idText((node as NamespaceExportDeclaration).name)), ModifierFlags.None);
    //                   break;
    //               case SyntaxKind.ImportClause:
    //                   addResult(factory.createImportDeclaration(
    //                       /*decorators*/ undefined,
    //                       /*modifiers*/ undefined,
    //                       factory.createImportClause(/*isTypeOnly*/ false, factory.createIdentifier(localName), /*namedBindings*/ undefined),
    //                       // We use `target.parent || target` below as `target.parent` is unset when the target is a module which has been export assigned
    //                       // And then made into a default by the `esModuleInterop` or `allowSyntheticDefaultImports` flag
    //                       // In such cases, the `target` refers to the module itself already
    //                       factory.createStringLiteral(getSpecifierForModuleSymbol(target.parent || target, context)),
    //                        /*assertClause*/ undefined
    //                   ), ModifierFlags.None);
    //                   break;
    //               case SyntaxKind.NamespaceImport:
    //                   addResult(factory.createImportDeclaration(
    //                       /*decorators*/ undefined,
    //                       /*modifiers*/ undefined,
    //                       factory.createImportClause(/*isTypeOnly*/ false, /*importClause*/ undefined, factory.createNamespaceImport(factory.createIdentifier(localName))),
    //                       factory.createStringLiteral(getSpecifierForModuleSymbol(target, context)),
    //                        /*assertClause*/ undefined
    //                   ), ModifierFlags.None);
    //                   break;
    //               case SyntaxKind.NamespaceExport:
    //                   addResult(factory.createExportDeclaration(
    //                       /*decorators*/ undefined,
    //                       /*modifiers*/ undefined,
    //                       /*isTypeOnly*/ false,
    //                       factory.createNamespaceExport(factory.createIdentifier(localName)),
    //                       factory.createStringLiteral(getSpecifierForModuleSymbol(target, context))
    //                   ), ModifierFlags.None);
    //                   break;
    //               case SyntaxKind.ImportSpecifier:
    //                   addResult(factory.createImportDeclaration(
    //                       /*decorators*/ undefined,
    //                       /*modifiers*/ undefined,
    //                       factory.createImportClause(
    //                           /*isTypeOnly*/ false,
    //                           /*importClause*/ undefined,
    //                           factory.createNamedImports([
    //                               factory.createImportSpecifier(
    //                                   /*isTypeOnly*/ false,
    //                                   localName !== verbatimTargetName ? factory.createIdentifier(verbatimTargetName) : undefined,
    //                                   factory.createIdentifier(localName)
    //                               )
    //                           ])),
    //                       factory.createStringLiteral(getSpecifierForModuleSymbol(target.parent || target, context)),
    //                        /*assertClause*/ undefined
    //                   ), ModifierFlags.None);
    //                   break;
    //               case SyntaxKind.ExportSpecifier:
    //                   // does not use localName because the symbol name in this case refers to the name in the exports table,
    //                   // which we must exactly preserve
    //                   const specifier = (node.parent.parent as ExportDeclaration).moduleSpecifier;
    //                   // targetName is only used when the target is local, as otherwise the target is an alias that points at
    //                   // another file
    //                   serializeExportSpecifier(
    //                       unescapeLeadingUnderscores(symbol.escapedName),
    //                       specifier ? verbatimTargetName : targetName,
    //                       specifier && isStringLiteralLike(specifier) ? factory.createStringLiteral(specifier.text) : undefined
    //                   );
    //                   break;
    //               case SyntaxKind.ExportAssignment:
    //                   serializeMaybeAliasAssignment(symbol);
    //                   break;
    //               case SyntaxKind.BinaryExpression:
    //               case SyntaxKind.PropertyAccessExpression:
    //               case SyntaxKind.ElementAccessExpression:
    //                   // Could be best encoded as though an export specifier or as though an export assignment
    //                   // If name is default or export=, do an export assignment
    //                   // Otherwise do an export specifier
    //                   if (symbol.escapedName === InternalSymbolName.Default || symbol.escapedName === InternalSymbolName.ExportEquals) {
    //                       serializeMaybeAliasAssignment(symbol);
    //                   }
    //                   else {
    //                       serializeExportSpecifier(localName, targetName);
    //                   }
    //                   break;
    //               default:
    //                   return Debug.failBadSyntaxKind(node, "Unhandled alias declaration kind in symbol serializer!");
    //           }
    //       }

    //       function serializeExportSpecifier(localName: string, targetName: string, specifier?: Expression) {
    //           addResult(factory.createExportDeclaration(
    //               /*decorators*/ undefined,
    //               /*modifiers*/ undefined,
    //               /*isTypeOnly*/ false,
    //               factory.createNamedExports([factory.createExportSpecifier(/*isTypeOnly*/ false, localName !== targetName ? targetName : undefined, localName)]),
    //               specifier
    //           ), ModifierFlags.None);
    //       }

    //       /**
    //        * Returns `true` if an export assignment or declaration was produced for the symbol
    //        */
    //       function serializeMaybeAliasAssignment(symbol: Symbol): boolean {
    //           if (symbol.flags & SymbolFlags.Prototype) {
    //               return false;
    //           }
    //           const name = unescapeLeadingUnderscores(symbol.escapedName);
    //           const isExportEquals = name === InternalSymbolName.ExportEquals;
    //           const isDefault = name === InternalSymbolName.Default;
    //           const isExportAssignmentCompatibleSymbolName = isExportEquals || isDefault;
    //           // synthesize export = ref
    //           // ref should refer to either be a locally scoped symbol which we need to emit, or
    //           // a reference to another namespace/module which we may need to emit an `import` statement for
    //           const aliasDecl = symbol.declarations && getDeclarationOfAliasSymbol(symbol);
    //           // serialize what the alias points to, preserve the declaration's initializer
    //           const target = aliasDecl && getTargetOfAliasDeclaration(aliasDecl, /*dontRecursivelyResolve*/ true);
    //           // If the target resolves and resolves to a thing defined in this file, emit as an alias, otherwise emit as a const
    //           if (target && length(target.declarations) && some(target.declarations, d => getSourceFileOfNode(d) === getSourceFileOfNode(enclosingDeclaration))) {
    //               // In case `target` refers to a namespace member, look at the declaration and serialize the leftmost symbol in it
    //               // eg, `namespace A { export class B {} }; exports = A.B;`
    //               // Technically, this is all that's required in the case where the assignment is an entity name expression
    //               const expr = aliasDecl && ((isExportAssignment(aliasDecl) || isBinaryExpression(aliasDecl)) ? getExportAssignmentExpression(aliasDecl) : getPropertyAssignmentAliasLikeExpression(aliasDecl as ShorthandPropertyAssignment | PropertyAssignment | PropertyAccessExpression));
    //               const first = expr && isEntityNameExpression(expr) ? getFirstNonModuleExportsIdentifier(expr) : undefined;
    //               const referenced = first && resolveEntityName(first, SymbolFlags.All, /*ignoreErrors*/ true, /*dontResolveAlias*/ true, enclosingDeclaration);
    //               if (referenced || target) {
    //                   includePrivateSymbol(referenced || target);
    //               }

    //               // We disable the context's symbol tracker for the duration of this name serialization
    //               // as, by virtue of being here, the name is required to print something, and we don't want to
    //               // issue a visibility error on it. Only anonymous classes that an alias points at _would_ issue
    //               // a visibility error here (as they're not visible within any scope), but we want to hoist them
    //               // into the containing scope anyway, so we want to skip the visibility checks.
    //               const oldTrack = context.tracker.trackSymbol;
    //               context.tracker.trackSymbol = () => false;
    //               if (isExportAssignmentCompatibleSymbolName) {
    //                   results.push(factory.createExportAssignment(
    //                       /*decorators*/ undefined,
    //                       /*modifiers*/ undefined,
    //                       isExportEquals,
    //                       symbolToExpression(target, context, SymbolFlags.All)
    //                   ));
    //               }
    //               else {
    //                   if (first === expr && first) {
    //                       // serialize as `export {target as name}`
    //                       serializeExportSpecifier(name, idText(first));
    //                   }
    //                   else if (expr && isClassExpression(expr)) {
    //                       serializeExportSpecifier(name, getInternalSymbolName(target, symbolName(target)));
    //                   }
    //                   else {
    //                       // serialize as `import _Ref = t.arg.et; export { _Ref as name }`
    //                       const varName = getUnusedName(name, symbol);
    //                       addResult(factory.createImportEqualsDeclaration(
    //                           /*decorators*/ undefined,
    //                           /*modifiers*/ undefined,
    //                           /*isTypeOnly*/ false,
    //                           factory.createIdentifier(varName),
    //                           symbolToName(target, context, SymbolFlags.All, /*expectsIdentifier*/ false)
    //                       ), ModifierFlags.None);
    //                       serializeExportSpecifier(name, varName);
    //                   }
    //               }
    //               context.tracker.trackSymbol = oldTrack;
    //               return true;
    //           }
    //           else {
    //               // serialize as an anonymous property declaration
    //               const varName = getUnusedName(name, symbol);
    //               // We have to use `getWidenedType` here since the object within a json file is unwidened within the file
    //               // (Unwidened types can only exist in expression contexts and should never be serialized)
    //               const typeToSerialize = getWidenedType(getTypeOfSymbol(getMergedSymbol(symbol)));
    //               if (isTypeRepresentableAsFunctionNamespaceMerge(typeToSerialize, symbol)) {
    //                   // If there are no index signatures and `typeToSerialize` is an object type, emit as a namespace instead of a const
    //                   serializeAsFunctionNamespaceMerge(typeToSerialize, symbol, varName, isExportAssignmentCompatibleSymbolName ? ModifierFlags.None : ModifierFlags.Export);
    //               }
    //               else {
    //                   const statement = factory.createVariableStatement(/*modifiers*/ undefined, factory.createVariableDeclarationList([
    //                       factory.createVariableDeclaration(varName, /*exclamationToken*/ undefined, serializeTypeForDeclaration(context, typeToSerialize, symbol, enclosingDeclaration, includePrivateSymbol, bundled))
    //                   ], NodeFlags.Const));
    //                   // Inlined JSON types exported with [module.]exports= will already emit an export=, so should use `declare`.
    //                   // Otherwise, the type itself should be exported.
    //                   addResult(statement,
    //                       target && target.flags & SymbolFlags.Property && target.escapedName === InternalSymbolName.ExportEquals ? ModifierFlags.Ambient
    //                       : name === varName ? ModifierFlags.Export
    //                       : ModifierFlags.None);
    //               }
    //               if (isExportAssignmentCompatibleSymbolName) {
    //                   results.push(factory.createExportAssignment(
    //                       /*decorators*/ undefined,
    //                       /*modifiers*/ undefined,
    //                       isExportEquals,
    //                       factory.createIdentifier(varName)
    //                   ));
    //                   return true;
    //               }
    //               else if (name !== varName) {
    //                   serializeExportSpecifier(name, varName);
    //                   return true;
    //               }
    //               return false;
    //           }
    //       }

    //       function isTypeRepresentableAsFunctionNamespaceMerge(typeToSerialize: Type, hostSymbol: Symbol) {
    //           // Only object types which are not constructable, or indexable, whose members all come from the
    //           // context source file, and whose property names are all valid identifiers and not late-bound, _and_
    //           // whose input is not type annotated (if the input symbol has an annotation we can reuse, we should prefer it)
    //           const ctxSrc = getSourceFileOfNode(context.enclosingDeclaration);
    //           return getObjectFlags(typeToSerialize) & (ObjectFlags.Anonymous | ObjectFlags.Mapped) &&
    //           !length(getIndexInfosOfType(typeToSerialize)) &&
    //           !isClassInstanceSide(typeToSerialize) && // While a class instance is potentially representable as a NS, prefer printing a reference to the instance type and serializing the class
    //           !!(length(filter(getPropertiesOfType(typeToSerialize), isNamespaceMember)) || length(getSignaturesOfType(typeToSerialize, SignatureKind.Call))) &&
    //           !length(getSignaturesOfType(typeToSerialize, SignatureKind.Construct)) && // TODO: could probably serialize as function + ns + class, now that that's OK
    //           !getDeclarationWithTypeAnnotation(hostSymbol, enclosingDeclaration) &&
    //           !(typeToSerialize.symbol && some(typeToSerialize.symbol.declarations, d => getSourceFileOfNode(d) !== ctxSrc)) &&
    //           !some(getPropertiesOfType(typeToSerialize), p => isLateBoundName(p.escapedName)) &&
    //           !some(getPropertiesOfType(typeToSerialize), p => some(p.declarations, d => getSourceFileOfNode(d) !== ctxSrc)) &&
    //           every(getPropertiesOfType(typeToSerialize), p => isIdentifierText(symbolName(p), languageVersion));
    //       }

    //       function makeSerializePropertySymbol<T extends Node>(createProperty: (
    //           decorators: readonly Decorator[] | undefined,
    //           modifiers: readonly Modifier[] | undefined,
    //           name: string | PropertyName,
    //           questionOrExclamationToken: QuestionToken | undefined,
    //           type: TypeNode | undefined,
    //           initializer: Expression | undefined
    //       ) => T, methodKind: SignatureDeclaration["kind"], useAccessors: true): (p: Symbol, isStatic: boolean, baseType: Type | undefined) => (T | AccessorDeclaration | (T | AccessorDeclaration)[]);
    //       function makeSerializePropertySymbol<T extends Node>(createProperty: (
    //           decorators: readonly Decorator[] | undefined,
    //           modifiers: readonly Modifier[] | undefined,
    //           name: string | PropertyName,
    //           questionOrExclamationToken: QuestionToken | undefined,
    //           type: TypeNode | undefined,
    //           initializer: Expression | undefined
    //       ) => T, methodKind: SignatureDeclaration["kind"], useAccessors: false): (p: Symbol, isStatic: boolean, baseType: Type | undefined) => (T | T[]);
    //       function makeSerializePropertySymbol<T extends Node>(createProperty: (
    //           decorators: readonly Decorator[] | undefined,
    //           modifiers: readonly Modifier[] | undefined,
    //           name: string | PropertyName,
    //           questionOrExclamationToken: QuestionToken | undefined,
    //           type: TypeNode | undefined,
    //           initializer: Expression | undefined
    //       ) => T, methodKind: SignatureDeclaration["kind"], useAccessors: boolean): (p: Symbol, isStatic: boolean, baseType: Type | undefined) => (T | AccessorDeclaration | (T | AccessorDeclaration)[]) {
    //           return function serializePropertySymbol(p: Symbol, isStatic: boolean, baseType: Type | undefined): (T | AccessorDeclaration | (T | AccessorDeclaration)[]) {
    //               const modifierFlags = getDeclarationModifierFlagsFromSymbol(p);
    //               const isPrivate = !!(modifierFlags & ModifierFlags.Private);
    //               if (isStatic && (p.flags & (SymbolFlags.Type | SymbolFlags.Namespace | SymbolFlags.Alias))) {
    //                   // Only value-only-meaning symbols can be correctly encoded as class statics, type/namespace/alias meaning symbols
    //                   // need to be merged namespace members
    //                   return [];
    //               }
    //               if (p.flags & SymbolFlags.Prototype ||
    //                   (baseType && getPropertyOfType(baseType, p.escapedName)
    //                    && isReadonlySymbol(getPropertyOfType(baseType, p.escapedName)!) === isReadonlySymbol(p)
    //                    && (p.flags & SymbolFlags.Optional) === (getPropertyOfType(baseType, p.escapedName)!.flags & SymbolFlags.Optional)
    //                    && isTypeIdenticalTo(getTypeOfSymbol(p), getTypeOfPropertyOfType(baseType, p.escapedName)!))) {
    //                   return [];
    //               }
    //               const flag = (modifierFlags & ~ModifierFlags.Async) | (isStatic ? ModifierFlags.Static : 0);
    //               const name = getPropertyNameNodeForSymbol(p, context);
    //               const firstPropertyLikeDecl = p.declarations?.find(or(isPropertyDeclaration, isAccessor, isVariableDeclaration, isPropertySignature, isBinaryExpression, isPropertyAccessExpression));
    //               if (p.flags & SymbolFlags.Accessor && useAccessors) {
    //                   const result: AccessorDeclaration[] = [];
    //                   if (p.flags & SymbolFlags.SetAccessor) {
    //                       result.push(setTextRange(factory.createSetAccessorDeclaration(
    //                           /*decorators*/ undefined,
    //                           factory.createModifiersFromModifierFlags(flag),
    //                           name,
    //                           [factory.createParameterDeclaration(
    //                               /*decorators*/ undefined,
    //                               /*modifiers*/ undefined,
    //                               /*dotDotDotToken*/ undefined,
    //                               "arg",
    //                               /*questionToken*/ undefined,
    //                               isPrivate ? undefined : serializeTypeForDeclaration(context, getTypeOfSymbol(p), p, enclosingDeclaration, includePrivateSymbol, bundled)
    //                           )],
    //                           /*body*/ undefined
    //                       ), p.declarations?.find(isSetAccessor) || firstPropertyLikeDecl));
    //                   }
    //                   if (p.flags & SymbolFlags.GetAccessor) {
    //                       const isPrivate = modifierFlags & ModifierFlags.Private;
    //                       result.push(setTextRange(factory.createGetAccessorDeclaration(
    //                           /*decorators*/ undefined,
    //                           factory.createModifiersFromModifierFlags(flag),
    //                           name,
    //                           [],
    //                           isPrivate ? undefined : serializeTypeForDeclaration(context, getTypeOfSymbol(p), p, enclosingDeclaration, includePrivateSymbol, bundled),
    //                           /*body*/ undefined
    //                       ), p.declarations?.find(isGetAccessor) || firstPropertyLikeDecl));
    //                   }
    //                   return result;
    //               }
    //               // This is an else/if as accessors and properties can't merge in TS, but might in JS
    //               // If this happens, we assume the accessor takes priority, as it imposes more constraints
    //               else if (p.flags & (SymbolFlags.Property | SymbolFlags.Variable | SymbolFlags.Accessor)) {
    //                   return setTextRange(createProperty(
    //                       /*decorators*/ undefined,
    //                       factory.createModifiersFromModifierFlags((isReadonlySymbol(p) ? ModifierFlags.Readonly : 0) | flag),
    //                       name,
    //                       p.flags & SymbolFlags.Optional ? factory.createToken(SyntaxKind.QuestionToken) : undefined,
    //                       isPrivate ? undefined : serializeTypeForDeclaration(context, getTypeOfSymbol(p), p, enclosingDeclaration, includePrivateSymbol, bundled),
    //                       // TODO: https://github.com/microsoft/TypeScript/pull/32372#discussion_r328386357
    //                       // interface members can't have initializers, however class members _can_
    //                       /*initializer*/ undefined
    //                   ), p.declarations?.find(or(isPropertyDeclaration, isVariableDeclaration)) || firstPropertyLikeDecl);
    //               }
    //               if (p.flags & (SymbolFlags.Method | SymbolFlags.Function)) {
    //                   const type = getTypeOfSymbol(p);
    //                   const signatures = getSignaturesOfType(type, SignatureKind.Call);
    //                   if (flag & ModifierFlags.Private) {
    //                       return setTextRange(createProperty(
    //                           /*decorators*/ undefined,
    //                           factory.createModifiersFromModifierFlags((isReadonlySymbol(p) ? ModifierFlags.Readonly : 0) | flag),
    //                           name,
    //                           p.flags & SymbolFlags.Optional ? factory.createToken(SyntaxKind.QuestionToken) : undefined,
    //                           /*type*/ undefined,
    //                           /*initializer*/ undefined
    //                       ), p.declarations?.find(isFunctionLikeDeclaration) || signatures[0] && signatures[0].declaration || p.declarations && p.declarations[0]);
    //                   }

    //                   const results = [];
    //                   for (const sig of signatures) {
    //                       // Each overload becomes a separate method declaration, in order
    //                       const decl = signatureToSignatureDeclarationHelper(
    //                           sig,
    //                           methodKind,
    //                           context,
    //                           {
    //                               name,
    //                               questionToken: p.flags & SymbolFlags.Optional ? factory.createToken(SyntaxKind.QuestionToken) : undefined,
    //                               modifiers: flag ? factory.createModifiersFromModifierFlags(flag) : undefined
    //                           }
    //                       );
    //                       const location = sig.declaration && isPrototypePropertyAssignment(sig.declaration.parent) ? sig.declaration.parent : sig.declaration;
    //                       results.push(setTextRange(decl, location));
    //                   }
    //                   return results as unknown as T[];
    //               }
    //               // The `Constructor`'s symbol isn't in the class's properties lists, obviously, since it's a signature on the static
    //               return Debug.fail(`Unhandled class member kind! ${(p as any).__debugFlags || p.flags}`);
    //           };
    //       }

    //       function serializePropertySymbolForInterface(p: Symbol, baseType: Type | undefined) {
    //           return serializePropertySymbolForInterfaceWorker(p, /*isStatic*/ false, baseType);
    //       }

    //       function serializeSignatures(kind: SignatureKind, input: Type, baseType: Type | undefined, outputKind: SignatureDeclaration["kind"]) {
    //           const signatures = getSignaturesOfType(input, kind);
    //           if (kind === SignatureKind.Construct) {
    //               if (!baseType && every(signatures, s => length(s.parameters) === 0)) {
    //                   return []; // No base type, every constructor is empty - elide the extraneous `constructor()`
    //               }
    //               if (baseType) {
    //                   // If there is a base type, if every signature in the class is identical to a signature in the baseType, elide all the declarations
    //                   const baseSigs = getSignaturesOfType(baseType, SignatureKind.Construct);
    //                   if (!length(baseSigs) && every(signatures, s => length(s.parameters) === 0)) {
    //                       return []; // Base had no explicit signatures, if all our signatures are also implicit, return an empty list
    //                   }
    //                   if (baseSigs.length === signatures.length) {
    //                       let failed = false;
    //                       for (let i = 0; i < baseSigs.length; i++) {
    //                           if (!compareSignaturesIdentical(signatures[i], baseSigs[i], /*partialMatch*/ false, /*ignoreThisTypes*/ false, /*ignoreReturnTypes*/ true, compareTypesIdentical)) {
    //                               failed = true;
    //                               break;
    //                           }
    //                       }
    //                       if (!failed) {
    //                           return []; // Every signature was identical - elide constructor list as it is inherited
    //                       }
    //                   }
    //               }
    //               let privateProtected: ModifierFlags = 0;
    //               for (const s of signatures) {
    //                   if (s.declaration) {
    //                       privateProtected |= getSelectedEffectiveModifierFlags(s.declaration, ModifierFlags.Private | ModifierFlags.Protected);
    //                   }
    //               }
    //               if (privateProtected) {
    //                   return [setTextRange(factory.createConstructorDeclaration(
    //                       /*decorators*/ undefined,
    //                       factory.createModifiersFromModifierFlags(privateProtected),
    //                       /*parameters*/ [],
    //                       /*body*/ undefined,
    //                   ), signatures[0].declaration)];
    //               }
    //           }

    //           const results = [];
    //           for (const sig of signatures) {
    //               // Each overload becomes a separate constructor declaration, in order
    //               const decl = signatureToSignatureDeclarationHelper(sig, outputKind, context);
    //               results.push(setTextRange(decl, sig.declaration));
    //           }
    //           return results;
    //       }

    //       function serializeIndexSignatures(input: Type, baseType: Type | undefined) {
    //           const results: IndexSignatureDeclaration[] = [];
    //           for (const info of getIndexInfosOfType(input)) {
    //               if (baseType) {
    //                   const baseInfo = getIndexInfoOfType(baseType, info.keyType);
    //                   if (baseInfo) {
    //                       if (isTypeIdenticalTo(info.type, baseInfo.type)) {
    //                           continue; // elide identical index signatures
    //                       }
    //                   }
    //               }
    //               results.push(indexInfoToIndexSignatureDeclarationHelper(info, context, /*typeNode*/ undefined));
    //           }
    //           return results;
    //       }

    //       function serializeBaseType(t: Type, staticType: Type, rootName: string) {
    //           const ref = trySerializeAsTypeReference(t, SymbolFlags.Value);
    //           if (ref) {
    //               return ref;
    //           }
    //           const tempName = getUnusedName(`${rootName}_base`);
    //           const statement = factory.createVariableStatement(/*modifiers*/ undefined, factory.createVariableDeclarationList([
    //               factory.createVariableDeclaration(tempName, /*exclamationToken*/ undefined, typeToTypeNodeHelper(staticType, context))
    //           ], NodeFlags.Const));
    //           addResult(statement, ModifierFlags.None);
    //           return factory.createExpressionWithTypeArguments(factory.createIdentifier(tempName), /*typeArgs*/ undefined);
    //       }

    //       function trySerializeAsTypeReference(t: Type, flags: SymbolFlags) {
    //           let typeArgs: TypeNode[] | undefined;
    //           let reference: Expression | undefined;

    //           // We don't use `isValueSymbolAccessible` below. since that considers alternative containers (like modules)
    //           // which we can't write out in a syntactically valid way as an expression
    //           if ((t as TypeReference).target && isSymbolAccessibleByFlags((t as TypeReference).target.symbol, enclosingDeclaration, flags)) {
    //               typeArgs = map(getTypeArguments(t as TypeReference), t => typeToTypeNodeHelper(t, context));
    //               reference = symbolToExpression((t as TypeReference).target.symbol, context, SymbolFlags.Type);
    //           }
    //           else if (t.symbol && isSymbolAccessibleByFlags(t.symbol, enclosingDeclaration, flags)) {
    //               reference = symbolToExpression(t.symbol, context, SymbolFlags.Type);
    //           }
    //           if (reference) {
    //               return factory.createExpressionWithTypeArguments(reference, typeArgs);
    //           }
    //       }

    //       function serializeImplementedType(t: Type) {
    //           const ref = trySerializeAsTypeReference(t, SymbolFlags.Type);
    //           if (ref) {
    //               return ref;
    //           }
    //           if (t.symbol) {
    //               return factory.createExpressionWithTypeArguments(symbolToExpression(t.symbol, context, SymbolFlags.Type), /*typeArgs*/ undefined);
    //           }
    //       }

    //       function getUnusedName(input: string, symbol?: Symbol): string {
    //           const id = symbol ? getSymbolId(symbol) : undefined;
    //           if (id) {
    //               if (context.remappedSymbolNames!.has(id)) {
    //                   return context.remappedSymbolNames!.get(id)!;
    //               }
    //           }
    //           if (symbol) {
    //               input = getNameCandidateWorker(symbol, input);
    //           }
    //           let i = 0;
    //           const original = input;
    //           while (context.usedSymbolNames?.has(input)) {
    //               i++;
    //               input = `${original}_${i}`;
    //           }
    //           context.usedSymbolNames?.add(input);
    //           if (id) {
    //               context.remappedSymbolNames!.set(id, input);
    //           }
    //           return input;
    //       }

    //       function getNameCandidateWorker(symbol: Symbol, localName: string) {
    //           if (localName === InternalSymbolName.Default || localName === InternalSymbolName.Class || localName === InternalSymbolName.Function) {
    //               const flags = context.flags;
    //               context.flags |= NodeBuilderFlags.InInitialEntityName;
    //               const nameCandidate = getNameOfSymbolAsWritten(symbol, context);
    //               context.flags = flags;
    //               localName = nameCandidate.length > 0 && isSingleOrDoubleQuote(nameCandidate.charCodeAt(0)) ? stripQuotes(nameCandidate) : nameCandidate;
    //           }
    //           if (localName === InternalSymbolName.Default) {
    //               localName = "_default";
    //           }
    //           else if (localName === InternalSymbolName.ExportEquals) {
    //               localName = "_exports";
    //           }
    //           localName = isIdentifierText(localName, languageVersion) && !isStringANonContextualKeyword(localName) ? localName : "_" + localName.replace(/[^a-zA-Z0-9]/g, "_");
    //           return localName;
    //       }

    //       function getInternalSymbolName(symbol: Symbol, localName: string) {
    //           const id = getSymbolId(symbol);
    //           if (context.remappedSymbolNames!.has(id)) {
    //               return context.remappedSymbolNames!.get(id)!;
    //           }
    //           localName = getNameCandidateWorker(symbol, localName);
    //           // The result of this is going to be used as the symbol's name - lock it in, so `getUnusedName` will also pick it up
    //           context.remappedSymbolNames!.set(id, localName);
    //           return localName;
    //       }
    //   }
}

// fn withContext<T, C>(
//     enclosingDeclaration: Option<BoundNode>,
//     flags: Option<NodeBuilderFlags>,
//     // tracker: Option<SymbolTracker>,
//     cb: C,
// ) -> Option<T>
// where
//     C: FnOnce(NodeBuilderContext) -> T,
// {
//     //   Debug.assert(enclosingDeclaration === undefined || (enclosingDeclaration.flags & NodeFlags.Synthesized) === 0);
//     let context = NodeBuilderContext {
//         enclosingDeclaration,
//         flags: flags.unwrap_or(NodeBuilderFlags::None),
//         //   // If no full tracker is provided, fake up a dummy one with a basic limited-functionality moduleResolverHost
//         //   tracker: tracker && tracker.trackSymbol ? tracker : { trackSymbol: () => false, moduleResolverHost: flags! & NodeBuilderFlags.DoNotIncludeSymbolChain ? {
//         //       getCommonSourceDirectory: !!(host as Program).getCommonSourceDirectory ? () => (host as Program).getCommonSourceDirectory() : () => "",
//         //       getCurrentDirectory: () => host.getCurrentDirectory(),
//         //       getSymlinkCache: maybeBind(host, host.getSymlinkCache),
//         //       useCaseSensitiveFileNames: maybeBind(host, host.useCaseSensitiveFileNames),
//         //       redirectTargetsMap: host.redirectTargetsMap,
//         //       getProjectReferenceRedirect: fileName => host.getProjectReferenceRedirect(fileName),
//         //       isSourceOfProjectReferenceRedirect: fileName => host.isSourceOfProjectReferenceRedirect(fileName),
//         //       fileExists: fileName => host.fileExists(fileName),
//         //       getFileIncludeReasons: () => host.getFileIncludeReasons(),
//         //       readFile: host.readFile ? (fileName => host.readFile!(fileName)) : undefined,
//         //   } : undefined },
//         //   encounteredError: false,
//         //   reportedDiagnostic: false,
//         //   visitedTypes: undefined,
//         //   symbolDepth: undefined,
//         //   inferTypeParameters: undefined,
//         //   approximateLength: 0
//     };
//     //   context.tracker = wrapSymbolTrackerToReportForContext(context, context.tracker);
//     let resultingNode = cb(context);
//     //   if (context.truncating && context.flags & NodeBuilderFlags.NoTruncation) {
//     //       context.tracker?.reportTruncationError?.();
//     //   }
//     //   return context.encounteredError ? undefined : resultingNode;
//     Some(resultingNode)
// }
