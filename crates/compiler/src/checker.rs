use crate::ast;
use crate::binder::Binder;
use crate::node::*;
use crate::types::*;
use crate::types_composition::*;
use crate::utils::*;
use crate::CompilerOptions;
use crate::TypeCheckerHost;
use ahash::AHashMap;
use index::vec::IndexVec;
use std::collections::hash_map::Entry;
use std::convert::TryFrom;
use std::mem;
use swc_atoms::{js_word, JsWord};

// fn with_table<T: Default, F, Ret>(table: &mut T, op: F) -> Ret
// where
//     F: FnOnce(&mut T) -> Ret,
// {
//     let mut t = mem::take(table);
//     let res = op(&mut t);
//     *table = t;
//     res
// }

macro_rules! with_table {
    ($ident:ident = $table:expr, $block:block) => {{
        let $ident = mem::take($table);
        let res = $block;
        *$table = $ident;
        res
    }};
}

pub struct Checker {
    //////////////////// TSC: /////////////////////
    // typeCount: usize,
    // symbolCount: usize,
    // enumCount: usize,
    // totalInstantiationCount: usize,
    // instantiationCount: usize,
    // instantiationDepth: usize,
    // inlineLevel: usize,
    // currentNode: Option<BoundNode>,

    // emptySymbols:SymbolTable,
    // const arrayVariances = [VarianceFlags.Covariant];
    compilerOptions: CompilerOptions,
    languageVersion: ScriptTarget,
    // moduleKind = getEmitModuleKind(compilerOptions);
    useDefineForClassFields: bool,
    allowSyntheticDefaultImports: bool,
    strictNullChecks: bool,
    strictFunctionTypes: bool,
    strictBindCallApply: bool,
    strictPropertyInitialization: bool,
    noImplicitAny: bool,
    noImplicitThis: bool,
    useUnknownInCatchVariables: bool,
    // keyofStringsOnly: bool,
    // freshObjectLiteralFlag: bool,
    exactOptionalPropertyTypes: bool,

    // const checkBinaryExpression = createCheckBinaryExpression();
    // const emitResolver = createResolver();
    // const nodeBuilder = createNodeBuilder();
    undefinedSymbol: SymbolId,

    globalThisSymbol: SymbolId,

    argumentsSymbol: SymbolId,
    requireSymbol: SymbolId,

    /// This will be set during calls to `getResolvedSignature` where services determines an apparent number of arguments greater than what is actually provided.
    apparentArgumentCount: Option<usize>,

    // function getResolvedSignatureWorker(nodeIn: CallLikeExpression, candidatesOutArray: Signature[] | undefined, argumentCount: number | undefined, checkMode: CheckMode): Signature | undefined {
    //     const node = getParseTreeNode(nodeIn, isCallLikeExpression);
    //     apparentArgumentCount = argumentCount;
    //     const res = node ? getResolvedSignature(node, candidatesOutArray, checkMode) : undefined;
    //     apparentArgumentCount = undefined;
    //     return res;
    // }

    // const tupleTypes = new Map<string, GenericType>();
    // const unionTypes = new Map<string, UnionType>();
    // const intersectionTypes = new Map<string, Type>();
    // const stringLiteralTypes = new Map<string, StringLiteralType>();
    // const numberLiteralTypes = new Map<number, NumberLiteralType>();
    // const bigIntLiteralTypes = new Map<string, BigIntLiteralType>();
    // const enumLiteralTypes = new Map<string, LiteralType>();
    // const indexedAccessTypes = new Map<string, IndexedAccessType>();
    // const templateLiteralTypes = new Map<string, TemplateLiteralType>();
    // const stringMappingTypes = new Map<string, StringMappingType>();
    // const substitutionTypes = new Map<string, SubstitutionType>();
    // const subtypeReductionCache = new Map<string, Type[]>();
    // const evolvingArrayTypes: EvolvingArrayType[] = [];
    // const undefinedProperties: SymbolTable = new Map();
    unknownSymbol: SymbolId,
    resolvingSymbol: SymbolId,
    // const unresolvedSymbols = new Map<string, TransientSymbol>();
    // const errorTypes = new Map<string, Type>();
    anyType: TypeId,
    autoType: TypeId,
    wildcardType: TypeId,
    errorType: TypeId,
    unresolvedType: TypeId,
    nonInferrableAnyType: TypeId,
    intrinsicMarkerType: TypeId,
    unknownType: TypeId,
    nonNullUnknownType: TypeId,
    undefinedType: TypeId,
    undefinedWideningType: TypeId,
    optionalType: TypeId,
    missingType: TypeId,
    nullType: TypeId,
    nullWideningType: TypeId,
    stringType: TypeId,
    numberType: TypeId,
    bigintType: TypeId,
    falseType: TypeId,
    regularFalseType: TypeId,
    trueType: TypeId,
    regularTrueType: TypeId,
    // const booleanType = getUnionType([regularFalseType, regularTrueType]);
    esSymbolType: TypeId,
    voidType: TypeId,
    neverType: TypeId,
    silentNeverType: TypeId,
    nonInferrableType: TypeId,
    implicitNeverType: TypeId,
    unreachableNeverType: TypeId,
    nonPrimitiveType: TypeId,
    // const stringOrNumberType = getUnionType([stringType, numberType]);
    // const stringNumberSymbolType = getUnionType([stringType, numberType, esSymbolType]);
    // const keyofConstraintType = keyofStringsOnly ? stringType : stringNumberSymbolType;
    // const numberOrBigIntType = getUnionType([numberType, bigintType]);
    // const templateConstraintType = getUnionType([stringType, numberType, booleanType, bigintType, nullType, undefinedType]) as UnionType;

    // const restrictiveMapper: TypeMapper = makeFunctionTypeMapper(t => t.flags & TypeFlags.TypeParameter ? getRestrictiveTypeParameter(t as TypeParameter) : t);
    // const permissiveMapper: TypeMapper = makeFunctionTypeMapper(t => t.flags & TypeFlags.TypeParameter ? wildcardType : t);
    emptyObjectType: TypeId,
    // const emptyJsxObjectType = createAnonymousType(undefined, emptySymbols, emptyArray, emptyArray, emptyArray);

    // const emptyTypeLiteralSymbol = createSymbol(SymbolFlags::TypeLiteral, InternalSymbolName.Type);
    // const emptyTypeLiteralType = createAnonymousType(emptyTypeLiteralSymbol, emptySymbols, emptyArray, emptyArray, emptyArray);
    emptyGenericType: TypeId,

    // const anyFunctionType = createAnonymousType(undefined, emptySymbols, emptyArray, emptyArray, emptyArray);
    // // The anyFunctionType contains the anyFunctionType by definition. The flag is further propagated
    // // in getPropagatingFlagsOfTypes, and it is checked in inferFromTypes.
    // anyFunctionType.objectFlags |= ObjectFlags.NonInferrableType;

    // const noConstraintType = createAnonymousType(undefined, emptySymbols, emptyArray, emptyArray, emptyArray);
    // const circularConstraintType = createAnonymousType(undefined, emptySymbols, emptyArray, emptyArray, emptyArray);
    // const resolvingDefaultType = createAnonymousType(undefined, emptySymbols, emptyArray, emptyArray, emptyArray);

    // const markerSuperType = createTypeParameter();
    // const markerSubType = createTypeParameter();
    // markerSubType.constraint = markerSuperType;
    // const markerOtherType = createTypeParameter();

    // const noTypePredicate = createTypePredicate(TypePredicateKind.Identifier, "<<unresolved>>", 0, anyType);

    // const anySignature = createSignature(undefined, undefined, undefined, emptyArray, anyType, /*resolvedTypePredicate*/ undefined, 0, SignatureFlags.None);
    // const unknownSignature = createSignature(undefined, undefined, undefined, emptyArray, errorType, /*resolvedTypePredicate*/ undefined, 0, SignatureFlags.None);
    // const resolvingSignature = createSignature(undefined, undefined, undefined, emptyArray, anyType, /*resolvedTypePredicate*/ undefined, 0, SignatureFlags.None);
    // const silentNeverSignature = createSignature(undefined, undefined, undefined, emptyArray, silentNeverType, /*resolvedTypePredicate*/ undefined, 0, SignatureFlags.None);

    // const enumNumberIndexInfo = createIndexInfo(numberType, stringType, /*isReadonly*/ true);

    // const iterationTypesCache = new Map<string, IterationTypes>(); // cache for common IterationTypes instances
    // const noIterationTypes: IterationTypes = {
    //     get yieldType(): Type { return Debug.fail("Not supported"); },
    //     get returnType(): Type { return Debug.fail("Not supported"); },
    //     get nextType(): Type { return Debug.fail("Not supported"); },
    // };

    // const anyIterationTypes = createIterationTypes(anyType, anyType, anyType);
    // const anyIterationTypesExceptNext = createIterationTypes(anyType, anyType, unknownType);
    // const defaultIterationTypes = createIterationTypes(neverType, anyType, undefinedType); // default iteration types for `Iterator`.

    // const asyncIterationTypesResolver: IterationTypesResolver = {
    //     iterableCacheKey: "iterationTypesOfAsyncIterable",
    //     iteratorCacheKey: "iterationTypesOfAsyncIterator",
    //     iteratorSymbolName: "asyncIterator",
    //     getGlobalIteratorType: getGlobalAsyncIteratorType,
    //     getGlobalIterableType: getGlobalAsyncIterableType,
    //     getGlobalIterableIteratorType: getGlobalAsyncIterableIteratorType,
    //     getGlobalGeneratorType: getGlobalAsyncGeneratorType,
    //     resolveIterationType: getAwaitedType,
    //     mustHaveANextMethodDiagnostic: Diagnostics.An_async_iterator_must_have_a_next_method,
    //     mustBeAMethodDiagnostic: Diagnostics.The_0_property_of_an_async_iterator_must_be_a_method,
    //     mustHaveAValueDiagnostic: Diagnostics.The_type_returned_by_the_0_method_of_an_async_iterator_must_be_a_promise_for_a_type_with_a_value_property,
    // };

    // const syncIterationTypesResolver: IterationTypesResolver = {
    //     iterableCacheKey: "iterationTypesOfIterable",
    //     iteratorCacheKey: "iterationTypesOfIterator",
    //     iteratorSymbolName: "iterator",
    //     getGlobalIteratorType,
    //     getGlobalIterableType,
    //     getGlobalIterableIteratorType,
    //     getGlobalGeneratorType,
    //     resolveIterationType: (type, _errorNode) => type,
    //     mustHaveANextMethodDiagnostic: Diagnostics.An_iterator_must_have_a_next_method,
    //     mustBeAMethodDiagnostic: Diagnostics.The_0_property_of_an_iterator_must_be_a_method,
    //     mustHaveAValueDiagnostic: Diagnostics.The_type_returned_by_the_0_method_of_an_iterator_must_have_a_value_property,
    // };

    // interface DuplicateInfoForSymbol {
    //     readonly firstFileLocations: Declaration[];
    //     readonly secondFileLocations: Declaration[];
    //     readonly isBlockScoped: boolean;
    // }
    // interface DuplicateInfoForFiles {
    //     readonly firstFile: SourceFile;
    //     readonly secondFile: SourceFile;
    //     /** Key is symbol name. */
    //     readonly conflictingSymbols: ESMap<string, DuplicateInfoForSymbol>;
    // }
    // /** Key is "/path/to/a.ts|/path/to/b.ts". */
    // let amalgamatedDuplicates: ESMap<string, DuplicateInfoForFiles> | undefined;
    // const reverseMappedCache = new Map<string, Type | undefined>();
    // let inInferTypeForHomomorphicMappedType = false;
    // let ambientModulesCache: Symbol[] | undefined;
    // /**
    //  * List of every ambient module with a "*" wildcard.
    //  * Unlike other ambient modules, these can't be stored in `globals` because symbol tables only deal with exact matches.
    //  * This is only used if there is no exact match.
    //  */
    // let patternAmbientModules: PatternAmbientModule[];
    // let patternAmbientModuleAugmentations: ESMap<string, Symbol> | undefined;
    globalObjectType: Option<TypeId>,
    globalFunctionType: Option<TypeId>,
    globalCallableFunctionType: Option<TypeId>,
    globalNewableFunctionType: Option<TypeId>,
    globalArrayType: Option<TypeId>,
    globalReadonlyArrayType: Option<TypeId>,
    globalStringType: Option<TypeId>,
    globalNumberType: Option<TypeId>,
    globalBooleanType: Option<TypeId>,
    globalRegExpType: Option<TypeId>,
    globalThisType: Option<TypeId>,
    // let anyArrayType: Type;
    // let autoArrayType: Type;
    // let anyReadonlyArrayType: Type;
    // let deferredGlobalNonNullableTypeAlias: Symbol;

    // // The library files are only loaded when the feature is used.
    // // This allows users to just specify library files they want to used through --lib
    // // and they will not get an error from not having unrelated library files
    // let deferredGlobalESSymbolConstructorSymbol: Symbol | undefined;
    // let deferredGlobalESSymbolConstructorTypeSymbol: Symbol | undefined;
    // let deferredGlobalESSymbolType: ObjectType | undefined;
    // let deferredGlobalTypedPropertyDescriptorType: GenericType;
    // let deferredGlobalPromiseType: GenericType | undefined;
    // let deferredGlobalPromiseLikeType: GenericType | undefined;
    // let deferredGlobalPromiseConstructorSymbol: Symbol | undefined;
    // let deferredGlobalPromiseConstructorLikeType: ObjectType | undefined;
    // let deferredGlobalIterableType: GenericType | undefined;
    // let deferredGlobalIteratorType: GenericType | undefined;
    // let deferredGlobalIterableIteratorType: GenericType | undefined;
    // let deferredGlobalGeneratorType: GenericType | undefined;
    // let deferredGlobalIteratorYieldResultType: GenericType | undefined;
    // let deferredGlobalIteratorReturnResultType: GenericType | undefined;
    // let deferredGlobalAsyncIterableType: GenericType | undefined;
    // let deferredGlobalAsyncIteratorType: GenericType | undefined;
    // let deferredGlobalAsyncIterableIteratorType: GenericType | undefined;
    // let deferredGlobalAsyncGeneratorType: GenericType | undefined;
    // let deferredGlobalTemplateStringsArrayType: ObjectType | undefined;
    // let deferredGlobalImportMetaType: ObjectType;
    // let deferredGlobalImportMetaExpressionType: ObjectType;
    // let deferredGlobalImportCallOptionsType: ObjectType | undefined;
    // let deferredGlobalExtractSymbol: Symbol | undefined;
    // let deferredGlobalOmitSymbol: Symbol | undefined;
    // let deferredGlobalAwaitedSymbol: Symbol | undefined;
    // let deferredGlobalBigIntType: ObjectType | undefined;

    // const allPotentiallyUnusedIdentifiers = new Map<Path, PotentiallyUnusedIdentifier[]>(); // key is file name
    flowLoopStart: usize,
    flowLoopCount: usize,
    sharedFlowCount: usize,
    flowAnalysisDisabled: bool,
    flowInvocationCount: usize,
    lastFlowNode: Option<FlowNode>,
    lastFlowNodeReachable: bool,
    // let flowTypeCache: Type[] | undefined;

    // const emptyStringType = getStringLiteralType("");
    // const zeroType = getNumberLiteralType(0);
    // const zeroBigIntType = getBigIntLiteralType({ negative: false, base10Value: "0" });

    // const resolutionTargets: TypeSystemEntity[] = [];
    // const resolutionResults: boolean[] = [];
    // const resolutionPropertyNames: TypeSystemPropertyName[] = [];

    // let suggestionCount = 0;
    // const maximumSuggestionCount = 10;
    mergedSymbols: IndexVec<SymbolMergeId, SymbolId>,
    // const symbolLinks: SymbolLinks[] = [];
    // const nodeLinks: NodeLinks[] = [];
    // const flowLoopCaches: ESMap<string, Type>[] = [];
    // const flowLoopNodes: FlowNode[] = [];
    // const flowLoopKeys: string[] = [];
    // const flowLoopTypes: Type[][] = [];
    // const sharedFlowNodes: FlowNode[] = [];
    // const sharedFlowTypes: FlowType[] = [];
    // const flowNodeReachable: (boolean | undefined)[] = [];
    // const flowNodePostSuper: (boolean | undefined)[] = [];
    // const potentialThisCollisions: Node[] = [];
    // const potentialNewTargetCollisions: Node[] = [];
    // const potentialWeakMapSetCollisions: Node[] = [];
    // const potentialReflectCollisions: Node[] = [];
    // const awaitedTypeStack: number[] = [];

    // const diagnostics = createDiagnosticCollection();
    // const suggestionDiagnostics = createDiagnosticCollection();

    // const typeofTypesByName: ReadonlyESMap<string, Type> = new Map(getEntries({
    //     string: stringType,
    //     number: numberType,
    //     bigint: bigintType,
    //     boolean: booleanType,
    //     symbol: esSymbolType,
    //     undefined: undefinedType
    // }));
    // const typeofType = createTypeofType();

    // let _jsxNamespace: __String;
    // let _jsxFactoryEntity: EntityName | undefined;
    // let outofbandVarianceMarkerHandler: ((onlyUnreliable: boolean) => void) | undefined;

    // const subtypeRelation = new Map<string, RelationComparisonResult>();
    // const strictSubtypeRelation = new Map<string, RelationComparisonResult>();
    // const assignableRelation = new Map<string, RelationComparisonResult>();
    // const comparableRelation = new Map<string, RelationComparisonResult>();
    // const identityRelation = new Map<string, RelationComparisonResult>();
    // const enumRelation = new Map<string, RelationComparisonResult>();
    builtinGlobals: SymbolTable,
    ///////////////////////////////////////////////

    //////////////////// Ours: ////////////////////
    pub node_data: AHashMap<BoundNode, NodeData>,
    pub flow_nodes: IndexVec<FlowNodeId, FlowNode>,
    pub symbols: IndexVec<SymbolId, Symbol>,

    pub types: IndexVec<TypeId, Type>,
    pub host: TypeCheckerHost,
    pub signatures: IndexVec<SignatureId, Signature>,
    ///////////////////////////////////////////////
}

impl Checker {
    pub fn new(mut host: TypeCheckerHost) -> Self {
        // TODO: comment from tsc
        let mut bind_res = Binder::bind_source_files(&mut host.files);

        // std::fs::write("./node_data.txt", format!("{:#?}", &bind_res.node_data));
        // std::fs::write("./flow_nodes.txt", format!("{:#?}", &bind_res.flow_nodes));
        // std::fs::write("./symbols.txt", format!("{:#?}", &bind_res.symbols));

        // println!("node_data: {:#?}", &bind_res.node_data);
        // println!("flow_nodes: {:#?}", &bind_res.flow_nodes);
        // println!("symbols: {:#?}", &bind_res.symbols);

        println!("node_data: {:#?}", &bind_res.node_data.len());
        println!("flow_nodes: {:#?}", &bind_res.flow_nodes.len());
        println!("symbols: {:#?}", &bind_res.symbols.len());

        // const getPackagesMap = memoize(() => {
        //     // A package name maps to true when we detect it has .d.ts files.
        //     // This is useful as an approximation of whether a package bundles its own types.
        //     // Note: we only look at files already found by module resolution,
        //     // so there may be files we did not consider.
        //     const map = new Map<string, boolean>();
        //     host.getSourceFiles().forEach(sf => {
        //         if (!sf.resolvedModules) return;

        //         sf.resolvedModules.forEach(r => {
        //             if (r && r.packageId) map.set(r.packageId.name, r.extension === Extension.Dts || !!map.get(r.packageId.name));
        //         });
        //     });
        //     return map;
        // });

        // // Cancellation that controls whether or not we can cancel in the middle of type checking.
        // // In general cancelling is *not* safe for the type checker.  We might be in the middle of
        // // computing something, and we will leave our internals in an inconsistent state.  Callers
        // // who set the cancellation token should catch if a cancellation exception occurs, and
        // // should throw away and create a new TypeChecker.
        // //
        // // Currently we only support setting the cancellation token when getting diagnostics.  This
        // // is because diagnostics can be quite expensive, and we want to allow hosts to bail out if
        // // they no longer need the information (for example, if the user started editing again).
        // let cancellationToken: CancellationToken | undefined;
        // let requestedExternalEmitHelpers: ExternalEmitHelpers;
        // let externalHelpersModule: Symbol;

        // const Symbol = objectAllocator.getSymbolConstructor();
        // const Type = objectAllocator.getTypeConstructor();
        // const Signature = objectAllocator.getSignatureConstructor();

        // let typeCount = 0;
        // let symbolCount = 0;
        // let enumCount = 0;
        // let totalInstantiationCount = 0;
        // let instantiationCount = 0;
        // let instantiationDepth = 0;
        // let inlineLevel = 0;
        // let currentNode: Node | undefined;

        // const emptySymbols = createSymbolTable();
        // const arrayVariances = [VarianceFlags.Covariant];

        let compilerOptions = host.compiler_options;
        let languageVersion = compilerOptions.getEmitScriptTarget();
        // let moduleKind = compilerOptions.getEmitModuleKind();
        let useDefineForClassFields = compilerOptions.getUseDefineForClassFields();
        let allowSyntheticDefaultImports = compilerOptions.getAllowSyntheticDefaultImports();
        let strictNullChecks = compilerOptions.getStrictOptionValue("strictNullChecks");
        let strictFunctionTypes = compilerOptions.getStrictOptionValue("strictFunctionTypes");
        let strictBindCallApply = compilerOptions.getStrictOptionValue("strictBindCallApply");
        let strictPropertyInitialization =
            compilerOptions.getStrictOptionValue("strictPropertyInitialization");
        let noImplicitAny = compilerOptions.getStrictOptionValue("noImplicitAny");
        let noImplicitThis = compilerOptions.getStrictOptionValue("noImplicitThis");
        let useUnknownInCatchVariables =
            compilerOptions.getStrictOptionValue("useUnknownInCatchVariables");
        // let keyofStringsOnly = !!compilerOptions.keyofStringsOnly;
        // let freshObjectLiteralFlag = compilerOptions.suppressExcessPropertyErrors ? 0 : ObjectFlags.FreshLiteral;
        let exactOptionalPropertyTypes = compilerOptions.exactOptionalPropertyTypes();

        // const checkBinaryExpression = createCheckBinaryExpression();
        // const emitResolver = createResolver();
        // const nodeBuilder = createNodeBuilder();

        macro_rules! create_symbol {
            ($flags:expr, $name:expr) => {
                create_symbol!($flags, $name, Default::default())
            };
            ($flags:expr, $name:expr, $check_flags:expr) => {{
                let transient_symbol = Symbol::new_transient_symbol(
                    $flags | SymbolFlags::Transient,
                    $name,
                    $check_flags,
                );
                bind_res.symbols.push(transient_symbol)
            }};
        }

        let mut globals = SymbolTable::default();
        let undefinedSymbol = create_symbol!(SymbolFlags::Property, "undefined".into());

        let globalThisSymbol = create_symbol!(
            SymbolFlags::Module,
            "globalThis".into(),
            CheckFlags::Readonly
        );
        globals.insert(
            bind_res.symbols[globalThisSymbol].escapedName().clone(),
            globalThisSymbol,
        );
        *bind_res.symbols[globalThisSymbol].exports_mut() = globals;

        let argumentsSymbol = create_symbol!(SymbolFlags::Property, "arguments".into());
        let requireSymbol = create_symbol!(SymbolFlags::Property, "require".into());

        // function getResolvedSignatureWorker(nodeIn: CallLikeExpression, candidatesOutArray: Signature[] | undefined, argumentCount: number | undefined, checkMode: CheckMode): Signature | undefined {
        //     const node = getParseTreeNode(nodeIn, isCallLikeExpression);
        //     apparentArgumentCount = argumentCount;
        //     const res = node ? getResolvedSignature(node, candidatesOutArray, checkMode) : undefined;
        //     apparentArgumentCount = undefined;
        //     return res;
        // }

        // const tupleTypes = new Map<string, GenericType>();
        // const unionTypes = new Map<string, UnionType>();
        // const intersectionTypes = new Map<string, Type>();
        // const stringLiteralTypes = new Map<string, StringLiteralType>();
        // const numberLiteralTypes = new Map<number, NumberLiteralType>();
        // const bigIntLiteralTypes = new Map<string, BigIntLiteralType>();
        // const enumLiteralTypes = new Map<string, LiteralType>();
        // const indexedAccessTypes = new Map<string, IndexedAccessType>();
        // const templateLiteralTypes = new Map<string, TemplateLiteralType>();
        // const stringMappingTypes = new Map<string, StringMappingType>();
        // const substitutionTypes = new Map<string, SubstitutionType>();
        // const subtypeReductionCache = new Map<string, Type[]>();
        // const evolvingArrayTypes: EvolvingArrayType[] = [];
        // const undefinedProperties: SymbolTable = new Map();

        let unknownSymbol = create_symbol!(SymbolFlags::Property, "unknown".into());
        let resolvingSymbol =
            create_symbol!(SymbolFlags::default(), InternalSymbolName::Resolving.into());
        // const unresolvedSymbols = new Map<string, TransientSymbol>();
        // const errorTypes = new Map<string, Type>();

        let mut types = IndexVec::new();

        macro_rules! create_intrinsic_type {
            ($flags:expr, $intrinsic_name:literal) => {
                create_intrinsic_type!($flags, $intrinsic_name, ObjectFlags::empty())
            };
            ($flags:expr, $intrinsic_name:literal, $object_flags:expr) => {{
                let ty = Type::IntrinsicType(IntrinsicType {
                    type_base: TypeBase::new($flags, None),
                    intrinsic_type_base: IntrinsicTypeBase {
                        intrinsicName: $intrinsic_name.into(),
                        objectFlags: $object_flags,
                    },
                });
                types.push(ty)
            }};
        }

        macro_rules! create_freshable_bool_types {
            ($flags:expr, $intrinsic_name:literal) => {{
                let regular_id = types.next_index();
                let fresh_id = regular_id + 1;

                let regular_ty = Type::FreshableIntrinsicType(FreshableIntrinsicType {
                    intrinsic_type_base: IntrinsicTypeBase {
                        intrinsicName: $intrinsic_name.into(),
                        objectFlags: ObjectFlags::empty(),
                    },
                    type_base: TypeBase::new($flags, None),
                    regularType: regular_id,
                    freshType: fresh_id,
                });

                let fresh_ty = Type::FreshableIntrinsicType(FreshableIntrinsicType {
                    intrinsic_type_base: IntrinsicTypeBase {
                        intrinsicName: $intrinsic_name.into(),
                        objectFlags: ObjectFlags::empty(),
                    },
                    type_base: TypeBase::new($flags, None),
                    regularType: regular_id,
                    freshType: fresh_id,
                });

                (types.push(regular_ty), types.push(fresh_ty))
            }};
        }

        let anyType = create_intrinsic_type!(TypeFlags::Any, "any");
        let autoType = create_intrinsic_type!(TypeFlags::Any, "any");
        let wildcardType = create_intrinsic_type!(TypeFlags::Any, "any");
        let errorType = create_intrinsic_type!(TypeFlags::Any, "error");
        let unresolvedType = create_intrinsic_type!(TypeFlags::Any, "unresolved");
        let nonInferrableAnyType =
            create_intrinsic_type!(TypeFlags::Any, "any", ObjectFlags::ContainsWideningType);
        let intrinsicMarkerType = create_intrinsic_type!(TypeFlags::Any, "intrinsic");
        let unknownType = create_intrinsic_type!(TypeFlags::Unknown, "unknown");
        let nonNullUnknownType = create_intrinsic_type!(TypeFlags::Unknown, "unknown");
        let undefinedType = create_intrinsic_type!(TypeFlags::Undefined, "undefined");
        let undefinedWideningType = if strictNullChecks {
            undefinedType
        } else {
            create_intrinsic_type!(
                TypeFlags::Undefined,
                "undefined",
                ObjectFlags::ContainsWideningType
            )
        };
        let optionalType = create_intrinsic_type!(TypeFlags::Undefined, "undefined");
        let missingType = if exactOptionalPropertyTypes {
            create_intrinsic_type!(TypeFlags::Undefined, "undefined")
        } else {
            undefinedType
        };
        let nullType = create_intrinsic_type!(TypeFlags::Null, "null");
        let nullWideningType = if strictNullChecks {
            nullType
        } else {
            create_intrinsic_type!(TypeFlags::Null, "null", ObjectFlags::ContainsWideningType)
        };
        let stringType = create_intrinsic_type!(TypeFlags::String, "string");
        let numberType = create_intrinsic_type!(TypeFlags::Number, "number");
        let bigintType = create_intrinsic_type!(TypeFlags::BigInt, "bigint");
        let (regularFalseType, falseType) =
            create_freshable_bool_types!(TypeFlags::BooleanLiteral, "false");
        let (regularTrueType, trueType) =
            create_freshable_bool_types!(TypeFlags::BooleanLiteral, "true");
        // let booleanType = getUnionType([regularFalseType, regularTrueType]);
        // let booleanType = {

        //     let typeSet = vec![regularFalseType, regularTrueType];

        //     let id = format_args!("{:?},{:?}", regularFalseType, regularTrueType);
        //     let ty = createType(TypeFlags.Union) as UnionType;
        //     ty.objectFlags = ObjectFlags::PrimitiveUnion;
        //     ty.types = typeSet;
        //     ty.flags |= TypeFlags::Boolean;
        //     (ty as UnionType & IntrinsicType).intrinsicName = "boolean";
        //     unionTypes.set(id, ty);
        //     ty
        // }
        let esSymbolType = create_intrinsic_type!(TypeFlags::ESSymbol, "symbol");
        let voidType = create_intrinsic_type!(TypeFlags::Void, "void");
        let neverType = create_intrinsic_type!(TypeFlags::Never, "never");
        let silentNeverType = create_intrinsic_type!(TypeFlags::Never, "never");
        let nonInferrableType =
            create_intrinsic_type!(TypeFlags::Never, "never", ObjectFlags::NonInferrableType);
        let implicitNeverType = create_intrinsic_type!(TypeFlags::Never, "never");
        let unreachableNeverType = create_intrinsic_type!(TypeFlags::Never, "never");
        let nonPrimitiveType = create_intrinsic_type!(TypeFlags::NonPrimitive, "object");
        // const stringOrNumberType = getUnionType([stringType, numberType]);
        // const stringNumberSymbolType = getUnionType([stringType, numberType, esSymbolType]);
        // const keyofConstraintType = keyofStringsOnly ? stringType : stringNumberSymbolType;
        // const numberOrBigIntType = getUnionType([numberType, bigintType]);
        // const templateConstraintType = getUnionType([stringType, numberType, booleanType, bigintType, nullType, undefinedType]) as UnionType;

        // const restrictiveMapper: TypeMapper = makeFunctionTypeMapper(t => t.flags & TypeFlags.TypeParameter ? getRestrictiveTypeParameter(t as TypeParameter) : t);
        // const permissiveMapper: TypeMapper = makeFunctionTypeMapper(t => t.flags & TypeFlags.TypeParameter ? wildcardType : t);

        macro_rules! create_anonymous_type {
            ($symbol:expr $(, $extra_object_flags:path)?) => {{
                let ty = Type::ResolvedType(ResolvedType::new(
                    TypeFlags::Object, $symbol, ObjectFlags::Anonymous $(| $extra_object_flags)?
                ));

                types.push(ty)
            }};
        }

        let emptyObjectType = create_anonymous_type!(None);
        let emptyJsxObjectType = create_anonymous_type!(None, ObjectFlags::JsxAttributes);

        let emptyTypeLiteralSymbol =
            create_symbol!(SymbolFlags::TypeLiteral, InternalSymbolName::Type.into());
        let emptyTypeLiteralType = create_anonymous_type!(Some(emptyTypeLiteralSymbol));

        let emptyGenericType = {
            let ty = Type::GenericType(GenericType::new(
                InterfaceType::default(),
                TypeReference::default(),
                ObjectTypeBase::new(ObjectFlags::Anonymous),
                TypeBase::new(TypeFlags::Object, None),
            ));

            types.push(ty)
        };

        // The anyFunctionType contains the anyFunctionType by definition. The flag is further propagated
        // in getPropagatingFlagsOfTypes, and it is checked in inferFromTypes.
        let anyFunctionType = create_anonymous_type!(None, ObjectFlags::NonInferrableType);

        let noConstraintType = create_anonymous_type!(None);
        let circularConstraintType = create_anonymous_type!(None);
        let resolvingDefaultType = create_anonymous_type!(None);

        // const markerSuperType = createTypeParameter();
        // const markerSubType = createTypeParameter();
        // markerSubType.constraint = markerSuperType;
        // const markerOtherType = createTypeParameter();

        // const noTypePredicate = createTypePredicate(TypePredicateKind.Identifier, "<<unresolved>>", 0, anyType);

        // const anySignature = createSignature(undefined, undefined, undefined, emptyArray, anyType, /*resolvedTypePredicate*/ undefined, 0, SignatureFlags.None);
        // const unknownSignature = createSignature(undefined, undefined, undefined, emptyArray, errorType, /*resolvedTypePredicate*/ undefined, 0, SignatureFlags.None);
        // const resolvingSignature = createSignature(undefined, undefined, undefined, emptyArray, anyType, /*resolvedTypePredicate*/ undefined, 0, SignatureFlags.None);
        // const silentNeverSignature = createSignature(undefined, undefined, undefined, emptyArray, silentNeverType, /*resolvedTypePredicate*/ undefined, 0, SignatureFlags.None);

        // const enumNumberIndexInfo = createIndexInfo(numberType, stringType, /*isReadonly*/ true);

        // const iterationTypesCache = new Map<string, IterationTypes>(); // cache for common IterationTypes instances
        // const noIterationTypes: IterationTypes = {
        //     get yieldType(): Type { return Debug.fail("Not supported"); },
        //     get returnType(): Type { return Debug.fail("Not supported"); },
        //     get nextType(): Type { return Debug.fail("Not supported"); },
        // };

        // const anyIterationTypes = createIterationTypes(anyType, anyType, anyType);
        // const anyIterationTypesExceptNext = createIterationTypes(anyType, anyType, unknownType);
        // const defaultIterationTypes = createIterationTypes(neverType, anyType, undefinedType); // default iteration types for `Iterator`.

        // const asyncIterationTypesResolver: IterationTypesResolver = {
        //     iterableCacheKey: "iterationTypesOfAsyncIterable",
        //     iteratorCacheKey: "iterationTypesOfAsyncIterator",
        //     iteratorSymbolName: "asyncIterator",
        //     getGlobalIteratorType: getGlobalAsyncIteratorType,
        //     getGlobalIterableType: getGlobalAsyncIterableType,
        //     getGlobalIterableIteratorType: getGlobalAsyncIterableIteratorType,
        //     getGlobalGeneratorType: getGlobalAsyncGeneratorType,
        //     resolveIterationType: getAwaitedType,
        //     mustHaveANextMethodDiagnostic: Diagnostics.An_async_iterator_must_have_a_next_method,
        //     mustBeAMethodDiagnostic: Diagnostics.The_0_property_of_an_async_iterator_must_be_a_method,
        //     mustHaveAValueDiagnostic: Diagnostics.The_type_returned_by_the_0_method_of_an_async_iterator_must_be_a_promise_for_a_type_with_a_value_property,
        // };

        // const syncIterationTypesResolver: IterationTypesResolver = {
        //     iterableCacheKey: "iterationTypesOfIterable",
        //     iteratorCacheKey: "iterationTypesOfIterator",
        //     iteratorSymbolName: "iterator",
        //     getGlobalIteratorType,
        //     getGlobalIterableType,
        //     getGlobalIterableIteratorType,
        //     getGlobalGeneratorType,
        //     resolveIterationType: (type, _errorNode) => type,
        //     mustHaveANextMethodDiagnostic: Diagnostics.An_iterator_must_have_a_next_method,
        //     mustBeAMethodDiagnostic: Diagnostics.The_0_property_of_an_iterator_must_be_a_method,
        //     mustHaveAValueDiagnostic: Diagnostics.The_type_returned_by_the_0_method_of_an_iterator_must_have_a_value_property,
        // };

        // interface DuplicateInfoForSymbol {
        //     readonly firstFileLocations: Declaration[];
        //     readonly secondFileLocations: Declaration[];
        //     readonly isBlockScoped: boolean;
        // }
        // interface DuplicateInfoForFiles {
        //     readonly firstFile: SourceFile;
        //     readonly secondFile: SourceFile;
        //     /** Key is symbol name. */
        //     readonly conflictingSymbols: ESMap<string, DuplicateInfoForSymbol>;
        // }
        // /** Key is "/path/to/a.ts|/path/to/b.ts". */
        // let amalgamatedDuplicates: ESMap<string, DuplicateInfoForFiles> | undefined;
        // const reverseMappedCache = new Map<string, Type | undefined>();
        // let inInferTypeForHomomorphicMappedType = false;
        // let ambientModulesCache: Symbol[] | undefined;
        // /**
        //  * List of every ambient module with a "*" wildcard.
        //  * Unlike other ambient modules, these can't be stored in `globals` because symbol tables only deal with exact matches.
        //  * This is only used if there is no exact match.
        //  */
        // let patternAmbientModules: PatternAmbientModule[];
        // let patternAmbientModuleAugmentations: ESMap<string, Symbol> | undefined;

        // let anyArrayType: Type;
        // let autoArrayType: Type;
        // let anyReadonlyArrayType: Type;
        // let deferredGlobalNonNullableTypeAlias: Symbol;

        // // The library files are only loaded when the feature is used.
        // // This allows users to just specify library files they want to used through --lib
        // // and they will not get an error from not having unrelated library files
        // let deferredGlobalESSymbolConstructorSymbol: Symbol | undefined;
        // let deferredGlobalESSymbolConstructorTypeSymbol: Symbol | undefined;
        // let deferredGlobalESSymbolType: ObjectType | undefined;
        // let deferredGlobalTypedPropertyDescriptorType: GenericType;
        // let deferredGlobalPromiseType: GenericType | undefined;
        // let deferredGlobalPromiseLikeType: GenericType | undefined;
        // let deferredGlobalPromiseConstructorSymbol: Symbol | undefined;
        // let deferredGlobalPromiseConstructorLikeType: ObjectType | undefined;
        // let deferredGlobalIterableType: GenericType | undefined;
        // let deferredGlobalIteratorType: GenericType | undefined;
        // let deferredGlobalIterableIteratorType: GenericType | undefined;
        // let deferredGlobalGeneratorType: GenericType | undefined;
        // let deferredGlobalIteratorYieldResultType: GenericType | undefined;
        // let deferredGlobalIteratorReturnResultType: GenericType | undefined;
        // let deferredGlobalAsyncIterableType: GenericType | undefined;
        // let deferredGlobalAsyncIteratorType: GenericType | undefined;
        // let deferredGlobalAsyncIterableIteratorType: GenericType | undefined;
        // let deferredGlobalAsyncGeneratorType: GenericType | undefined;
        // let deferredGlobalTemplateStringsArrayType: ObjectType | undefined;
        // let deferredGlobalImportMetaType: ObjectType;
        // let deferredGlobalImportMetaExpressionType: ObjectType;
        // let deferredGlobalImportCallOptionsType: ObjectType | undefined;
        // let deferredGlobalExtractSymbol: Symbol | undefined;
        // let deferredGlobalOmitSymbol: Symbol | undefined;
        // let deferredGlobalAwaitedSymbol: Symbol | undefined;
        // let deferredGlobalBigIntType: ObjectType | undefined;

        // const allPotentiallyUnusedIdentifiers = new Map<Path, PotentiallyUnusedIdentifier[]>(); // key is file name

        // let flowTypeCache: Type[] | undefined;

        // const emptyStringType = getStringLiteralType("");
        // const zeroType = getNumberLiteralType(0);
        // const zeroBigIntType = getBigIntLiteralType({ negative: false, base10Value: "0" });

        // const resolutionTargets: TypeSystemEntity[] = [];
        // const resolutionResults: boolean[] = [];
        // const resolutionPropertyNames: TypeSystemPropertyName[] = [];

        // let suggestionCount = 0;
        // const maximumSuggestionCount = 10;
        // const symbolLinks: SymbolLinks[] = [];
        // const nodeLinks: NodeLinks[] = [];
        // const flowLoopCaches: ESMap<string, Type>[] = [];
        // const flowLoopNodes: FlowNode[] = [];
        // const flowLoopKeys: string[] = [];
        // const flowLoopTypes: Type[][] = [];
        // const sharedFlowNodes: FlowNode[] = [];
        // const sharedFlowTypes: FlowType[] = [];
        // const flowNodeReachable: (boolean | undefined)[] = [];
        // const flowNodePostSuper: (boolean | undefined)[] = [];
        // const potentialThisCollisions: Node[] = [];
        // const potentialNewTargetCollisions: Node[] = [];
        // const potentialWeakMapSetCollisions: Node[] = [];
        // const potentialReflectCollisions: Node[] = [];
        // const awaitedTypeStack: number[] = [];

        // const diagnostics = createDiagnosticCollection();
        // const suggestionDiagnostics = createDiagnosticCollection();

        // const typeofTypesByName: ReadonlyESMap<string, Type> = new Map(getEntries({
        //     string: stringType,
        //     number: numberType,
        //     bigint: bigintType,
        //     boolean: booleanType,
        //     symbol: esSymbolType,
        //     undefined: undefinedType
        // }));
        // const typeofType = createTypeofType();

        // let _jsxNamespace: __String;
        // let _jsxFactoryEntity: EntityName | undefined;
        // let outofbandVarianceMarkerHandler: ((onlyUnreliable: boolean) => void) | undefined;

        // const subtypeRelation = new Map<string, RelationComparisonResult>();
        // const strictSubtypeRelation = new Map<string, RelationComparisonResult>();
        // const assignableRelation = new Map<string, RelationComparisonResult>();
        // const comparableRelation = new Map<string, RelationComparisonResult>();
        // const identityRelation = new Map<string, RelationComparisonResult>();
        // const enumRelation = new Map<string, RelationComparisonResult>();

        let mut builtinGlobals = SymbolTable::default();
        builtinGlobals.insert(
            bind_res.symbols[undefinedSymbol].escapedName().clone(),
            undefinedSymbol,
        );

        let mut checker = Checker {
            // typeCount: usize,
            // symbolCount: usize,
            // enumCount: usize,
            // totalInstantiationCount: usize,
            // instantiationCount: usize,
            // instantiationDepth: usize,
            // inlineLevel: usize,
            // currentNode: Option<BoundNode>,

            // emptySymbols:SymbolTable,
            // const arrayVariances = [VarianceFlags.Covariant];
            compilerOptions,
            languageVersion,
            // moduleKind,
            useDefineForClassFields,
            allowSyntheticDefaultImports,
            strictNullChecks,
            strictFunctionTypes,
            strictBindCallApply,
            strictPropertyInitialization,
            noImplicitAny,
            noImplicitThis,
            useUnknownInCatchVariables,
            // keyofStringsOnly,
            // freshObjectLiteralFlag,
            exactOptionalPropertyTypes,

            // const checkBinaryExpression = createCheckBinaryExpression();
            // const emitResolver = createResolver();
            // const nodeBuilder = createNodeBuilder();
            undefinedSymbol,

            globalThisSymbol,

            argumentsSymbol,
            requireSymbol,

            apparentArgumentCount: None,

            // const tupleTypes = new Map<string, GenericType>();
            // const unionTypes = new Map<string, UnionType>();
            // const intersectionTypes = new Map<string, Type>();
            // const stringLiteralTypes = new Map<string, StringLiteralType>();
            // const numberLiteralTypes = new Map<number, NumberLiteralType>();
            // const bigIntLiteralTypes = new Map<string, BigIntLiteralType>();
            // const enumLiteralTypes = new Map<string, LiteralType>();
            // const indexedAccessTypes = new Map<string, IndexedAccessType>();
            // const templateLiteralTypes = new Map<string, TemplateLiteralType>();
            // const stringMappingTypes = new Map<string, StringMappingType>();
            // const substitutionTypes = new Map<string, SubstitutionType>();
            // const subtypeReductionCache = new Map<string, Type[]>();
            // const evolvingArrayTypes: EvolvingArrayType[] = [];
            // const undefinedProperties: SymbolTable = new Map();
            unknownSymbol,
            resolvingSymbol,
            // const unresolvedSymbols = new Map<string, TransientSymbol>();
            // const errorTypes = new Map<string, Type>();
            anyType,
            autoType,
            wildcardType,
            errorType,
            unresolvedType,
            nonInferrableAnyType,
            intrinsicMarkerType,
            unknownType,
            nonNullUnknownType,
            undefinedType,
            undefinedWideningType,
            optionalType,
            missingType,
            nullType,
            nullWideningType,
            stringType,
            numberType,
            bigintType,
            falseType,
            regularFalseType,
            trueType,
            regularTrueType,
            // const booleanType = getUnionType([regularFalseType, regularTrueType]);
            esSymbolType,
            voidType,
            neverType,
            silentNeverType,
            nonInferrableType,
            implicitNeverType,
            unreachableNeverType,
            nonPrimitiveType,
            // const stringOrNumberType = getUnionType([stringType, numberType]);
            // const stringNumberSymbolType = getUnionType([stringType, numberType, esSymbolType]);
            // const keyofConstraintType = keyofStringsOnly ? stringType : stringNumberSymbolType;
            // const numberOrBigIntType = getUnionType([numberType, bigintType]);
            // const templateConstraintType = getUnionType([stringType, numberType, booleanType, bigintType, nullType, undefinedType]) as UnionType;

            // const restrictiveMapper: TypeMapper = makeFunctionTypeMapper(t => t.flags & TypeFlags.TypeParameter ? getRestrictiveTypeParameter(t as TypeParameter) : t);
            // const permissiveMapper: TypeMapper = makeFunctionTypeMapper(t => t.flags & TypeFlags.TypeParameter ? wildcardType : t);
            emptyObjectType,
            // const emptyJsxObjectType = createAnonymousType(undefined, emptySymbols, emptyArray, emptyArray, emptyArray);

            // const emptyTypeLiteralSymbol = createSymbol(SymbolFlags::TypeLiteral, InternalSymbolName.Type);
            // const emptyTypeLiteralType = createAnonymousType(emptyTypeLiteralSymbol, emptySymbols, emptyArray, emptyArray, emptyArray);
            emptyGenericType,
            // const anyFunctionType = createAnonymousType(undefined, emptySymbols, emptyArray, emptyArray, emptyArray);

            // const noConstraintType = createAnonymousType(undefined, emptySymbols, emptyArray, emptyArray, emptyArray);
            // const circularConstraintType = createAnonymousType(undefined, emptySymbols, emptyArray, emptyArray, emptyArray);
            // const resolvingDefaultType = createAnonymousType(undefined, emptySymbols, emptyArray, emptyArray, emptyArray);

            // const markerSuperType = createTypeParameter();
            // const markerSubType = createTypeParameter();
            // const markerOtherType = createTypeParameter();

            // const noTypePredicate = createTypePredicate(TypePredicateKind.Identifier, "<<unresolved>>", 0, anyType);

            // const anySignature = createSignature(undefined, undefined, undefined, emptyArray, anyType, /*resolvedTypePredicate*/ undefined, 0, SignatureFlags.None);
            // const unknownSignature = createSignature(undefined, undefined, undefined, emptyArray, errorType, /*resolvedTypePredicate*/ undefined, 0, SignatureFlags.None);
            // const resolvingSignature = createSignature(undefined, undefined, undefined, emptyArray, anyType, /*resolvedTypePredicate*/ undefined, 0, SignatureFlags.None);
            // const silentNeverSignature = createSignature(undefined, undefined, undefined, emptyArray, silentNeverType, /*resolvedTypePredicate*/ undefined, 0, SignatureFlags.None);

            // const enumNumberIndexInfo = createIndexInfo(numberType, stringType, /*isReadonly*/ true);

            // const iterationTypesCache = new Map<string, IterationTypes>(); // cache for common IterationTypes instances
            // const noIterationTypes: IterationTypes = {
            //     get yieldType(): Type { return Debug.fail("Not supported"); },
            //     get returnType(): Type { return Debug.fail("Not supported"); },
            //     get nextType(): Type { return Debug.fail("Not supported"); },
            // };

            // const anyIterationTypes = createIterationTypes(anyType, anyType, anyType);
            // const anyIterationTypesExceptNext = createIterationTypes(anyType, anyType, unknownType);
            // const defaultIterationTypes = createIterationTypes(neverType, anyType, undefinedType); // default iteration types for `Iterator`.

            // const asyncIterationTypesResolver: IterationTypesResolver = {
            //     iterableCacheKey: "iterationTypesOfAsyncIterable",
            //     iteratorCacheKey: "iterationTypesOfAsyncIterator",
            //     iteratorSymbolName: "asyncIterator",
            //     getGlobalIteratorType: getGlobalAsyncIteratorType,
            //     getGlobalIterableType: getGlobalAsyncIterableType,
            //     getGlobalIterableIteratorType: getGlobalAsyncIterableIteratorType,
            //     getGlobalGeneratorType: getGlobalAsyncGeneratorType,
            //     resolveIterationType: getAwaitedType,
            //     mustHaveANextMethodDiagnostic: Diagnostics.An_async_iterator_must_have_a_next_method,
            //     mustBeAMethodDiagnostic: Diagnostics.The_0_property_of_an_async_iterator_must_be_a_method,
            //     mustHaveAValueDiagnostic: Diagnostics.The_type_returned_by_the_0_method_of_an_async_iterator_must_be_a_promise_for_a_type_with_a_value_property,
            // };

            // const syncIterationTypesResolver: IterationTypesResolver = {
            //     iterableCacheKey: "iterationTypesOfIterable",
            //     iteratorCacheKey: "iterationTypesOfIterator",
            //     iteratorSymbolName: "iterator",
            //     getGlobalIteratorType,
            //     getGlobalIterableType,
            //     getGlobalIterableIteratorType,
            //     getGlobalGeneratorType,
            //     resolveIterationType: (type, _errorNode) => type,
            //     mustHaveANextMethodDiagnostic: Diagnostics.An_iterator_must_have_a_next_method,
            //     mustBeAMethodDiagnostic: Diagnostics.The_0_property_of_an_iterator_must_be_a_method,
            //     mustHaveAValueDiagnostic: Diagnostics.The_type_returned_by_the_0_method_of_an_iterator_must_have_a_value_property,
            // };

            // let amalgamatedDuplicates: ESMap<string, DuplicateInfoForFiles> | undefined;
            // const reverseMappedCache = new Map<string, Type | undefined>();
            // let inInferTypeForHomomorphicMappedType = false;
            // let ambientModulesCache: Symbol[] | undefined;

            // let patternAmbientModules: PatternAmbientModule[];
            // let patternAmbientModuleAugmentations: ESMap<string, Symbol> | undefined;
            globalObjectType: None,
            globalFunctionType: None,
            globalCallableFunctionType: None,
            globalNewableFunctionType: None,
            globalArrayType: None,
            globalReadonlyArrayType: None,
            globalStringType: None,
            globalNumberType: None,
            globalBooleanType: None,
            globalRegExpType: None,
            globalThisType: None,
            // let anyArrayType: Type;
            // let autoArrayType: Type;
            // let anyReadonlyArrayType: Type;
            // let deferredGlobalNonNullableTypeAlias: Symbol;

            // let deferredGlobalESSymbolConstructorSymbol: Symbol | undefined;
            // let deferredGlobalESSymbolConstructorTypeSymbol: Symbol | undefined;
            // let deferredGlobalESSymbolType: ObjectType | undefined;
            // let deferredGlobalTypedPropertyDescriptorType: GenericType;
            // let deferredGlobalPromiseType: GenericType | undefined;
            // let deferredGlobalPromiseLikeType: GenericType | undefined;
            // let deferredGlobalPromiseConstructorSymbol: Symbol | undefined;
            // let deferredGlobalPromiseConstructorLikeType: ObjectType | undefined;
            // let deferredGlobalIterableType: GenericType | undefined;
            // let deferredGlobalIteratorType: GenericType | undefined;
            // let deferredGlobalIterableIteratorType: GenericType | undefined;
            // let deferredGlobalGeneratorType: GenericType | undefined;
            // let deferredGlobalIteratorYieldResultType: GenericType | undefined;
            // let deferredGlobalIteratorReturnResultType: GenericType | undefined;
            // let deferredGlobalAsyncIterableType: GenericType | undefined;
            // let deferredGlobalAsyncIteratorType: GenericType | undefined;
            // let deferredGlobalAsyncIterableIteratorType: GenericType | undefined;
            // let deferredGlobalAsyncGeneratorType: GenericType | undefined;
            // let deferredGlobalTemplateStringsArrayType: ObjectType | undefined;
            // let deferredGlobalImportMetaType: ObjectType;
            // let deferredGlobalImportMetaExpressionType: ObjectType;
            // let deferredGlobalImportCallOptionsType: ObjectType | undefined;
            // let deferredGlobalExtractSymbol: Symbol | undefined;
            // let deferredGlobalOmitSymbol: Symbol | undefined;
            // let deferredGlobalAwaitedSymbol: Symbol | undefined;
            // let deferredGlobalBigIntType: ObjectType | undefined;

            // const allPotentiallyUnusedIdentifiers = new Map<Path, PotentiallyUnusedIdentifier[]>(); // key is file name
            flowLoopStart: 0,
            flowLoopCount: 0,
            sharedFlowCount: 0,
            flowAnalysisDisabled: false,
            flowInvocationCount: 0,
            lastFlowNode: None,
            lastFlowNodeReachable: false,
            // let flowTypeCache: Type[] | undefined;

            // const emptyStringType = getStringLiteralType("");
            // const zeroType = getNumberLiteralType(0);
            // const zeroBigIntType = getBigIntLiteralType({ negative: false, base10Value: "0" });

            // const resolutionTargets: TypeSystemEntity[] = [];
            // const resolutionResults: boolean[] = [];
            // const resolutionPropertyNames: TypeSystemPropertyName[] = [];

            // let suggestionCount = 0;
            // const maximumSuggestionCount = 10;
            mergedSymbols: IndexVec::default(),
            // const symbolLinks: SymbolLinks[] = [];
            // const nodeLinks: NodeLinks[] = [];
            // const flowLoopCaches: ESMap<string, Type>[] = [];
            // const flowLoopNodes: FlowNode[] = [];
            // const flowLoopKeys: string[] = [];
            // const flowLoopTypes: Type[][] = [];
            // const sharedFlowNodes: FlowNode[] = [];
            // const sharedFlowTypes: FlowType[] = [];
            // const flowNodeReachable: (boolean | undefined)[] = [];
            // const flowNodePostSuper: (boolean | undefined)[] = [];
            // const potentialThisCollisions: Node[] = [];
            // const potentialNewTargetCollisions: Node[] = [];
            // const potentialWeakMapSetCollisions: Node[] = [];
            // const potentialReflectCollisions: Node[] = [];
            // const awaitedTypeStack: number[] = [];

            // const diagnostics = createDiagnosticCollection();
            // const suggestionDiagnostics = createDiagnosticCollection();

            // const typeofTypesByName: ReadonlyESMap<string, Type> = new Map(getEntries({
            //     string: stringType,
            //     number: numberType,
            //     bigint: bigintType,
            //     boolean: booleanType,
            //     symbol: esSymbolType,
            //     undefined: undefinedType
            // }));
            // const typeofType = createTypeofType();

            // let _jsxNamespace: __String;
            // let _jsxFactoryEntity: EntityName | undefined;
            // let outofbandVarianceMarkerHandler: ((onlyUnreliable: boolean) => void) | undefined;

            // const subtypeRelation = new Map<string, RelationComparisonResult>();
            // const strictSubtypeRelation = new Map<string, RelationComparisonResult>();
            // const assignableRelation = new Map<string, RelationComparisonResult>();
            // const comparableRelation = new Map<string, RelationComparisonResult>();
            // const identityRelation = new Map<string, RelationComparisonResult>();
            // const enumRelation = new Map<string, RelationComparisonResult>();
            builtinGlobals,

            node_data: bind_res.node_data,
            flow_nodes: bind_res.flow_nodes,
            symbols: bind_res.symbols,

            types,
            host,
            signatures: Default::default(),
        };

        checker.initializeTypeChecker();

        checker
    }

    // getNodeCount: () => sum(host.getSourceFiles(), "nodeCount"),
    // getIdentifierCount: () => sum(host.getSourceFiles(), "identifierCount"),
    // getSymbolCount: () => sum(host.getSourceFiles(), "symbolCount") + symbolCount,
    // getTypeCount: () => typeCount,
    // getInstantiationCount: () => totalInstantiationCount,
    // getRelationCacheSizes: () => ({
    //     assignable: assignableRelation.size,
    //     identity: identityRelation.size,
    //     subtype: subtypeRelation.size,
    //     strictSubtype: strictSubtypeRelation.size,
    // }),
    // isUndefinedSymbol: symbol => symbol === undefinedSymbol,
    // isArgumentsSymbol: symbol => symbol === argumentsSymbol,
    // isUnknownSymbol: symbol => symbol === unknownSymbol,
    // getMergedSymbol,
    // getDiagnostics,
    // getGlobalDiagnostics,
    // getRecursionIdentity,
    // getUnmatchedProperties,
    // getTypeOfSymbolAtLocation: (symbol, locationIn) => {
    //     let location = getParseTreeNode(locationIn);
    //     return location ? getTypeOfSymbolAtLocation(symbol, location) : errorType;
    // },
    // getSymbolsOfParameterPropertyDeclaration: (parameterIn, parameterName) => {
    //     let parameter = getParseTreeNode(parameterIn, isParameter);
    //     if (parameter === undefined) return Debug.fail("Cannot get symbols of a synthetic parameter that cannot be resolved to a parse-tree node.");
    //     return getSymbolsOfParameterPropertyDeclaration(parameter, escapeLeadingUnderscores(parameterName));
    // },
    // getDeclaredTypeOfSymbol,
    // getPropertiesOfType,
    // getPropertyOfType: (type, name) => getPropertyOfType(type, escapeLeadingUnderscores(name)),
    // getPrivateIdentifierPropertyOfType: (leftType: Type, name: string, location: Node) => {
    //     let node = getParseTreeNode(location);
    //     if (!node) {
    //         return undefined;
    //     }
    //     let propName = escapeLeadingUnderscores(name);
    //     let lexicallyScopedIdentifier = lookupSymbolForPrivateIdentifierDeclaration(propName, node);
    //     return lexicallyScopedIdentifier ? getPrivateIdentifierPropertyOfType(leftType, lexicallyScopedIdentifier) : undefined;
    // },
    // getTypeOfPropertyOfType: (type, name) => getTypeOfPropertyOfType(type, escapeLeadingUnderscores(name)),
    // getIndexInfoOfType: (type, kind) => getIndexInfoOfType(type, kind === IndexKind.String ? stringType : numberType),
    // getIndexInfosOfType,
    // getSignaturesOfType,
    // getIndexTypeOfType: (type, kind) => getIndexTypeOfType(type, kind === IndexKind.String ? stringType : numberType),
    // getBaseTypes,
    // getBaseTypeOfLiteralType,
    // getWidenedType,
    // getTypeFromTypeNode: nodeIn => {
    //     let node = getParseTreeNode(nodeIn, isTypeNode);
    //     return node ? getTypeFromTypeNode(node) : errorType;
    // },
    // getParameterType: getTypeAtPosition,
    // getParameterIdentifierNameAtPosition,
    // getPromisedTypeOfPromise,
    // getAwaitedType: type => getAwaitedType(type),
    // getReturnTypeOfSignature,
    // isNullableType,
    // getNullableType,
    // getNonNullableType,
    // getNonOptionalType: removeOptionalTypeMarker,
    // getTypeArguments,
    // typeToTypeNode: nodeBuilder.typeToTypeNode,
    // indexInfoToIndexSignatureDeclaration: nodeBuilder.indexInfoToIndexSignatureDeclaration,
    // signatureToSignatureDeclaration: nodeBuilder.signatureToSignatureDeclaration,
    // symbolToEntityName: nodeBuilder.symbolToEntityName,
    // symbolToExpression: nodeBuilder.symbolToExpression,
    // symbolToTypeParameterDeclarations: nodeBuilder.symbolToTypeParameterDeclarations,
    // symbolToParameterDeclaration: nodeBuilder.symbolToParameterDeclaration,
    // typeParameterToDeclaration: nodeBuilder.typeParameterToDeclaration,
    // getSymbolsInScope: (locationIn, meaning) => {
    //     let location = getParseTreeNode(locationIn);
    //     return location ? getSymbolsInScope(location, meaning) : [];
    // },
    // getSymbolAtLocation: nodeIn => {
    //     let node = getParseTreeNode(nodeIn);
    //     // set ignoreErrors: true because any lookups invoked by the API shouldn't cause any new errors
    //     return node ? getSymbolAtLocation(node, /*ignoreErrors*/ true) : undefined;
    // },
    // getIndexInfosAtLocation: nodeIn => {
    //     let node = getParseTreeNode(nodeIn);
    //     return node ? getIndexInfosAtLocation(node) : undefined;
    // },
    // getShorthandAssignmentValueSymbol: nodeIn => {
    //     let node = getParseTreeNode(nodeIn);
    //     return node ? getShorthandAssignmentValueSymbol(node) : undefined;
    // },
    // getExportSpecifierLocalTargetSymbol: nodeIn => {
    //     let node = getParseTreeNode(nodeIn, isExportSpecifier);
    //     return node ? getExportSpecifierLocalTargetSymbol(node) : undefined;
    // },
    // getExportSymbolOfSymbol(symbol) {
    //     return getMergedSymbol(symbol.exportSymbol || symbol);
    // },
    pub fn getTypeAtLocation(&mut self, nodeIn: BoundNode) -> TypeId {
        // let node = getParseTreeNode(nodeIn);
        // return node ? getTypeOfNode(node) : errorType;
        self.getTypeOfNode(nodeIn)
    }
    // getTypeOfAssignmentPattern: nodeIn => {
    //     let node = getParseTreeNode(nodeIn, isAssignmentPattern);
    //     return node && getTypeOfAssignmentPattern(node) || errorType;
    // },
    // getPropertySymbolOfDestructuringAssignment: locationIn => {
    //     let location = getParseTreeNode(locationIn, isIdentifier);
    //     return location ? getPropertySymbolOfDestructuringAssignment(location) : undefined;
    // },
    // signatureToString: (signature, enclosingDeclaration, flags, kind) => {
    //     return signatureToString(signature, getParseTreeNode(enclosingDeclaration), flags, kind);
    // },
    // typeToString: (type, enclosingDeclaration, flags) => {
    //     return typeToString(type, getParseTreeNode(enclosingDeclaration), flags);
    // },
    // symbolToString: (symbol, enclosingDeclaration, meaning, flags) => {
    //     return symbolToString(symbol, getParseTreeNode(enclosingDeclaration), meaning, flags);
    // },
    // typePredicateToString: (predicate, enclosingDeclaration, flags) => {
    //     return typePredicateToString(predicate, getParseTreeNode(enclosingDeclaration), flags);
    // },
    // writeSignature: (signature, enclosingDeclaration, flags, kind, writer) => {
    //     return signatureToString(signature, getParseTreeNode(enclosingDeclaration), flags, kind, writer);
    // },
    // writeType: (type, enclosingDeclaration, flags, writer) => {
    //     return typeToString(type, getParseTreeNode(enclosingDeclaration), flags, writer);
    // },
    // writeSymbol: (symbol, enclosingDeclaration, meaning, flags, writer) => {
    //     return symbolToString(symbol, getParseTreeNode(enclosingDeclaration), meaning, flags, writer);
    // },
    // writeTypePredicate: (predicate, enclosingDeclaration, flags, writer) => {
    //     return typePredicateToString(predicate, getParseTreeNode(enclosingDeclaration), flags, writer);
    // },
    // getAugmentedPropertiesOfType,
    // getRootSymbols,
    // getSymbolOfExpando,
    // getContextualType: (nodeIn: Expression, contextFlags?: ContextFlags) => {
    //     let node = getParseTreeNode(nodeIn, isExpression);
    //     if (!node) {
    //         return undefined;
    //     }
    //     let containingCall = findAncestor(node, isCallLikeExpression);
    //     let containingCallResolvedSignature = containingCall && getNodeLinks(containingCall).resolvedSignature;
    //     if (contextFlags! & ContextFlags.Completions && containingCall) {
    //         let toMarkSkip = node as Node;
    //         do {
    //             getNodeLinks(toMarkSkip).skipDirectInference = true;
    //             toMarkSkip = toMarkSkip.parent;
    //         } while (toMarkSkip && toMarkSkip !== containingCall);
    //         getNodeLinks(containingCall).resolvedSignature = undefined;
    //     }
    //     let result = getContextualType(node, contextFlags);
    //     if (contextFlags! & ContextFlags.Completions && containingCall) {
    //         let toMarkSkip = node as Node;
    //         do {
    //             getNodeLinks(toMarkSkip).skipDirectInference = undefined;
    //             toMarkSkip = toMarkSkip.parent;
    //         } while (toMarkSkip && toMarkSkip !== containingCall);
    //         getNodeLinks(containingCall).resolvedSignature = containingCallResolvedSignature;
    //     }
    //     return result;
    // },
    // getContextualTypeForObjectLiteralElement: nodeIn => {
    //     let node = getParseTreeNode(nodeIn, isObjectLiteralElementLike);
    //     return node ? getContextualTypeForObjectLiteralElement(node) : undefined;
    // },
    // getContextualTypeForArgumentAtIndex: (nodeIn, argIndex) => {
    //     let node = getParseTreeNode(nodeIn, isCallLikeExpression);
    //     return node && getContextualTypeForArgumentAtIndex(node, argIndex);
    // },
    // getContextualTypeForJsxAttribute: (nodeIn) => {
    //     let node = getParseTreeNode(nodeIn, isJsxAttributeLike);
    //     return node && getContextualTypeForJsxAttribute(node);
    // },
    // isContextSensitive,
    // getTypeOfPropertyOfContextualType,
    // getFullyQualifiedName,
    // getResolvedSignature: (node, candidatesOutArray, argumentCount) =>
    //     getResolvedSignatureWorker(node, candidatesOutArray, argumentCount, CheckMode.Normal),
    // getResolvedSignatureForSignatureHelp: (node, candidatesOutArray, argumentCount) =>
    //     getResolvedSignatureWorker(node, candidatesOutArray, argumentCount, CheckMode.IsForSignatureHelp),
    // getExpandedParameters,
    // hasEffectiveRestParameter,
    // containsArgumentsReference,
    // getConstantValue: nodeIn => {
    //     let node = getParseTreeNode(nodeIn, canHaveConstantValue);
    //     return node ? getConstantValue(node) : undefined;
    // },
    // isValidPropertyAccess: (nodeIn, propertyName) => {
    //     let node = getParseTreeNode(nodeIn, isPropertyAccessOrQualifiedNameOrImportTypeNode);
    //     return !!node && isValidPropertyAccess(node, escapeLeadingUnderscores(propertyName));
    // },
    // isValidPropertyAccessForCompletions: (nodeIn, type, property) => {
    //     let node = getParseTreeNode(nodeIn, isPropertyAccessExpression);
    //     return !!node && isValidPropertyAccessForCompletions(node, type, property);
    // },
    // getSignatureFromDeclaration: declarationIn => {
    //     let declaration = getParseTreeNode(declarationIn, isFunctionLike);
    //     return declaration ? getSignatureFromDeclaration(declaration) : undefined;
    // },
    // isImplementationOfOverload: nodeIn => {
    //     let node = getParseTreeNode(nodeIn, isFunctionLike);
    //     return node ? isImplementationOfOverload(node) : undefined;
    // },
    // getImmediateAliasedSymbol,
    // getAliasedSymbol: resolveAlias,
    // getEmitResolver,
    // getExportsOfModule: getExportsOfModuleAsArray,
    // getExportsAndPropertiesOfModule,
    // forEachExportAndPropertyOfModule,
    // getSymbolWalker: createGetSymbolWalker(
    //     getRestTypeOfSignature,
    //     getTypePredicateOfSignature,
    //     getReturnTypeOfSignature,
    //     getBaseTypes,
    //     resolveStructuredTypeMembers,
    //     getTypeOfSymbol,
    //     getResolvedSymbol,
    //     getConstraintOfTypeParameter,
    //     getFirstIdentifier,
    //     getTypeArguments,
    // ),
    // getAmbientModules,
    // getJsxIntrinsicTagNamesAt,
    // isOptionalParameter: nodeIn => {
    //     let node = getParseTreeNode(nodeIn, isParameter);
    //     return node ? isOptionalParameter(node) : false;
    // },
    // tryGetMemberInModuleExports: (name, symbol) => tryGetMemberInModuleExports(escapeLeadingUnderscores(name), symbol),
    // tryGetMemberInModuleExportsAndProperties: (name, symbol) => tryGetMemberInModuleExportsAndProperties(escapeLeadingUnderscores(name), symbol),
    // tryFindAmbientModule: moduleName => tryFindAmbientModule(moduleName, /*withAugmentations*/ true),
    // tryFindAmbientModuleWithoutAugmentations: moduleName => {
    //     // we deliberately exclude augmentations
    //     // since we are only interested in declarations of the module itself
    //     return tryFindAmbientModule(moduleName, /*withAugmentations*/ false);
    // },
    // getApparentType,
    // getUnionType,
    // isTypeAssignableTo,
    // createAnonymousType,
    // createSignature,
    // createSymbol,
    // createIndexInfo,
    // getAnyType: () => anyType,
    // getStringType: () => stringType,
    // getNumberType: () => numberType,
    // createPromiseType,
    // createArrayType,
    // getElementTypeOfArrayType,
    // getBooleanType: () => booleanType,
    // getFalseType: (fresh?) => fresh ? falseType : regularFalseType,
    // getTrueType: (fresh?) => fresh ? trueType : regularTrueType,
    // getVoidType: () => voidType,
    // getUndefinedType: () => undefinedType,
    // getNullType: () => nullType,
    // getESSymbolType: () => esSymbolType,
    // getNeverType: () => neverType,
    // getOptionalType: () => optionalType,
    // getPromiseType: () => getGlobalPromiseType(/*reportErrors*/ false),
    // getPromiseLikeType: () => getGlobalPromiseLikeType(/*reportErrors*/ false),
    // isSymbolAccessible,
    // isArrayType,
    // isTupleType,
    // isArrayLikeType,
    // isTypeInvalidDueToUnionDiscriminant,
    // getExactOptionalProperties,
    // getAllPossiblePropertiesOfTypes,
    // getSuggestedSymbolForNonexistentProperty,
    // getSuggestionForNonexistentProperty,
    // getSuggestedSymbolForNonexistentJSXAttribute,
    // getSuggestedSymbolForNonexistentSymbol: (location, name, meaning) => getSuggestedSymbolForNonexistentSymbol(location, escapeLeadingUnderscores(name), meaning),
    // getSuggestionForNonexistentSymbol: (location, name, meaning) => getSuggestionForNonexistentSymbol(location, escapeLeadingUnderscores(name), meaning),
    // getSuggestedSymbolForNonexistentModule,
    // getSuggestionForNonexistentExport,
    // getSuggestedSymbolForNonexistentClassMember,
    // getBaseConstraintOfType,
    // getDefaultFromTypeParameter: type => type && type.flags & TypeFlags.TypeParameter ? getDefaultFromTypeParameter(type as TypeParameter) : undefined,
    // resolveName(name, location, meaning, excludeGlobals) {
    //     return resolveName(location, escapeLeadingUnderscores(name), meaning, /*nameNotFoundMessage*/ undefined, /*nameArg*/ undefined, /*isUse*/ false, excludeGlobals);
    // },
    // getJsxNamespace: n => unescapeLeadingUnderscores(getJsxNamespace(n)),
    // getJsxFragmentFactory: n => {
    //     let jsxFragmentFactory = getJsxFragmentFactoryEntity(n);
    //     return jsxFragmentFactory && unescapeLeadingUnderscores(getFirstIdentifier(jsxFragmentFactory).escapedText);
    // },
    // getAccessibleSymbolChain,
    // getTypePredicateOfSignature,
    // resolveExternalModuleName: moduleSpecifierIn => {
    //     let moduleSpecifier = getParseTreeNode(moduleSpecifierIn, isExpression);
    //     return moduleSpecifier && resolveExternalModuleName(moduleSpecifier, moduleSpecifier, /*ignoreErrors*/ true);
    // },
    // resolveExternalModuleSymbol,
    // tryGetThisTypeAt: (nodeIn, includeGlobalThis) => {
    //     let node = getParseTreeNode(nodeIn);
    //     return node && tryGetThisTypeAt(node, includeGlobalThis);
    // },
    // getTypeArgumentConstraint: nodeIn => {
    //     let node = getParseTreeNode(nodeIn, isTypeNode);
    //     return node && getTypeArgumentConstraint(node);
    // },
    // getSuggestionDiagnostics: (fileIn, ct) => {
    //     let file = getParseTreeNode(fileIn, isSourceFile) || Debug.fail("Could not determine parsed source file.");
    //     if (skipTypeChecking(file, compilerOptions, host)) {
    //         return emptyArray;
    //     }

    //     let diagnostics: DiagnosticWithLocation[] | undefined;
    //     try {
    //         // Record the cancellation token so it can be checked later on during checkSourceElement.
    //         // Do this in a finally block so we can ensure that it gets reset back to nothing after
    //         // this call is done.
    //         cancellationToken = ct;

    //         // Ensure file is type checked
    //         checkSourceFile(file);
    //         Debug.assert(!!(getNodeLinks(file).flags & NodeCheckFlags.TypeChecked));

    //         diagnostics = addRange(diagnostics, suggestionDiagnostics.getDiagnostics(file.fileName));
    //         checkUnusedIdentifiers(getPotentiallyUnusedIdentifiers(file), (containingNode, kind, diag) => {
    //             if (!containsParseError(containingNode) && !unusedIsError(kind, !!(containingNode.flags & NodeFlags.Ambient))) {
    //                 (diagnostics || (diagnostics = [])).push({ ...diag, category: DiagnosticCategory.Suggestion });
    //             }
    //         });

    //         return diagnostics || emptyArray;
    //     }
    //     finally {
    //         cancellationToken = undefined;
    //     }
    // },

    // runWithCancellationToken: (token, callback) => {
    //     try {
    //         cancellationToken = token;
    //         return callback(checker);
    //     }
    //     finally {
    //         cancellationToken = undefined;
    //     }
    // },

    // getLocalTypeParametersOfClassOrInterfaceOrTypeAlias,
    // isDeclarationVisible,
    // isPropertyAccessible,
    // getTypeOnlyAliasDeclaration,
}

impl Checker {
    fn globals(&self) -> &SymbolTable {
        &self.symbols[self.globalThisSymbol].exports()
    }

    fn globals_mut(&mut self) -> &mut SymbolTable {
        self.symbols[self.globalThisSymbol].exports_mut()
    }

    fn get_transient_sym(&self, id: SymbolId) -> &TransientSymbol {
        self.symbols[id].as_transient_symbol()
    }

    fn get_transient_sym_mut(&mut self, id: SymbolId) -> &mut TransientSymbol {
        self.symbols[id].as_transient_symbol_mut()
    }

    fn node_data(&mut self, node: BoundNode) -> &NodeData {
        self.node_data.entry(node).or_default()
    }

    fn node_data_mut(&mut self, node: BoundNode) -> &mut NodeData {
        self.node_data.entry(node).or_default()
    }

    fn globalObjectType(&self) -> TypeId {
        self.globalObjectType.unwrap()
    }
    fn globalFunctionType(&self) -> TypeId {
        self.globalFunctionType.unwrap()
    }
    fn globalCallableFunctionType(&self) -> TypeId {
        self.globalCallableFunctionType.unwrap()
    }
    fn globalNewableFunctionType(&self) -> TypeId {
        self.globalNewableFunctionType.unwrap()
    }
    fn globalArrayType(&self) -> TypeId {
        self.globalArrayType.unwrap()
    }
    fn globalReadonlyArrayType(&self) -> TypeId {
        self.globalReadonlyArrayType.unwrap()
    }
    fn globalStringType(&self) -> TypeId {
        self.globalStringType.unwrap()
    }
    fn globalNumberType(&self) -> TypeId {
        self.globalNumberType.unwrap()
    }
    fn globalBooleanType(&self) -> TypeId {
        self.globalBooleanType.unwrap()
    }
    fn globalRegExpType(&self) -> TypeId {
        self.globalRegExpType.unwrap()
    }
    fn globalThisType(&self) -> TypeId {
        self.globalThisType.unwrap()
    }
    // TODO: accessors:
    // let anyArrayType: Type;
    // let autoArrayType: Type;
    // let anyReadonlyArrayType: Type;

    // TODO:
    // getResolvedSignatureWorker
    // TODO:
    // getJsxNamespace
    // TODO:
    // getLocalJsxNamespace
    // TODO:
    // markAsSynthetic
    // TODO:
    // getEmitResolver
    // TODO:
    // lookupOrIssueError
    // TODO:
    // errorSkippedOn
    // TODO:
    // createError
    // TODO:
    // error
    // TODO:
    // addErrorOrSuggestion
    // TODO:
    // errorOrSuggestion
    // TODO:
    // errorAndMaybeSuggestAwait
    // TODO:
    // addDeprecatedSuggestionWorker
    // TODO:
    // addDeprecatedSuggestion
    // TODO:
    // addDeprecatedSuggestionWithSignature
    // TODO:
    // createSymbol
    // TODO:
    // getExcludedSymbolFlags
    // TODO:
    // recordMergedSymbol
    // TODO:
    // cloneSymbol
    // TODO:
    // mergeSymbol

    fn recordMergedSymbol(&mut self, target: SymbolId, source: SymbolId) {
        todo!();
        // if (!source.mergeId) {
        //     source.mergeId = nextMergeId;
        //     nextMergeId++;
        // }
        // mergedSymbols[source.mergeId] = target;
    }

    fn cloneSymbol(&mut self, symbol: SymbolId) -> SymbolId {
        todo!();
        // const result = createSymbol(symbol.flags, symbol.escapedName);
        // result.declarations = symbol.declarations ? symbol.declarations.slice() : [];
        // result.parent = symbol.parent;
        // if (symbol.valueDeclaration) result.valueDeclaration = symbol.valueDeclaration;
        // if (symbol.constEnumOnlyModule) result.constEnumOnlyModule = true;
        // if (symbol.members) result.members = new Map(symbol.members);
        // if (symbol.exports) result.exports = new Map(symbol.exports);
        // recordMergedSymbol(result, symbol);
        // return result;
    }

    /**
     * Note: if target is transient, then it is mutable, and mergeSymbol with both mutate and return it.
     * If target is not transient, mergeSymbol will produce a transient clone, mutate that and return it.
     */
    fn mergeSymbol(
        &mut self,
        mut target: SymbolId,
        source: SymbolId,
        unidirectional: bool,
    ) -> SymbolId {
        if !(self.symbols[target]
            .flags()
            .intersects(getExcludedSymbolFlags(*self.symbols[source].flags())))
            || (*self.symbols[source].flags() | *self.symbols[target].flags())
                .intersects(SymbolFlags::Assignment)
        {
            if source == target {
                // This can happen when an export assigned namespace exports something also erroneously exported at the top level
                // See `declarationFileNoCrashOnExtraExportModifier` for an example
                return target;
            }
            if !(self.symbols[target]
                .flags()
                .intersects(SymbolFlags::Transient))
            {
                let resolvedTarget = self.resolveSymbol(target, false);
                if resolvedTarget == self.unknownSymbol {
                    return source;
                }
                target = self.cloneSymbol(resolvedTarget);
            }
            // Javascript static-property-assignment declarations always merge, even though they are also values
            if self.symbols[source]
                .flags()
                .intersects(SymbolFlags::ValueModule)
                && self.symbols[target]
                    .flags()
                    .intersects(SymbolFlags::ValueModule)
                && self.symbols[target].constEnumOnlyModule()
                && !self.symbols[source].constEnumOnlyModule()
            {
                // reset flag when merging instantiated module into value module that has only const enums
                *self.symbols[target].constEnumOnlyModule_mut() = false;
            }
            {
                let (source_sym, target_sym) = self.symbols.pick2_mut(source, target);

                let existing = *source_sym.flags();
                *target_sym.flags_mut() |= existing;

                if let Some(valueDeclaration) = source_sym.valueDeclaration().clone() {
                    setValueDeclaration(target_sym, valueDeclaration);
                }

                target_sym
                    .declarations_mut()
                    .extend(source_sym.declarations().iter().cloned());
            }
            if !self.symbols[source].members().is_empty() {
                // TODO: all of this moving is a bit janky
                let (source_sym, target_sym) = self.symbols.pick2_mut(source, target);
                let mut target_table = mem::take(target_sym.members_mut());
                let source_table = mem::take(source_sym.members_mut());
                self.mergeSymbolTable(&mut target_table, &source_table, unidirectional);
                *self.symbols[target].members_mut() = target_table;
                *self.symbols[source].members_mut() = source_table;
            }
            if !self.symbols[source].exports().is_empty() {
                // TODO: all of this moving is a bit janky
                let (source_sym, target_sym) = self.symbols.pick2_mut(source, target);
                let mut target_table = mem::take(target_sym.exports_mut());
                let source_table = mem::take(source_sym.exports_mut());
                self.mergeSymbolTable(&mut target_table, &source_table, unidirectional);
                *self.symbols[target].exports_mut() = target_table;
                *self.symbols[source].exports_mut() = source_table;
            }
            if !unidirectional {
                self.recordMergedSymbol(target, source);
            }
        } else if self.symbols[target]
            .flags()
            .intersects(SymbolFlags::NamespaceModule)
        {
            // Do not report an error when merging `var globalThis` with the built-in `globalThis`,
            // as we will already report a "Declaration name conflicts..." error, and this error
            // won't make much sense.
            if target != self.globalThisSymbol {
                todo!();
                // error(
                //             self.symbols[source].declarations && getNameOfDeclaration(self.symbols[source].declarations[0]),
                //             Diagnostics.Cannot_augment_module_0_with_value_exports_because_it_resolves_to_a_non_module_entity,
                //             symbolToString(target));
            }
        } else {
            todo!();
            // error
            // let isEitherEnum = !!(target.flags & SymbolFlags::Enum || source.flags & SymbolFlags::Enum);
            // let isEitherBlockScoped = !!(target.flags & SymbolFlags::BlockScopedVariable || source.flags & SymbolFlags::BlockScopedVariable);
            // let message = isEitherEnum
            //     ? Diagnostics.Enum_declarations_can_only_merge_with_namespace_or_other_enum_declarations
            //     : isEitherBlockScoped
            //         ? Diagnostics.Cannot_redeclare_block_scoped_variable_0
            //         : Diagnostics.Duplicate_identifier_0;
            // let sourceSymbolFile = source.declarations && getSourceFileOfNode(source.declarations[0]);
            // let targetSymbolFile = target.declarations && getSourceFileOfNode(target.declarations[0]);
            // let symbolName = symbolToString(source);

            // // Collect top-level duplicate identifier errors into one mapping, so we can then merge their diagnostics if there are a bunch
            // if (sourceSymbolFile && targetSymbolFile && amalgamatedDuplicates && !isEitherEnum && sourceSymbolFile !== targetSymbolFile) {
            //     const firstFile = comparePaths(sourceSymbolFile.path, targetSymbolFile.path) === Comparison.LessThan ? sourceSymbolFile : targetSymbolFile;
            //     const secondFile = firstFile === sourceSymbolFile ? targetSymbolFile : sourceSymbolFile;
            //     const filesDuplicates = getOrUpdate(amalgamatedDuplicates, `${firstFile.path}|${secondFile.path}`, () =>
            //         ({ firstFile, secondFile, conflictingSymbols: new Map() } as DuplicateInfoForFiles));
            //     const conflictingSymbolInfo = getOrUpdate(filesDuplicates.conflictingSymbols, symbolName, () =>
            //         ({ isBlockScoped: isEitherBlockScoped, firstFileLocations: [], secondFileLocations: [] } as DuplicateInfoForSymbol));
            //     addDuplicateLocations(conflictingSymbolInfo.firstFileLocations, source);
            //     addDuplicateLocations(conflictingSymbolInfo.secondFileLocations, target);
            // }
            // else {
            //     addDuplicateDeclarationErrorsForSymbols(source, message, symbolName, target);
            //     addDuplicateDeclarationErrorsForSymbols(target, message, symbolName, source);
            // }
        }
        target

        // fn addDuplicateLocations(locs: Declaration[], symbol: Symbol): void {
        //     if (symbol.declarations) {
        //         for (const decl of symbol.declarations) {
        //             pushIfUnique(locs, decl);
        //         }
        //     }
        // }
    }

    // TODO:
    // addDuplicateDeclarationErrorsForSymbols
    // TODO:
    // addDuplicateDeclarationError
    // TODO:
    // combineSymbolTables

    fn mergeSymbolTable(
        &mut self,
        target: &mut SymbolTable,
        source: &SymbolTable,
        unidirectional: bool,
    ) {
        for (id, sourceSymbol) in source.iter() {
            match target.entry(id.clone()) {
                Entry::Occupied(mut existing) => {
                    existing.insert(self.mergeSymbol(
                        *existing.get(),
                        *sourceSymbol,
                        unidirectional,
                    ));
                }
                Entry::Vacant(slot) => {
                    slot.insert(*sourceSymbol);
                }
            }
        }
    }

    // TODO:
    // mergeModuleAugmentation

    // TODO:
    // fn getSymbolLinks(&self, symbol: SymbolId) -> SymbolLinksId {
    //     if (symbol.flags & SymbolFlags::Transient) {return symbol as TransientSymbol};
    //     const id = getSymbolId(symbol);
    //     return symbolLinks[id] || (symbolLinks[id] = new (SymbolLinks as any)());
    // }

    // TODO:
    // getNodeLinks
    // TODO:
    // isGlobalSourceFile

    fn getSymbol(
        &mut self,
        symbols: &SymbolTable,
        name: JsWord,
        meaning: SymbolFlags,
    ) -> Option<SymbolId> {
        todo!();
        // if let Some(meaning) = meaning {
        //     let symbol_id = symbols.get(&name).map(|id|self.getMergedSymbol(*id));
        //     if let Some(symbol_id) = symbol_id {
        //         let symbol = &self.symbols[symbol_id];
        //         debug_assert!(
        //             !getCheckFlags(symbol).intersects(CheckFlags::Instantiated),
        //             "Should never get an instantiated symbol here."
        //         );
        //         if symbol.flags().intersects(meaning) {
        //             return Some(symbol_id);
        //         }
        //         if symbol.flags().intersects(SymbolFlags::Alias) {
        //             let target = self.resolveAlias(symbol);
        //             // Unknown symbol means an error occurred in alias resolution, treat it as positive answer to avoid cascading errors
        //             if target == unknownSymbol || target.flags.intersects(meaning) {
        //                 return Some(symbol_id);
        //             }
        //         }
        //     }
        // }
        // // return None if we can't find a symbol.
        // None
    }

    // TODO:
    // getSymbolsOfParameterPropertyDeclaration
    // TODO:
    // isBlockScopedNameDeclaredBeforeUse
    // TODO:
    // useOuterVariableScopeInParameter

    /**
     * Resolve a given name for a given meaning at a given location. An error is reported if the name was not found and
     * the nameNotFoundMessage argument is not undefined. Returns the resolved symbol, or undefined if no symbol with
     * the given name can be found.
     *
     * @param isUse If true, this will count towards --noUnusedLocals / --noUnusedParameters.
     */
    fn resolveName(
        &mut self,
        location: Option<BoundNode>,
        name: JsWord,
        meaning: SymbolFlags,
        // nameNotFoundMessage: DiagnosticMessage | undefined,
        nameArg: Option<JsWord>,
        isUse: bool,
        excludeGlobals: bool,
    ) -> Option<SymbolId> {
        self.resolveNameHelper(
            location,
            name,
            meaning,
            // nameNotFoundMessage,
            // nameArg,
            isUse,
            excludeGlobals,
        )
    }

    fn resolveNameHelper(
        &mut self,
        mut location: Option<BoundNode>,
        name: JsWord,
        meaning: SymbolFlags,
        // nameNotFoundMessage: DiagnosticMessage | undefined,
        // nameArg: __String | Identifier | undefined,
        isUse: bool,
        excludeGlobals: bool,
    ) -> Option<SymbolId> {
        // let originalLocation = location.clone(); // needed for did-you-mean error reporting, which gathers candidates starting from the original location
        let mut result = None;
        // let mut lastLocation: Node | undefined;
        // let mut lastSelfReferenceLocation: Node | undefined;
        // let mut propertyWithInvalidInitializer: Node | undefined;
        let mut lastLocation = None;
        let mut lastSelfReferenceLocation = None;
        let mut propertyWithInvalidInitializer = None;
        // let mut associatedDeclarationForContainingInitializerOrBindingName: ParameterDeclaration | BindingElement | undefined;
        let mut associatedDeclarationForContainingInitializerOrBindingName = None;
        let mut withinDeferredContext = false;
        // let errorLocation = location;
        let mut isInExternalModule = false;

        // TODO: fix all breaks
        '_loop: while let Some(loc) = location.clone() {
            // TODO: commonjs
            // if !self.node_data(loc).locals.is_empty() && !isGlobalSourceFile(loc) {
            // Locals of a source file are not in scope (because they get merged into the global symbol table)
            if matches!(loc, BoundNode::Script(_)) && !self.node_data(loc.clone()).locals.is_empty()
            {
                todo!();
                // if (result = self.getSymbol(loc.locals, name, meaning)) {
                //     let useResult = true;
                //     if (isFunctionLike(loc) && lastLocation && lastLocation !== (loc as FunctionLikeDeclaration).body) {
                //         // symbol lookup restrictions for function-like declarations
                //         // - Type parameters of a function are in scope in the entire function declaration, including the parameter
                //         //   list and return type. However, local types are only in scope in the function body.
                //         // - parameters are only in the scope of function body
                //         // This restriction does not apply to JSDoc comment types because they are parented
                //         // at a higher level than type parameters would normally be
                //         if (meaning & result.flags & SymbolFlags::Type && lastLocation.kind !== SyntaxKind.JSDocComment) {
                //             useResult = result.flags & SymbolFlags::TypeParameter
                //                 // type parameters are visible in parameter list, return type and type parameter list
                //                 ? lastLocation === (loc as FunctionLikeDeclaration).type ||
                //                 lastLocation.kind === SyntaxKind.Parameter ||
                //                 lastLocation.kind === SyntaxKind.TypeParameter
                //                 // local types not visible outside the function body
                //                 : false;
                //         }
                //         if (meaning & result.flags & SymbolFlags::Variable) {
                //             // expression inside parameter will lookup as normal variable scope when targeting es2015+
                //             if (useOuterVariableScopeInParameter(result, loc, lastLocation)) {
                //                 useResult = false;
                //             }
                //             else if (result.flags & SymbolFlags::FunctionScopedVariable) {
                //                 // parameters are visible only inside function body, parameter list and return type
                //                 // technically for parameter list case here we might mix parameters and variables declared in function,
                //                 // however it is detected separately when checking initializers of parameters
                //                 // to make sure that they reference no variables declared after them.
                //                 useResult =
                //                     lastLocation.kind === SyntaxKind.Parameter ||
                //                     (
                //                         lastLocation === (loc as FunctionLikeDeclaration).type &&
                //                         !!findAncestor(result.valueDeclaration, isParameter)
                //                     );
                //             }
                //         }
                //     }
                //     else if (loc.kind === SyntaxKind.ConditionalType) {
                //         // A type parameter declared using 'infer T' in a conditional type is visible only in
                //         // the true branch of the conditional type.
                //         useResult = lastLocation === (loc as ConditionalTypeNode).trueType;
                //     }

                //     if (useResult) {
                //         break loop;
                //     }
                //     else {
                //         result = undefined;
                //     }
                // }
            }
            withinDeferredContext =
                withinDeferredContext || getIsDeferredContext(&loc, &lastLocation);
            match &loc {
                BoundNode::Script(_) | BoundNode::Module(_) | BoundNode::TsModuleDecl(_) => {
                    let mod_decl = match &loc {
                        BoundNode::Script(_) | BoundNode::Module(_) => {
                            // TODO:
                            // if !isExternalOrCommonJsModule(loc) {
                            //     break;
                            // }
                            isInExternalModule = true;
                            None
                        }
                        BoundNode::TsModuleDecl(mod_decl) => Some(mod_decl),
                        _ => unreachable!(),
                    };

                    let mod_symbol_id = self.getSymbolOfNode(loc.clone()).unwrap();
                    if matches!(loc, BoundNode::Script(_) | BoundNode::Module(_))
                        || (self
                            .node_data(loc.clone())
                            .flags
                            .intersects(NodeFlags::Ambient)
                            && !isGlobalScopeAugmentation(mod_decl.unwrap()))
                    {
                        let moduleExports = self.symbols[mod_symbol_id].exports();

                        // It's an external module. First see if the module has an export default and if the local
                        // name of that export default matches.
                        result = moduleExports
                            .get(&InternalSymbolName::Default.into())
                            .copied();
                        if let Some(res) = result {
                            todo!();
                            // let localSymbol = self.getLocalSymbolForExportDefault(res);
                            // if localSymbol
                            //     && self.symbols[res].flags().intersects(meaning)
                            //     && localSymbol.escapedName == name
                            // {
                            //     break '_loop;
                            // }
                            // result = None;
                        }

                        // Because of module/namespace merging, a module's exports are in scope,
                        // yet we never want to treat an export specifier as putting a member in scope.
                        // Therefore, if the name we find is purely an export specifier, it is not actually considered in scope.
                        // Two things to note about this:
                        //     1. We have to check this without calling getSymbol. The problem with calling getSymbol
                        //        on an export specifier is that it might find the export specifier itself, and try to
                        //        resolve it as an alias. This will cause the checker to consider the export specifier
                        //        a circular alias reference when it might not be.
                        //     2. We check === SymbolFlags::Alias in order to check that the symbol is *purely*
                        //        an alias. If we used &, we'd be throwing out symbols that have non alias aspects,
                        //        which is not the desired behavior.
                        let moduleExport = moduleExports.get(&name);
                        if let Some(moduleExport) = moduleExport {
                            todo!();
                            // if moduleExport.flags == SymbolFlags::Alias
                            //     && (getDeclarationOfKind(moduleExport, SyntaxKind.ExportSpecifier)
                            //         || getDeclarationOfKind(
                            //             moduleExport,
                            //             SyntaxKind.NamespaceExport,
                            //         ))
                            // {
                            //     break;
                            // }
                        }
                    }

                    // ES6 exports are also visible locally (except for 'default'), but commonjs exports are not (except typedefs)
                    result = with_table!(table = self.symbols[mod_symbol_id].exports_mut(), {
                        self.getSymbol(&table, name.clone(), meaning & SymbolFlags::ModuleMember)
                    });
                    if name != JsWord::from(InternalSymbolName::Default) && result.is_some() {
                        todo!();
                        // if isSourceFile(loc)
                        //     && loc.commonJsModuleIndicator
                        //     && !result.declarations?.some(isJSDocTypeAlias)
                        // {
                        //     result = None;
                        // } else {
                        //     break '_loop;
                        // }
                    }
                }
                BoundNode::TsEnumDecl(_) => {
                    let decl_symbol_id = self.getSymbolOfNode(loc.clone()).unwrap();
                    result = with_table!(table = self.symbols[decl_symbol_id].exports_mut(), {
                        self.getSymbol(&table, name.clone(), meaning & SymbolFlags::EnumMember)
                    });
                    if result.is_some() {
                        break '_loop;
                    }
                }
                BoundNode::ClassProp(_) | BoundNode::PrivateProp(_) => {
                    // TypeScript 1.0 spec (April 2014): 8.4.1
                    // Initializer expressions for instance member variables are evaluated in the scope
                    // of the class constructor body but are not permitted to reference parameters or
                    // local variables of the constructor. This effectively means that entities from outer scopes
                    // by the same name as a constructor parameter or local variable are inaccessible
                    // in initializer expressions for instance member variables.
                    if !isStatic(&loc) {
                        let ctor = loc
                            .parent()
                            .as_ref()
                            .and_then(|p| findConstructorDeclaration(p));
                        if let Some(ctor) = ctor {
                            let symbol = with_table!(
                                table = &mut self.node_data_mut(ctor.clone()).locals,
                                {
                                    self.getSymbol(
                                        &table,
                                        name.clone(),
                                        meaning & SymbolFlags::Value,
                                    )
                                }
                            );
                            if symbol.is_some() {
                                // Remember the property node, it will be used later to report appropriate error
                                propertyWithInvalidInitializer = Some(loc.clone());
                            }
                        }
                    }
                }
                BoundNode::ClassDecl(_)
                | BoundNode::ClassExpr(_)
                | BoundNode::TsInterfaceDecl(_) => {
                    // The below is used to lookup type parameters within a class or interface, as they are added to the class/interface locals
                    // These can never be latebound, so the symbol's raw members are sufficient. `getMembersOfNode` cannot be used, as it would
                    // trigger resolving late-bound names, which we may already be in the process of doing while we're here!
                    let decl_symbol_id = self.getSymbolOfNode(loc.clone()).unwrap();
                    result = with_table!(table = self.symbols[decl_symbol_id].members_mut(), {
                        self.getSymbol(&table, name.clone(), meaning & SymbolFlags::Type)
                    });
                    if let Some(res) = result {
                        if !self.isTypeParameterSymbolDeclaredInContainer(res, &loc) {
                            // ignore type parameters not declared in this container
                            result = None;
                            break;
                        }
                        if let Some(lastLocation) = &lastLocation {
                            if isStatic(lastLocation) {
                                // TypeScript 1.0 spec (April 2014): 3.4.1
                                // The scope of a type parameter extends over the entire declaration with which the type
                                // parameter list is associated, with the exception of static member declarations in classes.
                                todo!("error");
                                // error(
                                //     errorLocation,
                                //     Diagnostics.Static_members_cannot_reference_class_type_parameters,
                                // );
                                // return None;
                            }
                        }
                        break '_loop;
                    }
                    if let BoundNode::ClassExpr(class_expr) = &loc {
                        if meaning.intersects(SymbolFlags::Class) {
                            if let Some(class_name) = &class_expr.ident {
                                if name == class_name.sym {
                                    result = self.node_data(loc.clone()).symbol;
                                    break '_loop;
                                }
                            }
                        }
                    }
                }
                BoundNode::TsExprWithTypeArgs(expr) => {
                    // TODO: TSC's version of this block also handles the 'extends' (super class) of a class.
                    // Ours only handles the 'extends' clause of interfaces. This is because TS uses 'ExpressionWithTypeArguments'
                    // for class 'extends' and 'implements', as well as interface 'extends'. We use 'TsExprWithTypeArgs'
                    // for class and interface 'implements', but use two fields for class 'extends':
                    // pub super_class: Option<Box<Expr>>,
                    // pub super_type_params: Option<TsTypeParamInstantiation>,

                    // The type parameters of a class are not in scope in the base class expression.
                    // if lastLocation == Some(expr.expr.bind(loc)) {
                    //     if let BoundNode::TsInterfaceDecl(interface) = loc.parent().unwrap() {
                    //         let container = loc.parent.parent;
                    //         if isClassLike(container) {
                    //             result = self.getSymbol(
                    //                 getSymbolOfNode(container).members.unwrap(),
                    //                 name,
                    //                 meaning & SymbolFlags::Type,
                    //             );
                    //             if result.is_some() {
                    //                 if nameNotFoundMessage {
                    //                     error(errorLocation, Diagnostics.Base_class_expressions_cannot_reference_class_type_parameters);
                    //                 }
                    //                 return None;
                    //             }
                    //         }
                    //     }
                    // }
                }
                // It is not legal to reference a class's own type parameters from a computed property name that
                // belongs to the class. For example:
                //
                //   function foo<T>() { return '' }
                //   class C<T> { // <-- Class's own type parameter T
                //       [foo<T>()]() { } // <-- Reference to T from class's own computed property
                //   }
                //
                // TODO:
                // BoundNode::ComputedPropertyName(_) => {
                //     let grandparent = loc.parent.parent;
                //     if isClassLike(grandparent)
                //         || grandparent.kind == SyntaxKind.InterfaceDeclaration
                //     {
                //         // A reference to this grandparent's type parameters would be an error
                //         result = self.getSymbol(
                //             getSymbolOfNode(
                //                 grandparent as ClassLikeDeclaration | InterfaceDeclaration,
                //             )
                //             .members
                //             .unwrap(),
                //             name,
                //             meaning & SymbolFlags::Type,
                //         );
                //         if result.is_some() {
                //             error(errorLocation, Diagnostics.A_computed_property_name_cannot_reference_a_type_parameter_from_its_containing_type);
                //             return None;
                //         }
                //     }
                // }
                BoundNode::ArrowExpr(_) => {
                    // when targeting ES6 or higher there is no 'arguments' in an arrow function
                    // for lower compile targets the resolved symbol is used to emit an error
                    if self.compilerOptions.getEmitScriptTarget() >= ScriptTarget::ES2015 {
                        break;
                    }

                    if meaning.intersects(SymbolFlags::Variable) && name == js_word!("arguments") {
                        result = Some(self.argumentsSymbol);
                        break '_loop;
                    }
                }
                BoundNode::PrivateMethod(_)
                | BoundNode::ClassMethod(_)
                | BoundNode::MethodProp(_)
                | BoundNode::Constructor(_)
                | BoundNode::GetterProp(_)
                | BoundNode::SetterProp(_)
                | BoundNode::FnDecl(_) => {
                    if meaning.intersects(SymbolFlags::Variable) && name == js_word!("arguments") {
                        result = Some(self.argumentsSymbol);
                        break '_loop;
                    }
                }
                BoundNode::FnExpr(fn_expr) => {
                    if meaning.intersects(SymbolFlags::Variable) && name == js_word!("arguments") {
                        result = Some(self.argumentsSymbol);
                        break '_loop;
                    }

                    if meaning.intersects(SymbolFlags::Function) {
                        if let Some(fn_name) = &fn_expr.ident {
                            if name == fn_name.sym {
                                result = self.node_data(loc.clone()).symbol;
                                break '_loop;
                            }
                        }
                    }
                }
                BoundNode::Decorator(_) => {
                    // Decorators are resolved at the class declaration. Resolving at the parameter
                    // or member would result in looking up locals in the method.
                    //
                    //   function y() {}
                    //   class C {
                    //       method(@y x, y) {} // <-- decorator y should be resolved at the class declaration, not the parameter.
                    //   }
                    //
                    if let Some(BoundNode::Param(_)) = loc.parent() {
                        location = loc.parent();
                    }
                    //
                    //   function y() {}
                    //   class C {
                    //       @y method(x, y) {} // <-- decorator y should be resolved at the class declaration, not the method.
                    //   }
                    //

                    // class Decorators are resolved outside of the class to avoid referencing type parameters of that class.
                    //
                    //   type T = number;
                    //   declare function y(x: T): any;
                    //   @param(1 as T) // <-- T should resolve to the type alias outside of class C
                    //   class C<T> {}
                    if let Some(parent) = loc.parent() {
                        if isClassElement(&parent) || matches!(parent, BoundNode::ClassDecl(_)) {
                            location = loc.parent();
                        }
                    }
                }
                // BoundNode::JSDocTypedefTag:
                // BoundNode::JSDocCallbackTag:
                // BoundNode::JSDocEnumTag:
                //     // js type aliases do not resolve names from their host, so skip past it
                //     const root = getJSDocRoot(loc);
                //     if (root) {
                //         location = root.parent;
                //     }
                //     break;
                BoundNode::Param(param) => {
                    let pat_node = param.pat.bind(loc.clone());
                    let initializer = match &param.pat {
                        ast::Pat::Assign(pat) => Some(pat.right.bind(pat_node)),
                        _ => None,
                    };

                    if let Some(last_location) = &lastLocation {
                        if lastLocation == initializer
                            || last_location == &param.pat.bind(loc.clone())
                                && isBindingPattern(last_location)
                        {
                            if associatedDeclarationForContainingInitializerOrBindingName.is_none()
                            {
                                associatedDeclarationForContainingInitializerOrBindingName =
                                    location.clone();
                            }
                        }
                    }
                }
                BoundNode::BindingIdent(_)
                | BoundNode::ArrayPat(_)
                | BoundNode::RestPat(_)
                | BoundNode::ObjectPat(_)
                | BoundNode::AssignPat(_)
                | BoundNode::KeyValuePatProp(_)
                | BoundNode::AssignPatProp(_) => {
                    todo!("BindingElement");
                    // if let Some(last_location) = lastLocation {
                    //     if lastLocation == (loc as BindingElement).initializer
                    //         || lastLocation == (loc as BindingElement).name
                    //             && isBindingPattern(lastLocation)
                    //     {
                    //         if isParameterDeclaration(loc as BindingElement)
                    //             && associatedDeclarationForContainingInitializerOrBindingName
                    //                 .is_none()
                    //         {
                    //             associatedDeclarationForContainingInitializerOrBindingName =
                    //                 Some(loc);
                    //         }
                    //     }
                    // }
                }
                BoundNode::TsInferType(infer_type) => {
                    if meaning.intersects(SymbolFlags::TypeParameter) {
                        let type_param = &infer_type.type_param;
                        if name == type_param.name.sym {
                            result = self.node_data(type_param.bind(loc.clone())).symbol;
                            break '_loop;
                        }
                    }
                }
                _ => {}
            }
            if isSelfReferenceLocation(loc.clone()) {
                lastSelfReferenceLocation = location.clone();
            }
            lastLocation = location.clone();
            // location = if isJSDocTemplateTag(location) {
            //     getEffectiveContainerForJSDocTemplateTag(location) || location.parent
            // } else {
            //     location.parent
            // };
            location = loc.parent()
        }

        // We just climbed up parents looking for the name, meaning that we started in a descendant node of `lastLocation`.
        // If `result === lastSelfReferenceLocation.symbol`, that means that we are somewhere inside `lastSelfReferenceLocation` looking up a name, and resolving to `lastLocation` itself.
        // That means that this is a self-reference of `lastLocation`, and shouldn't count this when considering whether `lastLocation` is used.
        if isUse {
            if let Some(result_id) = result {
                if lastSelfReferenceLocation
                    .map(|l| result != self.node_data(l).symbol)
                    .unwrap_or_default()
                {
                    let result_sym = &mut self.symbols[result_id];
                    let meaning = result_sym.isReferenced().unwrap() | meaning;
                    // TODO: .map(||) may be nicer here
                    *result_sym.isReferenced_mut() = Some(meaning);
                }
            }
        }

        // if result.is_none() {
        //     if let Some(_) = lastLocation {
        //         todo!();
        //         // Debug.assert(lastLocation.kind == SyntaxKind.SourceFile);
        //         // if ((lastLocation as SourceFile).commonJsModuleIndicator
        //         //     && name == "exports"
        //         //     && meaning & lastLocation.symbol.flags)
        //         // {
        //         //     return lastLocation.symbol;
        //         // }
        //     }

        //     if !excludeGlobals {
        //         result = self.getSymbol(globals, name, meaning);
        //     }
        // }
        // if (!result) {
        //     if (originalLocation && isInJSFile(originalLocation) && originalLocation.parent) {
        //         if (isRequireCall(originalLocation.parent, /*checkArgumentIsStringLiteralLike*/ false)) {
        //             return requireSymbol;
        //         }
        //     }
        // }
        // if (!result) {
        //     if (nameNotFoundMessage) {
        //         if (!errorLocation ||
        //             !checkAndReportErrorForMissingPrefix(errorLocation, name, nameArg!) && // TODO: GH#18217
        //             !checkAndReportErrorForExtendingInterface(errorLocation) &&
        //             !checkAndReportErrorForUsingTypeAsNamespace(errorLocation, name, meaning) &&
        //             !checkAndReportErrorForExportingPrimitiveType(errorLocation, name) &&
        //             !checkAndReportErrorForUsingTypeAsValue(errorLocation, name, meaning) &&
        //             !checkAndReportErrorForUsingNamespaceModuleAsValue(errorLocation, name, meaning) &&
        //             !checkAndReportErrorForUsingValueAsType(errorLocation, name, meaning)) {
        //             let suggestion: Symbol | undefined;
        //             if (suggestionCount < maximumSuggestionCount) {
        //                 suggestion = getSuggestedSymbolForNonexistentSymbol(originalLocation, name, meaning);
        //                 const isGlobalScopeAugmentationDeclaration = suggestion?.valueDeclaration && isAmbientModule(suggestion.valueDeclaration) && isGlobalScopeAugmentation(suggestion.valueDeclaration);
        //                 if (isGlobalScopeAugmentationDeclaration) {
        //                     suggestion = undefined;
        //                 }
        //                 if (suggestion) {
        //                     const suggestionName = symbolToString(suggestion);
        //                     const isUncheckedJS = isUncheckedJSSuggestion(originalLocation, suggestion, /*excludeClasses*/ false);
        //                     const message = meaning === SymbolFlags::Namespace || nameArg && typeof nameArg !== "string" && nodeIsSynthesized(nameArg) ? Diagnostics.Cannot_find_namespace_0_Did_you_mean_1
        //                         : isUncheckedJS ? Diagnostics.Could_not_find_name_0_Did_you_mean_1
        //                         : Diagnostics.Cannot_find_name_0_Did_you_mean_1;
        //                     const diagnostic = createError(errorLocation, message, diagnosticName(nameArg!), suggestionName);
        //                     addErrorOrSuggestion(!isUncheckedJS, diagnostic);
        //                     if (suggestion.valueDeclaration) {
        //                         addRelatedInfo(
        //                             diagnostic,
        //                             createDiagnosticForNode(suggestion.valueDeclaration, Diagnostics._0_is_declared_here, suggestionName)
        //                         );
        //                     }
        //                 }
        //             }
        //             if (!suggestion) {
        //                 if (nameArg) {
        //                     const lib = getSuggestedLibForNonExistentName(nameArg);
        //                     if (lib) {
        //                         error(errorLocation, nameNotFoundMessage, diagnosticName(nameArg), lib);
        //                     }
        //                     else {
        //                         error(errorLocation, nameNotFoundMessage, diagnosticName(nameArg));
        //                     }
        //                 }
        //             }
        //             suggestionCount++;
        //         }
        //     }
        //     return undefined;
        // }

        // // Perform extra checks only if error reporting was requested
        // if (nameNotFoundMessage) {
        //     if (propertyWithInvalidInitializer && !(getEmitScriptTarget(compilerOptions) === ScriptTarget.ESNext && useDefineForClassFields)) {
        //         // We have a match, but the reference occurred within a property initializer and the identifier also binds
        //         // to a local variable in the constructor where the code will be emitted. Note that this is actually allowed
        //         // with ESNext+useDefineForClassFields because the scope semantics are different.
        //         const propertyName = (propertyWithInvalidInitializer as PropertyDeclaration).name;
        //         error(errorLocation, Diagnostics.Initializer_of_instance_member_variable_0_cannot_reference_identifier_1_declared_in_the_constructor,
        //             declarationNameToString(propertyName), diagnosticName(nameArg!));
        //         return undefined;
        //     }

        //     // Only check for block-scoped variable if we have an error location and are looking for the
        //     // name with variable meaning
        //     //      For example,
        //     //          declare module foo {
        //     //              interface bar {}
        //     //          }
        //     //      const foo/*1*/: foo/*2*/.bar;
        //     // The foo at /*1*/ and /*2*/ will share same symbol with two meanings:
        //     // block-scoped variable and namespace module. However, only when we
        //     // try to resolve name in /*1*/ which is used in variable position,
        //     // we want to check for block-scoped
        //     if (errorLocation &&
        //         (meaning & SymbolFlags::BlockScopedVariable ||
        //          ((meaning & SymbolFlags::Class || meaning & SymbolFlags::Enum) && (meaning & SymbolFlags::Value) === SymbolFlags::Value))) {
        //         const exportOrLocalSymbol = getExportSymbolOfValueSymbolIfExported(result);
        //         if (exportOrLocalSymbol.flags & SymbolFlags::BlockScopedVariable || exportOrLocalSymbol.flags & SymbolFlags::Class || exportOrLocalSymbol.flags & SymbolFlags::Enum) {
        //             checkResolvedBlockScopedVariable(exportOrLocalSymbol, errorLocation);
        //         }
        //     }

        //     // If we're in an external module, we can't reference value symbols created from UMD export declarations
        //     if (result && isInExternalModule && (meaning & SymbolFlags::Value) === SymbolFlags::Value && !(originalLocation!.flags & NodeFlags.JSDoc)) {
        //         const merged = getMergedSymbol(result);
        //         if (length(merged.declarations) && every(merged.declarations, d => isNamespaceExportDeclaration(d) || isSourceFile(d) && !!d.symbol.globalExports)) {
        //             errorOrSuggestion(!compilerOptions.allowUmdGlobalAccess, errorLocation!, Diagnostics._0_refers_to_a_UMD_global_but_the_current_file_is_a_module_Consider_adding_an_import_instead, unescapeLeadingUnderscores(name));
        //         }
        //     }

        //     // If we're in a parameter initializer or binding name, we can't reference the values of the parameter whose initializer we're within or parameters to the right
        //     if (result && associatedDeclarationForContainingInitializerOrBindingName && !withinDeferredContext && (meaning & SymbolFlags::Value) === SymbolFlags::Value) {
        //         const candidate = getMergedSymbol(getLateBoundSymbol(result));
        //         const root = (getRootDeclaration(associatedDeclarationForContainingInitializerOrBindingName) as ParameterDeclaration);
        //         // A parameter initializer or binding pattern initializer within a parameter cannot refer to itself
        //         if (candidate === getSymbolOfNode(associatedDeclarationForContainingInitializerOrBindingName)) {
        //             error(errorLocation, Diagnostics.Parameter_0_cannot_reference_itself, declarationNameToString(associatedDeclarationForContainingInitializerOrBindingName.name));
        //         }
        //         // And it cannot refer to any declarations which come after it
        //         else if (candidate.valueDeclaration && candidate.valueDeclaration.pos > associatedDeclarationForContainingInitializerOrBindingName.pos && root.parent.locals && self.getSymbol(root.parent.locals, candidate.escapedName, meaning) === candidate) {
        //             error(errorLocation, Diagnostics.Parameter_0_cannot_reference_identifier_1_declared_after_it, declarationNameToString(associatedDeclarationForContainingInitializerOrBindingName.name), declarationNameToString(errorLocation as Identifier));
        //         }
        //     }
        //     if (result && errorLocation && meaning & SymbolFlags::Value && result.flags & SymbolFlags::Alias) {
        //         checkSymbolUsageInExpressionContext(result, name, errorLocation);
        //     }
        // }
        result
    }

    // TODO:
    // checkSymbolUsageInExpressionContext
    // TODO:
    // addTypeOnlyDeclarationRelatedInfo
    // TODO:
    // getIsDeferredContext
    // TODO:
    // isSelfReferenceLocation
    // TODO:
    // diagnosticName

    fn isTypeParameterSymbolDeclaredInContainer(
        &self,
        symbol: SymbolId,
        container: &BoundNode,
    ) -> bool {
        for decl in self.symbols[symbol].declarations() {
            if let BoundNode::TsTypeParam(param) = decl {
                // TODO: JSdoc
                // let parent = isJSDocTemplateTag(decl.parent) ? getJSDocHost(decl.parent) : decl.parent;
                let parent = decl.parent();
                if parent.as_ref() == Some(container) {
                    // return !(isJSDocTemplateTag(decl.parent) && find((decl.parent.parent as JSDoc).tags!, isJSDocTypeAlias));
                    return true;
                }
            }
        }

        false
    }

    // TODO:
    // checkAndReportErrorForMissingPrefix
    // TODO:
    // checkAndReportErrorForExtendingInterface
    // TODO:
    // getEntityNameForExtendingInterface
    // TODO:
    // checkAndReportErrorForUsingTypeAsNamespace
    // TODO:
    // checkAndReportErrorForUsingValueAsType
    // TODO:
    // isPrimitiveTypeName
    // TODO:
    // checkAndReportErrorForExportingPrimitiveType
    // TODO:
    // checkAndReportErrorForUsingTypeAsValue
    // TODO:
    // maybeMappedType
    // TODO:
    // isES2015OrLaterConstructorName
    // TODO:
    // checkAndReportErrorForUsingNamespaceModuleAsValue
    // TODO:
    // checkResolvedBlockScopedVariable
    // TODO:
    // isSameScopeDescendentOf
    // TODO:
    // getAnyImportSyntax
    // TODO:
    // getDeclarationOfAliasSymbol
    // TODO:
    // isAliasSymbolDeclaration
    // TODO:
    // isAliasableOrJsExpression
    // TODO:
    // getTargetOfImportEqualsDeclaration
    // TODO:
    // checkAndReportErrorForResolvingImportAliasToTypeOnlySymbol
    // TODO:
    // resolveExportByName
    // TODO:
    // isSyntacticDefault
    // TODO:
    // getUsageModeForExpression
    // TODO:
    // isESMFormatImportImportingCommonjsFormatFile
    // TODO:
    // canHaveSyntheticDefault
    // TODO:
    // getTargetOfImportClause
    // TODO:
    // reportNonDefaultExport
    // TODO:
    // getTargetOfNamespaceImport
    // TODO:
    // getTargetOfNamespaceExport
    // TODO:
    // combineValueAndTypeSymbols
    // TODO:
    // getExportOfModule
    // TODO:
    // getPropertyOfVariable
    // TODO:
    // getExternalModuleMember
    // TODO:
    // reportNonExportedMember
    // TODO:
    // reportInvalidImportEqualsExportMember
    // TODO:
    // getTargetOfImportSpecifier
    // TODO:
    // getCommonJSPropertyAccess
    // TODO:
    // getTargetOfNamespaceExportDeclaration
    // TODO:
    // getTargetOfExportSpecifier
    // TODO:
    // getTargetOfExportAssignment
    // TODO:
    // getTargetOfAliasLikeExpression
    // TODO:
    // getTargetOfPropertyAssignment
    // TODO:
    // getTargetOfAccessExpression
    // TODO:
    // getTargetOfAliasDeclaration
    // TODO:
    // isNonLocalAlias

    fn resolveSymbol(&mut self, symbol: SymbolId, dontResolveAlias: bool) -> SymbolId {
        todo!();
        // if !dontResolveAlias && isNonLocalAlias(symbol) {
        //     resolveAlias(symbol)
        // } else {
        //     symbol
        // }
    }

    fn maybeResolveSymbol(
        &mut self,
        symbol: Option<SymbolId>,
        dontResolveAlias: bool,
    ) -> Option<SymbolId> {
        todo!();
        // if !dontResolveAlias && isNonLocalAlias(symbol) {
        //     resolveAlias(symbol)
        // } else {
        //     symbol
        // }
    }

    fn resolveAlias(&self, symbol: SymbolId) -> SymbolId {
        todo!();
        // debug_assert!(self.symbols[symbol].flags().intersects(SymbolFlags::Alias), "Should only get Alias here.");
        // let links = self.getSymbolLinks(symbol);
        // if !links.target {
        //     links.target = self.resolvingSymbol;
        //     let node = self.getDeclarationOfAliasSymbol(symbol);
        //     if (!node) {return Debug.fail()};
        //     let target = self.getTargetOfAliasDeclaration(node);
        //     if links.target == self.resolvingSymbol {
        //         links.target = target || self.unknownSymbol;
        //     }
        //     else {
        //         todo!("error");
        //         // error(node, Diagnostics.Circular_definition_of_import_alias_0, symbolToString(symbol));
        //     }
        // }
        // else if links.target == self.resolvingSymbol {
        //     links.target = self.unknownSymbol;
        // }
        // links.target
    }

    // TODO:
    // tryResolveAlias
    // TODO:
    // markSymbolOfAliasDeclarationIfTypeOnly
    // TODO:
    // markSymbolOfAliasDeclarationIfTypeOnlyWorker
    // TODO:
    // getTypeOnlyAliasDeclaration
    // TODO:
    // markExportAsReferenced
    // TODO:
    // markAliasSymbolAsReferenced
    // TODO:
    // markConstEnumAliasAsReferenced
    // TODO:
    // getSymbolOfPartOfRightHandSideOfImportEquals
    // TODO:
    // getFullyQualifiedName
    // TODO:
    // getContainingQualifiedNameNode
    // TODO:
    // tryGetQualifiedNameAsValue
    // TODO:
    // resolveEntityName
    // TODO:
    // resolveEntityNameFromAssignmentDeclaration
    // TODO:
    // getAssignmentDeclarationLocation
    // TODO:
    // getDeclarationOfJSPrototypeContainer
    // TODO:
    // getExpandoSymbol
    // TODO:
    // resolveExternalModuleName
    // TODO:
    // resolveExternalModuleNameWorker
    // TODO:
    // resolveExternalModule
    // TODO:
    // errorOnImplicitAnyModule
    // TODO:
    // typesPackageExists
    // TODO:
    // packageBundlesTypes
    // TODO:
    // resolveExternalModuleSymbol
    // TODO:
    // resolveExternalModuleSymbol
    // TODO:
    // resolveExternalModuleSymbol
    // TODO:
    // getCommonJsExportEquals
    // TODO:
    // resolveESModuleSymbol
    // TODO:
    // hasExportAssignmentSymbol
    // TODO:
    // getExportsOfModuleAsArray
    // TODO:
    // getExportsAndPropertiesOfModule
    // TODO:
    // forEachExportAndPropertyOfModule
    // TODO:
    // tryGetMemberInModuleExports
    // TODO:
    // tryGetMemberInModuleExportsAndProperties
    // TODO:
    // shouldTreatPropertiesOfExternalModuleAsExports
    // TODO:
    // getExportsOfSymbol
    // TODO:
    // getExportsOfModule
    // TODO:
    // extendExportSymbols
    // TODO:
    // getExportsOfModuleWorker

    fn getMergedSymbol(&self, symbol: SymbolId) -> SymbolId {
        if let Some(mergeId) = self.symbols[symbol].mergeId() {
            self.mergedSymbols[*mergeId]
        } else {
            symbol
        }
    }

    fn getMergedSymbolOptional(&self, symbol: Option<SymbolId>) -> Option<SymbolId> {
        if let Some(symbol) = symbol {
            if let Some(mergeId) = self.symbols[symbol].mergeId() {
                return Some(self.mergedSymbols[*mergeId]);
            }
        }
        symbol
    }

    fn getSymbolOfNode(&mut self, node: BoundNode) -> Option<SymbolId> {
        todo!();
        // return getMergedSymbol(node.symbol && getLateBoundSymbol(node.symbol));
    }

    // TODO:
    // getParentOfSymbol
    // TODO:
    // getAlternativeContainingModules
    // TODO:
    // getContainersOfSymbol
    // TODO:
    // getVariableDeclarationOfObjectLiteral
    // TODO:
    // getFileSymbolIfFileSymbolExportEqualsContainer
    // TODO:
    // getAliasForSymbolInContainer
    // TODO:
    // getSymbolIfSameReference
    // TODO:
    // getExportSymbolOfValueSymbolIfExported
    // TODO:
    // getExportSymbolOfValueSymbolIfExported
    // TODO:
    // getExportSymbolOfValueSymbolIfExported
    // TODO:
    // symbolIsValue

    // TODO:
    // createOriginType

    // TODO:
    // fn createIntrinsicType(
    //     &mut self,
    //     kind: TypeFlags,
    //     intrinsic_name: &'static str,
    //     object_flags: ObjectFlags,
    // ) -> TypeId {
    //     let ty = Type::new_intrinsic(kind, intrinsic_name, object_flags);
    //     self.types.push(ty)
    // }

    fn createObjectType(&mut self, objectFlags: ObjectFlags, symbol: Option<SymbolId>) -> TypeId {
        let ty = Type::ObjectType(ObjectType::new(objectFlags, symbol));
        self.types.push(ty)
    }

    // TODO:
    // createTypeofType
    // TODO:
    // createTypeParameter
    // TODO:
    // isReservedMemberName
    // TODO:
    // getNamedMembers
    // TODO:
    // getNamedOrIndexSignatureMembers
    // TODO:
    // setStructuredTypeMembers
    // TODO:
    // createAnonymousType
    // TODO:
    // getResolvedTypeWithoutAbstractConstructSignatures
    // TODO:
    // forEachSymbolTableInScope
    // TODO:
    // getQualifiedLeftMeaning
    // TODO:
    // getAccessibleSymbolChain
    // TODO:
    // needsQualification
    // TODO:
    // isPropertyOrMethodDeclarationSymbol
    // TODO:
    // isTypeSymbolAccessible
    // TODO:
    // isValueSymbolAccessible
    // TODO:
    // isSymbolAccessibleByFlags
    // TODO:
    // isAnySymbolAccessible
    // TODO:
    // isSymbolAccessible
    // TODO:
    // isSymbolAccessibleWorker
    // TODO:
    // getExternalModuleContainer
    // TODO:
    // hasExternalModuleSymbol
    // TODO:
    // hasNonGlobalAugmentationExternalModuleSymbol
    // TODO:
    // hasVisibleDeclarations
    // TODO:
    // isEntityNameVisible
    // TODO:
    // symbolToString
    // TODO:
    // signatureToString
    // TODO:
    // typeToString
    // TODO:
    // getTypeNamesForErrorDisplay
    // TODO:
    // getTypeNameForErrorDisplay
    // TODO:
    // symbolValueDeclarationIsContextSensitive
    // TODO:
    // toNodeBuilderFlags
    // TODO:
    // isClassInstanceSide
    // TODO:
    // createNodeBuilder
    // TODO:
    // typePredicateToString
    // TODO:
    // formatUnionTypes
    // TODO:
    // visibilityToString
    // TODO:
    // getTypeAliasForTypeLiteral
    // TODO:
    // isTopLevelInExternalModuleAugmentation
    // TODO:
    // isDefaultBindingContext
    // TODO:
    // getNameOfSymbolFromNameType
    // TODO:
    // getNameOfSymbolAsWritten
    // TODO:
    // isDeclarationVisible
    // TODO:
    // collectLinkedAliases
    // TODO:
    // pushTypeResolution
    // TODO:
    // findResolutionCycleStartIndex
    // TODO:
    // hasType
    // TODO:
    // popTypeResolution
    // TODO:
    // getDeclarationContainer
    // TODO:
    // getTypeOfPrototypeProperty
    // TODO:
    // getTypeOfPropertyOfType
    // TODO:
    // getTypeOfPropertyOrIndexSignature
    // TODO:
    // isTypeAny
    // TODO:
    // isErrorType
    // TODO:
    // getTypeForBindingElementParent
    // TODO:
    // getRestType
    // TODO:
    // isGenericTypeWithUndefinedConstraint
    // TODO:
    // getNonUndefinedType
    // TODO:
    // getFlowTypeOfDestructuring
    // TODO:
    // getSyntheticElementAccess
    // TODO:
    // getParentElementAccess
    // TODO:
    // getDestructuringPropertyName
    // TODO:
    // getLiteralPropertyNameText
    // TODO:
    // getTypeForBindingElement
    // TODO:
    // getTypeForDeclarationFromJSDocComment
    // TODO:
    // isNullOrUndefined
    // TODO:
    // isEmptyArrayLiteral
    // TODO:
    // addOptionality
    // TODO:
    // getTypeForVariableLikeDeclaration
    // TODO:
    // isConstructorDeclaredProperty
    // TODO:
    // isAutoTypedProperty
    // TODO:
    // getDeclaringConstructor
    // TODO:
    // getFlowTypeFromCommonJSExport
    // TODO:
    // getFlowTypeInStaticBlocks
    // TODO:
    // getFlowTypeInConstructor
    // TODO:
    // getFlowTypeOfProperty
    // TODO:
    // getWidenedTypeForAssignmentDeclaration
    // TODO:
    // getJSContainerObjectType
    // TODO:
    // getAnnotatedTypeForAssignmentDeclaration
    // TODO:
    // getInitializerTypeFromAssignmentDeclaration
    // TODO:
    // containsSameNamedThisProperty
    // TODO:
    // isDeclarationInConstructor
    // TODO:
    // getConstructorDefinedThisAssignmentTypes
    // TODO:
    // getTypeFromBindingElement
    // TODO:
    // getTypeFromObjectBindingPattern
    // TODO:
    // getTypeFromArrayBindingPattern
    // TODO:
    // getTypeFromBindingPattern
    // TODO:
    // getWidenedTypeForVariableLikeDeclaration
    // TODO:
    // isGlobalSymbolConstructor
    // TODO:
    // widenTypeForVariableLikeDeclaration
    // TODO:
    // declarationBelongsToPrivateAmbientMember
    // TODO:
    // tryGetTypeFromEffectiveTypeNode
    // TODO:
    // getTypeOfVariableOrParameterOrProperty
    // TODO:
    // getTypeOfVariableOrParameterOrPropertyWorker
    // TODO:
    // getAnnotatedAccessorTypeNode
    // TODO:
    // getAnnotatedAccessorType
    // TODO:
    // getAnnotatedAccessorThisParameter
    // TODO:
    // getThisTypeOfDeclaration
    // TODO:
    // getTypeOfAccessors
    // TODO:
    // getTypeOfSetAccessor
    // TODO:
    // getTypeOfAccessorsWorker
    // TODO:
    // resolveTypeOfAccessors
    // TODO:
    // getBaseTypeVariableOfClass
    // TODO:
    // getTypeOfFuncClassEnumModule
    // TODO:
    // getTypeOfFuncClassEnumModuleWorker
    // TODO:
    // getTypeOfEnumMember
    // TODO:
    // getTypeOfAlias
    // TODO:
    // getTypeOfInstantiatedSymbol
    // TODO:
    // reportCircularityError
    // TODO:
    // getTypeOfSymbolWithDeferredType
    // TODO:
    // getSetAccessorTypeOfSymbol
    // TODO:
    // getTypeOfSymbol
    // TODO:
    // getNonMissingTypeOfSymbol
    // TODO:
    // isReferenceToType
    // TODO:
    // getTargetType
    // TODO:
    // hasBaseType
    // TODO:
    // appendTypeParameters
    // TODO:
    // getOuterTypeParameters
    // TODO:
    // getOuterTypeParametersOfClassOrInterface
    // TODO:
    // getLocalTypeParametersOfClassOrInterfaceOrTypeAlias
    // TODO:
    // getTypeParametersOfClassOrInterface
    // TODO:
    // isMixinConstructorType
    // TODO:
    // isConstructorType
    // TODO:
    // getBaseTypeNodeOfClass
    // TODO:
    // getConstructorsForTypeArguments
    // TODO:
    // getInstantiatedConstructorsForTypeArguments
    // TODO:
    // getBaseConstructorTypeOfClass
    // TODO:
    // getImplementsTypes
    // TODO:
    // reportCircularBaseType
    // TODO:
    // getBaseTypes
    // TODO:
    // getTupleBaseType
    // TODO:
    // resolveBaseTypesOfClass
    // TODO:
    // areAllOuterTypeParametersApplied
    // TODO:
    // isValidBaseType
    // TODO:
    // resolveBaseTypesOfInterface
    // TODO:
    // isThislessInterface
    // TODO:
    // getDeclaredTypeOfClassOrInterface
    // TODO:
    // getDeclaredTypeOfTypeAlias
    // TODO:
    // isStringConcatExpression
    // TODO:
    // isLiteralEnumMember
    // TODO:
    // getEnumKind
    // TODO:
    // getBaseTypeOfEnumLiteralType
    // TODO:
    // getDeclaredTypeOfEnum
    // TODO:
    // getDeclaredTypeOfEnumMember
    // TODO:
    // getDeclaredTypeOfTypeParameter
    // TODO:
    // getDeclaredTypeOfAlias

    fn getDeclaredTypeOfSymbol(&mut self, symbol: SymbolId) -> TypeId {
        self.tryGetDeclaredTypeOfSymbol(symbol)
            .unwrap_or(self.errorType)
    }

    fn tryGetDeclaredTypeOfSymbol(&mut self, symbol: SymbolId) -> Option<TypeId> {
        todo!();
        // let flags = *self.symbols[symbol].flags();
        // if flags.intersects(SymbolFlags::Class | SymbolFlags::Interface) {
        //     self.getDeclaredTypeOfClassOrInterface(symbol)
        // } else if flags.intersects(SymbolFlags::TypeAlias) {
        //     self.getDeclaredTypeOfTypeAlias(symbol)
        // } else if flags.intersects(SymbolFlags::TypeParameter) {
        //     self.getDeclaredTypeOfTypeParameter(symbol)
        // } else if flags.intersects(SymbolFlags::Enum) {
        //     self.getDeclaredTypeOfEnum(symbol)
        // } else if flags.intersects(SymbolFlags::EnumMember) {
        //     self.getDeclaredTypeOfEnumMember(symbol)
        // } else if flags.intersects(SymbolFlags::Alias) {
        //     self.getDeclaredTypeOfAlias(symbol)
        // } else {
        //     None
        // }
    }

    // TODO:
    // isThislessType
    // TODO:
    // isThislessTypeParameter
    // TODO:
    // isThislessVariableLikeDeclaration
    // TODO:
    // isThislessFunctionLikeDeclaration
    // TODO:
    // isThisless
    // TODO:
    // createInstantiatedSymbolTable
    // TODO:
    // addInheritedMembers
    // TODO:
    // isStaticPrivateIdentifierProperty
    // TODO:
    // resolveDeclaredMembers
    // TODO:
    // isTypeUsableAsPropertyName
    // TODO:
    // isLateBindableName
    // TODO:
    // isLateBoundName
    // TODO:
    // hasLateBindableName
    // TODO:
    // hasBindableName
    // TODO:
    // isNonBindableDynamicName
    // TODO:
    // getPropertyNameFromType
    // TODO:
    // addDeclarationToLateBoundSymbol
    // TODO:
    // lateBindMember
    // TODO:
    // getResolvedMembersOrExportsOfSymbol
    // TODO:
    // getMembersOfSymbol
    // TODO:
    // getLateBoundSymbol
    // TODO:
    // getTypeWithThisArgument
    // TODO:
    // resolveObjectTypeMembers
    // TODO:
    // resolveClassOrInterfaceMembers
    // TODO:
    // resolveTypeReferenceMembers
    // TODO:
    // createSignature
    // TODO:
    // cloneSignature
    // TODO:
    // createUnionSignature
    // TODO:
    // getOptionalCallSignature
    // TODO:
    // createOptionalCallSignature
    // TODO:
    // getExpandedParameters
    // TODO:
    // getDefaultConstructSignatures
    // TODO:
    // findMatchingSignature
    // TODO:
    // findMatchingSignatures
    // TODO:
    // getUnionSignatures
    // TODO:
    // compareTypeParametersIdentical
    // TODO:
    // combineUnionThisParam
    // TODO:
    // combineUnionParameters
    // TODO:
    // combineSignaturesOfUnionMembers
    // TODO:
    // getUnionIndexInfos
    // TODO:
    // resolveUnionTypeMembers
    // TODO:
    // intersectTypes
    // TODO:
    // intersectTypes
    // TODO:
    // intersectTypes
    // TODO:
    // findMixins
    // TODO:
    // includeMixinType
    // TODO:
    // resolveIntersectionTypeMembers
    // TODO:
    // appendSignatures
    // TODO:
    // appendIndexInfo
    // TODO:
    // resolveAnonymousTypeMembers
    // TODO:
    // replaceIndexedAccess
    // TODO:
    // resolveReverseMappedTypeMembers
    // TODO:
    // getLowerBoundOfKeyType
    // TODO:
    // getIsLateCheckFlag
    // TODO:
    // forEachMappedTypePropertyKeyTypeAndIndexSignatureKeyType
    // TODO:
    // resolveMappedTypeMembers
    // TODO:
    // getTypeOfMappedSymbol
    // TODO:
    // getTypeParameterFromMappedType
    // TODO:
    // getConstraintTypeFromMappedType
    // TODO:
    // getNameTypeFromMappedType
    // TODO:
    // getTemplateTypeFromMappedType
    // TODO:
    // getConstraintDeclarationForMappedType
    // TODO:
    // isMappedTypeWithKeyofConstraintDeclaration
    // TODO:
    // getModifiersTypeFromMappedType
    // TODO:
    // getMappedTypeModifiers
    // TODO:
    // getMappedTypeOptionality
    // TODO:
    // getCombinedMappedTypeOptionality
    // TODO:
    // isPartialMappedType
    // TODO:
    // isGenericMappedType
    // TODO:
    // resolveStructuredTypeMembers
    // TODO:
    // getPropertiesOfObjectType
    // TODO:
    // getPropertyOfObjectType
    // TODO:
    // getPropertiesOfUnionOrIntersectionType
    // TODO:
    // getPropertiesOfType
    // TODO:
    // isTypeInvalidDueToUnionDiscriminant
    // TODO:
    // getAllPossiblePropertiesOfTypes
    // TODO:
    // getConstraintOfType
    // TODO:
    // getConstraintOfTypeParameter
    // TODO:
    // getConstraintOfIndexedAccess
    // TODO:
    // getSimplifiedTypeOrConstraint
    // TODO:
    // getConstraintFromIndexedAccess
    // TODO:
    // getDefaultConstraintOfConditionalType
    // TODO:
    // getConstraintOfDistributiveConditionalType
    // TODO:
    // getConstraintFromConditionalType
    // TODO:
    // getConstraintOfConditionalType
    // TODO:
    // getEffectiveConstraintOfIntersection
    // TODO:
    // getBaseConstraintOfType
    // TODO:
    // getBaseConstraintOrType
    // TODO:
    // hasNonCircularBaseConstraint
    // TODO:
    // getResolvedBaseConstraint
    // TODO:
    // getApparentTypeOfIntersectionType
    // TODO:
    // getResolvedTypeParameterDefault
    // TODO:
    // getDefaultFromTypeParameter
    // TODO:
    // hasNonCircularTypeParameterDefault
    // TODO:
    // hasTypeParameterDefault
    // TODO:
    // getApparentTypeOfMappedType
    // TODO:
    // getResolvedApparentTypeOfMappedType
    // TODO:
    // getApparentType
    // TODO:
    // getReducedApparentType
    // TODO:
    // createUnionOrIntersectionProperty
    // TODO:
    // getUnionOrIntersectionProperty
    // TODO:
    // getPropertyOfUnionOrIntersectionType
    // TODO:
    // getReducedType
    // TODO:
    // getReducedUnionType
    // TODO:
    // isNeverReducedProperty
    // TODO:
    // isDiscriminantWithNeverType
    // TODO:
    // isConflictingPrivateProperty
    // TODO:
    // elaborateNeverIntersection
    // TODO:
    // getPropertyOfType
    // TODO:
    // getSignaturesOfStructuredType
    // TODO:
    // getSignaturesOfType
    // TODO:
    // findIndexInfo
    // TODO:
    // findApplicableIndexInfo
    // TODO:
    // isApplicableIndexType
    // TODO:
    // getIndexInfosOfStructuredType
    // TODO:
    // getIndexInfosOfType
    // TODO:
    // getIndexInfoOfType
    // TODO:
    // getIndexTypeOfType
    // TODO:
    // getApplicableIndexInfos
    // TODO:
    // getApplicableIndexInfo
    // TODO:
    // getApplicableIndexInfoForName
    // TODO:
    // getTypeParametersFromDeclaration
    // TODO:
    // symbolsToArray
    // TODO:
    // isJSDocOptionalParameter
    // TODO:
    // tryFindAmbientModule
    // TODO:
    // isOptionalParameter
    // TODO:
    // isOptionalPropertyDeclaration
    // TODO:
    // isOptionalJSDocPropertyLikeTag
    // TODO:
    // createTypePredicate
    // TODO:
    // getMinTypeArgumentCount
    // TODO:
    // fillMissingTypeArguments
    // TODO:
    // fillMissingTypeArguments
    // TODO:
    // fillMissingTypeArguments
    // TODO:
    // getSignatureFromDeclaration
    // TODO:
    // maybeAddJsSyntheticRestParameter
    // TODO:
    // getSignatureOfTypeTag
    // TODO:
    // getReturnTypeOfTypeTag
    // TODO:
    // containsArgumentsReference
    // TODO:
    // getSignaturesOfSymbol
    // TODO:
    // resolveExternalModuleTypeByLiteral
    // TODO:
    // getThisTypeOfSignature
    // TODO:
    // getTypePredicateOfSignature
    // TODO:
    // createTypePredicateFromTypePredicateNode
    // TODO:
    // getUnionOrIntersectionType
    // TODO:
    // getReturnTypeOfSignature
    // TODO:
    // getReturnTypeFromAnnotation
    // TODO:
    // isResolvingReturnTypeOfSignature
    // TODO:
    // getRestTypeOfSignature
    // TODO:
    // tryGetRestTypeOfSignature
    // TODO:
    // getSignatureInstantiation
    // TODO:
    // getSignatureInstantiationWithoutFillingInTypeArguments
    // TODO:
    // createSignatureInstantiation
    // TODO:
    // createSignatureTypeMapper
    // TODO:
    // getErasedSignature
    // TODO:
    // createErasedSignature
    // TODO:
    // getCanonicalSignature
    // TODO:
    // createCanonicalSignature
    // TODO:
    // getBaseSignature
    // TODO:
    // getOrCreateTypeFromSignature
    // TODO:
    // getIndexSymbol
    // TODO:
    // getIndexSymbolFromSymbolTable
    // TODO:
    // createIndexInfo
    // TODO:
    // getIndexInfosOfSymbol
    // TODO:
    // getIndexInfosOfIndexSymbol
    // TODO:
    // isValidIndexKeyType
    // TODO:
    // getConstraintDeclaration
    // TODO:
    // getInferredTypeParameterConstraint
    // TODO:
    // getConstraintFromTypeParameter
    // TODO:
    // getParentSymbolOfTypeParameter
    // TODO:
    // getTypeListId
    // TODO:
    // getAliasId
    // TODO:
    // getPropagatingFlagsOfTypes
    // TODO:
    // createTypeReference
    // TODO:
    // cloneTypeReference
    // TODO:
    // createDeferredTypeReference
    // TODO:
    // getTypeArguments
    // TODO:
    // getTypeReferenceArity
    // TODO:
    // getTypeFromClassOrInterfaceReference
    // TODO:
    // getTypeAliasInstantiation
    // TODO:
    // getTypeFromTypeAliasReference
    // TODO:
    // isLocalTypeAlias
    // TODO:
    // getTypeReferenceName
    // TODO:
    // getSymbolPath
    // TODO:
    // getUnresolvedSymbolForEntityName
    // TODO:
    // resolveTypeReferenceName
    // TODO:
    // getTypeReferenceType
    // TODO:
    // getTypeFromJSDocValueReference
    // TODO:
    // getSubstitutionType
    // TODO:
    // isUnaryTupleTypeNode
    // TODO:
    // getImpliedConstraint
    // TODO:
    // getConditionalFlowTypeOfType
    // TODO:
    // isJSDocTypeReference
    // TODO:
    // checkNoTypeArguments
    // TODO:
    // getIntendedTypeFromJSDocTypeReference
    // TODO:
    // getTypeFromJSDocNullableTypeNode
    // TODO:
    // getTypeFromTypeReference
    // TODO:
    // typeArgumentsFromTypeReferenceNode
    // TODO:
    // getTypeFromTypeQueryNode

    fn getTypeOfGlobalSymbol(&mut self, symbol: Option<SymbolId>, arity: u8) -> TypeId {
        fn getTypeDeclaration(symbol: &Symbol) -> Option<BoundNode> {
            todo!();
            // let declarations = symbol.declarations;
            // if (declarations) {
            //     for declaration in declarations {
            //         switch (declaration.kind) {
            //             case SyntaxKind.ClassDeclaration:
            //             case SyntaxKind.InterfaceDeclaration:
            //             case SyntaxKind.EnumDeclaration:
            //                 return declaration;
            //         }
            //     }
            // }
        }

        let symbol = match symbol {
            Some(s) => s,
            None => {
                return if arity > 0 {
                    self.emptyGenericType
                } else {
                    self.emptyObjectType
                }
            }
        };

        let type_id = self.getDeclaredTypeOfSymbol(symbol);
        let ty = &self.types[type_id];
        if !(ty.get_flags().intersects(TypeFlags::Object)) {
            todo!("error");
            // error(
            //     getTypeDeclaration(symbol),
            //     Diagnostics.Global_type_0_must_be_a_class_or_interface_type,
            //     symbolName(&symbol),
            // );
            // return if arity > 0 {
            //     self.emptyGenericType
            // } else {
            //     self.emptyObjectType
            // };
        }
        // TODO:
        // if (length((ty as InterfaceType).typeParameters) != arity) {
        //     todo!("error");
        //     // error(
        //     //     getTypeDeclaration(symbol),
        //     //     Diagnostics.Global_type_0_must_have_1_type_parameter_s,
        //     //     symbolName(&symbol),
        //     //     arity,
        //     // );
        //     // return if arity > 0 {
        //     //     self.emptyGenericType
        //     // } else {
        //     //     self.emptyObjectType
        //     // };
        // }
        type_id
    }

    // TODO:
    // getGlobalValueSymbol

    fn getGlobalTypeSymbol(&mut self, name: JsWord, reportErrors: bool) -> Option<SymbolId> {
        self.getGlobalSymbol(name, SymbolFlags::Type/*, reportErrors ? Diagnostics.Cannot_find_global_type_0 : undefined*/)
    }

    // TODO:
    // getGlobalTypeAliasSymbol

    fn getGlobalSymbol(
        &mut self,
        name: JsWord,
        meaning: SymbolFlags, /*, diagnostic: DiagnosticMessage | undefined*/
    ) -> Option<SymbolId> {
        // Don't track references for global symbols anyway, so value if `isReference` is arbitrary
        self.resolveName(
            None,
            name.clone(),
            meaning,
            /*diagnostic,*/ Some(name),
            false,
            false,
        )
    }

    // function getGlobalType(name: JsWord, arity: 0, reportErrors: true): ObjectType;
    // function getGlobalType(name: JsWord, arity: 0, reportErrors: boolean): ObjectType | undefined;
    // function getGlobalType(name: JsWord, arity: number, reportErrors: true): GenericType;
    // function getGlobalType(name: JsWord, arity: number, reportErrors: boolean): GenericType | undefined;
    // function getGlobalType(name: JsWord, arity: number, reportErrors: boolean): ObjectType | undefined
    fn getGlobalType(&mut self, name: JsWord, arity: u8, reportErrors: bool) -> Option<TypeId> {
        let symbol = self.getGlobalTypeSymbol(name, reportErrors);

        if symbol.is_some() || reportErrors {
            Some(self.getTypeOfGlobalSymbol(symbol, arity))
        } else {
            None
        }
    }

    // TODO:
    // getGlobalTypedPropertyDescriptorType
    // TODO:
    // getGlobalTemplateStringsArrayType
    // TODO:
    // getGlobalImportMetaType
    // TODO:
    // getGlobalImportMetaExpressionType
    // TODO:
    // getGlobalImportCallOptionsType
    // TODO:
    // getGlobalESSymbolConstructorSymbol
    // TODO:
    // getGlobalESSymbolConstructorTypeSymbol
    // TODO:
    // getGlobalESSymbolType
    // TODO:
    // getGlobalPromiseType
    // TODO:
    // getGlobalPromiseLikeType
    // TODO:
    // getGlobalPromiseConstructorSymbol
    // TODO:
    // getGlobalPromiseConstructorLikeType
    // TODO:
    // getGlobalAsyncIterableType
    // TODO:
    // getGlobalAsyncIteratorType
    // TODO:
    // getGlobalAsyncIterableIteratorType
    // TODO:
    // getGlobalAsyncGeneratorType
    // TODO:
    // getGlobalIterableType
    // TODO:
    // getGlobalIteratorType
    // TODO:
    // getGlobalIterableIteratorType
    // TODO:
    // getGlobalGeneratorType
    // TODO:
    // getGlobalIteratorYieldResultType
    // TODO:
    // getGlobalIteratorReturnResultType
    // TODO:
    // getGlobalTypeOrUndefined
    // TODO:
    // getGlobalExtractSymbol
    // TODO:
    // getGlobalOmitSymbol
    // TODO:
    // getGlobalAwaitedSymbol
    // TODO:
    // getGlobalBigIntType
    // TODO:
    // createTypeFromGenericGlobalType
    // TODO:
    // createTypedPropertyDescriptorType
    // TODO:
    // createIterableType
    // TODO:
    // createArrayType
    // TODO:
    // getTupleElementFlags
    // TODO:
    // getRestTypeElementFlags
    // TODO:
    // getArrayOrTupleTargetType
    // TODO:
    // isDeferredTypeReferenceNode
    // TODO:
    // isResolvedByTypeAlias
    // TODO:
    // mayResolveTypeAlias
    // TODO:
    // getTypeFromArrayOrTupleTypeNode
    // TODO:
    // isReadonlyTypeOperator
    // TODO:
    // createTupleType
    // TODO:
    // getTupleTargetType
    // TODO:
    // createTupleTargetType
    // TODO:
    // createNormalizedTypeReference
    // TODO:
    // createNormalizedTupleType
    // TODO:
    // sliceTupleType
    // TODO:
    // getKnownKeysOfTupleType
    // TODO:
    // getStartElementCount
    // TODO:
    // getEndElementCount
    // TODO:
    // getTypeFromOptionalTypeNode
    // TODO:
    // getTypeId
    // TODO:
    // containsType
    // TODO:
    // insertType
    // TODO:
    // addTypeToUnion
    // TODO:
    // addTypesToUnion
    // TODO:
    // removeSubtypes
    // TODO:
    // removeRedundantLiteralTypes
    // TODO:
    // removeStringLiteralsMatchedByTemplateLiterals
    // TODO:
    // isNamedUnionType
    // TODO:
    // addNamedUnions
    // TODO:
    // createOriginUnionOrIntersectionType
    // TODO:
    // getUnionType
    // TODO:
    // getUnionOrIntersectionTypePredicate
    // TODO:
    // typePredicateKindsMatch
    // TODO:
    // getUnionTypeFromSortedList
    // TODO:
    // getTypeFromUnionTypeNode
    // TODO:
    // addTypeToIntersection
    // TODO:
    // addTypesToIntersection
    // TODO:
    // removeRedundantPrimitiveTypes
    // TODO:
    // eachUnionContains
    // TODO:
    // extractRedundantTemplateLiterals
    // TODO:
    // eachIsUnionContaining
    // TODO:
    // removeFromEach
    // TODO:
    // intersectUnionsOfPrimitiveTypes
    // TODO:
    // createIntersectionType
    // TODO:
    // getIntersectionType
    // TODO:
    // getCrossProductUnionSize
    // TODO:
    // checkCrossProductUnion
    // TODO:
    // getCrossProductIntersections
    // TODO:
    // getTypeFromIntersectionTypeNode
    // TODO:
    // createIndexType
    // TODO:
    // createOriginIndexType
    // TODO:
    // getIndexTypeForGenericType
    // TODO:
    // getIndexTypeForMappedType
    // TODO:
    // hasDistributiveNameType
    // TODO:
    // getLiteralTypeFromPropertyName
    // TODO:
    // getLiteralTypeFromProperty
    // TODO:
    // isKeyTypeIncluded
    // TODO:
    // getLiteralTypeFromProperties
    // TODO:
    // getIndexType
    // TODO:
    // getExtractStringType
    // TODO:
    // getIndexTypeOrString
    // TODO:
    // getTypeFromTypeOperatorNode
    // TODO:
    // getTypeFromTemplateTypeNode
    // TODO:
    // getTemplateLiteralType
    // TODO:
    // getTemplateStringForType
    // TODO:
    // createTemplateLiteralType
    // TODO:
    // getStringMappingType
    // TODO:
    // applyStringMapping
    // TODO:
    // getStringMappingTypeForGenericType
    // TODO:
    // createStringMappingType
    // TODO:
    // createIndexedAccessType
    // TODO:
    // isJSLiteralType
    // TODO:
    // getPropertyNameFromIndex
    // TODO:
    // isUncalledFunctionReference
    // TODO:
    // getPropertyTypeForIndexType
    // TODO:
    // getIndexNodeForAccessExpression
    // TODO:
    // isPatternLiteralPlaceholderType
    // TODO:
    // isPatternLiteralType
    // TODO:
    // isGenericType
    // TODO:
    // isGenericObjectType
    // TODO:
    // isGenericIndexType
    // TODO:
    // getGenericObjectFlags
    // TODO:
    // isThisTypeParameter
    // TODO:
    // getSimplifiedType
    // TODO:
    // distributeIndexOverObjectType
    // TODO:
    // distributeObjectOverIndexType
    // TODO:
    // getSimplifiedIndexedAccessType
    // TODO:
    // isConditionalTypeAlwaysTrueDisregardingInferTypes
    // TODO:
    // getSimplifiedConditionalType
    // TODO:
    // isIntersectionEmpty
    // TODO:
    // substituteIndexedMappedType
    // TODO:
    // getIndexedAccessType
    // TODO:
    // indexTypeLessThan
    // TODO:
    // getIndexedAccessTypeOrUndefined
    // TODO:
    // getTypeFromIndexedAccessTypeNode
    // TODO:
    // getTypeFromMappedTypeNode
    // TODO:
    // getActualTypeVariable
    // TODO:
    // isTypicalNondistributiveConditional
    // TODO:
    // isSingletonTupleType
    // TODO:
    // unwrapNondistributiveConditionalTuple
    // TODO:
    // getConditionalType
    // TODO:
    // getTrueTypeFromConditionalType
    // TODO:
    // getFalseTypeFromConditionalType
    // TODO:
    // getInferredTrueTypeFromConditionalType
    // TODO:
    // getInferTypeParameters
    // TODO:
    // getTypeFromConditionalTypeNode
    // TODO:
    // getTypeFromInferTypeNode
    // TODO:
    // getIdentifierChain
    // TODO:
    // getTypeFromImportTypeNode
    // TODO:
    // resolveImportSymbolType
    // TODO:
    // getTypeFromTypeLiteralOrFunctionOrConstructorTypeNode
    // TODO:
    // getAliasSymbolForTypeNode
    // TODO:
    // getTypeArgumentsForAliasSymbol
    // TODO:
    // isNonGenericObjectType
    // TODO:
    // isEmptyObjectTypeOrSpreadsIntoEmptyObject
    // TODO:
    // tryMergeUnionOfObjectTypeAndEmptyObject
    // TODO:
    // getSpreadType
    // TODO:
    // isSpreadableProperty
    // TODO:
    // getSpreadSymbol
    // TODO:
    // getIndexInfoWithReadonly
    // TODO:
    // createLiteralType
    // TODO:
    // getFreshTypeOfLiteralType
    // TODO:
    // getRegularTypeOfLiteralType
    // TODO:
    // isFreshLiteralType
    // TODO:
    // getStringLiteralType
    // TODO:
    // getNumberLiteralType
    // TODO:
    // getBigIntLiteralType
    // TODO:
    // getEnumLiteralType
    // TODO:
    // getTypeFromLiteralTypeNode
    // TODO:
    // createUniqueESSymbolType
    // TODO:
    // getESSymbolLikeTypeForNode
    // TODO:
    // getThisType
    // TODO:
    // getTypeFromThisTypeNode
    // TODO:
    // getTypeFromRestTypeNode
    // TODO:
    // getArrayElementTypeNode
    // TODO:
    // getTypeFromNamedTupleTypeNode
    // TODO:
    // getTypeFromTypeNode
    // TODO:
    // getTypeFromTypeNodeWorker
    // TODO:
    // instantiateList
    // TODO:
    // instantiateList
    // TODO:
    // instantiateList
    // TODO:
    // instantiateTypes
    // TODO:
    // instantiateTypes
    // TODO:
    // instantiateTypes
    // TODO:
    // instantiateSignatures
    // TODO:
    // instantiateIndexInfos
    // TODO:
    // createTypeMapper
    // TODO:
    // getMappedType
    // TODO:
    // makeUnaryTypeMapper
    // TODO:
    // makeArrayTypeMapper
    // TODO:
    // makeFunctionTypeMapper
    // TODO:
    // makeCompositeTypeMapper
    // TODO:
    // createTypeEraser
    // TODO:
    // createBackreferenceMapper
    // TODO:
    // combineTypeMappers
    // TODO:
    // mergeTypeMappers
    // TODO:
    // prependTypeMapping
    // TODO:
    // appendTypeMapping
    // TODO:
    // getRestrictiveTypeParameter
    // TODO:
    // cloneTypeParameter
    // TODO:
    // instantiateTypePredicate
    // TODO:
    // instantiateSignature
    // TODO:
    // instantiateSymbol
    // TODO:
    // getObjectTypeInstantiation
    // TODO:
    // maybeTypeParameterReference
    // TODO:
    // isTypeParameterPossiblyReferenced
    // TODO:
    // getHomomorphicTypeVariable
    // TODO:
    // instantiateMappedType
    // TODO:
    // getModifiedReadonlyState
    // TODO:
    // instantiateMappedGenericTupleType
    // TODO:
    // instantiateMappedArrayType
    // TODO:
    // instantiateMappedTupleType
    // TODO:
    // instantiateMappedTypeTemplate
    // TODO:
    // instantiateAnonymousType
    // TODO:
    // getConditionalTypeInstantiation
    // TODO:
    // instantiateType
    // TODO:
    // instantiateType
    // TODO:
    // instantiateType
    // TODO:
    // instantiateTypeWithAlias
    // TODO:
    // instantiateTypeWorker
    // TODO:
    // instantiateReverseMappedType
    // TODO:
    // getPermissiveInstantiation
    // TODO:
    // getRestrictiveInstantiation
    // TODO:
    // instantiateIndexInfo
    // TODO:
    // isContextSensitive
    // TODO:
    // isContextSensitiveFunctionLikeDeclaration
    // TODO:
    // hasContextSensitiveReturnExpression
    // TODO:
    // isContextSensitiveFunctionOrObjectLiteralMethod
    // TODO:
    // getTypeWithoutSignatures
    // TODO:
    // isTypeIdenticalTo
    // TODO:
    // compareTypesIdentical
    // TODO:
    // compareTypesAssignable
    // TODO:
    // compareTypesSubtypeOf
    // TODO:
    // isTypeSubtypeOf
    // TODO:
    // isTypeAssignableTo
    // TODO:
    // isTypeDerivedFrom
    // TODO:
    // isTypeComparableTo
    // TODO:
    // areTypesComparable
    // TODO:
    // checkTypeAssignableTo
    // TODO:
    // checkTypeAssignableToAndOptionallyElaborate
    // TODO:
    // checkTypeRelatedToAndOptionallyElaborate
    // TODO:
    // isOrHasGenericConditional
    // TODO:
    // elaborateError
    // TODO:
    // elaborateDidYouMeanToCallOrConstruct
    // TODO:
    // elaborateArrowFunction
    // TODO:
    // getBestMatchIndexedAccessTypeOrUndefined
    // TODO:
    // checkExpressionForMutableLocationWithContextualType
    // TODO:
    // elaborateElementwise
    // TODO:
    // generateJsxAttributes
    // TODO:
    // generateJsxChildren
    // TODO:
    // getElaborationElementForJsxChild
    // TODO:
    // elaborateJsxComponents
    // TODO:
    // generateLimitedTupleElements
    // TODO:
    // elaborateArrayLiteral
    // TODO:
    // generateObjectLiteralElements
    // TODO:
    // elaborateObjectLiteral
    // TODO:
    // checkTypeComparableTo
    // TODO:
    // isSignatureAssignableTo
    // TODO:
    // isAnySignature
    // TODO:
    // compareSignaturesRelated
    // TODO:
    // compareTypePredicateRelatedTo
    // TODO:
    // isImplementationCompatibleWithOverload
    // TODO:
    // isEmptyResolvedType
    // TODO:
    // isEmptyObjectType
    // TODO:
    // isEmptyAnonymousObjectType
    // TODO:
    // isStringIndexSignatureOnlyType
    // TODO:
    // isEnumTypeRelatedTo
    // TODO:
    // isSimpleTypeRelatedTo
    // TODO:
    // isTypeRelatedTo
    // TODO:
    // isIgnoredJsxProperty
    // TODO:
    // getNormalizedType
    // TODO:
    // checkTypeRelatedTo
    // TODO:
    // typeCouldHaveTopLevelSingletonTypes
    // TODO:
    // getExactOptionalUnassignableProperties
    // TODO:
    // isExactOptionalPropertyMismatch
    // TODO:
    // getExactOptionalProperties
    // TODO:
    // getBestMatchingType
    // TODO:
    // discriminateTypeByDiscriminableItems
    // TODO:
    // discriminateTypeByDiscriminableItems
    // TODO:
    // discriminateTypeByDiscriminableItems
    // TODO:
    // isWeakType
    // TODO:
    // hasCommonProperties
    // TODO:
    // getMarkerTypeReference
    // TODO:
    // getAliasVariances
    // TODO:
    // getVariancesWorker
    // TODO:
    // getVariances
    // TODO:
    // hasCovariantVoidArgument
    // TODO:
    // isUnconstrainedTypeParameter
    // TODO:
    // isNonDeferredTypeReference
    // TODO:
    // isTypeReferenceWithGenericArguments
    // TODO:
    // getTypeReferenceId
    // TODO:
    // getRelationKey
    // TODO:
    // forEachProperty
    // TODO:
    // getDeclaringClass
    // TODO:
    // getTypeOfPropertyInBaseClass
    // TODO:
    // isPropertyInClassDerivedFrom
    // TODO:
    // isValidOverrideOf
    // TODO:
    // isClassDerivedFromDeclaringClasses
    // TODO:
    // isDeeplyNestedType
    // TODO:
    // getRecursionIdentity
    // TODO:
    // isPropertyIdenticalTo
    // TODO:
    // compareProperties
    // TODO:
    // isMatchingSignature
    // TODO:
    // compareSignaturesIdentical
    // TODO:
    // compareTypePredicatesIdentical
    // TODO:
    // literalTypesWithSameBaseType
    // TODO:
    // getSupertypeOrUnion
    // TODO:
    // getCommonSupertype
    // TODO:
    // getCommonSubtype
    // TODO:
    // isArrayType
    // TODO:
    // isReadonlyArrayType
    // TODO:
    // isMutableArrayOrTuple
    // TODO:
    // getElementTypeOfArrayType
    // TODO:
    // isArrayLikeType
    // TODO:
    // getSingleBaseForNonAugmentingSubtype
    // TODO:
    // isEmptyLiteralType
    // TODO:
    // isEmptyArrayLiteralType
    // TODO:
    // isTupleLikeType
    // TODO:
    // isArrayOrTupleLikeType
    // TODO:
    // getTupleElementType
    // TODO:
    // isNeitherUnitTypeNorNever
    // TODO:
    // isUnitType
    // TODO:
    // isUnitLikeType
    // TODO:
    // extractUnitType
    // TODO:
    // isLiteralType
    // TODO:
    // getBaseTypeOfLiteralType
    // TODO:
    // getWidenedLiteralType
    // TODO:
    // getWidenedUniqueESSymbolType
    // TODO:
    // getWidenedLiteralLikeTypeForContextualType
    // TODO:
    // getWidenedLiteralLikeTypeForContextualReturnTypeIfNeeded
    // TODO:
    // getWidenedLiteralLikeTypeForContextualIterationTypeIfNeeded
    // TODO:
    // isTupleType
    // TODO:
    // isGenericTupleType
    // TODO:
    // isSingleElementGenericTupleType
    // TODO:
    // getRestTypeOfTupleType
    // TODO:
    // getRestArrayTypeOfTupleType
    // TODO:
    // getElementTypeOfSliceOfTupleType
    // TODO:
    // isTupleTypeStructureMatching
    // TODO:
    // isZeroBigInt
    // TODO:
    // getFalsyFlagsOfTypes
    // TODO:
    // getFalsyFlags
    // TODO:
    // removeDefinitelyFalsyTypes
    // TODO:
    // extractDefinitelyFalsyTypes
    // TODO:
    // getDefinitelyFalsyPartOfType
    // TODO:
    // getNullableType
    // TODO:
    // getOptionalType
    // TODO:
    // getGlobalNonNullableTypeInstantiation
    // TODO:
    // getNonNullableType
    // TODO:
    // addOptionalTypeMarker
    // TODO:
    // removeOptionalTypeMarker
    // TODO:
    // propagateOptionalTypeMarker
    // TODO:
    // getOptionalExpressionType
    // TODO:
    // removeMissingType
    // TODO:
    // containsMissingType
    // TODO:
    // removeMissingOrUndefinedType
    // TODO:
    // isCoercibleUnderDoubleEquals
    // TODO:
    // isObjectTypeWithInferableIndex
    // TODO:
    // createSymbolWithType
    // TODO:
    // transformTypeOfMembers
    // TODO:
    // getRegularTypeOfObjectLiteral
    // TODO:
    // createWideningContext
    // TODO:
    // getSiblingsOfContext
    // TODO:
    // getPropertiesOfContext
    // TODO:
    // getWidenedProperty
    // TODO:
    // getUndefinedProperty
    // TODO:
    // getWidenedTypeOfObjectLiteral
    // TODO:
    // getWidenedType
    // TODO:
    // getWidenedTypeWithContext
    // TODO:
    // reportWideningErrorsInType
    // TODO:
    // reportImplicitAny
    // TODO:
    // reportErrorsFromWidening
    // TODO:
    // applyToParameterTypes
    // TODO:
    // applyToReturnTypes
    // TODO:
    // createInferenceContext
    // TODO:
    // cloneInferenceContext
    // TODO:
    // createInferenceContextWorker
    // TODO:
    // mapToInferredType
    // TODO:
    // clearCachedInferences
    // TODO:
    // createInferenceInfo
    // TODO:
    // cloneInferenceInfo
    // TODO:
    // cloneInferredPartOfContext
    // TODO:
    // getMapperFromContext
    // TODO:
    // couldContainTypeVariables
    // TODO:
    // isNonGenericTopLevelType
    // TODO:
    // isTypeParameterAtTopLevel
    // TODO:
    // createEmptyObjectTypeFromStringLiteral
    // TODO:
    // inferTypeForHomomorphicMappedType
    // TODO:
    // isPartiallyInferableType
    // TODO:
    // createReverseMappedType
    // TODO:
    // getTypeOfReverseMappedSymbol
    // TODO:
    // inferReverseMappedType
    // TODO:
    // getUnmatchedProperties
    // TODO:
    // getUnmatchedProperty
    // TODO:
    // tupleTypesDefinitelyUnrelated
    // TODO:
    // typesDefinitelyUnrelated
    // TODO:
    // getTypeFromInference
    // TODO:
    // hasSkipDirectInferenceFlag
    // TODO:
    // isFromInferenceBlockedSource
    // TODO:
    // templateLiteralTypesDefinitelyUnrelated
    // TODO:
    // isValidBigIntString
    // TODO:
    // isValidTypeForTemplateLiteralPlaceholder
    // TODO:
    // inferTypesFromTemplateLiteralType
    // TODO:
    // getStringLikeTypeForType
    // TODO:
    // inferFromLiteralPartsToTemplateLiteral
    // TODO:
    // inferTypes
    // TODO:
    // isTypeOrBaseIdenticalTo
    // TODO:
    // isTypeCloselyMatchedBy
    // TODO:
    // hasPrimitiveConstraint
    // TODO:
    // isObjectLiteralType
    // TODO:
    // isObjectOrArrayLiteralType
    // TODO:
    // unionObjectAndArrayLiteralCandidates
    // TODO:
    // getContravariantInference
    // TODO:
    // getCovariantInference
    // TODO:
    // getInferredType
    // TODO:
    // getDefaultTypeArgumentType
    // TODO:
    // getInferredTypes
    // TODO:
    // getCannotFindNameDiagnosticForName
    // TODO:
    // getResolvedSymbol
    // TODO:
    // isInTypeQuery
    // TODO:
    // getFlowCacheKey
    // TODO:
    // isMatchingReference
    // TODO:
    // getPropertyAccess
    // TODO:
    // getAccessedPropertyName
    // TODO:
    // containsMatchingReference
    // TODO:
    // optionalChainContainsReference
    // TODO:
    // isDiscriminantProperty
    // TODO:
    // findDiscriminantProperties
    // TODO:
    // mapTypesByKeyProperty
    // TODO:
    // getKeyPropertyName
    // TODO:
    // getConstituentTypeForKeyType
    // TODO:
    // getMatchingUnionConstituentForType
    // TODO:
    // getMatchingUnionConstituentForObjectLiteral
    // TODO:
    // isOrContainsMatchingReference
    // TODO:
    // hasMatchingArgument
    // TODO:
    // getFlowNodeId
    // TODO:
    // typeMaybeAssignableTo
    // TODO:
    // getAssignmentReducedType
    // TODO:
    // isFunctionObjectType
    // TODO:
    // getTypeFacts
    // TODO:
    // getTypeWithFacts
    // TODO:
    // getTypeWithDefault
    // TODO:
    // getTypeOfDestructuredProperty
    // TODO:
    // getTypeOfDestructuredArrayElement
    // TODO:
    // includeUndefinedInIndexSignature
    // TODO:
    // getTypeOfDestructuredSpreadExpression
    // TODO:
    // getAssignedTypeOfBinaryExpression
    // TODO:
    // isDestructuringAssignmentTarget
    // TODO:
    // getAssignedTypeOfArrayLiteralElement
    // TODO:
    // getAssignedTypeOfSpreadExpression
    // TODO:
    // getAssignedTypeOfPropertyAssignment
    // TODO:
    // getAssignedTypeOfShorthandPropertyAssignment
    // TODO:
    // getAssignedType
    // TODO:
    // getInitialTypeOfBindingElement
    // TODO:
    // getTypeOfInitializer
    // TODO:
    // getInitialTypeOfVariableDeclaration
    // TODO:
    // getInitialType
    // TODO:
    // isEmptyArrayAssignment
    // TODO:
    // getReferenceCandidate
    // TODO:
    // getReferenceRoot
    // TODO:
    // getTypeOfSwitchClause
    // TODO:
    // getSwitchClauseTypes
    // TODO:
    // getSwitchClauseTypeOfWitnesses
    // TODO:
    // getSwitchClauseTypeOfWitnesses
    // TODO:
    // getSwitchClauseTypeOfWitnesses
    // TODO:
    // eachTypeContainedIn
    // TODO:
    // isTypeSubsetOf
    // TODO:
    // isTypeSubsetOfUnion
    // TODO:
    // forEachType
    // TODO:
    // someType
    // TODO:
    // everyType
    // TODO:
    // everyContainedType
    // TODO:
    // filterType
    // TODO:
    // removeType
    // TODO:
    // countTypes
    // TODO:
    // mapType
    // TODO:
    // mapType
    // TODO:
    // mapType
    // TODO:
    // mapTypeWithAlias
    // TODO:
    // getConstituentCount
    // TODO:
    // extractTypesOfKind
    // TODO:
    // replacePrimitivesWithLiterals
    // TODO:
    // isIncomplete
    // TODO:
    // getTypeFromFlowType
    // TODO:
    // createFlowType
    // TODO:
    // createEvolvingArrayType
    // TODO:
    // getEvolvingArrayType
    // TODO:
    // addEvolvingArrayElementType
    // TODO:
    // createFinalArrayType
    // TODO:
    // getFinalArrayType
    // TODO:
    // finalizeEvolvingArrayType
    // TODO:
    // getElementTypeOfEvolvingArrayType
    // TODO:
    // isEvolvingArrayTypeList
    // TODO:
    // isEvolvingArrayOperationTarget
    // TODO:
    // isDeclarationWithExplicitTypeAnnotation
    // TODO:
    // getExplicitTypeOfSymbol
    // TODO:
    // getTypeOfDottedName
    // TODO:
    // getEffectsSignature
    // TODO:
    // hasTypePredicateOrNeverReturnType
    // TODO:
    // getTypePredicateArgument
    // TODO:
    // reportFlowControlError
    // TODO:
    // isReachableFlowNode
    // TODO:
    // isFalseExpression
    // TODO:
    // isReachableFlowNodeWorker
    // TODO:
    // isPostSuperFlowNode
    // TODO:
    // isConstantReference
    // TODO:
    // getFlowTypeOfReference
    // TODO:
    // getTypeOfSymbolAtLocation
    // TODO:
    // getControlFlowContainer
    // TODO:
    // isSymbolAssigned
    // TODO:
    // hasParentWithAssignmentsMarked
    // TODO:
    // markNodeAssignments
    // TODO:
    // isConstVariable
    // TODO:
    // removeOptionalityFromDeclaredType
    // TODO:
    // isConstraintPosition
    // TODO:
    // isGenericTypeWithUnionConstraint
    // TODO:
    // isGenericTypeWithoutNullableConstraint
    // TODO:
    // hasNonBindingPatternContextualTypeWithNoGenericTypes
    // TODO:
    // getNarrowableTypeForReference
    // TODO:
    // isExportOrExportExpression
    // TODO:
    // markAliasReferenced
    // TODO:
    // checkIdentifier
    // TODO:
    // isInsideFunctionOrInstancePropertyInitializer
    // TODO:
    // getPartOfForStatementContainingNode
    // TODO:
    // getEnclosingIterationStatement
    // TODO:
    // checkNestedBlockScopedBinding
    // TODO:
    // isBindingCapturedByNode
    // TODO:
    // isAssignedInBodyOfForStatement
    // TODO:
    // captureLexicalThis
    // TODO:
    // findFirstSuperCall
    // TODO:
    // classDeclarationExtendsNull
    // TODO:
    // checkThisBeforeSuper
    // TODO:
    // checkThisInStaticClassFieldInitializerInDecoratedClass
    // TODO:
    // checkThisExpression
    // TODO:
    // tryGetThisTypeAt
    // TODO:
    // getExplicitThisType
    // TODO:
    // getClassNameFromPrototypeMethod
    // TODO:
    // getTypeForThisExpressionFromJSDoc
    // TODO:
    // isInConstructorArgumentInitializer
    // TODO:
    // checkSuperExpression
    // TODO:
    // getContainingObjectLiteral
    // TODO:
    // getThisTypeArgument
    // TODO:
    // getThisTypeFromContextualType
    // TODO:
    // getContextualThisParameterType
    // TODO:
    // getContextuallyTypedParameterType
    // TODO:
    // getContextualTypeForVariableLikeDeclaration
    // TODO:
    // getContextualTypeForBindingElement
    // TODO:
    // getContextualTypeForStaticPropertyDeclaration
    // TODO:
    // getContextualTypeForInitializerExpression
    // TODO:
    // getContextualTypeForReturnExpression
    // TODO:
    // getContextualTypeForAwaitOperand
    // TODO:
    // getContextualTypeForYieldOperand
    // TODO:
    // isInParameterInitializerBeforeContainingFunction
    // TODO:
    // getContextualIterationType
    // TODO:
    // getContextualReturnType
    // TODO:
    // getContextualTypeForArgument
    // TODO:
    // getContextualTypeForArgumentAtIndex
    // TODO:
    // getContextualTypeForSubstitutionExpression
    // TODO:
    // getContextualTypeForBinaryOperand
    // TODO:
    // getSymbolForExpression
    // TODO:
    // getContextualTypeForAssignmentDeclaration
    // TODO:
    // isPossiblyAliasedThisProperty
    // TODO:
    // getContextualTypeForThisPropertyAssignment
    // TODO:
    // isCircularMappedProperty
    // TODO:
    // getTypeOfPropertyOfContextualType
    // TODO:
    // getContextualTypeForObjectLiteralMethod
    // TODO:
    // getContextualTypeForObjectLiteralElement
    // TODO:
    // getContextualTypeForElementExpression
    // TODO:
    // getContextualTypeForConditionalOperand
    // TODO:
    // getContextualTypeForChildJsxExpression
    // TODO:
    // getContextualTypeForJsxExpression
    // TODO:
    // getContextualTypeForJsxAttribute
    // TODO:
    // isPossiblyDiscriminantValue
    // TODO:
    // discriminateContextualTypeByObjectMembers
    // TODO:
    // discriminateContextualTypeByJSXAttributes
    // TODO:
    // getApparentTypeOfContextualType
    // TODO:
    // instantiateContextualType
    // TODO:
    // instantiateInstantiableTypes
    // TODO:
    // getContextualType
    // TODO:
    // getInferenceContext
    // TODO:
    // getContextualJsxElementAttributesType
    // TODO:
    // getEffectiveFirstArgumentForJsxSignature
    // TODO:
    // getJsxPropsTypeFromCallSignature
    // TODO:
    // getJsxPropsTypeForSignatureFromMember
    // TODO:
    // getStaticTypeOfReferencedJsxConstructor
    // TODO:
    // getJsxManagedAttributesFromLocatedAttributes
    // TODO:
    // getJsxPropsTypeFromClassType
    // TODO:
    // getIntersectedSignatures
    // TODO:
    // combineIntersectionThisParam
    // TODO:
    // combineIntersectionParameters
    // TODO:
    // combineSignaturesOfIntersectionMembers
    // TODO:
    // getContextualCallSignature
    // TODO:
    // isAritySmaller
    // TODO:
    // getContextualSignatureForFunctionLikeDeclaration
    // TODO:
    // getContextualSignature
    // TODO:
    // checkSpreadExpression
    // TODO:
    // checkSyntheticExpression
    // TODO:
    // hasDefaultValue
    // TODO:
    // checkArrayLiteral
    // TODO:
    // createArrayLiteralType
    // TODO:
    // isNumericName
    // TODO:
    // isNumericComputedName
    // TODO:
    // isNumericLiteralName
    // TODO:
    // checkComputedPropertyName
    // TODO:
    // isSymbolWithNumericName
    // TODO:
    // isSymbolWithSymbolName
    // TODO:
    // getObjectLiteralIndexInfo
    // TODO:
    // getImmediateAliasedSymbol
    // TODO:
    // checkObjectLiteral
    // TODO:
    // isValidSpreadType
    // TODO:
    // checkJsxSelfClosingElementDeferred
    // TODO:
    // checkJsxSelfClosingElement
    // TODO:
    // checkJsxElementDeferred
    // TODO:
    // checkJsxElement
    // TODO:
    // checkJsxFragment
    // TODO:
    // isHyphenatedJsxName
    // TODO:
    // isJsxIntrinsicIdentifier
    // TODO:
    // checkJsxAttribute
    // TODO:
    // createJsxAttributesTypeFromAttributesProperty
    // TODO:
    // checkJsxChildren
    // TODO:
    // checkSpreadPropOverrides
    // TODO:
    // checkJsxAttributes
    // TODO:
    // getJsxType
    // TODO:
    // getIntrinsicTagSymbol
    // TODO:
    // getJsxNamespaceContainerForImplicitImport
    // TODO:
    // getJsxNamespaceAt
    // TODO:
    // getNameFromJsxElementAttributesContainer
    // TODO:
    // getJsxLibraryManagedAttributes
    // TODO:
    // getJsxElementPropertiesName
    // TODO:
    // getJsxElementChildrenPropertyName
    // TODO:
    // getUninstantiatedJsxSignaturesOfType
    // TODO:
    // getIntrinsicAttributesTypeFromStringLiteralType
    // TODO:
    // checkJsxReturnAssignableToAppropriateBound
    // TODO:
    // getIntrinsicAttributesTypeFromJsxOpeningLikeElement
    // TODO:
    // getJsxElementClassTypeAt
    // TODO:
    // getJsxElementTypeAt
    // TODO:
    // getJsxStatelessElementTypeAt
    // TODO:
    // getJsxIntrinsicTagNamesAt
    // TODO:
    // checkJsxPreconditions
    // TODO:
    // checkJsxOpeningLikeElementOrOpeningFragment
    // TODO:
    // isKnownProperty
    // TODO:
    // isExcessPropertyCheckTarget
    // TODO:
    // checkJsxExpression
    // TODO:
    // getDeclarationNodeFlagsFromSymbol
    // TODO:
    // isPrototypeProperty
    // TODO:
    // checkPropertyAccessibility
    // TODO:
    // checkPropertyAccessibilityAtLocation
    // TODO:
    // getThisParameterFromNodeContext
    // TODO:
    // symbolHasNonMethodDeclaration
    // TODO:
    // checkNonNullExpression
    // TODO:
    // isNullableType
    // TODO:
    // getNonNullableTypeIfNeeded
    // TODO:
    // reportObjectPossiblyNullOrUndefinedError
    // TODO:
    // reportCannotInvokePossiblyNullOrUndefinedError
    // TODO:
    // checkNonNullTypeWithReporter
    // TODO:
    // checkNonNullType
    // TODO:
    // checkNonNullNonVoidType
    // TODO:
    // checkPropertyAccessExpression
    // TODO:
    // checkPropertyAccessChain
    // TODO:
    // checkQualifiedName
    // TODO:
    // isMethodAccessForCall
    // TODO:
    // lookupSymbolForPrivateIdentifierDeclaration
    // TODO:
    // checkGrammarPrivateIdentifierExpression
    // TODO:
    // checkPrivateIdentifierExpression
    // TODO:
    // getSymbolForPrivateIdentifierExpression
    // TODO:
    // getPrivateIdentifierPropertyOfType
    // TODO:
    // checkPrivateIdentifierPropertyAccess
    // TODO:
    // isThisPropertyAccessInConstructor
    // TODO:
    // checkPropertyAccessExpressionOrQualifiedName
    // TODO:
    // isUncheckedJSSuggestion
    // TODO:
    // getFlowTypeOfAccessExpression
    // TODO:
    // checkPropertyNotUsedBeforeDeclaration
    // TODO:
    // isInPropertyInitializerOrClassStaticBlock
    // TODO:
    // isPropertyDeclaredInAncestorClass
    // TODO:
    // getSuperClass
    // TODO:
    // reportNonexistentProperty
    // TODO:
    // containerSeemsToBeEmptyDomElement
    // TODO:
    // typeHasStaticProperty
    // TODO:
    // getSuggestedLibForNonExistentName
    // TODO:
    // getSuggestedLibForNonExistentProperty
    // TODO:
    // getSuggestedSymbolForNonexistentClassMember
    // TODO:
    // getSuggestedSymbolForNonexistentProperty
    // TODO:
    // getSuggestedSymbolForNonexistentJSXAttribute
    // TODO:
    // getSuggestionForNonexistentProperty
    // TODO:
    // getSuggestedSymbolForNonexistentSymbol
    // TODO:
    // getSuggestionForNonexistentSymbol
    // TODO:
    // getSuggestedSymbolForNonexistentModule
    // TODO:
    // getSuggestionForNonexistentExport
    // TODO:
    // getSuggestionForNonexistentIndexSignature
    // TODO:
    // getSuggestedTypeForNonexistentStringLiteralType
    // TODO:
    // getSpellingSuggestionForName
    // TODO:
    // markPropertyAsReferenced
    // TODO:
    // isSelfTypeAccess
    // TODO:
    // isValidPropertyAccess
    // TODO:
    // isValidPropertyAccessForCompletions
    // TODO:
    // isValidPropertyAccessWithType
    // TODO:
    // isPropertyAccessible
    // TODO:
    // getForInVariableSymbol
    // TODO:
    // hasNumericPropertyNames
    // TODO:
    // isForInVariableForNumericPropertyNames
    // TODO:
    // checkIndexedAccess
    // TODO:
    // checkElementAccessChain
    // TODO:
    // checkElementAccessExpression
    // TODO:
    // callLikeExpressionMayHaveTypeArguments
    // TODO:
    // resolveUntypedCall
    // TODO:
    // resolveErrorCall
    // TODO:
    // reorderCandidates
    // TODO:
    // isSpreadArgument
    // TODO:
    // getSpreadArgumentIndex
    // TODO:
    // acceptsVoid
    // TODO:
    // acceptsVoidUndefinedUnknownOrAny
    // TODO:
    // hasCorrectArity
    // TODO:
    // hasCorrectTypeArgumentArity
    // TODO:
    // getSingleCallSignature
    // TODO:
    // getSingleCallOrConstructSignature
    // TODO:
    // getSingleSignature
    // TODO:
    // instantiateSignatureInContextOf
    // TODO:
    // inferJsxTypeArguments
    // TODO:
    // getThisArgumentType
    // TODO:
    // inferTypeArguments
    // TODO:
    // getMutableArrayOrTupleType
    // TODO:
    // getSpreadArgumentType
    // TODO:
    // checkTypeArguments
    // TODO:
    // getJsxReferenceKind
    // TODO:
    // checkApplicableSignatureForJsxOpeningLikeElement
    // TODO:
    // getSignatureApplicabilityError
    // TODO:
    // getThisArgumentOfCall
    // TODO:
    // createSyntheticExpression
    // TODO:
    // getEffectiveCallArguments
    // TODO:
    // getEffectiveDecoratorArguments
    // TODO:
    // getDecoratorArgumentCount
    // TODO:
    // getDiagnosticSpanForCallNode
    // TODO:
    // getDiagnosticForCallNode
    // TODO:
    // isPromiseResolveArityError
    // TODO:
    // getArgumentArityError
    // TODO:
    // getTypeArgumentArityError
    // TODO:
    // resolveCall
    // TODO:
    // getCandidateForOverloadFailure
    // TODO:
    // createUnionOfSignaturesForOverloadFailure
    // TODO:
    // getNumNonRestParameters
    // TODO:
    // createCombinedSymbolFromTypes
    // TODO:
    // createCombinedSymbolForOverloadFailure
    // TODO:
    // pickLongestCandidateSignature
    // TODO:
    // getTypeArgumentsFromNodes
    // TODO:
    // inferSignatureInstantiationForOverloadFailure
    // TODO:
    // getLongestCandidateIndex
    // TODO:
    // resolveCallExpression
    // TODO:
    // isGenericFunctionReturningFunction
    // TODO:
    // isUntypedFunctionCall
    // TODO:
    // resolveNewExpression
    // TODO:
    // typeHasProtectedAccessibleBase
    // TODO:
    // isConstructorAccessible
    // TODO:
    // invocationErrorDetails
    // TODO:
    // invocationError
    // TODO:
    // invocationErrorRecovery
    // TODO:
    // resolveTaggedTemplateExpression
    // TODO:
    // getDiagnosticHeadMessageForDecoratorResolution
    // TODO:
    // resolveDecorator
    // TODO:
    // createSignatureForJSXIntrinsic
    // TODO:
    // resolveJsxOpeningLikeElement
    // TODO:
    // isPotentiallyUncalledDecorator
    // TODO:
    // resolveSignature
    // TODO:
    // getResolvedSignature
    // TODO:
    // isJSConstructor
    // TODO:
    // mergeJSSymbols
    // TODO:
    // getAssignedClassSymbol
    // TODO:
    // getSymbolOfExpando
    // TODO:
    // getAssignedJSPrototype
    // TODO:
    // checkCallExpression
    // TODO:
    // checkDeprecatedSignature
    // TODO:
    // getDeprecatedSuggestionNode
    // TODO:
    // isSymbolOrSymbolForCall
    // TODO:
    // checkImportCallExpression
    // TODO:
    // getTypeWithSyntheticDefaultImportType
    // TODO:
    // isCommonJsRequire
    // TODO:
    // checkTaggedTemplateExpression
    // TODO:
    // checkAssertion
    // TODO:
    // isValidConstAssertionArgument
    // TODO:
    // checkAssertionWorker
    // TODO:
    // checkNonNullChain
    // TODO:
    // checkNonNullAssertion
    // TODO:
    // checkMetaProperty
    // TODO:
    // checkMetaPropertyKeyword
    // TODO:
    // checkNewTargetMetaProperty
    // TODO:
    // checkImportMetaProperty
    // TODO:
    // getTypeOfParameter
    // TODO:
    // getTupleElementLabel
    // TODO:
    // getParameterNameAtPosition
    // TODO:
    // getParameterIdentifierNameAtPosition
    // TODO:
    // isParameterDeclarationWithIdentifierName
    // TODO:
    // isValidDeclarationForTupleLabel
    // TODO:
    // getNameableDeclarationAtPosition
    // TODO:
    // getTypeAtPosition
    // TODO:
    // tryGetTypeAtPosition
    // TODO:
    // getRestTypeAtPosition
    // TODO:
    // getParameterCount
    // TODO:
    // getMinArgumentCount
    // TODO:
    // hasEffectiveRestParameter
    // TODO:
    // getEffectiveRestType
    // TODO:
    // getNonArrayRestType
    // TODO:
    // getTypeOfFirstParameterOfSignature
    // TODO:
    // getTypeOfFirstParameterOfSignatureWithFallback
    // TODO:
    // inferFromAnnotatedParameters
    // TODO:
    // assignContextualParameterTypes
    // TODO:
    // assignNonContextualParameterTypes
    // TODO:
    // assignParameterType
    // TODO:
    // assignBindingElementTypes
    // TODO:
    // createPromiseType
    // TODO:
    // createPromiseLikeType
    // TODO:
    // createPromiseReturnType
    // TODO:
    // createNewTargetExpressionType
    // TODO:
    // getReturnTypeFromBody
    // TODO:
    // createGeneratorReturnType
    // TODO:
    // checkAndAggregateYieldOperandTypes
    // TODO:
    // getYieldedTypeOfYieldExpression
    // TODO:
    // getFactsFromTypeofSwitch
    // TODO:
    // isExhaustiveSwitchStatement
    // TODO:
    // computeExhaustiveSwitchStatement
    // TODO:
    // functionHasImplicitReturn
    // TODO:
    // checkAndAggregateReturnExpressionTypes
    // TODO:
    // mayReturnNever
    // TODO:
    // checkAllCodePathsInNonVoidFunctionReturnOrThrow
    // TODO:
    // checkFunctionExpressionOrObjectLiteralMethod
    // TODO:
    // contextuallyCheckFunctionExpressionOrObjectLiteralMethod
    // TODO:
    // checkFunctionExpressionOrObjectLiteralMethodDeferred
    // TODO:
    // checkArithmeticOperandType
    // TODO:
    // isReadonlyAssignmentDeclaration
    // TODO:
    // isReadonlySymbol
    // TODO:
    // isAssignmentToReadonlyEntity
    // TODO:
    // checkReferenceExpression
    // TODO:
    // checkDeleteExpression
    // TODO:
    // checkDeleteExpressionMustBeOptional
    // TODO:
    // checkTypeOfExpression
    // TODO:
    // checkVoidExpression
    // TODO:
    // checkAwaitExpression
    // TODO:
    // checkPrefixUnaryExpression
    // TODO:
    // checkPostfixUnaryExpression
    // TODO:
    // getUnaryResultType
    // TODO:
    // maybeTypeOfKind
    // TODO:
    // isTypeAssignableToKind
    // TODO:
    // allTypesAssignableToKind
    // TODO:
    // isConstEnumObjectType
    // TODO:
    // isConstEnumSymbol
    // TODO:
    // checkInstanceOfExpression
    // TODO:
    // checkInExpression
    // TODO:
    // checkObjectLiteralAssignment
    // TODO:
    // checkObjectLiteralDestructuringPropertyAssignment
    // TODO:
    // checkArrayLiteralAssignment
    // TODO:
    // checkArrayLiteralDestructuringElementAssignment
    // TODO:
    // checkDestructuringAssignment
    // TODO:
    // checkReferenceAssignment
    // TODO:
    // isSideEffectFree
    // TODO:
    // isTypeEqualityComparableTo
    // TODO:
    // createCheckBinaryExpression
    // TODO:
    // checkGrammarNullishCoalesceWithLogicalExpression
    // TODO:
    // checkBinaryLikeExpression
    // TODO:
    // checkBinaryLikeExpressionWorker
    // TODO:
    // getBaseTypesIfUnrelated
    // TODO:
    // checkYieldExpression
    // TODO:
    // checkConditionalExpression
    // TODO:
    // isTemplateLiteralContext
    // TODO:
    // checkTemplateExpression
    // TODO:
    // isTemplateLiteralContextualType
    // TODO:
    // getContextNode
    // TODO:
    // checkExpressionWithContextualType
    // TODO:
    // checkExpressionCached
    // TODO:
    // isTypeAssertion
    // TODO:
    // checkDeclarationInitializer
    // TODO:
    // padTupleType
    // TODO:
    // widenTypeInferredFromInitializer
    // TODO:
    // isLiteralOfContextualType
    // TODO:
    // isConstContext
    // TODO:
    // checkExpressionForMutableLocation
    // TODO:
    // checkPropertyAssignment
    // TODO:
    // checkObjectLiteralMethod
    // TODO:
    // instantiateTypeWithSingleGenericCallSignature
    // TODO:
    // skippedGenericFunction
    // TODO:
    // hasInferenceCandidates
    // TODO:
    // hasOverlappingInferences
    // TODO:
    // mergeInferences
    // TODO:
    // getUniqueTypeParameters
    // TODO:
    // hasTypeParameterByName
    // TODO:
    // getUniqueTypeParameterName
    // TODO:
    // getReturnTypeOfSingleNonGenericCallSignature
    // TODO:
    // getReturnTypeOfSingleNonGenericSignatureOfCallChain
    // TODO:
    // getTypeOfExpression
    // TODO:
    // getQuickTypeOfExpression
    // TODO:
    // getContextFreeTypeOfExpression
    // TODO:
    // checkExpression
    // TODO:
    // checkConstEnumAccess
    // TODO:
    // checkParenthesizedExpression
    // TODO:
    // checkExpressionWorker
    // TODO:
    // checkTypeParameter
    // TODO:
    // checkParameter
    // TODO:
    // checkTypePredicate
    // TODO:
    // getTypePredicateParent
    // TODO:
    // checkIfTypePredicateVariableIsDeclaredInBindingPattern
    // TODO:
    // checkSignatureDeclaration
    // TODO:
    // checkClassForDuplicateDeclarations
    // TODO:
    // checkClassForStaticPropertyNameConflicts
    // TODO:
    // checkObjectTypeForDuplicateDeclarations
    // TODO:
    // checkTypeForDuplicateIndexSignatures
    // TODO:
    // checkPropertyDeclaration
    // TODO:
    // checkPropertySignature
    // TODO:
    // checkMethodDeclaration
    // TODO:
    // setNodeLinksForPrivateIdentifierScope
    // TODO:
    // checkClassStaticBlockDeclaration
    // TODO:
    // checkConstructorDeclaration
    // TODO:
    // checkAccessorDeclaration
    // TODO:
    // checkMissingDeclaration
    // TODO:
    // getEffectiveTypeArguments
    // TODO:
    // checkTypeArgumentConstraints
    // TODO:
    // getTypeParametersForTypeReference
    // TODO:
    // checkTypeReferenceNode
    // TODO:
    // getTypeArgumentConstraint
    // TODO:
    // checkTypeQuery
    // TODO:
    // checkTypeLiteral
    // TODO:
    // checkArrayType
    // TODO:
    // checkTupleType
    // TODO:
    // checkUnionOrIntersectionType
    // TODO:
    // checkIndexedAccessIndexType
    // TODO:
    // checkIndexedAccessType
    // TODO:
    // checkMappedType
    // TODO:
    // checkThisType
    // TODO:
    // checkTypeOperator
    // TODO:
    // checkConditionalType
    // TODO:
    // checkInferType
    // TODO:
    // checkTemplateLiteralType
    // TODO:
    // checkImportType
    // TODO:
    // checkNamedTupleMember
    // TODO:
    // isPrivateWithinAmbient
    // TODO:
    // getEffectiveDeclarationFlags
    // TODO:
    // checkFunctionOrConstructorSymbol
    // TODO:
    // checkExportsOnMergedDeclarations
    // TODO:
    // getAwaitedTypeOfPromise
    // TODO:
    // getPromisedTypeOfPromise
    // TODO:
    // checkAwaitedType
    // TODO:
    // isThenableType
    // TODO:
    // isAwaitedTypeInstantiation
    // TODO:
    // unwrapAwaitedType
    // TODO:
    // createAwaitedTypeIfNeeded
    // TODO:
    // getAwaitedType
    // TODO:
    // getAwaitedTypeNoAlias
    // TODO:
    // checkAsyncFunctionReturnType
    // TODO:
    // checkDecorator
    // TODO:
    // markTypeNodeAsReferenced
    // TODO:
    // markEntityNameOrEntityExpressionAsReference
    // TODO:
    // markDecoratorMedataDataTypeNodeAsReferenced
    // TODO:
    // getEntityNameForDecoratorMetadata
    // TODO:
    // getEntityNameForDecoratorMetadataFromTypeList
    // TODO:
    // getParameterTypeNodeForDecoratorCheck
    // TODO:
    // checkDecorators
    // TODO:
    // checkFunctionDeclaration
    // TODO:
    // checkJSDocTypeAliasTag
    // TODO:
    // checkJSDocTemplateTag
    // TODO:
    // checkJSDocTypeTag
    // TODO:
    // checkJSDocParameterTag
    // TODO:
    // checkJSDocPropertyTag
    // TODO:
    // checkJSDocFunctionType
    // TODO:
    // checkJSDocImplementsTag
    // TODO:
    // checkJSDocAugmentsTag
    // TODO:
    // checkJSDocAccessibilityModifiers
    // TODO:
    // getIdentifierFromEntityNameExpression
    // TODO:
    // getIdentifierFromEntityNameExpression
    // TODO:
    // getIdentifierFromEntityNameExpression
    // TODO:
    // checkFunctionOrMethodDeclaration
    // TODO:
    // registerForUnusedIdentifiersCheck
    // TODO:
    // checkUnusedIdentifiers
    // TODO:
    // errorUnusedLocal
    // TODO:
    // isIdentifierThatStartsWithUnderscore
    // TODO:
    // checkUnusedClassMembers
    // TODO:
    // checkUnusedInferTypeParameter
    // TODO:
    // checkUnusedTypeParameters
    // TODO:
    // isTypeParameterUnused
    // TODO:
    // addToGroup
    // TODO:
    // tryGetRootParameterDeclaration
    // TODO:
    // isValidUnusedLocalDeclaration
    // TODO:
    // checkUnusedLocalsAndParameters
    // TODO:
    // bindingNameText
    // TODO:
    // isImportedDeclaration
    // TODO:
    // importClauseFromImported
    // TODO:
    // checkBlock
    // TODO:
    // checkCollisionWithArgumentsInGeneratedCode
    // TODO:
    // needCollisionCheckForIdentifier
    // TODO:
    // checkIfThisIsCapturedInEnclosingScope
    // TODO:
    // checkIfNewTargetIsCapturedInEnclosingScope
    // TODO:
    // checkCollisionWithRequireExportsInGeneratedCode
    // TODO:
    // checkCollisionWithGlobalPromiseInGeneratedCode
    // TODO:
    // recordPotentialCollisionWithWeakMapSetInGeneratedCode
    // TODO:
    // checkWeakMapSetCollision
    // TODO:
    // recordPotentialCollisionWithReflectInGeneratedCode
    // TODO:
    // checkReflectCollision
    // TODO:
    // checkCollisionsForDeclarationName
    // TODO:
    // checkVarDeclaredNamesNotShadowed
    // TODO:
    // convertAutoToAny
    // TODO:
    // checkVariableLikeDeclaration
    // TODO:
    // errorNextVariableOrPropertyDeclarationMustHaveSameType
    // TODO:
    // areDeclarationFlagsIdentical
    // TODO:
    // checkVariableDeclaration
    // TODO:
    // checkBindingElement
    // TODO:
    // checkVariableStatement
    // TODO:
    // checkExpressionStatement
    // TODO:
    // checkIfStatement
    // TODO:
    // checkTestingKnownTruthyCallableOrAwaitableType
    // TODO:
    // isSymbolUsedInConditionBody
    // TODO:
    // isSymbolUsedInBinaryExpressionChain
    // TODO:
    // checkDoStatement
    // TODO:
    // checkWhileStatement
    // TODO:
    // checkTruthinessOfType
    // TODO:
    // checkTruthinessExpression
    // TODO:
    // checkForStatement
    // TODO:
    // checkForOfStatement
    // TODO:
    // checkForInStatement
    // TODO:
    // checkForInOrForOfVariableDeclaration
    // TODO:
    // checkRightHandSideOfForOf
    // TODO:
    // checkIteratedTypeOrElementType
    // TODO:
    // getIteratedTypeOrElementType
    // TODO:
    // isES2015OrLaterIterable
    // TODO:
    // getIterationTypeOfIterable
    // TODO:
    // createIterationTypes
    // TODO:
    // combineIterationTypes
    // TODO:
    // getCachedIterationTypes
    // TODO:
    // setCachedIterationTypes
    // TODO:
    // getIterationTypesOfIterable
    // TODO:
    // getAsyncFromSyncIterationTypes
    // TODO:
    // getIterationTypesOfIterableWorker
    // TODO:
    // getIterationTypesOfIterableCached
    // TODO:
    // getIterationTypesOfGlobalIterableType
    // TODO:
    // getIterationTypesOfIterableFast
    // TODO:
    // getPropertyNameForKnownSymbolName
    // TODO:
    // getIterationTypesOfIterableSlow
    // TODO:
    // reportTypeNotIterableError
    // TODO:
    // getIterationTypesOfIterator
    // TODO:
    // getIterationTypesOfIteratorCached
    // TODO:
    // getIterationTypesOfIteratorFast
    // TODO:
    // isIteratorResult
    // TODO:
    // isYieldIteratorResult
    // TODO:
    // isReturnIteratorResult
    // TODO:
    // getIterationTypesOfIteratorResult
    // TODO:
    // getIterationTypesOfMethod
    // TODO:
    // getIterationTypesOfIteratorSlow
    // TODO:
    // getIterationTypeOfGeneratorFunctionReturnType
    // TODO:
    // getIterationTypesOfGeneratorFunctionReturnType
    // TODO:
    // checkBreakOrContinueStatement
    // TODO:
    // unwrapReturnType
    // TODO:
    // isUnwrappedReturnTypeVoidOrAny
    // TODO:
    // checkReturnStatement
    // TODO:
    // checkWithStatement
    // TODO:
    // checkSwitchStatement
    // TODO:
    // checkLabeledStatement
    // TODO:
    // checkThrowStatement
    // TODO:
    // checkTryStatement
    // TODO:
    // checkIndexConstraints
    // TODO:
    // checkIndexConstraintForProperty
    // TODO:
    // checkIndexConstraintForIndexSignature
    // TODO:
    // checkTypeNameIsReserved
    // TODO:
    // checkClassNameCollisionWithObject
    // TODO:
    // checkTypeParameters
    // TODO:
    // checkTypeParametersNotReferenced
    // TODO:
    // checkTypeParameterListsIdentical
    // TODO:
    // areTypeParametersIdentical
    // TODO:
    // checkClassExpression
    // TODO:
    // checkClassExpressionDeferred
    // TODO:
    // checkClassDeclaration
    // TODO:
    // checkClassLikeDeclaration
    // TODO:
    // checkMembersForMissingOverrideModifier
    // TODO:
    // issueMemberSpecificError
    // TODO:
    // checkBaseTypeAccessibility
    // TODO:
    // getTargetSymbol
    // TODO:
    // getClassOrInterfaceDeclarationsOfSymbol
    // TODO:
    // checkKindsOfPropertyMemberOverrides
    // TODO:
    // getNonInterhitedProperties
    // TODO:
    // checkInheritedPropertiesAreIdentical
    // TODO:
    // checkPropertyInitialization
    // TODO:
    // isPropertyWithoutInitializer
    // TODO:
    // isPropertyInitializedInStaticBlocks
    // TODO:
    // isPropertyInitializedInConstructor
    // TODO:
    // checkInterfaceDeclaration
    // TODO:
    // checkTypeAliasDeclaration
    // TODO:
    // computeEnumMemberValues
    // TODO:
    // computeMemberValue
    // TODO:
    // computeConstantValue
    // TODO:
    // isConstantMemberAccess
    // TODO:
    // checkEnumDeclaration
    // TODO:
    // checkEnumMember
    // TODO:
    // getFirstNonAmbientClassOrFunctionDeclaration
    // TODO:
    // inSameLexicalScope
    // TODO:
    // checkModuleDeclaration
    // TODO:
    // checkModuleAugmentationElement
    // TODO:
    // getFirstNonModuleExportsIdentifier
    // TODO:
    // checkExternalImportOrExportDeclaration
    // TODO:
    // checkAliasSymbol
    // TODO:
    // checkImportBinding
    // TODO:
    // checkAssertClause
    // TODO:
    // checkImportDeclaration
    // TODO:
    // checkImportEqualsDeclaration
    // TODO:
    // checkExportDeclaration
    // TODO:
    // checkGrammarExportDeclaration
    // TODO:
    // checkGrammarModuleElementContext
    // TODO:
    // importClauseContainsReferencedImport
    // TODO:
    // importClauseContainsConstEnumUsedAsValue
    // TODO:
    // canConvertImportDeclarationToTypeOnly
    // TODO:
    // canConvertImportEqualsDeclarationToTypeOnly
    // TODO:
    // checkImportsForTypeOnlyConversion
    // TODO:
    // checkExportSpecifier
    // TODO:
    // checkExportAssignment
    // TODO:
    // hasExportedMembers
    // TODO:
    // checkExternalModuleExports
    // TODO:
    // isDuplicatedCommonJSExport
    // TODO:
    // checkSourceElement
    // TODO:
    // checkSourceElementWorker
    // TODO:
    // checkJSDocTypeIsInJsFile
    // TODO:
    // checkJSDocVariadicType
    // TODO:
    // getTypeFromJSDocVariadicType
    // TODO:
    // checkNodeDeferred
    // TODO:
    // checkDeferredNodes
    // TODO:
    // checkDeferredNode
    // TODO:
    // checkSourceFile
    // TODO:
    // unusedIsError
    // TODO:
    // getPotentiallyUnusedIdentifiers
    // TODO:
    // checkSourceFileWorker
    // TODO:
    // getDiagnostics
    // TODO:
    // getDiagnosticsWorker
    // TODO:
    // getGlobalDiagnostics
    // TODO:
    // throwIfNonDiagnosticsProducing
    // TODO:
    // getSymbolsInScope
    // TODO:
    // isTypeDeclarationName
    // TODO:
    // isTypeDeclaration
    // TODO:
    // isTypeReferenceIdentifier
    // TODO:
    // isHeritageClauseElementIdentifier
    // TODO:
    // forEachEnclosingClass
    // TODO:
    // isNodeUsedDuringClassInitialization
    // TODO:
    // isNodeWithinClass
    // TODO:
    // getLeftSideOfImportEqualsOrExportAssignment
    // TODO:
    // isInRightSideOfImportOrExportAssignment
    // TODO:
    // getSpecialPropertyAssignmentSymbolFromEntityName
    // TODO:
    // isImportTypeQualifierPart
    // TODO:
    // getSymbolOfNameOrPropertyAccessExpression
    // TODO:
    // resolveJSDocMemberName
    // TODO:
    // getSymbolAtLocation
    // TODO:
    // getIndexInfosAtLocation
    // TODO:
    // getShorthandAssignmentValueSymbol
    // TODO:
    // getExportSpecifierLocalTargetSymbol

    fn getTypeOfNode(&mut self, node: BoundNode) -> TypeId {
        // TODO:
        // if (isSourceFile(node) && !isExternalModule(node)) {
        //     return errorType;
        // }

        // TODO:
        // if (node.flags & NodeFlags.InWithStatement) {
        //     // We cannot answer semantic questions within a with block, do not proceed any further
        //     return errorType;
        // }

        // TODO:
        // let classDecl = tryGetClassImplementingOrExtendingExpressionWithTypeArguments(node);
        // let classType = classDecl && getDeclaredTypeOfClassOrInterface(getSymbolOfNode(classDecl.class));
        // if isPartOfTypeNode(node) {
        //     let typeFromTypeNode = getTypeFromTypeNode(node as TypeNode);
        //     return classType ? getTypeWithThisArgument(typeFromTypeNode, classType.thisType) : typeFromTypeNode;
        // }

        // TODO:
        // if isExpressionNode(node) {
        //     return getRegularTypeOfExpression(node as Expression);
        // }

        // TODO:
        // if classType && !classDecl.isImplements {
        //     // A SyntaxKind.ExpressionWithTypeArguments is considered a type node, except when it occurs in the
        //     // extends clause of a class. We handle that case here.
        //     let baseType = firstOrUndefined(getBaseTypes(classType));
        //     return baseType ? getTypeWithThisArgument(baseType, classType.thisType) : errorType;
        // }

        // TODO:
        // if isTypeDeclaration(node) {
        //     // In this case, we call getSymbolOfNode instead of getSymbolAtLocation because it is a declaration
        //     let symbol = getSymbolOfNode(node);
        //     return getDeclaredTypeOfSymbol(symbol);
        // }

        // TODO:
        // if isTypeDeclarationName(node) {
        //     let symbol = getSymbolAtLocation(node);
        //     return symbol ? getDeclaredTypeOfSymbol(symbol) : errorType;
        // }

        if let Ok(decl) = Declaration::try_from(node.clone()) {
            todo!();
            // let symbol = self.getSymbolOfDeclaration(decl);
            // return self.getTypeOfSymbol(symbol);
        } else {
            dbg!(node);
            todo!();
        }

        // TODO:
        // if isDeclarationNameOrImportPropertyName(node) {
        //     let symbol = getSymbolAtLocation(node);
        //     if symbol {
        //         return getTypeOfSymbol(symbol);
        //     }
        //     return errorType;
        // }

        // TODO:
        // if isBindingPattern(node) {
        //     return getTypeForVariableLikeDeclaration(node.parent, /*includeOptionality*/ true) || errorType;
        // }

        // TODO:
        // if isInRightSideOfImportOrExportAssignment(node as Identifier) {
        //     let symbol = getSymbolAtLocation(node);
        //     if symbol {
        //         let declaredType = getDeclaredTypeOfSymbol(symbol);
        //         return if !isErrorType(declaredType) {declaredType}else{getTypeOfSymbol(symbol)};
        //     }
        // }

        // TODO:
        // if isMetaProperty(node.parent) && node.parent.keywordToken == node.kind {
        //     return checkMetaPropertyKeyword(node.parent);
        // }

        todo!();

        // return errorType;
    }

    // TODO:
    // getTypeOfAssignmentPattern
    // TODO:
    // getPropertySymbolOfDestructuringAssignment
    // TODO:
    // getRegularTypeOfExpression
    // TODO:
    // getParentTypeOfClassElement
    // TODO:
    // getClassElementPropertyKeyType
    // TODO:
    // getAugmentedPropertiesOfType
    // TODO:
    // typeHasCallOrConstructSignatures
    // TODO:
    // getRootSymbols
    // TODO:
    // getImmediateRootSymbols
    // TODO:
    // tryGetAliasTarget
    // TODO:
    // isArgumentsLocalBinding
    // TODO:
    // moduleExportsSomeValue
    // TODO:
    // isNameOfModuleOrEnumDeclaration
    // TODO:
    // getReferencedExportContainer
    // TODO:
    // getReferencedImportDeclaration
    // TODO:
    // isSymbolOfDestructuredElementOfCatchBinding
    // TODO:
    // isSymbolOfDeclarationWithCollidingName
    // TODO:
    // getReferencedDeclarationWithCollidingName
    // TODO:
    // isDeclarationWithCollidingName
    // TODO:
    // isValueAliasDeclaration
    // TODO:
    // isTopLevelValueImportEqualsWithEntityName
    // TODO:
    // isAliasResolvedToValue
    // TODO:
    // isConstEnumOrConstEnumOnlyModule
    // TODO:
    // isReferencedAliasDeclaration
    // TODO:
    // isImplementationOfOverload
    // TODO:
    // isRequiredInitializedParameter
    // TODO:
    // isOptionalUninitializedParameterProperty
    // TODO:
    // isOptionalUninitializedParameter
    // TODO:
    // isExpandoFunctionDeclaration
    // TODO:
    // getPropertiesOfContainerFunction
    // TODO:
    // getNodeCheckFlags
    // TODO:
    // getEnumMemberValue
    // TODO:
    // canHaveConstantValue
    // TODO:
    // getConstantValue
    // TODO:
    // isFunctionType
    // TODO:
    // getTypeReferenceSerializationKind
    // TODO:
    // createTypeOfDeclaration
    // TODO:
    // createReturnTypeOfSignatureDeclaration
    // TODO:
    // createTypeOfExpression
    // TODO:
    // hasGlobalName
    // TODO:
    // getReferencedValueSymbol
    // TODO:
    // getReferencedValueDeclaration
    // TODO:
    // isLiteralConstDeclaration
    // TODO:
    // literalTypeToNode
    // TODO:
    // createLiteralConstValue
    // TODO:
    // getJsxFactoryEntity
    // TODO:
    // getJsxFragmentFactoryEntity
    // TODO:
    // createResolver
    // TODO:
    // getExternalModuleFileFromDeclaration

    fn initializeTypeChecker(&mut self) {
        // let bind_res = Binder::bind_source_files(files);

        // dbg!(&res.node_data);
        // dbg!(&res.flow_nodes);
        // dbg!(&res.containers);
        // dbg!(&res.container_map);
        // dbg!(&res.symbols);

        // // Bind all source files and propagate errors
        // for (const file of host.getSourceFiles()) {
        //     bindSourceFile(file, compilerOptions);
        // }

        // amalgamatedDuplicates = new Map();

        let files = mem::take(&mut self.host.files);

        // Initialize global symbol table
        // let augmentations: (readonly (StringLiteral | Identifier)[])[] | undefined;
        for f in &files {
            // if (file.redirectInfo) {
            //     continue;
            // }

            let program = f.program.bind_to_opt_parent(None);

            if !f.isExternalOrCommonJsModule() {
                // It is an error for a non-external-module (i.e. script) to declare its own `globalThis`.
                // We can't use `builtinGlobals` for this due to synthetic expando-namespace generation in JS files.
                let fileGlobalThisSymbol = self
                    .node_data(program.clone())
                    .locals
                    .get(&"globalThis".into())
                    .copied();

                if let Some(sym_id) = fileGlobalThisSymbol {
                    let symbol = &self.symbols[sym_id];
                    if !symbol.declarations().is_empty() {
                        todo!();
                        // for declaration in fileGlobalThisSymbol.declarations {
                        //     diagnostics.add(createDiagnosticForNode(
                        //         declaration,
                        //         Diagnostics
                        //             .Declaration_name_conflicts_with_built_in_global_identifier_0,
                        //         "globalThis",
                        //     ));
                        // }
                    }
                }

                // TODO: all of this moving is a bit janky
                let mut target_table = mem::take(self.globals_mut());
                let source_table = mem::take(&mut self.node_data_mut(program.clone()).locals);
                self.mergeSymbolTable(&mut target_table, &source_table, false);
                *self.globals_mut() = target_table;
                self.node_data_mut(program).locals = source_table;
            }
            if !f.jsGlobalAugmentations.is_empty() {
                // TODO: all of this moving is a bit janky
                let mut target_table = mem::take(self.globals_mut());
                self.mergeSymbolTable(&mut target_table, &f.jsGlobalAugmentations, false);
                *self.globals_mut() = target_table;
            }
            // if (f.patternAmbientModules && f.patternAmbientModules.length) {
            //     patternAmbientModules = concatenate(patternAmbientModules, f.patternAmbientModules);
            // }
            // if (f.moduleAugmentations.length) {
            //     // (augmentations || (augmentations = [])).push(f.moduleAugmentations);
            // }
            // if (f.symbol && f.symbol.globalExports) {
            //     // Merge in UMD exports with first-in-wins semantics (see #9771)
            //     let source = f.symbol.globalExports;
            //     // source.forEach((sourceSymbol, id) => {
            //     //     if (!globals.has(id)) {
            //     //         globals.set(id, sourceSymbol);
            //     //     }
            //     // });
            // }
        }

        self.host.files = files;

        // // We do global augmentations separately from module augmentations (and before creating global types) because they
        // //  1. Affect global types. We won't have the correct global types until global augmentations are merged. Also,
        // //  2. Module augmentation instantiation requires creating the type of a module, which, in turn, can require
        // //       checking for an export or property on the module (if export=) which, in turn, can fall back to the
        // //       apparent type of the module - either globalObjectType or globalFunctionType - which wouldn't exist if we
        // //       did module augmentations prior to finalizing the global types.
        // if (augmentations) {
        //     // merge _global_ module augmentations.
        //     // this needs to be done after global symbol table is initialized to make sure that all ambient modules are indexed
        //     for (const list of augmentations) {
        //         for (const augmentation of list) {
        //             if (!isGlobalScopeAugmentation(augmentation.parent as ModuleDeclaration)) continue;
        //             mergeModuleAugmentation(augmentation);
        //         }
        //     }
        // }

        // Setup global builtins
        addToSymbolTable(
            self.symbols[self.globalThisSymbol].exports_mut(),
            &self.builtinGlobals,
            /*Diagnostics.Declaration_name_conflicts_with_built_in_global_identifier_0,*/
        );

        self.symbols[self.undefinedSymbol]
            .as_transient_symbol_mut()
            .symbol_links
            .ty = Some(self.undefinedWideningType);
        self.symbols[self.argumentsSymbol]
            .as_transient_symbol_mut()
            .symbol_links
            .ty = self.getGlobalType("IArguments".into(), 0, true);
        self.symbols[self.unknownSymbol]
            .as_transient_symbol_mut()
            .symbol_links
            .ty = Some(self.errorType);
        self.symbols[self.globalThisSymbol]
            .as_transient_symbol_mut()
            .symbol_links
            .ty = Some(self.createObjectType(ObjectFlags::Anonymous, Some(self.globalThisSymbol)));

        // Initialize special types
        self.globalArrayType = Some(self.getGlobalType("Array".into(), 1, true).unwrap());
        self.globalObjectType = Some(self.getGlobalType("Object".into(), 0, true).unwrap());
        self.globalFunctionType = Some(self.getGlobalType("Function".into(), 0, true).unwrap());
        // let globalCallableFunctionType = strictBindCallApply && getGlobalType("CallableFunction" .into(),  0,  true) || globalFunctionType;
        // let globalNewableFunctionType = strictBindCallApply && getGlobalType("NewableFunction" .into(),  0,  true) || globalFunctionType;
        self.globalStringType = Some(self.getGlobalType("String".into(), 0, true).unwrap());
        self.globalNumberType = Some(self.getGlobalType("Number".into(), 0, true).unwrap());
        self.globalBooleanType = Some(self.getGlobalType("Boolean".into(), 0, true).unwrap());
        self.globalRegExpType = Some(self.getGlobalType("RegExp".into(), 0, true).unwrap());
        // anyArrayType = createArrayType(anyType);

        println!(
            "self.globalArrayType: {:?}",
            &self.types[self.globalArrayType()]
        );
        println!(
            "self.globalObjectType: {:?}",
            &self.types[self.globalObjectType()]
        );
        println!(
            "self.globalFunctionType: {:?}",
            &self.types[self.globalFunctionType()]
        );
        println!(
            "self.globalStringType: {:?}",
            &self.types[self.globalStringType()]
        );
        println!(
            "self.globalNumberType: {:?}",
            &self.types[self.globalNumberType()]
        );
        println!(
            "self.globalBooleanType: {:?}",
            &self.types[self.globalBooleanType()]
        );
        println!(
            "self.globalRegExpType: {:?}",
            &self.types[self.globalRegExpType()]
        );

        // autoArrayType = createArrayType(autoType);
        // if (autoArrayType === emptyObjectType) {
        //     // autoArrayType is used as a marker, so even if global Array type is not defined, it needs to be a unique type
        //     autoArrayType = createAnonymousType(undefined, emptySymbols, emptyArray, emptyArray, emptyArray);
        // }

        // globalReadonlyArrayType = getGlobalTypeOrUndefined("ReadonlyArray" .into(),  1) as GenericType || globalArrayType;
        // anyReadonlyArrayType = globalReadonlyArrayType ? createTypeFromGenericGlobalType(globalReadonlyArrayType, [anyType]) : anyArrayType;
        // globalThisType = getGlobalTypeOrUndefined("ThisType" .into(),  1) as GenericType;

        // if (augmentations) {
        //     // merge _nonglobal_ module augmentations.
        //     // this needs to be done after global symbol table is initialized to make sure that all ambient modules are indexed
        //     for (const list of augmentations) {
        //         for (const augmentation of list) {
        //             if (isGlobalScopeAugmentation(augmentation.parent as ModuleDeclaration)) continue;
        //             mergeModuleAugmentation(augmentation);
        //         }
        //     }
        // }

        // amalgamatedDuplicates.forEach(({ firstFile, secondFile, conflictingSymbols }) => {
        //     // If not many things conflict, issue individual errors
        //     if (conflictingSymbols.size < 8) {
        //         conflictingSymbols.forEach(({ isBlockScoped, firstFileLocations, secondFileLocations }, symbolName) => {
        //             const message = isBlockScoped ? Diagnostics.Cannot_redeclare_block_scoped_variable_0 : Diagnostics.Duplicate_identifier_0;
        //             for (const node of firstFileLocations) {
        //                 addDuplicateDeclarationError(node, message, symbolName, secondFileLocations);
        //             }
        //             for (const node of secondFileLocations) {
        //                 addDuplicateDeclarationError(node, message, symbolName, firstFileLocations);
        //             }
        //         });
        //     }
        //     else {
        //         // Otherwise issue top-level error since the files appear very identical in terms of what they contain
        //         const list = arrayFrom(conflictingSymbols.keys()).join(", ");
        //         diagnostics.add(addRelatedInfo(
        //             createDiagnosticForNode(firstFile, Diagnostics.Definitions_of_the_following_identifiers_conflict_with_those_in_another_file_Colon_0, list),
        //             createDiagnosticForNode(secondFile, Diagnostics.Conflicts_are_in_this_file)
        //         ));
        //         diagnostics.add(addRelatedInfo(
        //             createDiagnosticForNode(secondFile, Diagnostics.Definitions_of_the_following_identifiers_conflict_with_those_in_another_file_Colon_0, list),
        //             createDiagnosticForNode(firstFile, Diagnostics.Conflicts_are_in_this_file)
        //         ));
        //     }
        // });
        // amalgamatedDuplicates = undefined;
    }

    // TODO:
    // checkExternalEmitHelpers
    // TODO:
    // getHelperName
    // TODO:
    // resolveHelpersModule
    // TODO:
    // checkGrammarDecoratorsAndModifiers
    // TODO:
    // checkGrammarDecorators
    // TODO:
    // checkGrammarModifiers
    // TODO:
    // reportObviousModifierErrors
    // TODO:
    // shouldReportBadModifier
    // TODO:
    // nodeHasAnyModifiersExcept
    // TODO:
    // checkGrammarAsyncModifier
    // TODO:
    // checkGrammarForDisallowedTrailingComma
    // TODO:
    // checkGrammarTypeParameterList
    // TODO:
    // checkGrammarParameterList
    // TODO:
    // getNonSimpleParameters
    // TODO:
    // checkGrammarForUseStrictSimpleParameterList
    // TODO:
    // checkGrammarFunctionLikeDeclaration
    // TODO:
    // checkGrammarClassLikeDeclaration
    // TODO:
    // checkGrammarArrowFunction
    // TODO:
    // checkGrammarIndexSignatureParameters
    // TODO:
    // checkGrammarIndexSignature
    // TODO:
    // checkGrammarForAtLeastOneTypeArgument
    // TODO:
    // checkGrammarTypeArguments
    // TODO:
    // checkGrammarTaggedTemplateChain
    // TODO:
    // checkGrammarForOmittedArgument
    // TODO:
    // checkGrammarArguments
    // TODO:
    // checkGrammarHeritageClause
    // TODO:
    // checkGrammarExpressionWithTypeArguments
    // TODO:
    // checkGrammarClassDeclarationHeritageClauses
    // TODO:
    // checkGrammarInterfaceDeclaration
    // TODO:
    // checkGrammarComputedPropertyName
    // TODO:
    // checkGrammarForGenerator
    // TODO:
    // checkGrammarForInvalidQuestionMark
    // TODO:
    // checkGrammarForInvalidExclamationToken
    // TODO:
    // checkGrammarObjectLiteralExpression
    // TODO:
    // checkGrammarJsxElement
    // TODO:
    // checkGrammarJsxName
    // TODO:
    // checkGrammarJsxExpression
    // TODO:
    // checkGrammarForInOrForOfStatement
    // TODO:
    // checkGrammarAccessor
    // TODO:
    // doesAccessorHaveCorrectParameterCount
    // TODO:
    // getAccessorThisParameter
    // TODO:
    // checkGrammarTypeOperatorNode
    // TODO:
    // checkGrammarForInvalidDynamicName
    // TODO:
    // checkGrammarMethod
    // TODO:
    // checkGrammarBreakOrContinueStatement
    // TODO:
    // checkGrammarBindingElement
    // TODO:
    // isStringOrNumberLiteralExpression
    // TODO:
    // isBigIntLiteralExpression
    // TODO:
    // isSimpleLiteralEnumReference
    // TODO:
    // checkAmbientInitializer
    // TODO:
    // checkGrammarVariableDeclaration
    // TODO:
    // checkESModuleMarker
    // TODO:
    // checkGrammarNameInLetOrConstDeclarations
    // TODO:
    // checkGrammarVariableDeclarationList
    // TODO:
    // allowLetAndConstDeclarations
    // TODO:
    // checkGrammarForDisallowedLetOrConstStatement
    // TODO:
    // checkGrammarMetaProperty
    // TODO:
    // hasParseDiagnostics
    // TODO:
    // grammarErrorOnFirstToken
    // TODO:
    // grammarErrorAtPos
    // TODO:
    // grammarErrorOnNodeSkippedOn
    // TODO:
    // grammarErrorOnNode
    // TODO:
    // checkGrammarConstructorTypeParameters
    // TODO:
    // checkGrammarConstructorTypeAnnotation
    // TODO:
    // checkGrammarProperty
    // TODO:
    // checkGrammarTopLevelElementForRequiredDeclareModifier
    // TODO:
    // checkGrammarTopLevelElementsForRequiredDeclareModifier
    // TODO:
    // checkGrammarSourceFile
    // TODO:
    // checkGrammarStatementInAmbientContext
    // TODO:
    // checkGrammarNumericLiteral
    // TODO:
    // checkNumericLiteralValueSize
    // TODO:
    // checkGrammarBigIntLiteral
    // TODO:
    // grammarErrorAfterFirstToken
    // TODO:
    // getAmbientModules
    // TODO:
    // checkGrammarImportClause
    // TODO:
    // checkGrammarNamedImportsOrExports
    // TODO:
    // checkGrammarImportCallExpression
    // TODO:
    // findMatchingTypeReferenceOrTypeAliasReference
    // TODO:
    // findBestTypeForObjectLiteral
    // TODO:
    // findBestTypeForInvokable
    // TODO:
    // findMostOverlappyType
    // TODO:
    // filterPrimitivesIfContainsNonPrimitive
    // TODO:
    // findMatchingDiscriminantType
}

fn addToSymbolTable(
    target: &mut SymbolTable,
    source: &SymbolTable,
    // message: DiagnosticMessage,
) {
    for (id, sourceSymbol) in source.iter() {
        match target.entry(id.clone()) {
            Entry::Occupied(existing) => {
                // Error on redeclarations
                todo!("Error on redeclarations");
                // forEach(
                //     targetSymbol.declarations,
                //     addDeclarationDiagnostic(unescapeLeadingUnderscores(id), message),
                // );
            }
            Entry::Vacant(slot) => {
                slot.insert(*sourceSymbol);
            }
        }
    }

    // fn addDeclarationDiagnostic(id: string, message: DiagnosticMessage) {
    //     return (declaration: Declaration) => diagnostics.add(createDiagnosticForNode(declaration, message, id));
    // }
}

fn getExcludedSymbolFlags(flags: SymbolFlags) -> SymbolFlags {
    let mut result = SymbolFlags::empty();
    if flags.intersects(SymbolFlags::BlockScopedVariable) {
        result |= SymbolFlags::BlockScopedVariableExcludes
    }
    if flags.intersects(SymbolFlags::FunctionScopedVariable) {
        result |= SymbolFlags::FunctionScopedVariableExcludes
    }
    if flags.intersects(SymbolFlags::Property) {
        result |= SymbolFlags::PropertyExcludes
    }
    if flags.intersects(SymbolFlags::EnumMember) {
        result |= SymbolFlags::EnumMemberExcludes
    }
    if flags.intersects(SymbolFlags::Function) {
        result |= SymbolFlags::FunctionExcludes
    }
    if flags.intersects(SymbolFlags::Class) {
        result |= SymbolFlags::ClassExcludes
    }
    if flags.intersects(SymbolFlags::Interface) {
        result |= SymbolFlags::InterfaceExcludes
    }
    if flags.intersects(SymbolFlags::RegularEnum) {
        result |= SymbolFlags::RegularEnumExcludes
    }
    if flags.intersects(SymbolFlags::ConstEnum) {
        result |= SymbolFlags::ConstEnumExcludes
    }
    if flags.intersects(SymbolFlags::ValueModule) {
        result |= SymbolFlags::ValueModuleExcludes
    }
    if flags.intersects(SymbolFlags::Method) {
        result |= SymbolFlags::MethodExcludes
    }
    if flags.intersects(SymbolFlags::GetAccessor) {
        result |= SymbolFlags::GetAccessorExcludes
    }
    if flags.intersects(SymbolFlags::SetAccessor) {
        result |= SymbolFlags::SetAccessorExcludes
    }
    if flags.intersects(SymbolFlags::TypeParameter) {
        result |= SymbolFlags::TypeParameterExcludes
    }
    if flags.intersects(SymbolFlags::TypeAlias) {
        result |= SymbolFlags::TypeAliasExcludes
    }
    if flags.intersects(SymbolFlags::Alias) {
        result |= SymbolFlags::AliasExcludes
    }
    result
}

fn getIsDeferredContext(location: &BoundNode, lastLocation: &Option<BoundNode>) -> bool {
    todo!();
    // if (location.kind !== SyntaxKind.ArrowFunction && location.kind !== SyntaxKind.FunctionExpression) {
    //     // initializers in instance property declaration of class like entities are executed in constructor and thus deferred
    //     return isTypeQueryNode(location) || ((
    //         isFunctionLikeDeclaration(location) ||
    //         (location.kind === SyntaxKind.PropertyDeclaration && !isStatic(location))
    //     ) && (!lastLocation || lastLocation !== (location as SignatureDeclaration | PropertyDeclaration).name)); // A name is evaluated within the enclosing scope - so it shouldn't count as deferred
    // }
    // if (lastLocation && lastLocation === (location as FunctionExpression | ArrowFunction).name) {
    //     return false;
    // }
    // // generator functions and async functions are not inlined in control flow when immediately invoked
    // if ((location as FunctionExpression | ArrowFunction).asteriskToken || hasSyntacticModifier(location, ModifierFlags.Async)) {
    //     return true;
    // }
    // return !getImmediatelyInvokedFunctionExpression(location);
}

fn isSelfReferenceLocation(node: BoundNode) -> bool {
    // ModuleDeclaration is for `namespace N { N; }`
    // match node {
    //     BoundNode::FunctionDeclaration()
    //     | BoundNode::ClassDeclaration()
    //     | BoundNode::InterfaceDeclaration()
    //     | BoundNode::EnumDeclaration()
    //     | BoundNode::TypeAliasDeclaration()
    //     | BoundNode::ModuleDeclaration() => true,
    //     _ => false,
    // }
    todo!();
}

fn findConstructorDeclaration(node: &BoundNode) -> Option<BoundNode> {
    let members = match &node {
        BoundNode::ClassExpr(e) => &e.class.body,
        BoundNode::ClassDecl(d) => &d.class.body,
        _ => unreachable!(),
    };

    for member in members {
        if let ast::ClassMember::Constructor(constructor) = member {
            if constructor.body.is_some() {
                return Some(constructor.bind(node.clone()));
            }
        }
    }

    None
}
