use crate::node::BoundNode;
use crate::types::*;
use ahash::AHashMap;
use bitflags::bitflags;
use num_bigint::BigInt as BigIntValue;
use swc_atoms::JsWord;

// Properties common to all types
// pub struct Type {
// pub flags: TypeFlags, // Flags
// pub symbol: SymbolId, // Symbol associated with type (if any)
// // pub pattern?: DestructuringPattern,  // Destructuring pattern represented by type (if any)
// pub aliasSymbol: Option<SymbolId>, // Alias associated with type
// pub aliasTypeArguments: Vec<TypeId>, // Alias type arguments (if any)
// pub aliasTypeArgumentsContainsMarker: bool, // Alias type arguments (if any)
// pub permissiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to wildcard type
// pub restrictiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to unconstrained form
// pub immediateBaseConstraint: Option<TypeId>,  // Immediate base constraint cache
// pub widened: Option<TypeId>,                  // Cached widened form of the type
// }

pub struct Type {
    pub kind: TypeKind,
    pub flags: TypeFlags,         // Flags
    pub symbol: Option<SymbolId>, // Symbol associated with type (if any)
    // pub pattern?: DestructuringPattern,  // Destructuring pattern represented by type (if any)
    pub aliasSymbol: Option<SymbolId>, // Alias associated with type
    pub aliasTypeArguments: Vec<TypeId>, // Alias type arguments (if any)
    pub aliasTypeArgumentsContainsMarker: bool, // Alias type arguments (if any)
    pub permissiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to wildcard type
    pub restrictiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to unconstrained form
    pub immediateBaseConstraint: Option<TypeId>,  // Immediate base constraint cache
    pub widened: Option<TypeId>,                  // Cached widened form of the type
}

impl Type {
    pub fn new(type_flags: TypeFlags, symbol: Option<SymbolId>, kind: TypeKind) -> Type {
        Self {
            kind,
            flags: type_flags,
            symbol,
            aliasSymbol: None,
            aliasTypeArguments: Vec::new(),
            aliasTypeArgumentsContainsMarker: false,
            permissiveInstantiation: None,
            restrictiveInstantiation: None,
            immediateBaseConstraint: None,
            widened: None,
        }
    }
}

pub enum TypeKind {
    // TODO: remove "type" suffix from varient names
    TypeParameter(TypeParameter),
    FreshableIntrinsicType(FreshableIntrinsicType),
    NullableType(NullableType),
    UniqueESSymbolType(UniqueESSymbolType),
    StringLiteralType(StringLiteralType),
    NumberLiteralType(NumberLiteralType),
    BigIntLiteralType(BigIntLiteralType),
    EnumType(EnumType),
    InterfaceTypeWithDeclaredMembers(InterfaceTypeWithDeclaredMembers),
    DeferredTypeReference(DeferredTypeReference),
    TupleType(TupleType),
    TupleTypeReference(TupleTypeReference),
    UnionType(UnionType),
    IntersectionType(IntersectionType),
    MappedType(MappedType),
    EvolvingArrayType(EvolvingArrayType),
    ReverseMappedType(ReverseMappedType),
    ResolvedType(ResolvedType),
    FreshObjectLiteralType(FreshObjectLiteralType),
    // Todo: neccesary? remove?
    IterableOrIteratorType(IterableOrIteratorType),
    PromiseOrAwaitableType(PromiseOrAwaitableType),
    SyntheticDefaultModuleType(SyntheticDefaultModuleType),
    IndexedAccessType(IndexedAccessType),
    IndexType(IndexType),
    ConditionalType(ConditionalType),
    TemplateLiteralType(TemplateLiteralType),
    StringMappingType(StringMappingType),
    SubstitutionType(SubstitutionType),
    IntrinsicType(IntrinsicType),
    GenericType(GenericType),
}

// Intrinsic types (TypeFlags.Intrinsic)
// pub struct  IntrinsicType extends Type {
//     pub intrinsicName: JsWord,        // Name of intrinsic type
//     pub objectFlags: ObjectFlags,
// }

pub struct IntrinsicType {
    pub intrinsicName: JsWord, // Name of intrinsic type
    pub objectFlags: ObjectFlags,
}

impl IntrinsicType {
    pub fn new(intrinsicName: JsWord, objectFlags: ObjectFlags) -> IntrinsicType {
        Self {
            intrinsicName,
            objectFlags,
        }
    }
}

// pub struct  NullableType extends IntrinsicType {
//     objectFlags: ObjectFlags,
// }

pub struct NullableType {
    pub intrinsicName: JsWord, // Name of intrinsic type
    pub objectFlags: ObjectFlags,
}

// pub struct  FreshableIntrinsicType extends IntrinsicType {
//     freshType: IntrinsicType,     // Fresh version of type
//     regularType: IntrinsicType,   // Regular version of type
// }

pub struct FreshableIntrinsicType {
    pub freshType: TypeId,   // Fresh version of type
    pub regularType: TypeId, // Regular version of type

    pub intrinsicName: JsWord, // Name of intrinsic type
    pub objectFlags: ObjectFlags,
}

impl FreshableIntrinsicType {
    pub fn new(
        intrinsicName: JsWord,
        objectFlags: ObjectFlags,
        freshType: TypeId,
        regularType: TypeId,
    ) -> FreshableIntrinsicType {
        Self {
            freshType,
            regularType,

            intrinsicName,
            objectFlags,
        }
    }
}

// export type FreshableType = LiteralType | FreshableIntrinsicType;

// String literal types (TypeFlags.StringLiteral)
// Numeric literal types (TypeFlags.NumberLiteral)
// BigInt literal types (TypeFlags.BigIntLiteral)
// pub struct  LiteralType extends Type {
//     freshType: LiteralType,                // Fresh version of type
//     regularType: LiteralType,              // Regular version of type
// }

// pub struct LiteralType {
//     freshType: LiteralType,   // Fresh version of type
//     regularType: LiteralType, // Regular version of type

//     pub flags: TypeFlags, // Flags
//     pub symbol: SymbolId, // Symbol associated with type (if any)
//     // pub pattern?: DestructuringPattern,  // Destructuring pattern represented by type (if any)
//     pub aliasSymbol: Option<SymbolId>, // Alias associated with type
//     pub aliasTypeArguments: Vec<TypeId>, // Alias type arguments (if any)
//     pub aliasTypeArgumentsContainsMarker: bool, // Alias type arguments (if any)
//     pub permissiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to wildcard type
//     pub restrictiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to unconstrained form
//     pub immediateBaseConstraint: Option<TypeId>,  // Immediate base constraint cache
//     pub widened: Option<TypeId>,                  // Cached widened form of the type
// }

// Unique symbol types (TypeFlags.UniqueESSymbol)
// pub struct  UniqueESSymbolType extends Type {
//     symbol: Symbol,
//     escapedName: __String,
// }

pub struct UniqueESSymbolType {
    symbol: Symbol,
    escapedName: JsWord,
}

// pub struct  StringLiteralType extends LiteralType {
//     value: string;
// }

pub struct StringLiteralType {
    pub value: JsWord,

    pub freshType: TypeId,   // Fresh version of type
    pub regularType: TypeId, // Regular version of type
}

// pub struct  NumberLiteralType extends LiteralType {
//     value: number;
// }

pub struct NumberLiteralType {
    pub value: f64,

    pub freshType: TypeId,   // Fresh version of type
    pub regularType: TypeId, // Regular version of type
}

// pub struct  BigIntLiteralType extends LiteralType {
//     value: PseudoBigInt;
// }

pub struct BigIntLiteralType {
    pub value: BigIntValue,

    pub freshType: TypeId,   // Fresh version of type
    pub regularType: TypeId, // Regular version of type
}

// // Enum types (TypeFlags.Enum)
// pub struct  EnumType extends Type {
// }

pub struct EnumType {}

// export type ObjectFlagsType = NullableType | ObjectType | UnionType | IntersectionType;

// // Object types (TypeFlags.ObjectType)
// pub struct  ObjectType extends Type {
//     objectFlags: ObjectFlags;
//     /* @internal */ members?: SymbolTable;             // Properties by name
//     /* @internal */ properties?: Symbol[];             // Properties
//     /* @internal */ callSignatures?: readonly Signature[];      // Call signatures of type
//     /* @internal */ constructSignatures?: readonly Signature[]; // Construct signatures of type
//     /* @internal */ indexInfos?: readonly IndexInfo[];  // Index signatures
//     /* @internal */ objectTypeWithoutAbstractConstructSignatures?: ObjectType;
// }

// pub struct ObjectType {
//     objectFlags: ObjectFlags,
//     members: SymbolTable,                // Properties by name
//     properties: Vec<SymbolId>,           // Properties
//     callSignatures: Vec<Signature>,      // Call signatures of type
//     constructSignatures: Vec<Signature>, // Construct signatures of type
//     indexInfos: Vec<IndexInfo>,          // Index signatures
//     objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,

//     pub flags: TypeFlags, // Flags
//     pub symbol: SymbolId, // Symbol associated with type (if any)
//     // pub pattern?: DestructuringPattern,  // Destructuring pattern represented by type (if any)
//     pub aliasSymbol: Option<SymbolId>, // Alias associated with type
//     pub aliasTypeArguments: Vec<TypeId>, // Alias type arguments (if any)
//     pub aliasTypeArgumentsContainsMarker: bool, // Alias type arguments (if any)
//     pub permissiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to wildcard type
//     pub restrictiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to unconstrained form
//     pub immediateBaseConstraint: Option<TypeId>,  // Immediate base constraint cache
//     pub widened: Option<TypeId>,                  // Cached widened form of the type
// }

// /** Class and interface types (ObjectFlags.Class and ObjectFlags.Interface). */
// pub struct  InterfaceType extends ObjectType {
//     typeParameters: TypeParameter[] | undefined;      // Type parameters (undefined if non-generic)
//     outerTypeParameters: TypeParameter[] | undefined; // Outer type parameters (undefined if none)
//     localTypeParameters: TypeParameter[] | undefined; // Local type parameters (undefined if none)
//     thisType: TypeParameter | undefined;              // The "this" type (undefined if none)
//     /* @internal */
//     resolvedBaseConstructorType?: Type;               // Resolved base constructor type of class
//     /* @internal */
//     resolvedBaseTypes: BaseType[];                    // Resolved base types
//     /* @internal */
//     baseTypesResolved?: boolean;
// }

// pub struct InterfaceType {
//     typeParameters: Option<Vec<TypeParameter>>, // Type parameters (undefined if non-generic)
//     outerTypeParameters: Option<Vec<TypeParameter>>, // Outer type parameters (undefined if none)
//     localTypeParameters: Option<Vec<TypeParameter>>, // Local type parameters (undefined if none)
//     thisType: Option<TypeParameter>,            // The "this" type (undefined if none)
//     /* @internal */
//     resolvedBaseConstructorType: Option<Type>, // Resolved base constructor type of class
//     /* @internal */
//     resolvedBaseTypes: Vec<BaseType>, // Resolved base types
//     /* @internal */
//     baseTypesResolved: bool,

//     objectFlags: ObjectFlags,
//     members: SymbolTable,                // Properties by name
//     properties: Vec<SymbolId>,           // Properties
//     callSignatures: Vec<Signature>,      // Call signatures of type
//     constructSignatures: Vec<Signature>, // Construct signatures of type
//     indexInfos: Vec<IndexInfo>,          // Index signatures
//     objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,

//     pub flags: TypeFlags, // Flags
//     pub symbol: SymbolId, // Symbol associated with type (if any)
//     // pub pattern?: DestructuringPattern,  // Destructuring pattern represented by type (if any)
//     pub aliasSymbol: Option<SymbolId>, // Alias associated with type
//     pub aliasTypeArguments: Vec<TypeId>, // Alias type arguments (if any)
//     pub aliasTypeArgumentsContainsMarker: bool, // Alias type arguments (if any)
//     pub permissiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to wildcard type
//     pub restrictiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to unconstrained form
//     pub immediateBaseConstraint: Option<TypeId>,  // Immediate base constraint cache
//     pub widened: Option<TypeId>,                  // Cached widened form of the type
// }

// // Object type or intersection of object types
// export type BaseType = ObjectType | IntersectionType | TypeVariable; // Also `any` and `object`
pub enum BaseType {
    ObjectType(TypeId),
    IntersectionType(TypeId),
    TypeVariable(TypeId),
    // Also `any` and `object`
}

// pub struct  InterfaceTypeWithDeclaredMembers extends InterfaceType {
//     declaredProperties: Symbol[];                   // Declared members
//     declaredCallSignatures: Signature[];            // Declared call signatures
//     declaredConstructSignatures: Signature[];       // Declared construct signatures
//     declaredIndexInfos: IndexInfo[];                // Declared index signatures
// }

pub struct InterfaceTypeWithDeclaredMembers {
    declaredProperties: Vec<SymbolId>,             // Declared members
    declaredCallSignatures: Vec<SignatureId>,      // Declared call signatures
    declaredConstructSignatures: Vec<SignatureId>, // Declared construct signatures
    declaredIndexInfos: Vec<IndexInfo>,            // Declared index signatures

    typeParameters: Option<Vec<TypeId>>, // Type parameters (undefined if non-generic)
    outerTypeParameters: Option<Vec<TypeId>>, // Outer type parameters (undefined if none)
    localTypeParameters: Option<Vec<TypeId>>, // Local type parameters (undefined if none)
    thisType: Option<TypeId>,            // The "this" type (undefined if none)
    /* @internal */
    resolvedBaseConstructorType: Option<TypeId>, // Resolved base constructor type of class
    /* @internal */
    resolvedBaseTypes: Vec<BaseType>, // Resolved base types
    /* @internal */
    baseTypesResolved: bool,

    objectFlags: ObjectFlags,
    members: SymbolTable,                  // Properties by name
    properties: Vec<SymbolId>,             // Properties
    callSignatures: Vec<SignatureId>,      // Call signatures of type
    constructSignatures: Vec<SignatureId>, // Construct signatures of type
    indexInfos: Vec<IndexInfo>,            // Index signatures
    objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

// /**
//  * Type references (ObjectFlags.Reference). When a class or interface has type parameters or
//  * a "this" type, references to the class or interface are made using type references. The
//  * typeArguments property specifies the types to substitute for the type parameters of the
//  * class or interface and optionally includes an extra element that specifies the type to
//  * substitute for "this" in the resulting instantiation. When no extra argument is present,
//  * the type reference itself is substituted for "this". The typeArguments property is undefined
//  * if the class or interface has no type parameters and the reference isn't specifying an
//  * explicit "this" argument.
//  */
// pub struct  TypeReference extends ObjectType {
//     target: GenericType;    // Type reference target
//     node?: TypeReferenceNode | ArrayTypeNode | TupleTypeNode;
//     /* @internal */
//     mapper?: TypeMapper;
//     /* @internal */
//     resolvedTypeArguments?: readonly Type[];  // Resolved type reference type arguments
//     /* @internal */
//     literalType?: TypeReference;  // Clone of type with ObjectFlags.ArrayLiteral set
//     /* @internal */
//     cachedEquivalentBaseType?: Type; // Only set on references to class or interfaces with a single base type and no augmentations
// }

pub struct TypeReference {
    target: TypeId, // Type reference target
    // TODO:
    // node: Option<TypeReferenceNode | ArrayTypeNode | TupleTypeNode>,
    node: Option<BoundNode>,
    /* @internal */
    mapper: Option<TypeMapper>,
    /* @internal */
    resolvedTypeArguments: Option<Vec<TypeId>>, // Resolved type reference type arguments
    /* @internal */
    literalType: Option<TypeId>, // Clone of type with ObjectFlags.ArrayLiteral set
    /* @internal */
    cachedEquivalentBaseType: Option<TypeId>, // Only set on references to class or interfaces with a single base type and no augmentations

    objectFlags: ObjectFlags,
    members: SymbolTable,                  // Properties by name
    properties: Vec<SymbolId>,             // Properties
    callSignatures: Vec<SignatureId>,      // Call signatures of type
    constructSignatures: Vec<SignatureId>, // Construct signatures of type
    indexInfos: Vec<IndexInfo>,            // Index signatures
    objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

// pub struct  DeferredTypeReference extends TypeReference {
//     /* @internal */
//     node: TypeReferenceNode | ArrayTypeNode | TupleTypeNode;
//     /* @internal */
//     mapper?: TypeMapper;
//     /* @internal */
//     instantiations?: ESMap<string, Type>; // Instantiations of generic type alias (undefined if non-generic)
// }

pub struct DeferredTypeReference {
    // TODO:
    // node: TypeReferenceNode | ArrayTypeNode | TupleTypeNode,
    node: BoundNode,
    /* @internal */
    instantiations: AHashMap<JsWord, TypeId>, // Instantiations of generic type alias (undefined if non-generic)

    target: TypeId, // Type reference target
    /* @internal */
    mapper: Option<TypeMapper>,
    /* @internal */
    resolvedTypeArguments: Option<Vec<TypeId>>, // Resolved type reference type arguments
    /* @internal */
    literalType: Option<TypeId>, // Clone of type with ObjectFlags.ArrayLiteral set
    /* @internal */
    cachedEquivalentBaseType: Option<TypeId>, // Only set on references to class or interfaces with a single base type and no augmentations

    objectFlags: ObjectFlags,
    members: SymbolTable,                  // Properties by name
    properties: Vec<SymbolId>,             // Properties
    callSignatures: Vec<SignatureId>,      // Call signatures of type
    constructSignatures: Vec<SignatureId>, // Construct signatures of type
    indexInfos: Vec<IndexInfo>,            // Index signatures
    objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

bitflags! {
    pub struct VarianceFlags: u8 {
        const Invariant     =      0;  // Neither covariant nor contravariant
        const Covariant     = 1 << 0;  // Covariant
        const Contravariant = 1 << 1;  // Contravariant
        const Bivariant     = Self::Covariant.bits | Self::Contravariant.bits;  // Both covariant and contravariant
        const Independent   = 1 << 2;  // Unwitnessed type parameter
        const VarianceMask  = Self::Invariant.bits | Self::Covariant.bits | Self::Contravariant.bits | Self::Independent.bits; // Mask containing all measured variances without the unmeasurable flag
        const Unmeasurable  = 1 << 3;  // Variance result is unusable - relationship relies on structural comparisons which are not reflected in generic relationships
        const Unreliable    = 1 << 4;  // Variance result is unreliable - checking may produce false negatives, but not false positives
        const AllowsStructuralFallback = Self::Unmeasurable.bits | Self::Unreliable.bits;
    }
}

// // Generic class and interface types
// pub struct  GenericType extends InterfaceType, TypeReference {
//     /* @internal */
//     instantiations: ESMap<string, TypeReference>;  // Generic instantiation cache
//     /* @internal */
//     variances?: VarianceFlags[];  // Variance of each type parameter
// }

pub struct GenericType {
    instantiations: AHashMap<JsWord, TypeId>, // Generic instantiation cache
    variances: Vec<VarianceFlags>,            // Variance of each type parameter

    // target: TypeId, // Type reference target
    // // TODO:
    // // node: Option<TypeReferenceNode | ArrayTypeNode | TupleTypeNode>,
    // node: Option<BoundNode>,
    // mapper: Option<TypeMapper>,
    // resolvedTypeArguments: Option<Vec<TypeId>>, // Resolved type reference type arguments
    // literalType: Option<TypeId>,                // Clone of type with ObjectFlags.ArrayLiteral set
    // cachedEquivalentBaseType: Option<TypeId>, // Only set on references to class or interfaces with a single base type and no augmentations

    // typeParameters: Option<Vec<TypeId>>, // Type parameters (undefined if non-generic)
    // outerTypeParameters: Option<Vec<TypeId>>, // Outer type parameters (undefined if none)
    // localTypeParameters: Option<Vec<TypeId>>, // Local type parameters (undefined if none)
    // thisType: Option<TypeId>,            // The "this" type (undefined if none)
    // resolvedBaseConstructorType: Option<TypeId>, // Resolved base constructor type of class
    // resolvedBaseTypes: Vec<BaseType>,    // Resolved base types
    // baseTypesResolved: bool,
    objectFlags: ObjectFlags,
    // members: SymbolTable,                  // Properties by name
    // properties: Vec<SymbolId>,             // Properties
    // callSignatures: Vec<SignatureId>,      // Call signatures of type
    // constructSignatures: Vec<SignatureId>, // Construct signatures of type
    // indexInfos: Vec<IndexInfo>,            // Index signatures
    // objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

impl GenericType {
    pub fn new(objectFlags: ObjectFlags) -> GenericType {
        Self {
            instantiations: Default::default(),
            variances: Vec::new(),

            objectFlags,
        }
    }
}

bitflags! {
    pub struct ElementFlags : u8{
        const Required    = 1 << 0;  // T
        const Optional    = 1 << 1;  // T?
        const Rest        = 1 << 2;  // ...T[]
        const Variadic    = 1 << 3;  // ...T
        const Fixed       = Self::Required.bits | Self::Optional.bits;
        const Variable    = Self::Rest.bits | Self::Variadic.bits;
        const NonRequired = Self::Optional.bits | Self::Rest.bits | Self::Variadic.bits;
        const NonRest     = Self::Required.bits | Self::Optional.bits | Self::Variadic.bits;
    }
}

// pub struct  TupleType extends GenericType {
//     elementFlags: readonly ElementFlags[];
//     minLength: number;  // Number of required or variadic elements
//     fixedLength: number;  // Number of initial required or optional elements
//     hasRestElement: boolean;  // True if tuple has any rest or variadic elements
//     combinedFlags: ElementFlags;
//     readonly: boolean;
//     labeledElementDeclarations?: readonly (NamedTupleMember | ParameterDeclaration)[];
// }

pub struct TupleType {
    elementFlags: Vec<ElementFlags>,
    minLength: usize,     // Number of required or variadic elements
    fixedLength: usize,   // Number of initial required or optional elements
    hasRestElement: bool, // True if tuple has any rest or variadic elements
    combinedFlags: ElementFlags,
    readonly: bool,
    // TODO:
    // labeledElementDeclarations: Vec<(NamedTupleMember | ParameterDeclaration)>,
    labeledElementDeclarations: Vec<BoundNode>,

    /* @internal */
    instantiations: AHashMap<JsWord, TypeId>, // Generic instantiation cache
    /* @internal */
    variances: Vec<VarianceFlags>, // Variance of each type parameter

    target: TypeId, // Type reference target
    // TODO:
    // node: Option<TypeReferenceNode | ArrayTypeNode | TupleTypeNode>,
    node: Option<BoundNode>,
    /* @internal */
    mapper: Option<TypeMapper>,
    /* @internal */
    resolvedTypeArguments: Option<Vec<TypeId>>, // Resolved type reference type arguments
    /* @internal */
    literalType: Option<TypeId>, // Clone of type with ObjectFlags.ArrayLiteral set
    /* @internal */
    cachedEquivalentBaseType: Option<TypeId>, // Only set on references to class or interfaces with a single base type and no augmentations

    typeParameters: Option<Vec<TypeId>>, // Type parameters (undefined if non-generic)
    outerTypeParameters: Option<Vec<TypeId>>, // Outer type parameters (undefined if none)
    localTypeParameters: Option<Vec<TypeId>>, // Local type parameters (undefined if none)
    thisType: Option<TypeId>,            // The "this" type (undefined if none)
    /* @internal */
    resolvedBaseConstructorType: Option<TypeId>, // Resolved base constructor type of class
    /* @internal */
    resolvedBaseTypes: Vec<BaseType>, // Resolved base types
    /* @internal */
    baseTypesResolved: bool,

    objectFlags: ObjectFlags,
    members: SymbolTable,                  // Properties by name
    properties: Vec<SymbolId>,             // Properties
    callSignatures: Vec<SignatureId>,      // Call signatures of type
    constructSignatures: Vec<SignatureId>, // Construct signatures of type
    indexInfos: Vec<IndexInfo>,            // Index signatures
    objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

// pub struct  TupleTypeReference extends TypeReference {
//     target: TupleType;
// }

pub struct TupleTypeReference {
    target: TupleType,

    // node: Option<TypeReferenceNode | ArrayTypeNode | TupleTypeNode>,
    node: Option<BoundNode>,
    /* @internal */
    mapper: Option<TypeMapper>,
    /* @internal */
    resolvedTypeArguments: Option<Vec<TypeId>>, // Resolved type reference type arguments
    /* @internal */
    literalType: Option<TypeId>, // Clone of type with ObjectFlags.ArrayLiteral set
    /* @internal */
    cachedEquivalentBaseType: Option<TypeId>, // Only set on references to class or interfaces with a single base type and no augmentations

    objectFlags: ObjectFlags,
    members: SymbolTable,                  // Properties by name
    properties: Vec<SymbolId>,             // Properties
    callSignatures: Vec<SignatureId>,      // Call signatures of type
    constructSignatures: Vec<SignatureId>, // Construct signatures of type
    indexInfos: Vec<IndexInfo>,            // Index signatures
    objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

// pub struct  UnionOrIntersectionType extends Type {
//     types: Type[];                    // Constituent types
//     /* @internal */
//     objectFlags: ObjectFlags;
//     /* @internal */
//     propertyCache?: SymbolTable;       // Cache of resolved properties
//     /* @internal */
//     propertyCacheWithoutObjectFunctionPropertyAugment?: SymbolTable; // Cache of resolved properties that does not augment function or object type properties
//     /* @internal */
//     resolvedProperties: Symbol[];
//     /* @internal */
//     resolvedIndexType: IndexType;
//     /* @internal */
//     resolvedStringIndexType: IndexType;
//     /* @internal */
//     resolvedBaseConstraint: Type;
// }

// pub struct UnionOrIntersectionType {
//     types: Vec<Type>, // Constituent types
//     /* @internal */
//     objectFlags: ObjectFlags,
//     /* @internal */
//     propertyCache: SymbolTable, // Cache of resolved properties
//     /* @internal */
//     propertyCacheWithoutObjectFunctionPropertyAugment: SymbolTable, // Cache of resolved properties that does not augment function or object type properties
//     /* @internal */
//     resolvedProperties: Vec<SymbolId>,
//     /* @internal */
//     resolvedIndexType: IndexType,
//     /* @internal */
//     resolvedStringIndexType: IndexType,
//     /* @internal */
//     resolvedBaseConstraint: Type,

//     pub flags: TypeFlags, // Flags
//     pub symbol: SymbolId, // Symbol associated with type (if any)
//     // pub pattern?: DestructuringPattern,  // Destructuring pattern represented by type (if any)
//     pub aliasSymbol: Option<SymbolId>, // Alias associated with type
//     pub aliasTypeArguments: Vec<TypeId>, // Alias type arguments (if any)
//     pub aliasTypeArgumentsContainsMarker: bool, // Alias type arguments (if any)
//     pub permissiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to wildcard type
//     pub restrictiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to unconstrained form
//     pub immediateBaseConstraint: Option<TypeId>,  // Immediate base constraint cache
//     pub widened: Option<TypeId>,                  // Cached widened form of the type
// }

// pub struct  UnionType extends UnionOrIntersectionType {
//     /* @internal */
//     resolvedReducedType?: Type;
//     /* @internal */
//     regularType?: UnionType;
//     /* @internal */
//     origin?: Type;  // Denormalized union, intersection, or index type in which union originates
//     /* @internal */
//     keyPropertyName?: __String;  // Property with unique unit type that exists in every object/intersection in union type
//     /* @internal */
//     constituentMap?: ESMap<TypeId, Type>;  // Constituents keyed by unit type discriminants
// }

pub struct UnionType {
    resolvedReducedType: Option<TypeId>,
    regularType: Option<TypeId>,
    origin: Option<TypeId>, // Denormalized union, intersection, or index type in which union originates
    keyPropertyName: Option<JsWord>, // Property with unique unit type that exists in every object/intersection in union type
    constituentMap: AHashMap<TypeId, TypeId>, // Constituents keyed by unit type discriminants

    types: Vec<TypeId>, // Constituent types
    objectFlags: ObjectFlags,
    propertyCache: SymbolTable, // Cache of resolved properties
    propertyCacheWithoutObjectFunctionPropertyAugment: SymbolTable, // Cache of resolved properties that does not augment function or object type properties
    resolvedProperties: Vec<SymbolId>,
    resolvedIndexType: IndexType,
    resolvedStringIndexType: IndexType,
    resolvedBaseConstraint: TypeId,
}

// pub struct  IntersectionType extends UnionOrIntersectionType {
//     /* @internal */
//     resolvedApparentType: Type;
// }

pub struct IntersectionType {
    resolvedApparentType: TypeId,

    types: Vec<TypeId>, // Constituent types
    objectFlags: ObjectFlags,
    propertyCache: SymbolTable, // Cache of resolved properties
    propertyCacheWithoutObjectFunctionPropertyAugment: SymbolTable, // Cache of resolved properties that does not augment function or object type properties
    resolvedProperties: Vec<SymbolId>,
    resolvedIndexType: IndexType,
    resolvedStringIndexType: IndexType,
    resolvedBaseConstraint: TypeId,
}

// export type StructuredType = ObjectType | UnionType | IntersectionType;

// // An instantiated anonymous type has a target and a mapper
// pub struct  AnonymousType extends ObjectType {
//     target?: AnonymousType;  // Instantiation target
//     mapper?: TypeMapper;     // Instantiation mapper
//     instantiations?: ESMap<string, Type>; // Instantiations of generic type alias (undefined if non-generic)
// }

// pub struct AnonymousType {
//     target: Option<AnonymousType>,          // Instantiation target
//     mapper: Option<TypeMapper>,             // Instantiation mapper
//     instantiations: AHashMap<JsWord, Type>, // Instantiations of generic type alias (undefined if non-generic)

//     objectFlags: ObjectFlags,
//     members: SymbolTable,                // Properties by name
//     properties: Vec<SymbolId>,           // Properties
//     callSignatures: Vec<Signature>,      // Call signatures of type
//     constructSignatures: Vec<Signature>, // Construct signatures of type
//     indexInfos: Vec<IndexInfo>,          // Index signatures
//     objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,

//     pub flags: TypeFlags, // Flags
//     pub symbol: SymbolId, // Symbol associated with type (if any)
//     // pub pattern?: DestructuringPattern,  // Destructuring pattern represented by type (if any)
//     pub aliasSymbol: Option<SymbolId>, // Alias associated with type
//     pub aliasTypeArguments: Vec<TypeId>, // Alias type arguments (if any)
//     pub aliasTypeArgumentsContainsMarker: bool, // Alias type arguments (if any)
//     pub permissiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to wildcard type
//     pub restrictiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to unconstrained form
//     pub immediateBaseConstraint: Option<TypeId>,  // Immediate base constraint cache
//     pub widened: Option<TypeId>,                  // Cached widened form of the type
// }

// pub struct  MappedType extends AnonymousType {
//     declaration: MappedTypeNode;
//     typeParameter?: TypeParameter;
//     constraintType?: Type;
//     nameType?: Type;
//     templateType?: Type;
//     modifiersType?: Type;
//     resolvedApparentType?: Type;
//     containsError?: boolean;
// }

pub struct MappedType {
    // TODO:
    // declaration: MappedTypeNode,
    declaration: BoundNode,
    typeParameter: Option<TypeId>,
    constraintType: Option<TypeId>,
    nameType: Option<TypeId>,
    templateType: Option<TypeId>,
    modifiersType: Option<TypeId>,
    resolvedApparentType: Option<TypeId>,
    containsError: bool,

    target: Option<TypeId>,                   // Instantiation target
    mapper: Option<TypeMapper>,               // Instantiation mapper
    instantiations: AHashMap<JsWord, TypeId>, // Instantiations of generic type alias (undefined if non-generic)

    objectFlags: ObjectFlags,
    members: SymbolTable,                  // Properties by name
    properties: Vec<SymbolId>,             // Properties
    callSignatures: Vec<SignatureId>,      // Call signatures of type
    constructSignatures: Vec<SignatureId>, // Construct signatures of type
    indexInfos: Vec<IndexInfo>,            // Index signatures
    objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

// pub struct  EvolvingArrayType extends ObjectType {
//     elementType: Type;      // Element expressions of evolving array type
//     finalArrayType?: Type;  // Final array type of evolving array type
// }

pub struct EvolvingArrayType {
    elementType: TypeId,            // Element expressions of evolving array type
    finalArrayType: Option<TypeId>, // Final array type of evolving array type

    objectFlags: ObjectFlags,
    members: SymbolTable,                  // Properties by name
    properties: Vec<SymbolId>,             // Properties
    callSignatures: Vec<SignatureId>,      // Call signatures of type
    constructSignatures: Vec<SignatureId>, // Construct signatures of type
    indexInfos: Vec<IndexInfo>,            // Index signatures
    objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

// pub struct  ReverseMappedType extends ObjectType {
//     source: Type;
//     mappedType: MappedType;
//     constraintType: IndexType;
// }

pub struct ReverseMappedType {
    source: TypeId,
    mappedType: MappedType,
    constraintType: IndexType,

    objectFlags: ObjectFlags,
    members: SymbolTable,                  // Properties by name
    properties: Vec<SymbolId>,             // Properties
    callSignatures: Vec<SignatureId>,      // Call signatures of type
    constructSignatures: Vec<SignatureId>, // Construct signatures of type
    indexInfos: Vec<IndexInfo>,            // Index signatures
    objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

// // Resolved object, union, or intersection type
// pub struct  ResolvedType extends ObjectType, UnionOrIntersectionType {
//     members: SymbolTable;             // Properties by name
//     properties: Symbol[];             // Properties
//     callSignatures: readonly Signature[];      // Call signatures of type
//     constructSignatures: readonly Signature[]; // Construct signatures of type
//     indexInfos: readonly IndexInfo[];  // Index signatures
// }

pub struct ResolvedType {
    types: Vec<TypeId>,         // Constituent types
    propertyCache: SymbolTable, // Cache of resolved properties
    propertyCacheWithoutObjectFunctionPropertyAugment: SymbolTable, // Cache of resolved properties that does not augment function or object type properties
    resolvedProperties: Vec<SymbolId>,
    resolvedIndexType: IndexType,
    resolvedStringIndexType: IndexType,
    resolvedBaseConstraint: TypeId,

    objectFlags: ObjectFlags,
    members: SymbolTable,                  // Properties by name
    properties: Vec<SymbolId>,             // Properties
    callSignatures: Vec<SignatureId>,      // Call signatures of type
    constructSignatures: Vec<SignatureId>, // Construct signatures of type
    indexInfos: Vec<IndexInfo>,            // Index signatures
    objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

impl ResolvedType {
    pub fn new(
        members: SymbolTable,
        callSignatures: Vec<SignatureId>,
        constructSignatures: Vec<SignatureId>,
        indexInfos: Vec<IndexInfo>,
        objectFlags: ObjectFlags,
    ) -> ResolvedType {
        Self {
            types: todo!(),
            propertyCache: todo!(),
            propertyCacheWithoutObjectFunctionPropertyAugment: todo!(),
            resolvedProperties: todo!(),
            resolvedIndexType: todo!(),
            resolvedStringIndexType: todo!(),
            resolvedBaseConstraint: todo!(),

            objectFlags,
            members,
            properties: todo!(),
            callSignatures,
            constructSignatures,
            indexInfos,
            objectTypeWithoutAbstractConstructSignatures: None,
        }
    }
}

// // Object literals are initially marked fresh. Freshness disappears following an assignment,
// // before a type assertion, or when an object literal's type is widened. The regular
// // version of a fresh type is identical except for the TypeFlags.FreshObjectLiteral flag.
// pub struct  FreshObjectLiteralType extends ResolvedType {
//     regularType: ResolvedType;  // Regular version of fresh type
// }

pub struct FreshObjectLiteralType {
    regularType: ResolvedType, // Regular version of fresh type

    types: Vec<TypeId>,         // Constituent types
    propertyCache: SymbolTable, // Cache of resolved properties
    propertyCacheWithoutObjectFunctionPropertyAugment: SymbolTable, // Cache of resolved properties that does not augment function or object type properties
    resolvedProperties: Vec<SymbolId>,
    resolvedIndexType: IndexType,
    resolvedStringIndexType: IndexType,
    resolvedBaseConstraint: TypeId,

    objectFlags: ObjectFlags,
    members: SymbolTable,                  // Properties by name
    properties: Vec<SymbolId>,             // Properties
    callSignatures: Vec<SignatureId>,      // Call signatures of type
    constructSignatures: Vec<SignatureId>, // Construct signatures of type
    indexInfos: Vec<IndexInfo>,            // Index signatures
    objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

pub struct IterationTypes {
    yieldType: TypeId,
    returnType: TypeId,
    nextType: TypeId,
}

// // Just a place to cache element types of iterables and iterators

// pub struct  IterableOrIteratorType extends ObjectType, UnionType {
//     iterationTypesOfGeneratorReturnType?: IterationTypes;
//     iterationTypesOfAsyncGeneratorReturnType?: IterationTypes;
//     iterationTypesOfIterable?: IterationTypes;
//     iterationTypesOfIterator?: IterationTypes;
//     iterationTypesOfAsyncIterable?: IterationTypes;
//     iterationTypesOfAsyncIterator?: IterationTypes;
//     iterationTypesOfIteratorResult?: IterationTypes;
// }

pub struct IterableOrIteratorType {
    iterationTypesOfGeneratorReturnType: Option<IterationTypes>,
    iterationTypesOfAsyncGeneratorReturnType: Option<IterationTypes>,
    iterationTypesOfIterable: Option<IterationTypes>,
    iterationTypesOfIterator: Option<IterationTypes>,
    iterationTypesOfAsyncIterable: Option<IterationTypes>,
    iterationTypesOfAsyncIterator: Option<IterationTypes>,
    iterationTypesOfIteratorResult: Option<IterationTypes>,

    objectFlags: ObjectFlags,
    members: SymbolTable,                  // Properties by name
    properties: Vec<SymbolId>,             // Properties
    callSignatures: Vec<SignatureId>,      // Call signatures of type
    constructSignatures: Vec<SignatureId>, // Construct signatures of type
    indexInfos: Vec<IndexInfo>,            // Index signatures
    objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,

    resolvedReducedType: Option<TypeId>,
    regularType: Option<UnionType>,
    origin: Option<TypeId>, // Denormalized union, intersection, or index type in which union originates
    keyPropertyName: Option<JsWord>, // Property with unique unit type that exists in every object/intersection in union type
    constituentMap: AHashMap<TypeId, TypeId>, // Constituents keyed by unit type discriminants

    types: Vec<TypeId>,         // Constituent types
    propertyCache: SymbolTable, // Cache of resolved properties
    propertyCacheWithoutObjectFunctionPropertyAugment: SymbolTable, // Cache of resolved properties that does not augment function or object type properties
    resolvedProperties: Vec<SymbolId>,
    resolvedIndexType: IndexType,
    resolvedStringIndexType: IndexType,
    resolvedBaseConstraint: TypeId,
}

// pub struct  PromiseOrAwaitableType extends ObjectType, UnionType {
//     promiseTypeOfPromiseConstructor?: Type;
//     promisedTypeOfPromise?: Type;
//     awaitedTypeOfType?: Type;
// }

pub struct PromiseOrAwaitableType {
    promiseTypeOfPromiseConstructor: Option<TypeId>,
    promisedTypeOfPromise: Option<TypeId>,
    awaitedTypeOfType: Option<TypeId>,

    objectFlags: ObjectFlags,
    members: SymbolTable,                  // Properties by name
    properties: Vec<SymbolId>,             // Properties
    callSignatures: Vec<SignatureId>,      // Call signatures of type
    constructSignatures: Vec<SignatureId>, // Construct signatures of type
    indexInfos: Vec<IndexInfo>,            // Index signatures
    objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,

    resolvedReducedType: Option<TypeId>,
    regularType: Option<UnionType>,
    origin: Option<TypeId>, // Denormalized union, intersection, or index type in which union originates
    keyPropertyName: Option<JsWord>, // Property with unique unit type that exists in every object/intersection in union type
    constituentMap: AHashMap<TypeId, TypeId>, // Constituents keyed by unit type discriminants

    types: Vec<TypeId>,         // Constituent types
    propertyCache: SymbolTable, // Cache of resolved properties
    propertyCacheWithoutObjectFunctionPropertyAugment: SymbolTable, // Cache of resolved properties that does not augment function or object type properties
    resolvedProperties: Vec<SymbolId>,
    resolvedIndexType: IndexType,
    resolvedStringIndexType: IndexType,
    resolvedBaseConstraint: TypeId,
}

// pub struct  SyntheticDefaultModuleType extends Type {
//     syntheticType?: Type;
// }

pub struct SyntheticDefaultModuleType {
    syntheticType: Option<TypeId>,
}

// pub struct  InstantiableType extends Type {
//     /* @internal */
//     resolvedBaseConstraint?: Type;
//     /* @internal */
//     resolvedIndexType?: IndexType;
//     /* @internal */
//     resolvedStringIndexType?: IndexType;
// }

// pub struct InstantiableType {
//     /* @internal */
//     resolvedBaseConstraint: Option<Type>,
//     /* @internal */
//     resolvedIndexType: Option<IndexType>,
//     /* @internal */
//     resolvedStringIndexType: Option<IndexType>,

//     pub flags: TypeFlags, // Flags
//     pub symbol: SymbolId, // Symbol associated with type (if any)
//     // pub pattern?: DestructuringPattern,  // Destructuring pattern represented by type (if any)
//     pub aliasSymbol: Option<SymbolId>, // Alias associated with type
//     pub aliasTypeArguments: Vec<TypeId>, // Alias type arguments (if any)
//     pub aliasTypeArgumentsContainsMarker: bool, // Alias type arguments (if any)
//     pub permissiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to wildcard type
//     pub restrictiveInstantiation: Option<TypeId>, // Instantiation with type parameters mapped to unconstrained form
//     pub immediateBaseConstraint: Option<TypeId>,  // Immediate base constraint cache
//     pub widened: Option<TypeId>,                  // Cached widened form of the type
// }

// // Type parameters (TypeFlags.TypeParameter)
// pub struct  TypeParameter extends InstantiableType {
//     /** Retrieve using getConstraintFromTypeParameter */
//     /* @internal */
//     constraint?: Type;        // Constraint
//     /* @internal */
//     default?: Type;
//     /* @internal */
//     target?: TypeParameter;  // Instantiation target
//     /* @internal */
//     mapper?: TypeMapper;     // Instantiation mapper
//     /* @internal */
//     isThisType?: boolean;
//     /* @internal */
//     resolvedDefaultType?: Type;
// }

pub struct TypeParameter {
    /** Retrieve using getConstraintFromTypeParameter */
    constraint: Option<TypeId>, // Constraint
    default: Option<TypeId>,
    target: Option<TypeId>,     // Instantiation target
    mapper: Option<TypeMapper>, // Instantiation mapper
    isThisType: bool,
    resolvedDefaultType: Option<TypeId>,

    resolvedBaseConstraint: Option<TypeId>,
    resolvedIndexType: Option<IndexType>,
    resolvedStringIndexType: Option<IndexType>,
}

bitflags! {
    pub struct AccessFlags: u16 {
        const None = 0;
        const IncludeUndefined = 1 << 0;
        const NoIndexSignatures = 1 << 1;
        const Writing = 1 << 2;
        const CacheSymbol = 1 << 3;
        const NoTupleBoundsCheck = 1 << 4;
        const ExpressionPosition = 1 << 5;
        const ReportDeprecated = 1 << 6;
        const SuppressNoImplicitAnyError = 1 << 7;
        const Contextual = 1 << 8;
        const Persistent = Self::IncludeUndefined.bits;
    }
}

// // Indexed access types (TypeFlags.IndexedAccess)
// // Possible forms are T[xxx], xxx[T], or xxx[keyof T], where T is a type variable
// pub struct  IndexedAccessType extends InstantiableType {
//     objectType: Type;
//     indexType: Type;
//     /* @internal */
//     accessFlags: AccessFlags;  // Only includes AccessFlags.Persistent
//     constraint?: Type;
//     simplifiedForReading?: Type;
//     simplifiedForWriting?: Type;
// }

pub struct IndexedAccessType {
    objectType: TypeId,
    indexType: TypeId,
    accessFlags: AccessFlags, // Only includes AccessFlags.Persistent
    constraint: Option<TypeId>,
    simplifiedForReading: Option<TypeId>,
    simplifiedForWriting: Option<TypeId>,

    resolvedBaseConstraint: Option<TypeId>,
    resolvedIndexType: Option<IndexType>,
    resolvedStringIndexType: Option<IndexType>,
}

// export type TypeVariable = TypeParameter | IndexedAccessType;

// // keyof T types (TypeFlags.Index)
// pub struct  IndexType extends InstantiableType {
//     type: InstantiableType | UnionOrIntersectionType;
//     /* @internal */
//     stringsOnly: boolean;
// }

pub struct IndexType {
    // TODO:
    // ty: InstantiableType | UnionOrIntersectionType,
    ty: TypeId,
    stringsOnly: bool,

    resolvedBaseConstraint: Option<TypeId>,
    resolvedIndexType: Option<TypeId>,
    resolvedStringIndexType: Option<TypeId>,
}

pub struct ConditionalRoot {
    // node: ConditionalTypeNode,
    node: BoundNode,
    checkType: TypeId,
    extendsType: TypeId,
    isDistributive: bool,
    inferTypeParameters: Vec<TypeId>,
    outerTypeParameters: Vec<TypeId>,
    // instantiations?: Map<Type>,
    aliasSymbol: Option<SymbolId>,
    aliasTypeArguments: Vec<TypeId>,
}

// // T extends U ? X : Y (TypeFlags.Conditional)
// pub struct  ConditionalType extends InstantiableType {
//     root: ConditionalRoot;
//     checkType: Type;
//     extendsType: Type;
//     resolvedTrueType?: Type;
//     resolvedFalseType?: Type;
//     /* @internal */
//     resolvedInferredTrueType?: Type; // The `trueType` instantiated with the `combinedMapper`, if present
//     /* @internal */
//     resolvedDefaultConstraint?: Type;
//     /* @internal */
//     mapper?: TypeMapper;
//     /* @internal */
//     combinedMapper?: TypeMapper;
// }

pub struct ConditionalType {
    root: ConditionalRoot,
    checkType: TypeId,
    extendsType: TypeId,
    resolvedTrueType: Option<TypeId>,
    resolvedFalseType: Option<TypeId>,
    resolvedInferredTrueType: Option<TypeId>, // The `trueType` instantiated with the `combinedMapper`, if present
    resolvedDefaultConstraint: Option<TypeId>,
    mapper: Option<TypeMapper>,
    combinedMapper: Option<TypeMapper>,

    resolvedBaseConstraint: Option<TypeId>,
    resolvedIndexType: Option<IndexType>,
    resolvedStringIndexType: Option<IndexType>,
}

// pub struct  TemplateLiteralType extends InstantiableType {
//     texts: readonly string[];  // Always one element longer than types
//     types: readonly Type[];  // Always at least one element
// }

pub struct TemplateLiteralType {
    texts: Vec<JsWord>, // Always one element longer than types
    types: Vec<TypeId>, // Always at least one element

    resolvedBaseConstraint: Option<TypeId>,
    resolvedIndexType: Option<IndexType>,
    resolvedStringIndexType: Option<IndexType>,
}

// pub struct  StringMappingType extends InstantiableType {
//     symbol: Symbol;
//     type: Type;
// }

pub struct StringMappingType {
    ty: TypeId,

    resolvedBaseConstraint: Option<TypeId>,
    resolvedIndexType: Option<IndexType>,
    resolvedStringIndexType: Option<IndexType>,
}

// // Type parameter substitution (TypeFlags.Substitution)
// // Substitution types are created for type parameters or indexed access types that occur in the
// // true branch of a conditional type. For example, in 'T extends string ? Foo<T> : Bar<T>', the
// // reference to T in Foo<T> is resolved as a substitution type that substitutes 'string & T' for T.
// // Thus, if Foo has a 'string' constraint on its type parameter, T will satisfy it. Substitution
// // types disappear upon instantiation (just like type parameters).
// pub struct  SubstitutionType extends InstantiableType {
//     objectFlags: ObjectFlags;
//     baseType: Type;     // Target type
//     substitute: Type;   // Type to substitute for type parameter
// }

pub struct SubstitutionType {
    objectFlags: ObjectFlags,
    baseType: TypeId,   // Target type
    substitute: TypeId, // Type to substitute for type parameter

    resolvedBaseConstraint: Option<TypeId>,
    resolvedIndexType: Option<IndexType>,
    resolvedStringIndexType: Option<IndexType>,
}
