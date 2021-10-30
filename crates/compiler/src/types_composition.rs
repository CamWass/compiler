use crate::node::BoundNode;
use crate::types::*;
use ahash::AHashMap;
use bitflags::bitflags;
use num_bigint::BigInt as BigIntValue;
use swc_atoms::JsWord;

// Properties common to all types
#[derive(Debug)]
pub struct TypeBase {
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

impl TypeBase {
    pub fn new(type_flags: TypeFlags, symbol: Option<SymbolId>) -> Self {
        Self {
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

pub trait AsTypeBase {
    fn get_flags(&self) -> &TypeFlags;
    fn get_symbol(&self) -> &Option<SymbolId>;
    fn get_aliasSymbol(&self) -> &Option<SymbolId>;
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId>;
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool;
    fn get_permissiveInstantiation(&self) -> &Option<TypeId>;
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId>;
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId>;
    fn get_widened(&self) -> &Option<TypeId>;
}

#[derive(Debug)]
pub enum Type {
    IntrinsicType(IntrinsicType),
    NullableType(NullableType),
    FreshableIntrinsicType(FreshableIntrinsicType),
    // LiteralType(LiteralType),
    UniqueESSymbolType(UniqueESSymbolType),
    StringLiteralType(StringLiteralType),
    NumberLiteralType(NumberLiteralType),
    BigIntLiteralType(BigIntLiteralType),
    EnumType(EnumType),
    ObjectType(ObjectType),
    // InterfaceType(InterfaceType),
    InterfaceTypeWithDeclaredMembers(InterfaceTypeWithDeclaredMembers),
    // TypeReference(TypeReference),
    DeferredTypeReference(DeferredTypeReference),
    GenericType(GenericType),
    TupleType(TupleType),
    TupleTypeReference(TupleTypeReference),
    MappedType(MappedType),
    EvolvingArrayType(EvolvingArrayType),
    ReverseMappedType(ReverseMappedType),
    ResolvedType(ResolvedType),
    // FreshObjectLiteralType(FreshObjectLiteralType),
    // IterationTypes(IterationTypes),
    IterableOrIteratorType(IterableOrIteratorType),
    PromiseOrAwaitableType(PromiseOrAwaitableType),
    SyntheticDefaultModuleType(SyntheticDefaultModuleType),
    // InstantiableType(InstantiableType),
    TypeParameter(TypeParameter),
    IndexedAccessType(IndexedAccessType),
    IndexType(IndexType),
    ConditionalType(ConditionalType),
    TemplateLiteralType(TemplateLiteralType),
    StringMappingType(StringMappingType),
    SubstitutionType(SubstitutionType),
}

impl AsTypeBase for Type {
    fn get_flags(&self) -> &TypeFlags {
        match self {
            Type::IntrinsicType(t) => t.get_flags(),
            Type::NullableType(t) => t.get_flags(),
            Type::FreshableIntrinsicType(t) => t.get_flags(),
            // Type::LiteralType(t) => t.get_flags(),
            Type::UniqueESSymbolType(t) => t.get_flags(),
            Type::StringLiteralType(t) => t.get_flags(),
            Type::NumberLiteralType(t) => t.get_flags(),
            Type::BigIntLiteralType(t) => t.get_flags(),
            Type::EnumType(t) => t.get_flags(),
            Type::ObjectType(t) => t.get_flags(),
            // Type::InterfaceType(t) => t.get_flags(),
            Type::InterfaceTypeWithDeclaredMembers(t) => t.get_flags(),
            // Type::TypeReference(t) => t.get_flags(),
            Type::DeferredTypeReference(t) => t.get_flags(),
            Type::GenericType(t) => t.get_flags(),
            Type::TupleType(t) => t.get_flags(),
            Type::TupleTypeReference(t) => t.get_flags(),
            Type::MappedType(t) => t.get_flags(),
            Type::EvolvingArrayType(t) => t.get_flags(),
            Type::ReverseMappedType(t) => t.get_flags(),
            Type::ResolvedType(t) => t.get_flags(),
            // Type::FreshObjectLiteralType(t) => t.get_flags(),
            // Type::IterationTypes(t) => t.get_flags(),
            Type::IterableOrIteratorType(t) => t.get_flags(),
            Type::PromiseOrAwaitableType(t) => t.get_flags(),
            Type::SyntheticDefaultModuleType(t) => t.get_flags(),
            // Type::InstantiableType(t) => t.get_flags(),
            Type::TypeParameter(t) => t.get_flags(),
            Type::IndexedAccessType(t) => t.get_flags(),
            Type::IndexType(t) => t.get_flags(),
            Type::ConditionalType(t) => t.get_flags(),
            Type::TemplateLiteralType(t) => t.get_flags(),
            Type::StringMappingType(t) => t.get_flags(),
            Type::SubstitutionType(t) => t.get_flags(),
        }
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        match self {
            Type::IntrinsicType(t) => t.get_symbol(),
            Type::NullableType(t) => t.get_symbol(),
            Type::FreshableIntrinsicType(t) => t.get_symbol(),
            // Type::LiteralType(t) => t.get_symbol(),
            Type::UniqueESSymbolType(t) => t.get_symbol(),
            Type::StringLiteralType(t) => t.get_symbol(),
            Type::NumberLiteralType(t) => t.get_symbol(),
            Type::BigIntLiteralType(t) => t.get_symbol(),
            Type::EnumType(t) => t.get_symbol(),
            Type::ObjectType(t) => t.get_symbol(),
            // Type::InterfaceType(t) => t.get_symbol(),
            Type::InterfaceTypeWithDeclaredMembers(t) => t.get_symbol(),
            // Type::TypeReference(t) => t.get_symbol(),
            Type::DeferredTypeReference(t) => t.get_symbol(),
            Type::GenericType(t) => t.get_symbol(),
            Type::TupleType(t) => t.get_symbol(),
            Type::TupleTypeReference(t) => t.get_symbol(),
            Type::MappedType(t) => t.get_symbol(),
            Type::EvolvingArrayType(t) => t.get_symbol(),
            Type::ReverseMappedType(t) => t.get_symbol(),
            Type::ResolvedType(t) => t.get_symbol(),
            // Type::FreshObjectLiteralType(t) => t.get_symbol(),
            // Type::IterationTypes(t) => t.get_symbol(),
            Type::IterableOrIteratorType(t) => t.get_symbol(),
            Type::PromiseOrAwaitableType(t) => t.get_symbol(),
            Type::SyntheticDefaultModuleType(t) => t.get_symbol(),
            // Type::InstantiableType(t) => t.get_symbol(),
            Type::TypeParameter(t) => t.get_symbol(),
            Type::IndexedAccessType(t) => t.get_symbol(),
            Type::IndexType(t) => t.get_symbol(),
            Type::ConditionalType(t) => t.get_symbol(),
            Type::TemplateLiteralType(t) => t.get_symbol(),
            Type::StringMappingType(t) => t.get_symbol(),
            Type::SubstitutionType(t) => t.get_symbol(),
        }
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        todo!();
        // &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        todo!();
        // &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        todo!();
        // self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        todo!();
        // &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        todo!();
        // &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        todo!();
        // &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        todo!();
        // &self.type_base.widened
    }
}

// Intrinsic types (TypeFlags.Intrinsic)
#[derive(Debug)]
pub struct IntrinsicTypeBase {
    pub intrinsicName: JsWord, // Name of intrinsic type
    pub objectFlags: ObjectFlags,
}

#[derive(Debug)]
pub struct IntrinsicType {
    pub type_base: TypeBase,

    pub intrinsic_type_base: IntrinsicTypeBase,
}

impl AsTypeBase for IntrinsicType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct NullableType {
    pub intrinsic_type_base: IntrinsicTypeBase,
    pub type_base: TypeBase,
}

impl AsTypeBase for NullableType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct FreshableIntrinsicType {
    pub intrinsic_type_base: IntrinsicTypeBase,
    pub type_base: TypeBase,

    pub freshType: TypeId,   // Fresh version of type
    pub regularType: TypeId, // Regular version of type
}

impl AsTypeBase for FreshableIntrinsicType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

// export type FreshableType = LiteralType | FreshableIntrinsicType;

// String literal types (TypeFlags.StringLiteral)
// Numeric literal types (TypeFlags.NumberLiteral)
// BigInt literal types (TypeFlags.BigIntLiteral)
#[derive(Debug)]
pub struct LiteralType {
    pub freshType: TypeId,   // Fresh version of type
    pub regularType: TypeId, // Regular version of type
}

// Unique symbol types (TypeFlags.UniqueESSymbol)
#[derive(Debug)]
pub struct UniqueESSymbolType {
    pub type_base: TypeBase,

    pub symbol: Symbol,
    pub escapedName: JsWord,
}

impl AsTypeBase for UniqueESSymbolType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct StringLiteralType {
    pub literal_type: LiteralType,
    pub type_base: TypeBase,

    pub value: JsWord,
}

impl AsTypeBase for StringLiteralType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct NumberLiteralType {
    pub literal_type: LiteralType,
    pub type_base: TypeBase,

    pub value: f64,
}

impl AsTypeBase for NumberLiteralType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct BigIntLiteralType {
    pub literal_type: LiteralType,
    pub type_base: TypeBase,

    pub value: BigIntValue,
}

impl AsTypeBase for BigIntLiteralType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

// Enum types (TypeFlags.Enum)
#[derive(Debug)]
pub struct EnumType {
    pub type_base: TypeBase,
}

impl AsTypeBase for EnumType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

// export type ObjectFlagsType = NullableType | ObjectType | UnionType | IntersectionType;

// Object types (TypeFlags.ObjectType)
#[derive(Debug)]
pub struct ObjectTypeBase {
    pub objectFlags: ObjectFlags,
    pub members: SymbolTable,                  // Properties by name
    pub properties: Vec<SymbolId>,             // Properties
    pub callSignatures: Vec<SignatureId>,      // Call signatures of type
    pub constructSignatures: Vec<SignatureId>, // Construct signatures of type
    pub indexInfos: Vec<IndexInfo>,            // Index signatures
    pub objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

impl ObjectTypeBase {
    pub fn new(objectFlags: ObjectFlags) -> Self {
        Self {
            objectFlags,
            members: SymbolTable::default(),
            properties: Vec::new(),
            callSignatures: Vec::new(),
            constructSignatures: Vec::new(),
            indexInfos: Vec::new(),
            objectTypeWithoutAbstractConstructSignatures: None,
        }
    }
}

#[derive(Debug)]
pub struct ObjectType {
    pub type_base: TypeBase,
    pub object_type_base: ObjectTypeBase,
}

impl ObjectType {
    pub fn new(objectFlags: ObjectFlags, symbol: Option<SymbolId>) -> Self {
        Self {
            type_base: TypeBase::new(TypeFlags::Object, symbol),
            object_type_base: ObjectTypeBase::new(objectFlags),
        }
    }
}

impl AsTypeBase for ObjectType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

/** Class and interface types (ObjectFlags.Class and ObjectFlags.Interface). */
#[derive(Default, Debug)]
pub struct InterfaceType {
    pub typeParameters: Option<Vec<TypeId>>, // Type parameters (undefined if non-generic)
    pub outerTypeParameters: Option<Vec<TypeId>>, // Outer type parameters (undefined if none)
    pub localTypeParameters: Option<Vec<TypeId>>, // Local type parameters (undefined if none)
    pub thisType: Option<TypeId>,            // The "this" type (undefined if none)
    pub resolvedBaseConstructorType: Option<TypeId>, // Resolved base constructor type of class
    pub resolvedBaseTypes: Vec<BaseType>,    // Resolved base types
    pub baseTypesResolved: bool,
}

// // Object type or intersection of object types
// export type BaseType = ObjectType | IntersectionType | TypeVariable; // Also `any` and `object`
#[derive(Debug)]
pub enum BaseType {
    ObjectType(TypeId),
    IntersectionType(TypeId),
    TypeVariable(TypeId),
    // Also `any` and `object`
}

#[derive(Debug)]
pub struct InterfaceTypeWithDeclaredMembers {
    pub interface_type: InterfaceType,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    pub declaredProperties: Vec<SymbolId>, // Declared members
    pub declaredCallSignatures: Vec<SignatureId>, // Declared call signatures
    pub declaredConstructSignatures: Vec<SignatureId>, // Declared construct signatures
    pub declaredIndexInfos: Vec<IndexInfo>, // Declared index signatures
}

impl AsTypeBase for InterfaceTypeWithDeclaredMembers {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

/**
 * Type references (ObjectFlags.Reference). When a class or interface has type parameters or
 * a "this" type, references to the class or interface are made using type references. The
 * typeArguments property specifies the types to substitute for the type parameters of the
 * class or interface and optionally includes an extra element that specifies the type to
 * substitute for "this" in the resulting instantiation. When no extra argument is present,
 * the type reference itself is substituted for "this". The typeArguments property is undefined
 * if the class or interface has no type parameters and the reference isn't specifying an
 * explicit "this" argument.
 */
#[derive(Default, Debug)]
pub struct TypeReference {
    pub target: Option<TypeId>, // Type reference target
    // TODO:
    // node: Option<TypeReferenceNode | ArrayTypeNode | TupleTypeNode>,
    pub node: Option<BoundNode>,
    pub mapper: Option<TypeMapper>,
    pub resolvedTypeArguments: Option<Vec<TypeId>>, // Resolved type reference type arguments
    pub literalType: Option<TypeId>, // Clone of type with ObjectFlags.ArrayLiteral set
    pub cachedEquivalentBaseType: Option<TypeId>, // Only set on references to class or interfaces with a single base type and no augmentations
}

#[derive(Debug)]
pub struct DeferredTypeReference {
    pub type_reference: TypeReference,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    // TODO:
    // node: TypeReferenceNode | ArrayTypeNode | TupleTypeNode,
    pub node: BoundNode,
    pub instantiations: AHashMap<JsWord, TypeId>, // Instantiations of generic type alias (undefined if non-generic)
}

impl AsTypeBase for DeferredTypeReference {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
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

// Generic class and interface types
#[derive(Default, Debug)]
pub struct GenericTypeBase {
    pub instantiations: AHashMap<JsWord, TypeId>, // Generic instantiation cache
    pub variances: Vec<VarianceFlags>,            // Variance of each type parameter
}

#[derive(Debug)]
pub struct GenericType {
    pub interface_type: InterfaceType,
    pub type_reference: TypeReference,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    pub generic_type_base: GenericTypeBase,
}

impl AsTypeBase for GenericType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

impl GenericType {
    pub fn new(
        interface_type: InterfaceType,
        type_reference: TypeReference,
        object_type_base: ObjectTypeBase,
        type_base: TypeBase,
    ) -> Self {
        Self {
            interface_type,
            type_reference,
            object_type_base,
            type_base,

            generic_type_base: GenericTypeBase::default(),
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

#[derive(Debug)]
pub struct TupleType {
    pub generic_type: GenericTypeBase,
    pub interface_type: InterfaceType,
    pub type_reference: TypeReference,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    pub elementFlags: Vec<ElementFlags>,
    pub minLength: usize,     // Number of required or variadic elements
    pub fixedLength: usize,   // Number of initial required or optional elements
    pub hasRestElement: bool, // True if tuple has any rest or variadic elements
    pub combinedFlags: ElementFlags,
    pub readonly: bool,
    // TODO:
    // labeledElementDeclarations: Vec<(NamedTupleMember | ParameterDeclaration)>,
    pub labeledElementDeclarations: Vec<BoundNode>,
}

impl AsTypeBase for TupleType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct TupleTypeReference {
    pub type_reference: TypeReference,
    pub type_base: TypeBase,
    pub object_type_base: ObjectTypeBase,

    pub target: TupleType,
}

impl AsTypeBase for TupleTypeReference {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct UnionOrIntersectionType {
    pub types: Vec<TypeId>, // Constituent types
    pub objectFlags: ObjectFlags,
    pub propertyCache: SymbolTable, // Cache of resolved properties
    pub propertyCacheWithoutObjectFunctionPropertyAugment: SymbolTable, // Cache of resolved properties that does not augment function or object type properties
    pub resolvedProperties: Vec<SymbolId>,
    pub resolvedIndexType: Option<TypeId>,
    pub resolvedStringIndexType: Option<TypeId>,
    pub resolvedBaseConstraint: Option<TypeId>,
}

impl UnionOrIntersectionType {
    pub fn new(objectFlags: ObjectFlags) -> Self {
        Self {
            types: Vec::new(),
            objectFlags,
            propertyCache: SymbolTable::default(),
            propertyCacheWithoutObjectFunctionPropertyAugment: SymbolTable::default(),
            resolvedProperties: Vec::new(),
            resolvedIndexType: None,
            resolvedStringIndexType: None,
            resolvedBaseConstraint: None,
        }
    }
}

// pub trait AsUnionOrIntersectionType {
//     fn get_types(&self) -> &Vec<TypeId>;
//     fn get_objectFlags(&self) -> &ObjectFlags;
//     fn get_propertyCache(&self) -> &SymbolTable;
//     fn get_propertyCacheWithoutObjectFunctionPropertyAugment(&self) -> &SymbolTable;
//     fn get_resolvedProperties(&self) -> &Vec<SymbolId>;
//     fn get_resolvedIndexType(&self) -> &Option<TypeId>;
//     fn get_resolvedStringIndexType(&self) -> &Option<TypeId>;
//     fn get_resolvedBaseConstraint(&self) -> &Option<TypeId>;
// }

// pub trait AsUnionOrIntersectionTypeMut {
//     fn get_types_mut(&self) -> &mut Vec<TypeId>;
//     fn get_objectFlags_mut(&self) -> &mut ObjectFlags;
//     fn get_propertyCache_mut(&self) -> &mut SymbolTable;
//     fn get_propertyCacheWithoutObjectFunctionPropertyAugment_mut(&self) -> &mut SymbolTable;
//     fn get_resolvedProperties_mut(&self) -> &mut Vec<SymbolId>;
//     fn get_resolvedIndexType_mut(&self) -> &mut Option<TypeId>;
//     fn get_resolvedStringIndexType_mut(&self) -> &mut Option<TypeId>;
//     fn get_resolvedBaseConstraint_mut(&self) -> &mut Option<TypeId>;
// }

#[derive(Debug)]
pub struct UnionTypeBase {
    pub resolvedReducedType: Option<TypeId>,
    pub regularType: Option<TypeId>,
    pub origin: Option<TypeId>, // Denormalized union, intersection, or index type in which union originates
    pub keyPropertyName: Option<JsWord>, // Property with unique unit type that exists in every object/intersection in union type
    pub constituentMap: AHashMap<TypeId, TypeId>, // Constituents keyed by unit type discriminants
}

#[derive(Debug)]
pub struct UnionType {
    pub union_or_intersection_type: UnionOrIntersectionType,

    pub union_type_base: UnionTypeBase,
}

// impl AsUnionOrIntersectionType for UnionType {
//     fn get_types(&self) -> &Vec<TypeId> {
//         &self.union_or_intersection_type.types
//     }
//     fn get_objectFlags(&self) -> &ObjectFlags {
//         &self.union_or_intersection_type.objectFlags
//     }
//     fn get_propertyCache(&self) -> &SymbolTable {
//         &self.union_or_intersection_type.propertyCache
//     }
//     fn get_propertyCacheWithoutObjectFunctionPropertyAugment(&self) -> &SymbolTable {
//         &self
//             .union_or_intersection_type
//             .propertyCacheWithoutObjectFunctionPropertyAugment
//     }
//     fn get_resolvedProperties(&self) -> &Vec<SymbolId> {
//         &self.union_or_intersection_type.resolvedProperties
//     }
//     fn get_resolvedIndexType(&self) -> &Option<TypeId> {
//         &self.union_or_intersection_type.resolvedIndexType
//     }
//     fn get_resolvedStringIndexType(&self) -> &Option<TypeId> {
//         &self.union_or_intersection_type.resolvedStringIndexType
//     }
//     fn get_resolvedBaseConstraint(&self) -> &Option<TypeId> {
//         &self.union_or_intersection_type.resolvedBaseConstraint
//     }
// }

// impl AsUnionOrIntersectionTypeMut for UnionType {
//     fn get_types_mut(&self) -> &mut Vec<TypeId> {
//         &mut self.union_or_intersection_type.types
//     }
//     fn get_objectFlags_mut(&self) -> &mut ObjectFlags {
//         &mut self.union_or_intersection_type.objectFlags
//     }
//     fn get_propertyCache_mut(&self) -> &mut SymbolTable {
//         &mut self.union_or_intersection_type.propertyCache
//     }
//     fn get_propertyCacheWithoutObjectFunctionPropertyAugment_mut(&self) -> &mut SymbolTable {
//         &mut self
//             .union_or_intersection_type
//             .propertyCacheWithoutObjectFunctionPropertyAugment
//     }
//     fn get_resolvedProperties_mut(&self) -> &mut Vec<SymbolId> {
//         &mut self.union_or_intersection_type.resolvedProperties
//     }
//     fn get_resolvedIndexType_mut(&self) -> &mut Option<TypeId> {
//         &mut self.union_or_intersection_type.resolvedIndexType
//     }
//     fn get_resolvedStringIndexType_mut(&self) -> &mut Option<TypeId> {
//         &mut self.union_or_intersection_type.resolvedStringIndexType
//     }
//     fn get_resolvedBaseConstraint_mut(&self) -> &mut Option<TypeId> {
//         &mut self.union_or_intersection_type.resolvedBaseConstraint
//     }
// }

#[derive(Debug)]
pub struct IntersectionType {
    pub union_or_intersection_type: UnionOrIntersectionType,

    pub resolvedApparentType: TypeId,
}

// export type StructuredType = ObjectType | UnionType | IntersectionType;

// An instantiated anonymous type has a target and a mapper
#[derive(Debug)]
pub struct AnonymousType {
    pub target: Option<TypeId>,                   // Instantiation target
    pub mapper: Option<TypeMapper>,               // Instantiation mapper
    pub instantiations: AHashMap<JsWord, TypeId>, // Instantiations of generic type alias (undefined if non-generic)
}

#[derive(Debug)]
pub struct MappedType {
    pub anonymous_type: AnonymousType,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    // TODO:
    // declaration: MappedTypeNode,
    pub declaration: BoundNode,
    pub typeParameter: Option<TypeId>,
    pub constraintType: Option<TypeId>,
    pub nameType: Option<TypeId>,
    pub templateType: Option<TypeId>,
    pub modifiersType: Option<TypeId>,
    pub resolvedApparentType: Option<TypeId>,
    pub containsError: bool,
}

impl AsTypeBase for MappedType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct EvolvingArrayType {
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    pub elementType: TypeId, // Element expressions of evolving array type
    pub finalArrayType: Option<TypeId>, // Final array type of evolving array type
}

impl AsTypeBase for EvolvingArrayType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct ReverseMappedType {
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    pub source: TypeId,
    pub mappedType: MappedType,
    pub constraintType: IndexType,
}

impl AsTypeBase for ReverseMappedType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

// Resolved object, union, or intersection type
#[derive(Debug)]
pub struct ResolvedType {
    pub type_base: TypeBase,
    pub object_type_base: ObjectTypeBase,
    pub union_or_intersection_type: UnionOrIntersectionType,
}

impl ResolvedType {
    pub fn new(type_flags: TypeFlags, symbol: Option<SymbolId>, object_flags: ObjectFlags) -> Self {
        Self {
            type_base: TypeBase::new(TypeFlags::Object, symbol),
            object_type_base: ObjectTypeBase::new(object_flags),
            union_or_intersection_type: UnionOrIntersectionType::new(object_flags),
        }
    }
}

impl AsTypeBase for ResolvedType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

// Object literals are initially marked fresh. Freshness disappears following an assignment,
// before a type assertion, or when an object literal's type is widened. The regular
// version of a fresh type is identical except for the TypeFlags.FreshObjectLiteral flag.
#[derive(Debug)]
pub struct FreshObjectLiteralType {
    pub resolved_type: ResolvedType,

    pub regularType: ResolvedType, // Regular version of fresh type
}

#[derive(Debug)]
pub struct IterationTypes {
    pub yieldType: TypeId,
    pub returnType: TypeId,
    pub nextType: TypeId,
}

// Just a place to cache element types of iterables and iterators

#[derive(Debug)]
pub struct IterableOrIteratorType {
    pub object_type_base: ObjectTypeBase,
    pub union_type: UnionType,
    pub union_or_intersection_type: UnionOrIntersectionType,
    pub type_base: TypeBase,

    pub iterationTypesOfGeneratorReturnType: Option<IterationTypes>,
    pub iterationTypesOfAsyncGeneratorReturnType: Option<IterationTypes>,
    pub iterationTypesOfIterable: Option<IterationTypes>,
    pub iterationTypesOfIterator: Option<IterationTypes>,
    pub iterationTypesOfAsyncIterable: Option<IterationTypes>,
    pub iterationTypesOfAsyncIterator: Option<IterationTypes>,
    pub iterationTypesOfIteratorResult: Option<IterationTypes>,
}

impl AsTypeBase for IterableOrIteratorType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct PromiseOrAwaitableType {
    pub object_type_base: ObjectTypeBase,
    pub union_type: UnionType,
    pub union_or_intersection_type: UnionOrIntersectionType,
    pub type_base: TypeBase,

    pub promiseTypeOfPromiseConstructor: Option<TypeId>,
    pub promisedTypeOfPromise: Option<TypeId>,
    pub awaitedTypeOfType: Option<TypeId>,
}

impl AsTypeBase for PromiseOrAwaitableType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct SyntheticDefaultModuleType {
    pub type_base: TypeBase,

    pub syntheticType: Option<TypeId>,
}

impl AsTypeBase for SyntheticDefaultModuleType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct InstantiableType {
    pub resolvedBaseConstraint: Option<TypeId>,
    pub resolvedIndexType: Option<TypeId>,
    pub resolvedStringIndexType: Option<TypeId>,
}

// Type parameters (TypeFlags.TypeParameter)
#[derive(Debug)]

pub struct TypeParameter {
    pub instantiable_type: InstantiableType,
    pub type_base: TypeBase,

    /** Retrieve using getConstraintFromTypeParameter */
    pub constraint: Option<TypeId>, // Constraint
    pub default: Option<TypeId>,
    pub target: Option<TypeId>,     // Instantiation target
    pub mapper: Option<TypeMapper>, // Instantiation mapper
    pub isThisType: bool,
    pub resolvedDefaultType: Option<TypeId>,
}

impl AsTypeBase for TypeParameter {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
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

// Indexed access types (TypeFlags.IndexedAccess)
// Possible forms are T[xxx], xxx[T], or xxx[keyof T], where T is a type variable
#[derive(Debug)]
pub struct IndexedAccessType {
    pub instantiable_type: InstantiableType,
    pub type_base: TypeBase,

    pub objectType: TypeId,
    pub indexType: TypeId,
    pub accessFlags: AccessFlags, // Only includes AccessFlags.Persistent
    pub constraint: Option<TypeId>,
    pub simplifiedForReading: Option<TypeId>,
    pub simplifiedForWriting: Option<TypeId>,
}

impl AsTypeBase for IndexedAccessType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

// export type TypeVariable = TypeParameter | IndexedAccessType;

// keyof T types (TypeFlags.Index)
#[derive(Debug)]
pub struct IndexType {
    pub instantiable_type: InstantiableType,
    pub type_base: TypeBase,

    // TODO:
    // ty: InstantiableType | UnionOrIntersectionType,
    pub ty: TypeId,
    pub stringsOnly: bool,
}

impl AsTypeBase for IndexType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct ConditionalRoot {
    // node: ConditionalTypeNode,
    pub node: BoundNode,
    pub checkType: TypeId,
    pub extendsType: TypeId,
    pub isDistributive: bool,
    pub inferTypeParameters: Vec<TypeId>,
    pub outerTypeParameters: Vec<TypeId>,
    // instantiations?: Map<Type>,
    pub aliasSymbol: Option<SymbolId>,
    pub aliasTypeArguments: Vec<TypeId>,
}

// T extends U ? X : Y (TypeFlags.Conditional)
#[derive(Debug)]
pub struct ConditionalType {
    pub instantiable_type: InstantiableType,
    pub type_base: TypeBase,

    pub root: ConditionalRoot,
    pub checkType: TypeId,
    pub extendsType: TypeId,
    pub resolvedTrueType: Option<TypeId>,
    pub resolvedFalseType: Option<TypeId>,
    pub resolvedInferredTrueType: Option<TypeId>, // The `trueType` instantiated with the `combinedMapper`, if present
    pub resolvedDefaultConstraint: Option<TypeId>,
    pub mapper: Option<TypeMapper>,
    pub combinedMapper: Option<TypeMapper>,
}

impl AsTypeBase for ConditionalType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct TemplateLiteralType {
    pub instantiable_type: InstantiableType,
    pub type_base: TypeBase,

    pub texts: Vec<JsWord>, // Always one element longer than types
    pub types: Vec<TypeId>, // Always at least one element
}

impl AsTypeBase for TemplateLiteralType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

#[derive(Debug)]
pub struct StringMappingType {
    pub instantiable_type: InstantiableType,
    pub type_base: TypeBase,

    pub ty: TypeId,
    pub symbol: SymbolId,
}

impl AsTypeBase for StringMappingType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}

// Type parameter substitution (TypeFlags.Substitution)
// Substitution types are created for type parameters or indexed access types that occur in the
// true branch of a conditional type. For example, in 'T extends string ? Foo<T> : Bar<T>', the
// reference to T in Foo<T> is resolved as a substitution type that substitutes 'string & T' for T.
// Thus, if Foo has a 'string' constraint on its type parameter, T will satisfy it. Substitution
// types disappear upon instantiation (just like type parameters).
#[derive(Debug)]
pub struct SubstitutionType {
    pub instantiable_type: InstantiableType,
    pub type_base: TypeBase,

    pub objectFlags: ObjectFlags,
    pub baseType: TypeId,   // Target type
    pub substitute: TypeId, // Type to substitute for type parameter
}

impl AsTypeBase for SubstitutionType {
    fn get_flags(&self) -> &TypeFlags {
        &self.type_base.flags
    }
    fn get_symbol(&self) -> &Option<SymbolId> {
        &self.type_base.symbol
    }
    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        &self.type_base.aliasSymbol
    }
    fn get_aliasTypeArguments(&self) -> &Vec<TypeId> {
        &self.type_base.aliasTypeArguments
    }
    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.permissiveInstantiation
    }
    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        &self.type_base.restrictiveInstantiation
    }
    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        &self.type_base.immediateBaseConstraint
    }
    fn get_widened(&self) -> &Option<TypeId> {
        &self.type_base.widened
    }
}
