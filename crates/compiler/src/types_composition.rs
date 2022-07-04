use std::rc::Rc;

use crate::node::BoundNode;
use crate::types::*;
use ahash::AHashMap;
use bitflags::bitflags;
use num_bigint::BigInt as BigIntValue;
use swc_atoms::JsWord;

// Properties common to all types
#[derive(Debug, Default)]
pub struct TypeBase {
    pub flags: TypeFlags,                            // Flags
    pub symbol: Option<SymbolId>,                    // Symbol associated with type (if any)
    pub pattern: Option<BoundNode>, // Destructuring pattern represented by type (if any)
    pub aliasSymbol: Option<SymbolId>, // Alias associated with type
    pub aliasTypeArguments: Option<Rc<Vec<TypeId>>>, // Alias type arguments (if any)
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
            ..Default::default()
        }
    }
}

pub trait AsTypeBase {
    fn get_flags(&self) -> TypeFlags;
    fn get_flags_mut(&mut self) -> &mut TypeFlags;

    fn get_symbol(&self) -> &Option<SymbolId>;
    fn get_symbol_mut(&mut self) -> &mut Option<SymbolId>;

    fn get_pattern(&self) -> &Option<BoundNode>;
    fn get_pattern_mut(&mut self) -> &mut Option<BoundNode>;

    fn get_aliasSymbol(&self) -> &Option<SymbolId>;
    fn get_aliasSymbol_mut(&mut self) -> &mut Option<SymbolId>;

    fn get_aliasTypeArguments(&self) -> &Option<Rc<Vec<TypeId>>>;
    fn get_aliasTypeArguments_mut(&mut self) -> &mut Option<Rc<Vec<TypeId>>>;

    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool;
    fn get_aliasTypeArgumentsContainsMarker_mut(&mut self) -> &mut bool;

    fn get_permissiveInstantiation(&self) -> &Option<TypeId>;
    fn get_permissiveInstantiation_mut(&mut self) -> &mut Option<TypeId>;

    fn get_restrictiveInstantiation(&self) -> &Option<TypeId>;
    fn get_restrictiveInstantiation_mut(&mut self) -> &mut Option<TypeId>;

    fn get_immediateBaseConstraint(&self) -> &Option<TypeId>;
    fn get_immediateBaseConstraint_mut(&mut self) -> &mut Option<TypeId>;

    fn get_widened(&self) -> &Option<TypeId>;
    fn get_widened_mut(&mut self) -> &mut Option<TypeId>;
}

macro_rules! impl_AsTypeBase {
    [$($ty:ident $(<$gen:ident>)? $(,)?)*] => {
        $(
            impl $(<$gen>)? AsTypeBase for $ty$(<$gen>)? {
                fn get_flags(&self) -> TypeFlags {
                    self.type_base.flags
                }
                fn get_flags_mut(&mut self) -> &mut TypeFlags {
                    &mut self.type_base.flags
                }
                fn get_symbol(&self) -> &Option<SymbolId> {
                    &self.type_base.symbol
                }
                fn get_symbol_mut(&mut self) -> &mut Option<SymbolId> {
                    &mut self.type_base.symbol
                }
                fn get_pattern(&self) -> &Option<BoundNode> {
                    &self.type_base.pattern
                }
                fn get_pattern_mut(&mut self) -> &mut Option<BoundNode> {
                    &mut self.type_base.pattern
                }
                fn get_aliasSymbol(&self) -> &Option<SymbolId> {
                    &self.type_base.aliasSymbol
                }
                fn get_aliasSymbol_mut(&mut self) -> &mut Option<SymbolId> {
                    &mut self.type_base.aliasSymbol
                }
                fn get_aliasTypeArguments(&self) -> &Option<Rc<Vec<TypeId>>> {
                    &self.type_base.aliasTypeArguments
                }
                fn get_aliasTypeArguments_mut(&mut self) -> &mut Option<Rc<Vec<TypeId>>> {
                    &mut self.type_base.aliasTypeArguments
                }
                fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
                    self.type_base.aliasTypeArgumentsContainsMarker
                }
                fn get_aliasTypeArgumentsContainsMarker_mut(&mut self) -> &mut bool {
                    &mut self.type_base.aliasTypeArgumentsContainsMarker
                }
                fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
                    &self.type_base.permissiveInstantiation
                }
                fn get_permissiveInstantiation_mut(&mut self) -> &mut Option<TypeId> {
                    &mut self.type_base.permissiveInstantiation
                }
                fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
                    &self.type_base.restrictiveInstantiation
                }
                fn get_restrictiveInstantiation_mut(&mut self) -> &mut Option<TypeId> {
                    &mut self.type_base.restrictiveInstantiation
                }
                fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
                    &self.type_base.immediateBaseConstraint
                }
                fn get_immediateBaseConstraint_mut(&mut self) -> &mut Option<TypeId> {
                    &mut self.type_base.immediateBaseConstraint
                }
                fn get_widened(&self) -> &Option<TypeId> {
                    &self.type_base.widened
                }
                fn get_widened_mut(&mut self) -> &mut Option<TypeId> {
                    &mut self.type_base.widened
                }
            }
        )*
    }
}

impl_AsTypeBase![
    IntrinsicType,
    NullableType,
    FreshableIntrinsicType,
    UniqueESSymbolType,
    StringLiteralType,
    NumberLiteralType,
    BigIntLiteralType,
    EnumType,
    ObjectType,
    InterfaceType,
    InterfaceTypeWithDeclaredMembers,
    TypeReference,
    DeferredTypeReference,
    GenericType,
    TupleType,
    TupleTypeReference,
    UnionType,
    IntersectionType,
    AnonymousType,
    MappedType,
    EvolvingArrayType,
    ReverseMappedType,
    ResolvedType,
    // FreshObjectLiteralType,
    IterableOrIteratorType,
    PromiseOrAwaitableType,
    SyntheticDefaultModuleType,
    TypeParameter,
    IndexedAccessType,
    IndexType,
    ConditionalType,
    TemplateLiteralType,
    StringMappingType,
    SubstitutionType,
];

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
    InterfaceType(InterfaceType),
    InterfaceTypeWithDeclaredMembers(InterfaceTypeWithDeclaredMembers),
    TypeReference(TypeReference),
    DeferredTypeReference(DeferredTypeReference),
    GenericType(GenericType),
    TupleType(TupleType),
    TupleTypeReference(TupleTypeReference),
    UnionType(UnionType),
    IntersectionType(IntersectionType),
    AnonymousType(AnonymousType),
    MappedType(MappedType),
    EvolvingArrayType(EvolvingArrayType),
    ReverseMappedType(ReverseMappedType),
    ResolvedType(ResolvedType),
    FreshObjectLiteralType(FreshObjectLiteralType),
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
    Dummy,
}

impl Type {
    pub fn unwrap_as_interface_type(&self) -> &InterfaceTypeBase {
        match self {
            Type::InterfaceType(t) => &t.interface_type,
            Type::InterfaceTypeWithDeclaredMembers(t) => &t.interface_type,
            Type::GenericType(t) => &t.interface_type,
            Type::TupleType(t) => &t.interface_type,
            Type::UnionType(_) => todo!(),
            Type::IntersectionType(_) => todo!(),
            Type::ResolvedType(t) => match &t.old {
                Some(Old::GenericType { interface_type, .. }) => interface_type,
                Some(Old::InterfaceType { interface_type, .. }) => interface_type,
                _ => {
                    dbg!(t);
                    todo!()
                }
            },
            _ => unreachable!(),
        }
    }
    pub fn unwrap_as_interface_type_mut(&mut self) -> &mut InterfaceTypeBase {
        match self {
            Type::InterfaceType(t) => &mut t.interface_type,
            Type::InterfaceTypeWithDeclaredMembers(t) => &mut t.interface_type,
            Type::GenericType(t) => &mut t.interface_type,
            Type::TupleType(t) => &mut t.interface_type,
            Type::UnionType(_) => todo!(),
            Type::IntersectionType(_) => todo!(),
            Type::ResolvedType(t) => match &mut t.old {
                Some(Old::GenericType { interface_type, .. }) => interface_type,
                _ => todo!(),
            },
            _ => unreachable!(),
        }
    }
    pub fn get_object_flags(&self) -> ObjectFlags {
        match self {
            Type::IntrinsicType(t) => t.intrinsic_type_base.objectFlags,
            Type::NullableType(t) => t.intrinsic_type_base.objectFlags,
            Type::FreshableIntrinsicType(t) => t.intrinsic_type_base.objectFlags,
            Type::UniqueESSymbolType(_) => Default::default(),
            Type::StringLiteralType(_) => Default::default(),
            Type::NumberLiteralType(_) => Default::default(),
            Type::BigIntLiteralType(_) => Default::default(),
            Type::EnumType(_) => todo!(),
            Type::ObjectType(t) => t.object_type_base.objectFlags,
            Type::InterfaceType(t) => t.object_type_base.objectFlags,
            Type::InterfaceTypeWithDeclaredMembers(t) => t.object_type_base.objectFlags,
            Type::TypeReference(t) => t.object_type_base.objectFlags,
            Type::DeferredTypeReference(t) => t.object_type_base.objectFlags,
            Type::GenericType(t) => t.object_type_base.objectFlags,
            Type::TupleType(t) => t.object_type_base.objectFlags,
            Type::TupleTypeReference(t) => t.object_type_base.objectFlags,
            Type::UnionType(t) => t.union_or_intersection_type.objectFlags,
            Type::IntersectionType(t) => t.union_or_intersection_type.objectFlags,
            Type::AnonymousType(t) => t.object_type_base.objectFlags,
            Type::MappedType(t) => t.object_type_base.objectFlags,
            Type::EvolvingArrayType(t) => t.object_type_base.objectFlags,
            Type::ReverseMappedType(t) => t.object_type_base.objectFlags,
            Type::ResolvedType(t) => t.object_type_base.objectFlags,
            Type::FreshObjectLiteralType(t) => t.resolved_type.object_type_base.objectFlags,
            Type::IterableOrIteratorType(t) => t.object_type_base.objectFlags,
            Type::PromiseOrAwaitableType(t) => t.object_type_base.objectFlags,
            Type::SyntheticDefaultModuleType(_) => todo!(),
            Type::TypeParameter(_) => Default::default(),
            Type::IndexedAccessType(_) => todo!(),
            Type::IndexType(_) => Default::default(),
            Type::ConditionalType(_) => Default::default(),
            Type::TemplateLiteralType(_) => todo!(),
            Type::StringMappingType(_) => todo!(),
            Type::SubstitutionType(_) => todo!(),
            Type::Dummy => unreachable!(),
        }
    }
    pub fn get_object_flags_mut(&mut self) -> &mut ObjectFlags {
        match self {
            Type::IntrinsicType(t) => &mut t.intrinsic_type_base.objectFlags,
            Type::NullableType(t) => &mut t.intrinsic_type_base.objectFlags,
            Type::FreshableIntrinsicType(t) => &mut t.intrinsic_type_base.objectFlags,
            Type::UniqueESSymbolType(_) => todo!(),
            Type::StringLiteralType(_) => todo!(),
            Type::NumberLiteralType(_) => todo!(),
            Type::BigIntLiteralType(_) => todo!(),
            Type::EnumType(_) => todo!(),
            Type::ObjectType(t) => &mut t.object_type_base.objectFlags,
            Type::InterfaceType(t) => &mut t.object_type_base.objectFlags,
            Type::InterfaceTypeWithDeclaredMembers(t) => &mut t.object_type_base.objectFlags,
            Type::TypeReference(t) => &mut t.object_type_base.objectFlags,
            Type::DeferredTypeReference(t) => &mut t.object_type_base.objectFlags,
            Type::GenericType(t) => &mut t.object_type_base.objectFlags,
            Type::TupleType(t) => &mut t.object_type_base.objectFlags,
            Type::TupleTypeReference(t) => &mut t.object_type_base.objectFlags,
            Type::UnionType(t) => &mut t.union_or_intersection_type.objectFlags,
            Type::IntersectionType(t) => &mut t.union_or_intersection_type.objectFlags,
            Type::AnonymousType(t) => &mut t.object_type_base.objectFlags,
            Type::MappedType(t) => &mut t.object_type_base.objectFlags,
            Type::EvolvingArrayType(t) => &mut t.object_type_base.objectFlags,
            Type::ReverseMappedType(t) => &mut t.object_type_base.objectFlags,
            Type::ResolvedType(t) => &mut t.object_type_base.objectFlags,
            Type::FreshObjectLiteralType(t) => &mut t.resolved_type.object_type_base.objectFlags,
            Type::IterableOrIteratorType(t) => &mut t.object_type_base.objectFlags,
            Type::PromiseOrAwaitableType(t) => &mut t.object_type_base.objectFlags,
            Type::SyntheticDefaultModuleType(_) => todo!(),
            Type::TypeParameter(_) => todo!(),
            Type::IndexedAccessType(_) => todo!(),
            Type::IndexType(_) => todo!(),
            Type::ConditionalType(_) => todo!(),
            Type::TemplateLiteralType(_) => todo!(),
            Type::StringMappingType(_) => todo!(),
            Type::SubstitutionType(_) => todo!(),
            Type::Dummy => unreachable!(),
        }
    }
    pub fn resolvedBaseConstraint(&self) -> Option<TypeId> {
        match self {
            Type::IntrinsicType(_) => todo!(),
            Type::NullableType(_) => todo!(),
            Type::FreshableIntrinsicType(_) => todo!(),
            Type::UniqueESSymbolType(_) => todo!(),
            Type::StringLiteralType(_) => todo!(),
            Type::NumberLiteralType(_) => todo!(),
            Type::BigIntLiteralType(_) => todo!(),
            Type::EnumType(_) => todo!(),
            Type::ObjectType(_) => todo!(),
            Type::InterfaceType(_) => todo!(),
            Type::InterfaceTypeWithDeclaredMembers(_) => todo!(),
            Type::TypeReference(_) => todo!(),
            Type::DeferredTypeReference(_) => todo!(),
            Type::GenericType(_) => todo!(),
            Type::TupleType(_) => todo!(),
            Type::TupleTypeReference(_) => todo!(),
            Type::UnionType(t) => t.union_or_intersection_type.resolvedBaseConstraint,
            Type::IntersectionType(_) => todo!(),
            Type::AnonymousType(_) => todo!(),
            Type::MappedType(_) => todo!(),
            Type::EvolvingArrayType(_) => todo!(),
            Type::ReverseMappedType(_) => todo!(),
            Type::ResolvedType(t) => t.union_or_intersection_type.resolvedBaseConstraint,
            Type::FreshObjectLiteralType(_) => todo!(),
            Type::IterableOrIteratorType(t) => t.union_or_intersection_type.resolvedBaseConstraint,
            Type::PromiseOrAwaitableType(t) => t.union_or_intersection_type.resolvedBaseConstraint,
            Type::SyntheticDefaultModuleType(_) => todo!(),
            Type::TypeParameter(t) => t.instantiable_type.resolvedBaseConstraint,
            Type::IndexedAccessType(t) => t.instantiable_type.resolvedBaseConstraint,
            Type::IndexType(t) => t.instantiable_type.resolvedBaseConstraint,
            Type::ConditionalType(t) => t.instantiable_type.resolvedBaseConstraint,
            Type::TemplateLiteralType(t) => t.instantiable_type.resolvedBaseConstraint,
            Type::StringMappingType(t) => t.instantiable_type.resolvedBaseConstraint,
            Type::SubstitutionType(t) => t.instantiable_type.resolvedBaseConstraint,
            Type::Dummy => unreachable!(),
        }
    }
    pub fn resolvedBaseConstraintMut(&mut self) -> &mut Option<TypeId> {
        match self {
            Type::IntrinsicType(_) => todo!(),
            Type::NullableType(_) => todo!(),
            Type::FreshableIntrinsicType(_) => todo!(),
            Type::UniqueESSymbolType(_) => todo!(),
            Type::StringLiteralType(_) => todo!(),
            Type::NumberLiteralType(_) => todo!(),
            Type::BigIntLiteralType(_) => todo!(),
            Type::EnumType(_) => todo!(),
            Type::ObjectType(_) => todo!(),
            Type::InterfaceType(_) => todo!(),
            Type::InterfaceTypeWithDeclaredMembers(_) => todo!(),
            Type::TypeReference(_) => todo!(),
            Type::DeferredTypeReference(_) => todo!(),
            Type::GenericType(_) => todo!(),
            Type::TupleType(_) => todo!(),
            Type::TupleTypeReference(_) => todo!(),
            Type::UnionType(t) => &mut t.union_or_intersection_type.resolvedBaseConstraint,
            Type::IntersectionType(_) => todo!(),
            Type::AnonymousType(_) => todo!(),
            Type::MappedType(_) => todo!(),
            Type::EvolvingArrayType(_) => todo!(),
            Type::ReverseMappedType(_) => todo!(),
            Type::ResolvedType(t) => &mut t.union_or_intersection_type.resolvedBaseConstraint,
            Type::FreshObjectLiteralType(_) => todo!(),
            Type::IterableOrIteratorType(t) => {
                &mut t.union_or_intersection_type.resolvedBaseConstraint
            }
            Type::PromiseOrAwaitableType(t) => {
                &mut t.union_or_intersection_type.resolvedBaseConstraint
            }
            Type::SyntheticDefaultModuleType(_) => todo!(),
            Type::TypeParameter(t) => &mut t.instantiable_type.resolvedBaseConstraint,
            Type::IndexedAccessType(t) => &mut t.instantiable_type.resolvedBaseConstraint,
            Type::IndexType(t) => &mut t.instantiable_type.resolvedBaseConstraint,
            Type::ConditionalType(t) => &mut t.instantiable_type.resolvedBaseConstraint,
            Type::TemplateLiteralType(t) => &mut t.instantiable_type.resolvedBaseConstraint,
            Type::StringMappingType(t) => &mut t.instantiable_type.resolvedBaseConstraint,
            Type::SubstitutionType(t) => &mut t.instantiable_type.resolvedBaseConstraint,
            Type::Dummy => unreachable!(),
        }
    }
    pub fn unwrap_as_type_reference(&self) -> &TypeReferenceBase {
        match self {
            Type::IntrinsicType(_) => todo!(),
            Type::NullableType(_) => todo!(),
            Type::FreshableIntrinsicType(_) => todo!(),
            Type::UniqueESSymbolType(_) => todo!(),
            Type::StringLiteralType(_) => todo!(),
            Type::NumberLiteralType(_) => todo!(),
            Type::BigIntLiteralType(_) => todo!(),
            Type::EnumType(_) => todo!(),
            Type::ObjectType(_) => todo!(),
            Type::InterfaceType(_) => todo!(),
            Type::InterfaceTypeWithDeclaredMembers(_) => todo!(),
            Type::TypeReference(t) => &t.type_reference,
            Type::DeferredTypeReference(t) => &t.type_reference,
            Type::GenericType(t) => &t.type_reference,
            Type::TupleType(t) => &t.type_reference,
            Type::TupleTypeReference(_) => todo!(),
            Type::UnionType(_) => todo!(),
            Type::IntersectionType(_) => todo!(),
            Type::AnonymousType(_) => todo!(),
            Type::MappedType(_) => todo!(),
            Type::EvolvingArrayType(_) => todo!(),
            Type::ReverseMappedType(_) => todo!(),
            Type::ResolvedType(t) => match &t.old {
                Some(Old::TypeReference { type_reference }) => type_reference,
                Some(Old::GenericType { type_reference, .. }) => type_reference,
                _ => todo!(),
            },
            Type::FreshObjectLiteralType(_) => todo!(),
            Type::IterableOrIteratorType(_) => todo!(),
            Type::PromiseOrAwaitableType(_) => todo!(),
            Type::SyntheticDefaultModuleType(_) => todo!(),
            Type::TypeParameter(_) => todo!(),
            Type::IndexedAccessType(_) => todo!(),
            Type::IndexType(_) => todo!(),
            Type::ConditionalType(_) => todo!(),
            Type::TemplateLiteralType(_) => todo!(),
            Type::StringMappingType(_) => todo!(),
            Type::SubstitutionType(_) => todo!(),
            Type::Dummy => unreachable!(),
        }
    }
    pub fn unwrap_as_type_reference_mut(&mut self) -> &mut TypeReferenceBase {
        match self {
            Type::IntrinsicType(_) => todo!(),
            Type::NullableType(_) => todo!(),
            Type::FreshableIntrinsicType(_) => todo!(),
            Type::UniqueESSymbolType(_) => todo!(),
            Type::StringLiteralType(_) => todo!(),
            Type::NumberLiteralType(_) => todo!(),
            Type::BigIntLiteralType(_) => todo!(),
            Type::EnumType(_) => todo!(),
            Type::ObjectType(_) => todo!(),
            Type::InterfaceType(_) => todo!(),
            Type::InterfaceTypeWithDeclaredMembers(_) => todo!(),
            Type::TypeReference(t) => &mut t.type_reference,
            Type::DeferredTypeReference(t) => &mut t.type_reference,
            Type::GenericType(t) => &mut t.type_reference,
            Type::TupleType(t) => &mut t.type_reference,
            Type::TupleTypeReference(_) => todo!(),
            Type::UnionType(_) => todo!(),
            Type::IntersectionType(_) => todo!(),
            Type::AnonymousType(_) => todo!(),
            Type::MappedType(_) => todo!(),
            Type::EvolvingArrayType(_) => todo!(),
            Type::ReverseMappedType(_) => todo!(),
            Type::ResolvedType(_) => todo!(),
            Type::FreshObjectLiteralType(_) => todo!(),
            Type::IterableOrIteratorType(_) => todo!(),
            Type::PromiseOrAwaitableType(_) => todo!(),
            Type::SyntheticDefaultModuleType(_) => todo!(),
            Type::TypeParameter(_) => todo!(),
            Type::IndexedAccessType(_) => todo!(),
            Type::IndexType(_) => todo!(),
            Type::ConditionalType(_) => todo!(),
            Type::TemplateLiteralType(_) => todo!(),
            Type::StringMappingType(_) => todo!(),
            Type::SubstitutionType(_) => todo!(),
            Type::Dummy => unreachable!(),
        }
    }
    pub fn unwrap_as_union_or_intersection(&self) -> &UnionOrIntersectionType {
        match self {
            Type::UnionType(t) => &t.union_or_intersection_type,
            Type::IntersectionType(t) => &t.union_or_intersection_type,
            Type::ResolvedType(t) => &t.union_or_intersection_type,
            Type::IterableOrIteratorType(t) => &t.union_or_intersection_type,
            Type::PromiseOrAwaitableType(t) => &t.union_or_intersection_type,
            Type::Dummy => unreachable!(),
            _ => todo!(),
        }
    }
    pub fn unwrap_as_union_or_intersection_mut(&mut self) -> &mut UnionOrIntersectionType {
        match self {
            Type::UnionType(t) => &mut t.union_or_intersection_type,
            Type::IntersectionType(t) => &mut t.union_or_intersection_type,
            Type::ResolvedType(t) => &mut t.union_or_intersection_type,
            Type::IterableOrIteratorType(t) => &mut t.union_or_intersection_type,
            Type::PromiseOrAwaitableType(t) => &mut t.union_or_intersection_type,
            Type::Dummy => unreachable!(),
            _ => todo!(),
        }
    }

    pub fn get_literal_fresh_type(&self) -> Option<TypeId> {
        match self {
            Type::StringLiteralType(t) => t.literal_type.freshType,
            Type::NumberLiteralType(t) => t.literal_type.freshType,
            Type::BigIntLiteralType(t) => t.literal_type.freshType,
            Type::FreshableIntrinsicType(t) => Some(t.freshType),
            _ => unreachable!(),
        }
    }
    pub fn set_literal_fresh_type(&mut self, new_fresh_type: TypeId) {
        match self {
            Type::StringLiteralType(t) => t.literal_type.freshType = Some(new_fresh_type),
            Type::NumberLiteralType(t) => t.literal_type.freshType = Some(new_fresh_type),
            Type::BigIntLiteralType(t) => t.literal_type.freshType = Some(new_fresh_type),
            Type::FreshableIntrinsicType(t) => t.freshType = new_fresh_type,
            _ => unreachable!(),
        }
    }
    pub fn get_literal_regular_type(&self) -> TypeId {
        match self {
            Type::StringLiteralType(t) => t.literal_type.regularType,
            Type::NumberLiteralType(t) => t.literal_type.regularType,
            Type::BigIntLiteralType(t) => t.literal_type.regularType,
            Type::FreshableIntrinsicType(t) => t.regularType,
            _ => unreachable!(),
        }
    }

    // pub fn unwrap_as_literal_type(&self) -> &LiteralType {
    //     match self {
    //         Type::StringLiteralType(t) => &t.literal_type,
    //         Type::NumberLiteralType(t) => &t.literal_type,
    //         Type::BigIntLiteralType(t) => &t.literal_type,
    //         Type::FreshableIntrinsicType(t) => todo!(),
    //         _ => unreachable!(),
    //     }
    // }
    // pub fn unwrap_as_literal_type_mut(&mut self) -> &mut LiteralType {
    //     match self {
    //         Type::StringLiteralType(t) => &mut t.literal_type,
    //         Type::NumberLiteralType(t) => &mut t.literal_type,
    //         Type::BigIntLiteralType(t) => &mut t.literal_type,
    //         Type::FreshableIntrinsicType(t) => todo!(),
    //         _ => unreachable!(),
    //     }
    // }

    pub fn unwrap_as_generic_type_base(&self) -> &GenericTypeBase {
        match self {
            Type::GenericType(t) => &t.generic_type_base,
            Type::TupleType(t) => &t.generic_type_base,
            // TODO:
            Type::ResolvedType(_) => todo!(),
            _ => unreachable!(),
        }
    }
    pub fn unwrap_as_generic_type_base_mut(&mut self) -> &mut GenericTypeBase {
        match self {
            Type::GenericType(t) => &mut t.generic_type_base,
            Type::TupleType(t) => &mut t.generic_type_base,
            // TODO:
            Type::ResolvedType(_) => todo!(),
            _ => unreachable!(),
        }
    }

    pub fn unwrap_instantiations(&self) -> &AHashMap<TypeList, TypeId> {
        match self {
            Type::DeferredTypeReference(t) => &t.instantiations,
            Type::GenericType(t) => &t.generic_type_base.instantiations,
            Type::TupleType(t) => &t.generic_type_base.instantiations,
            Type::ResolvedType(t) => match t.old.as_ref().unwrap() {
                Old::InterfaceType { .. } => todo!(),
                Old::TypeReference { .. } => todo!(),
                Old::GenericType {
                    generic_type_base, ..
                } => &generic_type_base.instantiations,
                Old::AnonymousType { anonymous_type } => &anonymous_type.instantiations,
                Old::UnionType { .. } => todo!(),
            },
            Type::AnonymousType(t) => &t.anonymous_type.instantiations,
            Type::MappedType(t) => &t.anonymous_type.instantiations,
            _ => unreachable!(),
        }
    }
    pub fn unwrap_instantiations_mut(&mut self) -> &mut AHashMap<TypeList, TypeId> {
        match self {
            Type::DeferredTypeReference(t) => &mut t.instantiations,
            Type::GenericType(t) => &mut t.generic_type_base.instantiations,
            Type::TupleType(t) => &mut t.generic_type_base.instantiations,
            Type::ResolvedType(_) => todo!(),
            Type::AnonymousType(t) => &mut t.anonymous_type.instantiations,
            Type::MappedType(t) => &mut t.anonymous_type.instantiations,
            _ => unreachable!(),
        }
    }

    pub fn get_type_mapper(&self) -> Option<&Rc<TypeMapper>> {
        match self {
            Type::TupleTypeReference(t) => t.type_reference.mapper.as_ref(),
            Type::TupleType(t) => t.type_reference.mapper.as_ref(),
            Type::GenericType(t) => t.type_reference.mapper.as_ref(),
            Type::DeferredTypeReference(t) => t.type_reference.mapper.as_ref(),
            Type::TypeReference(t) => t.type_reference.mapper.as_ref(),
            Type::AnonymousType(t) => t.anonymous_type.mapper.as_ref(),
            Type::MappedType(t) => t.anonymous_type.mapper.as_ref(),
            Type::TypeParameter(t) => t.mapper.as_ref(),
            Type::ConditionalType(t) => t.mapper.as_ref(),
            Type::ResolvedType(_) => todo!(),
            _ => None,
        }
    }

    pub fn unwrap_as_union_type_base(&self) -> &UnionTypeBase {
        match self {
            Type::UnionType(t) => &t.union_type_base,
            Type::IterableOrIteratorType(t) => &t.union_type_base,
            Type::PromiseOrAwaitableType(t) => &t.union_type_base,
            Type::ResolvedType(_) => todo!(),
            _ => unreachable!(),
        }
    }
    pub fn unwrap_as_union_type_base_mut(&mut self) -> &mut UnionTypeBase {
        match self {
            Type::UnionType(t) => &mut t.union_type_base,
            Type::IterableOrIteratorType(t) => &mut t.union_type_base,
            Type::PromiseOrAwaitableType(t) => &mut t.union_type_base,
            Type::ResolvedType(_) => todo!(),
            _ => unreachable!(),
        }
    }

    pub fn as_instantiable_type_mut(&mut self) -> Option<&mut InstantiableType> {
        match self {
            Type::TypeParameter(t) => Some(&mut t.instantiable_type),
            Type::IndexedAccessType(t) => Some(&mut t.instantiable_type),
            Type::IndexType(t) => Some(&mut t.instantiable_type),
            Type::ConditionalType(t) => Some(&mut t.instantiable_type),
            Type::TemplateLiteralType(t) => Some(&mut t.instantiable_type),
            Type::StringMappingType(t) => Some(&mut t.instantiable_type),
            Type::SubstitutionType(t) => Some(&mut t.instantiable_type),
            Type::ResolvedType(_) => todo!(),
            _ => None,
        }
    }

    pub fn unwrap_object_type_base(&self) -> &ObjectTypeBase {
        match self {
            Type::ObjectType(t) => &t.object_type_base,
            Type::InterfaceType(t) => &t.object_type_base,
            Type::InterfaceTypeWithDeclaredMembers(t) => &t.object_type_base,
            Type::TypeReference(t) => &t.object_type_base,
            Type::DeferredTypeReference(t) => &t.object_type_base,
            Type::GenericType(t) => &t.object_type_base,
            Type::TupleType(t) => &t.object_type_base,
            Type::TupleTypeReference(t) => &t.object_type_base,
            Type::AnonymousType(t) => &t.object_type_base,
            Type::MappedType(t) => &t.object_type_base,
            Type::EvolvingArrayType(t) => &t.object_type_base,
            Type::ReverseMappedType(t) => &t.object_type_base,
            Type::ResolvedType(t) => &t.object_type_base,
            Type::IterableOrIteratorType(t) => &t.object_type_base,
            Type::PromiseOrAwaitableType(t) => &t.object_type_base,
            _ => unreachable!(),
        }
    }
    pub fn unwrap_object_type_base_mut(&mut self) -> &mut ObjectTypeBase {
        match self {
            Type::ObjectType(t) => &mut t.object_type_base,
            Type::InterfaceType(t) => &mut t.object_type_base,
            Type::InterfaceTypeWithDeclaredMembers(t) => &mut t.object_type_base,
            Type::TypeReference(t) => &mut t.object_type_base,
            Type::DeferredTypeReference(t) => &mut t.object_type_base,
            Type::GenericType(t) => &mut t.object_type_base,
            Type::TupleType(t) => &mut t.object_type_base,
            Type::TupleTypeReference(t) => &mut t.object_type_base,
            Type::AnonymousType(t) => &mut t.object_type_base,
            Type::MappedType(t) => &mut t.object_type_base,
            Type::EvolvingArrayType(t) => &mut t.object_type_base,
            Type::ReverseMappedType(t) => &mut t.object_type_base,
            Type::ResolvedType(t) => &mut t.object_type_base,
            Type::IterableOrIteratorType(t) => &mut t.object_type_base,
            Type::PromiseOrAwaitableType(t) => &mut t.object_type_base,
            _ => unreachable!(),
        }
    }
}

impl AsTypeBase for Type {
    fn get_flags(&self) -> TypeFlags {
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
            Type::InterfaceType(t) => t.get_flags(),
            Type::InterfaceTypeWithDeclaredMembers(t) => t.get_flags(),
            Type::TypeReference(t) => t.get_flags(),
            Type::DeferredTypeReference(t) => t.get_flags(),
            Type::GenericType(t) => t.get_flags(),
            Type::TupleType(t) => t.get_flags(),
            Type::TupleTypeReference(t) => t.get_flags(),
            Type::UnionType(t) => t.get_flags(),
            Type::IntersectionType(t) => t.get_flags(),
            Type::AnonymousType(t) => t.get_flags(),
            Type::MappedType(t) => t.get_flags(),
            Type::EvolvingArrayType(t) => t.get_flags(),
            Type::ReverseMappedType(t) => t.get_flags(),
            Type::ResolvedType(t) => t.get_flags(),
            Type::FreshObjectLiteralType(t) => t.resolved_type.get_flags(),
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
            Type::Dummy => unreachable!(),
        }
    }
    fn get_flags_mut(&mut self) -> &mut TypeFlags {
        todo!()
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
            Type::InterfaceType(t) => t.get_symbol(),
            Type::InterfaceTypeWithDeclaredMembers(t) => t.get_symbol(),
            Type::TypeReference(t) => t.get_symbol(),
            Type::DeferredTypeReference(t) => t.get_symbol(),
            Type::GenericType(t) => t.get_symbol(),
            Type::TupleType(t) => t.get_symbol(),
            Type::TupleTypeReference(t) => t.get_symbol(),
            Type::UnionType(t) => t.get_symbol(),
            Type::IntersectionType(t) => t.get_symbol(),
            Type::AnonymousType(t) => t.get_symbol(),
            Type::MappedType(t) => t.get_symbol(),
            Type::EvolvingArrayType(t) => t.get_symbol(),
            Type::ReverseMappedType(t) => t.get_symbol(),
            Type::ResolvedType(t) => t.get_symbol(),
            Type::FreshObjectLiteralType(t) => t.resolved_type.get_symbol(),
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
            Type::Dummy => unreachable!(),
        }
    }
    fn get_symbol_mut(&mut self) -> &mut Option<SymbolId> {
        todo!()
    }

    fn get_pattern(&self) -> &Option<BoundNode> {
        match self {
            Type::IntrinsicType(t) => t.get_pattern(),
            Type::NullableType(t) => t.get_pattern(),
            Type::FreshableIntrinsicType(t) => t.get_pattern(),
            // Type::LiteralType(t) => t.get_pattern(),
            Type::UniqueESSymbolType(t) => t.get_pattern(),
            Type::StringLiteralType(t) => t.get_pattern(),
            Type::NumberLiteralType(t) => t.get_pattern(),
            Type::BigIntLiteralType(t) => t.get_pattern(),
            Type::EnumType(t) => t.get_pattern(),
            Type::ObjectType(t) => t.get_pattern(),
            Type::InterfaceType(t) => t.get_pattern(),
            Type::InterfaceTypeWithDeclaredMembers(t) => t.get_pattern(),
            Type::TypeReference(t) => t.get_pattern(),
            Type::DeferredTypeReference(t) => t.get_pattern(),
            Type::GenericType(t) => t.get_pattern(),
            Type::TupleType(t) => t.get_pattern(),
            Type::TupleTypeReference(t) => t.get_pattern(),
            Type::UnionType(t) => t.get_pattern(),
            Type::IntersectionType(t) => t.get_pattern(),
            Type::AnonymousType(t) => t.get_pattern(),
            Type::MappedType(t) => t.get_pattern(),
            Type::EvolvingArrayType(t) => t.get_pattern(),
            Type::ReverseMappedType(t) => t.get_pattern(),
            Type::ResolvedType(t) => t.get_pattern(),
            Type::FreshObjectLiteralType(t) => t.resolved_type.get_pattern(),
            // Type::IterationTypes(t) => t.get_pattern(),
            Type::IterableOrIteratorType(t) => t.get_pattern(),
            Type::PromiseOrAwaitableType(t) => t.get_pattern(),
            Type::SyntheticDefaultModuleType(t) => t.get_pattern(),
            // Type::InstantiableType(t) => t.get_pattern(),
            Type::TypeParameter(t) => t.get_pattern(),
            Type::IndexedAccessType(t) => t.get_pattern(),
            Type::IndexType(t) => t.get_pattern(),
            Type::ConditionalType(t) => t.get_pattern(),
            Type::TemplateLiteralType(t) => t.get_pattern(),
            Type::StringMappingType(t) => t.get_pattern(),
            Type::SubstitutionType(t) => t.get_pattern(),
            Type::Dummy => unreachable!(),
        }
    }
    fn get_pattern_mut(&mut self) -> &mut Option<BoundNode> {
        todo!()
    }

    fn get_aliasSymbol(&self) -> &Option<SymbolId> {
        match self {
            Type::IntrinsicType(t) => &t.type_base.aliasSymbol,
            Type::NullableType(t) => &t.type_base.aliasSymbol,
            Type::FreshableIntrinsicType(t) => &t.type_base.aliasSymbol,
            Type::UniqueESSymbolType(t) => &t.type_base.aliasSymbol,
            Type::StringLiteralType(t) => &t.type_base.aliasSymbol,
            Type::NumberLiteralType(t) => &t.type_base.aliasSymbol,
            Type::BigIntLiteralType(t) => &t.type_base.aliasSymbol,
            Type::EnumType(t) => &t.type_base.aliasSymbol,
            Type::ObjectType(t) => &t.type_base.aliasSymbol,
            Type::InterfaceType(t) => &t.type_base.aliasSymbol,
            Type::InterfaceTypeWithDeclaredMembers(t) => &t.type_base.aliasSymbol,
            Type::TypeReference(t) => &t.type_base.aliasSymbol,
            Type::DeferredTypeReference(t) => &t.type_base.aliasSymbol,
            Type::GenericType(t) => &t.type_base.aliasSymbol,
            Type::TupleType(t) => &t.type_base.aliasSymbol,
            Type::TupleTypeReference(t) => &t.type_base.aliasSymbol,
            Type::UnionType(t) => &t.type_base.aliasSymbol,
            Type::IntersectionType(t) => &t.type_base.aliasSymbol,
            Type::AnonymousType(t) => &t.type_base.aliasSymbol,
            Type::MappedType(t) => &t.type_base.aliasSymbol,
            Type::EvolvingArrayType(t) => &t.type_base.aliasSymbol,
            Type::ReverseMappedType(t) => &t.type_base.aliasSymbol,
            Type::ResolvedType(t) => &t.type_base.aliasSymbol,
            Type::FreshObjectLiteralType(_) => todo!(),
            Type::IterableOrIteratorType(t) => &t.type_base.aliasSymbol,
            Type::PromiseOrAwaitableType(t) => &t.type_base.aliasSymbol,
            Type::SyntheticDefaultModuleType(t) => &t.type_base.aliasSymbol,
            Type::TypeParameter(t) => &t.type_base.aliasSymbol,
            Type::IndexedAccessType(t) => &t.type_base.aliasSymbol,
            Type::IndexType(t) => &t.type_base.aliasSymbol,
            Type::ConditionalType(t) => &t.type_base.aliasSymbol,
            Type::TemplateLiteralType(t) => &t.type_base.aliasSymbol,
            Type::StringMappingType(t) => &t.type_base.aliasSymbol,
            Type::SubstitutionType(t) => &t.type_base.aliasSymbol,
            Type::Dummy => unreachable!(),
        }
    }
    fn get_aliasSymbol_mut(&mut self) -> &mut Option<SymbolId> {
        match self {
            Type::IntrinsicType(t) => &mut t.type_base.aliasSymbol,
            Type::NullableType(t) => &mut t.type_base.aliasSymbol,
            Type::FreshableIntrinsicType(t) => &mut t.type_base.aliasSymbol,
            Type::UniqueESSymbolType(t) => &mut t.type_base.aliasSymbol,
            Type::StringLiteralType(t) => &mut t.type_base.aliasSymbol,
            Type::NumberLiteralType(t) => &mut t.type_base.aliasSymbol,
            Type::BigIntLiteralType(t) => &mut t.type_base.aliasSymbol,
            Type::EnumType(t) => &mut t.type_base.aliasSymbol,
            Type::ObjectType(t) => &mut t.type_base.aliasSymbol,
            Type::InterfaceType(t) => &mut t.type_base.aliasSymbol,
            Type::InterfaceTypeWithDeclaredMembers(t) => &mut t.type_base.aliasSymbol,
            Type::TypeReference(t) => &mut t.type_base.aliasSymbol,
            Type::DeferredTypeReference(t) => &mut t.type_base.aliasSymbol,
            Type::GenericType(t) => &mut t.type_base.aliasSymbol,
            Type::TupleType(t) => &mut t.type_base.aliasSymbol,
            Type::TupleTypeReference(t) => &mut t.type_base.aliasSymbol,
            Type::UnionType(t) => &mut t.type_base.aliasSymbol,
            Type::IntersectionType(t) => &mut t.type_base.aliasSymbol,
            Type::AnonymousType(t) => &mut t.type_base.aliasSymbol,
            Type::MappedType(t) => &mut t.type_base.aliasSymbol,
            Type::EvolvingArrayType(t) => &mut t.type_base.aliasSymbol,
            Type::ReverseMappedType(t) => &mut t.type_base.aliasSymbol,
            Type::ResolvedType(t) => &mut t.type_base.aliasSymbol,
            Type::FreshObjectLiteralType(_) => todo!(),
            Type::IterableOrIteratorType(t) => &mut t.type_base.aliasSymbol,
            Type::PromiseOrAwaitableType(t) => &mut t.type_base.aliasSymbol,
            Type::SyntheticDefaultModuleType(t) => &mut t.type_base.aliasSymbol,
            Type::TypeParameter(t) => &mut t.type_base.aliasSymbol,
            Type::IndexedAccessType(t) => &mut t.type_base.aliasSymbol,
            Type::IndexType(t) => &mut t.type_base.aliasSymbol,
            Type::ConditionalType(t) => &mut t.type_base.aliasSymbol,
            Type::TemplateLiteralType(t) => &mut t.type_base.aliasSymbol,
            Type::StringMappingType(t) => &mut t.type_base.aliasSymbol,
            Type::SubstitutionType(t) => &mut t.type_base.aliasSymbol,
            Type::Dummy => unreachable!(),
        }
    }

    fn get_aliasTypeArguments(&self) -> &Option<Rc<Vec<TypeId>>> {
        match self {
            Type::IntrinsicType(t) => &t.type_base.aliasTypeArguments,
            Type::NullableType(t) => &t.type_base.aliasTypeArguments,
            Type::FreshableIntrinsicType(t) => &t.type_base.aliasTypeArguments,
            Type::UniqueESSymbolType(t) => &t.type_base.aliasTypeArguments,
            Type::StringLiteralType(t) => &t.type_base.aliasTypeArguments,
            Type::NumberLiteralType(t) => &t.type_base.aliasTypeArguments,
            Type::BigIntLiteralType(t) => &t.type_base.aliasTypeArguments,
            Type::EnumType(t) => &t.type_base.aliasTypeArguments,
            Type::ObjectType(t) => &t.type_base.aliasTypeArguments,
            Type::InterfaceType(t) => &t.type_base.aliasTypeArguments,
            Type::InterfaceTypeWithDeclaredMembers(t) => &t.type_base.aliasTypeArguments,
            Type::TypeReference(t) => &t.type_base.aliasTypeArguments,
            Type::DeferredTypeReference(t) => &t.type_base.aliasTypeArguments,
            Type::GenericType(t) => &t.type_base.aliasTypeArguments,
            Type::TupleType(t) => &t.type_base.aliasTypeArguments,
            Type::TupleTypeReference(t) => &t.type_base.aliasTypeArguments,
            Type::UnionType(t) => &t.type_base.aliasTypeArguments,
            Type::IntersectionType(t) => &t.type_base.aliasTypeArguments,
            Type::AnonymousType(t) => &t.type_base.aliasTypeArguments,
            Type::MappedType(t) => &t.type_base.aliasTypeArguments,
            Type::EvolvingArrayType(t) => &t.type_base.aliasTypeArguments,
            Type::ReverseMappedType(t) => &t.type_base.aliasTypeArguments,
            Type::ResolvedType(t) => &t.type_base.aliasTypeArguments,
            Type::FreshObjectLiteralType(_) => todo!(),
            Type::IterableOrIteratorType(t) => &t.type_base.aliasTypeArguments,
            Type::PromiseOrAwaitableType(t) => &t.type_base.aliasTypeArguments,
            Type::SyntheticDefaultModuleType(t) => &t.type_base.aliasTypeArguments,
            Type::TypeParameter(t) => &t.type_base.aliasTypeArguments,
            Type::IndexedAccessType(t) => &t.type_base.aliasTypeArguments,
            Type::IndexType(t) => &t.type_base.aliasTypeArguments,
            Type::ConditionalType(t) => &t.type_base.aliasTypeArguments,
            Type::TemplateLiteralType(t) => &t.type_base.aliasTypeArguments,
            Type::StringMappingType(t) => &t.type_base.aliasTypeArguments,
            Type::SubstitutionType(t) => &t.type_base.aliasTypeArguments,
            Type::Dummy => unreachable!(),
        }
    }
    fn get_aliasTypeArguments_mut(&mut self) -> &mut Option<Rc<Vec<TypeId>>> {
        match self {
            Type::IntrinsicType(t) => &mut t.type_base.aliasTypeArguments,
            Type::NullableType(t) => &mut t.type_base.aliasTypeArguments,
            Type::FreshableIntrinsicType(t) => &mut t.type_base.aliasTypeArguments,
            Type::UniqueESSymbolType(t) => &mut t.type_base.aliasTypeArguments,
            Type::StringLiteralType(t) => &mut t.type_base.aliasTypeArguments,
            Type::NumberLiteralType(t) => &mut t.type_base.aliasTypeArguments,
            Type::BigIntLiteralType(t) => &mut t.type_base.aliasTypeArguments,
            Type::EnumType(t) => &mut t.type_base.aliasTypeArguments,
            Type::ObjectType(t) => &mut t.type_base.aliasTypeArguments,
            Type::InterfaceType(t) => &mut t.type_base.aliasTypeArguments,
            Type::InterfaceTypeWithDeclaredMembers(t) => &mut t.type_base.aliasTypeArguments,
            Type::TypeReference(t) => &mut t.type_base.aliasTypeArguments,
            Type::DeferredTypeReference(t) => &mut t.type_base.aliasTypeArguments,
            Type::GenericType(t) => &mut t.type_base.aliasTypeArguments,
            Type::TupleType(t) => &mut t.type_base.aliasTypeArguments,
            Type::TupleTypeReference(t) => &mut t.type_base.aliasTypeArguments,
            Type::UnionType(t) => &mut t.type_base.aliasTypeArguments,
            Type::IntersectionType(t) => &mut t.type_base.aliasTypeArguments,
            Type::AnonymousType(t) => &mut t.type_base.aliasTypeArguments,
            Type::MappedType(t) => &mut t.type_base.aliasTypeArguments,
            Type::EvolvingArrayType(t) => &mut t.type_base.aliasTypeArguments,
            Type::ReverseMappedType(t) => &mut t.type_base.aliasTypeArguments,
            Type::ResolvedType(t) => &mut t.type_base.aliasTypeArguments,
            Type::FreshObjectLiteralType(_) => todo!(),
            Type::IterableOrIteratorType(t) => &mut t.type_base.aliasTypeArguments,
            Type::PromiseOrAwaitableType(t) => &mut t.type_base.aliasTypeArguments,
            Type::SyntheticDefaultModuleType(t) => &mut t.type_base.aliasTypeArguments,
            Type::TypeParameter(t) => &mut t.type_base.aliasTypeArguments,
            Type::IndexedAccessType(t) => &mut t.type_base.aliasTypeArguments,
            Type::IndexType(t) => &mut t.type_base.aliasTypeArguments,
            Type::ConditionalType(t) => &mut t.type_base.aliasTypeArguments,
            Type::TemplateLiteralType(t) => &mut t.type_base.aliasTypeArguments,
            Type::StringMappingType(t) => &mut t.type_base.aliasTypeArguments,
            Type::SubstitutionType(t) => &mut t.type_base.aliasTypeArguments,
            Type::Dummy => unreachable!(),
        }
    }

    fn get_aliasTypeArgumentsContainsMarker(&self) -> bool {
        todo!();
        // self.type_base.aliasTypeArgumentsContainsMarker
    }
    fn get_aliasTypeArgumentsContainsMarker_mut(&mut self) -> &mut bool {
        todo!()
    }

    fn get_permissiveInstantiation(&self) -> &Option<TypeId> {
        todo!();
        // &self.type_base.permissiveInstantiation
    }
    fn get_permissiveInstantiation_mut(&mut self) -> &mut Option<TypeId> {
        todo!()
    }

    fn get_restrictiveInstantiation(&self) -> &Option<TypeId> {
        todo!();
        // &self.type_base.restrictiveInstantiation
    }
    fn get_restrictiveInstantiation_mut(&mut self) -> &mut Option<TypeId> {
        todo!()
    }

    fn get_immediateBaseConstraint(&self) -> &Option<TypeId> {
        match self {
            Type::IntrinsicType(t) => t.get_immediateBaseConstraint(),
            Type::NullableType(t) => t.get_immediateBaseConstraint(),
            Type::FreshableIntrinsicType(t) => t.get_immediateBaseConstraint(),
            Type::UniqueESSymbolType(t) => t.get_immediateBaseConstraint(),
            Type::StringLiteralType(t) => t.get_immediateBaseConstraint(),
            Type::NumberLiteralType(t) => t.get_immediateBaseConstraint(),
            Type::BigIntLiteralType(t) => t.get_immediateBaseConstraint(),
            Type::EnumType(t) => t.get_immediateBaseConstraint(),
            Type::ObjectType(t) => t.get_immediateBaseConstraint(),
            Type::InterfaceType(t) => t.get_immediateBaseConstraint(),
            Type::InterfaceTypeWithDeclaredMembers(t) => t.get_immediateBaseConstraint(),
            Type::TypeReference(t) => t.get_immediateBaseConstraint(),
            Type::DeferredTypeReference(t) => t.get_immediateBaseConstraint(),
            Type::GenericType(t) => t.get_immediateBaseConstraint(),
            Type::TupleType(t) => t.get_immediateBaseConstraint(),
            Type::TupleTypeReference(t) => t.get_immediateBaseConstraint(),
            Type::UnionType(t) => t.get_immediateBaseConstraint(),
            Type::IntersectionType(t) => t.get_immediateBaseConstraint(),
            Type::AnonymousType(t) => t.get_immediateBaseConstraint(),
            Type::MappedType(t) => t.get_immediateBaseConstraint(),
            Type::EvolvingArrayType(t) => t.get_immediateBaseConstraint(),
            Type::ReverseMappedType(t) => t.get_immediateBaseConstraint(),
            Type::ResolvedType(t) => t.get_immediateBaseConstraint(),
            Type::FreshObjectLiteralType(_) => todo!(),
            Type::IterableOrIteratorType(t) => t.get_immediateBaseConstraint(),
            Type::PromiseOrAwaitableType(t) => t.get_immediateBaseConstraint(),
            Type::SyntheticDefaultModuleType(t) => t.get_immediateBaseConstraint(),
            Type::TypeParameter(t) => t.get_immediateBaseConstraint(),
            Type::IndexedAccessType(t) => t.get_immediateBaseConstraint(),
            Type::IndexType(t) => t.get_immediateBaseConstraint(),
            Type::ConditionalType(t) => t.get_immediateBaseConstraint(),
            Type::TemplateLiteralType(t) => t.get_immediateBaseConstraint(),
            Type::StringMappingType(t) => t.get_immediateBaseConstraint(),
            Type::SubstitutionType(t) => t.get_immediateBaseConstraint(),
            Type::Dummy => unreachable!(),
        }
    }
    fn get_immediateBaseConstraint_mut(&mut self) -> &mut Option<TypeId> {
        match self {
            Type::IntrinsicType(t) => t.get_immediateBaseConstraint_mut(),
            Type::NullableType(t) => t.get_immediateBaseConstraint_mut(),
            Type::FreshableIntrinsicType(t) => t.get_immediateBaseConstraint_mut(),
            Type::UniqueESSymbolType(t) => t.get_immediateBaseConstraint_mut(),
            Type::StringLiteralType(t) => t.get_immediateBaseConstraint_mut(),
            Type::NumberLiteralType(t) => t.get_immediateBaseConstraint_mut(),
            Type::BigIntLiteralType(t) => t.get_immediateBaseConstraint_mut(),
            Type::EnumType(t) => t.get_immediateBaseConstraint_mut(),
            Type::ObjectType(t) => t.get_immediateBaseConstraint_mut(),
            Type::InterfaceType(t) => t.get_immediateBaseConstraint_mut(),
            Type::InterfaceTypeWithDeclaredMembers(t) => t.get_immediateBaseConstraint_mut(),
            Type::TypeReference(t) => t.get_immediateBaseConstraint_mut(),
            Type::DeferredTypeReference(t) => t.get_immediateBaseConstraint_mut(),
            Type::GenericType(t) => t.get_immediateBaseConstraint_mut(),
            Type::TupleType(t) => t.get_immediateBaseConstraint_mut(),
            Type::TupleTypeReference(t) => t.get_immediateBaseConstraint_mut(),
            Type::UnionType(t) => t.get_immediateBaseConstraint_mut(),
            Type::IntersectionType(t) => t.get_immediateBaseConstraint_mut(),
            Type::AnonymousType(t) => t.get_immediateBaseConstraint_mut(),
            Type::MappedType(t) => t.get_immediateBaseConstraint_mut(),
            Type::EvolvingArrayType(t) => t.get_immediateBaseConstraint_mut(),
            Type::ReverseMappedType(t) => t.get_immediateBaseConstraint_mut(),
            Type::ResolvedType(t) => t.get_immediateBaseConstraint_mut(),
            Type::FreshObjectLiteralType(_) => todo!(),
            Type::IterableOrIteratorType(t) => t.get_immediateBaseConstraint_mut(),
            Type::PromiseOrAwaitableType(t) => t.get_immediateBaseConstraint_mut(),
            Type::SyntheticDefaultModuleType(t) => t.get_immediateBaseConstraint_mut(),
            Type::TypeParameter(t) => t.get_immediateBaseConstraint_mut(),
            Type::IndexedAccessType(t) => t.get_immediateBaseConstraint_mut(),
            Type::IndexType(t) => t.get_immediateBaseConstraint_mut(),
            Type::ConditionalType(t) => t.get_immediateBaseConstraint_mut(),
            Type::TemplateLiteralType(t) => t.get_immediateBaseConstraint_mut(),
            Type::StringMappingType(t) => t.get_immediateBaseConstraint_mut(),
            Type::SubstitutionType(t) => t.get_immediateBaseConstraint_mut(),
            Type::Dummy => unreachable!(),
        }
    }

    fn get_widened(&self) -> &Option<TypeId> {
        match self {
            Type::IntrinsicType(t) => t.get_widened(),
            Type::NullableType(t) => t.get_widened(),
            Type::FreshableIntrinsicType(t) => t.get_widened(),
            Type::UniqueESSymbolType(t) => t.get_widened(),
            Type::StringLiteralType(t) => t.get_widened(),
            Type::NumberLiteralType(t) => t.get_widened(),
            Type::BigIntLiteralType(t) => t.get_widened(),
            Type::EnumType(t) => t.get_widened(),
            Type::ObjectType(t) => t.get_widened(),
            Type::InterfaceType(t) => t.get_widened(),
            Type::InterfaceTypeWithDeclaredMembers(t) => t.get_widened(),
            Type::TypeReference(t) => t.get_widened(),
            Type::DeferredTypeReference(t) => t.get_widened(),
            Type::GenericType(t) => t.get_widened(),
            Type::TupleType(t) => t.get_widened(),
            Type::TupleTypeReference(t) => t.get_widened(),
            Type::UnionType(t) => t.get_widened(),
            Type::IntersectionType(t) => t.get_widened(),
            Type::AnonymousType(t) => t.get_widened(),
            Type::MappedType(t) => t.get_widened(),
            Type::EvolvingArrayType(t) => t.get_widened(),
            Type::ReverseMappedType(t) => t.get_widened(),
            Type::ResolvedType(t) => t.get_widened(),
            Type::FreshObjectLiteralType(_) => todo!(),
            Type::IterableOrIteratorType(t) => t.get_widened(),
            Type::PromiseOrAwaitableType(t) => t.get_widened(),
            Type::SyntheticDefaultModuleType(t) => t.get_widened(),
            Type::TypeParameter(t) => t.get_widened(),
            Type::IndexedAccessType(t) => t.get_widened(),
            Type::IndexType(t) => t.get_widened(),
            Type::ConditionalType(t) => t.get_widened(),
            Type::TemplateLiteralType(t) => t.get_widened(),
            Type::StringMappingType(t) => t.get_widened(),
            Type::SubstitutionType(t) => t.get_widened(),
            Type::Dummy => unreachable!(),
        }
    }
    fn get_widened_mut(&mut self) -> &mut Option<TypeId> {
        match self {
            Type::IntrinsicType(t) => t.get_widened_mut(),
            Type::NullableType(t) => t.get_widened_mut(),
            Type::FreshableIntrinsicType(t) => t.get_widened_mut(),
            Type::UniqueESSymbolType(t) => t.get_widened_mut(),
            Type::StringLiteralType(t) => t.get_widened_mut(),
            Type::NumberLiteralType(t) => t.get_widened_mut(),
            Type::BigIntLiteralType(t) => t.get_widened_mut(),
            Type::EnumType(t) => t.get_widened_mut(),
            Type::ObjectType(t) => t.get_widened_mut(),
            Type::InterfaceType(t) => t.get_widened_mut(),
            Type::InterfaceTypeWithDeclaredMembers(t) => t.get_widened_mut(),
            Type::TypeReference(t) => t.get_widened_mut(),
            Type::DeferredTypeReference(t) => t.get_widened_mut(),
            Type::GenericType(t) => t.get_widened_mut(),
            Type::TupleType(t) => t.get_widened_mut(),
            Type::TupleTypeReference(t) => t.get_widened_mut(),
            Type::UnionType(t) => t.get_widened_mut(),
            Type::IntersectionType(t) => t.get_widened_mut(),
            Type::AnonymousType(t) => t.get_widened_mut(),
            Type::MappedType(t) => t.get_widened_mut(),
            Type::EvolvingArrayType(t) => t.get_widened_mut(),
            Type::ReverseMappedType(t) => t.get_widened_mut(),
            Type::ResolvedType(t) => t.get_widened_mut(),
            Type::FreshObjectLiteralType(_) => todo!(),
            Type::IterableOrIteratorType(t) => t.get_widened_mut(),
            Type::PromiseOrAwaitableType(t) => t.get_widened_mut(),
            Type::SyntheticDefaultModuleType(t) => t.get_widened_mut(),
            Type::TypeParameter(t) => t.get_widened_mut(),
            Type::IndexedAccessType(t) => t.get_widened_mut(),
            Type::IndexType(t) => t.get_widened_mut(),
            Type::ConditionalType(t) => t.get_widened_mut(),
            Type::TemplateLiteralType(t) => t.get_widened_mut(),
            Type::StringMappingType(t) => t.get_widened_mut(),
            Type::SubstitutionType(t) => t.get_widened_mut(),
            Type::Dummy => unreachable!(),
        }
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

#[derive(Debug)]
pub struct NullableType {
    pub intrinsic_type_base: IntrinsicTypeBase,
    pub type_base: TypeBase,
}

#[derive(Debug)]
pub struct FreshableIntrinsicType {
    pub intrinsic_type_base: IntrinsicTypeBase,
    pub type_base: TypeBase,

    pub freshType: TypeId,   // Fresh version of type
    pub regularType: TypeId, // Regular version of type
}

// export type FreshableType = LiteralType | FreshableIntrinsicType;

// String literal types (TypeFlags.StringLiteral)
// Numeric literal types (TypeFlags.NumberLiteral)
// BigInt literal types (TypeFlags.BigIntLiteral)
#[derive(Debug)]
pub struct LiteralType {
    pub freshType: Option<TypeId>, // Fresh version of type
    pub regularType: TypeId,       // Regular version of type
}

// Unique symbol types (TypeFlags.UniqueESSymbol)
#[derive(Debug)]
pub struct UniqueESSymbolType {
    pub type_base: TypeBase,

    pub symbol: Symbol,
    pub escapedName: JsWord,
}

#[derive(Debug)]
pub struct StringLiteralType {
    pub literal_type: LiteralType,
    pub type_base: TypeBase,

    pub value: JsWord,
}

impl StringLiteralType {
    pub fn new(value: JsWord, regularType: TypeId) -> Self {
        Self {
            literal_type: LiteralType {
                freshType: None,
                regularType,
            },
            type_base: TypeBase::new(TypeFlags::StringLiteral, None),
            value,
        }
    }
}

#[derive(Debug)]
pub struct NumberLiteralType {
    pub literal_type: LiteralType,
    pub type_base: TypeBase,

    pub value: f64,
}

impl NumberLiteralType {
    pub fn new(value: f64, regularType: TypeId) -> Self {
        Self {
            literal_type: LiteralType {
                freshType: None,
                regularType,
            },
            type_base: TypeBase::new(TypeFlags::NumberLiteral, None),
            value,
        }
    }
}

#[derive(Debug)]
pub struct BigIntLiteralType {
    pub literal_type: LiteralType,
    pub type_base: TypeBase,

    pub value: BigIntValue,
}

// Enum types (TypeFlags.Enum)
#[derive(Debug)]
pub struct EnumType {
    pub type_base: TypeBase,
}

// export type ObjectFlagsType = NullableType | ObjectType | UnionType | IntersectionType;

// Object types (TypeFlags.ObjectType)
#[derive(Debug, Default)]
pub struct ObjectTypeBase {
    pub objectFlags: ObjectFlags,
    pub members: Option<SymbolTableId>,       // Properties by name
    pub properties: Rc<Vec<SymbolId>>,        // Properties
    pub callSignatures: Rc<Vec<SignatureId>>, // Call signatures of type
    pub constructSignatures: Rc<Vec<SignatureId>>, // Construct signatures of type
    pub indexInfos: Rc<Vec<IndexInfoId>>,     // Index signatures
    pub objectTypeWithoutAbstractConstructSignatures: Option<TypeId>,
}

impl ObjectTypeBase {
    pub fn new(objectFlags: ObjectFlags) -> Self {
        Self {
            objectFlags,
            members: None,
            properties: Default::default(),
            callSignatures: Default::default(),
            constructSignatures: Default::default(),
            indexInfos: Default::default(),
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

/// Class and interface types (ObjectFlags::Class and ObjectFlags::Interface).
#[derive(Default, Debug)]
pub struct InterfaceTypeBase {
    pub typeParameters: Option<Rc<Vec<TypeId>>>, // Type parameters (undefined if non-generic)
    pub outerTypeParameters: Option<Rc<Vec<TypeId>>>, // Outer type parameters (undefined if none)
    pub localTypeParameters: Option<Rc<Vec<TypeId>>>, // Local type parameters (undefined if none)
    pub thisType: Option<TypeId>,                // The "this" type (undefined if none)
    pub resolvedBaseConstructorType: Option<TypeId>, // Resolved base constructor type of class
    pub resolvedBaseTypes: Option<Rc<Vec<TypeId>>>, // Resolved base types
    pub baseTypesResolved: bool,

    // TODO: this field may not be used by all types that posses a InterfaceTypeBase.
    // If this is true, maybe lower it down into only those that use it.
    pub declared_members: Option<DeclaredMembers>,
}

/// Class and interface types (ObjectFlags::Class and ObjectFlags::Interface).
#[derive(Debug)]
pub struct InterfaceType {
    pub interface_type: InterfaceTypeBase,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,
}

// // Object type or intersection of object types
// export type BaseType = ObjectType | IntersectionType | TypeVariable; // Also `any` and `object`
// #[derive(Debug)]
// pub enum BaseType {
//     ObjectType(TypeId),
//     IntersectionType(TypeId),
//     TypeVariable(TypeId),
//     // Also `any` and `object`
// }

#[derive(Debug)]
pub struct DeclaredMembers {
    pub declaredProperties: Rc<Vec<SymbolId>>, // Declared members
    pub declaredCallSignatures: Rc<Vec<SignatureId>>, // Declared call signatures
    pub declaredConstructSignatures: Rc<Vec<SignatureId>>, // Declared construct signatures
    pub declaredIndexInfos: Rc<Vec<IndexInfoId>>, // Declared index signatures
}

#[derive(Debug)]
pub struct InterfaceTypeWithDeclaredMembers {
    pub interface_type: InterfaceTypeBase,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    pub declaredProperties: Vec<SymbolId>, // Declared members
    pub declaredCallSignatures: Vec<SignatureId>, // Declared call signatures
    pub declaredConstructSignatures: Vec<SignatureId>, // Declared construct signatures
    pub declaredIndexInfos: Vec<IndexInfoId>, // Declared index signatures
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
pub struct TypeReferenceBase {
    pub target: Option<TypeId>, // Type reference target
    // TODO:
    // node: Option<TypeReferenceNode | ArrayTypeNode | TupleTypeNode>,
    pub node: Option<BoundNode>,
    pub mapper: Option<Rc<TypeMapper>>,
    pub resolvedTypeArguments: Option<Rc<Vec<TypeId>>>, // Resolved type reference type arguments
    pub literalType: Option<TypeId>, // Clone of type with ObjectFlags.ArrayLiteral set
    pub cachedEquivalentBaseType: Option<TypeId>, // Only set on references to class or interfaces with a single base type and no augmentations
}

#[derive(Default, Debug)]
pub struct TypeReference {
    pub type_reference: TypeReferenceBase,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,
}

#[derive(Debug)]
pub struct DeferredTypeReference {
    pub type_reference: TypeReferenceBase,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    // TODO:
    // node: TypeReferenceNode | ArrayTypeNode | TupleTypeNode,
    // pub node: BoundNode,
    pub instantiations: AHashMap<TypeList, TypeId>, // Instantiations of generic type alias (undefined if non-generic)
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
    pub instantiations: AHashMap<TypeList, TypeId>, // Generic instantiation cache
    // TODO: possible unsed (even by TSC)
    pub variances: Rc<Vec<VarianceFlags>>, // Variance of each type parameter
}

#[derive(Debug)]
pub struct GenericType {
    pub interface_type: InterfaceTypeBase,
    pub type_reference: TypeReferenceBase,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    pub generic_type_base: GenericTypeBase,
}

impl GenericType {
    pub fn new(
        interface_type: InterfaceTypeBase,
        type_reference: TypeReferenceBase,
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
    #[derive(Default)]
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
    pub generic_type_base: GenericTypeBase,
    pub interface_type: InterfaceTypeBase,
    pub type_reference: TypeReferenceBase,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    pub elementFlags: Rc<Vec<ElementFlags>>,
    pub minLength: usize,     // Number of required or variadic elements
    pub fixedLength: usize,   // Number of initial required or optional elements
    pub hasRestElement: bool, // True if tuple has any rest or variadic elements
    pub combinedFlags: ElementFlags,
    pub readonly: bool,
    // TODO:
    // labeledElementDeclarations: Vec<(NamedTupleMember | ParameterDeclaration)>,
    pub labeledElementDeclarations: Rc<Vec<BoundNode>>,
}

#[derive(Debug)]
pub struct TupleTypeReference {
    pub type_reference: TypeReferenceBase,
    pub type_base: TypeBase,
    pub object_type_base: ObjectTypeBase,

    pub target: TypeId,
}

#[derive(Debug, Default)]
pub struct UnionOrIntersectionType {
    pub types: Rc<Vec<TypeId>>, // Constituent types
    pub objectFlags: ObjectFlags,
    pub propertyCache: Option<SymbolTableId>, // Cache of resolved properties
    pub propertyCacheWithoutObjectFunctionPropertyAugment: Option<SymbolTableId>, // Cache of resolved properties that does not augment function or object type properties
    pub resolvedProperties: Option<Rc<Vec<SymbolId>>>,
    pub resolvedIndexType: Option<TypeId>,
    pub resolvedStringIndexType: Option<TypeId>,
    pub resolvedBaseConstraint: Option<TypeId>,
}

impl UnionOrIntersectionType {
    pub fn new(objectFlags: ObjectFlags) -> Self {
        Self {
            types: Default::default(),
            objectFlags,
            propertyCache: None,
            propertyCacheWithoutObjectFunctionPropertyAugment: None,
            resolvedProperties: None,
            resolvedIndexType: None,
            resolvedStringIndexType: None,
            resolvedBaseConstraint: None,
        }
    }
}

#[derive(Debug, Default)]
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
    pub type_base: TypeBase,

    pub union_type_base: UnionTypeBase,
}

#[derive(Debug)]
pub struct IntersectionType {
    pub union_or_intersection_type: UnionOrIntersectionType,
    pub type_base: TypeBase,

    pub resolvedApparentType: Option<TypeId>,
}

// export type StructuredType = ObjectType | UnionType | IntersectionType;

// An instantiated anonymous type has a target and a mapper
#[derive(Debug, Default)]
pub struct AnonymousTypeBase {
    pub target: Option<TypeId>,                     // Instantiation target
    pub mapper: Option<Rc<TypeMapper>>,             // Instantiation mapper
    pub instantiations: AHashMap<TypeList, TypeId>, // Instantiations of generic type alias (undefined if non-generic)
}

#[derive(Debug, Default)]
pub struct AnonymousType {
    pub anonymous_type: AnonymousTypeBase,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,
}

#[derive(Debug)]
pub struct MappedType {
    pub anonymous_type: AnonymousTypeBase,
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    pub declaration: Rc<crate::node::TsMappedType>,
    pub typeParameter: Option<TypeId>,
    pub constraintType: Option<TypeId>,
    pub nameType: Option<TypeId>,
    pub templateType: Option<TypeId>,
    pub modifiersType: Option<TypeId>,
    pub resolvedApparentType: Option<TypeId>,
    pub containsError: bool,
}

#[derive(Debug)]
pub struct EvolvingArrayType {
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    pub elementType: TypeId, // Element expressions of evolving array type
    pub finalArrayType: Option<TypeId>, // Final array type of evolving array type
}

#[derive(Debug)]
pub struct ReverseMappedType {
    pub object_type_base: ObjectTypeBase,
    pub type_base: TypeBase,

    pub source: TypeId,
    pub mappedType: MappedType,
    pub constraintType: IndexType,
}

// TODO: rename:
#[derive(Debug)]
pub enum Old {
    InterfaceType {
        interface_type: InterfaceTypeBase,
    },
    // InterfaceTypeWithDeclaredMembers
    TypeReference {
        type_reference: TypeReferenceBase,
    },
    // DeferredTypeReference
    GenericType {
        interface_type: InterfaceTypeBase,
        type_reference: TypeReferenceBase,
        generic_type_base: GenericTypeBase,
    },
    // TupleType
    // TupleTypeReference
    AnonymousType {
        anonymous_type: AnonymousTypeBase,
    }, // MappedType
    // EvolvingArrayType
    // ReverseMappedType
    UnionType {
        union_type_base: UnionTypeBase,
    }, // IterableOrIteratorType
       // PromiseOrAwaitableType
}

// Resolved object, union, or intersection type
#[derive(Debug)]
pub struct ResolvedType {
    pub type_base: TypeBase,
    pub object_type_base: ObjectTypeBase,
    pub union_or_intersection_type: UnionOrIntersectionType,

    // TODO: rename:
    pub old: Option<Old>,
}

impl ResolvedType {
    pub fn new(
        symbol: Option<SymbolId>,
        object_flags: ObjectFlags,
        members: SymbolTableId,
    ) -> Self {
        let mut object_type_base = ObjectTypeBase::new(object_flags);
        object_type_base.members = Some(members);
        Self {
            type_base: TypeBase::new(TypeFlags::Object, symbol),
            object_type_base,
            union_or_intersection_type: UnionOrIntersectionType::new(object_flags),
            old: None,
        }
    }
    // temp
    pub fn convert(ty: &mut Type) {
        if matches!(ty, Type::ResolvedType(_)) {
            return;
        }
        let t = std::mem::replace(ty, Type::Dummy);
        match t {
            Type::IntrinsicType(_) => todo!(),
            Type::NullableType(_) => todo!(),
            Type::FreshableIntrinsicType(_) => todo!(),
            Type::UniqueESSymbolType(_) => todo!(),
            Type::StringLiteralType(_) => todo!(),
            Type::NumberLiteralType(_) => todo!(),
            Type::BigIntLiteralType(_) => todo!(),
            Type::EnumType(_) => todo!(),
            Type::ObjectType(t) => {
                *ty = Type::ResolvedType(Self {
                    type_base: t.type_base,
                    union_or_intersection_type: UnionOrIntersectionType::new(
                        t.object_type_base.objectFlags,
                    ),
                    object_type_base: t.object_type_base,
                    old: None,
                })
            }
            Type::InterfaceType(t) => {
                *ty = Type::ResolvedType(Self {
                    type_base: t.type_base,
                    union_or_intersection_type: UnionOrIntersectionType::new(
                        t.object_type_base.objectFlags,
                    ),
                    object_type_base: t.object_type_base,
                    old: Some(Old::InterfaceType {
                        interface_type: t.interface_type,
                    }),
                })
            }
            Type::InterfaceTypeWithDeclaredMembers(_) => todo!(),
            Type::TypeReference(t) => {
                *ty = Type::ResolvedType(Self {
                    type_base: t.type_base,
                    union_or_intersection_type: UnionOrIntersectionType::new(
                        t.object_type_base.objectFlags,
                    ),
                    object_type_base: t.object_type_base,
                    old: Some(Old::TypeReference {
                        type_reference: t.type_reference,
                    }),
                })
            }
            Type::DeferredTypeReference(_) => todo!(),
            Type::GenericType(t) => {
                *ty = Type::ResolvedType(Self {
                    type_base: t.type_base,
                    union_or_intersection_type: UnionOrIntersectionType::new(
                        t.object_type_base.objectFlags,
                    ),
                    object_type_base: t.object_type_base,
                    old: Some(Old::GenericType {
                        interface_type: t.interface_type,
                        type_reference: t.type_reference,
                        generic_type_base: t.generic_type_base,
                    }),
                });
            }
            Type::TupleType(_) => todo!(),
            Type::TupleTypeReference(_) => todo!(),
            Type::UnionType(t) => {
                *ty = Type::ResolvedType(Self {
                    type_base: t.type_base,
                    union_or_intersection_type: t.union_or_intersection_type,
                    object_type_base: ObjectTypeBase::default(),
                    old: Some(Old::UnionType {
                        union_type_base: t.union_type_base,
                    }),
                });
            }
            Type::IntersectionType(_) => todo!(),
            Type::AnonymousType(t) => {
                *ty = Type::ResolvedType(Self {
                    type_base: t.type_base,
                    union_or_intersection_type: UnionOrIntersectionType::new(
                        t.object_type_base.objectFlags,
                    ),
                    object_type_base: t.object_type_base,
                    old: Some(Old::AnonymousType {
                        anonymous_type: t.anonymous_type,
                    }),
                });
            }
            Type::MappedType(_) => todo!(),
            Type::EvolvingArrayType(_) => todo!(),
            Type::ReverseMappedType(_) => todo!(),
            Type::IterableOrIteratorType(_) => todo!(),
            Type::PromiseOrAwaitableType(_) => todo!(),
            Type::SyntheticDefaultModuleType(_) => todo!(),
            Type::TypeParameter(_) => todo!(),
            Type::IndexedAccessType(_) => todo!(),
            Type::IndexType(_) => todo!(),
            Type::ConditionalType(_) => todo!(),
            Type::TemplateLiteralType(_) => todo!(),
            Type::StringMappingType(_) => todo!(),
            Type::SubstitutionType(_) => todo!(),
            Type::ResolvedType(_) | Type::FreshObjectLiteralType(_) | Type::Dummy => unreachable!(),
        }
    }
}

// Object literals are initially marked fresh. Freshness disappears following an assignment,
// before a type assertion, or when an object literal's type is widened. The regular
// version of a fresh type is identical except for the TypeFlags.FreshObjectLiteral flag.
#[derive(Debug)]
pub struct FreshObjectLiteralType {
    pub resolved_type: ResolvedType,

    pub regularType: TypeId, // Regular version of fresh type
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
    pub union_type_base: UnionTypeBase,
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

#[derive(Debug)]
pub struct PromiseOrAwaitableType {
    pub object_type_base: ObjectTypeBase,
    pub union_type_base: UnionTypeBase,
    pub union_or_intersection_type: UnionOrIntersectionType,
    pub type_base: TypeBase,

    pub promiseTypeOfPromiseConstructor: Option<TypeId>,
    pub promisedTypeOfPromise: Option<TypeId>,
    pub awaitedTypeOfType: Option<TypeId>,
}

#[derive(Debug)]
pub struct SyntheticDefaultModuleType {
    pub type_base: TypeBase,

    pub syntheticType: Option<TypeId>,
}

#[derive(Debug, Default)]
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
    pub target: Option<TypeId>,         // Instantiation target
    pub mapper: Option<Rc<TypeMapper>>, // Instantiation mapper
    pub isThisType: bool,
    pub resolvedDefaultType: Option<TypeId>,
}

bitflags! {
    #[derive(Default)]
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

#[derive(Debug)]
pub struct ConditionalRoot {
    // node: ConditionalTypeNode,
    pub node: Rc<crate::node::TsConditionalType>,
    pub checkType: TypeId,
    pub extendsType: TypeId,
    pub isDistributive: bool,
    pub inferTypeParameters: Option<Rc<Vec<TypeId>>>,
    pub outerTypeParameters: Option<Rc<Vec<TypeId>>>,
    // TODO: key type:
    pub instantiations: AHashMap<u64, TypeId>,
    pub aliasSymbol: Option<SymbolId>,
    pub aliasTypeArguments: Option<Rc<Vec<TypeId>>>,
}

index::newtype_index! {
    pub struct ConditionalRootId {
        DEBUG_FORMAT = "ConditionalRootId({})"
    }
}

// T extends U ? X : Y (TypeFlags.Conditional)
#[derive(Debug)]
pub struct ConditionalType {
    pub instantiable_type: InstantiableType,
    pub type_base: TypeBase,

    pub root: ConditionalRootId,
    pub checkType: TypeId,
    pub extendsType: TypeId,
    pub resolvedTrueType: Option<TypeId>,
    pub resolvedFalseType: Option<TypeId>,
    pub resolvedInferredTrueType: Option<TypeId>, // The `trueType` instantiated with the `combinedMapper`, if present
    pub resolvedDefaultConstraint: Option<TypeId>,
    pub mapper: Option<Rc<TypeMapper>>,
    pub combinedMapper: Option<Rc<TypeMapper>>,
}

#[derive(Debug)]
pub struct TemplateLiteralType {
    pub instantiable_type: InstantiableType,
    pub type_base: TypeBase,

    pub texts: Rc<Vec<JsWord>>, // Always one element longer than types
    pub types: Rc<Vec<TypeId>>, // Always at least one element
}

#[derive(Debug)]
pub struct StringMappingType {
    pub instantiable_type: InstantiableType,
    pub type_base: TypeBase,

    pub ty: TypeId,
    pub symbol: SymbolId,
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
