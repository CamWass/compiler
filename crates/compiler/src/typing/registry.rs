use crate::typing::{all_type::AllType, big_int_type::BigIntType, boolean_type::BooleanType, null_type::NullType, number_type::NumberType, string_type::StringType, symbol_type::SymbolType, types::TyKind, unknown_type::UnknownType, void_type::VoidType};

use super::types::{CommonTypes, Ty, TypeId};
use ast::AstNode;
use fxhash::FxHashMap;
use index::vec::IndexVec;

pub struct TypeRegistry<'tcx> {
    types: IndexVec<TypeId, Ty<'tcx>>,
    type_map: FxHashMap<AstNode<'tcx>, TypeId>,
    pub common_types: CommonTypes,
}

impl<'tcx> TypeRegistry<'tcx> {
    pub fn new() -> Self {
        // Number of common types.
        let capacity = 78;

        debug_assert!(
            std::mem::size_of::<CommonTypes>() == capacity * std::mem::size_of::<TypeId>()
        );

        let type_map = FxHashMap::with_capacity_and_hasher(capacity, Default::default());
        let mut types = IndexVec::with_capacity(capacity);

        let mut mk = |kind: TyKind| types.push(Ty::new(kind));
        let common_types = CommonTypes {
            BOOLEAN_TYPE: mk(TyKind::BOOLEAN(BooleanType{})),
            NULL_TYPE: mk(TyKind::NULL(NullType{})),
            BIGINT_TYPE: mk(TyKind::BIGINT(BigIntType{})),
            NUMBER_TYPE: mk(TyKind::NUMBER(NumberType{})),
            STRING_TYPE: mk(TyKind::STRING(StringType{})),
            SYMBOL_TYPE: mk(TyKind::SYMBOL(SymbolType{})),

            UNKNOWN_TYPE: mk(TyKind::UNKNOWN(UnknownType::new(false))),
            CHECKED_UNKNOWN_TYPE: mk(TyKind::UNKNOWN(UnknownType::new(true))),

            VOID_TYPE: mk(TyKind::VOID(VoidType{})),
            ALL_TYPE: mk(TyKind::ALL(AllType{})),

            // TODO: Placeholder TypeIds:
            ARGUMENTS_TYPE: TypeId::MAX,
            ARGUMENTS_FUNCTION_TYPE: TypeId::MAX,
            ARRAY_TYPE: TypeId::MAX,
            ARRAY_FUNCTION_TYPE: TypeId::MAX,
            ASYNC_ITERABLE_FUNCTION_TYPE: TypeId::MAX,
            ASYNC_ITERABLE_TYPE: TypeId::MAX,
            ASYNC_ITERATOR_FUNCTION_TYPE: TypeId::MAX,
            ASYNC_ITERATOR_TYPE: TypeId::MAX,
            ASYNC_ITERATOR_ITERABLE_FUNCTION_TYPE: TypeId::MAX,
            ASYNC_ITERATOR_ITERABLE_TYPE: TypeId::MAX,
            ASYNC_GENERATOR_FUNCTION_TYPE: TypeId::MAX,
            ASYNC_GENERATOR_TYPE: TypeId::MAX,
            BIGINT_OBJECT_TYPE: TypeId::MAX,
            BIGINT_OBJECT_FUNCTION_TYPE: TypeId::MAX,
            BOOLEAN_OBJECT_TYPE: TypeId::MAX,
            BOOLEAN_OBJECT_FUNCTION_TYPE: TypeId::MAX,
            DATE_TYPE: TypeId::MAX,
            DATE_FUNCTION_TYPE: TypeId::MAX,
            FUNCTION_TYPE: TypeId::MAX,
            FUNCTION_FUNCTION_TYPE: TypeId::MAX,
            FUNCTION_PROTOTYPE: TypeId::MAX,
            FUNCTION_INSTANCE_PROTOTYPE: TypeId::MAX,
            GENERATOR_FUNCTION_TYPE: TypeId::MAX,
            GENERATOR_TYPE: TypeId::MAX,
            I_ITERABLE_RESULT_FUNCTION_TYPE: TypeId::MAX,
            I_ITERABLE_RESULT_TYPE: TypeId::MAX,
            ITERABLE_FUNCTION_TYPE: TypeId::MAX,
            ITERABLE_TYPE: TypeId::MAX,
            ITERATOR_FUNCTION_TYPE: TypeId::MAX,
            ITERATOR_TYPE: TypeId::MAX,
            I_ARRAY_LIKE_FUNCTION_TYPE: TypeId::MAX,
            I_ARRAY_LIKE_TYPE: TypeId::MAX,
            I_TEMPLATE_ARRAY_TYPE: TypeId::MAX,
            I_OBJECT_FUNCTION_TYPE: TypeId::MAX,
            I_OBJECT_TYPE: TypeId::MAX,
            I_THENABLE_FUNCTION_TYPE: TypeId::MAX,
            I_THENABLE_TYPE: TypeId::MAX,
            NUMBER_OBJECT_TYPE: TypeId::MAX,
            NUMBER_OBJECT_FUNCTION_TYPE: TypeId::MAX,
            PROMISE_TYPE: TypeId::MAX,
            PROMISE_FUNCTION_TYPE: TypeId::MAX,
            OBJECT_TYPE: TypeId::MAX,
            OBJECT_FUNCTION_TYPE: TypeId::MAX,
            OBJECT_PROTOTYPE: TypeId::MAX,
            REGEXP_TYPE: TypeId::MAX,
            REGEXP_FUNCTION_TYPE: TypeId::MAX,
            STRING_OBJECT_TYPE: TypeId::MAX,
            STRING_OBJECT_FUNCTION_TYPE: TypeId::MAX,
            SYMBOL_OBJECT_TYPE: TypeId::MAX,
            SYMBOL_OBJECT_FUNCTION_TYPE: TypeId::MAX,
            THENABLE_TYPE: TypeId::MAX,
            NO_TYPE: TypeId::MAX,
            NO_OBJECT_TYPE: TypeId::MAX,
            NO_RESOLVED_TYPE: TypeId::MAX,
            GLOBAL_THIS: TypeId::MAX,
            LEAST_FUNCTION_TYPE: TypeId::MAX,
            GREATEST_FUNCTION_TYPE: TypeId::MAX,
            NULL_VOID: TypeId::MAX,
            NUMBER_STRING_BOOLEAN: TypeId::MAX,
            VALUE_TYPES: TypeId::MAX,
            NUMBER_SYMBOL: TypeId::MAX,
            STRING_SYMBOL: TypeId::MAX,
            NUMBER_STRING: TypeId::MAX,
            NUMBER_STRING_SYMBOL: TypeId::MAX,
            BIGINT_NUMBER: TypeId::MAX,
            BIGINT_NUMBER_OBJECT: TypeId::MAX,
            BIGINT_NUMBER_STRING: TypeId::MAX,
            BIGINT_NUMBER_STRING_OBJECT: TypeId::MAX,
        };
        Self {
            types,
            type_map,
            common_types,
        }
    }
    pub fn get_type(&self, node: AstNode) -> Option<TypeId> {
        self.type_map.get(&node).map(|t| *t)
    }

    pub fn set_type(&mut self, node: AstNode<'tcx>, ty: TypeId) -> Option<TypeId> {
        self.type_map.insert(node, ty)
    }
}
