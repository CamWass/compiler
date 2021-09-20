use std::{hash::Hash, marker::PhantomData};

use super::{
    all_type::AllType, big_int_type::BigIntType, boolean_type::BooleanType, null_type::NullType,
    number_type::NumberType, string_type::StringType, symbol_type::SymbolType,
    unknown_type::UnknownType, void_type::VoidType,
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    ALL(AllType),
    ARROW,
    BOOLEAN(BooleanType),
    BIGINT(BigIntType),
    ENUM,
    ENUM_ELEMENT,
    FUNCTION,
    INSTANCE_OBJECT,
    NAMED,
    NO,
    NO_OBJECT,
    NO_RESOLVED,
    NULL(NullType),
    NUMBER(NumberType),
    PROTOTYPE_OBJECT,
    PROXY_OBJECT,
    RECORD,
    STRING(StringType),
    SYMBOL(SymbolType),
    TEMPLATE,
    TEMPLATIZED,
    UNION,
    UNKNOWN(UnknownType),
    VOID(VoidType),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ty<'tcx> {
    kind: TyKind,
    // TODO:
    _p: PhantomData<&'tcx str>,
}

impl<'tcx> Ty<'tcx> {
    pub fn new(kind: TyKind) -> Self {
        Self {
            kind,
            _p: Default::default(),
        }
    }
}

/// Constants corresponding to types that are built into a JavaScript engine
/// and other types that occur very often in the type system.
pub struct CommonTypes {
    pub ARGUMENTS_TYPE: TypeId,
    pub ARGUMENTS_FUNCTION_TYPE: TypeId,

    pub ARRAY_TYPE: TypeId,
    pub ARRAY_FUNCTION_TYPE: TypeId,

    pub ASYNC_ITERABLE_FUNCTION_TYPE: TypeId,
    pub ASYNC_ITERABLE_TYPE: TypeId,

    pub ASYNC_ITERATOR_FUNCTION_TYPE: TypeId,
    pub ASYNC_ITERATOR_TYPE: TypeId,

    pub ASYNC_ITERATOR_ITERABLE_FUNCTION_TYPE: TypeId,
    pub ASYNC_ITERATOR_ITERABLE_TYPE: TypeId,

    pub ASYNC_GENERATOR_FUNCTION_TYPE: TypeId,
    pub ASYNC_GENERATOR_TYPE: TypeId,

    pub BIGINT_TYPE: TypeId,
    pub BIGINT_OBJECT_TYPE: TypeId,
    pub BIGINT_OBJECT_FUNCTION_TYPE: TypeId,

    pub BOOLEAN_TYPE: TypeId,
    pub BOOLEAN_OBJECT_TYPE: TypeId,
    pub BOOLEAN_OBJECT_FUNCTION_TYPE: TypeId,

    /// A checked unknown type is a type that we know something about,
    /// but we're not really sure what we know about it.
    ///
    /// Examples of checked unknown types include:
    /// ```js
    /// if (x) { // x is unknown
    ///   alert(x); // x is checked unknown
    /// }
    /// ```
    ///
    /// ```js
    /// /* @param {SomeForwardDeclaredType} x */
    /// function f(x) {
    ///   // x is checked unknown. We know it's some type, but the type
    ///   // has not been included in this binary.
    /// }
    /// ```
    ///
    /// This is useful for missing property warnings, where we don't
    /// want to emit warnings on things that have been checked.
    pub CHECKED_UNKNOWN_TYPE: TypeId,

    pub DATE_TYPE: TypeId,
    pub DATE_FUNCTION_TYPE: TypeId,

    pub FUNCTION_TYPE: TypeId,
    pub FUNCTION_FUNCTION_TYPE: TypeId,
    pub FUNCTION_PROTOTYPE: TypeId,
    pub FUNCTION_INSTANCE_PROTOTYPE: TypeId,

    pub GENERATOR_FUNCTION_TYPE: TypeId,
    pub GENERATOR_TYPE: TypeId,

    pub I_ITERABLE_RESULT_FUNCTION_TYPE: TypeId,
    pub I_ITERABLE_RESULT_TYPE: TypeId,
    pub ITERABLE_FUNCTION_TYPE: TypeId,
    pub ITERABLE_TYPE: TypeId,
    pub ITERATOR_FUNCTION_TYPE: TypeId,
    pub ITERATOR_TYPE: TypeId,

    pub I_ARRAY_LIKE_FUNCTION_TYPE: TypeId,
    pub I_ARRAY_LIKE_TYPE: TypeId,

    pub I_TEMPLATE_ARRAY_TYPE: TypeId,

    pub I_OBJECT_FUNCTION_TYPE: TypeId,
    pub I_OBJECT_TYPE: TypeId,

    pub I_THENABLE_FUNCTION_TYPE: TypeId,
    pub I_THENABLE_TYPE: TypeId,

    pub NULL_TYPE: TypeId,

    pub NUMBER_TYPE: TypeId,
    pub NUMBER_OBJECT_TYPE: TypeId,
    pub NUMBER_OBJECT_FUNCTION_TYPE: TypeId,

    pub PROMISE_TYPE: TypeId,
    pub PROMISE_FUNCTION_TYPE: TypeId,

    pub OBJECT_TYPE: TypeId,
    // The Object constructor
    pub OBJECT_FUNCTION_TYPE: TypeId,
    pub OBJECT_PROTOTYPE: TypeId,

    pub REGEXP_TYPE: TypeId,
    pub REGEXP_FUNCTION_TYPE: TypeId,

    pub STRING_OBJECT_TYPE: TypeId,
    pub STRING_OBJECT_FUNCTION_TYPE: TypeId,
    pub STRING_TYPE: TypeId,

    pub SYMBOL_OBJECT_TYPE: TypeId,
    pub SYMBOL_OBJECT_FUNCTION_TYPE: TypeId,
    pub SYMBOL_TYPE: TypeId,

    pub THENABLE_TYPE: TypeId,

    pub UNKNOWN_TYPE: TypeId,

    pub VOID_TYPE: TypeId,

    // Commonly used types
    pub ALL_TYPE: TypeId,
    pub NO_TYPE: TypeId,
    pub NO_OBJECT_TYPE: TypeId,
    pub NO_RESOLVED_TYPE: TypeId,
    pub GLOBAL_THIS: TypeId,

    pub LEAST_FUNCTION_TYPE: TypeId,
    pub GREATEST_FUNCTION_TYPE: TypeId,

    /// (null, void)
    pub NULL_VOID: TypeId,

    /// (number,string,boolean)
    pub NUMBER_STRING_BOOLEAN: TypeId,

    /// (number,string,boolean,symbol)
    pub VALUE_TYPES: TypeId,

    /// (number,symbol)
    pub NUMBER_SYMBOL: TypeId,

    /// (string,symbol)
    pub STRING_SYMBOL: TypeId,

    /// (number,string)
    pub NUMBER_STRING: TypeId,

    /// (number,string,symbol)
    pub NUMBER_STRING_SYMBOL: TypeId,

    /// (bigint,number)
    pub BIGINT_NUMBER: TypeId,

    /// (BigInt,Number)
    pub BIGINT_NUMBER_OBJECT: TypeId,

    /// (bigint,number,string)
    pub BIGINT_NUMBER_STRING: TypeId,

    /// (Bigint,Number,String)
    pub BIGINT_NUMBER_STRING_OBJECT: TypeId,
}

index::newtype_index! {
    pub struct TypeId {
        DEBUG_FORMAT = "TypeId({})"
    }
}
