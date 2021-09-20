use crate::typed_scope::{ScopeId, TypedScope};
use crate::typed_var::{TypedVar, TypedVarId};
use crate::typing::TypeRegistry;
use ast::AstNode;
use fxhash::FxHashMap;
use index::vec::IndexVec;

// /// This declares a list of types which can be allocated by `Arena`.
// ///
// /// The `few` modifier will cause allocation to use the shared arena and recording the destructor.
// /// This is faster and more memory efficient if there's only a few allocations of the type.
// /// Leaving `few` out will cause the type to get its own dedicated `TypedArena` which is
// /// faster and more memory efficient if there is lots of allocations.
// ///
// /// Specifying the `decode` modifier will add decode impls for `&T` and `&[T]` where `T` is the type
// /// listed. These impls will appear in the implement_ty_decoder! macro.
// #[macro_export]
// macro_rules! arena_types {
//     ($macro:path, $args:tt, $tcx:lifetime) => (
//         $macro!($args, [
//             [] typed_vars: crate::typed_var::TypedVar<$tcx>,
//             [] scopes: crate::typed_scope::TypedScope,
//         ], $tcx);
//     )
// }

// arena_types!(arena::declare_arena, [], 'tcx);

pub struct Ctx<'tcx> {
    // arena: Arena<'tcx>,
    pub scopes: IndexVec<ScopeId, TypedScope<'tcx>>,
    pub vars: IndexVec<TypedVarId, TypedVar<'tcx>>,
    pub type_registry: TypeRegistry<'tcx>
}

impl<'tcx> Ctx<'tcx> {
    pub fn new() -> Self {

        Self {
            // arena,
            scopes: Default::default(),
            vars: Default::default(),
            type_registry:TypeRegistry::new()
        }
    }


}
