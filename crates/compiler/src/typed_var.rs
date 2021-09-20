use ast::AstNode;
use std::hash::Hash;
use swc_atoms::JsWord;
use crate::{typed_scope::ScopeId, typing::types::Ty};

index::newtype_index! {
    pub struct TypedVarId {
        DEBUG_FORMAT = "TypedVarId({})"
    }
}

/**
 * {@link AbstractVar} subclass for use with {@link TypedScope}.
 *
 * <p>Note that this class inherits its {@link #equals} and {@link #hashCode} implementations from
 * {@link ScopedName}, which does not include any type information. This is necessary because {@code
 * Var}-keyed maps are used across multiple top scopes, but it comes with the caveat that if {@code
 * TypedVar} instances are stored in a set, the type information is at risk of disappearing if an
 * untyped (or differently typed) var is added for the same symbol.
 */
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct TypedVar<'tcx> {
    /**
     * The index at which the var is declared. e.g. if it's 0, it's the first declared variable in
     * that scope
     */
    index: usize,

    pub scope: ScopeId,

    pub ty: Option<Ty<'tcx>>,
    // The next two fields and the associated methods are only used by
    // TypeInference.java. Maybe there is a way to avoid having them in all typed variable instances.
    pub markedEscaped: bool,
    pub markedAssignedExactlyOnce: bool,

    /**
     * Whether the variable's type has been inferred or is declared. An inferred
     * type may change over time (as more code is discovered), whereas a
     * declared type is a static contract that must be matched.
     */
    pub typeInferred: bool,
    name: JsWord,
}

impl<'tcx> TypedVar<'tcx> {
    pub fn new(
        inferred: bool,
        name: &JsWord,
        nameNode: AstNode,
        ty: Option<Ty<'tcx>>,
        scope: ScopeId,
        index: usize,
    ) -> Self {
        //   super(name, nameNode, scope, index, input, /* implicitGoogNamespaceDefinition= */ null);
        //   if nameNode != null {
        //     checkArgument(
        //         NAME_NODE_TYPES.contains(nameNode.getToken()),
        //         "Invalid name node token %s",
        //         nameNode.getToken());
        //   }
        Self {
            index,
            scope,
            ty,
            markedEscaped: false,
            markedAssignedExactlyOnce: false,
            typeInferred: inferred,
            name: name.clone(),
        }
    }
}

// impl StaticSlot for TypedVar {
//     fn getName(&self) {
//         todo!()
//     }

//     fn getDeclaration(&self) {
//         todo!()
//     }

//     fn getJSDocInfo(&self) {
//         todo!()
//     }

//     fn getScope(&self) {
//         todo!()
//     }
// }
// impl StaticTypedSlot for TypedVar {
//     fn getType(&self) -> crate::typing::types::Ty {
//         todo!()
//     }

//     fn isTypeInferred(&self) -> bool {
//         todo!()
//     }
// }
