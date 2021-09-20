use crate::lattice_element::LatticeElement;
use crate::typing::types::Ty;

// /**
//  * A symbol table for inferring types during data flow analysis.
//  *
//  * <p>Each flow scope represents the types of all variables in the scope at a particular point in
//  * the flow analysis.
//  */
// pub trait FlowScope: StaticTypedScope + LatticeElement {
//     /**
//      * Returns a flow scope with the given syntactic scope, which may be required to be a specific
//      * subclass, such as TypedScope.
//      */
//     fn withSyntacticScope(scope: StaticTypedScope) -> Self;

//     /**
//      * Returns a flow scope with the type of the given {@code symbol} updated to {@code type}.
//      *
//      * @throws IllegalArgumentException If no slot for this symbol exists.
//      */
//     fn inferSlotType(symbol: String, ty: Ty) -> Self;

//     /**
//      * Returns a flow scope with the type of the given {@code symbol} updated to {@code inferredType}.
//      * Updates are not performed in-place.
//      *
//      * <p>When traversing the control flow of a function, simple names are declared at the bottom of
//      * the flow lattice. But there are far too many qualified names to be able to do this and be
//      * performant. So the bottoms of qualified names are declared lazily.
//      *
//      * <p>Therefore, when inferring a qualified slot, we need both the "bottom" type of the slot when
//      * we enter the scope, and the current type being inferred.
//      */
//     fn inferQualifiedSlot(
//         node: Node,
//         symbol: String,
//         bottomType: Ty,
//         inferredType: Ty,
//         declare: boolean,
//     ) -> Self;

//     /** Returns the underlying TypedScope. */
//     fn getDeclarationScope() -> StaticTypedScope;
// }
