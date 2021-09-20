use crate::static_scope::StaticScope;
use crate::static_typed_slot::StaticTypedSlot;
use crate::typing::types::Ty;

/**
 * The {@code StaticTypedScope} interface must be implemented by any object that defines variables
 * for the purposes of static analysis. It is distinguished from the {@code Scriptable} class that
 * Rhino normally uses to represent a run-time scope.
 *
 */
pub trait StaticTypedScope<'tcx, S: StaticTypedSlot>: StaticScope<S> {
    /** Returns the scope enclosing this one or null if none. */
    fn getParentScope() -> Self;

    /**
     * Returns any defined slot within this scope for this name. This call continues searching through
     * parent scopes if a slot with this name is not found in the current scope.
     *
     * @param name The name of the variable slot to look up.
     * @return The defined slot for the variable, or {@code null} if no definition exists.
     */
    fn getSlot(name: String) -> S;

    /** Like {@code getSlot} but does not recurse into parent scopes. */
    fn getOwnSlot(name: String) -> S;

    /** Returns the expected type of {@code this} in the current scope. */
    fn getTypeOfThis() -> Ty<'tcx>;

    // /**
    //  * Looks up a given qualified name in the scope. if that fails, looks up the component properties
    //  * off of any owner types that are in scope.
    //  *
    //  * <p>This is always more or equally expensive as calling getSlot(String name), so should only be
    //  * used when necessary.
    //  *
    //  * <p>This only returns declared qualified names and known properties. It returns null given an
    //  * inferred name.
    //  */
    // fn lookupQualifiedName(qname: QualifiedName) -> Option<Ty> {
    //     todo!();
    //     // StaticTypedSlot slot = getSlot(qname.join());
    //     // if (slot != null && !slot.isTypeInferred()) {
    //     //   return slot.getType();
    //     // } else if (!qname.isSimple()) {
    //     //   JSType type = lookupQualifiedName(qname.getOwner());
    //     //   if (type != null && !type.isUnknownType()) {
    //     //     return type.findPropertyType(qname.getComponent());
    //     //   }
    //     // }
    //     // return null;
    // }
}
