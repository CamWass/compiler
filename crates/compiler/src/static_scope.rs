use crate::static_slot::StaticSlot;

/**
 * The {@code StaticScope} interface must be implemented by any object that
 * defines variables for the purposes of static analysis.  It is distinguished
 * from the {@code Scriptable} class that Rhino normally uses to represent a
 * run-time scope.
 *
 */
pub trait StaticScope<S: StaticSlot> {
    /**
     * Returns the root node associated with this scope. May be null.
     */
    // fn getRootNode() -> Node;

    /** Returns the scope enclosing this one or null if none. */
    fn getParentScope() -> Self;

    /**
     * Returns any defined slot within this scope for this name.  This call
     * continues searching through parent scopes if a slot with this name is not
     * found in the current scope.
     * @param name The name of the variable slot to look up.
     * @return The defined slot for the variable, or {@code null} if no
     *         definition exists.
     */
    fn getSlot(name: String) -> S;

    /** Like {@code getSlot} but does not recurse into parent scopes. */
    fn getOwnSlot(name: String) -> S;

    /**
     * Returns the topmost slot containing this name, or null if no slots do. May return true in cases
     * where {@link #getSlot} returns null, though. Do not rely on this method if you need an actual
     * slot.
     *
     * <p>This method is intended for use while scopes are still being built, hence the name
     * 'eventual' declaration. Once scope building is complete, the scope returned from this method
     * must be equivalent to "getSlot(name).getScope()" or null
     */
    fn getTopmostScopeOfEventualDeclaration(name: String) -> Self where Self: Sized {
        todo!();
        // StaticSlot slot = getOwnSlot(name);
        // if (slot != null) {
        //   return slot.getScope();
        // }
        // return getParentScope() != null
        //     ? getParentScope().getTopmostScopeOfEventualDeclaration(name)
        //     : null;
    }
}
