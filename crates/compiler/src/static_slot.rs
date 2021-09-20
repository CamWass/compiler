/**
 * The {@code StaticSlot} interface must be implemented by variables that can
 * appear as members of a {@code StaticScope}.
 *
 */
pub trait StaticSlot {
    /**
     * Gets the name of the slot.
     */
    fn getName(&self)/* -> String*/;

    /** Gets the declaration of this symbol. May not exist. */
    fn getDeclaration(&self)/* -> StaticRef*/;

    /** Gets the JSDoc for this slot. */
    fn getJSDocInfo(&self)/* -> JSDocInfo*/;

    fn getScope(&self)/* -> StaticScope*/;
}
