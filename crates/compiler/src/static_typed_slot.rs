use crate::static_slot::StaticSlot;
use crate::static_typed_scope::StaticTypedScope;
use crate::typing::types::Ty;

/**
 * The {@code StaticTypedSlot} interface must be implemented by variables that can appear as members
 * of a {@code StaticTypedScope}.
 *
 */
 pub trait StaticTypedSlot : StaticSlot {
  /**
   * Returns the type information, if any, for this slot.
   *
   * @return The type or {@code null} if no type is declared for it.
   */
  fn getType(&self) -> Option<Ty>;

  /**
   * Returns whether the type has been inferred (as opposed to declared).
   */
  fn isTypeInferred(&self) -> bool;

  // /** Gets the declaration of this symbol. May not exist. */
  // fn getDeclaration(&self) -> StaticTypedRef;

  // fn getScope(&self) -> StaticTypedScope;
}