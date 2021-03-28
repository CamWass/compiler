#[rustfmt::skip]
pub mod Flags {
  // Each scope gets a bitset that may contain these flags.
  pub const SCOPE_OTHER:        u16 = 0b000000000;
  pub const SCOPE_PROGRAM:      u16 = 0b000000001;
  pub const SCOPE_FUNCTION:     u16 = 0b000000010;
  pub const SCOPE_ARROW:        u16 = 0b000000100;
  pub const SCOPE_SIMPLE_CATCH: u16 = 0b000001000;
  pub const SCOPE_SUPER:        u16 = 0b000010000;
  pub const SCOPE_DIRECT_SUPER: u16 = 0b000100000;
  pub const SCOPE_CLASS:        u16 = 0b001000000;
  pub const SCOPE_STATIC_BLOCK: u16 = 0b010000000;
  pub const SCOPE_TS_MODULE:    u16 = 0b100000000;
  pub const SCOPE_VAR:          u16 = SCOPE_PROGRAM | SCOPE_FUNCTION | SCOPE_TS_MODULE;

  // These flags are meant to be _only_ used inside the Scope class (or subclasses).
  pub const BIND_KIND_VALUE            :u16 = 0b000000_0000_01;
  pub const BIND_KIND_TYPE             :u16 = 0b000000_0000_10;
  // Used in checkLVal and declareName to determine the type of a binding
  pub const BIND_SCOPE_VAR             :u16 = 0b000000_0001_00; // Var-style binding
  pub const BIND_SCOPE_LEXICAL         :u16 = 0b000000_0010_00; // Let- or const-style binding
  pub const BIND_SCOPE_FUNCTION        :u16 = 0b000000_0100_00; // Function declaration
  pub const BIND_SCOPE_OUTSIDE         :u16 = 0b000000_1000_00; // Special case for function names as
                                                                // bound inside the function
  // Misc flags
  pub const BIND_FLAGS_NONE            :u16 = 0b000001_0000_00;
  pub const BIND_FLAGS_CLASS           :u16 = 0b000010_0000_00;
  pub const BIND_FLAGS_TS_ENUM         :u16 = 0b000100_0000_00;
  pub const BIND_FLAGS_TS_CONST_ENUM   :u16 = 0b001000_0000_00;
  pub const BIND_FLAGS_TS_EXPORT_ONLY  :u16 = 0b010000_0000_00;
  pub const BIND_FLAGS_FLOW_DECLARE_FN :u16 = 0b100000_0000_00;

  // These flags are meant to be _only_ used by Scope consumers
  /*                                  =    is value?    |    is type?    |      scope          |    misc flags    */
  pub const BIND_CLASS           :u16 = BIND_KIND_VALUE | BIND_KIND_TYPE | BIND_SCOPE_LEXICAL  | BIND_FLAGS_CLASS  ;
  pub const BIND_LEXICAL         :u16 = BIND_KIND_VALUE                  | BIND_SCOPE_LEXICAL                      ;
  pub const BIND_VAR             :u16 = BIND_KIND_VALUE                  | BIND_SCOPE_VAR                          ;
  pub const BIND_FUNCTION        :u16 = BIND_KIND_VALUE                  | BIND_SCOPE_FUNCTION                     ;
  pub const BIND_TS_INTERFACE    :u16 =                   BIND_KIND_TYPE                       | BIND_FLAGS_CLASS  ;
  pub const BIND_TS_TYPE         :u16 =                   BIND_KIND_TYPE                                           ;
  pub const BIND_TS_ENUM         :u16 = BIND_KIND_VALUE | BIND_KIND_TYPE | BIND_SCOPE_LEXICAL  | BIND_FLAGS_TS_ENUM;
  pub const BIND_TS_AMBIENT      :u16 =                                                          BIND_FLAGS_TS_EXPORT_ONLY;
  // These bindings don't introduce anything in the scope. They are used for assignments and
  // function expressions IDs.
  pub const BIND_NONE            :u16 =                                                          BIND_FLAGS_NONE   ;
  pub const BIND_OUTSIDE         :u16 = BIND_KIND_VALUE                                        | BIND_FLAGS_NONE   ;

  pub const BIND_TS_CONST_ENUM   :u16 = BIND_TS_ENUM | BIND_FLAGS_TS_CONST_ENUM;
  pub const BIND_TS_NAMESPACE    :u16 =                                                          BIND_FLAGS_TS_EXPORT_ONLY;

  pub const BIND_FLOW_DECLARE_FN :u16 = BIND_FLAGS_FLOW_DECLARE_FN;

pub const CLASS_ELEMENT_FLAG_STATIC   :u16 = 0b1_00;
pub const CLASS_ELEMENT_KIND_GETTER   :u16 = 0b0_10;
pub const CLASS_ELEMENT_KIND_SETTER   :u16 = 0b0_01;
pub const CLASS_ELEMENT_KIND_ACCESSOR :u16 = CLASS_ELEMENT_KIND_GETTER | CLASS_ELEMENT_KIND_SETTER;

pub const CLASS_ELEMENT_STATIC_GETTER   :u16 = CLASS_ELEMENT_KIND_GETTER | CLASS_ELEMENT_FLAG_STATIC;
pub const CLASS_ELEMENT_STATIC_SETTER   :u16 = CLASS_ELEMENT_KIND_SETTER | CLASS_ELEMENT_FLAG_STATIC;
pub const CLASS_ELEMENT_INSTANCE_GETTER :u16 = CLASS_ELEMENT_KIND_GETTER;
pub const CLASS_ELEMENT_INSTANCE_SETTER :u16 = CLASS_ELEMENT_KIND_SETTER;
pub const CLASS_ELEMENT_OTHER           :u16 = 0;
}
