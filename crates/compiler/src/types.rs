use crate::ast;
use crate::node::*;
use crate::Checker;
use ahash::{AHashMap, AHashSet};
use bitflags::bitflags;
use std::convert::TryFrom;
use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;
use std::rc::Rc;
use swc_atoms::JsWord;

// TODO: use a hash (u64) instead of string:
pub type TypeRelationTable = AHashMap<String, RelationComparisonResult>;

index::newtype_index! {
    pub struct TypeRelationTableId {
        DEBUG_FORMAT = "SymbolTableId({})"
    }
}

// TODO:
// remove #[derive(Default)] from bitflags. Use ::empty() instead.

index::newtype_index! {
    pub struct SymbolMergeId {
        DEBUG_FORMAT = "SymbolMergeId({})"
    }
}

#[derive(PartialEq, Eq)]
pub enum UnionReduction {
    None,
    Literal,
    Subtype,
}

bitflags! {
    pub struct ContextFlags: u8 {
        const None           = 0;
        const Signature      = 1 << 0; // Obtaining contextual signature
        const NoConstraints  = 1 << 1; // Don't obtain type variable constraints
        const Completions    = 1 << 2; // Ignore inference to current node and parent nodes out to the containing call for completions
        const SkipBindingPatterns = 1 << 3; // Ignore contextual types applied by binding patterns
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TypePredicate {
    ThisTypePredicate {
        ty: TypeId,
    },
    IdentifierTypePredicate {
        parameterName: JsWord,
        parameterIndex: usize,
        ty: TypeId,
    },
    AssertsThisTypePredicate {
        ty: Option<TypeId>,
    },
    AssertsIdentifierTypePredicate {
        parameterName: JsWord,
        parameterIndex: usize,
        ty: Option<TypeId>,
    },
}

impl TypePredicate {
    pub fn new_identifier_type_predicate(
        parameterName: JsWord,
        parameterIndex: usize,
        ty: TypeId,
    ) -> Self {
        Self::IdentifierTypePredicate {
            parameterName,
            parameterIndex,
            ty,
        }
    }

    pub fn ty(&self) -> Option<TypeId> {
        match self {
            TypePredicate::ThisTypePredicate { ty } => Some(*ty),
            TypePredicate::IdentifierTypePredicate { ty, .. } => Some(*ty),
            TypePredicate::AssertsThisTypePredicate { ty } => *ty,
            TypePredicate::AssertsIdentifierTypePredicate { ty, .. } => *ty,
        }
    }

    pub fn parameterIndex(&self) -> Option<usize> {
        match self {
            TypePredicate::ThisTypePredicate { .. } => None,
            TypePredicate::IdentifierTypePredicate { parameterIndex, .. } => Some(*parameterIndex),
            TypePredicate::AssertsThisTypePredicate { .. } => None,
            TypePredicate::AssertsIdentifierTypePredicate { parameterIndex, .. } => {
                Some(*parameterIndex)
            }
        }
    }
}

bitflags! {
    #[derive(Default)]
    pub struct SymbolFlags:u32 {
        const None                    = 0;
        const FunctionScopedVariable  = 1 << 0;   // Variable (var) or parameter
        const BlockScopedVariable     = 1 << 1;   // A block-scoped variable (let or const)
        const Property                = 1 << 2;   // Property or enum member
        const EnumMember              = 1 << 3;   // Enum member
        const Function                = 1 << 4;   // Function
        const Class                   = 1 << 5;   // Class
        const Interface               = 1 << 6;   // Interface
        const ConstEnum               = 1 << 7;   // Const enum
        const RegularEnum             = 1 << 8;   // Enum
        const ValueModule             = 1 << 9;   // Instantiated module
        const NamespaceModule         = 1 << 10;  // Uninstantiated module
        const TypeLiteral             = 1 << 11;  // Type Literal or mapped type
        const ObjectLiteral           = 1 << 12;  // Object Literal
        const Method                  = 1 << 13;  // Method
        const Constructor             = 1 << 14;  // Constructor
        const GetAccessor             = 1 << 15;  // Get accessor
        const SetAccessor             = 1 << 16;  // Set accessor
        const Signature               = 1 << 17;  // Call, construct, or index signature
        const TypeParameter           = 1 << 18;  // Type parameter
        const TypeAlias               = 1 << 19;  // Type alias
        const ExportValue             = 1 << 20;  // Exported value marker (see comment in declareModuleMember in binder)
        const Alias                   = 1 << 21;  // An alias for another symbol (see comment in isAliasSymbolDeclaration in checker)
        const Prototype               = 1 << 22;  // Prototype property (no source representation)
        const ExportStar              = 1 << 23;  // Export * declaration
        const Optional                = 1 << 24;  // Optional property
        const Transient               = 1 << 25;  // Transient symbol (created during type check)
        const Assignment              = 1 << 26;  // Assignment treated as declaration (eg `this.prop = 1`)
        const ModuleExports           = 1 << 27;  // Symbol for CommonJS `module` of `module.exports`
        /* @internal */
        const All = Self::FunctionScopedVariable.bits | Self::BlockScopedVariable.bits | Self::Property.bits | Self::EnumMember.bits | Self::Function.bits | Self::Class.bits | Self::Interface.bits | Self::ConstEnum.bits | Self::RegularEnum.bits | Self::ValueModule.bits | Self::NamespaceModule.bits | Self::TypeLiteral.bits
            | Self::ObjectLiteral.bits | Self::Method.bits | Self::Constructor.bits | Self::GetAccessor.bits | Self::SetAccessor.bits | Self::Signature.bits | Self::TypeParameter.bits | Self::TypeAlias.bits | Self::ExportValue.bits | Self::Alias.bits | Self::Prototype.bits | Self::ExportStar.bits | Self::Optional.bits | Self::Transient.bits;

        const Enum = Self::RegularEnum.bits | Self::ConstEnum.bits;
        const Variable = Self::FunctionScopedVariable.bits | Self::BlockScopedVariable.bits;
        const Value = Self::Variable.bits | Self::Property.bits | Self::EnumMember.bits | Self::ObjectLiteral.bits | Self::Function.bits | Self::Class.bits | Self::Enum.bits | Self::ValueModule.bits | Self::Method.bits | Self::GetAccessor.bits | Self::SetAccessor.bits;
        const Type = Self::Class.bits | Self::Interface.bits | Self::Enum.bits | Self::EnumMember.bits | Self::TypeLiteral.bits | Self::TypeParameter.bits | Self::TypeAlias.bits;
        const Namespace = Self::ValueModule.bits | Self::NamespaceModule.bits | Self::Enum.bits;
        const Module = Self::ValueModule.bits | Self::NamespaceModule.bits;
        const Accessor = Self::GetAccessor.bits | Self::SetAccessor.bits;

        // Variables can be redeclared, but can not redeclare a block-scoped declaration with the
        // same name, or any other value that is not a variable, e.g. ValueModule or Class
        const FunctionScopedVariableExcludes = Self::Value.bits & !Self::FunctionScopedVariable.bits;

        // Block-scoped declarations are not allowed to be re-declared
        // they can not merge with anything in the value space
        const BlockScopedVariableExcludes = Self::Value.bits;

        const ParameterExcludes = Self::Value.bits;
        const PropertyExcludes = Self::None.bits;
        const EnumMemberExcludes = Self::Value.bits | Self::Type.bits;
        const FunctionExcludes = Self::Value.bits & !(Self::Function.bits | Self::ValueModule.bits | Self::Class.bits);
        const ClassExcludes = (Self::Value.bits | Self::Type.bits) & !(Self::ValueModule.bits | Self::Interface.bits | Self::Function.bits); // class-interface mergability done in checker.ts
        const InterfaceExcludes = Self::Type.bits & !(Self::Interface.bits | Self::Class.bits);
        const RegularEnumExcludes = (Self::Value.bits | Self::Type.bits) & !(Self::RegularEnum.bits | Self::ValueModule.bits); // regular enums merge only with regular enums and modules
        const ConstEnumExcludes = (Self::Value.bits | Self::Type.bits) & !Self::ConstEnum.bits; // const enums merge only with const enums
        const ValueModuleExcludes = Self::Value.bits & !(Self::Function.bits | Self::Class.bits | Self::RegularEnum.bits | Self::ValueModule.bits);
        const NamespaceModuleExcludes = 0;
        const MethodExcludes = Self::Value.bits & !Self::Method.bits;
        const GetAccessorExcludes = Self::Value.bits & !Self::SetAccessor.bits;
        const SetAccessorExcludes = Self::Value.bits & !Self::GetAccessor.bits;
        const TypeParameterExcludes = Self::Type.bits & !Self::TypeParameter.bits;
        const TypeAliasExcludes = Self::Type.bits;
        const AliasExcludes = Self::Alias.bits;

        const ModuleMember = Self::Variable.bits | Self::Function.bits | Self::Class.bits | Self::Interface.bits | Self::Enum.bits | Self::Module.bits | Self::TypeAlias.bits | Self::Alias.bits;

        const ExportHasLocal = Self::Function.bits | Self::Class.bits | Self::Enum.bits | Self::ValueModule.bits;

        const BlockScoped = Self::BlockScopedVariable.bits | Self::Class.bits | Self::Enum.bits;

        const PropertyOrAccessor = Self::Property.bits | Self::Accessor.bits;

        const ClassMember = Self::Method.bits | Self::Accessor.bits | Self::Property.bits;

        /* @internal */
        const ExportSupportsDefaultModifier = Self::Class.bits | Self::Function.bits | Self::Interface.bits;

        /* @internal */
        const ExportDoesNotSupportDefaultModifier = !Self::ExportSupportsDefaultModifier.bits;

        /* @internal */
        // The set of things we consider semantically classifiable.  Used to speed up the LS during
        // classification.
        const Classifiable = Self::Class.bits | Self::Enum.bits | Self::TypeAlias.bits | Self::Interface.bits | Self::TypeParameter.bits | Self::Module.bits | Self::Alias.bits;

        /* @internal */
        const LateBindingContainer = Self::Class.bits | Self::Interface.bits | Self::TypeLiteral.bits | Self::ObjectLiteral.bits | Self::Function.bits;
    }
}

index::newtype_index! {
    pub struct SymbolId {
        DEBUG_FORMAT = "SymbolId({})"
    }
}

#[derive(Clone, Debug)]
pub struct BaseSymbol {
    pub flags: SymbolFlags,                   // Symbol flags
    pub escapedName: JsWord,                  // Name of symbol
    pub declarations: Vec<BoundNode>,         // Declarations associated with this symbol
    pub valueDeclaration: Option<BoundNode>,  // First value declaration of the symbol
    pub members: Option<SymbolTableId>,       // Class, interface or object literal instance members
    pub exports: Option<SymbolTableId>,       // Module exports
    pub globalExports: Option<SymbolTableId>, // Conditional global UMD exports
    // id: Option<SymbolId>,             // Unique id (used to look up SymbolLinks)
    pub mergeId: Option<SymbolMergeId>, // Merge id (used to look up merged symbol)
    pub parent: Option<SymbolId>,       // Parent symbol
    pub exportSymbol: Option<SymbolId>, // Exported symbol associated with this symbol
    pub constEnumOnlyModule: bool, // True if module contains only const enums or other modules with only const enums
    pub isReferenced: SymbolFlags, // True if the symbol is referenced elsewhere. Keeps track of the meaning of a reference in case a symbol is both a type parameter and parameter.
    pub isReplaceableByMethod: bool, // Can this Javascript class property be replaced by a method symbol?
    pub isAssigned: bool,            // True if the symbol is a parameter with assignments
                                     // assignmentDeclarationMembers: Option<ESMap<number, Declaration>>, // detected late-bound assignment declarations associated with the symbol
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct SymbolLinksAccessibleChainCacheKey {
    useOnlyExternalAliasing: bool,
    firstRelevantLocation: Option<BoundNode>,
    meaning: SymbolFlags,
}

impl SymbolLinksAccessibleChainCacheKey {
    pub fn new(
        useOnlyExternalAliasing: bool,
        firstRelevantLocation: Option<BoundNode>,
        meaning: SymbolFlags,
    ) -> Self {
        Self {
            useOnlyExternalAliasing,
            firstRelevantLocation,
            meaning,
        }
    }
}

#[derive(Debug)]
pub enum BoundNodeOrFalse {
    BoundNode(BoundNode),
    False,
}

#[derive(Default, Debug)]
pub struct SymbolLinks {
    pub kind: SymbolLinksKind,
    // immediateTarget?: Symbol;                   // Immediate target of an alias. May be another alias. Do not access directly, use `checker.getImmediateAliasedSymbol` instead.
    pub target: Option<SymbolId>, // Resolved (non-alias) target of an alias
    pub ty: Option<TypeId>,       // Type of value symbol
    // writeType?: Type;                           // Type of value symbol in write contexts
    pub nameType: Option<TypeId>, // Type associated with a late-bound symbol
    // uniqueESSymbolType?: Type;                  // UniqueESSymbol type for a symbol
    pub declaredType: Option<TypeId>, // Type of class, interface, enum, type alias, or type parameter
    // typeParameters?: TypeParameter[];           // Type parameters of type alias (undefined if non-generic)
    // outerTypeParameters?: TypeParameter[];      // Outer type parameters of anonymous object type
    // instantiations?: ESMap<string, Type>;       // Instantiations of generic type alias (undefined if non-generic)
    // aliasSymbol?: Symbol;                       // Alias associated with generic type alias instantiation
    // aliasTypeArguments?: readonly Type[]        // Alias type arguments (if any)
    // inferredClassSymbol?: ESMap<SymbolId, TransientSymbol>; // Symbol of an inferred ES5 constructor function
    pub mapper: Option<Rc<TypeMapper>>, // Type mapper for instantiation alias
    // referenced?: boolean;                       // True if alias symbol has been referenced as a value that can be emitted
    // constEnumReferenced?: boolean;              // True if alias symbol resolves to a const enum and is referenced as a value ('referenced' will be false)
    // containingType?: UnionOrIntersectionType;   // Containing union or intersection type for synthetic property
    // leftSpread?: Symbol;                        // Left source for synthetic spread property
    // rightSpread?: Symbol;                       // Right source for synthetic spread property
    // syntheticOrigin?: Symbol;                   // For a property on a mapped or spread type, points back to the original property
    // isDiscriminantProperty?: boolean;           // True if discriminant synthetic property
    pub resolvedExports: Option<SymbolTableId>, // Resolved exports of module or combined early- and late-bound static members of a class.
    pub resolvedMembers: Option<SymbolTableId>, // Combined early- and late-bound members of a symbol
    // exportsChecked?: boolean;                   // True if exports of external module have been checked
    // typeParametersChecked?: boolean;            // True if type parameters of merged class and interface declarations have been checked.
    // isDeclarationWithCollidingName?: boolean;   // True if symbol is block scoped redeclaration
    // bindingElement?: BindingElement;            // Binding element associated with property symbol
    // exportsSomeValue?: boolean;                 // True if module exports some value (not just types)
    // enumKind?: EnumKind;                        // Enum declaration classification
    // originatingImport?: ImportDeclaration | ImportCall; // Import declaration which produced the symbol, present if the symbol is marked as uncallable but had call signatures in `resolveESModuleSymbol`
    pub lateSymbol: Option<SymbolId>, // Late-bound symbol for a computed property
    // specifierCache?: ESMap<string, string>;     // For symbols corresponding to external modules, a cache of incoming path -> module specifier name mappings
    // extendedContainers?: Symbol[];              // Containers (other than the parent) which this symbol is aliased in
    // extendedContainersByFile?: ESMap<NodeId, Symbol[]>; // Containers (other than the parent) which this symbol is aliased in
    // variances?: VarianceFlags[];                // Alias symbol type argument variance cache
    // deferralConstituents?: Type[];              // Calculated list of constituents for a deferred type
    // deferralParent?: Type;                      // Source union/intersection of a deferred type
    // cjsExportMerged?: Symbol;                   // Version of the symbol with all non export= exports merged with the export= target
    pub typeOnlyDeclaration: Option<BoundNodeOrFalse>, // First resolved alias declaration that makes the symbol only usable in type constructs
    pub isConstructorDeclaredProperty: Option<bool>, // Property declared through 'this.x = ...' assignment in constructor
    pub tupleLabelDeclaration: Option<BoundNode>, // Declaration associated with the tuple's label
    // TODO: RC<Vec<_>> maybe?
    pub accessibleChainCache: AHashMap<SymbolLinksAccessibleChainCacheKey, Option<Vec<SymbolId>>>,
}

bitflags! {
    #[derive(Default)]
    pub struct CheckFlags: u32 {
        const Instantiated      = 1 << 0;         // Instantiated symbol
        const SyntheticProperty = 1 << 1;         // Property in union or intersection type
        const SyntheticMethod   = 1 << 2;         // Method in union or intersection type
        const Readonly          = 1 << 3;         // Readonly transient symbol
        const ReadPartial       = 1 << 4;         // Synthetic property present in some but not all constituents
        const WritePartial      = 1 << 5;         // Synthetic property present in some but only satisfied by an index signature in others
        const HasNonUniformType = 1 << 6;         // Synthetic property with non-uniform type in constituents
        const HasLiteralType    = 1 << 7;         // Synthetic property with at least one literal type in constituents
        const ContainsPublic    = 1 << 8;         // Synthetic property with public constituent(s)
        const ContainsProtected = 1 << 9;         // Synthetic property with protected constituent(s)
        const ContainsPrivate   = 1 << 10;        // Synthetic property with private constituent(s)
        const ContainsStatic    = 1 << 11;        // Synthetic property with static constituent(s)
        const Late              = 1 << 12;        // Late-bound symbol for a computed property with a dynamic name
        const ReverseMapped     = 1 << 13;        // Property of reverse-inferred homomorphic mapped type
        const OptionalParameter = 1 << 14;        // Optional parameter
        const RestParameter     = 1 << 15;        // Rest parameter
        const DeferredType      = 1 << 16;        // Calculation of the type of this symbol is deferred due to processing costs, should be fetched with `getTypeOfSymbolWithDeferredType`
        const HasNeverType      = 1 << 17;        // Synthetic property with at least one never type in constituents
        const Mapped            = 1 << 18;        // Property of mapped type
        const StripOptional     = 1 << 19;        // Strip optionality in mapped property
        const Unresolved        = 1 << 20;        // Unresolved type alias symbol
        const Synthetic = Self::SyntheticProperty.bits | Self::SyntheticMethod.bits;
        const Discriminant = Self::HasNonUniformType.bits | Self::HasLiteralType.bits;
        const Partial = Self::ReadPartial.bits | Self::WritePartial.bits;
    }
}

#[derive(Debug)]
pub struct MappedSymbol {
    // pub mappedType: MappedType,
    pub keyType: TypeId,
}

#[derive(Debug)]
pub struct ReverseMappedSymbol {
    pub propertyType: TypeId,
    // pub mappedType: MappedType,
    // pub constraintType: IndexType,
}

#[derive(Debug)]
pub struct TransientSymbol {
    pub flags: SymbolFlags,                   // Symbol flags
    pub escapedName: JsWord,                  // Name of symbol
    pub declarations: Vec<BoundNode>,         // Declarations associated with this symbol
    pub valueDeclaration: Option<BoundNode>,  // First value declaration of the symbol
    pub members: Option<SymbolTableId>,       // Class, interface or object literal instance members
    pub exports: Option<SymbolTableId>,       // Module exports
    pub globalExports: Option<SymbolTableId>, // Conditional global UMD exports
    // id: Option<SymbolId>,             // Unique id (used to look up SymbolLinks)
    pub mergeId: Option<SymbolMergeId>, // Merge id (used to look up merged symbol)
    pub parent: Option<SymbolId>,       // Parent symbol
    pub exportSymbol: Option<SymbolId>, // Exported symbol associated with this symbol
    pub constEnumOnlyModule: bool, // True if module contains only const enums or other modules with only const enums
    pub isReferenced: SymbolFlags, // True if the symbol is referenced elsewhere. Keeps track of the meaning of a reference in case a symbol is both a type parameter and parameter.
    pub isReplaceableByMethod: bool, // Can this Javascript class property be replaced by a method symbol?
    pub isAssigned: bool,            // True if the symbol is a parameter with assignments
    pub assignmentDeclarationMembers: AHashMap<usize, BoundNode>, // detected late-bound assignment declarations associated with the symbol
    pub symbol_links: SymbolLinks,
    pub check_flags: CheckFlags,
}

#[derive(Debug)]
pub enum SymbolLinksKind {
    None,
    Mapped(MappedSymbol),
    ReverseMapped(ReverseMappedSymbol),
}

impl Default for SymbolLinksKind {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug)]
pub enum Symbol {
    Base(BaseSymbol),
    TransientSymbol(TransientSymbol),
}

impl Symbol
// where
//     F: FnMut(&mut Checker<F, TC>, TypeId) -> bool,
//     TC: FnMut(&mut Checker<F, TC>, TypeId, TypeId, bool) -> Ternary,
{
    pub fn new_base_symbol(flags: SymbolFlags, escapedName: JsWord) -> Symbol {
        Symbol::Base(BaseSymbol {
            flags,
            escapedName,
            declarations: Default::default(),
            valueDeclaration: None,
            members: None,
            exports: None,
            globalExports: None,
            // id: Option<SymbolId>,
            mergeId: None,
            parent: None,
            exportSymbol: None,
            constEnumOnlyModule: false,
            isReferenced: SymbolFlags::default(),
            isReplaceableByMethod: false,
            isAssigned: false,
        })
    }

    pub fn new_transient_symbol(
        flags: SymbolFlags,
        escapedName: JsWord,
        check_flags: CheckFlags,
    ) -> Symbol {
        Symbol::TransientSymbol(TransientSymbol {
            flags,
            escapedName,
            declarations: Default::default(),
            valueDeclaration: None,
            members: None,
            exports: None,
            globalExports: None,
            mergeId: None,
            parent: None,
            exportSymbol: None,
            constEnumOnlyModule: false,
            isReferenced: SymbolFlags::default(),
            isReplaceableByMethod: false,
            isAssigned: false,
            assignmentDeclarationMembers: AHashMap::default(),
            symbol_links: SymbolLinks::default(),
            check_flags,
        })
    }

    pub fn as_transient_symbol(&self) -> &TransientSymbol {
        match self {
            Symbol::Base(_) => unreachable!(),
            Symbol::TransientSymbol(t) => t,
        }
    }

    pub fn as_transient_symbol_mut(&mut self) -> &mut TransientSymbol {
        match self {
            Symbol::Base(_) => unreachable!(),
            Symbol::TransientSymbol(t) => t,
        }
    }

    pub fn flags(&self) -> SymbolFlags {
        match self {
            Symbol::Base(s) => s.flags,
            Symbol::TransientSymbol(s) => s.flags,
        }
    }

    pub fn flags_mut(&mut self) -> &mut SymbolFlags {
        match self {
            Symbol::Base(s) => &mut s.flags,
            Symbol::TransientSymbol(s) => &mut s.flags,
        }
    }

    pub fn escapedName(&self) -> &JsWord {
        match self {
            Symbol::Base(s) => &s.escapedName,
            Symbol::TransientSymbol(s) => &s.escapedName,
        }
    }

    pub fn escapedName_mut(&mut self) -> &mut JsWord {
        match self {
            Symbol::Base(s) => &mut s.escapedName,
            Symbol::TransientSymbol(s) => &mut s.escapedName,
        }
    }

    pub fn declarations(&self) -> &Vec<BoundNode> {
        match self {
            Symbol::Base(s) => &s.declarations,
            Symbol::TransientSymbol(s) => &s.declarations,
        }
    }

    pub fn declarations_mut(&mut self) -> &mut Vec<BoundNode> {
        match self {
            Symbol::Base(s) => &mut s.declarations,
            Symbol::TransientSymbol(s) => &mut s.declarations,
        }
    }

    pub fn valueDeclaration(&self) -> &Option<BoundNode> {
        match self {
            Symbol::Base(s) => &s.valueDeclaration,
            Symbol::TransientSymbol(s) => &s.valueDeclaration,
        }
    }

    pub fn valueDeclaration_mut(&mut self) -> &mut Option<BoundNode> {
        match self {
            Symbol::Base(s) => &mut s.valueDeclaration,
            Symbol::TransientSymbol(s) => &mut s.valueDeclaration,
        }
    }

    pub fn members(&self) -> &Option<SymbolTableId> {
        match self {
            Symbol::Base(s) => &s.members,
            Symbol::TransientSymbol(s) => &s.members,
        }
    }

    pub fn members_mut(&mut self) -> &mut Option<SymbolTableId> {
        match self {
            Symbol::Base(s) => &mut s.members,
            Symbol::TransientSymbol(s) => &mut s.members,
        }
    }

    pub fn exports(&self) -> &Option<SymbolTableId> {
        match self {
            Symbol::Base(s) => &s.exports,
            Symbol::TransientSymbol(s) => &s.exports,
        }
    }

    pub fn exports_mut(&mut self) -> &mut Option<SymbolTableId> {
        match self {
            Symbol::Base(s) => &mut s.exports,
            Symbol::TransientSymbol(s) => &mut s.exports,
        }
    }

    pub fn globalExports(&self) -> &Option<SymbolTableId> {
        match self {
            Symbol::Base(s) => &s.globalExports,
            Symbol::TransientSymbol(s) => &s.globalExports,
        }
    }

    pub fn globalExports_mut(&mut self) -> &mut Option<SymbolTableId> {
        match self {
            Symbol::Base(s) => &mut s.globalExports,
            Symbol::TransientSymbol(s) => &mut s.globalExports,
        }
    }

    pub fn mergeId(&self) -> &Option<SymbolMergeId> {
        match self {
            Symbol::Base(s) => &s.mergeId,
            Symbol::TransientSymbol(s) => &s.mergeId,
        }
    }

    pub fn mergeId_mut(&mut self) -> &mut Option<SymbolMergeId> {
        match self {
            Symbol::Base(s) => &mut s.mergeId,
            Symbol::TransientSymbol(s) => &mut s.mergeId,
        }
    }

    pub fn parent(&self) -> Option<SymbolId> {
        match self {
            Symbol::Base(s) => s.parent,
            Symbol::TransientSymbol(s) => s.parent,
        }
    }

    pub fn parent_mut(&mut self) -> &mut Option<SymbolId> {
        match self {
            Symbol::Base(s) => &mut s.parent,
            Symbol::TransientSymbol(s) => &mut s.parent,
        }
    }

    pub fn exportSymbol(&self) -> Option<SymbolId> {
        match self {
            Symbol::Base(s) => s.exportSymbol,
            Symbol::TransientSymbol(s) => s.exportSymbol,
        }
    }

    pub fn exportSymbol_mut(&mut self) -> &mut Option<SymbolId> {
        match self {
            Symbol::Base(s) => &mut s.exportSymbol,
            Symbol::TransientSymbol(s) => &mut s.exportSymbol,
        }
    }

    pub fn constEnumOnlyModule(&self) -> bool {
        match self {
            Symbol::Base(s) => s.constEnumOnlyModule,
            Symbol::TransientSymbol(s) => s.constEnumOnlyModule,
        }
    }

    pub fn constEnumOnlyModule_mut(&mut self) -> &mut bool {
        match self {
            Symbol::Base(s) => &mut s.constEnumOnlyModule,
            Symbol::TransientSymbol(s) => &mut s.constEnumOnlyModule,
        }
    }

    pub fn isReferenced(&self) -> SymbolFlags {
        match self {
            Symbol::Base(s) => s.isReferenced,
            Symbol::TransientSymbol(s) => s.isReferenced,
        }
    }

    pub fn isReferenced_mut(&mut self) -> &mut SymbolFlags {
        match self {
            Symbol::Base(s) => &mut s.isReferenced,
            Symbol::TransientSymbol(s) => &mut s.isReferenced,
        }
    }

    pub fn isReplaceableByMethod(&self) -> bool {
        match self {
            Symbol::Base(s) => s.isReplaceableByMethod,
            Symbol::TransientSymbol(s) => s.isReplaceableByMethod,
        }
    }

    pub fn isReplaceableByMethod_mut(&mut self) -> &mut bool {
        match self {
            Symbol::Base(s) => &mut s.isReplaceableByMethod,
            Symbol::TransientSymbol(s) => &mut s.isReplaceableByMethod,
        }
    }

    pub fn isAssigned(&self) -> bool {
        match self {
            Symbol::Base(s) => s.isAssigned,
            Symbol::TransientSymbol(s) => s.isAssigned,
        }
    }

    pub fn isAssigned_mut(&mut self) -> &mut bool {
        match self {
            Symbol::Base(s) => &mut s.isAssigned,
            Symbol::TransientSymbol(s) => &mut s.isAssigned,
        }
    }

    pub fn assignmentDeclarationMembers(&self) -> Option<&AHashMap<usize, BoundNode>> {
        match self {
            Symbol::Base(_) => None,
            Symbol::TransientSymbol(s) => Some(&s.assignmentDeclarationMembers),
        }
    }
}

// TODO: add these to a forked version of swc_atoms
pub mod InternalSymbolName {
    /// Call signatures.
    pub const Call: &'static str = "__call";
    /// Constructor implementations.
    pub const Constructor: &'static str = "__constructor";
    /// Constructor signatures.
    pub const New: &'static str = "__new";
    /// Index signatures.
    pub const Index: &'static str = "__index";
    /// Module export * declarations.
    pub const ExportStar: &'static str = "__export";
    /// Global self-reference.
    pub const Global: &'static str = "__global";
    /// Indicates missing symbol.
    pub const Missing: &'static str = "__missing";
    /// Anonymous type literal symbol.
    pub const Type: &'static str = "__type";
    /// Anonymous object literal declaration.
    pub const Object: &'static str = "__object";
    /// Anonymous JSX attributes object literal declaration.
    pub const JSXAttributes: &'static str = "__jsxAttributes";
    /// Unnamed class expression.
    pub const Class: &'static str = "__class";
    /// Unnamed function expression.
    pub const Function: &'static str = "__function";
    /// Computed property name declaration with dynamic name.
    pub const Computed: &'static str = "__computed";
    /// Indicator symbol used to mark partially resolved type aliases.
    pub const Resolving: &'static str = "__resolving__";
    /// Export assignment symbol.
    pub const ExportEquals: &'static str = "export=";
    /// Default export symbol (technically not wholly internal, but included here for usability).
    pub const Default: &'static str = "default";
    pub const This: &'static str = "this";
}

// TODO: use this:
// /// This represents a string whose leading underscore have been escaped by adding extra leading underscores.
// ///
// /// e.g. `"__a"` => `"___a"` (note the exta underscore).
// ///
// /// This avoids issues with magic names like `'__proto__'`.
// pub type JsWord = swc_atoms::JsWord;

pub type SymbolTable = AHashMap<JsWord, SymbolId>;

index::newtype_index! {
    pub struct SymbolTableId {
        DEBUG_FORMAT = "SymbolTableId({})"
    }
}

bitflags! {
    #[derive(Default)]
    pub struct NodeCheckFlags: u32 {
        const TypeChecked                              = 0x00000001;  // Node has been type checked
        const LexicalThis                              = 0x00000002;  // Lexical 'this' reference
        const CaptureThis                              = 0x00000004;  // Lexical 'this' used in body
        const CaptureNewTarget                         = 0x00000008;  // Lexical 'new.target' used in body
        const SuperInstance                            = 0x00000100;  // Instance 'super' reference
        const SuperStatic                              = 0x00000200;  // Static 'super' reference
        const ContextChecked                           = 0x00000400;  // Contextual types have been assigned
        const AsyncMethodWithSuper                     = 0x00000800;  // An async method that reads a value from a member of 'super'.
        const AsyncMethodWithSuperBinding              = 0x00001000;  // An async method that assigns a value to a member of 'super'.
        const CaptureArguments                         = 0x00002000;  // Lexical 'arguments' used in body
        const EnumValuesComputed                       = 0x00004000;  // Values for enum members have been computed, and any errors have been reported for them.
        const LexicalModuleMergesWithClass             = 0x00008000;  // Instantiated lexical module declaration is merged with a previous class declaration.
        const LoopWithCapturedBlockScopedBinding       = 0x00010000;  // Loop that contains block scoped variable captured in closure
        const ContainsCapturedBlockScopeBinding        = 0x00020000;  // Part of a loop that contains block scoped variable captured in closure
        const CapturedBlockScopedBinding               = 0x00040000;  // Block-scoped binding that is captured in some function
        const BlockScopedBindingInLoop                 = 0x00080000;  // Block-scoped binding with declaration nested inside iteration statement
        const ClassWithBodyScopedClassBinding          = 0x00100000;  // Decorated class that contains a binding to itself inside of the class body.
        const BodyScopedClassBinding                   = 0x00200000;  // Binding to a decorated class inside of the class's body.
        const NeedsLoopOutParameter                    = 0x00400000;  // Block scoped binding whose value should be explicitly copied outside of the converted loop
        const AssignmentsMarked                        = 0x00800000;  // Parameter assignments have been marked
        const ClassWithConstructorReference            = 0x01000000;  // Class that contains a binding to its constructor inside of the class body.
        const ConstructorReferenceInClass              = 0x02000000;  // Binding to a class constructor inside of the class's body.
        const ContainsClassWithPrivateIdentifiers      = 0x04000000;  // Marked on all block-scoped containers containing a class with private identifiers.
        const ContainsSuperPropertyInStaticInitializer = 0x08000000;  // Marked on all block-scoped containers containing a static initializer with 'super.x' or 'super[x]'.
    }
}

#[derive(Default)]
pub struct NodeLinks {
    pub flags: NodeCheckFlags,        // Set of flags specific to Node
    pub resolvedType: Option<TypeId>, // Cached type of type node
    //     resolvedEnumType?: Type;            // Cached constraint type from enum jsdoc tag
    pub resolvedSignature: Option<SignatureId>, // Cached signature of signature node or call expression
    pub resolvedSymbol: Option<SymbolId>,       // Cached name resolution result
    //     resolvedIndexInfo?: IndexInfo;      // Cached indexing info resolution result
    //     effectsSignature?: Signature;       // Signature with possible control flow effects
    //     enumMemberValue?: string | number;  // Constant value of enum member
    //     isVisible?: boolean;                // Is this node visible
    //     containsArgumentsReference?: boolean; // Whether a function-like declaration contains an 'arguments' reference
    //     hasReportedStatementInAmbientContext?: boolean; // Cache boolean if we report statements in ambient context
    //     jsxFlags: JsxFlags;                 // flags for knowing what kind of element/attributes we're dealing with
    //     resolvedJsxElementAttributesType?: Type; // resolved element attributes type of a JSX openinglike element
    //     resolvedJsxElementAllAttributesType?: Type; // resolved all element attributes type of a JSX openinglike element
    //     resolvedJSDocType?: Type;           // Resolved type of a JSDoc type reference
    //     switchTypes?: Type[];               // Cached array of switch case expression types
    //     jsxNamespace?: Symbol | false;      // Resolved jsx namespace symbol for this node
    //     jsxImplicitImportContainer?: Symbol | false; // Resolved module symbol the implicit jsx import of this file should refer to
    //     contextFreeType?: Type;             // Cached context-free type used by the first pass of inference; used when a function's return is partially contextually sensitive
    pub deferredNodes: AHashSet<BoundNode>, // Set of nodes whose checking has been deferred
    //     capturedBlockScopeBindings?: Symbol[]; // Block-scoped bindings captured beneath this part of an IterationStatement
    pub outerTypeParameters: Option<Rc<Vec<TypeId>>>, // Outer type parameters of anonymous object type
    //     isExhaustive?: boolean;             // Is node an exhaustive switch statement
    //     skipDirectInference?: true;         // Flag set by the API `getContextualType` call on a node when `Completions` is passed to force the checker to skip making inferences to a node's type
    pub declarationRequiresScopeChange: Option<bool>, // Set by `useOuterVariableScopeInParameter` in checker when downlevel emit would change the name resolution scope inside of a parameter.
                                                      //     serializedTypes?: ESMap<string, TypeNode & {truncating?: boolean, addedLength: number}>; // Collection of types serialized at this location
}

bitflags! {
    #[derive(Default)]
    pub struct TypeFlags :u32 {
        const Any             = 1 << 0;
        const Unknown         = 1 << 1;
        const String          = 1 << 2;
        const Number          = 1 << 3;
        const Boolean         = 1 << 4;
        const Enum            = 1 << 5;
        const BigInt          = 1 << 6;
        const StringLiteral   = 1 << 7;
        const NumberLiteral   = 1 << 8;
        const BooleanLiteral  = 1 << 9;
        const EnumLiteral     = 1 << 10;  // Always combined with StringLiteral, NumberLiteral, or Union
        const BigIntLiteral   = 1 << 11;
        const ESSymbol        = 1 << 12;  // Type of symbol primitive introduced in ES6
        const UniqueESSymbol  = 1 << 13;  // unique symbol
        const Void            = 1 << 14;
        const Undefined       = 1 << 15;
        const Null            = 1 << 16;
        const Never           = 1 << 17;  // Never type
        const TypeParameter   = 1 << 18;  // Type parameter
        const Object          = 1 << 19;  // Object type
        const Union           = 1 << 20;  // Union (T | U)
        const Intersection    = 1 << 21;  // Intersection (T & U)
        const Index           = 1 << 22;  // keyof T
        const IndexedAccess   = 1 << 23;  // T[K]
        const Conditional     = 1 << 24;  // T extends U ? X : Y
        const Substitution    = 1 << 25;  // Type parameter substitution
        const NonPrimitive    = 1 << 26;  // intrinsic object type
        const TemplateLiteral = 1 << 27;  // Template literal type
        const StringMapping   = 1 << 28;  // Uppercase/Lowercase type

        const AnyOrUnknown = Self::Any.bits | Self::Unknown.bits;
        const Nullable = Self::Undefined.bits | Self::Null.bits;
        const Literal = Self::StringLiteral.bits | Self::NumberLiteral.bits | Self::BigIntLiteral.bits | Self::BooleanLiteral.bits;
        const Unit = Self::Literal.bits | Self::UniqueESSymbol.bits | Self::Nullable.bits;
        const StringOrNumberLiteral = Self::StringLiteral.bits | Self::NumberLiteral.bits;
        const StringOrNumberLiteralOrUnique = Self::StringLiteral.bits | Self::NumberLiteral.bits | Self::UniqueESSymbol.bits;
        const DefinitelyFalsy = Self::StringLiteral.bits | Self::NumberLiteral.bits | Self::BigIntLiteral.bits | Self::BooleanLiteral.bits | Self::Void.bits | Self::Undefined.bits | Self::Null.bits;
        const PossiblyFalsy = Self::DefinitelyFalsy.bits | Self::String.bits | Self::Number.bits | Self::BigInt.bits | Self::Boolean.bits;
        const Intrinsic = Self::Any.bits | Self::Unknown.bits | Self::String.bits | Self::Number.bits | Self::BigInt.bits | Self::Boolean.bits | Self::BooleanLiteral.bits | Self::ESSymbol.bits | Self::Void.bits | Self::Undefined.bits | Self::Null.bits | Self::Never.bits | Self::NonPrimitive.bits;
        const Primitive = Self::String.bits | Self::Number.bits | Self::BigInt.bits | Self::Boolean.bits | Self::Enum.bits | Self::EnumLiteral.bits | Self::ESSymbol.bits | Self::Void.bits | Self::Undefined.bits | Self::Null.bits | Self::Literal.bits | Self::UniqueESSymbol.bits;
        const StringLike = Self::String.bits | Self::StringLiteral.bits | Self::TemplateLiteral.bits | Self::StringMapping.bits;
        const NumberLike = Self::Number.bits | Self::NumberLiteral.bits | Self::Enum.bits;
        const BigIntLike = Self::BigInt.bits | Self::BigIntLiteral.bits;
        const BooleanLike = Self::Boolean.bits | Self::BooleanLiteral.bits;
        const EnumLike = Self::Enum.bits | Self::EnumLiteral.bits;
        const ESSymbolLike = Self::ESSymbol.bits | Self::UniqueESSymbol.bits;
        const VoidLike = Self::Void.bits | Self::Undefined.bits;
        const DisjointDomains = Self::NonPrimitive.bits | Self::StringLike.bits | Self::NumberLike.bits | Self::BigIntLike.bits | Self::BooleanLike.bits | Self::ESSymbolLike.bits | Self::VoidLike.bits | Self::Null.bits;
        const UnionOrIntersection = Self::Union.bits | Self::Intersection.bits;
        const StructuredType = Self::Object.bits | Self::Union.bits | Self::Intersection.bits;
        const TypeVariable = Self::TypeParameter.bits | Self::IndexedAccess.bits;
        const InstantiableNonPrimitive = Self::TypeVariable.bits | Self::Conditional.bits | Self::Substitution.bits;
        const InstantiablePrimitive = Self::Index.bits | Self::TemplateLiteral.bits | Self::StringMapping.bits;
        const Instantiable = Self::InstantiableNonPrimitive.bits | Self::InstantiablePrimitive.bits;
        const StructuredOrInstantiable = Self::StructuredType.bits | Self::Instantiable.bits;
        const ObjectFlagsType = Self::Any.bits | Self::Nullable.bits | Self::Never.bits | Self::Object.bits | Self::Union.bits | Self::Intersection.bits;
        const Simplifiable = Self::IndexedAccess.bits | Self::Conditional.bits;
        const Singleton = Self::Any.bits | Self::Unknown.bits | Self::String.bits | Self::Number.bits | Self::Boolean.bits | Self::BigInt.bits | Self::ESSymbol.bits | Self::Void.bits | Self::Undefined.bits | Self::Null.bits | Self::Never.bits | Self::NonPrimitive.bits;
        // 'Narrowable' types are types where narrowing actually narrows.
        // This *should* be every type other than null, undefined, void, and never
        const Narrowable = Self::Any.bits | Self::Unknown.bits | Self::StructuredOrInstantiable.bits | Self::StringLike.bits | Self::NumberLike.bits | Self::BigIntLike.bits | Self::BooleanLike.bits | Self::ESSymbol.bits | Self::UniqueESSymbol.bits | Self::NonPrimitive.bits;
        const NotPrimitiveUnion = Self::Any.bits | Self::Unknown.bits | Self::Enum.bits | Self::Void.bits | Self::Never.bits | Self::Object.bits | Self::Intersection.bits | Self::Instantiable.bits;
        // The following flags are aggregated during union and intersection type construction
        const IncludesMask = Self::Any.bits | Self::Unknown.bits | Self::Primitive.bits | Self::Never.bits | Self::Object.bits | Self::Union.bits | Self::Intersection.bits | Self::NonPrimitive.bits | Self::TemplateLiteral.bits;
        // The following flags are used for different purposes during union and intersection type construction
        const IncludesMissingType = Self::TypeParameter.bits;
        const IncludesNonWideningType = Self::Index.bits;
        const IncludesWildcard = Self::IndexedAccess.bits;
        const IncludesEmptyObject = Self::Conditional.bits;
    }
}

index::newtype_index! {
    pub struct TypeId {
        DEBUG_FORMAT = "TypeId({})"
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct TypeList(pub u64);

impl TypeList {
    pub fn new(types: Option<&Vec<TypeId>>) -> Self {
        let mut s = ahash::AHasher::default();
        types.hash(&mut s);
        Self(s.finish())
        // TODO: is the following needed/more correct than above
        // let result = "";
        // if (types) {
        //     const length = types.length;
        //     let i = 0;
        //     while (i < length) {
        //         const startId = types[i].id;
        //         let count = 1;
        //         while (i + count < length && types[i + count].id === startId + count) {
        //             count++;
        //         }
        //         if (result.length) {
        //             result += ",";
        //         }
        //         result += startId;
        //         if (count > 1) {
        //             result += ":" + count;
        //         }
        //         i += count;
        //     }
        // }
        // return result;
    }
}

// Properties common to all types
// pub struct Type {
//     pub kind: TypeKind,
//     pub flags: TypeFlags,         // Flags
//     pub symbol: Option<SymbolId>, // Symbol associated with type (if any)
//     // pattern?: DestructuringPattern;  // Destructuring pattern represented by type (if any)
//     // aliasSymbol?: Symbol;            // Alias associated with type
//     // aliasTypeArguments?: readonly Type[]; // Alias type arguments (if any)
//     /// Alias type arguments (if any)
//     pub aliasTypeArgumentsContainsMarker: bool,
//     /// Instantiation with type parameters mapped to wildcard type
//     pub permissiveInstantiation: Option<TypeId>,
//     /// Instantiation with type parameters mapped to unconstrained form
//     pub restrictiveInstantiation: Option<TypeId>,
//     /// Immediate base constraint cache
//     pub immediateBaseConstraint: Option<TypeId>,
//     /// Cached widened form of the type
//     pub widened: Option<TypeId>,
// }

// impl Type {
//     fn new(kind: TypeKind, flags: TypeFlags) -> Type {
//         Type {
//             kind,
//             flags,
//             symbol: None,
//             // pattern?: DestructuringPattern;
//             // aliasSymbol?: Symbol;
//             // aliasTypeArguments?: readonly Type[];
//             aliasTypeArgumentsContainsMarker: false,
//             permissiveInstantiation: None,
//             restrictiveInstantiation: None,
//             immediateBaseConstraint: None,
//             widened: None,
//         }
//     }

//     pub fn new_intrinsic(
//         flags: TypeFlags,
//         intrinsic_name: &'static str,
//         object_flags: ObjectFlags,
//     ) -> Type {
//         Type::new(
//             TypeKind::Intrinsic(IntrinsicType {
//                 intrinsic_name,
//                 object_flags,
//                 sub_kind: IntrinsicTypeSubKind::None,
//             }),
//             flags,
//         )
//     }

//     pub fn new_freshable_intrinsic(
//         flags: TypeFlags,
//         intrinsic_name: &'static str,
//         object_flags: ObjectFlags,
//         regularType: TypeId,
//         freshType: TypeId,
//     ) -> Type {
//         Type::new(
//             TypeKind::Intrinsic(IntrinsicType {
//                 intrinsic_name,
//                 object_flags,
//                 sub_kind: IntrinsicTypeSubKind::Freshable(FreshableIntrinsicType {
//                     regularType,
//                     freshType,
//                 }),
//             }),
//             flags,
//         )
//     }
// }

// pub enum TypeKind {
//     Intrinsic(IntrinsicType),
//     UnionOrIntersection(UnionOrIntersectionType),
//     Instantiable(InstantiableType),
//     Object(ObjectType),
// }

// /// Intrinsic types (TypeFlags.Intrinsic)
// pub struct IntrinsicType {
//     /// Name of intrinsic type
//     pub intrinsic_name: &'static str,
//     pub object_flags: ObjectFlags,
//     pub sub_kind: IntrinsicTypeSubKind,
// }

// pub enum IntrinsicTypeSubKind {
//     None,
//     Nullable(NullableType),
//     Freshable(FreshableIntrinsicType),
// }

// pub struct NullableType {
//     pub objectFlags: ObjectFlags,
// }

// pub struct FreshableIntrinsicType {
//     // // Fresh version of type
//     // freshType: IntrinsicType,
//     // // Regular version of type
//     // regularType: IntrinsicType,

//     // Fresh version of type
//     pub freshType: TypeId,
//     // Regular version of type
//     pub regularType: TypeId,
// }

// /* @internal */
// export type FreshableType = LiteralType | FreshableIntrinsicType;

// // String literal types (TypeFlags.StringLiteral)
// // Numeric literal types (TypeFlags.NumberLiteral)
// // BigInt literal types (TypeFlags.BigIntLiteral)
// export interface LiteralType extends Type {
//     value: string | number | PseudoBigInt; // Value of literal
//     freshType: LiteralType;                // Fresh version of type
//     regularType: LiteralType;              // Regular version of type
// }

// // Unique symbol types (TypeFlags.UniqueESSymbol)
// export interface UniqueESSymbolType extends Type {
//     symbol: Symbol;
//     escapedName: __String;
// }

// export interface StringLiteralType extends LiteralType {
//     value: string;
// }

// export interface NumberLiteralType extends LiteralType {
//     value: number;
// }

// export interface BigIntLiteralType extends LiteralType {
//     value: PseudoBigInt;
// }

// // Enum types (TypeFlags.Enum)
// export interface EnumType extends Type {
// }

bitflags! {
    /// Types included in TypeFlags.ObjectFlagsType have an objectFlags property. Some ObjectFlags
    /// are specific to certain types and reuse the same bit position. Those ObjectFlags require a check
    /// for a certain TypeFlags value to determine their meaning.
    #[derive(Default)]
    pub struct ObjectFlags : u32{
        const Class            = 1 << 0;  // Class
        const Interface        = 1 << 1;  // Interface
        const Reference        = 1 << 2;  // Generic type reference
        const Tuple            = 1 << 3;  // Synthesized generic tuple type
        const Anonymous        = 1 << 4;  // Anonymous
        const Mapped           = 1 << 5;  // Mapped
        const Instantiated     = 1 << 6;  // Instantiated anonymous or mapped type
        const ObjectLiteral    = 1 << 7;  // Originates in an object literal
        const EvolvingArray    = 1 << 8;  // Evolving array type
        const ObjectLiteralPatternWithComputedProperties = 1 << 9;  // Object literal pattern with computed properties
        const ReverseMapped    = 1 << 10; // Object contains a property from a reverse-mapped type
        const JsxAttributes    = 1 << 11; // Jsx attributes type
        const MarkerType       = 1 << 12; // Marker type used for variance probing
        const JSLiteral        = 1 << 13; // Object type declared in JS - disables errors on read/write of nonexisting members
        const FreshLiteral     = 1 << 14; // Fresh object literal
        const ArrayLiteral     = 1 << 15; // Originates in an array literal
        const PrimitiveUnion   = 1 << 16; // Union of only primitive types
        const ContainsWideningType = 1 << 17; // Type is or contains undefined or null widening type
        const ContainsObjectOrArrayLiteral = 1 << 18; // Type is or contains object literal type
        const NonInferrableType = 1 << 19; // Type is or contains anyFunctionType or silentNeverType
        const CouldContainTypeVariablesComputed = 1 << 20; // CouldContainTypeVariables flag has been computed
        const CouldContainTypeVariables = 1 << 21; // Type could contain a type variable

        const ClassOrInterface = Self::Class.bits | Self::Interface.bits;
        const RequiresWidening = Self::ContainsWideningType.bits | Self::ContainsObjectOrArrayLiteral.bits;
        const PropagatingFlags = Self::ContainsWideningType.bits | Self::ContainsObjectOrArrayLiteral.bits | Self::NonInferrableType.bits;
        // Object flags that uniquely identify the kind of ObjectType
        const ObjectTypeKindMask = Self::ClassOrInterface.bits | Self::Reference.bits | Self::Tuple.bits | Self::Anonymous.bits | Self::Mapped.bits | Self::ReverseMapped.bits | Self::EvolvingArray.bits;

        // Flags that require TypeFlags.Object
        const ContainsSpread   = 1 << 22;  // Object literal contains spread operation
        const ObjectRestType   = 1 << 23;  // Originates in object rest declaration
        const IsClassInstanceClone = 1 << 24; // Type is a clone of a class instance type
        // Flags that require TypeFlags.Object and ObjectFlags.Reference
        const IdenticalBaseTypeCalculated = 1 << 25; // has had `getSingleBaseForNonAugmentingSubtype` invoked on it already
        const IdenticalBaseTypeExists = 1 << 26; // has a defined cachedEquivalentBaseType member

        // Flags that require TypeFlags.UnionOrIntersection or TypeFlags.Substitution
        const IsGenericTypeComputed = 1 << 22; // IsGenericObjectType flag has been computed
        const IsGenericObjectType = 1 << 23; // Union or intersection contains generic object type
        const IsGenericIndexType = 1 << 24; // Union or intersection contains generic index type
        const IsGenericType = Self::IsGenericObjectType.bits | Self::IsGenericIndexType.bits;

        // Flags that require TypeFlags.Union
        const ContainsIntersections = 1 << 25; // Union contains intersections

        // Flags that require TypeFlags.Intersection
        const IsNeverIntersectionComputed = 1 << 25; // IsNeverLike flag has been computed
        const IsNeverIntersection = 1 << 26; // Intersection reduces to never
    }
}

// /* @internal */
// export type ObjectFlagsType = NullableType | ObjectType | UnionType | IntersectionType;

// Object types (TypeFlags.ObjectType)
// pub struct ObjectType {
//     pub kind: ObjectTypeKind,
//     pub objectFlags: ObjectFlags,
//     pub members: SymbolTable, // Properties by name
//     pub properties: Vec<SymbolId>, // Properties
//                               // pub callSignatures: Option<readonly Signature[]>,      // Call signatures of type
//                               // pub constructSignatures: Option<readonly Signature[]>, // Construct signatures of type
//                               // pub indexInfos: Option<readonly IndexInfo[]>,  // Index signatures
//                               // pub objectTypeWithoutAbstractConstructSignatures: Option<ObjectType>,
// }

// pub enum ObjectTypeKind {
//     Resolved(ResolvedType),
// }

// /** Class and interface types (ObjectFlags.Class and ObjectFlags.Interface). */
// export interface InterfaceType extends ObjectType {
//     typeParameters: TypeParameter[] | undefined;      // Type parameters (undefined if non-generic)
//     outerTypeParameters: TypeParameter[] | undefined; // Outer type parameters (undefined if none)
//     localTypeParameters: TypeParameter[] | undefined; // Local type parameters (undefined if none)
//     thisType: TypeParameter | undefined;              // The "this" type (undefined if none)
//     /* @internal */
//     resolvedBaseConstructorType?: Type;               // Resolved base constructor type of class
//     /* @internal */
//     resolvedBaseTypes: BaseType[];                    // Resolved base types
//     /* @internal */
//     baseTypesResolved?: boolean;
// }

// // Object type or intersection of object types
// export type BaseType = ObjectType | IntersectionType | TypeVariable; // Also `any` and `object`

// export interface InterfaceTypeWithDeclaredMembers extends InterfaceType {
//     declaredProperties: Symbol[];                   // Declared members
//     declaredCallSignatures: Signature[];            // Declared call signatures
//     declaredConstructSignatures: Signature[];       // Declared construct signatures
//     declaredIndexInfos: IndexInfo[];                // Declared index signatures
// }

// /**
//  * Type references (ObjectFlags.Reference). When a class or interface has type parameters or
//  * a "this" type, references to the class or interface are made using type references. The
//  * typeArguments property specifies the types to substitute for the type parameters of the
//  * class or interface and optionally includes an extra element that specifies the type to
//  * substitute for "this" in the resulting instantiation. When no extra argument is present,
//  * the type reference itself is substituted for "this". The typeArguments property is undefined
//  * if the class or interface has no type parameters and the reference isn't specifying an
//  * explicit "this" argument.
//  */
// export interface TypeReference extends ObjectType {
//     target: GenericType;    // Type reference target
//     node?: TypeReferenceNode | ArrayTypeNode | TupleTypeNode;
//     /* @internal */
//     mapper?: TypeMapper;
//     /* @internal */
//     resolvedTypeArguments?: readonly Type[];  // Resolved type reference type arguments
//     /* @internal */
//     literalType?: TypeReference;  // Clone of type with ObjectFlags.ArrayLiteral set
//     /* @internal */
//     cachedEquivalentBaseType?: Type; // Only set on references to class or interfaces with a single base type and no augmentations
// }

// export interface DeferredTypeReference extends TypeReference {
//     /* @internal */
//     node: TypeReferenceNode | ArrayTypeNode | TupleTypeNode;
//     /* @internal */
//     mapper?: TypeMapper;
//     /* @internal */
//     instantiations?: ESMap<string, Type>; // Instantiations of generic type alias (undefined if non-generic)
// }

// bitflags! {
//     pub struct VarianceFlags: u8 {
//         const Invariant     =      0;  // Neither covariant nor contravariant
//         const Covariant     = 1 << 0;  // Covariant
//         const Contravariant = 1 << 1;  // Contravariant
//         const Bivariant     = Self::Covariant.bits | Self::Contravariant.bits;  // Both covariant and contravariant
//         const Independent   = 1 << 2;  // Unwitnessed type parameter
//         const VarianceMask  = Self::Invariant.bits | Self::Covariant.bits | Self::Contravariant.bits | Self::Independent.bits; // Mask containing all measured variances without the unmeasurable flag
//         const Unmeasurable  = 1 << 3;  // Variance result is unusable - relationship relies on structural comparisons which are not reflected in generic relationships
//         const Unreliable    = 1 << 4;  // Variance result is unreliable - checking may produce false negatives, but not false positives
//         const AllowsStructuralFallback = Self::Unmeasurable.bits | Self::Unreliable.bits;
//     }
// }

// // Generic class and interface types
// export interface GenericType extends InterfaceType, TypeReference {
//     /* @internal */
//     instantiations: ESMap<string, TypeReference>;  // Generic instantiation cache
//     /* @internal */
//     variances?: VarianceFlags[];  // Variance of each type parameter
// }

// export const enum ElementFlags {
//     Required    = 1 << 0,  // T
//     Optional    = 1 << 1,  // T?
//     Rest        = 1 << 2,  // ...T[]
//     Variadic    = 1 << 3,  // ...T
//     Fixed       = Required | Optional,
//     Variable    = Rest | Variadic,
//     NonRequired = Optional | Rest | Variadic,
//     NonRest     = Required | Optional | Variadic,
// }

// export interface TupleType extends GenericType {
//     elementFlags: readonly ElementFlags[];
//     minLength: number;  // Number of required or variadic elements
//     fixedLength: number;  // Number of initial required or optional elements
//     hasRestElement: boolean;  // True if tuple has any rest or variadic elements
//     combinedFlags: ElementFlags;
//     readonly: boolean;
//     labeledElementDeclarations?: readonly (NamedTupleMember | ParameterDeclaration)[];
// }

// export interface TupleTypeReference extends TypeReference {
//     target: TupleType;
// }

// pub struct UnionOrIntersectionType {
//     kind: UnionOrIntersectionTypeKind,
//     // Constituent types
//     types: Vec<TypeId>,
//     objectFlags: ObjectFlags,
//     // Cache of resolved properties
//     propertyCache: SymbolTable,
//     // Cache of resolved properties that does not augment function or object type properties
//     propertyCacheWithoutObjectFunctionPropertyAugment: SymbolTable,
//     resolvedProperties: Vec<SymbolId>,
//     // resolvedIndexType: IndexType,
//     // resolvedStringIndexType: IndexType,
//     resolvedIndexType: TypeId,
//     resolvedStringIndexType: TypeId,
//     resolvedBaseConstraint: TypeId,
// }

// pub enum UnionOrIntersectionTypeKind {
//     Union(UnionType),
//     Intersection(IntersectionType),
//     Resolved(ResolvedType),
// }

// pub struct UnionType {
//     resolvedReducedType: Option<TypeId>,
//     // regularType: Option<UnionType>,
//     regularType: Option<TypeId>,
//     /// Denormalized union, intersection, or index type in which union originates
//     origin: Option<TypeId>,
//     /// Property with unique unit type that exists in every object/intersection in union type
//     keyPropertyName: Option<JsWord>,
//     // /// Constituents keyed by unit type discriminants
//     // constituentMap?: ESMap<TypeId, Type>;
// }

// pub struct IntersectionType {
//     resolvedApparentType: TypeId,
// }

// export type StructuredType = ObjectType | UnionType | IntersectionType;

// /* @internal */
// // An instantiated anonymous type has a target and a mapper
// export interface AnonymousType extends ObjectType {
//     target?: AnonymousType;  // Instantiation target
//     mapper?: TypeMapper;     // Instantiation mapper
//     instantiations?: ESMap<string, Type>; // Instantiations of generic type alias (undefined if non-generic)
// }

// /* @internal */
// export interface MappedType extends AnonymousType {
//     declaration: MappedTypeNode;
//     typeParameter?: TypeParameter;
//     constraintType?: Type;
//     nameType?: Type;
//     templateType?: Type;
//     modifiersType?: Type;
//     resolvedApparentType?: Type;
//     containsError?: boolean;
// }

// export interface EvolvingArrayType extends ObjectType {
//     elementType: Type;      // Element expressions of evolving array type
//     finalArrayType?: Type;  // Final array type of evolving array type
// }

// /* @internal */
// export interface ReverseMappedType extends ObjectType {
//     source: Type;
//     mappedType: MappedType;
//     constraintType: IndexType;
// }

// Resolved object, union, or intersection type
// pub struct ResolvedType {
//     // TODO: these are duplicated for ObjectType { ObjectTypeKind::Resolved(..), ..}
//     pub members: SymbolTable, // Properties by name
//     pub properties: Vec<SymbolId>, // Properties
//                               // pub callSignatures: readonly Signature[],      // Call signatures of type
//                               // pub constructSignatures: readonly Signature[], // Construct signatures of type
//                               // pub indexInfos: readonly IndexInfo[],  // Index signatures
// }

// /* @internal */
// // Object literals are initially marked fresh. Freshness disappears following an assignment,
// // before a type assertion, or when an object literal's type is widened. The regular
// // version of a fresh type is identical except for the TypeFlags.FreshObjectLiteral flag.
// export interface FreshObjectLiteralType extends ResolvedType {
//     regularType: ResolvedType;  // Regular version of fresh type
// }

// /* @internal */
// export interface IterationTypes {
//     readonly yieldType: Type;
//     readonly returnType: Type;
//     readonly nextType: Type;
// }

// // Just a place to cache element types of iterables and iterators
// /* @internal */
// export interface IterableOrIteratorType extends ObjectType, UnionType {
//     iterationTypesOfGeneratorReturnType?: IterationTypes;
//     iterationTypesOfAsyncGeneratorReturnType?: IterationTypes;
//     iterationTypesOfIterable?: IterationTypes;
//     iterationTypesOfIterator?: IterationTypes;
//     iterationTypesOfAsyncIterable?: IterationTypes;
//     iterationTypesOfAsyncIterator?: IterationTypes;
//     iterationTypesOfIteratorResult?: IterationTypes;
// }

// /* @internal */
// export interface PromiseOrAwaitableType extends ObjectType, UnionType {
//     promiseTypeOfPromiseConstructor?: Type;
//     promisedTypeOfPromise?: Type;
//     awaitedTypeOfType?: Type;
// }

// /* @internal */
// export interface SyntheticDefaultModuleType extends Type {
//     syntheticType?: Type;
// }

// pub struct InstantiableType {
//     kind: InstantiableTypeKind,
//     resolvedBaseConstraint: Option<TypeId>,
//     // resolvedIndexType: Option<IndexType>,
//     // resolvedStringIndexType: Option<IndexType>,
//     resolvedIndexType: Option<TypeId>,
//     resolvedStringIndexType: Option<TypeId>,
// }

// pub enum InstantiableTypeKind {
//     Index(IndexType),
// }

// // Type parameters (TypeFlags.TypeParameter)
// export interface TypeParameter extends InstantiableType {
//     /** Retrieve using getConstraintFromTypeParameter */
//     /* @internal */
//     constraint?: Type;        // Constraint
//     /* @internal */
//     default?: Type;
//     /* @internal */
//     target?: TypeParameter;  // Instantiation target
//     /* @internal */
//     mapper?: TypeMapper;     // Instantiation mapper
//     /* @internal */
//     isThisType?: boolean;
//     /* @internal */
//     resolvedDefaultType?: Type;
// }

// /* @internal */
// export const enum AccessFlags {
//     None = 0,
//     IncludeUndefined = 1 << 0,
//     NoIndexSignatures = 1 << 1,
//     Writing = 1 << 2,
//     CacheSymbol = 1 << 3,
//     NoTupleBoundsCheck = 1 << 4,
//     ExpressionPosition = 1 << 5,
//     ReportDeprecated = 1 << 6,
//     SuppressNoImplicitAnyError = 1 << 7,
//     Contextual = 1 << 8,
//     Persistent = IncludeUndefined,
// }

// // Indexed access types (TypeFlags.IndexedAccess)
// // Possible forms are T[xxx], xxx[T], or xxx[keyof T], where T is a type variable
// export interface IndexedAccessType extends InstantiableType {
//     objectType: Type;
//     indexType: Type;
//     /* @internal */
//     accessFlags: AccessFlags;  // Only includes AccessFlags.Persistent
//     constraint?: Type;
//     simplifiedForReading?: Type;
//     simplifiedForWriting?: Type;
// }

// export type TypeVariable = TypeParameter | IndexedAccessType;

// /// keyof T types (TypeFlags.Index)
// pub struct IndexType {
//     // ty: InstantiableType | UnionOrIntersectionType,
//     ty: TypeId,
//     stringsOnly: bool,
// }

// export interface ConditionalRoot {
//     node: ConditionalTypeNode;
//     checkType: Type;
//     extendsType: Type;
//     isDistributive: boolean;
//     inferTypeParameters?: TypeParameter[];
//     outerTypeParameters?: TypeParameter[];
//     instantiations?: Map<Type>;
//     aliasSymbol?: Symbol;
//     aliasTypeArguments?: Type[];
// }

// // T extends U ? X : Y (TypeFlags.Conditional)
// export interface ConditionalType extends InstantiableType {
//     root: ConditionalRoot;
//     checkType: Type;
//     extendsType: Type;
//     resolvedTrueType?: Type;
//     resolvedFalseType?: Type;
//     /* @internal */
//     resolvedInferredTrueType?: Type; // The `trueType` instantiated with the `combinedMapper`, if present
//     /* @internal */
//     resolvedDefaultConstraint?: Type;
//     /* @internal */
//     mapper?: TypeMapper;
//     /* @internal */
//     combinedMapper?: TypeMapper;
// }

// export interface TemplateLiteralType extends InstantiableType {
//     texts: readonly string[];  // Always one element longer than types
//     types: readonly Type[];  // Always at least one element
// }

// export interface StringMappingType extends InstantiableType {
//     symbol: Symbol;
//     type: Type;
// }

// // Type parameter substitution (TypeFlags.Substitution)
// // Substitution types are created for type parameters or indexed access types that occur in the
// // true branch of a conditional type. For example, in 'T extends string ? Foo<T> : Bar<T>', the
// // reference to T in Foo<T> is resolved as a substitution type that substitutes 'string & T' for T.
// // Thus, if Foo has a 'string' constraint on its type parameter, T will satisfy it. Substitution
// // types disappear upon instantiation (just like type parameters).
// export interface SubstitutionType extends InstantiableType {
//     objectFlags: ObjectFlags;
//     baseType: Type;     // Target type
//     substitute: Type;   // Type to substitute for type parameter
// }

pub enum JsxReferenceKind {
    Component,
    Function,
    Mixed,
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum SignatureKind {
    Call,
    Construct,
}

bitflags! {
    #[derive(Default)]
    pub struct SignatureFlags: u8 {
        const None = 0;

        // Propagating flags
        const HasRestParameter = 1 << 0;          // Indicates last parameter is rest parameter
        const HasLiteralTypes = 1 << 1;           // Indicates signature is specialized
        const Abstract = 1 << 2;                  // Indicates signature comes from an abstract class, abstract construct signature, or abstract constructor type

        // Non-propagating flags
        const IsInnerCallChain = 1 << 3;          // Indicates signature comes from a CallChain nested in an outer OptionalChain
        const IsOuterCallChain = 1 << 4;          // Indicates signature comes from a CallChain that is the outermost chain of an optional expression
        const IsUntypedSignatureInJSFile = 1 << 5; // Indicates signature is from a js file and has no types

        // We do not propagate `IsInnerCallChain` or `IsOuterCallChain` to instantiated signatures, as that would result in us
        // attempting to add `| undefined` on each recursive call to `getReturnTypeOfSignature` when
        // instantiating the return type.
        const PropagatingFlags = Self::HasRestParameter.bits | Self::HasLiteralTypes.bits | Self::Abstract.bits | Self::IsUntypedSignatureInJSFile.bits;

        const CallChainFlags = Self::IsInnerCallChain.bits | Self::IsOuterCallChain.bits;
    }
}

index::newtype_index! {
    pub struct SignatureId {
        DEBUG_FORMAT = "SignatureId({})"
    }
}

#[derive(Default, Debug)]
pub struct OptionalCallSignatureCache {
    pub inner: Option<SignatureId>,
    pub outer: Option<SignatureId>,
}

#[derive(Default, Debug)]
pub struct Signature {
    pub flags: SignatureFlags,
    pub declaration: Option<BoundNode>,  // Originating declaration
    pub typeParameters: Rc<Vec<TypeId>>, // Type parameters (undefined if non-generic)
    pub parameters: Rc<Vec<SymbolId>>,   // Parameters
    pub thisParameter: Option<SymbolId>, // symbol of this-type parameter
    // See comment in `instantiateSignature` for why these are set lazily.
    pub resolvedReturnType: Option<TypeId>, // Lazily set by `getReturnTypeOfSignature`.
    // Lazily set by `getTypePredicateOfSignature`.
    // `undefined` indicates a type predicate that has not yet been computed.
    // Uses a special `noTypePredicate` sentinel value to indicate that there is no type predicate. This looks like a TypePredicate at runtime to avoid polymorphism.
    pub resolvedTypePredicate: Option<TypePredicate>,
    pub minArgumentCount: usize, // Number of non-optional parameters
    pub resolvedMinArgumentCount: Option<usize>, // Number of non-optional parameters (excluding trailing `void`)
    pub target: Option<SignatureId>,             // Instantiation target
    pub mapper: Option<Rc<TypeMapper>>,          // Instantiation mapper
    pub compositeSignatures: Rc<Vec<SignatureId>>, // Underlying signatures of a union/intersection signature
    pub compositeKind: Option<TypeFlags>, // TypeFlags.Union if the underlying signatures are from union members, otherwise TypeFlags.Intersection
    pub erasedSignatureCache: Option<SignatureId>, // Erased version of signature (deferred)
    pub canonicalSignatureCache: Option<SignatureId>, // Canonical version of signature (deferred)
    pub baseSignatureCache: Option<SignatureId>, // Base version of signature (deferred)
    pub optionalCallSignatureCache: OptionalCallSignatureCache, // Optional chained call version of signature (deferred)
    pub isolatedSignatureType: Option<TypeId>, // A manufactured type that just contains the signature for purposes of signature comparison
    pub instantiations: AHashMap<TypeList, SignatureId>, // Generic signature instantiation cache
}

pub enum IndexKind {
    String,
    Number,
}

#[derive(Debug)]
pub struct IndexInfo {
    pub keyType: TypeId,
    pub ty: TypeId,
    pub isReadonly: bool,
    pub declaration: Option<Rc<TsIndexSignature>>,
}

index::newtype_index! {
    pub struct IndexInfoId {
        DEBUG_FORMAT = "IndexInfoId({})"
    }
}

// pub enum TypeMapKind {
//     Simple,
//     Array,
//     Function,
//     Composite,
//     Merged,
// }

pub enum TypeMapper {
    Simple {
        source: TypeId,
        target: TypeId,
    },
    Array {
        sources: Rc<Vec<TypeId>>,
        targets: Option<Rc<Vec<TypeId>>>,
    },
    Function {
        // func: Box<dyn FnMut(&mut Checker, TypeId) -> TypeId + Debug>,
        func: Box<dyn Fn(&mut Checker, TypeId) -> TypeId>,
    },
    // CompositeOrMerged {
    //     mapper1: Rc<TypeMapper>,
    //     mapper2: Rc<TypeMapper>,
    // },
    Composite {
        mapper1: Rc<TypeMapper>,
        mapper2: Rc<TypeMapper>,
    },
    Merged {
        mapper1: Rc<TypeMapper>,
        mapper2: Rc<TypeMapper>,
    },
}

impl Debug for TypeMapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple { source, target } => f
                .debug_struct("Simple")
                .field("source", source)
                .field("target", target)
                .finish(),
            Self::Array { sources, targets } => f
                .debug_struct("Array")
                .field("sources", sources)
                .field("targets", targets)
                .finish(),
            Self::Function { .. } => f
                .debug_struct("Function")
                .field("func", &"[closure]")
                .finish(),
            Self::Composite { mapper1, mapper2 } => f
                .debug_struct("Composite")
                .field("mapper1", mapper1)
                .field("mapper2", mapper2)
                .finish(),
            Self::Merged { mapper1, mapper2 } => f
                .debug_struct("Merged")
                .field("mapper1", mapper1)
                .field("mapper2", mapper2)
                .finish(),
        }
    }
}

impl TypeMapper {
    pub fn makeUnaryTypeMapper(source: TypeId, target: TypeId) -> Rc<TypeMapper> {
        Rc::new(TypeMapper::Simple { source, target })
    }

    pub fn makeArrayTypeMapper(
        sources: Rc<Vec<TypeId>>,
        targets: Option<Rc<Vec<TypeId>>>,
    ) -> Rc<TypeMapper> {
        debug_assert!(targets.is_none() || targets.as_ref().unwrap().len() == sources.len());
        Rc::new(TypeMapper::Array { sources, targets })
    }

    pub fn makeFunctionTypeMapper(
        func: impl Fn(&mut Checker, TypeId) -> TypeId + 'static,
    ) -> Rc<TypeMapper> {
        Rc::new(TypeMapper::Function {
            func: Box::new(func),
        })
    }

    // pub fn makeCompositeTypeMapper(kind: TypeMapKind.Composite | TypeMapKind.Merged, mapper1: TypeMapper, mapper2: TypeMapper): TypeMapper {
    //     return { kind, mapper1, mapper2 };
    // }

    /**
     * Maps forward-references to later types parameters to the empty object type.
     * This is used during inference when instantiating type parameter defaults.
     */
    pub fn createBackreferenceMapper(context: InferenceContextId, index: usize) -> Rc<TypeMapper> {
        TypeMapper::makeFunctionTypeMapper(move |checker, t| {
            let pos = checker.inference_contexts[context]
                .inferences
                .iter()
                .position(|info| info.typeParameter == t);
            if pos.is_some() && pos.unwrap() >= index {
                checker.unknownType
            } else {
                t
            }
        })
    }

    pub fn combineTypeMappers(
        mapper1: Option<Rc<TypeMapper>>,
        mapper2: Rc<TypeMapper>,
    ) -> Rc<TypeMapper> {
        if let Some(mapper1) = mapper1 {
            Rc::new(TypeMapper::Composite { mapper1, mapper2 })
        } else {
            mapper2
        }
    }

    pub fn mergeTypeMappers(
        mapper1: Option<Rc<TypeMapper>>,
        mapper2: Rc<TypeMapper>,
    ) -> Rc<TypeMapper> {
        if let Some(mapper1) = mapper1 {
            Rc::new(TypeMapper::Merged { mapper1, mapper2 })
        } else {
            mapper2
        }
    }
}

// bitflags! {
//     pub struct InferencePriority {
//         NakedTypeVariable            = 1 << 0,  // Naked type variable in union or intersection type
//         SpeculativeTuple             = 1 << 1,  // Speculative tuple inference
//         SubstituteSource             = 1 << 2,  // Source of inference originated within a substitution type's substitute
//         HomomorphicMappedType        = 1 << 3,  // Reverse inference for homomorphic mapped type
//         PartialHomomorphicMappedType = 1 << 4,  // Partial reverse inference for homomorphic mapped type
//         MappedTypeConstraint         = 1 << 5,  // Reverse inference for mapped type
//         ContravariantConditional     = 1 << 6,  // Conditional type in contravariant position
//         ReturnType                   = 1 << 7,  // Inference made from return type of generic function
//         LiteralKeyof                 = 1 << 8,  // Inference made from a string literal to a keyof T
//         NoConstraints                = 1 << 9,  // Don't infer from constraints of instantiable types
//         AlwaysStrict                 = 1 << 10,  // Always use strict rules for contravariant inferences
//         MaxValue                     = 1 << 11, // Seed for inference priority tracking

//         PriorityImpliesCombination = ReturnType | MappedTypeConstraint | LiteralKeyof,  // These priorities imply that the resulting type should be a combination of all candidates
//         Circularity = -1,  // Inference circularity (value less than all other priorities)
//     }
// }

bitflags! {
    pub struct InferencePriority: u16 {
        const Circularity                  = 0;       // Inference circularity (value less than all other priorities)
        const Default                      = 1 << 0;
        const NakedTypeVariable            = 1 << 1;  // Naked type variable in union or intersection type
        const SpeculativeTuple             = 1 << 2;  // Speculative tuple inference
        const SubstituteSource             = 1 << 3;  // Source of inference originated within a substitution type's substitute
        const HomomorphicMappedType        = 1 << 4;  // Reverse inference for homomorphic mapped type
        const PartialHomomorphicMappedType = 1 << 5;  // Partial reverse inference for homomorphic mapped type
        const MappedTypeConstraint         = 1 << 6;  // Reverse inference for mapped type
        const ContravariantConditional     = 1 << 7;  // Conditional type in contravariant position
        const ReturnType                   = 1 << 8;  // Inference made from return type of generic function
        const LiteralKeyof                 = 1 << 9;  // Inference made from a string literal to a keyof T
        const NoConstraints                = 1 << 10; // Don't infer from constraints of instantiable types
        const AlwaysStrict                 = 1 << 11; // Always use strict rules for contravariant inferences
        const MaxValue                     = 1 << 12; // Seed for inference priority tracking

        // These priorities imply that the resulting type should be a combination of all candidates
        const PriorityImpliesCombination = Self::ReturnType.bits | Self::MappedTypeConstraint.bits | Self::LiteralKeyof.bits;
    }
}

impl Default for InferencePriority {
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Clone)]
pub struct InferenceInfo {
    pub typeParameter: TypeId, // Type parameter for which inferences are being made
    pub candidates: Option<Vec<TypeId>>, // Candidates in covariant positions (or undefined)
    pub contraCandidates: Option<Vec<TypeId>>, // Candidates in contravariant positions (or undefined)
    pub inferredType: Option<TypeId>,          // Cache for resolved inferred type
    pub priority: Option<InferencePriority>,   // Priority of current inference set
    pub topLevel: bool, // True if all inferences are to top level occurrences
    pub isFixed: bool,  // True if inferences are fixed
    pub impliedArity: Option<usize>,
}

impl InferenceInfo {
    pub fn new(typeParameter: TypeId) -> Self {
        Self {
            typeParameter,
            candidates: None,
            contraCandidates: None,
            inferredType: None,
            priority: None,
            topLevel: true,
            isFixed: false,
            impliedArity: None,
        }
    }
}

bitflags! {
    #[derive(Default)]
    pub struct InferenceFlags: u8 {
        const None            =      0;  // No special inference behaviors
        const NoDefault       = 1 << 0;  // Infer unknownType for no inferences (otherwise anyType or emptyObjectType)
        const AnyDefault      = 1 << 1;  // Infer anyType for no inferences (otherwise emptyObjectType)
        const SkippedGenericFunction = 1 << 2; // A generic function was skipped during inference
    }
}

/// Ternary values are defined such that
/// `x & y` picks the lesser in the order `False < Unknown < Maybe < True`, and
/// `x | y` picks the greater in the order `False < Unknown < Maybe < True`.
/// Generally, `Ternary::Maybe` is used as the result of a relation that depends on itself, and
/// `Ternary::Unknown` is used as the result of a variance check that depends on itself. We make
/// a distinction because we don't want to cache circular variance check results.
#[derive(PartialEq, Eq)]
pub enum Ternary {
    False = 0,
    Unknown = 1,
    Maybe = 3,
    True = -1,
}

// /* @internal */
// export type TypeComparer = (s: Type, t: Type, reportErrors?: boolean) => Ternary;

// pub enum TypeComparer {
//     CompareTypesAssignable,
//     IsRelatedToWorker,
//     CompareTypes,
// }

pub struct InferenceContext {
    pub inferences: Vec<InferenceInfo>, // Inferences made for each type parameter
    pub signature: Option<SignatureId>, // Generic signature for which inferences are made (if any)
    pub flags: InferenceFlags,          // Inference flags
    /// Type comparer function.
    /// `fn f(checker: &mut Checker, source: TypeId, target: TypeId, report_errors: bool) -> Ternary`
    pub compareTypes: fn(&mut Checker, TypeId, TypeId, bool) -> Ternary,
    pub mapper: Rc<TypeMapper>,          // Mapper that fixes inferences
    pub nonFixingMapper: Rc<TypeMapper>, // Mapper that doesn't fix inferences
    pub returnMapper: Option<Rc<TypeMapper>>, // Type mapper for inferences from return types (if any)
    pub inferredTypeParameters: Option<Rc<Vec<TypeId>>>, // Inferred type parameters for function result
}

index::newtype_index! {
    pub struct InferenceContextId {
        DEBUG_FORMAT = "InferenceContextId({})"
    }
}

pub struct WideningContext {
    pub parent: Option<Rc<WideningContext>>, // Parent context
    pub propertyName: Option<JsWord>,        // Name of property in parent
    pub siblings: Option<Vec<TypeId>>,       // Types of siblings
    pub resolvedProperties: Option<Vec<SymbolId>>, // Properties occurring in sibling object literals
}

#[derive(PartialEq, Eq)]
pub enum AssignmentDeclarationKind {
    None,
    /// exports.name = expr
    /// module.exports.name = expr
    ExportsProperty,
    /// module.exports = expr
    ModuleExports,
    /// className.prototype.name = expr
    PrototypeProperty,
    /// this.name = expr
    ThisProperty,
    // F.name = expr
    Property,
    // F.prototype = { ... }
    Prototype,
    // Object.defineProperty(x, 'name', { value: any, writable?: boolean (false by default) });
    // Object.defineProperty(x, 'name', { get: Function, set: Function });
    // Object.defineProperty(x, 'name', { get: Function });
    // Object.defineProperty(x, 'name', { set: Function });
    ObjectDefinePropertyValue,
    // Object.defineProperty(exports || module.exports, 'name', ...);
    ObjectDefinePropertyExports,
    // Object.defineProperty(Foo.prototype, 'name', ...);
    ObjectDefinePrototypeProperty,
}

bitflags! {
    #[derive(Default)]
    pub struct NodeFlags: u32 {
        const None               = 0;
         // Variable declaration
        const Let                = 1 << 0;
         // Variable declaration
        const Const              = 1 << 1;
         // Namespace declaration
        const NestedNamespace    = 1 << 2;
         // Node was synthesized during transformation
        const Synthesized        = 1 << 3;
         // Namespace declaration
        const Namespace          = 1 << 4;
         // Chained MemberExpression rooted to a pseudo-OptionalExpression
        const OptionalChain      = 1 << 5;
         // Export context (initialized by binding)
        const ExportContext      = 1 << 6;
         // Interface contains references to "this"
        const ContainsThis       = 1 << 7;
         // If function implicitly returns on one of codepaths (initialized by binding)
        const HasImplicitReturn  = 1 << 8;
         // If function has explicit reachable return on one of codepaths (initialized by binding)
        const HasExplicitReturn  = 1 << 9;
         // Set if module declaration is an augmentation for the global scope
        const GlobalAugmentation = 1 << 10;
        // If the file has async functions (initialized by binding)
        const HasAsyncFunctions  = 1 << 11;
        // If node was parsed in a context where 'in-expressions' are not allowed
        const DisallowInContext  = 1 << 12;
        // If node was parsed in the 'yield' context created when parsing a generator
        const YieldContext       = 1 << 13;
        // If node was parsed as part of a decorator
        const DecoratorContext   = 1 << 14;
        // If node was parsed in the 'await' context created when parsing an async function
        const AwaitContext       = 1 << 15;
        // If the parser encountered an error when parsing the code that created this node
        const ThisNodeHasError   = 1 << 16;
        // If node was parsed in a JavaScript
        const JavaScriptFile     = 1 << 17;
        // If this node or any of its children had an error
        const ThisNodeOrAnySubNodesHasError = 1 << 18;
        // If we've computed data from children and cached it in this node
        const HasAggregatedChildData = 1 << 19;

        // These flags will be set when the parser encounters a dynamic import expression or 'import.meta' to avoid
        // walking the tree if the flags are not set. However, these flags are just a approximation
        // (hence why it's named "PossiblyContainsDynamicImport") because once set, the flags never get cleared.
        // During editing, if a dynamic import is removed, incremental parsing will *NOT* clear this flag.
        // This means that the tree will always be traversed during module resolution, or when looking for external module indicators.
        // However, the removal operation should not occur often and in the case of the
        // removal, it is likely that users will add the import anyway.
        // The advantage of this approach is its simplicity. For the case of batch compilation,
        // we guarantee that users won't have to pay the price of walking the tree if a dynamic import isn't used.
        const PossiblyContainsDynamicImport = 1 << 20;
        const PossiblyContainsImportMeta    = 1 << 21;

        // If node was parsed inside jsdoc
        const JSDoc                                         = 1 << 22;
        // If node was inside an ambient context -- a declaration file, or inside something with the `declare` modifier.
        const Ambient                       = 1 << 23;
        // If any ancestor of node was the `statement` of a WithStatement (not the `expression`)
        const InWithStatement               = 1 << 24;
        // If node was parsed in a Json
        const JsonFile                                      = 1 << 25;
        // If a type was cached for node at any point
        const TypeCached                    = 1 << 26;
        // If has '@deprecated' JSDoc tag
        const Deprecated                    = 1 << 27;

        const BlockScoped = Self::Let.bits | Self::Const.bits;

        const ReachabilityCheckFlags = Self::HasImplicitReturn.bits | Self::HasExplicitReturn.bits;
        const ReachabilityAndEmitFlags = Self::ReachabilityCheckFlags.bits | Self::HasAsyncFunctions.bits;

        // Parsing context flags
        const ContextFlags = Self::DisallowInContext.bits | Self::YieldContext.bits | Self::DecoratorContext.bits | Self::AwaitContext.bits | Self::JavaScriptFile.bits | Self::InWithStatement.bits | Self::Ambient.bits;

        // Exclude these flags when parsing a Type
        const TypeExcludesFlags = Self::YieldContext.bits | Self::AwaitContext.bits;

        // Represents all flags that are potentially set once and
        // never cleared on SourceFiles which get re-used in between incremental parses.
        // See the comment above on `PossiblyContainsDynamicImport` and `PossiblyContainsImportMeta`.
        const PermanentlySetIncrementalFlags = Self::PossiblyContainsDynamicImport.bits | Self::PossiblyContainsImportMeta.bits;
    }
}

#[derive(Default, Debug)]
pub struct NodeData {
    pub flags: NodeFlags,
    // pub modifierFlagsCache: ModifierFlags;
    // transformFlags: TransformFlags; // Flags for transforms
    // decorators?: NodeArray<Decorator>;           // Array of decorators (in document order)
    // modifiers?: ModifiersArray;                  // Array of modifiers
    // pub id?: NodeId;                          // Unique id (used to look up NodeLinks)
    // parent: Node;                                // Parent node (initialized by binding)
    // pub original?: Node;                      // The original node if this is an updated node.
    pub symbol: Option<SymbolId>, // Symbol declared by node (initialized by binding)
    pub locals: Option<SymbolTableId>, // Locals associated with node (initialized by binding)
    pub nextContainer: Option<BoundNode>, // Next container in declaration order (initialized by binding)
    pub localSymbol: Option<SymbolId>, // Local symbol declared by node (initialized by binding only for exported nodes)
    pub flowNode: Option<FlowNodeId>,  // Associated FlowNode (initialized by binding)
    // pub emitNode?: EmitNode;                  // Associated EmitNode (initialized by transforms)
    pub contextualType: Option<TypeId>, // Used to temporarily assign a contextual type during overload resolution
    pub inferenceContext: Option<InferenceContextId>, // Inference context for contextual type

    // FunctionLikeDeclarationBase:
    pub endFlowNode: Option<FlowNodeId>,
    pub returnFlowNode: Option<FlowNodeId>,
}

// bitflags! {
//     pub struct ModifierFlags:u32 {
//         const None =               0;
//         const Export =             1 << 0;  // Declarations
//         const Ambient =            1 << 1;  // Declarations
//         const Public =             1 << 2;  // Property/Method
//         const Private =            1 << 3;  // Property/Method
//         const Protected =          1 << 4;  // Property/Method
//         const Static =             1 << 5;  // Property/Method
//         const Readonly =           1 << 6;  // Property/Method
//         const Abstract =           1 << 7;  // Class/Method/ConstructSignature
//         const Async =              1 << 8;  // Property/Method/Function
//         const Default =            1 << 9;  // Function/Class (export default declaration)
//         const Const =              1 << 11; // Const enum
//         const HasComputedJSDocModifiers = 1 << 12; // Indicates the computed modifier flags include modifiers from JSDoc.

//         const Deprecated =         1 << 13; // Deprecated tag.
//         const Override =           1 << 14; // Override method.
//         const HasComputedFlags =   1 << 29; // Modifier flags have been computed

//         const AccessibilityModifier = Self::Public.bits | Self::Private.bits | Self::Protected.bits;
//         // Accessibility modifiers and 'readonly' can be attached to a parameter in a constructor to make it a property.
//         const ParameterPropertyModifier = Self::AccessibilityModifier.bits | Self::Readonly.bits | Self::Override.bits;
//         const NonPublicAccessibilityModifier = Self::Private.bits | Self::Protected.bits;

//         const TypeScriptModifier = Self::Ambient.bits | Self::Public.bits | Self::Private.bits | Self::Protected.bits | Self::Readonly.bits | Self::Abstract.bits | Self::Const.bits | Self::Override.bits;
//         const ExportDefault = Self::Export.bits | Self::Default.bits;
//         const All = Self::Export.bits | Self::Ambient.bits | Self::Public.bits | Self::Private.bits | Self::Protected.bits | Self::Static.bits | Self::Readonly.bits | Self::Abstract.bits | Self::Async.bits | Self::Default.bits | Self::Const.bits | Self::Deprecated.bits | Self::Override.bits;
//     }
// }

bitflags! {
    pub struct RelationComparisonResult: u8 {
        const Succeeded           = 1 << 0; // Should be truthy
        const Failed              = 1 << 1;
        const Reported            = 1 << 2;

        const ReportsUnmeasurable = 1 << 3;
        const ReportsUnreliable   = 1 << 4;
        const ReportsMask         = Self::ReportsUnmeasurable.bits | Self::ReportsUnreliable.bits;
    }
}

bitflags! {
    // NOTE: Ensure this is up-to-date with src/debug/debug.ts
    pub struct FlowFlags: u16 {
        const Unreachable    = 1 << 0;  // Unreachable code
        const Start          = 1 << 1;  // Start of flow graph
        const BranchLabel    = 1 << 2;  // Non-looping junction
        const LoopLabel      = 1 << 3;  // Looping junction
        const Assignment     = 1 << 4;  // Assignment
        const TrueCondition  = 1 << 5;  // Condition known to be true
        const FalseCondition = 1 << 6;  // Condition known to be false
        const SwitchClause   = 1 << 7;  // Switch statement clause
        const ArrayMutation  = 1 << 8;  // Potential array mutation
        const Call           = 1 << 9;  // Potential assertion call
        const ReduceLabel    = 1 << 10; // Temporarily reduce antecedents of label
        const Referenced     = 1 << 11; // Referenced as antecedent once
        const Shared         = 1 << 12; // Referenced as antecedent more than once

        const Label = Self::BranchLabel.bits | Self::LoopLabel.bits;
        const Condition = Self::TrueCondition.bits | Self::FalseCondition.bits;
    }
}

index::newtype_index! {
    pub struct FlowNodeId {
        DEBUG_FORMAT = "FlowNodeId({})"
    }
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub enum FlowNodeKind {
    None,
    FlowStart(FlowStart),
    FlowLabel(FlowLabel),
    FlowAssignment(FlowAssignment),
    FlowCall(FlowCall),
    FlowCondition(FlowCondition),
    FlowSwitchClause(FlowSwitchClause),
    FlowArrayMutation(FlowArrayMutation),
    FlowReduceLabel(FlowReduceLabel),
}

impl FlowNodeKind {
    pub fn unwrap_flow_label(&self) -> &FlowLabel {
        match self {
            FlowNodeKind::FlowLabel(label) => &label,
            _ => unreachable!(),
        }
    }

    pub fn unwrap_flow_label_mut(&mut self) -> &mut FlowLabel {
        match self {
            FlowNodeKind::FlowLabel(label) => label,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct FlowNode {
    pub flags: FlowFlags,
    // TODO: Node id used by flow type cache in checker
    pub id: Option<u32>,
    pub kind: FlowNodeKind,
}

impl FlowNode {
    pub fn new_branch_label() -> Self {
        Self {
            flags: FlowFlags::BranchLabel,
            id: None,
            kind: FlowNodeKind::FlowLabel(FlowLabel {
                antecedents: Default::default(),
            }),
        }
    }

    pub fn new_branch_label_with_antecedents(antecedents: Vec<FlowNodeId>) -> Self {
        Self {
            flags: FlowFlags::BranchLabel,
            id: None,
            kind: FlowNodeKind::FlowLabel(FlowLabel {
                antecedents: Rc::new(antecedents),
            }),
        }
    }

    pub fn new_loop_label() -> Self {
        Self {
            flags: FlowFlags::LoopLabel,
            id: None,
            kind: FlowNodeKind::FlowLabel(FlowLabel {
                antecedents: Default::default(),
            }),
        }
    }

    pub fn new_reduce_label(
        target: FlowNodeId,
        antecedents: Rc<Vec<FlowNodeId>>,
        antecedent: FlowNodeId,
    ) -> Self {
        Self {
            flags: FlowFlags::ReduceLabel,
            id: None,
            kind: FlowNodeKind::FlowReduceLabel(FlowReduceLabel {
                target,
                antecedents: antecedents,
                antecedent,
            }),
        }
    }
}

// #[derive(Clone, Hash, PartialEq, Eq, Debug)]
// pub enum FlowStartNode {
//     FunctionExpr(Rc<FnExpr>),
//     ArrowExpr(Rc<ArrowExpr>),
//     ClassMethod(Rc<ClassMethod>),
//     PrivateMethod(Rc<PrivateMethod>),
//     MethodProp(Rc<MethodProp>),
//     GetterProp(Rc<GetterProp>),
//     SetterProp(Rc<SetterProp>),
// }

// FlowStart represents the start of a control flow. For a function expression or arrow
// function, the node property references the function (which in turn has a flowNode
// property for the containing control flow).
#[derive(Hash, PartialEq, Eq, Debug)]
pub struct FlowStart {
    // pub node: Option<FlowStartNode>,
    pub node: Option<BoundNode>,
}

// FlowLabel represents a junction with multiple possible preceding control flows.
#[derive(Hash, PartialEq, Eq, Debug)]
pub struct FlowLabel {
    pub antecedents: Rc<Vec<FlowNodeId>>,
}

//todo:
// #[derive(Clone, Hash, PartialEq, Eq, Debug)]
// pub enum FlowAssignmentNode {
//     Expression(ast::Expr),
//     VariableDeclaration(Rc<VarDecl>),
//     BindingElement(ast::Pat),
// }

// FlowAssignment represents a node that assigns a value to a narrowable reference,
// i.e. an identifier or a dotted name that starts with an identifier or 'this'.
#[derive(Hash, PartialEq, Eq, Debug)]
pub struct FlowAssignment {
    // TODO:
    // pub node: FlowAssignmentNode,
    pub node: BoundNode,
    pub antecedent: FlowNodeId,
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub struct FlowCall {
    pub node: Rc<CallExpr>,
    pub antecedent: FlowNodeId,
}

// FlowCondition represents a condition that is known to be true or false at the
// node's location in the control flow.
#[derive(Hash, PartialEq, Eq, Debug)]
pub struct FlowCondition {
    pub node: ast::Expr,
    pub antecedent: FlowNodeId,
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub struct FlowSwitchClause {
    pub switchStatement: Rc<SwitchStmt>,
    pub clauseStart: u32, // Start index of case/default clause range
    pub clauseEnd: u32,   // End index of case/default clause range
    pub antecedent: FlowNodeId,
}

// TODO:
// #[derive(Clone, Hash, PartialEq, Eq, Debug)]
// pub enum FlowArrayMutationNode {
//     CallExpression(Rc<CallExpr>),
//     BinaryExpression(Rc<BinExpr>),
// }

// FlowArrayMutation represents a node potentially mutates an array, i.e. an
// operation of the form 'x.push(value)', 'x.unshift(value)' or 'x[n] = value'.
#[derive(Hash, PartialEq, Eq, Debug)]
pub struct FlowArrayMutation {
    // TODO:
    // pub node: FlowArrayMutationNode,
    pub node: BoundNode,
    pub antecedent: FlowNodeId,
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub struct FlowReduceLabel {
    pub target: FlowNodeId,
    pub antecedents: Rc<Vec<FlowNodeId>>,
    pub antecedent: FlowNodeId,
}

pub enum FlowType {
    Type(TypeId),
    IncompleteType(IncompleteType),
}

// Incomplete types occur during control flow analysis of loops. An IncompleteType
// is distinguished from a regular type by a flags value of zero. Incomplete type
// objects are internal to the getFlowTypeOfReference function and never escape it.
pub struct IncompleteType {
    // flags: TypeFlags,  // No flags set
    pub ty: TypeId, // The type marked incomplete
}

// TODO:
pub enum Declaration {
    ArrowExpr(Rc<ArrowExpr>),
    AssignProp(Rc<AssignProp>),
    // BindingElement(Rc<BindingElement>),
    ClassDecl(Rc<ClassDecl>),
    ClassExpr(Rc<ClassExpr>),
    ClassMethod(Rc<ClassMethod>),
    ClassProp(Rc<ClassProp>),
    // ClassStaticBlockDeclaration(Rc<ClassStaticBlockDeclaration>),
    Constructor(Rc<Constructor>),
    TsEnumDecl(Rc<TsEnumDecl>),
    TsEnumMember(Rc<TsEnumMember>),
    ExportNamespaceSpecifier(Rc<ExportNamespaceSpecifier>),
    // ExportSpecifier(Rc<ExportSpecifier>),
    FnDecl(Rc<FnDecl>),
    FnExpr(Rc<FnExpr>),
    GetterProp(Rc<GetterProp>),
    // ImportClause(Rc<ImportClause>),
    // ImportEqualsDeclaration(Rc<ImportEqualsDeclaration>),
    // ImportSpecifier(Rc<ImportSpecifier>),
    ImportStarAsSpecifier(Rc<ImportStarAsSpecifier>),
    TsInterfaceDecl(Rc<TsInterfaceDecl>),
    // JsxAttribute(Rc<JsxAttribute>),
    KeyValueProp(Rc<KeyValueProp>),
    TsMethodSignature(Rc<TsMethodSignature>),
    MethodProp(Rc<MethodProp>),
    // ModuleDeclaration(Rc<ModuleDeclaration>),
    // NamespaceExportDeclaration(Rc<NamespaceExportDeclaration>),
    Param(Rc<Param>),
    ParamWithoutDecorators(Rc<ParamWithoutDecorators>),
    TsAmbientParam(Rc<TsAmbientParam>),
    TsParamProp(Rc<TsParamProp>),
    PrivateMethod(Rc<PrivateMethod>),
    PrivateProp(Rc<PrivateProp>),
    TsPropertySignature(Rc<TsPropertySignature>),
    SetterProp(Rc<SetterProp>),
    // ShorthandPropertyAssignment(Rc<ShorthandPropertyAssignment>),
    TsTypeAliasDecl(Rc<TsTypeAliasDecl>),
    TsTypeParam(Rc<TsTypeParam>),
    VarDeclarator(Rc<VarDeclarator>),
    // JSDocTypedefTag(Rc<JSDocTypedefTag>),
    // JSDocCallbackTag(Rc<JSDocCallbackTag>),
    // JSDocPropertyTag(Rc<JSDocPropertyTag>),
}

impl TryFrom<BoundNode> for Declaration {
    type Error = ();

    fn try_from(node: BoundNode) -> Result<Self, Self::Error> {
        match node {
            BoundNode::ArrowExpr(n) => Ok(Declaration::ArrowExpr(n)),
            BoundNode::AssignProp(n) => Ok(Declaration::AssignProp(n)),
            BoundNode::ClassDecl(n) => Ok(Declaration::ClassDecl(n)),
            BoundNode::ClassExpr(n) => Ok(Declaration::ClassExpr(n)),
            BoundNode::ClassMethod(n) => Ok(Declaration::ClassMethod(n)),
            BoundNode::ClassProp(n) => Ok(Declaration::ClassProp(n)),
            BoundNode::Constructor(n) => Ok(Declaration::Constructor(n)),
            BoundNode::TsEnumDecl(n) => Ok(Declaration::TsEnumDecl(n)),
            BoundNode::TsEnumMember(n) => Ok(Declaration::TsEnumMember(n)),
            BoundNode::ExportNamespaceSpecifier(n) => Ok(Declaration::ExportNamespaceSpecifier(n)),
            BoundNode::FnDecl(n) => Ok(Declaration::FnDecl(n)),
            BoundNode::FnExpr(n) => Ok(Declaration::FnExpr(n)),
            BoundNode::GetterProp(n) => Ok(Declaration::GetterProp(n)),
            BoundNode::ImportStarAsSpecifier(n) => Ok(Declaration::ImportStarAsSpecifier(n)),
            BoundNode::TsInterfaceDecl(n) => Ok(Declaration::TsInterfaceDecl(n)),
            BoundNode::KeyValueProp(n) => Ok(Declaration::KeyValueProp(n)),
            BoundNode::TsMethodSignature(n) => Ok(Declaration::TsMethodSignature(n)),
            BoundNode::MethodProp(n) => Ok(Declaration::MethodProp(n)),
            BoundNode::Param(n) => Ok(Declaration::Param(n)),
            BoundNode::ParamWithoutDecorators(n) => Ok(Declaration::ParamWithoutDecorators(n)),
            BoundNode::TsAmbientParam(n) => Ok(Declaration::TsAmbientParam(n)),
            BoundNode::TsParamProp(n) => Ok(Declaration::TsParamProp(n)),
            BoundNode::PrivateMethod(n) => Ok(Declaration::PrivateMethod(n)),
            BoundNode::PrivateProp(n) => Ok(Declaration::PrivateProp(n)),
            BoundNode::TsPropertySignature(n) => Ok(Declaration::TsPropertySignature(n)),
            BoundNode::SetterProp(n) => Ok(Declaration::SetterProp(n)),
            BoundNode::TsTypeAliasDecl(n) => Ok(Declaration::TsTypeAliasDecl(n)),
            BoundNode::TsTypeParam(n) => Ok(Declaration::TsTypeParam(n)),
            BoundNode::VarDeclarator(n) => Ok(Declaration::VarDeclarator(n)),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ScriptTarget {
    ES3,
    ES5,
    ES2015,
    ES2016,
    ES2017,
    ES2018,
    ES2019,
    ES2020,
    ES2021,
    ESNext,
}

bitflags! {
    // NOTE: If modifying this enum, must modify `TypeFormatFlags` too!
    pub struct NodeBuilderFlags: u32 {
        const None                                    = 0;
        // Options
        const NoTruncation                            = 1 << 0;   // Don't truncate result
        const WriteArrayAsGenericType                 = 1 << 1;   // Write Array<T> instead T[]
        const GenerateNamesForShadowedTypeParams      = 1 << 2;   // When a type parameter T is shadowing another T, generate a name for it so it can still be referenced
        const UseStructuralFallback                   = 1 << 3;   // When an alias cannot be named by its symbol, rather than report an error, fallback to a structural printout if possible
        const ForbidIndexedAccessSymbolReferences     = 1 << 4;   // Forbid references like `I["a"]["b"]` - print `typeof I.a<x>.b<y>` instead
        const WriteTypeArgumentsOfSignature           = 1 << 5;   // Write the type arguments instead of type parameters of the signature
        const UseFullyQualifiedType                   = 1 << 6;   // Write out the fully qualified type name (eg. Module.Type, instead of Type)
        const UseOnlyExternalAliasing                 = 1 << 7;   // Only use external aliases for a symbol
        const SuppressAnyReturnType                   = 1 << 8;   // If the return type is any-like and can be elided, don't offer a return type.
        const WriteTypeParametersInQualifiedName      = 1 << 9;
        const MultilineObjectLiterals                 = 1 << 10;  // Always write object literals across multiple lines
        const WriteClassExpressionAsTypeLiteral       = 1 << 11;  // Write class {} as { new(): {} } - used for mixin declaration emit
        const UseTypeOfFunction                       = 1 << 12;  // Build using typeof instead of function type literal
        const OmitParameterModifiers                  = 1 << 13;  // Omit modifiers on parameters
        const UseAliasDefinedOutsideCurrentScope      = 1 << 14;  // Allow non-visible aliases
        const UseSingleQuotesForStringLiteralType     = 1 << 28;  // Use single quotes for string literal type
        const NoTypeReduction                         = 1 << 29;  // Don't call getReducedType
        const NoUndefinedOptionalParameterType        = 1 << 30;  // Do not add undefined to optional parameter type

        // Error handling
        const AllowThisInObjectLiteral                = 1 << 15;
        const AllowQualifiedNameInPlaceOfIdentifier    = 1 << 16;
        /** @deprecated AllowQualifedNameInPlaceOfIdentifier. Use AllowQualifiedNameInPlaceOfIdentifier instead. */
        const AllowQualifedNameInPlaceOfIdentifier    = Self::AllowQualifiedNameInPlaceOfIdentifier.bits;
        const AllowAnonymousIdentifier                = 1 << 17;
        const AllowEmptyUnionOrIntersection           = 1 << 18;
        const AllowEmptyTuple                         = 1 << 19;
        const AllowUniqueESSymbolType                 = 1 << 20;
        const AllowEmptyIndexInfoType                 = 1 << 21;

        // Errors (cont.)
        const AllowNodeModulesRelativePaths           = 1 << 26;
        const DoNotIncludeSymbolChain = 1 << 27;    // Skip looking up and printing an accessible symbol chain

        const IgnoreErrors = Self::AllowThisInObjectLiteral.bits | Self::AllowQualifiedNameInPlaceOfIdentifier.bits | Self::AllowAnonymousIdentifier.bits | Self::AllowEmptyUnionOrIntersection.bits | Self::AllowEmptyTuple.bits | Self::AllowEmptyIndexInfoType.bits | Self::AllowNodeModulesRelativePaths.bits;

        // State
        const InObjectTypeLiteral                     = 1 << 22;
        const InTypeAlias                             = 1 << 23;    // Writing type in type alias declaration
        const InInitialEntityName                     = 1 << 24;    // Set when writing the LHS of an entity name or entity name expression
    }
}

bitflags! {
    pub struct SymbolFormatFlags: u8 {
        const None = 0;

        // Write symbols's type argument if it is instantiated symbol
        // eg. class C<T> { p: T }   <-- Show p as C<T>.p here
        //     var a: C<number>;
        //     var p = a.p; <--- Here p is property of C<number> so show it as C<number>.p instead of just C.p
        const WriteTypeParametersOrArguments = 1 << 0;

        // Use only external alias information to get the symbol name in the given context
        // eg.  module m { export class c { } } import x = m.c;
        // When this flag is specified m.c will be used to refer to the class instead of alias symbol x
        const UseOnlyExternalAliasing = 1 << 1;

        // Build symbol name using any nodes needed, instead of just components of an entity name
        const AllowAnyNodeKind = 1 << 2;

        // Prefer aliases which are not directly visible
        const UseAliasDefinedOutsideCurrentScope = 1 << 3;

        // Skip building an accessible symbol chain
        const DoNotIncludeSymbolChain = 1 << 4;
    }
}
