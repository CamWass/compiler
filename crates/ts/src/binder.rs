use std::rc::Rc;

use bitflags::bitflags;
use index::vec::IndexVec;

use crate::core::*;
use crate::factory::nodeTests::*;
use crate::node::*;
use crate::our_types::*;
use crate::our_utils::unwrap_as;
use crate::parser::{forEachChild, isExternalModule};
use crate::scanner::tokenToString;
use crate::types::*;
use crate::utilities::*;
use crate::utilitiesPublic::*;

// export const enum ModuleInstanceState {
//     NonInstantiated = 0,
//     Instantiated = 1,
//     ConstEnumOnly = 2
// }

#[derive(Clone)]
struct ActiveLabel {
    name: __String,
    breakTarget: FlowLabelId,
    continueTarget: Option<FlowLabelId>,
    referenced: bool,
}

// export function getModuleInstanceState(node: ModuleDeclaration, visited?: ESMap<number, ModuleInstanceState | undefined>): ModuleInstanceState {
//     if (node.body && !node.body.parent) {
//         // getModuleInstanceStateForAliasTarget needs to walk up the parent chain, so parent pointers must be set on this tree already
//         setParent(node.body, node);
//         setParentRecursive(node.body, /*incremental*/ false);
//     }
//     return node.body ? getModuleInstanceStateCached(node.body, visited) : ModuleInstanceState.Instantiated;
// }

// function getModuleInstanceStateCached(node: Node, visited = new Map<number, ModuleInstanceState | undefined>()) {
//     const nodeId = getNodeId(node);
//     if (visited.has(nodeId)) {
//         return visited.get(nodeId) || ModuleInstanceState.NonInstantiated;
//     }
//     visited.set(nodeId, undefined);
//     const result = getModuleInstanceStateWorker(node, visited);
//     visited.set(nodeId, result);
//     return result;
// }

// function getModuleInstanceStateWorker(node: Node, visited: ESMap<number, ModuleInstanceState | undefined>): ModuleInstanceState {
//     // A module is uninstantiated if it contains only
//     switch (node.kind) {
//         // 1. interface declarations, type alias declarations
//         case SyntaxKind::InterfaceDeclaration:
//         case SyntaxKind::TypeAliasDeclaration:
//             return ModuleInstanceState.NonInstantiated;
//         // 2. const enum declarations
//         case SyntaxKind::EnumDeclaration:
//             if (isEnumConst(node as EnumDeclaration)) {
//                 return ModuleInstanceState.ConstEnumOnly;
//             }
//             break;
//         // 3. non-exported import declarations
//         case SyntaxKind::ImportDeclaration:
//         case SyntaxKind::ImportEqualsDeclaration:
//             if (!(hasSyntacticModifier(node, ModifierFlags.Export))) {
//                 return ModuleInstanceState.NonInstantiated;
//             }
//             break;
//         // 4. Export alias declarations pointing at only uninstantiated modules or things uninstantiated modules contain
//         case SyntaxKind::ExportDeclaration:
//             const exportDeclaration = node as ExportDeclaration;
//             if (!exportDeclaration.moduleSpecifier && exportDeclaration.exportClause && exportDeclaration.exportClause.kind === SyntaxKind::NamedExports) {
//                 let state = ModuleInstanceState.NonInstantiated;
//                 for (const specifier of exportDeclaration.exportClause.elements) {
//                     const specifierState = getModuleInstanceStateForAliasTarget(specifier, visited);
//                     if (specifierState > state) {
//                         state = specifierState;
//                     }
//                     if (state === ModuleInstanceState.Instantiated) {
//                         return state;
//                     }
//                 }
//                 return state;
//             }
//             break;
//         // 5. other uninstantiated module declarations.
//         case SyntaxKind::ModuleBlock: {
//             let state = ModuleInstanceState.NonInstantiated;
//             forEachChild(node, n => {
//                 const childState = getModuleInstanceStateCached(n, visited);
//                 switch (childState) {
//                     case ModuleInstanceState.NonInstantiated:
//                         // child is non-instantiated - continue searching
//                         return;
//                     case ModuleInstanceState.ConstEnumOnly:
//                         // child is const enum only - record state and continue searching
//                         state = ModuleInstanceState.ConstEnumOnly;
//                         return;
//                     case ModuleInstanceState.Instantiated:
//                         // child is instantiated - record state and stop
//                         state = ModuleInstanceState.Instantiated;
//                         return true;
//                     default:
//                         Debug.assertNever(childState);
//                 }
//             });
//             return state;
//         }
//         case SyntaxKind::ModuleDeclaration:
//             return getModuleInstanceState(node as ModuleDeclaration, visited);
//         case SyntaxKind::Identifier:
//             // Only jsdoc typedef definition can exist in jsdoc namespace, and it should
//             // be considered the same as type alias
//             if ((node as Identifier).isInJSDocNamespace) {
//                 return ModuleInstanceState.NonInstantiated;
//             }
//     }
//     return ModuleInstanceState.Instantiated;
// }

// function getModuleInstanceStateForAliasTarget(specifier: ExportSpecifier, visited: ESMap<number, ModuleInstanceState | undefined>) {
//     const name = specifier.propertyName || specifier.name;
//     let p: Node | undefined = specifier.parent;
//     while (p) {
//         if (isBlock(p) || isModuleBlock(p) || isSourceFile(p)) {
//             const statements = p.statements;
//             let found: ModuleInstanceState | undefined;
//             for (const statement of statements) {
//                 if (nodeHasName(statement, name)) {
//                     if (!statement.parent) {
//                         setParent(statement, p);
//                         setParentRecursive(statement, /*incremental*/ false);
//                     }
//                     const state = getModuleInstanceStateCached(statement, visited);
//                     if (found === undefined || state > found) {
//                         found = state;
//                     }
//                     if (found === ModuleInstanceState.Instantiated) {
//                         return found;
//                     }
//                 }
//             }
//             if (found !== undefined) {
//                 return found;
//             }
//         }
//         p = p.parent;
//     }
//     return ModuleInstanceState.Instantiated; // Couldn't locate, assume could refer to a value
// }

bitflags! {
    struct ContainerFlags: u8 {
        // The current node is not a container, and no container manipulation should happen before
        // recursing into it.
        const None = 0;

        // The current node is a container.  It should be set as the current container (and block-
        // container) before recursing into it.  The current node does not have locals.  Examples:
        //
        //      Classes, ObjectLiterals, TypeLiterals, Interfaces...
        const IsContainer = 1 << 0;

        // The current node is a block-scoped-container.  It should be set as the current block-
        // container before recursing into it.  Examples:
        //
        //      Blocks (when not parented by functions), Catch clauses, For/For-in/For-of statements...
        const IsBlockScopedContainer = 1 << 1;

        // The current node is the container of a control flow path. The current control flow should
        // be saved and restored, and a new control flow initialized within the container.
        const IsControlFlowContainer = 1 << 2;

        const IsFunctionLike = 1 << 3;
        const IsFunctionExpression = 1 << 4;
        const HasLocals = 1 << 5;
        const IsInterface = 1 << 6;
        const IsObjectLiteralOrClassExpressionMethodOrAccessor = 1 << 7;
    }
}

// function initFlowNode<T extends FlowNode>(node: T) {
//     Debug.attachFlowNodeDebugInfo(node);
//     return node;
// }

pub struct Binder<'a> {
    file: Rc<SourceFile>,
    options: CompilerOptions,
    languageVersion: ScriptTarget,
    parent: Option<Rc<BoundNode>>,
    container: Option<Rc<BoundNode>>,
    thisParentContainer: Option<Rc<BoundNode>>, // Container one level up
    blockScopeContainer: Option<Rc<BoundNode>>,
    lastContainer: Option<Rc<BoundNode>>,
    // // delayedTypeAliases: (JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag)[],
    seenThisKeyword: bool,

    // state used by control flow analysis
    currentFlow: FlowNodeId,
    currentBreakTarget: Option<FlowLabelId>,
    currentContinueTarget: Option<FlowLabelId>,
    currentReturnTarget: Option<FlowLabelId>,
    currentTrueTarget: Option<FlowLabelId>,
    currentFalseTarget: Option<FlowLabelId>,
    currentExceptionTarget: Option<FlowLabelId>,
    preSwitchCaseFlow: Option<FlowNodeId>,
    activeLabelList: Vec<ActiveLabel>,
    hasExplicitReturn: bool,

    // state used for emit helpers
    emitFlags: NodeFlags,

    // If this file is an external module, then it is automatically in strict-mode according to
    // ES6.  If it is not an external module, then we'll determine if it is in strict mode or
    // not depending on if we see "use strict" in certain places or if we hit a class/namespace
    // or if compiler options contain alwaysStrict.
    inStrictMode: bool,
    // If we are binding an assignment pattern, we will bind certain expressions differently.
    inAssignmentPattern: bool,

    //     let symbolCount = 0;

    //     let Symbol: new (flags: SymbolFlags, name: __String) => Symbol;
    //     let classifiableNames: Set<__String>;

    //     const unreachableFlow: FlowNode = { flags: FlowFlags.Unreachable };
    //     const reportedUnreachableFlow: FlowNode = { flags: FlowFlags.Unreachable };
    //     const bindBinaryExpressionFlow = createBindBinaryExpressionFlow();
    store: &'a mut NodeDataStore,
    flow_nodes: IndexVec<FlowNodeId, FlowNode>,
    symbols: IndexVec<SymbolId, Symbol>,
    symbol_tables: IndexVec<SymbolTableId, SymbolTable>,
}

impl<'a> Binder<'a> {
    fn new(
        options: CompilerOptions,
        intitial_file: Rc<SourceFile>,
        store: &'a mut NodeDataStore,
    ) -> Binder {
        Binder {
            file: intitial_file,
            options,
            // TODO:
            // languageVersion: getEmitScriptTarget(options),
            languageVersion: ScriptTarget::ESNext,
            parent: None,
            container: None,
            thisParentContainer: None,
            blockScopeContainer: None,
            lastContainer: None,
            seenThisKeyword: false,

            // TODO: tsc does not use unreachableFlow here, they just don't
            // initialize currentFlow until later - somthing we can't really do.
            // But tsc (almost) alway checks if currentFlow is undefined, but we dont.
            // If we replicte TSC's behavior, we can use an option for currentFlow,
            // and initialise it to None.
            currentFlow: FlowNodeId::MAX,
            currentBreakTarget: None,
            currentContinueTarget: None,
            currentReturnTarget: None,
            currentTrueTarget: None,
            currentFalseTarget: None,
            currentExceptionTarget: None,
            preSwitchCaseFlow: None,
            activeLabelList: Vec::new(),
            hasExplicitReturn: false,

            emitFlags: NodeFlags::None,

            inStrictMode: false,
            inAssignmentPattern: false,
            store,
            flow_nodes: IndexVec::default(),
            symbols: IndexVec::default(),
            symbol_tables: IndexVec::default(),
        }
    }

    pub fn bind_source_files(
        files: &[Rc<SourceFile>],
        options: CompilerOptions,
        store: &'a mut NodeDataStore,
    ) {
        if files.is_empty() {
            return;
        }

        let mut binder = Binder::new(options, files[0].clone(), store);

        for file in files {
            binder.bindSourceFile(file.clone());
        }
    }

    fn createSymbolTable(&mut self) -> SymbolTableId {
        self.symbol_tables.push(SymbolTable::default())
    }

    fn node_data<T: HasNodeId>(&mut self, node: T) -> &NodeData {
        self.store.node_data(node)
    }

    fn node_data_mut<T: HasNodeId>(&mut self, node: T) -> &mut NodeData {
        self.store.node_data_mut(node)
    }

    fn alloc_flow_node(&mut self, node: FlowNode) -> FlowNodeId {
        self.flow_nodes.push(node)
    }
}

impl<'a> Binder<'a> {
    //     /**
    //      * Inside the binder, we may create a diagnostic for an as-yet unbound node (with potentially no parent pointers, implying no accessible source file)
    //      * If so, the node _must_ be in the current file (as that's the only way anything could have traversed to it to yield it as the error node)
    //      * This version of `createDiagnosticForNode` uses the binder's context to account for this, and always yields correct diagnostics even in these situations.
    //      */
    //     function createDiagnosticForNode(node: Node, message: DiagnosticMessage, arg0?: string | number, arg1?: string | number, arg2?: string | number): DiagnosticWithLocation {
    //         return createDiagnosticForNodeInSourceFile(getSourceFileOfNode(node) || file, node, message, arg0, arg1, arg2);
    //     }

    fn bindSourceFile(&mut self, f: Rc<SourceFile>) {
        self.file = f;
        // self.languageVersion = getEmitScriptTarget(options);
        self.inStrictMode = Self::bindInStrictMode(&self.file, &self.options);
        // self.classifiableNames = new Set();
        // self.symbolCount = 0;

        // Symbol = objectAllocator.getSymbolConstructor();

        // // Attach debugging information if necessary
        // Debug.attachFlowNodeDebugInfo(unreachableFlow);
        // Debug.attachFlowNodeDebugInfo(reportedUnreachableFlow);

        // TODO:
        // if !self.file.locals {
        self.bind(Some(self.file.bind_to_opt_parent(None)));
        // self.file.symbolCount = symbolCount;
        // self.file.classifiableNames = classifiableNames;
        // delayedBindJSDocTypedefTag();
        // }

        // TODO:
        // file = undefined!;
        // options = undefined!;
        // languageVersion = undefined!;
        // parent = undefined!;
        // container = undefined!;
        // thisParentContainer = undefined!;
        // blockScopeContainer = undefined!;
        // lastContainer = undefined!;
        // delayedTypeAliases = undefined!;
        // seenThisKeyword = false;
        // currentFlow = undefined!;
        // currentBreakTarget = undefined;
        // currentContinueTarget = undefined;
        // currentReturnTarget = undefined;
        // currentTrueTarget = undefined;
        // currentFalseTarget = undefined;
        // currentExceptionTarget = undefined;
        // activeLabelList = undefined;
        // hasExplicitReturn = false;
        // inAssignmentPattern = false;
        // emitFlags = NodeFlags::None;b
    }

    //     return bindSourceFile;

    fn bindInStrictMode(file: &SourceFile, opts: &CompilerOptions) -> bool {
        // TODO:
        true
        //     if getStrictOptionValue(opts, "alwaysStrict") && !file.isDeclarationFile {
        //         // bind in strict mode source files with alwaysStrict option
        //         true
        //     }
        //     else {
        // file.externalModuleIndicator.is_some()
        //     }
    }

    fn createSymbol(&mut self, flags: SymbolFlags, name: __String) -> SymbolId {
        // symbolCount++;
        self.symbols.push(Symbol::new(flags, name))
    }

    fn addDeclarationToSymbol(
        &mut self,
        symbol: SymbolId,
        node: &Rc<BoundNode>,
        symbolFlags: SymbolFlags,
    ) {
        let sym = &mut self.symbols[symbol];

        sym.flags |= symbolFlags;

        self.store.node_data_mut(node).symbol = Some(symbol);
        pushIfUnique(&mut sym.declarations, node.clone(), |n1, n2| {
            n1.node_id() == n2.node_id()
        });

        if symbolFlags.intersects(
            SymbolFlags::Class | SymbolFlags::Enum | SymbolFlags::Module | SymbolFlags::Variable,
        ) && sym.exports.is_none()
        {
            self.symbols[symbol].exports = Some(self.createSymbolTable());
        }

        if symbolFlags.intersects(
            SymbolFlags::Class
                | SymbolFlags::Interface
                | SymbolFlags::TypeLiteral
                | SymbolFlags::ObjectLiteral,
        ) && self.symbols[symbol].members.is_none()
        {
            self.symbols[symbol].members = Some(self.createSymbolTable());
        }

        let sym = &mut self.symbols[symbol];

        // On merge of const enum module with class or function, reset const enum only flag (namespaces will already recalculate)
        if sym.constEnumOnlyModule == Some(true)
            && sym
                .flags
                .intersects(SymbolFlags::Function | SymbolFlags::Class | SymbolFlags::RegularEnum)
        {
            sym.constEnumOnlyModule = Some(false);
        }

        if symbolFlags.intersects(SymbolFlags::Value) {
            setValueDeclaration(sym, node, &mut self.store);
        }
    }

    // Should not be called on a declaration with a computed property name,
    // unless it is a well known Symbol.
    fn getDeclarationName(&mut self, node: &Rc<BoundNode>) -> Option<__String> {
        if let Node::ExportAssignment(n) = &node.node {
            return Some(__String(if n.isExportEquals {
                InternalSymbolName::ExportEquals.into()
            } else {
                InternalSymbolName::Default.into()
            }));
        }

        if let Some(name) = getNameOfDeclaration(Some(node)) {
            if isAmbientModule(self.store.node_and_data(&node.node)) {
                let moduleName = getTextOfIdentifierOrLiteral(&name);
                return Some(__String(
                    if isGlobalScopeAugmentation(self.node_data(node)) {
                        InternalSymbolName::Global.into()
                    } else {
                        moduleName
                    },
                ));
            }
            if let Node::ComputedPropertyName(name) = &name {
                let nameExpression = &name.expression;
                // treat computed property names where expression is string/numeric literal as just string/numeric literal
                if isStringOrNumericLiteralLike(nameExpression) {
                    return Some(getEscapedTextOfIdentifierOrLiteral(
                        &nameExpression.clone().into(),
                    ));
                }
                if isSignedNumericLiteral(&nameExpression.clone().into()) {
                    let nameExpression =
                        unwrap_as!(nameExpression, Expression::PrefixUnaryExpression(e), e);
                    let operand = unwrap_as!(
                        &nameExpression.operand,
                        UnaryExpression::NumericLiteral(e),
                        e
                    );
                    let mut name = tokenToString(nameExpression.operator).unwrap().to_string();
                    name.push_str(&operand.text);
                    return Some(__String(name.into()));
                } else {
                    unreachable!(
                        "Only computed properties with literal names have declaration names"
                    );
                }
            }
            if let Node::PrivateIdentifier(name) = &name {
                // containingClass exists because private names only allowed inside classes
                let containingClass = match getContainingClass(node) {
                    Some(c) => c,
                    // we can get here in cases where there is already a parse error.
                    None => return None,
                };
                let containingClassSymbol = self.node_data(&containingClass).symbol.unwrap();
                return Some(getSymbolNameForPrivateIdentifier(
                    containingClassSymbol,
                    &name.escapedText,
                ));
            }
            return if isPropertyNameLiteral(&name) {
                Some(getEscapedTextOfIdentifierOrLiteral(&name))
            } else {
                None
            };
        }
        match &node.node {
            Node::ConstructorDeclaration(_) => {
                Some(__String(InternalSymbolName::Constructor.into()))
            }
            // TODO: jsdoc:
            // | Node::JSDocSignature(_)
            Node::FunctionTypeNode(_) | Node::CallSignatureDeclaration(_) => {
                Some(__String(InternalSymbolName::Call.into()))
            }
            Node::ConstructorTypeNode(_) | Node::ConstructSignatureDeclaration(_) => {
                Some(__String(InternalSymbolName::New.into()))
            }
            Node::IndexSignatureDeclaration(_) => Some(__String(InternalSymbolName::Index.into())),
            Node::ExportDeclaration(_) => Some(__String(InternalSymbolName::ExportStar.into())),
            Node::SourceFile(_) => {
                // json file should behave as
                // module.exports = ...
                Some(__String(InternalSymbolName::ExportEquals.into()))
            }
            Node::BinaryExpression(_) => {
                if getAssignmentDeclarationKind(self.store.node_and_data(&node.node))
                    == AssignmentDeclarationKind::ModuleExports
                {
                    // module.exports = ...
                    Some(__String(InternalSymbolName::ExportEquals.into()))
                } else {
                    unreachable!("Unknown binary declaration kind");
                }
            }
            Node::JSDocFunctionType(_) => {
                Some(__String(if isJSDocConstructSignature(&node.node) {
                    InternalSymbolName::New.into()
                } else {
                    InternalSymbolName::Call.into()
                }))
            }
            Node::ParameterDeclaration(_) => {
                todo!();
                // Parameters with names are handled at the top of this function.  Parameters
                // without names can only come from JSDocFunctionTypes.
                // Debug.assert(node.parent.kind == SyntaxKind::JSDocFunctionType, "Impossible parameter parent kind", () => `parent is: ${(ts as any).SyntaxKind ? (ts as any).SyntaxKind[node.parent.kind] : node.parent.kind}, expected JSDocFunctionType`);
                // let functionType = node.parent as JSDocFunctionType;
                // let index = functionType.parameters.indexOf(node as ParameterDeclaration);
                // "arg" + index as __String
            }
            _ => None,
        }
    }

    //     function getDisplayName(node: Declaration): string {
    //         return isNamedDeclaration(node) ? declarationNameToString(node.name) : unescapeLeadingUnderscores(Debug.checkDefined(getDeclarationName(node)));
    //     }

    /**
     * Declares a Symbol for the node and adds it to symbols. Reports errors for conflicting identifier names.
     * @param symbolTable - The symbol table which node will be added to.
     * @param parent - node's parent declaration.
     * @param node - The declaration to be added to the symbol table
     * @param includes - The SymbolFlags that node has in addition to its declaration type (eg: export, ambient, etc.)
     * @param excludes - The flags which node cannot be declared alongside in a symbol table. Used to report forbidden declarations.
     */
    fn declareSymbol(
        &mut self,
        symbolTable: SymbolTableId,
        parent: Option<SymbolId>,
        node: &Rc<BoundNode>,
        includes: SymbolFlags,
        excludes: SymbolFlags,
        isReplaceableByMethod: bool,
        isComputedName: bool,
    ) -> SymbolId {
        debug_assert!(isComputedName || !hasDynamicName(node));

        let isDefaultExport =
            hasSyntacticModifier(self.store.node_and_data(&node.node), ModifierFlags::Default);
        let isDefaultExport = isDefaultExport
            || if let Node::ExportSpecifier(n) = &node.node {
                n.name.escapedText == "default"
            } else {
                false
            };

        // The exported symbol for an export default function/class node is always named "default"
        let name = if isComputedName {
            Some(__String(InternalSymbolName::Computed.into()))
        } else if isDefaultExport && parent.is_some() {
            Some(__String(InternalSymbolName::Default.into()))
        } else {
            self.getDeclarationName(node)
        };

        let symbol = if let Some(name) = name {
            // Check and see if the symbol table already has a symbol with this name.  If not,
            // create a new symbol with this name and add it to the table.  Note that we don't
            // give the new symbol any flags *yet*.  This ensures that it will not conflict
            // with the 'excludes' flags we pass in.
            //
            // If we do get an existing symbol, see if it conflicts with the new symbol we're
            // creating.  For example, a 'var' symbol and a 'class' symbol will conflict within
            // the same symbol table.  If we have a conflict, report the issue on each
            // declaration we have for this symbol, and then create a new symbol for this
            // declaration.
            //
            // Note that when properties declared in Javascript constructors
            // (marked by isReplaceableByMethod) conflict with another symbol, the property loses.
            // Always. This allows the common Javascript pattern of overwriting a prototype method
            // with an bound instance method of the same type: `this.method = this.method.bind(this)`
            //
            // If we created a new symbol, either because we didn't have a symbol with this name
            // in the symbol table, or we conflicted with an existing symbol, then just add this
            // node as the sole declaration of the new symbol.
            //
            // Otherwise, we'll be merging into a compatible existing symbol (for example when
            // you have multiple 'vars' with the same name in the same container).  In this case
            // just add this node into the declarations list of the symbol.
            let sym = self.symbol_tables[symbolTable].get(&name).copied();

            if includes.intersects(SymbolFlags::Classifiable) {
                todo!();
                // classifiableNames.add(name);
            }

            if let Some(sym) = sym {
                if isReplaceableByMethod && !self.symbols[sym].isReplaceableByMethod {
                    // A symbol already exists, so don't add this as a declaration.
                    return sym;
                } else if self.symbols[sym].flags.intersects(excludes) {
                    if self.symbols[sym].isReplaceableByMethod {
                        // Javascript constructor-declared symbols can be discarded in favor of
                        // prototype symbols like methods.
                        let s = self.createSymbol(SymbolFlags::None, name.clone());
                        self.symbol_tables[symbolTable].insert(name, s);
                        s
                    } else if !(includes.intersects(SymbolFlags::Variable)
                        && self.symbols[sym].flags.intersects(SymbolFlags::Assignment))
                    {
                        todo!()
                        // // Assignment declarations are allowed to merge with variables, no matter what other flags they have.
                        // if (isNamedDeclaration(node)) {
                        //     setParent(node.name, node);
                        // }
                        // // Report errors every position with duplicate declaration
                        // // Report errors on previous encountered declarations
                        // let message = if symbol.flags & SymbolFlags::BlockScopedVariable
                        //     {Diagnostics.Cannot_redeclare_block_scoped_variable_0}
                        //     else{Diagnostics.Duplicate_identifier_0};
                        // let messageNeedsName = true;

                        // if (symbol.flags & SymbolFlags::Enum || includes & SymbolFlags::Enum) {
                        //     message = Diagnostics.Enum_declarations_can_only_merge_with_namespace_or_other_enum_declarations;
                        //     messageNeedsName = false;
                        // }

                        // let multipleDefaultExports = false;
                        // if (length(symbol.declarations)) {
                        //     // If the current node is a default export of some sort, then check if
                        //     // there are any other default exports that we need to error on.
                        //     // We'll know whether we have other default exports depending on if `symbol` already has a declaration list set.
                        //     if (isDefaultExport) {
                        //         message = Diagnostics.A_module_cannot_have_multiple_default_exports;
                        //         messageNeedsName = false;
                        //         multipleDefaultExports = true;
                        //     }
                        //     else {
                        //         // This is to properly report an error in the case "export default { }" is after export default of class declaration or function declaration.
                        //         // Error on multiple export default in the following case:
                        //         // 1. multiple export default of class declaration or function declaration by checking NodeFlags::Default
                        //         // 2. multiple export default of export assignment. This one doesn't have NodeFlags::Default on (as export default doesn't considered as modifiers)
                        //         if (symbol.declarations && symbol.declarations.length &&
                        //             (node.kind == SyntaxKind::ExportAssignment && !(node as ExportAssignment).isExportEquals)) {
                        //             message = Diagnostics.A_module_cannot_have_multiple_default_exports;
                        //             messageNeedsName = false;
                        //             multipleDefaultExports = true;
                        //         }
                        //     }
                        // }

                        // const relatedInformation: DiagnosticRelatedInformation[] = [];
                        // if (isTypeAliasDeclaration(node) && nodeIsMissing(node.ty) && hasSyntacticModifier(node, ModifierFlags.Export) && symbol.flags & (SymbolFlags::Alias | SymbolFlags::Type | SymbolFlags::Namespace)) {
                        //     todo!();
                        //     // export type T; - may have meant export type { T }?
                        //     // relatedInformation.push(createDiagnosticForNode(node, Diagnostics.Did_you_mean_0, `export type { ${unescapeLeadingUnderscores(node.name.escapedText)} }`));
                        // }

                        // let declarationName = getNameOfDeclaration(node) || node;
                        // forEach(symbol.declarations, |(declaration, index)| {
                        //     let decl = getNameOfDeclaration(declaration) || declaration;
                        //     let diag = createDiagnosticForNode(decl, message, if messageNeedsName {getDisplayName(declaration)}else{undefined});
                        //     self.file.bindDiagnostics.push(
                        //         multipleDefaultExports ? addRelatedInfo(diag, createDiagnosticForNode(declarationName, index === 0 ? Diagnostics.Another_export_default_is_here : Diagnostics.and_here)) : diag
                        //     );
                        //     if (multipleDefaultExports) {
                        //         relatedInformation.push(createDiagnosticForNode(decl, Diagnostics.The_first_export_default_is_here));
                        //     }
                        // });

                        // let diag = createDiagnosticForNode(declarationName, message, messageNeedsName ? getDisplayName(node) : undefined);
                        // self.file.bindDiagnostics.push(addRelatedInfo(diag, ...relatedInformation));

                        // symbol = createSymbol(SymbolFlags::None, name);
                    } else {
                        sym
                    }
                } else {
                    sym
                }
            } else {
                let s = self.createSymbol(SymbolFlags::None, name.clone());
                self.symbol_tables[symbolTable].insert(name, s);
                if isReplaceableByMethod {
                    self.symbols[s].isReplaceableByMethod = true;
                }
                s
            }
        } else {
            self.createSymbol(
                SymbolFlags::None,
                __String(InternalSymbolName::Missing.into()),
            )
        };

        self.addDeclarationToSymbol(symbol, node, includes);
        if self.symbols[symbol].parent.is_some() {
            debug_assert!(
                self.symbols[symbol].parent == parent,
                "Existing symbol parent should match new one",
            );
        } else {
            self.symbols[symbol].parent = parent;
        }

        symbol
    }

    //     function declareModuleMember(node: Declaration, symbolFlags: SymbolFlags, symbolExcludes: SymbolFlags): Symbol {
    //         const hasExportModifier = !!(getCombinedModifierFlags(node) & ModifierFlags.Export) || jsdocTreatAsExported(node);
    //         if (symbolFlags & SymbolFlags::Alias) {
    //             if (node.kind === SyntaxKind::ExportSpecifier || (node.kind === SyntaxKind::ImportEqualsDeclaration && hasExportModifier)) {
    //                 return declareSymbol(container.symbol.exports!, container.symbol, node, symbolFlags, symbolExcludes);
    //             }
    //             else {
    //                 return declareSymbol(container.locals!, /*parent*/ undefined, node, symbolFlags, symbolExcludes);
    //             }
    //         }
    //         else {
    //             // Exported module members are given 2 symbols: A local symbol that is classified with an ExportValue flag,
    //             // and an associated export symbol with all the correct flags set on it. There are 2 main reasons:
    //             //
    //             //   1. We treat locals and exports of the same name as mutually exclusive within a container.
    //             //      That means the binder will issue a Duplicate Identifier error if you mix locals and exports
    //             //      with the same name in the same container.
    //             //      TODO: Make this a more specific error and decouple it from the exclusion logic.
    //             //   2. When we checkIdentifier in the checker, we set its resolved symbol to the local symbol,
    //             //      but return the export symbol (by calling getExportSymbolOfValueSymbolIfExported). That way
    //             //      when the emitter comes back to it, it knows not to qualify the name if it was found in a containing scope.

    //             // NOTE: Nested ambient modules always should go to to 'locals' table to prevent their automatic merge
    //             //       during global merging in the checker. Why? The only case when ambient module is permitted inside another module is module augmentation
    //             //       and this case is specially handled. Module augmentations should only be merged with original module definition
    //             //       and should never be merged directly with other augmentation, and the latter case would be possible if automatic merge is allowed.
    //             if (isJSDocTypeAlias(node)) Debug.assert(isInJSFile(node)); // We shouldn't add symbols for JSDoc nodes if not in a JS file.
    //             if (!isAmbientModule(node) && (hasExportModifier || container.flags & NodeFlags::ExportContext)) {
    //                 if (!container.locals || (hasSyntacticModifier(node, ModifierFlags.Default) && !getDeclarationName(node))) {
    //                     return declareSymbol(container.symbol.exports!, container.symbol, node, symbolFlags, symbolExcludes); // No local symbol for an unnamed default!
    //                 }
    //                 const exportKind = symbolFlags & SymbolFlags::Value ? SymbolFlags::ExportValue : 0;
    //                 const local = declareSymbol(container.locals, /*parent*/ undefined, node, exportKind, symbolExcludes);
    //                 local.exportSymbol = declareSymbol(container.symbol.exports!, container.symbol, node, symbolFlags, symbolExcludes);
    //                 node.localSymbol = local;
    //                 return local;
    //             }
    //             else {
    //                 return declareSymbol(container.locals!, /*parent*/ undefined, node, symbolFlags, symbolExcludes);
    //             }
    //         }
    //     }

    //     function jsdocTreatAsExported(node: Node) {
    //         if (node.parent && isModuleDeclaration(node)) {
    //             node = node.parent;
    //         }
    //         if (!isJSDocTypeAlias(node)) return false;
    //         // jsdoc typedef handling is a bit of a doozy, but to summarize, treat the typedef as exported if:
    //         // 1. It has an explicit name (since by default typedefs are always directly exported, either at the top level or in a container), or
    //         if (!isJSDocEnumTag(node) && !!node.fullName) return true;
    //         // 2. The thing a nameless typedef pulls its name from is implicitly a direct export (either by assignment or actual export flag).
    //         const declName = getNameOfDeclaration(node);
    //         if (!declName) return false;
    //         if (isPropertyAccessEntityNameExpression(declName.parent) && isTopLevelNamespaceAssignment(declName.parent)) return true;
    //         if (isDeclaration(declName.parent) && getCombinedModifierFlags(declName.parent) & ModifierFlags.Export) return true;
    //         // This could potentially be simplified by having `delayedBindJSDocTypedefTag` pass in an override for `hasExportModifier`, since it should
    //         // already have calculated and branched on most of this.
    //         return false;
    //     }

    // All container nodes are kept on a linked list in declaration order. This list is used by
    // the getLocalNameOfContainer function in the type checker to validate that the local name
    // used for a container is unique.
    fn bindContainer(&mut self, node: Rc<BoundNode>, containerFlags: ContainerFlags) {
        // Before we recurse into a node's children, we first save the existing parent, container
        // and block-container.  Then after we pop out of processing the children, we restore
        // these saved values.
        let saveContainer = self.container.clone();
        let saveThisParentContainer = self.thisParentContainer.clone();
        let savedBlockScopeContainer = self.blockScopeContainer.clone();

        // Depending on what kind of node this is, we may have to adjust the current container
        // and block-container.   If the current node is a container, then it is automatically
        // considered the current block-container as well.  Also, for containers that we know
        // may contain locals, we eagerly initialize the .locals field. We do this because
        // it's highly likely that the .locals will be needed to place some child in (for example,
        // a parameter, or variable declaration).
        //
        // However, we do not proactively create the .locals for block-containers because it's
        // totally normal and common for block-containers to never actually have a block-scoped
        // variable in them.  We don't want to end up allocating an object for every 'block' we
        // run into when most of them won't be necessary.
        //
        // Finally, if this is a block-container, then we clear out any existing .locals object
        // it may contain within it.  This happens in incremental scenarios.  Because we can be
        // reusing a node from a previous compilation, that node may have had 'locals' created
        // for it.  We must clear this so we don't accidentally move any stale data forward from
        // a previous compilation.
        if containerFlags.intersects(ContainerFlags::IsContainer) {
            if node.kind() != SyntaxKind::ArrowFunction {
                self.thisParentContainer = self.container.clone();
            }
            self.container = Some(node.clone());
            self.blockScopeContainer = Some(node.clone());
            if containerFlags.intersects(ContainerFlags::HasLocals) {
                self.node_data_mut(&node).locals = Some(self.createSymbolTable());
            }
            self.addToContainerChain(node.clone());
        } else if containerFlags.intersects(ContainerFlags::IsBlockScopedContainer) {
            self.blockScopeContainer = Some(node.clone());
            self.node_data_mut(&node).locals = None;
        }
        if containerFlags.intersects(ContainerFlags::IsControlFlowContainer) {
            let saveCurrentFlow = self.currentFlow;
            let saveBreakTarget = self.currentBreakTarget;
            let saveContinueTarget = self.currentContinueTarget;
            let saveReturnTarget = self.currentReturnTarget;
            let saveExceptionTarget = self.currentExceptionTarget;
            // TODO: if activeLabelList is used a stack, we might be able to just
            // keep track of an index, then truncate the list to that index.
            let saveActiveLabelList = self.activeLabelList.clone();
            let saveHasExplicitReturn = self.hasExplicitReturn;
            let isIIFE = if containerFlags.intersects(ContainerFlags::IsFunctionExpression) {
                todo!("see below")
            } else {
                false
            };
            // TODO:
            // let isIIFE = containerFlags.intersects(ContainerFlags::IsFunctionExpression)
            //     && !hasSyntacticModifier(node, ModifierFlags::Async)
            //     && !(node as FunctionLikeDeclaration).asteriskToken
            //     && !!getImmediatelyInvokedFunctionExpression(node);
            // A non-async, non-generator IIFE is considered part of the containing control flow. Return statements behave
            // similarly to break statements that exit to a label just past the statement body.
            if !isIIFE {
                let mut flow_start = FlowStart { node: None };
                if containerFlags.intersects(
                    ContainerFlags::IsFunctionExpression
                        | ContainerFlags::IsObjectLiteralOrClassExpressionMethodOrAccessor,
                ) {
                    flow_start.node = Some(node.clone());
                }
                self.currentFlow = self.alloc_flow_node(FlowNode {
                    flags: FlowFlags::Start,
                    kind: FlowNodeKind::FlowStart(flow_start),
                });
            }
            // We create a return control flow graph for IIFEs and constructors. For constructors
            // we use the return control flow graph in strict property initialization checks.
            self.currentReturnTarget = if isIIFE
                || node.kind() == SyntaxKind::Constructor
                || node.kind() == SyntaxKind::ClassStaticBlockDeclaration
                || (isInJSFile(Some(self.store.node_and_data(&node)))
                    && (node.kind() == SyntaxKind::FunctionDeclaration
                        || node.kind() == SyntaxKind::FunctionExpression))
            {
                todo!()
                // Some(FlowNode::new_branch_label())
            } else {
                None
            };
            self.currentExceptionTarget = None;
            self.currentBreakTarget = None;
            self.currentContinueTarget = None;
            self.activeLabelList = Vec::new();
            self.hasExplicitReturn = false;
            self.bindChildren(&node);
            // Reset all reachability check related flags on node (for incremental scenarios)
            self.node_data_mut(&node).flags &= !NodeFlags::ReachabilityAndEmitFlags;
            if !self.flow_nodes[self.currentFlow]
                .flags
                .intersects(FlowFlags::Unreachable)
                && containerFlags.intersects(ContainerFlags::IsFunctionLike)
            // && nodeIsPresent(
            //     (node as FunctionLikeDeclaration | ClassStaticBlockDeclaration).body,
            // )
            {
                todo!("condition above and body below");
                // self.node_data_mut(&node).flags |= NodeFlags::HasImplicitReturn;
                // if self.hasExplicitReturn {
                //     self.node_data_mut(&node).flags |= NodeFlags::HasExplicitReturn;
                // }
                // (node as FunctionLikeDeclaration | ClassStaticBlockDeclaration).endFlowNode =
                //     self.currentFlow;
            }
            if node.kind() == SyntaxKind::SourceFile {
                self.store.node_data_mut(&node).flags |= self.emitFlags;
                // TODO:
                // (node as SourceFile).endFlowNode = self.currentFlow;
            }

            if let Some(currentReturnTarget) = self.currentReturnTarget {
                self.addAntecedent(currentReturnTarget, self.currentFlow);
                self.currentFlow = self.finishFlowLabel(currentReturnTarget);
                if node.kind() == SyntaxKind::Constructor
                    || node.kind() == SyntaxKind::ClassStaticBlockDeclaration
                    || (isInJSFile(Some(self.store.node_and_data(&node)))
                        && (node.kind() == SyntaxKind::FunctionDeclaration
                            || node.kind() == SyntaxKind::FunctionExpression))
                {
                    todo!();
                    // (node as FunctionLikeDeclaration | ClassStaticBlockDeclaration)
                    //     .returnFlowNode = self.currentFlow;
                }
            }
            if !isIIFE {
                self.currentFlow = saveCurrentFlow;
            }
            self.currentBreakTarget = saveBreakTarget;
            self.currentContinueTarget = saveContinueTarget;
            self.currentReturnTarget = saveReturnTarget;
            self.currentExceptionTarget = saveExceptionTarget;
            self.activeLabelList = saveActiveLabelList;
            self.hasExplicitReturn = saveHasExplicitReturn;
        } else if containerFlags.intersects(ContainerFlags::IsInterface) {
            self.seenThisKeyword = false;
            self.bindChildren(&node);
            if self.seenThisKeyword {
                self.node_data_mut(&node).flags |= NodeFlags::ContainsThis;
            } else {
                self.node_data_mut(&node).flags &= !NodeFlags::ContainsThis;
            };
        } else {
            self.bindChildren(&node);
        }

        self.container = saveContainer;
        self.thisParentContainer = saveThisParentContainer;
        self.blockScopeContainer = savedBlockScopeContainer;
    }

    fn bindEachFunctionsFirst<T>(
        &mut self,
        parent: Option<&Rc<BoundNode>>,
        nodes: Option<&NodeArray<T>>,
    ) where
        T: Bind,
    {
        self.bindEachWith(parent, nodes, |b, n| {
            if n.kind() == SyntaxKind::FunctionDeclaration {
                b.bind(Some(n))
            }
        });
        self.bindEachWith(parent, nodes, |b, n| {
            if n.kind() != SyntaxKind::FunctionDeclaration {
                b.bind(Some(n))
            }
        });
    }

    fn bindEach<T>(&mut self, parent: Option<&Rc<BoundNode>>, nodes: Option<&NodeArray<T>>)
    where
        T: Bind,
    {
        if let Some(nodes) = nodes {
            for n in nodes.iter() {
                self.bind(Some(n.bind_to_opt_parent(parent)))
            }
        }
    }

    fn bindEachWith<T, F>(
        &mut self,
        parent: Option<&Rc<BoundNode>>,
        nodes: Option<&NodeArray<T>>,
        mut bindFunction: F,
    ) where
        T: Bind,
        F: FnMut(&mut Self, Rc<BoundNode>),
    {
        if let Some(nodes) = nodes {
            for n in nodes.iter() {
                bindFunction(self, n.bind_to_opt_parent(parent))
            }
        }
    }

    fn bindEachChild(&mut self, parent: Option<&Rc<BoundNode>>, node: Node) {
        forEachChild::<(), _>(Some(node), |n| {
            self.bind(Some(n.bind_to_opt_parent(parent)));
            None
        });
    }

    fn bindChildren(&mut self, node: &Rc<BoundNode>) {
        let saveInAssignmentPattern = self.inAssignmentPattern;
        // Most nodes aren't valid in an assignment pattern, so we clear the value here
        // and set it before we descend into nodes that could actually be part of an assignment pattern.
        self.inAssignmentPattern = false;
        if self.checkUnreachable(node) {
            todo!();
            // self.bindEachChild(node);
            // self.bindJSDoc(node);
            // self.inAssignmentPattern = saveInAssignmentPattern;
            // return;
        }
        if node.kind() >= SyntaxKind::FirstStatement
            && node.kind() <= SyntaxKind::LastStatement
            && !self.options.allowUnreachableCode
        {
            self.node_data_mut(node).flowNode = Some(self.currentFlow);
        }
        match &node.node {
            Node::WhileStatement(n) => {
                todo!();
                // self.bindWhileStatement(n);
            }
            Node::DoStatement(n) => {
                todo!();
                // self.bindDoStatement(n);
            }
            Node::ForStatement(n) => {
                todo!();
                // self.bindForStatement(n);
            }
            Node::ForInStatement(_) | Node::ForOfStatement(_) => {
                todo!();
                // self.bindForInOrForOfStatement(node as ForInOrOfStatement);
            }
            Node::IfStatement(_) => {
                todo!();
                // self.bindIfStatement(n);
            }
            Node::ReturnStatement(_) | Node::ThrowStatement(_) => {
                todo!();
                // self.bindReturnOrThrow(node as ReturnStatement | ThrowStatement);
            }
            Node::BreakStatement(_) | Node::ContinueStatement(_) => {
                todo!();
                // self.bindBreakOrContinueStatement(node as BreakOrContinueStatement);
            }
            Node::TryStatement(n) => {
                todo!();
                // self.bindTryStatement(n);
            }
            Node::SwitchStatement(n) => {
                todo!();
                // self.bindSwitchStatement(n);
            }
            Node::CaseBlock(n) => {
                todo!();
                // self.bindCaseBlock(n);
            }
            Node::CaseClause(n) => {
                todo!();
                // self.bindCaseClause(n);
            }
            Node::ExpressionStatement(n) => {
                todo!();
                // self.bindExpressionStatement(n);
            }
            Node::LabeledStatement(n) => {
                todo!();
                // self.bindLabeledStatement(n);
            }
            Node::PrefixUnaryExpression(n) => {
                todo!();
                // self.bindPrefixUnaryExpressionFlow(n);
            }
            Node::PostfixUnaryExpression(n) => {
                todo!();
                // self.bindPostfixUnaryExpressionFlow(n);
            }
            Node::BinaryExpression(_) => {
                todo!();
                // if isDestructuringAssignment(node) {
                //     // Carry over whether we are in an assignment pattern to
                //     // binary expressions that could actually be an initializer
                //     self.inAssignmentPattern = saveInAssignmentPattern;
                //     self.bindDestructuringAssignmentFlow(node);
                //     return;
                // }
                // self.bindBinaryExpressionFlow(node as BinaryExpression);
            }
            Node::DeleteExpression(_) => {
                todo!();
                // self.bindDeleteExpressionFlow(node as DeleteExpression);
            }
            Node::ConditionalExpression(_) => {
                todo!();
                // self.bindConditionalExpressionFlow(node as ConditionalExpression);
            }
            Node::VariableDeclaration(n) => {
                self.bindVariableDeclarationFlow(node, n);
            }
            Node::PropertyAccessExpression(_) | Node::ElementAccessExpression(_) => {
                todo!();
                // self.bindAccessExpressionFlow(node as AccessExpression);
            }
            Node::CallExpression(_) => {
                todo!();
                // self.bindCallExpressionFlow(node as CallExpression);
            }
            Node::NonNullExpression(_) => {
                todo!();
                // self.bindNonNullExpressionFlow(node as NonNullExpression);
            }
            // TODO: jsdoc
            // Node::JSDocTypedefTag(_) | Node::JSDocCallbackTag(_) | Node::JSDocEnumTag(_) => {
            //     todo!();
            //     // self.bindJSDocTypeAlias(node as JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag);
            // }
            // In source files and blocks, bind functions first to match hoisting that occurs at runtime
            Node::SourceFile(n) => {
                self.bindEachFunctionsFirst(Some(node), Some(&n.statements));
                self.bind(Some(n.endOfFileToken.bind(node)));
            }
            Node::Block(_) | Node::ModuleBlock(_) => {
                todo!();
                // self.bindEachFunctionsFirst((node as Block).statements);
            }
            Node::BindingElement(_) => {
                todo!();
                // self.bindBindingElementFlow(node as BindingElement);
            }
            Node::ObjectLiteralExpression(_)
            | Node::ArrayLiteralExpression(_)
            | Node::PropertyAssignment(_)
            | Node::SpreadElement(_) => {
                todo!();
                // // Carry over whether we are in an assignment pattern of Object and Array literals
                // // as well as their children that are valid assignment targets.
                // self.inAssignmentPattern = saveInAssignmentPattern;
                // self.bindEachChild(node);
            }
            _ => {
                self.bindEachChild(node.parent().as_ref(), node.node.clone());
            }
        }
        self.bindJSDoc(node);
        self.inAssignmentPattern = saveInAssignmentPattern;
    }

    //     function isNarrowingExpression(expr: Expression): boolean {
    //         switch (expr.kind) {
    //             case SyntaxKind::Identifier:
    //             case SyntaxKind::PrivateIdentifier:
    //             case SyntaxKind::ThisKeyword:
    //             case SyntaxKind::PropertyAccessExpression:
    //             case SyntaxKind::ElementAccessExpression:
    //                 return containsNarrowableReference(expr);
    //             case SyntaxKind::CallExpression:
    //                 return hasNarrowableArgument(expr as CallExpression);
    //             case SyntaxKind::ParenthesizedExpression:
    //             case SyntaxKind::NonNullExpression:
    //                 return isNarrowingExpression((expr as ParenthesizedExpression | NonNullExpression).expression);
    //             case SyntaxKind::BinaryExpression:
    //                 return isNarrowingBinaryExpression(expr as BinaryExpression);
    //             case SyntaxKind::PrefixUnaryExpression:
    //                 return (expr as PrefixUnaryExpression).operator === SyntaxKind::ExclamationToken && isNarrowingExpression((expr as PrefixUnaryExpression).operand);
    //             case SyntaxKind::TypeOfExpression:
    //                 return isNarrowingExpression((expr as TypeOfExpression).expression);
    //         }
    //         return false;
    //     }

    //     function isNarrowableReference(expr: Expression): boolean {
    //         return isDottedName(expr)
    //             || (isPropertyAccessExpression(expr) || isNonNullExpression(expr) || isParenthesizedExpression(expr)) && isNarrowableReference(expr.expression)
    //             || isBinaryExpression(expr) && expr.operatorToken.kind === SyntaxKind::CommaToken && isNarrowableReference(expr.right)
    //             || isElementAccessExpression(expr) && isStringOrNumericLiteralLike(expr.argumentExpression) && isNarrowableReference(expr.expression)
    //             || isAssignmentExpression(expr) && isNarrowableReference(expr.left);
    //     }

    //     function containsNarrowableReference(expr: Expression): boolean {
    //         return isNarrowableReference(expr) || isOptionalChain(expr) && containsNarrowableReference(expr.expression);
    //     }

    //     function hasNarrowableArgument(expr: CallExpression) {
    //         if (expr.arguments) {
    //             for (const argument of expr.arguments) {
    //                 if (containsNarrowableReference(argument)) {
    //                     return true;
    //                 }
    //             }
    //         }
    //         if (expr.expression.kind === SyntaxKind::PropertyAccessExpression &&
    //             containsNarrowableReference((expr.expression as PropertyAccessExpression).expression)) {
    //             return true;
    //         }
    //         return false;
    //     }

    //     function isNarrowingTypeofOperands(expr1: Expression, expr2: Expression) {
    //         return isTypeOfExpression(expr1) && isNarrowableOperand(expr1.expression) && isStringLiteralLike(expr2);
    //     }

    //     function isNarrowingBinaryExpression(expr: BinaryExpression) {
    //         switch (expr.operatorToken.kind) {
    //             case SyntaxKind::EqualsToken:
    //             case SyntaxKind::BarBarEqualsToken:
    //             case SyntaxKind::AmpersandAmpersandEqualsToken:
    //             case SyntaxKind::QuestionQuestionEqualsToken:
    //                 return containsNarrowableReference(expr.left);
    //             case SyntaxKind::EqualsEqualsToken:
    //             case SyntaxKind::ExclamationEqualsToken:
    //             case SyntaxKind::EqualsEqualsEqualsToken:
    //             case SyntaxKind::ExclamationEqualsEqualsToken:
    //                 return isNarrowableOperand(expr.left) || isNarrowableOperand(expr.right) ||
    //                     isNarrowingTypeofOperands(expr.right, expr.left) || isNarrowingTypeofOperands(expr.left, expr.right);
    //             case SyntaxKind::InstanceOfKeyword:
    //                 return isNarrowableOperand(expr.left);
    //             case SyntaxKind::InKeyword:
    //                 return isNarrowingExpression(expr.right);
    //             case SyntaxKind::CommaToken:
    //                 return isNarrowingExpression(expr.right);
    //         }
    //         return false;
    //     }

    //     function isNarrowableOperand(expr: Expression): boolean {
    //         switch (expr.kind) {
    //             case SyntaxKind::ParenthesizedExpression:
    //                 return isNarrowableOperand((expr as ParenthesizedExpression).expression);
    //             case SyntaxKind::BinaryExpression:
    //                 switch ((expr as BinaryExpression).operatorToken.kind) {
    //                     case SyntaxKind::EqualsToken:
    //                         return isNarrowableOperand((expr as BinaryExpression).left);
    //                     case SyntaxKind::CommaToken:
    //                         return isNarrowableOperand((expr as BinaryExpression).right);
    //                 }
    //         }
    //         return containsNarrowableReference(expr);
    //     }

    //     function createBranchLabel(): FlowLabel {
    //         return initFlowNode({ flags: FlowFlags.BranchLabel, antecedents: undefined });
    //     }

    //     function createLoopLabel(): FlowLabel {
    //         return initFlowNode({ flags: FlowFlags.LoopLabel, antecedents: undefined });
    //     }

    //     function createReduceLabel(target: FlowLabel, antecedents: FlowNode[], antecedent: FlowNode): FlowReduceLabel {
    //         return initFlowNode({ flags: FlowFlags.ReduceLabel, target, antecedents, antecedent });
    //     }

    fn setFlowNodeReferenced(&mut self, flow: FlowNodeId) {
        // On first reference we set the Referenced flag, thereafter we set the Shared flag
        let is_referenced = self.flow_nodes[flow]
            .flags
            .intersects(FlowFlags::Referenced);
        self.flow_nodes[flow].flags |= if is_referenced {
            FlowFlags::Shared
        } else {
            FlowFlags::Referenced
        };
    }

    fn addAntecedent(&mut self, label: FlowLabelId, antecedent: FlowNodeId) {
        todo!();
        // if (!(antecedent.flags & FlowFlags.Unreachable) && !contains(label.antecedents, antecedent)) {
        //     (label.antecedents || (label.antecedents = [])).push(antecedent);
        //     setFlowNodeReferenced(antecedent);
        // }
    }

    //     function createFlowCondition(flags: FlowFlags, antecedent: FlowNode, expression: Expression | undefined): FlowNode {
    //         if (antecedent.flags & FlowFlags.Unreachable) {
    //             return antecedent;
    //         }
    //         if (!expression) {
    //             return flags & FlowFlags.TrueCondition ? antecedent : unreachableFlow;
    //         }
    //         if ((expression.kind === SyntaxKind::TrueKeyword && flags & FlowFlags.FalseCondition ||
    //             expression.kind === SyntaxKind::FalseKeyword && flags & FlowFlags.TrueCondition) &&
    //             !isExpressionOfOptionalChainRoot(expression) && !isNullishCoalesce(expression.parent)) {
    //             return unreachableFlow;
    //         }
    //         if (!isNarrowingExpression(expression)) {
    //             return antecedent;
    //         }
    //         setFlowNodeReferenced(antecedent);
    //         return initFlowNode({ flags, antecedent, node: expression });
    //     }

    //     function createFlowSwitchClause(antecedent: FlowNode, switchStatement: SwitchStatement, clauseStart: number, clauseEnd: number): FlowNode {
    //         setFlowNodeReferenced(antecedent);
    //         return initFlowNode({ flags: FlowFlags.SwitchClause, antecedent, switchStatement, clauseStart, clauseEnd });
    //     }

    fn createFlowAssignment(&mut self, antecedent: FlowNodeId, node: Rc<BoundNode>) -> FlowNodeId {
        self.setFlowNodeReferenced(antecedent);
        let node = FlowNode {
            flags: FlowFlags::Assignment,
            kind: FlowNodeKind::FlowAssignment(FlowAssignment { antecedent, node }),
        };
        let id = self.alloc_flow_node(node);
        if let Some(currentExceptionTarget) = self.currentExceptionTarget {
            self.addAntecedent(currentExceptionTarget, id);
        }
        id
    }

    //     function createFlowCall(antecedent: FlowNode, node: CallExpression): FlowNode {
    //         setFlowNodeReferenced(antecedent);
    //         return initFlowNode({ flags: FlowFlags.Call, antecedent, node });
    //     }

    fn finishFlowLabel(&mut self, flow: FlowLabelId) -> FlowNodeId {
        todo!();
        // const antecedents = flow.antecedents;
        // if (!antecedents) {
        //     return unreachableFlow;
        // }
        // if (antecedents.length === 1) {
        //     return antecedents[0];
        // }
        // return flow;
    }

    //     function isStatementCondition(node: Node) {
    //         const parent = node.parent;
    //         switch (parent.kind) {
    //             case SyntaxKind::IfStatement:
    //             case SyntaxKind::WhileStatement:
    //             case SyntaxKind::DoStatement:
    //                 return (parent as IfStatement | WhileStatement | DoStatement).expression === node;
    //             case SyntaxKind::ForStatement:
    //             case SyntaxKind::ConditionalExpression:
    //                 return (parent as ForStatement | ConditionalExpression).condition === node;
    //         }
    //         return false;
    //     }

    //     function isLogicalExpression(node: Node) {
    //         while (true) {
    //             if (node.kind === SyntaxKind::ParenthesizedExpression) {
    //                 node = (node as ParenthesizedExpression).expression;
    //             }
    //             else if (node.kind === SyntaxKind::PrefixUnaryExpression && (node as PrefixUnaryExpression).operator === SyntaxKind::ExclamationToken) {
    //                 node = (node as PrefixUnaryExpression).operand;
    //             }
    //             else {
    //                 return node.kind === SyntaxKind::BinaryExpression && (
    //                     (node as BinaryExpression).operatorToken.kind === SyntaxKind::AmpersandAmpersandToken ||
    //                     (node as BinaryExpression).operatorToken.kind === SyntaxKind::BarBarToken ||
    //                     (node as BinaryExpression).operatorToken.kind === SyntaxKind::QuestionQuestionToken);
    //             }
    //         }
    //     }

    //     function isLogicalAssignmentExpression(node: Node) {
    //         node = skipParentheses(node);
    //         return isBinaryExpression(node) && isLogicalOrCoalescingAssignmentOperator(node.operatorToken.kind);
    //     }

    //     function isTopLevelLogicalExpression(node: Node): boolean {
    //         while (isParenthesizedExpression(node.parent) ||
    //             isPrefixUnaryExpression(node.parent) && node.parent.operator === SyntaxKind::ExclamationToken) {
    //             node = node.parent;
    //         }
    //         return !isStatementCondition(node) &&
    //             !isLogicalAssignmentExpression(node.parent) &&
    //             !isLogicalExpression(node.parent) &&
    //             !(isOptionalChain(node.parent) && node.parent.expression === node);
    //     }

    //     function doWithConditionalBranches<T>(action: (value: T) => void, value: T, trueTarget: FlowLabel, falseTarget: FlowLabel) {
    //         const savedTrueTarget = currentTrueTarget;
    //         const savedFalseTarget = currentFalseTarget;
    //         currentTrueTarget = trueTarget;
    //         currentFalseTarget = falseTarget;
    //         action(value);
    //         currentTrueTarget = savedTrueTarget;
    //         currentFalseTarget = savedFalseTarget;
    //     }

    //     function bindCondition(node: Expression | undefined, trueTarget: FlowLabel, falseTarget: FlowLabel) {
    //         doWithConditionalBranches(bind, node, trueTarget, falseTarget);
    //         if (!node || !isLogicalAssignmentExpression(node) && !isLogicalExpression(node) && !(isOptionalChain(node) && isOutermostOptionalChain(node))) {
    //             addAntecedent(trueTarget, createFlowCondition(FlowFlags.TrueCondition, currentFlow, node));
    //             addAntecedent(falseTarget, createFlowCondition(FlowFlags.FalseCondition, currentFlow, node));
    //         }
    //     }

    //     function bindIterativeStatement(node: Statement, breakTarget: FlowLabel, continueTarget: FlowLabel): void {
    //         const saveBreakTarget = currentBreakTarget;
    //         const saveContinueTarget = currentContinueTarget;
    //         currentBreakTarget = breakTarget;
    //         currentContinueTarget = continueTarget;
    //         bind(node);
    //         currentBreakTarget = saveBreakTarget;
    //         currentContinueTarget = saveContinueTarget;
    //     }

    //     function setContinueTarget(node: Node, target: FlowLabel) {
    //         let label = activeLabelList;
    //         while (label && node.parent.kind === SyntaxKind::LabeledStatement) {
    //             label.continueTarget = target;
    //             label = label.next;
    //             node = node.parent;
    //         }
    //         return target;
    //     }

    //     function bindWhileStatement(node: WhileStatement): void {
    //         const preWhileLabel = setContinueTarget(node, createLoopLabel());
    //         const preBodyLabel = createBranchLabel();
    //         const postWhileLabel = createBranchLabel();
    //         addAntecedent(preWhileLabel, currentFlow);
    //         currentFlow = preWhileLabel;
    //         bindCondition(node.expression, preBodyLabel, postWhileLabel);
    //         currentFlow = finishFlowLabel(preBodyLabel);
    //         bindIterativeStatement(node.statement, postWhileLabel, preWhileLabel);
    //         addAntecedent(preWhileLabel, currentFlow);
    //         currentFlow = finishFlowLabel(postWhileLabel);
    //     }

    //     function bindDoStatement(node: DoStatement): void {
    //         const preDoLabel = createLoopLabel();
    //         const preConditionLabel = setContinueTarget(node, createBranchLabel());
    //         const postDoLabel = createBranchLabel();
    //         addAntecedent(preDoLabel, currentFlow);
    //         currentFlow = preDoLabel;
    //         bindIterativeStatement(node.statement, postDoLabel, preConditionLabel);
    //         addAntecedent(preConditionLabel, currentFlow);
    //         currentFlow = finishFlowLabel(preConditionLabel);
    //         bindCondition(node.expression, preDoLabel, postDoLabel);
    //         currentFlow = finishFlowLabel(postDoLabel);
    //     }

    //     function bindForStatement(node: ForStatement): void {
    //         const preLoopLabel = setContinueTarget(node, createLoopLabel());
    //         const preBodyLabel = createBranchLabel();
    //         const postLoopLabel = createBranchLabel();
    //         bind(node.initializer);
    //         addAntecedent(preLoopLabel, currentFlow);
    //         currentFlow = preLoopLabel;
    //         bindCondition(node.condition, preBodyLabel, postLoopLabel);
    //         currentFlow = finishFlowLabel(preBodyLabel);
    //         bindIterativeStatement(node.statement, postLoopLabel, preLoopLabel);
    //         bind(node.incrementor);
    //         addAntecedent(preLoopLabel, currentFlow);
    //         currentFlow = finishFlowLabel(postLoopLabel);
    //     }

    //     function bindForInOrForOfStatement(node: ForInOrOfStatement): void {
    //         const preLoopLabel = setContinueTarget(node, createLoopLabel());
    //         const postLoopLabel = createBranchLabel();
    //         bind(node.expression);
    //         addAntecedent(preLoopLabel, currentFlow);
    //         currentFlow = preLoopLabel;
    //         if (node.kind === SyntaxKind::ForOfStatement) {
    //             bind(node.awaitModifier);
    //         }
    //         addAntecedent(postLoopLabel, currentFlow);
    //         bind(node.initializer);
    //         if (node.initializer.kind !== SyntaxKind::VariableDeclarationList) {
    //             bindAssignmentTargetFlow(node.initializer);
    //         }
    //         bindIterativeStatement(node.statement, postLoopLabel, preLoopLabel);
    //         addAntecedent(preLoopLabel, currentFlow);
    //         currentFlow = finishFlowLabel(postLoopLabel);
    //     }

    //     function bindIfStatement(node: IfStatement): void {
    //         const thenLabel = createBranchLabel();
    //         const elseLabel = createBranchLabel();
    //         const postIfLabel = createBranchLabel();
    //         bindCondition(node.expression, thenLabel, elseLabel);
    //         currentFlow = finishFlowLabel(thenLabel);
    //         bind(node.thenStatement);
    //         addAntecedent(postIfLabel, currentFlow);
    //         currentFlow = finishFlowLabel(elseLabel);
    //         bind(node.elseStatement);
    //         addAntecedent(postIfLabel, currentFlow);
    //         currentFlow = finishFlowLabel(postIfLabel);
    //     }

    //     function bindReturnOrThrow(node: ReturnStatement | ThrowStatement): void {
    //         bind(node.expression);
    //         if (node.kind === SyntaxKind::ReturnStatement) {
    //             hasExplicitReturn = true;
    //             if (currentReturnTarget) {
    //                 addAntecedent(currentReturnTarget, currentFlow);
    //             }
    //         }
    //         currentFlow = unreachableFlow;
    //     }

    //     function findActiveLabel(name: __String) {
    //         for (let label = activeLabelList; label; label = label.next) {
    //             if (label.name === name) {
    //                 return label;
    //             }
    //         }
    //         return undefined;
    //     }

    //     function bindBreakOrContinueFlow(node: BreakOrContinueStatement, breakTarget: FlowLabel | undefined, continueTarget: FlowLabel | undefined) {
    //         const flowLabel = node.kind === SyntaxKind::BreakStatement ? breakTarget : continueTarget;
    //         if (flowLabel) {
    //             addAntecedent(flowLabel, currentFlow);
    //             currentFlow = unreachableFlow;
    //         }
    //     }

    //     function bindBreakOrContinueStatement(node: BreakOrContinueStatement): void {
    //         bind(node.label);
    //         if (node.label) {
    //             const activeLabel = findActiveLabel(node.label.escapedText);
    //             if (activeLabel) {
    //                 activeLabel.referenced = true;
    //                 bindBreakOrContinueFlow(node, activeLabel.breakTarget, activeLabel.continueTarget);
    //             }
    //         }
    //         else {
    //             bindBreakOrContinueFlow(node, currentBreakTarget, currentContinueTarget);
    //         }
    //     }

    //     function bindTryStatement(node: TryStatement): void {
    //         // We conservatively assume that *any* code in the try block can cause an exception, but we only need
    //         // to track code that causes mutations (because only mutations widen the possible control flow type of
    //         // a variable). The exceptionLabel is the target label for control flows that result from exceptions.
    //         // We add all mutation flow nodes as antecedents of this label such that we can analyze them as possible
    //         // antecedents of the start of catch or finally blocks. Furthermore, we add the current control flow to
    //         // represent exceptions that occur before any mutations.
    //         const saveReturnTarget = currentReturnTarget;
    //         const saveExceptionTarget = currentExceptionTarget;
    //         const normalExitLabel = createBranchLabel();
    //         const returnLabel = createBranchLabel();
    //         let exceptionLabel = createBranchLabel();
    //         if (node.finallyBlock) {
    //             currentReturnTarget = returnLabel;
    //         }
    //         addAntecedent(exceptionLabel, currentFlow);
    //         currentExceptionTarget = exceptionLabel;
    //         bind(node.tryBlock);
    //         addAntecedent(normalExitLabel, currentFlow);
    //         if (node.catchClause) {
    //             // Start of catch clause is the target of exceptions from try block.
    //             currentFlow = finishFlowLabel(exceptionLabel);
    //             // The currentExceptionTarget now represents control flows from exceptions in the catch clause.
    //             // Effectively, in a try-catch-finally, if an exception occurs in the try block, the catch block
    //             // acts like a second try block.
    //             exceptionLabel = createBranchLabel();
    //             addAntecedent(exceptionLabel, currentFlow);
    //             currentExceptionTarget = exceptionLabel;
    //             bind(node.catchClause);
    //             addAntecedent(normalExitLabel, currentFlow);
    //         }
    //         currentReturnTarget = saveReturnTarget;
    //         currentExceptionTarget = saveExceptionTarget;
    //         if (node.finallyBlock) {
    //             // Possible ways control can reach the finally block:
    //             // 1) Normal completion of try block of a try-finally or try-catch-finally
    //             // 2) Normal completion of catch block (following exception in try block) of a try-catch-finally
    //             // 3) Return in try or catch block of a try-finally or try-catch-finally
    //             // 4) Exception in try block of a try-finally
    //             // 5) Exception in catch block of a try-catch-finally
    //             // When analyzing a control flow graph that starts inside a finally block we want to consider all
    //             // five possibilities above. However, when analyzing a control flow graph that starts outside (past)
    //             // the finally block, we only want to consider the first two (if we're past a finally block then it
    //             // must have completed normally). Likewise, when analyzing a control flow graph from return statements
    //             // in try or catch blocks in an IIFE, we only want to consider the third. To make this possible, we
    //             // inject a ReduceLabel node into the control flow graph. This node contains an alternate reduced
    //             // set of antecedents for the pre-finally label. As control flow analysis passes by a ReduceLabel
    //             // node, the pre-finally label is temporarily switched to the reduced antecedent set.
    //             const finallyLabel = createBranchLabel();
    //             finallyLabel.antecedents = concatenate(concatenate(normalExitLabel.antecedents, exceptionLabel.antecedents), returnLabel.antecedents);
    //             currentFlow = finallyLabel;
    //             bind(node.finallyBlock);
    //             if (currentFlow.flags & FlowFlags.Unreachable) {
    //                 // If the end of the finally block is unreachable, the end of the entire try statement is unreachable.
    //                 currentFlow = unreachableFlow;
    //             }
    //             else {
    //                 // If we have an IIFE return target and return statements in the try or catch blocks, add a control
    //                 // flow that goes back through the finally block and back through only the return statements.
    //                 if (currentReturnTarget && returnLabel.antecedents) {
    //                     addAntecedent(currentReturnTarget, createReduceLabel(finallyLabel, returnLabel.antecedents, currentFlow));
    //                 }
    //                 // If we have an outer exception target (i.e. a containing try-finally or try-catch-finally), add a
    //                 // control flow that goes back through the finally blok and back through each possible exception source.
    //                 if (currentExceptionTarget && exceptionLabel.antecedents) {
    //                     addAntecedent(currentExceptionTarget, createReduceLabel(finallyLabel, exceptionLabel.antecedents, currentFlow));
    //                 }
    //                 // If the end of the finally block is reachable, but the end of the try and catch blocks are not,
    //                 // convert the current flow to unreachable. For example, 'try { return 1; } finally { ... }' should
    //                 // result in an unreachable current control flow.
    //                 currentFlow = normalExitLabel.antecedents ? createReduceLabel(finallyLabel, normalExitLabel.antecedents, currentFlow) : unreachableFlow;
    //             }
    //         }
    //         else {
    //             currentFlow = finishFlowLabel(normalExitLabel);
    //         }
    //     }

    //     function bindSwitchStatement(node: SwitchStatement): void {
    //         const postSwitchLabel = createBranchLabel();
    //         bind(node.expression);
    //         const saveBreakTarget = currentBreakTarget;
    //         const savePreSwitchCaseFlow = preSwitchCaseFlow;
    //         currentBreakTarget = postSwitchLabel;
    //         preSwitchCaseFlow = currentFlow;
    //         bind(node.caseBlock);
    //         addAntecedent(postSwitchLabel, currentFlow);
    //         const hasDefault = forEach(node.caseBlock.clauses, c => c.kind === SyntaxKind::DefaultClause);
    //         // We mark a switch statement as possibly exhaustive if it has no default clause and if all
    //         // case clauses have unreachable end points (e.g. they all return). Note, we no longer need
    //         // this property in control flow analysis, it's there only for backwards compatibility.
    //         node.possiblyExhaustive = !hasDefault && !postSwitchLabel.antecedents;
    //         if (!hasDefault) {
    //             addAntecedent(postSwitchLabel, createFlowSwitchClause(preSwitchCaseFlow, node, 0, 0));
    //         }
    //         currentBreakTarget = saveBreakTarget;
    //         preSwitchCaseFlow = savePreSwitchCaseFlow;
    //         currentFlow = finishFlowLabel(postSwitchLabel);
    //     }

    //     function bindCaseBlock(node: CaseBlock): void {
    //         const clauses = node.clauses;
    //         const isNarrowingSwitch = isNarrowingExpression(node.parent.expression);
    //         let fallthroughFlow = unreachableFlow;
    //         for (let i = 0; i < clauses.length; i++) {
    //             const clauseStart = i;
    //             while (!clauses[i].statements.length && i + 1 < clauses.length) {
    //                 bind(clauses[i]);
    //                 i++;
    //             }
    //             const preCaseLabel = createBranchLabel();
    //             addAntecedent(preCaseLabel, isNarrowingSwitch ? createFlowSwitchClause(preSwitchCaseFlow!, node.parent, clauseStart, i + 1) : preSwitchCaseFlow!);
    //             addAntecedent(preCaseLabel, fallthroughFlow);
    //             currentFlow = finishFlowLabel(preCaseLabel);
    //             const clause = clauses[i];
    //             bind(clause);
    //             fallthroughFlow = currentFlow;
    //             if (!(currentFlow.flags & FlowFlags.Unreachable) && i !== clauses.length - 1 && options.noFallthroughCasesInSwitch) {
    //                 clause.fallthroughFlowNode = currentFlow;
    //             }
    //         }
    //     }

    //     function bindCaseClause(node: CaseClause): void {
    //         const saveCurrentFlow = currentFlow;
    //         currentFlow = preSwitchCaseFlow!;
    //         bind(node.expression);
    //         currentFlow = saveCurrentFlow;
    //         bindEach(node.statements);
    //     }

    //     function bindExpressionStatement(node: ExpressionStatement): void {
    //         bind(node.expression);
    //         maybeBindExpressionFlowIfCall(node.expression);
    //     }

    //     function maybeBindExpressionFlowIfCall(node: Expression) {
    //         // A top level or LHS of comma expression call expression with a dotted function name and at least one argument
    //         // is potentially an assertion and is therefore included in the control flow.
    //         if (node.kind === SyntaxKind::CallExpression) {
    //             const call = node as CallExpression;
    //             if (call.expression.kind !== SyntaxKind::SuperKeyword && isDottedName(call.expression)) {
    //                 currentFlow = createFlowCall(currentFlow, call);
    //             }
    //         }
    //     }

    //     function bindLabeledStatement(node: LabeledStatement): void {
    //         const postStatementLabel = createBranchLabel();
    //         activeLabelList = {
    //             next: activeLabelList,
    //             name: node.label.escapedText,
    //             breakTarget: postStatementLabel,
    //             continueTarget: undefined,
    //             referenced: false
    //         };
    //         bind(node.label);
    //         bind(node.statement);
    //         if (!activeLabelList.referenced && !options.allowUnusedLabels) {
    //             errorOrSuggestionOnNode(unusedLabelIsError(options), node.label, Diagnostics.Unused_label);
    //         }
    //         activeLabelList = activeLabelList.next;
    //         addAntecedent(postStatementLabel, currentFlow);
    //         currentFlow = finishFlowLabel(postStatementLabel);
    //     }

    //     function bindDestructuringTargetFlow(node: Expression) {
    //         if (node.kind === SyntaxKind::BinaryExpression && (node as BinaryExpression).operatorToken.kind === SyntaxKind::EqualsToken) {
    //             bindAssignmentTargetFlow((node as BinaryExpression).left);
    //         }
    //         else {
    //             bindAssignmentTargetFlow(node);
    //         }
    //     }

    //     function bindAssignmentTargetFlow(node: Expression) {
    //         if (isNarrowableReference(node)) {
    //             currentFlow = createFlowMutation(FlowFlags.Assignment, currentFlow, node);
    //         }
    //         else if (node.kind === SyntaxKind::ArrayLiteralExpression) {
    //             for (const e of (node as ArrayLiteralExpression).elements) {
    //                 if (e.kind === SyntaxKind::SpreadElement) {
    //                     bindAssignmentTargetFlow((e as SpreadElement).expression);
    //                 }
    //                 else {
    //                     bindDestructuringTargetFlow(e);
    //                 }
    //             }
    //         }
    //         else if (node.kind === SyntaxKind::ObjectLiteralExpression) {
    //             for (const p of (node as ObjectLiteralExpression).properties) {
    //                 if (p.kind === SyntaxKind::PropertyAssignment) {
    //                     bindDestructuringTargetFlow(p.initializer);
    //                 }
    //                 else if (p.kind === SyntaxKind::ShorthandPropertyAssignment) {
    //                     bindAssignmentTargetFlow(p.name);
    //                 }
    //                 else if (p.kind === SyntaxKind::SpreadAssignment) {
    //                     bindAssignmentTargetFlow(p.expression);
    //                 }
    //             }
    //         }
    //     }

    //     function bindLogicalLikeExpression(node: BinaryExpression, trueTarget: FlowLabel, falseTarget: FlowLabel) {
    //         const preRightLabel = createBranchLabel();
    //         if (node.operatorToken.kind === SyntaxKind::AmpersandAmpersandToken || node.operatorToken.kind === SyntaxKind::AmpersandAmpersandEqualsToken) {
    //             bindCondition(node.left, preRightLabel, falseTarget);
    //         }
    //         else {
    //             bindCondition(node.left, trueTarget, preRightLabel);
    //         }
    //         currentFlow = finishFlowLabel(preRightLabel);
    //         bind(node.operatorToken);

    //         if (isLogicalOrCoalescingAssignmentOperator(node.operatorToken.kind)) {
    //             doWithConditionalBranches(bind, node.right, trueTarget, falseTarget);
    //             bindAssignmentTargetFlow(node.left);

    //             addAntecedent(trueTarget, createFlowCondition(FlowFlags.TrueCondition, currentFlow, node));
    //             addAntecedent(falseTarget, createFlowCondition(FlowFlags.FalseCondition, currentFlow, node));
    //         }
    //         else {
    //             bindCondition(node.right, trueTarget, falseTarget);
    //         }
    //     }

    //     function bindPrefixUnaryExpressionFlow(node: PrefixUnaryExpression) {
    //         if (node.operator === SyntaxKind::ExclamationToken) {
    //             const saveTrueTarget = currentTrueTarget;
    //             currentTrueTarget = currentFalseTarget;
    //             currentFalseTarget = saveTrueTarget;
    //             bindEachChild(node);
    //             currentFalseTarget = currentTrueTarget;
    //             currentTrueTarget = saveTrueTarget;
    //         }
    //         else {
    //             bindEachChild(node);
    //             if (node.operator === SyntaxKind::PlusPlusToken || node.operator === SyntaxKind::MinusMinusToken) {
    //                 bindAssignmentTargetFlow(node.operand);
    //             }
    //         }
    //     }

    //     function bindPostfixUnaryExpressionFlow(node: PostfixUnaryExpression) {
    //         bindEachChild(node);
    //         if (node.operator === SyntaxKind::PlusPlusToken || node.operator === SyntaxKind::MinusMinusToken) {
    //             bindAssignmentTargetFlow(node.operand);
    //         }
    //     }

    //     function bindDestructuringAssignmentFlow(node: DestructuringAssignment) {
    //         if (inAssignmentPattern) {
    //             inAssignmentPattern = false;
    //             bind(node.operatorToken);
    //             bind(node.right);
    //             inAssignmentPattern = true;
    //             bind(node.left);
    //         }
    //         else {
    //             inAssignmentPattern = true;
    //             bind(node.left);
    //             inAssignmentPattern = false;
    //             bind(node.operatorToken);
    //             bind(node.right);
    //         }
    //         bindAssignmentTargetFlow(node.left);
    //     }

    //     function createBindBinaryExpressionFlow() {
    //         interface WorkArea {
    //             stackIndex: number;
    //             skip: boolean;
    //             inStrictModeStack: (boolean | undefined)[];
    //             parentStack: (Node | undefined)[];
    //         }

    //         return createBinaryExpressionTrampoline(onEnter, onLeft, onOperator, onRight, onExit, /*foldState*/ undefined);

    //         function onEnter(node: BinaryExpression, state: WorkArea | undefined) {
    //             if (state) {
    //                 state.stackIndex++;
    //                 // Emulate the work that `bind` does before reaching `bindChildren`. A normal call to
    //                 // `bindBinaryExpressionFlow` will already have done this work.
    //                 setParent(node, parent);
    //                 const saveInStrictMode = inStrictMode;
    //                 bindWorker(node);
    //                 const saveParent = parent;
    //                 parent = node;
    //                 state.skip = false;
    //                 state.inStrictModeStack[state.stackIndex] = saveInStrictMode;
    //                 state.parentStack[state.stackIndex] = saveParent;
    //             }
    //             else {
    //                 state = {
    //                     stackIndex: 0,
    //                     skip: false,
    //                     inStrictModeStack: [undefined],
    //                     parentStack: [undefined]
    //                 };
    //             }
    //             // TODO: bindLogicalExpression is recursive - if we want to handle deeply nested `&&` expressions
    //             // we'll need to handle the `bindLogicalExpression` scenarios in this state machine, too
    //             // For now, though, since the common cases are chained `+`, leaving it recursive is fine
    //             const operator = node.operatorToken.kind;
    //             if (operator === SyntaxKind::AmpersandAmpersandToken ||
    //                 operator === SyntaxKind::BarBarToken ||
    //                 operator === SyntaxKind::QuestionQuestionToken ||
    //                 isLogicalOrCoalescingAssignmentOperator(operator)) {
    //                 if (isTopLevelLogicalExpression(node)) {
    //                     const postExpressionLabel = createBranchLabel();
    //                     bindLogicalLikeExpression(node, postExpressionLabel, postExpressionLabel);
    //                     currentFlow = finishFlowLabel(postExpressionLabel);
    //                 }
    //                 else {
    //                     bindLogicalLikeExpression(node, currentTrueTarget!, currentFalseTarget!);
    //                 }
    //                 state.skip = true;
    //             }
    //             return state;
    //         }

    //         function onLeft(left: Expression, state: WorkArea, _node: BinaryExpression) {
    //             if (!state.skip) {
    //                 return maybeBind(left);
    //             }
    //         }

    //         function onOperator(operatorToken: BinaryOperatorToken, state: WorkArea, node: BinaryExpression) {
    //             if (!state.skip) {
    //                 if (operatorToken.kind === SyntaxKind::CommaToken) {
    //                     maybeBindExpressionFlowIfCall(node.left);
    //                 }
    //                 bind(operatorToken);
    //             }
    //         }

    //         function onRight(right: Expression, state: WorkArea, _node: BinaryExpression) {
    //             if (!state.skip) {
    //                 return maybeBind(right);
    //             }
    //         }

    //         function onExit(node: BinaryExpression, state: WorkArea) {
    //             if (!state.skip) {
    //                 const operator = node.operatorToken.kind;
    //                 if (isAssignmentOperator(operator) && !isAssignmentTarget(node)) {
    //                     bindAssignmentTargetFlow(node.left);
    //                     if (operator === SyntaxKind::EqualsToken && node.left.kind === SyntaxKind::ElementAccessExpression) {
    //                         const elementAccess = node.left as ElementAccessExpression;
    //                         if (isNarrowableOperand(elementAccess.expression)) {
    //                             currentFlow = createFlowMutation(FlowFlags.ArrayMutation, currentFlow, node);
    //                         }
    //                     }
    //                 }
    //             }
    //             const savedInStrictMode = state.inStrictModeStack[state.stackIndex];
    //             const savedParent = state.parentStack[state.stackIndex];
    //             if (savedInStrictMode !== undefined) {
    //                 inStrictMode = savedInStrictMode;
    //             }
    //             if (savedParent !== undefined) {
    //                 parent = savedParent;
    //             }
    //             state.skip = false;
    //             state.stackIndex--;
    //         }

    //         function maybeBind(node: Node) {
    //             if (node && isBinaryExpression(node) && !isDestructuringAssignment(node)) {
    //                 return node;
    //             }
    //             bind(node);
    //         }
    //     }

    //     function bindDeleteExpressionFlow(node: DeleteExpression) {
    //         bindEachChild(node);
    //         if (node.expression.kind === SyntaxKind::PropertyAccessExpression) {
    //             bindAssignmentTargetFlow(node.expression);
    //         }
    //     }

    //     function bindConditionalExpressionFlow(node: ConditionalExpression) {
    //         const trueLabel = createBranchLabel();
    //         const falseLabel = createBranchLabel();
    //         const postExpressionLabel = createBranchLabel();
    //         bindCondition(node.condition, trueLabel, falseLabel);
    //         currentFlow = finishFlowLabel(trueLabel);
    //         bind(node.questionToken);
    //         bind(node.whenTrue);
    //         addAntecedent(postExpressionLabel, currentFlow);
    //         currentFlow = finishFlowLabel(falseLabel);
    //         bind(node.colonToken);
    //         bind(node.whenFalse);
    //         addAntecedent(postExpressionLabel, currentFlow);
    //         currentFlow = finishFlowLabel(postExpressionLabel);
    //     }

    fn bindInitializedVariableFlow(&mut self, node: &Rc<BoundNode>) {
        let name = match &node.node {
            Node::BindingElement(n) => Some(&n.name),
            Node::OmittedExpression(_) => None,
            Node::VariableDeclaration(n) => Some(&n.name),
            _ => unreachable!(),
        };
        match name {
            Some(BindingName::ArrayBindingPattern(n)) => {
                for child in n.elements.iter() {
                    self.bindInitializedVariableFlow(&child.bind(node));
                }
            }
            Some(BindingName::ObjectBindingPattern(n)) => {
                for child in n.elements.iter() {
                    self.bindInitializedVariableFlow(&child.bind(node));
                }
            }
            _ => {
                self.currentFlow = self.createFlowAssignment(self.currentFlow, node.clone());
            }
        }
    }

    fn bindVariableDeclarationFlow(
        &mut self,
        node: &Rc<BoundNode>,
        decl: &Rc<VariableDeclaration>,
    ) {
        self.bindEachChild(node.parent().as_ref(), node.node.clone());
        if decl.initializer.is_some()
            || isForInOrOfStatement(&node.parent().unwrap().parent().unwrap())
        {
            self.bindInitializedVariableFlow(node);
        }
    }

    //     function bindBindingElementFlow(node: BindingElement) {
    //         if (isBindingPattern(node.name)) {
    //             // When evaluating a binding pattern, the initializer is evaluated before the binding pattern, per:
    //             // - https://tc39.es/ecma262/#sec-destructuring-binding-patterns-runtime-semantics-iteratorbindinginitialization
    //             //   - `BindingElement: BindingPattern Initializer?`
    //             // - https://tc39.es/ecma262/#sec-runtime-semantics-keyedbindinginitialization
    //             //   - `BindingElement: BindingPattern Initializer?`
    //             bindEach(node.decorators);
    //             bindEach(node.modifiers);
    //             bind(node.dotDotDotToken);
    //             bind(node.propertyName);
    //             bind(node.initializer);
    //             bind(node.name);
    //         }
    //         else {
    //             bindEachChild(node);
    //         }
    //     }

    //     function bindJSDocTypeAlias(node: JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag) {
    //         bind(node.tagName);
    //         if (node.kind !== SyntaxKind::JSDocEnumTag && node.fullName) {
    //             // don't bind the type name yet; that's delayed until delayedBindJSDocTypedefTag
    //             setParent(node.fullName, node);
    //             setParentRecursive(node.fullName, /*incremental*/ false);
    //         }
    //         if (typeof node.comment !== "string") {
    //             bindEach(node.comment);
    //         }
    //     }

    //     function bindJSDocClassTag(node: JSDocClassTag) {
    //         bindEachChild(node);
    //         const host = getHostSignatureFromJSDoc(node);
    //         if (host && host.kind !== SyntaxKind::MethodDeclaration) {
    //             addDeclarationToSymbol(host.symbol, host, SymbolFlags::Class);
    //         }
    //     }

    //     function bindOptionalExpression(node: Expression, trueTarget: FlowLabel, falseTarget: FlowLabel) {
    //         doWithConditionalBranches(bind, node, trueTarget, falseTarget);
    //         if (!isOptionalChain(node) || isOutermostOptionalChain(node)) {
    //             addAntecedent(trueTarget, createFlowCondition(FlowFlags.TrueCondition, currentFlow, node));
    //             addAntecedent(falseTarget, createFlowCondition(FlowFlags.FalseCondition, currentFlow, node));
    //         }
    //     }

    //     function bindOptionalChainRest(node: OptionalChain) {
    //         switch (node.kind) {
    //             case SyntaxKind::PropertyAccessExpression:
    //                 bind(node.questionDotToken);
    //                 bind(node.name);
    //                 break;
    //             case SyntaxKind::ElementAccessExpression:
    //                 bind(node.questionDotToken);
    //                 bind(node.argumentExpression);
    //                 break;
    //             case SyntaxKind::CallExpression:
    //                 bind(node.questionDotToken);
    //                 bindEach(node.typeArguments);
    //                 bindEach(node.arguments);
    //                 break;
    //         }
    //     }

    //     function bindOptionalChain(node: OptionalChain, trueTarget: FlowLabel, falseTarget: FlowLabel) {
    //         // For an optional chain, we emulate the behavior of a logical expression:
    //         //
    //         // a?.b         -> a && a.b
    //         // a?.b.c       -> a && a.b.c
    //         // a?.b?.c      -> a && a.b && a.b.c
    //         // a?.[x = 1]   -> a && a[x = 1]
    //         //
    //         // To do this we descend through the chain until we reach the root of a chain (the expression with a `?.`)
    //         // and build it's CFA graph as if it were the first condition (`a && ...`). Then we bind the rest
    //         // of the node as part of the "true" branch, and continue to do so as we ascend back up to the outermost
    //         // chain node. We then treat the entire node as the right side of the expression.
    //         const preChainLabel = isOptionalChainRoot(node) ? createBranchLabel() : undefined;
    //         bindOptionalExpression(node.expression, preChainLabel || trueTarget, falseTarget);
    //         if (preChainLabel) {
    //             currentFlow = finishFlowLabel(preChainLabel);
    //         }
    //         doWithConditionalBranches(bindOptionalChainRest, node, trueTarget, falseTarget);
    //         if (isOutermostOptionalChain(node)) {
    //             addAntecedent(trueTarget, createFlowCondition(FlowFlags.TrueCondition, currentFlow, node));
    //             addAntecedent(falseTarget, createFlowCondition(FlowFlags.FalseCondition, currentFlow, node));
    //         }
    //     }

    //     function bindOptionalChainFlow(node: OptionalChain) {
    //         if (isTopLevelLogicalExpression(node)) {
    //             const postExpressionLabel = createBranchLabel();
    //             bindOptionalChain(node, postExpressionLabel, postExpressionLabel);
    //             currentFlow = finishFlowLabel(postExpressionLabel);
    //         }
    //         else {
    //             bindOptionalChain(node, currentTrueTarget!, currentFalseTarget!);
    //         }
    //     }

    //     function bindNonNullExpressionFlow(node: NonNullExpression | NonNullChain) {
    //         if (isOptionalChain(node)) {
    //             bindOptionalChainFlow(node);
    //         }
    //         else {
    //             bindEachChild(node);
    //         }
    //     }

    //     function bindAccessExpressionFlow(node: AccessExpression | PropertyAccessChain | ElementAccessChain) {
    //         if (isOptionalChain(node)) {
    //             bindOptionalChainFlow(node);
    //         }
    //         else {
    //             bindEachChild(node);
    //         }
    //     }

    //     function bindCallExpressionFlow(node: CallExpression | CallChain) {
    //         if (isOptionalChain(node)) {
    //             bindOptionalChainFlow(node);
    //         }
    //         else {
    //             // If the target of the call expression is a function expression or arrow function we have
    //             // an immediately invoked function expression (IIFE). Initialize the flowNode property to
    //             // the current control flow (which includes evaluation of the IIFE arguments).
    //             const expr = skipParentheses(node.expression);
    //             if (expr.kind === SyntaxKind::FunctionExpression || expr.kind === SyntaxKind::ArrowFunction) {
    //                 bindEach(node.typeArguments);
    //                 bindEach(node.arguments);
    //                 bind(node.expression);
    //             }
    //             else {
    //                 bindEachChild(node);
    //                 if (node.expression.kind === SyntaxKind::SuperKeyword) {
    //                     currentFlow = createFlowCall(currentFlow, node);
    //                 }
    //             }
    //         }
    //         if (node.expression.kind === SyntaxKind::PropertyAccessExpression) {
    //             const propertyAccess = node.expression as PropertyAccessExpression;
    //             if (isIdentifier(propertyAccess.name) && isNarrowableOperand(propertyAccess.expression) && isPushOrUnshiftIdentifier(propertyAccess.name)) {
    //                 currentFlow = createFlowMutation(FlowFlags.ArrayMutation, currentFlow, node);
    //             }
    //         }
    //     }

    fn getContainerFlags(node: &BoundNode) -> ContainerFlags {
        match &node.node {
            // TODO:
            // | Node::JSDocTypeLiteral(_)
            // | Node::JsxAttributes(_)
            Node::ClassExpression(_)
            | Node::ClassDeclaration(_)
            | Node::EnumDeclaration(_)
            | Node::ObjectLiteralExpression(_)
            | Node::TypeLiteralNode(_) => ContainerFlags::IsContainer,

            Node::InterfaceDeclaration(_) => {
                ContainerFlags::IsContainer | ContainerFlags::IsInterface
            }
            Node::ModuleDeclaration(_)
            | Node::TypeAliasDeclaration(_)
            | Node::MappedTypeNode(_) => ContainerFlags::IsContainer | ContainerFlags::HasLocals,
            Node::SourceFile(_) => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
            }
            Node::GetAccessorDeclaration(_)
            | Node::SetAccessorDeclaration(_)
            | Node::MethodDeclaration(_) => {
                if isObjectLiteralOrClassExpressionMethodOrAccessor(node) {
                    return ContainerFlags::IsContainer
                        | ContainerFlags::IsControlFlowContainer
                        | ContainerFlags::HasLocals
                        | ContainerFlags::IsFunctionLike
                        | ContainerFlags::IsObjectLiteralOrClassExpressionMethodOrAccessor;
                }
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike
            }
            // TODO:
            // | Node::JSDocSignature(_)
            Node::ConstructorDeclaration(_)
            | Node::FunctionDeclaration(_)
            | Node::MethodSignature(_)
            | Node::CallSignatureDeclaration(_)
            | Node::JSDocFunctionType(_)
            | Node::FunctionTypeNode(_)
            | Node::ConstructSignatureDeclaration(_)
            | Node::IndexSignatureDeclaration(_)
            | Node::ConstructorTypeNode(_)
            | Node::ClassStaticBlockDeclaration(_) => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike
            }
            Node::FunctionExpression(_) | Node::ArrowFunction(_) => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike
                    | ContainerFlags::IsFunctionExpression
            }
            Node::ModuleBlock(_) => ContainerFlags::IsControlFlowContainer,
            Node::PropertyDeclaration(n) => {
                if n.initializer.is_some() {
                    ContainerFlags::IsControlFlowContainer
                } else {
                    ContainerFlags::None
                }
            }
            Node::CatchClause(_)
            | Node::ForStatement(_)
            | Node::ForInStatement(_)
            | Node::ForOfStatement(_)
            | Node::CaseBlock(_) => ContainerFlags::IsBlockScopedContainer,
            Node::Block(_) => {
                // do not treat blocks directly inside a function as a block-scoped-container.
                // Locals that reside in this block should go to the function locals. Otherwise 'x'
                // would not appear to be a redeclaration of a block scoped local in the following
                // example:
                //
                //      function foo() {
                //          var x;
                //          let x;
                //      }
                //
                // If we placed 'var x' into the function locals and 'let x' into the locals of
                // the block, then there would be no collision.
                //
                // By not creating a new block-scoped-container here, we ensure that both 'var x'
                // and 'let x' go into the Function-container's locals, and we do get a collision
                // conflict.
                if isFunctionLike(node.parent_node().as_ref())
                    || matches!(
                        node.parent_node(),
                        Some(Node::ClassStaticBlockDeclaration(_))
                    )
                {
                    ContainerFlags::None
                } else {
                    ContainerFlags::IsBlockScopedContainer
                }
            }
            _ => ContainerFlags::None,
        }
    }

    fn addToContainerChain(&mut self, next: Rc<BoundNode>) {
        if let Some(last_container) = &self.lastContainer {
            self.store.node_data_mut(last_container).nextContainer = Some(next.clone());
        }

        self.lastContainer = Some(next);
    }

    fn declareSymbolAndAddToSymbolTable(
        &mut self,
        node: &Rc<BoundNode>,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) -> Option<SymbolId> {
        match &self.container.as_ref().unwrap().node {
            // Modules, source files, and classes need specialized handling for how their
            // members are declared (for example, a member of a class will go into a specific
            // symbol table depending on if it is static or not). We defer to specialized
            // handlers to take care of declaring these child members.
            Node::ModuleDeclaration(_) => {
                todo!()
                // self.declareModuleMember(node, symbolFlags, symbolExcludes)
            }
            Node::SourceFile(_) => {
                Some(self.declareSourceFileMember(node, symbolFlags, symbolExcludes))
            }
            Node::ClassExpression(_) | Node::ClassDeclaration(_) => {
                todo!()
                // self.declareClassMember(node, symbolFlags, symbolExcludes)
            }
            Node::EnumDeclaration(_) => {
                todo!()
                // self.declareSymbol(container.symbol.exports, container.symbol, node, symbolFlags, symbolExcludes)
            }
            // TODO: jsdoc
            // Node::JSDocTypeLiteral(_)|
            // TODO: jsx
            // Node::JsxAttributes(_)|
            Node::TypeLiteralNode(_)
            | Node::ObjectLiteralExpression(_)
            | Node::InterfaceDeclaration(_) => {
                // Interface/Object-types always have their children added to the 'members' of
                // their container. They are only accessible through an instance of their
                // container, and are never in scope otherwise (even inside the body of the
                // object / type / interface declaring them). An exception is type parameters,
                // which are in scope without qualification (similar to 'locals').
                todo!()
                // self.declareSymbol(container.symbol.members, container.symbol, node, symbolFlags, symbolExcludes)
            }
            // TODO: jsdoc
            // Node::JSDocSignature(_)|
            // Node::JSDocTypedefTag(_)|
            // Node::JSDocCallbackTag(_)|
            Node::FunctionTypeNode(_)
            | Node::ConstructorTypeNode(_)
            | Node::CallSignatureDeclaration(_)
            | Node::ConstructSignatureDeclaration(_)
            | Node::IndexSignatureDeclaration(_)
            | Node::MethodDeclaration(_)
            | Node::MethodSignature(_)
            | Node::ConstructorDeclaration(_)
            | Node::GetAccessorDeclaration(_)
            | Node::SetAccessorDeclaration(_)
            | Node::FunctionDeclaration(_)
            | Node::FunctionExpression(_)
            | Node::ArrowFunction(_)
            | Node::JSDocFunctionType(_)
            | Node::ClassStaticBlockDeclaration(_)
            | Node::TypeAliasDeclaration(_)
            | Node::MappedTypeNode(_) => {
                // All the children of these container types are never visible through another
                // symbol (i.e. through another symbol's 'exports' or 'members').  Instead,
                // they're only accessed 'lexically' (i.e. from code that exists underneath
                // their container in the tree). To accomplish this, we simply add their declared
                // symbol to the 'locals' of the container.  These symbols can then be found as
                // the type checker walks up the containers, checking them for matching names.
                todo!()
                // self.declareSymbol(container.locals, /*parent*/ undefined, node, symbolFlags, symbolExcludes)
            }
            _ => unreachable!("container must be declaration"),
        }
    }

    //     function declareClassMember(node: Declaration, symbolFlags: SymbolFlags, symbolExcludes: SymbolFlags) {
    //         return isStatic(node)
    //             ? declareSymbol(container.symbol.exports!, container.symbol, node, symbolFlags, symbolExcludes)
    //             : declareSymbol(container.symbol.members!, container.symbol, node, symbolFlags, symbolExcludes);
    //     }

    fn declareSourceFileMember(
        &mut self,
        node: &Rc<BoundNode>,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) -> SymbolId {
        if isExternalModule(&self.file) {
            todo!();
            // self.declareModuleMember(node, symbolFlags, symbolExcludes)
        } else {
            let file = self.file.node_id();
            let symbolTable = self.node_data(file).locals.unwrap();
            self.declareSymbol(
                symbolTable,
                None,
                node,
                symbolFlags,
                symbolExcludes,
                false,
                false,
            )
        }
    }

    //     function hasExportDeclarations(node: ModuleDeclaration | SourceFile): boolean {
    //         const body = isSourceFile(node) ? node : tryCast(node.body, isModuleBlock);
    //         return !!body && body.statements.some(s => isExportDeclaration(s) || isExportAssignment(s));
    //     }

    //     function setExportContextFlag(node: Mutable<ModuleDeclaration | SourceFile>) {
    //         // A declaration source file or ambient module declaration that contains no export declarations (but possibly regular
    //         // declarations with export modifiers) is an export context in which declarations are implicitly exported.
    //         if (node.flags & NodeFlags::Ambient && !hasExportDeclarations(node)) {
    //             node.flags |= NodeFlags::ExportContext;
    //         }
    //         else {
    //             node.flags &= ~NodeFlags::ExportContext;
    //         }
    //     }

    //     function bindModuleDeclaration(node: ModuleDeclaration) {
    //         setExportContextFlag(node);
    //         if (isAmbientModule(node)) {
    //             if (hasSyntacticModifier(node, ModifierFlags.Export)) {
    //                 errorOnFirstToken(node, Diagnostics.export_modifier_cannot_be_applied_to_ambient_modules_and_module_augmentations_since_they_are_always_visible);
    //             }
    //             if (isModuleAugmentationExternal(node)) {
    //                 declareModuleSymbol(node);
    //             }
    //             else {
    //                 let pattern: string | Pattern | undefined;
    //                 if (node.name.kind === SyntaxKind::StringLiteral) {
    //                     const { text } = node.name;
    //                     pattern = tryParsePattern(text);
    //                     if (pattern === undefined) {
    //                         errorOnFirstToken(node.name, Diagnostics.Pattern_0_can_have_at_most_one_Asterisk_character, text);
    //                     }
    //                 }

    //                 const symbol = declareSymbolAndAddToSymbolTable(node, SymbolFlags::ValueModule, SymbolFlags::ValueModuleExcludes)!;
    //                 file.patternAmbientModules = append<PatternAmbientModule>(file.patternAmbientModules, pattern && !isString(pattern) ? { pattern, symbol } : undefined);
    //             }
    //         }
    //         else {
    //             const state = declareModuleSymbol(node);
    //             if (state !== ModuleInstanceState.NonInstantiated) {
    //                 const { symbol } = node;
    //                 // if module was already merged with some function, class or non-const enum, treat it as non-const-enum-only
    //                 symbol.constEnumOnlyModule = (!(symbol.flags & (SymbolFlags::Function | SymbolFlags::Class | SymbolFlags::RegularEnum)))
    //                     // Current must be `const enum` only
    //                     && state === ModuleInstanceState.ConstEnumOnly
    //                     // Can't have been set to 'false' in a previous merged symbol. ('undefined' OK)
    //                     && symbol.constEnumOnlyModule !== false;
    //             }
    //         }
    //     }

    //     function declareModuleSymbol(node: ModuleDeclaration): ModuleInstanceState {
    //         const state = getModuleInstanceState(node);
    //         const instantiated = state !== ModuleInstanceState.NonInstantiated;
    //         declareSymbolAndAddToSymbolTable(node,
    //             instantiated ? SymbolFlags::ValueModule : SymbolFlags::NamespaceModule,
    //             instantiated ? SymbolFlags::ValueModuleExcludes : SymbolFlags::NamespaceModuleExcludes);
    //         return state;
    //     }

    //     function bindFunctionOrConstructorType(node: SignatureDeclaration | JSDocSignature): void {
    //         // For a given function symbol "<...>(...) => T" we want to generate a symbol identical
    //         // to the one we would get for: { <...>(...): T }
    //         //
    //         // We do that by making an anonymous type literal symbol, and then setting the function
    //         // symbol as its sole member. To the rest of the system, this symbol will be indistinguishable
    //         // from an actual type literal symbol you would have gotten had you used the long form.
    //         const symbol = createSymbol(SymbolFlags::Signature, getDeclarationName(node)!); // TODO: GH#18217
    //         addDeclarationToSymbol(symbol, node, SymbolFlags::Signature);

    //         const typeLiteralSymbol = createSymbol(SymbolFlags::TypeLiteral, InternalSymbolName.Type);
    //         addDeclarationToSymbol(typeLiteralSymbol, node, SymbolFlags::TypeLiteral);
    //         typeLiteralSymbol.members = createSymbolTable();
    //         typeLiteralSymbol.members.set(symbol.escapedName, symbol);
    //     }

    //     function bindObjectLiteralExpression(node: ObjectLiteralExpression) {
    //         const enum ElementKind {
    //             Property = 1,
    //             Accessor = 2
    //         }

    //         if (inStrictMode && !isAssignmentTarget(node)) {
    //             const seen = new Map<__String, ElementKind>();

    //             for (const prop of node.properties) {
    //                 if (prop.kind === SyntaxKind::SpreadAssignment || prop.name.kind !== SyntaxKind::Identifier) {
    //                     continue;
    //                 }

    //                 const identifier = prop.name;

    //                 // ECMA-262 11.1.5 Object Initializer
    //                 // If previous is not undefined then throw a SyntaxError exception if any of the following conditions are true
    //                 // a.This production is contained in strict code and IsDataDescriptor(previous) is true and
    //                 // IsDataDescriptor(propId.descriptor) is true.
    //                 //    b.IsDataDescriptor(previous) is true and IsAccessorDescriptor(propId.descriptor) is true.
    //                 //    c.IsAccessorDescriptor(previous) is true and IsDataDescriptor(propId.descriptor) is true.
    //                 //    d.IsAccessorDescriptor(previous) is true and IsAccessorDescriptor(propId.descriptor) is true
    //                 // and either both previous and propId.descriptor have[[Get]] fields or both previous and propId.descriptor have[[Set]] fields
    //                 const currentKind = prop.kind === SyntaxKind::PropertyAssignment || prop.kind === SyntaxKind::ShorthandPropertyAssignment || prop.kind === SyntaxKind::MethodDeclaration
    //                     ? ElementKind.Property
    //                     : ElementKind.Accessor;

    //                 const existingKind = seen.get(identifier.escapedText);
    //                 if (!existingKind) {
    //                     seen.set(identifier.escapedText, currentKind);
    //                     continue;
    //                 }

    //                 if (currentKind === ElementKind.Property && existingKind === ElementKind.Property) {
    //                     const span = getErrorSpanForNode(file, identifier);
    //                     file.bindDiagnostics.push(createFileDiagnostic(file, span.start, span.length,
    //                         Diagnostics.An_object_literal_cannot_have_multiple_properties_with_the_same_name_in_strict_mode));
    //                 }
    //             }
    //         }

    //         return bindAnonymousDeclaration(node, SymbolFlags::ObjectLiteral, InternalSymbolName.Object);
    //     }

    //     function bindJsxAttributes(node: JsxAttributes) {
    //         return bindAnonymousDeclaration(node, SymbolFlags::ObjectLiteral, InternalSymbolName.JSXAttributes);
    //     }

    //     function bindJsxAttribute(node: JsxAttribute, symbolFlags: SymbolFlags, symbolExcludes: SymbolFlags) {
    //         return declareSymbolAndAddToSymbolTable(node, symbolFlags, symbolExcludes);
    //     }

    fn bindAnonymousDeclaration(
        &mut self,
        node: &Rc<BoundNode>,
        symbolFlags: SymbolFlags,
        name: __String,
    ) {
        let symbol = self.createSymbol(symbolFlags, name);
        if symbolFlags.intersects(SymbolFlags::EnumMember | SymbolFlags::ClassMember) {
            let container = self.container.as_ref().unwrap();
            self.symbols[symbol].parent = self.store.node_data(container).symbol;
        }
        self.addDeclarationToSymbol(symbol, node, symbolFlags);
    }

    fn bindBlockScopedDeclaration(
        &mut self,
        node: &Rc<BoundNode>,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) {
        match &self.blockScopeContainer.as_ref().unwrap().node {
            Node::ModuleDeclaration(_) => {
                todo!();
                // self.declareModuleMember(node, symbolFlags, symbolExcludes);
            }
            Node::SourceFile(_) => {
                todo!();
                // if (isExternalOrCommonJsModule(container as SourceFile)) {
                //     self.declareModuleMember(node, symbolFlags, symbolExcludes);
                //     return;
                // }
                // if (!blockScopeContainer.locals) {
                //     blockScopeContainer.locals = createSymbolTable();
                //     self.addToContainerChain(blockScopeContainer);
                // }
                // self.declareSymbol(blockScopeContainer.locals, /*parent*/ undefined, node, symbolFlags, symbolExcludes);
            }
            _ => {
                todo!();
                // if !self.blockScopeContainer.locals {
                //     self.blockScopeContainer.locals = self.createSymbolTable();
                //     self.addToContainerChain(blockScopeContainer);
                // }
                // self.declareSymbol(blockScopeContainer.locals, /*parent*/ undefined, node, symbolFlags, symbolExcludes);
            }
        }
    }

    //     function delayedBindJSDocTypedefTag() {
    //         if (!delayedTypeAliases) {
    //             return;
    //         }
    //         const saveContainer = container;
    //         const saveLastContainer = lastContainer;
    //         const saveBlockScopeContainer = blockScopeContainer;
    //         const saveParent = parent;
    //         const saveCurrentFlow = currentFlow;
    //         for (const typeAlias of delayedTypeAliases) {
    //             const host = typeAlias.parent.parent;
    //             container = findAncestor(host.parent, n => !!(getContainerFlags(n) & ContainerFlags::IsContainer)) || file;
    //             blockScopeContainer = getEnclosingBlockScopeContainer(host) || file;
    //             currentFlow = initFlowNode({ flags: FlowFlags.Start });
    //             parent = typeAlias;
    //             bind(typeAlias.typeExpression);
    //             const declName = getNameOfDeclaration(typeAlias);
    //             if ((isJSDocEnumTag(typeAlias) || !typeAlias.fullName) && declName && isPropertyAccessEntityNameExpression(declName.parent)) {
    //                 // typedef anchored to an A.B.C assignment - we need to bind into B's namespace under name C
    //                 const isTopLevel = isTopLevelNamespaceAssignment(declName.parent);
    //                 if (isTopLevel) {
    //                     bindPotentiallyMissingNamespaces(file.symbol, declName.parent, isTopLevel,
    //                         !!findAncestor(declName, d => isPropertyAccessExpression(d) && d.name.escapedText === "prototype"), /*containerIsClass*/ false);
    //                     const oldContainer = container;
    //                     switch (getAssignmentDeclarationPropertyAccessKind(declName.parent)) {
    //                         case AssignmentDeclarationKind.ExportsProperty:
    //                         case AssignmentDeclarationKind.ModuleExports:
    //                             if (!isExternalOrCommonJsModule(file)) {
    //                                 container = undefined!;
    //                             }
    //                             else {
    //                                 container = file;
    //                             }
    //                             break;
    //                         case AssignmentDeclarationKind.ThisProperty:
    //                             container = declName.parent.expression;
    //                             break;
    //                         case AssignmentDeclarationKind.PrototypeProperty:
    //                             container = (declName.parent.expression as PropertyAccessExpression).name;
    //                             break;
    //                         case AssignmentDeclarationKind.Property:
    //                             container = isExportsOrModuleExportsOrAlias(file, declName.parent.expression) ? file
    //                                 : isPropertyAccessExpression(declName.parent.expression) ? declName.parent.expression.name
    //                                 : declName.parent.expression;
    //                             break;
    //                         case AssignmentDeclarationKind.None:
    //                             return Debug.fail("Shouldn't have detected typedef or enum on non-assignment declaration");
    //                     }
    //                     if (container) {
    //                         declareModuleMember(typeAlias, SymbolFlags::TypeAlias, SymbolFlags::TypeAliasExcludes);
    //                     }
    //                     container = oldContainer;
    //                 }
    //             }
    //             else if (isJSDocEnumTag(typeAlias) || !typeAlias.fullName || typeAlias.fullName.kind === SyntaxKind::Identifier) {
    //                 parent = typeAlias.parent;
    //                 bindBlockScopedDeclaration(typeAlias, SymbolFlags::TypeAlias, SymbolFlags::TypeAliasExcludes);
    //             }
    //             else {
    //                 bind(typeAlias.fullName);
    //             }
    //         }
    //         container = saveContainer;
    //         lastContainer = saveLastContainer;
    //         blockScopeContainer = saveBlockScopeContainer;
    //         parent = saveParent;
    //         currentFlow = saveCurrentFlow;
    //     }

    // The binder visits every node in the syntax tree so it is a convenient place to perform a single localized
    // check for reserved words used as identifiers in strict mode code, as well as `yield` or `await` in
    // [Yield] or [Await] contexts, respectively.
    fn checkContextualIdentifier(&mut self, node: &Rc<BoundNode>, ident: &Rc<Identifier>) {
        // Report error only if there are no parse errors in file
        if self.file.parseDiagnostics.is_empty()
            && !self.node_data(node).flags.intersects(NodeFlags::Ambient)
            && !self.node_data(node).flags.intersects(NodeFlags::JSDoc)
            && !isIdentifierName(node)
        {
            // strict mode identifiers
            if self.inStrictMode
                && ident.originalKeywordKind.is_some()
                && ident.originalKeywordKind.unwrap() >= SyntaxKind::FirstFutureReservedWord
                && ident.originalKeywordKind.unwrap() <= SyntaxKind::LastFutureReservedWord
            {
                todo!();
                // file.bindDiagnostics.push(createDiagnosticForNode(node,
                //     getStrictModeIdentifierMessage(node), declarationNameToString(node)));
            } else if ident.originalKeywordKind == Some(SyntaxKind::AwaitKeyword) {
                if isExternalModule(&self.file) && isInTopLevelContext(node.clone()) {
                    todo!();
                    // file.bindDiagnostics.push(createDiagnosticForNode(node,
                    //     Diagnostics.Identifier_expected_0_is_a_reserved_word_at_the_top_level_of_a_module,
                    //     declarationNameToString(node)));
                } else if self
                    .node_data(ident)
                    .flags
                    .intersects(NodeFlags::AwaitContext)
                {
                    todo!();
                    // file.bindDiagnostics.push(createDiagnosticForNode(node,
                    //     Diagnostics.Identifier_expected_0_is_a_reserved_word_that_cannot_be_used_here,
                    //     declarationNameToString(node)));
                }
            } else if ident.originalKeywordKind == Some(SyntaxKind::YieldKeyword)
                && self
                    .node_data(node)
                    .flags
                    .intersects(NodeFlags::YieldContext)
            {
                todo!();
                // file.bindDiagnostics.push(createDiagnosticForNode(node,
                //     Diagnostics.Identifier_expected_0_is_a_reserved_word_that_cannot_be_used_here,
                //     declarationNameToString(node)));
            }
        }
    }

    //     function getStrictModeIdentifierMessage(node: Node) {
    //         // Provide specialized messages to help the user understand why we think they're in
    //         // strict mode.
    //         if (getContainingClass(node)) {
    //             return Diagnostics.Identifier_expected_0_is_a_reserved_word_in_strict_mode_Class_definitions_are_automatically_in_strict_mode;
    //         }

    //         if (file.externalModuleIndicator) {
    //             return Diagnostics.Identifier_expected_0_is_a_reserved_word_in_strict_mode_Modules_are_automatically_in_strict_mode;
    //         }

    //         return Diagnostics.Identifier_expected_0_is_a_reserved_word_in_strict_mode;
    //     }

    // The binder visits every node, so this is a good place to check for
    // the reserved private name (there is only one)
    fn checkPrivateIdentifier(&mut self, node: &Rc<PrivateIdentifier>) {
        if node.escapedText == "#constructor" {
            todo!();
            // Report error only if there are no parse errors in file
            // if !file.parseDiagnostics.length {
            //     file.bindDiagnostics.push(createDiagnosticForNode(node,
            //         Diagnostics.constructor_is_a_reserved_word, declarationNameToString(node)));
            // }
        }
    }

    fn checkStrictModeBinaryExpression(
        &mut self,
        node: &Rc<BoundNode>,
        expr: &Rc<BinaryExpression>,
    ) {
        if self.inStrictMode
            && isLeftHandSideExpression(expr.left.clone().into())
            && isAssignmentOperator(expr.operatorToken.kind())
        {
            // ECMA 262 (Annex C) The identifier eval or arguments may not appear as the LeftHandSideExpression of an
            // Assignment operator(11.13) or of a PostfixExpression(11.3)
            self.checkStrictModeEvalOrArguments(node, Some(expr.left.clone().into()));
        }
    }

    fn checkStrictModeCatchClause(&mut self, node: &Rc<BoundNode>, clause: &Rc<CatchClause>) {
        // It is a SyntaxError if a TryStatement with a Catch occurs within strict code and the Identifier of the
        // Catch production is eval or arguments
        if self.inStrictMode && clause.variableDeclaration.is_some() {
            if let Some(variableDeclaration) = &clause.variableDeclaration {
                self.checkStrictModeEvalOrArguments(
                    node,
                    Some(variableDeclaration.name.clone().into()),
                );
            }
        }
    }

    fn checkStrictModeDeleteExpression(&mut self, node: &Rc<DeleteExpression>) {
        // Grammar checking
        if self.inStrictMode && node.expression.kind() == SyntaxKind::Identifier {
            todo!();
            // When a delete operator occurs within strict mode code, a SyntaxError is thrown if its
            // UnaryExpression is a direct reference to a variable, function argument, or function name
            // const span = getErrorSpanForNode(file, node.expression);
            // file.bindDiagnostics.push(createFileDiagnostic(file, span.start, span.length, Diagnostics.delete_cannot_be_called_on_an_identifier_in_strict_mode));
        }
    }

    fn isEvalOrArgumentsIdentifier(node: &Rc<Identifier>) -> bool {
        node.escapedText == "eval" || node.escapedText == "arguments"
    }

    fn checkStrictModeEvalOrArguments(&mut self, contextNode: &Rc<BoundNode>, name: Option<Node>) {
        if let Some(Node::Identifier(identifier)) = name {
            if Self::isEvalOrArgumentsIdentifier(&identifier) {
                todo!();
                // // We check first if the name is inside class declaration or class expression; if so give explicit message
                // // otherwise report generic error message.
                // const span = getErrorSpanForNode(file, name);
                // file.bindDiagnostics.push(createFileDiagnostic(file, span.start, span.length,
                //     getStrictModeEvalOrArgumentsMessage(contextNode), idText(identifier)));
            }
        }
    }

    //     function getStrictModeEvalOrArgumentsMessage(node: Node) {
    //         // Provide specialized messages to help the user understand why we think they're in
    //         // strict mode.
    //         if (getContainingClass(node)) {
    //             return Diagnostics.Code_contained_in_a_class_is_evaluated_in_JavaScript_s_strict_mode_which_does_not_allow_this_use_of_0_For_more_information_see_https_Colon_Slash_Slashdeveloper_mozilla_org_Slashen_US_Slashdocs_SlashWeb_SlashJavaScript_SlashReference_SlashStrict_mode;
    //         }

    //         if (file.externalModuleIndicator) {
    //             return Diagnostics.Invalid_use_of_0_Modules_are_automatically_in_strict_mode;
    //         }

    //         return Diagnostics.Invalid_use_of_0_in_strict_mode;
    //     }

    fn checkStrictModeFunctionName(&mut self, node: &Rc<BoundNode>, name: Option<Node>) {
        if self.inStrictMode {
            // It is a SyntaxError if the identifier eval or arguments appears within a FormalParameterList of a strict mode FunctionDeclaration or FunctionExpression (13.1))
            self.checkStrictModeEvalOrArguments(node, name);
        }
    }

    //     function getStrictModeBlockScopeFunctionDeclarationMessage(node: Node) {
    //         // Provide specialized messages to help the user understand why we think they're in
    //         // strict mode.
    //         if (getContainingClass(node)) {
    //             return Diagnostics.Function_declarations_are_not_allowed_inside_blocks_in_strict_mode_when_targeting_ES3_or_ES5_Class_definitions_are_automatically_in_strict_mode;
    //         }

    //         if (file.externalModuleIndicator) {
    //             return Diagnostics.Function_declarations_are_not_allowed_inside_blocks_in_strict_mode_when_targeting_ES3_or_ES5_Modules_are_automatically_in_strict_mode;
    //         }

    //         return Diagnostics.Function_declarations_are_not_allowed_inside_blocks_in_strict_mode_when_targeting_ES3_or_ES5;
    //     }

    fn checkStrictModeFunctionDeclaration(
        &mut self,
        node: &Rc<BoundNode>,
        decl: &Rc<FunctionDeclaration>,
    ) {
        if self.languageVersion < ScriptTarget::ES2015 {
            // Report error if function is not top level function declaration
            let block_scope_container = self.blockScopeContainer.as_ref().unwrap();
            if block_scope_container.kind() != SyntaxKind::SourceFile
                && block_scope_container.kind() != SyntaxKind::ModuleDeclaration
                && !isFunctionLikeOrClassStaticBlockDeclaration(Some(block_scope_container))
            {
                todo!();
                // We check first if the name is inside class declaration or class expression; if so give explicit message
                // otherwise report generic error message.
                // const errorSpan = getErrorSpanForNode(file, node);
                // file.bindDiagnostics.push(createFileDiagnostic(file, errorSpan.start, errorSpan.length,
                //     getStrictModeBlockScopeFunctionDeclarationMessage(node)));
            }
        }
    }

    fn checkStrictModeNumericLiteral(&mut self, node: &Rc<NumericLiteral>) {
        if self.inStrictMode && node.numericLiteralFlags.intersects(TokenFlags::Octal) {
            todo!();
            // file.bindDiagnostics.push(createDiagnosticForNode(node, Diagnostics.Octal_literals_are_not_allowed_in_strict_mode));
        }
    }

    fn checkStrictModePostfixUnaryExpression(
        &mut self,
        node: &Rc<BoundNode>,
        expr: &Rc<PostfixUnaryExpression>,
    ) {
        // Grammar checking
        // The identifier eval or arguments may not appear as the LeftHandSideExpression of an
        // Assignment operator(11.13) or of a PostfixExpression(11.3) or as the UnaryExpression
        // operated upon by a Prefix Increment(11.4.4) or a Prefix Decrement(11.4.5) operator.
        if self.inStrictMode {
            self.checkStrictModeEvalOrArguments(node, Some(expr.operand.clone().into()));
        }
    }

    fn checkStrictModePrefixUnaryExpression(
        &mut self,
        node: &Rc<BoundNode>,
        expr: &Rc<PrefixUnaryExpression>,
    ) {
        // Grammar checking
        if self.inStrictMode {
            if expr.operator == SyntaxKind::PlusPlusToken
                || expr.operator == SyntaxKind::MinusMinusToken
            {
                self.checkStrictModeEvalOrArguments(node, Some(expr.operand.clone().into()));
            }
        }
    }

    fn checkStrictModeWithStatement(&mut self, node: &Rc<WithStatement>) {
        // Grammar checking for withStatement
        if self.inStrictMode {
            todo!();
            // errorOnFirstToken(node, Diagnostics.with_statements_are_not_allowed_in_strict_mode);
        }
    }

    fn checkStrictModeLabeledStatement(
        &mut self,
        node: &Rc<BoundNode>,
        stmt: &Rc<LabeledStatement>,
    ) {
        // Grammar checking for labeledStatement
        if self.inStrictMode && getEmitScriptTarget(&self.options) >= ScriptTarget::ES2015 {
            if isDeclarationStatement(&stmt.statement) || isVariableStatement(&stmt.statement) {
                todo!();
                // errorOnFirstToken(node.label, Diagnostics.A_label_is_not_allowed_here);
            }
        }
    }

    //     function errorOnFirstToken(node: Node, message: DiagnosticMessage, arg0?: any, arg1?: any, arg2?: any) {
    //         const span = getSpanOfTokenAtPosition(file, node.pos);
    //         file.bindDiagnostics.push(createFileDiagnostic(file, span.start, span.length, message, arg0, arg1, arg2));
    //     }

    //     function errorOrSuggestionOnNode(isError: boolean, node: Node, message: DiagnosticMessage): void {
    //         errorOrSuggestionOnRange(isError, node, node, message);
    //     }

    //     function errorOrSuggestionOnRange(isError: boolean, startNode: Node, endNode: Node, message: DiagnosticMessage): void {
    //         addErrorOrSuggestionDiagnostic(isError, { pos: getTokenPosOfNode(startNode, file), end: endNode.end }, message);
    //     }

    //     function addErrorOrSuggestionDiagnostic(isError: boolean, range: TextRange, message: DiagnosticMessage): void {
    //         const diag = createFileDiagnostic(file, range.pos, range.end - range.pos, message);
    //         if (isError) {
    //             file.bindDiagnostics.push(diag);
    //         }
    //         else {
    //             file.bindSuggestionDiagnostics = append(file.bindSuggestionDiagnostics, { ...diag, category: DiagnosticCategory.Suggestion });
    //         }
    //     }

    fn bind(&mut self, node: Option<Rc<BoundNode>>) {
        let node = match node {
            Some(n) => n,
            None => return,
        };
        // setParent(node, parent);
        let saveInStrictMode = self.inStrictMode;

        // Even though in the AST the jsdoc @typedef node belongs to the current node,
        // its symbol might be in the same scope with the current node's symbol. Consider:
        //
        //     /** @typedef {string | number} MyType */
        //     function foo();
        //
        // Here the current node is "foo", which is a container, but the scope of "MyType" should
        // not be inside "foo". Therefore we always bind @typedef before bind the parent node,
        // and skip binding this tag later when binding all the other jsdoc tags.

        // First we bind declaration nodes to a symbol if possible. We'll both create a symbol
        // and then potentially add the symbol to an appropriate symbol table. Possible
        // destination symbol tables are:
        //
        //  1) The 'exports' table of the current container's symbol.
        //  2) The 'members' table of the current container's symbol.
        //  3) The 'locals' table of the current container.
        //
        // However, not all symbols will end up in any of these tables. 'Anonymous' symbols
        // (like TypeLiterals for example) will not be put in any table.
        self.bindWorker(&node);
        // Then we recurse into the children of the node to bind them as well. For certain
        // symbols we do specialized work when we recurse. For example, we'll keep track of
        // the current 'container' node when it changes. This helps us know which symbol table
        // a local should go into for example. Since terminal nodes are known not to have
        // children, as an optimization we don't process those.
        if node.kind() > SyntaxKind::LastToken {
            let saveParent = self.parent.clone();
            self.parent = Some(node.clone());
            let containerFlags = Self::getContainerFlags(&node);
            if containerFlags == ContainerFlags::None {
                self.bindChildren(&node);
            } else {
                self.bindContainer(node, containerFlags);
            }
            self.parent = saveParent;
        } else {
            let saveParent = self.parent.clone();
            if node.kind() == SyntaxKind::EndOfFileToken {
                self.parent = Some(node.clone());
            }
            self.bindJSDoc(&node);
            self.parent = saveParent;
        }
        self.inStrictMode = saveInStrictMode;
    }

    fn bindJSDoc(&mut self, node: &Rc<BoundNode>) {
        if hasJSDocNodes(&node.node) {
            todo!();
            // if (isInJSFile(node)) {
            //     for (const j of node.jsDoc!) {
            //         bind(j);
            //     }
            // }
            // else {
            //     for (const j of node.jsDoc!) {
            //         setParent(j, node);
            //         setParentRecursive(j, /*incremental*/ false);
            //     }
            // }
        }
    }

    fn updateStrictModeStatementList(&mut self, statements: &NodeArray<Statement>) {
        if !self.inStrictMode {
            todo!();
            // for statement in statements.iter() {
            //     if !isPrologueDirective(statement) {
            //         return;
            //     }

            //     if isUseStrictPrologueDirective(statement as ExpressionStatement) {
            //         self.inStrictMode = true;
            //         return;
            //     }
            // }
        }
    }

    //     /// Should be called only on prologue directives (isPrologueDirective(node) should be true)
    //     function isUseStrictPrologueDirective(node: ExpressionStatement): boolean {
    //         const nodeText = getSourceTextOfNodeFromSourceFile(file, node.expression);

    //         // Note: the node text must be exactly "use strict" or 'use strict'.  It is not ok for the
    //         // string to contain unicode escapes (as per ES5).
    //         return nodeText === '"use strict"' || nodeText === "'use strict'";
    //     }

    fn bindWorker(&mut self, node: &Rc<BoundNode>) {
        match &node.node {
            /* Strict mode checks */
            Node::Identifier(n) => {
                // for typedef type names with namespaces, bind the new jsdoc type symbol here
                // because it requires all containing namespaces to be in effect, namely the
                // current "blockScopeContainer" needs to be set to its immediate namespace parent.
                if n.isInJSDocNamespace {
                    let mut parentNode = node.parent();
                    loop {
                        if let Some(parent) = &parentNode {
                            if !isJSDocTypeAlias(parent) {
                                parentNode = parent.parent();
                                continue;
                            }
                        }
                        break;
                    }
                    self.bindBlockScopedDeclaration(
                        &parentNode.unwrap(),
                        SymbolFlags::TypeAlias,
                        SymbolFlags::TypeAliasExcludes,
                    );
                    return;
                }
                // TODO: check self.currentFlow (see below)
                if isExpression(node.node.clone())
                    || self.parent.as_ref().unwrap().kind()
                        == SyntaxKind::ShorthandPropertyAssignment
                {
                    self.node_data_mut(node).flowNode = Some(self.currentFlow);
                }
                // if self.currentFlow && (isExpression(node) || parent.kind == SyntaxKind::ShorthandPropertyAssignment) {
                //     node.flowNode = self.currentFlow;
                // }
                self.checkContextualIdentifier(node, n);
            }
            Node::ThisExpression(_) => {
                // TODO: check currentFlow (see below)
                if isExpression(node.node.clone())
                    || self.parent.as_ref().unwrap().kind()
                        == SyntaxKind::ShorthandPropertyAssignment
                {
                    self.node_data_mut(node).flowNode = Some(self.currentFlow);
                }
                // if (currentFlow && (isExpression(node) || parent.kind == SyntaxKind::ShorthandPropertyAssignment)) {
                //     node.flowNode = currentFlow;
                // }
            }
            Node::QualifiedName(_) => {
                // TODO: check currentFlow (see below)
                if isPartOfTypeQuery(node.clone()) {
                    self.node_data_mut(node).flowNode = Some(self.currentFlow);
                }
                // if (currentFlow && isPartOfTypeQuery(node)) {
                //     node.flowNode = currentFlow;
                // }
            }
            Node::MetaProperty(_) | Node::SuperExpression(_) => {
                self.node_data_mut(node).flowNode = Some(self.currentFlow);
            }
            Node::PrivateIdentifier(n) => {
                self.checkPrivateIdentifier(n);
            }
            Node::PropertyAccessExpression(_) | Node::ElementAccessExpression(_) => {
                todo!();
                // const expr = node as PropertyAccessExpression | ElementAccessExpression;
                // if (currentFlow && isNarrowableReference(expr)) {
                //     expr.flowNode = currentFlow;
                // }
                // if (isSpecialPropertyDeclaration(expr)) {
                //     bindSpecialPropertyDeclaration(expr);
                // }
                // if (isInJSFile(expr) &&
                //     file.commonJsModuleIndicator &&
                //     isModuleExportsAccessExpression(expr) &&
                //     !lookupSymbolForName(blockScopeContainer, "module" as __String)) {
                //     declareSymbol(file.locals!, /*parent*/ undefined, expr.expression,
                //         SymbolFlags::FunctionScopedVariable | SymbolFlags::ModuleExports, SymbolFlags::FunctionScopedVariableExcludes);
                // }
            }
            Node::BinaryExpression(n) => {
                let specialKind =
                    getAssignmentDeclarationKind(self.store.node_and_data(&node.node));
                match specialKind {
                    AssignmentDeclarationKind::ExportsProperty => {
                        todo!();
                        // self.bindExportsPropertyAssignment(node as BindableStaticPropertyAssignmentExpression);
                    }
                    AssignmentDeclarationKind::ModuleExports => {
                        todo!();
                        // self.bindModuleExportsAssignment(node as BindablePropertyAssignmentExpression);
                    }
                    AssignmentDeclarationKind::PrototypeProperty => {
                        todo!();
                        // self.bindPrototypePropertyAssignment((node as BindableStaticPropertyAssignmentExpression).left, node);
                    }
                    AssignmentDeclarationKind::Prototype => {
                        todo!();
                        // self.bindPrototypeAssignment(node as BindableStaticPropertyAssignmentExpression);
                    }
                    AssignmentDeclarationKind::ThisProperty => {
                        todo!();
                        // self.bindThisPropertyAssignment(node as BindablePropertyAssignmentExpression);
                    }
                    AssignmentDeclarationKind::Property => {
                        let expression = match &n.left {
                            Expression::ElementAccessExpression(n) => &n.expression,
                            Expression::PropertyAccessExpression(n) => &n.expression,
                            _ => unreachable!(),
                        };
                        if let LeftHandSideExpression::Identifier(expression) = expression {
                            if isInJSFile(Some(self.store.node_and_data(&node.node))) {
                                let symbol = self.lookupSymbolForName(
                                    &self.blockScopeContainer.as_ref().unwrap().node.clone(),
                                    &expression.escapedText,
                                );
                                let value_declaration = symbol
                                    .and_then(|s| self.symbols[s].valueDeclaration.as_ref())
                                    .map(|n| &n.node);
                                if isThisInitializedDeclaration(value_declaration) {
                                    todo!();
                                    // self.bindThisPropertyAssignment(node as BindablePropertyAssignmentExpression);
                                } else {
                                    todo!();
                                    // self.bindSpecialPropertyAssignment(node as BindablePropertyAssignmentExpression);
                                }
                            } else {
                                todo!();
                                // self.bindSpecialPropertyAssignment(node as BindablePropertyAssignmentExpression);
                            }
                        } else {
                            todo!();
                            // self.bindSpecialPropertyAssignment(node as BindablePropertyAssignmentExpression);
                        }
                    }
                    AssignmentDeclarationKind::None => {
                        // Nothing to do
                    }
                    _ => unreachable!("Unknown binary expression special property assignment kind"),
                }
                self.checkStrictModeBinaryExpression(node, n);
            }
            Node::CatchClause(n) => {
                self.checkStrictModeCatchClause(node, n);
            }
            Node::DeleteExpression(n) => {
                self.checkStrictModeDeleteExpression(n);
            }
            Node::NumericLiteral(n) => {
                self.checkStrictModeNumericLiteral(n);
            }
            Node::PostfixUnaryExpression(n) => {
                self.checkStrictModePostfixUnaryExpression(node, n);
            }
            Node::PrefixUnaryExpression(n) => {
                self.checkStrictModePrefixUnaryExpression(node, n);
            }
            Node::WithStatement(n) => {
                self.checkStrictModeWithStatement(n);
            }
            Node::LabeledStatement(n) => {
                self.checkStrictModeLabeledStatement(node, n);
            }
            Node::ThisTypeNode(_) => {
                self.seenThisKeyword = true;
            }
            Node::TypePredicateNode(_) => {
                // Binding the children will handle everything
            }
            Node::TypeParameterDeclaration(_) => {
                self.bindTypeParameter(node);
            }
            Node::ParameterDeclaration(_) => {
                self.bindParameter(node);
            }
            Node::VariableDeclaration(n) => {
                self.bindVariableDeclaration(node, n);
            }
            Node::BindingElement(n) => {
                self.node_data_mut(n).flowNode = Some(self.currentFlow);
                self.bindBindingElement(node, n);
            }
            Node::PropertyDeclaration(n) => {
                self.bindPropertyWorker(node, n.questionToken.is_some());
            }
            Node::PropertySignature(n) => {
                self.bindPropertyWorker(node, n.questionToken.is_some());
            }
            Node::PropertyAssignment(_) | Node::ShorthandPropertyAssignment(_) => {
                self.bindPropertyOrMethodOrAccessor(
                    node,
                    SymbolFlags::Property,
                    SymbolFlags::PropertyExcludes,
                );
            }
            Node::EnumMember(_) => {
                self.bindPropertyOrMethodOrAccessor(
                    node,
                    SymbolFlags::EnumMember,
                    SymbolFlags::EnumMemberExcludes,
                );
            }

            Node::CallSignatureDeclaration(_)
            | Node::ConstructSignatureDeclaration(_)
            | Node::IndexSignatureDeclaration(_) => {
                self.declareSymbolAndAddToSymbolTable(
                    node,
                    SymbolFlags::Signature,
                    SymbolFlags::None,
                );
            }
            Node::MethodDeclaration(n) => {
                // If this is an ObjectLiteralExpression method, then it sits in the same space
                // as other properties in the object literal.  So we use SymbolFlags::PropertyExcludes
                // so that it will conflict with any other object literal members with the same
                // name.
                self.bindPropertyOrMethodOrAccessor(
                    node,
                    SymbolFlags::Method
                        | (if n.questionToken.is_some() {
                            SymbolFlags::Optional
                        } else {
                            SymbolFlags::None
                        }),
                    if isObjectLiteralMethod(Some(node)) {
                        SymbolFlags::PropertyExcludes
                    } else {
                        SymbolFlags::MethodExcludes
                    },
                );
            }
            Node::MethodSignature(n) => {
                // If this is an ObjectLiteralExpression method, then it sits in the same space
                // as other properties in the object literal.  So we use SymbolFlags::PropertyExcludes
                // so that it will conflict with any other object literal members with the same
                // name.
                self.bindPropertyOrMethodOrAccessor(
                    node,
                    SymbolFlags::Method
                        | (if n.questionToken.is_some() {
                            SymbolFlags::Optional
                        } else {
                            SymbolFlags::None
                        }),
                    if isObjectLiteralMethod(Some(node)) {
                        todo!("unreachable?");
                        // SymbolFlags::PropertyExcludes
                    } else {
                        SymbolFlags::MethodExcludes
                    },
                );
            }
            Node::FunctionDeclaration(n) => {
                self.bindFunctionDeclaration(node, n);
            }
            Node::ConstructorDeclaration(_) => {
                self.declareSymbolAndAddToSymbolTable(
                    node,
                    SymbolFlags::Constructor,
                    SymbolFlags::None,
                );
            }
            Node::GetAccessorDeclaration(_) => {
                self.bindPropertyOrMethodOrAccessor(
                    node,
                    SymbolFlags::GetAccessor,
                    SymbolFlags::GetAccessorExcludes,
                );
            }
            Node::SetAccessorDeclaration(_) => {
                self.bindPropertyOrMethodOrAccessor(
                    node,
                    SymbolFlags::SetAccessor,
                    SymbolFlags::SetAccessorExcludes,
                );
            }
            // TODO: jsdoc
            // | Node::JSDocSignature(_)
            Node::FunctionTypeNode(_)
            | Node::JSDocFunctionType(_)
            | Node::ConstructorTypeNode(_) => {
                todo!();
                // bindFunctionOrConstructorType(node as SignatureDeclaration | JSDocSignature);
            }
            // TODO: jsdoc
            // | Node::JSDocTypeLiteral(_)
            Node::TypeLiteralNode(_) | Node::MappedTypeNode(_) => {
                todo!();
                // bindAnonymousTypeWorker(node as TypeLiteralNode | MappedTypeNode | JSDocTypeLiteral);
            }
            // TODO: jsdoc
            // Node::JSDocClassTag(_) => {
            //     todo!();
            //     // bindJSDocClassTag(node as JSDocClassTag);
            // }
            Node::ObjectLiteralExpression(_) => {
                todo!();
                // bindObjectLiteralExpression(node as ObjectLiteralExpression);
            }
            Node::FunctionExpression(_) | Node::ArrowFunction(_) => {
                self.bindFunctionExpression(node);
            }

            Node::CallExpression(_) => {
                todo!();
                // let assignmentKind = getAssignmentDeclarationKind(node as CallExpression);
                // switch (assignmentKind) {
                //     case AssignmentDeclarationKind.ObjectDefinePropertyValue:
                //         return bindObjectDefinePropertyAssignment(node as BindableObjectDefinePropertyCall);
                //     case AssignmentDeclarationKind.ObjectDefinePropertyExports:
                //         return bindObjectDefinePropertyExport(node as BindableObjectDefinePropertyCall);
                //     case AssignmentDeclarationKind.ObjectDefinePrototypeProperty:
                //         return bindObjectDefinePrototypeProperty(node as BindableObjectDefinePropertyCall);
                //     case AssignmentDeclarationKind.None:
                //         break; // Nothing to do
                //     default:
                //         return Debug.fail("Unknown call expression assignment declaration kind");
                // }
                // if (isInJSFile(node)) {
                //     bindCallExpression(node as CallExpression);
                // }
                // break;
            }
            // Members of classes, interfaces, and modules
            Node::ClassExpression(_) | Node::ClassDeclaration(_) => {
                // All classes are automatically in strict mode in ES6.
                self.inStrictMode = true;
                self.bindClassLikeDeclaration(node);
            }
            Node::InterfaceDeclaration(_) => {
                self.bindBlockScopedDeclaration(
                    node,
                    SymbolFlags::Interface,
                    SymbolFlags::InterfaceExcludes,
                );
            }
            Node::TypeAliasDeclaration(_) => {
                self.bindBlockScopedDeclaration(
                    node,
                    SymbolFlags::TypeAlias,
                    SymbolFlags::TypeAliasExcludes,
                );
            }
            Node::EnumDeclaration(_) => {
                self.bindEnumDeclaration(node);
            }
            Node::ModuleDeclaration(_) => {
                todo!();
                // bindModuleDeclaration(node as ModuleDeclaration);
            }
            // TODO: jsx
            // Jsx-attributes
            // Node::JsxAttributes(_) => {
            //     todo!();
            //     // bindJsxAttributes(node as JsxAttributes);
            // }
            // Node::JsxAttribute(_) => {
            //     todo!();
            //     // bindJsxAttribute(node as JsxAttribute, SymbolFlags::Property, SymbolFlags::PropertyExcludes);
            // }

            // Imports and exports
            Node::ImportEqualsDeclaration(_)
            | Node::NamespaceImport(_)
            | Node::ImportSpecifier(_)
            | Node::ExportSpecifier(_) => {
                self.declareSymbolAndAddToSymbolTable(
                    node,
                    SymbolFlags::Alias,
                    SymbolFlags::AliasExcludes,
                );
            }
            Node::NamespaceExportDeclaration(_) => {
                todo!();
                // bindNamespaceExportDeclaration(node as NamespaceExportDeclaration);
            }
            Node::ImportClause(_) => {
                todo!();
                // bindImportClause(node as ImportClause);
            }
            Node::ExportDeclaration(_) => {
                todo!();
                // bindExportDeclaration(node as ExportDeclaration);
            }
            Node::ExportAssignment(_) => {
                todo!();
                // bindExportAssignment(node as ExportAssignment);
            }
            Node::SourceFile(n) => {
                self.updateStrictModeStatementList(&n.statements);
                self.bindSourceFileIfExternalModule();
            }
            Node::Block(n) => {
                if !isFunctionLikeOrClassStaticBlockDeclaration(node.parent().as_ref()) {
                    return;
                }
                self.updateStrictModeStatementList(&n.statements);
            }
            Node::ModuleBlock(n) => {
                self.updateStrictModeStatementList(&n.statements);
            }
            // TODO: jsdoc
            // Node::JSDocParameterTag(_) => {
            //     todo!();
            //     // if node.parent.kind == SyntaxKind::JSDocSignature {
            //     //     return bindParameter(node as JSDocParameterTag);
            //     // }
            //     // if node.parent.kind != SyntaxKind::JSDocTypeLiteral {
            //     //     break;
            //     // }
            //     // let propTag = node as JSDocPropertyLikeTag;
            //     // let flags = if propTag.isBracketed || propTag.typeExpression && propTag.typeExpression.ty.kind == SyntaxKind::JSDocOptionalType
            //     //     {SymbolFlags::Property | SymbolFlags::Optional }
            //     //     else{SymbolFlags::Property};
            //     // declareSymbolAndAddToSymbolTable(propTag, flags, SymbolFlags::PropertyExcludes);
            // }
            // Node::JSDocPropertyTag(_) => {
            //     todo!();
            //     // let propTag = node as JSDocPropertyLikeTag;
            //     // let flags = if propTag.isBracketed || propTag.typeExpression && propTag.typeExpression.ty.kind == SyntaxKind::JSDocOptionalType
            //     //     {SymbolFlags::Property | SymbolFlags::Optional }
            //     //     else{SymbolFlags::Property};
            //     // declareSymbolAndAddToSymbolTable(propTag, flags, SymbolFlags::PropertyExcludes);
            // }
            // Node::JSDocTypedefTag(_) | Node::JSDocCallbackTag(_) | Node::JSDocEnumTag(_) => {
            //     todo!();
            //     // (delayedTypeAliases || (delayedTypeAliases = [])).push(node as JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag);
            // }
            _ => {}
        }
    }

    fn bindPropertyWorker(&mut self, node: &Rc<BoundNode>, optional: bool) {
        self.bindPropertyOrMethodOrAccessor(
            node,
            SymbolFlags::Property
                | (if optional {
                    SymbolFlags::Optional
                } else {
                    SymbolFlags::None
                }),
            SymbolFlags::PropertyExcludes,
        );
    }

    //     function bindAnonymousTypeWorker(node: TypeLiteralNode | MappedTypeNode | JSDocTypeLiteral) {
    //         return bindAnonymousDeclaration(node as Declaration, SymbolFlags::TypeLiteral, InternalSymbolName.Type);
    //     }

    fn bindSourceFileIfExternalModule(&mut self) {
        // TODO:
        // self.setExportContextFlag(self.file);
        if isExternalModule(self.file.as_ref()) {
            todo!();
            // self.bindSourceFileAsExternalModule();
        } else if isJsonSourceFile(self.file.as_ref()) {
            todo!();
            // self.bindSourceFileAsExternalModule();
            // // Create symbol equivalent for the module.exports = {}
            // const originalSymbol = self.file.symbol;
            // self.declareSymbol(self.file.symbol.exports!, self.file.symbol, self.file, SymbolFlags::Property, SymbolFlags::All);
            // self.file.symbol = originalSymbol;
        }
    }

    //     function bindSourceFileAsExternalModule() {
    //         bindAnonymousDeclaration(file, SymbolFlags::ValueModule, `"${removeFileExtension(file.fileName)}"` as __String);
    //     }

    //     function bindExportAssignment(node: ExportAssignment) {
    //         if (!container.symbol || !container.symbol.exports) {
    //             // Incorrect export assignment in some sort of block construct
    //             bindAnonymousDeclaration(node, SymbolFlags::Value, getDeclarationName(node)!);
    //         }
    //         else {
    //             const flags = exportAssignmentIsAlias(node)
    //                 // An export default clause with an EntityNameExpression or a class expression exports all meanings of that identifier or expression;
    //                 ? SymbolFlags::Alias
    //                 // An export default clause with any other expression exports a value
    //                 : SymbolFlags::Property;
    //             // If there is an `export default x;` alias declaration, can't `export default` anything else.
    //             // (In contrast, you can still have `export default function f() {}` and `export default interface I {}`.)
    //             const symbol = declareSymbol(container.symbol.exports, container.symbol, node, flags, SymbolFlags::All);

    //             if (node.isExportEquals) {
    //                 // Will be an error later, since the module already has other exports. Just make sure this has a valueDeclaration set.
    //                 setValueDeclaration(symbol, node);
    //             }
    //         }
    //     }

    //     function bindNamespaceExportDeclaration(node: NamespaceExportDeclaration) {
    //         if (node.modifiers && node.modifiers.length) {
    //             file.bindDiagnostics.push(createDiagnosticForNode(node, Diagnostics.Modifiers_cannot_appear_here));
    //         }
    //         const diag = !isSourceFile(node.parent) ? Diagnostics.Global_module_exports_may_only_appear_at_top_level
    //             : !isExternalModule(node.parent) ? Diagnostics.Global_module_exports_may_only_appear_in_module_files
    //             : !node.parent.isDeclarationFile ? Diagnostics.Global_module_exports_may_only_appear_in_declaration_files
    //             : undefined;
    //         if (diag) {
    //             file.bindDiagnostics.push(createDiagnosticForNode(node, diag));
    //         }
    //         else {
    //             file.symbol.globalExports = file.symbol.globalExports || createSymbolTable();
    //             declareSymbol(file.symbol.globalExports, file.symbol, node, SymbolFlags::Alias, SymbolFlags::AliasExcludes);
    //         }
    //     }

    //     function bindExportDeclaration(node: ExportDeclaration) {
    //         if (!container.symbol || !container.symbol.exports) {
    //             // Export * in some sort of block construct
    //             bindAnonymousDeclaration(node, SymbolFlags::ExportStar, getDeclarationName(node)!);
    //         }
    //         else if (!node.exportClause) {
    //             // All export * declarations are collected in an __export symbol
    //             declareSymbol(container.symbol.exports, container.symbol, node, SymbolFlags::ExportStar, SymbolFlags::None);
    //         }
    //         else if (isNamespaceExport(node.exportClause)) {
    //             // declareSymbol walks up parents to find name text, parent _must_ be set
    //             // but won't be set by the normal binder walk until `bindChildren` later on.
    //             setParent(node.exportClause, node);
    //             declareSymbol(container.symbol.exports, container.symbol, node.exportClause, SymbolFlags::Alias, SymbolFlags::AliasExcludes);
    //         }
    //     }

    //     function bindImportClause(node: ImportClause) {
    //         if (node.name) {
    //             declareSymbolAndAddToSymbolTable(node, SymbolFlags::Alias, SymbolFlags::AliasExcludes);
    //         }
    //     }

    //     function setCommonJsModuleIndicator(node: Node) {
    //         if (file.externalModuleIndicator) {
    //             return false;
    //         }
    //         if (!file.commonJsModuleIndicator) {
    //             file.commonJsModuleIndicator = node;
    //             bindSourceFileAsExternalModule();
    //         }
    //         return true;
    //     }

    //     function bindObjectDefinePropertyExport(node: BindableObjectDefinePropertyCall) {
    //         if (!setCommonJsModuleIndicator(node)) {
    //             return;
    //         }
    //         const symbol = forEachIdentifierInEntityName(node.arguments[0], /*parent*/ undefined, (id, symbol) => {
    //             if (symbol) {
    //                 addDeclarationToSymbol(symbol, id, SymbolFlags::Module | SymbolFlags::Assignment);
    //             }
    //             return symbol;
    //         });
    //         if (symbol) {
    //             const flags = SymbolFlags::Property | SymbolFlags::ExportValue;
    //             declareSymbol(symbol.exports!, symbol, node, flags, SymbolFlags::None);
    //         }
    //     }

    //     function bindExportsPropertyAssignment(node: BindableStaticPropertyAssignmentExpression) {
    //         // When we create a property via 'exports.foo = bar', the 'exports.foo' property access
    //         // expression is the declaration
    //         if (!setCommonJsModuleIndicator(node)) {
    //             return;
    //         }
    //         const symbol = forEachIdentifierInEntityName(node.left.expression, /*parent*/ undefined, (id, symbol) => {
    //             if (symbol) {
    //                 addDeclarationToSymbol(symbol, id, SymbolFlags::Module | SymbolFlags::Assignment);
    //             }
    //             return symbol;
    //         });
    //         if (symbol) {
    //             const isAlias = isAliasableExpression(node.right) && (isExportsIdentifier(node.left.expression) || isModuleExportsAccessExpression(node.left.expression));
    //             const flags = isAlias ? SymbolFlags::Alias : SymbolFlags::Property | SymbolFlags::ExportValue;
    //             setParent(node.left, node);
    //             declareSymbol(symbol.exports!, symbol, node.left, flags, SymbolFlags::None);
    //         }
    //     }

    //     function bindModuleExportsAssignment(node: BindablePropertyAssignmentExpression) {
    //         // A common practice in node modules is to set 'export = module.exports = {}', this ensures that 'exports'
    //         // is still pointing to 'module.exports'.
    //         // We do not want to consider this as 'export=' since a module can have only one of these.
    //         // Similarly we do not want to treat 'module.exports = exports' as an 'export='.
    //         if (!setCommonJsModuleIndicator(node)) {
    //             return;
    //         }
    //         const assignedExpression = getRightMostAssignedExpression(node.right);
    //         if (isEmptyObjectLiteral(assignedExpression) || container === file && isExportsOrModuleExportsOrAlias(file, assignedExpression)) {
    //             return;
    //         }

    //         if (isObjectLiteralExpression(assignedExpression) && every(assignedExpression.properties, isShorthandPropertyAssignment)) {
    //             forEach(assignedExpression.properties, bindExportAssignedObjectMemberAlias);
    //             return;
    //         }

    //         // 'module.exports = expr' assignment
    //         const flags = exportAssignmentIsAlias(node)
    //             ? SymbolFlags::Alias // An export= with an EntityNameExpression or a ClassExpression exports all meanings of that identifier or class
    //             : SymbolFlags::Property | SymbolFlags::ExportValue | SymbolFlags::ValueModule;
    //         const symbol = declareSymbol(file.symbol.exports!, file.symbol, node, flags | SymbolFlags::Assignment, SymbolFlags::None);
    //         setValueDeclaration(symbol, node);
    //     }

    //     function bindExportAssignedObjectMemberAlias(node: ShorthandPropertyAssignment) {
    //         declareSymbol(file.symbol.exports!, file.symbol, node, SymbolFlags::Alias | SymbolFlags::Assignment, SymbolFlags::None);
    //     }

    //     function bindThisPropertyAssignment(node: BindablePropertyAssignmentExpression | PropertyAccessExpression | LiteralLikeElementAccessExpression) {
    //         Debug.assert(isInJSFile(node));
    //         // private identifiers *must* be declared (even in JS files)
    //         const hasPrivateIdentifier = (isBinaryExpression(node) && isPropertyAccessExpression(node.left) && isPrivateIdentifier(node.left.name))
    //             || (isPropertyAccessExpression(node) && isPrivateIdentifier(node.name));
    //         if (hasPrivateIdentifier) {
    //             return;
    //         }
    //         const thisContainer = getThisContainer(node, /*includeArrowFunctions*/ false);
    //         switch (thisContainer.kind) {
    //             case SyntaxKind::FunctionDeclaration:
    //             case SyntaxKind::FunctionExpression:
    //                 let constructorSymbol: Symbol | undefined = thisContainer.symbol;
    //                 // For `f.prototype.m = function() { this.x = 0; }`, `this.x = 0` should modify `f`'s members, not the function expression.
    //                 if (isBinaryExpression(thisContainer.parent) && thisContainer.parent.operatorToken.kind === SyntaxKind::EqualsToken) {
    //                     const l = thisContainer.parent.left;
    //                     if (isBindableStaticAccessExpression(l) && isPrototypeAccess(l.expression)) {
    //                         constructorSymbol = lookupSymbolForPropertyAccess(l.expression.expression, thisParentContainer);
    //                     }
    //                 }

    //                 if (constructorSymbol && constructorSymbol.valueDeclaration) {
    //                     // Declare a 'member' if the container is an ES5 class or ES6 constructor
    //                     constructorSymbol.members = constructorSymbol.members || createSymbolTable();
    //                     // It's acceptable for multiple 'this' assignments of the same identifier to occur
    //                     if (hasDynamicName(node)) {
    //                         bindDynamicallyNamedThisPropertyAssignment(node, constructorSymbol, constructorSymbol.members);
    //                     }
    //                     else {
    //                         declareSymbol(constructorSymbol.members, constructorSymbol, node, SymbolFlags::Property | SymbolFlags::Assignment, SymbolFlags::PropertyExcludes & ~SymbolFlags::Property);
    //                     }
    //                     addDeclarationToSymbol(constructorSymbol, constructorSymbol.valueDeclaration, SymbolFlags::Class);
    //                 }
    //                 break;

    //             case SyntaxKind::Constructor:
    //             case SyntaxKind::PropertyDeclaration:
    //             case SyntaxKind::MethodDeclaration:
    //             case SyntaxKind::GetAccessor:
    //             case SyntaxKind::SetAccessor:
    //                 // this.foo assignment in a JavaScript class
    //                 // Bind this property to the containing class
    //                 const containingClass = thisContainer.parent;
    //                 const symbolTable = isStatic(thisContainer) ? containingClass.symbol.exports! : containingClass.symbol.members!;
    //                 if (hasDynamicName(node)) {
    //                     bindDynamicallyNamedThisPropertyAssignment(node, containingClass.symbol, symbolTable);
    //                 }
    //                 else {
    //                     declareSymbol(symbolTable, containingClass.symbol, node, SymbolFlags::Property | SymbolFlags::Assignment, SymbolFlags::None, /*isReplaceableByMethod*/ true);
    //                 }
    //                 break;
    //             case SyntaxKind::SourceFile:
    //                 // this.property = assignment in a source file -- declare symbol in exports for a module, in locals for a script
    //                 if (hasDynamicName(node)) {
    //                     break;
    //                 }
    //                 else if ((thisContainer as SourceFile).commonJsModuleIndicator) {
    //                     declareSymbol(thisContainer.symbol.exports!, thisContainer.symbol, node, SymbolFlags::Property | SymbolFlags::ExportValue, SymbolFlags::None);
    //                 }
    //                 else {
    //                     declareSymbolAndAddToSymbolTable(node, SymbolFlags::FunctionScopedVariable, SymbolFlags::FunctionScopedVariableExcludes);
    //                 }
    //                 break;

    //             default:
    //                 Debug.failBadSyntaxKind(thisContainer);
    //         }
    //     }

    //     function bindDynamicallyNamedThisPropertyAssignment(node: BinaryExpression | DynamicNamedDeclaration, symbol: Symbol, symbolTable: SymbolTable) {
    //         declareSymbol(symbolTable, symbol, node, SymbolFlags::Property, SymbolFlags::None, /*isReplaceableByMethod*/ true, /*isComputedName*/ true);
    //         addLateBoundAssignmentDeclarationToSymbol(node, symbol);
    //     }

    //     function addLateBoundAssignmentDeclarationToSymbol(node: BinaryExpression | DynamicNamedDeclaration, symbol: Symbol | undefined) {
    //         if (symbol) {
    //             (symbol.assignmentDeclarationMembers || (symbol.assignmentDeclarationMembers = new Map())).set(getNodeId(node), node);
    //         }
    //     }

    //     function bindSpecialPropertyDeclaration(node: PropertyAccessExpression | LiteralLikeElementAccessExpression) {
    //         if (node.expression.kind === SyntaxKind::ThisKeyword) {
    //             bindThisPropertyAssignment(node);
    //         }
    //         else if (isBindableStaticAccessExpression(node) && node.parent.parent.kind === SyntaxKind::SourceFile) {
    //             if (isPrototypeAccess(node.expression)) {
    //                 bindPrototypePropertyAssignment(node, node.parent);
    //             }
    //             else {
    //                 bindStaticPropertyAssignment(node);
    //             }
    //         }
    //     }

    //     /** For `x.prototype = { p, ... }`, declare members p,... if `x` is function/class/{}, or not declared. */
    //     function bindPrototypeAssignment(node: BindableStaticPropertyAssignmentExpression) {
    //         setParent(node.left, node);
    //         setParent(node.right, node);
    //         bindPropertyAssignment(node.left.expression, node.left, /*isPrototypeProperty*/ false, /*containerIsClass*/ true);
    //     }

    //     function bindObjectDefinePrototypeProperty(node: BindableObjectDefinePropertyCall) {
    //         const namespaceSymbol = lookupSymbolForPropertyAccess((node.arguments[0] as PropertyAccessExpression).expression as EntityNameExpression);
    //         if (namespaceSymbol && namespaceSymbol.valueDeclaration) {
    //             // Ensure the namespace symbol becomes class-like
    //             addDeclarationToSymbol(namespaceSymbol, namespaceSymbol.valueDeclaration, SymbolFlags::Class);
    //         }
    //         bindPotentiallyNewExpandoMemberToNamespace(node, namespaceSymbol, /*isPrototypeProperty*/ true);
    //     }

    //     /**
    //      * For `x.prototype.y = z`, declare a member `y` on `x` if `x` is a function or class, or not declared.
    //      * Note that jsdoc preceding an ExpressionStatement like `x.prototype.y;` is also treated as a declaration.
    //      */
    //     function bindPrototypePropertyAssignment(lhs: BindableStaticAccessExpression, parent: Node) {
    //         // Look up the function in the local scope, since prototype assignments should
    //         // follow the function declaration
    //         const classPrototype = lhs.expression as BindableStaticAccessExpression;
    //         const constructorFunction = classPrototype.expression;

    //         // Fix up parent pointers since we're going to use these nodes before we bind into them
    //         setParent(constructorFunction, classPrototype);
    //         setParent(classPrototype, lhs);
    //         setParent(lhs, parent);

    //         bindPropertyAssignment(constructorFunction, lhs, /*isPrototypeProperty*/ true, /*containerIsClass*/ true);
    //     }

    //     function bindObjectDefinePropertyAssignment(node: BindableObjectDefinePropertyCall) {
    //         let namespaceSymbol = lookupSymbolForPropertyAccess(node.arguments[0]);
    //         const isToplevel = node.parent.parent.kind === SyntaxKind::SourceFile;
    //         namespaceSymbol = bindPotentiallyMissingNamespaces(namespaceSymbol, node.arguments[0], isToplevel, /*isPrototypeProperty*/ false, /*containerIsClass*/ false);
    //         bindPotentiallyNewExpandoMemberToNamespace(node, namespaceSymbol, /*isPrototypeProperty*/ false);
    //     }

    //     function bindSpecialPropertyAssignment(node: BindablePropertyAssignmentExpression) {
    //         // Class declarations in Typescript do not allow property declarations
    //         const parentSymbol = lookupSymbolForPropertyAccess(node.left.expression, container) || lookupSymbolForPropertyAccess(node.left.expression, blockScopeContainer) ;
    //         if (!isInJSFile(node) && !isFunctionSymbol(parentSymbol)) {
    //             return;
    //         }
    //         const rootExpr = getLeftmostAccessExpression(node.left);
    //         if (isIdentifier(rootExpr) && lookupSymbolForName(container, rootExpr.escapedText)!?.flags & SymbolFlags::Alias) {
    //             return;
    //         }
    //         // Fix up parent pointers since we're going to use these nodes before we bind into them
    //         setParent(node.left, node);
    //         setParent(node.right, node);
    //         if (isIdentifier(node.left.expression) && container === file && isExportsOrModuleExportsOrAlias(file, node.left.expression)) {
    //             // This can be an alias for the 'exports' or 'module.exports' names, e.g.
    //             //    var util = module.exports;
    //             //    util.property = function ...
    //             bindExportsPropertyAssignment(node as BindableStaticPropertyAssignmentExpression);
    //         }
    //         else if (hasDynamicName(node)) {
    //             bindAnonymousDeclaration(node, SymbolFlags::Property | SymbolFlags::Assignment, InternalSymbolName.Computed);
    //             const sym = bindPotentiallyMissingNamespaces(parentSymbol, node.left.expression, isTopLevelNamespaceAssignment(node.left), /*isPrototype*/ false, /*containerIsClass*/ false);
    //             addLateBoundAssignmentDeclarationToSymbol(node, sym);
    //         }
    //         else {
    //             bindStaticPropertyAssignment(cast(node.left, isBindableStaticNameExpression));
    //         }
    //     }

    //     /**
    //      * For nodes like `x.y = z`, declare a member 'y' on 'x' if x is a function (or IIFE) or class or {}, or not declared.
    //      * Also works for expression statements preceded by JSDoc, like / ** @type number * / x.y;
    //      */
    //     function bindStaticPropertyAssignment(node: BindableStaticNameExpression) {
    //         Debug.assert(!isIdentifier(node));
    //         setParent(node.expression, node);
    //         bindPropertyAssignment(node.expression, node, /*isPrototypeProperty*/ false, /*containerIsClass*/ false);
    //     }

    //     function bindPotentiallyMissingNamespaces(namespaceSymbol: Symbol | undefined, entityName: BindableStaticNameExpression, isToplevel: boolean, isPrototypeProperty: boolean, containerIsClass: boolean) {
    //         if (namespaceSymbol?.flags! & SymbolFlags::Alias) {
    //             return namespaceSymbol;
    //         }
    //         if (isToplevel && !isPrototypeProperty) {
    //             // make symbols or add declarations for intermediate containers
    //             const flags = SymbolFlags::Module | SymbolFlags::Assignment;
    //             const excludeFlags = SymbolFlags::ValueModuleExcludes & ~SymbolFlags::Assignment;
    //             namespaceSymbol = forEachIdentifierInEntityName(entityName, namespaceSymbol, (id, symbol, parent) => {
    //                 if (symbol) {
    //                     addDeclarationToSymbol(symbol, id, flags);
    //                     return symbol;
    //                 }
    //                 else {
    //                     const table = parent ? parent.exports! :
    //                         file.jsGlobalAugmentations || (file.jsGlobalAugmentations = createSymbolTable());
    //                     return declareSymbol(table, parent, id, flags, excludeFlags);
    //                 }
    //             });
    //         }
    //         if (containerIsClass && namespaceSymbol && namespaceSymbol.valueDeclaration) {
    //             addDeclarationToSymbol(namespaceSymbol, namespaceSymbol.valueDeclaration, SymbolFlags::Class);
    //         }
    //         return namespaceSymbol;
    //     }

    //     function bindPotentiallyNewExpandoMemberToNamespace(declaration: BindableStaticAccessExpression | CallExpression, namespaceSymbol: Symbol | undefined, isPrototypeProperty: boolean) {
    //         if (!namespaceSymbol || !isExpandoSymbol(namespaceSymbol)) {
    //             return;
    //         }

    //         // Set up the members collection if it doesn't exist already
    //         const symbolTable = isPrototypeProperty ?
    //             (namespaceSymbol.members || (namespaceSymbol.members = createSymbolTable())) :
    //             (namespaceSymbol.exports || (namespaceSymbol.exports = createSymbolTable()));

    //         let includes = SymbolFlags::None;
    //         let excludes = SymbolFlags::None;
    //         // Method-like
    //         if (isFunctionLikeDeclaration(getAssignedExpandoInitializer(declaration)!)) {
    //             includes = SymbolFlags::Method;
    //             excludes = SymbolFlags::MethodExcludes;
    //         }
    //         // Maybe accessor-like
    //         else if (isCallExpression(declaration) && isBindableObjectDefinePropertyCall(declaration)) {
    //             if (some(declaration.arguments[2].properties, p => {
    //                 const id = getNameOfDeclaration(p);
    //                 return !!id && isIdentifier(id) && idText(id) === "set";
    //             })) {
    //                 // We mix in `SymbolFLags.Property` so in the checker `getTypeOfVariableParameterOrProperty` is used for this
    //                 // symbol, instead of `getTypeOfAccessor` (which will assert as there is no real accessor declaration)
    //                 includes |= SymbolFlags::SetAccessor | SymbolFlags::Property;
    //                 excludes |= SymbolFlags::SetAccessorExcludes;
    //             }
    //             if (some(declaration.arguments[2].properties, p => {
    //                 const id = getNameOfDeclaration(p);
    //                 return !!id && isIdentifier(id) && idText(id) === "get";
    //             })) {
    //                 includes |= SymbolFlags::GetAccessor | SymbolFlags::Property;
    //                 excludes |= SymbolFlags::GetAccessorExcludes;
    //             }
    //         }

    //         if (includes === SymbolFlags::None) {
    //             includes = SymbolFlags::Property;
    //             excludes = SymbolFlags::PropertyExcludes;
    //         }

    //         declareSymbol(symbolTable, namespaceSymbol, declaration, includes | SymbolFlags::Assignment, excludes & ~SymbolFlags::Assignment);
    //     }

    //     function isTopLevelNamespaceAssignment(propertyAccess: BindableAccessExpression) {
    //         return isBinaryExpression(propertyAccess.parent)
    //             ? getParentOfBinaryExpression(propertyAccess.parent).parent.kind === SyntaxKind::SourceFile
    //             : propertyAccess.parent.parent.kind === SyntaxKind::SourceFile;
    //     }

    //     function bindPropertyAssignment(name: BindableStaticNameExpression, propertyAccess: BindableStaticAccessExpression, isPrototypeProperty: boolean, containerIsClass: boolean) {
    //         let namespaceSymbol = lookupSymbolForPropertyAccess(name, container) || lookupSymbolForPropertyAccess(name, blockScopeContainer);
    //         const isToplevel = isTopLevelNamespaceAssignment(propertyAccess);
    //         namespaceSymbol = bindPotentiallyMissingNamespaces(namespaceSymbol, propertyAccess.expression, isToplevel, isPrototypeProperty, containerIsClass);
    //         bindPotentiallyNewExpandoMemberToNamespace(propertyAccess, namespaceSymbol, isPrototypeProperty);
    //     }

    //     /**
    //      * Javascript expando values are:
    //      * - Functions
    //      * - classes
    //      * - namespaces
    //      * - variables initialized with function expressions
    //      * -                       with class expressions
    //      * -                       with empty object literals
    //      * -                       with non-empty object literals if assigned to the prototype property
    //      */
    //     function isExpandoSymbol(symbol: Symbol): boolean {
    //         if (symbol.flags & (SymbolFlags::Function | SymbolFlags::Class | SymbolFlags::NamespaceModule)) {
    //             return true;
    //         }
    //         const node = symbol.valueDeclaration;
    //         if (node && isCallExpression(node)) {
    //             return !!getAssignedExpandoInitializer(node);
    //         }
    //         let init = !node ? undefined :
    //             isVariableDeclaration(node) ? node.initializer :
    //             isBinaryExpression(node) ? node.right :
    //             isPropertyAccessExpression(node) && isBinaryExpression(node.parent) ? node.parent.right :
    //             undefined;
    //         init = init && getRightMostAssignedExpression(init);
    //         if (init) {
    //             const isPrototypeAssignment = isPrototypeAccess(isVariableDeclaration(node!) ? node.name : isBinaryExpression(node!) ? node.left : node!);
    //             return !!getExpandoInitializer(isBinaryExpression(init) && (init.operatorToken.kind === SyntaxKind::BarBarToken || init.operatorToken.kind === SyntaxKind::QuestionQuestionToken) ? init.right : init, isPrototypeAssignment);
    //         }
    //         return false;
    //     }

    //     function getParentOfBinaryExpression(expr: Node) {
    //         while (isBinaryExpression(expr.parent)) {
    //             expr = expr.parent;
    //         }
    //         return expr.parent;
    //     }

    //     function lookupSymbolForPropertyAccess(node: BindableStaticNameExpression, lookupContainer: Node = container): Symbol | undefined {
    //         if (isIdentifier(node)) {
    //             return lookupSymbolForName(lookupContainer, node.escapedText);
    //         }
    //         else {
    //             const symbol = lookupSymbolForPropertyAccess(node.expression);
    //             return symbol && symbol.exports && symbol.exports.get(getElementOrPropertyAccessName(node));
    //         }
    //     }

    //     function forEachIdentifierInEntityName(e: BindableStaticNameExpression, parent: Symbol | undefined, action: (e: Declaration, symbol: Symbol | undefined, parent: Symbol | undefined) => Symbol | undefined): Symbol | undefined {
    //         if (isExportsOrModuleExportsOrAlias(file, e)) {
    //             return file.symbol;
    //         }
    //         else if (isIdentifier(e)) {
    //             return action(e, lookupSymbolForPropertyAccess(e), parent);
    //         }
    //         else {
    //             const s = forEachIdentifierInEntityName(e.expression, parent, action);
    //             const name = getNameOrArgument(e);
    //             // unreachable
    //             if (isPrivateIdentifier(name)) {
    //                 Debug.fail("unexpected PrivateIdentifier");
    //             }
    //             return action(name, s && s.exports && s.exports.get(getElementOrPropertyAccessName(e)), s);
    //         }
    //     }

    //     function bindCallExpression(node: CallExpression) {
    //         // We're only inspecting call expressions to detect CommonJS modules, so we can skip
    //         // this check if we've already seen the module indicator
    //         if (!file.commonJsModuleIndicator && isRequireCall(node, /*checkArgumentIsStringLiteralLike*/ false)) {
    //             setCommonJsModuleIndicator(node);
    //         }
    //     }

    fn bindClassLikeDeclaration(&mut self, node: &Rc<BoundNode>) {
        match &node.node {
            Node::ClassDeclaration(_) => {
                self.bindBlockScopedDeclaration(
                    node,
                    SymbolFlags::Class,
                    SymbolFlags::ClassExcludes,
                );
            }
            Node::ClassExpression(n) => {
                let bindingName = if let Some(name) = &n.name {
                    name.escapedText.clone()
                } else {
                    __String(InternalSymbolName::Class.into())
                };
                self.bindAnonymousDeclaration(node, SymbolFlags::Class, bindingName);
                // TODO:
                // Add name of class expression into the map for semantic classifier
                // if (node.name) {
                //     classifiableNames.add(node.name.escapedText);
                // }
            }
            _ => unreachable!(),
        }

        let symbol = self.node_data(node).symbol.unwrap();

        // TypeScript 1.0 spec (April 2014): 8.4
        // Every class automatically contains a static property member named 'prototype', the
        // type of which is an instantiation of the class type with type Any supplied as a type
        // argument for each type parameter. It is an error to explicitly declare a static
        // property member with the name 'prototype'.
        //
        // Note: we check for this here because this class may be merging into a module.  The
        // module might have an exported variable called 'prototype'.  We can't allow that as
        // that would clash with the built-in 'prototype' for the class.
        let prototypeSymbol = self.createSymbol(
            SymbolFlags::Property | SymbolFlags::Prototype,
            __String("prototype".into()),
        );
        let exports = self.symbols[symbol].exports.unwrap();
        let symbolExport =
            self.symbol_tables[exports].get(&self.symbols[prototypeSymbol].escapedName);
        if symbolExport.is_some() {
            todo!();
            // if (node.name) {
            //     setParent(node.name, node);
            // }
            // file.bindDiagnostics.push(createDiagnosticForNode(symbolExport.declarations[0], Diagnostics.Duplicate_identifier_0, symbolName(prototypeSymbol)));
        }
        self.symbol_tables[exports].insert(
            self.symbols[prototypeSymbol].escapedName.clone(),
            prototypeSymbol,
        );
        self.symbols[prototypeSymbol].parent = Some(symbol);
    }

    fn bindEnumDeclaration(&mut self, node: &Rc<BoundNode>) {
        if isEnumConst(node.clone(), &mut self.store) {
            self.bindBlockScopedDeclaration(
                node,
                SymbolFlags::ConstEnum,
                SymbolFlags::ConstEnumExcludes,
            );
        } else {
            self.bindBlockScopedDeclaration(
                node,
                SymbolFlags::RegularEnum,
                SymbolFlags::RegularEnumExcludes,
            );
        }
    }

    // Split from `bindVariableDeclarationOrBindingElement`
    fn bindVariableDeclaration(
        &mut self,
        node: &Rc<BoundNode>,
        declaration: &Rc<VariableDeclaration>,
    ) {
        if self.inStrictMode {
            self.checkStrictModeEvalOrArguments(node, Some(declaration.name.clone().into()));
        }

        if !isBindingPattern(Some(&declaration.name)) {
            if isInJSFile(Some(self.store.node_and_data(declaration)))
            // && isRequireVariableDeclaration(node)
            // && !getJSDocTypeTag(node)
            {
                todo!("condition above and body below");
                // self.declareSymbolAndAddToSymbolTable(
                //     node as Declaration,
                //     SymbolFlags::Alias,
                //     SymbolFlags::AliasExcludes,
                // );
            } else if isBlockOrCatchScoped(node.clone(), &mut self.store) {
                self.bindBlockScopedDeclaration(
                    node,
                    SymbolFlags::BlockScopedVariable,
                    SymbolFlags::BlockScopedVariableExcludes,
                );
            } else if isParameterDeclaration(node.clone()) {
                unreachable!("todo confirm and remove");
                // It is safe to walk up parent chain to find whether the node is a destructuring parameter declaration
                // because its parent chain has already been set up, since parents are set before descending into children.
                //
                // If node is a binding element in parameter declaration, we need to use ParameterExcludes.
                // Using ParameterExcludes flag allows the compiler to report an error on duplicate identifiers in Parameter Declaration
                // For example:
                //      function foo([a,a]) {} // Duplicate Identifier error
                //      function bar(a,a) {}   // Duplicate Identifier error, parameter declaration in this case is handled in bindParameter
                //                             // which correctly set excluded symbols
                // self.declareSymbolAndAddToSymbolTable(node, SymbolFlags::FunctionScopedVariable, SymbolFlags::ParameterExcludes);
            } else {
                self.declareSymbolAndAddToSymbolTable(
                    node,
                    SymbolFlags::FunctionScopedVariable,
                    SymbolFlags::FunctionScopedVariableExcludes,
                );
            }
        }
    }

    // Split from `bindVariableDeclarationOrBindingElement`
    fn bindBindingElement(&mut self, node: &Rc<BoundNode>, element: &Rc<BindingElement>) {
        if self.inStrictMode {
            self.checkStrictModeEvalOrArguments(node, Some(element.name.clone().into()));
        }

        if !isBindingPattern(Some(&element.name)) {
            if isInJSFile(Some(self.store.node_and_data(element)))
            // && isRequireVariableDeclaration(node)
            // && !getJSDocTypeTag(node)
            {
                todo!("condition above and body below");
                // self.declareSymbolAndAddToSymbolTable(
                //     node as Declaration,
                //     SymbolFlags::Alias,
                //     SymbolFlags::AliasExcludes,
                // );
            } else if isBlockOrCatchScoped(node.clone(), &mut self.store) {
                self.bindBlockScopedDeclaration(
                    node,
                    SymbolFlags::BlockScopedVariable,
                    SymbolFlags::BlockScopedVariableExcludes,
                );
            } else if isParameterDeclaration(node.clone()) {
                // It is safe to walk up parent chain to find whether the node is a destructuring parameter declaration
                // because its parent chain has already been set up, since parents are set before descending into children.
                //
                // If node is a binding element in parameter declaration, we need to use ParameterExcludes.
                // Using ParameterExcludes flag allows the compiler to report an error on duplicate identifiers in Parameter Declaration
                // For example:
                //      function foo([a,a]) {} // Duplicate Identifier error
                //      function bar(a,a) {}   // Duplicate Identifier error, parameter declaration in this case is handled in bindParameter
                //                             // which correctly set excluded symbols
                self.declareSymbolAndAddToSymbolTable(
                    node,
                    SymbolFlags::FunctionScopedVariable,
                    SymbolFlags::ParameterExcludes,
                );
            } else {
                self.declareSymbolAndAddToSymbolTable(
                    node,
                    SymbolFlags::FunctionScopedVariable,
                    SymbolFlags::FunctionScopedVariableExcludes,
                );
            }
        }
    }

    fn bindParameter(&mut self, node: &Rc<BoundNode>) {
        if node.kind() == SyntaxKind::JSDocParameterTag
            && self.container.as_ref().unwrap().kind() != SyntaxKind::JSDocSignature
        {
            return;
        }
        let name = match &node.node {
            Node::ParameterDeclaration(n) => &n.name,
            // TODO: jsdoc
            // Node::JSDocParameterTag()
            _ => unreachable!(),
        };
        if self.inStrictMode && !self.node_data(node).flags.intersects(NodeFlags::Ambient) {
            // It is a SyntaxError if the identifier eval or arguments appears within a FormalParameterList of a
            // strict mode FunctionLikeDeclaration or FunctionExpression(13.1)
            self.checkStrictModeEvalOrArguments(node, Some(name.clone().into()));
        }

        if isBindingPattern(Some(name)) {
            debug_assert!(matches!(&node.node, Node::ParameterDeclaration(_)));
            let parent = node.parent_node().unwrap();
            let params = match &parent {
                Node::CallSignatureDeclaration(n) => &n.parameters,
                Node::ConstructSignatureDeclaration(n) => &n.parameters,
                Node::MethodSignature(n) => &n.parameters,
                Node::IndexSignatureDeclaration(n) => &n.parameters,
                Node::FunctionTypeNode(n) => &n.parameters,
                Node::ConstructorTypeNode(n) => &n.parameters,
                Node::JSDocFunctionType(n) => &n.parameters,
                Node::FunctionDeclaration(n) => &n.parameters,
                Node::MethodDeclaration(n) => &n.parameters,
                Node::ConstructorDeclaration(n) => &n.parameters,
                Node::GetAccessorDeclaration(n) => &n.parameters,
                Node::SetAccessorDeclaration(n) => &n.parameters,
                Node::FunctionExpression(n) => &n.parameters,
                Node::ArrowFunction(n) => &n.parameters,
                _ => unreachable!(),
            };
            let node_id = node.node_id();
            let index = params.iter().position(|p| p.node_id == node_id).unwrap();
            self.bindAnonymousDeclaration(
                node,
                SymbolFlags::FunctionScopedVariable,
                __String(format!("__{}", index).into()),
            );
        } else {
            self.declareSymbolAndAddToSymbolTable(
                node,
                SymbolFlags::FunctionScopedVariable,
                SymbolFlags::ParameterExcludes,
            );
        }

        // If this is a property-parameter, then also declare the property symbol into the
        // containing class.
        if isParameterPropertyDeclaration(
            self.store.node_and_data(&node.node),
            node.parent_node().unwrap(),
        ) {
            let classDeclaration = node.parent().unwrap().parent().unwrap();
            let class_symbol = self.node_data(&classDeclaration).symbol.unwrap();
            let class_members = self.symbols[class_symbol].members.unwrap();
            let optional = unwrap_as!(&node.node, Node::ParameterDeclaration(n), n)
                .questionToken
                .is_some();
            self.declareSymbol(
                class_members,
                Some(class_symbol),
                node,
                SymbolFlags::Property
                    | (if optional {
                        SymbolFlags::Optional
                    } else {
                        SymbolFlags::None
                    }),
                SymbolFlags::PropertyExcludes,
                false,
                false,
            );
        }
    }

    fn bindFunctionDeclaration(&mut self, node: &Rc<BoundNode>, decl: &Rc<FunctionDeclaration>) {
        if !self.file.isDeclarationFile
            && !self.node_data(node).flags.intersects(NodeFlags::Ambient)
        {
            if isAsyncFunction(self.store.node_and_data(&node.node)) {
                self.emitFlags |= NodeFlags::HasAsyncFunctions;
            }
        }

        self.checkStrictModeFunctionName(node, decl.name.clone().map(Node::from));
        if self.inStrictMode {
            self.checkStrictModeFunctionDeclaration(node, decl);
            self.bindBlockScopedDeclaration(
                node,
                SymbolFlags::Function,
                SymbolFlags::FunctionExcludes,
            );
        } else {
            self.declareSymbolAndAddToSymbolTable(
                node,
                SymbolFlags::Function,
                SymbolFlags::FunctionExcludes,
            );
        }
    }

    fn bindFunctionExpression(&mut self, node: &Rc<BoundNode>) {
        if !self.file.isDeclarationFile
            && !self.node_data(node).flags.intersects(NodeFlags::Ambient)
        {
            if isAsyncFunction(self.store.node_and_data(&node.node)) {
                self.emitFlags |= NodeFlags::HasAsyncFunctions;
            }
        }
        // TODO: check for currentFlow (see below)
        self.node_data_mut(node).flowNode = Some(self.currentFlow);
        // if (currentFlow) {
        //     node.flowNode = currentFlow;
        // }
        self.checkStrictModeFunctionName(node, node.name());
        let bindingName = if let Some(Node::Identifier(name)) = node.name() {
            name.escapedText.clone()
        } else {
            __String(InternalSymbolName::Function.into())
        };
        self.bindAnonymousDeclaration(node, SymbolFlags::Function, bindingName);
    }

    fn bindPropertyOrMethodOrAccessor(
        &mut self,
        node: &Rc<BoundNode>,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) {
        if !self.file.isDeclarationFile
            && !self.node_data(node).flags.intersects(NodeFlags::Ambient)
            && isAsyncFunction(self.store.node_and_data(&node.node))
        {
            self.emitFlags |= NodeFlags::HasAsyncFunctions;
        }

        // TODO: check currentFlow (see below)
        if isObjectLiteralOrClassExpressionMethodOrAccessor(node) {
            self.node_data_mut(node).flowNode = Some(self.currentFlow);
        }
        // if (currentFlow && isObjectLiteralOrClassExpressionMethodOrAccessor(node)) {
        //     node.flowNode = currentFlow;
        // }

        if hasDynamicName(node) {
            self.bindAnonymousDeclaration(
                node,
                symbolFlags,
                __String(InternalSymbolName::Computed.into()),
            );
        } else {
            self.declareSymbolAndAddToSymbolTable(node, symbolFlags, symbolExcludes);
        }
    }

    // function getInferTypeContainer(node: Node): ConditionalTypeNode | undefined {
    //     const extendsType = findAncestor(node, n => n.parent && isConditionalTypeNode(n.parent) && n.parent.extendsType === n);
    //     return extendsType && extendsType.parent as ConditionalTypeNode;
    // }

    fn bindTypeParameter(&mut self, node: &Rc<BoundNode>) {
        // TODO: jsdoc
        // if isJSDocTemplateTag(node.parent) {
        //     todo!();
        //     // let container = getEffectiveContainerForJSDocTemplateTag(node.parent);
        //     // if (container) {
        //     //     if (!container.locals) {
        //     //         container.locals = createSymbolTable();
        //     //     }
        //     //     self.declareSymbol(
        //     //         container.locals,
        //     //         None,
        //     //         node,
        //     //         SymbolFlags::TypeParameter,
        //     //         SymbolFlags::TypeParameterExcludes,
        //     //         false,
        //     //         false,
        //     //     );
        //     // } else {
        //     //     self.declareSymbolAndAddToSymbolTable(
        //     //         node,
        //     //         SymbolFlags::TypeParameter,
        //     //         SymbolFlags::TypeParameterExcludes,
        //     //     );
        //     // }
        // } else
        if matches!(node.parent_node(), Some(Node::InferTypeNode(_))) {
            todo!();
            // let container = getInferTypeContainer(node.parent);
            // if (container) {
            //     if (!container.locals) {
            //         container.locals = createSymbolTable();
            //     }
            //     self.declareSymbol(
            //         container.locals,
            //         None,
            //         node,
            //         SymbolFlags::TypeParameter,
            //         SymbolFlags::TypeParameterExcludes,
            //         false,
            //         false,
            //     );
            // } else {
            //     self.bindAnonymousDeclaration(
            //         node,
            //         SymbolFlags::TypeParameter,
            //         getDeclarationName(node),
            //     ); // TODO: GH#18217
            // }
        } else {
            self.declareSymbolAndAddToSymbolTable(
                node,
                SymbolFlags::TypeParameter,
                SymbolFlags::TypeParameterExcludes,
            );
        }
    }

    // reachability checks

    //     function shouldReportErrorOnModuleDeclaration(node: ModuleDeclaration): boolean {
    //         const instanceState = getModuleInstanceState(node);
    //         return instanceState === ModuleInstanceState.Instantiated || (instanceState === ModuleInstanceState.ConstEnumOnly && shouldPreserveConstEnums(options));
    //     }

    fn checkUnreachable(&mut self, node: &Rc<BoundNode>) -> bool {
        if !self.flow_nodes[self.currentFlow]
            .flags
            .intersects(FlowFlags::Unreachable)
        {
            return false;
        }
        todo!();
        // if (currentFlow === unreachableFlow) {
        //     const reportError =
        //         // report error on all statements except empty ones
        //         (isStatementButNotDeclaration(node) && node.kind !== SyntaxKind::EmptyStatement) ||
        //         // report error on class declarations
        //         node.kind === SyntaxKind::ClassDeclaration ||
        //         // report error on instantiated modules or const-enums only modules if preserveConstEnums is set
        //         (node.kind === SyntaxKind::ModuleDeclaration && shouldReportErrorOnModuleDeclaration(node as ModuleDeclaration));

        //     if (reportError) {
        //         currentFlow = reportedUnreachableFlow;

        //         if (!options.allowUnreachableCode) {
        //             // unreachable code is reported if
        //             // - user has explicitly asked about it AND
        //             // - statement is in not ambient context (statements in ambient context is already an error
        //             //   so we should not report extras) AND
        //             //   - node is not variable statement OR
        //             //   - node is block scoped variable statement OR
        //             //   - node is not block scoped variable statement and at least one variable declaration has initializer
        //             //   Rationale: we don't want to report errors on non-initialized var's since they are hoisted
        //             //   On the other side we do want to report errors on non-initialized 'lets' because of TDZ
        //             const isError =
        //                 unreachableCodeIsError(options) &&
        //                 !(node.flags & NodeFlags::Ambient) &&
        //                 (
        //                     !isVariableStatement(node) ||
        //                     !!(getCombinedNodeFlags(node.declarationList) & NodeFlags::BlockScoped) ||
        //                     node.declarationList.declarations.some(d => !!d.initializer)
        //                 );

        //             eachUnreachableRange(node, (start, end) => errorOrSuggestionOnRange(isError, start, end, Diagnostics.Unreachable_code_detected));
        //         }
        //     }
        // }
        // return true;
    }

    fn lookupSymbolForName(&mut self, container: &Node, name: &__String) -> Option<SymbolId> {
        let locals = self.node_data(container).locals;
        let local = locals.and_then(|l| self.symbol_tables[l].get(name));
        if let Some(local) = local {
            return self.symbols[*local].exportSymbol.or(Some(*local));
        }
        if let Node::SourceFile(container) = container {
            todo!();
            // if container.jsGlobalAugmentations && container.jsGlobalAugmentations.has(name) {
            //     return container.jsGlobalAugmentations.get(name);
            // }
        }
        self.node_data(container)
            .symbol
            .and_then(|s| self.symbols[s].exports)
            .and_then(|t| self.symbol_tables[t].get(name))
            .copied()
    }
}

// function eachUnreachableRange(node: Node, cb: (start: Node, last: Node) => void): void {
//     if (isStatement(node) && isExecutableStatement(node) && isBlock(node.parent)) {
//         const { statements } = node.parent;
//         const slice = sliceAfter(statements, node);
//         getRangesWhere(slice, isExecutableStatement, (start, afterEnd) => cb(slice[start], slice[afterEnd - 1]));
//     }
//     else {
//         cb(node, node);
//     }
// }
// // As opposed to a pure declaration like an `interface`
// function isExecutableStatement(s: Statement): boolean {
//     // Don't remove statements that can validly be used before they appear.
//     return !isFunctionDeclaration(s) && !isPurelyTypeDeclaration(s) && !isEnumDeclaration(s) &&
//         // `var x;` may declare a variable used above
//         !(isVariableStatement(s) && !(getCombinedNodeFlags(s) & (NodeFlags::Let | NodeFlags::Const)) && s.declarationList.declarations.some(d => !d.initializer));
// }

// function isPurelyTypeDeclaration(s: Statement): boolean {
//     switch (s.kind) {
//         case SyntaxKind::InterfaceDeclaration:
//         case SyntaxKind::TypeAliasDeclaration:
//             return true;
//         case SyntaxKind::ModuleDeclaration:
//             return getModuleInstanceState(s as ModuleDeclaration) !== ModuleInstanceState.Instantiated;
//         case SyntaxKind::EnumDeclaration:
//             return hasSyntacticModifier(s, ModifierFlags.Const);
//         default:
//             return false;
//     }
// }

// export function isExportsOrModuleExportsOrAlias(sourceFile: SourceFile, node: Expression): boolean {
//     let i = 0;
//     const q = [node];
//     while (q.length && i < 100) {
//         i++;
//         node = q.shift()!;
//         if (isExportsIdentifier(node) || isModuleExportsAccessExpression(node)) {
//             return true;
//         }
//         else if (isIdentifier(node)) {
//             const symbol = lookupSymbolForName(sourceFile, node.escapedText);
//             if (!!symbol && !!symbol.valueDeclaration && isVariableDeclaration(symbol.valueDeclaration) && !!symbol.valueDeclaration.initializer) {
//                 const init = symbol.valueDeclaration.initializer;
//                 q.push(init);
//                 if (isAssignmentExpression(init, /*excludeCompoundAssignment*/ true)) {
//                     q.push(init.left);
//                     q.push(init.right);
//                 }
//             }
//         }
//     }
//     return false;
// }
