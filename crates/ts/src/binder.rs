use std::collections::hash_map::Entry;
use std::collections::VecDeque;
use std::rc::Rc;

use bitflags::bitflags;
use diagnostics::Diagnostics;
use index::vec::IndexVec;
use rustc_hash::FxHashMap;

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

#[derive(PartialEq, PartialOrd, Clone, Copy)]
pub enum ModuleInstanceState {
    NonInstantiated,
    Instantiated,
    ConstEnumOnly,
}

#[derive(Clone)]
struct ActiveLabel {
    name: __String,
    breakTarget: FlowLabelId,
    continueTarget: Option<FlowLabelId>,
    referenced: bool,
}

pub fn getModuleInstanceState(
    node: &Rc<BoundNode>,
    decl: &Rc<ModuleDeclaration>,
    node_data: &mut NodeDataStore,
    visited: &mut Option<FxHashMap<NodeId, Option<ModuleInstanceState>>>,
) -> ModuleInstanceState {
    if let Some(body) = &decl.body {
        getModuleInstanceStateCached(&body.bind(node), node_data, visited)
    } else {
        ModuleInstanceState::Instantiated
    }
}

fn getModuleInstanceStateCached(
    node: &Rc<BoundNode>,
    node_data: &mut NodeDataStore,
    visited: &mut Option<FxHashMap<NodeId, Option<ModuleInstanceState>>>,
) -> ModuleInstanceState {
    visited.get_or_insert_with(|| FxHashMap::default());
    let nodeId = node.node_id();
    if let Some(cached) = visited.as_ref().unwrap().get(&nodeId) {
        return cached.unwrap_or(ModuleInstanceState::NonInstantiated);
    }
    visited.as_mut().unwrap().insert(nodeId, None);
    let result = getModuleInstanceStateWorker(node, node_data, visited);
    visited.as_mut().unwrap().insert(nodeId, Some(result));
    result
}

fn getModuleInstanceStateWorker(
    node: &Rc<BoundNode>,
    node_data: &mut NodeDataStore,
    visited: &mut Option<FxHashMap<NodeId, Option<ModuleInstanceState>>>,
) -> ModuleInstanceState {
    // A module is uninstantiated if it contains only
    match &node.node {
        // 1. interface declarations, type alias declarations
        Node::InterfaceDeclaration(_) | Node::TypeAliasDeclaration(_) => {
            return ModuleInstanceState::NonInstantiated;
        }
        // 2. const enum declarations
        Node::EnumDeclaration(_) => {
            if isEnumConst(node.clone(), node_data) {
                return ModuleInstanceState::ConstEnumOnly;
            }
        }
        // 3. non-exported import declarations
        Node::ImportDeclaration(_) | Node::ImportEqualsDeclaration(_) => {
            if !hasSyntacticModifier(node_data.node_and_data(&node.node), ModifierFlags::Export) {
                return ModuleInstanceState::NonInstantiated;
            }
        }
        // 4. Export alias declarations pointing at only uninstantiated modules or things uninstantiated modules contain
        Node::ExportDeclaration(exportDeclaration) => {
            if exportDeclaration.moduleSpecifier.is_none() {
                if let Some(NamedExportBindings::NamedExports(exportClause)) =
                    &exportDeclaration.exportClause
                {
                    let mut state = ModuleInstanceState::NonInstantiated;
                    for specifier in exportClause.elements.iter() {
                        let specifierState = getModuleInstanceStateForAliasTarget(
                            &specifier.bind(&exportClause.bind(node)),
                            specifier,
                            node_data,
                            visited,
                        );
                        if specifierState > state {
                            state = specifierState;
                        }
                        if state == ModuleInstanceState::Instantiated {
                            return state;
                        }
                    }
                    return state;
                }
            }
        }
        // 5. other uninstantiated module declarations.
        Node::ModuleBlock(_) => {
            let mut state = ModuleInstanceState::NonInstantiated;
            forEachChild(Some(node.node.clone()), node.parent().as_ref(), |n| {
                let childState = getModuleInstanceStateCached(&n, node_data, visited);
                match childState {
                    ModuleInstanceState::NonInstantiated => {
                        // child is non-instantiated - continue searching
                        None
                    }
                    ModuleInstanceState::ConstEnumOnly => {
                        // child is const enum only - record state and continue searching
                        state = ModuleInstanceState::ConstEnumOnly;
                        None
                    }
                    ModuleInstanceState::Instantiated => {
                        // child is instantiated - record state and stop
                        state = ModuleInstanceState::Instantiated;
                        Some(())
                    }
                }
            });
            return state;
        }
        Node::ModuleDeclaration(n) => {
            return getModuleInstanceState(node, n, node_data, visited);
        }
        Node::Identifier(n) => {
            // Only jsdoc typedef definition can exist in jsdoc namespace, and it should
            // be considered the same as type alias
            if n.isInJSDocNamespace {
                return ModuleInstanceState::NonInstantiated;
            }
        }
        _ => {}
    }
    ModuleInstanceState::Instantiated
}

fn getModuleInstanceStateForAliasTarget(
    node: &Rc<BoundNode>,
    specifier: &Rc<ExportSpecifier>,
    node_data: &mut NodeDataStore,
    visited: &mut Option<FxHashMap<NodeId, Option<ModuleInstanceState>>>,
) -> ModuleInstanceState {
    let name = specifier.propertyName.as_ref().unwrap_or(&specifier.name);
    let mut parent = node.parent();
    while let Some(p) = parent {
        let statements = match &p.node {
            Node::SourceFile(n) => Some(&n.statements),
            Node::ModuleBlock(n) => Some(&n.statements),
            Node::Block(n) => Some(&n.statements),
            _ => None,
        };
        if let Some(statements) = statements {
            let mut found = None;
            for statement in statements.iter() {
                if nodeHasName(&statement.clone().into(), name) {
                    let state =
                        getModuleInstanceStateCached(&statement.bind(&p), node_data, visited);
                    if found.is_none() || state > found.unwrap() {
                        found = Some(state);
                    }
                    if found.unwrap() == ModuleInstanceState::Instantiated {
                        return found.unwrap();
                    }
                }
            }
            if let Some(found) = found {
                return found;
            }
        }
        parent = p.parent();
    }
    ModuleInstanceState::Instantiated // Couldn't locate, assume could refer to a value
}

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
    unreachableFlow: FlowNodeId,
    reportedUnreachableFlow: FlowNodeId,
    //     const bindBinaryExpressionFlow = createBindBinaryExpressionFlow();
    store: &'a mut NodeDataStore,
    flow_nodes: FlowNodeStore,
    symbols: IndexVec<SymbolId, Symbol>,
    symbol_tables: IndexVec<SymbolTableId, SymbolTable>,
}

impl<'a> Binder<'a> {
    fn new(
        options: CompilerOptions,
        intitial_file: Rc<SourceFile>,
        store: &'a mut NodeDataStore,
    ) -> Binder {
        let mut flow_nodes = FlowNodeStore::with_capacity(2);
        let unreachableFlow = flow_nodes.push_flow_node(FlowNode {
            flags: FlowFlags::Unreachable,
            kind: FlowNodeKind::None,
        });
        let reportedUnreachableFlow = flow_nodes.push_flow_node(FlowNode {
            flags: FlowFlags::Unreachable,
            kind: FlowNodeKind::None,
        });
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
            unreachableFlow,
            reportedUnreachableFlow,

            store,
            flow_nodes,
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
        self.flow_nodes.push_flow_node(node)
    }
    fn alloc_flow_label(&mut self, node: FlowNode) -> FlowLabelId {
        self.flow_nodes.push_flow_label(node)
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
                // Parameters with names are handled at the top of this function.  Parameters
                // without names can only come from JSDocFunctionTypes.
                let functionType =
                    unwrap_as!(node.parent_node(), Some(Node::JSDocFunctionType(n)), n);
                let index = functionType
                    .parameters
                    .iter()
                    .position(|n| n.node_id() == node.node_id());
                Some(__String(format!("arg{}", index.unwrap()).into()))
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

    fn declareModuleMember(
        &mut self,
        node: &Rc<BoundNode>,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) -> SymbolId {
        let container = self.container.clone().unwrap();
        let container_symbol = self.node_data(&container).symbol.unwrap();
        let container_exports = self.symbols[container_symbol].exports.unwrap();
        let container_locals = self.node_data(&container).locals;
        let hasExportModifier = getCombinedModifierFlags(node.clone(), &mut self.store)
            .intersects(ModifierFlags::Export)
            || self.jsdocTreatAsExported(node.clone());
        if symbolFlags.intersects(SymbolFlags::Alias) {
            if node.kind() == SyntaxKind::ExportSpecifier
                || (node.kind() == SyntaxKind::ImportEqualsDeclaration && hasExportModifier)
            {
                self.declareSymbol(
                    container_exports,
                    Some(container_symbol),
                    node,
                    symbolFlags,
                    symbolExcludes,
                    false,
                    false,
                )
            } else {
                self.declareSymbol(
                    container_locals.unwrap(),
                    None,
                    node,
                    symbolFlags,
                    symbolExcludes,
                    false,
                    false,
                )
            }
        } else {
            // Exported module members are given 2 symbols: A local symbol that is classified with an ExportValue flag,
            // and an associated export symbol with all the correct flags set on it. There are 2 main reasons:
            //
            //   1. We treat locals and exports of the same name as mutually exclusive within a container.
            //      That means the binder will issue a Duplicate Identifier error if you mix locals and exports
            //      with the same name in the same container.
            //      TODO: Make this a more specific error and decouple it from the exclusion logic.
            //   2. When we checkIdentifier in the checker, we set its resolved symbol to the local symbol,
            //      but return the export symbol (by calling getExportSymbolOfValueSymbolIfExported). That way
            //      when the emitter comes back to it, it knows not to qualify the name if it was found in a containing scope.

            // NOTE: Nested ambient modules always should go to to 'locals' table to prevent their automatic merge
            //       during global merging in the checker. Why? The only case when ambient module is permitted inside another module is module augmentation
            //       and this case is specially handled. Module augmentations should only be merged with original module definition
            //       and should never be merged directly with other augmentation, and the latter case would be possible if automatic merge is allowed.
            if isJSDocTypeAlias(node) {
                debug_assert!(isInJSFile(Some(self.store.node_and_data(&node.node))));
                // We shouldn't add symbols for JSDoc nodes if not in a JS file.
            }
            if !isAmbientModule(self.store.node_and_data(&node.node))
                && (hasExportModifier
                    || self
                        .node_data(&container)
                        .flags
                        .intersects(NodeFlags::ExportContext))
            {
                if container_locals.is_none()
                    || (hasSyntacticModifier(
                        self.store.node_and_data(&node.node),
                        ModifierFlags::Default,
                    ) && self.getDeclarationName(node).is_none())
                {
                    return self.declareSymbol(
                        container_exports,
                        Some(container_symbol),
                        node,
                        symbolFlags,
                        symbolExcludes,
                        false,
                        false,
                    ); // No local symbol for an unnamed default!
                }
                let exportKind = if symbolFlags.intersects(SymbolFlags::Value) {
                    SymbolFlags::ExportValue
                } else {
                    SymbolFlags::None
                };
                let local = self.declareSymbol(
                    container_locals.unwrap(),
                    None,
                    node,
                    exportKind,
                    symbolExcludes,
                    false,
                    false,
                );
                self.symbols[local].exportSymbol = Some(self.declareSymbol(
                    container_exports,
                    Some(container_symbol),
                    node,
                    symbolFlags,
                    symbolExcludes,
                    false,
                    false,
                ));
                self.node_data_mut(&node).localSymbol = Some(local);
                local
            } else {
                self.declareSymbol(
                    container_locals.unwrap(),
                    None,
                    node,
                    symbolFlags,
                    symbolExcludes,
                    false,
                    false,
                )
            }
        }
    }

    fn jsdocTreatAsExported(&mut self, mut node: Rc<BoundNode>) -> bool {
        if let Some(parent) = node.parent() {
            if isModuleDeclaration(&node) {
                node = parent;
            }
        }
        if !isJSDocTypeAlias(&node) {
            return false;
        }
        todo!();
        // // jsdoc typedef handling is a bit of a doozy, but to summarize, treat the typedef as exported if:
        // // 1. It has an explicit name (since by default typedefs are always directly exported, either at the top level or in a container), or
        // if !isJSDocEnumTag(node) && !!node.fullName {
        //     return true;
        // }
        // // 2. The thing a nameless typedef pulls its name from is implicitly a direct export (either by assignment or actual export flag).
        // let declName = match getNameOfDeclaration(node){
        //     Some(n)=>n,
        //     None => return false,
        // };
        // if isPropertyAccessEntityNameExpression(declName.parent)
        //     && isTopLevelNamespaceAssignment(declName.parent)
        // {
        //     return true;
        // }
        // if isDeclaration(declName.parent)
        //     && getCombinedModifierFlags(declName.parent).intersects(ModifierFlags::Export)
        // {
        //     return true;
        // }
        // // This could potentially be simplified by having `delayedBindJSDocTypedefTag` pass in an override for `hasExportModifier`, since it should
        // // already have calculated and branched on most of this.
        // return false;
    }

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
            let saveActiveLabelListLength = self.activeLabelList.len();
            let saveHasExplicitReturn = self.hasExplicitReturn;

            let isIIFE = containerFlags.intersects(ContainerFlags::IsFunctionExpression)
                && !hasSyntacticModifier(
                    self.store.node_and_data(&node.node),
                    ModifierFlags::Async,
                )
                && !matches!(&node.node, Node::FunctionExpression(n) if n.asteriskToken.is_some())
                && getImmediatelyInvokedFunctionExpression(&node).is_some();
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
                Some(self.createBranchLabel())
            } else {
                None
            };
            self.currentExceptionTarget = None;
            self.currentBreakTarget = None;
            self.currentContinueTarget = None;
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
            self.activeLabelList.truncate(saveActiveLabelListLength);
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
        T: Bind<Bound = Rc<BoundNode>>,
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
        T: Bind<Bound = Rc<BoundNode>>,
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
        T: Bind<Bound = Rc<BoundNode>>,
        F: FnMut(&mut Self, Rc<BoundNode>),
    {
        if let Some(nodes) = nodes {
            for n in nodes.iter() {
                bindFunction(self, n.bind_to_opt_parent(parent))
            }
        }
    }

    fn bindEachChild(&mut self, parent: Option<&Rc<BoundNode>>, node: Node) {
        forEachChild::<(), _>(Some(node), parent, |n| {
            self.bind(Some(n));
            None
        });
    }

    fn bindChildren(&mut self, node: &Rc<BoundNode>) {
        let saveInAssignmentPattern = self.inAssignmentPattern;
        // Most nodes aren't valid in an assignment pattern, so we clear the value here
        // and set it before we descend into nodes that could actually be part of an assignment pattern.
        self.inAssignmentPattern = false;
        if self.checkUnreachable(node) {
            self.bindEachChild(node.parent().as_ref(), node.node.clone());
            self.bindJSDoc(node);
            self.inAssignmentPattern = saveInAssignmentPattern;
            return;
        }
        if node.kind() >= SyntaxKind::FirstStatement
            && node.kind() <= SyntaxKind::LastStatement
            && !self.options.allowUnreachableCode
        {
            self.node_data_mut(node).flowNode = Some(self.currentFlow);
        }
        match &node.node {
            Node::WhileStatement(n) => {
                self.bindWhileStatement(node, n);
            }
            Node::DoStatement(n) => {
                self.bindDoStatement(node, n);
            }
            Node::ForStatement(n) => {
                self.bindForStatement(node, n);
            }
            Node::ForInStatement(n) => {
                self.bindForInOrForOfStatement(
                    node,
                    &n.expression,
                    &None,
                    &n.statement,
                    &n.initializer,
                );
            }
            Node::ForOfStatement(n) => {
                self.bindForInOrForOfStatement(
                    node,
                    &n.expression,
                    &n.awaitModifier,
                    &n.statement,
                    &n.initializer,
                );
            }
            Node::IfStatement(n) => {
                self.bindIfStatement(node, n);
            }
            Node::ReturnStatement(n) => {
                self.bindReturn(node, n);
            }
            Node::ThrowStatement(n) => {
                self.bindThrow(node, n);
            }
            Node::BreakStatement(n) => {
                self.bindBreakOrContinueStatement(
                    node,
                    BreakOrContinueStatement::BreakStatement(n.clone()),
                );
            }
            Node::ContinueStatement(n) => {
                self.bindBreakOrContinueStatement(
                    node,
                    BreakOrContinueStatement::ContinueStatement(n.clone()),
                );
            }
            Node::TryStatement(n) => {
                self.bindTryStatement(node, n);
            }
            Node::SwitchStatement(n) => {
                self.bindSwitchStatement(node, n);
            }
            Node::CaseBlock(n) => {
                self.bindCaseBlock(node, n);
            }
            Node::CaseClause(n) => {
                self.bindCaseClause(node, n);
            }
            Node::ExpressionStatement(n) => {
                self.bindExpressionStatement(node, n);
            }
            Node::LabeledStatement(n) => {
                self.bindLabeledStatement(node, n);
            }
            Node::PrefixUnaryExpression(n) => {
                self.bindPrefixUnaryExpressionFlow(node, n);
            }
            Node::PostfixUnaryExpression(n) => {
                self.bindPostfixUnaryExpressionFlow(node, n);
            }
            Node::BinaryExpression(n) => {
                if isDestructuringAssignment(&node.node) {
                    // Carry over whether we are in an assignment pattern to
                    // binary expressions that could actually be an initializer
                    self.inAssignmentPattern = saveInAssignmentPattern;
                    self.bindDestructuringAssignmentFlow(node, n);
                    return;
                }
                todo!("binary expression");
                // self.bindBinaryExpressionFlow(node, n);
            }
            Node::DeleteExpression(n) => {
                self.bindDeleteExpressionFlow(node, n);
            }
            Node::ConditionalExpression(n) => {
                self.bindConditionalExpressionFlow(node, n);
            }
            Node::VariableDeclaration(n) => {
                self.bindVariableDeclarationFlow(node, n);
            }
            Node::PropertyAccessExpression(_) | Node::ElementAccessExpression(_) => {
                self.bindAccessExpressionFlow(node);
            }
            Node::CallExpression(n) => {
                self.bindCallExpressionFlow(node, n);
            }
            Node::NonNullExpression(_) => {
                self.bindNonNullExpressionFlow(node);
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
            Node::Block(n) => {
                self.bindEachFunctionsFirst(Some(node), Some(&n.statements));
            }
            Node::ModuleBlock(n) => {
                self.bindEachFunctionsFirst(Some(node), Some(&n.statements));
            }
            Node::BindingElement(n) => {
                self.bindBindingElementFlow(node, n);
            }
            Node::ObjectLiteralExpression(_)
            | Node::ArrayLiteralExpression(_)
            | Node::PropertyAssignment(_)
            | Node::SpreadElement(_) => {
                // Carry over whether we are in an assignment pattern of Object and Array literals
                // as well as their children that are valid assignment targets.
                self.inAssignmentPattern = saveInAssignmentPattern;
                self.bindEachChild(node.parent().as_ref(), node.node.clone());
            }
            _ => {
                self.bindEachChild(node.parent().as_ref(), node.node.clone());
            }
        }
        self.bindJSDoc(node);
        self.inAssignmentPattern = saveInAssignmentPattern;
    }

    fn isNarrowingExpression(&mut self, expr: &Node) -> bool {
        match expr {
            Node::Identifier(_)
            | Node::PrivateIdentifier(_)
            | Node::ThisExpression(_)
            | Node::PropertyAccessExpression(_)
            | Node::ElementAccessExpression(_) => self.containsNarrowableReference(expr),
            Node::CallExpression(n) => self.hasNarrowableArgument(n),
            Node::ParenthesizedExpression(n) => {
                self.isNarrowingExpression(&n.expression.clone().into())
            }
            Node::NonNullExpression(n) => self.isNarrowingExpression(&n.expression.clone().into()),
            Node::BinaryExpression(n) => self.isNarrowingBinaryExpression(n),
            Node::PrefixUnaryExpression(n) => {
                n.operator == SyntaxKind::ExclamationToken
                    && self.isNarrowingExpression(&n.operand.clone().into())
            }
            Node::TypeOfExpression(n) => self.isNarrowingExpression(&n.expression.clone().into()),
            _ => false,
        }
    }

    fn isNarrowableReference(&mut self, expr: &Node) -> bool {
        if isDottedName(expr) {
            return true;
        }
        match expr {
            Node::PropertyAccessExpression(n) => {
                self.isNarrowableReference(&n.expression.clone().into())
            }
            Node::NonNullExpression(n) => self.isNarrowableReference(&n.expression.clone().into()),
            Node::ParenthesizedExpression(n) => {
                self.isNarrowableReference(&n.expression.clone().into())
            }
            Node::BinaryExpression(n) if n.operatorToken.kind() == SyntaxKind::CommaToken => {
                self.isNarrowableReference(&n.right.clone().into())
            }
            Node::ElementAccessExpression(n) => {
                isStringOrNumericLiteralLike(&n.argumentExpression)
                    && self.isNarrowableReference(&n.expression.clone().into())
            }
            Node::BinaryExpression(n) => {
                isAssignmentExpression(expr, false)
                    && self.isNarrowableReference(&n.left.clone().into())
            }
            _ => false,
        }
    }

    fn containsNarrowableReference(&mut self, expr: &Node) -> bool {
        self.isNarrowableReference(expr)
            || if let Some(expr) = asOptionalChain(self.store.node_and_data(expr)) {
                let expr = match expr {
                    OptionalChain::PropertyAccessExpression(n) => n.expression.clone(),
                    OptionalChain::ElementAccessExpression(n) => n.expression.clone(),
                    OptionalChain::CallExpression(n) => n.expression.clone(),
                    OptionalChain::NonNullExpression(n) => n.expression.clone(),
                };
                self.containsNarrowableReference(&expr.into())
            } else {
                false
            }
    }

    fn hasNarrowableArgument(&mut self, expr: &Rc<CallExpression>) -> bool {
        for argument in expr.arguments.iter() {
            if self.containsNarrowableReference(&argument.clone().into()) {
                return true;
            }
        }
        if let LeftHandSideExpression::PropertyAccessExpression(e) = &expr.expression {
            return self.containsNarrowableReference(&e.expression.clone().into());
        }
        false
    }

    fn isNarrowingTypeofOperands(&mut self, expr1: &Expression, expr2: &Expression) -> bool {
        if let Expression::TypeOfExpression(expr1) = expr1 {
            self.isNarrowableOperand(&expr1.expression.clone().into()) && isStringLiteralLike(expr2)
        } else {
            false
        }
    }

    fn isNarrowingBinaryExpression(&mut self, expr: &Rc<BinaryExpression>) -> bool {
        match expr.operatorToken.kind() {
            SyntaxKind::EqualsToken
            | SyntaxKind::BarBarEqualsToken
            | SyntaxKind::AmpersandAmpersandEqualsToken
            | SyntaxKind::QuestionQuestionEqualsToken => {
                self.containsNarrowableReference(&expr.left.clone().into())
            }
            SyntaxKind::EqualsEqualsToken
            | SyntaxKind::ExclamationEqualsToken
            | SyntaxKind::EqualsEqualsEqualsToken
            | SyntaxKind::ExclamationEqualsEqualsToken => {
                self.isNarrowableOperand(&expr.left)
                    || self.isNarrowableOperand(&expr.right)
                    || self.isNarrowingTypeofOperands(&expr.right, &expr.left)
                    || self.isNarrowingTypeofOperands(&expr.left, &expr.right)
            }
            SyntaxKind::InstanceOfKeyword => self.isNarrowableOperand(&expr.left),
            SyntaxKind::InKeyword => self.isNarrowingExpression(&expr.right.clone().into()),
            SyntaxKind::CommaToken => self.isNarrowingExpression(&expr.right.clone().into()),
            _ => false,
        }
    }

    fn isNarrowableOperand(&mut self, expr: &Expression) -> bool {
        match expr {
            Expression::ParenthesizedExpression(n) => {
                return self.isNarrowableOperand(&n.expression);
            }
            Expression::BinaryExpression(n) => match n.operatorToken.kind() {
                SyntaxKind::EqualsToken => {
                    return self.isNarrowableOperand(&n.left);
                }
                SyntaxKind::CommaToken => {
                    return self.isNarrowableOperand(&n.right);
                }
                _ => {}
            },
            _ => {}
        }
        self.containsNarrowableReference(&expr.clone().into())
    }

    fn createBranchLabel(&mut self) -> FlowLabelId {
        self.alloc_flow_label(FlowNode::new_branch_label())
    }

    fn createLoopLabel(&mut self) -> FlowLabelId {
        self.alloc_flow_label(FlowNode::new_loop_label())
    }

    fn createReduceLabel(
        &mut self,
        target: FlowLabelId,
        antecedents: Rc<Vec<FlowNodeId>>,
        antecedent: FlowNodeId,
    ) -> FlowNodeId {
        let antecedent = FlowNode::new_reduce_label(target, antecedents, antecedent);
        self.alloc_flow_node(antecedent)
    }

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
        if !self.flow_nodes[antecedent]
            .flags
            .intersects(FlowFlags::Unreachable)
        {
            if let Some(antecedents) = &mut self.flow_nodes[label].antecedents {
                if !antecedents.contains(&antecedent) {
                    Rc::make_mut(antecedents).push(antecedent);
                    self.setFlowNodeReferenced(antecedent);
                }
            } else {
                self.flow_nodes[label].antecedents = Some(vec![antecedent].into());
                self.setFlowNodeReferenced(antecedent);
            }
        }
    }

    fn createFlowCondition(
        &mut self,
        flags: FlowFlags,
        antecedent: FlowNodeId,
        expression: Option<&Rc<BoundNode>>,
    ) -> FlowNodeId {
        if self.flow_nodes[antecedent]
            .flags
            .intersects(FlowFlags::Unreachable)
        {
            return antecedent;
        }
        let expression = match expression {
            Some(e) => e,
            None => {
                return if flags.intersects(FlowFlags::TrueCondition) {
                    antecedent
                } else {
                    self.unreachableFlow
                };
            }
        };
        if (expression.kind() == SyntaxKind::TrueKeyword
            && flags.intersects(FlowFlags::FalseCondition)
            || expression.kind() == SyntaxKind::FalseKeyword
                && flags.intersects(FlowFlags::TrueCondition))
            && !isExpressionOfOptionalChainRoot(
                &expression.node,
                self.store.node_and_data(&expression.parent_node().unwrap()),
            )
            && !isNullishCoalesce(&expression.parent_node().unwrap())
        {
            return self.unreachableFlow;
        }
        if !self.isNarrowingExpression(&expression.node) {
            return antecedent;
        }
        self.setFlowNodeReferenced(antecedent);
        let node = FlowNode {
            flags,
            kind: FlowNodeKind::FlowCondition(FlowCondition {
                antecedent,
                node: expression.clone(),
            }),
        };
        self.alloc_flow_node(node)
    }

    fn createFlowSwitchClause(
        &mut self,
        antecedent: FlowNodeId,
        switchStatement: Rc<SwitchStatement>,
        clauseStart: u32,
        clauseEnd: u32,
    ) -> FlowNodeId {
        self.setFlowNodeReferenced(antecedent);
        let node = FlowNode {
            flags: FlowFlags::SwitchClause,
            kind: FlowNodeKind::FlowSwitchClause(FlowSwitchClause {
                switchStatement,
                clauseStart,
                clauseEnd,
                antecedent,
            }),
        };
        self.alloc_flow_node(node)
    }

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

    fn createFlowArrayMutation(
        &mut self,
        antecedent: FlowNodeId,
        node: Rc<BoundNode>,
    ) -> FlowNodeId {
        self.setFlowNodeReferenced(antecedent);
        let node = FlowNode {
            flags: FlowFlags::ArrayMutation,
            kind: FlowNodeKind::FlowArrayMutation(FlowArrayMutation { antecedent, node }),
        };
        let id = self.alloc_flow_node(node);
        if let Some(currentExceptionTarget) = self.currentExceptionTarget {
            self.addAntecedent(currentExceptionTarget, id);
        }
        id
    }

    fn createFlowCall(&mut self, antecedent: FlowNodeId, node: Rc<CallExpression>) -> FlowNodeId {
        self.setFlowNodeReferenced(antecedent);
        let node = FlowNode {
            flags: FlowFlags::Call,
            kind: FlowNodeKind::FlowCall(FlowCall { antecedent, node }),
        };
        self.alloc_flow_node(node)
    }

    fn finishFlowLabel(&mut self, flow: FlowLabelId) -> FlowNodeId {
        let antecedents = match &self.flow_nodes[flow].antecedents {
            Some(a) => a,
            None => return self.unreachableFlow,
        };
        if antecedents.len() == 1 {
            return antecedents[0];
        }
        flow.into()
    }

    fn isStatementCondition(node: &Rc<BoundNode>) -> bool {
        let parent = node.parent_node();
        match parent {
            Some(Node::IfStatement(n)) => n.expression.node_id() == node.node_id(),
            Some(Node::WhileStatement(n)) => n.expression.node_id() == node.node_id(),
            Some(Node::DoStatement(n)) => n.expression.node_id() == node.node_id(),
            Some(Node::ForStatement(n)) => n
                .condition
                .as_ref()
                .map(|c| c.node_id() == node.node_id())
                .unwrap_or_default(),
            Some(Node::ConditionalExpression(n)) => n.condition.node_id() == node.node_id(),
            _ => false,
        }
    }

    fn isLogicalExpression(mut node: Node) -> bool {
        loop {
            match node {
                Node::ParenthesizedExpression(n) => {
                    node = n.expression.clone().into();
                }
                Node::PrefixUnaryExpression(n) if n.operator == SyntaxKind::ExclamationToken => {
                    node = n.operand.clone().into()
                }
                Node::BinaryExpression(n) => {
                    return n.operatorToken.kind() == SyntaxKind::AmpersandAmpersandToken
                        || n.operatorToken.kind() == SyntaxKind::BarBarToken
                        || n.operatorToken.kind() == SyntaxKind::QuestionQuestionToken;
                }
                _ => return false,
            }
        }
    }

    fn isLogicalAssignmentExpression(node: Node) -> bool {
        let node = skipParentheses(node, false);
        if let Node::BinaryExpression(n) = node {
            isLogicalOrCoalescingAssignmentOperator(n.operatorToken.kind())
        } else {
            false
        }
    }

    fn isTopLevelLogicalExpression(mut node: Rc<BoundNode>) -> bool {
        loop {
            let parent = node.parent().unwrap();
            if isParenthesizedExpression(&parent) {
                node = parent;
                continue;
            }
            if let Node::PrefixUnaryExpression(p) = &parent.node {
                if p.operator == SyntaxKind::ExclamationToken {
                    node = parent;
                    continue;
                }
            }
            break;
        }
        if Self::isStatementCondition(&node)
            || Self::isLogicalAssignmentExpression(node.parent_node().unwrap())
            || Self::isLogicalExpression(node.parent_node().unwrap())
        {
            return false;
        }
        match node.parent_node().unwrap() {
            Node::PropertyAccessExpression(n) => n.expression.node_id() != node.node_id(),
            Node::ElementAccessExpression(n) => n.expression.node_id() != node.node_id(),
            Node::CallExpression(n) => n.expression.node_id() != node.node_id(),
            Node::NonNullExpression(n) => n.expression.node_id() != node.node_id(),
            _ => true,
        }
    }

    fn doWithConditionalBranches<T, F>(
        &mut self,
        action: F,
        value: T,
        trueTarget: FlowLabelId,
        falseTarget: FlowLabelId,
    ) where
        F: Fn(&mut Self, T),
    {
        let savedTrueTarget = self.currentTrueTarget;
        let savedFalseTarget = self.currentFalseTarget;
        self.currentTrueTarget = Some(trueTarget);
        self.currentFalseTarget = Some(falseTarget);
        action(self, value);
        self.currentTrueTarget = savedTrueTarget;
        self.currentFalseTarget = savedFalseTarget;
    }

    fn bindCondition(
        &mut self,
        node: Option<&Rc<BoundNode>>,
        trueTarget: FlowLabelId,
        falseTarget: FlowLabelId,
    ) {
        self.doWithConditionalBranches(|b, n| b.bind(n.cloned()), node, trueTarget, falseTarget);
        if node.is_none() || {
            let node = node.unwrap();
            !Self::isLogicalAssignmentExpression(node.node.clone())
                && !Self::isLogicalExpression(node.node.clone())
                && !(isOptionalChain(self.store.node_and_data(&node.node))
                    && isOutermostOptionalChain(
                        node,
                        self.store.node_and_data(&node.parent_node().unwrap()),
                    ))
        } {
            let true_antecedent =
                self.createFlowCondition(FlowFlags::TrueCondition, self.currentFlow, node);
            self.addAntecedent(trueTarget, true_antecedent);
            let false_antecedent =
                self.createFlowCondition(FlowFlags::FalseCondition, self.currentFlow, node);
            self.addAntecedent(falseTarget, false_antecedent);
        }
    }

    fn bindIterativeStatement(
        &mut self,
        node: Rc<BoundNode>,
        breakTarget: FlowLabelId,
        continueTarget: FlowLabelId,
    ) {
        let saveBreakTarget = self.currentBreakTarget;
        let saveContinueTarget = self.currentContinueTarget;
        self.currentBreakTarget = Some(breakTarget);
        self.currentContinueTarget = Some(continueTarget);
        self.bind(Some(node));
        self.currentBreakTarget = saveBreakTarget;
        self.currentContinueTarget = saveContinueTarget;
    }

    fn setContinueTarget(&mut self, mut node: Rc<BoundNode>, target: FlowLabelId) -> FlowLabelId {
        for label in self.activeLabelList.iter_mut().rev() {
            if let Some(Node::LabeledStatement(_)) = node.parent_node() {
                label.continueTarget = Some(target);
                node = node.parent().unwrap();
            } else {
                break;
            }
        }
        target
    }

    fn bindWhileStatement(&mut self, node: &Rc<BoundNode>, stmt: &Rc<WhileStatement>) {
        let continue_target = self.createLoopLabel();
        let preWhileLabel = self.setContinueTarget(node.clone(), continue_target);
        let preBodyLabel = self.createBranchLabel();
        let postWhileLabel = self.createBranchLabel();
        self.addAntecedent(preWhileLabel, self.currentFlow);
        self.currentFlow = preWhileLabel.into();
        self.bindCondition(
            Some(&stmt.expression.bind(node)),
            preBodyLabel,
            postWhileLabel,
        );
        self.currentFlow = self.finishFlowLabel(preBodyLabel);
        self.bindIterativeStatement(stmt.statement.bind(node), postWhileLabel, preWhileLabel);
        self.addAntecedent(preWhileLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(postWhileLabel);
    }

    fn bindDoStatement(&mut self, node: &Rc<BoundNode>, stmt: &Rc<DoStatement>) {
        let preDoLabel = self.createLoopLabel();
        let continue_target = self.createBranchLabel();
        let preConditionLabel = self.setContinueTarget(node.clone(), continue_target);
        let postDoLabel = self.createBranchLabel();
        self.addAntecedent(preDoLabel, self.currentFlow);
        self.currentFlow = preDoLabel.into();
        self.bindIterativeStatement(stmt.statement.bind(node), postDoLabel, preConditionLabel);
        self.addAntecedent(preConditionLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(preConditionLabel);
        self.bindCondition(Some(&stmt.expression.bind(node)), preDoLabel, postDoLabel);
        self.currentFlow = self.finishFlowLabel(postDoLabel);
    }

    fn bindForStatement(&mut self, node: &Rc<BoundNode>, stmt: &Rc<ForStatement>) {
        let continue_target = self.createLoopLabel();
        let preLoopLabel = self.setContinueTarget(node.clone(), continue_target);
        let preBodyLabel = self.createBranchLabel();
        let postLoopLabel = self.createBranchLabel();
        self.bind(stmt.initializer.bind(node));
        self.addAntecedent(preLoopLabel, self.currentFlow);
        self.currentFlow = preLoopLabel.into();
        self.bindCondition(
            stmt.condition.bind(node).as_ref(),
            preBodyLabel,
            postLoopLabel,
        );
        self.currentFlow = self.finishFlowLabel(preBodyLabel);
        self.bindIterativeStatement(stmt.statement.bind(node), postLoopLabel, preLoopLabel);
        self.bind(stmt.incrementor.bind(node));
        self.addAntecedent(preLoopLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(postLoopLabel);
    }

    fn bindForInOrForOfStatement(
        &mut self,
        node: &Rc<BoundNode>,
        expression: &Expression,
        awaitModifier: &Option<Rc<AwaitKeyword>>,
        statement: &Statement,
        initializer: &ForInitializer,
    ) {
        let continue_target = self.createLoopLabel();
        let preLoopLabel = self.setContinueTarget(node.clone(), continue_target);
        let postLoopLabel = self.createBranchLabel();
        self.bind(Some(expression.bind(node)));
        self.addAntecedent(preLoopLabel, self.currentFlow);
        self.currentFlow = preLoopLabel.into();
        self.bind(awaitModifier.bind(node));
        self.addAntecedent(postLoopLabel, self.currentFlow);
        self.bind(Some(initializer.bind(node)));
        if initializer.kind() != SyntaxKind::VariableDeclarationList {
            self.bindAssignmentTargetFlow(&initializer.bind(node));
        }
        self.bindIterativeStatement(statement.bind(node), postLoopLabel, preLoopLabel);
        self.addAntecedent(preLoopLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(postLoopLabel);
    }

    fn bindIfStatement(&mut self, node: &Rc<BoundNode>, stmt: &Rc<IfStatement>) {
        let thenLabel = self.createBranchLabel();
        let elseLabel = self.createBranchLabel();
        let postIfLabel = self.createBranchLabel();
        self.bindCondition(Some(&stmt.expression.bind(node)), thenLabel, elseLabel);
        self.currentFlow = self.finishFlowLabel(thenLabel);
        self.bind(Some(stmt.thenStatement.bind(node)));
        self.addAntecedent(postIfLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(elseLabel);
        self.bind(stmt.elseStatement.bind(node));
        self.addAntecedent(postIfLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(postIfLabel);
    }

    fn bindReturn(&mut self, node: &Rc<BoundNode>, stmt: &Rc<ReturnStatement>) {
        self.bind(stmt.expression.bind(node));
        self.hasExplicitReturn = true;
        if let Some(currentReturnTarget) = self.currentReturnTarget {
            self.addAntecedent(currentReturnTarget, self.currentFlow);
        }
        self.currentFlow = self.unreachableFlow;
    }

    fn bindThrow(&mut self, node: &Rc<BoundNode>, stmt: &Rc<ThrowStatement>) {
        self.bind(Some(stmt.expression.bind(node)));
        self.currentFlow = self.unreachableFlow;
    }

    fn findActiveLabel(&mut self, name: &__String) -> Option<&mut ActiveLabel> {
        self.activeLabelList
            .iter_mut()
            .rfind(|label| &label.name == name)
    }

    fn bindBreakOrContinueFlow(
        &mut self,
        node: BreakOrContinueStatement,
        breakTarget: Option<FlowLabelId>,
        continueTarget: Option<FlowLabelId>,
    ) {
        let flowLabel = if matches!(node, BreakOrContinueStatement::BreakStatement(_)) {
            breakTarget
        } else {
            continueTarget
        };
        if let Some(flowLabel) = flowLabel {
            self.addAntecedent(flowLabel, self.currentFlow);
            self.currentFlow = self.unreachableFlow;
        }
    }

    fn bindBreakOrContinueStatement(
        &mut self,
        node: &Rc<BoundNode>,
        stmt: BreakOrContinueStatement,
    ) {
        self.bind(stmt.label().bind(node));
        if let Some(label) = stmt.label() {
            if let Some(activeLabel) = self.findActiveLabel(&label.escapedText) {
                activeLabel.referenced = true;
                let breakTarget = activeLabel.breakTarget;
                let continueTarget = activeLabel.continueTarget;
                self.bindBreakOrContinueFlow(stmt, Some(breakTarget), continueTarget);
            }
        } else {
            self.bindBreakOrContinueFlow(stmt, self.currentBreakTarget, self.currentContinueTarget);
        }
    }

    fn bindTryStatement(&mut self, node: &Rc<BoundNode>, stmt: &Rc<TryStatement>) {
        // We conservatively assume that *any* code in the try block can cause an exception, but we only need
        // to track code that causes mutations (because only mutations widen the possible control flow type of
        // a variable). The exceptionLabel is the target label for control flows that result from exceptions.
        // We add all mutation flow nodes as antecedents of this label such that we can analyze them as possible
        // antecedents of the start of catch or finally blocks. Furthermore, we add the current control flow to
        // represent exceptions that occur before any mutations.
        let saveReturnTarget = self.currentReturnTarget;
        let saveExceptionTarget = self.currentExceptionTarget;
        let normalExitLabel = self.createBranchLabel();
        let returnLabel = self.createBranchLabel();
        let mut exceptionLabel = self.createBranchLabel();
        if stmt.finallyBlock.is_some() {
            self.currentReturnTarget = Some(returnLabel);
        }
        self.addAntecedent(exceptionLabel, self.currentFlow);
        self.currentExceptionTarget = Some(exceptionLabel);
        self.bind(Some(stmt.tryBlock.bind(node)));
        self.addAntecedent(normalExitLabel, self.currentFlow);
        if stmt.catchClause.is_some() {
            // Start of catch clause is the target of exceptions from try block.
            self.currentFlow = self.finishFlowLabel(exceptionLabel);
            // The currentExceptionTarget now represents control flows from exceptions in the catch clause.
            // Effectively, in a try-catch-finally, if an exception occurs in the try block, the catch block
            // acts like a second try block.
            exceptionLabel = self.createBranchLabel();
            self.addAntecedent(exceptionLabel, self.currentFlow);
            self.currentExceptionTarget = Some(exceptionLabel);
            self.bind(stmt.catchClause.bind(node));
            self.addAntecedent(normalExitLabel, self.currentFlow);
        }
        self.currentReturnTarget = saveReturnTarget;
        self.currentExceptionTarget = saveExceptionTarget;
        if stmt.finallyBlock.is_some() {
            // Possible ways control can reach the finally block:
            // 1) Normal completion of try block of a try-finally or try-catch-finally
            // 2) Normal completion of catch block (following exception in try block) of a try-catch-finally
            // 3) Return in try or catch block of a try-finally or try-catch-finally
            // 4) Exception in try block of a try-finally
            // 5) Exception in catch block of a try-catch-finally
            // When analyzing a control flow graph that starts inside a finally block we want to consider all
            // five possibilities above. However, when analyzing a control flow graph that starts outside (past)
            // the finally block, we only want to consider the first two (if we're past a finally block then it
            // must have completed normally). Likewise, when analyzing a control flow graph from return statements
            // in try or catch blocks in an IIFE, we only want to consider the third. To make this possible, we
            // inject a ReduceLabel node into the control flow graph. This node contains an alternate reduced
            // set of antecedents for the pre-finally label. As control flow analysis passes by a ReduceLabel
            // node, the pre-finally label is temporarily switched to the reduced antecedent set.
            let finallyLabel = self.createBranchLabel();
            let mut finally_label_antecedents = Vec::new();
            finally_label_antecedents.extend(
                self.flow_nodes[normalExitLabel]
                    .antecedents
                    .iter()
                    .flat_map(|a| a.iter()),
            );
            finally_label_antecedents.extend(
                self.flow_nodes[exceptionLabel]
                    .antecedents
                    .iter()
                    .flat_map(|a| a.iter()),
            );
            finally_label_antecedents.extend(
                self.flow_nodes[returnLabel]
                    .antecedents
                    .iter()
                    .flat_map(|a| a.iter()),
            );
            self.flow_nodes[finallyLabel].antecedents = Some(finally_label_antecedents.into());
            self.currentFlow = finallyLabel.into();
            self.bind(stmt.finallyBlock.bind(node));
            if self.flow_nodes[self.currentFlow]
                .flags
                .intersects(FlowFlags::Unreachable)
            {
                // If the end of the finally block is unreachable, the end of the entire try statement is unreachable.
                self.currentFlow = self.unreachableFlow;
            } else {
                // If we have an IIFE return target and return statements in the try or catch blocks, add a control
                // flow that goes back through the finally block and back through only the return statements.
                if let Some(currentReturnTarget) = self.currentReturnTarget {
                    if let Some(antecedents) = &self.flow_nodes[returnLabel].antecedents {
                        let antecedent = self.createReduceLabel(
                            finallyLabel,
                            antecedents.clone(),
                            self.currentFlow,
                        );
                        self.addAntecedent(currentReturnTarget, antecedent);
                    }
                }
                // If we have an outer exception target (i.e. a containing try-finally or try-catch-finally), add a
                // control flow that goes back through the finally blok and back through each possible exception source.
                if let Some(currentExceptionTarget) = self.currentExceptionTarget {
                    if let Some(antecedents) = &self.flow_nodes[exceptionLabel].antecedents {
                        let antecedent = self.createReduceLabel(
                            finallyLabel,
                            antecedents.clone(),
                            self.currentFlow,
                        );
                        self.addAntecedent(currentExceptionTarget, antecedent);
                    }
                }
                // If the end of the finally block is reachable, but the end of the try and catch blocks are not,
                // convert the current flow to unreachable. For example, 'try { return 1; } finally { ... }' should
                // result in an unreachable current control flow.
                self.currentFlow =
                    if let Some(antecedents) = &self.flow_nodes[normalExitLabel].antecedents {
                        self.createReduceLabel(finallyLabel, antecedents.clone(), self.currentFlow)
                    } else {
                        self.unreachableFlow
                    };
            }
        } else {
            self.currentFlow = self.finishFlowLabel(normalExitLabel);
        }
    }

    fn bindSwitchStatement(&mut self, node: &Rc<BoundNode>, stmt: &Rc<SwitchStatement>) {
        let postSwitchLabel = self.createBranchLabel();
        self.bind(Some(stmt.expression.bind(node)));
        let saveBreakTarget = self.currentBreakTarget;
        let savePreSwitchCaseFlow = self.preSwitchCaseFlow;
        self.currentBreakTarget = Some(postSwitchLabel);
        self.preSwitchCaseFlow = Some(self.currentFlow);
        self.bind(Some(stmt.caseBlock.bind(node)));
        self.addAntecedent(postSwitchLabel, self.currentFlow);
        let hasDefault = stmt
            .caseBlock
            .clauses
            .iter()
            .any(|c| matches!(c, CaseOrDefaultClause::DefaultClause(_)));
        // We mark a switch statement as possibly exhaustive if it has no default clause and if all
        // case clauses have unreachable end points (e.g. they all return). Note, we no longer need
        // this property in control flow analysis, it's there only for backwards compatibility.
        // TODO: possiblyExhaustive
        // stmt.possiblyExhaustive =
        //     !hasDefault && self.flow_nodes[postSwitchLabel].antecedents.is_none();
        if !hasDefault {
            let antecedent =
                self.createFlowSwitchClause(self.preSwitchCaseFlow.unwrap(), stmt.clone(), 0, 0);
            self.addAntecedent(postSwitchLabel, antecedent);
        }
        self.currentBreakTarget = saveBreakTarget;
        self.preSwitchCaseFlow = savePreSwitchCaseFlow;
        self.currentFlow = self.finishFlowLabel(postSwitchLabel);
    }

    fn bindCaseBlock(&mut self, node: &Rc<BoundNode>, case: &Rc<CaseBlock>) {
        let clauses = &case.clauses;
        let parent = node.parent().unwrap();
        let parent_switch = unwrap_as!(&parent.node, Node::SwitchStatement(n), n);
        let isNarrowingSwitch =
            self.isNarrowingExpression(&parent_switch.expression.clone().into());
        let mut fallthroughFlow = self.unreachableFlow;
        let mut i = 0;
        while i < clauses.len() {
            let clauseStart = i;
            while clauses[i].statements().is_empty() && i + 1 < clauses.len() {
                self.bind(Some(clauses[i].bind(node)));
                i += 1;
            }
            let preCaseLabel = self.createBranchLabel();
            let pre_case_label_antecedent = if isNarrowingSwitch {
                self.createFlowSwitchClause(
                    self.preSwitchCaseFlow.unwrap(),
                    parent_switch.clone(),
                    clauseStart as u32,
                    i as u32 + 1,
                )
            } else {
                self.preSwitchCaseFlow.unwrap()
            };
            self.addAntecedent(preCaseLabel, pre_case_label_antecedent);
            self.addAntecedent(preCaseLabel, fallthroughFlow);
            self.currentFlow = self.finishFlowLabel(preCaseLabel);
            let clause = &clauses[i];
            self.bind(Some(clause.bind(node)));
            fallthroughFlow = self.currentFlow;
            if !self.flow_nodes[self.currentFlow]
                .flags
                .intersects(FlowFlags::Unreachable)
                && i != clauses.len() - 1
                && self.options.noFallthroughCasesInSwitch
            {
                todo!();
                // clause.fallthroughFlowNode = self.currentFlow;
            }
            i += 1;
        }
    }

    fn bindCaseClause(&mut self, node: &Rc<BoundNode>, clause: &Rc<CaseClause>) {
        let saveCurrentFlow = self.currentFlow;
        self.currentFlow = self.preSwitchCaseFlow.unwrap();
        self.bind(Some(clause.expression.bind(node)));
        self.currentFlow = saveCurrentFlow;
        self.bindEach(Some(node), Some(&clause.statements));
    }

    fn bindExpressionStatement(&mut self, node: &Rc<BoundNode>, stmt: &Rc<ExpressionStatement>) {
        self.bind(Some(stmt.expression.bind(node)));
        self.maybeBindExpressionFlowIfCall(&stmt.expression);
    }

    fn maybeBindExpressionFlowIfCall(&mut self, node: &Expression) {
        // A top level or LHS of comma expression call expression with a dotted function name and at least one argument
        // is potentially an assertion and is therefore included in the control flow.
        if let Expression::CallExpression(call) = node {
            if call.expression.kind() != SyntaxKind::SuperKeyword
                && isDottedName(&call.expression.clone().into())
            {
                self.currentFlow = self.createFlowCall(self.currentFlow, call.clone());
            }
        }
    }

    fn bindLabeledStatement(&mut self, node: &Rc<BoundNode>, stmt: &Rc<LabeledStatement>) {
        let postStatementLabel = self.createBranchLabel();
        self.activeLabelList.push(ActiveLabel {
            name: stmt.label.escapedText.clone(),
            breakTarget: postStatementLabel,
            continueTarget: None,
            referenced: false,
        });
        self.bind(Some(stmt.label.bind(node)));
        self.bind(Some(stmt.statement.bind(node)));
        if !self.activeLabelList.last().unwrap().referenced && !self.options.allowUnusedLabels {
            todo!();
            // errorOrSuggestionOnNode(unusedLabelIsError(options), node.label, Diagnostics.Unused_label);
        }
        self.activeLabelList.pop();
        self.addAntecedent(postStatementLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(postStatementLabel);
    }

    fn bindDestructuringTargetFlow(&mut self, node: &Rc<BoundNode>) {
        if let Node::BinaryExpression(n) = &node.node {
            if n.operatorToken.kind() == SyntaxKind::EqualsToken {
                return self.bindAssignmentTargetFlow(&n.left.bind(node));
            }
        }
        self.bindAssignmentTargetFlow(node);
    }

    fn bindAssignmentTargetFlow(&mut self, node: &Rc<BoundNode>) {
        if self.isNarrowableReference(&node.node) {
            self.currentFlow = self.createFlowAssignment(self.currentFlow, node.clone());
        } else if let Node::ArrayLiteralExpression(n) = &node.node {
            for e in n.elements.iter() {
                if let Expression::SpreadElement(e) = e {
                    self.bindAssignmentTargetFlow(&e.expression.bind(&e.bind(node)));
                } else {
                    self.bindDestructuringTargetFlow(&e.bind(&node));
                }
            }
        } else if let Node::ObjectLiteralExpression(n) = &node.node {
            for p in n.properties.iter() {
                if let ObjectLiteralElementLike::PropertyAssignment(p) = p {
                    self.bindDestructuringTargetFlow(&p.initializer.bind(&p.bind(node)));
                } else if let ObjectLiteralElementLike::ShorthandPropertyAssignment(p) = p {
                    self.bindAssignmentTargetFlow(&p.name.bind(&p.bind(node)));
                } else if let ObjectLiteralElementLike::SpreadAssignment(p) = p {
                    self.bindAssignmentTargetFlow(&p.expression.bind(&p.bind(node)));
                }
            }
        }
    }

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

    fn bindPrefixUnaryExpressionFlow(
        &mut self,
        node: &Rc<BoundNode>,
        expr: &Rc<PrefixUnaryExpression>,
    ) {
        if expr.operator == SyntaxKind::ExclamationToken {
            let saveTrueTarget = self.currentTrueTarget;
            self.currentTrueTarget = self.currentFalseTarget;
            self.currentFalseTarget = saveTrueTarget;
            self.bindEachChild(node.parent().as_ref(), node.node.clone());
            self.currentFalseTarget = self.currentTrueTarget;
            self.currentTrueTarget = saveTrueTarget;
        } else {
            self.bindEachChild(node.parent().as_ref(), node.node.clone());
            if expr.operator == SyntaxKind::PlusPlusToken
                || expr.operator == SyntaxKind::MinusMinusToken
            {
                self.bindAssignmentTargetFlow(&expr.operand.bind(node));
            }
        }
    }

    fn bindPostfixUnaryExpressionFlow(
        &mut self,
        node: &Rc<BoundNode>,
        expr: &Rc<PostfixUnaryExpression>,
    ) {
        self.bindEachChild(node.parent().as_ref(), node.node.clone());
        if expr.operator == SyntaxKind::PlusPlusToken
            || expr.operator == SyntaxKind::MinusMinusToken
        {
            self.bindAssignmentTargetFlow(&expr.operand.bind(node));
        }
    }

    fn bindDestructuringAssignmentFlow(
        &mut self,
        node: &Rc<BoundNode>,
        expr: &Rc<BinaryExpression>,
    ) {
        if self.inAssignmentPattern {
            self.inAssignmentPattern = false;
            self.bind(Some(expr.operatorToken.bind(node)));
            self.bind(Some(expr.right.bind(node)));
            self.inAssignmentPattern = true;
            self.bind(Some(expr.left.bind(node)));
        } else {
            self.inAssignmentPattern = true;
            self.bind(Some(expr.left.bind(node)));
            self.inAssignmentPattern = false;
            self.bind(Some(expr.operatorToken.bind(node)));
            self.bind(Some(expr.right.bind(node)));
        }
        self.bindAssignmentTargetFlow(&expr.left.bind(node));
    }

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

    fn bindDeleteExpressionFlow(&mut self, node: &Rc<BoundNode>, expr: &Rc<DeleteExpression>) {
        self.bindEachChild(node.parent().as_ref(), node.node.clone());
        if expr.expression.kind() == SyntaxKind::PropertyAccessExpression {
            self.bindAssignmentTargetFlow(&expr.expression.bind(node));
        }
    }

    fn bindConditionalExpressionFlow(
        &mut self,
        node: &Rc<BoundNode>,
        expr: &Rc<ConditionalExpression>,
    ) {
        let trueLabel = self.createBranchLabel();
        let falseLabel = self.createBranchLabel();
        let postExpressionLabel = self.createBranchLabel();
        self.bindCondition(Some(&expr.condition.bind(node)), trueLabel, falseLabel);
        self.currentFlow = self.finishFlowLabel(trueLabel);
        self.bind(Some(expr.questionToken.bind(node)));
        self.bind(Some(expr.whenTrue.bind(node)));
        self.addAntecedent(postExpressionLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(falseLabel);
        self.bind(Some(expr.colonToken.bind(node)));
        self.bind(Some(expr.whenFalse.bind(node)));
        self.addAntecedent(postExpressionLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(postExpressionLabel);
    }

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

    fn bindBindingElementFlow(&mut self, node: &Rc<BoundNode>, element: &Rc<BindingElement>) {
        if isBindingPattern(Some(&element.name)) {
            // When evaluating a binding pattern, the initializer is evaluated before the binding pattern, per:
            // - https://tc39.es/ecma262/#sec-destructuring-binding-patterns-runtime-semantics-iteratorbindinginitialization
            //   - `BindingElement: BindingPattern Initializer?`
            // - https://tc39.es/ecma262/#sec-runtime-semantics-keyedbindinginitialization
            //   - `BindingElement: BindingPattern Initializer?`
            // TODO:
            // self.bindEach(element.decorators);
            // self.bindEach(element.modifiers);
            self.bind(element.dotDotDotToken.bind(node));
            self.bind(element.propertyName.bind(node));
            self.bind(element.initializer.bind(node));
            self.bind(Some(element.name.bind(node)));
        } else {
            self.bindEachChild(node.parent().as_ref(), node.node.clone());
        }
    }

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

    fn bindOptionalExpression(
        &mut self,
        node: &Rc<BoundNode>,
        trueTarget: FlowLabelId,
        falseTarget: FlowLabelId,
    ) {
        self.doWithConditionalBranches(
            |b, n| b.bind(Some(n.clone())),
            node,
            trueTarget,
            falseTarget,
        );
        if !isOptionalChain(self.store.node_and_data(&node.node))
            || isOutermostOptionalChain(
                node,
                self.store.node_and_data(&node.parent_node().unwrap()),
            )
        {
            let true_antecedent =
                self.createFlowCondition(FlowFlags::TrueCondition, self.currentFlow, Some(node));
            self.addAntecedent(trueTarget, true_antecedent);
            let false_antecedent =
                self.createFlowCondition(FlowFlags::FalseCondition, self.currentFlow, Some(node));
            self.addAntecedent(falseTarget, false_antecedent);
        }
    }

    fn bindOptionalChainRest(&mut self, node: &Rc<BoundNode>, chain: OptionalChain) {
        match chain {
            OptionalChain::PropertyAccessExpression(n) => {
                self.bind(n.questionDotToken.bind(node));
                self.bind(Some(n.name.bind(node)));
            }
            OptionalChain::ElementAccessExpression(n) => {
                self.bind(n.questionDotToken.bind(node));
                self.bind(Some(n.argumentExpression.bind(node)));
            }
            OptionalChain::CallExpression(n) => {
                self.bind(n.questionDotToken.bind(node));
                self.bindEach(Some(&n.bind(node)), n.typeArguments.as_ref());
                self.bindEach(Some(&n.bind(node)), Some(&n.arguments));
            }
            OptionalChain::NonNullExpression(_) => {}
        }
    }

    fn bindOptionalChain(
        &mut self,
        node: &Rc<BoundNode>,
        chain: OptionalChain,
        trueTarget: FlowLabelId,
        falseTarget: FlowLabelId,
    ) {
        // For an optional chain, we emulate the behavior of a logical expression:
        //
        // a?.b         -> a && a.b
        // a?.b.c       -> a && a.b.c
        // a?.b?.c      -> a && a.b && a.b.c
        // a?.[x = 1]   -> a && a[x = 1]
        //
        // To do this we descend through the chain until we reach the root of a chain (the expression with a `?.`)
        // and build it's CFA graph as if it were the first condition (`a && ...`). Then we bind the rest
        // of the node as part of the "true" branch, and continue to do so as we ascend back up to the outermost
        // chain node. We then treat the entire node as the right side of the expression.
        let preChainLabel = if isOptionalChainRoot(self.store.node_and_data(&node.node)) {
            Some(self.createBranchLabel())
        } else {
            None
        };
        self.bindOptionalExpression(
            &chain.expression().bind(node),
            preChainLabel.unwrap_or(trueTarget),
            falseTarget,
        );
        if let Some(preChainLabel) = preChainLabel {
            self.currentFlow = self.finishFlowLabel(preChainLabel);
        }
        self.doWithConditionalBranches(
            |b, (node, chain)| b.bindOptionalChainRest(node, chain),
            (node, chain),
            trueTarget,
            falseTarget,
        );
        if isOutermostOptionalChain(node, self.store.node_and_data(&node.parent_node().unwrap())) {
            let true_antecedent =
                self.createFlowCondition(FlowFlags::TrueCondition, self.currentFlow, Some(node));
            self.addAntecedent(trueTarget, true_antecedent);
            let false_antecedent =
                self.createFlowCondition(FlowFlags::FalseCondition, self.currentFlow, Some(node));
            self.addAntecedent(falseTarget, false_antecedent);
        }
    }

    fn bindOptionalChainFlow(&mut self, node: &Rc<BoundNode>, chain: OptionalChain) {
        if Self::isTopLevelLogicalExpression(node.clone()) {
            let postExpressionLabel = self.createBranchLabel();
            self.bindOptionalChain(node, chain, postExpressionLabel, postExpressionLabel);
            self.currentFlow = self.finishFlowLabel(postExpressionLabel);
        } else {
            self.bindOptionalChain(
                node,
                chain,
                self.currentTrueTarget.unwrap(),
                self.currentFalseTarget.unwrap(),
            );
        }
    }

    fn bindNonNullExpressionFlow(&mut self, node: &Rc<BoundNode>) {
        if let Some(chain) = asOptionalChain(self.store.node_and_data(&node.node)) {
            self.bindOptionalChainFlow(node, chain);
        } else {
            self.bindEachChild(node.parent().as_ref(), node.node.clone());
        }
    }

    fn bindAccessExpressionFlow(&mut self, node: &Rc<BoundNode>) {
        if let Some(optional_chain) = asOptionalChain(self.store.node_and_data(&node.node)) {
            self.bindOptionalChainFlow(node, optional_chain);
        } else {
            self.bindEachChild(node.parent().as_ref(), node.node.clone());
        }
    }

    fn bindCallExpressionFlow(&mut self, node: &Rc<BoundNode>, call: &Rc<CallExpression>) {
        if let Some(chain) = asOptionalChain(self.store.node_and_data(&node.node)) {
            self.bindOptionalChainFlow(node, chain);
        } else {
            // If the target of the call expression is a function expression or arrow function we have
            // an immediately invoked function expression (IIFE). Initialize the flowNode property to
            // the current control flow (which includes evaluation of the IIFE arguments).
            let expr = skipParentheses(call.expression.clone().into(), false);
            if expr.kind() == SyntaxKind::FunctionExpression
                || expr.kind() == SyntaxKind::ArrowFunction
            {
                self.bindEach(Some(node), call.typeArguments.as_ref());
                self.bindEach(Some(node), Some(&call.arguments));
                self.bind(Some(call.expression.bind(node)));
            } else {
                self.bindEachChild(node.parent().as_ref(), node.node.clone());
                if call.expression.kind() == SyntaxKind::SuperKeyword {
                    self.currentFlow = self.createFlowCall(self.currentFlow, call.clone());
                }
            }
        }
        if let LeftHandSideExpression::PropertyAccessExpression(propertyAccess) = &call.expression {
            if let MemberName::Identifier(name) = &propertyAccess.name {
                if self.isNarrowableOperand(&propertyAccess.expression.clone().into())
                    && isPushOrUnshiftIdentifier(name)
                {
                    self.currentFlow = self.createFlowArrayMutation(self.currentFlow, node.clone());
                }
            }
        }
    }

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
        let container = self.container.clone().unwrap();
        match &container.node {
            // Modules, source files, and classes need specialized handling for how their
            // members are declared (for example, a member of a class will go into a specific
            // symbol table depending on if it is static or not). We defer to specialized
            // handlers to take care of declaring these child members.
            Node::ModuleDeclaration(_) => {
                Some(self.declareModuleMember(node, symbolFlags, symbolExcludes))
            }
            Node::SourceFile(_) => {
                Some(self.declareSourceFileMember(node, symbolFlags, symbolExcludes))
            }
            Node::ClassExpression(_) | Node::ClassDeclaration(_) => {
                Some(self.declareClassMember(node, symbolFlags, symbolExcludes))
            }
            Node::EnumDeclaration(_) => {
                let container_symbol = self.node_data(&container).symbol.unwrap();
                let container_exports = self.symbols[container_symbol].exports.unwrap();
                Some(self.declareSymbol(
                    container_exports,
                    Some(container_symbol),
                    node,
                    symbolFlags,
                    symbolExcludes,
                    false,
                    false,
                ))
            }
            // TODO: jsdoc
            // Node::JSDocTypeLiteral(_)|
            // TODO: jsx
            // Node::JsxAttributes(_)|
            Node::TypeLiteralNode(_)
            | Node::ObjectLiteralExpression(_)
            | Node::InterfaceDeclaration(_) => {
                let container_symbol = self.node_data(&container).symbol.unwrap();
                let container_members = self.symbols[container_symbol].members.unwrap();
                // Interface/Object-types always have their children added to the 'members' of
                // their container. They are only accessible through an instance of their
                // container, and are never in scope otherwise (even inside the body of the
                // object / type / interface declaring them). An exception is type parameters,
                // which are in scope without qualification (similar to 'locals').
                Some(self.declareSymbol(
                    container_members,
                    Some(container_symbol),
                    node,
                    symbolFlags,
                    symbolExcludes,
                    false,
                    false,
                ))
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
                let container_locals = self.node_data(&container).locals.unwrap();
                // All the children of these container types are never visible through another
                // symbol (i.e. through another symbol's 'exports' or 'members').  Instead,
                // they're only accessed 'lexically' (i.e. from code that exists underneath
                // their container in the tree). To accomplish this, we simply add their declared
                // symbol to the 'locals' of the container.  These symbols can then be found as
                // the type checker walks up the containers, checking them for matching names.
                Some(self.declareSymbol(
                    container_locals,
                    None,
                    node,
                    symbolFlags,
                    symbolExcludes,
                    false,
                    false,
                ))
            }
            _ => unreachable!("container must be declaration"),
        }
    }

    fn declareClassMember(
        &mut self,
        node: &Rc<BoundNode>,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) -> SymbolId {
        let container_symbol = self
            .node_data(&self.container.clone().unwrap())
            .symbol
            .unwrap();

        if isStatic(self.store.node_and_data(&node.node)) {
            let container_exports = self.symbols[container_symbol].exports.unwrap();
            self.declareSymbol(
                container_exports,
                Some(container_symbol),
                node,
                symbolFlags,
                symbolExcludes,
                false,
                false,
            )
        } else {
            let container_members = self.symbols[container_symbol].members.unwrap();
            self.declareSymbol(
                container_members,
                Some(container_symbol),
                node,
                symbolFlags,
                symbolExcludes,
                false,
                false,
            )
        }
    }

    fn declareSourceFileMember(
        &mut self,
        node: &Rc<BoundNode>,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) -> SymbolId {
        if isExternalModule(&self.file) {
            self.declareModuleMember(node, symbolFlags, symbolExcludes)
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

    fn hasExportDeclarations(node: &Node) -> bool {
        let statements = match node {
            Node::SourceFile(n) => Some(&n.statements),
            Node::ModuleDeclaration(n) => n.body.as_ref().map(|b| &b.statements),
            _ => unreachable!(),
        };
        if let Some(statements) = statements {
            statements
                .iter()
                .any(|s| isExportDeclaration(s) || isExportAssignment(s))
        } else {
            false
        }
    }

    fn setExportContextFlag(&mut self, node: &Node) {
        debug_assert!(matches!(
            node,
            Node::ModuleDeclaration(_) | Node::SourceFile(_)
        ));
        // A declaration source file or ambient module declaration that contains no export declarations (but possibly regular
        // declarations with export modifiers) is an export context in which declarations are implicitly exported.
        if self.node_data(node).flags.intersects(NodeFlags::Ambient)
            && !Self::hasExportDeclarations(node)
        {
            self.node_data_mut(node).flags |= NodeFlags::ExportContext;
        } else {
            self.node_data_mut(node).flags &= !NodeFlags::ExportContext;
        }
    }

    fn bindModuleDeclaration(&mut self, node: &Rc<BoundNode>, decl: &Rc<ModuleDeclaration>) {
        self.setExportContextFlag(&node.node);
        if isAmbientModule(self.store.node_and_data(&node.node)) {
            if hasSyntacticModifier(self.store.node_and_data(&node.node), ModifierFlags::Export) {
                todo!();
                // errorOnFirstToken(node, Diagnostics.export_modifier_cannot_be_applied_to_ambient_modules_and_module_augmentations_since_they_are_always_visible);
            }
            if isModuleAugmentationExternal(node, &mut self.store) {
                self.declareModuleSymbol(node, decl);
            } else {
                todo!();
                // let mut pattern = None;
                // if (node.name.kind == SyntaxKind::StringLiteral) {
                //     let text= node.name.text;
                //     pattern = tryParsePattern(text);
                //     if (pattern == undefined) {
                //         errorOnFirstToken(node.name, Diagnostics.Pattern_0_can_have_at_most_one_Asterisk_character, text);
                //     }
                // }

                // let symbol = self.declareSymbolAndAddToSymbolTable(node, SymbolFlags::ValueModule, SymbolFlags::ValueModuleExcludes)!;
                // file.patternAmbientModules = append<PatternAmbientModule>(file.patternAmbientModules, pattern && !isString(pattern) ? { pattern, symbol } : undefined);
            }
        } else {
            let state = self.declareModuleSymbol(node, decl);
            if state != ModuleInstanceState::NonInstantiated {
                let symbol = &mut self.symbols[self.store.node_data(node).symbol.unwrap()];
                // if module was already merged with some function, class or non-const enum, treat it as non-const-enum-only
                symbol.constEnumOnlyModule = Some(
                    !symbol.flags.intersects(SymbolFlags::Function | SymbolFlags::Class | SymbolFlags::RegularEnum)
                        // Current must be `const enum` only
                        && state == ModuleInstanceState::ConstEnumOnly
                        // Can't have been set to 'false' in a previous merged symbol. ('undefined' OK)
                        && symbol.constEnumOnlyModule != Some(false),
                );
            }
        }
    }

    fn declareModuleSymbol(
        &mut self,
        node: &Rc<BoundNode>,
        decl: &Rc<ModuleDeclaration>,
    ) -> ModuleInstanceState {
        let state = getModuleInstanceState(node, decl, &mut self.store, &mut None);
        let instantiated = state != ModuleInstanceState::NonInstantiated;
        self.declareSymbolAndAddToSymbolTable(
            node,
            if instantiated {
                SymbolFlags::ValueModule
            } else {
                SymbolFlags::NamespaceModule
            },
            if instantiated {
                SymbolFlags::ValueModuleExcludes
            } else {
                SymbolFlags::NamespaceModuleExcludes
            },
        );
        state
    }

    fn bindFunctionOrConstructorType(&mut self, node: &Rc<BoundNode>) {
        // For a given function symbol "<...>(...) => T" we want to generate a symbol identical
        // to the one we would get for: { <...>(...): T }
        //
        // We do that by making an anonymous type literal symbol, and then setting the function
        // symbol as its sole member. To the rest of the system, this symbol will be indistinguishable
        // from an actual type literal symbol you would have gotten had you used the long form.
        let name = self.getDeclarationName(node).unwrap(); // TODO: GH#18217
        let symbol = self.createSymbol(SymbolFlags::Signature, name);
        self.addDeclarationToSymbol(symbol, node, SymbolFlags::Signature);

        let typeLiteralSymbol = self.createSymbol(
            SymbolFlags::TypeLiteral,
            __String(InternalSymbolName::Type.into()),
        );
        self.addDeclarationToSymbol(typeLiteralSymbol, node, SymbolFlags::TypeLiteral);
        let members = self.createSymbolTable();
        self.symbols[typeLiteralSymbol].members = Some(members);
        self.symbol_tables[members].insert(self.symbols[symbol].escapedName.clone(), symbol);
    }

    fn bindObjectLiteralExpression(
        &mut self,
        node: &Rc<BoundNode>,
        object: &Rc<ObjectLiteralExpression>,
    ) {
        #[derive(PartialEq, Clone, Copy)]
        enum ElementKind {
            Property,
            Accessor,
        }

        if self.inStrictMode && !isAssignmentTarget(node.clone()) {
            let mut seen = FxHashMap::default();

            for prop in object.properties.iter() {
                let identifier = match prop {
                    ObjectLiteralElementLike::PropertyAssignment(n) => n.name.clone().into(),
                    ObjectLiteralElementLike::ShorthandPropertyAssignment(n) => {
                        n.name.clone().into()
                    }
                    ObjectLiteralElementLike::MethodDeclaration(n) => n.name.clone().into(),
                    ObjectLiteralElementLike::GetAccessorDeclaration(n) => n.name.clone().into(),
                    ObjectLiteralElementLike::SetAccessorDeclaration(n) => n.name.clone().into(),
                    ObjectLiteralElementLike::SpreadAssignment(_) => continue,
                };
                let identifier = match identifier {
                    Node::Identifier(i) => i,
                    _ => continue,
                };

                // ECMA-262 11.1.5 Object Initializer
                // If previous is not undefined then throw a SyntaxError exception if any of the following conditions are true
                // a.This production is contained in strict code and IsDataDescriptor(previous) is true and
                // IsDataDescriptor(propId.descriptor) is true.
                //    b.IsDataDescriptor(previous) is true and IsAccessorDescriptor(propId.descriptor) is true.
                //    c.IsAccessorDescriptor(previous) is true and IsDataDescriptor(propId.descriptor) is true.
                //    d.IsAccessorDescriptor(previous) is true and IsAccessorDescriptor(propId.descriptor) is true
                // and either both previous and propId.descriptor have[[Get]] fields or both previous and propId.descriptor have[[Set]] fields
                let currentKind = if prop.kind() == SyntaxKind::PropertyAssignment
                    || prop.kind() == SyntaxKind::ShorthandPropertyAssignment
                    || prop.kind() == SyntaxKind::MethodDeclaration
                {
                    ElementKind::Property
                } else {
                    ElementKind::Accessor
                };

                let existingKind = match seen.entry(identifier.escapedText.clone()) {
                    Entry::Occupied(entry) => *entry.get(),
                    Entry::Vacant(entry) => {
                        entry.insert(currentKind);
                        continue;
                    }
                };

                if currentKind == ElementKind::Property && existingKind == ElementKind::Property {
                    todo!();
                    // let span = getErrorSpanForNode(file, identifier);
                    // file.bindDiagnostics.push(createFileDiagnostic(file, span.start, span.length,
                    //     Diagnostics.An_object_literal_cannot_have_multiple_properties_with_the_same_name_in_strict_mode));
                }
            }
        }

        self.bindAnonymousDeclaration(
            node,
            SymbolFlags::ObjectLiteral,
            __String(InternalSymbolName::Object.into()),
        );
    }

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
        let blockScopeContainer = self.blockScopeContainer.clone().unwrap();
        match &blockScopeContainer.node {
            Node::ModuleDeclaration(_) => {
                self.declareModuleMember(node, symbolFlags, symbolExcludes);
                return;
            }
            Node::SourceFile(n) => {
                if isExternalOrCommonJsModule(n) {
                    self.declareModuleMember(node, symbolFlags, symbolExcludes);
                    return;
                }
            }
            _ => {}
        }
        let locals = match self.node_data(&blockScopeContainer).locals {
            Some(locals) => locals,
            None => {
                let locals = self.createSymbolTable();
                self.node_data_mut(&blockScopeContainer).locals = Some(locals);
                self.addToContainerChain(blockScopeContainer);
                locals
            }
        };
        self.declareSymbol(
            locals,
            None,
            node,
            symbolFlags,
            symbolExcludes,
            false,
            false,
        );
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

    //     function errorOrSuggestionOnNode(isError: boolean, node: Node, message: DiagnosticMessage) {
    //         errorOrSuggestionOnRange(isError, node, node, message);
    //     }

    //     function errorOrSuggestionOnRange(isError: boolean, startNode: Node, endNode: Node, message: DiagnosticMessage) {
    //         addErrorOrSuggestionDiagnostic(isError, { pos: getTokenPosOfNode(startNode, file), end: endNode.end }, message);
    //     }

    //     function addErrorOrSuggestionDiagnostic(isError: boolean, range: TextRange, message: DiagnosticMessage) {
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

    fn updateStrictModeStatementList(
        &mut self,
        parent: &Rc<BoundNode>,
        statements: &NodeArray<Statement>,
    ) {
        if !self.inStrictMode {
            for statement in statements.iter() {
                if !isPrologueDirective(&statement.clone().into()) {
                    return;
                }

                let node = statement.bind(parent);
                let stmt = unwrap_as!(&statement, Statement::ExpressionStatement(n), n);

                if self.isUseStrictPrologueDirective(&node, stmt) {
                    self.inStrictMode = true;
                    return;
                }
            }
        }
    }

    /// Should be called only on prologue directives (isPrologueDirective(node) should be true)
    fn isUseStrictPrologueDirective(
        &mut self,
        node: &Rc<BoundNode>,
        stmt: &Rc<ExpressionStatement>,
    ) -> bool {
        let expression = stmt.expression.bind(node);
        let nodeText = getSourceTextOfNodeFromSourceFile(
            &self.file,
            self.store.node_and_data(&expression),
            false,
        );

        // Note: the node text must be exactly "use strict" or 'use strict'.  It is not ok for the
        // string to contain unicode escapes (as per ES5).
        nodeText == "\"use strict\"" || nodeText == "'use strict'"
    }

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
                // TODO: check currentFlow
                if self.isNarrowableReference(&node.node) {
                    self.node_data_mut(node).flowNode = Some(self.currentFlow);
                }
                // if self.currentFlow && self.isNarrowableReference(&node.node) {
                //     self.node_data_mut(node).flowNode = Some(self.currentFlow);
                // }
                if isSpecialPropertyDeclaration(self.store.node_and_data(node)) {
                    self.bindSpecialPropertyDeclaration(node);
                }
                if isInJSFile(Some(self.store.node_and_data(node)))
                    && self.file.commonJsModuleIndicator.is_some()
                    && isModuleExportsAccessExpression(&node.node)
                    && {
                        let container = self.blockScopeContainer.as_ref().unwrap().node.clone();
                        self.lookupSymbolForName(&container, &__String("module".into()))
                            .is_none()
                    }
                {
                    let expression = match &node.node {
                        Node::PropertyAccessExpression(n) => &n.expression,
                        Node::ElementAccessExpression(n) => &n.expression,
                        _ => unreachable!(),
                    };
                    let symbolTable = self.store.node_data(&self.file).locals.unwrap();
                    self.declareSymbol(
                        symbolTable,
                        None,
                        &expression.bind(node),
                        SymbolFlags::FunctionScopedVariable | SymbolFlags::ModuleExports,
                        SymbolFlags::FunctionScopedVariableExcludes,
                        false,
                        false,
                    );
                }
            }
            Node::BinaryExpression(n) => {
                let specialKind =
                    getAssignmentDeclarationKind(self.store.node_and_data(&node.node));
                match specialKind {
                    AssignmentDeclarationKind::ExportsProperty => {
                        self.bindExportsPropertyAssignment(node, n);
                    }
                    AssignmentDeclarationKind::ModuleExports => {
                        self.bindModuleExportsAssignment(node, n);
                    }
                    AssignmentDeclarationKind::PrototypeProperty => {
                        self.bindPrototypePropertyAssignment(&n.left.bind(node));
                    }
                    AssignmentDeclarationKind::Prototype => {
                        self.bindPrototypeAssignment(node, n);
                    }
                    AssignmentDeclarationKind::ThisProperty => {
                        self.bindThisPropertyAssignment(node);
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
                                    self.bindThisPropertyAssignment(node);
                                } else {
                                    self.bindSpecialPropertyAssignment(node, n);
                                }
                            } else {
                                self.bindSpecialPropertyAssignment(node, n);
                            }
                        } else {
                            self.bindSpecialPropertyAssignment(node, n);
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
                self.bindFunctionOrConstructorType(node);
            }
            // TODO: jsdoc
            // | Node::JSDocTypeLiteral(_)
            Node::TypeLiteralNode(_) | Node::MappedTypeNode(_) => {
                self.bindAnonymousTypeWorker(node);
            }
            // TODO: jsdoc
            // Node::JSDocClassTag(_) => {
            //     todo!();
            //     // bindJSDocClassTag(node as JSDocClassTag);
            // }
            Node::ObjectLiteralExpression(n) => {
                self.bindObjectLiteralExpression(node, n);
            }
            Node::FunctionExpression(_) | Node::ArrowFunction(_) => {
                self.bindFunctionExpression(node);
            }

            Node::CallExpression(n) => {
                let assignmentKind =
                    getAssignmentDeclarationKind(self.store.node_and_data(&node.node));
                match assignmentKind {
                    AssignmentDeclarationKind::ObjectDefinePropertyValue => {
                        self.bindObjectDefinePropertyAssignment(node, n);
                    }
                    AssignmentDeclarationKind::ObjectDefinePropertyExports => {
                        self.bindObjectDefinePropertyExport(node, n);
                    }
                    AssignmentDeclarationKind::ObjectDefinePrototypeProperty => {
                        self.bindObjectDefinePrototypeProperty(node, n);
                    }
                    AssignmentDeclarationKind::None => {
                        if isInJSFile(Some(self.store.node_and_data(node))) {
                            self.bindCallExpression(&node.node);
                        }
                    }
                    _ => {
                        unreachable!("Unknown call expression assignment declaration kind");
                    }
                }
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
            Node::ModuleDeclaration(n) => {
                self.bindModuleDeclaration(node, n);
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
            Node::NamespaceExportDeclaration(n) => {
                self.bindNamespaceExportDeclaration(node, n);
            }
            Node::ImportClause(n) => {
                self.bindImportClause(node, n);
            }
            Node::ExportDeclaration(n) => {
                self.bindExportDeclaration(node, n);
            }
            Node::ExportAssignment(n) => {
                self.bindExportAssignment(node, n);
            }
            Node::SourceFile(n) => {
                self.updateStrictModeStatementList(node, &n.statements);
                self.bindSourceFileIfExternalModule();
            }
            Node::Block(n) => {
                if !isFunctionLikeOrClassStaticBlockDeclaration(node.parent().as_ref()) {
                    return;
                }
                self.updateStrictModeStatementList(node, &n.statements);
            }
            Node::ModuleBlock(n) => {
                self.updateStrictModeStatementList(node, &n.statements);
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

    fn bindAnonymousTypeWorker(&mut self, node: &Rc<BoundNode>) {
        self.bindAnonymousDeclaration(
            node,
            SymbolFlags::TypeLiteral,
            __String(InternalSymbolName::Type.into()),
        );
    }

    fn bindSourceFileIfExternalModule(&mut self) {
        self.setExportContextFlag(&self.file.clone().into());
        if isExternalModule(self.file.as_ref()) {
            self.bindSourceFileAsExternalModule();
        } else if isJsonSourceFile(self.file.as_ref()) {
            self.bindSourceFileAsExternalModule();
            // Create symbol equivalent for the module.exports = {}
            let originalSymbol = self.store.node_data(&self.file).symbol;
            let exports = self.symbols[originalSymbol.unwrap()].exports.unwrap();
            self.declareSymbol(
                exports,
                originalSymbol,
                &self.file.bind_to_opt_parent(None),
                SymbolFlags::Property,
                SymbolFlags::All,
                false,
                false,
            );
            self.store.node_data_mut(&self.file).symbol = originalSymbol;
        }
    }

    fn bindSourceFileAsExternalModule(&mut self) {
        self.bindAnonymousDeclaration(
            &self.file.bind_to_opt_parent(None),
            SymbolFlags::ValueModule,
            __String(format!("\"{}\"", removeFileExtension(&self.file.fileName)).into()),
        );
    }

    fn bindExportAssignment(&mut self, node: &Rc<BoundNode>, assignment: &Rc<ExportAssignment>) {
        let container_symbol = self
            .container
            .as_ref()
            .and_then(|c| self.store.node_data(c).symbol);
        let container_exports = container_symbol.and_then(|s| self.symbols[s].exports);
        if let (Some(container_symbol), Some(container_exports)) =
            (container_symbol, container_exports)
        {
            let flags = if exportAssignmentIsAlias(&node.node) {
                // An export default clause with an EntityNameExpression or a class expression exports all meanings of that identifier or expression;
                SymbolFlags::Alias
            } else {
                // An export default clause with any other expression exports a value
                SymbolFlags::Property
            };
            // If there is an `export default x;` alias declaration, can't `export default` anything else.
            // (In contrast, you can still have `export default function f() {}` and `export default interface I {}`.)
            let symbol = self.declareSymbol(
                container_exports,
                Some(container_symbol),
                node,
                flags,
                SymbolFlags::All,
                false,
                false,
            );

            if assignment.isExportEquals {
                // Will be an error later, since the module already has other exports. Just make sure this has a valueDeclaration set.
                setValueDeclaration(&mut self.symbols[symbol], node, &mut self.store);
            }
        } else {
            // Incorrect export assignment in some sort of block construct
            let name = self.getDeclarationName(node).unwrap();
            self.bindAnonymousDeclaration(node, SymbolFlags::Value, name);
        }
    }

    fn bindNamespaceExportDeclaration(
        &mut self,
        node: &Rc<BoundNode>,
        decl: &Rc<NamespaceExportDeclaration>,
    ) {
        if decl
            .modifiers
            .as_ref()
            .map(|m| !m.is_empty())
            .unwrap_or_default()
        {
            todo!();
            // file.bindDiagnostics.push(createDiagnosticForNode(node, Diagnostics.Modifiers_cannot_appear_here));
        }
        let mut diag = None;
        if let Some(Node::SourceFile(parent)) = node.parent_node() {
            if !isExternalModule(&parent) {
                diag = Some(Diagnostics::Global_module_exports_may_only_appear_in_module_files);
            } else if !parent.isDeclarationFile {
                diag =
                    Some(Diagnostics::Global_module_exports_may_only_appear_in_declaration_files);
            }
        } else {
            diag = Some(Diagnostics::Global_module_exports_may_only_appear_at_top_level);
        }
        if let Some(diag) = diag {
            todo!();
            // file.bindDiagnostics.push(createDiagnosticForNode(node, diag));
        } else {
            let file_symbol = self.store.node_data(&self.file).symbol.unwrap();
            let globalExports = match self.symbols[file_symbol].globalExports {
                Some(e) => e,
                None => {
                    let globalExports = self.createSymbolTable();
                    self.symbols[file_symbol].globalExports = Some(globalExports);
                    globalExports
                }
            };
            self.declareSymbol(
                globalExports,
                Some(file_symbol),
                node,
                SymbolFlags::Alias,
                SymbolFlags::AliasExcludes,
                false,
                false,
            );
        }
    }

    fn bindExportDeclaration(&mut self, node: &Rc<BoundNode>, decl: &Rc<ExportDeclaration>) {
        let container_symbol = self
            .container
            .as_ref()
            .and_then(|c| self.store.node_data(c).symbol);
        let container_exports = container_symbol.and_then(|s| self.symbols[s].exports);
        if let (Some(container_symbol), Some(container_exports)) =
            (container_symbol, container_exports)
        {
            if let Some(exportClause) = &decl.exportClause {
                if isNamespaceExport(exportClause) {
                    self.declareSymbol(
                        container_exports,
                        Some(container_symbol),
                        &exportClause.bind(node),
                        SymbolFlags::Alias,
                        SymbolFlags::AliasExcludes,
                        false,
                        false,
                    );
                }
            } else {
                // All export * declarations are collected in an __export symbol
                self.declareSymbol(
                    container_exports,
                    Some(container_symbol),
                    node,
                    SymbolFlags::ExportStar,
                    SymbolFlags::None,
                    false,
                    false,
                );
            }
        } else {
            // Export * in some sort of block construct
            let name = self.getDeclarationName(node).unwrap();
            self.bindAnonymousDeclaration(node, SymbolFlags::ExportStar, name);
        }
    }

    fn bindImportClause(&mut self, node: &Rc<BoundNode>, clause: &Rc<ImportClause>) {
        if clause.name.is_some() {
            self.declareSymbolAndAddToSymbolTable(
                node,
                SymbolFlags::Alias,
                SymbolFlags::AliasExcludes,
            );
        }
    }

    fn setCommonJsModuleIndicator(&mut self, node: &Node) -> bool {
        if self.file.externalModuleIndicator.is_some() {
            return false;
        }
        if self.file.commonJsModuleIndicator.is_none() {
            todo!();
            // self.file.commonJsModuleIndicator = node;
            // self.bindSourceFileAsExternalModule();
        }
        true
    }

    fn bindObjectDefinePropertyExport(&mut self, node: &Rc<BoundNode>, call: &Rc<CallExpression>) {
        if !self.setCommonJsModuleIndicator(&node.node) {
            return;
        }
        let symbol = self.forEachIdentifierInEntityName(
            &call.arguments[0].bind(node),
            None,
            |binder, id, symbol, _| {
                if let Some(symbol) = symbol {
                    binder.addDeclarationToSymbol(
                        symbol,
                        id,
                        SymbolFlags::Module | SymbolFlags::Assignment,
                    );
                }
                symbol
            },
        );
        if let Some(symbol) = symbol {
            let flags = SymbolFlags::Property | SymbolFlags::ExportValue;
            self.declareSymbol(
                self.symbols[symbol].exports.unwrap(),
                Some(symbol),
                node,
                flags,
                SymbolFlags::None,
                false,
                false,
            );
        }
    }

    fn bindExportsPropertyAssignment(
        &mut self,
        node: &Rc<BoundNode>,
        assignment: &Rc<BinaryExpression>,
    ) {
        // When we create a property via 'exports.foo = bar', the 'exports.foo' property access
        // expression is the declaration
        if !self.setCommonJsModuleIndicator(&node.node) {
            return;
        }
        let left = match &assignment.left {
            Expression::ElementAccessExpression(n) => n.expression.bind(&n.bind(node)),
            Expression::PropertyAccessExpression(n) => n.expression.bind(&n.bind(node)),
            _ => unreachable!(),
        };
        let symbol = self.forEachIdentifierInEntityName(&left, None, |binder, id, symbol, _| {
            if let Some(symbol) = symbol {
                binder.addDeclarationToSymbol(
                    symbol,
                    id,
                    SymbolFlags::Module | SymbolFlags::Assignment,
                );
            }
            symbol
        });
        if let Some(symbol) = symbol {
            let isAlias = isAliasableExpression(&assignment.right.clone().into())
                && (isExportsIdentifier(&left.node) || isModuleExportsAccessExpression(&left.node));
            let flags = if isAlias {
                SymbolFlags::Alias
            } else {
                SymbolFlags::Property | SymbolFlags::ExportValue
            };
            self.declareSymbol(
                self.symbols[symbol].exports.unwrap(),
                Some(symbol),
                &assignment.left.bind(node),
                flags,
                SymbolFlags::None,
                false,
                false,
            );
        }
    }

    fn bindModuleExportsAssignment(
        &mut self,
        node: &Rc<BoundNode>,
        assignment: &Rc<BinaryExpression>,
    ) {
        // A common practice in node modules is to set 'export = module.exports = {}', this ensures that 'exports'
        // is still pointing to 'module.exports'.
        // We do not want to consider this as 'export=' since a module can have only one of these.
        // Similarly we do not want to treat 'module.exports = exports' as an 'export='.
        if !self.setCommonJsModuleIndicator(&node.node) {
            return;
        }
        let assignedExpression = getRightMostAssignedExpression(assignment.right.bind(node));
        if isEmptyObjectLiteral(&assignedExpression.node)
            || self.container.as_ref().unwrap().node_id() == self.file.node_id()
                && self.isExportsOrModuleExportsOrAlias(
                    &self.file.clone(),
                    assignedExpression.node.clone(),
                )
        {
            return;
        }

        if let Node::ObjectLiteralExpression(assigned_object) = &assignedExpression.node {
            if assigned_object
                .properties
                .iter()
                .all(|p| matches!(p, ObjectLiteralElementLike::ShorthandPropertyAssignment(_)))
            {
                for prop in assigned_object.properties.iter() {
                    self.bindExportAssignedObjectMemberAlias(&prop.bind(&assignedExpression))
                }
                return;
            }
        }

        // 'module.exports = expr' assignment
        let flags = if exportAssignmentIsAlias(&node.node) {
            SymbolFlags::Alias
        }
        // An export= with an EntityNameExpression or a ClassExpression exports all meanings of that identifier or class
        else {
            SymbolFlags::Property | SymbolFlags::ExportValue | SymbolFlags::ValueModule
        };
        let file_symbol = self.store.node_data(&self.file).symbol.unwrap();
        let symbol = self.declareSymbol(
            self.symbols[file_symbol].exports.unwrap(),
            Some(file_symbol),
            node,
            flags | SymbolFlags::Assignment,
            SymbolFlags::None,
            false,
            false,
        );
        setValueDeclaration(&mut self.symbols[symbol], node, &mut self.store);
    }

    fn bindExportAssignedObjectMemberAlias(&mut self, node: &Rc<BoundNode>) {
        let file_symbol = self.store.node_data(&self.file).symbol.unwrap();
        self.declareSymbol(
            self.symbols[file_symbol].exports.unwrap(),
            Some(file_symbol),
            node,
            SymbolFlags::Alias | SymbolFlags::Assignment,
            SymbolFlags::None,
            false,
            false,
        );
    }

    fn bindThisPropertyAssignment(
        &mut self,
        node: &Rc<BoundNode>, /*BindablePropertyAssignmentExpression | PropertyAccessExpression | LiteralLikeElementAccessExpression*/
    ) {
        debug_assert!(isInJSFile(Some(self.store.node_and_data(node))));
        // private identifiers *must* be declared (even in JS files)
        let hasPrivateIdentifier = match &node.node {
            Node::BinaryExpression(n) => {
                if let Expression::PropertyAccessExpression(left) = &n.left {
                    isPrivateIdentifier(&left.name)
                } else {
                    false
                }
            }
            Node::PropertyAccessExpression(n) => isPrivateIdentifier(&n.name),
            _ => false,
        };
        if hasPrivateIdentifier {
            return;
        }
        let thisContainer = getThisContainer(node.clone(), false);
        match &thisContainer.node {
            Node::FunctionDeclaration(_) | Node::FunctionExpression(_) => {
                let mut constructorSymbol = self.node_data(&thisContainer).symbol;
                // For `f.prototype.m = function() { this.x = 0; }`, `this.x = 0` should modify `f`'s members, not the function expression.
                if let Some(Node::BinaryExpression(parent)) = thisContainer.parent_node() {
                    if parent.operatorToken.kind() == SyntaxKind::EqualsToken {
                        let l = &parent.left;
                        if isBindableStaticAccessExpression(&l.clone().into(), false) {
                            let bindable_static_access_expression = match l {
                                Expression::ElementAccessExpression(n) => &n.expression,
                                Expression::PropertyAccessExpression(n) => &n.expression,
                                _ => unreachable!(),
                            };
                            if isPrototypeAccess(&bindable_static_access_expression.clone().into())
                            {
                                let prototype_access_expression =
                                    match bindable_static_access_expression {
                                        LeftHandSideExpression::ElementAccessExpression(n) => {
                                            &n.expression
                                        }
                                        LeftHandSideExpression::PropertyAccessExpression(n) => {
                                            &n.expression
                                        }
                                        _ => unreachable!(),
                                    };
                                constructorSymbol = self.lookupSymbolForPropertyAccess(
                                    &prototype_access_expression.clone().into(),
                                    self.thisParentContainer.as_ref().map(|n| n.node.clone()),
                                );
                            }
                        }
                    }
                }

                if let Some(constructorSymbol) = constructorSymbol {
                    if let Some(valueDeclaration) =
                        self.symbols[constructorSymbol].valueDeclaration.clone()
                    {
                        // Declare a 'member' if the container is an ES5 class or ES6 constructor
                        let constructor_members = match self.symbols[constructorSymbol].members {
                            Some(m) => m,
                            None => {
                                let members = self.createSymbolTable();
                                self.symbols[constructorSymbol].members = Some(members);
                                members
                            }
                        };
                        // It's acceptable for multiple 'this' assignments of the same identifier to occur
                        if hasDynamicName(node) {
                            self.bindDynamicallyNamedThisPropertyAssignment(
                                node,
                                constructorSymbol,
                                constructor_members,
                            );
                        } else {
                            self.declareSymbol(
                                constructor_members,
                                Some(constructorSymbol),
                                node,
                                SymbolFlags::Property | SymbolFlags::Assignment,
                                SymbolFlags::PropertyExcludes & !SymbolFlags::Property,
                                false,
                                false,
                            );
                        }
                        self.addDeclarationToSymbol(
                            constructorSymbol,
                            &valueDeclaration,
                            SymbolFlags::Class,
                        );
                    }
                }
            }
            Node::ConstructorDeclaration(_)
            | Node::PropertyDeclaration(_)
            | Node::MethodDeclaration(_)
            | Node::GetAccessorDeclaration(_)
            | Node::SetAccessorDeclaration(_) => {
                // this.foo assignment in a JavaScript class
                // Bind this property to the containing class
                let containingClass = thisContainer.parent_node().unwrap();
                let containingClass_symbol = self.node_data(&containingClass).symbol.unwrap();
                let symbolTable = if isStatic(self.store.node_and_data(&thisContainer.node)) {
                    self.symbols[containingClass_symbol].exports.unwrap()
                } else {
                    self.symbols[containingClass_symbol].members.unwrap()
                };
                if hasDynamicName(node) {
                    self.bindDynamicallyNamedThisPropertyAssignment(
                        node,
                        containingClass_symbol,
                        symbolTable,
                    );
                } else {
                    self.declareSymbol(
                        symbolTable,
                        Some(containingClass_symbol),
                        node,
                        SymbolFlags::Property | SymbolFlags::Assignment,
                        SymbolFlags::None,
                        true,
                        false,
                    );
                }
            }
            Node::SourceFile(n) => {
                // this.property = assignment in a source file -- declare symbol in exports for a module, in locals for a script
                if hasDynamicName(node) {
                    return;
                } else if n.commonJsModuleIndicator.is_some() {
                    let thisContainer_symbol = self.node_data(n).symbol.unwrap();
                    let thisContainer_exports = self.symbols[thisContainer_symbol].exports.unwrap();
                    self.declareSymbol(
                        thisContainer_exports,
                        Some(thisContainer_symbol),
                        node,
                        SymbolFlags::Property | SymbolFlags::ExportValue,
                        SymbolFlags::None,
                        false,
                        false,
                    );
                } else {
                    self.declareSymbolAndAddToSymbolTable(
                        node,
                        SymbolFlags::FunctionScopedVariable,
                        SymbolFlags::FunctionScopedVariableExcludes,
                    );
                }
            }
            _ => unreachable!(),
        }
    }

    fn bindDynamicallyNamedThisPropertyAssignment(
        &mut self,
        node: &Rc<BoundNode>,
        symbol: SymbolId,
        symbolTable: SymbolTableId,
    ) {
        self.declareSymbol(
            symbolTable,
            Some(symbol),
            node,
            SymbolFlags::Property,
            SymbolFlags::None,
            true,
            true,
        );
        self.addLateBoundAssignmentDeclarationToSymbol(node, Some(symbol));
    }

    fn addLateBoundAssignmentDeclarationToSymbol(
        &mut self,
        node: &Rc<BoundNode>,
        symbol: Option<SymbolId>,
    ) {
        if let Some(symbol) = symbol {
            self.symbols[symbol]
                .assignmentDeclarationMembers
                .insert(node.node_id(), node.clone());
        }
    }

    fn bindSpecialPropertyDeclaration(&mut self, node: &Rc<BoundNode>) {
        let expression = match &node.node {
            Node::PropertyAccessExpression(n) => &n.expression,
            Node::ElementAccessExpression(n) => &n.expression,
            _ => unreachable!(),
        };
        if expression.kind() == SyntaxKind::ThisKeyword {
            self.bindThisPropertyAssignment(node);
        } else if isBindableStaticAccessExpression(&node.node, false)
            && matches!(
                node.parent().unwrap().parent_node(),
                Some(Node::SourceFile(_))
            )
        {
            if isPrototypeAccess(&expression.clone().into()) {
                self.bindPrototypePropertyAssignment(node);
            } else {
                self.bindStaticPropertyAssignment(node);
            }
        }
    }

    /** For `x.prototype = { p, ... }`, declare members p,... if `x` is function/class/{}, or not declared. */
    fn bindPrototypeAssignment(&mut self, node: &Rc<BoundNode>, assignment: &Rc<BinaryExpression>) {
        let left_expression = match &assignment.left {
            Expression::ElementAccessExpression(n) => &n.expression,
            Expression::PropertyAccessExpression(n) => &n.expression,
            _ => unreachable!(),
        };
        self.bindPropertyAssignment(
            &left_expression.clone().into(),
            &assignment.left.bind(node),
            false,
            true,
        );
    }

    fn bindObjectDefinePrototypeProperty(
        &mut self,
        node: &Rc<BoundNode>,
        call: &Rc<CallExpression>,
    ) {
        let arg = match &call.arguments[0] {
            Expression::ElementAccessExpression(n) => &n.expression,
            Expression::PropertyAccessExpression(n) => &n.expression,
            _ => unreachable!(),
        };
        let namespaceSymbol = self.lookupSymbolForPropertyAccess(&arg.clone().into(), None);
        if let Some(namespaceSymbol) = namespaceSymbol {
            if let Some(valueDeclaration) = self.symbols[namespaceSymbol].valueDeclaration.clone() {
                // Ensure the namespace symbol becomes class-like
                self.addDeclarationToSymbol(namespaceSymbol, &valueDeclaration, SymbolFlags::Class);
            }
        }
        self.bindPotentiallyNewExpandoMemberToNamespace(node, namespaceSymbol, true);
    }

    /**
     * For `x.prototype.y = z`, declare a member `y` on `x` if `x` is a function or class, or not declared.
     * Note that jsdoc preceding an ExpressionStatement like `x.prototype.y;` is also treated as a declaration.
     */
    fn bindPrototypePropertyAssignment(&mut self, lhs: &Rc<BoundNode>) {
        // Look up the function in the local scope, since prototype assignments should
        // follow the function declaration
        let classPrototype = match &lhs.node {
            Node::PropertyAccessExpression(n) => &n.expression,
            Node::ElementAccessExpression(n) => &n.expression,
            _ => unreachable!(),
        };
        let constructorFunction = match classPrototype {
            LeftHandSideExpression::PropertyAccessExpression(n) => &n.expression,
            LeftHandSideExpression::ElementAccessExpression(n) => &n.expression,
            _ => unreachable!(),
        };

        self.bindPropertyAssignment(&constructorFunction.clone().into(), lhs, true, true);
    }

    fn bindObjectDefinePropertyAssignment(
        &mut self,
        node: &Rc<BoundNode>,
        call: &Rc<CallExpression>,
    ) {
        let mut namespaceSymbol =
            self.lookupSymbolForPropertyAccess(&call.arguments[0].clone().into(), None);
        let isToplevel = matches!(
            node.parent().unwrap().parent_node(),
            Some(Node::SourceFile(_))
        );
        namespaceSymbol = self.bindPotentiallyMissingNamespaces(
            namespaceSymbol,
            &call.arguments[0].bind(node),
            isToplevel,
            false,
            false,
        );
        self.bindPotentiallyNewExpandoMemberToNamespace(node, namespaceSymbol, false);
    }

    fn bindSpecialPropertyAssignment(
        &mut self,
        node: &Rc<BoundNode>,
        assignment: &Rc<BinaryExpression>,
    ) {
        let container = self.container.as_ref().unwrap().node.clone();
        let property_access = match &assignment.left {
            Expression::ElementAccessExpression(n) => n.expression.bind(node),
            Expression::PropertyAccessExpression(n) => n.expression.bind(node),
            _ => unreachable!(),
        };
        // Class declarations in Typescript do not allow property declarations
        let parentSymbol = self
            .lookupSymbolForPropertyAccess(&property_access.node, Some(container.clone()))
            .or_else(|| {
                self.lookupSymbolForPropertyAccess(
                    &property_access.node,
                    self.blockScopeContainer.as_ref().map(|n| n.node.clone()),
                )
            });
        if !isInJSFile(Some(self.store.node_and_data(node)))
            && !isFunctionSymbol(parentSymbol.map(|s| &self.symbols[s]))
        {
            return;
        }
        let rootExpr = getLeftmostAccessExpression(assignment.left.clone().into());
        if let Node::Identifier(rootExpr) = rootExpr {
            let rootExpr_symbol_flags = self
                .lookupSymbolForName(&container, &rootExpr.escapedText)
                .map(|s| self.symbols[s].flags);
            if rootExpr_symbol_flags
                .unwrap_or_default()
                .intersects(SymbolFlags::Alias)
            {
                return;
            }
        }
        if isIdentifier(&property_access)
            && container.node_id() == self.file.node_id()
            && self
                .isExportsOrModuleExportsOrAlias(&self.file.clone(), property_access.node.clone())
        {
            // This can be an alias for the 'exports' or 'module.exports' names, e.g.
            //    var util = module.exports;
            //    util.property = function ...
            self.bindExportsPropertyAssignment(node, assignment);
        } else if hasDynamicName(node) {
            self.bindAnonymousDeclaration(
                node,
                SymbolFlags::Property | SymbolFlags::Assignment,
                __String(InternalSymbolName::Computed.into()),
            );
            let sym = self.bindPotentiallyMissingNamespaces(
                parentSymbol,
                &property_access,
                Self::isTopLevelNamespaceAssignment(&assignment.left.bind(node)),
                false,
                false,
            );
            self.addLateBoundAssignmentDeclarationToSymbol(node, sym);
        } else {
            self.bindStaticPropertyAssignment(&assignment.left.bind(node));
        }
    }

    /**
     * For nodes like `x.y = z`, declare a member 'y' on 'x' if x is a function (or IIFE) or class or {}, or not declared.
     * Also works for expression statements preceded by JSDoc, like / ** @type number * / x.y;
     */
    fn bindStaticPropertyAssignment(&mut self, node: &Rc<BoundNode>) {
        let expression = match &node.node {
            Node::ElementAccessExpression(n) => &n.expression,
            Node::PropertyAccessExpression(n) => &n.expression,
            _ => unreachable!(),
        };
        self.bindPropertyAssignment(&expression.clone().into(), node, false, false);
    }

    fn bindPotentiallyMissingNamespaces(
        &mut self,
        mut namespaceSymbol: Option<SymbolId>,
        entityName: &Rc<BoundNode>,
        isToplevel: bool,
        isPrototypeProperty: bool,
        containerIsClass: bool,
    ) -> Option<SymbolId> {
        if let Some(namespaceSymbol) = namespaceSymbol {
            if self.symbols[namespaceSymbol]
                .flags
                .intersects(SymbolFlags::Alias)
            {
                return Some(namespaceSymbol);
            }
        }
        if isToplevel && !isPrototypeProperty {
            // make symbols or add declarations for intermediate containers
            let flags = SymbolFlags::Module | SymbolFlags::Assignment;
            let excludeFlags = SymbolFlags::ValueModuleExcludes & !SymbolFlags::Assignment;
            namespaceSymbol = self.forEachIdentifierInEntityName(
                entityName,
                namespaceSymbol,
                |binder, id, symbol, parent| {
                    if let Some(symbol) = symbol {
                        binder.addDeclarationToSymbol(symbol, id, flags);
                        return Some(symbol);
                    } else {
                        let table = if let Some(parent) = parent {
                            binder.symbols[parent].exports.unwrap()
                        } else {
                            todo!();
                            // binder.file.jsGlobalAugmentations
                            //     || (binder.file.jsGlobalAugmentations = createSymbolTable())
                        };
                        return Some(binder.declareSymbol(
                            table,
                            parent,
                            id,
                            flags,
                            excludeFlags,
                            false,
                            false,
                        ));
                    }
                },
            );
        }
        if containerIsClass {
            if let Some(namespaceSymbol) = namespaceSymbol {
                if let Some(valueDeclaration) =
                    self.symbols[namespaceSymbol].valueDeclaration.clone()
                {
                    self.addDeclarationToSymbol(
                        namespaceSymbol,
                        &valueDeclaration,
                        SymbolFlags::Class,
                    );
                }
            }
        }
        namespaceSymbol
    }

    fn bindPotentiallyNewExpandoMemberToNamespace(
        &mut self,
        declaration: &Rc<BoundNode>,
        namespaceSymbol: Option<SymbolId>,
        isPrototypeProperty: bool,
    ) {
        let namespaceSymbol = match namespaceSymbol {
            Some(s) => s,
            None => return,
        };
        if !self.isExpandoSymbol(namespaceSymbol) {
            return;
        }

        // Set up the members collection if it doesn't exist already
        let symbolTable = if isPrototypeProperty {
            match self.symbols[namespaceSymbol].members {
                Some(t) => t,
                None => {
                    let table = self.createSymbolTable();
                    self.symbols[namespaceSymbol].members = Some(table);
                    table
                }
            }
        } else {
            match self.symbols[namespaceSymbol].exports {
                Some(t) => t,
                None => {
                    let table = self.createSymbolTable();
                    self.symbols[namespaceSymbol].exports = Some(table);
                    table
                }
            }
        };

        let mut includes = SymbolFlags::None;
        let mut excludes = SymbolFlags::None;
        // Method-like
        if isFunctionLikeDeclaration(
            getAssignedExpandoInitializer(Some(declaration.clone())).as_ref(),
        ) {
            includes = SymbolFlags::Method;
            excludes = SymbolFlags::MethodExcludes;
        }
        // Maybe accessor-like
        else if let Node::CallExpression(call) = &declaration.node {
            if isBindableObjectDefinePropertyCall(call) {
                if let Expression::ObjectLiteralExpression(attributes) = &call.arguments[2] {
                    if attributes.properties.iter().any(|p| {
                        getNameOfDeclaration(Some(&p.bind(&attributes.bind(declaration))))
                            .map(|id| isIdentifier(&id) && idText(&id).as_ref() == "set")
                            .unwrap_or_default()
                    }) {
                        // We mix in `SymbolFLags.Property` so in the checker `getTypeOfVariableParameterOrProperty` is used for this
                        // symbol, instead of `getTypeOfAccessor` (which will assert as there is no real accessor declaration)
                        includes |= SymbolFlags::SetAccessor | SymbolFlags::Property;
                        excludes |= SymbolFlags::SetAccessorExcludes;
                    }
                    if attributes.properties.iter().any(|p| {
                        getNameOfDeclaration(Some(&p.bind(&attributes.bind(declaration))))
                            .map(|id| isIdentifier(&id) && idText(&id).as_ref() == "get")
                            .unwrap_or_default()
                    }) {
                        includes |= SymbolFlags::GetAccessor | SymbolFlags::Property;
                        excludes |= SymbolFlags::GetAccessorExcludes;
                    }
                }
            }
        }

        if includes == SymbolFlags::None {
            includes = SymbolFlags::Property;
            excludes = SymbolFlags::PropertyExcludes;
        }

        self.declareSymbol(
            symbolTable,
            Some(namespaceSymbol),
            declaration,
            includes | SymbolFlags::Assignment,
            excludes & !SymbolFlags::Assignment,
            false,
            false,
        );
    }

    fn isTopLevelNamespaceAssignment(propertyAccess: &Rc<BoundNode>) -> bool {
        if let Some(Node::BinaryExpression(_)) = propertyAccess.parent_node() {
            let parentOfBinaryExpression =
                Self::getParentOfBinaryExpression(propertyAccess.parent().unwrap());
            matches!(
                parentOfBinaryExpression.parent_node(),
                Some(Node::SourceFile(_))
            )
        } else {
            matches!(
                propertyAccess.parent().unwrap().parent_node(),
                Some(Node::SourceFile(_))
            )
        }
    }

    fn bindPropertyAssignment(
        &mut self,
        name: &Node,
        propertyAccess: &Rc<BoundNode>,
        isPrototypeProperty: bool,
        containerIsClass: bool,
    ) {
        let mut namespaceSymbol = self
            .lookupSymbolForPropertyAccess(name, self.container.as_ref().map(|n| n.node.clone()))
            .or_else(|| {
                self.lookupSymbolForPropertyAccess(
                    name,
                    self.blockScopeContainer.as_ref().map(|n| n.node.clone()),
                )
            });
        let isToplevel = Self::isTopLevelNamespaceAssignment(propertyAccess);
        let expression = match &propertyAccess.node {
            Node::ElementAccessExpression(n) => &n.expression,
            Node::PropertyAccessExpression(n) => &n.expression,
            _ => unreachable!(),
        };
        namespaceSymbol = self.bindPotentiallyMissingNamespaces(
            namespaceSymbol,
            &expression.bind(propertyAccess),
            isToplevel,
            isPrototypeProperty,
            containerIsClass,
        );
        self.bindPotentiallyNewExpandoMemberToNamespace(
            propertyAccess,
            namespaceSymbol,
            isPrototypeProperty,
        );
    }

    /**
     * Javascript expando values are:
     * - Functions
     * - classes
     * - namespaces
     * - variables initialized with function expressions
     * -                       with class expressions
     * -                       with empty object literals
     * -                       with non-empty object literals if assigned to the prototype property
     */
    fn isExpandoSymbol(&mut self, symbol: SymbolId) -> bool {
        if self.symbols[symbol]
            .flags
            .intersects(SymbolFlags::Function | SymbolFlags::Class | SymbolFlags::NamespaceModule)
        {
            return true;
        }
        let valueDeclaration = self.symbols[symbol].valueDeclaration.clone();
        let valueDeclaration_node = valueDeclaration.as_ref().map(|n| &n.node);
        if matches!(valueDeclaration_node, Some(Node::CallExpression(_))) {
            return getAssignedExpandoInitializer(valueDeclaration).is_some();
        }
        let init = match valueDeclaration_node {
            Some(Node::VariableDeclaration(n)) => {
                n.initializer.bind_to_opt_parent(valueDeclaration.as_ref())
            }
            Some(Node::BinaryExpression(n)) => {
                Some(n.right.bind_to_opt_parent(valueDeclaration.as_ref()))
            }
            Some(Node::PropertyAccessExpression(_)) => {
                let parent = valueDeclaration.as_ref().unwrap().parent();
                match parent.as_ref().map(|p| &p.node) {
                    Some(Node::BinaryExpression(p)) => {
                        Some(p.right.bind_to_opt_parent(parent.as_ref()))
                    }
                    _ => None,
                }
            }
            _ => None,
        };
        let init = init.map(|init| getRightMostAssignedExpression(init));
        if let Some(init) = init {
            let valueDeclaration = valueDeclaration.unwrap();
            let node = match &valueDeclaration.node {
                Node::VariableDeclaration(n) => n.name.clone().into(),
                Node::BinaryExpression(n) => n.left.clone().into(),
                _ => valueDeclaration.node.clone(),
            };
            let isPrototypeAssignment = isPrototypeAccess(&node);
            let mut initializer = init;
            if let Node::BinaryExpression(i) = &initializer.node {
                if i.operatorToken.kind() == SyntaxKind::BarBarToken
                    || i.operatorToken.kind() == SyntaxKind::QuestionQuestionToken
                {
                    initializer = i.right.bind(&initializer);
                }
            }
            return getExpandoInitializer(&initializer, isPrototypeAssignment).is_some();
        }
        false
    }

    fn getParentOfBinaryExpression(mut expr: Rc<BoundNode>) -> Rc<BoundNode> {
        while matches!(expr.parent_node(), Some(Node::BinaryExpression(_))) {
            expr = expr.parent().unwrap();
        }
        expr.parent().unwrap()
    }

    fn lookupSymbolForPropertyAccess(
        &mut self,
        node: &Node,
        lookupContainer: Option<Node>,
    ) -> Option<SymbolId> {
        let lookupContainer =
            lookupContainer.unwrap_or_else(|| self.container.as_ref().unwrap().node.clone());
        if let Node::Identifier(node) = node {
            self.lookupSymbolForName(&lookupContainer, &node.escapedText)
        } else {
            let expression = match node {
                Node::ElementAccessExpression(n) => &n.expression,
                Node::PropertyAccessExpression(n) => &n.expression,
                _ => unreachable!(),
            };
            let symbol = self.lookupSymbolForPropertyAccess(&expression.clone().into(), None);
            symbol
                .and_then(|s| self.symbols[s].exports)
                .and_then(|e| {
                    self.symbol_tables[e].get(&getElementOrPropertyAccessName(node).unwrap())
                })
                .copied()
        }
    }

    fn forEachIdentifierInEntityName<F>(
        &mut self,
        e: &Rc<BoundNode>,
        parent: Option<SymbolId>,
        mut action: F,
    ) -> Option<SymbolId>
    where
        F: FnMut(&mut Self, &Rc<BoundNode>, Option<SymbolId>, Option<SymbolId>) -> Option<SymbolId>,
    {
        if self.isExportsOrModuleExportsOrAlias(&self.file.clone(), e.node.clone()) {
            self.store.node_data(&self.file).symbol
        } else if isIdentifier(e) {
            let symbol = self.lookupSymbolForPropertyAccess(&e.node, None);
            action(self, e, symbol, parent)
        } else {
            let expression = match &e.node {
                Node::ElementAccessExpression(n) => &n.expression,
                Node::PropertyAccessExpression(n) => &n.expression,
                _ => unreachable!(),
            };
            let s = self.forEachIdentifierInEntityName(&expression.bind(e), parent, &mut action);
            let name = getNameOrArgument(e);
            // unreachable
            if isPrivateIdentifier(&name) {
                unreachable!("unexpected PrivateIdentifier");
            }
            let symbol = s
                .and_then(|s| self.symbols[s].exports)
                .and_then(|exports| {
                    self.symbol_tables[exports]
                        .get(&getElementOrPropertyAccessName(&e.node).unwrap())
                })
                .copied();
            action(self, &name, symbol, s)
        }
    }

    fn bindCallExpression(&mut self, node: &Node) {
        // We're only inspecting call expressions to detect CommonJS modules, so we can skip
        // this check if we've already seen the module indicator
        if self.file.commonJsModuleIndicator.is_none() && isRequireCall(node, false) {
            self.setCommonJsModuleIndicator(node);
        }
    }

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

    fn getInferTypeContainer(node: Option<Rc<BoundNode>>) -> Option<Rc<BoundNode>> {
        let extendsType = findAncestor(node, |n| {
            if let Some(Node::ConditionalTypeNode(parent)) = n.parent_node() {
                if parent.extendsType.node_id() == n.node_id() {
                    return FindResult::Some(n.clone());
                }
            }
            FindResult::None
        });
        extendsType.and_then(|n| n.parent())
    }

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
            let container = Self::getInferTypeContainer(node.parent());
            if let Some(container) = container {
                let locals = match self.node_data(&container).locals {
                    Some(l) => l,
                    None => {
                        let locals = self.createSymbolTable();
                        self.node_data_mut(&container).locals = Some(locals);
                        locals
                    }
                };
                self.declareSymbol(
                    locals,
                    None,
                    node,
                    SymbolFlags::TypeParameter,
                    SymbolFlags::TypeParameterExcludes,
                    false,
                    false,
                );
            } else {
                let name = self.getDeclarationName(node).unwrap(); // TODO: GH#18217
                self.bindAnonymousDeclaration(node, SymbolFlags::TypeParameter, name);
            }
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

    fn isExportsOrModuleExportsOrAlias(
        &mut self,
        sourceFile: &Rc<SourceFile>,
        mut node: Node,
    ) -> bool {
        let sourceFile = sourceFile.clone().into();
        let mut i = 0;
        let mut q = VecDeque::from([node]);
        while !q.is_empty() && i < 100 {
            i += 1;
            node = q.pop_front().unwrap();
            if isExportsIdentifier(&node) || isModuleExportsAccessExpression(&node) {
                return true;
            } else if let Node::Identifier(node) = node {
                if let Some(symbol) = self.lookupSymbolForName(&sourceFile, &node.escapedText) {
                    if let Some(valueDeclaration) = &self.symbols[symbol].valueDeclaration {
                        if let Node::VariableDeclaration(valueDeclaration) = &valueDeclaration.node
                        {
                            if let Some(init) = &valueDeclaration.initializer {
                                q.push_back(init.clone().into());
                                if isAssignmentExpression(&init.clone().into(), true) {
                                    let init = unwrap_as!(init, Expression::BinaryExpression(n), n);
                                    q.push_back(init.left.clone().into());
                                    q.push_back(init.right.clone().into());
                                }
                            }
                        }
                    }
                }
            }
        }
        false
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

// function eachUnreachableRange(node: Node, cb: (start: Node, last: Node) => void) {
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
