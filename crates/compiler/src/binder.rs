use crate::ast;
use crate::node::*;
use crate::types::*;
use crate::utils::*;
use crate::SourceFile;
use ahash::AHashMap;
use bitflags::bitflags;
use index::vec::IndexVec;
use std::hash::Hash;
use std::mem;
use std::rc::Rc;
use swc_atoms::JsWord;

macro_rules! assert_id_is_for_flow_label {
    ($binder:expr, $id:expr) => {
        debug_assert!(matches!(
            $binder.flow_nodes[$id].kind,
            FlowNodeKind::FlowLabel(_)
        ));
    };
}

macro_rules! bind {
    ($binder:ident, $node:expr, $parent:expr) => {
        $binder.bind(Some($node.bind($parent)))
    };
}
macro_rules! bind_opt {
    ($binder:ident, $node:expr, $parent:expr) => {
        $binder.bind($node.as_ref().map(|n| n.bind($parent)))
    };
}
macro_rules! bind_vec {
    ($binder:ident, $nodes:expr, $parent:expr) => {
        for n in $nodes.iter() {
            bind!($binder, n, $parent);
        }
    };
}
macro_rules! bind_vec_opt {
    ($binder:ident, $nodes:expr, $parent:expr) => {
        for n in $nodes.iter() {
            bind_opt!($binder, n, $parent);
        }
    };
}
macro_rules! bind_opt_vec {
    ($binder:ident, $node:expr, $parent:expr) => {
        match &$node {
            Some(n) => bind_vec!($binder, n, $parent),
            None => {}
        }
    };
}

pub struct BindResult {
    pub node_data: AHashMap<BoundNode, NodeData>,
    pub flow_nodes: IndexVec<FlowNodeId, FlowNode>,
    pub symbols: IndexVec<SymbolId, Symbol>,
    pub symbol_tables: IndexVec<SymbolTableId, SymbolTable>,
}

// //todo:
// const enum InternalSymbolName {
//     Call = "__call", // Call signatures
//     Constructor = "__constructor", // Constructor implementations
//     New = "__new", // Constructor signatures
//     Index = "__index", // Index signatures
//     ExportStar = "__export", // Module export * declarations
//     Global = "__global", // Global self-reference
//     Missing = "__missing", // Indicates missing symbol
//     Type = "__type", // Anonymous type literal symbol
//     Object = "__object", // Anonymous object literal declaration
//     JSXAttributes = "__jsxAttributes", // Anonymous JSX attributes object literal declaration
//     Class = "__class", // Unnamed class expression
//     Function = "__function", // Unnamed function expression
//     Computed = "__computed", // Computed property name declaration with dynamic name
//     Resolving = "__resolving__", // Indicator symbol used to mark partially resolved type aliases
//     ExportEquals = "export=", // Export assignment symbol
//     Default = "default", // Default export symbol (technically not wholly internal, but included here for usability)
//     This = "this",
// }

macro_rules! declare_symbol {
    ($binder:ident, $symbol_table:expr, $parent:expr, $node:expr, $includes:expr, $excludes:expr, $is_replaced_by_method:expr, $is_computed_name:expr) => {{
        let name = $binder.f($node.clone(), $parent, $is_computed_name);
        let symbol_table = $symbol_table;
        let symbol = $binder.declareSymbol(
            symbol_table,
            name.clone(),
            $parent,
            $node.clone(),
            $includes,
            $excludes,
            $is_replaced_by_method,
        );
        // if Some(symbol) != exisiting_symbol {
        //     $symbol_table.insert(name.clone(), symbol);
        // }
        symbol
    }};
}

// TODO: use assert_id_is_for_flow_label everywhere tsc expects a flowLabel, but we use a flowId

// TODO: fields only pub for testing:
pub struct Binder<'a> {
    //////////////////// From tsc: ////////////////////
    pub file: Option<&'a mut SourceFile>,
    // options: CompilerOptions,
    // languageVersion: ScriptTarget,
    pub parent: Option<BoundNode>,
    pub container: Option<BoundNode>,
    // Container one level up
    pub thisParentContainer: Option<BoundNode>,
    pub blockScopeContainer: Option<BoundNode>,
    pub lastContainer: Option<BoundNode>,
    // delayedTypeAliases: (JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag)[],
    pub seenThisKeyword: bool,

    // state used by control flow analysis
    pub currentFlow: FlowNodeId,
    pub currentBreakTarget: Option<FlowNodeId>,
    pub currentContinueTarget: Option<FlowNodeId>,
    pub currentReturnTarget: Option<FlowNodeId>,
    pub currentTrueTarget: Option<FlowNodeId>,
    pub currentFalseTarget: Option<FlowNodeId>,
    pub currentExceptionTarget: Option<FlowNodeId>,
    pub preSwitchCaseFlow: Option<FlowNodeId>,
    // TODO: I think this is used to track parent labeled stmts as we decend the
    // AST. ASTs tend to be shallow, and I think labeled stmts are quite rare,
    // so this stack will never be very big. Maybe replace with a SmallVec or
    // somthing.
    pub activeLabelStack: Vec<ActiveLabel>,
    pub hasExplicitReturn: bool,

    // state used for emit helpers
    // emitFlags: NodeFlags,

    // If this file is an external module, then it is automatically in strict-mode according to
    // ES6.  If it is not an external module, then we'll determine if it is in strict mode or
    // not depending on if we see "use strict" in certain places or if we hit a class/namespace
    // or if compiler options contain alwaysStrict.
    inStrictMode: bool,

    // If we are binding an assignment pattern, we will bind certain expressions differently.
    // todo: initialization:
    // inAssignmentPattern = false;
    pub inAssignmentPattern: bool,

    // symbolCount = 0;

    // Symbol: new (flags: SymbolFlags, name: __String) => Symbol;
    // classifiableNames: Set<__String>;
    pub unreachableFlow: FlowNodeId,
    // reportedUnreachableFlow: FlowNodeId,
    ///////////////////////////////////////////////////

    //////////////////// Ours: ////////////////////
    pub node_data: AHashMap<BoundNode, NodeData>,
    pub flow_nodes: IndexVec<FlowNodeId, FlowNode>,
    pub symbols: IndexVec<SymbolId, Symbol>,
    pub symbol_tables: IndexVec<SymbolTableId, SymbolTable>,
    ///////////////////////////////////////////////////
}

impl<'a> Binder<'a> {
    fn new() -> Binder<'a> {
        let mut flow_nodes = IndexVec::with_capacity(2);
        let unreachableFlow = flow_nodes.push(FlowNode {
            flags: FlowFlags::Unreachable,
            kind: FlowNodeKind::None,
        });
        let reportedUnreachableFlow = flow_nodes.push(FlowNode {
            flags: FlowFlags::Unreachable,
            kind: FlowNodeKind::None,
        });
        Binder {
            file: None,
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

            activeLabelStack: Vec::new(),
            hasExplicitReturn: false,

            // emitFlags: NodeFlags::None,
            inStrictMode: false,

            inAssignmentPattern: false,

            unreachableFlow,
            // reportedUnreachableFlow,
            node_data: Default::default(),
            flow_nodes,
            symbols: Default::default(),
            symbol_tables: Default::default(),
        }
    }

    pub fn bind_source_files(files: &'a mut [SourceFile]) -> BindResult {
        let mut binder = Binder::new();

        for file in files {
            let node = match &file.program {
                ast::Program::Script(s) => s.bind_to_opt_parent(None),
                ast::Program::Module(m) => m.bind_to_opt_parent(None),
            };

            binder.file = Some(file);
            binder.bind(Some(node));
        }

        BindResult {
            node_data: binder.node_data,
            flow_nodes: binder.flow_nodes,
            symbols: binder.symbols,
            symbol_tables: binder.symbol_tables,
        }
    }

    // pub fn bind_source_file(file: &ast::Program) -> Self {
    //     let mut binder = Binder::new(file.clone());
    //     let node = match file {
    //         ast::Program::Script(s) => s.bind_to_opt_parent(None),
    //         ast::Program::Module(m) => m.bind_to_opt_parent(None),
    //     };

    //     binder.bind(Some(node));
    //     binder
    // }

    fn alloc_flow_node(&mut self, node: FlowNode) -> FlowNodeId {
        self.flow_nodes.push(node)
    }

    fn file(&self) -> &SourceFile {
        self.file.as_ref().unwrap()
    }

    fn program(&self) -> &ast::Program {
        &self.file().program
    }

    fn node_data(&mut self, node: BoundNode) -> &NodeData {
        self.node_data.entry(node).or_default()
    }

    fn node_data_mut(&mut self, node: BoundNode) -> &mut NodeData {
        self.node_data.entry(node).or_default()
    }

    // TODO: remove
    fn update_node_flags<F>(&mut self, node: BoundNode, f: F)
    where
        F: FnOnce(NodeFlags) -> NodeFlags,
    {
        let existing = &mut self.node_data.entry(node).or_default().flags;

        let new = f(*existing);

        *existing = new;
    }

    fn createSymbol(&mut self, flags: SymbolFlags, name: JsWord) -> SymbolId {
        // symbolCount+=1;
        let symbol = Symbol::new_base_symbol(flags, name);
        self.symbols.push(symbol)
    }

    fn addDeclarationToSymbol(
        &mut self,
        symbol_id: SymbolId,
        node: BoundNode,
        symbolFlags: SymbolFlags,
    ) {
        self.node_data_mut(node.clone()).symbol = Some(symbol_id);

        let symbol = &mut self.symbols[symbol_id];
        *symbol.flags_mut() |= symbolFlags;

        symbol.declarations_mut().push_if_unique(node.clone());

        if symbolFlags.intersects(
            SymbolFlags::Class | SymbolFlags::Enum | SymbolFlags::Module | SymbolFlags::Variable,
        ) && symbol.exports().is_none()
        {
            *symbol.exports_mut() = Some(self.symbol_tables.push(SymbolTable::default()));
        }

        if symbolFlags.intersects(
            SymbolFlags::Class
                | SymbolFlags::Interface
                | SymbolFlags::TypeLiteral
                | SymbolFlags::ObjectLiteral,
        ) && symbol.members().is_none()
        {
            *symbol.members_mut() = Some(self.symbol_tables.push(SymbolTable::default()));
        }

        // On merge of const enum module with class or function, reset const enum only flag (namespaces will already recalculate)
        if symbol.constEnumOnlyModule()
            && (symbol
                .flags()
                .intersects(SymbolFlags::Function | SymbolFlags::Class | SymbolFlags::RegularEnum))
        {
            *symbol.constEnumOnlyModule_mut() = false;
        }

        if symbolFlags.intersects(SymbolFlags::Value) {
            setValueDeclaration(symbol, node);
        }
    }

    // Should not be called on a declaration with a computed property name,
    // unless it is a well known Symbol.
    fn getDeclarationName(&self, node: BoundNode) -> JsWord {
        // TODO:
        // if (node.kind == SyntaxKind.ExportAssignment) {
        //     return (node as ExportAssignment).isExportEquals ? InternalSymbolName.ExportEquals : InternalSymbolName.Default;
        // }

        if let Some(name) = getNameOfDeclaration(&node) {
            // TODO:
            // if isAmbientModule(node) {
            //     const moduleName = getTextOfIdentifierOrLiteral(name as Identifier | StringLiteral);
            //     return (isGlobalScopeAugmentation(node as ModuleDeclaration) ? "__global" : `"${moduleName}"`) as __String;
            // }

            // if (isPrivateIdentifier(name)) {
            //     // containingClass exists because private names only allowed inside classes
            //     const containingClass = getContainingClass(node);
            //     if (!containingClass) {
            //         // we can get here in cases where there is already a parse error.
            //         return undefined;
            //     }
            //     const containingClassSymbol = containingClass.symbol;
            //     return getSymbolNameForPrivateIdentifier(containingClassSymbol, name.escapedText);
            // }

            return match &name {
                DeclName::Ident(i) => i.sym.clone(),
                DeclName::String(s) => s.value.clone(),
                DeclName::NoSubstitutionTemplate(t) => {
                    get_text_of_no_substitution_template(t.as_ref())
                }
                DeclName::Number(n) => n.raw.clone().unwrap(),
                DeclName::ComputedProperty(expr) => {
                    match &expr.expr {
                        // treat computed property names where expression is string/numeric literal as just string/numeric literal
                        // TODO: escapeLeadingUnderscores:
                        ast::Expr::Lit(ast::Lit::Num(_)) => todo!(),
                        ast::Expr::Lit(ast::Lit::Str(s)) => s.value.clone(),
                        ast::Expr::Tpl(t) if t.exprs.is_empty() => {
                            get_text_of_no_substitution_template(t.as_ref())
                        }
                        ast::Expr::Unary(e)
                            if e.op == ast::UnaryOp::Plus || e.op == ast::UnaryOp::Minus =>
                        {
                            match &e.arg {
                                ast::Expr::Lit(ast::Lit::Num(_)) => {
                                    todo!();
                                    // tokenToString(nameExpression.operator) + nameExpression.operand.text as __String
                                }
                                _ => unreachable!(
                                    "Only computed properties with literal names have declaration names"
                                )
                            }
                        }
                        _ => unreachable!(
                            "Only computed properties with literal names have declaration names"
                        ),
                    }
                }
                _ => todo!(),
            };
        }

        match node {
            BoundNode::Constructor(_) => InternalSymbolName::Constructor.into(),
            // TODO: JSDoc
            // | BoundNode::JSDocSignature(_)
            BoundNode::TsFnType(_) | BoundNode::TsCallSignatureDecl(_) => {
                InternalSymbolName::Call.into()
            }
            BoundNode::TsConstructorType(_) | BoundNode::TsConstructSignatureDecl(_) => {
                InternalSymbolName::New.into()
            }
            BoundNode::TsIndexSignature(_) => InternalSymbolName::Index.into(),
            // TODO:
            // BoundNode::ExportDeclaration(_) => InternalSymbolName::ExportStar.into(),
            BoundNode::Script(_) | BoundNode::Module(_) => {
                // json file should behave as
                // module.exports = ...
                InternalSymbolName::ExportEquals.into()
            }
            // BoundNode::BinExpr(_) => {
            //     if (getAssignmentDeclarationKind(node as BinaryExpression)
            //         == AssignmentDeclarationKind.ModuleExports)
            //     {
            //         // module.exports = ...
            //         InternalSymbolName::ExportEquals.into()
            //     } else {
            //         todo!("Unknown binary declaration kind");
            //         // Debug.fail("Unknown binary declaration kind");
            //         // break;
            //     }
            // }
            // TODO: JSDoc
            // BoundNode::JSDocFunctionType(_) => {
            //     return (isJSDocConstructSignature(node) ? InternalSymbolName::New : InternalSymbolName::Call);
            // }
            // BoundNode::Parameter(_) => {
            //     // Parameters with names are handled at the top of this function.  Parameters
            //     // without names can only come from JSDocFunctionTypes.
            //     Debug.assert(node.parent.kind === SyntaxKind.JSDocFunctionType, "Impossible parameter parent kind", () => `parent is: ${(ts as any).SyntaxKind ? (ts as any).SyntaxKind[node.parent.kind] : node.parent.kind}, expected JSDocFunctionType`);
            //     const functionType = node.parent as JSDocFunctionType;
            //     const index = functionType.parameters.indexOf(node as ParameterDeclaration);
            //     return "arg" + index as __String;
            // }
            _ => unreachable!(),
        }
    }

    // fn getDisplayName(node: &ast::Decl) -> JsWord {
    //     if isNamedDeclaration(node) {
    //         declarationNameToString(node.name)
    //     } else {
    //         unescapeLeadingUnderscores(Debug.checkDefined(self.getDeclarationName(node)))
    //     }
    // }

    // TODO: rename:
    fn f(&mut self, node: BoundNode, parent: Option<SymbolId>, isComputedName: bool) -> JsWord {
        // TODO:
        // Debug.assert(isComputedName || !hasDynamicName(node));

        // TODO:
        let isDefaultExport = false;
        // let isDefaultExport = hasSyntacticModifier(node, ModifierFlags.Default)
        //     || isExportSpecifier(node) && node.name.sym == "default";

        // The exported symbol for an export default function/class node is always named "default"
        if isComputedName {
            InternalSymbolName::Computed.into()
        } else if isDefaultExport && parent.is_some() {
            InternalSymbolName::Default.into()
        } else {
            self.getDeclarationName(node.clone())
        }
    }

    /// Declares a Symbol for the node and adds it to symbols. Reports errors for conflicting identifier names.
    /// @param symbolTable - The symbol table which node will be added to.
    /// @param parent - node's parent declaration.
    /// @param node - The declaration to be added to the symbol table
    /// @param includes - The SymbolFlags that node has in addition to its declaration type (eg: export, ambient, etc.)
    /// @param excludes - The flags which node cannot be declared alongside in a symbol table. Used to report forbidden declarations.
    fn declareSymbol(
        &mut self,
        symbol_table: SymbolTableId,
        name: JsWord,
        parent: Option<SymbolId>,
        node: BoundNode,
        includes: SymbolFlags,
        excludes: SymbolFlags,
        isReplaceableByMethod: bool,
    ) -> SymbolId {
        // TODO:
        // Debug.assert(isComputedName || !hasDynamicName(node));

        // TODO:
        // let isDefaultExport = hasSyntacticModifier(node, ModifierFlags.Default)
        //     || isExportSpecifier(node) && node.name.sym == "default";

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

        // TODO:
        //     if (includes & SymbolFlags::Classifiable) {
        //         classifiableNames.add(name);
        //     }

        let symbol = if let Some(&sym_id) = self.symbol_tables[symbol_table].get(&name) {
            let unwrapped_symbol = &self.symbols[sym_id];
            if isReplaceableByMethod && !unwrapped_symbol.isReplaceableByMethod() {
                // A symbol already exists, so don't add this as a declaration.
                return sym_id;
            } else if unwrapped_symbol.flags().intersects(excludes) {
                if unwrapped_symbol.isReplaceableByMethod() {
                    // Javascript constructor-declared symbols can be discarded in favor of
                    // prototype symbols like methods.
                    self.createSymbol(SymbolFlags::None, name.clone())
                } else if !(includes.intersects(SymbolFlags::Variable)
                    && unwrapped_symbol.flags().intersects(SymbolFlags::Assignment))
                {
                    // TODO: warnings/errors on redeclaration
                    // // Assignment declarations are allowed to merge with variables, no matter what other flags they have.
                    // if (isNamedDeclaration(node)) {
                    //     setParent(node.name, node);
                    // }
                    // // Report errors every position with duplicate declaration
                    // // Report errors on previous encountered declarations
                    // let message = if unwrapped_symbol.flags & SymbolFlags::BlockScopedVariable {
                    //     Diagnostics.Cannot_redeclare_block_scoped_variable_0
                    // } else {
                    //     Diagnostics.Duplicate_identifier_0
                    // };
                    // let messageNeedsName = true;

                    // if unwrapped_symbol.flags & SymbolFlags::Enum
                    //     || includes & SymbolFlags::Enum
                    // {
                    //     message = Diagnostics.Enum_declarations_can_only_merge_with_namespace_or_other_enum_declarations;
                    //     messageNeedsName = false;
                    // }

                    // let multipleDefaultExports = false;
                    // if length(unwrapped_symbol.declarations) {
                    //     // If the current node is a default export of some sort, then check if
                    //     // there are any other default exports that we need to error on.
                    //     // We'll know whether we have other default exports depending on if `symbol` already has a declaration list set.
                    //     if (isDefaultExport) {
                    //         message = Diagnostics.A_module_cannot_have_multiple_default_exports;
                    //         messageNeedsName = false;
                    //         multipleDefaultExports = true;
                    //     } else {
                    //         // This is to properly report an error in the case "export default { }" is after export default of class declaration or function declaration.
                    //         // Error on multiple export default in the following case:
                    //         // 1. multiple export default of class declaration or function declaration by checking NodeFlags.Default
                    //         // 2. multiple export default of export assignment. This one doesn't have NodeFlags.Default on (as export default doesn't considered as modifiers)
                    //         if (unwrapped_symbol.declarations
                    //             && unwrapped_symbol.declarations.length
                    //             && (node.kind == SyntaxKind.ExportAssignment
                    //                 && !(node as ExportAssignment).isExportEquals))
                    //         {
                    //             message =
                    //                 Diagnostics.A_module_cannot_have_multiple_default_exports;
                    //             messageNeedsName = false;
                    //             multipleDefaultExports = true;
                    //         }
                    //     }
                    // }

                    // let relatedInformation: DiagnosticRelatedInformation[] = [];
                    // if (isTypeAliasDeclaration(node) && nodeIsMissing(node.type) && hasSyntacticModifier(node, ModifierFlags.Export) && symbol.flags & (SymbolFlags::Alias | SymbolFlags::Type | SymbolFlags::Namespace)) {
                    //     // export type T; - may have meant export type { T }?
                    //     relatedInformation.push(createDiagnosticForNode(node, Diagnostics.Did_you_mean_0, `export type { ${unescapeLeadingUnderscores(node.name.escapedText)} }`));
                    // }

                    // let declarationName = self.getNameOfDeclaration(node) || node;
                    // forEach(symbol.declarations, (declaration, index) => {
                    //     let decl = getNameOfDeclaration(declaration) || declaration;
                    //     let diag = createDiagnosticForNode(decl, message, messageNeedsName ? getDisplayName(declaration) : undefined);
                    //     file.bindDiagnostics.push(
                    //         multipleDefaultExports ? addRelatedInfo(diag, createDiagnosticForNode(declarationName, index === 0 ? Diagnostics.Another_export_default_is_here : Diagnostics.and_here)) : diag
                    //     );
                    //     if (multipleDefaultExports) {
                    //         relatedInformation.push(createDiagnosticForNode(decl, Diagnostics.The_first_export_default_is_here));
                    //     }
                    // });

                    // let diag = createDiagnosticForNode(declarationName, message, messageNeedsName ? getDisplayName(node) : undefined);
                    // file.bindDiagnostics.push(addRelatedInfo(diag, ...relatedInformation));

                    self.createSymbol(SymbolFlags::None, name)
                } else {
                    sym_id
                }
            } else {
                sym_id
            }
        } else {
            let s = self.createSymbol(SymbolFlags::None, name.clone());
            self.symbol_tables[symbol_table].insert(name, s);
            if isReplaceableByMethod {
                *self.symbols[s].isReplaceableByMethod_mut() = true
            };
            s
        };

        self.addDeclarationToSymbol(symbol, node.clone(), includes);
        if self.symbols[symbol].parent().is_some() {
            debug_assert!(
                self.symbols[symbol].parent() == parent,
                "Existing symbol parent should match new one"
            );
        } else {
            *self.symbols[symbol].parent_mut() = parent;
        }

        symbol
    }

    fn declareModuleMember(
        &mut self,
        node: BoundNode,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) -> SymbolId {
        let container_node = match &self.container {
            Some(c) => c.clone(),
            None => unreachable!("symbol must be declared within a container"),
        };

        // let container_symbol_id = self.node_data(container_node.clone()).symbol.unwrap();

        // todo:
        let hasExportModifier = false;
        // let hasExportModifier =
        //     !!(getCombinedModifierFlags(node) & ModifierFlags.Export) || jsdocTreatAsExported(node);
        if symbolFlags.intersects(SymbolFlags::Alias) {
            todo!();
            // if node.kind == SyntaxKind.ExportSpecifier
            //     || (node.kind == SyntaxKind.ImportEqualsDeclaration && hasExportModifier)
            // {
            //     return self.declareSymbol(
            //         &mut container_symbol.unwrap().exports,
            //         container_symbol_id,
            //         node,
            //         symbolFlags,
            //         symbolExcludes,
            //         false,
            //         false,
            //     );
            // } else {
            //     return self.declareSymbol(
            //         &mut container.locals,
            //         None,
            //         node,
            //         symbolFlags,
            //         symbolExcludes,
            //         false,
            //         false,
            //     );
            // }
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
            // todo:
            // if isJSDocTypeAlias(node) {
            //     // We shouldn't add symbols for JSDoc nodes if not in a JS file.
            //     debug_assert!(isInJSFile(node));
            // }
            if !isAmbientModule(&node)
                && (hasExportModifier
                    || self
                        .node_data(container_node.clone())
                        .flags
                        .intersects(NodeFlags::ExportContext))
            {
                let container_symbol_id = self.node_data(container_node.clone()).symbol.unwrap();

                if let Some(locals) = self.node_data(container_node.clone()).locals {
                    // TODO:
                    // if hasSyntacticModifier(node, ModifierFlags::Default) && !self.getDeclarationName(node)
                    // {
                    //     // No local symbol for an unnamed default!
                    //     return declare_symbol!(
                    //         self,
                    //         self.symbols[container_symbol_id].exports_mut(),
                    //         Some(container_symbol_id),
                    //         node,
                    //         symbolFlags,
                    //         symbolExcludes,
                    //         false,
                    //         false
                    //     );
                    // }
                    let exportKind = if symbolFlags.intersects(SymbolFlags::Value) {
                        SymbolFlags::ExportValue
                    } else {
                        SymbolFlags::None
                    };
                    let local = declare_symbol!(
                        self,
                        locals,
                        None,
                        node,
                        exportKind,
                        symbolExcludes,
                        false,
                        false
                    );

                    *self.symbols[local].exportSymbol_mut() = Some(declare_symbol!(
                        self,
                        self.symbols[container_symbol_id].exports().unwrap(),
                        Some(container_symbol_id),
                        node,
                        symbolFlags,
                        symbolExcludes,
                        false,
                        false
                    ));
                    self.node_data_mut(node).localSymbol = Some(local);
                    return local;
                } else {
                    return declare_symbol!(
                        self,
                        self.symbols[container_symbol_id].exports().unwrap(),
                        Some(container_symbol_id),
                        node,
                        symbolFlags,
                        symbolExcludes,
                        false,
                        false
                    );
                }
            } else {
                return declare_symbol!(
                    self,
                    self.node_data(container_node.clone()).locals.unwrap(),
                    None,
                    node,
                    symbolFlags,
                    symbolExcludes,
                    false,
                    false
                );
            }
        }
    }

    // TODO:
    // jsdocTreatAsExported

    // All container nodes are kept on a linked list in declaration order. This list is used by
    // the getLocalNameOfContainer function in the type checker to validate that the local name
    // used for a container is unique.
    fn bindContainer(&mut self, node: BoundNode, container_flags: ContainerFlags) {
        // Before we recurse into a node's children, we first save the existing parent, container
        // and block-container.  Then after we pop out of processing the children, we restore
        // these saved values.
        let save_container = self.container.clone();
        let save_this_parent_container = self.thisParentContainer.clone();
        let saved_block_scope_container = self.blockScopeContainer.clone();

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
        if container_flags.intersects(ContainerFlags::IsContainer) {
            if !matches!(node, BoundNode::ArrowExpr(_)) {
                self.thisParentContainer = self.container.clone();
            }
            self.blockScopeContainer = Some(node.clone());
            if container_flags.intersects(ContainerFlags::HasLocals) {
                self.node_data_mut(node.clone()).locals =
                    Some(self.symbol_tables.push(SymbolTable::default()));
            }
            self.addToContainerChain(node.clone());
            self.container = Some(node.clone());
        } else if container_flags.intersects(ContainerFlags::IsBlockScopedContainer) {
            self.blockScopeContainer = Some(node.clone());
            self.node_data_mut(node.clone()).locals = None;
        }

        if container_flags.intersects(ContainerFlags::IsControlFlowContainer) {
            let save_current_flow = self.currentFlow;
            let save_break_target = self.currentBreakTarget;
            let save_continue_target = self.currentContinueTarget;
            let save_return_target = self.currentReturnTarget;
            let save_exception_target = self.currentExceptionTarget;
            // let saveActiveLabelList = self.activeLabelList;
            let saveHasExplicitReturn = self.hasExplicitReturn;
            let isIIFE = if let BoundNode::FnExpr(e) = &node {
                !e.function.is_async
                    && !e.function.is_generator
                    && getImmediatelyInvokedFunctionExpression(node.clone()).is_some()
            } else if let BoundNode::ArrowExpr(e) = &node {
                !e.is_async && getImmediatelyInvokedFunctionExpression(node.clone()).is_some()
            } else {
                false
            };
            // A non-async, non-generator IIFE is considered part of the containing control flow. Return statements behave
            // similarly to break statements that exit to a label just past the statement body.
            if !isIIFE {
                let mut flow_start = FlowStart { node: None };
                if container_flags.intersects(
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
            // self.currentReturnTarget = if isIIFE
            //     || node.kind == SyntaxKind.Constructor
            //     || node.kind == SyntaxKind.ClassStaticBlockDeclaration
            //     || (isInJSFile(node)
            //         && (node.kind == SyntaxKind.FunctionDeclaration
            //             || node.kind == SyntaxKind.FunctionExpression))
            // {
            //     createBranchLabel()
            // } else {
            //     None
            // };
            self.currentExceptionTarget = None;
            self.currentBreakTarget = None;
            self.currentContinueTarget = None;
            // self.activeLabelList = None;
            self.hasExplicitReturn = false;
            self.bindChildren(node.clone());
            // Reset all reachability check related flags on node (for incremental scenarios)
            self.update_node_flags(node, |existing| {
                existing & !NodeFlags::ReachabilityAndEmitFlags
            });
            // if !(self.currentFlow.flags.intersects(FlowFlags::Unreachable))
            //     && containerFlags.intersects(ContainerFlags::IsFunctionLike)
            //     && nodeIsPresent(
            //         (node as FunctionLikeDeclaration | ClassStaticBlockDeclaration).body,
            //     )
            // {
            //     self.update_node_flags(node, |existing| existing | NodeFlags::HasImplicitReturn);
            //     if self.hasExplicitReturn {
            //         self.update_node_flags(node, |existing| {
            //             existing | NodeFlags::HasExplicitReturn
            //         });
            //     }
            //     (node as FunctionLikeDeclaration | ClassStaticBlockDeclaration).endFlowNode =
            //         self.currentFlow;
            // }
            // if node.kind == SyntaxKind.SourceFile {
            //     node.flags().update(|flags| flags | emitFlags);
            //     (node as SourceFile).endFlowNode = self.currentFlow;
            // }

            if let Some(current_return_target) = self.currentReturnTarget {
                self.addAntecedent(current_return_target, self.currentFlow);
                self.currentFlow = self.finishFlowLabel(current_return_target);
                // if node.kind == SyntaxKind.Constructor
                //     || node.kind == SyntaxKind.ClassStaticBlockDeclaration
                //     || (isInJSFile(node)
                //         && (node.kind == SyntaxKind.FunctionDeclaration
                //             || node.kind == SyntaxKind.FunctionExpression))
                // {
                //     (node as FunctionLikeDeclaration | ClassStaticBlockDeclaration)
                //         .returnFlowNode = self.currentFlow;
                // }
            }
            if !isIIFE {
                self.currentFlow = save_current_flow;
            }
            self.currentBreakTarget = save_break_target;
            self.currentContinueTarget = save_continue_target;
            self.currentReturnTarget = save_return_target;
            self.currentExceptionTarget = save_exception_target;
            // self.activeLabelList = saveActiveLabelList;
            self.hasExplicitReturn = saveHasExplicitReturn;
        } else if container_flags.intersects(ContainerFlags::IsInterface) {
            self.seenThisKeyword = false;
            self.bindChildren(node.clone());
            let seen_this_keyword = self.seenThisKeyword;
            self.node_data_mut(node)
                .flags
                .set(NodeFlags::ContainsThis, seen_this_keyword);
        } else {
            self.bindChildren(node);
        }

        self.container = save_container;
        self.thisParentContainer = save_this_parent_container;
        self.blockScopeContainer = saved_block_scope_container;
    }

    fn bindEachFunctionsFirst(&mut self, nodes: impl Iterator<Item = BoundNode> + Clone) {
        // Bind fn decls first.
        for node in nodes.clone().filter(|n| matches!(n, BoundNode::FnDecl(_))) {
            self.bind(Some(node));
        }
        // Then all other nodes.
        for node in nodes.filter(|n| !matches!(n, BoundNode::FnDecl(_))) {
            self.bind(Some(node));
        }
    }

    fn bindChildren(&mut self, node: BoundNode) {
        let save_in_assignment_pattern = self.inAssignmentPattern;
        // Most nodes aren't valid in an assignment pattern, so we clear the value here
        // and set it before we descend into nodes that could actually be part of an assignment pattern.
        self.inAssignmentPattern = false;
        if self.checkUnreachable(&node) {
            todo!();
            // self.bindEachChild(node);
            // // TODO: jsdoc
            // // self.bindJSDoc(node);
            // self.inAssignmentPattern = save_in_assignment_pattern;
            // return;
        }
        // if (node.kind >= SyntaxKind.FirstStatement && node.kind <= SyntaxKind.LastStatement && !options.allowUnreachableCode) {
        //     self.flow_node_map.insert(node, self.currentFlow);
        // }
        match node {
            BoundNode::WhileStmt(s) => {
                self.bindWhileStatement(s);
            }
            BoundNode::DoWhileStmt(s) => {
                self.bindDoStatement(s);
            }
            BoundNode::ForStmt(s) => {
                self.bindForStatement(s);
            }
            BoundNode::ForInStmt(ref s) => {
                self.bindForInOrForOfStatement(
                    node.clone(),
                    s.left.clone(),
                    s.right.clone(),
                    s.body.clone(),
                );
            }
            BoundNode::ForOfStmt(ref s) => {
                self.bindForInOrForOfStatement(
                    node.clone(),
                    s.left.clone(),
                    s.right.clone(),
                    s.body.clone(),
                );
            }
            BoundNode::IfStmt(s) => {
                self.bindIfStatement(s);
            }
            BoundNode::ReturnStmt(s) => {
                self.bindReturnStatement(s);
            }
            BoundNode::ThrowStmt(s) => {
                self.bindThrowStatement(s);
            }
            BoundNode::BreakStmt(ref s) => {
                self.bindBreakOrContinueStatement(node.clone(), s.label.clone());
            }
            BoundNode::ContinueStmt(ref s) => {
                self.bindBreakOrContinueStatement(node.clone(), s.label.clone());
            }
            BoundNode::TryStmt(s) => {
                self.bindTryStatement(s);
            }
            BoundNode::SwitchStmt(s) => {
                self.bindSwitchStatement(s);
            }
            BoundNode::SwitchCase(case) => {
                self.bindCaseClause(case);
            }
            BoundNode::ExprStmt(s) => {
                self.bindExpressionStatement(s);
            }
            BoundNode::LabeledStmt(s) => {
                self.bindLabeledStatement(s);
            }
            BoundNode::UnaryExpr(e) => {
                self.bindUnaryExpressionFlow(e);
            }
            BoundNode::UpdateExpr(e) => {
                self.bindUpdateExpressionFlow(e);
            }
            // BoundNode::BinaryExpression(_) => {
            //     if isDestructuringAssignment(node) {
            //         // Carry over whether we are in an assignment pattern to
            //         // binary expressions that could actually be an initializer
            //         self.inAssignmentPattern = save_in_assignment_pattern;
            //         self.bindDestructuringAssignmentFlow(node);
            //         return;
            //     }
            //     self.bindBinaryExpressionFlow(node as BinaryExpression);
            // }
            BoundNode::CondExpr(e) => {
                self.bindConditionalExpressionFlow(e);
            }
            BoundNode::VarDeclarator(ref d) => {
                self.bindVariableDeclarationFlow(node.clone(), d.clone());
            }
            BoundNode::MemberExpr(ref expr) => {
                self.bindAccessExpressionFlow(node.clone(), expr.clone());
            }
            BoundNode::CallExpr(ref n) => {
                self.bindCallExpressionFlow(n);
            }
            // BoundNode::NonNullExpression(_) => {
            //     self.bindNonNullExpressionFlow(node as NonNullExpression);
            // }
            // BoundNode::JSDocTypedefTag(_)
            // | BoundNode::JSDocCallbackTag(_)
            // | BoundNode::JSDocEnumTag(_) => {
            //     self.bindJSDocTypeAlias(node as JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag);
            // }
            // In source files and blocks, bind functions first to match hoisting that occurs at runtime
            BoundNode::Module(ref m) => {
                self.bindEachFunctionsFirst(m.body.iter().map(|i| i.bind(node.clone())));
            }
            BoundNode::Script(ref s) => {
                self.bindEachFunctionsFirst(s.body.iter().map(|s| s.bind(node.clone())));
            }
            BoundNode::BlockStmt(ref block) => {
                self.bindEachFunctionsFirst(block.stmts.iter().map(|s| s.bind(node.clone())));
            }
            BoundNode::TsModuleBlock(ref module) => {
                self.bindEachFunctionsFirst(module.body.iter().map(|i| i.bind(node.clone())));
            }
            BoundNode::AssignPat(ref p) => {
                if matches!(p.left, ast::Pat::Array(_) | ast::Pat::Object(_)) {
                    // When evaluating a binding pattern, the initializer is evaluated before the binding pattern, per:
                    // - https://tc39.es/ecma262/#sec-destructuring-binding-patterns-runtime-semantics-iteratorbindinginitialization
                    //   - `BindingElement: BindingPattern Initializer?`
                    // - https://tc39.es/ecma262/#sec-runtime-semantics-keyedbindinginitialization
                    //   - `BindingElement: BindingPattern Initializer?`
                    bind!(self, p.right, node.clone());
                    bind!(self, p.left, node.clone());
                } else {
                    bind!(self, p.left, node.clone());
                    bind!(self, p.right, node.clone());
                }

                bind_opt!(self, p.type_ann, node.clone());
            }
            // Carry over whether we are in an assignment pattern of Object and Array literals
            // as well as their children that are valid assignment targets.
            BoundNode::ObjectLit(ref o) => {
                self.inAssignmentPattern = save_in_assignment_pattern;
                bind_vec!(self, o.props, node.clone());
            }
            BoundNode::ArrayLit(ref a) => {
                self.inAssignmentPattern = save_in_assignment_pattern;
                bind_vec_opt!(self, a.elems, node.clone());
            }
            BoundNode::KeyValueProp(ref p) => {
                self.inAssignmentPattern = save_in_assignment_pattern;
                bind!(self, p.key, node.clone());
                bind!(self, p.value, node.clone());
            }
            BoundNode::SpreadElement(ref s) => {
                self.inAssignmentPattern = save_in_assignment_pattern;
                bind!(self, s.expr, node.clone());
            }
            _ => {
                self.bindEachChild(node);
            }
        }
        // TODO: jsdoc
        // bindJSDoc(node);
        self.inAssignmentPattern = save_in_assignment_pattern;
    }

    fn bindEachChild(&mut self, node: BoundNode) {
        // TODO some of these will be handled by bindChildren, but are currently
        // commented-out. Remove them from here when they are added to bindChildren
        // TODO: use destructuring for these so future developers are forced to
        // update/check this method if thet modify the node's definitions.
        // TODO: make sure the order we bind a node's children matches
        // execution/evaluation order (same as tsc).
        match &node {
            BoundNode::ClassDecl(c) => {
                // let ast::Class {
                //     decorators,
                //     type_params,
                //     super_class,
                //     super_type_params,
                //     implements,
                //     body,
                //     is_abstract: _,
                //     span: _,
                //     cached_hash: _,
                // } = c.class.as_ref();

                // bind_vec!(self, decorators, node.clone());
                // bind!(self, c.ident, node.clone());
                // bind_opt!(self, type_params, node.clone());
                // bind_opt!(self, super_class, node.clone());
                // bind_opt!(self, super_type_params, node.clone());
                // bind_vec!(self, implements, node.clone());
                // bind_vec!(self, body, node.clone());
                bind!(self, c.ident, node.clone());
                bind!(self, c.class, node.clone());
            }
            BoundNode::ClassExpr(c) => {
                // let ast::Class {
                //     decorators,
                //     type_params,
                //     super_class,
                //     super_type_params,
                //     implements,
                //     body,
                //     is_abstract: _,
                //     span: _,
                //     cached_hash: _,
                // } = c.class.as_ref();

                // bind_vec!(self, decorators, node.clone());
                // bind_opt!(self, c.ident, node.clone());
                // bind_opt!(self, type_params, node.clone());
                // bind_opt!(self, super_class, node.clone());
                // bind_opt!(self, super_type_params, node.clone());
                // bind_vec!(self, implements, node.clone());
                // bind_vec!(self, body, node.clone());
                bind_opt!(self, c.ident, node.clone());
                bind!(self, c.class, node.clone());
            }
            BoundNode::Class(c) => {
                let ast::Class {
                    node_id: _,
                    decorators,
                    type_params,
                    extends,
                    implements,
                    body,
                    is_abstract: _,
                    span: _,
                    cached_hash: _,
                } = c.node.as_ref();

                bind_vec!(self, decorators, node.clone());
                bind_opt_vec!(self, type_params, node.clone());
                bind_opt!(self, extends, node.clone());
                bind_vec!(self, implements, node.clone());
                bind_vec!(self, body, node.clone());
            }
            BoundNode::ClassProp(p) => {
                let ast::ClassProp {
                    node_id: _,
                    span: _,
                    key,
                    value,
                    type_ann,
                    is_static: _,
                    decorators,
                    accessibility: _,
                    is_abstract: _,
                    is_optional: _,
                    is_override: _,
                    readonly: _,
                    declare: _,
                    definite: _,
                    cached_hash: _,
                } = p.node.as_ref();

                bind_vec!(self, decorators, node.clone());
                bind!(self, key, node.clone());
                bind_opt!(self, type_ann, node.clone());
                bind_opt!(self, value, node.clone());
            }
            BoundNode::PrivateProp(p) => {
                let ast::PrivateProp {
                    node_id: _,
                    span: _,
                    key,
                    value,
                    type_ann,
                    is_static: _,
                    decorators,
                    accessibility: _,
                    is_abstract: _,
                    is_optional: _,
                    is_override: _,
                    readonly: _,
                    definite: _,
                    cached_hash: _,
                } = p.node.as_ref();

                bind_vec!(self, decorators, node.clone());
                bind!(self, key, node.clone());
                bind_opt!(self, type_ann, node.clone());
                bind_opt!(self, value, node.clone());
            }
            BoundNode::ClassMethod(m) => {
                // let ast::ClassMethod {
                //     span: _,
                //     key,
                //     function,
                //     kind: _,
                //     is_static: _,
                //     accessibility: _,
                //     is_abstract: _,
                //     is_optional: _,
                //     is_override: _,
                //     cached_hash: _,
                // } = m.node.as_ref();

                // let ast::Function {
                //     params,
                //     decorators,
                //     span: _,
                //     body,
                //     is_generator: _,
                //     is_async: _,
                //     type_params,
                //     return_type,
                //     cached_hash: _,
                // } = function.as_ref();

                // bind_vec!(self, decorators, node.clone());
                // bind!(self, key, node.clone());
                // bind_opt!(self, type_params, node.clone());
                // bind_vec!(self, params, node.clone());
                // bind_opt!(self, return_type, node.clone());
                // bind_opt!(self, body, node.clone());
                bind!(self, m.node.function, node.clone());
            }
            BoundNode::PrivateMethod(m) => {
                // let ast::PrivateMethod {
                //     span: _,
                //     key,
                //     function,
                //     kind: _,
                //     is_static: _,
                //     accessibility: _,
                //     is_abstract: _,
                //     is_optional: _,
                //     is_override: _,
                //     cached_hash: _,
                // } = m.node.as_ref();

                // let ast::Function {
                //     params,
                //     decorators,
                //     span: _,
                //     body,
                //     is_generator: _,
                //     is_async: _,
                //     type_params,
                //     return_type,
                //     cached_hash: _,
                // } = function.as_ref();

                // bind_vec!(self, decorators, node.clone());
                // bind!(self, key, node.clone());
                // bind_opt!(self, type_params, node.clone());
                // bind_vec!(self, params, node.clone());
                // bind_opt!(self, return_type, node.clone());
                // bind_opt!(self, body, node.clone());
                bind!(self, m.node.function, node.clone());
            }
            BoundNode::Constructor(c) => {
                let ast::Constructor {
                    node_id: _,
                    span: _,
                    params,
                    body,
                    accessibility: _,
                    is_optional: _,
                    cached_hash: _,
                } = c.node.as_ref();

                bind_vec!(self, params, node.clone());
                bind_opt!(self, body, node.clone());
            }
            BoundNode::Decorator(d) => bind!(self, d.expr, node.clone()),
            BoundNode::FnDecl(f) => {
                let ast::FnDecl {
                    node_id: _,
                    ident,
                    function,
                    declare: _,
                    cached_hash: _,
                } = f.node.as_ref();

                // let ast::Function {
                //     params,
                //     decorators,
                //     span: _,
                //     body,
                //     is_generator: _,
                //     is_async: _,
                //     type_params,
                //     return_type,
                //     cached_hash: _,
                // } = function.as_ref();

                // bind_vec!(self, decorators, node.clone());
                // bind!(self, ident, node.clone());
                // bind_opt!(self, type_params, node.clone());
                // bind_vec!(self, params, node.clone());
                // bind_opt!(self, return_type, node.clone());
                // bind_opt!(self, body, node.clone());
                bind!(self, ident, node.clone());
                bind!(self, function, node.clone());
            }
            BoundNode::VarDecl(v) => bind_vec!(self, v.decls, node.clone()),
            // BoundNode::VarDeclarator(v) => {
            //     bind!(self, v.name, node.clone());
            //     bind_opt!(self, v.init, node.clone());
            // }
            BoundNode::ThisExpr(_) => {}
            // BoundNode::ArrayLit(a) => bind_vec_opt!(self, a.elems, node.clone()),
            // BoundNode::ObjectLit(o) => bind_vec!(self, o.props, node.clone()),
            // BoundNode::SpreadElement(s) => bind!(self, s.expr, node.clone()),
            BoundNode::BinExpr(e) => {
                bind!(self, e.left, node.clone());
                bind!(self, e.right, node.clone());
            }
            BoundNode::FnExpr(f) => {
                let ast::FnExpr {
                    node_id: _,
                    ident,
                    function,
                    cached_hash: _,
                } = f.node.as_ref();

                // let ast::Function {
                //     params,
                //     decorators,
                //     span: _,
                //     body,
                //     is_generator: _,
                //     is_async: _,
                //     type_params,
                //     return_type,
                //     cached_hash: _,
                // } = function.as_ref();

                // bind_vec!(self, decorators, node.clone());
                // bind_opt!(self, ident, node.clone());
                // bind_opt!(self, type_params, node.clone());
                // bind_vec!(self, params, node.clone());
                // bind_opt!(self, return_type, node.clone());
                // bind_opt!(self, body, node.clone());
                bind_opt!(self, ident, node.clone());
                bind!(self, function, node.clone());
            }
            BoundNode::AssignExpr(e) => {
                bind!(self, e.left, node.clone());
                bind!(self, e.right, node.clone());
            }
            // BoundNode::MemberExpr(e) => {
            // bind!(self, e.obj, node.clone());
            // bind!(self, e.prop, node.clone());
            // }
            BoundNode::CallExpr(e) => {
                bind!(self, e.callee, node.clone());
                bind_opt!(self, e.type_args, node.clone());
                bind_vec!(self, e.args, node.clone());
            }
            BoundNode::NewExpr(e) => {
                bind!(self, e.callee, node.clone());
                bind_opt!(self, e.type_args, node.clone());
                bind_opt_vec!(self, e.args, node.clone());
            }
            BoundNode::SeqExpr(e) => bind_vec!(self, e.exprs, node.clone()),
            BoundNode::ArrowExpr(e) => {
                bind_opt_vec!(self, e.type_params, node.clone());
                bind_vec!(self, e.params, node.clone());
                bind_opt!(self, e.return_type, node.clone());
                bind!(self, e.body, node.clone());
            }
            BoundNode::YieldExpr(e) => bind_opt!(self, e.arg, node.clone()),
            BoundNode::MetaPropExpr(e) => bind!(self, e.prop, node.clone()),
            BoundNode::AwaitExpr(e) => bind!(self, e.arg, node.clone()),
            BoundNode::Tpl(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TaggedTpl(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TplElement(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::ParenExpr(e) => bind!(self, e.expr, node.clone()),
            BoundNode::Super(_) => {}
            BoundNode::OptChainExpr(e) => bind!(self, e.expr, node.clone()),
            BoundNode::Function(n) => {
                let ast::Function {
                    node_id: _,
                    params,
                    decorators,
                    span: _,
                    body,
                    is_generator: _,
                    is_async: _,
                    type_params,
                    return_type,
                    cached_hash: _,
                } = n.node.as_ref();

                bind_vec!(self, decorators, node.clone());
                bind_opt_vec!(self, type_params, node.clone());
                bind_vec!(self, params, node.clone());
                bind_opt!(self, return_type, node.clone());
                bind_opt!(self, body, node.clone());
            }
            BoundNode::Param(p) => {
                bind_vec!(self, p.decorators, node.clone());
                bind!(self, p.pat, node.clone());
            }
            BoundNode::ParamWithoutDecorators(p) => {
                bind!(self, p.pat, node.clone());
            }
            BoundNode::TsAmbientParam(p) => {
                bind!(self, p.pat, node.clone());
            }
            BoundNode::BindingIdent(b) => {
                bind!(self, b.id, node.clone());
                bind_opt!(self, b.type_ann, node.clone());
            }
            BoundNode::Ident(_) => {}
            BoundNode::PrivateName(p) => bind!(self, p.id, node.clone()),
            BoundNode::JSXMemberExpr(_) => todo!("temp: node: {:?}", &node),
            BoundNode::JSXNamespacedName(_) => todo!("temp: node: {:?}", &node),
            BoundNode::JSXEmptyExpr(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::JSXExprContainer(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::JSXSpreadChild(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::JSXOpeningElement(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::JSXClosingElement(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::JSXAttr(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::JSXText(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::JSXElement(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::JSXFragment(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::JSXOpeningFragment(n) => {
                todo!("temp: node: {:?}, span: {:?}", &node, n.span)
            }
            BoundNode::JSXClosingFragment(n) => {
                todo!("temp: node: {:?}, span: {:?}", &node, n.span)
            }
            BoundNode::Invalid(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::Str(_) => {}
            BoundNode::Bool(_) => {}
            BoundNode::Null(_) => {}
            BoundNode::Number(_) => {}
            BoundNode::BigInt(_) => {}
            BoundNode::Regex(_) => {}
            BoundNode::ExportDefaultExpr(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::ExportDecl(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::ImportDecl(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::ExportAll(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::NamedExport(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::ExportDefaultDecl(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::ImportDefaultSpecifier(n) => {
                todo!("temp: node: {:?}, span: {:?}", &node, n.span)
            }
            BoundNode::ImportStarAsSpecifier(n) => {
                todo!("temp: node: {:?}, span: {:?}", &node, n.span)
            }
            BoundNode::ImportNamedSpecifier(n) => {
                todo!("temp: node: {:?}, span: {:?}", &node, n.span)
            }
            BoundNode::ExportNamespaceSpecifier(n) => {
                todo!("temp: node: {:?}, span: {:?}", &node, n.span)
            }
            BoundNode::ExportDefaultSpecifier(_) => todo!("temp: node: {:?}", &node),
            BoundNode::ExportNamedSpecifier(n) => {
                todo!("temp: node: {:?}, span: {:?}", &node, n.span)
            }
            BoundNode::ArrayPat(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::ObjectPat(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            // BoundNode::AssignPat(n) => {// handled above},
            BoundNode::RestPat(p) => {
                bind!(self, p.arg, node.clone());
                bind_opt!(self, p.type_ann, node.clone());
            }
            BoundNode::KeyValuePatProp(_) => todo!("temp: node: {:?}", &node),
            BoundNode::AssignPatProp(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            // BoundNode::KeyValueProp(_) => todo!(),
            BoundNode::AssignProp(_) => todo!("temp: node: {:?}", &node),
            BoundNode::GetterProp(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::SetterProp(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::MethodProp(_) => todo!("temp: node: {:?}", &node),
            BoundNode::ComputedPropName(n) => {
                bind!(self, n.expr, node.clone());
            }
            BoundNode::EmptyStmt(n) => {}
            BoundNode::DebuggerStmt(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::WithStmt(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::CatchClause(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsTypeAnn(a) => bind!(self, a.type_ann, node.clone()),
            BoundNode::TsTypeParamDecl(p) => {
                bind!(self, p.name, node.clone());
                bind_opt!(self, p.constraint, node.clone());
                bind_opt!(self, p.default, node.clone());
            }
            BoundNode::TsTypeParamInstantiation(i) => bind_vec!(self, i.params, node.clone()),
            BoundNode::TsParamProp(n) => {
                bind_vec!(self, n.decorators, node.clone());
                bind!(self, n.param, node.clone());
            }
            BoundNode::TsQualifiedName(n) => {
                bind!(self, n.left, node.clone());
                bind!(self, n.right, node.clone());
            }
            BoundNode::TsCallSignatureDecl(s) => {
                bind_opt_vec!(self, s.type_params, node.clone());
                bind_vec!(self, s.params, node.clone());
                bind_opt!(self, s.type_ann, node.clone());
            }
            BoundNode::TsConstructSignatureDecl(s) => {
                bind_opt_vec!(self, s.type_params, node.clone());
                bind_vec!(self, s.params, node.clone());
                bind_opt!(self, s.type_ann, node.clone());
            }
            BoundNode::TsPropertySignature(s) => {
                bind!(self, s.key, node.clone());
                bind_opt!(self, s.type_ann, node.clone());
            }
            BoundNode::TsGetterSignature(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsSetterSignature(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsMethodSignature(s) => {
                bind!(self, s.key, node.clone());
                bind_opt_vec!(self, s.type_params, node.clone());
                bind_vec!(self, s.params, node.clone());
                bind_opt!(self, s.type_ann, node.clone());
            }
            BoundNode::TsIndexSignature(s) => {
                bind_vec!(self, s.params, node.clone());
                bind_opt!(self, s.type_ann, node.clone());
            }
            BoundNode::TsKeywordType(_) => {}
            BoundNode::TsThisType(_) => {}
            BoundNode::TsFnType(f) => {
                bind_opt_vec!(self, f.type_params, node.clone());
                bind_vec!(self, f.params, node.clone());
                bind!(self, f.type_ann, node.clone());
            }
            BoundNode::TsConstructorType(t) => {
                bind_opt_vec!(self, t.type_params, node.clone());
                bind_vec!(self, t.params, node.clone());
                bind!(self, t.type_ann, node.clone());
            }
            BoundNode::TsTypeRef(r) => {
                bind!(self, r.type_name, node.clone());
                bind_opt!(self, r.type_params, node.clone());
            }
            BoundNode::TsTypePredicate(p) => {
                bind!(self, p.param_name, node.clone());
                bind_opt!(self, p.type_ann, node.clone());
            }
            BoundNode::TsTypeQuery(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsImportType(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsTypeLit(l) => bind_vec!(self, l.members, node.clone()),
            BoundNode::TsArrayType(t) => bind!(self, t.elem_type, node.clone()),
            BoundNode::TsTupleType(t) => bind_vec!(self, t.elem_types, node.clone()),
            BoundNode::TsTupleElement(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsOptionalType(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsRestType(t) => bind!(self, t.type_ann, node.clone()),
            BoundNode::TsUnionType(u) => bind_vec!(self, u.types, node.clone()),
            BoundNode::TsIntersectionType(t) => bind_vec!(self, t.types, node.clone()),
            BoundNode::TsConditionalType(t) => {
                bind!(self, t.check_type, node.clone());
                bind!(self, t.extends_type, node.clone());
                bind!(self, t.true_type, node.clone());
                bind!(self, t.false_type, node.clone());
            }
            BoundNode::TsInferType(t) => bind!(self, t.type_param, node.clone()),
            BoundNode::TsParenthesizedType(t) => bind!(self, t.type_ann, node.clone()),
            BoundNode::TsTypeOperator(o) => bind!(self, o.type_ann, node.clone()),
            BoundNode::TsIndexedAccessType(t) => {
                bind!(self, t.obj_type, node.clone());
                bind!(self, t.index_type, node.clone());
            }
            BoundNode::TsMappedType(t) => {
                bind!(self, t.type_param, node.clone());
                bind_opt!(self, t.name_type, node.clone());
                bind_opt!(self, t.type_ann, node.clone());
            }
            BoundNode::TsLitType(l) => bind!(self, l.lit, node.clone()),
            BoundNode::TsTplLitType(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsInterfaceDecl(i) => {
                bind!(self, i.id, node.clone());
                bind_opt_vec!(self, i.type_params, node.clone());
                bind_vec!(self, i.extends, node.clone());
                bind!(self, i.body, node.clone());
            }
            BoundNode::TsInterfaceBody(i) => bind_vec!(self, i.body, node.clone()),
            BoundNode::TsExprWithTypeArgs(e) => {
                bind!(self, e.expr, node.clone());
                bind_opt!(self, e.type_args, node.clone());
            }
            BoundNode::TsTypeAliasDecl(a) => {
                bind!(self, a.id, node.clone());
                bind_opt_vec!(self, a.type_params, node.clone());
                bind!(self, a.type_ann, node.clone());
            }
            BoundNode::TsEnumDecl(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsEnumMember(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsModuleDecl(d) => {
                bind!(self, d.id, node.clone());
                bind_opt!(self, d.body, node.clone());
            }
            BoundNode::TsModuleBlock(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsNamespaceDecl(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsImportEqualsDecl(n) => {
                todo!("temp: node: {:?}, span: {:?}", &node, n.span)
            }
            BoundNode::TsExternalModuleRef(n) => {
                todo!("temp: node: {:?}, span: {:?}", &node, n.span)
            }
            BoundNode::TsExportAssignment(n) => {
                todo!("temp: node: {:?}, span: {:?}", &node, n.span)
            }
            BoundNode::TsNamespaceExportDecl(n) => {
                todo!("temp: node: {:?}, span: {:?}", &node, n.span)
            }
            BoundNode::TsAsExpr(n) => {
                bind!(self, n.expr, node.clone());
                bind!(self, n.type_ann, node.clone());
            }
            BoundNode::TsTypeAssertion(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsNonNullExpr(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::TsConstAssertion(n) => todo!("temp: node: {:?}, span: {:?}", &node, n.span),
            BoundNode::ExtendsClause(n) => {
                bind!(self, n.super_class, node.clone());
                bind_opt!(self, n.super_type_params, node.clone());
            }
            _ => {
                dbg!(node);
                unreachable!("handled by bindChildren")
            }
        }
    }

    fn setFlowNodeReferenced(&mut self, flow: FlowNodeId) {
        let flow_node = &mut self.flow_nodes[flow];
        // On first reference we set the Referenced flag, thereafter we set the Shared flag
        if flow_node.flags.intersects(FlowFlags::Referenced) {
            flow_node.flags.set(FlowFlags::Shared, true);
        } else {
            flow_node.flags.set(FlowFlags::Referenced, true);
        }
    }

    fn addAntecedent(&mut self, label: FlowNodeId, antecedent_id: FlowNodeId) {
        let (label, antecedent) = self.flow_nodes.pick2_mut(label, antecedent_id);
        let label = label.kind.unwrap_flow_label_mut();
        if !(antecedent.flags.intersects(FlowFlags::Unreachable))
            && !label.antecedents.contains(&antecedent_id)
        {
            Rc::make_mut(&mut label.antecedents).push(antecedent_id);
            self.setFlowNodeReferenced(antecedent_id);
        }
    }

    fn createFlowCondition(
        &mut self,
        flags: FlowFlags,
        antecedent: FlowNodeId,
        expression: Option<ast::Expr>,
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
        // if (matches!(expression, ast::Expr::Lit(ast::Lit::Bool(b)) if b.value == true)
        //     && flags.intersects(FlowFlags::FalseCondition)
        //     || matches!(expression, ast::Expr::Lit(ast::Lit::Bool(b)) if b.value == false)
        //         && flags.intersects(FlowFlags::TrueCondition))
        //     && !isExpressionOfOptionalChainRoot(expression)
        //     && !isNullishCoalesce(expression.parent)
        // {
        //     return self.unreachableFlow;
        // }
        if matches!(&expression, ast::Expr::Lit(ast::Lit::Bool(b)) if b.value == true)
            && flags.intersects(FlowFlags::FalseCondition)
            || matches!(&expression, ast::Expr::Lit(ast::Lit::Bool(b)) if b.value == false)
                && flags.intersects(FlowFlags::TrueCondition)
        {
            todo!("see above");
        }
        if !isNarrowingExpression(&expression) {
            return antecedent;
        }
        self.setFlowNodeReferenced(antecedent);
        let node = FlowNode {
            flags,
            kind: FlowNodeKind::FlowCondition(FlowCondition {
                antecedent,
                node: expression,
            }),
        };
        self.alloc_flow_node(node)
    }

    fn createFlowSwitchClause(
        &mut self,
        antecedent: FlowNodeId,
        switchStatement: Rc<SwitchStmt>,
        clauseStart: u32,
        clauseEnd: u32,
    ) -> FlowNodeId {
        self.setFlowNodeReferenced(antecedent);
        let node = FlowNode {
            flags: FlowFlags::SwitchClause,
            kind: FlowNodeKind::FlowSwitchClause(FlowSwitchClause {
                antecedent,
                switchStatement,
                clauseStart,
                clauseEnd,
            }),
        };
        self.alloc_flow_node(node)
    }

    fn createFlowAssignment(&mut self, antecedent: FlowNodeId, node: BoundNode) -> FlowNodeId {
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

    // fn createFlowArrayMutation(&mut self, antecedent: FlowNodeId, node: BoundNode) -> FlowNodeId {
    //     self.setFlowNodeReferenced(antecedent);
    //     let node = FlowNode {
    //         flags: FlowFlags::ArrayMutation,
    //         id: None,
    //         kind: FlowNodeKind::FlowArrayMutation(FlowArrayMutation { antecedent, node }),
    //     };
    //     let id = self.alloc_flow_node(node);
    //     if let Some(currentExceptionTarget) = self.currentExceptionTarget {
    //         self.addAntecedent(currentExceptionTarget, id);
    //     }
    //     id
    // }

    fn createFlowCall(&mut self, antecedent: FlowNodeId, node: Rc<CallExpr>) -> FlowNodeId {
        self.setFlowNodeReferenced(antecedent);
        let node = FlowNode {
            flags: FlowFlags::Call,
            kind: FlowNodeKind::FlowCall(FlowCall { antecedent, node }),
        };
        self.alloc_flow_node(node)
    }

    fn finishFlowLabel(&mut self, flow: FlowNodeId) -> FlowNodeId {
        let label = self.flow_nodes[flow].kind.unwrap_flow_label();
        let antecedents = &label.antecedents;
        if antecedents.is_empty() {
            return self.unreachableFlow;
        }
        if antecedents.len() == 1 {
            return antecedents[0];
        }
        return flow;
    }

    // fn doWithConditionalBranches<T>(action: (value: T) => void, value: T, trueTarget: FlowLabel, falseTarget: FlowLabel) {
    //     const savedTrueTarget = currentTrueTarget;
    //     const savedFalseTarget = currentFalseTarget;
    //     currentTrueTarget = trueTarget;
    //     currentFalseTarget = falseTarget;
    //     action(value);
    //     currentTrueTarget = savedTrueTarget;
    //     currentFalseTarget = savedFalseTarget;
    // }

    fn bindCondition(
        &mut self,
        expr: ast::Expr,
        parent: Option<BoundNode>,
        trueTarget: FlowNodeId,
        falseTarget: FlowNodeId,
    ) {
        assert_id_is_for_flow_label!(self, trueTarget);
        assert_id_is_for_flow_label!(self, falseTarget);

        let node = expr.bind_to_opt_parent(parent);

        // self.doWithConditionalBranches(bind, node, trueTarget, falseTarget);
        if !isLogicalAssignmentExpression(node)
        // && !isLogicalExpression(node)
        // && !(isOptionalChain(node) && isOutermostOptionalChain(node))
        {
            let true_id = self.createFlowCondition(
                FlowFlags::TrueCondition,
                self.currentFlow,
                Some(expr.clone()),
            );
            self.addAntecedent(trueTarget, true_id);
            let false_id =
                self.createFlowCondition(FlowFlags::FalseCondition, self.currentFlow, Some(expr));
            self.addAntecedent(falseTarget, false_id);
        }
    }

    fn bindIterativeStatement(
        &mut self,
        node: BoundNode,
        breakTarget: FlowNodeId,
        continueTarget: FlowNodeId,
    ) {
        assert_id_is_for_flow_label!(self, breakTarget);
        assert_id_is_for_flow_label!(self, continueTarget);

        // TODO: maybe assert that node is a stmt

        let saveBreakTarget = self.currentBreakTarget;
        let saveContinueTarget = self.currentContinueTarget;
        self.currentBreakTarget = Some(breakTarget);
        self.currentContinueTarget = Some(continueTarget);
        self.bind(Some(node));
        self.currentBreakTarget = saveBreakTarget;
        self.currentContinueTarget = saveContinueTarget;
    }

    fn setContinueTarget(&mut self, node: BoundNode, target: FlowNodeId) -> FlowNodeId {
        assert_id_is_for_flow_label!(self, target);

        let mut parent = node.parent();
        for label in self.activeLabelStack.iter_mut().rev() {
            if !matches!(parent, Some(BoundNode::LabeledStmt(_))) {
                break;
            }
            label.continueTarget = Some(target);
            parent = match parent {
                Some(parent) => parent.parent(),
                None => None,
            };
        }
        target
    }

    fn bindWhileStatement(&mut self, node: Rc<WhileStmt>) {
        let continue_target = self.alloc_flow_node(FlowNode::new_loop_label());
        let preWhileLabel = self.setContinueTarget(node.clone().into(), continue_target);
        let preBodyLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        let postWhileLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        self.addAntecedent(preWhileLabel, self.currentFlow);
        self.currentFlow = preWhileLabel;
        self.bindCondition(
            node.test.clone(),
            Some(node.clone().into()),
            preBodyLabel,
            postWhileLabel,
        );
        self.currentFlow = self.finishFlowLabel(preBodyLabel);
        self.bindIterativeStatement(
            node.body.bind(node.clone().into()),
            postWhileLabel,
            preWhileLabel,
        );
        self.addAntecedent(preWhileLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(postWhileLabel);
    }

    fn bindDoStatement(&mut self, node: Rc<DoWhileStmt>) {
        let preDoLabel = self.alloc_flow_node(FlowNode::new_loop_label());
        let continue_target = self.alloc_flow_node(FlowNode::new_branch_label());
        let preConditionLabel = self.setContinueTarget(node.clone().into(), continue_target);
        let postDoLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        self.addAntecedent(preDoLabel, self.currentFlow);
        self.currentFlow = preDoLabel;
        self.bindIterativeStatement(
            node.body.bind(node.clone().into()),
            postDoLabel,
            preConditionLabel,
        );
        self.addAntecedent(preConditionLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(preConditionLabel);
        self.bindCondition(
            node.test.clone(),
            Some(node.clone().into()),
            preDoLabel,
            postDoLabel,
        );
        self.currentFlow = self.finishFlowLabel(postDoLabel);
    }

    fn bindForStatement(&mut self, node: Rc<ForStmt>) {
        let continue_target = self.alloc_flow_node(FlowNode::new_loop_label());
        let preLoopLabel = self.setContinueTarget(node.clone().into(), continue_target);
        let preBodyLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        let postLoopLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        bind_opt!(self, node.init, node.clone().into());
        self.addAntecedent(preLoopLabel, self.currentFlow);
        self.currentFlow = preLoopLabel;
        if let Some(test) = &node.test {
            self.bindCondition(
                test.clone(),
                Some(node.clone().into()),
                preBodyLabel,
                postLoopLabel,
            );
        }
        self.currentFlow = self.finishFlowLabel(preBodyLabel);
        self.bindIterativeStatement(
            node.body.bind(node.clone().into()),
            postLoopLabel,
            preLoopLabel,
        );
        bind_opt!(self, node.update, node.clone().into());
        self.addAntecedent(preLoopLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(postLoopLabel);
    }

    fn bindForInOrForOfStatement(
        &mut self,
        node: BoundNode,
        left: ast::VarDeclOrPat,
        right: ast::Expr,
        body: ast::Stmt,
    ) {
        let continue_target = self.alloc_flow_node(FlowNode::new_loop_label());
        let preLoopLabel = self.setContinueTarget(node.clone().into(), continue_target);
        let postLoopLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        bind!(self, right, node.clone().into());
        self.addAntecedent(preLoopLabel, self.currentFlow);
        self.currentFlow = preLoopLabel;
        self.addAntecedent(postLoopLabel, self.currentFlow);
        bind!(self, left, node.clone().into());
        if let ast::VarDeclOrPat::Pat(_) = left {
            todo!();
            // self.bindAssignmentTargetFlow(left);
        }
        self.bindIterativeStatement(body.bind(node.clone().into()), postLoopLabel, preLoopLabel);
        self.addAntecedent(preLoopLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(postLoopLabel);
    }

    fn bindIfStatement(&mut self, node: Rc<IfStmt>) {
        let thenLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        let elseLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        let postIfLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        self.bindCondition(
            node.test.clone(),
            Some(node.clone().into()),
            thenLabel,
            elseLabel,
        );
        self.currentFlow = self.finishFlowLabel(thenLabel);
        bind!(self, node.cons, node.clone().into());
        self.addAntecedent(postIfLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(elseLabel);
        bind_opt!(self, node.alt, node.clone().into());
        self.addAntecedent(postIfLabel, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(postIfLabel);
    }

    fn bindReturnStatement(&mut self, node: Rc<ReturnStmt>) {
        bind_opt!(self, node.arg, node.clone().into());
        self.hasExplicitReturn = true;
        if let Some(currentReturnTarget) = self.currentReturnTarget {
            self.addAntecedent(currentReturnTarget, self.currentFlow);
        }
        self.currentFlow = self.unreachableFlow;
    }

    fn bindThrowStatement(&mut self, node: Rc<ThrowStmt>) {
        bind!(self, node.arg, node.clone().into());

        self.currentFlow = self.unreachableFlow;
    }

    fn findActiveLabel(&mut self, name: JsWord) -> Option<&mut ActiveLabel> {
        self.activeLabelStack
            .iter_mut()
            .rev()
            .find(|label| label.name == name)
    }

    fn bindBreakOrContinueFlow(
        &mut self,
        node: BoundNode,
        break_target: Option<FlowNodeId>,
        continue_target: Option<FlowNodeId>,
    ) {
        let flowLabelId = if let BoundNode::BreakStmt(_) = node {
            break_target
        } else {
            continue_target
        };
        if let Some(flow_label) = flowLabelId {
            assert_id_is_for_flow_label!(self, flow_label);
            self.addAntecedent(flow_label, self.currentFlow);
            self.currentFlow = self.unreachableFlow;
        }
    }

    fn bindBreakOrContinueStatement(&mut self, node: BoundNode, label: Option<Rc<ast::Ident>>) {
        bind_opt!(self, label, node.clone().into());
        if let Some(label) = label {
            let active_label = self.findActiveLabel(label.sym.clone());
            if let Some(active_label) = active_label {
                active_label.referenced = true;
                let break_target = Some(active_label.breakTarget.clone());
                let continue_target = active_label.continueTarget.clone();
                self.bindBreakOrContinueFlow(node, break_target, continue_target);
            }
        } else {
            self.bindBreakOrContinueFlow(node, self.currentBreakTarget, self.currentContinueTarget);
        }
    }

    fn bindTryStatement(&mut self, node: Rc<TryStmt>) {
        // We conservatively assume that *any* code in the try block can cause an exception, but we only need
        // to track code that causes mutations (because only mutations widen the possible control flow type of
        // a variable). The exceptionLabel is the target label for control flows that result from exceptions.
        // We add all mutation flow nodes as antecedents of this label such that we can analyze them as possible
        // antecedents of the start of catch or finally blocks. Furthermore, we add the current control flow to
        // represent exceptions that occur before any mutations.
        let save_return_target = self.currentReturnTarget;
        let save_exception_target = self.currentExceptionTarget;
        let normalExitLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        let returnLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        let mut exceptionLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        if node.finalizer.is_some() {
            self.currentReturnTarget = Some(returnLabel);
        }
        self.addAntecedent(exceptionLabel, self.currentFlow);
        self.currentExceptionTarget = Some(exceptionLabel);
        bind!(self, node.block, node.clone().into());
        self.addAntecedent(normalExitLabel, self.currentFlow);
        if let Some(catch) = &node.handler {
            // Start of catch clause is the target of exceptions from try block.
            self.currentFlow = self.finishFlowLabel(exceptionLabel);
            // The currentExceptionTarget now represents control flows from exceptions in the catch clause.
            // Effectively, in a try-catch-finally, if an exception occurs in the try block, the catch block
            // acts like a second try block.
            exceptionLabel = self.alloc_flow_node(FlowNode::new_branch_label());
            self.addAntecedent(exceptionLabel, self.currentFlow);
            self.currentExceptionTarget = Some(exceptionLabel);
            bind!(self, catch, node.clone().into());
            self.addAntecedent(normalExitLabel, self.currentFlow);
        }
        self.currentReturnTarget = save_return_target;
        self.currentExceptionTarget = save_exception_target;
        if let Some(finally) = &node.finalizer {
            macro_rules! concat_antecedents {
                ($($name:ident, )+) => {{
                    $(
                        let $name = &self.flow_nodes[$name]
                            .kind
                            .unwrap_flow_label()
                            .antecedents;
                    )+

                    let mut antecedents = Vec::with_capacity(
                        $($name.len() +)+ 0
                    );

                    $(antecedents.extend($name.iter());)+

                    antecedents
                }};
            }
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
            let finallyLabel = FlowNode::new_branch_label_with_antecedents(concat_antecedents!(
                normalExitLabel,
                exceptionLabel,
                returnLabel,
            ));
            let finallyLabel = self.alloc_flow_node(finallyLabel);
            self.currentFlow = finallyLabel;
            bind!(self, finally, node.clone().into());
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
                    let returnLabel = self.flow_nodes[returnLabel].kind.unwrap_flow_label();
                    if !returnLabel.antecedents.is_empty() {
                        let antecedent = FlowNode::new_reduce_label(
                            finallyLabel,
                            returnLabel.antecedents.clone(),
                            self.currentFlow,
                        );
                        let antecedent_id = self.alloc_flow_node(antecedent);
                        self.addAntecedent(currentReturnTarget, antecedent_id);
                    }
                }
                // If we have an outer exception target (i.e. a containing try-finally or try-catch-finally), add a
                // control flow that goes back through the finally blok and back through each possible exception source.
                if let Some(currentExceptionTarget) = self.currentExceptionTarget {
                    let exceptionLabel = self.flow_nodes[exceptionLabel].kind.unwrap_flow_label();
                    if !exceptionLabel.antecedents.is_empty() {
                        let antecedent = FlowNode::new_reduce_label(
                            finallyLabel,
                            exceptionLabel.antecedents.clone(),
                            self.currentFlow,
                        );
                        let antecedent_id = self.alloc_flow_node(antecedent);
                        self.addAntecedent(currentExceptionTarget, antecedent_id);
                    }
                }
                // If the end of the finally block is reachable, but the end of the try and catch blocks are not,
                // convert the current flow to unreachable. For example, 'try { return 1; } finally { ... }' should
                // result in an unreachable current control flow.
                self.currentFlow = {
                    let normalExitLabel = self.flow_nodes[normalExitLabel].kind.unwrap_flow_label();
                    if !normalExitLabel.antecedents.is_empty() {
                        let label = FlowNode::new_reduce_label(
                            finallyLabel,
                            normalExitLabel.antecedents.clone(),
                            self.currentFlow,
                        );

                        self.alloc_flow_node(label)
                    } else {
                        self.unreachableFlow
                    }
                };
            }
        } else {
            self.currentFlow = self.finishFlowLabel(normalExitLabel);
        }
    }

    fn bindSwitchStatement(&mut self, node: Rc<SwitchStmt>) {
        let postSwitchLabel = self.alloc_flow_node(FlowNode::new_branch_label());
        bind!(self, node.discriminant, node.clone().into());
        let saveBreakTarget = self.currentBreakTarget;
        let savePreSwitchCaseFlow = self.preSwitchCaseFlow;
        self.currentBreakTarget = Some(postSwitchLabel);
        let preSwitchCaseFlow = self.currentFlow;
        self.preSwitchCaseFlow = Some(preSwitchCaseFlow);
        self.bindCaseBlock(node.clone(), &node.cases);
        self.addAntecedent(postSwitchLabel, self.currentFlow);
        let hasDefault = node.cases.iter().any(|case| case.test.is_none());
        if !hasDefault {
            let antecedent = self.createFlowSwitchClause(preSwitchCaseFlow, node, 0, 0);
            self.addAntecedent(postSwitchLabel, antecedent);
        }
        self.currentBreakTarget = saveBreakTarget;
        self.preSwitchCaseFlow = savePreSwitchCaseFlow;
        self.currentFlow = self.finishFlowLabel(postSwitchLabel);
    }

    fn bindCaseBlock(&mut self, switch: Rc<SwitchStmt>, cases: &[Rc<ast::SwitchCase>]) {
        let isNarrowingSwitch = isNarrowingExpression(&switch.discriminant);
        let mut fallthroughFlow = self.unreachableFlow;
        let mut i = 0;
        while i < cases.len() {
            let clauseStart = i;
            while cases[i].cons.is_empty() && i + 1 < cases.len() {
                bind!(self, cases[i], switch.clone().into());
                i += 1;
            }
            let preCaseLabel = self.alloc_flow_node(FlowNode::new_branch_label());
            {
                let antecedent = if isNarrowingSwitch {
                    self.createFlowSwitchClause(
                        self.preSwitchCaseFlow.unwrap(),
                        switch.clone(),
                        clauseStart as u32,
                        (i + 1) as u32,
                    )
                } else {
                    self.preSwitchCaseFlow.unwrap()
                };
                self.addAntecedent(preCaseLabel, antecedent);
            }
            self.addAntecedent(preCaseLabel, fallthroughFlow);
            self.currentFlow = self.finishFlowLabel(preCaseLabel);
            bind!(self, cases[i], switch.clone().into());
            fallthroughFlow = self.currentFlow;
            // TODO:
            // if !(self.flow_nodes[self.currentFlow]
            //     .flags
            //     .intersects(FlowFlags::Unreachable))
            //     && i != cases.len() - 1
            //     && options.noFallthroughCasesInSwitch
            // {
            //     cases[i].fallthroughFlowNode = self.currentFlow;
            // }
            i += 1;
        }
    }

    fn bindCaseClause(&mut self, node: Rc<SwitchCase>) {
        let save_current_flow = self.currentFlow;
        self.currentFlow = self.preSwitchCaseFlow.unwrap();
        bind_opt!(self, node.test, node.clone().into());
        self.currentFlow = save_current_flow;
        bind_vec!(self, node.cons, node.clone().into());
    }

    fn bindExpressionStatement(&mut self, node: Rc<ExprStmt>) {
        bind!(self, node.expr, node.clone().into());
        self.maybeBindExpressionFlowIfCall(node.clone().into(), node.expr.clone());
    }

    fn maybeBindExpressionFlowIfCall(&mut self, parent: BoundNode, node: ast::Expr) {
        // A top level or LHS of comma expression call expression with a dotted function name and at least one argument
        // is potentially an assertion and is therefore included in the control flow.
        if let ast::Expr::Call(call) = node {
            if let ast::ExprOrSuper::Expr(callee) = &call.callee {
                if isDottedName(&callee.clone().into()) {
                    self.currentFlow = self.createFlowCall(
                        self.currentFlow,
                        Rc::new(CallExpr {
                            node: call,
                            parent: Some(parent),
                        }),
                    );
                }
            }
        }
    }

    fn bindLabeledStatement(&mut self, node: Rc<LabeledStmt>) {
        let post_statement_label = self.alloc_flow_node(FlowNode::new_branch_label());
        self.activeLabelStack.push(ActiveLabel {
            name: node.label.sym.clone(),
            breakTarget: post_statement_label,
            continueTarget: None,
            referenced: false,
        });
        bind!(self, node.label, node.clone().into());
        bind!(self, node.body, node.clone().into());
        // TODO:
        // if !self.activeLabelStack.last().unwrap().referenced && !options.allowUnusedLabels {
        //     self.errorOrSuggestionOnNode(
        //         unusedLabelIsError(options),
        //         node.label,
        //         Diagnostics.Unused_label,
        //     );
        // }
        self.activeLabelStack.pop();
        self.addAntecedent(post_statement_label, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(post_statement_label);
    }

    // fn bindDestructuringTargetFlow(&mut self, node: ast::Expr) {
    //     todo!();
    //     // match node {
    //     //     ast::Expr::Assign(e) if e.op == ast::AssignOp::Assign => {
    //     //         self.bindAssignmentTargetFlow(e.left)
    //     //     }
    //     //     _ => self.bindAssignmentTargetFlow(node),
    //     // }
    // }

    fn bindAssignmentTargetFlow(&mut self, _node: ast::Expr) {
        todo!();
        // if isNarrowableReference(node) {
        //     self.currentFlow =
        //         self.createFlowMutation(FlowFlags::Assignment, self.currentFlow, node);
        // } else if let ast::Expr::Array(array) = node {
        //     for e in array.elems {
        //         if let Some(elem) = e {
        //             if elem.spread.is_some() {
        //                 self.bindAssignmentTargetFlow(elem.expr);
        //             } else {
        //                 self.bindDestructuringTargetFlow(elem.expr);
        //             }
        //         }
        //     }
        // } else if let ast::Expr::Object(object) = node {
        //     for p in object.props {
        //         match p {
        //             ast::PropOrSpread::Spread(s) => self.bindAssignmentTargetFlow(s.expr),
        //             ast::PropOrSpread::Prop(p) => match p {
        //                 ast::Prop::KeyValue(p) => self.bindDestructuringTargetFlow(p.value),
        //                 ast::Prop::Shorthand(p) => self.bindAssignmentTargetFlow(p.sym.into()),
        //                 _ => todo!("Should this match be exhaustive?"),
        //             },
        //         }
        //     }
        // }
    }

    // fn bindLogicalLikeExpression(
    //     &mut self,
    //     node: ast::Expr,
    //     trueTarget: FlowNodeId,
    //     falseTarget: FlowNodeId,
    // ) {
    //     todo!();
    //     // assert_id_is_for_flow_label!(self, trueTarget);
    //     // assert_id_is_for_flow_label!(self, falseTarget);

    //     // let preRightLabel = self.alloc_flow_node(FlowNode::new_branch_label());
    //     // match node {
    //     //     ast::Expr::Bin(e) if e.op = ast::BinaryOp::LogicalAnd => self.bindCondition(node.left, preRightLabel, falseTarget),
    //     //     ast::Expr::Assign(e) if e.op == ast::AssignOp::AndAssign => self.bindCondition(node.left, preRightLabel, falseTarget),
    //     //     _ =>self.bindCondition(node.left, trueTarget, preRightLabel)
    //     // }
    //     // if node.op == ast::BinaryOp::LogicalAnd
    //     //     || node.op == SyntaxKind.AmpersandAmpersandEqualsToken
    //     // {
    //     //     self.bindCondition(node.left, preRightLabel, falseTarget);
    //     // } else {
    //     //     self.bindCondition(node.left, trueTarget, preRightLabel);
    //     // }
    //     // self.currentFlow = self.finishFlowLabel(preRightLabel);
    //     // self.bind(node.operatorToken);

    //     // if isLogicalOrCoalescingAssignmentOperator(node.operatorToken.kind) {
    //     //     self.doWithConditionalBranches(bind, node.right, trueTarget, falseTarget);
    //     //     self.bindAssignmentTargetFlow(node.left);

    //     //     self.addAntecedent(
    //     //         trueTarget,
    //     //         self.createFlowCondition(FlowFlags::TrueCondition, self.currentFlow, node),
    //     //     );
    //     //     self.addAntecedent(
    //     //         falseTarget,
    //     //         self.createFlowCondition(FlowFlags::FalseCondition, self.currentFlow, node),
    //     //     );
    //     // } else {
    //     //     self.bindCondition(node.right, trueTarget, falseTarget);
    //     // }
    // }

    fn bindUnaryExpressionFlow(&mut self, node: Rc<UnaryExpr>) {
        if node.op == ast::UnaryOp::Bang {
            let save_true_target = self.currentTrueTarget;
            self.currentTrueTarget = self.currentFalseTarget;
            self.currentFalseTarget = save_true_target;
            bind!(self, node.arg, node.clone().into());
            self.currentFalseTarget = self.currentTrueTarget;
            self.currentTrueTarget = save_true_target;
        } else if node.op == ast::UnaryOp::Delete {
            bind!(self, node.arg, node.clone().into());
            if matches!(node.arg, ast::Expr::Member(_)) {
                self.bindAssignmentTargetFlow(node.arg.clone());
            }
        } else {
            bind!(self, node.arg, node.clone().into());
        }
    }

    fn bindUpdateExpressionFlow(&mut self, node: Rc<UpdateExpr>) {
        bind!(self, node.arg, node.clone().into());
        todo!();
        // self.bindAssignmentTargetFlow(node.operand);
    }

    // function bindDestructuringAssignmentFlow(node: DestructuringAssignment) {
    //     if (inAssignmentPattern) {
    //         inAssignmentPattern = false;
    //         bind(node.operatorToken);
    //         bind(node.right);
    //         inAssignmentPattern = true;
    //         bind(node.left);
    //     }
    //     else {
    //         inAssignmentPattern = true;
    //         bind(node.left);
    //         inAssignmentPattern = false;
    //         bind(node.operatorToken);
    //         bind(node.right);
    //     }
    //     bindAssignmentTargetFlow(node.left);
    // }

    // function createBindBinaryExpressionFlow() {
    //     interface WorkArea {
    //         stackIndex: number;
    //         skip: boolean;
    //         inStrictModeStack: (boolean | undefined)[];
    //         parentStack: (Node | undefined)[];
    //     }

    //     return createBinaryExpressionTrampoline(onEnter, onLeft, onOperator, onRight, onExit, /*foldState*/ undefined);

    //     function onEnter(node: BinaryExpression, state: WorkArea | undefined) {
    //         if (state) {
    //             state.stackIndex++;
    //             // Emulate the work that `bind` does before reaching `bindChildren`. A normal call to
    //             // `bindBinaryExpressionFlow` will already have done this work.
    //             setParent(node, parent);
    //             const saveInStrictMode = inStrictMode;
    //             bindWorker(node);
    //             const saveParent = parent;
    //             parent = node;
    //             state.skip = false;
    //             state.inStrictModeStack[state.stackIndex] = saveInStrictMode;
    //             state.parentStack[state.stackIndex] = saveParent;
    //         }
    //         else {
    //             state = {
    //                 stackIndex: 0,
    //                 skip: false,
    //                 inStrictModeStack: [undefined],
    //                 parentStack: [undefined]
    //             };
    //         }
    //         // TODO: bindLogicalExpression is recursive - if we want to handle deeply nested `&&` expressions
    //         // we'll need to handle the `bindLogicalExpression` scenarios in this state machine, too
    //         // For now, though, since the common cases are chained `+`, leaving it recursive is fine
    //         const operator = node.operatorToken.kind;
    //         if (operator === SyntaxKind.AmpersandAmpersandToken ||
    //             operator === SyntaxKind.BarBarToken ||
    //             operator === SyntaxKind.QuestionQuestionToken ||
    //             isLogicalOrCoalescingAssignmentOperator(operator)) {
    //             if (isTopLevelLogicalExpression(node)) {
    //                 const postExpressionLabel = createBranchLabel();
    //                 bindLogicalLikeExpression(node, postExpressionLabel, postExpressionLabel);
    //                 currentFlow = finishFlowLabel(postExpressionLabel);
    //             }
    //             else {
    //                 bindLogicalLikeExpression(node, currentTrueTarget!, currentFalseTarget!);
    //             }
    //             state.skip = true;
    //         }
    //         return state;
    //     }

    //     function onLeft(left: Expression, state: WorkArea, _node: BinaryExpression) {
    //         if (!state.skip) {
    //             return maybeBind(left);
    //         }
    //     }

    //     function onOperator(operatorToken: BinaryOperatorToken, state: WorkArea, node: BinaryExpression) {
    //         if (!state.skip) {
    //             if (operatorToken.kind === SyntaxKind.CommaToken) {
    //                 maybeBindExpressionFlowIfCall(node.left);
    //             }
    //             bind(operatorToken);
    //         }
    //     }

    //     function onRight(right: Expression, state: WorkArea, _node: BinaryExpression) {
    //         if (!state.skip) {
    //             return maybeBind(right);
    //         }
    //     }

    //     function onExit(node: BinaryExpression, state: WorkArea) {
    //         if (!state.skip) {
    //             const operator = node.operatorToken.kind;
    //             if (isAssignmentOperator(operator) && !isAssignmentTarget(node)) {
    //                 bindAssignmentTargetFlow(node.left);
    //                 if (operator === SyntaxKind.EqualsToken && node.left.kind === SyntaxKind.ElementAccessExpression) {
    //                     const elementAccess = node.left as ElementAccessExpression;
    //                     if (isNarrowableOperand(elementAccess.expression)) {
    //                         currentFlow = createFlowMutation(FlowFlags.ArrayMutation, currentFlow, node);
    //                     }
    //                 }
    //             }
    //         }
    //         const savedInStrictMode = state.inStrictModeStack[state.stackIndex];
    //         const savedParent = state.parentStack[state.stackIndex];
    //         if (savedInStrictMode !== undefined) {
    //             inStrictMode = savedInStrictMode;
    //         }
    //         if (savedParent !== undefined) {
    //             parent = savedParent;
    //         }
    //         state.skip = false;
    //         state.stackIndex--;
    //     }

    //     function maybeBind(node: Node) {
    //         if (node && isBinaryExpression(node) && !isDestructuringAssignment(node)) {
    //             return node;
    //         }
    //         bind(node);
    //     }
    // }

    fn bindConditionalExpressionFlow(&mut self, node: Rc<CondExpr>) {
        let true_label = self.alloc_flow_node(FlowNode::new_branch_label());
        let false_label = self.alloc_flow_node(FlowNode::new_branch_label());
        let post_expression_label = self.alloc_flow_node(FlowNode::new_branch_label());
        self.bindCondition(
            node.test.clone(),
            Some(node.clone().into()),
            true_label,
            false_label,
        );
        self.currentFlow = self.finishFlowLabel(true_label);
        bind!(self, node.cons, node.clone().into());
        self.addAntecedent(post_expression_label, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(false_label);
        bind!(self, node.alt, node.clone().into());
        self.addAntecedent(post_expression_label, self.currentFlow);
        self.currentFlow = self.finishFlowLabel(post_expression_label);
    }

    fn bindInitializedVariableFlow(&mut self, node: BoundNode) {
        debug_assert!(matches!(
            node,
            BoundNode::VarDeclarator(_)
                | BoundNode::BindingIdent(_)
                | BoundNode::ArrayPat(_)
                | BoundNode::RestPat(_)
                | BoundNode::ObjectPat(_)
                | BoundNode::AssignPat(_)
        ));
        // TODO:
        // let name = if !isOmittedExpression(node) {node.name }else{ None};
        let name = match node {
            BoundNode::VarDeclarator(ref d) => d.name.bind(node.clone()),
            _ => node,
        };
        match name {
            BoundNode::ArrayPat(ref p) => {
                for child in &p.elems {
                    if let Some(child) = child {
                        self.bindInitializedVariableFlow(child.bind(name.clone()));
                    }
                }
            }
            BoundNode::ObjectPat(ref p) => {
                for child in &p.props {
                    self.bindInitializedVariableFlow(child.bind(name.clone()));
                }
            }
            BoundNode::BindingIdent(_) => {
                self.currentFlow = self.createFlowAssignment(self.currentFlow, name);
            }
            _ => unreachable!(),
        }
    }

    fn bindVariableDeclarationFlow(&mut self, node: BoundNode, d: Rc<VarDeclarator>) {
        bind!(self, d.name, node.clone());
        bind_opt!(self, d.init, node.clone());
        if d.init.is_some()
            || matches!(
                node.parent().and_then(|p| p.parent()),
                Some(BoundNode::ForInStmt(_) | BoundNode::ForOfStmt(_))
            )
        {
            self.bindInitializedVariableFlow(node);
        }
    }

    // fn bindJSDocTypeAlias(node: JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag)
    // fn bindJSDocClassTag(node: JSDocClassTag)
    // fn bindOptionalExpression(node: Expression, trueTarget: FlowLabel, falseTarget: FlowLabel)
    // fn bindOptionalChainRest(node: OptionalChain)
    // fn bindOptionalChain(node: OptionalChain, trueTarget: FlowLabel, falseTarget: FlowLabel)
    // fn bindOptionalChainFlow(node: OptionalChain)
    // fn bindNonNullExpressionFlow(node: NonNullExpression | NonNullChain)

    fn bindAccessExpressionFlow(&mut self, node: BoundNode, expr: Rc<MemberExpr>) {
        if matches!(expr.prop, ast::Expr::OptChain(_)) {
            todo!();
            // self.bindOptionalChainFlow(node);
        } else {
            bind!(self, expr.obj, node.clone());
            bind!(self, expr.prop, node.clone());
        }
    }

    fn bindCallExpressionFlow(&mut self, node: &Rc<CallExpr>) {
        let bound_node = BoundNode::CallExpr(node.clone());
        // TODO: optional chain
        // if (isOptionalChain(node)) {
        //     bindOptionalChainFlow(node);
        // }
        // else {
        // If the target of the call expression is a function expression or arrow function we have
        // an immediately invoked function expression (IIFE). Initialize the flowNode property to
        // the current control flow (which includes evaluation of the IIFE arguments).
        let expr = skipParenthesesOfNode(node.callee.bind(bound_node.clone()));
        if matches!(expr, BoundNode::FnExpr(_) | BoundNode::ArrowExpr(_)) {
            bind_opt!(self, node.type_args, bound_node.clone());
            bind_vec!(self, node.args, bound_node.clone());
            self.bind(Some(node.callee.bind(bound_node)));
        } else {
            self.bindEachChild(bound_node);
            if matches!(node.callee, ast::ExprOrSuper::Super(_)) {
                self.currentFlow = self.createFlowCall(self.currentFlow, node.clone());
            }
        }
        // }
        if let ast::ExprOrSuper::Expr(ast::Expr::Member(callee)) = &node.callee {
            if !callee.computed {
                if let ast::Expr::Ident(prop) = &callee.prop {
                    if isNarrowableOperand(&callee.obj.clone().into())
                        && isPushOrUnshiftIdentifier(&prop.sym)
                    {
                        todo!();
                        // self.currentFlow =
                        //     self.createFlowMutation(FlowFlags::ArrayMutation, self.currentFlow, node);
                    }
                }
            }
        }
    }

    fn getContainerFlags(&self, node: BoundNode) -> ContainerFlags {
        match node {
            // | BoundNode::JSDocTypeLiteral(_)
            // | BoundNode::JsxAttributes(_)
            BoundNode::ClassExpr(_)
            | BoundNode::ClassDecl(_)
            | BoundNode::TsEnumDecl(_)
            | BoundNode::ObjectLit(_)
            | BoundNode::TsTypeLit(_) => ContainerFlags::IsContainer,

            BoundNode::TsInterfaceDecl(_) => {
                ContainerFlags::IsContainer | ContainerFlags::IsInterface
            }

            BoundNode::TsModuleDecl(_)
            | BoundNode::TsTypeAliasDecl(_)
            | BoundNode::TsMappedType(_) => ContainerFlags::IsContainer | ContainerFlags::HasLocals,

            BoundNode::Module(_) | BoundNode::Script(_) => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
            }
            BoundNode::ClassMethod(_)
            | BoundNode::PrivateMethod(_)
            | BoundNode::MethodProp(_)
            | BoundNode::GetterProp(_)
            | BoundNode::SetterProp(_) => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike
                    | ContainerFlags::IsObjectLiteralOrClassExpressionMethodOrAccessor
            }
            // | BoundNode::JSDocSignature(_)
            // | BoundNode::JSDocFunctionType(_)
            // TODO: static blocks:
            // | BoundNode::ClassStaticBlockDeclaration(_)
            BoundNode::Constructor(_)
            | BoundNode::FnDecl(_)
            | BoundNode::TsMethodSignature(_)
            | BoundNode::TsCallSignatureDecl(_)
            | BoundNode::TsFnType(_)
            | BoundNode::TsConstructSignatureDecl(_)
            | BoundNode::TsIndexSignature(_)
            | BoundNode::TsConstructorType(_) => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike
            }
            BoundNode::FnExpr(_) | BoundNode::ArrowExpr(_) => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike
                    | ContainerFlags::IsFunctionExpression
            }

            BoundNode::TsModuleBlock(_) => ContainerFlags::IsControlFlowContainer,
            BoundNode::ClassProp(ref p) if p.value.is_some() => {
                ContainerFlags::IsControlFlowContainer
            }
            BoundNode::PrivateProp(ref p) if p.value.is_some() => {
                ContainerFlags::IsControlFlowContainer
            }

            BoundNode::CatchClause(_)
            | BoundNode::ForStmt(_)
            | BoundNode::ForInStmt(_)
            | BoundNode::ForOfStmt(_)
            | BoundNode::SwitchStmt(_) => ContainerFlags::IsBlockScopedContainer,

            BoundNode::BlockStmt(node) => {
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

                // TODO: static blocks:
                // || isClassStaticBlockDeclaration(node.parent)
                if isFunctionLike(node.parent.as_ref()) {
                    ContainerFlags::None
                } else {
                    ContainerFlags::IsBlockScopedContainer
                }
            }
            _ => ContainerFlags::None,
        }
    }

    fn addToContainerChain(&mut self, next: BoundNode) {
        if let Some(ref last_container) = self.lastContainer {
            self.node_data_mut(last_container.clone()).nextContainer = Some(next.clone());
        }

        self.lastContainer = Some(next);
    }

    fn declareSymbolAndAddToSymbolTable(
        &mut self,
        node: BoundNode,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) -> SymbolId {
        let container_node = match &self.container {
            Some(c) => c.clone(),
            None => unreachable!("symbol must be declared within a container"),
        };
        let container_symbol_id = self.node_data(container_node.clone()).symbol;

        match container_node {
            // Modules, source files, and classes need specialized handling for how their
            // members are declared (for example, a member of a class will go into a specific
            // symbol table depending on if it is static or not). We defer to specialized
            // handlers to take care of declaring these child members.
            BoundNode::TsModuleDecl(_) => {
                self.declareModuleMember(node, symbolFlags, symbolExcludes)
            }

            BoundNode::Script(_) | BoundNode::Module(_) => {
                self.declareSourceFileMember(node, symbolFlags, symbolExcludes)
            }

            BoundNode::ClassExpr(_) | BoundNode::ClassDecl(_) => {
                self.declareClassMember(node, symbolFlags, symbolExcludes)
            }

            BoundNode::TsEnumDecl(_) => {
                let container_symbol_id = container_symbol_id.unwrap();
                declare_symbol!(
                    self,
                    self.symbols[container_symbol_id].exports().unwrap(),
                    Some(container_symbol_id),
                    node,
                    symbolFlags,
                    symbolExcludes,
                    false,
                    false
                )
            }

            // TODO: jsdoc
            // | BoundNode::JSDocTypeLiteral(_)
            //todo:
            // | BoundNode::JsxAttributes(_)
            BoundNode::TsTypeLit(_) | BoundNode::ObjectLit(_) | BoundNode::TsInterfaceDecl(_) => {
                // Interface/Object-types always have their children added to the 'members' of
                // their container. They are only accessible through an instance of their
                // container, and are never in scope otherwise (even inside the body of the
                // object / type / interface declaring them). An exception is type parameters,
                // which are in scope without qualification (similar to 'locals').
                let container_symbol_id = container_symbol_id.unwrap();
                declare_symbol!(
                    self,
                    self.symbols[container_symbol_id].members().unwrap(),
                    Some(container_symbol_id),
                    node,
                    symbolFlags,
                    symbolExcludes,
                    false,
                    false
                )
            }

            // TODO: static blocks:
            // | BoundNode::ClassStaticBlockDeclaration(_)
            // TODO: jsdoc
            // | BoundNode::JSDocFunctionType(_)
            // | BoundNode::JSDocTypedefTag(_)
            // | BoundNode::JSDocCallbackTag(_)
            // | BoundNode::JSDocSignature(_)
            BoundNode::TsFnType(_)
            | BoundNode::TsConstructorType(_)
            | BoundNode::TsCallSignatureDecl(_)
            | BoundNode::TsConstructSignatureDecl(_)
            | BoundNode::TsIndexSignature(_)
            | BoundNode::TsMethodSignature(_)
            | BoundNode::Constructor(_)
            | BoundNode::ClassMethod(_)
            | BoundNode::PrivateMethod(_)
            | BoundNode::GetterProp(_)
            | BoundNode::SetterProp(_)
            | BoundNode::MethodProp(_)
            | BoundNode::FnDecl(_)
            | BoundNode::FnExpr(_)
            | BoundNode::ArrowExpr(_)
            | BoundNode::TsTypeAliasDecl(_)
            | BoundNode::TsMappedType(_) => {
                // All the children of these container types are never visible through another
                // symbol (i.e. through another symbol's 'exports' or 'members').  Instead,
                // they're only accessed 'lexically' (i.e. from code that exists underneath
                // their container in the tree). To accomplish this, we simply add their declared
                // symbol to the 'locals' of the container.  These symbols can then be found as
                // the type checker walks up the containers, checking them for matching names.
                declare_symbol!(
                    self,
                    self.node_data(container_node.clone()).locals.unwrap(),
                    None,
                    node,
                    symbolFlags,
                    symbolExcludes,
                    false,
                    false
                )
            }
            _ => unreachable!(),
        }
    }

    fn declareClassMember(
        &mut self,
        node: BoundNode,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) -> SymbolId {
        let container_node = match &self.container {
            Some(c) => c.clone(),
            None => unreachable!("symbol must be declared within a container"),
        };
        let container_symbol_id = self.node_data(container_node).symbol.unwrap();

        if isStatic(&node) {
            declare_symbol!(
                self,
                self.symbols[container_symbol_id].exports().unwrap(),
                Some(container_symbol_id),
                node,
                symbolFlags,
                symbolExcludes,
                false,
                false
            )
        } else {
            declare_symbol!(
                self,
                self.symbols[container_symbol_id].members().unwrap(),
                Some(container_symbol_id),
                node,
                symbolFlags,
                symbolExcludes,
                false,
                false
            )
        }
    }

    fn declareSourceFileMember(
        &mut self,
        node: BoundNode,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) -> SymbolId {
        match self.program() {
            ast::Program::Module(_) => self.declareModuleMember(node, symbolFlags, symbolExcludes),
            ast::Program::Script(s) => {
                let locals = self.node_data(s.bind_to_opt_parent(None)).locals.unwrap();
                declare_symbol!(
                    self,
                    locals,
                    None,
                    node,
                    symbolFlags,
                    symbolExcludes,
                    false,
                    false
                )
            }
        }
    }

    fn bindModuleDeclaration(&mut self, node: BoundNode, module: Rc<TsModuleDecl>) {
        self.setExportContextFlag(&module);
        if isAmbientModule(&node) {
            todo!();
            // if hasSyntacticModifier(node, ModifierFlags.Export) {
            //     errorOnFirstToken(node, Diagnostics.export_modifier_cannot_be_applied_to_ambient_modules_and_module_augmentations_since_they_are_always_visible);
            // }
            // if isModuleAugmentationExternal(node) {
            //     self.declareModuleSymbol(node);
            // } else {
            //     let pattern: string | Pattern | undefined;
            //     if (node.name.kind == SyntaxKind.StringLiteral) {
            //         let text  = node.name.text;
            //         pattern = tryParsePattern(text);
            //         if (pattern == undefined) {
            //             errorOnFirstToken(node.name, Diagnostics.Pattern_0_can_have_at_most_one_Asterisk_character, text);
            //         }
            //     }

            //     let symbol = declareSymbolAndAddToSymbolTable(node, SymbolFlags.ValueModule, SymbolFlags.ValueModuleExcludes)!;
            //     file.patternAmbientModules = append<PatternAmbientModule>(file.patternAmbientModules, pattern && !isString(pattern) ? { pattern, symbol } : undefined);
            // }
        } else {
            let state = self.declareModuleSymbol(node.clone(), module);
            if state != ModuleInstanceState::NonInstantiated {
                let symbol = self.node_data(node).symbol.unwrap();
                let symbol = &mut self.symbols[symbol];
                // if module was already merged with some function, class or non-const enum, treat it as non-const-enum-only
                *symbol.constEnumOnlyModule_mut() = !symbol.flags().contains(SymbolFlags::Function | SymbolFlags::Class | SymbolFlags::RegularEnum)
                    // Current must be `const enum` only
                    && state == ModuleInstanceState::ConstEnumOnly
                    // Can't have been set to 'false' in a previous merged symbol. ('undefined' OK)
                    && symbol.constEnumOnlyModule() != false;
            }
        }
    }

    fn declareModuleSymbol(
        &mut self,
        node: BoundNode,
        module: Rc<TsModuleDecl>,
    ) -> ModuleInstanceState {
        let state = getModuleInstanceState(node.clone(), module);
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

    fn bindFunctionOrConstructorType(
        &mut self,
        node: BoundNode, /*node: SignatureDeclaration | JSDocSignature*/
    ) {
        // For a given function symbol "<...>(...) => T" we want to generate a symbol identical
        // to the one we would get for: { <...>(...): T }
        //
        // We do that by making an anonymous type literal symbol, and then setting the function
        // symbol as its sole member. To the rest of the system, this symbol will be indistinguishable
        // from an actual type literal symbol you would have gotten had you used the long form.
        let symbol_name = self.getDeclarationName(node.clone());
        let symbol = self.createSymbol(SymbolFlags::Signature, symbol_name.clone());
        self.addDeclarationToSymbol(symbol, node.clone(), SymbolFlags::Signature);

        let typeLiteralSymbol =
            self.createSymbol(SymbolFlags::TypeLiteral, InternalSymbolName::Type.into());
        self.addDeclarationToSymbol(typeLiteralSymbol, node, SymbolFlags::TypeLiteral);
        let members = new_ahash_map![(symbol_name, symbol)];
        *self.symbols[typeLiteralSymbol].members_mut() = Some(self.symbol_tables.push(members));
    }

    fn bindObjectLiteralExpression(&mut self, node: BoundNode) {
        // TODO:
        // enum ElementKind {
        //     Property,
        //     Accessor
        // }

        // if inStrictMode && !isAssignmentTarget(node) {
        //     const seen = new Map<__String, ElementKind>();

        //     for (const prop of node.properties) {
        //         if (prop.kind === SyntaxKind.SpreadAssignment || prop.name.kind !== SyntaxKind.Identifier) {
        //             continue;
        //         }

        //         const identifier = prop.name;

        //         // ECMA-262 11.1.5 Object Initializer
        //         // If previous is not undefined then throw a SyntaxError exception if any of the following conditions are true
        //         // a.This production is contained in strict code and IsDataDescriptor(previous) is true and
        //         // IsDataDescriptor(propId.descriptor) is true.
        //         //    b.IsDataDescriptor(previous) is true and IsAccessorDescriptor(propId.descriptor) is true.
        //         //    c.IsAccessorDescriptor(previous) is true and IsDataDescriptor(propId.descriptor) is true.
        //         //    d.IsAccessorDescriptor(previous) is true and IsAccessorDescriptor(propId.descriptor) is true
        //         // and either both previous and propId.descriptor have[[Get]] fields or both previous and propId.descriptor have[[Set]] fields
        //         const currentKind = prop.kind == SyntaxKind.PropertyAssignment || prop.kind === SyntaxKind.ShorthandPropertyAssignment || prop.kind === SyntaxKind.MethodDeclaration
        //             ? ElementKind.Property
        //             : ElementKind.Accessor;

        //         const existingKind = seen.get(identifier.escapedText);
        //         if (!existingKind) {
        //             seen.set(identifier.escapedText, currentKind);
        //             continue;
        //         }

        //         if (currentKind == ElementKind.Property && existingKind == ElementKind.Property) {
        //             const span = getErrorSpanForNode(file, identifier);
        //             file.bindDiagnostics.push(createFileDiagnostic(file, span.start, span.length,
        //                 Diagnostics.An_object_literal_cannot_have_multiple_properties_with_the_same_name_in_strict_mode));
        //         }
        //     }
        // }

        self.bindAnonymousDeclaration(
            node,
            SymbolFlags::ObjectLiteral,
            InternalSymbolName::Object.into(),
        );
    }

    //TODO:
    // bindJsxAttributes
    // bindJsxAttribute

    fn bindAnonymousDeclaration(
        &mut self,
        node: BoundNode,
        symbolFlags: SymbolFlags,
        name: JsWord,
    ) -> SymbolId {
        let symbol = self.createSymbol(symbolFlags, name);
        if symbolFlags.intersects(SymbolFlags::EnumMember | SymbolFlags::ClassMember) {
            let container_node = self.container.clone().unwrap();
            let container_symbol = self.node_data(container_node).symbol;
            *self.symbols[symbol].parent_mut() = container_symbol;
        }
        self.addDeclarationToSymbol(symbol, node, symbolFlags);
        symbol
    }

    fn bindBlockScopedDeclaration(
        &mut self,
        node: BoundNode,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) {
        let container_node = match &self.blockScopeContainer {
            Some(c) => c.clone(),
            None => unreachable!("symbol must be declared within a container"),
        };

        match container_node {
            BoundNode::TsModuleDecl(_) | BoundNode::Module(_) => {
                self.declareModuleMember(node, symbolFlags, symbolExcludes);
                return;
            }
            BoundNode::Script(_) => {
                // TODO: for commonjs
                // if isExternalOrCommonJsModule(container as SourceFile) {
                //     self.declareModuleMember(node, symbolFlags, symbolExcludes);
                //     return;
                // }
            }
            _ => {}
        }

        let locals = match self.node_data(container_node.clone()).locals {
            Some(locals) => locals,
            None => {
                self.addToContainerChain(container_node.clone());
                let locals = self.symbol_tables.push(SymbolTable::default());
                self.node_data_mut(container_node.clone()).locals = Some(locals);
                locals
            }
        };

        declare_symbol!(
            self,
            locals,
            None,
            node,
            symbolFlags,
            symbolExcludes,
            false,
            false
        );
    }

    // delayedBindJSDocTypedefTag
    // checkContextualIdentifier
    // getStrictModeIdentifierMessage
    // checkPrivateIdentifier
    // checkStrictModeBinaryExpression
    // checkStrictModeCatchClause
    // checkStrictModeDeleteExpression
    // isEvalOrArgumentsIdentifier
    // checkStrictModeEvalOrArguments
    // getStrictModeEvalOrArgumentsMessage
    // checkStrictModeFunctionName
    // getStrictModeBlockScopeFunctionDeclarationMessage
    // checkStrictModeFunctionDeclaration
    // checkStrictModeNumericLiteral
    // checkStrictModePostfixUnaryExpression
    // checkStrictModePrefixUnaryExpression
    // checkStrictModeWithStatement
    // checkStrictModeLabeledStatement
    // errorOnFirstToken
    // errorOrSuggestionOnNode
    // errorOrSuggestionOnRange
    // addErrorOrSuggestionDiagnostic
    // todo: above ^^^^

    fn bind(&mut self, node: Option<BoundNode>) {
        let node = match node {
            Some(node) => node,
            None => return,
        };

        // self.setParent(node.clone());
        // const saveInStrictMode = inStrictMode;

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
        self.bindWorker(node.clone());
        // Then we recurse into the children of the node to bind them as well. For certain
        // symbols we do specialized work when we recurse. For example, we'll keep track of
        // the current 'container' node when it changes. This helps us know which symbol table
        // a local should go into for example. Since terminal nodes are known not to have
        // children, as an optimization we don't process those.
        // if node.kind > SyntaxKind.LastToken {
        let save_parent = mem::replace(&mut self.parent, Some(node.clone()));
        let container_flags = self.getContainerFlags(node.clone());
        if container_flags == ContainerFlags::None {
            self.bindChildren(node.clone());
        } else {
            self.bindContainer(node.clone(), container_flags);
        }
        self.parent = save_parent;
        // }
        // else {
        //     const save_parent = parent;
        //     if (node.kind == SyntaxKind.EndOfFileToken) parent = node;
        //     bindJSDoc(node);
        //     parent = save_parent;
        // }
        // inStrictMode = saveInStrictMode;
    }

    //todo:
    // bindJSDoc
    // updateStrictModeStatementList
    // isUseStrictPrologueDirective

    fn bindWorker(&mut self, node: BoundNode) {
        match node {
            // TODO:
            /* Strict mode checks */
            BoundNode::Ident(_) => {
                // for typedef type names with namespaces, bind the new jsdoc type symbol here
                // because it requires all containing namespaces to be in effect, namely the
                // current "blockScopeContainer" needs to be set to its immediate namespace parent.
                // if (ident.isInJSDocNamespace) {
                //     let parentNode = node.parent;
                //     while (parentNode && !isJSDocTypeAlias(parentNode)) {
                //         parentNode = parentNode.parent;
                //     }
                //     bindBlockScopedDeclaration(parentNode as Declaration, SymbolFlags::TypeAlias, SymbolFlags::TypeAliasExcludes);
                //     return;
                // }

                self.node_data_mut(node).flowNode = Some(self.currentFlow);
            }
            BoundNode::ThisExpr(_) => {
                self.node_data_mut(node).flowNode = Some(self.currentFlow);
            }
            BoundNode::TsQualifiedName(_) => {
                if isPartOfTypeQuery(node.clone()) {
                    self.node_data_mut(node.clone()).flowNode = Some(self.currentFlow);
                }
            }
            BoundNode::MetaPropExpr(_) | BoundNode::Super(_) => {
                self.node_data_mut(node).flowNode = Some(self.currentFlow);
            }
            // BoundNode::PrivateIdentifier(_) => {
            //     return checkPrivateIdentifier(node as PrivateIdentifier);
            // }
            BoundNode::MemberExpr(ref expr) => {
                if isNarrowableReference(&expr.node.clone().into()) {
                    self.node_data_mut(node).flowNode = Some(self.currentFlow);
                }
                // TODO:
                // if isSpecialPropertyDeclaration(expr) {
                //     self.bindSpecialPropertyDeclaration(expr);
                // }
                // TODO: commonjs
                // if (isInJSFile(expr) &&
                //     file.commonJsModuleIndicator &&
                //     isModuleExportsAccessExpression(expr) &&
                //     !lookupSymbolForName(blockScopeContainer, "module" as __String)) {
                //     declareSymbol(file.locals!, /*parent*/ undefined, expr.expression,
                //         SymbolFlags::FunctionScopedVariable | SymbolFlags::ModuleExports, SymbolFlags::FunctionScopedVariableExcludes);
                // }
            }
            // BoundNode::BinaryExpression(_)=> {
            //     let  specialKind = getAssignmentDeclarationKind(node as BinaryExpression);
            //     match specialKind {
            //         AssignmentDeclarationKind::ExportsProperty(_) => {
            //             bindExportsPropertyAssignment(node as BindableStaticPropertyAssignmentExpression);

            //         }
            //         AssignmentDeclarationKind::ModuleExports(_) => {
            //             bindModuleExportsAssignment(node as BindablePropertyAssignmentExpression);

            //         }
            //         AssignmentDeclarationKind::PrototypeProperty(_) => {
            //             bindPrototypePropertyAssignment((node as BindableStaticPropertyAssignmentExpression).left, node);

            //         }
            //         AssignmentDeclarationKind::Prototype(_) => {
            //             bindPrototypeAssignment(node as BindableStaticPropertyAssignmentExpression);

            //         }
            //         AssignmentDeclarationKind::ThisProperty(_) => {
            //             bindThisPropertyAssignment(node as BindablePropertyAssignmentExpression);

            //         }
            //         AssignmentDeclarationKind::Property(_) => {
            //             let expression = ((node as BinaryExpression).left as AccessExpression).expression;
            //             if (isInJSFile(node) && isIdentifier(expression)) {
            //                 let symbol = lookupSymbolForName(blockScopeContainer, expression.escapedText);
            //                 if (isThisInitializedDeclaration(symbol?.valueDeclaration)) {
            //                     bindThisPropertyAssignment(node as BindablePropertyAssignmentExpression);
            //                     break;
            //                 }
            //             }
            //             bindSpecialPropertyAssignment(node as BindablePropertyAssignmentExpression);
            //         }
            //         AssignmentDeclarationKind::None(_) => {
            //             // Nothing to do
            //         }
            //         _ => {
            //             panic!("Unknown binary expression special property assignment kind");
            //         }
            //     }
            //     return checkStrictModeBinaryExpression(node as BinaryExpression);
            // }
            // BoundNode::CatchClause(_)=>{
            //     return checkStrictModeCatchClause(node as CatchClause);
            // }
            // BoundNode::DeleteExpression(_)=>{
            //     return checkStrictModeDeleteExpression(node as DeleteExpression);
            // }
            // BoundNode::NumericLiteral(_)=>{
            //     return checkStrictModeNumericLiteral(node as NumericLiteral);
            // }
            // BoundNode::PostfixUnaryExpression(_)=>{
            //     return checkStrictModePostfixUnaryExpression(node as PostfixUnaryExpression);
            // }
            // BoundNode::PrefixUnaryExpression(_)=>{
            //     return checkStrictModePrefixUnaryExpression(node as PrefixUnaryExpression);
            // }
            // BoundNode::WithStatement(_)=>{
            //     return checkStrictModeWithStatement(node as WithStatement);
            // }
            // BoundNode::LabeledStatement(_)=>{
            //     return checkStrictModeLabeledStatement(node as LabeledStatement);
            // }
            BoundNode::TsThisType(_) => {
                self.seenThisKeyword = true;
            }
            BoundNode::TsTypePredicate(_) => {
                // Binding the children will handle everything
            }
            BoundNode::TsTypeParamDecl(ref p) => {
                return self.bindTypeParameter(node.clone(), p.clone());
            }
            BoundNode::Param(ref p) => {
                return self.bindParameter(p.clone().into());
            }
            BoundNode::ParamWithoutDecorators(ref p) => {
                return self.bindParameter(p.clone().into());
            }
            BoundNode::TsAmbientParam(ref p) => {
                return self.bindParameter(p.clone().into());
            }
            BoundNode::TsParamProp(ref p) => {
                return self.bindParameter(p.clone().into());
            }
            BoundNode::VarDeclarator(ref v) => {
                return self.bindVariableDeclaration(node.clone(), v.clone());
            }
            // TODO:
            // BoundNode::BindingIdent(_) |
            BoundNode::AssignPat(_) | BoundNode::RestPat(_) => {
                self.node_data_mut(node.clone()).flowNode = Some(self.currentFlow);
                self.bindBindingElement(node);
            }
            BoundNode::ClassProp(_)
            | BoundNode::PrivateProp(_)
            | BoundNode::TsPropertySignature(_) => {
                return self.bindPropertyWorker(node);
            }
            //TODO:
            // BoundNode::ShorthandPropertyAssignment(_) |
            BoundNode::KeyValueProp(ref prop) => {
                self.bindPropertyOrMethodOrAccessor(
                    node.clone(),
                    matches!(prop.key, ast::PropName::Computed(ref n) if hasDynamicName(&n.expr.bind_to_opt_parent(None))),
                    SymbolFlags::Property,
                    SymbolFlags::PropertyExcludes,
                );
            }
            BoundNode::TsEnumMember(_) => {
                self.bindPropertyOrMethodOrAccessor(
                    node,
                    false,
                    SymbolFlags::EnumMember,
                    SymbolFlags::EnumMemberExcludes,
                );
            }
            BoundNode::TsCallSignatureDecl(_)
            | BoundNode::TsConstructSignatureDecl(_)
            | BoundNode::TsIndexSignature(_) => {
                self.declareSymbolAndAddToSymbolTable(
                    node,
                    SymbolFlags::Signature,
                    SymbolFlags::None,
                );
            }
            BoundNode::PrivateMethod(ref m) => {
                let mut symbol_flags = SymbolFlags::Method;
                symbol_flags.set(SymbolFlags::Optional, m.is_optional);

                let symbol_excludes = match m.kind {
                    ast::MethodKind::Method => SymbolFlags::MethodExcludes,
                    ast::MethodKind::Getter => SymbolFlags::GetAccessorExcludes,
                    ast::MethodKind::Setter => SymbolFlags::SetAccessorExcludes,
                };

                self.bindPropertyOrMethodOrAccessor(
                    node.clone(),
                    false,
                    symbol_flags,
                    symbol_excludes,
                );
            }
            BoundNode::ClassMethod(ref m) => {
                let mut symbol_flags = SymbolFlags::Method;
                symbol_flags.set(SymbolFlags::Optional, m.is_optional);

                let symbol_excludes = match m.kind {
                    ast::MethodKind::Method => SymbolFlags::MethodExcludes,
                    ast::MethodKind::Getter => SymbolFlags::GetAccessorExcludes,
                    ast::MethodKind::Setter => SymbolFlags::SetAccessorExcludes,
                };

                self.bindPropertyOrMethodOrAccessor(
                    node.clone(),
                    matches!(m.key, ast::PropName::Computed(ref n) if hasDynamicName(&n.expr.bind_to_opt_parent(None))),
                    symbol_flags,
                    symbol_excludes,
                );
            }
            BoundNode::MethodProp(ref prop) => {
                // An ObjectLiteralExpression method sits in the same space as
                // other properties in the object literal. So we use SymbolFlags::PropertyExcludes
                // so that it will conflict with any other object literal
                // members with the same name.
                self.bindPropertyOrMethodOrAccessor(
                    node.clone(),
                    matches!(prop.key, ast::PropName::Computed(ref n) if hasDynamicName(&n.expr.bind_to_opt_parent(None))),
                    SymbolFlags::Method,
                    SymbolFlags::PropertyExcludes,
                );
            }
            BoundNode::TsMethodSignature(ref s) => {
                let mut symbol_flags = SymbolFlags::Method;
                symbol_flags.set(SymbolFlags::Optional, s.optional);

                self.bindPropertyOrMethodOrAccessor(
                    node.clone(),
                    hasDynamicName(&s.key.bind(node.clone())),
                    symbol_flags,
                    SymbolFlags::MethodExcludes,
                );
            }
            BoundNode::FnDecl(_) => {
                self.bindFunctionDeclaration(node);
            }
            BoundNode::Constructor(_) => {
                self.declareSymbolAndAddToSymbolTable(
                    node,
                    SymbolFlags::Constructor,
                    SymbolFlags::None,
                );
            }
            BoundNode::GetterProp(ref prop) => {
                self.bindPropertyOrMethodOrAccessor(
                    node.clone(),
                    matches!(prop.key, ast::PropName::Computed(ref n) if hasDynamicName(&n.expr.bind_to_opt_parent(None))),
                    SymbolFlags::GetAccessor,
                    SymbolFlags::GetAccessorExcludes,
                );
            }
            BoundNode::SetterProp(ref prop) => {
                self.bindPropertyOrMethodOrAccessor(
                    node.clone(),
                    matches!(prop.key, ast::PropName::Computed(ref n) if hasDynamicName(&n.expr.bind_to_opt_parent(None))),
                    SymbolFlags::SetAccessor,
                    SymbolFlags::SetAccessorExcludes,
                );
            }
            // TODO: jsdoc
            // BoundNode::JSDocFunctionType(_)|
            // BoundNode::JSDocSignature(_)|
            BoundNode::TsFnType(_) | BoundNode::TsConstructorType(_) => {
                // return self.bindFunctionOrConstructorType(node as SignatureDeclaration | JSDocSignature);
                return self.bindFunctionOrConstructorType(node);
            }
            // TODO: jsdoc
            // BoundNode::JSDocTypeLiteral(_)|
            BoundNode::TsTypeLit(_) | BoundNode::TsMappedType(_) => {
                return self.bindAnonymousTypeWorker(node);
            }
            // TODO: jsdoc
            // BoundNode::JSDocClassTag(_) => {
            //     return bindJSDocClassTag(node as JSDocClassTag);
            // }
            BoundNode::ObjectLit(_) => {
                self.bindObjectLiteralExpression(node);
            }
            BoundNode::FnExpr(ref f) => {
                return self
                    .bindFunctionExpression(node.clone(), f.ident.as_ref().map(|i| i.sym.clone()));
            }
            BoundNode::ArrowExpr(_) => {
                return self.bindFunctionExpression(node, None);
            }

            BoundNode::CallExpr(c) => {
                let assignmentKind = getAssignmentDeclarationKind(&ast::Expr::Call(c.node.clone()));
                match assignmentKind {
                    AssignmentDeclarationKind::ObjectDefinePropertyValue => {
                        todo!();
                        // return self.bindObjectDefinePropertyAssignment(
                        //     node as BindableObjectDefinePropertyCall,
                        // );
                    }
                    AssignmentDeclarationKind::ObjectDefinePropertyExports => {
                        todo!();
                        // return self.bindObjectDefinePropertyExport(
                        //     node as BindableObjectDefinePropertyCall,
                        // );
                    }
                    AssignmentDeclarationKind::ObjectDefinePrototypeProperty => {
                        todo!();
                        // return self.bindObjectDefinePrototypeProperty(
                        //     node as BindableObjectDefinePropertyCall,
                        // );
                    }
                    AssignmentDeclarationKind::None => {
                        // Nothing to do
                    }
                    _ => unreachable!("Unknown call expression assignment declaration kind"),
                }
                //todo:
                // if isInJSFile(node) {
                //     self.bindCallExpression(node.clone(), c.clone());
                // }
            }

            // Members of classes, interfaces, and modules
            BoundNode::ClassExpr(_) | BoundNode::ClassDecl(_) => {
                // All classes are automatically in strict mode in ES6.
                self.inStrictMode = true;
                return self.bindClassLikeDeclaration(node);
            }
            BoundNode::TsInterfaceDecl(_) => {
                return self.bindBlockScopedDeclaration(
                    node,
                    SymbolFlags::Interface,
                    SymbolFlags::InterfaceExcludes,
                );
            }
            BoundNode::TsTypeAliasDecl(_) => {
                return self.bindBlockScopedDeclaration(
                    node,
                    SymbolFlags::TypeAlias,
                    SymbolFlags::TypeAliasExcludes,
                );
            }
            BoundNode::TsEnumDecl(_) => {
                todo!();
                // return bindEnumDeclaration(node as EnumDeclaration);
            }
            BoundNode::TsModuleDecl(ref m) => {
                return self.bindModuleDeclaration(node.clone(), m.clone());
            }
            // // Jsx-attributes
            // BoundNode::JsxAttributes(_) => {
            //     return bindJsxAttributes(node as JsxAttributes);
            // }
            // BoundNode::JsxAttribute(_) => {
            //     return bindJsxAttribute(node as JsxAttribute, SymbolFlags::Property, SymbolFlags::PropertyExcludes);
            // }

            // // Imports and exports
            // BoundNode::ImportEqualsDeclaration(_) |
            // BoundNode::NamespaceImport(_) |
            // BoundNode::ImportSpecifier(_) |
            // BoundNode::ExportSpecifier(_) => {
            //     return declareSymbolAndAddToSymbolTable(node as Declaration, SymbolFlags::Alias, SymbolFlags::AliasExcludes);
            // }
            // BoundNode::NamespaceExportDeclaration(_) => {
            //     return bindNamespaceExportDeclaration(node as NamespaceExportDeclaration);
            // }
            // BoundNode::ImportClause(_) => {
            //     return bindImportClause(node as ImportClause);
            // }
            // BoundNode::ExportDeclaration(_) => {
            //     return bindExportDeclaration(node as ExportDeclaration);
            // }
            // BoundNode::ExportAssignment(_) => {
            //     return bindExportAssignment(node as ExportAssignment);
            // }
            BoundNode::Module(_) | BoundNode::Script(_) => {
                self.bindSourceFileIfExternalModule();
            }
            BoundNode::BlockStmt(_) => {
                // if (!isFunctionLikeOrClassStaticBlockDeclaration(node.parent)) {
                //     return;
                // }
                return;
            }
            BoundNode::TsModuleBlock(_) => {
                return;
            }

            // case SyntaxKind.JSDocParameterTag:
            //     if (node.parent.kind === SyntaxKind.JSDocSignature) {
            //         return bindParameter(node as JSDocParameterTag);
            //     }
            //     if (node.parent.kind !== SyntaxKind.JSDocTypeLiteral) {
            //         break;
            //     }
            //     // falls through
            // case SyntaxKind.JSDocPropertyTag:
            //     const propTag = node as JSDocPropertyLikeTag;
            //     const flags = propTag.isBracketed || propTag.typeExpression && propTag.typeExpression.type.kind === SyntaxKind.JSDocOptionalType ?
            //         SymbolFlags::Property | SymbolFlags::Optional :
            //         SymbolFlags::Property;
            //     return declareSymbolAndAddToSymbolTable(propTag, flags, SymbolFlags::PropertyExcludes);
            // case SyntaxKind.JSDocTypedefTag:
            // case SyntaxKind.JSDocCallbackTag:
            // case SyntaxKind.JSDocEnumTag:
            //     return (delayedTypeAliases || (delayedTypeAliases = [])).push(node as JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag);
            _ => {
                // println!("WARNING: bindWorker: Unknown node passed. Node: {:?}", node);
                // todo!();
            }
        }
    }

    fn bindPropertyWorker(&mut self, node: BoundNode) {
        let has_dynamic_name = hasDynamicName(&node);

        let optional = match &node {
            BoundNode::ClassProp(n) => n.is_optional,
            BoundNode::PrivateProp(n) => n.is_optional,
            BoundNode::TsPropertySignature(n) => n.optional,
            _ => unreachable!(),
        };

        let mut symbol_flags = SymbolFlags::Property;
        symbol_flags.set(SymbolFlags::Optional, optional);

        self.bindPropertyOrMethodOrAccessor(
            node,
            has_dynamic_name,
            symbol_flags,
            SymbolFlags::PropertyExcludes,
        );
    }

    fn bindAnonymousTypeWorker(&mut self, node: BoundNode) {
        debug_assert!(matches!(
            node,
            /*BoundNode::JSDocTypeLiteral(_)|*/
            BoundNode::TsTypeLit(_) | BoundNode::TsMappedType(_)
        ));
        self.bindAnonymousDeclaration(
            node,
            SymbolFlags::TypeLiteral,
            InternalSymbolName::Type.into(),
        );
    }

    fn bindSourceFileIfExternalModule(&mut self) {
        // todo:
        // setExportContextFlag(file);
        if let ast::Program::Module(_) = self.program() {
            self.bindSourceFileAsExternalModule();
        }
        // todo:
        // else if (isJsonSourceFile(file)) {
        //     bindSourceFileAsExternalModule();
        //     // Create symbol equivalent for the module.exports = {}
        //     const originalSymbol = file.symbol;
        //     declareSymbol(file.symbol.exports!, file.symbol, file, SymbolFlags.Property, SymbolFlags.All);
        //     file.symbol = originalSymbol;
        // }
    }

    fn bindSourceFileAsExternalModule(&mut self) {
        //TODO:
        // self.bindAnonymousDeclaration(file, SymbolFlags.ValueModule, `"${removeFileExtension(file.fileName)}"` as __String);
        self.bindAnonymousDeclaration(
            self.program().bind_to_opt_parent(None),
            SymbolFlags::ValueModule,
            "TODO_FILE_NAME".into(),
        );
    }

    // todo:
    // bindExportAssignment
    // bindNamespaceExportDeclaration
    // bindExportDeclaration
    // bindImportClause
    // setCommonJsModuleIndicator
    // bindObjectDefinePropertyExport
    // bindExportsPropertyAssignment
    // bindModuleExportsAssignment
    // bindExportAssignedObjectMemberAlias
    // bindThisPropertyAssignment
    // bindDynamicallyNamedThisPropertyAssignment
    // addLateBoundAssignmentDeclarationToSymbol
    // bindSpecialPropertyDeclaration
    // bindPrototypeAssignment

    // fn bindObjectDefinePrototypeProperty(&mut self, node: BindableObjectDefinePropertyCall) {
    //     todo!();
    //     // let namespaceSymbol = self.lookupSymbolForPropertyAccess(
    //     //     (node.arguments[0] as PropertyAccessExpression).expression as EntityNameExpression,
    //     // );
    //     // if namespaceSymbol && namespaceSymbol.valueDeclaration {
    //     //     // Ensure the namespace symbol becomes class-like
    //     //     self.addDeclarationToSymbol(
    //     //         namespaceSymbol,
    //     //         namespaceSymbol.valueDeclaration,
    //     //         SymbolFlags::Class,
    //     //     );
    //     // }
    //     // self.bindPotentiallyNewExpandoMemberToNamespace(
    //     //     node,
    //     //     namespaceSymbol,
    //     //     /*isPrototypeProperty*/ true,
    //     // );
    // }

    // bindPrototypePropertyAssignment
    // bindObjectDefinePropertyAssignment
    // bindSpecialPropertyAssignment
    // bindStaticPropertyAssignment
    // bindPotentiallyMissingNamespaces

    // fn bindPotentiallyNewExpandoMemberToNamespace(declaration: BindableStaticAccessExpression | CallExpression, namespaceSymbol: Option<SymbolId>, isPrototypeProperty: bool) {
    //     if !namespaceSymbol || !isExpandoSymbol(namespaceSymbol) {
    //         return;
    //     }

    //     // Set up the members collection if it doesn't exist already
    //     let symbolTable = if isPrototypeProperty {
    //         (namespaceSymbol.members || (namespaceSymbol.members = createSymbolTable()))}else{
    //         (namespaceSymbol.exports || (namespaceSymbol.exports = createSymbolTable()))};

    //     let mut includes = SymbolFlags::None;
    //     let mut excludes = SymbolFlags::None;
    //     // Method-like
    //     if isFunctionLikeDeclaration(getAssignedExpandoInitializer(declaration).unwrap()) {
    //         includes = SymbolFlags::Method;
    //         excludes = SymbolFlags::MethodExcludes;
    //     }
    //     // Maybe accessor-like
    //     else if (isCallExpression(declaration) && isBindableObjectDefinePropertyCall(declaration)) {
    //         // if (some(declaration.arguments[2].properties, p => {
    //         //     const id = getNameOfDeclaration(p);
    //         //     return !!id && isIdentifier(id) && idText(id) == "set";
    //         // })) {
    //         //     // We mix in `SymbolFlags::Property` so in the checker `getTypeOfVariableParameterOrProperty` is used for this
    //         //     // symbol, instead of `getTypeOfAccessor` (which will assert as there is no real accessor declaration)
    //         //     includes |= SymbolFlags::SetAccessor | SymbolFlags::Property;
    //         //     excludes |= SymbolFlags::SetAccessorExcludes;
    //         // }
    //         // if (some(declaration.arguments[2].properties, p => {
    //         //     const id = getNameOfDeclaration(p);
    //         //     return !!id && isIdentifier(id) && idText(id) == "get";
    //         // })) {
    //         //     includes |= SymbolFlags::GetAccessor | SymbolFlags::Property;
    //         //     excludes |= SymbolFlags::GetAccessorExcludes;
    //         // }
    //     }

    //     if includes == SymbolFlags::None {
    //         includes = SymbolFlags::Property;
    //         excludes = SymbolFlags::PropertyExcludes;
    //     }

    //     declareSymbol(symbolTable, namespaceSymbol, declaration, includes | SymbolFlags::Assignment, excludes & ~SymbolFlags::Assignment);
    // }

    // isTopLevelNamespaceAssignment
    // bindPropertyAssignment
    // isExpandoSymbol
    // getParentOfBinaryExpression
    // lookupSymbolForPropertyAccess
    // forEachIdentifierInEntityName
    // bindCallExpression

    fn bindClassLikeDeclaration(&mut self, node: BoundNode) {
        match &node {
            BoundNode::ClassDecl(_) => {
                self.bindBlockScopedDeclaration(
                    node.clone(),
                    SymbolFlags::Class,
                    SymbolFlags::ClassExcludes,
                );
            }
            BoundNode::ClassExpr(class) => {
                let bindingName = class
                    .ident
                    .as_ref()
                    .map(|i| i.sym.clone())
                    .unwrap_or_else(|| InternalSymbolName::Class.into());
                self.bindAnonymousDeclaration(node.clone(), SymbolFlags::Class, bindingName);
                // // Add name of class expression into the map for semantic classifier
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
            "prototype".into(),
        );
        let symbolExport = self.symbols[symbol].exports().unwrap();
        let symbolExport = self.symbol_tables[symbolExport].get(&"prototype".into());
        if let Some(_) = symbolExport {
            todo!();
            // if (node.name) {
            //     setParent(node.name, node);
            // }
            // file.bindDiagnostics.push(createDiagnosticForNode(symbolExport.declarations![0], Diagnostics.Duplicate_identifier_0, symbolName(prototypeSymbol)));
        }

        self.symbol_tables[self.symbols[symbol].exports_mut().unwrap()]
            .insert("prototype".into(), prototypeSymbol);

        *self.symbols[prototypeSymbol].parent_mut() = Some(symbol);
    }

    // bindEnumDeclaration

    fn bindVariableDeclaration(&mut self, node: BoundNode, decl: Rc<VarDeclarator>) {
        // if (inStrictMode) {
        //     checkStrictModeEvalOrArguments(node, node.name);
        // }

        // if !isBindingPattern(node.name) {
        if !matches!(decl.name, ast::Pat::Object(_) | ast::Pat::Array(_)) {
            let kind = match &decl.parent {
                Some(BoundNode::VarDecl(d)) => d.kind,
                _ => unreachable!(),
            };
            // todo
            /*if isInJSFile(decl) && isRequireVariableDeclaration(decl) && !getJSDocTypeTag(decl) {
                self.declareSymbolAndAddToSymbolTable(
                    node,
                    SymbolFlags::Alias,
                    SymbolFlags::AliasExcludes,
                );
            } else */
            if kind == ast::VarDeclKind::Let || kind == ast::VarDeclKind::Const {
                self.bindBlockScopedDeclaration(
                    node,
                    SymbolFlags::BlockScopedVariable,
                    SymbolFlags::BlockScopedVariableExcludes,
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

    fn bindBindingElement(&mut self, node: BoundNode) {
        debug_assert!(matches!(
            node,
            BoundNode::BindingIdent(_) | BoundNode::RestPat(_) | BoundNode::AssignPat(_)
        ));
        // if (inStrictMode) {
        //     checkStrictModeEvalOrArguments(node, node.name);
        // }

        let name = match &node {
            BoundNode::RestPat(n) => Some(&n.arg),
            BoundNode::AssignPat(n) => Some(&n.left),
            _ => None,
        };
        let is_name_binding_pat = matches!(name, Some(ast::Pat::Object(_) | ast::Pat::Array(_)));

        if !is_name_binding_pat {
            // TODO:
            // if isInJSFile(node) && isRequireVariableDeclaration(node) && !getJSDocTypeTag(node) {
            //     self.declareSymbolAndAddToSymbolTable(
            //         node as Declaration,
            //         SymbolFlags::Alias,
            //         SymbolFlags::AliasExcludes,
            //     );
            // } else
            if isBlockOrCatchScoped(node.clone()) {
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

    fn bindParameter(&mut self, param: Parameter /* | JSDocParameterTag*/) {
        // if (node.kind === SyntaxKind.JSDocParameterTag && container.kind !== SyntaxKind.JSDocSignature) {
        //     return;
        // }
        // if (inStrictMode && !(node.flags & NodeFlags.Ambient)) {
        //     // It is a SyntaxError if the identifier eval or arguments appears within a FormalParameterList of a
        //     // strict mode FunctionLikeDeclaration or FunctionExpression(13.1)
        //     checkStrictModeEvalOrArguments(node, node.name);
        // }

        match param.pat() {
            ast::Pat::Array(_) | ast::Pat::Object(_) => {
                let index_in_parent = match param.parent() {
                    BoundNode::PrivateMethod(n) => {
                        n.function.params.iter().position(|p| param == p)
                    }
                    BoundNode::ClassMethod(n) => n.function.params.iter().position(|p| param == p),
                    BoundNode::MethodProp(n) => n.function.params.iter().position(|p| param == p),
                    BoundNode::FnExpr(n) => n.function.params.iter().position(|p| param == p),
                    BoundNode::ArrowExpr(n) => n.params.iter().position(|p| param == p),
                    _ => todo!(),
                };
                let mut name = String::with_capacity(8);
                name.push_str("__");
                name.push_str(index_in_parent.unwrap().to_string().as_str());
                // TODO: remove clone():
                self.bindAnonymousDeclaration(
                    param.clone().into(),
                    SymbolFlags::FunctionScopedVariable,
                    name.into(),
                );
            }
            ast::Pat::Ident(_) | ast::Pat::Rest(_) | ast::Pat::Assign(_) => {
                // TODO: remove clone():
                self.declareSymbolAndAddToSymbolTable(
                    param.clone().into(),
                    SymbolFlags::FunctionScopedVariable,
                    SymbolFlags::ParameterExcludes,
                );
            }
            _ => unreachable!(),
        }

        // If this is a property-parameter, then also declare the property symbol into the
        // containing class.
        if let Parameter::TsParamProp(p) = param {
            let classDeclaration = p
                .parent
                .as_ref()
                .unwrap()
                .parent()
                .unwrap()
                .parent()
                .unwrap();
            debug_assert!(matches!(
                classDeclaration,
                BoundNode::ClassDecl(_) | BoundNode::ClassExpr(_)
            ));
            let class_sym = self.node_data(classDeclaration).symbol.unwrap();

            let is_optional = matches!(&p.param, ast::TsParamPropParam::Ident(i) if i.id.optional);
            let mut symbol_flags = SymbolFlags::Property;
            symbol_flags.set(SymbolFlags::Optional, is_optional);

            let bound = BoundNode::TsParamProp(p);

            declare_symbol!(
                self,
                self.symbols[class_sym].members().unwrap(),
                Some(class_sym),
                bound,
                symbol_flags,
                SymbolFlags::PropertyExcludes,
                false,
                false
            );
        }
    }

    fn bindFunctionDeclaration(&mut self, node: BoundNode) {
        //todo:
        // if (!file.isDeclarationFile && !(node.flags & NodeFlags.Ambient)) {
        //     if (isAsyncFunction(node)) {
        //         emitFlags |= NodeFlags.HasAsyncFunctions;
        //     }
        // }

        // checkStrictModeFunctionName(node);
        // if (inStrictMode) {
        //     checkStrictModeFunctionDeclaration(node);
        //     bindBlockScopedDeclaration(node, SymbolFlags.Function, SymbolFlags.FunctionExcludes);
        // } else {
        self.declareSymbolAndAddToSymbolTable(
            node,
            SymbolFlags::Function,
            SymbolFlags::FunctionExcludes,
        );
        // }
    }

    fn bindFunctionExpression(&mut self, node: BoundNode, name: Option<JsWord>) {
        // if (!file.isDeclarationFile && !(node.flags & NodeFlags.Ambient)) {
        //     if (isAsyncFunction(node)) {
        //         emitFlags |= NodeFlags.HasAsyncFunctions;
        //     }
        // }
        self.node_data_mut(node.clone()).flowNode = Some(self.currentFlow);
        // self.checkStrictModeFunctionName(node);
        let bindingName = name.unwrap_or_else(|| InternalSymbolName::Function.into());
        self.bindAnonymousDeclaration(node, SymbolFlags::Function, bindingName);
    }

    fn bindPropertyOrMethodOrAccessor(
        &mut self,
        node: BoundNode,
        has_dynamic_name: bool,
        symbolFlags: SymbolFlags,
        symbolExcludes: SymbolFlags,
    ) -> SymbolId {
        // TODO:
        // if !self.file.isDeclarationFile && !(node.flags & NodeFlags.Ambient) && isAsyncFunction(node) {
        //     emitFlags |= NodeFlags.HasAsyncFunctions;
        // }

        if isObjectLiteralOrClassExpressionMethodOrAccessor(&node) {
            self.node_data_mut(node.clone()).flowNode = Some(self.currentFlow);
        }

        if has_dynamic_name {
            self.bindAnonymousDeclaration(node, symbolFlags, InternalSymbolName::Computed.into())
        } else {
            self.declareSymbolAndAddToSymbolTable(node, symbolFlags, symbolExcludes)
        }
    }

    fn bindTypeParameter(&mut self, node: BoundNode, param: Rc<TsTypeParamDecl>) {
        // TODO: jsdoc
        // if (isJSDocTemplateTag(node.parent)) {
        //     const container = getEffectiveContainerForJSDocTemplateTag(node.parent);
        //     if (container) {
        //         if (!container.locals) {
        //             container.locals = createSymbolTable();
        //         }
        //         declareSymbol(container.locals, /*parent*/ undefined, node, SymbolFlags.TypeParameter, SymbolFlags.TypeParameterExcludes);
        //     }
        //     else {
        //         declareSymbolAndAddToSymbolTable(node, SymbolFlags.TypeParameter, SymbolFlags.TypeParameterExcludes);
        //     }
        // }
        // else
        if let parent @ BoundNode::TsInferType(_) = node.parent().unwrap() {
            if let Some(container) = getInferTypeContainer(parent) {
                let locals = match self.node_data(container.clone()).locals {
                    Some(locals) => locals,
                    None => {
                        let locals = self.symbol_tables.push(SymbolTable::default());
                        self.node_data_mut(container.clone()).locals = Some(locals);
                        locals
                    }
                };
                declare_symbol!(
                    self,
                    locals,
                    None,
                    node,
                    SymbolFlags::TypeParameter,
                    SymbolFlags::TypeParameterExcludes,
                    false,
                    false
                );
            } else {
                self.bindAnonymousDeclaration(
                    node.clone(),
                    SymbolFlags::TypeParameter,
                    self.getDeclarationName(node),
                );
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

    // fn shouldReportErrorOnModuleDeclaration(&mut self, node: ModuleDeclaration) -> bool {
    //     const instanceState = getModuleInstanceState(node);
    //     return instanceState === ModuleInstanceState::Instantiated || (instanceState === ModuleInstanceState::ConstEnumOnly && shouldPreserveConstEnums(options));
    // }

    fn checkUnreachable(&mut self, _node: &BoundNode) -> bool {
        if !self.flow_nodes[self.currentFlow]
            .flags
            .intersects(FlowFlags::Unreachable)
        {
            return false;
        }
        if self.currentFlow == self.unreachableFlow {
            todo!();
            // let reportError =
            //     // report error on all statements except empty ones
            //     (isStatementButNotDeclaration(node) && node.kind != SyntaxKind.EmptyStatement) ||
            //     // report error on class declarations
            //     node.kind == SyntaxKind.ClassDeclaration ||
            //     // report error on instantiated modules or const-enums only modules if preserveConstEnums is set
            //     (node.kind == SyntaxKind.ModuleDeclaration && shouldReportErrorOnModuleDeclaration(node as ModuleDeclaration));

            // if reportError {
            //     self.currentFlow = reportedUnreachableFlow;

            //     if !options.allowUnreachableCode {
            //         // unreachable code is reported if
            //         // - user has explicitly asked about it AND
            //         // - statement is in not ambient context (statements in ambient context is already an error
            //         //   so we should not report extras) AND
            //         //   - node is not variable statement OR
            //         //   - node is block scoped variable statement OR
            //         //   - node is not block scoped variable statement and at least one variable declaration has initializer
            //         //   Rationale: we don't want to report errors on non-initialized var's since they are hoisted
            //         //   On the other side we do want to report errors on non-initialized 'lets' because of TDZ
            //         let isError =
            //             unreachableCodeIsError(options) &&
            //             !(node.flags & NodeFlags.Ambient) &&
            //             (
            //                 !isVariableStatement(node) ||
            //                 !!(getCombinedNodeFlags(node.declarationList) & NodeFlags.BlockScoped) ||
            //                 node.declarationList.declarations.some(d => !!d.initializer)
            //             );

            //         eachUnreachableRange(node, (start, end) => errorOrSuggestionOnRange(isError, start, end, Diagnostics.Unreachable_code_detected));
            //     }
            // }
        }
        return true;
    }

    fn setExportContextFlag(
        &mut self,
        node: &Rc<TsModuleDecl>, /*node: Mutable<ModuleDeclaration | SourceFile>*/
    ) {
        let bound_node = BoundNode::TsModuleDecl(node.clone());
        // A declaration source file or ambient module declaration that contains no export declarations (but possibly regular
        // declarations with export modifiers) is an export context in which declarations are implicitly exported.
        if node.declare && !hasExportDeclarations(&bound_node) {
            self.node_data_mut(bound_node).flags |= NodeFlags::ExportContext;
        } else {
            self.node_data_mut(bound_node).flags &= !NodeFlags::ExportContext;
        }
    }
}

#[derive(PartialEq, Eq)]
pub enum ModuleInstanceState {
    NonInstantiated,
    Instantiated,
    ConstEnumOnly,
}

// pub fn getModuleInstanceState(node: ModuleDeclaration, visited?: ESMap<number, ModuleInstanceState | undefined>)->ModuleInstanceState {
pub fn getModuleInstanceState(node: BoundNode, module: Rc<TsModuleDecl>) -> ModuleInstanceState {
    if let Some(body) = &module.body {
        getModuleInstanceStateCached(body.bind(node) /*, visited*/)
    } else {
        ModuleInstanceState::Instantiated
    }
}

fn getModuleInstanceStateCached(
    node: BoundNode, /*, visited = new Map<number, ModuleInstanceState | undefined>()*/
) -> ModuleInstanceState {
    // const nodeId = getNodeId(node);
    // if (visited.has(nodeId)) {
    //     return visited.get(nodeId) || ModuleInstanceState::NonInstantiated;
    // }
    // visited.set(nodeId, undefined);
    let result = getModuleInstanceStateWorker(node /*, visited*/);
    // visited.set(nodeId, result);
    result
}

fn getModuleInstanceStateWorker(
    node: BoundNode, /*, visited: ESMap<number, ModuleInstanceState | undefined>*/
) -> ModuleInstanceState {
    // A module is uninstantiated if it contains only
    match node {
        // 1. interface declarations, type alias declarations
        BoundNode::TsInterfaceDecl(_) | BoundNode::TsTypeAliasDecl(_) => {
            return ModuleInstanceState::NonInstantiated;
        }
        // 2. const enum declarations
        BoundNode::TsEnumDecl(ref e) => {
            if e.is_const {
                return ModuleInstanceState::ConstEnumOnly;
            }
        }
        // TODO:
        // 3. non-exported import declarations
        // BoundNode::ImportDeclaration(_) | BoundNode::ImportEqualsDeclaration(_) => {
        //     if (!(hasSyntacticModifier(node, ModifierFlags.Export))) {
        //         return ModuleInstanceState::NonInstantiated;
        //     }
        // }
        // TODO:
        // // 4. Export alias declarations pointing at only uninstantiated modules or things uninstantiated modules contain
        // BoundNode::ExportDeclaration(_) => {
        //     let exportDeclaration = node as ExportDeclaration;
        //     if (!exportDeclaration.moduleSpecifier
        //         && exportDeclaration.exportClause
        //         && exportDeclaration.exportClause.kind == SyntaxKind.NamedExports)
        //     {
        //         let state = ModuleInstanceState::NonInstantiated;
        //         for specifier in exportDeclaration.exportClause.elements {
        //             let specifierState = getModuleInstanceStateForAliasTarget(specifier);
        //             if (specifierState > state) {
        //                 state = specifierState;
        //             }
        //             if (state == ModuleInstanceState::Instantiated) {
        //                 return state;
        //             }
        //         }
        //         return state;
        //     }
        // }
        // 5. other uninstantiated module declarations.
        BoundNode::TsModuleBlock(ref block) => {
            let mut state = ModuleInstanceState::NonInstantiated;
            for n in &block.body {
                let childState = getModuleInstanceStateCached(n.bind(node.clone()));
                match childState {
                    ModuleInstanceState::NonInstantiated => {
                        // child is non-instantiated - continue searching
                    }
                    ModuleInstanceState::ConstEnumOnly => {
                        // child is const enum only - record state and continue searching
                        state = ModuleInstanceState::ConstEnumOnly;
                    }
                    ModuleInstanceState::Instantiated => {
                        // child is instantiated - record state and stop
                        state = ModuleInstanceState::Instantiated;
                        break;
                    }
                }
            }
            return state;
        }
        BoundNode::TsModuleDecl(ref module) => {
            return getModuleInstanceState(node.clone(), module.clone());
        }
        // BoundNode::Ident(_) => {
        //     // Only jsdoc typedef definition can exist in jsdoc namespace, and it should
        //     // be considered the same as type alias
        //     if ((node as Identifier).isInJSDocNamespace) {
        //         return ModuleInstanceState::NonInstantiated;
        //     }
        // }
        _ => {}
    }
    ModuleInstanceState::Instantiated
}

// TODO:
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
//                     if (found === ModuleInstanceState::Instantiated) {
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
//     return ModuleInstanceState::Instantiated; // Couldn't locate, assume could refer to a value
// }

fn hasExportDeclarations(node: &BoundNode /*node: ModuleDeclaration | SourceFile*/) -> bool {
    false
    // TODO:
    // const body = isSourceFile(node) ? node : tryCast(node.body, isModuleBlock);
    // return !!body && body.statements.some(s => isExportDeclaration(s) || isExportAssignment(s));
}

fn getInferTypeContainer(mut node: BoundNode) -> Option<BoundNode> {
    // todo: returns TsConditionalType
    while let Some(parent) = node.parent() {
        if let BoundNode::TsConditionalType(ref cond_ty) = parent {
            if cond_ty.extends_type.bind(parent.clone()) == node {
                return Some(parent);
            }
        }
        node = parent;
    }
    None
}

// fn isStatementCondition(node: &BoundNode) -> bool {
//     if let Some(parent) = node.parent() {
//         match parent {
//             BoundNode::IfStmt(ref s) => s.test.bind(parent.clone()) == *node,
//             BoundNode::WhileStmt(ref s) => s.test.bind(parent.clone()) == *node,
//             BoundNode::DoWhileStmt(ref s) => s.test.bind(parent.clone()) == *node,
//             BoundNode::ForStmt(ref s) => match &s.test {
//                 Some(test) => test.bind(parent.clone()) == *node,
//                 None => false,
//             },
//             BoundNode::CondExpr(ref s) => s.test.bind(parent.clone()) == *node,
//             _ => false,
//         }
//     } else {
//         false
//     }
// }

// fn isLogicalExpression(mut node: BoundNode) -> bool {
//     loop {
//         match node {
//             BoundNode::ParenExpr(ref expr) => {
//                 node = expr.expr.bind(node.clone());
//             }
//             BoundNode::UnaryExpr(ref expr) if expr.op == ast::UnaryOp::Bang => {
//                 node = expr.arg.bind(node.clone());
//             }
//             BoundNode::BinExpr(expr)
//                 if matches!(
//                     expr.op,
//                     ast::BinaryOp::LogicalAnd
//                         | ast::BinaryOp::LogicalOr
//                         | ast::BinaryOp::NullishCoalescing
//                 ) =>
//             {
//                 return true
//             }
//             _ => return false,
//         }
//     }
// }

fn isLogicalAssignmentExpression(node: BoundNode) -> bool {
    let node = skipParenthesesOfNode(node);
    if let BoundNode::AssignExpr(expr) = node {
        isLogicalOrCoalescingAssignmentOperator(expr.op)
    } else {
        false
    }
}

// fn isTopLevelLogicalExpression(mut node: BoundNode) -> bool {
//     loop {
//         match node.parent() {
//             Some(parent @ BoundNode::ParenExpr(_)) => node = parent,
//             Some(ref parent @ BoundNode::UnaryExpr(ref e)) if e.op == ast::UnaryOp::Bang => {
//                 node = parent.clone()
//             }
//             _ => break,
//         }
//     }

//     return !isStatementCondition(&node)
//         && !node
//             .parent()
//             .map(|parent| isLogicalAssignmentExpression(parent))
//             .unwrap_or_default()
//         && !node
//             .parent()
//             .map(|parent| isLogicalExpression(parent))
//             .unwrap_or_default()
//         && !(if let Some(ref parent_node @ BoundNode::OptChainExpr(ref opt)) = node.parent() {
//             opt.expr.bind_to_opt_parent(parent_node.parent()) == node
//         } else {
//             false
//         });
// }

fn isNarrowingExpression(expr: &ast::Expr) -> bool {
    match expr {
        ast::Expr::Ident(_)
        | ast::Expr::PrivateName(_)
        | ast::Expr::This(_)
        | ast::Expr::Member(_) => containsNarrowableReference(&expr.clone().into()),
        ast::Expr::Call(e) => hasNarrowableArgument(e.as_ref()),
        ast::Expr::Paren(e) => isNarrowingExpression(&e.expr.clone().into()),
        ast::Expr::TsNonNull(e) => isNarrowingExpression(&e.expr.clone().into()),
        ast::Expr::Bin(e) => isNarrowingBinaryExpression(e.as_ref()),
        ast::Expr::Unary(e) if e.op == ast::UnaryOp::Bang || e.op == ast::UnaryOp::TypeOf => {
            isNarrowingExpression(&e.arg)
        }
        _ => false,
    }
}

fn isNarrowableReference(expr: &Node) -> bool {
    if isDottedName(expr) {
        return true;
    }

    match expr {
        Node::TsNonNullExpr(e) => isNarrowableReference(&e.expr.clone().into()),
        Node::ParenExpr(e) => isNarrowableReference(&e.expr.clone().into()),
        Node::SeqExpr(e) => e
            .exprs
            .last()
            .map(|e| isNarrowableReference(&e.clone().into()))
            .unwrap_or_default(),
        Node::MemberExpr(e) if !e.computed || isStringOrNumericLiteralLike(&e.prop) => {
            isNarrowableReference(&e.obj.clone().into())
        }
        Node::AssignExpr(e) => match &e.left {
            ast::PatOrExpr::Expr(e) => isNarrowableReference(&e.clone().into()),
            ast::PatOrExpr::Pat(_) => false,
        },
        _ => false,
    }
}

fn containsNarrowableReference(expr: &Node) -> bool {
    if isNarrowableReference(expr) {
        return true;
    }

    todo!();
    // if isOptionalChain(expr) && containsNarrowableReference(expr.expression) {
    //     return true;
    // }

    // false
}

fn hasNarrowableArgument(expr: &ast::CallExpr) -> bool {
    for arg in &expr.args {
        if let ast::ExprOrSpread::Expr(e) = arg {
            if containsNarrowableReference(&e.clone().into()) {
                return true;
            }
        }
    }
    if let ast::ExprOrSuper::Expr(ast::Expr::Member(member_expr)) = &expr.callee {
        if containsNarrowableReference(&member_expr.obj.clone().into()) {
            return true;
        }
    }
    false
}

fn isNarrowingTypeofOperands(_expr1: &ast::Expr, _expr2: &ast::Expr) -> bool {
    todo!();
    // return isTypeOfExpression(expr1) && isNarrowableOperand(expr1.expression) && isStringLiteralLike(expr2);
}

fn isNarrowingBinaryExpression(_expr: &ast::BinExpr) -> bool {
    todo!();
    // switch (expr.operatorToken.kind) {
    //     case SyntaxKind.EqualsToken:
    //     case SyntaxKind.BarBarEqualsToken:
    //     case SyntaxKind.AmpersandAmpersandEqualsToken:
    //     case SyntaxKind.QuestionQuestionEqualsToken:
    //         return containsNarrowableReference(expr.left);
    //     case SyntaxKind.EqualsEqualsToken:
    //     case SyntaxKind.ExclamationEqualsToken:
    //     case SyntaxKind.EqualsEqualsEqualsToken:
    //     case SyntaxKind.ExclamationEqualsEqualsToken:
    //         return isNarrowableOperand(expr.left) || isNarrowableOperand(expr.right) ||
    //             isNarrowingTypeofOperands(expr.right, expr.left) || isNarrowingTypeofOperands(expr.left, expr.right);
    //     case SyntaxKind.InstanceOfKeyword:
    //         return isNarrowableOperand(expr.left);
    //     case SyntaxKind.InKeyword:
    //         return isNarrowingExpression(expr.right);
    //     case SyntaxKind.CommaToken:
    //         return isNarrowingExpression(expr.right);
    // }
    // return false;
}

fn isNarrowableOperand(expr: &Node) -> bool {
    match expr {
        Node::ParenExpr(e) => isNarrowableOperand(&e.expr.clone().into()),
        Node::AssignExpr(e) if e.op == ast::AssignOp::Assign => {
            isNarrowableOperand(&e.left.clone().into())
        }
        Node::SeqExpr(e) => {
            todo!();
            // isNarrowableOperand(Node::from(e.right))
        }
        _ => containsNarrowableReference(expr),
    }
}

// TODO: pub only for testing
pub struct ActiveLabel {
    name: JsWord,
    breakTarget: FlowNodeId,
    continueTarget: Option<FlowNodeId>,
    referenced: bool,
}

bitflags! {
    struct ContainerFlags: u32 {
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
