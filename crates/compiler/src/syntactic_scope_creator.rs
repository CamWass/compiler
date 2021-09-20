use ast::*;
use fxhash::{FxHashMap, FxHashSet};
use swc_atoms::JsWord;

/**
 * The syntactic scope creator scans the parse tree to create a Scope object containing all the
 * variable declarations in that scope. This class adds support for block-level scopes introduced in
 * ECMAScript 6.
 *
 * <p>This implementation is not thread-safe.
 */
pub struct SyntacticScopeCreator {}

impl SyntacticScopeCreator {
    pub fn createScope<'tcx, 's>(
        &mut self,
        node: AstNode<'tcx>,
        is_function_block: bool,
        parent: Option<&'s SyntacticScope<'tcx>>,
    ) -> SyntacticScope<'tcx> {
        let mut scope = SyntacticScope::new(N {
            node,
            is_function_block,
        });
        ScopeScanner::new(&mut scope, parent).populate();
        scope
    }
}

pub struct N<'tcx> {
    pub node: AstNode<'tcx>,
    pub is_function_block: bool,
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum DeclarationType {
    VAR,
    LET,
    CONST,
    FUNCTION,
    CLASS,
    CATCH,
    IMPORT,
    PARAM_LIST,
}

pub struct Var<'tcx>(pub &'tcx Ident, pub DeclarationType);

pub struct SyntacticScope<'tcx> {
    pub root: N<'tcx>,
    pub names: FxHashMap<JsWord, Var<'tcx>>,
}

impl<'tcx> SyntacticScope<'tcx> {
    pub fn new(root: N<'tcx>) -> Self {
        Self {
            root,
            names: Default::default(),
        }
    }

    pub fn is_function_block_scope(&self) -> bool {
        self.root.is_function_block
    }
}

struct ScopeScanner<'tcx, 'a> {
    scope: &'a mut SyntacticScope<'tcx>,
    parent_scope: Option<&'a SyntacticScope<'tcx>>,
}

impl<'tcx, 'a> ScopeScanner<'tcx, 'a> {
    pub fn new(
        scope: &'a mut SyntacticScope<'tcx>,
        parent_scope: Option<&'a SyntacticScope<'tcx>>,
    ) -> Self {
        Self {
            scope,
            parent_scope,
        }
    }

    pub fn is_global_scope(&self) -> bool {
        self.parent_scope.is_none()
    }

    fn handle_pat(&mut self, pat: &'tcx Pat, decl_type: DeclarationType) {
        match pat {
            Pat::Ident(ref p) => self.declareVar(&p.id, decl_type),
            Pat::Array(p) => {
                for el in &p.elems {
                    if let Some(el) = el {
                        self.handle_pat(el, decl_type);
                    }
                }
            }
            Pat::Rest(p) => self.handle_pat(&*p.arg, decl_type),
            Pat::Object(p) => {
                for prop in &p.props {
                    match prop {
                        ObjectPatProp::KeyValue(kv) => self.handle_pat(&*kv.value, decl_type),
                        ObjectPatProp::Assign(p) => self.declareVar(&p.key, decl_type),
                        ObjectPatProp::Rest(p) => self.handle_pat(&*p.arg, decl_type),
                    }
                }
            }
            Pat::Assign(p) => self.handle_pat(&*p.left, decl_type),
            Pat::Invalid(_) | Pat::Expr(_) => {}
        }
    }

    pub fn populate(&mut self) {
        match self.scope.root.node {
            AstNode::FnDecl(n) => {
                // Args: Declare function variables
                // checkState(args.isParamList());
                for param in &n.function.params {
                    self.handle_pat(&param.pat, DeclarationType::PARAM_LIST);
                }

                // Since we create a separate scope for body, stop scanning here
            }
            AstNode::FnExpr(n) => {
                // Args: Declare function variables
                for param in &n.function.params {
                    self.handle_pat(&param.pat, DeclarationType::PARAM_LIST);
                }

                // Bleed the function name into the scope, if it hasn't been declared in the outer scope
                // and the name isn't already in the scope via the param list.
                if let Some(name) = &n.ident {
                    self.declareVar(name, DeclarationType::FUNCTION);
                }

                // Since we create a separate scope for body, stop scanning here
            }
            AstNode::ClassDecl(_) => todo!(),
            AstNode::ClassExpr(n) => {
                // Bleed the class name into the scope, if it hasn't
                // been declared in the outer scope.
                if let Some(name) = &n.ident {
                    self.declareVar(name, DeclarationType::CLASS);
                }
            }
            AstNode::Script(n) => {
                // TODO: Handle root node here as well.

                // n is the global scope
                // checkState(scope.isGlobal(), scope);
                self.scanVars(self.scope.root.node, true, true);
            }
            AstNode::Module(n) => {
                self.scanVars(self.scope.root.node, true, true);
            }
            AstNode::ForStmt(_)
            | AstNode::ForOfStmt(_)
            | AstNode::ForInStmt(_)
            | AstNode::SwitchStmt(_) => {
                self.scanVars(self.scope.root.node, false, true);
            }
            AstNode::BlockStmt(_) => {
                if self.scope.root.is_function_block {
                    self.scanVars(self.scope.root.node, true, true);
                } else {
                    self.scanVars(self.scope.root.node, false, true);
                }
            }
            _ => unreachable!("Illegal scope root: {:?}", self.scope.root.node),
        }
    }

    /**
     * Scans and gather variables declarations under a Node
     *
     * @param n The node
     * @param hoistScope The scope that is the hoist target for vars, if we are scanning for vars.
     * @param blockScope The scope that is the hoist target for block-level declarations, if we are
     *     scanning for block level declarations.
     */
    fn scanVars(&mut self, n: AstNode<'tcx>, hoistScope: bool, blockScope: bool) {
        match n {
            AstNode::VarDecl(decl) => {
                match decl.kind {
                    VarDeclKind::Var => {
                        if hoistScope {
                            for decl in &decl.decls {
                                self.handle_pat(&decl.name, DeclarationType::VAR);
                            }
                        }
                        return;
                    }
                    VarDeclKind::Let => {
                        // Only declare when scope is the current lexical scope
                        if blockScope {
                            for decl in &decl.decls {
                                self.handle_pat(&decl.name, DeclarationType::LET);
                            }
                        }
                        return;
                    }
                    VarDeclKind::Const => {
                        // Only declare when scope is the current lexical scope
                        if blockScope {
                            for decl in &decl.decls {
                                self.handle_pat(&decl.name, DeclarationType::CONST);
                            }
                        }
                        return;
                    }
                }
            }

            AstNode::ImportDecl(decl) => {
                for specifier in &decl.specifiers {
                    match specifier {
                        ImportSpecifier::Named(ImportNamedSpecifier { local, .. })
                        | ImportSpecifier::Default(ImportDefaultSpecifier { local, .. })
                        | ImportSpecifier::Namespace(ImportStarAsSpecifier { local, .. }) => {
                            self.declareVar(local, DeclarationType::IMPORT)
                        }
                    }
                }
                return;
            }

            AstNode::ExportDecl(d) => {
                // The first child of an EXPORT can be a declaration, in the case of
                // export var/let/const/function/class name ...
                self.scanVars(AstNode::from(&d.decl), hoistScope, blockScope);
                return;
            }

            AstNode::FnDecl(n) => {
                if !blockScope {
                    return;
                }

                self.declareVar(&n.ident, DeclarationType::FUNCTION);
                // should not examine function's children
                return;
            }
            AstNode::ClassDecl(n) => {
                if !blockScope {
                    return;
                }

                self.declareVar(&n.ident, DeclarationType::CLASS);
                // should not examine class's children
                return;
            }
            AstNode::CatchClause(n) => {
                if blockScope {
                    if let Some(param) = &n.param {
                        self.handle_pat(param, DeclarationType::CATCH)
                    }
                }
                // TODO: this is wrong: we don't have a "block above the catch".
                // A new scope is not created for this BLOCK because there is a scope
                // created for the BLOCK above the CATCH
                self.scanVars(AstNode::BlockStmt(&n.body), hoistScope, blockScope);
                // only one child to scan
                return;
            }
            AstNode::Script(n) => {
                // if (changeRootSet != null && !changeRootSet.contains(n)) {
                //   // If there is a changeRootSet configured, that means
                //   // a partial update is being done and we should skip
                //   // any SCRIPT that aren't being asked for.
                //   return;
                // }
                // inputId = n.getInputId();
                // break;
            }
            AstNode::Module(n) => {
                // Module bodies are not part of global scope, but may declare an implicit goog namespace.
                if self.is_global_scope() {
                    //   Node expr = n.getFirstChild();
                    //   if (expr != null && isLegacyGoogModule(expr)) {
                    //     declareImplicitGoogNamespaceFromCall(hoistScope, expr);
                    //   }
                    return;
                }
                // break;
            }

            _ => {}
        }

        let isBlockStart = blockScope && n == self.scope.root.node;
        let enteringNewBlock = !isBlockStart && createsBlockScope(n);
        if enteringNewBlock && !hoistScope {
            // We only enter new blocks when scanning for hoisted vars
            return;
        }

        let blockScope = if enteringNewBlock { false } else { blockScope };

        // Variables can only occur in statement-level nodes, so
        // we only need to traverse children in a couple special cases.
        // TODO: some of these cases are not reachable.
        // TODO: root node
        match n {
            AstNode::Script(Script { body: stmts, .. })
            | AstNode::BlockStmt(BlockStmt { stmts, .. })
            | AstNode::SwitchCase(SwitchCase { cons: stmts, .. }) => {
                for s in stmts {
                    self.scanVars(s.into(), hoistScope, blockScope);
                }
            }
            AstNode::Module(n) => {
                for i in &n.body {
                    self.scanVars(i.into(), hoistScope, blockScope);
                }
            }
            AstNode::ForStmt(ForStmt { body, .. })
            | AstNode::ForInStmt(ForInStmt { body, .. })
            | AstNode::ForOfStmt(ForOfStmt { body, .. })
            | AstNode::DoWhileStmt(DoWhileStmt { body, .. })
            | AstNode::WhileStmt(WhileStmt { body, .. })
            | AstNode::WithStmt(WithStmt { body, .. })
            | AstNode::LabeledStmt(LabeledStmt { body, .. }) => {
                self.scanVars(AstNode::from(&**body), hoistScope, blockScope);
            }
            AstNode::IfStmt(n) => {
                self.scanVars(AstNode::from(&*n.cons), hoistScope, blockScope);
                if let Some(alt) = &n.alt {
                    self.scanVars(AstNode::from(&**alt), hoistScope, blockScope);
                }
            }
            AstNode::TryStmt(n) => {
                self.scanVars(AstNode::BlockStmt(&n.block), hoistScope, blockScope);
                if let Some(finalizer) = &n.finalizer {
                    self.scanVars(AstNode::BlockStmt(finalizer), hoistScope, blockScope);
                }
                if let Some(handler) = &n.handler {
                    self.scanVars(AstNode::CatchClause(handler), hoistScope, blockScope);
                }
            }
            AstNode::SwitchStmt(n) => {
                for c in &n.cases {
                    self.scanVars(AstNode::SwitchCase(c), hoistScope, blockScope);
                }
            }
            _ => {}
        }
    }

    /**
     * Declares a variable.
     *
     * @param s The scope to declare the variable in.
     * @param n The node corresponding to the variable name.
     */
    fn declareVar(&mut self, n: &'tcx Ident, decl_type: DeclarationType) {
        // Because of how we scan the variables, it is possible to encounter
        // the same var declared name node twice. Bail out in this case.
        // TODO(johnlenz): Hash lookups are not free and building scopes are already expensive.
        // Restructure the scope building to avoid this check.
        if let Some(n1) = self.scope.names.get(&n.sym) {
            if n1.0 == n {
                return;
            }
        }

        // CompilerInput input = compiler.getInput(inputId);
        if self.scope.names.get(&n.sym).is_some() || !self.isShadowingAllowed(&n.sym)
        // || ((s.isFunctionScope() || s.isFunctionBlockScope()) && name.equals(ARGUMENTS))
        {
            //   self.redeclarationHandler.onRedeclaration(s, name, n, input);
        } else {
            self.scope.names.insert(n.sym.clone(), Var(n, decl_type));
            //   s.declare(name, n, input);
        }
    }

    // Function body declarations are not allowed to shadow
    // function parameters.
    fn isShadowingAllowed(&self, name: &JsWord) -> bool {
        if self.scope.is_function_block_scope() {
            let maybeParam = self.parent_scope.and_then(|s| s.names.get(&name));
            return maybeParam.is_none() || maybeParam.unwrap().1 != DeclarationType::PARAM_LIST;
        }
        true
    }
}

/// Returns whether the node creates a block scope.
fn createsBlockScope(n: AstNode) -> bool {
    matches!(
        n,
        AstNode::BlockStmt(_)
            | AstNode::ForStmt(_)
            | AstNode::ForInStmt(_)
            | AstNode::ForOfStmt(_)
            | AstNode::SwitchStmt(_)
            | AstNode::Class(_)
    )
}
