use crate::control_flow_analysis::ControlFlowAnalysis;
use crate::control_flow_graph::{ControlFlowGraph, DummyAnnotation, Node};
use crate::ctx::Ctx;
use crate::typed_scope::ScopeId;
use crate::typed_scope_creator::TypedScopeCreator;
use ast::*;
use ecma_visit::{noop_visit_type, Visit, VisitAstNodeWith, VisitWith};

pub struct TypeInferencePass;

impl TypeInferencePass {
    /**
     * Execute type inference running over part of the scope tree.
     *
     * @return the top scope, either newly created, or patched by this inference.
     */
    pub fn inferAllScopes<'tcx, 'ast, 's>(
        &mut self,
        ctx: &'tcx mut Ctx<'tcx>,
        scopeCreator: &mut TypedScopeCreator<'tcx>,
        inferenceRoot: AstNode<'tcx>,
    ) -> ScopeId {
        // Type analysis happens in two major phases.
        // 1) Finding all the symbols.
        // 2) Propagating all the inferred types.
        //
        // The order of this analysis is non-obvious. In a complete inference
        // system, we may need to backtrack arbitrarily far. But the compile-time
        // costs would be unacceptable.
        //
        // We do one pass where we do typed scope creation for all scopes
        // in pre-order.
        //
        // Then we do a second pass where we do all type inference
        // (type propagation) in pre-order.
        //
        // We use a memoized scope creator so that we never create a scope
        // more than once.
        //
        // This will allow us to handle cases like:
        // var ns = {};
        // (function() { /** JSDoc */ ns.method = function() {}; })();
        // ns.method();
        // In this code, we need to build the symbol table for the inner scope in
        // order to propagate the type of ns.method in the outer scope.
        // try (JSTypeResolver.Closer closer = this.registry.getResolver().openForDefinition()) {
        // checkState(inferenceRoot.isRoot());
        // checkState(inferenceRoot.getParent() == null);
        // checkState(this.topScope == null);
        let topScope = scopeCreator.createScope(ctx,inferenceRoot, None);

        // let mut first = V::new(scopeCreator, |_| {});

        // first.traverseWithScope(inferenceRoot, topScope);

        // NodeTraversal.builder()
        //     .setCompiler(compiler)
        //     .setCallback(new FirstScopeBuildingCallback())
        //     .setScopeCreator(scopeCreator)
        //     .traverseWithScope(inferenceRoot, self.topScope);
        // self.scopeCreator.resolveWeakImportsPreResolution();
        //   }
        //   self.scopeCreator.undoTypeAliasChains();

        //   NodeTraversal.builder()
        //       .setCompiler(compiler)
        //       .setCallback(new SecondScopeBuildingCallback())
        //       .setScopeCreator(scopeCreator)
        //       .traverseWithScope(inferenceRoot, this.topScope);

        //   // Normalize TypedVars to have the '?' type instead of null after inference is complete. This
        //   // currently cannot be done any earlier because it breaks inference of variables assigned in
        //   // local scopes.
        //   // TODO(b/149843534): this should be a crash instead.
        //   final JSType unknownType = this.registry.getNativeType(UNKNOWN_TYPE);
        //   for (TypedVar var : this.scopeCreator.getAllSymbols()) {
        //     if (var.getType() == null) {
        //       var.setType(unknownType);
        //     }
        //   }

        //   if (this.stepCountHistogram != null) {
        //     try (LogFile histogram =
        //         this.compiler.createOrReopenLog(this.getClass(), "step_histogram.log")) {
        //       histogram.log("step_count token population");

        //       int[] totals = new int[] {0, 0};
        //       this.stepCountHistogram.keySet().stream()
        //           .sorted(Comparator.<Integer>naturalOrder().reversed())
        //           .forEach(
        //               (stepCount) ->
        //                   this.stepCountHistogram.get(stepCount).entrySet().stream()
        //                       .sorted(comparingInt(Multiset.Entry::getCount))
        //                       .forEach(
        //                           (e) -> {
        //                             totals[0] += stepCount * e.getCount();
        //                             totals[1] += e.getCount();
        //                             histogram.log("%s %s %s", stepCount, e.getElement(), e.getCount());
        //                           }));
        //       histogram.log("%s TOTAL %s", totals[0], totals[1]);
        //     }
        //   }

        topScope
    }

    fn compute_cfg(n: Node) -> ControlFlowGraph<'_, DummyAnnotation, DummyAnnotation> {
        let mut cfa = ControlFlowAnalysis::new(n, false);
        cfa.process();
        cfa.cfg()
    }
}

struct V<'tcx,'a, F>
where
    F: FnMut(ScopeId),
{
    scope_callback: F,
    scope_creator: &'tcx mut TypedScopeCreator<'tcx>,

    ctx: &'a mut Ctx<'tcx>,

    /// Stack containing the Scopes that have been created. The Scope objects are lazily created; so
    /// the {@code scopeRoots} stack contains the Nodes for all Scopes that have not been created yet.
    scopes: Vec<ScopeId>,
}

impl<'tcx,'a, F> V<'tcx,'a, F>
where
    F: FnMut(ScopeId),
{
    pub fn new(ctx: &'a mut Ctx<'tcx>,scope_creator: &'tcx mut TypedScopeCreator<'tcx>, on_enter_scope: F) -> Self {
        Self {
            scope_callback: on_enter_scope,
            scope_creator,

            ctx,

            scopes: Vec::new(),
        }
    }
    /**
     * Traverses a parse tree recursively with a scope, starting with the given root. This should only
     * be used in the global scope or module scopes. Otherwise, use {@link #traverseAtScope}.
     */
    fn traverseWithScope(&mut self, root: AstNode<'tcx>, s: ScopeId) {
        //TODO:
        // debug_assert!(s.isGlobal() || s.isModuleScope());

        self.push_scope(s);
        root.visit_with(self);
        self.pop_scope();
    }

    /// Creates a new scope (e.g. when entering a function).
    fn create_scope(&mut self, node: AstNode<'tcx>) {
        let scope = self
            .scope_creator
            .createScope(self.ctx,node, self.scopes.last().map(|s| *s));
        self.scopes.push(scope);

        (self.scope_callback)(scope);
    }

    /// Push an existing scope onto the stack.
    fn push_scope(&mut self, s: ScopeId) {
        self.scopes.push(s);

        (self.scope_callback)(s);
    }

    /// Pops back to the previous scope (e.g. when leaving a function).
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}

macro_rules! visit_children_with_scope {
    ($visitor:expr, $node:expr) => {{
        $visitor.create_scope(AstNode::from($node));
        $node.visit_children_with($visitor);
        $visitor.pop_scope();
    }};
}

// TODO: ignore nodes that can't contain scopes.
impl<'tcx,'a, F> Visit<'tcx> for V<'tcx, 'a,F>
where
    F: FnMut(ScopeId),
{
    noop_visit_type!();

    fn visit_script(&mut self, n: &'tcx Script) {
        n.visit_children_with(self);
    }
    fn visit_function(&mut self, n: &'tcx Function) {
        visit_children_with_scope!(self, n);
    }
    fn visit_module(&mut self, n: &'tcx Module) {
        visit_children_with_scope!(self, n);
    }
    /**
     * Traverses a class. Note that we traverse some of the child nodes slightly out of order to
     * ensure children are visited in the correct scope. The following children are in the outer
     * scope: (1) the 'extends' clause, (2) any computed method keys, (3) the class name for class
     * declarations only (class expression names are traversed in the class scope). This requires that
     * we visit the extends node (second child) and any computed member keys (grandchildren of the
     * last, body, child) before visiting the name (first child) or body (last child).
     */
    fn visit_class(&mut self, n: &'tcx Class) {
        n.super_class.visit_with(self);

        // Visit keys of computed props.
        for member in &n.body {
            // TODO: Do all static members need to be handled here as well?
            // Also, normal class props?
            match member {
                // TODO: why does constructor even have a PropName? And can it
                // really be a computed prop?
                ClassMember::Constructor(Constructor {
                    key: PropName::Computed(key),
                    ..
                })
                | ClassMember::Method(ClassMethod {
                    key: PropName::Computed(key),
                    ..
                }) => {
                    key.expr.visit_with(self);
                }
                ClassMember::ClassProp(_) => {
                    todo!();
                }
                _ => {}
            }
        }

        self.create_scope(AstNode::Class(n));

        // Traverse class members, excluding keys of computed props.
        for member in &n.body {
            match member {
                ClassMember::Constructor(n) => {
                    // Don't visit computed key.
                    if !matches!(n.key, PropName::Computed(_)) {
                        n.key.visit_with(self);
                    }

                    // TODO: treat as function
                }
                ClassMember::Method(n) => {
                    // Don't visit computed key.
                    if !matches!(n.key, PropName::Computed(_)) {
                        n.key.visit_with(self);
                    }

                    n.function.visit_with(self);
                }
                ClassMember::PrivateMethod(n) => n.visit_with(self),
                ClassMember::ClassProp(_) => todo!(),
                ClassMember::PrivateProp(n) => n.visit_with(self),
                ClassMember::TsIndexSignature(_) | ClassMember::Empty(_) => {}
            }
        }

        self.pop_scope();
    }

    fn visit_catch_clause(&mut self, n: &'tcx CatchClause) {
        visit_children_with_scope!(self, n);
    }

    fn visit_block_stmt(&mut self, n: &'tcx BlockStmt) {
        visit_children_with_scope!(self, n);
    }
    fn visit_for_stmt(&mut self, n: &'tcx ForStmt) {
        visit_children_with_scope!(self, n);
    }
    fn visit_for_in_stmt(&mut self, n: &'tcx ForInStmt) {
        visit_children_with_scope!(self, n);
    }
    fn visit_for_of_stmt(&mut self, n: &'tcx ForOfStmt) {
        visit_children_with_scope!(self, n);
    }
    fn visit_switch_stmt(&mut self, n: &'tcx SwitchStmt) {
        visit_children_with_scope!(self, n);
    }
}
