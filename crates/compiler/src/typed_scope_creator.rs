use std::{cell::RefCell, marker::PhantomData};

use ast::*;
use ecma_visit::{noop_visit_type, Visit, VisitAstNodeWith, VisitWith};
use fxhash::{FxHashMap, FxHashSet};
use swc_atoms::JsWord;

use crate::{
    ctx::Ctx,
    syntactic_scope_creator::{SyntacticScope, SyntacticScopeCreator},
    typed_scope::{RootNode, ScopeId, TypedScope},
};

pub struct TypedScopeCreator<'tcx> {
    memoized: FxHashMap<AstNode<'tcx>, ScopeId>,
    /// Untyped scopes which contain unqualified names. Populated by FirstOrderFunctionAnalyzer to
    /// reserve names before the TypedScope is populated.
    untyped_scopes: FxHashMap<AstNode<'tcx>, FxHashSet<JsWord>>,
}

impl<'tcx> TypedScopeCreator<'tcx> {
    pub fn new() -> Self {
        Self {
            memoized: FxHashMap::default(),
            untyped_scopes: Default::default(),
        }
    }

    /**
     * Creates a scope with all types declared. Declares newly discovered types and type properties in
     * the type registry.
     */
    pub fn createScope<'a>(
        &mut self,
        ctx: &'a mut Ctx<'tcx>,
        root: AstNode<'tcx>,
        parent: Option<ScopeId>,
    ) -> ScopeId {
        if self.memoized.get(&root).is_none() {
            let scope = self.createScopeInternal(ctx, root, parent);
            self.memoized.insert(root, scope);
        }

        *self.memoized.get(&root).unwrap()
        // *self.memoized.entry(root).or_insert_with(|| {
        //     let scope = self.createScopeInternal(root, parent);
        //     scope
        // })

        // match self.memoized.get(&root) {
        //     Some(scope) => {
        //         debug_assert!(parent == scope.get_parent());
        //         scope
        //     }
        //     None => {
        //         let scope = self.createScopeInternal(root, parent);
        //         self.memoized.insert(root, scope);
        //         &scope
        //     }
        // }
    }

    fn createScopeInternal<'a>(
        &self,
        ctx: &'a mut Ctx<'tcx>,
        root: AstNode,
        typedParent: Option<ScopeId>,
    ) -> ScopeId {
        // todo!();

        // Constructing the global scope is very different than constructing
        // inner scopes, because only global scopes can contain named classes that
        // show up in the type registry.
        // let mut newScope = None;

        // AbstractScopeBuilder scopeBuilder = null;

        // Module module = ModuleImportResolver.getModuleFromScopeRoot(moduleMap, compiler, root);
        let newScope = if let Some(typedParent) = typedParent {
            todo!();
        //     // Because JSTypeRegistry#getType looks up the scope in which a root of a qualified name is
        //     // declared, pre-populate this TypedScope with all qualified name roots. This prevents
        //     // type resolution from accidentally returning a type from an outer scope that is shadowed.
        //     let untypedScope = self.untypedScopes.get(root);
        //     let reservedNames = untypedScope
        //         .getAllSymbols()
        //         .map(|symbol| symbol.getName())
        //         .collect::<FxHashSet<_>>();
        //     if module != null && module.metadata().isGoogModule() {
        //         // TypedScopeCreator treats default export assignments, like `exports = class {};`, as
        //         // declarations. However, the untyped scope only contains an implicit slot for `exports`.
        //         reservedNames.add("exports");
        //     } else if root.isFunction() && NodeUtil.isBundledGoogModuleCall(root.getParent()) {
        //         // Pretend that 'exports' is declared in the block of goog.loadModule
        //         // functions, not the function scope. See the above comment for why.
        //         reservedNames.remove("exports");
        //     }

        //     TypedScope::new(typedParent, root, reservedNames, module)
        } else {
            todo!();
            //     //   checkState(root.isRoot(), root);
            //     //   Node externsRoot = root.getFirstChild();
            //     //   Node jsRoot = root.getSecondChild();
            //     //   checkState(externsRoot.isRoot(), externsRoot);
            //     //   checkState(jsRoot.isRoot(), jsRoot);
            //     //   let globalThis = self.typeRegistry.getNativeObjectType(JSTypeNative.GLOBAL_THIS);

            //     // Mark the main root, the externs root, and the src root
            //     // with the global this type.
            //     //   root.setJSType(globalThis);
            //     //   externsRoot.setJSType(globalThis);
            //     //   jsRoot.setJSType(globalThis);
            //     // ctx.type_registry.set_type(root, ctx.type_registry.common_types.GLOBAL_THIS);

            //     // Find all the classes in the global scope.
            //     self.createInitialScope(ctx, root)
        };

        if let AstNode::Function(_) = root {
            // FunctionScopeBuilder::new(newScope).build();
        } else if let AstNode::Class(_) = root {
            // ClassScopeBuilder::new(newScope).build();
        } else {
            NormalScopeBuilder::new(newScope, &mut self, ctx).build(ctx);
        };

        // if typedParent.is_none() {
        //   List<NominalTypeBuilder> delegateProxies = new ArrayList<>();
        //   for (FunctionType delegateProxyCtor : delegateProxyCtors) {
        //     delegateProxies.add(
        //         new NominalTypeBuilder(delegateProxyCtor, delegateProxyCtor.getInstanceType()));
        //   }
        //   codingConvention.defineDelegateProxyPrototypeProperties(
        //       typeRegistry, delegateProxies, delegateCallingConventions);
        // }
        // if module != null && module.metadata().isEs6Module() {
        //   // Declare an implicit variable representing the namespace of this module, then add a property
        //   // for each exported name to that variable's type.
        //   ObjectType namespaceType = typeRegistry.createAnonymousObjectType(null);
        //   newScope.declare(
        //       Export.NAMESPACE,
        //       root, // Use the given MODULE_BODY as the 'declaration node' for lack of a better option.
        //       namespaceType,
        //       compiler.getInput(NodeUtil.getInputId(root)),
        //       /* inferred= */ false);

        //   // Store the module object type on the MODULE_BODY.
        //   // The Es6RewriteModules will retrieve it from there for use when it creates a global for
        //   // the module object.
        //   root.setJSType(namespaceType);
        //   moduleImportResolver.updateEsModuleNamespaceType(namespaceType, module, newScope);
        // }

        todo!();

        // newScope
    }

    /**
     * Create the outermost scope. This scope contains native binding such as {@code Object}, {@code
     * Date}, etc.
     *
     * @param root The global ROOT node
     */
    fn createInitialScope(&mut self, ctx: &mut Ctx, root: RootNode<'tcx>) -> TypedScope {
        //  checkArgument(root.isRoot(), root);

        // Gather global information used in typed scope creation. Use a memoized scope creator because
        // scope-building takes a nontrivial amount of time.
        //  MemoizedScopeCreator scopeCreator =
        //      new MemoizedScopeCreator(new SyntacticScopeCreator(compiler));

        let mut scope_creator = SyntacticScopeCreator {};

        let mut visitor = FirstOrderFunctionAnalyzer::new(&mut scope_creator, |scope| {
            self.untyped_scopes.insert(
                scope.root.node,
                scope
                    .names
                    .keys()
                    .map(|sym| sym.clone())
                    .collect::<FxHashSet<_>>(),
            );
        });

        visitor.traverseRoots(root.node);

        //  NodeTraversal.builder()
        //      .setCompiler(compiler)
        //      .setCallback(new FirstOrderFunctionAnalyzer())
        //      .setScopeCreator(scopeCreator)
        //      .traverseRoots(root.getFirstChild(), root.getLastChild());

        //  NodeTraversal.builder()
        //      .setCompiler(compiler)
        //      .setCallback(new IdentifyEnumsAndTypedefsAsNonNullable(typeRegistry, codingConvention))
        //      .setScopeCreator(scopeCreator)
        //      .traverse(root);

        let id = ctx.scopes.next_index();

        let s = TypedScope::createGlobalScope(id, root);
        //  declareNativeFunctionType(s, ARRAY_FUNCTION_TYPE);
        //  declareNativeFunctionType(s, BIGINT_OBJECT_FUNCTION_TYPE);
        //  declareNativeFunctionType(s, BOOLEAN_OBJECT_FUNCTION_TYPE);
        //  declareNativeFunctionType(s, DATE_FUNCTION_TYPE);
        //  declareNativeFunctionType(s, FUNCTION_FUNCTION_TYPE);
        //  declareNativeFunctionType(s, GENERATOR_FUNCTION_TYPE);
        //  declareNativeFunctionType(s, ITERABLE_FUNCTION_TYPE);
        //  declareNativeFunctionType(s, ITERATOR_FUNCTION_TYPE);
        //  declareNativeFunctionType(s, NUMBER_OBJECT_FUNCTION_TYPE);
        //  declareNativeFunctionType(s, OBJECT_FUNCTION_TYPE);
        //  declareNativeFunctionType(s, REGEXP_FUNCTION_TYPE);
        //  declareNativeFunctionType(s, STRING_OBJECT_FUNCTION_TYPE);
        //  declareNativeValueType(s, "undefined", VOID_TYPE);

        //  gatherAllProvides(root);

        // Memoize the global scope so that module scope creation can access it. See
        // AbstractScopeBuilder#shouldTraverse - modules are traversed early, as if they were always
        // executed when control flow reaches the module body.
        //  memoized.put(root, s);

        s
    }
}

/** A shallow traversal of the global scope to build up all classes, functions, and methods. */
struct NormalScopeBuilder<'tcx, 'a, 'c> {
    inner: AbstractScopeBuilder<'tcx, 'a, 'c>,
}

impl<'tcx, 'a, 'c> NormalScopeBuilder<'tcx, 'a, 'c> {
    pub fn new(
        scope: TypedScope<'tcx>,
        scope_creator: &'a mut TypedScopeCreator<'tcx>,
        ctx: &'c mut Ctx<'tcx>,
    ) -> Self {
        Self {
            inner: AbstractScopeBuilder::new(scope, scope_creator, ctx),
        }
    }

    pub fn build(self, ctx: &'c Ctx<'tcx>) -> TypedScope<'tcx> {
        let scope_builder: RefCell<NormalScopeBuilder<'tcx, 'a, 'c>> = RefCell::new(self);

        self.inner.build(
            ctx,
            |current_scope_root, node, parent| self.visitPreorder(current_scope_root, node, parent),
            |current_scope_root, node, parent| {
                self.visitPostorder(current_scope_root, node, parent)
            },
        )
    }

    fn visitPreorder(
        &mut self,
        current_scope_root: Option<AstNode<'tcx>>,
        node: AstNode<'tcx>,
        parent: Option<AstNode<'tcx>>,
    ) {
        todo!();
        //   // Create any child block scopes "pre-order" as we see them.
        //   //
        //   // This is required because hoisted or qualified names defined in earlier blocks might be
        //   // referred to later outside the block. This isn't a big deal in most cases since a NamedType
        //   // will be created and resolved later, but if a NamedType is used for a superclass, we lose a
        //   // lot of valuable checking. Recursing into child blocks immediately prevents this from being
        //   // a problem.
        //   //
        //   // We don't traverse into CLASSes because we haven't yet have created the class-type on which
        //   // to assign members. We'll do this on the way back up (post-order) instead, after the
        //   // class-type has been attached to the AST.
        //   if (parent != null && NodeUtil.createsBlockScope(n) && !n.isClass()) {
        //     createScope(n, currentScope);
        //   }

        //   // All other functions (and classes, etc) are handled when we see the actual function node.
        //   if (n.isFunction()) {
        //     defineFunctionLiteral(n);
        //   }
    }

    fn visitPostorder(
        &mut self,
        current_scope_root: Option<AstNode<'tcx>>,
        node: AstNode<'tcx>,
        parent: AstNode<'tcx>,
    ) {
        todo!();
        //   switch (n.getToken()) {
        //     case CALL:
        //       checkForClassDefiningCalls(n);
        //       break;

        //     case ASSIGN:
        //       // Handle initialization of properties.
        //       // We only allow qualified name declarations of the form
        //       //   /** @type {number} */ a.b.c = rhs;
        //       // TODO(b/77597706): Ensure that CheckJSDoc warns for JSDoc on assignments not to
        //       // qualified names, e.g.
        //       //   /** @type {number} */ [a.b.c] = someArr;
        //       Node firstChild = n.getFirstChild();
        //       if (firstChild.isGetProp() && firstChild.isQualifiedName()) {
        //         maybeDeclareQualifiedName(t, n.getJSDocInfo(), firstChild, n, firstChild.getNext());
        //       } else if (undeclaredNamesForClosure.contains(firstChild)) {
        //         defineAssignAsIfDeclaration(n);
        //       }
        //       break;

        //     case CATCH:
        //       defineCatch(n);
        //       break;

        //     case VAR:
        //     case LET:
        //     case CONST:
        //       defineVars(n);
        //       break;

        //     case GETPROP:
        //       codingConvention.checkForCallingConventionDefinitions(n, delegateCallingConventions);
        //       // Handle stubbed properties.
        //       if (parent.isExprResult() && n.isQualifiedName()) {
        //         maybeDeclareQualifiedName(t, n.getJSDocInfo(), n, parent, null);
        //       }
        //       break;

        //     case CLASS:
        //       // Analyse CLASS child-scopes now because later code in this scope may assign
        //       // properties to these class-types. We want to ensure declarations within the CLASS have
        //       // priority.
        //       createScope(n, currentScope);
        //       break;

        //     case EXPR_RESULT:
        //       Collection<ProvidedName> names = providedNamesFromCall.get(n);
        //       if (names != null) {
        //         for (ProvidedName name : names) {
        //           declareProvidedNs(n, name);
        //         }
        //       }
        //       break;

        //     case EXPORT:
        //       if (n.getBooleanProp(Node.EXPORT_DEFAULT)) {
        //         // Define a dummy var for "export default <someExpr>" so that other utilities have
        //         // access to the type.
        //         JSType declaredType = n.getOnlyChild().getJSType();
        //         new SlotDefiner()
        //             .inScope(currentScope)
        //             .forDeclarationNode(n)
        //             .withType(declaredType)
        //             .forVariableName(Export.DEFAULT_EXPORT_NAME)
        //             .allowLaterTypeInference(declaredType == null)
        //             .defineSlot();
        //       }
        //       break;

        //     default:
        //       break;
        //   }
    }
}

struct AbstractScopeBuilder<'tcx, 'a, 'c>
// where
//     Pre: FnMut(Option<AstNode<'tcx>>, AstNode<'tcx>, Option<AstNode<'tcx>>) -> bool,
//     Post: FnMut(Option<AstNode<'tcx>>, AstNode<'tcx>, AstNode<'tcx>),
{
    /// The scope that we're building.
    currentScope: TypedScope<'tcx>,

    /// The current hoist scope.
    currentHoistScope: ScopeId,

    scope_creator: &'a mut TypedScopeCreator<'tcx>,

    ctx: &'c mut Ctx<'tcx>,
    // pre_order_visitor: Pre,
    // post_order_visitor: Post,
}

impl<'tcx, 'a, 'c> AbstractScopeBuilder<'tcx, 'a, 'c>
// where
//     Pre: FnMut(Option<AstNode<'tcx>>, AstNode<'tcx>, Option<AstNode<'tcx>>) -> bool,
//     Post: FnMut(Option<AstNode<'tcx>>, AstNode<'tcx>, AstNode<'tcx>),
{
    pub fn new(
        scope: TypedScope<'tcx>,
        scope_creator: &'a mut TypedScopeCreator<'tcx>,
        ctx: &'c mut Ctx<'tcx>,
        // pre_order_visitor: Pre,
        // post_order_visitor: Post,
    ) -> Self {
        Self {
            // TODO: try to remove unwrap()
            currentHoistScope: scope.getClosestHoistScope(ctx).unwrap(),
            currentScope: scope,
            scope_creator,
            ctx,
            // pre_order_visitor,
            // post_order_visitor,
        }
    }

    /** Traverse the scope root and build it. */
    fn build<Pre, Post>(
        self,
        ctx: &'c Ctx<'tcx>,
        mut pre_order_visitor: Pre,
        mut post_order_visitor: Post,
    ) -> TypedScope<'tcx>
    where
        Pre: FnMut(Option<AstNode<'tcx>>, AstNode<'tcx>, Option<AstNode<'tcx>>),
        Post: FnMut(Option<AstNode<'tcx>>, AstNode<'tcx>, AstNode<'tcx>),
    {
        // initializeModuleScope(currentScope.getRootNode());

        let scope_builder: RefCell<AbstractScopeBuilder<'tcx, 'a, 'c>> = RefCell::new(self);

        let mut visitor: AbstractScopeBuilderVisitor<'tcx, 'c, _, _> =
            AbstractScopeBuilderVisitor::new(
                ctx,
                |current_scope_root, node, parent| {
                    // self.should_traverse(current_scope_root, node, parent)
                    scope_builder.borrow_mut().should_traverse(
                        current_scope_root,
                        node,
                        parent,
                        &mut pre_order_visitor,
                    )
                },
                |current_scope_root, node, parent| {
                    // self.visit(current_scope_root, node, parent)
                    scope_builder.borrow_mut().visit(
                        current_scope_root,
                        node,
                        parent,
                        &mut post_order_visitor,
                    )
                },
            );

        visitor.traverseAtScope(&scope_builder.borrow().currentScope);
        // visitor.traverseAtScope(&self.currentScope);

        scope_builder.into_inner().currentScope
    }

    fn should_traverse<Pre>(
        &mut self,
        current_scope_root: Option<AstNode<'tcx>>,
        node: AstNode<'tcx>,
        parent: Option<AstNode<'tcx>>,
        mut pre_order_visitor: &mut Pre,
    ) -> bool
    where
        Pre: FnMut(Option<AstNode<'tcx>>, AstNode<'tcx>, Option<AstNode<'tcx>>),
    {
        if parent.is_none() || self.inCurrentScope(current_scope_root) {
            pre_order_visitor(current_scope_root, node, parent);
            return true;
        } else if let AstNode::Module(_) = node {
            // Visit modules pre-order. While this doesn't exactly match execution semantics, it
            // does match how the compiler rewrites modules into the global scope.

            self.scope_creator
                .createScope(self.ctx, node, Some(self.currentScope.id()));
        }
        false
    }

    fn inCurrentScope(&self, scope_root: Option<AstNode>) -> bool {
        scope_root == Some(self.currentScope.getRootNode().node)
    }

    fn visit<Post>(
        &mut self,
        current_scope_root: Option<AstNode<'tcx>>,
        node: AstNode<'tcx>,
        parent: Option<AstNode<'tcx>>,
        mut post_order_visitor: &mut Post,
    ) where
        Post: FnMut(Option<AstNode<'tcx>>, AstNode<'tcx>, AstNode<'tcx>),
    {
        if let Some(parent) = parent {
            self.attachLiteralTypes(node);
            post_order_visitor(current_scope_root, node, parent);
            // if deferredActions.containsKey(n) { // streams are expensive, only make if needed
            //   deferredActions.removeAll(n).forEach(Runnable::run);
            // }
        } /*  else if (!deferredActions.isEmpty()) {
            // Run *all* remaining deferred actions, in case any were missed.
            deferredActions.values().forEach(Runnable::run);
          }*/
    }

    fn attachLiteralTypes(&mut self, n: AstNode) {
        // switch (n.getToken()) {
        //   case NULL:
        //     n.setJSType(getNativeType(NULL_TYPE));
        //     break;

        //   case VOID:
        //     n.setJSType(getNativeType(VOID_TYPE));
        //     break;

        //   case STRINGLIT:
        //   case TEMPLATELIT_STRING:
        //     n.setJSType(getNativeType(STRING_TYPE));
        //     break;

        //   case NUMBER:
        //     n.setJSType(getNativeType(NUMBER_TYPE));
        //     break;

        //   case BIGINT:
        //     n.setJSType(getNativeType(BIGINT_TYPE));
        //     break;

        //   case TRUE:
        //   case FALSE:
        //     n.setJSType(getNativeType(BOOLEAN_TYPE));
        //     break;

        //   case REGEXP:
        //     n.setJSType(getNativeType(REGEXP_TYPE));
        //     break;

        //   case OBJECTLIT:
        //     JSDocInfo info = n.getJSDocInfo();
        //     if (info != null && info.hasLendsName()) {
        //       // Defer analyzing object literals with a @lends annotation until we
        //       // reach the root of the statement they're defined in.
        //       //
        //       // This ensures that if there are any @lends annotations on the object
        //       // literals, the type on the @lends annotation resolves correctly.
        //       //
        //       // For more information, see http://blickly.github.io/closure-compiler-issues/#314
        //       deferredActions.put(NodeUtil.getEnclosingStatement(n), () -> defineObjectLiteral(n));
        //     } else {
        //       defineObjectLiteral(n);
        //     }
        //     break;

        //   case CLASS:
        //     // NOTE(sdh): We can't handle function nodes here because they need special behavior to
        //     // deal with hoisting.  But since classes aren't hoisted, and may need to be handled in
        //     // such places as default method initializers (i.e. in a FunctionScope) or class extends
        //     // clauses (technically part of the ClassScope, but visited instead by the NormalScope),
        //     // they can be handled consistently in all scopes.
        //     defineClassLiteral(n);
        //     break;

        //     // NOTE(johnlenz): If we ever support Array tuples,
        //     // we will need to handle them here as we do object literals
        //     // above.
        //   case ARRAYLIT:
        //     n.setJSType(getNativeType(ARRAY_TYPE));
        //     break;
        //   default:
        //     break;
        // }
    }

    //   private void defineObjectLiteral(Node objectLit) {
    //     // Handle the @lends annotation.
    //     JSType type = null;
    //     JSDocInfo info = objectLit.getJSDocInfo();
    //     if (info != null && info.hasLendsName()) {
    //       String lendsName = info.getLendsName().getRoot().getString();
    //       TypedVar lendsVar = currentScope.getVar(lendsName);
    //       if (lendsVar == null) {
    //         report(JSError.make(objectLit, UNKNOWN_LENDS, lendsName));
    //       } else {
    //         type = lendsVar.getType();
    //         if (type == null) {
    //           type = unknownType;
    //         }
    //         if (!type.isSubtypeOf(typeRegistry.getNativeType(OBJECT_TYPE))) {
    //           report(JSError.make(objectLit, LENDS_ON_NON_OBJECT, lendsName, type.toString()));
    //           type = null;
    //         } else {
    //           objectLit.setJSType(type);
    //         }
    //       }
    //     }

    //     info = NodeUtil.getBestJSDocInfo(objectLit);
    //     boolean createEnumType = info != null && info.hasEnumParameterType();
    //     if (createEnumType) {
    //       Node lValue = NodeUtil.getBestLValue(objectLit);
    //       String lValueName = NodeUtil.getBestLValueName(lValue);
    //       type = createEnumTypeFromNodes(objectLit, lValueName, lValue, info);
    //     }

    //     if (type == null) {
    //       type = typeRegistry.createAnonymousObjectType(info);
    //     }

    //     setDeferredType(objectLit, type);

    //     // If this is an enum, the properties were already taken care of above.
    //     processObjectLitProperties(
    //         objectLit, ObjectType.cast(objectLit.getJSType()), !createEnumType);
    //   }

    //   /**
    //    * Process an object literal and all the types on it.
    //    *
    //    * @param objLit The OBJECTLIT node.
    //    * @param objLitType The type of the OBJECTLIT node. This might be a named type, because of the
    //    *     lends annotation.
    //    * @param declareOnOwner If true, declare properties on the objLitType as well. If false, the
    //    *     caller should take care of this.
    //    */
    //   void processObjectLitProperties(Node objLit, ObjectType objLitType, boolean declareOnOwner) {
    //     for (Node keyNode = objLit.getFirstChild(); keyNode != null; keyNode = keyNode.getNext()) {
    //       if (keyNode.isComputedProp() || keyNode.isSpread()) {
    //         // Don't try defining computed or spread properties on an object. Note that for spread
    //         // type inference will try to determine the properties and types. We cannot do it here as
    //         // we don't have all the type information of the spread object.
    //         continue;
    //       }
    //       Node value = keyNode.getFirstChild();
    //       String memberName = NodeUtil.getObjectOrClassLitKeyName(keyNode);
    //       JSDocInfo info = keyNode.getJSDocInfo();
    //       JSType valueType = getDeclaredType(info, keyNode, value, null);
    //       JSType keyType =
    //           objLitType.isEnumType()
    //               ? objLitType.toMaybeEnumType().getElementsType()
    //               : TypeCheck.getObjectLitKeyTypeFromValueType(keyNode, valueType);

    //       // Try to declare this property in the current scope if it
    //       // has an authoritative name.
    //       String qualifiedName = NodeUtil.getBestLValueName(keyNode);
    //       if (qualifiedName != null) {
    //         new SlotDefiner()
    //             .forDeclarationNode(keyNode)
    //             .forVariableName(qualifiedName)
    //             .inScope(getLValueRootScope(keyNode))
    //             .withType(keyType)
    //             .allowLaterTypeInference(keyType == null)
    //             .defineSlot();
    //       } else if (keyType != null) {
    //         setDeferredType(keyNode, keyType);
    //       }

    //       if (keyType != null && objLitType != null && declareOnOwner) {
    //         // Declare this property on its object literal.
    //         objLitType.defineDeclaredProperty(memberName, keyType, keyNode);
    //       }
    //     }
    //   }

    //   /**
    //    * Returns the type specified in a JSDoc annotation near a GETPROP, NAME, member function, or
    //    * object literal key.
    //    *
    //    * <p>Extracts type information from the {@code @type} tag.
    //    */
    //   private JSType getDeclaredTypeInAnnotation(Node node, JSDocInfo info) {
    //     checkArgument(info.hasType());

    //     ImmutableList<TemplateType> ownerTypeKeys = ImmutableList.of();
    //     Node ownerNode = NodeUtil.getBestLValueOwner(node);
    //     String ownerName = NodeUtil.getBestLValueName(ownerNode);
    //     ObjectType ownerType = null;
    //     if (ownerName != null) {
    //       TypedVar ownerVar = currentScope.getVar(ownerName);
    //       if (ownerVar != null) {
    //         ownerType = getPrototypeOwnerType(ObjectType.cast(ownerVar.getType()));
    //         if (ownerType != null) {
    //           ownerTypeKeys = ownerType.getTemplateTypeMap().getTemplateKeys();
    //         }
    //       }
    //     }

    //     StaticTypedScope templateScope =
    //         !ownerTypeKeys.isEmpty()
    //             ? typeRegistry.createScopeWithTemplates(currentScope, ownerTypeKeys)
    //             : currentScope;
    //     return info.getType().evaluate(templateScope, typeRegistry);
    //   }

    //   /**
    //    * Asserts that it's OK to define this node's name. The node should have a source name and be of
    //    * the specified type.
    //    */
    //   void assertDefinitionNode(Node n, Token type) {
    //     checkState(sourceName != null);
    //     checkState(n.getToken() == type, n);
    //   }

    //   /** Defines a catch parameter. */
    //   void defineCatch(Node n) {
    //     assertDefinitionNode(n, Token.CATCH);
    //     // Though almost certainly a terrible idea, it is possible to do destructuring in
    //     // the catch declaration.
    //     // e.g. `} catch ({message, errno}) {`
    //     for (Node catchName : NodeUtil.findLhsNodesInNode(n)) {
    //       JSType type = getDeclaredType(catchName.getJSDocInfo(), catchName, null, null);
    //       new SlotDefiner()
    //           .forDeclarationNode(catchName)
    //           .forVariableName(catchName.getString())
    //           .inScope(currentScope)
    //           .withType(type)
    //           .allowLaterTypeInference(type == null)
    //           .defineSlot();
    //     }
    //   }

    //   /** Defines an assignment to a name as if it were an actual declaration. */
    //   void defineAssignAsIfDeclaration(Node assignment) {
    //     JSDocInfo info = assignment.getJSDocInfo();
    //     Node name = assignment.getFirstChild();
    //     checkArgument(name.isName(), name);
    //     Node rvalue = assignment.getSecondChild();
    //     defineName(name, rvalue, currentScope, info);
    //   }

    //   /** Defines a variable declared with `var`, `let`, or `const`. */
    //   void defineVars(Node n) {
    //     checkState(sourceName != null);
    //     checkState(NodeUtil.isNameDeclaration(n));
    //     JSDocInfo info = n.getJSDocInfo();
    //     // `var` declarations are hoisted, but `let` and `const` are not.
    //     TypedScope scope = n.isVar() ? currentHoistScope : currentScope;

    //     if (n.hasMoreThanOneChild() && info != null) {
    //       report(JSError.make(n, MULTIPLE_VAR_DEF));
    //     }

    //     for (Node child = n.getFirstChild(); child != null; child = child.getNext()) {
    //       defineVarChild(info, child, scope);
    //     }
    //     if (n.hasOneChild() && isValidTypedefDeclaration(n.getOnlyChild(), n.getJSDocInfo())) {
    //       declareTypedefType(n.getOnlyChild(), n.getJSDocInfo());
    //     }
    //   }

    //   /** Defines a variable declared with `var`, `let`, or `const`. */
    //   void defineVarChild(JSDocInfo declarationInfo, Node child, TypedScope scope) {
    //     if (child.isName()) {
    //       if (declarationInfo == null) {
    //         declarationInfo = child.getJSDocInfo();
    //         // TODO(bradfordcsmith): Report an error if both the declaration node and the name itself
    //         //     have JSDoc.
    //       }
    //       defineName(child, child.getFirstChild(), scope, declarationInfo);
    //     } else {
    //       checkState(child.isDestructuringLhs(), child);
    //       Node pattern = child.getFirstChild();
    //       Node value = child.getSecondChild();

    //       if (ModuleImportResolver.isGoogModuleDependencyCall(value)) {
    //         // Define destructuring names here, since goog.require destructuring patterns can only
    //         // have one level and require some special handling.
    //         ScopedName defaultImport = moduleImportResolver.getClosureNamespaceTypeFromCall(value);
    //         for (Node key = pattern.getFirstChild(); key != null; key = key.getNext()) {
    //           defineModuleImport(key.getFirstChild(), defaultImport, key.getString(), scope);
    //         }
    //         return;
    //       }

    //       defineDestructuringPatternInVarDeclaration(
    //           pattern,
    //           scope,
    //           () ->
    //               // Note that value will be null if we are in an enhanced for loop
    //               //   for (const {x, y} of data) {
    //               value != null
    //                   ? new RValueInfo(
    //                       getDeclaredRValueType(/* lValue= */ null, value),
    //                       value.getQualifiedNameObject())
    //                   : new RValueInfo(unknownType, /* qualifiedName= */ null));
    //     }
    //   }

    //   /**
    //    * Returns information about the qualified name and type of the target, if it exists.
    //    *
    //    * <p>Never returns null, but will return an RValueInfo with null `type` and `qualifiedName`
    //    * slots.
    //    */
    //   private RValueInfo inferTypeForDestructuredTarget(
    //       DestructuredTarget target, Supplier<RValueInfo> patternTypeSupplier) {
    //     // Currently we only do type inference for string key nodes in object patterns here, to
    //     // handle aliasing types. e.g
    //     //   const {Foo} = ns;
    //     // TypeInference takes care of the rest.
    //     // Note that although DestructuredTarget includes logic for inferring types, we don't use
    //     // it here because we only do some very limited type inference during TypedScopeCreation,
    //     // and only return a non-null type here if we are accessing a declared property on a known
    //     // type.
    //     if (!target.hasStringKey() || target.hasDefaultValue()) {
    //       return RValueInfo.empty();
    //     }
    //     RValueInfo rvalue = patternTypeSupplier.get();
    //     JSType patternType = rvalue.type;
    //     String propertyName = target.getStringKey().getString();
    //     QualifiedName qname =
    //         rvalue.qualifiedName != null ? rvalue.qualifiedName.getprop(propertyName) : null;
    //     if (patternType == null || patternType.isUnknownType()) {
    //       return new RValueInfo(null, qname);
    //     }
    //     if (patternType.hasProperty(propertyName)) {
    //       JSType type = patternType.findPropertyType(propertyName);
    //       return new RValueInfo(type, qname);
    //     }
    //     return new RValueInfo(null, qname);
    //   }

    //   void defineDestructuringPatternInVarDeclaration(
    //       Node pattern, TypedScope scope, Supplier<RValueInfo> patternTypeSupplier) {
    //     for (DestructuredTarget target :
    //         DestructuredTarget.createAllNonEmptyTargetsInPattern(
    //             typeRegistry, () -> patternTypeSupplier.get().type, pattern)) {

    //       Supplier<RValueInfo> typeSupplier =
    //           () -> inferTypeForDestructuredTarget(target, patternTypeSupplier);

    //       if (target.getNode().isDestructuringPattern()) {
    //         defineDestructuringPatternInVarDeclaration(target.getNode(), scope, typeSupplier);
    //       } else {
    //         Node name = target.getNode();
    //         checkState(name.isName(), "This method is only for declaring variables: %s", name);

    //         // variable's type
    //         JSType type =
    //             getDeclaredType(name.getJSDocInfo(), name, /* rValue= */ null, typeSupplier);
    //         if (type == null) {
    //           // The variable's type will be inferred.
    //           type = name.isFromExterns() ? unknownType : null;
    //         }
    //         new SlotDefiner()
    //             .forDeclarationNode(name)
    //             .forVariableName(name.getString())
    //             .inScope(scope)
    //             .withType(type)
    //             .allowLaterTypeInference(type == null)
    //             .defineSlot();
    //       }
    //     }
    //   }

    //   /**
    //    * Defines a class literal. Handles any of the following cases:
    //    *
    //    * <ul>
    //    *   <li>Class declarations: <code>class Foo { ... }</code>
    //    *   <li>Class assignments: <code>foo.Bar = class { ... }</code>
    //    *   <li>Bleeding names: <code>foo.Bar = class Baz { ... }</code>
    //    *   <li>Properties: <code>{foo: class { ... }}</code>
    //    *   <li>Callbacks: <code>foo(class { ... })</code>
    //    * </ul>
    //    */
    //   void defineClassLiteral(Node n) {
    //     assertDefinitionNode(n, Token.CLASS);

    //     // Determine the name and JSDocInfo and l-value for the class.
    //     // Any of these may be null.
    //     Node lValue = NodeUtil.getBestLValue(n);
    //     JSDocInfo info = NodeUtil.getBestJSDocInfo(n);
    //     String className = NodeUtil.getBestLValueName(lValue);
    //     String classTypeIdentifier = getBestTypeName(lValue, className);

    //     // Create the type and assign it on the CLASS node.
    //     FunctionType classType =
    //         createClassTypeFromNodes(n, classTypeIdentifier, className, info, lValue);
    //     setDeferredType(n, classType);

    //     // Declare this symbol in the current scope iff it's a class
    //     // declaration. Otherwise, the declaration will happen in other
    //     // code paths.
    //     if (NodeUtil.isClassDeclaration(n)) {
    //       checkNotNull(className);
    //       new SlotDefiner()
    //           .forDeclarationNode(n.getFirstChild())
    //           .forVariableName(className)
    //           .inScope(currentScope)
    //           .withType(classType)
    //           .allowLaterTypeInference(classType == null)
    //           .defineSlot();
    //     }
    //   }

    //   private String getBestTypeName(Node lvalue, @Nullable String syntacticLvalueName) {
    //     if (syntacticLvalueName == null) {
    //       return null;
    //     }
    //     if (isGoogModuleExports(lvalue)) {
    //       return syntacticLvalueName.replace("exports", this.getModule().closureNamespace());
    //     }
    //     if (compiler.getOptions().isCheckingMissingOverrideTypes()
    //         && this.getModule() != null
    //         && this.getModule().closureNamespace() != null) {
    //       // When fixing missing override types, we want to generate a fully
    //       // qualified type name for this `syntacticLValueName` in the JSDoc.
    //       if (!syntacticLvalueName.contains(this.getModule().closureNamespace() + ".")) {
    //         return this.getModule().closureNamespace() + "." + syntacticLvalueName;
    //       }
    //     }
    //     return syntacticLvalueName;
    //   }

    //   /** Defines a function literal. */
    //   void defineFunctionLiteral(Node n) {
    //     assertDefinitionNode(n, Token.FUNCTION);

    //     // Determine the name and JSDocInfo and l-value for the function.
    //     // Any of these may be null.
    //     Node lValue = NodeUtil.getBestLValue(n);
    //     JSDocInfo info = NodeUtil.getBestJSDocInfo(n);
    //     String functionName = NodeUtil.getBestLValueName(lValue);
    //     FunctionType functionType = createFunctionTypeFromNodes(n, functionName, info, lValue);

    //     // Assigning the function type to the function node
    //     setDeferredType(n, functionType);

    //     // Declare this symbol in the current scope iff it's a function
    //     // declaration. Otherwise, the declaration will happen in other
    //     // code paths.
    //     if (NodeUtil.isFunctionDeclaration(n)) {
    //       new SlotDefiner()
    //           .forDeclarationNode(n.getFirstChild())
    //           .forVariableName(functionName)
    //           .inScope(currentScope)
    //           .withType(functionType)
    //           .allowLaterTypeInference(functionType == null)
    //           .defineSlot();
    //     }
    //   }

    //   /**
    //    * Defines a variable based on the {@link Token#NAME} node passed.
    //    *
    //    * @param name The {@link Token#NAME} node.
    //    * @param value Optionally, the value assigned to the name node.
    //    * @param scope
    //    * @param info the {@link JSDocInfo} information relating to this {@code name} node.
    //    */
    //   private void defineName(Node name, Node value, TypedScope scope, JSDocInfo info) {
    //     if (ModuleImportResolver.isGoogModuleDependencyCall(value)) {
    //       defineModuleImport(
    //           name, moduleImportResolver.getClosureNamespaceTypeFromCall(value), null, scope);
    //       return;
    //     }
    //     JSType type = getDeclaredType(info, name, value, /* declaredRValueTypeSupplier= */ null);
    //     if (type == null) {
    //       // The variable's type will be inferred.
    //       type = name.isFromExterns() ? unknownType : null;
    //     }
    //     new SlotDefiner()
    //         .forDeclarationNode(name)
    //         .forVariableName(name.getString())
    //         .inScope(scope)
    //         .withType(type)
    //         .allowLaterTypeInference(type == null)
    //         .defineSlot();
    //   }

    //   /**
    //    * @param localNameNode The name node of the LHS of the import being defined
    //    * @param importedModuleObject The root node of the scope in which the type being imported was
    //    *     defined, along with the local name of the overall module object inside the scope where it
    //    *     was defined. Or null if no module with that name exists.
    //    * @param optionalProperty The property name of the locally imported type on the module object,
    //    *     if destructuring-style importing was used. Or null if this is a namespace import.
    //    * @param scopeToDeclareIn The scope in which localNameNode is defined
    //    */
    //   private void defineModuleImport(
    //       Node localNameNode,
    //       @Nullable ScopedName importedModuleObject,
    //       String optionalProperty,
    //       TypedScope scopeToDeclareIn) {
    //     if (importedModuleObject == null) {
    //       // We could not find the module defining this import. Just declare the name as unknown.
    //       new SlotDefiner()
    //           .forDeclarationNode(localNameNode)
    //           .forVariableName(localNameNode.getString())
    //           .inScope(scopeToDeclareIn)
    //           .withType(unknownType)
    //           .allowLaterTypeInference(true)
    //           .defineSlot();
    //       return;
    //     }

    //     final ScopedName exportedName;
    //     if (optionalProperty == null) {
    //       exportedName = importedModuleObject;
    //     } else {
    //       // If this is a destucuring-style import, find its type.
    //       exportedName =
    //           ScopedName.of(
    //               importedModuleObject.getName() + "." + optionalProperty,
    //               importedModuleObject.getScopeRoot());
    //     }
    //     // Try getting the actual scope. The scope will be null in the following cases:
    //     //   - Someone has required a module that does not exist at all.
    //     //   - Someone has requireType'd or forwardDeclare'd a module that exists, but does not have
    //     //     an associated scope yet.
    //     TypedScope exportScope =
    //         exportedName.getScopeRoot() != null ? memoized.get(exportedName.getScopeRoot()) : null;

    //     // The scope is null for modules that were not visited yet.
    //     if (exportScope != null) {
    //       JSType type = exportScope.lookupQualifiedName(QualifiedName.of(exportedName.getName()));

    //       // The type is null if either the name is inferred or this is an early ref.
    //       if (type != null) {
    //         declareAliasTypeIfRvalueIsAliasable(
    //             localNameNode, QualifiedName.of(exportedName.getName()), type, exportScope);

    //         new SlotDefiner()
    //             .forDeclarationNode(localNameNode)
    //             .forVariableName(localNameNode.getString())
    //             .inScope(scopeToDeclareIn)
    //             .withType(type)
    //             .allowLaterTypeInference(type == null)
    //             .defineSlot();
    //         return;
    //       }
    //     }
    //     // Defer defining this name until after we have visited the entire AST.
    //     weakImports.add(new WeakModuleImport(localNameNode, exportedName, scopeToDeclareIn));
    //   }

    //   /**
    //    * If a variable is assigned a function literal in the global scope, make that a declared type
    //    * (even if there's no doc info). There's only one exception to this rule: if the return type is
    //    * inferred, and we're in a local scope, we should assume the whole function is inferred.
    //    */
    //   private boolean shouldUseFunctionLiteralType(FunctionType type, JSDocInfo info, Node lValue) {
    //     if (info != null) {
    //       return true;
    //     }
    //     if (lValue != null && NodeUtil.mayBeObjectLitKey(lValue)) {
    //       return false;
    //     }
    //     // TODO(johnlenz): consider unifying global and local behavior
    //     return isLValueRootedInGlobalScope(lValue) || !type.isReturnTypeInferred();
    //   }

    //   /**
    //    * Creates a new class type from the given class literal. This function does not need to worry
    //    * about stubs and aliases because they are handled by createFunctionTypeFromNodes instead.
    //    */
    //   private FunctionType createClassTypeFromNodes(
    //       Node clazz,
    //       @Nullable String name,
    //       @Nullable String syntacticName,
    //       @Nullable JSDocInfo info,
    //       @Nullable Node lvalueNode) {
    //     checkArgument(clazz.isClass(), clazz);

    //     FunctionTypeBuilder builder =
    //         new FunctionTypeBuilder(name, compiler, clazz, currentScope)
    //             .usingClassSyntax()
    //             .setSyntacticFunctionName(syntacticName)
    //             .setContents(new AstFunctionContents(clazz))
    //             .setDeclarationScope(
    //                 lvalueNode != null ? getLValueRootScope(lvalueNode) : currentScope)
    //             .inferKind(info)
    //             .inferTemplateTypeName(info, null);

    //     Node extendsClause = clazz.getSecondChild();

    //     // Look at the extends clause and/or JSDoc info to find a super class.  Use generics from the
    //     // JSDoc to supplement the extends type when available.
    //     ObjectType baseType = findSuperClassFromNodes(extendsClause, info);
    //     builder.inferInheritance(info, baseType);

    //     // Look for an explicit constructor.
    //     Node constructor = NodeUtil.getEs6ClassConstructorMemberFunctionDef(clazz);
    //     if (constructor != null) {
    //       constructor = constructor.getOnlyChild(); // We want the FUNCTION, not the member.
    //     }

    //     if (constructor != null) {
    //       // Note: constructor should have the following structure:
    //       //   MEMBER_FUNCTION_DEF [jsdoc_info]
    //       //     FUNCTION
    //       //       NAME
    //       //       PARAM_LIST ...
    //       //       BLOCK ...
    //       // NodeUtil.getFirstPropMatchingKey returns the FUNCTION node.
    //       JSDocInfo ctorInfo = NodeUtil.getBestJSDocInfo(constructor);
    //       builder.inferConstructorParameters(constructor.getSecondChild(), ctorInfo);
    //     } else if (extendsClause.isEmpty()) {
    //       // No explicit constructor and no superclass: constructor is no-args.
    //       builder.inferImplicitConstructorParameters(ImmutableList.of());
    //     } else {
    //       // No explicit constructor, but we have a superclass.  If we know its type, then copy its
    //       // constructor arguments (and templates).  If not, make the constructor arguments unknown.
    //       // TODO(sdh): consider allowing attaching constructor @param annotations somewhere else?
    //       FunctionType extendsCtor = baseType != null ? baseType.getConstructor() : null;
    //       if (extendsCtor != null) {
    //         // Known superclass: copy the parameters node.
    //         builder.inferImplicitConstructorParameters(extendsCtor.getParameters());
    //       } else {
    //         // Unresolveable extends clause: suppress typechecking.
    //         builder.inferImplicitConstructorParameters(
    //             typeRegistry.createParametersWithVarArgs(
    //                 typeRegistry.getNativeType(JSTypeNative.UNKNOWN_TYPE)));
    //       }
    //     }

    //     // TODO(sdh): Handle template parameters.  The constructor should store all parameters,
    //     // while the instance type should only have the class-level parameters?

    //     // Add the type for the "constructor" property.
    //     FunctionType classType = builder.buildAndRegister();
    //     if (classType.isConstructor()) {
    //       // NOTE: This logic is similar to the goog.inherits handling in
    //       // ClosureCodingConvention#applySubclassRelationship. If this logic is modified
    //       // it is likely that code needs to be modified as well.

    //       // Notice that constructor functions do not need to be covariant on the superclass.
    //       // So if G extends F, new G() and new F() can accept completely different argument
    //       // types, but G.prototype.constructor needs to be covariant on F.prototype.constructor.
    //       // To get around this, we just turn off type-checking on arguments and return types
    //       // of G.prototype.constructor.

    //       // NOTE: For final classes we could do better here and retain the parameter types.

    //       FunctionType qmarkCtor = classType.forgetParameterAndReturnTypes();
    //       ObjectType classPrototypeType = classType.getPrototypeProperty();
    //       classPrototypeType.defineDeclaredProperty("constructor", qmarkCtor, constructor);
    //     }
    //     if (classType.hasInstanceType()) {
    //       Property classPrototype = classType.getSlot("prototype");
    //       // SymbolTable users expect the class prototype and actual class to have the same
    //       // declaration node.
    //       classPrototype.setNode(lvalueNode != null ? lvalueNode : classPrototype.getNode());
    //     }

    //     return classType;
    //   }

    //   /**
    //    * Look at the {@code extends} clause to find the instance type being extended. Returns {@code
    //    * null} if there is no such clause, and unknown if the type cannot be determined.
    //    */
    //   @Nullable
    //   private ObjectType findSuperClassFromNodes(Node extendsNode, @Nullable JSDocInfo info) {
    //     if (extendsNode.isEmpty()) {
    //       // No extends clause: return null.
    //       return null;
    //     }
    //     JSType ctorType = extendsNode.getJSType();
    //     if (ctorType == null) {
    //       if (extendsNode.isQualifiedName()) {
    //         String superclass = extendsNode.getQualifiedName();
    //         // Look up qualified names in the scope (types won't be set on the AST until inference).
    //         ctorType = currentScope.lookupQualifiedName(QualifiedName.of(superclass));
    //         if (ctorType == null) {
    //           TypedVar var = currentScope.getVar(superclass);
    //           ctorType = var != null ? var.getType() : null;
    //         }
    //         // If that doesn't work, then fall back on the registry
    //         if (ctorType == null) {
    //           return ObjectType.cast(
    //               typeRegistry.getType(
    //                   currentScope,
    //                   superclass,
    //                   extendsNode.getSourceFileName(),
    //                   extendsNode.getLineno(),
    //                   extendsNode.getCharno()));
    //         }
    //       } else if (isGoogModuleGetProperty(extendsNode)) {
    //         ctorType = getCtorForGoogModuleGet(extendsNode);
    //       } else {
    //         // Anything TypedScopeCreator can infer has already been read off the AST.  This is likely
    //         // a CALL or GETELEM, which are unknown until TypeInference.  Instead, ignore it for now,
    //         // require an @extends tag in the JSDoc, and verify correctness in TypeCheck.
    //         if (info == null || !info.hasBaseType()) {
    //           report(JSError.make(extendsNode, DYNAMIC_EXTENDS_WITHOUT_JSDOC));
    //         }
    //       }
    //     }

    //     if (ctorType != null) {
    //       if (ctorType.isConstructor() || ctorType.isInterface()) {
    //         return ctorType.toMaybeFunctionType().getInstanceType();
    //       } else if (ctorType.isUnknownType()) {
    //         // The constructor could have an unknown type for cases where it is dynamically
    //         // created or passed in from elsewhere.
    //         // e.g. with a mixin pattern
    //         // function mixinSomething(ctor) {
    //         //   return class extends ctor { ... };
    //         // }
    //         // In that case consider the super class instance type to be unknown.
    //         return ctorType.toMaybeObjectType();
    //       }
    //     }

    //     // We couldn't determine the type, so for TypedScope creation purposes we will treat it as if
    //     // there were no extends clause.  TypeCheck will give a more precise error later.
    //     return null;
    //   }

    //   private boolean isGoogModuleGetProperty(Node extendsClause) {
    //     if (extendsClause.isCall()) {
    //       return ModuleImportResolver.isGoogModuleDependencyCall(extendsClause);
    //     } else if (extendsClause.isGetProp()) {
    //       return isGoogModuleGetProperty(extendsClause.getFirstChild());
    //     } else {
    //       return false;
    //     }
    //   }

    //   /**
    //    * Looks up the type of a goog.module.get property access
    //    *
    //    * @param extendsNode `goog.module.get('x');` or a getprop `goog.module.get('x').y.z
    //    */
    //   @Nullable
    //   private JSType getCtorForGoogModuleGet(Node extendsNode) {
    //     Node call = extendsNode;
    //     ArrayList<String> properties = new ArrayList<>();
    //     while (!call.isCall()) {
    //       properties.add(0, call.getString());
    //       call = call.getFirstChild();
    //     }
    //     ScopedName extendsCall = moduleImportResolver.getClosureNamespaceTypeFromCall(call);
    //     if (extendsCall == null || !memoized.containsKey(extendsCall.getScopeRoot())) {
    //       // Handle invalid goog.module.get calls
    //       return null;
    //     }
    //     TypedScope moduleScope = memoized.get(extendsCall.getScopeRoot());
    //     properties.add(0, extendsCall.getName());
    //     QualifiedName superclassName = QualifiedName.of(Joiner.on('.').join(properties));
    //     return moduleScope.lookupQualifiedName(superclassName);
    //   }

    //   /**
    //    * Creates a new function type, based on the given nodes.
    //    *
    //    * <p>This handles two cases that are semantically very different, but are not mutually
    //    * exclusive: - A function literal that needs a type attached to it (called from
    //    * defineClassLiteral with a non-null FUNCTION node for rValue). - An assignment expression with
    //    * function-type info in the JsDoc (called from getDeclaredType on a stub (rValue == null) or
    //    * alias (rValue is a qualified name).
    //    *
    //    * <p>All parameters are optional, and we will do the best we can to create a function type.
    //    *
    //    * <p>This function will always create a function type, so only call it if you're sure that's
    //    * what you want.
    //    *
    //    * @param rValue The function node.
    //    * @param name the function's name
    //    * @param info the {@link JSDocInfo} attached to the function definition
    //    * @param lvalueNode The node where this function is being assigned. For example, {@code
    //    *     A.prototype.foo = ...} would be used to determine that this function is a method of
    //    *     A.prototype. May be null to indicate that this is not being assigned to a qualified name.
    //    */
    //   private FunctionType createFunctionTypeFromNodes(
    //       @Nullable Node rValue,
    //       @Nullable String name,
    //       @Nullable JSDocInfo info,
    //       @Nullable Node lvalueNode) {
    //     // Check for an alias.
    //     if (rValue != null && rValue.isQualifiedName() && lvalueNode != null) {
    //       TypedVar var = currentScope.getVar(rValue.getQualifiedName());
    //       if (var != null && var.getType() != null && var.getType().isFunctionType()) {
    //         FunctionType aliasedType = var.getType().toMaybeFunctionType();
    //         if (aliasedType.isConstructor() || aliasedType.isInterface()) {
    //           // TODO(nick): Remove this. This should already be handled by normal type resolution.
    //           if (name != null) {
    //             typeRegistry.declareType(currentScope, name, aliasedType.getInstanceType());
    //           }
    //           checkFunctionAliasAnnotations(lvalueNode, aliasedType, info);
    //           return aliasedType;
    //         }
    //       }
    //     }

    //     // No alias: look for an explicit @type in JSDocInfo.
    //     if (info != null && info.hasType()) {
    //       JSType type = info.getType().evaluate(currentScope, typeRegistry);

    //       // Known to be not null since we have the FUNCTION token there.
    //       type = type.restrictByNotNullOrUndefined();
    //       if (type.isFunctionType()) {
    //         FunctionType functionType = type.toMaybeFunctionType();
    //         functionType.setJSDocInfo(info);
    //         return functionType;
    //       }
    //     }

    //     // No alias or explicit @type, so look for a function literal, or @param/@return.
    //     Node errorRoot = rValue == null ? lvalueNode : rValue;
    //     boolean isFnLiteral = rValue != null && rValue.isFunction();
    //     Node fnRoot = isFnLiteral ? rValue : null;
    //     Node parametersNode = isFnLiteral ? rValue.getSecondChild() : null;

    //     // If this function is being assigned as a property on a type, try finding the owner type
    //     // and the property name.
    //     // This is easy to do for class members because the owner type is on the CLASS node and
    //     // the property name is the MEMBER_FUNCTION_DEF/GETTER_DEF/SETTER_DEF string.
    //     // For other functions, we rely on NodeUtil.getBestLValueOwner.
    //     Node classRoot =
    //         lvalueNode != null && lvalueNode.getParent().isClassMembers()
    //             ? lvalueNode.getGrandparent()
    //             : null;
    //     Node ownerNode = NodeUtil.getBestLValueOwner(lvalueNode);

    //     ObjectType ownerType = null;
    //     String propName = null;
    //     if (classRoot != null) {
    //       // Static members are owned by the constructor, non-statics are owned by the prototype.
    //       ownerType = JSType.toMaybeFunctionType(classRoot.getJSType());
    //       if (!lvalueNode.isStaticMember() && ownerType != null) {
    //         ownerType = ((FunctionType) ownerType).getPrototype();
    //       }
    //       propName = lvalueNode.isComputedProp() ? null : lvalueNode.getString();
    //     } else {
    //       String ownerName = NodeUtil.getBestLValueName(ownerNode);
    //       TypedVar ownerVar = ownerName != null ? currentScope.getVar(ownerName) : null;
    //       if (ownerVar != null) {
    //         ownerType = ObjectType.cast(ownerVar.getType());
    //       }

    //       if (ownerName != null && name != null) {
    //         // TODO(b/111621092): Use the AST rather than manipulating strings here.
    //         checkState(
    //             name.startsWith(ownerName), "Expected \"%s\" to start with \"%s\"", name, ownerName);
    //         propName = name.substring(ownerName.length() + 1);
    //       }
    //     }

    //     ObjectType prototypeOwner = getPrototypeOwnerType(ownerType);
    //     TemplateTypeMap prototypeOwnerTypeMap = null;
    //     if (prototypeOwner != null && prototypeOwner.getTypeOfThis() != null) {
    //       prototypeOwnerTypeMap = prototypeOwner.getTypeOfThis().getTemplateTypeMap();
    //     }

    //     // Find the type of any overridden function.
    //     FunctionType overriddenType = null;
    //     if (ownerType != null && propName != null) {
    //       // the type of the property this overrides, not necessarily a function.
    //       JSType overriddenPropType =
    //           findOverriddenProperty(ownerType, propName, prototypeOwnerTypeMap);
    //       if (overriddenPropType != null) {
    //         // Overridden getters and setters need special handling because we declare
    //         // getters/setters as simple properties with their respective return/parameter type. This
    //         // causes a split during inference where left and right sides of a getter/setter
    //         // declaration will be inferred to have different types; if the left side has type `T`,
    //         // the right side will be some function type involving `T`.
    //         if (lvalueNode.isGetterDef()) {
    //           // Convert `number` to `function(): number`
    //           overriddenType = typeRegistry.createFunctionType(overriddenPropType);
    //         } else if (lvalueNode.isSetterDef()) {
    //           // Convert `number` to `function(number): undefined`
    //           overriddenType =
    //               typeRegistry.createFunctionType(getNativeType(VOID_TYPE), overriddenPropType);
    //         } else if (overriddenPropType.isFunctionType()) {
    //           // for cases where we override a non-method (e.g. a number) with a method, don't put the
    //           // non-method type (e.g. number) on the function.
    //           // Instead do some basic inference to create a function type.
    //           // we will warn during typechecking for an invalid override, but we don't want to put a
    //           // non-function type on this function because that will interfere with type inference
    //           // inside the function.
    //           overriddenType = overriddenPropType.toMaybeFunctionType();
    //         }
    //       }
    //     }

    //     AstFunctionContents contents = fnRoot != null ? new AstFunctionContents(fnRoot) : null;
    //     if (functionsWithNonEmptyReturns.contains(fnRoot)) {
    //       contents.recordNonEmptyReturn();
    //     }

    //     String bestTypeName = getBestTypeName(lvalueNode, name);
    //     FunctionTypeBuilder builder =
    //         new FunctionTypeBuilder(bestTypeName, compiler, errorRoot, currentScope)
    //             .setSyntacticFunctionName(name)
    //             .setContents(contents)
    //             .setDeclarationScope(
    //                 lvalueNode != null ? getLValueRootScope(lvalueNode) : currentScope)
    //             .inferFromOverriddenFunction(overriddenType, parametersNode)
    //             .inferKind(info)
    //             .inferClosurePrimitive(info)
    //             .inferTemplateTypeName(info, prototypeOwner)
    //             .inferInheritance(info, null);

    //     if (info == null || !info.hasReturnType()) {
    //       // when there is no @return annotation, look for inline return type declaration
    //       if (rValue != null && rValue.isFunction() && rValue.hasChildren()) {
    //         JSDocInfo nameDocInfo = rValue.getFirstChild().getJSDocInfo();
    //         builder.inferReturnType(nameDocInfo, true);
    //       }
    //     } else {
    //       builder.inferReturnType(info, false);
    //     }

    //     // Infer the context type.
    //     JSType fallbackReceiverType = null;
    //     if (ownerType != null
    //         && ownerType.isFunctionPrototypeType()
    //         && ownerType.getOwnerFunction().hasInstanceType()) {
    //       fallbackReceiverType = ownerType.getOwnerFunction().getInstanceType();
    //     } else if (ownerType != null
    //         && ownerType.isFunctionType()
    //         && ownerType.toMaybeFunctionType().hasInstanceType()
    //         && lvalueNode != null
    //         && lvalueNode.isStaticMember()) {
    //       // Limit this case to members of ctors and interfaces decalared using `static`. Most
    //       // namespaces, like object literals, are assumed to declare free functions, so we exclude
    //       // them. Additionally, methods *assigned* to a ctor, especially an ES5 ctor, were never
    //       // designed with static polymorphism in mind, so excluding them preserves their assumptions.
    //       fallbackReceiverType = ownerType;
    //     } else if (ownerNode != null && ownerNode.isThis()) {
    //       fallbackReceiverType = currentScope.getTypeOfThis();
    //     }

    //     FunctionType fnType =
    //         builder
    //             .inferThisType(info, fallbackReceiverType)
    //             .inferParameterTypes(parametersNode, info)
    //             .buildAndRegister();

    //     // Do some additional validation for constructors and interfaces.
    //     if (fnType.hasInstanceType() && lvalueNode != null) {
    //       Property prototypeSlot = fnType.getSlot("prototype");

    //       // We want to make sure that the function and its prototype are declared at the same node.
    //       // This consistency is helpful to users of SymbolTable, because everything gets declared at
    //       // the same place.
    //       prototypeSlot.setNode(lvalueNode);
    //     }
    //     return fnType;
    //   }

    //   /**
    //    * Checks that the annotations in {@code info} are compatible with the aliased {@code type}. Any
    //    * errors will be reported at {@code n}, which should be the qualified name node.
    //    */
    //   private void checkFunctionAliasAnnotations(Node n, FunctionType type, JSDocInfo info) {
    //     if (info == null) {
    //       return;
    //     }
    //     String annotation = null;
    //     if (info.usesImplicitMatch()) {
    //       if (!type.isStructuralInterface()) {
    //         annotation = "@record";
    //       }
    //     } else if (info.isInterface()) {
    //       if (!type.isInterface()) {
    //         annotation = "@interface";
    //       }
    //     } else if (info.isConstructor() && !type.isConstructor()) {
    //       annotation = "@constructor";
    //     }
    //     // TODO(sdh): consider checking @template, @param, @return, and/or @this.
    //     if (annotation != null
    //         // TODO(sdh): Remove this extra check once TypeScript stops passing us duplicate
    //         // conflicting externs.  In particular, TS considers everything an interface, but Closure
    //         // externs mark most things as @constructor.  The load order is not always the same, so
    //         // the error can show up in either the generated TS externs file or in our own extern.
    //         && (!n.isFromExterns() || annotation.equals("@record"))) {
    //       report(JSError.make(n, INCOMPATIBLE_ALIAS_ANNOTATION, annotation, n.getQualifiedName()));
    //     }
    //   }

    //   private ObjectType getPrototypeOwnerType(ObjectType ownerType) {
    //     if (ownerType != null && ownerType.isFunctionPrototypeType()) {
    //       return ownerType.getOwnerFunction();
    //     }
    //     return null;
    //   }

    //   /**
    //    * Find the property that's being overridden on this type, if any.
    //    *
    //    * <p>Said property could be a method, field, getter, or setter. We don't distinguish between
    //    * these when looking up a property type.
    //    */
    //   private JSType findOverriddenProperty(
    //       ObjectType ownerType, String propName, TemplateTypeMap typeMap) {
    //     JSType result = null;

    //     // First, check to see if the property is implemented
    //     // on a superclass.
    //     JSType propType = ownerType.getPropertyType(propName);
    //     if (propType != null && !propType.isUnknownType()) {
    //       result = propType;
    //     } else {
    //       // If it's not, then check to see if it's implemented
    //       // on an implemented interface.
    //       for (ObjectType iface : ownerType.getCtorImplementedInterfaces()) {
    //         propType = iface.getPropertyType(propName);
    //         if (propType != null && !propType.isUnknownType()) {
    //           result = propType;
    //           break;
    //         }
    //       }
    //     }

    //     if (result != null && typeMap != null && !typeMap.isEmpty()) {
    //       result = result.visit(TemplateTypeReplacer.forPartialReplacement(typeRegistry, typeMap));
    //     }

    //     return result;
    //   }

    //   /**
    //    * Creates a new enum type, based on the given nodes.
    //    *
    //    * <p>This handles two cases that are semantically very different, but are not mutually
    //    * exclusive: (1) An object literal that needs an enum type attached to it. (2) An assignment
    //    * expression with an enum tag in the JsDoc.
    //    *
    //    * <p>This function will always create an enum type, so only call it if you're sure that's what
    //    * you want.
    //    *
    //    * @param rValue The right-hand side of the enum, or null if none.
    //    * @param lValue The left-hand side of the enum.
    //    * @param name The qualified name of the enum
    //    * @param info The {@link JSDocInfo} attached to the enum definition.
    //    */
    //   private EnumType createEnumTypeFromNodes(
    //       @Nullable Node rValue, @Nullable String name, Node lValue, JSDocInfo info) {
    //     checkNotNull(info);
    //     checkState(info.hasEnumParameterType());
    //     checkState(
    //         lValue != null || rValue != null,
    //         "An enum initializer should come from either an lvalue or rvalue");

    //     EnumType enumType = null;
    //     if (rValue != null && rValue.isQualifiedName()) {
    //       // Handle an aliased enum. Note that putting @enum on an enum alias is optional. If the
    //       // rValue is not an enum, then this assignment errors during TypeCheck.
    //       TypedVar var = currentScope.getVar(rValue.getQualifiedName());
    //       if (var != null && var.getType() != null && var.getType().isEnumType()) {
    //         enumType = var.getType().toMaybeEnumType();
    //       }
    //     }

    //     if (enumType == null) {
    //       enumType =
    //           EnumType.builder(typeRegistry)
    //               .setName(getBestTypeName(lValue, name))
    //               .setGoogModuleId(containingGoogModuleIdOf(this.currentScope))
    //               .setSource(rValue)
    //               .setElementType(info.getEnumParameterType().evaluate(currentScope, typeRegistry))
    //               .build();

    //       if (rValue != null && rValue.isObjectLit()) {
    //         // collect enum elements
    //         Node key = rValue.getFirstChild();
    //         while (key != null) {
    //           if (key.isComputedProp()) {
    //             report(JSError.make(key, INVALID_ENUM_KEY));
    //             key = key.getNext();
    //             continue;
    //           }
    //           String keyName = key.getString();
    //           Preconditions.checkNotNull(keyName, "Invalid enum key: %s", key);
    //           enumType.defineElement(keyName, key);
    //           key = key.getNext();
    //         }
    //       }
    //     }

    //     if (name != null) {
    //       typeRegistry.declareType(currentScope, name, enumType.getElementsType());
    //     }

    //     if (rValue == null || !(rValue.isObjectLit() || rValue.isQualifiedName())) {
    //       report(JSError.make(lValue != null ? lValue : rValue, ENUM_INITIALIZER));
    //     }
    //     return enumType;
    //   }

    //   /** Responsible for defining typed variable "slots". */
    //   class SlotDefiner {
    //     Node declarationNode;
    //     String variableName;
    //     TypedScope scope;
    //     // default is no type and a type may be inferred later
    //     JSType type = null;
    //     boolean allowLaterTypeInference = true;
    //     boolean forGoogProvidedName = false;

    //     // TODO(bradfordcsmith): Once all the logic needed for ES_2017 features has been added,
    //     //     make the API to this class more restrictive to avoid accidental misuse.
    //     //     e.g. There will probably always be a declarationNode, so make it a constructor
    //     //     parameter.

    //     /** @param declarationNode the defining NAME or GETPROP or object literal key node. */
    //     SlotDefiner forDeclarationNode(Node declarationNode) {
    //       this.declarationNode = declarationNode;
    //       return this;
    //     }

    //     SlotDefiner readVariableNameFromDeclarationNode() {
    //       // Only qualified name nodes can use this method to get the variable name
    //       // Object literal keys will have to compute their names themselves.
    //       // TODO(bradfordcsmith): Clean up these checks of the parent.
    //       Node parent = declarationNode.getParent();
    //       if (declarationNode.isName()) {
    //         checkArgument(
    //             parent.isFunction()
    //                 || parent.isClass()
    //                 || NodeUtil.isNameDeclaration(parent)
    //                 || parent.isParamList()
    //                 || (parent.isRest() && parent.getParent().isParamList())
    //                 || parent.isCatch());
    //       } else {
    //         checkArgument(
    //             declarationNode.isGetProp() && (parent.isAssign() || parent.isExprResult()));
    //       }
    //       variableName = declarationNode.getQualifiedName();
    //       return this;
    //     }

    //     // TODO(bradfordcsmith): maybe change to withVariableName(). Need to make these names more
    //     //     consistent.
    //     SlotDefiner forVariableName(String variableName) {
    //       this.variableName = variableName;
    //       return this;
    //     }

    //     /**
    //      * Sets the scope in which the variable should be declared.
    //      *
    //      * <p>If the given name is a qualified name, this scope should be the scope in which the root
    //      * of the name is (or will later be) declared.
    //      */
    //     SlotDefiner inScope(TypedScope scope) {
    //       this.scope = checkNotNull(scope);
    //       return this;
    //     }

    //     SlotDefiner withType(@Nullable JSType type) {
    //       this.type = type;
    //       return this;
    //     }

    //     SlotDefiner allowLaterTypeInference(boolean allowLaterTypeInference) {
    //       this.allowLaterTypeInference = allowLaterTypeInference;
    //       return this;
    //     }

    //     SlotDefiner forGoogProvidedName() {
    //       this.forGoogProvidedName = true;
    //       return this;
    //     }

    //     /**
    //      * Define the slot and do related work.
    //      *
    //      * <p>At minimum the declaration node and variable name must have been set.
    //      */
    //     void defineSlot() {
    //       checkNotNull(declarationNode, "declarationNode not set");
    //       checkState(
    //           declarationNode.isName()
    //               || declarationNode.isGetProp()
    //               || NodeUtil.mayBeObjectLitKey(declarationNode)
    //               || declarationNode.isModuleBody()
    //               || declarationNode.isExport()
    //               || (this.forGoogProvidedName && NodeUtil.isExprCall(declarationNode)),
    //           "declaration node must be an lvalue or goog.provide call, found %s",
    //           declarationNode);
    //       checkNotNull(variableName, "variableName not set");
    //       checkState(allowLaterTypeInference || type != null, "null type but inference not allowed");
    //       checkState(!variableName.isEmpty());
    //       checkNotNull(scope);

    //       Node parent = declarationNode.getParent();

    //       TypedScope scopeToDeclareIn = scope;

    //       boolean isGlobalVar = declarationNode.isName() && scopeToDeclareIn.isGlobal();
    //       boolean shouldDeclareOnGlobalThis =
    //           isGlobalVar && (parent.isVar() || parent.isFunction())
    //               || this.forGoogProvidedName && !variableName.contains(".");

    //       // TODO(sdh): Remove this special case.  It is required to reproduce the original
    //       // non-block-scoped behavior, which is depended on in several places including
    //       // https://github.com/angular/tsickle/issues/761.  But it's more correct to always
    //       // declare on the owner scope.  Once all the bugs are fixed, this should be removed.
    //       // We may be able to get by with checking a "declared" function's source for jsdoc.
    //       if (scopeToDeclareIn != currentHoistScope
    //           && scopeToDeclareIn.isGlobal()
    //           && scopeToDeclareIn.hasOwnSlot(variableName)
    //           && !this.forGoogProvidedName) {
    //         scopeToDeclareIn = currentHoistScope;
    //       }

    //       // The input may be null if we are working with a AST snippet. So read
    //       // the extern info from the node.

    //       // declared in closest scope?
    //       CompilerInput input = compiler.getInput(inputId);
    //       if (!scopeToDeclareIn.canDeclare(variableName)) {
    //         TypedVar oldVar = scopeToDeclareIn.getVar(variableName);
    //         validator.expectUndeclaredVariable(
    //             sourceName, input, declarationNode, parent, oldVar, variableName, type);
    //       } else {
    //         if (type != null) {
    //           setDeferredType(declarationNode, type);
    //         }

    //         declare(
    //             scopeToDeclareIn,
    //             variableName,
    //             declarationNode,
    //             type,
    //             input,
    //             allowLaterTypeInference);
    //       }

    //       // We need to do some additional work for constructors and interfaces.
    //       FunctionType fnType = JSType.toMaybeFunctionType(type);
    //       if (fnType != null
    //           // We don't want to look at empty function types.
    //           && !type.isEmptyType()) {

    //         // We want to make sure that when we declare a new instance type
    //         // (with @constructor) that there's actually a ctor for it.
    //         // This doesn't apply to structural constructors (like
    //         // function(new:Array). Checking the constructed type against
    //         // the variable name is a sufficient check for this.
    //         if (fnType.isConstructor() || fnType.isInterface()) {
    //           finishConstructorDefinition(
    //               declarationNode, variableName, fnType, scopeToDeclareIn, input);
    //         }
    //       }

    //       if (shouldDeclareOnGlobalThis) {
    //         ObjectType globalThis = typeRegistry.getNativeObjectType(GLOBAL_THIS);
    //         if (allowLaterTypeInference) {
    //           globalThis.defineInferredProperty(
    //               variableName,
    //               type == null ? getNativeType(JSTypeNative.NO_TYPE) : type,
    //               declarationNode);
    //         } else {
    //           globalThis.defineDeclaredProperty(variableName, type, declarationNode);
    //         }
    //       }

    //       if (isGlobalVar
    //           && "Window".equals(variableName)
    //           && type != null
    //           && type.isFunctionType()
    //           && type.isConstructor()) {
    //         FunctionType globalThisCtor =
    //             typeRegistry.getNativeObjectType(GLOBAL_THIS).getConstructor();
    //         globalThisCtor.getInstanceType().clearCachedValues();
    //         globalThisCtor.getPrototype().clearCachedValues();
    //         globalThisCtor.setPrototypeBasedOn((type.toMaybeFunctionType()).getInstanceType());
    //       }
    //     }
    //   }

    //   /**
    //    * Declares a variable with the given {@code name} and {@code type} on the given {@code scope},
    //    * returning the newly-declared {@link TypedVar}. Additionally checks the {@link
    //    * #escapedVarNames} and {@link #assignedVarNames} maps (which were populated during the {@link
    //    * FirstOrderFunctionAnalyzer} and marks the result as escaped or assigned exactly once if
    //    * appropriate.
    //    */
    //   private TypedVar declare(
    //       TypedScope scope, String name, Node n, JSType type, CompilerInput input, boolean inferred) {
    //     TypedVar var = scope.declare(name, n, type, input, inferred);
    //     ScopedName scopedName = ScopedName.of(name, scope.getRootNode());
    //     if (escapedVarNames.contains(scopedName)) {
    //       var.markEscaped();
    //     }
    //     if (assignedVarNames.count(scopedName) == 1) {
    //       var.markAssignedExactlyOnce();
    //     }
    //     return var;
    //   }

    //   private void finishConstructorDefinition(
    //       Node declarationNode,
    //       String variableName,
    //       FunctionType fnType,
    //       TypedScope scopeToDeclareIn,
    //       CompilerInput input) {
    //     // Declare var.prototype in the scope chain.
    //     FunctionType superClassCtor = fnType.getSuperClassConstructor();
    //     Property prototypeSlot = fnType.getSlot("prototype");

    //     String prototypeName = variableName + ".prototype";

    //     // There are some rare cases where the prototype will already
    //     // be declared. See TypedScopeCreatorTest#testBogusPrototypeInit.
    //     // Fortunately, other warnings will complain if this happens.
    //     TypedVar prototypeVar = scopeToDeclareIn.getVar(prototypeName);
    //     if (prototypeVar != null && prototypeVar.getScope() == scopeToDeclareIn) {
    //       scopeToDeclareIn.undeclare(prototypeVar);
    //     }

    //     scopeToDeclareIn.declare(
    //         prototypeName,
    //         declarationNode,
    //         prototypeSlot.getType(),
    //         input,
    //         // declared iff there's an explicit supertype
    //         superClassCtor == null
    //             || superClassCtor.getInstanceType().equals(getNativeType(OBJECT_TYPE)));
    //   }

    //   /** Check if the given node is a property of a name in the global scope. */
    //   private boolean isLValueRootedInGlobalScope(Node n) {
    //     return getLValueRootScope(n).isGlobal();
    //   }

    //   /** Return the scope for the name of the given node. */
    //   private TypedScope getLValueRootScope(Node n) {
    //     Node root = NodeUtil.getBestLValueRoot(n);
    //     if (root != null) {
    //       if (root.isName()) {
    //         Node nameParent = root.getParent();
    //         switch (nameParent.getToken()) {
    //           case VAR:
    //             return currentHoistScope;
    //           case LET:
    //           case CONST:
    //           case CLASS:
    //           case FUNCTION:
    //           case PARAM_LIST:
    //           case CATCH:
    //             return currentScope;

    //           case ITER_REST:
    //           case OBJECT_REST:
    //             // TODO(bradfordcsmith): Handle array destructuring REST
    //             checkState(nameParent.getParent().isParamList(), nameParent);
    //             return currentScope;

    //           default:
    //             if (isGoogModuleExports(root)) {
    //               // Ensure that 'exports = class {}' in a goog.module returns the module scope.
    //               return currentScope;
    //             }
    //             TypedVar var = currentScope.getVar(root.getString());
    //             if (var != null) {
    //               return var.getScope();
    //             }
    //         }
    //       } else if (root.isThis() || root.isSuper()) {
    //         // We want the enclosing function scope, or the global scope if not in a function.
    //         return currentHoistScope.getScopeOfThis();
    //       }
    //     }
    //     return currentHoistScope.getGlobalScope();
    //   }

    //   /**
    //    * Look for a type declaration on a property assignment (in an ASSIGN or an object literal key).
    //    *
    //    * @param info The doc info for this property.
    //    * @param lValue The l-value node.
    //    * @param rValue The node that {@code n} is being initialized to, or {@code null} if this is a
    //    *     stub declaration.
    //    * @param declaredRValueTypeSupplier A supplier for the declared type of the rvalue, used for
    //    *     destructuring declarations where we have to do additional work on the rvalue.
    //    */
    //   JSType getDeclaredType(
    //       JSDocInfo info,
    //       Node lValue,
    //       @Nullable Node rValue,
    //       @Nullable Supplier<RValueInfo> declaredRValueTypeSupplier) {
    //     if (info != null && info.hasType()) {
    //       return getDeclaredTypeInAnnotation(lValue, info);
    //     } else if (rValue != null
    //         && rValue.isFunction()
    //         && shouldUseFunctionLiteralType(
    //             JSType.toMaybeFunctionType(rValue.getJSType()), info, lValue)) {
    //       return rValue.getJSType();
    //     } else if (rValue != null && rValue.isClass()) {
    //       return rValue.getJSType();
    //     } else if (info != null) {
    //       if (info.hasEnumParameterType()) {
    //         if (rValue != null && rValue.isObjectLit()) {
    //           return rValue.getJSType();
    //         } else {
    //           return createEnumTypeFromNodes(rValue, lValue.getQualifiedName(), lValue, info);
    //         }
    //       } else if (info.isConstructorOrInterface()) {
    //         FunctionType fnType =
    //             createFunctionTypeFromNodes(rValue, lValue.getQualifiedName(), info, lValue);
    //         if (rValue == null && !lValue.isFromExterns()) {
    //           report(
    //               JSError.make(
    //                   lValue,
    //                   fnType.isConstructor() ? CTOR_INITIALIZER : IFACE_INITIALIZER,
    //                   lValue.getQualifiedName()));
    //         }
    //         return fnType;
    //       }
    //     }

    //     // Check if this is constant, and if it has a known type.
    //     if (NodeUtil.isConstantDeclaration(info, lValue) || isGoogModuleExports(lValue)) {
    //       if (rValue != null) {
    //         JSType rValueType = getDeclaredRValueType(lValue, rValue);
    //         declareAliasTypeIfRvalueIsAliasable(
    //             lValue, rValue.getQualifiedNameObject(), rValueType, currentScope);
    //         if (rValueType != null) {
    //           return rValueType;
    //         }
    //       } else if (declaredRValueTypeSupplier != null) {
    //         RValueInfo rvalueInfo = declaredRValueTypeSupplier.get();
    //         if (rvalueInfo != null) {
    //           declareAliasTypeIfRvalueIsAliasable(
    //               lValue, rvalueInfo.qualifiedName, rvalueInfo.type, currentScope);
    //           if (rvalueInfo.type != null) {
    //             return rvalueInfo.type;
    //           }
    //         }
    //       }
    //     }

    //     if (info != null && FunctionTypeBuilder.isFunctionTypeDeclaration(info)) {
    //       String fnName = lValue.getQualifiedName();
    //       return createFunctionTypeFromNodes(null, fnName, info, lValue);
    //     }

    //     if (isValidTypedefDeclaration(lValue, info)) {
    //       return getNativeType(JSTypeNative.NO_TYPE);
    //     }

    //     return null;
    //   }

    //   /**
    //    * For a const alias, like `const alias = other.name`, this may declare `alias` as a type name,
    //    * depending on what other.name is defined to be.
    //    *
    //    * <p>This method recognizes three kinds of type aliases: @typedefs, @constructor/@interface
    //    * types, and @enums.
    //    *
    //    * <p>Given any of those three types, this method redeclares the aliasing name in the
    //    * typeRegistry. For @typedefs and global @enums, this method also marks the qualified name
    //    * referring to the type as non-nullable by default.
    //    */
    //   private void declareAliasTypeIfRvalueIsAliasable(
    //       Node lValue,
    //       @Nullable QualifiedName rValue,
    //       @Nullable JSType rValueType,
    //       TypedScope rValueLookupScope) {
    //     declareAliasTypeIfRvalueIsAliasable(
    //         lValue.getQualifiedName(), lValue, rValue, rValueType, rValueLookupScope, currentScope);
    //   }

    //   /**
    //    * For a const alias, like `const alias = other.name`, this may declare `alias` as a type name,
    //    * depending on what other.name is defined to be.
    //    *
    //    * <p>NOTE: in most cases, call the version with fewer arguments. This version only exists to
    //    * handle goog.declareLegacyNamespace, which is strange compared to normal type aliasing because
    //    * 1) there's no GETPROP node representing the lvalue and 2) the type is declared in the global
    //    * scope, not the current module-local scope.
    //    *
    //    * @param lValueName the fully qualified lValue name, if any. If null, all this method will do
    //    *     is propagate the @typedef Node annotation to actualLvalueNode.
    //    * @param aliasDeclarationScope The scope in which to declare the alias name. In most cases,
    //    *     this should just be the {@link #currentScope}.
    //    */
    //   private void declareAliasTypeIfRvalueIsAliasable(
    //       @Nullable String lValueName,
    //       @Nullable Node actualLvalueNode,
    //       @Nullable QualifiedName rValue,
    //       @Nullable JSType rValueType,
    //       TypedScope rValueLookupScope,
    //       TypedScope aliasDeclarationScope) {
    //     // NOTE: this allows some strange patterns such allowing instance properties
    //     // to be aliases of constructors, and then creating a local alias of that to be
    //     // used as a type name.  Consider restricting this.

    //     if (rValue == null) {
    //       return;
    //     }

    //     // Look for a @typedef annotation on the definition node
    //     Node definitionNode = getDefinitionNode(rValue, rValueLookupScope);
    //     if (definitionNode != null) {
    //       JSType typedefType = definitionNode.getTypedefTypeProp();
    //       if (typedefType != null) {
    //         // Propagate typedef type to typedef aliases.
    //         actualLvalueNode.setTypedefTypeProp(typedefType);
    //         if (lValueName != null) {
    //           typeRegistry.identifyNonNullableName(aliasDeclarationScope, lValueName);
    //           typeRegistry.declareType(aliasDeclarationScope, lValueName, typedefType);
    //         }
    //         return;
    //       }
    //     }

    //     if (lValueName == null) {
    //       return;
    //     }

    //     // Check if the provided rValueType indicates that we should declare this type
    //     // Note that we only look for enums and constructors/interfaces here: this step cannot work
    //     // for @typedefs. The 'type' of the TypedVar representing a @typedef'd name is the None type,
    //     // not the @typedef'd type.
    //     if (rValueType != null
    //         && rValueType.isFunctionType()
    //         && rValueType.toMaybeFunctionType().hasInstanceType()) {
    //       // Look for @constructor/@interface by checking if the RHS has an instance type
    //       FunctionType functionType = rValueType.toMaybeFunctionType();
    //       typeRegistry.declareType(aliasDeclarationScope, lValueName, functionType.getInstanceType());
    //       return;
    //     }

    //     if (rValueType != null && rValueType.isEnumType()) {
    //       // Look for cases where the rValue is an Enum namespace
    //       typeRegistry.declareType(
    //           aliasDeclarationScope, lValueName, rValueType.toMaybeEnumType().getElementsType());
    //       typeRegistry.identifyNonNullableName(aliasDeclarationScope, lValueName);
    //     }
    //   }

    //   /** Whether this lvalue is either `exports`, `exports.x`, or a string key in `exports = {x}`. */
    //   boolean isGoogModuleExports(Node lValue) {
    //     if (this.getModule() == null || lValue == null) {
    //       return false;
    //     }
    //     if (undeclaredNamesForClosure.contains(lValue)) {
    //       // includes "exports" and "exports.x"
    //       return true;
    //     }
    //     // ASSIGN
    //     //   NAME "exports"
    //     //   OBJECT_LIT
    //     //     STRING_KEY "x"
    //     //     [value]
    //     return lValue.isStringKey()
    //         && lValue.getParent().isObjectLit()
    //         && lValue.getGrandparent().isAssign()
    //         && lValue.getParent().getPrevious().matchesName("exports")
    //         && undeclaredNamesForClosure.contains(lValue.getParent().getPrevious());
    //   }

    //   /** Returns the AST node associated with the definition, if any. */
    //   private Node getDefinitionNode(QualifiedName qname, TypedScope scope) {
    //     if (qname.isSimple()) {
    //       TypedVar var = scope.getVar(qname.getComponent());
    //       return var != null ? var.getNameNode() : null;
    //     }
    //     ObjectType parent = ObjectType.cast(scope.lookupQualifiedName(qname.getOwner()));
    //     return parent != null ? parent.getPropertyDefSite(qname.getComponent()) : null;
    //   }

    //   /**
    //    * Check for common idioms of a typed R-value assigned to a const L-value.
    //    *
    //    * <p>Normally, we would only want this sort of propagation to happen under type inference. But
    //    * we want a declared const to be nameable in a type annotation, so we need to figure out the
    //    * type before we try to resolve the annotation.
    //    *
    //    * @param lValue is the lvalue node if this is a simple assignment, null for destructuring
    //    */
    //   private JSType getDeclaredRValueType(@Nullable Node lValue, Node rValue) {
    //     // If rValue has a type-cast, we use the type in the type-cast.
    //     JSDocInfo rValueInfo = rValue.getJSDocInfo();
    //     if (rValue.isCast() && rValueInfo != null && rValueInfo.hasType()) {
    //       return rValueInfo.getType().evaluate(currentScope, typeRegistry);
    //     }

    //     // Check if the type has already been computed during scope-creation.
    //     // This is mostly useful for literals like BOOLEAN, NUMBER, STRING, and
    //     // OBJECT_LITERAL
    //     JSType type = rValue.getJSType();
    //     if (type != null && !type.isUnknownType()) {
    //       return type;
    //     }

    //     // If rValue is a name, try looking it up in the current scope.
    //     if (rValue.isQualifiedName()) {
    //       return currentScope.lookupQualifiedName(rValue.getQualifiedNameObject());
    //     }

    //     // Check for simple invariant operations, such as "!x" or "+x" or "''+x"
    //     if (NodeUtil.isBooleanResult(rValue)) {
    //       return getNativeType(BOOLEAN_TYPE);
    //     }

    //     if (NodeUtil.isNumericResult(rValue)) {
    //       return getNativeType(NUMBER_TYPE);
    //     }

    //     if (NodeUtil.isBigIntResult(rValue)) {
    //       return getNativeType(BIGINT_TYPE);
    //     }

    //     if (NodeUtil.isStringResult(rValue)) {
    //       return getNativeType(STRING_TYPE);
    //     }

    //     if (rValue.isNew() && rValue.getFirstChild().isQualifiedName()) {
    //       JSType targetType =
    //           currentScope.lookupQualifiedName(rValue.getFirstChild().getQualifiedNameObject());
    //       if (targetType != null) {
    //         FunctionType fnType = targetType.restrictByNotNullOrUndefined().toMaybeFunctionType();
    //         if (fnType != null && fnType.hasInstanceType()) {
    //           return fnType.getInstanceType();
    //         }
    //       }
    //     }

    //     // Check for a very specific JS idiom:
    //     // var x = x || TYPE;
    //     // This is used by Closure's base namespace for esoteric
    //     // reasons, so we only really care about that case.
    //     if (rValue.isOr()) {
    //       Node firstClause = rValue.getFirstChild();
    //       Node secondClause = firstClause.getNext();
    //       boolean namesMatch =
    //           firstClause.isName()
    //               && lValue != null
    //               && lValue.isName()
    //               && firstClause.getString().equals(lValue.getString());
    //       if (namesMatch) {
    //         type = secondClause.getJSType();
    //         if (type != null && !type.isUnknownType()) {
    //           return type;
    //         }
    //       }
    //     }

    //     return null;
    //   }

    //   /**
    //    * Look for class-defining calls. Because JS has no 'native' syntax for defining classes, this
    //    * is often very coding-convention dependent and business-logic heavy.
    //    */
    //   void checkForClassDefiningCalls(Node n) {
    //     SubclassRelationship relationship = codingConvention.getClassesDefinedByCall(n);
    //     if (relationship != null) {
    //       ObjectType superClass =
    //           TypeValidator.getInstanceOfCtor(
    //               currentScope.lookupQualifiedName(QualifiedName.of(relationship.superclassName)));
    //       ObjectType subClass =
    //           TypeValidator.getInstanceOfCtor(
    //               currentScope.lookupQualifiedName(QualifiedName.of(relationship.subclassName)));
    //       if (superClass != null && subClass != null) {
    //         // superCtor and subCtor might be structural constructors
    //         // (like {function(new:Object)}) so we need to resolve them back
    //         // to the original ctor objects.
    //         FunctionType superCtor = superClass.getConstructor();
    //         FunctionType subCtor = subClass.getConstructor();
    //         if (superCtor != null && subCtor != null) {
    //           codingConvention.applySubclassRelationship(
    //               new NominalTypeBuilder(superCtor, superClass),
    //               new NominalTypeBuilder(subCtor, subClass),
    //               relationship.type);
    //         }
    //       }
    //     }

    //     String singletonGetterClassName = codingConvention.getSingletonGetterClassName(n);
    //     if (singletonGetterClassName != null) {
    //       ObjectType objectType =
    //           ObjectType.cast(typeRegistry.getType(currentScope, singletonGetterClassName));
    //       if (objectType != null) {
    //         FunctionType functionType = objectType.getConstructor();

    //         if (functionType != null) {
    //           FunctionType getterType = typeRegistry.createFunctionType(objectType);
    //           codingConvention.applySingletonGetter(
    //               new NominalTypeBuilder(functionType, objectType), getterType);
    //         }
    //       }
    //     }

    //     DelegateRelationship delegateRelationship = codingConvention.getDelegateRelationship(n);
    //     if (delegateRelationship != null) {
    //       applyDelegateRelationship(delegateRelationship);
    //     }

    //     ObjectLiteralCast objectLiteralCast = codingConvention.getObjectLiteralCast(n);
    //     if (objectLiteralCast != null) {
    //       if (objectLiteralCast.diagnosticType == null) {
    //         ObjectType type =
    //             ObjectType.cast(typeRegistry.getType(currentScope, objectLiteralCast.typeName));
    //         if (type != null && type.getConstructor() != null) {
    //           setDeferredType(objectLiteralCast.objectNode, type);
    //           objectLiteralCast.objectNode.putBooleanProp(Node.REFLECTED_OBJECT, true);
    //         } else {
    //           report(JSError.make(n, CONSTRUCTOR_EXPECTED));
    //         }
    //       } else {
    //         report(JSError.make(n, objectLiteralCast.diagnosticType));
    //       }
    //     }
    //   }

    //   /** Apply special properties that only apply to delegates. */
    //   private void applyDelegateRelationship(DelegateRelationship delegateRelationship) {
    //     ObjectType delegatorObject =
    //         ObjectType.cast(
    //             typeRegistry.getType(
    //                 currentScope, delegateRelationship.delegator.getQualifiedName()));
    //     ObjectType delegateBaseObject =
    //         ObjectType.cast(
    //             typeRegistry.getType(
    //                 currentScope, delegateRelationship.delegateBase.getQualifiedName()));
    //     ObjectType delegateSuperObject =
    //         ObjectType.cast(
    //             typeRegistry.getType(currentScope, codingConvention.getDelegateSuperclassName()));
    //     if (delegatorObject != null && delegateBaseObject != null && delegateSuperObject != null) {
    //       FunctionType delegatorCtor = delegatorObject.getConstructor();
    //       FunctionType delegateBaseCtor = delegateBaseObject.getConstructor();
    //       FunctionType delegateSuperCtor = delegateSuperObject.getConstructor();

    //       if (delegatorCtor != null && delegateBaseCtor != null && delegateSuperCtor != null) {
    //         FunctionParamBuilder functionParamBuilder = new FunctionParamBuilder(typeRegistry);
    //         functionParamBuilder.addRequiredParams(getNativeType(FUNCTION_TYPE));
    //         FunctionType findDelegate =
    //             typeRegistry.createFunctionType(
    //                 typeRegistry.createNullableType(delegateBaseObject),
    //                 functionParamBuilder.build());

    //         FunctionType delegateProxy =
    //             typeRegistry.createConstructorType(
    //                 delegateBaseObject.getReferenceName() + DELEGATE_PROXY_SUFFIX /* name */,
    //                 delegateBaseCtor.getSource() /* source */,
    //                 null /* parameters */,
    //                 null /* returnType */,
    //                 null /* templateKeys */,
    //                 false /* isAbstract */);
    //         delegateProxy.setPrototypeBasedOn(delegateBaseObject);

    //         codingConvention.applyDelegateRelationship(
    //             new NominalTypeBuilder(delegateSuperCtor, delegateSuperObject),
    //             new NominalTypeBuilder(delegateBaseCtor, delegateBaseObject),
    //             new NominalTypeBuilder(delegatorCtor, delegatorObject),
    //             (ObjectType) delegateProxy.getTypeOfThis(),
    //             findDelegate);
    //         delegateProxyCtors.add(delegateProxy);
    //       }
    //     }
    //   }

    //   /**
    //    * Declare the symbol for a qualified name in the global scope.
    //    *
    //    * @param info The doc info for this property.
    //    * @param n A top-level GETPROP node (it should not be contained inside another GETPROP).
    //    * @param parent The parent of {@code n}.
    //    * @param rhsValue The node that {@code n} is being initialized to, or {@code null} if this is a
    //    *     stub declaration.
    //    */
    //   void maybeDeclareQualifiedName(
    //       NodeTraversal t, JSDocInfo info, Node n, Node parent, Node rhsValue) {
    //     boolean isTypedef = isValidTypedefDeclaration(n, info);
    //     if (isTypedef) {
    //       declareTypedefType(n, info);
    //     }

    //     Node ownerNode = n.getFirstChild();
    //     String ownerName = ownerNode.getQualifiedName();
    //     String qName = n.getQualifiedName();
    //     String propName = n.getString();
    //     checkArgument(qName != null && ownerName != null);

    //     // Precedence of type information on GETPROPs:
    //     // 1) @type annotation / @enum annotation
    //     // 2) ASSIGN to FUNCTION literal
    //     // 3) @param/@return annotation (with no function literal)
    //     // 4) ASSIGN to something marked @const
    //     // 5) ASSIGN to anything else
    //     //
    //     // 1, 3, and 4 are declarations, 5 is inferred, and 2 is a declaration iff
    //     // the function has JsDoc or has not been declared before.
    //     //
    //     // FUNCTION literals are special because TypedScopeCreator is very smart
    //     // about getting as much type information as possible for them.

    //     // Determining type for #1 + #2 + #3 + #4
    //     JSType valueType = getDeclaredType(info, n, rhsValue, null);
    //     if (valueType == null && rhsValue != null) {
    //       // Determining type for #5
    //       valueType = rhsValue.getJSType();
    //     }

    //     // Function prototypes are special.
    //     // It's a common JS idiom to do:
    //     // F.prototype = { ... };
    //     // So if F does not have an explicitly declared super type,
    //     // allow F.prototype to be redefined arbitrarily.
    //     if ("prototype".equals(propName)) {
    //       TypedVar qVar = currentScope.getVar(qName);
    //       if (qVar != null) {
    //         // If the programmer has declared that F inherits from Super,
    //         // and they assign F.prototype to an object literal,
    //         // then they are responsible for making sure that the object literal's
    //         // implicit prototype is set up appropriately. We just obey
    //         // the @extends tag.
    //         ObjectType qVarType = ObjectType.cast(qVar.getType());
    //         if (qVarType != null && rhsValue != null && rhsValue.isObjectLit()) {
    //           typeRegistry.resetImplicitPrototype(
    //               rhsValue.getJSType(), qVarType.getImplicitPrototype());
    //         } else if (!qVar.isTypeInferred()) {
    //           // If the programmer has declared that F inherits from Super,
    //           // and they assign F.prototype to some arbitrary expression,
    //           // there's not much we can do. We just ignore the expression,
    //           // and hope they've annotated their code in a way to tell us
    //           // what props are going to be on that prototype.
    //           return;
    //         }

    //         qVar.getScope().undeclare(qVar);
    //       }
    //     }

    //     if (valueType == null) {
    //       if (parent.isExprResult()) {
    //         // t is mutable so make sure to capture the current state before the lambda.
    //         boolean isExtern = t.getInput() != null && t.getInput().isExtern();
    //         deferredActions.put(
    //             currentScope.getRootNode(), () -> resolveStubDeclaration(n, isExtern, ownerName));
    //       }

    //       return;
    //     }

    //     boolean inferred = isQualifiedNameInferred(qName, n, info, rhsValue, valueType);
    //     if (!inferred) {
    //       ObjectType ownerType = getObjectSlot(ownerName);
    //       if (ownerType != null) {
    //         declarePropertyIfNamespaceType(ownerType, ownerNode, propName, valueType, n);
    //       }

    //       // If the property is already declared, the error will be
    //       // caught when we try to declare it in the current scope.
    //       new SlotDefiner()
    //           .forDeclarationNode(n)
    //           .forVariableName(qName)
    //           .inScope(getLValueRootScope(n))
    //           .withType(valueType)
    //           .allowLaterTypeInference(inferred)
    //           .defineSlot();
    //     }
    //   }

    //    /// Determines whether a qualified name is inferred. NOTE(nicksantos): Determining whether a
    //    /// property is declared or not is really really obnoxious.
    //    ///
    //    /// <p>The problem is that there are two (equally valid) coding styles:
    //    ///
    //    /// <p>(function() { /* The authoritative definition of goog.bar. / goog.bar = function() {};
    //    /// })();
    //    ///
    //    /// <p>function f() { goog.bar(); /* Reset goog.bar to a no-op. / goog.bar = function() {}; }
    //    ///
    //    /// <p>In a dynamic language with first-class functions, it's very difficult to know which one
    //    /// the user intended without looking at lots of contextual information (the second example
    //    /// demonstrates a small case of this, but there are some really pathological cases as well).
    //    ///
    //    /// <p>The current algorithm checks if either the declaration has JsDoc type information,
    //    /// or @const with a known type, or a function literal with a name we haven't seen before.
    //    ///
    //   private boolean isQualifiedNameInferred(
    //       @Nullable String qName,
    //       Node n,
    //       @Nullable JSDocInfo info,
    //       @Nullable Node rhsValue,
    //       JSType valueType) {
    //     // Prototypes of constructors and interfaces are always declared.
    //     if (qName != null && qName.endsWith(".prototype")) {
    //       String className = qName.substring(0, qName.lastIndexOf(".prototype"));
    //       TypedVar slot = currentScope.getVar(className);
    //       JSType classType = slot == null ? null : slot.getType();
    //       if (classType != null && (classType.isConstructor() || classType.isInterface())) {
    //         return false;
    //       }
    //     }

    //     // If the jsdoc or RHS specifies a concrete type, it's not inferred.
    //     if ((info != null
    //             && (info.hasType()
    //                 || info.hasEnumParameterType()
    //                 || isValidTypedefDeclaration(n, info)
    //                 || FunctionTypeBuilder.isFunctionTypeDeclaration(info)
    //                 || (rhsValue != null && rhsValue.isFunction())))
    //         || isTypedConstantDeclaration(info, n, valueType)) {
    //       return false;
    //     }

    //     // At this point, we're pretty sure it's inferred, since there's neither
    //     // useful jsdoc info, nor a useful const or doc'd function RHS.  But
    //     // there's still one case where it may still not be: if the RHS is a
    //     // class or function that is not
    //     //   (1) a scoped qualified name (i.e. this.b.c or super.b.c),
    //     //   (2) already declared in a scope,
    //     //   (3) assigned in a conditional block, or
    //     //   (4) escaped to a closure,
    //     // then we treat it as if it is declared, rather than inferred.
    //     // Stubs and other values are always considered inferred at this point.
    //     if (rhsValue == null || (!rhsValue.isFunction() && !rhsValue.isClass())) {
    //       return true;
    //     }

    //     // "Scoped" qualified names (e.g. this.b.c or super.d) are inferred.
    //     if (!n.isUnscopedQualifiedName()) {
    //       return true;
    //     }

    //     // If this qname is already declared then treat this definition as inferred.
    //     TypedScope ownerScope = getLValueRootScope(n);
    //     if (ownerScope != null && ownerScope.hasOwnSlot(qName)) {
    //       return true;
    //     }

    //     // Check if this is in a conditional block.
    //     // Functions assigned in conditional blocks are inferred.
    //     if (hasControlStructureAncestor(n.getParent())) {
    //       return true;
    //     }

    //     // Check if this is assigned in an inner scope.
    //     // Functions assigned in inner scopes are inferred.
    //     if (ownerScope != null
    //         && escapedVarNames.contains(ScopedName.of(qName, ownerScope.getRootNode()))) {
    //       return true;
    //     }

    //     return false;
    //   }

    //   /**
    //    * Given a `goog.provide()` or legacy `goog.module()` call and implicit ProvidedName, declares
    //    * the name in the global scope.
    //    */
    //   void declareProvidedNs(Node provideCall, ProvidedName providedName) {
    //     // Redefine this name if we haven't already added a provide definition.
    //     // Note: in some cases, this will cause a redefinition error.
    //     ObjectType anonymousObjectType = typeRegistry.createAnonymousObjectType(null);
    //     new SlotDefiner()
    //         .inScope(currentScope.getGlobalScope())
    //         .allowLaterTypeInference(false)
    //         .forVariableName(providedName.getNamespace())
    //         .forDeclarationNode(provideCall)
    //         .withType(anonymousObjectType)
    //         .forGoogProvidedName()
    //         .defineSlot();

    //     QualifiedName namespace = QualifiedName.of(providedName.getNamespace());
    //     if (!namespace.isSimple()) {
    //       JSType ownerType = currentScope.lookupQualifiedName(namespace.getOwner());
    //       if (ownerType != null && ownerType.isObjectType()) {
    //         ownerType
    //             .toMaybeObjectType()
    //             .defineDeclaredProperty(namespace.getComponent(), anonymousObjectType, provideCall);
    //       }
    //     }
    //   }

    //   private boolean isTypedConstantDeclaration(JSDocInfo info, Node n, JSType valueType) {
    //     return (NodeUtil.isConstantDeclaration(info, n) || isGoogModuleExports(n))
    //         && valueType != null;
    //   }

    //   private boolean hasControlStructureAncestor(Node n) {
    //     while (!(n.isScript() || n.isFunction())) {
    //       if (NodeUtil.isControlStructure(n)) {
    //         return true;
    //       }
    //       n = n.getParent();
    //     }
    //     return false;
    //   }

    //   /**
    //    * Find the ObjectType associated with the given slot.
    //    *
    //    * @param slotName The name of the slot to find the type in.
    //    * @return An object type, or null if this slot does not contain an object.
    //    */
    //   private ObjectType getObjectSlot(String slotName) {
    //     TypedVar ownerVar = currentScope.getVar(slotName);
    //     if (ownerVar != null) {
    //       JSType ownerVarType = ownerVar.getType();
    //       return ObjectType.cast(
    //           ownerVarType == null ? null : ownerVarType.restrictByNotNullOrUndefined());
    //     }
    //     return null;
    //   }

    //   /**
    //    * When a class has a stub for a property, and the property exists on a super interface, use
    //    * that type.
    //    */
    //   private JSType getInheritedInterfacePropertyType(ObjectType obj, String propName) {
    //     if (obj != null && obj.isFunctionPrototypeType()) {
    //       FunctionType f = obj.getOwnerFunction();
    //       for (ObjectType i : f.getImplementedInterfaces()) {
    //         if (i.hasProperty(propName)) {
    //           return i.getPropertyType(propName);
    //         }
    //       }
    //     }
    //     return null;
    //   }

    //   /**
    //    * Resolve any type-less stub declarations to unknown types if we could not find types for them
    //    * during traversal. This method is only called as a deferred action after the root node is
    //    * visted.
    //    */
    //   void resolveStubDeclaration(Node n, boolean isExtern, String ownerName) {
    //     String qName = n.getQualifiedName();
    //     String propName = n.getString();

    //     // TODO(b/111216910): should this be getLValueRoot(n).hasOwnSlot(qName)?
    //     if (currentScope.hasOwnSlot(qName)) {
    //       return;
    //     }

    //     // If we see a stub property, make sure to register this property
    //     // in the type registry.
    //     ObjectType ownerType = getObjectSlot(ownerName);
    //     JSType inheritedType = getInheritedInterfacePropertyType(ownerType, propName);
    //     JSType stubType = inheritedType == null ? unknownType : inheritedType;
    //     new SlotDefiner()
    //         .forDeclarationNode(n)
    //         .readVariableNameFromDeclarationNode()
    //         .inScope(getLValueRootScope(n))
    //         .withType(stubType)
    //         .allowLaterTypeInference(true)
    //         .defineSlot();

    //     if (ownerType != null && (isExtern || ownerType.isFunctionPrototypeType())) {
    //       // If this is a stub for a prototype, just declare it
    //       // as an unknown type. These are seen often in externs.
    //       ownerType.defineInferredProperty(propName, stubType, n);
    //     } else {
    //       typeRegistry.registerPropertyOnType(propName, ownerType == null ? stubType : ownerType);
    //     }
    //   }

    //   /**
    //    * Returns whether this is a valid declaration of a @typedef.
    //    *
    //    * @param candidate A qualified name node.
    //    * @param info JSDoc comments.
    //    */
    //   private boolean isValidTypedefDeclaration(Node candidate, @Nullable JSDocInfo info) {
    //     if (info == null || !info.hasTypedefType()) {
    //       return false;
    //     }
    //     // `isUnscopedQualifiedName` excludes `this` and `super` properties.
    //     return candidate.isUnscopedQualifiedName() && !NodeUtil.isPrototypeProperty(candidate);
    //   }

    //   /** Declares a typedef'd name in the {@link JSTypeRegistry}. */
    //   void declareTypedefType(Node candidate, JSDocInfo info) {
    //     String typedef = candidate.getQualifiedName();

    //     // TODO(nicksantos|user): This is a terrible, terrible hack
    //     // to bail out on recursive typedefs. We'll eventually need
    //     // to handle these properly.
    //     typeRegistry.declareType(currentScope, typedef, unknownType);

    //     JSType realType = info.getTypedefType().evaluate(currentScope, typeRegistry);
    //     if (realType == null) {
    //       report(JSError.make(candidate, MALFORMED_TYPEDEF, typedef));
    //     } else {
    //       candidate.setTypedefTypeProp(realType);
    //     }

    //     typeRegistry.overwriteDeclaredType(currentScope, typedef, realType);
    //   }

    //   void declarePropertyIfNamespaceType(
    //       ObjectType ownerType,
    //       Node ownerNode,
    //       String propName,
    //       JSType valueType,
    //       Node declarationNode) {
    //     // Only declare this as an official property if it has not been
    //     // declared yet.
    //     if (ownerType.hasOwnProperty(propName) && !ownerType.isPropertyTypeInferred(propName)) {
    //       return;
    //     }
    //     // Define the property if any of the following are true:
    //     //   (1) it's a non-native extern type. Native types are excluded here because we don't
    //     //       want externs of the form "/** @type {!Object} */ var api = {}; api.foo;" to
    //     //       cause a property "foo" to be declared on Object.
    //     //   (2) it's a non-instance type. This primarily covers static properties on
    //     //       constructors (which are FunctionTypes, not InstanceTypes).
    //     //   (3) it's an assignment to 'this', which covers instance properties assigned in
    //     //       constructors or other methods.
    //     boolean isNonNativeExtern =
    //         getCompilerInput() != null
    //             && getCompilerInput().isExtern()
    //             && !ownerType.isNativeObjectType();
    //     if (isNonNativeExtern || !ownerType.isInstanceType() || ownerNode.isThis()) {
    //       // If the property is undeclared or inferred, declare it now.
    //       ownerType.defineDeclaredProperty(propName, valueType, declarationNode);
    //     }
    //   }
}

struct AbstractScopeBuilderVisitor<'tcx, 'c, S, V>
where
    S: FnMut(Option<AstNode<'tcx>>, AstNode<'tcx>, Option<AstNode<'tcx>>) -> bool,
    V: FnMut(Option<AstNode<'tcx>>, AstNode<'tcx>, Option<AstNode<'tcx>>),
{
    ctx: &'c Ctx<'tcx>,
    should_traverse: S,
    visit_fn: V,

    stack: Vec<AstNode<'tcx>>,
    scope_root_stack: Vec<AstNode<'tcx>>,
}

impl<'tcx, 'c, S, V> AbstractScopeBuilderVisitor<'tcx, 'c, S, V>
where
    S: FnMut(Option<AstNode<'tcx>>, AstNode<'tcx>, Option<AstNode<'tcx>>) -> bool,
    V: FnMut(Option<AstNode<'tcx>>, AstNode<'tcx>, Option<AstNode<'tcx>>),
{
    pub fn new(ctx: &'c Ctx<'tcx>, should_traverse: S, visit_fn: V) -> Self {
        Self {
            ctx,
            should_traverse,
            visit_fn,
            // 16 is an arbitrary capacity; this is just to avoid the first few
            // allocations.
            stack: Vec::with_capacity(16),
            scope_root_stack: Vec::with_capacity(2),
        }
    }

    /**
     * Traverses a parse tree recursively with a scope, starting at that scope's root. Omits children
     * of the scope root that are traversed in the outer scope (specifically, non-bleeding function
     * and class name nodes, class extends clauses, and computed property keys).
     */
    pub fn traverseAtScope(&mut self, s: &TypedScope<'tcx>) {
        let n = s.getRootNode().node;

        if let Some(parent_scope_id) = s.get_parent() {
            let parent_root_node = self.ctx.scopes[parent_scope_id].getRootNode();
            self.scope_root_stack.push(parent_root_node.node);
        }

        match n {
            AstNode::FnDecl(f) => {
                if (self.should_traverse)(self.scope_root_stack.last().map(|n| *n), n, None) {
                    self.scope_root_stack.push(s.getRootNode().node);

                    f.function.params.visit_with(self);
                    f.function.body.visit_with(self);

                    self.scope_root_stack.pop();
                    (self.visit_fn)(self.scope_root_stack.last().map(|n| *n), n, None);
                }
            }
            AstNode::FnExpr(f) => {
                if (self.should_traverse)(self.scope_root_stack.last().map(|n| *n), n, None) {
                    self.scope_root_stack.push(s.getRootNode().node);

                    // Only traverse the function name if it's a bleeding function expression name.
                    f.ident.visit_with(self);
                    f.function.params.visit_with(self);
                    f.function.body.visit_with(self);

                    self.scope_root_stack.pop();
                    (self.visit_fn)(self.scope_root_stack.last().map(|n| *n), n, None);
                }
            }
            AstNode::ClassDecl(c) => {
                if (self.should_traverse)(self.scope_root_stack.last().map(|n| *n), n, None) {
                    self.scope_root_stack.push(s.getRootNode().node);

                    // Omit the extends node, which is in the outer scope. Computed property keys are already
                    // excluded by handleClassMembers.
                    // TODO: are they?
                    c.class.body.visit_with(self);

                    self.scope_root_stack.pop();
                    (self.visit_fn)(self.scope_root_stack.last().map(|n| *n), n, None);
                }
            }
            AstNode::ClassExpr(c) => {
                if (self.should_traverse)(self.scope_root_stack.last().map(|n| *n), n, None) {
                    self.scope_root_stack.push(s.getRootNode().node);

                    // Only traverse the class name if it's a bleeding class expression name.
                    c.ident.visit_with(self);
                    // Omit the extends node, which is in the outer scope. Computed property keys are already
                    // excluded by handleClassMembers.
                    // TODO: are they?
                    c.class.body.visit_with(self);

                    self.scope_root_stack.pop();
                    (self.visit_fn)(self.scope_root_stack.last().map(|n| *n), n, None);
                }
            }
            AstNode::BlockStmt(b) => {
                if (self.should_traverse)(self.scope_root_stack.last().map(|n| *n), n, None) {
                    self.scope_root_stack.push(s.getRootNode().node);

                    b.visit_children_with(self);

                    self.scope_root_stack.pop();
                    (self.visit_fn)(self.scope_root_stack.last().map(|n| *n), n, None);
                }
            }
            AstNode::ForStmt(_) => {
                // TODO: in closure, this is handled with the next block of
                // logic. This looks incorrect as the logic appears to be
                // designed for enhanced for-loops only - it only handles the
                // first three children; enhanced for-loops have 3 children
                // while normal for-loops have 4. This means that body of a
                // normal for-loop won't be visited (because it's the 4th child).
                // It looks like the condition was updated to include normal
                // for-loops here: https://github.com/google/closure-compiler/commit/716b57cdf3db6374434cc2a52a6e6922f0018148
                // but the logic in the condition body was not updated.
                todo!();
            }
            AstNode::ForInStmt(ForInStmt {
                left, right, body, ..
            })
            | AstNode::ForOfStmt(ForOfStmt {
                left, right, body, ..
            }) => {
                if (self.should_traverse)(self.scope_root_stack.last().map(|n| *n), n, None) {
                    self.scope_root_stack.push(s.getRootNode().node);

                    left.visit_with(self);
                    right.visit_with(self);
                    body.visit_with(self);

                    self.scope_root_stack.pop();
                    (self.visit_fn)(self.scope_root_stack.last().map(|n| *n), n, None);
                }
            }
            AstNode::SwitchStmt(switch) => {
                if (self.should_traverse)(self.scope_root_stack.last().map(|n| *n), n, None) {
                    self.scope_root_stack.push(s.getRootNode().node);

                    switch.visit_children_with(self);

                    self.scope_root_stack.pop();
                    (self.visit_fn)(self.scope_root_stack.last().map(|n| *n), n, None);
                }
            }
            _ => {
                // debug_assert!(
                //     s.isGlobal() || s.isModuleScope(),
                //     "Expected global or module scope."
                // );
                self.traverseWithScope(n, s);
            }
        }
    }

    /**
     * Traverses a parse tree recursively with a scope, starting with the given root. This should only
     * be used in the global scope or module scopes. Otherwise, use {@link #traverseAtScope}.
     */
    fn traverseWithScope(&mut self, root: AstNode<'tcx>, s: &TypedScope) {
        // debug_assert!(
        //     s.isGlobal() || s.isModuleScope(),
        //     "Expected global or module scope."
        // );

        self.scope_root_stack.push(root);
        root.visit_with(self);
        self.scope_root_stack.pop();
    }
}

macro_rules! generate_visitors {
    ([$([$name:ident, $N:tt]$(,)?)*]) => {
        $(
            #[inline]
            fn $name(&mut self, n: &'ast $N) {
                let node = AstNode::from(n);

                let scope = self.scope_root_stack.last().map(|n| *n);

                if !(self.should_traverse)(scope, node, self.stack.last().map(|n|*n)) {
                    return;
                }

                self.stack.push(node);

                n.visit_children_with(self);

                self.stack.pop();

                (self.visit_fn)(scope, node, self.stack.last().map(|n|*n));
            }
        )*

    };
}

impl<'ast, 'c, S, V> Visit<'ast> for AbstractScopeBuilderVisitor<'ast, 'c, S, V>
where
    S: FnMut(Option<AstNode<'ast>>, AstNode<'ast>, Option<AstNode<'ast>>) -> bool,
    V: FnMut(Option<AstNode<'ast>>, AstNode<'ast>, Option<AstNode<'ast>>),
{
    noop_visit_type!();
    generate_visitors!([
        [visit_class, Class],
        [visit_class_prop, ClassProp],
        [visit_private_prop, PrivateProp],
        [visit_class_method, ClassMethod],
        [visit_private_method, PrivateMethod],
        [visit_constructor, Constructor],
        [visit_decorator, Decorator],
        [visit_fn_decl, FnDecl],
        [visit_class_decl, ClassDecl],
        [visit_var_decl, VarDecl],
        [visit_var_declarator, VarDeclarator],
        [visit_this_expr, ThisExpr],
        [visit_array_lit, ArrayLit],
        [visit_object_lit, ObjectLit],
        [visit_spread_element, SpreadElement],
        [visit_unary_expr, UnaryExpr],
        [visit_update_expr, UpdateExpr],
        [visit_bin_expr, BinExpr],
        [visit_fn_expr, FnExpr],
        [visit_class_expr, ClassExpr],
        [visit_assign_expr, AssignExpr],
        [visit_member_expr, MemberExpr],
        [visit_cond_expr, CondExpr],
        [visit_call_expr, CallExpr],
        [visit_new_expr, NewExpr],
        [visit_seq_expr, SeqExpr],
        [visit_arrow_expr, ArrowExpr],
        [visit_yield_expr, YieldExpr],
        [visit_meta_prop_expr, MetaPropExpr],
        [visit_await_expr, AwaitExpr],
        [visit_tpl, Tpl],
        [visit_tagged_tpl, TaggedTpl],
        [visit_tpl_element, TplElement],
        [visit_paren_expr, ParenExpr],
        [visit_super, Super],
        [visit_expr_or_spread, ExprOrSpread],
        [visit_opt_chain_expr, OptChainExpr],
        [visit_function, Function],
        [visit_param, Param],
        [visit_binding_ident, BindingIdent],
        [visit_ident, Ident],
        [visit_private_name, PrivateName],
        [visit_jsx_member_expr, JSXMemberExpr],
        [visit_jsx_namespaced_name, JSXNamespacedName],
        [visit_jsx_empty_expr, JSXEmptyExpr],
        [visit_jsx_expr_container, JSXExprContainer],
        [visit_jsx_spread_child, JSXSpreadChild],
        [visit_jsx_opening_element, JSXOpeningElement],
        [visit_jsx_closing_element, JSXClosingElement],
        [visit_jsx_attr, JSXAttr],
        [visit_jsx_text, JSXText],
        [visit_jsx_element, JSXElement],
        [visit_jsx_fragment, JSXFragment],
        [visit_jsx_opening_fragment, JSXOpeningFragment],
        [visit_jsx_closing_fragment, JSXClosingFragment],
        [visit_invalid, Invalid],
        [visit_big_int, BigInt],
        [visit_str, Str],
        [visit_bool, Bool],
        [visit_null, Null],
        [visit_regex, Regex],
        [visit_number, Number],
        [visit_module, Module],
        [visit_script, Script],
        [visit_export_default_expr, ExportDefaultExpr],
        [visit_export_decl, ExportDecl],
        [visit_import_decl, ImportDecl],
        [visit_export_all, ExportAll],
        [visit_named_export, NamedExport],
        [visit_export_default_decl, ExportDefaultDecl],
        [visit_import_default_specifier, ImportDefaultSpecifier],
        [visit_import_star_as_specifier, ImportStarAsSpecifier],
        [visit_import_named_specifier, ImportNamedSpecifier],
        [visit_export_namespace_specifier, ExportNamespaceSpecifier],
        [visit_export_default_specifier, ExportDefaultSpecifier],
        [visit_export_named_specifier, ExportNamedSpecifier],
        [visit_array_pat, ArrayPat],
        [visit_object_pat, ObjectPat],
        [visit_assign_pat, AssignPat],
        [visit_rest_pat, RestPat],
        [visit_key_value_pat_prop, KeyValuePatProp],
        [visit_assign_pat_prop, AssignPatProp],
        [visit_key_value_prop, KeyValueProp],
        [visit_assign_prop, AssignProp],
        [visit_getter_prop, GetterProp],
        [visit_setter_prop, SetterProp],
        [visit_method_prop, MethodProp],
        [visit_computed_prop_name, ComputedPropName],
        [visit_block_stmt, BlockStmt],
        [visit_expr_stmt, ExprStmt],
        [visit_empty_stmt, EmptyStmt],
        [visit_debugger_stmt, DebuggerStmt],
        [visit_with_stmt, WithStmt],
        [visit_return_stmt, ReturnStmt],
        [visit_labeled_stmt, LabeledStmt],
        [visit_break_stmt, BreakStmt],
        [visit_continue_stmt, ContinueStmt],
        [visit_if_stmt, IfStmt],
        [visit_switch_stmt, SwitchStmt],
        [visit_throw_stmt, ThrowStmt],
        [visit_try_stmt, TryStmt],
        [visit_while_stmt, WhileStmt],
        [visit_do_while_stmt, DoWhileStmt],
        [visit_for_stmt, ForStmt],
        [visit_for_in_stmt, ForInStmt],
        [visit_for_of_stmt, ForOfStmt],
        [visit_switch_case, SwitchCase],
        [visit_catch_clause, CatchClause],
        // [visit_ts_type_ann, TsTypeAnn],
        // [visit_ts_type_param_decl, TsTypeParamDecl],
        // [visit_ts_type_param, TsTypeParam],
        // [visit_ts_type_param_instantiation, TsTypeParamInstantiation],
        // [visit_ts_param_prop, TsParamProp],
        // [visit_ts_qualified_name, TsQualifiedName],
        // [visit_ts_call_signature_decl, TsCallSignatureDecl],
        // [visit_ts_construct_signature_decl, TsConstructSignatureDecl],
        // [visit_ts_property_signature, TsPropertySignature],
        // [visit_ts_getter_signature, TsGetterSignature],
        // [visit_ts_setter_signature, TsSetterSignature],
        // [visit_ts_method_signature, TsMethodSignature],
        // [visit_ts_index_signature, TsIndexSignature],
        // [visit_ts_keyword_type, TsKeywordType],
        // [visit_ts_this_type, TsThisType],
        // [visit_ts_fn_type, TsFnType],
        // [visit_ts_constructor_type, TsConstructorType],
        // [visit_ts_type_ref, TsTypeRef],
        // [visit_ts_type_predicate, TsTypePredicate],
        // [visit_ts_type_query, TsTypeQuery],
        // [visit_ts_import_type, TsImportType],
        // [visit_ts_type_lit, TsTypeLit],
        // [visit_ts_array_type, TsArrayType],
        // [visit_ts_tuple_type, TsTupleType],
        // [visit_ts_tuple_element, TsTupleElement],
        // [visit_ts_optional_type, TsOptionalType],
        // [visit_ts_rest_type, TsRestType],
        // [visit_ts_union_type, TsUnionType],
        // [visit_ts_intersection_type, TsIntersectionType],
        // [visit_ts_conditional_type, TsConditionalType],
        // [visit_ts_infer_type, TsInferType],
        // [visit_ts_parenthesized_type, TsParenthesizedType],
        // [visit_ts_type_operator, TsTypeOperator],
        // [visit_ts_indexed_access_type, TsIndexedAccessType],
        // [visit_ts_mapped_type, TsMappedType],
        // [visit_ts_lit_type, TsLitType],
        // [visit_ts_tpl_lit_type, TsTplLitType],
        // [visit_ts_interface_decl, TsInterfaceDecl],
        // [visit_ts_interface_body, TsInterfaceBody],
        // [visit_ts_expr_with_type_args, TsExprWithTypeArgs],
        // [visit_ts_type_alias_decl, TsTypeAliasDecl],
        // [visit_ts_enum_decl, TsEnumDecl],
        // [visit_ts_enum_member, TsEnumMember],
        // [visit_ts_module_decl, TsModuleDecl],
        // [visit_ts_module_block, TsModuleBlock],
        // [visit_ts_namespace_decl, TsNamespaceDecl],
        // [visit_ts_import_equals_decl, TsImportEqualsDecl],
        // [visit_ts_external_module_ref, TsExternalModuleRef],
        // [visit_ts_export_assignment, TsExportAssignment],
        // [visit_ts_namespace_export_decl, TsNamespaceExportDecl],
        // [visit_ts_as_expr, TsAsExpr],
        // [visit_ts_type_assertion, TsTypeAssertion],
        // [visit_ts_non_null_expr, TsNonNullExpr],
        // [visit_ts_const_assertion, TsConstAssertion],
    ]);
}

struct FirstOrderFunctionAnalyzer<'tcx, 'sc, F>
where
    F: FnMut(&SyntacticScope<'tcx>),
{
    scope_callback: F,
    scope_creator: &'sc mut SyntacticScopeCreator,

    /// Stack containing the Scopes that have been created. The Scope objects are lazily created; so
    /// the {@code scopeRoots} stack contains the Nodes for all Scopes that have not been created yet.
    scopes: Vec<SyntacticScope<'tcx>>,
}

impl<'tcx, 'sc, F> FirstOrderFunctionAnalyzer<'tcx, 'sc, F>
where
    F: FnMut(&SyntacticScope<'tcx>),
{
    pub fn new(scope_creator: &'sc mut SyntacticScopeCreator, on_enter_scope: F) -> Self {
        Self {
            scope_callback: on_enter_scope,
            scope_creator,

            scopes: Vec::new(),
        }
    }

    // TODO: re-port this from closure to account for externs:
    fn traverseRoots(&mut self, /*externs: AstNode,*/ root: AstNode<'tcx>) {
        // Node scopeRoot = externs.getParent();
        // checkNotNull(scopeRoot);

        self.create_scope(root);

        // traverseBranch(externs, scopeRoot);
        // checkState(root.getParent() == scopeRoot);
        root.visit_with(self);

        self.pop_scope();
    }

    /// Creates a new scope (e.g. when entering a function).
    fn create_scope(&mut self, node: AstNode<'tcx>) {
        let parent = self.scopes.last();
        // TODO: is_function_block bool:
        let scope = self.scope_creator.createScope(node, false, parent);
        self.scopes.push(scope);

        (self.scope_callback)(self.scopes.last().unwrap());
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
impl<'tcx, 'sc, 's, F> Visit<'tcx> for FirstOrderFunctionAnalyzer<'tcx, 'sc, F>
where
    F: FnMut(&SyntacticScope<'tcx>),
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
