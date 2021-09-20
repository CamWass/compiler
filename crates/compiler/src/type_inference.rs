
use ast::AstNode;
use ast2::NodeId;

use crate::{
    control_flow_graph::{Annotation, Branch, ControlFlowGraph, DummyAnnotation},
    linked_flow_scope::{FlowScopeJoinOp, LinkedFlowScope},
    typed_scope::{TypedScope,ScopeId},
    typing::{types::Ty, BooleanLiteralSet},
};

pub struct TypeInference<'ast, 'tcx> {
    cfg: ControlFlowGraph<'ast, DummyAnnotation, DummyAnnotation>,
    bottomScope: LinkedFlowScope<'tcx>,
    // either the global scope or a function scope
    containerScope: ScopeId,
}

impl<'ast, 'tcx> TypeInference<'ast, 'tcx> {
    pub fn new(
        // compiler: AbstractCompiler,
        cfg: ControlFlowGraph<'ast, DummyAnnotation, DummyAnnotation>,
        // reverseInterpreter: ReverseAbstractInterpreter,
        syntacticScope: ScopeId,
        // scopeCreator: TypedScopeCreator,
        // assertionFunctionLookup: AssertionFunctionLookup,
    ) -> Self {
        // super(cfg);
        // checkArgument(
        //     syntacticScope.isGlobal() || syntacticScope.isFunctionScope(),
        //     "Expected global or function scope, got %s",
        //     syntacticScope);
        // this.compiler = checkNotNull(compiler);
        // this.registry = checkNotNull(compiler.getTypeRegistry());
        // this.reverseInterpreter = checkNotNull(reverseInterpreter);
        // this.moduleImportResolver =
        //     new ModuleImportResolver(
        //         compiler.getModuleMap(), scopeCreator.getNodeToScopeMapper(), this.registry);
        // this.containerScope = syntacticScope;
        // this.scopeCreator = checkNotNull(scopeCreator);
        // this.assertionFunctionLookup = checkNotNull(assertionFunctionLookup);

        // this.unknownType = registry.getNativeObjectType(UNKNOWN_TYPE);
        // this.numberAdditionSupertype =
        //     registry.createUnionType(
        //         VOID_TYPE,
        //         NULL_TYPE,
        //         NUMBER_TYPE,
        //         NUMBER_OBJECT_TYPE,
        //         BOOLEAN_TYPE,
        //         BOOLEAN_OBJECT_TYPE);

        // this.bottomScope =
        //     LinkedFlowScope.createEntryLattice(
        //         compiler, TypedScope.createLatticeBottom(syntacticScope.getRootNode()));

        todo!();
        // Self {
        //     cfg,
        //     containerScope: syntacticScope,
        //     bottomScope: LinkedFlowScope::createEntryLattice(
        //         // compiler,
        //         &TypedScope::createLatticeBottom(syntacticScope.getRootNode()),
        //     ),
        // }
    }

    fn inferDeclarativelyUnboundVarsWithoutTypes(
        &mut self,
        mut flow: LinkedFlowScope,
    ) -> LinkedFlowScope {
        todo!();
        // let scope = flow.getDeclarationScope();
        // if !self.inferredUnboundVars.add(scope) {
        //     return flow;
        // }
        // // For each local variable declared with the VAR keyword, the entry
        // // type is VOID.
        // for var in scope.getDeclarativelyUnboundVarsWithoutTypes() {
        //     if self.isUnflowable(var) {
        //         continue;
        //     }

        //     todo!();
        //     // flow = flow.inferSlotType(var.getName(), self.getNativeType(VOID_TYPE));
        // }
        // flow
    }

    /** Infers all of a function's parameters if their types aren't declared. */
    fn inferParameters(&mut self, entryFlowScope: LinkedFlowScope) -> LinkedFlowScope {
        todo!();
        // Node functionNode = containerScope.getRootNode();
        // if (!functionNode.isFunction()) {
        //   return entryFlowScope; // we're in the global scope
        // } else if (NodeUtil.isBundledGoogModuleCall(functionNode.getParent())) {
        //   // Pretend the function literal in `goog.loadModule(function(exports) {` does not exist.
        //   return entryFlowScope;
        // }
        // Node astParameters = functionNode.getSecondChild();
        // Node iifeArgumentNode = null;

        // if (NodeUtil.isInvocationTarget(functionNode)) {
        //   iifeArgumentNode = functionNode.getNext();
        // }

        // FunctionType functionType = JSType.toMaybeFunctionType(functionNode.getJSType());
        // Iterator<Parameter> parameterTypes = functionType.getParameters().iterator();
        // Parameter parameter = parameterTypes.hasNext() ? parameterTypes.next() : null;

        // // This really iterates over three different things at once:
        // //   - the actual AST parameter nodes (which may be REST, DEFAULT_VALUE, etc.)
        // //   - the argument nodes in an IIFE
        // //   - the parameter type nodes from the FunctionType on the FUNCTION node
        // // Always visit every AST parameter once, regardless of how many IIFE arguments or
        // // FunctionType param nodes there are.
        // for (Node astParamItr = astParameters.getFirstChild();
        //     astParamItr != null;
        //     astParamItr = astParamItr.getNext()) {
        //   Node astParam = astParamItr;
        //   if (iifeArgumentNode != null && iifeArgumentNode.isSpread()) {
        //     // block inference on all parameters that might possibly be set by a spread, e.g. `z` in
        //     // (function f(x, y, z = 1))(...[1, 2], 'foo')
        //     iifeArgumentNode = null;
        //   }

        //   // Running variable for the type of the param within the body of the function. We use the
        //   // existing type on the param node as the default, and then transform it according to the
        //   // declaration syntax.
        //   JSType inferredType = getJSType(astParam);

        //   if (iifeArgumentNode != null) {
        //     if (iifeArgumentNode.getJSType() != null) {
        //       inferredType = iifeArgumentNode.getJSType();
        //     }
        //   } else if (parameter != null) {
        //     if (parameter.getJSType() != null) {
        //       inferredType = parameter.getJSType();
        //     }
        //   }

        //   Node defaultValue = null;
        //   if (astParam.isDefaultValue()) {
        //     defaultValue = astParam.getSecondChild();
        //     // must call `traverse` to correctly type the default value
        //     entryFlowScope = traverse(defaultValue, entryFlowScope);
        //     astParam = astParam.getFirstChild();
        //   } else if (astParam.isRest()) {
        //     // e.g. `function f(p1, ...restParamName) {}`
        //     // set astParam = restParamName
        //     astParam = astParam.getOnlyChild();
        //     // convert 'number' into 'Array<number>' for rest parameters
        //     inferredType =
        //         registry.createTemplatizedType(registry.getNativeObjectType(ARRAY_TYPE), inferredType);
        //   }

        //   if (defaultValue != null) {
        //     // The param could possibly be the default type, and `undefined` args won't propagate in.
        //     inferredType =
        //         registry.createUnionType(
        //             inferredType.restrictByNotUndefined(), getJSType(defaultValue));
        //   }

        //   if (astParam.isDestructuringPattern()) {
        //     // even if the inferredType is null, we still need to type all the nodes inside the
        //     // destructuring pattern. (e.g. in computed properties or default value expressions)
        //     entryFlowScope = updateDestructuringParameter(astParam, inferredType, entryFlowScope);
        //   } else {
        //     // for simple named parameters, we only need to update the scope/AST if we have a new
        //     // inferred type.
        //     entryFlowScope =
        //         updateNamedParameter(astParam, defaultValue != null, inferredType, entryFlowScope);
        //   }

        //   parameter = parameterTypes.hasNext() ? parameterTypes.next() : null;
        //   iifeArgumentNode = iifeArgumentNode != null ? iifeArgumentNode.getNext() : null;
        // }

        // return entryFlowScope;
    }

    /**
     * Sets the types of a un-named/destructuring function parameter to an inferred type.
     *
     * <p>This method is responsible for typing:
     *
     * <ul>
     *   <li>The scope slot
     *   <li>The pattern nodes
     * </ul>
     */
    fn updateDestructuringParameter(
        &mut self,
        pattern: NodeId,
        inferredType: Ty,
        entryFlowScope: LinkedFlowScope,
    ) -> LinkedFlowScope {
        todo!();
        //  // look through all expressions and lvalues in the pattern.
        //  // given an lvalue, change its type if either a) it's inferred (not declared in
        //  // TypedScopeCreator) or b) it has a default value
        //  entryFlowScope =
        //      traverseDestructuringPatternHelper(
        //          pattern,
        //          entryFlowScope,
        //          inferredType,
        //          (FlowScope scope, Node lvalue, JSType type) -> {
        //            TypedVar var = containerScope.getVar(lvalue.getString());
        //            checkNotNull(var);
        //            // This condition will trigger on cases like
        //            //   (function f({x}) {})({x: 3})
        //            // where `x` is of unknown type during the typed scope creation phase, but
        //            // here we can infer that it is of type `number`
        //            if (var.isTypeInferred()) {
        //              var.setType(type);
        //              lvalue.setJSType(type);
        //            }
        //            if (lvalue.getParent().isDefaultValue()) {
        //              // Given
        //              //   /** @param {{age: (number|undefined)}} data */
        //              //   function f({age = 99}) {}
        //              // infer that `age` is now a `number` and not `number|undefined`
        //              // treat this similarly to if there was an assignment inside the function body
        //              // TODO(b/117162687): allow people to narrow the declared type to
        //              // exclude 'undefined' inside the function body.
        //              scope = updateScopeForAssignment(scope, lvalue, type, AssignmentType.ASSIGN);
        //            }
        //            return scope;
        //          });

        //  return entryFlowScope;
    }

    /**
     * Sets the types of a named/non-destructuring function parameter to an inferred type.
     *
     * <p>This method is responsible for typing:
     *
     * <ul>
     *   <li>The scope slot
     *   <li>The param node
     * </ul>
     */
    fn updateNamedParameter(
        &mut self,
        paramName: NodeId,
        hasDefaultValue: bool,
        inferredType: Ty,
        entryFlowScope: LinkedFlowScope,
    ) -> LinkedFlowScope {
        todo!();
        // let var = match self.containerScope.getVar(paramName.getString()) {
        //     Some(var) => var,
        //     None => unreachable!("Missing var for parameter {:?}", paramName),
        // };

        // paramName.setJSType(inferredType);

        // if var.isTypeInferred() {
        //     var.ty = Some(inferredType);
        // } else if hasDefaultValue {
        //     // If this is a declared type with a default value, update the LinkedFlowScope slots but not
        //     // the actual TypedVar. This is similar to what would happen if the default value was moved
        //     // into an assignment in the fn body
        //     entryFlowScope = self.redeclareSimpleVar(entryFlowScope, paramName, inferredType);
        // }
        // entryFlowScope
    }

    fn createInitialEstimateLattice(&self) -> &LinkedFlowScope<'tcx> {
        &self.bottomScope
    }

    fn createEntryLattice(&mut self) -> LinkedFlowScope {
        todo!();
        // // only ever called once so we don't need to cache this computation
        // let entryScope = self.inferDeclarativelyUnboundVarsWithoutTypes(
        //     LinkedFlowScope::createEntryLattice(&self.containerScope),
        // );

        // self.inferParameters(entryScope)
    }

    fn flowThrough(&mut self, n: NodeId, input: LinkedFlowScope) -> LinkedFlowScope {
        todo!();
        // // If we have not walked a path from <entry> to <n>, then we don't
        // // want to infer anything about this scope.
        // if input == self.bottomScope {
        //   return input;
        // }

        // // This method also does some logic for ES modules right before and after entering/exiting the
        // // scope rooted at the module. The reasoning for separating out this logic is that we can just
        // // ignore the actual AST nodes for IMPORT/EXPORT, in most cases, because we have already
        // // created an abstraction of imports and exports.
        // Node root = NodeUtil.getEnclosingScopeRoot(n);
        // // Inferred types of ES module imports/exports aren't knowable until after TypeInference runs.
        // // First update the type of all imports in the scope, then do flow-sensitive inference, then
        // // update the implicit '*exports*' object.
        // Module module =
        //     ModuleImportResolver.getModuleFromScopeRoot(compiler.getModuleMap(), compiler, root);
        // TypedScope syntacticBlockScope = scopeCreator.createScope(root);
        // if (module != null && module.metadata().isEs6Module()) {
        //   moduleImportResolver.declareEsModuleImports(
        //       module, syntacticBlockScope, compiler.getInput(NodeUtil.getInputId(n)));
        // }

        // // This logic is not specific to ES modules.
        // FlowScope output = input.withSyntacticScope(syntacticBlockScope);
        // output = inferDeclarativelyUnboundVarsWithoutTypes(output);
        // output = traverse(n, output);

        // updateModuleScope(module, syntacticBlockScope);
        // return output;
    }

    fn updateModuleScope() {
        todo!()
    }

    fn isForward(&self) -> bool {
        true
    }

    fn isBranched(&self) -> bool {
        true
    }

    fn createFlowJoiner(&self) -> FlowScopeJoinOp {
        FlowScopeJoinOp::new()
    }

    fn createFlowBrancher<'s>(source: NodeId, output: LinkedFlowScope) -> FlowBrancher<'tcx, 's> {
        FlowBrancher::new(source, output)
    }

    fn traverse(&mut self, n: NodeId, scope: LinkedFlowScope) -> LinkedFlowScope {
        todo!();
        // let mut isTypeable = true;
        // match n {
        //     //   case ASSIGN:
        //     //     scope = traverseAssign(n, scope);
        //     //     break;
        //     NodeId::Ident(i) => {
        //         scope = self.traverseName(i, scope);
        //     }

        //     //   case OPTCHAIN_GETPROP:
        //     //   case OPTCHAIN_CALL:
        //     //   case OPTCHAIN_GETELEM:
        //     //     scope = traverseOptChain(n, scope);
        //     //     break;

        //     //   case GETPROP:
        //     //     scope = traverseGetProp(n, scope);
        //     //     break;

        //     //   case CLASS:
        //     //     scope = traverseClass(n, scope);
        //     //     break;

        //     //   case ASSIGN_AND:
        //     //   case ASSIGN_OR:
        //     //     scope = traverseShortCircuitingBinOpAssignment(n, scope);
        //     //     break;

        //     //   case AND:
        //     //     scope = traverseAnd(n, scope).getJoinedFlowScope();
        //     //     break;

        //     //   case OR:
        //     //     scope = traverseOr(n, scope).getJoinedFlowScope();
        //     //     break;

        //     //   case ASSIGN_COALESCE:
        //     //   case COALESCE:
        //     //     scope = traverseNullishCoalesce(n, scope);
        //     //     break;

        //     //   case HOOK:
        //     //     scope = traverseHook(n, scope);
        //     //     break;

        //     //   case OBJECTLIT:
        //     //     scope = traverseObjectLiteral(n, scope);
        //     //     break;
        //     NodeId::CallExpr(e) => {
        //         scope = self.traverseCall(e, scope);
        //     }

        //     NodeId::NewExpr(e) => {
        //         scope = self.traverseNew(e, scope);
        //     }

        //     //   case NEW_TARGET:
        //     //     traverseNewTarget(n);
        //     //     break;

        //     //   case ASSIGN_ADD:
        //     //   case ADD:
        //     //     scope = traverseAdd(n, scope);
        //     //     break;

        //     //   case POS:
        //     //     scope = traverseUnaryPlus(n, scope);
        //     //     break;

        //     //   case NEG:
        //     //   case BITNOT:
        //     //   case DEC:
        //     //   case INC:
        //     //     scope = traverseBigIntCompatibleUnaryOperator(n, scope);
        //     //     break;

        //     //   case ARRAYLIT:
        //     //     scope = traverseArrayLiteral(n, scope);
        //     //     break;

        //     //   case THIS:
        //     //     n.setJSType(scope.getTypeOfThis());
        //     //     break;

        //     //   case ASSIGN_LSH:
        //     //   case ASSIGN_RSH:
        //     //   case ASSIGN_DIV:
        //     //   case ASSIGN_MOD:
        //     //   case ASSIGN_BITAND:
        //     //   case ASSIGN_BITXOR:
        //     //   case ASSIGN_BITOR:
        //     //   case ASSIGN_MUL:
        //     //   case ASSIGN_SUB:
        //     //   case ASSIGN_EXPONENT:
        //     //     scope = traverseAssignOp(n, scope);
        //     //     break;

        //     //   case ASSIGN_URSH:
        //     //     // >>> is not compatible with BigInt
        //     //     scope = traverseAssignUnsignedRightShift(n, scope);
        //     //     break;

        //     //   case BITAND:
        //     //   case BITXOR:
        //     //   case BITOR:
        //     //   case LSH:
        //     //   case RSH:
        //     //   case SUB:
        //     //   case MUL:
        //     //   case DIV:
        //     //   case MOD:
        //     //   case EXPONENT:
        //     //     scope = traverseBigIntCompatibleBinaryOperator(n, scope);
        //     //     break;

        //     //   case URSH:
        //     //     // >>> is not compatible with BigInt
        //     //     scope = traverseUnsignedRightShift(n, scope);
        //     //     break;

        //     //   case COMMA:
        //     //     scope = traverseChildren(n, scope);
        //     //     n.setJSType(getJSType(n.getLastChild()));
        //     //     break;

        //     //   case TEMPLATELIT:
        //     //   case TYPEOF:
        //     //     scope = traverseChildren(n, scope);
        //     //     n.setJSType(getNativeType(STRING_TYPE));
        //     //     break;

        //     //   case TEMPLATELIT_SUB:
        //     //   case THROW:
        //     //   case ITER_SPREAD:
        //     //   case OBJECT_SPREAD:
        //     //   case IMPORT:
        //     //   case IMPORT_SPECS:
        //     //   case IMPORT_STAR:
        //     //     // these nodes are untyped but have children that may affect the flow scope and need to
        //     //     // be typed.
        //     //     scope = traverseChildren(n, scope);
        //     //     isTypeable = false;
        //     //     break;

        //     //   case IMPORT_SPEC:
        //     //     // these nodes are untyped but have children that need to be typed.
        //     //     traverseImportSpec(scope, n);
        //     //     isTypeable = false;
        //     //     break;

        //     //   case TAGGED_TEMPLATELIT:
        //     //     scope = traverseTaggedTemplateLit(n, scope);
        //     //     break;

        //     //   case DELPROP:
        //     //   case LT:
        //     //   case LE:
        //     //   case GT:
        //     //   case GE:
        //     //   case NOT:
        //     //   case EQ:
        //     //   case NE:
        //     //   case SHEQ:
        //     //   case SHNE:
        //     //   case INSTANCEOF:
        //     //   case IN:
        //     //     scope = traverseChildren(n, scope);
        //     //     n.setJSType(getNativeType(BOOLEAN_TYPE));
        //     //     break;
        //     //   case GETELEM:
        //     //     scope = traverseGetElem(n, scope);
        //     //     break;

        //     //   case EXPR_RESULT:
        //     //     scope = traverseChildren(n, scope);
        //     //     if (n.getFirstChild().isGetProp()) {
        //     //       Node getprop = n.getFirstChild();
        //     //       ObjectType ownerType =
        //     //           ObjectType.cast(getJSType(getprop.getFirstChild()).restrictByNotNullOrUndefined());
        //     //       if (ownerType != null) {
        //     //         ensurePropertyDeclaredHelper(getprop, ownerType, scope);
        //     //       }
        //     //     }
        //     //     isTypeable = false;
        //     //     break;

        //     //   case SWITCH:
        //     //     scope = traverse(n.getFirstChild(), scope);
        //     //     isTypeable = false;
        //     //     break;
        //     NodeId::ReturnStmt(s) => {
        //         scope = self.traverseReturn(s, scope);
        //         isTypeable = false;
        //     }

        //     NodeId::YieldExpr(e) => {
        //         scope = traverseChildren(e, scope);
        //         n.setJSType(getNativeType(UNKNOWN_TYPE));
        //     }

        //     //   case VAR:
        //     //   case LET:
        //     //   case CONST:
        //     //     scope = traverseDeclaration(n, scope);
        //     //     isTypeable = false;
        //     //     break;

        //     //   case CATCH:
        //     //     scope = traverseCatch(n, scope);
        //     //     isTypeable = false;
        //     //     break;

        //     //   case CAST:
        //     //     scope = traverseChildren(n, scope);
        //     //     JSDocInfo info = n.getJSDocInfo();
        //     //     // TODO(b/123955687): also check that info.hasType() is true
        //     //     checkNotNull(info, "CAST node should always have JSDocInfo");
        //     //     if (info.hasType()) {
        //     //       // NOTE(lharker) - I tried moving CAST type evaluation into the typed scope creation
        //     //       // phase.
        //     //       // Since it caused a few new, seemingly spurious, 'Bad type annotation' and
        //     //       // 'unknown property type' warnings, and having it in TypeInference seems to work, we just
        //     //       // do the lookup + resolution here.
        //     //       n.setJSType(
        //     //           info.getType()
        //     //               .evaluate(scope.getDeclarationScope(), registry)
        //     //               .resolve(registry.getErrorReporter()));
        //     //     } else {
        //     //       n.setJSType(unknownType);
        //     //     }
        //     //     break;

        //     //   case SUPER:
        //     //     traverseSuper(n);
        //     //     break;
        //     NodeId::AwaitExpr(e) => {
        //         scope = self.traverseAwait(e, scope);
        //     }

        //     //   case VOID:
        //     //     n.setJSType(getNativeType(VOID_TYPE));
        //     //     scope = traverseChildren(n, scope);
        //     //     break;

        //     //   case EXPORT:
        //     //     scope = traverseChildren(n, scope);
        //     //     if (n.getBooleanProp(Node.EXPORT_DEFAULT)) {
        //     //       // TypedScopeCreator declared a dummy variable *default* to store this type. Update the
        //     //       // variable with the inferred type.
        //     //       TypedVar defaultExport = getDeclaredVar(scope, Export.DEFAULT_EXPORT_NAME);
        //     //       if (defaultExport.isTypeInferred()) {
        //     //         defaultExport.setType(getJSType(n.getOnlyChild()));
        //     //       }
        //     //     }
        //     //     isTypeable = false;
        //     //     break;

        //     //   case IMPORT_META:
        //     //     // TODO(b/137797083): Set an appropriate type.
        //     //     n.setJSType(unknownType);
        //     //     break;

        //     //   case ROOT:
        //     //   case SCRIPT:
        //     //   case MODULE_BODY:
        //     //   case FUNCTION:
        //     //   case PARAM_LIST:
        //     //   case BLOCK:
        //     //   case EMPTY:
        //     //   case IF:
        //     //   case WHILE:
        //     //   case DO:
        //     //   case FOR:
        //     //   case FOR_IN:
        //     //   case FOR_OF:
        //     //   case FOR_AWAIT_OF:
        //     //   case BREAK:
        //     //   case CONTINUE:
        //     //   case TRY:
        //     //   case CASE:
        //     //   case DEFAULT_CASE:
        //     //   case WITH:
        //     //   case DEBUGGER:
        //     //   case EXPORT_SPECS:
        //     //     // These don't need to be typed here, since they only affect control flow.
        //     //     isTypeable = false;
        //     //     break;

        //     //   case DYNAMIC_IMPORT:
        //     //     traverseDynamicImport(n, scope);
        //     //     break;

        //     // todo:
        //     //   case TEMPLATELIT_STRING:
        //     NodeId::Bool(_)
        //     | NodeId::Bool(_)
        //     | NodeId::Str(_)
        //     | NodeId::Number(_)
        //     | NodeId::BigInt(_)
        //     | NodeId::Null(_)
        //     | NodeId::Regex(_) => {
        //         // Primitives are typed in TypedScopeCreator.AbstractScopeBuilder#attachLiteralTypes
        //     }

        //     _ => panic!("Type inference doesn't know to handle node: {:?}" + n),
        // }

        // if isTypeable && n.getJSType() == null && !TOKENS_ALLOWING_NULL_TYPES.contains(n.getToken())
        // {
        //     panic!("Failed to infer JSType for " + n);
        // }
        // scope
    }

    ///////////////////////////////////////////////////////
    fn newBooleanOutcomePair(
        &self,
        jsType: Option<Ty>,
        flowScope: &LinkedFlowScope,
    ) -> BooleanOutcomePair {
        todo!();
        // if let Some(ty) = jsType {
        //     BooleanOutcomePair::new(
        //         jsType.getPossibleToBooleanOutcomes(),
        //         if self.registry.getNativeType(BOOLEAN_TYPE).isSubtypeOf(jsType) {
        //             BooleanLiteralSet::BOTH
        //         } else {
        //             BooleanLiteralSet::EMPTY
        //         },
        //         flowScope,
        //         flowScope,
        //     )
        // } else {
        //     BooleanOutcomePair::new(
        //         BooleanLiteralSet::BOTH,
        //         BooleanLiteralSet::BOTH,
        //         flowScope,
        //         flowScope,
        //     )
        // }
    }
}

// NOTE(nicksantos): Right now, we just treat ON_EX edges like UNCOND
// edges. If we wanted to be perfect, we'd actually JOIN all the out
// lattices of this flow with the in lattice, and then make that the out
// lattice for the ON_EX edge. But it's probably too expensive to be
// worthwhile.
struct FlowBrancher<'tcx, 'a> {
    condition: Option<NodeId>,
    conditionFlowScope: Option<LinkedFlowScope<'tcx>>,
    conditionOutcomes: Option<BooleanOutcomePair<'tcx, 'a>>,
}

impl<'tcx, 'a> FlowBrancher<'tcx, 'a> {
    pub fn new(source: NodeId, output: LinkedFlowScope) -> Self {
        Self {
            condition: None,
            conditionFlowScope: None,
            conditionOutcomes: None,
        }
    }

    pub fn branchFlow(branch: Branch) -> LinkedFlowScope<'tcx> {
        todo!();
        // switch (branch) {
        //   case ON_TRUE:
        //     if (NodeUtil.isEnhancedFor(source)) {
        //       return initializeEnhancedForScope(source, output);
        //     }
        //     // FALL THROUGH

        //   case ON_FALSE:
        //     if (condition == null) {
        //       if (source.isCase()) {
        //         condition = source;
        //         conditionFlowScope = traverse(condition.getFirstChild(), output);
        //       } else {
        //         condition = NodeUtil.getConditionExpression(source);
        //         if (condition == null) {
        //           return output;
        //         }
        //       }
        //     }

        //     if (condition.isAnd() || condition.isOr()) {
        //       // When handling the short-circuiting binary operators,
        //       // the outcome scope on true can be different than the outcome
        //       // scope on false.
        //       //
        //       // TODO(nicksantos): The "right" way to do this is to
        //       // carry the known outcome all the way through the
        //       // recursive traversal, so that we can construct a
        //       // different flow scope based on the outcome. However,
        //       // this would require a bunch of code and a bunch of
        //       // extra computation for an edge case. This seems to be
        //       // a "good enough" approximation.

        //       // conditionOutcomes is cached from previous calls to the brancher
        //       if (conditionOutcomes == null) {
        //         conditionOutcomes =
        //             condition.isAnd()
        //                 ? traverseAnd(condition, output)
        //                 : traverseOr(condition, output);
        //       }
        //       return reverseInterpreter.getPreciserScopeKnowingConditionOutcome(
        //           condition,
        //           conditionOutcomes.getOutcomeFlowScope(
        //               condition.getToken(), branch == Branch.ON_TRUE),
        //           Outcome.forBoolean(branch.equals(Branch.ON_TRUE)));
        //     }

        //     // conditionFlowScope is cached from previous calls to the brancher
        //     if (conditionFlowScope == null) {
        //       conditionFlowScope = traverse(condition, output);
        //     }
        //     return reverseInterpreter.getPreciserScopeKnowingConditionOutcome(
        //         condition, conditionFlowScope, Outcome.forBoolean(branch.equals(Branch.ON_TRUE)));

        //   default:
        //     return output;
        // }
    }
}

/**
 * When traversing short-circuiting binary operations, we need to keep track of two sets of
 * boolean literals: 1. {@code toBooleanOutcomes}: boolean literals as converted from any types,
 * 2. {@code booleanValues}: boolean literals from just boolean types.
 */
struct BooleanOutcomePair<'tcx, 'a> {
    toBooleanOutcomes: BooleanLiteralSet,
    booleanValues: BooleanLiteralSet,

    // The scope if only half of the expression executed, when applicable.
    leftScope: &'a LinkedFlowScope<'tcx>,

    // The scope when the whole expression executed.
    rightScope: &'a LinkedFlowScope<'tcx>,

    // The scope when we don't know how much of the expression is executed.
    joinedScope: Option<LinkedFlowScope<'tcx>>,
}

impl<'tcx, 'a> BooleanOutcomePair<'tcx, 'a> {
    pub fn new(
        toBooleanOutcomes: BooleanLiteralSet,
        booleanValues: BooleanLiteralSet,
        leftScope: &'a LinkedFlowScope<'tcx>,
        rightScope: &'a LinkedFlowScope<'tcx>,
    ) -> Self {
        Self {
            toBooleanOutcomes,
            booleanValues,
            leftScope,
            rightScope,
            joinedScope: None,
        }
    }

    /**
     * Gets the safe estimated scope without knowing if all of the subexpressions will be evaluated.
     */
    pub fn getJoinedFlowScope(&mut self, inferer: &TypeInference) -> &LinkedFlowScope<'tcx> {
        todo!();
        // match self.joinedScope {
        //     Some(ref joinedScope) => joinedScope,
        //     None => {
        //         let joinedScope = if self.leftScope as *const _ == self.rightScope as *const _ {
        //             self.rightScope
        //         } else {
        //             inferer.join(self.leftScope, self.rightScope)
        //         };
        //         self.joinedScope = Some(joinedScope);
        //         &joinedScope
        //     }
        // }
    }

    /** Gets the outcome scope if we do know the outcome of the entire expression. */
    pub fn getOutcomeFlowScope(
        &mut self,
        inferer: &TypeInference,
        nodeType: NodeId,
        outcome: bool,
    ) -> &LinkedFlowScope<'tcx> {
        if (matches!(nodeType, LogicalAnd) && outcome)
            || (matches!(nodeType, LogicalOr) && !outcome)
        {
            // We know that the whole expression must have executed.
            self.rightScope
        } else {
            self.getJoinedFlowScope(inferer)
        }
    }
}

/** Abstracts logic for declaring an lvalue in a particular scope */
trait TypeDeclaringCallback<'tcx, 'ast, 'a> {
    /**
     * Updates the given scope upon seeing an assignment or declaration
     *
     * @param scope the scope we are in
     * @param lvalue the value being updated, a NAME, GETPROP, GETELEM, or CAST
     * @param type the type we've inferred for the lvalue
     * @return the updated flow scope
     */
    fn declareTypeInScope(
        scope: LinkedFlowScope,
        lvalue: NodeId,
        ty: Option<Ty<'tcx>>,
    ) -> LinkedFlowScope<'tcx>;
}

// TODO(b/154044898): delete these exceptions. Names are given null types to enable inference of
// undeclared names assigned in multiple local scopes. The compiler also infers call and new
// types when the invocation target is such a name.
fn null_type_allowed_for_node(node: &AstNode) -> bool {
    matches!(
        node,
        AstNode::Ident(_) | AstNode::CallExpr(_) | AstNode::NewExpr(_)
    )
}
