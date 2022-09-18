use super::*;

fn getTypeFromFlowType(flowType: FlowType) -> TypeId {
    match flowType {
        FlowType::Type(t) => t,
        FlowType::IncompleteType(t) => t.ty,
    }
}

impl Checker {
    pub fn getFlowTypeOfReference(
        &mut self,
        reference: &BoundNode,
        declaredType: TypeId,
        initialType: TypeId,
        flowContainer: Option<BoundNode>,
    ) -> TypeId {
        // let mut key: string | undefined;
        // let mut isKeySet = false;
        let mut flowDepth = 0;
        // TODO:
        // if flowAnalysisDisabled {
        //     return errorType;
        // }
        if self.node_data(reference.clone()).flowNode.is_none() {
            return declaredType;
        }
        self.flowInvocationCount += 1;
        let sharedFlowStart = self.sharedFlowCount;
        let reference_flow = self.node_data(reference.clone()).flowNode.unwrap();
        let evolvedType = getTypeFromFlowType(getTypeAtFlowNode(
            self,
            reference,
            declaredType,
            initialType,
            &flowContainer,
            &mut flowDepth,
            sharedFlowStart,
            reference_flow,
        ));
        self.sharedFlowCount = sharedFlowStart;
        // When the reference is 'x' in an 'x.length', 'x.push(value)', 'x.unshift(value)' or x[n] = value' operation,
        // we give type 'any[]' to 'x' instead of using the type determined by control flow analysis such that operations
        // on empty arrays are possible without implicit any errors and new element types can be inferred without
        // type mismatch errors.
        let resultType = if self.types[evolvedType]
            .get_object_flags()
            .intersects(ObjectFlags::EvolvingArray)
            && self.isEvolvingArrayOperationTarget(reference.clone())
        {
            todo!();
            // self.autoArrayType
        } else {
            self.finalizeEvolvingArrayType(evolvedType)
        };
        if resultType == self.unreachableNeverType {
            return declaredType;
        }
        if let Some(BoundNode::TsNonNullExpr(_)) = reference.parent() {
            todo!();
            // if !self.types[resultType]
            //     .get_flags()
            //     .intersects(TypeFlags::Never)
            //     && self.types[getTypeWithFacts(resultType, TypeFacts::NEUndefinedOrNull)]
            //         .get_flags()
            //         .intersects(TypeFlags::Never)
            // {
            //     return declaredType;
            // }
        }
        // The non-null unknown type should never escape control flow analysis.
        return if resultType == self.nonNullUnknownType {
            self.unknownType
        } else {
            resultType
        };

        // function getOrSetCacheKey() {
        //     if (isKeySet) {
        //         return key;
        //     }
        //     isKeySet = true;
        //     return key = getFlowCacheKey(reference, declaredType, initialType, flowContainer);
        // }

        fn getTypeAtFlowNode(
            checker: &mut Checker,
            reference: &BoundNode,
            declaredType: TypeId,
            initialType: TypeId,
            flowContainer: &Option<BoundNode>,
            flowDepth: &mut u16,
            sharedFlowStart: usize,

            mut flow: FlowNodeId,
        ) -> FlowType {
            if *flowDepth == 2000 {
                todo!();
                // // We have made 2000 recursive invocations. To avoid overflowing the call stack we report an error
                // // and disable further control flow analysis in the containing function or module body.
                // tracing?.instant(tracing.Phase.CheckTypes, "getTypeAtFlowNode_DepthLimit", { flowId: flow.id });
                // flowAnalysisDisabled = true;
                // reportFlowControlError(reference);
                // return errorType;
            }
            *flowDepth += 1;
            let mut sharedFlow = None;
            loop {
                let flags = checker.flow_nodes[flow].flags;
                if flags.intersects(FlowFlags::Shared) {
                    // We cache results of flow type resolution for shared nodes that were previously visited in
                    // the same getFlowTypeOfReference invocation. A node is considered shared when it is the
                    // antecedent of more than one node.

                    for i in sharedFlowStart..checker.sharedFlowCount {
                        if checker.sharedFlowNodes[i] == flow {
                            *flowDepth -= 1;
                            return checker.sharedFlowTypes[i];
                        }
                    }
                    sharedFlow = Some(flow);
                }
                let ty = match &checker.flow_nodes[flow].kind {
                    FlowNodeKind::FlowAssignment(f) => {
                        let antecedent = f.antecedent;
                        if let Some(ty) = getTypeAtFlowAssignment(
                            checker,
                            reference,
                            declaredType,
                            initialType,
                            flowContainer,
                            flowDepth,
                            sharedFlowStart,
                            flow,
                        ) {
                            ty
                        } else {
                            flow = antecedent;
                            continue;
                        }
                    }
                    FlowNodeKind::FlowCall(f) => {
                        let antecedent = f.antecedent;
                        if let Some(ty) = getTypeAtFlowCall(
                            checker,
                            reference,
                            declaredType,
                            initialType,
                            flowContainer,
                            flowDepth,
                            flow,
                        ) {
                            ty
                        } else {
                            flow = antecedent;
                            continue;
                        }
                    }
                    FlowNodeKind::FlowCondition(_) => getTypeAtFlowCondition(
                        checker,
                        reference,
                        declaredType,
                        initialType,
                        flowContainer,
                        flowDepth,
                        sharedFlowStart,
                        flow,
                    ),
                    FlowNodeKind::FlowSwitchClause(_) => {
                        todo!();
                        // ty = getTypeAtSwitchClause(flow as FlowSwitchClause);
                    }
                    FlowNodeKind::FlowLabel(f) => {
                        if f.antecedents.len() == 1 {
                            flow = f.antecedents[0];
                            continue;
                        }
                        if flags.intersects(FlowFlags::BranchLabel) {
                            getTypeAtFlowBranchLabel(
                                checker,
                                reference,
                                declaredType,
                                initialType,
                                flowContainer,
                                flowDepth,
                                sharedFlowStart,
                                flow,
                            )
                        } else {
                            todo!();
                            // getTypeAtFlowLoopLabel(flow)
                        }
                    }
                    FlowNodeKind::FlowArrayMutation(_) => {
                        todo!();
                        // ty = getTypeAtFlowArrayMutation(flow as FlowArrayMutation);
                        // if !ty.is_none() {
                        //     flow = (flow as FlowArrayMutation).antecedent;
                        //     continue;
                        // }
                    }
                    FlowNodeKind::FlowReduceLabel(_) => {
                        todo!();
                        // let target = (flow as FlowReduceLabel).target;
                        // let saveAntecedents = target.antecedents;
                        // target.antecedents = (flow as FlowReduceLabel).antecedents;
                        // ty = getTypeAtFlowNode((flow as FlowReduceLabel).antecedent);
                        // target.antecedents = saveAntecedents;
                    }
                    FlowNodeKind::FlowStart(f) => {
                        // Check if we should continue with the control flow of the containing function.
                        if let Some(container) = &f.node {
                            if Some(container) != flowContainer.as_ref()
                                && matches!(
                                    reference,
                                    BoundNode::MemberExpr(_) | BoundNode::ThisExpr(_)
                                )
                            {
                                flow = checker.node_data(container.clone()).flowNode.unwrap();
                                continue;
                            }
                        }
                        // At the top of the flow we have the initial type.
                        FlowType::Type(initialType)
                    }
                    FlowNodeKind::None => {
                        todo!();
                        // Unreachable code errors are reported in the binding phase. Here we
                        // simply return the non-auto declared type to reduce follow-on errors.
                        // ty = convertAutoToAny(declaredType);
                    }
                };
                if let Some(sharedFlow) = sharedFlow {
                    // Record visited node and the associated type in the cache.
                    checker.sharedFlowNodes.push(sharedFlow);
                    checker.sharedFlowTypes.push(ty);
                    // debug_assert_eq!(checker.sharedFlowCount, checker.sharedFlowNodes.len() - 1);
                    // debug_assert_eq!(checker.sharedFlowCount, checker.sharedFlowTypes.len() - 1);
                    checker.sharedFlowNodes[checker.sharedFlowCount] = sharedFlow;
                    checker.sharedFlowTypes[checker.sharedFlowCount] = ty;
                    checker.sharedFlowCount += 1;
                }
                *flowDepth -= 1;
                return ty;
            }
        }

        // function getInitialOrAssignedType(flow: FlowAssignment) {
        //     const node = flow.node;
        //     return getNarrowableTypeForReference(node.kind === SyntaxKind.VariableDeclaration || node.kind === SyntaxKind.BindingElement ?
        //         getInitialType(node as VariableDeclaration | BindingElement) :
        //         getAssignedType(node), reference);
        // }

        fn getTypeAtFlowAssignment(
            checker: &mut Checker,
            reference: &BoundNode,
            declaredType: TypeId,
            initialType: TypeId,
            flowContainer: &Option<BoundNode>,
            flowDepth: &mut u16,
            sharedFlowStart: usize,

            flow: FlowNodeId,
        ) -> Option<FlowType> {
            let node = unwrap_as!(
                &checker.flow_nodes[flow].kind,
                FlowNodeKind::FlowAssignment(a),
                a
            )
            .node
            .clone();
            // Assignments only narrow the computed type if the declared type is a union type. Thus, we
            // only need to evaluate the assigned type if the declared type is a union type.
            if checker.isMatchingReference(reference, &node) {
                if !checker.isReachableFlowNode(flow) {
                    return Some(FlowType::Type(checker.unreachableNeverType));
                }
                if getAssignmentTargetKind(node.clone()) == AssignmentKind::Compound {
                    todo!();
                    // let flowType = getTypeAtFlowNode(flow.antecedent);
                    // return createFlowType(getBaseTypeOfLiteralType(getTypeFromFlowType(flowType)), isIncomplete(flowType));
                }
                if declaredType == checker.autoType || declaredType == checker.autoArrayType() {
                    todo!();
                    // if isEmptyArrayAssignment(node) {
                    //     return getEvolvingArrayType(checker.neverType);
                    // }
                    // let assignedType = getWidenedLiteralType(getInitialOrAssignedType(flow));
                    // return if isTypeAssignableTo(assignedType, declaredType) {assignedType}else{checker.anyArrayType};
                }
                if checker.types[declaredType]
                    .get_flags()
                    .intersects(TypeFlags::Union)
                {
                    todo!();
                    // return getAssignmentReducedType(declaredType as UnionType, getInitialOrAssignedType(flow));
                }
                return Some(FlowType::Type(declaredType));
            }
            // We didn't have a direct match. However, if the reference is a dotted name, this
            // may be an assignment to a left hand part of the reference. For example, for a
            // reference 'x.y.z', we may be at an assignment to 'x.y' or 'x'. In that case,
            // return the declared type.
            if checker.containsMatchingReference(reference.clone(), &node) {
                if !checker.isReachableFlowNode(flow) {
                    return Some(FlowType::Type(checker.unreachableNeverType));
                }
                // A matching dotted name might also be an expando property on a function *expression*,
                // in which case we continue control flow analysis back to the function's declaration
                if matches!(node, BoundNode::VarDeclarator(_))
                    && (isBoundNodeInJSFile(&node) || isVarConst(&node))
                {
                    let init = getDeclaredExpandoInitializer(&node);
                    if matches!(init, Some(BoundNode::FnExpr(_) | BoundNode::ArrowExpr(_))) {
                        let antecedent = unwrap_as!(
                            &checker.flow_nodes[flow].kind,
                            FlowNodeKind::FlowAssignment(a),
                            a
                        )
                        .antecedent;
                        return Some(getTypeAtFlowNode(
                            checker,
                            reference,
                            declaredType,
                            initialType,
                            flowContainer,
                            flowDepth,
                            sharedFlowStart,
                            antecedent,
                        ));
                    }
                }
                return Some(FlowType::Type(declaredType));
            }
            // for (const _ in ref) acts as a nonnull on ref
            if matches!(node, BoundNode::VarDeclarator(_)) {
                if let Some(BoundNode::ForInStmt(for_in_loop)) = node.parent().unwrap().parent() {
                    if checker.isMatchingReference(
                        reference,
                        &for_in_loop.right.bind(for_in_loop.clone().into()),
                    ) {
                        todo!();
                        // return getNonNullableTypeIfNeeded(getTypeFromFlowType(getTypeAtFlowNode(flow.antecedent)));
                    }
                }
            }
            // Assignment doesn't affect reference
            None
        }

        // function narrowTypeByAssertion(type: Type, expr: Expression): Type {
        //     const node = skipParentheses(expr, /*excludeJSDocTypeAssertions*/ true);
        //     if (node.kind === SyntaxKind.FalseKeyword) {
        //         return unreachableNeverType;
        //     }
        //     if (node.kind === SyntaxKind.BinaryExpression) {
        //         if ((node as BinaryExpression).operatorToken.kind === SyntaxKind.AmpersandAmpersandToken) {
        //             return narrowTypeByAssertion(narrowTypeByAssertion(type, (node as BinaryExpression).left), (node as BinaryExpression).right);
        //         }
        //         if ((node as BinaryExpression).operatorToken.kind === SyntaxKind.BarBarToken) {
        //             return getUnionType([narrowTypeByAssertion(type, (node as BinaryExpression).left), narrowTypeByAssertion(type, (node as BinaryExpression).right)]);
        //         }
        //     }
        //     return narrowType(type, node, /*assumeTrue*/ true);
        // }

        fn getTypeAtFlowCall(
            checker: &mut Checker,
            reference: &BoundNode,
            declaredType: TypeId,
            initialType: TypeId,
            flowContainer: &Option<BoundNode>,
            flowDepth: &mut u16,

            flow: FlowNodeId,
        ) -> Option<FlowType> {
            debug_assert!(matches!(
                checker.flow_nodes[flow].kind,
                FlowNodeKind::FlowCall(_)
            ));
            let call_node =
                unwrap_as!(&checker.flow_nodes[flow].kind, FlowNodeKind::FlowCall(c), c)
                    .node
                    .clone();
            if let Some(signature) = checker.getEffectsSignature(&call_node) {
                todo!();
                // let predicate = getTypePredicateOfSignature(signature);
                // if (predicate && (predicate.kind == TypePredicateKind.AssertsThis || predicate.kind == TypePredicateKind.AssertsIdentifier)) {
                //     let flowType = getTypeAtFlowNode(flow.antecedent);
                //     let ty = finalizeEvolvingArrayType(getTypeFromFlowType(flowType));
                //     let narrowedType = if predicate.ty {narrowTypeByTypePredicate(ty, predicate, flow.node, /*assumeTrue*/ true) }
                //         else if predicate.kind == TypePredicateKind.AssertsIdentifier && predicate.parameterIndex >= 0 && predicate.parameterIndex < flow.node.arguments.length {narrowTypeByAssertion(ty, flow.node.arguments[predicate.parameterIndex]) }
                //         else{ty};
                //     return narrowedType == ty ? flowType : createFlowType(narrowedType, isIncomplete(flowType));
                // }
                // if getReturnTypeOfSignature(signature).flags & TypeFlags.Never {
                //     return unreachableNeverType;
                // }
            }
            None
        }

        // function getTypeAtFlowArrayMutation(flow: FlowArrayMutation): FlowType | undefined {
        //     if (declaredType === autoType || declaredType === autoArrayType) {
        //         const node = flow.node;
        //         const expr = node.kind === SyntaxKind.CallExpression ?
        //             (node.expression as PropertyAccessExpression).expression :
        //             (node.left as ElementAccessExpression).expression;
        //         if (isMatchingReference(reference, getReferenceCandidate(expr))) {
        //             const flowType = getTypeAtFlowNode(flow.antecedent);
        //             const type = getTypeFromFlowType(flowType);
        //             if (getObjectFlags(type) & ObjectFlags.EvolvingArray) {
        //                 let evolvedType = type as EvolvingArrayType;
        //                 if (node.kind === SyntaxKind.CallExpression) {
        //                     for (const arg of node.arguments) {
        //                         evolvedType = addEvolvingArrayElementType(evolvedType, arg);
        //                     }
        //                 }
        //                 else {
        //                     // We must get the context free expression type so as to not recur in an uncached fashion on the LHS (which causes exponential blowup in compile time)
        //                     const indexType = getContextFreeTypeOfExpression((node.left as ElementAccessExpression).argumentExpression);
        //                     if (isTypeAssignableToKind(indexType, TypeFlags.NumberLike)) {
        //                         evolvedType = addEvolvingArrayElementType(evolvedType, node.right);
        //                     }
        //                 }
        //                 return evolvedType === type ? flowType : createFlowType(evolvedType, isIncomplete(flowType));
        //             }
        //             return flowType;
        //         }
        //     }
        //     return undefined;
        // }

        fn getTypeAtFlowCondition(
            checker: &mut Checker,
            reference: &BoundNode,
            declaredType: TypeId,
            initialType: TypeId,
            flowContainer: &Option<BoundNode>,
            flowDepth: &mut u16,
            sharedFlowStart: usize,

            flow: FlowNodeId,
        ) -> FlowType {
            let antecedent = unwrap_as!(
                &checker.flow_nodes[flow].kind,
                FlowNodeKind::FlowCondition(a),
                a
            )
            .antecedent;
            let flowType = getTypeAtFlowNode(
                checker,
                reference,
                declaredType,
                initialType,
                flowContainer,
                flowDepth,
                sharedFlowStart,
                antecedent,
            );
            let ty = getTypeFromFlowType(flowType);
            if checker.types[ty].get_flags().intersects(TypeFlags::Never) {
                return flowType;
            }
            // If we have an antecedent type (meaning we're reachable in some way), we first
            // attempt to narrow the antecedent type. If that produces the never type, and if
            // the antecedent type is incomplete (i.e. a transient type in a loop), then we
            // take the type guard as an indication that control *could* reach here once we
            // have the complete type. We proceed by switching to the silent never type which
            // doesn't report errors when operators are applied to it. Note that this is the
            // *only* place a silent never type is ever generated.
            let assumeTrue = checker.flow_nodes[flow]
                .flags
                .intersects(FlowFlags::TrueCondition);
            let nonEvolvingType = checker.finalizeEvolvingArrayType(ty);
            let node = &unwrap_as!(
                &checker.flow_nodes[flow].kind,
                FlowNodeKind::FlowCondition(a),
                a
            )
            .node
            .clone();
            let narrowedType = narrowType(
                checker,
                reference,
                declaredType,
                nonEvolvingType,
                &node,
                assumeTrue,
            );
            if narrowedType == nonEvolvingType {
                return flowType;
            }
            checker.createFlowType(narrowedType, isIncomplete(flowType))
        }

        // function getTypeAtSwitchClause(flow: FlowSwitchClause): FlowType {
        //     const expr = flow.switchStatement.expression;
        //     const flowType = getTypeAtFlowNode(flow.antecedent);
        //     let type = getTypeFromFlowType(flowType);
        //     if (isMatchingReference(reference, expr)) {
        //         type = narrowTypeBySwitchOnDiscriminant(type, flow.switchStatement, flow.clauseStart, flow.clauseEnd);
        //     }
        //     else if (expr.kind === SyntaxKind.TypeOfExpression && isMatchingReference(reference, (expr as TypeOfExpression).expression)) {
        //         type = narrowBySwitchOnTypeOf(type, flow.switchStatement, flow.clauseStart, flow.clauseEnd);
        //     }
        //     else {
        //         if (strictNullChecks) {
        //             if (optionalChainContainsReference(expr, reference)) {
        //                 type = narrowTypeBySwitchOptionalChainContainment(type, flow.switchStatement, flow.clauseStart, flow.clauseEnd,
        //                     t => !(t.flags & (TypeFlags.Undefined | TypeFlags.Never)));
        //             }
        //             else if (expr.kind === SyntaxKind.TypeOfExpression && optionalChainContainsReference((expr as TypeOfExpression).expression, reference)) {
        //                 type = narrowTypeBySwitchOptionalChainContainment(type, flow.switchStatement, flow.clauseStart, flow.clauseEnd,
        //                     t => !(t.flags & TypeFlags.Never || t.flags & TypeFlags.StringLiteral && (t as StringLiteralType).value === "undefined"));
        //             }
        //         }
        //         const access = getDiscriminantPropertyAccess(expr, type);
        //         if (access) {
        //             type = narrowTypeBySwitchOnDiscriminantProperty(type, access, flow.switchStatement, flow.clauseStart, flow.clauseEnd);
        //         }
        //     }
        //     return createFlowType(type, isIncomplete(flowType));
        // }

        fn getTypeAtFlowBranchLabel(
            checker: &mut Checker,
            reference: &BoundNode,
            declaredType: TypeId,
            initialType: TypeId,
            flowContainer: &Option<BoundNode>,
            flowDepth: &mut u16,
            sharedFlowStart: usize,

            flow: FlowNodeId,
        ) -> FlowType {
            let mut antecedentTypes = Vec::new();
            let mut subtypeReduction = false;
            let mut seenIncomplete = false;
            let mut bypassFlow = None;
            let antecedents = checker.flow_nodes[flow]
                .kind
                .unwrap_flow_label()
                .antecedents
                .clone();
            for &antecedent in antecedents.iter() {
                if bypassFlow.is_none() {
                    if let FlowNodeKind::FlowSwitchClause(clause) =
                        &checker.flow_nodes[antecedent].kind
                    {
                        if clause.clauseStart == clause.clauseEnd {
                            // The antecedent is the bypass branch of a potentially exhaustive switch statement.
                            bypassFlow = Some(antecedent);
                            continue;
                        }
                    }
                }
                let flowType = getTypeAtFlowNode(
                    checker,
                    reference,
                    declaredType,
                    initialType,
                    flowContainer,
                    flowDepth,
                    sharedFlowStart,
                    antecedent,
                );
                let ty = getTypeFromFlowType(flowType);
                // If the type at a particular antecedent path is the declared type and the
                // reference is known to always be assigned (i.e. when declared and initial types
                // are the same), there is no reason to process more antecedents since the only
                // possible outcome is subtypes that will be removed in the final union type anyway.
                if ty == declaredType && declaredType == initialType {
                    return FlowType::Type(ty);
                }
                antecedentTypes.push_if_unique(ty);
                // If an antecedent type is not a subset of the declared type, we need to perform
                // subtype reduction. This happens when a "foreign" type is injected into the control
                // flow using the instanceof operator or a user defined type predicate.
                if !checker.isTypeSubsetOf(ty, declaredType) {
                    subtypeReduction = true;
                }
                if isIncomplete(flowType) {
                    seenIncomplete = true;
                }
            }
            if let Some(bypassFlow) = bypassFlow {
                let flowType = getTypeAtFlowNode(
                    checker,
                    reference,
                    declaredType,
                    initialType,
                    flowContainer,
                    flowDepth,
                    sharedFlowStart,
                    bypassFlow,
                );
                let ty = getTypeFromFlowType(flowType);
                let switch_stmt = unwrap_as!(
                    &checker.flow_nodes[bypassFlow].kind,
                    FlowNodeKind::FlowSwitchClause(s),
                    s
                )
                .switchStatement
                .clone();
                // If the bypass flow contributes a type we haven't seen yet and the switch statement
                // isn't exhaustive, process the bypass flow type. Since exhaustiveness checks increase
                // the risk of circularities, we only want to perform them when they make a difference.
                if !antecedentTypes.contains(&ty)
                    && !checker.isExhaustiveSwitchStatement(&switch_stmt)
                {
                    if ty == declaredType && declaredType == initialType {
                        return FlowType::Type(ty);
                    }
                    antecedentTypes.push(ty);
                    if !checker.isTypeSubsetOf(ty, declaredType) {
                        subtypeReduction = true;
                    }
                    if isIncomplete(flowType) {
                        seenIncomplete = true;
                    }
                }
            }
            let ty = getUnionOrEvolvingArrayType(
                checker,
                declaredType,
                antecedentTypes,
                if subtypeReduction {
                    UnionReduction::Subtype
                } else {
                    UnionReduction::Literal
                },
            );
            checker.createFlowType(ty, seenIncomplete)
        }

        // function getTypeAtFlowLoopLabel(flow: FlowLabel): FlowType {
        //     // If we have previously computed the control flow type for the reference at
        //     // this flow loop junction, return the cached type.
        //     const id = getFlowNodeId(flow);
        //     const cache = flowLoopCaches[id] || (flowLoopCaches[id] = new Map<string, Type>());
        //     const key = getOrSetCacheKey();
        //     if (!key) {
        //         // No cache key is generated when binding patterns are in unnarrowable situations
        //         return declaredType;
        //     }
        //     const cached = cache.get(key);
        //     if (cached) {
        //         return cached;
        //     }
        //     // If this flow loop junction and reference are already being processed, return
        //     // the union of the types computed for each branch so far, marked as incomplete.
        //     // It is possible to see an empty array in cases where loops are nested and the
        //     // back edge of the outer loop reaches an inner loop that is already being analyzed.
        //     // In such cases we restart the analysis of the inner loop, which will then see
        //     // a non-empty in-process array for the outer loop and eventually terminate because
        //     // the first antecedent of a loop junction is always the non-looping control flow
        //     // path that leads to the top.
        //     for (let i = flowLoopStart; i < flowLoopCount; i++) {
        //         if (flowLoopNodes[i] === flow && flowLoopKeys[i] === key && flowLoopTypes[i].length) {
        //             return createFlowType(getUnionOrEvolvingArrayType(flowLoopTypes[i], UnionReduction.Literal), /*incomplete*/ true);
        //         }
        //     }
        //     // Add the flow loop junction and reference to the in-process stack and analyze
        //     // each antecedent code path.
        //     const antecedentTypes: Type[] = [];
        //     let subtypeReduction = false;
        //     let firstAntecedentType: FlowType | undefined;
        //     for (const antecedent of flow.antecedents!) {
        //         let flowType;
        //         if (!firstAntecedentType) {
        //             // The first antecedent of a loop junction is always the non-looping control
        //             // flow path that leads to the top.
        //             flowType = firstAntecedentType = getTypeAtFlowNode(antecedent);
        //         }
        //         else {
        //             // All but the first antecedent are the looping control flow paths that lead
        //             // back to the loop junction. We track these on the flow loop stack.
        //             flowLoopNodes[flowLoopCount] = flow;
        //             flowLoopKeys[flowLoopCount] = key;
        //             flowLoopTypes[flowLoopCount] = antecedentTypes;
        //             flowLoopCount++;
        //             const saveFlowTypeCache = flowTypeCache;
        //             flowTypeCache = undefined;
        //             flowType = getTypeAtFlowNode(antecedent);
        //             flowTypeCache = saveFlowTypeCache;
        //             flowLoopCount--;
        //             // If we see a value appear in the cache it is a sign that control flow analysis
        //             // was restarted and completed by checkExpressionCached. We can simply pick up
        //             // the resulting type and bail out.
        //             const cached = cache.get(key);
        //             if (cached) {
        //                 return cached;
        //             }
        //         }
        //         const type = getTypeFromFlowType(flowType);
        //         pushIfUnique(antecedentTypes, type);
        //         // If an antecedent type is not a subset of the declared type, we need to perform
        //         // subtype reduction. This happens when a "foreign" type is injected into the control
        //         // flow using the instanceof operator or a user defined type predicate.
        //         if (!isTypeSubsetOf(type, declaredType)) {
        //             subtypeReduction = true;
        //         }
        //         // If the type at a particular antecedent path is the declared type there is no
        //         // reason to process more antecedents since the only possible outcome is subtypes
        //         // that will be removed in the final union type anyway.
        //         if (type === declaredType) {
        //             break;
        //         }
        //     }
        //     // The result is incomplete if the first antecedent (the non-looping control flow path)
        //     // is incomplete.
        //     const result = getUnionOrEvolvingArrayType(antecedentTypes, subtypeReduction ? UnionReduction.Subtype : UnionReduction.Literal);
        //     if (isIncomplete(firstAntecedentType!)) {
        //         return createFlowType(result, /*incomplete*/ true);
        //     }
        //     cache.set(key, result);
        //     return result;
        // }

        // At flow control branch or loop junctions, if the type along every antecedent code path
        // is an evolving array type, we construct a combined evolving array type. Otherwise we
        // finalize all evolving array types.
        fn getUnionOrEvolvingArrayType(
            checker: &mut Checker,
            declaredType: TypeId,
            types: Vec<TypeId>,
            subtypeReduction: UnionReduction,
        ) -> TypeId {
            if checker.isEvolvingArrayTypeList(&types) {
                todo!();
                // return checker.getEvolvingArrayType(
                //     checker.getUnionType(map(types, getElementTypeOfEvolvingArrayType)),
                // );
            }
            let types = types
                .into_iter()
                .map(|t| checker.finalizeEvolvingArrayType(t))
                .collect::<Vec<_>>();
            let result = checker.getUnionType(&types, Some(subtypeReduction), None, None, None);
            if result != declaredType {
                let result = &checker.types[result];
                let declared_ty = &checker.types[declaredType];
                if result.get_flags().intersects(TypeFlags::Union)
                    && declared_ty.get_flags().intersects(TypeFlags::Union)
                {
                    if result.unwrap_as_union_or_intersection().types
                        == declared_ty.unwrap_as_union_or_intersection().types
                    {
                        return declaredType;
                    }
                }
            }
            result
        }

        fn getDiscriminantPropertyAccess(
            checker: &mut Checker,
            reference: &BoundNode,
            declaredType: TypeId,

            expr: &BoundNode,
            computedType: TypeId,
        ) -> Option<BoundNode> {
            let ty = if checker.types[declaredType]
                .get_flags()
                .intersects(TypeFlags::Union)
            {
                declaredType
            } else {
                computedType
            };

            if checker.types[ty].get_flags().intersects(TypeFlags::Union) {
                if let Some(access) = checker.getPropertyAccess(expr) {
                    if let Some(name) = checker.getAccessedPropertyName(&access) {
                        let target = if let BoundNode::MemberExpr(e) = &access {
                            e.obj.bind(access.clone())
                        } else {
                            todo!("BindingElement");
                            // access.parent.parent.initializer.unwrap()
                        };
                        if checker.isMatchingReference(reference, &target)
                            && checker.isDiscriminantProperty(Some(ty), &name)
                        {
                            return Some(access);
                        }
                    }
                }
            }

            None
        }

        // function narrowTypeByDiscriminant(type: Type, access: AccessExpression | BindingElement, narrowType: (t: Type) => Type): Type {
        //     const propName = getAccessedPropertyName(access);
        //     if (propName === undefined) {
        //         return type;
        //     }
        //     const removeNullable = strictNullChecks && isOptionalChain(access) && maybeTypeOfKind(type, TypeFlags.Nullable);
        //     let propType = getTypeOfPropertyOfType(removeNullable ? getTypeWithFacts(type, TypeFacts.NEUndefinedOrNull) : type, propName);
        //     if (!propType) {
        //         return type;
        //     }
        //     propType = removeNullable ? getOptionalType(propType) : propType;
        //     const narrowedPropType = narrowType(propType);
        //     return filterType(type, t => {
        //         const discriminantType = getTypeOfPropertyOrIndexSignature(t, propName);
        //         return !(narrowedPropType.flags & TypeFlags.Never) && isTypeComparableTo(narrowedPropType, discriminantType);
        //     });
        // }

        // function narrowTypeByDiscriminantProperty(type: Type, access: AccessExpression | BindingElement, operator: SyntaxKind, value: Expression, assumeTrue: boolean) {
        //     if ((operator === SyntaxKind.EqualsEqualsEqualsToken || operator === SyntaxKind.ExclamationEqualsEqualsToken) && type.flags & TypeFlags.Union) {
        //         const keyPropertyName = getKeyPropertyName(type as UnionType);
        //         if (keyPropertyName && keyPropertyName === getAccessedPropertyName(access)) {
        //             const candidate = getConstituentTypeForKeyType(type as UnionType, getTypeOfExpression(value));
        //             if (candidate) {
        //                 return operator === (assumeTrue ? SyntaxKind.EqualsEqualsEqualsToken : SyntaxKind.ExclamationEqualsEqualsToken) ? candidate :
        //                     isUnitType(getTypeOfPropertyOfType(candidate, keyPropertyName) || unknownType) ? removeType(type, candidate) :
        //                     type;
        //             }
        //         }
        //     }
        //     return narrowTypeByDiscriminant(type, access, t => narrowTypeByEquality(t, operator, value, assumeTrue));
        // }

        // function narrowTypeBySwitchOnDiscriminantProperty(type: Type, access: AccessExpression | BindingElement, switchStatement: SwitchStatement, clauseStart: number, clauseEnd: number) {
        //     if (clauseStart < clauseEnd && type.flags & TypeFlags.Union && getKeyPropertyName(type as UnionType) === getAccessedPropertyName(access)) {
        //         const clauseTypes = getSwitchClauseTypes(switchStatement).slice(clauseStart, clauseEnd);
        //         const candidate = getUnionType(map(clauseTypes, t => getConstituentTypeForKeyType(type as UnionType, t) || unknownType));
        //         if (candidate !== unknownType) {
        //             return candidate;
        //         }
        //     }
        //     return narrowTypeByDiscriminant(type, access, t => narrowTypeBySwitchOnDiscriminant(t, switchStatement, clauseStart, clauseEnd));
        // }

        fn narrowTypeByTruthiness(
            checker: &mut Checker,
            reference: &BoundNode,
            declaredType: TypeId,

            mut ty: TypeId,
            expr: &BoundNode,
            assumeTrue: bool,
        ) -> TypeId {
            if checker.isMatchingReference(reference, expr) {
                return if checker.types[ty].get_flags().intersects(TypeFlags::Unknown) && assumeTrue
                {
                    checker.nonNullUnknownType
                } else {
                    todo!();
                    // getTypeWithFacts(ty, assumeTrue ? TypeFacts.Truthy : TypeFacts.Falsy)
                };
            }
            if checker.strictNullChecks
                && assumeTrue
                && checker.optionalChainContainsReference(expr, reference)
            {
                ty = checker.getTypeWithFacts(ty, TypeFacts::NEUndefinedOrNull);
            }
            if let Some(access) =
                getDiscriminantPropertyAccess(checker, reference, declaredType, expr, ty)
            {
                todo!();
                // return narrowTypeByDiscriminant(ty, access, t => getTypeWithFacts(t, assumeTrue ? TypeFacts.Truthy : TypeFacts.Falsy));
            }
            ty
        }

        // function isTypePresencePossible(type: Type, propName: __String, assumeTrue: boolean) {
        //     const prop = getPropertyOfType(type, propName);
        //     if (prop) {
        //         return prop.flags & SymbolFlags.Optional ? true : assumeTrue;
        //     }
        //     return getApplicableIndexInfoForName(type, propName) ? true : !assumeTrue;
        // }

        // function narrowByInKeyword(type: Type, name: __String, assumeTrue: boolean) {
        //     if (type.flags & TypeFlags.Union
        //         || type.flags & TypeFlags.Object && declaredType !== type
        //         || isThisTypeParameter(type)
        //         || type.flags & TypeFlags.Intersection && every((type as IntersectionType).types, t => t.symbol !== globalThisSymbol)) {
        //         return filterType(type, t => isTypePresencePossible(t, name, assumeTrue));
        //     }
        //     return type;
        // }

        // function narrowTypeByBinaryExpression(type: Type, expr: BinaryExpression, assumeTrue: boolean): Type {
        //     switch (expr.operatorToken.kind) {
        //         case SyntaxKind.EqualsToken:
        //         case SyntaxKind.BarBarEqualsToken:
        //         case SyntaxKind.AmpersandAmpersandEqualsToken:
        //         case SyntaxKind.QuestionQuestionEqualsToken:
        //             return narrowTypeByTruthiness(narrowType(type, expr.right, assumeTrue), expr.left, assumeTrue);
        //         case SyntaxKind.EqualsEqualsToken:
        //         case SyntaxKind.ExclamationEqualsToken:
        //         case SyntaxKind.EqualsEqualsEqualsToken:
        //         case SyntaxKind.ExclamationEqualsEqualsToken:
        //             const operator = expr.operatorToken.kind;
        //             const left = getReferenceCandidate(expr.left);
        //             const right = getReferenceCandidate(expr.right);
        //             if (left.kind === SyntaxKind.TypeOfExpression && isStringLiteralLike(right)) {
        //                 return narrowTypeByTypeof(type, left as TypeOfExpression, operator, right, assumeTrue);
        //             }
        //             if (right.kind === SyntaxKind.TypeOfExpression && isStringLiteralLike(left)) {
        //                 return narrowTypeByTypeof(type, right as TypeOfExpression, operator, left, assumeTrue);
        //             }
        //             if (isMatchingReference(reference, left)) {
        //                 return narrowTypeByEquality(type, operator, right, assumeTrue);
        //             }
        //             if (isMatchingReference(reference, right)) {
        //                 return narrowTypeByEquality(type, operator, left, assumeTrue);
        //             }
        //             if (strictNullChecks) {
        //                 if (optionalChainContainsReference(left, reference)) {
        //                     type = narrowTypeByOptionalChainContainment(type, operator, right, assumeTrue);
        //                 }
        //                 else if (optionalChainContainsReference(right, reference)) {
        //                     type = narrowTypeByOptionalChainContainment(type, operator, left, assumeTrue);
        //                 }
        //             }
        //             const leftAccess = getDiscriminantPropertyAccess(left, type);
        //             if (leftAccess) {
        //                 return narrowTypeByDiscriminantProperty(type, leftAccess, operator, right, assumeTrue);
        //             }
        //             const rightAccess = getDiscriminantPropertyAccess(right, type);
        //             if (rightAccess) {
        //                 return narrowTypeByDiscriminantProperty(type, rightAccess, operator, left, assumeTrue);
        //             }
        //             if (isMatchingConstructorReference(left)) {
        //                 return narrowTypeByConstructor(type, operator, right, assumeTrue);
        //             }
        //             if (isMatchingConstructorReference(right)) {
        //                 return narrowTypeByConstructor(type, operator, left, assumeTrue);
        //             }
        //             break;
        //         case SyntaxKind.InstanceOfKeyword:
        //             return narrowTypeByInstanceof(type, expr, assumeTrue);
        //         case SyntaxKind.InKeyword:
        //             if (isPrivateIdentifier(expr.left)) {
        //                 return narrowTypeByPrivateIdentifierInInExpression(type, expr, assumeTrue);
        //             }
        //             const target = getReferenceCandidate(expr.right);
        //             const leftType = getTypeOfNode(expr.left);
        //             if (leftType.flags & TypeFlags.StringLiteral) {
        //                 const name = escapeLeadingUnderscores((leftType as StringLiteralType).value);
        //                 if (containsMissingType(type) && isAccessExpression(reference) && isMatchingReference(reference.expression, target) &&
        //                     getAccessedPropertyName(reference) === name) {
        //                     return getTypeWithFacts(type, assumeTrue ? TypeFacts.NEUndefined : TypeFacts.EQUndefined);
        //                 }
        //                 if (isMatchingReference(reference, target)) {
        //                     return narrowByInKeyword(type, name, assumeTrue);
        //                 }
        //             }
        //             break;
        //         case SyntaxKind.CommaToken:
        //             return narrowType(type, expr.right, assumeTrue);
        //         // Ordinarily we won't see && and || expressions in control flow analysis because the Binder breaks those
        //         // expressions down to individual conditional control flows. However, we may encounter them when analyzing
        //         // aliased conditional expressions.
        //         case SyntaxKind.AmpersandAmpersandToken:
        //             return assumeTrue ?
        //                 narrowType(narrowType(type, expr.left, /*assumeTrue*/ true), expr.right, /*assumeTrue*/ true) :
        //                 getUnionType([narrowType(type, expr.left, /*assumeTrue*/ false), narrowType(type, expr.right, /*assumeTrue*/ false)]);
        //         case SyntaxKind.BarBarToken:
        //             return assumeTrue ?
        //                 getUnionType([narrowType(type, expr.left, /*assumeTrue*/ true), narrowType(type, expr.right, /*assumeTrue*/ true)]) :
        //                 narrowType(narrowType(type, expr.left, /*assumeTrue*/ false), expr.right, /*assumeTrue*/ false);
        //     }
        //     return type;
        // }

        // function narrowTypeByPrivateIdentifierInInExpression(type: Type, expr: BinaryExpression, assumeTrue: boolean): Type {
        //     const target = getReferenceCandidate(expr.right);
        //     if (!isMatchingReference(reference, target)) {
        //         return type;
        //     }

        //     Debug.assertNode(expr.left, isPrivateIdentifier);
        //     const symbol = getSymbolForPrivateIdentifierExpression(expr.left);
        //     if (symbol === undefined) {
        //         return type;
        //     }
        //     const classSymbol = symbol.parent!;
        //     const targetType = hasStaticModifier(Debug.checkDefined(symbol.valueDeclaration, "should always have a declaration"))
        //         ? getTypeOfSymbol(classSymbol) as InterfaceType
        //         : getDeclaredTypeOfSymbol(classSymbol);
        //     return getNarrowedType(type, targetType, assumeTrue, isTypeDerivedFrom);
        // }

        // function narrowTypeByOptionalChainContainment(type: Type, operator: SyntaxKind, value: Expression, assumeTrue: boolean): Type {
        //     // We are in a branch of obj?.foo === value (or any one of the other equality operators). We narrow obj as follows:
        //     // When operator is === and type of value excludes undefined, null and undefined is removed from type of obj in true branch.
        //     // When operator is !== and type of value excludes undefined, null and undefined is removed from type of obj in false branch.
        //     // When operator is == and type of value excludes null and undefined, null and undefined is removed from type of obj in true branch.
        //     // When operator is != and type of value excludes null and undefined, null and undefined is removed from type of obj in false branch.
        //     // When operator is === and type of value is undefined, null and undefined is removed from type of obj in false branch.
        //     // When operator is !== and type of value is undefined, null and undefined is removed from type of obj in true branch.
        //     // When operator is == and type of value is null or undefined, null and undefined is removed from type of obj in false branch.
        //     // When operator is != and type of value is null or undefined, null and undefined is removed from type of obj in true branch.
        //     const equalsOperator = operator === SyntaxKind.EqualsEqualsToken || operator === SyntaxKind.EqualsEqualsEqualsToken;
        //     const nullableFlags = operator === SyntaxKind.EqualsEqualsToken || operator === SyntaxKind.ExclamationEqualsToken ? TypeFlags.Nullable : TypeFlags.Undefined;
        //     const valueType = getTypeOfExpression(value);
        //     // Note that we include any and unknown in the exclusion test because their domain includes null and undefined.
        //     const removeNullable = equalsOperator !== assumeTrue && everyType(valueType, t => !!(t.flags & nullableFlags)) ||
        //         equalsOperator === assumeTrue && everyType(valueType, t => !(t.flags & (TypeFlags.AnyOrUnknown | nullableFlags)));
        //     return removeNullable ? getTypeWithFacts(type, TypeFacts.NEUndefinedOrNull) : type;
        // }

        // function narrowTypeByEquality(type: Type, operator: SyntaxKind, value: Expression, assumeTrue: boolean): Type {
        //     if (type.flags & TypeFlags.Any) {
        //         return type;
        //     }
        //     if (operator === SyntaxKind.ExclamationEqualsToken || operator === SyntaxKind.ExclamationEqualsEqualsToken) {
        //         assumeTrue = !assumeTrue;
        //     }
        //     const valueType = getTypeOfExpression(value);
        //     if (assumeTrue && (type.flags & TypeFlags.Unknown) && (operator === SyntaxKind.EqualsEqualsToken || operator === SyntaxKind.ExclamationEqualsToken) && (valueType.flags & TypeFlags.Null)) {
        //         return getUnionType([nullType, undefinedType]);
        //     }
        //     if ((type.flags & TypeFlags.Unknown) && assumeTrue && (operator === SyntaxKind.EqualsEqualsEqualsToken || operator === SyntaxKind.ExclamationEqualsEqualsToken)) {
        //         if (valueType.flags & (TypeFlags.Primitive | TypeFlags.NonPrimitive)) {
        //             return valueType;
        //         }
        //         if (valueType.flags & TypeFlags.Object) {
        //             return nonPrimitiveType;
        //         }
        //         return type;
        //     }
        //     if (valueType.flags & TypeFlags.Nullable) {
        //         if (!strictNullChecks) {
        //             return type;
        //         }
        //         const doubleEquals = operator === SyntaxKind.EqualsEqualsToken || operator === SyntaxKind.ExclamationEqualsToken;
        //         const facts = doubleEquals ?
        //             assumeTrue ? TypeFacts.EQUndefinedOrNull : TypeFacts.NEUndefinedOrNull :
        //             valueType.flags & TypeFlags.Null ?
        //                 assumeTrue ? TypeFacts.EQNull : TypeFacts.NENull :
        //                 assumeTrue ? TypeFacts.EQUndefined : TypeFacts.NEUndefined;
        //         return type.flags & TypeFlags.Unknown && facts & (TypeFacts.NENull | TypeFacts.NEUndefinedOrNull) ? nonNullUnknownType : getTypeWithFacts(type, facts);
        //     }
        //     if (assumeTrue) {
        //         const filterFn: (t: Type) => boolean = operator === SyntaxKind.EqualsEqualsToken ?
        //             t => areTypesComparable(t, valueType) || isCoercibleUnderDoubleEquals(t, valueType) :
        //             t => areTypesComparable(t, valueType);
        //         return replacePrimitivesWithLiterals(filterType(type, filterFn), valueType);
        //     }
        //     if (isUnitType(valueType)) {
        //         return filterType(type, t => !(isUnitLikeType(t) && areTypesComparable(t, valueType)));
        //     }
        //     return type;
        // }

        // function narrowTypeByTypeof(type: Type, typeOfExpr: TypeOfExpression, operator: SyntaxKind, literal: LiteralExpression, assumeTrue: boolean): Type {
        //     // We have '==', '!=', '===', or !==' operator with 'typeof xxx' and string literal operands
        //     if (operator === SyntaxKind.ExclamationEqualsToken || operator === SyntaxKind.ExclamationEqualsEqualsToken) {
        //         assumeTrue = !assumeTrue;
        //     }
        //     const target = getReferenceCandidate(typeOfExpr.expression);
        //     if (!isMatchingReference(reference, target)) {
        //         if (strictNullChecks && optionalChainContainsReference(target, reference) && assumeTrue === (literal.text !== "undefined")) {
        //             return getTypeWithFacts(type, TypeFacts.NEUndefinedOrNull);
        //         }
        //         return type;
        //     }
        //     if (type.flags & TypeFlags.Any && literal.text === "function") {
        //         return type;
        //     }
        //     if (assumeTrue && type.flags & TypeFlags.Unknown && literal.text === "object") {
        //         // The non-null unknown type is used to track whether a previous narrowing operation has removed the null type
        //         // from the unknown type. For example, the expression `x && typeof x === 'object'` first narrows x to the non-null
        //         // unknown type, and then narrows that to the non-primitive type.
        //         return type === nonNullUnknownType ? nonPrimitiveType : getUnionType([nonPrimitiveType, nullType]);
        //     }
        //     const facts = assumeTrue ?
        //         typeofEQFacts.get(literal.text) || TypeFacts.TypeofEQHostObject :
        //         typeofNEFacts.get(literal.text) || TypeFacts.TypeofNEHostObject;
        //     const impliedType = getImpliedTypeFromTypeofGuard(type, literal.text);
        //     return getTypeWithFacts(assumeTrue && impliedType ? mapType(type, narrowUnionMemberByTypeof(impliedType)) : type, facts);
        // }

        // function narrowTypeBySwitchOptionalChainContainment(type: Type, switchStatement: SwitchStatement, clauseStart: number, clauseEnd: number, clauseCheck: (type: Type) => boolean) {
        //     const everyClauseChecks = clauseStart !== clauseEnd && every(getSwitchClauseTypes(switchStatement).slice(clauseStart, clauseEnd), clauseCheck);
        //     return everyClauseChecks ? getTypeWithFacts(type, TypeFacts.NEUndefinedOrNull) : type;
        // }

        // function narrowTypeBySwitchOnDiscriminant(type: Type, switchStatement: SwitchStatement, clauseStart: number, clauseEnd: number) {
        //     // We only narrow if all case expressions specify
        //     // values with unit types, except for the case where
        //     // `type` is unknown. In this instance we map object
        //     // types to the nonPrimitive type and narrow with that.
        //     const switchTypes = getSwitchClauseTypes(switchStatement);
        //     if (!switchTypes.length) {
        //         return type;
        //     }
        //     const clauseTypes = switchTypes.slice(clauseStart, clauseEnd);
        //     const hasDefaultClause = clauseStart === clauseEnd || contains(clauseTypes, neverType);
        //     if ((type.flags & TypeFlags.Unknown) && !hasDefaultClause) {
        //         let groundClauseTypes: Type[] | undefined;
        //         for (let i = 0; i < clauseTypes.length; i += 1) {
        //             const t = clauseTypes[i];
        //             if (t.flags & (TypeFlags.Primitive | TypeFlags.NonPrimitive)) {
        //                 if (groundClauseTypes !== undefined) {
        //                     groundClauseTypes.push(t);
        //                 }
        //             }
        //             else if (t.flags & TypeFlags.Object) {
        //                 if (groundClauseTypes === undefined) {
        //                     groundClauseTypes = clauseTypes.slice(0, i);
        //                 }
        //                 groundClauseTypes.push(nonPrimitiveType);
        //             }
        //             else {
        //                 return type;
        //             }
        //         }
        //         return getUnionType(groundClauseTypes === undefined ? clauseTypes : groundClauseTypes);
        //     }
        //     const discriminantType = getUnionType(clauseTypes);
        //     const caseType =
        //         discriminantType.flags & TypeFlags.Never ? neverType :
        //         replacePrimitivesWithLiterals(filterType(type, t => areTypesComparable(discriminantType, t)), discriminantType);
        //     if (!hasDefaultClause) {
        //         return caseType;
        //     }
        //     const defaultType = filterType(type, t => !(isUnitLikeType(t) && contains(switchTypes, getRegularTypeOfLiteralType(extractUnitType(t)))));
        //     return caseType.flags & TypeFlags.Never ? defaultType : getUnionType([caseType, defaultType]);
        // }

        // function getImpliedTypeFromTypeofGuard(type: Type, text: string) {
        //     switch (text) {
        //         case "function":
        //             return type.flags & TypeFlags.Any ? type : globalFunctionType;
        //         case "object":
        //             return type.flags & TypeFlags.Unknown ? getUnionType([nonPrimitiveType, nullType]) : type;
        //         default:
        //             return typeofTypesByName.get(text);
        //     }
        // }

        // // When narrowing a union type by a `typeof` guard using type-facts alone, constituent types that are
        // // super-types of the implied guard will be retained in the final type: this is because type-facts only
        // // filter. Instead, we would like to replace those union constituents with the more precise type implied by
        // // the guard. For example: narrowing `{} | undefined` by `"boolean"` should produce the type `boolean`, not
        // // the filtered type `{}`. For this reason we narrow constituents of the union individually, in addition to
        // // filtering by type-facts.
        // function narrowUnionMemberByTypeof(candidate: Type) {
        //     return (type: Type) => {
        //         if (isTypeSubtypeOf(type, candidate)) {
        //             return type;
        //         }
        //         if (isTypeSubtypeOf(candidate, type)) {
        //             return candidate;
        //         }
        //         if (type.flags & TypeFlags.Instantiable) {
        //             const constraint = getBaseConstraintOfType(type) || anyType;
        //             if (isTypeSubtypeOf(candidate, constraint)) {
        //                 return getIntersectionType([type, candidate]);
        //             }
        //         }
        //         return type;
        //     };
        // }

        // function narrowBySwitchOnTypeOf(type: Type, switchStatement: SwitchStatement, clauseStart: number, clauseEnd: number): Type {
        //     const switchWitnesses = getSwitchClauseTypeOfWitnesses(switchStatement, /*retainDefault*/ true);
        //     if (!switchWitnesses.length) {
        //         return type;
        //     }
        //     //  Equal start and end denotes implicit fallthrough; undefined marks explicit default clause
        //     const defaultCaseLocation = findIndex(switchWitnesses, elem => elem === undefined);
        //     const hasDefaultClause = clauseStart === clauseEnd || (defaultCaseLocation >= clauseStart && defaultCaseLocation < clauseEnd);
        //     let clauseWitnesses: string[];
        //     let switchFacts: TypeFacts;
        //     if (defaultCaseLocation > -1) {
        //         // We no longer need the undefined denoting an explicit default case. Remove the undefined and
        //         // fix-up clauseStart and clauseEnd.  This means that we don't have to worry about undefined in the
        //         // witness array.
        //         const witnesses = switchWitnesses.filter(witness => witness !== undefined) as string[];
        //         // The adjusted clause start and end after removing the `default` statement.
        //         const fixedClauseStart = defaultCaseLocation < clauseStart ? clauseStart - 1 : clauseStart;
        //         const fixedClauseEnd = defaultCaseLocation < clauseEnd ? clauseEnd - 1 : clauseEnd;
        //         clauseWitnesses = witnesses.slice(fixedClauseStart, fixedClauseEnd);
        //         switchFacts = getFactsFromTypeofSwitch(fixedClauseStart, fixedClauseEnd, witnesses, hasDefaultClause);
        //     }
        //     else {
        //         clauseWitnesses = switchWitnesses.slice(clauseStart, clauseEnd) as string[];
        //         switchFacts = getFactsFromTypeofSwitch(clauseStart, clauseEnd, switchWitnesses as string[], hasDefaultClause);
        //     }
        //     if (hasDefaultClause) {
        //         return filterType(type, t => (getTypeFacts(t) & switchFacts) === switchFacts);
        //     }
        //     /*
        //       The implied type is the raw type suggested by a
        //       value being caught in this clause.

        //       When the clause contains a default case we ignore
        //       the implied type and try to narrow using any facts
        //       we can learn: see `switchFacts`.

        //       Example:
        //       switch (typeof x) {
        //           case 'number':
        //           case 'string': break;
        //           default: break;
        //           case 'number':
        //           case 'boolean': break
        //       }

        //       In the first clause (case `number` and `string`) the
        //       implied type is number | string.

        //       In the default clause we de not compute an implied type.

        //       In the third clause (case `number` and `boolean`)
        //       the naive implied type is number | boolean, however
        //       we use the type facts to narrow the implied type to
        //       boolean. We know that number cannot be selected
        //       because it is caught in the first clause.
        //     */
        //     const impliedType = getTypeWithFacts(getUnionType(clauseWitnesses.map(text => getImpliedTypeFromTypeofGuard(type, text) || type)), switchFacts);
        //     return getTypeWithFacts(mapType(type, narrowUnionMemberByTypeof(impliedType)), switchFacts);
        // }

        // function isMatchingConstructorReference(expr: Expression) {
        //     return (isPropertyAccessExpression(expr) && idText(expr.name) === "constructor" ||
        //         isElementAccessExpression(expr) && isStringLiteralLike(expr.argumentExpression) && expr.argumentExpression.text === "constructor") &&
        //         isMatchingReference(reference, expr.expression);
        // }

        // function narrowTypeByConstructor(type: Type, operator: SyntaxKind, identifier: Expression, assumeTrue: boolean): Type {
        //     // Do not narrow when checking inequality.
        //     if (assumeTrue ? (operator !== SyntaxKind.EqualsEqualsToken && operator !== SyntaxKind.EqualsEqualsEqualsToken) : (operator !== SyntaxKind.ExclamationEqualsToken && operator !== SyntaxKind.ExclamationEqualsEqualsToken)) {
        //         return type;
        //     }

        //     // Get the type of the constructor identifier expression, if it is not a function then do not narrow.
        //     const identifierType = getTypeOfExpression(identifier);
        //     if (!isFunctionType(identifierType) && !isConstructorType(identifierType)) {
        //         return type;
        //     }

        //     // Get the prototype property of the type identifier so we can find out its type.
        //     const prototypeProperty = getPropertyOfType(identifierType, "prototype" as __String);
        //     if (!prototypeProperty) {
        //         return type;
        //     }

        //     // Get the type of the prototype, if it is undefined, or the global `Object` or `Function` types then do not narrow.
        //     const prototypeType = getTypeOfSymbol(prototypeProperty);
        //     const candidate = !isTypeAny(prototypeType) ? prototypeType : undefined;
        //     if (!candidate || candidate === globalObjectType || candidate === globalFunctionType) {
        //         return type;
        //     }

        //     // If the type that is being narrowed is `any` then just return the `candidate` type since every type is a subtype of `any`.
        //     if (isTypeAny(type)) {
        //         return candidate;
        //     }

        //     // Filter out types that are not considered to be "constructed by" the `candidate` type.
        //     return filterType(type, t => isConstructedBy(t, candidate));

        //     function isConstructedBy(source: Type, target: Type) {
        //         // If either the source or target type are a class type then we need to check that they are the same exact type.
        //         // This is because you may have a class `A` that defines some set of properties, and another class `B`
        //         // that defines the same set of properties as class `A`, in that case they are structurally the same
        //         // type, but when you do something like `instanceOfA.constructor === B` it will return false.
        //         if (source.flags & TypeFlags.Object && getObjectFlags(source) & ObjectFlags.Class ||
        //             target.flags & TypeFlags.Object && getObjectFlags(target) & ObjectFlags.Class) {
        //             return source.symbol === target.symbol;
        //         }

        //         // For all other types just check that the `source` type is a subtype of the `target` type.
        //         return isTypeSubtypeOf(source, target);
        //     }
        // }

        // function narrowTypeByInstanceof(type: Type, expr: BinaryExpression, assumeTrue: boolean): Type {
        //     const left = getReferenceCandidate(expr.left);
        //     if (!isMatchingReference(reference, left)) {
        //         if (assumeTrue && strictNullChecks && optionalChainContainsReference(left, reference)) {
        //             return getTypeWithFacts(type, TypeFacts.NEUndefinedOrNull);
        //         }
        //         return type;
        //     }

        //     // Check that right operand is a function type with a prototype property
        //     const rightType = getTypeOfExpression(expr.right);
        //     if (!isTypeDerivedFrom(rightType, globalFunctionType)) {
        //         return type;
        //     }

        //     let targetType: Type | undefined;
        //     const prototypeProperty = getPropertyOfType(rightType, "prototype" as __String);
        //     if (prototypeProperty) {
        //         // Target type is type of the prototype property
        //         const prototypePropertyType = getTypeOfSymbol(prototypeProperty);
        //         if (!isTypeAny(prototypePropertyType)) {
        //             targetType = prototypePropertyType;
        //         }
        //     }

        //     // Don't narrow from 'any' if the target type is exactly 'Object' or 'Function'
        //     if (isTypeAny(type) && (targetType === globalObjectType || targetType === globalFunctionType)) {
        //         return type;
        //     }

        //     if (!targetType) {
        //         const constructSignatures = getSignaturesOfType(rightType, SignatureKind.Construct);
        //         targetType = constructSignatures.length ?
        //             getUnionType(map(constructSignatures, signature => getReturnTypeOfSignature(getErasedSignature(signature)))) :
        //             emptyObjectType;
        //     }

        //     // We can't narrow a union based off instanceof without negated types see #31576 for more info
        //     if (!assumeTrue && rightType.flags & TypeFlags.Union) {
        //         const nonConstructorTypeInUnion = find((rightType as UnionType).types, (t) => !isConstructorType(t));
        //         if (!nonConstructorTypeInUnion) return type;
        //     }

        //     return getNarrowedType(type, targetType, assumeTrue, isTypeDerivedFrom);
        // }

        // function getNarrowedType(type: Type, candidate: Type, assumeTrue: boolean, isRelated: (source: Type, target: Type) => boolean) {
        //     if (!assumeTrue) {
        //         return filterType(type, t => !isRelated(t, candidate));
        //     }
        //     // If the current type is a union type, remove all constituents that couldn't be instances of
        //     // the candidate type. If one or more constituents remain, return a union of those.
        //     if (type.flags & TypeFlags.Union) {
        //         const assignableType = filterType(type, t => isRelated(t, candidate));
        //         if (!(assignableType.flags & TypeFlags.Never)) {
        //             return assignableType;
        //         }
        //     }

        //     // If the candidate type is a subtype of the target type, narrow to the candidate type.
        //     // Otherwise, if the target type is assignable to the candidate type, keep the target type.
        //     // Otherwise, if the candidate type is assignable to the target type, narrow to the candidate
        //     // type. Otherwise, the types are completely unrelated, so narrow to an intersection of the
        //     // two types.
        //     return isTypeSubtypeOf(candidate, type) ? candidate :
        //          isTypeAssignableTo(type, candidate) ? type :
        //          isTypeAssignableTo(candidate, type) ? candidate :
        //          getIntersectionType([type, candidate]);
        // }

        // function narrowTypeByCallExpression(type: Type, callExpression: CallExpression, assumeTrue: boolean): Type {
        //     if (hasMatchingArgument(callExpression, reference)) {
        //         const signature = assumeTrue || !isCallChain(callExpression) ? getEffectsSignature(callExpression) : undefined;
        //         const predicate = signature && getTypePredicateOfSignature(signature);
        //         if (predicate && (predicate.kind === TypePredicateKind.This || predicate.kind === TypePredicateKind.Identifier)) {
        //             return narrowTypeByTypePredicate(type, predicate, callExpression, assumeTrue);
        //         }
        //     }
        //     if (containsMissingType(type) && isAccessExpression(reference) && isPropertyAccessExpression(callExpression.expression)) {
        //         const callAccess = callExpression.expression;
        //         if (isMatchingReference(reference.expression, getReferenceCandidate(callAccess.expression)) &&
        //             isIdentifier(callAccess.name) && callAccess.name.escapedText === "hasOwnProperty" && callExpression.arguments.length === 1) {
        //             const argument = callExpression.arguments[0];
        //             if (isStringLiteralLike(argument) && getAccessedPropertyName(reference) === escapeLeadingUnderscores(argument.text)) {
        //                 return getTypeWithFacts(type, assumeTrue ? TypeFacts.NEUndefined : TypeFacts.EQUndefined);
        //             }
        //         }
        //     }
        //     return type;
        // }

        // function narrowTypeByTypePredicate(type: Type, predicate: TypePredicate, callExpression: CallExpression, assumeTrue: boolean): Type {
        //     // Don't narrow from 'any' if the predicate type is exactly 'Object' or 'Function'
        //     if (predicate.type && !(isTypeAny(type) && (predicate.type === globalObjectType || predicate.type === globalFunctionType))) {
        //         const predicateArgument = getTypePredicateArgument(predicate, callExpression);
        //         if (predicateArgument) {
        //             if (isMatchingReference(reference, predicateArgument)) {
        //                 return getNarrowedType(type, predicate.type, assumeTrue, isTypeSubtypeOf);
        //             }
        //             if (strictNullChecks && assumeTrue && optionalChainContainsReference(predicateArgument, reference) &&
        //                 !(getTypeFacts(predicate.type) & TypeFacts.EQUndefined)) {
        //                 type = getTypeWithFacts(type, TypeFacts.NEUndefinedOrNull);
        //             }
        //             const access = getDiscriminantPropertyAccess(predicateArgument, type);
        //             if (access) {
        //                 return narrowTypeByDiscriminant(type, access, t => getNarrowedType(t, predicate.type!, assumeTrue, isTypeSubtypeOf));
        //             }
        //         }
        //     }
        //     return type;
        // }

        // Narrow the given type based on the given expression having the assumed boolean value. The returned type
        // will be a subtype or the same type as the argument.
        fn narrowType(
            checker: &mut Checker,
            reference: &BoundNode,
            declaredType: TypeId,
            ty: TypeId,
            expr: &BoundNode,
            assumeTrue: bool,
        ) -> TypeId {
            if matches!(expr, BoundNode::OptChainExpr(_)) {
                todo!("see below");
            }
            // for `a?.b`, we emulate a synthetic `a !== null && a !== undefined` condition for `a`
            // if isExpressionOfOptionalChainRoot(expr)
            //     || isBinaryExpression(expr.parent)
            //         && expr.parent.operatorToken.kind == SyntaxKind.QuestionQuestionToken
            //         && expr.parent.left == expr
            // {
            //     return narrowTypeByOptionality(ty, expr, assumeTrue);
            // }
            match expr {
                BoundNode::Ident(i) => {
                    // When narrowing a reference to a const variable, non-assigned parameter, or readonly property, we inline
                    // up to five levels of aliased conditional expressions that are themselves declared as const variables.
                    if !checker.isMatchingReference(reference, expr) && checker.inlineLevel < 5 {
                        let symbol = checker.getResolvedSymbol(i);
                        if checker.isConstVariable(symbol) {
                            if let Some(ref d @ BoundNode::VarDeclarator(ref decl)) =
                                checker.symbols[symbol].valueDeclaration().clone()
                            {
                                let has_type_ann = match &decl.name {
                                    ast::Pat::Ident(p) => p.type_ann.is_some(),
                                    ast::Pat::Array(p) => p.type_ann.is_some(),
                                    ast::Pat::Object(p) => p.type_ann.is_some(),
                                    _ => unreachable!(),
                                };
                                if has_type_ann
                                    && decl.init.is_some()
                                    && checker.isConstantReference(reference)
                                {
                                    checker.inlineLevel += 1;
                                    let result = narrowType(
                                        checker,
                                        reference,
                                        declaredType,
                                        ty,
                                        &decl.init.as_ref().unwrap().bind(d.clone()),
                                        assumeTrue,
                                    );
                                    checker.inlineLevel -= 1;
                                    return result;
                                }
                            }
                        }
                    }
                    return narrowTypeByTruthiness(
                        checker,
                        reference,
                        declaredType,
                        ty,
                        expr,
                        assumeTrue,
                    );
                }
                BoundNode::ThisExpr(_) | BoundNode::Super(_) | BoundNode::MemberExpr(_) => {
                    todo!();
                    // return narrowTypeByTruthiness(ty, expr, assumeTrue);
                }
                BoundNode::CallExpr(_) => {
                    todo!();
                    // return narrowTypeByCallExpression(ty, expr as CallExpression, assumeTrue);
                }
                BoundNode::ParenExpr(_) | BoundNode::TsNonNullExpr(_) => {
                    todo!();
                    // return narrowType(
                    //     ty,
                    //     (expr as ParenthesizedExpression | NonNullExpression).expression,
                    //     assumeTrue,
                    // );
                }
                BoundNode::AssignExpr(_) | BoundNode::BinExpr(_) => {
                    todo!();
                    // return narrowTypeByBinaryExpression(ty, expr as BinaryExpression, assumeTrue);
                }
                BoundNode::UnaryExpr(e) => {
                    if e.op == ast::UnaryOp::Bang {
                        return narrowType(
                            checker,
                            reference,
                            declaredType,
                            ty,
                            &e.arg.bind(expr.clone()),
                            !assumeTrue,
                        );
                    }
                }
                _ => {}
            }
            ty
        }

        // function narrowTypeByOptionality(type: Type, expr: Expression, assumePresent: boolean): Type {
        //     if (isMatchingReference(reference, expr)) {
        //         return getTypeWithFacts(type, assumePresent ? TypeFacts.NEUndefinedOrNull : TypeFacts.EQUndefinedOrNull);
        //     }
        //     const access = getDiscriminantPropertyAccess(expr, type);
        //     if (access) {
        //         return narrowTypeByDiscriminant(type, access, t => getTypeWithFacts(t, assumePresent ? TypeFacts.NEUndefinedOrNull : TypeFacts.EQUndefinedOrNull));
        //     }
        //     return type;
        // }
    }

    fn createFlowType(&self, ty: TypeId, incomplete: bool) -> FlowType {
        if incomplete {
            if self.types[ty].get_flags().intersects(TypeFlags::Never) {
                FlowType::IncompleteType(IncompleteType {
                    ty: self.silentNeverType,
                })
            } else {
                FlowType::IncompleteType(IncompleteType { ty: ty })
            }
        } else {
            FlowType::Type(ty)
        }
    }
}

fn isIncomplete(flowType: FlowType) -> bool {
    matches!(flowType, FlowType::IncompleteType(_))
}
