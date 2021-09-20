use crate::ctx::Ctx;
use crate::data_flow_analysis::FlowJoiner;
use crate::static_slot::StaticSlot;
// use crate::flow_scope::FlowScope;
use crate::static_typed_slot::StaticTypedSlot;
use crate::typed_scope::{ScopeId, TypedScope};
use crate::typed_var::TypedVarId;
use crate::typing::types::Ty;
use ast::AstNode;
use fxhash::{FxHashMap, FxHashSet};
use im_rc::HashMap;
use std::hash::Hash;
use swc_atoms::JsWord;

// trait Reconciler<K, V> {
//     fn merge(key: K, this_val: Option<V>, that_val: Option<V>);
// }

// impl<K, V> Reconciler<K, V> for HashMap<K, V> {
//     fn merge(key: K, this_val: Option<V>, that_val: Option<V>) {}
// }

trait Reconcile<K, V>
where
    K: Hash + Eq + Clone,
    V: Clone + PartialEq,
{
    fn reconcile<J>(self, other: HashMap<K, V>, joiner: J) -> HashMap<K, V>
    where
        J: FnOnce(&K, Option<V>, Option<V>) -> V + Copy;
}

impl<K, V> Reconcile<K, V> for HashMap<K, V>
where
    K: Hash + Eq + Clone,
    V: Clone + PartialEq,
{
    fn reconcile<J>(self, other: HashMap<K, V>, joiner: J) -> HashMap<K, V>
    where
        J: FnOnce(&K, Option<V>, Option<V>) -> V + Copy,
    {
        let mut out = self.new_from();

        let keys = self.keys().chain(other.keys()).collect::<FxHashSet<_>>();

        let mut last_seen = None;
        for key in keys {
            // Skip duplicate, consecutive keys.
            if Some(key) == last_seen {
                continue;
            }
            last_seen = Some(key);

            let this_value = self.get(key);
            let that_value = other.get(key);

            if let Some(value) = this_value {
                if this_value == that_value {
                    // There's no need to call the joiner fn when the values are equal.
                    out.insert(key.clone(), value.clone());
                    continue;
                }
            }

            let final_value = joiner(
                key,
                this_value.map(|v| v.clone()),
                that_value.map(|v| v.clone()),
            );

            out.insert(key.clone(), final_value);
        }

        out
    }
}

/**
 * A flow scope that tries to store as little symbol information as possible,
 * instead delegating to its parents. Optimized for low memory use.
 */
pub struct LinkedFlowScope<'tcx> {
    /// Map from TypedScope to OverlayScope.
    scopes: HashMap<ScopeId, OverlayScope<'tcx>>,
    functionScope: ScopeId,
    /// The TypedScope for the block that this flow scope is defined for.
    syntacticScope: ScopeId,
}

impl<'tcx, 'ast, 'a> LinkedFlowScope<'tcx> {
    /**
     * Creates a flow scope without a direct parent. This can happen in three cases: (1) the "bottom"
     * scope for a CFG root, (2) a direct child of a parent at the maximum depth, or (3) a joined
     * scope with more than one direct parent. The parent is non-null only in the second case.
     */
    fn new(
        scopes: HashMap<ScopeId, OverlayScope<'tcx>>,
        syntacticScope: ScopeId,
        functionScope: ScopeId,
    ) -> Self {
        Self {
            scopes,
            syntacticScope,
            functionScope,
        }
    }

    /**
     * Returns the scope map, trimmed to the common ancestor between this FlowScope's syntacticScope
     * and the given scope. Any inferred types on variables in deeper scopes cannot be propagated past
     * this point (since they're no longer in scope), and trimming them eagerly allows us to ignore
     * these irrelevant types when checking equality and joining.
     */
    fn trimScopes(&self, ctx: &Ctx<'tcx>, scope: ScopeId) -> HashMap<ScopeId, OverlayScope<'tcx>> {
        let mut thisScope = Some(self.syntacticScope);
        let mut thatScope = Some(scope);
        let mut thisDepth = ctx.scopes[self.syntacticScope].get_depth();
        let mut thatDepth = ctx.scopes[scope].get_depth();
        let mut result = self.scopes.clone();
        while thatDepth > thisDepth && thatScope.is_some() {
            thatScope = ctx.scopes[thatScope.unwrap()].get_parent();
            thatDepth -= 1;
        }
        while thisDepth > thatDepth && thisScope.is_some() {
            result = result.without(&thisScope.unwrap());
            thisScope = ctx.scopes[thisScope.unwrap()].get_parent();
            thisDepth -= 1;
        }
        while thisScope != thatScope && thisScope.is_some() && thatScope.is_some() {
            result = result.without(&thisScope.unwrap());
            thisScope = ctx.scopes[thisScope.unwrap()].get_parent();
            thatScope = ctx.scopes[thatScope.unwrap()].get_parent();
        }
        result
    }

    /** Whether this flows from a bottom scope. */
    fn flowsFromBottom(&self, ctx: &Ctx<'tcx>) -> bool {
        ctx.scopes[self.functionScope].isBottom()
    }

    /** Creates an entry lattice for the flow. */
    pub fn createEntryLattice(scope: ScopeId) -> LinkedFlowScope<'tcx> {
        LinkedFlowScope::new(HashMap::new(), scope, scope)
    }

    pub fn inferSlotType(self, ctx: &Ctx<'tcx>,symbol: &JsWord, ty: Ty<'tcx>) -> LinkedFlowScope<'tcx> {
        let scope = self.getOverlayScopeForNameB(ctx,symbol);
        let newScope = scope.infer(symbol, ty);
        // Aggressively remove empty scopes to maintain a reasonable equivalence.
        let newScopes = if !newScope.slots.is_empty() {
            self.scopes.update(scope.scope.clone(), newScope)
        } else {
            self.scopes.without(&scope.scope)
        };
        // todo!();
        if newScopes != self.scopes {
            LinkedFlowScope::new(newScopes, self.syntacticScope, self.functionScope)
        } else {
            self
        }
    }

    pub fn inferQualifiedSlot(
        self,
        node: AstNode,
        symbol: &JsWord,
        bottomType: Ty,
        inferredType: Ty,
        declared: bool,
    ) -> LinkedFlowScope<'tcx> {
        todo!();
        // if self.functionScope.isGlobal() {
        //     // Do not infer qualified names on the global scope.  Ideally these would be
        //     // added to the scope by TypedScopeCreator, but if they are not, adding them
        //     // here causes scaling problems (large projects can have tens of thousands of
        //     // undeclared qualified names in the global scope) with no real benefit.
        //     return self;
        // }
        // let v = self.syntacticScope.getVar(symbol);
        // if v.is_none() && !self.functionScope.isBottom() {
        //     // NOTE(sdh): Qualified names are declared on scopes lazily via this method.
        //     // The difficulty is that it's not always clear which scope they need to be
        //     // defined on.  In particular, syntacticScope is wrong because it is often a
        //     // nested block scope that is ignored when branches are joined; functionScope
        //     // is also wrong because it could lead to ambiguity if the same root name is
        //     // declared in multiple different blocks.  Instead, the qualified name is declared
        //     // on the scope that owns the root, when possible. When the root is undeclared, the qualified
        //     // name is declared in the global scope, as only global variables can be undeclared.
        //     let rootVar = self
        //         .syntacticScope
        //         .getVar(LinkedFlowScope::getRootOfQualifiedName(symbol));
        //     let rootScope = if let Some(rootVar) = rootVar {
        //         rootVar.scope
        //     } else {
        //         self.syntacticScope.getGlobalScope()
        //     };
        //     v = Some(&rootScope.declare(symbol, node, bottomType, !declared));
        // }

        // if let Some(v) = v {
        //     let declared_type = v.ty;
        //     if !v.typeInferred {
        //         // Use the inferred type over the declared type only if the
        //         // inferred type is a strict subtype of the declared type.
        //         if let Some(declaredType) = declared_type {
        //             if !inferredType.isSubtypeOf(declaredType)
        //                 || declaredType.isSubtypeOf(inferredType)
        //                 || inferredType.equals(declaredType)
        //             {
        //                 return self;
        //             }
        //         } else {
        //             return self;
        //         }
        //     } else if let Some(declared_type) = declared_type {
        //         if !inferredType.isSubtypeOf(declared_type) {
        //             // If this inferred type is incompatible with another type previously
        //             // inferred and stored on the scope, then update the scope.
        //             v.ty = v.ty.map(|ty| ty.getLeastSupertype(inferredType));
        //         }
        //     }
        // }

        // self.inferSlotType(symbol, inferredType)
    }

    // pub fn  getTypeOfThis(&self) -> Ty {
    //     self.functionScope.getTypeOfThis()
    //   }

    //   pub fn  getRootNode(&self) -> Node {
    //     self.syntacticScope.getRootNode()
    //   }

    //   pub fn  getParentScope(&self) -> StaticTypedScope {
    //     // throw new UnsupportedOperationException();
    //     todo!();
    //   }

    /** Get the slot for the given symbol. */
    pub fn getSlot(&self, ctx: &Ctx<'tcx>, name: &JsWord) -> Option<Slot> {
        let var = ctx.scopes[self.syntacticScope].getVar(name);
        let scope = if let Some(var) = var {
            let scope_id = ctx.vars[var].scope;
            self.getOverlayScopeForScopeA(scope_id)
        } else {
            self.getOverlayScopeForNameA(ctx, name)
        };

        if let Some(scope) = scope {
            scope.get_slot(ctx,name)
        } else {
            var.map(|v| Slot::Typed(v))
        }
    }

    fn getRootOfQualifiedName(name: &JsWord) -> &JsWord {
        todo!();
        // int index = name.indexOf('.');
        // return index < 0 ? name : name.substring(0, index);
    }

    /// Returns the overlay scope corresponding to this qualified name.
    fn getOverlayScopeForNameA(
        &self,
        ctx: &Ctx<'tcx>,
        name: &JsWord,
    ) -> Option<&OverlayScope<'tcx>> {
        let rootVar =
            ctx.scopes[self.syntacticScope].getVar(LinkedFlowScope::getRootOfQualifiedName(name));
        let scope = rootVar
            .map(|var| ctx.vars[var].scope)
            .unwrap_or(self.functionScope);
        self.getOverlayScopeForScopeA(scope)
    }

    /// Returns the overlay scope corresponding to this qualified name,
    /// creating it if one does not already exist.
    fn getOverlayScopeForNameB(&self, ctx: &Ctx<'tcx>, name: &JsWord) -> &OverlayScope<'tcx> {
        let rootVar =
            ctx.scopes[self.syntacticScope].getVar(LinkedFlowScope::getRootOfQualifiedName(name));
        let scope = rootVar
            .map(|var| ctx.vars[var].scope)
            .unwrap_or(self.functionScope);
        self.getOverlayScopeForScopeB(scope)
    }

    /**
     * Returns the overlay scope corresponding to this syntactic scope.
     *
     * <p>Use instead of {@link #getOverlayScopeForName(String)} if you already know the
     * correct scope in order to avoid a variable lookup.
     *
     * @param scope the syntactic scope
     */
    fn getOverlayScopeForScopeA(&self, scope: ScopeId) -> Option<&OverlayScope<'tcx>> {
        self.scopes.get(&scope)
    }

    /**
     * Returns the overlay scope corresponding to this syntactic scope.
     *
     * <p>Use instead of {@link #getOverlayScopeForName(String)} if you already know the
     * correct scope in order to avoid a variable lookup.
     *
     * @param scope the syntactic scope
     */
    fn getOverlayScopeForScopeB(&self, scope: ScopeId) -> &OverlayScope<'tcx> {
        todo!();
        // match self.scopes.get(&scope) {
        //     Some(overlay) => overlay,
        //     None => &OverlayScope::new(scope),
        // }
    }

    // @Override
    // public StaticTypedSlot getOwnSlot(String name) {
    //   throw new UnsupportedOperationException();
    // }

    pub fn withSyntacticScope(self, ctx: &Ctx<'tcx>,scope: ScopeId) -> LinkedFlowScope<'tcx> {
        if scope != self.syntacticScope {
            LinkedFlowScope::new(
                // inputProvider,
                self.trimScopes(ctx,scope),
                scope,
                self.functionScope,
            )
        } else {
            self
        }
    }

    pub fn getDeclarationScope(&self) -> ScopeId {
        self.syntacticScope
    }

    pub fn getCommonParentDeclarationScope(
        ctx: &Ctx<'tcx>,
        left: &LinkedFlowScope<'tcx>,
        right: &LinkedFlowScope<'tcx>,
    ) -> ScopeId {
        if left.flowsFromBottom(ctx) {
            return right.syntacticScope;
        } else if right.flowsFromBottom(ctx) {
            return left.syntacticScope;
        }
        ctx.scopes[left.syntacticScope].getCommonParent(ctx,right.syntacticScope)
    }

    fn join(
        linkedA: &LinkedFlowScope<'tcx>,
        linkedB: &LinkedFlowScope<'tcx>,
        commonParent: ScopeId,
    ) -> HashMap<ScopeId, OverlayScope<'tcx>> {
        todo!();
        // TODO: this monstrosity needs a huge clean up. Notable, check the
        // logic around the .unwrap() calls.
        // return linkedA.trimScopes(commonParent).reconcile(
        //     linkedB.trimScopes(commonParent),
        //     |scopeKey, scopeA, scopeB| {
        //         debug_assert!(scopeA.is_some() || scopeB.is_some());
        //         let slotsA = if let Some(scopeA) = scopeA {
        //             scopeA.slots
        //         } else {
        //             Default::default()
        //         };
        //         let slotsB = if let Some(scopeB) = scopeB {
        //             scopeB.slots
        //         } else {
        //             Default::default()
        //         };
        //         // TODO(sdh): Simplify this logic: we want the best non-bottom scope we can get,
        //         // for the purpose of (a) passing to the joined OverlayScope constructor, and
        //         // (b) joining types only present in one scope.
        //         let typedScopeA = if linkedA.flowsFromBottom() {
        //             None
        //         } else if scopeA.is_some() {
        //             scopeA.map(|s| s.scope)
        //         } else {
        //             // If scopeA is None, the scopeB will be Some.
        //             scopeB.map(|s| s.scope)
        //         };
        //         let typedScopeB = if linkedB.flowsFromBottom() {
        //             None
        //         } else if scopeB.is_some() {
        //             scopeB.map(|s| s.scope)
        //         } else {
        //             // If scopeB is None, the scopeA will be Some.
        //             scopeA.map(|s| s.scope)
        //         };

        //         let bestScope = if typedScopeA.is_some() {
        //             typedScopeA
        //         } else {
        //             // If typedScopeA is None, the typedScopeB will be Some.
        //             typedScopeB
        //         };
        //         let bestScope = if let Some(bestScope) = bestScope {
        //             bestScope
        //         } else {
        //             if let Some(scopeA) = scopeA {
        //                 scopeA.scope
        //             } else {
        //                 // If scopeA is None, the scopeB will be Some.
        //                 scopeB.unwrap().scope
        //             }
        //         };
        //         return OverlayScope::new_with_slots(
        //             bestScope,
        //             slotsA.reconcile(slotsB, |slotKey, slotA, slotB| {
        //                 debug_assert!(slotA.is_some() || slotB.is_some());
        //                 // There are 5 different join cases:
        //                 // 1) The type is present in joinedScopeA, not in joinedScopeB,
        //                 //    and not in functionScope. Just use the one in A.
        //                 // 2) The type is present in joinedScopeB, not in joinedScopeA,
        //                 //    and not in functionScope. Just use the one in B.
        //                 // 3) The type is present in functionScope and joinedScopeA, but
        //                 //    not in joinedScopeB. Join the two types.
        //                 // 4) The type is present in functionScope and joinedScopeB, but
        //                 //    not in joinedScopeA. Join the two types.
        //                 // 5) The type is present in joinedScopeA and joinedScopeB. Join
        //                 //    the two types.
        //                 let name = if let Some(slotA) = slotA {
        //                     slotA.name
        //                 } else {
        //                     // If scopeA is None, the scopeB will be Some.
        //                     slotB.unwrap().name
        //                 };
        //                 if slotB.is_none() || slotB.unwrap().getType().is_none() {
        //                     let fnSlot = if let Some(typedScopeB) = typedScopeB {
        //                         typedScopeB.getSlot(&name)
        //                     } else {
        //                         None
        //                     };
        //                     let fnSlotType = if let Some(fnSlot) = fnSlot {
        //                         fnSlot.ty
        //                     } else {
        //                         None
        //                     };
        //                     if fnSlotType.is_none()
        //                         || (slotA.is_some()
        //                             && slotA.unwrap().getType().is_some()
        //                             && identical(
        //                                 fnSlotType.unwrap(),
        //                                 slotA.unwrap().getType().unwrap(),
        //                             ))
        //                     {
        //                         // Case #1
        //                         return slotA.unwrap();
        //                     } else {
        //                         // Case #3
        //                         let joinedType = slotA
        //                             .unwrap()
        //                             .getType()
        //                             .unwrap()
        //                             .getLeastSupertype(fnSlotType.unwrap());
        //                         return if identical(joinedType, slotA.unwrap().getType().unwrap()) {
        //                             slotA.unwrap()
        //                         } else {
        //                             OverlaySlot::new(name, Some(joinedType))
        //                         };
        //                     }
        //                 } else if slotA.is_none() || slotA.unwrap().getType().is_none() {
        //                     let fnSlot = if let Some(typedScopeA) = typedScopeA {
        //                         typedScopeA.getSlot(&name)
        //                     } else {
        //                         None
        //                     };
        //                     let fnSlotType = if let Some(fnSlot) = fnSlot {
        //                         fnSlot.ty
        //                     } else {
        //                         None
        //                     };
        //                     if fnSlotType.is_none()
        //                         || identical(fnSlotType, slotB.unwrap().getType().unwrap())
        //                     {
        //                         // Case #2
        //                         return slotB;
        //                     } else {
        //                         // Case #4
        //                         let joinedType = slotB.getType().getLeastSupertype(fnSlotType);
        //                         return if identical(joinedType, slotB.getType()) {
        //                             slotB
        //                         } else {
        //                             OverlaySlot::new(name, joinedType)
        //                         };
        //                     }
        //                 }
        //                 // Case #5
        //                 if identical(slotA.getType(), slotB.getType()) {
        //                     return slotA;
        //                 }
        //                 let joinedType = slotA.getType().getLeastSupertype(slotB.getType());
        //                 return if identical(joinedType, slotA.getType()) {
        //                     slotA
        //                 } else {
        //                     OverlaySlot::new(name, joinedType)
        //                 };
        //             }),
        //         );
        //     },
        // );
    }
}

fn identical<T>(left: &T, right: &T) -> bool {
    left as *const T == right as *const T
}

/** Join the two FlowScopes. */
pub struct FlowScopeJoinOp<'tcx> {
    result: Option<LinkedFlowScope<'tcx>>,
    // final CompilerInputProvider inputProvider;
}

impl<'tcx> FlowScopeJoinOp<'tcx> {
    pub fn new(/*CompilerInputProvider inputProvider*/) -> Self {
        // this.inputProvider = inputProvider;
        Self { result: None }
    }
}

impl<'tcx> FlowJoiner<'tcx,LinkedFlowScope<'tcx>> for FlowScopeJoinOp<'tcx> {
    // NOTE(sdh): When joining flow scopes with different syntactic scopes,
    // we do not attempt to recover the correct syntactic scope.  This is
    // okay because joins only occur in two situations: (1) performed by
    // the DataFlowAnalysis class automatically between CFG nodes, and (2)
    // requested manually while traversing a single expression within a CFG
    // node.  The syntactic scope is always set at the beginning of flowing
    // through a CFG node.  In the case of (1), the join result's syntactic
    // scope is immediately replaced with the correct one when we flow through
    // the next node.  In the case of (2), both inputs will always have the
    // same syntactic scope.  So simply propagating either input's scope is
    // perfectly fine.
    fn joinFlow(&mut self,ctx: &Ctx<'tcx>, input: LinkedFlowScope<'tcx>) {
        // To join the two scopes, we have to
        let result = match &self.result {
            Some(result) => result,
            None => {
                self.result = Some(input);
                return;
            }
        };
        if result.scopes == input.scopes && result.functionScope == input.functionScope {
            return;
        }

        // NOTE: it would be nice to put 'null' as the syntactic scope if they're not
        // equal, but this is not currently feasible.  For joins that occur within a
        // single CFG node's flow, it's irrelevant, but for joins between separate
        // CFG nodes, there is *one* place where the syntactic scope is actually used:
        // when joining more than two scopes, the first two scopes are joined, and
        // then the join result is joined with the third.  When joining, we look up
        // the types (and existence) of vars in one scope in the other; so when a var
        // from the third scope (say, a local) is missing from the join result, it
        // looks through the syntactic scope before realizing  this.  A quick fix
        // might be to just check that the scope is non-null before trying to join;
        // a better long-term fix would be to improve how we do joins to avoid
        // excessive map entry creation: find a common ancestor, etc.  One
        // interesting consequence of the current approach is that we may end up
        // adding irrelevant block-local variables to the joined scope unnecessarily.
        let common = LinkedFlowScope::getCommonParentDeclarationScope(ctx,&result, &input);
        let result = LinkedFlowScope::new(
            //   inputProvider,
            LinkedFlowScope::join(&result, &input, common),
            common,
            if result.flowsFromBottom(ctx,) {
                input.functionScope
            } else {
                result.functionScope
            },
        );
        self.result = Some(result);
    }

    fn finish(self) -> LinkedFlowScope<'tcx> {
        todo!();
        // self.result
    }
}

#[derive(Clone, PartialEq, Eq)]
struct OverlayScope<'tcx> {
    scope: ScopeId,
    pub slots: HashMap<JsWord, OverlaySlot<'tcx>>,
}

impl<'tcx, 'ast> OverlayScope<'tcx> {
    pub fn new(scope: ScopeId) -> Self {
        Self {
            scope,
            slots: Default::default(),
        }
    }

    pub fn new_with_slots(scope: ScopeId, slots: HashMap<JsWord, OverlaySlot<'tcx>>) -> Self {
        Self { scope, slots }
    }

    pub fn infer(&self, name: &JsWord, ty: Ty<'tcx>) -> OverlayScope<'tcx> {
        // TODO(sdh): variants that do or don't clobber properties (i.e. look up and modify instead)
        if let Some(slot) = self.slots.get(&name.clone()) {
            // TODO: im not sure reference equality is right here.
            // if identical(ty, slot.ty) {
            //   return self;
            // }
            if let Some(slot_ty) = &slot.ty {
                if identical(&ty, slot_ty) {
                    return self.clone();
                }
            }
        }
        OverlayScope::new_with_slots(
            self.scope,
            self.slots
                .update(name.clone(), OverlaySlot::new(name.clone(), Some(ty))),
        )
    }

    pub fn get_slot(&self,ctx: &Ctx<'tcx>, name: &JsWord) -> Option<Slot> {
        match self.slots.get(name) {
            Some(slot) => Some(Slot::Overlay(slot)),
            None => ctx.scopes[self.scope].getSlot(name).map(|slot| Slot::Typed(slot)),
        }
    }
}

pub enum Slot<'tcx, 'a> {
    Typed(TypedVarId),
    Overlay(&'a OverlaySlot<'tcx>),
}

#[derive(Clone, PartialEq, Eq)]
pub struct OverlaySlot<'tcx> {
    name: JsWord,
    ty: Option<Ty<'tcx>>,
}

impl<'tcx> OverlaySlot<'tcx> {
    pub fn new(name: JsWord, ty: Option<Ty<'tcx>>) -> Self {
        Self { name, ty }
    }
}

impl StaticSlot for OverlaySlot<'_> {
    fn getName(&self) {
        todo!()
    }

    fn getDeclaration(&self) {
        todo!()
    }

    fn getJSDocInfo(&self) {
        todo!()
    }

    fn getScope(&self) {
        todo!()
    }
}
impl<'tcx> StaticTypedSlot for OverlaySlot<'tcx> {
    fn getType(&self) -> Option<Ty> {
        todo!()
    }

    fn isTypeInferred(&self) -> bool {
        todo!()
    }
}
