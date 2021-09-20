use crate::ctx::Ctx;
use crate::{typed_var::TypedVarId, typing::types::Ty};
use ast::AstNode;
use std::collections::{BTreeMap, BTreeSet};
use std::hash::Hash;
use swc_atoms::JsWord;

index::newtype_index! {
    pub struct ScopeId {
        DEBUG_FORMAT = "ScopeId({})"
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct RootNode<'tcx> {
    pub node: AstNode<'tcx>,
    pub is_function_block: bool,
}

impl<'tcx> RootNode<'tcx> {
    pub fn new_from_non_block(node: AstNode<'tcx>) -> Self {
        debug_assert!(!matches!(node, AstNode::BlockStmt(_)));
        Self {
            node,
            is_function_block: false,
        }
    }
}

// impl <'tcx> PartialEq<AstNode<'tcx>> for RootNode<'tcx> {
//     fn eq(&self, other: &AstNode<'tcx>) -> bool {
//         self.node == *other
//     }
// }

#[derive(PartialEq, Eq, Clone)]
pub struct TypedScope<'tcx> {
    id: ScopeId,
    vars: BTreeMap<JsWord, TypedVarId>,
    rootNode: RootNode<'tcx>,

    parent: Option<ScopeId>,
    depth: usize,

    /// Whether this is a bottom scope for the purposes of type inference.
    isBottom: bool,

    /// Not immutable; will shrink over time.
    reservedNames: BTreeSet<JsWord>,
}

impl<'tcx> TypedScope<'tcx> {
    /**
     * Creates a empty Scope (bottom of the lattice).
     *
     * @param rootNode Typically a FUNCTION node or the global ROOT.
     * @param isBottom Whether this is the bottom of a lattice. Otherwise, it must be a global scope.
     */
    fn new_empty(id: ScopeId, rootNode: RootNode<'tcx>, is_bottom: bool) -> Self {
        // super(rootNode);
        // checkRootScope();
        Self {
            id,
            vars: BTreeMap::new(),
            rootNode,
            parent: None,
            depth: 0,
            isBottom: is_bottom,
            reservedNames: BTreeSet::new(),
            // module: null,
        }
    }

    pub fn createGlobalScope(id: ScopeId, rootNode: RootNode<'tcx>) -> Self {
        TypedScope::new_empty(id, rootNode, false)
    }

    pub fn createLatticeBottom(id: ScopeId, rootNode: RootNode<'tcx>) -> Self {
        TypedScope::new_empty(id, rootNode, true)
    }

    pub fn id(&self) -> ScopeId {
        self.id
    }

    /** Whether this is the bottom of the lattice. */
    pub fn isBottom(&self) -> bool {
        self.isBottom
    }

    pub fn get_depth(&self) -> usize {
        self.depth
    }

    pub fn get_parent(&self) -> Option<ScopeId> {
        self.parent
    }

    pub fn getSlot(&self, name: &JsWord) -> Option<TypedVarId> {
        self.getVar(name)
    }

    pub fn getVar(&self, name: &JsWord) -> Option<TypedVarId> {
        todo!()
    }

    pub fn declare(
        &mut self,
        name: &JsWord,
        nameNode: AstNode,
        ty: Ty,
        inferred: bool,
    ) -> TypedVarId {
        todo!();
        // self.reservedNames.remove(name);
        // let var = TypedVar::new(inferred, name, nameNode, Some(ty), self, self.getVarCount());
        // self.declareInternal(name, var);
        // var
    }
}

// AbstractScope
impl<'tcx> TypedScope<'tcx> {
    /**
     * Gets the container node of the scope. This is typically the FUNCTION node or the global
     * BLOCK/SCRIPT node.
     */
    pub fn getRootNode(&self) -> RootNode<'tcx> {
        self.rootNode
    }

    /** Walks up the tree to find the global scope. */
    pub fn getGlobalScope(&self, ctx: &Ctx) -> ScopeId {
        let mut result = self.id;
        while let Some(parent) = ctx.scopes[result].parent {
            result = parent;
        }
        result
    }

    fn declareInternal(&mut self, name: &JsWord, var: TypedVarId) {
        todo!();
        // debug_assert!(
        //     self.hasOwnSlot(name) || self.canDeclare(name) /*, "Illegal shadow: {}", var.getNode()*/
        // );

        // self.vars.insert(*name, var);
    }

    /** Returns the nearest common parent between two scopes. */
    pub fn getCommonParent(&self, ctx: &Ctx, other: ScopeId) -> ScopeId {
        let mut left = Some(self.id);
        let mut right = Some(other);
        loop {
            if let Some(left_id) = left {
                if let Some(right_id) = right {
                    if left_id != right_id {
                        let left_scope = &ctx.scopes[left_id];
                        let right_scope = &ctx.scopes[right_id];

                        let left_depth = left_scope.get_depth();
                        let right_depth = right_scope.get_depth();
                        if left_depth >= right_depth {
                            left = left_scope.parent;
                        }
                        if left_depth <= right_depth {
                            right = right_scope.parent;
                        }

                        continue;
                    }
                }
            }

            break;
        }

        match left {
            Some(left_scope) if left == right => left_scope,
            _ => unreachable!(),
        }
    }

    /** Returns number of variables in this scope (excluding the special 'arguments' variable) */
    pub fn getVarCount(&self) -> usize {
        self.vars.len()
    }

    /** Returns whether this is the global scope. */
    pub fn isGlobal(&self) -> bool {
        self.parent.is_none()
    }

    /** Returns whether this is a local scope (i.e. not the global scope). */
    pub fn isLocal(&self) -> bool {
        self.parent.is_some()
    }

    // pub fn isBlockScope(&self) -> bool {
    //     NodeUtil.createsBlockScope(rootNode)
    //   }

    pub fn isFunctionBlockScope(&self) -> bool {
        self.rootNode.is_function_block
    }

    pub fn isFunctionScope(&self) -> bool {
        matches!(self.rootNode.node, AstNode::Function(_))
    }

    pub fn isModuleScope(&self) -> bool {
        matches!(self.rootNode.node, AstNode::Module(_))
    }

    //   pub fn isCatchScope(&self) -> bool {
    //      self.rootNode.isBlock()
    //         && self.rootNode.hasOneChild()
    //         && self.rootNode.getFirstChild().isCatch()
    //   }

    /**
     * If a var were declared in this scope, would it belong to this scope (as opposed to some
     * enclosing scope)?
     *
     * We consider function scopes to be hoist scopes. Even though it's impossible to declare a var
     * inside function parameters, it would make less sense to say that if you did declare one in
     * the function parameters, it would be hoisted somewhere else.
     */
    fn isHoistScope(&self) -> bool {
        self.isFunctionScope()
            || self.isFunctionBlockScope()
            || self.isGlobal()
            || self.isModuleScope()
    }

    /**
     * If a var were declared in this scope, return the scope it would be hoisted to.
     *
     * <p>For function scopes, we return back the scope itself, since even though there is no way to
     * declare a var inside function parameters, it would make even less sense to say that such
     * declarations would be "hoisted" somewhere else.
     */
    pub fn getClosestHoistScope(&self, ctx: &'tcx Ctx<'tcx>) -> Option<ScopeId> {
        let mut current = Some(self);
        while let Some(current_scope) = current {
            if current_scope.isHoistScope() {
                return current.map(|s|s.id);
            }
            current = current_scope.parent.map(|id| &ctx.scopes[id]);
        }
        None
    }
}
