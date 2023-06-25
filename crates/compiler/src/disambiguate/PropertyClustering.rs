use super::ColorGraphNode::ColorGraphNodeId;
use super::Invalidation::Invalidation;
use crate::graph::StandardUnionFind::StandardUnionFind;
use atoms::JsWord;
use index::newtype_index;
use rustc_hash::FxHashMap;

/**
 * The disambiguation clusters for a given property name.
 *
 * <p>This is a struct used to aggregate information to be processed by other classes in this
 * package. It is intentionally mutable and doesn't attempt to enforce invariants in the contents of
 * its datastructures. Instances trust that sibling classes make mutations correctly.
 */
#[derive(Debug)]
pub struct PropertyClustering {
    pub name: JsWord,
    // /**
    //  * The locations properties with this name were used in the program, mapping to their receiver
    //  * type.
    //  *
    //  * <p>This index allows property references to be efficiently renamed once all clusters have been
    //  * found. It prevents us from re-traversing the code.
    //  */
    pub useSites: FxHashMap<ast::NodeId, ColorGraphNodeId>,
    pub clusters: StandardUnionFind<ColorGraphNodeId>,
    originalNameClusterRep: Option<ColorGraphNodeId>,
    lastInvalidation: Option<Invalidation>,
}

impl PropertyClustering {
    pub fn new(name: JsWord) -> Self {
        Self {
            name,
            useSites: FxHashMap::default(),
            clusters: StandardUnionFind::default(),
            originalNameClusterRep: None,
            lastInvalidation: None,
        }
    }

    /**
     * The current representative of the cluster of types whose properties must keep their original
     * name.
     *
     * <p>The following refers to all computations with respect to a single property name. Since
     * extern and enum properties cannot be renamed, all other types in a cluster with an extern type
     * or enum cannot rename their property either. In theory, there could be many such clusters
     * containing an extern or enum; however, in practice we conflate them into one. This is
     * equivalent because all of those clusters would end up using the same, unchanged, property name.
     * This representation also simplifies tracking of such clusters.
     *
     * <p>In practice the types in this cluster include
     *
     * <ul>
     *   <li>externs, whose properties cannot be renamed without breaking references to external code
     *   <li>enums, e.g. for const Actions = {STOP: 0, GO: 1}; the disambiguator will not rename STOP
     *       or GO
     *   <li>boxable scalars, like string and number
     * </ul>
     *
     * <p>Note that enum properties could probably be safely renamed, but this would require cleaning
     * up code depending on the legacy no-renaming behavior.
     */
    pub fn getOriginalNameClusterRep(&mut self) -> Option<ColorGraphNodeId> {
        debug_assert!(!self.isInvalidated());
        if let Some(originalNameClusterRep) = self.originalNameClusterRep {
            Some(self.clusters.find(originalNameClusterRep))
        } else {
            None
        }
    }

    pub fn isInvalidated(&self) -> bool {
        self.lastInvalidation.is_some()
    }

    pub fn invalidate(&mut self, invalidation: Invalidation) {
        self.lastInvalidation = Some(invalidation);
    }

    /**
     * Indicate that all property references off this {@link ColorGraphNode} must keep their original
     * name.
     *
     * <p>See {@link #getOriginalNameClusterRep()} for more details.
     */
    pub fn registerOriginalNameType(&mut self, ty: ColorGraphNodeId) {
        debug_assert!(!self.isInvalidated());
        let originalNameClusterRep = match self.originalNameClusterRep {
            Some(rep) => rep,
            None => {
                self.originalNameClusterRep = Some(ty);
                ty
            }
        };
        self.clusters.union(originalNameClusterRep, ty);
    }
}

newtype_index! {
    pub struct PropertyClusteringId {
        DEBUG_FORMAT = "PropertyClusteringId({})"
    }
}
