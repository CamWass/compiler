use super::PropertyClustering::PropertyClusteringId;
use crate::{colors::ColorId, types::TypeId};
use ahash::AHashMap;
use index::{newtype_index, vec::IndexVec};

// TODO: comment from closure
#[derive(Debug)]
pub struct ColorGraphNode {
    pub color: ColorId,
    pub associatedProps: AHashMap<PropertyClusteringId, PropAssociation>,
    /**
     * An ID used to efficiently construct a unique name for any cluster this node becomes the
     * represenative of.
     */
    // TODO: remove
    index: ColorGraphNodeId,
}

impl ColorGraphNode {
    pub fn new(single: ColorId, index: ColorGraphNodeId) -> Self {
        Self {
            color: single,
            associatedProps: AHashMap::default(),
            index,
        }
    }
}

/**
 * Reasons a property name became associated with a type.
 *
 * <p>This information is only used for debugging. It doesn't affect the behaviour of the pass.
 */
#[derive(Debug)]
pub enum PropAssociation {
    /** Because of an access in the AST (e.g. foo.prop) */
    AST,
    /**
     * Because the type system recorded such an association, without any reason visible in the AST.
     * Usually this means the relevant AST segment has been optimized away by an earlier pass.
     */
    TYPE_SYSTEM,
    /** Because it was inherited from a supertype in the type graph */
    SUPERTYPE,
}

newtype_index! {
    pub struct ColorGraphNodeId {
        DEBUG_FORMAT = "ColorGraphNodeId({})"
    }
}
