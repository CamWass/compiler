use super::ColorGraphNode::ColorGraphNodeId;

#[derive(Clone, Copy, Debug)]
pub enum Invalidation {
    /** Certain well-known properties like "prototype" are always invalidated. */
    WELL_KNOWN_PROPERTY,
    /** Properties accessed on invalidating types (like Object) are invalidated. */
    INVALIDATING_TYPE { reciever_type: ColorGraphNodeId },
    /** Properties accessed on types that don't declare them are invalidated. */
    UNDECLARED_ACCESS { reciever_type: ColorGraphNodeId },
}
