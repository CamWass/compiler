use super::color::ColorId;
use index::{bit_set::GrowableBitSet, newtype_index};

#[derive(Debug)]
pub struct ColorGraphNode {
    pub color: ColorId,
    pub subtype_indices: GrowableBitSet<ColorGraphNodeId>,
}

impl ColorGraphNode {
    pub fn new(color: ColorId) -> Self {
        Self {
            color,
            subtype_indices: GrowableBitSet::new_empty(),
        }
    }
}

newtype_index! {
    pub struct ColorGraphNodeId {
        DEBUG_FORMAT = "ColorGraphNodeId({})"
    }
}
