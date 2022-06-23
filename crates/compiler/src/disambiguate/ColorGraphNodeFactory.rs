use super::ColorGraphNode::{ColorGraphNode, ColorGraphNodeId};
use crate::colors::{color_registry::ColorRegistry, ColorId};
use index::vec::IndexVec;
use rustc_hash::FxHashMap;

pub struct ColorGraphNodeFactory<'c> {
    typeIndex: FxHashMap<ColorId, ColorGraphNodeId>,
    // TODO: pub for testing
    pub nodes: IndexVec<ColorGraphNodeId, ColorGraphNode>,
    pub colours: &'c mut ColorRegistry,
}

impl<'c> ColorGraphNodeFactory<'c> {
    pub fn new(colours: &'c mut ColorRegistry) -> Self {
        let mut typeIndex = FxHashMap::default();
        let mut nodes = IndexVec::default();

        let unknown_type = colours.unknown_color;

        let unknownTypeNode = ColorGraphNode::new(unknown_type);
        let unknownTypeNodeId = nodes.push(unknownTypeNode);
        typeIndex.insert(unknown_type, unknownTypeNodeId);

        Self {
            typeIndex,
            nodes,
            colours,
        }
    }

    /**
     * Returns the {@link ColorGraphNode} known by this factory for {@code type}.
     *
     * <p>For a given {@code type} and factory, this method will always return the same result. The
     * results are cached.
     */
    pub fn createNode(&mut self, ty: Option<ColorId>) -> ColorGraphNodeId {
        let key = self.simplifyColor(ty);
        match self.typeIndex.get(&key) {
            Some(node) => *node,
            None => {
                let id = self.nodes.push(ColorGraphNode::new(key));
                self.typeIndex.insert(key, id);
                id
            }
        }
    }

    pub fn getAllKnownTypes(&self) -> Vec<ColorGraphNodeId> {
        self.typeIndex.values().copied().collect::<Vec<_>>()
    }

    // Merges different colors with the same ambiguation-behavior into one
    fn simplifyColor(&self, ty: Option<ColorId>) -> ColorId {
        match ty {
            Some(t) => t,
            None => {
                todo!();
                // self.colours.unknown_color
            }
        }
        // if (type == null) {
        //   return StandardColors.UNKNOWN;
        // }

        // if (type.isUnion()) {
        //   // First remove null/void, then recursively simplify any primitive components
        //   type = type.subtractNullOrVoid();
        //   return type.isUnion()
        //       ? Color.createUnion(
        //           type.getUnionElements().stream().map(this::simplifyColor).collect(toImmutableSet()))
        //       : simplifyColor(type);
        // } else if (type.getBoxId() != null) {
        //   return this.registry.get(type.getBoxId());
        // } else if (type.equals(StandardColors.NULL_OR_VOID)) {
        //   return StandardColors.UNKNOWN;
        // } else {
        //   return type;
        // }
    }
}

impl std::ops::Index<ColorGraphNodeId> for ColorGraphNodeFactory<'_> {
    type Output = ColorGraphNode;
    #[inline]
    fn index(&self, index: ColorGraphNodeId) -> &ColorGraphNode {
        &self.nodes[index]
    }
}

impl std::ops::IndexMut<ColorGraphNodeId> for ColorGraphNodeFactory<'_> {
    #[inline]
    fn index_mut(&mut self, index: ColorGraphNodeId) -> &mut ColorGraphNode {
        &mut self.nodes[index]
    }
}

// TODO: tests from closure
