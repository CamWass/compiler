use super::color::Color;
use super::ColorId;
use ast::NodeId;
use index::vec::{Idx, IndexVec};
use rustc_hash::{FxHashMap, FxHashSet};
use std::ops::{Index, IndexMut};

#[derive(Debug)]
pub struct ColorRegistry {
    colors: IndexVec<ColorId, Color>,
    node_to_color_map: FxHashMap<NodeId, ColorId>,
    supertype_graph: FxHashMap<ColorId, FxHashSet<ColorId>>,

    // Standard colours:
    pub unknown_color: ColorId,
    pub bigint_color: ColorId,
    pub boolean_color: ColorId,
    pub null_or_void_color: ColorId,
    pub number_color: ColorId,
    pub string_color: ColorId,
    pub symbol_color: ColorId,
    pub top_object_color: ColorId,
}

impl ColorRegistry {
    /// Creates a new color registry containing only the standard colors.
    pub fn new() -> Self {
        let mut colors: IndexVec<ColorId, Color> = IndexVec::default();

        macro_rules! populate_standard_colors {
            [$( ($name:ident, $color:expr)$(,)? )*] => {
                let mut start = colors.next_index();
                $(
                    let $name = start;
                    start = start.plus(1);
                )*
                colors.extend([$($color,)*]);
            };
        }

        populate_standard_colors![
            (unknown_color, Color::unknown()),
            (bigint_color, Color::bigint()),
            (boolean_color, Color::boolean()),
            (null_or_void_color, Color::null_or_void()),
            (number_color, Color::number()),
            (string_color, Color::string()),
            (symbol_color, Color::symbol()),
            (top_object_color, Color::top_object()),
        ];

        Self {
            colors,
            node_to_color_map: FxHashMap::default(),
            supertype_graph: FxHashMap::default(),

            unknown_color,
            bigint_color,
            boolean_color,
            null_or_void_color,
            number_color,
            string_color,
            symbol_color,
            top_object_color,
        }
    }

    /// Creates a blank color and returns its ID.
    pub fn new_color(&mut self) -> ColorId {
        let color = Color::default();
        self.colors.push(color)
    }

    /// Adds an edge between `super_color` and `sub_color` in the subtyping graph.
    pub fn add_supertype_edge(&mut self, super_color: ColorId, sub_color: ColorId) {
        self.supertype_graph
            .entry(sub_color)
            .or_default()
            .insert(super_color);
    }

    /// The colors directly above `x` in the subtyping graph.
    pub fn get_supertypes(&self, x: ColorId) -> Option<&FxHashSet<ColorId>> {
        self.supertype_graph.get(&x)
    }

    /// Retrieves the color associated with the node, if any.
    pub fn get_color_of_node(&self, node_id: NodeId) -> Option<ColorId> {
        self.node_to_color_map.get(&node_id).copied()
    }

    /// Sets the color associated with the node, overwiting any previous color.
    pub fn set_color_of_node(&mut self, node_id: NodeId, color: ColorId) {
        self.node_to_color_map.insert(node_id, color);
    }
}

impl Index<ColorId> for ColorRegistry {
    type Output = Color;

    #[inline]
    fn index(&self, index: ColorId) -> &Self::Output {
        &self.colors[index]
    }
}

impl IndexMut<ColorId> for ColorRegistry {
    #[inline]
    fn index_mut(&mut self, index: ColorId) -> &mut Self::Output {
        &mut self.colors[index]
    }
}
