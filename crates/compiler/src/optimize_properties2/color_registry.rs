use super::color::*;
use index::vec::{Idx, IndexVec};
use std::ops::{Index, IndexMut};

#[derive(Debug)]
pub struct ColorRegistry {
    pub colors: IndexVec<ColorId, Color>,
    // node_to_color_map: FxHashMap<NodeId, ColorId>,
    // supertype_graph: FxHashMap<ColorId, FxHashSet<ColorId>>,

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
                let mut _start = colors.next_index();
                $(
                    let $name = _start;
                    _start = _start.plus(1);
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
