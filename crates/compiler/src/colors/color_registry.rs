use super::color::Color;
use super::ColorId;
use crate::ast;
use crate::node::{Bind, BoundNode};
use crate::types::*;
use crate::types_composition::*;
use crate::utils::*;
use crate::visit::{Visit, VisitWith};
use crate::Checker;
use crate::CompProgram;
use ahash::{AHashMap, AHashSet};
use index::{
    newtype_index,
    vec::{Idx, IndexVec},
};
use std::iter::FromIterator;
use std::rc::Rc;
use swc_atoms::{js_word, JsWord};

#[derive(Debug)]
pub struct ColorRegistry {
    pub colors: IndexVec<ColorId, Color>,
    pub node_to_color_map: AHashMap<BoundNode, ColorId>,
    pub disambiguationSupertypeGraph: AHashMap<ColorId, AHashSet<ColorId>>,
    /// Maps instantiable color -> union of instance colors.
    instanceColorUnions: AHashMap<ColorId, ColorId>,

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
    pub fn new() -> Self {
        let mut colors: IndexVec<ColorId, Color> = IndexVec::default();

        macro_rules! populate_standard_colors {
            [$( ($name:ident, $color:expr)$(,)? )*] => {
                let mut start = colors.next_index();
                $(
                    let $name = start;
                    start = start.plus(1);
                )*
                colors.extend(std::array::IntoIter::new([$($color,)*]));
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
            node_to_color_map: AHashMap::default(),
            disambiguationSupertypeGraph: AHashMap::default(),
            instanceColorUnions: AHashMap::default(),

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

    /**
     * The colors directly above `x` in the subtyping graph for the purposes of property
     * (dis)ambiguation.
     */
    pub fn getDisambiguationSupertypes(&self, x: ColorId) -> Option<&AHashSet<ColorId>> {
        self.disambiguationSupertypeGraph.get(&x)
    }

    pub fn get_color_of_node(&self, node: &BoundNode) -> Option<ColorId> {
        self.node_to_color_map.get(node).copied()
    }

    // TODO: temp
    pub fn debug_get_color(&self, color: ColorId) -> &Color {
        &self.colors[color]
    }

    // pub fn createUnionOfInstanceColors(&mut self, color: ColorId) -> ColorId {
    //     let elements = &self.colors[color].instanceColors;

    //     debug_assert!(!elements.is_empty());

    //     if elements.len() == 1 {
    //         return *elements.iter().next().unwrap();
    //     }

    //     if let Some(cached) = self.instanceColorUnions.get(&color) {
    //         return *cached;
    //     }

    //     let mut prototypes = AHashSet::default();
    //     let mut instanceColors = AHashSet::default();
    //     // let mut ownProperties = AHashSet::default();
    //     let mut isInvalidating = false;

    //     for &element in elements {
    //         prototypes.extend(self.colors[element].prototypes.iter());
    //         instanceColors.extend(self.colors[element].instanceColors.iter());
    //         // ownProperties.extend(self.colors[element].ownProperties.iter().cloned()); // Are these actually the "own props"?
    //         isInvalidating |= self.colors[element].isInvalidating;
    //     }

    //     let result = Color {
    //         isInvalidating,
    //         // ownProperties,
    //         prototypes,
    //         instanceColors,
    //         unionElements: elements.clone(),
    //     };

    //     let result = self.colors.push(result);

    //     self.instanceColorUnions.insert(color, result);

    //     result
    // }
}
