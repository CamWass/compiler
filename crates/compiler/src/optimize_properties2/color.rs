use rustc_hash::FxHashSet;

/** A simplified version of a Closure or TS type for use by optimizations */
#[derive(Default, Debug)]
pub struct Color {
    pub is_invalidating: bool,

    pub union_elements: FxHashSet<ColorId>,
}

impl Color {
    pub fn is_union(&self) -> bool {
        debug_assert_ne!(self.union_elements.len(), 1);
        !self.union_elements.is_empty()
    }
}

impl Color {
    pub fn bigint() -> Color {
        Color::default()
    }

    pub fn boolean() -> Color {
        Color::default()
    }

    pub fn null_or_void() -> Color {
        Color::default()
    }

    pub fn number() -> Color {
        Color::default()
    }

    pub fn string() -> Color {
        Color::default()
    }

    pub fn symbol() -> Color {
        Color::default()
    }

    /// The supertype of all objects but not primitives.
    ///
    /// Separate from UNKNOWN because some optimizations back off on any non-object primitives +
    /// unknown but operate on the top object.
    pub fn top_object() -> Color {
        Color {
            is_invalidating: true,
            ..Color::default()
        }
    }

    /// Analagous to TS unknown/any
    pub fn unknown() -> Color {
        Color {
            is_invalidating: true,
            ..Color::default()
        }
    }
}

index::newtype_index! {
    pub struct ColorId {
        DEBUG_FORMAT = "ColorId({})"
    }
}
