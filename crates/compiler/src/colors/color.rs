use super::ColorId;
use rustc_hash::FxHashSet;
use swc_atoms::JsWord;

/** A simplified version of a Closure or TS type for use by optimizations */
#[derive(Default, Debug)]
pub struct Color {
    pub isInvalidating: bool,
    // /**
    //  * Property names 'declared' on an object (as opposed to being conceptually inherited from some
    //  * supertype).
    //  */
    // pub ownProperties: FxHashSet<JsWord>,
    /** Given `function Foo() {}` or `class Foo {}`, color of Foo.prototype. null otherwise. */
    // pub prototypes: FxHashSet<ColorId>,
    pub staticType: Option<ColorId>,
    // pub instanceColors: FxHashSet<ColorId>,
    pub unionElements: FxHashSet<ColorId>,
}

impl Color {
    pub fn isUnion(&self) -> bool {
        // Single element sets are banned in the builder.
        !self.unionElements.is_empty()
    }
}

impl Color {
    pub fn bigint() -> Color {
        //.setBoxId(BIGINT_OBJECT_ID)
        Color::default()
    }

    pub fn boolean() -> Color {
        // .setBoxId(BOOLEAN_OBJECT_ID)
        Color::default()
    }

    pub fn null_or_void() -> Color {
        Color::default()
    }

    pub fn number() -> Color {
        // .setBoxId(NUMBER_OBJECT_ID)
        Color::default()
    }

    pub fn string() -> Color {
        // .setBoxId(STRING_OBJECT_ID)
        Color::default()
    }

    pub fn symbol() -> Color {
        // .setBoxId(SYMBOL_OBJECT_ID)
        Color::default()
    }

    /// The supertype of all objects but not primitives.
    ///
    /// Separate from UNKNOWN because some optimizations back off on any non-object primitives +
    /// unknown but operate on the top object.
    pub fn top_object() -> Color {
        Color {
            isInvalidating: true,
            ..Color::default()
        }
    }

    /// Analagous to TS unknown/any
    pub fn unknown() -> Color {
        Color {
            isInvalidating: true,
            ..Color::default()
        }
    }
}
