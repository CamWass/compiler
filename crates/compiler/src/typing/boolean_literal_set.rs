/// A set in the domain {`true`,`false`}.
/// There are four possible sets: {}, {`true`}, {`false`}, {`true`,`false`}.
#[derive(Clone, Copy)]
pub enum BooleanLiteralSet {
    EMPTY,
    TRUE,
    FALSE,
    BOTH,
}
impl BooleanLiteralSet {
    /// Computes the intersection of this set and `other`.
    pub fn intersection(&self, other: BooleanLiteralSet) -> BooleanLiteralSet {
        BooleanLiteralSet::from(*self as u8 & other as u8)
    }

    /// Computes the union of this set and `other`.
    pub fn union(&self, other: BooleanLiteralSet) -> BooleanLiteralSet {
        BooleanLiteralSet::from(*self as u8 | other as u8)
    }

    // /**
    //  * Returns whether {@code this} contains the given literal value.
    //  */
    // public boolean contains(boolean literalValue) {
    //   switch (this.ordinal()) {
    //     case 0: return false;
    //     case 1: return literalValue;
    //     case 2: return !literalValue;
    //     case 3: return true;
    //     default: throw new IndexOutOfBoundsException("Ordinal: " +
    //         this.ordinal());
    //   }
    // }

    /// Returns the singleton set {`literal_value`}.
    fn get(literal_value: bool) -> BooleanLiteralSet {
        if literal_value {
            BooleanLiteralSet::TRUE
        } else {
            BooleanLiteralSet::FALSE
        }
    }

    // /** Converts to a Tri. */
    // public Tri toTri() {
    //   if (this == TRUE) {
    //     return Tri.TRUE;
    //   } else if (this == FALSE) {
    //     return Tri.FALSE;
    //   }
    //   return Tri.UNKNOWN;
    // }
}

impl From<u8> for BooleanLiteralSet {
    fn from(ordinal: u8) -> Self {
        match ordinal {
            0 => BooleanLiteralSet::EMPTY,
            1 => BooleanLiteralSet::TRUE,
            2 => BooleanLiteralSet::FALSE,
            3 => BooleanLiteralSet::BOTH,
            _ => panic!(),
        }
    }
}
