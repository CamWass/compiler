extern crate unicode_xid;

use unicode_xid::UnicodeXID;

/// Test whether a given character code starts an identifier.
///
/// https://tc39.github.io/ecma262/#prod-IdentifierStart
// #[inline]
pub fn is_ident_start(character: char) -> bool {
    // todo!();
    // // TODO (swc): Use Unicode ID instead of XID.
    match character {
        '$' | '_' | 'A'..='Z' | 'a'..='z' => true,
        _ => {
            if character.is_ascii() {
                false
            } else {
                UnicodeXID::is_xid_start(character)
            }
        }
    }
}

/// Test whether a given character is part of an identifier.
// #[inline]
pub fn is_ident_part(character: char) -> bool {
    // todo!();
    // // TODO (swc): Use Unicode ID instead of XID.
    match character {
        '$' | '_' /*| 8204 | 8205 */| 'A'..='Z' | 'a'..='z' => true,
        _ => {
            if character.is_ascii() {
                false
            } else {
                UnicodeXID::is_xid_continue(character)
            }
        }
    }
}
