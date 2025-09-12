use super::char_literals;

/// Test whether a given character code starts an identifier.
///
/// https://tc39.github.io/ecma262/#prod-IdentifierStart
#[inline]
pub fn is_ident_start(character: char) -> bool {
    // // TODO (swc): Use Unicode ID instead of XID.
    match character {
        '$' | '_' | 'A'..='Z' | 'a'..='z' => true,
        _ => {
            if character.is_ascii() {
                false
            } else {
                unicode_id_start::is_id_start(character)
            }
        }
    }
}

/// Test whether a given character is part of an identifier.
#[inline]
pub fn is_ident_part(character: char) -> bool {
    // // TODO (swc): Use Unicode ID instead of XID.
    match character {
        '$'
        | '_'
        | char_literals::ZERO_WIDTH_NON_JOINER
        | char_literals::ZERO_WIDTH_JOINER
        | '0'..='9'
        | 'A'..='Z'
        | 'a'..='z' => true,
        _ => {
            if character.is_ascii() {
                false
            } else {
                unicode_id_start::is_id_continue(character)
            }
        }
    }
}
