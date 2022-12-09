use crate::types::*;

pub fn setTextRange<T: HasTextRange>(range: &mut T, location: Option<TextRange>) {
    if let Some(location) = location {
        range.setTextRangePosEnd(location.pos, location.end);
    }
}
