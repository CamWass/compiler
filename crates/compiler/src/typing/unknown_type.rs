use super::{js_type::JSType, object_type::ObjectType};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct UnknownType {
    // See the explanation of checked unknown types in JSTypeNative.
    isChecked: bool,
}

impl UnknownType {
    pub fn new(isChecked: bool) -> Self {
        Self { isChecked }
    }
}

impl JSType for UnknownType {}
impl ObjectType for UnknownType {}
