use super::{js_type::JSType, value_type::ValueType};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NullType {}

impl JSType for NullType{}
impl ValueType for NullType{}