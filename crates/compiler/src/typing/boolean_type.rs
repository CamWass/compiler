use super::{js_type::JSType, value_type::ValueType};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BooleanType {}

impl JSType for BooleanType{}
impl ValueType for BooleanType{}