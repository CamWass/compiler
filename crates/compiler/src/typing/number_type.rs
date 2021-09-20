use super::{js_type::JSType, value_type::ValueType};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NumberType {}

impl JSType for NumberType{}
impl ValueType for NumberType{}