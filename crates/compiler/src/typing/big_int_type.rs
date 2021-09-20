use super::{js_type::JSType, value_type::ValueType};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BigIntType {}

impl JSType for BigIntType{}
impl ValueType for BigIntType{}