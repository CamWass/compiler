use super::{js_type::JSType, value_type::ValueType};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StringType {}

impl JSType for StringType{}
impl ValueType for StringType{}