use super::{js_type::JSType, value_type::ValueType};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct VoidType {}

impl JSType for VoidType{}
impl ValueType for VoidType{}