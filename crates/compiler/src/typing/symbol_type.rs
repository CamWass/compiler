use super::{js_type::JSType, value_type::ValueType};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SymbolType {}

impl JSType for SymbolType{}
impl ValueType for SymbolType{}