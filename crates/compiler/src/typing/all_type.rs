use super::{js_type::JSType};
use std::hash::Hash;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AllType {}

impl JSType for AllType{}