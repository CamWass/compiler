use global_visit::define;
use std::any::Any;

/// Visitable nodes.
pub trait Node: Any {}

impl<T: ?Sized> Node for T where T: Any {}

pub struct Item {
    pub opt_vec1: Option<Vec<Item>>,
    pub opt_vec2: Option<Vec<Enum>>,
}
pub enum Enum {
    Item(Item),
}

define!({
    pub struct Item {
        pub opt_vec1: Option<Vec<Item>>,
        pub opt_vec2: Option<Vec<Enum>>,
    }
    pub enum Enum {
        Item(Item),
    }
});
