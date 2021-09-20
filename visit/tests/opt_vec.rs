use global_visit::define;
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
