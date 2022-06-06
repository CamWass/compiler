use index::newtype_index;

pub mod color;
pub mod color_collector;
pub mod color_registry;

newtype_index! {
    pub struct ColorId {
        DEBUG_FORMAT = "ColorId({})"
    }
}
