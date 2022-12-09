use crate::{node::Node, types::SyntaxKind};

use std::rc::Rc;

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeId(u32);

#[derive(Clone, Debug)]
pub struct NodeIdGen {
    cur: NodeId,
}

impl Default for NodeIdGen {
    fn default() -> Self {
        Self { cur: NodeId(0) }
    }
}

impl NodeIdGen {
    pub fn next(&mut self) -> NodeId {
        // Incrementing after we take the id ensures that NodeId(0) is used.
        let id = self.cur;
        self.cur.0 += 1;
        id
    }
}

pub trait HasNodeId {
    fn node_id(&self) -> NodeId;
}

impl<T> HasNodeId for &T
where
    T: HasNodeId,
{
    fn node_id(&self) -> NodeId {
        (*self).node_id()
    }
}

impl<T> HasNodeId for Rc<T>
where
    T: HasNodeId,
{
    fn node_id(&self) -> NodeId {
        self.as_ref().node_id()
    }
}

impl HasNodeId for NodeId {
    fn node_id(&self) -> NodeId {
        *self
    }
}

pub trait IsNode: HasNodeId {
    fn kind(&self) -> SyntaxKind;

    fn name(&self) -> Option<Node>;

    fn isPropertyName(&self) -> bool;
}

impl<T> IsNode for Rc<T>
where
    T: IsNode,
{
    fn kind(&self) -> SyntaxKind {
        self.as_ref().kind()
    }

    fn name(&self) -> Option<Node> {
        self.as_ref().name()
    }

    fn isPropertyName(&self) -> bool {
        self.as_ref().isPropertyName()
    }
}

// macro_rules! impl_is_node {
//     ($t:ty) => {
//         impl HasNodeId for $t {
//             fn node_id(&self) -> NodeId {
//                 self.node_id
//             }
//         }

//         impl Into<Node> for Rc<$t> {
//             fn into(self) -> Node {
//                 Node::$t(self)
//             }
//         }
//     };
// }

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Tristate {
    False,
    True,
    Unknown,
}

pub trait IsFalsy {
    fn is_falsy(&self) -> bool;
}

impl IsFalsy for bool {
    fn is_falsy(&self) -> bool {
        !self
    }
}

impl IsFalsy for Tristate {
    fn is_falsy(&self) -> bool {
        *self == Tristate::False
    }
}

impl<T> IsFalsy for Option<T> {
    fn is_falsy(&self) -> bool {
        self.is_none()
    }
}
