use crate::{node::Node, types::*};

use std::rc::Rc;

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeId(u32);

impl NodeId {
    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
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

    fn modifiers(&self) -> Option<&NodeArray<Modifier>>;
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

    fn modifiers(&self) -> Option<&NodeArray<Modifier>> {
        self.as_ref().modifiers()
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

pub struct NodeAndData<'n, 'd, T: IsNode>(pub &'n T, pub &'d NodeData);

#[derive(Default)]
pub struct NodeDataStore {
    pub node_id_gen: NodeIdGen,
    pub node_data: Vec<Option<NodeData>>,
}

impl NodeDataStore {
    fn ensure_node_data_capacity(&mut self, node_id: NodeId) {
        if node_id.as_usize() >= self.node_data.len() {
            let additional = node_id.as_usize() - self.node_data.len() + 1;
            self.node_data.reserve(additional);
            for _ in 0..additional {
                self.node_data.push(None);
            }
        }
    }

    pub fn node_data<T: HasNodeId>(&mut self, node: T) -> &NodeData {
        self.node_data_mut(node)
    }

    pub fn node_data_mut<T: HasNodeId>(&mut self, node: T) -> &mut NodeData {
        let node_id = node.node_id();
        self.ensure_node_data_capacity(node_id);
        self.node_data[node_id.as_usize()].get_or_insert_with(|| NodeData::default())
    }

    pub fn node_and_data<'n, 'd, T: IsNode>(&'d mut self, node: &'n T) -> NodeAndData<'n, 'd, T> {
        let data = self.node_data(node);
        NodeAndData(node, data)
    }
}
