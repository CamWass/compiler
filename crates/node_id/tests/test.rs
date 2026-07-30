use node_id::GetNodeIdMacro;

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct NodeId(u32);

pub trait GetNodeId {
    fn node_id(&self) -> NodeId;
}

#[derive(GetNodeIdMacro, Clone, Copy)]
struct Node1 {
    pub node_id: NodeId,
}

#[derive(GetNodeIdMacro)]
enum SomeNode {
    Node1(Node1),
}

#[test]
fn node_id_macro() {
    let node = Node1 { node_id: NodeId(0) };
    let e = SomeNode::Node1(node);

    assert_eq!(node.node_id, NodeId(0));
    assert_eq!(node.node_id(), NodeId(0));
    assert_eq!(e.node_id(), NodeId(0));
}
