use crate::types::*;

// /**
//  * A `BaseNodeFactory` is an abstraction over an `ObjectAllocator` that handles caching `Node` constructors
//  * and allocating `Node` instances based on a set of predefined types.
//  */
// /* @internal */
// pub trait BaseNodeFactory {
//     fn createBaseSourceFileNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T;
//     fn createBaseIdentifierNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T;
//     fn createBasePrivateIdentifierNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T;
//     fn createBaseTokenNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T;
//     fn createBaseNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T;
// }

// #[derive(Default)]
// pub struct DefaultBaseNodeFactory {}

// impl BaseNodeFactory for DefaultBaseNodeFactory {
//     fn createBaseSourceFileNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T {
//         todo!();
//         // SourceFileConstructor(kind, /*pos*/ -1, /*end*/ -1)
//     }

//     fn createBaseIdentifierNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T {
//         todo!();
//         // IdentifierConstructor(kind, /*pos*/ -1, /*end*/ -1)
//     }

//     fn createBasePrivateIdentifierNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T {
//         todo!();
//         // PrivateIdentifierConstructor(kind, /*pos*/ -1, /*end*/ -1)
//     }

//     fn createBaseTokenNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T {
//         todo!();
//         // TokenConstructor(kind, /*pos*/ -1, /*end*/ -1)
//     }

//     fn createBaseNode<T: HasNodeData>(&mut self, kind: SyntaxKind) -> T {
//         todo!();
//         // NodeConstructor(kind, /*pos*/ -1, /*end*/ -1)
//     }
// }

// /**
//  * Creates a `BaseNodeFactory` which can be used to create `Node` instances from the constructors provided by the object allocator.
//  */
// pub fn createBaseNodeFactory() -> DefaultBaseNodeFactory {
//     DefaultBaseNodeFactory {}
// }
