use ahash::AHashMap;
use index::{newtype_index, vec::IndexVec};
use std::fmt::Debug;
use std::hash::Hash;

#[derive(Debug)]
pub struct StandardUnionFind<E>
where
    E: Eq + Hash + Copy + Debug,
{
    /** All values with the same root node are in the same equivalence set. */
    elmap: AHashMap<E, NodeId>,
    nodes: IndexVec<NodeId, Node<E>>,
}

impl<E> Default for StandardUnionFind<E>
where
    E: Eq + Hash + Copy + Debug,
{
    fn default() -> Self {
        Self {
            elmap: AHashMap::default(),
            nodes: IndexVec::default(),
        }
    }
}

impl<E> StandardUnionFind<E>
where
    E: Eq + Hash + Copy + Debug,
{
    /**
     * Creates an UnionFind structure being a copy of other structure.
     * The created structure is optimal in a sense that the paths to
     * the root from any element will have a length of at most 1.
     *
     * @param other structure to be copied
     */
    //   public StandardUnionFind(UnionFind<E> other) {
    //     for (E elem : other.elements()) {
    //       union(other.find(elem), elem);
    //     }
    //   }

    pub fn add(&mut self, e: E) {
        self.union(e, e);
    }

    //   pub fn addAll(Iterable<E> es) {
    //     for (E e : es) {
    //       this.add(e);
    //     }
    //   }

    pub fn union(&mut self, a: E, b: E) -> E {
        let nodeAId = self.findRootOrCreateNode(a);
        let nodeBId = self.findRootOrCreateNode(b);

        if nodeAId == nodeBId {
            let nodeA = &self.nodes[nodeAId];
            return nodeA.element;
        }

        let (nodeA, nodeB) = self.nodes.pick2_mut(nodeAId, nodeBId);

        // If possible, prefer nodeA over nodeB, to preserve insertion order.
        if nodeA.rank >= nodeB.rank {
            nodeB.parent = nodeAId;
            nodeA.size += nodeB.size;
            if nodeA.rank == nodeB.rank {
                nodeA.rank += 1;
            }
            return nodeA.element;
        }
        nodeA.parent = nodeBId;
        nodeB.size += nodeA.size;
        let temp = nodeB.element;
        nodeB.element = nodeA.element;
        nodeA.element = temp;
        nodeB.element
    }

    // TODO: maybe make this return Option<E> and remove the debug check and unwrap()
    pub fn find(&mut self, e: E) -> E {
        debug_assert!(
            self.elmap.contains_key(&e),
            "Element does not exist: {:?}",
            e
        );
        let node = self.elmap.get(&e).unwrap();
        let res = self.findRoot(*node);
        self.nodes[res].element
    }

    //   @Override
    //   public boolean areEquivalent(@Nullable E a, @Nullable E b) {
    //     E aRep = find(a);
    //     E bRep = find(b);
    //     return aRep == bRep;
    //   }

    //   @Override
    //   public Set<E> elements() {
    //     return Collections.unmodifiableSet(elmap.keySet());
    //   }

    //   @Override
    //   public ImmutableList<ImmutableSet<E>> allEquivalenceClasses() {
    //     SetMultimap<Node<E>, E> groupsTmp =
    //         MultimapBuilder.linkedHashKeys().linkedHashSetValues().build();
    //     for (Node<E> elem : elmap.values()) {
    //       groupsTmp.put(findRoot(elem), elem.element);
    //     }
    //     ImmutableList.Builder<ImmutableSet<E>> result = ImmutableList.builder();
    //     for (Set<E> group : asMap(groupsTmp).values()) {
    //       result.add(ImmutableSet.copyOf(group));
    //     }
    //     return result.build();
    //   }

    /**
     * Return the reprsentative elements of all the equivalence classes.
     *
     * <p>This is a "snapshot" view of the representatives at the time the method was called.
     */
    pub fn allRepresentatives(&self) -> Vec<E> {
        // TODO: bad collect
        self.elmap
            .values()
            .filter(|&&n| n == self.nodes[n].parent)
            .map(|&n| self.nodes[n].element)
            .collect::<Vec<_>>()
    }

    /**
     * If e is already in a non-trivial equivalence class, that is, a class with
     * more than two elements, then return the {@link Node} corresponding to the
     * representative element. Otherwise, if e sits in an equivalence class by
     * itself, then create a {@link Node}, put it into elmap and return it.
     */
    fn findRootOrCreateNode(&mut self, e: E) -> NodeId {
        match self.elmap.get(&e) {
            Some(node) => self.findRoot(*node),
            None => {
                let node = self.new_node(e);
                self.elmap.insert(e, node);
                node
            }
        }
    }

    /**
     * Given a {@link Node}, walk the parent field as far as possible, until
     * reaching the root, which is the {@link Node} for the current
     * representative of this equivalence class. To achieve low runtime
     * complexity, also compress the path, by making each node a direct child of
     * the root.
     */
    fn findRoot(&mut self, node: NodeId) -> NodeId {
        if self.nodes[node].parent != node {
            self.nodes[node].parent = self.findRoot(self.nodes[node].parent);
        }
        self.nodes[node].parent
    }

    //   @Override
    //   public Set<E> findAll(@Nullable final E value) {
    //     checkArgument(elmap.containsKey(value), "Element does not exist: %s", value);

    //     final Predicate<Object> isSameRoot = new Predicate<Object>() {

    //       /** some node that's close to the root, or null */
    //       Node<E> nodeForValue = elmap.get(value);

    //       @Override
    //       public boolean apply(@Nullable Object b) {
    //         if (Objects.equal(value, b)) {
    //           return true;
    //         }
    //         Node<E> nodeForB = elmap.get(b);
    //         if (nodeForB == null) {
    //           return false;
    //         }
    //         nodeForValue = findRoot(nodeForValue);
    //         return findRoot(nodeForB) == nodeForValue;
    //       }
    //     };

    //     return new AbstractSet<E>() {

    //       @Override public boolean contains(Object o) {
    //         return isSameRoot.apply(o);
    //       }

    //       @Override public Iterator<E> iterator() {
    //         return filter(elmap.keySet().iterator(), isSameRoot);
    //       }

    //       @Override public int size() {
    //         return findRoot(elmap.get(value)).size;
    //       }
    //     };
    //   }

    fn new_node(&mut self, element: E) -> NodeId {
        let id = self.nodes.next_index();
        self.nodes.push(Node {
            parent: id,
            element,
            rank: 0,
            size: 1,
        })
    }
}

/** The internal node representation. */
#[derive(Debug)]
struct Node<E>
where
    E: Eq + Hash + Copy + Debug,
{
    /** The parent node of this element. */
    parent: NodeId,

    /** The element represented by this node. */
    element: E,

    /** A bound on the depth of the subtree rooted to this node. */
    rank: usize,

    /**
     * If this node is the root of a tree, this is the number of elements in the
     * tree. Otherwise, it's undefined.
     */
    size: usize,
}

newtype_index! {
    struct NodeId {
        DEBUG_FORMAT = "NodeId({})"
    }
}
