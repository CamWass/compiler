use crate::find_vars::FunctionLike;

use super::node::*;
use super::util::*;
use super::ControlFlowGraph::{Annotation, Branch, ControlFlowGraph};
use ast::*;
use ecma_visit::{Visit, VisitWith};
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;
use std::collections::BinaryHeap;

pub enum ControlFlowRoot<'ast> {
    Script(&'ast Script),
    Module(&'ast Module),
    // Function-like
    Function(&'ast Function),
    Constructor(&'ast Constructor),
    ArrowExpr(&'ast ArrowExpr),
    GetterProp(&'ast GetterProp),
    SetterProp(&'ast SetterProp),
}

impl<'ast> Into<Node<'ast>> for ControlFlowRoot<'ast> {
    fn into(self) -> Node<'ast> {
        match self {
            ControlFlowRoot::Script(n) => Node::from(n),
            ControlFlowRoot::Module(n) => Node::from(n),
            ControlFlowRoot::Function(n) => Node::from(n),
            ControlFlowRoot::Constructor(n) => Node::from(n),
            ControlFlowRoot::ArrowExpr(n) => Node::from(n),
            ControlFlowRoot::GetterProp(n) => Node::from(n),
            ControlFlowRoot::SetterProp(n) => Node::from(n),
        }
    }
}

macro_rules! impl_from {
    ($t:ty, $name:ident) => {
        impl<'ast> From<$t> for ControlFlowRoot<'ast> {
            fn from(other: $t) -> Self {
                Self::$name(other)
            }
        }
    };
}

impl_from!(&'ast Script, Script);
impl_from!(&'ast Module, Module);
impl_from!(&'ast Function, Function);
impl_from!(&'ast Constructor, Constructor);
impl_from!(&'ast ArrowExpr, ArrowExpr);
impl_from!(&'ast GetterProp, GetterProp);
impl_from!(&'ast SetterProp, SetterProp);

pub struct ControlFlowAnalysisResult<N: CfgNode, NA: Annotation, EA: Annotation> {
    pub cfg: ControlFlowGraph<N, NA, EA>,
    pub nodePriorities: FxHashMap<N, usize>,
}

pub struct ControlFlowAnalysis<'ast, N: Annotation, E: Annotation> {
    pub(super) cfg: ControlFlowGraph<Node<'ast>, N, E>,
    astPosition: FxHashMap<NodeId, usize>,
    pub(super) nodePriorities: FxHashMap<Node<'ast>, usize>,
    astPositionCounter: usize,
    priorityCounter: usize,
    // We need to store where we started, in case we aren't doing a flow analysis
    // for the whole scope. This happens, for example, when running type inference
    // on only the externs.
    root: Node<'ast>,
    should_traverse_functions: bool,
    /// This stack captures the structure of nested TRY blocks. The top of the
    /// stack is the inner most TRY block. A FUNCTION node in this stack implies
    /// that the handler is determined by the caller of the function at runtime.
    exception_handler: Vec<ExceptionHandler<'ast>>,
    parent_stack: ParentStack<'ast>,
    /// This map is used to handle the follow of FINALLY. For example:
    /// ```js
    /// while(x) {
    ///  try {
    ///    try {
    ///      break;
    ///    } catch (a) {
    ///    } finally {
    ///      foo();
    ///    }
    ///    fooFollow();
    ///  } catch (b) {
    ///  } finally {
    ///    bar();
    ///  }
    ///  barFollow();
    /// }
    /// END();
    /// ```
    ///
    /// In this case `finally_map` will contain a map from:
    ///    first FINALLY -> bar()
    ///    second FINALLY -> END()
    ///
    /// When we are connecting foo() and bar() to to their respective follow, we
    /// must also look up this map and connect:
    ///   foo() -> bar()
    ///   bar() -> END
    ///
    finally_map: MultiMap<Node<'ast>, Node<'ast>>,
}

impl<'ast, N, E> ControlFlowAnalysis<'ast, N, E>
where
    N: Annotation,
    E: Annotation,
{
    pub fn analyze(
        root: ControlFlowRoot<'ast>,
        should_traverse_functions: bool,
    ) -> ControlFlowAnalysisResult<Node<'ast>, N, E> {
        let root = root.into();
        let mut cfa = Self {
            cfg: ControlFlowGraph::new(compute_fall_through(root)),
            astPosition: FxHashMap::default(),
            nodePriorities: FxHashMap::default(),
            astPositionCounter: 0,
            priorityCounter: 0,
            root,
            should_traverse_functions,
            exception_handler: Vec::new(),
            parent_stack: ParentStack::new(),
            finally_map: MultiMap::default(),
        };

        root.visit_with(&mut cfa);

        cfa.prioritize_node(cfa.cfg.implicit_return); // the implicit return is last.

        // Every node in the cfg should have been prioritized.
        for node in cfa.cfg.graph.node_weights() {
            debug_assert!(
                cfa.astPosition.contains_key(&node.node_id),
                "node should have ast position {:#?}",
                node
            );
        }
        debug_assert_eq!(cfa.astPosition.len(), cfa.astPositionCounter);

        // Now, generate the priority of nodes by doing a depth-first
        // search on the CFG.
        let entry = cfa.cfg.entry;
        cfa.prioritizeFromEntryNode(entry);

        if cfa.should_traverse_functions {
            // If we're traversing inner functions, we need to rank the
            // priority of them too.

            // prioritizeFromEntryNode doesn't modify the graph, so we don't have
            // to worry about skipping nodes.
            for i in 0..cfa.cfg.graph.node_count() {
                let candidate = cfa.cfg.graph.raw_nodes()[i].weight;
                // TODO: classes?
                if candidate.is_function_like() {
                    cfa.prioritizeFromEntryNode(candidate);
                }
            }
        }

        // At this point, all reachable nodes have been given a priority, but
        // unreachable nodes have not been given a priority. Put them last.
        // Presumably, it doesn't really matter what priority they get, since
        // this shouldn't happen in real code.
        for &candidate in cfa.cfg.graph.node_weights() {
            if let Entry::Vacant(entry) = cfa.nodePriorities.entry(candidate) {
                entry.insert(cfa.priorityCounter);
                cfa.priorityCounter += 1;
            }
        }

        // Again, the implicit return node is always last.
        cfa.nodePriorities
            .insert(cfa.cfg.implicit_return, cfa.priorityCounter);
        // TODO: unnecessary? (never read after this?)
        // cfa.priorityCounter += 1;

        // Every node in the cfg should have been prioritized.
        debug_assert_eq!(cfa.nodePriorities.len(), cfa.cfg.graph.node_count());
        debug_assert_eq!(cfa.nodePriorities.len(), cfa.priorityCounter);

        ControlFlowAnalysisResult {
            cfg: cfa.cfg,
            nodePriorities: cfa.nodePriorities,
        }
    }

    /**
     * Given an entry node, find all the nodes reachable from that node
     * and prioritize them.
     */
    fn prioritizeFromEntryNode(&mut self, entry: Node<'ast>) {
        #[derive(Debug)]
        struct PrioritizedNode<'ast>(usize, Node<'ast>);

        impl PartialEq for PrioritizedNode<'_> {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }

        impl Eq for PrioritizedNode<'_> {}

        impl std::cmp::Ord for PrioritizedNode<'_> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                // self.0.cmp(&other.0)
                // Reversed so lower priorities are visited first.
                other.0.cmp(&self.0)
            }
        }

        // `PartialOrd` needs to be implemented as well.
        impl std::cmp::PartialOrd for PrioritizedNode<'_> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        let mk = |s: &ControlFlowAnalysis<'ast, N, E>, node: Node<'ast>| {
            let position = *s.astPosition.get(&node.node_id).unwrap();
            PrioritizedNode(position, node)
        };

        let mut worklist = BinaryHeap::with_capacity(10);
        worklist.push(mk(self, entry));

        while let Some(PrioritizedNode(_, current)) = worklist.pop() {
            if self.nodePriorities.contains_key(&current) {
                continue;
            }

            self.nodePriorities.insert(current, self.priorityCounter);
            self.priorityCounter += 1;

            let successors = self
                .cfg
                .getDirectedSuccNodes(current)
                .map(|n| mk(self, self.cfg.graph[n]));
            worklist.extend(successors);
        }
    }

    fn prioritize_node(&mut self, node: Node) {
        debug_assert!(
            !self.astPosition.contains_key(&node.node_id),
            "node has already been prioritized {:#?}",
            node
        );
        self.astPosition
            .insert(node.node_id, self.astPositionCounter);
        self.astPositionCounter += 1;
    }

    pub fn cfg(self) -> ControlFlowGraph<Node<'ast>, N, E> {
        self.cfg
    }

    pub fn print_simple_graph(&self) {
        self.cfg.print_simple();
    }

    pub fn print_full_graph(&self) {
        self.cfg.print_full();
    }

    fn handle_simple_stmt(&mut self, node: Node<'ast>) {
        self.prioritize_node(node);
        // Simply transfer to the next line.
        let follow_node = self.compute_follow_node(node);
        self.cfg.create_edge(node, Branch::UNCOND, follow_node);

        self.connect_to_possible_exception_handler(
            ExceptionHandler::new(&self.parent_stack, node),
            node.into(),
        );
    }

    fn handle_enhanced_for(&mut self, for_node: Node<'ast>) {
        debug_assert!(matches!(
            for_node.kind,
            NodeKind::ForInStmt(_) | NodeKind::ForOfStmt(_)
        ));
        let (right, body) = match for_node.kind {
            NodeKind::ForInStmt(ForInStmt { right, body, .. })
            | NodeKind::ForOfStmt(ForOfStmt { right, body, .. }) => (right, body),
            _ => unreachable!(),
        };

        self.prioritize_node(for_node);

        self.parent_stack.push_with_child(for_node, body.as_ref());
        // Skip for-loop-head, only traverse the body.
        body.visit_with(self);
        self.parent_stack.pop();

        // We have:  for (index in collection) { body }
        // or:       for (item of collection) { body }
        // or:       for await (item of collection) { body }

        let collection_node = Node::from(right.as_ref());
        // We don't visit the head of the for-loop-head, so this is our only
        // chance to prioritize the collection node.
        self.prioritize_node(collection_node);
        let body_node = Node::from(&**body);
        // The collection behaves like init.
        self.cfg
            .create_edge(collection_node, Branch::UNCOND, for_node);
        // The edge that transfer control to the beginning of the loop body.
        self.cfg
            .create_edge(for_node, Branch::ON_TRUE, compute_fall_through(body_node));

        let follow_node = self.compute_follow_node(for_node);
        // The edge to end of the loop.
        self.cfg
            .create_edge(for_node, Branch::ON_FALSE, follow_node);
        self.connect_to_possible_exception_handler(
            ExceptionHandler::new(&self.parent_stack, for_node),
            collection_node,
        );
    }

    /// Handles functions/constructors/etc
    fn handle_function_like<T>(&mut self, node: &'ast T)
    where
        T: FunctionLike<'ast>,
        &'ast T: Into<Node<'ast>>,
    {
        let body = node.body();
        let node = node.into();
        self.prioritize_node(node);

        if self.should_traverse_functions || node == self.cfg.entry {
            // We don't want to descend into an expr body of an ArrowExpr e.g. `a => foo()`.
            if let Some(
                body @ Node {
                    kind: NodeKind::BlockStmt(_),
                    ..
                },
            ) = body
            {
                self.exception_handler
                    .push(ExceptionHandler::new(&self.parent_stack, node));

                self.parent_stack.push_with_child_node(node, body);
                // Only traverse the body.
                body.visit_with(self);
                self.parent_stack.pop();

                // A function transfers control to its body.
                self.cfg
                    .create_edge(node, Branch::UNCOND, compute_fall_through(body));

                debug_assert!(
                    self.exception_handler.last().map(|handler| handler.node) == Some(node)
                );
                self.exception_handler.pop();
            }
        }
    }

    // This is a standalone fn rather than a case-visitor so the parent switch
    // statement can be passed to it. This saves us having to lookup the parent
    // for each case, and simplifies finding a case's next siblings.
    fn handle_switch_cases(&mut self, switch: &'ast SwitchStmt) {
        let cases = &switch.cases;
        let mut cases_iter = cases.iter();

        // There should only be one default case.
        debug_assert!(cases.iter().filter(|c| c.is_default()).count() <= 1);

        let mut default_case = None;
        while let Some(current_case) = cases_iter.next() {
            // p!(self, SwitchCase);

            let case_node = Node::from(current_case);
            self.prioritize_node(case_node);

            self.parent_stack.push_with_child_nodes(
                case_node,
                current_case.cons.iter().map(|s| Node::from(s)).collect(),
            );
            // Skip test expression, only traverse the body.
            current_case.cons.visit_with(self);
            self.parent_stack.pop();

            match &current_case.test {
                Some(test) => {
                    // Normal case.

                    // Case is a bit tricky. First, if condition is true, it
                    // goes into...
                    match current_case.cons.first() {
                        Some(stmt) => {
                            // ...the first stmt of the body. Or...
                            self.cfg
                                .create_edge(case_node, Branch::ON_TRUE, Node::from(stmt));
                        }
                        None => {
                            // ...if the body is empty...

                            let mut follow = None;
                            let next_siblings = cases_iter.clone();

                            for next_sibling in next_siblings {
                                if let Some(stmt) = next_sibling.cons.first() {
                                    follow = Some(Node::from(stmt));
                                    break;
                                }
                            }

                            match follow {
                                Some(follow) => {
                                    // ...it falls through into the body of the
                                    // next case. Or...
                                    self.cfg.create_edge(case_node, Branch::ON_TRUE, follow)
                                }
                                None => {
                                    // ...if there are no more cases, or they
                                    // are all empty, it goes to the follow node
                                    // of the parent switch stmt.
                                    let follow_node = self.compute_follow_node_with_parent(
                                        Node::from(switch),
                                        self.parent_stack.len() - 1,
                                    );

                                    self.cfg
                                        .create_edge(case_node, Branch::ON_TRUE, follow_node);
                                }
                            }
                        }
                    }

                    // Look for the next CASE, skipping over DEFAULT.
                    let next = cases_iter.clone().find(|case| {
                        if case.is_default() {
                            default_case = Some(*case);
                            false
                        } else {
                            true
                        }
                    });

                    // If the cases condition is false, it goes to the next
                    // case, or, if there are no more cases, the default case.
                    match next {
                        Some(next) => {
                            // Found next case.
                            self.cfg
                                .create_edge(case_node, Branch::ON_FALSE, Node::from(next));
                        }
                        None => {
                            // No more cases.
                            match default_case {
                                Some(default) => match default.cons.first() {
                                    Some(_) => {
                                        // Go to default case.
                                        self.cfg.create_edge(
                                            case_node,
                                            Branch::ON_FALSE,
                                            Node::from(default),
                                        );
                                    }
                                    None => {
                                        // Default case has no stmts, go to the follow of the switch.
                                        self.create_edge_to_case_follow(
                                            case_node,
                                            Branch::ON_FALSE,
                                        );
                                    }
                                },
                                None => {
                                    // No default case, go to the follow of the switch.
                                    self.create_edge_to_case_follow(case_node, Branch::ON_FALSE);
                                }
                            }
                        }
                    }

                    self.connect_to_possible_exception_handler(
                        ExceptionHandler::new(&self.parent_stack, case_node),
                        Node::from(&**test),
                    );
                }
                None => {
                    // Default case. Control unconditionally goes to the
                    // default's first stmt, or, if the default is empty, the
                    // default's follow.

                    default_case = Some(current_case);
                    match current_case.cons.first() {
                        Some(stmt) => {
                            self.cfg
                                .create_edge(case_node, Branch::UNCOND, Node::from(stmt));
                        }
                        None => {
                            self.create_edge_to_case_follow(case_node, Branch::UNCOND);
                        }
                    }
                }
            }
        }
    }

    fn create_edge_to_case_follow(&mut self, case: Node<'ast>, branch: Branch) {
        let follow_node = self.compute_follow_node(case);
        self.cfg.create_edge(case, branch, follow_node);
    }

    /**
     * Connects cfgNode to the proper CATCH block if target subtree might throw
     * an exception. If there are FINALLY blocks reached before a CATCH, it will
     * make the corresponding entry in finallyMap.
     */
    fn connect_to_possible_exception_handler(
        &mut self,
        cfg_node: ExceptionHandler<'ast>,
        target: Node<'ast>,
    ) {
        if may_throw_exception(target) && !self.exception_handler.is_empty() {
            let mut last_jump = &cfg_node;
            for handler in self.exception_handler.iter().rev() {
                let handler_node = match handler.node.kind {
                    NodeKind::TryStmt(t) => t,
                    _ if handler.node.is_function_like() => return,
                    _ => unreachable!(),
                };

                let catch = handler_node.handler.as_ref();

                let mut last_jump_in_catch_block = false;
                for &ancestor in last_jump.parent_stack.iter().rev() {
                    if ancestor.node_id == handler_node.node_id {
                        break;
                    } else if Some(ancestor.node_id) == catch.as_ref().map(|c| c.node_id) {
                        last_jump_in_catch_block = true;
                        break;
                    }
                }

                // No catch but a FINALLY, or lastJump is inside the catch block.
                if catch.is_none() || last_jump_in_catch_block {
                    let finally = Node::from(handler_node.finalizer.as_ref().unwrap());

                    if last_jump == &cfg_node {
                        self.cfg.create_edge(cfg_node.node, Branch::ON_EX, finally);
                    } else {
                        self.finally_map.put(last_jump.node, finally);
                    }
                } else {
                    // Has a catch.
                    if last_jump == &cfg_node {
                        self.cfg.create_edge(
                            cfg_node.node,
                            Branch::ON_EX,
                            Node::from(catch.unwrap()),
                        );
                        return;
                    } else {
                        self.finally_map
                            .put(last_jump.node, Node::from(catch.unwrap()));
                    }
                }

                last_jump = handler;
            }
        }
    }

    /**
     * Computes the follow() node of a given node and its parent. There is a side
     * effect when calling this function. If this function computed an edge that
     * exists a FINALLY, it'll attempt to connect the fromNode to the outer
     * FINALLY according to the finallyMap.
     *
     * @param fromNode The original source node since {@code node} is changed
     *        during recursion.
     * @param node The node that follow() should compute.
     */
    fn compute_follow_node(&mut self, node: Node<'ast>) -> Node<'ast> {
        self.compute_follow_node_with_parent(node, self.parent_stack.len())
    }

    /// parent_index is the index of `node`'s direct parent in the parent stack.
    fn compute_follow_node_with_parent<'a>(
        &mut self,
        mut node: Node<'ast>,
        parent_index: usize,
    ) -> Node<'ast> {
        let from_node = node;
        let mut parents = self.parent_stack.0[..parent_index].into_iter().rev();
        while let Some(parent) = parents.next() {
            /*
             * This is the case where:
             *
             * 1. Parent is null implies that we are transferring control to the end of
             * the script.
             *
             * 2. Parent is a function implies that we are transferring control back to
             * the caller of the function.
             *
             * 3. If the node is a return statement, we should also transfer control
             * back to the caller of the function.
             *
             * 4. If the node is root then we have reached the end of what we have been
             * asked to traverse.
             *
             * In all cases we should transfer control to a "symbolic return" node.
             * This will make life easier for DFAs.
             */
            if parent.node.is_function_like() || node == self.root {
                return Node::ImplicitReturn;
            }

            // If we are just before a IF/WHILE/DO/FOR:
            match parent.node.kind {
                // The follow() of any of the path from IF would be what follows IF.
                NodeKind::IfStmt(_) => {
                    // Control is transferred up the AST to the parent's follow
                    // node.
                    node = parent.node;
                    continue;
                }
                NodeKind::ForInStmt(_) | NodeKind::ForOfStmt(_) => return parent.node,
                NodeKind::ForStmt(for_stmt) => {
                    return match &for_stmt.update {
                        Some(update) => Node::from(update.as_ref()),
                        // If there is no update expr for the for loop, control is
                        // transferred directly back to the for loop.
                        None => parent.node,
                    };
                }
                NodeKind::WhileStmt(_) | NodeKind::DoWhileStmt(_) => return parent.node,
                NodeKind::TryStmt(try_stmt) => {
                    // If we are coming out of the TRY block...
                    if try_stmt.block.node_id == node.node_id {
                        match &try_stmt.finalizer {
                            Some(finally) => {
                                // and have FINALLY block.
                                return compute_fall_through(Node::from(finally));
                            }
                            None => {
                                // and have no FINALLY.
                                // Control is transferred up the AST to the parent's follow
                                // node.
                                node = parent.node;
                                continue;
                            }
                        }

                    // CATCH block.
                    } else if try_stmt.handler.as_ref().map(|s| Node::from(s)) == Some(node) {
                        match &try_stmt.finalizer {
                            Some(finally) => {
                                // and have FINALLY block.
                                return compute_fall_through(Node::from(finally));
                            }
                            None => {
                                // Control is transferred up the AST to the parent's follow
                                // node.
                                node = parent.node;
                                continue;
                            }
                        }

                    // If we are coming out of the FINALLY block...
                    } else if try_stmt.finalizer.as_ref().map(|s| Node::from(s)) == Some(node) {
                        if let Some(nodes) = self.finally_map.get(parent.node) {
                            for finally_node in nodes {
                                self.cfg
                                    .create_edge(from_node, Branch::ON_EX, *finally_node);
                            }
                        }

                        // Control is transferred up the AST to the parent's follow
                        // node.
                        node = parent.node;
                        continue;
                    }
                }
                _ => {}
            }

            // Now that we are done with the special cases follow should be its
            // immediate sibling, unless its sibling is a function.

            debug_assert!(
                parent
                    .children
                    .iter()
                    .position(|child| *child == node)
                    .is_some(),
                "Needle should be present in haystack"
            );

            let next_sibling = parent
                .children
                .iter()
                .advance_while(|&&child| child != node)
                // Skip function declarations because control doesn't get passed into it.
                .find(|sibling| !matches!(sibling.kind, NodeKind::FnDecl(_)));

            match next_sibling {
                Some(next_sibling) => return compute_fall_through(*next_sibling),
                None => {
                    // If there are no more siblings, control is transferred up
                    // the AST to the parent's follow node.
                    node = parent.node;
                    continue;
                }
            }
        }

        Node::ImplicitReturn
    }

    fn handle_class_decl(&mut self, class: &'ast ClassDecl) {
        // p!(self, Class);

        // Node: the class decl node will be prioritized by handle_simple_stmt.
        self.handle_simple_stmt(Node::from(class));

        if self.should_traverse_functions {
            // Only traverse class body.
            class.class.body.visit_with(self);
        }
    }
}

macro_rules! p {
    ($cfa:expr, $name:ident) => {
        // println!("node: {:?}", stringify!($name));
        // println!("parent_stack: {:#?}", $cfa.parent_stack);
    };
}

/// Returns whether the given expr is a bool literal with the value `true`.
fn is_true_literal(expr: &Expr) -> bool {
    matches!(expr, Expr::Lit(Lit::Bool(Bool { value: true, .. })))
}

macro_rules! generate_visitors {
    ([$([$name:ident, $N:ident]$(,)?)*]) => {
        $(
            #[inline]
            fn $name(&mut self, node: &'ast $N) {
                let cfg_node = Node::from(node);
                self.prioritize_node(cfg_node);
                node.visit_children_with(self);
            }
        )*
    };
}

/*
 * We are going to stop the traversal depending on what the node's parent
 * is.
 *
 * We are only interested in adding edges between nodes that change control
 * flow. The most obvious ones are loops and IF-ELSE's. A statement
 * transfers control to its next sibling.
 *
 * In case of an expression tree, there is no control flow within the tree
 * even when there are short circuited operators and conditionals. When we
 * are doing data flow analysis, we will simply synthesize lattices up the
 * expression tree by finding the meet at each expression node.
 *
 * For example: within a Token.SWITCH, the expression in question does not
 * change the control flow and need not to be considered.
 */
impl<'ast, N, E> Visit<'ast> for ControlFlowAnalysis<'ast, N, E>
where
    N: Annotation,
    E: Annotation,
{
    fn visit_for_in_stmt(&mut self, node: &'ast ForInStmt) {
        p!(self, ForInStmt);
        self.handle_enhanced_for(Node::from(node));
    }

    fn visit_for_of_stmt(&mut self, node: &'ast ForOfStmt) {
        p!(self, ForOfStmt);
        self.handle_enhanced_for(Node::from(node));
    }

    fn visit_for_stmt(&mut self, node: &'ast ForStmt) {
        p!(self, ForStmt);
        let for_node = Node::from(node);
        self.prioritize_node(for_node);
        self.parent_stack.push_with_child(for_node, &*node.body);
        // Skip for-loop-head, only traverse the body.
        node.body.visit_with(self);
        self.parent_stack.pop();

        // TODO: the following may be incorrect due to closure using EMPTY
        // (which is a valid CFG node for them) for the empty portions of the
        // for head, while we use Option::None, which is not a valid CFG node.

        // We have for (init; cond; update) { body }

        let init_node = node.init.as_ref().map(Node::from);
        let update_node = node.update.as_ref().map(|update| Node::from(&**update));

        // After initialization, we transfer to the FOR which is in charge of
        // checking the condition (for the first time).
        if let Some(init_node) = init_node {
            self.prioritize_node(init_node);
            self.cfg.create_edge(init_node, Branch::UNCOND, for_node);
        }

        // The edge that transfer control to the beginning of the loop body.
        self.cfg.create_edge(
            for_node,
            Branch::ON_TRUE,
            compute_fall_through(Node::from(&*node.body)),
        );
        // The edge to end of the loop.
        // If the loop test is empty or the `true` keyword then the loop is
        // infinite and we don't add the on-false branch.
        if let Some(test) = &node.test {
            if !is_true_literal(&*test) {
                let follow_node = self.compute_follow_node(for_node);
                self.cfg
                    .create_edge(for_node, Branch::ON_FALSE, follow_node);
            }
        }

        // The end of the body will have a unconditional branch to our update
        // (handled by calling computeFollowNode of the last instruction of the
        // body. Our update will jump to the for node again to another condition
        // check.
        if let Some(update_node) = update_node {
            self.prioritize_node(update_node);
            self.cfg.create_edge(update_node, Branch::UNCOND, for_node);
        }

        if let Some(init_node) = init_node {
            self.connect_to_possible_exception_handler(
                ExceptionHandler::new(&self.parent_stack, init_node),
                init_node,
            );
        }
        if let Some(test) = &node.test {
            let test_node = Node::from(&**test);
            self.prioritize_node(test_node);
            self.connect_to_possible_exception_handler(
                ExceptionHandler::new(&self.parent_stack, for_node),
                Node::from(&**test),
            );
        }
        if let Some(update_node) = update_node {
            self.connect_to_possible_exception_handler(
                ExceptionHandler::new(&self.parent_stack, update_node),
                update_node,
            );
        }
    }

    fn visit_do_while_stmt(&mut self, node: &'ast DoWhileStmt) {
        p!(self, DoWhileStmt);
        let do_while_node = Node::from(node);
        self.prioritize_node(do_while_node);

        self.parent_stack
            .push_with_child(do_while_node, &*node.body);
        // Skip test expression, only traverse the body.
        node.body.visit_with(self);
        self.parent_stack.pop();

        let body = Node::from(&*node.body);
        // The first edge can be the initial iteration as well as the iterations
        // after.
        self.cfg
            .create_edge(do_while_node, Branch::ON_TRUE, compute_fall_through(body));

        let test = Node::from(node.test.as_ref());
        self.prioritize_node(test);

        // We add the on-false branch unless the loop test is the `true` keyword
        // (i.e. `do {} while (true)`), in which case the loop is infinite and
        // there is no on-false branch.
        // if !is_true_literal(&*node.test) {
        // The edge that leaves the do loop if the condition fails.
        let follow_node = self.compute_follow_node(do_while_node);
        self.cfg
            .create_edge(do_while_node, Branch::ON_FALSE, follow_node);
        // }

        self.connect_to_possible_exception_handler(
            ExceptionHandler::new(&self.parent_stack, do_while_node),
            Node::from(&*node.test),
        );
    }

    fn visit_if_stmt(&mut self, node: &'ast IfStmt) {
        p!(self, IfStmt);
        let if_node = Node::from(node);
        self.prioritize_node(if_node);
        let test_node = Node::from(node.test.as_ref());
        self.prioritize_node(test_node);
        let then_node = Node::from(&*node.cons);

        self.parent_stack.push_with_child(if_node, &*node.cons);
        // Skip test expression, only traverse the bodies.
        node.cons.visit_with(self);
        self.parent_stack.pop();
        self.parent_stack
            .push_with_optional_child(if_node, node.alt.as_ref().map(|s| &**s));
        node.alt.visit_with(self);
        self.parent_stack.pop();

        self.cfg
            .create_edge(if_node, Branch::ON_TRUE, compute_fall_through(then_node));

        match &node.alt {
            Some(alt) => {
                let else_node = Node::from(&**alt);
                self.cfg
                    .create_edge(if_node, Branch::ON_FALSE, compute_fall_through(else_node));
            }
            None => {
                // not taken branch
                let to_node = self.compute_follow_node(if_node);
                self.cfg.create_edge(if_node, Branch::ON_FALSE, to_node);
            }
        }

        self.connect_to_possible_exception_handler(
            ExceptionHandler::new(&self.parent_stack, if_node),
            Node::from(&*node.test),
        );
    }

    fn visit_while_stmt(&mut self, node: &'ast WhileStmt) {
        p!(self, WhileStmt);
        let while_node = Node::from(node);
        self.prioritize_node(while_node);

        self.parent_stack.push_with_child(while_node, &*node.body);
        // Skip test expression, only traverse the body.
        node.body.visit_with(self);
        self.parent_stack.pop();

        // Control goes to the first statement if the condition evaluates to true.
        self.cfg.create_edge(
            while_node,
            Branch::ON_TRUE,
            compute_fall_through(Node::from(&*node.body)),
        );

        let test_node = Node::from(node.test.as_ref());
        self.prioritize_node(test_node);

        // We add the on-false branch unless the loop test is the `true` keyword
        // (i.e. `while (true) {}`), in which case the loop is infinite and
        // there is no on-false branch.
        if !is_true_literal(&*node.test) {
            // Control goes to the follow() if the condition evaluates to false.
            let follow_node = self.compute_follow_node(while_node);
            self.cfg
                .create_edge(while_node, Branch::ON_FALSE, follow_node);
        }

        self.connect_to_possible_exception_handler(
            ExceptionHandler::new(&self.parent_stack, while_node),
            Node::from(&*node.test),
        );
    }

    fn visit_with_stmt(&mut self, node: &'ast WithStmt) {
        p!(self, WithStmt);
        let with_node = Node::from(node);
        self.prioritize_node(with_node);
        self.parent_stack.push_with_child(with_node, &*node.body);
        // Only traverse the body.
        node.body.visit_with(self);
        self.parent_stack.pop();

        // TODO: comment should not mention cases
        // Directly goes to the body. It should not transfer to the next case.
        self.cfg
            .create_edge(with_node, Branch::UNCOND, Node::from(&*node.body));

        self.connect_to_possible_exception_handler(
            ExceptionHandler::new(&self.parent_stack, with_node),
            Node::from(&*node.obj),
        );
    }

    fn visit_switch_stmt(&mut self, node: &'ast SwitchStmt) {
        p!(self, SwitchStmt);

        let switch_node = Node::from(node);
        self.prioritize_node(switch_node);

        self.parent_stack.push_with_child_nodes(
            switch_node,
            node.cases.iter().map(|s| Node::from(s)).collect(),
        );
        // Skip discriminant expression, only traverse the cases.
        self.handle_switch_cases(node);
        self.parent_stack.pop();

        // Transfer to the first non-DEFAULT CASE. If there are none, transfer
        // to the DEFAULT or the EMPTY node.
        let next = node.cases.iter().find(|case| !case.is_default());
        match next {
            Some(next) => {
                // Has at least one CASE or EMPTY
                self.cfg
                    .create_edge(switch_node, Branch::UNCOND, Node::from(next));
            }
            None => {
                // Has no CASE but possibly a DEFAULT
                if node.cases.len() > 0 {
                    let default_node = Node::from(&node.cases[0]);
                    self.cfg
                        .create_edge(switch_node, Branch::UNCOND, default_node);
                } else {
                    // No CASE, no DEFAULT
                    let follow_node = self.compute_follow_node(switch_node);
                    self.cfg
                        .create_edge(switch_node, Branch::UNCOND, follow_node);
                }
            }
        }
        self.connect_to_possible_exception_handler(
            ExceptionHandler::new(&self.parent_stack, switch_node),
            Node::from(&*node.discriminant),
        );
    }

    fn visit_switch_case(&mut self, _node: &'ast SwitchCase) {
        unreachable!("handled by switch stmt");
    }

    // fn visit_switch_case(&mut self, node: &'ast SwitchCase) {
    //     p!(self, SwitchCase);

    //     let case_node = Node::SwitchCase(node);

    //     self.parent_stack
    //         .push_with_child_nodes(case_node, node.cons.iter().map(|s| Node::from(s)).collect());
    //     // Skip test expression, only traverse the body.
    //     node.cons.visit_with(self);
    //     self.parent_stack.pop();

    //     match &node.test {
    //         Some(test) => {
    //             // Normal case.

    //             let switch = match self.parent_stack.0.last() {
    //                 Some(ParentNode {
    //                     node: Node::SwitchStmt(switch),
    //                     ..
    //                 }) => *switch,
    //                 _ => unreachable!("Case should always be child of a switch statement"),
    //             };

    //             let cases = &switch.cases;

    //             // todo: we can reuse some of the follow node computations below, assuming the existing logic passes tests.

    //             // Case is a bit tricky....First it goes into the body if condition is true.
    //             match node.cons.first() {
    //                 Some(stmt) => {
    //                     self.cfg
    //                         .create_edge(case_node, Branch::ON_TRUE, Node::from(stmt));
    //                 }
    //                 None => {
    //                     let mut next_siblings = cases.iter();
    //                     let next_siblings = next_siblings
    //                         // Advance to current case.
    //                         .advance_while(|case| *case != node);

    //                     let mut follow = None;
    //                     while let Some(next_sibling) = next_siblings.next() {
    //                         if let Some(stmt) = next_sibling.cons.first() {
    //                             follow = Some(Node::from(stmt));
    //                             break;
    //                         }
    //                     }

    //                     match follow {
    //                         Some(follow) => {
    //                             self.cfg.create_edge(case_node, Branch::ON_TRUE, follow);
    //                         }
    //                         None => {
    //                             let follow_node = self.compute_follow_node_f(
    //                                 Node::SwitchStmt(switch),
    //                                 self.parent_stack.len() - 1,
    //                             );

    //                             self.cfg
    //                                 .create_edge(case_node, Branch::ON_TRUE, follow_node);
    //                         }
    //                     }
    //                 }
    //             }

    //             // Look for the next CASE, skipping over DEFAULT.
    //             let next = cases
    //                 .iter()
    //                 // Advance to current case.
    //                 .advance_while(|case| *case != node)
    //                 // Find next non-default case.
    //                 .find(|case| !case.is_default());

    //             match next {
    //                 Some(next) => {
    //                     // Found a CASE
    //                     self.cfg
    //                         .create_edge(case_node, Branch::ON_FALSE, Node::SwitchCase(next));
    //                 }
    //                 None => {
    //                     // No more CASE found, go back and search for a DEFAULT.
    //                     let default = cases.iter().find(|case| case.is_default());
    //                     match default {
    //                         Some(default) => match default.cons.first() {
    //                             Some(stmt) => {
    //                                 self.cfg.create_edge(
    //                                     case_node,
    //                                     Branch::ON_FALSE,
    //                                     Node::from(stmt),
    //                                 );
    //                             }
    //                             None => {
    //                                 let follow_node = self.compute_follow_node(case_node);
    //                                 self.cfg
    //                                     .create_edge(case_node, Branch::ON_FALSE, follow_node);
    //                             }
    //                         },
    //                         None => {
    //                             // No DEFAULT found, go to the follow of the SWITCH.
    //                             let follow_node = self.compute_follow_node(case_node);
    //                             self.cfg
    //                                 .create_edge(case_node, Branch::ON_FALSE, follow_node);
    //                         }
    //                     }
    //                 }
    //             }

    //             self.connect_to_possible_exception_handler(
    //                 ExceptionHandler::new(&self.parent_stack, case_node),
    //                 AstNode::from(&**test),
    //             );
    //         }
    //         None => {
    //             // Default case.
    //             // Directly goes to the body. It should not transfer to the next case.
    //             match node.cons.first() {
    //                 Some(stmt) => {
    //                     self.cfg
    //                         .create_edge(case_node, Branch::UNCOND, Node::from(stmt));
    //                 }
    //                 None => {
    //                     let follow_node = self.compute_follow_node(case_node);
    //                     self.cfg.create_edge(case_node, Branch::UNCOND, follow_node);
    //                 }
    //             }
    //         }
    //     }
    // }

    fn visit_catch_clause(&mut self, node: &'ast CatchClause) {
        p!(self, CatchClause);

        let catch_node = Node::from(node);
        self.prioritize_node(catch_node);

        self.parent_stack
            .push_with_child_node(catch_node, Node::from(&node.body));

        // Skip exception binding, only traverse the body.
        node.body.visit_with(self);
        self.parent_stack.pop();

        self.cfg
            .create_edge(catch_node, Branch::UNCOND, Node::from(&node.body));
    }

    fn visit_labeled_stmt(&mut self, node: &'ast LabeledStmt) {
        p!(self, LabeledStmt);
        let label_node = Node::from(node);
        self.prioritize_node(label_node);
        self.parent_stack.push_with_child(label_node, &*node.body);
        // Skip label, only traverse the body.
        node.body.visit_with(self);
        self.parent_stack.pop();
    }

    fn visit_function(&mut self, node: &'ast Function) {
        p!(self, Function);
        self.handle_function_like(node);
    }
    fn visit_constructor(&mut self, node: &'ast Constructor) {
        p!(self, Constructor);
        self.handle_function_like(node);
    }
    fn visit_arrow_expr(&mut self, node: &'ast ArrowExpr) {
        p!(self, ArrowExpr);
        self.handle_function_like(node);
    }
    fn visit_getter_prop(&mut self, node: &'ast GetterProp) {
        p!(self, GetterProp);
        self.handle_function_like(node);
    }
    fn visit_setter_prop(&mut self, node: &'ast SetterProp) {
        p!(self, SetterProp);
        self.handle_function_like(node);
    }

    fn visit_class(&mut self, _: &'ast Class) {
        unreachable!("class exprs not reachable; class decls handled by visit_stmt");
    }

    fn visit_try_stmt(&mut self, node: &'ast TryStmt) {
        p!(self, TryStmt);
        let try_node = Node::from(node);
        self.prioritize_node(try_node);
        self.exception_handler
            .push(ExceptionHandler::new(&self.parent_stack, try_node));
        self.parent_stack
            .push_with_child_node(try_node, Node::from(&node.block));

        node.block.visit_with(self);

        if node.finalizer.is_none() {
            // When we are done with the TRY block and there is no FINALLY block, then no more exceptions
            // can be handled at this TRY statement, so it can be taken out of the
            // stack.
            debug_assert!(
                self.exception_handler.last().map(|handler| handler.node) == Some(try_node)
            );
            self.exception_handler.pop();
        }

        if let Some(catch) = &node.handler {
            catch.visit_with(self);
        }

        if let Some(finally) = &node.finalizer {
            // When we are done with both the TRY and CATCH block, then no more exceptions
            // can be handled at this TRY statement, so it can be taken out of the
            // stack.
            debug_assert!(
                self.exception_handler.last().map(|handler| handler.node) == Some(try_node)
            );
            self.exception_handler.pop();

            finally.visit_with(self);
        }

        self.parent_stack.pop();

        self.cfg
            .create_edge(try_node, Branch::UNCOND, Node::from(&node.block))
    }

    fn visit_script(&mut self, node: &'ast Script) {
        p!(self, Script);

        let script_node = Node::from(node);
        self.prioritize_node(script_node);
        self.parent_stack.push_with_child_nodes(
            script_node,
            node.body.iter().map(|s| Node::from(s)).collect(),
        );

        // A block transfer control to its first child if it is not empty.
        // Function declarations are skipped since control doesn't go into that
        // function (unless it is called)
        let child = node
            .body
            .iter()
            .find(|stmt| !matches!(stmt, Stmt::Decl(Decl::Fn(_))));

        match child {
            Some(child) => {
                self.cfg.create_edge(
                    script_node,
                    Branch::UNCOND,
                    compute_fall_through(Node::from(child)),
                );
            }
            None => {
                self.cfg
                    .create_edge(script_node, Branch::UNCOND, Node::ImplicitReturn);
            }
        }

        node.body.visit_with(self);

        self.parent_stack.pop();
    }

    fn visit_module(&mut self, node: &'ast Module) {
        p!(self, Module);

        let module_node = Node::from(node);
        self.prioritize_node(module_node);
        let children = node
            .body
            .iter()
            .map(|item| match item {
                ModuleItem::ModuleDecl(_) => todo!(),
                ModuleItem::Stmt(s) => Node::from(s),
            })
            .collect();
        self.parent_stack
            .push_with_child_nodes(module_node, children);

        // A block transfer control to its first child if it is not empty.
        // Function declarations are skipped since control doesn't go into that
        // function (unless it is called)
        let child = node
            .body
            .iter()
            .map(|item| match item {
                ModuleItem::ModuleDecl(_) => todo!(),
                ModuleItem::Stmt(s) => Node::from(s),
            })
            .find(|node| !matches!(node.kind, NodeKind::FnDecl(_)));

        match child {
            Some(child) => {
                self.cfg
                    .create_edge(module_node, Branch::UNCOND, compute_fall_through(child));
            }
            None => {
                self.cfg
                    .create_edge(module_node, Branch::UNCOND, Node::ImplicitReturn);
            }
        }

        node.body.visit_with(self);

        self.parent_stack.pop();
    }

    fn visit_block_stmt(&mut self, node: &'ast BlockStmt) {
        p!(self, BlockStmt);
        let block_node = Node::from(node);
        self.prioritize_node(block_node);

        // A block transfer control to its first child if it is not empty.
        // Function declarations are skipped since control doesn't go into that
        // function (unless it is called)
        let child = node
            .stmts
            .iter()
            .find(|stmt| !matches!(stmt, Stmt::Decl(Decl::Fn(_))));

        match child {
            Some(child) => {
                self.cfg.create_edge(
                    block_node,
                    Branch::UNCOND,
                    compute_fall_through(Node::from(child)),
                );

                self.parent_stack.push_with_child_nodes(
                    block_node,
                    node.stmts.iter().map(|s| Node::from(s)).collect(),
                );

                node.stmts.visit_with(self);
                self.parent_stack.pop();
            }
            None => {
                let follow_node = self.compute_follow_node(block_node);
                self.cfg
                    .create_edge(block_node, Branch::UNCOND, follow_node);
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::Block(s) => s.visit_with(self),
            Stmt::Empty(_) | Stmt::Debugger(_) => self.handle_simple_stmt(Node::from(stmt)),
            Stmt::Expr(s) => s.visit_with(self),
            Stmt::With(s) => s.visit_with(self),
            Stmt::Return(s) => s.visit_with(self),
            Stmt::Labeled(s) => s.visit_with(self),
            Stmt::Break(s) => s.visit_with(self),
            Stmt::Continue(s) => s.visit_with(self),
            Stmt::If(s) => s.visit_with(self),
            Stmt::Switch(s) => s.visit_with(self),
            Stmt::Throw(s) => s.visit_with(self),
            Stmt::Try(s) => s.visit_with(self),
            Stmt::While(s) => s.visit_with(self),
            Stmt::DoWhile(s) => s.visit_with(self),
            Stmt::For(s) => s.visit_with(self),
            Stmt::ForIn(s) => s.visit_with(self),
            Stmt::ForOf(s) => s.visit_with(self),
            Stmt::Decl(d) => match d {
                Decl::Fn(d) => d.visit_with(self),
                Decl::Class(c) => self.handle_class_decl(c),
                _ => self.handle_simple_stmt(Node::from(stmt)),
            },
        }
    }

    // TODO
    // case COMPUTED_PROP:
    // case EXPORT:
    // case IMPORT:

    fn visit_continue_stmt(&mut self, node: &'ast ContinueStmt) {
        let continue_node = Node::from(node);
        self.prioritize_node(continue_node);

        let mut cur = continue_node;
        let mut previous = None;
        let mut last_jump = continue_node;

        let mut parents = self.parent_stack.into_iter().rev();

        // Similar to handBreak's logic with a few minor variation.
        while !is_continue_target(
            cur,
            parents.clone(),
            node.label.as_ref().map(|ident| &ident.sym),
        ) {
            if let NodeKind::TryStmt(t) = cur.kind {
                if let Some(finally) = &t.finalizer {
                    let finally_node = Node::from(finally);
                    if Some(finally_node) != previous {
                        if last_jump == continue_node {
                            self.cfg
                                .create_edge(last_jump, Branch::UNCOND, finally_node);
                        } else {
                            self.finally_map
                                .put(last_jump, compute_fall_through(finally_node));
                        }
                        last_jump = cur;
                    }
                }
            }

            previous = Some(cur);

            let parent = match parents.next() {
                Some(parent) => parent,
                None => unreachable!("Cannot find continue target."),
            };

            cur = parent.node;
        }

        let mut iter = cur;

        if let NodeKind::ForStmt(f) = cur.kind {
            if let Some(update) = &f.update {
                // the update expression happens after the continue
                iter = Node::from(update.as_ref());
            }
        }

        if last_jump == continue_node {
            self.cfg.create_edge(continue_node, Branch::UNCOND, iter);
        } else {
            self.finally_map.put(last_jump, iter);
        }
    }
    fn visit_break_stmt(&mut self, node: &'ast BreakStmt) {
        let break_node = Node::from(node);
        self.prioritize_node(break_node);

        let mut cur = break_node;
        let mut previous = None;
        let mut last_jump = break_node;

        let mut parents = self.parent_stack.into_iter().rev();
        // The index of the break target's parent in the parent stack.
        let mut break_target_parent_index = self.parent_stack.len();

        /*
         * Continuously look up the ancestor tree for the BREAK target or the target
         * with the corresponding label and connect to it. If along the path we
         * discover a FINALLY, we will connect the BREAK to that FINALLY. From then
         * on, we will just record the control flow changes in the finallyMap. This
         * is due to the fact that we need to connect any node that leaves its own
         * FINALLY block to the outer FINALLY or the BREAK's target but those nodes
         * are not known yet due to the way we traverse the nodes.
         */

        while !is_break_target(
            cur,
            parents.clone(),
            node.label.as_ref().map(|ident| &ident.sym),
        ) {
            if let NodeKind::TryStmt(t) = cur.kind {
                if let Some(finally) = &t.finalizer {
                    let finally_node = Node::from(finally);
                    if Some(finally_node) != previous {
                        let to = compute_fall_through(finally_node);
                        if last_jump == break_node {
                            self.cfg.create_edge(last_jump, Branch::UNCOND, to);
                        } else {
                            self.finally_map.put(last_jump, to);
                        }
                        last_jump = cur;
                    }
                }
            }

            let parent = match parents.next() {
                Some(parent) => parent,
                None => unreachable!("Cannot find break target."),
            };

            break_target_parent_index = break_target_parent_index - 1;

            previous = Some(cur);

            cur = parent.node;
        }

        let follow_node = self.compute_follow_node_with_parent(cur, break_target_parent_index);
        if last_jump == break_node {
            self.cfg.create_edge(last_jump, Branch::UNCOND, follow_node);
        } else {
            self.finally_map.put(last_jump, follow_node);
        }
    }
    fn visit_expr_stmt(&mut self, node: &'ast ExprStmt) {
        self.handle_simple_stmt(Node::from(node));
    }
    fn visit_var_decl(&mut self, _node: &'ast VarDecl) {
        unreachable!("handled by visit_stmt");
    }
    fn visit_return_stmt(&mut self, node: &'ast ReturnStmt) {
        let return_node = Node::from(node);
        self.prioritize_node(return_node);

        let mut last_jump = None;
        for cur_handler in self.exception_handler.iter().rev() {
            match cur_handler.node.kind {
                NodeKind::TryStmt(t) => {
                    if let Some(finally) = &t.finalizer {
                        let finally_node = Node::from(finally);
                        match last_jump {
                            Some(last_jump) => {
                                self.finally_map
                                    .put(last_jump, compute_fall_through(finally_node));
                            }
                            None => {
                                self.cfg
                                    .create_edge(return_node, Branch::UNCOND, finally_node);
                            }
                        }
                        last_jump = Some(cur_handler.node);
                    }
                }
                _ if cur_handler.node.is_function_like() => break,
                _ => unreachable!(),
            }
        }

        if let Some(arg) = &node.arg {
            self.connect_to_possible_exception_handler(
                ExceptionHandler::new(&self.parent_stack, return_node),
                Node::from(&**arg),
            );
        }

        match last_jump {
            Some(last_jump) => {
                self.finally_map.put(last_jump, self.cfg.implicit_return);
            }
            None => {
                self.cfg
                    .create_edge(return_node, Branch::UNCOND, self.cfg.implicit_return);
            }
        }
    }
    fn visit_throw_stmt(&mut self, node: &'ast ThrowStmt) {
        let throw_node = Node::from(node);
        self.prioritize_node(throw_node);
        self.connect_to_possible_exception_handler(
            ExceptionHandler::new(&self.parent_stack, throw_node),
            throw_node,
        );
    }

    generate_visitors!([
        // [visit_class, Class],
        [visit_extends_clause, ExtendsClause],
        [visit_class_prop, ClassProp],
        [visit_private_prop, PrivateProp],
        [visit_class_method, ClassMethod],
        [visit_private_method, PrivateMethod],
        // [visit_constructor, Constructor],
        [visit_decorator, Decorator],
        [visit_fn_decl, FnDecl],
        [visit_class_decl, ClassDecl],
        // [visit_var_decl, VarDecl],
        [visit_var_declarator, VarDeclarator],
        [visit_this_expr, ThisExpr],
        [visit_array_lit, ArrayLit],
        [visit_object_lit, ObjectLit],
        [visit_spread_element, SpreadElement],
        [visit_unary_expr, UnaryExpr],
        [visit_update_expr, UpdateExpr],
        [visit_bin_expr, BinExpr],
        [visit_fn_expr, FnExpr],
        [visit_class_expr, ClassExpr],
        [visit_assign_expr, AssignExpr],
        [visit_member_expr, MemberExpr],
        [visit_cond_expr, CondExpr],
        [visit_call_expr, CallExpr],
        [visit_new_expr, NewExpr],
        [visit_seq_expr, SeqExpr],
        // [visit_arrow_expr, ArrowExpr],
        [visit_yield_expr, YieldExpr],
        [visit_meta_prop_expr, MetaPropExpr],
        [visit_await_expr, AwaitExpr],
        [visit_tpl, Tpl],
        [visit_tagged_tpl, TaggedTpl],
        [visit_tpl_element, TplElement],
        [visit_paren_expr, ParenExpr],
        [visit_super, Super],
        [visit_opt_chain_expr, OptChainExpr],
        // [visit_function, Function],
        [visit_param, Param],
        [visit_param_without_decorators, ParamWithoutDecorators],
        [visit_binding_ident, BindingIdent],
        [visit_ident, Ident],
        [visit_private_name, PrivateName],
        [visit_jsx_member_expr, JSXMemberExpr],
        [visit_jsx_namespaced_name, JSXNamespacedName],
        [visit_jsx_empty_expr, JSXEmptyExpr],
        [visit_jsx_expr_container, JSXExprContainer],
        [visit_jsx_spread_child, JSXSpreadChild],
        [visit_jsx_opening_element, JSXOpeningElement],
        [visit_jsx_closing_element, JSXClosingElement],
        [visit_jsx_attr, JSXAttr],
        [visit_jsx_text, JSXText],
        [visit_jsx_element, JSXElement],
        [visit_jsx_fragment, JSXFragment],
        [visit_jsx_opening_fragment, JSXOpeningFragment],
        [visit_jsx_closing_fragment, JSXClosingFragment],
        [visit_invalid, Invalid],
        [visit_big_int, BigInt],
        [visit_str, Str],
        [visit_bool, Bool],
        [visit_null, Null],
        [visit_regex, Regex],
        [visit_number, Number],
        // [visit_module, Module],
        // [visit_script, Script],
        [visit_export_default_expr, ExportDefaultExpr],
        [visit_export_decl, ExportDecl],
        [visit_import_decl, ImportDecl],
        [visit_export_all, ExportAll],
        [visit_named_export, NamedExport],
        [visit_export_default_decl, ExportDefaultDecl],
        [visit_import_default_specifier, ImportDefaultSpecifier],
        [visit_import_star_as_specifier, ImportStarAsSpecifier],
        [visit_import_named_specifier, ImportNamedSpecifier],
        [visit_export_namespace_specifier, ExportNamespaceSpecifier],
        [visit_export_default_specifier, ExportDefaultSpecifier],
        [visit_export_named_specifier, ExportNamedSpecifier],
        [visit_array_pat, ArrayPat],
        [visit_object_pat, ObjectPat],
        [visit_assign_pat, AssignPat],
        [visit_rest_pat, RestPat],
        [visit_key_value_pat_prop, KeyValuePatProp],
        [visit_assign_pat_prop, AssignPatProp],
        [visit_key_value_prop, KeyValueProp],
        [visit_assign_prop, AssignProp],
        // [visit_getter_prop, GetterProp],
        // [visit_setter_prop, SetterProp],
        [visit_method_prop, MethodProp],
        [visit_computed_prop_name, ComputedPropName],
        [visit_spread_assignment, SpreadAssignment],
        // [visit_block_stmt, BlockStmt],
        // [visit_expr_stmt, ExprStmt],
        [visit_empty_stmt, EmptyStmt],
        [visit_debugger_stmt, DebuggerStmt],
        // [visit_with_stmt, WithStmt],
        // [visit_return_stmt, ReturnStmt],
        // [visit_labeled_stmt, LabeledStmt],
        // [visit_break_stmt, BreakStmt],
        // [visit_continue_stmt, ContinueStmt],
        // [visit_if_stmt, IfStmt],
        // [visit_switch_stmt, SwitchStmt],
        // [visit_throw_stmt, ThrowStmt],
        // [visit_try_stmt, TryStmt],
        // [visit_while_stmt, WhileStmt],
        // [visit_do_while_stmt, DoWhileStmt],
        // [visit_for_stmt, ForStmt],
        // [visit_for_in_stmt, ForInStmt],
        // [visit_for_of_stmt, ForOfStmt],
        // [visit_switch_case, SwitchCase],
        // [visit_catch_clause, CatchClause],
    ]);
}
