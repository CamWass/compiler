use super::control_flow_graph::{
    get_condition_expression, is_entering_new_cfg_node, Annotation, Branch, ControlFlowGraph, Node,
};
use ast::*;
use ecma_visit::{noop_visit_type, Visit, VisitWith, VisitAstNodeWith};
use std::collections::HashMap;
use std::hash::Hash;
use swc_atoms::JsWord;

trait AdvanceWhile: Iterator {
    /// Advances the iterator while the `predicate` returns true.
    ///
    /// This method will eagerly skip elements by calling [`next`] until the
    /// predicate returns false, or [`None`] is encountered.
    ///
    /// This method will always call [`next`] at least once, even if the
    /// predicate always returns false:
    /// ```
    /// let mut a = [1, 2, 3].iter();
    /// a.advance_while(|_| false);
    /// assert_eq!(a.next(), Some(&2));
    /// ```
    /// This means that is consumes the elements up to, **and including**, the
    /// element the predicate returns true for.
    ///
    /// Because `advance_while()` takes a reference, and many iterators iterate
    /// over references, this leads to a possibly confusing situation where the
    /// argument is a double reference. You can see this effect in the examples
    /// below, with `&&x`.
    ///
    /// [`next`]: Iterator::next
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let a = [1, 2, 3, 4, 5, 6];
    /// let mut iter = a.iter();
    ///
    /// iter.advance_while(|&&x| x != 3);
    /// assert_eq!(iter.next(), Some(&4));
    /// iter.advance_while(|_| false);
    /// // Always calls iter.next(), even if predicate always returns false.
    /// assert_eq!(iter.next(), Some(&6));
    /// ```
    #[inline]
    fn advance_while<P>(&mut self, mut predicate: P) -> &mut Self
    where
        Self: Sized,
        P: FnMut(&Self::Item) -> bool,
    {
        while let Some(i) = self.next() {
            if !predicate(&i) {
                break;
            }
        }

        self
    }
}

impl<T> AdvanceWhile for T where T: Iterator {}

/// Computes the destination node of n when we want to fallthrough into the
/// subtree of n. We don't always create a CFG edge into n itself because of
/// DOs and FORs.
fn compute_fall_through(n: Node) -> Node {
    match n {
        Node::DoWhile(d) => Node::Expr(&*d.test),
        Node::For(f) => {
            match &f.init {
                Some(init) => match init {
                    VarDeclOrExpr::Expr(expr) => Node::Expr(&*expr),
                    VarDeclOrExpr::VarDecl(ref decl) => Node::VarDecl(decl),
                },
                // If there is no init, transfer control immediately to the
                // for-loop.
                None => n,
            }
        }
        Node::ForIn(ForInStmt { right, .. }) | Node::ForOf(ForOfStmt { right, .. }) => {
            Node::Expr(right.as_ref())
        }
        Node::Labeled(l) => compute_fall_through(Node::from(&*l.body)),
        _ => n,
    }
}

/// Determines whether the given node is a FOR, DO, or WHILE node.
fn is_loop_structure(n: Node) -> bool {
    matches!(
        n,
        Node::For(_) | Node::ForIn(_) | Node::ForOf(_) | Node::DoWhile(_) | Node::While(_)
    )
}

/**
 * Determines whether the given node can be terminated with a BREAK node.
 */
fn is_break_structure(n: Node, labeled: bool) -> bool {
    match n {
        Node::For(_)
        | Node::ForIn(_)
        | Node::ForOf(_)
        | Node::DoWhile(_)
        | Node::While(_)
        | Node::Switch(_) => true,
        // TODO: case ROOT:
        Node::Block(_) | Node::If(_) | Node::Try(_) => labeled,
        _ => false,
    }
}

struct MultiMap<K, V>(HashMap<K, Vec<V>>)
where
    K: Eq + Hash;

impl<K, V> MultiMap<K, V>
where
    K: Eq + Hash,
{
    fn put(&mut self, key: K, value: V) {
        let existing = self.0.get_mut(&key);

        match existing {
            Some(vec) => {
                vec.push(value);
            }
            None => {
                self.0.insert(key, vec![value]);
            }
        }
    }

    fn get(&self, key: K) -> Option<&Vec<V>> {
        self.0.get(&key)
    }

    fn new() -> Self {
        Self(HashMap::new())
    }
}

#[derive(Default)]
struct FindPossibleExceptions {
    found: bool,
}

impl<'ast> Visit<'ast> for FindPossibleExceptions {
    noop_visit_type!();

    fn visit_script(&mut self, _node: &'ast Script) {}
    fn visit_block_stmt(&mut self, _node: &'ast BlockStmt) {}
    fn visit_try_stmt(&mut self, _node: &'ast TryStmt) {}

    fn visit_while_stmt(&mut self, node: &'ast WhileStmt) {
        node.test.visit_with(self);
    }
    fn visit_do_while_stmt(&mut self, node: &'ast DoWhileStmt) {
        node.test.visit_with(self);
    }
    fn visit_if_stmt(&mut self, node: &'ast IfStmt) {
        node.test.visit_with(self);
    }

    fn visit_for_stmt(&mut self, node: &'ast ForStmt) {
        node.test.visit_with(self);
    }

    fn visit_for_in_stmt(&mut self, node: &'ast ForInStmt) {
        node.left.visit_with(self);
    }

    fn visit_switch_stmt(&mut self, node: &'ast SwitchStmt) {
        node.discriminant.visit_with(self);
    }

    fn visit_catch_clause(&mut self, node: &'ast CatchClause) {
        node.param.visit_with(self);
    }

    fn visit_with_stmt(&mut self, node: &'ast WithStmt) {
        node.obj.visit_with(self);
    }

    fn visit_function(&mut self, _node: &'ast Function) {}

    fn visit_throw_stmt(&mut self, _node: &'ast ThrowStmt) {
        self.found = true;
    }

    fn visit_update_expr(&mut self, _node: &'ast UpdateExpr) {
        self.found = true;
    }
    fn visit_bin_expr(&mut self, node: &'ast BinExpr) {
        if matches!(node.op, BinaryOp::In | BinaryOp::InstanceOf) {
            self.found = true;
        } else {
            node.visit_children_with(self);
        }
    }
    fn visit_assign_expr(&mut self, _node: &'ast AssignExpr) {
        self.found = true;
    }
    fn visit_member_expr(&mut self, _node: &'ast MemberExpr) {
        self.found = true;
    }
    fn visit_call_expr(&mut self, _node: &'ast CallExpr) {
        self.found = true;
    }
    fn visit_new_expr(&mut self, _node: &'ast NewExpr) {
        self.found = true;
    }
    fn visit_tagged_tpl(&mut self, _node: &'ast TaggedTpl) {
        self.found = true;
    }
    fn visit_yield_expr(&mut self, _node: &'ast YieldExpr) {
        self.found = true;
    }
    fn visit_await_expr(&mut self, _node: &'ast AwaitExpr) {
        self.found = true;
    }
}

/// Determines if the subtree might throw an exception.
fn may_throw_exception<'ast>(n: AstNode<'ast>) -> bool {
    let mut v = FindPossibleExceptions::default();

    n.visit_with(&mut v);

    v.found
}

/// Checks if target is actually the break target of labeled continue. The
/// label can be null if it is an unlabeled break.
fn is_break_target<'ast>(
    target: Node<'ast>,
    target_ancestors: impl Iterator<Item = &'ast ParentNode<'ast>> + Clone,
    label: Option<&JsWord>,
) -> bool {
    is_break_structure(target, label.is_some()) && match_label(target_ancestors, label)
}

/// Checks if target is actually the continue target of labeled continue. The
/// label can be null if it is an unlabeled continue.
fn is_continue_target<'ast>(
    target: Node<'ast>,
    target_ancestors: impl Iterator<Item = &'ast ParentNode<'ast>>,
    label: Option<&JsWord>,
) -> bool {
    is_loop_structure(target) && match_label(target_ancestors, label)
}

/// Check if label is actually referencing the target control structure. If
/// label is null, it always returns true.
fn match_label<'ast>(
    mut target_ancestors: impl Iterator<Item = &'ast ParentNode<'ast>>,
    label: Option<&JsWord>,
) -> bool {
    let mut target = target_ancestors.next().map(|parent| parent.node);
    match label {
        Some(label) => {
            while let Some(Node::Labeled(target_label)) = target {
                if &target_label.label.sym == label {
                    return true;
                }
                target = target_ancestors.next().map(|parent| parent.node);
            }
            false
        }
        None => true,
    }
}

#[derive(Debug)]
struct ParentNode<'ast> {
    node: Node<'ast>,
    children: Vec<Node<'ast>>,
}

#[derive(Debug)]
struct ParentStack<'ast>(Vec<ParentNode<'ast>>);

impl<'ast> ParentStack<'ast> {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn pop(&mut self) {
        self.0.pop();
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn push_with_child_nodes(&mut self, parent: Node<'ast>, children: Vec<Node<'ast>>) {
        self.0.push(ParentNode {
            node: parent,
            children,
        });
    }

    fn push_with_child(&mut self, parent: Node<'ast>, child: &'ast Stmt) {
        self.0.push(ParentNode {
            node: parent,
            children: vec![Node::from(child)],
        });
    }

    fn push_with_child_node(&mut self, parent: Node<'ast>, child: Node<'ast>) {
        self.0.push(ParentNode {
            node: parent,
            children: vec![child],
        });
    }

    fn push_with_optional_child(&mut self, parent: Node<'ast>, child: Option<&'ast Stmt>) {
        let children = match child {
            Some(child_stmt) => {
                vec![Node::from(child_stmt)]
            }
            None => Vec::new(),
        };

        self.0.push(ParentNode {
            node: parent,
            children,
        });
    }
}

impl<'a, 'ast> IntoIterator for &'a ParentStack<'ast> {
    type Item = &'a ParentNode<'ast>;
    type IntoIter = std::slice::Iter<'a, ParentNode<'ast>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.as_slice().into_iter()
    }
}

#[derive(Debug)]
struct ExceptionHandler<'ast> {
    node: Node<'ast>,
    parent_stack: Vec<Node<'ast>>,
}

impl<'ast> ExceptionHandler<'ast> {
    fn new(ancestors: &ParentStack<'ast>, node: Node<'ast>) -> Self {
        // We only care about try, catch, and function ancestors.
        let parent_stack = ancestors
            .into_iter()
            .filter_map(|parent| match parent.node {
                Node::Try(_) | Node::Fn(_) | Node::Constructor(_) | Node::Catch(_) => {
                    Some(parent.node)
                }
                _ => None,
            })
            .collect();

        Self { node, parent_stack }
    }
}

impl<'ast> PartialEq<ExceptionHandler<'ast>> for ExceptionHandler<'ast> {
    fn eq(&self, other: &ExceptionHandler<'ast>) -> bool {
        self.node == other.node
    }
}



pub struct ControlFlowAnalysis<'ast, NA: Annotation, EA: Annotation> {
    cfg: ControlFlowGraph<'ast, NA, EA>,
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
    /*
     * This map is used to handle the follow of FINALLY. For example:
     *
     * while(x) {
     *  try {
     *    try {
     *      break;
     *    } catch (a) {
     *    } finally {
     *      foo();
     *    }
     *    fooFollow();
     *  } catch (b) {
     *  } finally {
     *    bar();
     *  }
     *  barFollow();
     * }
     * END();
     *
     * In this case finallyMap will contain a map from:
     *    first FINALLY -> bar()
     *    second FINALLY -> END()
     *
     * When we are connecting foo() and bar() to to their respective follow, we
     * must also look up this map and connect:
     *   foo() -> bar()
     *   bar() -> END
     */
    finally_map: MultiMap<Node<'ast>, Node<'ast>>,
}

impl<'ast, NA, EA> ControlFlowAnalysis<'ast, NA, EA>
where
    NA: Annotation,
    EA: Annotation,
{
    pub fn new(root: Node<'ast>, should_traverse_functions: bool) -> Self {
        debug_assert!(matches!(
            root,
            Node::Fn(..) | Node::Script(..) | Node::Module(..)
        ));

        Self {
            cfg: ControlFlowGraph::new(compute_fall_through(root)),
            root,
            should_traverse_functions,
            exception_handler: Vec::new(),
            parent_stack: ParentStack::new(),
            finally_map: MultiMap::new(),
        }
    }

    pub fn process(&mut self) {
        match self.root {
            Node::Fn(n) => n.visit_with(self),
            Node::Script(n) => n.visit_with(self),
            Node::Module(n) => n.visit_with(self),
            _ => unreachable!(),
        }
    }

    pub fn cfg(self) -> ControlFlowGraph<'ast, NA, EA> {
        self.cfg
    }

    pub fn print_simple_graph(&self) {
        self.cfg.print_simplified();
    }

    pub fn print_full_graph(&self) {
        self.cfg.print_full();
    }

    fn handle_simple_stmt(&mut self, node: Node<'ast>) {
        // Simply transfer to the next line.
        let follow_node = self.compute_follow_node(node);
        self.cfg.create_edge(node, Branch::UNCOND, follow_node);

        self.connect_to_possible_exception_handler(
            ExceptionHandler::new(&self.parent_stack, node),
            node.into(),
        );
    }

    fn handle_enhanced_for(&mut self, for_node: Node<'ast>) {
        debug_assert!(matches!(for_node, Node::ForIn(_) | Node::ForOf(_)));
        let (right, body) = match for_node {
            Node::ForIn(ForInStmt { right, body, .. })
            | Node::ForOf(ForOfStmt { right, body, .. }) => (right, body),
            _ => unreachable!(),
        };
        // We have:  for (index in collection) { body }
        // or:       for (item of collection) { body }
        // or:       for await (item of collection) { body }

        let collection_node = Node::Expr(&*right);
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
            collection_node.into(),
        );
    }

    /// Handles functions and constructors
    fn handle_function_like(&mut self, node: Node<'ast>) {
        let body = match node {
            Node::Fn(Function { body, .. }) | Node::Constructor(Constructor { body, .. }) => body,
            _ => unreachable!(),
        };
        if let Some(ref body) = body {
            self.exception_handler
                .push(ExceptionHandler::new(&self.parent_stack, node));

            let body_node = Node::Block(body);

            self.parent_stack.push_with_child_node(node, body_node);
            // Only traverse the body.
            body.visit_with(self);
            self.parent_stack.pop();

            // A block transfer control to its first child if it is not empty.
            self.cfg
                .create_edge(node, Branch::UNCOND, compute_fall_through(body_node));

            debug_assert!(self.exception_handler.last().map(|handler| handler.node) == Some(node));
            self.exception_handler.pop();
        }
    }

    // This is a standalone fn rather than a case-visitor so the parent switch
    // statement can be passed to it. This saves us having to lookup the parent
    // for each case, and simplifies finding a case's next siblings.
    fn handle_switch_cases(&mut self, switch: &'ast SwitchStmt) {
        let cases = &switch.cases;
        let mut cases_iter = cases.iter();

        // There should only be one default case.
        debug_assert_eq!(cases.iter().filter(|c| c.is_default()).count(), 1);

        let mut default_case = None;
        while let Some(current_case) = cases_iter.next() {
            // p!(self, SwitchCase);

            let case_node = Node::Case(current_case);

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
                                        Node::Switch(switch),
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

                    // If the cases condition if false, it goes to the next
                    // case, or, if there are no more cases, the default case.
                    match next {
                        Some(next) => {
                            // Found next case.
                            self.cfg
                                .create_edge(case_node, Branch::ON_FALSE, Node::Case(next));
                        }
                        None => {
                            // No more cases.
                            match default_case {
                                Some(default) => match default.cons.first() {
                                    Some(stmt) => {
                                        // Go to the first stmt of the default case.
                                        self.cfg.create_edge(
                                            case_node,
                                            Branch::ON_FALSE,
                                            Node::from(stmt),
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
                        AstNode::from(&**test),
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
        target: AstNode<'ast>,
    ) {
        if may_throw_exception(target) && !self.exception_handler.is_empty() {
            let mut last_jump = &cfg_node;
            for handler in self.exception_handler.iter().rev() {
                let handler_node = match handler.node {
                    Node::Try(t) => t,
                    Node::Fn(_) | Node::Constructor(_) => return,
                    _ => unreachable!(),
                };

                let catch = handler_node.handler.as_ref();

                let mut last_jump_in_catch_block = false;
                for ancestor in last_jump.parent_stack.iter().rev() {
                    if *ancestor == Node::Try(handler_node) {
                        break;
                    } else if Some(*ancestor) == catch.as_ref().map(|c| Node::Catch(c)) {
                        last_jump_in_catch_block = true;
                        break;
                    }
                }

                // No catch but a FINALLY, or lastJump is inside the catch block.
                if catch.is_none() || last_jump_in_catch_block {
                    let finally = Node::Block(&handler_node.finalizer.as_ref().unwrap());

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
                            Node::Catch(&catch.unwrap()),
                        );
                        return;
                    } else {
                        self.finally_map
                            .put(last_jump.node, Node::Catch(&catch.unwrap()));
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
            if matches!(parent.node, Node::Fn(_) | Node::Constructor(_)) || node == self.root {
                return Node::ImplicitReturn;
            }

            // If we are just before a IF/WHILE/DO/FOR:
            match parent.node {
                // The follow() of any of the path from IF would be what follows IF.
                Node::If(_) => {
                    // Control is transferred up the AST to the parent's follow
                    // node.
                    node = parent.node;
                    continue;
                }
                Node::Case(case) => {
                    // After the body of a CASE, the control goes to the body of the next
                    // case, without having to go to the case condition.

                    let mut cases = match parents.clone().next() {
                        Some(ParentNode {
                            node: Node::Switch(switch),
                            ..
                        }) => switch.cases.iter(),
                        _ => unreachable!("Case should always be child of a switch statement"),
                    };
                    let next_siblings = cases.advance_while(|&child| child != case);

                    while let Some(next_sibling) = next_siblings.next() {
                        if let Some(stmt) = next_sibling.cons.first() {
                            return Node::from(stmt);
                        }
                    }

                    // Control is transferred up the AST to the parent's follow
                    // node.
                    node = parent.node;
                    continue;
                }
                Node::ForIn(_) | Node::ForOf(_) => return parent.node,
                Node::For(for_stmt) => {
                    return match &for_stmt.update {
                        Some(update) => Node::Expr(&*update),
                        // If there is no update expr for the for loop, control is
                        // transferred directly back to the for loop.
                        None => parent.node,
                    };
                }
                Node::While(_) | Node::DoWhile(_) => return parent.node,
                Node::Try(try_stmt) => {
                    // If we are coming out of the TRY block...
                    if Node::Block(&try_stmt.block) == node {
                        match &try_stmt.finalizer {
                            Some(finally) => {
                                // and have FINALLY block.
                                return compute_fall_through(Node::Block(finally));
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
                    } else if try_stmt.handler.as_ref().map(|s| Node::Catch(&s)) == Some(node) {
                        match &try_stmt.finalizer {
                            Some(finally) => {
                                // and have FINALLY block.
                                return compute_fall_through(Node::Block(finally));
                            }
                            None => {
                                // Control is transferred up the AST to the parent's follow
                                // node.
                                node = parent.node;
                                continue;
                            }
                        }

                    // If we are coming out of the FINALLY block...
                    } else if try_stmt.finalizer.as_ref().map(|s| Node::Block(&s)) == Some(node) {
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
                // TODO: is Node::Constructor(_) necessary here?
                .find(|sibling| !matches!(sibling, Node::Decl(Decl::Fn(_))));

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
impl<'ast, NA, EA> Visit<'ast> for ControlFlowAnalysis<'ast, NA, EA>
where
    NA: Annotation,
    EA: Annotation,
{
    noop_visit_type!();

    fn visit_for_in_stmt(&mut self, node: &'ast ForInStmt) {
        p!(self, ForInStmt);

        let for_node = Node::ForIn(node);

        self.parent_stack.push_with_child(for_node, &*node.body);
        // Skip for-loop-head, only traverse the body.
        node.body.visit_with(self);

        self.parent_stack.pop();

        self.handle_enhanced_for(for_node);
    }

    fn visit_for_of_stmt(&mut self, node: &'ast ForOfStmt) {
        p!(self, ForOfStmt);
        let for_node = Node::ForOf(node);
        self.parent_stack.push_with_child(for_node, &*node.body);
        // Skip for-loop-head, only traverse the body.
        node.body.visit_with(self);
        self.parent_stack.pop();

        self.handle_enhanced_for(for_node);
    }

    fn visit_for_stmt(&mut self, node: &'ast ForStmt) {
        p!(self, ForStmt);
        let for_node = Node::For(node);
        self.parent_stack.push_with_child(for_node, &*node.body);
        // Skip for-loop-head, only traverse the body.
        node.body.visit_with(self);
        self.parent_stack.pop();

        // TODO: the following may be incorrect due to closure using EMPTY
        // (which is a valid CFG node for them) for the empty portions of the
        // for head, while we use Option::None, which is not a valid CFG node.

        // We have for (init; cond; update) { body }

        let init_node = node.init.as_ref().map(|init| match init {
            VarDeclOrExpr::Expr(expr) => Node::Expr(&*expr),
            VarDeclOrExpr::VarDecl(ref decl) => Node::VarDecl(decl),
        });
        let update_node = node.update.as_ref().map(|update| Node::Expr(&**update));

        // After initialization, we transfer to the FOR which is in charge of
        // checking the condition (for the first time).
        if let Some(init_node) = init_node {
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
            self.cfg.create_edge(update_node, Branch::UNCOND, for_node);
        }

        if let Some(init_node) = init_node {
            self.connect_to_possible_exception_handler(
                ExceptionHandler::new(&self.parent_stack, init_node),
                init_node.into(),
            );
        }
        if let Some(test) = &node.test {
            let test_node = Node::Expr(&**test);
            self.connect_to_possible_exception_handler(
                ExceptionHandler::new(&self.parent_stack, for_node),
                test_node.into(),
            );
        }
        if let Some(update_node) = update_node {
            self.connect_to_possible_exception_handler(
                ExceptionHandler::new(&self.parent_stack, update_node),
                update_node.into(),
            );
        }
    }

    fn visit_do_while_stmt(&mut self, node: &'ast DoWhileStmt) {
        p!(self, DoWhileStmt);
        let do_while_node = Node::DoWhile(node);

        self.parent_stack
            .push_with_child(do_while_node, &*node.body);
        // Skip test expression, only traverse the body.
        node.body.visit_with(self);
        self.parent_stack.pop();

        let cond_node = Node::Expr(&*node.test);
        // The first edge can be the initial iteration as well as the iterations
        // after.
        self.cfg.create_edge(
            do_while_node,
            Branch::ON_TRUE,
            compute_fall_through(cond_node),
        );
        // We add the on-false branch unless the loop test is the `true` keyword
        // (i.e. `do {} while (true)`), in which case the loop is infinite and
        // there is no on-false branch.
        if !is_true_literal(&*node.test) {
            // The edge that leaves the do loop if the condition fails.
            let follow_node = self.compute_follow_node(do_while_node);
            self.cfg
                .create_edge(do_while_node, Branch::ON_FALSE, follow_node);
        }

        self.connect_to_possible_exception_handler(
            ExceptionHandler::new(&self.parent_stack, do_while_node),
            (&*node.test).into(),
        );
    }

    fn visit_if_stmt(&mut self, node: &'ast IfStmt) {
        p!(self, IfStmt);
        self.parent_stack
            .push_with_child(Node::If(node), &*node.cons);
        // Skip test expression, only traverse the bodies.
        node.cons.visit_with(self);
        self.parent_stack.pop();
        self.parent_stack
            .push_with_optional_child(Node::If(node), node.alt.as_ref().map(|s| &**s));
        node.alt.visit_with(self);
        self.parent_stack.pop();

        let if_node = Node::If(node);
        let test_node = Node::Expr(&node.test);
        let then_node = Node::from(&*node.cons);

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
            test_node.into(),
        );
    }

    fn visit_while_stmt(&mut self, node: &'ast WhileStmt) {
        p!(self, WhileStmt);
        let while_node = Node::While(node);

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
            (&*node.test).into(),
        );
    }

    fn visit_with_stmt(&mut self, node: &'ast WithStmt) {
        p!(self, WithStmt);
        let with_node = Node::With(node);
        self.parent_stack.push_with_child(with_node, &*node.body);
        // Only traverse the body.
        node.body.visit_with(self);
        self.parent_stack.pop();

        // Directly goes to the body. It should not transfer to the next case.
        self.cfg
            .create_edge(with_node, Branch::UNCOND, Node::from(&*node.body));

        self.connect_to_possible_exception_handler(
            ExceptionHandler::new(&self.parent_stack, with_node),
            (&*node.obj).into(),
        );
    }

    fn visit_switch_stmt(&mut self, node: &'ast SwitchStmt) {
        p!(self, SwitchStmt);

        let switch_node = Node::Switch(node);

        self.parent_stack.push_with_child_nodes(
            switch_node,
            node.cases.iter().map(|s| Node::Case(s)).collect(),
        );
        // Skip discriminant expression, only traverse the cases.
        self.handle_switch_cases(node);
        self.parent_stack.pop();

        // Transfer to the first non-DEFAULT CASE. if there are none, transfer
        // to the DEFAULT or the EMPTY node.
        let next = node.cases.iter().find(|case| !case.is_default());
        match next {
            Some(next) => {
                // Has at least one CASE or EMPTY
                self.cfg
                    .create_edge(switch_node, Branch::UNCOND, Node::Case(next));
            }
            None => {
                // Has no CASE but possibly a DEFAULT
                if node.cases.len() > 0 {
                    let default_node = Node::Case(&node.cases[0]);
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
            AstNode::from(&*node.discriminant),
        );
    }

    fn visit_switch_case(&mut self, _node: &'ast SwitchCase) {
        unreachable!()
    }

    // fn visit_switch_case(&mut self, node: &'ast SwitchCase) {
    //     p!(self, SwitchCase);

    //     let case_node = Node::Case(node);

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
    //                     node: Node::Switch(switch),
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
    //                                 Node::Switch(switch),
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
    //                         .create_edge(case_node, Branch::ON_FALSE, Node::Case(next));
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

        let catch_node = Node::Catch(node);

        self.parent_stack
            .push_with_child_node(catch_node, Node::Block(&node.body));

        // Skip exception binding, only traverse the body.
        node.body.visit_with(self);
        self.parent_stack.pop();

        self.cfg
            .create_edge(catch_node, Branch::UNCOND, Node::Block(&node.body));
    }

    fn visit_labeled_stmt(&mut self, node: &'ast LabeledStmt) {
        p!(self, LabeledStmt);
        self.parent_stack
            .push_with_child(Node::Labeled(node), &*node.body);
        // Skip label, only traverse the body.
        node.body.visit_with(self);
        self.parent_stack.pop();
    }

    fn visit_function(&mut self, node: &'ast Function) {
        p!(self, Function);
        let fn_node = Node::Fn(node);
        if self.should_traverse_functions || fn_node == self.cfg.entry {
            self.handle_function_like(fn_node);
        }
    }

    fn visit_class(&mut self, node: &'ast Class) {
        p!(self, Class);
        if self.should_traverse_functions {
            // Only traverse class body.
            node.body.visit_with(self);
        }
    }

    fn visit_constructor(&mut self, node: &'ast Constructor) {
        p!(self, Constructor);
        if self.should_traverse_functions {
            self.handle_function_like(Node::Constructor(node));
        }
    }

    fn visit_arrow_expr(&mut self, _node: &'ast ArrowExpr) {
        p!(self, ArrowExpr);
        // TODO: special handling
    }

    fn visit_try_stmt(&mut self, node: &'ast TryStmt) {
        p!(self, TryStmt);
        let try_node = Node::Try(node);
        self.exception_handler
            .push(ExceptionHandler::new(&self.parent_stack, try_node));
        self.parent_stack
            .push_with_child_node(try_node, Node::Block(&node.block));

        node.block.visit_with(self);

        if let Some(catch) = &node.handler {
            catch.visit_with(self);
        }

        if let Some(finally) = &node.finalizer {
            finally.visit_with(self);
        }

        /* When we are done with the TRY block and there is no FINALLY block,
         * or done with both the TRY and CATCH block, then no more exceptions
         * can be handled at this TRY statement, so it can be taken out of the
         * stack.
         */
        debug_assert!(self.exception_handler.last().map(|handler| handler.node) == Some(try_node));
        self.exception_handler.pop();

        self.parent_stack.pop();

        self.cfg
            .create_edge(try_node, Branch::UNCOND, Node::Block(&node.block))
    }

    fn visit_script(&mut self, node: &'ast Script) {
        p!(self, Script);

        let script_node = Node::Script(node);
        self.parent_stack.push_with_child_nodes(
            script_node,
            node.body.iter().map(|s| Node::from(s)).collect(),
        );

        // A block transfer control to its first child if it is not empty.
        // Function declarations are skipped since control doesn't go into that
        // function (unless it is called)
        // TODO: what about other decls (like class)?
        let child = node
            .body
            .iter()
            // TODO: is Node::Constructor(_) necessary here?
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

        let module_node = Node::Module(node);
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
        // TODO: what about other decls (like class)?
        let child = node
            .body
            .iter()
            .map(|item| match item {
                ModuleItem::ModuleDecl(_) => todo!(),
                ModuleItem::Stmt(s) => Node::from(s),
            })
            // TODO: is Node::Constructor(_) necessary here?
            .find(|node| !matches!(node, Node::Decl(Decl::Fn(_))));

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
        let block_node = Node::Block(node);

        // A block transfer control to its first child if it is not empty.
        // Function declarations are skipped since control doesn't go into that
        // function (unless it is called)
        // TODO: what about other decls (like class)?
        let child = node
            .stmts
            .iter()
            // TODO: is Node::Constructor(_) necessary here?
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
                Decl::Class(c) => {
                    c.visit_with(self);
                    self.handle_simple_stmt(Node::from(stmt))
                }
                _ => self.handle_simple_stmt(Node::from(stmt)),
            },
        }
    }

    // TODO
    // case COMPUTED_PROP:
    // case EXPORT:
    // case IMPORT:

    fn visit_continue_stmt(&mut self, node: &'ast ContinueStmt) {
        let continue_node = Node::Continue(node);

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
            if let Node::Try(t) = cur {
                if let Some(finally) = &t.finalizer {
                    let finally_node = Node::Block(finally);
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

        if let Node::For(f) = cur {
            if let Some(update) = &f.update {
                // the update expression happens after the continue
                iter = Node::Expr(&*update);
            }
        }

        if last_jump == continue_node {
            self.cfg.create_edge(continue_node, Branch::UNCOND, iter);
        } else {
            self.finally_map.put(last_jump, iter);
        }
    }
    fn visit_break_stmt(&mut self, node: &'ast BreakStmt) {
        let break_node = Node::Break(node);

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
            if let Node::Try(t) = cur {
                if let Some(finally) = &t.finalizer {
                    let finally_node = Node::Block(finally);
                    if Some(finally_node) != previous {
                        if last_jump == break_node {
                            self.cfg.create_edge(
                                last_jump,
                                Branch::UNCOND,
                                compute_fall_through(finally_node),
                            );
                        } else {
                            self.finally_map
                                .put(last_jump, compute_fall_through(finally_node));
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

        if last_jump == break_node {
            let follow_node = self.compute_follow_node_with_parent(cur, break_target_parent_index);
            self.cfg.create_edge(last_jump, Branch::UNCOND, follow_node);
        } else {
            let follow_node = self.compute_follow_node_with_parent(cur, break_target_parent_index);
            self.finally_map.put(last_jump, follow_node);
        }
    }
    fn visit_expr_stmt(&mut self, node: &'ast ExprStmt) {
        self.handle_simple_stmt(Node::ExprStmt(node));
    }
    fn visit_var_decl(&mut self, _node: &'ast VarDecl) {
        unreachable!();
    }
    fn visit_return_stmt(&mut self, node: &'ast ReturnStmt) {
        let return_node = Node::Return(node);

        let mut last_jump = None;
        for cur_handler in &self.exception_handler {
            match cur_handler.node {
                Node::Try(t) => {
                    if let Some(finally) = &t.finalizer {
                        let finally_node = Node::Block(finally);
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
                Node::Fn(_) => break,
                _ => unreachable!(),
            }
        }

        if let Some(arg) = &node.arg {
            self.connect_to_possible_exception_handler(
                ExceptionHandler::new(&self.parent_stack, return_node),
                AstNode::from(&**arg),
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
        self.connect_to_possible_exception_handler(
            ExceptionHandler::new(&self.parent_stack, Node::Throw(node)),
            node.into(),
        );
    }
}
