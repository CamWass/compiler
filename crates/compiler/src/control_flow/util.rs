use super::node::{Node, NodeKind};
use ast::*;
use atoms::JsWord;
use ecma_visit::{Visit, VisitWith};
use rustc_hash::FxHashMap;
use std::hash::Hash;

/// Gets the condition of an `ON_TRUE` / `ON_FALSE` CFG edge.
///
/// `n` - a node with an outgoing conditional CFG edge.
///
/// Returns the condition node or `None` if the condition is not obviously a node.
pub fn get_condition_expression(n: Node) -> Option<Node> {
    match n.kind {
        NodeKind::IfStmt(n) => Some(Node::from(&*n.test)),
        NodeKind::WhileStmt(n) => Some(Node::from(&*n.test)),
        NodeKind::DoWhileStmt(n) => Some(Node::from(&*n.test)),
        NodeKind::ForStmt(n) => n.test.as_ref().map(|test| Node::from(test.as_ref())),
        NodeKind::ForInStmt(..) | NodeKind::ForOfStmt(..) | NodeKind::SwitchCase(..) => None,
        _ => unreachable!("Node does not have a condition."),
    }
}

/// Determines whether the given node is a FOR, DO, or WHILE node.
pub fn is_loop_structure(n: Node) -> bool {
    matches!(
        n.kind,
        NodeKind::ForStmt(_)
            | NodeKind::ForInStmt(_)
            | NodeKind::ForOfStmt(_)
            | NodeKind::DoWhileStmt(_)
            | NodeKind::WhileStmt(_)
    )
}

/// Computes the destination node of n when we want to fallthrough into the
/// subtree of n. We don't always create a CFG edge into n itself because of
/// DOs and FORs.
pub fn compute_fall_through(n: Node) -> Node {
    match n.kind {
        NodeKind::DoWhileStmt(d) => Node::from(d.body.as_ref()),
        NodeKind::ForStmt(f) => {
            match &f.init {
                Some(init) => match init {
                    VarDeclOrExpr::Expr(expr) => Node::from(expr.as_ref()),
                    VarDeclOrExpr::VarDecl(decl) => Node::from(decl),
                },
                // If there is no init, transfer control immediately to the
                // for-loop.
                None => n,
            }
        }
        NodeKind::ForInStmt(ForInStmt { right, .. })
        | NodeKind::ForOfStmt(ForOfStmt { right, .. }) => Node::from(right.as_ref()),
        NodeKind::LabeledStmt(l) => compute_fall_through(Node::from(&*l.body)),
        _ => n,
    }
}

/// Determines whether the given node can be terminated with a BREAK node.
fn is_break_structure(n: Node, labeled: bool) -> bool {
    match n.kind {
        NodeKind::ForStmt(_)
        | NodeKind::ForInStmt(_)
        | NodeKind::ForOfStmt(_)
        | NodeKind::DoWhileStmt(_)
        | NodeKind::WhileStmt(_)
        | NodeKind::SwitchStmt(_) => true,
        // TODO: case ROOT:
        NodeKind::BlockStmt(_) | NodeKind::IfStmt(_) | NodeKind::TryStmt(_) => labeled,
        _ => false,
    }
}

#[derive(Default)]
pub struct FindPossibleExceptions {
    found: bool,
}

impl<'ast> Visit<'ast> for FindPossibleExceptions {
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
pub fn may_throw_exception(n: Node) -> bool {
    let mut v = FindPossibleExceptions::default();

    n.visit_with(&mut v);

    v.found
}

/// Checks if target is actually the break target of labeled continue. The
/// label can be null if it is an unlabeled break.
pub fn is_break_target<'ast>(
    target: Node<'ast>,
    target_ancestors: impl Iterator<Item = &'ast ParentNode<'ast>> + Clone,
    label: Option<&JsWord>,
) -> bool {
    is_break_structure(target, label.is_some()) && match_label(target_ancestors, label)
}

/// Checks if target is actually the continue target of labeled continue. The
/// label can be null if it is an unlabeled continue.
pub fn is_continue_target<'ast>(
    target: Node<'ast>,
    target_ancestors: impl Iterator<Item = &'ast ParentNode<'ast>>,
    label: Option<&JsWord>,
) -> bool {
    is_loop_structure(target) && match_label(target_ancestors, label)
}

/// Check if label is actually referencing the target control structure. If
/// label is null, it always returns true.
pub fn match_label<'ast>(
    mut target_ancestors: impl Iterator<Item = &'ast ParentNode<'ast>>,
    label: Option<&JsWord>,
) -> bool {
    let mut target = target_ancestors.next().map(|parent| parent.node);
    match label {
        Some(label) => {
            while let Some(Node {
                kind: NodeKind::LabeledStmt(target_label),
                ..
            }) = target
            {
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
pub struct ParentNode<'ast> {
    pub node: Node<'ast>,
    pub children: Vec<Node<'ast>>,
}

#[derive(Debug, Default)]
pub struct ParentStack<'ast>(pub Vec<ParentNode<'ast>>);

impl<'ast> ParentStack<'ast> {
    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn push_with_child_nodes(&mut self, parent: Node<'ast>, children: Vec<Node<'ast>>) {
        self.0.push(ParentNode {
            node: parent,
            children,
        });
    }

    pub fn push_with_child(&mut self, parent: Node<'ast>, child: &'ast Stmt) {
        self.0.push(ParentNode {
            node: parent,
            children: vec![Node::from(child)],
        });
    }

    pub fn push_with_child_node(&mut self, parent: Node<'ast>, child: Node<'ast>) {
        self.0.push(ParentNode {
            node: parent,
            children: vec![child],
        });
    }

    pub fn push_with_optional_child(&mut self, parent: Node<'ast>, child: Option<&'ast Stmt>) {
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
        self.0.as_slice().iter()
    }
}

#[derive(Debug)]
pub struct ExceptionHandler<'ast> {
    pub node: Node<'ast>,
    pub parent_stack: Vec<Node<'ast>>,
}

impl<'ast> ExceptionHandler<'ast> {
    pub fn new(ancestors: &ParentStack<'ast>, node: Node<'ast>) -> Self {
        // TODO: other function like nodes
        // We only care about try, catch, and function ancestors.
        let parent_stack = ancestors
            .into_iter()
            .filter_map(|parent| match parent.node.kind {
                NodeKind::TryStmt(_)
                | NodeKind::Function(_)
                | NodeKind::Constructor(_)
                | NodeKind::CatchClause(_) => Some(parent.node),
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

#[derive(PartialEq, Clone, Debug)]
pub struct MultiMap<K, V>(FxHashMap<K, Vec<V>>)
where
    K: Eq + Hash;

impl<K, V> MultiMap<K, V>
where
    K: Eq + Hash,
    V: PartialEq,
{
    pub fn put(&mut self, key: K, value: V) {
        let existing = self.0.get_mut(&key);

        match existing {
            Some(vec) => {
                if !vec.contains(&value) {
                    vec.push(value);
                }
            }
            None => {
                self.0.insert(key, vec![value]);
            }
        }
    }

    pub fn remove(&mut self, key: &K) {
        self.0.remove(key);
    }

    pub fn get(&self, key: K) -> Option<&Vec<V>> {
        self.0.get(&key)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &Vec<V>)> {
        self.0.iter()
    }
}

impl<K, V> std::default::Default for MultiMap<K, V>
where
    K: Eq + Hash,
{
    fn default() -> Self {
        Self(Default::default())
    }
}

// TODO: can this be replaced by Iterator::skip_while?
pub trait AdvanceWhile: Iterator {
    /// Advances the iterator while the `predicate` returns true.
    ///
    /// This method will eagerly skip elements by calling [`next`] until the
    /// predicate returns false, or [`None`] is encountered.
    ///
    /// This method will always call [`next`] at least once, even if the
    /// predicate always returns false:
    /// ```ignore
    /// let mut a = [1, 2, 3].iter();
    /// a.advance_while(|_| false);
    /// assert_eq!(a.next(), Some(&2));
    /// ```
    /// This means that it consumes the elements up to, **and including**, the
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
    /// ```ignore
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
        for i in self.by_ref() {
            if !predicate(&i) {
                break;
            }
        }

        self
    }
}

impl<T> AdvanceWhile for T where T: Iterator {}
