use super::node::Node;
use ast::*;
use ecma_visit::{Visit, VisitWith};
use petgraph::graph::{DiGraph, NodeIndex};
use rustc_hash::FxHashMap;
use std::{
    fmt::{self, Debug},
    ops::{Deref, DerefMut},
};

// TODO: account for other function like types (such as methods/getters etc)

/// A graph edge annotation with an optional label.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Edge<E>(pub Option<E>)
where
    E: Debug + PartialEq + Clone + Copy;

impl<E> fmt::Debug for Edge<E>
where
    E: Debug + PartialEq + Clone + Copy,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Some(e) => f.write_fmt(format_args!("{:?}", e)),
            None => Ok(()),
        }
    }
}

pub fn ast_graph<'ast, E>(
    node: &'ast Node<'ast>,
) -> (
    FxHashMap<Node<'ast>, NodeIndex>,
    DiGraph<Node<'ast>, Edge<E>>,
)
where
    E: Debug + PartialEq + Clone + Copy,
{
    let mut v = NodeVisitor::new();

    v.create_node(*node);

    node.visit_with(&mut v);

    (v.map, v.graph)
}

struct NodeVisitor<'ast, E>
where
    E: Debug + PartialEq + Clone + Copy,
{
    graph: DiGraph<Node<'ast>, Edge<E>>,
    map: FxHashMap<Node<'ast>, NodeIndex>,
    current_parent: Option<Node<'ast>>,
}

impl<'ast, E> NodeVisitor<'ast, E>
where
    E: Debug + PartialEq + Clone + Copy,
{
    fn new() -> Self {
        Self {
            graph: DiGraph::new(),
            map: FxHashMap::with_capacity_and_hasher(2, Default::default()),
            current_parent: None,
        }
    }

    fn create_node(&mut self, value: Node<'ast>) -> NodeIndex {
        match self.map.get(&value) {
            Some(index) => *index,
            None => {
                let index = self.graph.add_node(value);

                self.map.insert(value, index);

                index
            }
        }
    }

    fn create_edge(&mut self, from: Node<'ast>, branch: Edge<E>, to: Node<'ast>) {
        let from_node = self.create_node(from);
        let to_node = self.create_node(to);
        self.connect_if_not_found(from_node, branch, to_node);
    }

    /// Checks whether two nodes in the graph are connected by the given
    /// edge type.
    fn is_connected(&self, n1: NodeIndex, edge: Edge<E>, n2: NodeIndex) -> bool {
        self.graph
            .edges_connecting(n1, n2)
            .any(|e| *e.weight() == edge)
    }

    /// Connects two nodes in the graph with an edge if such edge does not already
    /// exists between the nodes.
    fn connect_if_not_found(&mut self, n1: NodeIndex, edge: Edge<E>, n2: NodeIndex) {
        if !self.is_connected(n1, edge, n2) {
            self.graph.add_edge(n1, n2, edge);
        }
    }

    fn connect_node_to_parent(&mut self, node: Node<'ast>) {
        if let Some(parent) = self.current_parent {
            self.create_edge(parent, Edge(None), node);
        }
    }

    /// Original parent is restored when returned guard is dropped.
    fn with_parent(&mut self, parent: Node<'ast>) -> WithParent<'_, 'ast, E> {
        let orig = self.current_parent;
        self.current_parent = Some(parent);
        WithParent { orig, inner: self }
    }

    #[inline(always)]
    fn with<F, Ret>(&mut self, f: F) -> Ret
    where
        F: FnOnce(&mut Self) -> Ret,
    {
        f(self)
    }
}

struct WithParent<'w, 'ast, E>
where
    E: Debug + PartialEq + Clone + Copy,
{
    inner: &'w mut NodeVisitor<'ast, E>,
    orig: Option<Node<'ast>>,
}
impl<'ast, E: Debug + PartialEq + Clone + Copy> Deref for WithParent<'_, 'ast, E> {
    type Target = NodeVisitor<'ast, E>;

    fn deref(&self) -> &NodeVisitor<'ast, E> {
        &self.inner
    }
}
impl<'ast, E: Debug + PartialEq + Clone + Copy> DerefMut for WithParent<'_, 'ast, E> {
    fn deref_mut(&mut self) -> &mut NodeVisitor<'ast, E> {
        &mut self.inner
    }
}
impl<'w, 'ast, E> Drop for WithParent<'w, 'ast, E>
where
    E: Debug + PartialEq + Clone + Copy,
{
    fn drop(&mut self) {
        std::mem::swap(&mut self.inner.current_parent, &mut self.orig);
    }
}

macro_rules! generate_visitors {
    ([$([$name:ident, $N:tt]$(,)?)*]) => {
        $(
            #[inline]
            fn $name(&mut self, n: &'ast $N) {
                let new_parent = Node::from(n);
                self.connect_node_to_parent(new_parent);
                self.with_parent(new_parent)
                    .with(|v| n.visit_children_with(v));
            }
        )*

    };
}

impl<'ast, E> Visit<'ast> for NodeVisitor<'ast, E>
where
    E: Debug + PartialEq + Clone + Copy,
{
    generate_visitors!([
        [visit_class, Class],
        [visit_extends_clause, ExtendsClause],
        [visit_class_prop, ClassProp],
        [visit_private_prop, PrivateProp],
        [visit_class_method, ClassMethod],
        [visit_private_method, PrivateMethod],
        [visit_constructor, Constructor],
        [visit_decorator, Decorator],
        [visit_fn_decl, FnDecl],
        [visit_class_decl, ClassDecl],
        [visit_var_decl, VarDecl],
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
        [visit_arrow_expr, ArrowExpr],
        [visit_yield_expr, YieldExpr],
        [visit_meta_prop_expr, MetaPropExpr],
        [visit_await_expr, AwaitExpr],
        [visit_tpl, Tpl],
        [visit_tagged_tpl, TaggedTpl],
        [visit_tpl_element, TplElement],
        [visit_paren_expr, ParenExpr],
        [visit_super, Super],
        [visit_opt_chain_expr, OptChainExpr],
        [visit_function, Function],
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
        [visit_module, Module],
        [visit_script, Script],
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
        [visit_getter_prop, GetterProp],
        [visit_setter_prop, SetterProp],
        [visit_method_prop, MethodProp],
        [visit_computed_prop_name, ComputedPropName],
        [visit_spread_assignment, SpreadAssignment],
        [visit_block_stmt, BlockStmt],
        [visit_expr_stmt, ExprStmt],
        [visit_empty_stmt, EmptyStmt],
        [visit_debugger_stmt, DebuggerStmt],
        [visit_with_stmt, WithStmt],
        [visit_return_stmt, ReturnStmt],
        [visit_labeled_stmt, LabeledStmt],
        [visit_break_stmt, BreakStmt],
        [visit_continue_stmt, ContinueStmt],
        [visit_if_stmt, IfStmt],
        [visit_switch_stmt, SwitchStmt],
        [visit_throw_stmt, ThrowStmt],
        [visit_try_stmt, TryStmt],
        [visit_while_stmt, WhileStmt],
        [visit_do_while_stmt, DoWhileStmt],
        [visit_for_stmt, ForStmt],
        [visit_for_in_stmt, ForInStmt],
        [visit_for_of_stmt, ForOfStmt],
        [visit_switch_case, SwitchCase],
        [visit_catch_clause, CatchClause],
        [visit_ts_type_ann, TsTypeAnn],
        [visit_ts_type_param_decl, TsTypeParamDecl],
        [visit_ts_type_param_instantiation, TsTypeParamInstantiation],
        [visit_ts_param_prop, TsParamProp],
        [visit_ts_qualified_name, TsQualifiedName],
        [visit_ts_call_signature_decl, TsCallSignatureDecl],
        [visit_ts_construct_signature_decl, TsConstructSignatureDecl],
        [visit_ts_property_signature, TsPropertySignature],
        [visit_ts_getter_signature, TsGetterSignature],
        [visit_ts_setter_signature, TsSetterSignature],
        [visit_ts_method_signature, TsMethodSignature],
        [visit_ts_index_signature, TsIndexSignature],
        [visit_ts_keyword_type, TsKeywordType],
        [visit_ts_this_type, TsThisType],
        [visit_ts_ambient_param, TsAmbientParam],
        [visit_ts_fn_type, TsFnType],
        [visit_ts_constructor_type, TsConstructorType],
        [visit_ts_type_ref, TsTypeRef],
        [visit_ts_type_predicate, TsTypePredicate],
        [visit_ts_type_query, TsTypeQuery],
        [visit_ts_import_type, TsImportType],
        [visit_ts_type_lit, TsTypeLit],
        [visit_ts_array_type, TsArrayType],
        [visit_ts_tuple_type, TsTupleType],
        [visit_ts_tuple_element, TsTupleElement],
        [visit_ts_optional_type, TsOptionalType],
        [visit_ts_rest_type, TsRestType],
        [visit_ts_union_type, TsUnionType],
        [visit_ts_intersection_type, TsIntersectionType],
        [visit_ts_conditional_type, TsConditionalType],
        [visit_ts_infer_type, TsInferType],
        [visit_ts_parenthesized_type, TsParenthesizedType],
        [visit_ts_type_operator, TsTypeOperator],
        [visit_ts_indexed_access_type, TsIndexedAccessType],
        [visit_ts_mapped_type, TsMappedType],
        [visit_ts_lit_type, TsLitType],
        [visit_ts_tpl_lit_type, TsTplLitType],
        [visit_ts_interface_decl, TsInterfaceDecl],
        [visit_ts_interface_body, TsInterfaceBody],
        [visit_ts_expr_with_type_args, TsExprWithTypeArgs],
        [visit_ts_type_alias_decl, TsTypeAliasDecl],
        [visit_ts_enum_decl, TsEnumDecl],
        [visit_ts_enum_member, TsEnumMember],
        [visit_ts_module_decl, TsModuleDecl],
        [visit_ts_module_block, TsModuleBlock],
        [visit_ts_namespace_decl, TsNamespaceDecl],
        [visit_ts_import_equals_decl, TsImportEqualsDecl],
        [visit_ts_external_module_ref, TsExternalModuleRef],
        [visit_ts_export_assignment, TsExportAssignment],
        [visit_ts_namespace_export_decl, TsNamespaceExportDecl],
        [visit_ts_as_expr, TsAsExpr],
        [visit_ts_type_assertion, TsTypeAssertion],
        [visit_ts_non_null_expr, TsNonNullExpr],
        [visit_ts_const_assertion, TsConstAssertion],
    ]);
}
