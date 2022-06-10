use super::color::Color;
use super::color_registry::ColorRegistry;
use super::ColorId;
use crate::ast;
use crate::node::{Bind, BoundNode};
use crate::types::*;
use crate::types_composition::*;
use crate::utils::*;
use crate::visit::{Visit, VisitWith};
use crate::Checker;
use crate::CompProgram;
use index::{
    newtype_index,
    vec::{Idx, IndexVec},
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::iter::FromIterator;
use std::rc::Rc;
use swc_atoms::{js_word, JsWord};

pub fn collect(checker: &mut Checker, program: &CompProgram) -> ColorRegistry {
    let referenced_props = find_props(program);
    let mut data = Data {
        checker,
        registry: ColorRegistry::new(),
        type_to_color_map: FxHashMap::default(),
        referenced_props,
    };
    data.add_standard_colors();

    let mut collector = ColorCollector { data: &mut data };
    program.visit_with(&mut collector, None);

    let mut class_visitor = ClassVisitor { data: &mut data };
    program.visit_with(&mut class_visitor, None);

    data.registry
}

struct Data<'c> {
    checker: &'c mut Checker,
    registry: ColorRegistry,
    type_to_color_map: FxHashMap<TypeId, ColorId>,
    referenced_props: FxHashSet<JsWord>,
}

impl Data<'_> {
    fn add_standard_colors(&mut self) {
        // debug_assert!(
        //     self.registry.colors.is_empty() && self.registry.node_to_color_map.is_empty()
        // );

        // macro_rules! populate_standard_colors {
        //     [$( ($name:ident, $color:expr)$(,)? )*] => {
        //         let mut start = self.registry.colors.next_index();
        //         $(
        //             let $name = start;
        //             start = start.plus(1);
        //         )*
        //         self.registry.colors.extend(std::array::IntoIter::new([$($color,)*]));
        //     };
        // }

        // populate_standard_colors![
        //     (unknown_color, Color::unknown()),
        //     (bigint_color, Color::bigint()),
        //     (boolean_color, Color::boolean()),
        //     (null_or_void_color, Color::null_or_void()),
        //     (number_color, Color::number()),
        //     (string_color, Color::string()),
        //     (symbol_color, Color::symbol()),
        //     (null_or_void_color, Color::null_or_void()),
        //     (top_object_color, Color::top_object()),
        // ];

        debug_assert!(self.registry.node_to_color_map.is_empty());

        self.type_to_color_map.extend(std::array::IntoIter::new([
            // Merge all the various top/bottom-like/unknown types into a single unknown type.
            (self.checker.unknownType, self.registry.unknown_color),
            (self.checker.anyType, self.registry.unknown_color),
            // Map all the primitives in the obvious way.
            (self.checker.bigintType, self.registry.bigint_color),
            (self.checker.trueType, self.registry.boolean_color),
            (self.checker.regularTrueType, self.registry.boolean_color),
            (self.checker.falseType, self.registry.boolean_color),
            (self.checker.regularFalseType, self.registry.boolean_color),
            (self.checker.nullType, self.registry.null_or_void_color),
            (self.checker.numberType, self.registry.number_color),
            (self.checker.stringType, self.registry.string_color),
            (self.checker.esSymbolType, self.registry.symbol_color),
            (self.checker.voidType, self.registry.null_or_void_color),
            // Smoosh top-like objects into a single type.
            (
                self.checker.nonPrimitiveType,
                self.registry.top_object_color,
            ),
        ]));
    }

    fn recordType(&mut self, mut ty: TypeId, todo_temp_node: Option<&BoundNode>) -> ColorId {
        ty = self.checker.getConstraintOfType(ty).unwrap_or(ty);
        ty = self.checker.getTargetType(ty);
        // ty = self.checker.getApparentType(ty);

        if let Some(cached) = self.type_to_color_map.get(&ty) {
            return *cached;
        }

        let ty_flags = self.checker.types[ty].get_flags();
        // if ty_flags.intersects(TypeFlags::Union) {
        //     return self.recordUnionType(ty);
        // }

        // if ty_flags.intersects(TypeFlags::Object) {
        //     return self.recordObjectType(ty);
        // }

        if ty_flags.intersects(TypeFlags::Intersection) {
            todo!();
            // TODO
            // Closure doesn't support intersections, so just use unknown colour.
            // return *self
            //     .type_to_color_map
            //     .get(&self.checker.unknownType)
            //     .unwrap();
        }

        if ty_flags.intersects(TypeFlags::Union) {
            let types = self.checker.types[ty]
                .unwrap_as_union_or_intersection()
                .types
                .clone();
            let types = types.iter().map(|t| self.recordType(*t, None));
            let alt_colours = FxHashSet::from_iter(types);
            // Some elements of the union may be equal as Colors
            if alt_colours.len() == 1 {
                return *alt_colours.iter().next().unwrap();
            }

            let color = Color::default();
            let color_id = self.registry.colors.push(color);
            self.type_to_color_map.insert(ty, color_id);

            for &alt_colour in &alt_colours {
                self.registry.colors[color_id].isInvalidating |=
                    self.registry.colors[alt_colour].isInvalidating
            }

            self.registry.colors[color_id].unionElements = alt_colours;

            color_id
        } else {
            let color = Color::default();
            let color_id = self.registry.colors.push(color);
            self.type_to_color_map.insert(ty, color_id);
            self.addSupertypeEdges(ty, color_id);
            color_id
        }

        // if self.checker.types[ty]
        //     .get_flags()
        //     .intersects(TypeFlags::TypeParameter)
        //     || self.checker.types[ty]
        //         .get_object_flags()
        //         .intersects(ObjectFlags::ClassOrInterface)
        // {
        // let color = Color::default();
        // let color_id = self.registry.colors.push(color);
        // self.type_to_color_map.insert(ty, color_id);
        // self.addSupertypeEdges(ty, color_id);
        // color_id
        // } else {
        //     dbg!(
        //         &self.checker.types[ty],
        //         todo_temp_node,
        //         &self.checker.types[ty]
        //             .get_symbol()
        //             .clone()
        //             .map(|s| &self.checker.symbols[s])
        //     );
        //     // dbg!(&self.checker.types[ty]);
        //     todo!();
        //     // *self
        //     //     .type_to_color_map
        //     //     .get(&self.checker.unknownType)
        //     //     .unwrap()
        // }
    }

    // fn recordUnionType(&mut self, ty: TypeId) -> ColorId {
    //     let types = self.checker.types[ty]
    //         .unwrap_as_union_or_intersection()
    //         .types
    //         .clone();
    //     let types = types.iter().map(|t| self.recordType(*t, None));
    //     let alt_colours = FxHashSet::<ColorId, ahash::RandomState>::from_iter(types);
    //     // Some elements of the union may be equal as Colors
    //     if alt_colours.len() == 1 {
    //         return *alt_colours.iter().next().unwrap();
    //     }
    //     // TODO: the following was made up on the fly, and is not based on anything in closure.

    //     let color = Color::default();
    //     let color_id = self.registry.colors.push(color);
    //     self.type_to_color_map.insert(ty, color_id);

    //     let mut isInvalidating = false;

    //     for alt_colour in alt_colours {
    //         if let Some(alt_super_types) =
    //             self.registry.disambiguationSupertypeGraph.get(&alt_colour)
    //         {
    //             let alt_super_types = alt_super_types.clone();
    //             // TODO: bad clone.
    //             // see https://docs.rs/hashbrown/latest/hashbrown/struct.HashMap.html#method.get_many_mut
    //             // for a possible solution.
    //             self.registry
    //                 .disambiguationSupertypeGraph
    //                 .entry(color_id)
    //                 .or_default()
    //                 .extend(alt_super_types);
    //         }
    //         self.registry.colors[color_id].isInvalidating |=
    //             self.registry.colors[alt_colour].isInvalidating
    //     }

    //     color_id
    // }

    // fn recordObjectType(&mut self, ty: TypeId) -> ColorId {
    //     let color = Color::default();
    //     let color_id = self.registry.colors.push(color);
    //     self.type_to_color_map.insert(ty, color_id);
    //     self.addSupertypeEdges(ty, color_id);
    //     color_id
    // }

    fn addSupertypeEdges(&mut self, subType: TypeId, subTypeColor: ColorId) {
        if let Some(base_types) = self.checker.pubGetBaseTypes(subType) {
            for &base_type in base_types.iter() {
                self.addSupertypeEdge(base_type, subTypeColor);
            }
        }

        if let Some(constraint) = self.checker.getConstraintOfType(subType) {
            if constraint != subType {
                dbg!(
                    &self.checker.types[subType],
                    &self.checker.types[constraint]
                );
                todo!();
                // self.addSupertypeEdge(constraint, subTypeColor);
            }
        }
    }

    fn addSupertypeEdge(&mut self, super_ty: TypeId, subTypeColor: ColorId) {
        let super_color = self.recordType(super_ty, None);
        self.registry
            .disambiguationSupertypeGraph
            .entry(subTypeColor)
            .or_default()
            .insert(super_color);
    }
}

pub struct ColorCollector<'c, 'd> {
    data: &'d mut Data<'c>,
}

macro_rules! generate_visitors {
    ([$([$name:ident, $N:ident]$(,)?)*]) => {
        $(
            #[inline]
            fn $name(&mut self, n: &Rc<ast::$N>, parent: Option<BoundNode>) {
                n.visit_children_with(self, parent.clone());
                let node = n.bind_to_opt_parent(parent);
                if isExpressionNode(&node) || isDeclaration(&node) || matches!(node, BoundNode::Ident(_)) || isDeclarationName(node.clone())  {
                    let ty = self.data.checker.getTypeAtLocation(node.clone());
                    let color = self.data.recordType(ty, Some(&node));
                    self.data.registry.node_to_color_map.insert(node, color);
                }
            }
        )*

    };
}

impl Visit for ColorCollector<'_, '_> {
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

// ClassVisitor needs access to instance types. Typescript has no api to directly find these,
// so we have to rely on ColorCollector incrementally accumulating them as it traverses the AST.
// After we have visited all prop accesses on the classes, we will know the the set of instance types that
// the props are accessed on.
struct ClassVisitor<'c, 'd> {
    data: &'d mut Data<'c>,
}

impl Visit for ClassVisitor<'_, '_> {
    fn visit_class(&mut self, node: &Rc<ast::Class>, parent: Option<BoundNode>) {
        let class_decl = parent.clone().unwrap();
        let class_type = self.data.checker.getTypeAtLocation(class_decl);
        let class_color = *self.data.type_to_color_map.get(&class_type).unwrap();

        // TODO: bad collect
        // let instantiations = self.data.checker.types[class_type]
        //     .unwrap_instantiations()
        //     .values()
        //     .copied()
        //     .collect::<Vec<_>>();

        // for instance_type in instantiations {
        //     let instance_color = self.data.recordType(instance_type, None);
        //     self.data.registry.colors[class_color]
        //         .instanceColors
        //         .insert(instance_color);
        // }

        let bound_class = node.bind_to_opt_parent(parent);
        // We consider implemented interfaces to be super types of classes so that properties are
        // consistently renamed across all implementations of an interface.
        for implemented in &node.implements {
            let implemented = implemented.bind(bound_class.clone());
            let interface_type = self.data.checker.getTypeAtLocation(implemented);
            self.data.addSupertypeEdge(interface_type, class_color);
        }

        let class_sym = self.data.checker.types[class_type].get_symbol().unwrap();
        let static_type = self.data.checker.getTypeOfSymbol(class_sym);
        // TODO: I think we can skip the normalisation
        // (apprent type, target, constraint etc) that recordType performs for this type.
        let static_color = self.data.recordType(static_type, None);
        self.data.registry.colors[class_color].staticType = Some(static_color);
    }
}

fn find_props(program: &CompProgram) -> FxHashSet<JsWord> {
    let mut finder = PropertyFinder::default();
    program.visit_with(&mut finder, None);
    finder.props
}

#[derive(Default)]
struct PropertyFinder {
    props: FxHashSet<JsWord>,
}

impl Visit for PropertyFinder {
    // "name" from `someObject.name` or `someObject?.name`
    fn visit_member_expr(&mut self, node: &Rc<ast::MemberExpr>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent);
        if !node.computed {
            self.props
                .insert(unwrap_as!(&node.prop, ast::Expr::Ident(i), i).sym.clone());
        }
    }

    // "name" from `obj = {name: 0}`
    fn visit_key_value_prop(&mut self, node: &Rc<ast::KeyValueProp>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent);
        if let ast::PropName::Ident(key) = &node.key {
            self.props.insert(key.sym.clone());
        }
    }

    // "name" from `obj = { name() {} }`
    fn visit_method_prop(&mut self, node: &Rc<ast::MethodProp>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent);
        if let ast::PropName::Ident(key) = &node.key {
            self.props.insert(key.sym.clone());
        }
    }

    // "name" from `class C { name() {} }`, `class C { get name() {} }`, or `class C { set name() {} }`
    fn visit_class_method(&mut self, node: &Rc<ast::ClassMethod>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent);
        if let ast::PropName::Ident(key) = &node.key {
            self.props.insert(key.sym.clone());
        }
    }

    // "name" from `class C { #name() {} }`, `class C { get #name() {} }`, or `class C { set #name() {} }`
    fn visit_private_method(&mut self, node: &Rc<ast::PrivateMethod>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent);
        self.props.insert(node.key.id.sym.clone());
    }

    // "name" from `obj = { get name() {} }`
    fn visit_getter_prop(&mut self, node: &Rc<ast::GetterProp>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent);
        if let ast::PropName::Ident(key) = &node.key {
            self.props.insert(key.sym.clone());
        }
    }

    // "name" from `obj = { set name() {} }`
    fn visit_setter_prop(&mut self, node: &Rc<ast::SetterProp>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent);
        if let ast::PropName::Ident(key) = &node.key {
            self.props.insert(key.sym.clone());
        }
    }

    // "name" from `class C { name = 0; }`
    fn visit_class_prop(&mut self, node: &Rc<ast::ClassProp>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent);
        if let ast::PropName::Ident(key) = &node.key {
            self.props.insert(key.sym.clone());
        }
    }

    // "name" from `class C { #name = 0; }`
    fn visit_private_prop(&mut self, node: &Rc<ast::PrivateProp>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent);
        self.props.insert(node.key.id.sym.clone());
    }

    fn visit_constructor(&mut self, node: &Rc<ast::Constructor>, parent: Option<BoundNode>) {
        node.visit_children_with(self, parent);
        self.props.insert(js_word!("constructor"));
    }
}
