// TODO:
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(unused_variables)]
#![deny(non_shorthand_field_patterns)]
#![allow(warnings)]

pub mod ast;
mod binder;
mod checker;
pub mod node;
pub mod types;
mod types_composition;
mod typesv2;
mod typesv3;
pub mod utils;
pub mod visit;

use crate::ast as local_ast;
use crate::node::BoundNode;
use crate::visit::{Visit, VisitWith};
pub use checker::Checker;
use std::rc::Rc;
use types::*;

#[derive(Debug)]
pub struct SourceFile {
    pub file_name: String,
    pub program: local_ast::Program,

    /// JS identifier-declarations that are intended to merge with globals
    jsGlobalAugmentations: SymbolTable,
}

impl SourceFile {
    pub fn new(file_name: String, program: local_ast::Program) -> SourceFile {
        SourceFile {
            file_name,
            program,
            jsGlobalAugmentations: Default::default(),
        }
    }
}

impl SourceFile {
    pub fn isExternalOrCommonJsModule(&self) -> bool {
        // TODO: make this like TSC's version and handle common js
        matches!(self.program, local_ast::Program::Module(_))
    }
}

pub struct Compiler {}

impl Compiler {
    pub fn compile(
        &self,
        libs: Vec<(String, ::ast::Program)>,
        programs: Vec<(String, ::ast::Program)>,
    ) {
        let mut source_files = Vec::with_capacity(libs.len() + programs.len());
        let mut program_asts = Vec::with_capacity(programs.len());

        for (file_name, ast) in libs {
            source_files.push(SourceFile {
                file_name,
                program: local_ast::convert::convert_program(ast),
                jsGlobalAugmentations: Default::default(),
            });
        }

        // for (file_name, ast) in programs {
        //     let ast = local_ast::convert::convert_program(ast);
        //     source_files.push(SourceFile {
        //         file_name,
        //         program: ast.clone(),
        //         jsGlobalAugmentations: Default::default(),
        //     });
        //     program_asts.push(ast);
        // }

        for (_file_name, ast) in programs.iter().cloned() {
            let ast = local_ast::convert::convert_program(ast);
            program_asts.push(ast);
        }

        print(
            program_asts
                .iter()
                .enumerate()
                .map(|(i, ast)| (programs[i].0.as_str(), ast)),
        );

        // let host = TypeCheckerHost {
        //     files: source_files,
        //     compiler_options: CompilerOptions {},
        // };

        // let checker = Checker::new(host);
    }
}

// fn check<'a>(checker: Checker, asts: &[local_ast::Program]) {
//     struct NodeVisitor {
//         checker: Checker,
//     }

//     let mut visitor = NodeVisitor { checker };

//     for ast in asts {
//         ast.visit_with(&mut visitor, None);
//     }

//     macro_rules! generate_visitors {
//         ([$([$name:ident, $N:ident]$(,)?)*]) => {
//             $(
//                 #[inline]
//                 fn $name(&mut self, n: &Rc<local_ast::$N>, parent: Option<BoundNode>) {
//                     self.checker.getTypeAtLocation(n.bind_to_opt_parent(parent.clone()));
//                     n.visit_children_with(self, parent);
//                 }
//             )*

//         };
//     }

//     impl Visit for NodeVisitor {
//         generate_visitors!([
//             [visit_class, Class],
//             [visit_class_prop, ClassProp],
//             [visit_private_prop, PrivateProp],
//             [visit_class_method, ClassMethod],
//             [visit_private_method, PrivateMethod],
//             [visit_constructor, Constructor],
//             [visit_decorator, Decorator],
//             [visit_fn_decl, FnDecl],
//             [visit_class_decl, ClassDecl],
//             [visit_var_decl, VarDecl],
//             [visit_var_declarator, VarDeclarator],
//             [visit_this_expr, ThisExpr],
//             [visit_array_lit, ArrayLit],
//             [visit_object_lit, ObjectLit],
//             [visit_spread_element, SpreadElement],
//             [visit_unary_expr, UnaryExpr],
//             [visit_update_expr, UpdateExpr],
//             [visit_bin_expr, BinExpr],
//             [visit_fn_expr, FnExpr],
//             [visit_class_expr, ClassExpr],
//             [visit_assign_expr, AssignExpr],
//             [visit_member_expr, MemberExpr],
//             [visit_cond_expr, CondExpr],
//             [visit_call_expr, CallExpr],
//             [visit_new_expr, NewExpr],
//             [visit_seq_expr, SeqExpr],
//             [visit_arrow_expr, ArrowExpr],
//             [visit_yield_expr, YieldExpr],
//             [visit_meta_prop_expr, MetaPropExpr],
//             [visit_await_expr, AwaitExpr],
//             [visit_tpl, Tpl],
//             [visit_tagged_tpl, TaggedTpl],
//             [visit_tpl_element, TplElement],
//             [visit_paren_expr, ParenExpr],
//             [visit_super, Super],
//             [visit_expr_or_spread, ExprOrSpread],
//             [visit_opt_chain_expr, OptChainExpr],
//             [visit_function, Function],
//             [visit_param, Param],
//             [visit_binding_ident, BindingIdent],
//             [visit_ident, Ident],
//             [visit_private_name, PrivateName],
//             [visit_jsx_member_expr, JSXMemberExpr],
//             [visit_jsx_namespaced_name, JSXNamespacedName],
//             [visit_jsx_empty_expr, JSXEmptyExpr],
//             [visit_jsx_expr_container, JSXExprContainer],
//             [visit_jsx_spread_child, JSXSpreadChild],
//             [visit_jsx_opening_element, JSXOpeningElement],
//             [visit_jsx_closing_element, JSXClosingElement],
//             [visit_jsx_attr, JSXAttr],
//             [visit_jsx_text, JSXText],
//             [visit_jsx_element, JSXElement],
//             [visit_jsx_fragment, JSXFragment],
//             [visit_jsx_opening_fragment, JSXOpeningFragment],
//             [visit_jsx_closing_fragment, JSXClosingFragment],
//             [visit_invalid, Invalid],
//             [visit_big_int, BigInt],
//             [visit_str, Str],
//             [visit_bool, Bool],
//             [visit_null, Null],
//             [visit_regex, Regex],
//             [visit_number, Number],
//             [visit_module, Module],
//             [visit_script, Script],
//             [visit_export_default_expr, ExportDefaultExpr],
//             [visit_export_decl, ExportDecl],
//             [visit_import_decl, ImportDecl],
//             [visit_export_all, ExportAll],
//             [visit_named_export, NamedExport],
//             [visit_export_default_decl, ExportDefaultDecl],
//             [visit_import_default_specifier, ImportDefaultSpecifier],
//             [visit_import_star_as_specifier, ImportStarAsSpecifier],
//             [visit_import_named_specifier, ImportNamedSpecifier],
//             [visit_export_namespace_specifier, ExportNamespaceSpecifier],
//             [visit_export_default_specifier, ExportDefaultSpecifier],
//             [visit_export_named_specifier, ExportNamedSpecifier],
//             [visit_array_pat, ArrayPat],
//             [visit_object_pat, ObjectPat],
//             [visit_assign_pat, AssignPat],
//             [visit_rest_pat, RestPat],
//             [visit_key_value_pat_prop, KeyValuePatProp],
//             [visit_assign_pat_prop, AssignPatProp],
//             [visit_key_value_prop, KeyValueProp],
//             [visit_assign_prop, AssignProp],
//             [visit_getter_prop, GetterProp],
//             [visit_setter_prop, SetterProp],
//             [visit_method_prop, MethodProp],
//             [visit_computed_prop_name, ComputedPropName],
//             [visit_block_stmt, BlockStmt],
//             [visit_expr_stmt, ExprStmt],
//             [visit_empty_stmt, EmptyStmt],
//             [visit_debugger_stmt, DebuggerStmt],
//             [visit_with_stmt, WithStmt],
//             [visit_return_stmt, ReturnStmt],
//             [visit_labeled_stmt, LabeledStmt],
//             [visit_break_stmt, BreakStmt],
//             [visit_continue_stmt, ContinueStmt],
//             [visit_if_stmt, IfStmt],
//             [visit_switch_stmt, SwitchStmt],
//             [visit_throw_stmt, ThrowStmt],
//             [visit_try_stmt, TryStmt],
//             [visit_while_stmt, WhileStmt],
//             [visit_do_while_stmt, DoWhileStmt],
//             [visit_for_stmt, ForStmt],
//             [visit_for_in_stmt, ForInStmt],
//             [visit_for_of_stmt, ForOfStmt],
//             [visit_switch_case, SwitchCase],
//             [visit_catch_clause, CatchClause],
//             [visit_ts_type_ann, TsTypeAnn],
//             [visit_ts_type_param_decl, TsTypeParamDecl],
//             [visit_ts_type_param, TsTypeParam],
//             [visit_ts_type_param_instantiation, TsTypeParamInstantiation],
//             [visit_ts_param_prop, TsParamProp],
//             [visit_ts_qualified_name, TsQualifiedName],
//             [visit_ts_call_signature_decl, TsCallSignatureDecl],
//             [visit_ts_construct_signature_decl, TsConstructSignatureDecl],
//             [visit_ts_property_signature, TsPropertySignature],
//             [visit_ts_getter_signature, TsGetterSignature],
//             [visit_ts_setter_signature, TsSetterSignature],
//             [visit_ts_method_signature, TsMethodSignature],
//             [visit_ts_index_signature, TsIndexSignature],
//             [visit_ts_keyword_type, TsKeywordType],
//             [visit_ts_this_type, TsThisType],
//             [visit_ts_fn_type, TsFnType],
//             [visit_ts_constructor_type, TsConstructorType],
//             [visit_ts_type_ref, TsTypeRef],
//             [visit_ts_type_predicate, TsTypePredicate],
//             [visit_ts_type_query, TsTypeQuery],
//             [visit_ts_import_type, TsImportType],
//             [visit_ts_type_lit, TsTypeLit],
//             [visit_ts_array_type, TsArrayType],
//             [visit_ts_tuple_type, TsTupleType],
//             [visit_ts_tuple_element, TsTupleElement],
//             [visit_ts_optional_type, TsOptionalType],
//             [visit_ts_rest_type, TsRestType],
//             [visit_ts_union_type, TsUnionType],
//             [visit_ts_intersection_type, TsIntersectionType],
//             [visit_ts_conditional_type, TsConditionalType],
//             [visit_ts_infer_type, TsInferType],
//             [visit_ts_parenthesized_type, TsParenthesizedType],
//             [visit_ts_type_operator, TsTypeOperator],
//             [visit_ts_indexed_access_type, TsIndexedAccessType],
//             [visit_ts_mapped_type, TsMappedType],
//             [visit_ts_lit_type, TsLitType],
//             [visit_ts_tpl_lit_type, TsTplLitType],
//             [visit_ts_interface_decl, TsInterfaceDecl],
//             [visit_ts_interface_body, TsInterfaceBody],
//             [visit_ts_expr_with_type_args, TsExprWithTypeArgs],
//             [visit_ts_type_alias_decl, TsTypeAliasDecl],
//             [visit_ts_enum_decl, TsEnumDecl],
//             [visit_ts_enum_member, TsEnumMember],
//             [visit_ts_module_decl, TsModuleDecl],
//             [visit_ts_module_block, TsModuleBlock],
//             [visit_ts_namespace_decl, TsNamespaceDecl],
//             [visit_ts_import_equals_decl, TsImportEqualsDecl],
//             [visit_ts_external_module_ref, TsExternalModuleRef],
//             [visit_ts_export_assignment, TsExportAssignment],
//             [visit_ts_namespace_export_decl, TsNamespaceExportDecl],
//             [visit_ts_as_expr, TsAsExpr],
//             [visit_ts_type_assertion, TsTypeAssertion],
//             [visit_ts_non_null_expr, TsNonNullExpr],
//             [visit_ts_const_assertion, TsConstAssertion],
//         ]);
//     }
// }

fn print<'a>(files: impl Iterator<Item = (&'a str, &'a local_ast::Program)>) {
    struct NodeVisitor {
        txt: String,
    }

    impl NodeVisitor {
        pub fn print<'a>(files: impl Iterator<Item = (&'a str, &'a local_ast::Program)>) -> String {
            let mut visitor = NodeVisitor { txt: String::new() };

            for (filename, ast) in files {
                visitor.txt.push_str(filename);
                visitor.txt.push_str(":\n");
                ast.visit_with(&mut visitor, None);
                visitor.txt.push('\n');
            }

            visitor.txt
        }
    }

    let result = NodeVisitor::print(files);

    std::fs::write("nodes.txt", result);

    macro_rules! generate_visitors {
        ([$([$name:ident, $N:ident]$(,)?)*]) => {
            $(
                #[inline]
                fn $name(&mut self, n: &Rc<local_ast::$N>, parent: Option<BoundNode>) {
                    self.txt.push_str(stringify!($N));
                    self.txt.push('\n');
                    n.visit_children_with(self, parent);
                }
            )*

        };
    }

    impl Visit for NodeVisitor {
        generate_visitors!([
            [visit_class, Class],
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
            [visit_ts_type_param, TsTypeParam],
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
}

// TODO: temp
// fn foo() {
//     let mut buffer = Vec::new();

//     fn bar(n: u32, cb: &mut FnMut(u32)) {
//         cb(n);
//     }

//     let mut f = |n| {
//         buffer.push(n);
//     };

//     for i in [1, 2, 3, 4] {
//         bar(i, &mut f);
//     }
// }

pub struct TypeCheckerHost {
    pub files: Vec<SourceFile>,
    pub compiler_options: CompilerOptions,
}

#[derive(Default, Clone, Copy)]
pub struct CompilerOptions {
    // /*@internal*/ all?: boolean;
    // allowJs?: boolean;
    // /*@internal*/ allowNonTsExtensions?: boolean;
    // allowSyntheticDefaultImports?: boolean;
    // allowUmdGlobalAccess?: boolean;
    // allowUnreachableCode?: boolean;
    // allowUnusedLabels?: boolean;
    // alwaysStrict?: boolean;  // Always combine with strict property
    // baseUrl?: string;
    // /** An error if set - this should only go through the -b pipeline and not actually be observed */
    // /*@internal*/
    // build?: boolean;
    // charset?: string;
    // checkJs?: boolean;
    // /* @internal */ configFilePath?: string;
    // /** configFile is set as non enumerable property so as to avoid checking of json source files */
    // /* @internal */ readonly configFile?: TsConfigSourceFile;
    // declaration?: boolean;
    // declarationMap?: boolean;
    // emitDeclarationOnly?: boolean;
    // declarationDir?: string;
    // /* @internal */ diagnostics?: boolean;
    // /* @internal */ extendedDiagnostics?: boolean;
    // disableSizeLimit?: boolean;
    // disableSourceOfProjectReferenceRedirect?: boolean;
    // disableSolutionSearching?: boolean;
    // disableReferencedProjectLoad?: boolean;
    pub downlevelIteration: bool,
    // emitBOM?: boolean;
    // emitDecoratorMetadata?: boolean;
    // exactOptionalPropertyTypes?: boolean;
    // experimentalDecorators?: boolean;
    // forceConsistentCasingInFileNames?: boolean;
    // /*@internal*/generateCpuProfile?: string;
    // /*@internal*/generateTrace?: string;
    // /*@internal*/help?: boolean;
    // importHelpers?: boolean;
    // importsNotUsedAsValues?: ImportsNotUsedAsValues;
    // /*@internal*/init?: boolean;
    // inlineSourceMap?: boolean;
    // inlineSources?: boolean;
    pub isolatedModules: bool,
    // jsx?: JsxEmit;
    pub keyofStringsOnly: bool,
    // lib?: string[];
    // /*@internal*/listEmittedFiles?: boolean;
    // /*@internal*/listFiles?: boolean;
    // /*@internal*/explainFiles?: boolean;
    // /*@internal*/listFilesOnly?: boolean;
    // locale?: string;
    // mapRoot?: string;
    // maxNodeModuleJsDepth?: number;
    // module?: ModuleKind;
    // moduleResolution?: ModuleResolutionKind;
    // newLine?: NewLineKind;
    // noEmit?: boolean;
    // /*@internal*/noEmitForJsFiles?: boolean;
    // noEmitHelpers?: boolean;
    // noEmitOnError?: boolean;
    // noErrorTruncation?: boolean;
    // noFallthroughCasesInSwitch?: boolean;
    // noImplicitAny?: boolean;  // Always combine with strict property
    // noImplicitReturns?: boolean;
    // noImplicitThis?: boolean;  // Always combine with strict property
    // noStrictGenericChecks?: boolean;
    // noUnusedLocals?: boolean;
    // noUnusedParameters?: boolean;
    // noImplicitUseStrict?: boolean;
    pub noPropertyAccessFromIndexSignature: bool,
    // assumeChangesOnlyAffectDirectDependencies?: boolean;
    // noLib?: boolean;
    // noResolve?: boolean;
    pub noUncheckedIndexedAccess: bool,
    // out?: string;
    // outDir?: string;
    // outFile?: string;
    // paths?: MapLike<string[]>;
    // /** The directory of the config file that specified 'paths'. Used to resolve relative paths when 'baseUrl' is absent. */
    // /*@internal*/ pathsBasePath?: string;
    // /*@internal*/ plugins?: PluginImport[];
    // preserveConstEnums?: boolean;
    // noImplicitOverride?: boolean;
    // preserveSymlinks?: boolean;
    // preserveValueImports?: boolean;
    // /* @internal */ preserveWatchOutput?: boolean;
    // project?: string;
    // /* @internal */ pretty?: boolean;
    // reactNamespace?: string;
    // jsxFactory?: string;
    // jsxFragmentFactory?: string;
    // jsxImportSource?: string;
    // composite?: boolean;
    // incremental?: boolean;
    // tsBuildInfoFile?: string;
    // removeComments?: boolean;
    // rootDir?: string;
    // rootDirs?: string[];
    // skipLibCheck?: boolean;
    // skipDefaultLibCheck?: boolean;
    // sourceMap?: boolean;
    // sourceRoot?: string;
    // strict?: boolean;
    // strictFunctionTypes?: boolean;  // Always combine with strict property
    // strictBindCallApply?: boolean;  // Always combine with strict property
    // strictNullChecks?: boolean;  // Always combine with strict property
    // strictPropertyInitialization?: boolean;  // Always combine with strict property
    // stripInternal?: boolean;
    // suppressExcessPropertyErrors?: boolean;
    // suppressImplicitAnyIndexErrors?: boolean;
    // /* @internal */ suppressOutputPathCheck?: boolean;
    // target?: ScriptTarget;
    // traceResolution?: boolean;
    // useUnknownInCatchVariables?: boolean;
    // resolveJsonModule?: boolean;
    // types?: string[];
    // /** Paths used to compute primary types search locations */
    // typeRoots?: string[];
    // /*@internal*/ version?: boolean;
    // /*@internal*/ watch?: boolean;
    // esModuleInterop?: boolean;
    // /* @internal */ showConfig?: boolean;
    // useDefineForClassFields?: boolean;
}

impl CompilerOptions {
    pub fn getEmitScriptTarget(&self) -> ScriptTarget {
        // todo:
        ScriptTarget::ESNext
    }

    //     pub fn getEmitModuleKind(&self,) -> bool {
    // todo!();
    //     }

    pub fn getUseDefineForClassFields(&self) -> bool {
        // todo:
        false
    }

    pub fn getAllowSyntheticDefaultImports(&self) -> bool {
        // todo:
        false
    }

    pub fn getStrictOptionValue(&self, _field: &'static str) -> bool {
        // todo:
        false
    }

    pub fn exactOptionalPropertyTypes(&self) -> bool {
        // todo:
        false
    }
}
