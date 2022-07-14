#![feature(iter_order_by)]
#![feature(map_first_last)]
// TODO:
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(unused_variables)]
#![deny(non_shorthand_field_patterns)]
#![allow(warnings)]
#![deny(unused_imports)]

mod DataFlowAnalysis;
mod DefaultNameGenerator;
mod OptimizeArgumentsArray;
mod RenameLabels;
pub mod ast;
mod binder;
mod checker;
mod colors;
mod control_flow;
mod disambiguate;
mod graph;
pub mod node;
mod normalize;
mod resolver;
mod transform_ts;
pub mod types;
mod types_composition;
pub mod utils;
pub mod visit;

#[cfg(test)]
mod testing;

use crate::ast as local_ast;
use crate::node::BoundNode;
use crate::resolver::resolver;
use crate::visit::{Visit, VisitWith};
pub use checker::Checker;
use ecma_visit::VisitMutWith;
use global_common::{Globals, Mark, SyntaxContext, GLOBALS};
use serde::Deserialize;
use swc_atoms::JsWord;
use types::*;

pub type Id = (JsWord, SyntaxContext);

#[derive(Debug, Clone)]
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

// TODO: rename to Program and change the ast node 'Program' to something else.
pub struct CompProgram {
    libs: Vec<SourceFile>,
    source: SourceFile,
}

impl<V: Visit> VisitWith<V> for CompProgram {
    // TODO: ideally callers would not be able to provide a parent when visiting ast::Program/Script/Module.
    fn visit_with(&self, v: &mut V, parent: Option<BoundNode>) {
        for lib in &self.libs {
            v.visit_program(&lib.program, parent.clone());
        }
        v.visit_program(&self.source.program, parent);
    }

    fn visit_children_with(&self, v: &mut V, parent: Option<BoundNode>) {
        for lib in &self.libs {
            crate::visit::visit_program(v, &lib.program, parent.clone());
        }
        crate::visit::visit_program(v, &self.source.program, parent);
    }
}

#[derive(Debug, Clone, Copy, Default, Deserialize)]
pub struct PassConfig {
    #[serde(default)]
    optimize_arguments_array: bool,
    #[serde(default)]
    disambiguate_properties: bool,
    #[serde(default)]
    ambiguate_properties: bool,
    #[serde(default)]
    rename_labels: bool,
}

pub struct Compiler {
    globals: Globals,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            globals: Globals::new(),
        }
    }

    /// Runs `op` in current compiler's context.
    ///
    /// Note: Other methods of `Compiler` already uses this internally.
    pub fn run<R, F>(&self, op: F) -> R
    where
        F: FnOnce() -> R,
    {
        GLOBALS.set(&self.globals, op)
    }

    pub fn compile<'a>(
        &self,
        libs: Vec<(String, ::ast::Program)>,
        program: (String, ::ast::Program),
        passes: PassConfig,
        node_id_gen: &'a mut ::ast::NodeIdGen,
    ) -> ::ast::Program {
        self.run(|| {
            // TODO: maybe add an 'AST verifier' that checks basic invariants after
            // each pass (e.g. that no two nodes have the same node_id).

            let mut program_ast = program.1;

            let mut colours = {
                let mut source_files = Vec::with_capacity(libs.len() + 1);

                let mut normalize = normalize::Normalize {};
                program_ast.visit_mut_with(&mut normalize);

                let program_source_file = SourceFile {
                    file_name: program.0,
                    program: local_ast::convert::convert_program(program_ast.clone()),
                    jsGlobalAugmentations: Default::default(),
                };
                source_files.push(program_source_file.clone());

                let mut program = CompProgram {
                    libs: Vec::with_capacity(libs.len()),
                    // TODO: temp clone()
                    source: program_source_file.clone(),
                };

                for (file_name, ast) in libs.iter().cloned() {
                    let source_file = SourceFile {
                        file_name,
                        program: local_ast::convert::convert_program(ast),
                        jsGlobalAugmentations: Default::default(),
                    };
                    source_files.push(source_file.clone());
                    program.libs.push(source_file);
                }

                let host = TypeCheckerHost {
                    files: source_files,
                    compiler_options: CompilerOptions::default(),
                };

                let mut checker = Checker::new(host, false);

                // TODO:
                let p = CompProgram {
                    libs: Vec::new(),
                    source: program.source.clone(),
                };

                colors::color_collector::collect(&mut checker, &p)
            };

            let unresolved_mark = Mark::new();
            let top_level_mark = Mark::new();

            program_ast.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark, true));

            // Note: The Rc based AST is only to be used by the checker and passes that directly access
            // the checker (color collector). All other passes should use the reference based AST.

            transform_ts::transform_param_props(&mut program_ast, node_id_gen, &mut colours);

            let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

            if passes.optimize_arguments_array {
                OptimizeArgumentsArray::OptimizeArgumentsArray::process(
                    &mut program_ast,
                    node_id_gen,
                    unresolved_ctxt,
                );
            }

            // TODO: inlineAndCollapseProperties

            // TODO: inferConsts

            // TODO: earlyInlineVariables
            // TODO: earlyPeepholeOptimizations

            // TODO: removeUnusedCodeOnce

            if passes.disambiguate_properties {
                disambiguate::DisambiguateProperties::DisambiguateProperties::process(
                    &mut program_ast,
                    &mut colours,
                );
            }

            // TODO: markPureFunctions

            getEarlyOptimizationLoopPasses(&mut program_ast);

            // TODO: crossModuleCodeMotion
            // TODO: devirtualizeMethods
            // TODO: flowSensitiveInlineVariables
            getMainOptimizationLoop(&mut program_ast);

            // Finalizations:

            // TODO: flowSensitiveInlineVariables
            // TODO: removeUnusedCodeOnce
            // TODO: crossModuleCodeMotion
            // TODO: crossModuleMethodMotion
            // TODO: optimizeConstructors
            // TODO: collapseAnonymousFunctions

            if passes.ambiguate_properties {
                disambiguate::AmbiguateProperties::AmbiguateProperties::process(
                    &mut program_ast,
                    &mut colours,
                );
            }

            // TODO: renameProperties
            // TODO: convertToDottedProperties
            // TODO: rewriteFunctionExpressions
            // TODO: aliasStrings
            // TODO: coalesceVariableNames
            // TODO: peepholeOptimizationsOnce
            // TODO: exploitAssign
            // TODO: collapseVariableDeclarations
            // TODO: denormalize
            // TODO: renameVars

            if passes.rename_labels {
                RenameLabels::process(&mut program_ast);
            }

            // TODO: latePeepholeOptimizations
            // TODO: optimizeToEs6

            program_ast
        })
    }
}

fn getEarlyOptimizationLoopPasses(ast: &mut ::ast::Program) {
    // TODO: inlineVariables
    // TODO: collapseObjectLiterals
    // TODO: removeUnusedCode
    // TODO: peepholeOptimizations
    // TODO: removeUnreachableCode
}

fn getMainOptimizationLoop(ast: &mut ::ast::Program) {
    // TODO: inlineSimpleMethods
    // TODO: inlineProperties
    // TODO: deadPropertyAssignmentElimination
    // TODO: optimizeCalls
    // TODO: inlineFunctions
    // TODO: inlineVariables
    // TODO: deadAssignmentsElimination
    // TODO: collapseObjectLiterals
    // TODO: removeUnusedCode
    // TODO: peepholeOptimizations
    // TODO: removeUnreachableCode
}

#[derive(Default)]
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
    suppressExcessPropertyErrors: bool,
    suppressImplicitAnyIndexErrors: bool,
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
