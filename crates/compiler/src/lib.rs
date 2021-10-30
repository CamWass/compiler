#![feature(cell_update)]
// TODO:
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(unused_variables)]
#![deny(non_shorthand_field_patterns)]
#![allow(warnings)]

mod ast;
mod binder;
mod checker;
mod node;
mod types;
mod types_composition;
mod typesv2;
mod typesv3;
mod utils;

use crate::ast as local_ast;
use checker::Checker;
use types::*;

pub struct SourceFile {
    pub file_name: String,
    pub program: local_ast::Program,

    /// JS identifier-declarations that are intended to merge with globals
    jsGlobalAugmentations: SymbolTable,
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

        for (file_name, ast) in programs {
            let ast = local_ast::convert::convert_program(ast);
            source_files.push(SourceFile {
                file_name,
                program: ast.clone(),
                jsGlobalAugmentations: Default::default(),
            });
            program_asts.push(ast);
        }

        let host = TypeCheckerHost {
            files: source_files,
            compiler_options: CompilerOptions {},
        };

        let checker = Checker::new(host);
    }
}

pub struct TypeCheckerHost {
    pub files: Vec<SourceFile>,
    pub compiler_options: CompilerOptions,
}

#[derive(Clone, Copy)]
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
// downlevelIteration?: boolean;
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
// isolatedModules?: boolean;
// jsx?: JsxEmit;
// keyofStringsOnly?: boolean;
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
// noPropertyAccessFromIndexSignature?: boolean;
// assumeChangesOnlyAffectDirectDependencies?: boolean;
// noLib?: boolean;
// noResolve?: boolean;
// noUncheckedIndexedAccess?: boolean;
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

    pub fn getStrictOptionValue(&self, field: &'static str) -> bool {
        // todo:
        false
    }

    pub fn exactOptionalPropertyTypes(&self) -> bool {
        // todo:
        false
    }
}
