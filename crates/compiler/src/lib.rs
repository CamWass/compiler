// TODO:
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(unused_variables)]
#![deny(non_shorthand_field_patterns)]
#![allow(warnings)]

mod DefaultNameGenerator;
pub mod ast;
mod binder;
mod checker;
mod colors;
mod disambiguate;
mod graph;
pub mod node;
mod normalize;
pub mod types;
mod types_composition;
pub mod utils;
pub mod visit;

use crate::ast as local_ast;
use crate::node::{Bind, BoundNode};
use crate::visit::{Visit, VisitWith};
pub use checker::Checker;
use ecma_visit::VisitMutWith;
use std::rc::Rc;
use types::*;

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

pub struct Compiler {}

impl Compiler {
    pub fn compile(
        &self,
        libs: Vec<(String, ::ast::Program)>,
        program: (String, ::ast::Program),
        disambiguate: bool,
        ambiguate: bool,
    ) -> ::ast::Program {
        let mut source_files = Vec::with_capacity(libs.len() + 1);

        let mut program_ast = program.1;

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

        let mut colours = colors::color_collector::collect(&mut checker, &p);

        if disambiguate {
            disambiguate::DisambiguateProperties::DisambiguateProperties::process(
                &mut program_ast,
                &p,
                &mut colours,
            );
        }

        if ambiguate {
            disambiguate::AmbiguateProperties::AmbiguateProperties::process(
                &mut program_ast,
                &p,
                &mut colours,
            );
        }

        program_ast
    }
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