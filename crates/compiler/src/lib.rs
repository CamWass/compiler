#![feature(iter_order_by)]
#![feature(map_many_mut)]
// TODO:
#![allow(non_snake_case)]
#![deny(unused_imports)]

mod CoalesceVariableNames;
mod DataFlowAnalysis;
mod DefaultNameGenerator;
mod LiveVariablesAnalysis;
mod MaybeReachingVariableUse;
mod OptimizeArgumentsArray;
mod RenameLabels;
mod RenameVars;
mod control_flow;
mod convert;
mod denormalize;
mod find_vars;
mod graph;
mod normalize;
pub mod normalize_properties;
pub mod optimize_properties;
pub mod resolver;
mod utils;

#[cfg(test)]
mod testing;

use crate::resolver::resolver;
use atoms::JsWord;
use ecma_visit::VisitMutWith;
use global_common::{Globals, Mark, SyntaxContext, GLOBALS};
use serde::Deserialize;

pub type Id = (JsWord, SyntaxContext);

trait ToId {
    fn to_id(&self) -> Id;
}

impl ToId for ::ast::Ident {
    fn to_id(&self) -> Id {
        (self.sym.clone(), self.ctxt)
    }
}

impl ToId for ::ast::BindingIdent {
    fn to_id(&self) -> Id {
        self.id.to_id()
    }
}

#[derive(Debug, Clone, Copy, Default, Deserialize)]
pub struct PassConfig {
    #[serde(default)]
    pub optimize_arguments_array: bool,
    #[serde(default)]
    pub coalesce_variable_names: bool,
    #[serde(default)]
    pub rename_vars: bool,
    #[serde(default)]
    pub rename_labels: bool,
    #[serde(default)]
    pub optimize_properties: bool,
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

    pub fn compile(
        &self,
        mut ast: ::ast::Program,
        passes: PassConfig,
        program_data: &mut ::ast::ProgramData,
    ) -> ::ast::Program {
        self.run(|| {
            // TODO: maybe add an 'AST verifier' that checks basic invariants after
            // each pass (e.g. that no two nodes have the same node_id).

            normalize_properties::normalize_properties(&mut ast, program_data);
            normalize::normalize(&mut ast, program_data);

            let unresolved_mark = Mark::new();
            let top_level_mark = Mark::new();

            ast.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

            let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

            optimise(&mut ast, passes, program_data, unresolved_ctxt);

            finalise(&mut ast, passes, program_data, unresolved_ctxt);

            ast
        })
    }
}

fn optimise(
    ast: &mut ::ast::Program,
    passes: PassConfig,
    program_data: &mut ::ast::ProgramData,
    unresolved_ctxt: SyntaxContext,
) {
    if passes.optimize_arguments_array {
        OptimizeArgumentsArray::OptimizeArgumentsArray::process(ast, program_data, unresolved_ctxt);
    }

    // TODO: inlineAndCollapseProperties

    // TODO: inferConsts

    // TODO: earlyInlineVariables
    // TODO: earlyPeepholeOptimizations

    // TODO: removeUnusedCodeOnce

    // TODO: markPureFunctions

    getEarlyOptimizationLoopPasses(ast);

    // TODO: crossModuleCodeMotion
    // TODO: devirtualizeMethods
    // TODO: flowSensitiveInlineVariables
    getMainOptimizationLoop(ast);
}

fn getEarlyOptimizationLoopPasses(_ast: &mut ::ast::Program) {
    // TODO: inlineVariables
    // TODO: collapseObjectLiterals
    // TODO: removeUnusedCode
    // TODO: peepholeOptimizations
    // TODO: removeUnreachableCode
}

fn getMainOptimizationLoop(_ast: &mut ::ast::Program) {
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

fn finalise(
    ast: &mut ::ast::Program,
    passes: PassConfig,
    program_data: &mut ::ast::ProgramData,
    unresolved_ctxt: SyntaxContext,
) {
    // TODO: flowSensitiveInlineVariables
    // TODO: removeUnusedCodeOnce
    // TODO: crossModuleCodeMotion
    // TODO: crossModuleMethodMotion
    // TODO: optimizeConstructors
    // TODO: collapseAnonymousFunctions

    if passes.optimize_properties {
        optimize_properties::process(ast, program_data, unresolved_ctxt);
    }

    // TODO: renameProperties
    // TODO: convertToDottedProperties
    // TODO: rewriteFunctionExpressions
    // TODO: aliasStrings
    if passes.coalesce_variable_names {
        CoalesceVariableNames::coalesce_variable_names(ast, unresolved_ctxt, program_data);
    }
    // TODO: coalesceVariableNames
    // TODO: peepholeOptimizationsOnce
    // TODO: exploitAssign
    // TODO: collapseVariableDeclarations
    denormalize::denormalize(ast);
    if passes.rename_vars {
        RenameVars::rename_vars(ast, unresolved_ctxt);
    }

    if passes.rename_labels {
        RenameLabels::process(ast);
    }

    // TODO: latePeepholeOptimizations
    // TODO: optimizeToEs6
}
