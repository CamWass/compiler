#![cfg_attr(test, feature(iter_order_by))]
#![deny(unused_imports)]
#![deny(unused)]
// TODO:
#![allow(non_snake_case)]

mod CoalesceVariableNames;
mod DataFlowAnalysis;
mod LiveVariablesAnalysis;
mod MaybeReachingVariableUse;
mod OptimizeArgumentsArray;
mod RenameLabels;
mod RenameVars;
mod collapse_variable_declarations;
mod control_flow;
mod convert;
mod convert_to_dot_properties;
mod dead_assignment_elimination;
mod denormalize;
mod find_vars;
mod graph;
mod inline_functions;
mod name_generator;
mod node_util;
mod normalize;
mod optimise_equality;
pub mod optimize_properties;
mod peephole;
pub mod resolver;
mod utils;

#[cfg(test)]
mod testing;

use crate::resolver::resolve;
use atoms::JsWord;
use common::{GLOBALS, Globals, Mark, SyntaxContext};
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
    #[serde(default)]
    pub fuse_stmts: bool,
    #[serde(default)]
    pub inline_functions: bool,
    #[serde(default)]
    pub convert_to_dot_properties: bool,
    #[serde(default)]
    pub dead_assignment_elimination: bool,
    #[serde(default)]
    pub optimise_equality: bool,
    #[serde(default)]
    pub remove_dead_code: bool,
    #[serde(default)]
    pub collapse_variable_declarations: bool,
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

            normalize::normalize(&mut ast, program_data);

            let unresolved_mark = Mark::new();

            resolve(&mut ast, unresolved_mark);

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
    // TODO: PeepholeRemoveDeadCode

    // TODO: removeUnusedCodeOnce

    // TODO: markPureFunctions

    getEarlyOptimizationLoopPasses(ast);

    // TODO: crossModuleCodeMotion
    // TODO: devirtualizeMethods
    // TODO: flowSensitiveInlineVariables
    getMainOptimizationLoop(ast, passes, program_data, unresolved_ctxt);
}

fn getEarlyOptimizationLoopPasses(_ast: &mut ::ast::Program) {
    // TODO: inlineVariables
    // TODO: collapseObjectLiterals
    // TODO: removeUnusedCode
    // TODO: peepholeOptimizations
    // TODO: removeUnreachableCode
}

fn getMainOptimizationLoop(
    ast: &mut ::ast::Program,
    passes: PassConfig,
    program_data: &mut ::ast::ProgramData,
    unresolved_ctxt: SyntaxContext,
) {
    // TODO: inlineSimpleMethods
    // TODO: inlineProperties
    // TODO: deadPropertyAssignmentElimination
    // TODO: optimizeCalls
    // TODO: inlineFunctions

    if passes.inline_functions {
        inline_functions::process(ast, program_data, unresolved_ctxt);
    }

    // TODO: inlineVariables

    if passes.dead_assignment_elimination {
        dead_assignment_elimination::process(ast, program_data, unresolved_ctxt);
    }

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
    if passes.convert_to_dot_properties {
        convert_to_dot_properties::process(ast, program_data, unresolved_ctxt);
    }
    // TODO: convertToDottedProperties
    // TODO: rewriteFunctionExpressions
    // TODO: aliasStrings
    if passes.coalesce_variable_names {
        CoalesceVariableNames::coalesce_variable_names(ast, unresolved_ctxt, program_data);
    }
    // TODO: peepholeOptimizationsOnce
    // TODO: exploitAssign

    if passes.collapse_variable_declarations {
        collapse_variable_declarations::process(ast);
    }

    denormalize::denormalize(ast);

    if passes.rename_vars {
        RenameVars::process(ast, unresolved_ctxt);
    }

    if passes.rename_labels {
        RenameLabels::process(ast);
    }

    late_peephole_optimisations(ast, passes, program_data, unresolved_ctxt);
    // TODO: latePeepholeOptimizations
    // TODO: optimizeToEs6

    if passes.optimise_equality {
        optimise_equality::process(ast, unresolved_ctxt);
    }
}

fn late_peephole_optimisations(
    ast: &mut ::ast::Program,
    passes: PassConfig,
    program_data: &mut ::ast::ProgramData,
    unresolved_ctxt: SyntaxContext,
) {
    //     final boolean late = true;
    //     final boolean useTypesForOptimization = options.useTypesForLocalOptimization;
    //     return new PeepholeOptimizationsPass(
    //         compiler,
    //         "latePeepholeOptimizations",
    if passes.fuse_stmts {
        peephole::fuse_stmts::process(ast, program_data);
    }

    if passes.remove_dead_code {
        peephole::remove_dead_code::process(ast, program_data, unresolved_ctxt);
    }

    //         new PeepholeMinimizeConditions(late),
    //         new PeepholeSubstituteAlternateSyntax(late),
    //         new PeepholeReplaceKnownMethods(late, useTypesForOptimization),
    //         new PeepholeFoldConstants(late, useTypesForOptimization),
    //         new PeepholeReorderConstantExpression());
    //   })
}
