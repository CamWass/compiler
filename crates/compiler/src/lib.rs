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
pub mod RenameVars;
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

use crate::{
    control_flow::ControlFlowAnalysis::{ControlFlowAnalysis, ControlFlowRoot},
    resolver::resolve,
};
use ast::Program;
use serde::Deserialize;

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

pub struct Compiler;

impl Compiler {
    pub fn new() -> Self {
        Self
    }

    pub fn compile(
        &self,
        mut ast: ::ast::Program,
        passes: PassConfig,
        program_data: &mut ::ast::TransformerProgramData,
    ) -> ::ast::Program {
        // TODO: maybe add an 'AST verifier' that checks basic invariants after
        // each pass (e.g. that no two nodes have the same node_id).

        normalize::normalize(&mut ast, program_data);

        resolve(&mut ast, program_data);

        optimise(&mut ast, passes, program_data);

        finalise(&mut ast, passes, program_data);

        ast
    }
}

fn optimise(
    ast: &mut ::ast::Program,
    passes: PassConfig,
    program_data: &mut ::ast::TransformerProgramData,
) {
    if passes.optimize_arguments_array {
        OptimizeArgumentsArray::OptimizeArgumentsArray::process(ast, program_data);
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
    getMainOptimizationLoop(ast, passes, program_data);
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
    program_data: &mut ::ast::TransformerProgramData,
) {
    // TODO: inlineSimpleMethods
    // TODO: inlineProperties
    // TODO: deadPropertyAssignmentElimination
    // TODO: optimizeCalls
    // TODO: inlineFunctions

    if passes.inline_functions {
        inline_functions::process(ast, program_data);
    }

    // TODO: inlineVariables

    if passes.dead_assignment_elimination {
        dead_assignment_elimination::process(ast, program_data);
    }

    // TODO: collapseObjectLiterals
    // TODO: removeUnusedCode
    // TODO: peepholeOptimizations
    // TODO: removeUnreachableCode
}

fn finalise(
    ast: &mut ::ast::Program,
    passes: PassConfig,
    program_data: &mut ::ast::TransformerProgramData,
) {
    // TODO: flowSensitiveInlineVariables
    // TODO: removeUnusedCodeOnce
    // TODO: crossModuleCodeMotion
    // TODO: crossModuleMethodMotion
    // TODO: optimizeConstructors
    // TODO: collapseAnonymousFunctions

    if passes.optimize_properties {
        optimize_properties::process(ast, program_data);
    }

    // TODO: renameProperties
    if passes.convert_to_dot_properties {
        convert_to_dot_properties::process(ast, program_data);
    }
    // TODO: convertToDottedProperties
    // TODO: rewriteFunctionExpressions
    // TODO: aliasStrings
    if passes.coalesce_variable_names {
        CoalesceVariableNames::coalesce_variable_names(ast, program_data);
    }
    // TODO: peepholeOptimizationsOnce
    // TODO: exploitAssign

    if passes.collapse_variable_declarations {
        collapse_variable_declarations::process(ast);
    }

    denormalize::denormalize(ast);

    if passes.rename_vars {
        RenameVars::process(ast, program_data);
    }

    if passes.rename_labels {
        RenameLabels::process(ast, program_data);
    }

    late_peephole_optimisations(ast, passes, program_data);
    // TODO: latePeepholeOptimizations
    // TODO: optimizeToEs6

    if passes.optimise_equality {
        optimise_equality::process(ast);
    }
}

fn late_peephole_optimisations(
    ast: &mut ::ast::Program,
    passes: PassConfig,
    program_data: &mut ::ast::TransformerProgramData,
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
        peephole::remove_dead_code::process(ast, program_data);
    }

    //         new PeepholeMinimizeConditions(late),
    //         new PeepholeSubstituteAlternateSyntax(late),
    //         new PeepholeReplaceKnownMethods(late, useTypesForOptimization),
    //         new PeepholeFoldConstants(late, useTypesForOptimization),
    //         new PeepholeReorderConstantExpression());
    //   })
}

/// Renders the full CFG to a string in GraphViz DOT format, including the CFGs
/// of each inner function.
///
/// Returns `None` if the graph contains too many nodes for the resulting DOT
/// graph to be rendered in a reasonable amount of time.
pub fn print_control_flow_graph(
    program: &Program,
    program_data: &::ast::TransformerProgramData,
) -> Option<String> {
    let cfg = ControlFlowAnalysis::<()>::analyze(ControlFlowRoot::from(program), true).cfg;
    if cfg.graph.node_count() <= 200 {
        let mut dot = cfg.print_full(program_data);

        // Remove edge labels. These just make the graph harder to read (worse
        // edge layout and visual clutter). The edge colours already
        // differentiate the edges.
        dot = dot
            .replace("[label=\"Unconditional\", ", "[")
            .replace("[label=\"False\", ", "[")
            .replace("[label=\"True\", ", "[")
            .replace("[label=\"Exception\", ", "[");

        // Change AST edges from black to light grey to make the control flow
        // edges more prominent.
        dot = dot.replace(
            "[ label = \"\" ]",
            "[label = \"\", color=\"lightgrey\", fontcolor=\"lightgrey\"]",
        );

        Some(dot)
    } else {
        None
    }
}
