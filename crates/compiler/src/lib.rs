#![feature(step_trait_ext)]
// TODO:
// #![allow(warnings)]
// TODO: this can be removed by updating rust tool chain file
#![feature(map_into_keys_values)]

use ast::*;
use control_flow_analysis::ControlFlowAnalysis;
use control_flow_graph::{Annotation, Node};
use ctx::Ctx;
use typed_scope_creator::TypedScopeCreator;

use crate::type_inference_pass::TypeInferencePass;

mod abstract_scope;
mod control_flow_analysis;
mod control_flow_graph;
mod data_flow_analysis;
mod f;
mod flow_scope;
mod foo;
mod lattice_element;
mod linked_flow_scope;
mod print;
mod static_scope;
mod static_slot;
mod static_typed_scope;
mod static_typed_slot;
mod syntactic_scope_creator;
mod type_inference;
mod type_inference_pass;
mod typed_scope;
mod typed_scope_creator;
mod typed_var;
mod typing;
mod ctx;

// TODO: use #[must_use] for functions that are annotated with @CheckReturnValue in their java equivalent

pub struct Compiler<'tcx> {
    ctx: Ctx<'tcx>
}

impl<'tcx> Compiler<'tcx> {
    pub fn new() -> Self {
        Self {
            ctx: Ctx::new()
        }
    }

    // pub fn analyse(&self, todo: &Program) {
    //     let n = match todo {
    //         Program::Module(m) => Node::Module(m),
    //         Program::Script(s) => Node::Script(s),
    //     };

    //     // TODO:
    //     let should_traverse_functions = true;

    //     let mut a = ControlFlowAnalysis::<'_, DummyAnnotation, DummyAnnotation>::new(
    //         n,
    //         should_traverse_functions,
    //     );

    //     a.process();

    //     a.print_full_graph();
    // }

    pub fn analyse(&'tcx mut self, todo: &'tcx Program) {
        let n = AstNode::from(todo);

        let mut typed_scope_creator =  TypedScopeCreator::new();

        let mut pass = TypeInferencePass{};

        let s = pass.inferAllScopes(&mut self.ctx, &mut typed_scope_creator,n);
    }
}
