//! Creation of dummy nodes used by the parser during error recovery. Rather
//! than halt parsing on any error, we try to continue parsing, but we have to
//! insert _something_ into the AST, so we create dummy nodes.
//!
//! Since we don't continue compilation when the parser errors, these nodes
//! should never escape the parser.
//!
//! Having these sentinel 'invalid' nodes prevents us from having dedicated
//! 'invalid' AST node types that we'd have to explicitly ignore throughout the
//! compiler.

use common::DUMMY_SP;

use super::*;

impl Parser<'_> {
    fn create_invalid_ident(&mut self) -> Ident {
        // We should always emit an error before we create an invalid node;
        // If the parser has no errors, then compilation will continue and the
        // invalid node will escape the parse stage.
        debug_assert!(self.input.has_errors());

        Ident {
            node_id: node_id!(self, DUMMY_SP),
            name: INVALID_IDENT_NAME,
        }
    }

    pub(super) fn create_invalid_binding_ident(&mut self) -> BindingIdent {
        BindingIdent::from_ident(self.create_invalid_ident())
    }

    pub(super) fn create_invalid_expr(&mut self) -> Expr {
        Expr::Ident(self.create_invalid_ident())
    }

    pub(super) fn create_invalid_binding_element(&mut self) -> BindingElement {
        BindingElement {
            node_id: node_id!(self, DUMMY_SP),
            target: self.create_invalid_binding_pat_or_ident(),
            init: None,
        }
    }

    pub(super) fn create_invalid_assignment_element(&mut self) -> AssignmentElement {
        AssignmentElement {
            node_id: node_id!(self, DUMMY_SP),
            target: Box::new(self.create_invalid_assign_target()),
            init: None,
        }
    }

    pub(super) fn create_invalid_assign_target(&mut self) -> AssignTarget {
        AssignTarget::Simple(self.create_invalid_simple_assign_target())
    }

    pub(super) fn create_invalid_simple_assign_target(&mut self) -> SimpleAssignTarget {
        SimpleAssignTarget::Ident(self.create_invalid_binding_ident())
    }

    pub(super) fn create_invalid_binding_pat_or_ident(&mut self) -> BindingPatOrIdent {
        BindingPatOrIdent::Ident(self.create_invalid_binding_ident())
    }

    pub(super) fn create_invalid_param(&mut self) -> Param {
        Param {
            node_id: node_id!(self, DUMMY_SP),
            pat: self.create_invalid_binding_element(),
        }
    }
}
