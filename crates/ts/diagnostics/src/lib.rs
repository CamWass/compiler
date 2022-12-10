#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

#[derive(Debug, Clone, Copy)]
pub struct DiagnosticMessage {
    pub key: &'static str,
    pub category: DiagnosticCategory,
    pub code: usize,
    pub message: &'static str,
    pub reportsUnnecessary: bool,
    pub reportsDeprecated: bool,
    pub elidedInCompatabilityPyramid: bool,
}

#[derive(Clone, Copy, Debug)]
pub enum DiagnosticCategory {
    Warning,
    Error,
    Suggestion,
    Message,
}

include!(concat!(env!("OUT_DIR"), "/diagnostics.rs"));
