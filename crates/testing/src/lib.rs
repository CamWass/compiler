#![deny(unused)]

pub use self::output::{NormalizedOutput, StdErr, StdOut, TestOutput};
use difference::Changeset;
use global_common::{FilePathMapping, SourceMap, errors::Handler};
pub use pretty_assertions::{assert_eq, assert_ne};
use std::{
    fmt::{self, Debug, Display, Formatter},
    rc::Rc,
};

mod output;
mod paths;
mod string_errors;

/// Run test and print errors.
pub fn run_test<F, Ret>(treat_err_as_bug: bool, op: F) -> Result<Ret, StdErr>
where
    F: FnOnce(Rc<SourceMap>, &Handler) -> Result<Ret, ()>,
{
    let cm = Rc::new(SourceMap::new(FilePathMapping::empty()));
    let (handler, errors) = self::string_errors::new_handler(cm.clone(), treat_err_as_bug);
    let result = global_common::GLOBALS.set(&global_common::Globals::new(), || op(cm, &handler));

    match result {
        Ok(res) => Ok(res),
        Err(()) => Err(errors.into()),
    }
}

pub fn diff(l: &str, r: &str) -> String {
    let cs = Changeset::new(l, r, "\n");

    format!("{cs}")
}

/// Used for assertions.
///
/// Prints string without escaping special characters on failure.
#[derive(PartialEq, Eq)]
pub struct DebugUsingDisplay<'a>(pub &'a str);

impl Debug for DebugUsingDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.0, f)
    }
}
