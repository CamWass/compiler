#![deny(unused)]

use ansi_term::Color;
use ast::*;
use codegen::Emitter;
use common::{FileName, SourceMap, errors::Handler, util::take::Take};
use parser::{Parser, Syntax};
use visit::{VisitMut, VisitMutWith};

use std::rc::Rc;

pub struct Tester<'a> {
    pub cm: Rc<SourceMap>,
    pub handler: &'a Handler,
}

impl Tester<'_> {
    pub fn run<F, Ret>(op: F) -> Ret
    where
        F: FnOnce(&mut Tester<'_>) -> Result<Ret, ()>,
    {
        let out = ::testing::run_test(false, |cm, handler| op(&mut Tester { cm, handler }));

        match out {
            Ok(ret) => ret,
            Err(stderr) => panic!("Stderr:\n{stderr}"),
        }
    }

    #[allow(clippy::result_unit_err)]
    pub fn apply_transform<T: for<'b> VisitMut<'b>>(
        &mut self,
        mut tr: T,
        name: &str,
        syntax: Syntax,
        src: &str,
    ) -> Result<(Module, ParserProgramData), ()> {
        let fm = self
            .cm
            .new_source_file(FileName::Real(name.into()), src.into());

        let mut program_data = ast::ParserProgramData::default();

        let module = {
            let mut p = Parser::new(syntax, &fm, &mut program_data);
            let res = p
                .parse_module()
                .map_err(|e| e.into_diagnostic(self.handler).emit());

            for e in p.take_errors() {
                e.into_diagnostic(self.handler).emit();
            }

            res?
        };

        let mut module = Program::Module(module);
        module.visit_mut_with(&mut tr);
        module.visit_mut_with(&mut Normalizer);

        match module {
            Program::Module(m) => Ok((m, program_data)),
            Program::Script(_) => unreachable!(),
        }
    }

    pub fn print(&mut self, module: &Module, program_data: ParserProgramData) -> String {
        let program_data = program_data.into_codegen_program_data();

        let mut buf = String::new();

        let mut emitter = Emitter::new(
            Default::default(),
            self.cm.clone(),
            codegen::JsWriter::new("\n", &mut buf, None),
            &program_data,
        );

        emitter.emit_module(module).unwrap();

        buf
    }
}

#[track_caller]
pub fn test_transform<F, P>(
    syntax: Syntax,
    tr: F,
    input: &str,
    expected: &str,
    _always_ok_if_code_eq: bool,
) where
    F: FnOnce(&mut Tester) -> P,
    P: for<'a> VisitMut<'a>,
{
    struct Dummy;
    impl VisitMut<'_> for Dummy {}

    Tester::run(|tester| {
        let expected = tester.apply_transform(Dummy, "output.js", syntax, expected)?;

        println!("----- Actual -----");

        let tr = tr(tester);
        let actual = tester.apply_transform(tr, "input.js", syntax, input)?;

        if actual.0.eq_ignoring_node_id(&expected.0) {
            return Ok(());
        }

        let (actual_src, expected_src) = (
            tester.print(&actual.0, actual.1),
            tester.print(&expected.0, expected.1),
        );

        if actual_src == expected_src {
            return Ok(());
        }

        println!(">>>>> {} <<<<<\n{}", Color::Green.paint("Orig"), input);
        println!(">>>>> {} <<<<<\n{}", Color::Green.paint("Code"), actual_src);

        if actual_src != expected_src {
            panic!(
                r#"assertion failed: `(left == right)`
            {}"#,
                ::testing::diff(&actual_src, &expected_src),
            );
        }

        Err(())
    });
}

/// Test transformation.
#[macro_export]
macro_rules! test {
    (ignore, $syntax:expr, $tr:expr, $test_name:ident, $input:expr, $expected:expr) => {
        #[test]
        #[ignore]
        fn $test_name() {
            ::transform_testing::test_transform($syntax, $tr, $input, $expected, false)
        }
    };

    ($syntax:expr, $tr:expr, $test_name:ident, $input:expr, $expected:expr) => {
        #[test]
        fn $test_name() {
            ::transform_testing::test_transform($syntax, $tr, $input, $expected, false)
        }
    };

    ($syntax:expr, $tr:expr, $test_name:ident, $input:expr, $expected:expr, ok_if_code_eq) => {
        #[test]
        fn $test_name() {
            ::transform_testing::test_transform($syntax, $tr, $input, $expected, true)
        }
    };
}

struct Normalizer;
impl VisitMut<'_> for Normalizer {
    fn visit_mut_pat_or_expr(&mut self, node: &mut PatOrExpr) {
        node.visit_mut_children_with(self);

        if let PatOrExpr::Pat(pat) = node {
            if let Pat::Expr(e) = &mut **pat {
                *node = PatOrExpr::Expr(e.take());
            }
        }
    }
}
