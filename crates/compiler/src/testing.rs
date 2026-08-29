use std::rc::Rc;

use ast::*;
use codegen::{Emitter, text_writer::JsWriter};
use common::{FileName, SourceMap, errors::Handler};
use parser::{Parser, Syntax};
use rustc_hash::FxHashSet;
use visit::{Visit, VisitWith};

struct Tester<'a> {
    cm: Rc<SourceMap>,
    handler: &'a Handler,
}

impl Tester<'_> {
    fn run<F>(op: F)
    where
        F: FnOnce(&mut Tester<'_>) -> Result<(), ()>,
    {
        let out = ::testing::run_test(false, |cm, handler| op(&mut Tester { cm, handler }));

        match out {
            Ok(()) => {}
            Err(stderr) => panic!("Stderr:\n{}", stderr),
        }
    }

    fn apply_transform<T>(
        &mut self,
        tr: T,
        name: &str,
        src: &str,
    ) -> Result<(Program, TransformerProgramData), ()>
    where
        T: FnOnce(&mut Program, &mut TransformerProgramData),
    {
        let fm = self
            .cm
            .new_source_file(FileName::Real(name.into()), src.into());

        let mut program_data = ParserProgramData::default();

        let mut program = {
            let mut p = Parser::new(
                Syntax::Typescript(Default::default()),
                &fm,
                &mut program_data,
            );
            let res = p
                .parse_program()
                .map_err(|e| e.into_diagnostic(self.handler).emit());

            for e in p.take_errors() {
                e.into_diagnostic(self.handler).emit();
            }

            res?
        };

        let mut program_data = program_data.into_transformer_program_data();

        tr(&mut program, &mut program_data);

        Ok((program, program_data))
    }

    fn print(&mut self, program: &Program, program_data: TransformerProgramData) -> String {
        let program_data = program_data.into_codegen_program_data();

        let mut buf = String::new();

        let mut emitter = Emitter::new(
            Default::default(),
            self.cm.clone(),
            JsWriter::new("\n", &mut buf, None),
            &program_data,
        );

        emitter.emit_program(program).unwrap();

        buf
    }
}

pub fn test_transform<T>(transform: T, input: &str, expected: &str)
where
    T: FnOnce(&mut Program, &mut TransformerProgramData),
{
    Tester::run(|tester| {
        let expected = tester.apply_transform(|_, _| {}, "output.js", expected)?;
        let actual = tester.apply_transform(transform, "input.js", input)?;

        expected.0.visit_with(&mut Validator::default());
        actual.0.visit_with(&mut Validator::default());

        let (actual_src, expected_src) = (
            tester.print(&actual.0, actual.1),
            tester.print(&expected.0, expected.1),
        );

        if actual_src == expected_src {
            return Ok(());
        }

        if actual.0.eq_ignoring_node_id(&expected.0) {
            return Ok(());
        }

        println!(">>>>> Input <<<<<\n{input}");
        println!(">>>>> Output <<<<<\n{actual_src}");
        println!(">>>>> Expected <<<<<\n{expected_src}");
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

#[derive(Default)]
struct Validator {
    node_ids: FxHashSet<NodeId>,
}

impl Visit<'_> for Validator {
    fn visit_node_id(&mut self, node_id: &NodeId) {
        assert!(*node_id != NodeId::DUMMY);

        if !self.node_ids.insert(*node_id) {
            panic!("Duplicate node ID {:?} has already been seen", node_id);
        }
    }

    fn visit_name_id(&mut self, name: &NameId) {
        assert!(*name != NameId::DUMMY);
    }
}
