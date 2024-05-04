use ast::*;
use codegen::{text_writer::JsWriter, Emitter};
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::{errors::Handler, sync::Lrc, FileName, SourceMap};
use parser::{Parser, Syntax};
use std::cell::RefCell;
use std::rc::Rc;

struct Tester<'a> {
    cm: Lrc<SourceMap>,
    handler: &'a Handler,
}

impl<'a> Tester<'a> {
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
    ) -> Result<(Program, ProgramData), ()>
    where
        T: FnOnce(Program, &mut ast::ProgramData) -> Program,
    {
        let fm = self
            .cm
            .new_source_file(FileName::Real(name.into()), src.into());

        let program_data = Rc::new(RefCell::new(ast::ProgramData::default()));

        let program = {
            let mut p = Parser::new(
                Syntax::Typescript(Default::default()),
                &fm,
                program_data.clone(),
            );
            let res = p
                .parse_program()
                .map_err(|e| e.into_diagnostic(self.handler).emit());

            for e in p.take_errors() {
                e.into_diagnostic(self.handler).emit()
            }

            res?
        };

        let mut program_data = Rc::try_unwrap(program_data).unwrap().into_inner();

        let mut program = tr(program, &mut program_data);

        program.visit_mut_with(&mut DropNodeId);

        Ok((program, program_data))
    }

    fn print(&mut self, program: &Program, program_data: &ProgramData) -> String {
        let mut buf = vec![];
        {
            let mut emitter = Emitter {
                cfg: Default::default(),
                cm: self.cm.clone(),
                wr: Box::new(JsWriter::new(self.cm.clone(), "\n", &mut buf, None)),
                comments: None,
                program_data,
            };

            emitter.emit_program(program).unwrap();
        }

        let s = String::from_utf8_lossy(&buf);
        s.to_string()
    }
}

pub fn test_transform<T>(transform: T, input: &str, expected: &str)
where
    T: FnOnce(Program, &mut ast::ProgramData) -> Program,
{
    Tester::run(|tester| {
        let expected = tester.apply_transform(|m, _| m, "output.js", expected)?;

        let mut actual = tester.apply_transform(transform, "input.js", input)?;

        actual.0.visit_mut_with(&mut DropNodeId);

        if actual.0 == expected.0 {
            return Ok(());
        }

        let (actual_src, expected_src) = (
            tester.print(&actual.0, &actual.1),
            tester.print(&expected.0, &actual.1),
        );

        if actual_src == expected_src {
            return Ok(());
        }

        println!(">>>>> Input <<<<<\n{}", input);
        println!(">>>>> Output <<<<<\n{}", actual_src);
        println!(">>>>> Expected <<<<<\n{}", expected_src);
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

struct DropNodeId;
impl VisitMut<'_> for DropNodeId {
    fn visit_mut_node_id(&mut self, span: &mut NodeId) {
        *span = NodeId::DUMMY;
    }
}
