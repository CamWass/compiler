use ast::*;
use codegen::{text_writer::JsWriter, Emitter};
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::{errors::Handler, sync::Lrc, FileName, SourceMap, Span, DUMMY_SP};
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

    fn apply_transform<T>(&mut self, tr: T, name: &str, src: &str) -> Result<Program, ()>
    where
        T: FnOnce(Program, ast::NodeIdGen) -> Program,
    {
        let fm = self
            .cm
            .new_source_file(FileName::Real(name.into()), src.into());

        let node_id_gen = Rc::new(RefCell::new(ast::NodeIdGen::default()));

        let program = {
            let mut p = Parser::new(
                Syntax::Typescript(Default::default()),
                &fm,
                node_id_gen.clone(),
            );
            let res = p
                .parse_program()
                .map_err(|e| e.into_diagnostic(self.handler).emit());

            for e in p.take_errors() {
                e.into_diagnostic(self.handler).emit()
            }

            res?
        };

        let node_id_gen = Rc::try_unwrap(node_id_gen).unwrap().into_inner();

        let mut program = tr(program, node_id_gen);

        program.visit_mut_with(&mut DropSpan {
            preserve_ctxt: true,
        });
        program.visit_mut_with(&mut DropNodeId);

        Ok(program)
    }

    fn print(&mut self, program: &Program) -> String {
        let mut buf = vec![];
        {
            let mut emitter = Emitter {
                cfg: Default::default(),
                cm: self.cm.clone(),
                wr: Box::new(JsWriter::new(self.cm.clone(), "\n", &mut buf, None)),
                comments: None,
            };

            emitter.emit_program(program).unwrap();
        }

        let s = String::from_utf8_lossy(&buf);
        s.to_string()
    }
}

pub fn test_transform<T>(transform: T, input: &str, expected: &str)
where
    T: FnOnce(Program, ast::NodeIdGen) -> Program,
{
    Tester::run(|tester| {
        let expected = tester.apply_transform(|m, _| m, "output.js", expected)?;

        let mut actual = tester.apply_transform(transform, "input.js", input)?;

        actual.visit_mut_with(&mut DropSpan {
            preserve_ctxt: false,
        });
        actual.visit_mut_with(&mut DropNodeId);

        if actual == expected {
            return Ok(());
        }

        let (actual_src, expected_src) = (tester.print(&actual), tester.print(&expected));

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

struct DropSpan {
    preserve_ctxt: bool,
}
impl VisitMut<'_> for DropSpan {
    fn visit_mut_span(&mut self, span: &mut Span) {
        *span = if self.preserve_ctxt {
            DUMMY_SP.with_ctxt(span.ctxt())
        } else {
            DUMMY_SP
        };
    }
}
struct DropNodeId;
impl VisitMut<'_> for DropNodeId {
    fn visit_mut_node_id(&mut self, span: &mut NodeId) {
        *span = NodeId::DUMMY;
    }
}
