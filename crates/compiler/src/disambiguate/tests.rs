use ast::{Pat, *};
use codegen::{text_writer::JsWriter, Emitter};
use ecma_visit::{as_folder, Fold, FoldWith, VisitMut, VisitMutWith};
use global_common::{errors::Handler, sync::Lrc, FileName, SourceMap, Span, DUMMY_SP};
use parser::{error::Error, lexer::Lexer, Parser, Syntax};

pub struct Tester<'a> {
    pub cm: Lrc<SourceMap>,
    pub handler: &'a Handler,
}

impl<'a> Tester<'a> {
    pub fn run<F>(op: F)
    where
        F: FnOnce(&mut Tester<'_>) -> Result<(), ()>,
    {
        let out = ::testing::run_test(false, |cm, handler| op(&mut Tester { cm, handler }));

        match out {
            Ok(()) => {}
            Err(stderr) => panic!("Stderr:\n{}", stderr),
        }
    }

    pub fn with_parser<F, T>(
        &mut self,
        file_name: &str,
        syntax: Syntax,
        src: &str,
        op: F,
    ) -> Result<T, ()>
    where
        F: FnOnce(&mut Parser<Lexer<'_>>) -> Result<T, Error>,
    {
        let fm = self
            .cm
            .new_source_file(FileName::Real(file_name.into()), src.into());

        let mut p = Parser::new(syntax, &fm);
        let res = op(&mut p).map_err(|e| e.into_diagnostic(self.handler).emit());

        for e in p.take_errors() {
            e.into_diagnostic(self.handler).emit()
        }

        res
    }

    pub fn parse_module(&mut self, file_name: &str, src: &str) -> Result<Module, ()> {
        self.with_parser(file_name, Syntax::default(), src, |p| p.parse_module())
    }

    pub fn parse_stmts(&mut self, file_name: &str, src: &str) -> Result<Vec<Stmt>, ()> {
        let stmts = self.with_parser(file_name, Syntax::default(), src, |p| {
            p.parse_script().map(|script| script.body)
        })?;

        Ok(stmts)
    }

    pub fn parse_stmt(&mut self, file_name: &str, src: &str) -> Result<Stmt, ()> {
        let mut stmts = self.parse_stmts(file_name, src)?;
        assert!(stmts.len() == 1);

        Ok(stmts.pop().unwrap())
    }

    pub fn apply_transform<T: VisitMut>(
        &mut self,
        mut tr: T,
        name: &str,
        syntax: Syntax,
        src: &str,
    ) -> Result<Module, ()> {
        let fm = self
            .cm
            .new_source_file(FileName::Real(name.into()), src.into());

        let mut module = {
            let mut p = Parser::new(syntax, &fm);
            let res = p
                .parse_module()
                .map_err(|e| e.into_diagnostic(self.handler).emit());

            for e in p.take_errors() {
                e.into_diagnostic(self.handler).emit()
            }

            res?
        };

        module.visit_mut_with(&mut tr);
        module.visit_mut_with(&mut DropSpan {
            preserve_ctxt: true,
        });
        module.visit_mut_with(&mut DropNodeId);

        Ok(module)
    }

    pub fn print(&mut self, module: &Module) -> String {
        let mut buf = vec![];
        {
            let mut emitter = Emitter {
                cfg: Default::default(),
                cm: self.cm.clone(),
                wr: Box::new(JsWriter::new(self.cm.clone(), "\n", &mut buf, None)),
                comments: None,
            };

            // println!("Emitting: {:?}", module);
            emitter.emit_module(module).unwrap();
        }

        let s = String::from_utf8_lossy(&buf);
        s.to_string()
    }
}

pub(crate) fn test_transform<F, P>(
    syntax: Syntax,
    tr: F,
    input: &str,
    expected: &str,
    ok_if_code_eq: bool,
) where
    F: FnOnce(&mut Tester) -> P,
    P: VisitMut,
{
    Tester::run(|tester| {
        let expected = tester.apply_transform(
            DropSpan {
                preserve_ctxt: true,
            },
            "output.js",
            syntax,
            expected,
        )?;

        println!("----- Actual -----");

        let tr = tr(tester);
        let mut actual = tester.apply_transform(tr, "input.js", syntax, input)?;

        actual.visit_mut_with(&mut DropSpan {
            preserve_ctxt: false,
        });
        actual.visit_mut_with(&mut DropNodeId);

        if actual == expected {
            return Ok(());
        }

        let (actual_src, expected_src) = (tester.print(&actual), tester.print(&expected));

        if actual_src == expected_src {
            if ok_if_code_eq {
                return Ok(());
            }
            // Diff it
            println!(">>>>> Code <<<<<\n{}", actual_src);
            assert_eq!(actual, expected, "different ast was detected");
            return Err(());
        }

        println!(">>>>> Orig <<<<<\n{}", input);
        println!(">>>>> Code <<<<<\n{}", actual_src);
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
    pub preserve_ctxt: bool,
}
impl VisitMut for DropSpan {
    fn visit_mut_span(&mut self, span: &mut Span) {
        *span = if self.preserve_ctxt {
            DUMMY_SP.with_ctxt(span.ctxt())
        } else {
            DUMMY_SP
        };
    }
}
struct DropNodeId;
impl VisitMut for DropNodeId {
    fn visit_mut_node_id(&mut self, span: &mut NodeId) {
        *span = NodeId::from_u32(0);
    }
}
