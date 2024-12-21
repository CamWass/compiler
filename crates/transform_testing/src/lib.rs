#![deny(clippy::all)]
#![deny(clippy::all)]
#![deny(unused)]
#![allow(clippy::result_unit_err)]

use ansi_term::Color;
use ast::{Pat, *};
use codegen::Emitter;
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::{errors::Handler, sync::Lrc, util::take::Take, FileName, SourceMap};
use parser::{Parser, Syntax};
// use swc_ecma_testing::{exec_node_js, JsExecOptions};
// use swc_ecma_transforms_base::{
//     fixer,
//     helpers::{inject_helpers, HELPERS},
//     hygiene,
//     pass::noop,
// };
// use swc_ecma_utils::{quote_ident, quote_str, ExprFactory};

pub struct Tester<'a> {
    pub cm: Lrc<SourceMap>,
    pub handler: &'a Handler,
}

impl<'a> Tester<'a> {
    pub fn run<F, Ret>(op: F) -> Ret
    where
        F: FnOnce(&mut Tester<'_>) -> Result<Ret, ()>,
    {
        let out = ::testing::run_test(false, |cm, handler| op(&mut Tester { cm, handler }));

        match out {
            Ok(ret) => ret,
            Err(stderr) => panic!("Stderr:\n{}", stderr),
        }
    }

    pub fn apply_transform<T: for<'b> VisitMut<'b>>(
        &mut self,
        mut tr: T,
        name: &str,
        syntax: Syntax,
        src: &str,
    ) -> Result<(Module, ProgramData), ()> {
        let fm = self
            .cm
            .new_source_file(FileName::Real(name.into()), src.into());

        let mut program_data = ast::ProgramData::default();

        let module = {
            let mut p = Parser::new(syntax, &fm, &mut program_data);
            let res = p
                .parse_module()
                .map_err(|e| e.into_diagnostic(self.handler).emit());

            for e in p.take_errors() {
                e.into_diagnostic(self.handler).emit()
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

    pub fn print(&mut self, module: &Module, program_data: &ProgramData) -> String {
        let mut buf = vec![];
        {
            let mut emitter = Emitter::new(
                Default::default(),
                self.cm.clone(),
                codegen::JsWriter::new("\n", &mut buf, None),
                program_data,
            );

            // println!("Emitting: {:?}", module);
            emitter.emit_module(module).unwrap();
        }

        let s = String::from_utf8_lossy(&buf);
        s.to_string()
    }
}

// struct RegeneratorHandler;

// impl VisitMut<'_> for RegeneratorHandler {
//     fn visit_mut_module_item(&mut self, item: &mut ModuleItem) {
//         if let ModuleItem::ModuleDecl(ModuleDecl::Import(import)) = item {
//             if &*import.src.value != "regenerator-runtime" {
//                 return;
//             }

//             let s = import.specifiers.iter().find_map(|v| match v {
//                 ImportSpecifier::Default(rt) => Some(rt.local.clone()),
//                 _ => None,
//             });

//             let s = match s {
//                 Some(v) => v,
//                 _ => return,
//             };

//             let init = Box::new(Expr::Call(CallExpr {
//                 span: DUMMY_SP,
//                 callee: quote_ident!("require").as_callee(),
//                 args: vec![quote_str!("regenerator-runtime").as_arg()],
//                 type_args: Default::default(),
//             }));

//             let decl = VarDeclarator {
//                 span: DUMMY_SP,
//                 name: s.into(),
//                 init: Some(init),
//                 definite: Default::default(),
//             };
//             *item = ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
//                 span: import.span,
//                 kind: VarDeclKind::Var,
//                 declare: false,
//                 decls: vec![decl],
//             }))))
//         }
//     }
// }

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

        // match ::std::env::var("PRINT_HYGIENE") {
        //     Ok(ref s) if s == "1" => {
        //         let hygiene_src = tester.print(
        //             &actual.clone().fold_with(&mut HygieneVisualizer),
        //         );
        //         println!("----- Hygiene -----\n{}", hygiene_src);
        //     }
        //     _ => {}
        // }

        if actual.0 == expected.0 {
            return Ok(());
        }

        let (actual_src, expected_src) = (
            tester.print(&actual.0, &actual.1),
            tester.print(&expected.0, &expected.1),
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

// /// Execute `node` for `input` and ensure that it prints same output after
// /// transformation.
// pub fn compare_stdout<F, P>(syntax: Syntax, tr: F, input: &str)
// where
//     F: FnOnce(&mut Tester<'_>) -> P,
//     P: Fold,
// {
//     Tester::run(|tester| {
//         let tr = make_tr(tr, tester);

//         let module = tester.apply_transform(tr, "input.js", syntax, input)?;

//         match ::std::env::var("PRINT_HYGIENE") {
//             Ok(ref s) if s == "1" => {
//                 let hygiene_src = tester.print(
//                     &module.clone().fold_with(&mut HygieneVisualizer),
//                 );
//                 println!("----- Hygiene -----\n{}", hygiene_src);
//             }
//             _ => {}
//         }

//         let mut module = module
//             .fold_with(&mut hygiene::hygiene())
//             .fold_with(&mut fixer::fixer());

//         let src_without_helpers = tester.print(&module);
//         module = module.fold_with(&mut inject_helpers(Mark::fresh(Mark::root())));

//         let transformed_src = tester.print(&module);

//         println!(
//             "\t>>>>> Orig <<<<<\n{}\n\t>>>>> Code <<<<<\n{}",
//             input, src_without_helpers
//         );

//         let expected = stdout_of(input).unwrap();

//         println!("\t>>>>> Expected stdout <<<<<\n{}", expected);

//         let actual = stdout_of(&transformed_src).unwrap();

//         assert_eq!(expected, actual);

//         Ok(())
//     })
// }

// /// Execute `jest` after transpiling `input` using `tr`.
// pub fn exec_tr<F, P>(test_name: &str, syntax: Syntax, tr: F, input: &str)
// where
//     F: FnOnce(&mut Tester<'_>) -> P,
//     P: Fold,
// {
//     Tester::run(|tester| {
//         let tr = make_tr(tr, tester);

//         let module = tester.apply_transform(
//             tr,
//             "input.js",
//             syntax,
//             &format!(
//                 "it('should work', async function () {{
//                     {}
//                 }})",
//                 input
//             ),
//         )?;
//         match ::std::env::var("PRINT_HYGIENE") {
//             Ok(ref s) if s == "1" => {
//                 let hygiene_src = tester.print(
//                     &module.clone().fold_with(&mut HygieneVisualizer),
//                 );
//                 println!("----- Hygiene -----\n{}", hygiene_src);
//             }
//             _ => {}
//         }

//         let mut module = module
//             .fold_with(&mut hygiene::hygiene())
//             .fold_with(&mut fixer::fixer());

//         let src_without_helpers = tester.print(&module);
//         module = module.fold_with(&mut inject_helpers(Mark::fresh(Mark::root())));

//         let src = tester.print(&module);

//         println!(
//             "\t>>>>> {} <<<<<\n{}\n\t>>>>> {} <<<<<\n{}",
//             Color::Green.paint("Orig"),
//             input,
//             Color::Green.paint("Code"),
//             src_without_helpers
//         );

//         exec_with_node_test_runner(test_name, &src)
//     })
// }

// fn calc_hash(s: &str) -> String {
//     let mut hasher = Sha1::new();
//     hasher.update(s.as_bytes());
//     let sum = hasher.finalize();

//     hex::encode(sum)
// }

// fn exec_with_node_test_runner(test_name: &str, src: &str) -> Result<(), ()> {
//     let root = Path::new(env!("CARGO_MANIFEST_DIR"))
//         .join("target")
//         .join("testing")
//         .join(test_name);

//     create_dir_all(&root).expect("failed to create parent directory for temp directory");

//     let hash = calc_hash(src);
//     let success_cache = root.join(format!("{}.success", hash));

//     if env::var("SWC_CACHE_TEST").unwrap_or_default() == "1" {
//         println!("Trying cache as `SWC_CACHE_TEST` is `1`");

//         if success_cache.exists() {
//             println!("Cache: success");
//             return Ok(());
//         }
//     }

//     let tmp_dir = tempdir_in(&root).expect("failed to create a temp directory");
//     create_dir_all(&tmp_dir).unwrap();

//     let path = tmp_dir.path().join(format!("{}.test.js", test_name));

//     let mut tmp = OpenOptions::new()
//         .create(true)
//         .write(true)
//         .open(&path)
//         .expect("failed to create a temp file");
//     write!(tmp, "{}", src).expect("failed to write to temp file");
//     tmp.flush().unwrap();

//     let test_runner_path = find_executable("mocha").expect("failed to find `mocha` from path");

//     let mut base_cmd = if cfg!(target_os = "windows") {
//         let mut c = Command::new("cmd");
//         c.arg("/C").arg(&test_runner_path);
//         c
//     } else {
//         Command::new(&test_runner_path)
//     };

//     let output = base_cmd
//         .arg(&format!("{}", path.display()))
//         .arg("--color")
//         .current_dir(root)
//         .output()
//         .expect("failed to run mocha");

//     println!(">>>>> {} <<<<<", Color::Red.paint("Stdout"));
//     println!("{}", String::from_utf8_lossy(&output.stdout));
//     println!(">>>>> {} <<<<<", Color::Red.paint("Stderr"));
//     println!("{}", String::from_utf8_lossy(&output.stderr));

//     if output.status.success() {
//         fs::write(&success_cache, "").unwrap();
//         return Ok(());
//     }
//     let dir_name = path.display().to_string();
//     ::std::mem::forget(tmp_dir);
//     panic!("Execution failed: {dir_name}")
// }

// fn stdout_of(code: &str) -> Result<String, Error> {
//     exec_node_js(
//         code,
//         JsExecOptions {
//             cache: true,
//             module: false,
//             ..Default::default()
//         },
//     )
// }

// /// Test transformation.
// #[macro_export]
// macro_rules! test_exec {
//     (ignore, $syntax:expr, $tr:expr, $test_name:ident, $input:expr) => {
//         #[test]
//         #[ignore]
//         fn $test_name() {
//             $crate::exec_tr(stringify!($test_name), $syntax, $tr, $input)
//         }
//     };

//     ($syntax:expr, $tr:expr, $test_name:ident, $input:expr) => {
//         #[test]
//         fn $test_name() {
//             if ::std::env::var("EXEC").unwrap_or(String::from("")) == "0" {
//                 return;
//             }

//             $crate::exec_tr(stringify!($test_name), $syntax, $tr, $input)
//         }
//     };
// }

// /// Test transformation by invoking it using `node`. The code must print
// /// something to stdout.
// #[macro_export]
// macro_rules! compare_stdout {
//     ($syntax:expr, $tr:expr, $test_name:ident, $input:expr) => {
//         #[test]
//         fn $test_name() {
//             $crate::compare_stdout($syntax, $tr, $input)
//         }
//     };
// }

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

// /// Converts `foo#1` to `foo__1` so it can be verified by the test.
// pub struct HygieneTester;
// impl Fold for HygieneTester {
//     fn fold_ident(&mut self, ident: Ident) -> Ident {
//         Ident {
//             sym: format!("{}__{}", ident.sym, ident.span.ctxt.as_u32()).into(),
//             ..ident
//         }
//     }

//     fn fold_member_prop(&mut self, p: MemberProp) -> MemberProp {
//         match p {
//             MemberProp::Computed(..) => p.fold_children_with(self),
//             _ => p,
//         }
//     }

//     fn fold_prop_name(&mut self, p: PropName) -> PropName {
//         match p {
//             PropName::Computed(..) => p.fold_children_with(self),
//             _ => p,
//         }
//     }
// }

// pub struct HygieneVisualizer;
// impl Fold for HygieneVisualizer {
//     fn fold_ident(&mut self, ident: Ident) -> Ident {
//         Ident {
//             sym: format!("{}{:?}", ident.sym, ident.span.ctxt()).into(),
//             ..ident
//         }
//     }
// }

// /// Just like babel, walk up the directory tree and find a file named
// /// `options.json`.
// pub fn parse_options<T>(dir: &Path) -> T
// where
//     T: DeserializeOwned,
// {
//     let mut s = String::from("{}");

//     fn check(dir: &Path) -> Option<String> {
//         let file = dir.join("options.json");
//         if let Ok(v) = read_to_string(&file) {
//             eprintln!("Using options.json at {}", file.display());
//             eprintln!("----- {} -----\n{}", Color::Green.paint("Options"), v);

//             return Some(v);
//         }

//         dir.parent().and_then(check)
//     }

//     if let Some(content) = check(dir) {
//         s = content;
//     }

//     serde_json::from_str(&s)
//         .unwrap_or_else(|err| panic!("failed to deserialize options.json: {}\n{}", err, s))
// }

// /// Config for [test_fixture]. See [test_fixture] for documentation.
// #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct FixtureTestConfig {
//     /// If true, source map will be printed to the `.map` file.
//     ///
//     /// Defaults to false.
//     pub sourcemap: bool,

//     /// If true, diagnostics written to [HANDLER] will be printed as a fixture,
//     /// with `.stderr` extension.
//     ///
//     /// If false, test will fail if diagnostics are emitted.
//     ///
//     /// Defaults to false.
//     pub allow_error: bool,
// }

// /// You can do `UPDATE=1 cargo test` to update fixtures.
// pub fn test_fixture<P>(
//     syntax: Syntax,
//     tr: &dyn Fn(&mut Tester) -> P,
//     input: &Path,
//     output: &Path,
//     config: FixtureTestConfig,
// ) where
//     P: Fold,
// {
//     let _logger = testing::init();

//     let expected = read_to_string(output);
//     let _is_really_expected = expected.is_ok();
//     let expected = expected.unwrap_or_default();

//     let expected_src = Tester::run(|tester| {
//         let expected_module = tester.apply_transform(noop(), "expected.js", syntax, &expected)?;

//         let expected_src = tester.print(&expected_module);

//         println!(
//             "----- {} -----\n{}",
//             Color::Green.paint("Expected"),
//             expected_src
//         );

//         Ok(expected_src)
//     });

//     let mut src_map = if config.sourcemap { Some(vec![]) } else { None };

//     let mut sourcemap = None;

//     let (actual_src, stderr) = Tester::run_captured(|tester| {
//         let input_str = read_to_string(input).unwrap();
//         println!("----- {} -----\n{}", Color::Green.paint("Input"), input_str);

//         let tr = tr(tester);

//         println!("----- {} -----", Color::Green.paint("Actual"));

//         let actual =
//             tester.apply_transform(tr, "input.js", syntax, &read_to_string(input).unwrap())?;

//         // match ::std::env::var("PRINT_HYGIENE") {
//         //     Ok(ref s) if s == "1" => {
//         //         let hygiene_src = tester.print(
//         //             &actual.clone().fold_with(&mut HygieneVisualizer),
//         //         );
//         //         println!(
//         //             "----- {} -----\n{}",
//         //             Color::Green.paint("Hygiene"),
//         //             hygiene_src
//         //         );
//         //     }
//         //     _ => {}
//         // }

//         let actual_src = {
//             let module = &actual;
//             let mut buf = vec![];
//             {
//                 let mut emitter = Emitter {
//                     cfg: Default::default(),
//                     cm: tester.cm.clone(),
//                     wr: Box::new(codegen::text_writer::JsWriter::new(
//                         tester.cm.clone(),
//                         "\n",
//                         &mut buf,
//                         src_map.as_mut(),
//                     )),
//                 };

//                 // println!("Emitting: {:?}", module);
//                 emitter.emit_module(module).unwrap();
//             }

//             if let Some(src_map) = &mut src_map {
//                 sourcemap = Some(tester.cm.build_source_map_with_config(
//                     src_map,
//                     None,
//                     SourceMapConfigImpl,
//                 ));
//             }

//             let s = String::from_utf8_lossy(&buf);
//             s.to_string()
//         };

//         Ok(actual_src)
//     });

//     if config.allow_error {
//         stderr
//             .compare_to_file(output.with_extension("stderr"))
//             .unwrap();
//     } else if !stderr.is_empty() {
//         panic!("stderr: {}", stderr);
//     }

//     if let Some(actual_src) = actual_src {
//         println!("{}", actual_src);

//         if actual_src != expected_src {
//             NormalizedOutput::from(actual_src)
//                 .compare_to_file(output)
//                 .unwrap();
//         }
//     }

//     if let Some(sourcemap) = sourcemap {
//         let map = {
//             let mut buf = vec![];
//             sourcemap.to_writer(&mut buf).unwrap();
//             String::from_utf8(buf).unwrap()
//         };
//         NormalizedOutput::from(map)
//             .compare_to_file(output.with_extension("map"))
//             .unwrap();
//     }
// }

// struct SourceMapConfigImpl;

// impl SourceMapGenConfig for SourceMapConfigImpl {
//     fn file_name_to_source(&self, f: &global_common::FileName) -> String {
//         f.to_string()
//     }

//     fn inline_sources_content(&self, _: &global_common::FileName) -> bool {
//         true
//     }
// }
