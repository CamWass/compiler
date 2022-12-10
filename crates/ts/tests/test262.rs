#![feature(test)]

extern crate test;

use std::{
    env,
    fs::{read_dir, File},
    io::{self, Read},
    path::Path,
    rc::Rc,
};
use test::{
    test_main, DynTestFn, Options, ShouldPanic::No, TestDesc, TestDescAndFn, TestName, TestType,
};
// use testing::{NormalizedOutput, StdErr};

use ts::{parser::createSourceFile, types::*};

const IGNORED_PASS_TESTS: &[&str] = &[];

fn add_test<F: FnOnce() + Send + 'static>(
    tests: &mut Vec<TestDescAndFn>,
    name: String,
    ignore: bool,
    f: F,
) {
    tests.push(TestDescAndFn {
        desc: TestDesc {
            test_type: TestType::UnitTest,
            name: TestName::DynTestName(name),
            ignore,
            should_panic: No,
            compile_fail: Default::default(),
            no_run: Default::default(),
            ignore_message: Default::default(),
        },
        testfn: DynTestFn(Box::new(f)),
    });
}

// fn error_tests(tests: &mut Vec<TestDescAndFn>) -> Result<(), io::Error> {
//     const IGNORED_ERROR_TESTS: &[&str] = &[];

//     let root = {
//         let mut root = Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf();
//         root.push("tests");
//         root.push("test262-parser");
//         root
//     };

//     eprintln!("Loading tests from {}", root.display());

//     const TYPES: &[&str] = &[
//         "fail", /* TODO
//                 * "early" */
//     ];

//     for err_type in TYPES {
//         let dir = root.join(err_type);
//         let error_reference_dir = {
//             let mut root = Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf();
//             root.push("tests");
//             root.push("test262-error-references");
//             root.push(err_type);
//             root
//         };

//         for entry in read_dir(&dir)? {
//             let entry = entry?;
//             let file_name = entry
//                 .path()
//                 .strip_prefix(&dir)
//                 .expect("failed to strip prefix")
//                 .to_str()
//                 .unwrap()
//                 .to_string();

//             let input = {
//                 let mut buf = String::new();
//                 File::open(entry.path())?.read_to_string(&mut buf)?;
//                 buf
//             };

//             let ignore = IGNORED_ERROR_TESTS.contains(&&*file_name);

//             let module = file_name.contains("module");

//             let dir = dir.clone();
//             let error_reference_dir = error_reference_dir.clone();
//             let name = format!("test262::error_reporting::{}::{}", err_type, file_name);
//             add_test(tests, name, ignore, move || {
//                 eprintln!(
//                     "\n\n========== Running error reporting test {}\nSource:\n{}\n",
//                     file_name, input
//                 );

//                 let path = dir.join(&file_name);
//                 // Parse source
//                 let err = if module {
//                     parse_module(&path).expect_err("should fail, but parsed as")
//                 } else {
//                     parse_script(&path).expect_err("should fail, but parsed as")
//                 };

//                 // if err
//                 //     .compare_to_file(format!(
//                 //         "{}.stderr",
//                 //         error_reference_dir.join(file_name).display()
//                 //     ))
//                 //     .is_err()
//                 // {
//                 //     panic!()
//                 // }
//             });
//         }
//     }

//     Ok(())
// }

fn identity_tests(tests: &mut Vec<TestDescAndFn>) -> Result<(), io::Error> {
    let root = {
        let mut root = Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf();
        root.push("..");
        root.push("parser");
        root.push("tests");
        root.push("test262-parser");
        root
    };

    eprintln!("Loading tests from {}", root.display());

    let pass_dir = root.join("pass");

    let files = read_dir(&pass_dir)?;

    for entry in files {
        let entry = entry?;
        let file_name = entry
            .path()
            .strip_prefix(&pass_dir)
            .expect("failed to strip prefix")
            .to_str()
            .unwrap()
            .to_string();

        let input = {
            let mut buf = String::new();
            File::open(entry.path())?.read_to_string(&mut buf)?;
            buf
        };
        let explicit = {
            let mut buf = String::new();
            File::open(root.join("pass-explicit").join(&file_name))?.read_to_string(&mut buf)?;
            buf
        };

        let ignore = IGNORED_PASS_TESTS.contains(&&*file_name);

        let module = file_name.contains("module");

        let name = format!("test262::identity::{}", file_name);
        add_test(tests, name, ignore, move || {
            eprintln!(
                "\n\n\n========== Running test {}\nSource:\n{}\nExplicit:\n{}",
                file_name, input, explicit
            );

            if module {
                let p = |is_explicit| {
                    let src = if is_explicit { &explicit } else { &input };
                    parse_script(&file_name, src.clone().into()).unwrap()
                };
                p(false);
                p(true);
                // let src = p(false).statements;
                // let expected = p(true).statements;
                // assert_eq!(src, expected);
            } else {
                let p = |is_explicit| {
                    let src = if is_explicit { &explicit } else { &input };
                    parse_script(&file_name, src.clone().into()).unwrap()
                };
                p(false);
                p(true);
                // let src = p(false).statements;
                // let expected = p(true).statements;
                // assert_eq!(src, expected);
            }
        });
    }

    Ok(())
}

fn parse_script(file_name: &str, source: Rc<str>) -> Result<SourceFile, ()> {
    let res = createSourceFile(
        file_name.into(),
        source,
        ScriptTarget::ESNext,
        false,
        Some(ScriptKind::JS),
    );

    if res.parseDiagnostics.is_empty() {
        Ok(res)
    } else {
        Err(())
    }
}

#[test]
fn identity() {
    let args: Vec<_> = env::args().collect();
    let mut tests = Vec::new();
    identity_tests(&mut tests).unwrap();
    test_main(&args, tests, Some(Options::new()));
}

// #[test]
// fn error() {
//     let args: Vec<_> = env::args().collect();
//     let mut tests = Vec::new();
//     error_tests(&mut tests).unwrap();
//     test_main(&args, tests, Some(Options::new()));
// }
