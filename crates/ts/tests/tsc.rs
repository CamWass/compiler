#![feature(test)]

extern crate test;

use std::{env, fs::read_to_string, path::Path};
use test::{
    test_main, DynTestFn, Options, ShouldPanic::No, TestDesc, TestDescAndFn, TestName, TestType,
};

use ts::{parser::createSourceFile, types::*};

use glob::glob;

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

fn tsc_tests(tests: &mut Vec<TestDescAndFn>) -> Result<(), std::io::Error> {
    let base_path = {
        let mut base_path = Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf();
        base_path.push("..");
        base_path.push("compiler");
        base_path.push("tests");
        base_path.push("TypeScript");
        base_path.push("tests");
        base_path.push("cases");
        base_path.push("conformance");
        base_path
    };

    eprintln!("Loading tests from {}", base_path.display());

    let path = format!("{}/**/*.ts", base_path.to_string_lossy());
    let files = glob(&path).unwrap().map(|p| p.unwrap());

    for entry in files {
        let file_name = entry
            .as_path()
            .strip_prefix(&base_path)
            .expect("failed to strip prefix")
            .to_str()
            .unwrap()
            .to_string();

        let source = read_to_string(&entry).unwrap();

        let ignore = source.contains("// @filename:");

        let name = format!("tsc::conformance::{}", file_name);
        add_test(tests, name, ignore, move || {
            eprintln!(
                "\n\n\n========== Running test {}\nSource:\n{}\n",
                file_name, source,
            );

            let res = createSourceFile(
                file_name.into(),
                source.into(),
                ScriptTarget::ESNext,
                false,
                Some(ScriptKind::TS),
                &mut Default::default(),
            );

            eprintln!("{:#?}", res.parseDiagnostics);
            assert!(res.parseDiagnostics.is_empty());
        });
    }

    Ok(())
}

#[test]
fn tsc() {
    let args: Vec<_> = env::args().collect();
    let mut tests = Vec::new();
    tsc_tests(&mut tests).unwrap();
    test_main(&args, tests, Some(Options::new()));
}
