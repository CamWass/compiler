use std::{env, fs::read_to_string, rc::Rc};

use ts::binder::Binder;
use ts::parser::createSourceFile;
use ts::types::CompilerOptions;

fn main() {
    let args: Vec<String> = env::args().collect();
    let entry_file: Rc<str> = args[1].clone().into();

    let source_text = read_to_string(entry_file.as_ref()).unwrap();

    let mut store = Default::default();

    let source_file = createSourceFile(
        entry_file,
        source_text.into(),
        ts::types::ScriptTarget::ESNext,
        false,
        None,
        &mut store,
    );

    dbg!(&source_file.parseDiagnostics);

    Binder::bind_source_files(
        &[Rc::new(source_file)],
        CompilerOptions::default(),
        &mut store,
    );
}
