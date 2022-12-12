use std::{env, fs::read_to_string, rc::Rc};

use ts::parser::createSourceFile;

fn main() {
    let args: Vec<String> = env::args().collect();
    let entry_file: Rc<str> = args[1].clone().into();

    let source_text = read_to_string(entry_file.as_ref()).unwrap();

    let res = createSourceFile(
        entry_file,
        source_text.into(),
        ts::types::ScriptTarget::ESNext,
        false,
        None,
    );

    dbg!(res.parseDiagnostics);
}
