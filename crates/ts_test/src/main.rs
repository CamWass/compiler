use ts::parser::createSourceFile;

fn main() {
    let res = createSourceFile(
        "filename".into(),
        "class A { ".into(),
        ts::types::ScriptTarget::ESNext,
        false,
        Some(ts::types::ScriptKind::TS),
    );

    dbg!(res);
}
