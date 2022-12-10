use ts::parser::createSourceFile;

fn main() {
    let res = createSourceFile(
        "filename".into(),
        r"``;
        `xx\`x`;
        `${ a + 1 }`;
        ` foo ${ b + `baz ${ c }` }`;"
            .into(),
        ts::types::ScriptTarget::ESNext,
        false,
        Some(ts::types::ScriptKind::TS),
    );

    dbg!(res);
}
