use ast::Program;
use global_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
    SourceMap,
};
use parser::Parser;
use std::path::Path;

fn main() {
    let cm = Lrc::<SourceMap>::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

    let fm = cm
        .load_file(Path::new("foo.js"))
        .expect("failed to load file");

    let mut parser = Parser::new(StringInput::from(&*fm));

    let is_module = true;
    let program = if is_module {
        let m = parser.parse_module();

        for e in parser.take_errors() {
            e.into_diagnostic(&handler).emit();
        }

        m.map_err(|e| {
            e.into_diagnostic(&handler).emit();

            // std::error::Error("failed to parse module")
        })
        .map(Program::Module)
    } else {
        let s = parser.parse_script();

        for e in parser.take_errors() {
            e.into_diagnostic(&handler).emit();
        }

        s.map_err(|e| {
            e.into_diagnostic(&handler).emit();
            // Error::msg("failed to parse module")
        })
        .map(Program::Script)
    };

    println!("{:#?}", program);
}
