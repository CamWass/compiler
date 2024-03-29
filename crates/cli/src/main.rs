use anyhow::{bail, Context, Error, Result};
use codegen::{self, Emitter, Node};
use config::load_config;
use global_common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    SourceMap,
};
use parser::{Parser, Syntax};
use std::{env, path::Path};

mod config;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let filename = &args[1];

    let path = Path::new(filename);

    let config_file = Path::new("config.json");

    let config = load_config(config_file)?;

    let syntax = match path.extension().and_then(|s| s.to_str()) {
        Some("js") => Syntax::Es(config.ecmascript),
        Some("ts") => Syntax::Typescript(config.typescript),
        _ => panic!(),
    };

    let cm = Lrc::<SourceMap>::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

    let fm = cm.load_file(path).expect("Failed to load file");

    let mut parser = Parser::new(syntax, &fm.src);

    let program = parser.parse_program();

    let mut error = false;

    for e in parser.take_errors() {
        e.into_diagnostic(&handler).emit();
        error = true;
    }

    let program = program.map_err(|e| {
        e.into_diagnostic(&handler).emit();
        Error::msg("Failed to parse")
    })?;

    if error {
        bail!("Failed to parse");
    }

    println!("\n\n\nSuccessfully parsed");

    let src = {
        let mut buf = vec![];
        {
            let mut emitter = Emitter {
                cfg: codegen::Config { minify: false },
                comments: None,
                cm: cm.clone(),
                wr: Box::new(codegen::text_writer::JsWriter::new(
                    cm.clone(),
                    "\n",
                    &mut buf,
                    None,
                )),
            };

            program
                .emit_with(&mut emitter)
                .context("Failed to emit module")?;
        }
        // Invalid utf8 is valid in javascript world.
        String::from_utf8(buf).expect("Invalid utf8 character detected")
    };

    std::fs::write("out.js", src).context("Failed to write file")
}
