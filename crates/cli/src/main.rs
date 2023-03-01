use anyhow::{bail, Context, Error, Result};
use codegen::{self, Emitter, Node};
use compiler::Compiler;
use config::{load_config, Config};
use global_common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    SourceMap,
};
use parser::{Parser, Syntax};
use std::cell::RefCell;
use std::rc::Rc;
use std::{env, path::Path};

mod config;

fn create_program(
    filename: &str,
    config: &Config,
    cm: Lrc<SourceMap>,
    handler: &Handler,
    node_id_gen: Rc<RefCell<ast::NodeIdGen>>,
) -> Result<(String, ast::Program)> {
    let syntax = if filename.ends_with(".js") {
        Syntax::Es(config.ecmascript)
    } else if filename.ends_with(".ts") {
        let mut ts_config = config.typescript;
        ts_config.dts = filename.ends_with(".d.ts");
        Syntax::Typescript(ts_config)
    } else {
        panic!()
    };

    let fm = cm
        .load_file(Path::new(filename))
        .expect("Failed to load file");

    let mut parser = Parser::new(syntax, &fm, node_id_gen);

    let program = parser.parse_program();

    let mut error = false;

    for e in parser.take_errors() {
        e.into_diagnostic(handler).emit();
        error = true;
    }

    let program = program.map_err(|e| {
        e.into_diagnostic(handler).emit();
        Error::msg("Failed to parse")
    })?;

    if error {
        bail!("Failed to parse");
    }

    Ok((filename.into(), program))
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    let entry_file = &args[1];

    let config_file = Path::new("config.json");
    let config = load_config(config_file)?;

    let cm = Lrc::<SourceMap>::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

    let node_id_gen = Rc::new(RefCell::new(ast::NodeIdGen::default()));

    // let lib = create_program(
    //     "lib.es5.d.ts",
    //     &config,
    //     cm.clone(),
    //     &handler,
    //     node_id_gen.clone(),
    // )?;
    let program = create_program(
        entry_file,
        &config,
        cm.clone(),
        &handler,
        node_id_gen.clone(),
    )?;

    let mut node_id_gen = Rc::try_unwrap(node_id_gen).unwrap().into_inner();

    let compiler = Compiler::new();

    let result = compiler.compile(vec![], program, config.passes, &mut node_id_gen);

    // dbg!(result);

    // Ok(())
    println!("\n\n\nSuccessfully parsed");

    let src = {
        let mut buf = vec![];
        {
            let mut emitter = Emitter {
                cfg: codegen::Config {
                    minify: !config.pretty_print,
                },
                comments: None,
                cm: cm.clone(),
                wr: Box::new(codegen::text_writer::JsWriter::new(
                    cm, "\n", &mut buf, None,
                )),
            };

            result
                .emit_with(&mut emitter)
                .context("Failed to emit module")?;
        }
        // Invalid utf8 is valid in javascript world.
        String::from_utf8(buf).expect("Invalid utf8 character detected")
    };

    std::fs::write("out.ts", src).context("Failed to write file")
}
