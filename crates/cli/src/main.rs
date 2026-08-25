extern crate custom_alloc;

use anyhow::{Context, Error, Result, bail};
use codegen::{self, Emitter, JsWriter};
use common::{
    SourceMap,
    errors::{ColorConfig, Handler},
};
use compiler::Compiler;
use config::{Config, load_config};
use parser::{Parser, Syntax};
use std::time::{Duration, Instant};
use std::{env, path::Path, rc::Rc};

mod config;

fn create_program(
    filename: &str,
    config: &Config,
    cm: &SourceMap,
    handler: &Handler,
    program_data: &mut ast::ParserProgramData,
) -> Result<(ast::Program, Duration)> {
    let syntax = if filename.ends_with(".js") {
        Syntax::Es(config.ecmascript)
    } else if filename.ends_with(".ts") {
        let mut ts_config = config.typescript;
        ts_config.dts = filename.ends_with(".d.ts");
        Syntax::Typescript(ts_config)
    } else {
        panic!("Unknown file type. Input file must end in .ts or .js");
    };

    let fm = cm
        .load_file(Path::new(filename))
        .expect("Failed to load file");

    let mut parser = Parser::new(syntax, &fm, program_data);

    let parse_start = Instant::now();

    let program = parser.parse_program();

    let parse_duration = parse_start.elapsed();

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

    Ok((program, parse_duration))
}

fn compile(entry_file: &str, config: Config, output_file: Option<&str>) -> Result<()> {
    let compile_start = Instant::now();

    let cm = Rc::<SourceMap>::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

    let mut program_data = ast::ParserProgramData::default();

    let (program, parse_duration) =
        create_program(entry_file, &config, &cm, &handler, &mut program_data)?;

    let compiler = Compiler::new();

    let mut program_data = program_data.into_transformer_program_data();

    let transform_start = Instant::now();

    let result = compiler.compile(program, config.passes, &mut program_data);

    let transform_duration = transform_start.elapsed();

    let program_data = program_data.into_codegen_program_data();

    let codegen_start = Instant::now();

    let src = {
        let mut buf = String::new();

        let mut emitter = Emitter::new(
            codegen::Config {
                minify: !config.pretty_print,
                target: ast::EsVersion::EsNext,
            },
            cm.clone(),
            JsWriter::new("\n", &mut buf, None),
            &program_data,
        );

        emitter
            .emit_program(&result)
            .context("Failed to emit module")?;

        buf
    };

    let codegen_duration = codegen_start.elapsed();

    let compile_duration = compile_start.elapsed();

    println!(
        "\nSuccessfully compiled in {compile_duration:.2?} (parse: {parse_duration:.2?}, transform: {transform_duration:.2?}, codegen: {codegen_duration:.2?})"
    );

    let res = std::fs::write(output_file.unwrap_or("out.js"), &src).context("Failed to write file");

    std::mem::forget(cm);
    std::mem::forget(handler);
    std::mem::forget(program_data);
    std::mem::forget(compiler);
    std::mem::forget(src);
    std::mem::forget(result);

    res
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    let entry_file = &args[1];

    let config_file = Path::new("config.json");
    let config = load_config(config_file)?;

    let output_file = args.get(2).map(|s| s.as_str());
    compile(entry_file, config, output_file)
}
