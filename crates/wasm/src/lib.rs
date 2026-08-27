use wasm_bindgen::prelude::*;

use anyhow::{Context, Error, Result, bail};
use codegen::{self, Emitter, JsWriter};
use common::{
    FileName, SourceMap,
    errors::{EmitterWriter, Handler, HandlerFlags},
};
use compiler::{Compiler, print_control_flow_graph};
use parser::{Parser, Syntax};
use std::{
    io::{self, Write},
    rc::Rc,
    sync::{Arc, RwLock},
};

use compiler::PassConfig;
use parser::{EsConfig, TsConfig};
use serde::{Deserialize, Serialize};
use serde_json::error::Category;

#[derive(Debug, Deserialize, Clone)]
#[serde(deny_unknown_fields)]
pub struct Config {
    #[serde(default)]
    pub pretty_print: bool,
    #[serde(default)]
    pub ecmascript: EsConfig,
    #[serde(default)]
    pub typescript: TsConfig,
    #[serde(default)]
    pub passes: PassConfig,
}

fn create_program(
    filename: &str,
    src: &str,
    config: &Config,
    cm: &SourceMap,
    handler: &Handler,
    program_data: &mut ast::ParserProgramData,
) -> Result<ast::Program> {
    let syntax = if filename.ends_with(".js") {
        Syntax::Es(config.ecmascript)
    } else if filename.ends_with(".ts") {
        let mut ts_config = config.typescript;
        ts_config.dts = filename.ends_with(".d.ts");
        Syntax::Typescript(ts_config)
    } else {
        panic!()
    };

    let fm = cm.new_source_file(FileName::Real(filename.into()), src.into());

    let mut parser = Parser::new(syntax, &fm, program_data);

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

    Ok(program)
}

#[derive(Clone, Default)]
pub(crate) struct BufferedError(Arc<RwLock<Vec<u8>>>);

impl Write for BufferedError {
    fn write(&mut self, d: &[u8]) -> io::Result<usize> {
        self.0.write().unwrap().write(d)
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
struct CompileOutput {
    output: String,
    output_ast: String,
    input_ast: String,
    output_cfg: String,
    input_cfg: String,
}

fn compile(
    entry_file: &str,
    src: &str,
    config: &str,
    error_buffer: &BufferedError,
) -> Result<CompileOutput> {
    let config = load_config(config)?;

    let cm = Rc::<SourceMap>::default();

    let e = EmitterWriter::new(
        Box::new(error_buffer.clone()),
        Some(cm.clone()),
        false,
        true,
    );

    let handler = Handler::with_emitter_and_flags(
        Box::new(e),
        HandlerFlags {
            treat_err_as_bug: false,
            can_emit_warnings: true,
            ..Default::default()
        },
    );

    let mut program_data = ast::ParserProgramData::default();

    let program = create_program(entry_file, src, &config, &cm, &handler, &mut program_data)?;

    let input_ast_string = serde_json::to_string_pretty(&program)?;

    let mut program_data = program_data.into_transformer_program_data();

    let input_cfg = print_control_flow_graph(&program, &program_data);

    let compiler = Compiler::new();

    let result = compiler.compile(program, config.passes, &mut program_data);

    let output_cfg = print_control_flow_graph(&result, &program_data);

    let program_data = program_data.into_codegen_program_data();

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

    let output_ast_string = serde_json::to_string_pretty(&result)?;

    Ok(CompileOutput {
        output: buf,
        output_ast: output_ast_string,
        input_ast: input_ast_string,
        output_cfg: output_cfg.unwrap_or_default(),
        input_cfg: input_cfg.unwrap_or_default(),
    })
}

pub fn load_config(content: &str) -> Result<Config> {
    fn convert_json_err(e: serde_json::Error) -> Error {
        let line = e.line();
        let column = e.column();

        let msg = match e.classify() {
            Category::Io => "io error",
            Category::Syntax => "syntax error",
            Category::Data => "unmatched data",
            Category::Eof => "unexpected eof",
        };
        Error::new(e).context(format!(
            "Failed to deserialize config (json) file: {msg}: {line}:{column}",
        ))
    }

    serde_json::from_str::<Config>(&content).map_err(convert_json_err)
}

#[wasm_bindgen]
pub fn process(input: &str, config: &str) -> Result<JsValue, JsError> {
    console_error_panic_hook::set_once();

    let error_buffer: BufferedError = Default::default();

    let res = compile("input_file.js", input, config, &error_buffer).map_err(|e| {
        let buffered_errors = String::from_utf8_lossy(&error_buffer.0.read().unwrap()).into_owned();
        JsError::new(&format!("{buffered_errors}\n{e}"))
    })?;

    serde_wasm_bindgen::to_value(&res)
        .map_err(|e| JsError::new(&format!("Serialization failed: {e}")))
}
