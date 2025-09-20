extern crate custom_alloc;

use anyhow::{bail, Context, Error, Result};
use codegen::{self, Emitter, JsWriter};
use compiler::Compiler;
use config::{load_config, Config};
use global_common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    FileName, SourceMap,
};
use parser::{Parser, Syntax};
use rustc_hash::FxHashSet;
use std::time::Instant;
use std::{env, path::Path};
use swc_common::Spanned;

mod config;

fn create_program(
    filename: &str,
    config: &Config,
    cm: &SourceMap,
    handler: &Handler,
    program_data: &mut ast::ProgramData,
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

    let fm = cm
        .load_file(Path::new(filename))
        .expect("Failed to load file");

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

fn compile(entry_file: &str, config: Config) -> Result<()> {
    let cm = Lrc::<SourceMap>::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

    let mut program_data = ast::ProgramData::default();

    let program = create_program(entry_file, &config, &cm, &handler, &mut program_data)?;

    let compiler = Compiler::new();

    let start = Instant::now();

    let result = compiler.compile(program, config.passes, &mut program_data);

    let elapsed = start.elapsed();
    println!("\nCompilation took: {elapsed:.2?}",);

    // dbg!(&result);

    // Ok(())
    println!("\n\n\nSuccessfully parsed");

    let src = {
        let mut buf = vec![];
        {
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
        }
        // Invalid utf8 is valid in javascript world.
        String::from_utf8(buf).expect("Invalid utf8 character detected")
    };

    std::fs::write("out.js", src).context("Failed to write file")
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    let entry_file = &args[1];

    let config_file = Path::new("config.json");
    let config = load_config(config_file)?;

    let reduce_mode = matches!(args.get(2).map(|s| s.as_str()), Some("--reduce"));

    if reduce_mode {
        reduce(entry_file, config);
        Ok(())
    } else {
        compile(entry_file, config)
    }
}

fn reduce(entry_file: &str, config: Config) {
    let initial = std::fs::read_to_string(entry_file).unwrap();

    let initial_len = initial.len();

    let mut best_program = swc_parse(initial.clone()).unwrap();

    // let predicate = |ours: &str, theirs: &str| ours.contains("bad");
    let predicate = |ours: &str, theirs: &str| ours.len() > theirs.len();

    let mut swc_output = swc_print(&best_program, true);
    let mut our_output = compile_ours(initial, config.clone()).unwrap();
    {
        if !predicate(&our_output, &swc_output) {
            println!("Original file did not satisfy predicate");
            std::fs::write("reduced_ours.js", our_output).unwrap();
            std::fs::write("reduced_swc.js", swc_output).unwrap();
        }
    }

    // dbg!(swc_output.len(), our_output.len());

    // dbg!(
    //     swc_output.bytes().filter(|b| *b == b';').count(),
    //     our_output.bytes().filter(|b| *b == b';').count()
    // );

    // std::fs::write("reduced_ours.js", our_output).unwrap();
    // std::fs::write("reduced_swc.js", swc_output).unwrap();

    // todo!();

    // match &mut best_program {
    //     swc_ecma_ast::Program::Module(module) => todo!(),
    //     swc_ecma_ast::Program::Script(script) => script.body.remove(0),
    // };

    // let best_string = swc_print(&best_program, false).unwrap();

    // let mut swc_output = swc_print(&best_program, true).unwrap();
    // let mut our_output = compile_ours(best_string, config.clone()).unwrap();

    // dbg!(swc_output.len(), our_output.len());

    // todo!();

    reduce_first(&mut best_program, &config, predicate);

    let best_string = swc_print(&best_program, false);
    let mut best_string_len = best_string.len();

    swc_output = swc_print(&best_program, true);
    our_output = compile_ours(best_string.clone(), config.clone()).unwrap();

    let mut reducer = Reducer::default();

    let mut iter = 0;
    let mut gen = 0;

    'outer: loop {
        println!("Starting generation: {gen}",);
        gen += 1;
        let mut changed = false;
        loop {
            if iter % 10 == 0 {
                println!("Current length: {best_string_len}",);
                std::fs::write("reduced_out.js", &best_string).unwrap();
                std::fs::write("reduced_ours.js", &our_output).unwrap();
                std::fs::write("reduced_swc.js", &swc_output).unwrap();
            }

            reducer.changed = false;
            let mut new_best_program = best_program.clone();
            new_best_program.visit_mut_with(&mut reducer);
            if !reducer.changed {
                break;
            }
            changed = true;
            let new_best_string = swc_print(&new_best_program, false);
            let new_best_string_len = new_best_string.len();

            let new_swc_output = swc_print(&new_best_program, true);
            let new_our_output = compile_ours(new_best_string, config.clone()).unwrap();

            if iter == 100 {
                println!("Reached max iterations; aborting");
                best_program = new_best_program;
                swc_output = new_swc_output;
                our_output = new_our_output;
                break 'outer;
            }

            if predicate(&new_our_output, &new_swc_output) {
                best_program = new_best_program;
                swc_output = new_swc_output;
                our_output = new_our_output;
                best_string_len = new_best_string_len;
            }

            iter += 1;
        }

        if !changed {
            break;
        }
    }

    // TODO: new_best_string?
    let reduced = swc_print(&best_program, false);

    if reduced.len() == initial_len {
        println!("Could not reduce input");
    } else {
        println!("Reduced length: {}", reduced.len());
    }

    std::fs::write("reduced_out.js", reduced).unwrap();
    std::fs::write("reduced_ours.js", our_output).unwrap();
    std::fs::write("reduced_swc.js", swc_output).unwrap();
}

fn reduce_first(
    program: &mut swc_ecma_ast::Program,
    config: &Config,
    predicate: impl Fn(&str, &str) -> bool,
) {
    let swc_ecma_ast::Program::Script(script) = program else {
        unimplemented!();
    };

    let mut removed = Vec::new();

    // try removing as many chunks of size sz from array as possible
    // once we're done, switch to size sz/2; if size drops to zero,
    // recursively invoke minimise on the individual elements
    // of the array
    let mut sz = script.body.len() >> 1;
    while sz > 0 {
        println!("chunk size {sz}",);
        let nchunks = script.body.len() / sz;
        let mut chunk_idx = nchunks.checked_sub(1);
        while let Some(i) = chunk_idx {
            // try removing chunk i
            println!("chunk #{i}");
            let lo = i * sz;
            let hi = if i == nchunks - 1 {
                script.body.len()
            } else {
                (i + 1) * sz
            };

            // avoid creating empty array if nonempty is set
            if lo > 0 || hi < script.body.len() {
                removed.extend(script.body.drain(lo..hi));

                let best_string = swc_print_script(script, false);

                let swc_output = swc_print_script(script, true);
                let our_output = compile_ours(best_string, config.clone()).unwrap();

                if !predicate(&our_output, &swc_output) {
                    // didn't work, need to put it back
                    script.body.splice(lo..lo, removed.drain(..));
                }
            }
            chunk_idx = i.checked_sub(1);
        }
        sz >>= 1;
    }
}

fn compile_ours(input: String, config: Config) -> Result<String> {
    let cm = Lrc::<SourceMap>::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

    let mut program_data = ast::ProgramData::default();

    let fm = cm.new_source_file(FileName::Anon, input);

    let program = {
        let mut parser = Parser::new(Default::default(), &fm, &mut program_data);

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

        program
    };

    // let compiler = Compiler::new();

    // let result = compiler.compile(program, config.passes, &mut program_data);

    let mut buf = Vec::new();
    {
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
            .emit_program(&program)
            .context("Failed to emit module")?;
    }
    // Invalid utf8 is valid in javascript world.
    Ok(String::from_utf8(buf).expect("Invalid utf8 character detected"))
}

fn swc_parse(input: String) -> Result<swc_ecma_ast::Program> {
    use swc_common::{
        errors::{ColorConfig, Handler},
        sync::Lrc,
        FileName, SourceMap,
    };
    use swc_ecma_parser::{lexer::Lexer, Capturing, Parser, StringInput};

    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));

    let fm = cm.new_source_file(FileName::Anon.into(), input);

    let lexer = Lexer::new(
        Default::default(),
        Default::default(),
        StringInput::from(&*fm),
        None,
    );

    let capturing = Capturing::new(lexer);

    let mut parser = Parser::new_from(capturing);

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

    Ok(program)
}

fn swc_print(program: &swc_ecma_ast::Program, minify: bool) -> String {
    use swc_ecma_codegen::{text_writer::JsWriter, Config, Emitter};

    let code = {
        let mut buf = Vec::new();

        {
            let cm: Lrc<swc_common::SourceMap> = Default::default();
            let mut emitter = Emitter {
                cfg: Config::default().with_minify(minify),
                wr: JsWriter::new(cm.clone(), "\n", &mut buf, None),
                cm,
                comments: None,
            };

            emitter.emit_program(program).unwrap();
        }

        String::from_utf8_lossy(&buf).to_string()
    };

    code

    // use swc_common::Mark;
    // use swc_ecma_minifier::optimize;
    // use swc_ecma_minifier::option::{CompressOptions, ExtraOptions, MinifyOptions};
    // use swc_ecma_transforms_base::resolver;
    // use swc_ecma_visit::FoldWith;

    // if minify {
    //     let cm: Lrc<swc_common::SourceMap> = Default::default();
    //     let minify_options = MinifyOptions {
    //         rename: false,
    //         compress: Some(CompressOptions {
    //             arguments: false,
    //             arrows: false,
    //             bools: false,
    //             bools_as_ints: false,
    //             collapse_vars: false,
    //             comparisons: false,
    //             computed_props: false,
    //             conditionals: false,
    //             dead_code: false,
    //             directives: false,
    //             drop_console: false,
    //             drop_debugger: false,
    //             ecma: swc_ecma_ast::EsVersion::EsNext,
    //             evaluate: false,
    //             expr: false,
    //             global_defs: Default::default(),
    //             hoist_fns: false,
    //             hoist_props: false,
    //             hoist_vars: false,
    //             ie8: false,
    //             if_return: false,
    //             inline: 0,
    //             join_vars: false,
    //             keep_classnames: false,
    //             keep_fargs: false,
    //             keep_fnames: false,
    //             keep_infinity: false,
    //             loops: false,
    //             module: false,
    //             negate_iife: false,
    //             passes: 1,
    //             props: false,
    //             pure_getters: Default::default(),
    //             pure_funcs: Default::default(),
    //             reduce_fns: false,
    //             reduce_vars: false,
    //             sequences: 0,
    //             side_effects: false,
    //             switches: false,
    //             top_retain: Default::default(),
    //             top_level: Default::default(),
    //             typeofs: false,
    //             unsafe_passes: false,
    //             unsafe_arrows: false,
    //             unsafe_comps: false,
    //             unsafe_function: false,
    //             unsafe_math: false,
    //             unsafe_symbols: false,
    //             unsafe_methods: false,
    //             unsafe_proto: false,
    //             unsafe_regexp: false,
    //             unsafe_undefined: false,
    //             unused: false,
    //             const_to_let: false,
    //             pristine_globals: false,
    //         }),
    //         mangle: None,
    //         wrap: false,
    //         enclose: false,
    //     };

    //     swc_common::GLOBALS.set(&swc_common::Globals::new(), || {
    //         let unresolved_mark = Mark::new();
    //         let top_level_mark = Mark::new();

    //         let program =
    //             program
    //                 .clone()
    //                 .fold_with(&mut resolver(unresolved_mark, top_level_mark, false));

    //         let program = optimize(
    //             program,
    //             cm.clone(),
    //             None,
    //             None,
    //             &minify_options,
    //             &ExtraOptions {
    //                 unresolved_mark,
    //                 top_level_mark,
    //                 mangle_name_cache: None,
    //             },
    //         );
    //         let code = {
    //             let mut buf = Vec::new();

    //             {
    //                 let mut emitter = Emitter {
    //                     cfg: Config::default().with_minify(true),
    //                     wr: JsWriter::new(cm.clone(), "\n", &mut buf, None),
    //                     cm,
    //                     comments: None,
    //                 };

    //                 emitter.emit_program(&program).unwrap();
    //             }

    //             String::from_utf8_lossy(&buf).to_string()
    //         };

    //         Ok(code)
    //     })
    // } else {
    //     let code = {
    //         let mut buf = Vec::new();

    //         {
    //             let cm: Lrc<swc_common::SourceMap> = Default::default();
    //             let mut emitter = Emitter {
    //                 cfg: Config::default().with_minify(true),
    //                 wr: JsWriter::new(cm.clone(), "\n", &mut buf, None),
    //                 cm,
    //                 comments: None,
    //             };

    //             emitter.emit_program(&program).unwrap();
    //         }

    //         String::from_utf8_lossy(&buf).to_string()
    //     };

    //     Ok(code)
    // }
}

fn swc_print_script(program: &swc_ecma_ast::Script, minify: bool) -> String {
    use swc_ecma_codegen::{text_writer::JsWriter, Config, Emitter};

    let code = {
        let mut buf = Vec::new();

        {
            let cm: Lrc<swc_common::SourceMap> = Default::default();
            let mut emitter = Emitter {
                cfg: Config::default().with_minify(minify),
                wr: JsWriter::new(cm.clone(), "\n", &mut buf, None),
                cm,
                comments: None,
            };

            emitter.emit_script(program).unwrap();
        }

        String::from_utf8_lossy(&buf).to_string()
    };

    code
}

use swc_ecma_visit::{noop_visit_mut_type, VisitMut, VisitMutWith};

#[derive(Default, Debug)]
struct Reducer {
    skipped: FxHashSet<swc_common::Span>,
    finished: FxHashSet<swc_common::Span>,
    changed: bool,
}

impl VisitMut for Reducer {
    noop_visit_mut_type!();

    fn visit_mut_stmt(&mut self, node: &mut swc_ecma_ast::Stmt) {
        if !self.changed {
            node.visit_mut_children_with(self);
        }
    }
    fn visit_mut_expr(&mut self, node: &mut swc_ecma_ast::Expr) {
        if !self.changed {
            node.visit_mut_children_with(self);
        }
    }

    fn visit_mut_block_stmt(&mut self, node: &mut swc_ecma_ast::BlockStmt) {
        if self.finished.contains(&node.span) {
            return;
        }

        if let Some(next) = node
            .stmts
            .iter()
            .position(|s| !self.skipped.contains(&s.span()))
        {
            let removed = node.stmts.remove(next);
            self.skipped.insert(removed.span());
            self.changed = true;
        } else {
            for stmt in &mut node.stmts {
                stmt.visit_mut_with(self);
                if self.changed {
                    return;
                }
            }
            self.finished.insert(node.span);
        }
    }

    fn visit_mut_switch_case(&mut self, node: &mut swc_ecma_ast::SwitchCase) {
        if self.finished.contains(&node.span) {
            return;
        }

        if let Some(next) = node
            .cons
            .iter()
            .position(|s| !self.skipped.contains(&s.span()))
        {
            let removed = node.cons.remove(next);
            self.skipped.insert(removed.span());
            self.changed = true;
        } else {
            for stmt in &mut node.cons {
                stmt.visit_mut_with(self);
                if self.changed {
                    return;
                }
            }
            self.finished.insert(node.span);
        }
    }

    // fn visit_mut_script(&mut self, node: &mut swc_ecma_ast::Script) {
    //     if self.finished.contains(&node.span) {
    //         return;
    //     }
    //     dbg!(node.body.len());

    //     if let Some(next) = node
    //         .body
    //         .iter()
    //         .position(|s| !self.skipped.contains(&s.span()))
    //     {
    //         // TODO: check predicate here
    //         let removed = node.body.remove(next);
    //         self.skipped.insert(removed.span());
    //         self.changed = true;
    //     } else {
    //         for stmt in &mut node.body {
    //             stmt.visit_mut_with(self);
    //             if self.changed {
    //                 return;
    //             }
    //         }
    //         self.finished.insert(node.span);
    //     }
    // }
}
