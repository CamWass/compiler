use global_common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    SourceMap,
};
use parser::{EsConfig, Parser, Syntax, TsConfig};
use std::{env, path::Path};

fn main() {
    let args: Vec<String> = env::args().collect();

    let filename = &args[1];

    let path = Path::new(filename);

    let syntax = match path.extension().and_then(|s| s.to_str()) {
        Some("js") => Syntax::Es(EsConfig {
            ..Default::default()
        }),
        Some("ts") => Syntax::Typescript(TsConfig {
            ..Default::default()
        }),
        _ => panic!(),
    };

    let cm = Lrc::<SourceMap>::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

    let fm = cm.load_file(path).expect("failed to load file");

    let mut parser = Parser::new(syntax, &fm.src);

    let res = parser.parse_program();

    let mut error = false;

    for e in parser.take_errors() {
        e.into_diagnostic(&handler).emit();
        error = true;
    }

    let p = match res {
        Ok(p) => p,
        Err(e) => {
            e.into_diagnostic(&handler).emit();
            println!("\n\n\nFailed to parsed");
            return;
        }
    };

    if error {
        println!("\n\n\nFailed to parsed");
    } else {
        // println!("{:#?}", p);
        println!("\n\n\nSuccessfully parsed");
        // let src = {
        //     let mut buf = vec![];
        //     {
        //         let mut emitter = Emitter {
        //             cfg: codegen::Config { minify: true },
        //             comments: None,
        //             cm: cm.clone(),
        //             wr: Box::new(codegen::text_writer::JsWriter::new(
        //                 cm.clone(),
        //                 "\n",
        //                 &mut buf,
        //                 None,
        //             )),
        //         };

        //         p.emit_with(&mut emitter)
        //             .expect("failed to emit module");
        //     }
        //     // Invalid utf8 is valid in javascript world.
        //     String::from_utf8(buf).expect("invalid utf8 characeter detected")
        // };

        // fs::write("out.js", src).expect("failed to write file");
    }
}
