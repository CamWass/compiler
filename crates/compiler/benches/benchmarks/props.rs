#[deny(unused_imports)]
use std::time::Duration;

use compiler::resolver::resolver;
use criterion::{black_box, criterion_group, Criterion, Throughput};
use ecma_visit::VisitMutWith;
use global_common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    SourceMap, SyntaxContext,
};
use global_common::{FileName, FilePathMapping};
use global_common::{Globals, Mark, GLOBALS};
use parser::{Parser, Syntax};
use std::cell::RefCell;
use std::rc::Rc;

struct Bench(&'static str, &'static str);

fn bench(c: &mut Criterion) {
    let benches: [(Bench, u64, usize); 2] = [
        (
            Bench(
                "small_typescript",
                include_str!("../files/small_typescript.js"),
            ),
            20,
            100,
        ),
        (
            Bench("typescript", include_str!("../files/typescript.js")),
            20,
            10,
        ),
    ];

    let mut group = c.benchmark_group("props");
    for (Bench(id, src), time, samples) in benches.iter() {
        group
            .measurement_time(Duration::from_secs(*time))
            .sample_size(*samples);
        group.throughput(Throughput::Bytes(src.len() as u64));

        let cm = Lrc::new(SourceMap::new(FilePathMapping::empty()));
        let fm = cm.new_source_file(FileName::Anon, src.to_string());

        GLOBALS.set(&Globals::new(), || {
            let unresolved_mark = Mark::new();
            let top_level_mark = Mark::new();

            let node_id_gen = Rc::new(RefCell::new(ast::NodeIdGen::default()));

            let handler =
                Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

            let mut program = {
                let mut p = Parser::new(
                    Syntax::Typescript(Default::default()),
                    &fm,
                    node_id_gen.clone(),
                );
                let res = p
                    .parse_program()
                    .map_err(|e| e.into_diagnostic(&handler).emit());

                for e in p.take_errors() {
                    e.into_diagnostic(&handler).emit()
                }

                res.unwrap()
            };

            let mut node_id_gen = Rc::try_unwrap(node_id_gen).unwrap().into_inner();

            program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

            compiler::normalize_properties::normalize_properties(&mut program, &mut node_id_gen);

            let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

            group.bench_with_input(*id, &program, |b, program| {
                b.iter(|| {
                    let r = compiler::optimize_properties2::analyse(program, unresolved_ctxt);
                    black_box(r);
                })
            });

            program
        });
    }
    group.finish();
}

criterion_group!(benches, bench);
