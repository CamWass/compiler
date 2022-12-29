use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};

use std::rc::Rc;

use ts::parser::createSourceFile;

pub fn bench(c: &mut Criterion) {
    let source_text: Rc<str> = include_str!("./typescript.js").into();
    let file_name: Rc<str> = "typescript.js".into();

    let mut group = c.benchmark_group("parse");
    group.sample_size(20);
    group.throughput(Throughput::Bytes(source_text.len() as u64));

    group.bench_with_input(
        file_name.as_ref(),
        &(source_text, file_name.clone()),
        |b, (source_text, file_name)| {
            b.iter(|| {
                let source_file = createSourceFile(
                    file_name.clone(),
                    source_text.clone(),
                    ts::types::ScriptTarget::ESNext,
                    false,
                    None,
                    &mut Default::default(),
                );
                black_box(source_file);
            });
        },
    );

    group.finish();
}

criterion_group!(benches, bench);
criterion_main!(benches);
