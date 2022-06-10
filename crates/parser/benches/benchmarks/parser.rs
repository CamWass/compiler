use criterion::{black_box, criterion_group, Criterion, Throughput};
use global_common::{sync::Lrc, FileName, FilePathMapping, SourceMap};
use parser::Parser;
use std::time::Duration;

struct Bench(&'static str, &'static str);

fn bench(c: &mut Criterion) {
    let benches = [
        Bench("angular", include_str!("../files/angular-1.2.5.js")),
        Bench("backbone", include_str!("../files/backbone-1.1.0.js")),
        Bench("jquery", include_str!("../files/jquery-1.9.1.js")),
        Bench(
            "jquery_mobile",
            include_str!("../files/jquery.mobile-1.4.2.js"),
        ),
        Bench("mootools", include_str!("../files/mootools-1.4.5.js")),
        Bench("underscore", include_str!("../files/underscore-1.5.2.js")),
        Bench("yui", include_str!("../files/yui-3.12.0.js")),
    ];

    let mut group = c.benchmark_group("parser");
    group
        .measurement_time(Duration::from_secs(20))
        .sample_size(200);
    for Bench(id, src) in benches.iter() {
        group.throughput(Throughput::Bytes(src.len() as u64));

        let cm = Lrc::new(SourceMap::new(FilePathMapping::empty()));
        let fm = cm.new_source_file(FileName::Anon, src.to_string());

        group.bench_with_input(*id, &fm, |b, f| {
            b.iter(|| {
                let mut parser = Parser::new(Default::default(), f);
                black_box(parser.parse_module())
            })
        });
    }
    group.finish();
}

criterion_group!(benches, bench);
