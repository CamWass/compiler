use criterion::{criterion_group, criterion_main};

mod benchmarks;

criterion_group!(benches, benchmarks::props::bench);
criterion_main! {
    benches
}
