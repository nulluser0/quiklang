use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn rust_factorial(n: u64) -> u64 {
    if n <= 1 {
        1
    } else {
        n * rust_factorial(n - 1)
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("rust_factorial", |b| {
        b.iter(|| {
            let result = rust_factorial(black_box(5));
            black_box(result);
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(200).warm_up_time(std::time::Duration::from_secs(2));
    targets = criterion_benchmark
}
criterion_main!(benches);
