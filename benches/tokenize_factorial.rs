use criterion::{black_box, criterion_group, criterion_main, Criterion};
use quiklang::frontend::lexer::tokenize;

fn criterion_benchmark(c: &mut Criterion) {
    let source_code = r#"
        fn factorial(n: integer) -> integer {
            if n <= 1 {
                1
            } else {
                n * factorial(n - 1)
            }
        }

        factorial(5)
    "#
    .to_string();

    c.bench_function("tokenize_factorial", |b| {
        b.iter(|| {
            let tokens = tokenize(black_box(&source_code));
            // Use the tokens to prevent optimization removal
            let total_length: usize = tokens.iter().map(|t| format!("{:?}", t).len()).sum();
            black_box(total_length);
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(200).warm_up_time(std::time::Duration::from_secs(2));
    targets = criterion_benchmark
}
criterion_main!(benches);
