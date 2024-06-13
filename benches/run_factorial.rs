use std::{cell::RefCell, rc::Rc};

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use quiklang::{backend::environment::Environment, utils::run::run};

fn criterion_benchmark(c: &mut Criterion) {
    let source_code = r#"
        fn factorial(n) {
            if n <= 1 {
                1
            } else {
                n * factorial(n - 1)
            }
        }
        
        factorial(5)
    "#
    .to_string();

    c.bench_function("run_factorial", |b| {
        b.iter(|| {
            let env = Rc::new(RefCell::new(Environment::new_with_parent(Rc::new(
                RefCell::new(Environment::new()),
            ))));
            run(black_box(source_code.clone()), &env);
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(200).warm_up_time(std::time::Duration::from_secs(2));
    targets = criterion_benchmark
}
criterion_main!(benches);
