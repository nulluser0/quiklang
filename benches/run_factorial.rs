use std::{cell::RefCell, rc::Rc};

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use quiklang::{
    backend_interpreter::environment::Environment, frontend::type_environment::TypeEnvironment, utils::run::run,
};

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

    c.bench_function("run_factorial", |b| {
        b.iter(|| {
            let env = Rc::new(RefCell::new(Environment::new_with_parent(Rc::new(
                RefCell::new(Environment::default()),
            ))));
            let root_type_env = Rc::new(RefCell::new(TypeEnvironment::default()));
            let type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
                root_type_env.clone(),
            )));
            run(
                black_box(source_code.clone()),
                &env,
                &type_env,
                &root_type_env,
            );
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(200).warm_up_time(std::time::Duration::from_secs(2));
    targets = criterion_benchmark
}
criterion_main!(benches);
