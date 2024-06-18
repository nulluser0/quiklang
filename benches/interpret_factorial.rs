use std::{cell::RefCell, process::exit, rc::Rc};

use criterion::{criterion_group, criterion_main, Criterion};
use quiklang::{
    backend::{environment::Environment, interpreter::evaluate},
    frontend::parser,
};

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

    let mut parser = parser::Parser::new();
    let program = match parser.produce_ast(source_code) {
        Ok(program) => program,
        Err(e) => {
            println!("Error: {:?}", e);
            exit(1);
        }
    };

    c.bench_function("interpret_factorial", |b| {
        b.iter(|| {
            let env = Rc::new(RefCell::new(Environment::new_with_parent(Rc::new(
                RefCell::new(Environment::default()),
            ))));
            let root_env = env
                .borrow()
                .get_parent()
                .expect("env must have parent env.");
            for stmt in &program.statements {
                let _ = evaluate(stmt.clone(), &env, &root_env);
            }
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(200).warm_up_time(std::time::Duration::from_secs(2));
    targets = criterion_benchmark
}
criterion_main!(benches);
