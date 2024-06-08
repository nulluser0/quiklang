use criterion::{black_box, criterion_group, criterion_main, Criterion};
use quiklang::frontend::lexer::tokenize;

fn criterion_benchmark(c: &mut Criterion) {
    let source_code = r#"
        import async

        println("Hello, World!");

        // Example factorial function
        fn factorial(n: Integer) -> Integer {
            if n <= 1 {
                return 1
            } else {
                return n * factorial(n - 1)
            }
        }

        // Concurrency
        async fn task1() -> String {
            sleep(2000).await
            return "OK1"
        }

        async fn task2() {
            sleep(1000).await;
            return "OK2"
        }

        let mut values: [String, String] = async.spawn!(task1(), task2);
        println("These functions are called, but we can still do stuff here!");
        println("Now lets collect the results of the functions, and print them.");
        values = values > async.collect();
        println("We pipe the values with futures into the async collector using the '>' operator. The async collector will sort them correctly into the right order of execution.");
        println("We can expect 'OK1' then 'OK2' as a result.");
        println(values>expand()>concat(", ")) // Whitespace around '>' is not strictly required.

        println("Let's create our own pipable function now.");

        fn add_1(self: Integer) -> Integer {
            return self + 1
        } 

        let two: Integer = 2;
        two = two > add_1();
        println(two); // Outputs 3

        println("What about error handling?");

        fn ok_if_three(number: Integer) -> Result|(), String| {
            if !(number == 3) {
                return Err("Not three!!!")
            }
            return Ok(())
        }

        println(ok_if_three(2)>unwrap_result); // Results in "Not three!!!"
        println(ok_if_three(3)>unwrap_result)
            > catch(error)
            > println(error);

        // Two forms of error handling here! One is a Rust-like Result<> return form, and the other is catching an error via pipes using catch()!
        // We then pipe the catch into a print statement!

        println("Now to end this with a blast...")

        // We define a panic module here which will be used instead of the default panic module.
        // If on_panic fails somehow, it defaults back to the default panic module.
        process.on_panic(error) {
            import io;
            println("And now we panic here!");
            println("The panic error: " + error);
            process.exit // Exit the code.
        };

        ok_if_three(1);

        println("This area is unreachable!!! How did you get here? Anyways, bye!");
        panic();

        // In the code checker, this would say something like this:
        //      WARNING: This may cause a panic!
        //
        //         ┌─ ./something.quik:63:1
        //         |
        //      63 | ok_if_three(1)
        //         | ^^^^^^^^^^^^^^ This may return an error, which will cause the whole script to panic!
        //         |
        //      ok_if_three() at ./something.quik:46:1 returns Result<(), String>.
        //      
        //      Note: If this is intentional, annotate ./something.quik:63:1 with:
        //         |
        //      -1 | #[intentional_panic]
        //      63 | ok_if_three(1)
        //         |
        "a
    "#.to_string();

    c.bench_function("tokenize_large", |b| {
        b.iter(|| {
            let tokens = tokenize(black_box(source_code.clone()));
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
