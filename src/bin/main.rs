use std::{cell::RefCell, fs::File, io::Read, process, rc::Rc};

use quiklang::{
    backend_interpreter::environment::Environment, frontend::type_environment::TypeEnvironment, utils::run::run,
};
use rustyline::{error::ReadlineError, Config, Editor};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.as_slice() {
        // If no arguments are passed, print usage
        [_] => print_usage(),

        // If "repl" is passed, start the REPL
        [_, cmd] if cmd == "repl" => repl(),

        // If a file path is passed, execute the file
        [_, file_path] => run_file(file_path),

        // Print usage instructions if the input is invalid
        _ => print_usage(),
    }
}

fn print_usage() {
    println!("Usage: quiklang <command>");
    println!("Commands:");
    println!("  repl - Start the QuikLang REPL");
    println!("  <file> - Execute the specified QuikLang script file");
}

fn run_file(file_path: &str) {
    let mut file = match File::open(file_path) {
        Ok(file) => file,
        Err(e) => {
            println!("Error opening file {}: {}", file_path, e);
            process::exit(1);
        }
    };

    let mut content = String::new();
    if let Err(e) = file.read_to_string(&mut content) {
        println!("Error reading file {}: {}", file_path, e);
        process::exit(1);
    }

    let env = Rc::new(RefCell::new(Environment::new_with_parent(Rc::new(
        RefCell::new(Environment::default()),
    ))));
    let root_type_env = Rc::new(RefCell::new(TypeEnvironment::default()));
    let type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
        root_type_env.clone(),
    )));
    run(content, &env, &type_env, &root_type_env)
}

fn repl() {
    println!("QuikLang REPL v{}", env!("CARGO_PKG_VERSION"));

    let env = Rc::new(RefCell::new(Environment::new_with_parent(Rc::new(
        RefCell::new(Environment::default()),
    ))));

    let root_type_env = Rc::new(RefCell::new(TypeEnvironment::default()));
    let type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
        root_type_env.clone(),
    )));

    let config = Config::builder().build();
    let mut rl = Editor::<()>::with_config(config);

    if rl.load_history(".quiklang_history").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline("quiklang> ");
        match readline {
            Ok(line) => {
                let trimmed_line = line.trim();
                // Exit if user enters "exit" or "quit"
                if trimmed_line == "exit" || trimmed_line == "quit" {
                    println!("Exiting QuikLang REPL.");
                    break;
                }

                rl.add_history_entry(trimmed_line);

                run(trimmed_line.to_string(), &env, &type_env, &root_type_env);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history(".quiklang_history").unwrap();
}
