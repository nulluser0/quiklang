use std::{
    cell::RefCell,
    fs::File,
    io::{self, Read, Write},
    process,
    rc::Rc,
};

use quiklang::{backend::environment::Environment, utils::run::run};

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

    let env = Rc::new(RefCell::new(Environment::new()));
    run(content, &env)
}

fn repl() {
    println!("QuikLang REPL v{}", env!("CARGO_PKG_VERSION"));

    let env = Rc::new(RefCell::new(Environment::new()));

    loop {
        print!("quiklang> ");
        io::stdout().flush().expect("Failed to flush stdout");

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        // Exit if user enters "exit" or "quit"
        if input.trim() == "exit" || input.trim() == "quit" {
            println!("Exiting QuikLang REPL.");
            process::exit(0); // Exit normally
        }
        run(input, &env);
    }
}
