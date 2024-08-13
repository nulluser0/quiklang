use std::{cell::RefCell, fs::File, io::Read, process, rc::Rc};

use quiklang::{
    backend_interpreter::environment::Environment,
    backend_vm::{bytecode_compiler::compiler::Compiler, vm::VM},
    frontend::type_environment::TypeEnvironment,
    utils::run::{run_interpreter, run_vm, run_vm_repl},
};
use rustyline::{error::ReadlineError, Config, Editor};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.as_slice() {
        // If no arguments are passed, print usage
        [_] => print_usage(),

        // If "vm" is passed then the file, execute the file using vm mode
        [_, cmd, file_path] if cmd == "vm" => run_file_vm(file_path),

        // If "repl" is passed, start the REPL
        [_, cmd] if cmd == "repl" => repl(),

        // If "repl_vm" is passed, start the REPL_VM
        [_, cmd] if cmd == "repl_vm" => repl_vm(),

        // If a file path is passed, execute the file
        [_, file_path] => run_file_interpreter(file_path),

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

fn run_file_interpreter(file_path: &str) {
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
    run_interpreter(content, &env, &type_env, &root_type_env)
}

fn run_file_vm(file_path: &str) {
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

    let root_type_env = Rc::new(RefCell::new(TypeEnvironment::default()));
    let type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
        root_type_env.clone(),
    )));

    run_vm(content, &type_env, &root_type_env);
}

fn repl_vm() {
    println!("QuikLang REPL v{}", env!("CARGO_PKG_VERSION"));
    println!("Running experimental VM backend mode. Some things may be broken.");

    let mut compiler = Compiler::new();
    let mut vm = VM::new(vec![], vec![], vec![], 0);

    let mut root_type_env = Rc::new(RefCell::new(TypeEnvironment::default()));
    let mut type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
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

                if trimmed_line == "drain" {
                    println!("Draining variables and functions in type_env, VM, and compiler.");
                    root_type_env = Rc::new(RefCell::new(TypeEnvironment::default()));
                    type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
                        root_type_env.clone(),
                    )));
                    compiler = Compiler::new();
                    vm = VM::new(vec![], vec![], vec![], 0);
                    continue;
                }

                run_vm_repl(
                    trimmed_line.to_string(),
                    &type_env,
                    &root_type_env,
                    &mut compiler,
                    &mut vm,
                );
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

fn repl() {
    println!("QuikLang REPL v{}", env!("CARGO_PKG_VERSION"));

    let env = Rc::new(RefCell::new(Environment::new_with_parent(Rc::new(
        RefCell::new(Environment::default()),
    ))));

    let mut root_type_env = Rc::new(RefCell::new(TypeEnvironment::default()));
    let mut type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
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

                if trimmed_line == "drain" {
                    println!("Draining variables and functions in both type_env and runtime_env.");
                    root_type_env = Rc::new(RefCell::new(TypeEnvironment::default()));
                    type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
                        root_type_env.clone(),
                    )));
                    continue;
                }

                run_interpreter(trimmed_line.to_string(), &env, &type_env, &root_type_env);
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
