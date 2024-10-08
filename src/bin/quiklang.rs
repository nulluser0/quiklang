use tokio::io::AsyncReadExt;

use std::{cell::RefCell, collections::HashSet, process, rc::Rc};

use quiklang::{
    backend_vm::{
        bytecode::ByteCode,
        bytecode_compiler::{
            compiler::Compiler, symbol_tracker::SymbolTable, type_table::TypeTable,
        },
        vm::VM,
    },
    errors::{self, VMRuntimeError},
    frontend::type_environment::TypeEnvironment,
    utils::run::{print_e, run_vm},
};
use rustyline::{error::ReadlineError, Config, Editor};

#[tokio::main]
async fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.as_slice() {
        // If no arguments are passed, print usage
        [_] => print_usage(),

        // If "repl" is passed, start the REPL
        [_, cmd] if cmd == "repl" => repl_vm().await,

        // If a file path is passed, execute the file
        [_, file_path] => run_file_vm(file_path).await,

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

async fn run_file_vm(file_path: &str) {
    let mut file = match tokio::fs::File::open(file_path).await {
        Ok(file) => file,
        Err(e) => {
            println!("Error opening file {}: {}", file_path, e);
            process::exit(1);
        }
    };

    if file_path.ends_with(".qlbc") {
        // Read the file as binary data
        let mut content = Vec::new();
        if let Err(e) = file.read_to_end(&mut content).await {
            println!("Error reading file {}: {}", file_path, e);
            process::exit(1);
        }

        // Decode and run bytecode
        let bytecode = ByteCode::decode(&content).unwrap_or_else(|e| {
            println!("Bytecode decode error: {}", e);
            process::exit(1);
        });

        let vm = VM::from_bytecode(bytecode);

        // For memory efficiency, we can discard unused stuff
        drop(content);
        drop(file);

        match vm.execute().await {
            Ok(_) => {}
            Err(VMRuntimeError::Exit(code)) => process::exit(code),
            Err(e) => {
                print_e(errors::Error::VMRuntimeError(e));
            }
        }
    } else {
        // Read the file as a UTF-8 string
        let mut content = String::new();
        if let Err(e) = file.read_to_string(&mut content).await {
            println!("Error reading file {}: {}", file_path, e);
            process::exit(1);
        }

        let root_type_env = Rc::new(RefCell::new(TypeEnvironment::default()));
        let type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
            root_type_env.clone(),
        )));

        run_vm(content, type_env, root_type_env).await;
    }
}

async fn repl_vm() {
    println!("QuikLang REPL v{}", env!("CARGO_PKG_VERSION"));
    println!("Running experimental VM backend mode. Some things may be broken.");

    let mut compiler = Compiler::new();
    let mut vm = VM::new(vec![], vec![], HashSet::new(), vec![]);

    let mut root_type_env = Rc::new(RefCell::new(TypeEnvironment::default()));
    let mut type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
        root_type_env.clone(),
    )));

    let root_symbol_table: &Rc<RefCell<SymbolTable>> = &Rc::new(RefCell::new(SymbolTable::new()));
    let symbol_table: &Rc<RefCell<SymbolTable>> = &Rc::new(RefCell::new(
        SymbolTable::new_with_parent(root_symbol_table.clone()),
    ));

    let root_type_table = &Rc::new(RefCell::new(TypeTable::new()));
    let type_table = &Rc::new(RefCell::new(TypeTable::new_with_parent(
        root_type_table.clone(),
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
                    vm = VM::new(vec![], vec![], HashSet::new(), vec![]);
                    continue;
                }

                if trimmed_line == "debug" {
                    println!("VM: {:?}", vm);
                    println!("Compiler: {:?}", compiler);
                    println!("Symbol Table: {:?}", symbol_table);
                    continue;
                }

                unimplemented!(
                    "yeah repl_vm is broken due to implementation of multithreading in VM"
                );

                // run_vm_repl(RunVmReplArgs {
                //     input: trimmed_line.to_string(),
                //     type_env: &type_env,
                //     root_type_env: &root_type_env,
                //     compiler: &mut compiler,
                //     vm: &mut vm,
                //     symbol_table,
                //     root_symbol_table,
                //     type_table,
                //     root_type_table,
                // });
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
