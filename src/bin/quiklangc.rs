use std::{
    cell::RefCell,
    fs::File,
    io::{Read, Write},
    process,
    rc::Rc,
};

use quiklang::{
    backend_vm::{bytecode::ByteCode, bytecode_compiler::compiler::Compiler},
    frontend::{parser::Parser, type_environment::TypeEnvironment},
};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.as_slice() {
        // If no arguments are passed, print usage
        [_] => print_usage(),

        // If "ls" is passed, list the instructions
        [_, cmd, file_path] if cmd == "ls" => list(file_path),

        // If a file path is passed, compile it
        [_, file_path, file_destination] => compile_file(file_path, file_destination),

        // Print usage instructions if the input is invalid
        _ => print_usage(),
    }
}

fn print_usage() {
    println!("Usage: quiklangc <command>");
    println!("Commands:");
    println!("  ls <file>            - Prints the instructions in the Quiklang Bytecode file");
    println!("  <file> <output path> - Compiles the Quiklang file and outputs it to output path");
}

fn list(file_path: &str) {
    let mut file: File = File::open(file_path).unwrap_or_else(|e| {
        println!("Error opening file {}: {}", file_path, e);
        process::exit(1);
    });

    // Read the entire file into buffer
    let mut buffer: Vec<u8> = Vec::new();
    file.read_to_end(&mut buffer).unwrap_or_else(|e| {
        println!("Error reading file {}: {}", file_path, e);
        process::exit(1);
    });

    // Check if the first 4 bytes are "QLBC"
    if buffer.len() >= 4 && &buffer[..4] == b"QLBC" {
        // Decode the bytecode
        let bytecode_decoded = ByteCode::decode(&buffer).unwrap_or_else(|e| {
            println!("Bytecode decode error: {}", e);
            process::exit(1);
        });
        println!("{}", bytecode_decoded);
    } else {
        // Compile the file
        let bytecode = compile(file_path);
        println!("{}", bytecode);
    }
}

fn compile(file_path: &str) -> ByteCode {
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

    // Parse, then compile, then save.

    let mut parser = Parser::new();
    let program = match parser.produce_ast(content, &type_env, &root_type_env) {
        Ok(result) => result,
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        }
    };

    let mut compiler = Compiler::new();
    match compiler.compile(program.statements) {
        Ok(result) => result,
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        }
    }
}

fn compile_file(file_path: &str, file_destination: &str) {
    let bytecode = compile(file_path);
    let encoded_bytecode = ByteCode::encode(&bytecode).unwrap_or_else(|e| {
        println!("{}", e);
        process::exit(1);
    });

    // Save the file
    let mut file = File::create(file_destination).unwrap();
    file.write_all(&encoded_bytecode).unwrap_or_else(|e| {
        println!("{}", e);
        process::exit(1);
    });
}
