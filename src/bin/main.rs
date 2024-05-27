use std::{io::{self, Write}, process};

use quiklang::frontend::parser;

fn main() {
    // Simple implementation for command system. Later, I can port the command system from my other projects...
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 2 && args[1] == "repl" {
        repl();
    } else {
        println!("Usage: quiklang <command>");
        println!("Commands:");
        println!("  repl - Start the QuikLang REPL");
    }
}

fn repl() {
    println!("QuikLang REPL v{}", env!("CARGO_PKG_VERSION"));

    loop {
        let mut parser = parser::Parser::new();
        print!("quiklang> ");
        io::stdout().flush().expect("Failed to flush stdout");

        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Failed to read line");

        // Exit if user enters "exit" or "quit"
        if input.trim() == "exit" || input.trim() == "quit" {
            println!("Exiting QuikLang REPL.");
            process::exit(1);
        }

        let program = parser.produce_ast(input);
        println!("{:#?}", program);
    }
}