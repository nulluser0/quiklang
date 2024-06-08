use std::{cell::RefCell, rc::Rc};

use crate::{
    backend::{environment::Environment, interpreter::evaluate},
    frontend::parser,
};

pub fn run(input: String, env: &Rc<RefCell<Environment>>) {
    let mut parser = parser::Parser::new();
    match parser.produce_ast(input) {
        Ok(program) => {
            // println!("{:#?}", program);

            for stmt in program.statements {
                let _ = evaluate(stmt, env);
            }
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}
