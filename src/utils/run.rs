use std::{cell::RefCell, rc::Rc};

use crate::{
    backend::{environment::Environment, interpreter::evaluate},
    frontend::parser,
};

pub fn run(input: String, env: &Rc<RefCell<Environment>>) {
    let root_env = env
        .borrow()
        .get_parent()
        .expect("Env should have a root env!");
    let mut parser = parser::Parser::new();
    match parser.produce_ast(input) {
        Ok(program) => {
            // println!("{:#?}", program);

            for stmt in program.statements {
                let _ = evaluate(stmt, env, &root_env);
            }
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }
}
