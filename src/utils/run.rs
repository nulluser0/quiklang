use std::{cell::RefCell, rc::Rc};

use crate::{
    backend::{environment::Environment, interpreter::evaluate},
    errors::Error,
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
                match evaluate(stmt, env, &root_env) {
                    Ok(_) => continue,
                    Err(e) => {
                        print_e(Error::RuntimeError(e));
                        break;
                    }
                };
            }
        }
        Err(e) => print_e(e),
    }
}

fn print_e(e: Error) {
    println!("E: {}", e);
}
