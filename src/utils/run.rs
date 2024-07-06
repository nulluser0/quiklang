use std::{cell::RefCell, rc::Rc};

use crate::{
    backend_interpreter::{environment::Environment, interpreter::evaluate},
    errors::Error,
    frontend::{parser, type_environment::TypeEnvironment},
};

pub fn run(
    input: String,
    env: &Rc<RefCell<Environment>>,
    type_env: &Rc<RefCell<TypeEnvironment>>,
    root_type_env: &Rc<RefCell<TypeEnvironment>>,
) {
    let root_env = env
        .borrow()
        .get_parent()
        .expect("Env should have a root env!");
    let mut parser = parser::Parser::new();
    match parser.produce_ast(input, type_env, root_type_env) {
        Ok(program) => {
            // println!("{:#?}", program);

            for stmt in program.statements {
                match evaluate(stmt, env, &root_env) {
                    Ok(_) => continue,
                    Err(e) => {
                        print_e(Error::InterpreterError(e));
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
