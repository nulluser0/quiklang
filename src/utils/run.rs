use std::{cell::RefCell, rc::Rc};

use crate::{
    backend::{environment::Environment, interpreter::evaluate},
    errors::Error,
    frontend::{parser, type_environment::TypeEnvironment},
};

pub fn run(input: String, env: &Rc<RefCell<Environment>>) {
    let root_env = env
        .borrow()
        .get_parent()
        .expect("Env should have a root env!");
    let mut parser = parser::Parser::new();
    let root_type_env = Rc::new(RefCell::new(match TypeEnvironment::new() {
        Ok(result) => result,
        Err(e) => {
            print_e(Error::ParserError(e));
            return;
        }
    }));
    let type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
        root_type_env.clone(),
    )));
    match parser.produce_ast(input, &type_env, &root_type_env) {
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
