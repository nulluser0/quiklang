use std::{cell::RefCell, rc::Rc};

use crate::{
    backend_interpreter::{environment::Environment, interpreter::evaluate},
    backend_vm::{bytecode_compiler::compiler::Compiler, vm::VM},
    errors::{self, Error},
    frontend::{parser, type_environment::TypeEnvironment},
};

pub fn run_vm(
    input: String,
    type_env: &Rc<RefCell<TypeEnvironment>>,
    root_type_env: &Rc<RefCell<TypeEnvironment>>,
) -> VM {
    let mut parser = parser::Parser::new();
    match parser.produce_ast(input, type_env, root_type_env) {
        Ok(program) => {
            let mut compiler = Compiler::new();
            match compiler.compile(program.statements) {
                Ok(bytecode) => {
                    let mut vm = VM::from_bytecode(bytecode);
                    vm.execute();
                    // TODO: for now only for debugging...
                    vm
                }
                Err(e) => {
                    print_e(errors::Error::VMCompileError(e));
                    VM::new(vec![0], vec![], 0)
                }
            }
        }
        Err(e) => panic!("{}", e),
    }
}

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
