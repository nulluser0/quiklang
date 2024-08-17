use std::{cell::RefCell, process, rc::Rc};

use crate::{
    backend_interpreter::{environment::Environment, interpreter::evaluate},
    backend_vm::{bytecode_compiler::compiler::Compiler, vm::VM},
    errors::{self, Error, VMRuntimeError},
    frontend::{parser, type_environment::TypeEnvironment},
};

pub fn run_vm_repl(
    input: String,
    type_env: &Rc<RefCell<TypeEnvironment>>,
    root_type_env: &Rc<RefCell<TypeEnvironment>>,
    compiler: &mut Compiler,
    vm: &mut VM,
) {
    let mut parser = parser::Parser::new();
    match parser.produce_ast(input, type_env, root_type_env) {
        Ok(program) => match compiler.compile(program.statements) {
            Ok(bytecode) => {
                vm.constant_pool = bytecode.constants;
                vm.instructions = bytecode.instructions;
                match vm.execute() {
                    Ok(_) => {}
                    Err(VMRuntimeError::Exit(code)) => println!("Exited with code: {}", code),
                    Err(e) => {
                        print_e(errors::Error::VMRuntimeError(e));
                        vm.on_error_cleanup();
                        println!("Please use 'drain' to reset VM, parser, and compiler.")
                    }
                }
            }
            Err(e) => {
                print_e(errors::Error::VMCompileError(e));
            }
        },
        Err(e) => print_e(e),
    }
}

pub fn run_vm(
    input: String,
    type_env: &Rc<RefCell<TypeEnvironment>>,
    root_type_env: &Rc<RefCell<TypeEnvironment>>,
) {
    let mut parser = parser::Parser::new();
    match parser.produce_ast(input, type_env, root_type_env) {
        Ok(program) => {
            let mut compiler = Compiler::new();
            match compiler.compile(program.statements) {
                Ok(bytecode) => {
                    let mut vm = VM::from_bytecode(bytecode);
                    match vm.execute() {
                        Ok(_) => {}
                        Err(VMRuntimeError::Exit(code)) => process::exit(code),
                        Err(e) => {
                            print_e(errors::Error::VMRuntimeError(e));
                            vm.on_error_cleanup();
                        }
                    }
                }
                Err(e) => {
                    print_e(errors::Error::VMCompileError(e));
                }
            }
        }
        Err(e) => print_e(e),
    }
}

pub fn run_interpreter(
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
