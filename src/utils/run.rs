use std::{cell::RefCell, process, rc::Rc};

use crate::{
    backend_vm::{
        bytecode_compiler::{compiler::Compiler, symbol_tracker::SymbolTable, type_table},
        vm::VM,
    },
    errors::{self, Error, VMRuntimeError},
    frontend::{parser, type_environment::TypeEnvironment},
};

// To also shut up clippy
pub struct RunVmReplArgs<'a> {
    pub input: String,
    pub type_env: &'a Rc<RefCell<TypeEnvironment>>,
    pub root_type_env: &'a Rc<RefCell<TypeEnvironment>>,
    pub compiler: &'a mut Compiler,
    pub vm: &'a mut VM,
    pub symbol_table: &'a Rc<RefCell<SymbolTable>>,
    pub root_symbol_table: &'a Rc<RefCell<SymbolTable>>,
    pub type_table: &'a Rc<RefCell<type_table::TypeTable>>,
    pub root_type_table: &'a Rc<RefCell<type_table::TypeTable>>,
}

// pub fn run_vm_repl(
//     RunVmReplArgs {
//         input,
//         type_env,
//         root_type_env,
//         compiler,
//         vm,
//         symbol_table,
//         root_symbol_table,
//         type_table,
//         root_type_table,
//     }: RunVmReplArgs,
// ) {
//     let mut parser = parser::Parser::new();
//     match parser.produce_ast(input, type_env, root_type_env) {
//         Ok(program) => {
//             match compiler.compile(
//                 program.statements,
//                 symbol_table,
//                 root_symbol_table,
//                 type_table,
//                 root_type_table,
//             ) {
//                 Ok(mut bytecode) => {
//                     vm.constant_pool = bytecode.constants;
//                     vm.program_counter = if !bytecode.instructions.is_empty() {
//                         vm.instructions.len()
//                     } else {
//                         vm.program_counter
//                     };
//                     vm.instructions.append(&mut bytecode.instructions);
//                     vm.function_indexes = bytecode
//                         .qlang_functions
//                         .iter()
//                         .map(|f| *f as usize)
//                         .collect::<Vec<usize>>();
//                     match vm.execute() {
//                         Ok(_) => {}
//                         Err(VMRuntimeError::Exit(code)) => println!("Exited with code: {}", code),
//                         Err(e) => {
//                             print_e(errors::Error::VMRuntimeError(e));
//                             vm.on_error_cleanup();
//                             println!("Please use 'drain' to reset VM, parser, and compiler.")
//                         }
//                     }
//                 }
//                 Err(e) => {
//                     print_e(errors::Error::VMCompileError(e));
//                 }
//             }
//         }
//         Err(e) => print_e(e),
//     }
// }

pub async fn run_vm(
    input: String,
    type_env: &Rc<RefCell<TypeEnvironment>>,
    root_type_env: &Rc<RefCell<TypeEnvironment>>,
) {
    let mut parser = parser::Parser::new();
    match parser.produce_ast(input, type_env, root_type_env) {
        Ok(program) => {
            let mut compiler = Compiler::new();
            match compiler.compile_no_symbol_table_input(program.statements) {
                Ok(bytecode) => {
                    let vm = VM::from_bytecode(bytecode);
                    match vm.execute().await {
                        Ok(_) => {}
                        Err(VMRuntimeError::Exit(code)) => process::exit(code),
                        Err(e) => {
                            print_e(errors::Error::VMRuntimeError(e));
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

pub fn print_e(e: Error) {
    println!("E: {}", e);
}
