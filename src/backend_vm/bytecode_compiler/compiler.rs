// Compiler

use std::{cell::RefCell, collections::HashMap, mem::swap, rc::Rc};

use crate::{
    backend_vm::{
        bytecode::{BCIntegrityInfo, BCMetadata, ByteCode},
        instructions::Instruction,
        vm::RegisterVal,
    },
    errors::VMCompileError,
    frontend::ast::Stmt,
};

use super::symbol_tracker::SymbolTable;

#[derive(Debug, Clone, Copy)]
pub(super) enum ReturnValue {
    Normal(isize), // A standard isize format.
    Break(isize),  // Standard isize with Break discriminant.
    Return(isize), // Standard isize with Return discriminant.
}

impl ReturnValue {
    pub(super) fn safe_unwrap(&self) -> isize {
        match self {
            ReturnValue::Normal(i) => *i,
            ReturnValue::Break(i) => *i,
            ReturnValue::Return(i) => *i,
        }
    }

    pub(super) fn is_break(&self) -> bool {
        matches!(self, ReturnValue::Break(_))
    }

    pub(super) fn is_return(&self) -> bool {
        matches!(self, ReturnValue::Return(_))
    }
}

const fn str_to_byte_array(s: &str) -> [u8; 8] {
    // Convert the string to a byte array with exactly 8 bytes.
    // If the string is shorter, pad with zeros. If it's longer, truncate.
    let mut bytes = [0u8; 8];
    let s_bytes = s.as_bytes();
    let len = if s_bytes.len() > 8 { 8 } else { s_bytes.len() };

    let mut i = 0;
    while i < len {
        bytes[i] = s_bytes[i];
        i += 1;
    }
    bytes
}

pub struct Compiler {
    max_reg: usize,
    reg_top: usize,
    constants: Vec<RegisterVal>,
    constant_map: HashMap<RegisterVal, usize>,
    instructions: Vec<Instruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            max_reg: 0,
            reg_top: 0,
            constants: Vec::new(),
            constant_map: HashMap::new(),
            instructions: Vec::new(),
        }
    }

    pub(super) fn reg_top(&self) -> usize {
        self.reg_top
    }

    pub(super) fn manually_change_register_count(&mut self, count: usize) -> usize {
        let reg = self.reg_top;
        self.reg_top = count;
        if self.reg_top > self.max_reg {
            self.max_reg = self.reg_top;
        }
        reg
    }

    pub(super) fn allocate_register(&mut self) -> usize {
        let reg = self.reg_top;
        self.reg_top += 1;
        if self.reg_top > self.max_reg {
            self.max_reg = self.reg_top;
        }
        reg
    }

    pub(super) fn deallocate_register(&mut self) {
        self.reg_top -= 1;
    }

    pub(super) fn add_constant(&mut self, constant: RegisterVal) -> usize {
        if let Some(&index) = self.constant_map.get(&constant) {
            return index;
        }
        let index = self.constants.len();
        self.constants.push(constant.clone());
        self.constant_map.insert(constant, index);
        index
    }

    pub(super) fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub(super) fn instructions_len(&mut self) -> usize {
        self.instructions.len()
    }

    pub(super) fn replace_instruction(&mut self, len: usize, instruction: Instruction) {
        self.instructions[len] = instruction
    }

    pub(super) fn pop_instruction(&mut self) -> Option<Instruction> {
        self.instructions.pop()
    }

    pub fn compile(&mut self, stmts: Vec<Stmt>) -> Result<ByteCode, VMCompileError> {
        let metadata = BCMetadata {
            ql_version: str_to_byte_array(env!("CARGO_PKG_VERSION")),
            ql_vm_ver: env!("QUIKLANG_VM_VERSION").parse().unwrap(),
            flags: 0,
        };
        let symbol_table: &Rc<RefCell<SymbolTable>> = &Rc::new(RefCell::new(SymbolTable::new()));
        // Generate bytecode from AST
        self.compile_statements(stmts, symbol_table)?;

        let integrity_info = BCIntegrityInfo {
            num_register: self.max_reg as i32,
            num_constants: self.constants.len() as i32,
            num_inst: self.instructions.len() as i32,
        };

        let mut bytecode = ByteCode::new(metadata, integrity_info);

        swap(&mut self.constants, &mut bytecode.constants);
        swap(&mut self.instructions, &mut bytecode.instructions);

        Ok(bytecode)
    }

    fn compile_statements(
        &mut self,
        stmts: Vec<Stmt>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<(), VMCompileError> {
        for stmt in stmts {
            self.compile_statement(stmt, true, false, symbol_table)?;
        }
        Ok(())
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use std::{
        fs::File,
        io::{Read, Write},
    };

    use crate::{
        backend_vm::{
            instructions::{get_argsbx, to_string, ASBx, OP_JUMP},
            vm::VM,
        },
        frontend::{
            ast::{BinaryOp, Expr, Literal, Type},
            parser,
            type_environment::TypeEnvironment,
        },
    };

    use super::*;

    #[test]
    fn compile_including_parse_etc() {
        let source_code = r#"
        // let mut n = 5;
        // let a = block {
        //     if n <= 1 {
        //         1
        //     } else {
        //         n
        //     }
        // };
        
        // n = a + 1
        loop {}
        "#
        .to_string();

        let root_type_env = Rc::new(RefCell::new(TypeEnvironment::default()));
        let type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
            root_type_env.clone(),
        )));
        let mut parser = parser::Parser::new();
        let ast = parser
            .produce_ast(source_code, &type_env, &root_type_env)
            .expect("fail");
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(ast.statements).expect("fail parse");
        println!("{}", bytecode);

        println!("ENCoded: {}", bytecode);

        let encoded_bytecode = ByteCode::encode(&bytecode).unwrap();
        let mut file = File::create("test.qlbc").unwrap();
        file.write_all(&encoded_bytecode).unwrap();

        let mut raw_bytecode: Vec<u8> = vec![];
        let mut read_file = File::open("test.qlbc").unwrap();
        read_file.read_to_end(&mut raw_bytecode).unwrap();
        let decoded_bytecode = ByteCode::decode(&raw_bytecode).unwrap();

        println!("DECoded: {}", decoded_bytecode);

        let mut vm = VM::from_bytecode(bytecode);
        // vm.execute();
        println!("{:#?}", vm);
    }

    #[test]
    fn simple_compile_test() {
        let mut compiler = Compiler::new();
        compiler
            .compile(vec![
                // Stmt::ExprStmt(Expr::Literal(Literal::Integer(209309))),
                Stmt::ExprStmt(Expr::BinaryOp {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Literal(Literal::Integer(123))),
                    right: Box::new(Expr::Literal(Literal::Integer(123))),
                }),
                Stmt::ExprStmt(Expr::IfExpr {
                    condition: Box::new(Expr::Identifier("true".to_string())),
                    then: vec![Stmt::ExprStmt(Expr::BinaryOp {
                        op: BinaryOp::Add,
                        left: Box::new(Expr::Literal(Literal::Integer(1223))),
                        right: Box::new(Expr::Literal(Literal::Integer(1223))),
                    })],
                    else_stmt: Some(vec![Stmt::ExprStmt(Expr::BinaryOp {
                        op: BinaryOp::Add,
                        left: Box::new(Expr::Literal(Literal::Integer(1233))),
                        right: Box::new(Expr::Literal(Literal::Integer(1233))),
                    })]),
                }),
                Stmt::DeclareStmt {
                    name: "idk".to_string(),
                    is_mutable: false,
                    is_global: false,
                    var_type: Type::Integer,
                    expr: Some(Expr::IfExpr {
                        condition: Box::new(Expr::Identifier("false".to_string())),
                        then: vec![Stmt::ExprStmt(Expr::Literal(Literal::Integer(42)))],
                        else_stmt: Some(vec![Stmt::ExprStmt(Expr::Literal(Literal::Integer(21)))]),
                    }),
                },
                Stmt::ExprStmt(Expr::BinaryOp {
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Identifier("idk".to_string())),
                    right: Box::new(Expr::Literal(Literal::Integer(12))),
                }),
            ])
            .expect("compile fail");

        println!("{}", compiler.max_reg);
        println!("{:#?}", compiler.constants);

        for inst in compiler.instructions {
            println!("{}", to_string(inst))
        }
    }

    #[test]
    fn sbx_test() {
        let sbx = ASBx(OP_JUMP, 3, 1);
        println!("{:#b} | sBx: {}", sbx, get_argsbx(sbx));
    }
}
