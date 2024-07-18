// Compiler

use crate::{backend_vm::bytecode::BCMetadata, errors::VMCompileError, frontend::ast::{Expr, Stmt}};

use super::bytecode::{BCIntegrityInfo, ByteCode};

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

pub struct Compiler;

impl Compiler {
    pub fn compile(stmts: Vec<Stmt>) -> Result<ByteCode, VMCompileError> {
        let metadata = BCMetadata {
            ql_version: str_to_byte_array(env!("CARGO_PKG_VERSION")),
            ql_vm_ver: env!("QUIKLANG_VM_VERSION").parse().unwrap(),
            flags: 0,
        };
        let integrity_info = BCIntegrityInfo {
            num_register: 0,
            num_constants: 0,
            num_inst: 0,
            num_string_points: 0,
        };
        let mut bytecode = ByteCode::new(metadata, integrity_info);
        // Generate bytecode from AST
        Self::compile_statements(&mut bytecode, stmts)?;

        Ok(bytecode)
    }

    fn compile_statements(bytecode: &mut ByteCode, stmts: Vec<Stmt>) -> Result<(), VMCompileError> {
        for stmt in stmts {
            Self::compile_statement(bytecode, stmt)?;
        }
        Ok(())
    }

    fn compile_statement(bytecode: &mut ByteCode, stmt: Stmt) -> Result<(), VMCompileError> {
        match stmt {
            Stmt::ExprStmt(expr) => Self::compile_expression(bytecode, expr)?,
            Stmt::DeclareStmt {
                name,
                is_mutable,
                is_global,
                var_type,
                expr,
            } => todo!(),
            Stmt::ReturnStmt(_) => todo!(),
            Stmt::BreakStmt(_) => todo!(),
            Stmt::FunctionDeclaration {
                parameters,
                name,
                return_type,
                body,
                is_async,
            } => todo!(),
            Stmt::StructDefStmt {
                ident,
                key_type_values,
            } => todo!(),
            Stmt::EnumDefStmt { ident, variants } => todo!(),
            Stmt::AliasDefStmt { ident, alias } => todo!(),
        }
        Ok(())
    }

    fn compile_expression(bytecode: &mut ByteCode, expr: Expr) -> Result<(), VMCompileError> {
        match expr {
            Expr::Literal(_) => todo!(),
            Expr::Array(_, _) => todo!(),
            Expr::Identifier(_) => todo!(),
            Expr::Tuple(_) => todo!(),
            Expr::AssignmentExpr { assignee, expr } => todo!(),
            Expr::ConcatOp { left, right } => todo!(),
            Expr::BinaryOp { op, left, right } => todo!(),
            Expr::UnaryOp(_, _) => todo!(),
            Expr::FunctionCall(_, _) => todo!(),
            Expr::Member(_, _) => todo!(),
            Expr::IfExpr { condition, then, else_stmt } => todo!(),
            Expr::ForExpr { identifier, iterable, then } => todo!(),
            Expr::WhileExpr { condition, then } => todo!(),
            Expr::BlockExpr(_) => todo!(),
            Expr::ForeverLoopExpr(_) => todo!(),
            Expr::SpecialNull => todo!(),
            Expr::StructLiteral(_, _) => todo!(),
            Expr::EnumLiteral(_, _, _) => todo!(),
        }
    }
}
