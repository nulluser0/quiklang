// Compiler Statements

use crate::{backend_vm::bytecode::ByteCode, errors::VMCompileError, frontend::ast::Stmt};

use super::compiler::Compiler;

impl Compiler {
    pub(super) fn compile_statement(
        bytecode: &mut ByteCode,
        stmt: Stmt,
    ) -> Result<(), VMCompileError> {
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
}
