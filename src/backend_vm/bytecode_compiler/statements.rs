// Compiler Statements

use std::{cell::RefCell, rc::Rc};

use crate::{errors::VMCompileError, frontend::ast::Stmt};

use super::{compiler::Compiler, symbol_tracker::SymbolTable};

impl Compiler {
    pub(super) fn compile_statement(
        &mut self,
        stmt: Stmt,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<isize, VMCompileError> {
        match stmt {
            Stmt::ExprStmt(expr) => self.compile_expression(expr, symbol_table),
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
    }
}
