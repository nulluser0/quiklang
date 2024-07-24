// Compiler Statements

use std::{cell::RefCell, rc::Rc};

use crate::{
    errors::VMCompileError,
    frontend::ast::{Expr, Stmt},
};

use super::{compiler::Compiler, symbol_tracker::SymbolTable};

impl Compiler {
    pub(super) fn compile_statement(
        &mut self,
        stmt: Stmt,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<isize, VMCompileError> {
        match stmt {
            Stmt::ExprStmt(expr) => self.compile_expression(expr, true, false, symbol_table),
            Stmt::DeclareStmt {
                name,
                is_mutable,
                is_global,
                var_type,
                expr,
            } => self.compile_declare_stmt(name, is_global, expr, symbol_table),
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

    fn compile_declare_stmt(
        &mut self,
        name: String,
        is_global: bool,
        expr: Option<Expr>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<isize, VMCompileError> {
        let reg;
        if let Some(inner) = expr {
            reg = self.compile_expression(inner, true, true, symbol_table)?;
        } else {
            reg = self.allocate_register() as isize;
        }
        symbol_table.borrow_mut().declare_var(name, reg as usize);
        Ok(reg)
    }
}
