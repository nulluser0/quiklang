// Compiler Statements

use std::{cell::RefCell, rc::Rc};

use crate::{
    errors::VMCompileError,
    frontend::ast::{Expr, Stmt},
};

use super::{
    compiler::{Compiler, ReturnValue},
    symbol_tracker::SymbolTable,
};

impl Compiler {
    pub(super) fn compile_statement(
        &mut self,
        stmt: Stmt,
        require_constant_as_register: bool,
        require_result: bool,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        match stmt {
            Stmt::ExprStmt(expr) => self.compile_expression(expr, true, false, symbol_table),
            Stmt::DeclareStmt {
                name,
                is_mutable,
                is_global,
                var_type,
                expr,
            } => self.compile_declare_stmt(name, is_global, expr, symbol_table),
            Stmt::ReturnStmt(expr) => self.compile_return_stmt(expr, symbol_table),
            Stmt::BreakStmt(expr) => self.compile_break_stmt(expr, symbol_table),
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
    ) -> Result<ReturnValue, VMCompileError> {
        let reg;
        if let Some(inner) = expr {
            reg = self
                .compile_expression(inner, true, true, symbol_table)?
                .safe_unwrap();
        } else {
            reg = self.allocate_register() as isize;
        }
        symbol_table.borrow_mut().declare_var(name, reg as usize);
        Ok(ReturnValue::Normal(reg))
    }

    fn compile_return_stmt(
        &mut self,
        expr: Option<Expr>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        if let Some(inner) = expr {
            let reg = self
                .compile_expression(inner, true, true, symbol_table)?
                .safe_unwrap();
            return Ok(ReturnValue::Return(reg));
        }
        Ok(ReturnValue::Return(0))
    }

    fn compile_break_stmt(
        &mut self,
        expr: Option<Expr>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        if let Some(inner) = expr {
            let reg = self
                .compile_expression(inner, true, true, symbol_table)?
                .safe_unwrap();
            return Ok(ReturnValue::Break(reg));
        }
        Ok(ReturnValue::Break(0))
    }
}
