// Compiler Statements

use std::{cell::RefCell, rc::Rc};

use crate::{
    backend_vm::instructions::{Abc, OP_MOVE, OP_RETURN},
    errors::VMCompileError,
    frontend::ast::{Expr, Stmt, Type},
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
            Stmt::ExprStmt(expr) => self.compile_expression(
                expr,
                require_constant_as_register,
                require_result,
                symbol_table,
            ),
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
            } => self.compile_function_declaration(
                parameters,
                name,
                return_type,
                body,
                is_async,
                symbol_table,
            ),
            Stmt::StructDefStmt {
                ident,
                key_type_values,
            } => todo!(),
            Stmt::EnumDefStmt { ident, variants } => todo!(),
            Stmt::AliasDefStmt { ident, alias } => todo!(),
            Stmt::ExternFnDeclaration {
                parameters,
                name,
                return_type,
            } => todo!(),
        }
    }

    fn compile_declare_stmt(
        &mut self,
        name: String,
        _is_global: bool,
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

    fn compile_function_declaration(
        &mut self,
        parameters: Vec<(String, Type, bool)>,
        name: String,
        _return_type: Type,
        body: Vec<Stmt>,
        _is_async: bool,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        let mut function_compiler = self.new_fake_compiler();
        let function_symbol_table = &Rc::new(RefCell::new(SymbolTable::new()));

        function_symbol_table
            .borrow_mut()
            .declare_var(name.clone(), self.function_len());

        // Allocate result register
        let result_register = function_compiler.allocate_register();

        // Allocate registers for the parameters in the function's symbol table
        for param in parameters {
            let reg = function_compiler.allocate_register();
            function_symbol_table.borrow_mut().declare_var(param.0, reg);
        }

        // Compile body
        let result =
            function_compiler.compile_statements_with_result(body, function_symbol_table)?;

        // Move body result into function result
        function_compiler.add_instruction(Abc(
            OP_MOVE,
            result_register as i32,
            result.safe_unwrap() as i32,
            0,
        ));

        // Add return opcode to prevent running into other code outside of function
        function_compiler.add_instruction(Abc(OP_RETURN, 0, 0, 0));

        // We have finished compiling the function itself. We can now transfer the instructions into the real compiler's function vec.
        // Also get the index of the function to save into the symbol table.
        let index = self.add_function(&mut function_compiler);

        // Add function to symbol table
        symbol_table.borrow_mut().declare_var(name, index);

        Ok(ReturnValue::Normal(0))
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
