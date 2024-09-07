// Compiler Statements

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    backend_vm::instructions::{Abc, OP_MOVE, OP_RETURN},
    errors::VMCompileError,
    frontend::ast::{Expr, FromType, Stmt, Type},
};

use super::{
    compiler::{Compiler, ReturnValue},
    symbol_tracker::SymbolTable,
    type_table::{self, TypeTableEntry, VMCompilerType},
};

impl Compiler {
    pub(super) fn compile_statement(
        &mut self,
        stmt: Stmt,
        require_constant_as_register: bool,
        require_result: bool,
        fn_return: Option<usize>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        type_table: &Rc<RefCell<type_table::TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        match stmt {
            Stmt::ExprStmt(expr) => self.compile_expression(
                expr,
                require_constant_as_register,
                require_result,
                fn_return,
                symbol_table,
                type_table,
            ),
            Stmt::DeclareStmt {
                name,
                is_mutable: _,
                is_global,
                var_type: _,
                expr,
            } => self.compile_declare_stmt(name, is_global, expr, symbol_table, type_table),
            Stmt::ReturnStmt(expr) => {
                self.compile_return_stmt(expr, fn_return, symbol_table, type_table)
            }
            Stmt::BreakStmt(expr) => self.compile_break_stmt(expr, symbol_table, type_table),
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
                type_table,
            ),
            Stmt::StructDefStmt {
                ident,
                key_type_values,
            } => self.compile_struct_def_stmt(ident, key_type_values, type_table),
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
        type_table: &Rc<RefCell<type_table::TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        let reg;
        if let Some(inner) = expr {
            reg = self
                .compile_expression(inner, true, true, None, symbol_table, type_table)?
                .safe_unwrap();
        } else {
            reg = self.allocate_register() as isize;
        }
        symbol_table.borrow_mut().declare_var(name, reg);
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
        type_table: &Rc<RefCell<type_table::TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        let mut function_compiler = self.new_fake_compiler();
        let function_symbol_table = &Rc::new(RefCell::new(SymbolTable::new()));

        function_symbol_table
            .borrow_mut()
            .declare_var(name.clone(), self.function_len() as isize);

        // Allocate result register
        let result_register = function_compiler.allocate_register();

        // Allocate registers for the parameters in the function's symbol table
        for param in parameters {
            let reg = function_compiler.allocate_register();
            function_symbol_table
                .borrow_mut()
                .declare_var(param.0, reg as isize);
        }

        let mut result = ReturnValue::Normal(0);

        // Compile body
        for stmt in body {
            result = function_compiler.compile_statement(
                stmt,
                true,
                true,
                Some(result_register),
                function_symbol_table,
                type_table,
            )?;
        }

        if !result.is_return() {
            // Move body result into function result
            function_compiler.add_instruction(Abc(
                OP_MOVE,
                result_register as i32,
                result.safe_unwrap() as i32,
                0,
            ));

            // Add return opcode to prevent running into other code outside of function
            function_compiler.add_instruction(Abc(OP_RETURN, 0, 0, 0));
        }

        // We have finished compiling the function itself. We can now transfer the instructions into the real compiler's function vec.
        // Also get the index of the function to save into the symbol table.
        let index = self.add_function(&mut function_compiler);

        // Add function to symbol table
        symbol_table.borrow_mut().declare_var(name, index as isize);

        Ok(ReturnValue::Normal(0))
    }

    fn compile_struct_def_stmt(
        &self,
        ident: String,
        key_type_values: HashMap<String, Type>,
        type_table: &Rc<RefCell<type_table::TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        let ident: &'static str = Box::leak(ident.into_boxed_str());
        let mut fields: HashMap<&'static str, VMCompilerType> =
            HashMap::with_capacity(key_type_values.len());

        for (key, value) in key_type_values {
            let key: &'static str = Box::leak(key.into_boxed_str());
            let value = VMCompilerType::from_type(&value).ok_or(VMCompileError::UndefinedType)?;
            fields.insert(key, value);
        }

        let entry = TypeTableEntry::StructDefStmt {
            key_type_values: fields,
        };

        type_table.borrow_mut().declare_type(ident, entry);

        Ok(ReturnValue::Normal(0))
    }

    fn compile_return_stmt(
        &mut self,
        expr: Option<Expr>,
        fn_return: Option<usize>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        type_table: &Rc<RefCell<type_table::TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        if let Some(inner) = expr {
            let reg = self
                .compile_expression(inner, true, true, None, symbol_table, type_table)?
                .safe_unwrap();
            if let Some(fn_reg) = fn_return {
                self.add_instruction(Abc(OP_MOVE, fn_reg as i32, reg as i32, 0));
                self.add_instruction(Abc(OP_RETURN, 0, 0, 0));
                return Ok(ReturnValue::Return(fn_reg as isize));
            }
            // Return when fn return not required???
            return Ok(ReturnValue::Return(reg));
        }
        Ok(ReturnValue::Return(0))
    }

    fn compile_break_stmt(
        &mut self,
        expr: Option<Expr>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        type_table: &Rc<RefCell<type_table::TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        if let Some(inner) = expr {
            let reg = self
                .compile_expression(inner, true, true, None, symbol_table, type_table)?
                .safe_unwrap();
            return Ok(ReturnValue::Break(reg));
        }
        Ok(ReturnValue::Break(0))
    }
}
