// Compiler Expressions

use std::{cell::RefCell, rc::Rc};

use crate::{
    backend_vm::{
        instructions::{
            ABx, ASBx, Abc, OP_ADD, OP_AND, OP_DIV, OP_EQ, OP_GE, OP_GT, OP_JUMP, OP_JUMP_IF_FALSE,
            OP_LE, OP_LOADBOOL, OP_LOADCONST, OP_LOADNULL, OP_LT, OP_MOD, OP_MOVE, OP_MUL, OP_NE,
            OP_NOT, OP_OR, OP_SUB,
        },
        vm::RegisterVal,
    },
    errors::VMCompileError,
    frontend::ast::{BinaryOp, Expr, Literal, Stmt, UnaryOp},
};

use super::{compiler::Compiler, symbol_tracker::SymbolTable};

impl Compiler {
    pub(super) fn compile_expression(
        &mut self,
        expr: Expr,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<isize, VMCompileError> {
        match expr {
            Expr::Literal(literal) => self.compile_literal(literal),
            Expr::Array(_, _) => todo!(),
            Expr::Identifier(identifier) => self.compile_identifier(identifier, symbol_table),
            Expr::Tuple(_) => todo!(),
            Expr::AssignmentExpr { assignee, expr } => todo!(),
            Expr::ConcatOp { left, right } => todo!(),
            Expr::BinaryOp { op, left, right } => {
                self.compile_binary_op(op, *left, *right, symbol_table)
            }
            Expr::UnaryOp(op, expr) => self.compile_unary_op(op, *expr, symbol_table),
            Expr::FunctionCall(_, _) => todo!(),
            Expr::Member(_, _) => todo!(),
            Expr::IfExpr {
                condition,
                then,
                else_stmt,
            } => self.compile_if_expr(*condition, then, else_stmt, symbol_table),
            Expr::ForExpr {
                identifier,
                iterable,
                then,
            } => todo!(),
            Expr::WhileExpr { condition, then } => todo!(),
            Expr::BlockExpr(_) => todo!(),
            Expr::ForeverLoopExpr(_) => todo!(),
            Expr::SpecialNull => todo!(),
            Expr::StructLiteral(_, _) => todo!(),
            Expr::EnumLiteral(_, _, _) => todo!(),
        }
    }

    fn compile_literal(
        &mut self,
        literal: Literal,
        require_constant: bool,
    ) -> Result<isize, VMCompileError> {
        let constant = match literal {
            Literal::Integer(integer) => RegisterVal::Int(integer),
            Literal::Float(float) => RegisterVal::Float(float),
            Literal::String(string) => RegisterVal::Str(string.into()),
            Literal::Object(_) => todo!(),
        };
        let index = self.add_constant(constant);

        if require_constant {
            let reg = self.allocate_register();
            self.add_instruction(ABx(OP_LOADCONST, reg as i32, index as i32));
            return Ok(reg as isize);
        }

        Ok(-(index as isize))
    }

    fn compile_identifier(
        &mut self,
        identifier: String,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        require_constant: bool,
    ) -> Result<isize, VMCompileError> {
        match identifier.as_str() {
            "null" => {
                let index = self.add_constant(RegisterVal::Null);
                Ok(-(index as isize))
            }
            "true" => {
                let index = self.add_constant(RegisterVal::Bool(true));
                Ok(-(index as isize))
            }
            "false" => {
                let index = self.add_constant(RegisterVal::Bool(false));
                Ok(-(index as isize))
            }
            other => {
                // No special ident_keyword. Instead, match symbol table.
                Ok(symbol_table
                    .borrow()
                    .lookup_var(other)
                    .ok_or(VMCompileError::UndefinedVariable(other.to_string()))?
                    as isize)
            }
        }
    }

    fn compile_binary_op(
        &mut self,
        op: BinaryOp,
        left: Expr,
        right: Expr,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<isize, VMCompileError> {
        let reg = self.allocate_register();
        let b = self.compile_expression(left, symbol_table)? as i32;
        let c = self.compile_expression(right, symbol_table)? as i32;
        let opcode = match op {
            BinaryOp::Add => OP_ADD,
            BinaryOp::Subtract => OP_SUB,
            BinaryOp::Multiply => OP_MUL,
            BinaryOp::Divide => OP_DIV,
            BinaryOp::GreaterThan => OP_GT,
            BinaryOp::LessThan => OP_LT,
            BinaryOp::GreaterOrEqual => OP_GE,
            BinaryOp::LessOrEqual => OP_LE,
            BinaryOp::Equal => OP_EQ,
            BinaryOp::NotEqual => OP_NE,
            BinaryOp::And => OP_AND,
            BinaryOp::Or => OP_OR,
            BinaryOp::Modulus => OP_MOD,
        };
        self.add_instruction(Abc(opcode, reg as i32, b, c));
        Ok(reg as isize)
    }

    fn compile_unary_op(
        &mut self,
        op: UnaryOp,
        expr: Expr,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<isize, VMCompileError> {
        let reg = self.allocate_register();
        let b = self.compile_expression(expr, symbol_table)? as i32;
        let opcode = match op {
            UnaryOp::LogicalNot => OP_NOT,
            UnaryOp::ArithmeticNegative => todo!(),
            UnaryOp::ArithmeticPositive => todo!(),
            UnaryOp::BitwiseNot => OP_NOT,
        };
        self.add_instruction(Abc(opcode, reg as i32, b, 0));
        Ok(reg as isize)
    }

    fn compile_if_expr(
        &mut self,
        condition: Expr,
        then: Vec<Stmt>,
        else_stmt: Option<Vec<Stmt>>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<isize, VMCompileError> {
        // TODO: only return result if required, optimisation

        // Typical bytecode representation of if expr:
        //      r1 = evaluate condition
        //      jump if r1 is false to else/endif area
        //      then area
        //      jump to endif area
        //      else area
        //      endif area

        // Store the if expr result
        let result_register = self.allocate_register();
        let mut result: isize = 0;

        // Save the top register so that we can disregard the registers from the if expr after use.
        let current_reg_top = self.reg_top();

        // First, get the condition's result and store its register.
        let condition_result = self.compile_expression(condition, symbol_table)?;

        // Add instruction to jump to else/endif if false
        let jump_to_end_or_else = self.instructions_len(); // Index of the JUMP_IF_ELSE inst
        self.add_instruction(ASBx(OP_JUMP_IF_FALSE, condition_result as i32, 0)); // Placeholder for now

        // Now we compile the 'then' area.
        // Create a new symbol table since we are entering a child scope.
        let child_symbol_table = &Rc::new(RefCell::new(SymbolTable::new_with_parent(
            symbol_table.clone(),
        )));
        for stmt in then {
            result = self.compile_statement(stmt, child_symbol_table)?;
        }
        self.add_instruction(Abc(OP_MOVE, result_register as i32, result as i32, 0));

        // Add placeholder jump to endif after then block
        let jump_to_end = self.instructions_len();

        // Reset register count back to normal in preparation for else/endif
        self.manually_change_register_count(current_reg_top);

        if else_stmt.is_none() {
            // No else statement.
            // Update jump_if_false
            self.replace_instruction(
                jump_to_end_or_else,
                ASBx(
                    OP_JUMP_IF_FALSE,
                    condition_result as i32,
                    (jump_to_end - jump_to_end_or_else) as i32,
                ),
            )
        }

        // There is an else stmt.
        // Create jump to endif after the then block
        // Using jump_to_end var from before.
        self.add_instruction(ASBx(OP_JUMP, 0, 0)); // Placeholder for now

        // Compile 'else' area.
        let child_symbol_table = &Rc::new(RefCell::new(SymbolTable::new_with_parent(
            symbol_table.clone(),
        )));
        for stmt in else_stmt.unwrap() {
            result = self.compile_statement(stmt, child_symbol_table)?;
        }
        self.add_instruction(Abc(OP_MOVE, result_register as i32, result as i32, 0));

        // Update jumps
        let end = self.instructions_len();

        // Jump to else stmt
        self.replace_instruction(
            jump_to_end_or_else,
            ASBx(
                OP_JUMP_IF_FALSE,
                condition_result as i32,
                (jump_to_end + 1 - jump_to_end_or_else) as i32,
            ),
        );

        // Jump to end from then block
        self.replace_instruction(jump_to_end, ASBx(OP_JUMP, 0, (end - jump_to_end) as i32));

        // Reset register count back to normal in preparation for endif
        self.manually_change_register_count(current_reg_top);

        Ok(result_register as isize)
    }
}
