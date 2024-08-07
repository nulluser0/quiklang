// Compiler Expressions

use std::{cell::RefCell, rc::Rc};

use crate::{
    backend_vm::{
        instructions::{
            rk_ask, ABx, ASBx, Abc, OP_ADD, OP_AND, OP_DIV, OP_EQ, OP_GE, OP_GT, OP_JUMP,
            OP_JUMP_IF_FALSE, OP_LE, OP_LOADBOOL, OP_LOADCONST, OP_LOADNULL, OP_LT, OP_MOD,
            OP_MOVE, OP_MUL, OP_NE, OP_NOT, OP_OR, OP_SUB,
        },
        vm::RegisterVal,
    },
    errors::VMCompileError,
    frontend::ast::{BinaryOp, Expr, Literal, Stmt, UnaryOp},
};

use super::{
    compiler::{Compiler, ReturnValue},
    symbol_tracker::SymbolTable,
};

impl Compiler {
    pub(super) fn compile_expression(
        &mut self,
        expr: Expr,
        require_constant_as_register: bool, // true = allocates register to a constant.
        require_result: bool, // true = certain exprs will return a result, like ifs, loops, fors, whiles, blocks.
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        match expr {
            Expr::Literal(literal) => self.compile_literal(literal, require_constant_as_register),
            Expr::Array(_, _) => todo!(),
            Expr::Identifier(identifier) => {
                self.compile_identifier(identifier, symbol_table, require_constant_as_register)
            }
            Expr::Tuple(_) => todo!(),
            Expr::AssignmentExpr { assignee, expr } => {
                self.compile_assignment(*assignee, *expr, symbol_table)
            }
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
            } => self.compile_if_expr(*condition, then, else_stmt, require_result, symbol_table),
            Expr::ForExpr {
                identifier,
                iterable,
                then,
            } => todo!(),
            Expr::WhileExpr { condition, then } => {
                self.compile_while_expr(*condition, then, require_result, symbol_table)
            }
            Expr::BlockExpr(block) => self.compile_block_expr(block, require_result, symbol_table),
            Expr::ForeverLoopExpr(block) => {
                self.compile_forever_loop_expr(block, require_result, symbol_table)
            }
            Expr::SpecialNull => self.compile_identifier(
                "null".to_string(),
                symbol_table,
                require_constant_as_register,
            ),
            Expr::StructLiteral(_, _) => todo!(),
            Expr::EnumLiteral(_, _, _) => todo!(),
        }
    }

    fn compile_literal(
        &mut self,
        literal: Literal,
        require_constant_as_register: bool,
    ) -> Result<ReturnValue, VMCompileError> {
        let constant = match literal {
            Literal::Integer(integer) => RegisterVal::Int(integer),
            Literal::Float(float) => RegisterVal::Float(float),
            Literal::String(string) => RegisterVal::Str(string.into()),
            Literal::Object(_) => todo!(),
        };
        let index = self.add_constant(constant);

        if require_constant_as_register {
            let reg = self.allocate_register();
            self.add_instruction(ABx(OP_LOADCONST, reg as i32, index as i32));
            return Ok(ReturnValue::Normal(reg as isize));
        }

        Ok(ReturnValue::Normal((rk_ask(index as i32)) as isize))
    }

    fn compile_identifier(
        &mut self,
        identifier: String,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        require_constant_as_register: bool,
    ) -> Result<ReturnValue, VMCompileError> {
        match identifier.as_str() {
            "null" => {
                if require_constant_as_register {
                    let reg = self.allocate_register();
                    self.add_instruction(Abc(OP_LOADNULL, reg as i32, reg as i32, 0));
                    return Ok(ReturnValue::Normal(reg as isize));
                }
                let index = self.add_constant(RegisterVal::Null);
                Ok(ReturnValue::Normal((rk_ask(index as i32)) as isize))
            }
            "true" => {
                if require_constant_as_register {
                    let reg = self.allocate_register();
                    self.add_instruction(Abc(OP_LOADBOOL, reg as i32, 1, 0));
                    return Ok(ReturnValue::Normal(reg as isize));
                }
                let index = self.add_constant(RegisterVal::Bool(true));
                Ok(ReturnValue::Normal((rk_ask(index as i32)) as isize))
            }
            "false" => {
                if require_constant_as_register {
                    let reg = self.allocate_register();
                    self.add_instruction(Abc(OP_LOADBOOL, reg as i32, 0, 0));
                    return Ok(ReturnValue::Normal(reg as isize));
                }
                let index = self.add_constant(RegisterVal::Bool(false));
                Ok(ReturnValue::Normal((rk_ask(index as i32)) as isize))
            }
            other => {
                // No special ident_keyword. Instead, match symbol table.
                Ok(ReturnValue::Normal(
                    symbol_table
                        .borrow()
                        .lookup_var(other)
                        .ok_or(VMCompileError::UndefinedVariable(other.to_string()))?
                        as isize,
                ))
            }
        }
    }

    fn compile_assignment(
        &mut self,
        assignee: Expr,
        expr: Expr,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        let reg = self
            .compile_expression(assignee, true, true, symbol_table)?
            .safe_unwrap();
        let reassigned = self
            .compile_expression(expr, true, true, symbol_table)?
            .safe_unwrap();
        self.add_instruction(Abc(OP_MOVE, reg as i32, reassigned as i32, 0)); // NOTE: Move is a glorified "clone". Move may be replcaed by a real move, and a clone OP might be added.
        Ok(ReturnValue::Normal(reg))
    }

    fn compile_binary_op(
        &mut self,
        op: BinaryOp,
        left: Expr,
        right: Expr,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        let reg = self.allocate_register();
        let b = self
            .compile_expression(left, false, true, symbol_table)?
            .safe_unwrap() as i32;
        let c = self
            .compile_expression(right, false, true, symbol_table)?
            .safe_unwrap() as i32;
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
        Ok(ReturnValue::Normal(reg as isize))
    }

    fn compile_unary_op(
        &mut self,
        op: UnaryOp,
        expr: Expr,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        let reg = self.allocate_register();
        let b = self
            .compile_expression(expr, false, true, symbol_table)?
            .safe_unwrap() as i32;
        let opcode = match op {
            UnaryOp::LogicalNot => OP_NOT,
            UnaryOp::ArithmeticNegative => todo!(),
            UnaryOp::ArithmeticPositive => todo!(),
            UnaryOp::BitwiseNot => OP_NOT,
        };
        self.add_instruction(Abc(opcode, reg as i32, b, 0));
        Ok(ReturnValue::Normal(reg as isize))
    }

    fn compile_if_expr(
        &mut self,
        condition: Expr,
        then: Vec<Stmt>,
        else_stmt: Option<Vec<Stmt>>,
        require_result: bool,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        // Typical bytecode representation of if expr:
        //      r1 = evaluate condition
        //      jump if r1 is false to else/endif area
        //      then area
        //      jump to endif area
        //      else area
        //      endif area

        // Store the if expr result if required
        let result_register = self.reg_top();
        if require_result {
            self.allocate_register();
        }
        let mut result: isize = 0;

        // Save the top register so that we can disregard the registers from the if expr after use.
        let current_reg_top = self.reg_top();

        // First, get the condition's result and store its register.
        let condition_result = self
            .compile_expression(condition, true, true, symbol_table)?
            .safe_unwrap();

        // Add instruction to jump to else/endif if false
        let jump_to_end_or_else = self.instructions_len(); // Index of the JUMP_IF_ELSE inst
        self.add_instruction(ASBx(OP_JUMP_IF_FALSE, condition_result as i32, 0)); // Placeholder for now

        // Now we compile the 'then' area.
        // Create a new symbol table since we are entering a child scope.
        let child_symbol_table = &Rc::new(RefCell::new(SymbolTable::new_with_parent(
            symbol_table.clone(),
        )));
        for stmt in then {
            result = self
                .compile_statement(stmt, true, true, child_symbol_table)?
                .safe_unwrap();
        }
        if require_result {
            self.add_instruction(Abc(OP_MOVE, result_register as i32, result as i32, 0));
        }

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
                    (jump_to_end - jump_to_end_or_else - 1) as i32,
                ),
            );

            return Ok(ReturnValue::Normal(result_register as isize));
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
            result = self
                .compile_statement(stmt, true, true, child_symbol_table)?
                .safe_unwrap();
        }
        if require_result {
            self.add_instruction(Abc(OP_MOVE, result_register as i32, result as i32, 0));
        }

        // Update jumps
        let end = self.instructions_len();

        // Jump to else stmt
        self.replace_instruction(
            jump_to_end_or_else,
            ASBx(
                OP_JUMP_IF_FALSE,
                condition_result as i32,
                (jump_to_end - jump_to_end_or_else) as i32,
            ),
        );

        // Jump to end from then block
        self.replace_instruction(
            jump_to_end,
            ASBx(OP_JUMP, 0, (end - jump_to_end - 1) as i32),
        );

        // Reset register count back to normal in preparation for endif
        self.manually_change_register_count(current_reg_top);

        Ok(ReturnValue::Normal(result_register as isize))
    }

    fn compile_while_expr(
        &mut self,
        condition: Expr,
        then: Vec<Stmt>,
        require_result: bool,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        // Store the while expr result if required
        let result_register = self.reg_top();
        if require_result {
            self.allocate_register();
        }
        let mut result = ReturnValue::Normal(0);
        // Save current top register to restore later
        let current_reg_top = self.reg_top();

        // Get current position, which is the start of condition evaluation
        let loop_start = self.instructions_len();

        // Compile condition expr
        let condition_result = self
            .compile_expression(condition, true, true, symbol_table)?
            .safe_unwrap();

        // Add instruction to jump to end if condition is false
        let jump_to_end = self.instructions_len();
        self.add_instruction(ASBx(OP_JUMP_IF_FALSE, condition_result as i32, 0)); // Placeholder for now

        // Track break positions to backtrack later
        let mut break_positions: Vec<usize> = Vec::new();

        // Compile loop body
        let child_symbol_table = &Rc::new(RefCell::new(SymbolTable::new_with_parent(
            symbol_table.clone(),
        )));
        for stmt in then {
            result = self.compile_statement(stmt, true, true, child_symbol_table)?;
            if let ReturnValue::Break(inner) = result {
                if require_result {
                    self.add_instruction(Abc(OP_MOVE, result_register as i32, inner as i32, 0));
                }
                let break_pos = self.instructions_len();
                self.add_instruction(ASBx(OP_JUMP, 0, 0)); // Placeholder
                break_positions.push(break_pos);
            }
        }
        if require_result {
            self.add_instruction(Abc(
                OP_MOVE,
                result_register as i32,
                result.safe_unwrap() as i32,
                0,
            ));
        }

        // Add instruction to jump back to the start of the loop
        let jump_back_to_start = self.instructions_len();
        self.add_instruction(ASBx(
            OP_JUMP,
            0,
            loop_start as i32 - jump_back_to_start as i32 - 1,
        ));

        // Update the jump to end instruction with the correct offset
        let loop_end = self.instructions_len();
        self.replace_instruction(
            jump_to_end,
            ASBx(
                OP_JUMP_IF_FALSE,
                condition_result as i32,
                (loop_end - jump_to_end - 1) as i32,
            ),
        );

        // Backpatch break positions to point to the end of the loop
        for break_pos in break_positions {
            self.replace_instruction(
                break_pos,
                ASBx(OP_JUMP, 0, (loop_end - break_pos - 1) as i32),
            );
        }

        // Restore the register count to the state before the loop
        self.manually_change_register_count(current_reg_top);

        Ok(ReturnValue::Normal(result_register as isize))
    }

    fn compile_block_expr(
        &mut self,
        block: Vec<Stmt>,
        require_result: bool,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        // Store the block expr result if required
        let result_register = self.reg_top();
        if require_result {
            self.allocate_register();
        }
        let mut result: isize = 0;
        // Save current top register to restore later
        let current_reg_top = self.reg_top();

        // Compile loop body
        let child_symbol_table = &Rc::new(RefCell::new(SymbolTable::new_with_parent(
            symbol_table.clone(),
        )));
        for stmt in block {
            result = self
                .compile_statement(stmt, true, true, child_symbol_table)?
                .safe_unwrap();
        }
        if require_result {
            self.add_instruction(Abc(OP_MOVE, result_register as i32, result as i32, 0));
        }

        // Restore the register count to the state before the loop
        self.manually_change_register_count(current_reg_top);

        Ok(ReturnValue::Normal(result_register as isize))
    }

    fn compile_forever_loop_expr(
        &mut self,
        block: Vec<Stmt>,
        require_result: bool,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        // Store the forever loop expr result if required
        let result_register = self.reg_top();
        if require_result {
            self.allocate_register();
        }
        // Save current top register to restore later
        let current_reg_top = self.reg_top();

        // Get current position, which is the start of condition evaluation
        let loop_start = self.instructions_len();

        // Track break positions to backtrack later
        let mut break_positions: Vec<usize> = Vec::new();

        // Compile loop body
        let child_symbol_table = &Rc::new(RefCell::new(SymbolTable::new_with_parent(
            symbol_table.clone(),
        )));
        for stmt in block {
            let result = self.compile_statement(stmt, true, true, child_symbol_table)?;
            if let ReturnValue::Break(inner) = result {
                if require_result {
                    self.add_instruction(Abc(OP_MOVE, result_register as i32, inner as i32, 0));
                }
                let break_pos = self.instructions_len();
                self.add_instruction(ASBx(OP_JUMP, 0, 0)); // Placeholder
                break_positions.push(break_pos);
            }
        }

        // Add instruction to jump back to the start of the loop
        let jump_back_to_start = self.instructions_len();
        self.add_instruction(ASBx(
            OP_JUMP,
            0,
            loop_start as i32 - jump_back_to_start as i32 - 1,
        ));

        // Save end of loop pos
        let loop_end = self.instructions_len();

        // Backpatch break positions to point to the end of the loop
        for break_pos in break_positions {
            self.replace_instruction(
                break_pos,
                ASBx(OP_JUMP, 0, (loop_end - break_pos - 1) as i32),
            );
        }

        // Restore the register count to the state before the loop
        self.manually_change_register_count(current_reg_top);

        Ok(ReturnValue::Normal(result_register as isize))
    }
}
