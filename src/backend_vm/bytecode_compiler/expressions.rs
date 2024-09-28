// Compiler Expressions

use std::{cell::RefCell, rc::Rc};

use crate::{
    backend_vm::instructions::{
        rk_ask, ABx, ASBx, Abc, OP_BITNOT, OP_CALL, OP_CLONE, OP_CONCAT, OP_DROP, OP_FLOAT_ADD,
        OP_FLOAT_DIV, OP_FLOAT_EQ, OP_FLOAT_GE, OP_FLOAT_GT, OP_FLOAT_LE, OP_FLOAT_LT,
        OP_FLOAT_MOD, OP_FLOAT_MUL, OP_FLOAT_NE, OP_FLOAT_NEG, OP_FLOAT_POSITIVE, OP_FLOAT_SUB,
        OP_FLOAT_TO_INT, OP_FLOAT_TO_STRING, OP_INT_ADD, OP_INT_DIV, OP_INT_EQ, OP_INT_GE,
        OP_INT_GT, OP_INT_LE, OP_INT_LT, OP_INT_MOD, OP_INT_MUL, OP_INT_NE, OP_INT_NEG,
        OP_INT_POSITIVE, OP_INT_SUB, OP_INT_TO_FLOAT, OP_INT_TO_STRING, OP_JUMP, OP_JUMP_IF_FALSE,
        OP_LOADBOOL, OP_LOADCONST, OP_LOADNULL, OP_LOGICAL_AND, OP_LOGICAL_NOT, OP_LOGICAL_OR,
        OP_MOVE, OP_NATIVE_CALL,
    },
    errors::VMCompileError,
    frontend::ast::{BinaryOp, Expr, Literal, Stmt, Type, UnaryOp},
};

use super::{
    compiler::{Compiler, ReturnValue, TaggedConstantValue},
    symbol_tracker::{SymbolTable, SymbolTableType},
    type_table::TypeTable,
};

// To shut up the clippy linter
struct IfArgs<'a> {
    condition: Expr,
    then: Vec<Stmt>,
    else_stmt: Option<Vec<Stmt>>,
    require_result: bool,
    fn_return: Option<SymbolTableType>,
    symbol_table: &'a Rc<RefCell<SymbolTable>>,
    type_table: &'a Rc<RefCell<TypeTable>>,
}

struct CompileBinaryOpArgs<'a> {
    op: BinaryOp,
    left: Expr,
    right: Expr,
    left_type: Type,
    right_type: Type,
    output_type: Type,
    symbol_table: &'a Rc<RefCell<SymbolTable>>,
    type_table: &'a Rc<RefCell<TypeTable>>,
}

impl Compiler {
    pub(super) fn compile_expression(
        &mut self,
        expr: Expr,
        require_constant_as_register: bool, // true = allocates register to a constant.
        require_result: bool, // true = certain exprs will return a result, like ifs, loops, fors, whiles, blocks.
        fn_return: Option<SymbolTableType>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        type_table: &Rc<RefCell<TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        match expr {
            Expr::Literal(literal) => self.compile_literal(literal, require_constant_as_register),
            Expr::Array(_, _) => todo!(),
            Expr::Range {
                start,
                end,
                inclusive,
                defined_type,
            } => todo!(),
            Expr::Identifier(identifier) => {
                self.compile_identifier(identifier, symbol_table, require_constant_as_register)
            }
            Expr::Tuple(_) => todo!(),
            Expr::AssignmentExpr { assignee, expr } => {
                self.compile_assignment(*assignee, *expr, symbol_table, type_table)
            }
            Expr::ConcatOp {
                left,
                right,
                defined_type,
            } => self.compile_concat_op(*left, *right, defined_type, symbol_table, type_table),
            Expr::BinaryOp {
                op,
                left,
                right,
                left_type,
                right_type,
                output_type,
            } => self.compile_binary_op(CompileBinaryOpArgs {
                op,
                left: *left,
                right: *right,
                left_type,
                right_type,
                output_type,
                symbol_table,
                type_table,
            }),
            Expr::UnaryOp(op, expr, output_type) => {
                self.compile_unary_op(op, *expr, output_type, symbol_table, type_table)
            }
            Expr::FunctionCall(args, caller, return_type) => {
                self.compile_function_call(args, *caller, return_type, symbol_table, type_table)
            }
            Expr::Member(_, _) => todo!(),
            Expr::IfExpr {
                condition,
                then,
                else_stmt,
            } => self.compile_if_expr(IfArgs {
                condition: *condition,
                then,
                else_stmt,
                require_result,
                fn_return,
                symbol_table,
                type_table,
            }),
            Expr::ForExpr {
                identifier,
                iterable,
                then,
            } => todo!(),
            Expr::WhileExpr { condition, then } => self.compile_while_expr(
                *condition,
                then,
                require_result,
                fn_return,
                symbol_table,
                type_table,
            ),
            Expr::BlockExpr(block) => {
                self.compile_block_expr(block, require_result, fn_return, symbol_table, type_table)
            }
            Expr::ForeverLoopExpr(block) => self.compile_forever_loop_expr(
                block,
                require_result,
                fn_return,
                symbol_table,
                type_table,
            ),
            Expr::SpecialNull => self.compile_identifier(
                "null".to_string(),
                symbol_table,
                require_constant_as_register,
            ),
            Expr::StructLiteral(_, _) => todo!(),
            Expr::EnumLiteral(_, _, _) => todo!(),
            Expr::TypeCast(expr, expr_type, cast_into_type) => self.compile_type_cast(
                *expr,
                expr_type,
                cast_into_type,
                require_constant_as_register,
                symbol_table,
                type_table,
            ),
            Expr::Await(_, _) => todo!(),
            Expr::Clone(expr, return_type) => {
                todo!()
            }
        }
    }

    fn compile_literal(
        &mut self,
        literal: Literal,
        require_constant_as_register: bool,
    ) -> Result<ReturnValue, VMCompileError> {
        let constant = match literal {
            Literal::Integer(integer) => &TaggedConstantValue::Int(integer),
            Literal::Float(float) => &TaggedConstantValue::Float(float),
            Literal::String(string) => &TaggedConstantValue::Str(string),
            Literal::Object(_) => todo!(),
        };
        let index = self.add_constant(constant.clone());

        if require_constant_as_register {
            let reg = self.allocate_register();
            self.add_instruction(ABx(OP_LOADCONST, reg as i32, index as i32));
            match constant {
                TaggedConstantValue::Str(_) => {
                    return Ok(ReturnValue::Normal(SymbolTableType::HeapAllocated(
                        reg as isize,
                    )));
                }
                _ => {
                    return Ok(ReturnValue::Normal(SymbolTableType::Primitive(
                        reg as isize,
                    )));
                }
            }
        }

        match constant {
            TaggedConstantValue::Str(_) => Ok(ReturnValue::Normal(SymbolTableType::HeapAllocated(
                (rk_ask(index as i32)) as isize,
            ))),
            _ => Ok(ReturnValue::Normal(SymbolTableType::Primitive(
                (rk_ask(index as i32)) as isize,
            ))),
        }
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
                    return Ok(ReturnValue::Normal(SymbolTableType::Primitive(
                        reg as isize,
                    )));
                }
                let index = self.add_constant(TaggedConstantValue::Null);
                Ok(ReturnValue::Normal(SymbolTableType::Primitive(
                    (rk_ask(index as i32)) as isize,
                )))
            }
            "true" => {
                if require_constant_as_register {
                    let reg = self.allocate_register();
                    self.add_instruction(Abc(OP_LOADBOOL, reg as i32, 1, 0));
                    return Ok(ReturnValue::Normal(SymbolTableType::Primitive(
                        reg as isize,
                    )));
                }
                let index = self.add_constant(TaggedConstantValue::Bool(true));
                Ok(ReturnValue::Normal(SymbolTableType::Primitive(
                    (rk_ask(index as i32)) as isize,
                )))
            }
            "false" => {
                if require_constant_as_register {
                    let reg = self.allocate_register();
                    self.add_instruction(Abc(OP_LOADBOOL, reg as i32, 0, 0));
                    return Ok(ReturnValue::Normal(SymbolTableType::Primitive(
                        reg as isize,
                    )));
                }
                let index = self.add_constant(TaggedConstantValue::Bool(false));
                Ok(ReturnValue::Normal(SymbolTableType::Primitive(
                    (rk_ask(index as i32)) as isize,
                )))
            }
            other => {
                // No special ident_keyword. Instead, match symbol table.
                // TODO: manage whether to clone or give a reference.
                Ok(ReturnValue::Normal(
                    symbol_table
                        .borrow()
                        .lookup_var(other)
                        .ok_or(VMCompileError::UndefinedVariable(other.to_string()))?,
                ))
            }
        }
    }

    fn compile_assignment(
        &mut self,
        assignee: Expr,
        expr: Expr,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        type_table: &Rc<RefCell<TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        let reg = self
            .compile_expression(assignee, true, true, None, symbol_table, type_table)?
            .safe_unwrap();
        let reassigned = self
            .compile_expression(expr, true, true, None, symbol_table, type_table)?
            .safe_unwrap();
        self.add_instruction(Abc(
            OP_MOVE,
            reg.safe_unwrap() as i32,
            reassigned.safe_unwrap() as i32,
            0,
        ));
        Ok(ReturnValue::Normal(reg))
    }

    fn compile_concat_op(
        &mut self,
        left: Expr,
        right: Expr,
        defined_type: Type,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        type_table: &Rc<RefCell<TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        let reg = self.allocate_register();
        let b = self
            .compile_expression(left, false, true, None, symbol_table, type_table)?
            .safe_unwrap()
            .safe_unwrap() as i32;
        let c = self
            .compile_expression(right, false, true, None, symbol_table, type_table)?
            .safe_unwrap()
            .safe_unwrap() as i32;
        match defined_type {
            Type::String => self.add_instruction(Abc(OP_CONCAT, reg as i32, b, c)),
            e => {
                // TODO: Handle other types when (custom) types can implement their own binary op traits.
                return Err(VMCompileError::UndefinedType(format!(
                    "Concat OP type mismatch. {:?}",
                    e
                )));
            }
        }
        Ok(ReturnValue::Normal(SymbolTableType::HeapAllocated(
            reg as isize,
        )))
    }

    fn compile_binary_op(
        &mut self,
        CompileBinaryOpArgs {
            op,
            left,
            right,
            left_type,
            right_type,
            output_type,
            symbol_table,
            type_table,
        }: CompileBinaryOpArgs,
    ) -> Result<ReturnValue, VMCompileError> {
        let reg = self.allocate_register();
        let b = self
            .compile_expression(left, false, true, None, symbol_table, type_table)?
            .safe_unwrap()
            .safe_unwrap() as i32;
        let c = self
            .compile_expression(right, false, true, None, symbol_table, type_table)?
            .safe_unwrap()
            .safe_unwrap() as i32;
        let opcode = match output_type {
            Type::Integer => match op {
                BinaryOp::Add => OP_INT_ADD,
                BinaryOp::Subtract => OP_INT_SUB,
                BinaryOp::Multiply => OP_INT_MUL,
                BinaryOp::Divide => OP_INT_DIV,
                BinaryOp::Modulus => OP_INT_MOD,
                _ => {
                    return Err(VMCompileError::UndefinedType(
                        "Binary OP type mismatch. Relational Op with a non bool result."
                            .to_string(),
                    ))
                }
            },
            Type::Float => {
                if left_type == Type::Integer {
                    // Convert left to float
                    self.add_instruction(Abc(OP_INT_TO_FLOAT, b, b, 0));
                }

                if right_type == Type::Integer {
                    // Convert right to float
                    self.add_instruction(Abc(OP_INT_TO_FLOAT, c, c, 0));
                }

                match op {
                    BinaryOp::Add => OP_FLOAT_ADD,
                    BinaryOp::Subtract => OP_FLOAT_SUB,
                    BinaryOp::Multiply => OP_FLOAT_MUL,
                    BinaryOp::Divide => OP_FLOAT_DIV,
                    BinaryOp::GreaterThan => OP_FLOAT_GT,
                    BinaryOp::LessThan => OP_FLOAT_LT,
                    BinaryOp::GreaterOrEqual => OP_FLOAT_GE,
                    BinaryOp::LessOrEqual => OP_FLOAT_LE,
                    BinaryOp::Equal => OP_FLOAT_EQ,
                    BinaryOp::NotEqual => OP_FLOAT_NE,
                    BinaryOp::And => OP_LOGICAL_AND,
                    BinaryOp::Or => OP_LOGICAL_OR,
                    BinaryOp::Modulus => OP_FLOAT_MOD,
                }
            }
            Type::Bool => match (left_type, right_type) {
                (Type::Bool, Type::Bool) => match op {
                    BinaryOp::And => OP_LOGICAL_AND,
                    BinaryOp::Or => OP_LOGICAL_OR,
                    e => {
                        return Err(VMCompileError::UndefinedType(format!(
                        "Binary OP type mismatch. AND or OR operator with incompatible types {:?}.",
                        e
                    )))
                    }
                },
                (Type::Integer, Type::Integer) => match op {
                    BinaryOp::GreaterThan => OP_INT_GT,
                    BinaryOp::LessThan => OP_INT_LT,
                    BinaryOp::GreaterOrEqual => OP_INT_GE,
                    BinaryOp::LessOrEqual => OP_INT_LE,
                    BinaryOp::Equal => OP_INT_EQ,
                    BinaryOp::NotEqual => OP_INT_NE,
                    e => {
                        return Err(VMCompileError::UndefinedType(format!(
                            "Binary OP type mismatch. Relational Op with a non bool result. {:?}",
                            e
                        )))
                    }
                },
                (Type::Float, Type::Float) => match op {
                    BinaryOp::GreaterThan => OP_FLOAT_GT,
                    BinaryOp::LessThan => OP_FLOAT_LT,
                    BinaryOp::GreaterOrEqual => OP_FLOAT_GE,
                    BinaryOp::LessOrEqual => OP_FLOAT_LE,
                    BinaryOp::Equal => OP_FLOAT_EQ,
                    BinaryOp::NotEqual => OP_FLOAT_NE,
                    e => {
                        return Err(VMCompileError::UndefinedType(format!(
                            "Binary OP type mismatch. Relational Op with a non bool result. {:?}",
                            e
                        )))
                    }
                },
                e => {
                    return Err(VMCompileError::UndefinedType(format!(
                        "Binary OP type mismatch. {:?}",
                        e
                    )))
                }
            },
            e => {
                // TODO: Handle other types when (custom) types can implement their own binary op traits.
                return Err(VMCompileError::UndefinedType(format!(
                    "Binary OP type mismatch. {:?}",
                    e
                )));
            }
        };
        self.add_instruction(Abc(opcode, reg as i32, b, c));
        Ok(ReturnValue::Normal(SymbolTableType::Primitive(
            reg as isize,
        )))
    }

    fn compile_unary_op(
        &mut self,
        op: UnaryOp,
        expr: Expr,
        output_type: Type,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        type_table: &Rc<RefCell<TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        let reg = self.allocate_register();
        let b = self
            .compile_expression(expr.clone(), false, true, None, symbol_table, type_table)?
            .safe_unwrap()
            .safe_unwrap() as i32;
        let opcode = match op {
            UnaryOp::LogicalNot => OP_LOGICAL_NOT,
            UnaryOp::ArithmeticNegative => match output_type {
                Type::Integer => OP_INT_NEG,
                Type::Float => OP_FLOAT_NEG,
                e => {
                    // TODO: Handle other types when (custom) types can implement their own unary op traits.
                    return Err(VMCompileError::UndefinedType(format!(
                        "Unary OP type mismatch. Arithmetic Negative. {:?}",
                        e
                    )));
                }
            },
            UnaryOp::ArithmeticPositive => match output_type {
                Type::Integer => OP_INT_POSITIVE,
                Type::Float => OP_FLOAT_POSITIVE,
                e => {
                    // TODO: Handle other types when (custom) types can implement their own unary op traits.
                    return Err(VMCompileError::UndefinedType(format!(
                        "Unary OP type mismatch. Arithmetic Positive. {:?}",
                        e
                    )));
                }
            },
            UnaryOp::BitwiseNot => OP_BITNOT,
        };
        self.add_instruction(Abc(opcode, reg as i32, b, 0));

        Ok(ReturnValue::Normal(SymbolTableType::Primitive(
            reg as isize,
        )))
    }

    fn compile_function_call(
        &mut self,
        args: Vec<(Expr, bool)>,
        caller: Expr,
        return_type: Type,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        type_table: &Rc<RefCell<TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        // We assume this is correct
        let function =
            self.compile_expression(caller, false, true, None, symbol_table, type_table)?;

        let mut native_fn = -1;

        if let SymbolTableType::QFFIFunction(val) = function.safe_unwrap() {
            native_fn = val
        }

        // Function result (and function run base)
        let result = self.allocate_register();

        // Args lens
        let arg_lens = args.len() as i32;

        // Allocate and compile arguments
        for arg in args {
            let arg_reg = self.reg_top();
            let reg = self
                .compile_expression(arg.0, true, true, None, symbol_table, type_table)?
                .safe_unwrap();
            if arg_reg as isize != reg.safe_unwrap() {
                self.add_instruction(Abc(OP_MOVE, arg_reg as i32, reg.safe_unwrap() as i32, 0))
            }
            self.allocate_register();
        }

        // Call function
        if native_fn != -1 {
            self.add_instruction(Abc(
                OP_NATIVE_CALL,
                native_fn as i32,
                arg_lens,
                result as i32,
            ));
            match return_type {
                Type::String | Type::Array(_) | Type::Range(_) => {
                    return Ok(ReturnValue::Normal(SymbolTableType::HeapAllocated(
                        result as isize,
                    )));
                }
                _ => {
                    return Ok(ReturnValue::Normal(SymbolTableType::Primitive(
                        result as isize,
                    )));
                }
            }
        }

        self.add_instruction(Abc(
            OP_CALL,
            function.safe_unwrap().safe_unwrap() as i32,
            arg_lens,
            result as i32,
        ));

        match return_type {
            Type::String | Type::Array(_) | Type::Range(_) => Ok(ReturnValue::Normal(
                SymbolTableType::HeapAllocated(result as isize),
            )),
            _ => Ok(ReturnValue::Normal(SymbolTableType::Primitive(
                result as isize,
            ))),
        }
    }

    fn compile_if_expr(
        &mut self,
        IfArgs {
            condition,
            then,
            else_stmt,
            require_result,
            fn_return,
            symbol_table,
            type_table,
        }: IfArgs,
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
        // let mut result: isize = 0;

        // Save the top register so that we can disregard the registers from the if expr after use.
        let current_reg_top = self.reg_top();

        // First, get the condition's result and store its register.
        let condition_result = self
            .compile_expression(condition, true, true, None, symbol_table, type_table)?
            .safe_unwrap();

        // Add instruction to jump to else/endif if false
        let jump_to_end_or_else = self.instructions_len(); // Index of the JUMP_IF_ELSE inst
        self.add_instruction(ASBx(
            OP_JUMP_IF_FALSE,
            condition_result.safe_unwrap() as i32,
            0,
        )); // Placeholder for now

        // Now we compile the 'then' area.
        // Create a new symbol table since we are entering a child scope.
        let child_symbol_table = &Rc::new(RefCell::new(SymbolTable::new_with_parent(
            symbol_table.clone(),
        )));

        // Track the result of the then block
        let mut result = ReturnValue::Normal(SymbolTableType::Primitive(0));
        for stmt in then {
            result = self.compile_statement(
                stmt,
                true,
                true,
                fn_return,
                child_symbol_table,
                type_table,
            )?;
        }

        // Drop heap allocated registers
        for (_, reg) in child_symbol_table.borrow().vars.iter() {
            if let SymbolTableType::HeapAllocated(_) = reg {
                self.add_instruction(Abc(OP_DROP, reg.safe_unwrap() as i32, 0, 0));
            }
        }

        if require_result && !result.is_return() {
            self.add_instruction(Abc(
                OP_MOVE,
                result_register as i32,
                result.safe_unwrap().safe_unwrap() as i32,
                0,
            ));
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
                    condition_result.safe_unwrap() as i32,
                    (jump_to_end - jump_to_end_or_else - 1) as i32,
                ),
            );

            return match result {
                ReturnValue::Normal(_) => Ok(ReturnValue::Normal(match result.safe_unwrap() {
                    SymbolTableType::QFFIFunction(_) => {
                        SymbolTableType::QFFIFunction(result_register as isize)
                    }
                    SymbolTableType::Function(_) => {
                        SymbolTableType::Function(result_register as isize)
                    }
                    SymbolTableType::Primitive(_) => {
                        SymbolTableType::Primitive(result_register as isize)
                    }
                    SymbolTableType::HeapAllocated(_) => {
                        SymbolTableType::HeapAllocated(result_register as isize)
                    }
                })),
                ReturnValue::Break(_) => Ok(ReturnValue::Break(match result.safe_unwrap() {
                    SymbolTableType::QFFIFunction(_) => {
                        SymbolTableType::QFFIFunction(result_register as isize)
                    }
                    SymbolTableType::Function(_) => {
                        SymbolTableType::Function(result_register as isize)
                    }
                    SymbolTableType::Primitive(_) => {
                        SymbolTableType::Primitive(result_register as isize)
                    }
                    SymbolTableType::HeapAllocated(_) => {
                        SymbolTableType::HeapAllocated(result_register as isize)
                    }
                })),
                ReturnValue::Return(_) => Ok(ReturnValue::Return(match result.safe_unwrap() {
                    SymbolTableType::QFFIFunction(_) => {
                        SymbolTableType::QFFIFunction(result_register as isize)
                    }
                    SymbolTableType::Function(_) => {
                        SymbolTableType::Function(result_register as isize)
                    }
                    SymbolTableType::Primitive(_) => {
                        SymbolTableType::Primitive(result_register as isize)
                    }
                    SymbolTableType::HeapAllocated(_) => {
                        SymbolTableType::HeapAllocated(result_register as isize)
                    }
                })),
            };
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
            result = self.compile_statement(
                stmt,
                true,
                true,
                fn_return,
                child_symbol_table,
                type_table,
            )?;
        }

        // Drop heap allocated registers
        for (_, reg) in child_symbol_table.borrow().vars.iter() {
            if let SymbolTableType::HeapAllocated(_) = reg {
                self.add_instruction(Abc(OP_DROP, reg.safe_unwrap() as i32, 0, 0));
            }
        }

        if require_result && !result.is_return() {
            self.add_instruction(Abc(
                OP_MOVE,
                result_register as i32,
                result.safe_unwrap().safe_unwrap() as i32,
                0,
            ));
        }

        // Update jumps
        let end = self.instructions_len();

        // Jump to else stmt
        self.replace_instruction(
            jump_to_end_or_else,
            ASBx(
                OP_JUMP_IF_FALSE,
                condition_result.safe_unwrap() as i32,
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

        match result {
            ReturnValue::Normal(_) => Ok(ReturnValue::Normal(match result.safe_unwrap() {
                SymbolTableType::QFFIFunction(_) => {
                    SymbolTableType::QFFIFunction(result_register as isize)
                }
                SymbolTableType::Function(_) => SymbolTableType::Function(result_register as isize),
                SymbolTableType::Primitive(_) => {
                    SymbolTableType::Primitive(result_register as isize)
                }
                SymbolTableType::HeapAllocated(_) => {
                    SymbolTableType::HeapAllocated(result_register as isize)
                }
            })),
            ReturnValue::Break(_) => Ok(ReturnValue::Break(match result.safe_unwrap() {
                SymbolTableType::QFFIFunction(_) => {
                    SymbolTableType::QFFIFunction(result_register as isize)
                }
                SymbolTableType::Function(_) => SymbolTableType::Function(result_register as isize),
                SymbolTableType::Primitive(_) => {
                    SymbolTableType::Primitive(result_register as isize)
                }
                SymbolTableType::HeapAllocated(_) => {
                    SymbolTableType::HeapAllocated(result_register as isize)
                }
            })),
            ReturnValue::Return(_) => Ok(ReturnValue::Return(match result.safe_unwrap() {
                SymbolTableType::QFFIFunction(_) => {
                    SymbolTableType::QFFIFunction(result_register as isize)
                }
                SymbolTableType::Function(_) => SymbolTableType::Function(result_register as isize),
                SymbolTableType::Primitive(_) => {
                    SymbolTableType::Primitive(result_register as isize)
                }
                SymbolTableType::HeapAllocated(_) => {
                    SymbolTableType::HeapAllocated(result_register as isize)
                }
            })),
        }
    }

    fn compile_while_expr(
        &mut self,
        condition: Expr,
        then: Vec<Stmt>,
        require_result: bool,
        fn_return: Option<SymbolTableType>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        type_table: &Rc<RefCell<TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        // Store the while expr result if required
        let result_register = self.reg_top();
        if require_result {
            self.allocate_register();
        }
        let mut result = ReturnValue::Normal(SymbolTableType::Primitive(0));
        // Save current top register to restore later
        let current_reg_top = self.reg_top();

        // Get current position, which is the start of condition evaluation
        let loop_start = self.instructions_len();

        // Compile condition expr
        let condition_result = self
            .compile_expression(condition, true, true, None, symbol_table, type_table)?
            .safe_unwrap();

        // Add instruction to jump to end if condition is false
        let jump_to_end = self.instructions_len();
        self.add_instruction(ASBx(
            OP_JUMP_IF_FALSE,
            condition_result.safe_unwrap() as i32,
            0,
        )); // Placeholder for now

        // Track break positions to backtrack later
        let mut break_positions: Vec<usize> = Vec::new();

        // Compile loop body
        let child_symbol_table = &Rc::new(RefCell::new(SymbolTable::new_with_parent(
            symbol_table.clone(),
        )));
        for stmt in then {
            result = self.compile_statement(
                stmt,
                true,
                true,
                fn_return,
                child_symbol_table,
                type_table,
            )?;
            if let ReturnValue::Break(inner) = result {
                if require_result {
                    self.add_instruction(Abc(
                        OP_MOVE,
                        result_register as i32,
                        inner.safe_unwrap() as i32,
                        0,
                    ));
                }
                let break_pos = self.instructions_len();
                self.add_instruction(ASBx(OP_JUMP, 0, 0)); // Placeholder
                break_positions.push(break_pos);
            }
        }

        // Drop heap allocated registers
        for (_, reg) in child_symbol_table.borrow().vars.iter() {
            if let SymbolTableType::HeapAllocated(_) = reg {
                self.add_instruction(Abc(OP_DROP, reg.safe_unwrap() as i32, 0, 0));
            }
        }

        if require_result {
            self.add_instruction(Abc(
                OP_MOVE,
                result_register as i32,
                result.safe_unwrap().safe_unwrap() as i32,
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
                condition_result.safe_unwrap() as i32,
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

        Ok(ReturnValue::Normal(match result.safe_unwrap() {
            SymbolTableType::QFFIFunction(_) => {
                SymbolTableType::QFFIFunction(result_register as isize)
            }
            SymbolTableType::Function(_) => SymbolTableType::Function(result_register as isize),
            SymbolTableType::Primitive(_) => SymbolTableType::Primitive(result_register as isize),
            SymbolTableType::HeapAllocated(_) => {
                SymbolTableType::HeapAllocated(result_register as isize)
            }
        }))
    }

    fn compile_block_expr(
        &mut self,
        block: Vec<Stmt>,
        require_result: bool,
        fn_return: Option<SymbolTableType>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        type_table: &Rc<RefCell<TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        // Store the block expr result if required
        let result_register = self.reg_top();
        if require_result {
            self.allocate_register();
        }
        let mut result = ReturnValue::Normal(SymbolTableType::Primitive(0));
        // Save current top register to restore later
        let current_reg_top = self.reg_top();

        // Compile loop body
        let child_symbol_table = &Rc::new(RefCell::new(SymbolTable::new_with_parent(
            symbol_table.clone(),
        )));
        for stmt in block {
            result = self.compile_statement(
                stmt,
                true,
                true,
                fn_return,
                child_symbol_table,
                type_table,
            )?;
        }
        // Drop heap allocated registers
        for (_, reg) in child_symbol_table.borrow().vars.iter() {
            if let SymbolTableType::HeapAllocated(_) = reg {
                self.add_instruction(Abc(OP_DROP, reg.safe_unwrap() as i32, 0, 0));
            }
        }
        if require_result {
            self.add_instruction(Abc(
                OP_MOVE,
                result_register as i32,
                result.safe_unwrap().safe_unwrap() as i32,
                0,
            ));
        }

        // Restore the register count to the state before the loop
        self.manually_change_register_count(current_reg_top);

        match result {
            ReturnValue::Normal(_) => Ok(ReturnValue::Normal(match result.safe_unwrap() {
                SymbolTableType::QFFIFunction(_) => {
                    SymbolTableType::QFFIFunction(result_register as isize)
                }
                SymbolTableType::Function(_) => SymbolTableType::Function(result_register as isize),
                SymbolTableType::Primitive(_) => {
                    SymbolTableType::Primitive(result_register as isize)
                }
                SymbolTableType::HeapAllocated(_) => {
                    SymbolTableType::HeapAllocated(result_register as isize)
                }
            })),
            ReturnValue::Break(_) => Ok(ReturnValue::Break(match result.safe_unwrap() {
                SymbolTableType::QFFIFunction(_) => {
                    SymbolTableType::QFFIFunction(result_register as isize)
                }
                SymbolTableType::Function(_) => SymbolTableType::Function(result_register as isize),
                SymbolTableType::Primitive(_) => {
                    SymbolTableType::Primitive(result_register as isize)
                }
                SymbolTableType::HeapAllocated(_) => {
                    SymbolTableType::HeapAllocated(result_register as isize)
                }
            })),
            ReturnValue::Return(_) => Ok(ReturnValue::Return(match result.safe_unwrap() {
                SymbolTableType::QFFIFunction(_) => {
                    SymbolTableType::QFFIFunction(result_register as isize)
                }
                SymbolTableType::Function(_) => SymbolTableType::Function(result_register as isize),
                SymbolTableType::Primitive(_) => {
                    SymbolTableType::Primitive(result_register as isize)
                }
                SymbolTableType::HeapAllocated(_) => {
                    SymbolTableType::HeapAllocated(result_register as isize)
                }
            })),
        }
    }

    fn compile_forever_loop_expr(
        &mut self,
        block: Vec<Stmt>,
        require_result: bool,
        fn_return: Option<SymbolTableType>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        type_table: &Rc<RefCell<TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        // Store the forever loop expr result if required
        let result_register = self.reg_top();
        if require_result {
            self.allocate_register();
        }
        let mut result = ReturnValue::Normal(SymbolTableType::Primitive(0));

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
            result = self.compile_statement(
                stmt,
                true,
                true,
                fn_return,
                child_symbol_table,
                type_table,
            )?;
            if let ReturnValue::Break(inner) = result {
                if require_result {
                    self.add_instruction(Abc(
                        OP_MOVE,
                        result_register as i32,
                        inner.safe_unwrap() as i32,
                        0,
                    ));
                }
                let break_pos = self.instructions_len();
                self.add_instruction(ASBx(OP_JUMP, 0, 0)); // Placeholder
                break_positions.push(break_pos);
            }
        }

        // Drop heap allocated registers
        for (_, reg) in child_symbol_table.borrow().vars.iter() {
            if let SymbolTableType::HeapAllocated(_) = reg {
                self.add_instruction(Abc(OP_DROP, reg.safe_unwrap() as i32, 0, 0));
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

        Ok(ReturnValue::Normal(match result.safe_unwrap() {
            SymbolTableType::QFFIFunction(_) => {
                SymbolTableType::QFFIFunction(result_register as isize)
            }
            SymbolTableType::Function(_) => SymbolTableType::Function(result_register as isize),
            SymbolTableType::Primitive(_) => SymbolTableType::Primitive(result_register as isize),
            SymbolTableType::HeapAllocated(_) => {
                SymbolTableType::HeapAllocated(result_register as isize)
            }
        }))
    }

    fn compile_type_cast(
        &mut self,
        expr: Expr,
        expr_type: Type,
        cast_into_type: Type,
        require_constant_as_register: bool,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        type_table: &Rc<RefCell<TypeTable>>,
    ) -> Result<ReturnValue, VMCompileError> {
        let _ = require_constant_as_register;
        let reg = self.allocate_register();
        let b = self
            .compile_expression(expr, false, true, None, symbol_table, type_table)?
            .safe_unwrap()
            .safe_unwrap() as i32;
        match expr_type {
            Type::Integer => match cast_into_type {
                Type::Float => {
                    self.add_instruction(Abc(OP_INT_TO_FLOAT, reg as i32, b, 0));
                }
                Type::String => {
                    self.add_instruction(Abc(OP_INT_TO_STRING, reg as i32, b, 0));
                }
                e => {
                    return Err(VMCompileError::UndefinedType(format!(
                        "Type cast from integer to {:?} is not supported.",
                        e
                    )));
                }
            },
            Type::Float => match cast_into_type {
                Type::Integer => {
                    self.add_instruction(Abc(OP_FLOAT_TO_INT, reg as i32, b, 0));
                }
                Type::String => {
                    self.add_instruction(Abc(OP_FLOAT_TO_STRING, reg as i32, b, 0));
                }
                e => {
                    return Err(VMCompileError::UndefinedType(format!(
                        "Type cast from float to {:?} is not supported.",
                        e
                    )));
                }
            },
            e => {
                return Err(VMCompileError::UndefinedType(format!(
                    "Type cast from {:?} is not supported.",
                    e
                )));
            }
        }
        Ok(ReturnValue::Normal(match cast_into_type {
            Type::String => SymbolTableType::HeapAllocated(reg as isize),
            _ => SymbolTableType::Primitive(reg as isize),
        }))
    }
}
