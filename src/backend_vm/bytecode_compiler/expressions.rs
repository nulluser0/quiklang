// Compiler Expressions

use std::{cell::RefCell, rc::Rc};

use crate::{
    backend_vm::{
        instructions::{
            ABx, Abc, OP_ADD, OP_AND, OP_DIV, OP_EQ, OP_GE, OP_GT, OP_LE, OP_LOADBOOL,
            OP_LOADCONST, OP_LOADNULL, OP_LT, OP_MOD, OP_MUL, OP_NE, OP_OR, OP_SUB,
        },
        vm::RegisterVal,
    },
    errors::VMCompileError,
    frontend::ast::{BinaryOp, Expr, Literal},
};

use super::{compiler::Compiler, symbol_tracker::SymbolTable};

impl Compiler {
    pub(super) fn compile_expression(
        &mut self,
        expr: Expr,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<usize, VMCompileError> {
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
            Expr::UnaryOp(_, _) => todo!(),
            Expr::FunctionCall(_, _) => todo!(),
            Expr::Member(_, _) => todo!(),
            Expr::IfExpr {
                condition,
                then,
                else_stmt,
            } => todo!(),
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

    fn compile_literal(&mut self, literal: Literal) -> Result<usize, VMCompileError> {
        let constant = match literal {
            Literal::Integer(integer) => RegisterVal::Int(integer),
            Literal::Float(float) => RegisterVal::Float(float),
            Literal::String(string) => RegisterVal::Str(string.into()),
            Literal::Object(_) => todo!(),
        };
        let index = self.add_constant(constant);
        let reg = self.allocate_register();
        self.add_instruction(ABx(OP_LOADCONST, reg as i32, index as i32));

        Ok(reg)
    }

    fn compile_identifier(
        &mut self,
        identifier: String,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<usize, VMCompileError> {
        match identifier.as_str() {
            "null" => {
                let reg = self.allocate_register();
                self.add_instruction(Abc(OP_LOADNULL, reg as i32, reg as i32, 0));
                Ok(reg)
            }
            "true" => {
                let reg = self.allocate_register();
                self.add_instruction(Abc(OP_LOADBOOL, reg as i32, 1, 0));
                Ok(reg)
            }
            "false" => {
                let reg = self.allocate_register();
                self.add_instruction(Abc(OP_LOADBOOL, reg as i32, 0, 0));
                Ok(reg)
            }
            other => {
                // No special ident_keyword. Instead, match symbol table.
                symbol_table
                    .borrow()
                    .lookup_var(other)
                    .ok_or(VMCompileError::UndefinedVariable(other.to_string()))
            }
        }
    }

    fn compile_binary_op(
        &mut self,
        op: BinaryOp,
        left: Expr,
        right: Expr,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<usize, VMCompileError> {
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
        Ok(reg)
    }
}
