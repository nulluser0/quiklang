// Compiler Expressions

use crate::{backend_vm::bytecode::ByteCode, errors::VMCompileError, frontend::ast::Expr};

use super::compiler::Compiler;

impl Compiler {
    pub(super) fn compile_expression(
        bytecode: &mut ByteCode,
        expr: Expr,
    ) -> Result<(), VMCompileError> {
        match expr {
            Expr::Literal(_) => todo!(),
            Expr::Array(_, _) => todo!(),
            Expr::Identifier(_) => todo!(),
            Expr::Tuple(_) => todo!(),
            Expr::AssignmentExpr { assignee, expr } => todo!(),
            Expr::ConcatOp { left, right } => todo!(),
            Expr::BinaryOp { op, left, right } => todo!(),
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
}
