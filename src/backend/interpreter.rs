// Interpreter

use crate::frontend::ast::Stmt;

use super::environment::Environment;
use super::eval::expressions::evaluate_expr;
use super::eval::statements::evaluate_declare_var;
use super::values::Val;

pub fn evaluate(stmt: Stmt, env: &mut Environment) -> Val {
    match stmt {
        Stmt::ExprStmt(expr) => evaluate_expr(expr, env),
        Stmt::AssignStmt(name, is_const, is_mutable, expr) => evaluate_declare_var(name, is_const, is_mutable, expr, env),
        Stmt::ReturnStmt(_) => unimplemented!(),
        Stmt::IfStmt(_, _, _) => unimplemented!(),
        // Handle other statement types...
    }
}