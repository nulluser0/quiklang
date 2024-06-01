// Interpreter

use crate::frontend::ast::Stmt;

use super::environment::Environment;
use super::eval::expressions::evaluate_expr;
use super::eval::statements::evaluate_declare_var;
use super::values::Val;

pub fn evaluate(stmt: Stmt, env: &mut Environment) -> Val {
    match stmt {
        Stmt::ExprStmt(expr) => evaluate_expr(expr, env),
        Stmt::DeclareStmt(name, is_mutable, expr) => {
            evaluate_declare_var(name, is_mutable, expr, env)
        }
        Stmt::ReturnStmt(_) => unimplemented!(),
        Stmt::IfStmt(_, _, _) => unimplemented!(),
        Stmt::FunctionDeclaration(parameters, name, body, is_async) => todo!(),
        // Handle other statement types...
    }
}
