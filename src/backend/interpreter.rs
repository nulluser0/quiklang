// Interpreter

use std::cell::RefCell;
use std::rc::Rc;

use crate::frontend::ast::Stmt;

use super::environment::Environment;
use super::eval::expressions::evaluate_expr;
use super::eval::statements::{
    evaluate_break_stmt, evaluate_declare_fn, evaluate_declare_var, evaluate_return_stmt,
};
use super::values::Val;

pub fn evaluate(
    stmt: Stmt,
    env: &Rc<RefCell<Environment>>,
    parent_env: &Rc<RefCell<Environment>>,
) -> Val {
    match stmt {
        Stmt::ExprStmt(expr) => evaluate_expr(expr, env, parent_env),
        Stmt::DeclareStmt {
            name,
            is_mutable,
            is_global,
            expr,
        } => evaluate_declare_var(name, is_mutable, is_global, expr, env, parent_env),
        Stmt::ReturnStmt(expr) => evaluate_return_stmt(expr, env, parent_env),
        Stmt::FunctionDeclaration {
            parameters,
            name,
            body,
            is_async,
        } => evaluate_declare_fn(parameters, name, body, is_async, env),
        Stmt::BreakStmt(expr) => evaluate_break_stmt(expr, env, parent_env), // Handle other statement types...
    }
}
