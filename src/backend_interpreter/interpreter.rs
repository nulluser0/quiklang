// Interpreter

use std::cell::RefCell;
use std::rc::Rc;

use crate::errors::RuntimeError;
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
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, RuntimeError> {
    match stmt {
        Stmt::ExprStmt(expr) => evaluate_expr(expr, env, root_env),
        Stmt::DeclareStmt {
            name,
            is_mutable,
            is_global,
            var_type: _, // TODO: use this if needed
            expr,
        } => evaluate_declare_var(name, is_mutable, is_global, expr, env, root_env),
        Stmt::ReturnStmt(expr) => evaluate_return_stmt(expr, env, root_env),
        Stmt::FunctionDeclaration {
            parameters,
            name,
            return_type: _, // TODO: use this if needed
            body,
            is_async,
        } => evaluate_declare_fn(parameters, name, body, is_async, env),
        Stmt::BreakStmt(expr) => evaluate_break_stmt(expr, env, root_env),
        Stmt::StructDefStmt {
            ident: _,
            key_type_values: _,
        } => todo!(), // Handle other statement types...
    }
}
