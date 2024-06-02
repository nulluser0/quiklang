use std::collections::HashMap;

use crate::{
    backend::{
        environment::Environment,
        interpreter::evaluate,
        values::{BoolVal, FloatVal, IntegerVal, NullVal, ObjectVal, ToFloat, Val},
    },
    frontend::ast::{BinaryOp, Expr, Literal, Property, Stmt},
    mk_float, mk_integer, mk_null,
};

pub fn evaluate_expr(expr: Expr, env: &mut Environment) -> Val {
    match expr {
        Expr::Literal(literal) => evaluate_literal(literal, env),
        Expr::Identifier(identifier) => evaluate_identifier(identifier, env),
        Expr::BinaryOp { op, left, right } => evaluate_binary_op(op, *left, *right, env),
        Expr::UnaryOp(_, _) => unimplemented!("{:?}", expr),
        Expr::FunctionCall(args, caller) => evaluate_call_expr(args, *caller, env),
        Expr::AssignmentExpr(assignee, expr) => evaluate_assignment(*assignee, *expr, env),
        Expr::Member(_, _, _) => todo!("{:?}", expr),
        Expr::IfExpr(condition, then, else_stmt) => {
            evaluate_if_expr(*condition, then, else_stmt, env)
        }
    }
}

pub fn evaluate_if_expr(
    condition: Expr,
    then: Vec<Stmt>,
    else_stmt: Option<Vec<Stmt>>,
    env: &mut Environment,
) -> Val {
    // Evaluate the condition. If it is true, then complete then, else do the else_stmt (or ignore if statement.)
    let condition_value = evaluate_expr(condition, env);

    // Check if the condition evaluates to true
    if let Val::Bool(condition_bool) = condition_value {
        if condition_bool.value {
            // Evaluate the consequent block if the condition is true
            for stmt in then {
                let result = evaluate(stmt, env);
                match result {
                    Val::Null(_) => {}
                    val => return val,
                }
            }
        } else if let Some(alt) = else_stmt {
            // Evaluate the alternative block if the condition is false and an alternative is provided
            for stmt in alt {
                let result = evaluate(stmt, env);
                match result {
                    Val::Null(_) => {}
                    val => return val,
                }
            }
        }
    } else {
        // Error: If condition doesn't evaluate to a boolean value
        panic!("If condition must evaluate to a boolean value.");
    }
    // TODO: Handle cases where expr returns a value. Also as a result:
    // TODO: Handle cases where Exprs do not return a value using the semicolon.
    mk_null!()
}

pub fn evaluate_assignment(assignee: Expr, expr: Expr, env: &mut Environment) -> Val {
    let varname = match assignee {
        Expr::Identifier(name) => name,
        _ => panic!("Invalid LHS in assignment expression. {:?}", assignee),
    };
    let value = evaluate_expr(expr, env);
    env.assign_var(&varname, value)
}

pub fn evaluate_identifier(identifier: String, env: &mut Environment) -> Val {
    env.lookup_var(&identifier)
}

pub fn evaluate_literal(literal: Literal, env: &mut Environment) -> Val {
    match literal {
        Literal::Integer(value) => mk_integer!(value) as Val,
        Literal::Float(value) => mk_float!(value) as Val,
        Literal::String(_) => unimplemented!(),
        Literal::Object(object) => evaluate_object_expr(object, env),
    }
}

pub fn evaluate_object_expr(obj: Vec<Property>, env: &mut Environment) -> Val {
    let mut object = ObjectVal {
        properties: HashMap::new(),
    };
    for property in obj {
        let runtime_val: Option<Val> = property.value.map(|expr| evaluate_expr(expr, env));
        object.properties.insert(property.key, runtime_val);
    }

    Val::Object(object)
}

pub fn evaluate_call_expr(args: Vec<Expr>, caller: Expr, env: &mut Environment) -> Val {
    let evaluated_args: Vec<Val> = args
        .into_iter()
        .map(|expr| evaluate_expr(expr, env))
        .collect();
    let function = evaluate_expr(caller, env);
    match &function {
        Val::NativeFunction(callable) => return (callable.call)(evaluated_args, env),
        Val::Function(fn_value) => {
            let mut scope = Environment::new_with_parent(fn_value.declaration_env.to_owned());
            for (i, _) in evaluated_args
                .iter()
                .enumerate()
                .take(fn_value.parameters.len())
            {
                // TODO: Check the bounds here.
                // Verify arity of function.
                let varname = &fn_value.parameters[i];
                let arg = &evaluated_args[i];
                scope.declare_var(varname, arg.clone(), false);
            }
            let mut result: Val = mk_null!();
            for stmt in &fn_value.body {
                result = evaluate(stmt.clone(), &mut scope)
            }
            return result;
        }
        _ => {}
    };
    panic!("Cannot call value that is not a function: {:?}", function);
}

pub fn evaluate_binary_op(op: BinaryOp, left: Expr, right: Expr, env: &mut Environment) -> Val {
    let left_val = evaluate_expr(left, env);
    let right_val = evaluate_expr(right, env);

    match (left_val, right_val) {
        (Val::Integer(l), Val::Integer(r)) => match op {
            BinaryOp::Add => Val::Integer(IntegerVal {
                value: l.value + r.value,
            }),
            BinaryOp::Subtract => Val::Integer(IntegerVal {
                value: l.value - r.value,
            }),
            BinaryOp::Multiply => Val::Integer(IntegerVal {
                value: l.value * r.value,
            }),
            BinaryOp::Divide => Val::Integer(IntegerVal {
                value: l.value / r.value,
            }),
            BinaryOp::Modulus => Val::Integer(IntegerVal {
                value: l.value % r.value,
            }),
            BinaryOp::GreaterThan => Val::Bool(BoolVal {
                value: l.value > r.value,
            }),
            BinaryOp::LessThan => Val::Bool(BoolVal {
                value: l.value < r.value,
            }),
            BinaryOp::GreaterOrEqual => Val::Bool(BoolVal {
                value: l.value >= r.value,
            }),
            BinaryOp::LessOrEqual => Val::Bool(BoolVal {
                value: l.value <= r.value,
            }),
            BinaryOp::Equal => Val::Bool(BoolVal {
                value: l.value == r.value,
            }),
            BinaryOp::NotEqual => Val::Bool(BoolVal {
                value: l.value != r.value,
            }),
            _ => panic!("Unsupported binary operation"),
        },
        (Val::Float(l), Val::Float(r)) => {
            match op {
                BinaryOp::Add => Val::Float(FloatVal {
                    value: l.value + r.value,
                }),
                BinaryOp::Subtract => Val::Float(FloatVal {
                    value: l.value - r.value,
                }),
                BinaryOp::Multiply => Val::Float(FloatVal {
                    value: l.value * r.value,
                }),
                BinaryOp::Divide => Val::Float(FloatVal {
                    value: l.value / r.value,
                }),
                BinaryOp::Modulus => Val::Float(FloatVal {
                    value: l.value % r.value,
                }), // Note: % operator for floats
                BinaryOp::GreaterThan => Val::Bool(BoolVal {
                    value: l.value > r.value,
                }),
                BinaryOp::LessThan => Val::Bool(BoolVal {
                    value: l.value < r.value,
                }),
                BinaryOp::GreaterOrEqual => Val::Bool(BoolVal {
                    value: l.value >= r.value,
                }),
                BinaryOp::LessOrEqual => Val::Bool(BoolVal {
                    value: l.value <= r.value,
                }),
                BinaryOp::Equal => Val::Bool(BoolVal {
                    value: l.value == r.value,
                }),
                BinaryOp::NotEqual => Val::Bool(BoolVal {
                    value: l.value != r.value,
                }),
                _ => panic!("Unsupported binary operation"),
            }
        }
        (Val::Integer(l), Val::Float(r)) => {
            match op {
                BinaryOp::Add => Val::Float(FloatVal {
                    value: l.to_float() + r.value,
                }),
                BinaryOp::Subtract => Val::Float(FloatVal {
                    value: l.to_float() - r.value,
                }),
                BinaryOp::Multiply => Val::Float(FloatVal {
                    value: l.to_float() * r.value,
                }),
                BinaryOp::Divide => Val::Float(FloatVal {
                    value: l.to_float() / r.value,
                }),
                BinaryOp::Modulus => Val::Float(FloatVal {
                    value: l.to_float() % r.value,
                }), // Convert to float and then mod
                BinaryOp::GreaterThan => Val::Bool(BoolVal {
                    value: l.to_float() > r.value,
                }),
                BinaryOp::LessThan => Val::Bool(BoolVal {
                    value: l.to_float() < r.value,
                }),
                BinaryOp::GreaterOrEqual => Val::Bool(BoolVal {
                    value: l.to_float() >= r.value,
                }),
                BinaryOp::LessOrEqual => Val::Bool(BoolVal {
                    value: l.to_float() <= r.value,
                }),
                BinaryOp::Equal => Val::Bool(BoolVal {
                    value: l.to_float() == r.value,
                }),
                BinaryOp::NotEqual => Val::Bool(BoolVal {
                    value: l.to_float() != r.value,
                }),
                _ => panic!("Unsupported binary operation"),
            }
        }
        (Val::Float(l), Val::Integer(r)) => {
            match op {
                BinaryOp::Add => Val::Float(FloatVal {
                    value: l.value + r.to_float(),
                }),
                BinaryOp::Subtract => Val::Float(FloatVal {
                    value: l.value - r.to_float(),
                }),
                BinaryOp::Multiply => Val::Float(FloatVal {
                    value: l.value * r.to_float(),
                }),
                BinaryOp::Divide => Val::Float(FloatVal {
                    value: l.value / r.to_float(),
                }),
                BinaryOp::Modulus => Val::Float(FloatVal {
                    value: l.value % r.to_float(),
                }), // Convert to float and then mod
                BinaryOp::GreaterThan => Val::Bool(BoolVal {
                    value: l.value > r.to_float(),
                }),
                BinaryOp::LessThan => Val::Bool(BoolVal {
                    value: l.value < r.to_float(),
                }),
                BinaryOp::GreaterOrEqual => Val::Bool(BoolVal {
                    value: l.value >= r.to_float(),
                }),
                BinaryOp::LessOrEqual => Val::Bool(BoolVal {
                    value: l.value <= r.to_float(),
                }),
                BinaryOp::Equal => Val::Bool(BoolVal {
                    value: l.value == r.to_float(),
                }),
                BinaryOp::NotEqual => Val::Bool(BoolVal {
                    value: l.value != r.to_float(),
                }),
                _ => panic!("Unsupported binary operation"),
            }
        }
        _ => panic!("Binary operations are only supported for numbers"),
    }
}
