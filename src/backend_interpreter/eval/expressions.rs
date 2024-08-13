// Expressions

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    backend_interpreter::{
        environment::Environment,
        interpreter::evaluate,
        values::{
            ArrayVal, BoolVal, FloatVal, IntegerVal, NullVal, ObjectVal, RangeVal, RuntimeVal,
            SpecialVal, SpecialValKeyword, StringVal, ToFloat, TupleVal, Val, ValueType,
        },
    },
    errors::InterpreterError,
    frontend::ast::{BinaryOp, Expr, FromType, Literal, Property, Stmt, Type, UnaryOp},
    mk_float, mk_integer, mk_null, mk_string,
};

pub fn evaluate_expr(
    expr: Expr,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    match expr {
        Expr::Literal(literal) => evaluate_literal(literal, env, root_env),
        Expr::Identifier(identifier) => evaluate_identifier(identifier, env),
        Expr::Tuple(values) => evaluate_tuple(values, env, root_env),
        Expr::BinaryOp { op, left, right } => evaluate_binary_op(op, *left, *right, env, root_env),
        Expr::UnaryOp(op, expr) => evaluate_unary_op(op, *expr, env, root_env),
        Expr::FunctionCall(args, caller) => evaluate_call_expr(args, *caller, env, root_env),
        Expr::AssignmentExpr { assignee, expr } => {
            evaluate_assignment(*assignee, *expr, env, root_env)
        }
        Expr::Member(object, property) => evaluate_member_expr(*object, *property, env, root_env),
        Expr::IfExpr {
            condition,
            then,
            else_stmt,
        } => evaluate_if_expr(*condition, then, else_stmt, env, root_env),
        Expr::ForExpr {
            identifier,
            iterable,
            then,
        } => evaluate_for_expr(identifier, *iterable, then, env, root_env),
        Expr::WhileExpr { condition, then } => evaluate_while_expr(*condition, then, env, root_env),
        Expr::ForeverLoopExpr(then) => evaluate_loop_expr(then, env, root_env),
        Expr::Array(elements, elements_type) => {
            evaluate_array_expr(elements, elements_type, env, root_env)
        }
        Expr::Range {
            start,
            end,
            inclusive,
            defined_type,
        } => evaluate_range_expr(*start, *end, inclusive, defined_type, env, root_env),
        Expr::SpecialNull => Ok(mk_null!()),
        Expr::ConcatOp { left, right } => evaluate_concatenation_expr(*left, *right, env, root_env),
        Expr::BlockExpr(then) => evaluate_block_expr(then, env, root_env),
        Expr::StructLiteral(_, _) => todo!(),
        Expr::EnumLiteral(_, _, _) => todo!(),
    }
}

pub fn evaluate_tuple(
    tuple: Vec<(Expr, Type)>,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let mut evaluated_elements: Vec<Val> = Vec::new();
    for element in tuple.iter().map(|(values, _)| values) {
        evaluated_elements.push(evaluate_expr(element.clone(), env, root_env)?)
    }
    Ok(Val::Tuple(TupleVal {
        values: evaluated_elements,
    }))
}

pub fn evaluate_array_expr(
    elements: Vec<Expr>,
    elements_type: Type,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let mut evaluated_elements: Vec<Val> = Vec::new();
    for element in elements {
        evaluated_elements.push(evaluate_expr(element, env, root_env)?)
    }
    Ok(Val::Array(ArrayVal {
        values: evaluated_elements,
        inner_type: Val::from_type(&elements_type)
            .expect("Unable to convert Type to ValueType. This should not happen."),
    }))
}

pub fn evaluate_range_expr(
    start: Expr,
    end: Expr,
    inclusive: bool,
    defined_type: Type,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let start = Box::new(evaluate_expr(start, env, root_env)?);
    let end = Box::new(evaluate_expr(end, env, root_env)?);
    Ok(Val::Range(RangeVal {
        start,
        end,
        inclusive,
        inner_type: Val::from_type(&defined_type)
            .expect("Unable to convert Type to ValueType. This should not happen."),
    }))
}

pub fn evaluate_for_expr(
    identifier: String,
    iterable: Expr,
    then: Vec<Stmt>,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let iterable_value = evaluate_expr(iterable, env, root_env)?;
    let iterator = match iterable_value.to_iterator() {
        Ok(iter) => iter,
        Err(err) => return Err(err),
    };

    while let Some(item) = iterator.borrow_mut().next() {
        let scope = Rc::new(RefCell::new(Environment::new_with_parent(env.clone())));
        scope.borrow_mut().declare_var(&identifier, item, false)?;
        for stmt in &then {
            if let Val::Special(SpecialVal {
                keyword: SpecialValKeyword::Break,
                return_value,
            }) = evaluate(stmt.clone(), &scope, root_env)?
            {
                return return_value.map_or(Ok(mk_null!()), |val| Ok(*val));
            }
        }
    }
    Ok(mk_null!())
}

pub fn evaluate_while_expr(
    condition: Expr,
    then: Vec<Stmt>,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let value_type = evaluate_expr(condition.clone(), env, root_env)?.get_type();
    if value_type == ValueType::Bool {
        while let Val::Bool(BoolVal { value: true }) =
            evaluate_expr(condition.clone(), env, root_env)?
        {
            // Evaluate the consequent block if the condition is true
            let scope = Rc::new(RefCell::new(Environment::new_with_parent(env.clone())));
            for stmt in &then {
                if let Val::Special(SpecialVal {
                    keyword: SpecialValKeyword::Break,
                    return_value,
                }) = evaluate(stmt.clone(), &scope, root_env)?
                {
                    return return_value.map_or(Ok(mk_null!()), |val| Ok(*val));
                }
            }
        }
    } else {
        return Err(InterpreterError::TypeError {
            message: "While expression conditions must evaluate to a bool.".to_string(),
            expected: ValueType::Bool,
            found: value_type,
        });
    }
    Ok(mk_null!())
}

pub fn evaluate_loop_expr(
    then: Vec<Stmt>,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    loop {
        let scope = Rc::new(RefCell::new(Environment::new_with_parent(env.clone())));
        for stmt in &then {
            if let Val::Special(SpecialVal {
                keyword: SpecialValKeyword::Break,
                return_value,
            }) = evaluate(stmt.clone(), &scope, root_env)?
            {
                return return_value.map_or(Ok(mk_null!()), |val| Ok(*val));
            }
        }
    }
}

pub fn evaluate_block_expr(
    then: Vec<Stmt>,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let scope = Rc::new(RefCell::new(Environment::new_with_parent(env.clone())));
    let mut result = mk_null!();
    for stmt in then {
        result = evaluate(stmt, &scope, root_env)?;
    }
    Ok(result)
}

pub fn evaluate_unary_op(
    op: UnaryOp,
    expr: Expr,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let evaluated_expr = evaluate_expr(expr, env, root_env)?;
    match op {
        UnaryOp::LogicalNot => {
            if let Val::Bool(condition_bool) = evaluated_expr {
                Ok(Val::Bool(BoolVal {
                    value: !(condition_bool.value),
                }))
            } else {
                panic!("Unary Operation `LogicalNot` (`!`) must apply to a boolean value.")
            }
        }
        UnaryOp::ArithmeticNegative => match evaluated_expr {
            Val::Float(FloatVal { value }) => Ok(Val::Float(FloatVal { value: -value })),
            Val::Integer(IntegerVal { value }) => Ok(Val::Integer(IntegerVal { value: -value })),
            _ => panic!(
                "Unary Operation `ArithmeticNegative` (`-`) must apply to a float or integer."
            ),
        },
        UnaryOp::ArithmeticPositive => match evaluated_expr {
            Val::Float(_) => Ok(evaluated_expr),
            Val::Integer(_) => Ok(evaluated_expr),
            _ => panic!(
                "Unary Operation `ArithmeticPositive` (`+`) must apply to a float or integer."
            ),
        },
        UnaryOp::BitwiseNot => match evaluated_expr {
            Val::Integer(IntegerVal { value }) => Ok(Val::Integer(IntegerVal { value: !value })),
            _ => panic!("Unary Operation `BitwiseNot` (`~`) must apply to an integer."),
        },
    }
}

pub fn evaluate_if_expr(
    condition: Expr,
    then: Vec<Stmt>,
    else_stmt: Option<Vec<Stmt>>,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    // Evaluate the condition. If it is true, then complete then, else do the else_stmt (or ignore if statement.)
    let condition_value = evaluate_expr(condition, env, root_env)?;

    let mut result = mk_null!();

    // Check if the condition evaluates to true
    if let Val::Bool(condition_bool) = condition_value {
        if condition_bool.value {
            // Evaluate the consequent block if the condition is true
            for stmt in then {
                result = evaluate(stmt, env, root_env)?;
            }
        } else if let Some(alt) = else_stmt {
            // Evaluate the alternative block if the condition is false and an alternative is provided
            for stmt in alt {
                result = evaluate(stmt, env, root_env)?;
            }
        }
    } else {
        // Error: If condition doesn't evaluate to a boolean value
        return Err(InterpreterError::TypeError {
            message: "If expression must evaluate to a bool.".to_string(),
            expected: ValueType::Bool,
            found: condition_value.get_type(),
        });
    }
    // TODO: Handle cases where Exprs do not return a value using the semicolon.
    Ok(result)
}

pub fn evaluate_assignment(
    assignee: Expr,
    expr: Expr,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let varname = match assignee {
        Expr::Identifier(name) => name,
        _ => return Err(InterpreterError::InvalidAssignExpr(assignee.to_string())),
    };
    let value = evaluate_expr(expr, env, root_env)?;
    Environment::assign_var(env, &varname, value)
}

pub fn evaluate_member_expr(
    object: Expr,
    property: Expr,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let object = evaluate_expr(object, env, root_env)?;
    let property = match property {
        Expr::Identifier(result) => result,
        _ => {
            return Err(InterpreterError::RuntimeError(
                "Cannot find member of non-identifier.".to_string(),
            )); // TODO: When proper data strctures are impl, the entire member/object classes are flagged for removal.
        }
    };
    match object {
        Val::Object(ObjectVal { properties }) => match properties.get(&property) {
            Some(result) => Ok(result.clone().unwrap_or(mk_null!())),
            None => Ok(mk_null!()),
        },
        _ => Err(InterpreterError::RuntimeError(
            "Cannot get a member of a non-object type.".to_string(),
        )),
    }
}

pub fn evaluate_identifier(
    identifier: String,
    env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    Environment::lookup_var(env, &identifier)
}

pub fn evaluate_literal(
    literal: Literal,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    match literal {
        Literal::Integer(value) => Ok(mk_integer!(value) as Val),
        Literal::Float(value) => Ok(mk_float!(value) as Val),
        Literal::String(value) => Ok(mk_string!(value) as Val),
        Literal::Object(object) => evaluate_object_expr(object, env, root_env),
    }
}

pub fn evaluate_object_expr(
    obj: Vec<Property>,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let mut object = ObjectVal {
        properties: HashMap::new(),
    };
    for property in obj {
        let runtime_val = property
            .value
            .map(|expr| evaluate_expr(expr, env, root_env))
            .transpose()?;
        object.properties.insert(property.key, runtime_val);
    }

    Ok(Val::Object(object))
}

pub fn evaluate_call_expr(
    args: Vec<(Expr, bool)>,
    caller: Expr,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let function = evaluate_expr(caller, env, root_env)?;
    match &function {
        Val::NativeFunction(callable) => (callable.call)(args, env, root_env),
        Val::Function(fn_value) => {
            let evaluated_args: Result<Vec<(Val, bool)>, InterpreterError> = args
                .into_iter()
                .map(|(expr, is_mut)| {
                    let eval_expr = match evaluate_expr(expr, env, root_env) {
                        Ok(result) => result,
                        Err(e) => return Err(e),
                    };

                    Ok((eval_expr, is_mut))
                })
                .collect();
            let evaluated_args = evaluated_args?;
            let scope = Rc::new(RefCell::new(Environment::new_with_parent(root_env.clone())));
            scope.borrow_mut().declare_var(
                &fn_value.name,
                Val::Function(fn_value.clone()),
                false,
            )?;
            for (varname, arg) in fn_value.parameters.iter().zip(evaluated_args.iter()) {
                scope
                    .borrow_mut()
                    .declare_var(&varname.0, arg.0.clone(), arg.1)?;
            }
            let mut result: Val = mk_null!();
            for stmt in &fn_value.body {
                result = evaluate(stmt.clone(), &scope, root_env)?;
                if let Val::Special(SpecialVal {
                    keyword: SpecialValKeyword::Return,
                    return_value,
                }) = result
                {
                    return return_value.map_or(Ok(mk_null!()), |val| Ok(*val));
                }
            }
            Ok(result)
        }
        _ => panic!("Cannot call value that is not a function: {:?}", function),
    }
}

pub fn evaluate_concatenation_expr(
    left: Expr,
    right: Expr,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let left_val = evaluate_expr(left, env, root_env)?;
    let right_val = evaluate_expr(right, env, root_env)?;

    match (left_val, right_val) {
        (Val::String(StringVal { value: l }), Val::String(StringVal { value: r })) => {
            Ok(Val::String(StringVal { value: l + &r }))
        }
        e => Err(InterpreterError::TypeError {
            message: "Concatenation expressions only work on Strings.".to_string(),
            expected: ValueType::String,
            found: e.0.get_type(),
        }),
    }
}

pub fn evaluate_binary_op(
    op: BinaryOp,
    left: Expr,
    right: Expr,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let left_val = evaluate_expr(left, env, root_env)?;
    let right_val = evaluate_expr(right, env, root_env)?;

    match (left_val, right_val) {
        (Val::Integer(l), Val::Integer(r)) => match op {
            BinaryOp::Add => Ok(Val::Integer(IntegerVal {
                value: l.value + r.value,
            })),
            BinaryOp::Subtract => Ok(Val::Integer(IntegerVal {
                value: l.value - r.value,
            })),
            BinaryOp::Multiply => Ok(Val::Integer(IntegerVal {
                value: l.value * r.value,
            })),
            BinaryOp::Divide => Ok(Val::Integer(IntegerVal {
                value: l.value / r.value,
            })),
            BinaryOp::Modulus => Ok(Val::Integer(IntegerVal {
                value: l.value % r.value,
            })),
            BinaryOp::GreaterThan => Ok(Val::Bool(BoolVal {
                value: l.value > r.value,
            })),
            BinaryOp::LessThan => Ok(Val::Bool(BoolVal {
                value: l.value < r.value,
            })),
            BinaryOp::GreaterOrEqual => Ok(Val::Bool(BoolVal {
                value: l.value >= r.value,
            })),
            BinaryOp::LessOrEqual => Ok(Val::Bool(BoolVal {
                value: l.value <= r.value,
            })),
            BinaryOp::Equal => Ok(Val::Bool(BoolVal {
                value: l.value == r.value,
            })),
            BinaryOp::NotEqual => Ok(Val::Bool(BoolVal {
                value: l.value != r.value,
            })),
            e => Err(InterpreterError::UnsupportedBinaryOp(e)),
        },
        (Val::Float(l), Val::Float(r)) => {
            match op {
                BinaryOp::Add => Ok(Val::Float(FloatVal {
                    value: l.value + r.value,
                })),
                BinaryOp::Subtract => Ok(Val::Float(FloatVal {
                    value: l.value - r.value,
                })),
                BinaryOp::Multiply => Ok(Val::Float(FloatVal {
                    value: l.value * r.value,
                })),
                BinaryOp::Divide => Ok(Val::Float(FloatVal {
                    value: l.value / r.value,
                })),
                BinaryOp::Modulus => Ok(Val::Float(FloatVal {
                    value: l.value % r.value,
                })), // Note: % operator for floats
                BinaryOp::GreaterThan => Ok(Val::Bool(BoolVal {
                    value: l.value > r.value,
                })),
                BinaryOp::LessThan => Ok(Val::Bool(BoolVal {
                    value: l.value < r.value,
                })),
                BinaryOp::GreaterOrEqual => Ok(Val::Bool(BoolVal {
                    value: l.value >= r.value,
                })),
                BinaryOp::LessOrEqual => Ok(Val::Bool(BoolVal {
                    value: l.value <= r.value,
                })),
                BinaryOp::Equal => Ok(Val::Bool(BoolVal {
                    value: l.value == r.value,
                })),
                BinaryOp::NotEqual => Ok(Val::Bool(BoolVal {
                    value: l.value != r.value,
                })),
                e => Err(InterpreterError::UnsupportedBinaryOp(e)),
            }
        }
        (Val::Integer(l), Val::Float(r)) => {
            match op {
                BinaryOp::Add => Ok(Val::Float(FloatVal {
                    value: l.to_float() + r.value,
                })),
                BinaryOp::Subtract => Ok(Val::Float(FloatVal {
                    value: l.to_float() - r.value,
                })),
                BinaryOp::Multiply => Ok(Val::Float(FloatVal {
                    value: l.to_float() * r.value,
                })),
                BinaryOp::Divide => Ok(Val::Float(FloatVal {
                    value: l.to_float() / r.value,
                })),
                BinaryOp::Modulus => Ok(Val::Float(FloatVal {
                    value: l.to_float() % r.value,
                })), // Convert to float and then mod
                BinaryOp::GreaterThan => Ok(Val::Bool(BoolVal {
                    value: l.to_float() > r.value,
                })),
                BinaryOp::LessThan => Ok(Val::Bool(BoolVal {
                    value: l.to_float() < r.value,
                })),
                BinaryOp::GreaterOrEqual => Ok(Val::Bool(BoolVal {
                    value: l.to_float() >= r.value,
                })),
                BinaryOp::LessOrEqual => Ok(Val::Bool(BoolVal {
                    value: l.to_float() <= r.value,
                })),
                BinaryOp::Equal => Ok(Val::Bool(BoolVal {
                    value: l.to_float() == r.value,
                })),
                BinaryOp::NotEqual => Ok(Val::Bool(BoolVal {
                    value: l.to_float() != r.value,
                })),
                e => Err(InterpreterError::UnsupportedBinaryOp(e)),
            }
        }
        (Val::Float(l), Val::Integer(r)) => {
            match op {
                BinaryOp::Add => Ok(Val::Float(FloatVal {
                    value: l.value + r.to_float(),
                })),
                BinaryOp::Subtract => Ok(Val::Float(FloatVal {
                    value: l.value - r.to_float(),
                })),
                BinaryOp::Multiply => Ok(Val::Float(FloatVal {
                    value: l.value * r.to_float(),
                })),
                BinaryOp::Divide => Ok(Val::Float(FloatVal {
                    value: l.value / r.to_float(),
                })),
                BinaryOp::Modulus => Ok(Val::Float(FloatVal {
                    value: l.value % r.to_float(),
                })), // Convert to float and then mod
                BinaryOp::GreaterThan => Ok(Val::Bool(BoolVal {
                    value: l.value > r.to_float(),
                })),
                BinaryOp::LessThan => Ok(Val::Bool(BoolVal {
                    value: l.value < r.to_float(),
                })),
                BinaryOp::GreaterOrEqual => Ok(Val::Bool(BoolVal {
                    value: l.value >= r.to_float(),
                })),
                BinaryOp::LessOrEqual => Ok(Val::Bool(BoolVal {
                    value: l.value <= r.to_float(),
                })),
                BinaryOp::Equal => Ok(Val::Bool(BoolVal {
                    value: l.value == r.to_float(),
                })),
                BinaryOp::NotEqual => Ok(Val::Bool(BoolVal {
                    value: l.value != r.to_float(),
                })),
                e => Err(InterpreterError::UnsupportedBinaryOp(e)),
            }
        }
        e => Err(InterpreterError::TypeError {
            message: "Binary operations are only for integers and floats.".to_string(),
            expected: ValueType::Integer,
            found: e.0.get_type(),
        }),
    }
}
