// AST logic

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::errors::ParserError;

use super::{lexer::Operator, type_environment::TypeEnvironment};

// Program struct
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Stmt>, // SHOULD BE STMT!!!
}

impl Program {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Program { statements }
    }
}

pub trait ParsetimeType: std::fmt::Debug {
    fn get_type(
        &self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        line: usize,
        col: usize,
    ) -> Result<Type, ParserError>;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Any,
    String,
    Integer,
    Float,
    Object,
    Null,
    Bool,
    Function(Vec<(Type, bool)>, Box<Type>),
    Array(Box<Type>),
    Range(Box<Type>),
    Tuple(Vec<Type>),
    Mismatch,

    Struct(String, HashMap<String, Type>),
    Enum(String, HashMap<String, Vec<Type>>),
    Alias(String, Box<Type>),
    // TODO: Custom(String)
}

pub trait FromType: std::fmt::Debug {
    type Output;
    fn from_type(type_val: &Type) -> Option<Self::Output>;
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => write!(f, "Any"),
            Type::String => write!(f, "String"),
            Type::Integer => write!(f, "Integer"),
            Type::Float => write!(f, "Float"),
            Type::Object => write!(f, "Object"),
            Type::Null => write!(f, "Null"),
            Type::Bool => write!(f, "Bool"),
            Type::Array(inner) => write!(f, "Array<{}>", inner),
            Type::Range(inner) => write!(f, "Range<{}>", inner),
            Type::Tuple(inner) => {
                let elements: Vec<String> = inner.iter().map(|val| format!("{}", val)).collect();
                write!(f, "({})", elements.join(", "))
            }
            Type::Mismatch => write!(f, "(Multiple, Mismatched Return Types)"),
            Type::Function(params, return_type) => {
                let mut params_string: String = String::new();
                for (i, param) in params.iter().enumerate() {
                    if i != 0 || i != params.len() - 1 {
                        params_string.push_str(", ");
                    }
                    if param.1 {
                        params_string.push_str("mut ")
                    }
                    params_string.push_str(&param.0.to_string());
                }

                write!(f, "Function ({}) -> {}", params_string, return_type)
            }
            Type::Struct(_, _) => todo!(),
            Type::Enum(_, _) => todo!(),
            Type::Alias(name, actual) => write!(f, "{} ==> {}", name, actual),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(Literal),
    Array(Vec<Expr>, Type),
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        inclusive: bool,
        defined_type: Type,
    },
    Identifier(String),
    Tuple(Vec<(Expr, Type)>),
    AssignmentExpr {
        assignee: Box<Expr>,
        expr: Box<Expr>,
    }, // Assignee, Expr
    ConcatOp {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp(UnaryOp, Box<Expr>),
    FunctionCall(Vec<(Expr, bool)>, Box<Expr>), // Args, Caller
    Member(Box<Expr>, Box<Expr>),               // Object, Property
    IfExpr {
        condition: Box<Expr>,
        then: Vec<Stmt>,
        else_stmt: Option<Vec<Stmt>>,
    }, // Condition, Then, Optional Else
    ForExpr {
        identifier: String,
        iterable: Box<Expr>,
        then: Vec<Stmt>,
    }, // For identifier in iterable, Do Stmts
    WhileExpr {
        condition: Box<Expr>,
        then: Vec<Stmt>,
    }, // While Condition, Do Stmts
    BlockExpr(Vec<Stmt>), // A block expr, literally the same as `let x; {x=10} let y;` in rust.
    ForeverLoopExpr(Vec<Stmt>), // Forever loop! Has vec of stmts.
    SpecialNull, // Literally just returns null. Should ONLY be returned as a result of a semicolon.
    StructLiteral(String, Vec<Property>),
    EnumLiteral(String, String, Vec<Expr>),
}

impl Expr {
    pub fn verify_type(
        self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        line: usize,
        col: usize,
    ) -> Result<Self, ParserError> {
        match self.get_type(type_env, line, col) {
            Ok(_) => Ok(self),
            Err(e) => Err(e),
        }
    }
}

impl ParsetimeType for Expr {
    fn get_type(
        &self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        line: usize,
        col: usize,
    ) -> Result<Type, ParserError> {
        match self {
            Expr::Literal(literal) => literal.get_type(type_env, line, col),
            Expr::Array(_, defined_type) => Ok(Type::Array(Box::new(defined_type.clone()))),
            Expr::Range { defined_type, .. } => Ok(Type::Range(Box::new(defined_type.clone()))),
            Expr::Identifier(ident) => Ok(type_env
                .borrow()
                .lookup_var(ident)
                .ok_or(ParserError::UndefinedVariable(line, col, ident.to_owned()))?),
            Expr::Tuple(values) => Ok(Type::Tuple(
                values
                    .iter()
                    .map(|(_, type_val)| type_val.clone())
                    .collect(),
            )),
            Expr::AssignmentExpr { expr, .. } => expr.get_type(type_env, line, col),
            Expr::ConcatOp { .. } => Ok(Type::String),
            Expr::BinaryOp { op, left, right } => match op {
                BinaryOp::Add
                | BinaryOp::Subtract
                | BinaryOp::Multiply
                | BinaryOp::Divide
                | BinaryOp::Modulus => {
                    match (
                        left.get_type(type_env, line, col)?,
                        right.get_type(type_env, line, col)?,
                    ) {
                        (Type::Integer, Type::Integer) => Ok(Type::Integer),
                        (Type::Float, Type::Integer)
                        | (Type::Integer, Type::Float)
                        | (Type::Float, Type::Float) => Ok(Type::Float),
                        _ => Err(ParserError::TypeError {
                            expected: Type::Float,
                            found: Type::Mismatch,
                            line,
                            col,
                            message: "Binary operations can only take in either float or integer."
                                .to_string(),
                        }),
                    }
                }
                BinaryOp::GreaterThan
                | BinaryOp::LessThan
                | BinaryOp::GreaterOrEqual
                | BinaryOp::LessOrEqual
                | BinaryOp::Equal
                | BinaryOp::NotEqual
                | BinaryOp::And
                | BinaryOp::Or => Ok(Type::Bool),
            },
            Expr::UnaryOp(op, expr) => match op {
                UnaryOp::LogicalNot => {
                    // Should be boolean
                    let expr_type = expr.get_type(type_env, line, col)?;
                    if expr_type != Type::Bool {
                        return Err(ParserError::TypeError {
                            expected: Type::Bool,
                            found: expr_type,
                            line,
                            col,
                            message: "Logical Not can only take in bool.".to_string(),
                        });
                    }
                    Ok(Type::Bool)
                }
                UnaryOp::ArithmeticNegative | UnaryOp::ArithmeticPositive => {
                    // Should be float or integer
                    let expr_type = expr.get_type(type_env, line, col)?;
                    if expr_type != Type::Integer || expr_type != Type::Float {
                        return Err(ParserError::TypeError {
                            expected: Type::Float,
                            found: expr_type,
                            line,
                            col,
                            message: "Arithmetic negative/positive can only take in either float or integer."
                                .to_string(),
                        });
                    }
                    Ok(expr_type)
                }
                UnaryOp::BitwiseNot => {
                    // Should be integer
                    let expr_type = expr.get_type(type_env, line, col)?;
                    if expr_type != Type::Integer {
                        return Err(ParserError::TypeError {
                            expected: Type::Integer,
                            found: expr_type,
                            line,
                            col,
                            message: "Bitwise Not can only take in integer.".to_string(),
                        });
                    }
                    Ok(Type::Integer)
                }
            },
            Expr::FunctionCall(_, caller) => {
                if let Expr::Identifier(fn_name) = &**caller {
                    if let Some(Type::Function(_, return_type)) =
                        type_env.borrow().lookup_fn(fn_name)
                    {
                        Ok(*return_type.clone())
                    } else {
                        Err(ParserError::UndefinedFunction(
                            line,
                            col,
                            fn_name.to_string(),
                        ))
                    }
                } else {
                    Err(ParserError::InvalidFunctionCall(line, col))
                }
            }
            Expr::Member(_, _) => todo!("IMPLEMENT MEMBER RETURN TYPES!"), // flagged for removal sometime after new impl of datatypes
            Expr::IfExpr {
                then, else_stmt, ..
            } => {
                let then_type = then
                    .last()
                    .map_or(Ok(Type::Null), |stmt| stmt.get_type(type_env, line, col))?;
                if let Some(else_block) = else_stmt {
                    let else_type = else_block
                        .last()
                        .map_or(Ok(Type::Null), |stmt| stmt.get_type(type_env, line, col))?;
                    if then_type == else_type {
                        Ok(then_type)
                    } else {
                        Ok(Type::Mismatch)
                    }
                } else {
                    Ok(then_type)
                }
            }
            Expr::ForExpr {
                identifier: _,
                iterable: _,
                then,
            } => then
                .last()
                .map_or(Ok(Type::Null), |stmt| stmt.get_type(type_env, line, col)),
            Expr::WhileExpr { condition: _, then } => then
                .last()
                .map_or(Ok(Type::Null), |stmt| stmt.get_type(type_env, line, col)),
            Expr::BlockExpr(statements) => statements
                .last()
                .map_or(Ok(Type::Null), |stmt| stmt.get_type(type_env, line, col)),
            Expr::ForeverLoopExpr(statements) => statements
                .last()
                .map_or(Ok(Type::Null), |stmt| stmt.get_type(type_env, line, col)),
            Expr::SpecialNull => Ok(Type::Null),
            Expr::StructLiteral(_, _) => todo!(),
            Expr::EnumLiteral(_, _, _) => todo!(),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Identifier(keyword) => write!(f, "{:?}", keyword),
            a => write!(f, "{:?}", a),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    ExprStmt(Expr),
    DeclareStmt {
        name: String,
        is_mutable: bool,
        is_global: bool,
        var_type: Type,
        expr: Option<Expr>,
    },
    ReturnStmt(Option<Expr>),
    BreakStmt(Option<Expr>),
    FunctionDeclaration {
        parameters: Vec<(String, Type, bool)>,
        name: String,
        return_type: Type,
        body: Vec<Stmt>,
        is_async: bool,
    }, // Parameters, Name, Body, Is async?
    StructDefStmt {
        ident: String,
        key_type_values: HashMap<String, Type>,
    },
    EnumDefStmt {
        ident: String,
        variants: HashMap<String, Vec<Type>>,
    },
    AliasDefStmt {
        ident: String,
        alias: Box<Type>,
    },
}

impl ParsetimeType for Stmt {
    fn get_type(
        &self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        line: usize,
        col: usize,
    ) -> Result<Type, ParserError> {
        match self {
            Stmt::ExprStmt(expr) => expr.get_type(type_env, line, col),
            Stmt::DeclareStmt { .. } => Ok(Type::Null),
            Stmt::ReturnStmt(expr) => match expr {
                Some(expr) => expr.get_type(type_env, line, col),
                None => Ok(Type::Null),
            },
            Stmt::BreakStmt(expr) => match expr {
                Some(expr) => expr.get_type(type_env, line, col),
                None => Ok(Type::Null),
            },
            Stmt::FunctionDeclaration { .. } => Ok(Type::Null),
            Stmt::StructDefStmt { .. } => Ok(Type::Null),
            Stmt::EnumDefStmt { .. } => Ok(Type::Null),
            Stmt::AliasDefStmt { .. } => Ok(Type::Null),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Object(Vec<Property>),
    // Other literal types...
}

impl ParsetimeType for Literal {
    fn get_type(
        &self,
        _type_env: &Rc<RefCell<TypeEnvironment>>,
        _line: usize,
        _col: usize,
    ) -> Result<Type, ParserError> {
        match self {
            Literal::Integer(_) => Ok(Type::Integer),
            Literal::Float(_) => Ok(Type::Float),
            Literal::String(_) => Ok(Type::String),
            Literal::Object(_) => Ok(Type::Object),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Property {
    pub key: String,
    pub value: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    GreaterOrEqual,
    LessOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
    Modulus,
}

impl From<Operator> for BinaryOp {
    fn from(op: Operator) -> Self {
        match op {
            Operator::Add => BinaryOp::Add,
            Operator::Subtract => BinaryOp::Subtract,
            Operator::Multiply => BinaryOp::Multiply,
            Operator::Divide => BinaryOp::Divide,
            Operator::GreaterThan => BinaryOp::GreaterThan,
            Operator::LessThan => BinaryOp::LessThan,
            Operator::GreaterOrEqual => BinaryOp::GreaterOrEqual,
            Operator::LessOrEqual => BinaryOp::LessOrEqual,
            Operator::Equal => BinaryOp::Equal,
            Operator::NotEqual => BinaryOp::NotEqual,
            Operator::And => BinaryOp::And,
            Operator::Or => BinaryOp::Or,
            Operator::Modulus => BinaryOp::Modulus,
            _ => panic!("Operator {:?} cannot be converted to BinaryOp", op),
        }
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "Add"),
            BinaryOp::Subtract => write!(f, "Subtract"),
            BinaryOp::Multiply => write!(f, "Multiply"),
            BinaryOp::Divide => write!(f, "Divide"),
            BinaryOp::GreaterThan => write!(f, "GreaterThan"),
            BinaryOp::LessThan => write!(f, "LessThan"),
            BinaryOp::GreaterOrEqual => write!(f, "GreaterOrEqual"),
            BinaryOp::LessOrEqual => write!(f, "LessOrEqual"),
            BinaryOp::Equal => write!(f, "Equal"),
            BinaryOp::NotEqual => write!(f, "NotEqual"),
            BinaryOp::And => write!(f, "And"),
            BinaryOp::Or => write!(f, "Or"),
            BinaryOp::Modulus => write!(f, "Modulus"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    LogicalNot,         // !
    ArithmeticNegative, // -
    ArithmeticPositive, // +
    BitwiseNot,         // ~
}
