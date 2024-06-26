// AST logic

use crate::{backend::values::ValueType, errors::ParserError};

use super::lexer::Operator;

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
    fn get_type(&self) -> Result<Type, ParserError>;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    String,
    Integer,
    Float,
    Object,
    Null,
    Bool,
    Array(Box<Type>),
    Mismatch,
    // TODO: Custom(String)
}

impl Type {
    pub fn to_val(&self) -> Option<ValueType> {
        match self {
            Type::String => Some(ValueType::String),
            Type::Integer => Some(ValueType::Integer),
            Type::Float => Some(ValueType::Float),
            Type::Object => Some(ValueType::Object),
            Type::Null => Some(ValueType::Null),
            Type::Bool => Some(ValueType::Bool),
            Type::Array(inner) => Some(ValueType::Array(Box::new(inner.to_val()?))),
            Type::Mismatch => None,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::String => write!(f, "String"),
            Type::Integer => write!(f, "Integer"),
            Type::Float => write!(f, "Float"),
            Type::Object => write!(f, "Object"),
            Type::Null => write!(f, "Null"),
            Type::Bool => write!(f, "Bool"),
            Type::Array(inner) => write!(f, "Array<{}>", inner),
            Type::Mismatch => write!(f, "(Multiple, Mismatched Return Types)"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(Literal),
    Array(Vec<Expr>, Type),
    Identifier(String),
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
    FunctionCall(Vec<Expr>, Box<Expr>), // Args, Caller
    Member(Box<Expr>, Box<Expr>),       // Object, Property
    IfExpr {
        condition: Box<Expr>,
        then: Vec<Stmt>,
        else_stmt: Option<Vec<Stmt>>,
    }, // Condition, Then, Optional Else
    ForExpr {
        item: Box<Expr>,
        then: Vec<Stmt>,
    }, // For item in iterable, Do Stmts
    WhileExpr {
        condition: Box<Expr>,
        then: Vec<Stmt>,
    }, // While Condition, Do Stmts
    BlockExpr(Vec<Stmt>), // A block expr, literally the same as `let x; {x=10} let y;` in rust.
    ForeverLoopExpr(Vec<Stmt>), // Forever loop! Has vec of stmts.
    SpecialNull, // Literally just returns null. Should ONLY be returned as a result of a semicolon.
}

impl ParsetimeType for Expr {
    fn get_type(&self) -> Result<Type, ParserError> {
        match self {
            Expr::Literal(literal) => literal.get_type(),
            Expr::Array(_, defined_type) => Ok(Type::Array(Box::new(defined_type.clone()))),
            Expr::Identifier(_) => todo!("IMPLEMENT IDENTIFIER TYPES"),
            Expr::AssignmentExpr { expr, .. } => expr.get_type(),
            Expr::ConcatOp { .. } => Ok(Type::String),
            Expr::BinaryOp { op, left, right } => match op {
                BinaryOp::Add
                | BinaryOp::Subtract
                | BinaryOp::Multiply
                | BinaryOp::Divide
                | BinaryOp::Modulus => match (left.get_type()?, right.get_type()?) {
                    (Type::Integer, Type::Integer) => Ok(Type::Integer),
                    (Type::Float, Type::Integer)
                    | (Type::Integer, Type::Float)
                    | (Type::Float, Type::Float) => Ok(Type::Float),
                    _ => Ok(Type::Mismatch),
                },
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
                    if expr.get_type()? != Type::Bool {
                        unimplemented!("logical not is not bool")
                    }
                    Ok(Type::Bool)
                }
                UnaryOp::ArithmeticNegative | UnaryOp::ArithmeticPositive => {
                    // Should be float or integer
                    let expr_type = expr.get_type()?;
                    if expr_type != Type::Integer || expr_type != Type::Float {
                        unimplemented!("arithmetic +/- not int/float")
                    }
                    Ok(expr_type)
                }
                UnaryOp::BitwiseNot => {
                    // Should be integer
                    if expr.get_type()? != Type::Integer {
                        unimplemented!("bitwise not is not int")
                    }
                    Ok(Type::Integer)
                }
            },
            Expr::FunctionCall(_, _) => todo!("IMPLEMENT FUNCTION RETURN TYPES!"),
            Expr::Member(_, _) => todo!("IMPLEMENT MEMBER RETURN TYPES!"),
            Expr::IfExpr {
                then, else_stmt, ..
            } => {
                let then_type = then.last().map_or(Ok(Type::Null), |stmt| stmt.get_type())?;
                if let Some(else_block) = else_stmt {
                    let else_type = else_block
                        .last()
                        .map_or(Ok(Type::Null), |stmt| stmt.get_type())?;
                    if then_type == else_type {
                        Ok(then_type)
                    } else {
                        Ok(Type::Mismatch)
                    }
                } else {
                    Ok(then_type)
                }
            }
            Expr::ForExpr { item: _, then } => {
                then.last().map_or(Ok(Type::Null), |stmt| stmt.get_type())
            }
            Expr::WhileExpr { condition: _, then } => {
                then.last().map_or(Ok(Type::Null), |stmt| stmt.get_type())
            }
            Expr::BlockExpr(statements) => statements
                .last()
                .map_or(Ok(Type::Null), |stmt| stmt.get_type()),
            Expr::ForeverLoopExpr(statements) => statements
                .last()
                .map_or(Ok(Type::Null), |stmt| stmt.get_type()),
            Expr::SpecialNull => Ok(Type::Null),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Identifier(keyword) => write!(f, "{:?}", keyword),
            _ => unimplemented!(),
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
        parameters: Vec<(String, Type)>,
        name: String,
        body: Vec<Stmt>,
        is_async: bool,
    }, // Parameters, Name, Body, Is async?
}

impl ParsetimeType for Stmt {
    fn get_type(&self) -> Result<Type, ParserError> {
        match self {
            Stmt::ExprStmt(expr) => expr.get_type(),
            Stmt::DeclareStmt { .. } => Ok(Type::Null),
            Stmt::ReturnStmt(expr) => match expr {
                Some(expr) => expr.get_type(),
                None => Ok(Type::Null),
            },
            Stmt::BreakStmt(expr) => match expr {
                Some(expr) => expr.get_type(),
                None => Ok(Type::Null),
            },
            Stmt::FunctionDeclaration { .. } => Ok(Type::Null),
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
    fn get_type(&self) -> Result<Type, ParserError> {
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
