// AST logic

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

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp(UnaryOp, Box<Expr>),
    FunctionCall(String, Vec<Expr>),
}

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(Expr),
    AssignStmt(String, bool, bool, Option<Expr>), // Name, is_const, is_mutable, expr
    ReturnStmt(Option<Expr>),
    IfStmt(Expr, Vec<Stmt>, Vec<Stmt>),
}

#[derive(Debug)]
pub enum Literal {
    Integer(i64),
    String(String),
    // Other literal types...
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum UnaryOp {
    Negate,
    // Other unary operators...
}
