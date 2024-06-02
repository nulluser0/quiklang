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

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    AssignmentExpr(Box<Expr>, Box<Expr>), // Assignee, Expr
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp(UnaryOp, Box<Expr>),
    FunctionCall(Vec<Expr>, Box<Expr>), // Args, Caller
    Member(Box<Expr>, Box<Expr>, bool), // Object, Property, Computed?
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
    DeclareStmt(String, bool, Option<Expr>), // Name, is_mutable, expr
    ReturnStmt(Option<Expr>),
    FunctionDeclaration(Vec<String>, String, Vec<Stmt>, bool), // Parameters, Name, Body, Is async?
    IfStmt(Expr, Vec<Stmt>, Option<Vec<Stmt>>),                // Condition, Then, Else
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Object(Vec<Property>),
    // Other literal types...
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

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Negate,
    // Other unary operators...
}
