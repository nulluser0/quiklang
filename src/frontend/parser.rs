// Parser

use std::process;

use crate::frontend::ast::{BinaryOp, Program};

use super::{
    ast::{Expr, Literal, Stmt},
    lexer::{tokenize, Operator, Symbol, Token},
};

pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new() -> Self {
        Parser { tokens: Vec::new() }
    }

    fn not_eof(&self) -> bool {
        !self.tokens.is_empty() && self.tokens[0] != Token::EOF
    }

    fn at(&self) -> &Token {
        &self.tokens[0] as &Token
    }

    fn eat(&mut self) -> Token {
        return self.tokens.remove(0) as Token;
    }

    fn expect(&mut self, token: Token, err: &str) -> Token {
        let prev = self.tokens.remove(0) as Token;
        if prev != token {
            println!("Parser error:\n{} {:?} - Expecting: {:?}", err, prev, token);
            process::exit(1);
        }
        prev
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.at() {
            _ => Stmt::ExprStmt(self.parse_expr()),
        }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_additive_expr()
    }

    fn parse_additive_expr(&mut self) -> Expr {
        let mut left = self.parse_multiplicative_expr();

        while matches!(
            self.at(),
            Token::Operator(Operator::Add) | Token::Operator(Operator::Subtract)
        ) {
            let operator_astoken = self.eat();
            let operator: BinaryOp = match operator_astoken {
                Token::Operator(op) => op.into(),
                _ => {
                    println!("Token is not an operator! {:#?}", operator_astoken);
                    process::exit(1);
                }
            };
            let right = self.parse_multiplicative_expr();
            left = Expr::BinaryOp {
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_multiplicative_expr(&mut self) -> Expr {
        let mut left = self.parse_primary_expr();

        while matches!(
            self.at(),
            Token::Operator(Operator::Multiply)
                | Token::Operator(Operator::Divide)
                | Token::Operator(Operator::Modulus)
        ) {
            let operator_astoken = self.eat();
            let operator: BinaryOp = match operator_astoken {
                Token::Operator(op) => op.into(),
                _ => {
                    println!("Token is not an operator! {:#?}", operator_astoken);
                    process::exit(1);
                }
            };
            let right = self.parse_primary_expr();
            left = Expr::BinaryOp {
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        left
    }

    // Orders of prescidence:
    // AssignmentExpr
    // MemberExpr
    // FunctionCall
    // LogicalExpr
    // ComparisonExpr
    // AdditiveExpr (+ or -)
    // MultiplicativeExpr (* or /)
    // UnaryExpr
    // PrimaryExpr

    fn parse_primary_expr(&mut self) -> Expr {
        let tk = self.eat();
        match tk {
            // Token::Keyword(_) => todo!(),
            Token::Identifier(name) => return Expr::Identifier(name.to_string()) as Expr,
            Token::IntegerLiteral(integer) => {
                return Expr::Literal(Literal::Integer(integer)) as Expr
            }
            // Token::StringLiteral(_) => todo!(),
            // Token::Operator(_) => todo!(),
            Token::Symbol(Symbol::LeftParen) => {
                let value = self.parse_expr();
                self.expect(
                    Token::Symbol(Symbol::RightParen),
                    "Unexpected token found inside parenthesised expression. Expected closing parenthesis."
                ); // rightParen
                value
            }
            // Token::EOF => todo!(),
            _ => {
                println!("Unexpected token found during parsing! {:#?}", tk);
                process::exit(1);
            }
        }
    }

    pub fn produce_ast(&mut self, source_code: String) -> Result<Program, String> {
        self.tokens = tokenize(source_code);
        let mut program = Program::new(Vec::new());

        while self.not_eof() {
            program.statements.push(self.parse_stmt()) // SHOULD BE PARSE_STMT!!!
        }

        Ok(program)
    }
}
