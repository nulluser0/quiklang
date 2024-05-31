// Parser

use std::process;

use crate::frontend::ast::{BinaryOp, Program, Property};

use super::{
    ast::{Expr, Literal, Stmt},
    lexer::{tokenize, Keyword, Operator, Symbol, Token},
};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
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
        self.tokens.remove(0) as Token
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
            Token::Keyword(Keyword::Let) => self.parse_var_declaration(false),
            Token::Keyword(Keyword::Const) => self.parse_var_declaration(true),
            // Token::Identifier(_) => todo!(),
            // Token::IntegerLiteral(_) => todo!(),
            // Token::StringLiteral(_) => todo!(),
            // Token::Operator(_) => todo!(),
            // Token::Symbol(_) => todo!(),
            // Token::EOF => todo!(),
            _ => Stmt::ExprStmt(self.parse_expr()),
        }
    }

    // `let (mut) ident(: type) = expr`
    fn parse_var_declaration(&mut self, is_const: bool) -> Stmt {
        let _ = self.eat(); // remove unneeded let/const token.
        let is_mutable = match self.at() {
            Token::Keyword(Keyword::Mut) => {
                let _ = self.eat(); // We now know mut keyword us used. Advance.
                if is_const {
                    panic!("Cannot apply mutability `mut` to a constant `const`!");
                }
                true
            }
            _ => false, // No mut keyword, variable is immutable.
        };

        let identifier = match self.eat() {
            Token::Identifier(name) => Token::Identifier(name),
            _ => panic!("Expected identifier name following let keyword."),
        };

        // TODO: Implement type support!

        if *self.at() == Token::Symbol(Symbol::Semicolon) {
            self.eat();
            if is_const {
                panic!("Must assign value to `const` expression. No value provided.");
            }
            return Stmt::DeclareStmt(identifier.to_string(), is_mutable, None);
        }

        self.expect(
            Token::Operator(Operator::Assign),
            "Expected assign token `=` following identifier in var declaration.",
        );
        let declaration =
            Stmt::DeclareStmt(identifier.to_string(), is_mutable, Some(self.parse_expr()));
        self.expect(
            Token::Symbol(Symbol::Semicolon),
            "Variable declaration is a statement. It must end with a semicolon.",
        );
        declaration
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_assignment_expr()
    }

    fn parse_assignment_expr(&mut self) -> Expr {
        let left = self.parse_object_expr();
        if *self.at() == Token::Operator(Operator::Assign) {
            let _ = self.eat(); // Advance after.
            let value = self.parse_assignment_expr();
            return Expr::AssignmentExpr(Box::new(left), Box::new(value));
        }
        left
    }

    fn parse_object_expr(&mut self) -> Expr {
        if *self.at() != Token::Symbol(Symbol::LeftBrace) {
            return self.parse_additive_expr();
        }
        self.eat(); // advance past leftbrace
        let mut properties: Vec<Property> = Vec::new();
        while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
            let key = match self.eat() {
                Token::Identifier(name) => name,
                _ => panic!("Object literal key expected."),
            };

            // Allows shorthand key: pair -> { key, }
            if *self.at() == Token::Symbol(Symbol::Comma) {
                self.eat(); // Advance
                properties.push(Property { key, value: None });
                break;
            // Allows shorthand key: pair -> { key }
            } else if *self.at() == Token::Symbol(Symbol::RightBrace) {
                properties.push(Property { key, value: None });
                break;
            }

            // { key: val }
            self.expect(
                Token::Symbol(Symbol::Colon),
                "Missing colon following identifier in Object Expression",
            );
            let value = self.parse_expr();
            properties.push(Property {
                key,
                value: Some(value),
            });
            if *self.at() != Token::Symbol(Symbol::RightBrace) {
                self.expect(
                    Token::Symbol(Symbol::Comma),
                    "Expected comma or Right Brace following property.",
                );
            }
        }
        self.expect(
            Token::Symbol(Symbol::RightBrace),
            "Object literal missing right brace `}`.",
        );
        Expr::Literal(Literal::Object(properties))
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
        let mut left = self.parse_call_member_expr();

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
            let right = self.parse_call_member_expr();
            left = Expr::BinaryOp {
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_call_member_expr(&mut self) -> Expr {
        let member = self.parse_member_expr();
        if *self.at() == Token::Symbol(Symbol::LeftParen) {
            return self.parse_call_expr(member);
        }
        member
    }

    fn parse_call_expr(&mut self, caller: Expr) -> Expr {
        let mut call_expr: Expr = Expr::FunctionCall(self.parse_args(), Box::new(caller));
        if *self.at() == Token::Symbol(Symbol::LeftParen) {
            call_expr = self.parse_call_expr(call_expr);
        }
        call_expr
    }

    fn parse_args(&mut self) -> Vec<Expr> {
        self.expect(Token::Symbol(Symbol::LeftParen), "Expected left paren.");
        let args = match *self.at() == Token::Symbol(Symbol::RightParen) {
            true => vec![],
            false => self.parse_arguments_list(),
        };
        self.expect(
            Token::Symbol(Symbol::RightParen),
            "Missing right paren inside arguments list.",
        );
        args
    }

    fn parse_arguments_list(&mut self) -> Vec<Expr> {
        let mut args = vec![self.parse_expr()];
        while *self.at() == Token::Symbol(Symbol::Comma) && self.not_eof() {
            self.eat();
            args.push(self.parse_assignment_expr());
        }
        args
    }

    fn parse_member_expr(&mut self) -> Expr {
        let mut object = self.parse_primary_expr();
        while *self.at() == Token::Symbol(Symbol::Dot)
            || *self.at() == Token::Symbol(Symbol::LeftBracket)
        {
            let operator = self.eat();
            let property: Expr;
            let computed: bool;

            // Non-computed properties (like obj.expr):
            if operator == Token::Symbol(Symbol::Dot) {
                computed = false;
                // Get identifier
                property = self.parse_primary_expr();
                match property {
                    Expr::Identifier(_) => {}
                    _ => panic!(
                        "Cannot use dot operator without right hand side being an identifier."
                    ),
                }
            } else {
                // This allows obj[computed value]
                computed = true;
                property = self.parse_expr();
                self.expect(
                    Token::Symbol(Symbol::RightBracket),
                    "Missing right bracket in computed value.",
                );
            }
            object = Expr::Member(Box::new(object), Box::new(property), computed);
        }
        object
    }

    fn parse_primary_expr(&mut self) -> Expr {
        let tk = self.eat();
        match tk {
            // Token::Keyword(_) => todo!(),
            Token::Identifier(name) => Expr::Identifier(name.to_string()) as Expr,
            Token::IntegerLiteral(integer) => Expr::Literal(Literal::Integer(integer)) as Expr,
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
