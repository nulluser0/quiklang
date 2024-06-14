// Parser

use core::panic;
use std::process;

use crate::frontend::ast::{BinaryOp, Program, Property};

use super::{
    ast::{Expr, Literal, Stmt, UnaryOp},
    lexer::{tokenize, Keyword, Operator, Symbol, Token},
};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    inside_loop: bool,
    inside_function: bool,
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser {
    pub fn new() -> Self {
        Parser {
            tokens: Vec::new(),
            inside_loop: false,
            inside_function: false,
        }
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
            Token::Keyword(Keyword::Fn) => self.parse_fn_declaration(false),
            Token::Keyword(Keyword::Async) => {
                self.eat();
                match self.at() {
                    Token::Keyword(Keyword::Fn) => self.parse_fn_declaration(true),
                    _ => panic!("Async: `fn` missing!"),
                }
            }
            Token::Keyword(Keyword::Break) => self.parse_break_declaration(),
            Token::Keyword(Keyword::Return) => self.parse_return_declaration(),
            // Token::Identifier(_) => todo!(),
            // Token::IntegerLiteral(_) => todo!(),
            // Token::StringLiteral(_) => todo!(),
            // Token::Operator(_) => todo!(),
            // Token::Symbol(_) => todo!(),
            // Token::EOF => todo!(),
            _ => Stmt::ExprStmt(self.parse_expr()),
        }
    }

    fn parse_break_declaration(&mut self) -> Stmt {
        self.eat();
        if !self.inside_loop {
            panic!("`break` statement found outside of a loop (while, for, loop) context.")
        }
        if *self.at() == Token::Symbol(Symbol::Semicolon) {
            self.eat();
            return Stmt::BreakStmt(None);
        }
        let expr = self.parse_expr();
        self.expect(
            Token::Symbol(Symbol::Semicolon),
            "break declaration is a statement. It must end with a semicolon.",
        );
        Stmt::BreakStmt(Some(expr))
    }

    fn parse_return_declaration(&mut self) -> Stmt {
        self.eat();
        if !self.inside_function {
            panic!("`return` statement found outside of a function context.")
        }
        if *self.at() == Token::Symbol(Symbol::Semicolon) {
            self.eat();
            return Stmt::ReturnStmt(None);
        }
        let expr = self.parse_expr();
        self.expect(
            Token::Symbol(Symbol::Semicolon),
            "return declaration is a statement. It must end with a semicolon.",
        );
        Stmt::ReturnStmt(Some(expr))
    }

    fn parse_fn_declaration(&mut self, is_async: bool) -> Stmt {
        self.eat();
        let name = match self.eat() {
            Token::Identifier(name) => name,
            _ => panic!("No identifier for fn!"),
        };
        let args: Vec<Expr> = self.parse_args();
        let mut params: Vec<String> = Vec::new();
        for arg in args {
            match arg {
                Expr::Identifier(name) => params.push(name),
                _ => panic!(
                    "Inside function declaration expected parameters to be one of type String."
                ),
            }
        }
        self.expect(
            Token::Symbol(Symbol::LeftBrace),
            "Expected function body following declaration.",
        );
        let prev_inside_function = self.inside_function;
        self.inside_function = true;
        let mut body: Vec<Stmt> = Vec::new();
        while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
            body.push(self.parse_stmt());
        }
        self.inside_function = prev_inside_function;
        self.expect(
            Token::Symbol(Symbol::RightBrace),
            "Closing brace expected inside function declaration.",
        );
        Stmt::FunctionDeclaration {
            parameters: params,
            name,
            body,
            is_async,
        }
    }

    // `let (global) (mut) ident(: type) = expr`
    fn parse_var_declaration(&mut self, is_const: bool) -> Stmt {
        let _ = self.eat(); // remove unneeded let/const token.
        let is_global = match self.at() {
            Token::Keyword(Keyword::Global) => {
                let _ = self.eat(); // We now know global keyword us used. Advance.
                true
            }
            _ => false, // No mut keyword, variable is immutable.
        };

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
            return Stmt::DeclareStmt {
                name: identifier.to_string(),
                is_mutable,
                is_global,
                expr: None,
            };
        }

        self.expect(
            Token::Operator(Operator::Assign),
            "Expected assign token `=` following identifier in var declaration.",
        );
        let declaration = Stmt::DeclareStmt {
            name: identifier.to_string(),
            is_mutable,
            is_global,
            expr: Some(self.parse_expr()),
        };
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
            return Expr::AssignmentExpr {
                assignee: Box::new(left),
                expr: Box::new(value),
            };
        }
        left
    }

    fn parse_object_expr(&mut self) -> Expr {
        if *self.at() != Token::Symbol(Symbol::LeftBrace) {
            return self.parse_relational_expr();
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

    fn parse_relational_expr(&mut self) -> Expr {
        let mut left = self.parse_concatenation_expr();
        while matches!(
            self.at(),
            Token::Operator(Operator::GreaterThan)
                | Token::Operator(Operator::LessThan)
                | Token::Operator(Operator::GreaterOrEqual)
                | Token::Operator(Operator::LessOrEqual)
                | Token::Operator(Operator::Equal)
                | Token::Operator(Operator::NotEqual)
        ) {
            let operator_astoken = self.eat();
            let operator: BinaryOp = match operator_astoken {
                Token::Operator(Operator::GreaterThan) => BinaryOp::GreaterThan,
                Token::Operator(Operator::LessThan) => BinaryOp::LessThan,
                Token::Operator(Operator::GreaterOrEqual) => BinaryOp::GreaterOrEqual,
                Token::Operator(Operator::LessOrEqual) => BinaryOp::LessOrEqual,
                Token::Operator(Operator::Equal) => BinaryOp::Equal,
                Token::Operator(Operator::NotEqual) => BinaryOp::NotEqual,
                _ => {
                    println!("Token is not an operator! {:#?}", operator_astoken);
                    process::exit(1);
                }
            };
            let right = self.parse_concatenation_expr();
            left = Expr::BinaryOp {
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_concatenation_expr(&mut self) -> Expr {
        let mut left = self.parse_additive_expr();

        while matches!(self.at(), Token::Operator(Operator::Concat)) {
            self.eat();
            let right = self.parse_additive_expr();
            left = Expr::ConcatOp {
                left: Box::new(left),
                right: Box::new(right),
            }
        }
        left
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

            // obj.expr
            if operator == Token::Symbol(Symbol::Dot) {
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
                property = self.parse_expr();
                self.expect(
                    Token::Symbol(Symbol::RightBracket),
                    "Missing right bracket in computed value.",
                );
            }
            object = Expr::Member(Box::new(object), Box::new(property));
        }
        object
    }

    fn parse_primary_expr(&mut self) -> Expr {
        let tk = self.eat();
        match tk {
            // Keyword
            Token::Keyword(Keyword::If) => self.parse_if_expr() as Expr,
            Token::Keyword(Keyword::While) => self.parse_while_expr() as Expr,
            Token::Keyword(Keyword::Loop) => self.parse_loop_expr() as Expr,
            Token::Keyword(Keyword::Block) => self.parse_block_expr() as Expr,
            // Identifier
            Token::Identifier(name) => Expr::Identifier(name.to_string()) as Expr,
            // Literals
            Token::StringLiteral(string) => Expr::Literal(Literal::String(string)) as Expr,
            Token::IntegerLiteral(integer) => Expr::Literal(Literal::Integer(integer)) as Expr,
            Token::FloatLiteral(float) => Expr::Literal(Literal::Float(float)) as Expr,
            // Token::StringLiteral(_) => todo!(),
            // Unary Operators
            Token::Operator(Operator::LogicalNot) => {
                let expr = self.parse_call_member_expr();
                Expr::UnaryOp(UnaryOp::LogicalNot, Box::new(expr))
            }
            Token::Operator(Operator::Subtract) => {
                let expr = self.parse_call_member_expr();
                Expr::UnaryOp(UnaryOp::ArithmeticNegative, Box::new(expr))
            }
            Token::Operator(Operator::Add) => {
                let expr = self.parse_call_member_expr();
                Expr::UnaryOp(UnaryOp::ArithmeticPositive, Box::new(expr))
            }
            Token::Operator(Operator::BitwiseNot) => {
                let expr = self.parse_call_member_expr();
                Expr::UnaryOp(UnaryOp::BitwiseNot, Box::new(expr))
            }
            // Symbols
            Token::Symbol(Symbol::LeftParen) => {
                let value = self.parse_expr();
                self.expect(
                    Token::Symbol(Symbol::RightParen),
                    "Unexpected token found inside parenthesised expression. Expected closing parenthesis."
                ); // rightParen
                value
            }
            Token::Symbol(Symbol::LeftBracket) => {
                let mut elements: Vec<Expr> = Vec::new();
                if self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBracket) {
                    // Parse elements
                    elements.push(self.parse_expr());
                    while *self.at() == Token::Symbol(Symbol::Comma) {
                        self.eat(); // Consume comma
                        elements.push(self.parse_expr());
                    }
                }
                self.expect(
                    Token::Symbol(Symbol::RightBracket),
                    "Expected right bracket after array literal.",
                );
                Expr::Array(elements)
            }
            Token::Symbol(Symbol::Semicolon) => Expr::SpecialNull,
            // Token::EOF => todo!(),
            _ => {
                println!("Unexpected token found during parsing! {:#?}", tk);
                process::exit(1);
            }
        }
    }

    fn parse_while_expr(&mut self) -> Expr {
        let condition = self.parse_expr();
        self.expect(
            Token::Symbol(Symbol::LeftBrace),
            "Expected left brace before `while` expression.",
        );

        let prev_inside_loop = self.inside_loop; // Save previous context.
        self.inside_loop = true; // We are now in a loop context. Modify parser.
        let mut statements: Vec<Stmt> = Vec::new();
        while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
            statements.push(self.parse_stmt());
        }
        self.inside_loop = prev_inside_loop; // Restore previous context
        self.expect(
            Token::Symbol(Symbol::RightBrace),
            "Expected right brace after `while` expression.",
        );
        Expr::WhileExpr {
            condition: Box::new(condition),
            then: statements,
        }
    }

    fn parse_block_expr(&mut self) -> Expr {
        self.expect(
            Token::Symbol(Symbol::LeftBrace),
            "Expected left brace before `block` expression.",
        );
        let mut statements: Vec<Stmt> = Vec::new();
        while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
            statements.push(self.parse_stmt());
        }
        self.expect(
            Token::Symbol(Symbol::RightBrace),
            "Expected right brace after `while` expression.",
        );
        Expr::BlockExpr(statements)
    }

    fn parse_loop_expr(&mut self) -> Expr {
        self.expect(
            Token::Symbol(Symbol::LeftBrace),
            "Expected left brace before `loop` expression.",
        );
        let prev_inside_loop = self.inside_loop; // Save previous context.
        self.inside_loop = true; // We are now in a loop context. Modify parser.
        let mut statements: Vec<Stmt> = Vec::new();
        while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
            statements.push(self.parse_stmt());
        }
        self.inside_loop = prev_inside_loop; // Restore previous context
        self.expect(
            Token::Symbol(Symbol::RightBrace),
            "Expected right brace after `loop` expression.",
        );
        Expr::ForeverLoopExpr(statements)
    }

    fn parse_if_expr(&mut self) -> Expr {
        let condition = self.parse_expr();
        self.expect(
            Token::Symbol(Symbol::LeftBrace),
            "Expected left brace before `if` expression.",
        );
        let mut consequent: Vec<Stmt> = Vec::new();
        while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
            consequent.push(self.parse_stmt());
        }
        self.expect(
            Token::Symbol(Symbol::RightBrace),
            "Expected right brace after `if` expression.",
        );
        let mut alternative: Option<Vec<Stmt>> = None;
        if *self.at() == Token::Keyword(Keyword::Else) {
            self.eat(); // Advance from else
            if *self.at() == Token::Keyword(Keyword::If) {
                alternative = Some(vec![Stmt::ExprStmt(self.parse_if_expr())])
            } else {
                self.expect(
                    Token::Symbol(Symbol::LeftBrace),
                    "Expected left brace before `else` expression.",
                );
                let mut else_block: Vec<Stmt> = Vec::new();
                while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
                    else_block.push(self.parse_stmt());
                }
                self.expect(
                    Token::Symbol(Symbol::RightBrace),
                    "Expected right brace after `else` expression.",
                );
                alternative = Some(else_block);
            }
        }
        Expr::IfExpr {
            condition: Box::new(condition),
            then: consequent,
            else_stmt: alternative,
        }
    }

    pub fn produce_ast(&mut self, source_code: String) -> Result<Program, String> {
        self.tokens = tokenize(&source_code);
        let mut program = Program::new(Vec::new());

        while self.not_eof() {
            program.statements.push(self.parse_stmt()) // SHOULD BE PARSE_STMT!!!
        }

        Ok(program)
    }
}
