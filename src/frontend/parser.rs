// Parser

use crate::{
    errors::ParserError,
    frontend::ast::{BinaryOp, Program, Property},
};

use super::{
    ast::{Expr, Literal, Stmt, UnaryOp},
    lexer::{tokenize, Keyword, Operator, Symbol, Token},
};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    inside_loop: bool,
    inside_function: bool,
    position: usize,
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
            position: 0,
        }
    }

    fn not_eof(&self) -> bool {
        !self.tokens.is_empty() && self.tokens[0] != Token::EOF
    }

    fn at(&self) -> &Token {
        &self.tokens[0] as &Token
    }

    fn eat(&mut self) -> Token {
        self.position += 1;
        self.tokens.remove(0) as Token
    }

    fn expect(&mut self, token: Token, err: &str) -> Result<Token, ParserError> {
        let prev = self.tokens.remove(0) as Token;
        if prev != token {
            return Err(ParserError::UnexpectedToken {
                expected: token,
                found: prev,
                position: self.position,
                message: err.to_string(),
            });
        }
        self.position += 1;
        Ok(prev)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParserError> {
        match self.at() {
            Token::Keyword(Keyword::Let) => self.parse_var_declaration(false),
            Token::Keyword(Keyword::Const) => self.parse_var_declaration(true),
            Token::Keyword(Keyword::Fn) => self.parse_fn_declaration(false),
            Token::Keyword(Keyword::Async) => {
                self.eat();
                match self.at() {
                    Token::Keyword(Keyword::Fn) => self.parse_fn_declaration(true),
                    _ => Err(ParserError::MissingAsyncFn),
                }
            }
            Token::Keyword(Keyword::Break) => self.parse_break_declaration(),
            Token::Keyword(Keyword::Return) => self.parse_return_declaration(),
            _ => Ok(Stmt::ExprStmt(self.parse_expr()?)),
        }
    }

    fn parse_break_declaration(&mut self) -> Result<Stmt, ParserError> {
        self.eat();
        if !self.inside_loop {
            return Err(ParserError::BreakOutsideLoop(self.position));
        }
        if *self.at() == Token::Symbol(Symbol::Semicolon) {
            self.eat();
            return Ok(Stmt::BreakStmt(None));
        }
        let expr = self.parse_expr()?;
        self.expect(
            Token::Symbol(Symbol::Semicolon),
            "break declaration is a statement. It must end with a semicolon.",
        )?;
        Ok(Stmt::BreakStmt(Some(expr)))
    }

    fn parse_return_declaration(&mut self) -> Result<Stmt, ParserError> {
        self.eat();
        if !self.inside_function {
            return Err(ParserError::ReturnOutsideFunction(self.position));
        }
        if *self.at() == Token::Symbol(Symbol::Semicolon) {
            self.eat();
            return Ok(Stmt::ReturnStmt(None));
        }
        let expr = self.parse_expr()?;
        self.expect(
            Token::Symbol(Symbol::Semicolon),
            "return declaration is a statement. It must end with a semicolon.",
        )?;
        Ok(Stmt::ReturnStmt(Some(expr)))
    }

    fn parse_fn_declaration(&mut self, is_async: bool) -> Result<Stmt, ParserError> {
        self.eat();
        let name = match self.eat() {
            Token::Identifier(name) => name,
            _ => return Err(ParserError::MissingFunctionIdentifier(self.position)),
        };
        let args: Vec<Expr> = self.parse_args()?;
        let mut params: Vec<String> = Vec::new();
        for arg in args {
            match arg {
                Expr::Identifier(name) => params.push(name),
                _ => return Err(ParserError::InvalidFunctionParameter(self.position)),
            }
        }
        self.expect(
            Token::Symbol(Symbol::LeftBrace),
            "Expected function body following declaration.",
        )?;
        let prev_inside_function = self.inside_function;
        self.inside_function = true;
        let mut body: Vec<Stmt> = Vec::new();
        while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
            body.push(self.parse_stmt()?);
        }
        self.inside_function = prev_inside_function;
        self.expect(
            Token::Symbol(Symbol::RightBrace),
            "Closing brace expected inside function declaration.",
        )?;
        Ok(Stmt::FunctionDeclaration {
            parameters: params,
            name,
            body,
            is_async,
        })
    }

    // `let (global) (mut) ident(: type) = expr`
    fn parse_var_declaration(&mut self, is_const: bool) -> Result<Stmt, ParserError> {
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
                    return Err(ParserError::MutConstConflict(self.position));
                }
                true
            }
            _ => false, // No mut keyword, variable is immutable.
        };

        let identifier = match self.eat() {
            Token::Identifier(name) => Token::Identifier(name),
            _ => return Err(ParserError::MissingIdentifier(self.position)),
        };

        if *self.at() == Token::Symbol(Symbol::Semicolon) {
            self.eat();
            if is_const {
                return Err(ParserError::ConstWithoutValue(self.position));
            }
            return Ok(Stmt::DeclareStmt {
                name: identifier.to_string(),
                is_mutable,
                is_global,
                expr: None,
            });
        }

        self.expect(
            Token::Operator(Operator::Assign),
            "Expected assign token `=` following identifier in var declaration.",
        )?;
        let declaration = Stmt::DeclareStmt {
            name: identifier.to_string(),
            is_mutable,
            is_global,
            expr: Some(self.parse_expr()?),
        };
        self.expect(
            Token::Symbol(Symbol::Semicolon),
            "Variable declaration is a statement. It must end with a semicolon.",
        )?;
        Ok(declaration)
    }

    fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        self.parse_assignment_expr()
    }

    fn parse_assignment_expr(&mut self) -> Result<Expr, ParserError> {
        let left = self.parse_object_expr()?;
        if *self.at() == Token::Operator(Operator::Assign) {
            self.eat(); // Advance after.
            let value = self.parse_assignment_expr()?;
            return Ok(Expr::AssignmentExpr {
                assignee: Box::new(left),
                expr: Box::new(value),
            });
        }
        Ok(left)
    }

    fn parse_object_expr(&mut self) -> Result<Expr, ParserError> {
        if *self.at() != Token::Symbol(Symbol::LeftBrace) {
            return self.parse_relational_expr();
        }
        self.eat(); // advance past leftbrace
        let mut properties: Vec<Property> = Vec::new();
        while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
            let key = match self.eat() {
                Token::Identifier(name) => name,
                _ => return Err(ParserError::ObjectLiteralKeyExpected(self.position)),
            };

            if *self.at() == Token::Symbol(Symbol::Comma) {
                self.eat(); // Advance
                properties.push(Property { key, value: None });
                break;
            } else if *self.at() == Token::Symbol(Symbol::RightBrace) {
                properties.push(Property { key, value: None });
                break;
            }

            self.expect(
                Token::Symbol(Symbol::Colon),
                "Missing colon following identifier in Object Expression",
            )?;
            let value = self.parse_expr()?;
            properties.push(Property {
                key,
                value: Some(value),
            });
            if *self.at() != Token::Symbol(Symbol::RightBrace) {
                self.expect(
                    Token::Symbol(Symbol::Comma),
                    "Expected comma or Right Brace following property.",
                )?;
            }
        }
        self.expect(
            Token::Symbol(Symbol::RightBrace),
            "Object literal missing right brace `}`.",
        )?;
        Ok(Expr::Literal(Literal::Object(properties)))
    }

    fn parse_relational_expr(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_concatenation_expr()?;
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
                    return Err(ParserError::InvalidOperator(
                        self.position,
                        operator_astoken.clone(),
                    ));
                }
            };
            let right = self.parse_concatenation_expr()?;
            left = Expr::BinaryOp {
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_concatenation_expr(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_additive_expr()?;

        while matches!(self.at(), Token::Operator(Operator::Concat)) {
            self.eat();
            let right = self.parse_additive_expr()?;
            left = Expr::ConcatOp {
                left: Box::new(left),
                right: Box::new(right),
            }
        }
        Ok(left)
    }

    fn parse_additive_expr(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_multiplicative_expr()?;

        while matches!(
            self.at(),
            Token::Operator(Operator::Add) | Token::Operator(Operator::Subtract)
        ) {
            let operator_astoken = self.eat();
            let operator: BinaryOp = match operator_astoken {
                Token::Operator(op) => op.into(),
                _ => {
                    return Err(ParserError::InvalidOperator(
                        self.position,
                        operator_astoken.clone(),
                    ));
                }
            };
            let right = self.parse_multiplicative_expr()?;
            left = Expr::BinaryOp {
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_multiplicative_expr(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_call_member_expr()?;

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
                    return Err(ParserError::InvalidOperator(
                        self.position,
                        operator_astoken.clone(),
                    ));
                }
            };
            let right = self.parse_call_member_expr()?;
            left = Expr::BinaryOp {
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_call_member_expr(&mut self) -> Result<Expr, ParserError> {
        let member = self.parse_member_expr()?;
        if *self.at() == Token::Symbol(Symbol::LeftParen) {
            return self.parse_call_expr(member);
        }
        Ok(member)
    }

    fn parse_call_expr(&mut self, caller: Expr) -> Result<Expr, ParserError> {
        let mut call_expr: Expr = Expr::FunctionCall(self.parse_args()?, Box::new(caller));
        if *self.at() == Token::Symbol(Symbol::LeftParen) {
            call_expr = self.parse_call_expr(call_expr)?;
        }
        Ok(call_expr)
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>, ParserError> {
        self.expect(Token::Symbol(Symbol::LeftParen), "Expected left paren.")?;
        let args = match *self.at() == Token::Symbol(Symbol::RightParen) {
            true => vec![],
            false => self.parse_arguments_list()?,
        };
        self.expect(
            Token::Symbol(Symbol::RightParen),
            "Missing right paren inside arguments list.",
        )?;
        Ok(args)
    }

    fn parse_arguments_list(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut args = vec![self.parse_expr()?];
        while *self.at() == Token::Symbol(Symbol::Comma) && self.not_eof() {
            self.eat();
            args.push(self.parse_assignment_expr()?);
        }
        Ok(args)
    }

    fn parse_member_expr(&mut self) -> Result<Expr, ParserError> {
        let mut object = self.parse_primary_expr()?;
        while *self.at() == Token::Symbol(Symbol::Dot)
            || *self.at() == Token::Symbol(Symbol::LeftBracket)
        {
            let operator = self.eat();
            let property: Expr;

            // obj.expr
            if operator == Token::Symbol(Symbol::Dot) {
                // Get identifier
                property = self.parse_primary_expr()?;
                match property {
                    Expr::Identifier(_) => {}
                    _ => {
                        return Err(ParserError::InvalidDotProperty(
                            self.position,
                            property.clone(),
                        ));
                    }
                }
            } else {
                // This allows obj[computed value]
                property = self.parse_expr()?;
                self.expect(
                    Token::Symbol(Symbol::RightBracket),
                    "Missing right bracket in computed value.",
                )?;
            }
            object = Expr::Member(Box::new(object), Box::new(property));
        }
        Ok(object)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, ParserError> {
        let tk = self.eat();
        match tk {
            // Keyword
            Token::Keyword(Keyword::If) => self.parse_if_expr() as Result<Expr, ParserError>,
            Token::Keyword(Keyword::While) => self.parse_while_expr() as Result<Expr, ParserError>,
            Token::Keyword(Keyword::Loop) => self.parse_loop_expr() as Result<Expr, ParserError>,
            Token::Keyword(Keyword::Block) => self.parse_block_expr() as Result<Expr, ParserError>,
            // Identifier
            Token::Identifier(name) => {
                Ok(Expr::Identifier(name.to_string())) as Result<Expr, ParserError>
            }
            // Literals
            Token::StringLiteral(string) => {
                Ok(Expr::Literal(Literal::String(string))) as Result<Expr, ParserError>
            }
            Token::IntegerLiteral(integer) => {
                Ok(Expr::Literal(Literal::Integer(integer))) as Result<Expr, ParserError>
            }
            Token::FloatLiteral(float) => {
                Ok(Expr::Literal(Literal::Float(float))) as Result<Expr, ParserError>
            }
            // Unary Operators
            Token::Operator(Operator::LogicalNot) => {
                let expr = self.parse_call_member_expr()?;
                Ok(Expr::UnaryOp(UnaryOp::LogicalNot, Box::new(expr)))
            }
            Token::Operator(Operator::Subtract) => {
                let expr = self.parse_call_member_expr()?;
                Ok(Expr::UnaryOp(UnaryOp::ArithmeticNegative, Box::new(expr)))
            }
            Token::Operator(Operator::Add) => {
                let expr = self.parse_call_member_expr()?;
                Ok(Expr::UnaryOp(UnaryOp::ArithmeticPositive, Box::new(expr)))
            }
            Token::Operator(Operator::BitwiseNot) => {
                let expr = self.parse_call_member_expr()?;
                Ok(Expr::UnaryOp(UnaryOp::BitwiseNot, Box::new(expr)))
            }
            // Symbols
            Token::Symbol(Symbol::LeftParen) => {
                let value = self.parse_expr()?;
                self.expect(
                    Token::Symbol(Symbol::RightParen),
                    "Unexpected token found inside parenthesised expression. Expected closing parenthesis."
                )?; // rightParen
                Ok(value)
            }
            Token::Symbol(Symbol::LeftBracket) => {
                let mut elements: Vec<Expr> = Vec::new();
                if self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBracket) {
                    // Parse elements
                    elements.push(self.parse_expr()?);
                    while *self.at() == Token::Symbol(Symbol::Comma) {
                        self.eat(); // Consume comma
                        elements.push(self.parse_expr()?);
                    }
                }
                self.expect(
                    Token::Symbol(Symbol::RightBracket),
                    "Expected right bracket after array literal.",
                )?;
                Ok(Expr::Array(elements))
            }
            Token::Symbol(Symbol::Semicolon) => Ok(Expr::SpecialNull),
            _ => Err(ParserError::UnexpectedToken {
                expected: Token::EOF,
                found: tk.clone(),
                position: self.position,
                message: format!("Unexpected token found during parsing! {:?}", tk),
            }),
        }
    }

    fn parse_while_expr(&mut self) -> Result<Expr, ParserError> {
        let condition = self.parse_expr()?;
        self.expect(
            Token::Symbol(Symbol::LeftBrace),
            "Expected left brace before `while` expression.",
        )?;

        let prev_inside_loop = self.inside_loop; // Save previous context.
        self.inside_loop = true; // We are now in a loop context. Modify parser.
        let mut statements: Vec<Stmt> = Vec::new();
        while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
            statements.push(self.parse_stmt()?);
        }
        self.inside_loop = prev_inside_loop; // Restore previous context
        self.expect(
            Token::Symbol(Symbol::RightBrace),
            "Expected right brace after `while` expression.",
        )?;
        Ok(Expr::WhileExpr {
            condition: Box::new(condition),
            then: statements,
        })
    }

    fn parse_block_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect(
            Token::Symbol(Symbol::LeftBrace),
            "Expected left brace before `block` expression.",
        )?;
        let mut statements: Vec<Stmt> = Vec::new();
        while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
            statements.push(self.parse_stmt()?);
        }
        self.expect(
            Token::Symbol(Symbol::RightBrace),
            "Expected right brace after `block` expression.",
        )?;
        Ok(Expr::BlockExpr(statements))
    }

    fn parse_loop_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect(
            Token::Symbol(Symbol::LeftBrace),
            "Expected left brace before `loop` expression.",
        )?;
        let prev_inside_loop = self.inside_loop; // Save previous context.
        self.inside_loop = true; // We are now in a loop context. Modify parser.
        let mut statements: Vec<Stmt> = Vec::new();
        while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
            statements.push(self.parse_stmt()?);
        }
        self.inside_loop = prev_inside_loop; // Restore previous context
        self.expect(
            Token::Symbol(Symbol::RightBrace),
            "Expected right brace after `loop` expression.",
        )?;
        Ok(Expr::ForeverLoopExpr(statements))
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParserError> {
        let condition = self.parse_expr()?;
        self.expect(
            Token::Symbol(Symbol::LeftBrace),
            "Expected left brace before `if` expression.",
        )?;
        let mut consequent: Vec<Stmt> = Vec::new();
        while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
            consequent.push(self.parse_stmt()?);
        }
        self.expect(
            Token::Symbol(Symbol::RightBrace),
            "Expected right brace after `if` expression.",
        )?;
        let mut alternative: Option<Vec<Stmt>> = None;
        if *self.at() == Token::Keyword(Keyword::Else) {
            self.eat(); // Advance from else
            if *self.at() == Token::Keyword(Keyword::If) {
                self.eat();
                alternative = Some(vec![Stmt::ExprStmt(self.parse_if_expr()?)])
            } else {
                self.expect(
                    Token::Symbol(Symbol::LeftBrace),
                    "Expected left brace before `else` expression.",
                )?;
                let mut else_block: Vec<Stmt> = Vec::new();
                while self.not_eof() && *self.at() != Token::Symbol(Symbol::RightBrace) {
                    else_block.push(self.parse_stmt()?);
                }
                self.expect(
                    Token::Symbol(Symbol::RightBrace),
                    "Expected right brace after `else` expression.",
                )?;
                alternative = Some(else_block);
            }
        }
        Ok(Expr::IfExpr {
            condition: Box::new(condition),
            then: consequent,
            else_stmt: alternative,
        })
    }

    pub fn produce_ast(&mut self, source_code: String) -> Result<Program, ParserError> {
        self.tokens = tokenize(&source_code);
        let mut program = Program::new(Vec::new());

        while self.not_eof() {
            program.statements.push(self.parse_stmt()?); // SHOULD BE PARSE_STMT!!!
        }

        Ok(program)
    }
}
