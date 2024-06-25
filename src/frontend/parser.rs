// Parser

use crate::{
    errors::{Error, ParserError},
    frontend::ast::{BinaryOp, Program, Property},
};

use super::{
    ast::{Expr, Literal, Stmt, UnaryOp},
    lexer::{tokenize, Keyword, Operator, Symbol, Token, TokenType},
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
        !self.tokens.is_empty() && self.tokens[0].token != TokenType::EOF
    }

    fn at(&self) -> &Token {
        &self.tokens[0] as &Token
    }

    fn eat(&mut self) -> Token {
        self.position += 1;
        self.tokens.remove(0) as Token
    }

    fn expect(&mut self, token: TokenType, err: &str) -> Result<Token, ParserError> {
        let prev = self.tokens.remove(0);
        if prev.token != token {
            return Err(ParserError::UnexpectedToken {
                expected: token,
                found: prev.token,
                line: prev.line,
                col: prev.col,
                message: err.to_string(),
            });
        }
        self.position += 1;
        Ok(prev)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParserError> {
        match self.at().token {
            TokenType::Keyword(Keyword::Let) => self.parse_var_declaration(false),
            TokenType::Keyword(Keyword::Const) => self.parse_var_declaration(true),
            TokenType::Keyword(Keyword::Fn) => self.parse_fn_declaration(false),
            TokenType::Keyword(Keyword::Async) => {
                self.eat();
                match self.at().token {
                    TokenType::Keyword(Keyword::Fn) => self.parse_fn_declaration(true),
                    _ => Err(ParserError::MissingAsyncFn),
                }
            }
            TokenType::Keyword(Keyword::Break) => self.parse_break_declaration(),
            TokenType::Keyword(Keyword::Return) => self.parse_return_declaration(),
            _ => Ok(Stmt::ExprStmt(self.parse_expr()?)),
        }
    }

    fn parse_break_declaration(&mut self) -> Result<Stmt, ParserError> {
        let break_declaration = self.eat();
        if !self.inside_loop {
            return Err(ParserError::BreakOutsideLoop(
                break_declaration.line,
                break_declaration.col,
            ));
        }
        if self.at().token == TokenType::Symbol(Symbol::Semicolon) {
            self.eat();
            return Ok(Stmt::BreakStmt(None));
        }
        let expr = self.parse_expr()?;
        self.expect(
            TokenType::Symbol(Symbol::Semicolon),
            "break declaration is a statement. It must end with a semicolon.",
        )?;
        Ok(Stmt::BreakStmt(Some(expr)))
    }

    fn parse_return_declaration(&mut self) -> Result<Stmt, ParserError> {
        let return_declaration = self.eat();
        if !self.inside_function {
            return Err(ParserError::ReturnOutsideFunction(
                return_declaration.line,
                return_declaration.col,
            ));
        }
        if self.at().token == TokenType::Symbol(Symbol::Semicolon) {
            self.eat();
            return Ok(Stmt::ReturnStmt(None));
        }
        let expr = self.parse_expr()?;
        self.expect(
            TokenType::Symbol(Symbol::Semicolon),
            "return declaration is a statement. It must end with a semicolon.",
        )?;
        Ok(Stmt::ReturnStmt(Some(expr)))
    }

    fn parse_fn_declaration(&mut self, is_async: bool) -> Result<Stmt, ParserError> {
        self.eat();
        let ident = self.eat();
        let name = match ident.token {
            TokenType::Identifier(name) => name,
            _ => {
                return Err(ParserError::MissingFunctionIdentifier(
                    ident.line, ident.col,
                ))
            }
        };
        let args: Vec<Expr> = self.parse_args()?;
        let mut params: Vec<String> = Vec::new();
        for arg in args {
            match arg {
                Expr::Identifier(name) => params.push(name),
                _ => return Err(ParserError::InvalidFunctionParameter(ident.line, ident.col)),
            }
        }
        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected function body following declaration.",
        )?;
        let prev_inside_function = self.inside_function;
        self.inside_function = true;
        let mut body: Vec<Stmt> = Vec::new();
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            body.push(self.parse_stmt()?);
        }
        self.inside_function = prev_inside_function;
        self.expect(
            TokenType::Symbol(Symbol::RightBrace),
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
        let declaration = self.eat(); // remove unneeded let/const token.
        let is_global = match self.at().token {
            TokenType::Keyword(Keyword::Global) => {
                let _ = self.eat(); // We now know global keyword us used. Advance.
                true
            }
            _ => false, // No mut keyword, variable is immutable.
        };

        let is_mutable = match self.at().token {
            TokenType::Keyword(Keyword::Mut) => {
                let _ = self.eat(); // We now know mut keyword us used. Advance.
                if is_const {
                    return Err(ParserError::MutConstConflict(
                        declaration.line,
                        declaration.col,
                    ));
                }
                true
            }
            _ => false, // No mut keyword, variable is immutable.
        };
        let identifier_token = self.eat();
        let identifier = match identifier_token.token {
            TokenType::Identifier(name) => TokenType::Identifier(name),
            _ => {
                return Err(ParserError::MissingIdentifier(
                    identifier_token.line,
                    identifier_token.col,
                ))
            }
        };

        if self.at().token == TokenType::Symbol(Symbol::Semicolon) {
            self.eat();
            if is_const {
                return Err(ParserError::ConstWithoutValue(
                    declaration.line,
                    declaration.col,
                ));
            }
            return Ok(Stmt::DeclareStmt {
                name: identifier.to_string(),
                is_mutable,
                is_global,
                expr: None,
            });
        }

        self.expect(
            TokenType::Operator(Operator::Assign),
            "Expected assign token `=` following identifier in var declaration.",
        )?;
        let declaration = Stmt::DeclareStmt {
            name: identifier.to_string(),
            is_mutable,
            is_global,
            expr: Some(self.parse_expr()?),
        };
        self.expect(
            TokenType::Symbol(Symbol::Semicolon),
            "Variable declaration is a statement. It must end with a semicolon.",
        )?;
        Ok(declaration)
    }

    fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        self.parse_assignment_expr()
    }

    fn parse_assignment_expr(&mut self) -> Result<Expr, ParserError> {
        let left = self.parse_object_expr()?;
        if self.at().token == TokenType::Operator(Operator::Assign) {
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
        if self.at().token != TokenType::Symbol(Symbol::LeftBrace) {
            return self.parse_relational_expr();
        }
        self.eat(); // advance past leftbrace
        let mut properties: Vec<Property> = Vec::new();
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            let key_token = self.eat();
            let key = match key_token.token {
                TokenType::Identifier(name) => name,
                _ => {
                    return Err(ParserError::ObjectLiteralKeyExpected(
                        key_token.line,
                        key_token.col,
                    ))
                }
            };

            if self.at().token == TokenType::Symbol(Symbol::Comma) {
                self.eat(); // Advance
                properties.push(Property { key, value: None });
                break;
            } else if self.at().token == TokenType::Symbol(Symbol::RightBrace) {
                properties.push(Property { key, value: None });
                break;
            }

            self.expect(
                TokenType::Symbol(Symbol::Colon),
                "Missing colon following identifier in Object Expression",
            )?;
            let value = self.parse_expr()?;
            properties.push(Property {
                key,
                value: Some(value),
            });
            if self.at().token != TokenType::Symbol(Symbol::RightBrace) {
                self.expect(
                    TokenType::Symbol(Symbol::Comma),
                    "Expected comma or Right Brace following property.",
                )?;
            }
        }
        self.expect(
            TokenType::Symbol(Symbol::RightBrace),
            "Object literal missing right brace `}`.",
        )?;
        Ok(Expr::Literal(Literal::Object(properties)))
    }

    fn parse_relational_expr(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_concatenation_expr()?;
        while matches!(
            self.at().token,
            TokenType::Operator(Operator::GreaterThan)
                | TokenType::Operator(Operator::LessThan)
                | TokenType::Operator(Operator::GreaterOrEqual)
                | TokenType::Operator(Operator::LessOrEqual)
                | TokenType::Operator(Operator::Equal)
                | TokenType::Operator(Operator::NotEqual)
        ) {
            let operator_astoken = self.eat();
            let operator: BinaryOp = match operator_astoken.token {
                TokenType::Operator(Operator::GreaterThan) => BinaryOp::GreaterThan,
                TokenType::Operator(Operator::LessThan) => BinaryOp::LessThan,
                TokenType::Operator(Operator::GreaterOrEqual) => BinaryOp::GreaterOrEqual,
                TokenType::Operator(Operator::LessOrEqual) => BinaryOp::LessOrEqual,
                TokenType::Operator(Operator::Equal) => BinaryOp::Equal,
                TokenType::Operator(Operator::NotEqual) => BinaryOp::NotEqual,
                _ => {
                    return Err(ParserError::InvalidOperator(
                        operator_astoken.line,
                        operator_astoken.col,
                        operator_astoken.token,
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

        while matches!(self.at().token, TokenType::Operator(Operator::Concat)) {
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
            self.at().token,
            TokenType::Operator(Operator::Add) | TokenType::Operator(Operator::Subtract)
        ) {
            let operator_astoken = self.eat();
            let operator: BinaryOp = match operator_astoken.token {
                TokenType::Operator(op) => op.into(),
                _ => {
                    return Err(ParserError::InvalidOperator(
                        operator_astoken.line,
                        operator_astoken.col,
                        operator_astoken.token,
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
            self.at().token,
            TokenType::Operator(Operator::Multiply)
                | TokenType::Operator(Operator::Divide)
                | TokenType::Operator(Operator::Modulus)
        ) {
            let operator_astoken = self.eat();
            let operator: BinaryOp = match operator_astoken.token {
                TokenType::Operator(op) => op.into(),
                _ => {
                    return Err(ParserError::InvalidOperator(
                        operator_astoken.line,
                        operator_astoken.col,
                        operator_astoken.token,
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
        if self.at().token == TokenType::Symbol(Symbol::LeftParen) {
            return self.parse_call_expr(member);
        }
        Ok(member)
    }

    fn parse_call_expr(&mut self, caller: Expr) -> Result<Expr, ParserError> {
        let mut call_expr: Expr = Expr::FunctionCall(self.parse_args()?, Box::new(caller));
        if self.at().token == TokenType::Symbol(Symbol::LeftParen) {
            call_expr = self.parse_call_expr(call_expr)?;
        }
        Ok(call_expr)
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>, ParserError> {
        self.expect(TokenType::Symbol(Symbol::LeftParen), "Expected left paren.")?;
        let args = match self.at().token == TokenType::Symbol(Symbol::RightParen) {
            true => vec![],
            false => self.parse_arguments_list()?,
        };
        self.expect(
            TokenType::Symbol(Symbol::RightParen),
            "Missing right paren inside arguments list.",
        )?;
        Ok(args)
    }

    fn parse_arguments_list(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut args = vec![self.parse_expr()?];
        while self.at().token == TokenType::Symbol(Symbol::Comma) && self.not_eof() {
            self.eat();
            args.push(self.parse_assignment_expr()?);
        }
        Ok(args)
    }

    fn parse_member_expr(&mut self) -> Result<Expr, ParserError> {
        let mut object = self.parse_primary_expr()?;
        while self.at().token == TokenType::Symbol(Symbol::Dot)
            || self.at().token == TokenType::Symbol(Symbol::LeftBracket)
        {
            let operator = self.eat();
            let property: Expr;

            // obj.expr
            if operator.token == TokenType::Symbol(Symbol::Dot) {
                // Get identifier
                property = self.parse_primary_expr()?;
                match property {
                    Expr::Identifier(_) => {}
                    _ => {
                        return Err(ParserError::InvalidDotProperty(
                            operator.line,
                            operator.col,
                            property.clone(),
                        ));
                    }
                }
            } else {
                // This allows obj[computed value]
                property = self.parse_expr()?;
                self.expect(
                    TokenType::Symbol(Symbol::RightBracket),
                    "Missing right bracket in computed value.",
                )?;
            }
            object = Expr::Member(Box::new(object), Box::new(property));
        }
        Ok(object)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, ParserError> {
        let tk = self.eat();
        match tk.token {
            // Keyword
            TokenType::Keyword(Keyword::If) => self.parse_if_expr() as Result<Expr, ParserError>,
            TokenType::Keyword(Keyword::While) => {
                self.parse_while_expr() as Result<Expr, ParserError>
            }
            TokenType::Keyword(Keyword::Loop) => {
                self.parse_loop_expr() as Result<Expr, ParserError>
            }
            TokenType::Keyword(Keyword::Block) => {
                self.parse_block_expr() as Result<Expr, ParserError>
            }
            // Identifier
            TokenType::Identifier(name) => {
                Ok(Expr::Identifier(name.to_string())) as Result<Expr, ParserError>
            }
            // Literals
            TokenType::StringLiteral(string) => {
                Ok(Expr::Literal(Literal::String(string))) as Result<Expr, ParserError>
            }
            TokenType::IntegerLiteral(integer) => {
                Ok(Expr::Literal(Literal::Integer(integer))) as Result<Expr, ParserError>
            }
            TokenType::FloatLiteral(float) => {
                Ok(Expr::Literal(Literal::Float(float))) as Result<Expr, ParserError>
            }
            // Unary Operators
            TokenType::Operator(Operator::LogicalNot) => {
                let expr = self.parse_call_member_expr()?;
                Ok(Expr::UnaryOp(UnaryOp::LogicalNot, Box::new(expr)))
            }
            TokenType::Operator(Operator::Subtract) => {
                let expr = self.parse_call_member_expr()?;
                Ok(Expr::UnaryOp(UnaryOp::ArithmeticNegative, Box::new(expr)))
            }
            TokenType::Operator(Operator::Add) => {
                let expr = self.parse_call_member_expr()?;
                Ok(Expr::UnaryOp(UnaryOp::ArithmeticPositive, Box::new(expr)))
            }
            TokenType::Operator(Operator::BitwiseNot) => {
                let expr = self.parse_call_member_expr()?;
                Ok(Expr::UnaryOp(UnaryOp::BitwiseNot, Box::new(expr)))
            }
            // Symbols
            TokenType::Symbol(Symbol::LeftParen) => {
                let value = self.parse_expr()?;
                self.expect(
                    TokenType::Symbol(Symbol::RightParen),
                    "Unexpected token found inside parenthesised expression. Expected closing parenthesis."
                )?; // rightParen
                Ok(value)
            }
            TokenType::Symbol(Symbol::LeftBracket) => {
                let mut elements: Vec<Expr> = Vec::new();
                if self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBracket) {
                    // Parse elements
                    elements.push(self.parse_expr()?);
                    while self.at().token == TokenType::Symbol(Symbol::Comma) {
                        self.eat(); // Consume comma
                        elements.push(self.parse_expr()?);
                    }
                }
                self.expect(
                    TokenType::Symbol(Symbol::RightBracket),
                    "Expected right bracket after array literal.",
                )?;
                Ok(Expr::Array(elements))
            }
            TokenType::Symbol(Symbol::Semicolon) => Ok(Expr::SpecialNull),
            _ => Err(ParserError::UnexpectedToken {
                expected: TokenType::EOF,
                found: tk.token,
                line: tk.line,
                col: tk.col,
                message: "Unexpected token found during parsing!".to_string(),
            }),
        }
    }

    fn parse_while_expr(&mut self) -> Result<Expr, ParserError> {
        let condition = self.parse_expr()?;
        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected left brace before `while` expression.",
        )?;

        let prev_inside_loop = self.inside_loop; // Save previous context.
        self.inside_loop = true; // We are now in a loop context. Modify parser.
        let mut statements: Vec<Stmt> = Vec::new();
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            statements.push(self.parse_stmt()?);
        }
        self.inside_loop = prev_inside_loop; // Restore previous context
        self.expect(
            TokenType::Symbol(Symbol::RightBrace),
            "Expected right brace after `while` expression.",
        )?;
        Ok(Expr::WhileExpr {
            condition: Box::new(condition),
            then: statements,
        })
    }

    fn parse_block_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected left brace before `block` expression.",
        )?;
        let mut statements: Vec<Stmt> = Vec::new();
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            statements.push(self.parse_stmt()?);
        }
        self.expect(
            TokenType::Symbol(Symbol::RightBrace),
            "Expected right brace after `block` expression.",
        )?;
        Ok(Expr::BlockExpr(statements))
    }

    fn parse_loop_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected left brace before `loop` expression.",
        )?;
        let prev_inside_loop = self.inside_loop; // Save previous context.
        self.inside_loop = true; // We are now in a loop context. Modify parser.
        let mut statements: Vec<Stmt> = Vec::new();
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            statements.push(self.parse_stmt()?);
        }
        self.inside_loop = prev_inside_loop; // Restore previous context
        self.expect(
            TokenType::Symbol(Symbol::RightBrace),
            "Expected right brace after `loop` expression.",
        )?;
        Ok(Expr::ForeverLoopExpr(statements))
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParserError> {
        let condition = self.parse_expr()?;
        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected left brace before `if` expression.",
        )?;
        let mut consequent: Vec<Stmt> = Vec::new();
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            consequent.push(self.parse_stmt()?);
        }
        self.expect(
            TokenType::Symbol(Symbol::RightBrace),
            "Expected right brace after `if` expression.",
        )?;
        let mut alternative: Option<Vec<Stmt>> = None;
        if self.at().token == TokenType::Keyword(Keyword::Else) {
            self.eat(); // Advance from else
            if self.at().token == TokenType::Keyword(Keyword::If) {
                self.eat();
                alternative = Some(vec![Stmt::ExprStmt(self.parse_if_expr()?)])
            } else {
                self.expect(
                    TokenType::Symbol(Symbol::LeftBrace),
                    "Expected left brace before `else` expression.",
                )?;
                let mut else_block: Vec<Stmt> = Vec::new();
                while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
                    else_block.push(self.parse_stmt()?);
                }
                self.expect(
                    TokenType::Symbol(Symbol::RightBrace),
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

    pub fn produce_ast(&mut self, source_code: String) -> Result<Program, Error> {
        self.tokens = tokenize(&source_code).map_err(Error::LexerError)?;
        let mut program = Program::new(Vec::new());

        while self.not_eof() {
            program
                .statements
                .push(self.parse_stmt().map_err(Error::ParserError)?);
        }

        Ok(program)
    }
}
