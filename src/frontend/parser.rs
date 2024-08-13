// Parser

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    errors::{Error, ParserError},
    frontend::ast::{BinaryOp, Program, Property},
};

use super::{
    ast::{Expr, Literal, ParsetimeType, Stmt, Type, UnaryOp},
    lexer::{tokenize, Keyword, Operator, Symbol, Token, TokenType},
    type_environment::TypeEnvironment,
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

    fn parse_stmt(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Stmt, ParserError> {
        match self.at().token {
            TokenType::Keyword(Keyword::Let) => {
                self.parse_var_declaration(false, type_env, root_type_env)
            }
            TokenType::Keyword(Keyword::Const) => {
                self.parse_var_declaration(true, type_env, root_type_env)
            }
            TokenType::Keyword(Keyword::Fn) => {
                self.parse_fn_declaration(false, type_env, root_type_env)
            }
            TokenType::Keyword(Keyword::Async) => {
                self.eat();
                match self.at().token {
                    TokenType::Keyword(Keyword::Fn) => {
                        self.parse_fn_declaration(true, type_env, root_type_env)
                    }
                    _ => Err(ParserError::MissingAsyncFn),
                }
            }
            TokenType::Keyword(Keyword::Break) => {
                self.parse_break_declaration(type_env, root_type_env)
            }
            TokenType::Keyword(Keyword::Return) => {
                self.parse_return_declaration(type_env, root_type_env)
            }
            TokenType::Keyword(Keyword::Struct) => {
                self.parse_struct_definition(type_env, root_type_env)
            }
            TokenType::Keyword(Keyword::Enum) => {
                self.parse_enum_definition(type_env, root_type_env)
            }
            TokenType::Keyword(Keyword::Type) => {
                self.parse_type_definition(type_env, root_type_env)
            }
            _ => Ok(Stmt::ExprStmt(self.parse_expr(type_env, root_type_env)?)),
        }
    }

    fn parse_struct_definition(
        &mut self,
        _type_env: &Rc<RefCell<TypeEnvironment>>,
        _root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Stmt, ParserError> {
        self.eat();
        let ident = self.eat();
        let name = match ident.token {
            TokenType::Identifier(ident) => ident,
            _ => return Err(ParserError::MissingIdentifier(ident.line, ident.col)),
        };
        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected struct body following definition.",
        )?;

        let mut struct_definition: HashMap<String, Type> = HashMap::new();

        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            // ident
            let key_token = self.eat();
            let key = match key_token.token {
                TokenType::Identifier(ident) => ident,
                _ => return Err(ParserError::MissingIdentifier(ident.line, ident.col)),
            };

            // :
            self.expect(
                TokenType::Symbol(Symbol::Colon),
                "Keys in structs must be followed with a colon ':', then its type.",
            )?;

            // type
            let key_type = self.parse_type_declaration()?;

            // Add to definition
            struct_definition.insert(key, key_type);

            // ',', ',)', or ')'
            match self.at().token {
                TokenType::Symbol(Symbol::Comma) => {
                    if self.at().token == TokenType::Symbol(Symbol::RightBrace) {
                        break;
                    } else {
                        continue;
                    }
                }
                _ => break,
            }
        }
        self.expect(
            TokenType::Symbol(Symbol::RightBrace),
            "Struct body must end with a right brace '}'",
        )?;

        Ok(Stmt::StructDefStmt {
            ident: name,
            key_type_values: struct_definition,
        })
    }

    fn parse_enum_definition(
        &mut self,
        _type_env: &Rc<RefCell<TypeEnvironment>>,
        _root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Stmt, ParserError> {
        self.eat(); // consume 'enum'
        let ident = self.eat();
        let name = match ident.token {
            TokenType::Identifier(ident) => ident,
            _ => return Err(ParserError::MissingIdentifier(ident.line, ident.col)),
        };
        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected enum body following definition.",
        )?;

        let mut variants: HashMap<String, Vec<Type>> = HashMap::new();

        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            let key_token = self.eat();
            let key = match key_token.token {
                TokenType::Identifier(ident) => ident,
                _ => {
                    return Err(ParserError::MissingIdentifier(
                        key_token.line,
                        key_token.col,
                    ))
                }
            };

            let mut variant_types = Vec::new();
            if self.at().token == TokenType::Symbol(Symbol::LeftParen) {
                self.eat(); // consume '('
                while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightParen) {
                    let variant_type = self.parse_type_declaration()?;
                    variant_types.push(variant_type);

                    if self.at().token == TokenType::Symbol(Symbol::Comma) {
                        self.eat(); // consume ','
                    }
                }
                self.expect(
                    TokenType::Symbol(Symbol::RightParen),
                    "Expected right parenthesis ')' after enum variant types.",
                )?;
            }

            variants.insert(key, variant_types);

            if self.at().token == TokenType::Symbol(Symbol::Comma) {
                self.eat(); // consume ','
            }
        }

        self.expect(
            TokenType::Symbol(Symbol::RightBrace),
            "Enum body must end with a right brace '}'",
        )?;

        Ok(Stmt::EnumDefStmt {
            ident: name,
            variants,
        })
    }

    fn parse_type_definition(
        &mut self,
        _type_env: &Rc<RefCell<TypeEnvironment>>,
        _root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Stmt, ParserError> {
        self.eat(); // consume 'type'
        let ident = self.eat();
        let name = match ident.token {
            TokenType::Identifier(ident) => ident,
            _ => return Err(ParserError::MissingIdentifier(ident.line, ident.col)),
        };
        self.expect(
            TokenType::Operator(Operator::Assign),
            "Expected type alias following definition.",
        )?;
        let alias = Box::new(self.parse_type_declaration()?);
        self.expect(
            TokenType::Symbol(Symbol::Semicolon),
            "type declaration is a statement. It must end with a semicolon.",
        )?;
        Ok(Stmt::AliasDefStmt { ident: name, alias })
    }

    fn parse_break_declaration(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Stmt, ParserError> {
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
        let expr = self.parse_expr(type_env, root_type_env)?;
        self.expect(
            TokenType::Symbol(Symbol::Semicolon),
            "break declaration is a statement. It must end with a semicolon.",
        )?;
        Ok(Stmt::BreakStmt(Some(expr)))
    }

    fn parse_return_declaration(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Stmt, ParserError> {
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
        let expr = self.parse_expr(type_env, root_type_env)?;
        self.expect(
            TokenType::Symbol(Symbol::Semicolon),
            "return declaration is a statement. It must end with a semicolon.",
        )?;
        Ok(Stmt::ReturnStmt(Some(expr)))
    }

    fn parse_fn_declaration(
        &mut self,
        is_async: bool,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Stmt, ParserError> {
        let declaration = self.eat();
        let ident = self.eat();
        let name = match ident.token {
            TokenType::Identifier(name) => name,
            _ => {
                return Err(ParserError::MissingFunctionIdentifier(
                    ident.line, ident.col,
                ))
            }
        };
        let args: Vec<(Expr, Type, bool)> = self.parse_args_with_types(type_env, root_type_env)?;
        let mut params: Vec<(String, Type, bool)> = Vec::new();
        for arg in args {
            match arg {
                (Expr::Identifier(name), param_type, is_mut) => {
                    params.push((name, param_type, is_mut))
                }
                _ => return Err(ParserError::InvalidFunctionParameter(ident.line, ident.col)),
            }
        }
        let mut return_type = Type::Null;
        if self.at().token == TokenType::Symbol(Symbol::Arrow) {
            // Defined return type
            self.eat();
            return_type = self.parse_type_declaration()?;
        }

        // Declare the function into the declared scope
        let fn_type = Type::Function(
            params
                .iter()
                .map(|(_, t, is_mut)| (t.clone(), *is_mut))
                .collect(),
            Box::new(return_type.clone()),
        );
        type_env
            .borrow_mut()
            .declare_fn(&name, fn_type.clone(), &declaration)?;

        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected function body following declaration.",
        )?;
        let prev_inside_function = self.inside_function;
        self.inside_function = true;
        let mut body: Vec<Stmt> = Vec::new();
        let fn_type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
            root_type_env.clone(),
        )));
        fn_type_env
            .borrow_mut()
            .declare_fn(&name, fn_type.clone(), &declaration)?;
        for (param_name, param_type, is_mut) in params.iter() {
            fn_type_env.borrow_mut().declare_var(
                param_name.to_string(),
                param_type.clone(),
                *is_mut,
                &declaration,
            )?;
        }
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            let stmt = self.parse_stmt(&fn_type_env, root_type_env)?;
            match stmt {
                Stmt::ReturnStmt(_) | Stmt::BreakStmt(_) => {
                    body.push(stmt);
                    while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace)
                    {
                        // Reached break or return stmt in its absolute scope (not outside, not inside scope).
                        // Ignore other stmts after the break/return.
                        self.eat();
                    }
                    break;
                }
                _ => body.push(stmt),
            }
        }
        let actual_return_type =
            body.last()
                .unwrap()
                .get_type(&fn_type_env, self.at().line, self.at().col)?;
        if actual_return_type != return_type {
            return Err(ParserError::TypeError {
                expected: return_type,
                found: actual_return_type,
                line: self.at().line,
                col: self.at().col,
                message: "Mismatched return type for function".to_string(),
            });
        }
        self.inside_function = prev_inside_function;
        self.expect(
            TokenType::Symbol(Symbol::RightBrace),
            "Closing brace expected inside function declaration.",
        )?;
        Ok(Stmt::FunctionDeclaration {
            parameters: params,
            name,
            return_type,
            body,
            is_async,
        })
    }

    // `let (global) (mut) ident(: type) = expr`
    fn parse_var_declaration(
        &mut self,
        is_const: bool,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Stmt, ParserError> {
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

        let mut expected_type: Option<Type> = None;

        if self.at().token == TokenType::Symbol(Symbol::Colon) {
            self.eat();
            expected_type = Some(self.parse_type_declaration()?);
        }

        if self.at().token == TokenType::Symbol(Symbol::Semicolon) {
            self.eat();
            if is_const {
                return Err(ParserError::ConstWithoutValue(
                    declaration.line,
                    declaration.col,
                ));
            }
            if expected_type.is_none() {
                return Err(ParserError::MissingTypeForVarDeclaration(
                    declaration.line,
                    declaration.col,
                ));
            }
            type_env.borrow_mut().declare_var(
                identifier.to_string(),
                expected_type.clone().unwrap(),
                is_mutable,
                &declaration,
            )?;
            return Ok(Stmt::DeclareStmt {
                name: identifier.to_string(),
                is_mutable,
                is_global,
                var_type: expected_type.unwrap(),
                expr: None,
            });
        }

        self.expect(
            TokenType::Operator(Operator::Assign),
            "Expected assign token `=` following identifier in var declaration.",
        )?;
        let expr = self.parse_expr(type_env, root_type_env)?;
        if expected_type.is_none() {
            let expr_type = expr.get_type(type_env, declaration.line, declaration.col)?;
            type_env.borrow_mut().declare_var(
                identifier.to_string(),
                expr_type.clone(),
                is_mutable,
                &declaration,
            )?;
            self.expect(
                TokenType::Symbol(Symbol::Semicolon),
                "Variable declaration is a statement. It must end with a semicolon.",
            )?;
            return Ok(Stmt::DeclareStmt {
                name: identifier.to_string(),
                is_mutable,
                is_global,
                var_type: expr_type,
                expr: Some(expr),
            });
        }
        if expr.get_type(type_env, declaration.line, declaration.col)?
            != expected_type.clone().unwrap()
        {
            return Err(ParserError::TypeError {
                expected: expected_type.unwrap(),
                found: expr.get_type(type_env, declaration.line, declaration.col)?,
                line: declaration.line,
                col: declaration.col,
                message: "Mismatched types between static declaration and value type".to_string(),
            });
        }
        type_env.borrow_mut().declare_var(
            identifier.to_string(),
            expected_type.clone().unwrap(),
            is_mutable,
            &declaration,
        )?;
        let declaration = Stmt::DeclareStmt {
            name: identifier.to_string(),
            is_mutable,
            is_global,
            var_type: expected_type.unwrap(),
            expr: Some(expr),
        };
        self.expect(
            TokenType::Symbol(Symbol::Semicolon),
            "Variable declaration is a statement. It must end with a semicolon.",
        )?;
        Ok(declaration)
    }

    fn parse_type_declaration(&mut self) -> Result<Type, ParserError> {
        let current = self.eat();
        match current.token {
            TokenType::Identifier(ident) => match ident.as_str() {
                "string" => Ok(Type::String),
                "integer" => Ok(Type::Integer),
                "float" => Ok(Type::Float),
                "object" => Ok(Type::Object),
                "null" => Ok(Type::Null),
                "bool" => Ok(Type::Bool),
                "array" => {
                    self.expect(
                        TokenType::Operator(Operator::LessThan),
                        "Expected Left angled bracket (LessThan) '<' for type declaration, array takes another type.",
                    )?;
                    let inner = self.parse_type_declaration()?;
                    self.expect(
                        TokenType::Operator(Operator::GreaterThan),
                        "Expected Right angled bracket (GreaterThan) '>' to close type declaration, array takes another type.",
                    )?;
                    Ok(Type::Array(Box::new(inner)))
                }
                _ => Err(ParserError::InvalidTypeDeclaration(
                    current.line,
                    current.col,
                )),
            },
            TokenType::Symbol(Symbol::LeftParen) => {
                let mut inner_types: Vec<Type> = Vec::new();
                while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightParen) {
                    inner_types.push(self.parse_type_declaration()?);
                    let next = self.at();
                    match next.token {
                        TokenType::Symbol(Symbol::Comma) => {
                            if self.at().token == TokenType::Symbol(Symbol::RightParen) {
                                break;
                            }
                            self.eat();
                        }
                        TokenType::Symbol(Symbol::RightParen) => break,
                        _ => return Err(ParserError::InvalidTypeDeclaration(next.line, next.col)),
                    }
                }

                self.expect(
                    TokenType::Symbol(Symbol::RightParen),
                    "Expected Right parenthesis ')' to close tuple type expression.",
                )?;

                Ok(Type::Tuple(inner_types))
            }
            _ => Err(ParserError::InvalidTypeDeclaration(
                current.line,
                current.col,
            )),
        }
    }

    fn parse_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        self.parse_assignment_expr(type_env, root_type_env)
    }

    fn parse_assignment_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let left = self.parse_object_expr(type_env, root_type_env)?;
        if self.at().token == TokenType::Operator(Operator::Assign) {
            let assign_token = self.eat(); // Advance after.
            let value = self.parse_assignment_expr(type_env, root_type_env)?;
            let left_type = left.get_type(type_env, assign_token.line, assign_token.col)?;
            let value_type = value.get_type(type_env, assign_token.line, assign_token.col)?;
            if left_type != value_type {
                return Err(ParserError::TypeError {
                    expected: left_type,
                    found: value_type,
                    line: assign_token.line,
                    col: assign_token.col,
                    message: "Expression assigning to the assignee must be the same.".to_string(),
                });
            }
            return Ok(Expr::AssignmentExpr {
                assignee: Box::new(left),
                expr: Box::new(value),
            });
        }
        Ok(left)
    }

    fn parse_object_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        if self.at().token != TokenType::Symbol(Symbol::LeftBrace) {
            return self.parse_relational_expr(type_env, root_type_env);
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
            let value = self.parse_expr(type_env, root_type_env)?;
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

    fn parse_relational_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let mut left = self.parse_concatenation_expr(type_env, root_type_env)?;
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
            let right = self.parse_concatenation_expr(type_env, root_type_env)?;
            left.clone()
                .verify_type(type_env, operator_astoken.line, operator_astoken.col)?;
            left = Expr::BinaryOp {
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_concatenation_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let mut left = self.parse_additive_expr(type_env, root_type_env)?;

        while matches!(self.at().token, TokenType::Operator(Operator::Concat)) {
            let op = self.eat();
            let right = self.parse_additive_expr(type_env, root_type_env)?;
            left = Expr::ConcatOp {
                left: Box::new(left),
                right: Box::new(right),
            };
            left.clone().verify_type(type_env, op.line, op.col)?;
        }
        Ok(left)
    }

    fn parse_range_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
        start_expr: Expr,
    ) -> Result<Expr, ParserError> {
        let inclusive = match self.eat().token {
            TokenType::Operator(Operator::RangeInclusive) => true,
            TokenType::Operator(Operator::RangeExclusive) => false,
            _ => {
                return Err(ParserError::InvalidOperator(
                    self.at().line,
                    self.at().col,
                    self.at().token.clone(),
                ))
            }
        };

        let end_expr = self.parse_expr(type_env, root_type_env)?;

        match start_expr.get_type(type_env, self.at().line, self.at().col)? {
            Type::Integer => {}
            e => {
                return Err(ParserError::TypeError {
                    expected: Type::Integer,
                    found: e,
                    line: self.at().line,
                    col: self.at().col,
                    message: "First argument for Range Expr should be integer.".to_string(),
                })
            }
        }

        match end_expr.get_type(type_env, self.at().line, self.at().col)? {
            Type::Integer => {}
            e => {
                return Err(ParserError::TypeError {
                    expected: Type::Integer,
                    found: e,
                    line: self.at().line,
                    col: self.at().col,
                    message: "Second argument for Range Expr should be integer.".to_string(),
                })
            }
        }

        Ok(Expr::Range {
            start: Box::new(start_expr),
            end: Box::new(end_expr),
            inclusive,
            defined_type: Type::Integer,
        })
    }

    fn parse_additive_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let mut left = self.parse_multiplicative_expr(type_env, root_type_env)?;

        if matches!(
            self.at().token,
            TokenType::Operator(Operator::RangeInclusive)
                | TokenType::Operator(Operator::RangeExclusive)
        ) {
            return self.parse_range_expr(type_env, root_type_env, left);
        }

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
            let right = self.parse_multiplicative_expr(type_env, root_type_env)?;
            left.clone()
                .verify_type(type_env, operator_astoken.line, operator_astoken.col)?;
            left = Expr::BinaryOp {
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_multiplicative_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let mut left = self.parse_call_member_expr(type_env, root_type_env)?;

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
            let right = self.parse_call_member_expr(type_env, root_type_env)?;
            left.clone()
                .verify_type(type_env, operator_astoken.line, operator_astoken.col)?;
            left = Expr::BinaryOp {
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_call_member_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let member = self.parse_member_expr(type_env, root_type_env)?;
        if self.at().token == TokenType::Symbol(Symbol::LeftParen) {
            return self.parse_call_expr(member, type_env, root_type_env);
        }
        Ok(member)
    }

    fn parse_call_expr(
        &mut self,
        caller: Expr,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let mut call_expr: Expr = Expr::FunctionCall(
            self.parse_args(type_env, root_type_env)?,
            Box::new(caller.clone()),
        );
        if self.at().token == TokenType::Symbol(Symbol::LeftParen) {
            call_expr = self.parse_call_expr(call_expr, type_env, root_type_env)?;
        }

        // Ensure correct parameters
        if let Expr::Identifier(fn_name) = &caller {
            if let Some(Type::Function(param_types, _)) = type_env.borrow().lookup_fn(fn_name) {
                let args = match &call_expr {
                    Expr::FunctionCall(args, _) => args,
                    _ => unreachable!(),
                };
                if args.len() != param_types.len() {
                    return Err(ParserError::TypeError {
                        expected: Type::Function(param_types.clone(), Box::new(Type::Null)),
                        found: Type::Function(
                            args.iter()
                                .map(|(arg, is_mut)| {
                                    (
                                        arg.get_type(type_env, self.at().line, self.at().col)
                                            .unwrap(),
                                        *is_mut,
                                    )
                                })
                                .collect(),
                            Box::new(Type::Null),
                        ),
                        line: self.at().line,
                        col: self.at().col,
                        message: "Incorrect number of arguments.".to_string(),
                    });
                }
                for (arg, expected_type) in args.iter().zip(param_types.iter()) {
                    if expected_type.0 == Type::Any {
                        continue;
                    }
                    let arg_type = arg.0.get_type(type_env, self.at().line, self.at().col)?;
                    if arg_type != expected_type.0 {
                        return Err(ParserError::TypeError {
                            expected: expected_type.0.clone(),
                            found: arg_type,
                            line: self.at().line,
                            col: self.at().col,
                            message: "Argument type mismatch.".to_string(),
                        });
                    }
                    if arg.1 != expected_type.1 {
                        return Err(ParserError::MutabilityMismatch {
                            expected: expected_type.1,
                            found: arg.1,
                            line: self.at().line,
                            col: self.at().col,
                        });
                    }
                }
            }
        }

        Ok(call_expr)
    }

    fn parse_args(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Vec<(Expr, bool)>, ParserError> {
        self.expect(TokenType::Symbol(Symbol::LeftParen), "Expected left paren.")?;
        let args = match self.at().token == TokenType::Symbol(Symbol::RightParen) {
            true => vec![],
            false => self.parse_arguments_list(type_env, root_type_env)?,
        };
        self.expect(
            TokenType::Symbol(Symbol::RightParen),
            "Missing right paren inside arguments list.",
        )?;
        Ok(args)
    }

    fn parse_args_with_types(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Vec<(Expr, Type, bool)>, ParserError> {
        self.expect(TokenType::Symbol(Symbol::LeftParen), "Expected left paren.")?;
        let args = match self.at().token == TokenType::Symbol(Symbol::RightParen) {
            true => vec![],
            false => self.parse_arguments_list_with_types(type_env, root_type_env)?,
        };
        self.expect(
            TokenType::Symbol(Symbol::RightParen),
            "Missing right paren inside arguments list.",
        )?;
        Ok(args)
    }

    fn parse_arguments_list(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Vec<(Expr, bool)>, ParserError> {
        let mut first_mut = false;
        if self.at().token == TokenType::Keyword(Keyword::Mut) {
            self.eat();
            first_mut = true;
        }
        let mut args: Vec<(Expr, bool)> =
            vec![(self.parse_expr(type_env, root_type_env)?, first_mut)];
        while self.at().token == TokenType::Symbol(Symbol::Comma) && self.not_eof() {
            self.eat();
            let mut is_mut = false;
            if self.at().token == TokenType::Keyword(Keyword::Mut) {
                self.eat();
                is_mut = true;
            }
            args.push((self.parse_expr(type_env, root_type_env)?, is_mut));
        }
        Ok(args)
    }

    fn parse_arguments_list_with_types(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Vec<(Expr, Type, bool)>, ParserError> {
        let mut first_mut = false;
        if self.at().token == TokenType::Keyword(Keyword::Mut) {
            self.eat();
            first_mut = true;
        }
        let first = match self.parse_expr(type_env, root_type_env) {
            Ok(result) => result,
            Err(e) => {
                let return_val = if let ParserError::UndefinedVariable(_, _, var_name) = e {
                    Ok(Expr::Identifier(var_name))
                } else {
                    Err(e)
                };
                return_val?
            }
        };
        self.expect(
            TokenType::Symbol(Symbol::Colon),
            "Expected colon to define type for args",
        )?;
        let first_type = self.parse_type_declaration()?;
        let mut args: Vec<(Expr, Type, bool)> = vec![(first, first_type, first_mut)];
        while self.at().token == TokenType::Symbol(Symbol::Comma) && self.not_eof() {
            self.eat();
            let mut ident_mut = false;
            if self.at().token == TokenType::Keyword(Keyword::Mut) {
                self.eat();
                ident_mut = true;
            }
            let ident = match self.parse_expr(type_env, root_type_env) {
                Ok(result) => result,
                Err(e) => {
                    let return_val = if let ParserError::UndefinedVariable(_, _, var_name) = e {
                        Ok(Expr::Identifier(var_name))
                    } else {
                        Err(e)
                    };
                    return_val?
                }
            };
            self.expect(
                TokenType::Symbol(Symbol::Colon),
                "Expected colon to define type for args",
            )?;
            let ident_type = self.parse_type_declaration()?;
            args.push((ident, ident_type, ident_mut));
        }
        Ok(args)
    }

    fn parse_member_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let mut object = self.parse_primary_expr(type_env, root_type_env)?;
        while self.at().token == TokenType::Symbol(Symbol::Dot)
            || self.at().token == TokenType::Symbol(Symbol::LeftBracket)
        {
            let operator = self.eat();
            let property: Expr;

            // obj.expr
            if operator.token == TokenType::Symbol(Symbol::Dot) {
                // Get identifier
                property = self.parse_primary_expr(type_env, root_type_env)?;
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
                property = self.parse_expr(type_env, root_type_env)?;
                self.expect(
                    TokenType::Symbol(Symbol::RightBracket),
                    "Missing right bracket in computed value.",
                )?;
            }
            object = Expr::Member(Box::new(object), Box::new(property));
        }
        Ok(object)
    }

    fn parse_primary_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let tk = self.eat();
        match tk.token {
            // Keyword
            TokenType::Keyword(Keyword::If) => {
                self.parse_if_expr(tk.line, tk.col, type_env, root_type_env)
                    as Result<Expr, ParserError>
            }
            TokenType::Keyword(Keyword::While) => {
                self.parse_while_expr(tk.line, tk.col, type_env, root_type_env)
                    as Result<Expr, ParserError>
            }
            TokenType::Keyword(Keyword::For) => {
                self.parse_for_expr(tk.line, tk.col, type_env, root_type_env)
                    as Result<Expr, ParserError>
            }
            TokenType::Keyword(Keyword::Loop) => {
                self.parse_loop_expr(type_env, root_type_env) as Result<Expr, ParserError>
            }
            TokenType::Keyword(Keyword::Block) => {
                self.parse_block_expr(type_env, root_type_env) as Result<Expr, ParserError>
            }
            // Identifier
            TokenType::Identifier(name) => Expr::Identifier(name.to_string())
                .verify_type(type_env, tk.line, tk.col)
                as Result<Expr, ParserError>,
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
                let expr = self.parse_call_member_expr(type_env, root_type_env)?;
                Expr::UnaryOp(UnaryOp::LogicalNot, Box::new(expr))
                    .verify_type(type_env, tk.line, tk.col)
            }
            TokenType::Operator(Operator::Subtract) => {
                let expr = self.parse_call_member_expr(type_env, root_type_env)?;
                Expr::UnaryOp(UnaryOp::ArithmeticNegative, Box::new(expr))
                    .verify_type(type_env, tk.line, tk.col)
            }
            TokenType::Operator(Operator::Add) => {
                let expr = self.parse_call_member_expr(type_env, root_type_env)?;
                Expr::UnaryOp(UnaryOp::ArithmeticPositive, Box::new(expr))
                    .verify_type(type_env, tk.line, tk.col)
            }
            TokenType::Operator(Operator::BitwiseNot) => {
                let expr = self.parse_call_member_expr(type_env, root_type_env)?;
                Expr::UnaryOp(UnaryOp::BitwiseNot, Box::new(expr))
                    .verify_type(type_env, tk.line, tk.col)
            }
            // Symbols
            TokenType::Symbol(Symbol::LeftParen) => {
                let value = self.parse_expr(type_env, root_type_env)?;
                let possibility = self.eat();
                match possibility.token {
                    TokenType::Symbol(Symbol::Comma) => {
                        // It's a tuple expr.
                        self.parse_tuple_expr(tk.line, tk.col, value, type_env, root_type_env)
                    }
                    TokenType::Symbol(Symbol::RightParen) => {
                        // It's a standard bracketed expr.
                        Ok(value)
                    }
                    _ => {
                        Err(ParserError::UnexpectedToken {
                            expected: TokenType::Symbol(Symbol::RightParen),
                            found: possibility.token,
                            line: possibility.line,
                            col: possibility.col,
                            message: "Expected closing parenthesis ')' (or ',' for tuples) for bracketed expr"
                                .to_string(),
                        })
                    }
                }
            }
            TokenType::Symbol(Symbol::LeftBracket) => {
                let mut elements: Vec<Expr> = Vec::new();
                let mut array_type: Type = Type::Null;
                if self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBracket) {
                    // Parse elements
                    let first = self.parse_expr(type_env, root_type_env)?;
                    array_type = first.get_type(type_env, tk.line, tk.col)?;
                    if array_type == Type::Mismatch {
                        return Err(ParserError::MultipleReturnTypes(tk.line, tk.col));
                    }
                    elements.push(first);
                    while self.at().token == TokenType::Symbol(Symbol::Comma) {
                        let comma_token = self.eat(); // Consume comma
                        let value = self.parse_expr(type_env, root_type_env)?;
                        let value_type = value.get_type(type_env, tk.line, tk.col)?;
                        if value_type != array_type {
                            return Err(ParserError::TypeError {
                                expected: array_type,
                                found: value_type,
                                line: comma_token.line,
                                col: comma_token.col,
                                message: "Mismatched types in array.".to_string(),
                            });
                        }
                        elements.push(value);
                    }
                }
                self.expect(
                    TokenType::Symbol(Symbol::RightBracket),
                    "Expected right bracket after array literal.",
                )?;
                Ok(Expr::Array(elements, array_type))
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

    fn parse_tuple_expr(
        &mut self,
        line: usize,
        col: usize,
        first_value: Expr,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let mut tuple: Vec<(Expr, Type)> = Vec::new();
        tuple.push((
            first_value.clone(),
            first_value.get_type(type_env, line, col)?,
        ));
        // (value1, value2)
        // ---------^------ here now
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightParen) {
            let value = self.parse_expr(type_env, root_type_env)?;
            tuple.push((value.clone(), value.get_type(type_env, line, col)?));
            let next = self.eat();
            match next.token {
                TokenType::Symbol(Symbol::Comma) => {
                    if self.eat().token == TokenType::Symbol(Symbol::RightParen) {
                        return Ok(Expr::Tuple(tuple));
                    }
                    continue;
                }
                TokenType::Symbol(Symbol::RightParen) => return Ok(Expr::Tuple(tuple)),
                e => {
                    return Err(ParserError::UnexpectedToken {
                        expected: TokenType::Symbol(Symbol::RightParen),
                        found: e,
                        line: next.line,
                        col: next.col,
                        message:
                            "Expected comma ',' or right parenthesis ')' to delimit or end tuple."
                                .to_string(),
                    })
                }
            };
        }
        self.expect(
            TokenType::Symbol(Symbol::RightParen),
            "Expected right parenthesis ')' to end tuple expression.",
        )?;
        Ok(Expr::Tuple(tuple))
    }

    fn parse_for_expr(
        &mut self,
        line: usize,
        col: usize,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let mut is_mut = false;
        if self.at().token == TokenType::Keyword(Keyword::Mut) {
            self.eat();
            is_mut = true;
        }
        let identifier_token = self.eat();
        let identifier = match identifier_token.token {
            TokenType::Identifier(ref ident) => ident,
            _ => {
                return Err(ParserError::MissingIdentifier(
                    identifier_token.line,
                    identifier_token.col,
                ))
            }
        };

        self.expect(TokenType::Keyword(Keyword::In), "Expected 'in' keyword.")?;

        let iterable = self.parse_expr(type_env, root_type_env)?;
        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected left brace before `for` expression.",
        )?;

        let prev_inside_loop = self.inside_loop; // Save previous context.
        self.inside_loop = true; // We are now in a loop context. Modify parser.
        let mut then: Vec<Stmt> = Vec::new();
        let for_type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
            type_env.clone(),
        )));
        for_type_env.borrow_mut().declare_var(
            identifier.to_string(),
            iterable.get_type(type_env, line, col)?,
            is_mut,
            &identifier_token,
        )?;
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            let stmt = self.parse_stmt(&for_type_env, root_type_env)?;
            match stmt {
                Stmt::ReturnStmt(_) | Stmt::BreakStmt(_) => {
                    then.push(stmt);
                    while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace)
                    {
                        // Reached break or return stmt in its absolute scope (not outside, not inside scope).
                        // Ignore other stmts after the break/return.
                        self.eat();
                    }
                    break;
                }
                _ => then.push(stmt),
            }
        }
        self.inside_loop = prev_inside_loop; // Restore previous context
        self.expect(
            TokenType::Symbol(Symbol::RightBrace),
            "Expected right brace after `for` expression.",
        )?;

        Ok(Expr::ForExpr {
            identifier: identifier.to_string(),
            iterable: Box::new(iterable),
            then,
        })
    }

    fn parse_while_expr(
        &mut self,
        line: usize,
        col: usize,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let condition = self.parse_expr(type_env, root_type_env)?;
        let condition_type = condition.get_type(type_env, line, col)?;
        if condition_type != Type::Bool {
            return Err(ParserError::TypeError {
                expected: Type::Bool,
                found: condition_type,
                line,
                col,
                message: "While expressions must have a boolean condition.".to_string(),
            });
        }
        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected left brace before `while` expression.",
        )?;

        let prev_inside_loop = self.inside_loop; // Save previous context.
        self.inside_loop = true; // We are now in a loop context. Modify parser.
        let mut statements: Vec<Stmt> = Vec::new();
        let while_type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
            type_env.clone(),
        )));
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            let stmt = self.parse_stmt(&while_type_env, root_type_env)?;
            match stmt {
                Stmt::ReturnStmt(_) | Stmt::BreakStmt(_) => {
                    statements.push(stmt);
                    while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace)
                    {
                        // Reached break or return stmt in its absolute scope (not outside, not inside scope).
                        // Ignore other stmts after the break/return.
                        self.eat();
                    }
                    break;
                }
                _ => statements.push(stmt),
            }
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

    fn parse_block_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected left brace before `block` expression.",
        )?;
        let mut statements: Vec<Stmt> = Vec::new();
        let block_type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
            type_env.clone(),
        )));
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            let stmt = self.parse_stmt(&block_type_env, root_type_env)?;
            match stmt {
                Stmt::ReturnStmt(_) | Stmt::BreakStmt(_) => {
                    statements.push(stmt);
                    while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace)
                    {
                        // Reached break or return stmt in its absolute scope (not outside, not inside scope).
                        // Ignore other stmts after the break/return.
                        self.eat();
                    }
                    break;
                }
                _ => statements.push(stmt),
            }
        }
        self.expect(
            TokenType::Symbol(Symbol::RightBrace),
            "Expected right brace after `block` expression.",
        )?;
        Ok(Expr::BlockExpr(statements))
    }

    fn parse_loop_expr(
        &mut self,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected left brace before `loop` expression.",
        )?;
        let prev_inside_loop = self.inside_loop; // Save previous context.
        self.inside_loop = true; // We are now in a loop context. Modify parser.
        let mut statements: Vec<Stmt> = Vec::new();
        let loop_type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
            type_env.clone(),
        )));
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            let stmt = self.parse_stmt(&loop_type_env, root_type_env)?;
            match stmt {
                Stmt::ReturnStmt(_) | Stmt::BreakStmt(_) => {
                    statements.push(stmt);
                    while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace)
                    {
                        // Reached break or return stmt in its absolute scope (not outside, not inside scope).
                        // Ignore other stmts after the break/return.
                        self.eat();
                    }
                    break;
                }
                _ => statements.push(stmt),
            }
        }
        self.inside_loop = prev_inside_loop; // Restore previous context
        self.expect(
            TokenType::Symbol(Symbol::RightBrace),
            "Expected right brace after `loop` expression.",
        )?;
        Ok(Expr::ForeverLoopExpr(statements))
    }

    fn parse_if_expr(
        &mut self,
        line: usize,
        col: usize,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Expr, ParserError> {
        let condition = self.parse_expr(type_env, root_type_env)?;
        let condition_type = condition.get_type(type_env, line, col)?;
        if condition_type != Type::Bool {
            return Err(ParserError::TypeError {
                expected: Type::Bool,
                found: condition_type,
                line,
                col,
                message: "If expressions must have a boolean condition.".to_string(),
            });
        }
        self.expect(
            TokenType::Symbol(Symbol::LeftBrace),
            "Expected left brace before `if` expression.",
        )?;
        let mut consequent: Vec<Stmt> = Vec::new();
        let if_type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
            type_env.clone(),
        )));
        while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
            let stmt = self.parse_stmt(&if_type_env, root_type_env)?;
            match stmt {
                Stmt::ReturnStmt(_) | Stmt::BreakStmt(_) => {
                    consequent.push(stmt);
                    while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace)
                    {
                        // Reached break or return stmt in its absolute scope (not outside, not inside scope).
                        // Ignore other stmts after the break/return.
                        self.eat();
                    }
                    break;
                }
                _ => consequent.push(stmt),
            }
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
                alternative = Some(vec![Stmt::ExprStmt(self.parse_if_expr(
                    line,
                    col,
                    type_env,
                    root_type_env,
                )?)])
            } else {
                self.expect(
                    TokenType::Symbol(Symbol::LeftBrace),
                    "Expected left brace before `else` expression.",
                )?;
                let mut else_block: Vec<Stmt> = Vec::new();
                let else_type_env = Rc::new(RefCell::new(TypeEnvironment::new_with_parent(
                    type_env.clone(),
                )));
                while self.not_eof() && self.at().token != TokenType::Symbol(Symbol::RightBrace) {
                    let stmt = self.parse_stmt(&else_type_env, root_type_env)?;
                    match stmt {
                        Stmt::ReturnStmt(_) | Stmt::BreakStmt(_) => {
                            else_block.push(stmt);
                            while self.not_eof()
                                && self.at().token != TokenType::Symbol(Symbol::RightBrace)
                            {
                                // Reached break or return stmt in its absolute scope (not outside, not inside scope).
                                // Ignore other stmts after the break/return.
                                self.eat();
                            }
                            break;
                        }
                        _ => else_block.push(stmt),
                    }
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

    pub fn produce_ast(
        &mut self,
        source_code: String,
        type_env: &Rc<RefCell<TypeEnvironment>>,
        root_type_env: &Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Program, Error> {
        self.tokens = tokenize(&source_code).map_err(Error::LexerError)?;
        let mut program = Program::new(Vec::new());

        while self.not_eof() {
            program.statements.push(
                self.parse_stmt(type_env, root_type_env)
                    .map_err(Error::ParserError)?,
            );
        }

        Ok(program)
    }
}
