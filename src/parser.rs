use crate::{ast::*, types::Type};
use std::time::{Duration, Instant};

use super::lexer::{Token, TokenType};
use std::{fmt::Debug, vec};

#[derive(Debug)]
pub struct NewParser<'a> {
    tokens: &'a [Token],
    current: usize,
    errors: Vec<NewParserError>,

    scope_parenthesis: usize,
    id_gen: NodeIdGenerator,
}

#[derive(Debug, Clone)]
pub enum ErrorType {
    MissmatchedTypes(Span, Span), // .0 = expected  .1 = found

    ExpressionExpected,
    MissingSemicolon,
    AdjacentExpression,
    EndOfInput,
    UnclosedParenthesis,
    Expected,
    DeclarationExpected,
    Unexpected,
    IncompatibleTypes,
    UndefinedIdentifier,
    MissingTypeAnnotation,
    InvalidType,
}

#[derive(Debug, Clone)]
pub struct NewParserError {
    pub span: Span,
    pub message: String,
    pub ty: ErrorType,
}

impl<'a> NewParser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            current: 0,
            errors: vec![],
            scope_parenthesis: 0,
            id_gen: NodeIdGenerator::new(),
        }
    }

    fn span_from(&self, start: usize) -> Span {
        Span::new(start, self.current)
    }

    fn untyped_expr(&mut self, expr: ExprKind, span: Span) -> Expr {
        Expr {
            id: self.id_gen.next(),
            kind: expr,
            span,
        }
    }

    fn stmt(&mut self, stmt: StmtKind, span: Span) -> Stmt {
        Stmt {
            id: self.id_gen.next(),
            kind: stmt,
            span,
        }
    }

    fn synchronize(&mut self) {
        self.advance(); // Skip the problematic token

        while !self.is_at_end() {
            if self.tokens[self.current - 1].ty == TokenType::Semicolon {
                return;
            }

            // Look for tokens that might start a new declaration
            match self.peek().unwrap().ty {
                TokenType::Function
                | TokenType::Let
                | TokenType::For
                | TokenType::If
                | TokenType::Return => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn error(&self, ty: ErrorType, message: String) -> NewParserError {
        let token = self.peek().cloned();

        NewParserError {
            span: token.unwrap().span.clone(),
            message: message.to_string(),
            ty,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || self.tokens[self.current].ty == TokenType::EOF
    }

    pub fn peek(&self) -> Option<&Token> {
        if self.is_at_end() {
            None
        } else {
            Some(&self.tokens[self.current])
        }
    }

    pub fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            let token = &self.tokens[self.current];
            self.current += 1;
            Some(token)
        } else {
            None
        }
    }

    pub fn check(&self, ty: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        return self.tokens[self.current].ty == ty;
    }

    fn expect(&mut self, ty: TokenType) -> Result<&Token, NewParserError> {
        if !self.is_at_end() && !self.check(ty) {
            Err(self.error(
                ErrorType::Expected,
                format!(
                    "Expected {:?} but got {:?}",
                    ty, self.tokens[self.current].ty
                ),
            ))
        } else {
            // we can unrwap because advance only fails if is_at_end() succeeds, but we already
            // checked that
            Ok(self.advance().unwrap())
        }
    }

    fn is_expression_start(&self, token: &Token) -> bool {
        matches!(
            token.ty,
            TokenType::Number
                | TokenType::String
                | TokenType::Identifier
                | TokenType::Bool
                | TokenType::LeftParen
                | TokenType::Minus
                | TokenType::Bang
                | TokenType::At // For closures
                | TokenType::LeftBrace // For blocks
        )
    }

    fn is_valid_operator(&self) -> bool {
        self.check(TokenType::Plus)
            || self.check(TokenType::Minus)
            || self.check(TokenType::Star)
            || self.check(TokenType::Slash)
            || self.check(TokenType::Equal)
    }

    fn primary(&mut self) -> Result<Expr, NewParserError> {
        macro_rules! consume {
            ($expr:expr) => {{
                self.advance();
                Ok($expr)
            }};
        }

        if self.is_at_end() {
            return Err(NewParserError {
                span: Span::new(0, 0),
                ty: ErrorType::EndOfInput,
                message: "Unexpected end of input.".into(),
            });
        }

        let start = self.current;

        match self.peek() {
            Some(token) => {
                let token = token.clone();
                match token.ty {
                    TokenType::Identifier => {
                        consume!(self.untyped_expr(
                            ExprKind::Identifier(token.lexeme.clone()),
                            self.span_from(start)
                        ))
                    }
                    TokenType::Number => {
                        consume!(self.untyped_expr(
                            ExprKind::Literal(
                                Literal::Number(
                                    token.lexeme.parse::<f64>().expect(
                                        "Somehow got a non parsable number in a Number token"
                                    )
                                )
                            ),
                            self.span_from(start)
                        ))
                    }
                    TokenType::String => consume!(self.untyped_expr(
                        ExprKind::Literal(Literal::String(token.lexeme.clone())),
                        self.span_from(start)
                    )),
                    TokenType::Bool => {
                        consume!(self.untyped_expr(
                            ExprKind::Literal(Literal::Boolean(token.lexeme == "true")),
                            self.span_from(start)
                        ))
                    }
                    TokenType::LeftParen => {
                        self.advance(); // Consume the left parenthesis
                        let expr = self.expression()?;

                        match self.expect(TokenType::RightParen) {
                            Ok(_) => Ok(expr),
                            Err(_) => {
                                if let Some(token) = self.peek() {
                                    if self.is_expression_start(token) {
                                        Err(self.error(ErrorType::AdjacentExpression,format!(
                                            "Missing operator or comma before '{}'. Did you forget a ')' or ','?", 
                                            token.lexeme
                                        ).into()))
                                    } else {
                                        Err(self.error(
                                            ErrorType::UnclosedParenthesis,
                                            "Expected ')' to close the parenthesized expression."
                                                .into(),
                                        ))
                                    }
                                } else {
                                    Err(self.error(
                                        ErrorType::UnclosedParenthesis,
                                        "Unexpected end of input. Expected ')'.".into(),
                                    ))
                                }
                            }
                        }
                    }
                    _ => Err(self.error(
                        ErrorType::ExpressionExpected,
                        format!("Expected expression, got '{}'", token.lexeme).into(),
                    )),
                }
            }
            None => Err(self.error(
                ErrorType::ExpressionExpected,
                "Expected expression, got none.".into(),
            )),
        }
    }

    fn call(&mut self) -> Result<Expr, NewParserError> {
        let start = self.current;
        let mut expr = self.primary()?;

        if self.check(TokenType::LeftParen) {
            self.scope_parenthesis += 1;

            self.advance();

            let mut arguments = Vec::new();

            if !self.check(TokenType::RightParen) {
                loop {
                    if arguments.len() >= 255 {
                        return Err(self.error(
                            ErrorType::Unexpected,
                            "Cannot have more than 255 arguments.".into(),
                        ));
                    }

                    arguments.push(self.expression()?);

                    if !self.check(TokenType::Comma) {
                        break;
                    }

                    self.advance();
                }
            }

            self.scope_parenthesis -= 1;
            self.expect(TokenType::RightParen)?;

            expr = self.untyped_expr(
                ExprKind::Call(CallExpr {
                    callee: Box::new(expr),
                    arguments,
                }),
                self.span_from(start),
            )
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, NewParserError> {
        if self.check(TokenType::Bang) || self.check(TokenType::Minus) {
            let start = self.current;
            let operator = self.advance().unwrap().clone();
            let right = self.unary()?;

            let operator = match operator.ty {
                TokenType::Bang => UnaryOp::Not,
                TokenType::Minus => UnaryOp::Negate,
                _ => unreachable!(),
            };

            return Ok(self.untyped_expr(
                ExprKind::Unary(UnaryExpr {
                    operator,
                    operand: Box::new(right),
                }),
                self.span_from(start),
            ));
        }

        self.call()
    }

    fn factor(&mut self) -> Result<Expr, NewParserError> {
        let start = self.current;
        let mut expr = self.unary()?;

        while self.check(TokenType::Star) || self.check(TokenType::Slash) {
            let operator = self.advance().unwrap().clone();
            let right = self.unary()?;

            let op = match operator.ty {
                TokenType::Star => BinaryOp::Multiply,
                TokenType::Slash => BinaryOp::Divide,
                _ => unreachable!(),
            };

            expr = self.untyped_expr(
                ExprKind::Binary(BinaryExpr {
                    operator: op,
                    left: Box::new(expr),
                    right: Box::new(right),
                }),
                self.span_from(start),
            );
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, NewParserError> {
        let start = self.current;
        let mut expr = self.factor()?;

        while self.check(TokenType::Plus) || self.check(TokenType::Minus) {
            let operator = self.advance().unwrap().clone();
            let right = self.factor()?;

            let op = match operator.ty {
                TokenType::Plus => BinaryOp::Add,
                TokenType::Minus => BinaryOp::Subtract,
                _ => unreachable!(),
            };

            expr = self.untyped_expr(
                ExprKind::Binary(BinaryExpr {
                    operator: op,
                    left: Box::new(expr),
                    right: Box::new(right),
                }),
                self.span_from(start),
            );
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, NewParserError> {
        let start = self.current;
        let mut expr = self.term()?;

        while self.check(TokenType::Greater) || self.check(TokenType::Less) {
            let operator = self.advance().unwrap().clone();

            let is_equal_follows = self.check(TokenType::Equal);
            if is_equal_follows {
                self.advance();
            }

            let right = self.term()?;

            let op = match (operator.ty, is_equal_follows) {
                (TokenType::Greater, false) => BinaryOp::Greater,
                (TokenType::Less, false) => BinaryOp::Lesser,
                (TokenType::Greater, true) => BinaryOp::GreaterOrEqual,
                (TokenType::Less, true) => BinaryOp::LessesOrEqual,
                _ => unreachable!(),
            };

            expr = self.untyped_expr(
                ExprKind::Binary(BinaryExpr {
                    operator: op,
                    left: Box::new(expr),
                    right: Box::new(right),
                }),
                self.span_from(start),
            );
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, NewParserError> {
        let start = self.current;
        let mut expr = self.comparison()?;

        if self.check(TokenType::Equal) {
            self.advance();

            if self.check(TokenType::Equal) {
                self.advance();

                let right = self.comparison()?;

                expr = self.untyped_expr(
                    ExprKind::Binary(BinaryExpr {
                        operator: BinaryOp::Equal,
                        left: Box::new(expr),
                        right: Box::new(right),
                    }),
                    self.span_from(start),
                );
            } else {
                return Err(self.error(
                    ErrorType::Unexpected,
                    "Unexpected '='. Did you mean '==' for equality comparison?".into(),
                ));
            }
        }

        Ok(expr)
    }

    fn closure(&mut self) -> Result<Expr, NewParserError> {
        let start = self.current;

        self.advance();
        self.expect(TokenType::LeftParen)?;
        let args = self.parse_parameters()?;
        self.expect(TokenType::RightParen)?;
        let body = self.block()?;

        if let ExprKind::Block(_) = &body.kind {
            let parameters = args
                .clone()
                .iter()
                .map(|(name, span, ty)| Parameter {
                    id: self.id_gen.next(),
                    name: name.clone(),
                    type_annotation: ty.clone(),
                    span: span.clone(),
                })
                .collect();

            Ok(self.untyped_expr(
                ExprKind::Closure(ClosureExpr {
                    parameters,
                    body: Box::new(body),
                    return_type: None,
                }),
                self.span_from(start),
            ))
        } else {
            unreachable!()
        }
    }

    fn expression(&mut self) -> Result<Expr, NewParserError> {
        if self.check(TokenType::At) {
            self.closure()
        } else {
            self.equality()
        }

        // we arent expecting a closing parenthesis here ')'
        // if self.scope_parenthesis == 0 {
        //     if !self.is_at_end() && !self.check(TokenType::Semicolon) && !self.is_valid_operator() {
        //         if self.check(TokenType::RightParen) || self.check(TokenType::RightBrace) {
        //             let delim = self.peek().unwrap().lexeme.clone();
        //             return Err(self.error(
        //                 ErrorType::Unexpected,
        //                 format!("Unexpected '{}'. Check for unbalanced delimiters.", delim),
        //             ));
        //         }

        //         return Err(self.error(
        //             ErrorType::Unexpected,
        //             format!(
        //                 "Unexpected token '{}'. Expected an operator or semicolon.",
        //                 self.peek().unwrap().lexeme
        //             ),
        //         ));
        //     }
        // }
    }

    fn return_statement(&mut self) -> Result<Stmt, NewParserError> {
        let start = self.current;
        self.expect(TokenType::Return)?;
        if let Ok(_) = self.expect(TokenType::Semicolon) {
            let span = self.span_from(start);
            Ok(self.stmt(StmtKind::Return(None), span))
        } else {
            // parse expression
            let value = self.expression()?;
            self.expect(TokenType::Semicolon)?;
            let span = self.span_from(start);
            Ok(self.stmt(StmtKind::Return(Some(value)), span))
        }
    }

    fn variable_declaration(&mut self) -> Result<Stmt, NewParserError> {
        let start = self.current;
        self.expect(TokenType::Let)?;
        let identifier = self.expect(TokenType::Identifier)?.clone();

        // type annotation
        let ty = if self.check(TokenType::Colon) {
            self.advance();
            let ty_start = self.current;
            let ty = self.expect(TokenType::Identifier)?;
            Some(TypeAnnotation::Named(
                ty.lexeme.clone(),
                self.span_from(ty_start),
            ))
        } else {
            None
        };

        self.expect(TokenType::Equal)?;
        let initializer = self.expression()?.clone();
        self.expect(TokenType::Semicolon)?;

        let span = self.span_from(start);
        Ok(self.stmt(
            StmtKind::Let(LetStmt {
                name: identifier.lexeme.clone(),
                ty,
                initializer: Box::new(initializer),
            }),
            span,
        ))
    }

    fn block(&mut self) -> Result<Expr, NewParserError> {
        let start = self.current;

        self.expect(TokenType::LeftBrace)?;

        let mut statements = vec![];

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize();
                }
            }
        }

        self.expect(TokenType::RightBrace)?;

        Ok(self.untyped_expr(
            ExprKind::Block(BlockExpr {
                statements,
                result: None,
                scope_id: 0,
            }),
            self.span_from(start),
        ))
    }

    // TODO: parse type annotation and return vec of Parameters...
    fn parse_parameters(&mut self) -> Result<Vec<(String, Span, TypeAnnotation)>, NewParserError> {
        let mut parameters = Vec::new();

        if !self.check(TokenType::RightParen) {
            parameters.push(self.parse_parameter()?);

            while self.check(TokenType::Comma) {
                self.advance(); // ,
                parameters.push(self.parse_parameter()?);
            }
        }

        Ok(parameters)
    }

    fn parse_parameter(&mut self) -> Result<(String, Span, TypeAnnotation), NewParserError> {
        let name = self.expect(TokenType::Identifier)?.clone();
        self.expect(TokenType::Colon).cloned().map_err(|_| {
            self.error(
                ErrorType::MissingTypeAnnotation,
                format!("Missing type annotation for argument '{}'", name.lexeme),
            )
        })?;
        let ty = self.expect(TokenType::Identifier)?.clone();

        let start = self.current;
        Ok((
            name.lexeme.clone(),
            name.span,
            TypeAnnotation::Named(ty.lexeme.clone(), self.span_from(start)),
        ))
    }

    fn function_declaration(&mut self) -> Result<Stmt, NewParserError> {
        let start = self.current;
        self.expect(TokenType::Function)?;
        let identifier = self.expect(TokenType::Identifier)?.clone();
        self.expect(TokenType::LeftParen)?;
        let parameters = self.parse_parameters()?;

        self.expect(TokenType::RightParen)?;

        // return type annotation
        let ty = if self.check(TokenType::ArrowRight) {
            self.advance();

            let ty_start = self.current;
            let ty = self.expect(TokenType::Identifier)?;
            Some(TypeAnnotation::Named(
                ty.lexeme.clone(),
                self.span_from(ty_start),
            ))
        } else {
            Some(TypeAnnotation::Never)
        };

        let body = self.block()?;

        if let ExprKind::Block(_) = body.kind {
        } else {
            panic!()
        }

        let parameters = parameters
            .clone()
            .iter()
            .map(|(name, span, ty)| Parameter {
                id: self.id_gen.next(),
                name: name.clone(),
                type_annotation: ty.clone(),
                span: span.clone(),
            })
            .collect();

        let span = self.span_from(start);
        Ok(self.stmt(
            StmtKind::FunctionDeclaration(FunctionDeclarationStmt {
                name: identifier.lexeme.clone(),
                return_ty: ty,
                parameters,
                body: Box::new(body),
            }),
            span,
        ))
    }

    fn assignment_statement(&mut self) -> Result<Stmt, NewParserError> {
        let start = self.current;
        let identifier = self.expect(TokenType::Identifier)?.clone();
        self.expect(TokenType::Equal)?;

        let assignment = self.expression()?;

        self.expect(TokenType::Semicolon)?;

        let span = self.span_from(start);
        Ok(self.stmt(
            StmtKind::Assignment(identifier.lexeme.clone(), Box::new(assignment)),
            span,
        ))
    }

    fn if_statement(&mut self) -> Result<Stmt, NewParserError> {
        let start = self.current;
        self.advance();

        let condition = self.expression()?;
        let then_block = self.block()?;

        let else_block = if self.check(TokenType::Else) {
            let else_start = self.current;
            self.advance();

            if self.check(TokenType::If) {
                Some(Box::new(self.if_statement()?))
            } else {
                let b = self.block()?;
                let span = self.span_from(else_start);
                Some(Box::new(self.stmt(StmtKind::Expr(Box::new(b)), span)))
            }
        } else {
            None
        };

        let span = self.span_from(start);
        let then_span = then_block.span.clone();
        let then_block = Box::new(self.stmt(StmtKind::Expr(Box::new(then_block)), then_span));
        Ok(self.stmt(
            StmtKind::If(IfStmt {
                condition: Box::new(condition),
                then_block,
                else_block,
            }),
            span,
        ))
    }

    fn declaration(&mut self) -> Result<Stmt, NewParserError> {
        let start = self.current;
        if self.check(TokenType::Function) {
            self.function_declaration()
        } else if self.check(TokenType::Let) {
            self.variable_declaration()
        } else if self.check(TokenType::Return) {
            self.return_statement()
        } else if self.check(TokenType::If) {
            self.if_statement()
        } else if self.check(TokenType::Identifier) {
            let saved_position = self.current;
            self.advance();

            if self.check(TokenType::Equal) {
                self.current = saved_position;
                let assignment = self.assignment_statement()?;
                Ok(assignment)
            } else {
                self.current = saved_position;
                let expression = self.expression()?;
                self.expect(TokenType::Semicolon)?;
                let span = self.span_from(start);
                Ok(self.stmt(StmtKind::Expr(Box::new(expression)), span))
            }
        } else {
            let expression = self.expression()?;
            self.expect(TokenType::Semicolon)?;

            let span = self.span_from(start);
            Ok(self.stmt(StmtKind::Expr(Box::new(expression)), span))
        }
    }

    fn parse_program(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize();
                }
            }
        }

        statements
    }

    pub fn generate_ast(&mut self) -> Result<Vec<Stmt>, Vec<NewParserError>> {
        // Create a timestamp for the start time
        let start = Instant::now();

        let statements = self.parse_program();
        // Calculate the time elapsed since start
        let elapsed = start.elapsed();

        // Get the elapsed time in different units
        let elapsed_ms = elapsed.as_millis();
        let elapsed_secs = elapsed.as_secs_f64();

        println!("Time elapsed: {} milliseconds", elapsed_ms);
        println!("Time elapsed: {:.6} seconds", elapsed_secs);

        if self.errors.is_empty() {
            Ok(statements)
        } else {
            Err(self.errors.clone())
        }
    }
}
