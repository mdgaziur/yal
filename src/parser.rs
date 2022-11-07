use crate::ast::{
    AssignmentExpr, BinaryExpr, BinaryOperation, CallExpr, DataExpr, DataStmt, Expr, ExprContainer,
    FunStmt, GetExpr, IfStmt, IndexExpr, IndexSetExpr, IterStmt, LogicalExpr, LogicalOperation,
    MethodsStmt, Mutable, NumberExpr, SetExpr, Stmt, StmtContainer, UnaryExpr, UnaryOperation,
    VarStmt, VariableExpr,
};
use crate::diagnostics::{Diagnostic, ErrorCode, Severity, Span};

use crate::lexer::{Lexer, Token, TokenKind};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    diagnostics: Vec<Diagnostic>,
    pos: usize,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        let mut lexer = Lexer::new(source);
        let mut tokens = vec![];
        let mut lexer_diags = vec![];

        loop {
            let res = lexer.lex_once();
            if let Ok(token) = res {
                tokens.push(token);
                if token.kind == TokenKind::Eof {
                    break;
                }
            } else if let Err(e) = res {
                lexer_diags.push(e);
            }
        }

        Self {
            tokens,
            diagnostics: lexer_diags,
            pos: 0,
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, Diagnostic> {
        let token = self.peek();

        if token.kind == kind {
            self.advance();
            Ok(token)
        } else if token.kind == TokenKind::Eof {
            Err(Self::error_for_token(
                ErrorCode::UnexpectedToken,
                "unexpected end of file",
                token,
            ))
        } else {
            Err(Self::error_for_token(
                ErrorCode::UnexpectedToken,
                "Unexpected token",
                token,
            ))
        }
    }

    fn previous(&self) -> Token {
        self.tokens[self.pos - 1]
    }

    fn unless(&mut self, kind: TokenKind) -> Result<bool, Diagnostic> {
        if self.is_eof() {
            Err(Self::error_for_token(
                ErrorCode::UnexpectedToken,
                "unexpected end of file",
                self.peek(),
            ))
        } else if self.peek().kind == kind {
            self.advance();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn peek(&self) -> Token {
        self.tokens[self.pos]
    }

    fn is_eof(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    pub fn parse(&mut self) -> Vec<StmtContainer> {
        let mut stmts = vec![];

        while !self.is_eof() {
            let res = self.declaration();
            if let Ok(stmt) = res {
                stmts.push(stmt);
            } else if let Err(e) = res {
                self.diagnostics.push(e);
                self.synchronize();
            }
        }

        stmts
    }

    fn synchronize(&mut self) {
        if self.is_eof() {
            return;
        }
        self.advance();

        loop {
            match self.previous().kind {
                TokenKind::Semicolon
                | TokenKind::LeftBrace
                | TokenKind::RightBrace
                | TokenKind::If
                | TokenKind::Const
                | TokenKind::Mut
                | TokenKind::Loop
                | TokenKind::Iter
                | TokenKind::Fun
                | TokenKind::Data
                | TokenKind::Continue
                | TokenKind::Break
                | TokenKind::Ret
                | TokenKind::Methods
                | TokenKind::Eof => break,
                _ => (),
            }

            self.advance();
        }
    }

    fn declaration(&mut self) -> Result<StmtContainer, Diagnostic> {
        match self.peek().kind {
            TokenKind::Const => self.var_declaration(Mutable::No),
            TokenKind::Mut => self.var_declaration(Mutable::Yes),
            TokenKind::Fun => self.fun_stmt(false),
            TokenKind::Data => self.data_stmt(),
            TokenKind::Methods => self.methods_stmt(),
            _ => {
                let token = self.peek();
                self.advance();

                Err(Self::error_for_token(
                    ErrorCode::UnexpectedToken,
                    "unexpected token",
                    token,
                ))
            }
        }
    }

    fn data_stmt(&mut self) -> Result<StmtContainer, Diagnostic> {
        let start_span = self.peek().span;
        self.advance();
        let name = self.expect(TokenKind::Identifier)?.value.get_string();
        let mut fields = vec![];

        self.expect(TokenKind::LeftBrace)?;

        let mut is_first_property = true;
        while !self.unless(TokenKind::RightBrace)? {
            if !is_first_property {
                self.expect(TokenKind::Comma)?;
            } else {
                is_first_property = false;
            }

            fields.push(self.expect(TokenKind::Identifier)?.value.get_string());
        }

        Ok(Stmt::Data(Box::new(DataStmt { name, fields }))
            .into_container(start_span.merge(self.previous().span)))
    }

    fn methods_stmt(&mut self) -> Result<StmtContainer, Diagnostic> {
        let start_span = self.peek().span;
        self.advance();
        let data = self.expect(TokenKind::Identifier)?.value.get_string();
        let mut methods = vec![];

        self.expect(TokenKind::LeftBrace)?;
        while !self.unless(TokenKind::RightBrace)? {
            methods.push(self.fun_stmt(true)?);
        }

        Ok(Stmt::Methods(Box::new(MethodsStmt { methods, data }))
            .into_container(start_span.merge(self.previous().span)))
    }

    fn var_declaration(&mut self, mutable: Mutable) -> Result<StmtContainer, Diagnostic> {
        let span_start = self.peek().span;
        self.advance();
        let name = self.expect(TokenKind::Identifier)?.value.get_string();
        self.expect(TokenKind::Equal)?;
        let value = self.expr()?;
        self.expect(TokenKind::Semicolon)?;

        Ok(Stmt::Var(Box::new(VarStmt {
            name,
            value,
            mutable,
        }))
        .into_container(span_start.merge(self.previous().span)))
    }

    fn fun_stmt(&mut self, is_method: bool) -> Result<StmtContainer, Diagnostic> {
        let start_span = self.peek().span;
        self.advance();
        let name = self.expect(TokenKind::Identifier)?.value.get_string();
        self.expect(TokenKind::LeftParen)?;

        let mut arguments = vec![];
        let mut is_first_arg = true;
        loop {
            if matches!(self.peek().kind, TokenKind::RightParen) {
                break;
            }
            if !is_first_arg {
                self.expect(TokenKind::Comma)?;
            } else {
                is_first_arg = false;
            }

            let current_token = self.peek();
            if current_token.kind == TokenKind::Identifier {
                self.advance();
                arguments.push(current_token.value.get_string());
            } else {
                self.advance();
                break;
            }
        }
        self.expect(TokenKind::RightParen)?;
        let body = self.block_stmt()?;

        Ok(Stmt::Fun(Box::new(FunStmt {
            name,
            arguments,
            body,
            is_method,
        }))
        .into_container(start_span.merge(self.peek().span)))
    }

    fn ret_stmt(&mut self) -> Result<StmtContainer, Diagnostic> {
        let start_span = self.peek().span;
        self.advance();
        let expr = self.expr()?;
        self.expect(TokenKind::Semicolon)?;

        Ok(Stmt::Ret(expr).into_container(start_span.merge(self.previous().span)))
    }

    fn iter_stmt(&mut self) -> Result<StmtContainer, Diagnostic> {
        let start_span = self.peek().span;
        self.advance();
        let iterable = self.expr()?;
        self.expect(TokenKind::Colon)?;
        let binding = self.expect(TokenKind::Identifier)?.value.get_string();
        let block = self.block_stmt()?;

        Ok(Stmt::Iter(Box::new(IterStmt {
            iterable,
            binding,
            block,
        }))
        .into_container(start_span.merge(self.previous().span)))
    }

    fn if_stmt(&mut self) -> Result<StmtContainer, Diagnostic> {
        let start_span = self.peek().span;
        self.advance();
        self.expect(TokenKind::LeftParen)?;
        let condition = self.expr()?;
        self.expect(TokenKind::RightParen)?;
        let block = self.block_stmt()?;

        let else_;
        let mut then = None;
        if self.peek().kind == TokenKind::Else {
            self.advance();
            if self.peek().kind == TokenKind::If {
                else_ = Some(self.if_stmt()?);
            } else {
                else_ = None;
                then = Some(self.block_stmt()?);
            }
        } else {
            else_ = None;
        }

        Ok(Stmt::If(Box::new(IfStmt {
            condition,
            block,
            else_,
            then,
        }))
        .into_container(start_span.merge(self.previous().span)))
    }

    fn block_stmt(&mut self) -> Result<StmtContainer, Diagnostic> {
        let start_span = self.peek().span;
        let mut stmts = vec![];
        self.expect(TokenKind::LeftBrace)?;

        while !self.unless(TokenKind::RightBrace)? {
            let res =
                match self.peek().kind {
                    TokenKind::Const => self.var_declaration(Mutable::No),
                    TokenKind::Mut => self.var_declaration(Mutable::Yes),
                    // TokenKind::Fun => self.fun_stmt(false),
                    // TokenKind::Data => self.data_stmt(),
                    // TokenKind::Methods => self.methods_stmt(),
                    TokenKind::LeftBrace => self.block_stmt(),
                    TokenKind::Ret => self.ret_stmt(),
                    TokenKind::Continue => {
                        let span = self.peek().span;
                        self.advance();
                        self.expect(TokenKind::Semicolon)?;

                        Ok(Stmt::Continue.into_container(span.merge(self.previous().span)))
                    }
                    TokenKind::Break => {
                        let span = self.peek().span;
                        self.advance();
                        self.expect(TokenKind::Semicolon)?;

                        Ok(Stmt::Break.into_container(span.merge(self.previous().span)))
                    }
                    TokenKind::Loop => {
                        let span = self.peek().span;
                        self.advance();
                        let block = self.block_stmt()?;

                        Ok(Stmt::Loop(Box::new(block))
                            .into_container(span.merge(self.previous().span)))
                    }
                    TokenKind::Iter => self.iter_stmt(),
                    TokenKind::If => self.if_stmt(),
                    _ => {
                        let expr = self.expr()?;
                        let span = expr.span;
                        self.expect(TokenKind::Semicolon)?;

                        Ok(Stmt::Expr(expr).into_container(span))
                    }
                };

            if let Ok(res) = res {
                stmts.push(res);
            } else if let Err(e) = res {
                self.diagnostics.push(e);
                self.synchronize();
            }
        }

        Ok(Stmt::Block(stmts).into_container(start_span.merge(self.previous().span)))
    }

    fn expr(&mut self) -> Result<ExprContainer, Diagnostic> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<ExprContainer, Diagnostic> {
        let start_span = self.peek().span;

        let expr = self.logical_or()?;

        match self.peek().kind {
            TokenKind::DoubleLeftAngleEq
            | TokenKind::DoubleRightAngleEq
            | TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::AsteriskEq
            | TokenKind::SlashEq
            | TokenKind::PercentEq
            | TokenKind::AmpersandEq
            | TokenKind::PipeEq
            | TokenKind::CaretEq => {
                let op = match self.peek().kind {
                    TokenKind::DoubleLeftAngleEq => BinaryOperation::LeftShift,
                    TokenKind::DoubleRightAngleEq => BinaryOperation::RightShift,
                    TokenKind::PlusEq => BinaryOperation::Plus,
                    TokenKind::MinusEq => BinaryOperation::Minus,
                    TokenKind::AsteriskEq => BinaryOperation::Multiply,
                    TokenKind::SlashEq => BinaryOperation::Divide,
                    TokenKind::PercentEq => BinaryOperation::Modulus,
                    TokenKind::AmpersandEq => BinaryOperation::And,
                    TokenKind::PipeEq => BinaryOperation::Or,
                    TokenKind::CaretEq => BinaryOperation::Xor,
                    _ => unreachable!(),
                };
                let eq_token = self.peek();
                self.advance();
                let rhs = self.expr()?;

                return match &expr.expr {
                    Expr::Get(get_expr) => Ok(Expr::Set(Box::new(SetExpr {
                        property: get_expr.property,
                        object: expr.clone(),
                        value: Expr::Binary(Box::new(BinaryExpr {
                            lhs: expr.clone(),
                            op,
                            rhs,
                        }))
                        .into_container(start_span.merge(self.peek().span)),
                    }))
                    .into_container(start_span.merge(self.peek().span))),
                    Expr::Index(_) => Ok(Expr::IndexSet(Box::new(IndexSetExpr {
                        object: expr.clone(),
                        value: Expr::Binary(Box::new(BinaryExpr {
                            lhs: expr.clone(),
                            op,
                            rhs,
                        }))
                        .into_container(start_span.merge(self.peek().span)),
                    }))
                    .into_container(start_span.merge(self.peek().span))),
                    Expr::Variable(_) => Ok(Expr::Assignment(Box::new(AssignmentExpr {
                        lvalue: expr.clone(),
                        rvalue: Expr::Binary(Box::new(BinaryExpr {
                            lhs: expr.clone(),
                            op,
                            rhs,
                        }))
                        .into_container(start_span.merge(self.peek().span)),
                    }))
                    .into_container(start_span.merge(self.peek().span))),
                    _ => Err(Self::error_for_token(
                        ErrorCode::InvalidAssignment,
                        "invalid assignment target",
                        eq_token,
                    )),
                };
            }
            _ => (),
        }

        if self.peek().kind == TokenKind::Equal {
            let eq_token = self.peek();
            self.advance();
            let rhs = self.expr()?;

            return match &expr.expr {
                Expr::Get(get_expr) => {
                    let expr = get_expr.clone();

                    Ok(Expr::Set(Box::new(SetExpr {
                        property: expr.property,
                        object: expr.object,
                        value: rhs,
                    }))
                    .into_container(start_span.merge(self.peek().span)))
                }
                Expr::Index(_) => Ok(Expr::IndexSet(Box::new(IndexSetExpr {
                    object: expr,
                    value: rhs,
                }))
                .into_container(start_span.merge(self.peek().span))),
                Expr::Variable(_) => Ok(Expr::Assignment(Box::new(AssignmentExpr {
                    lvalue: expr,
                    rvalue: rhs,
                }))
                .into_container(start_span.merge(self.peek().span))),
                _ => Err(Self::error_for_token(
                    ErrorCode::InvalidAssignment,
                    "invalid assignment target",
                    eq_token,
                )),
            };
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<ExprContainer, Diagnostic> {
        let start_span = self.peek().span;
        let mut expr = self.logical_and()?;

        let mut op;

        loop {
            op = LogicalOperation::try_from(self.peek().kind);
            if let Ok(op) = op {
                if op == LogicalOperation::Or {
                    self.advance();
                    let rhs = self.logical_and()?;

                    expr = Expr::Logical(Box::new(LogicalExpr { lhs: expr, op, rhs }))
                        .into_container(start_span.merge(self.peek().span));
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<ExprContainer, Diagnostic> {
        let start_span = self.peek().span;
        let mut expr = self.equality()?;

        let mut op;

        loop {
            op = LogicalOperation::try_from(self.peek().kind);
            if let Ok(op) = op {
                if op == LogicalOperation::And {
                    self.advance();
                    let rhs = self.equality()?;

                    expr = Expr::Logical(Box::new(LogicalExpr { lhs: expr, op, rhs }))
                        .into_container(start_span.merge(self.peek().span));
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<ExprContainer, Diagnostic> {
        let start_span = self.peek().span;
        let mut expr = self.comparison()?;

        let mut op;

        loop {
            op = LogicalOperation::try_from(self.peek().kind);
            if let Ok(op) = op {
                if op == LogicalOperation::Equals || op == LogicalOperation::NotEquals {
                    self.advance();
                    let rhs = self.comparison()?;

                    expr = Expr::Logical(Box::new(LogicalExpr { lhs: expr, op, rhs }))
                        .into_container(start_span.merge(self.peek().span));
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<ExprContainer, Diagnostic> {
        let start_span = self.peek().span;
        let mut expr = self.bitwise_or()?;

        let mut op;

        loop {
            op = LogicalOperation::try_from(self.peek().kind);
            if let Ok(op) = op {
                if op == LogicalOperation::LessThan
                    || op == LogicalOperation::LessThanOrEquals
                    || op == LogicalOperation::GreaterThan
                    || op == LogicalOperation::GreaterThanOrEquals
                {
                    self.advance();
                    let rhs = self.bitwise_or()?;

                    expr = Expr::Logical(Box::new(LogicalExpr { lhs: expr, op, rhs }))
                        .into_container(start_span.merge(self.peek().span));
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn bitwise_or(&mut self) -> Result<ExprContainer, Diagnostic> {
        let start_span = self.peek().span;
        let mut expr = self.bitwise_xor()?;

        let mut op;

        loop {
            op = BinaryOperation::try_from(self.peek().kind);
            if let Ok(op) = op {
                if op == BinaryOperation::Or {
                    self.advance();
                    let rhs = self.bitwise_xor()?;

                    expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }))
                        .into_container(start_span.merge(self.peek().span));
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn bitwise_xor(&mut self) -> Result<ExprContainer, Diagnostic> {
        let start_span = self.peek().span;
        let mut expr = self.bitwise_and()?;

        let mut op;

        loop {
            op = BinaryOperation::try_from(self.peek().kind);
            if let Ok(op) = op {
                if op == BinaryOperation::Xor {
                    self.advance();
                    let rhs = self.bitwise_and()?;

                    expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }))
                        .into_container(start_span.merge(self.peek().span));
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn bitwise_and(&mut self) -> Result<ExprContainer, Diagnostic> {
        let start_span = self.peek().span;
        let mut expr = self.bitwise_shift()?;

        let mut op;

        loop {
            op = BinaryOperation::try_from(self.peek().kind);
            if let Ok(op) = op {
                if op == BinaryOperation::And {
                    self.advance();
                    let rhs = self.bitwise_shift()?;

                    expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }))
                        .into_container(start_span.merge(self.peek().span));
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn bitwise_shift(&mut self) -> Result<ExprContainer, Diagnostic> {
        let start_span = self.peek().span;
        let mut expr = self.term()?;

        let mut op;

        loop {
            op = BinaryOperation::try_from(self.peek().kind);
            if let Ok(op) = op {
                if op == BinaryOperation::LeftShift || op == BinaryOperation::RightShift {
                    self.advance();
                    let rhs = self.term()?;

                    expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }))
                        .into_container(start_span.merge(self.peek().span));
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<ExprContainer, Diagnostic> {
        let start_span = self.peek().span;
        let mut expr = self.factor()?;

        let mut op;

        loop {
            op = BinaryOperation::try_from(self.peek().kind);
            if let Ok(op) = op {
                if op == BinaryOperation::Plus || op == BinaryOperation::Minus {
                    self.advance();
                    let rhs = self.factor()?;

                    expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }))
                        .into_container(start_span.merge(self.peek().span));
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<ExprContainer, Diagnostic> {
        let start_span = self.peek().span;
        let mut expr = self.unary()?;

        let mut op;

        loop {
            op = BinaryOperation::try_from(self.peek().kind);
            if let Ok(op) = op {
                if op == BinaryOperation::Multiply || op == BinaryOperation::Divide {
                    self.advance();
                    let rhs = self.unary()?;

                    expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }))
                        .into_container(start_span.merge(self.peek().span));
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<ExprContainer, Diagnostic> {
        let op = match UnaryOperation::try_from(self.peek().kind) {
            Ok(op) => op,
            Err(_) => return self.callexpr(),
        };
        let start_span = self.peek().span;
        self.advance();

        Ok(Expr::Unary(Box::new(UnaryExpr {
            expr: self.unary()?,
            op,
        }))
        .into_container(start_span.merge(self.peek().span)))
    }

    fn callexpr(&mut self) -> Result<ExprContainer, Diagnostic> {
        let start_span = self.peek().span;
        let mut expr = self.primary()?;

        loop {
            if self.peek().kind == TokenKind::LeftParen {
                self.advance();
                let args = self.arguments()?;
                self.expect(TokenKind::RightParen)?;
                expr = Expr::Call(Box::new(CallExpr { callee: expr, args }))
                    .into_container(start_span.merge(self.peek().span));
            } else if self.peek().kind == TokenKind::Dot {
                self.advance();
                let name = self.expect(TokenKind::Identifier)?;
                expr = Expr::Get(Box::new(GetExpr {
                    property: name.value.get_string(),
                    object: expr,
                }))
                .into_container(start_span.merge(self.peek().span));
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn arguments(&mut self) -> Result<Vec<ExprContainer>, Diagnostic> {
        let mut args = vec![];
        if self.peek().kind == TokenKind::RightParen {
            return Ok(args);
        }
        args.push(self.expr()?);

        loop {
            if self.peek().kind != TokenKind::Comma {
                break;
            }

            self.advance();
            args.push(self.expr()?);
        }

        Ok(args)
    }

    fn primary(&mut self) -> Result<ExprContainer, Diagnostic> {
        let data_member;
        if self.peek().kind == TokenKind::At {
            data_member = true;
            self.advance();

            if self.is_eof() {
                return Err(Self::error_for_token(
                    ErrorCode::UnexpectedToken,
                    "Unexpected EOF",
                    self.peek(),
                ));
            }
        } else {
            data_member = false;
        }
        let current_token = self.peek();
        self.advance();

        match current_token.kind {
            TokenKind::True => Ok(Expr::Boolean(true).into_container(current_token.span)),
            TokenKind::False => Ok(Expr::Boolean(false).into_container(current_token.span)),
            TokenKind::None => Ok(Expr::None.into_container(current_token.span)),
            TokenKind::Identifier => {
                let start_span = self.peek().span;
                let mut expr = Expr::Variable(VariableExpr {
                    name: current_token.value.get_string(),
                    data_member,
                })
                .into_container(current_token.span);
                if self.peek().kind == TokenKind::LeftBracket {
                    self.advance();
                    let index = self.expr()?;
                    self.expect(TokenKind::RightBracket)?;

                    expr = Expr::Index(Box::new(IndexExpr {
                        object: expr,
                        index,
                    }))
                    .into_container(start_span.merge(self.previous().span));
                } else if self.peek().kind == TokenKind::LeftBrace {
                    if data_member {
                        return Err(Self::error_for_token(
                            ErrorCode::InvalidType,
                            "Expected data, found type",
                            current_token,
                        ));
                    }
                    self.advance();
                    let name = match expr.expr {
                        Expr::Variable(n) => n.name,
                        _ => unreachable!(),
                    };
                    let mut props = HashMap::new();
                    let mut is_first_prop = true;
                    let mut err = None;

                    while !self.unless(TokenKind::RightBrace)? {
                        if !is_first_prop {
                            self.expect(TokenKind::Comma)?;
                        } else {
                            is_first_prop = false;
                        }

                        let prop_span = self.peek().span;
                        let prop = self.expect(TokenKind::Identifier)?.value.get_string();
                        self.expect(TokenKind::Colon)?;
                        let value = self.expr()?;
                        let sp = prop_span.merge(value.span);

                        if let Some(_) = props.insert(prop, value) {
                            err = Some(Diagnostic {
                                code: ErrorCode::InvalidDataPropertySet,
                                severity: Severity::Error,
                                message: String::from(
                                    "Attempt to set same property more than once",
                                ),
                                span: sp,
                            })
                        }
                    }

                    if let Some(e) = err {
                        return Err(e);
                    }

                    expr = Expr::Data(Box::new(DataExpr { name, props }))
                        .into_container(start_span.merge(self.previous().span));
                }

                Ok(expr)
            }
            TokenKind::String => {
                Ok(Expr::String(current_token.value.get_string())
                    .into_container(current_token.span))
            }
            TokenKind::Number => {
                let (value, kind) = current_token.value.get_number();
                Ok(Expr::Number(NumberExpr { kind, value }).into_container(current_token.span))
            }
            TokenKind::LeftParen => {
                let expr = self.expr()?;
                self.expect(TokenKind::RightParen)?;

                Ok(Expr::Grouping(Box::new(expr)).into_container(current_token.span))
            }
            TokenKind::LeftBracket => {
                let mut exprs = vec![];
                let mut is_first_item = true;
                while !self.unless(TokenKind::RightBracket)? {
                    if !is_first_item {
                        self.expect(TokenKind::Comma)?;
                    } else {
                        is_first_item = false;
                    }

                    exprs.push(self.expr()?);
                }

                Ok(Expr::Array(exprs).into_container(current_token.span))
            }
            _ => Err(Self::error_for_token(
                ErrorCode::UnexpectedToken,
                "expected primary expression",
                current_token,
            )),
        }
    }

    fn error_for_token(code: ErrorCode, message: &str, token: Token) -> Diagnostic {
        Diagnostic {
            severity: Severity::Error,
            code,
            message: message.to_string(),
            span: token.span,
        }
    }

    pub fn get_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }
}
