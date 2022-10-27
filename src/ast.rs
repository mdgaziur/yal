use crate::diagnostics::Span;
use crate::interner::InternedString;
use crate::lexer::{NumberKind, TokenKind};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct StmtContainer {
    pub stmt: Stmt,
    pub span: Span,
}

impl StmtContainer {
    pub fn new(stmt: Stmt, span: Span) -> Self {
        Self { stmt, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Var(Box<VarStmt>),
    Fun(Box<FunStmt>),
    Data(Box<DataStmt>),
    Methods(Box<MethodsStmt>),
    Block(Vec<StmtContainer>),
    Loop(Box<StmtContainer>),
    Iter(Box<IterStmt>),
    If(Box<IfStmt>),
    Expr(ExprContainer),
    Ret(ExprContainer),
    Continue,
    Break,
}

impl Stmt {
    pub fn into_container(self, span: Span) -> StmtContainer {
        StmtContainer { stmt: self, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub condition: ExprContainer,
    pub block: StmtContainer,
    pub else_: Option<StmtContainer>,
    pub then: Option<StmtContainer>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IterStmt {
    pub iterable: ExprContainer,
    pub binding: InternedString,
    pub block: StmtContainer,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarStmt {
    pub name: InternedString,
    pub value: ExprContainer,
    pub mutable: Mutable,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mutable {
    Yes,
    No,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunStmt {
    pub name: InternedString,
    pub arguments: Vec<InternedString>,
    pub body: StmtContainer,
    pub is_method: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataStmt {
    pub name: InternedString,
    pub fields: Vec<InternedString>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodsStmt {
    pub data: InternedString,
    pub methods: Vec<StmtContainer>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprContainer {
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Data(Box<DataExpr>),
    Assignment(Box<AssignmentExpr>),
    Get(Box<GetExpr>),
    Index(Box<IndexExpr>),
    IndexSet(Box<IndexSetExpr>),
    Set(Box<SetExpr>),
    Logical(Box<LogicalExpr>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Call(Box<CallExpr>),
    Boolean(bool),
    None,
    String(InternedString),
    Number(NumberExpr),
    Grouping(Box<ExprContainer>),
    Array(Vec<ExprContainer>),
    Variable(InternedString),
}

impl Expr {
    pub fn into_container(self, span: Span) -> ExprContainer {
        ExprContainer { expr: self, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataExpr {
    pub name: InternedString,
    pub props: HashMap<InternedString, ExprContainer>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpr {
    pub lvalue: ExprContainer,
    pub rvalue: ExprContainer,
    pub data_member: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GetExpr {
    pub property: InternedString,
    pub object: ExprContainer,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub index: ExprContainer,
    pub object: ExprContainer,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexSetExpr {
    pub object: ExprContainer,
    pub value: ExprContainer,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SetExpr {
    pub object: ExprContainer,
    pub property: InternedString,
    pub value: ExprContainer,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalExpr {
    pub lhs: ExprContainer,
    pub op: LogicalOperation,
    pub rhs: ExprContainer,
}

#[derive(Debug, Clone, Copy, Eq, Ord, PartialOrd, PartialEq)]
pub enum LogicalOperation {
    And,
    Or,
    Equals,
    NotEquals,
    GreaterThan,
    GreaterThanOrEquals,
    LessThan,
    LessThanOrEquals,
}

impl TryFrom<TokenKind> for LogicalOperation {
    type Error = ();

    fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
        match kind {
            TokenKind::DoubleAmpersand => Ok(Self::And),
            TokenKind::DoublePipe => Ok(Self::Or),
            TokenKind::DoubleEqual => Ok(Self::Equals),
            TokenKind::ExclamationEq => Ok(Self::NotEquals),
            TokenKind::RightAngle => Ok(Self::GreaterThan),
            TokenKind::RightAngleEq => Ok(Self::GreaterThanOrEquals),
            TokenKind::LeftAngle => Ok(Self::LessThan),
            TokenKind::LeftAngleEq => Ok(Self::LessThanOrEquals),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub lhs: ExprContainer,
    pub op: BinaryOperation,
    pub rhs: ExprContainer,
}

#[derive(Debug, Clone, Copy, Eq, Ord, PartialOrd, PartialEq)]
pub enum BinaryOperation {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,
    And,
    Or,
    Xor,
    LeftShift,
    RightShift,
}

impl TryFrom<TokenKind> for BinaryOperation {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Plus => Ok(Self::Plus),
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Asterisk => Ok(Self::Multiply),
            TokenKind::Slash => Ok(Self::Divide),
            TokenKind::Percent => Ok(Self::Modulus),
            TokenKind::Ampersand => Ok(Self::And),
            TokenKind::Pipe => Ok(Self::Or),
            TokenKind::Caret => Ok(Self::Xor),
            TokenKind::DoubleLeftAngle => Ok(Self::LeftShift),
            TokenKind::DoubleRightAngle => Ok(Self::RightShift),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOperation,
    pub expr: ExprContainer,
}

#[derive(Debug, Clone, Copy, Eq, Ord, PartialOrd, PartialEq)]
pub enum UnaryOperation {
    Not,
    Negate,
}

impl TryFrom<TokenKind> for UnaryOperation {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Exclamation => Ok(Self::Not),
            TokenKind::Minus => Ok(Self::Negate),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: ExprContainer,
    pub args: Vec<ExprContainer>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumberExpr {
    pub value: InternedString,
    pub kind: NumberKind,
}
