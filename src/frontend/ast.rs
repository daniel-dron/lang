use std::collections::HashMap;

use crate::types::Type;

#[derive(Debug)]
pub struct NodeIdGenerator {
    next_id: usize,
}

impl NodeIdGenerator {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    pub fn next(&mut self) -> NodeId {
        let id = self.next_id;
        self.next_id += 1;
        NodeId(id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }

    pub fn len(&self) -> usize {
        self.end - self.start // inclusive
    }
}

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub ty: Type,
    pub expr: TypeExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub id: NodeId,
    pub name: String,
    pub type_annotation: TypeAnnotation,
    pub span: Span, // this span is actually into tokens
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Array(Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    Greater,
    Lesser,
    GreaterOrEqual,
    LessesOrEqual,
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate, // -
    Not,    // !
}

///
/// Expressions
///

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(Literal),
    Identifier(String),
    IndexAccess(ArrayIndexExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Block(BlockExpr),
    Call(CallExpr),
    Closure(ClosureExpr),
    Instance(InstanciateTypeExp),
    MemberAccess(MemberAccessExpr),
}

#[derive(Debug, Clone)]
pub struct ArrayIndexExpr {
    pub target: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub operator: BinaryOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub operand: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub statements: Vec<Stmt>,
    pub result: Option<Box<Expr>>,
    pub scope_id: usize,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct ClosureExpr {
    pub parameters: Vec<Parameter>,
    pub body: Box<Expr>,
    pub return_type: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct InstanciateTypeExp {
    pub name: String,
    pub fields: HashMap<String, Expr>,
}

#[derive(Debug, Clone)]
pub struct MemberAccessExpr {
    pub target: Box<Expr>,
    pub name: String,
    pub id: usize,
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Unspecified,
    Named(String),
    Function {
        parameters: Vec<TypeExpr>,
        return_ty: Box<TypeExpr>,
    },
    Array(Box<TypeExpr>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span, // this span is actually into tokens
    pub ty: Type,
}

impl Expr {
    pub fn new(id: NodeId, kind: ExprKind, span: Span) -> Self {
        Self {
            id,
            kind,
            span,
            ty: Type::Never,
        }
    }
}

///
/// Statements
///

#[derive(Debug, Clone)]
pub enum Assignable {
    Identifier(String), // Regular variable assignment: x = ...
    IndexAccess {
        // Array element assignment: arr[0] = ...
        object: Box<Expr>,
        index: Box<Expr>,
    },
    MemberAcess {
        target: Box<Expr>,
        field: String,
    },
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expr(Box<Expr>),
    Let(LetStmt),
    Assignment(Assignable, Box<Expr>),
    If(IfStmt),
    Return(Option<Expr>),
    FunctionDeclaration(FunctionDeclarationStmt),
    TypeDeclaration(Box<TypeDeclarationStmt>),
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span, // this span is actually into tokens!!
    pub ty: Type,
}

impl Stmt {
    pub fn new(id: NodeId, kind: StmtKind, span: Span) -> Self {
        Self {
            id,
            kind,
            span,
            ty: Type::Never,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub name: String,
    pub ty: Option<TypeAnnotation>,
    pub initializer: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Box<Expr>,
    pub then_block: Box<Stmt>,
    pub else_block: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclarationStmt {
    pub name: String,
    pub return_ty: TypeAnnotation,
    pub parameters: Vec<Parameter>,
    pub body: Box<Expr>, // usually ExprKind::Block
}

#[derive(Debug, Clone)]
pub struct TypeDeclarationStmt {
    pub name: String,
    pub fields: HashMap<String, TypeAnnotation>,
    pub fields_ordered: Vec<String>,
    pub ty: Type,
}
