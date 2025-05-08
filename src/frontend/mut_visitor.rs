use crate::types;

use super::ast::{
    Assignable, AssignmentStmt, BinaryExpr, BlockExpr, CallExpr, Expr, ExprKind,
    FunctionDeclarationStmt, Identifier, IfStmt, LetStmt, Literal, Parameter, Stmt, StmtKind,
    TypeAnnotation, TypeDeclarationStmt,
};

pub trait MutVisitor: Sized {
    fn visit_ast(&mut self, ast: &mut [Stmt]) {
        walk_ast(self, ast);
    }

    fn visit_statement(&mut self, stmt: &mut Stmt) {
        walk_statement(self, stmt);
    }

    fn visit_let_statement(&mut self, stmt: &mut LetStmt) {
        walk_let_statement(self, stmt);
    }

    fn visit_assignment(&mut self, assignment: &mut AssignmentStmt) {
        walk_assignment(self, assignment);
    }

    fn visit_if_statement(&mut self, stmt: &mut IfStmt) {
        walk_if_statement(self, stmt);
    }

    fn visit_return(&mut self, ret: &mut Option<Expr>) {
        walk_return(self, ret);
    }

    fn visit_function_declaration(&mut self, stmt: &mut FunctionDeclarationStmt) {
        walk_function_declaration(self, stmt);
    }

    fn visit_type_declaration(&mut self, stmt: &mut TypeDeclarationStmt) {
        walk_type_declaration(self, stmt);
    }

    fn visit_expression(&mut self, expr: &mut Expr) {
        walk_expression(self, expr);
    }

    fn visit_literal(&mut self, literal: &mut Literal) {
        walk_literal(self, literal);
    }

    fn visit_identifier(&mut self, identifier: &mut Identifier) {
        walk_identifier(self, identifier);
    }

    fn visit_index_access(&mut self, object: &mut Expr, index: &mut Expr) {
        walk_index_access(self, object, index);
    }

    fn visit_member_access(&mut self, target: &mut Expr, field: &mut Identifier) {
        walk_member_access(self, target, field);
    }

    fn visit_binary(&mut self, binary: &mut BinaryExpr) {
        walk_binary(self, binary);
    }

    fn visit_block(&mut self, block: &mut BlockExpr) {
        walk_block(self, block);
    }

    fn visit_call(&mut self, call: &mut CallExpr) {
        walk_call(self, call);
    }

    fn visit_type_annotation(&mut self, ty: &mut TypeAnnotation) {
        walk_visit_type_annotation(self, ty);
    }

    fn visit_parameter(&mut self, parameter: &mut Parameter) {
        walk_parameter(self, parameter);
    }
}

pub fn walk_statement<T: MutVisitor>(
    vis: &mut T,
    Stmt {
        id: _,
        kind,
        span: _,
        ty,
    }: &mut Stmt,
) {
    match kind {
        StmtKind::Expr(expr) => vis.visit_expression(expr),
        StmtKind::Let(let_stmt) => vis.visit_let_statement(let_stmt),
        StmtKind::Assignment(assignment) => vis.visit_assignment(assignment),
        StmtKind::If(if_stmt) => vis.visit_if_statement(if_stmt),
        StmtKind::Return(expr) => vis.visit_return(expr),
        StmtKind::FunctionDeclaration(function_declaration_stmt) => {
            vis.visit_function_declaration(function_declaration_stmt)
        }
        StmtKind::TypeDeclaration(type_declaration_stmt) => {
            vis.visit_type_declaration(type_declaration_stmt)
        }
    }
}

pub fn walk_ast<T: MutVisitor>(vis: &mut T, ast: &mut [Stmt]) {
    for stmt in ast {
        vis.visit_statement(stmt);
    }
}

pub fn walk_type_declaration<T: MutVisitor>(
    vis: &mut T,
    TypeDeclarationStmt {
        name: _,
        fields,
        fields_ordered: _,
        ty: _,
    }: &mut TypeDeclarationStmt,
) {
    for field in fields {
        vis.visit_type_annotation(field.1);
    }
}

pub fn walk_expression<T: MutVisitor>(vis: &mut T, Expr { id, kind, span, ty }: &mut Expr) {
    match kind {
        ExprKind::Literal(literal) => vis.visit_literal(literal),
        ExprKind::Identifier(identifier) => vis.visit_identifier(identifier),
        ExprKind::IndexAccess(array_index_expr) => todo!(),
        ExprKind::Binary(binary_expr) => vis.visit_binary(binary_expr),
        ExprKind::Unary(unary_expr) => todo!(),
        ExprKind::Block(block_expr) => vis.visit_block(block_expr),
        ExprKind::Call(call_expr) => vis.visit_call(call_expr),
        ExprKind::Closure(closure_expr) => todo!(),
        ExprKind::Instance(instanciate_type_exp) => todo!(),
        ExprKind::MemberAccess(member_access_expr) => todo!(),
    }
}

pub fn walk_let_statement<T: MutVisitor>(
    vis: &mut T,
    LetStmt {
        name: _,
        ty,
        initializer,
    }: &mut LetStmt,
) {
    if let Some(ty) = ty {
        vis.visit_type_annotation(ty);
    }
    vis.visit_expression(initializer);
}

pub fn walk_assignment<T: MutVisitor>(
    vis: &mut T,
    AssignmentStmt { target, value }: &mut AssignmentStmt,
) {
    match target {
        Assignable::Identifier(identifier) => vis.visit_identifier(identifier),
        Assignable::IndexAccess { object, index } => vis.visit_index_access(object, index),
        Assignable::MemberAcess { target, field } => vis.visit_member_access(target, field),
    }
}

pub fn walk_if_statement<T: MutVisitor>(
    vis: &mut T,
    IfStmt {
        condition,
        then_block,
        else_block,
    }: &mut IfStmt,
) {
    vis.visit_expression(condition);
    vis.visit_statement(then_block);

    if let Some(else_block) = else_block {
        vis.visit_statement(else_block);
    }
}

pub fn walk_return<T: MutVisitor>(vis: &mut T, ret: &mut Option<Expr>) {
    if let Some(expr) = ret {
        vis.visit_expression(expr);
    }
}

pub fn walk_function_declaration<T: MutVisitor>(
    vis: &mut T,
    FunctionDeclarationStmt {
        name: _,
        return_ty,
        parameters,
        body,
    }: &mut FunctionDeclarationStmt,
) {
    for parameter in parameters {
        vis.visit_parameter(parameter);
    }

    vis.visit_type_annotation(return_ty);

    vis.visit_expression(body);
}

pub fn walk_literal<T: MutVisitor>(vis: &mut T, literal: &mut Literal) {
    match literal {
        Literal::Number(_) => {}
        Literal::String(_) => {}
        Literal::Boolean(_) => {}
        Literal::Array(exprs) => {
            for expr in exprs {
                vis.visit_expression(expr);
            }
        }
    }
}

pub fn walk_identifier<T: MutVisitor>(_: &mut T, Identifier { name: _, span: _, resolved: _ }: &mut Identifier) {
}

pub fn walk_index_access<T: MutVisitor>(vis: &mut T, object: &mut Expr, index: &mut Expr) {
    vis.visit_expression(object);
    vis.visit_expression(index);
}

pub fn walk_member_access<T: MutVisitor>(vis: &mut T, target: &mut Expr, field: &mut Identifier) {
    vis.visit_expression(target);
    vis.visit_identifier(field);
}

pub fn walk_binary<T: MutVisitor>(
    vis: &mut T,
    BinaryExpr {
        operator: _,
        left,
        right,
    }: &mut BinaryExpr,
) {
    vis.visit_expression(left);
    vis.visit_expression(right);
}

pub fn walk_block<T: MutVisitor>(
    vis: &mut T,
    BlockExpr {
        statements,
        result,
        scope_id: _,
    }: &mut BlockExpr,
) {
    for statement in statements {
        vis.visit_statement(statement);
    }

    if let Some(result) = result {
        vis.visit_expression(result);
    }
}

pub fn walk_call<T: MutVisitor>(vis: &mut T, CallExpr { callee, arguments }: &mut CallExpr) {
    vis.visit_expression(callee);

    for arg in arguments {
        vis.visit_expression(arg);
    }
}

pub fn walk_visit_type_annotation<T: MutVisitor>(
    _: &mut T,
    TypeAnnotation {
        ty: _,
        expr: _,
        span: _,
    }: &mut TypeAnnotation,
) {
}

pub fn walk_parameter<T: MutVisitor>(
    vis: &mut T,
    Parameter {
        id: _,
        name,
        type_annotation,
        span: _,
    }: &mut Parameter,
) {
    vis.visit_identifier(name);
    vis.visit_type_annotation(type_annotation);
}
