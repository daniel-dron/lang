use std::collections::HashMap;

use crate::{
    ast::{
        BinaryOp, Expr, ExprKind, Literal, NodeId, Span, Stmt, StmtKind, TypeAnnotation, UnaryOp,
    },
    parser::ErrorType,
    types::{FunctionType, Type},
};

#[derive(Debug, Clone)]
pub struct TypeError {
    pub span: Span,
    pub ty: TypeErrorKind,
}

#[derive(Debug, Clone)]
pub enum Expected {
    Span(Span),
    Named(String),
}

#[derive(Debug, Clone)]
pub enum TypeErrorKind {
    MissmatchedTypes {
        expected: Expected,
        got_ty: Type,
        got_span: Span,
    },
    TODO(String),
}

/// The type checker not only enforces type correctness but also infers types
#[derive(Debug)]
pub struct TypeChecker {
    // For expressions (based on NodeId)
    expr_types: HashMap<NodeId, Type>,

    // For variables in each scope
    variable_types: HashMap<String, Type>,

    // For functions
    function_types: HashMap<String, FunctionType>,

    errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            expr_types: HashMap::new(),
            variable_types: HashMap::new(),
            function_types: HashMap::new(),
            errors: vec![],
        }
    }

    pub fn register_native(&mut self, name: String, ty: FunctionType) {
        self.function_types.insert(name, ty);
    }

    pub fn infer(&mut self, mut ast: Vec<Stmt>) -> Result<Vec<Stmt>, Vec<TypeError>> {
        // recursively look through statements and expressions and infer type and propagate it up
        // similar to compiler architecture. its depth first

        for stmt in &mut ast {
            if let Err(err) = self.infer_statement(stmt) {
                self.errors.push(err);
            } else {
                continue;
            }
        }

        if self.errors.is_empty() {
            Ok(ast)
        } else {
            Err(self.errors.clone())
        }
    }

    fn infer_statement(&mut self, statment: &mut Stmt) -> Result<Option<Type>, TypeError> {
        match &mut statment.kind {
            StmtKind::Expr(expr) => {
                let ty = self.infer_expression(expr)?;

                // only block expressions may return something
                // stuff like `10 + 10;` does not
                if let ExprKind::Block(_) = &expr.kind {
                    Ok(Some(ty))
                } else {
                    Ok(None)
                }
            }
            StmtKind::Let(let_stmt) => {
                let initializer_type = self.infer_expression(&mut let_stmt.initializer)?;

                // if the declaration was type annotated, we must check if it was a valid type,
                // and if the initializer type matches with it
                if let Some(ty_annotation) = &let_stmt.ty {
                    let ty_annotation = match Type::from(ty_annotation) {
                        Ok(ty) => ty,
                        Err(e) => {
                            return Err(TypeError {
                                span: statment.span.clone(),
                                ty: TypeErrorKind::TODO(format!("Let Type error ({:?})", e)),
                            });
                        }
                    };

                    if initializer_type != ty_annotation {
                        if let TypeAnnotation::Named(_, span) = let_stmt.ty.clone().unwrap() {
                            return Err(TypeError {
                                span: statment.span.clone(),
                                ty: TypeErrorKind::MissmatchedTypes {
                                    expected: Expected::Span(span.clone()),
                                    got_ty: initializer_type,
                                    got_span: let_stmt.initializer.span.clone(),
                                },
                            });
                        }
                    }
                }

                self.variable_types
                    .insert(let_stmt.name.clone(), initializer_type.clone());

                Ok(None)
            }
            StmtKind::Assignment(name, expr) => {
                let ty = match self.variable_types.get(name) {
                    Some(ty) => Ok(ty.clone()),
                    None => Err(TypeError {
                        span: statment.span.clone(),
                        ty: TypeErrorKind::TODO("Statement Type Error".into()),
                    }),
                }?;

                let right_ty = self.infer_expression(expr)?;

                if ty != right_ty {
                    return Err(TypeError {
                        span: expr.span.clone(),
                        ty: TypeErrorKind::MissmatchedTypes {
                            expected: Expected::Named(ty.to_string().into()),
                            got_ty: right_ty,
                            got_span: expr.span.clone(),
                        },
                    });
                }

                Ok(None)
            }
            StmtKind::If(if_stmt) => {
                let _ = self.infer_expression(&mut if_stmt.condition)?;

                // we must confirm that both statements collapse to the same type
                let then_ty = self.infer_statement(&mut if_stmt.then_block)?;
                if let Some(else_block) = &mut if_stmt.else_block {
                    let else_ty = self.infer_statement(else_block)?;
                    if then_ty != else_ty {
                        return Err(TypeError {
                            span: statment.span.clone(),
                            ty: TypeErrorKind::TODO("If Statement Error".into()),
                        });
                    }
                }

                Ok(then_ty)
            }
            StmtKind::Return(expr) => {
                if let Some(expr) = expr {
                    Ok(Some(self.infer_expression(expr)?))
                } else {
                    Ok(Some(Type::Never))
                }
            }
            StmtKind::FunctionDeclaration(function_declaration_stmt) => {
                let parameters = function_declaration_stmt
                    .parameters
                    .iter()
                    .map(|param| Type::from(&param.type_annotation))
                    .collect::<Result<Vec<Type>, TypeError>>()?;

                // register param types first
                for (param, ty) in function_declaration_stmt.parameters.iter().zip(&parameters) {
                    self.variable_types.insert(param.name.clone(), ty.clone());
                }

                let expected_ty = if let Some(ty) = &function_declaration_stmt.return_ty {
                    Type::from(ty).map_err(|err| TypeError {
                        span: statment.span.clone(),
                        ty: TypeErrorKind::TODO(format!("Function Declaration Err 1 ({:#?})", err)),
                    })?
                } else {
                    Type::Never
                };

                // must register the function type first. must be available for recursion
                self.function_types.insert(
                    function_declaration_stmt.name.clone(),
                    FunctionType {
                        parameters,
                        ret_ty: Box::new(expected_ty.clone()),
                    },
                );

                // parse the body
                let ret_ty = self.infer_expression(&mut function_declaration_stmt.body)?;

                if ret_ty != expected_ty {
                    return Err(TypeError {
                        span: statment.span.clone(),
                        ty: TypeErrorKind::TODO("Function Declaration Err 2".into()),
                    });
                }

                Ok(None)
            }
        }
    }

    pub fn infer_expression(&mut self, expression: &mut Expr) -> Result<Type, TypeError> {
        match &mut expression.kind {
            ExprKind::Literal(literal) => match literal {
                Literal::Number(_) => Ok(Type::Float64),
                Literal::String(_) => Ok(Type::String),
                Literal::Boolean(_) => Ok(Type::Boolean),
            },
            ExprKind::Identifier(identifier) => {
                if let Some(ty) = self.variable_types.get(identifier) {
                    return Ok(ty.clone());
                }

                if let Some(ty) = self.function_types.get(identifier) {
                    return Ok(Type::Function(ty.clone()));
                }
                Err(TypeError {
                    span: expression.span.clone(),
                    ty: TypeErrorKind::TODO("Identifier Type Error".into()),
                })
            }
            ExprKind::Binary(binary_expr) => {
                let left_type = self.infer_expression(&mut binary_expr.left)?;
                let right_type = self.infer_expression(&mut binary_expr.right)?;

                let result_type = match binary_expr.operator {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        // we allow any concatenation with string
                        if binary_expr.operator == BinaryOp::Add
                            && (left_type == Type::String || right_type == Type::String)
                        {
                            return Ok(Type::String);
                        }

                        // we can only perform arithmetic operations on numbers (except the string concatenation above)
                        if matches!(left_type, Type::Float64) && left_type == right_type {
                            Ok(left_type)
                        } else {
                            return Err(TypeError {
                                span: expression.span.clone(),
                                ty: TypeErrorKind::TODO("Binary Type Err 1".into()),
                            });
                        }
                    }
                    BinaryOp::Equal => {
                        if left_type == right_type {
                            Ok(Type::Boolean)
                        } else {
                            Err(TypeError {
                                span: expression.span.clone(),
                                ty: TypeErrorKind::TODO("Binary Type Err 2".into()),
                            })
                        }
                    }
                    BinaryOp::Greater
                    | BinaryOp::Lesser
                    | BinaryOp::GreaterOrEqual
                    | BinaryOp::LessesOrEqual => {
                        if left_type == Type::Float64 && right_type == Type::Float64 {
                            Ok(Type::Boolean)
                        } else {
                            Err(TypeError {
                                span: expression.span.clone(),
                                ty: TypeErrorKind::TODO("Binary Type Err 3".into()),
                            })
                        }
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        if left_type == Type::Float64 && right_type == Type::Float64 {
                            Ok(Type::Boolean)
                        } else {
                            Err(TypeError {
                                span: expression.span.clone(),
                                ty: TypeErrorKind::TODO("Binary Type Err 4".into()),
                            })
                        }
                    }
                }?;

                Ok(result_type)
            }
            ExprKind::Unary(unary_expr) => {
                let operand_type = self.infer_expression(&mut unary_expr.operand)?;
                match unary_expr.operator {
                    UnaryOp::Negate | UnaryOp::Not => {
                        if operand_type == Type::Boolean {
                            Ok(Type::Boolean)
                        } else {
                            Err(TypeError {
                                span: expression.span.clone(),
                                ty: TypeErrorKind::TODO("Unary Type Err".into()),
                            })
                        }
                    }
                }
            }
            ExprKind::Block(block_expr) => {
                let mut ret_types = vec![];

                for stmt in &mut block_expr.statements {
                    match self.infer_statement(stmt) {
                        Ok(ty) => {
                            if let Some(ty) = ty {
                                ret_types.push(ty);
                            }
                        }
                        Err(err) => self.errors.push(err),
                    }
                }

                // check if all return types are the same
                if ret_types.is_empty() {
                    return Ok(Type::Never);
                }

                let all_equal = {
                    let first = &ret_types[0];
                    ret_types.iter().all(|item| item == first)
                };

                if !all_equal {
                    return Err(TypeError {
                        span: expression.span.clone(),
                        ty: TypeErrorKind::TODO("Block Type Err".into()),
                    });
                }

                return Ok(ret_types[0].clone());
            }
            ExprKind::Call(call_expr) => match self.infer_expression(&mut call_expr.callee)? {
                Type::Float64 | Type::Boolean | Type::String | Type::Never => Err(TypeError {
                    span: expression.span.clone(),
                    ty: TypeErrorKind::TODO("Invalid Call Err".into()),
                }),
                Type::Function(function_type) => {
                    // LITTLE HACK FOR NOW since we dont support variadic parameter count
                    if let ExprKind::Identifier(name) = &call_expr.callee.kind {
                        if name == "print" {
                            return Ok(*function_type.ret_ty.clone());
                        }
                    }

                    // check parameter types
                    if function_type.parameters.len() != call_expr.arguments.len() {
                        Err(TypeError {
                            span: expression.span.clone(),
                            ty: TypeErrorKind::TODO("Function Call Type Err 1".into()),
                        })
                    } else {
                        for (expr, ty) in
                            call_expr.arguments.iter_mut().zip(function_type.parameters)
                        {
                            match self.infer_expression(expr) {
                                Ok(expr_ty) => {
                                    if expr_ty != ty {
                                        return Err(TypeError {
                                            span: expression.span.clone(),
                                            ty: TypeErrorKind::TODO(
                                                "Function Call Type Err 2".into(),
                                            ),
                                        });
                                    }
                                }
                                Err(err) => self.errors.push(err),
                            }
                        }

                        Ok(*function_type.ret_ty.clone())
                    }
                }
            },
            ExprKind::Closure(closure_expr) => {
                // parse the body
                let ret_ty = self.infer_expression(&mut closure_expr.body)?;

                if let Some(ty) = &closure_expr.return_type {
                    let expected_ty = Type::from(ty)?;

                    if ret_ty != expected_ty {
                        return Err(TypeError {
                            span: expression.span.clone(),
                            ty: TypeErrorKind::TODO("Closure Err 1".into()),
                        });
                    }
                }

                let parameters = closure_expr
                    .parameters
                    .iter()
                    .map(|param| Type::from(&param.type_annotation))
                    .collect::<Result<Vec<Type>, TypeError>>()?;

                // register param types first
                for (param, ty) in closure_expr.parameters.iter().zip(&parameters) {
                    self.variable_types.insert(param.name.clone(), ty.clone());
                }

                Ok(Type::Function(FunctionType {
                    parameters,
                    ret_ty: Box::new(ret_ty),
                }))
            }
        }
    }
}
