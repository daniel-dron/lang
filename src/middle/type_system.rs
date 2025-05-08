use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::frontend::{
    ast::{
        Assignable, AssignmentStmt, BinaryOp, Expr, ExprKind, Literal, Span, Stmt, StmtKind,
        TypeAnnotation, TypeExpr, UnaryOp,
    },
    resolve::{NameResolver, Scope, ScopeId, SymbolId},
};

use super::types::{FunctionType, Type, TypeDescriptor};

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
pub struct TypeChecker<'nr> {
    types: HashMap<SymbolId, Type>,
    name_solver: &'nr mut NameResolver,
    current_scope: Option<Rc<RefCell<Scope>>>,
    errors: Vec<TypeError>,
}

impl<'nr> TypeChecker<'nr> {
    pub fn new(name_solver: &'nr mut NameResolver) -> TypeChecker<'nr> {
        let mut tc = Self {
            types: HashMap::new(),
            name_solver,
            errors: vec![],
            current_scope: None,
        };

        // set global scope
        tc.current_scope = Some(tc.name_solver.scope());

        tc
    }

    pub fn register_native(&mut self, name: String, ty: FunctionType) {
        println!("[TYPE] Looking for: {}", name);
        let symbol_id = self
            .current_scope
            .as_ref()
            .unwrap()
            .borrow_mut()
            .lookup_symbol(&name)
            .unwrap();
        self.types.insert(symbol_id, Type::Function(ty));
    }

    fn push_scope(&mut self, scope_id: &ScopeId) {
        println!("[TYPE] Entering scope: {:?}", scope_id);
        self.current_scope = self.name_solver.scopes.get(scope_id).cloned();
    }

    fn pop_scope(&mut self) {
        println!(
            "[TYPE] Leaving scope: {:?}",
            self.current_scope.as_ref().unwrap().borrow().id
        );
        let parent = self.current_scope.as_ref().unwrap().borrow().parent.clone();
        self.current_scope = parent;
    }

    fn lookup_symbol(&self, name: String) -> Option<Type> {
        println!("[TYPE] Looking for symbol: {}", name);
        let symbol_id = self
            .current_scope
            .as_ref()
            .unwrap()
            .borrow_mut()
            .lookup_symbol(&name)
            .unwrap();
        println!("  [TYPE] got: {:?}", symbol_id);

        self.types.get(&symbol_id).cloned()
    }

    fn register_symbol(&mut self, name: String, ty: Type) {
        println!("[TYPE] Looking for symbol: {}", name);
        let symbol_id = self
            .current_scope
            .as_ref()
            .unwrap()
            .borrow_mut()
            .lookup_symbol(&name)
            .unwrap();
        println!("  [TYPE] got: {:?}", symbol_id);

        self.types.insert(symbol_id, ty);
    }

    fn register_type(&mut self, name: String, ty: TypeDescriptor) {
        let symbol_id = self
            .current_scope
            .as_ref()
            .unwrap()
            .borrow_mut()
            .lookup_symbol(&name)
            .unwrap();
        self.types.insert(symbol_id, Type::NamedType(ty));
    }

    fn look_up_type(&mut self, name: &String) -> Option<TypeDescriptor> {
        let symbol_id = self
            .current_scope
            .as_ref()
            .unwrap()
            .borrow_mut()
            .lookup_symbol(&name)
            .unwrap();

        if let Some(ty) = self.types.get(&symbol_id) {
            if let Type::NamedType(type_descriptor) = ty {
                Some(type_descriptor.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn apply_type(&mut self, type_annotation: &mut TypeAnnotation) -> Result<(), TypeError> {
        let ty = self.resolve_type_expr(&type_annotation.expr, &type_annotation.span)?;
        type_annotation.ty = ty;
        Ok(())
    }

    pub fn resolve_type_expr(
        &mut self,
        ty_expr: &TypeExpr,
        span: &Span,
    ) -> Result<Type, TypeError> {
        match ty_expr {
            TypeExpr::Named(name) => {
                // Check if it's a built-in type
                match name.as_str() {
                    "f64" => Ok(Type::Float64),
                    "str" => Ok(Type::String),
                    "bool" => Ok(Type::Boolean),
                    _ => {
                        // check if its a named type
                        if let Some(ty) = self.look_up_type(name) {
                            Ok(Type::NamedType(ty.clone()))
                        } else {
                            Err(TypeError {
                                span: span.clone(),
                                ty: TypeErrorKind::TODO(format!("Unknown type: {}", name)),
                            })
                        }
                    }
                }
            }
            TypeExpr::Function {
                parameters,
                return_ty,
            } => {
                let param_types = parameters
                    .iter()
                    .map(|param| self.resolve_type_expr(param, span))
                    .collect::<Result<Vec<Type>, TypeError>>()?;

                let ret_ty = self.resolve_type_expr(return_ty, span)?;

                Ok(Type::Function(FunctionType {
                    parameters: param_types,
                    ret_ty: Box::new(ret_ty),
                }))
            }
            TypeExpr::Array(elem_type) => {
                let ty = self.resolve_type_expr(elem_type, span)?;
                Ok(Type::Array(Box::new(ty)))
            }
            TypeExpr::Unspecified => Ok(Type::Never),
        }
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
                    statment.ty = ty.clone();
                    Ok(Some(ty))
                } else {
                    Ok(None)
                }
            }
            StmtKind::Let(let_stmt) => {
                /*
                 *  Variable declarations. Can be type annotated or not, depending on the
                 *  initializer. Cannot declare a variable with an initializer that does not return
                 *  any type (Type::Never).
                 *
                 *  Case 1. Deduced
                 *  let x = 10;
                 *  In this case, the type is deduced from the initalizer and attached to `x`
                 *
                 *  Case 2. Annotated
                 *  let x: f64 = 10;
                 *  Since `x` is type annotated with `f64` we must check if it matches with the
                 *  type infered from the initializer expression.
                 *
                 *  Case 3. Arrays
                 *  For arrays there can be two cases. If you give it an non-empty array literal,
                 *  all value types must be the same, and the type of `x` can then be infered from
                 *  the array literal as `[type]`.
                 *  let x = [1, 2, 3, 4]; // x: [f64]
                 *
                 *  If the initializer is an empty array literal `[]`, then the declaration must be
                 *  type annotated, as it's impossible to know what the array type is.
                 *  TODO: later on, we can lift this limitation by looking for an access to the
                 *  array and infer the type from there
                 *  let x = []; // error, what type is `[]`?
                 *
                 */
                let mut initializer_type = self.infer_expression(&mut let_stmt.initializer)?;

                // the right side will never return anything, we can´t assign this value
                if initializer_type == Type::Never {
                    return Err(TypeError {
                        span: statment.span.clone(),
                        ty: TypeErrorKind::MissmatchedTypes {
                            expected: Expected::Named("Any".into()),
                            got_ty: initializer_type,
                            got_span: let_stmt.initializer.span.clone(),
                        },
                    });
                }

                match &mut let_stmt.ty {
                    Some(type_annotation) => {
                        self.apply_type(type_annotation)?;

                        // we allow empty arrays, but they are of type [Never], so must be patched
                        if let (Type::Array(init_ty), Type::Array(_)) =
                            (&initializer_type, &type_annotation.ty)
                        {
                            if **init_ty == Type::Never {
                                initializer_type = type_annotation.ty.clone();
                            }
                        }

                        if type_annotation.ty != initializer_type {
                            return Err(TypeError {
                                span: statment.span.clone(),
                                ty: TypeErrorKind::MissmatchedTypes {
                                    expected: Expected::Span(type_annotation.span.clone()),
                                    got_ty: initializer_type,
                                    got_span: let_stmt.initializer.span.clone(),
                                },
                            });
                        }
                    }
                    None => {
                        // if there was no type annotation, we must confirm that the initializer is
                        // not an empty array
                        if let Type::Array(array_ty) = &initializer_type {
                            if **array_ty == Type::Never {
                                return Err(TypeError {
                                    span: statment.span.clone(),
                                    ty: TypeErrorKind::TODO(
                                        "Invalid empty array on untyped variable".into(),
                                    ),
                                });
                            }
                        }
                    }
                };

                self.register_symbol(let_stmt.name.clone(), initializer_type);

                Ok(None)
            }
            StmtKind::Assignment(assignment) => {
                let AssignmentStmt { target, value } = &mut **assignment;
                println!("Assignment: {:#?} {:#?}", target, value);

                // infer the type of the target
                let ty = match target {
                    Assignable::Identifier(target) => match self.lookup_symbol(target.name.clone())
                    {
                        Some(ty) => Ok(ty.clone()),
                        None => Err(TypeError {
                            span: statment.span.clone(),
                            ty: TypeErrorKind::TODO("Statement Type Error".into()),
                        }),
                    }?,
                    Assignable::IndexAccess { object, index } => {
                        let left = self.infer_expression(object)?;
                        let right = self.infer_expression(index)?;

                        if let Type::Array(ty) = left {
                            if let Type::Float64 = right {
                                Ok(*ty)
                            } else {
                                Err(TypeError {
                                    span: statment.span.clone(),
                                    ty: TypeErrorKind::MissmatchedTypes {
                                        expected: Expected::Named(Type::Float64.to_string()),
                                        got_ty: right,
                                        got_span: value.span.clone(),
                                    },
                                })
                            }
                        } else {
                            Err(TypeError {
                                span: statment.span.clone(),
                                ty: TypeErrorKind::MissmatchedTypes {
                                    expected: Expected::Named(left.to_string().into()),
                                    got_ty: right,
                                    got_span: value.span.clone(),
                                },
                            })
                        }
                    }?,
                    Assignable::MemberAcess { target, field } => {
                        let ty = self.infer_expression(target)?;
                        if let Type::NamedType(ty) = &ty {
                            // check if type has member name
                            if ty.fields.contains_key(&field.name) {
                                let field_type = ty.fields.get(&field.name).unwrap();

                                field_type.clone()
                            } else {
                                return Err(TypeError {
                                    span: target.span.clone(),
                                    ty: TypeErrorKind::TODO(format!(
                                        "{:?} is not a named type",
                                        ty
                                    )),
                                });
                            }
                        } else {
                            return Err(TypeError {
                                span: target.span.clone(),
                                ty: TypeErrorKind::TODO(format!("{:?} is not a named type", ty)),
                            });
                        }
                    }
                };

                let right_ty = self.infer_expression(value)?;

                // the right side will never return anything, we can´t assign this value
                if right_ty == Type::Never {
                    return Err(TypeError {
                        span: statment.span.clone(),
                        ty: TypeErrorKind::MissmatchedTypes {
                            expected: Expected::Named("Any".into()),
                            got_ty: right_ty,
                            got_span: value.span.clone(),
                        },
                    });
                }

                if ty != right_ty {
                    return Err(TypeError {
                        span: value.span.clone(),
                        ty: TypeErrorKind::MissmatchedTypes {
                            expected: Expected::Named(ty.to_string().into()),
                            got_ty: right_ty,
                            got_span: value.span.clone(),
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

                if let Some(ty) = &then_ty {
                    statment.ty = ty.clone();
                }

                Ok(then_ty)
            }
            StmtKind::Return(expr) => {
                if let Some(expr) = expr {
                    let ty = self.infer_expression(expr)?;

                    statment.ty = ty.clone();
                    Ok(Some(ty))
                } else {
                    Ok(Some(Type::Never))
                }
            }
            StmtKind::FunctionDeclaration(function_declaration_stmt) => {
                self.apply_type(&mut function_declaration_stmt.return_ty)?;

                for param in &mut function_declaration_stmt.parameters {
                    self.apply_type(&mut param.type_annotation)?;
                }

                let parameters = function_declaration_stmt
                    .parameters
                    .iter()
                    .map(|param| param.type_annotation.ty.clone())
                    .collect::<Vec<Type>>();

                let expected_ty = &function_declaration_stmt.return_ty.ty;

                // must register the function type first. must be available for recursion
                self.register_symbol(
                    function_declaration_stmt.name.clone(),
                    Type::Function(FunctionType {
                        parameters: parameters.clone(),
                        ret_ty: Box::new(expected_ty.clone()),
                    }),
                );

                println!("STMT: {:#?}", function_declaration_stmt);

                if let ExprKind::Block(block) = &function_declaration_stmt.body.kind {
                    // temporarely enter the block scope
                    println!(
                        "[TYPE] func (PARAMS SCOPE): {:#?}",
                        function_declaration_stmt.name
                    );
                    self.push_scope(&block.scope_id);
                    // register param types first
                    for (param, ty) in function_declaration_stmt.parameters.iter().zip(&parameters)
                    {
                        self.register_symbol(param.name.name.clone(), ty.clone());
                    }
                    self.pop_scope();

                    // parse the body
                    let ret_ty = self.infer_expression(&mut function_declaration_stmt.body)?;

                    if ret_ty != *expected_ty {
                        return Err(TypeError {
                            span: statment.span.clone(),
                            ty: TypeErrorKind::TODO("Function Declaration Err 2".into()),
                        });
                    }
                }

                Ok(None)
            }
            StmtKind::TypeDeclaration(type_declaration_stmt) => {
                for (_, type_annotation) in &mut type_declaration_stmt.fields {
                    self.apply_type(type_annotation)?;
                }

                let fields = type_declaration_stmt
                    .fields
                    .clone()
                    .into_iter()
                    .map(|(name, type_annotation)| (name, type_annotation.ty))
                    .collect();

                let ty = TypeDescriptor {
                    name: type_declaration_stmt.name.clone(),
                    fields,
                    fields_ordered: type_declaration_stmt.fields_ordered.clone(),
                };

                type_declaration_stmt.ty = Type::NamedType(ty.clone());
                self.register_type(type_declaration_stmt.name.clone(), ty);

                Ok(None)
            }
        }
    }

    pub fn infer_expression(&mut self, expression: &mut Expr) -> Result<Type, TypeError> {
        let ty = match &mut expression.kind {
            ExprKind::Literal(literal) => match literal {
                Literal::Number(_) => Ok(Type::Float64),
                Literal::String(_) => Ok(Type::String),
                Literal::Boolean(_) => Ok(Type::Boolean),
                Literal::Array(exprs) => {
                    if exprs.is_empty() {
                        // Empty array, we'll infer the type later based on assignment context
                        Ok(Type::Array(Box::new(Type::Never)))
                    } else {
                        let first_type = self.infer_expression(&mut exprs[0])?;

                        // Check all other elements have the same type
                        for (_, expr) in exprs.iter_mut().enumerate().skip(1) {
                            let element_type = self.infer_expression(expr)?;
                            if element_type != first_type {
                                return Err(TypeError {
                                    span: expr.span.clone(),
                                    ty: TypeErrorKind::TODO(
                                        "Array elements not all of the same type".into(),
                                    ),
                                });
                            }
                        }

                        Ok(Type::Array(Box::new(first_type)))
                    }
                }
            },
            ExprKind::Identifier(identifier) => match self.lookup_symbol(identifier.name.clone()) {
                Some(ty) => Ok(ty),
                None => Err(TypeError {
                    span: expression.span.clone(),
                    ty: TypeErrorKind::TODO("Identifier Type Error".into()),
                }),
            },
            ExprKind::Binary(binary_expr) => {
                let left_type = self.infer_expression(&mut binary_expr.left)?;
                let right_type = self.infer_expression(&mut binary_expr.right)?;

                let result_type = match binary_expr.operator {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        // we allow any concatenation with string
                        if binary_expr.operator == BinaryOp::Add
                            && (left_type == Type::String || right_type == Type::String)
                        {
                            expression.ty = Type::String;
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
                self.push_scope(&block_expr.scope_id);

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
                    self.pop_scope();
                    return Ok(Type::Never);
                }

                let all_equal = {
                    let first = &ret_types[0];
                    ret_types.iter().all(|item| item == first)
                };

                if !all_equal {
                    self.pop_scope();
                    return Err(TypeError {
                        span: expression.span.clone(),
                        ty: TypeErrorKind::TODO("Block Type Err".into()),
                    });
                }

                self.pop_scope();
                Ok(ret_types[0].clone())
            }
            ExprKind::Call(call_expr) => match self.infer_expression(&mut call_expr.callee)? {
                Type::Float64
                | Type::Boolean
                | Type::String
                | Type::Array(_)
                | Type::Never
                | Type::NamedType(_) => Err(TypeError {
                    span: expression.span.clone(),
                    ty: TypeErrorKind::TODO("Invalid Call Err".into()),
                }),
                Type::Function(function_type) => {
                    // LITTLE HACK FOR NOW since we dont support variadic parameter count
                    if let ExprKind::Identifier(name) = &call_expr.callee.kind {
                        if name.name == "print" {
                            expression.ty = *function_type.ret_ty.clone();
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

                let parameters = closure_expr
                    .parameters
                    .iter()
                    .map(|param| param.type_annotation.ty.clone())
                    .collect::<Vec<Type>>();

                // register param types first
                for (param, ty) in closure_expr.parameters.iter().zip(&parameters) {
                    self.register_symbol(param.name.name.clone(), ty.clone());
                }

                Ok(Type::Function(FunctionType {
                    parameters,
                    ret_ty: Box::new(ret_ty),
                }))
            }
            ExprKind::IndexAccess(array_index_expr) => {
                let target_ty = self.infer_expression(&mut array_index_expr.target)?;
                let index_ty = self.infer_expression(&mut array_index_expr.index)?;

                match target_ty {
                    Type::Array(array) => {
                        // check if index is a number
                        if let Type::Float64 = index_ty {
                            Ok(*array.clone())
                        } else {
                            todo!()
                        }
                    }
                    _ => todo!(),
                }
            }
            ExprKind::Instance(instanciate_type_exp) => {
                // get instance type
                if let Some(ty) = self.look_up_type(&instanciate_type_exp.name) {
                    // make sure types match
                    let required_fields = &ty.fields;

                    for (field_name, _) in required_fields {
                        if !instanciate_type_exp.fields.contains_key(field_name) {
                            return Err(TypeError {
                                span: expression.span.clone(),
                                ty: TypeErrorKind::TODO(format!(
                                    "Missing field '{}' for type '{}'",
                                    field_name, instanciate_type_exp.name
                                )),
                            });
                        }
                    }

                    for field_name in instanciate_type_exp.fields.keys() {
                        if !required_fields.contains_key(field_name) {
                            return Err(TypeError {
                                span: expression.span.clone(),
                                ty: TypeErrorKind::TODO(format!(
                                    "Unknown field '{}' for type '{}'",
                                    field_name, instanciate_type_exp.name
                                )),
                            });
                        }
                    }

                    for (field_name, field_expr) in &mut instanciate_type_exp.fields {
                        let expected_type = required_fields.get(field_name).unwrap();
                        let actual_type = self.infer_expression(field_expr)?;

                        if &actual_type != expected_type {
                            return Err(TypeError {
                                span: field_expr.span.clone(),
                                ty: TypeErrorKind::MissmatchedTypes {
                                    expected: Expected::Named(expected_type.to_string()),
                                    got_ty: actual_type,
                                    got_span: field_expr.span.clone(),
                                },
                            });
                        }
                    }

                    expression.ty = Type::NamedType(ty.clone());

                    Ok(expression.ty.clone())
                } else {
                    Ok(Type::Never)
                }
            }
            ExprKind::MemberAccess(member_access_expr) => {
                let ty = self.infer_expression(&mut member_access_expr.target)?;
                if let Type::NamedType(ty) = &ty {
                    // check if type has member name
                    if ty.fields.contains_key(&member_access_expr.name) {
                        let field_type = ty.fields.get(&member_access_expr.name).unwrap();

                        // get id
                        let id = ty
                            .fields_ordered
                            .iter()
                            .position(|item| *item == member_access_expr.name)
                            .unwrap();

                        member_access_expr.id = id;

                        Ok(field_type.clone())
                    } else {
                        return Err(TypeError {
                            span: expression.span.clone(),
                            ty: TypeErrorKind::TODO(format!("{:?} is not a named type", ty)),
                        });
                    }
                } else {
                    return Err(TypeError {
                        span: expression.span.clone(),
                        ty: TypeErrorKind::TODO(format!("{:?} is not a named type", ty)),
                    });
                }
            }
        };

        expression.ty = ty.clone()?;
        return ty;
    }
}
