use std::collections::HashMap;

use crate::ast::{ExprKind, StmtKind, TypeAnnotation};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Float64,
    Boolean,
    String,

    Function(FunctionType),
    // Tuple(Vec<Type>),
    // Array(Box<Type>, Option<usize>),
    Never, // For expressions that never return
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub ret_ty: Box<Type>,
}

impl Type {
    pub fn from(value: &TypeAnnotation) -> Option<Self> {
        match value {
            TypeAnnotation::Named(name, _) => match name.as_str() {
                "f64" => Some(Type::Float64),
                "str" => Some(Type::String),
                "bool" => Some(Type::String),
                _ => todo!(),
            },
            TypeAnnotation::Never => Some(Type::Never),
        }
    }

    pub fn to_string(&self) -> &'static str {
        match self {
            Type::Float64 => "f64",
            Type::Boolean => "bool",
            Type::String => "str",
            Type::Function(function_type) => "func",
            Type::Never => "())",
        }
    }
}
