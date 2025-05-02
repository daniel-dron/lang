use crate::frontend::ast::TypeAnnotation;

use super::type_system::TypeError;

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
    pub fn from(value: &TypeAnnotation) -> Result<Self, TypeError> {
        match value {
            TypeAnnotation::Named(name, _) => match name.as_str() {
                "f64" => Ok(Type::Float64),
                "str" => Ok(Type::String),
                "bool" => Ok(Type::Boolean),
                _ => todo!(),
            },
            TypeAnnotation::Never => Ok(Type::Never),
            TypeAnnotation::Function(type_annotations, type_annotation) => {
                Ok(Type::Function(FunctionType {
                    parameters: type_annotations
                        .iter()
                        .map(|ty| Type::from(ty))
                        .collect::<Result<Vec<Type>, TypeError>>()?,
                    ret_ty: Box::new(Type::from(type_annotation)?),
                }))
            }
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Type::Float64 => "f64".into(),
            Type::Boolean => "bool".into(),
            Type::String => "str".into(),
            Type::Function(function_type) => {
                let formatted_types = if function_type.parameters.is_empty() {
                    String::new()
                } else {
                    let mut result = function_type.parameters[..function_type.parameters.len() - 1]
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", ");

                    if !function_type.parameters.is_empty() {
                        if !result.is_empty() {
                            result.push_str(", ");
                        }
                        result.push_str(&function_type.parameters.last().unwrap().to_string());
                    }

                    result
                };

                format!(
                    "fn({}) -> {}",
                    formatted_types,
                    function_type.ret_ty.to_string()
                )
            }
            Type::Never => "())".into(),
        }
    }
}
