use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Float64,
    Boolean,
    String,

    NamedType(TypeDescriptor),
    Function(FunctionType),
    // Tuple(Vec<Type>),
    Array(Box<Type>),
    Never, // For expressions that never return
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDescriptor {
    pub name: String,
    pub fields: HashMap<String, Type>,
    pub fields_ordered: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub ret_ty: Box<Type>,
}

impl Type {
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
            Type::Never => "()".into(),
            Type::Array(ty) => format!("[{:?}]", *ty),
            Type::NamedType(type_descriptor) => todo!(),
        }
    }
}
