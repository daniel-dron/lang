use std::{
    cell::RefCell,
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Not, Sub},
    rc::Rc,
};

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct RegisterId(pub usize);

/// Used at compile time only
/// Describes
#[derive(Debug, Default, Clone, PartialEq)]
pub struct UpvalueTarget {
    pub scope: usize, // how many call frames up is the target value
    pub idx: RegisterId,

    // debug
    pub func_name: String,
    pub value_name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Upvalue {
    Open(UpvalueTarget),
    Closed(Rc<RefCell<Value>>),
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub function_id: usize,
    pub upvalues: Vec<Upvalue>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Unitialized, // Unitialized values. This type is not accessible to the programmer. Its a compiler intrinsic
    // Primitive
    Number(f64),
    Boolean(bool),

    // Reference
    String(Rc<String>),
    Array(Rc<RefCell<Vec<Value>>>),
    Function(usize), // index into Prototype::functions
    Closure(Rc<Closure>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Function(l0), Self::Function(r0)) => l0 == r0,
            (Self::Closure(l0), Self::Closure(r0)) => Rc::ptr_eq(l0, r0),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0.partial_cmp(r0),
            (Self::Boolean(l0), Self::Boolean(r0)) => l0.partial_cmp(r0),
            (Self::String(l0), Self::String(r0)) => l0.partial_cmp(r0),

            _ => {
                todo!()
            }
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{}", *n as i64)
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Function(index) => write!(f, "@Function{{Unknown__{}}}>", index),
            Value::Closure(_) => todo!(),
            Value::Unitialized => todo!(),
            Value::Array(vec) => {
                write!(
                    f,
                    "[{}]",
                    vec.borrow()
                        .iter()
                        .map(|val| format!("{}", val))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}

impl Add for Value {
    type Output = Result<Value, String>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left + right)),
            (Value::String(left), Value::String(right)) => {
                Ok(Value::String(Rc::new(format!("{}{}", left, right))))
            }
            (Value::String(left), Value::Number(right)) => Ok(Value::String(Rc::new(format!(
                "{}{}",
                left,
                &right.to_string()
            )))),
            (Value::Number(left), Value::String(right)) => Ok(Value::String(Rc::new(format!(
                "{}{}",
                left.to_string(),
                right
            )))),
            (a, b) => Err(format!("Cannot add {:?} and {:?}", a, b)),
        }
    }
}

impl Sub for Value {
    type Output = Result<Value, String>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left - right)),
            (a, b) => Err(format!("Cannot add {:?} and {:?}", a, b)),
        }
    }
}

impl Mul for Value {
    type Output = Result<Value, String>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left * right)),
            (a, b) => Err(format!("Cannot add {:?} and {:?}", a, b)),
        }
    }
}

impl Div for Value {
    type Output = Result<Value, String>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left / right)),
            (a, b) => Err(format!("Cannot add {:?} and {:?}", a, b)),
        }
    }
}

impl Neg for Value {
    type Output = Result<Value, String>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(number) => Ok(Value::Number(-number)),
            a => Err(format!("Cannot negate {:?}", a)),
        }
    }
}

impl Not for Value {
    type Output = Result<Value, String>;

    fn not(self) -> Self::Output {
        match self {
            Value::Boolean(boolean) => Ok(Value::Boolean(!boolean)),
            a => Err(format!("Cannot not {:?}", a)),
        }
    }
}
