pub mod backend;
pub mod common;
pub mod frontend;
pub mod middle;
pub mod utils;

pub use frontend::lexer::Lexer;
pub use frontend::parser::NewParser;
pub use frontend::scanner::Scanner;

pub use middle::type_system::TypeChecker;
pub use middle::types;

pub use backend::compiler::CompilationUnit;
pub use backend::vm::ExecutionContext;
pub use backend::vm::VirtualMachine;

pub use common::value::Value;
pub use common::error_reporter;
