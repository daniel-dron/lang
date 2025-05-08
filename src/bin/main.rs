use core::error;
use std::{
    env, fs, process,
    time::{SystemTime, UNIX_EPOCH},
};

use lang_core::{
    CompilationUnit, ExecutionContext, Lexer, NewParser, Scanner, TypeChecker, Value,
    VirtualMachine,
    error_reporter::report,
    frontend::{mut_visitor::MutVisitor, resolve::NameResolver},
    types::{FunctionType, Type},
};

fn print(context: &ExecutionContext, params: Vec<Value>) -> Result<Value, String> {
    print!("EXECUTION: ");
    for value in params {
        match value {
            Value::Function(function) => {
                if let Some(function) = context.unit.functions.get(function) {
                    print!(
                        "@Function{{{}({})}}",
                        function.name,
                        function
                            .parameters
                            .iter()
                            .map(|value| value.0.clone())
                            .collect::<Vec<String>>()
                            .join(", ")
                    );
                } else {
                    print!("@Function{{Unknown {}}}", function);
                }
            }
            _ => print!("{}", value),
        }
    }

    println!();

    return Ok(Value::Number(0.0));
}

fn assert_vm(context: &ExecutionContext, params: Vec<Value>) -> Result<Value, String> {
    let _ = context;
    // expected params
    // arg1: boolean
    // arg2: expected message string
    if let (Value::Boolean(b), Value::String(message)) = (&params[0], &params[1]) {
        if *b {
            return Ok(Value::Boolean(true));
        } else {
            Err(message.to_string())
        }
    } else {
        panic!("Wrong parameters on assert call: {:?}", params);
    }
}

fn time_vm(_context: &ExecutionContext, _params: Vec<Value>) -> Result<Value, String> {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();

    let millis = now.as_millis() as f64;
    Ok(Value::Number(millis))
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Error: No file path provided");
        eprintln!("Usage: {} <file_path>", args[0]);
        process::exit(1);
    }

    let file_path = &args[1];

    let source = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(error) => {
            eprintln!("Error reading file '{}': {}", file_path, error);
            process::exit(1);
        }
    };

    let scanner = Scanner::new(&source);
    let lexer = Lexer::from(scanner);

    let tokens = lexer.iter().cloned().collect::<Vec<_>>();

    let mut parser = NewParser::new(&tokens);
    let ast = parser.generate_ast();

    println!("{}", source);

    match ast {
        Ok(mut v) => {
            let mut name_resolver = NameResolver::new();
            name_resolver.register_function("print");
            name_resolver.register_function("assert");
            if let Err(errors) = name_resolver.solve(v.as_mut_slice()) {
                println!("Failed name resolution: {:#?}", errors);
            }
            // println!("NEW AST: {:#?}", v);

            let mut type_checker = TypeChecker::new(&mut name_resolver);
            type_checker.register_native(
                "print".into(),
                FunctionType {
                    parameters: vec![],
                    ret_ty: Box::new(Type::Never),
                },
            );
            type_checker.register_native(
                "assert".into(),
                FunctionType {
                    parameters: vec![Type::Boolean, Type::String],
                    ret_ty: Box::new(Type::Never),
                },
            );
            let new_ast = type_checker.infer(v);

            match new_ast {
                Ok(stmts) => {
                    let unit = CompilationUnit::new(&stmts.as_slice());

                    let mut vm = VirtualMachine::new();
                    vm.register_native("print".into(), print);
                    vm.register_native("assert", assert_vm);
                    vm.register_native("time", time_vm);

                    // initialize global context
                    let mut execution_context = vm.create_context(&unit);

                    // run global init
                    let _ = vm.run_named(&mut execution_context, "__global__");

                    println!("========= CALLING INIT ==========");
                    match vm.run_named(&mut execution_context, "init") {
                        Ok(return_value) => println!("Returned {:?}", return_value),
                        Err(err) => println!("Error {:?}", err),
                    }
                }
                Err(errors) => {
                    for error in errors {
                        report(file_path, error, &source, &tokens);
                    }
                }
            }
        }
        Err(e) => {
            for error in e {
                let span = &source[error.span.start..error.span.end];
                println!("[Err] {}\n{:#?}", span, error);
            }
        }
    }
}
