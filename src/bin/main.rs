use std::{
    env, fs, process,
    time::{SystemTime, UNIX_EPOCH},
};

use lang_core::{
    CompilationUnit, ExecutionContext, Lexer, NewParser, Scanner, TypeChecker, Value,
    VirtualMachine,
    error_reporter::report,
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
    // expected params
    // arg1: boolean
    // arg2: expected message string
    if let (Value::Boolean(b), Value::String(message)) = (&params[0], &params[1]) {
        if *b {
            return Ok(Value::Boolean(true));
        } else {
            Err(message.clone())
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
        Ok(v) => {
            let mut type_checker = TypeChecker::new();
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

            println!("{:#?}", type_checker);

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
                    // let status = vm.run_named(&mut execution_context, "__global__");
                    let status = vm.run_named(&mut execution_context, "__global__");

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

            // match e.token {
            //     Some(token) => {
            // let line = token.line;
            // let column = token.column - token.lexeme.len() + 1;
            // let token_length = token.lexeme.len();

            // // Split the source into lines and get the error line
            // let lines: Vec<&str> = source.split('\n').collect();

            // if line > 0 && line <= lines.len() {
            //     let error_line = lines[line - 1];
            //     let line_info = format!("[{}:{}]: ", line, column);

            //     // Format the error title
            //     let error_title = "E0001: Found an unexpected start of expression";

            //     // Print error title, location, and code
            //     println!("\x1b[1;31m{}\x1b[0m", error_title);
            //     println!("{}{}", line_info, error_line);

            //     // Create squiggly underline
            //     let padding = " ".repeat(column + line_info.len() - 2); // 1 for the ^
            //     let squiggles = "~".repeat(token_length);

            //     // Calculate position for the pointer line
            //     // The pointer should point to the position before the token (where the operator should go)
            //     let pointer_position = column + line_info.len() - 2; // -1 for zero-indexing, -1 to point before token

            //     // Create the pointer line with a helpful suggestion
            //     let pointer_padding = " ".repeat(pointer_position);

            //     // Print the pointer line// Print the squiggly underline and error message
            //     println!(
            //         "{}\x1b[32m│\x1b[31m{} {}\x1b[0m ",
            //         padding, squiggles, e.message
            //     );
            //     println!("\x1b[32m{}│\x1b[0m", pointer_padding);
            //     println!(
            //         "{}\x1b[32m╰── place a ';' or an operator (+, -, *, /) here\x1b[0m",
            //         pointer_padding
            //     );

            //     // Print a more detailed explanation/help text
            //     println!(
            //         "\nHelp: Found numeric literal '{}' where an operator was expected.",
            //         token.lexeme
            //     );
            //     println!(
            //         "Adjacent expressions must be separated by an operator or terminated with a semicolon."
            //     );
            // } else {
            //     println!("\x1b[1;31mE0001: Missing Operator\x1b[0m");
            //     println!("Error at position {:#?}: {}", token, e.message);
            // }
            //     }
            //     None => {
            //         println!("\x1b[1;31mE0001: Syntax Error\x1b[0m");
            //         println!("Error at end of file: {}", e.message);
            //     }
            // }
        }
    }
}
