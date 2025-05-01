// Example demonstrating pure function behavior without any value capturing
// This shows how functions work in isolation without closure behavior

fn basic_function_calls() {
    print("\n1. Basic Function Calls:");

    // Function with no arguments
    fn greet() {
        print("  Hello, world!");
    }

    // Function with one argument
    fn greet_person(name: str) {
        print("  Hello, " + name + "!");
    }

    // Function with multiple arguments
    fn describe_person(name: str, age: f64, occupation: str) {
        print("  " + name + " is " + age + " years old and works as a " + occupation + ".");
    }

    // Call all the functions
    greet();
    greet_person("Alice");
    describe_person("Bob", 30, "programmer");
}

fn recursion_examples() {
    print("\n2. Recursion Examples:");

    // Simple recursive function to calculate factorial
    fn factorial(n: f64) -> f64 {
        if (n <= 1) {
            return 1;
        } else {
            return n * factorial(n - 1);
        }
    }

    // Test factorial
    print("  Factorial of 5: " + factorial(5));
    assert(factorial(5) == 120, "Factorial calculation is incorrect");

    // Recursive function to calculate Fibonacci numbers
    fn fibonacci(n: f64) -> f64 {
        if (n <= 0) {
            return 0;
        } else if (n == 1) {
            return 1;
        } else {
            return fibonacci(n - 1) + fibonacci(n - 2);
        }
    }

    // Test fibonacci
    print("  Fibonacci(7): " + fibonacci(7));
    assert(fibonacci(7) == 13, "Fibonacci calculation is incorrect");

    // Tail-recursive function
    fn sum_to_n(n: f64, acc: f64) -> f64 {
        if n <= 0 {
            return acc;
        } else {
            return sum_to_n(n - 1, acc + n);
        }
    }

    print("  Sum from 1 to 10: " + sum_to_n(10, 0));
    assert(sum_to_n(10, 0) == 55, "Sum calculation is incorrect");
}

fn function_return_values() {
    print("\n4. Function Return Values:");

    // Return different types
    fn return_string() -> str {
        return "This is a string";
    }

    fn return_number() -> f64 {
        return 42;
    }

    fn return_boolean() -> bool {
        return true;
    }

    // Functions returning functions
    fn get_add_function() -> fn(f64, f64) -> f64 {
        fn add(a: f64, b: f64) -> f64 {
            return a + b;
        }

        return add;
    }

    fn get_multiply_function() -> fn(f64, f64) -> f64 {
        fn multiply(a: f64, b: f64) -> f64 {
            return a * b;
        }

        return multiply;
    }

    // Using returned functions
    let add_func = get_add_function();
    let multiply_func = get_multiply_function();

    print("  add_func(10, 20): " + add_func(10, 20));
    print("  multiply_func(10, 20): " + multiply_func(10, 20));

    // Conditionally return different functions
    fn get_operation(is_addition: bool) -> fn(f64, f64) -> f64 {
        if (is_addition) {
            fn operation(a: f64, b: f64) -> f64 {
                return a + b;
            }
            return operation;
        } else {
            fn operation(a: f64, b: f64) -> f64 {
                return a * b;
            }
            return operation;
        }
    }

    let operation1 = get_operation(true);
    let operation2 = get_operation(false);

    print("  operation1(5, 3): " + operation1(5, 3)); // Addition: 8
    print("  operation2(5, 3): " + operation2(5, 3)); // Multiplication: 15
}

fn init() -> f64 {
    print("=== Functions, Arguments, and Recursion ===");

    basic_function_calls();
    recursion_examples();
    function_return_values();

    print("All function tests completed!");
    return 0;
}
