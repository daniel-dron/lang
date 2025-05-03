// Example script demonstrating closures vs functions
// This shows how closures capture values from outer scopes

fn basic_capture_example() {
    print("\n1. Basic Capture Example:");
    
    let outer_value = 42;
    
    // Regular function - doesn't capture outer_value
    fn regular_function() {
        // This would fail at compile time because functions don't capture
        // Uncomment to see the error:
        // return outer_value;
    }
    
    // Closure - captures outer_value
    let closure = @() {
        return outer_value; // Successfully captures
    };  // Note the semicolon here
    
    let result = closure();
    print("  Closure returned: " + result);
    assert(result == 42, "Closure should capture outer_value");
}

fn shared_capture_example() {
    print("\n2. Shared Capture Example:");
    
    let counter = 0;
    
    // Create two closures that both capture the same variable
    let increment = @() {
        counter = counter + 1;
        return counter;
    };  // Semicolon here
    
    let get_counter = @() {
        return counter;
    };  // Semicolon here
    
    // First increment
    let result1 = increment();
    print("  After first increment: " + result1);
    assert(result1 == 1, "First increment should return 1");
    
    // Second increment
    let result2 = increment();
    print("  After second increment: " + result2);
    assert(result2 == 2, "Second increment should return 2");
    
    // Get current value
    let current = get_counter();
    print("  Current counter value: " + current);
    assert(current == 2, "get_counter should return the shared value");
}

fn persistence_example() {
    print("\n3. Closure Persistence Example:");
    
    // Function that returns a closure
    fn create_counter(start: f64) -> fn() -> f64 {
        let count = start;
        
        let counter = @() {
            count = count + 1;
            return count;
        };  // Semicolon here
        
        return counter;
    }
    
    // Create a counter starting at 100
    let my_counter = create_counter(100);
    
    // The original function has returned, but the closure still works
    let value1 = my_counter();
    print("  First call: " + value1);
    assert(value1 == 101, "First call should return 101");
    
    let value2 = my_counter();
    print("  Second call: " + value2);
    assert(value2 == 102, "Second call should return 102");
    
    // Create another counter with different starting value
    let another_counter = create_counter(500);
    let result = another_counter();
    print("  Separate counter: " + result);
    assert(result == 501, "Separate counter should start from its own value");
}

fn nested_closure_example() {
    print("\n4. Nested Closure Example:");
    
    let x = 10;
    
    // Outer closure
    let outer = @() {
        let y = 20;
        
        // Inner closure captures both x from global and y from outer
        let inner = @() {
            return x + y;
        };  // Semicolon here
        
        return inner;
    };  // Semicolon here
    
    let inner_fn = outer();
    let result = inner_fn();
    
    print("  Nested closure result: " + result);
    assert(result == 30, "Nested closure should capture from multiple scopes");
}

fn init() {
    print("=== Function vs Closure Value Capturing ===");
    
    // Demonstrate basic capturing behavior
    basic_capture_example();
    
    // Show that captured values are shared between closures
    shared_capture_example();
    
    // Demonstrate closure persistence after parent function returns
    persistence_example();
    
    // Show nested closures and multi-level capturing
    nested_closure_example();
    
    print("All tests passed successfully!");
}