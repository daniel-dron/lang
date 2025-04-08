// Example demonstrating global variables in our language

// Global variables are declared in the top-level scope
let global_counter = 0;
let global_message = "Hello from global scope";

fn access_from_function() {
    print("\n1. Accessing globals from function:");
    
    // Regular functions can access globals directly
    print("  global_counter from function: " + global_counter);
    assert(global_counter == 1, "Unexpected global counter value");
    global_counter = global_counter + 1;
    print("  After incrementing in function: " + global_counter);
    assert(global_counter == 2, "Unexpected global counter value");
}

fn access_from_closure() {
    print("\n2. Accessing globals from closure:");
    
    // Closures can also access globals
    let my_closure = @() {
        print("  global_counter from closure: " + global_counter);
        global_counter = global_counter + 1;
        return global_counter;
    };
    
    let result = my_closure();
    print("  Closure returned: " + result);
}

fn modify_in_nested_scopes() {
    print("\n3. Modifying globals in nested scopes:");
    
    fn nested_function() {
        global_counter = global_counter + 10;
        print("  Increased by 10 in nested function: " + global_counter);
        
        let nested_closure = @() {
            global_counter = global_counter * 2;
            print("  Doubled in nested closure: " + global_counter);
        };
        
        nested_closure();
    }
    
    nested_function();
}

fn shadowing_example() {
    print("\n4. Shadowing globals:");
    
    // Local variable shadows the global
    let global_counter = 100;
    print("  Local shadow value: " + global_counter);
    
    // This modifies the local shadow, not the global
    global_counter = global_counter + 5;
    print("  Local shadow after increment: " + global_counter);
    
    // This closure captures the local shadow, not the global
    let closure_with_shadow = @() {
        return global_counter;
    };
    
    print("  Closure with shadowed value: " + closure_with_shadow());
    
    // Check that the actual global wasn't affected by the shadowing
    print("  Actual global_counter: " + global_counter);
    
    // We can still access the global by removing the local shadow
    // (Note: In many languages you'd use a special syntax here, but for this
    // example we'll just demonstrate the concept of shadowing)
}

fn init() {
    print("=== Global Variables Example ===");
    
    // Access globals from main function
    print("Initial global_counter: " + global_counter);
    print("Global message: " + global_message);
    
    // Modify a global
    global_counter = global_counter + 1;
    print("After incrementing: " + global_counter);
    
    // Test accessing globals from different scopes
    access_from_function();
    access_from_closure();
    modify_in_nested_scopes();
    shadowing_example();
    
    print("Final global_counter: " + global_counter);
    return 0;
}