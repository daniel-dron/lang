- [x] Parameter arguments
- [x] Make a main entry point. Do not allow normal code outside functions. Only declarations should be possible.
    ```
    let global = "Hello";
    
    let update() {
        do_update_stuff();
        // ...
    }

    update();  // <-- this should not be allowed.
    ```
- [x] Instead of treating the compiled script as a Prototype, create a wrapper type (CompilationUnit) 
    * inside it, there should be the globals and a list of all functions inside the script
    * also create a pseudo-anonymous function that runs all global initializer expressions
        - [x] Move all compilation to CompilationUnit. Prototype and FunctionPrototype are just data holders
        - [x] Use the shared constants vector in CompilationUnit
        - [x] Move all execution logic to VirtualMachine. CallFrame are just data, and their operations should not depend on VirtualMachine 
        - [x] Globals
- [x] Better parameter parsing
- [x] Native call binds. (print, input, file I/O, etc)
- [x] Comments
- [x] Closures/Access to higher scoped variables
    ```
        let test = 10; // <-- make this accessible inside foo
        fn foo() {
            return test + 10;
        }
        return foo();
    ```
    - [x] Upvalue and Closed up values
    - [x] On nested closures, something breaks... Example:
        Also, in this case, when inner is returned, it should not close on X since its still available. Maybe should check scope >= n
        ```
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

        ``` 
- [ ] Create a concept of unresolved symbols. Placeholder concepts for identifiers/calls to be resolved right before running.
    * right before running, the VM must have registered all the symbols
    * the prototype should have a list of opcodes that need resolving
    * resolve/fix the instructions
    * this would support native calls and other global variables
    * in the future, we could "export" these VM native symbols as snapshots to be used in LSP

- [ ] Optimize opcode size
    * Either try to pack and reduce current size
    or
    * Use variable sized opcodes. Have a Chunk/Bytecode struct, and a Bytecode dispatcher that encodes and decodes bytecodes.
        ```
            //...
            bytes: Vec<u8>
            //...

            dispatcher.encode(OpCode) // bytes.push(...) ...
            dispatcher.encode(OpCode);
            ...

            dispatcher.decode_next() -> OpCode // <-- (returns the original opcode)
        ```

- [x] Rewrite parser from combinatoric parser to recursive descent parser
    - [x] expressions
    - [x] function declarations
    - [x] parameters
    - [x] basic error reporting
    - [x] error sync
    - [x] calls
    - [x] assignment
    - [x] conditionals
    - [x] if statements
    - [x] closures

- [ ] Static types.
    - [x] Implement support for static types. Do not allow re-typing at comp/runtime
    - [x] Type checks during compile and runtime
    - [ ] Type inference
        - [ ] Scopes during type inference (for stuff like parameters and shadowing)
        - [ ] Detect conflict in multiple return paths type
            ```
                /// this func has a conflict of return types
                fn test(b: bool) -> f64 {
                    if b {
                        return 10;
                    } else {
                        return "text";
                    }
                }

                // this one also does (float vs never)
                fn foo(bar: bool) -> f64 {
                    if bar {
                        return 10;
                    }
                }
            ```
    ```
        // infered
        let var = 10.0f; // Number
        let text = "Hello World!"; // String
        let array = [1, 2, 3, 4, 5]; // Array
        
        // explicit
        let var: f64 = 10.0f; // Number
        let text: str = "Hello World!"; // String
        let array: vec<f64> = [1, 2, 3, 4, 5]; // Array
    ```

- [ ] Detect paths that are missing return statements
    ```
        fn foo(bar: bool) -> f64 {
            if bar {
                return 10;
            }
        }
    ```

- [ ] Array types (check lua implementation? or just a simple hashmap)
- [ ] For loops. Range and iterator based.
    ```
    for i in 0..n {
        // ...
    }

    let array = [1, 2, 3, 4, 5];
    for i in array {
        // ...
    }
    ```