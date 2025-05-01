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