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

- [ ] do not allow assigning a void return to a variable
    ``` rust
        fn foo() {
            // nothing
        }

        let test = foo(); // this should return an error
    ```

- [x] array types
- [ ] primitive vs complex types
    - [ ] look into completly removing tagged union types at runtime in the VM
        the vm has no need to know the type of the values if the compiler emits type specific instructions
        this optimization would halven the runtime memory usage, since a value would go from 16 bytes (8 value + 1 tag + pad to 16)
        to only being 8 bytes (64 bits = value itself)
- [ ] track variable life times
    - [ ] for register reuse
    - [ ] for moved objects
- [ ] implement move, copy, clone and reference
    - [ ] reference
    - [ ] copy
    - [ ] move
    - [ ] clone
- [ ] for loops
    - [ ] on ranges
    - [ ] on iterables (str, arr)
- [ ] match with simple patterns
- [ ] optional
- [ ] errors

## Labels Issues:
- bug: For errors and incorrect behavior
- enhancement: For improvements to existing features
- feature: For new functionality
- documentation: For documentation improvements
- refactor: For code cleanup without functionality changes
- performance: For optimizations
- help-wanted: To attract contributors
- good-first-issue: For beginner-friendly tasks

## Commits

```
<type>(<scope>): <short summary>

<optional body>

<optional footer>
```

### Tags
- feat: New feature (e.g., feat(parser): Add support for lambda expressions)
- fix: Bug fix (e.g., fix(lexer): Fix token position calculation)
- refactor: Code changes that neither fix bugs nor add features
- test: Adding or correcting tests
- docs: Documentation changes
- perf: Performance improvements
- chore: Maintenance tasks, dependency updates, etc.

### Scopes for Your Project

Examples:
- frontend, lexer, parser, ast
- middle, type-system, typechecker
- backend, compiler, vm
- ci, build, project