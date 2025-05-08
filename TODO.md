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

- [x] array types
    - [ ] proper operations like push/remove (later with objects)
- [x] primitive vs complex types
    - [ ] look into completly removing tagged union types at runtime in the VM
        the vm has no need to know the type of the values if the compiler emits type specific instructions
        this optimization would halven the runtime memory usage, since a value would go from 16 bytes (8 value + 1 tag + pad to 16)
        to only being 8 bytes (64 bits = value itself)
- [ ] class/struct
    - [x] custom types
    - [x] member fields
    - [ ] methods
    - [ ] recursive types (during type checking is problem)
- [ ] track variable life times
    - [ ] for register reuse
- [ ] copy objects
- [ ] for loops
    - [ ] on ranges
    - [ ] on iterables (str, arr)
- [ ] match with simple patterns
- [ ] optional
- [ ] errors
- [ ] JIT

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
