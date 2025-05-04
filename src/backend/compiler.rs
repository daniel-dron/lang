use std::{
    collections::{BTreeSet, HashMap},
    fmt::Debug,
    vec,
};

use crate::backend::vm::ExecutionContext;
use crate::common::value::*;
use crate::frontend::ast::*;

#[derive(Debug, Clone, Copy)]
pub struct ConstantId(pub usize);

#[derive(Debug, Clone)]
pub struct BinaryOpArgs {
    pub dest: RegisterId,
    pub left: RegisterId,
    pub right: RegisterId,
}

#[derive(Debug, Clone)]
pub struct UnaryOpArgs {
    pub dest: RegisterId,
    pub right: RegisterId,
}

#[derive(Debug, Clone)]
pub enum CallId {
    Index(usize),
    Register(RegisterId),
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionId(pub usize);

#[derive(Debug, Clone)]
pub enum OpCode {
    LoadConstant {
        dest: RegisterId,
        constant: ConstantId,
    }, // R(dest) = K(constant)
    Move {
        dest: RegisterId,
        src: RegisterId,
    }, // R(dest) = R(src)
    LoadGlobal {
        dest: RegisterId,
        src: String,
    },
    StoreGlobal {
        dest: String,
        src: RegisterId,
    },

    Add(BinaryOpArgs),            // R(dest) = R(left) + R(right)
    Sub(BinaryOpArgs),            // R(dest) = R(left) - R(right)
    Mul(BinaryOpArgs),            // R(dest) = R(left) * R(right)
    Div(BinaryOpArgs),            // R(dest) = R(left) / R(right)
    Equal(BinaryOpArgs),          // R(dest) = R(left) == R(right)
    Greater(BinaryOpArgs),        // R(dest) = R(left) > R(right)
    Lesser(BinaryOpArgs),         // R(dest) = R(left) < R(right)
    GreaterOrEqual(BinaryOpArgs), // R(dest) = R(left) >= R(right)
    LesserOrEqual(BinaryOpArgs),  // R(dest) = R(left) <= R(right)
    And(BinaryOpArgs),            // R(dest) = R(left) && R(right)
    Or(BinaryOpArgs),             // R(dest) = R(left) || R(right)
    Negate(UnaryOpArgs),          // R(dest) = -R(right)
    Not(UnaryOpArgs),             // R(dest) = !R(right)
    Return {
        from: Option<RegisterId>,
    },
    JumpIfFalse {
        cond: RegisterId,
        offset: isize,
    },
    Jump {
        offset: isize,
    },
    Call {
        id: CallId,
        ret: RegisterId,
        params: Vec<RegisterId>,
    },
    CallNative {
        id: String,
        ret: RegisterId,
        params: Vec<RegisterId>,
    },
    CreateClosure {
        dest: RegisterId,
        function_id: usize,
    },
    GetUpvalue {
        dest: RegisterId,
        idx: usize,
    },
    SetUpvalue {
        src: RegisterId,
        idx: usize,
    },
    CreateArray {
        dest: RegisterId,
        elements: Vec<RegisterId>,
    },
    GetArrayElement {
        dest: RegisterId,
        array: RegisterId,
        index: RegisterId,
    },
    SetArrayElement {
        array: RegisterId,
        index: RegisterId,
        value: RegisterId,
    },
}

// temporary struct for now. In the future might have more members for debug purposes
#[derive(Debug, Clone)]
pub struct Instruction {
    pub op: OpCode,
}

// TODO: implement properly
#[derive(Debug, Clone)]
pub struct RegisterAllocator {
    max: usize,
    available: BTreeSet<usize>,
}

impl Default for RegisterAllocator {
    fn default() -> Self {
        let max = 100;
        let mut available = BTreeSet::new();
        for i in 0..=max {
            available.insert(i);
        }

        Self { max, available }
    }
}

impl RegisterAllocator {
    pub fn get_free(&mut self) -> Option<RegisterId> {
        if let Some(&number) = self.available.iter().next() {
            self.available.remove(&number);
            Some(RegisterId(number))
        } else {
            None
        }
    }

    pub fn free(&mut self, number: RegisterId) -> bool {
        if number.0 > self.max {
            return false;
        }

        self.available.insert(number.0)
    }

    pub fn len(&self) -> usize {
        self.available.len()
    }
}

#[derive(Default, Clone)]
pub struct Prototype {
    pub name: String,
    pub parameters: Vec<(String, RegisterId)>,
    pub code: Vec<Instruction>,
    pub parent: Option<FunctionId>,
    pub declarations: HashMap<String, RegisterId>,
    pub upvalues: Vec<UpvalueTarget>,
    pub register_allocator: RegisterAllocator,
}

impl Debug for Prototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Prototype")
            .field("name", &self.name)
            .field("parameters", &self.parameters)
            .field("code", &self.code)
            .field("declarations", &self.declarations)
            .field("upvalues", &self.upvalues)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct CompilationUnit {
    pub functions: Vec<Prototype>,
    pub functions_map: HashMap<String, usize>,
    pub constants: Vec<Value>,
    pub globals: HashMap<String, Value>,

    last_register: RegisterId,
}

impl CompilationUnit {
    pub fn new(ast: &[Stmt]) -> Self {
        let mut unit = Self {
            functions: vec![],
            functions_map: HashMap::new(),
            constants: vec![],
            globals: HashMap::new(),
            last_register: RegisterId(0),
        };

        let mut boot_prototype = Prototype::default();
        boot_prototype.name = "__global__".into();
        let boot_prototype_id = FunctionId(unit.functions.len());
        unit.functions_map
            .insert("__global__".into(), boot_prototype_id.0);
        unit.functions.push(boot_prototype);

        // statements in global scope
        for statement in ast {
            match &statement.kind {
                StmtKind::FunctionDeclaration(declaration) => {
                    let prototype = unit.compile_function(&declaration, None);
                    unit.globals
                        .insert(declaration.name.clone(), Value::Function(prototype));
                }
                StmtKind::Let(declaration) => {
                    // Globals (All NIL for now...)
                    match unit.globals.get(&declaration.name) {
                        Some(_) => panic!("Global '{}' already declared!", declaration.name),
                        None => {
                            unit.globals
                                .insert(declaration.name.clone(), Value::Number(0.0));

                            // store initializer expression in the init context anonymous function
                            let src =
                                unit.compile_expr(boot_prototype_id, &declaration.initializer);
                            unit.last_register = src;
                            unit.emit_instruction(
                                boot_prototype_id,
                                Instruction {
                                    op: OpCode::StoreGlobal {
                                        dest: declaration.name.clone(),
                                        src,
                                    },
                                },
                            );
                        }
                    };
                }
                _ => panic!("Unexpected statement {:#?}", statement),
            }
        }

        unit.emit_instruction(
            boot_prototype_id,
            Instruction {
                op: OpCode::Return { from: None },
            },
        );

        unit
    }

    fn compile_function(
        &mut self,
        decl: &FunctionDeclarationStmt,
        parent_id: Option<FunctionId>,
    ) -> usize {
        // register function early. this allows for recursion
        let index = self.functions.len();
        self.functions_map.insert(decl.name.clone(), index);

        let mut prototype = Prototype::default();
        prototype.name = decl.name.clone();
        prototype.parent = parent_id;

        // declare the parameters
        let parameters = decl
            .parameters
            .clone()
            .iter()
            .map(|param| {
                let reg = prototype
                    .register_allocator
                    .get_free()
                    .expect("Ran out of registers");
                prototype.declarations.insert(param.name.clone(), reg);
                (param.name.clone(), reg)
            })
            .collect::<Vec<(String, RegisterId)>>();
        prototype.parameters = parameters;

        // pre register it
        self.functions.push(prototype);

        // compile body
        self.compile_statement(
            FunctionId(index),
            &Stmt::new(
                NodeId(0),
                StmtKind::Expr(decl.body.clone()),
                decl.body.span.clone(),
            ),
        );

        if !matches!(self.functions[index].code.last(), Some(instr) if matches!(instr.op, OpCode::Return { .. }))
        {
            self.functions[index].code.push(Instruction {
                op: OpCode::Return { from: None },
            });
        }

        index
    }

    fn needs_capture(&self, prototype_id: FunctionId, name: &str) -> Option<UpvalueTarget> {
        // check if name is a local in current function
        if self.functions[prototype_id.0]
            .declarations
            .contains_key(name)
        {
            return None;
        }

        // check if name is defined in any parent scope
        let mut scope = 1;
        let mut current = &self.functions[prototype_id.0];
        while let Some(parent_id) = current.parent {
            let parent = &self.functions[parent_id.0];
            if let Some(reg) = parent.declarations.get(name) {
                return Some(UpvalueTarget {
                    scope,
                    idx: *reg,
                    func_name: parent.name.clone(),
                    value_name: name.into(),
                });
            }
            current = parent;
            scope += 1;
        }

        None // not found in any parent scope
    }

    fn register_constant(&mut self, val: Value) -> ConstantId {
        // check if constant already exists
        if let Some(id) = self.constants.iter().position(|x| *x == val) {
            return ConstantId(id);
        }

        let id = self.constants.len();
        self.constants.push(val);
        ConstantId(id)
    }

    fn emit_instruction(&mut self, prototype_id: FunctionId, instruction: Instruction) -> usize {
        let prototype = &mut self.functions[prototype_id.0];
        let pos = prototype.code.len();
        prototype.code.push(instruction);
        pos
    }

    // emit instruction to load a constant from the constant table into a free register
    fn emit_constant_load(&mut self, prototype_id: FunctionId, value: Value) -> RegisterId {
        let constant_id = self.register_constant(value);

        let prototype = &mut self.functions[prototype_id.0];
        let register_id = prototype
            .register_allocator
            .get_free()
            .expect("Ran out of registers");

        self.emit_instruction(
            prototype_id,
            Instruction {
                op: OpCode::LoadConstant {
                    dest: register_id,
                    constant: constant_id,
                },
            },
        );
        register_id
    }

    fn compile_statement(&mut self, prototype_id: FunctionId, statement: &Stmt) {
        match &statement.kind {
            StmtKind::Expr(expr) => {
                let ret = self.compile_expr(prototype_id, expr);
                self.last_register = ret;
            }
            StmtKind::Let(declaration_stmt) => {
                let dest = self.compile_expr(prototype_id, &declaration_stmt.initializer);

                let prototype = &mut self.functions[prototype_id.0];
                prototype
                    .declarations
                    .insert(declaration_stmt.name.clone(), dest);
            }
            StmtKind::Assignment(target, initializer) => {
                let src = self.compile_expr(prototype_id, initializer);

                match target {
                    Assignable::Identifier(target) => {
                        let prototype = &self.functions[prototype_id.0];
                        match prototype.declarations.get(target) {
                            Some(id) => {
                                self.emit_instruction(
                                    prototype_id,
                                    Instruction {
                                        op: OpCode::Move { dest: *id, src },
                                    },
                                );
                            }
                            None => {
                                if let Some(upvalue) = self.needs_capture(prototype_id, target) {
                                    self.emit_instruction(
                                        prototype_id,
                                        Instruction {
                                            op: OpCode::SetUpvalue {
                                                src,
                                                idx: prototype
                                                    .upvalues
                                                    .iter()
                                                    .position(|val| *val == upvalue)
                                                    .expect("Upvalue wasnt in the list"),
                                            },
                                        },
                                    );
                                } else if self.globals.contains_key(target) {
                                    self.emit_instruction(
                                        prototype_id,
                                        Instruction {
                                            op: OpCode::StoreGlobal {
                                                dest: target.clone(),
                                                src,
                                            },
                                        },
                                    );
                                } else {
                                    panic!("Variable '{}' not declared!", target);
                                }
                            }
                        }

                        let prototype = &mut self.functions[prototype_id.0];
                        prototype.register_allocator.free(src);
                    }
                    Assignable::IndexAccess { object, index } => {
                        let array_reg = self.compile_expr(prototype_id, object);
                        let index_reg = self.compile_expr(prototype_id, index);

                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::SetArrayElement {
                                    array: array_reg,
                                    index: index_reg,
                                    value: src,
                                },
                            },
                        );

                        let prototype = &mut self.functions[prototype_id.0];
                        prototype.register_allocator.free(array_reg);
                        prototype.register_allocator.free(index_reg);
                    }
                }
            }
            StmtKind::Return(expr) => {
                let mut ret: Option<RegisterId> = None;

                if let Some(expr) = expr {
                    ret = Some(self.compile_expr(prototype_id, &expr));
                }

                self.emit_instruction(
                    prototype_id,
                    Instruction {
                        op: OpCode::Return { from: ret },
                    },
                );
            }
            StmtKind::If(if_stmt) => {
                let cond = self.compile_expr(prototype_id, &if_stmt.condition);

                let cond_jump_pos = self.emit_instruction(
                    prototype_id,
                    Instruction {
                        op: OpCode::JumpIfFalse { cond, offset: 0 },
                    },
                );

                let _ = self.compile_statement(prototype_id, &if_stmt.then_block);

                if let Some(else_block) = &if_stmt.else_block {
                    // Emit a placeholder jump instruction to skip the "else" block
                    let then_end_jump_pos = self.emit_instruction(
                        prototype_id,
                        Instruction {
                            op: OpCode::Jump { offset: 0 },
                        },
                    );

                    {
                        let prototype = &mut self.functions[prototype_id.0];
                        // Patch the conditional jump to skip to the "else" block
                        let else_offset = prototype.code.len() as isize - cond_jump_pos as isize;
                        prototype.code[cond_jump_pos].op = OpCode::JumpIfFalse {
                            cond,
                            offset: else_offset,
                        };
                    }

                    // Compile the "else" block
                    let _ = self.compile_statement(prototype_id, else_block);

                    let prototype = &mut self.functions[prototype_id.0];
                    // Patch the jump at the end of the "then" block to skip the "else" block
                    let else_end_offset =
                        prototype.code.len() as isize - then_end_jump_pos as isize;
                    prototype.code[then_end_jump_pos].op = OpCode::Jump {
                        offset: else_end_offset,
                    };
                } else {
                    let prototype = &mut self.functions[prototype_id.0];

                    // patch jump instruction to jump after the <then block>
                    let then_offset = prototype.code.len() as isize - cond_jump_pos as isize;
                    prototype.code[cond_jump_pos].op = OpCode::JumpIfFalse {
                        cond,
                        offset: then_offset,
                    };
                }
            }
            StmtKind::FunctionDeclaration(function_declaration_stmt) => {
                let closure_prototype =
                    self.compile_function(&function_declaration_stmt, Some(prototype_id));

                // register symbol
                let dest =
                    self.emit_constant_load(prototype_id, Value::Function(closure_prototype));

                let prototype = &mut self.functions[prototype_id.0];
                prototype
                    .declarations
                    .insert(function_declaration_stmt.name.clone(), dest);
            }
        }
    }

    // compiles expression recursively and emits instructions
    // returns the register where the result lies
    fn compile_expr(&mut self, prototype_id: FunctionId, expr: &Expr) -> RegisterId {
        match &expr.kind {
            ExprKind::Literal(Literal::Number(val)) => {
                self.emit_constant_load(prototype_id, Value::Number(*val as f64))
            }
            ExprKind::Literal(Literal::Boolean(val)) => {
                self.emit_constant_load(prototype_id, Value::Boolean(*val))
            }
            ExprKind::Literal(Literal::String(val)) => {
                self.emit_constant_load(prototype_id, Value::String(val.clone()))
            }
            ExprKind::Literal(Literal::Array(vec)) => {
                let element_registers = vec
                    .iter()
                    .map(|val| self.compile_expr(prototype_id, val))
                    .collect::<Vec<RegisterId>>();

                let dest = self.functions[prototype_id.0]
                    .register_allocator
                    .get_free()
                    .expect("Ran out of registers");

                // Emit instruction to create the array
                self.emit_instruction(
                    prototype_id,
                    Instruction {
                        op: OpCode::CreateArray {
                            dest,
                            elements: element_registers.clone(),
                        },
                    },
                );

                // Free the element registers since they're now contained in the array
                let prototype = &mut self.functions[prototype_id.0];
                for reg in element_registers {
                    prototype.register_allocator.free(reg);
                }

                dest
            }
            ExprKind::Identifier(id) => {
                let dest = self.functions[prototype_id.0]
                    .register_allocator
                    .get_free()
                    .expect("Ran out of registers");

                if self.functions[prototype_id.0].declarations.contains_key(id) {
                    self.emit_instruction(
                        prototype_id,
                        Instruction {
                            op: OpCode::Move {
                                dest,
                                src: self.functions[prototype_id.0].declarations[id],
                            },
                        },
                    );
                } else if let Some(upvalue) = self.needs_capture(prototype_id, id) {
                    if !self.functions[prototype_id.0].upvalues.contains(&upvalue) {
                        self.functions[prototype_id.0].upvalues.push(upvalue);
                    }

                    // add instruction to get the upvalue
                    self.emit_instruction(
                        prototype_id,
                        Instruction {
                            op: OpCode::GetUpvalue {
                                dest,
                                idx: self.functions[prototype_id.0].upvalues.len() - 1,
                            },
                        },
                    );
                } else {
                    // check if its a global
                    if let Some(_) = self.globals.get(id) {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::LoadGlobal {
                                    dest,
                                    src: id.clone(),
                                },
                            },
                        );
                    } else {
                        self.functions[prototype_id.0].register_allocator.free(dest);
                        panic!("Non variable named {:?}", id);
                    }
                }
                dest
            }
            ExprKind::Binary(binary_expr) => {
                let left = self.compile_expr(prototype_id, &binary_expr.left);
                let right = self.compile_expr(prototype_id, &binary_expr.right);
                let dest = self.functions[prototype_id.0]
                    .register_allocator
                    .get_free()
                    .expect("Ran out of registers");

                match binary_expr.operator {
                    BinaryOp::Add => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::Add(BinaryOpArgs { dest, left, right }),
                            },
                        );
                    }
                    BinaryOp::Subtract => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::Sub(BinaryOpArgs { dest, left, right }),
                            },
                        );
                    }
                    BinaryOp::Multiply => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::Mul(BinaryOpArgs { dest, left, right }),
                            },
                        );
                    }
                    BinaryOp::Divide => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::Div(BinaryOpArgs { dest, left, right }),
                            },
                        );
                    }
                    BinaryOp::Equal => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::Equal(BinaryOpArgs { dest, left, right }),
                            },
                        );
                    }
                    BinaryOp::Greater => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::Greater(BinaryOpArgs { dest, left, right }),
                            },
                        );
                    }
                    BinaryOp::Lesser => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::Lesser(BinaryOpArgs { dest, left, right }),
                            },
                        );
                    }
                    BinaryOp::GreaterOrEqual => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::GreaterOrEqual(BinaryOpArgs { dest, left, right }),
                            },
                        );
                    }
                    BinaryOp::LessesOrEqual => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::LesserOrEqual(BinaryOpArgs { dest, left, right }),
                            },
                        );
                    }
                    BinaryOp::And => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::And(BinaryOpArgs { dest, left, right }),
                            },
                        );
                    }
                    BinaryOp::Or => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::Or(BinaryOpArgs { dest, left, right }),
                            },
                        );
                    }
                }

                // free the previous registers
                self.functions[prototype_id.0].register_allocator.free(left);
                self.functions[prototype_id.0]
                    .register_allocator
                    .free(right);

                dest
            }
            ExprKind::Unary(unary_expr) => {
                let right = self.compile_expr(prototype_id, &unary_expr.operand);
                let dest = self.functions[prototype_id.0]
                    .register_allocator
                    .get_free()
                    .expect("Ran out of registers");

                match &unary_expr.operator {
                    UnaryOp::Negate => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::Negate(UnaryOpArgs { dest, right }),
                            },
                        );
                    }
                    UnaryOp::Not => {
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::Not(UnaryOpArgs { dest, right }),
                            },
                        );
                    }
                }

                dest
            }
            ExprKind::Block(block) => {
                for statement in &block.statements {
                    self.compile_statement(prototype_id, &statement);
                }

                match &block.result {
                    Some(ret) => {
                        let out = self.compile_expr(prototype_id, ret);

                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::Move {
                                    dest: RegisterId(0),
                                    src: out,
                                },
                            },
                        );

                        out
                    }
                    None => RegisterId(0),
                }
            }
            ExprKind::Call(call_expr) => {
                let ret = self.functions[prototype_id.0]
                    .register_allocator
                    .get_free()
                    .expect("Ran out of registers");

                // compile arguments and get register allocations
                let args = call_expr
                    .arguments
                    .iter()
                    .map(|arg| self.compile_expr(prototype_id, arg))
                    .collect::<Vec<RegisterId>>();

                let target_call = if let ExprKind::Identifier(name) = &call_expr.callee.kind {
                    // named symbolic call
                    if let Some(id) = self.functions_map.get(name) {
                        if self.functions[*id].parameters.len() != call_expr.arguments.len() {
                            panic!("Unmatched parameters count on call to `{}`", name);
                        }
                        CallId::Index(*id)
                    } else if self.globals.contains_key(name) {
                        // this global MIGHT be a function, we need to check it at runtime, cant do more here...
                        let global_reg = self.functions[prototype_id.0]
                            .register_allocator
                            .get_free()
                            .expect("Ran out of registers");

                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::LoadGlobal {
                                    dest: global_reg,
                                    src: name.clone(),
                                },
                            },
                        );

                        CallId::Register(global_reg)
                    } else if self.functions[prototype_id.0]
                        .declarations
                        .contains_key(name)
                    {
                        // parameter
                        CallId::Register(
                            self.functions[prototype_id.0]
                                .declarations
                                .get(name)
                                .unwrap()
                                .clone(),
                        )
                    } else {
                        // maybe a native call? delay error to VM
                        self.emit_instruction(
                            prototype_id,
                            Instruction {
                                op: OpCode::CallNative {
                                    id: name.clone(),
                                    ret,
                                    params: args.clone(),
                                },
                            },
                        );
                        return ret;
                    }
                } else {
                    let ret = self.compile_expr(prototype_id, &call_expr.callee);
                    CallId::Register(ret)
                };

                self.emit_instruction(
                    prototype_id,
                    Instruction {
                        op: OpCode::Call {
                            id: target_call,
                            ret,
                            params: args,
                        },
                    },
                );

                ret
            }
            ExprKind::Closure(closure_expr) => {
                let name = format!("__closure_{}__", self.functions.len());
                let closure_prototype = self.compile_function(
                    &FunctionDeclarationStmt {
                        name: name.clone(),
                        return_ty: closure_expr.return_type.clone(),
                        parameters: closure_expr.parameters.clone(),
                        body: closure_expr.body.clone(),
                    },
                    Some(prototype_id),
                );

                let dest = self.functions[prototype_id.0]
                    .register_allocator
                    .get_free()
                    .expect("Ran out of registers!");
                self.emit_instruction(
                    prototype_id,
                    Instruction {
                        op: OpCode::CreateClosure {
                            dest,
                            function_id: closure_prototype,
                        },
                    },
                );

                // closures should not be accessable through their mangled names
                // self.functions_map.insert(name, index);

                self.emit_constant_load(prototype_id, Value::Function(closure_prototype));

                dest
            }
            ExprKind::IndexAccess(array_index_expr) => {
                let array = self.compile_expr(prototype_id, &array_index_expr.target);
                let index = self.compile_expr(prototype_id, &array_index_expr.index);

                let dest = self.functions[prototype_id.0]
                    .register_allocator
                    .get_free()
                    .expect("Ran out of registers");

                self.emit_instruction(
                    prototype_id,
                    Instruction {
                        op: OpCode::GetArrayElement { dest, array, index },
                    },
                );

                dest
            }
        }
    }
}

pub type NativeFn = fn(&ExecutionContext, Vec<Value>) -> Result<Value, String>;

#[derive(Debug, Default, Clone)]
pub struct NativeFunctionRegistry {
    functions: HashMap<String, NativeFn>,
}

impl NativeFunctionRegistry {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    pub fn register(&mut self, name: &str, func: NativeFn) {
        self.functions.insert(name.to_string(), func);
    }

    pub fn get(&self, name: &str) -> Option<&NativeFn> {
        self.functions.get(name)
    }

    pub fn call<'a>(
        &self,
        name: &str,
        context: &ExecutionContext,
        args: Vec<Value>,
    ) -> Result<Value, String> {
        match self.get(name) {
            Some(func) => func(context, args),
            None => Err(format!("Native function '{}' not found", name)),
        }
    }
}
