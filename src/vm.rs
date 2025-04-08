use core::panic;
use std::cell::RefCell;
use std::rc::Rc;

use lang_core::value;

use crate::compiler::{
    self, BinaryOpArgs, CallId, CompilationUnit, FunctionId, Instruction, NativeFn,
    NativeFunctionRegistry,
};
use crate::value::{Closure, RegisterId, Upvalue, UpvalueTarget, Value};

#[derive(Debug)]
pub struct ExecutionContext {
    call_stack: Vec<CallFrame>,
    current_frame: usize,
    pub unit: CompilationUnit,
}

impl ExecutionContext {
    pub fn new(unit: &CompilationUnit) -> ExecutionContext {
        Self {
            call_stack: vec![],
            current_frame: 0,
            unit: unit.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallFrame {
    prototype_id: FunctionId,
    registers: Vec<Value>,
    upvalues: Vec<Upvalue>,
    ip: usize,
    ret: RegisterId,
}

#[derive(Debug)]
pub struct VirtualMachine {
    native_registry: NativeFunctionRegistry,
}

#[derive(Debug)]
pub enum ExecuteStatus {
    Continue,
    Return(Value),
    Call(usize, Vec<(RegisterId, RegisterId)>, RegisterId),
    CallClosure(Rc<Closure>, Vec<(RegisterId, RegisterId)>, RegisterId),
    CallNative(String, Vec<RegisterId>, RegisterId),
}

type ExecutionResult = Result<ExecuteStatus, String>;

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            native_registry: NativeFunctionRegistry::new(),
        }
    }

    pub fn register_native(&mut self, name: &str, func: NativeFn) {
        self.native_registry.register(name.into(), func);
    }

    pub fn create_context(&self, unit: &CompilationUnit) -> ExecutionContext {
        ExecutionContext::new(unit)
    }

    fn apply_binary_op<F>(registers: &mut Vec<Value>, args: &BinaryOpArgs, op: F)
    where
        F: FnOnce(Value, Value) -> Result<Value, String>,
    {
        let left = registers[args.left.0].clone();
        let right = registers[args.right.0].clone();

        match op(left, right) {
            Ok(result) => registers[args.dest.0] = result,
            Err(err) => {
                panic!("{}", err);
            }
        }
    }

    fn execute_instruction(
        &self,
        instruction: Instruction,
        context: &mut ExecutionContext,
    ) -> ExecutionResult {
        let mut frame = &mut context.call_stack[context.current_frame];

        match &instruction.op {
            compiler::OpCode::LoadConstant { dest, constant } => {
                frame.registers[dest.0] = context.unit.constants[constant.0].clone();
            }
            compiler::OpCode::Add(args) => {
                Self::apply_binary_op(&mut frame.registers, &args, |left, right| left + right);
            }
            compiler::OpCode::Move { dest, src } => {
                frame.registers[dest.0] = frame.registers[src.0].clone();
            }
            compiler::OpCode::Return { from } => {
                return Ok(ExecuteStatus::Return(if let Some(from) = from {
                    frame.registers[from.0].clone()
                } else {
                    Value::Number(0.0)
                }));
            }
            compiler::OpCode::Call { id, params, ret } => {
                let index = match id {
                    compiler::CallId::Index(index) => *index,
                    compiler::CallId::Register(register_id) => {
                        if let Value::Function(index) = frame.registers[register_id.0] {
                            index
                        } else if let Value::Closure(closure) = &frame.registers[register_id.0] {
                            closure.function_id
                        } else {
                            panic!("Expected function index!");
                        }
                    }
                };

                let func = context.unit.functions[index].clone();

                // map parameters to (dest new callframe, src from current callframe)
                let dests = func
                    .parameters
                    .iter()
                    .map(|(_, place)| place.clone())
                    .collect::<Vec<RegisterId>>();
                let args = dests
                    .iter()
                    .map(|r| r.clone())
                    .zip(params.clone())
                    .collect::<Vec<(RegisterId, RegisterId)>>();

                // for dynamic calls, do argument matching check here
                if let CallId::Register(_) = id {
                    if func.parameters.len() != args.len() {
                        panic!(
                            "Unmatched parameters count on **dynamic** call to `{}`",
                            func.name
                        );
                    }
                }

                if let CallId::Register(reg) = id {
                    if let Value::Closure(closure) = &frame.registers[reg.0] {
                        return Ok(ExecuteStatus::CallClosure(closure.clone(), args, *ret));
                    }
                }

                return Ok(ExecuteStatus::Call(index, args, *ret));
            }
            compiler::OpCode::CallNative { id, ret, params } => {
                return Ok(ExecuteStatus::CallNative(
                    id.clone(),
                    params.clone(),
                    ret.clone(),
                ));
            }
            compiler::OpCode::LoadGlobal { dest, src } => {
                if let Some(value) = context.unit.globals.get(src) {
                    frame.registers[dest.0] = value.clone();
                } else {
                    panic!("No global!");
                }
            }
            compiler::OpCode::StoreGlobal { dest, src } => {
                context
                    .unit
                    .globals
                    .insert(dest.clone(), frame.registers[src.0].clone());
            }
            compiler::OpCode::Sub(args) => {
                Self::apply_binary_op(&mut frame.registers, &args, |left, right| left - right);
            }
            compiler::OpCode::Mul(args) => {
                Self::apply_binary_op(&mut frame.registers, &args, |left, right| left * right);
            }
            compiler::OpCode::Div(args) => {
                Self::apply_binary_op(&mut frame.registers, &args, |left, right| left / right);
            }
            compiler::OpCode::Equal(args) => {
                Self::apply_binary_op(&mut frame.registers, &args, |left, right| {
                    Ok(Value::Boolean(left == right))
                });
            }
            compiler::OpCode::Greater(args) => {
                Self::apply_binary_op(&mut frame.registers, &args, |left, right| {
                    Ok(Value::Boolean(left > right))
                });
            }
            compiler::OpCode::Lesser(args) => {
                Self::apply_binary_op(&mut frame.registers, &args, |left, right| {
                    Ok(Value::Boolean(left < right))
                });
            }
            compiler::OpCode::GreaterOrEqual(args) => {
                Self::apply_binary_op(&mut frame.registers, &args, |left, right| {
                    Ok(Value::Boolean(left >= right))
                });
            }
            compiler::OpCode::LesserOrEqual(args) => {
                Self::apply_binary_op(&mut frame.registers, &args, |left, right| {
                    Ok(Value::Boolean(left <= right))
                });
            }
            compiler::OpCode::And(args) => {
                Self::apply_binary_op(&mut frame.registers, &args, |left, right| {
                    if let (Value::Boolean(left), Value::Boolean(right)) = (left, right) {
                        return Ok(Value::Boolean(left && right));
                    }

                    return Err("Can't apply binary And operation to non boolean values.".into());
                });
            }
            compiler::OpCode::Or(args) => {
                Self::apply_binary_op(&mut frame.registers, &args, |left, right| {
                    if let (Value::Boolean(left), Value::Boolean(right)) = (left, right) {
                        return Ok(Value::Boolean(left || right));
                    }

                    return Err("Can't apply binary Or operation to non boolean values.".into());
                });
            }
            compiler::OpCode::Negate(unary_op_args) => {
                frame.registers[unary_op_args.dest.0] =
                    (-frame.registers[unary_op_args.right.0].clone())?;
            }
            compiler::OpCode::Not(unary_op_args) => {
                frame.registers[unary_op_args.dest.0] =
                    (!frame.registers[unary_op_args.right.0].clone())?;
            }
            compiler::OpCode::JumpIfFalse { cond, offset } => {
                if let Value::Boolean(val) = frame.registers[cond.0] {
                    if !val {
                        frame.ip += *offset as usize - 1;
                    }
                }
            }
            compiler::OpCode::Jump { offset } => {
                frame.ip += *offset as usize - 1;
            }
            compiler::OpCode::CreateClosure { dest, function_id } => {
                let prototype = &context.unit.functions[*function_id];
                let closure = Rc::new(Closure {
                    function_id: *function_id,
                    upvalues: prototype
                        .upvalues
                        .iter()
                        .map(|target| Upvalue::Open(target.clone()))
                        .collect(),
                });

                let frame = &mut context.call_stack[context.current_frame];
                frame.registers[dest.0] = Value::Closure(closure);
            }
            compiler::OpCode::GetUpvalue { dest, idx } => {
                let upvalue = &frame.upvalues[*idx].clone();
                match upvalue {
                    Upvalue::Open(upvalue_target) => {
                        let value = self.find_upvalue_in_callstack(context, upvalue_target);

                        let frame = &mut context.call_stack[context.current_frame];
                        frame.registers[dest.0] = value;
                    }
                    Upvalue::Closed(value_ref) => {
                        frame.registers[dest.0] = value_ref.borrow().clone();
                    }
                }
            }
            compiler::OpCode::SetUpvalue { src, idx } => {
                let upvalue = &frame.upvalues[*idx]; // Don't clone it
                match upvalue {
                    Upvalue::Open(upvalue_target) => {
                        let value = frame.registers[src.0].clone();
                        let current_idx = context.current_frame;
                        let target_idx = current_idx - upvalue_target.scope;
                        let register_idx = upvalue_target.idx.0;

                        if upvalue_target.scope > 0 {
                            let (lower_frames, _) = context.call_stack.split_at_mut(current_idx);

                            let target_frame = &mut lower_frames[target_idx];
                            target_frame.registers[register_idx] = value;
                        } else {
                            frame.registers[register_idx] = value;
                        }
                    }
                    Upvalue::Closed(value_ref) => {
                        let value = frame.registers[src.0].clone();
                        *value_ref.borrow_mut() = value; // Modify through RefCell
                    }
                }
            }
        }

        Ok(ExecuteStatus::Continue)
    }

    /// Returns a copy of the value!
    fn find_upvalue_in_callstack(
        &self,
        context: &ExecutionContext,
        target: &UpvalueTarget,
    ) -> Value {
        let frame = &context.call_stack[context.current_frame - target.scope];
        let declarations = &context.unit.functions[frame.prototype_id.0].declarations;
        let key = declarations
            .iter()
            .find_map(|(k, v)| if v == &target.idx { Some(k) } else { None });

        return frame.registers[target.idx.0].clone();
    }

    fn fetch_instruction(context: &mut ExecutionContext) -> Result<Instruction, String> {
        let frame = &mut context.call_stack[context.current_frame];
        let prototype = &context.unit.functions[frame.prototype_id.0];

        // instruction fetching
        if frame.ip >= prototype.code.len() {
            return Err(format!(
                "Reached end of function {} without a return statement!",
                prototype.name
            ));
        }

        let instruction = prototype.code[frame.ip].clone();
        frame.ip += 1;

        Ok(instruction)
    }

    pub fn run_named(&self, context: &mut ExecutionContext, name: &str) -> Result<Value, String> {
        // reset call stack
        context.call_stack.clear();
        context.current_frame = 0;

        // find function
        let prototype_id = if let Some(t) = context.unit.functions_map.get(name.into()) {
            *t
        } else {
            return Err(format!("Could not find function named {}", name));
        };

        let prototype = &context.unit.functions[prototype_id];

        context.call_stack.push(CallFrame {
            prototype_id: FunctionId(prototype_id),
            registers: vec![Value::Number(0.0); prototype.register_allocator.len()],
            upvalues: vec![],
            ip: 0,
            ret: RegisterId(0),
        });

        loop {
            let instruction = Self::fetch_instruction(context)?;

            // execute instruction
            match self.execute_instruction(instruction, context) {
                Ok(status) => {
                    let frame = &mut context.call_stack[context.current_frame];
                    match status {
                        ExecuteStatus::Continue => continue,
                        ExecuteStatus::Return(value) => {
                            // If its a closure, we MUST close its upvalue
                            let value = if let Value::Closure(closure_rc) = &value {
                                Value::Closure(Rc::new(Closure {
                                    function_id: closure_rc.function_id,
                                    upvalues: closure_rc
                                        .upvalues
                                        .clone()
                                        .iter()
                                        .map(|target| match target {
                                            Upvalue::Open(upvalue_target) => {
                                                let mut new_target = upvalue_target.clone();

                                                if new_target.scope == 1 {
                                                    new_target.scope -= 1;
                                                    Upvalue::Closed(Rc::new(RefCell::new(
                                                        self.find_upvalue_in_callstack(
                                                            &context,
                                                            &new_target,
                                                        ),
                                                    )))
                                                } else {
                                                    new_target.scope -= 1;
                                                    Upvalue::Open(new_target)
                                                }
                                            }
                                            Upvalue::Closed(_) => target.clone(),
                                        })
                                        .collect(),
                                }))
                            } else {
                                value
                            };

                            let frame = context.call_stack.pop();
                            if context.current_frame > 0 {
                                match frame {
                                    Some(frame) => {
                                        context.current_frame -= 1;
                                        context.call_stack[context.current_frame].registers
                                            [frame.ret.0] = value;
                                    }
                                    None => return Err("Underflow stack error!".into()),
                                }
                            } else {
                                return Ok(value);
                            }
                        }
                        ExecuteStatus::Call(prototype_id, params, ret) => {
                            let prototype = &context.unit.functions[prototype_id];
                            let mut call_frame = CallFrame {
                                prototype_id: FunctionId(prototype_id),
                                ip: 0,
                                registers: vec![Value::Number(0.0); prototype.register_allocator.len()],
                                upvalues: vec![],
                                ret,
                            };

                            for (dest, src) in params {
                                call_frame.registers[dest.0] = frame.registers[src.0].clone();
                            }

                            context.call_stack.push(call_frame);
                            context.current_frame += 1;
                        }
                        ExecuteStatus::CallNative(name, register_ids, ret) => {
                            let args = {
                                let frame = &context.call_stack[context.current_frame];
                                register_ids
                                    .iter()
                                    .map(|id| frame.registers[id.0].clone())
                                    .collect::<Vec<Value>>()
                            };

                            let result = self.native_registry.call(&name, context, args)?;

                            let frame = &mut context.call_stack[context.current_frame];
                            frame.registers[ret.0] = result;
                        }
                        ExecuteStatus::CallClosure(closure, params, ret) => {
                            let prototype = &context.unit.functions[closure.function_id];
                            let mut call_frame = CallFrame {
                                prototype_id: FunctionId(closure.function_id),
                                ip: 0,
                                registers: vec![Value::Number(0.0); prototype.register_allocator.len()],
                                upvalues: closure.upvalues.clone(),
                                ret,
                            };

                            for (dest, src) in params {
                                call_frame.registers[dest.0] = frame.registers[src.0].clone();
                            }

                            context.call_stack.push(call_frame);
                            context.current_frame += 1;
                        }
                    }
                }
                Err(_) => todo!(),
            }
        }
    }
}
