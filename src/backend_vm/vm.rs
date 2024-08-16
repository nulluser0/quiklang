// VM (Register-based)

use std::{
    collections::{HashMap, HashSet},
    hash::Hasher,
    process,
    rc::Rc,
};

use crate::errors::VMRuntimeError;

use std::hash::Hash;

use super::{
    bytecode::ByteCode,
    instructions::{
        get_arga, get_argb, get_argbx, get_argc, get_argsbx, get_opcode, is_k, rk_to_k,
        Instruction, OP_NOP,
    },
};

type VmHandler = fn(&mut VM, Instruction) -> Result<(), VMRuntimeError>;

#[derive(Debug, Clone, Copy)]
struct CallFrame {
    pub return_pc: usize, // PC to return to
    pub base: usize,      // Base register
}

// TODO: Consider using this:
//      Replace Rc<T> with *const T
//      As a result, carefully manage memory in the parsetime and compiletime.
//      In Parsetime, variables are dropped after their scope is dropped.
//      Example
//      block {
//          let x = [20]; // x is a pointer to a heap array.
//          block {
//              x.push(23); // x, an array, has its push method called. It modifies the heap array by accessing pointer.
//          }
//          // more stuff with x
//      } // x, a pointer to heap allocated array value, is dropped. It should be done by a "destructor" insstruction, which reads register's
//      //   pointer to the heap object, then destroys the heap object.
//      x.pop() // Parse error, x no longer exists (dropped) and cannot be accessed.
//
//      Issues. What about heap allocated objects in a heap allocated object (like an array)? How do we know all the references to that array don't exist?
//          Very temporary (bandaid-ahh) solution: pointers and concept of shared variables are not implemented. Theoretically, it should be fine to just
//          drop the "popped" object in the parent object.
//      When concepts of pointers/references/shared variables comes up, a solution is to ensure:
//          let A = B; where A is pointer/reference/share of heap object B (B would be represented as a heap pointer in vm runtime btw).
//          B must not be dropped/out of scope before A
//              let b = [123];
//                  block {
//                      let a = share b; // Valid, b does not go out of scope when a is alive.
//                  }
#[derive(Debug, Clone, PartialEq, Default)]
pub enum RegisterVal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(Rc<String>),
    Array(Rc<Vec<RegisterVal>>),
    Range(Rc<(RegisterVal, RegisterVal, bool)>),
    HashMap(Rc<HashMap<RegisterVal, RegisterVal>>),
    HashSet(Rc<HashSet<RegisterVal>>),
    #[default]
    Null,
}

impl std::fmt::Display for RegisterVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegisterVal::Int(int) => write!(f, "{:10}| {}", "integer", int),
            RegisterVal::Float(float) => write!(f, "{:10}| {}", "float", float),
            RegisterVal::Bool(boolean) => write!(f, "{:10}| {}", "bool", boolean),
            RegisterVal::Str(string) => write!(f, "{:10}| {}", "string", string),
            RegisterVal::Array(array) => write!(f, "{:10}| {:?}", "array", array),
            RegisterVal::Range(range) => write!(f, "{:10}| {:?}", "range", range),
            RegisterVal::HashMap(hashmap) => write!(f, "{:10}| {:?}", "hashmap", hashmap),
            RegisterVal::HashSet(hashset) => write!(f, "{:10}| {:?}", "hashset", hashset),
            RegisterVal::Null => write!(f, "{:10}| null", "null"),
        }
    }
}

// Manual implementations of certain traits
impl Eq for RegisterVal {}

impl Hash for RegisterVal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            RegisterVal::Int(val) => val.hash(state),
            RegisterVal::Float(val) => {
                let int_val: i64 = (*val).to_bits() as i64; // Convert to bits and hash as integer
                int_val.hash(state);
            }
            RegisterVal::Bool(val) => val.hash(state),
            RegisterVal::Str(val) => val.hash(state),
            RegisterVal::Null => 0_u8.hash(state),
            RegisterVal::Array(val) => val.hash(state),
            RegisterVal::Range(val) => val.hash(state),
            RegisterVal::HashMap(val) => {
                // Iterate over the entries and hash them
                for (key, value) in val.iter() {
                    key.hash(state);
                    value.hash(state);
                }
            }
            RegisterVal::HashSet(val) => {
                // Iterate over the items and hash them
                for item in val.iter() {
                    item.hash(state);
                }
            }
        }
    }
}

// impl PartialOrd for RegisterVal {
//     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//         match (self, other) {
//             (RegisterVal::Int(a), RegisterVal::Int(b)) => a.partial_cmp(b),
//             (RegisterVal::Float(a), RegisterVal::Float(b)) => a.partial_cmp(b),
//             (RegisterVal::Bool(a), RegisterVal::Bool(b)) => a.partial_cmp(b),
//             (RegisterVal::Str(a), RegisterVal::Str(b)) => a.partial_cmp(b),
//             (RegisterVal::Array(a), RegisterVal::Array(b)) => a.partial_cmp(b),
//             (RegisterVal::HashMap(a), RegisterVal::HashMap(b)) => a.len().partial_cmp(&b.len()),
//             (RegisterVal::HashSet(a), RegisterVal::HashSet(b)) => a.len().partial_cmp(&b.len()),
//             (RegisterVal::Null, RegisterVal::Null) => Some(Ordering::Equal),

//             // Comparisons between Int and Float
//             (RegisterVal::Int(a), RegisterVal::Float(b)) => (*a as f64).partial_cmp(b),
//             (RegisterVal::Float(a), RegisterVal::Int(b)) => a.partial_cmp(&(*b as f64)),

//             // Define the ordering between different types
//             (RegisterVal::Int(_), _) => Some(Ordering::Less),
//             (_, RegisterVal::Int(_)) => Some(Ordering::Greater),
//             (RegisterVal::Float(_), _) => Some(Ordering::Less),
//             (_, RegisterVal::Float(_)) => Some(Ordering::Greater),
//             (RegisterVal::Bool(_), _) => Some(Ordering::Less),
//             (_, RegisterVal::Bool(_)) => Some(Ordering::Greater),
//             (RegisterVal::Str(_), _) => Some(Ordering::Less),
//             (_, RegisterVal::Str(_)) => Some(Ordering::Greater),
//             (RegisterVal::Array(_), _) => Some(Ordering::Less),
//             (_, RegisterVal::Array(_)) => Some(Ordering::Greater),
//             (RegisterVal::HashMap(_), _) => Some(Ordering::Less),
//             (_, RegisterVal::HashMap(_)) => Some(Ordering::Greater),
//             (RegisterVal::HashSet(_), _) => Some(Ordering::Less),
//             (_, RegisterVal::HashSet(_)) => Some(Ordering::Greater),
//             (RegisterVal::Range(_), RegisterVal::Range(_)) => Some(Ordering::Equal),
//             (RegisterVal::Range(_), RegisterVal::Null) => Some(Ordering::Greater),
//             (RegisterVal::Null, RegisterVal::Range(_)) => Some(Ordering::Less),
//         }
//     }
// }

// A note about converting from bytecode to vm for strings specifically:
// A constant entry with a string has information we know: its lens and string content.
// Since the VM has a String pool for strings, the string would:
//      - Be added to the string pool, and
//      - Its pointer (either an index to a vec, or its raw pointer to the memory) added to the constant pool with the index as identified in the bytecode's constant pool.

const fn create_dispatch_table() -> [VmHandler; OP_NOP as usize + 1] {
    [
        VM::op_move,
        VM::op_loadconst,
        VM::op_loadbool,
        VM::op_loadnull,
        VM::op_add,
        VM::op_sub,
        VM::op_mul,
        VM::op_div,
        VM::op_mod,
        VM::op_pow,
        VM::op_not,
        VM::op_and,
        VM::op_or,
        VM::op_eq,
        VM::op_ne,
        VM::op_lt,
        VM::op_le,
        VM::op_gt,
        VM::op_ge,
        VM::op_jump,
        VM::op_jump_if_true,
        VM::op_jump_if_false,
        VM::op_call,
        VM::op_tailcall,
        VM::op_return,
        VM::op_inc,
        VM::op_dec,
        VM::op_bitand,
        VM::op_bitor,
        VM::op_bitxor,
        VM::op_shl,
        VM::op_shr,
        VM::op_concat,
        VM::op_destructor,
        VM::op_exit,
        VM::op_clone,
        VM::op_nop,
    ]
}

static DISPATCH_TABLE: [VmHandler; OP_NOP as usize + 1] = create_dispatch_table();

#[derive(Debug)]
pub struct VM {
    registers: [RegisterVal; 256],
    pub constant_pool: Vec<RegisterVal>,
    pub function_indexes: Vec<usize>,
    pub program_counter: usize,
    pub instructions: Vec<Instruction>,
    call_stack: [CallFrame; 1024], // Fixed-size array for stack allocation
    stack_pointer: usize,          // Points to the next free slot in the call stack
}

const ARRAY_REPEAT_VALUE: RegisterVal = RegisterVal::Null;
impl VM {
    pub fn new(
        instructions: Vec<Instruction>,
        constant_pool: Vec<RegisterVal>,
        function_indexes: Vec<usize>,
        _num_registers: usize,
    ) -> Self {
        VM {
            registers: [ARRAY_REPEAT_VALUE; 256],
            constant_pool,
            program_counter: 0,
            instructions,
            function_indexes,
            call_stack: [CallFrame {
                return_pc: 0,
                base: 0,
            }; 1024], // Initialize with default CallFrame
            stack_pointer: 0,
        }
    }

    fn push_call_frame(&mut self, frame: CallFrame) -> Result<(), VMRuntimeError> {
        if self.stack_pointer >= self.call_stack.len() {
            return Err(VMRuntimeError::StackOverflow);
        }
        self.call_stack[self.stack_pointer] = frame;
        self.stack_pointer += 1;
        Ok(())
    }

    fn pop_call_frame(&mut self) -> Result<CallFrame, VMRuntimeError> {
        if self.stack_pointer == 0 {
            return Err(VMRuntimeError::StackUnderflow);
        }
        self.stack_pointer -= 1;
        Ok(self.call_stack[self.stack_pointer])
    }

    #[inline]
    fn current_offset(&mut self) -> usize {
        if self.stack_pointer == 0 {
            return 0;
        }
        self.call_stack[self.stack_pointer - 1].base
    }

    // fn current_frame(&self) -> &CallFrame {
    //     &self.call_stack[self.stack_pointer - 1]
    // }

    pub fn from_bytecode(bytecode: ByteCode) -> Self {
        VM::new(
            bytecode.instructions().clone(),
            bytecode.constant_pool().clone(),
            bytecode
                .qlang_functions
                .iter()
                .map(|f| *f as usize)
                .collect::<Vec<usize>>()
                .clone(),
            *bytecode.register_count() as usize,
        )
    }

    // pub fn set_max_register(&mut self, num_registers: usize) {
    //     if num_registers > self.registers.len() {
    //         // Extend the registers vector with default values
    //         self.registers.resize(num_registers, RegisterVal::Null);
    //     } else {
    //         // Truncate the registers vector
    //         self.registers.truncate(num_registers);
    //     }
    // }

    #[inline(always)]
    fn fetch_instruction(&self) -> Instruction {
        self.instructions[self.program_counter]
    }

    // This on_err function unwinds the callstack, displaying the user the last 20
    // calls, and popping all of the frames on the stack.
    pub fn on_err_unwind_callstack(&mut self) {
        println!("Unwinding Callstack:");
        println!("Stack pointer is at: {}", self.stack_pointer);
        if self.stack_pointer > 20 {
            println!("Truncating callstack to last 20 calls...");
        }
        let mut count: usize = 0;
        while let Ok(call_frame) = self.pop_call_frame() {
            count += 1;
            if count <= 20 {
                println!(
                    "Return PC: {:6} | Base: {}",
                    call_frame.return_pc, call_frame.base
                )
            }
        }
        println!("END");
    }

    // This on_err function "bulldoses" (destructs) heap allocated objects to prevent memory leaking.
    // This is partically useful in cases when Rc is not used, and there are other processes (QLBPM) running.
    pub fn on_err_bulldoser(&mut self) {
        println!("Running memory bulldoser; destroying heap objects:");
        // TODO: This would run a destructor function (either OP_DESTRUCTOR or dedicated function)
        // to destroy all heap allocated objects.
        println!("END");
    }

    pub fn on_error_cleanup(&mut self) {
        self.on_err_unwind_callstack();
        println!();
        self.on_err_bulldoser();
    }

    pub fn execute(&mut self) -> Result<(), VMRuntimeError> {
        while self.program_counter < self.instructions.len() {
            // println!("{}", self.program_counter);
            // self.execute_instruction(self.fetch_instruction());
            let inst = self.fetch_instruction();
            let opcode = get_opcode(inst);
            DISPATCH_TABLE[opcode as usize](self, inst)?;
            self.program_counter += 1;
        }
        Ok(())
    }

    #[inline(always)]
    fn op_move(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let offset = self.current_offset();

        let value = std::mem::take(&mut self.registers[argb as usize + offset]);
        self.registers[arga as usize + offset] = value;

        Ok(())
    }

    #[inline(always)]
    fn op_loadconst(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argbx = get_argbx(inst);
        let offset = self.current_offset();

        let value = self.constant_pool[argbx as usize].clone();
        self.registers[arga as usize + offset] = value;

        Ok(())
    }

    #[inline(always)]
    fn op_loadbool(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        let value = if argb != 0 {
            RegisterVal::Bool(true)
        } else {
            RegisterVal::Bool(false)
        };
        self.registers[arga as usize + offset] = value;
        if argc != 0 {
            self.program_counter += 1;
        }

        Ok(())
    }

    #[inline(always)]
    fn op_loadnull(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let offset = self.current_offset();

        for i in arga as usize..=argb as usize {
            self.registers[i + offset] = RegisterVal::Null;
        }

        Ok(())
    }

    #[inline(always)]
    fn fetch_values(&self, argb: i32, argc: i32, offset: usize) -> (&RegisterVal, &RegisterVal) {
        let b_val = if is_k(argb) {
            &self.constant_pool[rk_to_k(argb) as usize]
        } else {
            &self.registers[argb as usize + offset]
        };
        let c_val = if is_k(argc) {
            &self.constant_pool[rk_to_k(argc) as usize]
        } else {
            &self.registers[argc as usize + offset]
        };
        (b_val, c_val)
    }

    #[inline(always)]
    fn op_add(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        self.perform_op(
            inst,
            |left, right| left.wrapping_add(right),
            |left, right| left + right,
        )
    }

    #[inline(always)]
    fn op_sub(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        self.perform_op(
            inst,
            |left, right| left.wrapping_sub(right),
            |left, right| left - right,
        )
    }

    #[inline(always)]
    fn op_mul(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        self.perform_op(
            inst,
            |left, right| left.wrapping_mul(right),
            |left, right| left * right,
        )
    }

    #[inline(always)]
    fn op_div(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        self.perform_op(
            inst,
            |left, right| left.wrapping_div(right),
            |left, right| left / right,
        )
    }

    #[inline(always)]
    fn op_mod(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        self.perform_op(
            inst,
            |left, right| left.wrapping_rem(right),
            |left, right| left % right,
        )
    }

    #[inline(always)]
    fn op_pow(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        self.perform_op(
            inst,
            |left, right| left.wrapping_pow(right as u32),
            |left, right| left.powf(right),
        )
    }

    #[inline(always)]
    fn perform_op<FInt, FFloat>(
        &mut self,
        inst: Instruction,
        int_op: FInt,
        float_op: FFloat,
    ) -> Result<(), VMRuntimeError>
    where
        FInt: FnOnce(i64, i64) -> i64,
        FFloat: FnOnce(f64, f64) -> f64,
    {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        let (b_val, c_val) = self.fetch_values(argb, argc, offset);

        let result = match (b_val, c_val) {
            (RegisterVal::Int(left), RegisterVal::Int(right)) => {
                RegisterVal::Int(int_op(*left, *right))
            }
            (RegisterVal::Float(left), RegisterVal::Float(right)) => {
                RegisterVal::Float(float_op(*left, *right))
            }
            (RegisterVal::Int(left), RegisterVal::Float(right)) => {
                RegisterVal::Float(float_op(*left as f64, *right))
            }
            (RegisterVal::Float(left), RegisterVal::Int(right)) => {
                RegisterVal::Float(float_op(*left, *right as f64))
            }
            _ => return Ok(()), // no-op for unsupported types
        };

        self.registers[arga as usize + offset] = result;

        Ok(())
    }

    #[inline(always)]
    fn op_not(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let offset = self.current_offset();

        if let RegisterVal::Int(value) = self.registers[argb as usize + offset] {
            self.registers[arga as usize + offset] = RegisterVal::Int(!value);
        }

        Ok(())
    }

    #[inline(always)]
    fn op_and(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        let b_val = if is_k(argb) {
            self.constant_pool[rk_to_k(argb) as usize].clone()
        } else {
            self.registers[argb as usize + offset].clone()
        };
        let c_val = if is_k(argc) {
            self.constant_pool[rk_to_k(argc) as usize].clone()
        } else {
            self.registers[argc as usize + offset].clone()
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.registers[arga as usize + offset] = RegisterVal::Int(left & right);
        }

        Ok(())
    }

    #[inline(always)]
    fn op_or(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        let b_val = if is_k(argb) {
            self.constant_pool[rk_to_k(argb) as usize].clone()
        } else {
            self.registers[argb as usize + offset].clone()
        };
        let c_val = if is_k(argc) {
            self.constant_pool[rk_to_k(argc) as usize].clone()
        } else {
            self.registers[argc as usize + offset].clone()
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.registers[arga as usize + offset] = RegisterVal::Int(left | right);
        }

        Ok(())
    }

    #[inline(always)]
    fn perform_comparison<F>(&mut self, inst: Instruction, comp: F) -> Result<(), VMRuntimeError>
    where
        F: FnOnce(&RegisterVal, &RegisterVal) -> bool,
    {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        let (b_val, c_val) = self.fetch_values(argb, argc, offset);

        self.registers[arga as usize + offset] = RegisterVal::Bool(comp(b_val, c_val));

        Ok(())
    }

    #[inline(always)]
    fn op_eq(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        self.perform_comparison(inst, |b, c| b == c)
    }

    #[inline(always)]
    fn op_ne(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        self.perform_comparison(inst, |b, c| b != c)
    }

    #[inline(always)]
    fn op_lt(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        self.perform_comparison(inst, |b, c| match (b, c) {
            (RegisterVal::Int(b), RegisterVal::Int(c)) => b < c,
            (RegisterVal::Float(b), RegisterVal::Float(c)) => b < c,
            (RegisterVal::Int(b), RegisterVal::Float(c)) => (*b as f64) < *c,
            (RegisterVal::Float(b), RegisterVal::Int(c)) => b < &(*c as f64),
            _ => false,
        })
    }

    #[inline(always)]
    fn op_le(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        self.perform_comparison(inst, |b, c| match (b, c) {
            (RegisterVal::Int(b), RegisterVal::Int(c)) => b <= c,
            (RegisterVal::Float(b), RegisterVal::Float(c)) => b <= c,
            (RegisterVal::Int(b), RegisterVal::Float(c)) => (*b as f64) <= *c,
            (RegisterVal::Float(b), RegisterVal::Int(c)) => b <= &(*c as f64),
            _ => false,
        })
    }

    #[inline(always)]
    fn op_gt(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        self.perform_comparison(inst, |b, c| match (b, c) {
            (RegisterVal::Int(b), RegisterVal::Int(c)) => b > c,
            (RegisterVal::Float(b), RegisterVal::Float(c)) => b > c,
            (RegisterVal::Int(b), RegisterVal::Float(c)) => (*b as f64) > *c,
            (RegisterVal::Float(b), RegisterVal::Int(c)) => b > &(*c as f64),
            _ => false,
        })
    }

    #[inline(always)]
    fn op_ge(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        self.perform_comparison(inst, |b, c| match (b, c) {
            (RegisterVal::Int(b), RegisterVal::Int(c)) => b >= c,
            (RegisterVal::Float(b), RegisterVal::Float(c)) => b >= c,
            (RegisterVal::Int(b), RegisterVal::Float(c)) => (*b as f64) >= *c,
            (RegisterVal::Float(b), RegisterVal::Int(c)) => b >= &(*c as f64),
            _ => false,
        })
    }

    #[inline(always)]
    fn op_jump(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let argsbx = get_argsbx(inst);

        self.program_counter = (self.program_counter as i32 + argsbx) as usize;

        Ok(())
    }

    #[inline(always)]
    fn op_jump_if_true(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argsbx = get_argsbx(inst);

        if let RegisterVal::Bool(true) = self.registers[arga as usize] {
            self.program_counter = (self.program_counter as i32 + argsbx) as usize;
        }

        Ok(())
    }

    #[inline(always)]
    fn op_jump_if_false(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argsbx = get_argsbx(inst);
        let offset = self.current_offset();

        if let RegisterVal::Bool(false) = self.registers[arga as usize + offset] {
            self.program_counter = (self.program_counter as i32 + argsbx) as usize;
        }

        Ok(())
    }

    #[inline(always)]
    fn op_call(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        // let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        // A is the pointer to a function on function_indexes
        // B is the number of args to be pushed into the function's scope
        // C is the base

        // Save callframe
        let call_frame = CallFrame {
            return_pc: self.program_counter,
            base: (argc as usize) + offset,
        };

        // push callframe
        self.push_call_frame(call_frame)?;

        // Set the program counter to the function's starting instruction
        if (arga as usize) < self.function_indexes.len() {
            self.program_counter = self.function_indexes[arga as usize];
            // return; TODO: Fix to possibly avoid pc --1
            self.program_counter -= 1;
        } else {
            panic!("Invalid function index: {}", arga);
        }

        Ok(())
    }

    #[inline(always)]
    fn op_tailcall(&mut self, _inst: Instruction) -> Result<(), VMRuntimeError> {
        // TODO!
        todo!()
    }

    #[inline(always)]
    fn op_return(&mut self, _inst: Instruction) -> Result<(), VMRuntimeError> {
        self.program_counter = self.pop_call_frame()?.return_pc;

        Ok(())
    }

    #[inline(always)]
    fn op_inc(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);

        if let RegisterVal::Int(value) = self.registers[arga as usize] {
            self.registers[arga as usize] = RegisterVal::Int(value.wrapping_add(1));
        }

        Ok(())
    }

    #[inline(always)]
    fn op_dec(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);

        if let RegisterVal::Int(value) = self.registers[arga as usize] {
            self.registers[arga as usize] = RegisterVal::Int(value.wrapping_sub(1));
        }

        Ok(())
    }

    #[inline(always)]
    fn op_bitand(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        let b_val = if is_k(argb) {
            self.constant_pool[rk_to_k(argb) as usize].clone()
        } else {
            self.registers[argb as usize + offset].clone()
        };
        let c_val = if is_k(argc) {
            self.constant_pool[rk_to_k(argc) as usize].clone()
        } else {
            self.registers[argc as usize].clone()
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.registers[arga as usize] = RegisterVal::Int(left & right);
        }

        Ok(())
    }

    #[inline(always)]
    fn op_bitor(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        let b_val = if is_k(argb) {
            self.constant_pool[rk_to_k(argb) as usize].clone()
        } else {
            self.registers[argb as usize + offset].clone()
        };
        let c_val = if is_k(argc) {
            self.constant_pool[rk_to_k(argc) as usize].clone()
        } else {
            self.registers[argc as usize].clone()
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.registers[arga as usize] = RegisterVal::Int(left | right);
        }

        Ok(())
    }

    #[inline(always)]
    fn op_bitxor(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        let b_val = if is_k(argb) {
            self.constant_pool[rk_to_k(argb) as usize].clone()
        } else {
            self.registers[argb as usize + offset].clone()
        };
        let c_val = if is_k(argc) {
            self.constant_pool[rk_to_k(argc) as usize].clone()
        } else {
            self.registers[argc as usize].clone()
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.registers[arga as usize] = RegisterVal::Int(left ^ right);
        }

        Ok(())
    }

    #[inline(always)]
    fn op_shl(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        let b_val = if is_k(argb) {
            self.constant_pool[rk_to_k(argb) as usize].clone()
        } else {
            self.registers[argb as usize + offset].clone()
        };
        let c_val = if is_k(argc) {
            self.constant_pool[rk_to_k(argc) as usize].clone()
        } else {
            self.registers[argc as usize].clone()
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.registers[arga as usize] = RegisterVal::Int(left << right);
        }

        Ok(())
    }

    #[inline(always)]
    fn op_shr(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        let b_val = if is_k(argb) {
            self.constant_pool[rk_to_k(argb) as usize].clone()
        } else {
            self.registers[argb as usize + offset].clone()
        };
        let c_val = if is_k(argc) {
            self.constant_pool[rk_to_k(argc) as usize].clone()
        } else {
            self.registers[argc as usize].clone()
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.registers[arga as usize] = RegisterVal::Int(left >> right);
        }

        Ok(())
    }

    #[inline(always)]
    fn op_concat(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        let b_val = if is_k(argb) {
            self.constant_pool[rk_to_k(argb) as usize].clone()
        } else {
            self.registers[argb as usize + offset].clone()
        };
        let c_val = if is_k(argc) {
            self.constant_pool[rk_to_k(argc) as usize].clone()
        } else {
            self.registers[argc as usize].clone()
        };

        if let (RegisterVal::Str(left), RegisterVal::Str(right)) = (b_val, c_val) {
            let concatenated = format!("{}{}", left, right);
            self.registers[arga as usize] = RegisterVal::Str(Rc::new(concatenated));
        }

        Ok(())
    }

    #[inline(always)]
    fn op_destructor(&mut self, _inst: Instruction) -> Result<(), VMRuntimeError> {
        // destroy heap objs, where A is a pointer to the heap obj flagged for destruction.
        // TODO!
        todo!()
    }

    #[inline(always)]
    fn op_exit(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);

        process::exit(arga)
    }

    #[inline(always)]
    fn op_clone(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let offset = self.current_offset();

        let value = self.registers[argb as usize + offset].clone();
        self.registers[arga as usize + offset] = value;

        Ok(())
    }

    #[inline(always)]
    fn op_nop(&mut self, _inst: Instruction) -> Result<(), VMRuntimeError> {
        // here only to waste cycles lol
        Ok(())
    }
}
