// VM (Register-based)

use std::{
    collections::{HashMap, HashSet},
    hash::Hasher,
    sync::Arc,
};

use tokio::sync::{oneshot, Mutex};

use crate::errors::VMRuntimeError;

use std::hash::Hash;

use super::{
    bytecode::ByteCode,
    instructions::{
        get_arga, get_argb, get_argbx, get_argc, get_argsbx, get_opcode, is_k, rk_to_k,
        Instruction, OP_NOP,
    },
    qffi::QFFI,
};

type VmHandler = fn(&mut VMThread, Instruction) -> Result<(), VMRuntimeError>;

#[repr(C, align(2))]
#[derive(Debug, Clone, Copy)]
struct CallFrame {
    pub return_pc: u32, // PC to return to
    pub base: u32,      // Base register
}

// TODO: Consider using this:
//      Replace Rc<T> with *const T (raw pointer) for performance reasons.
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

// TODO: Consider using this:
//      All the types are guaranteed at compile time, so we can just use a untagged union.
//      This would be a lot more efficient than using an enum.
//      There is forseeable performance benefits, it would be 8 bytes (instead of the enum's 16 bytes) for a 64-bit system.
//      The downside is that there is more emphasis on compile-time safety.
//      Also, more instructions would be needed since they would determine the type of the value.
#[repr(C, align(4))]
#[derive(Debug, Clone, PartialEq, Default)]
pub enum RegisterVal {
    #[default]
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(Arc<String>),
    Array(Arc<Vec<RegisterVal>>),
    Range(Arc<(RegisterVal, RegisterVal, bool)>),
    HashMap(Arc<HashMap<RegisterVal, RegisterVal>>),
    HashSet(Arc<HashSet<RegisterVal>>),
}

// pub union RegisterVal {
//     pub null: (), // Nulls, ints, floats, and bools are all primitives, so they are stored directly in the union.
//     pub int: i64,
//     pub float: f64,
//     pub bool: bool,
//     pub ptr: *const u8, // Universal pointer to heap allocated object. Unsafe, so requires a destructor and a brain to manage memory.
// }

impl std::fmt::Display for RegisterVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegisterVal::Null => write!(f, "null"),
            RegisterVal::Int(int) => write!(f, "{}", int),
            RegisterVal::Float(float) => write!(f, "{}", float),
            RegisterVal::Bool(boolean) => write!(f, "{}", boolean),
            RegisterVal::Str(string) => write!(f, "{}", string),
            RegisterVal::Array(array) => write!(f, "{:?}", array),
            RegisterVal::Range(range) => write!(f, "{:?}", range),
            RegisterVal::HashMap(hashmap) => write!(f, "{:?}", hashmap),
            RegisterVal::HashSet(hashset) => write!(f, "{:?}", hashset),
        }
    }
}

pub fn to_quiklangc_strings(inner: &RegisterVal) -> String {
    match inner {
        RegisterVal::Null => format!("{:10}| null", "null"),
        RegisterVal::Int(int) => format!("{:10}| {}", "integer", int),
        RegisterVal::Float(float) => format!("{:10}| {}", "float", float),
        RegisterVal::Bool(boolean) => format!("{:10}| {}", "bool", boolean),
        RegisterVal::Str(string) => format!("{:10}| {}", "string", string),
        RegisterVal::Array(array) => format!("{:10}| {:?}", "array", array),
        RegisterVal::Range(range) => format!("{:10}| {:?}", "range", range),
        RegisterVal::HashMap(hashmap) => format!("{:10}| {:?}", "hashmap", hashmap),
        RegisterVal::HashSet(hashset) => format!("{:10}| {:?}", "hashset", hashset),
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

// A note about converting from bytecode to vm for strings specifically:
// A constant entry with a string has information we know: its lens and string content.
// Since the VM has a String pool for strings, the string would:
//      - Be added to the string pool, and
//      - Its pointer (either an index to a vec, or its raw pointer to the memory) added to the constant pool with the index as identified in the bytecode's constant pool.

const fn create_dispatch_table() -> [VmHandler; OP_NOP as usize + 1] {
    [
        VMThread::op_move,
        VMThread::op_loadconst,
        VMThread::op_loadbool,
        VMThread::op_loadnull,
        VMThread::op_add,
        VMThread::op_sub,
        VMThread::op_mul,
        VMThread::op_div,
        VMThread::op_mod,
        VMThread::op_pow,
        VMThread::op_not,
        VMThread::op_and,
        VMThread::op_or,
        VMThread::op_eq,
        VMThread::op_ne,
        VMThread::op_lt,
        VMThread::op_le,
        VMThread::op_gt,
        VMThread::op_ge,
        VMThread::op_jump,
        VMThread::op_jump_if_true,
        VMThread::op_jump_if_false,
        VMThread::op_call,
        VMThread::op_native_call,
        VMThread::op_qffi_call,
        VMThread::op_tailcall,
        VMThread::op_return,
        VMThread::op_inc,
        VMThread::op_dec,
        VMThread::op_bitand,
        VMThread::op_bitor,
        VMThread::op_bitxor,
        VMThread::op_shl,
        VMThread::op_shr,
        VMThread::op_concat,
        VMThread::op_destructor,
        VMThread::op_exit,
        VMThread::op_clone,
        VMThread::op_nop,
    ]
}

static DISPATCH_TABLE: [VmHandler; OP_NOP as usize + 1] = create_dispatch_table();

// The global state, which is the VM itself. Holds immutable, shared data, which local states (VMThread) can access.
#[derive(Debug)]
#[repr(C)]
pub struct VM {
    constant_pool: Vec<RegisterVal>, // Constant pool
    function_indexes: Vec<usize>,    // Function indexes
    instructions: Vec<Instruction>,  // Instructions
    qffi: QFFI,                      // QFFI instance
    threads: Mutex<HashMap<usize, tokio::task::JoinHandle<()>>>, // Active threads
    thread_id_counter: Mutex<usize>, // For assigning unique thread IDs
                                     // TODO: Add a symbol table for global variables
                                     // TODO: MPSC channel for inter-thread communication
}

pub struct VMThread {
    thread_id: usize,              // Thread ID
    program_counter: usize,        // Local PC
    vm: Arc<VM>,                   // Global state (VM)
    registers: Box<[RegisterVal]>, // Local registers
    call_stack: Box<[CallFrame]>,  // Local call stack
    call_stack_pointer: usize,     // Local stack pointer
}

const ARRAY_REPEAT_VALUE: RegisterVal = RegisterVal::Null;

impl VM {
    // Create a new VM instance
    pub fn new(
        instructions: Vec<Instruction>,
        constant_pool: Vec<RegisterVal>,
        function_indexes: Vec<usize>,
        // main_fn_max_reg: usize,
    ) -> Self {
        VM {
            constant_pool,
            instructions,
            function_indexes,
            qffi: QFFI::new(),
            threads: Mutex::new(HashMap::new()),
            thread_id_counter: Mutex::new(0),
        }
    }

    // Create a new VM instance from a bytecode
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
            // *bytecode.register_count() as usize,
        )
    }

    // Async API to spawn a new thread (VMThread) and execute it
    #[inline(always)]
    pub async fn spawn_thread(&mut self, mut vm_thread: VMThread) -> Result<usize, VMRuntimeError> {
        let (tx, rx) = oneshot::channel();

        // TODO: Optimize this, as it is not efficient to lock the mutex every time a thread is spawned
        let thread_id = {
            let mut counter = self.thread_id_counter.lock().await;
            *counter += 1;
            *counter
        };

        vm_thread.thread_id = thread_id;

        let handle = tokio::spawn(async move {
            let result = vm_thread.execute().await; // Execute thread's instructions
            let _ = tx.send(result); // Send result back to parent
        });

        // Store thread in the thread manager
        self.threads.lock().await.insert(thread_id, handle);

        Ok(thread_id)
    }

    // Async API to check if a thread has finished and get the result
    #[inline(always)]
    pub async fn join_thread(&mut self, thread_id: usize) -> Result<(), VMRuntimeError> {
        if let Some(handle) = self.threads.lock().await.remove(&thread_id) {
            handle
                .await
                .map_err(|e| VMRuntimeError::ThreadJoinError(thread_id, e))?;
            Ok(())
        } else {
            Err(VMRuntimeError::InvalidThreadId(thread_id))
        }
    }

    // API to fetch a constant from the constant pool
    #[inline(always)]
    pub fn get_constant(&self, index: usize) -> Option<&RegisterVal> {
        self.constant_pool.get(index)
    }

    // API to fetch constant pool len
    #[inline(always)]
    pub fn get_constant_pool_len(&self) -> usize {
        self.constant_pool.len()
    }

    // API to fetch an instruction from the instruction pool
    #[inline(always)]
    pub fn get_instruction(&self, pc: usize) -> Option<&Instruction> {
        self.instructions.get(pc)
    }

    // API to fetch instruction pool len
    #[inline(always)]
    pub fn get_instruction_len(&self) -> usize {
        self.instructions.len()
    }

    // API to access Native QFFI functions
    #[inline(always)]
    pub fn call_qffi_native(
        &self,
        index: usize,
        args: &[RegisterVal],
    ) -> Result<RegisterVal, VMRuntimeError> {
        self.qffi.call_native_function(index, args)
    }

    // API to access Extern QFFI functions
    #[inline(always)]
    pub fn call_qffi_extern(
        &self,
        index: usize,
        args: &[RegisterVal],
    ) -> Result<RegisterVal, VMRuntimeError> {
        self.qffi.call_extern_function(index, args)
    }

    // API to fetch function index
    #[inline(always)]
    pub fn get_function_index(&self, fn_id: usize) -> Option<&usize> {
        self.function_indexes.get(fn_id)
    }

    // API to fetch function index len
    #[inline(always)]
    pub fn get_function_index_len(&self) -> usize {
        self.function_indexes.len()
    }

    // Async API to process exit code
    // We need to terminate all threads.
    // Propagating the exit code to the caller of VM.execute() is done by the thread that called this function.
    #[inline(always)]
    pub async fn process_exit(&self) {
        // Terminate all threads
        for (_, handle) in self.threads.lock().await.drain() {
            handle.abort();
        }
    }

    // Execute the VM
    // This function simply initializes a new VMThread and executes it.
    pub async fn execute(self) -> Result<RegisterVal, VMRuntimeError> {
        let mut vm_thread = VMThread::new(Arc::new(self));
        vm_thread.execute().await
    }
}

impl VMThread {
    pub fn new(vm: Arc<VM>) -> Self {
        VMThread {
            thread_id: 0,
            program_counter: 0,
            vm,
            registers: Box::new([ARRAY_REPEAT_VALUE; 20000]),
            call_stack: Box::new(
                [CallFrame {
                    return_pc: 0,
                    base: 0,
                }; 30000],
            ),
            call_stack_pointer: 0,
        }
    }

    fn push_call_frame(&mut self, frame: CallFrame) -> Result<(), VMRuntimeError> {
        // self.call_stack.push(frame);
        if self.call_stack_pointer >= self.call_stack.len() {
            return Err(VMRuntimeError::StackOverflow);
        }
        self.call_stack[self.call_stack_pointer] = frame;
        self.call_stack_pointer += 1;
        Ok(())
    }

    fn pop_call_frame(&mut self) -> Result<CallFrame, VMRuntimeError> {
        if self.call_stack_pointer == 0 {
            return Err(VMRuntimeError::StackUnderflow);
        }
        // let frame = self.call_stack.pop().unwrap();
        let frame = self.call_stack[self.call_stack_pointer - 1];
        self.call_stack_pointer -= 1;
        Ok(frame)
    }

    #[inline]
    fn current_offset(&mut self) -> usize {
        if self.call_stack_pointer == 0 {
            return 0;
        }
        self.call_stack[self.call_stack_pointer - 1].base as usize
    }

    // fn current_frame(&self) -> &CallFrame {
    //     &self.call_stack[self.stack_pointer - 1]
    // }

    #[inline(always)]
    fn fetch_instruction(&self) -> Instruction {
        // self.instructions[self.program_counter]
        *self.vm.get_instruction(self.program_counter).unwrap()
    }

    // This on_err function unwinds the callstack, displaying the user the last 20
    // calls, and popping all of the frames on the stack.
    pub fn on_err_unwind_callstack(&mut self) {
        println!("Unwinding Callstack:");
        println!("Stack pointer is at: {}", self.call_stack_pointer);
        if self.call_stack_pointer > 41 {
            println!("Truncating callstack to first and last 20 calls...");
        }
        let mut count: usize = 0;
        let mut reverse_count: usize = self.call_stack_pointer;
        while let Ok(call_frame) = self.pop_call_frame() {
            count += 1;
            if count <= 20 {
                println!(
                    "Return PC: {:6} | Base: {}",
                    call_frame.return_pc, call_frame.base
                )
            }

            if reverse_count == 21 {
                println!("...");
            }

            if reverse_count <= 20 {
                println!(
                    "Return PC: {:6} | Base: {}",
                    call_frame.return_pc, call_frame.base
                )
            }
            reverse_count -= 1;
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

    #[inline(always)]
    fn set_register(&mut self, register: usize, value: RegisterVal) -> Result<(), VMRuntimeError> {
        // When tinyvec is used, this will allocate memory for the register.
        let offset = self.current_offset();
        if register + offset >= self.registers.len() {
            // self.registers
            //     .resize(register + offset + 1, RegisterVal::Null);
            return Err(VMRuntimeError::InvalidRegisterAccess(register + offset));
        }
        self.registers[register + offset] = value;
        Ok(())
    }

    #[inline(always)]
    fn get_register_mutref(&mut self, register: usize) -> Result<&mut RegisterVal, VMRuntimeError> {
        let offset = self.current_offset();
        if register + offset >= self.registers.len() {
            return Err(VMRuntimeError::InvalidRegisterAccess(register + offset));
        }
        Ok(&mut self.registers[register + offset])
    }

    #[inline(always)]
    fn get_register_ref(
        &self,
        register: usize,
        offset: usize,
    ) -> Result<&RegisterVal, VMRuntimeError> {
        if register + offset >= self.registers.len() {
            return Err(VMRuntimeError::InvalidRegisterAccess(register + offset));
        }
        Ok(&self.registers[register + offset])
    }

    #[inline(always)]
    fn get_register_clone(&mut self, register: usize) -> Result<RegisterVal, VMRuntimeError> {
        let offset = self.current_offset();
        if register + offset >= self.registers.len() {
            return Err(VMRuntimeError::InvalidRegisterAccess(register + offset));
        }
        Ok(self.registers[register + offset].clone())
    }

    #[inline(always)]
    fn get_constant_clone(&mut self, index: usize) -> Result<RegisterVal, VMRuntimeError> {
        self.vm
            .get_constant(index)
            .cloned()
            .ok_or(VMRuntimeError::InvalidConstantAccess(index))
    }

    #[inline(always)]
    fn get_constant_ref(&self, index: usize) -> Result<&RegisterVal, VMRuntimeError> {
        self.vm
            .get_constant(index)
            .ok_or(VMRuntimeError::InvalidConstantAccess(index))
    }

    #[inline(always)]
    fn on_thread_failure(&mut self, error: VMRuntimeError) -> VMRuntimeError {
        println!("Thread ID: {} failed with error: {}", self.thread_id, error);
        self.on_error_cleanup();
        error
    }

    pub async fn execute(&mut self) -> Result<RegisterVal, VMRuntimeError> {
        // The result is typically register[0], which is the return value of the function. This is important for async functions.
        let inst_len = self.vm.get_instruction_len();
        while self.program_counter < inst_len {
            let inst = self.fetch_instruction();
            let opcode = get_opcode(inst);
            if opcode as usize >= DISPATCH_TABLE.len() {
                return Err(VMRuntimeError::InvalidOpcode(opcode, self.program_counter));
            }
            match DISPATCH_TABLE[opcode as usize](self, inst) {
                // The instruction was successful, so we continue to the next instruction
                Ok(_) => {}
                // Process exited, so we need to interrupt the execution and return the exit code
                Err(VMRuntimeError::Exit(code)) => {
                    self.vm.process_exit().await;
                    return Err(VMRuntimeError::Exit(code));
                }
                // The thread failed, so we should unwind the callstack and clean up the memory
                Err(e) => return Err(self.on_thread_failure(e)),
            };
            self.program_counter += 1;
        }
        Ok(self.registers.first().unwrap_or(&RegisterVal::Null).clone())
    }

    #[inline(always)]
    fn op_move(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);

        let value = std::mem::take(self.get_register_mutref(argb as usize)?);
        self.set_register(arga as usize, value)?;

        Ok(())
    }

    #[inline(always)]
    fn op_loadconst(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argbx = get_argbx(inst);

        let value = self.get_constant_clone(argbx as usize)?;
        self.set_register(arga as usize, value)?;

        Ok(())
    }

    #[inline(always)]
    fn op_loadbool(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);

        let value = if argb != 0 {
            RegisterVal::Bool(true)
        } else {
            RegisterVal::Bool(false)
        };
        self.set_register(arga as usize, value)?;
        if argc != 0 {
            self.program_counter += 1;
        }

        Ok(())
    }

    #[inline(always)]
    fn op_loadnull(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);

        for i in arga as usize..=argb as usize {
            self.set_register(i, RegisterVal::Null)?;
        }

        Ok(())
    }

    #[inline(always)]
    fn fetch_values(
        &self,
        argb: i32,
        argc: i32,
        offset: usize,
    ) -> Result<(&RegisterVal, &RegisterVal), VMRuntimeError> {
        let b_val = if is_k(argb) {
            self.get_constant_ref(rk_to_k(argb) as usize)?
        } else {
            self.get_register_ref(argb as usize, offset)?
        };
        let c_val = if is_k(argc) {
            self.get_constant_ref(rk_to_k(argc) as usize)?
        } else {
            self.get_register_ref(argc as usize, offset)?
        };
        Ok((b_val, c_val))
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

        let (b_val, c_val) = self.fetch_values(argb, argc, offset)?;

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

        self.set_register(arga as usize, result)?;

        Ok(())
    }

    #[inline(always)]
    fn op_not(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let offset = self.current_offset();

        if let RegisterVal::Int(value) = self.registers[argb as usize + offset] {
            self.registers[arga as usize + offset] = RegisterVal::Int(!value);
            self.set_register(arga as usize, RegisterVal::Int(!value))?;
        }

        Ok(())
    }

    #[inline(always)]
    fn op_and(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);

        let b_val = if is_k(argb) {
            self.get_constant_clone(rk_to_k(argb) as usize)?
        } else {
            self.get_register_clone(argb as usize)?
        };
        let c_val = if is_k(argc) {
            self.get_constant_clone(rk_to_k(argc) as usize)?
        } else {
            self.get_register_clone(argc as usize)?
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.set_register(arga as usize, RegisterVal::Int(left & right))?;
        }

        Ok(())
    }

    #[inline(always)]
    fn op_or(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);

        let b_val = if is_k(argb) {
            self.get_constant_clone(rk_to_k(argb) as usize)?
        } else {
            self.get_register_clone(argb as usize)?
        };
        let c_val = if is_k(argc) {
            self.get_constant_clone(rk_to_k(argc) as usize)?
        } else {
            self.get_register_clone(argc as usize)?
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.set_register(arga as usize, RegisterVal::Int(left | right))?;
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

        let (b_val, c_val) = self.fetch_values(argb, argc, offset)?;

        self.set_register(arga as usize, RegisterVal::Bool(comp(b_val, c_val)))?;

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

        if let RegisterVal::Bool(false) = self.get_register_mutref(arga as usize)? {
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
            return_pc: self.program_counter as u32,
            base: (argc as u32) + offset as u32,
        };

        // push callframe
        self.push_call_frame(call_frame)?;

        // Set the program counter to the function's starting instruction
        if (arga as usize) < self.vm.get_function_index_len() {
            self.program_counter = *self.vm.get_function_index(arga as usize).unwrap();
            // return; TODO: Fix to possibly avoid pc --1
            self.program_counter -= 1;
        } else {
            panic!("Invalid function index: {}", arga);
        }

        Ok(())
    }

    #[inline(always)]
    fn op_native_call(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let offset = self.current_offset();

        // Native calls rely on the QFFI module.
        // Args = (offset + argc + 1) to (offset + argb + argc)
        let mut args = Vec::with_capacity(argc as usize + 1);
        for i in ((argc as usize) + offset + 1)..=(((argc + argb) as usize) + offset) {
            args.push(self.registers[i].clone());
        }

        self.vm.call_qffi_native(arga as usize, &args)?;
        Ok(())
    }

    #[inline(always)]
    fn op_qffi_call(&mut self, _inst: Instruction) -> Result<(), VMRuntimeError> {
        todo!()
    }

    #[inline(always)]
    fn op_tailcall(&mut self, _inst: Instruction) -> Result<(), VMRuntimeError> {
        // TODO!
        todo!()
    }

    #[inline(always)]
    fn op_return(&mut self, _inst: Instruction) -> Result<(), VMRuntimeError> {
        self.program_counter = self.pop_call_frame()?.return_pc as usize;

        Ok(())
    }

    #[inline(always)]
    fn op_inc(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let offset = self.current_offset();

        if let RegisterVal::Int(value) = self.get_register_ref(arga as usize, offset)? {
            self.set_register(arga as usize, RegisterVal::Int(value.wrapping_add(1)))?;
        }

        Ok(())
    }

    #[inline(always)]
    fn op_dec(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let offset = self.current_offset();

        if let RegisterVal::Int(value) = self.get_register_ref(arga as usize, offset)? {
            self.set_register(arga as usize, RegisterVal::Int(value.wrapping_sub(1)))?;
        }

        Ok(())
    }

    #[inline(always)]
    fn op_bitand(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);

        let b_val = if is_k(argb) {
            self.get_constant_clone(rk_to_k(argb) as usize)?
        } else {
            self.get_register_clone(argb as usize)?
        };
        let c_val = if is_k(argc) {
            self.get_constant_clone(rk_to_k(argc) as usize)?
        } else {
            self.get_register_clone(argc as usize)?
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.set_register(arga as usize, RegisterVal::Int(left & right))?
        }

        Ok(())
    }

    #[inline(always)]
    fn op_bitor(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);

        let b_val = if is_k(argb) {
            self.get_constant_clone(rk_to_k(argb) as usize)?
        } else {
            self.get_register_clone(argb as usize)?
        };
        let c_val = if is_k(argc) {
            self.get_constant_clone(rk_to_k(argc) as usize)?
        } else {
            self.get_register_clone(argc as usize)?
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.set_register(arga as usize, RegisterVal::Int(left | right))?
        }

        Ok(())
    }

    #[inline(always)]
    fn op_bitxor(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);

        let b_val = if is_k(argb) {
            self.get_constant_clone(rk_to_k(argb) as usize)?
        } else {
            self.get_register_clone(argb as usize)?
        };
        let c_val = if is_k(argc) {
            self.get_constant_clone(rk_to_k(argc) as usize)?
        } else {
            self.get_register_clone(argc as usize)?
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.set_register(arga as usize, RegisterVal::Int(left ^ right))?
        }

        Ok(())
    }

    #[inline(always)]
    fn op_shl(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);

        let b_val = if is_k(argb) {
            self.get_constant_clone(rk_to_k(argb) as usize)?
        } else {
            self.get_register_clone(argb as usize)?
        };
        let c_val = if is_k(argc) {
            self.get_constant_clone(rk_to_k(argc) as usize)?
        } else {
            self.get_register_clone(argc as usize)?
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.set_register(arga as usize, RegisterVal::Int(left << right))?
        }

        Ok(())
    }

    #[inline(always)]
    fn op_shr(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);

        let b_val = if is_k(argb) {
            self.get_constant_clone(rk_to_k(argb) as usize)?
        } else {
            self.get_register_clone(argb as usize)?
        };
        let c_val = if is_k(argc) {
            self.get_constant_clone(rk_to_k(argc) as usize)?
        } else {
            self.get_register_clone(argc as usize)?
        };

        if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (b_val, c_val) {
            self.set_register(arga as usize, RegisterVal::Int(left >> right))?
        }

        Ok(())
    }

    #[inline(always)]
    fn op_concat(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);

        let b_val = if is_k(argb) {
            self.get_constant_clone(rk_to_k(argb) as usize)?
        } else {
            self.get_register_clone(argb as usize)?
        };
        let c_val = if is_k(argc) {
            self.get_constant_clone(rk_to_k(argc) as usize)?
        } else {
            self.get_register_clone(argc as usize)?
        };

        if let (RegisterVal::Str(left), RegisterVal::Str(right)) = (b_val, c_val) {
            let concatenated = format!("{}{}", left, right);
            self.set_register(arga as usize, RegisterVal::Str(Arc::new(concatenated)))?;
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

        Err(VMRuntimeError::Exit(arga))
    }

    #[inline(always)]
    fn op_clone(&mut self, inst: Instruction) -> Result<(), VMRuntimeError> {
        let arga = get_arga(inst);
        let argb = get_argb(inst);

        let value = self.get_register_clone(argb as usize)?;
        self.set_register(arga as usize, value)?;

        Ok(())
    }

    #[inline(always)]
    fn op_nop(&mut self, _inst: Instruction) -> Result<(), VMRuntimeError> {
        // here only to waste cycles lol
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::backend_vm::vm::{VMThread, VM};

    #[tokio::test]
    async fn sizes() {
        println!("Size of VM: {} bytes", std::mem::size_of::<VM>());
        println!(
            "Size of VMThread: {} bytes",
            std::mem::size_of::<VMThread>()
        );
    }
}
