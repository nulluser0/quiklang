// VM (Register-based)

use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    hash::Hasher,
    rc::Rc,
};

use crate::errors::VMRuntimeError;

use std::hash::Hash;

use super::{
    bytecode::ByteCode,
    instructions::{
        get_arga, get_argb, get_argbx, get_argc, get_argsbx, get_opcode, Instruction, OP_ADD,
        OP_AND, OP_BITAND, OP_BITOR, OP_BITXOR, OP_CALL, OP_CONCAT, OP_DEC, OP_DESTRUCTOR, OP_DIV,
        OP_EQ, OP_GE, OP_GT, OP_INC, OP_JUMP, OP_JUMP_IF_FALSE, OP_JUMP_IF_TRUE, OP_LE,
        OP_LOADBOOL, OP_LOADCONST, OP_LOADNULL, OP_LT, OP_MOD, OP_MOVE, OP_MUL, OP_NE, OP_NOP,
        OP_NOT, OP_OR, OP_POW, OP_RETURN, OP_SHL, OP_SHR, OP_SUB, OP_TAILCALL,
    },
};

// struct CallFrame {
//     function_name: String,
//     return_address: usize,
//     base: usize,
// }

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
#[derive(Debug, Clone, PartialEq)]
pub enum RegisterVal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(Rc<String>),
    Array(Rc<Vec<RegisterVal>>),
    HashMap(Rc<HashMap<RegisterVal, RegisterVal>>),
    HashSet(Rc<HashSet<RegisterVal>>),
    Null,
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

impl PartialOrd for RegisterVal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (RegisterVal::Int(a), RegisterVal::Int(b)) => a.partial_cmp(b),
            (RegisterVal::Float(a), RegisterVal::Float(b)) => a.partial_cmp(b),
            (RegisterVal::Bool(a), RegisterVal::Bool(b)) => a.partial_cmp(b),
            (RegisterVal::Str(a), RegisterVal::Str(b)) => a.partial_cmp(b),
            (RegisterVal::Array(a), RegisterVal::Array(b)) => a.partial_cmp(b),
            (RegisterVal::HashMap(a), RegisterVal::HashMap(b)) => a.len().partial_cmp(&b.len()),
            (RegisterVal::HashSet(a), RegisterVal::HashSet(b)) => a.len().partial_cmp(&b.len()),
            (RegisterVal::Null, RegisterVal::Null) => Some(Ordering::Equal),

            // Comparisons between Int and Float
            (RegisterVal::Int(a), RegisterVal::Float(b)) => (*a as f64).partial_cmp(b),
            (RegisterVal::Float(a), RegisterVal::Int(b)) => a.partial_cmp(&(*b as f64)),

            // Define the ordering between different types
            (RegisterVal::Int(_), _) => Some(Ordering::Less),
            (_, RegisterVal::Int(_)) => Some(Ordering::Greater),
            (RegisterVal::Float(_), _) => Some(Ordering::Less),
            (_, RegisterVal::Float(_)) => Some(Ordering::Greater),
            (RegisterVal::Bool(_), _) => Some(Ordering::Less),
            (_, RegisterVal::Bool(_)) => Some(Ordering::Greater),
            (RegisterVal::Str(_), _) => Some(Ordering::Less),
            (_, RegisterVal::Str(_)) => Some(Ordering::Greater),
            (RegisterVal::Array(_), _) => Some(Ordering::Less),
            (_, RegisterVal::Array(_)) => Some(Ordering::Greater),
            (RegisterVal::HashMap(_), _) => Some(Ordering::Less),
            (_, RegisterVal::HashMap(_)) => Some(Ordering::Greater),
            (RegisterVal::HashSet(_), _) => Some(Ordering::Less),
            (_, RegisterVal::HashSet(_)) => Some(Ordering::Greater),
        }
    }
}

// A note about converting from bytecode to vm for strings specifically:
// A constant entry with a string has information we know: its lens and string content.
// Since the VM has a String pool for strings, the string would:
//      - Be added to the string pool, and
//      - Its pointer (either an index to a vec, or its raw pointer to the memory) added to the constant pool with the index as identified in the bytecode's constant pool.

#[derive(Debug)]
pub struct VM {
    registers: Vec<RegisterVal>,
    pub constant_pool: Vec<RegisterVal>,
    program_counter: usize,
    pub instructions: Vec<Instruction>,
    // call_stack: Vec<CallFrame>,
}

impl VM {
    pub fn new(
        instructions: Vec<Instruction>,
        constant_pool: Vec<RegisterVal>,
        num_registers: usize,
    ) -> Self {
        VM {
            registers: vec![RegisterVal::Null; num_registers],
            constant_pool,
            program_counter: 0,
            instructions,
        }
    }

    pub fn from_bytecode(bytecode: ByteCode) -> Self {
        VM::new(
            bytecode.instructions().clone(),
            bytecode.constant_pool().clone(),
            *bytecode.register_count() as usize,
        )
    }

    pub fn set_register(&mut self, index: usize, value: RegisterVal) -> Result<(), VMRuntimeError> {
        if index < self.registers.len() {
            self.registers[index] = value;
            Ok(())
        } else {
            Err(VMRuntimeError::AccessToNonExistentRegister(
                index,
                self.registers.len(),
            ))
        }
    }

    pub fn get_register(&self, index: usize) -> Result<&RegisterVal, VMRuntimeError> {
        self.registers
            .get(index)
            .ok_or(VMRuntimeError::AccessToNonExistentRegister(
                index,
                self.registers.len(),
            ))
    }

    pub fn set_constant(&mut self, index: usize, value: RegisterVal) -> Result<(), VMRuntimeError> {
        if index < self.constant_pool.len() {
            self.constant_pool[index] = value;
            Ok(())
        } else {
            Err(VMRuntimeError::AccessToNonExistentConstant(
                index,
                self.constant_pool.len(),
            ))
        }
    }

    pub fn get_constant(&self, index: usize) -> Result<&RegisterVal, VMRuntimeError> {
        self.constant_pool
            .get(index)
            .ok_or(VMRuntimeError::AccessToNonExistentConstant(
                index,
                self.constant_pool.len(),
            ))
    }

    pub fn fetch_instruction(&self) -> Instruction {
        self.instructions[self.program_counter]
    }

    pub fn execute(&mut self) {
        while self.program_counter < self.instructions.len() {
            self.execute_instruction(self.fetch_instruction());
        }
    }

    pub fn execute_instruction(&mut self, inst: Instruction) {
        let op = get_opcode(inst);
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let argbx = get_argbx(inst);
        let argsbx = get_argsbx(inst);
        match op {
            OP_MOVE => {
                let value = self.registers[argb as usize].clone();
                self.registers[arga as usize] = value;
            }
            OP_LOADCONST => {
                let value = self.constant_pool[argbx as usize].clone();
                self.registers[arga as usize] = value;
            }
            OP_LOADBOOL => {
                let value = if argb != 0 {
                    RegisterVal::Bool(true)
                } else {
                    RegisterVal::Bool(false)
                };
                self.registers[arga as usize] = value;
                if argc != 0 {
                    self.program_counter += 1;
                }
            }
            OP_LOADNULL => {
                for i in arga as usize..=argb as usize {
                    self.registers[i] = RegisterVal::Null;
                }
            }
            OP_ADD => {
                match (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    (RegisterVal::Int(left), RegisterVal::Int(right)) => {
                        self.registers[arga as usize] = RegisterVal::Int(left.wrapping_add(*right));
                    }
                    (RegisterVal::Float(left), RegisterVal::Float(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(left + right);
                    }
                    (RegisterVal::Int(left), RegisterVal::Float(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(*left as f64 + right);
                    }
                    (RegisterVal::Float(left), RegisterVal::Int(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(left + *right as f64);
                    }
                    _ => {}
                }
            }
            OP_SUB => {
                match (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    (RegisterVal::Int(left), RegisterVal::Int(right)) => {
                        self.registers[arga as usize] = RegisterVal::Int(left.wrapping_sub(*right));
                    }
                    (RegisterVal::Float(left), RegisterVal::Float(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(left - right);
                    }
                    (RegisterVal::Int(left), RegisterVal::Float(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(*left as f64 - right);
                    }
                    (RegisterVal::Float(left), RegisterVal::Int(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(left - *right as f64);
                    }
                    _ => {}
                }
            }
            OP_MUL => {
                match (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    (RegisterVal::Int(left), RegisterVal::Int(right)) => {
                        self.registers[arga as usize] = RegisterVal::Int(left.wrapping_mul(*right));
                    }
                    (RegisterVal::Float(left), RegisterVal::Float(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(left * right);
                    }
                    (RegisterVal::Int(left), RegisterVal::Float(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(*left as f64 * right);
                    }
                    (RegisterVal::Float(left), RegisterVal::Int(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(left * *right as f64);
                    }
                    _ => {}
                }
            }
            OP_DIV => {
                match (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    (RegisterVal::Int(left), RegisterVal::Int(right)) => {
                        self.registers[arga as usize] = RegisterVal::Int(left.wrapping_div(*right));
                    }
                    (RegisterVal::Float(left), RegisterVal::Float(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(left / right);
                    }
                    (RegisterVal::Int(left), RegisterVal::Float(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(*left as f64 / right);
                    }
                    (RegisterVal::Float(left), RegisterVal::Int(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(left / *right as f64);
                    }
                    _ => {}
                }
            }
            OP_MOD => {
                match (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    (RegisterVal::Int(left), RegisterVal::Int(right)) => {
                        self.registers[arga as usize] = RegisterVal::Int(left.wrapping_rem(*right));
                    }
                    (RegisterVal::Float(left), RegisterVal::Float(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(left % right);
                    }
                    (RegisterVal::Int(left), RegisterVal::Float(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(*left as f64 % right);
                    }
                    (RegisterVal::Float(left), RegisterVal::Int(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(left % *right as f64);
                    }
                    _ => {}
                }
            }
            OP_POW => {
                match (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    (RegisterVal::Int(left), RegisterVal::Int(right)) => {
                        self.registers[arga as usize] =
                            RegisterVal::Int(left.wrapping_pow(*right as u32));
                    }
                    (RegisterVal::Float(left), RegisterVal::Float(right)) => {
                        self.registers[arga as usize] = RegisterVal::Float(left.powf(*right));
                    }
                    (RegisterVal::Int(left), RegisterVal::Float(right)) => {
                        self.registers[arga as usize] =
                            RegisterVal::Float((*left as f64).powf(*right));
                    }
                    (RegisterVal::Float(left), RegisterVal::Int(right)) => {
                        self.registers[arga as usize] =
                            RegisterVal::Float(left.powf(*right as f64));
                    }
                    _ => {}
                }
            }
            OP_NOT => {
                if let RegisterVal::Int(value) = self.registers[argb as usize] {
                    self.registers[arga as usize] = RegisterVal::Int(!value);
                }
            }
            OP_AND => {
                if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    self.registers[arga as usize] = RegisterVal::Int(left & right);
                }
            }
            OP_OR => {
                if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    self.registers[arga as usize] = RegisterVal::Int(left | right);
                }
            }
            OP_EQ => {
                let value = if self.registers[argb as usize] == self.registers[argc as usize] {
                    RegisterVal::Bool(true)
                } else {
                    RegisterVal::Bool(false)
                };
                self.registers[arga as usize] = value;
            }
            OP_NE => {
                let value = if self.registers[argb as usize] != self.registers[argc as usize] {
                    RegisterVal::Bool(true)
                } else {
                    RegisterVal::Bool(false)
                };
                self.registers[arga as usize] = value;
            }
            OP_LT => {
                let value = if self.registers[argb as usize] < self.registers[argc as usize] {
                    RegisterVal::Bool(true)
                } else {
                    RegisterVal::Bool(false)
                };
                self.registers[arga as usize] = value;
            }
            OP_LE => {
                let value = if self.registers[argb as usize] <= self.registers[argc as usize] {
                    RegisterVal::Bool(true)
                } else {
                    RegisterVal::Bool(false)
                };
                self.registers[arga as usize] = value;
            }
            OP_GT => {
                let value = if self.registers[argb as usize] > self.registers[argc as usize] {
                    RegisterVal::Bool(true)
                } else {
                    RegisterVal::Bool(false)
                };
                self.registers[arga as usize] = value;
            }
            OP_GE => {
                let value = if self.registers[argb as usize] >= self.registers[argc as usize] {
                    RegisterVal::Bool(true)
                } else {
                    RegisterVal::Bool(false)
                };
                self.registers[arga as usize] = value;
            }
            OP_JUMP => {
                self.program_counter = (self.program_counter as i32 + argsbx) as usize;
            }
            OP_JUMP_IF_TRUE => {
                if let RegisterVal::Bool(true) = self.registers[arga as usize] {
                    self.program_counter = (self.program_counter as i32 + argsbx) as usize;
                }
            }
            OP_JUMP_IF_FALSE => {
                if let RegisterVal::Bool(false) = self.registers[arga as usize] {
                    self.program_counter = (self.program_counter as i32 + argsbx) as usize;
                }
            }
            OP_CALL => {
                // Implement function call
            }
            OP_TAILCALL => {
                // Implement tail call optimization
            }
            OP_RETURN => {
                // Implement return from function
            }
            OP_INC => {
                if let RegisterVal::Int(value) = self.registers[arga as usize] {
                    self.registers[arga as usize] = RegisterVal::Int(value.wrapping_add(1));
                }
            }
            OP_DEC => {
                if let RegisterVal::Int(value) = self.registers[arga as usize] {
                    self.registers[arga as usize] = RegisterVal::Int(value.wrapping_sub(1));
                }
            }
            OP_BITAND => {
                if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    self.registers[arga as usize] = RegisterVal::Int(left & right);
                }
            }
            OP_BITOR => {
                if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    self.registers[arga as usize] = RegisterVal::Int(left | right);
                }
            }
            OP_BITXOR => {
                if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    self.registers[arga as usize] = RegisterVal::Int(left ^ right);
                }
            }
            OP_SHL => {
                if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    self.registers[arga as usize] = RegisterVal::Int(left << right);
                }
            }
            OP_SHR => {
                if let (RegisterVal::Int(left), RegisterVal::Int(right)) = (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    self.registers[arga as usize] = RegisterVal::Int(left >> right);
                }
            }
            OP_CONCAT => {
                if let (RegisterVal::Str(left), RegisterVal::Str(right)) = (
                    &self.registers[argb as usize],
                    &self.registers[argc as usize],
                ) {
                    let concatenated = format!("{}{}", left, right);
                    self.registers[arga as usize] = RegisterVal::Str(Rc::new(concatenated));
                }
            }
            OP_DESTRUCTOR => {
                // destroy heap objs, where A is a pointer to the heap obj flagged for destruction.
                todo!()
            }
            OP_NOP => {}
            _ => unreachable!(),
        }
        self.program_counter += 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::backend_vm::instructions::{to_string, ABx, ASBx};

    use super::*;

    fn potential_leaky_vm_create() -> VM {
        VM::new(
            vec![
                ABx(OP_LOADCONST, 0, 0), // Load constant K(0) into register R(0)
                ASBx(OP_JUMP, 0, 1),     // Jump to the next instruction (to create a loop)
                ABx(OP_LOADCONST, 1, 0), // Load constant K(0) into register R(1)
                ASBx(OP_JUMP, 0, -2),    // Jump back to the first instruction
            ],
            vec![RegisterVal::Int(10)], // Constant pool
            2,                          // Number of registers
        )
    }

    #[test]
    fn test_if_leaking() {
        let mut vm: VM = potential_leaky_vm_create();
        println!("{:#?}", vm);
        for inst in &vm.instructions {
            println!("{}", to_string(*inst))
        }
        for _ in 0..1000 {
            vm.execute_instruction(vm.fetch_instruction());
        }

        // If we reach here, test passes B)
    }
}
