// VM (Register-based)

use crate::errors::VMRuntimeError;

use super::{
    bytecode::ByteCode,
    instructions::{
        get_arga, get_argb, get_argbx, get_argc, get_argsbx, get_opcode, Instruction, OP_ADD,
        OP_AND, OP_BITAND, OP_BITOR, OP_BITXOR, OP_CALL, OP_DEC, OP_DIV, OP_EQ, OP_GT, OP_INC,
        OP_JUMP, OP_JUMP_IF_FALSE, OP_LOADBOOL, OP_LOADCONST, OP_LOADNULL, OP_LT, OP_MOD, OP_MOVE,
        OP_MUL, OP_NOP, OP_NOT, OP_OR, OP_POW, OP_RETURN, OP_SHL, OP_SHR, OP_SUB, OP_TAILCALL,
    },
};

// struct CallFrame {
//     function_name: String,
//     return_address: usize,
//     base: usize,
// }

pub type RegisterVal = u64;

// A note about converting from bytecode to vm for strings specifically:
// A constant entry with a string has information we know: its lens and string content.
// Since the VM has a String pool for strings, the string would:
//      - Be added to the string pool, and
//      - Its pointer (either an index to a vec, or its raw pointer to the memory) added to the constant pool with the index as identified in the bytecode's constant pool.

pub struct VM {
    registers: Vec<RegisterVal>,
    constant_pool: Vec<RegisterVal>,
    string_pool: Vec<String>,
    program_counter: usize,
    instructions: Vec<Instruction>,
    // call_stack: Vec<CallFrame>,
}

impl VM {
    pub fn new(
        instructions: Vec<Instruction>,
        string_pool: Vec<String>,
        constant_pool: Vec<RegisterVal>,
        num_registers: usize,
    ) -> Self {
        VM {
            registers: vec![0; num_registers],
            constant_pool,
            string_pool,
            program_counter: 0,
            instructions,
            // call_stack: Vec::new(),
        }
    }

    pub fn from_bytecode(bytecode: ByteCode) -> Self {
        VM::new(
            bytecode.instructions().clone(),
            bytecode.string_pool().clone(),
            bytecode.constant_pool().clone(),
            *bytecode.register_count() as usize,
        )
    }

    pub fn set_register(&mut self, index: usize, value: RegisterVal) -> Result<(), VMRuntimeError> {
        if index < self.registers.len() {
            return Err(VMRuntimeError::AccessToNonExistentRegister(
                index,
                self.registers.len(),
            ));
        }
        self.registers[index] = value;
        Ok(())
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
            return Err(VMRuntimeError::AccessToNonExistentConstant(
                index,
                self.constant_pool.len(),
            ));
        }
        self.constant_pool[index] = value;
        Ok(())
    }

    pub fn get_constant(&self, index: usize) -> Result<&RegisterVal, VMRuntimeError> {
        self.constant_pool
            .get(index)
            .ok_or(VMRuntimeError::AccessToNonExistentConstant(
                index,
                self.constant_pool.len(),
            ))
    }

    pub fn get_string(&self, index: RegisterVal) -> Result<&String, VMRuntimeError> {
        self.string_pool
            .get(index as usize)
            .ok_or(VMRuntimeError::AccessToNonExistentString(
                index as usize,
                self.string_pool.len(),
            ))
    }

    pub fn fetch_instruction(&self) -> Instruction {
        self.instructions[self.program_counter]
    }

    pub fn execute(&mut self) {
        while self.program_counter < self.instructions.len() {
            self.execute_instruction(self.fetch_instruction())
        }
    }

    pub fn execute_instruction(&mut self, inst: Instruction) {
        let op = get_opcode(inst);
        if op > OP_NOP {
            panic!("Invalid OpCode.");
        }
        // let prop = OP_NAMES[op as usize];
        let arga = get_arga(inst);
        let argb = get_argb(inst);
        let argc = get_argc(inst);
        let argbx = get_argbx(inst);
        let argsbx = get_argsbx(inst);
        match op {
            OP_MOVE => {
                let value = self.registers[argb as usize];
                self.registers[arga as usize] = value;
            }
            OP_LOADCONST => {
                let value = self.constant_pool[argbx as usize];
                self.registers[arga as usize] = value;
            }
            OP_LOADBOOL => {
                let value = if argb != 0 { 1 } else { 0 };
                self.registers[arga as usize] = value;
                if argc != 0 {
                    self.program_counter += 1;
                }
            }
            OP_LOADNULL => {
                for i in arga as usize..=argb as usize {
                    self.registers[i] = 0;
                }
            }
            OP_ADD => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left.wrapping_add(right);
            }
            OP_SUB => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left.wrapping_sub(right);
            }
            OP_MUL => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left.wrapping_mul(right);
            }
            OP_DIV => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left / right;
            }
            OP_MOD => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left % right;
            }
            OP_POW => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left ^ right;
            }
            OP_NOT => {
                let value = self.registers[argb as usize];
                self.registers[arga as usize] = !value;
            }
            OP_AND => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left & right;
            }
            OP_OR => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left | right;
            }
            OP_EQ => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = if left == right { 1 } else { 0 };
            }
            OP_LT => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = if left < right { 1 } else { 0 };
            }
            OP_GT => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = if left > right { 1 } else { 0 };
            }
            OP_JUMP => {
                self.program_counter = (self.program_counter as i32 + argsbx) as usize;
            }
            OP_JUMP_IF_FALSE => {
                if self.registers[arga as usize] == 0 {
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
                self.registers[arga as usize] = self.registers[arga as usize].wrapping_add(1);
            }
            OP_DEC => {
                self.registers[arga as usize] = self.registers[arga as usize].wrapping_sub(1);
            }
            OP_BITAND => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left & right;
            }
            OP_BITOR => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left | right;
            }
            OP_BITXOR => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left ^ right;
            }
            OP_SHL => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left << right;
            }
            OP_SHR => {
                let left = self.registers[argb as usize];
                let right = self.registers[argc as usize];
                self.registers[arga as usize] = left >> right;
            }
            OP_NOP => {}
            _ => unreachable!(),
        }
        self.program_counter += 1;
    }
}
