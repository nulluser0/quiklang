// VM (Register-based)

use super::instructions::{OpCode, Value};

pub struct VM {
    registers: Vec<Value>,
    constants: Vec<Value>,
    program_counter: usize,
    instructions: Vec<OpCode>,
}

impl VM {
    pub fn new(instructions: Vec<OpCode>, constants: Vec<Value>, num_registers: usize) -> Self {
        VM {
            registers: vec![Value::default(); num_registers],
            constants,
            program_counter: 0,
            instructions,
        }
    }

    pub fn 
}
