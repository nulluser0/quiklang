// Instructions

// Rx[n]   : usize = Register n
// Kx[n]   : usize = Constant n
// sBx     : isize = Signed Displacement
// PC      : usize = Program Counter

#[derive(Debug, Clone)]
pub enum OpCode {
    Move(usize, usize),      // 1.Rx[] = 2.Rx[]
    LoadConst(usize, usize), // 1.Rx[] = 2.Kx[]
    LoadBool(usize, bool),   // 1.Rx[] = 2.bool
    LoadNull(usize),         // 1.Rx[] = null

    // Binary Operations
    Add(usize, usize, usize), // 1.Rx[] = 2.Rx[] + 3.Rx[]
    Sub(usize, usize, usize), // 1.Rx[] = 2.Rx[] - 3.Rx[]
    Mul(usize, usize, usize), // 1.Rx[] = 2.Rx[] * 3.Rx[]
    Div(usize, usize, usize), // 1.Rx[] = 2.Rx[] / 3.Rx[]
    Mod(usize, usize, usize), // 1.Rx[] = 2.Rx[] % 3.Rx[]

    Jump(isize),               // PC += 1.sBx
    JumpIfFalse(usize, isize), // if 1.Rx[] == false { PC += 2.sBx }

    Halt, // Stop execution
}

#[derive(Debug, Clone, Default)]
pub enum Value {
    Integer(u64),
    Float(f64),
    String(String),
    Bool(bool),
    #[default]
    Null,
}
