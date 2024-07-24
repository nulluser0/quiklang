// Instructions

// Rx[n]   : usize = Register n
// Kx[n]   : usize = Constant n
// sBx     : isize = Signed Displacement
// PC      : usize = Program

use std::fmt::Display;

/// instruction = 32bit(fixed length)
///
/// +---------------------------------------------+
/// |0-5(6bits)|6-13(8bit)|14-22(9bit)|23-31(9bit)|
/// |==========+==========+===========+===========|
/// |  opcode  |    A     |     C     |    B      |
/// |----------+----------+-----------+-----------|
/// |  opcode  |    A     |      Bx(unsigned)     |
/// |----------+----------+-----------+-----------|
/// |  opcode  |    A     |      sBx(signed)      |
/// +---------------------------------------------+

type OpCodeSize = i32;

pub const OPCODE_SIZE: OpCodeSize = 6;
pub const OPCODE_SIZE_A: OpCodeSize = 8;
pub const OPCODE_SIZE_B: OpCodeSize = 9;
pub const OPCODE_SIZE_C: OpCodeSize = 9;
pub const OPCODE_SIZE_BX: OpCodeSize = 18;
pub const OPCODE_SIZE_SBX: OpCodeSize = 18;

pub const OPCODE_MAX_A: OpCodeSize = (1 << OPCODE_SIZE_A) - 1;
pub const OPCODE_MAX_B: OpCodeSize = (1 << OPCODE_SIZE_B) - 1;
pub const OPCODE_MAX_C: OpCodeSize = (1 << OPCODE_SIZE_C) - 1;
pub const OPCODE_MAX_BX: OpCodeSize = (1 << OPCODE_SIZE_BX) - 1;
pub const OPCODE_MAX_SBX: OpCodeSize = OPCODE_MAX_BX >> 1;

// Opcodes
pub type OpCode = i32;

// A B      R(A) := R(B)
pub const OP_MOVE: OpCode = 0;
// A Bx     R(A) := K(Bx)
pub const OP_LOADCONST: OpCode = 1;
// A B C    R(A) := (Bool)B; if (C) pc++
pub const OP_LOADBOOL: OpCode = 2;
// A B      R(A) := ... := R(B) := nil
pub const OP_LOADNULL: OpCode = 3;
pub const OP_ADD: OpCode = 4;
pub const OP_SUB: OpCode = 5;
pub const OP_MUL: OpCode = 6;
pub const OP_DIV: OpCode = 7;
pub const OP_MOD: OpCode = 8;
pub const OP_POW: OpCode = 9;
pub const OP_NOT: OpCode = 10;
pub const OP_AND: OpCode = 11;
pub const OP_OR: OpCode = 12;
pub const OP_EQ: OpCode = 13;
pub const OP_NE: OpCode = 14;
pub const OP_LT: OpCode = 15;
pub const OP_LE: OpCode = 16;
pub const OP_GT: OpCode = 17;
pub const OP_GE: OpCode = 18;
pub const OP_JUMP: OpCode = 19; // A  PC += A
pub const OP_JUMP_IF_TRUE: OpCode = 20;
pub const OP_JUMP_IF_FALSE: OpCode = 21; // A B  if (!R(A)) PC += B
pub const OP_CALL: OpCode = 22; // A B  Call function at R(A) with B arguments
pub const OP_TAILCALL: OpCode = 23;
pub const OP_RETURN: OpCode = 24; // A  Return with R(A)
pub const OP_INC: OpCode = 25; // A  R(A)++
pub const OP_DEC: OpCode = 26; // A  R(A)--
pub const OP_BITAND: OpCode = 27; // A B C  R(A) := R(B) & R(C)
pub const OP_BITOR: OpCode = 28; // A B C  R(A) := R(B) | R(C)
pub const OP_BITXOR: OpCode = 29; // A B C  R(A) := R(B) ^ R(C)
pub const OP_SHL: OpCode = 30; // A B C  R(A) := R(B) << R(C)
pub const OP_SHR: OpCode = 31; // A B C  R(A) := R(B) >> R(C)
pub const OP_CONCAT: OpCode = 32;
// NOP
pub const OP_NOP: OpCode = 33;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum OpArgMode {
    NotUsed,
    Used,
    RegisterOrJumpOffset,
    ConstantOrRegisterConstant,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum OpType {
    Abc,
    ABx,
    AsBx,
}

impl Display for OpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            OpType::Abc => write!(f, "Abc"),
            OpType::ABx => write!(f, "ABx"),
            OpType::AsBx => write!(f, "AsBx"),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct OpProp {
    name: &'static str,
    is_test: bool,
    set_reg_a: bool,
    mode_arg_b: OpArgMode,
    mode_arg_c: OpArgMode,
    typ: OpType,
}

pub static OP_NAMES: &[OpProp; OP_NOP as usize + 1] = &[
    OpProp {
        name: "MOVE",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::RegisterOrJumpOffset,
        mode_arg_c: OpArgMode::NotUsed,
        typ: OpType::Abc,
    },
    OpProp {
        name: "LOADCONST",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::NotUsed,
        typ: OpType::ABx,
    },
    OpProp {
        name: "LOADBOOL",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::Used,
        mode_arg_c: OpArgMode::Used,
        typ: OpType::Abc,
    },
    OpProp {
        name: "LOADNULL",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::RegisterOrJumpOffset,
        mode_arg_c: OpArgMode::NotUsed,
        typ: OpType::Abc,
    },
    OpProp {
        name: "ADD",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "SUB",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "MUL",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "DIV",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "MOD",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "POW",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "NOT",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::RegisterOrJumpOffset,
        mode_arg_c: OpArgMode::NotUsed,
        typ: OpType::Abc,
    },
    OpProp {
        name: "AND",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "OR",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "EQ",
        is_test: true,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "NE",
        is_test: true,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "LT",
        is_test: true,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "LE",
        is_test: true,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "GT",
        is_test: true,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "GE",
        is_test: true,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "JUMP",
        is_test: false,
        set_reg_a: false,
        mode_arg_b: OpArgMode::Used,
        mode_arg_c: OpArgMode::NotUsed,
        typ: OpType::AsBx,
    },
    OpProp {
        name: "JUMP_IF_TRUE",
        is_test: true,
        set_reg_a: true,
        mode_arg_b: OpArgMode::Used,
        mode_arg_c: OpArgMode::NotUsed,
        typ: OpType::AsBx,
    },
    OpProp {
        name: "JUMP_IF_FALSE",
        is_test: true,
        set_reg_a: true,
        mode_arg_b: OpArgMode::Used,
        mode_arg_c: OpArgMode::NotUsed,
        typ: OpType::AsBx,
    },
    OpProp {
        name: "CALL",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::Used,
        mode_arg_c: OpArgMode::Used,
        typ: OpType::Abc,
    },
    OpProp {
        name: "TAILCALL",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::Used,
        mode_arg_c: OpArgMode::Used,
        typ: OpType::Abc,
    },
    OpProp {
        name: "RETURN",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::NotUsed,
        mode_arg_c: OpArgMode::NotUsed,
        typ: OpType::Abc,
    },
    OpProp {
        name: "INC",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::NotUsed,
        mode_arg_c: OpArgMode::NotUsed,
        typ: OpType::Abc,
    },
    OpProp {
        name: "DEC",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::NotUsed,
        mode_arg_c: OpArgMode::NotUsed,
        typ: OpType::Abc,
    },
    OpProp {
        name: "BITAND",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "BITOR",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "BITXOR",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "SHL",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "SHR",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "CONCAT",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::ConstantOrRegisterConstant,
        mode_arg_c: OpArgMode::ConstantOrRegisterConstant,
        typ: OpType::Abc,
    },
    OpProp {
        name: "NOP",
        is_test: false,
        set_reg_a: true,
        mode_arg_b: OpArgMode::NotUsed,
        mode_arg_c: OpArgMode::NotUsed,
        typ: OpType::AsBx,
    },
];

pub fn op_to_string(op: i32) -> String {
    OP_NAMES[op as usize].name.to_string()
}

pub type Instruction = u32;

#[inline]
pub fn get_opcode(inst: Instruction) -> OpCode {
    (inst >> 26) as OpCode
}

#[inline]
pub fn set_opcode(inst: &mut Instruction, op: OpCode) {
    *inst = (*inst & 0x3ffffff) | ((op as u32) << 26);
}

#[inline]
pub fn get_arga(inst: Instruction) -> i32 {
    let a = ((inst >> 18) & 0xff) as i32;

    // Convert the 8-bit value to a signed 8-bit integer
    if a & 0x80 != 0 {
        a | !0xff // Extend the sign bit
    } else {
        a
    }
}

#[inline]
pub fn set_arga(inst: &mut Instruction, a: i32) {
    *inst = (*inst & 0xfc03ffff) | ((a as u32 & 0xff) << 18);
}

#[inline]
pub fn get_argb(inst: Instruction) -> i32 {
    let b = (inst & 0x1ff) as i32;
    if b & 0x100 != 0 {
        b | !0x1ff // Extend the sign bit
    } else {
        b
    }
}

#[inline]
pub fn set_argb(inst: &mut Instruction, b: i32) {
    *inst = (*inst & 0xfffffe00) | (b as u32 & 0x1ff);
}

#[inline]
pub fn get_argc(inst: Instruction) -> i32 {
    let c = ((inst >> 9) & 0x1ff) as i32;
    if c & 0x100 != 0 {
        c | !0x1ff // Extend the sign bit
    } else {
        c
    }
}

#[inline]
pub fn set_argc(inst: &mut Instruction, c: i32) {
    *inst = (*inst & 0xfffc01ff) | ((c as u32 & 0x1ff) << 9);
}

#[inline]
pub fn get_argbx(inst: Instruction) -> i32 {
    (inst & 0x3ffff) as i32
}

#[inline]
pub fn set_argbx(inst: &mut Instruction, bx: i32) {
    *inst = (*inst & 0xfffc0000) | (bx as u32 & 0x3ffff);
}

#[inline]
pub fn get_argsbx(inst: Instruction) -> i32 {
    get_argbx(inst) - (OPCODE_MAX_SBX + 1)
}

#[inline]
pub fn set_argsbx(inst: &mut Instruction, sbx: i32) {
    set_argbx(inst, sbx + (OPCODE_MAX_SBX + 1));
}

#[allow(non_snake_case)]
pub fn Abc(op: OpCode, a: i32, b: i32, c: i32) -> Instruction {
    let mut inst: Instruction = 0;
    set_opcode(&mut inst, op);
    set_arga(&mut inst, a);
    set_argb(&mut inst, b);
    set_argc(&mut inst, c);
    inst
}

#[allow(non_snake_case)]
pub fn ABx(op: OpCode, a: i32, bx: i32) -> Instruction {
    let mut inst: Instruction = 0;
    set_opcode(&mut inst, op);
    set_arga(&mut inst, a);
    set_argbx(&mut inst, bx);
    inst
}

#[allow(non_snake_case)]
pub fn ASBx(op: OpCode, a: i32, sbx: i32) -> Instruction {
    let mut inst: Instruction = 0;
    set_opcode(&mut inst, op);
    set_arga(&mut inst, a);
    set_argsbx(&mut inst, sbx);
    inst
}

pub const OP_BIT_RK: OpCodeSize = 1 << (OPCODE_SIZE_B - 1);
pub const OP_MAX_INDEX_RK: OpCodeSize = OP_BIT_RK - 1;

#[inline]
pub fn is_k(v: i32) -> bool {
    v & OP_BIT_RK != 0
}

#[inline]
pub fn rk_ask(v: i32) -> i32 {
    v | OP_BIT_RK
}

pub fn to_string(inst: Instruction) -> String {
    let op = get_opcode(inst);
    if op > OP_NOP {
        return String::new();
    }

    let prop = OP_NAMES[op as usize];
    let arga = get_arga(inst);
    let argb = get_argb(inst);
    let argc = get_argc(inst);
    let argbx = get_argbx(inst);
    let argsbx = get_argsbx(inst);

    let ops = match prop.typ {
        OpType::Abc => format!(
            "{:20}| {:4} | {:4}, {:4}, {:4}",
            prop.name,
            prop.typ.to_string(),
            arga,
            argb,
            argc
        ),
        OpType::ABx => format!(
            "{:20}| {:4} | {:4}, {:10}",
            prop.name,
            prop.typ.to_string(),
            arga,
            argbx
        ),
        OpType::AsBx => format!(
            "{:20}| {:4} | {:4}, {:10}",
            prop.name,
            prop.typ.to_string(),
            arga,
            argsbx
        ),
    };

    let ops = format!("{:36}", ops);

    match op {
        OP_MOVE => format!("{} | R({}) := R({}), argb => arga", ops, arga, argb),
        OP_LOADCONST => format!("{} | R({}) := Kst({})", ops, arga, argbx),
        OP_LOADBOOL => format!(
            "{} | R({}) := (Bool){}; if ({}) pc++",
            ops, arga, argb, argc
        ),
        OP_LOADNULL => format!("{} | R({}) := ... := R({}) := nil", ops, arga, argb),
        OP_ADD => format!("{} | R({}) := RK({}) + RK({})", ops, arga, argb, argc),
        OP_SUB => format!("{} | R({}) := RK({}) - RK({})", ops, arga, argb, argc),
        OP_MUL => format!("{} | R({}) := RK({}) * RK({})", ops, arga, argb, argc),
        OP_DIV => format!("{} | R({}) := RK({}) / RK({})", ops, arga, argb, argc),
        OP_MOD => format!("{} | R({}) := RK({}) % RK({})", ops, arga, argb, argc),
        OP_POW => format!("{} | R({}) := RK({}) ^ RK({})", ops, arga, argb, argc),
        OP_NOT => format!("{} | R({}) := not R({})", ops, arga, argb),
        OP_AND => format!("{} | R({}) := R({}) and R({})", ops, arga, argb, argc),
        OP_OR => format!("{} | R({}) := R({}) or R({})", ops, arga, argb, argc),
        OP_EQ => format!("{} | R({}) = (RK({}) == RK({})", ops, arga, argb, argc),
        OP_NE => format!("{} | R({}) := RK({}) != RK({})", ops, arga, argb, argc),
        OP_LT => format!("{} | R({}) = (RK({}) < RK({})", ops, arga, argb, argc),
        OP_LE => format!("{} | R({}) = (RK({}) <= RK({})", ops, arga, argb, argc),
        OP_GT => format!("{} | R({}) = (RK({}) > RK({})", ops, arga, argb, argc),
        OP_GE => format!("{} | R({}) = (RK({}) >= RK({})", ops, arga, argb, argc),
        OP_JUMP => format!("{} | pc += {}", ops, argsbx),
        OP_JUMP_IF_TRUE => format!("{} | if R({}) is true then pc += {}", ops, arga, argsbx),
        OP_JUMP_IF_FALSE => format!("{} | if R({}) is false then pc += {}", ops, arga, argsbx),
        OP_CALL => format!(
            "{} | R({}) ... R({}+{}-2) := R({})(R({}+1) ... R({}+{}-1))",
            ops, arga, arga, argc, arga, arga, arga, argb
        ),
        OP_TAILCALL => format!(
            "{} | return R({})(R({}+1) ... R({}+{}-1))",
            ops, arga, arga, arga, argb
        ),
        OP_RETURN => format!("{} | return R({}) ... R({}+{}-2)", ops, arga, arga, argb),
        OP_INC => format!("{} | R({}) += 1", op, arga),
        OP_DEC => format!("{} | R({}) -= 1", op, arga),
        OP_BITAND => format!(
            "{} | R({}) := R({}) bitwise and R({})",
            op, arga, argb, argc
        ),
        OP_BITOR => format!("{} | R({}) := R({}) bitwise or R({})", op, arga, argb, argc),
        OP_BITXOR => format!(
            "{} | R({}) := R({}) bitwise exclusive or R({})",
            op, arga, argb, argc
        ),
        OP_SHL => format!("{} | R({}) := R({}) << R({})", op, arga, argb, argc),
        OP_SHR => format!("{} | R({}) := R({}) >> R({})", op, arga, argb, argc),
        OP_CONCAT => format!("{} | R({}) := R({}) & R({})", op, arga, argb, argc),
        OP_NOP => ops.to_string(),
        _ => unreachable!(),
    }
}
