// Bytecode

// The typical structure of the bytecode follows:
// 1. Magic Number QLBC (0x514C4243)
// 2. Metadata                              -- Can be edited but shouldn't
//      - QuikLang Version:
//          - Which QuikLang Compiler compiled the code
//          - Qlang VM Runtime Version, must be the same number to run
//      - Flags
// 3. Setup and Integrity information:      -- Read only
//      - Register count
//      - Const count
//      - QL Functions count
//      - Instruction count
// 4. Constant pool/list                    -- Read only
// 5. QLang Functions list                  -- Read only
// 6. QFFI (extern/foreign) Functions list  -- Read only
// 7. Instructions (List of OpCodes, 32-bit/OpCode in size)    -- Read only

// Low-level overview:
// 1. Magic Number
//      Offset      Size (bytes)        Description
//          0           4                   Magic number indicating QLang Bytecode (QLBC - 0x514C4243)
//
// 2. Metadata
//      Offset      Size (bytes)        Description
//          4           8                   Qlang Version - 8 byte fixed size string, 8 chars (e.g. 'v1.0.0  ')
//          12          4                   QLang VM Runtime Version - i32 number, different numbers mean incompatible.
//          16          8                   Flags - 64 bits representing flags
//
// 3. Setup and Integrity information:
//      Offset      Size (bytes)        Description
//          24          4                   Register count - i32 number
//          28          4                   Constant count - i32 number
//          32          4                   QL Functions count - i32 number
//          36          4                   Instruction count - i32 number
//          40          4                   String indication pool - i32 number
//
// 4. Constant pool:
//      Offset      Size (bytes)        Description
//          -           9 each              64-bits (u64, 8-bytes) representing a constant, after a 1-byte discriminant. Note, alignment is very important here.
//                                              Discriminant:
//                                                  0 - Integer
//                                                  1 - Float
//                                                  2 - Bool
//                                                  3 - Null
//                                                  4 - String
//          -          1 + len * 8          For strings specifically: If identified as a string from (4), it will be treated as a string.
//                                              String would be from: `(current offset) -> (current offset + len * 8).
//
// 5. QLang Functions:
//      Offset      Size (bytes)        Description
//          -           8                   8-byte numbers representing u64 number, pointing to the PC of the function.
//
// 5. QFFI (extern/foreign) Functions:
//      Offset      Size (bytes)        Description
//          -           TODO!
// 6. Instructions:
//      Offset      Size (bytes)        Description
// After 4. offset      4 each              32-bit (u32) representing an instruction, with each segments of the u32 representing opcodes and args.
//                                          Check instructions.rs for more info.

use core::str;
use std::{
    fmt::Display,
    io::{Cursor, Read, Write},
    vec,
};

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};

use crate::{
    backend_vm::{instructions::to_string, register_val::to_quiklangc_strings},
    errors::VMBytecodeError,
};

use super::{bytecode_compiler::compiler::TaggedConstantValue, instructions::Instruction};

#[derive(Debug, Clone)]
pub struct ByteCode {
    pub metadata: BCMetadata,
    pub integrity_info: BCIntegrityInfo,
    pub constants: Vec<TaggedConstantValue>,
    pub qlang_functions: Vec<u64>,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct BCMetadata {
    pub ql_version: [u8; 8],
    pub ql_vm_ver: i32,
    pub flags: u64,
}

#[derive(Debug, Clone)]
pub struct BCIntegrityInfo {
    pub num_register: i32,
    pub num_constants: i32,
    pub num_qlang_functions: i32,
    pub num_inst: i32,
}

impl Display for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let version_as_string: &str = match str::from_utf8(&self.metadata.ql_version) {
            Ok(result) => result,
            Err(e) => &format!("Failed to decode version: {}", e),
        };
        let mut constants_fragments: Vec<String> = Vec::new();
        for (i, constant) in self.constants.iter().enumerate() {
            constants_fragments.push(format!("{:10}| {}", i, to_quiklangc_strings(constant)));
        }
        let constants_string = constants_fragments.join("\n");
        let qlang_functions_string: String = self
            .qlang_functions
            .iter()
            .map(|inner| format!("Inst: {}", inner))
            .collect::<Vec<String>>()
            .join("\n");
        let mut instructions_fragments: Vec<String> = Vec::new();
        for (i, instruction) in self.instructions.iter().enumerate() {
            instructions_fragments.push(format!("{:10}| {}", i, to_string(*instruction)))
        }
        let instructions_string = instructions_fragments.join("\n");
        write!(
            f,
            r#"QLBC - Quiklang Bytecode
-----------------------------
Quiklang Version:    {}
Quiklang VM Version: {}
Flags:               {}
-----------------------------
No. Registers:       {}
No. Constants:       {}
No. QL Functions:    {}
No. Instructions:    {}
-----------------------------
Constants:
{}
-----------------------------
Quiklang Functions:
{}
-----------------------------
Instructions:
{}
        "#,
            version_as_string,
            self.metadata.ql_vm_ver,
            self.metadata.flags,
            self.integrity_info.num_register,
            self.integrity_info.num_constants,
            self.integrity_info.num_qlang_functions,
            self.integrity_info.num_inst,
            constants_string,
            qlang_functions_string,
            instructions_string
        )
    }
}

impl ByteCode {
    pub fn new(metadata: BCMetadata, integrity_info: BCIntegrityInfo) -> Self {
        Self {
            metadata,
            integrity_info,
            constants: vec![],
            instructions: vec![],
            qlang_functions: vec![],
        }
    }

    // pub fn set_constants(&mut self, constants: Vec<RegisterVal>) {
    //     self.constants = constants
    // }

    // pub fn set_instructions(&mut self, instructions: Vec<Instruction>) {
    //     self.instructions = instructions
    // }

    // pub fn set_metadata(&mut self, metadata: BCMetadata) {
    //     self.metadata = metadata
    // }

    // pub fn set_integrity_info(&mut self, integrity_info: BCIntegrityInfo) {
    //     self.integrity_info = integrity_info
    // }

    /// Decodes from a binary bytecode format, represented as u8 slice (1 byte).
    pub fn decode(bytecode: &[u8]) -> Result<ByteCode, VMBytecodeError> {
        let mut cursor = Cursor::new(bytecode);

        // Read Magic Number
        let mut magic_number: [u8; 4] = [0; 4];
        cursor.read_exact(&mut magic_number)?;
        if &magic_number != b"QLBC" {
            return Err(VMBytecodeError::InvalidOrNoMagicNumber);
        }

        // Read Metadata
        let mut ql_version: [u8; 8] = [0; 8];
        cursor.read_exact(&mut ql_version)?;
        let ql_vm_ver = cursor.read_i32::<LittleEndian>()?;
        let current_ql_vm_ver = env!("QUIKLANG_VM_VERSION").parse().unwrap();
        if ql_vm_ver != current_ql_vm_ver {
            return Err(VMBytecodeError::IncompatibleBytecodeVersion(
                current_ql_vm_ver,
                ql_vm_ver,
            ));
        }
        let flags = cursor.read_u64::<LittleEndian>()?;

        let metadata = BCMetadata {
            ql_version,
            ql_vm_ver,
            flags,
        };

        // Read Integrity Info
        let num_register = cursor.read_i32::<LittleEndian>()?;
        let num_constants = cursor.read_i32::<LittleEndian>()?;
        let num_qlang_functions = cursor.read_i32::<LittleEndian>()?;
        let num_inst = cursor.read_i32::<LittleEndian>()?;

        let integrity_info = BCIntegrityInfo {
            num_register,
            num_constants,
            num_inst,
            num_qlang_functions,
        };

        // Read Constant Pool and add to String Pool
        let mut constants: Vec<TaggedConstantValue> = Vec::with_capacity(num_constants as usize);
        for _ in 0..num_constants {
            let discriminant = cursor.read_u8()?;
            match discriminant {
                0 => constants.push(TaggedConstantValue::Int(cursor.read_i64::<LittleEndian>()?)),
                1 => constants.push(TaggedConstantValue::Float(
                    cursor.read_f64::<LittleEndian>()?,
                )),
                2 => constants.push(TaggedConstantValue::Bool(cursor.read_u8()? != 0)),
                3 => constants.push(TaggedConstantValue::Null),
                4 => {
                    // It is a string.
                    let lens = cursor.read_u64::<LittleEndian>()? as usize;
                    let mut string: Vec<u8> = vec![0; lens];
                    cursor.read_exact(&mut string)?;
                    constants.push(TaggedConstantValue::Str(String::from_utf8(string)?));
                }
                _ => return Err(VMBytecodeError::InvalidConstantType(discriminant)),
            }
        }

        // Read QLang functions
        let mut qlang_functions: Vec<u64> = Vec::with_capacity(num_qlang_functions as usize);
        for _ in 0..num_qlang_functions {
            let inst_ptr = cursor.read_u64::<LittleEndian>()?;
            qlang_functions.push(inst_ptr);
        }

        // Read Instructions
        let mut instructions = Vec::with_capacity(num_inst as usize);
        for _ in 0..num_inst {
            instructions.push(cursor.read_u32::<LittleEndian>()?);
        }

        Ok(ByteCode {
            metadata,
            integrity_info,
            constants,
            instructions,
            qlang_functions,
        })
    }

    pub fn encode(bytecode: &ByteCode) -> Result<Vec<u8>, VMBytecodeError> {
        // Calculating the encoded bytecode's size:
        //      4                                       - Magic number              - fixed size
        //      20                                      - Metadata info             - fixed size
        //      16                                      - Integrity info            - fixed size
        // each 8 * (num_constants - num_string_points) - Constants (excl strings)  - variable size
        // each string_len + (8 - (string_len % 8)) % 8 - String constants          - variable size - 1 string len = 1 byte. String len aligned to 8 bytes.
        // each 4 * numm_inst                           - Instructions              - variable size
        const FIXED_SIZES: usize = 4 + 20 + 16;
        let constant_size: usize = bytecode.integrity_info.num_constants as usize;
        let instructions_size: usize = 4 * bytecode.integrity_info.num_inst as usize;
        let total_size = FIXED_SIZES + constant_size + instructions_size;

        let mut encoded_bytecode: Vec<u8> = Vec::with_capacity(total_size);

        // Write Magic Number
        encoded_bytecode.write_all(b"QLBC")?;

        // Write Metadata
        encoded_bytecode.write_all(&bytecode.metadata.ql_version)?;
        encoded_bytecode.write_i32::<LittleEndian>(bytecode.metadata.ql_vm_ver)?;
        encoded_bytecode.write_u64::<LittleEndian>(bytecode.metadata.flags)?;

        // Write Integrity Info
        encoded_bytecode.write_i32::<LittleEndian>(bytecode.integrity_info.num_register)?;
        encoded_bytecode.write_i32::<LittleEndian>(bytecode.integrity_info.num_constants)?;
        encoded_bytecode.write_i32::<LittleEndian>(bytecode.integrity_info.num_qlang_functions)?;
        encoded_bytecode.write_i32::<LittleEndian>(bytecode.integrity_info.num_inst)?;

        // Write Constant Pool
        for constant in bytecode.constants.iter() {
            match constant {
                TaggedConstantValue::Int(int) => {
                    encoded_bytecode.write_u8(0)?;
                    encoded_bytecode.write_i64::<LittleEndian>(*int)?;
                }
                TaggedConstantValue::Float(float) => {
                    encoded_bytecode.write_u8(1)?;
                    encoded_bytecode.write_f64::<LittleEndian>(*float)?;
                }
                TaggedConstantValue::Bool(boolean) => {
                    encoded_bytecode.write_u8(2)?;
                    encoded_bytecode.write_u8(if *boolean { 1 } else { 0 })?;
                }
                TaggedConstantValue::Str(string) => {
                    encoded_bytecode.write_u8(4)?;
                    encoded_bytecode.write_u64::<LittleEndian>(string.len() as u64)?;
                    encoded_bytecode.write_all(string.as_bytes())?;
                }
                TaggedConstantValue::Null => encoded_bytecode.write_u8(3)?,
            }
        }

        // Write QLang Functions
        for inst_ptr in &bytecode.qlang_functions {
            encoded_bytecode.write_u64::<LittleEndian>(*inst_ptr)?;
        }

        // Write Instructions
        for inst in &bytecode.instructions {
            encoded_bytecode.write_u32::<LittleEndian>(*inst)?;
        }

        Ok(encoded_bytecode)
    }

    pub fn instructions(&self) -> &Vec<Instruction> {
        &self.instructions
    }

    pub fn register_count(&self) -> &i32 {
        &self.integrity_info.num_register
    }

    pub fn constant_pool(&self) -> &Vec<TaggedConstantValue> {
        &self.constants
    }
}

#[cfg(test)]
mod tests {
    use crate::backend_vm::instructions::{ABx, Abc, OP_LOADBOOL, OP_LOADCONST, OP_MOVE};

    use super::*;
    use byteorder::{LittleEndian, WriteBytesExt};

    fn create_valid_bytecode() -> Vec<u8> {
        let mut bytecode: Vec<u8> = Vec::new();
        // Magic Number
        bytecode.extend_from_slice(b"QLBC");
        // Metadata
        bytecode.extend_from_slice(b"v1.0.0  "); // QL Version
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // QLang VM Runtime Version
        bytecode.write_u64::<LittleEndian>(0).unwrap(); // Flags

        // Setup and Integrity Information
        bytecode.write_i32::<LittleEndian>(4).unwrap(); // Register count
        bytecode.write_i32::<LittleEndian>(3).unwrap(); // Constant count
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // QL Functions count
        bytecode.write_i32::<LittleEndian>(3).unwrap(); // Instruction count

        // Constant Pool
        bytecode.write_u8(0).unwrap(); // Discriminant for Int
        bytecode.write_i64::<LittleEndian>(42).unwrap(); // Int constant
        bytecode.write_u8(4).unwrap(); // Discriminant for String
        bytecode.write_u64::<LittleEndian>(3).unwrap(); // Length of the string in bytes
        bytecode.extend_from_slice(b"foo"); // String constant "foo"
        bytecode.write_u8(2).unwrap(); // Discriminant for Bool
        bytecode.write_u8(1).unwrap(); // Bool constant true

        // QLang Fns
        bytecode.write_u64::<LittleEndian>(100).unwrap();

        // Instructions
        bytecode
            .write_u32::<LittleEndian>(Abc(OP_MOVE, 0, 1, 0))
            .unwrap(); // OP_MOVE
        bytecode
            .write_u32::<LittleEndian>(ABx(OP_LOADCONST, 1, 0))
            .unwrap(); // OP_LOADCONST
        bytecode
            .write_u32::<LittleEndian>(Abc(OP_LOADBOOL, 2, 1, 0))
            .unwrap(); // OP_LOADBOOL

        bytecode
    }

    fn create_invalid_bytecode() -> Vec<u8> {
        let mut bytecode: Vec<u8> = Vec::new();
        // Invalid Magic Number
        bytecode.extend_from_slice(b"XXXX");
        // Metadata
        bytecode.extend_from_slice(b"v1.0.0  "); // QL Version
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // QLang VM Runtime Version
        bytecode.write_u64::<LittleEndian>(0).unwrap(); // Flags

        // Setup and Integrity Information
        bytecode.write_i32::<LittleEndian>(4).unwrap(); // Register count
        bytecode.write_i32::<LittleEndian>(3).unwrap(); // Constant count
        bytecode.write_i32::<LittleEndian>(3).unwrap(); // Instruction count
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // String-pointing constant count

        // String-Pointing Constant Pool Indexes
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // Index of constant in constant pool
        bytecode.write_u64::<LittleEndian>(3).unwrap(); // Length of the string in bytes

        // Constant Pool
        bytecode.write_u8(0).unwrap(); // Discriminant for Int
        bytecode.write_i64::<LittleEndian>(42).unwrap(); // Int constant
        bytecode.write_u8(4).unwrap(); // Discriminant for String
        bytecode.write_u64::<LittleEndian>(3).unwrap(); // Length of the string in bytes
        bytecode.extend_from_slice(b"foo"); // String constant "foo"
        bytecode.write_u8(2).unwrap(); // Discriminant for Bool
        bytecode.write_u8(1).unwrap(); // Bool constant true

        // Instructions
        bytecode
            .write_u32::<LittleEndian>(Abc(OP_MOVE, 0, 1, 0))
            .unwrap(); // OP_MOVE
        bytecode
            .write_u32::<LittleEndian>(ABx(OP_LOADCONST, 1, 0))
            .unwrap(); // OP_LOADCONST
        bytecode
            .write_u32::<LittleEndian>(Abc(OP_LOADBOOL, 2, 1, 0))
            .unwrap(); // OP_LOADBOOL

        bytecode
    }

    #[test]
    fn test_valid_bytecode() {
        let valid_bytecode = create_valid_bytecode();
        let result = ByteCode::decode(&valid_bytecode);
        assert!(
            result.is_ok(),
            "Valid bytecode should be decoded successfully"
        );
        println!("{}", result.as_ref().unwrap());
        let bytecode = result.unwrap();
        assert_eq!(bytecode.constants.len(), 3);
        assert_eq!(bytecode.constants[0], TaggedConstantValue::Int(42));
        assert_eq!(bytecode.instructions.len(), 3);

        // Check instructions
        assert_eq!(bytecode.instructions[0], Abc(OP_MOVE, 0, 1, 0));
        assert_eq!(bytecode.instructions[1], ABx(OP_LOADCONST, 1, 0));
        assert_eq!(bytecode.instructions[2], Abc(OP_LOADBOOL, 2, 1, 0));
    }

    #[test]
    fn test_invalid_bytecode() {
        let invalid_bytecode = create_invalid_bytecode();
        let result = ByteCode::decode(&invalid_bytecode);
        println!("{:#?}", result);
        assert!(result.is_err(), "Invalid bytecode should not be decoded");
    }
}
