// Bytecode

// The typical structure of the bytecode follows:
// 1. Magic Number QLBC (0x514C4243)
// 2. Metadata                          -- Can be edited but shouldn't
//      - QuikLang Version:
//          - Which QuikLang Compiler compiled the code
//          - Qlang VM Runtime Version, must be the same number to run
//      - Flags
// 3. Setup and Integrity information:  -- Read only
//      - Register count
//      - Const count
//      - Instruction count
// 4. Constant indexes which are strings, and their information -- Read only
// 5. Constant pool/list                -- Read only
// 6. Instructions (List of OpCodes, 32-bit/OpCode in size)    -- Read only

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
//          32          4                   Instruction count - i32 number
//          36          4                   String indication pool - i32 number
//
// 4. String-pointing constant pool indexes:
//      Offset      Size (bytes)        Description
//          40          12 (4+8)          An entry which points to a const entry in the pool, providing information of the UTF-8 string.
//      1.                  4                   Index of constant in constant pool, i32.
//      2.                  8                   Lens (size) of string in const pool, which is 64-bits (u64).
//                                              Note that lens would be in bytes.
//
// 5. Constant pool:
//      Offset      Size (bytes)        Description
//          36          8 each              64-bits (u64) representing a constant. Note, alignment is very important here.
//          -           len * 8             For strings specifically: If identified as a string from (4), it will be treated as a string.
//                                              String would be from: `(current offset) -> (current offset + len * 8).
//                                              String would be padded with null (0) to the right until aligned with 4 bytes to next one.
//
// 6. Instructions:
//      Offset      Size (bytes)        Description
// After 5. offset      4 each              32-bit (u32) representing an instruction, with each segments of the u32 representing opcodes and args.
//                                          Check instructions.rs for more info.

use std::{
    collections::HashMap,
    fs::Metadata,
    io::{Cursor, Read},
};

use byteorder::{LittleEndian, ReadBytesExt};

use crate::errors::VMBytecodeError;

use super::{instructions::Instruction, vm::RegisterVal};

#[derive(Debug, Clone)]
pub struct ByteCode {
    metadata: BCMetadata,
    integrity_info: BCIntegrityInfo,
    constants: Vec<RegisterVal>,
    string_pool: Vec<String>,
    instructions: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct BCMetadata {
    ql_version: [u8; 8],
    ql_vm_ver: i32,
    flags: u64,
}

#[derive(Debug, Clone)]
pub struct BCIntegrityInfo {
    num_register: i32,
    num_constants: i32,
    num_inst: i32,
    num_string_points: i32,
}

impl ByteCode {
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
        let flags = cursor.read_u64::<LittleEndian>()?;

        let metadata = BCMetadata {
            ql_version,
            ql_vm_ver,
            flags,
        };

        // Read Integrity Info
        let num_register = cursor.read_i32::<LittleEndian>()?;
        let num_constants = cursor.read_i32::<LittleEndian>()?;
        let num_inst = cursor.read_i32::<LittleEndian>()?;
        let num_string_points = cursor.read_i32::<LittleEndian>()?;

        let integrity_info = BCIntegrityInfo {
            num_register,
            num_constants,
            num_inst,
            num_string_points,
        };

        // Read String-Pointing Constant Pool Indexes
        let mut string_indexes: HashMap<i32, usize> =
            HashMap::with_capacity(num_string_points as usize);
        for _ in 0..num_string_points {
            let index = cursor.read_i32::<LittleEndian>()?;
            let length = cursor.read_u64::<LittleEndian>()? as usize;
            string_indexes.insert(index, length);
        }

        // Read Constant Pool and add to String Pool
        let mut constants: Vec<u64> = Vec::with_capacity(num_constants as usize);
        let mut string_pool: Vec<String> = Vec::with_capacity(num_string_points as usize);
        for i in 0..num_constants {
            if let Some(lens) = string_indexes.get(&i) {
                // It is a string.
                let mut string: Vec<u8> = vec![0; *lens];
                cursor.read_exact(&mut string)?;
                string_pool.push(String::from_utf8(string)?);
                constants.push(i as u64);
            } else {
                // Not a string.
                constants.push(cursor.read_u64::<LittleEndian>()?);
            }
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
            string_pool,
            instructions,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::backend_vm::instructions::{ABx, Abc, OP_LOADBOOL, OP_LOADCONST, OP_MOVE};

    use super::*;
    use byteorder::{LittleEndian, WriteBytesExt};

    fn create_valid_bytecode() -> Vec<u8> {
        let mut bytecode = Vec::new();
        // Magic Number
        bytecode.extend_from_slice(b"QLBC");
        // Metadata
        bytecode.extend_from_slice(b"v1.0.0  "); // QL Version
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // QLang VM Runtime Version
        bytecode.write_u64::<LittleEndian>(0).unwrap(); // Flags

        // Setup and Integrity Information
        bytecode.write_i32::<LittleEndian>(4).unwrap(); // Register count
        bytecode.write_i32::<LittleEndian>(2).unwrap(); // Constant count
        bytecode.write_i32::<LittleEndian>(3).unwrap(); // Instruction count
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // String-pointing constant count

        // String-Pointing Constant Pool Indexes
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // Index of constant in constant pool
        bytecode.write_u64::<LittleEndian>(3).unwrap(); // Length of the string in bytes

        // Constant Pool
        bytecode.write_u64::<LittleEndian>(42).unwrap(); // Non-string constant
        bytecode.extend_from_slice(b"foo\0\0\0\0\0\0\0\0\0"); // String constant "foo"

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
        let mut bytecode = Vec::new();
        // Invalid Magic Number
        bytecode.extend_from_slice(b"XXXX");
        // Metadata
        bytecode.extend_from_slice(b"v1.0.0  "); // QL Version
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // QLang VM Runtime Version
        bytecode.write_u64::<LittleEndian>(0).unwrap(); // Flags

        // Setup and Integrity Information
        bytecode.write_i32::<LittleEndian>(4).unwrap(); // Register count
        bytecode.write_i32::<LittleEndian>(2).unwrap(); // Constant count
        bytecode.write_i32::<LittleEndian>(3).unwrap(); // Instruction count
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // String-pointing constant count

        // String-Pointing Constant Pool Indexes
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // Index of constant in constant pool
        bytecode.write_u64::<LittleEndian>(3).unwrap(); // Length of the string in bytes

        // Constant Pool
        bytecode.write_u64::<LittleEndian>(42).unwrap(); // Non-string constant
        bytecode.extend_from_slice(b"foo\0\0\0\0\0\0\0\0\0"); // String constant "foo"

        // Instructions
        bytecode.write_u32::<LittleEndian>(0).unwrap(); // OP_MOVE
        bytecode.write_u32::<LittleEndian>(1).unwrap(); // OP_LOADCONST
        bytecode.write_u32::<LittleEndian>(2).unwrap(); // OP_LOADBOOL

        bytecode
    }

    #[test]
    fn test_valid_bytecode() {
        let valid_bytecode = create_valid_bytecode();
        let result = ByteCode::decode(&valid_bytecode);
        println!("{:#?}", result);
        assert!(
            result.is_ok(),
            "Valid bytecode should be decoded successfully"
        );
        let bytecode = result.unwrap();
        assert_eq!(bytecode.constants.len(), 2);
        assert_eq!(bytecode.constants[0], 42);
        assert_eq!(bytecode.string_pool.len(), 1);
        assert_eq!(bytecode.string_pool[0], "foo");
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
