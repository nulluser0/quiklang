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
//
// 4. String-pointing constant pool indexes:
//      Offset      Size (bytes)        Description
//          36          16 (4+4+8)          An entry which points to a const entry in the pool, providing information of the string.
//      1.                  4                   Index of constant in constant pool.
//      2.                  4                   Index of string in string pool.
//      3.                  8                   Lens (size) of string in const pool, which is 64-bits (u64).
//                                              Note that the actual length of the string is 8*lens, since the strings are UTF-8 (8-bits) encoded
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
