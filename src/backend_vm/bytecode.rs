// Bytecode

// The typical structure of the bytecode follows:
// 1. Metadata                          -- Can be edited but shouldn't
//      - QuikLang Version:
//          - Which QuikLang Compiler compiled the code
//          - Intended Quiklang VM Runtime
//      - Flags
// 2. Setup and Integrity information:  -- Read only
//      - Register count
//      - Const count
//      - Instruction count
// 3. Constant pool/list                -- Read only
// 4. Instructions (List of OpCodes)    -- Read only
