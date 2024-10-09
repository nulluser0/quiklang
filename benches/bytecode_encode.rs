use byteorder::{LittleEndian, WriteBytesExt};
use criterion::{criterion_group, criterion_main, Criterion};
use quiklang::qvm::{
    bytecode::ByteCode,
    instructions::{ABx, Abc, OP_LOADBOOL, OP_LOADCONST, OP_MOVE},
};

fn criterion_benchmark(c: &mut Criterion) {
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
        bytecode.write_i32::<LittleEndian>(2).unwrap(); // Constant count
        bytecode.write_i32::<LittleEndian>(3).unwrap(); // Instruction count
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // String-pointing constant count

        // String-Pointing Constant Pool Indexes
        bytecode.write_i32::<LittleEndian>(1).unwrap(); // Index of constant in constant pool
        bytecode.write_u64::<LittleEndian>(3).unwrap(); // Length of the string in bytes

        // Constant Pool
        bytecode.write_u64::<LittleEndian>(42).unwrap(); // Non-string constant
        bytecode.extend_from_slice(b"foo\0\0\0\0\0"); // String constant "foo"

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

    let bytecode_encoded: &[u8] = &create_valid_bytecode();

    let bytecode = ByteCode::decode(bytecode_encoded)
        .expect("Failed to decode literal bytecode during bench setup.");

    c.bench_function("bytecode_encode", |b| {
        b.iter(|| ByteCode::encode(&bytecode))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(200).warm_up_time(std::time::Duration::from_secs(2));
    targets = criterion_benchmark
}
criterion_main!(benches);
