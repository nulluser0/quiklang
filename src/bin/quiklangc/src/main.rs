//! The compilation pathway is as follows:
//! 1. Files are gathered into a file store. Package.toml is read.
//! 2. Dependency tree is built from the file store, with dependencies being resolved.
//! 3. For each dependency starting from the top of the tree:
//! 4. Beginning at root module (main or lib), the parser reads the file and generates an AST.
//! 5. The AST is passed to the semantic checker, which verifies that the AST is semantically correct.

fn main() {
    println!("Hello, world!");
}

