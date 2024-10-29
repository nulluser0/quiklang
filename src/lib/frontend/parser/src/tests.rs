use quiklang_common::{CompilationReport, FileStore};

use crate::Parser;

#[test]
fn test_parsing_a_package() {
    // Relies on CARGO_MANIFEST_DIR/tests/example_project/ to exist
    // and contain a valid Quiklang package

    let mut file_store = FileStore::new();

    let dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("example_project");

    file_store.add_project_files(&dir).unwrap();

    println!("file_store: {:#?}", file_store);

    let mut compilation_report = CompilationReport::new(file_store);

    let mut parser = Parser::new(&mut compilation_report);

    let package = parser.parse_package();

    println!("package: {:#?}", package);

    compilation_report.report();
}
