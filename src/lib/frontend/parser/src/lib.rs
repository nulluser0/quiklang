//! # Parser
//!
//! The parser is responsible for parsing the tokens generated by the lexer into an AST.
//!

pub mod symbol_table;

use std::fmt::Result;

use quiklang_common::{
    data_structs::ast::package_module::Package,
    errors::{parser::ParserError, CompilerError},
    CompilationReport, FileStore,
};
use symbol_table::SymbolTable;

pub struct Parser<'a> {
    file_store: &'a mut FileStore,
    compilation_report: &'a mut CompilationReport,
    symbol_table: SymbolTable,
}

impl<'a> Parser<'a> {
    pub fn new(
        file_store: &'a mut FileStore,
        compilation_report: &'a mut CompilationReport,
    ) -> Self {
        Self {
            file_store,
            compilation_report,
            symbol_table: SymbolTable::new(),
        }
    }

    /// Entry point for the parser, which parses the entire package.
    pub fn parse_package(&mut self, entry_file_id: usize) -> Package {
        let entry_file = match self.file_store.get_file(entry_file_id) {
            Some(file) => file,
            None => {
                self.compilation_report
                    .add_error(CompilerError::ParserError(ParserError::FileNotFound {
                        file_id: entry_file_id,
                    }));
                return;
            }
        };

        // Parse the entry module
        self.parse_module(entry_file.source.clone())
    }
}
