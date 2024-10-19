pub mod data_structs;
pub mod errors;

use std::collections::HashMap;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::{self, termcolor::StandardStream};
use errors::{CompilerError, CompilerWarning};

#[derive(Debug, Clone)]
pub struct File {
    pub id: usize,
    pub name: String,   // e.g., "src/main.quik"
    pub source: String, // The actual source code
}

/// Manages all files involved in the compilation process.
#[derive(Debug, Default, Clone)]
pub struct FileStore {
    files: HashMap<usize, File>,
    next_id: usize,
}

impl FileStore {
    /// Adds a new file to the store and returns its unique ID.
    pub fn add_file(&mut self, name: String, source: String) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        self.files.insert(id, File { id, name, source });
        id
    }

    /// Retrieves a file by its ID.
    pub fn get_file(&self, id: usize) -> Option<&File> {
        self.files.get(&id)
    }

    /// Retrieves a file by its name.
    pub fn get_file_by_name(&self, name: &str) -> Option<&File> {
        self.files.values().find(|f| f.name == name)
    }
}

#[derive(Debug, Default)]
pub struct CompilationReport {
    pub errors: Vec<CompilerError>,
    pub warnings: Vec<CompilerWarning>,
    pub file_store: FileStore,
}

impl CompilationReport {
    pub fn add_error(&mut self, error: CompilerError) {
        self.errors.push(error);
    }

    pub fn add_warning(&mut self, warning: CompilerWarning) {
        self.warnings.push(warning);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Reports all accumulated errors and warnings with file names.
    pub fn report(&self) {
        let config = codespan_reporting::term::Config::default();
        let mut writer = StandardStream::stderr(term::termcolor::ColorChoice::Auto);

        let mut files = SimpleFiles::new(); // Placeholder
        let mut file_id_map = std::collections::HashMap::new();

        // Add all files to `SimpleFiles` and map their `file_id`s
        for file in self.file_store.files.values() {
            let id = files.add(&file.name, &file.source);
            file_id_map.insert(file.id, id);
        }

        // Report Errors
        if !self.errors.is_empty() {
            for error in &self.errors {
                let diagnostic = match error {
                    CompilerError::LexerError(e) => Diagnostic::error()
                        .with_message(format!("{}", e))
                        .with_labels(vec![Label::primary(
                            *file_id_map.get(&e.span().file_id).unwrap(),
                            e.span().start..e.span().end,
                        )
                        .with_message("here")])
                        .with_notes(e.suggestions()),
                    CompilerError::ParserError(e) => Diagnostic::error()
                        .with_message(format!("{}", e))
                        .with_labels(vec![Label::primary(
                            *file_id_map.get(&e.span().file_id).unwrap(),
                            e.span().start..e.span().end,
                        )
                        .with_message("here")])
                        .with_notes(e.suggestions()),
                };

                codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic).unwrap();
            }
        }

        // Report Warnings
        if !self.warnings.is_empty() {
            #[allow(
                unreachable_code,
                unused_variables,
                clippy::match_single_binding,
                reason = "Remove this line when adding warning categories"
            )]
            for warning in &self.warnings {
                let diagnostic = match warning {
                    // CompilerWarning::UnusedVariable {
                    //     name,
                    //     span,
                    //     suggestion: _,
                    // } => Diagnostic::warning()
                    //     .with_message(format!("Unused Variable: '{}'", name))
                    //     .with_labels(vec![Label::primary(
                    //         *file_id_map.get(&span.file_id).unwrap(),
                    //         (span.start, span.end),
                    //     )
                    //     .with_message("not used here")]),
                    // CompilerWarning::DeprecatedFeature {
                    //     feature,
                    //     span,
                    //     suggestion: _,
                    // } => Diagnostic::warning()
                    //     .with_message(format!("Deprecated Feature: '{}'", feature))
                    //     .with_labels(vec![Label::primary(
                    //         *file_id_map.get(&span.file_id).unwrap(),
                    //         (span.start, span.end),
                    //     )
                    //     .with_message("used here")]),
                    // // Handle other warning categories
                    _ => todo!(),
                };

                codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic).unwrap();
            }
        }
    }
}
