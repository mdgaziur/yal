use crate::diagnostics::{Diagnostic, Severity};
use crate::interner::{InternedString, INTERNER};

use crate::analyzer::Analyzer;
use crate::ast::StmtContainer;
use crate::parser::Parser;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Session {
    pub entry_point: InternedString,
    pub debug: bool,
    pub files: HashMap<InternedString, String>,
    asts: HashMap<InternedString, Vec<StmtContainer>>,
    diagnostics: HashMap<InternedString, Vec<Diagnostic>>,
}

impl Session {
    pub fn from_file(filename: String, file: String, debug: bool) -> Self {
        let filename = INTERNER.write().intern_string(filename);
        let mut files = HashMap::new();
        files.insert(filename, file);

        Self {
            entry_point: filename,
            files,
            diagnostics: HashMap::new(),
            asts: HashMap::new(),
            debug,
        }
    }

    pub fn parse(&mut self, file: InternedString) {
        let file_content = &self.files[&file];
        let mut parser = Parser::new(file_content);
        let stmts = parser.parse();
        self.diagnostics.insert(file, parser.get_diagnostics());

        let analyzer = Analyzer::new(&stmts);
        analyzer.analyze();

        self.diagnostics
            .get_mut(&file)
            .unwrap()
            .extend(analyzer.get_diagnostics());

        if self.debug {
            dbg!(&stmts);
        }

        self.asts.insert(file, stmts);
    }

    pub fn parse_entrypoint(&mut self) {
        self.parse(self.entry_point);
    }

    pub fn has_diagnostics(&self) -> bool {
        for (_, diags) in &self.diagnostics {
            for diag in diags {
                if diag.severity == Severity::Error {
                    return true;
                }
            }
        }

        false
    }

    pub fn print_diagnostics(&self) {
        for (filename, diagnostics) in &self.diagnostics {
            for diagnostic in diagnostics {
                eprintln!(
                    "{}",
                    diagnostic.display(
                        INTERNER.read().get_interned_string(*filename),
                        self.files.get(filename).unwrap()
                    )
                );
            }
        }
    }

    pub fn get_ast(&self, file_name: InternedString) -> &[StmtContainer] {
        &self.asts[&file_name]
    }
}
