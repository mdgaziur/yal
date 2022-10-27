use crate::ast::{Stmt, StmtContainer};
use crate::diagnostics::{Diagnostic, ErrorCode, Severity, Span};

use std::cell::RefCell;

pub struct Analyzer<'tcx> {
    ast: &'tcx [StmtContainer],
    context: RefCell<AnalyzerContext>,
}

#[derive(Debug, Clone)]
struct AnalyzerContext {
    diagnostics: Vec<Diagnostic>,
    context: Vec<Context>,
}

impl<'tcx> Analyzer<'tcx> {
    pub fn new(ast: &'tcx [StmtContainer]) -> Self {
        Self {
            ast,
            context: RefCell::new(AnalyzerContext {
                diagnostics: vec![],
                context: vec![Context::Root],
            }),
        }
    }

    pub fn analyze(&self) {
        for stmt in self.ast {
            self.analyze_stmt(stmt);
        }
    }

    fn get_block_stmt<'a>(&'a self, stmt: &'a Stmt) -> &'a [StmtContainer] {
        match stmt {
            Stmt::Block(stmts) => stmts,
            _ => panic!("Cannot get block stmt from non block stmt"),
        }
    }

    fn analyze_stmt(&self, stmt: &StmtContainer) {
        match &stmt.stmt {
            Stmt::Fun(fun_stmt) => {
                self.context
                    .borrow_mut()
                    .context
                    .push(Context::Fun(stmt.span));
                for stmt in self.get_block_stmt(&fun_stmt.body.stmt) {
                    self.analyze_stmt(stmt);
                }
                self.context.borrow_mut().context.pop();
            }
            Stmt::Data(_) => {}
            Stmt::Methods(method_stmt) => {
                for method in &method_stmt.methods {
                    self.context
                        .borrow_mut()
                        .context
                        .push(Context::Method(method.span));
                    self.analyze_stmt(method);
                    self.context.borrow_mut().context.pop();
                }
            }
            Stmt::If(if_stmt) => {
                let body = self.get_block_stmt(&if_stmt.block.stmt);
                for stmt in body {
                    self.analyze_stmt(stmt);
                }

                if let Some(then) = &if_stmt.then {
                    self.analyze_stmt(then);
                }

                if let Some(else_) = &if_stmt.else_ {
                    let body = self.get_block_stmt(match &else_.stmt {
                        Stmt::If(if_stmt) => &if_stmt.block.stmt,
                        _ => unreachable!(),
                    });

                    for stmt in body {
                        self.analyze_stmt(stmt);
                    }
                }
            }
            Stmt::Loop(loop_stmt) => {
                self.context
                    .borrow_mut()
                    .context
                    .push(Context::Loop(stmt.span));
                for stmt in self.get_block_stmt(&loop_stmt.stmt) {
                    self.analyze_stmt(stmt);
                }
                self.context.borrow_mut().context.pop();
            }
            Stmt::Iter(iter_stmt) => {
                self.context
                    .borrow_mut()
                    .context
                    .push(Context::Loop(stmt.span));
                for stmt in self.get_block_stmt(&iter_stmt.block.stmt) {
                    self.analyze_stmt(stmt);
                }
                self.context.borrow_mut().context.pop();
            }
            Stmt::Continue | Stmt::Break => {
                if !matches!(
                    self.context.borrow_mut().context.last(),
                    Some(Context::Loop(_))
                ) {
                    self.context.borrow_mut().diagnostics.push(Diagnostic {
                        severity: Severity::Error,
                        code: ErrorCode::InvalidLoopControl,
                        message: String::from("Found `continue` or `break` outside loop"),
                        span: stmt.span,
                    });
                }
            }
            _ => (),
        }
    }

    pub fn get_diagnostics(self) -> Vec<Diagnostic> {
        self.context.into_inner().diagnostics
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
enum Context {
    Root,
    Fun(Span),
    Method(Span),
    Loop(Span),
}
