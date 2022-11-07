use crate::memory::Value;

use owo_colors::OwoColorize;
use std::cmp::max;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

use parking_lot::RwLock;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub span: Span,
    pub message: String,
    pub severity: Severity,
    pub code: ErrorCode,
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
struct Pos {
    line: usize,
    column: usize,
}

impl Diagnostic {
    fn calculate_pos_for_n(&self, file: &str, n: usize) -> Pos {
        let mut line = 1;
        let mut column = 1;

        for (idx, ch) in file.chars().enumerate() {
            if idx == n {
                break;
            }

            if ch == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }

        Pos { line, column }
    }

    fn max_line_number_size(pos1: Pos, pos2: Pos) -> usize {
        // XXX: Probably useless since it's kinda guaranteed that `span.hi` is always
        //      bigger than `span.lo`
        max(pos1.line.literal_size(), pos2.line.literal_size())
    }

    fn generate_line(
        buf: &mut String,
        line: &str,
        line_number: usize,
        max_line_number_size: usize,
    ) {
        buf.push_str(&format!(
            "{} {}\n",
            format!(
                "{}{}|",
                line_number,
                " ".repeat(max_line_number_size - line_number.literal_size() + 1)
            )
            .bright_blue()
            .bold(),
            line
        ));
    }

    fn write_marker(buf: &mut String, blank_line: &str, padding: usize, count: usize) {
        if count == 0 {
            return;
        }

        buf.push_str(&format!(
            "{}{}{}\n",
            blank_line,
            " ".repeat(padding),
            "^".repeat(count).bright_yellow().bold()
        ));
    }

    #[allow(clippy::too_many_arguments)]
    fn write_pos(
        &self,
        buf: &mut String,
        lines: &[&str],
        blank_line: &str,
        starting_pos: Pos,
        ending_pos: Pos,
        max_line_num_size: usize,
        print_trailing_empty_line: bool,
    ) {
        for line_num in starting_pos.line..ending_pos.line + 1 {
            let line = lines[line_num - 1].replace('\t', "    ");

            if !print_trailing_empty_line && line_num == ending_pos.line && line.is_empty() {
                break;
            }

            let tab_count = count_tab_until(lines[line_num - 1], starting_pos.column);
            let padding = if line_num == starting_pos.line {
                (starting_pos.column + tab_count * 4)
                    .checked_sub(tab_count)
                    .unwrap_or_default()
            } else {
                1
            };
            let count;

            if starting_pos.line == ending_pos.line {
                if starting_pos.column == ending_pos.column {
                    count = 1;
                } else {
                    let tab_count_until_end =
                        tab_count - count_tab_until(lines[line_num - 1], ending_pos.column);

                    count = (ending_pos.column - starting_pos.column) + 1 + 4 * tab_count_until_end
                        - tab_count_until_end;
                }
            } else if line_num == starting_pos.line {
                count = line.len().checked_sub(padding).unwrap_or(1) + 1;
            } else if line_num == ending_pos.line {
                let tab_count_until_end = count_tab_until(lines[line_num - 1], ending_pos.column);

                count = ending_pos.column + tab_count_until_end * 4 - tab_count_until_end;
            } else {
                count = line.len().checked_sub(padding).unwrap_or(1) + 1;
            }

            Self::generate_line(buf, &line, line_num, max_line_num_size);
            if !line.is_empty() {
                Self::write_marker(buf, blank_line, padding, count);
            }
        }
    }

    fn pos_to_string(pos1: Pos, pos2: Pos) -> String {
        if pos1 == pos2 {
            format!("{}:{}", pos1.line, pos1.column)
        } else {
            format!(
                "{}:{}-{}:{}",
                pos1.line, pos1.column, pos2.line, pos2.column
            )
        }
    }

    pub fn display(&self, file_name: &str, file_content: &str) -> String {
        let mut buf = String::new();
        let lines = file_content.split('\n').collect::<Vec<&str>>();
        let starting_pos = self.calculate_pos_for_n(file_content, self.span.lo);
        let ending_pos = self.calculate_pos_for_n(file_content, {
            if self.span.hi == usize::MAX {
                file_content.len() - 1
            } else {
                self.span.hi
            }
        });
        let max_line_num_size = Self::max_line_number_size(starting_pos, ending_pos);
        let blank_line = format!("{} |", " ".repeat(max_line_num_size))
            .bright_blue()
            .bold()
            .to_string();
        let blank_line_with_newline = blank_line.clone() + "\n";

        buf.push_str(&format!(
            "{}: {}\n",
            if self.severity == Severity::Error {
                format!("{}[{}]", "error", self.code)
                    .bright_red()
                    .bold()
                    .to_string()
            } else if self.severity == Severity::Warning {
                format!("{}[{}]", "warning", self.code)
                    .bright_yellow()
                    .bold()
                    .to_string()
            } else {
                "info".to_string().bright_blue().bold().to_string()
            },
            self.message,
        ));
        buf.push_str(&format!(
            "{}{} {}:{}\n",
            " ".repeat(max_line_num_size),
            "-->".bright_blue().bold(),
            file_name,
            Self::pos_to_string(starting_pos, ending_pos),
        ));
        buf.push_str(&blank_line_with_newline);
        self.write_pos(
            &mut buf,
            &lines,
            &blank_line,
            starting_pos,
            ending_pos,
            max_line_num_size,
            true,
        );
        buf.push_str(&blank_line_with_newline);
        buf.push_str(
            &format!(
                "For more information about this error, try `{} explain {}`\n",
                std::env::args().next().unwrap(),
                self.code
            )
            .bold()
            .to_string(),
        );

        buf
    }
}

fn count_tab_until(line: &str, offset: usize) -> usize {
    let mut count = 0;

    for (idx, ch) in line.chars().enumerate() {
        if ch == '\t' {
            count += 1;
        }

        if idx == offset {
            break;
        }
    }

    count
}

#[derive(Debug, Clone)]
pub enum ErrorCode {
    UnknownToken,
    InvalidNumber,
    UnterminatedString,
    UnexpectedToken,
    InvalidAssignment,
    InvalidLoopControl,
    UndefinedVariable,
    #[allow(unused)]
    UndefinedFunction,
    #[allow(unused)]
    InvalidBinaryOperation,
    InvalidUnaryOperation,
    InvalidEscapeCharacter,
    InvalidType,
    ArityError,
    Return(Arc<RwLock<Value>>), // Hack to return from functions
    InvalidDataPropertySet,
    MissingProp,
    UnknownProp,
    MutabilityError,
    Break,    // Hack for breaking loops
    Continue, // Hack for `continue`
    NoMain,
}

impl Display for ErrorCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorCode::UnknownToken => write!(f, "E0001"),
            ErrorCode::InvalidNumber => write!(f, "E0002"),
            ErrorCode::UnterminatedString => write!(f, "E0003"),
            ErrorCode::UnexpectedToken => write!(f, "E0004"),
            ErrorCode::InvalidAssignment => write!(f, "E0005"),
            ErrorCode::InvalidLoopControl => write!(f, "E0006"),
            ErrorCode::UndefinedVariable => write!(f, "E0007"),
            ErrorCode::UndefinedFunction => write!(f, "E0008"),
            ErrorCode::InvalidBinaryOperation => write!(f, "E0009"),
            ErrorCode::InvalidEscapeCharacter => write!(f, "E0010"),
            ErrorCode::InvalidUnaryOperation => write!(f, "E0012"),
            ErrorCode::InvalidType => write!(f, "E0013"),
            ErrorCode::ArityError => write!(f, "E0014"),
            ErrorCode::Return(_) => write!(f, "E0000"),
            ErrorCode::InvalidDataPropertySet => write!(f, "E0015"),
            ErrorCode::MissingProp => write!(f, "E0016"),
            ErrorCode::UnknownProp => write!(f, "E0017"),
            ErrorCode::MutabilityError => write!(f, "E0018"),
            ErrorCode::Break => write!(f, "E0000"),
            ErrorCode::Continue => write!(f, "E0000"),
            ErrorCode::NoMain => write!(f, "E0019"),
        }
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Severity {
    Error,
    Warning,
    #[allow(unused)]
    Info,
}

impl Display for Severity {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Info => "info",
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Self { lo, hi }
    }

    pub fn merge(self, s2: Span) -> Self {
        Self {
            lo: self.lo,
            hi: s2.hi,
        }
    }
}

trait LiteralSize {
    fn literal_size(self) -> usize;
}

impl LiteralSize for usize {
    fn literal_size(self) -> usize {
        (0..).take_while(|i| 10usize.pow(*i) <= self).count()
    }
}
