use crate::diagnostics::{Diagnostic, ErrorCode, Severity, Span};
use crate::interner::{InternedString, INTERNER};
use std::str::Chars;

#[derive(Debug)]
pub struct Lexer<'l> {
    source_chars: Chars<'l>,
    source_len: usize,
    pos: usize,
}

impl<'l> Lexer<'l> {
    pub fn new(source: &'l str) -> Self {
        Self {
            source_chars: source.chars(),
            source_len: source.len(),
            pos: 0,
        }
    }

    fn lex_identifier(&mut self) -> Result<Token, Diagnostic> {
        let start = self.pos;
        let mut end = self.pos;
        let mut ident = String::new();
        ident.push(self.get_current());
        self.advance();

        while !self.is_eof() {
            let c = self.get_current();
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                end += 1;
                self.advance();
            } else {
                break;
            }
        }

        let kind = match ident.as_str() {
            "as" => TokenKind::As,
            "const" => TokenKind::Const,
            "mut" => TokenKind::Mut,
            "fun" => TokenKind::Fun,
            "data" => TokenKind::Data,
            "methods" => TokenKind::Methods,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "none" => TokenKind::None,
            "loop" => TokenKind::Loop,
            "iter" => TokenKind::Iter,
            "ret" => TokenKind::Ret,
            "continue" => TokenKind::Continue,
            "break" => TokenKind::Break,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            _ => TokenKind::Identifier,
        };

        let value = if kind == TokenKind::Identifier {
            TokenValue::String(INTERNER.write().intern_string(ident))
        } else {
            TokenValue::None
        };

        Ok(Token {
            value,
            kind,
            span: Span::new(start, end),
        })
    }

    fn lex_decimal_number(&mut self) -> Result<Token, Diagnostic> {
        let start = self.pos;
        let mut end = self.pos;
        let mut number = String::new();
        number.push(self.get_current());
        self.advance();

        while !self.is_eof() {
            let c = self.get_current();
            if c.is_numeric() || c == '.' {
                number.push(c);
                end += 1;
                self.advance();
            } else {
                break;
            }
        }

        let dot_count = number.matches('.').count();
        if dot_count > 1 {
            return Err(Diagnostic {
                severity: Severity::Error,
                code: ErrorCode::InvalidNumber,
                message: "Invalid number".to_string(),
                span: Span::new(start, end),
            });
        }

        let number = INTERNER.write().intern_string(number);

        Ok(Token {
            value: TokenValue::Number {
                value: number,
                kind: if dot_count == 1 {
                    NumberKind::DecFloat
                } else {
                    NumberKind::DecInt
                },
            },
            kind: TokenKind::Number,
            span: Span::new(start, end),
        })
    }

    fn lex_hex_number(&mut self) -> Result<Token, Diagnostic> {
        self.advance();
        self.advance();
        let start = self.pos;
        let mut end = self.pos;
        let mut number = String::new();
        number.push(self.get_current());
        self.advance();

        while !self.is_eof() {
            let c = self.get_current();
            if c.is_numeric() || "ABCDEFabcdef".contains(c) {
                number.push(c);
                end += 1;
                self.advance();
            } else {
                break;
            }
        }

        let number = INTERNER.write().intern_string(number);

        Ok(Token {
            value: TokenValue::Number {
                value: number,
                kind: NumberKind::Hex,
            },
            kind: TokenKind::Number,
            span: Span::new(start, end),
        })
    }

    fn lex_binary_number(&mut self) -> Result<Token, Diagnostic> {
        self.advance();
        self.advance();
        let start = self.pos;
        let mut end = self.pos;
        let mut number = String::new();
        number.push(self.get_current());
        self.advance();

        while !self.is_eof() {
            let c = self.get_current();
            if "01".contains(c) {
                number.push(c);
                end += 1;
                self.advance();
            } else {
                break;
            }
        }

        let number = INTERNER.write().intern_string(number);

        Ok(Token {
            value: TokenValue::Number {
                value: number,
                kind: NumberKind::Bin,
            },
            kind: TokenKind::Number,
            span: Span::new(start, end),
        })
    }

    fn lex_octal_number(&mut self) -> Result<Token, Diagnostic> {
        self.advance();
        self.advance();
        let start = self.pos;
        let mut end = self.pos;
        let mut number = String::new();
        number.push(self.get_current());
        self.advance();

        while !self.is_eof() {
            let c = self.get_current();
            if "01234567".contains(c) {
                number.push(c);
                end += 1;
                self.advance();
            } else {
                break;
            }
        }

        let number = INTERNER.write().intern_string(number);

        Ok(Token {
            value: TokenValue::Number {
                value: number,
                kind: NumberKind::Oct,
            },
            kind: TokenKind::Number,
            span: Span::new(start, end),
        })
    }

    fn lex_number(&mut self) -> Result<Token, Diagnostic> {
        match self.try_peek_next() {
            Some('x') => self.lex_hex_number(),
            Some('b') => self.lex_binary_number(),
            Some('o') => self.lex_octal_number(),
            _ => self.lex_decimal_number(),
        }
    }

    fn single_char_token(&mut self, kind: TokenKind) -> Token {
        let start = self.pos;
        let end = self.pos;
        self.advance();
        Token {
            value: TokenValue::None,
            kind,
            span: Span::new(start, end),
        }
    }

    fn maybe_double_char_token(
        &mut self,
        kind: TokenKind,
        next: &[char],
        double_kind: &[TokenKind],
    ) -> Token {
        let start = self.pos;
        let end = self.pos;
        self.advance();

        if !self.is_eof() {
            let ch = self.get_current();
            if next.contains(&ch) {
                self.advance();
                return Token {
                    value: TokenValue::None,
                    kind: double_kind[next.iter().position(|&c| c == ch).unwrap()],
                    span: Span::new(start, end + 1),
                };
            }
        }

        Token {
            value: TokenValue::None,
            kind,
            span: Span::new(start, end),
        }
    }

    fn lex_string(&mut self) -> Result<Token, Diagnostic> {
        let start = self.pos;
        let mut end = self.pos;
        let mut string = String::new();
        self.advance();

        loop {
            if self.is_eof() {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    code: ErrorCode::UnterminatedString,
                    message: "Unterminated string".to_string(),
                    span: Span::new(start, end),
                });
            }

            end += 1;
            let c = self.get_current();

            if c == '"' {
                self.advance();
                break;
            } else if c == '\\' {
                let mut esc = "\\".to_string();
                self.advance();

                if self.is_eof() {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        code: ErrorCode::UnexpectedToken,
                        message: "Unexpected eof".to_string(),
                        span: Span::new(start, end + 1),
                    });
                }
                esc += &self.get_current().to_string();
                end += 1;
                let escape_char = match esc.as_str() {
                    "\\n" => '\n',
                    "\\t" => '\t',
                    "\\r" => '\r',
                    "\\\\" => '\\',
                    "\\\"" => '\"',
                    _ => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::InvalidEscapeCharacter,
                            message: "Invalid escape character".to_string(),
                            span: Span {
                                lo: end - 2,
                                hi: end,
                            },
                        })
                    }
                };
                string.push(escape_char);
                self.advance();
                continue;
            }
            string.push(c);
            self.advance();
        }

        Ok(Token {
            value: TokenValue::String(INTERNER.write().intern_string(string)),
            kind: TokenKind::String,
            span: Span::new(start, end - 1),
        })
    }

    pub fn lex_once(&mut self) -> Result<Token, Diagnostic> {
        if self.is_eof() {
            return Ok(Token {
                value: TokenValue::None,
                kind: TokenKind::Eof,
                span: Span::new(self.pos, self.pos),
            });
        }

        match self.get_current() {
            '{' => Ok(self.single_char_token(TokenKind::LeftBrace)),
            '}' => Ok(self.single_char_token(TokenKind::RightBrace)),
            '(' => Ok(self.single_char_token(TokenKind::LeftParen)),
            ')' => Ok(self.single_char_token(TokenKind::RightParen)),
            '[' => Ok(self.single_char_token(TokenKind::LeftBracket)),
            ']' => Ok(self.single_char_token(TokenKind::RightBracket)),
            ',' => Ok(self.single_char_token(TokenKind::Comma)),
            '.' => Ok(self.single_char_token(TokenKind::Dot)),
            ';' => Ok(self.single_char_token(TokenKind::Semicolon)),
            '~' => Ok(self.single_char_token(TokenKind::Tilde)),
            '@' => Ok(self.single_char_token(TokenKind::At)),
            ':' => Ok(self.single_char_token(TokenKind::Colon)),
            '+' => Ok(self.maybe_double_char_token(TokenKind::Plus, &['='], &[TokenKind::PlusEq])),
            '-' => {
                Ok(self.maybe_double_char_token(TokenKind::Minus, &['='], &[TokenKind::MinusEq]))
            }
            '*' => Ok(self.maybe_double_char_token(
                TokenKind::Asterisk,
                &['='],
                &[TokenKind::AsteriskEq],
            )),
            '/' => {
                if self.try_peek_next() == Some('/') {
                    self.advance();
                    while !self.is_eof() {
                        if self.get_current() == '\n' {
                            self.advance();
                            if !self.is_eof() {
                                return self.lex_once();
                            }
                        }
                        self.advance();
                    }

                    return self.lex_once();
                }
                Ok(self.maybe_double_char_token(TokenKind::Slash, &['='], &[TokenKind::SlashEq]))
            }
            '%' => Ok(self.maybe_double_char_token(
                TokenKind::Percent,
                &['='],
                &[TokenKind::PercentEq],
            )),
            '^' => {
                Ok(self.maybe_double_char_token(TokenKind::Caret, &['='], &[TokenKind::CaretEq]))
            }
            '&' => Ok(self.maybe_double_char_token(
                TokenKind::Ampersand,
                &['&', '='],
                &[TokenKind::DoubleAmpersand, TokenKind::AmpersandEq],
            )),
            '|' => Ok(self.maybe_double_char_token(
                TokenKind::Pipe,
                &['|', '='],
                &[TokenKind::DoublePipe, TokenKind::PipeEq],
            )),
            '!' => Ok(self.maybe_double_char_token(
                TokenKind::Exclamation,
                &['='],
                &[TokenKind::ExclamationEq],
            )),
            '=' => Ok(self.maybe_double_char_token(
                TokenKind::Equal,
                &['='],
                &[TokenKind::DoubleEqual],
            )),
            '<' => Ok({
                if let Some('<') = self.try_peek_next() {
                    self.advance();

                    if let Some('=') = self.try_peek_next() {
                        self.advance();
                        self.advance();

                        Token {
                            value: TokenValue::None,
                            kind: TokenKind::DoubleLeftAngleEq,
                            span: Span::new(self.pos - 3, self.pos - 1),
                        }
                    } else {
                        self.advance();
                        Token {
                            value: TokenValue::None,
                            kind: TokenKind::DoubleLeftAngle,
                            span: Span::new(self.pos - 2, self.pos - 1),
                        }
                    }
                } else if let Some('=') = self.try_peek_next() {
                    self.advance();
                    self.advance();

                    Token {
                        value: TokenValue::None,
                        kind: TokenKind::LeftAngleEq,
                        span: Span::new(self.pos - 2, self.pos - 1),
                    }
                } else {
                    self.advance();
                    Token {
                        value: TokenValue::None,
                        kind: TokenKind::LeftAngle,
                        span: Span::new(self.pos - 1, self.pos - 1),
                    }
                }
            }),
            '>' => Ok({
                if let Some('>') = self.try_peek_next() {
                    self.advance();

                    if let Some('=') = self.try_peek_next() {
                        self.advance();
                        self.advance();

                        Token {
                            value: TokenValue::None,
                            kind: TokenKind::DoubleRightAngleEq,
                            span: Span::new(self.pos - 3, self.pos - 1),
                        }
                    } else {
                        self.advance();
                        Token {
                            value: TokenValue::None,
                            kind: TokenKind::DoubleRightAngle,
                            span: Span::new(self.pos - 2, self.pos - 1),
                        }
                    }
                } else if let Some('=') = self.try_peek_next() {
                    self.advance();
                    self.advance();

                    Token {
                        value: TokenValue::None,
                        kind: TokenKind::RightAngleEq,
                        span: Span::new(self.pos - 2, self.pos - 1),
                    }
                } else {
                    self.advance();
                    Token {
                        value: TokenValue::None,
                        kind: TokenKind::RightAngle,
                        span: Span::new(self.pos - 1, self.pos - 1),
                    }
                }
            }),
            '"' => self.lex_string(),
            '0'..='9' => self.lex_number(),
            'a'..='z' | 'A'..='Z' => self.lex_identifier(),
            c if c.is_whitespace() => {
                self.advance();
                self.lex_once()
            }
            _ => {
                let e = Err(Diagnostic {
                    severity: Severity::Error,
                    code: ErrorCode::UnknownToken,
                    message: "Unknown character".to_string(),
                    span: self.current_char_span(),
                });
                self.advance();
                e
            }
        }
    }

    fn try_peek_next(&self) -> Option<char> {
        self.source_chars.clone().nth(self.pos + 1)
    }

    fn current_char_span(&self) -> Span {
        Span {
            lo: self.pos,
            hi: self.pos,
        }
    }

    fn get_current(&mut self) -> char {
        self.source_chars.clone().nth(self.pos).unwrap()
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.source_len
    }

    fn advance(&mut self) {
        self.pos += 1;
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenValue {
    String(InternedString),
    Number {
        kind: NumberKind,
        value: InternedString,
    },
    None,
}

impl TokenValue {
    pub fn get_string(&self) -> InternedString {
        match self {
            Self::String(s) => *s,
            _ => panic!("TokenValue::get_string() called on non TokenValue::String"),
        }
    }
    pub fn get_number(&self) -> (InternedString, NumberKind) {
        match self {
            Self::Number { value, kind } => (*value, *kind),
            _ => panic!("TokenValue::get_string() called on non TokenValue::String"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum NumberKind {
    Bin,
    Hex,
    Oct,
    DecInt,
    DecFloat,
}

impl NumberKind {
    pub fn radix(&self) -> u32 {
        match self {
            Self::Bin => 2,
            Self::Hex => 16,
            Self::Oct => 8,
            Self::DecInt | Self::DecFloat => 10,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum TokenKind {
    String,
    Number,
    Identifier,
    As,
    Const,
    Mut,
    Fun,
    Data,
    Methods,
    Continue,
    Break,
    Loop,
    Iter,
    Ret,
    If,
    Else,
    True,
    False,
    None,
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Asterisk,
    AsteriskEq,
    Slash,
    SlashEq,
    Percent,
    PercentEq,
    Caret,
    CaretEq,
    Ampersand,
    AmpersandEq,
    DoubleAmpersand,
    Pipe,
    PipeEq,
    DoublePipe,
    Tilde,
    Exclamation,
    ExclamationEq,
    LeftAngle,
    LeftAngleEq,
    DoubleLeftAngle,
    DoubleLeftAngleEq,
    RightAngle,
    RightAngleEq,
    DoubleRightAngle,
    DoubleRightAngleEq,
    Equal,
    DoubleEqual,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Colon,
    Semicolon,
    At,
    Eof,
}
