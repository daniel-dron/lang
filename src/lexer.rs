use crate::{ast::Span, scanner::Scanner};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    // keywords
    Let,
    Function,
    If,
    Else,
    For,
    On,
    Return,

    // literals
    Identifier,
    Number,
    String,
    Bool,

    // operators
    Plus,
    Minus,
    Equal,
    Star,
    Slash,
    Greater,
    Less,
    And,
    Or,
    Bang, // !

    // misc
    Semicolon,
    Comma,
    Dot,
    Colon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    At,

    ArrowRight,

    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub ty: TokenType,
    pub lexeme: String,

    // in source
    pub line: usize,
    pub column: usize,
    pub span: Span,
}

pub struct Lexer {
    tokens: Vec<Token>,

    start: usize,
}

impl Lexer {
    pub fn from(scanner: Scanner) -> Self {
        // generate tokens eagerly
        let mut lexer = Self {
            tokens: Vec::new(),
            start: 0,
        };

        lexer.generate_tokens(scanner);

        lexer
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Token> {
        self.tokens.iter()
    }

    fn add_token(&mut self, ty: TokenType, scanner: &Scanner) {
        let lexeme = scanner.get_lexeme(self.start, scanner.current);
        let span = Span::new(self.start, scanner.current);
        let (line, column) = scanner.get_line_diagnostics();
        self.tokens.push(Token {
            ty,
            lexeme,
            line,
            column,
            span,
        });
    }

    fn string(&mut self, scanner: &mut Scanner) {
        while let Some(c) = scanner.peek() {
            if c == '"' {
                break;
            }

            scanner.advance();
        }
        
        let (line, column) = scanner.get_line_diagnostics();

        if scanner.is_at_end() {
            panic!(
                "Unterminated string literal that started at line {}, column {}",
                line, column
            );
        }

        scanner.advance();

        let span = Span::new(self.start, scanner.current);
        let lexeme = scanner.get_lexeme(self.start + 1, scanner.current - 1);
        self.tokens.push(Token {
            ty: TokenType::String,
            lexeme,
            line,
            column,
            span,
        });
    }

    fn number(&mut self, scanner: &mut Scanner) {
        while let Some(c) = scanner.peek() {
            if c.is_numeric() {
                scanner.advance();
            } else {
                break;
            }
        }

        if scanner.peek() == Some('.') && scanner.peek_next().map_or(false, |c| c.is_numeric()) {
            scanner.advance();

            while let Some(c) = scanner.peek() {
                if c.is_numeric() {
                    scanner.advance();
                } else {
                    break;
                }
            }
        }

        self.add_token(TokenType::Number, scanner);
    }

    fn identifier(&mut self, scanner: &mut Scanner) {
        while let Some(c) = scanner.peek() {
            if c.is_alphanumeric() || c == '_' {
                scanner.advance();
            } else {
                break;
            }
        }

        let lexeme = scanner.get_lexeme(self.start, scanner.current);
        let ty = match lexeme.as_str() {
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            "for" => TokenType::For,
            "on" => TokenType::On,
            "and" => TokenType::And,
            "or" => TokenType::Or,
            "false" | "true" => TokenType::Bool,
            "return" => TokenType::Return,
            _ => TokenType::Identifier,
        };

        self.add_token(ty, scanner);
    }

    fn generate_tokens(&mut self, mut scanner: Scanner) {
        while !scanner.is_at_end() {
            self.start = scanner.current;

            let c = match scanner.advance() {
                Some(ch) => ch,
                None => break,
            };

            match c {
                ' ' | '\n' | '\t' | '\r' => {
                    // white space type characters, do nothing
                }

                // handle division operator and comments
                '/' => {
                    if scanner.peek() == Some('/') {
                        while scanner.peek() != Some('\n') && !scanner.is_at_end() {
                            scanner.advance();
                        }
                    } else if scanner.peek() == Some('*') {
                        scanner.advance();

                        let mut nested = 1;
                        while nested > 0 && !scanner.is_at_end() {
                            if scanner.peek() == Some('*') && scanner.peek_next() == Some('/') {
                                scanner.advance(); // consume '*'
                                scanner.advance(); // consume '/'
                                nested -= 1;
                            } else if scanner.peek() == Some('/')
                                && scanner.peek_next() == Some('*')
                            {
                                scanner.advance(); // consume '/'
                                scanner.advance(); // consume '*'
                                nested += 1;
                            } else {
                                scanner.advance();
                            }
                        }

                        if scanner.is_at_end() && nested > 0 {
                            let (line, column) = scanner.get_line_diagnostics();
                            panic!(
                                "Unterminated multi-line comment at line {}, column {}",
                                line, column
                            );
                        }
                    } else {
                        // Just a regular division operator
                        self.add_token(TokenType::Slash, &scanner);
                    }
                }

                // simple one character tokens
                '+' => self.add_token(TokenType::Plus, &scanner),
                '-' => {
                    if scanner.peek() == Some('>') {
                        scanner.advance();
                        self.add_token(TokenType::ArrowRight, &scanner)
                    } else {
                        self.add_token(TokenType::Minus, &scanner)
                    }
                }
                '*' => self.add_token(TokenType::Star, &scanner),
                '=' => self.add_token(TokenType::Equal, &scanner),
                '>' => self.add_token(TokenType::Greater, &scanner),
                '<' => self.add_token(TokenType::Less, &scanner),
                ';' => self.add_token(TokenType::Semicolon, &scanner),
                ',' => self.add_token(TokenType::Comma, &scanner),
                '.' => self.add_token(TokenType::Dot, &scanner),
                '(' => self.add_token(TokenType::LeftParen, &scanner),
                ')' => self.add_token(TokenType::RightParen, &scanner),
                '{' => self.add_token(TokenType::LeftBrace, &scanner),
                '}' => self.add_token(TokenType::RightBrace, &scanner),
                '[' => self.add_token(TokenType::LeftBracket, &scanner),
                ']' => self.add_token(TokenType::RightBracket, &scanner),
                '!' => self.add_token(TokenType::Bang, &scanner),
                '@' => self.add_token(TokenType::At, &scanner),
                ':' => self.add_token(TokenType::Colon, &scanner),

                '"' => self.string(&mut scanner),

                '0'..='9' => self.number(&mut scanner),

                c if c.is_alphabetic() || c == '_' => self.identifier(&mut scanner),

                _ => {
                    panic!("Unknown symbol {:?}", c)
                }
            }
        }

        self.add_token(TokenType::EOF, &scanner);
    }
}
