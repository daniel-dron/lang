use super::{ast::Span, scanner::Scanner};

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
    Type,
    Binds,

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

    // track start of current token
    start_line: usize,
    start_column: usize,
}

impl Lexer {
    pub fn from(scanner: Scanner) -> Self {
        // generate tokens eagerly
        let mut lexer = Self {
            tokens: Vec::new(),
            start: 0,
            start_line: 1,
            start_column: 0,
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
        self.tokens.push(Token {
            ty,
            lexeme,
            line: self.start_line,
            column: self.start_column,
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

        if scanner.is_at_end() {
            panic!(
                "Unterminated string literal that started at line {}, column {}",
                self.start_line, self.start_column
            );
        }

        scanner.advance();

        let span = Span::new(self.start, scanner.current);
        let lexeme = scanner.get_lexeme(self.start + 1, scanner.current - 1);
        self.tokens.push(Token {
            ty: TokenType::String,
            lexeme,
            line: self.start_line,
            column: self.start_column,
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
            "type" => TokenType::Type,
            "binds" => TokenType::Binds,
            _ => TokenType::Identifier,
        };

        self.add_token(ty, scanner);
    }

    fn generate_tokens(&mut self, mut scanner: Scanner) {
        while !scanner.is_at_end() {
            self.start = scanner.current;

            let (line, column) = scanner.get_line_diagnostics();
            self.start_line = line;
            self.start_column = column;

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

        // EOF token should use the most up to date line diagnostics
        let (line, column) = scanner.get_line_diagnostics();
        self.start_line = line;
        self.start_column = column;
        self.add_token(TokenType::EOF, &scanner);
    }
}

#[cfg(test)]
mod tests {
    use super::super::scanner::Scanner;
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let source = "let x = 5;";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].ty, TokenType::Let);
        assert_eq!(tokens[1].ty, TokenType::Identifier);
        assert_eq!(tokens[1].lexeme, "x");
        assert_eq!(tokens[2].ty, TokenType::Equal);
        assert_eq!(tokens[3].ty, TokenType::Number);
        assert_eq!(tokens[3].lexeme, "5");
        assert_eq!(tokens[4].ty, TokenType::Semicolon);
    }

    #[test]
    fn test_operators() {
        let source = "+ - * / > < = ! -> and or";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens[0].ty, TokenType::Plus);
        assert_eq!(tokens[1].ty, TokenType::Minus);
        assert_eq!(tokens[2].ty, TokenType::Star);
        assert_eq!(tokens[3].ty, TokenType::Slash);
        assert_eq!(tokens[4].ty, TokenType::Greater);
        assert_eq!(tokens[5].ty, TokenType::Less);
        assert_eq!(tokens[6].ty, TokenType::Equal);
        assert_eq!(tokens[7].ty, TokenType::Bang);
        assert_eq!(tokens[8].ty, TokenType::ArrowRight);
        assert_eq!(tokens[9].ty, TokenType::And);
        assert_eq!(tokens[10].ty, TokenType::Or);
    }

    #[test]
    fn test_keywords() {
        let source = "let fn if else for on return";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens[0].ty, TokenType::Let);
        assert_eq!(tokens[1].ty, TokenType::Function);
        assert_eq!(tokens[2].ty, TokenType::If);
        assert_eq!(tokens[3].ty, TokenType::Else);
        assert_eq!(tokens[4].ty, TokenType::For);
        assert_eq!(tokens[5].ty, TokenType::On);
        assert_eq!(tokens[6].ty, TokenType::Return);
    }

    #[test]
    fn test_delimiters() {
        let source = "( ) { } [ ] ; , . : @";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens[0].ty, TokenType::LeftParen);
        assert_eq!(tokens[1].ty, TokenType::RightParen);
        assert_eq!(tokens[2].ty, TokenType::LeftBrace);
        assert_eq!(tokens[3].ty, TokenType::RightBrace);
        assert_eq!(tokens[4].ty, TokenType::LeftBracket);
        assert_eq!(tokens[5].ty, TokenType::RightBracket);
        assert_eq!(tokens[6].ty, TokenType::Semicolon);
        assert_eq!(tokens[7].ty, TokenType::Comma);
        assert_eq!(tokens[8].ty, TokenType::Dot);
        assert_eq!(tokens[9].ty, TokenType::Colon);
        assert_eq!(tokens[10].ty, TokenType::At);
    }

    #[test]
    fn test_number_literals() {
        let source = "123 45.67";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens[0].ty, TokenType::Number);
        assert_eq!(tokens[0].lexeme, "123");
        assert_eq!(tokens[1].ty, TokenType::Number);
        assert_eq!(tokens[1].lexeme, "45.67");
    }

    #[test]
    fn test_string_literals() {
        let source = "\"hello\" \"world\"";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens[0].ty, TokenType::String);
        assert_eq!(tokens[0].lexeme, "hello");
        assert_eq!(tokens[1].ty, TokenType::String);
        assert_eq!(tokens[1].lexeme, "world");
    }

    #[test]
    fn test_boolean_literals() {
        let source = "true false";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens[0].ty, TokenType::Bool);
        assert_eq!(tokens[0].lexeme, "true");
        assert_eq!(tokens[1].ty, TokenType::Bool);
        assert_eq!(tokens[1].lexeme, "false");
    }

    #[test]
    fn test_identifiers() {
        let source = "x y_1 test_var _hidden";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens[0].ty, TokenType::Identifier);
        assert_eq!(tokens[0].lexeme, "x");
        assert_eq!(tokens[1].ty, TokenType::Identifier);
        assert_eq!(tokens[1].lexeme, "y_1");
        assert_eq!(tokens[2].ty, TokenType::Identifier);
        assert_eq!(tokens[2].lexeme, "test_var");
        assert_eq!(tokens[3].ty, TokenType::Identifier);
        assert_eq!(tokens[3].lexeme, "_hidden");
    }

    #[test]
    fn test_single_line_comments() {
        let source = "let x = 5; // This is a comment\nlet y = 10;";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens.len(), 11);
        assert_eq!(tokens[0].ty, TokenType::Let);
        assert_eq!(tokens[4].ty, TokenType::Semicolon);
        assert_eq!(tokens[5].ty, TokenType::Let);
        assert_eq!(tokens[5].line, 2);
    }

    #[test]
    fn test_multi_line_comments() {
        let source = "let x = 5; /* This is a\nmulti-line comment */\nlet y = 10;";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens.len(), 11);
        assert_eq!(tokens[0].ty, TokenType::Let);
        assert_eq!(tokens[4].ty, TokenType::Semicolon);
        assert_eq!(tokens[5].ty, TokenType::Let);
        assert_eq!(tokens[5].line, 3);
    }

    #[test]
    fn test_nested_comments() {
        let source = "/* outer /* nested */ comment */";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens.len(), 1); // Just EOF
    }

    #[test]
    fn test_mixed_tokens() {
        let source = "fn main() {\n  let x = 42;\n  if x > 10 {\n    return true;\n  }\n}";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens.len(), 21);
        assert_eq!(tokens[0].ty, TokenType::Function);
        assert_eq!(tokens[1].ty, TokenType::Identifier);
        assert_eq!(tokens[1].lexeme, "main");

        // Check line numbers
        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[5].line, 2); // let
        assert_eq!(tokens[10].line, 3); // if
        assert_eq!(tokens[15].line, 4); // return
    }

    #[test]
    fn test_token_position_tracking() {
        // Test to verify the line and column are correctly tracking the START position
        let source = "let x = 5;\nfn test() {\n  return true;\n}";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        // First line tokens
        assert_eq!(tokens[0].ty, TokenType::Let);
        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[0].column, 0); // 'let' should start at column 0

        assert_eq!(tokens[1].ty, TokenType::Identifier);
        assert_eq!(tokens[1].lexeme, "x");
        assert_eq!(tokens[1].line, 1);
        assert_eq!(tokens[1].column, 4); // 'x' should start at column 4

        // Second line tokens
        assert_eq!(tokens[5].ty, TokenType::Function);
        assert_eq!(tokens[5].line, 2);
        assert_eq!(tokens[5].column, 0); // 'fn' should start at column 0

        assert_eq!(tokens[6].ty, TokenType::Identifier);
        assert_eq!(tokens[6].lexeme, "test");
        assert_eq!(tokens[6].line, 2);
        assert_eq!(tokens[6].column, 3); // 'test' should start at column 3

        // Third line tokens
        assert_eq!(tokens[10].ty, TokenType::Return);
        assert_eq!(tokens[10].line, 3);
        assert_eq!(tokens[10].column, 2); // 'return' should start at column 2
    }

    #[test]
    fn test_multichar_token_position() {
        // Test specifically for tokens with multiple characters
        let source = "fn longIdentifier 12345 \"string literal\"";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        // 'function' keyword
        assert_eq!(tokens[0].ty, TokenType::Function);
        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[0].column, 0); // Should point to the start
        assert_eq!(tokens[0].span.start, 0);
        assert_eq!(tokens[0].span.end, 2);

        // 'longIdentifier'
        assert_eq!(tokens[1].ty, TokenType::Identifier);
        assert_eq!(tokens[1].lexeme, "longIdentifier");
        assert_eq!(tokens[1].line, 1);
        assert_eq!(tokens[1].column, 3); // Should point to the start

        // Number literal
        assert_eq!(tokens[2].ty, TokenType::Number);
        assert_eq!(tokens[2].lexeme, "12345");
        assert_eq!(tokens[2].line, 1);
        assert_eq!(tokens[2].column, 18); // Should point to the start

        // String literal
        assert_eq!(tokens[3].ty, TokenType::String);
        assert_eq!(tokens[3].lexeme, "string literal");
        assert_eq!(tokens[3].line, 1);
        assert_eq!(tokens[3].column, 24); // Should point to the opening quote
    }

    #[test]
    fn test_arrow_token_position() {
        // Special test for the arrow token which is multi-character
        let source = "x -> y";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens[0].ty, TokenType::Identifier);
        assert_eq!(tokens[0].lexeme, "x");
        assert_eq!(tokens[0].column, 0);

        assert_eq!(tokens[1].ty, TokenType::ArrowRight);
        assert_eq!(tokens[1].lexeme, "->");
        assert_eq!(tokens[1].column, 2); // The arrow should start at column 2

        assert_eq!(tokens[2].ty, TokenType::Identifier);
        assert_eq!(tokens[2].lexeme, "y");
        assert_eq!(tokens[2].column, 5);
    }

    #[test]
    fn test_empty_input() {
        let source = "";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        let tokens: Vec<Token> = lexer.iter().cloned().collect();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].ty, TokenType::EOF);
    }

    #[test]
    #[should_panic(expected = "Unterminated string")]
    fn test_unterminated_string() {
        let source = "\"unterminated";
        let scanner = Scanner::new(source);
        let _lexer = Lexer::from(scanner);
    }

    #[test]
    #[should_panic(expected = "Unterminated multi-line comment")]
    fn test_unterminated_comment() {
        let source = "/* unterminated comment";
        let scanner = Scanner::new(source);
        let _lexer = Lexer::from(scanner);
    }
}
