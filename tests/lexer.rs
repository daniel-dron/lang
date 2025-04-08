mod lexer_tests {
    use lang_core::lexer::{Lexer, TokenType};
    use lang_core::scanner::Scanner;

    #[test]
    fn test_lexer_keywords() {
        let source = "let fn if for on false true";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);

        let tokens: Vec<_> = lexer.iter().collect();

        assert_eq!(tokens.len(), 8);

        match tokens[0].ty {
            TokenType::Let => {}
            _ => panic!("Expected Let token, got {:?}", tokens[0].ty),
        }

        match tokens[1].ty {
            TokenType::Function => {}
            _ => panic!("Expected Function token, got {:?}", tokens[1].ty),
        }

        match tokens[2].ty {
            TokenType::If => {}
            _ => panic!("Expected If token, got {:?}", tokens[2].ty),
        }

        match tokens[3].ty {
            TokenType::For => {}
            _ => panic!("Expected For token, got {:?}", tokens[3].ty),
        }

        match tokens[4].ty {
            TokenType::On => {}
            _ => panic!("Expected On token, got {:?}", tokens[4].ty),
        }

        match tokens[5].ty {
            TokenType::Bool => {}
            _ => panic!("Expected On token, got {:?}", tokens[5].ty),
        }

        match tokens[6].ty {
            TokenType::Bool => {}
            _ => panic!("Expected On token, got {:?}", tokens[6].ty),
        }
    }

    #[test]
    fn test_lexer_operators() {
        let source = "+ - = * / > <";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);

        let tokens: Vec<_> = lexer.iter().collect();

        assert_eq!(tokens.len(), 8); // 7 operators + EOF

        let expected_types = [
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Equal,
            TokenType::Star,
            TokenType::Slash,
            TokenType::Greater,
            TokenType::Less,
            TokenType::EOF,
        ];

        for (i, expected_type) in expected_types.iter().enumerate() {
            assert!(
                matches!(&tokens[i].ty, type_val if std::mem::discriminant(type_val) == std::mem::discriminant(&expected_type))
            );
        }
    }

    #[test]
    fn test_lexer_misc() {
        let source = "; , . ( ) { } [ ]";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);

        let tokens: Vec<_> = lexer.iter().collect();

        assert_eq!(tokens.len(), 10); // + EOF

        let expected_types = [
            TokenType::Semicolon,
            TokenType::Comma,
            TokenType::Dot,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::RightBrace,
            TokenType::LeftBracket,
            TokenType::RightBracket,
            TokenType::EOF,
        ];

        for (i, expected_type) in expected_types.iter().enumerate() {
            assert!(
                matches!(&tokens[i].ty, type_val if std::mem::discriminant(type_val) == std::mem::discriminant(&expected_type))
            );
        }
    }

    #[test]
    fn test_lexer_string() {
        let source = "\"hello world\"";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);

        let tokens: Vec<_> = lexer.iter().collect();

        assert_eq!(tokens.len(), 2); // 1 string + EOF

        match &tokens[0].ty {
            TokenType::String => {}
            _ => panic!("Expected String token, got {:?}", tokens[0].ty),
        }

        assert_eq!(tokens[0].lexeme, "hello world");
    }

    #[test]
    fn test_lexer_numbers() {
        let source = "123 456.789";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);

        let tokens: Vec<_> = lexer.iter().collect();

        assert_eq!(tokens.len(), 3); // 2 numbers + EOF

        match &tokens[0].ty {
            TokenType::Number => {}
            _ => panic!("Expected Number token, got {:?}", tokens[0].ty),
        }
        assert_eq!(tokens[0].lexeme, "123");

        match &tokens[1].ty {
            TokenType::Number => {}
            _ => panic!("Expected Number token, got {:?}", tokens[1].ty),
        }
        assert_eq!(tokens[1].lexeme, "456.789");
    }

    #[test]
    fn test_lexer_identifiers() {
        let source = "foo bar_baz qux123";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);

        let tokens: Vec<_> = lexer.iter().collect();

        assert_eq!(tokens.len(), 4); // 3 identifiers + EOF

        for i in 0..3 {
            match &tokens[i].ty {
                TokenType::Identifier => {}
                _ => panic!("Expected Identifier token, got {:?}", tokens[i].ty),
            }
        }

        assert_eq!(tokens[0].lexeme, "foo");
        assert_eq!(tokens[1].lexeme, "bar_baz");
        assert_eq!(tokens[2].lexeme, "qux123");
    }

    #[test]
    fn test_lexer_line_column_tracking() {
        let source = "let x = 42\nfn add(a, b) {\n  return a + b\n}";
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);

        let tokens: Vec<_> = lexer.iter().collect();

        // Check "let" token position
        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[0].column, 0);

        // Find tokens on different lines
        let fn_token = tokens.iter().find(|t| t.lexeme == "fn").unwrap();
        assert_eq!(fn_token.line, 2);

        // Find a token on the third line
        let plus_token = tokens
            .iter()
            .find(|t| matches!(t.ty, TokenType::Plus))
            .unwrap();
        assert_eq!(plus_token.line, 3);
    }

    #[test]
    #[should_panic(expected = "Unterminated string literal")]
    fn test_lexer_unterminated_string() {
        let source = "\"unterminated string";
        let scanner = Scanner::new(source);
        let _ = Lexer::from(scanner); // Should panic due to unterminated string
    }

    #[test]
    fn test_complex_program() {
        let source = r#"
            let x = 42;
            let message = "hello world";
            
            fn calculate(a, b) {
                let result = a + b * 2;
                return result;
            }
            
            if x > 10 {
                for i = 0; i < 5; i = i + 1 {
                    x = x - 1;
                }
            }
        "#;

        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);

        let tokens: Vec<_> = lexer.iter().collect();

        // Verify we have a reasonable number of tokens
        assert!(tokens.len() > 30);

        // Verify key tokens are present with correct counts
        let let_count = tokens
            .iter()
            .filter(|t| matches!(t.ty, TokenType::Let))
            .count();
        assert_eq!(let_count, 3); // 3 let statements

        let fn_count = tokens
            .iter()
            .filter(|t| matches!(t.ty, TokenType::Function))
            .count();
        assert_eq!(fn_count, 1); // 1 function

        let if_count = tokens
            .iter()
            .filter(|t| matches!(t.ty, TokenType::If))
            .count();
        assert_eq!(if_count, 1); // 1 if statement

        let for_count = tokens
            .iter()
            .filter(|t| matches!(t.ty, TokenType::For))
            .count();
        assert_eq!(for_count, 1); // 1 for loop

        // Verify string token
        let string_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| matches!(t.ty, TokenType::String))
            .collect();
        assert_eq!(string_tokens.len(), 1);
        assert_eq!(string_tokens[0].lexeme, "hello world");

        // Last token should be EOF
        match tokens.last().unwrap().ty {
            TokenType::EOF => {}
            _ => panic!("Expected last token to be EOF"),
        }
    }
}
