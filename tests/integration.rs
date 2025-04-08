mod integration_tests {
    use lang_core::lexer::{Lexer, TokenType};
    use lang_core::scanner::Scanner;

    #[test]
    fn test_small_program() {
        let source = r#"
            // A simple example of our language
            let num = 42;
            let greeting = "Hello, World!";
            
            fn add(a, b) {
                return a + b;
            }
            
            let result = add(num, 10);
            
            if result > 50 {
                let message = "Result is greater than 50";
            } else {
                let message = "Result is 50 or less";
            }
            
            for i = 0; i < 5; i = i + 1 {
                // Loop body
            }
        "#;
        
        let scanner = Scanner::new(source);
        let lexer = Lexer::from(scanner);
        
        // Just ensure we can tokenize the whole program without errors
        let tokens: Vec<_> = lexer.iter().collect();

        for token in tokens.clone() {
            println!("{:?}", token);
        }
        
        // Make assertions about expected tokens
        assert!(tokens.len() > 0);
        assert!(matches!(tokens.last().unwrap().ty, TokenType::EOF));
        
        // Count important tokens
        let fn_count = tokens.iter().filter(|t| matches!(t.ty, TokenType::Function)).count();
        let let_count = tokens.iter().filter(|t| matches!(t.ty, TokenType::Let)).count();
        let if_count = tokens.iter().filter(|t| matches!(t.ty, TokenType::If)).count();
        let for_count = tokens.iter().filter(|t| matches!(t.ty, TokenType::For)).count();
        
        assert_eq!(fn_count, 1);
        assert!(let_count >= 5);
        assert_eq!(if_count, 1);
        assert_eq!(for_count, 1);
    }
}