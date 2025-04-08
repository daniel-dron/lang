mod scanner_tests {
    use lang_core::scanner::Scanner;

    #[test]
    fn test_scanner_initialization() {
        let source = "let x = 5;";
        let scanner = Scanner::new(source);
        
        assert_eq!(scanner.current, 0);
        assert!(!scanner.is_at_end());
        
        let (line, column) = scanner.get_line_diagnostics();
        assert_eq!(line, 1);
        assert_eq!(column, 0);
    }

    #[test]
    fn test_scanner_advance() {
        let source = "abc";
        let mut scanner = Scanner::new(source);
        
        assert_eq!(scanner.advance(), Some('a'));
        assert_eq!(scanner.advance(), Some('b'));
        assert_eq!(scanner.advance(), Some('c'));
        assert_eq!(scanner.advance(), None);
        assert!(scanner.is_at_end());
    }

    #[test]
    fn test_scanner_line_tracking() {
        let source = "line1\nline2\nline3";
        let mut scanner = Scanner::new(source);
        
        // Advance to end of line 1
        for _ in 0..5 { scanner.advance(); }
        let (line, _) = scanner.get_line_diagnostics();
        assert_eq!(line, 1);
        
        // Advance to newline and verify line change
        scanner.advance(); // '\n'
        let (line, column) = scanner.get_line_diagnostics();
        assert_eq!(line, 2);
        assert_eq!(column, 0);
    }

    #[test]
    fn test_scanner_peek_and_peek_next() {
        let source = "abc";
        let mut scanner = Scanner::new(source);
        
        assert_eq!(scanner.peek(), Some('a'));
        assert_eq!(scanner.peek_next(), Some('b'));
        
        scanner.advance();
        assert_eq!(scanner.peek(), Some('b'));
        assert_eq!(scanner.peek_next(), Some('c'));
        
        scanner.advance();
        assert_eq!(scanner.peek(), Some('c'));
        assert_eq!(scanner.peek_next(), None);
        
        scanner.advance();
        assert_eq!(scanner.peek(), None);
    }

    #[test]
    fn test_scanner_advance_if() {
        let source = "abc";
        let mut scanner = Scanner::new(source);
        
        assert!(scanner.advance_if('a'));
        assert_eq!(scanner.current, 1);
        
        assert!(!scanner.advance_if('a')); // Won't advance since current is 'b'
        assert_eq!(scanner.current, 1);
        
        assert!(scanner.advance_if('b'));
        assert_eq!(scanner.current, 2);
    }

    #[test]
    fn test_scanner_get_lexeme() {
        let source = "let x = 5;";
        let scanner = Scanner::new(source);
        
        assert_eq!(scanner.get_lexeme(0, 3), "let");
        assert_eq!(scanner.get_lexeme(4, 5), "x");
        assert_eq!(scanner.get_lexeme(6, 7), "=");
        assert_eq!(scanner.get_lexeme(8, 9), "5");
    }
}
