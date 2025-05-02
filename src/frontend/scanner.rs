// scanner to scan characters from source code
pub struct Scanner<'a> {
    source: &'a str,
    chars: Vec<char>,

    pub current: usize, // current index into chars

    // for diagnostics
    line: usize,   // line in source
    column: usize, // character position on current line
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        let chars = source.chars().collect();
        Self {
            source,
            chars,
            current: 0,
            line: 1,
            column: 0,
        }
    }

    pub fn get_line_diagnostics(&self) -> (usize, usize) {
        (self.line, self.column)
    }

    // is current position at or after the end of the source
    pub fn is_at_end(&self) -> bool {
        self.current == self.chars.len()
    }

    // consume current character, return it and advance
    pub fn advance(&mut self) -> Option<char> {
        if self.is_at_end() {
            return None;
        }

        let c = self.chars[self.current];

        self.current = self.current + 1;
        self.column = self.column + 1;

        // if it's a new line, increment line counter and reset column
        if c == '\n' {
            self.line = self.line + 1;
            self.column = 0;
        }

        Some(c)
    }

    // peek current character (if there is any) without advancing
    pub fn peek(&self) -> Option<char> {
        if self.is_at_end() {
            return None;
        }

        Some(self.chars[self.current])
    }

    // peek next character (if any) without advancing
    pub fn peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.chars.len() {
            return None;
        }

        Some(self.chars[self.current + 1])
    }

    // advance if and only if current character is equal to expected. return if advanced
    pub fn advance_if(&mut self, expected: char) -> bool {
        let c = self.peek();

        match c {
            Some(c) => {
                if c == expected {
                    self.advance();
                    return true;
                }

                return false;
            }
            None => {
                return false;
            }
        }
    }

    pub fn get_lexeme(&self, start: usize, end: usize) -> String {
        self.source[start..end].to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_scanner() {
        let source = "test";
        let scanner = Scanner::new(source);
        
        assert_eq!(scanner.current, 0);
        assert_eq!(scanner.line, 1);
        assert_eq!(scanner.column, 0);
        assert_eq!(scanner.chars, vec!['t', 'e', 's', 't']);
    }

    #[test]
    fn test_is_at_end() {
        let source = "a";
        let mut scanner = Scanner::new(source);
        
        assert!(!scanner.is_at_end());
        scanner.advance();
        assert!(scanner.is_at_end());
    }

    #[test]
    fn test_empty_source() {
        let source = "";
        let scanner = Scanner::new(source);
        
        assert!(scanner.is_at_end());
        assert_eq!(scanner.peek(), None);
        assert_eq!(scanner.peek_next(), None);
    }

    #[test]
    fn test_advance() {
        let source = "ab";
        let mut scanner = Scanner::new(source);
        
        assert_eq!(scanner.advance(), Some('a'));
        assert_eq!(scanner.current, 1);
        assert_eq!(scanner.advance(), Some('b'));
        assert_eq!(scanner.current, 2);
        assert_eq!(scanner.advance(), None);
    }
    
    #[test]
    fn test_peek() {
        let source = "abc";
        let mut scanner = Scanner::new(source);
        
        assert_eq!(scanner.peek(), Some('a'));
        scanner.advance();
        assert_eq!(scanner.peek(), Some('b'));
        scanner.advance();
        assert_eq!(scanner.peek(), Some('c'));
        scanner.advance();
        assert_eq!(scanner.peek(), None);
    }
    
    #[test]
    fn test_peek_next() {
        let source = "abc";
        let mut scanner = Scanner::new(source);
        
        assert_eq!(scanner.peek_next(), Some('b'));
        scanner.advance();
        assert_eq!(scanner.peek_next(), Some('c'));
        scanner.advance();
        assert_eq!(scanner.peek_next(), None);
    }
    
    #[test]
    fn test_advance_if() {
        let source = "abc";
        let mut scanner = Scanner::new(source);
        
        assert!(scanner.advance_if('a'));
        assert_eq!(scanner.current, 1);
        
        assert!(!scanner.advance_if('a'));
        assert_eq!(scanner.current, 1);
        
        assert!(scanner.advance_if('b'));
        assert_eq!(scanner.current, 2);
    }
    
    #[test]
    fn test_line_column_tracking() {
        let source = "abc\ndef\nghi";
        let mut scanner = Scanner::new(source);
        
        assert_eq!(scanner.get_line_diagnostics(), (1, 0));
        
        scanner.advance(); // 'a'
        assert_eq!(scanner.get_line_diagnostics(), (1, 1));
        
        scanner.advance(); // 'b'
        scanner.advance(); // 'c'
        scanner.advance(); // '\n'
        assert_eq!(scanner.get_line_diagnostics(), (2, 0));
        
        scanner.advance(); // 'd'
        scanner.advance(); // 'e'
        scanner.advance(); // 'f'
        scanner.advance(); // '\n'
        assert_eq!(scanner.get_line_diagnostics(), (3, 0));
    }
    
    #[test]
    fn test_get_lexeme() {
        let source = "function test() { return 42; }";
        let scanner = Scanner::new(source);
        
        assert_eq!(scanner.get_lexeme(0, 8), "function");
        assert_eq!(scanner.get_lexeme(9, 13), "test");
        assert_eq!(scanner.get_lexeme(18, 28), "return 42;");
    }
    
    #[test]
    fn test_unicode_support() {
        let source = "こんにちは世界";
        let mut scanner = Scanner::new(source);
        
        assert_eq!(scanner.advance(), Some('こ'));
        assert_eq!(scanner.advance(), Some('ん'));
        assert_eq!(scanner.peek(), Some('に'));
        assert_eq!(scanner.peek_next(), Some('ち'));
    }
}