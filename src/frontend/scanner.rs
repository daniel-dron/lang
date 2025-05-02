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
