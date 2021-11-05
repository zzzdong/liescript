use std::{iter::Peekable, str::Chars};




struct Tokenizer<'s> {
    source: &'s str,
    input: Peekable<Chars<'s>>,
    pos: usize,
    cur_line: usize,
    cur_column: usize,
}






impl<'s> Tokenizer<'s> {
    pub fn new(source: &'s str) -> Self {
        let input = source.chars().peekable();

        Tokenizer {
            source,
            input,
            pos: 0,
            cur_line: 0,
            cur_column: 0,
        }
    }

    fn is_eof(&mut self) -> bool {
        self.input.peek().is_none()
    }
}