use std::str::Chars;

use super::token::{Token, TokenSpan};
use super::{ident::Identifier, keyword::Keyword, literal::Literal, symbol::Symbol};
use crate::diagnostic::{Pos, Span, Spanned};

#[derive(Debug)]
pub struct TokenError {
    pub message: String,
    pub source: Option<Box<dyn std::error::Error + Send + Sync>>,
    pub span: Option<Span>,
}

impl TokenError {
    pub fn new(message: impl Into<String>) -> TokenError {
        TokenError {
            message: message.into(),
            source: None,
            span: None,
        }
    }

    pub fn with_source<E: std::error::Error + Send + Sync + 'static>(mut self, source: E) -> Self {
        self.source = Some(Box::new(source));
        self
    }

    pub fn with_span(mut self, span: Span) -> TokenError {
        self.span = Some(span);
        self
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn span(&self) -> Option<Span> {
        self.span
    }
}

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)?;
        if let Some(source) = &self.source {
            write!(f, ", {}", source)?;
        }
        if let Some(span) = &self.span {
            write!(f, " at {:?}", span)?;
        }
        Ok(())
    }
}

impl std::error::Error for TokenError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source
            .as_ref()
            .map(|e| e.as_ref() as &dyn std::error::Error)
    }
}

#[derive(Clone)]
pub struct Tokenizer<'i> {
    chars: Chars<'i>,
    input: &'i str,
    pos: Pos,
}

impl<'i> Tokenizer<'i> {
    pub fn new(input: &'i str) -> Self {
        let chars = input.chars();

        Tokenizer {
            chars,
            input,
            pos: Pos::new(),
        }
    }

    fn pos(&self) -> Pos {
        self.pos
    }

    fn new_token(&self, token: Token, span: Span) -> TokenSpan {
        Spanned::new(token, span)
    }

    pub fn next_token(&mut self) -> Result<TokenSpan, TokenError> {
        let start = self.pos();

        self.next_token_inner()
            .map(|token| self.new_token(token, Span::new(start, self.pos())))
            .map_err(|e| e.with_span(Span::new(start, self.pos())))
    }

    fn next_token_inner(&mut self) -> Result<Token, TokenError> {
        match self.peek() {
            Some(c) => {
                match c {
                    c if c.is_ascii_whitespace() => self.eat_whitespace(),
                    '_' | 'a'..='z' | 'A'..='Z' => self.eat_ident(),
                    '0'..='9' => self.eat_number(),
                    '\'' => self.eat_char(),
                    '"' => self.eat_string(),
                    c => {
                        // comment
                        if self.starts_with("//") {
                            return self.eat_comment();
                        }
                        // symbol
                        self.eat_symbol(c)
                    }
                }
            }
            None => Ok(Token::Eof),
        }
    }

    fn len(&self) -> usize {
        self.chars.clone().count()
    }

    fn has_at_lease(&self, n: usize) -> bool {
        self.chars.clone().nth(n - 1).is_some()
    }

    fn starts_with(&self, pat: &str) -> bool {
        self.chars.clone().as_str().starts_with(pat)
    }

    pub fn is_eof(&mut self) -> bool {
        self.peek().is_none()
    }

    fn next_char(&mut self) -> Option<char> {
        self.chars.next().inspect(|&c| {
            self.pos.offset += c.len_utf8();
            if c == '\n' {
                self.pos.line += 1;
                self.pos.column = 1;
            } else {
                self.pos.column += 1;
            }
        })
    }

    fn advance(&mut self, n: usize) {
        for _ in 0..n {
            if self.next_char().is_none() {
                break;
            }
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.clone().next()
    }

    /// 查看第n个字符而不移动指针 (0-based)
    fn peek_n(&self, n: usize) -> Option<char> {
        self.chars.clone().nth(n)
    }

    fn eat_while<P>(&mut self, mut predicate: P) -> &'i str
    where
        P: FnMut(char) -> bool,
    {
        let start = self.chars.as_str();
        let mut len = 0;

        while let Some(ch) = self.peek() {
            if !predicate(ch) {
                return &start[..len];
            }
            len += ch.len_utf8();
            self.advance(1);
        }

        &start[..len]
    }

    pub fn eat_whitespace(&mut self) -> Result<Token, TokenError> {
        let ws = self.eat_while(|c| c.is_ascii_whitespace());

        Ok(Token::Whitespace(ws.to_string()))
    }

    pub fn eat_ident(&mut self) -> Result<Token, TokenError> {
        if self.starts_with("b\'") {
            return self.eat_byte();
        }
        if self.starts_with("b\"") {
            return self.eat_byte_slice();
        }

        let got = self.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');

        let token = match got {
            "true" => Token::Literal(Literal::Bool(true)),
            "false" => Token::Literal(Literal::Bool(false)),
            kw if Keyword::STRS.contains(&kw) => {
                let kw = Keyword::from_str(kw).unwrap();
                Token::Keyword(kw)
            }
            _ => Token::Ident(Identifier::new(got)),
        };

        Ok(token)
    }

    fn eat_number(&mut self) -> Result<Token, TokenError> {
        // binary number, eg: 0b10101
        if self.starts_with("0b") || self.starts_with("0B") {
            return self.eat_bin_number();
        }
        // octal number, eg: 0o123
        if self.starts_with("0o") || self.starts_with("0O") {
            return self.eat_oct_number();
        }
        // hex number, eg: 0x123
        if self.starts_with("0x") || self.starts_with("0X") {
            return self.eat_hex_number();
        }

        let start = self.pos();

        let i = self.eat_while(|c| c.is_ascii_digit() || c == '_');
        if self.peek() == Some('.') && !self.starts_with("..") {
            self.advance(1);
            let f = self.eat_while(|c| c.is_ascii_digit() || c == '_');

            let mut number = i.to_string();
            number.push('.');
            number.push_str(f);

            let number = number.replace("_", "");

            return number.parse::<f64>().map(Token::float).map_err(|e| {
                TokenError::new("parse float failed")
                    .with_source(e)
                    .with_span(Span::new(start, self.pos()))
            });
        }

        let number = i.to_string().replace("_", "");

        number.parse::<i64>().map(Token::int).map_err(|e| {
            TokenError::new("parse float failed")
                .with_source(e)
                .with_span(Span::new(start, self.pos()))
        })
    }

    fn eat_bin_number(&mut self) -> Result<Token, TokenError> {
        self.advance(2);
        let s = self.eat_while(|c| matches!(c, '0' | '1'));
        let i = i64::from_str_radix(s, 2)
            .map_err(|e| TokenError::new("parse int failed").with_source(e))?;
        Ok(Token::Literal(Literal::Integer(i)))
    }

    fn eat_oct_number(&mut self) -> Result<Token, TokenError> {
        self.advance(2);
        let s = self.eat_while(|c| matches!(c, '0'..='7'));
        let i = i64::from_str_radix(s, 8)
            .map_err(|e| TokenError::new("parse int failed").with_source(e))?;
        Ok(Token::Literal(Literal::Integer(i)))
    }

    fn eat_hex_number(&mut self) -> Result<Token, TokenError> {
        self.advance(2);
        let s = self.eat_while(|c| c.is_ascii_hexdigit());
        let i = i64::from_str_radix(s, 16)
            .map_err(|e| TokenError::new("parse int failed").with_source(e))?;
        Ok(Token::Literal(Literal::Integer(i)))
    }

    fn eat_byte(&mut self) -> Result<Token, TokenError> {
        self.advance(1);
        let token = self.eat_char()?;
        match token {
            Token::Literal(Literal::Char(ch)) if ch as u32 <= 0xff => {
                Ok(Token::Literal(Literal::Byte(ch as u8)))
            }
            _ => Err(TokenError::new("invalid byte char literal")),
        }
    }

    fn eat_byte_slice(&mut self) -> Result<Token, TokenError> {
        self.advance(1);
        let token = self.eat_string()?;
        match token {
            Token::Literal(Literal::String(s)) => {
                Ok(Token::Literal(Literal::ByteSlice(s.into_bytes())))
            }
            _ => Err(TokenError::new("invalid byte slice string literal")),
        }
    }

    fn eat_string(&mut self) -> Result<Token, TokenError> {
        self.eat_qoutes('"')
            .map(|s| Token::Literal(Literal::String(s)))
    }

    fn eat_char(&mut self) -> Result<Token, TokenError> {
        let s = self.eat_qoutes('\'')?;

        if s.chars().count() == 1 {
            Ok(Token::Literal(Literal::Char(s.chars().next().unwrap())))
        } else {
            Err(TokenError::new("too many char for CharLit"))
        }
    }

    fn eat_comment(&mut self) -> Result<Token, TokenError> {
        self.advance(2);

        let s = self.eat_while(|c| c != '\n');

        self.advance(1); // eat `\n`

        Ok(Token::Comment(s.to_string()))
    }

    fn eat_qoutes(&mut self, qoute: char) -> Result<String, TokenError> {
        let mut ret = String::new();

        self.advance(1); // skip start qoute

        let mut is_backslash_previous = false;

        while let Some(c) = self.next_char() {
            match c {
                '\\' => {
                    if is_backslash_previous {
                        ret.push(c);
                        is_backslash_previous = false;
                    } else {
                        is_backslash_previous = true;
                    }
                }
                _ => {
                    if c == qoute {
                        return Ok(ret);
                    }

                    if is_backslash_previous {
                        let ch = match c {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            _ => {
                                if c == qoute {
                                    c
                                } else {
                                    return Err(TokenError::new("unknown char after escape"));
                                }
                            }
                        };

                        is_backslash_previous = true;
                        ret.push(ch);
                    } else {
                        ret.push(c);
                    }
                }
            }
        }

        Err(TokenError::new("incompleted qouted"))
    }

    fn eat_tree(&mut self) -> Result<TokenSpan, TokenError> {
        let mut group = Vec::new();

        let _open = self.next_char().unwrap();

        let group_start = self.pos();

        loop {
            let start = self.pos();

            let token = self.next_token()?;

            match token.value {
                Token::Eof => {
                    return Err(TokenError::new("unclose group"));
                }
                Token::Symbol(Symbol::RParen)
                | Token::Symbol(Symbol::RBracket)
                | Token::Symbol(Symbol::RBrace) => {
                    return Ok(
                        self.new_token(Token::Tree(group), Span::new(group_start, self.pos()))
                    );
                }

                t => {
                    group.push(self.new_token(t, Span::new(start, self.pos())));
                }
            }
        }
    }

    fn eat_symbol(&mut self, peek: char) -> Result<Token, TokenError> {
        // 优化：优先处理3字符标点
        if let Some(p) = match (peek, self.peek_n(1), self.peek_n(2)) {
            ('.', Some('.'), Some('=')) => Some((Symbol::DotDotEq, 3)),
            _ => None
        } {
            self.advance(p.1);
            return Ok(Token::Symbol(p.0));
        }

        // 优化：直接处理2字符标点
        if let Some(p) = match (peek, self.peek_n(1)) {
            ('&', Some('&')) => Some((Symbol::AndAnd, 2)),
            ('|', Some('|')) => Some((Symbol::OrOr, 2)),
            ('+', Some('=')) => Some((Symbol::PlusEq, 2)),
            ('-', Some('=')) => Some((Symbol::MinusEq, 2)),
            ('*', Some('=')) => Some((Symbol::StarEq, 2)),
            ('/', Some('=')) => Some((Symbol::SlashEq, 2)),
            ('%', Some('=')) => Some((Symbol::PercentEq, 2)),
            ('^', Some('=')) => Some((Symbol::CaretEq, 2)),
            ('&', Some('=')) => Some((Symbol::AndEq, 2)),
            ('|', Some('=')) => Some((Symbol::OrEq, 2)),
            ('=', Some('=')) => Some((Symbol::EqEq, 2)),
            ('!', Some('=')) => Some((Symbol::Ne, 2)),
            ('>', Some('=')) => Some((Symbol::Ge, 2)),
            ('<', Some('=')) => Some((Symbol::Le, 2)),
            ('<', Some('<')) => Some((Symbol::Shl, 2)),
            ('>', Some('>')) => Some((Symbol::Shr, 2)),
            (':', Some(':')) => Some((Symbol::ColonColon, 2)),
            ('.', Some('.')) => Some((Symbol::DotDot, 2)),
            ('-', Some('>')) => Some((Symbol::RArrow, 2)),
            ('=', Some('>')) => Some((Symbol::FatArrow, 2)),
            _ => None
        } {
            self.advance(p.1);
            return Ok(Token::Symbol(p.0));
        }

        let token = match peek {
            '+' => Some(Symbol::Plus),
            '-' => Some(Symbol::Minus),
            '*' => Some(Symbol::Star),
            '/' => Some(Symbol::Slash),
            '%' => Some(Symbol::Percent),
            '|' => Some(Symbol::Or),
            '&' => Some(Symbol::And),
            '^' => Some(Symbol::Caret),
            '>' => Some(Symbol::Gt),
            '<' => Some(Symbol::Lt),
            '(' => Some(Symbol::LParen),
            ')' => Some(Symbol::RParen),
            '[' => Some(Symbol::LBracket),
            ']' => Some(Symbol::RBracket),
            '{' => Some(Symbol::LBrace),
            '}' => Some(Symbol::RBrace),
            ',' => Some(Symbol::Comma),
            ':' => Some(Symbol::Colon),
            ';' => Some(Symbol::Semi),
            '#' => Some(Symbol::Pound),
            '!' => Some(Symbol::Not),
            '?' => Some(Symbol::Question),
            '=' => Some(Symbol::Eq),
            '.' => Some(Symbol::Dot),
            _ => None,
        };

        if let Some(p) = token {
            self.advance(1);
            return Ok(Token::Symbol(p));
        }

        Err(TokenError::new(format!("unknown punctuation({peek})")))
    }
}

// impl<'i> fmt::Debug for Tokenizer<'i> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         f.debug_struct("Tokenizer")
//             .field("input", &self.input)
//             .finish()
//     }
// }

impl<'i> Iterator for Tokenizer<'i> {
    type Item = Result<TokenSpan, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(t) if t.value == Token::Eof => None,
            t => Some(t),
        }
    }
}

pub struct TokenStream {
    tokens: Vec<TokenSpan>,
}

impl TokenStream {
    pub fn new(tokens: Vec<TokenSpan>) -> Self {
        TokenStream { tokens }
    }

    pub fn parse(input: &str) -> Result<Self, TokenError> {
        let tokens = Tokenizer::new(input).collect::<Result<Vec<_>, _>>()?;

        Ok(TokenStream { tokens })
    }

    pub fn iter<'i>(&'i self) -> std::slice::Iter<'_, TokenSpan> {
        self.tokens.iter()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenize_identifier() {
        let input = "let x = 5;";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::Let),
                Token::Ident("x".into()),
                Token::Symbol(Symbol::Eq),
                Token::Literal(Literal::Integer(5)),
                Token::Symbol(Symbol::Semi)
            ]
        );
    }

    #[test]
    fn test_tokenize_number() {
        let input = "123 45.67";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Literal(Literal::Integer(123)),
                Token::Literal(Literal::Float(45.67)),
            ]
        );
    }

    #[test]
    fn test_tokenize_special_number_formats() {
        let input = "0b101 0o644 0xFF";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Literal(Literal::Integer(5)),   // 0b101 = 5
                Token::Literal(Literal::Integer(420)), // 0o644 = 420
                Token::Literal(Literal::Integer(255)), // 0xFF = 255
            ]
        );
    }

    #[test]
    fn test_tokenize_string_char() {
        let input = "\"hello world\" 'a'";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Literal(Literal::String("hello world".to_string())),
                Token::Literal(Literal::Char('a')),
            ]
        );
    }

    #[test]
    fn test_tokenize_byte_string_char() {
        let input = r#"b"hello world" b'a'"#;
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Literal(Literal::ByteSlice("hello world".as_bytes().to_vec())),
                Token::Literal(Literal::Byte(b'a')),
            ]
        );
    }

    #[test]
    fn test_tokenize_comment() {
        let input = "// This is a comment\nlet y = 10;";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Comment(" This is a comment".to_string()),
                Token::Keyword(Keyword::Let),
                Token::Ident(Identifier::new("y")),
                Token::Symbol(Symbol::Eq),
                Token::Literal(Literal::Integer(10)),
                Token::Symbol(Symbol::Semi),
            ]
        );
    }

    #[test]
    fn test_tokenize_operator() {
        let input = "+ - * / % == != > < =>";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Symbol(Symbol::Plus),
                Token::Symbol(Symbol::Minus),
                Token::Symbol(Symbol::Star),
                Token::Symbol(Symbol::Slash),
                Token::Symbol(Symbol::Percent),
                Token::Symbol(Symbol::EqEq),
                Token::Symbol(Symbol::Ne),
                Token::Symbol(Symbol::Gt),
                Token::Symbol(Symbol::Lt),
                Token::Symbol(Symbol::FatArrow),
            ]
        );
    }

    #[test]
    fn test_tokenize_keyword() {
        let input = "if else for while loop match";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::If),
                Token::Keyword(Keyword::Else),
                Token::Keyword(Keyword::For),
                Token::Keyword(Keyword::While),
                Token::Keyword(Keyword::Loop),
                Token::Keyword(Keyword::Match),
            ]
        );
    }

    #[test]
    fn test_tokenize_bool() {
        let input = "true false";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Literal(Literal::Bool(true)),
                Token::Literal(Literal::Bool(false)),
            ]
        );
    }

    #[test]
    fn test_tokenize_parentheses() {
        let input = "()[]{}";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Symbol(Symbol::LParen),
                Token::Symbol(Symbol::RParen),
                Token::Symbol(Symbol::LBracket),
                Token::Symbol(Symbol::RBracket),
                Token::Symbol(Symbol::LBrace),
                Token::Symbol(Symbol::RBrace),
            ]
        );
    }

    #[test]
    fn test_tokenize_more_symbols() {
        let input = "!= += -= *= /= %= ^= &= |= << >> -> ::";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Symbol(Symbol::Ne),
                Token::Symbol(Symbol::PlusEq),
                Token::Symbol(Symbol::MinusEq),
                Token::Symbol(Symbol::StarEq),
                Token::Symbol(Symbol::SlashEq),
                Token::Symbol(Symbol::PercentEq),
                Token::Symbol(Symbol::CaretEq),
                Token::Symbol(Symbol::AndEq),
                Token::Symbol(Symbol::OrEq),
                Token::Symbol(Symbol::Shl),
                Token::Symbol(Symbol::Shr),
                Token::Symbol(Symbol::RArrow),
                Token::Symbol(Symbol::ColonColon),
            ]
        );
    }

    #[test]
    fn test_tokenize_mixed_statement() {
        let input = "if (x > 5) { return x; }";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::If),
                Token::Symbol(Symbol::LParen),
                Token::Ident(Identifier::new("x")),
                Token::Symbol(Symbol::Ge),
                Token::Literal(Literal::Integer(5)),
                Token::Symbol(Symbol::RParen),
                Token::Symbol(Symbol::LBrace),
                Token::Keyword(Keyword::Return),
                Token::Ident(Identifier::new("x")),
                Token::Symbol(Symbol::Semi),
                Token::Symbol(Symbol::RBrace),
            ]
        );
    }

    #[test]
    fn test_tokenizer_cast() {
        let input = "x * y as float";

        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.value))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::ident("x"),
                Token::Symbol(Symbol::Star),
                Token::ident("y"),
                Token::keyword("as"),
                Token::ident("float"),
            ]
        );
    }
}
