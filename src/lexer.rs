use crate::token::Token;
use crate::util::{is_digit, is_letter, lookup_ident};

pub struct Lexer {
    input: std::sync::Arc<str>,
    position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut l = Lexer {
            input: input.into(),
            position: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    pub fn next_token(&mut self) -> Token {
        let tok: Token;
        self.skip_whitespace();
        match self.ch {
            '"' => {
                let str = self.read_string();
                tok = Token::String(str.into());
            }
            '=' => {
                if self.peek_char() == '=' {
                    tok = Token::Eq;
                    self.read_char();
                } else {
                    tok = Token::Assign;
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    tok = Token::NotEq;
                    self.read_char();
                } else {
                    tok = Token::Bang;
                }
            }
            '+' => tok = Token::Plus,
            '-' => tok = Token::Minus,
            '/' => tok = Token::Slash,
            '*' => tok = Token::Asterisk,
            '<' => tok = Token::Lt,
            '>' => tok = Token::Gt,
            '(' => tok = Token::LParen,
            ')' => tok = Token::RParen,
            '{' => tok = Token::LSquirly,
            '}' => tok = Token::RSquirly,
            '[' => tok = Token::LBracket,
            ']' => tok = Token::RBracket,
            ',' => tok = Token::Comma,
            ':' => tok = Token::Colon,
            ';' => tok = Token::Semicolon,
            '\0' => tok = Token::Eof,
            _ => {
                if is_letter(self.ch) {
                    let str = self.read_ident();
                    return lookup_ident(&str);
                } else if is_digit(self.ch) {
                    let str = self.read_number();
                    tok = Token::Int(str.into());
                    return tok;
                } else {
                    tok = Token::Illegal;
                }
            }
        };
        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        if self.position >= self.input.len() {
            self.ch = '\0';
        } else {
            match self.input.chars().nth(self.position) {
                Some(ch) => self.ch = ch,
                None => self.ch = '\0',
            }
        }
        self.position += 1;
    }

    fn read_ident(&mut self) -> String {
        let mut res = String::new();
        while is_letter(self.ch) {
            res.push(self.ch);
            self.read_char();
        }
        res
    }

    fn read_number(&mut self) -> String {
        let mut res = String::new();
        while is_digit(self.ch) {
            res.push(self.ch);
            self.read_char();
        }
        res
    }

    fn read_string(&mut self) -> String {
        let mut res = String::new();
        self.read_char();
        loop {
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
            res.push(self.ch);
            self.read_char();
        }
        res
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn peek_char(&self) -> char {
        if self.position >= self.input.len() {
            '\0'
        } else {
            match self.input.chars().nth(self.position) {
                Some(ch) => ch,
                None => '\0',
            }
        }
    }
}

#[cfg(test)]
mod test {

    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
if (5 < 10) {
    return true;
} else {
    return false;
}
10 == 10;
10 != 9;
\"foobar\"
\"foo bar\"
[1, 2];
{\"foo\": \"bar\"}
";
        let mut l = Lexer::new(&input);
        let exps = vec![
            Token::Let,
            Token::Ident("five".into()),
            Token::Assign,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".into()),
            Token::Assign,
            Token::Int("10".into()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".into()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".into()),
            Token::Comma,
            Token::Ident("y".into()),
            Token::RParen,
            Token::LSquirly,
            Token::Ident("x".into()),
            Token::Plus,
            Token::Ident("y".into()),
            Token::Semicolon,
            Token::RSquirly,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".into()),
            Token::Assign,
            Token::Ident("add".into()),
            Token::LParen,
            Token::Ident("five".into()),
            Token::Comma,
            Token::Ident("ten".into()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::Int("5".into()),
            Token::Lt,
            Token::Int("10".into()),
            Token::Gt,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int("5".into()),
            Token::Lt,
            Token::Int("10".into()),
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RSquirly,
            Token::Else,
            Token::LSquirly,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RSquirly,
            Token::Int("10".into()),
            Token::Eq,
            Token::Int("10".into()),
            Token::Semicolon,
            Token::Int("10".into()),
            Token::NotEq,
            Token::Int("9".into()),
            Token::Semicolon,
            Token::String("foobar".into()),
            Token::String("foo bar".into()),
            Token::LBracket,
            Token::Int("1".into()),
            Token::Comma,
            Token::Int("2".into()),
            Token::RBracket,
            Token::Semicolon,
            Token::LSquirly,
            Token::String("foo".into()),
            Token::Colon,
            Token::String("bar".into()),
            Token::RSquirly,
            Token::Eof,
        ];
        for exp in exps.iter() {
            let tok = l.next_token();
            assert_eq!(tok, *exp);
        }
    }
}
