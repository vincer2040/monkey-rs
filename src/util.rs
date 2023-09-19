use std::io::Write;

use crate::token::Token;

pub fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

pub fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

pub fn lookup_ident(ident: &str) -> Token {
    match ident {
        "fn" => Token::Function,
        "let" => Token::Let,
        "if" => Token::If,
        "return" => Token::Return,
        "true" => Token::True,
        "false" => Token::False,
        "else" => Token::Else,
        _ => Token::Ident(ident.into()),
    }
}

pub fn read_line(prompt: &str) -> anyhow::Result<String> {
    let mut input = String::new();
    print!("{}", prompt);
    std::io::stdout().flush()?;
    std::io::stdin().read_line(&mut input)?;
    Ok(input)
}
