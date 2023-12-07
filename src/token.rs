#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Token {
    Illegal,
    Eof,
    Ident(std::rc::Rc<str>),
    Int(std::rc::Rc<str>),
    String(std::rc::Rc<str>),
    Assign,
    Plus,
    Minus,
    Slash,
    Asterisk,
    Bang,
    Lt,
    Gt,
    Eq,
    NotEq,
    Comma,
    Colon,
    Semicolon,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    LBracket,
    RBracket,
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

impl Default for Token {
    fn default() -> Self {
        Token::Illegal
    }
}
