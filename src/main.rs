pub mod token;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod util;

const PROMP: &'static str = ">> ";

fn main() -> anyhow::Result<()> {
    loop {
        let line = util::read_line(PROMP)?;
        let mut l: lexer::Lexer;
        let mut tok: token::Token;
        if line == "exit\n" {
            break;
        }
        l = lexer::Lexer::new(&line);
        tok = l.next_token();
        while tok != token::Token::Eof {
            println!("{:#?}", tok);
            tok = l.next_token();
        }
    }
    Ok(())
}
