use ast::Node;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod util;

const PROMP: &'static str = ">> ";

fn main() -> anyhow::Result<()> {
    loop {
        let line = util::read_line(PROMP)?;
        let l: lexer::Lexer;
        let mut p: parser::Parser;
        let program: ast::Program;
        let s: String;
        if line == "exit\n" {
            break;
        }
        l = lexer::Lexer::new(&line);
        p = parser::Parser::new(l);
        program = p.parse();
        if p.errors_len() != 0 {
            print_errors(&p);
            continue;
        }
        s = program.string();
        println!("{}", s);
    }
    Ok(())
}

fn print_errors(p: &parser::Parser) {
    let errors = p.get_errors();
    for err in errors.iter() {
        println!("{}", err);
    }
}
