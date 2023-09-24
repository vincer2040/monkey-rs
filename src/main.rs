use environment::Environment;
use object::ObjectTrait;

pub mod ast;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod token;
pub mod util;
pub mod environment;
pub mod builtins;
pub mod code;
pub mod compiler;

const PROMP: &'static str = ">> ";

fn main() -> anyhow::Result<()> {
    let mut env = Environment::new();
    loop {
        let line = util::read_line(PROMP)?;
        let l: lexer::Lexer;
        let mut p: parser::Parser;
        let program: ast::Program;
        let obj: Option<object::Object>;
        if line == "exit\n" {
            break;
        }
        if line == "\n" {
            continue;
        }
        l = lexer::Lexer::new(&line);
        p = parser::Parser::new(l);
        program = p.parse();
        if p.errors_len() != 0 {
            print_errors(&p);
            continue;
        }
        obj = evaluator::eval(&program, &mut env);
        match obj {
            Some(o) => {
                let val = o.inspect();
                println!("{}", val);
            }
            None => {}
        };
    }
    Ok(())
}

fn print_errors(p: &parser::Parser) {
    let errors = p.get_errors();
    for err in errors.iter() {
        println!("{}", err);
    }
}
