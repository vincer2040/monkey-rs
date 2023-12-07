// use environment::Environment;
use object::ObjectTrait;

pub mod ast;
pub mod builtins;
pub mod environment;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod token;
pub mod util;

const PROMP: &'static str = ">> ";

fn main() -> anyhow::Result<()> {
    Ok(())
}

fn print_errors(p: &parser::Parser) {
    let errors = p.get_errors();
    for err in errors.iter() {
        println!("{}", err);
    }
}
