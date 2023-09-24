// use environment::Environment;
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
pub mod vm;

const PROMP: &'static str = ">> ";

fn main() -> anyhow::Result<()> {
    // let mut env = Environment::new();
    loop {
        let line = util::read_line(PROMP)?;
        let l: lexer::Lexer;
        let mut p: parser::Parser;
        let program: ast::Program;
        let mut compiler: compiler::Compiler;
        let mut vm: vm::VM;
        let byte_code: compiler::ByteCode;
        let obj: Option<&object::Object>;
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
        compiler = compiler::Compiler::new();
        match compiler.compile(&program) {
            Ok(()) => {},
            Err(e) => {
                println!("compilation failed:\n{}", e);
                continue;
            }
        };
        byte_code = compiler.byte_code();
        vm = vm::VM::new(&byte_code);
        vm.run()?;
        obj = vm.stack_top();
        match obj {
            Some(o) => println!("{}", o.inspect()),
            None => {},
        }
    }
    Ok(())
}

fn print_errors(p: &parser::Parser) {
    let errors = p.get_errors();
    for err in errors.iter() {
        println!("{}", err);
    }
}
