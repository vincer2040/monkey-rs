// use environment::Environment;
use object::ObjectTrait;

pub mod ast;
pub mod builtins;
pub mod code;
pub mod compiler;
pub mod environment;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod token;
pub mod util;
pub mod vm;

const PROMP: &'static str = ">> ";

fn main() -> anyhow::Result<()> {
    let mut constants: Vec<object::Object> = Vec::new();
    let mut symbol_table: compiler::SymbolTable = compiler::SymbolTable::new();
    let mut globals: Vec<Option<object::Object>> = [vm::INIT; vm::GLOBAL_SIZE].to_vec();
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
        compiler = compiler::Compiler::new_with_state(symbol_table.clone(), constants.clone());
        match compiler.compile(&program) {
            Ok(()) => {}
            Err(e) => {
                println!("compilation failed:\n{}", e);
                continue;
            }
        };
        byte_code = compiler.byte_code();
        symbol_table = compiler.symbol_table.clone();
        constants = compiler.constants.clone();
        vm = vm::VM::new_with_global_store(&byte_code, &globals);
        vm.run()?;
        obj = vm.last_popped_stack_elem();
        match obj {
            Some(o) => println!("{}", o.inspect()),
            None => {}
        }
        globals = vm.globals.clone();
    }
    Ok(())
}

fn print_errors(p: &parser::Parser) {
    let errors = p.get_errors();
    for err in errors.iter() {
        println!("{}", err);
    }
}
