use crate::{
    ast::{Expression, InfixExpression, IntegerLiteral, Program, Statement},
    code::{make, Instructions, Opcode},
    object::Object,
};

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        let constants = Vec::new();
        let instructions = Vec::new();
        Self {
            instructions,
            constants,
        }
    }

    pub fn compile(&mut self, program: &Program) -> anyhow::Result<()> {
        self.compile_statements(&program.statements)
    }

    fn compile_statements(&mut self, stmts: &Vec<Statement>) -> anyhow::Result<()> {
        for s in stmts.iter() {
            self.compile_statement(s)?;
        }
        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement) -> anyhow::Result<()> {
        match stmt {
            Statement::ExpressionStatement(es) => self.compile_expression(&es.expression)?,
            _ => todo!(),
        };
        Ok(())
    }

    fn compile_expression(&mut self, exp: &Expression) -> anyhow::Result<()> {
        match exp {
            Expression::InfixExpression(ie) => self.compile_infix_expression(&ie)?,
            Expression::Integer(i) => self.compile_integer_literal(&i)?,
            _ => todo!(),
        }
        Ok(())
    }

    fn compile_infix_expression(&mut self, ie: &InfixExpression) -> anyhow::Result<()> {
        self.compile_expression(&ie.left)?;
        self.compile_expression(&ie.right)?;
        Ok(())
    }

    fn compile_integer_literal(&mut self, i: &IntegerLiteral) -> anyhow::Result<()> {
        let integer = Object::Integer(i.value);
        let operands = [self.add_constant(integer)];
        self.emit(Opcode::OpConstant, &operands);
        Ok(())
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn emit(&mut self, op: Opcode, operands: &[usize]) -> usize {
        let ins = make(op, operands);
        self.add_instruction(&ins)
    }

    fn add_instruction(&mut self, ins: &Vec<u8>) -> usize {
        let pos_new_instruction = self.instructions.len();
        for i in ins.iter() {
            self.instructions.push(*i);
        }
        pos_new_instruction
    }

    pub fn byte_code(&self) -> ByteCode {
        ByteCode {
            instructions: &self.instructions,
            constants: &self.constants,
        }
    }
}

pub struct ByteCode<'a> {
    pub instructions: &'a Instructions,
    pub constants: &'a Vec<Object>,
}

#[cfg(test)]
mod test {
    use crate::{
        ast::Program,
        code::{make, Instructions, Opcode},
        lexer::Lexer,
        object::Object,
        parser::Parser,
    };

    use super::Compiler;

    struct CompilerIntTestCase<'a> {
        input: &'static str,
        expected_constants: &'static [i64],
        expected_instructions: &'a [Instructions],
    }

    fn parse(input: &'static str) -> Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        p.parse()
    }

    fn run_int_compiler_test(test: &CompilerIntTestCase) {
        let program = parse(test.input);
        let mut compiler = Compiler::new();
        match compiler.compile(&program) {
            Ok(()) => {}
            Err(e) => panic!("{}", e),
        };
        let byte_code = compiler.byte_code();
        test_instructions(test.expected_instructions, byte_code.instructions);
        test_int_constants(test.expected_constants, byte_code.constants);
    }

    fn test_instructions(exp_instructions: &[Instructions], actual: &Instructions) {
        let concatted = concat_instructions(exp_instructions);
        assert_eq!(concatted.len(), actual.len());
        for (i, ins) in concatted.iter().enumerate() {
            assert_eq!(actual[i], *ins);
        }
    }

    fn test_int_constants(exp_constants: &[i64], constants: &Vec<Object>) {
        assert_eq!(exp_constants.len(), constants.len());
        for (i, exp) in exp_constants.iter().enumerate() {
            test_integer_object(*exp, &constants[i]);
        }
    }

    fn concat_instructions(s: &[Instructions]) -> Instructions {
        let mut res = Vec::new();
        for x in s.iter() {
            for y in x.iter() {
                res.push(*y);
            }
        }
        res
    }

    fn test_integer_object(exp: i64, actual: &Object) {
        if let Object::Integer(v) = actual {
            assert_eq!(*v, exp);
        } else {
            panic!("{:#?} is not an int", actual);
        }
    }

    #[test]
    fn test_integer_arithmatic() {
        let tests = [CompilerIntTestCase {
            input: "1 + 2",
            expected_constants: &[1, 2],
            expected_instructions: &[
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
            ],
        }];

        for test in tests.iter() {
            run_int_compiler_test(test);
        }
    }
}
