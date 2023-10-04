use crate::{
    ast::{
        Expression, InfixExpression, InfixOperator, IntegerLiteral, Node, PrefixOperator, Program,
        Statement,
    },
    code::{make, Instructions, Opcode},
    object::Object,
};

#[derive(Clone)]
pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
    pub last_instruction: Option<EmittedInstruction>,
    pub previous_instruction: Option<EmittedInstruction>,
    pub symbol_table: SymbolTable,
}

#[derive(Clone)]
pub struct EmittedInstruction {
    pub opcode: Opcode,
    pub position: usize,
}

pub struct ByteCode<'a> {
    pub instructions: &'a Instructions,
    pub constants: &'a Vec<Object>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SymbolScope {
    Global,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Symbol {
    pub name: std::sync::Arc<str>,
    pub scope: SymbolScope,
    pub idx: usize,
}

#[derive(Clone)]
pub struct SymbolTable {
    store: std::collections::HashMap<std::sync::Arc<str>, Symbol>,
    num_definitions: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let constants = Vec::new();
        let instructions = Vec::new();
        Self {
            instructions,
            constants,
            last_instruction: None,
            previous_instruction: None,
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn new_with_state(symbol_table: SymbolTable, constants: Vec<Object>) -> Self {
        let instructions = Vec::new();
        Self {
            instructions,
            constants,
            last_instruction: None,
            previous_instruction: None,
            symbol_table,
        }
    }

    pub fn compile(&mut self, program: &Program) -> anyhow::Result<()> {
        self.compile_statements(&program.statements)
    }

    pub fn byte_code(&self) -> ByteCode {
        ByteCode {
            instructions: &self.instructions,
            constants: &self.constants,
        }
    }

    fn compile_statements(&mut self, stmts: &Vec<Statement>) -> anyhow::Result<()> {
        for s in stmts.iter() {
            self.compile_statement(s)?;
        }
        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement) -> anyhow::Result<()> {
        match stmt {
            Statement::ExpressionStatement(es) => {
                self.compile_expression(&es.expression)?;
                self.emit(Opcode::OpPop, &[]);
            }
            Statement::LetStatement(ls) => {
                self.compile_expression(&ls.value)?;
                let symbol = self.symbol_table.define(ls.name.value.clone());
                self.emit(Opcode::OpSetGlobal, &[symbol.idx]);
            }
            _ => todo!(),
        };
        Ok(())
    }

    fn compile_expression(&mut self, exp: &Expression) -> anyhow::Result<()> {
        match exp {
            Expression::InfixExpression(ie) => {
                if ie.operator == InfixOperator::Lt {
                    self.compile_expression(&ie.right)?;
                    self.compile_expression(&ie.left)?;
                    self.emit(Opcode::OpGreaterThan, &[]);
                } else {
                    self.compile_infix_expression(&ie)?;
                }
            }
            Expression::Integer(i) => self.compile_integer_literal(&i)?,
            Expression::Boolean(b) => {
                if b.value {
                    self.emit(Opcode::OpTrue, &[]);
                } else {
                    self.emit(Opcode::OpFalse, &[]);
                }
            }
            Expression::PrefixExpression(pe) => {
                self.compile_expression(&pe.right)?;
                match pe.operator {
                    PrefixOperator::Minus => self.emit(Opcode::OpMinus, &[]),
                    PrefixOperator::Bang => self.emit(Opcode::OpBang, &[]),
                };
            }
            Expression::IfExpression(ife) => {
                self.compile_expression(&ife.condition)?;
                let jump_not_truthy_pos = self.emit(Opcode::OpJumpNotTruthy, &[9999]);
                self.compile_statements(&ife.consequence.statements)?;
                if self.last_instruction_is_pop() {
                    self.remove_last_pop()?;
                }
                let jump_pos = self.emit(Opcode::OpJump, &[9999]);
                let after_consequence_pos = self.instructions.len();
                self.change_operands(jump_not_truthy_pos, after_consequence_pos);
                match &ife.alternative {
                    None => {
                        self.emit(Opcode::OpNull, &[]);
                    }
                    Some(alt) => {
                        self.compile_statements(&alt.statements)?;
                        if self.last_instruction_is_pop() {
                            self.remove_last_pop()?;
                        }
                    }
                };
                let after_alternative_pos = self.instructions.len();
                self.change_operands(jump_pos, after_alternative_pos);
            }
            Expression::Identifier(ident) => {
                let symbol = match self.symbol_table.resolve(&ident.value) {
                    Some(s) => s,
                    None => return Err(anyhow::anyhow!("undefined variable {}", ident.value)),
                };
                self.emit(Opcode::OpGetGlobal, &[symbol.idx]);
            }
            Expression::String(s) => {
                let obj = Object::String(s.value.clone());
                let x = self.add_constant(obj);
                self.emit(Opcode::OpConstant, &[x]);
            }
            Expression::Array(arr) => {
                let len = arr.elements.len();
                for el in arr.elements.iter() {
                    self.compile_expression(el)?;
                }
                self.emit(Opcode::OpArray, &[len]);
            }
            Expression::Hash(hash) => {
                let mut keys = Vec::new();
                for k in hash.pairs.iter() {
                    keys.push(k.clone());
                }
                keys.sort_by(|a, b| a.0.string().cmp(&b.0.string()));
                for k in keys.iter() {
                    self.compile_expression(&k.0)?;
                    self.compile_expression(&k.1)?;
                }
                self.emit(Opcode::OpHash, &[keys.len() * 2]);
            }
            _ => todo!(),
        };
        Ok(())
    }

    fn compile_infix_expression(&mut self, ie: &InfixExpression) -> anyhow::Result<()> {
        self.compile_expression(&ie.left)?;
        self.compile_expression(&ie.right)?;
        match ie.operator {
            InfixOperator::Plus => self.emit(Opcode::OpAdd, &[]),
            InfixOperator::Minus => self.emit(Opcode::OpSub, &[]),
            InfixOperator::Asterisk => self.emit(Opcode::OpMul, &[]),
            InfixOperator::Slash => self.emit(Opcode::OpDiv, &[]),
            InfixOperator::Gt => self.emit(Opcode::OpGreaterThan, &[]),
            InfixOperator::Eq => self.emit(Opcode::OpEqual, &[]),
            InfixOperator::NotEq => self.emit(Opcode::OpNotEqual, &[]),
            _ => {
                return Err(anyhow::anyhow!(
                    "unkown operator {}",
                    ie.operator.to_string()
                ))
            }
        };
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
        let pos = self.add_instruction(&ins);
        self.set_last_instruction(op, pos);
        pos
    }

    fn add_instruction(&mut self, ins: &Vec<u8>) -> usize {
        let pos_new_instruction = self.instructions.len();
        for i in ins.iter() {
            self.instructions.push(*i);
        }
        pos_new_instruction
    }

    fn set_last_instruction(&mut self, opcode: Opcode, position: usize) {
        let prev = &self.previous_instruction;
        let last = EmittedInstruction { opcode, position };
        self.previous_instruction = prev.clone();
        self.last_instruction = Some(last);
    }

    fn last_instruction_is_pop(&self) -> bool {
        match &self.last_instruction {
            Some(l) => l.opcode == Opcode::OpPop,
            None => false,
        }
    }

    fn remove_last_pop(&mut self) -> anyhow::Result<()> {
        match &self.last_instruction {
            Some(l) => {
                self.instructions = self.instructions[0..l.position].to_owned();
                Ok(())
            }
            None => Err(anyhow::anyhow!("last instruction is None")),
        }
    }

    fn replace_instructions(&mut self, pos: usize, new_instruction: &[u8]) {
        for (i, ins) in new_instruction.iter().enumerate() {
            self.instructions[pos + i] = *ins;
        }
    }

    fn change_operands(&mut self, op_pos: usize, operand: usize) {
        let op_num = self.instructions[op_pos];
        let op: Opcode = op_num.into();
        let new_instruction = make(op, &[operand]);
        self.replace_instructions(op_pos, &new_instruction);
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        let store = std::collections::HashMap::new();
        let num_definitions = 0;
        Self {
            store,
            num_definitions,
        }
    }

    pub fn define(&mut self, key: std::sync::Arc<str>) -> Symbol {
        let sym = Symbol {
            name: key.clone(),
            scope: SymbolScope::Global,
            idx: self.num_definitions,
        };
        self.store.insert(key, sym.clone());
        self.num_definitions += 1;
        sym
    }

    pub fn resolve(&self, key: &std::sync::Arc<str>) -> Option<&Symbol> {
        self.store.get(key)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast::Program,
        code::{make, Instructions, Opcode},
        compiler::{Compiler, Symbol, SymbolScope, SymbolTable},
        lexer::Lexer,
        object::Object,
        parser::Parser,
    };

    struct CompilerIntTestCase<'a> {
        input: &'static str,
        expected_constants: &'static [i64],
        expected_instructions: &'a [Instructions],
    }

    struct CompilerBoolTestCase<'a> {
        input: &'static str,
        expected_constants: &'static [i64],
        expected_instructions: &'a [Instructions],
    }

    struct CompilerStringTestCase<'a> {
        input: &'static str,
        expected_constants: &'static [&'static str],
        expected_instructions: &'a [Instructions],
    }

    struct CompilerArrayTestCase<'a> {
        input: &'static str,
        expected_constants: &'a [i64],
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

    fn run_bool_compiler_test(test: &CompilerBoolTestCase) {
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

    fn run_string_compiler_test(test: &CompilerStringTestCase) {
        let program = parse(test.input);
        let mut compiler = Compiler::new();
        match compiler.compile(&program) {
            Ok(()) => {}
            Err(e) => panic!("{}", e),
        };
        let byte_code = compiler.byte_code();
        test_instructions(test.expected_instructions, byte_code.instructions);
        test_string_constants(test.expected_constants, byte_code.constants);
    }

    fn run_array_compiler_test(test: &CompilerArrayTestCase) {
        let program = parse(test.input);
        let mut compiler = Compiler::new();
        match compiler.compile(&program) {
            Ok(()) => {}
            Err(e) => panic!("{}", e),
        };
        let byte_code = compiler.byte_code();
        test_instructions(test.expected_instructions, byte_code.instructions);
        test_array_constants(test.expected_constants, byte_code.constants);
    }

    fn test_instructions(exp_instructions: &[Instructions], actual: &Instructions) {
        let concatted = concat_instructions(exp_instructions);
        println!("exp: {:?}\ngot: {:?}", concatted, actual);
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

    #[allow(unused)]
    fn test_bool_constants(exp_constants: &[bool], constants: &Vec<Object>) {
        assert_eq!(exp_constants.len(), constants.len());
        for (i, exp) in exp_constants.iter().enumerate() {
            test_bool_object(*exp, &constants[i]);
        }
    }

    fn test_string_constants(exp_constants: &[&'static str], constants: &Vec<Object>) {
        assert_eq!(exp_constants.len(), constants.len());
        for (i, exp) in exp_constants.iter().enumerate() {
            test_string_obj(*exp, &constants[i]);
        }
    }

    fn test_array_constants(exp_constants: &[i64], constants: &Vec<Object>) {
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

    fn test_string_obj(exp: &'static str, actual: &Object) {
        if let Object::String(v) = actual {
            assert_eq!(*v, exp.into());
        } else {
            panic!("{:#?} is not a string", actual);
        }
    }

    #[allow(unused)]
    fn test_array_obj(exp: &Vec<i64>, actual: &Object) {
        if let Object::Array(v) = actual {
            assert_eq!(exp.len(), v.elements.len());
            for (i, val) in v.elements.iter().enumerate() {
                test_integer_object(exp[i], val);
            }
        } else {
            panic!("{:#?} is not a string", actual);
        }
    }

    #[allow(unused)]
    fn test_bool_object(exp: bool, actual: &Object) {
        if let Object::Boolean(v) = actual {
            assert_eq!(*v, exp);
        } else {
            panic!("{:#?} is not an int", actual);
        }
    }

    #[test]
    fn test_integer_arithmatic() {
        let tests = [
            CompilerIntTestCase {
                input: "1 + 2",
                expected_constants: &[1, 2],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpAdd, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerIntTestCase {
                input: "1; 2",
                expected_constants: &[1, 2],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpPop, &[]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerIntTestCase {
                input: "1 - 2",
                expected_constants: &[1, 2],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpSub, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerIntTestCase {
                input: "1 * 2",
                expected_constants: &[1, 2],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpMul, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerIntTestCase {
                input: "2 / 1",
                expected_constants: &[2, 1],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpDiv, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerIntTestCase {
                input: "-1",
                expected_constants: &[1],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpMinus, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
        ];

        for test in tests.iter() {
            run_int_compiler_test(test);
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = [
            CompilerBoolTestCase {
                input: "true",
                expected_constants: &[],
                expected_instructions: &[make(Opcode::OpTrue, &[]), make(Opcode::OpPop, &[])],
            },
            CompilerBoolTestCase {
                input: "false",
                expected_constants: &[],
                expected_instructions: &[make(Opcode::OpFalse, &[]), make(Opcode::OpPop, &[])],
            },
            CompilerBoolTestCase {
                input: "1 > 2",
                expected_constants: &[1, 2],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpGreaterThan, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerBoolTestCase {
                input: "1 < 2",
                expected_constants: &[2, 1],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpGreaterThan, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerBoolTestCase {
                input: "1 == 2",
                expected_constants: &[1, 2],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpEqual, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerBoolTestCase {
                input: "1 != 2",
                expected_constants: &[1, 2],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpNotEqual, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerBoolTestCase {
                input: "true == false",
                expected_constants: &[],
                expected_instructions: &[
                    make(Opcode::OpTrue, &[]),
                    make(Opcode::OpFalse, &[]),
                    make(Opcode::OpEqual, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerBoolTestCase {
                input: "true != false",
                expected_constants: &[],
                expected_instructions: &[
                    make(Opcode::OpTrue, &[]),
                    make(Opcode::OpFalse, &[]),
                    make(Opcode::OpNotEqual, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerBoolTestCase {
                input: "!true",
                expected_constants: &[],
                expected_instructions: &[
                    make(Opcode::OpTrue, &[]),
                    make(Opcode::OpBang, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
        ];

        for test in tests.iter() {
            run_bool_compiler_test(test);
        }
    }

    #[test]
    fn test_conditionals() {
        let tests = [CompilerIntTestCase {
            input: "if (true) { 10 }; 3333;",
            expected_constants: &[10, 3333],
            expected_instructions: &[
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpJumpNotTruthy, &[10]),
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpJump, &[11]),
                make(Opcode::OpNull, &[]),
                make(Opcode::OpPop, &[]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpPop, &[]),
            ],
        }];

        for test in tests.iter() {
            run_int_compiler_test(test);
        }
    }

    #[test]
    fn test_global_let_statement() {
        let tests = [
            CompilerIntTestCase {
                input: "let one = 1;
                    let two = 2;",
                expected_constants: &[1, 2],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpSetGlobal, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpSetGlobal, &[1]),
                ],
            },
            CompilerIntTestCase {
                input: "let one = 1;
                       one;",
                expected_constants: &[1],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpSetGlobal, &[0]),
                    make(Opcode::OpGetGlobal, &[0]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerIntTestCase {
                input: "let one = 1;
                    let two = one;
                    two;",
                expected_constants: &[1],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpSetGlobal, &[0]),
                    make(Opcode::OpGetGlobal, &[0]),
                    make(Opcode::OpSetGlobal, &[1]),
                    make(Opcode::OpGetGlobal, &[1]),
                    make(Opcode::OpPop, &[]),
                ],
            },
        ];

        for test in tests.iter() {
            run_int_compiler_test(test);
        }
    }

    #[test]
    fn test_define() {
        let expected = std::collections::HashMap::from([
            (
                "a",
                Symbol {
                    name: "a".into(),
                    scope: SymbolScope::Global,
                    idx: 0,
                },
            ),
            (
                "b",
                Symbol {
                    name: "b".into(),
                    scope: SymbolScope::Global,
                    idx: 1,
                },
            ),
        ]);
        let mut global = SymbolTable::new();
        let a = global.define("a".into());
        assert_eq!(a, expected["a"]);
        let b = global.define("b".into());
        assert_eq!(b, expected["b"]);
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a".into());
        global.define("b".into());
        let expected = [
            Symbol {
                name: "a".into(),
                scope: SymbolScope::Global,
                idx: 0,
            },
            Symbol {
                name: "b".into(),
                scope: SymbolScope::Global,
                idx: 1,
            },
        ];

        for sym in expected.iter() {
            let result = match global.resolve(&sym.name) {
                Some(v) => v,
                None => panic!("global.resolve returned None for {}", sym.name),
            };
            assert_eq!(*result, *sym);
        }
    }

    #[test]
    fn test_string_expressions() {
        let tests = [
            CompilerStringTestCase {
                input: "\"monkey\"",
                expected_constants: &["monkey"],
                expected_instructions: &[make(Opcode::OpConstant, &[0]), make(Opcode::OpPop, &[])],
            },
            CompilerStringTestCase {
                input: "\"mon\" + \"key\"",
                expected_constants: &["mon", "key"],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpAdd, &[]),
                    make(Opcode::OpPop, &[]),
                ],
            },
        ];

        for test in tests.iter() {
            run_string_compiler_test(test);
        }
    }

    #[test]
    fn test_array_literals() {
        let tests = [
            CompilerArrayTestCase {
                input: "[]",
                expected_constants: &[],
                expected_instructions: &[make(Opcode::OpArray, &[0]), make(Opcode::OpPop, &[])],
            },
            CompilerArrayTestCase {
                input: "[1, 2, 3]",
                expected_constants: &[1, 2, 3],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpConstant, &[2]),
                    make(Opcode::OpArray, &[3]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerArrayTestCase {
                input: "[1 + 2, 3 - 4, 5 * 6]",
                expected_constants: &[1, 2, 3, 4, 5, 6],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpAdd, &[]),
                    make(Opcode::OpConstant, &[2]),
                    make(Opcode::OpConstant, &[3]),
                    make(Opcode::OpSub, &[]),
                    make(Opcode::OpConstant, &[4]),
                    make(Opcode::OpConstant, &[5]),
                    make(Opcode::OpMul, &[]),
                    make(Opcode::OpArray, &[3]),
                    make(Opcode::OpPop, &[]),
                ],
            },
        ];

        for test in tests.iter() {
            run_array_compiler_test(test);
        }
    }

    #[test]
    fn test_hash_literals() {
        let tests = [
            CompilerArrayTestCase {
                input: "{}",
                expected_constants: &[],
                expected_instructions: &[make(Opcode::OpHash, &[0]), make(Opcode::OpPop, &[])],
            },
            CompilerArrayTestCase {
                input: "{1: 2, 3: 4, 5: 6}",
                expected_constants: &[1, 2, 3, 4, 5, 6],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpConstant, &[2]),
                    make(Opcode::OpConstant, &[3]),
                    make(Opcode::OpConstant, &[4]),
                    make(Opcode::OpConstant, &[5]),
                    make(Opcode::OpHash, &[6]),
                    make(Opcode::OpPop, &[]),
                ],
            },
            CompilerArrayTestCase {
                input: "{1: 2 + 3, 4: 5 * 6}",
                expected_constants: &[1, 2, 3, 4, 5, 6],
                expected_instructions: &[
                    make(Opcode::OpConstant, &[0]),
                    make(Opcode::OpConstant, &[1]),
                    make(Opcode::OpConstant, &[2]),
                    make(Opcode::OpAdd, &[]),
                    make(Opcode::OpConstant, &[3]),
                    make(Opcode::OpConstant, &[4]),
                    make(Opcode::OpConstant, &[5]),
                    make(Opcode::OpMul, &[]),
                    make(Opcode::OpHash, &[4]),
                    make(Opcode::OpPop, &[]),
                ],
            },
        ];

        for test in tests.iter() {
            run_array_compiler_test(test);
        }
    }
}
