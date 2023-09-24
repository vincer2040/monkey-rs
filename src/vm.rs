use crate::{object::Object, code::{Instructions, Opcode}, compiler::ByteCode};



const STACK_SIZE: u16 = 2048;

pub struct VM<'a> {
    constants: &'a Vec<Object>,
    instructions: &'a Instructions,
    stack: [Option<&'a Object>; STACK_SIZE as usize],
    sp: u16,
}

impl<'a> VM<'a> {
    pub fn new(byte_code: &'a ByteCode) -> Self {
        Self {
            constants: byte_code.constants,
            instructions: byte_code.instructions,
            stack: [None; STACK_SIZE as usize],
            sp: 0,
        }
    }

    pub fn stack_top(&self) -> Option<&Object> {
        if self.sp == 0 {
            return None;
        }
        self.stack[(self.sp -1) as usize]
    }

    pub fn run(&mut self) -> anyhow::Result<()> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op: Opcode = self.instructions[ip].into();
            match op {
                Opcode::OpConstant => {
                    let mut const_idx: usize = 0;
                    ip += 1;
                    let s = self.instructions[ip] as u16;
                    ip += 1;
                    let e = self.instructions[ip];
                    const_idx |= (s << 8) as usize;
                    const_idx |= e as usize;
                    let o = &self.constants[const_idx];
                    self.push(o)?;
                }
                Opcode::OpAdd => {
                    let left = match self.pop() {
                        Some(l) => l.clone(),
                        None => return Err(anyhow::anyhow!("pop returned none")),
                    };
                    let right = match self.pop() {
                        Some(r) => r.clone(),
                        None => return Err(anyhow::anyhow!("pop returned none")),
                    };
                    if let Object::Integer(lval) = left {
                        if let Object::Integer(rval) = right {
                            // TODO: fix this
                            let res: &'static Object = Box::leak(Box::new(Object::Integer(lval + rval)));
                            self.push(&res)?;
                        } else {
                            return Err(anyhow::anyhow!("{:#?} is not an integer", right));
                        }
                    } else {
                        return Err(anyhow::anyhow!("{:#?} is not an integer", left));
                    }
                }
            };
            ip += 1;
        }
        Ok(())
    }

    fn push(&mut self, obj: &'a Object) -> anyhow::Result<()> {
        if self.sp >= STACK_SIZE {
            return Err(anyhow::anyhow!("stack overflow"));
        }
        self.stack[self.sp as usize] = Some(obj);
        self.sp += 1;
        Ok(())
    }

    fn pop(&mut self) -> Option<&Object> {
        let o = self.stack[(self.sp - 1) as usize];
        self.sp -= 1;
        o
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::ast::Program;
    use crate::object::Object;

    use super::VM;

    struct VmTestIntCase {
        input: &'static str,
        expected: i64,
    }

    fn parse(input: &'static str) -> Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        p.parse()
    }

    fn test_integer_object(exp: i64, actual: &Object) {
        if let Object::Integer(v) = actual {
            assert_eq!(*v, exp);
        } else {
            panic!("{:#?} is not an int", actual);
        }
    }

    fn run_int_vm_test(test: &VmTestIntCase) -> anyhow::Result<()> {
        let program = parse(test.input);
        let mut compiler = Compiler::new();
        compiler.compile(&program)?;
        let byte_code = compiler.byte_code();
        let mut vm = VM::new(&byte_code);
        vm.run()?;
        let stack_elem = vm.stack_top();
        if let Some(obj) = stack_elem {
            test_integer_object(test.expected, obj);
        } else {
            panic!("stack_top returned None");
        }
        Ok(())
    }

    #[test]
    fn test_integer_arithmatic() -> anyhow::Result<()>{
        let tests = [
            VmTestIntCase {
                input: "1",
                expected: 1,
            },
            VmTestIntCase {
                input: "2",
                expected: 2,
            },
            VmTestIntCase {
                input: "1 + 2",
                expected: 3,
            },
        ];

        for test in tests.iter() {
            run_int_vm_test(test)?;
        }

        Ok(())
    }
}
