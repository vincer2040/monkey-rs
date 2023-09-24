use crate::{
    code::{Instructions, Opcode},
    compiler::ByteCode,
    object::Object,
};

const STACK_SIZE: u16 = 2048;
const INIT: Option<Object> = None;

pub struct VM<'a> {
    constants: &'a Vec<Object>,
    instructions: &'a Instructions,
    stack: [Option<Object>; STACK_SIZE as usize],
    sp: u16,
}

impl<'a> VM<'a> {
    pub fn new(byte_code: &'a ByteCode) -> Self {
        Self {
            constants: byte_code.constants,
            instructions: byte_code.instructions,
            stack: [INIT; STACK_SIZE as usize],
            sp: 0,
        }
    }

    pub fn stack_top(&self) -> Option<&Object> {
        if self.sp == 0 {
            return None;
        }
        self.stack[(self.sp - 1) as usize].as_ref()
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
                    self.push(o.clone())?;
                }
                Opcode::OpAdd | Opcode::OpMul | Opcode::OpSub | Opcode::OpDiv => {
                    let right = match self.pop() {
                        Some(l) => l.clone(),
                        None => return Err(anyhow::anyhow!("pop returned none")),
                    };
                    let left = match self.pop() {
                        Some(r) => r.clone(),
                        None => return Err(anyhow::anyhow!("pop returned none")),
                    };
                    if let Object::Integer(lval) = left {
                        if let Object::Integer(rval) = right {
                            self.execute_binary_operation(lval, rval, op)?;
                        } else {
                            return Err(anyhow::anyhow!("{:#?} is not an integer", right));
                        }
                    } else {
                        return Err(anyhow::anyhow!("{:#?} is not an integer", left));
                    }
                }
                Opcode::OpPop => {
                    self.pop();
                } // _ => todo!(),
            };
            ip += 1;
        }
        Ok(())
    }

    pub fn last_popped_stack_elem(&self) -> Option<&Object> {
        self.stack[self.sp as usize].as_ref()
    }

    fn execute_binary_operation(&mut self, lval: i64, rval: i64, op: Opcode) -> anyhow::Result<()> {
        let result = match op {
            Opcode::OpAdd => lval + rval,
            Opcode::OpSub => lval - rval,
            Opcode::OpMul => lval * rval,
            Opcode::OpDiv => lval / rval,
            _ => {
                return Err(anyhow::anyhow!("unkown integer operator: {}", op));
            }
        };
        let obj = Object::Integer(result);
        self.push(obj)?;
        Ok(())
    }

    fn push(&mut self, obj: Object) -> anyhow::Result<()> {
        if self.sp >= STACK_SIZE {
            return Err(anyhow::anyhow!("stack overflow"));
        }
        self.stack[self.sp as usize] = Some(obj);
        self.sp += 1;
        Ok(())
    }

    fn pop(&mut self) -> Option<&Object> {
        let o = &self.stack[(self.sp - 1) as usize];
        self.sp -= 1;
        o.as_ref()
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Program;
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

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
        let stack_elem = vm.last_popped_stack_elem();
        if let Some(obj) = stack_elem {
            test_integer_object(test.expected, obj);
        } else {
            panic!("stack_top returned None");
        }
        Ok(())
    }

    #[test]
    fn test_integer_arithmatic() -> anyhow::Result<()> {
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
            VmTestIntCase {
                input: "1 - 2",
                expected: -1,
            },
            VmTestIntCase {
                input: "4 / 2",
                expected: 2,
            },
            VmTestIntCase {
                input: "50 / 2 * 2 + 10 - 5",
                expected: 55,
            },
            VmTestIntCase {
                input: "5 + 5 + 5 + 5 - 10",
                expected: 10,
            },
            VmTestIntCase {
                input: "2 * 2 * 2 * 2 * 2",
                expected: 32,
            },
            VmTestIntCase {
                input: "5 * 2 + 10",
                expected: 20,
            },
            VmTestIntCase {
                input: "5 + 2 * 10",
                expected: 25,
            },
            VmTestIntCase {
                input: "5 * (2 + 10)",
                expected: 60,
            },
        ];

        for test in tests.iter() {
            run_int_vm_test(test)?;
        }

        Ok(())
    }
}
