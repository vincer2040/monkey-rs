use crate::{
    code::{Instructions, Opcode},
    compiler::ByteCode,
    object::{Object, ObjectTrait, ObjectType},
};

const STACK_SIZE: u16 = 2048;
const INIT: Option<Object> = None;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

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
                }
                Opcode::OpTrue => self.push(TRUE)?,
                Opcode::OpFalse => self.push(FALSE)?,
                Opcode::OpEqual | Opcode::OpNotEqual | Opcode::OpGreaterThan => {
                    self.execute_comparison(op)?;
                }
                Opcode::OpBang => self.execute_bang_operator()?,
                Opcode::OpMinus => self.execute_minus_operator()?,
                _ => todo!(),
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

    fn execute_comparison(&mut self, op: Opcode) -> anyhow::Result<()> {
        let right = match self.pop() {
            Some(r) => r.clone(),
            None => return Err(anyhow::anyhow!("pop returned none")),
        };
        let left = match self.pop() {
            Some(l) => l.clone(),
            None => return Err(anyhow::anyhow!("pop returned none")),
        };

        if left.type_val() == ObjectType::Integer && right.type_val() == ObjectType::Integer {
            let lval = match left {
                Object::Integer(v) => v,
                _ => unreachable!("typeval is integer but {:#?} is not an integer", left),
            };
            let rval = match right {
                Object::Integer(v) => v,
                _ => unreachable!("typeval is integer but {:#?} is not an integer", left),
            };
            return self.execute_integer_comparison(lval, rval, op);
        }
        match op {
            Opcode::OpEqual => self.push(VM::native_bool_to_boolean_object(right == left))?,
            Opcode::OpNotEqual => self.push(VM::native_bool_to_boolean_object(right != left))?,
            _ => {
                return Err(anyhow::anyhow!(
                    "unkown operator {} ({} {})",
                    op,
                    left.type_string(),
                    right.type_string()
                ))
            }
        };
        Ok(())
    }

    fn execute_integer_comparison(
        &mut self,
        lval: i64,
        rval: i64,
        op: Opcode,
    ) -> anyhow::Result<()> {
        match op {
            Opcode::OpEqual => self.push(VM::native_bool_to_boolean_object(lval == rval))?,
            Opcode::OpNotEqual => self.push(VM::native_bool_to_boolean_object(lval != rval))?,
            Opcode::OpGreaterThan => self.push(VM::native_bool_to_boolean_object(lval > rval))?,
            _ => return Err(anyhow::anyhow!("unknown operator: {}", op)),
        };
        Ok(())
    }

    fn execute_bang_operator(&mut self) -> anyhow::Result<()> {
        let operand = self.pop();
        match operand {
            Some(v) => {
                if *v == TRUE {
                    return self.push(FALSE);
                } else if *v == FALSE {
                    return self.push(TRUE);
                } else {
                    return self.push(FALSE);
                }
            }
            None => Err(anyhow::anyhow!("pop returned None")),
        }
    }

    fn execute_minus_operator(&mut self) -> anyhow::Result<()> {
        let operand = match self.pop() {
            Some(v) => v,
            None => return Err(anyhow::anyhow!("pop returned None")),
        };
        match operand {
            Object::Integer(i) => {
                let obj = Object::Integer(-i);
                self.push(obj)
            }
            _ => Err(anyhow::anyhow!(
                "unsupported type for negation: {}",
                operand.type_string()
            )),
        }
    }

    fn native_bool_to_boolean_object(input: bool) -> Object {
        if input {
            TRUE
        } else {
            FALSE
        }
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

    struct VmTestBoolCase {
        input: &'static str,
        expected: bool,
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

    fn test_bool_object(exp: bool, actual: &Object) {
        if let Object::Boolean(v) = actual {
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

    fn run_bool_vm_test(test: &VmTestBoolCase) -> anyhow::Result<()> {
        let program = parse(test.input);
        let mut compiler = Compiler::new();
        compiler.compile(&program)?;
        let byte_code = compiler.byte_code();
        let mut vm = VM::new(&byte_code);
        vm.run()?;
        let stack_elem = vm.last_popped_stack_elem();
        if let Some(obj) = stack_elem {
            test_bool_object(test.expected, obj);
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
            VmTestIntCase {
                input: "-5",
                expected: -5,
            },
            VmTestIntCase {
                input: "-10",
                expected: -10,
            },
            VmTestIntCase {
                input: "-50 + 100 + -50",
                expected: 0,
            },
            VmTestIntCase {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                expected: 50,
            },
        ];

        for test in tests.iter() {
            run_int_vm_test(test)?;
        }

        Ok(())
    }

    #[test]
    fn test_boolean_expressions() -> anyhow::Result<()> {
        let tests = [
            VmTestBoolCase {
                input: "true",
                expected: true,
            },
            VmTestBoolCase {
                input: "false",
                expected: false,
            },
            VmTestBoolCase {
                input: "1 < 2",
                expected: true,
            },
            VmTestBoolCase {
                input: "1 > 2",
                expected: false,
            },
            VmTestBoolCase {
                input: "1 < 1",
                expected: false,
            },
            VmTestBoolCase {
                input: "1 > 1",
                expected: false,
            },
            VmTestBoolCase {
                input: "1 == 1",
                expected: true,
            },
            VmTestBoolCase {
                input: "1 != 1",
                expected: false,
            },
            VmTestBoolCase {
                input: "1 == 2",
                expected: false,
            },
            VmTestBoolCase {
                input: "1 != 2",
                expected: true,
            },
            VmTestBoolCase {
                input: "true == true",
                expected: true,
            },
            VmTestBoolCase {
                input: "false == false",
                expected: true,
            },
            VmTestBoolCase {
                input: "true == false",
                expected: false,
            },
            VmTestBoolCase {
                input: "true != false",
                expected: true,
            },
            VmTestBoolCase {
                input: "false != true",
                expected: true,
            },
            VmTestBoolCase {
                input: "(1 < 2) == true",
                expected: true,
            },
            VmTestBoolCase {
                input: "(1 < 2) == false",
                expected: false,
            },
            VmTestBoolCase {
                input: "(1 > 2) == true",
                expected: false,
            },
            VmTestBoolCase {
                input: "(1 > 2) == false",
                expected: true,
            },
            VmTestBoolCase {
                input: "!true",
                expected: false,
            },
            VmTestBoolCase {
                input: "!false",
                expected: true,
            },
            VmTestBoolCase {
                input: "!5",
                expected: false,
            },
            VmTestBoolCase {
                input: "!!true",
                expected: true,
            },
            VmTestBoolCase {
                input: "!!false",
                expected: false,
            },
            VmTestBoolCase {
                input: "!!5",
                expected: true,
            },
        ];

        for test in tests.iter() {
            run_bool_vm_test(test)?;
        }
        Ok(())
    }
}
