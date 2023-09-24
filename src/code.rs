use std::fmt::Display;

pub type Instructions = Vec<u8>;

pub struct Definition {
    pub name: &'static str,
    pub operand_widths: &'static [usize],
}

const OP_CONSTANT: Definition = Definition {
    name: "OpConstant",
    operand_widths: &[2],
};

const OP_ADD: Definition = Definition {
    name: "OpAdd",
    operand_widths: &[],
};

const OP_POP: Definition = Definition {
    name: "OpPop",
    operand_widths: &[],
};

const OP_SUB: Definition = Definition {
    name: "OpSub",
    operand_widths: &[],
};

const OP_MUL: Definition = Definition {
    name: "OpMul",
    operand_widths: &[],
};

const OP_DIV: Definition = Definition {
    name: "OpDiv",
    operand_widths: &[],
};

const OP_TRUE: Definition = Definition {
    name: "OpTrue",
    operand_widths: &[],
};

const OP_FALSE: Definition = Definition {
    name: "OpTrue",
    operand_widths: &[],
};

const OP_EQUAL: Definition = Definition {
    name: "OpEqual",
    operand_widths: &[],
};

const OP_NOT_EQUAL: Definition = Definition {
    name: "OpNotEqual",
    operand_widths: &[],
};

const OP_GREATER_THAN: Definition = Definition {
    name: "OpGreaterThan",
    operand_widths: &[],
};

const OP_MINUS: Definition = Definition {
    name: "OpMinus",
    operand_widths: &[],
};

const OP_BANG: Definition = Definition {
    name: "OpBang",
    operand_widths: &[],
};

pub trait InstructionsString {
    fn string(&self) -> String;
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
pub enum Opcode {
    OpConstant = 0,
    OpAdd = 1,
    OpPop = 2,
    OpSub = 3,
    OpMul = 4,
    OpDiv = 5,
    OpTrue = 6,
    OpFalse = 7,
    OpEqual = 8,
    OpNotEqual = 9,
    OpGreaterThan = 10,
    OpMinus = 11,
    OpBang = 12,
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::OpConstant => write!(f, "{}", 0),
            Opcode::OpAdd => write!(f, "{}", 1),
            Opcode::OpPop => write!(f, "{}", 2),
            Opcode::OpSub => write!(f, "{}", 3),
            Opcode::OpMul => write!(f, "{}", 4),
            Opcode::OpDiv => write!(f, "{}", 5),
            Opcode::OpTrue => write!(f, "{}", 6),
            Opcode::OpFalse => write!(f, "{}", 7),
            Opcode::OpEqual => write!(f, "{}", 8),
            Opcode::OpNotEqual => write!(f, "{}", 9),
            Opcode::OpGreaterThan => write!(f, "{}", 10),
            Opcode::OpMinus => write!(f, "{}", 11),
            Opcode::OpBang => write!(f, "{}", 12),
        }
    }
}

impl Into<Opcode> for u8 {
    fn into(self) -> Opcode {
        match self {
            0 => Opcode::OpConstant,
            1 => Opcode::OpAdd,
            2 => Opcode::OpPop,
            3 => Opcode::OpSub,
            4 => Opcode::OpMul,
            5 => Opcode::OpDiv,
            6 => Opcode::OpTrue,
            7 => Opcode::OpFalse,
            8 => Opcode::OpEqual,
            9 => Opcode::OpNotEqual,
            10 => Opcode::OpGreaterThan,
            11 => Opcode::OpMinus,
            12 => Opcode::OpBang,
            _ => unreachable!("unkown u8 opcode {}", self),
        }
    }
}

impl InstructionsString for Instructions {
    fn string(&self) -> String {
        let mut res = String::new();
        let mut i = 0;
        while i < self.len() {
            let op: Opcode = self[i].into();
            let def = lookup(&op);
            let (operands, read) = read_operands(&def, &self[i + 1..]);
            res.push_str(&format!(
                "{:04} {}\n",
                i,
                fmt_instruction(&self, &def, &operands)
            ));
            i += 1 + read;
        }
        res
    }
}

fn fmt_instruction(ins: &Instructions, def: &Definition, operands: &[usize]) -> String {
    let operand_count = def.operand_widths.len();
    let operands_len = operands.len();
    if operand_count != operands_len {
        return format!(
            "ERROR: operands len {} does not match defined {}\n",
            operands_len, operand_count
        );
    }
    match operand_count {
        0 => def.name.to_owned(),
        1 => format!("{} {}", def.name, operands[0]),
        _ => format!("ERROR: unhandled operand_count for {}\n", def.name),
    }
}

pub fn lookup(op: &Opcode) -> Definition {
    match op {
        Opcode::OpConstant => OP_CONSTANT,
        Opcode::OpAdd => OP_ADD,
        Opcode::OpPop => OP_POP,
        Opcode::OpSub => OP_SUB,
        Opcode::OpMul => OP_MUL,
        Opcode::OpDiv => OP_DIV,
        Opcode::OpTrue => OP_TRUE,
        Opcode::OpFalse => OP_FALSE,
        Opcode::OpEqual => OP_EQUAL,
        Opcode::OpNotEqual => OP_NOT_EQUAL,
        Opcode::OpGreaterThan => OP_GREATER_THAN,
        Opcode::OpMinus => OP_MINUS,
        Opcode::OpBang => OP_BANG,
    }
}

pub fn make(op: Opcode, operands: &[usize]) -> Vec<u8> {
    let mut instruction = Vec::new();
    let mut instruction_len = 1;
    let def = lookup(&op);

    for w in def.operand_widths {
        instruction_len += w;
    }
    instruction.reserve(instruction_len);
    instruction.push(op as u8);

    for (i, o) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        match width {
            2 => {
                let s = (o >> 8) as u8;
                let e = *o as u8;
                instruction.push(s);
                instruction.push(e);
            }
            _ => unreachable!("unkown width {}", width),
        }
    }
    instruction
}

pub fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<usize>, usize) {
    let mut operands = Vec::with_capacity(def.operand_widths.len());
    let mut offset = 0;

    for width in def.operand_widths.iter() {
        match width {
            2 => {
                let mut r: usize = 0;
                let s = ins[offset] as u16;
                offset += 1;
                let e = ins[offset];
                r |= (s << 8) as usize;
                r |= e as usize;
                operands.push(r);
                offset += 1;
            }
            _ => unreachable!("unkown width: {}", width),
        };
    }
    (operands, offset)
}

#[cfg(test)]
mod test {
    use crate::code::{make, Instructions, InstructionsString, Opcode};

    use super::{lookup, read_operands};

    struct MakeTest {
        op: Opcode,
        operands: &'static [usize],
        expected: &'static [u8],
    }

    struct ReadTest {
        op: Opcode,
        operands: &'static [usize],
        bytes_read: usize,
    }

    #[test]
    fn test_make() {
        let tests = [
            MakeTest {
                op: Opcode::OpConstant,
                operands: &[65534],
                expected: &[Opcode::OpConstant as u8, 255, 254],
            },
            MakeTest {
                op: Opcode::OpAdd,
                operands: &[],
                expected: &[Opcode::OpAdd as u8],
            },
        ];

        for test in tests {
            let instruction = make(test.op, test.operands);
            assert_eq!(instruction.len(), test.expected.len());

            for (i, b) in test.expected.iter().enumerate() {
                assert_eq!(instruction[i], *b);
            }
        }
    }

    #[test]
    fn test_instruction_string() {
        let instructions = [
            make(Opcode::OpAdd, &[]),
            make(Opcode::OpConstant, &[2]),
            make(Opcode::OpConstant, &[65535]),
        ];
        let exp = "0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
";
        let mut concatted: Instructions = Vec::new();
        for ins in instructions.iter() {
            for x in ins.iter() {
                concatted.push(*x);
            }
        }

        let s = concatted.string();
        assert_eq!(s, exp.to_owned());
    }

    #[test]
    fn test_read_operands() {
        let tests = [ReadTest {
            op: Opcode::OpConstant,
            operands: &[65535],
            bytes_read: 2,
        }];

        for test in tests.iter() {
            let instruction = make(test.op, test.operands);
            let def = lookup(&test.op);

            let (operands_read, n) = read_operands(&def, &instruction[1..]);
            assert_eq!(n, test.bytes_read);
            for (i, want) in test.operands.iter().enumerate() {
                assert_eq!(operands_read[i], *want);
            }
        }
    }
}
