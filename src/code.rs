use std::fmt::Display;

pub type Instructions = Vec<u8>;

pub trait InstructionsString {
    fn string(&self) -> String;
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
pub enum Opcode {
    OpConstant = 0,
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::OpConstant => write!(f, "{}", 0),
        }
    }
}

impl Into<Opcode> for u8 {
    fn into(self) -> Opcode {
        match self {
            0 => Opcode::OpConstant,
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
            let def = match lookup(&op) {
                Ok(d) => d,
                Err(e) => {
                    res.push_str(&format!("ERROR: {}\n", e));
                    continue;
                }
            };
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
        1 => format!("{} {}", def.name, operands[0]),
        _ => format!("ERROR: unhandled operand_count for {}\n", def.name),
    }
}

pub struct Definition {
    pub name: &'static str,
    pub operand_widths: &'static [usize],
}

const OP_CONSTANT: Definition = Definition {
    name: "OpConstant",
    operand_widths: &[2],
};

pub fn lookup(op: &Opcode) -> anyhow::Result<Definition> {
    match op {
        Opcode::OpConstant => Ok(OP_CONSTANT),
        _ => Err(anyhow::anyhow!("opcode {} undefined", op)),
    }
}

pub fn make(op: Opcode, operands: &[usize]) -> Vec<u8> {
    let mut instruction = Vec::new();
    let mut instruction_len = 1;
    let def = match lookup(&op) {
        Ok(d) => d,
        Err(_) => return instruction,
    };

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
        let tests = [MakeTest {
            op: Opcode::OpConstant,
            operands: &[65534],
            expected: &[Opcode::OpConstant as u8, 255, 254],
        }];

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
            make(Opcode::OpConstant, &[1]),
            make(Opcode::OpConstant, &[2]),
            make(Opcode::OpConstant, &[65535]),
        ];
        let exp = "0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535
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
            let def = match lookup(&test.op) {
                Ok(d) => d,
                Err(e) => panic!("{}", e),
            };

            let (operands_read, n) = read_operands(&def, &instruction[1..]);
            assert_eq!(n, test.bytes_read);
            for (i, want) in test.operands.iter().enumerate() {
                assert_eq!(operands_read[i], *want);
            }
        }
    }
}
