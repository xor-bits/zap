use std::{
    collections::{hash_map::Entry, HashMap},
    io::{BufRead, Read, Write},
    mem::{size_of, transmute},
};

use macros::Opcode;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub enum Error {
    Stdio(std::io::Error),
    InvalidOpcode,
    Incompatible,
    RedefinedSymbol(String),
    UnresolvedSymbol(String),
    InvalidOperand,
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Self::Stdio(value)
    }
}

//

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub struct Instruction {}

// impl Instruction {
//     pub fn encode(self, writer: &mut impl std::io::Write) -> Result<()> {
//         match self {
//             Instruction::I32Const(i) => {
//                 writer.write_all(&[1])?;
//                 writer.write_all(&i.to_le_bytes())?;
//             }
//             Instruction::I32Add => todo!(),
//             Instruction::I32Print => todo!(),
//         }

//         Ok(())
//     }

//     pub fn decode(reader: &mut impl std::io::Read) -> Result<Self> {
//         let mut opcode = [0u8];
//         reader.read_exact(&mut opcode)?

//         if opcode[0] > 3 {
//             return Err(Error::InvalidOpcode)
//         }

//         todo!()
//     }
// }

// impl

#[derive(Debug, Clone, Copy, PartialEq, Eq, Opcode)]
#[repr(u8)]
pub enum Opcode {
    /// invalid opcode
    Invalid,

    /// push i32 const
    I32Const,

    /// push i32 const from a register
    I32Load,

    /// pop i32 const into a register
    I32Store,

    /// pop 2xi32 and push the sum back
    I32Add,

    /// **debug print**, pop i32 and print it
    I32Print,

    /// jump into u64 const
    Goto,

    /// jump into u64 const and push a u64 return address
    Call,

    /// pop a u64 return address and jump into it
    Return,

    /// pop i32 and exit with that status
    Exit,
}

//

#[repr(C, packed)]
pub struct BytecodeHeader {
    pub file_type: [u8; 8],
    pub magic: u64,
    pub version: u64,
}

impl BytecodeHeader {
    pub const SIZE: usize = size_of::<Self>();
    const THIS: Self = Self {
        file_type: *b"UNI_LANG",
        magic: 0,
        version: 1,
    };
    const THIS_BYTES: [u8; Self::SIZE] = unsafe { transmute(Self::THIS) };

    pub fn encode(writer: &mut dyn Write) -> Result<()> {
        writer.write_all(&Self::THIS_BYTES)?;
        Ok(())
    }

    pub fn decode(reader: &mut dyn Read) -> Result<Self> {
        let mut header = [0u8; 24];
        reader.read_exact(&mut header)?;
        Ok(Self::from_bytes(header))
    }

    pub fn from_bytes(bytes: [u8; Self::SIZE]) -> Self {
        unsafe { transmute(bytes) }
    }

    pub fn is_compatible(&self) -> bool {
        self.file_type == Self::THIS.file_type
            && self.magic == Self::THIS.magic
            && self.version <= 1
    }
}

//

pub struct Resolver<'a> {
    labels: HashMap<&'a str, LabelState>,
}

impl<'a> Resolver<'a> {
    pub fn new() -> Self {
        Self {
            labels: HashMap::new(),
        }
    }

    pub fn found_label(&mut self, label: &'a str, bytecode: &mut Vec<u8>) -> Result<()> {
        match self.labels.entry(label) {
            Entry::Occupied(mut entry) => {
                match entry.insert(LabelState::Found { ip: bytecode.len() }) {
                    LabelState::Missing { needs_ip } => {
                        for reference in needs_ip {
                            let ip = bytecode.len();
                            bytecode[reference..reference + 8].copy_from_slice(&ip.to_ne_bytes());
                        }
                    }
                    LabelState::Found { .. } => {
                        return Err(Error::RedefinedSymbol(label.to_string()))
                    }
                }
            }
            Entry::Vacant(entry) => _ = entry.insert(LabelState::Found { ip: bytecode.len() }),
        }

        Ok(())
    }

    pub fn lazy_resolve(&mut self, label: &'a str, bytecode: &mut Vec<u8>) -> Result<()> {
        match self
            .labels
            .entry(label)
            .or_insert_with(|| LabelState::Missing {
                needs_ip: Vec::new(),
            }) {
            LabelState::Missing { needs_ip } => {
                needs_ip.push(bytecode.len());
                bytecode.write_all(&0usize.to_le_bytes()).unwrap();
            }
            LabelState::Found { ip } => {
                bytecode.write_all(&ip.to_le_bytes()).unwrap();
            }
        };

        Ok(())
    }

    pub fn finish(&self) -> Result<()> {
        for (label, state) in self.labels.iter() {
            match state {
                LabelState::Missing { .. } => {
                    return Err(Error::UnresolvedSymbol(label.to_string()))
                }
                LabelState::Found { .. } => {}
            }
        }

        Ok(())
    }
}

//

enum LabelState {
    Missing { needs_ip: Vec<usize> },
    Found { ip: usize },
}

//

pub fn assemble(assembly: &str) -> Result<Vec<u8>> {
    let mut bytecode = Vec::new();

    BytecodeHeader::encode(&mut bytecode)?;

    let mut resolver = Resolver::new();

    for line in assembly
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
    {
        // println!("parsing line `{line}`");
        if let Some(label) = line.strip_suffix(':') {
            // it is a label
            resolver.found_label(label, &mut bytecode)?;
            continue;
        }

        let mut line = line.split(' ');

        let op = line.next().expect("line should have an instruction");
        // println!("parsing op `{op}`");
        let op = Opcode::from_asm(op).expect("invalid asm instruction");

        bytecode.write_all(&[op.as_byte()]).unwrap();

        match op {
            Opcode::Invalid => panic!("invalid asm instruction"),
            Opcode::I32Const => {
                let operand = line
                    .next()
                    .unwrap_or_else(|| panic!("{} requires an operand", op.as_asm()))
                    .trim()
                    .parse::<i32>()
                    .expect("operand should be i32");

                bytecode.write_all(&operand.to_le_bytes()).unwrap();
            }
            Opcode::I32Load => {
                let operand = line
                    .next()
                    .unwrap_or_else(|| panic!("{} requires an operand", op.as_asm()))
                    .trim()
                    .parse::<u8>()
                    .expect("operand should be u8");

                bytecode.write_all(&operand.to_le_bytes()).unwrap();
            }
            Opcode::I32Store => {
                let operand = line
                    .next()
                    .unwrap_or_else(|| panic!("{} requires an operand", op.as_asm()))
                    .trim()
                    .parse::<u8>()
                    .expect("operand should be u8");

                bytecode.write_all(&operand.to_le_bytes()).unwrap();
            }
            Opcode::I32Add => {}
            Opcode::I32Print => {}
            Opcode::Goto | Opcode::Call => {
                let operand = line
                    .next()
                    .unwrap_or_else(|| panic!("{} requires an operand", op.as_asm()))
                    .trim();

                if let Some(label) = operand.strip_prefix(':') {
                    resolver.lazy_resolve(label, &mut bytecode)?;
                } else {
                    bytecode
                        .write_all(
                            &operand
                                .parse::<u64>()
                                .map_err(|_| Error::InvalidOperand)?
                                .to_le_bytes(),
                        )
                        .unwrap();
                }
            }
            Opcode::Return => {}
            Opcode::Exit => {
                let operand = line
                    .next()
                    .unwrap_or_else(|| panic!("{} requires an operand", op.as_asm()))
                    .parse::<i32>()
                    .expect("operand should be i32");

                bytecode.write_all(&operand.to_le_bytes()).unwrap();
            }
        }
    }

    resolver.finish()?;

    Ok(bytecode)
}

pub fn disassemble(bytecode: &mut dyn Read, assembly: &mut dyn Write) -> Result<()> {
    if !BytecodeHeader::decode(bytecode)?.is_compatible() {
        return Err(Error::Incompatible);
    }

    let mut bytes = bytecode.bytes();
    while let Some(opcode) = bytes.next() {
        let op = Opcode::from_byte(opcode?).expect("invalid opcode");

        assembly.write_all(op.as_asm().as_bytes())?;

        let mut next = || bytes.next().expect("unexpected EOI");

        match op {
            Opcode::Invalid => panic!("invalid opcode"),
            Opcode::I32Const => {
                let operand = [next()?, next()?, next()?, next()?];
                let operand = i32::from_le_bytes(operand);
                assembly.write_fmt(format_args!(" {operand}"))?;
            }
            Opcode::I32Load => {
                let operand = next()?;
                assembly.write_fmt(format_args!(" {operand}"))?;
            }
            Opcode::I32Store => {
                let operand = next()?;
                assembly.write_fmt(format_args!(" {operand}"))?;
            }
            Opcode::I32Add => {}
            Opcode::I32Print => {}
            Opcode::Goto | Opcode::Call => {
                let operand = [
                    next()?,
                    next()?,
                    next()?,
                    next()?,
                    next()?,
                    next()?,
                    next()?,
                    next()?,
                ];
                let operand = u64::from_le_bytes(operand);

                // TODO: rebuild labels
                assembly.write_fmt(format_args!(" {operand}"))?;
            }
            Opcode::Return => {}
            Opcode::Exit => {}
        }

        assembly.write_all(b"\n")?;
    }
    Ok(())
}

//

#[cfg(test)]
mod tests {

    use std::io::Cursor;

    use crate::{assemble, disassemble};

    #[test]
    fn assembler_test() {
        let asm = "i32const 3\ni32const 2\ni32add\ni32print\n";

        let bytecode = assemble(asm).unwrap();

        let mut disasm = Vec::new();
        disassemble(&mut Cursor::new(bytecode), &mut disasm).unwrap();

        let disasm = std::str::from_utf8(&disasm).unwrap();
        assert_eq!(asm, disasm);
    }
}
