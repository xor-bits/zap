use std::{mem::size_of, ops::DerefMut};

use bytecode::{BytecodeHeader, Opcode};

//

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Clone, Copy)]
pub enum Error {
    Incompatible,
    IpOutOfBounds,
    InvalidOpcode,
    StackUnderflow,
    StackOverflow,
    InvalidRegister,
}

//

pub struct VirtMachine {
    regs: Regs,
    stack: Option<Box<[u8]>>,
}

impl VirtMachine {
    pub const fn new() -> Self {
        Self {
            regs: Regs::new(),
            stack: None,
        }
    }

    pub fn run(&mut self, bytecode: &[u8]) -> Result<i32> {
        let stack = self
            .stack
            .get_or_insert_with(|| {
                // 65_536B stack
                (0..0x10000).map(|_| 0u8).collect()
            })
            .deref_mut();

        if !BytecodeHeader::from_bytes(
            bytecode
                .get(..24)
                .ok_or(Error::Incompatible)?
                .try_into()
                .unwrap(),
        )
        .is_compatible()
        {
            return Err(Error::Incompatible);
        }

        self.regs.reset(stack);
        loop {
            // println!("CPU: {:#?}", self.regs);
            // println!("code from ip: {:?}", &bytecode[self.regs.ip..]);
            let op = self.regs.next(bytecode)?;
            // println!("opcode: {}", op.as_asm());
            match op {
                Opcode::Invalid => return Err(Error::InvalidOpcode),
                Opcode::I32Const => {
                    let val: i32 = self.regs.pop_ip(bytecode)?;
                    self.regs.push_sp(val, stack)?;
                }
                Opcode::I32Load => {
                    let reg: u8 = self.regs.pop_ip(bytecode)?;
                    let reg_val = *self
                        .regs
                        .regs
                        .get(reg as usize)
                        .ok_or(Error::InvalidRegister)? as i32;
                    self.regs.push_sp(reg_val, stack)?;
                }
                Opcode::I32Store => {
                    let reg: u8 = self.regs.pop_ip(bytecode)?;
                    let reg_val: i32 = self.regs.pop_sp(stack)?;
                    *self
                        .regs
                        .regs
                        .get_mut(reg as usize)
                        .ok_or(Error::InvalidRegister)? = reg_val as u64;
                }
                Opcode::I32Add => {
                    let lhs: i32 = self.regs.pop_sp(stack)?;
                    let rhs: i32 = self.regs.pop_sp(stack)?;
                    self.regs.push_sp(lhs.wrapping_add(rhs), stack)?;
                }
                Opcode::I32Print => {
                    let var: i32 = self.regs.pop_sp(stack)?;
                    print!("{var}");
                }
                Opcode::Goto => {
                    let ip: u64 = self.regs.pop_ip(bytecode)?;
                    self.regs.ip = ip as _;
                }
                Opcode::Call => {
                    let ip: u64 = self.regs.pop_ip(bytecode)?;
                    self.regs.push_sp(self.regs.ip as u64, stack)?;
                    self.regs.ip = ip as usize;
                }
                Opcode::Return => {
                    let ip: u64 = self.regs.pop_sp(stack)?;
                    self.regs.ip = ip as usize;
                }
                Opcode::Exit => {
                    let exit_code: i32 = self.regs.pop_ip(bytecode)?;
                    return Ok(exit_code);
                }
            }
        }
    }
}

//

#[derive(Debug)]
struct Regs {
    ip: usize,
    sp: usize,
    regs: [u64; 4],
}

impl Regs {
    pub const fn new() -> Self {
        Self {
            ip: 0,
            sp: 0,
            regs: [0; 4],
        }
    }

    fn reset(&mut self, stack: &[u8]) {
        self.sp = stack.len();
        self.ip = BytecodeHeader::SIZE;
        self.regs = [0; 4];
    }

    fn next(&mut self, bytecode: &[u8]) -> Result<Opcode> {
        self.pop_ip(bytecode)
    }

    fn pop_ip<T: Value>(&mut self, bytecode: &[u8]) -> Result<T> {
        let le_bytes = bytecode
            .get(self.ip..self.ip + T::SIZE)
            .ok_or(Error::IpOutOfBounds)?;
        self.ip += T::SIZE;
        Ok(T::pop(le_bytes))
    }

    fn push_sp<T: Value>(&mut self, v: T, stack: &mut [u8]) -> Result<()> {
        self.sp -= T::SIZE;
        let le_bytes = stack
            .get_mut(self.sp..self.sp + T::SIZE)
            .ok_or(Error::StackOverflow)?;
        v.push(le_bytes);
        Ok(())
    }

    fn pop_sp<T: Value>(&mut self, stack: &[u8]) -> Result<T> {
        let le_bytes = stack
            .get(self.sp..self.sp + T::SIZE)
            .ok_or(Error::StackUnderflow)?;
        self.sp += T::SIZE;
        Ok(T::pop(le_bytes))
    }

    fn mov_from_ip_to_sp(&mut self, n: usize, stack: &mut [u8], bytecode: &[u8]) -> Result<()> {
        if n == 0 {
            return Ok(());
        }

        self.sp -= n;
        let stack = stack
            .get_mut(self.sp..self.sp + n)
            .ok_or(Error::StackOverflow)?;
        let code = bytecode
            .get(self.ip..self.ip + n)
            .ok_or(Error::IpOutOfBounds)?;
        self.ip += n;

        stack.copy_from_slice(code);

        Ok(())
    }
}

//

pub trait Value {
    const SIZE: usize;

    // b has the length `Self::SIZE`
    fn pop(b: &[u8]) -> Self;

    fn push(self, b: &mut [u8]);
}

impl Value for Opcode {
    const SIZE: usize = size_of::<u8>();

    fn pop(b: &[u8]) -> Self {
        Self::from_byte(b[0]).unwrap_or(Self::Invalid)
    }

    fn push(self, b: &mut [u8]) {
        b[0] = self.as_byte();
    }
}

impl Value for i32 {
    const SIZE: usize = size_of::<Self>();

    fn pop(b: &[u8]) -> Self {
        Self::from_le_bytes(b.try_into().unwrap())
    }

    fn push(self, b: &mut [u8]) {
        b.copy_from_slice(&self.to_le_bytes())
    }
}

impl Value for u8 {
    const SIZE: usize = size_of::<Self>();

    fn pop(b: &[u8]) -> Self {
        b[0]
    }

    fn push(self, b: &mut [u8]) {
        b[0] = self;
    }
}

impl Value for u64 {
    const SIZE: usize = size_of::<Self>();

    fn pop(b: &[u8]) -> Self {
        Self::from_le_bytes(b.try_into().unwrap())
    }

    fn push(self, b: &mut [u8]) {
        b.copy_from_slice(&self.to_le_bytes())
    }
}

//

#[cfg(test)]
mod tests {
    use bytecode::assemble;

    use crate::VirtMachine;

    #[test]
    fn run_bytecode() {
        let asm = r#"
            i32const r0, 1
            i32const r0, 2
            call :sum_2
            i32load 0
            i32print
            exit 0

            sum_2:
            i32load 0
            i32load 1
            i32add
            i32store 0
            return
        "#;

        let bytecode = assemble(asm).unwrap();

        let mut vm = VirtMachine::new();

        vm.run(&bytecode).unwrap();

        panic!();
    }
}
