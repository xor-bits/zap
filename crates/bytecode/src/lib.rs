#![feature(inline_const_pat)]

//

use lexer::Lexer;
use parser::{
    ast::{Ast, Root},
    ParseStream, Result,
};

#[cfg(test)]
use serde::Serialize;

use self::compiler::{Bytecode, Compile};

//

mod compiler;

//

pub struct Runtime {
    bytecode: Bytecode,

    fuel: usize,
    fuel_enabled: bool,

    ip: usize,
    stack: Vec<u8>,
    regs: Vec<Value>,
    call_stack: Vec<usize>,
}

impl Runtime {
    pub const fn new() -> Self {
        Self {
            bytecode: Bytecode::new(),

            fuel: 0,
            fuel_enabled: false,

            ip: 0,
            stack: Vec::new(),
            regs: Vec::new(),
            call_stack: Vec::new(),
        }
    }

    pub fn add_fuel(&mut self, added_fuel: usize) {
        self.fuel = self.fuel.saturating_add(added_fuel);
    }

    pub fn enable_fuel(&mut self) {
        self.fuel_enabled = true;
    }

    pub fn disable_fuel(&mut self) {
        self.fuel_enabled = false;
    }

    pub fn fuel_enabled(&self) -> bool {
        self.fuel_enabled
    }

    pub fn run(&mut self, code: &str) -> Result<()> {
        let mut parser = ParseStream::from_lexer(Lexer::new(code));
        let ast: Ast<Root> = parser.parse()?;

        self.bytecode.init();

        ast.compile(&mut self.bytecode).unwrap();
        self.bytecode.dump();

        self.exec();

        Ok(())
    }

    pub fn reset(&mut self) {
        self.ip = 0usize;
        self.stack.clear();
        self.regs.clear();
    }

    pub fn stack(&self) -> &[u8] {
        &self.stack[..]
    }

    pub fn registers(&self) -> &[Value] {
        &self.regs[..]
    }

    pub fn exec(&mut self) {
        let [0xDE, 0xAF, 0xC0, 0xDE, code @ ..] = self.bytecode.code() else {
            panic!("invalid bytecode");
        };

        loop {
            if self.fuel_enabled {
                if self.fuel == 0 {
                    panic!("out of fuel");
                }
                self.fuel -= 1;
            }

            println!("ip = {}", self.ip);
            if self.ip >= code.len() {
                panic!("ip out of bounds");
            }

            println!("opcode = {}", code[self.ip]);
            let ins = Instruction::from_bytes(&code[self.ip..]).expect("invalid opcode");
            println!("instr = {ins:?}");

            match ins {
                Instruction::Entry { addr } => {
                    self.call_stack.push(self.ip + ins.size());
                    self.ip = addr as usize;
                    continue;
                }
                Instruction::I32Const { val: small_const } => self
                    .stack
                    .extend_from_slice(&(small_const as i32).to_ne_bytes()[..]),
                Instruction::I32Load8 { idx } => {
                    let val = self
                        .bytecode
                        .const_pool()
                        .get_i32(idx as usize)
                        .expect("invalid constant");
                    Self::push_bytes(&mut self.stack, val.to_ne_bytes())
                }
                Instruction::I32Push { reg } => {
                    let val = if let Some(reg) = self.regs.get(reg as usize) {
                        *reg
                    } else {
                        self.regs.resize_with(reg as usize, || Value::Unknown);
                        Value::Unknown
                    };

                    let val = match val {
                        Value::I32(val) => val,
                        Value::Unknown => 0,
                        // _ => panic!("invalid register cast"),
                    };

                    Self::push_bytes(&mut self.stack, val.to_ne_bytes());
                }
                Instruction::I32Pop { reg } => {
                    let val = Self::pop_bytes(&mut self.stack);
                    let val = Value::I32(i32::from_ne_bytes(val));

                    if let Some(reg) = self.regs.get_mut(reg as usize) {
                        *reg = val;
                    } else {
                        self.regs.resize_with(reg as usize, || Value::Unknown);
                        self.regs.push(val);
                    }
                }
                Instruction::I32Add => {
                    let a = i32::from_ne_bytes(Self::pop_bytes(&mut self.stack));
                    let b = i32::from_ne_bytes(Self::pop_bytes(&mut self.stack));
                    let res = a.wrapping_add(b);

                    Self::push_bytes(&mut self.stack, i32::to_ne_bytes(res));
                }
                Instruction::I32Print => {
                    let i = i32::from_ne_bytes(Self::pop_bytes(&mut self.stack));
                    println!("{i}");
                }
                Instruction::I32Dup => {
                    let val: [u8; 4] = Self::pop_bytes(&mut self.stack);
                    Self::push_bytes(&mut self.stack, val);
                    Self::push_bytes(&mut self.stack, val);
                }
                Instruction::I32GotoAbs => {
                    let val = i32::from_ne_bytes(Self::pop_bytes(&mut self.stack));
                    self.ip = val as usize;
                }
                Instruction::I32CallAbs => {
                    self.call_stack.push(self.ip + ins.size());
                    let val = i32::from_ne_bytes(Self::pop_bytes(&mut self.stack));
                    self.ip = val as usize;
                }
                Instruction::GotoRelI8 { offs } => {
                    self.ip = self
                        .ip
                        .checked_add_signed(offs as isize)
                        .expect("ip out of bounds");
                    continue;
                }
                Instruction::CallRelI8 { offs } => {
                    self.call_stack.push(self.ip + ins.size());
                    self.ip = self
                        .ip
                        .checked_add_signed(offs as isize)
                        .expect("ip out of bounds");
                    continue;
                }
                Instruction::Return => {
                    self.ip = self.call_stack.pop().expect("call stack underflow");
                    continue;
                }
                Instruction::Exit => {
                    return;
                }
                Instruction::Debug => {
                    println!("ip={:#x} {:?} {:?}", self.ip, self.stack, self.regs);
                }
                Instruction::Step => {
                    self.ip += ins.size();
                    return;
                }
            }

            self.ip += ins.size();
        }
    }

    fn pop_bytes<const N: usize>(stack: &mut Vec<u8>) -> [u8; N] {
        let res = *stack.last_chunk().expect("stack underflow");
        stack.truncate(stack.len() - N);
        res
    }

    fn push_bytes<const N: usize>(stack: &mut Vec<u8>, bytes: [u8; N]) {
        stack.extend_from_slice(&bytes[..]);
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy)]
pub enum Value {
    I32(i32),
    Unknown,
}

//

macro_rules! impl_instructions {
    (
        $(#[$($attrs:tt)*])*
        pub enum $id:ident {
            $(
                $(#[$($variant_attrs:tt)*])*
                $variant_id:ident $( {$($field_id:ident : $field_type:ty,)*} )?,
            )*
        }
    ) => {
        $(#[$($attrs)*])*
        pub enum $id {
            $(
                $(#[$($variant_attrs)*])*
                $variant_id $( {$($field_id : $field_type),*} )?,
            )*
        }

        #[derive(Debug, Clone, Copy)]
        pub enum OpCode {
            $(
                $(#[$($variant_attrs)*])*
                $variant_id,
            )*
        }

        impl $id {
            pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
                Some(match bytes {
                    $(
                        [const { OpCode::$variant_id as usize as u8 }, _ops @ ..] => {
                            $(
                                let mut _ops: &[u8] = _ops;
                                $(
                                    let field;
                                    (field, _ops) = _ops.split_first_chunk().unwrap();
                                    let $field_id = <$field_type>::from_le_bytes(*field);
                                )*
                            )?
                            $id::$variant_id $( { $($field_id )* } )?
                        }
                    )*
                    _ => return None,
                })
            }

            pub const fn all() -> impl IntoIterator<Item = Self> {
                [
                    $(
                        $id::$variant_id $({$($field_id: 0,)*})?,
                    )*
                ]
            }

            pub const fn size(self) -> usize {
                match self {
                    $(
                        $id::$variant_id { .. } => 1 $($(+ std::mem::size_of::<$field_type>())*)?,
                    )*
                }
            }

            pub fn as_bytes(self, dst: &mut Vec<u8>) {
                match self {
                    $(
                        $id::$variant_id $( { $($field_id)* } )? => {
                            dst.reserve(self.size());
                            dst.push(OpCode::$variant_id as usize as u8);
                            $($(
                                dst.extend_from_slice(&$field_id.to_le_bytes()[..]);
                            )*)?
                        },
                    )*
                }
            }

            pub const fn as_opcode(self) -> OpCode {
                match self {
                    $(
                        Self::$variant_id { .. } => OpCode::$variant_id,
                    )*
                }
            }
        }
    };
}

impl_instructions! {

#[derive(Debug, Clone, Copy)]
// #[repr(u8)]
pub enum Instruction {
    /// call the entry point, the first instruction must be this
    Entry {
        addr: u64,
    },

    /// push a constant to stack
    I32Const {
        val: i8,
    },

    /// push a constant to stack from the i32 const pool
    I32Load8 {
        idx: u8,
    },

    /// push a local to stack
    I32Push {
        reg: u8,
    },

    /// pop stack to a local
    I32Pop {
        reg: u8,
    },

    /// pop two i32's and push the sum
    I32Add,

    /// pop a i32 and print it
    I32Print,

    /// duplicate the top of stack (i32)
    I32Dup,

    // /// pop a i32 and goto `i32 + ip`
    // I32GotoRel,

    // /// push the ip to the call stack, pop i32 and goto `i32 + ip`
    // I32CallRel,

    /// pop a i32 and `ip = i32`
    I32GotoAbs,

    /// push the ip to the call stack, pop i32 and `ip = i32`
    I32CallAbs,

    /// goto relative address
    GotoRelI8 {
        offs: i8,
    },

    /// push the ip to the call stack and goto relative address
    CallRelI8 {
        offs: i8,
    },

    /// pop a return address from the call stack and jump there
    Return,

    /// exit
    Exit,

    /// debug the program
    Debug,

    /// step the program
    Step,
}

}

//

#[cfg(test)]
mod tests {
    use insta::assert_yaml_snapshot;

    use crate::{Instruction, Runtime};

    #[test]
    fn fibonacci_series() {
        let instr = [
            // set X = 0 and Y = 1
            Instruction::I32Const { val: 0 },
            Instruction::I32Pop { reg: 0 },
            Instruction::I32Const { val: 1 },
            Instruction::I32Pop { reg: 1 },
            // print X
            Instruction::Step,
            // Instruction::I32Push { reg: 0 },
            // Instruction::I32Print,
            // tmp = X + Y
            Instruction::I32Push { reg: 0 },
            Instruction::I32Push { reg: 1 },
            Instruction::I32Add,
            // X = Y and Y = tmp
            Instruction::I32Push { reg: 1 },
            Instruction::I32Pop { reg: 0 },
            Instruction::I32Pop { reg: 1 },
            // loop
            Instruction::GotoRelI8 { offs: -12 }, // -15
        ];

        let mut runtime = Runtime::new();
        runtime.assemble(&instr[..]).unwrap();

        let mut results = Vec::new();

        for _ in 0..16 {
            runtime.exec();
            results.push(runtime.registers()[0]);
        }

        assert_yaml_snapshot!(results);
    }
}
