use std::{
    collections::{hash_map::Entry, BTreeSet, HashMap, HashSet},
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

use parser::ast::{AnyExpr, Ast, Block, Expr, Func, Init, Root, RootInit, RootItem, Stmt, Test};

use crate::Instruction;

//

pub struct Bytecode {
    code: Vec<u8>,
    const_pool: ConstPool,

    symbols: Option<HashMap<FnId, usize>>,
    next_fnid: AtomicUsize,

    function_stack: Vec<Function>,
}

impl Bytecode {
    pub const fn new() -> Self {
        Self {
            code: Vec::new(),
            const_pool: ConstPool::new(),
            symbols: None,
            next_fnid: AtomicUsize::new(1),
            function_stack: Vec::new(),
        }
    }

    pub fn init(&mut self) {
        if !self.code.is_empty() {
            return;
        }

        self.code.extend_from_slice(&[0xDE, 0xAF, 0xC0, 0xDE]);
        Instruction::Entry { addr: u64::MAX }.as_bytes(&mut self.code);
        Instruction::Exit.as_bytes(&mut self.code);
    }

    pub fn dump(&self) {
        println!("code:\n{:?}", self.code);
        if let Some(sym) = self.symbols.as_ref() {
            println!("symbols:\n{sym:?}");
        }

        let code = &self.code[4..];
        let mut ip = 0usize;
        loop {
            if ip == code.len() {
                break;
            }

            let ins = Instruction::from_bytes(&code[ip..]).expect("invalid opcode");
            println!(" - {ins:?}");
            ip += ins.size();
        }
    }

    pub fn code(&self) -> &[u8] {
        self.code.as_slice()
    }

    pub fn const_pool(&self) -> &ConstPool {
        &self.const_pool
    }

    pub fn addr(&self) -> usize {
        self.code.len() - 4
    }

    fn func(&mut self) -> &mut Function {
        self.function_stack.last_mut().unwrap()
    }

    fn next_fnid(&mut self) -> FnId {
        FnId(self.next_fnid.fetch_add(1, Ordering::Relaxed))
    }

    fn symbols(&mut self) -> &mut HashMap<FnId, usize> {
        self.symbols.get_or_insert_with(Default::default)
    }
}

//

pub struct ConstPool {
    i32_pool: Vec<u8>,                    // data
    i32_has: Option<HashMap<i32, usize>>, // metadata
}

impl ConstPool {
    pub const fn new() -> Self {
        Self {
            i32_pool: Vec::new(),
            i32_has: None,
        }
    }

    pub fn add_i32(&mut self, num: i32) -> Instruction {
        let idx = match self.i32_has.get_or_insert_with(Default::default).entry(num) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => *entry.insert(self.i32_pool.len() / 4),
        };

        self.i32_pool.extend_from_slice(&num.to_le_bytes()[..]);

        let idx: u8 = idx.try_into().expect("TODO: constant pool too big");

        Instruction::I32Load8 { idx }
    }

    pub fn get_i32(&self, idx: usize) -> Option<i32> {
        Some(i32::from_le_bytes(
            self.i32_pool.get(idx..idx + 4)?.try_into().ok()?,
        ))
    }
}

//

pub struct Function {
    variable_registers: HashMap<Box<str>, u8>,
    highest_register: u8,
    instructions: Vec<IncompleteInstruction>,
}

//

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnId(usize);

//

pub enum IncompleteInstruction {
    Instruction(Instruction),
    Goto(Symbol),
    Call(Symbol),
}

//

pub enum Symbol {
    Name(Rc<str>),
    This,
}

//

#[derive(Debug, Clone, Copy)]
pub enum Error {}

pub type Result<T, E = Error> = std::result::Result<T, E>;

//

pub trait Compile {
    fn compile(&self, bc: &mut Bytecode) -> Result<()>;
}

impl<T: Compile> Compile for Ast<T> {
    fn compile(&self, bc: &mut Bytecode) -> Result<()> {
        self.inner.compile(bc)
    }
}

impl Compile for Root {
    fn compile(&self, bc: &mut Bytecode) -> Result<()> {
        let addr = compile_functionlike(bc, |bc| {
            for item in self.inner.iter() {
                item.compile(bc)?;
            }
            Ok(())
        })?;

        // overwrite the entry

        let mut newcode = Vec::new();
        newcode.extend_from_slice(&[0xDE, 0xAF, 0xC0, 0xDE]);
        Instruction::Entry { addr: addr as _ }.as_bytes(&mut newcode);

        bc.code[..newcode.len()].copy_from_slice(&newcode);

        Ok(())
    }
}

fn compile_functionlike(
    bc: &mut Bytecode,
    f: impl FnOnce(&mut Bytecode) -> Result<()>,
) -> Result<usize> {
    println!("builder ip = {}", bc.addr());

    bc.function_stack.push(Function {
        variable_registers: HashMap::new(),
        instructions: Vec::new(),
        highest_register: 0,
    });

    f(bc)?;

    let f = bc.function_stack.pop().unwrap();

    let used_registers = f
        .variable_registers
        .values()
        .copied()
        .collect::<BTreeSet<u8>>();

    let self_addr = bc.addr();
    let self_fnid = bc.next_fnid();

    bc.symbols().insert(self_fnid, self_addr);

    // back up registers that get modified
    for reg in used_registers.iter().rev().copied() {
        Instruction::I32Push { reg }.as_bytes(&mut bc.code);
    }

    // finish compiling the function
    for instr in f.instructions {
        match instr {
            IncompleteInstruction::Instruction(i) => {
                i.as_bytes(&mut bc.code);
            }
            IncompleteInstruction::Goto(Symbol::Name(_)) => {
                todo!()
            }
            IncompleteInstruction::Call(Symbol::Name(_)) => {
                todo!()
            }
            IncompleteInstruction::Goto(Symbol::This) => {
                // FIXME: more relative jump widths
                Instruction::GotoRelI8 {
                    offs: (self_addr as isize - bc.addr() as isize) as i8,
                }
                .as_bytes(&mut bc.code);
            }
            IncompleteInstruction::Call(Symbol::This) => {
                // FIXME: more relative jump widths
                Instruction::CallRelI8 {
                    offs: (self_addr as isize - bc.addr() as isize) as i8,
                }
                .as_bytes(&mut bc.code);
            }
        }
    }

    // restore the registers (obv. in reverse order) and then return
    for reg in used_registers {
        Instruction::I32Pop { reg }.as_bytes(&mut bc.code);
    }

    Instruction::Return.as_bytes(&mut bc.code);

    Ok(self_addr)
}

impl Compile for RootItem {
    fn compile(&self, bc: &mut Bytecode) -> Result<()> {
        match self {
            RootItem::Init(v) => v.compile(bc),
            RootItem::Test(v) => v.compile(bc),
        }
    }
}

impl Compile for RootInit {
    fn compile(&self, bc: &mut Bytecode) -> Result<()> {
        todo!()
    }
}

impl Compile for Test {
    fn compile(&self, bc: &mut Bytecode) -> Result<()> {
        todo!()
    }
}

impl Compile for Init {
    fn compile(&self, bc: &mut Bytecode) -> Result<()> {
        for (target, expr) in self.targets.iter().zip(self.exprs.iter()) {
            // the result is on top of the stack
            expr.compile(bc)?;

            let last = bc.func();

            let reg = match last
                .variable_registers
                .entry(target.path.ident.value.as_str().into())
            {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    // allocate a new register for this variable
                    let new = last.highest_register;
                    last.highest_register += 1;
                    *entry.insert(new)
                }
            };

            last.instructions
                .push(IncompleteInstruction::Instruction(Instruction::I32Pop {
                    reg,
                }));
        }

        Ok(())
    }
}

impl Compile for Expr {
    fn compile(&self, bc: &mut Bytecode) -> Result<()> {
        match &self.expr {
            AnyExpr::Func(f) => f.compile(bc),
            // AnyExpr::Block(b) => todo!(),
            AnyExpr::LitInt(i) => {
                // FIXME: overflow
                bc.func()
                    .instructions
                    .push(IncompleteInstruction::Instruction(Instruction::I32Const {
                        val: i.value as i8,
                    }));
                Ok(())
            }
            // AnyExpr::LitStr(_) => todo!(),
            // AnyExpr::Load(_) => todo!(),
            // AnyExpr::Call(_) => todo!(),
            // AnyExpr::Binary { op, sides } => todo!(),
            _ => todo!("{self:?}"),
        }
    }
}

impl Compile for Func {
    fn compile(&self, bc: &mut Bytecode) -> Result<()> {
        let addr = compile_functionlike(bc, |bc| self.block.compile(bc))?;

        let num = bc
            .const_pool
            .add_i32(addr.try_into().expect("TODO: u64 const pool"));

        bc.func()
            .instructions
            .extend([IncompleteInstruction::Instruction(num)]);

        Ok(())
    }
}

impl Compile for Block {
    fn compile(&self, bc: &mut Bytecode) -> Result<()> {
        // let mut result = Value::Void;
        // let mut returns;

        // for stmt in self.stmts.iter() {
        //     (result, returns) = stmt.compile(bc)?;

        //     if returns {
        //         return result;
        //     }
        // }

        // if self.auto_return {
        //     result
        // } else {
        //     Value::Void
        // }
        // for stmt in self.stmts.iter() {}

        Ok(())
    }
}

impl Compile for Stmt {
    fn compile(&self, bc: &mut Bytecode) -> Result<()> {
        match self {
            Stmt::Init(_) => todo!(),
            Stmt::Set(_) => todo!(),
            Stmt::Cond(_) => todo!(),
            Stmt::Loop(_) => todo!(),
            Stmt::Expr(_) => todo!(),
            Stmt::Return(_) => todo!(),
        }
    }
}
