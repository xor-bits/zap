use std::sync::Mutex;

use codegen::{CodeGen, FnAsLlvm, ModuleGen};
use lexer::Lexer;
use parser::{
    ast::{Ast, Root},
    AsTypeId, TypeId,
};

//

pub use codegen::Str;

//

pub type Result<T, E = RunError> = std::result::Result<T, E>;

#[derive(Debug)]
pub enum RunError {
    Parse(parser::Error),
    Run(codegen::Error),
}

impl From<parser::Error> for RunError {
    fn from(value: parser::Error) -> Self {
        Self::Parse(value)
    }
}

impl From<codegen::Error> for RunError {
    fn from(value: codegen::Error) -> Self {
        Self::Run(value)
    }
}

//

pub struct Compiler {
    codegen: CodeGen,
    module: Option<ModuleGen>,
}

pub trait Func<A> {
    const RETURN: TypeId;
    const ARGS: &'static [TypeId];

    fn into_wrapper_ptr_and_id(self) -> (usize, usize);
}

macro_rules! gen_closure_wrapper {
    ($($arg_id:ident : $arg_t:ident),* $(,)?) => {
        impl<$($arg_t : AsTypeId,)* Ret: AsTypeId, Fun: Send + FnMut($($arg_t),*) -> Ret> Func<($($arg_t,)*)> for Fun {
            const RETURN: TypeId = Ret::TYPE_ID;
            const ARGS: &'static [TypeId] = &[$($arg_t::TYPE_ID),*];

            fn into_wrapper_ptr_and_id(self) -> (usize, usize) {
                static REG: Mutex<Vec<usize>> = Mutex::new(Vec::new());

                let wide_ptr = Box::new(self) as Box<dyn Send + FnMut($($arg_t),*) -> Ret>;
                let thin_ptr = Box::into_raw(Box::new(wide_ptr));
                let untyped_closure_ptr = thin_ptr as usize;

                let mut reg = REG.lock().unwrap();
                let index = reg.len();
                reg.push(untyped_closure_ptr);
                drop(reg);

                extern "C" fn wrapper<$($arg_t: AsTypeId,)* Ret: AsTypeId>(index: usize, $($arg_id: $arg_t),*) -> Ret {
                    let reg = REG.lock().unwrap();

                    let untyped_closure_ptr = reg[index];
                    let thin_ptr = untyped_closure_ptr as *mut Box<dyn Send + FnMut($($arg_t),*) -> Ret>;
                    let wide_ptr = unsafe { &mut **thin_ptr };

                    let res = (wide_ptr)($($arg_id),*);

                    drop(reg);

                    res
                }

                (wrapper::<$($arg_t,)* Ret> as _, index)
            }
        }
    };
}

gen_closure_wrapper! {}
gen_closure_wrapper! { a: A }
gen_closure_wrapper! { a: A, b: B }
gen_closure_wrapper! { a: A, b: B, c: C }
gen_closure_wrapper! { a: A, b: B, c: C, d: D }
gen_closure_wrapper! { a: A, b: B, c: C, d: D, e: E }
gen_closure_wrapper! { a: A, b: B, c: C, d: D, e: E, f: F }

impl Compiler {
    pub const fn new() -> Self {
        Self {
            codegen: CodeGen::new(),
            module: None,
        }
    }

    pub fn add<F, A>(&mut self, name: &str, f: F) -> Result<()>
    where
        F: Func<A>,
    {
        let (wrapper, index) = f.into_wrapper_ptr_and_id();
        unsafe {
            self.module
                .get_or_insert_with(|| self.codegen.module())
                .add_extern_userdata(name, wrapper, index, F::RETURN, F::ARGS.to_vec())?;
        }

        Ok(())
    }

    pub fn add_raw<F: FnAsLlvm>(&mut self, name: &str, f: F) -> Result<()> {
        self.module
            .get_or_insert_with(|| self.codegen.module())
            .add_extern(name, f)?;

        Ok(())
    }

    pub fn run(&mut self, code: &str) -> Result<i32> {
        let mut parser = parser::ParseStream::from_lexer(Lexer::new(code));
        let ast: Ast<Root> = parser.parse()?;

        let module = self.module.get_or_insert_with(|| self.codegen.module());

        // TODO: type checking before code gen

        module.add(ast).expect("code generation should not fail");

        let res = module.run()?;

        Ok(res)
    }
}
