use codegen::{CodeGen, FnAsLlvm, ModuleGen};
use lexer::Lexer;
use parser::ast::{Ast, Root};

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

impl Compiler {
    pub const fn new() -> Self {
        Self {
            codegen: CodeGen::new(),
            module: None,
        }
    }

    pub fn add_raw_extern<F: FnAsLlvm>(&mut self, name: &str, f: F) -> Result<()> {
        let module = self.module.get_or_insert_with(|| self.codegen.module());

        module.add_extern(name, f)?;

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
