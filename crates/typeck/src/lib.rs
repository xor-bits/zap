use std::{
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use parser::ast::{
    AnyExpr, Ast, BinaryOp, Block, Call, Expr, Func, Ident, Init, Return, Root, RootItem, Stmt,
    Test,
};

//

#[derive(Debug, Clone)]
pub enum Error {
    VariableNotFound(String),
    NotCallable,
    InvalidType,
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

//

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TmpId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FuncId(pub usize);

//

#[derive(Debug)]
pub struct Module {
    types: Types,
    functions: Vec<Function>,
    externs: Option<HashMap<Rc<str>, FuncId>>,
}

impl Module {
    pub const fn new() -> Self {
        Self {
            types: Types {
                type_links: Vec::new(),
                known_type_links: None,
            },
            functions: Vec::new(),
            externs: None,
        }
    }

    pub fn get_type(&self, id: LinkedType) -> &Type {
        &self.types.type_links[id.0]
    }

    pub fn get_function(&self, id: FuncId) -> &Function {
        &self.functions[id.0]
    }

    pub fn functions(&self) -> &[Function] {
        &self.functions[..]
    }

    pub fn add_extern(&mut self, name: &str, ret: Type, params: &[Type]) -> FuncId {
        let fn_id = FuncId(self.functions.len());
        self.externs
            .get_or_insert_with(Default::default)
            .insert(name.into(), fn_id);
        self.functions.push(Function {
            returns: self.types.create_known(ret),
            params: params
                .iter()
                .map(|ty| self.types.create_known(*ty))
                .collect(),
            is_extern: true,
            stmts: Vec::new(),
            variables: Vec::new(),
            variables_raw: HashMap::new(),
            temporaries: Vec::new(),
        });
        fn_id
    }

    pub fn process(&mut self, ast: &Ast<Root>) -> Result<FuncId> {
        let mut func = Function {
            returns: self.types.create_known(Type::Void),
            params: [].into(),
            is_extern: false,
            stmts: Vec::new(),
            variables: Vec::new(),
            variables_raw: HashMap::new(),
            temporaries: Vec::new(),
        };

        ast.inner.process(self, &mut func)?;

        let main = FuncId(self.functions.len());
        self.functions.push(func);
        Ok(main)
    }

    pub fn dump(&self) {
        for (i, func) in self.functions.iter().enumerate() {
            println!("Function{i}");
            print!(" - return: ");
            self.print_linked_type(func.returns);
            println!();

            println!(" - params: ");
            for (i, param) in func.params.iter().enumerate() {
                print!("   - {i}: ");
                self.print_linked_type(*param);
                println!();
            }

            println!(" - stmts: ");
            for stmt in func.stmts.iter() {
                match stmt {
                    Statement::Let { dst, src } => {
                        print!("   - let {}: ", dst.0);
                        self.print_linked_type(func.variables[dst.0]);
                        print!(" = %{}", src.0);
                    }
                    Statement::Store { dst, src } => {
                        print!("   - {}: ", dst.0);
                        self.print_linked_type(func.variables[dst.0]);
                        print!(" = %{}", src.0);
                    }
                    Statement::Load { dst, src } => {
                        print!("   - %{}: ", dst.0);
                        self.print_linked_type(func.temporaries[dst.0]);
                        print!(" = {}", src.0);
                    }
                    Statement::Extern { dst, src: _, name } => {
                        print!("   - %{}: ", dst.0);
                        self.print_linked_type(func.temporaries[dst.0]);
                        print!(" = {name}");
                    }
                    Statement::Func { dst, src } => {
                        print!("   - %{}: ", dst.0);
                        self.print_linked_type(func.temporaries[dst.0]);
                        print!(" = {:?}", src);
                    }
                    Statement::Const { dst, src } => {
                        print!("   - %{}: ", dst.0);
                        self.print_linked_type(func.temporaries[dst.0]);
                        print!(" = {:?}", src);
                    }
                    Statement::BinExpr { dst, lhs, op, rhs } => {
                        print!("   - %{}: ", dst.0);
                        self.print_linked_type(func.temporaries[dst.0]);
                        print!(" = %{} {op} %{}", lhs.0, rhs.0);
                    }
                    Statement::Call { dst, func: f, args } => {
                        print!("   - %{}: ", dst.0);
                        self.print_linked_type(func.temporaries[dst.0]);
                        print!(" = call %{} %({:?})", f.0, args);
                    }
                    Statement::Return { src } => {
                        print!("   - return %{}: ", src.0);
                        self.print_linked_type(func.temporaries[src.0]);
                    }
                }

                println!();
            }
        }
    }

    fn print_linked_type(&self, ty: LinkedType) {
        print!("{:?}", self.types.type_links[ty.0]);
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

//

trait Process {
    type Return;

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return>;
}

impl Process for Root {
    type Return = ();

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        for item in self.inner.iter() {
            item.process(module, function)?;
        }
        Ok(())
    }
}

impl Process for RootItem {
    type Return = ();

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        match self {
            RootItem::Init(v) => v.process(module, function),
            RootItem::Test(v) => v.process(module, function),
        }
    }
}

impl Process for Init {
    type Return = ();

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        for (target, expr) in self.targets.iter().zip(self.exprs.iter()) {
            let src = expr.process(module, function)?;
            let dst = function.new_varid(function.temporaries[src.0]);

            // shadow old variables
            function
                .variables_raw
                .insert(target.path.ident.value.as_str().into(), dst);

            function.stmts.push(Statement::Let { dst, src });
        }

        Ok(())
    }
}

impl Process for Test {
    type Return = ();

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        todo!()
    }
}

impl Process for Expr {
    type Return = TmpId;

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        match &self.expr {
            AnyExpr::Block(block) => block.process(module, function),
            AnyExpr::LitInt(int) => {
                let dst = function.new_tmpid(module.types.create_known(Type::I32));

                function.stmts.push(Statement::Const {
                    dst,
                    src: Literal::I32(int.value as _),
                });

                Ok(dst)
            }
            AnyExpr::LitStr(str) => {
                let dst = function.new_tmpid(module.types.create_known(Type::Str));

                function.stmts.push(Statement::Const {
                    dst,
                    src: Literal::Str(str.value.as_str().into()),
                });

                Ok(dst)
            }
            AnyExpr::Load(var) => {
                if let Some(src) = function.variables_raw.get(var.value.as_str()).copied() {
                    let dst = function.new_tmpid(function.variables[src.0]);
                    function.stmts.push(Statement::Load { dst, src });
                    return Ok(dst);
                }

                let src = module
                    .externs
                    .as_ref()
                    .and_then(|map| map.get_key_value(var.value.as_str()))
                    .ok_or_else(|| Error::VariableNotFound(var.value.clone()))?;
                let name = src.0.clone();
                let src = *src.1;

                let dst = function.new_tmpid(module.types.create_known(Type::Func(src)));
                // let dst = function.new_tmpid(module.functions[src.0].returns);

                function.stmts.push(Statement::Extern { dst, name, src });
                Ok(dst)
            }
            AnyExpr::Func(func) => func.process(module, function),
            AnyExpr::Call(call) => call.process(module, function),
            AnyExpr::Binary { op, sides } => {
                let lhs = sides.0.process(module, function)?;
                let rhs = sides.1.process(module, function)?;
                let dst = function.new_tmpid(function.temporaries[lhs.0]);
                let op = *op;

                function
                    .stmts
                    .push(Statement::BinExpr { dst, lhs, op, rhs });

                Ok(dst)
            }
        }
    }
}

impl Process for Block {
    type Return = TmpId;

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        let mut last = None;

        for stmt in self.stmts.iter() {
            last = stmt.process(module, function)?;
        }

        if self.auto_return {
            if let Some(last) = last {
                return Ok(last);
            }
        };

        Ok(function.new_tmpid(module.types.create_known(Type::Void)))
    }
}

impl Process for Func {
    type Return = TmpId;

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        let ret_ty =
            type_hint(self.proto.return_ty.as_ref().map(|(_, i)| i.value.as_str()))?.unwrap();

        let mut func = Function {
            returns: module.types.create_known(ret_ty),
            params: (0..self.proto.args.iter().len())
                .map(|_| module.types.create())
                .collect(),
            is_extern: false,
            stmts: Vec::new(),
            variables: Vec::new(),
            variables_raw: HashMap::new(),
            temporaries: Vec::new(),
        };

        self.block.process(module, &mut func)?;

        let func_id = FuncId(module.functions.len());
        module.functions.push(func);

        let dst = function.new_tmpid(module.types.create_known(Type::Func(func_id)));
        function.stmts.push(Statement::Func { dst, src: func_id });

        Ok(dst)
    }
}

impl Process for Call {
    type Return = TmpId;

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        let args = self
            .args()
            .map(|expr| expr.process(module, function))
            .collect::<Result<Box<[_]>>>()?;

        let func = self.func.process(module, function)?;
        let Type::Func(func_id) = module.types.type_links[function.temporaries[func.0].0] else {
            return Err(Error::NotCallable);
        };

        let func_ref = &module.functions[func_id.0];
        let dst = function.new_tmpid(func_ref.returns);

        for (arg, param) in args.iter().zip(func_ref.params.iter()) {
            let arg_ty = module.types.type_links[function.temporaries[arg.0].0];
            let param_ty = module.types.type_links[param.0];

            if arg_ty != param_ty {
                return Err(Error::InvalidType);
            }
        }

        function.stmts.push(Statement::Call { dst, func, args });

        Ok(dst)
    }
}

impl Process for Stmt {
    type Return = Option<TmpId>;

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Option<TmpId>> {
        match self {
            Stmt::Init(v) => {
                v.process(module, function)?;
                Ok(None)
                // Ok(function.new_tmpid(types.create_known(Type::Void)))
            }
            Stmt::Set(_) => todo!(),
            Stmt::Cond(_) => todo!(),
            Stmt::Loop(_) => todo!(),
            Stmt::Expr(v) => Ok(Some(v.expr.process(module, function)?)),
            Stmt::Return(v) => {
                v.process(module, function)?;
                Ok(None)
            }
        }
    }
}

impl Process for Return {
    type Return = ();

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        let src = if let Some(expr) = self.expr.as_ref() {
            expr.process(module, function)?
        } else {
            function.new_tmpid(module.types.create_known(Type::Void))
        };

        function.stmts.push(Statement::Return { src });

        Ok(())
    }
}

//

fn type_hint(v: Option<&str>) -> Result<Option<Type>> {
    Ok(match v {
        Some("i32") => Some(Type::I32),
        Some("void") => Some(Type::Void),
        Some(_) => return Err(Error::InvalidType),
        None => Some(Type::Void),
    })
}

//

#[derive(Debug)]
pub struct Function {
    pub returns: LinkedType,
    pub params: Box<[LinkedType]>,
    pub stmts: Vec<Statement>,
    pub is_extern: bool,

    pub variables: Vec<LinkedType>,
    variables_raw: HashMap<Rc<str>, VarId>,
    pub temporaries: Vec<LinkedType>,
}

impl Function {
    pub fn var(&self, var: VarId) -> LinkedType {
        self.variables[var.0]
    }

    pub fn tmp(&self, tmp: TmpId) -> LinkedType {
        self.temporaries[tmp.0]
    }

    pub fn new_varid(&mut self, ty: LinkedType) -> VarId {
        let id = VarId(self.variables.len());
        self.variables.push(ty);
        id
    }

    pub fn new_tmpid(&mut self, ty: LinkedType) -> TmpId {
        let id = TmpId(self.temporaries.len());
        self.temporaries.push(ty);
        id
    }
}

//

#[derive(Debug)]
pub enum Statement {
    Let {
        dst: VarId,
        src: TmpId,
    },
    Store {
        dst: VarId,
        src: TmpId,
    },
    Load {
        dst: TmpId,
        src: VarId,
    },
    Extern {
        dst: TmpId,
        src: FuncId,
        name: Rc<str>,
    },
    Func {
        dst: TmpId,
        src: FuncId,
    },
    Const {
        dst: TmpId,
        src: Literal,
    },
    BinExpr {
        dst: TmpId,
        lhs: TmpId,
        op: BinaryOp,
        rhs: TmpId,
    },
    Call {
        dst: TmpId,
        func: TmpId,
        args: Box<[TmpId]>,
    },
    Return {
        src: TmpId,
    },
}

//

#[derive(Debug)]
pub struct Types {
    type_links: Vec<Type>,
    known_type_links: Option<HashMap<Type, LinkedType>>,
}

#[derive(Debug, Clone, Copy)]
pub struct LinkedType(usize);

impl Types {
    pub fn create(&mut self) -> LinkedType {
        self.create_new(Type::Unknown)
    }

    pub fn create_known(&mut self, ty: Type) -> LinkedType {
        if let Some(known) = self.known_type_links.as_ref().and_then(|map| map.get(&ty)) {
            return *known;
        }

        self.create_new(ty)
    }

    fn create_new(&mut self, ty: Type) -> LinkedType {
        let id = LinkedType(self.type_links.len());
        self.type_links.push(ty);
        match self
            .known_type_links
            .get_or_insert_with(Default::default)
            .entry(ty)
        {
            Entry::Vacant(entry) => _ = entry.insert(id),
            Entry::Occupied(_) => {}
        }
        id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Func(FuncId),
    Bool,
    I32,
    Str,
    Never,
    Void,
    Unknown,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Bool(bool),
    I32(i32),
    Str(Box<str>),
}
