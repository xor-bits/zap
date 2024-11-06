use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
    rc::Rc,
};

use lexer::Span;
use parser::ast::{
    self, AnyExpr, Ast, BinaryOp, Call, Cond, Expr, Func, Init, Loop, Return, Root, Set, Stmt, Test,
};

//

#[derive(Debug, Clone)]
pub enum Error {
    VariableNotFound(String),
    NotCallable,
    InvalidType,
    UnexpectedType {
        span: Span,
        expected: LinkedType,
        got: LinkedType,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::VariableNotFound(v) => write!(f, "variable not found: {v}"),
            Error::NotCallable => write!(f, "variable is not a function"),
            Error::InvalidType => write!(f, "type mismatch"),
            Error::UnexpectedType { .. } => {
                write!(f, "unexpected type")
            }
        }
    }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

//

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LinkedType(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TmpId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FuncId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LabelId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub usize);

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
        self.functions.push(Function::new_extern(
            self.types.create_known(ret),
            params
                .iter()
                .map(|ty| self.types.create_known(*ty))
                .collect(),
            true,
        ));
        fn_id
    }

    pub fn process(&mut self, ast: &Ast<Root>) -> Result<FuncId> {
        let mut func = Function::new(self.types.create_known(Type::Void), [].into());

        ast.inner.process(self, &mut func)?;

        func.terminate();

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

            if func.is_extern {
                println!(" - is_extern");
                continue;
            }

            println!(" - blocks: ");
            for (block_id, block) in func.blocks() {
                println!("   - Block{}: ", block_id.0);
                println!("   - stmts: ");
                for stmt in block.stmts.iter() {
                    match stmt {
                        Statement::Let { dst, src } => {
                            print!("     - let {}: ", dst.0);
                            self.print_linked_type(func.variables[dst.0]);
                            print!(" = %{}", src.0);
                        }
                        Statement::Store { dst, src } => {
                            print!("     - {}: ", dst.0);
                            self.print_linked_type(func.variables[dst.0]);
                            print!(" = %{}", src.0);
                        }
                        Statement::Load { dst, src } => {
                            print!("     - %{}: ", dst.0);
                            self.print_linked_type(func.temporaries[dst.0]);
                            print!(" = {}", src.0);
                        }
                        Statement::Extern { dst, src: _, name } => {
                            print!("     - %{}: ", dst.0);
                            self.print_linked_type(func.temporaries[dst.0]);
                            print!(" = {name}");
                        }
                        Statement::Func { dst, src } => {
                            print!("     - %{}: ", dst.0);
                            self.print_linked_type(func.temporaries[dst.0]);
                            print!(" = {:?}", src);
                        }
                        Statement::Const { dst, src } => {
                            print!("     - %{}: ", dst.0);
                            self.print_linked_type(func.temporaries[dst.0]);
                            print!(" = {:?}", src);
                        }
                        Statement::BinExpr { dst, lhs, op, rhs } => {
                            print!("     - %{}: ", dst.0);
                            self.print_linked_type(func.temporaries[dst.0]);
                            print!(" = %{} {op} %{}", lhs.0, rhs.0);
                        }
                        Statement::Call { dst, func: f, args } => {
                            print!("     - %{}: ", dst.0);
                            self.print_linked_type(func.temporaries[dst.0]);
                            print!(" = call %{} %({:?})", f.0, args);
                        }
                        Statement::Return { src } => {
                            print!("     - return %{}: ", src.0);
                            self.print_linked_type(func.temporaries[src.0]);
                        }
                        Statement::ReturnVoid => {
                            print!("     - return void");
                        }
                        Statement::UnconditionalJump { id } => {
                            print!("     - jump {}", id.0);
                        }
                        Statement::ConditionalJump {
                            bool,
                            then_block,
                            else_block,
                        } => {
                            print!(
                                "     - if %{} then jump {} else jump {}",
                                bool.0, then_block.0, else_block.0
                            );
                        }
                    }

                    println!();
                }
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
        for item in self.stmts.iter() {
            item.process(module, function)?;
        }
        Ok(())
    }
}

impl Process for Init {
    type Return = ();

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        let mut expr_results = Vec::with_capacity(self.exprs.iter().len());
        for expr in self.exprs.iter() {
            expr_results.push(expr.process(module, function)?);
        }

        for (target, src) in self.targets.iter().zip(expr_results) {
            let dst = function.new_varid(function.temporaries[src.0]);

            // shadow old variables
            function
                .variables_raw
                .insert(target.path.ident.value.as_str().into(), dst);

            function.push_stmt(Statement::Let { dst, src });
        }

        Ok(())
    }
}

impl Process for Test {
    type Return = ();

    fn process(&self, _module: &mut Module, _function: &mut Function) -> Result<Self::Return> {
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

                function.push_stmt(Statement::Const {
                    dst,
                    src: Literal::I32(int.value as _),
                });

                Ok(dst)
            }
            AnyExpr::LitStr(str) => {
                let dst = function.new_tmpid(module.types.create_known(Type::Str));

                function.push_stmt(Statement::Const {
                    dst,
                    src: Literal::Str(str.value.as_str().into()),
                });

                Ok(dst)
            }
            AnyExpr::Load(var) => {
                if let Some(src) = function.variables_raw.get(var.value.as_str()).copied() {
                    let dst = function.new_tmpid(function.variables[src.0]);
                    function.push_stmt(Statement::Load { dst, src });
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

                function.push_stmt(Statement::Extern { dst, name, src });
                Ok(dst)
            }
            AnyExpr::Func(func) => func.process(module, function),
            AnyExpr::Call(call) => call.process(module, function),
            AnyExpr::Binary { op, sides } => {
                let lhs = sides.0.process(module, function)?;
                let rhs = sides.1.process(module, function)?;

                let ty = match op {
                    BinaryOp::Lt
                    | BinaryOp::Le
                    | BinaryOp::Gt
                    | BinaryOp::Ge
                    | BinaryOp::Eq
                    | BinaryOp::Neq
                    | BinaryOp::And
                    | BinaryOp::Or => module.types.create_known(Type::Bool),
                    _ => function.temporaries[lhs.0],
                };

                let dst = function.new_tmpid(ty);
                let op = *op;

                function.push_stmt(Statement::BinExpr { dst, lhs, op, rhs });

                Ok(dst)
            }
        }
    }
}

impl Process for ast::Block {
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

        let mut func = Function::new(
            module.types.create_known(ret_ty),
            (0..self.proto.args.iter().len())
                .map(|_| module.types.create())
                .collect(),
        );

        self.block.process(module, &mut func)?;

        func.terminate();

        let func_id = FuncId(module.functions.len());
        module.functions.push(func);

        let dst = function.new_tmpid(module.types.create_known(Type::Func(func_id)));
        function.push_stmt(Statement::Func { dst, src: func_id });

        Ok(dst)
    }
}

impl Process for Call {
    type Return = TmpId;

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        let func = self.func.process(module, function)?;
        let Type::Func(func_id) = module.types.type_links[function.temporaries[func.0].0] else {
            return Err(Error::NotCallable);
        };

        let args = self
            .args()
            .map(|expr| expr.process(module, function))
            .collect::<Result<Box<[_]>>>()?;

        let func_ref = &module.functions[func_id.0];
        let dst = function.new_tmpid(func_ref.returns);

        for ((arg, span), param_lty) in args
            .iter()
            .zip(self.args().map(|e| e.span()))
            .zip(func_ref.params.iter().copied())
        {
            let arg_lty = function.temporaries[arg.0];
            let arg_ty = module.types.type_links[arg_lty.0];
            let param_ty = module.types.type_links[param_lty.0];

            if arg_ty != param_ty {
                return Err(Error::UnexpectedType {
                    span,
                    expected: param_lty,
                    got: arg_lty,
                });
            }
        }

        function.push_stmt(Statement::Call { dst, func, args });

        Ok(dst)
    }
}

impl Process for Stmt {
    type Return = Option<TmpId>;

    fn process(
        &self,
        module: &mut Module,
        function: &mut Function,
    ) -> Result<<Self as Process>::Return> {
        match self {
            Stmt::Init(v) => {
                v.process(module, function)?;
                Ok(None)
            }
            Stmt::Set(v) => {
                v.process(module, function)?;
                Ok(None)
            }
            Stmt::Cond(v) => {
                v.process(module, function)?;
                Ok(None)
            }
            Stmt::Loop(v) => {
                v.process(module, function)?;
                Ok(None)
            }
            Stmt::Expr(v) => Ok(Some(v.expr.process(module, function)?)),
            Stmt::Return(v) => {
                v.process(module, function)?;
                Ok(None)
            }
        }
    }
}

impl Process for Set {
    type Return = ();

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        let mut expr_results = Vec::with_capacity(self.exprs.iter().len());
        for expr in self.exprs.iter() {
            expr_results.push(expr.process(module, function)?);
        }

        for (target, src) in self.targets.iter().zip(expr_results) {
            let dst = *function
                .variables_raw
                .get(target.path.ident.value.as_str())
                .ok_or_else(|| Error::VariableNotFound(target.path.ident.value.clone()))?;

            // let src_ty_link = function.tmp(src);
            // let dst_ty_link = function.var(dst);
            // let src_ty = module.get_type(src_ty_link);
            // let dst_ty = module.get_type(dst_ty_link);

            // match (src_ty, dst_ty) {
            //     (Type::Unknown, Type::Unknown) => {}
            //     // (Type::Unknown, _) => {
            //     //     module.types.type_links[];
            //     // }
            //     _ => {}
            // }

            function.push_stmt(Statement::Store { dst, src });
        }

        Ok(())
    }
}

impl Process for Cond {
    type Return = ();

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        // if a {} else if b {} else if c {} else {}
        // becomes
        // if a {} else { if b {} else { if c {} else {} } }

        let continue_block = function.push_block();

        // if/else if chain
        for i in [&self.if_first]
            .into_iter()
            .chain(self.else_ifs.iter().map(|s| &s.inner))
        {
            let then_block = function.push_block();
            let else_block = function.push_block();

            // TODO: check the type and narrow down the LinkedType
            let bool = i.check.process(module, function)?;

            function.push_stmt(Statement::ConditionalJump {
                bool,
                then_block,
                else_block,
            });

            // if true block
            function.move_to_block(then_block);
            i.block.process(module, function)?;
            function.terminate_with(Statement::UnconditionalJump { id: continue_block });

            // if false block
            function.move_to_block(else_block);
        }

        // final else block
        if let Some(else_last) = self.else_last.as_ref() {
            else_last.block.process(module, function)?;
        }
        function.terminate_with(Statement::UnconditionalJump { id: continue_block });

        // continue block
        function.move_to_block(continue_block);

        Ok(())
    }
}

impl Process for Loop {
    type Return = ();

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        let id = function.push_block();
        function.push_stmt(Statement::UnconditionalJump { id });
        function.move_to_block(id);

        for stmt in self.block.stmts.iter() {
            stmt.process(module, function)?;
        }

        function.push_stmt(Statement::UnconditionalJump { id });

        Ok(())
    }
}

impl Process for Return {
    type Return = ();

    fn process(&self, module: &mut Module, function: &mut Function) -> Result<Self::Return> {
        if let Some(expr) = self.expr.as_ref() {
            let src = expr.process(module, function)?;
            function.push_stmt(Statement::Return { src });
        } else {
            function.push_stmt(Statement::ReturnVoid);
        };

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
pub struct Block {
    pub stmts: Vec<Statement>,
}

//

#[derive(Debug)]
pub struct Function {
    pub returns: LinkedType,
    pub params: Box<[LinkedType]>,
    pub blocks: Vec<Block>,
    pub is_extern: bool,

    pub current_block: BlockId,

    pub variables: Vec<LinkedType>,
    variables_raw: HashMap<Rc<str>, VarId>,
    pub temporaries: Vec<LinkedType>,
}

impl Function {
    pub fn new(returns: LinkedType, params: Box<[LinkedType]>) -> Self {
        Self::new_extern(returns, params, false)
    }

    pub fn new_extern(returns: LinkedType, params: Box<[LinkedType]>, is_extern: bool) -> Self {
        Self {
            returns,
            params,
            blocks: Vec::new(),
            is_extern,

            current_block: BlockId(0),

            variables: Vec::new(),
            variables_raw: HashMap::new(),
            temporaries: Vec::new(),
        }
    }

    pub fn current(&self) -> Option<&Statement> {
        self.blocks
            .get(self.current_block.0)
            .and_then(|b| b.stmts.last())
    }

    pub fn push_stmt(&mut self, stmt: Statement) {
        if self.blocks.is_empty() {
            self.push_block();
        }

        self.blocks[self.current_block.0].stmts.push(stmt);
    }

    pub fn push_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(Block { stmts: Vec::new() });
        id
    }

    pub fn move_to_block(&mut self, id: BlockId) {
        self.current_block = id;
    }

    pub fn terminate(&mut self) {
        self.terminate_with(Statement::ReturnVoid);
    }

    pub fn terminate_with(&mut self, terminal_stmt: Statement) {
        // println!("current: {:?}", self.current());
        if self.current().map_or(false, |stmt| stmt.is_terminal()) {
            return;
        }

        self.push_stmt(terminal_stmt);
    }

    pub fn blocks(&self) -> impl DoubleEndedIterator<Item = (BlockId, &Block)> + ExactSizeIterator {
        self.blocks.iter().enumerate().map(|(i, b)| (BlockId(i), b))
    }

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
    ReturnVoid,
    UnconditionalJump {
        id: BlockId,
    },
    ConditionalJump {
        bool: TmpId,
        then_block: BlockId,
        else_block: BlockId,
    },
}

impl Statement {
    pub const fn is_terminal(&self) -> bool {
        matches!(
            self,
            Statement::Return { .. }
                | Statement::ReturnVoid
                | Statement::UnconditionalJump { .. }
                | Statement::ConditionalJump { .. }
        )
    }
}

//

#[derive(Debug)]
pub struct Types {
    type_links: Vec<Type>,
    known_type_links: Option<HashMap<Type, LinkedType>>,
}

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
