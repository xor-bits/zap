use std::{
    collections::{hash_map::Entry, HashMap},
    mem::transmute,
    rc::Rc,
};

use inkwell as llvm;
use llvm::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::{Linkage, Module},
    values::{
        AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue,
    },
    AddressSpace, OptimizationLevel,
};
use parser::ast;

//

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Clone)]
pub enum Error {
    NoMainFn,
    InvalidMainFn,
    StaticRedefined(String),
    VariableNotFound(String),
}

//

pub struct CodeGen {
    ctx: Option<&'static Context>,
}

impl CodeGen {
    pub const fn new() -> Self {
        Self { ctx: None }
    }

    pub fn module(&mut self) -> ModuleGen {
        let ctx = *self.ctx.get_or_insert_with(context);

        let module = ctx.create_module("<run>");
        let builder = ctx.create_builder();

        let engine = module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();

        ModuleGen {
            ctx,
            module,
            builder,
            engine,
            statics: HashMap::new(),
            namespace: "<run>".to_string(),
            locals: Vec::new(),
        }
    }
}

//

pub struct ModuleGen {
    ctx: &'static Context,
    module: Module<'static>,
    builder: Builder<'static>,

    engine: ExecutionEngine<'static>,

    statics: HashMap<Rc<str>, Value>,
    namespace: String,
    locals: Vec<HashMap<Rc<str>, Value>>,
}

impl ModuleGen {
    pub fn add(&mut self, ast: &ast::Ast<ast::Root>) -> Result<()> {
        ast.emit_ir(self)?;
        Ok(())
    }

    pub fn add_extern(&mut self, name: &str, f: fn()) -> Result<()> {
        let wrapper = self.ctx.void_type().fn_type(&[], false);
        let wrapper_ptr = self.module.add_function(name, wrapper, None);

        match self.statics.entry(name.into()) {
            Entry::Occupied(_) => return Err(Error::StaticRedefined(name.into())),
            Entry::Vacant(entry) => entry.insert(Value::Func(FuncValue {
                data: None,
                fn_ptr: wrapper_ptr,
            })),
        };

        let entry = self.ctx.append_basic_block(wrapper_ptr, "entry");
        self.builder.position_at_end(entry);

        let ty_usize = self
            .ctx
            .ptr_sized_int_type(self.engine.get_target_data(), None);
        let ty_ptr = ty_usize.ptr_type(AddressSpace::default());
        let fn_ptr = self
            .builder
            .build_int_to_ptr(
                ty_usize.const_int(f as usize as _, false),
                ty_ptr,
                "wrapped-fn-ptr",
            )
            .unwrap();
        self.builder
            .build_indirect_call(wrapper, fn_ptr, &[], "call-fn-ptr")
            .unwrap();

        self.builder.build_return(None).unwrap();

        if !wrapper_ptr.verify(true) {
            eprintln!("LLVM IR:\n");
            self.module.print_to_stderr();
            panic!("invalid fn");
        }

        Ok(())
    }

    pub fn run(&mut self) -> Result<i32> {
        // eprintln!("LLVM IR:\n");
        // module.module.print_to_stderr();

        // FIXME: validate the main function signature
        let main_fn = unsafe {
            self.engine
                .get_function::<unsafe extern "C" fn() -> i32>("<run>::main")
        }
        .unwrap();

        Ok(unsafe { main_fn.call() })
    }
}

//

#[derive(Clone)]
enum Value {
    Func(FuncValue),
    // IntLit(i128), // int literal that could be any int type (i8, i32, usize, ...)
    I32(IntValue<'static>),
    Never,
    None,
}

#[derive(Clone)]
struct FuncValue {
    data: Option<Box<Value>>,
    fn_ptr: FunctionValue<'static>,
}

//

trait EmitIr {
    type Val;

    fn emit_ir(&self, gen: &mut ModuleGen) -> Result<Self::Val>;
}

impl<T: EmitIr> EmitIr for ast::Ast<T> {
    type Val = T::Val;

    fn emit_ir(&self, gen: &mut ModuleGen) -> Result<Self::Val> {
        self.inner.emit_ir(gen)
    }
}

impl EmitIr for ast::Root {
    type Val = ();

    fn emit_ir(&self, gen: &mut ModuleGen) -> Result<Self::Val> {
        // TODO: allow recursion + calling functions that are declared later in the code
        for item in self.inner.iter() {
            match item {
                ast::RootItem::Init(init) => init.emit_ir(gen)?,
                _ => todo!(), // ast::RootItem::Test(test) => test.emit_ir(gen)?,
            }
        }

        Ok(())
    }
}

impl EmitIr for ast::RootInit {
    type Val = ();

    fn emit_ir(&self, gen: &mut ModuleGen) -> Result<Self::Val> {
        assert_eq!(self.targets.iter().len(), self.exprs.iter().len());
        for (target, expr) in self.targets.iter().zip(self.exprs.iter()) {
            let var_name = target.path.ident.value.as_str();
            gen.namespace.push_str("::");
            gen.namespace.push_str(var_name);

            let v = match expr {
                ast::Expr::Func(f) => Value::Func(f.emit_ir(gen)?),
                expr => expr.eval()?.emit_ir(gen)?,
            };

            match gen.statics.entry(var_name.into()) {
                Entry::Occupied(_) => return Err(Error::StaticRedefined(var_name.into())),
                Entry::Vacant(entry) => entry.insert(v),
            };

            gen.namespace
                .truncate(gen.namespace.len() - 2 - var_name.len());
        }
        Ok(())
    }
}

impl EmitIr for ast::Func {
    type Val = FuncValue;

    fn emit_ir(&self, gen: &mut ModuleGen) -> Result<Self::Val> {
        // TODO: generic functions are generated lazily

        let mut param_types = Vec::new();
        for arg in self.args.iter().flat_map(|a| a.iter()) {
            if arg.ty.value != "i32" {
                todo!()
            }

            param_types.push(gen.ctx.i32_type().into());
        }

        if Some("i32") != self.return_ty.as_ref().map(|(_, ty)| ty.value.as_str()) {
            todo!()
        }

        let fn_ty = gen.ctx.i32_type().fn_type(&param_types, false);
        let fn_val = gen.module.add_function(&gen.namespace, fn_ty, None);

        let entry = gen.ctx.append_basic_block(fn_val, "entry");
        gen.builder.position_at_end(entry);

        let mut locals = HashMap::new();
        for (param, arg) in fn_val
            .get_param_iter()
            .zip(self.args.iter().flat_map(|a| a.iter()))
        {
            if let BasicValueEnum::IntValue(int) = param {
                locals.insert(arg.id.value.as_str().into(), Value::I32(int));
            } else {
                todo!()
            }
        }

        gen.locals.push(locals);
        let val = self.block.emit_ir(gen)?;
        gen.locals.pop();

        match val {
            Value::Func(_) => todo!(),
            Value::I32(v) => _ = gen.builder.build_return(Some(&v)).unwrap(),
            Value::Never => {}
            Value::None => _ = gen.builder.build_return(None).unwrap(),
        };

        if !fn_val.verify(true) {
            eprintln!("LLVM IR:\n");
            gen.module.print_to_stderr();
            panic!("invalid fn");
        }

        Ok(FuncValue {
            data: None,
            fn_ptr: fn_val,
        })
    }
}

impl EmitIr for ast::Block {
    type Val = Value;

    fn emit_ir(&self, gen: &mut ModuleGen) -> Result<Self::Val> {
        let mut last = None;
        for stmt in self.stmts.iter() {
            match stmt.emit_ir(gen)? {
                Value::Never => return Ok(Value::Never),
                v => last = Some(v),
            };
        }

        if !self.auto_return {
            return Ok(Value::None);
        }

        Ok(last.unwrap_or(Value::None))
    }
}

impl EmitIr for ast::Stmt {
    type Val = Value;

    fn emit_ir(&self, gen: &mut ModuleGen) -> Result<Self::Val> {
        match self {
            ast::Stmt::Init(init) => {
                init.emit_ir(gen)?;
                Ok(Value::None)
            }
            ast::Stmt::Expr(expr) => expr.expr.emit_ir(gen),
            ast::Stmt::Return(expr) => {
                let expr = expr.expr.emit_ir(gen)?;
                match expr {
                    Value::Func(_) => todo!(),
                    Value::I32(v) => _ = gen.builder.build_return(Some(&v)).unwrap(),
                    Value::Never => {}
                    Value::None => _ = gen.builder.build_return(None).unwrap(),
                };
                Ok(Value::Never)
            }
        }
    }
}

impl EmitIr for ast::Init {
    type Val = ();

    fn emit_ir(&self, gen: &mut ModuleGen) -> Result<Self::Val> {
        assert_eq!(self.targets.iter().len(), self.exprs.iter().len());
        for (target, expr) in self.targets.iter().zip(self.exprs.iter()) {
            let var_name = target.path.ident.value.as_str();
            gen.namespace.push_str("::");
            gen.namespace.push_str(var_name);

            let v = expr.emit_ir(gen)?;

            // shadow the old var
            _ = gen.statics.insert(var_name.into(), v);

            gen.namespace
                .truncate(gen.namespace.len() - 2 - var_name.len());
        }
        Ok(())
    }
}

impl EmitIr for ast::Expr {
    type Val = Value;

    fn emit_ir(&self, gen: &mut ModuleGen) -> Result<Self::Val> {
        match self {
            ast::Expr::Block(_) => todo!(),
            ast::Expr::LitInt(v) => {
                let val = gen.ctx.i32_type().const_int(v.value as u64, false);
                Ok(Value::I32(val))
            }
            ast::Expr::LitStr(_) => todo!(),
            ast::Expr::Load(var) => {
                if let Some(locals) = gen.locals.last() {
                    if let Some(val) = locals.get(var.value.as_str()).cloned() {
                        return Ok(val);
                    }
                }

                if let Some(val) = gen.statics.get(var.value.as_str()).cloned() {
                    return Ok(val);
                }

                Err(Error::VariableNotFound(var.value.clone()))
            }
            ast::Expr::Func(_) => todo!(),
            ast::Expr::Add(add) => {
                let lhs = add.0.emit_ir(gen)?;
                let rhs = add.1.emit_ir(gen)?;

                match (lhs, rhs) {
                    (Value::I32(lhs), Value::I32(rhs)) => {
                        let val = gen.builder.build_int_add(lhs, rhs, "tmp-int-add").unwrap();
                        Ok(Value::I32(val))
                    }
                    _ => {
                        todo!()
                    }
                }
            }
            ast::Expr::Sub(_) => todo!(),
            ast::Expr::Mul(_) => todo!(),
            ast::Expr::Div(_) => todo!(),
            ast::Expr::Call(call) => {
                let func = call.func.emit_ir(gen)?;
                let func = match func {
                    Value::Func(f) => f,
                    Value::I32(_) => todo!(),
                    Value::Never => todo!(),
                    Value::None => todo!(),
                };

                let mut args = Vec::new();
                for arg in call.args.iter().flat_map(|a| a.iter()) {
                    let arg = match arg.emit_ir(gen)? {
                        Value::Func(_) => todo!(),
                        Value::I32(v) => v.into(),
                        Value::Never => todo!(),
                        Value::None => todo!(),
                    };
                    args.push(arg);
                }
                let val = gen
                    .builder
                    .build_direct_call(func.fn_ptr, &args, "tmp-call")
                    .unwrap();

                match val.try_as_basic_value().left() {
                    Some(BasicValueEnum::IntValue(v)) => Ok(Value::I32(v)),
                    None => Ok(Value::None),
                    _ => todo!(),
                }
            }
        }
    }
}

//

pub enum ConstValue {
    // TODO: LitInt(i128)
    I32(i32),
    Never,
    None,
}

impl EmitIr for ConstValue {
    type Val = Value;

    fn emit_ir(&self, gen: &mut ModuleGen) -> Result<Self::Val> {
        Ok(match self {
            ConstValue::I32(i) => Value::I32(gen.ctx.i32_type().const_int(*i as _, false)),
            ConstValue::Never => Value::Never,
            ConstValue::None => Value::None,
        })
    }
}

//

pub trait ConstEval {
    type Val;

    fn eval(&self) -> Result<Self::Val>;
}

impl ConstEval for ast::Expr {
    type Val = ConstValue;

    fn eval(&self) -> Result<Self::Val> {
        match self {
            ast::Expr::Block(block) => block.eval(),
            ast::Expr::LitInt(i) => Ok(ConstValue::I32(i.value as _)),
            ast::Expr::LitStr(_) => todo!(),
            ast::Expr::Load(_) => todo!(),
            ast::Expr::Func(_) => todo!(),
            ast::Expr::Add(sides) => match sides.eval()? {
                (ConstValue::I32(lhs), ConstValue::I32(rhs)) => {
                    Ok(ConstValue::I32(lhs.wrapping_add(rhs)))
                }
                _ => todo!(),
            },
            ast::Expr::Sub(_) => todo!(),
            ast::Expr::Mul(sides) => match sides.eval()? {
                (ConstValue::I32(lhs), ConstValue::I32(rhs)) => {
                    Ok(ConstValue::I32(lhs.wrapping_mul(rhs)))
                }
                _ => todo!(),
            },
            ast::Expr::Div(_) => todo!(),
            ast::Expr::Call(_) => todo!(),
        }
    }
}

impl ConstEval for ast::Block {
    type Val = ConstValue;

    fn eval(&self) -> Result<Self::Val> {
        let mut last = None;
        for stmt in self.stmts.iter() {
            match stmt.eval()? {
                ConstValue::Never => return Ok(ConstValue::Never),
                v => last = Some(v),
            };
        }

        if !self.auto_return {
            return Ok(ConstValue::None);
        }

        Ok(last.unwrap_or(ConstValue::None))
    }
}

impl ConstEval for ast::Stmt {
    type Val = ConstValue;

    fn eval(&self) -> Result<Self::Val> {
        match self {
            ast::Stmt::Init(_) => todo!(),
            ast::Stmt::Expr(expr) => expr.expr.eval(),
            ast::Stmt::Return(_) => todo!(),
        }
    }
}

impl<L: ConstEval, R: ConstEval> ConstEval for (L, R) {
    type Val = (L::Val, R::Val);

    fn eval(&self) -> Result<Self::Val> {
        Ok((self.0.eval()?, self.1.eval()?))
    }
}

//

fn context() -> &'static Context {
    thread_local! {
        static CTX: &'static Context = Box::leak(Box::new(Context::create()));
    }

    CTX.with(|c| *c)
}
