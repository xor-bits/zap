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
    types::BasicType,
    values::{
        AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue,
    },
    AddressSpace, OptimizationLevel,
};
use parser::{ast, AsTypeId, TypeId};
use typeck::{Func, TypeCheck};

use self::types::{AsLlvm, FnAsLlvm, TypeAsLlvm};

//

mod types;

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
            types: typeck::Context::new(),
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

    types: typeck::Context,

    statics: HashMap<Rc<str>, Value>,
    namespace: String,
    locals: Vec<HashMap<Rc<str>, Value>>,
}

impl ModuleGen {
    pub fn add(&mut self, mut ast: ast::Ast<ast::Root>) -> Result<()> {
        ast.type_check(&mut self.types);
        ast.emit_ir(self)?;
        Ok(())
    }

    pub fn add_extern<F: FnAsLlvm>(&mut self, name: &str, f: F) -> Result<()> {
        let ret = f.return_type();
        let args = f.args().to_vec();

        let param_types: Vec<_> = args
            .iter()
            .filter_map(|a| a.as_llvm_meta(&self.types))
            .collect();
        let wrapper_ty = ret.as_llvm_fn(&self.types, &param_types, false);
        let wrapper_ptr = self.module.add_function(name, wrapper_ty, None);

        self.types.add_extern(name, Func { ret, args });

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
                ty_usize.const_int(f.as_extern_c_fn_ptr() as _, false),
                ty_ptr,
                "wrapped-fn-ptr",
            )
            .unwrap();

        let args: Vec<BasicMetadataValueEnum> = wrapper_ptr
            .get_param_iter()
            .map(|p| p.as_any_value_enum().try_into().unwrap())
            .collect();

        let val = self
            .builder
            .build_indirect_call(wrapper_ty, fn_ptr, &args, "call-fn-ptr")
            .unwrap();

        let val = match val.try_as_basic_value().left() {
            Some(BasicValueEnum::ArrayValue(v)) => todo!("{v}"),
            Some(BasicValueEnum::IntValue(v)) => Ok(Value::I32(v)),
            Some(BasicValueEnum::FloatValue(v)) => todo!("{v}"),
            Some(BasicValueEnum::PointerValue(v)) => todo!("{v}"),
            Some(BasicValueEnum::StructValue(v)) => todo!("{v}"),
            Some(BasicValueEnum::VectorValue(v)) => todo!("{v}"),
            None => Ok(Value::None),
        }?;

        match val {
            Value::Func(_) => todo!(),
            Value::I32(v) => _ = self.builder.build_return(Some(&v)).unwrap(),
            Value::Never => {}
            Value::None => _ = self.builder.build_return(None).unwrap(),
        };

        if !wrapper_ptr.verify(true) {
            eprintln!("LLVM IR:\n");
            self.module.print_to_stderr();
            panic!("invalid fn");
        }

        Ok(())
    }

    pub fn run(&mut self) -> Result<i32> {
        eprintln!("LLVM IR:\n");
        self.module.print_to_stderr();

        // FIXME: validate the main function signature
        let main_fn = unsafe {
            self.engine
                .get_function::<unsafe extern "C" fn() -> i32>("<run>::main")
        }
        .expect("main not found");

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

            let v = match &expr.expr {
                ast::AnyExpr::Func(f) => Value::Func(f.emit_ir(gen)?),
                _ => expr.eval()?.emit_ir(gen)?,
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

        let func_id = gen
            .types
            .get_type(self.ty)
            .as_func()
            .expect("a function should be a function");
        let func = gen.types.get_func(func_id);

        let param_types: Vec<_> = func
            .args
            .iter()
            .filter_map(|id| id.as_llvm_meta(&gen.types))
            .collect();

        let proto = func.ret.as_llvm_fn(&gen.types, &param_types, false);
        let fn_val = gen.module.add_function(&gen.namespace, proto, None);

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
        match &self.expr {
            ast::AnyExpr::Block(_) => todo!(),
            ast::AnyExpr::LitInt(v) => {
                let val = gen.ctx.i32_type().const_int(v.value as u64, false);
                Ok(Value::I32(val))
            }
            ast::AnyExpr::LitStr(_) => todo!(),
            ast::AnyExpr::Load(var) => {
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
            ast::AnyExpr::Func(_) => todo!(),
            ast::AnyExpr::Add(sides) => match sides.emit_ir(gen)? {
                (Value::I32(lhs), Value::I32(rhs)) => {
                    let val = gen.builder.build_int_add(lhs, rhs, "tmp").unwrap();
                    Ok(Value::I32(val))
                }
                _ => {
                    todo!()
                }
            },
            ast::AnyExpr::Sub(sides) => match sides.emit_ir(gen)? {
                (Value::I32(lhs), Value::I32(rhs)) => {
                    let val = gen.builder.build_int_sub(lhs, rhs, "tmp").unwrap();
                    Ok(Value::I32(val))
                }
                _ => {
                    todo!()
                }
            },
            ast::AnyExpr::Mul(sides) => match sides.emit_ir(gen)? {
                (Value::I32(lhs), Value::I32(rhs)) => {
                    let val = gen.builder.build_int_mul(lhs, rhs, "tmp").unwrap();
                    Ok(Value::I32(val))
                }
                _ => {
                    todo!()
                }
            },
            ast::AnyExpr::Div(sides) => match sides.emit_ir(gen)? {
                (Value::I32(lhs), Value::I32(rhs)) => {
                    let val = gen.builder.build_int_signed_div(lhs, rhs, "tmp").unwrap();
                    Ok(Value::I32(val))
                }
                _ => {
                    todo!()
                }
            },
            ast::AnyExpr::Call(call) => {
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
                    Some(BasicValueEnum::ArrayValue(v)) => todo!("{v}"),
                    Some(BasicValueEnum::IntValue(v)) => Ok(Value::I32(v)),
                    Some(BasicValueEnum::FloatValue(v)) => todo!("{v}"),
                    Some(BasicValueEnum::PointerValue(v)) => todo!("{v}"),
                    Some(BasicValueEnum::StructValue(v)) => todo!("{v}"),
                    Some(BasicValueEnum::VectorValue(v)) => todo!("{v}"),
                    None => Ok(Value::None),
                }
            }
        }
    }
}

impl<L: EmitIr, R: EmitIr> EmitIr for (L, R) {
    type Val = (L::Val, R::Val);

    fn emit_ir(&self, gen: &mut ModuleGen) -> Result<Self::Val> {
        Ok((self.0.emit_ir(gen)?, self.1.emit_ir(gen)?))
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
        match &self.expr {
            ast::AnyExpr::Block(block) => block.eval(),
            ast::AnyExpr::LitInt(i) => Ok(ConstValue::I32(i.value as _)),
            ast::AnyExpr::LitStr(_) => todo!(),
            ast::AnyExpr::Load(_) => todo!(),
            ast::AnyExpr::Func(_) => todo!(),
            ast::AnyExpr::Add(sides) => match sides.eval()? {
                (ConstValue::I32(lhs), ConstValue::I32(rhs)) => {
                    Ok(ConstValue::I32(lhs.wrapping_add(rhs)))
                }
                _ => todo!(),
            },
            ast::AnyExpr::Sub(_) => todo!(),
            ast::AnyExpr::Mul(sides) => match sides.eval()? {
                (ConstValue::I32(lhs), ConstValue::I32(rhs)) => {
                    Ok(ConstValue::I32(lhs.wrapping_mul(rhs)))
                }
                _ => todo!(),
            },
            ast::AnyExpr::Div(_) => todo!(),
            ast::AnyExpr::Call(_) => todo!(),
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
