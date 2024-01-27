use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use parser::ast;

//

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    NoMainFunction,
    NotCallable,
    VarNotFound,
}

pub type Result<T, E = Error> = core::result::Result<T, E>;

//

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Int,
    Str,
    Func,
    None,
}

//

#[derive(Debug, Clone)]
pub enum Value {
    Int(i128),
    Str(Rc<str>),
    Func(Rc<ast::Func>),

    ExtFunc(ExtFuncWrapper),

    None,
}

impl Value {
    pub fn func<F: ExtFunc>(f: F) -> Self {
        Self::ExtFunc(ExtFuncWrapper { inner: Rc::new(f) })
    }

    pub const fn int(i: i128) -> Self {
        Self::Int(i)
    }

    pub fn str(s: impl Into<Rc<str>>) -> Self {
        Self::Str(s.into())
    }

    pub const fn none() -> Self {
        Self::None
    }

    pub const fn ty(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Str(_) => Type::Str,
            Value::Func(_) => Type::Func,
            Value::ExtFunc(_) => Type::Func,
            Value::None => Type::None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{v}"),
            Value::Str(v) => write!(f, "{v}"),
            Value::Func(_) => write!(f, "<func>"),
            Value::ExtFunc(_) => write!(f, "<ext-func>"),
            Value::None => write!(f, "none"),
        }
    }
}

//

#[derive(Clone)]
pub struct ExtFuncWrapper {
    inner: Rc<dyn ExtFunc>,
}

impl fmt::Debug for ExtFuncWrapper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("ExtFuncWrapper")
            .field("inner", &"..")
            .finish()
    }
}

//

pub trait ExtFunc: 'static {
    fn call(&self, vars: &mut Vars) -> Result<Value>;
}

impl<T: for<'a, 'b> Fn(&'a mut Vars<'b>) -> Result<Value> + 'static> ExtFunc for T {
    fn call(&self, vars: &mut Vars) -> Result<Value> {
        (self)(vars)
    }
}

//

#[derive(Debug, Default)]
pub struct Runner {
    global: StackFrame,
}

//

impl Runner {
    pub fn new() -> Self {
        Self {
            global: StackFrame::new(),
        }
    }

    pub fn add_var(&mut self, name: &str, val: Value) {
        self.global.init(name.into(), val);
    }

    pub fn add_fn(&mut self, name: &str, ext: impl ExtFunc) {
        self.add_var(name, Value::func(ext))
    }

    pub fn add_defaults(&mut self) {
        self.add_fn("print", |vars: &mut Vars| {
            for arg in (0usize..).map_while(|i| vars.ext_arg_nth(i)) {
                print!("{arg}");
            }
            println!();
            Ok(Value::None)
        });
        // self.add_fn("print", |vars| Ok(Value::None));
        // self.add_fn("print", Print);
    }

    pub fn exec(&mut self, t: &impl Execute) -> Result<Value> {
        t.exec(&mut self.vars())
    }

    fn vars(&mut self) -> Vars {
        Vars {
            global: self,
            local: Some(StackFrame::new()),
        }
    }

    fn call(&mut self, f: Value, args: Vec<Value>) -> Result<Value> {
        match f {
            Value::Func(f) => self.call_code(&f, args),
            Value::ExtFunc(f) => self.call_ext(f.inner.as_ref(), args),
            _ => Err(Error::NotCallable),
        }
    }

    fn call_code(&mut self, func: &ast::Func, args: Vec<Value>) -> Result<Value> {
        let mut vars = self.vars();

        assert_eq!(func.args.as_ref().map_or(0, |s| s.iter().len()), args.len());
        for (arg, val) in func.args.iter().flat_map(|c| c.iter()).zip(args) {
            vars.init(arg.id.value.as_str().into(), val);
        }

        func.block.exec(&mut vars)
    }

    fn call_ext(&mut self, func: &dyn ExtFunc, args: Vec<Value>) -> Result<Value> {
        let mut vars = self.vars();

        for (i, val) in args.into_iter().enumerate() {
            vars.init(format!("{i}").into(), val);
        }

        func.call(&mut vars)
    }
}

impl Deref for Runner {
    type Target = StackFrame;

    fn deref(&self) -> &Self::Target {
        &self.global
    }
}

impl DerefMut for Runner {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.global
    }
}

//

#[derive(Debug, Default)]
pub struct StackFrame {
    variables: HashMap<Rc<str>, Value>,
}

//

impl StackFrame {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn load(&mut self, s: &str) -> Option<Value> {
        self.variables.get(s).cloned()
    }

    pub fn store(&mut self, k: Rc<str>, v: Value) -> Result<(), Value> {
        match self.variables.entry(k) {
            Entry::Occupied(_) => Err(v),
            Entry::Vacant(entry) => Ok(_ = entry.insert(v)),
        }
        // if let Some(slot) = self.variables.get_mut(&k) {

        // }
        // self.variables.insert(k, v)
    }

    pub fn init(&mut self, k: Rc<str>, v: Value) -> Option<Value> {
        self.variables.insert(k, v)
    }
}

//

pub struct Vars<'a> {
    global: &'a mut Runner,
    local: Option<StackFrame>,
}

impl<'a> Vars<'a> {
    pub fn drop_local(&mut self) {
        self.local = None;
    }

    pub fn ext_arg_nth(&mut self, n: usize) -> Option<Value> {
        // FIXME: this is stupid
        self.load(&format!("{n}"))
    }

    pub fn load(&mut self, s: &str) -> Option<Value> {
        if let Some(val) = self.local.as_mut().and_then(|local| local.load(s)) {
            return Some(val);
        }

        self.global.load(s)
    }

    pub fn store(&mut self, k: Rc<str>, mut v: Value) -> Result<(), Value> {
        if let Some(local) = self.local.as_mut() {
            match local.store(k.clone(), v) {
                Ok(_) => return Ok(()),
                Err(_v) => v = _v,
            }
        }

        self.global.store(k, v)
    }

    pub fn init(&mut self, k: Rc<str>, v: Value) -> Option<Value> {
        if let Some(local) = self.local.as_mut() {
            local.init(k, v)
        } else {
            self.global.init(k, v)
        }
    }
}

//

pub trait Execute {
    fn exec(&self, vars: &mut Vars) -> Result<Value>;
}

//

impl<T: Execute> Execute for ast::Ast<T> {
    fn exec(&self, vars: &mut Vars) -> Result<Value> {
        self.inner.exec(vars)
    }
}

impl Execute for ast::Root {
    fn exec(&self, vars: &mut Vars) -> Result<Value> {
        vars.drop_local();

        // allow recursion:
        // for root_item in self.inner.iter() {
        //     root_item.exec_lazy();
        // }

        for root_item in self.inner.iter() {
            root_item.exec(vars)?;
        }

        let Some(main) = vars.load("main") else {
            return Err(Error::NoMainFunction);
        };

        vars.global.call(main, vec![])
    }
}

impl Execute for ast::RootItem {
    fn exec(&self, vars: &mut Vars) -> Result<Value> {
        match self {
            ast::RootItem::Init(init) => init.exec(vars),
            ast::RootItem::Test(_) => todo!(),
        }
    }
}

impl Execute for ast::Init {
    fn exec(&self, vars: &mut Vars) -> Result<Value> {
        assert_eq!(self.targets.inner.len(), self.exprs.inner.len());

        fn exec_one(target: &ast::Target, expr: &ast::Expr, vars: &mut Vars) -> Result<()> {
            let v = expr.exec(vars)?;
            vars.init(target.path.ident.value.as_str().into(), v);
            Ok(())
        }

        for (target, expr) in self.targets.iter().zip(self.exprs.iter()) {
            exec_one(target, expr, vars)?;
        }

        Ok(Value::None)
    }
}

impl Execute for ast::Expr {
    fn exec(&self, vars: &mut Vars) -> Result<Value> {
        match self {
            ast::Expr::Block(block) => block.exec(vars),
            ast::Expr::LitInt(lit) => Ok(Value::Int(lit.value)),
            ast::Expr::LitStr(lit) => Ok(Value::Str(lit.value.as_str().into())),
            ast::Expr::Load(load) => vars.load(&load.value).ok_or(Error::VarNotFound),
            ast::Expr::Func(func) => Ok(Value::Func(func.clone().into())),
            ast::Expr::Add(add) => {
                let lhs = add.0.exec(vars)?;
                let rhs = add.1.exec(vars)?;

                match (lhs, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs.wrapping_add(rhs))),
                    (Value::Str(lhs), Value::Int(rhs)) => {
                        Ok(Value::Str(format!("{lhs}{rhs}").into()))
                    }
                    (Value::Str(lhs), Value::Str(rhs)) => {
                        Ok(Value::Str(format!("{lhs}{rhs}").into()))
                    }
                    (lhs, rhs) => {
                        todo!("eval {lhs}+{rhs}")
                    }
                }
            }
            ast::Expr::Sub(sub) => {
                let lhs = sub.0.exec(vars)?;
                let rhs = sub.1.exec(vars)?;

                match (lhs, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs.wrapping_sub(rhs))),
                    (lhs, rhs) => {
                        todo!("eval {lhs}+{rhs}")
                    }
                }
            }
            ast::Expr::Mul(mul) => {
                let lhs = mul.0.exec(vars)?;
                let rhs = mul.1.exec(vars)?;

                match (lhs, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs.wrapping_mul(rhs))),
                    (lhs, rhs) => {
                        todo!("eval {lhs}+{rhs}")
                    }
                }
            }
            ast::Expr::Div(div) => {
                let lhs = div.0.exec(vars)?;
                let rhs = div.1.exec(vars)?;

                match (lhs, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs.wrapping_div(rhs))),
                    (lhs, rhs) => {
                        todo!("eval {lhs}+{rhs}")
                    }
                }
            }
            ast::Expr::Call(call) => call.exec(vars),
        }
    }
}

impl Execute for ast::Call {
    fn exec(&self, vars: &mut Vars) -> Result<Value> {
        let func = self.func.exec(vars)?;

        let args = self
            .args
            .iter()
            .flat_map(|c| c.iter())
            .map(|expr| expr.exec(vars))
            .collect::<Result<Vec<Value>>>()?;

        vars.global.call(func, args)
    }
}

impl Execute for ast::Block {
    fn exec(&self, vars: &mut Vars) -> Result<Value> {
        let mut last = Value::None;
        for stmt in self.stmts.iter() {
            last = stmt.exec(vars)?;
            if !matches!(last, Value::None) {
                return Ok(last);
            }
        }

        if self.auto_return {
            println!("auto return {last}");
            return Ok(last);
        }

        // TODO: drop variables declared in this block

        Ok(Value::None)
    }
}

impl Execute for ast::Stmt {
    fn exec(&self, vars: &mut Vars) -> Result<Value> {
        match self {
            ast::Stmt::Init(init) => init.exec(vars),
            ast::Stmt::Expr(expr) => expr.expr.exec(vars),
            ast::Stmt::Return(expr) => expr.expr.exec(vars),
        }
    }
}
