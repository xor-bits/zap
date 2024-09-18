use std::{collections::HashMap, fmt, rc::Rc};

use lexer::Lexer;
use once_cell::unsync::Lazy;
use parser::{
    ast::{AnyExpr, Ast, BinaryOp, Block, Call, Expr, Root, RootInit, RootItem, Stmt, Test},
    TypeId,
};

//

pub type Result<T, E = RunError> = std::result::Result<T, E>;

#[derive(Debug)]
pub enum RunError {
    Parse(parser::Error),
}

impl fmt::Display for RunError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RunError::Parse(err) => write!(f, "{err}"),
        }
    }
}

impl std::error::Error for RunError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
}

impl From<parser::Error> for RunError {
    fn from(value: parser::Error) -> Self {
        Self::Parse(value)
    }
}

//

pub struct Interpreter {
    globals: Lazy<HashMap<Box<str>, Value>>,
    stack: Vec<HashMap<Box<str>, Value>>,
}

impl Interpreter {
    pub const fn new() -> Self {
        Self {
            globals: Lazy::new(HashMap::new),
            stack: Vec::new(),
        }
    }

    pub fn add(&mut self, name: &str, f: impl Func + 'static) {
        let f = Box::leak(Box::new(f)) as &dyn Func; // FIXME:
        self.globals.insert(name.into(), Value::Func(f));
    }

    pub fn run(&mut self, code: &str) -> Result<()> {
        let mut parser = parser::ParseStream::from_lexer(Lexer::new(code));
        let ast: Ast<Root> = parser.parse()?;

        let ast = Box::leak(Box::new(ast));

        self.run_root(&ast.inner);

        Ok(())
    }

    fn run_root(&mut self, root: &'static Root) {
        for item in root.inner.iter() {
            match item {
                RootItem::Init(v) => self.run_root_init(v),
                RootItem::Test(v) => self.run_test(v),
            }
        }
    }

    fn run_root_init(&mut self, root_init: &'static RootInit) {
        for (target, expr) in root_init.targets.iter().zip(root_init.exprs.iter()) {
            let result = self.run_expr(expr);
            self.globals
                .insert(Box::from(target.path.ident.value.as_str()), result);
        }
    }

    fn run_test(&mut self, test: &'static Test) {
        todo!()
    }

    fn run_expr(&mut self, expr: &'static Expr) -> Value {
        match &expr.expr {
            AnyExpr::Func(f) => Value::Func(f as &dyn Func),

            AnyExpr::Block(block) => self.run_block(block),
            AnyExpr::LitInt(v) => Value::I32(v.value as i32),
            AnyExpr::LitStr(v) => Value::Str(&v.value),
            AnyExpr::Load(load) => self.run_load(&load.value),
            AnyExpr::Call(call) => self.run_call(call),
            AnyExpr::Binary { op, sides } => self.run_binary(*op, sides),
        }
    }

    fn run_block(&mut self, block: &'static Block) -> Value {
        let mut result = Value::Void;
        let mut returns;

        for stmt in block.stmts.iter() {
            (result, returns) = self.run_stmt(stmt);

            if returns {
                return result;
            }
        }

        if block.auto_return {
            result
        } else {
            Value::Void
        }
    }

    fn run_stmt(&mut self, stmt: &'static Stmt) -> (Value, bool) {
        match stmt {
            Stmt::Init(init) => {
                for (target, expr) in init.targets.iter().zip(init.exprs.iter()) {
                    let result = self.run_expr(expr);
                    self.stack
                        .last_mut()
                        .unwrap_or(&mut *self.globals)
                        .insert(Box::from(target.path.ident.value.as_str()), result);
                }

                (Value::Void, false)
            }
            Stmt::Set(set) => {
                for (target, expr) in set.targets.iter().zip(set.exprs.iter()) {
                    let result = self.run_expr(expr);
                    *self.run_load_mut(&target.path.ident.value) = result;
                }

                (Value::Void, false)
            }
            Stmt::Cond(_) => todo!(),
            Stmt::Loop(_) => todo!(),
            Stmt::Expr(expr) => (self.run_expr(&expr.expr), false),
            Stmt::Return(expr) => {
                if let Some(expr) = expr.expr.as_ref() {
                    (self.run_expr(expr), true)
                } else {
                    (Value::Void, true)
                }
            }
        }
    }

    fn run_load(&mut self, load: &'static str) -> Value {
        if let Some(stack_frame) = self
            .stack
            .last()
            .and_then(|stack_frame| stack_frame.get(load))
        {
            return *stack_frame;
        }

        *self.globals.get(load).expect("variable not found")
    }

    fn run_load_mut(&mut self, load: &'static str) -> &mut Value {
        if let Some(stack_frame) = self
            .stack
            .last_mut()
            .and_then(|stack_frame| stack_frame.get_mut(load))
        {
            return stack_frame;
        }

        self.globals.get_mut(load).expect("variable not found")
    }

    fn run_call(&mut self, call: &'static Call) -> Value {
        let func = self.run_expr(&call.func);

        match func {
            Value::Func(func) => func.call(self),
            _ => todo!(),
        }
    }

    fn run_binary(&mut self, op: BinaryOp, sides: &'static (Expr, Expr)) -> Value {
        let lhs = self.run_expr(&sides.0);
        let rhs = self.run_expr(&sides.1);

        match (lhs, op, rhs) {
            (Value::Bool(lhs), BinaryOp::Eq, Value::Bool(rhs)) => Value::Bool(lhs == rhs),
            (Value::Bool(lhs), BinaryOp::Neq, Value::Bool(rhs)) => Value::Bool(lhs != rhs),
            (Value::Bool(lhs), BinaryOp::And, Value::Bool(rhs)) => Value::Bool(lhs && rhs),
            (Value::Bool(lhs), BinaryOp::Or, Value::Bool(rhs)) => Value::Bool(lhs || rhs),

            _ => todo!(),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

//

pub trait Func<A> {
    fn returns(&self) -> TypeId;

    fn args(&self) -> &'static [TypeId];

    fn call(&'static self, interpreter: &mut Interpreter) -> Value;
}

impl<A> Func<A> for parser::ast::Func {
    fn returns(&self) -> TypeId {
        todo!()
    }

    fn args(&self) -> &'static [TypeId] {
        todo!()
    }

    fn call(&'static self, interpreter: &mut Interpreter) -> Value {
        interpreter.run_block(&self.block)
    }
}

impl<A> fmt::Debug for dyn Func<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Func")
    }
}

impl<F: Fn(A) -> R, A: From<Value>, R: Into<Value>> Func<(A, R)> for F {
    fn returns(&self) -> TypeId {
        todo!()
    }

    fn args(&self) -> &'static [TypeId] {
        todo!()
    }

    fn call(&'static self, interpreter: &mut Interpreter) -> Value {
        self.args();

        *self.call(interpreter.run_load("__0"));
        todo!()
    }
}

//

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Bool(bool),
    I32(i32),
    Str(&'static str),
    Func(&'static dyn Func),
    Void,
    Never,
    Unknown,
    Other,
}

//

pub trait IntoValue<A> {}

impl<F: Fn(A) -> R, A: From<Value>, R: Into<Value>> From<F> for Value {}
