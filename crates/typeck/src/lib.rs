use core::fmt;
use std::{collections::HashMap, rc::Rc};

use parser::{ast, TypeId};

//

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FuncId(pub usize);

//

#[derive(Debug)]
pub struct Func {
    pub ret: TypeId,
    pub args: Vec<TypeId>,
}

//

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    I32,
    Str,
    Void,
    Func(FuncId),
    // ExternFunc, // a function that can take any arguments
}

impl Type {
    pub const fn as_func(&self) -> Option<FuncId> {
        match self {
            Type::Func(i) => Some(*i),
            _ => None,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::Str => write!(f, "str"),
            Type::Void => write!(f, "void"),
            Type::Func(id) => write!(f, "[{}]", id.0),
        }
    }
}

//

#[derive(Debug)]
pub struct Context {
    funcs: Funcs,
    types: Types,
    vars: Vars,
    // fns: HashMap,
}

impl Context {
    pub const fn new() -> Self {
        Self {
            funcs: Funcs::new(),
            types: Types::new(),
            vars: Vars::new(),
        }
    }

    pub fn add_extern(&mut self, name: &str, func: Func) {
        let func_id = self.funcs.push(func);
        let type_id = self.types.push(Type::Func(func_id));
        if self.vars.init(name, type_id).is_some() {
            panic!("`{name}` defined twice");
        }
    }

    pub fn get_type(&self, id: TypeId) -> &Type {
        self.types.get(id).unwrap()
    }

    pub fn get_func(&self, id: FuncId) -> &Func {
        self.funcs.get(id).unwrap()
    }
}

//

#[derive(Debug)]
pub struct Vars {
    scopes: Vec<Scope>,
    global_scope: Scope,
}

impl Vars {
    pub const fn new() -> Self {
        Self {
            scopes: Vec::new(),
            global_scope: Scope::new(),
        }
    }

    pub fn get(&self, var: &str) -> Option<TypeId> {
        if let Some(local) = self.scopes.last() {
            if let Some(local) = local.get(var) {
                assert_ne!(local, TypeId::Unknown);

                return Some(local);
            }
        }

        let global = self.global_scope.get(var)?;

        assert_ne!(global, TypeId::Unknown);

        Some(global)
    }

    #[track_caller]
    pub fn init(&mut self, var: &str, ty: TypeId) -> Option<TypeId> {
        assert_ne!(ty, TypeId::Unknown);

        self.scopes
            .last_mut()
            .unwrap_or(&mut self.global_scope)
            .init(var, ty)
    }

    pub fn return_ty(&self) -> TypeId {
        self.scopes.last().unwrap_or(&self.global_scope).return_ty
    }

    pub fn push(&mut self, return_ty: TypeId) {
        self.scopes.push(Scope {
            vars: None,
            return_ty,
        })
    }

    pub fn pop(&mut self) {
        self.scopes.pop().unwrap();
    }
}

//

#[derive(Debug)]
pub struct Funcs {
    funcs: Vec<Func>,
    // func_lookup:
}

impl Funcs {
    pub const fn new() -> Self {
        Self { funcs: Vec::new() }
    }

    pub fn get(&self, id: FuncId) -> Option<&Func> {
        self.funcs.get(id.0)
    }

    pub fn push(&mut self, f: Func) -> FuncId {
        let id = FuncId(self.funcs.len());
        self.funcs.push(f);
        id
    }
}

//

#[derive(Debug)]
pub struct Types {
    types: Vec<Type>,
    // typename_lookup: Option<HashMap<Rc<str>, TypeId>>,
}

impl Types {
    pub const fn new() -> Self {
        Self {
            types: Vec::new(),
            // typename_lookup: None,
        }
    }

    pub fn get(&self, id: TypeId) -> Option<&Type> {
        match id {
            TypeId::I32 => Some(&Type::I32),
            TypeId::Str => Some(&Type::Str),
            TypeId::Void => Some(&Type::Void),
            TypeId::Unknown => unreachable!(),
            TypeId::Other(id) => self.types.get(id as usize),
        }
    }

    #[track_caller]
    pub fn lookup(&mut self, name: &str) -> Option<TypeId> {
        match name {
            "i32" => Some(TypeId::I32),
            _ => {
                todo!("unknown type `{name}`")
            }
        }
    }

    // pub fn find(&self, ty: &Type) -> Option<TypeId> {
    //     Some(TypeId(self.types.iter().position(|s| s == ty)?))
    // }

    // pub fn primitive(&mut self, ty: Type) -> TypeId {
    //     match ty {
    //         Type::I32 => self.lookup("i32"),
    //         Type::Void => todo!(),
    //         _ => panic!("`{ty:?}` is not a primitive"),
    //     }
    //     if let Some(id) = self.find(&ty) {
    //         return id;
    //     }

    //     self.typename_lookup
    //         .get_or_insert_with(<_>::default)
    //         .insert(ty.to_string().into());

    //     self.push(ty)
    // }

    pub fn push(&mut self, ty: Type) -> TypeId {
        Self::_push(&mut self.types, ty)
    }

    fn _push(types: &mut Vec<Type>, ty: Type) -> TypeId {
        let id = TypeId::Other(types.len() as _);
        types.push(ty);
        id
    }
}

//

#[derive(Debug)]
pub struct Scope {
    vars: Option<HashMap<Rc<str>, TypeId>>,
    return_ty: TypeId,
}

impl Scope {
    pub const fn new() -> Self {
        Self {
            vars: None,
            return_ty: TypeId::Unknown,
        }
    }

    pub fn get(&self, var: &str) -> Option<TypeId> {
        self.vars.as_ref()?.get(var).copied()
    }

    pub fn init(&mut self, var: &str, ty: TypeId) -> Option<TypeId> {
        self.vars
            .get_or_insert_with(<_>::default)
            .insert(var.into(), ty)
    }
}

//

pub trait TypeCheck {
    fn type_check(&mut self, ctx: &mut Context);
}

//

impl<T: TypeCheck> TypeCheck for ast::Ast<T> {
    fn type_check(&mut self, ctx: &mut Context) {
        self.inner.type_check(ctx)
    }
}

impl TypeCheck for ast::Root {
    fn type_check(&mut self, ctx: &mut Context) {
        for item in self.inner.iter_mut() {
            item.type_check(ctx);
        }
    }
}

impl TypeCheck for ast::RootItem {
    fn type_check(&mut self, ctx: &mut Context) {
        match self {
            ast::RootItem::Init(v) => v.type_check(ctx),
            ast::RootItem::Test(_) => todo!(),
        }
    }
}

impl TypeCheck for ast::RootInit {
    fn type_check(&mut self, ctx: &mut Context) {
        let targets = self.targets.iter();
        let exprs = self.exprs.iter_mut();
        assert_eq!(targets.len(), exprs.len());

        for (target, expr) in targets.zip(exprs) {
            expr.type_check(ctx);
            if expr.ty == TypeId::Unknown {
                panic!("{expr:#?}");
            }
            ctx.vars.init(&target.path.ident.value, expr.ty);
        }
    }
}

impl TypeCheck for ast::Expr {
    fn type_check(&mut self, ctx: &mut Context) {
        self.ty = match &mut self.expr {
            ast::AnyExpr::Block(v) => {
                v.type_check(ctx);
                v.ty
            }
            ast::AnyExpr::LitInt(_) => TypeId::I32,
            ast::AnyExpr::LitStr(_) => TypeId::Str,
            ast::AnyExpr::Load(v) => ctx
                .vars
                .get(&v.value)
                .unwrap_or_else(|| panic!("unknown variable `{}`", v.value)),
            ast::AnyExpr::Func(v) => {
                v.type_check(ctx);
                v.ty
            }
            ast::AnyExpr::Add(v) => {
                v.0.type_check(ctx);
                v.1.type_check(ctx);

                match (
                    ctx.types.get(v.0.ty).unwrap(),
                    ctx.types.get(v.1.ty).unwrap(),
                ) {
                    (Type::I32, Type::I32) => TypeId::I32,
                    (lhs, rhs) => {
                        panic!("cannot `{lhs}+{rhs}`")
                    }
                }
            }
            ast::AnyExpr::Sub(v) => {
                v.0.type_check(ctx);
                v.1.type_check(ctx);

                match (
                    ctx.types.get(v.0.ty).unwrap(),
                    ctx.types.get(v.1.ty).unwrap(),
                ) {
                    (Type::I32, Type::I32) => TypeId::I32,
                    (lhs, rhs) => {
                        panic!("cannot `{lhs}-{rhs}`")
                    }
                }
            }
            ast::AnyExpr::Mul(v) => {
                v.0.type_check(ctx);
                v.1.type_check(ctx);

                match (
                    ctx.types.get(v.0.ty).unwrap(),
                    ctx.types.get(v.1.ty).unwrap(),
                ) {
                    (Type::I32, Type::I32) => TypeId::I32,
                    (lhs, rhs) => {
                        panic!("cannot `{lhs}*{rhs}`")
                    }
                }
            }
            ast::AnyExpr::Div(v) => {
                v.0.type_check(ctx);
                v.1.type_check(ctx);

                match (
                    ctx.types.get(v.0.ty).unwrap(),
                    ctx.types.get(v.1.ty).unwrap(),
                ) {
                    (Type::I32, Type::I32) => TypeId::I32,
                    (lhs, rhs) => {
                        panic!("cannot `{lhs}/{rhs}`")
                    }
                }
            }
            ast::AnyExpr::Call(v) => {
                v.func.type_check(ctx);
                for arg in v.args_mut() {
                    arg.type_check(ctx);
                }

                let type_id = v.func.ty;
                let fn_type = ctx.types.get(type_id).unwrap();
                let &Type::Func(func_id) = fn_type else {
                    panic!("cannot call `{fn_type}`");
                };

                let func = ctx.funcs.get(func_id).unwrap();

                assert_eq!(
                    v.args().len(),
                    func.args.len(),
                    "argument count mismatch (got `{}`, expected `{}`)",
                    v.args().len(),
                    func.args.len(),
                );
                for (arg, arg_ty) in v.args().zip(func.args.iter().copied()) {
                    assert_eq!(
                        arg.ty,
                        arg_ty,
                        "argument type mismatch (got `{}`, expected `{}`) {arg:#?}",
                        TypeDisplay(ctx, arg.ty),
                        TypeDisplay(ctx, arg_ty),
                    );
                }

                assert_ne!(func.ret, TypeId::Unknown);

                func.ret
            }
        }
    }
}

impl TypeCheck for ast::Block {
    fn type_check(&mut self, ctx: &mut Context) {
        for stmt in self.stmts.iter_mut() {
            stmt.type_check(ctx);
        }

        self.ty = TypeId::Void;
        if self.auto_return {
            if let Some(last) = self.stmts.last() {
                self.ty = match last {
                    ast::Stmt::Init(_) => TypeId::Void,
                    ast::Stmt::Set(_) => TypeId::Void,
                    ast::Stmt::Expr(v) => v.expr.ty,
                    ast::Stmt::Return(v) => v.expr.ty,
                };
            }
        }
    }
}

impl TypeCheck for ast::Stmt {
    fn type_check(&mut self, ctx: &mut Context) {
        match self {
            ast::Stmt::Init(v) => {
                assert_eq!(v.targets.iter().len(), v.exprs.iter().len());
                for (target, expr) in v.targets.iter().zip(v.exprs.iter_mut()) {
                    expr.type_check(ctx);
                    ctx.vars.init(&target.path.ident.value, expr.ty);
                }
            }
            ast::Stmt::Set(v) => {
                assert_eq!(v.targets.iter().len(), v.exprs.iter().len());
                for (target, expr) in v.targets.iter().zip(v.exprs.iter_mut()) {
                    expr.type_check(ctx);
                    let current_ty = ctx
                        .vars
                        .get(&target.path.ident.value)
                        .expect("variable not found");
                    assert_eq!(
                        expr.ty,
                        current_ty,
                        "cannot assign `{}` to `{}`",
                        TypeDisplay(ctx, expr.ty),
                        TypeDisplay(ctx, current_ty)
                    );
                }
            }
            ast::Stmt::Expr(v) => v.expr.type_check(ctx),
            ast::Stmt::Return(v) => {
                v.expr.type_check(ctx);
                assert_eq!(v.expr.ty, ctx.vars.return_ty(), "invalid return type");
            }
        }
    }
}

pub struct Proto<'a, Args> {
    pub ty: TypeId,
    pub return_ty: Option<&'a str>,
    pub args: Args,
}

impl<'a, Args: Clone + ExactSizeIterator<Item = &'a ast::Argument>> TypeCheck for Proto<'a, Args> {
    fn type_check(&mut self, ctx: &mut Context) {
        let ret = self.return_ty.map_or(TypeId::Unknown, |ty| {
            ctx.types.lookup(ty).expect("unknown type")
        });

        let args: Vec<TypeId> = self
            .args
            .clone()
            .map(|arg| ctx.types.lookup(&arg.ty.value).expect("unknown type"))
            .collect();

        let func_id = ctx.funcs.push(Func { ret, args });
        let type_id = ctx.types.push(Type::Func(func_id));

        self.ty = type_id;
    }
}

impl TypeCheck for ast::Func {
    fn type_check(&mut self, ctx: &mut Context) {
        // TODO: run all Proto::type_check functions before running the block code
        // to allow recursion and ordering the global scope in any way
        self.ty = {
            let mut proto = Proto {
                ty: self.ty,
                return_ty: self
                    .return_ty
                    .as_ref()
                    .map(|(_, type_name)| type_name.value.as_str()),
                args: self.args(),
            };
            proto.type_check(ctx);
            proto.ty
        };

        let type_id = self.ty;

        let func_id = match ctx.types.get(type_id) {
            Some(Type::Func(func_id)) => *func_id,
            None => panic!("Partial<Func>::type_check not called"),
            _ => unreachable!(),
        };

        let func = ctx.funcs.get(func_id).unwrap();

        ctx.vars.push(func.ret);

        for (arg, arg_type_id) in self
            .args
            .iter()
            .flat_map(|s| s.iter())
            .zip(func.args.iter())
        {
            ctx.vars.init(&arg.id.value, *arg_type_id);
        }

        self.block.type_check(ctx);
        assert_eq!(self.block.ty, ctx.vars.return_ty(), "invalid return type");

        ctx.vars.pop();
    }
}

//

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "functions:")?;
        for (id, _) in self.funcs.funcs.iter().enumerate() {
            writeln!(f, "{}", FuncDisplay(self, FuncId(id)))?;
        }

        writeln!(f)?;
        writeln!(f, "types:")?;
        for (id, _) in self.types.types.iter().enumerate() {
            writeln!(f, "{}", TypeDisplay(self, TypeId::Other(id as _)))?;
        }

        writeln!(f)?;
        writeln!(f, "global scope:")?;
        for (name, &ty) in self.vars.global_scope.vars.iter().flat_map(|m| m.iter()) {
            writeln!(f, "{name}: {}", TypeDisplay(self, ty))?;
        }

        Ok(())
    }
}

struct FuncDisplay<'a>(&'a Context, FuncId);

impl fmt::Display for FuncDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // return Ok(());
        write!(f, "[anon_func_{}] (", self.1 .0)?;
        let func = self.0.funcs.get(self.1).unwrap();
        let mut iter = func.args.iter().copied();
        if let Some(arg) = iter.next() {
            write!(f, "{}", TypeDisplay(self.0, arg))?;
        }
        for arg in iter {
            write!(f, ", {}", TypeDisplay(self.0, arg))?;
        }
        write!(f, ") -> {}", TypeDisplay(self.0, func.ret))
    }
}

struct TypeDisplay<'a>(&'a Context, TypeId);

impl fmt::Display for TypeDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.1.is_unknown() {
            return write!(f, "!not-typechecked!");
        }

        // return Ok(());
        let ty = self.0.types.get(self.1).unwrap();
        match ty {
            Type::I32 => write!(f, "i32")?,
            Type::Str => write!(f, "str")?,
            Type::Void => write!(f, "void")?,
            Type::Func(v) => write!(f, "<{}>", FuncDisplay(self.0, *v))?,
        }
        Ok(())
    }
}

// use core::panic;
// use std::{
//     collections::HashMap,
//     rc::Rc,
//     sync::atomic::{AtomicUsize, Ordering},
// };

// use parser::ast;

// //

// #[derive(Debug)]
// pub enum LookupFnOneError {
//     UnknownFunction(String),
//     NoMatches,
//     MultipleMatches,
// }

// //

// #[derive(Debug)]
// pub struct Context {
//     fns: Functions,
//     types: Types,
//     current_fn: Vec<Func>,
// }

// impl Context {
//     pub const fn new() -> Self {
//         Self {
//             fns: Functions {
//                 fns: Vec::new(),
//                 fn_group_names: None,
//             },
//             types: Types {
//                 types: Vec::new(),
//                 type_names: None,
//             },
//             current_fn: Vec::new(),
//         }
//     }

//     pub fn add_built_in(&mut self) {
//         self.types.add_built_in();
//     }
// }

// //

// #[derive(Debug)]
// pub struct Types {
//     types: Vec<Type>,

//     // index into `types`
//     type_names: Option<HashMap<Rc<str>, TypeId>>,
// }

// impl Types {
//     pub fn add_built_in(&mut self) {
//         self.add_type(Some("void"), Type::Void);
//         self.add_type(Some("i32"), Type::I32);
//     }

//     pub fn add_type(&mut self, name: Option<&str>, ty: Type) -> TypeId {
//         if self.types.contains(&ty) {
//             todo!("type already defined");
//         }

//         let id = TypeId(self.types.len());
//         self.types.push(ty);
//         if let Some(name) = name {
//             self.type_names
//                 .get_or_insert_with(<_>::default)
//                 .insert(name.into(), id);
//         }
//         id
//     }

//     pub fn find(&self, ty: &Type) -> Option<TypeId> {
//         self.types.iter().enumerate().find_map(
//             |(i, t)| {
//                 if t == ty {
//                     Some(TypeId(i))
//                 } else {
//                     None
//                 }
//             },
//         )
//     }

//     pub fn lookup(&self, name: &str) -> Option<TypeId> {
//         self.type_names.as_ref()?.get(name).copied()
//     }
// }

// //

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub enum Type {
//     Void,
//     Integer(i128), // literal integer
//     I32,
//     Func(FuncId),
// }

// //

// #[derive(Debug)]
// pub struct Functions {
//     fns: Vec<Func>,

//     // index into `fns`
//     fn_group_names: Option<HashMap<Rc<str>, Vec<Func>>>,
// }

// impl Functions {
//     pub fn add_built_in(&mut self) {
//         self.add_type(Some("void"), Type::Void);
//         self.add_type(Some("i32"), Type::I32);
//     }

//     pub fn add_func(&mut self, name: &str, )

//     pub fn lookup_fn<'a>(
//         &'a mut self,
//         name: &'_ str,
//         sign_query: FnSignQuery<'a>,
//     ) -> Option<impl Iterator<Item = FuncId> + 'a> {
//         Some(
//             self.fn_group_names
//                 // lazy init the lookup function table
//                 .get_or_insert_with(<_>::default)
//                 // get the function 'group' like functions with different signatures but the same name
//                 .get(name)?
//                 .iter()
//                 .enumerate()
//                 // filter out incorrect param counts
//                 .filter(|(_, sign)| sign.params.len() == sign_query.params.len())
//                 // filter out incorrect return types (keep everything if the query is None)
//                 // the map_or thing just tests if the types match OR if the query is None
//                 .filter(move |(_, sign)| sign_query.ty.map_or(true, |ty| sign.ty == ty))
//                 // filter out incorrect parameter types (for each: keep, if the query is None)
//                 .filter(|(_, sign)| {
//                     sign_query
//                         .params
//                         .iter()
//                         .zip(sign.params.iter())
//                         .all(|(query_param, param)| {
//                             query_param.as_ref().map_or(true, |ty| param == ty)
//                         })
//                 })
//                 // and return all functions that match the query
//                 .map(|(idx, _)| FuncId(idx)),
//         )
//     }

//     pub fn lookup_fn_one(
//         &mut self,
//         name: &str,
//         sign_query: FnSignQuery,
//     ) -> Result<FuncId, LookupFnOneError> {
//         let mut iter = self
//             .lookup_fn(name, sign_query)
//             .ok_or_else(|| LookupFnOneError::UnknownFunction(name.to_string()))?;
//         let first = iter.next().ok_or(LookupFnOneError::NoMatches)?;
//         if iter.next().is_some() {
//             return Err(LookupFnOneError::MultipleMatches);
//         }
//         Ok(first)
//     }
// }

// //

// pub struct FnSignQuery<'a> {
//     ty: Option<TypeId>,
//     params: &'a [Option<TypeId>],
// }

// //

// #[derive(Debug)]
// pub struct Func {
//     ty: TypeId,
//     params: Vec<TypeId>,

//     name: Rc<str>,
//     // TODO: is_generic: bool,

//     // index into `stmts`
//     vars: HashMap<Rc<str>, VarId>,

//     stmts: Vec<Stmt>,
// }

// impl Func {
//     pub fn new(ty: TypeId, params: Vec<(Rc<str>, TypeId)>, name: Rc<str>) -> Self {
//         let mut new_fn = Self {
//             ty,
//             params: params.iter().map(|(_, ty)| *ty).collect(),
//             name,
//             vars: <_>::default(),
//             stmts: <_>::default(),
//         };

//         for (nth, (arg, _)) in params.iter().enumerate() {
//             new_fn.push(
//                 Some(arg.as_ref()),
//                 Stmt {
//                     ty,
//                     expr: Expr::LoadArg { nth },
//                 },
//             );
//         }

//         new_fn
//     }

//     pub fn push_fn_call(&mut self, name: &str, fns: &mut Functions, args: Vec<VarId>) -> VarId {
//         let arg_types = args
//             .iter()
//             .map(|v| Some(self.get(*v).unwrap()))
//             .collect::<Vec<Option<TypeId>>>();

//         let func = fns
//             .lookup_fn_one(
//                 name,
//                 FnSignQuery {
//                     ty: None,
//                     params: &arg_types,
//                 },
//             )
//             .unwrap();

//         self.push(
//             None,
//             Stmt {
//                 ty: fns.fns[func.0].ty,
//                 expr: Expr::Call { func, args },
//             },
//         )
//     }

//     pub fn push(&mut self, name: Option<&str>, stmt: Stmt) -> VarId {
//         let var = VarId(self.stmts.len());
//         if let Some(name) = name {
//             self.vars.insert(name.into(), var);
//         }
//         self.stmts.push(stmt);
//         var
//     }

//     pub fn get(&self, var: VarId) -> Option<TypeId> {
//         self.stmts.get(var.0).map(|s| s.ty)
//     }

//     pub fn lookup(&self, var: &str) -> Option<VarId> {
//         self.vars.get(var).copied()
//     }
// }

// //

// /// Semantic Analysis
// pub trait Analyze {
//     type Output;

//     fn analyze(&self, ctx: &mut Context) -> Self::Output;
// }

// //

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub struct TypeId(pub usize);

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub struct VarId(pub usize);

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub struct FuncId(pub usize);

// //

// #[derive(Debug)]
// pub struct Stmt {
//     /// type id of the output
//     pub ty: TypeId,

//     /// variable name, converted from a str into a (index) number
//     // pub var: VarId,

//     /// the expression itself, these are not like the AST expressions and have 0 nesting
//     pub expr: Expr,
// }

// #[derive(Debug)]
// pub enum Expr {
//     Call { func: FuncId, args: Vec<VarId> },
//     Return { var: VarId },
//     ConstInitI32 { v: i32 },
//     LoadGlobal { var: VarId },
//     LoadArg { nth: usize },
//     ZeroInit,
// }

// //

// impl<T: Analyze> Analyze for ast::Ast<T> {
//     type Output = T::Output;

//     fn analyze(&self, ctx: &mut Context) -> Self::Output {
//         self.inner.analyze(ctx)
//     }
// }

// impl Analyze for ast::Root {
//     type Output = ();

//     fn analyze(&self, ctx: &mut Context) -> Self::Output {
//         ctx.current_fn.push(Func {
//             ty: ctx.types.find(&Type::Void).unwrap(),
//             params: Vec::new(),
//             name: "[root]".into(),
//             vars: <_>::default(),
//             stmts: <_>::default(),
//         });

//         for item in self.inner.iter() {
//             item.analyze(ctx);
//         }

//         ctx.current_fn.pop();
//     }
// }

// impl Analyze for ast::RootItem {
//     type Output = ();

//     fn analyze(&self, ctx: &mut Context) -> Self::Output {
//         match self {
//             ast::RootItem::Init(init) => init.analyze(ctx),
//             ast::RootItem::Test(_) => todo!(),
//         }
//     }
// }

// impl Analyze for ast::RootInit {
//     type Output = ();

//     fn analyze(&self, ctx: &mut Context) -> Self::Output {
//         for expr in self.exprs.iter() {
//             expr.analyze(ctx);
//         }
//     }
// }

// impl Analyze for ast::Stmt {
//     type Output = ();

//     fn analyze(&self, ctx: &mut Context) -> Self::Output {
//         match self {
//             ast::Stmt::Init(_) => todo!(),
//             ast::Stmt::Expr(expr) => {
//                 _ = expr.expr.analyze(ctx);
//             }
//             ast::Stmt::Return(expr) => {
//                 let var = expr.expr.analyze(ctx);

//                 let curr = ctx.current_fn.last_mut().unwrap();
//                 curr.push(
//                     None,
//                     Stmt {
//                         ty: curr.get(var).unwrap(),
//                         expr: Expr::Return { var },
//                     },
//                 );
//             }
//         }
//     }
// }

// impl Analyze for ast::Expr {
//     type Output = VarId;

//     fn analyze(&self, ctx: &mut Context) -> Self::Output {
//         match self {
//             ast::Expr::Block(_) => todo!(),
//             ast::Expr::LitInt(i) => ctx.current_fn.last_mut().unwrap().push(
//                 None,
//                 Stmt {
//                     ty: ctx.types.find(&Type::I32).unwrap(),
//                     expr: Expr::ConstInitI32 { v: i.value as _ },
//                 },
//             ),
//             ast::Expr::LitStr(_) => todo!(),
//             ast::Expr::Load(id) => {
//                 // load local is simpler to load from,
//                 // but make sure that the current_fn is actually not the global scope
//                 if ctx.current_fn.len() >= 2 {
//                     let curr = ctx.current_fn.last_mut().unwrap();
//                     if let Some(var) = curr.lookup(&id.value) {
//                         return var;
//                     }
//                 }

//                 let curr = ctx.current_fn.first_mut().unwrap();
//                 if let Some(var) = curr.lookup(&id.value) {
//                     let ty = curr.get(var).unwrap();

//                     let curr = ctx.current_fn.last_mut().unwrap();
//                     return curr.push(
//                         None,
//                         Stmt {
//                             ty,
//                             expr: Expr::LoadGlobal { var },
//                         },
//                     );
//                 }

//                 panic!("cant find var `{}`", id.value);
//             }
//             ast::Expr::Func(func) => {
//                 let ty = if let Some((_, ty_name)) = func.return_ty.as_ref() {
//                     ctx.types.lookup(&ty_name.value).expect("unknown type")
//                 } else {
//                     ctx.types.find(&Type::Void).expect("unknown type")
//                 };

//                 static FN_NAME_GEN: AtomicUsize = AtomicUsize::new(0);
//                 let fn_id = FN_NAME_GEN.fetch_add(1, Ordering::Relaxed);
//                 let name = format!("__fn_{}", fn_id).into();
//                 let fn_type = ctx.types.add_type(None, Type::Func(FuncId(fn_id)));

//                 let params = func
//                     .args
//                     .iter()
//                     .flat_map(|s| s.iter())
//                     .map(|arg| {
//                         (
//                             arg.id.value.as_str().into(),
//                             ctx.types.lookup(&arg.ty.value).expect("unknown type"),
//                         )
//                     })
//                     .collect();

//                 // construct the anonymous struct with the function as a method
//                 let res = ctx.current_fn.last_mut().unwrap().push(
//                     None,
//                     Stmt {
//                         ty: fn_type,
//                         expr: Expr::ZeroInit,
//                     },
//                 );

//                 ctx.current_fn.push(Func::new(ty, params, name));

//                 for stmt in func.block.stmts.iter() {
//                     stmt.analyze(ctx);
//                 }

//                 ctx.current_fn.pop();

//                 res
//             }
//             ast::Expr::Add(expr) => {
//                 let lhs = expr.0.analyze(ctx);
//                 let rhs = expr.1.analyze(ctx);

//                 ctx.current_fn.last_mut().unwrap().push_fn_call(
//                     "[operator_add]",
//                     &mut ctx.fns,
//                     vec![lhs, rhs],
//                 )
//             }
//             ast::Expr::Sub(expr) => {
//                 let lhs = expr.0.analyze(ctx);
//                 let rhs = expr.1.analyze(ctx);

//                 ctx.current_fn.last_mut().unwrap().push_fn_call(
//                     "[operator_sub]",
//                     &mut ctx.fns,
//                     vec![lhs, rhs],
//                 )
//             }
//             ast::Expr::Mul(expr) => {
//                 let lhs = expr.0.analyze(ctx);
//                 let rhs = expr.1.analyze(ctx);

//                 ctx.current_fn.last_mut().unwrap().push_fn_call(
//                     "[operator_mul]",
//                     &mut ctx.fns,
//                     vec![lhs, rhs],
//                 )
//             }
//             ast::Expr::Div(_) => todo!(),
//             ast::Expr::Call(call) => {
//                 let func = call.func.analyze(ctx);

//                 let args = call
//                     .args
//                     .iter()
//                     .flat_map(|s| s.iter())
//                     .map(|expr| expr.analyze(ctx))
//                     .collect::<Vec<_>>();

//                 let curr = ctx.current_fn.last_mut().unwrap();
//                 let func_ty = curr.get(func).unwrap();
//                 let func_ty = ctx.types.types[func_ty.0];
//                 let func_id = match func_ty {
//                     Type::Void => todo!(),
//                     Type::Integer(_) => todo!(),
//                     Type::I32 => todo!(),
//                     Type::Func(func_id) => func_id,
//                 };

//                 curr.push(
//                     None,
//                     Stmt {
//                         ty: ctx.fns.fns[func_id.0].ty,
//                         expr: Expr::Call {
//                             func: func_id,
//                             args,
//                         },
//                     },
//                 )
//             }
//         }
//     }
// }
