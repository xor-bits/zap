use core::fmt;

use crate::{unexpected, Parse, ParseStream, Result, Token, TypeId};
use macros::Parse;

use token::*;

#[cfg(test)]
use serde::Serialize;

//

pub mod token;

//

// pub trait AstDisplay: Sized {
//     fn ast_display(&self) -> AsAstDisplay<Self> {
//         AsAstDisplay { v: self }
//     }

//     fn fmt(&self, f: &mut fmt::Formatter, i: Indent) -> fmt::Result;
// }

//

// pub struct AsAstDisplay<'a, T> {
//     v: &'a T,
// }

// impl<T: AstDisplay> fmt::Display for AsAstDisplay<'_, T> {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         self.v.fmt(f, Indent(0))
//     }
// }

//

// pub struct Indent(usize);

// impl Indent {
//     pub const fn push(&mut self) {
//         self.0 += 1;
//     }

//     pub const fn pop(&mut self) {
//         self.0 -= 1;
//     }
// }

// impl fmt::Display for Indent {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "{: <1$}", "", self.0)
//     }
// }

//

/// any part of the grammar, but terminated with EOI
#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
pub struct Ast<T> {
    pub inner: T,
    pub eoi: Eoi,
}

// impl<T: AstDisplay> AstDisplay for Ast<T> {
//     fn fmt(&self, f: &mut fmt::Formatter, i: Indent) -> fmt::Result {
//         self.inner.fmt(f, i)
//     }
// }

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Root {
    pub stmts: Vec<Stmt>,
}

impl Parse for Root {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let mut stmts = Vec::new();
        loop {
            let mut look = tokens.look1();
            if look.peek(Token::Semi) {
                _ = tokens.next();
            } else if look.peek(Token::Eoi) {
                break;
            } else {
                stmts.push(tokens.parse()?);
            }
        }
        Ok(Self { stmts })
    }
}

// impl AstDisplay for Root {
//     fn fmt(&self, f: &mut fmt::Formatter, i: Indent) -> fmt::Result {
//         for item in self.inner.iter() {
//             item.fmt(f, i)?;
//         }

//         Ok(())
//     }
// }

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct Target {
    pub path: Path,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct Path {
    pub ident: Ident,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommaSeparated<T> {
    pub first: T,
    pub inner: Vec<CommaSeparatedItem<T>>,
}

impl<T: Parse> Parse for CommaSeparated<T> {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let first = tokens.parse()?;
        let mut inner = Vec::new();
        while tokens.peek1(Token::Comma) {
            _ = tokens.next();
            inner.push(CommaSeparatedItem {
                comma: Comma,
                item: tokens.parse()?,
            });
        }
        Ok(Self { first, inner })
    }
}

impl<T> CommaSeparated<T> {
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &T> + Clone {
        ChainOne {
            first: Some(&self.first),
            inner: self.inner.iter().map(|s| &s.item),
        }
    }

    pub fn iter_mut(&mut self) -> impl ExactSizeIterator<Item = &mut T> {
        ChainOne {
            first: Some(&mut self.first),
            inner: self.inner.iter_mut().map(|s| &mut s.item),
        }
    }
}

//

#[derive(Clone)]
pub struct ChainOne<I: Iterator> {
    first: Option<I::Item>,
    inner: I,
}

impl<I: Iterator> Iterator for ChainOne<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(first) = self.first.take() {
            return Some(first);
        }

        self.inner.next()
    }
}

impl<I: ExactSizeIterator> ExactSizeIterator for ChainOne<I> {
    fn len(&self) -> usize {
        self.inner.len() + if self.first.is_some() { 1 } else { 0 }
    }
}

//

#[derive(Clone)]
pub struct OptionInner<I: Iterator> {
    inner: Option<I>,
}

impl<I: Iterator> Iterator for OptionInner<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.as_mut()?.next()
    }
}

impl<I: ExactSizeIterator> ExactSizeIterator for OptionInner<I> {
    fn len(&self) -> usize {
        self.inner.as_ref().map_or(0, |i| i.len())
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CommaSeparatedItem<T> {
    pub comma: Comma,
    pub item: T,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct Test {
    pub test_kw: token::Test,
    pub name: LitStr,
    pub block: Block,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub ty: TypeId,
    pub open: LBrace,
    pub stmts: Vec<Stmt>,
    pub auto_return: bool,
    pub close: RBrace,
}

impl Parse for Block {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let open = tokens.parse()?;
        let mut stmts = Vec::new();
        while !tokens.peek1(Token::RBrace) {
            stmts.push(tokens.parse()?);

            let mut has_semi = false;
            while tokens.peek1(Token::Semi) {
                // skip all semicolons
                has_semi = true;
                _ = tokens.next_token()?;
            }

            if !has_semi {
                // no semi == it is the last statement and an implicit return

                let r_brace = tokens.next_token()?;
                if r_brace.token() != Token::RBrace {
                    return Err(unexpected(r_brace, &[Token::Semi, Token::RBrace], false));
                }

                return Ok(Self {
                    ty: TypeId::Unknown,
                    open,
                    stmts,
                    auto_return: true,
                    close: RBrace,
                });
            }
        }
        let close = tokens.parse()?;

        Ok(Self {
            ty: TypeId::Unknown,
            open,
            stmts,
            auto_return: false,
            close,
        })
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Init(Init),
    Set(Set),
    Cond(Cond),
    Loop(Loop),
    Expr(StmtExpr),
    Return(Return),
}

impl Parse for Stmt {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        match (
            tokens.top1().map(|t| t.token()),
            tokens.top2().map(|t| t.token()),
        ) {
            (Some(Token::Ident), Some(Token::Walrus | Token::Assign | Token::Comma)) => {
                let targets = tokens.parse()?;
                if tokens.peek1(Token::Walrus) {
                    Ok(Self::Init(Init {
                        targets,
                        walrus: tokens.parse()?,
                        exprs: tokens.parse()?,
                    }))
                } else {
                    Ok(Self::Set(Set {
                        targets,
                        assign: tokens.parse()?,
                        exprs: tokens.parse()?,
                    }))
                }
            }
            (Some(Token::If), _) => Ok(Self::Cond(tokens.parse()?)),
            (Some(Token::For), _) => Ok(Self::Loop(tokens.parse()?)),
            (Some(Token::Return), _) => Ok(Self::Return(tokens.parse()?)),
            _ => Ok(Self::Expr(tokens.parse()?)),
        }
    }
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cond {
    pub if_first: If,
    pub else_ifs: Vec<ElseIf>,
    pub else_last: Option<Else>,
}

impl Parse for Cond {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let if_first = tokens.parse()?;

        let mut else_ifs = Vec::new();
        let mut else_last = None;
        while tokens.peek1(Token::Else) {
            let else_token: token::Else = tokens.parse()?;

            if !tokens.peek1(Token::If) {
                // the last check
                else_last = Some(Else {
                    else_token,
                    block: tokens.parse()?,
                });
                break;
            }

            else_ifs.push(ElseIf {
                else_token,
                inner: tokens.parse()?,
            });
        }

        Ok(Self {
            if_first,
            else_ifs,
            else_last,
        })
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct If {
    pub if_token: token::If,
    pub check: Expr,
    pub block: Block,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElseIf {
    pub else_token: token::Else,
    pub inner: If,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Else {
    pub else_token: token::Else,
    pub block: Block,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct Loop {
    pub for_token: For,
    pub block: Block,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct Init {
    pub targets: CommaSeparated<Target>,
    pub walrus: Walrus,
    pub exprs: CommaSeparated<Expr>,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct Set {
    pub targets: CommaSeparated<Target>,
    pub assign: Assign,
    pub exprs: CommaSeparated<Expr>,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Return {
    pub ty: TypeId,
    pub return_kw: token::Return,
    pub expr: Option<Expr>,
}

impl Parse for Return {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let return_kw = tokens.parse()?;
        let expr = if tokens.peek1(Token::Semi) {
            None
        } else {
            Some(tokens.parse()?)
        };

        Ok(Return {
            ty: TypeId::Unknown,
            return_kw,
            expr,
        })
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct StmtExpr {
    pub expr: Expr,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub ty: TypeId,
    pub expr: AnyExpr,
}

// impl AstDisplay for Expr {
//     fn fmt(&self, f: &mut fmt::Formatter, i: Indent) -> fmt::Result {

//     }
// }

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // order of ops:
    // 1: fn call (unary op)

    // TODO: 2: unary ops

    // 3: * / %
    Mul,
    Div,
    Rem,

    // 4: + -
    Add,
    Sub,

    // TODO: 5: << >>

    // 6: < <= > >=
    Lt,
    Le,
    Gt,
    Ge,

    // 7: ==, !=
    Eq,
    Neq,

    // 11: &&
    And,

    // 12: ||
    Or,
}

impl BinaryOp {
    pub const fn as_str(self) -> &'static str {
        match self {
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Rem => "%",
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnyExpr {
    Block(Box<Block>),
    LitInt(LitInt),
    LitStr(LitStr),
    Load(Ident),

    Func(Func),

    Call(Box<Call>),

    Binary {
        op: BinaryOp,
        sides: Box<(Expr, Expr)>,
    },
}

impl From<AnyExpr> for Expr {
    fn from(expr: AnyExpr) -> Self {
        Self {
            ty: TypeId::Unknown,
            expr,
        }
    }
}

impl Parse for Expr {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        Self::parse_or(tokens)
    }
}

impl Expr {
    fn parse_or(tokens: &mut ParseStream) -> Result<Self> {
        let mut lhs: Self = Self::parse_and(tokens)?;

        while tokens.peek1(Token::Or) {
            tokens.next_token()?;
            let op = BinaryOp::Or;
            let sides = Box::new((lhs, Self::parse_and(tokens)?));

            lhs = Self::from(AnyExpr::Binary { op, sides });
        }

        Ok(lhs)
    }

    fn parse_and(tokens: &mut ParseStream) -> Result<Self> {
        let mut lhs: Self = Self::parse_eq_cmp(tokens)?;

        while tokens.peek1(Token::And) {
            tokens.next_token()?;
            let op = BinaryOp::And;
            let sides = Box::new((lhs, Self::parse_eq_cmp(tokens)?));

            lhs = Self::from(AnyExpr::Binary { op, sides });
        }

        Ok(lhs)
    }

    fn parse_eq_cmp(tokens: &mut ParseStream) -> Result<Self> {
        let mut lhs: Self = Self::parse_ord_cmp(tokens)?;

        while tokens.peek1(Token::Eq) | tokens.peek1(Token::Neq) {
            let op = match tokens.next_token()?.token() {
                Token::Eq => BinaryOp::Eq,
                Token::Neq => BinaryOp::Neq,
                _ => unreachable!(),
            };
            let sides = Box::new((lhs, Self::parse_ord_cmp(tokens)?));

            lhs = Self::from(AnyExpr::Binary { op, sides });
        }

        Ok(lhs)
    }

    fn parse_ord_cmp(tokens: &mut ParseStream) -> Result<Self> {
        let mut lhs: Self = Self::parse_math_expr(tokens)?;

        while tokens.peek1(Token::Lt)
            | tokens.peek1(Token::Le)
            | tokens.peek1(Token::Gt)
            | tokens.peek1(Token::Ge)
        {
            let op = match tokens.next_token()?.token() {
                Token::Lt => BinaryOp::Lt,
                Token::Le => BinaryOp::Le,
                Token::Gt => BinaryOp::Gt,
                Token::Ge => BinaryOp::Ge,
                _ => unreachable!(),
            };
            let sides = Box::new((lhs, Self::parse_math_expr(tokens)?));

            lhs = Self::from(AnyExpr::Binary { op, sides });
        }

        Ok(lhs)
    }

    fn parse_math_expr(tokens: &mut ParseStream) -> Result<Self> {
        let mut lhs: Self = Self::parse_math_term(tokens)?;

        while tokens.peek1(Token::Plus) | tokens.peek1(Token::Minus) {
            let op = match tokens.next_token()?.token() {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            };
            let sides = Box::new((lhs, Self::parse_math_term(tokens)?));

            lhs = Self::from(AnyExpr::Binary { op, sides });
        }

        Ok(lhs)
    }

    fn parse_math_term(tokens: &mut ParseStream) -> Result<Self> {
        let mut lhs: Self = Self::parse_math_call(tokens)?;

        while tokens.peek1(Token::Asterisk)
            | tokens.peek1(Token::Slash)
            | tokens.peek1(Token::Percent)
        {
            let op = match tokens.next_token()?.token() {
                Token::Asterisk => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                Token::Percent => BinaryOp::Rem,
                _ => unreachable!(),
            };
            let sides = Box::new((lhs, Self::parse_math_call(tokens)?));

            lhs = Self::from(AnyExpr::Binary { op, sides });
        }

        Ok(lhs)
    }

    fn parse_math_call(tokens: &mut ParseStream) -> Result<Self> {
        let mut lhs: Self = Self::parse_math_atom(tokens)?;

        while tokens.peek1(Token::LParen) {
            let func = lhs;
            let args_beg: token::LParen = tokens.parse()?;

            let args = if !tokens.peek1(Token::RParen) {
                Some(tokens.parse()?)
            } else {
                None
            };

            let args_end: token::RParen = tokens.parse()?;

            lhs = AnyExpr::Call(Box::new(Call {
                func,
                args_beg,
                args,
                args_end,
            }))
            .into()
        }

        // while tokens.peek1(Token::LParen) {
        //     let is_mul = tokens.peek1(Token::Asterisk);

        //     let expr = Box::new((lhs, Self::parse_math_atom(tokens)?));
        //     if is_mul {
        //         lhs = Self::Add(expr);
        //     } else {
        //         lhs = Self::Sub(expr);
        //     }
        // }

        Ok(lhs)
    }

    fn parse_math_atom(tokens: &mut ParseStream) -> Result<Self> {
        let mut look = tokens.look1();
        if look.peek(Token::LBrace) {
            Ok(AnyExpr::Block(tokens.parse()?).into())
        } else if look.peek(Token::LitInt) {
            Ok(AnyExpr::LitInt(tokens.parse()?).into())
        } else if look.peek(Token::LitStr) {
            Ok(AnyExpr::LitStr(tokens.parse()?).into())
        } else if look.peek(Token::Ident) {
            Ok(AnyExpr::Load(tokens.parse()?).into())
        } else if look.peek(Token::LParen) {
            let _: token::LParen = tokens.parse()?;
            let expr: Expr = tokens.parse()?;
            let _: token::RParen = tokens.parse()?;
            Ok(expr)
        } else if look.peek(Token::Fn) {
            Ok(AnyExpr::Func(tokens.parse()?).into())
        } else {
            Err(look.err())
        }
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Call {
    pub func: Expr,
    pub args_beg: token::LParen,
    pub args: Option<CommaSeparated<Expr>>,
    pub args_end: token::RParen,
}

impl Call {
    pub fn args(&self) -> impl ExactSizeIterator<Item = &Expr> {
        OptionInner {
            inner: self.args.as_ref().map(|s| s.iter()),
        }
    }

    pub fn args_mut(&mut self) -> impl ExactSizeIterator<Item = &mut Expr> {
        OptionInner {
            inner: self.args.as_mut().map(|s| s.iter_mut()),
        }
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Proto {
    pub ty: TypeId,
    pub fn_kw: token::Fn,
    pub args_beg: token::LParen,
    pub args: Option<CommaSeparated<Argument>>,
    pub args_end: token::RParen,
    pub return_ty: Option<(token::RArrow, Ident)>,
}

impl Proto {
    pub fn args(&self) -> impl ExactSizeIterator<Item = &Argument> + Clone {
        OptionInner {
            inner: self.args.as_ref().map(|s| s.iter()),
        }
    }

    pub fn args_mut(&mut self) -> impl ExactSizeIterator<Item = &mut Argument> {
        OptionInner {
            inner: self.args.as_mut().map(|s| s.iter_mut()),
        }
    }
}

impl Parse for Proto {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let fn_kw = tokens.parse()?;
        let _args_beg = tokens.parse()?;

        let mut look = tokens.look1();
        let args = if look.peek(Token::Ident) {
            Some(tokens.parse()?)
        } else if look.peek(Token::RParen) {
            None
        } else {
            return Err(look.err());
        };

        let _args_end = tokens.parse()?;

        let mut look = tokens.look1();
        let return_ty = if look.peek(Token::RArrow) {
            Some((tokens.parse()?, tokens.parse()?))
        } else if look.peek(Token::LBrace) {
            None
        } else {
            return Err(look.err());
        };

        Ok(Proto {
            ty: TypeId::Unknown,
            fn_kw,
            args_beg: _args_beg,
            args,
            args_end: _args_end,
            return_ty,
        })
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct Func {
    pub proto: Proto,
    pub block: Block,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct Argument {
    pub id: Ident,
    pub colon: token::Colon,
    pub ty: Ident,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub value: String,
}

impl Parse for Ident {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let tok = tokens.expect_next(Token::Ident)?;
        let value = tok.as_str().to_string();

        Ok(Ident { value })
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LitStr {
    pub value: String,
}

impl Parse for LitStr {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let tok = tokens.expect_next(Token::LitStr)?;
        let tok = tok.as_str();
        let value = tok[1..tok.len() - 1].to_string();

        Ok(LitStr { value })
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LitInt {
    pub value: i128,
}

impl Parse for LitInt {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let tok = tokens.expect_next(Token::LitInt)?;
        let value = tok.as_str().parse().expect("this is a bug in the lexer");

        Ok(LitInt { value })
    }
}
