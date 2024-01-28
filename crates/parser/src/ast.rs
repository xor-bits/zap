use crate::{unexpected, Parse, ParseStream, Result, Token};
use macros::Parse;

use token::*;

#[cfg(test)]
use serde::Serialize;

//

pub mod token;

//

/// any part of the grammar, but terminated with EOI
#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
pub struct Ast<T> {
    pub inner: T,
    pub eoi: Eoi,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Root {
    pub inner: Vec<RootItem>,
}

impl Parse for Root {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let mut inner = Vec::new();
        loop {
            let mut look = tokens.look1();
            if look.peek(Token::Semi) {
                _ = tokens.next();
            } else if look.peek(Token::Eoi) {
                break;
            } else if look.peek(Token::Ident) || look.peek(Token::Test) {
                inner.push(tokens.parse()?);
            } else {
                return Err(look.err());
            }
        }
        Ok(Self { inner })
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RootItem {
    Init(RootInit),
    Test(Test),
}

impl Parse for RootItem {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let mut look = tokens.look1();
        if look.peek(Token::Ident) {
            Ok(Self::Init(tokens.parse()?))
        } else if look.peek(Token::Test) {
            Ok(Self::Test(tokens.parse()?))
        } else {
            Err(look.err())
        }
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct RootInit {
    pub targets: CommaSeparated<Target>,
    pub walrus: Walrus,
    pub exprs: CommaSeparated<Expr>,
}

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
    pub fn iter(&self) -> CommaSeparatedIter<T> {
        CommaSeparatedIter {
            first: Some(&self.first),
            inner: &self.inner[..],
        }
    }
}

//

pub struct CommaSeparatedIter<'a, T> {
    first: Option<&'a T>,
    inner: &'a [CommaSeparatedItem<T>],
}

impl<'a, T> Iterator for CommaSeparatedIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(first) = self.first.take() {
            return Some(first);
        }

        let (first, inner) = self.inner.split_first()?;
        self.inner = inner;
        Some(&first.item)
    }
}

impl<'a, T> ExactSizeIterator for CommaSeparatedIter<'a, T> {
    fn len(&self) -> usize {
        self.inner.len() + if self.first.is_some() { 1 } else { 0 }
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
                    open,
                    stmts,
                    auto_return: true,
                    close: RBrace,
                });
            }
        }
        let close = tokens.parse()?;

        Ok(Self {
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
                Ok(Self::Init(tokens.parse()?))
            }
            (Some(Token::Return), _) => Ok(Self::Return(tokens.parse()?)),
            _ => Ok(Self::Expr(tokens.parse()?)),
        }
    }
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
pub struct Return {
    pub return_kw: token::Return,
    pub expr: Expr,
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
pub enum Expr {
    Block(Box<Block>),
    LitInt(LitInt),
    LitStr(LitStr),
    Load(Ident),

    Func(Func),

    Add(Box<(Expr, Expr)>),
    Sub(Box<(Expr, Expr)>),
    Mul(Box<(Expr, Expr)>),
    Div(Box<(Expr, Expr)>),
    Call(Box<Call>),
}

impl Expr {
    fn parse_math_expr(tokens: &mut ParseStream) -> Result<Self> {
        let mut lhs: Self = Self::parse_math_term(tokens)?;

        while tokens.peek1(Token::Plus) | tokens.peek1(Token::Minus) {
            let is_add = tokens.next_token()?.token() == Token::Plus;

            let expr = Box::new((lhs, Self::parse_math_term(tokens)?));
            if is_add {
                lhs = Self::Add(expr);
            } else {
                lhs = Self::Sub(expr);
            }
        }

        Ok(lhs)
    }

    fn parse_math_term(tokens: &mut ParseStream) -> Result<Self> {
        let mut lhs: Self = Self::parse_math_call(tokens)?;

        while tokens.peek1(Token::Asterisk) | tokens.peek1(Token::Slash) {
            let is_mul = tokens.next_token()?.token() == Token::Asterisk;

            let expr = Box::new((lhs, Self::parse_math_call(tokens)?));
            if is_mul {
                lhs = Self::Mul(expr);
            } else {
                lhs = Self::Div(expr);
            }
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

            lhs = Self::Call(Box::new(Call {
                func,
                args_beg,
                args,
                args_end,
            }))
        }

        while tokens.peek1(Token::LParen) {
            let is_mul = tokens.peek1(Token::Asterisk);

            let expr = Box::new((lhs, Self::parse_math_atom(tokens)?));
            if is_mul {
                lhs = Self::Add(expr);
            } else {
                lhs = Self::Sub(expr);
            }
        }

        Ok(lhs)
    }

    fn parse_math_atom(tokens: &mut ParseStream) -> Result<Self> {
        let mut look = tokens.look1();
        if look.peek(Token::LBrace) {
            Ok(Self::Block(tokens.parse()?))
        } else if look.peek(Token::LitInt) {
            Ok(Self::LitInt(tokens.parse()?))
        } else if look.peek(Token::LitStr) {
            Ok(Self::LitStr(tokens.parse()?))
        } else if look.peek(Token::Ident) {
            Ok(Self::Load(tokens.parse()?))
        } else if look.peek(Token::LParen) {
            let _: token::LParen = tokens.parse()?;
            let expr = tokens.parse()?;
            let _: token::RParen = tokens.parse()?;
            Ok(expr)
        } else if look.peek(Token::Fn) {
            Ok(Self::Func(tokens.parse()?))
        } else {
            Err(look.err())
        }
    }
}

impl Parse for Expr {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        Self::parse_math_expr(tokens)
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

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub fn_kw: token::Fn,
    pub args_beg: token::LParen,
    pub args: Option<CommaSeparated<Argument>>,
    pub args_end: token::RParen,
    pub return_ty: Option<(token::RArrow, Ident)>,
    pub block: Block,
}

impl Parse for Func {
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
        } else if look.peek(Token::RBrace) {
            None
        } else {
            return Err(look.err());
        };

        let block = tokens.parse()?;

        Ok(Func {
            fn_kw,
            args_beg: _args_beg,
            args,
            args_end: _args_end,
            return_ty,
            block,
        })
    }
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
