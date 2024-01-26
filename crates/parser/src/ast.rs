use crate::{unexpected, Parse, ParseStream, Result, SingleToken, Token};
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

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RootItem {
    Init(Init),
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
pub struct Init {
    pub targets: Targets,
    pub walrus: Walrus,
    pub expr: Expr,
    pub semi: Semi,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct Targets {
    pub inner: CommaSeparated<Target>,
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
    pub close: RBrace,
}

impl Parse for Block {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let open = tokens.parse()?;
        let mut stmts = Vec::new();
        while !tokens.peek1(Token::RBrace) {
            stmts.push(tokens.parse()?);
        }
        let close = tokens.parse()?;

        Ok(Self { open, stmts, close })
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Init(Init),
    Expr(Expr),
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
            _ => Ok(Self::Expr(tokens.parse()?)),
        }
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Block(Box<Block>),
    LitInt(LitInt),
    Load(Ident),

    Func(CommaSeparated<Ident>),

    Add(Box<(Expr, Expr)>),
    Sub(Box<(Expr, Expr)>),
    Mul(Box<(Expr, Expr)>),
    Div(Box<(Expr, Expr)>),
}

impl Expr {
    fn parse_math_expr(tokens: &mut ParseStream) -> Result<Self> {
        println!("parse_math_expr");
        let mut lhs: Self = Self::parse_math_term(tokens)?;

        while tokens.peek1(Token::Plus) | tokens.peek1(Token::Minus) {
            let is_add = tokens.peek1(Token::Plus);

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
        println!("parse_math_term");
        let mut lhs: Self = Self::parse_math_atom(tokens)?;

        while tokens.peek1(Token::Asterisk) | tokens.peek1(Token::Slash) {
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
        println!("parse_math_atom");
        let mut look = tokens.look1();
        if look.peek(Token::LBrace) {
            Ok(Self::Block(tokens.parse()?))
        } else if look.peek(Token::LitInt) {
            Ok(Self::LitInt(tokens.parse()?))
        } else if look.peek(Token::Ident) {
            println!("top = Ident");
            Ok(Self::Load(tokens.parse()?))
        } else if look.peek(Token::LParen) {
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
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct Func {
    _args_beg: token::LParen,
    args: CommaSeparated<Argument>,
    _args_end: token::RParen,
    arrow: token::RArrow,
    block: Block,
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
        println!("LitStr {tok}");
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
