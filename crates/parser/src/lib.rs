use std::{collections::VecDeque, fmt, iter::Filter};

use lexer::{Lexer, Span, SpannedToken, Token, Unexpected};

#[cfg(test)]
use serde::Serialize;

//

pub mod ast;

//

pub type Result<T, E = Error> = core::result::Result<T, E>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Lexer(lexer::Error),
    UnexpectedToken(lexer::Unexpected<'static, Token>),
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Lexer(err) => fmt::Display::fmt(err, f),
            Error::UnexpectedToken(err) => fmt::Display::fmt(err, f),
        }
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, Default)]
#[repr(u8)]
pub enum Value {
    Bool(bool),
    I32(i32),
    Str(&'static str),
    Void,
    Never,
    #[default]
    Unknown,
    Other,
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, Default)]
#[repr(u8)]
pub enum TypeId {
    Bool,
    I32,
    Str,
    Void,
    Never, // never matches everything
    #[default]
    Unknown,
    Other(u32),
}

impl PartialEq for TypeId {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeId::Bool, TypeId::Bool) => true,
            (TypeId::I32, TypeId::I32) => true,
            (TypeId::Str, TypeId::Str) => true,
            (TypeId::Void, TypeId::Void) => true,
            (TypeId::Never, _) => true,
            (_, TypeId::Never) => true,
            (TypeId::Unknown, TypeId::Unknown) => true,
            (TypeId::Other(l), TypeId::Other(r)) => l == r,
            _ => false,
        }
    }
}

impl Eq for TypeId {}

impl TypeId {
    // pub fn from_type<T: AsTypeId>() -> Self {
    //     T::TYPE_ID
    // }

    /// Returns `true` if the type id is [`I32`].
    ///
    /// [`I32`]: TypeId::I32
    #[must_use]
    pub fn is_i32(&self) -> bool {
        matches!(self, Self::I32)
    }

    /// Returns `true` if the type id is [`Void`].
    ///
    /// [`Void`]: TypeId::Void
    #[must_use]
    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    /// Returns `true` if the type id is [`Unknown`].
    ///
    /// [`Unknown`]: TypeId::Unknown
    #[must_use]
    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }
}

impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeId::Bool => write!(f, "bool"),
            TypeId::I32 => write!(f, "i32"),
            TypeId::Str => write!(f, "str"),
            TypeId::Void => write!(f, "void"),
            TypeId::Never => write!(f, "!"),
            TypeId::Unknown => write!(f, "unknown"),
            TypeId::Other(id) => write!(f, "[{id}]"),
        }
    }
}

//

fn skip_comments(token: &lexer::Result<SpannedToken>) -> bool {
    let Ok(token) = token else { return true };
    token.token() != Token::LineComment
}

//

#[derive(Debug)]
pub enum ParseStream<'a> {
    Lexer {
        lexer: Filter<Lexer<'a>, fn(&lexer::Result<SpannedToken>) -> bool>,
        peek: VecDeque<SpannedToken>,
        source: &'a str,
    },
    Buffer {
        buffer: &'a [SpannedToken],
        source: &'a str,
    },
}

impl<'a> ParseStream<'a> {
    const EOI: SpannedToken = SpannedToken::empty(Token::Eoi);

    pub const fn new(buffer: &'a [SpannedToken], source: &'a str) -> Self {
        Self::Buffer { buffer, source }
    }

    pub const fn source(&self) -> &'a str {
        match self {
            ParseStream::Lexer { source, .. } => source,
            ParseStream::Buffer { source, .. } => source,
        }
    }

    pub fn from_lexer(lexer: Lexer<'a>) -> Self {
        Self::Lexer {
            source: lexer.source(),
            lexer: lexer.filter(skip_comments as _),
            peek: VecDeque::new(),
        }
    }

    pub fn look1(&mut self) -> Look {
        Look {
            token: self.top1().copied().unwrap_or(Self::EOI),
            arr: [Token::Eoi; 8],
            len: 0,
        }
    }

    pub fn look2(&mut self) -> Look {
        Look {
            token: self.top2().copied().unwrap_or(Self::EOI),
            arr: [Token::Eoi; 8],
            len: 0,
        }
    }

    pub fn peek1(&mut self, token: Token) -> bool {
        Some(token) == self.top1().map(|tok| tok.token())
    }

    pub fn peek2(&mut self, token: Token) -> bool {
        Some(token) == self.top1().map(|tok| tok.token())
    }

    pub fn next_token(&mut self) -> Result<SpannedToken> {
        Ok(self.next().transpose()?.unwrap_or(Self::EOI))
    }

    pub fn expect_next(&mut self, token: Token) -> Result<SpannedToken> {
        let tok = self.next_token()?;
        if tok.token() == token {
            Ok(tok)
        } else {
            Err(unexpected(tok, &[token], false))
        }
    }

    fn top1(&mut self) -> Option<&'_ SpannedToken> {
        match self {
            ParseStream::Lexer { lexer, peek, .. } => {
                if peek.is_empty() {
                    let next = lexer.next()?.unwrap_or(Self::EOI);
                    peek.push_back(next);
                }

                peek.front()
            }
            ParseStream::Buffer { buffer, .. } => buffer.first(),
        }
    }

    fn top2(&mut self) -> Option<&'_ SpannedToken> {
        match self {
            ParseStream::Lexer { lexer, peek, .. } => {
                if peek.len() < 2 {
                    let next = lexer.next()?.unwrap_or(Self::EOI);
                    peek.push_back(next);
                }

                if peek.len() == 1 {
                    let next = lexer.next()?.unwrap_or(Self::EOI);
                    peek.push_back(next);
                }

                peek.get(1)
            }
            ParseStream::Buffer { buffer, .. } => buffer.first(),
        }
    }

    #[allow(clippy::let_and_return)]
    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        // println!(
        //     "parsing {} (top: [{}, {}])",
        //     core::any::type_name::<T>(),
        //     self.top1().copied().unwrap_or(Self::EOI),
        //     self.top2().copied().unwrap_or(Self::EOI),
        // );

        let res = T::parse(self);

        // println!(
        //     " result {} = {:?}",
        //     core::any::type_name::<T>(),
        //     res.as_ref().map(|_| {})
        // );
        res
    }
}

impl<'a> Iterator for ParseStream<'a> {
    type Item = Result<SpannedToken>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ParseStream::Lexer { lexer, peek, .. } => {
                if let Some(next) = peek.remove(0) {
                    return Some(Ok(next));
                }

                let res = lexer.next()?;
                Some(res.map_err(Error::Lexer))
            }
            ParseStream::Buffer { buffer, .. } => {
                let (first, others) = buffer.split_first()?;
                *buffer = others;
                Some(Ok(*first))
            }
        }
    }
}

//

pub struct Look {
    token: SpannedToken,

    // expected one of: (overflow: ...)
    arr: [Token; 8],
    len: u8,
}

impl Look {
    pub fn peek(&mut self, expected: Token) -> bool {
        if self.token.token() == expected {
            return true;
        }

        if let Some(last) = self.arr.get_mut(self.len as usize) {
            *last = expected;
        }
        self.len += 1;

        false
    }

    pub fn err(self) -> Error {
        let arr = &self.arr[..self.arr.len().min(self.len as _)];
        let dots = self.len as usize > self.arr.len();

        unexpected(self.token, arr, dots)
    }
}

//

pub trait SingleToken: Sized {
    const TOKEN: Token;

    fn new(span: Span) -> Self;

    fn span(&self) -> Span;

    fn parse_single(tokens: &mut ParseStream) -> Result<Self> {
        let tok = tokens.next_token()?;
        if tok.token() != Self::TOKEN {
            return Err(unexpected(tok, &[Self::TOKEN], false));
        }

        Ok(Self::new(tok.span()))
    }
}

//

pub trait Parse: Sized {
    fn parse(tokens: &mut ParseStream) -> Result<Self>;
}

impl<T: Parse> Parse for Box<T> {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        Ok(Box::new(tokens.parse()?))
    }
}

impl<T: Parse + SingleToken> Parse for Option<T> {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        if tokens.peek1(T::TOKEN) {
            Ok(Some(T::parse_single(tokens)?))
        } else {
            Ok(None)
        }
    }
}

// impl<T: Parse> Parse for Vec<T> {
//     fn parse(tokens: &mut ParseStream) -> Result<Self> {
//         let mut result = Vec::new();

//         while let Some(next) = tokens.parse::<Option<T>>()? {
//             result.push(next);
//         }

//         Ok(result)
//     }
// }

//

pub fn unexpected(token: SpannedToken, arr: &[Token], dots: bool) -> Error {
    Error::UnexpectedToken(Unexpected::new("token", token.token(), arr.into(), dots))
}

//

#[cfg(test)]
mod tests {
    use lexer::Lexer;
    use serde::Serialize;

    use crate::{ast, Parse, ParseStream, Result};

    use insta::assert_yaml_snapshot;

    //

    fn parse<'a, T: Parse + Serialize + 'a>(str: &'a str) -> impl Serialize + 'a {
        let mut stream = ParseStream::from_lexer(Lexer::new(str));
        let ast: Result<ast::Ast<T>, _> = Parse::parse(&mut stream).map_err(|err| err.to_string());
        let extra = stream
            .next()
            .transpose()
            .map(|t| t.map(|t| t.as_str().to_string()))
            .map_err(|err| err.to_string());

        (str, ast, extra)
    }

    #[test]
    fn parse_test() {
        assert_yaml_snapshot!(parse::<ast::token::Comma>(","));
        assert_yaml_snapshot!(parse::<ast::Root>("val := "));
    }
}
