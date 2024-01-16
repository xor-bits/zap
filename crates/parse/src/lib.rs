use std::{fmt, iter::Peekable};

use token::{Lexer, SpannedToken, Token};

//

pub mod ast;

//

pub type Result<T, E = Error> = core::result::Result<T, E>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Lexer(token::Error),
    UnexpectedToken(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Lexer(err) => fmt::Display::fmt(err, f),
            Error::UnexpectedToken(tok) => write!(f, "unexpected token `{tok}`"),
        }
    }
}

//

#[derive(Debug, Clone)]
pub enum ParseStream<'a> {
    Lexer(Peekable<Lexer<'a>>),
    Buffer(&'a [SpannedToken<'a>]),
}

impl<'a> ParseStream<'a> {
    pub const fn new(buffer: &'a [SpannedToken<'a>]) -> Self {
        Self::Buffer(buffer)
    }

    pub fn from_lexer(lexer: Lexer<'a>) -> Self {
        Self::Lexer(lexer.peekable())
    }

    pub fn peek(&mut self, token: Token) -> bool {
        Some(token) == self.top().map(|tok| tok.token())
    }

    fn top(&mut self) -> Option<&'_ SpannedToken<'_>> {
        match self {
            ParseStream::Lexer(lexer) => lexer.peek()?.as_ref().ok(),
            ParseStream::Buffer(buffer) => buffer.first(),
        }
    }

    fn top_unexpected(&mut self) -> Error {
        Error::UnexpectedToken(
            self.top()
                .map(|tok| tok.str().to_string())
                .unwrap_or_else(|| "eof".to_string()),
        )
    }

    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        println!("parsing {}", core::any::type_name::<T>());
        T::parse(self)
    }
}

impl<'a> Iterator for ParseStream<'a> {
    type Item = Result<SpannedToken<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ParseStream::Lexer(lexer) => Some(lexer.next()?.map_err(Error::Lexer)),
            ParseStream::Buffer(buffer) => {
                let (first, others) = buffer.split_first()?;
                *buffer = others;
                Some(Ok(first.clone()))
            }
        }
    }
}

//

pub trait SingleToken: Sized {
    const TOKEN: Token;
    const SELF: Self;

    fn parse_single(tokens: &mut ParseStream) -> Result<Self> {
        if tokens.peek(Self::TOKEN) {
            tokens.next();
            Ok(Self::SELF)
        } else {
            Err(tokens.top_unexpected())
        }
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

impl<T: Parse> Parse for Option<T> {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        // cloning the parser is really cheap
        // it would be `Copy` if it wasn't an iterator
        let mut fork = tokens.clone();
        if let Ok(next) = fork.parse() {
            *tokens = fork;
            Ok(Some(next))
        } else {
            Ok(None)
        }
    }
}

impl<T: Parse> Parse for Vec<T> {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let mut result = Vec::new();

        loop {
            // cloning the parser is really cheap
            // it would be `Copy` if it wasn't an iterator
            let mut fork = tokens.clone();
            if let Ok(next) = fork.parse() {
                result.push(next);
                *tokens = fork;
            } else {
                break;
            }
        }

        Ok(result)
    }
}

//

#[cfg(test)]
mod tests {
    use serde::Serialize;
    use token::Lexer;

    use crate::{ast, Parse, ParseStream, Result};

    use insta::assert_yaml_snapshot;

    //

    fn parse<'a, T: Parse + Serialize + 'a>(str: &'a str) -> impl Serialize + 'a {
        let mut stream = ParseStream::from_lexer(Lexer::new(str));
        let ast: Result<ast::Ast<T>, _> = Parse::parse(&mut stream).map_err(|err| err.to_string());
        let extra = stream
            .next()
            .transpose()
            .map(|t| t.map(|t| t.str().to_string()))
            .map_err(|err| err.to_string());

        (str, ast, extra)
    }

    #[test]
    fn parse_test() {
        assert_yaml_snapshot!(parse::<ast::Comma>(","));
    }
}
