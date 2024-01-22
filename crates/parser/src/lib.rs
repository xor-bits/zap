use std::{any::Any, fmt, iter::Peekable, thread, time::Duration};

use lexer::{Lexer, Span, SpannedToken, Token};

//

pub mod ast;

//

pub type Result<T, E = Error> = core::result::Result<T, E>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Lexer(lexer::Error),
    UnexpectedToken(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Lexer(err) => fmt::Display::fmt(err, f),
            Error::UnexpectedToken(tok) => write!(f, "unexpected token {tok}"),
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

    pub fn look(&mut self) -> Look {
        Look {
            token: self.peek_2(),
            arr: [Token::Eoi; 8],
            len: 0,
        }
    }

    pub fn peek(&mut self, token: Token) -> bool {
        Some(token) == self.top().map(|tok| tok.token())
    }

    pub fn next_token(&mut self) -> Result<SpannedToken<'a>> {
        Ok(self
            .next()
            .transpose()?
            .unwrap_or_else(|| SpannedToken::from_whole_str(Token::Eoi, "")))
    }

    pub fn expect_next(&mut self, token: Token) -> Result<SpannedToken<'a>> {
        let tok = self
            .next()
            .transpose()?
            .unwrap_or_else(|| SpannedToken::from_whole_str(Token::Eoi, ""));

        if tok.token() == token {
            Ok(tok)
        } else {
            Err(unexpected(tok, &[token], false))
        }
    }

    pub fn next_if(&mut self, token: Token) -> Result<SpannedToken<'a>> {
        let tok = self.peek_2();
        if tok.token() == token {
            _ = self.next();
            Ok(tok)
        } else {
            Err(unexpected(tok, &[token], false))
        }
    }

    fn peek_2(&mut self) -> SpannedToken<'a> {
        match self {
            ParseStream::Lexer(lexer) => lexer.peek().cloned().transpose().ok().flatten(),
            ParseStream::Buffer(buffer) => buffer.first().cloned(),
        }
        .unwrap_or_else(|| SpannedToken::from_whole_str(Token::Eoi, ""))
    }

    fn top(&mut self) -> Option<&'_ SpannedToken<'_>> {
        match self {
            ParseStream::Lexer(lexer) => lexer.peek()?.as_ref().ok(),
            ParseStream::Buffer(buffer) => buffer.first(),
        }
    }

    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        println!("parsing {}", core::any::type_name::<T>());
        thread::sleep(Duration::from_millis(10));
        let res = T::parse(self);

        println!(
            " result {} = {:?}",
            core::any::type_name::<T>(),
            res.as_ref().map(|_| {})
        );
        res
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

pub struct Look<'a> {
    token: SpannedToken<'a>,

    // expected one of: (overflow: ...)
    arr: [Token; 8],
    len: u8,
}

impl<'a> Look<'a> {
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
    const SELF: Self;

    fn parse_single(tokens: &mut ParseStream) -> Result<Self> {
        let tok = tokens.next_token()?;
        if tok.token() != Self::TOKEN {
            return Err(unexpected(tok, &[Self::TOKEN], false));
        }

        Ok(Self::SELF)
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
        tokens.next_if(T::TOKEN)?;
        Ok(Some(T::SELF))
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
    use std::fmt::Write;

    // let unexpected = unexpected
    //     .token()
    //     .as_token_str()
    //     .map(str::to_string)
    //     .unwrap_or_else(|| {
    //         let mut s = format!("{unexpected:?}");
    //         s.make_ascii_lowercase();
    //         s
    //     });

    let mut output = String::new();

    write!(&mut output, "`{token}`, expected ").unwrap();

    match arr {
        [] => panic!("expected nothing"),
        [only] => write!(&mut output, "`{only}`").unwrap(),
        [slice @ .., last] => {
            output.push_str("one of `");
            for token in slice {
                write!(&mut output, "{token}, ").unwrap();
            }
            write!(&mut output, "{last}").unwrap();

            if dots {
                output.push_str(", ...");
            }
            output.push('`');
        }
    }

    Error::UnexpectedToken(output)
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
            .map(|t| t.map(|t| t.as_str().to_string()))
            .map_err(|err| err.to_string());

        (str, ast, extra)
    }

    #[test]
    fn parse_test() {
        assert_yaml_snapshot!(parse::<ast::Comma>(","));
        assert_yaml_snapshot!(parse::<ast::Root>("val := "));
    }
}
