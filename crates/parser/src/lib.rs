use std::{
    any::Any,
    fmt,
    iter::{Filter, Peekable, SkipWhile},
    mem::MaybeUninit,
    thread,
    time::Duration,
};

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

fn skip_comments(token: &lexer::Result<SpannedToken>) -> bool {
    let Ok(token) = token else { return true };
    token.token() != Token::LineComment
}

//

#[derive(Debug)]
pub enum ParseStream<'a> {
    Lexer {
        lexer: Filter<Lexer<'a>, fn(&lexer::Result<SpannedToken>) -> bool>,
        peek: StackVec<SpannedToken<'a>, 2>,
    },
    Buffer {
        buffer: &'a [SpannedToken<'a>],
    },
}

impl<'a> ParseStream<'a> {
    const EOI: SpannedToken<'static> = SpannedToken::from_whole_str(Token::Eoi, "");

    pub const fn new(buffer: &'a [SpannedToken<'a>]) -> Self {
        Self::Buffer { buffer }
    }

    pub fn from_lexer(lexer: Lexer<'a>) -> Self {
        Self::Lexer {
            lexer: lexer.filter(skip_comments as _),
            peek: StackVec::new(),
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

    fn top1(&mut self) -> Option<&'_ SpannedToken<'a>> {
        match self {
            ParseStream::Lexer { lexer, peek } => {
                if peek.is_empty() {
                    let next = lexer.next()?.unwrap_or(Self::EOI);
                    peek.push(next).unwrap();
                }

                peek.first()
            }
            ParseStream::Buffer { buffer } => buffer.first(),
        }
    }

    fn top2(&mut self) -> Option<&'_ SpannedToken<'a>> {
        match self {
            ParseStream::Lexer { lexer, peek } => {
                if peek.len() < 2 {
                    let next = lexer.next()?.unwrap_or(Self::EOI);
                    peek.push(next).unwrap();
                }

                if peek.len() == 1 {
                    let next = lexer.next()?.unwrap_or(Self::EOI);
                    peek.push(next).unwrap();
                }

                peek.as_slice().get(1)
            }
            ParseStream::Buffer { buffer } => buffer.first(),
        }
    }

    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        println!(
            "parsing {} (top: [{}, {}])",
            core::any::type_name::<T>(),
            self.top1().copied().unwrap_or(Self::EOI),
            self.top2().copied().unwrap_or(Self::EOI),
        );
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
            ParseStream::Lexer { lexer, peek } => {
                if let Some(next) = peek.remove(0) {
                    return Some(Ok(next));
                }

                let res = lexer.next()?;
                Some(res.map_err(Error::Lexer))
            }
            ParseStream::Buffer { buffer } => {
                let (first, others) = buffer.split_first()?;
                *buffer = others;
                Some(Ok(*first))
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
        if tokens.peek1(T::TOKEN) {
            _ = tokens.next();

            Ok(Some(T::SELF))
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

#[derive(Debug)]
pub struct StackVec<T, const CAP: usize> {
    arr: [MaybeUninit<T>; CAP],
    len: u8,
}

// impl<T: Clone, const CAP: usize> Clone for StackVec<T, CAP> {
//     fn clone(&self) -> Self {
//         todo!()
//     }
// }

impl<T, const CAP: usize> StackVec<T, CAP> {
    pub const fn new() -> Self {
        // SAFETY: all 'fields' (elements) are still MaybeUninit, so this is safe
        let arr: [MaybeUninit<T>; CAP] = unsafe { MaybeUninit::uninit().assume_init() };
        Self { arr, len: 0 }
    }

    pub fn push(&mut self, val: T) -> Result<(), T> {
        if self.len == u8::MAX {
            return Err(val);
        }

        let Some(slot) = self.arr.get_mut(self.len as usize) else {
            return Err(val);
        };

        slot.write(val);
        self.len += 1;

        Ok(())
    }

    pub fn pop(&mut self) -> Option<T> {
        self.len = self.len.checked_sub(1)?;
        let slot = &mut self.arr[self.len as usize];

        Some(unsafe { slot.assume_init_read() })
    }

    pub fn remove(&mut self, n: usize) -> Option<T> {
        // TODO: ring
        if n >= self.len as usize {
            return None;
        }

        // take the value from middle
        let value = unsafe { self.arr[n].assume_init_read() };

        // and fix the array
        self.arr[n..].rotate_left(1);
        self.len -= 1;

        Some(value)
    }

    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn first(&self) -> Option<&T> {
        if self.len == 0 {
            return None;
        }

        Some(unsafe { self.arr.first()?.assume_init_ref() })
    }

    pub fn first_mut(&mut self) -> Option<&mut T> {
        if self.len == 0 {
            return None;
        }

        Some(unsafe { self.arr.first_mut()?.assume_init_mut() })
    }

    pub fn last(&self) -> Option<&T> {
        if self.len == 0 {
            return None;
        }

        Some(unsafe { self.arr.get(self.len as usize - 1)?.assume_init_ref() })
    }

    pub fn last_mut(&mut self) -> Option<&mut T> {
        if self.len == 0 {
            return None;
        }

        Some(unsafe { self.arr.get_mut(self.len as usize - 1)?.assume_init_mut() })
    }

    pub fn as_slice(&self) -> &[T] {
        let slice = &self.arr[..self.len as usize];

        // TODO: MaybeUninit::slice_assume_init_ref is unstable
        let init_slice_ptr = slice as *const [MaybeUninit<T>] as *const [T];
        unsafe { &*init_slice_ptr }
    }

    pub fn as_slice_mut(&mut self) -> &mut [T] {
        let slice = &mut self.arr[..self.len as usize];

        // TODO: MaybeUninit::slice_assume_init_mut is unstable
        let init_slice_ptr = slice as *mut [MaybeUninit<T>] as *mut [T];
        unsafe { &mut *init_slice_ptr }
    }
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
