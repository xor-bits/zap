#[cfg(test)]
use serde::{ser::SerializeStruct, Serialize};
use std::{fmt, ops::Range};

//

macro_rules! some {
    ($v:expr) => {
        if let Some(v) = $v {
            return Some(v);
        }
    };
}

macro_rules! exhaustive {
    ($($tok:ident::$var:ident $({ $($t:tt)* })?,)*) => {{
        use crate::Token::*;

        #[allow(unreachable_code)]
        fn _assert_exhaustive() {
            match unreachable!() {
                $($var { .. } => {})*
            }
        }

        [
            $($tok::$var $({ $($t)* })?),*
        ]
    }};
}

//

pub type Result<T, E = Error> = core::result::Result<T, E>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    UnexpectedEoi,
    ExtraTokens,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::UnexpectedEoi => f.write_str("unexpected end of input"),
            Error::ExtraTokens => f.write_str("found extra tokens"),
        }
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    /// `;`
    Semi,

    /// `=`
    Assign,

    /// `(`
    LParen,

    /// `)`
    RParen,

    /// `{`
    LBrace,

    /// `}`
    RBrace,

    /// `+`
    Plus,

    /// `-`
    Minus,

    /// `*`
    Asterisk,

    /// `/`
    Slash,

    /// `:`
    Colon,

    /// `,`
    Comma,

    /// `@`
    At,

    /// `&`
    Ampersand,

    /// `<`
    Lt,

    /// `>`
    Gt,

    /// `<=`
    Le,

    /// `>=`
    Ge,

    /// `==`
    Eq,

    /// `!=`
    Neq,

    /// `:=`
    Walrus,

    /// `->`
    RArrow,

    /// `struct`
    Struct,

    /// `test`
    Test,

    /// single line comments like `// this is a comment`
    LineComment,

    /// identifiers like `_fn_name`
    Ident,

    /// floating point literal like `5.2`
    LitFloat,

    /// integer literal like `0xFFu8`
    LitInt,

    /// string literal like `"test"`
    LitStr,

    /// end of input
    Eoi,
}

impl Token {
    pub const fn enumerate() -> &'static [Self] {
        &exhaustive![
            Token::Semi,
            Token::Assign,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Plus,
            Token::Minus,
            Token::Asterisk,
            Token::Slash,
            Token::Colon,
            Token::Comma,
            Token::At,
            Token::Ampersand,
            Token::Lt,
            Token::Gt,
            Token::Le,
            Token::Ge,
            Token::Eq,
            Token::Neq,
            Token::Walrus,
            Token::RArrow,
            Token::Struct,
            Token::Test,
            Token::LineComment,
            Token::Ident,
            Token::LitFloat,
            Token::LitInt,
            Token::LitStr,
            Token::Eoi,
        ]
    }

    pub const fn as_token_str(self) -> Option<&'static str> {
        Some(match self {
            Token::Semi => ";",
            Token::Assign => "=",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Asterisk => "*",
            Token::Slash => "/",
            Token::Colon => ":",
            Token::Comma => ",",
            Token::At => "@",
            Token::Ampersand => "&",
            Token::Lt => "<",
            Token::Gt => ">",
            Token::Le => "<=",
            Token::Ge => ">=",
            Token::Eq => "==",
            Token::Neq => "!=",
            Token::Walrus => ":=",
            Token::RArrow => "->",
            Token::Struct => "struct",
            Token::Test => "test",
            Token::LineComment => return None,
            Token::Ident => return None,
            Token::LitFloat => return None,
            Token::LitInt => return None,
            Token::LitStr => return None,
            Token::Eoi => return None,
        })
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(s) = self.as_token_str() {
            f.write_str(s)
        } else {
            write!(f, "{self:?}")
        }
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpannedToken<'a> {
    token: Token,
    span: Span<'a>,
}

impl<'a> SpannedToken<'a> {
    pub fn from_whole_str(token: Token, str: &'a str) -> Self {
        Self {
            token,
            span: Span::from_whole_str(str),
        }
    }

    pub fn token(&self) -> Token {
        self.token
    }

    pub fn span(&self) -> Range<usize> {
        self.span.span()
    }

    pub fn as_str(&self) -> &'a str {
        self.span.as_str()
    }
}

impl fmt::Display for SpannedToken<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.token == Token::Eoi {
            write!(f, "<eoi>")
        } else {
            f.write_str(self.as_str())
        }
    }
}

//

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span<'a> {
    range: Range<usize>,
    source: &'a str,
}

impl<'a> Span<'a> {
    pub fn from_whole_str(str: &'a str) -> Self {
        Self {
            range: 0..str.len(),
            source: str,
        }
    }

    pub fn span(&self) -> Range<usize> {
        self.range.clone()
    }

    pub fn as_str(&self) -> &'a str {
        &self.source[self.span()]
    }
}

#[cfg(test)]
impl Serialize for Span<'_> {
    fn serialize<S>(&self, serializer: S) -> core::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut s = serializer.serialize_struct("SpannedToken", 3)?;
        s.serialize_field("range", &format!("{:?}", self.span()))?;
        s.serialize_field("str", &self.as_str())?;
        s.end()
    }
}

//

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    source: &'a str,
    at: &'a str,
    eoi: bool,
    err: bool,
}

impl<'a> Lexer<'a> {
    pub const fn new(source: &'a str) -> Self {
        Self {
            source,
            at: source,
            eoi: false,
            err: false,
        }
    }

    // pub const fn error(&self) -> Result<()> {
    //     self.err
    // }

    fn src_index(&self) -> usize {
        self.at.as_ptr() as usize - self.source.as_ptr() as usize
    }

    fn advance_by(&mut self, len: usize) {
        self.at = &self.at[len..];
    }

    fn advance(&mut self, span: &Span) {
        self.advance_by(span.range.len())
    }

    fn span_to(&self, len: usize) -> Span<'a> {
        let at = self.src_index();
        Span {
            range: at..at + len,
            source: self.source,
        }
    }

    fn spanned_token_to(&mut self, token: Token, len: usize) -> Option<Result<SpannedToken<'a>>> {
        let span = self.span_to(len);
        self.advance(&span);
        Some(Ok(SpannedToken { token, span }))
    }

    fn try_match(&mut self, token: Token) -> Option<Result<SpannedToken<'a>>> {
        let str = token.as_token_str()?;
        if !self.at.starts_with(str) {
            return None;
        }

        self.spanned_token_to(token, str.len())
    }

    fn try_match_number(&mut self) -> Option<Result<SpannedToken<'a>>> {
        let first = self.at.chars().next()?;

        if !first.is_ascii_digit() {
            return None;
        }

        let mut dot_found = false;
        let term = self.at[1..]
            .find(|c: char| {
                !c.is_ascii_digit()
                    || if dot_found {
                        false
                    } else if c == '.' {
                        dot_found = true;
                        true
                    } else {
                        false
                    }
            })
            .map(|term| term + 1)
            .unwrap_or(self.at.len());

        let token = if dot_found {
            Token::LitFloat
        } else {
            Token::LitInt
            // let num = self.at[..term].parse::<i128>().unwrap(); // the unwrap should never fail
            // Token::LitInt(num)
        };

        self.spanned_token_to(token, term)
    }

    fn try_match_str(&mut self) -> Option<Result<SpannedToken<'a>>> {
        if !self.at.starts_with('"') {
            return None;
        }

        let Some(term) = self.at[1..].find('"') else {
            return self.err(Error::UnexpectedEoi);
        };

        self.spanned_token_to(Token::LitStr, term + 2)
    }

    fn try_match_ident(&mut self) -> Option<Result<SpannedToken<'a>>> {
        let first = self.at.chars().next()?;

        if !(first == '_' || first.is_alphabetic()) {
            return None;
        }

        let term = self.at[1..]
            .find(|c: char| !c.is_alphanumeric())
            .map(|term| term + 1)
            .unwrap_or(self.at.len());

        self.spanned_token_to(Token::Ident, term)
    }

    fn try_match_comment(&mut self) -> Option<Result<SpannedToken<'a>>> {
        if !self.at.starts_with("//") {
            return None;
        }

        let term = self.at[2..]
            .lines()
            .next()
            .map(|line| line.len() + 2)
            .unwrap_or(self.at.len());

        self.spanned_token_to(Token::LineComment, term)
    }

    fn err(&mut self, err: Error) -> Option<Result<SpannedToken<'a>>> {
        self.eoi = true;
        Some(Err(err))
    }

    fn eoi(&mut self) -> Option<Result<SpannedToken<'a>>> {
        self.eoi = true;
        self.spanned_token_to(Token::Eoi, 0)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<SpannedToken<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eoi {
            return None;
        }
        if self.err {
            return self.eoi();
        }
        self.at = self.at.trim();

        // tokens in the order of 'more specific -> less specific' to emulate priority
        // for example := should be parsed as Init instead of Type and Assign

        // custom tokens
        some!(self.try_match_str());
        some!(self.try_match_comment());
        some!(self.try_match_number());

        // simple const str tokens
        for (_, token) in Token::enumerate()
            .iter()
            .rev()
            .copied()
            .filter(|tok| tok.as_token_str().is_some())
            // just a debug assert to make sure that the tokens are in the correct order
            .scan(usize::MAX, |prev, now| {
                let len = now.as_token_str().map(str::len).unwrap_or(0);
                debug_assert!(*prev >= len);
                Some((len, now))
            })
        {
            some!(self.try_match(token));
        }

        // idents after keywords
        some!(self.try_match_ident());

        if !self.at.is_empty() {
            return self.err(Error::ExtraTokens);
        }

        self.eoi()
    }
}

//

#[cfg(test)]
mod tests {
    use crate::{Lexer, Result, SpannedToken, Token};

    use insta::assert_yaml_snapshot;
    use serde::Serialize;

    fn lex(str: &str) -> impl Serialize + '_ {
        let tokens: Result<Vec<SpannedToken>> = Lexer::new(str).collect();
        (str, tokens.map_err(|err| err.to_string()))
    }

    #[test]
    fn lex_test() {
        assert_yaml_snapshot!(lex(
            "in->\"vali\"@->:d synt(ax} {but) t;h:e l=e:=xer should handle it"
        ));
    }

    #[test]
    fn lex_str_lit() {
        assert_yaml_snapshot!(lex("_a2"));
        assert_yaml_snapshot!(lex(""));
        assert_yaml_snapshot!(lex("\""));
        assert_yaml_snapshot!(lex("\"a"));
        assert_yaml_snapshot!(lex("\"a\""));
    }

    #[test]
    fn lex_all_simple() {
        let all_tokens: String = Token::enumerate()
            .iter()
            .filter_map(|tok| tok.as_token_str())
            .collect();

        assert_yaml_snapshot!(lex(&all_tokens));
    }

    #[test]
    fn lex_comment() {
        assert_yaml_snapshot!(lex("// comment\ncode"));
    }

    #[test]
    fn lex_example() {
        assert_yaml_snapshot!(lex(include_str!("../../../tests/trivial")));
    }
}
