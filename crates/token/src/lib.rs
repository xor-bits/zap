#[cfg(test)]
use serde::{ser::SerializeStruct, Serialize};
use std::ops::Range;

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

pub type Result<T> = core::result::Result<T, Error>;

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy)]
pub enum Error {
    UnexpectedEoi,
    ExtraTokens,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy)]
pub enum Token {
    /// `struct`
    Struct,

    /// `test`
    Test,

    /// identifiers like `_fn_name`
    Ident,

    /// `;`
    Semi,

    /// `:=`
    Init,

    /// `=`
    Assign,

    /// `(`
    LParen,

    /// `)`
    RParen,

    /// `{`
    Lbrace,

    /// `}`
    Rbrace,

    /// `+`
    Plus,

    /// `-`
    Minus,

    /// `*`
    Asterisk,

    /// `/`
    Slash,

    /// `->`
    Arrow,

    /// `:`
    Type,

    /// `@`
    Extern,

    /// `//`
    LineComment,

    /// integer literal like `0xFFu8`
    LitInt(i128),

    /// string literal like `"test"`
    LitStr,

    /// end of input
    Eoi,
}

impl Token {
    pub const fn enumerate() -> &'static [Self] {
        &exhaustive![
            Token::Struct,
            Token::Test,
            Token::Ident,
            Token::Semi,
            Token::Init,
            Token::Assign,
            Token::LParen,
            Token::RParen,
            Token::Lbrace,
            Token::Rbrace,
            Token::Plus,
            Token::Minus,
            Token::Asterisk,
            Token::Slash,
            Token::Arrow,
            Token::Type,
            Token::Extern,
            Token::LineComment,
            Token::LitInt { 0: 0 },
            Token::LitStr,
            Token::Eoi,
        ]
    }

    pub const fn as_token_str(self) -> Option<&'static str> {
        Some(match self {
            Token::Struct {} => "struct",
            Token::Test => "test",
            Token::Ident => return None,
            Token::Semi => ";",
            Token::Init => ":=",
            Token::Assign => "=",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::Lbrace => "{",
            Token::Rbrace => "}",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Asterisk => "*",
            Token::Slash => "/",
            Token::Arrow => "->",
            Token::Type => ":",
            Token::Extern => "@",
            Token::LineComment { .. } => "//",
            Token::LitInt { .. } => return None,
            Token::LitStr => return None,
            Token::Eoi => "",
        })
    }
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone)]
pub struct SpannedToken<'a> {
    token: Token,
    span: Span<'a>,
}

impl<'a> SpannedToken<'a> {
    pub fn token(&self) -> Token {
        self.token
    }

    pub fn span(&self) -> Range<usize> {
        self.span.span()
    }

    pub fn str(&self) -> &'a str {
        self.span.str()
    }
}

//

#[derive(Debug, Clone)]
pub struct Span<'a> {
    range: Range<usize>,
    source: &'a str,
}

impl<'a> Span<'a> {
    pub fn span(&self) -> Range<usize> {
        self.range.clone()
    }

    pub fn str(&self) -> &'a str {
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
        s.serialize_field("str", &self.str())?;
        s.end()
    }
}

//

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    source: &'a str,
    at: &'a str,
    eoi: Option<Token>,
    err: Result<()>,
}

impl<'a> Lexer<'a> {
    pub const fn new(source: &'a str) -> Self {
        Self {
            source,
            at: source,
            eoi: Some(Token::Eoi),
            err: Ok(()),
        }
    }

    pub const fn error(&self) -> Result<()> {
        self.err
    }

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

    fn spanned_token_to(&mut self, token: Token, len: usize) -> SpannedToken<'a> {
        let span = self.span_to(len);
        self.advance(&span);
        SpannedToken { token, span }
    }

    fn try_match(&mut self, token: Token) -> Option<SpannedToken<'a>> {
        let str = token.as_token_str()?;
        if !self.at.starts_with(str) {
            return None;
        }

        Some(self.spanned_token_to(token, str.len()))
    }

    fn try_match_number(&mut self) -> Option<SpannedToken<'a>> {
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
                    } else {
                        dot_found = true;
                        c == '.'
                    }
            })
            .map(|term| term + 1)
            .unwrap_or(self.at.len());

        let token = if dot_found {
            todo!("LitFloat")
        } else {
            let num = self.at[..term].parse::<i128>().unwrap(); // the unwrap should never fail
            Token::LitInt(num)
        };

        Some(self.spanned_token_to(token, term))
    }

    fn try_match_str(&mut self) -> Option<SpannedToken<'a>> {
        if !self.at.starts_with('"') {
            return None;
        }

        let Some(term) = self.at[1..].find('"') else {
            self.err = Err(Error::UnexpectedEoi);
            return self.eoi();
        };

        Some(self.spanned_token_to(Token::LitStr, term + 2))
    }

    fn try_match_ident(&mut self) -> Option<SpannedToken<'a>> {
        let first = self.at.chars().next()?;

        if !(first == '_' || first.is_alphabetic()) {
            return None;
        }

        let term = self.at[1..]
            .find(|c: char| !c.is_alphanumeric())
            .map(|term| term + 1)
            .unwrap_or(self.at.len());

        Some(self.spanned_token_to(Token::Ident, term))
    }

    fn try_match_comment(&mut self) -> Option<SpannedToken<'a>> {
        if !self.at.starts_with("//") {
            return None;
        }

        let term = self.at[2..]
            .lines()
            .next()
            .map(|line| line.len() + 2)
            .unwrap_or(self.at.len());

        Some(self.spanned_token_to(Token::LineComment, term))
    }

    fn eoi(&mut self) -> Option<SpannedToken<'a>> {
        let eoi = self.eoi.take()?;
        Some(self.spanned_token_to(eoi, 0))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = SpannedToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.eoi.as_ref()?;
        self.at = self.at.trim();

        // keywords first, so that keywords have a priority over idents
        some!(self.try_match(Token::Struct));
        some!(self.try_match(Token::Test));

        // custom tokens
        some!(self.try_match_str());
        some!(self.try_match_ident());
        some!(self.try_match_comment());
        some!(self.try_match_number());

        // simple tokens in the order of more specific -> less specific
        // length of 2
        some!(self.try_match(Token::Init));
        some!(self.try_match(Token::Arrow));
        // length of 1
        some!(self.try_match(Token::Semi));
        some!(self.try_match(Token::Assign));
        some!(self.try_match(Token::LParen));
        some!(self.try_match(Token::RParen));
        some!(self.try_match(Token::Lbrace));
        some!(self.try_match(Token::Rbrace));
        some!(self.try_match(Token::Type));
        some!(self.try_match(Token::Extern));
        some!(self.try_match(Token::Plus));
        some!(self.try_match(Token::Minus));
        some!(self.try_match(Token::Asterisk));
        some!(self.try_match(Token::Slash));

        if !self.at.is_empty() {
            self.err = Err(Error::ExtraTokens);
        }

        self.eoi()
    }
}

//

#[cfg(test)]
mod tests {
    use crate::{Lexer, Result, SpannedToken, Token};

    use insta::assert_yaml_snapshot;

    fn lex(str: &str) -> (&str, Result<()>, Vec<SpannedToken>) {
        let mut lexer = Lexer::new(str);
        let tokens: Vec<_> = lexer.by_ref().collect();
        (str, lexer.error(), tokens)
    }

    #[test]
    fn lex_test() {
        assert_yaml_snapshot!(lex(
            "in->\"vali\"@->:d synt(ax} {but) t;h:e l=e:=xer should handle it"
        ));

        assert_yaml_snapshot!(lex("_a2"));
        assert_yaml_snapshot!(lex(""));
        assert_yaml_snapshot!(lex("\""));
        assert_yaml_snapshot!(lex("\"a"));
        assert_yaml_snapshot!(lex("\"a\""));

        let all_tokens: String = Token::enumerate()
            .iter()
            .filter_map(|tok| tok.as_token_str())
            .collect();

        assert_yaml_snapshot!(lex(&all_tokens));

        assert_yaml_snapshot!(lex("// comment\ncode"));

        assert_yaml_snapshot!(lex(include_str!("../../../tests/trivial")));
    }
}
