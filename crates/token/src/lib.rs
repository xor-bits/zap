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

    /// `->`
    Arrow,

    /// `:`
    Type,

    /// `@`
    Extern,

    /// integer literal like `0xFFu8`
    LitInt(i128),

    /// string literal like `"test"`
    LitStr,

    /// end of input
    Eoi,
}

impl Token {
    pub const fn enumerate() -> &'static [Self] {
        &[
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
            Token::Arrow,
            Token::Type,
            Token::Extern,
            Token::LitInt(0),
            Token::LitStr,
            Token::Eoi,
        ]
    }

    pub const fn as_token_str(self) -> Option<&'static str> {
        Some(match self {
            Token::Struct => "struct",
            Token::Test => "test",
            Token::Ident => return None,
            Token::Semi => ";",
            Token::Init => ":=",
            Token::Assign => "=",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::Lbrace => "{",
            Token::Rbrace => "}",
            Token::Arrow => "->",
            Token::Type => ":",
            Token::Extern => "@",
            Token::LitInt(_) => return None,
            Token::LitStr => return None,
            Token::Eoi => return None,
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
        s.serialize_field("range", &self.span())?;
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

    fn starts_with(&self, token: &str) -> Option<Span<'a>> {
        if !self.at.starts_with(token) {
            return None;
        }

        Some(self.span_to(token.len()))
    }

    fn try_match(&mut self, token: Token) -> Option<SpannedToken<'a>> {
        let span = self.starts_with(token.as_token_str()?)?;
        self.advance(&span);
        Some(SpannedToken { token, span })
    }

    fn try_match_str(&mut self) -> Option<SpannedToken<'a>> {
        if !self.at.starts_with('"') {
            return None;
        }

        let Some(term) = self.at[1..].find('"') else {
            self.err = Err(Error::UnexpectedEoi);
            return self.eoi();
        };

        let span = self.span_to(term + 2);
        self.advance(&span);
        Some(SpannedToken {
            token: Token::LitStr,
            span,
        })
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

        let span = self.span_to(term);
        self.advance(&span);
        Some(SpannedToken {
            token: Token::Ident,
            span,
        })
    }

    fn eoi(&mut self) -> Option<SpannedToken<'a>> {
        let token = self.eoi.take()?;
        Some(SpannedToken {
            token,
            span: self.span_to(0),
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = SpannedToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.eoi.as_ref()?;

        self.at = self.at.trim();
        println!("at:{}", self.at);
        some!(self.try_match(Token::Struct));
        some!(self.try_match(Token::Test));
        some!(self.try_match(Token::Semi));
        some!(self.try_match(Token::Init));
        some!(self.try_match(Token::Assign));
        some!(self.try_match(Token::LParen));
        some!(self.try_match(Token::RParen));
        some!(self.try_match(Token::Lbrace));
        some!(self.try_match(Token::Rbrace));
        some!(self.try_match(Token::Arrow));
        some!(self.try_match(Token::Type));
        some!(self.try_match(Token::Extern));

        some!(self.try_match_str());
        some!(self.try_match_ident());

        if !self.at.is_empty() {
            self.err = Err(Error::ExtraTokens);
        }

        let token = self.eoi.take()?;
        Some(SpannedToken {
            token,
            span: self.span_to(0),
        })
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
    }
}
