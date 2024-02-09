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

    /// `%`
    Percent,

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

    /// `&&`
    And,

    /// `||`
    Or,

    /// `:=`
    Walrus,

    /// `->`
    RArrow,

    /// `fn`
    Fn,

    /// `for`
    For,

    /// `return`
    Return,

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
            Token::Percent,
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
            Token::And,
            Token::Or,
            Token::Walrus,
            Token::RArrow,
            Token::Fn,
            Token::For,
            Token::Return,
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

    pub const fn token_type(self) -> TokenType {
        match self {
            Token::Semi => TokenType::Symbols(";"),
            Token::Assign => TokenType::Symbols("="),
            Token::LParen => TokenType::Symbols("("),
            Token::RParen => TokenType::Symbols(")"),
            Token::LBrace => TokenType::Symbols("{"),
            Token::RBrace => TokenType::Symbols("}"),
            Token::Plus => TokenType::Symbols("+"),
            Token::Minus => TokenType::Symbols("-"),
            Token::Asterisk => TokenType::Symbols("*"),
            Token::Slash => TokenType::Symbols("/"),
            Token::Percent => TokenType::Symbols("%"),
            Token::Colon => TokenType::Symbols(":"),
            Token::Comma => TokenType::Symbols(","),
            Token::At => TokenType::Symbols("@"),
            Token::Ampersand => TokenType::Symbols("&"),
            Token::Lt => TokenType::Symbols("<"),
            Token::Gt => TokenType::Symbols(">"),
            Token::Le => TokenType::Symbols("<="),
            Token::Ge => TokenType::Symbols(">="),
            Token::Eq => TokenType::Symbols("=="),
            Token::Neq => TokenType::Symbols("!="),
            Token::And => TokenType::Symbols("&&"),
            Token::Or => TokenType::Symbols("||"),
            Token::Walrus => TokenType::Symbols(":="),
            Token::RArrow => TokenType::Symbols("->"),
            Token::Fn => TokenType::Keyword("fn"),
            Token::For => TokenType::Keyword("for"),
            Token::Return => TokenType::Keyword("return"),
            Token::Struct => TokenType::Keyword("struct"),
            Token::Test => TokenType::Keyword("test"),
            Token::LineComment => TokenType::Other,
            Token::Ident => TokenType::Other,
            Token::LitFloat => TokenType::Other,
            Token::LitInt => TokenType::Other,
            Token::LitStr => TokenType::Other,
            Token::Eoi => TokenType::Other,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.token_type() {
            TokenType::Symbols(s) => f.write_str(s),
            TokenType::Keyword(s) => f.write_str(s),
            TokenType::Other => write!(f, "{self:?}"),
        }
    }
}

//

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    Symbols(&'static str),
    Keyword(&'static str),
    Other,
}

//

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SpannedToken<'a> {
    token: Token,
    span: Span<'a>,
}

impl<'a> SpannedToken<'a> {
    pub const fn from_whole_str(token: Token, str: &'a str) -> Self {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span<'a> {
    from: usize,
    to: usize,
    source: &'a str,
}

impl<'a> Span<'a> {
    pub const fn from_whole_str(str: &'a str) -> Self {
        Self {
            from: 0,
            to: str.len(),
            source: str,
        }
    }

    pub fn span(&self) -> Range<usize> {
        self.from..self.to
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
        self.advance_by(span.span().len())
    }

    fn span_to(&self, len: usize) -> Span<'a> {
        let at = self.src_index();
        Span {
            from: at,
            to: at + len,
            source: self.source,
        }
    }

    fn spanned_token_to(&mut self, token: Token, len: usize) -> Option<Result<SpannedToken<'a>>> {
        let span = self.span_to(len);
        self.advance(&span);
        Some(Ok(SpannedToken { token, span }))
    }

    fn try_match(&mut self, token: Token) -> Option<Result<SpannedToken<'a>>> {
        let TokenType::Symbols(str) = token.token_type() else {
            return None;
        };
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
            .find(|c: char| !(c.is_alphanumeric() || c == '_'))
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
            .filter_map(|tok| match tok.token_type() {
                TokenType::Symbols(s) => Some((tok, s)),
                _ => None,
            })
            // just a debug assert to make sure that the tokens are in the correct order
            .scan(usize::MAX, |prev, (now, s)| {
                debug_assert!(*prev >= s.len());
                Some((s.len(), now))
            })
        {
            some!(self.try_match(token));
        }

        // idents after keywords
        if let Some(ident) = self.try_match_ident() {
            let ident = match ident {
                Ok(ident) => ident,
                Err(err) => return Some(Err(err)),
            };

            for (s, token) in
                Token::enumerate()
                    .iter()
                    .rev()
                    .copied()
                    .filter_map(|tok| match tok.token_type() {
                        TokenType::Keyword(s) => Some((s, tok)),
                        _ => None,
                    })
            {
                if ident.as_str() == s {
                    return Some(Ok(SpannedToken {
                        token,
                        span: ident.span,
                    }));
                }
            }

            return Some(Ok(ident));
        }

        if !self.at.is_empty() {
            return self.err(Error::ExtraTokens);
        }

        self.eoi()
    }
}

//

#[cfg(test)]
mod tests {
    use crate::{Lexer, Result, SpannedToken, Token, TokenType};

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
            .filter_map(|tok| match tok.token_type() {
                TokenType::Symbols(s) | TokenType::Keyword(s) => Some(format!("{s} ")),
                TokenType::Other => None,
            })
            .collect();

        assert_yaml_snapshot!(lex(&all_tokens));
    }

    #[test]
    fn lex_all_no_spaces() {
        let all_tokens: String = Token::enumerate()
            .iter()
            .filter_map(|tok| match tok.token_type() {
                TokenType::Symbols(s) | TokenType::Keyword(s) => Some(s),
                TokenType::Other => None,
            })
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
