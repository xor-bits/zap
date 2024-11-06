use lexer::{Span, Token};
use macros::Parse;

use crate::{Parse, ParseStream, Result, SingleToken};

use std::ops::Range;

#[cfg(test)]
use serde::Serialize;

//

macro_rules! simple_tokens {
    ($($(#[$($t:tt)*])? $variant:ident),* $(,)?) => {$(
        $(#[$($t)*])?
        #[cfg_attr(test, derive(Serialize))]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
        #[token(Token::$variant)]
        pub struct $variant(pub Span);
    )*};
}

simple_tokens! {
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

    /// `if`
    If,

    /// `else`
    Else,

    /// `for`
    For,

    /// `return`
    Return,

    /// `struct`
    Struct,

    /// `test`
    Test,

    /// end of input
    Eoi,
}
