use lexer::Token;
use macros::Parse;

use crate::{Parse, ParseStream, Result, SingleToken};

#[cfg(test)]
use serde::Serialize;

//

macro_rules! simple_tokens {
    ($($(#[$($t:tt)*])? $variant:ident),* $(,)?) => {$(
        $(#[$($t)*])?
        #[cfg_attr(test, derive(Serialize))]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
        #[token(Token::$variant)]
        pub struct $variant;
    )*};
}

#[macro_export]
macro_rules! Token {
    (;) => {
        <$crate::ast::token::Semi>
    };
}

//

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

    /// `:=`
    Walrus,

    /// `->`
    RArrow,

    /// `struct`
    Struct,

    /// `test`
    Test,

    /// end of input
    Eoi,
}

// #[cfg_attr(test, derive(Serialize))]
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
// #[token(Token::Comma)]
// pub struct Comma;

// #[cfg_attr(test, derive(Serialize))]
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
// #[token(Token::Eoi)]
// pub struct Eoi;
