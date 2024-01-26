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

// #[macro_export]
// #[rustfmt::skip]
// macro_rules! Tok {
//     (;) => { ast::token::Semi };
//     (=) => { ast::token::Assign };
//     // (() => { ast::token::};
//     // ()) => { ast::token::};
//     // ({) => { ast::token::};
//     // (}) => { ast::token::};
//     (+) => { ast::token::Plus };
//     (-) => { ast::token::Minus };
//     (*) => { ast::token::Asterisk };
//     (/) => { ast::token::Slash };
//     (:) => { ast::token::Colon };
//     (,) => { ast::token::Comma };
//     (@) => { ast::token::At };
//     (&) => { ast::token::Ampersand };
//     (<) => { ast::token::Lt };
//     (>) => { ast::token::Gt };
//     (<=) => { ast::token::Le };
//     (>=) => { ast::token::Ge };
//     (==) => { ast::token::Eq };
//     (!=) => { ast::token::Neq };
//     (:=) => { ast::token::Walrus };
//     (->) => { ast::token::RArrow };
//     (struct) => { ast::token::Struct };
//     (test) => { ast::token::Test };
// }

// fn x() {
//     let x: Tok![struct] = todo!();
// }

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

    /// `fn`
    Fn,

    /// `return`
    Return,

    /// `struct`
    Struct,

    /// `test`
    Test,

    /// end of input
    Eoi,
}
