use crate::{Parse, ParseStream, Result, SingleToken};
use macros::Parse;
use token::Token;

#[cfg(test)]
use serde::Serialize;

//

// macro_rules! impl_single_token {
//     ($($(#[$($t:tt)*])* pub struct $token:ident;)*) => {$(
//         $(#[$($t)*])*
//         pub struct $token;

//         impl SingleToken for $token {
//             const TOKEN: Token = Token::$token;
//             const SELF: Self = Self;
//         }

//         impl Parse for $token {
//             fn parse(tokens: &mut ParseStream) -> Result<Self> {
//                 Self::parse_single(tokens)
//             }
//         }
//     )*};
// }

// macro_rules! impl_parse {
//     ($(
//         $(#[$($t:tt)*])*
//         pub struct $token:ident $([ $($gen:tt)* ])? {
//             $($f_vis:vis $f_id:ident : $f_ty:ty),*
//             $(,)?
//         }
//     )*) => {$(
//         $(#[$($t)*])*
//         pub struct $token $(<$($gen)*>)? {
//             $($f_vis $f_id : $f_ty),*
//         }

//         impl $(<$($gen)*>)? Parse for $token {
//             fn parse(tokens: &mut ParseStream) -> Result<Self> {
//                 Ok($token {
//                     $($f_id: tokens.parse(),)*
//                 })
//             }
//         }
//     )*};
// }

//

// impl_single_token! {
//     /// `,`
// }

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[token(Token::Comma)]
pub struct Comma;

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[token(Token::Eoi)]
pub struct Eoi;

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
pub struct Ast<T> {
    pub inner: T,
    pub eoi: Eoi,
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct Root {
    pub inner: Vec<RootItem>,
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RootItem {
    Init(Init),
    Test(Test),
}

impl Parse for RootItem {
    fn parse(tokens: &mut ParseStream) -> Result<Self> {
        let old = tokens.clone();
        if let Ok(v) = tokens.parse() {
            return Ok(Self::Init(v));
        }
        *tokens = old;

        let old = tokens.clone();
        if let Ok(v) = tokens.parse() {
            return Ok(Self::Test(v));
        }
        *tokens = old;

        Err(tokens.top_unexpected())
    }
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct Init {
    pub targets: Targets,
}

pub type Targets = CommaSeparated<Target>;

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
pub struct Target {}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct CommaSeparated<T> {
    pub first: T,
    pub inner: Vec<CommaSeparatedItem<T>>,
    pub last: Comma,
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
pub struct CommaSeparatedItem<T> {
    pub item: T,
    pub comma: Comma,
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
pub struct Test {}
