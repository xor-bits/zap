use core::fmt;

use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, Data, Field, Generics, Meta, Type};

//

// I would rather write 150 lines of code to generate
// the boring copy/pasted 100 lines of code,
// instead of writing the 100 lines of code
#[proc_macro_derive(Parse, attributes(token))]
pub fn derive_single_token(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    match try_derive_single_token(input) {
        Ok(v) => v.into(),
        Err(err) => err.into_compile_error().into(),
    }
}

fn try_derive_single_token(input: syn::DeriveInput) -> syn::Result<TokenStream> {
    let mut target_token = None;
    for s in input.attrs {
        let list = match s.meta {
            Meta::List(list) => list,
            _ => continue,
        };

        let attr = list.path.segments.last().unwrap();
        if attr.ident != "token" {
            continue;
        }

        if target_token.is_some() {
            return err(list.span(), "target token already provided");
        }
        target_token = Some(list.tokens);
    }

    if let Some(target_token) = target_token {
        generate_single_token_impl(input.ident, input.data, target_token)
    } else {
        generate_parser_impl(input.ident, input.data, input.generics)
    }
}

fn generate_single_token_impl(
    id: Ident,
    data: Data,
    target_token: TokenStream,
) -> syn::Result<TokenStream> {
    let data = match data {
        syn::Data::Struct(data) => data,
        _ => return err(Span::call_site(), "only unit structs are supported"),
    };

    match data.fields {
        syn::Fields::Unit => {}
        _ => return err(Span::call_site(), "only unit structs are supported"),
    }

    Ok(quote! {
        impl SingleToken for #id {
            const TOKEN: Token = #target_token;
            const SELF: Self = Self;
        }

        impl Parse for #id {
            fn parse(tokens: &mut ParseStream) -> Result<Self> {
                Self::parse_single(tokens)
            }
        }
    })
}

fn generate_parser_impl(id: Ident, data: Data, generics: Generics) -> syn::Result<TokenStream> {
    let data = match data {
        Data::Struct(data) => data,
        _ => return err(Span::call_site(), "only structs are supported"),
    };

    fn fields_to_struct_builder(
        fields: impl IntoIterator<Item = Field>,
    ) -> (TokenStream, Vec<Type>) {
        let mut types = Vec::new();

        let fields = fields.into_iter().enumerate().map(|(i, field)| {
            types.push(field.ty);

            if let Some(ident) = field.ident {
                Either::A(ident)
                // quote! { #ident: tokens.parse()?, }
            } else {
                Either::B(i)
                // quote! { #i: tokens.parse()? }
            }
        });

        let fields = quote! {
            Ok(Self {
                #(#fields: tokens.parse()?,)*
            })
        };

        (fields, types)
    }

    let (stmts, extra_where) = match data.fields {
        syn::Fields::Named(named) => fields_to_struct_builder(named.named),
        syn::Fields::Unnamed(unnamed) => fields_to_struct_builder(unnamed.unnamed),
        syn::Fields::Unit => (quote! { Ok(Self) }, Vec::new()),
    };

    let (imp, gen, wher) = generics.split_for_impl();

    if let Some(wher) = wher {
        let wher = &wher.predicates;
        Ok(quote! {
            impl #imp Parse for #id #gen
            where
                #(#extra_where : Parse,)*
                #wher
            {
                fn parse(tokens: &mut ParseStream) -> Result<Self> {
                    #stmts
                }
            }
        })
    } else {
        Ok(quote! {
            impl #imp Parse for #id #gen
            where
                #(#extra_where : Parse,)*
            {
                fn parse(tokens: &mut ParseStream) -> Result<Self> {
                    #stmts
                }
            }
        })
    }

    // if let Some(mut wher) = wher.cloned() {
    //     let mut parse_trait = Punctuated::new();
    //     parse_trait.push_value(PathSegment {
    //         ident: Ident::new("Parse"),
    //         arguments: <_>::default(),
    //     });

    //     let mut field_must_impl_parse = Punctuated::new();
    //     field_must_impl_parse.push(TypeParamBound::Trait(TraitBound {
    //         paren_token: None,
    //         modifier: <_>::default(),
    //         lifetimes: None,
    //         path: syn::Path {
    //             leading_colon: None,
    //             segments: Punctuated::new(),
    //         },
    //     }));
    //     for ty in extra_where {
    //         wher.predicates
    //             .push_value(WherePredicate::Type(PredicateType {
    //                 lifetimes: None,
    //                 bounded_ty: ty,
    //                 colon_token: <_>::default(),
    //                 bounds: Punctuated::new(),
    //             }));
    //     }
    // }

    // Ok(quote! {
    //     impl #imp Parse for #id #gen #wher {
    //         fn parse(tokens: &mut ParseStream) -> Result<Self> {
    //             #stmts
    //         }
    //     }
    // })
}

//

#[proc_macro_derive(Opcode)]
pub fn derive_opcode(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    match try_derive_opcode(input) {
        Ok(v) => v.into(),
        Err(err) => err.into_compile_error().into(),
    }
}

fn try_derive_opcode(input: syn::DeriveInput) -> syn::Result<TokenStream> {
    let data = match input.data {
        syn::Data::Enum(data) => data,
        _ => return err(Span::call_site(), "only enums are supported"),
    };

    let ops = data
        .variants
        .into_iter()
        .enumerate()
        .map(|(i, variant)| {
            (
                u8::try_from(i).expect("too many variants"),
                variant.ident.to_string().to_lowercase(),
                variant.ident,
            )
        })
        .collect::<Vec<(u8, String, Ident)>>();

    let as_byte = ops.iter().map(|(i, _, id)| {
        quote! { Self::#id => #i, }
    });

    let from_byte = ops.iter().map(|(i, _, id)| {
        quote! { #i => Self::#id, }
    });

    let as_asm = ops.iter().map(|(_, name, id)| {
        quote! { Self::#id => #name, }
    });

    let from_asm = ops.iter().map(|(_, name, id)| {
        quote! { #name => Self::#id, }
    });

    // .fold(quote! {}, |acc, s| {});

    let id = input.ident;
    let (imp, gen, wher) = input.generics.split_for_impl();

    Ok(quote! {
        impl #imp #id #gen #wher {
            // $(
            //     $(#[$($t)*])?
            //     pub const $id: Self = Self($num);
            // )*

            pub const fn as_byte(self) -> u8 {
                match self {
                    #(#as_byte)*
                }
            }

            pub const fn from_byte(b: u8) -> Option<Self> {
                Some(match b {
                    #(#from_byte)*
                    _ => return None,
                })
            }

            pub const fn as_asm(self) -> &'static str {
                match self {
                    #(#as_asm)*
                }
            }

            pub fn from_asm(s: &str) -> Option<Self> {
                Some(match s {
                    #(#from_asm)*
                    _ => return None
                })
            }
        }
    })
}

//

fn err<T>(s: Span, e: impl fmt::Display) -> syn::Result<T> {
    Err(syn::Error::new(s.span(), e))
}

enum Either<A, B> {
    A(A),
    B(B),
}

impl<A: ToTokens, B: ToTokens> ToTokens for Either<A, B> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Either::A(a) => a.to_tokens(tokens),
            Either::B(b) => b.to_tokens(tokens),
        }
    }
}
