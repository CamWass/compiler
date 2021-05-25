#![recursion_limit = "1024"]

extern crate proc_macro;

use darling;
use macro_common::prelude::*;
use pmutil::{smart_quote, Quote, ToTokensExt};
use syn::{self, *};

mod ast_node_macro;
mod spanned;

#[proc_macro_derive(Spanned, attributes(span))]
pub fn derive_spanned(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::<DeriveInput>(input).expect("failed to parse input as DeriveInput");
    let name = input.ident.clone();

    let item = self::spanned::derive(input);

    print_item(
        "derive(Spanned)",
        &format!("IMPL_SPANNED_FOR_{}", name),
        item.dump(),
    )
}

/// Alias for
/// `#[derive(Spanned, Fold, Clone, Debug, PartialEq)]` for a struct and
/// `#[derive(Spanned, Fold, Clone, Debug, PartialEq, FromVariant)]` for an
/// enum.
#[proc_macro_attribute]
pub fn ast_node(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input: DeriveInput = parse(input).expect("failed to parse input as a DeriveInput");

    // we should use call_site
    let mut item = Quote::new(Span::call_site());
    item = match input.data {
        Data::Enum(..) => {
            if !args.is_empty() {
                panic!("#[ast_node] on enum does not accept any argument")
            }

            item.quote_with(smart_quote!(Vars { input }, {
                #[derive(
                    ::global_common::FromVariant, ::global_common::Spanned, Clone, Debug, PartialEq,
                )]
                input
            }))
        }
        _ => {
            let args: Option<ast_node_macro::Args> = if args.is_empty() {
                None
            } else {
                Some(parse(args).expect("failed to parse args of #[ast_node]"))
            };

            let ast_node_impl = match args {
                Some(ref args) => Some(ast_node_macro::expand_struct(args.clone(), input.clone())),
                None => None,
            };

            let mut quote = item.quote_with(smart_quote!(Vars { input }, {
                #[derive(::global_common::Spanned, Clone, Debug, PartialEq)]
                input
            }));

            if let Some(items) = ast_node_impl {
                for item in items {
                    quote = quote.quote_with(smart_quote!(Vars { item }, { item }))
                }
            }

            quote
        }
    };

    print("ast_node", item)
}

/// Workarounds https://github.com/rust-lang/rust/issues/44925
fn print_item<T: Into<TokenStream>>(
    name: &'static str,
    const_name: &str,
    item: T,
) -> proc_macro::TokenStream {
    let item = Quote::new(def_site::<Span>()).quote_with(smart_quote!(
        Vars {
            item: item.into(),
            NAME: Ident::new(&const_name, Span::call_site())
        },
        {
            const NAME: () = { item };
        }
    ));
    print(name, item)
}
