#![recursion_limit = "1024"]

extern crate proc_macro;

use macro_common::prelude::*;
use pmutil::{smart_quote, Quote, ToTokensExt};
use syn::{self, *};

mod ast_node_macro;

/// Alias for
/// `#[derive(Fold, Clone, Debug, PartialEq)]` for a struct and
/// `#[derive(Fold, Clone, Debug, PartialEq)]` for an
/// enum.
#[proc_macro_attribute]
pub fn ast_node(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    if !args.is_empty() {
        panic!("#[ast_node] does not accept any arguments");
    }
    let input: DeriveInput = parse(input).expect("failed to parse input as a DeriveInput");

    // we should use call_site
    let mut item = Quote::new(Span::call_site());
    item = match input.data {
        Data::Enum(..) => item.quote_with(smart_quote!(Vars { input }, {
            #[derive(Debug, PartialEq, ::node_id::GetNodeIdMacro, ::clone_node::CloneNode)]
            input
        })),
        _ => item.quote_with(smart_quote!(Vars { input }, {
            #[derive(Debug, PartialEq, ::node_id::GetNodeIdMacro, ::clone_node::CloneNode)]
            input
        })),
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
            NAME: Ident::new(const_name, Span::call_site())
        },
        {
            const NAME: () = { item };
        }
    ));
    print(name, item)
}
