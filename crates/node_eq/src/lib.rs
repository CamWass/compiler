#![deny(unused)]

use proc_macro2::Span;
use quote::ToTokens;
use quote::quote;
use syn::*;

#[proc_macro_derive(NodeEq)]
pub fn derive(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    Deriver {
        trait_name: Ident::new("NodeEq", Span::call_site()),
        method_name: Ident::new("eq_ignoring_node_id", Span::call_site()),
    }
    .derive(item)
}

struct Deriver {
    trait_name: Ident,
    method_name: Ident,
}

impl Deriver {
    fn derive(&self, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
        let input: DeriveInput = parse(item).unwrap();

        let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

        let body = self.make_body(&input.data);

        let trait_name = &self.trait_name;
        let ty = &input.ident;
        let method_name = &self.method_name;

        let item_impl: ItemImpl = parse_quote! {
            #[automatically_derived]
            impl #impl_generics crate::#trait_name for #ty #ty_generics #where_clause {
                fn #method_name(&self, other: &Self) -> bool {
                    #body
                }
            }
        };

        item_impl.to_token_stream().into()
    }

    fn make_body(&self, data: &Data) -> Expr {
        match data {
            Data::Struct(s) => self.make_body_for_struct(s),
            Data::Enum(e) => self.make_body_for_enum(e),
            Data::Union(_) => unimplemented!("union"),
        }
    }

    fn make_body_for_struct(&self, s: &DataStruct) -> Expr {
        let method_name = &self.method_name;

        let comparisons = s
            .fields
            .iter()
            .filter_map(|f| f.ident.as_ref())
            .filter(|ident| *ident != "node_id")
            .map(|field_name| {
                quote! {
                    self.#field_name.#method_name(&other.#field_name)
                }
            });

        // Join comparisons with `&&`.
        parse_quote! {
            true #(&& #comparisons)*
        }
    }

    fn make_body_for_enum(&self, e: &DataEnum) -> Expr {
        let method_name = &self.method_name;

        let mut arms = Vec::new();

        for v in &e.variants {
            let variant = &v.ident;

            match &v.fields {
                Fields::Unnamed(fields) => {
                    let self_bindings: Vec<_> = (0..fields.unnamed.len())
                        .map(|i| Ident::new(&format!("_self_{i}"), Span::call_site()))
                        .collect();

                    let other_bindings: Vec<_> = (0..fields.unnamed.len())
                        .map(|i| Ident::new(&format!("_other_{i}"), Span::call_site()))
                        .collect();

                    let comparisons = self_bindings.iter().zip(&other_bindings).map(|(s, o)| {
                        quote! { #s.#method_name(#o) }
                    });

                    let body = if comparisons.len() == 0 {
                        quote! { true }
                    } else {
                        // Join comparisons with `&&`.
                        quote! { #(#comparisons)&&* }
                    };

                    arms.push(quote! {
                        (Self::#variant(#(#self_bindings),*), Self::#variant(#(#other_bindings),*)) => {
                            #body
                        }
                    });
                }
                Fields::Unit => {
                    arms.push(quote! {
                        (Self::#variant, Self::#variant) => true
                    });
                }
                Fields::Named(_) => unimplemented!("named enum field"),
            }
        }

        parse_quote! {
            match (self, other) {
                #(#arms,)*
                _ => false,
            }
        }
    }
}
