extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::{self, *};

#[proc_macro_derive(GetNodeIdMacro)]
pub fn derive_string_enum(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = syn::parse::<DeriveInput>(input)
        .map(From::from)
        .expect("failed to parse derive input");
    let mut tts = TokenStream::new();

    let trait_impl: ItemImpl = match &input.data {
        Data::Struct(_) => {
            let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
            let type_name: &Ident = &input.ident;
            let trait_name = Ident::new("GetNodeId", Span::call_site());
            let method = Ident::new("node_id", Span::call_site());
            let return_ty = Ident::new("NodeId", Span::call_site());
            parse_quote!(
                impl #impl_generics #trait_name for #type_name #ty_generics #where_clause {
                    fn #method(&self) -> #return_ty {
                        self.node_id
                    }
                }
            )
        }
        Data::Enum(n) => {
            let arms = n
                .variants
                .iter()
                .map(|v| {
                    let pat = match &v.fields {
                        Fields::Unnamed(f) => {
                            assert!(f.unnamed.len() == 1);
                            let path = &v.ident;
                            Box::new(parse_quote!(Self::#path(field)))
                        }
                        _ => unimplemented!(),
                    };

                    let body = Box::new(parse_quote!(return field.node_id()));

                    Arm {
                        body,
                        attrs: Default::default(),
                        pat: Pat::Reference(PatReference {
                            and_token: Default::default(),
                            mutability: None,
                            pat,
                            attrs: Default::default(),
                        }),
                        guard: None,
                        fat_arrow_token: Default::default(),
                        comma: Some(Default::default()),
                    }
                })
                .collect();

            let body = Expr::Match(ExprMatch {
                attrs: Default::default(),
                match_token: Default::default(),
                brace_token: Default::default(),
                expr: Box::new(parse_quote!(&self)),
                arms,
            });

            let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

            let type_name = &input.ident;
            let trait_name = Ident::new("GetNodeId", Span::call_site());
            let method = Ident::new("node_id", Span::call_site());
            let return_ty = Ident::new("NodeId", Span::call_site());

            parse_quote! {
                impl #impl_generics #trait_name for #type_name #ty_generics #where_clause {
                    fn #method(&self) -> #return_ty {
                        #body
                    }
                }
            }
        }
        Data::Union(_) => unimplemented!(),
    };

    trait_impl.to_tokens(&mut tts);

    tts.into()
}
