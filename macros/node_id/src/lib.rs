extern crate proc_macro;

use macro_common::prelude::*;
use syn::{self, *};

#[proc_macro_derive(GetNodeIdMacro)]
pub fn derive_string_enum(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = syn::parse::<DeriveInput>(input)
        .map(From::from)
        .expect("failed to parse derive input");
    let mut tts = TokenStream::new();

    let trait_impl = match &input.data {
        Data::Struct(_) => {
            let type_name: &Ident = &input.ident;
            let trait_name = Ident::new("GetNodeId", call_site());
            let method = Ident::new("node_id", call_site());
            let return_ty = Ident::new("NodeId", call_site());
            let item_impl: ItemImpl = parse_quote!(
                impl #trait_name for #type_name {
                    fn #method(&self) -> #return_ty {
                        self.node_id
                    }
                }
            );
            item_impl.with_generics(input.generics.clone())
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

            let type_name = &input.ident;
            let trait_name = Ident::new("GetNodeId", call_site());
            let method = Ident::new("node_id", call_site());
            let return_ty = Ident::new("NodeId", call_site());

            let item_impl: ItemImpl = parse_quote! {
                impl #trait_name for #type_name {
                    fn #method(&self) -> #return_ty {
                        #body
                    }
                }
            };

            item_impl.with_generics(input.generics.clone())
        }
        Data::Union(_) => unimplemented!(),
    };

    trait_impl.to_tokens(&mut tts);

    tts.into()
}
