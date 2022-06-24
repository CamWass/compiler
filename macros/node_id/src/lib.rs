extern crate proc_macro;

use macro_common::prelude::*;
use pmutil::{smart_quote, Quote};
use syn::{self, *};

#[proc_macro_derive(GetNodeIdMacro)]
pub fn derive_string_enum(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = syn::parse::<DeriveInput>(input)
        .map(From::from)
        .expect("failed to parse derive input");
    let mut tts = TokenStream::new();

    let trait_impl = match &input.data {
        Data::Struct(_) => Quote::new(def_site::<Span>())
            .quote_with(smart_quote!(
                Vars {
                    Type: &input.ident,
                    Trait: Ident::new("GetNodeId", call_site()),
                    method: Ident::new("node_id", call_site()),
                    return_ty: Ident::new("NodeId", call_site())
                },
                {
                    impl Trait for Type {
                        fn method(&self) -> return_ty {
                            self.node_id
                        }
                    }
                }
            ))
            .parse::<ItemImpl>()
            .with_generics(input.generics.clone()),
        Data::Enum(n) => {
            let arms = n
                .variants
                .iter()
                .map(|v| {
                    let pat = match &v.fields {
                        Fields::Unnamed(f) => {
                            assert!(f.unnamed.len() == 1);
                            Box::new(
                                Quote::new(def_site::<Span>())
                                    .quote_with(smart_quote!(Vars { path: &v.ident }, {
                                        Self::path(field)
                                    }))
                                    .parse(),
                            )
                        }
                        _ => unimplemented!(),
                    };

                    let body = Box::new(
                        Quote::new(def_site::<Span>())
                            .quote_with(smart_quote!(Vars {}, { return field.node_id() }))
                            .parse(),
                    );

                    Arm {
                        body,
                        attrs: Default::default(),
                        pat: Pat::Reference(PatReference {
                            and_token: def_site(),
                            mutability: None,
                            pat,
                            attrs: Default::default(),
                        }),
                        guard: None,
                        fat_arrow_token: def_site(),
                        comma: Some(def_site()),
                    }
                })
                .collect();

            let body = Expr::Match(ExprMatch {
                attrs: Default::default(),
                match_token: def_site(),
                brace_token: def_site(),
                expr: Box::new(
                    Quote::new(def_site::<Span>())
                        .quote_with(smart_quote!(Vars {}, { &self }))
                        .parse(),
                ),
                arms,
            });

            Quote::new(def_site::<Span>())
                .quote_with(smart_quote!(
                    Vars {
                        Type: &input.ident,
                        body,
                        Trait: Ident::new("GetNodeId", call_site()),
                        method: Ident::new("node_id", call_site()),
                        return_ty: Ident::new("NodeId", call_site())
                    },
                    {
                        impl Trait for Type {
                            fn method(&self) -> return_ty {
                                body
                            }
                        }
                    }
                ))
                .parse::<ItemImpl>()
                .with_generics(input.generics.clone())
        }
        Data::Union(_) => unimplemented!(),
    };

    trait_impl.to_tokens(&mut tts);

    tts.into()
}
