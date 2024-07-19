use proc_macro2::Span;
use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;

/// Derives the `CloneNode` trait to clone nodes while keeping `NodeId`s unique.
/// For structs this creates a new struct of the same type, with each field
/// initialised with a call to `clone_node`. Enums defer to their variants.
/// The `node_id` field of structs is treated differently. It is initialised
/// with a call to `ProgramData::new` to create a unique `NodeId`.
/// Example:
/// ```
/// #[derive(CloneNode)]
/// struct AwaitExpr {
///     node_id: NodeId,
///     span: Span,
///     arg: Box<Expr>,
/// }
///
/// #[derive(CloneNode)]
/// enum SomeEnum {
///     Value(NodeA),
///     Pair(NodeA, NodeB),
///     None
/// }
/// ```
/// the output would be:
/// ```
/// impl crate::CloneNode for AwaitExpr  {
///     fn clone_node(&self, program_data: &mut crate::ProgramData) -> Self {
///         AwaitExpr {
///             node_id: program_data.new_id_from(self.node_id),
///             arg: crate::CloneNode::clone_node(&self.arg, program_data),
///         }
///     }
/// }
/// impl crate::CloneNode for SomeEnum  {
///     fn clone_node(&self, program_data: &mut crate::ProgramData) -> Self {
///         match self {
///             Self::Value(_0) => Self::Value(crate::CloneNode::clone_node(_0, program_data)),
///             Self::Pair(_0, _1) => Self::Pair(crate::CloneNode::clone_node(_0, program_data), crate::CloneNode::clone_node(_1, program_data)),
///             Self::None => Self::None,
///         }
///     }
/// }
/// ```
#[proc_macro_derive(CloneNode)]
pub fn derive_clone_node(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    Deriver {
        trait_name: Ident::new("CloneNode", Span::call_site()),
        method_name: Ident::new("clone_node", Span::call_site()),
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
                fn #method_name(&self, program_data: &mut crate::ProgramData) -> Self {
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
        let mut fields = Punctuated::<FieldValue, Token![,]>::default();

        for field in &s.fields {
            let field_name = field
                .ident
                .as_ref()
                .expect("should only be called on structs with named fields");
            if field_name == "node_id" {
                // Special case the node_id field.
                fields.push(parse_quote!(node_id: program_data.new_id_from(self.node_id)));
            } else {
                // Call clone_node on the other fields.
                let trait_name = &self.trait_name;
                let method_name = &self.method_name;
                fields.push(
                    parse_quote!(#field_name: crate::#trait_name::#method_name(&self.#field_name, program_data)),
                );
            }
        }
        // Assemble fields into new struct.
        Expr::Struct(ExprStruct {
            attrs: vec![],
            path: parse_quote!(Self),
            brace_token: Default::default(),
            fields,
            dot2_token: None,
            rest: None,
        })
    }

    fn make_body_for_enum(&self, e: &DataEnum) -> Expr {
        let mut arms = Punctuated::<Arm, Token![,]>::default();
        for v in &e.variants {
            let arm = match &v.fields {
                // Unnamed fields e.g. Variant(u32, bool).
                // For this variant we would generate the arm:
                // Self::Variant(_0, _1) => Self::Variant(CloneNode::clone_node(_0, program_data), CloneNode::clone_node(_1, program_data))
                Fields::Unnamed(fields) => {
                    // Create pairs of (binding_var, initialiser) and then unzip into punctuated lists.
                    let (names, args): (Punctuated<Pat, Token![,]>, Punctuated<Expr, Token![,]>) =
                        fields
                            .unnamed
                            .iter()
                            .enumerate()
                            .map(|(i, f)| {
                                // Since the field is unnamed, we need to generate
                                // a variable name to bind it to. e.g. _0, _1, _2
                                let field_name = Ident::new(&format!("_{}", i), f.ty.span());
                                // The bound variable.
                                let var = Pat::Ident(PatIdent {
                                    attrs: vec![],
                                    by_ref: None,
                                    mutability: None,
                                    ident: field_name.clone(),
                                    subpat: None,
                                });
                                let trait_name = &self.trait_name;
                                let method_name = &self.method_name;
                                // The initializer
                                let init: Expr = parse_quote! {
                                    crate::#trait_name::#method_name(#field_name, program_data)
                                };
                                (var, init)
                            })
                            .unzip();

                    let variant = &v.ident;
                    parse_quote!(Self::#variant(#names) => Self::#variant(#args))
                }
                // Unit variant e.g. `Variant`.
                // For this variant we would generate the arm:
                // Self::Variant => Self::Variant
                Fields::Unit => {
                    let variant = &v.ident;
                    parse_quote!(Self::#variant => Self::#variant)
                }
                Fields::Named(_) => unimplemented!("named enum field"),
            };
            arms.push(arm);
        }

        // The final match expression.
        parse_quote!(match self { #arms })
    }
}
