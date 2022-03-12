extern crate proc_macro;

use ahash::AHashSet;
use inflector::Inflector;
use macro_common::{call_site, def_site};
use pmutil::{q, Quote};
use proc_macro2::Ident;
use std::{collections::HashSet, mem::replace};
use syn::{
    parse_quote::parse, punctuated::Punctuated, spanned::Spanned, Arm, AttrStyle, Attribute, Block,
    Expr, ExprBlock, ExprMatch, ExprMethodCall, FieldValue, Fields, FnArg, GenericArgument,
    ImplItem, ImplItemMethod, Index, Item, ItemImpl, ItemTrait, Member, Path, PathArguments,
    ReturnType, Signature, Stmt, Token, TraitItem, TraitItemMethod, Type, TypePath, TypeReference,
    VisPublic, Visibility,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Visit,
    VisitAll,
}

impl Mode {
    fn trait_name(self) -> &'static str {
        match self {
            Mode::VisitAll => "VisitAll",
            Mode::Visit => "Visit",
        }
    }

    fn prefix(self) -> &'static str {
        match self {
            Mode::Visit | Mode::VisitAll => "visit",
        }
    }
}

/// This creates `Visit`. This is extensible visitor generator, and it
///
///  - works with stable rustc
///
///  - highly extensible and used to create Visitor for any types
///
///  - create `Visit`, `VisitAll`
#[proc_macro]
pub fn define(tts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let block: Block = parse(tts.into());

    let mut q = Quote::new_call_site();
    q.push_tokens(&make(Mode::Visit, &block.stmts));
    // q.push_tokens(&make(Mode::VisitAll, &block.stmts));

    proc_macro2::TokenStream::from(q).into()
}

fn make(mode: Mode, stmts: &[Stmt]) -> Quote {
    // let mut types = vec![];
    let mut methods = vec![];
    let mut tokens = q!({});

    // Analysis
    let mut root_names = AHashSet::with_capacity_and_hasher(stmts.len(), Default::default());
    let mut ignored_enums = AHashSet::default();
    for stmt in stmts {
        match stmt {
            Stmt::Item(Item::Struct(s)) => {
                root_names.insert(s.ident.to_string());
            }
            Stmt::Item(Item::Enum(e)) => {
                let name = e.ident.to_string();
                root_names.insert(name.clone());

                // If the all of the enum's varients are fieldless, we won't
                // generate visitors for it.
                if e.variants.iter().all(|v| v.fields.is_empty()) {
                    ignored_enums.insert(name.clone());
                }
            }
            _ => unimplemented!("error reporting for something other than Item"),
        }
    }

    for stmts in stmts {
        let item = match stmts {
            Stmt::Item(item) => item,
            _ => unimplemented!("error reporting for something other than Item"),
        };

        let (is_enum, ident) = match &item {
            Item::Struct(s) => (false, &s.ident),
            Item::Enum(e) => (true, &e.ident),
            _ => unimplemented!("error"),
        };

        if !ignored_enums.contains(&ident.to_string()) {
            let ty = Type::Path(TypePath {
                qself: None,
                path: ident.clone().into(),
            });

            let mtd = make_method(mode, item, &root_names, &ignored_enums);

            methods.push(mtd);

            // let parent_expr = if is_struct {
            //     q!(Vars { Type: &ty }, { BoundNode::Type(self) }).parse::<Expr>()
            // } else {
            //     q!(Vars { Type: &ty }, { self.bind_to_opt_parent(parent) }).parse::<Expr>()
            // };

            let impl_ty = if is_enum {
                q!(Vars { ty: &ty }, { ty }).parse::<Type>()
            } else {
                q!(Vars { ty: &ty }, { Rc<ty> }).parse::<Type>()
            };

            tokens.push_tokens(&q!(
                Vars {
                    impl_ty,
                    visit_name: method_name(mode, &ty)
                },
                {
                    impl<V: Visit> VisitWith<V> for impl_ty {
                        fn visit_with(&self, v: &mut V, parent: Option<BoundNode>) {
                            v.visit_name(self, parent)
                        }

                        fn visit_children_with(&self, visitor: &mut V, parent: Option<BoundNode>) {
                            // let parent = self.bind_to_opt_parent(parent);
                            visit_name(visitor, self, parent)
                        }
                    }
                }
            ));
        }
    }

    // let mut visit_all_methods = vec![];
    // {
    //     let mut new = vec![];
    //     for ty in &types {
    //         add_required(&mut new, ty);
    //     }
    //     types.extend(new);
    // }

    // // Remove `Box`
    // types.retain(|ty| as_box(ty).is_none());
    // types.sort_by_cached_key(|ty| method_name_as_str(mode, &ty));
    // types.dedup_by_key(|ty| method_name_as_str(mode, &ty));

    // let types = types;

    // methods.sort_by_cached_key(|v| v.sig.ident.to_string());
    // methods.dedup_by_key(|v| v.sig.ident.to_string());

    // for ty in &types {
    //     let sig = create_method_sig(mode, ty);
    //     let name = sig.ident.clone();
    //     let s = name.to_string();
    //     if methods.iter().any(|m| m.sig.ident == *s) {
    //         continue;
    //     }

    //     methods.push(TraitItemMethod {
    //         attrs: vec![],
    //         sig,
    //         default: Some(create_method_body(mode, &ty)),
    //         semi_token: None,
    //     });
    // }

    // methods.sort_by_cached_key(|v| v.sig.ident.to_string());

    // for ty in &types {
    //     let sig = create_method_sig(mode, ty);
    //     let name = sig.ident.clone();

    //     {
    //         // Visit <-> VisitAll using global_visit::All

    //         visit_all_methods.push(ImplItemMethod {
    //             attrs: vec![],
    //             vis: Visibility::Inherited,
    //             defaultness: None,
    //             sig: sig.clone(),
    //             block: q!(
    //                 Vars { visit: &name },
    //                 ({
    //                     self.visitor.visit(n, parent);
    //                     visit(self, n, parent);
    //                 })
    //             )
    //             .parse(),
    //         });
    //     }
    // }

    methods.iter_mut().for_each(|v| {
        v.attrs.push(Attribute {
            pound_token: def_site(),
            style: AttrStyle::Outer,
            bracket_token: def_site(),
            path: q!({ allow }).parse(),
            tokens: q!({ (unused_variables) }).parse(),
        });

        let fn_name = v.sig.ident.clone();
        let default_body = replace(
            &mut v.default,
            Some(match mode {
                Mode::Visit => q!(Vars { fn_name: &fn_name }, {
                    {
                        fn_name(self, n, parent)
                    }
                })
                .parse(),
                Mode::VisitAll => Block {
                    brace_token: def_site(),
                    stmts: Default::default(),
                },
            }),
        );

        let arg_ty = v
            .sig
            .inputs
            .iter()
            .nth(1)
            .map(|v| match *v {
                FnArg::Typed(ref pat) => &pat.ty,
                _ => unreachable!(),
            })
            .unwrap();

        if mode == Mode::Visit {
            tokens.push_tokens(&q!(
                Vars {
                    fn_name,
                    default_body,
                    Type: arg_ty,
                    Trait: Ident::new(mode.trait_name(), call_site()),
                },
                {
                    #[allow(unused_variables)]
                    pub fn fn_name<V: Trait>(visitor: &mut V, n: Type, parent: Option<BoundNode>) {
                        default_body
                    }
                }
            ));
        }
    });

    tokens.push_tokens(&ItemTrait {
        attrs: vec![],
        vis: Visibility::Public(VisPublic {
            pub_token: def_site(),
        }),
        unsafety: None,
        auto_token: None,
        trait_token: def_site(),
        ident: Ident::new(mode.trait_name(), call_site()),
        generics: Default::default(),
        colon_token: None,
        supertraits: {
            let mut p = Punctuated::default();
            p.push_value(q!({ Sized }).parse());
            p
        },
        brace_token: def_site(),
        items: methods.into_iter().map(TraitItem::Method).collect(),
    });

    // // impl Visit for global_visit::All<V> where V: VisitAll
    // if mode == Mode::VisitAll {
    //     let mut item = q!(
    //         Vars {
    //             Trait: Ident::new(mode.trait_name(), call_site()),
    //         },
    //         {
    //             impl<V> Visit for ::global_visit::All<V> where V: VisitAll {}
    //         }
    //     )
    //     .parse::<ItemImpl>();

    //     item.items
    //         .extend(visit_all_methods.into_iter().map(ImplItem::Method));

    //     tokens.push_tokens(&item);
    //     tokens.push_tokens(&q!({
    //         pub use global_visit::All;
    //     }));
    // }

    // Add VisitWith
    {
        match mode {
        Mode::Visit => {
            tokens.push_tokens(&q!({
            pub trait VisitWith<V: Visit> {
                fn visit_with(&self, v: &mut V, parent: Option<BoundNode>);

                /// Visit children nodes of self with `v`
                fn visit_children_with(&self, v: &mut V, parent: Option<BoundNode>);
            }
        }));

        tokens.push_tokens(&q!({
            impl<V, T> VisitWith<V> for Box<T>
            where
                V: Visit,
                T: VisitWith<V>,
            {
                fn visit_with(&self, v: &mut V, parent: Option<BoundNode>) {
                    (**self).visit_with(v, parent)
                }

                /// Visit children nodes of self with `v`
                fn visit_children_with(&self, v: &mut V, parent: Option<BoundNode>) {
                    (**self).visit_children_with(v, parent)
                }
            }

            impl<V, T> VisitWith<V> for Vec<T>
            where
                V: Visit,
                T: VisitWith<V>,
            {
                fn visit_with(&self, v: &mut V, parent: Option<BoundNode>) {
                    self.iter().for_each(|n| n.visit_with(v, parent.clone()))
                }

                /// Visit children nodes of self with `v`
                fn visit_children_with(&self, v: &mut V, parent: Option<BoundNode>) {
                    self.iter().for_each(|n| n.visit_children_with(v, parent.clone()))
                }
            }

            impl<V, T> VisitWith<V> for Option<T>
            where
                V: Visit,
                T: VisitWith<V>,
            {
                fn visit_with(&self, v: &mut V, parent: Option<BoundNode>) {
                    self.as_ref().map(|n| n.visit_with(v, parent));
                }

                /// Visit children nodes of self with `v`
                fn visit_children_with(&self, v: &mut V, parent: Option<BoundNode>) {
                    self.as_ref().map(|n| n.visit_children_with(v, parent));
                }
            }
        }));
    },
        _ => todo!()
        // Mode::VisitAll => q!({
        //     pub trait VisitAllWith<V: VisitAll> {
        //         fn visit_all_with(&self, _parent: &dyn Node, v: &mut V);

        //         /// Visit children nodes of self with `v`
        //         fn visit_all_children_with(&self, v: &mut V);
        //     }

        //     impl<V, T> VisitAllWith<V> for Box<T>
        //     where
        //         V: VisitAll,
        //         T: 'static + VisitAllWith<V>,
        //     {
        //         fn visit_all_with(&self, _parent: &dyn Node, v: &mut V) {
        //             (**self).visit_all_with(_parent, v)
        //         }

        //         /// Visit children nodes of self with `v`
        //         fn visit_all_children_with(&self, v: &mut V) {
        //             let _parent = self as &dyn Node;
        //             (**self).visit_all_children_with(v)
        //         }
        //     }
        // }),
    }
        // tokens.push_tokens(&trait_decl);
    }

    tokens
}

// fn adjust_expr(ty: &Type, expr: Expr) -> Expr {
//     if let Type::Path(p) = ty {
//         let last = p.path.segments.last().unwrap();

//         if !last.arguments.is_empty() {
//             match &last.arguments {
//                 PathArguments::AngleBracketed(tps) => {
//                     let arg = tps.args.first().unwrap();

//                     match arg {
//                         GenericArgument::Type(arg) => {
//                             if last.ident == "Option" {
//                                 todo!();
//                             } else if last.ident == "Vec" {
//                                 q!(
//                                     Vars { ident },
//                                     ({
//                                         n.iter().for_each(|v| _visitor.ident(v.as_ref(), _parent))
//                                     })
//                                 )
//                                 .parse()
//                             } else if last.ident == "Rc" {
//                                 todo!();
//                             }
//                         }
//                         _ => unimplemented!("generic parameter other than type"),
//                     }
//                 }
//                 _ => unimplemented!("Box() -> T or Box without a type parameter"),
//             }
//         }
//     }
// }

// fn adjust_expr(ty: &Type, base_expr: Expr) -> Expr {
//     enum Step {
//         Option,
//         Vec,
//         Rc,
//     }

//     let mut steps = Vec::new();

//     if let Type::Path(p) = ty {
//         let last = p.path.segments.last().unwrap();

//         if !last.arguments.is_empty() {
//             match &last.arguments {
//                 PathArguments::AngleBracketed(tps) => {
//                     let arg = tps.args.first().unwrap();

//                     match arg {
//                         GenericArgument::Type(arg) => {
//                             let inner_is_rc = matches!(arg, Type::Path(p) if p.path.segments.last().unwrap().ident == "Rc");

//                             if last.ident == "Option" {
//                                 if inner_is_rc {
//                                     return q!(
//                                         Vars {
//                                             base_expr,
//                                             visit_name
//                                         },
//                                         {
//                                             base_expr
//                                                 .as_ref()
//                                                 .map(|e| visitor.visit_name(e.as_ref(), parent))
//                                         }
//                                     )
//                                     .parse();
//                                 }
//                             } else if last.ident == "Vec" {
//                                 steps.push(Step::Vec);
//                                 continue;
//                             } else if last.ident == "Rc" {
//                                 steps.push(Step::Rc);
//                                 continue;
//                             }
//                         }
//                         _ => unimplemented!("generic parameter other than type"),
//                     }
//                 }
//                 _ => unimplemented!("Box() -> T or Box without a type parameter"),
//             }
//         }
//     }

//     // expr = q!(Vars { expr, visit_name }, {
//     //     visitor.visit_name(expr, parent)
//     // })
//     // .parse()

//     let m = q!(
//         Vars {
//             base_expr,
//             visit_name
//         },
//         { visitor.visit_name(expr, parent) }
//     )
//     .parse::<ExprMethodCall>();

//     let expr = Expr::MethodCall(m);

//     for step in steps.iter() {
//         match step {
//             Step::Option => {
//                 expr = q!(Vars { expr, visit_name }, {
//                     e.as_ref().map(|e| visitor.visit_name(e, parent))
//                 })
//                 .parse()
//             }
//             Step::Vec => {
//                 expr = q!(Vars { expr, visit_name }, {
//                     e.iter().for_each(|e| visitor.visit_name(e, parent))
//                 })
//                 .parse()
//             }
//             Step::Rc => {
//                 expr = q!(Vars { expr, visit_name }, {
//                     visitor.visit_name(e.as_ref(), parent)
//                 })
//                 .parse()
//             }
//         }
//     }
// }

fn make_visit_expr(mode: Mode, ty: &Type, expr: Expr) -> Expr {
    // let visit_name = method_name(mode, ty);

    // adjust_expr(ty, expr)

    q!(Vars { expr }, { expr.visit_with(visitor, parent.clone()) }).parse()
}

fn extract_base_type(ty: &Type) -> Type {
    if let Type::Path(p) = ty {
        let last = p.path.segments.last().unwrap();

        if !last.arguments.is_empty() {
            match &last.arguments {
                PathArguments::AngleBracketed(tps) => {
                    let arg = tps.args.first().unwrap();

                    match arg {
                        GenericArgument::Type(arg) => {
                            return extract_base_type(arg);
                        }
                        _ => unimplemented!("generic parameter other than type"),
                    }
                }
                _ => unimplemented!("Box() -> T or Box without a type parameter"),
            }
        }
    }

    ty.clone()
}

fn make_arm_from_struct(
    mode: Mode,
    path: &Path,
    variant: &Fields,
    root_names: &AHashSet<String>,
    ignored_enums: &AHashSet<String>,
) -> Arm {
    let mut stmts = vec![];
    let mut fields: Punctuated<FieldValue, Token![,]> = Default::default();

    for (i, field) in variant.iter().enumerate() {
        let ty = &field.ty;

        let binding_ident = field
            .ident
            .clone()
            .unwrap_or_else(|| Ident::new(&format!("_{}", i), call_site()));

        let base_type = extract_base_type(ty);
        let base_type_name = type_to_name(&base_type);

        if root_names.contains(&base_type_name) && !ignored_enums.contains(&base_type_name) {
            let expr = q!(
                Vars {
                    binding_ident: &binding_ident
                },
                { binding_ident }
            )
            .parse();

            let expr = make_visit_expr(mode, ty, expr);
            stmts.push(Stmt::Semi(expr, call_site()));
        }

        if field.ident.is_some() {
            fields.push(
                q!(
                    Vars {
                        field: &binding_ident
                    },
                    { field }
                )
                .parse(),
            );
        } else {
            fields.push(FieldValue {
                attrs: vec![],
                member: Member::Unnamed(Index {
                    index: i as _,
                    span: path.span(),
                }),
                colon_token: Some(def_site()),
                expr: q!(Vars { binding_ident }, { binding_ident }).parse(),
            });
        }
    }

    let block = Block {
        brace_token: def_site(),
        stmts,
    };

    Arm {
        attrs: vec![],
        pat: q!(Vars { Path: path, fields }, { Path { fields } }).parse(),
        guard: None,
        fat_arrow_token: def_site(),
        body: Box::new(Expr::Block(ExprBlock {
            attrs: vec![],
            label: None,
            block,
        })),
        comma: None,
    }
}

fn method_sig(mode: Mode, ty: &Type, is_struct: bool) -> Signature {
    Signature {
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        fn_token: def_site(),
        ident: method_name(mode, ty),
        generics: Default::default(),
        paren_token: def_site(),
        inputs: {
            let mut p = Punctuated::default();
            p.push_value(q!(Vars {}, { &mut self }).parse());
            p.push_punct(def_site());
            // Only structs get wrapped in Rc:
            if is_struct {
                p.push_value(q!(Vars {  ty }, { n: &Rc<ty> }).parse());
            } else {
                p.push_value(q!(Vars { ty }, { n: &ty }).parse());
            }
            p.push_punct(def_site());
            p.push_value(q!(Vars {}, { parent: Option<BoundNode> }).parse());

            p
        },
        variadic: None,
        output: match mode {
            _ => ReturnType::Default,
        },
    }
}

fn method_sig_from_ident(mode: Mode, v: &Ident, is_struct: bool) -> Signature {
    method_sig(
        mode,
        &Type::Path(TypePath {
            qself: None,
            path: v.clone().into(),
        }),
        is_struct,
    )
}

fn make_method(
    mode: Mode,
    e: &Item,
    root_names: &AHashSet<String>,
    ignored_enums: &AHashSet<String>,
) -> TraitItemMethod {
    match e {
        Item::Struct(s) => {
            let type_name = &s.ident;

            let block = {
                let arm = make_arm_from_struct(
                    mode,
                    &s.ident.clone().into(),
                    &s.fields,
                    root_names,
                    ignored_enums,
                );

                let mut match_expr: ExprMatch = q!((match n.as_ref() {})).parse();
                match_expr.arms.push(arm);

                Block {
                    brace_token: def_site(),
                    stmts: vec![
                        q!(
                            Vars {
                                a: &s.ident.clone().to_string()
                            },
                            {
                                {
                                    // TODO:
                                    // if let Some(BoundNode::ClassDecl(cp)) = parent.clone() {
                                    // assert_ne!(cp, n.clone());
                                    // dbg!(n, parent.clone());
                                    // }
                                }
                            }
                        )
                        .parse(),
                        q!({
                            let parent = Some(n.bind_to_opt_parent(parent));
                        })
                        .parse(),
                        q!(Vars { match_expr }, { match_expr }).parse(),
                    ],
                }
            };

            let sig = method_sig_from_ident(mode, type_name, true);

            TraitItemMethod {
                attrs: vec![],
                sig,
                default: Some(block),
                semi_token: None,
            }
        }
        Item::Enum(e) => {
            let type_name = &e.ident;

            // types.push(
            //     TypePath {
            //         qself: None,
            //         path: e.ident.clone().into(),
            //     }
            //     .into(),
            // );

            let block = {
                let mut arms = vec![];

                for variant in &e.variants {
                    // for f in &variant.fields {
                    //     if skip(&f.ty) {
                    //         continue;
                    //     }
                    //     // types.push(f.ty.clone());
                    // }

                    let arm = make_arm_from_struct(
                        mode,
                        &q!(
                            Vars {
                                Enum: &e.ident,
                                Variant: &variant.ident
                            },
                            { Enum::Variant }
                        )
                        .parse(),
                        &variant.fields,
                        root_names,
                        ignored_enums,
                    );
                    arms.push(arm);
                }

                Block {
                    brace_token: def_site(),
                    stmts: vec![Stmt::Expr(Expr::Match(ExprMatch {
                        attrs: vec![],
                        match_token: def_site(),
                        expr: q!((n)).parse(),
                        brace_token: def_site(),
                        arms,
                    }))],
                }
            };

            TraitItemMethod {
                attrs: vec![],
                sig: method_sig_from_ident(mode, type_name, false),
                default: Some(block),
                semi_token: None,
            }
        }

        _ => unimplemented!(
            "proper error reporting for item other than struct / enum: {:?}",
            e
        ),
    }
}

// fn create_method_sig(mode: Mode, ty: &Type) -> Signature {
//     fn mk_exact(mode: Mode, ident: Ident, ty: &Type) -> Signature {
//         Signature {
//             constness: None,
//             asyncness: None,
//             unsafety: None,
//             abi: None,
//             fn_token: def_site(),
//             ident,
//             generics: Default::default(),
//             paren_token: def_site(),
//             inputs: {
//                 let mut p = Punctuated::default();
//                 p.push_value(q!(Vars {}, { &mut self }).parse());
//                 p.push_punct(def_site());
//                 p.push_value(q!(Vars { Type: ty }, { n: Type }).parse());
//                 // match mode {
//                 //     Mode::Visit | Mode::VisitAll => {
//                 //         p.push_punct(def_site());
//                 //         p.push_value(q!(Vars {}, { parent: Option<BoundNode> }).parse());
//                 //     }
//                 // }

//                 p
//             },
//             variadic: None,
//             output: match mode {
//                 _ => ReturnType::Default,
//             },
//         }
//     }

//     fn mk_ref(mode: Mode, ident: Ident, ty: &Type, mutable: bool) -> Signature {
//         mk_exact(
//             mode,
//             ident,
//             &Type::Reference(TypeReference {
//                 and_token: def_site(),
//                 lifetime: None,
//                 mutability: if mutable { Some(def_site()) } else { None },
//                 elem: Box::new(ty.clone()),
//             }),
//         )
//     }

//     match ty {
//         Type::Array(_) => unimplemented!("type: array type"),
//         Type::BareFn(_) => unimplemented!("type: fn type"),
//         Type::Group(_) => unimplemented!("type: group type"),
//         Type::ImplTrait(_) => unimplemented!("type: impl trait"),
//         Type::Infer(_) => unreachable!("infer type"),
//         Type::Macro(_) => unimplemented!("type: macro"),
//         Type::Never(_) => unreachable!("never type"),
//         Type::Paren(ty) => create_method_sig(mode, &ty.elem),
//         Type::Path(p) => {
//             let last = p.path.segments.last().unwrap();
//             let ident = method_name(mode, ty);

//             if !last.arguments.is_empty() {
//                 if let Some(arg) = as_box(&ty) {
//                     let ident = method_name(mode, &arg);
//                     match mode {
//                         Mode::Visit | Mode::VisitAll => {
//                             return mk_ref(mode, ident, &arg, false);
//                         }
//                     }
//                 }

//                 if let Some(arg) = extract_generic("Option", ty) {
//                     let ident = method_name(mode, ty);

//                     if let Some(item) = extract_vec(arg) {
//                         match mode {
//                             Mode::Visit | Mode::VisitAll => {
//                                 return mk_exact(
//                                     mode,
//                                     ident,
//                                     &q!(Vars { item }, { Option<&[item]> }).parse(),
//                                 );
//                             }
//                         }
//                     }

//                     match mode {
//                         Mode::Visit | Mode::VisitAll => {
//                             return mk_exact(
//                                 mode,
//                                 ident,
//                                 &q!(Vars { arg }, { Option<&arg> }).parse(),
//                             );
//                         }
//                     }
//                 }

//                 if last.ident == "Vec" {
//                     match &last.arguments {
//                         PathArguments::AngleBracketed(tps) => {
//                             let arg = tps.args.first().unwrap();

//                             match arg {
//                                 GenericArgument::Type(arg) => {
//                                     let ident = method_name(mode, ty);

//                                     match mode {
//                                         Mode::Visit | Mode::VisitAll => {
//                                             return mk_ref(
//                                                 mode,
//                                                 ident,
//                                                 &q!(Vars { arg }, { [arg] }).parse(),
//                                                 false,
//                                             );
//                                         }
//                                     }
//                                 }
//                                 _ => unimplemented!("generic parameter other than type"),
//                             }
//                         }
//                         _ => unimplemented!("Vec() -> Ret or Vec without a type parameter"),
//                     }
//                 }
//             }

//             match mode {
//                 Mode::Visit | Mode::VisitAll => mk_ref(mode, ident, ty, false),
//             }
//         }
//         Type::Ptr(_) => unimplemented!("type: pointer"),
//         Type::Reference(ty) => create_method_sig(mode, &ty.elem),
//         Type::Slice(_) => unimplemented!("type: slice"),
//         Type::TraitObject(_) => unimplemented!("type: trait object"),
//         Type::Tuple(_) => unimplemented!("type: trait tuple"),
//         Type::Verbatim(_) => unimplemented!("type: verbatim"),
//         _ => unimplemented!("Unknown type: {:?}", ty),
//     }
// }

// fn create_method_body(mode: Mode, ty: &Type) -> Block {
//     if let Some(ty) = extract_generic("Rc", ty) {
//         match mode {
//             Mode::Visit | Mode::VisitAll => {
//                 let visit = method_name(mode, ty);

//                 return q!(Vars { visit }, ({ visitor.visit(n, parent) })).parse();
//             }
//         }
//     }

//     match ty {
//         Type::Array(_) => unimplemented!("type: array type"),
//         Type::BareFn(_) => unimplemented!("type: fn type"),
//         Type::Group(_) => unimplemented!("type: group type"),
//         Type::ImplTrait(_) => unimplemented!("type: impl trait"),
//         Type::Infer(_) => unreachable!("infer type"),
//         Type::Macro(_) => unimplemented!("type: macro"),
//         Type::Never(_) => unreachable!("never type"),
//         Type::Paren(ty) => create_method_body(mode, &ty.elem),
//         Type::Path(p) => {
//             let last = p.path.segments.last().unwrap();

//             if !last.arguments.is_empty() {
//                 if let Some(arg) = as_box(ty) {
//                     match mode {
//                         Mode::VisitAll | Mode::Visit => {
//                             return create_method_body(mode, arg);
//                         }
//                     }
//                 }

//                 if last.ident == "Option" {
//                     match &last.arguments {
//                         PathArguments::AngleBracketed(tps) => {
//                             let arg = tps.args.first().unwrap();

//                             match arg {
//                                 GenericArgument::Type(arg) => {
//                                     let ident = method_name(mode, arg);

//                                     return match mode {
//                                         Mode::Visit | Mode::VisitAll => q!(
//                                             Vars { ident },
//                                             ({
//                                                 match n {
//                                                     Some(n) => visitor.ident(n, parent),
//                                                     None => {}
//                                                 }
//                                             })
//                                         )
//                                         .parse(),
//                                     };
//                                 }
//                                 _ => unimplemented!("generic parameter other than type"),
//                             }
//                         }
//                         _ => unimplemented!("Box() -> T or Box without a type parameter"),
//                     }
//                 }

//                 if last.ident == "Vec" {
//                     match &last.arguments {
//                         PathArguments::AngleBracketed(tps) => {
//                             let arg = tps.args.first().unwrap();

//                             match arg {
//                                 GenericArgument::Type(arg) => {
//                                     let ident = method_name(mode, arg);

//                                     return if is_option(arg) {
//                                         match mode {
//                                             Mode::Visit | Mode::VisitAll => q!(
//                                                 Vars { ident },
//                                                 ({
//                                                     n.iter().for_each(|v| {
//                                                         visitor.ident(v.as_ref(), parent)
//                                                     })
//                                                 })
//                                             )
//                                             .parse(),
//                                         }
//                                     } else {
//                                         match mode {
//                                             Mode::Visit | Mode::VisitAll => q!(
//                                                 Vars { ident },
//                                                 ({
//                                                     n.iter().for_each(|v| visitor.ident(v, parent))
//                                                 })
//                                             )
//                                             .parse(),
//                                         }
//                                     };
//                                 }
//                                 _ => unimplemented!("generic parameter other than type"),
//                             }
//                         }
//                         _ => unimplemented!("Vec() -> Ret or Vec without a type parameter"),
//                     }
//                 }
//             }

//             match mode {
//                 Mode::VisitAll | Mode::Visit => q!(({})).parse(),
//             }
//         }
//         Type::Ptr(_) => unimplemented!("type: pointer"),
//         Type::Reference(ty) => create_method_body(mode, &ty.elem),
//         Type::Slice(_) => unimplemented!("type: slice"),
//         Type::TraitObject(_) => unimplemented!("type: trait object"),
//         Type::Tuple(_) => unimplemented!("type: trait tuple"),
//         Type::Verbatim(_) => unimplemented!("type: verbatim"),
//         _ => unimplemented!("Unknown type: {:?}", ty),
//     }
// }

// fn add_required(types: &mut Vec<Type>, ty: &Type) {
//     if let Some(ty) = extract_generic("Option", ty) {
//         add_required(types, ty);
//         types.push(ty.clone());
//         return;
//     }
//     if let Some(ty) = extract_generic("Vec", ty) {
//         add_required(types, ty);
//         types.push(ty.clone());
//         return;
//     }
//     if let Some(ty) = extract_generic("Rc", ty) {
//         add_required(types, ty);
//         types.push(ty.clone());
//         return;
//     }
// }

// fn is_option(ty: &Type) -> bool {
//     if let Type::Path(p) = ty {
//         let last = p.path.segments.last().unwrap();

//         if !last.arguments.is_empty() && last.ident == "Option" {
//             return true;
//         }
//     }

//     false
// }

// fn as_box(ty: &Type) -> Option<&Type> {
//     extract_generic("Box", ty)
// }

fn extract_generic<'a>(name: &str, ty: &'a Type) -> Option<&'a Type> {
    if let Type::Path(p) = ty {
        let last = p.path.segments.last().unwrap();

        if !last.arguments.is_empty() && last.ident == name {
            match &last.arguments {
                PathArguments::AngleBracketed(tps) => {
                    let arg = tps.args.first().unwrap();

                    match arg {
                        GenericArgument::Type(arg) => return Some(arg),
                        _ => unimplemented!("generic parameter other than type"),
                    }
                }
                _ => unimplemented!("Box() -> T or Box without a type parameter"),
            }
        }
    }

    None
}

// fn extract_vec(ty: &Type) -> Option<&Type> {
//     extract_generic("Vec", ty)
// }

// fn is_opt_vec(ty: &Type) -> bool {
//     if let Some(inner) = extract_generic("Option", ty) {
//         extract_vec(inner).is_some()
//     } else {
//         false
//     }
// }

fn method_name_as_str(mode: Mode, ty: &Type) -> String {
    fn suffix(ty: &Type) -> String {
        // Box<T> has same name as T
        if let Some(ty) = extract_generic("Box", ty) {
            return suffix(ty);
        }

        if let Some(ty) = extract_generic("Rc", ty) {
            return format!("rc_{}", suffix(ty));
        }
        if let Some(ty) = extract_generic("Option", ty) {
            return format!("opt_{}", suffix(ty));
        }
        if let Some(ty) = extract_generic("Vec", ty) {
            if let Some(ty) = extract_generic("Option", ty) {
                return format!("opt_vec_{}", suffix(ty).to_plural());
            }
            if suffix(ty).to_plural() == suffix(ty) {
                return format!("{}_vec", suffix(ty).to_plural());
            }
            return suffix(ty).to_plural();
        }
        type_to_name(&ty).to_snake_case()
    }

    format!("{}_{}", mode.prefix(), suffix(ty))
}

fn method_name(mode: Mode, ty: &Type) -> Ident {
    let span = ty.span();
    Ident::new(&method_name_as_str(mode, ty), span)
}

fn type_to_name(ty: &Type) -> String {
    match ty {
        Type::Path(ty) => ty.path.segments.last().unwrap().ident.to_string(),
        _ => unimplemented!("type_to_name for type other than path: {:?}", ty),
    }
}

// fn skip(ty: &Type) -> bool {
//     match ty {
//         Type::Path(p) => {
//             let i = &p.path.segments.last().as_ref().unwrap().ident;

//             if i == "bool"
//                 || i == "u128"
//                 || i == "u128"
//                 || i == "u64"
//                 || i == "u32"
//                 || i == "u16"
//                 || i == "u8"
//                 || i == "isize"
//                 || i == "i128"
//                 || i == "i128"
//                 || i == "i64"
//                 || i == "i32"
//                 || i == "i16"
//                 || i == "i8"
//                 || i == "isize"
//                 || i == "f64"
//                 || i == "f32"
//             {
//                 return true;
//             }

//             false
//         }
//         _ => false,
//     }
// }
