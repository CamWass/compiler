extern crate proc_macro;

use ahash::AHashSet;
use inflector::Inflector;
use macro_common::{call_site, def_site};
use pmutil::{q, Quote};
use proc_macro2::Ident;
use syn::{
    parse_quote::parse, punctuated::Punctuated, spanned::Spanned, Arm, AttrStyle, Attribute, Block,
    Expr, ExprBlock, ExprMatch, Field, FieldValue, Fields, GenericArgument, Index, Item, ItemFn,
    ItemImpl, ItemMod, ItemStruct, ItemUse, Member, Path, PathArguments, Signature, Stmt, Token,
    Type, TypePath, VisPublic, Visibility,
};

/// This creates `Visit`. This is extensible visitor generator, and it
///
///  - works with stable rustc
///
///  - highly extensible and used to create Visitor for any types
///
///  - create `Visit`, `VisitAll`, `VisitMut`, `Fold`
#[proc_macro]
pub fn define(tts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let block: Block = parse(tts.into());

    let mut q = Quote::new_call_site();
    q.push_tokens(&make(block.stmts));

    proc_macro2::TokenStream::from(q).into()
}

fn convert_ty(struct_names: &AHashSet<String>, ty: &Type) -> Type {
    if let Type::Path(p) = ty {
        let last = p.path.segments.last().unwrap();

        if !last.arguments.is_empty() {
            match &last.arguments {
                PathArguments::AngleBracketed(tps) => {
                    let arg = tps.args.first().unwrap();

                    match arg {
                        GenericArgument::Type(arg) => {
                            if last.ident == "Option" {
                                return q!(Vars { t: convert_ty(struct_names,arg) }, { Option<t> })
                                    .parse();
                            } else if last.ident == "Box" {
                                return convert_ty(struct_names, arg).clone();
                            } else if last.ident == "Vec" {
                                return q!(Vars { t: convert_ty(struct_names,arg) }, { Vec<t> })
                                    .parse();
                            } else {
                                unreachable!()
                            }
                        }
                        _ => unimplemented!("generic parameter other than type"),
                    }
                }
                _ => unimplemented!("Box() -> T or Box without a type parameter"),
            }
        }
    }

    if !skip(ty) && struct_names.contains(&type_to_name(ty)) {
        q!(Vars { ty }, { Rc<ty> }).parse()
    } else {
        ty.clone()
    }
}

const CACHED_HASH_FIELD_NAME: &'static str = "cached_hash";

fn make_hash_impl(s: &ItemStruct) -> ItemImpl {
    let mut default_block = Block {
        brace_token: Default::default(),
        stmts: Vec::with_capacity(s.fields.len()),
    };

    for f in s.fields.iter() {
        default_block.stmts.push(
            q!(
                Vars {
                    name: f.ident.clone()
                },
                {
                    self.name.hash(state);
                }
            )
            .parse(),
        );
    }
    q!(
        Vars {
            ty: s.ident.clone(),
            cached_hash_field_name: Ident::new(CACHED_HASH_FIELD_NAME.into(), call_site()),
            default_block
        },
        {
            impl Hash for ty {
                fn hash<H: Hasher>(&self, state: &mut H) {
                    state.write_u32(self.cached_hash_field_name);
                    // if let Some(cached) = self.cached_hash_field_name.get() {
                    //     state.write_u32(cached);
                    // } else {
                    //     default_block;
                    //     self.cached_hash_field_name.set(Some(state.finish() as u32));
                    // }
                }
            }
        }
    )
    .parse::<ItemImpl>()
}

fn make(stmts: Vec<Stmt>) -> Quote {
    let mut types = vec![];
    let mut methods = vec![];

    let mut root_struct_names = AHashSet::with_capacity_and_hasher(stmts.len(), Default::default());

    let mut root_names = AHashSet::with_capacity_and_hasher(stmts.len(), Default::default());

    let mut tokens = q!({});
    let mut special_tokens = q!({});

    for stmt in &stmts {
        match stmt {
            Stmt::Item(Item::Struct(s)) => {
                root_struct_names.insert(s.ident.to_string());
            }
            Stmt::Item(Item::Enum(_)) => {}
            _ => unimplemented!("error reporting for something other than Item"),
        }
    }

    for stmt in stmts {
        match stmt {
            Stmt::Item(Item::Struct(s)) => {
                root_names.insert(s.ident.to_string());

                let needs_hash_impl;

                // Modified struct:
                {
                    let mut s = s.clone();
                    let mut contains_float = false;
                    for f in s.fields.iter_mut() {
                        let ty = &f.ty;
                        if let Type::Path(p) = ty {
                            let i = &p.path.segments.last().as_ref().unwrap().ident;

                            if i == "f64" {
                                contains_float = true;
                            }
                        }

                        f.ty = convert_ty(&root_struct_names, ty);
                    }

                    let cached_hash_field = Field {
                        attrs: vec![],
                        vis: Visibility::Public(VisPublic {
                            pub_token: Default::default(),
                        }),
                        ident: Some(Ident::new(CACHED_HASH_FIELD_NAME.into(), call_site())),
                        colon_token: Some(Default::default()),
                        ty: q!({ u32 }).parse(),
                    };

                    match &mut s.fields {
                        Fields::Named(fields) => fields.named.push(cached_hash_field),
                        _ => unreachable!(),
                    }

                    special_tokens.push_tokens(&s);

                    // Can't derive Hash or Eq when struct conatians float.
                    if contains_float {
                        s.attrs.push(Attribute {
                            pound_token: def_site(),
                            style: AttrStyle::Outer,
                            bracket_token: def_site(),
                            path: q!({ derive }).parse(),
                            tokens: q!({ (PartialEq, Clone, Debug, EqIgnoreSpan) }).parse(),
                        });
                        needs_hash_impl = false;
                    } else {
                        // s.attrs.push(Attribute {
                        //     pound_token: def_site(),
                        //     style: AttrStyle::Outer,
                        //     bracket_token: def_site(),
                        //     path: q!({ derive }).parse(),
                        //     tokens: q!({ (PartialEq, Eq, Hash, Clone, Debug) }).parse(),
                        // });
                        s.attrs.push(Attribute {
                            pound_token: def_site(),
                            style: AttrStyle::Outer,
                            bracket_token: def_site(),
                            path: q!({ derive }).parse(),
                            tokens: q!({ (PartialEq, Eq, Clone, Debug, EqIgnoreSpan) }).parse(),
                        });
                        needs_hash_impl = true;
                    }

                    tokens.push_tokens(&s);
                }

                // Hash impl:
                if needs_hash_impl {
                    let hash_impl = make_hash_impl(&s);
                    tokens.push_tokens(&hash_impl);
                }

                // Converter fn:
                {
                    let mtd = make_root_type_fn(&Item::Struct(s), &mut types);

                    methods.push(mtd);
                }
            }
            Stmt::Item(Item::Enum(e)) => {
                root_names.insert(e.ident.to_string());

                // Modified enum:
                {
                    let mut e = e.clone();
                    for v in e.variants.iter_mut() {
                        for f in v.fields.iter_mut() {
                            f.ty = convert_ty(&root_struct_names, &f.ty);
                        }
                    }

                    special_tokens.push_tokens(&e);

                    if e.variants.iter().all(|v| v.fields.is_empty()) {
                        // We can derive Copy for fieldless enums:
                        e.attrs.push(Attribute {
                            pound_token: def_site(),
                            style: AttrStyle::Outer,
                            bracket_token: def_site(),
                            path: q!({ derive }).parse(),
                            tokens: q!({ (PartialEq, Eq, Hash, Copy, Clone, Debug, EqIgnoreSpan) })
                                .parse(),
                        });
                    } else {
                        e.attrs.push(Attribute {
                            pound_token: def_site(),
                            style: AttrStyle::Outer,
                            bracket_token: def_site(),
                            path: q!({ derive }).parse(),
                            tokens: q!({ (PartialEq, Eq, Hash, Clone, Debug, EqIgnoreSpan) })
                                .parse(),
                        });
                    }

                    tokens.push_tokens(&e);
                }

                // Converter fn:
                {
                    let mtd = make_root_type_fn(&Item::Enum(e), &mut types);

                    methods.push(mtd);
                }
            }
            _ => unimplemented!("error reporting for something other than Item"),
        }
    }

    {
        let mut q = Quote::new_call_site();
        q.push_tokens(&special_tokens);

        let res: proc_macro::TokenStream = proc_macro2::TokenStream::from(q).into();

        std::fs::write("temp.rs", res.to_string()).expect("foooo");
    }

    // let mut ref_methods = vec![];
    // let mut optional_methods = vec![];
    // let mut visit_all_methods = vec![];
    {
        let mut new = vec![];
        for ty in &types {
            add_required(&mut new, ty);
        }
        types.extend(new);
    }

    // Remove `Box`
    // types.retain(|ty| as_box(ty).is_none());
    types.sort_by_cached_key(|ty| method_name_as_str(&ty));
    types.dedup_by_key(|ty| method_name_as_str(&ty));

    let types = types;

    methods.sort_by_cached_key(|v| v.sig.ident.to_string());
    methods.dedup_by_key(|v| v.sig.ident.to_string());

    for ty in &types {
        let sig = fn_sig_for_sub_type(&root_names, &root_struct_names, ty);
        let name = sig.ident.clone();
        let s = name.to_string();
        if methods.iter().any(|m| m.sig.ident == *s) {
            continue;
        }

        methods.push(ItemFn {
            attrs: vec![],
            sig,
            block: Box::new(make_sub_type_fn(&ty)),
            vis: Visibility::Inherited,
        });
    }

    methods.sort_by_cached_key(|v| v.sig.ident.to_string());

    let mut module_items = Vec::with_capacity(methods.len() + 1);
    module_items.push(
        q!({
            use super::*;
        })
        .parse::<ItemUse>()
        .into(),
    );

    module_items.extend(methods.into_iter().map(|f| f.into()));

    let module = ItemMod {
        attrs: vec![],
        vis: Visibility::Public(VisPublic {
            pub_token: Default::default(),
        }),
        mod_token: Default::default(),
        ident: Ident::new("convert".into(), call_site()),
        content: Some((Default::default(), module_items)),
        semi: Default::default(),
    };

    tokens.push_tokens(&module);

    // for m in &methods {
    //     tokens.push_tokens(m);
    // }

    tokens
}

fn make_arm_from_struct(in_path: &Path, out_path: &Path, variant: &Fields, is_struct: bool) -> Arm {
    let mut stmts = vec![];
    let mut fields: Punctuated<FieldValue, Token![,]> = Default::default();

    let mut f64_indices = AHashSet::default();

    for (i, field) in variant.iter().enumerate() {
        let ty = &field.ty;

        if let Type::Path(p) = ty {
            let ident = &p.path.segments.last().as_ref().unwrap().ident;

            if ident == "f64" {
                f64_indices.insert(i);
            }
        }

        let binding_ident = field
            .ident
            .clone()
            .unwrap_or_else(|| Ident::new(&format!("_{}", i), call_site()));

        if !skip(&ty) {
            let ident: Expr = q!(
                Vars {
                    binding_ident: &binding_ident
                },
                { binding_ident }
            )
            .parse();

            let expr: Expr = q!(
                Vars {
                    ident,
                    visit_name: method_name(ty)
                },
                { visit_name(ident) }
            )
            .parse();
            stmts.push(
                q!(
                    Vars {
                        name: &binding_ident,
                        expr
                    },
                    {
                        let name = expr;
                    }
                )
                .parse(),
            );
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
                    span: in_path.span(),
                }),
                colon_token: Some(def_site()),
                expr: q!(Vars { binding_ident }, { binding_ident }).parse(),
            });
        }
    }

    // Append return statement
    if is_struct {
        let mut default_block = Block {
            brace_token: Default::default(),
            stmts: Vec::with_capacity(fields.len()),
        };

        for (i, f) in fields.iter().enumerate() {
            // Floats cannot be hashed directly:
            if f64_indices.contains(&i) {
                default_block.stmts.push(
                    q!(
                        Vars {
                            name: f.member.clone()
                        },
                        {
                            std::hash::Hash::hash(&integer_decode(name), &mut s);
                        }
                    )
                    .parse(),
                );
            } else {
                default_block.stmts.push(
                    q!(
                        Vars {
                            name: f.member.clone()
                        },
                        {
                            std::hash::Hash::hash(&name, &mut s);
                        }
                    )
                    .parse(),
                );
            }
        }
        let cached_hash_field = FieldValue {
            attrs: vec![],
            member: Member::Named(Ident::new(CACHED_HASH_FIELD_NAME.into(), call_site())),
            colon_token: Some(Default::default()),
            expr: q!(Vars{
                default_block
            },{{
                let mut s = ahash::AHasher::default();
                default_block
                std::hash::Hasher::finish(&s) as u32
            }})
            .parse(),
        };
        stmts.push(
            q!(
                Vars {
                    Path: &out_path,
                    fields: &fields,
                    cached_hash_field
                },
                {
                    //
                    return Rc::new(Path {
                        cached_hash_field,
                        fields,
                    });
                }
            )
            .parse(),
        );
    } else {
        stmts.push(
            q!(
                Vars {
                    Path: &out_path,
                    fields: &fields
                },
                {
                    //
                    return Path { fields };
                }
            )
            .parse(),
        );
    }

    let block = Block {
        brace_token: def_site(),
        stmts,
    };

    Arm {
        attrs: vec![],
        pat: q!(
            Vars {
                Path: in_path,
                fields
            },
            { Path { fields } }
        )
        .parse(),
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

fn method_sig(ty: &Type, is_struct: bool) -> Signature {
    Signature {
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        fn_token: def_site(),
        ident: method_name(ty),
        generics: Default::default(),
        paren_token: def_site(),
        inputs: {
            let mut p = Punctuated::default();
            p.push_value(q!(Vars { Type: ty }, { n: ast::Type }).parse());

            p
        },
        variadic: None,
        // Only structs get wrapped in Rc:
        output: if is_struct {
            q!(Vars { ty }, { -> Rc<ty> }).parse()
        } else {
            q!(Vars { ty }, { -> ty }).parse()
        },
    }
}

fn fn_sig_for_root_type(v: &Ident, is_struct: bool) -> Signature {
    method_sig(
        &Type::Path(TypePath {
            qself: None,
            path: v.clone().into(),
        }),
        is_struct,
    )
}

fn make_root_type_fn(e: &Item, types: &mut Vec<Type>) -> ItemFn {
    match e {
        Item::Struct(s) => {
            let type_name = &s.ident;
            types.push(Type::Path(TypePath {
                qself: None,
                path: type_name.clone().into(),
            }));

            for f in &s.fields {
                if skip(&f.ty) {
                    continue;
                }

                types.push(f.ty.clone());
            }

            let block = {
                let arm = make_arm_from_struct(
                    &q!(Vars { s: &s.ident }, { ast::s }).parse(),
                    &s.ident.clone().into(),
                    &s.fields,
                    true,
                );

                let mut match_expr: ExprMatch = q!((match n {})).parse();
                match_expr.arms.push(arm);

                Block {
                    brace_token: def_site(),
                    stmts: vec![q!(Vars { match_expr }, { match_expr }).parse()],
                }
            };

            let sig = fn_sig_for_root_type(type_name, true);

            ItemFn {
                attrs: vec![],
                sig,
                block: Box::new(block),
                vis: Visibility::Public(VisPublic {
                    pub_token: Default::default(),
                }),
            }
        }
        Item::Enum(e) => {
            //
            let type_name = &e.ident;

            types.push(
                TypePath {
                    qself: None,
                    path: e.ident.clone().into(),
                }
                .into(),
            );

            //

            let block = {
                let mut arms = vec![];

                for variant in &e.variants {
                    for f in &variant.fields {
                        if skip(&f.ty) {
                            continue;
                        }
                        types.push(f.ty.clone());
                    }

                    let arm = make_arm_from_struct(
                        &q!(
                            Vars {
                                Enum: &e.ident,
                                Variant: &variant.ident
                            },
                            { ast::Enum::Variant }
                        )
                        .parse(),
                        &q!(
                            Vars {
                                Enum: &e.ident,
                                Variant: &variant.ident
                            },
                            { Enum::Variant }
                        )
                        .parse(),
                        &variant.fields,
                        false,
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

            ItemFn {
                attrs: vec![],
                sig: fn_sig_for_root_type(type_name, false),
                block: Box::new(block),
                vis: Visibility::Public(VisPublic {
                    pub_token: Default::default(),
                }),
            }
        }

        _ => unimplemented!(
            "proper error reporting for item other than struct / enum: {:?}",
            e
        ),
    }
}

fn fn_sig_for_sub_type(
    top_level_names: &AHashSet<String>,
    struct_names: &AHashSet<String>,
    ty: &Type,
) -> Signature {
    fn mk_exact(ident: Ident, in_ty: &Type, out_ty: &Type) -> Signature {
        Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: def_site(),
            ident,
            generics: Default::default(),
            paren_token: def_site(),
            inputs: {
                let mut p = Punctuated::default();
                p.push_value(q!(Vars { in_ty }, { n: in_ty }).parse());
                p
            },
            variadic: None,
            output: q!(Vars { out_ty }, { -> out_ty }).parse(),
        }
    }

    fn get_ast_type(top_level_names: &AHashSet<String>, ty: &Type) -> Type {
        if let Type::Path(p) = ty {
            let last = p.path.segments.last().unwrap();

            if !last.arguments.is_empty() {
                match &last.arguments {
                    PathArguments::AngleBracketed(tps) => {
                        let arg = tps.args.first().unwrap();

                        match arg {
                            GenericArgument::Type(arg) => {
                                if last.ident == "Option" {
                                    return q!(Vars { t: get_ast_type(top_level_names, arg) }, { Option<t> })
                                        .parse();
                                } else if last.ident == "Box" {
                                    // return get_ast_type(arg).clone();
                                    return q!(Vars { t: get_ast_type(top_level_names, arg) }, { Box<t> }).parse();
                                } else if last.ident == "Vec" {
                                    return q!(Vars { t: get_ast_type(top_level_names, arg) }, { Vec<t> }).parse();
                                } else {
                                    unreachable!()
                                }
                            }
                            _ => unimplemented!("generic parameter other than type"),
                        }
                    }
                    _ => unimplemented!("Box() -> T or Box without a type parameter"),
                }
            }
        }

        if top_level_names.contains(&type_to_name(ty)) {
            q!(Vars { ty }, { ast::ty }).parse()
        } else {
            ty.clone()
        }
    }

    match ty {
        Type::Array(_) => unimplemented!("type: array type"),
        Type::BareFn(_) => unimplemented!("type: fn type"),
        Type::Group(_) => unimplemented!("type: group type"),
        Type::ImplTrait(_) => unimplemented!("type: impl trait"),
        Type::Infer(_) => unreachable!("infer type"),
        Type::Macro(_) => unimplemented!("type: macro"),
        Type::Never(_) => unreachable!("never type"),
        Type::Paren(ty) => fn_sig_for_sub_type(top_level_names, struct_names, &ty.elem),
        Type::Path(_) => {
            let ident = method_name(ty);

            mk_exact(
                ident,
                &get_ast_type(top_level_names, ty),
                &convert_ty(struct_names, ty),
            )
        }
        Type::Ptr(_) => unimplemented!("type: pointer"),
        Type::Reference(ty) => fn_sig_for_sub_type(top_level_names, struct_names, &ty.elem),
        Type::Slice(_) => unimplemented!("type: slice"),
        Type::TraitObject(_) => unimplemented!("type: trait object"),
        Type::Tuple(_) => unimplemented!("type: trait tuple"),
        Type::Verbatim(_) => unimplemented!("type: verbatim"),
        _ => unimplemented!("Unknown type: {:?}", ty),
    }
}

fn make_sub_type_fn(ty: &Type) -> Block {
    match ty {
        Type::Array(_) => unimplemented!("type: array type"),
        Type::BareFn(_) => unimplemented!("type: fn type"),
        Type::Group(_) => unimplemented!("type: group type"),
        Type::ImplTrait(_) => unimplemented!("type: impl trait"),
        Type::Infer(_) => unreachable!("infer type"),
        Type::Macro(_) => unimplemented!("type: macro"),
        Type::Never(_) => unreachable!("never type"),
        Type::Paren(ty) => make_sub_type_fn(&ty.elem),
        Type::Path(p) => {
            let last = p.path.segments.last().unwrap();

            if !last.arguments.is_empty() {
                if let Some(arg) = as_box(ty) {
                    let ident = method_name(arg);

                    return q!(Vars { ident }, ({ ident(*n) })).parse();

                    // let ident = method_name(ty);

                    // return q!(Vars { ident }, ({ ident(n) })).parse();
                }

                if last.ident == "Option" {
                    match &last.arguments {
                        PathArguments::AngleBracketed(tps) => {
                            let arg = tps.args.first().unwrap();

                            match arg {
                                GenericArgument::Type(arg) => {
                                    let ident = method_name(arg);

                                    // if as_box(arg).is_some() {
                                    //     return q!(
                                    //         Vars { ident },
                                    //         ({
                                    //             match n {
                                    //                 Some(n) => Some(ident(*n)),
                                    //                 None => None,
                                    //             }
                                    //         })
                                    //     )
                                    //     .parse();
                                    // }

                                    return q!(
                                        Vars { ident },
                                        ({
                                            match n {
                                                Some(n) => Some(ident(n)),
                                                None => None,
                                            }
                                        })
                                    )
                                    .parse();
                                }
                                _ => unimplemented!("generic parameter other than type"),
                            }
                        }
                        _ => unimplemented!("Box() -> T or Box without a type parameter"),
                    }
                }

                if last.ident == "Vec" {
                    match &last.arguments {
                        PathArguments::AngleBracketed(tps) => {
                            let arg = tps.args.first().unwrap();

                            match arg {
                                GenericArgument::Type(arg) => {
                                    let ident = method_name(arg);

                                    return q!(
                                        Vars { ident },
                                        ({ n.into_iter().map(|e| ident(e)).collect::<Vec<_>>() })
                                    )
                                    .parse();
                                }
                                _ => unimplemented!("generic parameter other than type"),
                            }
                        }
                        _ => unimplemented!("Vec() -> Ret or Vec without a type parameter"),
                    }
                }
            }

            q!(({ return n })).parse()
        }
        Type::Ptr(_) => unimplemented!("type: pointer"),
        Type::Reference(ty) => make_sub_type_fn(&ty.elem),
        Type::Slice(_) => unimplemented!("type: slice"),
        Type::TraitObject(_) => unimplemented!("type: trait object"),
        Type::Tuple(_) => unimplemented!("type: trait tuple"),
        Type::Verbatim(_) => unimplemented!("type: verbatim"),
        _ => unimplemented!("Unknown type: {:?}", ty),
    }
}

fn add_required(types: &mut Vec<Type>, ty: &Type) {
    if let Some(ty) = extract_generic("Option", ty) {
        add_required(types, ty);
        types.push(ty.clone());
        return;
    }
    if let Some(ty) = extract_generic("Vec", ty) {
        add_required(types, ty);
        types.push(ty.clone());
        return;
    }
}

fn as_box(ty: &Type) -> Option<&Type> {
    extract_generic("Box", ty)
}

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

const METHOD_NAME: &'static str = "convert";

fn method_name_as_str(ty: &Type) -> String {
    fn suffix(ty: &Type) -> String {
        // Box<T> has same name as T
        if let Some(ty) = extract_generic("Box", ty) {
            return format!("boxed_{}", suffix(ty));
            // return suffix(ty);
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

    format!("{}_{}", METHOD_NAME, suffix(ty))
}

fn method_name(ty: &Type) -> Ident {
    let span = ty.span();
    Ident::new(&method_name_as_str(ty), span)
}

fn type_to_name(ty: &Type) -> String {
    match ty {
        Type::Path(ty) => ty.path.segments.last().unwrap().ident.to_string(),
        _ => unimplemented!("type_to_name for type other than path: {:?}", ty),
    }
}

fn skip(ty: &Type) -> bool {
    match ty {
        Type::Path(p) => {
            let i = &p.path.segments.last().as_ref().unwrap().ident;

            if i == "bool"
                || i == "u128"
                || i == "u128"
                || i == "u64"
                || i == "u32"
                || i == "u16"
                || i == "u8"
                || i == "isize"
                || i == "i128"
                || i == "i128"
                || i == "i64"
                || i == "i32"
                || i == "i16"
                || i == "i8"
                || i == "isize"
                || i == "f64"
                || i == "f32"
                || i == "Span"
            {
                return true;
            }

            false
        }
        _ => false,
    }
}
