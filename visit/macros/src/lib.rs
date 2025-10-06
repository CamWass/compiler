extern crate proc_macro;

use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use std::{collections::HashSet, mem::replace};
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, spanned::Spanned, token::Underscore,
    Arm, Block, Expr, ExprBlock, ExprMatch, FieldPat, Fields, FnArg, GenericArgument, GenericParam,
    Generics, Index, Item, ItemTrait, Lifetime, LifetimeParam, Member, Pat, PatIdent, PatWild,
    Path, PathArguments, ReturnType, Signature, Stmt, Token, TraitItem, TraitItemFn, Type,
    TypePath, TypeReference, Visibility,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Visit,
    VisitMut,
}

impl Mode {
    fn trait_name(self) -> &'static str {
        match self {
            Mode::Visit => "Visit",
            Mode::VisitMut => "VisitMut",
        }
    }

    fn prefix(self) -> &'static str {
        match self {
            Mode::Visit => "visit",
            Mode::VisitMut => "visit_mut",
        }
    }
}

/// This creates `Visit`. This is extensible visitor generator, and it
///
///  - works with stable rustc
///
///  - highly extensible and used to create Visitor for any types
///
///  - create `Visit`, `VisitMut`
#[proc_macro]
pub fn define(tts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let block = parse_macro_input!(tts as Block);

    let mut q = TokenStream::new();
    q.extend(make(Mode::Visit, &block.stmts));
    q.extend(make(Mode::VisitMut, &block.stmts));

    q.into()
}

fn make(mode: Mode, stmts: &[Stmt]) -> TokenStream {
    let mut types = vec![];
    let mut methods = vec![];

    for stmts in stmts {
        let item = match stmts {
            Stmt::Item(item) => item,
            _ => unimplemented!("error reporting for something other than Item"),
        };

        let mtd = make_method(mode, item, &mut types);

        methods.push(mtd);
    }

    let mut tokens = TokenStream::new();
    {
        let mut new = vec![];
        for ty in &types {
            add_required(&mut new, ty);
        }
        types.extend(new);
    }

    // Remove `Box`
    types.retain(|ty| as_box(ty).is_none());
    types.sort_by_cached_key(|ty| method_name_as_str(mode, ty));
    types.dedup_by_key(|ty| method_name_as_str(mode, ty));

    let types = types;

    methods.sort_by_cached_key(|v| v.sig.ident.to_string());
    methods.dedup_by_key(|v| v.sig.ident.to_string());

    for ty in &types {
        let sig = create_method_sig(mode, ty);
        let name = sig.ident.clone();
        let s = name.to_string();
        if methods.iter().any(|m| m.sig.ident == *s) {
            continue;
        }

        methods.push(TraitItemFn {
            attrs: vec![],
            sig,
            default: Some(create_method_body(mode, ty)),
            semi_token: None,
        });
    }

    methods.sort_by_cached_key(|v| v.sig.ident.to_string());

    methods.iter_mut().for_each(|v| {
        let fn_name = v.sig.ident.clone();
        let default_body = replace(&mut v.default, Some(parse_quote!({#fn_name(self, node)})));

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

        let trait_name = Ident::new(mode.trait_name(), Span::call_site());

        tokens.extend(quote! {
            pub fn #fn_name<'ast, V: ?Sized + #trait_name<'ast>>(_visitor: &mut V, node: #arg_ty) {
                #default_body
            }
        })
    });

    let mut generics = Generics::default();
    generics
        .params
        .push(GenericParam::Lifetime(LifetimeParam::new(Lifetime::new(
            "'ast",
            Span::call_site(),
        ))));

    tokens.extend(
        ItemTrait {
            attrs: vec![],
            vis: Visibility::Public(Default::default()),
            unsafety: None,
            auto_token: None,
            trait_token: Default::default(),
            ident: Ident::new(mode.trait_name(), Span::call_site()),
            generics,
            colon_token: None,
            supertraits: Default::default(),
            brace_token: Default::default(),
            items: methods.into_iter().map(TraitItem::Fn).collect(),
            restriction: None,
        }
        .to_token_stream(),
    );

    // Add VisitWith
    {
        let trait_decl = match mode {
            Mode::Visit => quote! {
                pub trait VisitWith<'ast, V: Visit<'ast>> {
                    fn visit_with(&'ast self, v: &mut V);

                    /// Visit children nodes of self with `v`
                    fn visit_children_with(&'ast self, v: &mut V);
                }

                impl<'ast, V, T> VisitWith<'ast, V> for Box<T>
                where
                    V: Visit<'ast>,
                    T: 'static + VisitWith<'ast, V>,
                {
                    fn visit_with(&'ast self, v: &mut V) {
                        (**self).visit_with(v)
                    }

                    /// Visit children nodes of self with `v`
                    fn visit_children_with(&'ast self, v: &mut V) {
                        (**self).visit_children_with(v)
                    }
                }
            },

            Mode::VisitMut => quote! {
                pub trait VisitMutWith<'ast, V: VisitMut<'ast>> {
                    fn visit_mut_with(&'ast mut self, v: &mut V);

                    fn visit_mut_children_with(&'ast mut self, v: &mut V);
                }

                impl<'ast, V, T> VisitMutWith<'ast, V> for Box<T>
                where
                    V: VisitMut<'ast>,
                    T: 'static + VisitMutWith<'ast, V>,
                {
                    fn visit_mut_with(&'ast mut self, v: &mut V) {
                        (**self).visit_mut_with(v);
                    }

                    fn visit_mut_children_with(&'ast mut self, v: &mut V) {
                        (**self).visit_mut_children_with(v);
                    }
                }
            },
        };
        tokens.extend(trait_decl.to_token_stream());

        let mut names = HashSet::new();

        for ty in &types {
            if as_box(ty).is_some() {
                continue;
            }

            // Signature of visit_item
            let method_sig = method_sig(mode, ty);
            let method_name = method_sig.ident;

            // Prevent duplicate implementations.
            let s = method_name.to_string();
            if names.contains(&s) {
                continue;
            }
            names.insert(s);

            let expr = visit_expr(mode, ty, &parse_quote!(v), parse_quote!(self));

            match mode {
                Mode::Visit => {
                    let default_body = adjust_expr(
                        mode,
                        ty,
                        parse_quote!(self),
                        |expr| parse_quote!(#method_name(_visitor, #expr)),
                    );

                    tokens.extend(quote! {
                        impl<'ast, V: Visit<'ast>> VisitWith<'ast, V> for #ty {
                            fn visit_with(&'ast self, v: &mut V) {
                                #expr
                            }

                            fn visit_children_with(&'ast self, _visitor: &mut V) {
                                #default_body
                            }
                        }
                    });
                }

                Mode::VisitMut => {
                    let default_body = adjust_expr(
                        mode,
                        ty,
                        parse_quote!(self),
                        |expr| parse_quote!(#method_name(_visitor, #expr)),
                    );

                    tokens.extend(quote! {
                        impl<'ast, V: VisitMut<'ast>> VisitMutWith<'ast, V> for #ty {
                            fn visit_mut_with(&'ast mut self, v: &mut V) {
                                #expr
                            }

                            fn visit_mut_children_with(&'ast mut self, _visitor: &mut V) {
                                #default_body
                            }
                        }
                    });
                }
            }
        }
    }

    tokens
}

fn adjust_expr<F>(mode: Mode, ty: &Type, mut expr: Expr, visit: F) -> Expr
where
    F: FnOnce(Expr) -> Expr,
{
    if is_option(ty) {
        expr = if is_opt_vec(ty) {
            match mode {
                Mode::VisitMut => expr,
                Mode::Visit => {
                    parse_quote!( #expr.as_ref().map(|v| &**v) )
                }
            }
        } else {
            match mode {
                Mode::VisitMut => expr,
                Mode::Visit => parse_quote!( #expr.as_ref() ),
            }
        };
    }

    visit(expr)
}

///
///
/// - `Box<Expr>` => visit(&node) or Box::new(visit(*node))
/// - `Vec<Expr>` => &*node or
fn visit_expr(mode: Mode, ty: &Type, visitor: &Expr, expr: Expr) -> Expr {
    let visit_name = method_name(mode, ty);

    adjust_expr(
        mode,
        ty,
        expr,
        |expr| parse_quote!( #visitor.#visit_name(#expr) ),
    )
}

fn make_arm_from_struct(mode: Mode, path: &Path, variant: &Fields, is_enum: bool) -> Arm {
    let mut stmts = vec![];
    let mut fields: Punctuated<FieldPat, Token![,]> = Default::default();

    for (i, field) in variant.iter().enumerate() {
        let ty = &field.ty;

        let binding_ident = field
            .ident
            .clone()
            .unwrap_or_else(|| Ident::new(&format!("_{}", i), Span::call_site()));

        if !skip(ty) {
            let expr = parse_quote!(#binding_ident);

            let expr = visit_expr(mode, ty, &parse_quote!(_visitor), expr);
            stmts.push(Stmt::Expr(expr, Some(Token![;](Span::call_site()))));
        }

        fields.push(FieldPat {
            attrs: vec![],
            member: if field.ident.is_none() {
                Member::Unnamed(Index {
                    index: i as _,
                    span: path.span(),
                })
            } else {
                Member::Named(field.ident.clone().unwrap())
            },
            colon_token: if is_enum || skip(ty) {
                Some(Default::default())
            } else {
                None
            },
            pat: if skip(ty) {
                Box::new(Pat::Wild(PatWild {
                    attrs: Default::default(),
                    underscore_token: Underscore::default(),
                }))
            } else {
                Box::new(Pat::Ident(PatIdent {
                    attrs: Default::default(),
                    by_ref: None,
                    mutability: None,
                    ident: binding_ident,
                    subpat: None,
                }))
            },
        });
    }

    let block = Block {
        brace_token: Default::default(),
        stmts,
    };

    Arm {
        attrs: vec![],
        pat: parse_quote!( #path { #fields } ),
        guard: None,
        fat_arrow_token: Default::default(),
        body: Box::new(Expr::Block(ExprBlock {
            attrs: vec![],
            label: None,
            block,
        })),
        comma: None,
    }
}

fn method_sig(mode: Mode, ty: &Type) -> Signature {
    Signature {
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        fn_token: Default::default(),
        ident: method_name(mode, ty),
        generics: Default::default(),
        paren_token: Default::default(),
        inputs: {
            let mut p = Punctuated::default();
            p.push_value(parse_quote!(&mut self));
            p.push_punct(Default::default());
            match mode {
                Mode::VisitMut => {
                    p.push_value(parse_quote!( node: &'ast mut #ty ));
                }
                Mode::Visit => {
                    p.push_value(parse_quote!( node: &'ast #ty ));
                }
            }

            p
        },
        variadic: None,
        output: ReturnType::Default,
    }
}

fn method_sig_from_ident(mode: Mode, v: &Ident) -> Signature {
    method_sig(
        mode,
        &Type::Path(TypePath {
            qself: None,
            path: v.clone().into(),
        }),
    )
}

/// Returns None if it's skipped.
fn make_method(mode: Mode, e: &Item, types: &mut Vec<Type>) -> TraitItemFn {
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
                let arm = make_arm_from_struct(mode, &s.ident.clone().into(), &s.fields, false);

                let mut match_expr: ExprMatch = parse_quote!(match node {});
                match_expr.arms.push(arm);

                Block {
                    brace_token: Default::default(),
                    stmts: vec![parse_quote!( #match_expr )],
                }
            };

            let sig = method_sig_from_ident(mode, type_name);

            TraitItemFn {
                attrs: vec![],
                sig,
                default: Some(block),
                semi_token: None,
            }
        }
        Item::Enum(e) => {
            let type_name = &e.ident;

            types.push(
                TypePath {
                    qself: None,
                    path: e.ident.clone().into(),
                }
                .into(),
            );

            let block = {
                let mut arms = vec![];

                for variant in &e.variants {
                    for f in &variant.fields {
                        if skip(&f.ty) {
                            continue;
                        }
                        types.push(f.ty.clone());
                    }

                    let enum_name = &e.ident;
                    let variant_name = &variant.ident;

                    let arm = make_arm_from_struct(
                        mode,
                        &parse_quote!(#enum_name::#variant_name),
                        &variant.fields,
                        true,
                    );
                    arms.push(arm);
                }

                Block {
                    brace_token: Default::default(),
                    stmts: vec![Stmt::Expr(
                        Expr::Match(ExprMatch {
                            attrs: vec![],
                            match_token: Default::default(),
                            expr: parse_quote!(node),
                            brace_token: Default::default(),
                            arms,
                        }),
                        None,
                    )],
                }
            };

            TraitItemFn {
                attrs: vec![],
                sig: method_sig_from_ident(mode, type_name),
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

fn create_method_sig(mode: Mode, ty: &Type) -> Signature {
    fn mk_exact(ident: Ident, ty: &Type) -> Signature {
        Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: Default::default(),
            ident,
            generics: Default::default(),
            paren_token: Default::default(),
            inputs: {
                let mut p = Punctuated::default();
                p.push_value(parse_quote!(&mut self));
                p.push_punct(Default::default());
                p.push_value(parse_quote!( node: #ty ));

                p
            },
            variadic: None,
            output: ReturnType::Default,
        }
    }

    fn mk_ref(ident: Ident, ty: &Type, mutable: bool) -> Signature {
        mk_exact(
            ident,
            &Type::Reference(TypeReference {
                and_token: Default::default(),
                lifetime: Some(Lifetime::new("'ast", Span::call_site())),
                mutability: if mutable {
                    Some(Default::default())
                } else {
                    None
                },
                elem: Box::new(ty.clone()),
            }),
        )
    }

    match ty {
        Type::Paren(ty) => create_method_sig(mode, &ty.elem),
        Type::Path(p) => {
            let last = p.path.segments.last().unwrap();
            let ident = method_name(mode, ty);

            if !last.arguments.is_empty() {
                if let Some(arg) = as_box(ty) {
                    let ident = method_name(mode, arg);
                    let mutable = mode == Mode::VisitMut;
                    return mk_ref(ident, arg, mutable);
                }

                if let Some(arg) = extract_generic("Option", ty) {
                    let ident = method_name(mode, ty);

                    if let Some(item) = extract_vec(arg) {
                        match mode {
                            Mode::VisitMut => {
                                return mk_exact(
                                    ident,
                                    &parse_quote!( &'ast mut Option<Vec<#item>> ),
                                );
                            }
                            Mode::Visit => {
                                return mk_exact(ident, &parse_quote!( Option<&'ast [#item]> ));
                            }
                        }
                    }

                    match mode {
                        Mode::VisitMut => {
                            return mk_exact(ident, &parse_quote!( &'ast mut Option<#arg> ));
                        }
                        Mode::Visit => {
                            return mk_exact(ident, &parse_quote!( Option<&'ast #arg> ));
                        }
                    }
                }

                if last.ident == "Vec" {
                    match &last.arguments {
                        PathArguments::AngleBracketed(tps) => {
                            let arg = tps.args.first().unwrap();

                            match arg {
                                GenericArgument::Type(arg) => {
                                    let ident = method_name(mode, ty);

                                    match mode {
                                        Mode::VisitMut => {
                                            return mk_ref(ident, &parse_quote!( Vec<#arg> ), true);
                                        }
                                        Mode::Visit => {
                                            return mk_ref(ident, &parse_quote!( [#arg] ), false);
                                        }
                                    }
                                }
                                _ => unimplemented!("generic parameter other than type"),
                            }
                        }
                        _ => unimplemented!("Vec() -> Ret or Vec without a type parameter"),
                    }
                }
            }

            let mutable = mode == Mode::VisitMut;
            mk_ref(ident, ty, mutable)
        }
        Type::Reference(ty) => create_method_sig(mode, &ty.elem),
        _ => unimplemented!("Unknown type: {:?}", ty),
    }
}

fn create_method_body(mode: Mode, ty: &Type) -> Block {
    if let Some(ty) = extract_generic("Arc", ty) {
        match mode {
            Mode::Visit => {
                let visit = method_name(mode, ty);
                return parse_quote!({ _visitor.#visit(node) });
            }
            Mode::VisitMut => {
                return Block {
                    brace_token: Default::default(),
                    stmts: vec![],
                }
            }
        }
    }

    match ty {
        Type::Paren(ty) => create_method_body(mode, &ty.elem),
        Type::Path(p) => {
            let last = p.path.segments.last().unwrap();

            if !last.arguments.is_empty() {
                if let Some(arg) = as_box(ty) {
                    return create_method_body(mode, arg);
                }

                if last.ident == "Option" {
                    match &last.arguments {
                        PathArguments::AngleBracketed(tps) => {
                            let arg = tps.args.first().unwrap();

                            match arg {
                                GenericArgument::Type(arg) => {
                                    let ident = method_name(mode, arg);

                                    return parse_quote!({
                                        match node {
                                            Some(node) => _visitor.#ident(node),
                                            None => {}
                                        }
                                    });
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
                                    let ident = method_name(mode, arg);

                                    return if is_option(arg) {
                                        match mode {
                                            Mode::VisitMut => {
                                                parse_quote!({ node.iter_mut().for_each(|v| _visitor.#ident(v)) })
                                            }
                                            Mode::Visit => parse_quote!({
                                                node.iter()
                                                    .for_each(|v| _visitor.#ident(v.as_ref()))
                                            }),
                                        }
                                    } else {
                                        match mode {
                                            Mode::VisitMut => {
                                                parse_quote!({ node.iter_mut().for_each(|v| _visitor.#ident(v)) })
                                            }

                                            Mode::Visit => {
                                                parse_quote!({ node.iter().for_each(|v| _visitor.#ident(v)) })
                                            }
                                        }
                                    };
                                }
                                _ => unimplemented!("generic parameter other than type"),
                            }
                        }
                        _ => unimplemented!("Vec() -> Ret or Vec without a type parameter"),
                    }
                }
            }

            parse_quote!({})
        }
        Type::Reference(ty) => create_method_body(mode, &ty.elem),
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
    if let Some(ty) = extract_generic("Arc", ty) {
        add_required(types, ty);
        types.push(ty.clone());
    }
}

fn is_option(ty: &Type) -> bool {
    if let Type::Path(p) = ty {
        let last = p.path.segments.last().unwrap();

        if !last.arguments.is_empty() && last.ident == "Option" {
            return true;
        }
    }

    false
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

fn extract_vec(ty: &Type) -> Option<&Type> {
    extract_generic("Vec", ty)
}

fn is_opt_vec(ty: &Type) -> bool {
    if let Some(inner) = extract_generic("Option", ty) {
        extract_vec(inner).is_some()
    } else {
        false
    }
}

fn method_name_as_str(mode: Mode, ty: &Type) -> String {
    fn suffix(ty: &Type) -> String {
        // Box<T> has same name as T
        if let Some(ty) = extract_generic("Box", ty) {
            return suffix(ty);
        }

        if let Some(ty) = extract_generic("Arc", ty) {
            return format!("arc_{}", suffix(ty));
        }
        if let Some(ty) = extract_generic("Option", ty) {
            return format!("opt_{}", suffix(ty));
        }
        if let Some(ty) = extract_generic("Vec", ty) {
            if let Some(ty) = extract_generic("Option", ty) {
                return format!("opt_vec_{}", to_plural(suffix(ty)));
            }
            if to_plural(suffix(ty)) == suffix(ty) {
                return format!("{}_vec", to_plural(suffix(ty)));
            }
            return to_plural(suffix(ty));
        }
        to_case_snake_like(&type_to_name(ty))
    }

    format!("{}_{}", mode.prefix(), suffix(ty))
}

fn to_plural(mut s: String) -> String {
    if s.ends_with("child") {
        s.push_str("ren");
        return s;
    }
    if !s.ends_with('s') {
        s.push('s');
    }
    s
}

// Adapted from https://github.com/whatisinternet/Inflector/blob/a4a95eac75043f4bffb127c7c8ec886b5b106053/src/cases/case/mod.rs#L15
fn to_case_snake_like(convertable_string: &str) -> String {
    let mut first_character: bool = true;
    let mut result: String = String::with_capacity(convertable_string.len() * 2);
    for char_with_index in convertable_string.char_indices() {
        if !char_with_index.1.is_alphanumeric() {
            if !first_character {
                first_character = true;
                result.push('_');
            }
        } else if requires_seperator(char_with_index, first_character, convertable_string) {
            first_character = false;
            result.push('_');
            result.push(char_with_index.1.to_ascii_lowercase());
        } else {
            first_character = false;
            result.push(char_with_index.1.to_ascii_lowercase());
        }
    }
    result
}

fn requires_seperator(
    char_with_index: (usize, char),
    first_character: bool,
    convertable_string: &str,
) -> bool {
    !first_character
        && char_is_uppercase(char_with_index.1)
        && next_or_previous_char_is_lowercase(convertable_string, char_with_index.0)
}

fn next_or_previous_char_is_lowercase(convertable_string: &str, char_with_index: usize) -> bool {
    convertable_string
        .chars()
        .nth(char_with_index + 1)
        .unwrap_or('A')
        .is_lowercase()
        || convertable_string
            .chars()
            .nth(char_with_index - 1)
            .unwrap_or('A')
            .is_lowercase()
}

fn char_is_uppercase(test_char: char) -> bool {
    test_char == test_char.to_ascii_uppercase()
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

fn skip(ty: &Type) -> bool {
    match ty {
        Type::Path(p) => {
            let i = &p.path.segments.last().as_ref().unwrap().ident;

            if i == "bool"
                || i == "u128"
                || i == "u64"
                || i == "u32"
                || i == "u16"
                || i == "u8"
                || i == "isize"
                || i == "i128"
                || i == "i64"
                || i == "i32"
                || i == "i16"
                || i == "i8"
                || i == "isize"
                || i == "f64"
                || i == "f32"
            {
                return true;
            }

            false
        }
        _ => false,
    }
}
