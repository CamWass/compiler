extern crate proc_macro;

use inflector::Inflector;
use pmutil::{q, Quote};
use proc_macro2::Ident;
use syn::{
    parse_quote::parse, spanned::Spanned, Block, GenericArgument, Item, PathArguments, Stmt, Type,
    TypePath,
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
    q.push_tokens(&make(&block.stmts));

    let stream = proc_macro2::TokenStream::from(q).into();

    // println!("{}", stream);

    stream
}

fn make_foo(e: &Item) -> String {
    match e {
        Item::Struct(s) => {
            let type_name = &s.ident;

            let ty = &Type::Path(TypePath {
                qself: None,
                path: type_name.clone().into(),
            });

            // ty.into_token_stream().to_string();

            let ident = method_name(ty);

            format!("[{}, {}],", ident, type_name)
        }

        _ => "".into(),
    }
}

fn make(stmts: &[Stmt]) -> Quote {
    let tokens = q!({});

    let s = {
        let mut s = String::with_capacity(stmts.len() * 5);

        for stmts in stmts {
            let item = match stmts {
                Stmt::Item(item) => item,
                _ => unimplemented!("error reporting for something other than Item"),
            };

            s.push_str(make_foo(item).as_str());
        }

        s
    };

    println!("{}", s);

    std::fs::write("f.txt", s).expect("Failed to output control flow graph");

    tokens
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

fn method_name_as_str(ty: &Type) -> String {
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
                return format!("opt_vec_{}", suffix(ty).to_plural());
            }
            if suffix(ty).to_plural() == suffix(ty) {
                return format!("{}_vec", suffix(ty).to_plural());
            }
            return suffix(ty).to_plural();
        }
        type_to_name(&ty).to_snake_case()
    }

    format!("visit_{}", suffix(ty))
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
