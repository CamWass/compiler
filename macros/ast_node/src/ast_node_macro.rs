use macro_common::prelude::*;
use syn::{
    self,
    parse::{Parse, ParseStream},
};

#[derive(Clone)]
pub struct Args {
    pub ty: Literal,
}

impl Parse for Args {
    fn parse(i: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Args { ty: i.parse()? })
    }
}
