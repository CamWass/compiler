use ast::*;
use ecma_visit::{noop_visit_type, Visit, VisitWith};
use global_common::{Span, SyntaxContext};
use swc_atoms::JsWord;

pub trait IdentLike: Sized {
    fn from_ident(i: &Ident) -> Self;
    fn to_id(&self) -> Id;
    fn into_id(self) -> Id;
}

impl IdentLike for BindingIdent {
    fn from_ident(i: &Ident) -> Self {
        i.clone().into()
    }

    fn to_id(&self) -> Id {
        (self.id.sym.clone(), self.id.span.ctxt())
    }

    fn into_id(self) -> Id {
        self.id.into_id()
    }
}

impl IdentLike for (JsWord, Span) {
    #[inline]
    fn from_ident(i: &Ident) -> Self {
        (i.sym.clone(), i.span)
    }

    #[inline]
    fn to_id(&self) -> Id {
        (self.0.clone(), self.1.ctxt())
    }

    #[inline]
    fn into_id(self) -> Id {
        (self.0, self.1.ctxt())
    }
}

impl IdentLike for (JsWord, SyntaxContext) {
    #[inline]
    fn from_ident(i: &Ident) -> Self {
        (i.sym.clone(), i.span.ctxt())
    }

    #[inline]
    fn to_id(&self) -> Id {
        (self.0.clone(), self.1)
    }

    #[inline]
    fn into_id(self) -> Id {
        self
    }
}

impl IdentLike for Ident {
    #[inline]
    fn from_ident(i: &Ident) -> Self {
        i.clone()
    }

    #[inline]
    fn to_id(&self) -> Id {
        (self.sym.clone(), self.span.ctxt())
    }

    #[inline]
    fn into_id(self) -> Id {
        (self.sym, self.span.ctxt())
    }
}

#[inline(always)]
pub fn id(i: &Ident) -> Id {
    (i.sym.clone(), i.span.ctxt())
}

/// Finds all **binding** idents of variables.
pub struct DestructuringFinder<'a, I: IdentLike> {
    pub found: &'a mut Vec<I>,
}

/// Finds all **binding** idents of `node`.
pub fn find_ids<T, I: IdentLike>(node: &T) -> Vec<I>
where
    T: for<'any> VisitWith<DestructuringFinder<'any, I>>,
{
    let mut found = vec![];

    {
        let mut v = DestructuringFinder { found: &mut found };
        node.visit_with(&mut v);
    }

    found
}

impl<'a, I: IdentLike> Visit for DestructuringFinder<'a, I> {
    noop_visit_type!();

    /// No-op (we don't care about expressions)
    fn visit_expr(&mut self, _: &Expr) {}

    fn visit_ident(&mut self, i: &Ident) {
        self.found.push(I::from_ident(i));
    }

    /// No-op (we don't care about expressions)
    fn visit_prop_name(&mut self, _: &PropName) {}
}

pub type Id = (JsWord, SyntaxContext);
