use super::*;
use crate::{
    context::{Context, YesMaybe},
    token::Token,
};
use global_common::Span;
use std::ops::{Deref, DerefMut};

pub trait ParseObject<Obj> {
    type Prop;
    fn make_object(&mut self, span: Span, props: Vec<Self::Prop>) -> PResult<Obj>;
    fn parse_object_prop(&mut self) -> PResult<Self::Prop>;
}

pub struct WithState<'ast, 'w, I: Tokens> {
    inner: &'w mut Parser<'ast, I>,
    orig_state: State,
}
impl<'ast, I: Tokens> Deref for WithState<'ast, '_, I> {
    type Target = Parser<'ast, I>;

    fn deref(&self) -> &Parser<'ast, I> {
        &self.inner
    }
}
impl<'ast, I: Tokens> DerefMut for WithState<'ast, '_, I> {
    fn deref_mut(&mut self) -> &mut Parser<'ast, I> {
        &mut self.inner
    }
}
impl<'ast, I: Tokens> Drop for WithState<'ast, '_, I> {
    fn drop(&mut self) {
        std::mem::swap(&mut self.inner.state, &mut self.orig_state);
    }
}

pub struct WithCtx<'ast, 'w, I: Tokens> {
    inner: &'w mut Parser<'ast, I>,
    orig_ctx: Context,
}
impl<'ast, I: Tokens> Deref for WithCtx<'ast, '_, I> {
    type Target = Parser<'ast, I>;

    fn deref(&self) -> &Parser<'ast, I> {
        &self.inner
    }
}
impl<'ast, I: Tokens> DerefMut for WithCtx<'ast, '_, I> {
    fn deref_mut(&mut self) -> &mut Parser<'ast, I> {
        &mut self.inner
    }
}

impl<'ast, I: Tokens> Drop for WithCtx<'ast, '_, I> {
    fn drop(&mut self) {
        self.inner.set_ctx(self.orig_ctx);
    }
}

pub(super) trait ExprExt {
    /// "IsValidSimpleAssignmentTarget" from spec.
    fn is_valid_simple_assignment_target(&self, strict: YesMaybe) -> bool {
        match *self {
            Expr::Ident(Ident { ref sym, .. }) => {
                if strict == YesMaybe::Yes && (&*sym == "arguments" || &*sym == "eval") {
                    return false;
                }
                true
            }

            Expr::This(..)
            | Expr::Lit(..)
            | Expr::Array(..)
            | Expr::Object(..)
            | Expr::Fn(..)
            | Expr::Class(..)
            | Expr::Tpl(..)
            | Expr::TaggedTpl(..) => false,
            Expr::Paren(ParenExpr { ref expr, .. }) => {
                expr.is_valid_simple_assignment_target(strict)
            }

            Expr::Member(..) => true,

            Expr::New(..) | Expr::Call(..) => false,
            // TODO: Spec only mentions `new.target`
            Expr::MetaProp(..) => false,

            Expr::Update(..) => false,

            Expr::Unary(..) | Expr::Await(..) => false,

            Expr::Bin(..) => false,

            Expr::Cond(..) => false,

            Expr::Yield(..) | Expr::Arrow(..) | Expr::Assign(..) => false,

            Expr::Seq(..) => false,

            // MemberExpression is valid assignment target
            Expr::PrivateName(..) => false,

            // jsx
            Expr::JSXMember(..)
            | Expr::JSXNamespacedName(..)
            | Expr::JSXEmpty(..)
            | Expr::JSXElement(..)
            | Expr::JSXFragment(..) => false,

            // typescript
            Expr::OptChain(OptChainExpr { ref expr, .. })
            | Expr::TsNonNull(TsNonNullExpr { ref expr, .. })
            | Expr::TsTypeAssertion(TsTypeAssertion { ref expr, .. })
            | Expr::TsAs(TsAsExpr { ref expr, .. }) => {
                expr.is_valid_simple_assignment_target(strict)
            }

            Expr::TsConstAssertion(..) => false,

            Expr::Invalid(..) => false,
        }
    }
}

impl ExprExt for Expr<'_> {}

impl<'ast, I: Tokens> Parser<'ast, I> {
    pub(super) fn assert_and_bump(&mut self, token: &Token) {
        debug_assert!(
            self.input.is(token),
            "assertion failed: expected token {:?}, got {:?}",
            token,
            self.input.cur()
        );

        self.input.bump();
    }

    /// Original context is restored when returned guard is dropped.
    pub(super) fn with_ctx(&mut self, ctx: Context) -> WithCtx<'ast, '_, I> {
        let orig_ctx = self.ctx();
        self.set_ctx(ctx);
        WithCtx {
            orig_ctx,
            inner: self,
        }
    }

    /// Original state is restored when returned guard is dropped.
    pub(super) fn with_state(&mut self, state: State) -> WithState<'ast, '_, I> {
        let orig_state = std::mem::replace(&mut self.state, state);
        WithState {
            orig_state,
            inner: self,
        }
    }

    pub(super) fn set_ctx(&mut self, ctx: Context) {
        self.input.set_ctx(ctx);
    }

    /// Original context is restored when returned guard is dropped.
    pub(super) fn strict_mode(&mut self) -> WithCtx<'ast, '_, I> {
        let ctx = Context {
            strict: YesMaybe::Yes,
            ..self.ctx()
        };
        self.with_ctx(ctx)
    }

    /// Original context is restored when returned guard is dropped.
    pub(super) fn in_type(&mut self) -> WithCtx<'ast, '_, I> {
        let ctx = Context {
            in_type: true,
            ..self.ctx()
        };
        self.with_ctx(ctx)
    }

    /// Original context is restored when returned guard is dropped.
    pub(super) fn include_in_expr(&mut self, include_in_expr: bool) -> WithCtx<'ast, '_, I> {
        let ctx = Context {
            include_in_expr,
            ..self.ctx()
        };
        self.with_ctx(ctx)
    }

    /// Parse with given closure
    #[inline(always)]
    pub(super) fn parse_with<F, Ret>(&mut self, f: F) -> Ret
    where
        F: FnOnce(&mut Self) -> Ret,
    {
        f(self)
    }

    pub(super) fn syntax(&self) -> Syntax {
        self.input.syntax()
    }
}
