use super::*;
use crate::context::{Context, ContextFlags, YesMaybe};
use common::Span;
use std::ops::{Deref, DerefMut};

pub enum AssignProps {
    /// We're parsing an expression or a pattern. Buffer locations of AssignProps so
    /// we can emit errors for them if we later realise we're parsing an expression.
    Buffer(Vec<Span>),
    /// We're parsing a pattern, so AssignProps aren't errors.
    Ignore,
    /// We're parsing an expression, so AssignProps are immediate errors.
    Emit,
}

pub struct WithCtx<'d: 'p, 'p> {
    inner: &'p mut Parser<'d>,
    orig_ctx: Context,
}
impl<'d: 'p, 'p> Deref for WithCtx<'d, 'p> {
    type Target = Parser<'d>;

    fn deref(&self) -> &Parser<'d> {
        self.inner
    }
}
impl<'d> DerefMut for WithCtx<'d, '_> {
    fn deref_mut(&mut self) -> &mut Parser<'d> {
        self.inner
    }
}

impl Drop for WithCtx<'_, '_> {
    fn drop(&mut self) {
        self.inner.set_ctx(self.orig_ctx);
    }
}

impl<'d> Parser<'d> {
    pub(super) fn assert_and_bump(&mut self, token: Token) {
        debug_assert!(
            self.is(token),
            "assertion failed: expected token {:?}, got {:?}",
            token,
            self.input.cur()
        );

        self.input.bump();
    }

    /// Original context is restored when returned guard is dropped.
    pub(super) fn with_ctx(&mut self, ctx: Context) -> WithCtx<'d, '_> {
        let orig_ctx = self.ctx();
        self.set_ctx(ctx);
        WithCtx {
            orig_ctx,
            inner: self,
        }
    }

    pub(super) fn set_ctx(&mut self, ctx: Context) {
        self.input.set_ctx(ctx);
    }

    /// Original context is restored when returned guard is dropped.
    pub(super) fn strict_mode(&mut self) -> WithCtx<'d, '_> {
        let ctx = Context {
            strict: YesMaybe::Yes,
            ..self.ctx()
        };
        self.with_ctx(ctx)
    }

    /// Original context is restored when returned guard is dropped.
    pub(super) fn in_type(&mut self) -> WithCtx<'d, '_> {
        let ctx = Context {
            flags: self.ctx().flags | ContextFlags::in_type,
            ..self.ctx()
        };
        self.with_ctx(ctx)
    }

    /// Original context is restored when returned guard is dropped.
    pub(super) fn include_in_expr(&mut self, include_in_expr: bool) -> WithCtx<'d, '_> {
        let mut ctx = self.ctx();
        ctx.flags
            .set(ContextFlags::include_in_expr, include_in_expr);
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
