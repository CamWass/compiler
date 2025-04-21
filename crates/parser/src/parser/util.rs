use super::*;
use crate::{
    context::{Context, ContextFlags, YesMaybe},
    token::Token,
};
use global_common::Span;
use std::ops::{Deref, DerefMut};

pub trait ParseObject<Obj> {
    type Prop;
    fn make_object(&mut self, span: Span, props: Vec<Self::Prop>) -> PResult<Obj>;
    fn parse_object_prop(&mut self, assign_props: &mut AssignProps) -> PResult<Self::Prop>;
}

pub enum AssignProps {
    /// We're parsing an expression or a pattern. Buffer locations of AssignProps so
    /// we can emit errors for them if we later realise we're parsing an expression.
    Buffer(Vec<Span>),
    /// We're parsing a pattern, so AssignProps aren't errors.
    Ignore,
    /// We're parsing an expression, so AssignProps are immediate errors.
    Emit,
}

pub struct WithCtx<'d: 'p, 'p, I: Tokens> {
    inner: &'p mut Parser<'d, I>,
    orig_ctx: Context,
}
impl<'d: 'p, 'p, I: Tokens> Deref for WithCtx<'d, 'p, I> {
    type Target = Parser<'d, I>;

    fn deref(&self) -> &Parser<'d, I> {
        self.inner
    }
}
impl<'d, I: Tokens> DerefMut for WithCtx<'d, '_, I> {
    fn deref_mut(&mut self) -> &mut Parser<'d, I> {
        self.inner
    }
}

impl<I: Tokens> Drop for WithCtx<'_, '_, I> {
    fn drop(&mut self) {
        self.inner.set_ctx(self.orig_ctx);
    }
}

/// "IsValidSimpleAssignmentTarget" from spec.
pub(super) fn is_valid_simple_assignment_target(expr: &Expr, strict: YesMaybe) -> bool {
    match expr {
        Expr::Ident(Ident { sym, .. }) => {
            if strict == YesMaybe::Yes && (sym == "arguments" || sym == "eval") {
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

        Expr::OptChain(OptChainExpr { expr, .. }) => {
            is_valid_simple_assignment_target(expr, strict)
        }

        Expr::Invalid(..) => false,
    }
}

impl<'d, I: Tokens> Parser<'d, I> {
    pub(super) fn assert_and_bump(&mut self, token: &Token) {
        debug_assert!(
            self.input.is(token),
            "assertion failed: expected token {:?}, got {:?}",
            token,
            self.input.cur()
        );

        let _ = self.input.cur();

        self.input.bump();
    }

    /// Original context is restored when returned guard is dropped.
    pub(super) fn with_ctx(&mut self, ctx: Context) -> WithCtx<'d, '_, I> {
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
    pub(super) fn strict_mode(&mut self) -> WithCtx<'d, '_, I> {
        let ctx = Context {
            strict: YesMaybe::Yes,
            ..self.ctx()
        };
        self.with_ctx(ctx)
    }

    /// Original context is restored when returned guard is dropped.
    pub(super) fn in_type(&mut self) -> WithCtx<'d, '_, I> {
        let ctx = Context {
            flags: self.ctx().flags | ContextFlags::in_type,
            ..self.ctx()
        };
        self.with_ctx(ctx)
    }

    /// Original context is restored when returned guard is dropped.
    pub(super) fn include_in_expr(&mut self, include_in_expr: bool) -> WithCtx<'d, '_, I> {
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
