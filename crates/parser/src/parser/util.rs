use super::*;
use crate::{ context::Context, token::Token};
use global_common::Span;
use std::ops::{Deref, DerefMut};

pub trait ParseObject<Obj> {
    type Prop;
    fn make_object(&mut self, span: Span, props: Vec<Self::Prop>) -> Obj;
    fn parse_object_prop(&mut self) -> Self::Prop;
}

pub struct WithState<'w, I: 'w + Tokens> {
    inner: &'w mut Parser<I>,
    orig_state: State,
}
impl<'w, I: Tokens> Deref for WithState<'w, I> {
    type Target = Parser<I>;

    fn deref(&self) -> &Parser<I> {
        &self.inner
    }
}
impl<'w, I: Tokens> DerefMut for WithState<'w, I> {
    fn deref_mut(&mut self) -> &mut Parser<I> {
        &mut self.inner
    }
}
impl<'w, I: Tokens> Drop for WithState<'w, I> {
    fn drop(&mut self) {
        std::mem::swap(&mut self.inner.state, &mut self.orig_state);
    }
}

pub struct WithCtx<'w, I: 'w + Tokens> {
    inner: &'w mut Parser<I>,
    orig_ctx: Context,
}
impl<'w, I: Tokens> Deref for WithCtx<'w, I> {
    type Target = Parser<I>;

    fn deref(&self) -> &Parser<I> {
        &self.inner
    }
}
impl<'w, I: Tokens> DerefMut for WithCtx<'w, I> {
    fn deref_mut(&mut self) -> &mut Parser<I> {
        &mut self.inner
    }
}

impl<'w, I: Tokens> Drop for WithCtx<'w, I> {
    fn drop(&mut self) {
        self.inner.set_ctx(self.orig_ctx);
    }
}

pub(super) trait ExprExt {
    fn as_expr(&self) -> &Expr;

    /// "IsValidSimpleAssignmentTarget" from spec.
    fn is_valid_simple_assignment_target(&self, strict: bool) -> bool {
        match *self.as_expr() {
            Expr::Ident(Ident { ref sym, .. }) => {
                if strict && (&*sym == "arguments" || &*sym == "eval") {
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

impl ExprExt for Box<Expr> {
    fn as_expr(&self) -> &Expr {
        &*self
    }
}
impl ExprExt for Expr {
    fn as_expr(&self) -> &Expr {
        self
    }
}

impl<'a, I: Tokens> Parser<I> {
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
    pub(super) fn with_ctx(&mut self, ctx: Context) -> WithCtx<I> {
        let orig_ctx = self.ctx();
        self.set_ctx(ctx);
        WithCtx {
            orig_ctx,
            inner: self,
        }
    }

    /// Original state is restored when returned guard is dropped.
    pub(super) fn with_state(&mut self, state: State) -> WithState<I> {
        let orig_state = std::mem::replace(&mut self.state, state);
        WithState {
            orig_state,
            inner: self,
        }
    }

    pub(super) fn set_ctx(&mut self, ctx: Context) {
        self.input.set_ctx(ctx);
    }

    pub(super) fn strict_mode(&mut self) -> WithCtx<I> {
        let ctx = Context {
            strict: true,
            ..self.ctx()
        };
        self.with_ctx(ctx)
    }


    /// Original context is restored when returned guard is dropped.
    pub(super) fn include_in_expr(&mut self, include_in_expr: bool) -> WithCtx<I> {
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

    // Test whether a semicolon can be inserted at the current position.
    pub(super) fn can_insert_semicolon(&mut self) -> bool {
        self.input.cur().is_none()
            || self.input.is(&tok!('}'))
            || self.input.had_line_break_before_cur()
    }

    pub(super) fn is_line_terminator(&mut self) -> bool {
        self.input.eat(&tok!(';')) || self.can_insert_semicolon()
    }
}
