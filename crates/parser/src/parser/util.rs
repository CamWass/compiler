use super::*;
use crate::{
    context::{Context, YesMaybe},
    token::Token,
};
use global_common::Span;
use std::{
    mem::take,
    ops::{Deref, DerefMut},
};

pub trait ParseObject<Obj> {
    type Prop;
    fn make_object(&mut self, span: Span, props: Vec<Self::Prop>) -> PResult<Obj>;
    fn parse_object_prop(&mut self) -> PResult<Self::Prop>;
}

pub struct WithState<'w> {
    inner: &'w mut Parser,
    orig_state: State,
}
impl<'w> Deref for WithState<'w> {
    type Target = Parser;

    fn deref(&self) -> &Parser {
        &self.inner
    }
}
impl<'w> DerefMut for WithState<'w> {
    fn deref_mut(&mut self) -> &mut Parser {
        &mut self.inner
    }
}
impl<'w> Drop for WithState<'w> {
    fn drop(&mut self) {
        std::mem::swap(&mut self.inner.state, &mut self.orig_state);
    }
}

pub struct WithCtx<'w> {
    inner: &'w mut Parser,
    orig_ctx: Context,
}
impl<'w> Deref for WithCtx<'w> {
    type Target = Parser;

    fn deref(&self) -> &Parser {
        &self.inner
    }
}
impl<'w> DerefMut for WithCtx<'w> {
    fn deref_mut(&mut self) -> &mut Parser {
        &mut self.inner
    }
}

impl<'w> Drop for WithCtx<'w> {
    fn drop(&mut self) {
        self.inner.set_ctx(self.orig_ctx);
    }
}

pub(super) trait ExprExt {
    fn as_expr(&self) -> &Expr;

    /// "IsValidSimpleAssignmentTarget" from spec.
    fn is_valid_simple_assignment_target(&self, strict: YesMaybe) -> bool {
        match *self.as_expr() {
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

impl Parser {
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
    pub(super) fn with_ctx(&mut self, ctx: Context) -> WithCtx {
        let orig_ctx = self.ctx;
        self.set_ctx(ctx);
        WithCtx {
            orig_ctx,
            inner: self,
        }
    }

    /// Original state is restored when returned guard is dropped.
    pub(super) fn with_state(&mut self, state: State) -> WithState {
        let orig_state = std::mem::replace(&mut self.state, state);
        WithState {
            orig_state,
            inner: self,
        }
    }

    pub(super) fn strict_mode(&mut self) -> WithCtx {
        let ctx = Context {
            strict: YesMaybe::Yes,
            ..self.ctx
        };
        self.with_ctx(ctx)
    }

    /// Original context is restored when returned guard is dropped.
    pub(super) fn include_in_expr(&mut self, include_in_expr: bool) -> WithCtx {
        let ctx = Context {
            include_in_expr,
            ..self.ctx
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

    pub(super) fn set_ctx(&mut self, ctx: Context) {
        if ctx.is_module() && !self.module_errors.is_empty() {
            self.errors.append(&mut self.module_errors);
        }

        self.ctx = ctx
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        take(&mut self.errors)
    }

    pub(super) fn add_error(&mut self, error: Error) {
        self.errors.push(error);
    }

    /// Add an error for a strict mode violation.
    fn add_strict_mode_error(&mut self, error: Error) {
        match self.ctx.strict {
            YesMaybe::Yes => {
                // Definitely in strict mode, immediately add error.
                self.add_error(error);
            }
            YesMaybe::Maybe => {
                // Not yet sure if we are in strict mode, buffer error.
                self.strict_errors.push(error);
            }
        }
    }

    #[cold]
    #[inline(never)]
    pub(super) fn emit_error_span(&mut self, span: Span, kind: SyntaxError) {
        let err = Error {
            error: Box::new((span, kind)),
        };
        self.errors.push(err);
    }

    #[cold]
    #[inline(never)]
    pub(super) fn emit_strict_mode_error_span(&mut self, span: Span, kind: SyntaxError) {
        let err = Error {
            error: Box::new((span, kind)),
        };

        self.add_strict_mode_error(err);
    }

    /// Moves buffered strict mode errors that occurred before the current token
    /// into the regular error buffer.
    pub(super) fn emit_preceding_strict_errors(&mut self) {
        if self.strict_errors.is_empty() {
            return;
        }

        let cur = self.input.cur_span().lo;

        let end = self
            .strict_errors
            .iter()
            .position(|err| err.error.0.lo > cur)
            .unwrap_or_else(|| self.strict_errors.len());

        let errors = self.strict_errors.drain(..end);

        self.errors.extend(errors);
    }

    /// Converts buffered strict mode errors that occurred before the current
    /// token into module errors.
    pub(super) fn convert_preceding_strict_mode_errors_to_module_errors(&mut self) {
        if self.strict_errors.is_empty() {
            return;
        }

        let cur = self.input.cur_span().lo;

        let end = self
            .strict_errors
            .iter()
            .position(|err| err.error.0.lo > cur)
            .unwrap_or_else(|| self.strict_errors.len());

        let errors = self.strict_errors.drain(..end);

        match self.ctx.module {
            YesNoMaybe::Yes => {
                // Definitely in a module, immediately add errors.
                self.errors.extend(errors);
            }
            YesNoMaybe::No => {
                // Definitely not in a module, discard errors.
            }
            YesNoMaybe::Maybe => {
                // Not yet sure if we are in a module, buffer errors.
                self.module_errors.extend(errors);
            }
        }
    }
}
