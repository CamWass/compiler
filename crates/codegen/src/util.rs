use super::list::ListFormat;
use ast::*;
use global_common::{errors::SourceMapper, BytePos, SourceMap, SourceMapperDyn, Span};
use std::{rc::Rc, sync::Arc};

pub trait SourceMapperExt {
    fn get_code_map(&self) -> &dyn SourceMapper;

    fn is_on_same_line(&self, lo: BytePos, hi: BytePos) -> bool {
        // let cm = self.get_code_map();

        // let lo = cm.lookup_char_pos(lo);
        // let hi = cm.lookup_char_pos(hi);

        // lo.line == hi.line && lo.file.name_hash == hi.file.name_hash
        false
    }

    fn should_write_separating_line_terminator(&self, format: ListFormat) -> bool {
        if format.contains(ListFormat::MultiLine) {
            return true;
        }

        if format.contains(ListFormat::PreserveLines) {
            return format.contains(ListFormat::PreferNewLine);
        }

        false
    }

    fn should_write_leading_line_terminator<N>(
        &self,
        parent_node: Span,
        children: &[N],
        format: ListFormat,
    ) -> bool {
        if format.contains(ListFormat::MultiLine) {
            return true;
        }

        if format.contains(ListFormat::PreserveLines) {
            if children.is_empty() {
                return !self.is_on_same_line(parent_node.lo(), parent_node.hi());
            }

            format.contains(ListFormat::PreferNewLine)
        } else {
            false
        }
    }

    fn should_write_closing_line_terminator<N>(
        &self,
        parent_node: Span,
        children: &[N],
        format: ListFormat,
    ) -> bool {
        if format.contains(ListFormat::MultiLine) {
            return (format & ListFormat::NoTrailingNewLine) == ListFormat::None;
        }

        if format.contains(ListFormat::PreserveLines) {
            if children.is_empty() {
                return !self.is_on_same_line(parent_node.lo(), parent_node.hi());
            }

            format.contains(ListFormat::PreferNewLine)
        } else {
            false
        }
    }
}
impl SourceMapperExt for dyn SourceMapper {
    fn get_code_map(&self) -> &dyn SourceMapper {
        self
    }
}
impl SourceMapperExt for Arc<SourceMapperDyn> {
    fn get_code_map(&self) -> &dyn SourceMapper {
        &**self
    }
}

impl SourceMapperExt for Rc<SourceMapperDyn> {
    fn get_code_map(&self) -> &dyn SourceMapper {
        &**self
    }
}

impl SourceMapperExt for Arc<SourceMap> {
    fn get_code_map(&self) -> &dyn SourceMapper {
        &**self
    }
}

impl SourceMapperExt for Rc<SourceMap> {
    fn get_code_map(&self) -> &dyn SourceMapper {
        &**self
    }
}

pub trait EndsWithAlphaNum {
    fn ends_with_alpha_num(&self) -> bool;
}

// impl EndsWithAlphaNum for ForHead {
//     fn ends_with_alpha_num(&self) -> bool {
//         match self {
//             ForHead::VarDecl(n) => n.ends_with_alpha_num(),
//             ForHead::Pat(n) => n.ends_with_alpha_num(),
//             ForHead::UsingDecl(n) => n.ends_with_alpha_num(),
//         }
//     }
// }

// impl EndsWithAlphaNum for ExprOrSpread {
//     fn ends_with_alpha_num(&self) -> bool {
//         match self {
//             ExprOrSpread::Spread(e) => e.expr.ends_with_alpha_num(),
//             ExprOrSpread::Expr(e) => e.ends_with_alpha_num(),
//         }
//     }
// }

impl EndsWithAlphaNum for VarDeclOrPat {
    fn ends_with_alpha_num(&self) -> bool {
        match self {
            VarDeclOrPat::VarDecl(n) => n.ends_with_alpha_num(),
            VarDeclOrPat::Pat(n) => n.ends_with_alpha_num(),
        }
    }
}

impl EndsWithAlphaNum for Pat {
    fn ends_with_alpha_num(&self) -> bool {
        match self {
            Pat::Object(_) | Pat::Array(_) => false,
            Pat::Rest(p) => p.arg.ends_with_alpha_num(),
            Pat::Assign(p) => p.right.ends_with_alpha_num(),
            Pat::Expr(p) => p.ends_with_alpha_num(),
            _ => true,
        }
    }
}

impl EndsWithAlphaNum for VarDecl {
    fn ends_with_alpha_num(&self) -> bool {
        match self.decls.last() {
            None => true,
            Some(d) => match d.init.as_deref() {
                Some(e) => e.ends_with_alpha_num(),
                None => d.name.ends_with_alpha_num(),
            },
        }
    }
}

impl EndsWithAlphaNum for Expr {
    fn ends_with_alpha_num(&self) -> bool {
        !matches!(
            self,
            Expr::Array(..)
                | Expr::Object(..)
                | Expr::Lit(Lit::Str(..))
                | Expr::Paren(..)
                | Expr::Member(MemberExpr { computed: true, .. })
        )
    }
}

/// Leftmost recursion
pub trait StartsWithAlphaNum {
    fn starts_with_alpha_num(&self) -> bool;
}

macro_rules! alpha_num_const {
    ($value:tt, $($ty:ident),*) => {
        $(
            impl StartsWithAlphaNum for $ty {
                #[inline]
                fn starts_with_alpha_num(&self) -> bool {
                    $value
                }
            }
        )*
    };
}

alpha_num_const!(true, BindingIdent, Ident);
alpha_num_const!(false, ArrayPat, ObjectPat, Invalid, ParenExpr);

impl StartsWithAlphaNum for MemberExpr {
    #[inline]
    fn starts_with_alpha_num(&self) -> bool {
        self.obj.starts_with_alpha_num()
    }
}

impl StartsWithAlphaNum for PropName {
    #[inline]
    fn starts_with_alpha_num(&self) -> bool {
        match self {
            PropName::Str(_) | PropName::Computed(_) => false,
            PropName::Ident(_) | PropName::Num(_) | PropName::BigInt(_) => true,
        }
    }
}

impl StartsWithAlphaNum for Expr {
    fn starts_with_alpha_num(&self) -> bool {
        match self {
            Expr::Ident(_)
            | Expr::Lit(Lit::Bool(_))
            | Expr::Lit(Lit::Num(_))
            | Expr::Lit(Lit::Null(_))
            | Expr::Lit(Lit::BigInt(_))
            | Expr::Await(_)
            | Expr::Fn(_)
            | Expr::Class(_)
            | Expr::This(_)
            | Expr::Yield(_)
            | Expr::New(_)
            | Expr::MetaProp(_) => true,

            Expr::PrivateName(_) => false,

            // Handle other literals.
            Expr::Lit(_) => false,

            Expr::Seq(SeqExpr { exprs, .. }) => exprs
                .first()
                .map(|e| e.starts_with_alpha_num())
                .unwrap_or(false),

            //
            Expr::Assign(AssignExpr { left, .. }) => left.starts_with_alpha_num(),

            Expr::Bin(BinExpr { left, .. }) | Expr::Cond(CondExpr { test: left, .. }) => {
                left.starts_with_alpha_num()
            }
            Expr::Call(CallExpr { callee: left, .. }) => left.starts_with_alpha_num(),
            Expr::Member(MemberExpr { obj: left, .. }) => left.starts_with_alpha_num(),

            Expr::Unary(UnaryExpr { op, .. }) => {
                matches!(op, op!("void") | op!("delete") | op!("typeof"))
            }

            Expr::Arrow(expr) => {
                if expr.is_async {
                    true
                } else {
                    match expr.params.as_slice() {
                        [p] => p.pat.starts_with_alpha_num(),
                        _ => false,
                    }
                }
            }

            Expr::Update(expr) => {
                if expr.prefix {
                    false
                } else {
                    expr.arg.starts_with_alpha_num()
                }
            }

            Expr::Tpl(_) | Expr::Array(_) | Expr::Object(_) | Expr::Paren(_) => false,

            Expr::TaggedTpl(TaggedTpl { tag, .. }) => tag.starts_with_alpha_num(),


            Expr::OptChain(e) => e.expr.starts_with_alpha_num(),

            Expr::Invalid(..) => true,
        }
    }
}

impl StartsWithAlphaNum for Pat {
    fn starts_with_alpha_num(&self) -> bool {
        match *self {
            Pat::Ident(..) => true,
            Pat::Assign(AssignPat { ref left, .. }) => left.starts_with_alpha_num(),
            Pat::Object(..) | Pat::Array(..) | Pat::Rest(..) => false,
            Pat::Expr(ref expr) => expr.starts_with_alpha_num(),
            Pat::Invalid(..) => true,
        }
    }
}

impl StartsWithAlphaNum for ExprOrSpread {
    fn starts_with_alpha_num(&self) -> bool {
        match self {
            ExprOrSpread::Spread(_) => false,
            ExprOrSpread::Expr(e) => e.starts_with_alpha_num(),
        }
    }
}

impl StartsWithAlphaNum for Stmt {
    fn starts_with_alpha_num(&self) -> bool {
        match self {
            Stmt::Expr(expr) => expr.expr.starts_with_alpha_num(),
            Stmt::Decl(decl) => decl.starts_with_alpha_num(),
            Stmt::Debugger(..)
            | Stmt::With(..)
            | Stmt::While(..)
            | Stmt::DoWhile(..)
            | Stmt::Return(..)
            | Stmt::Labeled(..)
            | Stmt::Break(..)
            | Stmt::Continue(..)
            | Stmt::Switch(..)
            | Stmt::Throw(..)
            | Stmt::Try(..)
            | Stmt::For(..)
            | Stmt::ForIn(..)
            | Stmt::ForOf(..)
            | Stmt::If(..) => true,
            Stmt::Block(..) | Stmt::Empty(..) => false,
        }
    }
}

impl StartsWithAlphaNum for Decl {
    fn starts_with_alpha_num(&self) -> bool {
        match self {
            Decl::Class(..) | Decl::Fn(..) | Decl::Var(..) => true,
        }
    }
}

impl StartsWithAlphaNum for ExprOrSuper {
    fn starts_with_alpha_num(&self) -> bool {
        match self {
            ExprOrSuper::Super(_) => true,
            ExprOrSuper::Expr(e) => e.starts_with_alpha_num(),
        }
    }
}

impl StartsWithAlphaNum for PatOrExpr {
    fn starts_with_alpha_num(&self) -> bool {
        match self {
            PatOrExpr::Expr(n) => n.starts_with_alpha_num(),
            PatOrExpr::Pat(n) => n.starts_with_alpha_num(),
        }
    }
}
