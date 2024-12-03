#![recursion_limit = "1024"]

pub use self::config::Config;
use self::{list::ListFormat, util::SourceMapperExt};
use ast::*;
use global_common::{sync::Lrc, BytePos, SourceMap, Span, DUMMY_SP};
use std::{borrow::Cow, fmt::Write, io};
pub use text_writer::JsWriter;
use util::{for_var_ends_with_alpha_num, prop_name_starts_with_alpha_num};

#[macro_use]
pub mod macros;
mod config;
mod decl;
mod expr;
pub mod list;
mod stmt;
#[cfg(test)]
mod tests;
pub mod text_writer;
pub mod util;

pub type Result = io::Result<()>;

pub struct Emitter<'a> {
    cfg: config::Config,
    cm: Lrc<SourceMap>,
    wr: JsWriter<'a>,

    program_data: &'a ProgramData,

    ctx: Context,
    flags: Flags,
}

impl<'a> Emitter<'a> {
    pub fn new(
        cfg: config::Config,
        cm: Lrc<SourceMap>,
        wr: JsWriter<'a>,

        program_data: &'a ProgramData,
    ) -> Self {
        Self {
            cfg,
            cm,
            wr,
            program_data,
            ctx: Context::Default,
            flags: Flags::empty(),
        }
    }

    pub fn emit_program(&mut self, node: &Program) -> Result {
        match node {
            Program::Module(m) => self.emit_module(m),
            Program::Script(s) => self.emit_script(s),
        }
    }

    pub fn emit_module(&mut self, node: &Module) -> Result {
        if let Some(shebang) = &node.shebang {
            punct!(self, "#!");
            self.wr.write_str_lit(DUMMY_SP, shebang)?;
            self.wr.write_line()?;
        }
        for stmt in &node.body {
            self.emit_module_item(stmt, true)?;
        }
        Ok(())
    }

    pub fn emit_script(&mut self, node: &Script) -> Result {
        if let Some(shebang) = &node.shebang {
            punct!(self, "#!");
            self.wr.write_str_lit(DUMMY_SP, shebang)?;
            self.wr.write_line()?;
        }
        for stmt in &node.body {
            self.emit_stmt(stmt, true)?;
        }
        Ok(())
    }

    fn emit_module_item(&mut self, node: &ModuleItem, ignore_empty: bool) -> Result {
        match node {
            ModuleItem::Stmt(stmt) => self.emit_stmt(stmt, ignore_empty),
            ModuleItem::ModuleDecl(decl) => self.emit_module_decl(decl),
        }
    }

    fn emit_module_decl(&mut self, node: &ModuleDecl) -> Result {
        match node {
            ModuleDecl::Import(d) => self.emit_import(d),
            ModuleDecl::ExportDecl(d) => self.emit_export_decl(d),
            ModuleDecl::ExportNamed(d) => self.emit_named_export(d),
            ModuleDecl::ExportDefaultDecl(d) => self.emit_export_default_decl(d),
            ModuleDecl::ExportDefaultExpr(n) => self.emit_export_default_expr(n),
            ModuleDecl::ExportAll(d) => self.emit_export_all(d),
        }?;
        self.wr.write_line()
    }

    fn emit_export_decl(&mut self, node: &ExportDecl) -> Result {
        keyword!(self, "export");
        space!(self);
        self.emit_decl(&node.decl)
    }

    fn emit_export_default_expr(&mut self, node: &ExportDefaultExpr) -> Result {
        keyword!(self, "export");
        space!(self);
        keyword!(self, "default");

        let needs_parens = match node.expr.as_ref() {
            Expr::Arrow(..) | Expr::Seq(..) => true,
            Expr::Fn(FnExpr { ident: Some(_), .. })
            | Expr::Class(ClassExpr { ident: Some(_), .. }) => true,
            _ => false,
        };

        let old = self.ctx;
        self.ctx = Context::Default;

        if !needs_parens && self.expr_starts_with_alpha_num(&node.expr)? {
            space!(self);
        } else {
            formatting_space!(self);
        }

        if needs_parens {
            punct!(self, "(");
            self.emit_expr(&node.expr)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&node.expr)?;
        }

        self.ctx = old;

        formatting_semi!(self);
        Ok(())
    }

    fn emit_export_default_decl(&mut self, node: &ExportDefaultDecl) -> Result {
        keyword!(self, "export");
        space!(self);
        keyword!(self, "default");
        space!(self);
        match &node.decl {
            DefaultDecl::Class(n) => self.emit_class_expr(n),
            DefaultDecl::Fn(n) => self.emit_fn_expr(n),
        }?;
        formatting_semi!(self);
        Ok(())
    }

    fn emit_import(&mut self, node: &ImportDecl) -> Result {
        let span = get_span!(self, node.node_id);

        keyword!(self, "import");
        let starts_with_ident = !node.specifiers.is_empty()
            && matches!(node.specifiers[0], ImportSpecifier::Default(_));
        if starts_with_ident {
            space!(self);
        } else {
            formatting_space!(self);
        }

        let mut specifiers = vec![];
        let mut emitted_default = false;
        let mut emitted_ns = false;
        for specifier in &node.specifiers {
            match specifier {
                ImportSpecifier::Named(s) => {
                    specifiers.push(s);
                }
                ImportSpecifier::Default(s) => {
                    self.emit_ident(&s.local)?;
                    emitted_default = true;
                }
                ImportSpecifier::Namespace(ns) => {
                    if emitted_default {
                        punct!(self, ",");
                        formatting_space!(self);
                    }

                    emitted_ns = true;

                    assert!(node.specifiers.len() <= 2);
                    punct!(self, "*");
                    formatting_space!(self);
                    keyword!(self, "as");
                    space!(self);
                    self.emit_ident(&ns.local)?;
                }
            }
        }

        if specifiers.is_empty() {
            space!(self);
            if emitted_ns || emitted_default {
                keyword!(self, "from");
            }
        } else {
            if emitted_default {
                punct!(self, ",");
                formatting_space!(self);
            }

            punct!(self, "{");
            self.emit_list(
                span,
                &specifiers,
                |e, n| e.emit_import_specifier(n),
                ListFormat::NamedImportsOrExportsElements,
            )?;
            punct!(self, "}");
            formatting_space!(self);

            keyword!(self, "from");
        }

        formatting_space!(self);
        self.emit_str_lit(&node.src)?;
        formatting_semi!(self);
        Ok(())
    }

    fn emit_import_specifier(&mut self, node: &ImportNamedSpecifier) -> Result {
        if let Some(imported) = &node.imported {
            self.emit_ident(imported)?;
            space!(self);
            keyword!(self, "as");
            space!(self);
        }

        self.emit_ident(&node.local)
    }

    fn emit_export_specifier(&mut self, node: &ExportSpecifier) -> Result {
        match node {
            ExportSpecifier::Default(_) => {
                unimplemented!("codegen of `export default from 'foo';`")
            }
            ExportSpecifier::Namespace(node) => self.emit_namespace_export_specifier(node),
            ExportSpecifier::Named(node) => self.emit_named_export_specifier(node),
        }
    }

    fn emit_namespace_export_specifier(&mut self, node: &ExportNamespaceSpecifier) -> Result {
        punct!(self, "*");
        formatting_space!(self);
        keyword!(self, "as");
        space!(self);
        self.emit_ident(&node.name)
    }

    fn emit_named_export_specifier(&mut self, node: &ExportNamedSpecifier) -> Result {
        if let Some(exported) = &node.exported {
            self.emit_ident(&node.orig)?;
            space!(self);
            keyword!(self, "as");
            space!(self);
            self.emit_ident(exported)
        } else {
            self.emit_ident(&node.orig)
        }
    }

    fn emit_named_export(&mut self, node: &NamedExport) -> Result {
        let span = get_span!(self, node.node_id);

        struct Specifiers<'a> {
            has_namespace_spec: bool,
            namespace_spec: Option<&'a ExportNamespaceSpecifier>,
            has_named_specs: bool,
            named_specs: Vec<&'a ExportSpecifier>,
        }
        let Specifiers {
            has_namespace_spec,
            namespace_spec,
            has_named_specs,
            named_specs,
        } = node.specifiers.iter().fold(
            Specifiers {
                has_namespace_spec: false,
                namespace_spec: None,
                has_named_specs: false,
                named_specs: vec![],
            },
            |mut result, s| match s {
                ExportSpecifier::Namespace(spec) => {
                    result.has_namespace_spec = true;
                    // There can only be one namespace export specifier.
                    if result.namespace_spec.is_none() {
                        result.namespace_spec = Some(spec)
                    }
                    result
                }
                spec => {
                    result.has_named_specs = true;
                    result.named_specs.push(spec);
                    result
                }
            },
        );

        keyword!(self, "export");
        formatting_space!(self);
        if let Some(spec) = namespace_spec {
            self.emit_namespace_export_specifier(spec)?;
            if has_named_specs {
                punct!(self, ",");
                formatting_space!(self);
            }
        }
        if has_named_specs || (!has_namespace_spec && !has_named_specs) {
            punct!(self, "{");
            self.emit_list(
                span,
                &named_specs,
                |e, n| e.emit_export_specifier(n),
                ListFormat::NamedImportsOrExportsElements,
            )?;
            punct!(self, "}");
        }

        if let Some(src) = &node.src {
            if has_named_specs || (!has_namespace_spec && !has_named_specs) {
                formatting_space!(self);
            } else if has_namespace_spec {
                space!(self);
            }
            keyword!(self, "from");
            formatting_space!(self);
            self.emit_str_lit(src)?;
        }
        formatting_semi!(self);
        Ok(())
    }

    fn emit_export_all(&mut self, node: &ExportAll) -> Result {
        keyword!(self, "export");
        formatting_space!(self);
        punct!(self, "*");
        formatting_space!(self);
        keyword!(self, "from");
        formatting_space!(self);
        self.emit_str_lit(&node.src)?;
        formatting_semi!(self);
        Ok(())
    }

    fn emit_lit(&mut self, node: &Lit) -> Result {
        let span = get_span!(self, node.node_id());

        match node {
            Lit::Bool(Bool { value, .. }) => {
                if *value {
                    keyword!(self, span, "true")
                } else {
                    keyword!(self, span, "false")
                }
            }
            Lit::Null(_) => keyword!(self, span, "null"),
            Lit::Str(s) => self.emit_str_lit(s)?,
            Lit::BigInt(s) => self.emit_big_lit(s)?,
            Lit::Num(n) => self.emit_num_lit(n)?,
            Lit::Regex(n) => {
                punct!(self, "/");
                self.wr.write_str(&n.exp)?;
                punct!(self, "/");
                self.wr.write_str(&n.flags)?;
            }
        }
        Ok(())
    }

    fn emit_str_lit(&mut self, node: &Str) -> Result {
        self.wr.commit_pending_semi()?;

        let target = self.cfg.target;
        let value = get_quoted_utf16(&node.value, target);
        self.wr.write_str_lit(DUMMY_SP, &value)
    }

    fn emit_num_lit(&mut self, num: &Number) -> Result {
        self.emit_num_lit_internal(num, false)?;
        Ok(())
    }

    /// `1.toString` is an invalid property access,
    /// should emit a dot after the literal if return true
    fn emit_num_lit_internal(
        &mut self,
        num: &Number,
        detect_dot: bool,
    ) -> std::result::Result<bool, io::Error> {
        let span = get_span!(self, num.node_id);

        // Handle infinity
        if num.value.is_infinite() {
            if num.value.is_sign_negative() {
                self.wr.write_str_lit(span, "-")?;
            }
            self.wr.write_str_lit(span, "Infinity")?;

            return Ok(false);
        }

        let value = if self.cfg.minify {
            minify_number(num.value)
        } else {
            num.value.to_string()
        };
        self.wr.write_str_lit(span, &value)?;

        // fast return
        if !detect_dot {
            return Ok(false);
        }

        let bytes = value.as_bytes();

        if !bytes.contains(&b'.') && !bytes.contains(&b'e') {
            return Ok(true);
        }

        Ok(false)
    }

    fn emit_big_lit(&mut self, v: &BigInt) -> Result {
        let span = get_span!(self, v.node_id);

        self.wr.write_lit(span, &v.value.to_string())?;
        self.wr.write_lit(span, "n")
    }

    // fn emit_object_binding_pat(&mut self, node: &ObjectPat) -> Result {
    //     self.wr.write_punct("{")?;
    //     self.emit_list(
    //         node.span(),
    //         &node.props,
    //         ListFormat::ObjectBindingPatternElements,
    //     );
    //     self.wr.write_punct("}")?;

    //     Ok(())
    // }

    // fn emit_array_binding_pat(&mut self, node: &ArrayPat) -> Result {
    //     self.wr.write_punct("[")?;
    //     self.emit_list(
    //         node.span(),
    //         &node.elems,
    //         ListFormat::ArrayBindingPatternElements,
    //     );
    //     self.wr.write_punct("]")?;

    //     Ok(())
    // }

    fn emit_expr_or_super(&mut self, node: &ExprOrSuper) -> Result {
        match node {
            ExprOrSuper::Expr(e) => self.emit_expr(e),
            ExprOrSuper::Super(n) => self.emit_super(n),
        }
    }

    fn emit_super(&mut self, node: &Super) -> Result {
        keyword!(self, get_span!(self, node.node_id), "super");
        Ok(())
    }

    fn emit_expr(&mut self, node: &Expr) -> Result {
        let ctx = self.ctx;

        let in_expr_stmt_seq = self.flags.replace(Flags::in_expr_stmt_seq, false);

        if ctx == Context::Default {
            match node {
                // might have a child expr in start of stmt
                Expr::OptChain(_)
                | Expr::Member(_)
                | Expr::Bin(_)
                | Expr::Assign(_)
                | Expr::Seq(_)
                | Expr::Cond(_)
                | Expr::TaggedTpl(_)
                | Expr::Update(UpdateExpr { prefix: false, .. }) => {}
                _ => self.ctx = Context::FreeExpr,
            }
        }

        match node {
            Expr::Array(n) => self.emit_array_lit(n),
            Expr::Arrow(n) => self.emit_arrow_expr(n),
            Expr::Assign(n) => {
                let needs_parens = in_expr_stmt_seq
                    && matches!(&n.left, PatOrExpr::Pat(p) if matches!(p.as_ref(), Pat::Object(_)));

                if needs_parens {
                    punct!(self, "(");
                    self.emit_assign_expr(n)?;
                    punct!(self, ")");
                } else {
                    self.emit_assign_expr(n)?;
                }

                Ok(())
            }
            Expr::Await(n) => self.emit_await_expr(n),
            Expr::Bin(n) => {
                let wrap_left = ctx == Context::Default
                    && matches!(
                        n.left.as_ref(),
                        Expr::Object(..) | Expr::Fn(..) | Expr::Class(..)
                    );

                if n.op == op!("in") && self.flags.contains(Flags::in_for_stmt_head) {
                    punct!(self, "(");
                    self.emit_bin_expr(n, wrap_left)?;
                    punct!(self, ")");
                } else {
                    self.emit_bin_expr(n, wrap_left)?;
                }
                Ok(())
            }
            Expr::Call(n) => {
                let mut wrap_expr = false;
                let wrap_callee = match &n.callee {
                    ExprOrSuper::Super(_) => false,
                    ExprOrSuper::Expr(callee) => match callee.as_ref() {
                        // Function expression cannot start with `function`
                        Expr::Fn(_) => match ctx {
                            Context::ForcedExpr | Context::FreeExpr => false,

                            Context::Callee { is_new: true } => {
                                // wrap_expr = true;
                                false
                            }

                            _ => true,
                        },
                        Expr::OptChain(_) if !self.flags.contains(Flags::in_opt_chain) => {
                            wrap_expr = true;
                            false
                        }
                        _ => false,
                    },
                };
                if wrap_expr {
                    punct!(self, "(");
                    self.emit_call_expr(n, wrap_callee)?;
                    punct!(self, ")");
                } else {
                    self.emit_call_expr(n, wrap_callee)?;
                }

                Ok(())
            }
            Expr::Class(n) => {
                if in_expr_stmt_seq {
                    punct!(self, "(");
                    self.emit_class_expr(n)?;
                    punct!(self, ")");
                } else {
                    self.emit_class_expr(n)?;
                }
                Ok(())
            }
            Expr::Cond(n) => {
                let wrap_test = matches!(
                    n.test.as_ref(),
                    Expr::Object(_) | Expr::Fn(_) | Expr::Class(_)
                ) && ctx == Context::Default;
                if let Context::Callee { is_new: true } = ctx {
                    punct!(self, "(");
                    self.emit_cond_expr(n, wrap_test)?;
                    punct!(self, ")");
                } else {
                    self.emit_cond_expr(n, wrap_test)?;
                }

                Ok(())
            }
            Expr::Fn(n) => {
                if in_expr_stmt_seq {
                    punct!(self, "(");
                    self.emit_fn_expr(n)?;
                    punct!(self, ")");
                } else {
                    self.emit_fn_expr(n)?;
                }
                Ok(())
            }
            Expr::Ident(n) => self.emit_ident(n),
            Expr::Lit(n) => self.emit_lit(n),
            Expr::Member(n) => self.emit_member_expr(n),
            Expr::MetaProp(n) => self.emit_meta_prop_expr(n),
            Expr::New(n) => self.emit_new_expr(n),
            Expr::Object(n) => {
                if in_expr_stmt_seq {
                    punct!(self, "(");
                    self.emit_object_lit(n)?;
                    punct!(self, ")");
                } else {
                    self.emit_object_lit(n)?;
                }
                Ok(())
            }
            Expr::Seq(n) => {
                if ctx == Context::ForcedExpr {
                    punct!(self, "(");
                    self.emit_seq_expr(n)?;
                    punct!(self, ")");
                } else {
                    self.flags.set(Flags::in_expr_stmt_seq, in_expr_stmt_seq);
                    self.emit_seq_expr(n)?;
                }

                Ok(())
            }
            Expr::TaggedTpl(n) => self.emit_tagged_tpl_lit(n),
            Expr::This(n) => self.emit_this_expr(n),
            Expr::Tpl(n) => self.emit_tpl_lit(n),
            Expr::Unary(n) => self.emit_unary_expr(n),
            Expr::Update(n) => self.emit_update_expr(n),
            Expr::Yield(n) => self.emit_yield_expr(n),
            Expr::PrivateName(n) => self.emit_private_name(n),

            Expr::OptChain(n) => {
                let mut wrap_expr = false;
                let mut wrap_callee = false;
                if let Expr::Call(base) = n.expr.as_ref() {
                    match &base.callee {
                        ExprOrSuper::Expr(callee) => match callee.as_ref() {
                            Expr::Seq(_) | Expr::Arrow(_) | Expr::Await(_) | Expr::Assign(_) => {
                                wrap_callee = true;
                            }
                            Expr::Fn(_) => match ctx {
                                Context::ForcedExpr | Context::FreeExpr => {}

                                Context::Callee { is_new: true } => {
                                    wrap_expr = true;
                                }

                                _ => {
                                    wrap_callee = true;
                                }
                            },
                            _ => {}
                        },
                        ExprOrSuper::Super(_) => {}
                    }
                }
                if wrap_expr {
                    punct!(self, "(");
                    self.emit_opt_chain(n, wrap_callee)?;
                    punct!(self, ")");
                } else {
                    self.emit_opt_chain(n, wrap_callee)?;
                }
                Ok(())
            }
            Expr::Invalid(n) => self.emit_invalid(n),
        }?;

        self.flags.set(Flags::in_expr_stmt_seq, in_expr_stmt_seq);

        self.ctx = ctx;
        Ok(())
    }

    fn emit_opt_chain(&mut self, n: &OptChainExpr, wrap_callee: bool) -> Result {
        let span = get_span!(self, n.node_id);

        match n.expr.as_ref() {
            Expr::Member(e) => {
                let in_opt_chain = self.flags.replace(Flags::in_opt_chain, true);
                self.emit_expr_or_super(&e.obj)?;
                punct!(self, "?.");

                if e.computed {
                    punct!(self, "[");
                    self.emit_expr(&e.prop)?;
                    punct!(self, "]");
                } else {
                    self.emit_expr(&e.prop)?;
                }
                self.flags.set(Flags::in_opt_chain, in_opt_chain);
            }
            Expr::Call(e) => {
                let ctx = std::mem::replace(&mut self.ctx, Context::Callee { is_new: false });
                let in_opt_chain = self.flags.replace(Flags::in_opt_chain, true);

                let wrap_callee = wrap_callee
                    || match &e.callee {
                        ExprOrSuper::Super(_) => false,
                        ExprOrSuper::Expr(callee) => match callee.as_ref() {
                            Expr::Lit(Lit::Num(..) | Lit::Str(..)) => false,
                            Expr::Cond(..)
                            | Expr::Class(..)
                            | Expr::Bin(..)
                            | Expr::Lit(..)
                            | Expr::Unary(..)
                            | Expr::Object(..)
                            | Expr::Await(..)
                            | Expr::Yield(..) => true,
                            _ => false,
                        },
                    };

                if wrap_callee {
                    punct!(self, "(");
                }
                match &e.callee {
                    ExprOrSuper::Expr(callee) => {
                        if let Expr::New(new) = callee.as_ref() {
                            self.emit_new(new, false)?;
                        } else {
                            self.emit_expr(callee)?;
                        }
                    }
                    ExprOrSuper::Super(callee) => {
                        self.emit_super(callee)?;
                    }
                }

                self.flags.set(Flags::in_opt_chain, in_opt_chain);

                if wrap_callee {
                    punct!(self, ")");
                }
                punct!(self, "?.");

                self.ctx = Context::ForcedExpr;
                punct!(self, "(");
                self.emit_expr_or_spreads(span, &e.args, ListFormat::CallExpressionArguments)?;
                punct!(self, ")");
                self.ctx = ctx;
            }
            _ => {}
        }
        Ok(())
    }

    fn emit_invalid(&mut self, n: &Invalid) -> Result {
        let span = get_span!(self, n.node_id);

        self.wr.write_str_lit(span, "<invalid>")
    }

    fn emit_call_expr(&mut self, node: &CallExpr, wrap_callee: bool) -> Result {
        let ctx = std::mem::replace(&mut self.ctx, Context::Callee { is_new: false });

        let span = get_span!(self, node.node_id);

        match &node.callee {
            ExprOrSuper::Expr(callee) => {
                if let Expr::New(new) = callee.as_ref() {
                    self.emit_new(new, false)?;
                } else {
                    let needs_parens = wrap_callee
                        || match callee.as_ref() {
                            Expr::Lit(Lit::Num(..) | Lit::Str(..)) => false,
                            Expr::Cond(..)
                            | Expr::Class(..)
                            | Expr::Bin(..)
                            | Expr::Lit(..)
                            | Expr::Unary(..)
                            | Expr::Object(..)
                            | Expr::Await(..)
                            | Expr::Yield(..)
                            | Expr::Seq(_)
                            | Expr::Arrow(_)
                            | Expr::Assign(_) => true,
                            _ => false,
                        };
                    if needs_parens {
                        punct!(self, "(");
                        self.emit_expr(callee)?;
                        punct!(self, ")");
                    } else {
                        self.emit_expr(callee)?;
                    }
                }
            }
            ExprOrSuper::Super(callee) => {
                self.emit_super(callee)?;
            }
        }

        punct!(self, "(");
        self.ctx = Context::ForcedExpr;
        self.emit_expr_or_spreads(span, &node.args, ListFormat::CallExpressionArguments)?;
        punct!(self, ")");
        self.ctx = ctx;
        Ok(())
    }

    fn emit_new(&mut self, node: &NewExpr, should_ignore_empty_args: bool) -> Result {
        let span = get_span!(self, node.node_id);

        {
            let span = self.cm.span_until_char(span, ' ');
            keyword!(self, span, "new");
        }

        let callee_needs_parens = matches!(
            node.callee.as_ref(),
            Expr::Call(..)
                | Expr::Await(..)
                | Expr::Yield(..)
                | Expr::Bin(..)
                | Expr::Assign(..)
                | Expr::Seq(..)
                | Expr::Unary(..)
                | Expr::Lit(..)
        );

        let ctx = std::mem::replace(&mut self.ctx, Context::Callee { is_new: true });
        if !callee_needs_parens && self.expr_starts_with_alpha_num(&node.callee)? {
            space!(self);
        } else {
            formatting_space!(self);
        }
        if callee_needs_parens {
            punct!(self, "(");
            self.emit_expr(&node.callee)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&node.callee)?;
        }
        self.ctx = ctx;

        if let Some(args) = &node.args {
            if !(self.cfg.minify && args.is_empty() && should_ignore_empty_args) {
                punct!(self, "(");
                let ctx = std::mem::replace(&mut self.ctx, Context::ForcedExpr);
                self.emit_expr_or_spreads(span, args, ListFormat::NewExpressionArguments)?;
                self.ctx = ctx;
                punct!(self, ")");
            }
        }

        Ok(())
    }

    fn emit_new_expr(&mut self, node: &NewExpr) -> Result {
        self.emit_new(node, true)
    }

    fn emit_member_expr(&mut self, node: &MemberExpr) -> Result {
        let mut needs_2dots_for_property_access = false;

        let in_assign_lhs_member_expr = self.flags.replace(Flags::in_assign_lhs_member_expr, false);

        match &node.obj {
            ExprOrSuper::Expr(obj) => match obj.as_ref() {
                Expr::New(new) => {
                    if new.args.is_none() {
                        punct!(self, "(");
                        self.emit_new(new, false)?;
                        punct!(self, ")");
                    } else {
                        self.emit_new(new, false)?;
                    }
                }
                Expr::Lit(Lit::Num(num)) => {
                    if num.value.signum() == -1. {
                        punct!(self, "(");
                        needs_2dots_for_property_access = self.emit_num_lit_internal(num, true)?;
                        punct!(self, ")");
                    } else {
                        needs_2dots_for_property_access = self.emit_num_lit_internal(num, true)?;
                    }
                }
                _ => {
                    let needs_parens = match obj.as_ref() {
                        Expr::Object(..) if self.ctx == Context::ForcedExpr => false,
                        Expr::Fn(..)
                        | Expr::Cond(..)
                        | Expr::Unary(..)
                        | Expr::Seq(..)
                        | Expr::Update(..)
                        | Expr::Bin(..)
                        | Expr::Object(..)
                        | Expr::Assign(..)
                        | Expr::Arrow(..)
                        | Expr::Class(..)
                        | Expr::Yield(..)
                        | Expr::Await(..) => true,
                        Expr::Call(..) if self.ctx == Context::Callee { is_new: true } => true,
                        Expr::OptChain(..) if !self.flags.contains(Flags::in_opt_chain) => true,
                        Expr::OptChain(..) if in_assign_lhs_member_expr => true,
                        _ => false,
                    };
                    if in_assign_lhs_member_expr && matches!(obj.as_ref(), Expr::Member(_)) {
                        self.flags.set(Flags::in_assign_lhs_member_expr, true);
                    }
                    if needs_parens {
                        punct!(self, "(");
                        self.emit_expr(obj)?;
                        punct!(self, ")");
                    } else {
                        self.emit_expr(obj)?;
                    }
                    self.flags.set(Flags::in_assign_lhs_member_expr, false);
                }
            },
            ExprOrSuper::Super(obj) => {
                self.emit_super(obj)?;
            }
        }

        if node.computed {
            punct!(self, "[");
            self.emit_expr(&node.prop)?;
            punct!(self, "]");
        } else {
            match node.prop.as_ref() {
                Expr::Ident(ident) => {
                    if needs_2dots_for_property_access {
                        punct!(self, ".");
                    }
                    punct!(self, ".");
                    self.emit_ident(ident)?;
                }
                Expr::PrivateName(private) => {
                    if needs_2dots_for_property_access {
                        punct!(self, ".");
                    }
                    punct!(self, ".");
                    self.emit_private_name(private)?;
                }
                _ => unreachable!(),
            }
        }
        Ok(())
    }

    fn emit_arrow_expr(&mut self, node: &ArrowExpr) -> Result {
        let span = get_span!(self, node.node_id);

        let space = !self.cfg.minify
            || matches!(
                node.params.as_slice(),
                [Param {
                    pat: Pat::Ident(_),
                    ..
                }]
            );

        if node.is_async {
            keyword!(self, "async");
            if space {
                space!(self);
            } else {
                formatting_space!(self);
            }
        }

        let parens = !self.cfg.minify
            || !matches!(
                node.params.as_slice(),
                [Param {
                    pat: Pat::Ident(_),
                    ..
                }]
            );

        if parens {
            punct!(self, "(");
        }

        let old = self.ctx;
        self.ctx = Context::Default;

        self.emit_list(
            span,
            &node.params,
            |e, n| e.emit_param(n),
            ListFormat::CommaListElements,
        )?;
        if parens {
            punct!(self, ")");
        }

        punct!(self, "=>");
        if node.body.stmts.len() == 1 {
            if let Stmt::Return(ret) = &node.body.stmts[0] {
                self.wr.increase_indent()?;
                if let Some(expr) = &ret.arg {
                    let needs_parens = match expr.as_ref() {
                        Expr::Seq(_) | Expr::Object(_) => true,
                        Expr::Assign(e) => {
                            matches!(&e.left, PatOrExpr::Pat(p) if matches!(p.as_ref(), Pat::Array(_) | Pat::Object(_)))
                        }
                        _ => false,
                    };
                    if needs_parens {
                        punct!(self, "(");
                        self.emit_expr(expr)?;
                        punct!(self, ")");
                    } else {
                        self.emit_expr(expr)?;
                    }
                } else {
                    punct!(self, "{");
                    punct!(self, "}");
                }
                self.wr.decrease_indent()?;
                if !self.cfg.minify {
                    self.wr.write_line()?;
                }
            } else {
                self.emit_block_stmt(&node.body)?;
            }
        } else {
            self.emit_block_stmt(&node.body)?;
        }
        self.ctx = old;
        Ok(())
    }

    fn emit_meta_prop_expr(&mut self, node: &MetaPropExpr) -> Result {
        self.emit_ident(&node.meta)?;
        punct!(self, ".");
        self.emit_ident(&node.prop)
    }

    fn emit_seq_expr(&mut self, node: &SeqExpr) -> Result {
        assert!(node.exprs.len() > 1);

        let mut first = true;
        //TODO: Indention
        for e in &node.exprs {
            if first {
                first = false;
                self.emit_expr(e)?;
            } else {
                punct!(self, ",");
                formatting_space!(self);
                let ctx = self.ctx;
                self.ctx = Context::FreeExpr;
                self.emit_expr(e)?;
                self.ctx = ctx;
            }
        }
        Ok(())
    }

    fn emit_assign_expr(&mut self, node: &AssignExpr) -> Result {
        let lhs_is_member = match &node.left {
            PatOrExpr::Expr(lhs) => {
                matches!(lhs.as_ref(), Expr::Member(_))
            }
            PatOrExpr::Pat(lhs) => {
                if let Pat::Expr(lhs) = lhs.as_ref() {
                    matches!(lhs.as_ref(), Expr::Member(_))
                } else {
                    false
                }
            }
        };

        self.flags
            .set(Flags::in_assign_lhs_member_expr, lhs_is_member);
        self.emit_pat_or_expr(&node.left)?;
        self.flags.set(Flags::in_assign_lhs_member_expr, false);
        formatting_space!(self);
        operator!(self, node.op.as_str());
        formatting_space!(self);
        let ctx = self.ctx;
        self.ctx = Context::FreeExpr;

        fn rhs_need_paren(e: &Expr) -> bool {
            match e {
                Expr::Assign(e) => rhs_need_paren(&e.right),
                Expr::Seq(..) => true,
                _ => false,
            }
        }

        if rhs_need_paren(&node.right) {
            punct!(self, "(");
            self.emit_expr(&node.right)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&node.right)?;
        }

        self.ctx = ctx;
        Ok(())
    }

    fn emit_bin_expr(&mut self, node: &BinExpr, wrap_left: bool) -> Result {
        fn get_parens(expr: &BinExpr) -> (bool, bool) {
            let mut left_needs_parens = false;
            let mut right_needs_parens = false;

            match expr.op {
                op!("||") | op!("&&") => match (&*expr.left, &*expr.right) {
                    (Expr::Update(..), Expr::Call(..)) => {
                        return (false, false);
                    }

                    (Expr::Update(..), Expr::Assign(..)) => {
                        return (false, true);
                    }

                    _ => {}
                },

                op!(">") | op!(">=") | op!("<") | op!("<=") => {
                    if let (Expr::Update(..) | Expr::Lit(..), Expr::Update(..) | Expr::Lit(..)) =
                        (&*expr.left, &*expr.right)
                    {
                        return (false, false);
                    }
                }

                op!("**") => match &*expr.left {
                    Expr::Unary(..) => {
                        left_needs_parens = true;
                    }
                    Expr::Lit(Lit::Num(v)) if v.value.is_sign_negative() => {
                        left_needs_parens = true;
                    }
                    _ => {}
                },

                _ => {}
            }

            match expr.right.as_ref() {
                Expr::Assign(..)
                | Expr::Seq(..)
                | Expr::Yield(..)
                | Expr::Cond(..)
                | Expr::Arrow(..) => {
                    right_needs_parens = true;
                }
                Expr::Bin(BinExpr { op: op_of_rhs, .. }) => {
                    if *op_of_rhs == expr.op {
                        // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence#precedence_and_associativity
                        // `**` is the only right associative operator in js
                        if !(matches!(expr.op, op!("??") | op!("||") | op!("&&"))
                            || expr.op == op!("**"))
                        {
                            right_needs_parens = true;
                        }
                    } else if op_of_rhs.precedence() <= expr.op.precedence()
                        || (*op_of_rhs == op!("&&") && expr.op == op!("??"))
                    {
                        right_needs_parens = true;
                    }
                }
                _ => {}
            };

            match expr.left.as_ref() {
                Expr::Bin(BinExpr { op: op!("??"), .. }) if expr.op != op!("??") => {
                    left_needs_parens = true;
                }

                // While simplifying, (1 + x) * Nan becomes `1 + x * Nan`.
                // But it should be `(1 + x) * Nan`
                Expr::Bin(BinExpr { op: op_of_lhs, .. }) => {
                    if op_of_lhs.precedence() < expr.op.precedence()
                        || (op_of_lhs.precedence() == expr.op.precedence() && expr.op == op!("**"))
                    {
                        left_needs_parens = true;
                    }
                }

                Expr::Unary(UnaryExpr {
                    op: op!("void"), ..
                }) if expr.op == op!("==")
                    || expr.op == op!("===")
                    || expr.op == op!("!=")
                    || expr.op == op!("!==") => {}

                Expr::Seq(..)
                | Expr::Unary(UnaryExpr {
                    op: op!("delete"), ..
                })
                | Expr::Unary(UnaryExpr {
                    op: op!("void"), ..
                })
                | Expr::Yield(..)
                | Expr::Cond(..)
                | Expr::Assign(..)
                | Expr::Arrow(..) => {
                    left_needs_parens = true;
                }
                Expr::Object(..)
                    if expr.op == op!("instanceof")
                        || expr.op == op!("==")
                        || expr.op == op!("===")
                        || expr.op == op!("!=")
                        || expr.op == op!("!==") =>
                {
                    left_needs_parens = true;
                }
                _ => {}
            }

            if let op!("??") = expr.op {
                match &*expr.left {
                    Expr::Bin(BinExpr { op, .. }) if *op != op!("??") => {
                        left_needs_parens = true;
                    }
                    _ => (),
                }
            }

            (left_needs_parens, right_needs_parens)
        }

        let (left_needs_parens, right_needs_parens) = get_parens(node);

        let left_needs_parens = left_needs_parens || wrap_left;

        if left_needs_parens {
            punct!(self, "(");
            self.emit_expr(&node.left)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&node.left)?;
        }

        let is_kwd_op = matches!(node.op, op!("in") | op!("instanceof"));

        let need_pre_space = !left_needs_parens
            && if self.cfg.minify {
                if is_kwd_op {
                    matches!(node.left.as_ref(), Expr::Lit(Lit::Regex(_)))
                        || self.expr_ends_with_alpha_num(&node.left)?
                } else {
                    // space is mandatory to avoid outputting -->
                    match *node.left {
                        Expr::Update(UpdateExpr {
                            prefix: false, op, ..
                        }) => matches!(
                            (op, node.op),
                            (op!("--"), op!(">") | op!(">>") | op!(">>>") | op!(">="))
                        ),
                        _ => false,
                    }
                }
            } else {
                is_kwd_op || matches!(*node.left, Expr::Update(UpdateExpr { prefix: false, .. }))
            };
        if need_pre_space {
            space!(self);
        } else {
            formatting_space!(self);
        }
        operator!(self, node.op.as_str());

        let ctx = self.ctx;
        self.ctx = Context::FreeExpr;

        let need_post_space = !right_needs_parens
            && if self.cfg.minify {
                if is_kwd_op {
                    self.expr_starts_with_alpha_num(&node.right)?
                } else {
                    require_space_before_rhs(&node.right, &node.op)
                }
            } else {
                is_kwd_op
                    || matches!(
                        *node.right,
                        Expr::Unary(..) | Expr::Update(UpdateExpr { prefix: true, .. })
                    )
            };
        if need_post_space {
            space!(self);
        } else {
            formatting_space!(self);
        }

        if right_needs_parens {
            punct!(self, "(");
            self.emit_expr(&node.right)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&node.right)?;
        }
        self.ctx = ctx;
        Ok(())
    }

    fn emit_class_expr(&mut self, node: &ClassExpr) -> Result {
        keyword!(self, "class");

        if let Some(i) = &node.ident {
            space!(self);
            self.emit_ident(i)?;
        }

        self.emit_class_trailing(&node.class)
    }

    fn emit_class_trailing(&mut self, node: &Class) -> Result {
        let ctx = std::mem::replace(&mut self.ctx, Context::Default);

        if let Some(extends) = &node.extends {
            space!(self);
            self.emit_extends_clause(extends)?;
        }

        formatting_space!(self);
        punct!(self, "{");
        let in_for_stmt_head = self.flags.replace(Flags::in_for_stmt_head, false);
        self.emit_list(
            get_span!(self, node.node_id),
            &node.body,
            |e: &mut Emitter, n| e.emit_class_member(n),
            ListFormat::ClassMembers,
        )?;
        self.flags.set(Flags::in_for_stmt_head, in_for_stmt_head);
        self.ctx = ctx;
        punct!(self, "}");
        Ok(())
    }

    fn emit_extends_clause(&mut self, node: &ExtendsClause) -> Result {
        keyword!(self, "extends");
        let needs_parens = matches!(
            node.super_class.as_ref(),
            Expr::Seq(_)
                | Expr::Await(_)
                | Expr::Yield(_)
                | Expr::Bin(_)
                | Expr::Assign(_)
                | Expr::Cond(_)
                | Expr::Unary(_)
        );
        if needs_parens {
            formatting_space!(self);
            punct!(self, "(");
            self.emit_expr(&node.super_class)?;
            punct!(self, ")");
            Ok(())
        } else {
            if self.expr_starts_with_alpha_num(&node.super_class)? {
                space!(self);
            } else {
                formatting_space!(self);
            }
            self.emit_expr(&node.super_class)
        }
    }

    fn emit_class_member(&mut self, node: &ClassMember) -> Result {
        match node {
            ClassMember::Constructor(n) => self.emit_class_constructor(n),
            ClassMember::ClassProp(n) => self.emit_class_prop(n),
            ClassMember::Method(n) => self.emit_class_method(n),
            ClassMember::PrivateMethod(n) => self.emit_private_method(n),
            ClassMember::PrivateProp(n) => self.emit_private_prop(n),
            ClassMember::Empty(n) => self.emit_empty_stmt(n, true),
        }
    }

    fn emit_private_method(&mut self, n: &PrivateMethod) -> Result {
        if n.is_static {
            keyword!(self, "static");
            space!(self);
        }
        match n.kind {
            MethodKind::Method => {
                if n.function.is_async {
                    keyword!(self, "async");
                }
                space!(self);
                if n.function.is_generator {
                    punct!(self, "*");
                }
            }
            MethodKind::Getter => {
                keyword!(self, "get");
                space!(self);
            }
            MethodKind::Setter => {
                keyword!(self, "set");
                space!(self);
            }
        }
        self.emit_private_name(&n.key)?;

        self.emit_fn_trailing(&n.function)
    }

    fn emit_class_method(&mut self, n: &ClassMethod) -> Result {
        if n.is_static {
            keyword!(self, "static");

            let starts_with_alpha_num = match n.kind {
                MethodKind::Method => {
                    if n.function.is_async {
                        true
                    } else if n.function.is_generator {
                        false
                    } else {
                        prop_name_starts_with_alpha_num(&n.key)
                    }
                }
                MethodKind::Getter | MethodKind::Setter => true,
            };

            if starts_with_alpha_num {
                space!(self);
            } else {
                formatting_space!(self);
            }
        }
        match n.kind {
            MethodKind::Method => {
                if n.function.is_async {
                    keyword!(self, "async");
                    space!(self);
                }
                if n.function.is_generator {
                    punct!(self, "*");
                }
            }
            MethodKind::Getter => {
                keyword!(self, "get");

                if prop_name_starts_with_alpha_num(&n.key) {
                    space!(self);
                } else {
                    formatting_space!(self)
                }
            }
            MethodKind::Setter => {
                keyword!(self, "set");

                if prop_name_starts_with_alpha_num(&n.key) {
                    space!(self);
                } else {
                    formatting_space!(self)
                }
            }
        }
        self.emit_prop_name(&n.key)?;

        punct!(self, "(");
        self.emit_list(
            get_span!(self, n.function.node_id),
            &n.function.params,
            |e, n| e.emit_param(n),
            ListFormat::CommaListElements,
        )?;
        punct!(self, ")");

        formatting_space!(self);
        self.emit_block_stmt(&n.function.body)
    }

    fn emit_private_prop(&mut self, n: &PrivateProp) -> Result {
        self.emit_private_name(&n.key)?;

        if let Some(value) = &n.value {
            formatting_space!(self);
            punct!(self, "=");
            formatting_space!(self);

            if matches!(value.as_ref(), Expr::Seq(_)) {
                punct!(self, "(");
                self.emit_expr(value)?;
                punct!(self, ")");
            } else {
                self.emit_expr(value)?;
            }
        }

        formatting_semi!(self);
        Ok(())
    }

    fn emit_class_prop(&mut self, n: &ClassProp) -> Result {
        if n.is_static {
            keyword!(self, "static");
            space!(self);
        }

        self.emit_prop_name(&n.key)?;

        if let Some(v) = &n.value {
            formatting_space!(self);
            punct!(self, "=");
            formatting_space!(self);

            if matches!(v.as_ref(), Expr::Seq(_)) {
                punct!(self, "(");
                self.emit_expr(v)?;
                punct!(self, ")");
            } else {
                self.emit_expr(v)?;
            }
        }

        formatting_semi!(self);
        Ok(())
    }

    fn emit_class_constructor(&mut self, n: &Constructor) -> Result {
        let span = get_span!(self, n.node_id);

        keyword!(self, "constructor");
        punct!(self, "(");
        self.emit_list(
            span,
            &n.params,
            |e, n| e.emit_param(n),
            ListFormat::Parameters,
        )?;
        punct!(self, ")");

        self.emit_block_stmt(&n.body)
    }

    fn emit_prop_name(&mut self, node: &PropName) -> Result {
        match node {
            PropName::Ident(n) => self.emit_ident(n),
            PropName::Str(n) => self.emit_str_lit(n),
            PropName::Num(n) => self.emit_num_lit(n),
            PropName::Computed(n) => self.emit_computed_prop_name(n),
            PropName::BigInt(n) => self.emit_big_lit(n),
        }
    }

    fn emit_computed_prop_name(&mut self, node: &ComputedPropName) -> Result {
        punct!(self, "[");
        let ctx = self.ctx;
        self.ctx = Context::FreeExpr;
        if matches!(node.expr.as_ref(), Expr::Seq(_)) {
            punct!(self, "(");
            self.emit_expr(&node.expr)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&node.expr)?;
        }
        self.ctx = ctx;
        punct!(self, "]");
        Ok(())
    }

    fn emit_cond_expr(&mut self, node: &CondExpr, wrap_test: bool) -> Result {
        let test_needs_parens = wrap_test
            || matches!(
                node.test.as_ref(),
                Expr::Seq(_) | Expr::Assign(_) | Expr::Cond(_) | Expr::Arrow(_) | Expr::Yield(_)
            );

        if test_needs_parens {
            punct!(self, "(");
            self.emit_expr(&node.test)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&node.test)?;
        }

        formatting_space!(self);
        punct!(self, "?");
        formatting_space!(self);

        let ctx = self.ctx;
        self.ctx = Context::FreeExpr;

        if matches!(node.cons.as_ref(), Expr::Seq(_)) {
            punct!(self, "(");
            self.emit_expr(&node.cons)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&node.cons)?;
        }

        formatting_space!(self);
        punct!(self, ":");
        formatting_space!(self);

        if matches!(node.alt.as_ref(), Expr::Seq(_)) {
            punct!(self, "(");
            self.emit_expr(&node.alt)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&node.alt)?;
        }

        self.ctx = ctx;
        Ok(())
    }

    fn emit_fn_expr(&mut self, node: &FnExpr) -> Result {
        if node.function.is_async {
            keyword!(self, "async");
            space!(self);
        }
        keyword!(self, "function");

        if node.function.is_generator {
            punct!(self, "*");
        }
        if let Some(i) = &node.ident {
            space!(self);
            self.emit_ident(i)?;
        }

        self.emit_fn_trailing(&node.function)
    }

    /// prints `(b){}` from `function a(b){}`
    fn emit_fn_trailing(&mut self, node: &Function) -> Result {
        punct!(self, "(");
        self.emit_list(
            get_span!(self, node.node_id),
            &node.params,
            |e, n| e.emit_param(n),
            ListFormat::CommaListElements,
        )?;
        punct!(self, ")");

        formatting_space!(self);
        self.emit_block_stmt(&node.body)
    }

    fn emit_this_expr(&mut self, _: &ThisExpr) -> Result {
        keyword!(self, "this");
        Ok(())
    }

    fn emit_tpl_lit(&mut self, node: &Tpl) -> Result {
        debug_assert!(node.quasis.len() == node.exprs.len() + 1);

        punct!(self, "`");

        for i in 0..(node.quasis.len() + node.exprs.len()) {
            if i % 2 == 0 {
                self.emit_quasi(&node.quasis[i / 2])?;
            } else {
                punct!(self, "${");
                self.emit_expr(&node.exprs[i / 2])?;
                punct!(self, "}");
            }
        }

        punct!(self, "`");
        Ok(())
    }

    fn emit_tagged_tpl_lit(&mut self, node: &TaggedTpl) -> Result {
        if let Expr::New(new) = node.tag.as_ref() {
            self.emit_new(new, false)?;
        } else {
            let needs_parens = matches!(
                node.tag.as_ref(),
                Expr::OptChain(..)
                    | Expr::Arrow(..)
                    | Expr::Cond(..)
                    | Expr::Bin(..)
                    | Expr::Seq(..)
                    | Expr::Fn(..)
                    | Expr::Assign(..)
                    | Expr::Unary(..)
            );
            if needs_parens {
                punct!(self, "(");
                self.emit_expr(&node.tag)?;
                punct!(self, ")");
            } else {
                self.emit_expr(&node.tag)?;
            }
        }
        self.emit_tpl_lit(&node.tpl)
    }

    fn emit_quasi(&mut self, node: &TplElement) -> Result {
        self.wr.write_str_lit(
            get_span!(self, node.node_id),
            &unescape_tpl_lit(&node.raw.value),
        )
    }

    fn emit_unary_expr(&mut self, node: &UnaryExpr) -> Result {
        let need_formatting_space = match node.op {
            op!("typeof") | op!("void") | op!("delete") => {
                keyword!(self, node.op.as_str());
                true
            }
            op!(unary, "+") | op!(unary, "-") | op!("!") | op!("~") => {
                punct!(self, node.op.as_str());
                false
            }
        };

        let mut needs_parens = false;
        match &*node.arg {
            Expr::Bin(BinExpr {
                op: op!("/") | op!("*"),
                left,
                right,
                ..
            }) if node.op == op!(unary, "-")
                && match (&**left, &**right) {
                    (Expr::Lit(Lit::Num(l)), Expr::Lit(Lit::Num(..))) => {
                        !l.value.is_sign_negative()
                    }
                    _ => false,
                } => {}

            Expr::Assign(..)
            | Expr::Bin(..)
            | Expr::Seq(..)
            | Expr::Cond(..)
            | Expr::Arrow(..)
            | Expr::Yield(..) => {
                needs_parens = true;
            }

            _ => {}
        }

        let old = self.ctx;
        self.ctx = Context::FreeExpr;

        if needs_parens {
            if need_formatting_space {
                formatting_space!(self);
            }
        } else if self.should_emit_whitespace_before_operand(node)? {
            space!(self);
        }

        if needs_parens {
            punct!(self, "(");
            self.emit_expr(&node.arg)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&node.arg)?;
        }
        self.ctx = old;
        Ok(())
    }

    fn emit_update_expr(&mut self, node: &UpdateExpr) -> Result {
        if node.prefix {
            operator!(self, node.op.as_str());
            //TODO: Check if we should use should_emit_whitespace_before_operand
            self.emit_expr(&node.arg)?;
        } else {
            self.emit_expr(&node.arg)?;
            operator!(self, node.op.as_str());
        }
        Ok(())
    }

    fn emit_yield_expr(&mut self, node: &YieldExpr) -> Result {
        keyword!(self, "yield");
        if node.delegate {
            operator!(self, "*");
        }

        if let Some(arg) = &node.arg {
            if !node.delegate && self.expr_starts_with_alpha_num(arg)? {
                space!(self);
            } else {
                formatting_space!(self);
            }
            let old = self.ctx;
            self.ctx = Context::ForcedExpr;
            self.emit_expr(arg)?;
            self.ctx = old;
        }
        Ok(())
    }

    fn emit_expr_or_spreads(
        &mut self,
        parent_node: Span,
        nodes: &[ExprOrSpread],
        format: ListFormat,
    ) -> Result {
        self.emit_list(parent_node, nodes, |e, n| e.emit_expr_or_spread(n), format)
    }

    fn emit_expr_or_spread(&mut self, node: &ExprOrSpread) -> Result {
        let in_for_stmt_head = self.flags.replace(Flags::in_for_stmt_head, false);
        match node {
            ExprOrSpread::Spread(n) => self.emit_spread_element(n)?,
            ExprOrSpread::Expr(n) => {
                if matches!(n.as_ref(), Expr::Yield(_)) {
                    punct!(self, "(");
                    self.emit_expr(n)?;
                    punct!(self, ")");
                } else {
                    self.emit_expr(n)?;
                }
            }
        }
        self.flags.set(Flags::in_for_stmt_head, in_for_stmt_head);
        Ok(())
    }

    fn emit_await_expr(&mut self, node: &AwaitExpr) -> Result {
        keyword!(self, "await");

        let needs_parens = matches!(
            node.arg.as_ref(),
            Expr::Cond(..) | Expr::Assign(..) | Expr::Bin(..) | Expr::Yield(..)
        );

        if needs_parens || !self.expr_starts_with_alpha_num(&node.arg)? {
            formatting_space!(self);
        } else {
            space!(self);
        }

        let old = self.ctx;
        self.ctx = Context::ForcedExpr;
        if needs_parens {
            punct!(self, "(");
            self.emit_expr(&node.arg)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&node.arg)?;
        }
        self.ctx = old;
        Ok(())
    }

    fn emit_array_lit(&mut self, node: &ArrayLit) -> Result {
        let span = get_span!(self, node.node_id);

        punct!(self, "[");
        let mut format = ListFormat::ArrayLiteralExpressionElements;
        if let Some(None) = node.elems.last() {
            format |= ListFormat::ForceTrailingComma;
        }

        let ctx = std::mem::replace(&mut self.ctx, Context::ForcedExpr);
        let in_for_stmt_head = self.flags.replace(Flags::in_for_stmt_head, false);

        self.emit_list(
            span,
            &node.elems,
            |e, n| {
                if let Some(n) = n {
                    e.emit_expr_or_spread(n)
                } else {
                    Ok(())
                }
            },
            format,
        )?;

        self.flags.set(Flags::in_for_stmt_head, in_for_stmt_head);
        self.ctx = ctx;

        punct!(self, "]");
        Ok(())
    }

    fn emit_object_lit(&mut self, node: &ObjectLit) -> Result {
        let span = get_span!(self, node.node_id);

        punct!(self, "{");
        if !self.cfg.minify {
            self.wr.write_line()?;
        }
        let in_for_stmt_head = self.flags.replace(Flags::in_for_stmt_head, false);
        self.emit_list(
            span,
            &node.props,
            |e, n| e.emit_prop(n),
            ListFormat::ObjectLiteralExpressionProperties | ListFormat::CanSkipTrailingComma,
        )?;
        self.flags.set(Flags::in_for_stmt_head, in_for_stmt_head);
        if !self.cfg.minify {
            self.wr.write_line()?;
        }
        punct!(self, "}");
        Ok(())
    }

    fn emit_prop(&mut self, node: &Prop) -> Result {
        match node {
            Prop::KeyValue(n) => self.emit_kv_prop(n),
            Prop::Assign(n) => self.emit_assign_prop(n),
            Prop::Getter(n) => self.emit_getter_prop(n),
            Prop::Setter(n) => self.emit_setter_prop(n),
            Prop::Method(n) => self.emit_method_prop(n),
            Prop::Spread(n) => self.emit_spread_assignment(n),
        }
    }

    fn emit_kv_prop(&mut self, node: &KeyValueProp) -> Result {
        // Short hand properties e.g. `{foo:foo}` => `{foo}`
        if self.cfg.minify {
            if let PropName::Ident(key) = &node.key {
                if let Expr::Ident(value) = node.value.as_ref() {
                    if key.sym == value.sym {
                        return self.emit_ident(key);
                    }
                }
            }
        }

        self.emit_prop_name(&node.key)?;
        punct!(self, ":");
        formatting_space!(self);
        if matches!(node.value.as_ref(), Expr::Seq(_)) {
            punct!(self, "(");
            self.emit_expr(&node.value)?;
            punct!(self, ")");
            Ok(())
        } else {
            self.emit_expr(&node.value)
        }
    }

    fn emit_assign_prop(&mut self, node: &AssignProp) -> Result {
        self.emit_ident(&node.key)?;
        punct!(self, "=");
        self.emit_expr(&node.value)
    }

    fn emit_getter_prop(&mut self, node: &GetterProp) -> Result {
        keyword!(self, "get");

        let starts_with_alpha_num = !matches!(node.key, PropName::Str(_) | PropName::Computed(_));
        if starts_with_alpha_num {
            space!(self);
        } else {
            formatting_space!(self);
        }

        self.emit_prop_name(&node.key)?;
        formatting_space!(self);
        punct!(self, "(");
        punct!(self, ")");
        formatting_space!(self);
        self.emit_block_stmt(&node.body)
    }

    fn emit_setter_prop(&mut self, node: &SetterProp) -> Result {
        keyword!(self, "set");

        let starts_with_alpha_num = !matches!(node.key, PropName::Str(_) | PropName::Computed(_));

        if starts_with_alpha_num {
            space!(self);
        } else {
            formatting_space!(self);
        }

        self.emit_prop_name(&node.key)?;
        formatting_space!(self);

        punct!(self, "(");
        self.emit_param(&node.param)?;
        punct!(self, ")");

        self.emit_block_stmt(&node.body)
    }

    fn emit_method_prop(&mut self, node: &MethodProp) -> Result {
        if node.function.is_async {
            keyword!(self, "async");
            space!(self);
        }

        if node.function.is_generator {
            punct!(self, "*");
        }

        self.emit_prop_name(&node.key)?;
        formatting_space!(self);
        // TODO
        self.emit_fn_trailing(&node.function)
    }

    fn emit_spread_assignment(&mut self, node: &SpreadAssignment) -> Result {
        punct!(self, "...");
        let old = self.ctx;
        self.ctx = Context::ForcedExpr;
        self.emit_expr(&node.expr)?;
        self.ctx = old;
        Ok(())
    }

    fn emit_private_name(&mut self, n: &PrivateName) -> Result {
        punct!(self, "#");
        self.emit_ident(&n.id)
    }

    fn emit_binding_ident(&mut self, ident: &BindingIdent) -> Result {
        self.emit_ident(&ident.id)
    }

    fn emit_ident(&mut self, ident: &Ident) -> Result {
        let span = get_span!(self, ident.node_id);
        // TODO: Use write_symbol when ident is a symbol.

        // TODO: span
        self.wr
            .write_symbol(span, &handle_invalid_unicodes(&ident.sym))
    }

    fn emit_list<N>(
        &mut self,
        parent_node: Span,
        children: &[N],
        emit_child: impl Fn(&mut Emitter, &N) -> Result,
        format: ListFormat,
    ) -> Result {
        self.emit_list5(parent_node, children, emit_child, format, 0, children.len())
    }

    #[allow(clippy::cognitive_complexity)]
    fn emit_list5<N>(
        &mut self,
        parent_node: Span,
        children: &[N],
        emit_child: impl Fn(&mut Emitter, &N) -> Result,
        format: ListFormat,
        start: usize,
        count: usize,
    ) -> Result {
        let is_empty = start > children.len() || count == 0;
        if is_empty && format.contains(ListFormat::OptionalIfEmpty) {
            return Ok(());
        }

        if format.contains(ListFormat::BracketsMask) {
            self.wr.write_punct(None, format.opening_bracket())?;
        }

        // self.handlers.onBeforeEmitNodeArray(children);

        if is_empty {
            // Write a line terminator if the parent node was multi-line

            if format.contains(ListFormat::MultiLine) {
                if !self.cfg.minify {
                    self.wr.write_line()?;
                }
            } else if format.contains(ListFormat::SpaceBetweenBraces)
                && !(format.contains(ListFormat::NoSpaceIfEmpty))
                && !self.cfg.minify
            {
                self.wr.write_space()?;
            }
        } else {
            // Write the opening line terminator or leading whitespace.
            if self
                .cm
                .should_write_leading_line_terminator(parent_node, children, format)
            {
                if !self.cfg.minify {
                    self.wr.write_line()?;
                }
            } else if format.contains(ListFormat::SpaceBetweenBraces) && !self.cfg.minify {
                self.wr.write_space()?;
            }

            // Increase the indent, if requested.
            if format.contains(ListFormat::Indented) && !self.cfg.minify {
                self.wr.increase_indent()?;
            }

            // Emit each child.
            let mut first = true;
            let mut should_decrease_indent_after_emit = false;
            for i in 0..count {
                let child = &children[start + i];

                // Write the delimiter if this is not the first node.
                if !first {
                    self.write_delim(format)?;

                    // Write either a line terminator or whitespace to separate the elements.

                    if self.cm.should_write_separating_line_terminator(format) {
                        // If a synthesized node in a single-line list starts on a new
                        // line, we should increase the indent.
                        if (format & (ListFormat::LinesMask | ListFormat::Indented))
                            == ListFormat::SingleLine
                            && !self.cfg.minify
                        {
                            self.wr.increase_indent()?;
                            should_decrease_indent_after_emit = true;
                        }

                        if !self.cfg.minify {
                            self.wr.write_line()?;
                        }
                    } else if format.contains(ListFormat::SpaceBetweenSiblings) {
                        formatting_space!(self);
                    }
                }

                emit_child(self, child)?;

                if should_decrease_indent_after_emit {
                    self.wr.decrease_indent()?;
                    should_decrease_indent_after_emit = false;
                }

                first = false;
            }

            // Write a trailing comma, if requested.
            let has_trailing_comma = format.contains(ListFormat::ForceTrailingComma)
                || format.contains(ListFormat::AllowTrailingComma) && {
                    if parent_node.is_dummy() {
                        false
                    } else {
                        match self.cm.span_to_snippet(parent_node) {
                            Ok(snippet) => {
                                if snippet.len() < 3 {
                                    false
                                } else {
                                    let last_char = snippet.chars().last().unwrap();
                                    snippet[..snippet.len() - last_char.len_utf8()]
                                        .trim()
                                        .ends_with(',')
                                }
                            }
                            _ => false,
                        }
                    }
                };

            if has_trailing_comma && format.contains(ListFormat::CommaDelimited) {
                if !self.cfg.minify || !format.contains(ListFormat::CanSkipTrailingComma) {
                    punct!(self, ",");
                    formatting_space!(self);
                }
            }

            // Decrease the indent, if requested.
            if format.contains(ListFormat::Indented) && !self.cfg.minify {
                self.wr.decrease_indent()?;
            }

            // Write the closing line terminator or closing whitespace.
            if self
                .cm
                .should_write_closing_line_terminator(parent_node, children, format)
            {
                if !self.cfg.minify {
                    self.wr.write_line()?;
                }
            } else if format.contains(ListFormat::SpaceBetweenBraces) && !self.cfg.minify {
                self.wr.write_space()?;
            }
        }

        // self.handlers.onAfterEmitNodeArray(children);

        if format.contains(ListFormat::BracketsMask) {
            self.wr.write_punct(None, format.closing_bracket())?;
        }

        Ok(())
    }
}

/// Patterns
impl Emitter<'_> {
    fn emit_param(&mut self, node: &Param) -> Result {
        let old = self.ctx;
        self.ctx = Context::ForcedExpr;
        let in_for_stmt_head = self.flags.replace(Flags::in_for_stmt_head, false);

        self.emit_pat(&node.pat)?;
        self.flags.set(Flags::in_for_stmt_head, in_for_stmt_head);
        self.ctx = old;
        Ok(())
    }

    fn emit_pat(&mut self, node: &Pat) -> Result {
        match node {
            Pat::Array(n) => self.emit_array_pat(n),
            Pat::Assign(n) => self.emit_assign_pat(n),
            Pat::Expr(n) => self.emit_expr(n),
            Pat::Ident(n) => self.emit_binding_ident(n),
            Pat::Object(n) => self.emit_object_pat(n),
            Pat::Rest(n) => self.emit_rest_pat(n),
            Pat::Invalid(..) => invalid_pat(),
        }
    }

    fn emit_rest_pat(&mut self, node: &RestPat) -> Result {
        punct!(self, "...");
        self.emit_pat(&node.arg)
    }

    fn emit_spread_element(&mut self, node: &SpreadElement) -> Result {
        punct!(self, "...");
        let old = self.ctx;
        self.ctx = Context::ForcedExpr;
        self.emit_expr(&node.expr)?;
        self.ctx = old;
        Ok(())
    }

    fn emit_pat_or_expr(&mut self, node: &PatOrExpr) -> Result {
        match node {
            PatOrExpr::Expr(n) => self.emit_expr(n),
            PatOrExpr::Pat(n) => self.emit_pat(n),
        }
    }

    fn emit_array_pat(&mut self, node: &ArrayPat) -> Result {
        let span = get_span!(self, node.node_id);

        punct!(self, "[");
        self.emit_list(
            span,
            &node.elems,
            |e, n| {
                if let Some(n) = n {
                    e.emit_pat(n)
                } else {
                    Ok(())
                }
            },
            ListFormat::ArrayBindingPatternElements,
        )?;
        punct!(self, "]");
        Ok(())
    }

    fn emit_assign_pat(&mut self, node: &AssignPat) -> Result {
        let in_for_stmt_head = self.flags.replace(Flags::in_for_stmt_head, false);

        self.emit_pat(&node.left)?;
        formatting_space!(self);
        punct!(self, "=");
        formatting_space!(self);

        self.emit_expr(&node.right)?;

        self.flags.set(Flags::in_for_stmt_head, in_for_stmt_head);
        Ok(())
    }

    fn emit_object_pat(&mut self, node: &ObjectPat) -> Result {
        let span = get_span!(self, node.node_id);

        let is_last_rest = matches!(node.props.last(), Some(ObjectPatProp::Rest(..)));
        let format = if is_last_rest {
            ListFormat::ObjectBindingPatternElements ^ ListFormat::AllowTrailingComma
        } else {
            ListFormat::ObjectBindingPatternElements
        };

        punct!(self, "{");
        self.emit_list(
            span,
            &node.props,
            |e, n| e.emit_object_pat_prop(n),
            format | ListFormat::CanSkipTrailingComma,
        )?;
        punct!(self, "}");
        Ok(())
    }

    fn emit_object_pat_prop(&mut self, node: &ObjectPatProp) -> Result {
        match node {
            ObjectPatProp::KeyValue(node) => self.emit_object_kv_pat(node),
            ObjectPatProp::Rest(node) => self.emit_rest_pat(node),
        }
    }

    fn emit_object_kv_pat(&mut self, node: &KeyValuePatProp) -> Result {
        if self.cfg.minify {
            if let PropName::Ident(key) = &node.key {
                // Short hand properties e.g. `{foo:foo}` => `{foo}`
                if let Pat::Ident(value) = node.value.as_ref() {
                    if key.sym == value.id.sym {
                        return self.emit_ident(key);
                    }
                }

                // Short hand assign  e.g. `{foo: foo = bar}` => `{foo = bar}`
                if let Pat::Assign(value) = node.value.as_ref() {
                    if let Pat::Ident(lhs) = value.left.as_ref() {
                        if lhs.id.sym == key.sym {
                            self.emit_ident(key)?;
                            punct!(self, "=");
                            let old = self.ctx;
                            self.ctx = Context::ForcedExpr;
                            self.emit_expr(&value.right)?;
                            self.ctx = old;
                            return Ok(());
                        }
                    }
                }
            }
        }

        let old = self.ctx;
        self.ctx = Context::ForcedExpr;
        self.emit_prop_name(&node.key)?;
        self.ctx = old;

        punct!(self, ":");
        formatting_space!(self);
        self.emit_pat(&node.value)?;
        formatting_space!(self);
        Ok(())
    }

    fn emit_var_decl_or_pat(&mut self, node: &VarDeclOrPat) -> Result {
        match node {
            VarDeclOrPat::Pat(n) => self.emit_pat(n),
            VarDeclOrPat::VarDecl(n) => self.emit_var_decl(n),
        }
    }
}

/// Statements
impl Emitter<'_> {
    fn emit_stmt(&mut self, node: &Stmt, ignore_empty: bool) -> Result {
        let old = self.ctx;
        // only ExprStmt would have unparented expr,
        // which would be handled in its own visit function
        self.ctx = Context::FreeExpr;
        match node {
            Stmt::Expr(e) => self.emit_expr_stmt(e),
            Stmt::Block(e) => {
                return self.emit_block_stmt(e);
            }
            Stmt::Empty(e) => self.emit_empty_stmt(e, ignore_empty),
            Stmt::Debugger(e) => self.emit_debugger_stmt(e),
            Stmt::With(e) => self.emit_with_stmt(e),
            Stmt::Return(e) => self.emit_return_stmt(e),
            Stmt::Labeled(e) => self.emit_labeled_stmt(e),
            Stmt::Break(e) => self.emit_break_stmt(e),
            Stmt::Continue(e) => self.emit_continue_stmt(e),
            Stmt::If(e) => self.emit_if_stmt(e),
            Stmt::Switch(e) => self.emit_switch_stmt(e),
            Stmt::Throw(e) => self.emit_throw_stmt(e),
            Stmt::Try(e) => self.emit_try_stmt(e),
            Stmt::While(e) => self.emit_while_stmt(e),
            Stmt::DoWhile(e) => self.emit_do_while_stmt(e),
            Stmt::For(e) => self.emit_for_stmt(e),
            Stmt::ForIn(e) => self.emit_for_in_stmt(e),
            Stmt::ForOf(e) => self.emit_for_of_stmt(e),
            Stmt::Decl(e) => self.emit_decl(e),
        }?;
        self.ctx = old;

        if !self.cfg.minify {
            self.wr.write_line()?;
        }
        Ok(())
    }

    /// Emits a statement in a single-statement context
    fn emit_single_stmt(
        &mut self,
        mut stmt: &Stmt,
        needs_space_if_alpha_num: bool,
        if_cons: bool,
    ) -> Result {
        if let Stmt::Block(block) = stmt {
            if block.stmts.is_empty() {
                semi!(self);
                return Ok(());
            } else if block.stmts.len() == 1 {
                let first_stmt = &block.stmts[0];
                let is_class = matches!(first_stmt, Stmt::Decl(Decl::Class(_)));
                let is_lexical_var = matches!(
                    first_stmt,
                    Stmt::Decl(Decl::Var(VarDecl {
                        kind: VarDeclKind::Const | VarDeclKind::Let,
                        ..
                    }))
                );
                if !is_class && !is_lexical_var && !(if_cons && will_eat_else_token(stmt)) {
                    stmt = first_stmt;
                }
            }
        }
        if needs_space_if_alpha_num && self.stmt_starts_with_alpha_num(stmt)? {
            space!(self);
        } else {
            formatting_space!(self);
        }
        self.emit_stmt(stmt, false)
    }

    fn emit_expr_stmt(&mut self, e: &ExprStmt) -> Result {
        let old = self.ctx;
        self.ctx = Context::Default;

        let in_expr_stmt_seq = self.flags.contains(Flags::in_expr_stmt_seq);

        let needs_parens = match e.expr.as_ref() {
            Expr::Object(..) | Expr::Class(..) | Expr::Fn(..) => true,
            // ({ a } = foo)
            Expr::Assign(AssignExpr {
                left: PatOrExpr::Pat(left),
                ..
            }) => matches!(left.as_ref(), Pat::Object(_)),
            Expr::Seq(_) => {
                self.flags.set(Flags::in_expr_stmt_seq, true);
                false
            }
            _ => false,
        };

        if needs_parens {
            punct!(self, "(");
            self.emit_expr(&e.expr)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&e.expr)?;
        }

        self.flags.set(Flags::in_expr_stmt_seq, in_expr_stmt_seq);

        self.ctx = old;
        formatting_semi!(self);
        Ok(())
    }

    fn emit_block_stmt(&mut self, node: &BlockStmt) -> Result {
        let span = get_span!(self, node.node_id);

        {
            let span = if span.is_dummy() {
                DUMMY_SP
            } else {
                Span::new(span.lo, span.lo + BytePos(1))
            };
            punct!(self, span, "{");
        }

        let in_for_stmt_head = self.flags.replace(Flags::in_for_stmt_head, false);

        self.emit_list(
            span,
            &node.stmts,
            |e, n| e.emit_stmt(n, true),
            ListFormat::MultiLineBlockStatements,
        )?;

        self.flags.set(Flags::in_for_stmt_head, in_for_stmt_head);

        {
            let span = if span.is_dummy() {
                DUMMY_SP
            } else {
                Span::new(span.hi - BytePos(1), span.hi)
            };
            punct!(self, span, "}");
        }
        Ok(())
    }

    fn emit_empty_stmt(&mut self, _: &EmptyStmt, ignore_empty: bool) -> Result {
        if !ignore_empty {
            semi!(self);
        }
        Ok(())
    }

    fn emit_debugger_stmt(&mut self, _: &DebuggerStmt) -> Result {
        keyword!(self, "debugger");
        formatting_semi!(self);
        Ok(())
    }

    fn emit_with_stmt(&mut self, node: &WithStmt) -> Result {
        keyword!(self, "with");
        formatting_space!(self);
        punct!(self, "(");
        self.emit_expr(&node.obj)?;
        punct!(self, ")");

        self.emit_single_stmt(&node.body, false, false)
    }

    fn emit_return_stmt(&mut self, node: &ReturnStmt) -> Result {
        keyword!(self, "return");
        if let Some(arg) = &node.arg {
            let need_paren = self.expr_starts_with_alpha_num(arg)?;
            if need_paren {
                space!(self);
            } else {
                formatting_space!(self);
            }

            self.emit_expr(arg)?;
        }
        formatting_semi!(self);
        Ok(())
    }

    fn emit_labeled_stmt(&mut self, node: &LabeledStmt) -> Result {
        self.emit_ident(&node.label)?;

        punct!(self, ":");

        self.emit_single_stmt(&node.body, false, false)
    }

    fn emit_break_stmt(&mut self, node: &BreakStmt) -> Result {
        keyword!(self, "break");
        if let Some(label) = &node.label {
            space!(self);
            self.emit_ident(label)?;
        }
        formatting_semi!(self);
        Ok(())
    }

    fn emit_continue_stmt(&mut self, node: &ContinueStmt) -> Result {
        keyword!(self, "continue");
        if let Some(label) = &node.label {
            space!(self);
            self.emit_ident(label)?;
        }
        formatting_semi!(self);
        Ok(())
    }

    fn emit_if_stmt(&mut self, node: &IfStmt) -> Result {
        let span = get_span!(self, node.node_id);

        {
            let span = self.cm.span_until_char(span, ' ');
            keyword!(self, span, "if");
        }

        formatting_space!(self);
        punct!(self, "(");
        self.emit_expr(&node.test)?;
        punct!(self, ")");

        let is_cons_block = matches!(*node.cons, Stmt::Block(..));

        self.emit_single_stmt(&node.cons, false, true)?;

        if let Some(alt) = &node.alt {
            if is_cons_block {
                formatting_space!(self);
            }
            keyword!(self, "else");
            self.emit_single_stmt(alt, true, false)?;
        }
        Ok(())
    }

    fn emit_switch_stmt(&mut self, node: &SwitchStmt) -> Result {
        let span = get_span!(self, node.node_id);

        keyword!(self, "switch");

        punct!(self, "(");
        self.emit_expr(&node.discriminant)?;
        punct!(self, ")");

        punct!(self, "{");
        self.emit_list(
            span,
            &node.cases,
            |e, n| e.emit_switch_case(n),
            ListFormat::CaseBlockClauses,
        )?;
        punct!(self, "}");
        Ok(())
    }

    fn emit_catch_clause(&mut self, node: &CatchClause) -> Result {
        keyword!(self, "catch");
        formatting_space!(self);

        if let Some(param) = &node.param {
            punct!(self, "(");
            self.emit_pat(param)?;
            punct!(self, ")");
        }

        formatting_space!(self);

        self.emit_block_stmt(&node.body)
    }

    fn emit_switch_case(&mut self, node: &SwitchCase) -> Result {
        let span = get_span!(self, node.node_id);

        if let Some(test) = &node.test {
            keyword!(self, "case");

            if self.expr_starts_with_alpha_num(test)? {
                space!(self);
            } else {
                formatting_space!(self);
            }

            self.emit_expr(test)?;
        } else {
            keyword!(self, "default");
        }

        let emit_as_single_stmt = node.cons.len() == 1;

        let mut format = ListFormat::CaseOrDefaultClauseStatements;
        if emit_as_single_stmt {
            punct!(self, ":");
            formatting_space!(self);
            format &= !(ListFormat::MultiLine | ListFormat::Indented);
        } else {
            punct!(self, ":");
        }
        self.emit_list(span, &node.cons, |e, n| e.emit_stmt(n, true), format)
    }

    fn emit_throw_stmt(&mut self, node: &ThrowStmt) -> Result {
        let span = get_span!(self, node.node_id);

        let throw_span = self.cm.span_until_char(span, ' ');

        keyword!(self, throw_span, "throw");

        if self.expr_starts_with_alpha_num(&node.arg)? {
            space!(self);
        } else {
            formatting_space!(self);
        }
        self.emit_expr(&node.arg)?;

        formatting_semi!(self);
        Ok(())
    }

    fn emit_try_stmt(&mut self, node: &TryStmt) -> Result {
        keyword!(self, "try");
        formatting_space!(self);
        self.emit_block_stmt(&node.block)?;

        if let Some(catch) = &node.handler {
            formatting_space!(self);
            self.emit_catch_clause(catch)?;
        }

        if let Some(finally) = &node.finalizer {
            formatting_space!(self);
            keyword!(self, "finally");
            formatting_space!(self);
            self.emit_block_stmt(finally)?;
        }
        Ok(())
    }

    fn emit_while_stmt(&mut self, node: &WhileStmt) -> Result {
        keyword!(self, "while");
        formatting_space!(self);
        punct!(self, "(");
        self.emit_expr(&node.test)?;
        punct!(self, ")");

        self.emit_single_stmt(&node.body, false, false)
    }

    fn emit_do_while_stmt(&mut self, node: &DoWhileStmt) -> Result {
        keyword!(self, "do");
        self.emit_single_stmt(&node.body, true, false)?;

        keyword!(self, "while");

        formatting_space!(self);

        punct!(self, "(");
        self.emit_expr(&node.test)?;
        punct!(self, ")");
        Ok(())
    }

    fn emit_for_stmt(&mut self, node: &ForStmt) -> Result {
        keyword!(self, "for");
        formatting_space!(self);
        punct!(self, "(");
        let in_for_stmt_head = self.flags.replace(Flags::in_for_stmt_head, true);
        opt!(self, emit_var_decl_or_expr, node.init);
        self.flags.set(Flags::in_for_stmt_head, in_for_stmt_head);
        semi!(self);
        opt_leading_space!(self, emit_expr, node.test);
        semi!(self);
        opt_leading_space!(self, emit_expr, node.update);
        punct!(self, ")");

        self.emit_single_stmt(&node.body, false, false)
    }

    fn emit_for_in_stmt(&mut self, node: &ForInStmt) -> Result {
        keyword!(self, "for");
        formatting_space!(self);
        punct!(self, "(");
        let in_for_stmt_head = self.flags.replace(Flags::in_for_stmt_head, true);
        self.emit_var_decl_or_pat(&node.left)?;

        if for_var_ends_with_alpha_num(&node.left) {
            space!(self);
        } else {
            formatting_space!(self);
        }

        keyword!(self, "in");

        if self.expr_starts_with_alpha_num(&node.right)? {
            space!(self);
        } else {
            formatting_space!(self);
        }
        self.emit_expr(&node.right)?;

        self.flags.set(Flags::in_for_stmt_head, in_for_stmt_head);

        punct!(self, ")");

        self.emit_single_stmt(&node.body, false, false)
    }

    fn emit_for_of_stmt(&mut self, node: &ForOfStmt) -> Result {
        keyword!(self, "for");
        if node.is_await {
            space!(self);
            keyword!(self, "await");
        }
        formatting_space!(self);
        punct!(self, "(");

        let mut await_ident = false;

        if !node.is_await {
            if let VarDeclOrPat::Pat(p) = &node.left {
                match p {
                    Pat::Ident(ident) => {
                        if &ident.id.sym == "async" {
                            await_ident = true;
                        }
                    }
                    Pat::Expr(expr) => {
                        if let Expr::Ident(ident) = expr.as_ref() {
                            if &ident.sym == "async" {
                                await_ident = true;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        if await_ident {
            punct!(self, "(");
            keyword!(self, get_span!(self, node.left.node_id()), "await");
            punct!(self, ")");
        } else {
            self.emit_var_decl_or_pat(&node.left)?;
        }

        if for_var_ends_with_alpha_num(&node.left) {
            space!(self);
        } else {
            formatting_space!(self);
        }

        keyword!(self, "of");

        let right_needs_parens = matches!(node.right.as_ref(), Expr::Seq(..) | Expr::Await(..));
        if !right_needs_parens && self.expr_starts_with_alpha_num(&node.right)? {
            space!(self);
        } else {
            formatting_space!(self);
        }

        if right_needs_parens {
            punct!(self, "(");
            self.emit_expr(&node.right)?;
            punct!(self, ")");
        } else {
            self.emit_expr(&node.right)?;
        }

        punct!(self, ")");
        self.emit_single_stmt(&node.body, false, false)
    }
}

impl Emitter<'_> {
    fn write_delim(&mut self, f: ListFormat) -> Result {
        match f & ListFormat::DelimitersMask {
            ListFormat::None => {}
            ListFormat::CommaDelimited => self.wr.write_punct(None, ",")?,
            ListFormat::BarDelimited => {
                if !self.cfg.minify {
                    self.wr.write_space()?;
                }
                self.wr.write_punct(None, "|")?;
            }
            ListFormat::AmpersandDelimited => {
                if !self.cfg.minify {
                    self.wr.write_space()?;
                }
                self.wr.write_punct(None, "&")?;
            }
            _ => unreachable!(),
        }

        Ok(())
    }

    fn emit_var_decl_or_expr(&mut self, node: &VarDeclOrExpr) -> Result {
        match node {
            VarDeclOrExpr::Expr(node) => self.emit_expr(node),
            VarDeclOrExpr::VarDecl(node) => self.emit_var_decl(node),
        }
    }
}

impl Emitter<'_> {
    /// In some cases, we need to emit a space between the operator and the operand.
    /// One obvious case is when the operator is an identifier, like delete or
    /// typeof. We also need to do this for plus and minus expressions in certain
    /// cases. Specifically, consider the following two cases (parens are just for
    /// clarity of exposition, and not part of the source code):
    ///
    ///  (+(+1))
    ///  (+(++1))
    ///
    /// We need to emit a space in both cases. In the first case, the absence of a
    /// space will make the resulting expression a prefix increment operation. And
    /// in the second, it will make the resulting expression a prefix increment
    /// whose operand is a plus expression - (++(+x)) The same is true of minus of
    /// course.
    fn should_emit_whitespace_before_operand(&self, node: &UnaryExpr) -> io::Result<bool> {
        match node {
            UnaryExpr {
                op: op!("void"), ..
            }
            | UnaryExpr {
                op: op!("typeof"), ..
            }
            | UnaryExpr {
                op: op!("delete"), ..
            } => return self.expr_starts_with_alpha_num(&node.arg),
            _ => {}
        }

        match *node.arg {
            Expr::Update(UpdateExpr {
                op: op!("++"),
                prefix: true,
                ..
            })
            | Expr::Unary(UnaryExpr {
                op: op!(unary, "+"),
                ..
            }) if node.op == op!(unary, "+") => Ok(true),
            Expr::Update(UpdateExpr {
                op: op!("--"),
                prefix: true,
                ..
            })
            | Expr::Unary(UnaryExpr {
                op: op!(unary, "-"),
                ..
            }) if node.op == op!(unary, "-") => Ok(true),
            _ => Ok(false),
        }
    }

    fn expr_ends_with_alpha_num(&self, expr: &Expr) -> io::Result<bool> {
        let last = {
            let mut buffer = vec![];
            {
                let mut new = Emitter {
                    cfg: self.cfg,
                    cm: self.cm.clone(),
                    wr: JsWriter::new("\n", &mut buffer, None),
                    program_data: self.program_data,
                    ctx: self.ctx,
                    flags: self.flags,
                };

                new.emit_expr(expr)?;
            }
            *buffer.last().unwrap()
        };
        Ok(is_alpha_num(last))
    }

    fn expr_starts_with_alpha_num(&self, expr: &Expr) -> io::Result<bool> {
        let first = {
            let mut buffer = vec![];
            {
                let mut new = Emitter {
                    cfg: self.cfg,
                    cm: self.cm.clone(),
                    wr: JsWriter::new("\n", &mut buffer, None),
                    program_data: self.program_data,
                    ctx: self.ctx,
                    flags: self.flags,
                };

                new.emit_expr(expr)?;
            }
            *buffer.first().unwrap()
        };
        Ok(is_alpha_num(first))
    }

    fn stmt_starts_with_alpha_num(&self, stmt: &Stmt) -> io::Result<bool> {
        match stmt {
            Stmt::Expr(expr) => self.expr_starts_with_alpha_num(&expr.expr),
            Stmt::Decl(..)
            | Stmt::Debugger(..)
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
            | Stmt::If(..) => Ok(true),
            Stmt::Block(..) | Stmt::Empty(..) => Ok(false),
        }
    }
}

fn is_alpha_num(c: u8) -> bool {
    !matches!(
        c,
        b'`' | b'~'
            | b'!'
            | b'@'
            | b'#'
            | b'$'
            | b'%'
            | b'^'
            | b'&'
            | b'*'
            | b'('
            | b')'
            | b'-'
            | b'_'
            | b'='
            | b'+'
            | b'['
            | b']'
            | b'{'
            | b'}'
            | b'|'
            | b'\\'
            | b';'
            | b':'
            | b'\''
            | b'"'
            | b'<'
            | b'>'
            | b','
            | b'.'
            | b'/'
            | b'?'
    )
}

fn unescape_tpl_lit(s: &str) -> String {
    fn read_escaped(
        radix: u32,
        len: Option<usize>,
        buf: &mut String,
        chars: impl Iterator<Item = char>,
    ) {
        let mut v = 0;
        let mut pending = None;

        for (i, c) in chars.enumerate() {
            if let Some(len) = len {
                if i == len {
                    pending = Some(c);
                    break;
                }
            }

            match c.to_digit(radix) {
                None => {
                    pending = Some(c);
                    break;
                }
                Some(d) => {
                    v = v * radix + d;
                }
            }
        }

        match radix {
            2 => write!(buf, "\\b{:b}", v).unwrap(),

            8 => write!(buf, "\\o{:o}", v).unwrap(),

            16 => {
                if v < 16 {
                    write!(buf, "\\x0{:x}", v).unwrap()
                } else {
                    write!(buf, "\\x{:x}", v).unwrap()
                }
            }

            _ => unreachable!(),
        }

        if let Some(pending) = pending {
            buf.push(pending);
        }
    }

    let mut result = String::with_capacity(s.len() * 6 / 5);
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c != '\\' {
            match c {
                '\r' => {
                    if chars.peek().copied() == Some('\n') {
                        continue;
                    }

                    result.push_str("\\r");
                }
                '\n' => {
                    result.push_str("\\n");
                }

                // TODO: Handle all escapes
                _ => {
                    result.push(c);
                }
            }

            continue;
        }

        match chars.next() {
            None => {
                // This is wrong, but it seems like a mistake made by user.
                result.push('\\');
            }
            Some(c) => {
                match c {
                    '\\' => result.push_str(r"\\"),
                    'n' => result.push_str("\\n"),
                    'r' => result.push_str("\\r"),
                    't' => result.push_str("\\t"),
                    'b' => result.push_str("\\\u{0008}"),
                    'f' => result.push_str("\\\u{000C}"),
                    'v' => result.push_str("\\\u{000B}"),
                    '0' => match chars.next() {
                        Some('b') => read_escaped(2, None, &mut result, &mut chars),
                        Some('o') => read_escaped(8, None, &mut result, &mut chars),
                        Some('x') => read_escaped(16, Some(2), &mut result, &mut chars),
                        nc => {
                            // This is wrong, but it seems like a mistake made by user.
                            result.push_str("\\0");
                            result.extend(nc);
                        }
                    },

                    _ => {
                        result.push('\\');
                        result.push(c);
                    }
                }
            }
        }
    }

    result
}

fn get_quoted_utf16(v: &str, target: EsVersion) -> String {
    let mut buf = String::with_capacity(v.len() + 2);
    let mut iter = v.chars().peekable();

    let mut single_quote_count = 0;
    let mut double_quote_count = 0;

    while let Some(c) = iter.next() {
        match c {
            '\x00' => {
                if target < EsVersion::Es5 || matches!(iter.peek(), Some('0'..='9')) {
                    buf.push_str("\\x00");
                } else {
                    buf.push_str("\\0");
                }
            }
            '\u{0008}' => buf.push_str("\\b"),
            '\u{000c}' => buf.push_str("\\f"),
            '\n' => buf.push_str("\\n"),
            '\r' => buf.push_str("\\r"),
            '\u{000b}' => buf.push_str("\\v"),
            '\t' => buf.push('\t'),
            '\\' => {
                let next = iter.peek();

                match next {
                    // TODO fix me - workaround for surrogate pairs
                    Some('u') => {
                        let mut inner_iter = iter.clone();

                        inner_iter.next();

                        let mut is_curly = false;
                        let mut next = inner_iter.peek();

                        if next == Some(&'{') {
                            is_curly = true;

                            inner_iter.next();
                            next = inner_iter.peek();
                        } else if next != Some(&'D') && next != Some(&'d') {
                            buf.push('\\');
                        }

                        if let Some(c @ 'D' | c @ 'd') = next {
                            let mut inner_buf = String::new();

                            inner_buf.push('\\');
                            inner_buf.push('u');

                            if is_curly {
                                inner_buf.push('{');
                            }

                            inner_buf.push(*c);

                            inner_iter.next();

                            let mut is_valid = true;

                            for _ in 0..3 {
                                let c = inner_iter.next();

                                match c {
                                    Some('0'..='9') | Some('a'..='f') | Some('A'..='F') => {
                                        inner_buf.push(c.unwrap());
                                    }
                                    _ => {
                                        is_valid = false;

                                        break;
                                    }
                                }
                            }

                            if is_curly {
                                inner_buf.push('}');
                            }

                            let range = if is_curly {
                                3..(inner_buf.len() - 1)
                            } else {
                                2..6
                            };

                            if is_valid {
                                let val_str = &inner_buf[range];

                                let v = u32::from_str_radix(val_str, 16).unwrap_or_else(|err| {
                                    unreachable!(
                                        "failed to parse {} as a hex value: {:?}",
                                        val_str, err
                                    )
                                });

                                if v > 0xffff {
                                    buf.push_str(&inner_buf);

                                    let end = if is_curly { 7 } else { 5 };

                                    for _ in 0..end {
                                        iter.next();
                                    }
                                } else if (0xd800..=0xdfff).contains(&v) {
                                    buf.push('\\');
                                } else {
                                    buf.push_str("\\\\");
                                }
                            } else {
                                buf.push_str("\\\\")
                            }
                        } else if is_curly {
                            buf.push_str("\\\\");
                        } else {
                            buf.push('\\');
                        }
                    }
                    _ => {
                        buf.push_str("\\\\");
                    }
                }
            }
            '\'' => {
                single_quote_count += 1;
                buf.push('\'');
            }
            '"' => {
                double_quote_count += 1;
                buf.push('"');
            }
            '\x01'..='\x0f' => {
                let _ = write!(buf, "\\x0{:x}", c as u8);
            }
            '\x10'..='\x1f' => {
                let _ = write!(buf, "\\x{:x}", c as u8);
            }
            '\x20'..='\x7e' => {
                buf.push(c);
            }
            '\u{7f}'..='\u{ff}' => {
                if target <= EsVersion::Es5 {
                    let _ = write!(buf, "\\x{:x}", c as u8);
                } else {
                    buf.push(c);
                }
            }
            '\u{2028}' => {
                buf.push_str("\\u2028");
            }
            '\u{2029}' => {
                buf.push_str("\\u2029");
            }
            '\u{FEFF}' => {
                buf.push_str("\\uFEFF");
            }
            _ => {
                if c.is_ascii() {
                    buf.push(c);
                } else if c > '\u{FFFF}' {
                    // if we've got this far the char isn't reserved and if the callee has specified
                    // we should output unicode for non-ascii chars then we have
                    // to make sure we output unicode that is safe for the target
                    // Es5 does not support code point escapes and so surrograte formula must be
                    // used
                    if target <= EsVersion::Es5 {
                        // https://mathiasbynens.be/notes/javascript-encoding#surrogate-formulae
                        let h = ((c as u32 - 0x10000) / 0x400) + 0xd800;
                        let l = (c as u32 - 0x10000) % 0x400 + 0xdc00;

                        let _ = write!(buf, "\\u{:04X}\\u{:04X}", h, l);
                    } else {
                        buf.push(c);
                    }
                } else {
                    buf.push(c);
                }
            }
        }
    }

    if double_quote_count > single_quote_count {
        format!("'{}'", buf.replace('\'', "\\'"))
    } else {
        format!("\"{}\"", buf.replace('"', "\\\""))
    }
}

fn handle_invalid_unicodes(s: &str) -> Cow<str> {
    if !s.contains("\\\0") {
        return Cow::Borrowed(s);
    }

    Cow::Owned(s.replace("\\\0", "\\"))
}

fn require_space_before_rhs(rhs: &Expr, op: &BinaryOp) -> bool {
    match rhs {
        Expr::Lit(Lit::Num(v)) if v.value.is_sign_negative() && *op == op!(bin, "-") => true,

        Expr::Update(UpdateExpr {
            prefix: true,
            op: update,
            ..
        }) => matches!(
            (op, update),
            (op!(bin, "-"), op!("--")) | (op!(bin, "+"), op!("++"))
        ),

        // space is mandatory to avoid outputting <!--
        Expr::Unary(UnaryExpr {
            op: op!("!"), arg, ..
        }) if *op == op!("<") || *op == op!("<<") => {
            matches!(**arg, Expr::Update(UpdateExpr { op: op!("--"), .. }))
        }

        Expr::Unary(UnaryExpr { op: unary, .. }) => matches!(
            (op, unary),
            (op!(bin, "-"), op!(unary, "-")) | (op!(bin, "+"), op!(unary, "+"))
        ),

        Expr::Bin(BinExpr { left, .. }) => require_space_before_rhs(left, op),

        _ => false,
    }
}

fn minify_number(num: f64) -> String {
    // ddddd -> 0xhhhh
    // len(0xhhhh) == len(ddddd)
    // 10000000 <= num <= 0xffffff
    'hex: {
        if num.fract() == 0.0 && num.abs() <= u64::MAX as f64 {
            let int = num.abs() as u64;

            if int < 10000000 {
                break 'hex;
            }

            // use scientific notation
            if int % 1000 == 0 {
                break 'hex;
            }

            return format!(
                "{}{:#x}",
                if num.is_sign_negative() { "-" } else { "" },
                int
            );
        }
    }

    let mut num = num.to_string();

    if let Some(num) = num.strip_prefix("0.") {
        let cnt = clz(num);
        if cnt > 2 {
            return format!("{}e-{}", &num[cnt..], num.len());
        }
        return format!(".{}", num);
    }

    if let Some(num) = num.strip_prefix("-0.") {
        let cnt = clz(num);
        if cnt > 2 {
            return format!("-{}e-{}", &num[cnt..], num.len());
        }
        return format!("-.{}", num);
    }

    if num.ends_with("000") {
        let cnt = num
            .as_bytes()
            .iter()
            .rev()
            .skip(3)
            .take_while(|&&c| c == b'0')
            .count()
            + 3;

        num.truncate(num.len() - cnt);
        num.push('e');
        num.push_str(&cnt.to_string());
    }

    num
}

fn clz(s: &str) -> usize {
    s.as_bytes().iter().take_while(|&&c| c == b'0').count()
}

#[cold]
#[inline(never)]
fn invalid_pat() -> ! {
    unimplemented!("emit Pat::Invalid")
}

fn will_eat_else_token(s: &Stmt) -> bool {
    match s {
        Stmt::If(s) => match &s.alt {
            Some(alt) => will_eat_else_token(alt),
            None => true,
        },
        // Ends with `}`.
        Stmt::Block(..) => false,

        Stmt::Labeled(s) => will_eat_else_token(&s.body),
        Stmt::While(s) => will_eat_else_token(&s.body),
        Stmt::For(s) => will_eat_else_token(&s.body),
        Stmt::ForIn(s) => will_eat_else_token(&s.body),
        Stmt::ForOf(s) => will_eat_else_token(&s.body),

        _ => false,
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Context {
    #[default]
    Default,

    Callee {
        is_new: bool,
    },
    /// Always treated as expr. (But number of comma-separated expression
    /// matters)
    ///
    ///  - `foo((bar, x))` != `foo(bar, x)`
    ///  - `var foo = (bar, x)` != `var foo = bar, x`
    ///  - `[(foo, bar)]` != `[foo, bar]`
    ForcedExpr,

    /// Always treated as expr and comma does not matter.
    FreeExpr,
}

#[allow(non_upper_case_globals)]
mod flags {
    bitflags::bitflags! {
        pub(super) struct Flags: u8 {
            const in_for_stmt_head = 1 << 0;
            const in_opt_chain = 1 << 1;
            const in_expr_stmt_seq = 1 << 2;
            const in_assign_lhs_member_expr = 1 << 3;
        }
    }

    impl Flags {
        pub(super) fn replace(&mut self, flag: Flags, value: bool) -> bool {
            debug_assert_eq!(flag.bits.count_ones(), 1);
            let old = (self.bits & flag.bits) != 0;
            self.set(flag, value);
            old
        }
    }
}

use flags::Flags;
