#![recursion_limit = "1024"]
#![allow(unused_variables)]
// TODO: temp
#![allow(warnings)]

pub use self::config::Config;
use self::{
    list::ListFormat,
    text_writer::WriteJs,
    util::{SourceMapperExt, StartsWithAlphaNum},
};
use ast::*;
use atoms::JsWord;
use global_common::{
    comments::Comments, sync::Lrc, BytePos, SourceMap, Span, SyntaxContext, DUMMY_SP,
};
use parser::JscTarget;
use std::{borrow::Cow, fmt::Write, io, sync::Arc};

#[macro_use]
pub mod macros;
mod comments;
mod config;
mod decl;
mod expr;
mod jsx;
pub mod list;
mod stmt;
#[cfg(test)]
mod tests;
pub mod text_writer;
pub mod util;

pub type Result = io::Result<()>;

pub struct Emitter<'a> {
    pub cfg: config::Config,
    pub cm: Lrc<SourceMap>,
    pub comments: Option<&'a dyn Comments>,
    pub wr: Box<(dyn 'a + WriteJs)>,

    pub program_data: &'a ProgramData,
}

impl<'a> Emitter<'a> {
    pub fn emit_program(&mut self, node: &Program) -> Result {
        match *node {
            Program::Module(ref m) => self.emit_module(m),
            Program::Script(ref s) => self.emit_script(s),
        }
    }

    pub fn emit_module(&mut self, node: &Module) -> Result {
        if let Some(ref shebang) = node.shebang {
            punct!(self, "#!");
            self.wr.write_str_lit(DUMMY_SP, &*shebang)?;
            self.wr.write_line()?;
        }
        for stmt in &node.body {
            self.emit_module_item(stmt)?;
        }
        Ok(())
    }

    pub fn emit_script(&mut self, node: &Script) -> Result {
        if let Some(ref shebang) = node.shebang {
            punct!(self, "#!");
            self.wr.write_str_lit(DUMMY_SP, &*shebang)?;
            self.wr.write_line()?;
        }
        for stmt in &node.body {
            self.emit_stmt(stmt)?;
        }
        Ok(())
    }

    fn emit_module_item(&mut self, node: &ModuleItem) -> Result {
        match *node {
            ModuleItem::Stmt(ref stmt) => self.emit_stmt(stmt),
            ModuleItem::ModuleDecl(ref decl) => self.emit_module_decl(decl),
        }
    }

    fn emit_module_decl(&mut self, node: &ModuleDecl) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id()), false)?;

        match *node {
            ModuleDecl::Import(ref d) => self.emit_import(d),
            ModuleDecl::ExportDecl(ref d) => self.emit_export_decl(d),
            ModuleDecl::ExportNamed(ref d) => self.emit_named_export(d),
            ModuleDecl::ExportDefaultDecl(ref d) => self.emit_export_default_decl(d),
            ModuleDecl::ExportDefaultExpr(ref n) => self.emit_export_default_expr(n),
            ModuleDecl::ExportAll(ref d) => self.emit_export_all(d),
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
        space!(self);
        self.emit_expr(&node.expr)?;
        formatting_semi!(self);
        Ok(())
    }

    fn emit_export_default_decl(&mut self, node: &ExportDefaultDecl) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "export");
        space!(self);
        keyword!(self, "default");
        space!(self);
        match node.decl {
            DefaultDecl::Class(ref n) => self.emit_class_expr(n),
            DefaultDecl::Fn(ref n) => self.emit_fn_expr(n),
        }?;
        formatting_semi!(self);
        Ok(())
    }

    fn emit_import(&mut self, node: &ImportDecl) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        keyword!(self, "import");
        space!(self);

        let mut specifiers = vec![];
        let mut emitted_default = false;
        let mut emitted_ns = false;
        for specifier in &node.specifiers {
            match specifier {
                ImportSpecifier::Named(ref s) => {
                    specifiers.push(s);
                }
                ImportSpecifier::Default(ref s) => {
                    self.emit_ident(&s.local)?;
                    emitted_default = true;
                }
                ImportSpecifier::Namespace(ref ns) => {
                    if emitted_default {
                        punct!(self, ",");
                        formatting_space!(self);
                    }

                    emitted_ns = true;

                    assert!(node.specifiers.len() <= 2);
                    punct!(self, "*");
                    space!(self);
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
                |e, n| e.emit_import_specifier(n).map(|_| Some(n.node_id)),
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
        if let Some(ref imported) = node.imported {
            self.emit_ident(imported)?;
            space!(self);
            keyword!(self, "as");
            space!(self);
        }

        self.emit_ident(&node.local)
    }

    fn emit_export_specifier(&mut self, node: &ExportSpecifier) -> Result {
        match node {
            ExportSpecifier::Default(ref node) => {
                unimplemented!("codegen of `export default from 'foo';`")
            }
            ExportSpecifier::Namespace(ref node) => self.emit_namespace_export_specifier(node),
            ExportSpecifier::Named(ref node) => self.emit_named_export_specifier(node),
        }
    }

    fn emit_namespace_export_specifier(&mut self, node: &ExportNamespaceSpecifier) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        punct!(self, "*");
        formatting_space!(self);
        keyword!(self, "as");
        space!(self);
        self.emit_ident(&node.name)
    }

    fn emit_named_export_specifier(&mut self, node: &ExportNamedSpecifier) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        if let Some(ref exported) = node.exported {
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
        self.emit_leading_comments_of_span(span, false)?;

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
                    if let None = result.namespace_spec {
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
                |e, n| e.emit_export_specifier(n).map(|_| Some(n.node_id())),
                ListFormat::NamedImportsOrExportsElements,
            )?;
            punct!(self, "}");
        }

        if let Some(ref src) = node.src {
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
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "export");
        space!(self);
        punct!(self, "*");
        formatting_space!(self);
        keyword!(self, "from");
        space!(self);
        self.emit_str_lit(&node.src)?;
        formatting_semi!(self);
        Ok(())
    }

    fn emit_lit(&mut self, node: &Lit) -> Result {
        let span = get_span!(self, node.node_id());
        self.emit_leading_comments_of_span(span, false)?;

        match *node {
            Lit::Bool(Bool { value, .. }) => {
                if value {
                    keyword!(self, span, "true")
                } else {
                    keyword!(self, span, "false")
                }
            }
            Lit::Null(_) => keyword!(self, span, "null"),
            Lit::Str(ref s) => self.emit_str_lit(s)?,
            Lit::BigInt(ref s) => self.emit_big_lit(s)?,
            Lit::Num(ref n) => self.emit_num_lit(n)?,
            Lit::Regex(ref n) => {
                punct!(self, "/");
                self.wr.write_str(&n.exp)?;
                punct!(self, "/");
                self.wr.write_str(&n.flags)?;
            }
            Lit::JSXText(ref n) => self.emit_jsx_text(n)?,
        }
        Ok(())
    }

    fn emit_js_word(&mut self, span: Span, value: &JsWord) -> Result {
        self.wr.write_str_lit(span, &value)
    }

    fn emit_str_lit(&mut self, node: &Str) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        let (single_quote, value) = match node.kind {
            StrKind::Normal { contains_quote } => {
                let single_quote = if contains_quote {
                    is_single_quote(&self.cm, span)
                } else {
                    None
                };

                let value =
                    escape_with_source(&self.cm, self.wr.target(), span, &node.value, single_quote);

                (single_quote.unwrap_or(false), value)
            }
            StrKind::Synthesized => {
                let single_quote = false;
                let value = escape_without_source(&node.value, self.wr.target(), single_quote);

                (single_quote, value)
            }
        };

        if single_quote {
            punct!(self, "'");
            self.wr.write_str_lit(span, &value)?;
            punct!(self, "'");
        } else {
            punct!(self, "\"");
            self.wr.write_str_lit(span, &value)?;
            punct!(self, "\"");
        }
        Ok(())
    }

    fn emit_num_lit(&mut self, num: &Number) -> Result {
        let span = get_span!(self, num.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        // Handle infinity
        if num.value.is_infinite() {
            if num.value.is_sign_negative() {
                self.wr.write_str_lit(span, "-")?;
            }
            self.wr.write_str_lit(span, "Infinity")
        } else {
            match &num.raw {
                Some(raw) => self.wr.write_str_lit(span, raw),
                _ => self.wr.write_str_lit(span, &num.value.to_string()),
            }
        }
    }

    fn emit_big_lit(&mut self, v: &BigInt) -> Result {
        let span = get_span!(self, v.node_id);
        self.emit_leading_comments_of_span(span, false)?;

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
        match *node {
            ExprOrSuper::Expr(ref e) => self.emit_expr(e),
            ExprOrSuper::Super(ref n) => self.emit_super(n),
        }
    }

    fn emit_super(&mut self, node: &Super) -> Result {
        keyword!(self, get_span!(self, node.node_id), "super");
        Ok(())
    }

    fn emit_expr(&mut self, node: &Expr) -> Result {
        match *node {
            Expr::Array(ref n) => self.emit_array_lit(n),
            Expr::Arrow(ref n) => self.emit_arrow_expr(n),
            Expr::Assign(ref n) => self.emit_assign_expr(n),
            Expr::Await(ref n) => self.emit_await_expr(n),
            Expr::Bin(ref n) => self.emit_bin_expr(n),
            Expr::Call(ref n) => self.emit_call_expr(n),
            Expr::Class(ref n) => self.emit_class_expr(n),
            Expr::Cond(ref n) => self.emit_cond_expr(n),
            Expr::Fn(ref n) => self.emit_fn_expr(n),
            Expr::Ident(ref n) => self.emit_ident(n),
            Expr::Lit(ref n) => self.emit_lit(n),
            Expr::Member(ref n) => self.emit_member_expr(n),
            Expr::MetaProp(ref n) => self.emit_meta_prop_expr(n),
            Expr::New(ref n) => self.emit_new_expr(n),
            Expr::Object(ref n) => self.emit_object_lit(n),
            Expr::Paren(ref n) => self.emit_paren_expr(n),
            Expr::Seq(ref n) => self.emit_seq_expr(n),
            Expr::TaggedTpl(ref n) => self.emit_tagged_tpl_lit(n),
            Expr::This(ref n) => self.emit_this_expr(n),
            Expr::Tpl(ref n) => self.emit_tpl_lit(n),
            Expr::Unary(ref n) => self.emit_unary_expr(n),
            Expr::Update(ref n) => self.emit_update_expr(n),
            Expr::Yield(ref n) => self.emit_yield_expr(n),
            Expr::PrivateName(ref n) => self.emit_private_name(n),

            Expr::JSXMember(ref n) => self.emit_jsx_member_expr(n),
            Expr::JSXNamespacedName(ref n) => self.emit_jsx_namespaced_name(n),
            Expr::JSXEmpty(ref n) => self.emit_jsx_empty_expr(n),
            Expr::JSXElement(ref n) => self.emit_jsx_element(n),
            Expr::JSXFragment(ref n) => self.emit_jsx_fragment(n),

            Expr::OptChain(ref n) => self.emit_opt_chain(n),
            Expr::Invalid(ref n) => self.emit_invalid(n),
        }
    }

    fn emit_opt_chain(&mut self, n: &OptChainExpr) -> Result {
        let span = get_span!(self, n.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        match *n.expr {
            Expr::Member(ref e) => {
                self.emit_expr_or_super(&e.obj)?;
                punct!(self, "?.");

                if e.computed {
                    punct!(self, "[");
                    self.emit_expr(&e.prop)?;
                    punct!(self, "]");
                } else {
                    self.emit_expr(&e.prop)?;
                }
            }
            Expr::Call(ref e) => {
                self.emit_expr_or_super(&e.callee)?;
                punct!(self, "?.");

                punct!(self, "(");
                self.emit_expr_or_spreads(span, &e.args, ListFormat::CallExpressionArguments)?;
                punct!(self, ")");
            }
            _ => {}
        }
        Ok(())
    }

    fn emit_invalid(&mut self, n: &Invalid) -> Result {
        let span = get_span!(self, n.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        self.wr.write_str_lit(span, "<invalid>")
    }

    fn emit_call_expr(&mut self, node: &CallExpr) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        self.emit_expr_or_super(&node.callee)?;

        punct!(self, "(");
        self.emit_expr_or_spreads(span, &node.args, ListFormat::CallExpressionArguments)?;
        punct!(self, ")");
        Ok(())
    }

    fn emit_new_expr(&mut self, node: &NewExpr) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        {
            let span = self.cm.span_until_char(span, ' ');
            keyword!(self, span, "new");
        }
        space!(self);
        self.emit_expr(&node.callee)?;

        if let Some(ref args) = node.args {
            punct!(self, "(");
            self.emit_expr_or_spreads(span, args, ListFormat::NewExpressionArguments)?;
            punct!(self, ")");
        }
        Ok(())
    }

    fn emit_member_expr(&mut self, node: &MemberExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        self.emit_expr_or_super(&node.obj)?;

        if node.computed {
            punct!(self, "[");
            self.emit_expr(&node.prop)?;
            punct!(self, "]");
        } else {
            let prop_span = get_span!(self, node.prop.node_id());
            if self.needs_2dots_for_property_access(&node.obj) {
                if prop_span.lo() >= BytePos(2) {
                    self.emit_leading_comments(prop_span.lo() - BytePos(2), false)?;
                }
                punct!(self, ".");
            }
            if prop_span.lo() >= BytePos(1) {
                self.emit_leading_comments(prop_span.lo() - BytePos(1), false)?;
            }
            punct!(self, ".");
            self.emit_expr(&node.prop)?;
        }
        Ok(())
    }

    /// `1..toString` is a valid property access, emit a dot after the literal
    pub fn needs_2dots_for_property_access(&self, expr: &ExprOrSuper) -> bool {
        match *expr {
            ExprOrSuper::Expr(ref expr) => {
                match expr.as_ref() {
                    Expr::Lit(Lit::Num(Number { value, raw, .. })) => {
                        if value.is_nan() || value.is_infinite() {
                            return false;
                        }
                        match raw {
                            Some(raw) => {
                                if raw.bytes().all(|c| c.is_ascii_digit()) {
                                    // Legacy octal contains only digits, but `value` and `raw` are
                                    // different
                                    if !value.to_string().eq(raw.as_ref()) {
                                        return false;
                                    }

                                    return true;
                                }

                                false
                            }
                            _ => {
                                let s = value.to_string();
                                let bytes = s.as_bytes();

                                if !bytes.contains(&b'.') && !bytes.contains(&b'e') {
                                    return true;
                                }

                                false
                            }
                        }
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    }

    fn emit_arrow_expr(&mut self, node: &ArrowExpr) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        let space = !self.cfg.minify
            || match node.params.as_slice() {
                [ParamWithoutDecorators {
                    pat: Pat::Ident(_), ..
                }] => true,
                _ => false,
            };

        if node.is_async {
            keyword!(self, "async");
            if space {
                space!(self);
            } else {
                formatting_space!(self);
            }
        }

        let parens = !self.cfg.minify
            || match node.params.as_slice() {
                [ParamWithoutDecorators {
                    pat: Pat::Ident(_), ..
                }] => false,
                _ => true,
            };

        if parens {
            punct!(self, "(");
        }

        self.emit_list(
            span,
            &node.params,
            |e, n| e.emit_param_without_decorators(n).map(|_| Some(n.node_id)),
            ListFormat::CommaListElements,
        )?;
        if parens {
            punct!(self, ")");
        }

        punct!(self, "=>");
        self.emit_block_stmt_or_expr(&node.body)
    }

    fn emit_meta_prop_expr(&mut self, node: &MetaPropExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        self.emit_ident(&node.meta)?;
        punct!(self, ".");
        self.emit_ident(&node.prop)
    }

    fn emit_seq_expr(&mut self, node: &SeqExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        let mut first = true;
        //TODO: Indention
        for e in &node.exprs {
            if first {
                first = false
            } else {
                punct!(self, ",");
                formatting_space!(self);
            }

            self.emit_expr(e)?;
        }
        Ok(())
    }

    fn emit_assign_expr(&mut self, node: &AssignExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        self.emit_pat_or_expr(&node.left)?;
        formatting_space!(self);
        operator!(self, node.op.as_str());
        formatting_space!(self);
        self.emit_expr(&node.right)
    }

    /// Prints operator and right node of a binary expression.
    #[inline(never)]
    fn emit_bin_expr_trailing(&mut self, node: &BinExpr) -> Result {
        // let indent_before_op = needs_indention(node, &node.left, node.op);
        // let indent_after_op = needs_indention(node, node.op, &node.right);
        let need_space = match node.op {
            op!("in") | op!("instanceof") => true,
            _ => false,
        };

        let need_pre_space = need_space
            || match *node.left {
                Expr::Update(UpdateExpr { prefix: false, .. }) => true,
                _ => false,
            };
        if need_pre_space {
            space!(self);
        } else {
            formatting_space!(self);
        }
        operator!(self, node.op.as_str());

        let need_post_space = need_space
            || match *node.right {
                Expr::Unary(..) | Expr::Update(UpdateExpr { prefix: true, .. }) => true,
                _ => false,
            };
        if need_post_space {
            space!(self);
        } else {
            formatting_space!(self);
        }
        self.emit_expr(&node.right)
    }

    fn emit_bin_expr(&mut self, node: &BinExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        {
            let mut left = Some(node);
            let mut lefts = vec![];
            while let Some(l) = left {
                lefts.push(l);

                match &*l.left {
                    Expr::Bin(b) => {
                        left = Some(b);
                    }
                    _ => break,
                }
            }

            let len = lefts.len();

            for (i, left) in lefts.into_iter().rev().enumerate() {
                if i == 0 {
                    self.emit_expr(&left.left)?;
                }
                // Check if it's last
                if i + 1 != len {
                    self.emit_bin_expr_trailing(left)?;
                }
            }
        }

        self.emit_bin_expr_trailing(node)
    }

    fn emit_decorator(&mut self, node: &Decorator) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        punct!(self, "@");
        self.emit_expr(&node.expr)?;
        self.wr.write_line()
    }

    fn emit_class_expr(&mut self, node: &ClassExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        for dec in &node.class.decorators {
            self.emit_decorator(dec)?;
        }

        keyword!(self, "class");

        if let Some(ref i) = node.ident {
            space!(self);
            self.emit_ident(i)?;
        }

        self.emit_class_trailing(&node.class)
    }

    fn emit_class_trailing(&mut self, node: &Class) -> Result {
        if let Some(extends) = &node.extends {
            space!(self);
            self.emit_extends_clause(extends)?;
        }

        formatting_space!(self);
        punct!(self, "{");
        self.emit_list(
            get_span!(self, node.node_id),
            &node.body,
            |e: &mut Emitter, n| e.emit_class_member(n).map(|_| Some(n.node_id())),
            ListFormat::ClassMembers,
        )?;
        punct!(self, "}");
        Ok(())
    }

    fn emit_extends_clause(&mut self, node: &ExtendsClause) -> Result {
        keyword!(self, "extends");
        space!(self);
        self.emit_expr(&node.super_class)
    }

    fn emit_class_member(&mut self, node: &ClassMember) -> Result {
        match *node {
            ClassMember::Constructor(ref n) => self.emit_class_constructor(n),
            ClassMember::ClassProp(ref n) => self.emit_class_prop(n),
            ClassMember::Method(ref n) => self.emit_class_method(n),
            ClassMember::PrivateMethod(ref n) => self.emit_private_method(n),
            ClassMember::PrivateProp(ref n) => self.emit_private_prop(n),
            ClassMember::Empty(ref n) => self.emit_empty_stmt(n),
        }
    }

    fn emit_private_method(&mut self, n: &PrivateMethod) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, n.node_id), false)?;

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

    fn emit_bool(&mut self, n: &Bool) -> Result {
        let span = get_span!(self, n.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        if n.value {
            keyword!(self, span, "true")
        } else {
            keyword!(self, span, "false")
        }
        Ok(())
    }

    fn emit_class_method(&mut self, n: &ClassMethod) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, n.node_id), false)?;

        if n.is_static {
            keyword!(self, "static");
            space!(self);
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
                space!(self);
            }
            MethodKind::Setter => {
                keyword!(self, "set");
                space!(self);
            }
        }
        self.emit_prop_name(&n.key)?;

        punct!(self, "(");
        self.emit_list(
            get_span!(self, n.function.node_id),
            &n.function.params,
            |e, n| e.emit_param(n).map(|_| Some(n.node_id)),
            ListFormat::CommaListElements,
        )?;
        punct!(self, ")");

        formatting_space!(self);
        self.emit_block_stmt(&n.function.body)
    }

    fn emit_private_prop(&mut self, n: &PrivateProp) -> Result {
        let span = get_span!(self, n.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        self.emit_list(
            span,
            &n.decorators,
            |e, n| e.emit_decorator(n).map(|_| Some(n.node_id)),
            ListFormat::Decorators,
        )?;

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
        self.emit_leading_comments_of_span(get_span!(self, n.node_id), false)?;

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
        self.emit_leading_comments_of_span(span, false)?;

        keyword!(self, "constructor");
        punct!(self, "(");
        self.emit_list(
            span,
            &n.params,
            |e, n| e.emit_param(n).map(|_| Some(n.node_id)),
            ListFormat::Parameters,
        )?;
        punct!(self, ")");

        self.emit_block_stmt(&n.body)
    }

    fn emit_prop_name(&mut self, node: &PropName) -> Result {
        match *node {
            PropName::Ident(ref n) => self.emit_ident(n),
            PropName::Str(ref n) => self.emit_str_lit(n),
            PropName::Num(ref n) => self.emit_num_lit(n),
            PropName::Computed(ref n) => self.emit_computed_prop_name(n),
        }
    }

    fn emit_computed_prop_name(&mut self, node: &ComputedPropName) -> Result {
        punct!(self, "[");
        self.emit_expr(&node.expr)?;
        punct!(self, "]");
        Ok(())
    }

    fn emit_cond_expr(&mut self, node: &CondExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        self.emit_expr(&node.test)?;
        formatting_space!(self);
        punct!(self, "?");
        formatting_space!(self);
        self.emit_expr(&node.cons)?;
        formatting_space!(self);
        punct!(self, ":");
        formatting_space!(self);
        self.emit_expr(&node.alt)
    }

    fn emit_fn_expr(&mut self, node: &FnExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        if node.function.is_async {
            keyword!(self, "async");
            space!(self);
        }
        keyword!(self, "function");

        if node.function.is_generator {
            punct!(self, "*");
        }
        if let Some(ref i) = node.ident {
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
            |e, n| e.emit_param(n).map(|_| Some(n.node_id)),
            ListFormat::CommaListElements,
        )?;
        punct!(self, ")");

        formatting_space!(self);
        self.emit_block_stmt(&node.body)
    }

    fn emit_block_stmt_or_expr(&mut self, node: &BlockStmtOrExpr) -> Result {
        match *node {
            BlockStmtOrExpr::BlockStmt(ref block_stmt) => self.emit_block_stmt(block_stmt),
            BlockStmtOrExpr::Expr(ref expr) => {
                self.wr.increase_indent()?;
                self.emit_expr(expr)?;
                self.wr.decrease_indent()?;
                if !self.cfg.minify {
                    self.wr.write_line()?;
                }
                Ok(())
            }
        }
    }

    fn emit_this_expr(&mut self, node: &ThisExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "this");
        Ok(())
    }

    fn emit_tpl_lit(&mut self, node: &Tpl) -> Result {
        debug_assert!(node.quasis.len() == node.exprs.len() + 1);

        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        punct!(self, "`");
        let i = 0;

        for i in 0..(node.quasis.len() + node.exprs.len()) {
            if i % 2 == 0 {
                self.emit_quasi(&node.quasis[i / 2]);
            } else {
                punct!(self, "${");
                self.emit_expr(&node.exprs[i / 2]);
                punct!(self, "}");
            }
        }

        punct!(self, "`");
        Ok(())
    }

    fn emit_tagged_tpl_lit(&mut self, node: &TaggedTpl) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        self.emit_expr(&node.tag)?;
        self.emit_tpl_lit(&node.tpl)
    }

    fn emit_quasi(&mut self, node: &TplElement) -> Result {
        self.wr.write_str_lit(
            get_span!(self, node.node_id),
            &unescape_tpl_lit(&node.raw.value),
        )
    }

    fn emit_unary_expr(&mut self, node: &UnaryExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

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

        if should_emit_whitespace_before_operand(node) {
            space!(self);
        } else if need_formatting_space {
            formatting_space!(self);
        }

        self.emit_expr(&node.arg)
    }

    fn emit_update_expr(&mut self, node: &UpdateExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

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
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "yield");
        if node.delegate {
            operator!(self, "*");
        }

        if let Some(ref arg) = node.arg {
            if arg.starts_with_alpha_num() {
                space!(self);
            } else {
                formatting_space!(self);
            }
            self.emit_expr(arg)?;
        }
        Ok(())
    }

    fn emit_expr_or_spreads(
        &mut self,
        parent_node: Span,
        nodes: &[ExprOrSpread],
        format: ListFormat,
    ) -> Result {
        self.emit_list(
            parent_node,
            nodes,
            |e, n| e.emit_expr_or_spread(n).map(|_| Some(n.node_id())),
            format,
        )
    }

    fn emit_expr_or_spread(&mut self, node: &ExprOrSpread) -> Result {
        match node {
            ExprOrSpread::Spread(n) => self.emit_spread_element(n),
            ExprOrSpread::Expr(n) => self.emit_expr(n),
        }
    }

    fn emit_await_expr(&mut self, node: &AwaitExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "await");

        space!(self);

        self.emit_expr(&node.arg)
    }

    fn emit_array_lit(&mut self, node: &ArrayLit) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        punct!(self, "[");
        self.emit_list(
            span,
            &node.elems,
            |e, n| {
                if let Some(n) = n {
                    e.emit_expr_or_spread(n).map(|_| Some(n.node_id()))
                } else {
                    Ok(None)
                }
            },
            ListFormat::ArrayLiteralExpressionElements,
        )?;
        punct!(self, "]");
        Ok(())
    }

    fn emit_object_lit(&mut self, node: &ObjectLit) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        punct!(self, "{");
        if !self.cfg.minify {
            self.wr.write_line()?;
        }
        self.emit_list(
            span,
            &node.props,
            |e, n| e.emit_prop(n).map(|_| Some(n.node_id())),
            ListFormat::ObjectLiteralExpressionProperties,
        )?;
        if !self.cfg.minify {
            self.wr.write_line()?;
        }
        punct!(self, "}");
        Ok(())
    }

    fn emit_prop(&mut self, node: &Prop) -> Result {
        match *node {
            Prop::Shorthand(ref n) => self.emit_ident(n),
            Prop::KeyValue(ref n) => self.emit_kv_prop(n),
            Prop::Assign(ref n) => self.emit_assign_prop(n),
            Prop::Getter(ref n) => self.emit_getter_prop(n),
            Prop::Setter(ref n) => self.emit_setter_prop(n),
            Prop::Method(ref n) => self.emit_method_prop(n),
            Prop::Spread(ref n) => self.emit_spread_assignment(n),
        }
    }

    fn emit_kv_prop(&mut self, node: &KeyValueProp) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        self.emit_prop_name(&node.key)?;
        punct!(self, ":");
        formatting_space!(self);
        self.emit_expr(&node.value)
    }

    fn emit_assign_prop(&mut self, node: &AssignProp) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        self.emit_ident(&node.key)?;
        punct!(self, "=");
        self.emit_expr(&node.value)
    }

    fn emit_getter_prop(&mut self, node: &GetterProp) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "get");
        space!(self);
        self.emit_prop_name(&node.key)?;
        space!(self);
        punct!(self, "(");
        punct!(self, ")");
        formatting_space!(self);
        self.emit_block_stmt(&node.body)
    }

    fn emit_setter_prop(&mut self, node: &SetterProp) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "set");
        space!(self);
        self.emit_prop_name(&node.key)?;
        space!(self);

        punct!(self, "(");
        self.emit_param_without_decorators(&node.param)?;
        punct!(self, ")");

        self.emit_block_stmt(&node.body)
    }

    fn emit_method_prop(&mut self, node: &MethodProp) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

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
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        punct!(self, "...");
        self.emit_expr(&node.expr)
    }

    fn emit_paren_expr(&mut self, node: &ParenExpr) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        punct!(self, "(");
        self.emit_expr(&node.expr)?;
        punct!(self, ")");
        Ok(())
    }

    fn emit_private_name(&mut self, n: &PrivateName) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, n.node_id), false)?;

        punct!(self, "#");
        self.emit_ident(&n.id)
    }

    fn emit_binding_ident(&mut self, ident: &BindingIdent) -> Result {
        self.emit_ident(&ident.id)
    }

    fn emit_ident(&mut self, ident: &Ident) -> Result {
        let span = get_span!(self, ident.node_id);
        // TODO: Use write_symbol when ident is a symbol.
        self.emit_leading_comments_of_span(span, false)?;

        // TODO: span
        self.wr
            .write_symbol(span, &handle_invalid_unicodes(&ident.sym))
    }

    fn emit_list<N>(
        &mut self,
        parent_node: Span,
        children: &[N],
        emit_child: impl Fn(&mut Emitter, &N) -> io::Result<Option<NodeId>>,
        format: ListFormat,
    ) -> Result {
        self.emit_list5(parent_node, children, emit_child, format, 0, children.len())
    }

    #[allow(clippy::cognitive_complexity)]
    fn emit_list5<N>(
        &mut self,
        parent_node: Span,
        children: &[N],
        emit_child: impl Fn(&mut Emitter, &N) -> io::Result<Option<NodeId>>,
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

            if is_empty {
                self.emit_trailing_comments_of_pos(
                    {
                        // TODO: children.lo()

                        parent_node.lo()
                    },
                    true,
                    false,
                )?;
            }
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
            let may_emit_intervening_comments =
                !format.intersects(ListFormat::NoInterveningComments);
            let mut should_emit_intervening_comments = may_emit_intervening_comments;
            if self.cm.should_write_leading_line_terminator(
                parent_node,
                children,
                format,
                self.program_data,
            ) {
                if !self.cfg.minify {
                    self.wr.write_line()?;
                }
                should_emit_intervening_comments = false;
            } else if format.contains(ListFormat::SpaceBetweenBraces) && !self.cfg.minify {
                self.wr.write_space()?;
            }

            // Increase the indent, if requested.
            if format.contains(ListFormat::Indented) && !self.cfg.minify {
                self.wr.increase_indent()?;
            }

            // Emit each child.
            let mut previous_sibling: Option<Span> = None;
            let mut should_decrease_indent_after_emit = false;
            for i in 0..count {
                let child = &children[start + i];

                // Write the delimiter if this is not the first node.
                if let Some(previous_sibling) = previous_sibling {
                    // i.e
                    //      function commentedParameters(
                    //          /* Parameter a */
                    //          a
                    // /* End of parameter a */
                    // -> this comment isn't considered to be trailing comment of parameter "a" due
                    // to newline ,
                    if format.contains(ListFormat::DelimitersMask)
                        && previous_sibling.hi() != parent_node.hi()
                    {
                        self.emit_leading_comments(previous_sibling.hi(), true)?;
                    }

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
                        should_emit_intervening_comments = false;
                    } else if format.contains(ListFormat::SpaceBetweenSiblings) {
                        formatting_space!(self);
                    }
                }

                let child_span = emit_child(self, child)?
                    .map(|id| get_span!(self, id))
                    .unwrap_or(DUMMY_SP);

                // Emit this child.
                if should_emit_intervening_comments {
                    self.emit_trailing_comments_of_pos(child_span.hi(), false, true)?;
                } else {
                    should_emit_intervening_comments = may_emit_intervening_comments;
                }

                if should_decrease_indent_after_emit {
                    self.wr.decrease_indent()?;
                    should_decrease_indent_after_emit = false;
                }

                previous_sibling = Some(child_span);
            }

            // Write a trailing comma, if requested.
            let has_trailing_comma = format.contains(ListFormat::AllowTrailingComma) && {
                if parent_node.is_dummy() {
                    false
                } else {
                    match self.cm.span_to_snippet(parent_node) {
                        Ok(snippet) => {
                            if snippet.len() < 3 {
                                false
                            } else {
                                snippet[..snippet.len() - 1].trim().ends_with(',')
                            }
                        }
                        _ => false,
                    }
                }
            };

            if has_trailing_comma && format.contains(ListFormat::CommaDelimited) {
                punct!(self, ",");
                formatting_space!(self);
            }

            {
                // Emit any trailing comment of the last element in the list
                // i.e
                //       var array = [...
                //          2
                //          /* end of element 2 */
                //       ];

                let emit_trailing_comments = {
                    // TODO:
                    //
                    // !(getEmitFlags(previousSibling).contains(EmitFlags::NoTrailingComments))

                    true
                };

                if let Some(previous_sibling) = previous_sibling {
                    if format.contains(ListFormat::DelimitersMask)
                        && previous_sibling.hi() != parent_node.hi()
                        && emit_trailing_comments
                    {
                        self.emit_leading_comments(previous_sibling.hi(), true)?;
                    }
                }
            }

            // Decrease the indent, if requested.
            if format.contains(ListFormat::Indented) && !self.cfg.minify {
                self.wr.decrease_indent()?;
            }

            // Write the closing line terminator or closing whitespace.
            if self.cm.should_write_closing_line_terminator(
                parent_node,
                children,
                format,
                self.program_data,
            ) {
                if !self.cfg.minify {
                    self.wr.write_line()?;
                }
            } else if format.contains(ListFormat::SpaceBetweenBraces) && !self.cfg.minify {
                self.wr.write_space()?;
            }
        }

        // self.handlers.onAfterEmitNodeArray(children);

        if format.contains(ListFormat::BracketsMask) {
            if is_empty {
                self.emit_leading_comments(
                    {
                        //TODO: children.hi()

                        parent_node.hi()
                    },
                    true,
                )?; // Emit leading comments within empty lists
            }
            self.wr.write_punct(None, format.closing_bracket())?;
        }

        Ok(())
    }
}

/// Patterns
impl<'a> Emitter<'a> {
    fn emit_param(&mut self, node: &Param) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        self.emit_list(
            span,
            &node.decorators,
            |e, d| e.emit_decorator(d).map(|_| Some(d.node_id)),
            ListFormat::Decorators,
        )?;

        self.emit_pat(&node.pat)
    }

    fn emit_param_without_decorators(&mut self, node: &ParamWithoutDecorators) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        self.emit_pat(&node.pat)
    }

    fn emit_pat(&mut self, node: &Pat) -> Result {
        match *node {
            Pat::Array(ref n) => self.emit_array_pat(n),
            Pat::Assign(ref n) => self.emit_assign_pat(n),
            Pat::Expr(ref n) => self.emit_expr(n),
            Pat::Ident(ref n) => self.emit_binding_ident(n),
            Pat::Object(ref n) => self.emit_object_pat(n),
            Pat::Rest(ref n) => self.emit_rest_pat(n),
            Pat::Invalid(..) => invalid_pat(),
        }
    }

    fn emit_rest_pat(&mut self, node: &RestPat) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        punct!(self, "...");
        self.emit_pat(&node.arg)
    }

    fn emit_spread_element(&mut self, node: &SpreadElement) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        punct!(self, "...");
        self.emit_expr(&node.expr)
    }

    fn emit_pat_or_expr(&mut self, node: &PatOrExpr) -> Result {
        match *node {
            PatOrExpr::Expr(ref n) => self.emit_expr(n),
            PatOrExpr::Pat(ref n) => self.emit_pat(n),
        }
    }

    fn emit_array_pat(&mut self, node: &ArrayPat) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        punct!(self, "[");
        self.emit_list(
            span,
            &node.elems,
            |e, n| {
                if let Some(n) = n {
                    e.emit_pat(n).map(|_| Some(n.node_id()))
                } else {
                    Ok(None)
                }
            },
            ListFormat::ArrayBindingPatternElements,
        )?;
        punct!(self, "]");
        Ok(())
    }

    fn emit_assign_pat(&mut self, node: &AssignPat) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        self.emit_pat(&node.left)?;
        formatting_space!(self);
        punct!(self, "=");
        formatting_space!(self);
        self.emit_expr(&node.right)
    }

    fn emit_object_pat(&mut self, node: &ObjectPat) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        let is_last_rest = match node.props.last() {
            Some(ObjectPatProp::Rest(..)) => true,
            _ => false,
        };
        let format = if is_last_rest {
            ListFormat::ObjectBindingPatternElements ^ ListFormat::AllowTrailingComma
        } else {
            ListFormat::ObjectBindingPatternElements
        };

        punct!(self, "{");
        self.emit_list(
            span,
            &node.props,
            |e, n| e.emit_object_pat_prop(n).map(|_| Some(n.node_id())),
            format,
        )?;
        punct!(self, "}");
        Ok(())
    }

    fn emit_object_pat_prop(&mut self, node: &ObjectPatProp) -> Result {
        match *node {
            ObjectPatProp::KeyValue(ref node) => self.emit_object_kv_pat(node),
            ObjectPatProp::Assign(ref node) => self.emit_object_assign_pat(node),
            ObjectPatProp::Rest(ref node) => self.emit_rest_pat(node),
        }
    }

    fn emit_object_kv_pat(&mut self, node: &KeyValuePatProp) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        self.emit_prop_name(&node.key)?;
        punct!(self, ":");
        formatting_space!(self);
        self.emit_pat(&node.value)?;
        space!(self);
        Ok(())
    }

    fn emit_object_assign_pat(&mut self, node: &AssignPatProp) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        self.emit_ident(&node.key)?;
        formatting_space!(self);
        if let Some(ref value) = node.value {
            punct!(self, "=");
            self.emit_expr(&value)?;
            space!(self);
        }
        Ok(())
    }

    fn emit_var_decl_or_pat(&mut self, node: &VarDeclOrPat) -> Result {
        match *node {
            VarDeclOrPat::Pat(ref n) => self.emit_pat(n),
            VarDeclOrPat::VarDecl(ref n) => self.emit_var_decl(n),
        }
    }
}

/// Statements
impl<'a> Emitter<'a> {
    fn emit_stmt(&mut self, node: &Stmt) -> Result {
        match *node {
            Stmt::Expr(ref e) => self.emit_expr_stmt(e),
            Stmt::Block(ref e) => {
                return self.emit_block_stmt(e);
            }
            Stmt::Empty(ref e) => self.emit_empty_stmt(e),
            Stmt::Debugger(ref e) => self.emit_debugger_stmt(e),
            Stmt::With(ref e) => self.emit_with_stmt(e),
            Stmt::Return(ref e) => self.emit_return_stmt(e),
            Stmt::Labeled(ref e) => self.emit_labeled_stmt(e),
            Stmt::Break(ref e) => self.emit_break_stmt(e),
            Stmt::Continue(ref e) => self.emit_continue_stmt(e),
            Stmt::If(ref e) => self.emit_if_stmt(e),
            Stmt::Switch(ref e) => self.emit_switch_stmt(e),
            Stmt::Throw(ref e) => self.emit_throw_stmt(e),
            Stmt::Try(ref e) => self.emit_try_stmt(e),
            Stmt::While(ref e) => self.emit_while_stmt(e),
            Stmt::DoWhile(ref e) => self.emit_do_while_stmt(e),
            Stmt::For(ref e) => self.emit_for_stmt(e),
            Stmt::ForIn(ref e) => self.emit_for_in_stmt(e),
            Stmt::ForOf(ref e) => self.emit_for_of_stmt(e),
            Stmt::Decl(ref e) => self.emit_decl(e),
        }?;
        self.emit_trailing_comments_of_pos(get_span!(self, node.node_id()).hi(), true, true)?;

        if !self.cfg.minify {
            self.wr.write_line()?;
        }
        Ok(())
    }

    fn emit_expr_stmt(&mut self, e: &ExprStmt) -> Result {
        self.emit_expr(&e.expr)?;
        formatting_semi!(self);
        Ok(())
    }

    fn emit_block_stmt(&mut self, node: &BlockStmt) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        {
            let span = if span.is_dummy() {
                DUMMY_SP
            } else {
                Span::new(span.lo, span.lo + BytePos(1))
            };
            punct!(self, span, "{");
        }
        self.emit_list(
            span,
            &node.stmts,
            |e, n| e.emit_stmt(n).map(|_| Some(n.node_id())),
            ListFormat::MultiLineBlockStatements,
        )?;

        self.emit_leading_comments_of_span(span, true)?;

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

    fn emit_empty_stmt(&mut self, node: &EmptyStmt) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        semi!(self);
        Ok(())
    }

    fn emit_debugger_stmt(&mut self, node: &DebuggerStmt) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

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

        self.emit_stmt(&node.body)
    }

    fn emit_return_stmt(&mut self, node: &ReturnStmt) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "return");
        if let Some(ref arg) = node.arg {
            let arg_span = get_span!(self, arg.node_id());
            let need_paren = !arg_span.is_dummy()
                && if let Some(cmt) = self.comments {
                    let lo = arg_span.lo();

                    // see #415
                    cmt.has_leading(lo)
                } else {
                    false
                };
            if need_paren {
                punct!(self, "(");
            } else {
                space!(self);
            }

            self.emit_expr(arg)?;
            if need_paren {
                punct!(self, ")");
            }
        }
        formatting_semi!(self);
        Ok(())
    }

    fn emit_labeled_stmt(&mut self, node: &LabeledStmt) -> Result {
        self.emit_ident(&node.label)?;

        // TODO: Comment
        punct!(self, ":");
        formatting_space!(self);

        self.emit_stmt(&node.body)
    }

    fn emit_break_stmt(&mut self, node: &BreakStmt) -> Result {
        keyword!(self, "break");
        if let Some(ref label) = node.label {
            space!(self);
            self.emit_ident(label)?;
        }
        formatting_semi!(self);
        Ok(())
    }

    fn emit_continue_stmt(&mut self, node: &ContinueStmt) -> Result {
        keyword!(self, "continue");
        if let Some(ref label) = node.label {
            space!(self);
            self.emit_ident(label)?;
        }
        formatting_semi!(self);
        Ok(())
    }

    fn emit_if_stmt(&mut self, node: &IfStmt) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        {
            let span = self.cm.span_until_char(span, ' ');
            keyword!(self, span, "if");
        }

        formatting_space!(self);
        punct!(self, "(");
        self.emit_expr(&node.test)?;
        punct!(self, ")");
        formatting_space!(self);

        let is_cons_block = match *node.cons {
            Stmt::Block(..) => true,
            _ => false,
        };

        self.emit_stmt(&node.cons)?;

        if let Some(ref alt) = node.alt {
            if is_cons_block {
                formatting_space!(self);
            }
            keyword!(self, "else");
            if alt.starts_with_alpha_num() {
                space!(self);
            } else {
                formatting_space!(self);
            }
            self.emit_stmt(alt)?;
        }
        Ok(())
    }

    fn emit_switch_stmt(&mut self, node: &SwitchStmt) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        keyword!(self, "switch");

        punct!(self, "(");
        self.emit_expr(&node.discriminant)?;
        punct!(self, ")");

        punct!(self, "{");
        self.emit_list(
            span,
            &node.cases,
            |e, n| e.emit_switch_case(n).map(|_| Some(n.node_id)),
            ListFormat::CaseBlockClauses,
        )?;
        punct!(self, "}");
        Ok(())
    }

    fn emit_catch_clause(&mut self, node: &CatchClause) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "catch");
        formatting_space!(self);

        if let Some(param) = &node.param {
            punct!(self, "(");
            self.emit_pat(param)?;
            punct!(self, ")");
        }

        space!(self);

        self.emit_block_stmt(&node.body)
    }

    fn emit_switch_case(&mut self, node: &SwitchCase) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        if let Some(ref test) = node.test {
            keyword!(self, "case");
            space!(self);
            self.emit_expr(test)?;
        } else {
            keyword!(self, "default");
        }

        let emit_as_single_stmt = node.cons.len() == 1;

        let mut format = ListFormat::CaseOrDefaultClauseStatements;
        if emit_as_single_stmt {
            punct!(self, ":");
            space!(self);
            format &= !(ListFormat::MultiLine | ListFormat::Indented);
        } else {
            punct!(self, ":");
        }
        self.emit_list(
            span,
            &node.cons,
            |e, n| e.emit_stmt(n).map(|_| Some(n.node_id())),
            format,
        )
    }

    fn emit_throw_stmt(&mut self, node: &ThrowStmt) -> Result {
        let span = get_span!(self, node.node_id);
        self.emit_leading_comments_of_span(span, false)?;

        let throw_span = self.cm.span_until_char(span, ' ');

        keyword!(self, throw_span, "throw");
        space!(self);
        self.emit_expr(&node.arg)?;
        formatting_semi!(self);
        Ok(())
    }

    fn emit_try_stmt(&mut self, node: &TryStmt) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "try");
        formatting_space!(self);
        self.emit_block_stmt(&node.block)?;

        if let Some(ref catch) = node.handler {
            formatting_space!(self);
            self.emit_catch_clause(catch)?;
        }

        if let Some(ref finally) = node.finalizer {
            formatting_space!(self);
            keyword!(self, "finally");
            // space!();
            self.emit_block_stmt(finally)?;
        }
        Ok(())
    }

    fn emit_while_stmt(&mut self, node: &WhileStmt) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "while");

        punct!(self, "(");
        self.emit_expr(&node.test)?;
        punct!(self, ")");

        self.emit_stmt(&node.body)
    }

    fn emit_do_while_stmt(&mut self, node: &DoWhileStmt) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "do");
        if node.body.starts_with_alpha_num() {
            space!(self);
        } else {
            formatting_space!(self);
        }
        self.emit_stmt(&node.body)?;

        keyword!(self, "while");

        formatting_space!(self);

        punct!(self, "(");
        self.emit_expr(&node.test)?;
        punct!(self, ")");
        Ok(())
    }

    fn emit_for_stmt(&mut self, node: &ForStmt) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "for");
        punct!(self, "(");
        opt!(self, emit_var_decl_or_expr, node.init);
        semi!(self);
        opt_leading_space!(self, emit_expr, node.test);
        semi!(self);
        opt_leading_space!(self, emit_expr, node.update);
        punct!(self, ")");

        self.emit_stmt(&node.body)
    }

    fn emit_for_in_stmt(&mut self, node: &ForInStmt) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "for");
        punct!(self, "(");
        self.emit_var_decl_or_pat(&node.left)?;
        space!(self);
        keyword!(self, "in");
        space!(self);
        self.emit_expr(&node.right)?;
        punct!(self, ")");

        self.emit_stmt(&node.body)
    }

    fn emit_for_of_stmt(&mut self, node: &ForOfStmt) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        keyword!(self, "for");
        if node.is_await {
            space!(self);
            keyword!(self, "await");
        }
        formatting_space!(self);
        punct!(self, "(");
        self.emit_var_decl_or_pat(&node.left)?;
        space!(self);
        keyword!(self, "of");
        space!(self);
        self.emit_expr(&node.right)?;
        punct!(self, ")");
        self.emit_stmt(&node.body)
    }
}

impl<'a> Emitter<'a> {
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
        match *node {
            VarDeclOrExpr::Expr(ref node) => self.emit_expr(node),
            VarDeclOrExpr::VarDecl(ref node) => self.emit_var_decl(node),
        }
    }
}

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
fn should_emit_whitespace_before_operand(node: &UnaryExpr) -> bool {
    match *node {
        UnaryExpr {
            op: op!("void"), ..
        }
        | UnaryExpr {
            op: op!("typeof"), ..
        }
        | UnaryExpr {
            op: op!("delete"), ..
        } => return node.arg.starts_with_alpha_num(),
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
        }) if node.op == op!(unary, "+") => true,
        Expr::Update(UpdateExpr {
            op: op!("--"),
            prefix: true,
            ..
        })
        | Expr::Unary(UnaryExpr {
            op: op!(unary, "-"),
            ..
        }) if node.op == op!(unary, "-") => true,
        _ => false,
    }
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
                    if chars.peek().map(|&v| v) == Some('\n') {
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

fn escape_without_source(v: &str, target: JscTarget, single_quote: bool) -> String {
    let mut buf = String::with_capacity(v.len());
    let mut iter = v.chars().peekable();

    while let Some(c) = iter.next() {
        match c {
            '\u{0008}' => buf.push_str("\\b"),
            '\u{000c}' => buf.push_str("\\f"),
            '\n' => buf.push_str("\\n"),
            '\r' => buf.push_str("\\r"),
            '\t' => buf.push_str("\\t"),
            '\u{000b}' => buf.push_str("\\v"),
            '\0' => buf.push_str("\\x00"),

            '\\' => {
                if iter.peek() == Some(&'\0') {
                    buf.push_str("\\");
                    iter.next();
                } else {
                    buf.push_str("\\\\")
                }
            }

            '\'' if single_quote => buf.push_str("\\'"),
            '"' if !single_quote => buf.push_str("\\\""),

            '\x01'..='\x0f' => {
                let _ = write!(buf, "\\x0{:x}", c as u8);
            }
            '\x10'..='\x1f' => {
                let _ = write!(buf, "\\x{:x}", c as u8);
            }

            '\x20'..='\x7e' => {
                //
                buf.push(c);
            }
            '\u{7f}'..='\u{ff}' => {
                let _ = write!(buf, "\\x{:x}", c as u8);
            }

            _ => {
                buf.push(c);
            }
        }
    }

    buf
}

fn escape_with_source<'s>(
    cm: &SourceMap,
    target: JscTarget,
    span: Span,
    s: &'s str,
    single_quote: Option<bool>,
) -> String {
    if target <= JscTarget::Es5 {
        return escape_without_source(s, target, single_quote.unwrap_or(false));
    }

    if span.is_dummy() {
        return escape_without_source(s, target, single_quote.unwrap_or(false));
    }

    //
    let orig = cm.span_to_snippet(span);
    let orig = match orig {
        Ok(orig) => orig,
        Err(v) => {
            return escape_without_source(s, target, single_quote.unwrap_or(false));
        }
    };

    if single_quote.is_some() && orig.len() <= 2 {
        return escape_without_source(s, target, single_quote.unwrap_or(false));
    }

    let mut orig = &*orig;

    if (single_quote == Some(true) && orig.starts_with('\''))
        || (single_quote == Some(false) && orig.starts_with('"'))
    {
        orig = &orig[1..orig.len() - 1];
    } else {
        if single_quote.is_some() {
            return escape_without_source(s, target, single_quote.unwrap_or(false));
        }
    }

    let mut buf = String::with_capacity(s.len());
    let mut orig_iter = orig.chars().peekable();
    let mut s_iter = s.chars();

    while let Some(orig_c) = orig_iter.next() {
        // Javascript literal should not contain newlines
        if orig_c == '\n' {
            s_iter.next();
            s_iter.next();
            buf.push_str("\\n");
            continue;
        }

        if single_quote.is_none() && orig_c == '"' {
            s_iter.next();
            s_iter.next();
            buf.push_str("\\\"");
            continue;
        }

        if orig_c == '\\' {
            if s_iter.as_str().starts_with("\\\0") {
                for _ in 0..6 {
                    s_iter.next();
                }
            }

            buf.push('\\');
            match orig_iter.next() {
                Some('\\') => {
                    buf.push('\\');
                    s_iter.next();
                    continue;
                }
                Some(escaper) => {
                    buf.push(escaper);
                    match escaper {
                        'x' => {
                            buf.extend(orig_iter.next());
                            buf.extend(orig_iter.next());
                            s_iter.next();
                        }
                        'u' => match orig_iter.next() {
                            Some('{') => {
                                buf.push('{');
                                loop {
                                    let ch = orig_iter.next();
                                    buf.extend(ch);
                                    if ch == Some('}') {
                                        break;
                                    }
                                }
                                s_iter.next();
                            }
                            Some(ch) => {
                                buf.push(ch);
                                buf.extend(orig_iter.next());
                                buf.extend(orig_iter.next());
                                buf.extend(orig_iter.next());
                                s_iter.next();
                            }
                            None => break,
                        },
                        'b' | 'f' | 'n' | 'r' | 't' | 'v' | '0' => {
                            s_iter.next();
                        }

                        '\'' if single_quote == Some(true) => {
                            s_iter.next();
                        }

                        '"' if single_quote == Some(false) => {
                            s_iter.next();
                        }

                        _ => {
                            s_iter.next();
                        }
                    }

                    continue;
                }
                _ => {}
            }
        }

        s_iter.next();
        buf.push(orig_c);
    }

    buf.extend(s_iter);

    buf
}

/// Returns [Some] if the span points to a string literal written by user.
///
/// Returns [None] if the span is created from a pass of swc. For example,
/// spans of string literals created from [TplElement] do not have `starting`
/// quote.
fn is_single_quote(cm: &SourceMap, span: Span) -> Option<bool> {
    if span.is_dummy() {
        return None;
    }

    let start = cm.lookup_byte_offset(span.lo);
    let end = cm.lookup_byte_offset(span.hi);

    if start.sf.start_pos != end.sf.start_pos {
        return None;
    }

    // Empty file
    if start.sf.start_pos == start.sf.end_pos {
        return None;
    }

    let start_index = start.pos.0;
    let end_index = end.pos.0;
    let source_len = (start.sf.end_pos - start.sf.start_pos).0;

    if start_index > end_index || end_index > source_len {
        return None;
    }

    let src = &start.sf.src;
    let single_quote = match src.as_bytes()[start_index as usize] {
        b'\'' => true,
        b'"' => false,
        _ => return None,
    };
    if end_index == 0 {
        return None;
    }

    if src.as_bytes()[start_index as usize] != src.as_bytes()[(end_index - 1) as usize] {
        return None;
    }

    Some(single_quote)
}

fn handle_invalid_unicodes(s: &str) -> Cow<str> {
    if !s.contains("\\\0") {
        return Cow::Borrowed(s);
    }

    Cow::Owned(s.replace("\\\0", "\\"))
}

#[cold]
#[inline(never)]
fn invalid_pat() -> ! {
    unimplemented!("emit Pat::Invalid")
}
