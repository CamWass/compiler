use util::AssignProps;

use super::*;
use crate::context::{ContextFlags, YesMaybe, YesNoMaybe};

impl Parser<'_> {
    #[allow(clippy::cognitive_complexity)]
    fn parse_import(&mut self) -> PResult<ModuleItem> {
        let start = self.input.cur_pos();

        if self.peeked_is(tok!('.')) {
            let expr = self.parse_expr(&mut AssignProps::Emit)?.unwrap();

            self.eat_semi_with_asi();

            return Ok(ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                node_id: node_id!(self, self.span(start)),
                expr,
            })));
        }

        if self.input.syntax().dynamic_import() && self.peeked_is(tok!('(')) {
            let expr = self.parse_expr(&mut AssignProps::Emit)?.unwrap();

            self.eat_semi_with_asi();

            return Ok(ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                node_id: node_id!(self, self.span(start)),
                expr,
            })));
        }

        // It's now import statement

        if !self.ctx().is_module() {
            // Switch to module mode
            let ctx = Context {
                module: YesNoMaybe::Yes,
                strict: YesMaybe::Yes,
                ..self.ctx()
            };
            self.set_ctx(ctx);
        }

        expect!(self, "import");

        if self.input.syntax().typescript() && self.is_ident_ref() && self.peeked_is(tok!('=')) {
            todo!();
            // return self
            //     .parse_ts_import_equals_decl(start, false, false)
            //     .map(ModuleDecl::from)
            //     .map(ModuleItem::from);
        }

        // Handle import 'mod.js'
        let str_start = self.input.cur_pos();
        if let Token::Str { .. } = self.input.cur() {
            let src = match self.input.bump() {
                Token::Str { value } => Str {
                    node_id: node_id!(self, self.span(str_start)),
                    value,
                },
                _ => unreachable!(),
            };
            self.expect_semi_with_asi()?;
            return Ok(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
                node_id: node_id!(self, self.span(start)),
                src,
                specifiers: vec![],
                asserts: None,
            })));
        }

        let type_only = self.input.syntax().typescript()
            && self.is(tok!("type"))
            && (self.peeked_is(tok!('{'))
                || !self.peeked_is(tok!("from")) && !self.peeked_is(tok!(',')));

        if type_only {
            self.assert_and_bump(tok!("type"));

            if self.is_ident_ref() && self.peeked_is(tok!('=')) {
                todo!();
                // return self
                //     .parse_ts_import_equals_decl(start, false, true)
                //     .map(ModuleDecl::from)
                //     .map(ModuleItem::from);
            }
        }

        let mut specifiers = vec![];

        let ctx = self.ctx();
        if matches!(self.input.cur(), Token::Word(w) if !ctx.is_reserved_word(w.get_name_id())) {
            let local = self.parse_imported_default_binding()?;
            //TODO: Better error reporting
            if !self.is(tok!("from")) {
                expect!(self, ',');
            }
            specifiers.push(ImportSpecifier::Default(ImportDefaultSpecifier {
                node_id: node_id_from!(self, local.node_id),
                local,
            }));
        }

        {
            let import_spec_start = self.input.cur_pos();
            if self.eat(tok!('*')) {
                expect!(self, "as");
                let local = self.parse_imported_binding()?;
                specifiers.push(ImportSpecifier::Namespace(ImportStarAsSpecifier {
                    node_id: node_id!(self, self.span(import_spec_start)),
                    local,
                }));
            } else if self.eat(tok!('{')) {
                let mut first = true;
                while !self.is(tok!('}')) {
                    if first {
                        first = false;
                    } else if self.eat(tok!(',')) && self.is(tok!('}')) {
                        break;
                    }

                    specifiers.push(self.parse_import_specifier()?);
                }
                expect!(self, '}');
            }
        }

        let src = {
            expect!(self, "from");
            let str_start = self.input.cur_pos();
            match self.input.cur() {
                Token::Str { .. } => match self.input.bump() {
                    Token::Str { value } => Str {
                        node_id: node_id!(self, self.span(str_start)),
                        value,
                    },
                    _ => unreachable!(),
                },
                _ => unexpected!(self, "a string literal"),
            }
        };

        let asserts = if self.input.syntax().import_assertions()
            && !self.input.had_line_break_before_cur()
            && self.eat(tok!("assert"))
        {
            match *self.parse_object::<Box<Expr>>(&mut AssignProps::Emit)? {
                Expr::Object(v) => Some(v),
                _ => unreachable!(),
            }
        } else {
            None
        };

        self.expect_semi_with_asi()?;

        Ok(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
            node_id: node_id!(self, self.span(start)),
            specifiers,
            src,
            asserts,
        })))
    }

    /// Parse `foo`, `foo2 as bar` in `import { foo, foo2 as bar }`
    fn parse_import_specifier(&mut self) -> PResult<ImportSpecifier> {
        let start = self.input.cur_pos();
        if let Word(..) = self.input.cur() {
            let orig_name = self.parse_ident_name()?;

            if self.eat(tok!("as")) {
                let local = self.parse_binding_ident()?.id;
                let hi = get_span!(self, local.node_id).hi();
                let span = Span::new(start, hi);
                return Ok(ImportSpecifier::Named(ImportNamedSpecifier {
                    node_id: node_id!(self, span),
                    local,
                    imported: Some(orig_name),
                }));
            }

            // Handle difference between
            //
            // 'ImportedBinding'
            // 'IdentifierName' as 'ImportedBinding'
            if self.ctx().is_reserved_word(orig_name.name) {
                syntax_error!(
                    self,
                    get_span!(self, orig_name.node_id),
                    SyntaxError::ReservedWordInImport
                )
            }

            let local = orig_name;
            Ok(ImportSpecifier::Named(ImportNamedSpecifier {
                node_id: node_id!(self, self.span(start)),
                local,
                imported: None,
            }))
        } else {
            unexpected!(self, "an identifier")
        }
    }

    fn parse_imported_default_binding(&mut self) -> PResult<Ident> {
        self.parse_imported_binding()
    }

    fn parse_imported_binding(&mut self) -> PResult<Ident> {
        let ctx = Context {
            flags: self.ctx().flags & !ContextFlags::in_async & !ContextFlags::in_generator,
            ..self.ctx()
        };
        Ok(self.with_ctx(ctx).parse_binding_ident()?.id)
    }

    #[allow(clippy::cognitive_complexity)]
    fn parse_export(&mut self) -> PResult<Option<ModuleDecl>> {
        if !self.ctx().is_module() {
            // Switch to module mode
            let ctx = Context {
                module: YesNoMaybe::Yes,
                strict: YesMaybe::Yes,
                ..self.ctx()
            };
            self.set_ctx(ctx);
        }

        let start = self.input.cur_pos();
        self.assert_and_bump(tok!("export"));

        if self.input.cur() == &Token::Eof {
            return Err(self.eof_error());
        }

        let after_export_start = self.input.cur_pos();

        // "export declare" is equivalent to just "export".
        let declare = self.input.syntax().typescript() && self.eat(tok!("declare"));

        if declare {
            if let Some(decl) = self.try_parse_ts_declare(after_export_start)? {
                return match decl {
                    DeclOrEmpty::Decl(decl) => Ok(Some(ModuleDecl::ExportDecl(ExportDecl {
                        node_id: node_id!(self, self.span(start)),
                        decl,
                    }))),
                    DeclOrEmpty::Empty => Ok(None),
                };
            }
        }

        if self.input.syntax().typescript() && matches!(self.input.cur(), Token::Word(_)) {
            let sym = match self.input.cur() {
                Token::Word(w) => w.get_name_id(),
                _ => unreachable!(),
            };
            if let Some(decl) = self.try_parse_ts_export_decl(sym) {
                return match decl {
                    DeclOrEmpty::Decl(decl) => Ok(Some(ModuleDecl::ExportDecl(ExportDecl {
                        node_id: node_id!(self, self.span(start)),
                        decl,
                    }))),
                    DeclOrEmpty::Empty => Ok(None),
                };
            }
        }

        if self.input.syntax().typescript() {
            if self.eat(tok!("import")) {
                // export import A = B
                todo!();
                // return self
                //     .parse_ts_import_equals_decl(start, /* is_export */ true, false)
                //     .map(From::from);
            }

            if self.eat(tok!('=')) {
                // `export = x;`
                todo!();
                // let expr = self.parse_expr()?;
                // self.expect_semi_with_asi()?;
                // return Ok(TsExportAssignment {
                //     node_id: node_id!(self),
                //     span: self.span(start),
                //     expr,
                // }
                // .into());
            }

            if self.eat(tok!("as")) {
                // `export as namespace A;`
                // See `parseNamespaceExportDeclaration` in TypeScript's own parser
                // expect!(self, "namespace");
                // let id = self.parse_ident(false, false)?;
                // self.expect_semi_with_asi()?;
                todo!();
                // return Ok(TsNamespaceExportDecl {
                //     node_id: node_id!(self),
                //     span: self.span(start),
                //     id,
                // }
                // .into());
            }
        }

        let mut has_star = false;
        let mut export_ns = None;
        let ns_export_specifier_start = self.input.cur_pos();

        let type_only = self.input.syntax().typescript() && self.eat(tok!("type"));

        if self.eat(tok!('*')) {
            has_star = true;
            if self.is(tok!("from")) {
                let (src, asserts) = self.parse_from_clause_and_semi()?;
                return Ok(Some(ModuleDecl::ExportAll(ExportAll {
                    node_id: node_id!(self, self.span(start)),
                    src,
                    asserts,
                })));
            }
            if self.eat(tok!("as")) {
                let name = self.parse_ident_name()?;
                export_ns = Some(ExportSpecifier::Namespace(ExportNamespaceSpecifier {
                    node_id: node_id!(self, self.span(ns_export_specifier_start)),
                    name,
                }));
            }
        }

        // Some("default") if default is exported from 'src'
        let mut export_default = None;

        if !type_only && export_ns.is_none() && self.eat(tok!("default")) {
            if self.input.syntax().typescript() {
                if self.is(tok!("abstract"))
                    && self.peeked_is(tok!("class"))
                    && !self.input.has_linebreak_between_cur_and_peeked()
                {
                    let class_start = self.input.cur_pos();
                    self.assert_and_bump(tok!("abstract"));

                    if let Token::Error(_) = self.input.cur() {
                        if let Token::Error(e) = self.input.bump() {
                            return Err(e);
                        } else {
                            unreachable!();
                        }
                    }

                    let class = self.parse_default_class(start, class_start)?;
                    return Ok(Some(ModuleDecl::ExportDefaultDecl(class)));
                }
                if self.is(tok!("abstract")) && self.peeked_is(tok!("interface")) {
                    self.emit_err(self.input.cur_span(), SyntaxError::TS1242);
                    self.assert_and_bump(tok!("abstract"));
                }

                if self.is(tok!("interface")) {
                    todo!();
                    // let interface_start = self.input.cur_pos();
                    // self.assert_and_bump(tok!("interface"));
                    // let decl = self
                    //     .parse_ts_interface_decl(interface_start)
                    //     .map(DefaultDecl::from)?;
                    // return Ok(ExportDefaultDecl {
                    //     node_id: node_id!(self),
                    //     span: self.span(start),
                    //     decl,
                    // }
                    // .into());
                }
            }

            if self.is(tok!("class")) {
                let class_start = self.input.cur_pos();
                let decl = self.parse_default_class(start, class_start)?;
                return Ok(Some(ModuleDecl::ExportDefaultDecl(decl)));
            } else if self.is(tok!("async"))
                && self.peeked_is(tok!("function"))
                && !self.input.has_linebreak_between_cur_and_peeked()
            {
                let decl = self.parse_default_async_fn(start)?;
                return Ok(decl.map(ModuleDecl::ExportDefaultDecl));
            } else if self.is(tok!("function")) {
                let decl = self.parse_default_fn(start)?;
                return Ok(decl.map(ModuleDecl::ExportDefaultDecl));
            } else if self.input.syntax().export_default_from()
                && (self.is(tok!("from")) || (self.is(tok!(',')) && self.peeked_is(tok!('{'))))
            {
                export_default =
                    Some(self.new_ident(id_for_built_in!("default"), self.input.prev_span()));
            } else {
                let expr = self
                    .include_in_expr(true)
                    .parse_assignment_expr(&mut AssignProps::Emit)?
                    .unwrap();
                self.expect_semi_with_asi()?;
                return Ok(Some(ModuleDecl::ExportDefaultExpr(ExportDefaultExpr {
                    node_id: node_id!(self, self.span(start)),
                    expr,
                })));
            }
        }

        let decl = if !type_only && self.is(tok!("class")) {
            let class_start = self.input.cur_pos();
            self.parse_class_decl(start, class_start).map(Some)?
        } else if !type_only
            && self.is(tok!("async"))
            && self.peeked_is(tok!("function"))
            && !self.input.has_linebreak_between_cur_and_peeked()
        {
            self.parse_async_fn_decl()?
        } else if !type_only && self.is(tok!("function")) {
            self.parse_fn_decl_or_ts_overload_sig()?
        } else if !type_only
            && self.input.syntax().typescript()
            && self.is(tok!("const"))
            && self.peeked_is(tok!("enum"))
        {
            todo!();
            // let start = self.input.cur_pos();
            // self.assert_and_bump(tok!("const"));
            // self.assert_and_bump(tok!("enum"));
            // return self
            //     .parse_ts_enum_decl(start, true)
            //     .map(Decl::from)
            //     .map(|decl| {
            //         ModuleDecl::ExportDecl(ExportDecl {
            //             node_id: node_id!(self),
            //             span: self.span(start),
            //             decl,
            //         })
            //     });
        } else if !type_only
            && (self.is(tok!("var"))
                || self.is(tok!("const"))
                || (self.is(tok!("let")))
                    && self
                        .input
                        .peek()
                        .map(Token::follows_keyword_let)
                        .unwrap_or(false))
        {
            self.parse_var_stmt(false).map(Decl::Var).map(Some)?
        } else {
            // export {};
            // export {} from '';

            if self.is(tok!("from")) {
                if let Some(s) = export_ns {
                    let (src, asserts) = self.parse_from_clause_and_semi()?;
                    let hi = get_span!(self, src.node_id).hi();
                    let span = Span::new(start, hi);
                    return Ok(Some(ModuleDecl::ExportNamed(NamedExport {
                        node_id: node_id!(self, span),
                        specifiers: vec![s],
                        src: Some(src),
                        asserts,
                    })));
                }
            }

            let default = match export_default {
                Some(default) => Some(default),
                None => {
                    if self.input.syntax().export_default_from()
                        && matches!(self.input.cur(), Token::Word(_))
                    {
                        Some(self.parse_ident(false, false)?)
                    } else {
                        None
                    }
                }
            };

            if self.is(tok!("from")) {
                if let Some(default) = default {
                    let (src, asserts) = self.parse_from_clause_and_semi()?;
                    let hi = get_span!(self, src.node_id).hi();
                    let span = Span::new(start, hi);
                    return Ok(Some(ModuleDecl::ExportNamed(NamedExport {
                        node_id: node_id!(self, span),
                        specifiers: vec![ExportSpecifier::Default(ExportDefaultSpecifier {
                            node_id: node_id_from!(self, default.node_id),
                            exported: default,
                        })],
                        src: Some(src),
                        asserts,
                    })));
                }
            }

            if has_star && export_ns.is_none() {
                // improve error message for `export * from foo`
                let (src, asserts) = self.parse_from_clause_and_semi()?;
                let hi = get_span!(self, src.node_id).hi();
                let span = Span::new(start, hi);
                return Ok(Some(ModuleDecl::ExportAll(ExportAll {
                    node_id: node_id!(self, span),
                    src,
                    asserts,
                })));
            }

            let has_ns = export_ns.is_some();
            let has_default = default.is_some();
            if has_ns || has_default {
                expect!(self, ',');
            }

            expect!(self, '{');
            let mut specifiers = vec![];
            if let Some(s) = export_ns {
                specifiers.push(s);
            }
            if let Some(default) = default {
                specifiers.push(ExportSpecifier::Default(ExportDefaultSpecifier {
                    node_id: node_id_from!(self, default.node_id),
                    exported: default,
                }));
            }
            let mut first = true;
            while self.is(tok!(',')) || matches!(self.input.cur(), Token::Word(_)) {
                if first {
                    first = false;
                } else if self.eat(tok!(',')) && self.is(tok!('}')) {
                    break;
                }

                specifiers.push(
                    self.parse_named_export_specifier()
                        .map(ExportSpecifier::Named)?,
                );
            }
            expect!(self, '}');

            let opt = if self.is(tok!("from")) {
                Some(self.parse_from_clause_and_semi()?)
            } else {
                self.eat_semi_with_asi();
                if has_default || has_ns {
                    syntax_error!(
                        self,
                        self.span(start),
                        SyntaxError::ExportDefaultWithOutFrom
                    );
                }
                None
            };
            let (src, asserts) = match opt {
                Some(v) => (Some(v.0), v.1),
                None => (None, None),
            };
            return Ok(Some(ModuleDecl::ExportNamed(NamedExport {
                node_id: node_id!(self, self.span(start)),
                specifiers,
                src,
                asserts,
            })));
        };

        Ok(decl.map(|decl| {
            ModuleDecl::ExportDecl(ExportDecl {
                node_id: node_id!(self, self.span(start)),
                decl,
            })
        }))
    }

    fn parse_named_export_specifier(&mut self) -> PResult<ExportNamedSpecifier> {
        let start = self.input.cur_pos();

        let orig = self.parse_ident_name()?;

        let exported = if self.eat(tok!("as")) {
            Some(self.parse_ident_name()?)
        } else {
            None
        };

        Ok(ExportNamedSpecifier {
            node_id: node_id!(self, self.span(start)),
            orig,
            exported,
        })
    }

    /// Parses `from 'foo.js' assert {};`
    fn parse_from_clause_and_semi(&mut self) -> PResult<(Str, Option<ObjectLit>)> {
        expect!(self, "from");

        let str_start = self.input.cur_pos();
        let src = match self.input.cur() {
            Token::Str { .. } => match self.input.bump() {
                Token::Str { value } => Str {
                    node_id: node_id!(self, self.span(str_start)),
                    value,
                },
                _ => unreachable!(),
            },
            _ => unexpected!(self, "a string literal"),
        };

        let asserts = if self.input.syntax().import_assertions()
            && !self.input.had_line_break_before_cur()
            && self.eat(tok!("assert"))
        {
            match *self.parse_object::<Box<Expr>>(&mut AssignProps::Emit)? {
                Expr::Object(v) => Some(v),
                _ => unreachable!(),
            }
        } else {
            None
        };
        self.expect_semi_with_asi()?;
        Ok((src, asserts))
    }
}

impl IsDirective for ModuleItem {
    fn as_ref(&self) -> Option<&Stmt> {
        match self {
            ModuleItem::Stmt(s) => Some(s),
            ModuleItem::ModuleDecl(_) => None,
        }
    }
}

impl StmtLikeParser<ModuleItem> for Parser<'_> {
    fn handle_import_export(&mut self, top_level: bool) -> PResult<Option<ModuleItem>> {
        if !top_level {
            syntax_error!(self, SyntaxError::NonTopLevelImportExport);
        }

        if self.is(tok!("import")) {
            self.parse_import().map(Some)
        } else if self.is(tok!("export")) {
            self.parse_export().map(|d| d.map(ModuleItem::ModuleDecl))
        } else {
            unreachable!(
                "handle_import_export should not be called if current token isn't import nor \
                 export"
            )
        }
    }
}
