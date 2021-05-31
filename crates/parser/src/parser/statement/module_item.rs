use super::*;
use crate::context::{YesMaybe, YesNoMaybe};

impl<'a, I: Tokens> Parser<I> {
    #[allow(clippy::cognitive_complexity)]
    fn parse_import(&mut self) -> PResult<ModuleItem> {
        let start = self.input.cur_pos();

        if self.input.peeked_is(&tok!('.')) {
            let expr = self.parse_expr()?;

            eat!(self, ';');

            return Ok(Stmt::Expr(ExprStmt {
                span: span!(self, start),
                expr,
            })
            .into());
        }

        if self.input.peeked_is(&tok!('(')) {
            let expr = self.parse_expr()?;

            eat!(self, ';');

            return Ok(Stmt::Expr(ExprStmt {
                span: span!(self, start),
                expr,
            })
            .into());
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

        // Handle import 'mod.js'
        let str_start = self.input.cur_pos();
        if let Some(&Token::Str { .. }) = self.input.cur() {
            let src = match self.input.bump() {
                Token::Str { value, has_escape } => Str {
                    span: span!(self, str_start),
                    value,
                    has_escape,
                    kind: StrKind::Normal {
                        contains_quote: true,
                    },
                },
                _ => unreachable!(),
            };
            expect!(self, ';');
            return Ok(ModuleDecl::Import(ImportDecl {
                span: span!(self, start),
                src,
                specifiers: vec![],
                type_only: false,
                asserts: None,
            }))
            .map(ModuleItem::from);
        }

        let type_only = false;

        if type_only {
            self.assert_and_bump(&tok!("type"));
        }

        let mut specifiers = vec![];

        if is!(self, BindingIdent) {
            let local = self.parse_imported_default_binding()?;
            //TODO(swc): Better error reporting
            if !is!(self, "from") {
                expect!(self, ',');
            }
            specifiers.push(ImportSpecifier::Default(ImportDefaultSpecifier {
                span: local.span,
                local,
            }));
        }

        {
            let import_spec_start = self.input.cur_pos();
            if eat!(self, '*') {
                expect!(self, "as");
                let local = self.parse_imported_binding()?;
                specifiers.push(ImportSpecifier::Namespace(ImportStarAsSpecifier {
                    span: span!(self, import_spec_start),
                    local,
                }));
            } else if eat!(self, '{') {
                let mut first = true;
                while !eof!(self) && !is!(self, '}') {
                    if first {
                        first = false;
                    } else if eat!(self, ',') && is!(self, '}') {
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
            let src = match *cur!(self, true)? {
                Token::Str { .. } => match self.input.bump() {
                    Token::Str { value, has_escape } => Str {
                        value,
                        has_escape,
                        span: span!(self, str_start),
                        kind: StrKind::Normal {
                            contains_quote: true,
                        },
                    },
                    _ => unreachable!(),
                },
                _ => unexpected!(self, "a string literal"),
            };
            src
        };

        let asserts = if !self.input.had_line_break_before_cur() && eat!(self, "assert") {
            match *self.parse_object::<Box<Expr>>()? {
                Expr::Object(v) => Some(v),
                _ => unreachable!(),
            }
        } else {
            None
        };

        expect!(self, ';');

        Ok(ModuleDecl::Import(ImportDecl {
            span: span!(self, start),
            specifiers,
            src,
            type_only,
            asserts,
        }))
        .map(ModuleItem::from)
    }

    /// Parse `foo`, `foo2 as bar` in `import { foo, foo2 as bar }`
    fn parse_import_specifier(&mut self) -> PResult<ImportSpecifier> {
        let start = self.input.cur_pos();
        match self.input.cur() {
            Some(&Word(..)) => {
                let orig_name = self.parse_ident_name()?;

                if eat!(self, "as") {
                    let local = self.parse_binding_ident()?.id;
                    return Ok(ImportSpecifier::Named(ImportNamedSpecifier {
                        span: Span::new(start, local.span.hi(), Default::default()),
                        local,
                        imported: Some(orig_name),
                    }));
                }

                // Handle difference between
                //
                // 'ImportedBinding'
                // 'IdentifierName' as 'ImportedBinding'
                if self.ctx().is_reserved_word(&orig_name.sym) {
                    syntax_error!(self, orig_name.span, SyntaxError::ReservedWordInImport)
                }

                let local = orig_name;
                Ok(ImportSpecifier::Named(ImportNamedSpecifier {
                    span: span!(self, start),
                    local,
                    imported: None,
                }))
            }
            _ => unexpected!(self, "an identifier"),
        }
    }

    fn parse_imported_default_binding(&mut self) -> PResult<Ident> {
        self.parse_imported_binding()
    }

    fn parse_imported_binding(&mut self) -> PResult<Ident> {
        let ctx = Context {
            in_async: false,
            in_generator: false,
            ..self.ctx()
        };
        Ok(self.with_ctx(ctx).parse_binding_ident()?.id)
    }

    #[allow(clippy::cognitive_complexity)]
    fn parse_export(&mut self, decorators: Vec<Decorator>) -> PResult<ModuleDecl> {
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
        self.assert_and_bump(&tok!("export"));
        let _ = cur!(self, true);

        let mut has_star = false;
        let mut export_ns = None;
        let ns_export_specifier_start = self.input.cur_pos();

        // let type_only = self.input.syntax().typescript() && eat!(self, "type");
        let type_only = false;

        if eat!(self, '*') {
            has_star = true;
            if is!(self, "from") {
                let (src, asserts) = self.parse_from_clause_and_semi()?;
                return Ok(ModuleDecl::ExportAll(ExportAll {
                    span: span!(self, start),
                    src,
                    asserts,
                }));
            }
            if eat!(self, "as") {
                let name = self.parse_ident_name()?;
                export_ns = Some(ExportSpecifier::Namespace(ExportNamespaceSpecifier {
                    span: span!(self, ns_export_specifier_start),
                    name,
                }));
            }
        }

        if !type_only && export_ns.is_none() && eat!(self, "default") {
            if is!(self, "class") {
                let class_start = self.input.cur_pos();
                let decl = self.parse_default_class(start, class_start, decorators)?;
                return Ok(ModuleDecl::ExportDefaultDecl(decl));
            } else if is!(self, "async")
                && self.input.peeked_is(&tok!("function"))
                && !self.input.has_linebreak_between_cur_and_peeked()
            {
                let decl = self.parse_default_async_fn(decorators)?;
                return Ok(ModuleDecl::ExportDefaultDecl(decl));
            } else if is!(self, "function") {
                let decl = self.parse_default_fn(decorators)?;
                return Ok(ModuleDecl::ExportDefaultDecl(decl));
            } else {
                let expr = self.include_in_expr(true).parse_assignment_expr()?;
                expect!(self, ';');
                return Ok(ModuleDecl::ExportDefaultExpr(ExportDefaultExpr {
                    span: span!(self, start),
                    expr,
                }));
            }
        }

        let decl = if !type_only && is!(self, "class") {
            let class_start = self.input.cur_pos();
            self.parse_class_decl(start, class_start, decorators)?
        } else if !type_only
            && is!(self, "async")
            && self.input.peeked_is(&tok!("function"))
            && !self.input.has_linebreak_between_cur_and_peeked()
        {
            self.parse_async_fn_decl(decorators)?
        } else if !type_only && is!(self, "function") {
            self.parse_fn_decl(decorators)?
        } else if !type_only
            && (is!(self, "var")
                || is!(self, "const")
                || (is!(self, "let"))
                    && self
                        .input
                        .peek()
                        .map(|t| t.follows_keyword_let())
                        .unwrap_or(false))
        {
            self.parse_var_stmt(false).map(Decl::Var)?
        } else {
            // export {};
            // export {} from '';

            if is!(self, "from") {
                if let Some(s) = export_ns {
                    let (src, asserts) = self.parse_from_clause_and_semi()?;
                    return Ok(ModuleDecl::ExportNamed(NamedExport {
                        span: Span::new(start, src.span.hi(), Default::default()),
                        specifiers: vec![s],
                        src: Some(src),
                        type_only,
                        asserts,
                    }));
                }
            }

            if has_star && export_ns.is_none() {
                // improve error message for `export * from foo`
                let (src, asserts) = self.parse_from_clause_and_semi()?;
                return Ok(ModuleDecl::ExportAll(ExportAll {
                    span: Span::new(start, src.span.hi(), Default::default()),
                    src,
                    asserts,
                }));
            }

            let has_ns = export_ns.is_some();
            if has_ns {
                expect!(self, ',')
            }

            expect!(self, '{');
            let mut specifiers = vec![];
            if let Some(s) = export_ns {
                specifiers.push(s)
            }
            let mut first = true;
            while is_one_of!(self, ',', IdentName) {
                if first {
                    first = false;
                } else if eat!(self, ',') && is!(self, '}') {
                    break;
                }

                specifiers.push(
                    self.parse_named_export_specifier()
                        .map(ExportSpecifier::Named)?,
                );
            }
            expect!(self, '}');

            let opt = if is!(self, "from") {
                Some(self.parse_from_clause_and_semi()?)
            } else {
                eat!(self, ';');
                if has_ns {
                    syntax_error!(
                        self,
                        span!(self, start),
                        SyntaxError::ExportDefaultWithOutFrom
                    );
                }
                None
            };
            let (src, asserts) = match opt {
                Some(v) => (Some(v.0), v.1),
                None => (None, None),
            };
            return Ok(ModuleDecl::ExportNamed(NamedExport {
                span: span!(self, start),
                specifiers,
                src,
                type_only,
                asserts,
            }));
        };

        Ok(ModuleDecl::ExportDecl(ExportDecl {
            span: span!(self, start),
            decl,
        }))
    }

    fn parse_named_export_specifier(&mut self) -> PResult<ExportNamedSpecifier> {
        let start = self.input.cur_pos();

        let orig = self.parse_ident_name()?;

        let exported = if eat!(self, "as") {
            Some(self.parse_ident_name()?)
        } else {
            None
        };

        Ok(ExportNamedSpecifier {
            span: span!(self, start),
            orig,
            exported,
        })
    }

    /// Parses `from 'foo.js' assert {};`
    fn parse_from_clause_and_semi(&mut self) -> PResult<(Str, Option<ObjectLit>)> {
        expect!(self, "from");

        let str_start = self.input.cur_pos();
        let src = match *cur!(self, true)? {
            Token::Str { .. } => match self.input.bump() {
                Token::Str { value, has_escape } => Str {
                    value,
                    has_escape,
                    span: span!(self, str_start),
                    kind: StrKind::Normal {
                        contains_quote: true,
                    },
                },
                _ => unreachable!(),
            },
            _ => unexpected!(self, "a string literal"),
        };

        let asserts = if !self.input.had_line_break_before_cur() && eat!(self, "assert") {
            match *self.parse_object::<Box<Expr>>()? {
                Expr::Object(v) => Some(v),
                _ => unreachable!(),
            }
        } else {
            None
        };
        expect!(self, ';');
        Ok((src, asserts))
    }
}

impl IsDirective for ModuleItem {
    fn as_ref(&self) -> Option<&Stmt> {
        match *self {
            ModuleItem::Stmt(ref s) => Some(s),
            _ => None,
        }
    }
}

impl<'a, I: Tokens> StmtLikeParser<'a, ModuleItem> for Parser<I> {
    fn handle_import_export(
        &mut self,
        top_level: bool,
        decorators: Vec<Decorator>,
    ) -> PResult<ModuleItem> {
        if !top_level {
            syntax_error!(self, SyntaxError::NonTopLevelImportExport);
        }

        let decl = if is!(self, "import") {
            self.parse_import()?
        } else if is!(self, "export") {
            self.parse_export(decorators).map(ModuleItem::from)?
        } else {
            unreachable!(
                "handle_import_export should not be called if current token isn't import nor \
                 export"
            )
        };

        Ok(decl)
    }
}
