use util::AssignProps;

use super::*;
use crate::context::{ContextFlags, YesMaybe, YesNoMaybe};

impl<I: Tokens> Parser<'_, I> {
    #[allow(clippy::cognitive_complexity)]
    fn parse_import(&mut self) -> PResult<ModuleItem> {
        let start = self.input.cur_pos();

        if self.input.peeked_is(&tok!('.')) {
            let expr = self.parse_expr(&mut AssignProps::Emit)?.unwrap();

            eat!(self, ';');

            return Ok(ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                node_id: node_id!(self, span!(self, start)),
                expr,
            })));
        }

        if self.input.syntax().dynamic_import() && self.input.peeked_is(&tok!('(')) {
            let expr = self.parse_expr(&mut AssignProps::Emit)?.unwrap();

            eat!(self, ';');

            return Ok(ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                node_id: node_id!(self, span!(self, start)),
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

        if self.input.syntax().typescript() && is!(self, IdentRef) && peeked_is!(self, '=') {
            todo!();
            // return self
            //     .parse_ts_import_equals_decl(start, false, false)
            //     .map(ModuleDecl::from)
            //     .map(ModuleItem::from);
        }

        // Handle import 'mod.js'
        let str_start = self.input.cur_pos();
        if let Some(&Token::Str { .. }) = self.input.cur() {
            let src = match self.input.bump() {
                Token::Str { value, has_escape } => Str {
                    node_id: node_id!(self, span!(self, str_start)),
                    value,
                    has_escape,
                },
                _ => unreachable!(),
            };
            expect!(self, ';');
            return Ok(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
                node_id: node_id!(self, span!(self, start)),
                src,
                specifiers: vec![],
                asserts: None,
            })));
        }

        let type_only = self.input.syntax().typescript()
            && is!(self, "type")
            && (peeked_is!(self, '{') || !peeked_is!(self, "from") && !peeked_is!(self, ','));

        if type_only {
            self.assert_and_bump(&tok!("type"));

            if is!(self, IdentRef) && peeked_is!(self, '=') {
                todo!();
                // return self
                //     .parse_ts_import_equals_decl(start, false, true)
                //     .map(ModuleDecl::from)
                //     .map(ModuleItem::from);
            }
        }

        let mut specifiers = vec![];

        if is!(self, BindingIdent) {
            let local = self.parse_imported_default_binding()?;
            //TODO(swc): Better error reporting
            if !is!(self, "from") {
                expect!(self, ',');
            }
            specifiers.push(ImportSpecifier::Default(ImportDefaultSpecifier {
                node_id: node_id_from!(self, local.node_id),
                local,
            }));
        }

        {
            let import_spec_start = self.input.cur_pos();
            if eat!(self, '*') {
                expect!(self, "as");
                let local = self.parse_imported_binding()?;
                specifiers.push(ImportSpecifier::Namespace(ImportStarAsSpecifier {
                    node_id: node_id!(self, span!(self, import_spec_start)),
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
            match *cur!(self, true)? {
                Token::Str { .. } => match self.input.bump() {
                    Token::Str { value, has_escape } => Str {
                        node_id: node_id!(self, span!(self, str_start)),
                        value,
                        has_escape,
                    },
                    _ => unreachable!(),
                },
                _ => unexpected!(self, "a string literal"),
            }
        };

        let asserts = if self.input.syntax().import_assertions()
            && !self.input.had_line_break_before_cur()
            && eat!(self, "assert")
        {
            match *self.parse_object::<Box<Expr>>(&mut AssignProps::Emit)? {
                Expr::Object(v) => Some(v),
                _ => unreachable!(),
            }
        } else {
            None
        };

        expect!(self, ';');

        Ok(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
            node_id: node_id!(self, span!(self, start)),
            specifiers,
            src,
            asserts,
        })))
    }

    /// Parse `foo`, `foo2 as bar` in `import { foo, foo2 as bar }`
    fn parse_import_specifier(&mut self) -> PResult<ImportSpecifier> {
        let start = self.input.cur_pos();
        match self.input.cur() {
            Some(&Word(..)) => {
                let orig_name = self.parse_ident_name()?;

                if eat!(self, "as") {
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
                if self.ctx().is_reserved_word(&orig_name.sym) {
                    syntax_error!(
                        self,
                        get_span!(self, orig_name.node_id),
                        SyntaxError::ReservedWordInImport
                    )
                }

                let local = orig_name;
                Ok(ImportSpecifier::Named(ImportNamedSpecifier {
                    node_id: node_id!(self, span!(self, start)),
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
        self.assert_and_bump(&tok!("export"));

        let after_export_start = self.input.cur_pos();

        // "export declare" is equivalent to just "export".
        let declare = self.input.syntax().typescript() && eat!(self, "declare");

        if declare {
            if let Some(decl) = self.try_parse_ts_declare(after_export_start)? {
                return match decl {
                    DeclOrEmpty::Decl(decl) => Ok(Some(ModuleDecl::ExportDecl(ExportDecl {
                        node_id: node_id!(self, span!(self, start)),
                        decl,
                    }))),
                    DeclOrEmpty::Empty => Ok(None),
                };
            }
        }

        if self.input.syntax().typescript() && is!(self, IdentName) {
            let sym = match cur!(self, true)? {
                Token::Word(w) => w.clone().into(),
                _ => unreachable!(),
            };
            if let Some(decl) = self.try_parse_ts_export_decl(sym) {
                return match decl {
                    DeclOrEmpty::Decl(decl) => Ok(Some(ModuleDecl::ExportDecl(ExportDecl {
                        node_id: node_id!(self, span!(self, start)),
                        decl,
                    }))),
                    DeclOrEmpty::Empty => Ok(None),
                };
            }
        }

        if self.input.syntax().typescript() {
            if eat!(self, "import") {
                // export import A = B
                todo!();
                // return self
                //     .parse_ts_import_equals_decl(start, /* is_export */ true, false)
                //     .map(From::from);
            }

            if eat!(self, '=') {
                // `export = x;`
                todo!();
                // let expr = self.parse_expr()?;
                // expect!(self, ';');
                // return Ok(TsExportAssignment {
                //     node_id: node_id!(self),
                //     span: span!(self, start),
                //     expr,
                // }
                // .into());
            }

            if eat!(self, "as") {
                // `export as namespace A;`
                // See `parseNamespaceExportDeclaration` in TypeScript's own parser
                // expect!(self, "namespace");
                // let id = self.parse_ident(false, false)?;
                // expect!(self, ';');
                todo!();
                // return Ok(TsNamespaceExportDecl {
                //     node_id: node_id!(self),
                //     span: span!(self, start),
                //     id,
                // }
                // .into());
            }
        }

        let mut has_star = false;
        let mut export_ns = None;
        let ns_export_specifier_start = self.input.cur_pos();

        let type_only = self.input.syntax().typescript() && eat!(self, "type");

        if eat!(self, '*') {
            has_star = true;
            if is!(self, "from") {
                let (src, asserts) = self.parse_from_clause_and_semi()?;
                return Ok(Some(ModuleDecl::ExportAll(ExportAll {
                    node_id: node_id!(self, span!(self, start)),
                    src,
                    asserts,
                })));
            }
            if eat!(self, "as") {
                let name = self.parse_ident_name()?;
                export_ns = Some(ExportSpecifier::Namespace(ExportNamespaceSpecifier {
                    node_id: node_id!(self, span!(self, ns_export_specifier_start)),
                    name,
                }));
            }
        }

        // Some("default") if default is exported from 'src'
        let mut export_default = None;

        if !type_only && export_ns.is_none() && eat!(self, "default") {
            if self.input.syntax().typescript() {
                if is!(self, "abstract")
                    && peeked_is!(self, "class")
                    && !self.input.has_linebreak_between_cur_and_peeked()
                {
                    let class_start = self.input.cur_pos();
                    self.assert_and_bump(&tok!("abstract"));
                    let class = self.parse_default_class(start, class_start)?;
                    return Ok(Some(ModuleDecl::ExportDefaultDecl(class)));
                }
                if is!(self, "abstract") && peeked_is!(self, "interface") {
                    self.emit_err(self.input.cur_span(), SyntaxError::TS1242);
                    self.assert_and_bump(&tok!("abstract"));
                }

                if is!(self, "interface") {
                    todo!();
                    // let interface_start = self.input.cur_pos();
                    // self.assert_and_bump(&tok!("interface"));
                    // let decl = self
                    //     .parse_ts_interface_decl(interface_start)
                    //     .map(DefaultDecl::from)?;
                    // return Ok(ExportDefaultDecl {
                    //     node_id: node_id!(self),
                    //     span: span!(self, start),
                    //     decl,
                    // }
                    // .into());
                }
            }

            if is!(self, "class") {
                let class_start = self.input.cur_pos();
                let decl = self.parse_default_class(start, class_start)?;
                return Ok(Some(ModuleDecl::ExportDefaultDecl(decl)));
            } else if is!(self, "async")
                && self.input.peeked_is(&tok!("function"))
                && !self.input.has_linebreak_between_cur_and_peeked()
            {
                let decl = self.parse_default_async_fn(start)?;
                return Ok(decl.map(ModuleDecl::ExportDefaultDecl));
            } else if is!(self, "function") {
                let decl = self.parse_default_fn(start)?;
                return Ok(decl.map(ModuleDecl::ExportDefaultDecl));
            } else if self.input.syntax().export_default_from()
                && (is!(self, "from") || (is!(self, ',') && peeked_is!(self, '{')))
            {
                export_default = Some(self.new_ident("default".into(), self.input.prev_span()))
            } else {
                let expr = self
                    .include_in_expr(true)
                    .parse_assignment_expr(&mut AssignProps::Emit)?
                    .unwrap();
                expect!(self, ';');
                return Ok(Some(ModuleDecl::ExportDefaultExpr(ExportDefaultExpr {
                    node_id: node_id!(self, span!(self, start)),
                    expr,
                })));
            }
        }

        let decl = if !type_only && is!(self, "class") {
            let class_start = self.input.cur_pos();
            self.parse_class_decl(start, class_start).map(Some)?
        } else if !type_only
            && is!(self, "async")
            && self.input.peeked_is(&tok!("function"))
            && !self.input.has_linebreak_between_cur_and_peeked()
        {
            self.parse_async_fn_decl()?
        } else if !type_only && is!(self, "function") {
            self.parse_fn_decl_or_ts_overload_sig()?
        } else if !type_only
            && self.input.syntax().typescript()
            && is!(self, "const")
            && peeked_is!(self, "enum")
        {
            todo!();
            // let start = self.input.cur_pos();
            // self.assert_and_bump(&tok!("const"));
            // self.assert_and_bump(&tok!("enum"));
            // return self
            //     .parse_ts_enum_decl(start, true)
            //     .map(Decl::from)
            //     .map(|decl| {
            //         ModuleDecl::ExportDecl(ExportDecl {
            //             node_id: node_id!(self),
            //             span: span!(self, start),
            //             decl,
            //         })
            //     });
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
            self.parse_var_stmt(false).map(Decl::Var).map(Some)?
        } else {
            // export {};
            // export {} from '';

            if is!(self, "from") {
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
                    if self.input.syntax().export_default_from() && is!(self, IdentName) {
                        Some(self.parse_ident(false, false)?)
                    } else {
                        None
                    }
                }
            };

            if is!(self, "from") {
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
                expect!(self, ',')
            }

            expect!(self, '{');
            let mut specifiers = vec![];
            if let Some(s) = export_ns {
                specifiers.push(s)
            }
            if let Some(default) = default {
                specifiers.push(ExportSpecifier::Default(ExportDefaultSpecifier {
                    node_id: node_id_from!(self, default.node_id),
                    exported: default,
                }))
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
                if has_default || has_ns {
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
            return Ok(Some(ModuleDecl::ExportNamed(NamedExport {
                node_id: node_id!(self, span!(self, start)),
                specifiers,
                src,
                asserts,
            })));
        };

        Ok(decl.map(|decl| {
            ModuleDecl::ExportDecl(ExportDecl {
                node_id: node_id!(self, span!(self, start)),
                decl,
            })
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
            node_id: node_id!(self, span!(self, start)),
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
                    node_id: node_id!(self, span!(self, str_start)),
                    value,
                    has_escape,
                },
                _ => unreachable!(),
            },
            _ => unexpected!(self, "a string literal"),
        };

        let asserts = if self.input.syntax().import_assertions()
            && !self.input.had_line_break_before_cur()
            && eat!(self, "assert")
        {
            match *self.parse_object::<Box<Expr>>(&mut AssignProps::Emit)? {
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
        match self {
            ModuleItem::Stmt(s) => Some(s),
            _ => None,
        }
    }
}

impl<I: Tokens> StmtLikeParser<ModuleItem> for Parser<'_, I> {
    fn handle_import_export(&mut self, top_level: bool) -> PResult<Option<ModuleItem>> {
        if !top_level {
            syntax_error!(self, SyntaxError::NonTopLevelImportExport);
        }

        if is!(self, "import") {
            self.parse_import().map(Some)
        } else if is!(self, "export") {
            self.parse_export().map(|d| d.map(ModuleItem::ModuleDecl))
        } else {
            unreachable!(
                "handle_import_export should not be called if current token isn't import nor \
                 export"
            )
        }
    }
}
